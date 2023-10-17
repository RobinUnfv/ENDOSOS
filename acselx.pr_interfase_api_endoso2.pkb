CREATE OR REPLACE PACKAGE BODY ACSELX.pr_Interfase_API_Endoso2 IS

 /*-----------------------------------------------------------------------------
  Nombre      : PushError
  Proposito   : Concatenar los errores encontrados en formato XML
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cProceso       VARCHAR2   Proceso
                p_cCodError      VARCHAR2   CÛdigo del error
                p_cMsg           VARCHAR2   Mensaje de error
                p_dFecProc       DATE       Fecha del error
                p_cUser          VARCHAR2   Usuario
  Retorno     : p_cXMLError         XMLType    resultado del proceso

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    06/04/2022    Christian Bayona           Creacion
    18/04/2022    Christian Bayona           ModificaciÛn, se considera par·metros adicionales
    01/06/2022    Christian Bayona           ModificaciÛn, se adiciona mostrar mensaje de error
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona valores por defecto si no se remite Fecha y Usuario
  ----------------------------------------------------------------------------*/
PROCEDURE PushError(p_cProceso IN VARCHAR2,
                    p_cCodError IN VARCHAR2,
                    p_cMsg IN VARCHAR2,
                    p_dFecProc IN DATE,
                    p_cUser IN VARCHAR2,
                    --
                    p_cXMLError IN OUT NOCOPY XMLTYPE) IS
BEGIN
  SELECT XMLCONCAT(p_cXMLError,
    XMLELEMENT("mensajeValidacion",
      XMLFOREST(p_cProceso AS "nomProceso",
        NVL(p_dFecProc, SYSDATE) AS "fecProceso",  --<RTC 314175> / CBayona / 26-09-2022 / Se agregÛ valor por defecto si es nulo
        p_cCodError AS "codError",
        p_cMsg AS "msjError",
        NVL(p_cUser, pr_Sesion.Usuario_App) AS "usuario") ) )  --<RTC 314175> / CBayona / 26-09-2022 / Se agregÛ valor por defecto si es nulo
  INTO p_cXMLError
  FROM Dual;
  --
  DBMS_OUTPUT.Put_Line('>>ERROR ' || p_cProceso || '>> Cod: ' || p_cCodError || ' => ' || p_cMsg);
END PushError;


 /*-----------------------------------------------------------------------------
  Nombre      : GetTagEndoso
  Proposito   : FunciÛn que devuelve el tag principal por tipo de endoso
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTipoEnd       VARCHAR2   Tipo de Endoso
                p_cIndInOut      CHAR       Indicador de Entrada o Salida
  Retorno     :                  VARCHAR2   resultado de la funciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    20/04/2022    Christian Bayona           Creacion
  ----------------------------------------------------------------------------*/
FUNCTION GetTagEndoso(p_cTipoEnd IN VARCHAR2, p_cIndInOut IN CHAR)
RETURN VARCHAR2 IS
--
nIndex NUMBER(2);
cTag   VARCHAR2(30);
--
BEGIN
  nIndex := TO_NUMBER(p_cTipoEnd) - 69;
  cTag := 'endoso' || SUBSTR(vTags(nIndex), INSTR(vTags(nIndex), ',') + 1);
  IF p_cIndInOut = 'O' THEN
    cTag := cTag || 'Response';
  END IF;
  DBMS_OUTPUT.Put_Line(nIndex || ': ' || p_cTipoEnd || '<<' || p_cIndInOut || ' >> ' || cTag);
  --
  RETURN cTag;

EXCEPTION
  WHEN OTHERS THEN
    RETURN 'TagError';
END GetTagEndoso;


/*-----------------------------------------------------------------------------
  Nombre      : Valida_TramaXML
  Proposito   : Procedimiento que realiza validaciones sobre la trama
  Referencias : <RTC-314183> Endosos Modificaciones TREC
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta
                p_cXML             CLOB       N˙mero de Certificado
  Retorno     : p_cXMLError        XMLTYPE    Errores reportados previamente
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    29/08/2022    Christian Bayona           CreaciÛn
  ----------------------------------------------------------------------------*/
PROCEDURE Valida_TramaXML(p_cTag IN VARCHAR2,
                          p_cXML IN CLOB,
                          --
                          p_cXMLError IN OUT NOCOPY XMLTYPE,
                          p_cError IN OUT VARCHAR2) IS
--
l_xmltype  XMLTYPE;
cError VARCHAR2(2000);
nCode NUMBER(6);
--
BEGIN
--"*.xsd" registrado previamente
  l_xmltype := XMLTYPE(p_cXML, p_cTag || '.xsd');
  l_xmltype.SchemaValidate;
  DBMS_OUTPUT.Put_Line('Trama XML Valida');
  p_cError := '0,OK';

EXCEPTION
  WHEN OTHERS THEN
    nCode := SQLCODE;
    cError := SQLERRM;
    --
    SELECT
      CASE nCode
        WHEN -31000 THEN
          'Esquema de validaciÛn no registrado.'
        WHEN -31043 THEN
          'Etiqueta principal no definida en esquema.'
        WHEN -31154 THEN
          CASE
            WHEN INSTR(cError, 'LSX-00009') > 0 THEN
              'No debe ser NULO.'
            WHEN INSTR(cError, 'LSX-00333') > 0 THEN
              'Formato incorrecto.'
            WHEN INSTR(cError, 'LPX-00225') > 0 THEN
              'Etiqueta cerrada incorrectamente.'
            WHEN INSTR(cError, 'LSX-00213') > 0 THEN
              'Se requiere etiqueta o esta en orden incorrecto.'
          END
        WHEN -31011 THEN
          'Etiqueta no declarada correctamente.'
        WHEN -30937 THEN
          'Etiqueta no definida en Esquema.'
        WHEN -31038 THEN
          'Tipo de datos incorrecto.'
      END || CHR(10) ErrorMsg
    INTO p_cError
    FROM Dual;
    --
    p_cError := nCode || ',' || p_cError || cError;
    pr_Interfase_API_Endoso2.PushError('VALIDAXML', nCode, p_cError, SYSDATE, USER, p_cXMLError);
END Valida_TramaXML;


/*-----------------------------------------------------------------------------
  Nombre      : PreValidacion
  Proposito   : Procedimiento que realiza validaciones preliminares comunes
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_nNumPol          NUMBER     N˙mero de PÛliza
                p_cCodProd         VARCHAR2   CÛdigo de Producto
  Retorno     : p_cError           VARCHAR2   Resultado operaciÛn
                p_cXMLError        XMLTYPE    Errores reportados previamente

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    20/04/2022    Christian Bayona           Creacion
    04/05/2022    Christian Bayona           ModificaciÛn
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    23/08/2022    Christian Bayona           <RTC 314183> ModificaciÛn, Se agregÛ validaciÛn de Estado Certificado
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro de Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    20/06/2023    Christian Bayona           <RTC 317178> Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
  ----------------------------------------------------------------------------*/
PROCEDURE PreValidacion(p_nIdePol IN Poliza.IdePol%TYPE,
                        p_nNumCert IN Certificado.NumCert%TYPE,
                        -- <I RTC 313091> Renato Cortez - 18/07/2022
                        p_nNumPol IN Poliza.Numpol%TYPE,
                        p_cCodProd IN Poliza.CodProd%TYPE,
                        --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
                        p_dFecInicio IN DATE,
                        p_dFecFin IN DATE,
                        --<F RTC 316878>
                        -- <F RTC 313091>
                        p_cCodUsr IN Usuario.CodUsr%TYPE,  --<RTC 314175> / CBayona / 26-09-2022 / Se agregÛ par·metro
                        p_cError IN OUT VARCHAR2,
                        p_cXMLError IN OUT NOCOPY XMLTYPE) IS
--
nEsFacultativa     NUMBER(2);
nNumPol            Poliza.NumPol%TYPE;
cCodProd           Poliza.CodProd%TYPE;
dFecIniVigPol      Poliza.FecIniVig%TYPE;
dFecFinVigPol      Poliza.FecFinVig%TYPE;
cStsPol            Poliza.StsPol%TYPE;
bExiste            BOOLEAN;
cStsCert           Certificado.StsCert%TYPE; --<RTC 313344> CBayona / 23-08-2022 / Variable de estado Certificado
--
BEGIN
  p_cError := '0,OK';
  pr_Interfase_AX_SAS_Util.Cargar_Usuario_Sesion(p_cCodUsr);  --<RTC 314175> / CBayona / 26-09-2022 / Se carga usuario remitido
  --
  IF NVL(p_nIdePol, 0) = 0 THEN --IdePol no remitido en trama
    p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0049');
    PushError('PRE-VALID', 'VAL-0049', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
    p_cError := 'VAL-0049,' || p_cError;

  ELSE --Validaciones previas
    bExiste := pr_Interfase_AX_SAS_Util.Valida_Poliza_XIdePol(p_nIdePol, cStsPol, cCodProd, nNumPol, dFecIniVigPol, dFecFinVigPol);
    --
    IF NOT bExiste THEN --IdePol no existe
      p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0050');
      PushError('PRE-VALID', 'VAL-0050', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      p_cError := 'VAL-0050,' || p_cError;

    ELSE
      IF p_nNumCert > 0 AND pr_Certificado.Existe(p_nIdePol, p_nNumCert) != 'S' THEN --No existe el certificado : :mx1
        p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0139', p_nNumCert);
        PushError('PRE-VALID', 'VAL-0139', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
        p_cError := 'VAL-0139,' || p_cError;

      ELSE
        --<I RTC 313344> CBayona / 23-08-2022 / Se agregÛ validaciÛn
        SELECT MAX(StsCert)
        INTO cStsCert
        FROM Certificado
        WHERE IdePol = p_nIdePol
          AND NumCert = p_nNumCert
          AND StsCert IN ('EXC', 'ANU');
        --
        IF cStsCert IS NOT NULL THEN --El certificado :mx1 se encuentra en estado :mx2.No se puede registrar :mx3
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0385', p_nNumCert, cStsCert);
          PushError('PRE-VALID', 'VAL-0385', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
          p_cError := 'VAL-0385,' || p_cError;
        END IF;
        --<F RTC 313344>

        -- <I RTC 313091> Renato Cortez - 18/07/2022
        IF nNumPol != p_nNumPol OR p_nNumPol IS NULL THEN --El n˙mero de pÛliza no se encuentra asociado al IdePol.
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1526', 'ACT');
          PushError('PRE-VALID', 'VAL-1526', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
          p_cError := 'VAL-1526,' || p_cError;
        END IF;
        --
        IF cCodProd != p_cCodProd OR p_cCodProd IS NULL THEN --El cÛdigo de producto no se encuentra asociado al IdePol.
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1527', 'ACT');
          PushError('PRE-VALID', 'VAL-1527', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
          p_cError := 'VAL-1527,' || p_cError;
        END IF;
        -- <F RTC 313091>
        --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
        --IF p_dFecInicio IS NOT NULL AND p_dFecInicio < dFecIniVigPol THEN  --La Fecha inicial (:mx1) no puede ser menor al Inicio de Vigencia de la PÛliza (:mx2). --<RTC 317178> / Christian Bayona / 20-06-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
        IF p_dFecInicio IS NOT NULL AND p_dFecInicio < TRUNC(dFecIniVigPol) THEN  --<RTC 317178> / Christian Bayona / 20-06-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0053', TO_CHAR(p_dFecInicio, 'DD/MM/YYYY'), TO_CHAR(dFecIniVigPol, 'DD/MM/YYYY'));
          PushError('PRE-VALID', 'VAL-0053', p_cError, NULL, NULL, p_cXMLError);
          p_cError := 'VAL-0053,' || p_cError;
        END IF;
        --
        -- IF p_dFecFin IS NOT NULL AND p_dFecFin > dFecFinVigPol THEN --La Fecha Final (:mx1) no puede ser mayor al Fin de Vigencia de la PÛliza (:mx2). --<RTC 317178> / Christian Bayona / 20-06-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
        IF p_dFecFin IS NOT NULL AND p_dFecFin > TRUNC(dFecFinVigPol) THEN --<RTC 317178> / Christian Bayona / 20-06-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0056', TO_CHAR(p_dFecFin, 'DD/MM/YYYY'), TO_CHAR(dFecFinVigPol, 'DD/MM/YYYY'));
          PushError('PRE-VALID', 'VAL-0056', p_cError, NULL, NULL, p_cXMLError);
          p_cError := 'VAL-0056,' || p_cError;
        END IF;
        --<F RTC 316878>
        IF cStsPol != 'ACT' THEN --La pÛliza no se encuentra en estado :mx1. Verificar.
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1202', 'ACT');
          PushError('PRE-VALID', 'VAL-1202', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
          p_cError := 'VAL-1202,' || p_cError;
        END IF;
        --
        nEsFacultativa := acselx.pr_Interfase_AX.Valida_Facultativo(p_nIdePol);
        IF nEsFacultativa != 0 THEN --La pÛliza :mx1-:mx2 enviada es Facultativa
          p_cError := pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1500', cCodProd, nNumPol);
          PushError('PRE-VALID', 'VAL-1500', p_cError, NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
          p_cError := 'VAL-1500,' || p_cError;
        END IF;
      END IF;
    END IF;
  END IF;
  DBMS_OUTPUT.Put_Line('>>>PreValidaciones realizadas => Error: ' || p_cError);
END PreValidacion;


/*-----------------------------------------------------------------------------
  Nombre      : SetXMLOut
  Proposito   : Procedimiento que devuelve el XML de resultado del proceso
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTipoEnd         VARCHAR2   Tipo de Endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         VARCHAR2   N˙mero de Certificado
                p_nNumOper         VARCHAR2   N˙mero de OperaciÛn
                p_cError           VARCHAR2   Resultado operaciÛn
                p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_tNoAutoriza      TABLA      PolÌticas no autorizadas
  Retorno     : p_cXMLError        XMLTYPE    Errores reportados previamente

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/04/2022    Christian Bayona           Creacion
    16/05/2022    Christian Bayona           ModificaciÛn
    25/05/2022    Christian Bayona           ModificaciÛn, asignaciÛn de variable de retorno
    10/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE SetXMLOut(p_cTipoEnd IN VARCHAR2,
                    p_nIdePol IN Poliza.IdePol%TYPE,
                    p_nNumCert IN NUMBER,
                    p_nNumOper IN Factura.NumOper%TYPE,
                    p_cError IN VARCHAR2,
                    p_cMsgValida IN SYS_RefCursor,
                    p_tNoAutoriza IN acselx.TDU_Tabla_Autoriza_Violacion,
                    p_cXMLError IN XMLTYPE,
                    --
                    p_cXMLOut OUT NOCOPY CLOB) IS
--
cRetError     VARCHAR2(5);
l_xml_Out     XMLTYPE;
l_xml_Error   XMLTYPE;
l_xml_NoAut   XMLTYPE;
rMsg          pr_Interfase_API_Endoso.rMsgValidaRecordType;
nMtoOper      Oper_Pol.MtoOper%TYPE;  --<RTC 316878> / Christian Bayona / 10-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
--
BEGIN
  IF p_cError = '0,OK' THEN
    cRetError := 'OK';
  END IF;
  --
  IF p_cError != '0,OK' THEN
    -- Acumulamos los errores producidos durante el proceso
    BEGIN
      LOOP
        FETCH p_cMsgValida INTO rMsg;
        IF p_cMsgValida%FOUND THEN
          PushError(rMsg.Proc, rMsg.CodErr, rMsg.MsgErr, rMsg.FecPro, rMsg.UsuCre, l_xml_Error);
        END IF;
        EXIT WHEN p_cMsgValida%NOTFOUND;
      END LOOP;
      --
      CLOSE p_cMsgValida;

    EXCEPTION
      WHEN OTHERS THEN
        NULL;
    END;
    --Acumulamos las polÌticas reportadas durante el proceso
    BEGIN
      SELECT XMLAGG(
        XMLELEMENT("politica",
          XMLFOREST(
            na.CodBloq AS "Codigo",
            na.MotivoViolacion AS "Motivo") ) )
      INTO l_xml_NoAut
      FROM TABLE(p_tNoAutoriza) na;

    EXCEPTION
      WHEN OTHERS THEN
        l_xml_NoAut := NULL;
    END;
    --
    cRetError := 'ERROR';
  END IF;
--
--<I RTC 316878> / Christian Bayona / 10-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  SELECT MAX(MtoOper)
  INTO nMtoOper
  FROM Oper_Pol
  WHERE IdePol = p_nIdePol
    AND NumOper = p_nNumOper;
--<F RTC 316878>
--Enviar los resultados de la operaciÛn
  SELECT XMLELEMENT("P_CTAG",
    XMLELEMENT("idePol", p_nIdePol),
    (CASE SIGN(p_nNumCert)
       WHEN 1 THEN XMLELEMENT("numCert", p_nNumCert)
     END),
    XMLELEMENT("numOper", p_nNumOper),
    XMLELEMENT("mtoOper", nMtoOper),  --<RTC 316878> / Christian Bayona / 10-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    XMLELEMENT("resultado", cRetError),
    XMLELEMENT("resEndoso",
      XMLELEMENT("mensajeValidacionList", XMLCONCAT(p_cXMLError, l_xml_Error)),
      XMLELEMENT("politicasNoAutorizadas", l_xml_NoAut) ) )
  INTO l_xml_Out
  FROM Dual;
  --
  SELECT XMLSERIALIZE(CONTENT DELETEXML(l_xml_Out, '//*[.="" or contains(.,"?")]') INDENT)
  INTO p_cXMLOut
  FROM Dual;
  --
  p_cXMLOut := REPLACE(p_cXMLOut, 'P_CTAG', GetTagEndoso(p_cTipoEnd, 'O'));
END SetXMLOut;


/*-----------------------------------------------------------------------------
  Nombre      : SetPoliticasAut
  Proposito   : Procedimiento para establecer las polÌticas autorizadas
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
  Retorno     : p_Tabla_Autoriza   TABLA      AutorizaciÛn de PolÌticas

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/04/2022    Christian Bayona           Creacion
    27/05/2022    Christian Bayona           ModificaciÛn, data TDU
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
  ----------------------------------------------------------------------------*/
PROCEDURE SetPoliticasAut(p_cTag IN VARCHAR2,
                          p_cXMLInput IN CLOB,
                          p_nIdePol IN Poliza.IdePol%TYPE,
                          --
                          p_Tabla_Autoriza IN OUT NOCOPY acselx.TDU_Tabla_Autoriza_Violacion) IS
--
CURSOR c_politicasAutorizadas(cTag IN VARCHAR2) IS
  SELECT codBloq,
    autorizadoPor,
    fecAutorizado,
    horAutorizado,
    stsAutorizado,
    codProd,
    numPol,
    idePol,
    idePolitica,
    ideAut,
    codAut,
    motivoViolacion,
    autoriza,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/politicasAutorizadas/politica', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codBloq PATH 'codBloq',
      autorizadoPor PATH 'autorizadoPor',
      fecAutorizado DATE PATH 'fecAutorizado',
      horAutorizado PATH 'horAutorizado',
      stsAutorizado PATH 'stsAutorizado',
      codProd PATH 'codProd',
      numPol PATH 'numPol',
      idePol PATH 'idePol',
      idePolitica PATH 'idePolitica',
      ideAut PATH 'ideAut',
      codAut PATH 'codAut',
      motivoViolacion PATH 'motivoViolacion',
      autoriza PATH 'autoriza',
      txtAdicional PATH 'txtAdicional');
--
BEGIN
  --Cargar Tabla de autorizaciones en base a lo remitido en el XML
  p_Tabla_Autoriza := acselx.TDU_Tabla_Autoriza_Violacion();
  FOR rp IN c_politicasAutorizadas(p_cTag) LOOP
    IF rp.CodBloq IS NOT NULL THEN
      p_Tabla_Autoriza.Extend;
      p_Tabla_Autoriza(p_Tabla_Autoriza.LAST) :=
        acselx.TDU_Autoriza_Violacion(codProd         => rp.codProd,
                                      numPol          => rp.numPol,
                                      idePol          => NVL(rp.idePol, p_nIdePol),
                                      idePolitica     => rp.idePolitica,
                                      autorizadoPor   => rp.autorizadoPor,
                                      stsAutorizado   => NVL(rp.stsAutorizado, 'ACT'),
                                      fecAutorizado   => rp.fecautorizado,
                                      horAutorizado   => rp.horautorizado,
                                      ideAut          => rp.ideAut,
                                      codAut          => rp.codAut,
                                      motivoViolacion => rp.motivoViolacion,
                                      autoriza        => rp.autoriza,
                                      txtAdicional    => rp.txtAdicional,
                                      codBloq => rp.codBloq) ;
    END IF;
  END LOOP;
END SetPoliticasAut;


/*-----------------------------------------------------------------------------
  Nombre      : Ejecuta_Politicas
  Proposito   : Procedimiento para invocar la ejecuciÛn de PolÌticas
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumPol          NUMBER     N˙mero de PÛliza
                p_cCodProd         VARCHAR2   CÛdigo de Producto
                p_Tabla_Autoriza   TABLA      AutorizaciÛn de PolÌticas
  Retorno     : tTab_NoAutoriza TABLA      PolÌticas no autorizadas
                p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/04/2022    Christian Bayona           Creacion
    05/05/2022    Christian Bayona           ModificaciÛn
  ----------------------------------------------------------------------------*/
PROCEDURE Ejecuta_Politicas(p_nIdePol IN Poliza.IdePol%TYPE,
                            p_nNumPol IN Poliza.NumPol%TYPE,
                            p_cCodProd IN Poliza.CodProd%TYPE,
                            p_Tabla_Autoriza IN acselx.TDU_Tabla_Autoriza_Violacion,
                            --
                            p_Tabla_NoAutoriza IN OUT NOCOPY acselx.TDU_Tabla_Autoriza_Violacion,
                            p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                            p_cXMLError IN OUT NOCOPY XMLTYPE,
                            p_cError IN OUT VARCHAR2) IS
--
BEGIN
  DBMS_OUTPUT.Put_Line('>>>> EJECUTAR POLÕçTICAS');
  acselx.pr_Interfase_AX.Ejecuta_Politicas_Violadas(p_nIdePol,
                                                    p_nNumPol,
                                                    p_cCodProd,
                                                    NULL, --cTipoOper
                                                    NULL, --cCodUsr
                                                    p_Tabla_Autoriza,
                                                    --
                                                    p_Tabla_NoAutoriza,
                                                    p_cMsgValida,
                                                    p_cError) ;
  IF p_cError != '0,OK' THEN
    PushError('POLITICAS', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
  END IF;
END Ejecuta_Politicas;


/*-----------------------------------------------------------------------------
  Nombre      : SplitAnexo
  Proposito   : Procedimiento para dividir el texto de anexo si este sobrepasa los 4000 caracteres
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cCodAnex         VARCHAR2   CÛdigo de Anexo
                p_cTexto           VARCHAR2   Texto de Anexo
                p_cTxtAdic         VARCHAR2   Par·metros adicionales
  Retorno     : p_tAnexo           TABLA      Anexo PÛliza

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    03/06/2022    Christian Bayona           Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE SplitAnexo(p_cCodAnex IN VARCHAR2,
                     p_cTexto IN CLOB,
                     p_cTxtAdic IN VARCHAR2,
                     p_tAnexo IN OUT TDU_Tabla_Anexo_Pol) IS
nLen NUMBER;
nPos NUMBER;
BEGIN
  nLen := LENGTH(p_cTexto);
  nPos := 1;
  p_tAnexo := TDU_Tabla_Anexo_Pol();
  --
  IF nLen < 4001 THEN
    p_tAnexo := TDU_Tabla_Anexo_Pol( TDU_Anexo_Pol(p_cCodAnex, p_cTexto, p_cTxtAdic) );
  ELSE
    LOOP
      p_tAnexo.Extend;
      p_tAnexo(p_tAnexo.LAST) := TDU_Anexo_Pol(p_cCodAnex, '//001//' || SUBSTR(p_cTexto, nPos, 3993), p_cTxtAdic);
      nPos := nPos + 3993;
      EXIT WHEN nPos > nLen;
    END LOOP;
  END IF;
END SplitAnexo;


/*-----------------------------------------------------------------------------
  Nombre      : SplitAnexo
  Proposito   : Procedimiento para dividir el texto de anexo de Certificado si este sobrepasa los 4000 caracteres
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cCodRamo         VARCHAR2   CÛdigo de Ramo
                p_cCodAnex         VARCHAR2   CÛdigo de Anexo
                p_cTexto           VARCHAR2   Texto de Anexo
                p_cTxtAdic         VARCHAR2   Par·metros adicionales
  Retorno     : p_tAnexo           TABLA      Anexo Certificado

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    03/06/2022    Christian Bayona           Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE SplitAnexo(p_cCodRamo IN VARCHAR2,
                     p_cCodAnex IN VARCHAR2,
                     p_cTexto IN CLOB,
                     p_cTxtAdic IN VARCHAR2,
                     p_tAnexo IN OUT TDU_Tabla_Anexo_Cert) IS
nLen NUMBER;
nPos NUMBER;
BEGIN
  nLen := LENGTH(p_cTexto);
  nPos := 1;
  p_tAnexo := TDU_Tabla_Anexo_Cert();
  --
  IF nLen < 4001 THEN
    p_tAnexo := TDU_Tabla_Anexo_Cert( TDU_Anexo_Cert(p_cCodRamo, p_cCodAnex, p_cTexto, p_cTxtAdic) );
  ELSE
    LOOP
      p_tAnexo.Extend;
      p_tAnexo(p_tAnexo.LAST) := TDU_Anexo_Cert(p_cCodRamo, p_cCodAnex, '//001//' || SUBSTR(p_cTexto, nPos, 3993), p_cTxtAdic);
      nPos := nPos + 3993;
      EXIT WHEN nPos > nLen;
    END LOOP;
  END IF;
END SplitAnexo;


/*-----------------------------------------------------------------------------
  Nombre      : Anexo_PNuevoFormato
  Proposito   : Procedimiento para registrar el texto de anexo para el plan Nuevo Formato
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
  Retorno     : p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    21/04/2022    Christian Bayona           Creacion
    27/05/2022    Christian Bayona           ModificaciÛn, data TDU
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
  ----------------------------------------------------------------------------*/
PROCEDURE Anexo_PNuevoFormato(p_cTag IN VARCHAR2,
                              p_cCodProd IN VARCHAR2,
                              p_nIdePol IN NUMBER,
                              p_cXMLInput IN CLOB,
                              p_cXMLError IN OUT NOCOPY XMLTYPE,
                              p_cError IN OUT VARCHAR2) IS
--
CURSOR c_TextoAnexoPNF(cTag IN VARCHAR2) IS
  SELECT codEndoso,
    fecInicio,
    fecFin,
    codSec,
    textoAnexo,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/nuevoFormatoList/nuevoFormato', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codEndoso PATH 'codEndoso',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      codSec PATH 'codSec',
      textoAnexo PATH 'textoAnexo',
      txtAdicional PATH 'txtAdicional');
--
tPolOperacion        acselx.TDU_Tabla_Poliza_Operacion;
--
BEGIN
  IF p_cCodProd = '1301' THEN --SÛlo para multiriesgo
    tPolOperacion := acselx.TDU_Tabla_Poliza_Operacion();
    FOR r IN c_TextoAnexoPNF(p_cTag) LOOP
      DBMS_OUTPUT.Put_Line('>>>> GENERAR ANEXO PLAN NUEVO FORMATO -' || c_TextoAnexoPNF%ROWCOUNT);
      DBMS_OUTPUT.Put_Line('TextoAnexoPNF: ' || r.textoAnexo);
      tPolOperacion.Extend;
      tPolOperacion(tPolOperacion.LAST) := acselx.TDU_Poliza_Operacion(r.codEndoso, r.fecInicio, r.fecFin, r.codSec, r.textoAnexo, r.txtAdicional);
    END LOOP;

    acselx.pr_Interfase_AX.Mod_Nuevo_Formato(p_nIdePol, tPolOperacion, p_cError);
    --
    IF p_cError != '0,OK' THEN
      PushError('TXTANEXOPNF', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
    END IF;

    DBMS_OUTPUT.Put_Line('cError: ' || p_cError);
  END IF;
END Anexo_PNuevoFormato;


/*-----------------------------------------------------------------------------
  Nombre      : Anexo_Pol
  Proposito   : Procedimiento para registrar el texto de anexo a nivel pÛliza
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cCodUsr          VARCHAR2   CÛdigo del usuario
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/04/2022    Christian Bayona           Creacion
    05/05/2022    Christian Bayona           ModificaciÛn
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    27/05/2022    Christian Bayona           ModificaciÛn, data TDU
    03/06/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    07/06/2022    Christian Bayona           ModificaciÛn, trama soportar· m·s de un anexo
    02/09/2022    Christian Bayona           ModificaciÛn, se agrega validaciÛn
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se modifica par·metros de PushError
  ----------------------------------------------------------------------------*/
PROCEDURE Anexo_Pol(p_cTag IN VARCHAR2,
                    p_cXMLInput IN CLOB,
                    p_nIdePol IN NUMBER,
                    p_cCodUsr IN VARCHAR2,
                    p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                    p_cXMLError IN OUT NOCOPY XMLTYPE,
                    p_cError IN OUT VARCHAR2) IS
--
CURSOR c_TextoAnexo(cTag IN VARCHAR2) IS
  SELECT codAnexo,
    anexo,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/textoAnexo', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codAnexo PATH 'codAnexo',
      anexo CLOB PATH 'anexo',
      txtAdicional PATH 'txtAdicional')
  UNION ALL
  SELECT codAnexo,
    anexo,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/textoAnexoList/textoAnexo', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codAnexo PATH 'codAnexo',
      anexo CLOB PATH 'anexo',
      txtAdicional PATH 'txtAdicional');
--
tAnexoPol    TDU_Tabla_Anexo_Pol;
cCodProd     Producto.CodProd%TYPE;
--
BEGIN
  FOR r IN c_TextoAnexo(p_cTag) LOOP
    DBMS_OUTPUT.Put_Line('>>>> GENERAR ANEXO POLIZA');
    tAnexoPol := TDU_Tabla_Anexo_Pol();
    IF LENGTH(r.anexo) > 0 THEN
      r.codAnexo := NVL(r.codAnexo, 'SPECIF');
      SplitAnexo(r.codAnexo, r.anexo, r.txtAdicional, tAnexoPol);
      acselx.pr_Interfase_AX_SAS.Grabar_Anexo_Poliza(p_nIdePol, tAnexoPol, p_cCodUsr, p_cMsgValida, p_cError);
      DBMS_OUTPUT.Put_Line('Error Texto Anexo Pol: ' || p_cError);
    --<I RTC 314183> / CBayona / 02-09-2022 / Se agregÛ validaciÛn
    ELSE
      p_cError := '1,El texto del Anexo no puede ser nulo';
      PushError('TXTANEXO', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
    --<F RTC 314183>
    END IF;
  END LOOP;
  --Invocamos a Plan Nuevo Formato de haber registrado el anexo sin problemas
  IF p_cError = '0,OK' THEN
    SELECT codProd
    INTO cCodProd
    FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', p_cTag) PASSING xmltype(p_cXMLInput)
      COLUMNS
        codProd PATH 'codProd');
    --
    Anexo_PNuevoFormato(p_cTag, cCodProd, p_nIdePol, p_cXMLInput, p_cXMLError, p_cError);
  END IF;
END Anexo_Pol;



/*-----------------------------------------------------------------------------
  Nombre      : Anexo_Cert
  Proposito   : Procedimiento para registrar el texto de anexo a nivel Certificado
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cCodUsr          VARCHAR2   CÛdigo del usuario
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    07/06/2022    Christian Bayona           Creacion
    02/09/2022    Christian Bayona           ModificaciÛn, se agrega validaciÛn
    05/09/2022    Christian Bayona           ModificaciÛn, se agrega valores por defecto
  ----------------------------------------------------------------------------*/
PROCEDURE Anexo_Cert(p_cTag IN VARCHAR2,
                     p_cXMLInput IN CLOB,
                     p_nIdePol IN NUMBER,
                     p_nNumCert IN NUMBER,
                     p_cCodUsr IN VARCHAR2,
                     p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                     p_cError IN OUT VARCHAR2) IS
--
CURSOR c_anexoCertificado(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codAnexo,
    anexo,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/anexoCertificado', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codAnexo PATH 'codAnexo',
      anexo CLOB PATH 'anexo',
      txtAdicional PATH 'txtAdicional')
  UNION ALL
  SELECT codRamoCert,
    codAnexo,
    anexo,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/anexoCertificadoList/anexoCertificado', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codAnexo PATH 'codAnexo',
      anexo CLOB PATH 'anexo',
      txtAdicional PATH 'txtAdicional');
--
tAnexoCert    TDU_Tabla_Anexo_Cert;
--
BEGIN
  FOR r IN c_anexoCertificado(p_cTag) LOOP
    DBMS_OUTPUT.Put_Line('>>>> GENERAR ANEXO CERTIFICADO');
    tAnexoCert := TDU_Tabla_Anexo_Cert();
    IF LENGTH(r.anexo) > 0 THEN
      r.codAnexo := NVL(r.codAnexo, 'GARANT');  --<RTC-314183> / CBayona / 05-09-2022 / Se modifica valor por defecto
      r.txtAdicional := NVL(r.txtAdicional, 'RRGG');  --<RTC-314183> / CBayona / 05-09-2022 / Se agrega valor por defecto
      SplitAnexo(r.codRamoCert, r.codAnexo, r.anexo, r.txtAdicional, tAnexoCert);
      acselx.pr_Interfase_AX.Incluir_Anexo_Certificado(p_nIdePol, p_nNumCert, tAnexoCert, p_cCodUsr, p_cMsgValida, p_cError);
      DBMS_OUTPUT.Put_Line('Error Texto Anexo Pol: ' || p_cError);
    --<I RTC 314183> / CBayona / 02-09-2022 / Se agregÛ validaciÛn
    ELSE
      p_cError := '1,El texto del Anexo Certificado no puede ser nulo';
    --<F RTC 314183>
    END IF;
  END LOOP;
END Anexo_Cert;


/*-----------------------------------------------------------------------------
  Nombre      : Datos_Particulares
  Proposito   : Procedimiento para registrar los datos particulares de un certificado
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_dFecIni          DATE       Fecha de inicio
                p_dFecFin          DATE       Fecha de fin
                p_cCodRamo         VARCHAR2   CÛdigo de Ramo
  Retorno     : p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    19/05/2022    Christian Bayona           Creacion
    27/05/2022    Christian Bayona           ModificaciÛn, data TDU
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
  ----------------------------------------------------------------------------*/
PROCEDURE Datos_Particulares(p_cTag IN VARCHAR2,
                             p_cXMLInput IN CLOB,
                             p_nIdePol IN NUMBER,
                             p_nNumCert IN NUMBER,
                             p_dFecIni IN DATE,
                             p_dFecFin IN DATE,
                             p_cCodRamo IN VARCHAR2,
                             p_cXMLError IN OUT NOCOPY XMLTYPE,
                             p_cError IN OUT VARCHAR2) IS
--
CURSOR c_datosParticularesList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCrit,
    codTrasf,
    valTrasf,
    numOrd,
    valCodTab,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/datosParticularesList/datosParticulares', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCrit PATH 'codCrit',
      codTrasf PATH 'codTrasf',
      valTrasf PATH 'valTrasf',
      numOrd PATH 'numOrd',
      valCodTab PATH 'valCodTab',
      txtAdicional PATH 'txtAdicional'
      );
--
tDatosParticul  TDU_Tabla_Datos_Partic;
--
BEGIN
  tDatosParticul := TDU_Tabla_Datos_Partic();
  FOR r1 IN c_datosParticularesList(p_cTag) LOOP
    tDatosParticul.Extend;
    tDatosParticul(tDatosParticul.LAST) :=
      TDU_Datos_Partic(r1.codRamoCert, r1.codCrit, r1.codTrasf, r1.valTrasf, r1.numOrd, r1.valCodTab, r1.txtAdicional);
  END LOOP;
  --
  acselx.pr_Interfase_AX.Mod_DatParticulares(p_nIdePol, p_nNumCert, p_dFecIni, p_dFecFin, p_cCodRamo, tDatosParticul, p_cError);
  --
  IF p_cError != '0,OK' THEN
    PushError('DATPART', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
  END IF;
END Datos_Particulares;


/*-----------------------------------------------------------------------------
  Nombre      : Datos_Particulares
  Proposito   : Procedimiento para registrar los datos particulares de un certificado
  Referencias : <RTC-313350> Endosos ModificaciÛn de Corredor
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_dFecIni          DATE       Fecha de inicio
                p_dFecFin          DATE       Fecha de fin
                p_cCodRamo         VARCHAR2   CÛdigo de Ramo
                p_cCodUsr          VARCHAR2   CÛdigo de Usuario
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    11/10/2022    Christian Bayona           Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE Datos_Particulares(p_cTag IN VARCHAR2,
                             p_cXMLInput IN CLOB,
                             p_nIdePol IN NUMBER,
                             p_nNumCert IN NUMBER,
                             p_dFecIni IN DATE,
                             p_dFecFin IN DATE,
                             p_cCodRamo IN VARCHAR2,
                             p_cCodUsr IN VARCHAR2,
                             p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                             p_cError IN OUT VARCHAR2) IS
--
CURSOR c_datosParticularesList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCrit,
    codTrasf,
    valTrasf,
    numOrd,
    valCodTab,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/datosParticularesList/datosParticulares', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCrit PATH 'codCrit',
      codTrasf PATH 'codTrasf',
      valTrasf PATH 'valTrasf',
      numOrd PATH 'numOrd',
      valCodTab PATH 'valCodTab',
      txtAdicional PATH 'txtAdicional'
      );
--
tDatosParticul  TDU_Tabla_Datos_Partic;
--
BEGIN
  tDatosParticul := TDU_Tabla_Datos_Partic();
  FOR r IN c_datosParticularesList(p_cTag) LOOP
    IF r.codCrit IS NOT NULL THEN
      tDatosParticul.Extend;
      tDatosParticul(tDatosParticul.LAST) :=
        TDU_Datos_Partic(r.codRamoCert, r.codCrit, r.codTrasf, r.valTrasf, r.numOrd, r.valCodTab, r.txtAdicional);
    END IF;
  END LOOP;
  --
  IF tDatosParticul.COUNT > 0 THEN
    acselx.pr_Interfase_AX.Mod_DatParticulares(p_nIdePol, p_nNumCert, p_dFecIni, p_dFecFin, p_cCodRamo, tDatosParticul, p_cCodUsr, p_cMsgValida, p_cError);
  END IF;
END Datos_Particulares;


/*-----------------------------------------------------------------------------
  Nombre      : Add_CobertCert
  Proposito   : Procedimiento para registrar los datos de Coberturas de un certificado
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
  Retorno     : p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    01/06/2022    Christian Bayona           Creacion
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
  ----------------------------------------------------------------------------*/
PROCEDURE Add_CobertCert(p_cTag IN VARCHAR2,
                         p_cXMLInput IN CLOB,
                         p_nIdePol IN NUMBER,
                         p_nNumCert IN NUMBER,
                         p_cXMLError IN OUT NOCOPY XMLTYPE,
                         p_cError IN OUT VARCHAR2) IS
--
CURSOR c_CoberturaList(cTag IN VARCHAR2) IS
  SELECT codRamo,
   codCober,
   fecInicio,
   fecFin,
   sumAseg
  FROM XMLTABLE(REPLACE('/*/coberturaList/cobertura', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamo PATH 'codRamo',
      codCober PATH 'codCober',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      sumAseg PATH 'sumAseg');
--
nMtoPrima            NUMBER(10,2);
nTasa                NUMBER(10,2);
--
BEGIN
  <<coberturas>>
  FOR rC IN c_CoberturaList(p_cTag) LOOP
    DBMS_OUTPUT.Put_Line('~ ~ ~ ~ ~ ~ ADD_COBERT_CERT "' || rC.codCober || '" inicio');
    acselx.pr_Interfase_AX.Add_Cobert_Cert(p_nIdePol, p_nNumCert, rC.fecInicio, rC.fecFin, nMtoPrima, nTasa, rC.sumAseg, rC.codRamo, rC.codCober, p_cError);
    --
    IF p_cError != '0,OK' THEN
      PushError('COBERTCERT', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      EXIT coberturas;
    END IF;
    DBMS_OUTPUT.Put_Line('~ ~ ~ ~ ~ ~ ADD_COBERT_CERT "' || rC.codCober || '" fin');
  END LOOP coberturas;
END Add_CobertCert;


/*-----------------------------------------------------------------------------
  Nombre      : Carga_ContAseg
  Proposito   : Procedimiento para cargar datos de Contratante o Asegurado
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_cContAseg        VARCHAR2   Identificador de PÛliza
                p_cTipoMod         CHAR       N˙mero de Certificado
  Retorno     : p_tContAseg        TABLA      TDU_Tabla_Poliza_Cliente
                p_cError           VARCHAR2   Resultado de la operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    06/06/2022    Christian Bayona           Creacion
    16/06/2022    Christian Bayona           ModificaciÛn, se adiciona par·metro
  ----------------------------------------------------------------------------*/
PROCEDURE Carga_ContAseg(p_cTag IN VARCHAR2,
                         p_cXMLInput IN CLOB,
                         p_cContAseg IN VARCHAR2,
                         p_cTipoMod IN CHAR,
                         p_tContAseg IN OUT NOCOPY TDU_Tabla_Poliza_Cliente,
                         p_cError IN OUT VARCHAR2) IS
--
CURSOR c_ContAsegInc(cTag IN VARCHAR2, cContAseg IN VARCHAR2) IS
  SELECT numId,
    txtAdicional
  FROM XMLTABLE(REPLACE(REPLACE('/*/@sList/@Inc', '*', cTag), '@', cContAseg) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numId PATH 'numId',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_ContAsegExc(cTag IN VARCHAR2, cContAseg IN VARCHAR2) IS
  SELECT numId,
    fecExc,
    codMotvExc,
    txtAdicional
  FROM XMLTABLE(REPLACE(REPLACE('/*/@sList/@Exc', '*', cTag), '@', cContAseg) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numId PATH 'numId',
      fecExc DATE PATH 'fecExc',
      codMotvExc PATH 'codMotvExc',
      txtAdicional PATH 'txtAdicional');
--
nDups   NUMBER(3);
--
BEGIN
  SELECT COUNT(*)
  INTO nDups
  FROM (
    SELECT numId
    FROM XMLTABLE(REPLACE(REPLACE('/*/@sList/@Inc', '*', p_cTag), '@', p_cContAseg) PASSING xmltype(p_cXMLInput)
      COLUMNS
        numId PATH 'numId')
    INTERSECT
    SELECT numId
    FROM XMLTABLE(REPLACE(REPLACE('/*/@sList/@Exc', '*', p_cTag), '@', p_cContAseg) PASSING xmltype(p_cXMLInput)
      COLUMNS
        numId PATH 'numId')
  );
  --
  p_cError := '0,OK';
  IF nDups > 0 THEN
    p_cError := 'En XML de ' || p_cContAseg || 's existen cÛdigos duplicados';
    DBMS_OUTPUT.Put_Line(p_cError);
    p_cError := '1,' || p_cError;
    RETURN;
  END IF;
  --
  p_tContAseg := TDU_Tabla_Poliza_Cliente();
  --
  IF p_cTipoMod = 'I' THEN --InclusiÛn
    FOR r IN c_ContAsegInc(p_cTag, p_cContAseg) LOOP
      IF r.numId IS NOT NULL THEN
        p_tContAseg.Extend;
        p_tContAseg(p_tContAseg.LAST) := TDU_Poliza_Cliente(r.numId, 1, 'S', NULL, NULL, r.txtAdicional);
      END IF;
    END  LOOP;
  END IF;
  --
  IF p_cTipoMod = 'E' THEN --ExclusiÛn
    FOR r IN c_ContAsegExc(p_cTag, p_cContAseg) LOOP
      IF r.numId IS NOT NULL THEN
        p_tContAseg.Extend;
        r.fecExc := NVL(r.fecExc, SYSDATE);
        r.codMotvExc := NVL(r.codMotvExc, '0001');
        p_tContAseg(p_tContAseg.LAST) := TDU_Poliza_Cliente(r.numId, 1, 'N', r.fecExc, r.codMotvExc, r.txtAdicional);
      END IF;
    END  LOOP;
  END IF;
END Carga_ContAseg;


/*-----------------------------------------------------------------------------
  Nombre      : CambioDatosGenCert
  Proposito   : Procedimiento para Cambiar Datos Generales a nivel Certificado
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_cTipoMod         CHAR       N˙mero de Certificado
                p_tPolizaCliente   TABLA      TDU_Tabla_Poliza_Cliente
                p_cCodUsr          VARCHAR2   CÛdigo del usuario
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cXMLError        XML        Resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    25/04/2022    Christian Bayona           Creacion
    06/05/2022    Christian Bayona           ModificaciÛn
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    06/06/2022    Christian Bayona           ModificaciÛn, cambios en procedimiento
    24/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE CambioDatosGenCert(p_nIdePol IN Poliza.IdePol%TYPE,
                             p_nNumCert IN Certificado.NumCert%TYPE,
                             p_cTipoMod IN CHAR,
                             p_tPolizaCliente  IN TDU_Tabla_Poliza_Cliente,
                             p_cCodUsr IN VARCHAR2,
                             p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                             p_cXMLError IN OUT NOCOPY XMLTYPE,
                             p_cError IN OUT VARCHAR2) IS
--
tResp_Pago      TDU_Tabla_Resp_Pago;
--
BEGIN
  --CARGAR TDU
  --acselx.pr_Interfase_AX.Dev_Resp_Pago (p_nIdePol, tResp_Pago, p_cError);  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  acselx.pr_Interfase_AX.Dev_Resp_Pago (p_nIdePol, p_nNumCert, tResp_Pago, p_cError);  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  --
  IF p_cError != '0,OK' THEN
    PushError('RESPPAGO', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
  END IF;
  --
  IF p_cError = '0,OK' THEN
    acselx.pr_Interfase_AX.Cambio_Datos_Grales_Cert2(p_nIdePol, p_nNumCert, p_cTipoMod, p_tPolizaCliente, NULL, NULL, tResp_Pago, p_cCodUsr, p_cMsgValida, p_cError);
  END IF;
END CambioDatosGenCert;


/*-----------------------------------------------------------------------------
  Nombre      : ModClausulas
  Proposito   : Procedimiento para realizar la modificaciÛn de cl·usulas en base a par·metro XML
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsuario      VARCHAR2   CÛdigo de Usuario
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    07/06/2022    Christian Bayona           Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE ModClausulas(p_cTag IN VARCHAR2,
                       p_cXMLInput IN CLOB,
                       p_nIdePol IN NUMBER,
                       p_nNumCert IN NUMBER,
                       p_cCodUsr IN VARCHAR2,
                       p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                       p_cError IN OUT VARCHAR2) IS
--
CURSOR c_clausulaList(cTag VARCHAR2) IS
  SELECT codRamoCert,
    codClau,
    stsClau,
    fecIniValid,
    fecFinValid,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/clausulaList/clausula', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codClau PATH 'codClau',
      stsClau PATH 'stsClau',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      txtAdicional PATH 'txtAdicional');
--
tClausulas   TDU_Tabla_Clau_Cert := TDU_Tabla_Clau_Cert();
--
BEGIN
  FOR r IN c_clausulaList(p_cTag) LOOP
    tClausulas.Extend;
    tClausulas(tClausulas.LAST) := TDU_Clau_Cert(r.codRamoCert, r.codClau, r.stsClau, r.fecIniValid, r.fecFinValid, r.txtAdicional);
  END LOOP;
  --
  pr_Interfase_AX.Modificar_Clausula(p_nIdePol, p_nNumCert, tClausulas, p_cCodUsr, p_cMsgValida, p_cError);
END ModClausulas;


/*-----------------------------------------------------------------------------
  Nombre      : CargaTDUActivar
  Proposito   : Procedimiento para cargar los TDUs requeridos en la activaciÛn
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cTxtAdic         VARCHAR2   Par·metros adicionales
  Retorno     : p_tAnexo           TABLA      Texto Anexo
                p_tPrimaPactad     TABLA      Prima Pactada
                p_tResp_Pago       TABLA      Responsable de Pago
                p_tComision        TABLA      ComisiÛn
                p_tFinanc          TABLA      Financiamiento
                p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    25/04/2022    Christian Bayona           Creacion
    05/05/2022    Christian Bayona           ModificaciÛn, par·metro por defecto
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    09/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE CargaTDUActivar(p_cTag             IN VARCHAR2,
                          p_cXMLInput        IN CLOB,
                          p_nIdePol          IN NUMBER,
                          p_cTxtAdic         IN VARCHAR2 DEFAULT 'NSSS',
                          p_tAnexo           OUT NOCOPY acselx.TDU_Tabla_Anexo_Cert,
                          p_tPrima_Pactada   OUT NOCOPY acselx.TDU_Tabla_PrimaPactada,
                          p_tResp_Pago       OUT NOCOPY acselx.TDU_Tabla_Resp_Pago,
                          p_tComision        OUT NOCOPY acselx.TDU_Tabla_Comision,
                          p_tFinanciamiento  OUT NOCOPY acselx.TDU_Tabla_Financiamiento,
                          p_cXMLError        IN OUT NOCOPY XMLTYPE,
                          p_cError           IN OUT VARCHAR2) IS
--
CURSOR c_Datos(cTag IN VARCHAR2) IS
  SELECT primaPactada,
    numCert
  FROM XMLTABLE(REPLACE('/*', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      primaPactada PATH 'primaPactada',
      numCert PATH 'reqEndoso/numCert');
--
nNumCert     Certificado.NumCert%TYPE;
--
BEGIN
  --Anexo Texto
  p_tAnexo := NULL;

  --Prima Pactada
  p_tPrima_Pactada := NULL;
  IF SUBSTR(p_cTxtAdic, 3, 1) = 'S' THEN --Prima Pactada
    FOR r IN c_Datos(p_cTag) LOOP
      --IF r.primaPactada > 0 THEN  --<RTC 316878> / Christian Bayona / 19-04-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B / Se considera diferente a 0
      IF r.primaPactada != 0 THEN  --<RTC 316878> / Christian Bayona / 19-04-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B / Se considera diferente a 0
        nNumCert := NVL(r.numCert, 1);
        p_tPrima_Pactada := TDU_Tabla_PrimaPactada( acselx.TDU_PrimaPactada( nNumCert, r.primaPactada, '' ) );
        DBMS_OUTPUT.Put_Line('*** numCert: ' || nNumCert || ' - PPact: ' || r.primaPactada);
      END IF;
    END  LOOP;
  END IF;

  --Responsable de Pago
  p_tResp_Pago := NULL;
  IF (SUBSTR(p_cTxtAdic, 2, 1) = 'S') OR (SUBSTR(p_cTxtAdic, 3, 1) = 'S') THEN --ComisiÛn/Prima Pactada
    --<I RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    FOR r IN c_Datos(p_cTag) LOOP
      nNumCert := NVL(r.numCert, 1);
    END  LOOP;
    acselx.pr_Interfase_AX.Dev_Resp_Pago (p_nIdePol, nNumCert, p_tResp_Pago, p_cError);
    --<F RTC 316878>
    --
    IF p_cError != '0,OK' THEN
      PushError('RESPPAGO', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
    END IF;
  END IF;

  --ComisiÛn
  p_tComision := NULL;
  IF SUBSTR(p_cTxtAdic, 2, 1) = 'S' THEN --ComisiÛn
    NULL;
  END IF;

  --Financiamiento
  p_tFinanciamiento := NULL;
END CargaTDUActivar;


/*-----------------------------------------------------------------------------
  Nombre      : Activar_Pol
  Proposito   : Procedimiento para invocar la activaciÛn de Endosos
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cNumTramite      VARCHAR2   N˙mero de Tr·mite
                p_cCodUsuario      VARCHAR2   CÛdigo de Usuario
                p_cTipoOper        VARCHAR2   Tipo de OperaciÛn
                p_cIndTemp         VARCHAR2   Indicador de ActivaciÛn Temporal
                p_cTxtAdic         VARCHAR2   Par·metros adicionales
  Retorno     : p_nNumOper         VARCHAR2   N˙mero de OperaciÛn
                p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/04/2022    Christian Bayona           Creacion
    25/04/2022    Christian Bayona           ModificaciÛn, cambio de par·metros y uso de nuevo procedimiento
    06/05/2022    Christian Bayona           ModificaciÛn
    20/05/2022    Christian Bayona           ModificaciÛn, se quito rutina de reporte de errores
    27/05/2022    Christian Bayona           ModificaciÛn, se adiciono rutina de polÌticas y par·metros
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
  ----------------------------------------------------------------------------*/
PROCEDURE Activar_Pol(p_cTag         IN VARCHAR2,
                      p_cXMLInput    IN CLOB,
                      p_nIdePol      IN Poliza.IdePol%TYPE,
                      p_nNumPol      IN Poliza.NumPol%TYPE,
                      p_cCodProd     IN Poliza.CodProd%TYPE,
                      p_cCodUsuario  IN VARCHAR2,
                      p_cIndTemp     IN VARCHAR2 DEFAULT 'N',
                      p_cTxtAdic     IN VARCHAR2 DEFAULT 'NSSS',
                      -- Variables de retorno
                      p_nNumOper        OUT VARCHAR2,
                      p_cMsgValida      IN OUT NOCOPY SYS_RefCursor,
                      p_TabNoAutoriza   IN OUT NOCOPY acselx.TDU_Tabla_Autoriza_Violacion,
                      p_cXMLError       IN OUT NOCOPY XMLTYPE,
                      p_cError          IN OUT VARCHAR2) IS
--
CURSOR c_activarPoliza(cTag IN VARCHAR2) IS --Datos Activar PÛliza
  SELECT numTramite,
    TipoOper
  FROM XMLTABLE(REPLACE('/*/activarPoliza', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numTramite PATH 'numTramite',
      TipoOper PATH 'TipoOper');
--
tAnexo           acselx.TDU_Tabla_Anexo_Cert;
tPrima_Pactada   acselx.TDU_Tabla_PrimaPactada;
tResp_Pago       acselx.TDU_Tabla_Resp_Pago;
tComision        acselx.TDU_Tabla_Comision;
tFinanciamiento  acselx.TDU_Tabla_Financiamiento;
cTxtAdic         VARCHAR2(100);
tTab_Autoriza    acselx.TDU_Tabla_Autoriza_Violacion;
cNumTramite      Poliza_Tramite.NumTramite%TYPE;
cTipoOper        Oper_Pol_Det.TipoOp%TYPE;
--
BEGIN
  FOR r IN c_activarPoliza(p_cTag) LOOP
    cNumTramite := r.numTramite;
    cTipoOper := r.TipoOper;
  END LOOP;
  --
  IF SUBSTR(p_cTxtAdic, 1, 1) = 'N' THEN
    --Cargar Tabla de autorizaciones en base a lo remitido en el XML
    SetPoliticasAut(p_cTag, p_cXMLInput, p_nIdePol, tTab_Autoriza);
    Ejecuta_Politicas(p_nIdePol, p_nNumPol, p_cCodProd, tTab_Autoriza, p_TabNoAutoriza, p_cMsgValida, p_cXMLError, p_cError);
  END IF;
  --
  IF p_cError = '0,OK' THEN
    --Carga todos los TDU's para la activaciÛn de la pÛliza
    DBMS_OUTPUT.Put_Line('>>>> Carga de TDUs - ActivaciÛn de PÛliza');
    CargaTDUActivar(p_cTag, p_cXMLInput, p_nIdePol, p_cTxtAdic,
      tAnexo, tPrima_Pactada, tResp_Pago, tComision, tFinanciamiento,
      p_cXMLError, p_cError);
    --
    IF p_cError = '0,OK' THEN
      DBMS_OUTPUT.Put_Line('>>>> Inicio de ActivaciÛn de PÛliza');
      cTxtAdic := LPAD(' ', 34) || p_cTxtAdic;
      acselx.pr_Interfase_AX_SAS.Activar_Poliza_PP(p_nIdePol => p_nIdePol,
                                                   p_cNumTramite => cNumTramite,
                                                   p_cCodUsuario => p_cCodUsuario,
                                                   p_cTipoOper => cTipoOper, -- TipoOper
                                                   p_Tabla_PrimaPactada => tPrima_Pactada,
                                                   p_nNumOper => p_nNumOper, -- Variables de retorno
                                                   p_cursorMsgValida => p_cMsgValida,
                                                   p_cError => p_cError,
                                                   p_Tabla_Anexo => tAnexo,
                                                   p_Tabla_Resp_Pago => tResp_Pago,
                                                   p_cIndTemp => p_cIndTemp,
                                                   p_Tabla_Comision => tComision,
                                                   p_Tabla_Financiamiento => tFinanciamiento,
                                                   p_ctxtAdicional => cTxtAdic);
                                                 --'                                  NSSS');
                                                 --'123456789+123456789+123456789+12345)
      --
      DBMS_OUTPUT.Put_Line('Resultado Activar PÛliza: ' || p_cError);
    END IF;
  END IF;
END Activar_Pol;


/*-----------------------------------------------------------------------------
  Nombre      : Tramite_Poliza
  Proposito   : Vincular/Desvincular el n˙mero de tr·mite con su pÛliza respectiva
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesarios
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    05/05/2022    Christian Bayona           Creacion
    25/05/2022    Christian Bayona           ModificaciÛn, formatear la salida
  ----------------------------------------------------------------------------*/
PROCEDURE Tramite_Poliza ( p_cXMLInput  IN CLOB,
                           p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqTramite(cTag IN VARCHAR2) IS
  SELECT idePol,
    numOper,
    numTramite
  FROM XMLTABLE(REPLACE('/endosoTramite/*List/*', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numOper PATH 'numOper',
      numTramite PATH 'numTramite');
--
tPolizaTramite   acselx.TDU_Tabla_Poliza_Tramite;
cError           VARCHAR2(2000);
xError           XMLTYPE;
--
BEGIN
  xError := NULL;
  FOR r IN c_reqTramite('Vincular') LOOP
    acselx.pr_Interfase_AX.TramiteVincular(r.idePol, r.numOper, r.numTramite, cError);
    IF cError != '0,OK' THEN
      PushError('VINCTRAMITE', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, xError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
    END IF;
  END LOOP;
  --
  tPolizaTramite := acselx.TDU_Tabla_Poliza_Tramite();
  FOR r IN c_reqTramite('Desvincular') LOOP
    tPolizaTramite.Extend;
    tPolizaTramite(tPolizaTramite.LAST) := acselx.TDU_Poliza_Tramite(r.idePol, r.numOper, r.numTramite);
  END LOOP;
  --
  IF tPolizaTramite.COUNT > 0 THEN
    acselx.pr_Interfase_AX.TramiteDesvincular(tPolizaTramite, cError);
    IF cError != '0,OK' THEN
      PushError('DESVTRAMITE', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, xError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
    END IF;
  END IF;
  --
  SELECT XMLELEMENT("endosoTramiteResponse",
    XMLELEMENT("resultado", DECODE(cError, '0,OK', 'OK', 'ERROR')),
      XMLELEMENT("mensajeValidacionList", xError))
  INTO xError
  FROM Dual;

  SELECT XMLSERIALIZE(CONTENT DELETEXML(xError, '//*[.="" or contains(.,"?")]') INDENT)
  INTO p_cXMLOutput
  FROM Dual;
END Tramite_Poliza;


/*-----------------------------------------------------------------------------
  Nombre      : SetCesionINC
  Proposito   : Procedimiento para realizar una inclusiÛn de CesiÛn de Derecho
  Referencias : <RTC 314183> Endoso ModificaciÛn TREC
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsuario      VARCHAR2   CÛdigo de Usuario
  Retorno     : p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    26/08/2022    Christian Bayona           Creacion
    27/09/2022    Christian Bayona           ModificaciÛn, se agrega validaciÛn
    26/05/2023    Manuel Navarro             <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE SetCesionINC(p_cTag IN VARCHAR2,
                       p_cXMLInput  IN CLOB,
                       p_nIdePol IN Poliza.IdePol%TYPE,
                       p_nNumCert IN Certificado.NumCert%TYPE,
                       p_cCodUsr IN VARCHAR2,
                       p_cXMLError IN OUT NOCOPY XMLTYPE,
                       p_cError IN OUT VARCHAR2) IS
--
CURSOR c_CesionInc(cTag IN VARCHAR2) IS
  SELECT fecInicio,
    fecFin,
    numId,
    mtoEndoso,
    codTexto,
    texto,
    --<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B, Suma Asegurada VB Tech Core
    aprobado,
    sustento,
    usuarioAprob
    --<F RTC 316878>
  FROM XMLTABLE(REPLACE('/*/cesionINC', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      numId PATH 'numId',
      mtoEndoso PATH 'mtoEndoso',
      codTexto PATH 'codTexto',
      texto PATH 'texto',
      --<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B, Suma Asegurada VB Tech Core
      aprobado PATH 'aprobado',
      sustento PATH 'sustento',
      usuarioAprob PATH 'usuarioAprob'
      --<F RTC 316878>
      );

--
BEGIN
  FOR r IN c_CesionInc(p_cTag) LOOP
  --<I RTC 314175> / CBayona / 27-09-2022 / Se agrega validaciÛn
    IF r.numId IS NOT NULL THEN
      --<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B, Suma Asegurada VB Tech Core
      --acselx.pr_Interfase_AX.Inserta_Endosatario_Cert(p_nIdePol, p_nNumCert, r.numId, r.fecInicio, r.fecFin, r.mtoEndoso, r.texto, '001', r.codTexto, NULL, NULL, p_cError);
      acselx.pr_Interfase_AX.Inserta_Endosatario_Cert(p_nIdePol, p_nNumCert, r.numId, r.fecInicio, r.fecFin, r.mtoEndoso, r.texto, '001', r.codTexto, NULL, NULL, r.Aprobado, r.Sustento, r.Usuarioaprob, p_cError);
      --<F RTC 316878>
      --

      IF p_cError != '0,OK' THEN
        PushError('SETCESIONINC', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      END IF;
    END IF;
  --<F RTC 314175>
  END LOOP;
END SetCesionINC;


/*-----------------------------------------------------------------------------
  Nombre      : SetCesionEXC
  Proposito   : Procedimiento para realizar una exclusiÛn de CesiÛn de Derecho
  Referencias : <RTC 314183> Endoso ModificaciÛn TREC
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsuario      VARCHAR2   CÛdigo de Usuario
  Retorno     : p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    26/08/2022    Christian Bayona           Creacion
    27/09/2022    Christian Bayona           ModificaciÛn, se agrega validaciÛn
    06/02/2023    Christian Bayona           ModificaciÛn, se agrega par·metro txtAdicional (Identificador)
  ----------------------------------------------------------------------------*/
PROCEDURE SetCesionEXC(p_cTag IN VARCHAR2,
                       p_cXMLInput  IN CLOB,
                       p_nIdePol IN Poliza.IdePol%TYPE,
                       p_nNumCert IN Certificado.NumCert%TYPE,
                       p_cCodUsr IN VARCHAR2,
                       p_cXMLError IN OUT NOCOPY XMLTYPE,
                       p_cError IN OUT VARCHAR2) IS
--
CURSOR c_CesionExc(cTag IN VARCHAR2) IS
  SELECT fecExc,
    codExc,
    texto
  FROM XMLTABLE(REPLACE('/*/cesionEXC', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      fecExc DATE PATH 'fecExc',
      codExc PATH 'codExc',
      texto PATH 'texto');
--
CURSOR c_Endosatarios(cTag IN VARCHAR2) IS
  SELECT numId,
    txtAdicional  --<RTC 316106> / CBayona / 06-02-2023 / Se adiciona par·metro
  FROM XMLTABLE(REPLACE('/*/cesionEXC/endosatarioList/endosatario', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numId PATH 'numId',
      txtAdicional PATH 'txtAdicional');  --<RTC 316106> / CBayona / 06-02-2023 / Se adiciona par·metro
--
tTablaEndosatCert    acselx.TDU_Tabla_Endosatario_Cert;
--
BEGIN
  tTablaEndosatCert := TDU_Tabla_Endosatario_Cert();
  FOR rE IN c_Endosatarios(p_cTag) LOOP
    IF rE.numId IS NOT NULL THEN  --<RTC 314175> / CBayona / 27-09-2022 / Se agrega validaciÛn
      tTablaEndosatCert.Extend;
      tTablaEndosatCert(tTablaEndosatCert.LAST) := acselx.TDU_Endosatario_Cert(p_nidePol, p_nNumCert, rE.numId,
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, rE.txtAdicional);  --<RTC 316106> / CBayona / 06-02-2023 / Se adiciona par·metro Identificador
    END IF;  --<RTC 314175> / CBayona / 27-09-2022 / Fin validaciÛn
  END LOOP;
  --
  IF tTablaEndosatCert.COUNT > 0 THEN --<RTC 314175> / CBayona / 27-09-2022 / Se agrega validaciÛn
    FOR r IN c_CesionExc(p_cTag) LOOP
      acselx.pr_Interfase_AX.Excluye_Endosatario_Cert (p_nIdePol, p_nNumCert, tTablaEndosatCert, r.fecExc, r.codExc, r.texto, p_cError);
      --
      IF p_cError != '0,OK' THEN
        PushError('SETCESIONEXC', SUBSTR(p_cError, 1, INSTR(p_cError, ',') - 1), SUBSTR(p_cError, INSTR(p_cError, ',') + 1), NULL, NULL, p_cXMLError);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      END IF;
    END LOOP;
  END IF;  --<RTC 314175> / CBayona / 27-09-2022 / Fin validaciÛn
END SetCesionEXC;


/*-----------------------------------------------------------------------------
  Nombre      : SetRespPago
  Proposito   : Procedimiento para realizar un cambio de responsable de pago
  Referencias : <RTC 313349> Endoso Cambio Responsable de Pago
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsuario      VARCHAR2   CÛdigo de Usuario
  Retorno     : p_cXMLError        XML (CLob) con resultado del proceso
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    26/08/2022    Christian Bayona           Creacion
    19/09/2022    Christian Bayona           <RTC 314175> Se agregÛ par·metro a procedimiento
  ----------------------------------------------------------------------------*/
PROCEDURE SetRespPago(p_cTag IN VARCHAR2,
                      p_cXMLInput  IN CLOB,
                      p_nIdePol IN Poliza.IdePol%TYPE,
                      p_nNumCert IN Certificado.NumCert%TYPE,
                      p_cCodUsr IN VARCHAR2,
                      p_cMsgValida IN OUT NOCOPY SYS_RefCursor, --<RTC 313349> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
                      p_cXMLError IN OUT NOCOPY XMLTYPE,
                      p_cError IN OUT VARCHAR2) IS
--
CURSOR c_respPagoList(cTag IN VARCHAR2) IS
  SELECT numIdPerRespPago,
    porcPago,
    codPlanFracc,
    modPlanFracc,
    numDocRef,
    canCuotas,
    tipoRedondeo,
    codAgencia,
    codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    direc,
    telef1,
    telef2,
    telef3,
    teleX,
    zip,
    codZonaPostal,
    codRuta,
    formFact,
    numComprob,
    tipPagFac,
    txtTitular,
    tipDocCargo,
    numIdEntFinanc,
    fecVenc,
    codMoneda,
    codRiesgo,
    fecPrimerVenc,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/respPagoList/respPago', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numIdPerRespPago PATH 'numIdPerRespPago',
      porcPago PATH 'porcPago',
      codPlanFracc PATH 'codPlanFracc',
      modPlanFracc PATH 'modPlanFracc',
      numDocRef PATH 'numDocRef',
      canCuotas PATH 'canCuotas',
      tipoRedondeo PATH 'tipoRedondeo',
      codAgencia PATH 'codAgencia',
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      direc PATH 'direc',
      telef1 PATH 'telef1',
      telef2 PATH 'telef2',
      telef3 PATH 'telef3',
      teleX PATH 'teleX',
      zip PATH 'zip',
      codZonaPostal PATH 'codZonaPostal',
      codRuta PATH 'codRuta',
      formFact PATH 'formFact',
      numComprob PATH 'numComprob',
      tipPagFac PATH 'tipPagFac',
      txtTitular PATH 'txtTitular',
      tipDocCargo PATH 'tipDocCargo',
      numIdEntFinanc PATH 'numIdEntFinanc',
      fecVenc DATE PATH 'fecVenc',
      codMoneda PATH 'codMoneda',
      codRiesgo PATH 'codRiesgo',
      fecPrimerVenc DATE PATH 'fecPrimerVenc',
      txtAdicional PATH 'txtAdicional');
--
tResp_Pago       acselx.TDU_Tabla_Resp_Pago;
--
BEGIN
  tResp_Pago := TDU_Tabla_Resp_Pago();
  FOR t IN c_respPagoList(p_cTag) LOOP
    IF NVL(t.porcPago, 0) > 0 THEN
      tResp_Pago.Extend;
      tResp_Pago(tResp_Pago.LAST) :=
        TDU_Resp_Pago(p_nIdePol, p_nNumCert, t.numIdPerRespPago, t.porcPago, t.codPlanFracc, t.modPlanFracc, t.numDocRef, t.canCuotas,
          t.tipoRedondeo, t.codAgencia, t.codPais, t.codEstado, t.codCiudad, t.codMunicipio, t.direc, t.telef1, t.telef2,
          t.telef3, t.teleX, t.zip, t.codZonaPostal, t.codRuta, t.formFact, t.numComprob, t.tipPagFac, t.txtTitular,
          t.tipDocCargo, t.numIdEntFinanc, t.fecVenc, t.codMoneda, t.codRiesgo, t.fecPrimerVenc, t.txtAdicional);
    END IF;
  END LOOP;
  --
  IF tResp_Pago.COUNT > 0 THEN
    pr_Interfase_AX.Cambio_Resp_Pago(p_nIdePol, p_nNumCert, tResp_Pago, p_cCodUsr, p_cMsgValida, p_cError);  --<RTC 313349> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
  END IF;

END SetRespPago;


/*-----------------------------------------------------------------------------
  Nombre      : SetDeducible
  Proposito   : Procedimiento para generar un Deducible
  Referencias : <RTC-314181> Endoso de Modificacion Deducible
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_cNumCert         NUMBER     N˙mero de Certificado
                p_dFecIni          DATE       Fecha de inicio de vigencia
                p_dFecFin          DATE       Fecha de fin de vigencia
                p_cCodUsuario      VARCHAR2   CÛdigo de Usuario

  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    26/08/2022    Christian Bayona           <RTC 314183> CreaciÛn
    27/09/2022    Christian Bayona           ModificaciÛn, se agrega validaciÛn
  ----------------------------------------------------------------------------*/
PROCEDURE SetDeducible(p_cTag IN VARCHAR2,
                       p_cXMLInput  IN CLOB,
                       p_nIdePol IN Poliza.IdePol%TYPE,
                       p_nNumCert IN Certificado.NumCert%TYPE,
                       p_dFecIni IN DATE,
                       p_dFecFin IN DATE,
                       p_cCodUsr IN VARCHAR2,
                       p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                       p_cError IN OUT VARCHAR2) IS
--
CURSOR c_deducibleCobertList(cTag IN VARCHAR2) is
  SELECT codRamoCert,
    codCobert,
    numDed,
    baseDedCalculo,
    porcDedCalculo,
    mtoDedFijo,
    baseDedMinimo,
    porcDedMinimo,
    mtoDedMinimo,
    baseDedMaximo,
    porcDedMaximo,
    mtoDedMaximo,
    codEst,
    valEst,
    textoDeduc,
    ideDeduc,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/deducibleCobertList/deducibleCobert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCobert PATH 'codCobert',
      numDed PATH 'numDed',
      baseDedCalculo PATH 'baseDedCalculo',
      porcDedCalculo PATH 'porcDedCalculo',
      mtoDedFijo PATH 'mtoDedFijo',
      baseDedMinimo PATH 'baseDedMinimo',
      porcDedMinimo PATH 'porcDedMinimo',
      mtoDedMinimo PATH 'mtoDedMinimo',
      baseDedMaximo PATH 'baseDedMaximo',
      porcDedMaximo PATH 'porcDedMaximo',
      mtoDedMaximo PATH 'mtoDedMaximo',
      codEst PATH 'codEst',
      valEst PATH 'valEst',
      textoDeduc PATH 'textoDeduc',
      ideDeduc PATH 'ideDeduc',
      txtAdicional PATH 'txtAdicional');
--
tDeducibleCobert     TDU_Tabla_Deducible_Cobert;
--
BEGIN
  tDeducibleCobert := TDU_Tabla_Deducible_Cobert();
  FOR t IN c_deducibleCobertList(p_cTag) LOOP
    IF t.numDed IS NOT NULL THEN  --<RTC 314175> / CBayona / 27-09-2022 / Se agrega validaciÛn
      tDeducibleCobert.Extend;
      tDeducibleCobert(tDeducibleCobert.LAST) :=
      TDU_Deducible_Cobert(t.codRamoCert, t.codCobert, t.numDed, t.baseDedCalculo, t.porcDedCalculo, t.mtoDedFijo,
                           t.baseDedMinimo, t.porcDedMinimo, t.mtoDedMinimo, t.baseDedMaximo, t.porcDedMaximo,
                           t.mtoDedMaximo, t.codEst, t.valEst, t.textoDeduc, t.ideDeduc, t.txtAdicional);
    END IF;  --<RTC 314175> / CBayona / 27-09-2022 / Fin validaciÛn
  END LOOP;
  --
  IF tDeducibleCobert.COUNT > 0 THEN  --<RTC 314175> / CBayona / 27-09-2022 / Se agrega validaciÛn
    pr_Interfase_AX.Modificar_Deducible(p_nIdePol, p_nNumCert, p_dFecIni, p_dFecFin, tDeducibleCobert, NULL, p_cCodUsr, p_cMsgValida, p_cError);
  END IF;  --<RTC 314175> / CBayona / 27-09-2022 / Fin validaciÛn
END SetDeducible;


/*-----------------------------------------------------------------------------
  Nombre      : INC_CobertCert
  Proposito   : Procedimiento para incluir nuevas  Coberturas en un certificado
  Referencias : <RTC-314953> MultiEndosos
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsr          VARCHAR2   CÛdigo de Usuario (numID)
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cXMLError        XMLTYPE    Errores reportados previamente
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    05/10/2022    Christian Bayona           <RTC 313350> CreaciÛn
  ----------------------------------------------------------------------------*/
PROCEDURE INC_CobertCert(p_cTag IN VARCHAR2,
                         p_cXMLInput IN CLOB,
                         p_nIdePol IN NUMBER,
                         p_nNumCert IN NUMBER,
                         p_cCodUsr IN VARCHAR2,
                         p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                         p_cXMLError IN OUT NOCOPY XMLTYPE,
                         p_cError IN OUT VARCHAR2) IS
--
CURSOR c_incCobertCertList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
   codCobert,
   -- Montos segun la moneda de la poliza
   sumaAsegMoneda, --
   tasa,
   primaMoneda, --
   codMoneda,
   -- Montos en moneda Local
   prima,
   sumaAseg,
   -- datos de la cobertura
   codMonedaCob,
   sumaAsegMonedaCob,
   fecIniValid,
   fecFinValid,
   txtAdicional
  FROM XMLTABLE(REPLACE('/*/incCobertCertList/cobertura', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCobert PATH 'codCobert',
      sumaAsegMoneda PATH 'sumaAsegMoneda',
      tasa PATH 'tasa',
      primaMoneda PATH 'primaMoneda',
      codMoneda PATH 'codMoneda',
      prima PATH 'prima',
      sumaAseg PATH 'sumaAseg',
      codMonedaCob PATH 'codMonedaCob',
      sumaAsegMonedaCob PATH 'sumaAsegMonedaCob',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      txtAdicional PATH 'txtAdicional');
--
nMtoPrima            NUMBER(10,2);
nTasa                NUMBER(10,2);
tCobertCert          acselx.TDU_Tabla_Cobert_Cert;
--
nTasaCambio          NUMBER(14,9);
nTasaCambioCob       NUMBER(14,9);
nTasaCambioPol       NUMBER(14,9);
nTipTasa             NUMBER(4);
cCodMonPol           Poliza.CodMoneda%TYPE;
rCobPlan             Cobert_Plan_Prod%ROWTYPE;
--
BEGIN
  pr_vIniSusc.Cargar;
  nTasaCambio := pr.Tasa_Cambio('USD', pr_vIniSusc.Codigo_Moneda, SYSDATE, 'D');
  --
  SELECT MAX(CodProd), MAX(CodMoneda)
  INTO rCobPlan.CodProd, cCodMonPol
  FROM Poliza
  WHERE IdePol = p_nIdePol;
  --
  --Cargar TDU
  tCobertCert := acselx.TDU_Tabla_Cobert_Cert();
  <<coberturas>>
  FOR t IN c_incCobertCertList(p_cTag) LOOP
    IF t.codCobert IS NOT NULL AND t.sumaAsegMoneda > 0 THEN
      tCobertCert.Extend;
      --
      p_cError := pr_Interfase_AX.Valida_Cobertura(p_nIdePol, p_nNumCert, t.codRamoCert, t.codCobert);
      DBMS_OUTPUT.Put_Line('>>>> ValidaciÛn de Cobertura: ' || t.codCobert || ' => ' || p_cError);
      --
      IF p_cError != '0,OK' THEN
        pr_Interfase_API_Endoso2.PushError('INCCOBERT', '1', p_cError, NULL, NULL, p_cXMLError);
        EXIT coberturas;
      END IF;
      --
      SELECT MAX(CodPlan), MAX(RevPlan), NVL(t.fecIniValid, MAX(FecIniValid)), NVL(t.fecFinValid, MAX(FecFinValid))
      INTO rCobPlan.CodPlan, rCobPlan.RevPlan, t.fecIniValid, t.fecFinValid
      FROM Cert_Ramo
      WHERE IdePol = p_nIdePol
        AND NumCert = p_nNumCert
        AND CodRamoCert = t.codRamoCert;
      --
      rCobPlan := pr_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(rCobPlan.CodProd, rCobPlan.CodPlan, rCobPlan.RevPlan, t.codRamoCert, t.codCobert);
      --
      DBMS_OUTPUT.Put_Line('rCobPlan.ParamPreCobert: ' || rCobPlan.ParamPreCobert);
      DBMS_OUTPUT.Put_Line('>> 1 t.Tasa: ' || t.Tasa);
      IF t.Tasa IS NULL THEN
        DBMS_OUTPUT.Put_Line('>> 2 t.Tasa: ' || t.Tasa);
        t.Tasa := NVL(pr_Pre_Cobert.Pre_Tasa(p_nIdePol, p_nNumCert, rCobPlan.CodProd, rCobPlan.CodPlan, rCobPlan.RevPlan,
          t.codRamoCert, t.codCobert, rCobPlan.ParamPreCobert, NULL), 0);
        DBMS_OUTPUT.Put_Line('>> 3 t.Tasa: ' || t.Tasa);
      END IF;
      --
      t.codMoneda := NVL(cCodMonPol, t.codMoneda);
      t.sumaAseg := NVL(t.sumaAsegMoneda * nTasaCambio, t.sumaAseg);
      t.prima := NVL(t.primaMoneda * nTasaCambio, t.prima);
      t.codMonedaCob := NVL(t.codMoneda, t.codMonedaCob);
      t.sumaAsegMonedaCob := NVL(t.sumaAsegMoneda, t.sumaAsegMonedaCob);
      --
      tCobertCert(tCobertCert.LAST) := acselx.TDU_Cobert_Cert(t.CodRamoCert, t.CodCobert, 'VAL',
        t.SumaAsegMoneda, t.Tasa, t.PrimaMoneda, t.CodMoneda, t.Prima, t.SumaAseg, t.CodMonedaCob, t.SumaAsegMonedaCob, t.FecIniValid, t.FecFinValid, t.txtAdicional);
    END IF;
  END LOOP coberturas;
  --
  IF p_cError = '0,OK' THEN
    pr_Interfase_AX_SAS.Inclusion_Cobertura_Cert(p_nIdePol, p_nNumCert, tCobertCert, NULL, p_cCodUsr, p_cMsgValida, p_cError);
  END IF;
END INC_CobertCert;


/*-----------------------------------------------------------------------------
  Nombre      : EXC_CobertCert
  Proposito   : Procedimiento para excluir Coberturas de un certificado
  Referencias : <RTC-314953> MultiEndosos
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsr          VARCHAR2   CÛdigo de Usuario (numID)
  Retorno     : p_cMsgValida       CURSOR     Cursor de Mensajes de ValidaciÛn
                p_cXMLError        XMLTYPE    Errores reportados previamente
                p_cError           VARCHAR2   Resultado operaciÛn

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    06/10/2022    Christian Bayona           <RTC 313350> CreaciÛn
  ----------------------------------------------------------------------------*/
PROCEDURE EXC_CobertCert(p_cTag IN VARCHAR2,
                         p_cXMLInput IN CLOB,
                         p_nIdePol IN NUMBER,
                         p_nNumCert IN NUMBER,
                         p_cCodUsr IN VARCHAR2,
                         p_cMsgValida IN OUT NOCOPY SYS_RefCursor,
                         p_cXMLError IN OUT NOCOPY XMLTYPE,
                         p_cError IN OUT VARCHAR2) IS
--
CURSOR c_excCobertCertList(cTag IN VARCHAR2) IS
  SELECT codRamo,
   codCobert,
   fecExc,
   codMotvExc,
   textMotvExc
  FROM XMLTABLE(REPLACE('/*/excCobertCertList/cobertura', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamo PATH 'codRamo',
      codCobert PATH 'codCobert',
      fecExc DATE PATH 'fecExc',
      codMotvExc PATH 'codMotvExc',
      textMotvExc PATH 'textMotvExc');
--
nExiste      NUMBER(4);
cCodProd     Producto.CodProd%TYPE;
cCodPlan     Cert_Ramo.CodPlan%TYPE;
cRevPlan     Cert_Ramo.RevPlan%TYPE;
--
BEGIN
  <<coberturas>>
  FOR rC IN c_excCobertCertList(p_cTag) LOOP
    IF rC.codCobert IS NOT NULL THEN
      rC.fecExc := NVL(rC.fecExc, SYSDATE);
      rC.codMotvExc := NVL(rC.codMotvExc, '0001');
      --
      SELECT MAX(p.CodProd), MAX(cr.CodPlan), MAX(cr.RevPlan)
      INTO cCodProd, cCodPlan, cRevPlan
      FROM Cert_Ramo cr,
        Poliza p
      WHERE p.IdePol = p_nIdePol
        AND cr.IdePol = p.IdePol
        AND cr.NumCert = p_nNumCert
        AND cr.CodRamoCert = rC.codRamo;
      --
      SELECT COUNT(1)
      INTO nExiste
      FROM Cobert_Plan_Prod
      WHERE CodProd = cCodProd
        AND CodRamoPlan = rC.codRamo
        AND CodPlan = cCodPlan
        AND RevPlan = cRevPlan
        AND CodCobert = rC.codCobert
        AND IndCobertOblig = 'S';
      --
      IF nExiste = 0 THEN
        pr_Interfase_AX_SAS.Exclusion_Cobertura_Cert (p_nIdePol, p_nNumCert, rC.codRamo, rC.codCobert, rC.fecExc, rC.codMotvExc, rC.textMotvExc, p_cCodUsr, p_cMsgValida, p_cError);

      ELSE
        p_cError := '1,No se puede excluir una Cobertura Obligatoria';
        pr_Interfase_API_Endoso2.PushError('EXCCOBERT', '1', p_cError, NULL, NULL, p_cXMLError);
      END IF;
      --
      IF p_cError != '0,OK' THEN
        DBMS_OUTPUT.Put_Line('p_cError: ' || p_cError);
        EXIT coberturas;
      END IF;
    END IF;
  END LOOP coberturas;
END EXC_CobertCert;


/*-----------------------------------------------------------------------------
  Nombre      : Activa_Endoso
  Proposito   : Procedimiento para invocar la emisiÛn de Endosos dependiendo del Tipo
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTipoEnd       VARCHAR2   Tipo de Endoso (TipoLVal = 'APIENDRG')
                p_cXMLIn         XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOut        XML (CLob) con resultado del proceso

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    13/04/2022    Christian Bayona           Creacion
    18/04/2022    Christian Bayona           ModificaciÛn, llamada a LVal
    03/05/2022    Christian Bayona           ModificaciÛn, uso de mensajes de error
    25/05/2022    Christian Bayona           ModificaciÛn, validaciÛn de trama correspondiente
    12/07/2022    Renato Cortez              <RTC 313091> Se agregÛ nueva validaciÛn
  ----------------------------------------------------------------------------*/
PROCEDURE Activa_Endoso(
  p_cTipoEnd  IN VARCHAR2,
  p_cXMLIn    IN CLOB,
  --
  p_cXMLOut   IN OUT NOCOPY CLOB) IS
--
cCmd  VARCHAR2(200);
cXML  XMLTYPE := NULL;
--
BEGIN
  --Obtenemos el comando a ejecutar dependiendo al Endoso solicitado
  BEGIN
    SELECT Descrip
    INTO cCmd
    FROM LVal
    WHERE TipoLVal = 'APIENDRG'
      AND CodLVal = p_cTipoEnd;

    --Ejecutamos el endoso correspondiente al Tipo de Endoso solicitado
    BEGIN
      --ValidaciÛn previa de trama remitida corresponde al endoso
      IF INSTR(p_cXMLIn, '<' || GetTagEndoso(p_cTipoEnd, 'I')) = 0 THEN
        PushError('TRAMA', 'VAL-1503', pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1503', cCmd), NULL, NULL, cXML);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto

      ELSE
        EXECUTE IMMEDIATE
          'BEGIN acselx.pr_Interfase_API_Endoso2.' || cCmd || '(:IN, :OUT); END;'
          USING IN p_cXMLIn, IN OUT p_cXMLOut;
      END IF;

    EXCEPTION
      WHEN OTHERS THEN
        -- <I RTC 313091> Renato Cortez - 12/07/2022 - Se agregÛ nueva validaciÛn
        IF SQLCODE = '-31011' THEN
          pr_Interfase_API_Endoso2.PushError('EJECUCION', SQLCODE, 'ERROR EN LA TRAMA XML.', NULL, NULL, cXML);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
        ELSE
          --El procedimiento ":mx1" invocado no existe
          pr_Interfase_API_Endoso2.PushError('EJECUCION', 'VAL-1501', pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1501', cCmd), NULL, NULL, cXML);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
        END IF;
        -- <F RTC 313091>
    END;

  EXCEPTION
    WHEN OTHERS THEN --El procedimiento no registrado correctamente! (CÛdigo: :mx1)
      PushError('EJECUCION', 'VAL-1502', pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-1502', p_cTipoEnd), NULL, NULL, cXML);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
  END;
  --
  IF cXML IS NOT NULL THEN
    SELECT XMLELEMENT("ActivaEndosoResponse",
      XMLELEMENT("resultado", 'ERROR'),
        XMLELEMENT("mensajeValidacionList", cXML))
    INTO cXML
    FROM Dual;
    --
    SELECT XMLSERIALIZE(CONTENT DELETEXML(cXML, '//*[.="" or contains(.,"?")]') INDENT)
    INTO p_cXMLOut
    FROM Dual;
  END IF;
END Activa_Endoso;

/*-----------------------------------------------------------------------------
  Nombre      : Activa_Endoso
  Proposito   : Procedimiento para invocar la emisiÛn de Endosos dependiendo del Tipo
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cTipoEnd       VARCHAR2   Tipo de Endoso (TipoLVal = 'APIENDRG')
                p_cXMLIn         VARCHAR2   con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOut        VARCHAR2   con resultado del proceso

  Log de Cambios
    Fecha         Autor                      DescripciÛn
    19/05/2022    Christian Bayona           Creacion - Procedimiento sobrecargado para uso en forms 6.0
  ----------------------------------------------------------------------------*/
PROCEDURE Activa_Endoso(
  p_cTipoEnd  IN VARCHAR2,
  p_cXMLIn    IN VARCHAR2,
  --
  p_cXMLOut   IN OUT VARCHAR2) IS
--
cXMLOut CLOB;
--
BEGIN
  Activa_Endoso(p_cTipoEnd, TO_CLOB(p_cXMLIn), cXMLOut);
  p_cXMLOut := CAST(cXMLOut AS VARCHAR2);
END Activa_Endoso;

 /*-----------------------------------------------------------------------------
  Nombre      : Prorroga_Vigencia
  Proposito   : Emitir los endosos para generar PrÛrrogas de Vigencia
  Referencias : <RTC-313089> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    15/03/2022    Christian Bayona           Creacion
    01/04/2022    Christian Bayona           ModificaciÛn en llamada a Activar_Poliza_PP
    05/04/2022    Christian Bayona           ModificaciÛn en llamada a Activar_Poliza_PP
    18/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Prorroga_Vigencia ( p_cXMLInput  IN CLOB,
                              p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    fecInicio,
    fecEndoso,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecEndoso DATE PATH 'fecEndoso',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
---
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Prorroga_Vigencia, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Ejecutar procedimiento para generar el endoso de prorroga
      acselx.pr_Interfase_AX.Prorroga_Vigencia(r.idePol, NULL, r.fecEndoso, r.codUsuario, cError);
      --
      IF cError != '0,OK' THEN
        PushError('ENDOSO', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      ELSE
        --Insertar Anexo / Plan Nuevo Formato
        Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Prorroga_Vigencia, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Prorroga_Vigencia;


/*-----------------------------------------------------------------------------
  Nombre      : Modificar_Cant_Trab3D
  Proposito   : ModificaciÛn de n˙mero de trabajadores 3D
  Referencias : <RTC-313092> Endosos de ModificaciÛn de Numero de Trabadores 3D
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    30/03/2022    Christian Bayona           Creacion
    06/04/2022    Christian Bayona           ModificaciÛn en llamada a Activar_Poliza_PP
    18/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    14/06/2022    Christian Bayona           ModificaciÛn, reordenamiento de procedimientos
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Modificar_Cant_Trab3D ( p_cXMLInput  IN CLOB,
                                  p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamo,
    codCober,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    prima,
    tasa,
    sumAseg,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamo PATH 'codRamo',
      codCober PATH 'codCober',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      prima PATH 'prima',
      tasa PATH 'tasa',
      sumAseg PATH 'sumAseg',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
nMtoPrima            NUMBER(10,2);
nTasa                NUMBER(10,2);
nSumAseg             NUMBER(10,2);
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
---
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Cant_Trab3D, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      r.codRamo := NVL(r.codRamo, '3DDD');
      r.codCober := NVL(r.codCober, '3D01');
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      nMtoPrima := NVL(r.Prima, 0);
      nTasa := r.tasa;
      nSumAseg := r.sumAseg;
      cCodUsr := r.codUsuario;
      --Ejecutar procedimiento de Datos Particulares
      Datos_Particulares(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, r.codRamo, l_xml_Error0, cError);
      --<I RTC 316878> / Christian Bayona / 23-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
      pr_Interfase_AX_SAS_Util.Revertir_Cobertura(nIdePol, nNumCert, NULL, cError);
      --
      IF cError != '0,OK' THEN
        PushError('REVCOBERT', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
      END IF;
      --<F RTC 316878>
      IF cError = '0,OK' THEN
        DBMS_OUTPUT.Put_Line('~ ~ ~ ~ ~ ~ ADD_COBERT_CERT_3D inicio');
        acselx.pr_Interfase_AX.Add_Cobert_Cert(nIdePol, nNumCert, r.fecInicio, r.fecFin, nMtoPrima, nTasa, nSumAseg, r.codRamo, r.codCober, cError);
        --
        IF cError != '0,OK' THEN
          PushError('COBERTCERT', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
        END IF;
        DBMS_OUTPUT.Put_Line('~ ~ ~ ~ ~ ~ ADD_COBERT_CERT_3D fin');
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Cant_Trab3D, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Modificar_Cant_Trab3D;


/*-----------------------------------------------------------------------------
  Nombre      : Modificar_Cond_Especiales
  Proposito   : ModificaciÛn de condiciones especiales
  Referencias : <RTC-313091> Endosos Condiciones Especiales
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/03/2022    Christian Bayona           Creacion
    08/04/2022    Christian Bayona           ModificaciÛn
    18/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    22/04/2022    Christian Bayona           ModificaciÛn, asignaciÛn de variables
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    12/09/2023    Edgard LÛpez               <DIGB2B03-3505> [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
  ----------------------------------------------------------------------------*/
PROCEDURE Modificar_Cond_Especiales ( p_cXMLInput  IN CLOB,
                                      p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    --<I DIGB2B03-3505> Edgard LÛpez /12-09-2023/ CONDICION ESPECIAL CON PRIMA - LEGACY
    numCert,
    prima,
    --<F DIGB2B03-3505>
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      numCert PATH 'numCert',  --<DIGB2B03-3505> Edgard LÛpez /12-09-2023/ CONDICION ESPECIAL CON PRIMA - LEGACY
      prima PATH 'prima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--<I DIGB2B03-3505> Edgard LÛpez /12-09-2023/ CONDICION ESPECIAL CON PRIMA - LEGACY
nprima               number;
nnumCert             number;
cTxtAdic_ce          varchar2(10);
--<F DIGB2B03-3505>
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Cond_Especial, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --<I DIGB2B03-3505> Edgard LÛpez /12-09-2023/ CONDICION ESPECIAL CON PRIMA - LEGACY
    nprima := r.Prima;
    nnumCert := r.NumCert;
    --<F DIGB2B03-3505>
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Insertar Anexo / Plan Nuevo Formato
      Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
    END IF;
  END LOOP;
  ---
  --<I DIGB2B03-3505> Edgard LÛpez /12-09-2023/ CONDICION ESPECIAL CON PRIMA - LEGACY
  IF NVL(nprima, 0) > 0 THEN
    GENERA_MOVIMIENTO_PRIMA(nIdePol, nnumCert, 1);
    cTxtAdic_ce := 'NNSS';
  ELSE
    cTxtAdic_ce := 'NNNS';
  END IF;
  --<F DIGB2B03-3505>
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                --<I DIGB2B03-3505> Edgard LÛpez /12-09-2023/ CONDICION ESPECIAL CON PRIMA - LEGACY
                --p_cTxtAdic => 'NNNS',
                p_cTxtAdic => cTxtAdic_ce,
                --<F DIGB2B03-3505>
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Cond_Especial, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Modificar_Cond_Especiales;


/*-----------------------------------------------------------------------------
  Nombre      : Cambio_Asegurado
  Proposito   : Cambio de Asegurado
  Referencias : <RTC-313343> Endosos Cambio de Asegurado
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    12/04/2022    Christian Bayona           Creacion
    18/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    22/04/2022    Christian Bayona           ModificaciÛn, asignaciÛn de variables
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Cambio_Asegurado ( p_cXMLInput  IN CLOB,
                             p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    fecInicio,
    fecEndoso,
    numIdAseg,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecEndoso DATE PATH 'fecEndoso',
      numIdAseg PATH 'numIdAseg',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
tAsegurados          TDU_Tabla_Poliza_Cliente;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Cambio_Asegurado, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecEndoso, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      --Cargar TDUs para cambio de Asegurado
      tAsegurados := TDU_Tabla_Poliza_Cliente();
      Carga_ContAseg(cTag, p_cXMLInput, 'asegurado', 'I', tAsegurados, cError);
      IF cError != '0,OK' THEN
        PushError('CAMBASEG', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto

      ELSE
        IF tAsegurados.COUNT > 0 THEN
          CambioDatosGenCert(nIdePol, nNumCert, 'I', tAsegurados, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
      --
      IF cError = '0,OK' THEN
        Carga_ContAseg(cTag, p_cXMLInput, 'asegurado', 'E', tAsegurados, cError);
        IF tAsegurados.COUNT > 0 THEN
          CambioDatosGenCert(nIdePol, nNumCert, 'E', tAsegurados, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Cambio_Asegurado, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Cambio_Asegurado;


/*-----------------------------------------------------------------------------
  Nombre      : Cesion_Derecho_Inc
  Proposito   : Incluir endoso por CesiÛn de derecho
  Referencias : <RTC-313344> Endosos CesiÛn Derecho InclusiÛn
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    13/04/2022    Christian Bayona           Creacion
    18/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    22/04/2022    Christian Bayona           ModificaciÛn, asignaciÛn de variables
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    22/08/2022    Christian Bayona           <RTC 314183> Se muestra errores
    29/08/2022    Christian Bayona           <RTC 314183> Se agrega validaciÛn de tramas
    02/09/2022    Christian Bayona           <RTC 314183> Se comenta cÛdigo
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    26/05/2023    Manuel Navarro             <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Cesion_Derecho_Inc ( p_cXMLInput  IN CLOB,
                               p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    numId,
    mtoEndoso,
    codTexto,
    texto,
    --<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B, Suma Asegurada VB Tech Core
    --codUsuario
    codUsuario,
    aprobado,
    sustento,
    usuarioAprob
    --<F RTC 316878>
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      numId PATH 'numId',
      mtoEndoso PATH 'mtoEndoso',
      codTexto PATH 'codTexto',
      texto PATH 'texto',
      --<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B, Suma Asegurada VB Tech Core
      --codUsuario PATH 'codUsuario'
      codUsuario PATH 'codUsuario',
      aprobado PATH 'aprobado',
      sustento PATH 'sustento',
      usuarioAprob PATH 'usuarioAprob'
      --<F RTC 316878>
      );
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
--
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Cesion_Derecho_Inc, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      --<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B, Suma Asegurada VB Tech Core
      --acselx.pr_Interfase_AX.Inserta_Endosatario_Cert(r.IdePol, nNumCert, r.numId, r.fecInicio, r.fecFin, r.mtoEndoso, r.texto, '001', r.codTexto, NULL, NULL, cError);
      acselx.pr_Interfase_AX.Inserta_Endosatario_Cert(r.IdePol, nNumCert, r.numId, r.fecInicio, r.fecFin, r.mtoEndoso, r.texto, '001', r.codTexto, NULL, NULL, r.Aprobado, r.Sustento, r.Usuarioaprob, cError);
      --<F RTC 316878>
      --Invocamos a Plan Nuevo Formato de haber registrado el anexo sin problemas
      IF cError = '0,OK' THEN
        Anexo_PNuevoFormato(cTag, cCodProd, r.IdePol, p_cXMLInput, l_xml_Error0, cError);
      --<I RTC 313344> CBayona / 22-08-2022 / Se muestra errores generados
      ELSE
        PushError('CESIONINC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      --<F RTC 313344>
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Cesion_Derecho_Inc, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Cesion_Derecho_Inc;

/*-----------------------------------------------------------------------------
  Nombre      : Modificar_Garantias
  Proposito   : ModificaciÛn de garantÌas
  Referencias : <RTC-313345> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/03/2022    Christian Bayona           Creacion
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    07/06/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimientos
    10/06/2022    Christian Bayona           ModificaciÛn, se adiciono variable a llamada a procedimiento
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    02/09/2022    Christian Bayona           ModificaciÛn, se agrega validaciÛn
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Modificar_Garantias ( p_cXMLInput  IN CLOB,
                                p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Garantias, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Cl·usulas
      ModClausulas(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, cError);

      IF cError = '0,OK' THEN
        --Anexo Certificado
        Anexo_Cert(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, cError);
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        --<I RTC 314183> / CBayona / 02-09-2022 / Se agregÛ validaciÛn
        ELSE
          PushError('TXTANXCERT', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
        --<F RTC 314183>
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Garantias, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Modificar_Garantias;


/*-----------------------------------------------------------------------------
  Nombre      : Cambio_Contratante
  Proposito   : Cambio de contratante
  Referencias : <RTC-313346> Endoso Cambio de Contratante
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/03/2022    Christian Bayona           Creacion
    21/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    30/06/2022    Christian Bayona           <RTC 313091> ModificaciÛn, uso de validaciÛn
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    20/07/2022    Renato Cortez              <RTC 313091> Se agrega validacion paara casos de exclusion de contratante.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Cambio_Contratante ( p_cXMLInput  IN CLOB,
                               p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
tPolizaCliente       acselx.TDU_Tabla_Poliza_Cliente;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Cambio_Contratante, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);--<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Cargar TDUs para cambio de Contratante
      tPolizaCliente := TDU_Tabla_Poliza_Cliente();
      Carga_ContAseg(cTag, p_cXMLInput, 'contratante', 'I', tPolizaCliente, cError);
      IF cError != '0,OK' THEN
        PushError('CAMBCONT', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto

      ELSE
        IF tPolizaCliente.COUNT > 0 THEN
          -- <I RTC 313091> Christian Bayona - 30/06/2022 - Se reemplaza SP por uno que se ajuste a la lÛgica
          --pr_Interfase_AX_SAS.Endoso_Contratantes(nIdePol, 'I', tPolizaCliente, NULL, NULL, cCodUsr, cMsgValida, cError);
          pr_Interfase_AX.Valida_Contratante2(nIdePol, 'I', tPolizaCliente, cMsgValida, cError);
          --
          IF cError = '0,OK' THEN
            pr_Interfase_AX_SAS.Endoso_Contratantes(nIdePol, 'I', tPolizaCliente, NULL, NULL, cCodUsr, cMsgValida, cError);
          END IF;
          -- <F RTC 313091>
        END IF;
      END IF;
      --
      IF cError = '0,OK' THEN
        Carga_ContAseg(cTag, p_cXMLInput, 'contratante', 'E', tPolizaCliente, cError);
        --
        IF tPolizaCliente.COUNT > 0 THEN
          --pr_Interfase_AX_SAS.Endoso_Contratantes(nIdePol, 'E', tPolizaCliente, tPolizaCliente(1).fecExc, tPolizaCliente(1).MotvExc, cCodUsr, cMsgValida, cError); -- <RTC 313091> Renato Cortez - 20/07/2022 - Se comenta
          -- <I RTC 313091> Renato Cortez - 20/07/2022 - Se agrega la validacion para los casos de exclusion de contratante
          pr_Interfase_AX.Valida_Contratante2(nIdePol, 'E', tPolizaCliente, cMsgValida, cError);
          --
          IF cError = '0,OK' THEN
             pr_Interfase_AX_SAS.Endoso_Contratantes(nIdePol, 'E', tPolizaCliente, tPolizaCliente(1).fecExc, tPolizaCliente(1).MotvExc, cCodUsr, cMsgValida, cError);
          END IF;
          -- <F RTC 313091>
        END IF;
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Cambio_Contratante, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Cambio_Contratante;


/*-----------------------------------------------------------------------------
  Nombre      : Cesion_Derecho_exc
  Proposito   : CesiÛn de Derecho
  Referencias : <RTC-313347> Endoso ExclusiÛn de CesiÛn de Derecho
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/03/2022    Christian Bayona           Creacion
    22/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    29/08/2022    Christian Bayona           <RTC 314183> Se agrega validaciÛn de tramas
    01/09/2022    Christian Bayona           <RTC 314183> Se encola errores reportados
    02/09/2022    Christian Bayona           <RTC 314183> Se cambia identificador de error
    05/09/2022    Christian Bayona           <RTC 314183> Se cambia llamada procedimiento para generar texto anexo
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    06/02/2023    Christian Bayona           <RTC 316106> ModificaciÛn, se adiciona par·metro Identificador (txtAdicional)
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Cesion_Derecho_Exc ( p_cXMLInput  IN CLOB,
                               p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    fecExc,
    codExc,
    texto,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecExc DATE PATH 'fecExc',
      codExc PATH 'codExc',
      texto PATH 'texto',
      codUsuario PATH 'codUsuario');
--
CURSOR c_Endosatarios(cTag IN VARCHAR2) IS
  SELECT numId,
    txtAdicional  --<RTC 316106> / CBayona / 06-02-2023 / Se adiciona par·metro
  FROM XMLTABLE(REPLACE('/*/endosatarioList/endosatario', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numId PATH 'numId',
      txtAdicional PATH 'txtAdicional');  --<RTC 316106> / CBayona / 06-02-2023 / Se adiciona par·metro
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
tTablaEndosatCert    acselx.TDU_Tabla_Endosatario_Cert;
--
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Cesion_Derecho_Exc, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      --Cargar TDU
      tTablaEndosatCert := TDU_Tabla_Endosatario_Cert();
      FOR rE IN c_Endosatarios(cTag) LOOP
        tTablaEndosatCert.Extend;
        tTablaEndosatCert(tTablaEndosatCert.LAST) := acselx.TDU_Endosatario_Cert(r.idePol, nNumCert, rE.numId,
          NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, rE.txtAdicional);  --<RTC 316106> / CBayona / 06-02-2023 / Se adiciona par·metro Identificador
      END LOOP;
      --
      acselx.pr_Interfase_AX.Excluye_Endosatario_Cert (r.IdePol, nNumCert, tTablaEndosatCert, r.fecExc, r.codExc, r.texto, cError);
      --<I RTC-313347> / CBayona / 01-09-2022 / Encolar error reportado
      IF cError != '0,OK' THEN
        PushError('CESIONINC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0); --<RTC-313347> / CBayona / 02-09-2022 / Se modifica cÛdigo  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      END IF;
      --<F RTC-313347>

      --Invocamos a Plan Nuevo Formato de haber registrado el anexo sin problemas
      IF cError = '0,OK' THEN
        --Insertar Anexo / Plan Nuevo Formato
        Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);  --<RTC-313347> / CBayona / 02-09-2022 / Se utiliza procedimiento de anexo pol
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Cesion_Derecho_Exc, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Cesion_Derecho_Exc;


/*-----------------------------------------------------------------------------
  Nombre      : Aseg_Adicional_EntFinan
  Proposito   : Endoso de asegurado adicional a entidad financiera
  Referencias : <RTC-313348> Endosos PrÛrrogas de vigencia
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    18/03/2022    Christian Bayona           Creacion
    25/04/2022    Christian Bayona           ModificaciÛn, uso de nuevos procedimientos
    04/05/2022    Christian Bayona           ModificaciÛn, uso de nuevo par·metro
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Aseg_Adicional_EntFinan ( p_cXMLInput  IN CLOB,
                                    p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      prima PATH 'prima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Aseg_Adic_EntFin, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Insertar Anexo / Plan Nuevo Formato
      Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Aseg_Adic_EntFin, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Aseg_Adicional_EntFinan;


/*-----------------------------------------------------------------------------
  Nombre      : Mod_Clausula
  Proposito   : Endoso de ModificaciÛn de Cla˙sulas
  Referencias : <RTC-313746> Endoso ModificaciÛn de Cla˙sulas
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    28/04/2022    Christian Bayona           CreaciÛn
    03/05/2022    Christian Bayona           ModificaciÛn, uso de procedimientos
    16/05/2022    Christian Bayona           ModificaciÛn, se agregÛ nuevo procedimiento
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    07/06/2022    Christian Bayona           ModificaciÛn, uso de nuevo procedimiento
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Clausulas ( p_cXMLInput  IN CLOB,
                          p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Clausulas, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.NumCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      ModClausulas(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, cError);
      --
      IF cError = '0,OK' THEN
        --Insertar Anexo / Plan Nuevo Formato
        Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Clausulas, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Mod_Clausulas;


/*-----------------------------------------------------------------------------
  Nombre      : Incrementa_SA_SubLim
  Proposito   : Endoso de ModificaciÛn de Incremento de Suma Asegurada y SubLimites
  Referencias : <RTC-313752> ModificaciÛn INCREMENTO de suma asegurada y sub lÌmites
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    03/05/2022    Christian Bayona           CreaciÛn
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    14/06/2022    Christian Bayona           ModificaciÛn, reordenamiento de procedimientos
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/08/2022    Christian Bayona           <RTC 314183> Se agregÛ procedimientos para soporte multiendoso
    02/09/2022    Christian Bayona           <RTC 314183> Se comenta cÛdigo
    14/09/2022    Christian Bayona           <RTC 314183> Se agregÛ procedimiento de Cl·usulas para soporte multiendoso
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    06/10/2022    Christian Bayona           <RTC 313350> ModificaciÛn, se adiciona llamada a procedimiento
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Incrementa_SA_SubLim ( p_cXMLInput  IN CLOB,
                                 p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamo,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamo PATH 'codRamo',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Incrementa_SA_SubLim, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Ejecutar procedimiento de Datos Particulares
      Datos_Particulares(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, r.codRamo, l_xml_Error0, cError);
      IF cError = '0,OK' THEN
        Add_CobertCert(cTag, p_cXMLInput, nIdePol, nNumCert, l_xml_Error0, cError);
        --
        --<I RTC 313350> / CBayona / 06-10-2022 / Se agrega llamada a procedimiento de InclusiÛn de Coberturas
        IF cError = '0,OK' THEN
          DBMS_OUTPUT.Put_Line('>>> INC Cobert ' || cError);
          INC_CobertCert(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
        --<F RTC 313350>
        --
        --<I RTC-313752> / CBayona / 26-08-2022 / Se agrega procedimientos para soporte multiendoso
        --CesiÛn InclusiÛn
        IF cError = '0,OK' THEN
          SetCesionINC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --CesiÛn ExclusiÛn
        IF cError = '0,OK' THEN
          SetCesionEXC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --Cambiar Responsable Pago
        IF cError = '0,OK' THEN
          SetRespPago(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError);  --<RTC 313752> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
        END IF;
        --
        --Modificar Deducibles
        IF cError = '0,OK' THEN
          SetDeducible(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, cCodUsr, cMsgValida, cError);
        END IF;
        --<F RTC-313752>
        --
        --<I RTC-313752> / CBayona / 14-09-2022 / Se agrega procedimiento para soporte multiendoso
        --Modificar Cla˙sulas
        IF cError = '0,OK' THEN
          ModClausulas(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, cError);
        END IF;
        --<F RTC-313752>
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Incrementa_SA_SubLim, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Incrementa_SA_SubLim;


/*-----------------------------------------------------------------------------
  Nombre      : Disminuye_SA_SubLim
  Proposito   : Endoso de ModificaciÛn de DisminuciÛn de Suma Asegurada y SubLimites
  Referencias : <RTC-313752> Endosos ModificaciÛn DISMINUCI”N de suma asegurada y sub lÌmites
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    03/05/2022    Christian Bayona           CreaciÛn
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    14/06/2022    Christian Bayona           ModificaciÛn, reordenamiento de procedimientos
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/08/2022    Christian Bayona           <RTC 314183> Se agregÛ procedimientos para soporte multiendoso
    02/09/2022    Christian Bayona           <RTC 314183> Se comenta cÛdigo
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    06/10/2022    Christian Bayona           <RTC 313350> ModificaciÛn, se adiciona llamada a procedimiento
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Disminuye_SA_SubLim ( p_cXMLInput  IN CLOB,
                                p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamo,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamo PATH 'codRamo',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Disminuye_SA_SubLim, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Ejecutar procedimiento de Datos Particulares
      Datos_Particulares(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, r.codRamo, l_xml_Error0, cError);
      --
      IF cError = '0,OK' THEN
        Add_CobertCert(cTag, p_cXMLInput, nIdePol, nNumCert, l_xml_Error0, cError);
        --
        --<F RTC 313350> / CBayona / 06-10-2022 / Se agrego llamada a procedimiento de exclusiÛn de coberturas
        IF cError = '0,OK' THEN
          EXC_CobertCert(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
        --<F RTC 313350>
        --
        --<I RTC-313752> / CBayona / 26-08-2022 / Se agrega procedimientos para soporte multiendoso
        --CesiÛn InclusiÛn
        IF cError = '0,OK' THEN
          SetCesionINC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --CesiÛn ExclusiÛn
        IF cError = '0,OK' THEN
          SetCesionEXC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --Cambiar Responsable Pago
        IF cError = '0,OK' THEN
          SetRespPago(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError);  --<RTC 313752> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
        END IF;
        --
        --Modificar Deducibles
        IF cError = '0,OK' THEN
          SetDeducible(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, cCodUsr, cMsgValida, cError);
        END IF;
        --<F RTC-313752>
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  --
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Disminuye_SA_SubLim, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Disminuye_SA_SubLim;


/*-----------------------------------------------------------------------------
  Nombre      : Exportar_Locales
  Proposito   : Exportar los locales de manera masiva
  Referencias : <RTC-314175> Endoso de ModificaciÛn de Valor Declarado
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    27/06/2022    Christian Bayona           CreaciÛn
    30/06/2022    Christian Bayona           ModificaciÛn, descomentar cÛdigo
    21/09/2023    Manuel Navarro            <DIGB2B03-2815> InclusiÛn Locales Legacy Dubai Masivo
  ----------------------------------------------------------------------------*/
PROCEDURE Exportar_Locales ( p_cXMLInput  IN CLOB,
                             p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqExpLocal IS
  SELECT idePol,
    numCertIni,
    numCertFin,
    codProd,
    estado
  FROM XMLTABLE('/exportarLocal/reqExpLocal' PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCertIni PATH 'numCertIni',
      numCertFin PATH 'numCertFin',
      codProd PATH 'codProd',
      estado PATH 'estado');
--
  nIdePol      Poliza.IdePol%TYPE;
  nNumCertIni  Certificado.NumCert%TYPE;
  nNumCertFin  Certificado.NumCert%TYPE;
  cCodProd     Poliza.CodProd%TYPE;
  cEstado      VARCHAR2(10);
  tExpLocal    acselx.TDU_Tabla_Exp_Local;
  cError       VARCHAR2(2000);
  l_xmlExp     XMLTYPE;
  nCont        NUMBER(6);
--
BEGIN
  FOR r IN c_reqExpLocal LOOP
    nIdePol := r.idePol;
    nNumCertIni := r.numCertIni;
    nNumCertFin := r.numCertFin;
    cCodProd := r.codProd;
    cEstado := r.estado;
    --
    tExpLocal := TDU_Tabla_Exp_Local();
    pr_Interfase_AX.Exporta_Archivo_Locales (r.idePol, r.numCertIni, r.numCertFin, r.codProd, r.estado, tExpLocal, cError);
    nCont := tExpLocal.COUNT;
  END LOOP;
  --
  IF cError = '0,OK' THEN
    SELECT XMLAGG(
      XMLELEMENT("expLocal",
        XMLFOREST(
          e.tipoVia AS "tipoVia",
          e.nomVia AS "nomVia",
          e.numVia AS "numVia",
          e.tipoInt AS "tipoInt",
          e.numInt AS "numInt",
          e.manzana AS "manzana",
          e.lote AS "lote",
          e.kilome AS "kilome",
          e.tipoCha AS "tipoCha",
          --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
          e.nomCha as "nomCha",
          e.subDiv_Ch as "subDiv_Ch",
          e.nomSdi AS "nomSdi",
          --e.subdiv_ch AS "subdiv_ch",
          --e.nombre_subdiv_ch AS "nombre_subdiv_ch",
          --<F DIGB2B03-2815>
          e.referencia AS "referencia",
          e.resto AS "resto",
          e.codPais AS "codPais",
          e.codEstado AS "codEstado",
          e.codCiudad AS "codCiudad",
          e.codMunicipio AS "codMunicipio",
          e.ideGiro AS "ideGiro",
          e.numPisos AS "numPisos",
          e.numSotanos AS "numSotanos",
          e.anoConstrucc AS "anoConstrucc",
          e.usoInmuebleSBS AS "usoInmuebleSBS",
          e.tep AS "tep",
          e.tee AS "tee",
          e.propieEdificacion as "propieEdificacion", --<DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
          e.descBien1 AS "descBien1",
          e.mtoValDeclarDirec1 AS "mtoValDeclarDirec1",
          e.descBien2 AS "descBien2",
          e.mtoValDeclarDirec2 AS "mtoValDeclarDirec2",
          e.descBien3 AS "descBien3",
          e.mtoValDeclarDirec3 AS "mtoValDeclarDirec3",
          e.descBien4 AS "descBien4",
          e.mtoValDeclarDirec4 AS "mtoValDeclarDirec4",
          e.descBien5 AS "descBien5",
          e.mtoValDeclarDirec5 AS "mtoValDeclarDirec5",
          e.descBien6 AS "descBien6",
          e.mtoValDeclarDirec6 AS "mtoValDeclarDirec6",
          e.descBien7 AS "descBien7",
          e.mtoValDeclarDirec7 AS "mtoValDeclarDirec7",
          e.descBien8 AS "descBien8",
          e.mtoValDeclarDirec8 AS "mtoValDeclarDirec8",
          e.descBien9 AS "descBien9",
          e.mtoValDeclarDirec9 AS "mtoValDeclarDirec9",
          e.descBien10 AS "descBien10",
          e.mtoValDeclarDirec10 AS "mtoValDeclarDirec10",
          e.descBien11 AS "descBien11",
          e.mtoValDeclarDirec11 AS "mtoValDeclarDirec11",
          e.descBien12 AS "descBien12",
          e.mtoValDeclarDirec12 AS "mtoValDeclarDirec12",
          e.descBien13 AS "descBien13",
          e.mtoValDeclarDirec13 AS "mtoValDeclarDirec13",
          e.descBien14 AS "descBien14",
          e.mtoValDeclarDirec14 AS "mtoValDeclarDirec14",
          e.descBien15 AS "descBien15",
          e.mtoValDeclarDirec15 AS "mtoValDeclarDirec15",
          e.descBien16 AS "descBien16",
          e.mtoValDeclarDirec16 AS "mtoValDeclarDirec16",
          e.descBien17 AS "descBien17",
          e.mtoValDeclarDirec17 AS "mtoValDeclarDirec17",
          e.descBien18 AS "descBien18",
          e.mtoValDeclarDirec18 AS "mtoValDeclarDirec18",
          e.valor_Declarado_Total AS "valor_Declarado_Total",
          e.porcSuma AS "porcSuma",
          e.fecInspeccion AS "fecInspeccion",
          e.numCert AS "numCert",
          e.cod_Local AS "cod_Local",
          e.indInspeccion AS "indInspeccion",
          e.sujeto_Inspeccion AS "sujeto_Inspeccion",
          e.rer AS "rer",
          e.indSinInspeccion AS "indSinInspeccion",
          e.usuAutorizador AS "usuAutorizador",
          e.stsDirec AS "stsDirec",
          e.cumple_Norma_Sbs AS "cumple_Norma_Sbs",
          e.indGeoRef AS "indGeoRef",
          e.clasificacion AS "clasificacion")
        )
      )
    INTO l_xmlExp
    FROM TABLE(tExpLocal) e;
  END IF;
  --
  SELECT XMLSERIALIZE(CONTENT DELETEXML(
      XMLELEMENT("exportarLocalResponse",
        XMLELEMENT("idePol", nIdePol),
        XMLELEMENT("numCertIni", nNumCertIni),
        XMLELEMENT("numCertFin", nNumCertFin),
        XMLELEMENT("codProd", cCodProd),
        XMLELEMENT("estado", cEstado),
        XMLELEMENT("resultado", nCont),
        XMLELEMENT("expLocalList", l_xmlExp)
      ), '//*[.="" or contains(.,"?")]') INDENT)
  INTO p_cXMLOutput
  FROM Dual;
END Exportar_Locales;


/*-----------------------------------------------------------------------------
  Nombre      : Mod_Valor_Declarado
  Proposito   : Endoso de ModificaciÛn de Valor Declarado
  Referencias : <RTC-314175> Endosos ModificaciÛn de Valor Declarado
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    03/05/2022    Christian Bayona           CreaciÛn
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    27/06/2022    Christian Bayona           <RTC 313091> ModificaciÛn, uso de nuevos procedimientos
    30/06/2022    Christian Bayona           <RTC 313091> ModificaciÛn, uso de nuevos procedimientos
    12/07/2022    Renato Cortez              <RTC 313091> Se agregÛ nueva lÛgica del proceso
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/08/2022    Christian Bayona           <RTC 314183> Se agregÛ procedimientos para soporte multiendoso
    07/09/2022    Christian Bayona           <RTC 314183> Se agregÛ soporte para tramas adicionales
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    15/11/2022    Christian Bayona           <RTC 315519> ModificaciÛn, se adiciona nuevo atributo - Mejoras en Acsel/x para soportar cambios nueva versiÛn Carga SBS
    23/01/2023    Christian Bayona           <RTC 316106> ModificaciÛn, se adiciona nuevos par·metros
    31/03/2023    Christian Bayona           <RTC 316747> [Proy API Endoso] ImpresiÛn Vehicular
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    21/09/2023    Manuel Navarro            <DIGB2B03-2815> InclusiÛn Locales Legacy Dubai Masivo
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Valor_Declarado ( p_cXMLInput  IN CLOB,
                                p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    -- <I RTC 313091> Christian Bayona - 30/06/2022
    codRamo,
    codMoneda,
    -- <F RTC 313091>
    --<I RTC-316106> / CBayona / 23-01-2023 / Nuevos atributos
    fecInicio,
    fecFin,
    --<F RTC-316106> / CBayona / 23-01-2023
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      -- <I RTC 313091> Christian Bayona - 30/06/2022
      --prima PATH 'prima',
      codRamo PATH 'codRamo',
      codMoneda PATH 'codMoneda',
      -- <F RTC 313091>
      --<I RTC-316106> / CBayona / 23-01-2023 / Nuevos atributos
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      --<F RTC-316106> / CBayona / 23-01-2023
      codUsuario PATH 'codUsuario');
--
-- <I RTC 313091> Renato Cortez - 12/07/2022 - Se agregÛ nueva lÛgica del proceso
CURSOR c_datosPartxCertList(cTag IN VARCHAR2) IS
  SELECT numCert
  FROM XMLTABLE(REPLACE('/*/datosPartxCertList/datosPartxCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numCert PATH 'numCert');
--
CURSOR c_datosPartList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCrit,
    codTrasf,
    valTrasf,
    numOrd,
    valCodTab,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/datosPartxCertList/datosPartxCert/datosPartList/datoParticular', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCrit PATH 'codCrit',
      codTrasf PATH 'codTrasf',
      valTrasf PATH 'valTrasf',
      numOrd PATH 'numOrd',
      valCodTab PATH 'valCodTab',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_impLocalList(cTag IN VARCHAR2) IS
  SELECT codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    ubiGeo,
    tipoVia,
    nomVia,
    numVia,
    tipoInt,
    numInt,
    manzana,
    lote,
    kilome,
    tipoCha,
    nomCha,
    tipoSdi,
    nomSdi,
    referencia,
    resto,
    porcSuma,
    ideGiro,
    mtoValDeclarDirec,
    tep,
    tee,
    anoConstrucc,
    numPisos,
    numSotanos,
    fecInspeccion,
    numCert,
    ideDirec,
    indRiesgoIns,
    indInspeccion,
    notaInspeccion,
    indSinInspeccion,
    usuAutorizador,
    claseTipo,
    --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
    --propieEdificacion  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    propieEdificacion,  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    usoInmueble,
    edificio,
    edificOficina,
    edificotro1,
    edificotro2,
    edificotro3,
    contExistencias,
    contMaquinFija,
    contMaqMovilAcces,
    contEquipoElec,
    contMobiliarioGral,
    contOtro1,
    contOtro2,
    contOtro3,
    contOtro4,
    contOtro5,
    contOtro6,
    benefBruto,
    gastosEstable
    --<F DIGB2B03-2815> 
  FROM XMLTABLE(REPLACE('/*/impLocalList/impLocal', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      ubiGeo PATH 'ubiGeo',
      tipoVia PATH 'tipoVia',
      nomVia PATH 'nomVia',
      numVia PATH 'numVia',
      tipoInt PATH 'tipoInt',
      numInt PATH 'numInt',
      manzana PATH 'manzana',
      lote PATH 'lote',
      kilome PATH 'kilome',
      tipoCha PATH 'tipoCha',
      nomCha PATH 'nomCha',
      tipoSdi PATH 'tipoSdi',
      nomSdi PATH 'nomSdi',
      referencia PATH 'referencia',
      resto PATH 'resto',
      porcSuma PATH 'porcSuma',
      ideGiro PATH 'ideGiro',
      mtoValDeclarDirec PATH 'mtoValDeclarDirec',
      tep PATH 'tep',
      tee PATH 'tee',
      anoConstrucc PATH 'anoConstrucc',
      numPisos PATH 'numPisos',
      numSotanos PATH 'numSotanos',
      fecInspeccion DATE PATH 'fecInspeccion',-- <RTC 313091> Renato Cortez - 12/07/2022 - Se agregÛ nueva lÛgica del proceso
      numCert PATH 'numCert',
      ideDirec PATH 'ideDirec',
      indRiesgoIns PATH 'indRiesgoIns',
      indInspeccion PATH 'indInspeccion',
      notaInspeccion PATH 'notaInspeccion',
      indSinInspeccion PATH 'indSinInspeccion',
      usuAutorizador PATH 'usuAutorizador',
      claseTipo PATH 'claseTipo',
      --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
      --propieEdificacion PATH 'propieEdificacion');  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      propieEdificacion PATH 'propieEdificacion', --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      usoInmueble PATH 'usoInmueble',  --DIGB2B03
      edificio PATH 'edificio',
      edificOficina PATH 'edificOficina',
      edificotro1 PATH 'edificOtro1',
      edificotro2 PATH 'edificOtro2',
      edificotro3 PATH 'edificOtro3',
      contExistencias PATH 'contExistencias',
      contMaquinFija PATH 'contMaquinFija',
      contMaqMovilAcces PATH 'contMaqMovilAcces',
      contEquipoElec PATH 'contEquipoElec',
      contMobiliarioGral PATH 'contMobiliarioGral',
      contOtro1 PATH 'contOtro1',
      contOtro2 PATH 'contOtro2',
      contOtro3 PATH 'contOtro3',
      contOtro4 PATH 'contOtro4',
      contOtro5 PATH 'contOtro5',
      contOtro6 PATH 'contOtro6',
      benefBruto PATH 'benefBruto',
      gastosEstable PATH 'gastosEstable');   
      --<F DIGB2B03-2815>      
-- <F RTC 313091>
--
--<I RTC 314175> / CBayona / 07-09-2022 / Se agrega soporte para nuevas tramas
CURSOR c_direcRiesgoCertList(cTag IN VARCHAR2) IS
  SELECT ideDirec,
    codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    tipoVia,
    nomVia,
    numVia,
    tipoInt,
    numInt,
    manzana,
    lote,
    kilome,
    tipoCha,
    nomCha,
    tipoSdi,
    nomSdi,
    referencia,
    resto,
    ideGiro,
    mtoValDeclarDirec,
    porcSuma,
    usoInmueble,
    tep,
    anoConstrucc,
    numPisos,
    numSotanos,
    tipoGeo,
    xGeo,
    yGeo,
    indInspeccion,
    indRiesgoIns,
    indSinInspeccion,
    notaInspeccion,
    usuAutorizador,
    fecInspeccion,
    tipoConstrucc,
    fecExc,
    codMotvExc,
    texTmotvExc,
    txtAdicional,
    tee,
    usoInmuebleSBS,
    propieEdificacion  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
  FROM XMLTABLE(REPLACE('/*/direcRiesgoCertList/direcRiesgoCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      ideDirec PATH 'ideDirec',
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      tipoVia PATH 'tipoVia',
      nomVia PATH 'nomVia',
      numVia PATH 'numVia',
      tipoInt PATH 'tipoInt',
      numInt PATH 'numInt',
      manzana PATH 'manzana',
      lote PATH 'lote',
      kilome PATH 'kilome',
      tipoCha PATH 'tipoCha',
      nomCha PATH 'nomCha',
      tipoSdi PATH 'tipoSdi',
      nomSdi PATH 'nomSdi',
      referencia PATH 'referencia',
      resto PATH 'resto',
      ideGiro PATH 'ideGiro',
      mtoValDeclarDirec PATH 'mtoValDeclarDirec',
      porcSuma PATH 'porcSuma',
      usoInmueble PATH 'usoInmueble',
      tep PATH 'tep',
      anoConstrucc PATH 'anoConstrucc',
      numPisos PATH 'numPisos',
      numSotanos PATH 'numSotanos',
      tipoGeo PATH 'tipoGeo',
      xGeo PATH 'xGeo',
      yGeo PATH 'yGeo',
      indInspeccion PATH 'indInspeccion',
      indRiesgoIns PATH 'indRiesgoIns',
      indSinInspeccion PATH 'indSinInspeccion',
      notaInspeccion PATH 'notaInspeccion',
      usuAutorizador PATH 'usuAutorizador',
      fecInspeccion DATE PATH 'fecInspeccion',
      tipoConstrucc PATH 'tipoConstrucc',
      fecExc DATE PATH 'fecExc',
      codMotvExc PATH 'codMotvExc',
      texTmotvExc PATH 'texTmotvExc',
      txtAdicional PATH 'txtAdicional',
      tee PATH 'tee',
      usoInmuebleSBS PATH 'usoInmuebleSBS',
      propieEdificacion PATH 'propieEdificacion'  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      );
--
CURSOR c_bienCertRiesgoList(cTag IN VARCHAR2) IS
  SELECT ideDirec,
    mtoValDeclarBien,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/bienCertRiesgoList/bienCertRiesgo', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      ideDirec PATH 'ideDirec',
      mtoValDeclarBien PATH 'mtoValDeclarBien',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_endosatarioBienList(cTag IN VARCHAR2) IS
  SELECT numId,
    mtoEndoso,
    fecIng,
    codTexto,
    stsEnd,
    numPrestamo,
    porcPart,
    textoBien,
    fecExc,
    codMotvExclu,
    textoMotvExclu,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/endosatarioBienList/endosatarioBien', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numId PATH 'numId',
      mtoEndoso PATH 'mtoEndoso',
      fecIng PATH 'fecIng',
      codTexto PATH 'codTexto',
      stsEnd PATH 'stsEnd',
      numPrestamo PATH 'numPrestamo',
      porcPart PATH 'porcPart',
      textoBien PATH 'textoBien',
      fecExc PATH 'fecExc',
      codMotvExclu PATH 'codMotvExclu',
      textoMotvExclu PATH 'textoMotvExclu',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_cobertCertList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCobert,
    stsCobert,
    sumaAsegMoneda,
    tasa,
    primaMoneda,
    codMoneda,
    prima,
    sumaAseg,
    codMonedaCob,
    sumaAsegMonedaCob,
    fecIniValid,
    fecFinValid,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/cobertCertList/cobertCer', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCobert PATH 'codCobert',
      stsCobert PATH 'stsCobert',
      sumaAsegMoneda PATH 'sumaAsegMoneda',
      tasa PATH 'tasa',
      primaMoneda PATH 'primaMoneda',
      codMoneda PATH 'codMoneda',
      prima PATH 'prima',
      sumaAseg PATH 'sumaAseg',
      codMonedaCob PATH 'codMonedaCob',
      sumaAsegMonedaCob PATH 'sumaAsegMonedaCob',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_deducibleCobertList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCobert,
    numDed,
    baseDedcalculo,
    porcDedcalculo,
    mtoDedFijo,
    baseDedMinimo,
    porcDedMinimo,
    mtoDedMinimo,
    baseDedMaximo,
    porcDedMaximo,
    mtoDedMaximo,
    codEst,
    valEst,
    textoDeduc,
    ideDeduc,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/deducibleCobertList/deducibleCobert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCobert PATH 'codCobert',
      numDed PATH 'numDed',
      baseDedcalculo PATH 'baseDedcalculo',
      porcDedcalculo PATH 'porcDedcalculo',
      mtoDedFijo PATH 'mtoDedFijo',
      baseDedMinimo PATH 'baseDedMinimo',
      porcDedMinimo PATH 'porcDedMinimo',
      mtoDedMinimo PATH 'mtoDedMinimo',
      baseDedMaximo PATH 'baseDedMaximo',
      porcDedMaximo PATH 'porcDedMaximo',
      mtoDedMaximo PATH 'mtoDedMaximo',
      codEst PATH 'codEst',
      valEst PATH 'valEst',
      textoDeduc PATH 'textoDeduc',
      ideDeduc PATH 'ideDeduc',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_bienxCertList(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamoCert,
    fecIniValid,
    fecFinValid,
    claseBien,
    codBien,
    codMonedaBien,
    mtoValDeclarBien,
    porcRiesgo,
    indImprDeclar,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/bienxCertList/bienxCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamoCert PATH 'codRamoCert',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      claseBien PATH 'claseBien',
      codBien PATH 'codBien',
      codMonedaBien PATH 'codMonedaBien',
      mtoValDeclarBien PATH 'mtoValDeclarBien',
      porcRiesgo PATH 'porcRiesgo',
      indImprDeclar PATH 'indImprDeclar',
      txtAdicional PATH 'txtAdicional');
--<F RTC 314175>
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
-- <I RTC 313091> Christian Bayona - 30/06/2022
nNumCert             Certificado.NumCert%TYPE;
tDatosParticul       TDU_Tabla_Datos_Partic;
tImp_Local           acselx.TDU_Tabla_Imp_Local;
-- <I RTC 314175> / CBayona / 07-09-2022 / Variables adicionadas
tDirecRiesgoCert     TDU_Tabla_Direc_Riesgo_Cert;
tBienCertRiesgo      TDU_Tabla_Bien_Cert_Riesgo;
tEndosatarioBien     TDU_Tabla_Endosatario_Bien;
tCobertBien          TDU_Tabla_Cobert_Cert;
tDeducibleCobert     TDU_Tabla_Deducible_Cobert;
tBienes_Cert         TDU_T_Tabla_BienesxCert;
--<F RTC 314175>
-- <F RTC 313091>
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Valor_Declarado, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Cargar TDU's
      -- <I RTC 313091> Renato Cortez - 12/07/2022 - Se agregÛ nueva lÛgica del proceso
      FOR t IN c_datosPartxCertList(cTag) LOOP
        tDatosParticul := TDU_Tabla_Datos_Partic();
        nNumCert := t.numCert;
        FOR dp IN c_datosPartList(cTag) LOOP
          --Establecer valores por defecto
          dp.codTrasf := NVL(dp.codTrasf, '000');
          dp.valCodTab := NVL(dp.valCodTab, '000');
          --
          tDatosParticul.Extend;
          tDatosParticul(tDatosParticul.LAST) :=
            TDU_Datos_Partic(dp.codRamoCert, dp.codCrit, dp.codTrasf, dp.valTrasf, dp.numOrd, dp.valCodTab, dp.txtAdicional);
        END LOOP;
        --
        pr_Interfase_AX.Act_Datos_Particulares_ValDec (nIdePol, nNumCert, r.codRamo, r.fecInicio, r.fecFin, tDatosParticul, cError); --<RTC-316106> / CBayona / 23-01-2023 / Nuevos par·metros
        --
        IF cError != '0,OK' THEN
          PushError('DATPARTVD', '1', cError, NULL, NULL, l_xml_Error0); --<RTC 316747> / CBayona / 31-03-2023 / [Proy API Endoso] ImpresiÛn Vehicular
          EXIT;
        END IF;
      END LOOP;
      --
      IF cError = '0,OK' THEN
        tImp_Local := TDU_Tabla_Imp_Local();
        FOR t IN c_impLocalList(cTag) LOOP
          tImp_Local.Extend;
          tImp_Local(tImp_Local.LAST) :=
            TDU_Imp_Local(t.codPais, t.codEstado, t.codCiudad, t.codMunicipio, t.ubiGeo, t.tipoVia, t.nomVia, t.numVia, t.tipoInt, t.numInt,
              t.manzana, t.lote, t.kilome, t.tipoCha, t.nomCha, t.tipoSdi, t.nomSdi, t.referencia, t.resto, t.porcSuma, t.ideGiro,
              t.mtoValDeclarDirec, t.tep, t.tee, t.anoConstrucc, t.numPisos, t.numSotanos, t.fecInspeccion, t.numCert, t.ideDirec,
              --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
              --t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion);  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
              t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion,  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
              t.usoInmueble, t.edificio, t.edificOficina, t.edificotro1, t.edificotro2, t.edificotro3, t.contExistencias, t.contMaquinFija, 
              t.contMaqMovilAcces, t.contEquipoElec, t.contMobiliarioGral, t.contOtro1, t.contOtro2, t.contOtro3,t.contOtro4, t.contOtro5,
              t.contOtro6, t.benefBruto, t.gastosEstable);  
              --<F DIGB2B03-2815> 
        END LOOP;
        --
        --<I RTC 314175> / CBayona / 07-09-2022 / Carga de TDU's
        --
        tDirecRiesgoCert := TDU_Tabla_Direc_Riesgo_Cert();
        FOR t IN c_direcRiesgoCertList(cTag) LOOP
           tDirecRiesgoCert.Extend;
          tDirecRiesgoCert(tDirecRiesgoCert.LAST) := TDU_Direc_Riesgo_Cert(t.ideDirec, t.codPais, t.codEstado, t.codCiudad, t.codMunicipio,
            t.tipoVia, t.nomVia, t.numVia, t.tipoInt, t.numInt, t.manzana, t.lote, t.kilome, t.tipoCha, t.nomCha, t.tipoSdi, t.nomSdi,
            t.referencia, t.resto, t.ideGiro, t.mtoValDeclarDirec, t.porcSuma, t.usoInmueble, t.tep, t.anoConstrucc, t.numPisos, t.numSotanos,
            t.tipoGeo, t.xGeo, t.yGeo, t.indInspeccion, t.indRiesgoIns, t.indSinInspeccion, t.notaInspeccion, t.usuAutorizador, t.fecInspeccion,
            t.tipoConstrucc, t.fecExc, t.codMotvExc, t.texTmotvExc, t.txtAdicional, t.tee, t.usoInmuebleSBS, t.propieEdificacion);  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
        END LOOP;
        --
        tBienCertRiesgo := TDU_Tabla_Bien_Cert_Riesgo();
        FOR t IN c_bienCertRiesgoList(cTag) LOOP
          tBienCertRiesgo.Extend;
          tBienCertRiesgo(tBienCertRiesgo.LAST) := TDU_Bien_Cert_Riesgo(t.ideDirec, t.mtoValDeclarBien, t.txtAdicional);
        END LOOP;
        --
        tEndosatarioBien := TDU_Tabla_Endosatario_Bien();
        FOR t IN c_endosatarioBienList(cTag) LOOP
          tEndosatarioBien.Extend;
          tEndosatarioBien(tEndosatarioBien.LAST) := TDU_Endosatario_Bien(t.numId, t.mtoEndoso, t.fecIng, t.codTexto, t.stsEnd, t.numPrestamo, t.porcPart,
            t.textoBien, t.fecExc, t.codMotvExclu, t.textoMotvExclu, t.txtAdicional);
        END LOOP;
        --
        tCobertBien := TDU_Tabla_Cobert_Cert();
        FOR t IN c_cobertCertList(cTag) LOOP
          tCobertBien.Extend;
          tCobertBien(tCobertBien.LAST) := TDU_Cobert_Cert(t.codRamoCert, t.codCobert, t.stsCobert, t.sumaAsegMoneda, t.tasa, t.primaMoneda, t.codMoneda,
            t.prima, t.sumaAseg, t.codMonedaCob, t.sumaAsegMonedaCob, t.fecIniValid, t.fecFinValid, t.txtAdicional);
        END LOOP;
        --
        tDeducibleCobert := TDU_Tabla_Deducible_Cobert();
        FOR t IN c_deducibleCobertList(cTag) LOOP
          tDeducibleCobert.Extend;
          tDeducibleCobert(tDeducibleCobert.LAST) := TDU_Deducible_Cobert(t.codRamoCert, t.codCobert, t.numDed, t.baseDedcalculo, t.porcDedcalculo,
            t.mtoDedFijo, t.baseDedMinimo, t.porcDedMinimo, t.mtoDedMinimo, t.baseDedMaximo, t.porcDedMaximo, t.mtoDedMaximo, t.codEst, t.valEst,
            t.textoDeduc, t.ideDeduc, t.txtAdicional);
        END LOOP;
        --
        tBienes_Cert := TDU_T_Tabla_BienesxCert();
        FOR t IN c_bienxCertList(cTag) LOOP
          --Establecer valores por defecto
          t.idePol := nIdePol;
          t.numCert := nNumCert;
          --
          tBienes_Cert.Extend;
          tBienes_Cert(tBienes_Cert.LAST) := TDU_Tabla_BienesxCert(t.idePol, t.numCert, t.codRamoCert, t.fecIniValid, t.fecFinValid, t.claseBien, t.codBien,
            t.codMonedaBien, t.mtoValDeclarBien, t.porcRiesgo, t.indImprDeclar, NULL, tEndosatarioBien, tCobertBien, tDeducibleCobert, t.txtAdicional);
        END LOOP;
        --<F RTC 314175>
        --
        pr_Interfase_AX.Modicar_Valores_Declarados(nIdePol, cCodProd, r.codMoneda, tImp_Local, tBienes_Cert, tBienCertRiesgo, cError); --<RTC-314175> / CBayona / 07-09-2022 / Se agrega nuevos par·metros
        --<I RTC 316106> / CBayona / 23-01-2023 / Se encola errores remitidos al Modificar los Valores Declarados
        IF cError != '0,OK' THEN
          PushError('MODVALDEC', '1', cError, NULL, NULL, l_xml_Error0);
        END IF;
        --<F RTC 316106>
        --
        --<I RTC-314175> / CBayona / 26-08-2022 / Se agrega procedimientos para soporte multiendoso
        --CesiÛn InclusiÛn
        IF cError = '0,OK' THEN
          SetCesionINC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --CesiÛn ExclusiÛn
        IF cError = '0,OK' THEN
          SetCesionEXC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --Cambiar Responsable Pago
        IF cError = '0,OK' THEN
          SetRespPago(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError); --<RTC 314175> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
        END IF;
        --<F RTC-314175>
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
      -- <F RTC 313091>
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',-- <RTC 313091> Christian Bayona - 30/06/2022
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Valor_Declarado, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);-- <RTC 313091> Christian Bayona - 30/06/2022
END Mod_Valor_Declarado;


/*-----------------------------------------------------------------------------
  Nombre      : Mod_Locales
  Proposito   : Endoso de ModificaciÛn de Locales
  Referencias : <RTC-313742> Endosos de ModificaciÛn de Locales
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    03/05/2022    Christian Bayona           CreaciÛn
    19/05/2022    Christian Bayona           ModificaciÛn a llamada de anexo
    23/05/2022    Christian Bayona           ModificaciÛn, de variables
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    14/06/2022    Christian Bayona           ModificaciÛn, actualizaciÛn de tramas
    20/06/2022    Christian Bayona           ModificaciÛn, se comento carga TDU por pase a prod
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/08/2022    Christian Bayona           <RTC 314183> Se agregÛ procedimientos para soporte multiendoso
    14/09/2022    Christian Bayona           <RTC 314183> Se agregÛ procedimiento de Cl·usulas para soporte multiendoso
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    15/11/2022    Christian Bayona           <RTC 315519> ModificaciÛn, se adiciona nuevo atributo - Mejoras en Acsel/x para soportar cambios nueva versiÛn Carga SBS
    23/01/2023    Christian Bayona           <RTC 316106> ModificaciÛn, se adiciona nuevos par·metros
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    21/09/2023    Manuel Navarro             <DIGB2B03-2815> InclusiÛn Locales Legacy Dubai Masivo
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Locales ( p_cXMLInput  IN CLOB,
                        p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codProd,
    codRamo,
    numPol,
    fecInicio,
    fecFin,
    claseBien,
    codBien,
    codMonedaBien,
    sumAseg,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codProd PATH 'codProd',
      codRamo PATH 'codRamo',
      numPol PATH 'numPol',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      claseBien PATH 'claseBien',
      codBien PATH 'codBien',
      codMonedaBien PATH 'codMonedaBien',
      sumAseg PATH 'sumAseg',
      codUsuario PATH 'codUsuario');
--
CURSOR c_datosPartList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCrit,
    codTrasf,
    valTrasf,
    numOrd,
    valCodTab,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/datosPartxCertList/datosPartxCert/datosPartList/datoParticular', '*', cTag) PASSING xmltype(p_cXMLInput)  --<I RTC-313742> / CBayona / 27-09-2022 / Se modifica ruta de datos
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCrit PATH 'codCrit',
      codTrasf PATH 'codTrasf',
      valTrasf PATH 'valTrasf',
      numOrd PATH 'numOrd',
      valCodTab PATH 'valCodTab',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_direcRiesgoCertList(cTag IN VARCHAR2) IS
  SELECT ideDirec,
    codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    tipoVia,
    nomVia,
    numVia,
    tipoInt,
    numInt,
    manzana,
    lote,
    kilome,
    tipoCha,
    nomCha,
    tipoSdi,
    nomSdi,
    referencia,
    resto,
    ideGiro,
    mtoValDeclarDirec,
    porcSuma,
    usoInmueble,
    tep,
    anoConstrucc,
    numPisos,
    numSotanos,
    tipoGeo,
    xGeo,
    yGeo,
    indInspeccion,
    indRiesgoIns,
    indSinInspeccion,
    notaInspeccion,
    usuAutorizador,
    fecInspeccion,
    tipoConstrucc,
    fecExc,
    codMotvExc,
    texTmotvExc,
    txtAdicional,
    tee,
    usoInmuebleSBS,  --<RTC 313091> Christian Bayona - 20/06/2022
    propieEdificacion  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
  FROM XMLTABLE(REPLACE('/*/direcRiesgoCertList/direcRiesgoCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      ideDirec PATH 'ideDirec',
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      tipoVia PATH 'tipoVia',
      nomVia PATH 'nomVia',
      numVia PATH 'numVia',
      tipoInt PATH 'tipoInt',
      numInt PATH 'numInt',
      manzana PATH 'manzana',
      lote PATH 'lote',
      kilome PATH 'kilome',
      tipoCha PATH 'tipoCha',
      nomCha PATH 'nomCha',
      tipoSdi PATH 'tipoSdi',
      nomSdi PATH 'nomSdi',
      referencia PATH 'referencia',
      resto PATH 'resto',
      ideGiro PATH 'ideGiro',
      mtoValDeclarDirec PATH 'mtoValDeclarDirec',
      porcSuma PATH 'porcSuma',
      usoInmueble PATH 'usoInmueble',
      tep PATH 'tep',
      anoConstrucc PATH 'anoConstrucc',
      numPisos PATH 'numPisos',
      numSotanos PATH 'numSotanos',
      tipoGeo PATH 'tipoGeo',
      xGeo PATH 'xGeo',
      yGeo PATH 'yGeo',
      indInspeccion PATH 'indInspeccion',
      indRiesgoIns PATH 'indRiesgoIns',
      indSinInspeccion PATH 'indSinInspeccion',
      notaInspeccion PATH 'notaInspeccion',
      usuAutorizador PATH 'usuAutorizador',
      fecInspeccion DATE PATH 'fecInspeccion',
      tipoConstrucc PATH 'tipoConstrucc',
      fecExc DATE PATH 'fecExc',
      codMotvExc PATH 'codMotvExc',
      texTmotvExc PATH 'texTmotvExc',
      txtAdicional PATH 'txtAdicional',
      tee PATH 'tee',
      usoInmuebleSBS PATH 'usoInmuebleSBS',  --<RTC 313091> Christian Bayona - 20/06/2022
      propieEdificacion PATH 'propieEdificacion'  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      );
--
CURSOR c_bienCertRiesgoList(cTag IN VARCHAR2) IS
  SELECT ideDirec,
    mtoValDeclarBien,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/bienCertRiesgoList/bienCertRiesgo', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      ideDirec PATH 'ideDirec',
      mtoValDeclarBien PATH 'mtoValDeclarBien',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_endosatarioBienList(cTag IN VARCHAR2) IS
  SELECT numId,
    mtoEndoso,
    fecIng,
    codTexto,
    stsEnd,
    numPrestamo,
    porcPart,
    textoBien,
    fecExc,
    codMotvExclu,
    textoMotvExclu,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/endosatarioBienList/endosatarioBien', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numId PATH 'numId',
      mtoEndoso PATH 'mtoEndoso',
      fecIng PATH 'fecIng',
      codTexto PATH 'codTexto',
      stsEnd PATH 'stsEnd',
      numPrestamo PATH 'numPrestamo',
      porcPart PATH 'porcPart',
      textoBien PATH 'textoBien',
      fecExc PATH 'fecExc',
      codMotvExclu PATH 'codMotvExclu',
      textoMotvExclu PATH 'textoMotvExclu',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_cobertCertList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCobert,
    stsCobert,
    sumaAsegMoneda,
    tasa,
    primaMoneda,
    codMoneda,
    prima,
    sumaAseg,
    codMonedaCob,
    sumaAsegMonedaCob,
    fecIniValid,
    fecFinValid,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/cobertCertList/cobertCer', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCobert PATH 'codCobert',
      stsCobert PATH 'stsCobert',
      sumaAsegMoneda PATH 'sumaAsegMoneda',
      tasa PATH 'tasa',
      primaMoneda PATH 'primaMoneda',
      codMoneda PATH 'codMoneda',
      prima PATH 'prima',
      sumaAseg PATH 'sumaAseg',
      codMonedaCob PATH 'codMonedaCob',
      sumaAsegMonedaCob PATH 'sumaAsegMonedaCob',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_deducibleCobertList(cTag IN VARCHAR2) IS
  SELECT codRamoCert,
    codCobert,
    numDed,
    baseDedcalculo,
    porcDedcalculo,
    mtoDedFijo,
    baseDedMinimo,
    porcDedMinimo,
    mtoDedMinimo,
    baseDedMaximo,
    porcDedMaximo,
    mtoDedMaximo,
    codEst,
    valEst,
    textoDeduc,
    ideDeduc,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/deducibleCobertList/deducibleCobert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codRamoCert PATH 'codRamoCert',
      codCobert PATH 'codCobert',
      numDed PATH 'numDed',
      baseDedcalculo PATH 'baseDedcalculo',
      porcDedcalculo PATH 'porcDedcalculo',
      mtoDedFijo PATH 'mtoDedFijo',
      baseDedMinimo PATH 'baseDedMinimo',
      porcDedMinimo PATH 'porcDedMinimo',
      mtoDedMinimo PATH 'mtoDedMinimo',
      baseDedMaximo PATH 'baseDedMaximo',
      porcDedMaximo PATH 'porcDedMaximo',
      mtoDedMaximo PATH 'mtoDedMaximo',
      codEst PATH 'codEst',
      valEst PATH 'valEst',
      textoDeduc PATH 'textoDeduc',
      ideDeduc PATH 'ideDeduc',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_bienxCertList(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamoCert,
    fecIniValid,
    fecFinValid,
    claseBien,
    codBien,
    codMonedaBien,
    mtoValDeclarBien,
    porcRiesgo,
    indImprDeclar,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/bienxCertList/bienxCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamoCert PATH 'codRamoCert',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      claseBien PATH 'claseBien',
      codBien PATH 'codBien',
      codMonedaBien PATH 'codMonedaBien',
      mtoValDeclarBien PATH 'mtoValDeclarBien',
      porcRiesgo PATH 'porcRiesgo',
      indImprDeclar PATH 'indImprDeclar',
      txtAdicional PATH 'txtAdicional');

--<I RTC-313742> / CBayona / 27-09-2022 / Se agrega cursores de Datos Part por Certificado e ImportaciÛn de locales
CURSOR c_datosPartxCertList(cTag IN VARCHAR2) IS
  SELECT numCert
  FROM XMLTABLE(REPLACE('/*/datosPartxCertList/datosPartxCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      numCert PATH 'numCert');
--
CURSOR c_impLocalList(cTag IN VARCHAR2) IS
  SELECT codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    ubiGeo,
    tipoVia,
    nomVia,
    numVia,
    tipoInt,
    numInt,
    manzana,
    lote,
    kilome,
    tipoCha,
    nomCha,
    tipoSdi,
    nomSdi,
    referencia,
    resto,
    porcSuma,
    ideGiro,
    mtoValDeclarDirec,
    tep,
    tee,
    anoConstrucc,
    numPisos,
    numSotanos,
    fecInspeccion,
    numCert,
    ideDirec,
    indRiesgoIns,
    indInspeccion,
    notaInspeccion,
    indSinInspeccion,
    usuAutorizador,
    claseTipo,
    --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
    --propieEdificacion  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    propieEdificacion,  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    usoInmueble, 
    edificio,
    edificOficina,
    edificotro1,
    edificotro2,
    edificotro3,
    contExistencias,
    contMaquinFija,
    contMaqMovilAcces,
    contEquipoElec,
    contMobiliarioGral,
    contOtro1,
    contOtro2,
    contOtro3,
    contOtro4,
    contOtro5,
    contOtro6,
    benefBruto,
    gastosEstable
    --<F DIGB2B03-2815>
  FROM XMLTABLE(REPLACE('/*/impLocalList/impLocal', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      ubiGeo PATH 'ubiGeo',
      tipoVia PATH 'tipoVia',
      nomVia PATH 'nomVia',
      numVia PATH 'numVia',
      tipoInt PATH 'tipoInt',
      numInt PATH 'numInt',
      manzana PATH 'manzana',
      lote PATH 'lote',
      kilome PATH 'kilome',
      tipoCha PATH 'tipoCha',
      nomCha PATH 'nomCha',
      tipoSdi PATH 'tipoSdi',
      nomSdi PATH 'nomSdi',
      referencia PATH 'referencia',
      resto PATH 'resto',
      porcSuma PATH 'porcSuma',
      ideGiro PATH 'ideGiro',
      mtoValDeclarDirec PATH 'mtoValDeclarDirec',
      tep PATH 'tep',
      tee PATH 'tee',
      anoConstrucc PATH 'anoConstrucc',
      numPisos PATH 'numPisos',
      numSotanos PATH 'numSotanos',
      fecInspeccion DATE PATH 'fecInspeccion',
      numCert PATH 'numCert',
      ideDirec PATH 'ideDirec',
      indRiesgoIns PATH 'indRiesgoIns',
      indInspeccion PATH 'indInspeccion',
      notaInspeccion PATH 'notaInspeccion',
      indSinInspeccion PATH 'indSinInspeccion',
      usuAutorizador PATH 'usuAutorizador',
      claseTipo PATH 'claseTipo',
      --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
      --propieEdificacion PATH 'propieEdificacion');  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      propieEdificacion PATH 'propieEdificacion',  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      usoInmueble PATH 'usoInmueble',
      edificio PATH 'edificio',
      edificOficina PATH 'edificOficina',
      edificotro1 PATH 'edificOtro1',
      edificotro2 PATH 'edificOtro2',
      edificotro3 PATH 'edificOtro3',
      contExistencias PATH 'contExistencias',
      contMaquinFija PATH 'contMaquinFija',
      contMaqMovilAcces PATH 'contMaqMovilAcces',
      contEquipoElec PATH 'contEquipoElec',
      contMobiliarioGral PATH 'contMobiliarioGral',
      contOtro1 PATH 'contOtro1',
      contOtro2 PATH 'contOtro2',
      contOtro3 PATH 'contOtro3',
      contOtro4 PATH 'contOtro4',
      contOtro5 PATH 'contOtro5',
      contOtro6 PATH 'contOtro6',
      benefBruto PATH 'benefBruto',
      gastosEstable PATH 'gastosEstable'
      );  
      --<F DIGB2B03-2815>
--<F RTC-313742>
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
nSumAseg             NUMBER(10,2);
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
nNumOper             Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
--
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
tDatosParticul       TDU_Tabla_Datos_Partic;
tDirecRiesgoCert     TDU_Tabla_Direc_Riesgo_Cert;
tBienCertRiesgo      TDU_Tabla_Bien_Cert_Riesgo;
tEndosatarioBien     TDU_Tabla_Endosatario_Bien;
tCobertBien          TDU_Tabla_Cobert_Cert;
tDeducibleCobert     TDU_Tabla_Deducible_Cobert;
tBienes_Cert         TDU_T_Tabla_BienesxCert;
tImp_Local           acselx.TDU_Tabla_Imp_Local;  --<RTC 313742> / CBayona / 27-09-2022 / Se agrega TDU Locales
--
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Locales, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    DBMS_OUTPUT.Put_Line('IdePol: ' || r.IdePol || ' - CodBien: ' || r.codBien);
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      nSumAseg := r.sumAseg;
      cCodUsr := r.codUsuario;
      --Cargar TDU's
      --<I RTC-313742> / CBayona / 27-09-2022 / Se modifica carga de datos particulares
      FOR t IN c_datosPartxCertList(cTag) LOOP
        tDatosParticul := TDU_Tabla_Datos_Partic();
        nNumCert := t.numCert;
        FOR dp IN c_datosPartList(cTag) LOOP
          --Establecer valores por defecto
          dp.codTrasf := NVL(dp.codTrasf, '000');
          dp.valCodTab := NVL(dp.valCodTab, '000');
          --
          tDatosParticul.Extend;
          tDatosParticul(tDatosParticul.LAST) :=
            TDU_Datos_Partic(dp.codRamoCert, dp.codCrit, dp.codTrasf, dp.valTrasf, dp.numOrd, dp.valCodTab, dp.txtAdicional);
        END LOOP;
        --
        pr_Interfase_AX.Act_Datos_Particulares_ValDec (nIdePol, nNumCert, r.codRamo, r.fecInicio, r.fecFin, tDatosParticul, cError);  --<RTC-316106> / CBayona / 23-01-2023 / Nuevos par·metros
        --
        IF cError != '0,OK' THEN
          EXIT;
        END IF;
      END LOOP;
      --<F RTC-313742>
      --
      tDirecRiesgoCert := TDU_Tabla_Direc_Riesgo_Cert();
      --<I RTC 313091> Christian Bayona - 20/06/2022 Se descomentÛ para nueva lÛgica del proceso
      FOR t IN c_direcRiesgoCertList(cTag) LOOP
         tDirecRiesgoCert.Extend;
        tDirecRiesgoCert(tDirecRiesgoCert.LAST) := TDU_Direc_Riesgo_Cert(t.ideDirec, t.codPais, t.codEstado, t.codCiudad, t.codMunicipio,
          t.tipoVia, t.nomVia, t.numVia, t.tipoInt, t.numInt, t.manzana, t.lote, t.kilome, t.tipoCha, t.nomCha, t.tipoSdi, t.nomSdi,
          t.referencia, t.resto, t.ideGiro, t.mtoValDeclarDirec, t.porcSuma, t.usoInmueble, t.tep, t.anoConstrucc, t.numPisos, t.numSotanos,
          t.tipoGeo, t.xGeo, t.yGeo, t.indInspeccion, t.indRiesgoIns, t.indSinInspeccion, t.notaInspeccion, t.usuAutorizador, t.fecInspeccion,
          t.tipoConstrucc, t.fecExc, t.codMotvExc, t.texTmotvExc, t.txtAdicional, t.tee, t.usoInmuebleSBS, t.propieEdificacion);  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
        DBMS_OUTPUT.Put_Line('DIREC>> ideDirec: ' || t.ideDirec || ', codPais: ' || t.codPais || ', codEstado: ' || t.codEstado || ', codCiudad: ' || t.codCiudad || ', codMunicipio: ' || t.codMunicipio);
        DBMS_OUTPUT.Put_Line('tipoVia: ' || t.tipoVia || ', nomVia: ' || t.nomVia || ', numVia: ' || t.numVia || ', tipoInt: ' || t.tipoInt || ', numInt: ' || t.numInt || ', manzana: ' || t.manzana);
        DBMS_OUTPUT.Put_Line('lote: ' || t.lote || ', kilome: ' || t.kilome || ', tipoCha: ' || t.tipoCha || ', nomCha: ' || t.nomCha || ', tipoSdi: ' || t.tipoSdi || ', nomSdi: ' || t.nomSdi);
        DBMS_OUTPUT.Put_Line('referencia: ' || t.referencia || ', resto: ' || t.resto || ', ideGiro: ' || t.ideGiro || ', mtoValDeclarDirec: ' || t.mtoValDeclarDirec || ', porcSuma: ' || t.porcSuma);
        DBMS_OUTPUT.Put_Line('usoInmueble: ' || t.usoInmueble || ', tep: ' || t.tep || ', anoConstrucc: ' || t.anoConstrucc || ', numPisos: ' || t.numPisos || ', numSotanos: ' || t.numSotanos);
        DBMS_OUTPUT.Put_Line('tipoGeo: ' || t.tipoGeo || ', xGeo: ' || t.xGeo || ', yGeo: ' || t.yGeo || ', indInspeccion: ' || t.indInspeccion || ', indRiesgoIns: ' || t.indRiesgoIns || ', indSinInspeccion: ' || t.indSinInspeccion);
        DBMS_OUTPUT.Put_Line('notaInspeccion: ' || t.notaInspeccion || ', usuAutorizador: ' || t.usuAutorizador || ', fecInspeccion: ' || t.fecInspeccion || ', tipoConstrucc: ' || t.tipoConstrucc || ', fecExc: ' || t.fecExc ||
          ', codMotvExc: ' || t.codMotvExc || ', texTmotvExc: ' || t.texTmotvExc || ', txtAdicional: ' || t.txtAdicional || ', tee: ' || t.tee || ', usoInmuebleSBS: ' || t.usoInmuebleSBS);
      END LOOP;
      --<F RTC 313091> Christian Bayona - 20/06/2022
      --
      tBienCertRiesgo := TDU_Tabla_Bien_Cert_Riesgo();
      FOR t IN c_bienCertRiesgoList(cTag) LOOP
        tBienCertRiesgo.Extend;
        tBienCertRiesgo(tBienCertRiesgo.LAST) := TDU_Bien_Cert_Riesgo(t.ideDirec, t.mtoValDeclarBien, t.txtAdicional);
      END LOOP;
      --
      tEndosatarioBien := TDU_Tabla_Endosatario_Bien();
      FOR t IN c_endosatarioBienList(cTag) LOOP
        tEndosatarioBien.Extend;
        tEndosatarioBien(tEndosatarioBien.LAST) := TDU_Endosatario_Bien(t.numId, t.mtoEndoso, t.fecIng, t.codTexto, t.stsEnd, t.numPrestamo, t.porcPart,
          t.textoBien, t.fecExc, t.codMotvExclu, t.textoMotvExclu, t.txtAdicional);
      END LOOP;
      --
      tCobertBien := TDU_Tabla_Cobert_Cert();
      FOR t IN c_cobertCertList(cTag) LOOP
        tCobertBien.Extend;
        tCobertBien(tCobertBien.LAST) := TDU_Cobert_Cert(t.codRamoCert, t.codCobert, t.stsCobert, t.sumaAsegMoneda, t.tasa, t.primaMoneda, t.codMoneda,
          t.prima, t.sumaAseg, t.codMonedaCob, t.sumaAsegMonedaCob, t.fecIniValid, t.fecFinValid, t.txtAdicional);
      END LOOP;
      --
      tDeducibleCobert := TDU_Tabla_Deducible_Cobert();
      FOR t IN c_deducibleCobertList(cTag) LOOP
        tDeducibleCobert.Extend;
        tDeducibleCobert(tDeducibleCobert.LAST) := TDU_Deducible_Cobert(t.codRamoCert, t.codCobert, t.numDed, t.baseDedcalculo, t.porcDedcalculo,
          t.mtoDedFijo, t.baseDedMinimo, t.porcDedMinimo, t.mtoDedMinimo, t.baseDedMaximo, t.porcDedMaximo, t.mtoDedMaximo, t.codEst, t.valEst,
          t.textoDeduc, t.ideDeduc, t.txtAdicional);
      END LOOP;
      --
      tBienes_Cert := TDU_T_Tabla_BienesxCert();
      FOR t IN c_bienxCertList(cTag) LOOP
        --Establecer valores por defecto
        t.idePol := nIdePol;
        t.numCert := nNumCert;
        t.claseBien := NVL(t.claseBien, r.claseBien);
        t.codBien := NVL(t.codBien, r.codBien);
        t.codMonedaBien := NVL(t.codMonedaBien, r.codMonedaBien);
        t.mtoValDeclarBien := NVL(t.mtoValDeclarBien, r.sumAseg);
        --
        tBienes_Cert.Extend;
        tBienes_Cert(tBienes_Cert.LAST) := TDU_Tabla_BienesxCert(t.idePol, t.numCert, t.codRamoCert, t.fecIniValid, t.fecFinValid, t.claseBien, t.codBien,
          t.codMonedaBien, t.mtoValDeclarBien, t.porcRiesgo, t.indImprDeclar, tBienCertRiesgo, tEndosatarioBien, tCobertBien, tDeducibleCobert, t.txtAdicional);
        DBMS_OUTPUT.Put_Line('BIENES>> idePol: ' || t.idePol || ', numCert: ' || t.numCert || ', codRamoCert: ' || t.codRamoCert || ', fecIniValid: ' || t.fecIniValid ||
         ', fecFinValid: ' || t.fecFinValid || ', claseBien: ' || t.claseBien || ', codBien: ' || t.codBien || ', codMonedaBien: ' || t.codMonedaBien || ', mtoValDeclarBien: ' ||
         t.mtoValDeclarBien || ', porcRiesgo: ' || t.porcRiesgo || ', indImprDeclar: ' || t.indImprDeclar || ', txtAdicional: ' || t.txtAdicional);
      END LOOP;
      --
      --<I RTC-313742> / CBayona / 27-09-2022 / Se agrega importaciÛn de locales e invoca al procedimiento de locales actualizado
      tImp_Local := acselx.TDU_Tabla_Imp_Local();
        FOR t IN c_impLocalList(cTag) LOOP
          tImp_Local.Extend;
          tImp_Local(tImp_Local.LAST) :=
            acselx.TDU_Imp_Local(t.codPais, t.codEstado, t.codCiudad, t.codMunicipio, t.ubiGeo, t.tipoVia, t.nomVia, t.numVia, t.tipoInt, t.numInt,
              t.manzana, t.lote, t.kilome, t.tipoCha, t.nomCha, t.tipoSdi, t.nomSdi, t.referencia, t.resto, t.porcSuma, t.ideGiro,
              t.mtoValDeclarDirec, t.tep, t.tee, t.anoConstrucc, t.numPisos, t.numSotanos, t.fecInspeccion, t.numCert, t.ideDirec,
              --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
              --t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion);  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
              t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion,  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
              t.usoInmueble, t.edificio, t.edificOficina, t.edificotro1, t.edificotro2, t.edificotro3, t.contExistencias, t.contMaquinFija, 
              t.contMaqMovilAcces, t.contEquipoElec, t.contMobiliarioGral, t.contOtro1, t.contOtro2, t.contOtro3,t.contOtro4, t.contOtro5,
              t.contOtro6, t.benefBruto, t.gastosEstable);  
              --<F DIGB2B03-2815> 
        END LOOP;
      --
      pr_Interfase_AX.Modificacion_Locales(nIdePol, r.codProd, r.codMonedaBien, tImp_Local, tBienes_Cert, tBienCertRiesgo, cCodUsr, cMsgValida, cError);
      --<F RTC-313742>
      --
      --<I RTC-313742> / CBayona / 26-08-2022 / Se agrega procedimientos para soporte multiendoso
        --CesiÛn InclusiÛn
        IF cError = '0,OK' THEN
          SetCesionINC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --CesiÛn ExclusiÛn
        IF cError = '0,OK' THEN
          SetCesionEXC(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, l_xml_Error0, cError);
        END IF;
        --
        --Cambiar Responsable Pago
        IF cError = '0,OK' THEN
          SetRespPago(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError); --<RTC 313742> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
        END IF;
        --<F RTC-313742>

        --<I RTC-313742> / CBayona / 14-09-2022 / Se agrega procedimiento para soporte multiendoso
        --Modificar Cla˙sulas
        IF cError = '0,OK' THEN
          ModClausulas(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, cError);
        END IF;
        --<F RTC-313742>
      --
      IF cError = '0,OK' THEN
        --Insertar Anexo / Plan Nuevo Formato
        pr_Interfase_API_Endoso2.Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
      END IF;
    END IF;
  END LOOP;
  --
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',
                -- Variables de retorno
                p_nNumOper => nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Locales, nIdePol, nNumCert, nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Mod_Locales;


/*-----------------------------------------------------------------------------
  Nombre      : Aseg_Adic_EntNOFin
  Proposito   : Endoso de asegurado adicional a entidad No financiera
  Referencias : <RTC-313351> Endoso de asegurado adicional a entidad NO financiera
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    25/05/2022    Christian Bayona           Creacion
    27/05/2022    Christian Bayona           ModificaciÛn, uso nuevo procedimiento
    06/06/2022    Christian Bayona           ModificaciÛn, modificaciÛn de par·metro
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Aseg_Adic_EntNOFin ( p_cXMLInput  IN CLOB,
                               p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      prima PATH 'prima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Aseg_Adic_EntNOFin, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Insertar Anexo / Plan Nuevo Formato
      Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Aseg_Adic_EntNOFin, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Aseg_Adic_EntNOFin;


/*-----------------------------------------------------------------------------
  Nombre      : Mod_Corredor
  Proposito   : Endoso de ModificaciÛn de Corredor (Intermediario)
  Referencias : <RTC-313350> Endoso de ModificaciÛn de Corredor
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    10/06/2022    Christian Bayona           Creacion
    16/06/2022    Christian Bayona           ModificaciÛn, se agregÛ par·metros
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    29/09/2022    Christian Bayona           <RTC 313350> ModificaciÛn, se adiciona procedimiento para mostrar error
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Corredor ( p_cXMLInput  IN CLOB,
                         p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    numId,
    codAgencia,
    codOfiSusc,
    indLider,
    porcPart,
    codCanal,
    tipoCanal,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      numId PATH 'numId',
      codAgencia PATH 'codAgencia',
      codOfiSusc PATH 'codOfiSusc',
      indLider PATH 'indLider',
      porcPart PATH 'porcPart',
      codCanal PATH 'codCanal',
      tipoCanal PATH 'tipoCanal',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Corredor, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      r.indLider := NVL(r.indLider, 'S');
      r.porcPart := NVL(r.porcPart, 100);
      cCodUsr := r.codUsuario;
      --Procedimiento para Modificar al Corredor
      pr_Interfase_AX.ModificacionCorredor(nIdePol, r.numId, r.codAgencia, r.codOfiSusc, r.indLider, r.porcPart, r.codCanal, r.tipoCanal, cError);

      --<I RTC 313350> / CBayona / 29-09-2022 / Se encola errores remitidos al ModificacionCorredor
      IF cError != '0,OK' THEN
        PushError('MODCORREDOR', '1', cError, NULL, NULL, l_xml_Error0);
      END IF;
      --<F RTC 313350>
      --
      IF cError = '0,OK' THEN
        --Insertar Anexo / Plan Nuevo Formato
        Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Corredor, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Mod_Corredor;


/*-----------------------------------------------------------------------------
  Nombre      : Cambio_Comision
  Proposito   : Endoso de Cambio de ComisiÛn
  Referencias : <RTC-314182> Endosos de Cambio de ComisiÛn
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    27/06/2022    Christian Bayona           CreaciÛn
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Cambio_Comision ( p_cXMLInput  IN CLOB,
                            p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    porcCom,
    numTramite,
    autorizador,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      porcCom PATH 'porcCom',
      numTramite PATH 'numTramite',
      autorizador PATH 'autorizador',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Cambio_Comision, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Modificamos la comisiÛn
      pr_Interfase_AX.Modificar_Comision(nIdePol, r.porcCom, r.numTramite, r.autorizador, cCodUsr, cMsgValida, cError);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
      --
      /*-- <I RTC 313091> Renato Cortez - 07/07/2022 - Se comentÛ esta instrucciÛn ya que no es necesario para el endoso.
      IF cError ='0,OK' THEN
        --Insertar Anexo / Plan Nuevo Formato
        Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
      END IF;
      -- <F RTC 313091>
      */
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Cambio_Comision, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Cambio_Comision;

/*-----------------------------------------------------------------------------
  Nombre      : Traslado_Bienes
  Proposito   : Endoso de Traslado de Bienes
  Referencias : <RTC 314179> Endosos Traslado de Bienes
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    13/07/2022    Renato Cortez              CreaciÛn
    18/07/2022    Renato Cortez              <RTC 313091> Se agregÛ validaciones para producto y n˙mero de poliza.
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    15/11/2022    Christian Bayona           <RTC 315519> ModificaciÛn, se adiciona nuevo atributo - Mejoras en Acsel/x para soportar cambios nueva versiÛn Carga SBS
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    07/07/2023    Christian Bayona           <RTC 317178> Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
    21/09/2023    Manuel Navarro             <DIGB2B03-2815> InclusiÛn Locales Legacy Dubai Masivo
  ----------------------------------------------------------------------------*/
PROCEDURE Traslado_Bienes ( p_cXMLInput  IN CLOB,
                            p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    codRamo,
    codMoneda,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codRamo PATH 'codRamo',
      codMoneda PATH 'codMoneda',
      codUsuario PATH 'codUsuario');
--
CURSOR c_impLocalList(cTag IN VARCHAR2) IS
  SELECT codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    ubiGeo,
    tipoVia,
    nomVia,
    numVia,
    tipoInt,
    numInt,
    manzana,
    lote,
    kilome,
    tipoCha,
    nomCha,
    tipoSdi,
    nomSdi,
    referencia,
    resto,
    porcSuma,
    ideGiro,
    mtoValDeclarDirec,
    tep,
    tee,
    anoConstrucc,
    numPisos,
    numSotanos,
    fecInspeccion,
    numCert,
    ideDirec,
    indRiesgoIns,
    indInspeccion,
    notaInspeccion,
    indSinInspeccion,
    usuAutorizador,
    claseTipo,
    --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
    --propieEdificacion  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    propieEdificacion,  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    usoInmueble, 
    edificio,
    edificOficina,
    edificotro1,
    edificotro2,
    edificotro3,
    contExistencias,
    contMaquinFija,
    contMaqMovilAcces,
    contEquipoElec,
    contMobiliarioGral,
    contOtro1,
    contOtro2,
    contOtro3,
    contOtro4,
    contOtro5,
    contOtro6,
    benefBruto,
    gastosEstable
    --<F DIGB2B03-2815>
  FROM XMLTABLE(REPLACE('/*/impLocalList/impLocal', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      ubiGeo PATH 'ubiGeo',
      tipoVia PATH 'tipoVia',
      nomVia PATH 'nomVia',
      numVia PATH 'numVia',
      tipoInt PATH 'tipoInt',
      numInt PATH 'numInt',
      manzana PATH 'manzana',
      lote PATH 'lote',
      kilome PATH 'kilome',
      tipoCha PATH 'tipoCha',
      nomCha PATH 'nomCha',
      tipoSdi PATH 'tipoSdi',
      nomSdi PATH 'nomSdi',
      referencia PATH 'referencia',
      resto PATH 'resto',
      porcSuma PATH 'porcSuma',
      ideGiro PATH 'ideGiro',
      mtoValDeclarDirec PATH 'mtoValDeclarDirec',
      tep PATH 'tep',
      tee PATH 'tee',
      anoConstrucc PATH 'anoConstrucc',
      numPisos PATH 'numPisos',
      numSotanos PATH 'numSotanos',
      fecInspeccion DATE PATH 'fecInspeccion',
      numCert PATH 'numCert',
      ideDirec PATH 'ideDirec',
      indRiesgoIns PATH 'indRiesgoIns',
      indInspeccion PATH 'indInspeccion',
      notaInspeccion PATH 'notaInspeccion',
      indSinInspeccion PATH 'indSinInspeccion',
      usuAutorizador PATH 'usuAutorizador',
      claseTipo PATH 'claseTipo',
      --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
      --propieEdificacion PATH 'propieEdificacion');  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      propieEdificacion PATH 'propieEdificacion',  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
      usoInmueble PATH 'usoInmueble',
      edificio PATH 'edificio',
      edificOficina PATH 'edificOficina',
      edificotro1 PATH 'edificOtro1',
      edificotro2 PATH 'edificOtro2',
      edificotro3 PATH 'edificOtro3',
      contExistencias PATH 'contExistencias',
      contMaquinFija PATH 'contMaquinFija',
      contMaqMovilAcces PATH 'contMaqMovilAcces',
      contEquipoElec PATH 'contEquipoElec',
      contMobiliarioGral PATH 'contMobiliarioGral',
      contOtro1 PATH 'contOtro1',
      contOtro2 PATH 'contOtro2',
      contOtro3 PATH 'contOtro3',
      contOtro4 PATH 'contOtro4',
      contOtro5 PATH 'contOtro5',
      contOtro6 PATH 'contOtro6',
      benefBruto PATH 'benefBruto',
      gastosEstable PATH 'gastosEstable');
      --<F DIGB2B03-2815> 
--<I RTC 317178> / Christian Bayona / 07-07-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
CURSOR c_bienxCertList(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamoCert,
    fecIniValid,
    fecFinValid,
    claseBien,
    codBien,
    codMonedaBien,
    mtoValDeclarBien,
    porcRiesgo,
    indImprDeclar,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/bienxCertList/bienxCert', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamoCert PATH 'codRamoCert',
      fecIniValid DATE PATH 'fecIniValid',
      fecFinValid DATE PATH 'fecFinValid',
      claseBien PATH 'claseBien',
      codBien PATH 'codBien',
      codMonedaBien PATH 'codMonedaBien',
      mtoValDeclarBien PATH 'mtoValDeclarBien',
      porcRiesgo PATH 'porcRiesgo',
      indImprDeclar PATH 'indImprDeclar',
      txtAdicional PATH 'txtAdicional');
--
CURSOR c_bienCertRiesgoList(cTag IN VARCHAR2) IS
  SELECT ideDirec,
    mtoValDeclarBien,
    txtAdicional
  FROM XMLTABLE(REPLACE('/*/bienCertRiesgoList/bienCertRiesgo', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      ideDirec PATH 'ideDirec',
      mtoValDeclarBien PATH 'mtoValDeclarBien',
      txtAdicional PATH 'txtAdicional');
--<F RTC 317178>
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
nNumCert             Certificado.NumCert%TYPE;
tImp_Local           acselx.TDU_Tabla_Imp_Local;
--<I RTC 317178> / Christian Bayona / 13-06-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
tBienes_Cert         acselx.TDU_T_Tabla_BienesxCert;
tBienCertRiesgo      acselx.TDU_Tabla_Bien_Cert_Riesgo;
--<F RTC 317178>
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Traslado_Bienes, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 313091> 18/07/2022 Renato Cortez  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      IF cError = '0,OK' THEN
        tImp_Local := TDU_Tabla_Imp_Local();
        FOR t IN c_impLocalList(cTag) LOOP
          tImp_Local.Extend;
          tImp_Local(tImp_Local.LAST) :=
            TDU_Imp_Local(t.codPais, t.codEstado, t.codCiudad, t.codMunicipio, t.ubiGeo, t.tipoVia, t.nomVia, t.numVia, t.tipoInt, t.numInt,
              t.manzana, t.lote, t.kilome, t.tipoCha, t.nomCha, t.tipoSdi, t.nomSdi, t.referencia, t.resto, t.porcSuma, t.ideGiro,
              t.mtoValDeclarDirec, t.tep, t.tee, t.anoConstrucc, t.numPisos, t.numSotanos, t.fecInspeccion, t.numCert, t.ideDirec,
              --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / InclusiÛn Locales Legacy Dubai Masivo
              --t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion);  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
              t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion,  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
              t.usoInmueble, t.edificio, t.edificOficina, t.edificotro1, t.edificotro2, t.edificotro3, t.contExistencias, t.contMaquinFija, 
              t.contMaqMovilAcces, t.contEquipoElec, t.contMobiliarioGral, t.contOtro1, t.contOtro2, t.contOtro3,t.contOtro4, t.contOtro5,
              t.contOtro6, t.benefBruto, t.gastosEstable);  
              --<F DIGB2B03-2815> 
        END LOOP;
        --<I RTC 317178> / Christian Bayona / 13-06-2023 / Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
        tBienes_Cert := TDU_T_Tabla_BienesxCert();
        FOR t IN c_bienxCertList(cTag) LOOP
          --Establecer valores por defecto
          t.idePol := nIdePol;
          t.numCert := nNumCert;
          --
          tBienes_Cert.Extend;
          tBienes_Cert(tBienes_Cert.LAST) := TDU_Tabla_BienesxCert(t.idePol, t.numCert, t.codRamoCert, t.fecIniValid, t.fecFinValid, t.claseBien, t.codBien,
            t.codMonedaBien, t.mtoValDeclarBien, t.porcRiesgo, t.indImprDeclar, tBienCertRiesgo, NULL, NULL, NULL, t.txtAdicional);
          DBMS_OUTPUT.Put_Line('BIENES>> idePol: ' || t.idePol || ', numCert: ' || t.numCert || ', codRamoCert: ' || t.codRamoCert || ', fecIniValid: ' || t.fecIniValid ||
           ', fecFinValid: ' || t.fecFinValid || ', claseBien: ' || t.claseBien || ', codBien: ' || t.codBien || ', codMonedaBien: ' || t.codMonedaBien || ', mtoValDeclarBien: ' ||
           t.mtoValDeclarBien || ', porcRiesgo: ' || t.porcRiesgo || ', indImprDeclar: ' || t.indImprDeclar || ', txtAdicional: ' || t.txtAdicional);
        END LOOP;
        --
        tBienCertRiesgo := TDU_Tabla_Bien_Cert_Riesgo();
        FOR t IN c_bienCertRiesgoList(cTag) LOOP
          tBienCertRiesgo.Extend;
          tBienCertRiesgo(tBienCertRiesgo.LAST) := TDU_Bien_Cert_Riesgo(t.ideDirec, t.mtoValDeclarBien, t.txtAdicional);
        END LOOP;
        --
        -- pr_Interfase_AX.Endoso_Traslado_Bienes(nIdePol, cCodProd, r.codMoneda, tImp_Local, cError);
        pr_Interfase_AX.Endoso_Traslado_Bienes(nIdePol, cCodProd, r.codMoneda, tImp_Local, tBienes_Cert, tBienCertRiesgo, cError);
        --<F RTC 317178>
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Traslado_Bienes, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Traslado_Bienes;

/*-----------------------------------------------------------------------------
  Nombre      : Dec_Exist_TipoA_M
  Proposito   : Endoso de DeclaraciÛn de existencias tipo A Mensual
  Referencias : <RTC 314184> Endoso de DeclaraciÛn de existencias tipo AB
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    14/07/2022    Renato Cortez              CreaciÛn
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Dec_Exist_TipoA_M ( p_cXMLInput  IN CLOB,
                              p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Dec_Exist_TipoA_M, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, nNumCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, nNumCert, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      IF pr_Interfase_AX.ExisteBien(nIdePol, nNumCert) THEN
        --Insertar Anexo / Plan Nuevo Formato
        Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);

      ELSE
        cError := '1,No existe bien EXISTENCIAS en la PÛliza';
        PushError('VALEXIST', '1', cError, NULL, NULL, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se cambia a NULL para que tome valores por defecto
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Dec_Exist_TipoA_M, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Dec_Exist_TipoA_M;

/*-----------------------------------------------------------------------------
  Nombre      : Dec_Exist_TipoA_A
  Proposito   : Endoso de DeclaraciÛn de existencias tipo A Anual
  Referencias : <RTC 314184> Endoso de DeclaraciÛn de existencias tipo AB
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    04/08/2022    Renato Cortez              CreaciÛn
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Dec_Exist_TipoA_A ( p_cXMLInput  IN CLOB,
                              p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    codRamo,
    fecInicio,
    fecFin,
    mtoPrima,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codRamo PATH 'codRamo',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      mtoPrima VARCHAR2(15) PATH 'mtoPrima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cMtoPrima            VARCHAR2(16);
cError               VARCHAR2(2000);
nNumOper             Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Dec_Exist_TipoA_A, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := r.numCert;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, nNumCert, r.numPol, r.codProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, nNumCert, r.numPol, r.codProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      cMtoPrima := NVL(r.mtoPrima, '0');
      IF cMtoPrima = '0' THEN
        cMtoPrima := '';
      ELSE
        cMtoPrima := 'S' || cMtoPrima;
      END IF;
      --Ejecutar procedimiento de Datos Particulares
      Datos_Particulares(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, r.codRamo, l_xml_Error0, cError);
      --
      IF cError = '0,OK' THEN
        Add_CobertCert(cTag, p_cXMLInput, nIdePol, nNumCert, l_xml_Error0, cError);
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS' || cMtoPrima,
                -- Variables de retorno
                p_nNumOper => nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Dec_Exist_TipoA_A, nIdePol, nNumCert, nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Dec_Exist_TipoA_A;


/*-----------------------------------------------------------------------------
  Nombre      : Inc_Equipos_TREC
  Proposito   : Endoso de Inclusion TREC
  Referencias : <RTC-314183> Endoso de Inclusion TREC
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    27/06/2022    Renato Cortez              CreaciÛn
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    11/10/2022    Christian Bayona           <RTC 313350> ModificaciÛn, se adiciona validaciÛn y par·metros de Datos particulares
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    11/08/2023    Manuel Navarro             <DIGB2B03-2877> Mejora en Registro y ValidaciÛn de Maquinaria TREC
  ----------------------------------------------------------------------------*/
PROCEDURE Inc_Equipos_TREC ( p_cXMLInput  IN CLOB,
                            p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamo,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    prima,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamo PATH 'codRamo',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      prima PATH 'prima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
l_prima_x_cobrar   VARCHAR2(15);

--<I DIGB2B03-2877 > Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
nCantReg   NUMBER := 0;
cValTrec   VARCHAR2(1):= '0';

nNumCert   Certificado.NumCert%TYPE:= null;
--<F DIGB2B03-2877>
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Inc_Equipos_TREC, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    --
    --<I RTC-314183> / CBayona / 11-10-2022 / Se adiciona validaciÛn
    IF cError = '0,OK' THEN
      l_prima_x_cobrar := r.prima;
      pr_Interfase_AX.Val_Mto_Operacion(l_prima_x_cobrar, 'I', cMsgValida, cError);
    END IF;
    --<F RTC-314183>
    --
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      l_prima_x_cobrar := r.prima;
      --Ejecutar procedimiento de Datos Particulares
      Datos_Particulares(cTag, p_cXMLInput, nIdePol, r.numCert, r.fecInicio, r.fecFin, r.codRamo, cCodUsr, cMsgValida, cError); --<RTC-314183> / CBayona / 11-10-2022 / Se adiciona par·metros
    --
    IF cError = '0,OK' THEN
        Add_CobertCert(cTag, p_cXMLInput, nIdePol, r.numCert, l_xml_Error0, cError);
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
        --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 / Mejora en Registro y ValidaciÛn de Maquinaria TREC
        IF cError = '0,OK' THEN
            IF PR.BUSCA_LVAL('MAQTRECW', cCodProd)='1' THEN
               nNumCert := r.numCert;
               maquinarias_trec(cTag, p_cXMLInput, nIdePol, r.numCert, cCodUsr, 'I', cError);
               IF cError != '0,OK' THEN
                  PushError('Inc_Equipos_TREC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
                  EXIT;
               END IF;
            END IF;
        END IF;
        --<F DIGB2B03-2877>
      End if;
    END IF;
  END LOOP;

  --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 / Mejora en Registro y ValidaciÛn de Maquinaria TREC
  IF cError = '0,OK' THEN
     cValTrec := acselx.pr_Interfase_AX.Valida_Suma_Trec(nIdePol, cCodProd);

     IF NVL(cValTrec,'0') = '1' THEN
        cError := '1,El valor declarado en Datos Particulares no coincide con la sumatoria de valores declarados Registro Maquinarias TREC';
        PushError('Inc_Equipos_TREC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
     END IF;
  END IF;
  --<F DIGB2B03-2877>
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSSS' || l_prima_x_cobrar,
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);

    --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 / Mejora en Registro y ValidaciÛn de Maquinaria TREC
      IF cError = '0,OK' THEN
         IF PR.BUSCA_LVAL('MAQTRECW', cCodProd)='1' THEN
            Acselx.pr_Interfase_AX.NumOper_Update_Trec(cTag, p_cXMLInput, nIdePol, nNumCert, p_nNumOper, 'I', cError);
         END IF;
      END IF;
      --<F DIGB2B03-2877 >
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Inc_Equipos_TREC, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Inc_Equipos_TREC;


/*-----------------------------------------------------------------------------
  Nombre      : Cambio_Resp_Pago
  Proposito   : Endoso Cambio Responsable de Pago
  Referencias : <RTC 313349> Endoso Cambio Responsable de Pago
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    21/07/2022    Renato Cortez              CreaciÛn
    26/08/2022    Christian Bayona           <RTC 314183> ModificaciÛn, se consolido cÛdigo en nuevo procedimiento
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Cambio_Resp_Pago ( p_cXMLInput  IN CLOB,
                             p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
nNumCert             Certificado.NumCert%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Cambio_Resp_Pago, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := NVL(r.numCert, 1);
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(nIdePol, nNumCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(nIdePol, nNumCert, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      IF cError = '0,OK' THEN
        SetRespPago(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, l_xml_Error0, cError); --<RTC 313349> / CBayona / 26-08-2022 / Se reemplazo cÛdigo por procedimiento consolidado --<RTC 313349> CBayona / 19-09-2022 / InclusiÛn de cursor de salida
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Cambio_Resp_Pago, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Cambio_Resp_Pago;


/*-----------------------------------------------------------------------------
  Nombre      : Mod_Deducible
  Proposito   : Endoso de Modificacion Deducible
  Referencias : <RTC-314181> Endoso de Modificacion Deducible
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    02/08/2022    Renato Cortez              CreaciÛn
    26/08/2022    Christian Bayona           <RTC 314183> ModificaciÛn, se consolido cÛdigo en nuevo procedimiento
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Deducible ( p_cXMLInput  IN CLOB,
                          p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
nNumCert             Certificado.NumCert%TYPE; --<RTC-314181> / CBayona / 26-08-2022 / Se adiciona variable
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Deducible, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := r.numCert; --<RTC-314181> / CBayona / 26-08-2022 / Se inicializa variable
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --
      IF cError = '0,OK' THEN
        SetDeducible(cTag, p_cXMLInput, nIdePol, nNumCert, r.fecInicio, r.fecFin, cCodUsr, cMsgValida, cError); --<RTC-314181> / CBayona / 26-08-2022 / Se reemplazo cÛdigo por procedimiento consolidado
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Deducible, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput); --<RTC-314181> / CBayona / 26-08-2022 / Se agrega variable nNumCert
END Mod_Deducible;


/*-----------------------------------------------------------------------------
  Nombre      : Exc_Equipos_TREC
  Proposito   : Endoso de ExclusiÛn TREC
  Referencias : <RTC-314183> Endoso de Exclusion TREC
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    25/08/2022    Christian Bayona           CreaciÛn
    01/09/2022    Christian Bayona           Agregar variable
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    11/10/2022    Christian Bayona           <RTC 313350> ModificaciÛn, se adiciona validaciÛn y par·metros de Datos particulares
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    11/08/2023    Manuel Navarro             <DIGB2B03-2877> Mejora en Registro y ValidaciÛn de Maquinaria TREC
  ----------------------------------------------------------------------------*/
PROCEDURE Exc_Equipos_TREC ( p_cXMLInput  IN CLOB,
                             p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    codRamo,
    numPol,
    codProd,
    fecInicio,
    fecFin,
    prima,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      codRamo PATH 'codRamo',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      prima PATH 'prima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE; --<RTC-314183> / CBayona / 01-09-2022 / Agregar variable
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
l_prima_x_cobrar   VARCHAR2(15);

--<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
nCantReg   NUMBER := 0;
cValTrec   VARCHAR2(1):= '0';
--<F DIGB2B03-2877>
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Exc_Equipos_TREC, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    nNumCert := r.numCert; --<RTC-314183> / CBayona / 01-09-2022 / Inicializar variable
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, r.numCert, r.NumPol, r.CodProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    --
    --<I RTC-314183> / CBayona / 11-10-2022 / Se adiciona validaciÛn
    IF cError = '0,OK' THEN
      l_prima_x_cobrar := r.prima;
      pr_Interfase_AX.Val_Mto_Operacion(l_prima_x_cobrar, 'E', cMsgValida, cError);
    END IF;
    --<F RTC-314183>
    --
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      l_prima_x_cobrar := r.prima;
      --Ejecutar procedimiento de Datos Particulares
      Datos_Particulares(cTag, p_cXMLInput, nIdePol, r.numCert, r.fecInicio, r.fecFin, r.codRamo, cCodUsr, cMsgValida, cError); --<RTC-314183> / CBayona / 11-10-2022 / Se adiciona par·metros
    --
      IF cError = '0,OK' THEN
        Add_CobertCert(cTag, p_cXMLInput, nIdePol, r.numCert, l_xml_Error0, cError);
        --
        IF cError = '0,OK' THEN
          --Insertar Anexo / Plan Nuevo Formato
          Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
        END IF;

        --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
        IF cError = '0,OK' THEN
          IF PR.BUSCA_LVAL('MAQTRECW', cCodProd)='1' THEN
             nNumCert := r.numCert;
             maquinarias_trec(cTag, p_cXMLInput, nIdePol, r.numCert, cCodUsr, 'E', cError);
             IF cError != '0,OK' THEN
                PushError('Exc_Equipos_TREC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
                EXIT;
             END IF;
          END IF;
        END IF;
        --<F DIGB2B03-2877>

      END IF;
    END IF;
  END LOOP;

  --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
  IF cError = '0,OK' THEN
     cValTrec := acselx.pr_Interfase_AX.Valida_Suma_Trec(nIdePol, cCodProd);
     IF NVL(cValTrec,'0') = '1' THEN
        cError := 'El valor declarado en Datos Particulares no coincide con la sumatoria valores declarados Registro Maquinarias TREC';
        PushError('Exc_Equipos_TREC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
     END IF;
  END IF;
  --<F DIGB2B03-2877>
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSSS' || l_prima_x_cobrar,
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
    --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
      IF cError = '0,OK' THEN
         IF PR.BUSCA_LVAL('MAQTRECW', cCodProd)='1' THEN
            acselx.pr_Interfase_AX.NumOper_Update_Trec(cTag, p_cXMLInput, nIdePol, nNumCert, p_nNumOper, 'E', cError);
         END IF;
      END IF;
      --<F DIGB2B03-2877>

  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Exc_Equipos_TREC, nIdePol, nNumCert, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput); --<RTC-314183> / CBayona / 01-09-2022 / Agregar variable
END Exc_Equipos_TREC;


/*-----------------------------------------------------------------------------
  Nombre      : Mod_Equipos_TREC
  Proposito   : ModificaciÛn de Equipos TREC
  Referencias : <RTC-314183> Endosos Modificaciones TREC
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    25/08/2022    Christian Bayona           CreaciÛn
    26/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se adiciona par·metro Usuario
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    11/08/2023    Manuel Navarro             <DIGB2B03-2877> Mejora en Registro y ValidaciÛn de Maquinaria TREC
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Equipos_TREC ( p_cXMLInput  IN CLOB,
                             p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      prima PATH 'prima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);

--<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
nCantReg   NUMBER := 0;
cValTrec   VARCHAR2(1):= '0';
nNumCert   Certificado.NumCert%TYPE:= null;
--<F DIGB2B03-2877>
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Mod_Equipos_TREC, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --<I RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 314175> / CBayona / 26-09-2022 / Se agrega par·metro codUsuario
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);
    --<F RTC 316878>
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --Insertar Anexo / Plan Nuevo Formato
      Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
    END IF;

    --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
    IF cError = '0,OK' THEN
        IF PR.BUSCA_LVAL('MAQTRECW', cCodProd)='1' THEN
           maquinarias_trec(cTag, p_cXMLInput, nIdePol, 0, cCodUsr, 'M', cError);
           IF cError != '0,OK' THEN
              PushError('Mod_Equipos_TREC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
              EXIT;
           END IF;
        END IF;
    END IF;
    --<F DIGB2B03-2877>
  END LOOP;

  --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
  IF cError = '0,OK' THEN
     cValTrec := acselx.pr_Interfase_AX.Valida_Suma_Trec(nIdePol, cCodProd);--agregar ax
     IF NVL(cValTrec,'0') = '1' THEN
        cError := 'El valor declarado en Datos Particulares no coincide con la sumatoria valores declarados Registro Maquinarias TREC';
        PushError('Exc_Equipos_TREC', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
     END IF;
  END IF;
  --<F DIGB2B03-2877>
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);

     --<I DIGB2B03-2877> Manuel Navarro / 11-08-2023 /  Mejora en Registro y ValidaciÛn de Maquinaria TREC
      IF cError = '0,OK' THEN
         IF PR.BUSCA_LVAL('MAQTRECW', cCodProd)='1' THEN
            acselx.pr_Interfase_AX.NumOper_Update_Trec(cTag, p_cXMLInput, nIdePol, 0, p_nNumOper, 'M', cError);
         END IF;
      END IF;
      --<F DIGB2B03-2877>

  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Mod_Equipos_TREC, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Mod_Equipos_TREC;


/*-----------------------------------------------------------------------------
  Nombre      : Anula_Endoso
  Proposito   : AnulaciÛn de endosos vÌa anexo de texto
  Referencias : <RTC-314185> Endosos AnulaciÛn de endosos - Orquestador
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    25/08/2022    Christian Bayona           CreaciÛn
    08/09/2022    Christian Bayona           ModificaciÛn, se agregÛ procedimiento de anulaciÛn
    22/09/2022    Christian Bayona           <RTC 314175> ModificaciÛn, se agregÛ validaciÛn para omitir endosos a anular
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Anula_Endoso ( p_cXMLInput  IN CLOB,
                         p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    tipoEndoso, --<RTC 314185> / CBayona / 22-09-2022 / Se adiciona par·metro Tipo de Endoso
    numOper,
    txtAnexoOp,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      tipoEndoso PATH 'tipoEndoso',  --<RTC 314185> / CBayona / 22-09-2022 / Se adiciona par·metro Tipo de Endoso
      numOper PATH 'numOper',
      txtAnexoOp PATH 'txtAnexoOp',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
cCodMsg              LVal.Descrip%TYPE; --<RTC 314185> / CBayona / 22-09-2022 / Se adiciona par·metro Tipo de Endoso
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Anula_Endoso, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);  --<RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      --<I RTC 314185> / CBayona / 22-09-2022 / ValidaciÛn de endosos que no se anularan
      SELECT MAX(Descrip)
      INTO cCodMsg
      FROM LVal
      WHERE TipoLVal = 'ANUENDOS'
        AND CodLVal = r.tipoEndoso;
      --
      IF cCodMsg IS NOT NULL THEN
        cError := pr_Interfase_AX_SAS_Util.Mensaje_Error(cCodMsg);
        PushError('ENDOANUL', cCodMsg, cError, NULL, NULL, l_xml_Error0);
        cError := cCodMsg || cError;

      ELSE
        --<I RTC-314183> / CBayona / 08-09-2022 / Se adiciona procedimiento de endoso y de captura errores
        --Insertar Anexo de anulaciÛn
        pr_interfase_ax.Anular_Endoso(nIdePol, r.numOper, r.txtAnexoOp, cCodUsr, cMsgValida, cError);
        --
        IF cError != '0,OK' THEN
          PushError('ENDOANUL', SUBSTR(cError, 1, INSTR(cError, ',') - 1), SUBSTR(cError, INSTR(cError, ',') + 1), NULL, NULL, l_xml_Error0);
        END IF;
        --<F RTC-314183>
      END IF;
      --<F RTC 314185>
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Anula_Endoso, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
END Anula_Endoso;


/*------------------------------------------------------------------------
  Nombre     : Actualiza_Cert_Tipo_B
  PropÛsito  : Procedimiento que actualiza la informaciÛn de direcciones para el Tipo B
  Referencias: <RTC-314184> Endosos de DeclaraciÛn de existencias tipo "A y B"
  Par·metros :
                 p_cTag            VARCHAR2     Etiqueta
                 p_cXML            CLOB         N˙mero de Certificado
                 p_nIdePol         NUMBER       Identificador de PÛliza
                 p_cCodProd        VARCHAR2     CÛdigo de Producto
                 p_cCodMoneda      VARCHAR2     CÛdigo de Moneda
                 p_nNumCert        NUMBER       N˙mero de Certificado
                 p_cCodRamo        VARCHAR2     CÛdigo de Ramo
                 p_cDescCert       VARCHAR2     DescripciÛn del Certificado
                 p_dFecIni         DATE         Fecha de Inicio de Vigencia Certificado
                 p_dFecFin         DATE         Fecha de Fin de Vigencia Certificado
  Retorno    :   p_cXMLError       XMLTYPE      Errores reportados previamente
                 p_cError          VARCHAR2     Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               DescripciÛn
    14/09/2022    Christian Bayona    CreaciÛn
    15/11/2022    Christian Bayona           <RTC 315519> ModificaciÛn, se adiciona nuevo atributo - Mejoras en Acsel/x para soportar cambios nueva versiÛn Carga SBS
 -----------------------------------------------------------------------------*/
PROCEDURE Actualiza_Cert_Tipo_B(p_cTag IN VARCHAR2,
                                p_cXMLInput IN CLOB,
                                p_nIdePol IN Poliza.IdePol%TYPE,
                                p_cCodProd  IN Poliza.CodProd%TYPE,
                                p_cCodMoneda  IN Poliza.CodMoneda%TYPE,
                                p_nNumCert IN Certificado.NumCert%TYPE,
                                p_cCodRamo IN Cert_Ramo.CodRamoCert%TYPE,
                                p_cDescCert IN Certificado.DescCert%TYPE,
                                p_dFecIni IN DATE,
                                p_dFecFin IN DATE,
                                p_cXMLError IN OUT NOCOPY XMLTYPE,
                                p_cError IN OUT VARCHAR2) IS
--
CURSOR c_Ramos IS
  SELECT CodPlan,
    RevPlan,
    CodRamoCert,
    FecIniValid,
    FecFinValid,
    StsCertRamo
  FROM CERT_RAMO
  WHERE IdePol = p_nIdePol
    AND NumCert = p_nNumcert
  ORDER BY NumOrdVis;
--
CURSOR c_impLocalList(cTag IN VARCHAR2) IS
  SELECT codPais,
    codEstado,
    codCiudad,
    codMunicipio,
    ubiGeo,
    tipoVia,
    nomVia,
    numVia,
    tipoInt,
    numInt,
    manzana,
    lote,
    kilome,
    tipoCha,
    nomCha,
    tipoSdi,
    nomSdi,
    referencia,
    resto,
    porcSuma,
    ideGiro,
    mtoValDeclarDirec,
    tep,
    tee,
    anoConstrucc,
    numPisos,
    numSotanos,
    fecInspeccion,
    numCert,
    ideDirec,
    indRiesgoIns,
    indInspeccion,
    notaInspeccion,
    indSinInspeccion,
    usuAutorizador,
    claseTipo,
    propieEdificacion  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
  FROM XMLTABLE(REPLACE('/*/impLocalList/impLocal', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codPais PATH 'codPais',
      codEstado PATH 'codEstado',
      codCiudad PATH 'codCiudad',
      codMunicipio PATH 'codMunicipio',
      ubiGeo PATH 'ubiGeo',
      tipoVia PATH 'tipoVia',
      nomVia PATH 'nomVia',
      numVia PATH 'numVia',
      tipoInt PATH 'tipoInt',
      numInt PATH 'numInt',
      manzana PATH 'manzana',
      lote PATH 'lote',
      kilome PATH 'kilome',
      tipoCha PATH 'tipoCha',
      nomCha PATH 'nomCha',
      tipoSdi PATH 'tipoSdi',
      nomSdi PATH 'nomSdi',
      referencia PATH 'referencia',
      resto PATH 'resto',
      porcSuma PATH 'porcSuma',
      ideGiro PATH 'ideGiro',
      mtoValDeclarDirec PATH 'mtoValDeclarDirec',
      tep PATH 'tep',
      tee PATH 'tee',
      anoConstrucc PATH 'anoConstrucc',
      numPisos PATH 'numPisos',
      numSotanos PATH 'numSotanos',
      fecInspeccion DATE PATH 'fecInspeccion',
      numCert PATH 'numCert',
      ideDirec PATH 'ideDirec',
      indRiesgoIns PATH 'indRiesgoIns',
      indInspeccion PATH 'indInspeccion',
      notaInspeccion PATH 'notaInspeccion',
      indSinInspeccion PATH 'indSinInspeccion',
      usuAutorizador PATH 'usuAutorizador',
      claseTipo PATH 'claseTipo',
      propieEdificacion PATH 'propieEdificacion');  --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
--
nSec          Log_Poliza_SAS_AX.Secuencia%TYPE;
nIdeDirec     Direc_Riesgo_Cert.IdeDirec%TYPE;
cClaseBien    Bien_Cert.ClaseBien%TYPE;
cCodBien      Bien_Cert.CodBien%TYPE;
nMonto        NUMBER(38, 20);
cIndBien      Ramo_Plan_Prod.IndBen%TYPE;
nNumCert      Certificado.NumCert%TYPE;
cStsCert      Certificado.StsCert%TYPE;
dFecIniVig    Certificado.FecIng%TYPE;
dFecFinVig    Certificado.FecFin%TYPE;
--
BEGIN
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePol, NULL, NULL, 'LOG', NULL, SYSDATE, SYSDATE, 'ACT-CERT-TIPOB', '', nSec);
  --
  UPDATE Certificado
  SET DescCert = p_cDescCert
  WHERE IdePol = p_nIdePol
    AND NumCert = p_nNumCert;
  --
  DELETE Bien_Cert_Riesgo
  WHERE IdeDirec IN (
    SELECT IdeDirec
    FROM Direc_Riesgo_Cert
    WHERE IdePol = p_nIdePol
      AND NumCert = p_nNumCert);
  --
  DELETE Direc_Riesgo_Cert
  WHERE IdePol = p_nIdePol
    AND NumCert = p_nNumCert;
  --
  DELETE Bien_Cert
  WHERE IdePol = p_nIdePol
    AND NumCert = p_nNumCert;
  --
  DELETE Deducible_Cobert_Grupo
  WHERE IdeCobert IN (SELECT IdeCobert
                      FROM Cobert_Cert
                      WHERE IdePol  = p_nIdePol
                        AND NumCert = p_nNumCert);
  --
  FOR t IN c_impLocalList(p_cTag) LOOP
    t.numCert := p_nNumCert;
    t.ideDirec := NULL;
    --
    nIdeDirec := pr_Interfase_AX.Graba_Linea_Dir_Masi2(p_nIdePol, t.codPais, t.codEstado, t.codCiudad, t.codMunicipio, t.ubiGeo, t.tipoVia,
      t.nomVia, t.numVia, t.tipoInt, t.numInt, t.manzana, t.lote, t.kilome, t.tipoCha, t.nomCha, t.tipoSdi, t.nomSdi, t.referencia, t.resto,
      t.porcSuma, t.ideGiro, t.mtoValDeclarDirec, t.tep, t.tee, t.anoConstrucc, t.numPisos, t.numSotanos, t.fecInspeccion, t.numCert, t.ideDirec,
      t.indRiesgoIns, t.indInspeccion, t.notaInspeccion, t.indSinInspeccion, t.usuAutorizador, t.claseTipo, t.propieEdificacion, nNumCert, p_cError);  --<RTC-316106> / CBayona / 23-01-2023 / Mensaje de error --<RTC-315519> CBayona / 15-11-2022 / Nuevo atributo
    --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
    IF p_cError != '0,OK' THEN
      RETURN;
    END IF;
    --<F RTC-316106> / CBayona / 23-01-2023
    --
    IF nIdeDirec > 0 THEN
    --validar estado del certificado
      SELECT MAX(StsCert), MAX(FecIng), MAX(FecFin)
        INTO cStsCert, dFecIniVig, dFecFinVig
        FROM Certificado
       WHERE IdePol = p_nIdePol
         AND NumCert = p_nNumcert;
      --
      IF cStsCert IN ('VAL', 'INC', 'MOD') THEN
        -- eliminar los bienes por cada direccion
        BEGIN
          pr_Bien_Cert_Util.Eliminar_Bien_Riesgo(nIdeDirec);

        EXCEPTION
          WHEN OTHERS THEN
            p_cError := '1,' || SQLERRM;
            DBMS_OUTPUT.Put_Line('p_cError: ' || p_cError || ' - ideDirec: ' || nIdeDirec);
            RETURN;
        END;
      END IF;
      --
      FOR r IN c_Ramos LOOP
        SELECT NVL(MAX(IndBien), 'N')
          INTO cIndBien
          FROM Ramo_Plan_Prod
         WHERE CodProd = p_cCodProd
           AND CodPlan = r.CodPlan
           AND RevPlan = r.RevPlan
           AND CodRamoPlan = r.CodRamoCert
         ORDER BY NumOrdImp;
        --
        IF cIndBien = 'S' THEN
          BEGIN
            nMonto := t.mtoValDeclarDirec;
            IF t.claseTipo IS NOT NULL AND nMonto > 0 THEN
              cClaseBien := SUBSTR(t.claseTipo, 1, 3);
              cCodBien   := SUBSTR(t.claseTipo, 4, 4);
            END IF;
            --
            pr_Bien_Cert_Util.Genera_Bien_Masi(p_nIdePol, p_nNumcert, p_cCodProd, r.CodRamoCert, r.CodPlan, r.RevPlan,
              nIdeDirec, cClaseBien, cCodBien, nMonto, p_cCodMoneda, dFecIniVig, dFecFinVig);

            --Actualiza Tablas Bien_Cert y Bien_Cert_Riesgo
            pr_Bien_Cert_Util.Actualiza_Bien_Riesgo(p_nIdePol, p_nNumcert, r.CodRamoCert, p_cCodProd, r.CodPlan, r.RevPlan, 'VAL');

          EXCEPTION
            WHEN OTHERS THEN
              p_cError := '1,' || SQLERRM;
              DBMS_OUTPUT.Put_Line('>>> GENBIEN p_cError: ' || p_cError);
              RETURN;
          END;
        END IF;
      END LOOP;
    END IF;
  END LOOP;
  --
  BEGIN
    pr_Certificado.Incluir(p_nIdePol, p_nNumCert);
    --
    pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePol, NULL, NULL, 'LOG', 'OK', SYSDATE, SYSDATE, 'ACT-CERT-TIPOB', '', nSec);

  EXCEPTION
    WHEN OTHERS THEN
      p_cError := '1,Error en Incluir Certificado - (' || SQLCODE || '-' || SQLERRM || ')';
      pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'ACT-CERT-TIPOB', '', nSec);
  END;
END Actualiza_Cert_Tipo_B;


/*-----------------------------------------------------------------------------
  Nombre      : Dec_Exist_TipoB
  Proposito   : DeclaraciÛn de Existencias Tipo B
  Referencias : <RTC-314184> Endosos de DeclaraciÛn de existencias tipo "A y B"
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    30/09/2022    Christian Bayona           CreaciÛn
    14/03/2023    Christian Bayona           <RTC 316449> ModificaciÛn, se encola error de la copia del certificado
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Dec_Exist_TipoB ( p_cXMLInput  IN CLOB,
                            p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numCert,
    numPol,
    codProd,
    codCli,
    codRamo,
    fecInicio,
    fecFin,
    descCert,
    codMoneda,
    mtoPrima,
    codUsuario
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      codCli PATH 'codCli',
      codRamo PATH 'codRamo',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      descCert PATH 'descCert',
      codMoneda PATH 'codMoneda',
      mtoPrima VARCHAR2(15) PATH 'mtoPrima',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumCert             Certificado.NumCert%TYPE;
nNewNumCert          Certificado.NumCert%TYPE;
nNumPol              Poliza.NumPol%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cMtoPrima            VARCHAR2(16);
cError               VARCHAR2(2000);
nNumOper             Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
nPos                 NUMBER(4);
--
BEGIN
  l_xml_Error0 := NULL;
  cTag := GetTagEndoso(kTipo_Dec_Exist_TipoB, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    DBMS_OUTPUT.Put_Line('>> nIdePol: ' || nIdePol);
    --Validaciones previas
    --PreValidacion(r.IdePol, r.numCert, r.numPol, r.codProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    PreValidacion(r.IdePol, r.numCert, r.numPol, r.codProd, r.fecInicio, r.fecFin, r.codUsuario, cError, l_xml_Error0);  --<RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    IF cError = '0,OK' THEN
      nNumCert := pr_Interfase_AX.Existe_Cert_DE_Tipo_B(nIdePol, r.numCert);
      DBMS_OUTPUT.Put_Line('>>> nNumCert: ' || nNumCert);
      IF nNumCert < 0 THEN
        IF r.numCert IS NULL THEN
          cError := '1,No ha encontrado un certificado previo de DeclaraciÛn de Existencias';
          PushError('DE_TIPOB', '1', cError, SYSDATE, USER, l_xml_Error0);
        ELSE
          cError := '1,El certificado ' || r.numCert || ' no corresponde a una DeclaraciÛn de Existencias';
          PushError('DE_TIPOB', '1', cError, SYSDATE, USER, l_xml_Error0);
        END IF;
      ELSE
        --Inicializar variables
        nNumPol := r.numPol;
        cCodProd := r.codProd;
        cCodUsr := r.codUsuario;
        --
        cMtoPrima := NVL(r.mtoPrima, '0');
        IF cMtoPrima = '0' THEN
          cMtoPrima := '';
        ELSE
          cMtoPrima := 'S' || cMtoPrima;
        END IF;

        --Copiar Certificado
        pr_Interfase_AX.CopiaCertificado(nIdePol, nNumCert, r.codCli, r.fecInicio, r.fecFin, 1, nNewNumCert, cError);

        IF cError = '0,OK' THEN
          Actualiza_Cert_Tipo_B(cTag, p_cXMLInput, nIdePol, cCodProd, r.codMoneda, nNewNumCert, r.codRamo, r.descCert, r.fecInicio, r.fecFin, l_xml_Error0, cError) ;
        --<I RTC 316449> / CBayona / 14-03-2023 / Se encola errores en el caso los hubiera durante la copia del certificado
        ELSE
          pr_Interfase_API_Endoso2.PushError('DE_TIPOB', '1', cError, SYSDATE, USER, l_xml_Error0);
        --<F RTC 316449>
        END IF;
        --
        IF cError = '0,OK' THEN
          --Ejecutar procedimiento de Datos Particulares
          pr_Interfase_API_Endoso2.Datos_Particulares(cTag, p_cXMLInput, nIdePol, nNewNumCert, r.fecInicio, r.fecFin, r.codRamo, l_xml_Error0, cError);
          --
          IF cError = '0,OK' THEN
            --Insertar Anexo / Plan Nuevo Formato
            Anexo_Pol(cTag, p_cXMLInput, nIdePol, cCodUsr, cMsgValida, l_xml_Error0, cError);
          END IF;
        END IF;
      END IF;
    END IF;
  END LOOP;
  ---
  DBMS_OUTPUT.Put_Line('>>>> ACTIVAR');
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NSSS' || cMtoPrima,
                -- Variables de retorno
                p_nNumOper => nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Dec_Exist_TipoB, nIdePol, nNumCert, nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
  --
  --Insertar el n˙mero de Certificado generado en la respuesta
  IF cError = '0,OK' THEN
    nPos := INSTR(p_cXMLOutput, '</numOper>');
    p_cXMLOutput := SUBSTR(p_cXMLOutput, 1, nPos + 10) || '  <numCertGen>' || nNewNumCert || '</numCertGen>' || SUBSTR(p_cXMLOutput, nPos + 10);
  END IF;
END Dec_Exist_TipoB;


/*-----------------------------------------------------------------------------
  Nombre      : Aseg_Adic_EntNOFin_Cert
  Proposito   : Endoso de asegurado adicional para productos vehiculares
  Referencias : <RTC-316106>
  Parametros  :
                p_cXMLInput      XML (CLob) con los par·metros de entrada necesario para crear el endoso
  Retorno     : p_cXMLOutput     XML (CLob) con resultado del proceso
  Log de Cambios
    Fecha         Autor                      DescripciÛn
    31/01/2023    Karl Alcala                CreaciÛn
    12/05/2023    Christian Bayona           <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
  ----------------------------------------------------------------------------*/
PROCEDURE Aseg_Adic_EntNOFin_Cert ( p_cXMLInput  IN CLOB,
                                    p_cXMLOutput IN OUT NOCOPY CLOB) IS
--
CURSOR c_reqEndoso(cTag IN VARCHAR2) IS
  SELECT idePol,
    numPol,
    codProd,
    codUsuario,
    numCert
  FROM XMLTABLE(REPLACE('/*/reqEndoso', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      idePol PATH 'idePol',
      numPol PATH 'numPol',
      codProd PATH 'codProd',
      numCert PATH 'numCert',
      codUsuario PATH 'codUsuario');
--
nIdePol              Poliza.IdePol%TYPE;
nNumPol              Poliza.NumPol%TYPE;
nNumCert              Certificado.numCert%TYPE;
cCodProd             Poliza.CodProd%TYPE;
cCodUsr              VARCHAR2(15);
cError               VARCHAR2(2000);
p_nNumOper           Factura.NumOper%TYPE;
cMsgValida           SYS_RefCursor;
tTab_NoAutoriza      acselx.TDU_Tabla_Autoriza_Violacion;
---
l_xml_Error0         XMLTYPE;
cTag                 VARCHAR2(30);
--
BEGIN
  l_xml_Error0 := NULL;

  cTag := GetTagEndoso(kTipo_Aseg_Adic_EntNFCert, 'I');
  DBMS_OUTPUT.Put_Line('>>>> GeneraciÛn de ' || cTag || ' ***');
  FOR r IN c_reqEndoso(cTag) LOOP
    nIdePol := r.idePol;
    --Validaciones previas
    --PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, r.codUsuario, cError, l_xml_Error0);  --<RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B
    PreValidacion(r.IdePol, 0, r.NumPol, r.CodProd, NULL, NULL, r.codUsuario, cError, l_xml_Error0);  --<RTC 316878> / Christian Bayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de DeclaraciÛn de Existencias Tipo B

    IF cError = '0,OK' THEN
      --Inicializar variables
      nNumPol := r.numPol;
      cCodProd := r.codProd;
      cCodUsr := r.codUsuario;
      nNumCert := r.numCert;
      --Insertar Anexo / Plan Nuevo Formato
      Anexo_Cert(cTag, p_cXMLInput, nIdePol, nNumCert, cCodUsr, cMsgValida, cError);
    END IF;
  END LOOP;
  ---
  IF cError = '0,OK' THEN
    --Valida polÌticas y activa la pÛliza
    Activar_Pol(p_cTag => cTag,
                p_cXMLInput => p_cXMLInput,
                p_nIdePol => nIdePol,
                p_nNumPol => nNumPol,
                p_cCodProd => cCodProd,
                p_cCodUsuario => cCodUsr,
                p_cIndTemp => 'S',
                p_cTxtAdic => 'NNNS',
                -- Variables de retorno
                p_nNumOper => p_nNumOper,
                p_cMsgValida => cMsgValida,
                p_TabNoAutoriza => tTab_NoAutoriza,
                p_cXMLError => l_xml_Error0,
                p_cError => cError);
  END IF;
  --Enviar los resultados de la operaciÛn
  SetXMLOut(kTipo_Aseg_Adic_EntNFCert, nIdePol, NULL, p_nNumOper, cError, cMsgValida, tTab_NoAutoriza, l_xml_Error0, p_cXMLOutput);
  DBMS_OUTPUT.Put_Line('>>>> p_cXMLOutput' || p_cXMLOutput);
END Aseg_Adic_EntNOFin_Cert;

/*  <RTC 316449> Se comenta porque se ha creado un reemplazo para este procedimiento
*------------------------------------------------------------------------
  Nombre     : Datos_Impresion
  PropÛsito  : Procedimiento que devuelve la informaciÛn necesaria para realizar la impresiÛn
  Referencias: <RTC-316106>
  Par·metros :
                 p_nIdePol         NUMBER       Identificador de PÛliza
                 p_nNumOper        NUMBER       N˙mero de OperaciÛn
                 p_dFecIni         DATE         Fecha de Inicio de Vigencia
                 p_dFecFin         DATE         Fecha de Fin de Vigencia
  Retorno    :   p_cXMLOut         CLOB         Trama resultante

  Log de Cambios:
    Fecha         Autor               DescripciÛn
    31/01/2023    Christian Bayona    CreaciÛn
 -----------------------------------------------------------------------------*
PROCEDURE Datos_Impresion(p_nIdePol  IN Poliza.IdePol%TYPE,
                          p_nNumOper IN Oper_Pol.NumOper%TYPE,
                          p_dFecIni  IN DATE DEFAULT NULL,
                          p_dFecFin  IN DATE DEFAULT NULL,
                          --
                          p_cXMLOut  OUT CLOB) IS
--
cCodPlan       Cert_Ramo.CodPlan%TYPE;
cRevPlan       Cert_Ramo.RevPlan%TYPE;
cTitulo        VARCHAR2(500);
cTitConsPrima  VARCHAR2(50);
cTotConsPrima  VARCHAR2(50);
nNumAcre       Acreencia.NumAcre%TYPE;
nNumOblig      Obligacion.NumOblig%TYPE;
nMtoOper       Acreencia.MtoAcreMoneda%TYPE;
nMtoComm       Det_Comi_Riesgo.MtoComiRiesgoMoneda%TYPE;
cNumTramite    Poliza_Tramite.NumTramite%TYPE;
cCodAut        Usuario.CodUsr%TYPE;
cCodUsuario    Operacion.CodUsuario%TYPE;
xXMLAux        XMLTYPE;
xXMLOut        XMLTYPE;
cAnexo         CLOB;
--
CURSOR c_Impresion IS
  SELECT p.CodProd,
    p.NumPol,
    p.FecIniVig,
    p.FecFinVig,
    p.CodCli,
    p.CodMoneda,
    op.NumOper,
    op.TipoOp,
    op.FecMov,
    TO_CHAR(op.FecMov, '"San Isidro, "DD" de "fmMonth" de "YYYY', 'NLS_DATE_LANGUAGE=SPANISH') cFecMov,
    op.MtoOper,
    op.PoliFactura,
    --
    t.NomTer,
    t.ApeTer,
    t.TipoTer, --E(mpresa)
    l.Descrip TipoDoc,
    di.NumIdDoc,
    --
    'AL CONTADO' FormaPago
  FROM LVal l,
    Docs_Identidad di,
    Cliente c,
    Tercero t,
    Oper_Pol op,
    Poliza p
  WHERE p.IdePol = p_nIdePol
    AND op.IdePol = p.IdePol
    AND op.NumOper = NVL(p_nNumOper, op.NumOper)
    AND op.FecMov >= NVL(p_dFecIni, op.FecMov)
    AND op.FecMov < NVL(p_dFecFin, op.FecMov + 1)
    AND c.CodCli = p.CodCli
    AND t.NumId = c.NumId
    AND di.NumId = t.NumId
    AND l.TipoLVal = 'TIPODOC'
    AND l.CodLVal = di.TipoIdDoc
  ORDER BY op.NumOper;
--
CURSOR c_Anexo(nNumOper IN Oper_Pol.NumOper%TYPE) IS
  SELECT mtap.IdeAnexo,
      mtap.NumMod,
      mtap.TextoAnexo,
      mtap.CodUsu
  FROM Mod_Texto_Anexo_Pol mtap
  WHERE mtap.IdePol = p_nIdePol
    AND mtap.NumOper = nNumOper;
--
CURSOR c_Acreencia(nNumAcre IN Acreencia.NumAcre%TYPE) IS
  SELECT ca.DescCptoAcre,
    da.MtoDetAcreMoneda
  FROM Cpto_Acre ca,
    Det_Acre da
  WHERE da.NumAcre = nNumAcre
    AND ca.CodGrupoAcre = da.CodGrupoAcre
    AND ca.CodCptoAcre = da.CodCptoAcre;
--
CURSOR c_Obligacion(nNumOblig IN Obligacion.NumOblig%TYPE) IS
  SELECT ce.DescCptoEgre,
    d.MtoDetObligMoneda
  FROM Det_Oblig d,
    Cpto_Egre ce
  WHERE d.NumOblig = nNumOblig
    AND ce.CodClaEgre = d.CodClaEgre
    AND ce.CodCptoEgre = d.CodCptoEgre;
--
BEGIN
  FOR r IN c_Impresion LOOP
    --Desc Producto
    SELECT MAX(CodPlan), MAX(RevPlan)
    INTO cCodPlan, cRevPlan
    FROM Cert_Ramo c
    WHERE c.IdePol = p_nIdePol;

    cTitulo := pr_Plan_Prod.Titulo_Prod_Plan(r.CodProd, cCodPlan, cRevPlan ,'POLIZA','ENCABEZADO') ;
    --
    cTitConsPrima := 'CONSOLIDADO DE PRIMA';
    cTotConsPrima := 'PRIMA TOTAL';
    xXMLAux := NULL;
    --
    IF r.MtoOper > 0 THEN --Acreencia
      SELECT SUM(dcr.MtoComiRiesgoMoneda) MtoComm,
        MIN(gf.NumAcre) NumAcre
      INTO nMtoComm, nNumAcre
      FROM Cond_Financiamiento cf,
        Giros_Financiamiento gf,
        Det_Comi_Riesgo dcr
      WHERE cf.IdePol = p_nIdePol
        AND cf.NumOper = r.NumOper
        AND gf.NumFinanc = cf.NumFinanc
        AND dcr.NumAcre(+) = gf.NumAcre;
      --
      SELECT MAX(a.MtoAcreMoneda)
      INTO nMtoOper
      FROM Acreencia a
      WHERE a.NumAcre = nNumAcre;
      --
      FOR rA IN c_Acreencia(nNumAcre) LOOP
        IF rA.DescCptoAcre = 'PRIMA' THEN
          rA.DescCptoAcre := 'PRIMA NETA';
        END IF;
        --
        SELECT XMLCONCAT(xXMLAux, XMLAGG(
          XMLELEMENT("linea",
            XMLFOREST(
              rA.DescCptoAcre AS "concepto",
              rA.MtoDetAcreMoneda AS "monto"))))
        INTO xXMLAux
        FROM Dual;
      END LOOP;

    ELSIF r.MtoOper < 0 THEN --ObligaciÛn
      cTitConsPrima := 'CONSOLIDADO DE PRIMA A DEVOLVER';
      cTotConsPrima := 'PRIMA TOTAL A DEVOLVER';
      --
      SELECT MAX(o.NumOblig),
        MAX(o.SldoObligMoneda)
      INTO nNumOblig, nMtoOper
      FROM Obligacion o
      WHERE o.NumOper = r.NumOper;
      --
      FOR rO IN c_Obligacion(nNumOblig) LOOP
        SELECT XMLCONCAT(xXMLAux, XMLAGG(
          XMLELEMENT("linea",
            XMLFOREST(
              rO.DescCptoEgre AS "concepto",
              rO.MtoDetObligMoneda AS "monto"))))
        INTO xXMLAux
        FROM Dual;
      END LOOP;

    ELSE --ESV
      nMtoOper := 0.00;
      nMtoComm := 0.00;
      --
      SELECT XMLAGG(
        XMLELEMENT("linea",
          XMLFOREST(
            l.Descrip AS "concepto",
            0 AS "monto")))
      INTO xXMLAux
      FROM LVal l
      WHERE l.TipoLVal = 'CONSPRIM';
    END IF;
    --
    FOR rA IN c_Anexo(r.NumOper) LOOP
      cAnexo := SUBSTR(rA.TextoAnexo, 1);
    END LOOP;
    --
    SELECT MAX(CodUsuario)
    INTO cCodUsuario
    FROM Operacion
    WHERE IdeOp = r.NumOper;
    --
    cCodAut := pr_Interfase_AX.CodAutorizador(p_nIdePol, r.NumOper);
    --
    SELECT MAX(NumTramite)
    INTO cNumTramite
    FROM Poliza_Tramite
    WHERE IdePol = p_nIdePol
      AND NumOper = r.NumOper
      AND ROWNUM < 2;
  ----
    SELECT XMLCONCAT(xXMLOut, XMLAGG(
      XMLELEMENT("endoso",
        XMLFOREST(
          r.CodProd AS "codProd",
          cTitulo AS "descProducto",
          r.NumPol AS "numPol",
          r.FecIniVig AS "fecIniVig",
          r.FecFinVig AS "fecFinVig",
          r.CodCli AS "codCli",
          r.CodMoneda AS "codMoneda",
          r.TipoOp AS "tipoOp",
          r.FecMov AS "fecMov",
          TO_CHAR(r.FecMov, '"San Isidro, "DD" de "fmMonth" de "YYYY', 'NLS_DATE_LANGUAGE=SPANISH') AS "cFecMov",
          r.MtoOper AS "mtoOper",
          CASE WHEN r.TipoTer = 'E' THEN r.NomTer ELSE r.NomTer || ' ' || r.ApeTer END AS "nomCliente",
          r.TipoDoc AS "tipoDoc",
          r.NumIdDoc AS "numDoc",
          -- r.TextoAnexo AS "anexo",
          cAnexo AS "anexo",
          r.FormaPago AS "formaPago",
          cTitConsPrima AS "etTituloConsPrima",
          cTotConsPrima AS "etTotalConsPrima",
          xXMLAux AS "secPrimas",
          nMtoOper AS "primaTotal",
          nMtoComm AS "mtoComision",
          r.NumOper AS "numOper",
          cCodUsuario AS "codEmisor",
          cCodAut AS "codAutorizador",
          cNumTramite AS "numTramite"))))
    INTO xXMLOut
    FROM Dual;
  END LOOP;
  --
  SELECT XMLSERIALIZE(CONTENT DELETEXML(XMLELEMENT("reporteEndoso", xXMLOut), '//*[.="" or contains(.,"?")]') INDENT)
  INTO p_cXMLOut
  FROM Dual;
END Datos_Impresion; */


/*------------------------------------------------------------------------
  Nombre     : Datos_Impresion
  PropÛsito  : Procedimiento que devuelve la informaciÛn necesaria para realizar la impresiÛn
  Referencias: <RTC-316449> Endoso DeclaraciÛn de existencias tipo "A y B"
  Par·metros :
                 p_cXMLIn          CLOB         Trama de par·metros
  Retorno    :   p_cXMLOut         CLOB         Trama resultante

  Log de Cambios:
    Fecha         Autor               DescripciÛn
    31/01/2023    Christian Bayona    CreaciÛn
    17/02/2023    Christian Bayona    ModificaciÛn, se reconstruyo el procedimiento
    22/02/2023    Christian Bayona    ModificaciÛn, se agrega reporte LQ
    28/02/2023    Christian Bayona    ModificaciÛn, se usa valores registrar LVal
    02/03/2023    Christian Bayona    ModificaciÛn, se agrega columna en respuesta
    30/03/2023    Christian Bayona    <RTC 316747> [Proy API Endoso] ImpresiÛn Vehicular
 -----------------------------------------------------------------------------*/
PROCEDURE Datos_Impresion(p_cXMLIn   IN CLOB,
                          --
                          p_cXMLOut  OUT CLOB) IS
--
cCodPlan        Cert_Ramo.CodPlan%TYPE;
cRevPlan        Cert_Ramo.RevPlan%TYPE;
cTitulo         VARCHAR2(500);
cTitConsPrima   VARCHAR2(50);
cTotConsPrima   VARCHAR2(50);
nNumAcre        Acreencia.NumAcre%TYPE;
nNumOblig       Obligacion.NumOblig%TYPE;
nMtoOper        Acreencia.MtoAcreMoneda%TYPE;
nMtoComm        Det_Comi_Riesgo.MtoComiRiesgoMoneda%TYPE;
cNumTramite     Poliza_Tramite.NumTramite%TYPE;
cCodAut         Usuario.CodUsr%TYPE;
cCodUsuario     Operacion.CodUsuario%TYPE;
xXMLAux         XMLTYPE;
xXMLOut         XMLTYPE;
cAnexo          CLOB;
cTextoBien      Endosatario_Cert.TextoBien%TYPE;
cCodCli         Cliente.CodCli%TYPE;
nNumId          Tercero.NumId%TYPE;
cCodTexto       Endosatario_Cert.CodTexto%TYPE;
cCorreo         Texto_Cesion_Correo.Correo%TYPE;
nMtoPrimCom     Det_Acre.MtoDetAcreMoneda%TYPE;
cDistrito       Municipio.DescMunicipio%TYPE;
cCodInter       Obligacion.CodInterLider%TYPE;
cRuta           Desc_Ruta.DescRuta%TYPE;
cNomInter       VARCHAR2(250);
cDirecInter     Tercero.Direc%TYPE;
cDistInter      Municipio.DescMunicipio%TYPE;
cTipoDocPag     Obligacion.TipoDocPag%TYPE;
cDescPlan       Plan_Prod.DescPlanProd%TYPE;
cDescProd       Producto.DescProd%TYPE;
--<I RTC 316449> / CBayona / 22-02-2023 / Se agrega variable
cLiquid         VARCHAR2(20);
cNomRep         VARCHAR2(20);
cFirma          CLOB;
nNumFinanc      Cond_Financiamiento.NumFinanc%TYPE;
cCodFirma       VARCHAR2(2);
cDirec          VARCHAR2(300);
cTCEA           VARCHAR2(5);
cModalidad      VARCHAR2(150);
dFecEmi         DATE;
cMoneda         VARCHAR2(50);
xXMLAux1        XMLTYPE;
--<F RTC 316449>
--
CURSOR c_ReporteEndoso IS
  SELECT tipoRep,
    idePol,
    numCert,  --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular
    numOper,
    fecInicio,
    fecFin,
    numOperBco
  FROM XMLTABLE('/reporteEndoso/endoso' PASSING xmltype(p_cXMLIn)
    COLUMNS
      tipoRep PATH 'tipoRep',
      idePol PATH 'idePol',
      numCert PATH 'numCert',  --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular
      numOper PATH 'numOper',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      numOperBco PATH 'numOperBco');
--
CURSOR c_Impresion(nIdePol IN Poliza.IdePol%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE, dFecIni IN DATE, dFecFin IN DATE) IS
  SELECT p.IdePol,
    p.CodProd,
    p.CodPol,
    p.CodOfiEmi,
    p.NumPol,
    p.FecIniVig,
    p.FecFinVig,
    p.CodCli,
    p.CodMoneda,
    op.NumOper,
    op.TipoOp,
    op.FecMov,
    TO_CHAR(op.FecMov, pr.Busca_LVal('PRMIMPRE', 'UBIFECMOV'), 'NLS_DATE_LANGUAGE=SPANISH') cFecMov,  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal  --'"San Isidro, "DD" de "fmMonth" de "YYYY'
    op.MtoOper,
    op.PoliFactura,
    --
    t.NumId,
    t.NomTer,
    t.ApeTer,
    t.ApeMatTer,
    t.Direc,
    t.CodPais,
    t.CodEstado,
    t.CodCiudad,
    t.CodMunicipio,
    t.TipoTer, --E(mpresa)
    l.Descrip TipoDoc,
    di.NumIdDoc,
    --
    'AL CONTADO' FormaPago
  FROM LVal l,
    Docs_Identidad di,
    Cliente c,
    Tercero t,
    Oper_Pol op,
    Poliza p
  WHERE p.IdePol = nIdePol
    AND op.IdePol = p.IdePol
    AND op.NumOper = NVL(nNumOper, op.NumOper)
    AND op.FecMov >= NVL(dFecIni, op.FecMov)
    AND op.FecMov < NVL(dFecFin, op.FecMov + 1)
    AND c.CodCli = p.CodCli
    AND t.NumId = c.NumId
    AND di.NumId = t.NumId
    AND l.TipoLVal = 'TIPODOC'
    AND l.CodLVal = di.TipoIdDoc
  ORDER BY op.NumOper;
--
CURSOR c_Anexo(nIdePol IN Poliza.IdePol%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE) IS
  SELECT mtap.IdeAnexo,
      mtap.NumMod,
      mtap.TextoAnexo,
      mtap.CodUsu
  FROM Mod_Texto_Anexo_Pol mtap
  WHERE mtap.IdePol = nIdePol
    AND mtap.NumOper = nNumOper;
--<I RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular
CURSOR c_AnexoCert(nIdePol IN Poliza.IdePol%TYPE, nNumCert IN Certificado.NumCert%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE) IS
  SELECT mtac.IdeAnexo,
      mtac.NumMod,
      mtac.TextoAnexo
  FROM Mod_Texto_Anexo_Cert mtac
  WHERE mtac.IdePol = nIdePol
    AND mtac.NumCert = nNumCert
    AND mtac.NumOper = nNumOper;
--<F RTC 316747>
CURSOR c_Acreencia(nNumAcre IN Acreencia.NumAcre%TYPE) IS
  SELECT ca.DescCptoAcre,
    da.NumDetAcre,
    da.CodMoneda,
    da.MtoDetAcreMoneda
  FROM Cpto_Acre ca,
    Det_Acre da
  WHERE da.NumAcre = nNumAcre
    AND ca.CodGrupoAcre = da.CodGrupoAcre
    AND ca.CodCptoAcre = da.CodCptoAcre
  ORDER BY da.NumDetAcre;
--
CURSOR c_Obligacion(nNumOblig IN Obligacion.NumOblig%TYPE) IS
  SELECT ce.DescCptoEgre,
    d.NumDetOblig,
    d.CodMoneda,
    d.MtoDetObligMoneda
  FROM Det_Oblig d,
    Cpto_Egre ce
  WHERE d.NumOblig = nNumOblig
    AND ce.CodClaEgre = d.CodClaEgre
    AND ce.CodCptoEgre = d.CodCptoEgre
  ORDER BY d.NumDetOblig;
--
--<I RTC 316449> / CBayona / 22-02-2023 / Se agrega cursor de cupones
CURSOR c_Giros(nNumFinanc IN Cond_Financiamiento.NumFinanc%TYPE) IS
  SELECT gf.NumGiro,
    gf.NumAcre,
    gf.FecVct,
    gf.MtoGiroForaneo
  FROM Giros_Financiamiento gf
  WHERE NumFinanc = nNumFinanc;
--<F RTC 316449>
BEGIN
  FOR rD IN c_ReporteEndoso LOOP
    rD.tipoRep := NVL(rD.tipoRep, 'DG');
    --
    FOR r IN c_Impresion(rD.IdePol, rD.NumOper, rD.FecInicio, rD.FecFin) LOOP
      --IF rD.tipoRep IN ('DG', 'LQ') THEN  --<RTC 316449> Se agrega nuevo selector LQ --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular / Se comenta
      IF rD.tipoRep IN ('DG', 'DGC', 'LA', 'LQ') THEN  --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular
        --Desc Producto
        SELECT MAX(CodPlan), MAX(RevPlan)
        INTO cCodPlan, cRevPlan
        FROM Cert_Ramo c
        WHERE c.IdePol = r.IdePol;

        cTitulo := pr_Plan_Prod.Titulo_Prod_Plan(r.CodProd, cCodPlan, cRevPlan ,'POLIZA','ENCABEZADO') ;
        --<I RTC 316747> / CBayona / 30-03-2023 / [Proy API Endoso] ImpresiÛn Vehicular
        --Se obtiene descripciÛn del producto
        IF rD.tipoRep = 'DGC' THEN
          SELECT MAX(p.DescProd)
          INTO cTitulo
          FROM Producto p
          WHERE CodProd = r.CodProd;
        END IF;
        --<F RTC 316747>
        --<I RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
        cTitConsPrima := pr.Busca_LVal('PRMIMPRE', 'TITCPRIM'); --'CONSOLIDADO DE PRIMA';
        cTotConsPrima := pr.Busca_LVal('PRMIMPRE', 'TOTCPRIM'); --'PRIMA TOTAL';
        --<F RTC 316449>
        xXMLAux := NULL;
        --
        IF r.MtoOper > 0 THEN --Acreencia
          cNomRep := 'EMI00173';
          --
          SELECT NVL(SUM(dcr.MtoComiRiesgoMoneda), 0) MtoComm, --<RTC 316449> / CBayona / 22-02-2023 / Se agrega NVL para comisiÛn
            MIN(gf.NumAcre) NumAcre
          INTO nMtoComm, nNumAcre
          FROM Cond_Financiamiento cf,
            Giros_Financiamiento gf,
            Det_Comi_Riesgo dcr
          WHERE cf.IdePol = r.IdePol
            AND cf.NumOper = r.NumOper
            AND gf.NumFinanc = cf.NumFinanc
            AND dcr.NumAcre(+) = gf.NumAcre;
          --<I RTC 316449> / CBayona / 22-02-2023 / Se obtiene el nombre del intermediario
          SELECT TO_CHAR(NVL(MAX(f.NumFact), 0)),
            MAX(CodInter)
          INTO cLiquid, cCodInter
          FROM Factura f
          WHERE NumOper = rD.NumOper;
          --
          SELECT CASE WHEN t.TipoTer = 'E' THEN t.NomTer ELSE t.ApeTer || ' ' || t.ApeMatTer || ' ' || t.NomTer END AS "nomInter"
          INTO cNomInter
          FROM Intermediario i,
            Tercero t
          WHERE i.CodInter = cCodInter
            AND t.TipoId = i.TipoId
            AND t.NumId = i.NumId
            AND t.DvId = i.DvId;
          --<F RTC 316449>
          SELECT MAX(a.MtoAcreMoneda)
          INTO nMtoOper
          FROM Acreencia a
          WHERE a.NumAcre = nNumAcre;
          --
          nMtoPrimCom := 0;
          FOR rA IN c_Acreencia(nNumAcre) LOOP
            IF rA.DescCptoAcre = 'PRIMA' THEN
              rA.DescCptoAcre := pr.Busca_LVal('PRMIMPRE', 'CPRIMNET'); --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal  --'PRIMA NETA';
            END IF;
            --
            IF rA.DescCptoAcre = 'I.G.V.' THEN
              SELECT XMLCONCAT(xXMLAux, XMLAGG(
              XMLELEMENT("linea",
                XMLFOREST(
                  rA.NumDetAcre AS "orden",
                  pr.Busca_LVal('PRMIMPRE', 'PRIMCOM') AS "concepto", --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal --'PRIMA COMERCIAL'
                  pr.Busca_LVal('SIMBMON', rA.CodMoneda) AS "moneda", --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
                  nMtoPrimCom AS "monto"))))
              INTO xXMLAux
              FROM Dual;
              --
              nMtoPrimCom := 0;
              rA.NumDetAcre := rA.NumDetAcre + 1;

            ELSE
              nMtoPrimCom := nMtoPrimCom + rA.MtoDetAcreMoneda;
            END IF;
            --
            SELECT XMLCONCAT(xXMLAux, XMLAGG(
              XMLELEMENT("linea",
                XMLFOREST(
                  rA.NumDetAcre AS "orden",
                  rA.DescCptoAcre AS "concepto",
                  pr.Busca_LVal('SIMBMON', rA.CodMoneda) AS "moneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
                  rA.MtoDetAcreMoneda AS "monto"))))
            INTO xXMLAux
            FROM Dual;
          END LOOP;

        ELSIF r.MtoOper < 0 THEN --ObligaciÛn
          --<I RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
          cTitConsPrima := pr.Busca_LVal('PRMIMPRE', 'TOTCPRIMD'); --'CONSOLIDADO DE PRIMA A DEVOLVER';
          cTotConsPrima := pr.Busca_LVal('PRMIMPRE', 'TOTCPRIMD'); --'PRIMA TOTAL A DEVOLVER';
          --<F RTC 316449>
          cNomRep := 'EMI00287';
          --
          SELECT MAX(o.NumOblig),
            MAX(o.SldoObligMoneda)
          INTO nNumOblig, nMtoOper
          FROM Obligacion o
          WHERE o.NumOper = r.NumOper;
          --
          cLiquid := TO_CHAR(nNumOblig);
          --
          FOR rO IN c_Obligacion(nNumOblig) LOOP
            SELECT XMLCONCAT(xXMLAux, XMLAGG(
              XMLELEMENT("linea",
                XMLFOREST(
                  rO.NumDetOblig AS "orden",
                  rO.DescCptoEgre AS "concepto",
                  pr.Busca_LVal('SIMBMON', rO.CodMoneda) AS "moneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
                  rO.MtoDetObligMoneda AS "monto"))))
            INTO xXMLAux
            FROM Dual;
          END LOOP;

        ELSE --ESV
          nMtoOper := 0.00;
          nMtoComm := 0.00;
          cLiquid := '0';
          cNomRep := 'EMI00287';
          --
          SELECT XMLAGG(
            XMLELEMENT("linea",
              XMLFOREST(
                ROWNUM AS "orden",
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "moneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
                l.Descrip AS "concepto",
                0 AS "monto")))
          INTO xXMLAux
          FROM LVal l
          WHERE l.TipoLVal = 'CONSPRIM';
          --
          SELECT XMLCONCAT(xXMLAux, XMLAGG(
            XMLELEMENT("linea",
              XMLFOREST(
                3 AS "orden",
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "moneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
                pr.Busca_LVal('PRMIMPRE', 'TOTCPRIM') AS "concepto",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal --'PRIMA COMERCIAL'
                0 AS "monto"))))
          INTO xXMLAux
          FROM Dual;
          --
          SELECT XMLCONCAT(xXMLAux, XMLAGG(
            XMLELEMENT("linea",
              XMLFOREST(
                4 AS "orden",
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "moneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
                'I.G.V.' AS "concepto",
                0 AS "monto"))))
          INTO xXMLAux
          FROM Dual;
        END IF;
        --<I RTC 316747> / CBayona / 30-03-2023 / [Proy API Endoso] ImpresiÛn Vehicular
        --Se obtiene el texto anexo del Certificado
        IF rD.tipoRep = 'DGC' THEN
          FOR rA IN c_AnexoCert(r.IdePol, rD.NumCert, r.NumOper) LOOP
            cAnexo := SUBSTR(rA.TextoAnexo, 1);
          END LOOP;
        ELSE
        --<F RTC 316747>
          FOR rA IN c_Anexo(r.IdePol, r.NumOper) LOOP
            cAnexo := SUBSTR(rA.TextoAnexo, 1);
          END LOOP;
        END IF;  --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular
      END IF;
      --<I RTC 316449> / CBayona / 22-02-2023 / Se agrega nuevo par·metro
      IF rD.tipoRep = 'LQ' THEN
        SELECT MIN(cf.NumFinanc) NumFinanc,
          DECODE(MIN(cf.TipoFin), 'CON', pr.Busca_LVal('PRMIMPRE', 'TITPAGCON'),  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal --'PAGO UNICO FRACCIONADO',
            pr.Busca_LVal('PRMIMPRE', 'TITPAGFRA') || NVL(MIN(cf.NumFinanc), 0)) Modalidad,  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal --'PAGO FRACCIONADO N∫ '
          MIN(cf.FecCar) FecEmi,
          DECODE(MIN(r.CodMoneda), 'USD', pr.Busca_LVal('PRMIMPRE', 'MONUSD'), pr.Busca_LVal('PRMIMPRE', 'MONSOL')) Moneda,  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal --'DÛlares Americanos' / 'Soles'
          TO_CHAR(MIN(cf.PorcInteres), 'fm999') || '%' TCEA
        INTO nNumFinanc, cModalidad, dFecEmi, cMoneda, cTCEA
        FROM Cond_Financiamiento cf
        WHERE cf.IdePol = r.IdePol
          AND cf.NumOper = r.NumOper;
        --<I RTC 316449> / CBayona / 02-03-2023 / Se obtiene descripciÛn del Producto
        SELECT MAX(p.DescProd)
        INTO cDescProd
        FROM Producto p
        WHERE CodProd = r.CodProd;
        --<F RTC 316449>
        cNomRep := 'COB00164';
        --
        cCodFirma := acselx.pr_Gestion_Cobranza.fr_ImpreFirma(r.NumOper, nNumFinanc);
        --
        SELECT r.Direc || ' - ' || e.DescEstado || ' - ' || c.DescCiudad || ' - ' || m.DescMunicipio
        INTO cDirec
        FROM Estado e,
          Ciudad c,
          Municipio m
        WHERE e.CodPais = r.CodPais
          AND e.CodEstado = r.CodEstado
          AND c.CodPais = e.CodPais
          AND c.CodEstado = e.CodEstado
          AND c.CodCiudad = r.CodCiudad
          AND m.CodPais = c.CodPais
          AND m.CodEstado = c.CodEstado
          AND m.CodCiudad = c.CodCiudad
          AND m.CodMunicipio = r.CodMunicipio;
        --
        xXMLAux1 := NULL;
        FOR rG IN c_Giros(nNumFinanc) LOOP
          SELECT XMLCONCAT(xXMLAux1, XMLAGG(
            XMLELEMENT("documento",
              XMLFOREST(
                rG.NumGiro AS "numDoc",
                rG.NumAcre AS "docGen",
                rG.FecVct AS "fecVct",
                rG.MtoGiroForaneo AS "importe"))))
          INTO xXMLAux1
          FROM Dual;
        END LOOP;
      END IF;
      --<F RTC 316449>
      IF rD.tipoRep = 'LA' THEN
        SELECT MAX(o.CodInterLider),
          -MAX(o.SldoObligMoneda),
          MAX(DECODE(o.TipoDocPag, '9', NULL,
            (SELECT l.Descrip
            FROM LVal l
            WHERE l.TipoLVal = 'TIPODOCO'
              AND l.CodLVal = o.TipoDocPag)))
        INTO cCodInter, nMtoOper, cTipoDocPag
        FROM Obligacion o
        WHERE o.NumOper = r.NumOper;
        --
        SELECT MAX(p.DescProd)
        INTO cDescProd
        FROM Producto p
        WHERE CodProd = r.CodProd;
        --
        SELECT MAX(c.CodPlan), MAX(c.RevPlan)
        INTO cCodPlan, cRevPlan
        FROM Cert_Ramo c
        WHERE c.IdePol = r.IdePol;

        SELECT DescPlanProd
        INTO cDescPlan
        FROM Plan_Prod
        WHERE CodProd = r.CodProd
          AND CodPlan = cCodPlan
          AND RevPlan = cRevPlan;
        --
        SELECT CASE WHEN t.TipoTer = 'E' THEN t.NomTer ELSE t.ApeTer || ' ' || t.ApeMatTer || ' ' || t.NomTer END AS "nomInter",
          t.Direc, m.DescMunicipio
        INTO cNomInter, cDirecInter, cDistInter
        FROM Intermediario i,
          Tercero t,
          Municipio m
        WHERE i.CodInter = cCodInter
          AND t.TipoId = i.TipoId
          AND t.NumId = i.NumId
          AND t.DvId = i.DvId
          AND m.CodPais = t.CodPais
          AND m.CodEstado = t.CodEstado
          AND m.CodCiudad = t.CodCiudad
          AND m.CodMunicipio = r.CodMunicipio;
        --
        SELECT NVL(MAX(dr.DescRuta), pr.Busca_LVal('PRMIMPRE', 'NODEF'))  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal  --'NO DEFINIDO'
        INTO cRuta
        FROM Resp_Pago_Mov rp,
          Ruta r,
          Desc_Ruta dr
        WHERE rp.IdePol = rD.IdePol
          AND rp.NumOper = rD.NumOper
          AND r.CodRuta = rp.CodRuta
          AND dr.CodRuta = r.CodRuta;
        --
        SELECT MAX(m.DescMunicipio)
        INTO cDistrito
        FROM Municipio m
        WHERE m.CodPais = r.CodPais
          AND m.CodEstado = r.CodEstado
          AND m.CodCiudad = r.CodCiudad
          AND m.CodMunicipio = r.CodMunicipio;
      END IF;
      --
      IF rD.tipoRep = 'CD' THEN
        cNomRep := 'EMI00292';
        SELECT ec.FecIniVig, ec.FecFinVig, ec.MtoEndoso, ec.TextoBien,
          ec.CodCli, ec.NumId, ec.CodTexto
        INTO r.fecIniVig, r.fecFinVig, nMtoOper, cTextoBien,
          cCodCli, nNumId, cCodTexto
        FROM Endosatario_Cert ec,
          Mod_Endosatario_Cert mec
        WHERE ec.IdePol = mec.IdePol
          AND ec.NumCert = mec.NumCert
          AND ec.NumId = mec.NumId
          AND mec.IdeEndCert = ec.IdeEndCert
          AND mec.IdePol = rD.IdePol
          AND mec.NumOperBco = rD.numOperBco;
      END IF;
      --
      SELECT MAX(CodUsuario)
      INTO cCodUsuario
      FROM Operacion
      WHERE IdeOp = r.NumOper;
      --
      cCodAut := pr_Interfase_AX.CodAutorizador(r.IdePol, r.NumOper);
      --
      SELECT MAX(NumTramite)
      INTO cNumTramite
      FROM Poliza_Tramite
      WHERE IdePol = r.IdePol
        AND NumOper = r.NumOper
        AND ROWNUM < 2;
      --
      --Firma
      pr_Interfase_AX.Obtener_Firma(rD.IdePol, cNomRep, cCodFirma, cFirma); --<RTC 316449> / CBayona / 22-02-2023 / Se agrega nuevo par·metro
    ----
      --IF rD.tipoRep = 'DG' THEN --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular / Se comenta
      IF rD.tipoRep IN ('DG', 'DGC') THEN --<RTC 316747> / CBayona / 03-04-2023 / [Proy API Endoso] ImpresiÛn Vehicular
        SELECT XMLCONCAT(xXMLOut, XMLAGG(
          XMLELEMENT("endoso",
            XMLFOREST(
              rD.tipoRep AS "tipoRep",
              r.CodProd AS "codProd",
              cTitulo AS "descProducto",
              r.NumPol AS "numPol",
              r.FecIniVig AS "fecIniVig",
              r.FecFinVig AS "fecFinVig",
              r.CodCli AS "codCli",
              pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
              r.TipoOp AS "tipoOp",
              r.FecMov AS "fecMov",
              r.cFecMov AS "cFecMov",   --<RTC 316449> / CBayona / 28-02-2023 / Se usa columna de cursor
              r.MtoOper AS "mtoOper",
              CASE WHEN r.TipoTer = 'E' THEN r.NomTer ELSE r.ApeTer || ' ' || r.ApeMatTer || ' ' || r.NomTer END AS "nomCliente",
              r.TipoDoc AS "tipoDoc",
              r.NumIdDoc AS "numDoc",
              cAnexo AS "anexo",
              cLiquid AS "nroLiquidacion",
              r.FormaPago AS "formaPago",
              cTitConsPrima AS "etTituloConsPrima",
              cTotConsPrima AS "etTotalConsPrima",
              xXMLAux AS "secPrimas",
              nMtoOper AS "primaTotal",
              cNomInter AS "nomInter", --<RTC 316449> / CBayona / 22-02-2023 / Se agrega nueva columna
              nMtoComm AS "mtoComision",
              r.NumOper AS "numOper",
              cCodUsuario AS "codEmisor",
              cCodAut AS "codAutorizador",
              cNumTramite AS "numTramite",
              cFirma AS "firma"))))
        INTO xXMLOut
        FROM Dual;
      END IF;
      --
      IF rD.tipoRep = 'LA' THEN
        SELECT XMLCONCAT(xXMLOut, XMLAGG(
          XMLELEMENT("endoso",
            XMLFOREST(
              rD.tipoRep AS "tipoRep",
              r.CodProd AS "codProd",
              cDescProd AS "descProd",
              cDescPlan AS "descPlan",
              r.NumPol AS "numPol",
              r.FecIniVig AS "fecIniVig",
              r.FecFinVig AS "fecFinVig",
              pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
              r.TipoOp AS "tipoOp",
              r.FecMov AS "fecMov",
              r.cFecMov AS "cFecMov",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa columna de cursor
              cNomInter AS "nomInter",
              cDirecInter AS "direcInter",
              cDistInter AS "distInter",
              r.NumId AS "codCli",
              CASE WHEN r.TipoTer = 'E' THEN r.NomTer ELSE r.ApeTer || ' ' || r.ApeMatTer || ' ' || r.NomTer END AS "nomCliente",
              r.NumId AS "codAseg",
              CASE WHEN r.TipoTer = 'E' THEN r.NomTer ELSE r.ApeTer || ' ' || r.ApeMatTer || ' ' || r.NomTer END AS "nomAseg",
              r.TipoDoc AS "tipoDoc",
              r.NumIdDoc AS "numDoc",
              cLiquid AS "nroLiquidacion",
              r.Direc AS "direcCliente",
              cDistrito AS "distCliente",
              cRuta AS "Zona",
              nMtoOper AS "primaTotal",
              cTipoDocPag AS "tipoDocPago",
              r.NumOper AS "numOper",
              cCodUsuario AS "codEmisor",
              cCodAut AS "codAutorizador",
              cNumTramite AS "numTramite",
              r.CodOfiEmi AS "codOfiEmi",
              r.CodPol AS "codPol"))))
        INTO xXMLOut
        FROM Dual;
      END IF;
      --
      IF rD.tipoRep = 'CD' THEN
        SELECT LOWER(Correo)
        INTO cCorreo
        FROM Texto_Cesion_Correo
        WHERE CodTexto = cCodTexto
         AND StsCorreo = 'ACT'
         AND ROWNUM = 1
        ORDER BY Corr;
        --
        SELECT XMLCONCAT(xXMLOut, XMLAGG(
          XMLELEMENT("endoso",
            XMLFOREST(
              rD.tipoRep AS "tipoRep",
              r.CodProd AS "codProd",
              r.NumPol AS "numPol",
              r.FecIniVig AS "fecIniVig",
              r.FecFinVig AS "fecFinVig",
              r.CodCli AS "codCli",
              r.FecMov AS "fecMov",
              r.cFecMov  AS "cFecMov",
              CASE WHEN r.TipoTer = 'E' THEN r.NomTer ELSE r.NomTer || ' ' || r.ApeTer END AS "nomCliente",
              (SELECT CASE WHEN t.TipoTer = 'E' THEN t.NomTer ELSE t.ApeTer || ' ' || t.ApeMatTer || ' ' || t.NomTer END
               FROM Tercero t
               WHERE NumId = nNumId) AS "endosatario",
              (SELECT CASE WHEN t.TipoTer = 'E' THEN t.NomTer ELSE t.ApeTer || ' ' || t.ApeMatTer || ' ' || t.NomTer END
               FROM Tercero t,
                 Cliente c
               WHERE t.NumId = c.NumId
                 AND c.CodCli = cCodCli) AS "asegurado",
              cCorreo AS "correo",
              cTextoBien AS "textoEndoso",
              pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
              nMtoOper AS "mtoEndoso",
              r.NumOper AS "numOper",
              rD.NumOperBco AS "numOperBco",
              cCodUsuario AS "codEmisor",
              cCodAut AS "codAutorizador",
              cNumTramite AS "numTramite",
              cFirma AS "firma"))))
        INTO xXMLOut
        FROM Dual;
      END IF;
      --
      --<I RTC 316449> / CBayona / 22-02-2023 / Se agrega reporte LQ
      IF rD.tipoRep = 'LQ' THEN
        SELECT XMLCONCAT(xXMLOut, XMLAGG(
          XMLELEMENT("endoso",
            XMLFOREST(
              rD.tipoRep AS "tipoRep",
              r.CodProd AS "codProd",
              cDescProd AS "descProd",  --<RTC 316449> / CBayona / 02-03-2023 / Se agrega columna
              r.NumPol AS "numPol",
              pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa valores en LVal
              r.TipoOp AS "tipoOp",
              r.FecMov AS "fecMov",
              r.cFecMov AS "cFecMov",  --<RTC 316449> / CBayona / 28-02-2023 / Se usa columna de cursor
              cNomInter AS "nomInter",
              cDirecInter AS "direcInter",
              r.NumId AS "codCli",
              CASE WHEN r.TipoTer = 'E' THEN r.NomTer ELSE r.ApeTer || ' ' || r.ApeMatTer || ' ' || r.NomTer END AS "nomCliente",
              cDirec AS "direcCliente",
              r.TipoDoc AS "tipoDoc",
              r.NumIdDoc AS "numDoc",
              cLiquid AS "nroLiquidacion",
              cTCEA AS "TCEA",
              cModalidad AS "modalidad",
              cCodInter || ' ' || cNomInter AS "corredor",
              cMoneda AS "moneda",
              dFecEmi AS "fecEmi",
              xXMLAux AS "secPrimas",
              nMtoOper AS "primaTotal",
              cTipoDocPag AS "tipoDocPago",
              xXMLAux1 AS "secDocumentos",
              r.NumOper AS "numOper",
              cCodUsuario AS "codEmisor",
              cCodAut AS "codAutorizador",
              cNumTramite AS "numTramite",
              cFirma AS "firma"))))
        INTO xXMLOut
        FROM Dual;
      END IF;
      --<F RTC 316449>
    END LOOP;
  END LOOP;
  --
  SELECT XMLSERIALIZE(CONTENT DELETEXML(XMLELEMENT("reporteEndoso", xXMLOut), '//*[.="" or contains(.,"?")]') INDENT)
  INTO p_cXMLOut
  FROM Dual;
END Datos_Impresion;


/*------------------------------------------------------------------------
  Nombre     : Datos_Impresion
  PropÛsito  : Procedimiento que devuelve la informaciÛn necesaria para realizar la impresiÛn
  Referencias: <RTC-317178> Error en la obtenciÛn de datos para impresiÛn para los casos con varios responsables de pago no est· obteniendo todos los datos requeridos
  Par·metros :
                 p_cXMLIn          CLOB         Trama de par·metros
  Retorno    :   p_cXMLOut         CLOB         Trama resultante
                 p_cError          VARCHAR2     Error encontrado (OK = 0,OK / ERROR = CodError,DescError)

  Log de Cambios:
    Fecha         Autor               DescripciÛn
    13/06/2023    Christian Bayona    CreaciÛn
    17/08/2023    Christian Bayona    <DIGB2B03-2877> TREC Mejoras
    08/09/2023    Christian Bayona    <DIGB2B03-3505> [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
 -----------------------------------------------------------------------------*/
PROCEDURE Datos_Impresion(p_cXMLIn   IN CLOB,
                          --
                          p_cXMLOut  OUT CLOB,
                          p_cError   OUT VARCHAR2) IS
--
cCodPlan        Cert_Ramo.CodPlan%TYPE;
cRevPlan        Cert_Ramo.RevPlan%TYPE;
cTitulo         VARCHAR2(500);
cTitConsPrima   VARCHAR2(50);
cTotConsPrima   VARCHAR2(50);
cNomClt         VARCHAR2(250);
cNomAseg        VARCHAR2(250);  --<DIGB2B03-2877> / Christian Bayona / 31-08-2023 / TREC Mejoras
cTipoDoc        VARCHAR2(20);
cNumIdDoc       VARCHAR2(20);
cDirecClt       Tercero.Direc%TYPE;
nNumAcre        Acreencia.NumAcre%TYPE;
nNumOblig       Obligacion.NumOblig%TYPE;
nMtoOper        Acreencia.MtoAcreMoneda%TYPE;
nMtoComm        Det_Comi_Riesgo.MtoComiRiesgoMoneda%TYPE;
cNumTramite     Poliza_Tramite.NumTramite%TYPE;
cCodAut         Usuario.CodUsr%TYPE;
cCodUsuario     Operacion.CodUsuario%TYPE;
xXMLAux         XMLTYPE;
xXMLOut         XMLTYPE;
cAnexo          CLOB;
cAnexoCert      CLOB;
cTextoBien      Endosatario_Cert.TextoBien%TYPE;
cCodCli         Cliente.CodCli%TYPE;
nNumIdP         Tercero.NumId%TYPE;
nNumId          Tercero.NumId%TYPE;
cCodTexto       Endosatario_Cert.CodTexto%TYPE;
cCorreo         Texto_Cesion_Correo.Correo%TYPE;
nMtoPrimCom     Det_Acre.MtoDetAcreMoneda%TYPE;
cDistrito       Municipio.DescMunicipio%TYPE;
cCodInter       Obligacion.CodInterLider%TYPE;
cRuta           Desc_Ruta.DescRuta%TYPE;
cNomInter       VARCHAR2(250);
cNomEndosa      VARCHAR2(250);
cDirecInter     Tercero.Direc%TYPE;
cDistInter      Municipio.DescMunicipio%TYPE;
cTipoDocPag     Obligacion.TipoDocPag%TYPE;
cDescPlan       Plan_Prod.DescPlanProd%TYPE;
cDescProd       Producto.DescProd%TYPE;
cLiquid         VARCHAR2(20);
cNomRep         VARCHAR2(20);
cFirma          CLOB;
nNumFinanc      Cond_Financiamiento.NumFinanc%TYPE;
cCodFirma       VARCHAR2(2);
cDirec          VARCHAR2(300);
cTCEA           VARCHAR2(5);
cModalidad      VARCHAR2(150);
dFecEmi         DATE;
cMoneda         VARCHAR2(50);
xXMLAux1        XMLTYPE;
--
CURSOR c_ReporteEndoso IS
  SELECT tipoRep,
    idePol,
    numCert,
    numOper,
    numId,
    fecInicio,
    fecFin,
    numOperBco
  FROM XMLTABLE('/reporteEndoso/endoso' PASSING xmltype(p_cXMLIn)
    COLUMNS
      tipoRep PATH 'tipoRep',
      idePol PATH 'idePol',
      numCert PATH 'numCert',
      numOper PATH 'numOper',
      numId PATH 'numId',
      fecInicio DATE PATH 'fecInicio',
      fecFin DATE PATH 'fecFin',
      numOperBco PATH 'numOperBco');
--
CURSOR c_Impresion(nIdePol IN Poliza.IdePol%TYPE, nNumCert IN Certificado.NumCert%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE, dFecIni IN DATE, dFecFin IN DATE) IS
  SELECT p.IdePol,
    op.NumCert,
    p.CodProd,
    p.CodPol,
    p.CodOfiEmi,
    p.NumPol,
    p.FecIniVig,
    p.FecFinVig,
    p.CodCli,
    p.CodMoneda,
    op.NumOper,
    op.TipoOp,
    op.FecMov,
    TO_CHAR(op.FecMov, pr.Busca_LVal('PRMIMPRE', 'UBIFECMOV'), 'NLS_DATE_LANGUAGE=SPANISH') cFecMov,
    op.MtoOper,
    op.PoliFactura,
    pr.DescProd,
    'AL CONTADO' FormaPago
  FROM Producto pr,
    Oper_Pol op,
    Poliza p
  WHERE p.IdePol = nIdePol
    AND pr.CodProd = p.CodProd
    AND op.IdePol = p.IdePol
    AND ((nNumCert IS NULL AND op.NumCert IS NULL)
    OR (op.NumCert = NVL(nNumCert, op.NumCert)))
    AND op.NumOper = NVL(nNumOper, op.NumOper)
    AND op.FecMov >= NVL(dFecIni, op.FecMov)
    AND op.FecMov < NVL(dFecFin, op.FecMov + 1)
  ORDER BY op.NumOper;
--
CURSOR c_Anexo(nIdePol IN Poliza.IdePol%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE) IS
  SELECT mtap.IdeAnexo,
    mtap.NumMod,
    mtap.TextoAnexo,
    mtap.CodUsu
  FROM Mod_Texto_Anexo_Pol mtap
  WHERE mtap.IdePol = nIdePol
    AND mtap.NumOper = nNumOper;
--
CURSOR c_AnexoCert(nIdePol IN Poliza.IdePol%TYPE, nNumCert IN Certificado.NumCert%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE) IS
  SELECT mtac.IdeAnexo,
    mtac.NumMod,
    mtac.TextoAnexo,
    c.CodCli  --<DIGB2B03-2877> / Christian Bayona / 31-08-2023 / TREC Mejoras
  FROM Mod_Texto_Anexo_Cert mtac,
    Certificado c
  --<I DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
  /*WHERE mtac.IdePol = nIdePol
    AND mtac.NumCert = nNumCert
    AND mtac.NumOper = nNumOper
    --<I DIGB2B03-2877> / Christian Bayona / 31-08-2023 / TREC Mejoras
    AND c.IdePol = mtac.IdePol
    AND c.NumCert = mtac.NumCert;
    --<F DIGB2B03-2877>*/
  WHERE mtac.IdePol(+) = c.IdePol
    AND mtac.NumCert(+) = c.NumCert
    AND mtac.NumOper(+) = nNumOper
    AND c.IdePol = nIdePol
    AND c.NumCert = nNumCert;
  --<F DIGB2B03-3505>
--
CURSOR c_Financiamientos(nIdePol IN Poliza.IdePol%TYPE, nNumOper IN Oper_Pol.NumOper%TYPE, nNumId IN Tercero.NumId%TYPE DEFAULT NULL) IS
  SELECT NVL(SUM(dcr.MtoComiRiesgoMoneda), 0) MtoComm,
    MIN(gf.NumAcre) NumAcre,
    cf.NumFinanc NumFinanc,
    DECODE(cf.TipoFin, 'CON', pr.Busca_LVal('PRMIMPRE', 'TITPAGCON'),  --'PAGO UNICO FRACCIONADO',
      pr.Busca_LVal('PRMIMPRE', 'TITPAGFRA') || NVL(cf.NumFinanc, 0)) Modalidad,  --'PAGO FRACCIONADO N∫ '
    cf.FecCar FecEmi,
    DECODE(cf.CodMoneda, 'USD', pr.Busca_LVal('PRMIMPRE', 'MONUSD'), pr.Busca_LVal('PRMIMPRE', 'MONSOL')) Moneda,
    TO_CHAR(cf.PorcInteres, 'fm999') || '%' TCEA,
    cf.NumId
  FROM Cond_Financiamiento cf,
    Giros_Financiamiento gf,
    Det_Comi_Riesgo dcr
  WHERE cf.IdePol = nIdePol
    AND cf.NumOper = nNumOper
    AND cf.NumId = NVL(nNumId, cf.NumId)
    AND gf.NumFinanc = cf.NumFinanc
    AND dcr.NumAcre(+) = gf.NumAcre
  GROUP BY cf.NumFinanc, cf.TipoFin, cf.FecCar, cf.CodMoneda, cf.PorcInteres, cf.NumId;
--
CURSOR c_DetAcreencia(nNumAcre IN Acreencia.NumAcre%TYPE) IS
  SELECT ca.DescCptoAcre,
    da.NumDetAcre,
    da.CodMoneda,
    da.MtoDetAcreMoneda
  FROM Cpto_Acre ca,
    Det_Acre da
  WHERE da.NumAcre = nNumAcre
    AND ca.CodGrupoAcre = da.CodGrupoAcre
    AND ca.CodCptoAcre = da.CodCptoAcre
  ORDER BY da.NumDetAcre;
--
CURSOR c_Giros(nNumFinanc IN Cond_Financiamiento.NumFinanc%TYPE) IS
  SELECT gf.NumGiro,
    gf.NumAcre,
    gf.FecVct,
    gf.MtoGiroForaneo
  FROM Giros_Financiamiento gf
  WHERE NumFinanc = nNumFinanc;
--
CURSOR c_Tercero(nNumId IN Tercero.NumId%TYPE) IS
  SELECT t.TipoTer,
    t.NomTer,
    t.ApeTer,
    t.ApeMatTer,
    t.CodPais,
    t.CodEstado,
    e.DescEstado Departamento,
    t.CodCiudad,
    c.DescCiudad Provincia,
    t.CodMunicipio,
    m.DescMunicipio Distrito,
    t.Direc,
    CASE WHEN TipoTer = 'E' THEN
      t.NomTer
    ELSE
      t.ApeTer || ' ' || t.ApeMatTer || ' ' || t.NomTer
    END AS "nomTer",
    l.Descrip TipoDoc,
    di.NumIdDoc
  FROM LVal l,
    Docs_Identidad di,
    Tercero t,
    Estado e,
    Ciudad c,
    Municipio m
  WHERE t.TipoId = '0'
    AND t.NumId = nNumId
    AND t.DvId = '0'
    AND di.NumId = t.NumId
    AND l.TipoLVal = 'TIPODOC'
    AND l.CodLVal = di.TipoIdDoc
    AND e.CodPais = t.CodPais
    AND e.CodEstado = t.CodEstado
    AND c.CodPais = e.CodPais
    AND c.CodEstado = e.CodEstado
    AND c.CodCiudad = t.CodCiudad
    AND m.CodPais = c.CodPais
    AND m.CodEstado = c.CodEstado
    AND m.CodCiudad = c.CodCiudad
    AND m.CodMunicipio = t.CodMunicipio;
--
CURSOR c_Obligaciones(nNumOper IN Oper_Pol.NumOper%TYPE, nNumId IN Tercero.NumId%TYPE DEFAULT NULL) IS
  SELECT o.NumId,
    o.NumOblig,
    -o.SldoObligMoneda MtoOper,
    o.CodInterLider,
    DECODE(o.TipoDocPag, '9', NULL,
      (SELECT MAX(l.Descrip)
      FROM LVal l
      WHERE l.TipoLVal = 'TIPODOCO'
        AND l.CodLVal = o.TipoDocPag)) AS cTipoDocPag
  FROM Obligacion o
  WHERE o.NumOper = nNumOper
    AND o.NumId = NVL(nNumId, o.NumId);
--
CURSOR c_DetObligacion(nNumOblig IN Obligacion.NumOblig%TYPE) IS
  SELECT ce.DescCptoEgre,
    d.NumDetOblig,
    d.CodMoneda,
    d.MtoDetObligMoneda
  FROM Det_Oblig d,
    Cpto_Egre ce
  WHERE d.NumOblig = nNumOblig
    AND ce.CodClaEgre = d.CodClaEgre
    AND ce.CodCptoEgre = d.CodCptoEgre
  ORDER BY d.NumDetOblig;
--
CURSOR c_Cesion(nIdePol IN Poliza.IdePol%TYPE, nNumOper IN Endosatario_Cert.NumOper%TYPE DEFAULT NULL,
  nNumOperBCO IN Mod_Endosatario_Cert.NumOperBco%TYPE DEFAULT NULL) IS
  SELECT ec.FecIniVig, ec.FecFinVig, ec.MtoEndoso, ec.TextoBien,
    ec.CodCli, ec.NumId, ec.CodTexto, ec.NumOper, mec.NumOperBco
  FROM Endosatario_Cert ec,
    Mod_Endosatario_Cert mec
  WHERE ec.IdePol = mec.IdePol
    AND ec.NumOper = NVL(nNumOper, ec.NumOper)
    AND ec.NumCert = mec.NumCert
    AND ec.NumId = mec.NumId
    AND mec.IdeEndCert = ec.IdeEndCert
    AND mec.IdePol = nIdePol
    AND mec.NumOperBco = NVL(nNumOperBCO, mec.NumOperBco);
--
BEGIN
  p_cError := '1,No se encontraron datos';
  FOR rD IN c_ReporteEndoso LOOP
    rD.tipoRep := NVL(rD.tipoRep, 'DG');
    --rD.NumCert := NVL(rD.NumCert, 1);  --<DIGB2B03-2877> / Christian Bayona / 31-08-2023 / TREC Mejoras --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
    --Obtener Plan de la pÛliza
    SELECT MAX(CodPlan), MAX(RevPlan)
    INTO cCodPlan, cRevPlan
    FROM Cert_Ramo c
    WHERE c.IdePol = rD.IdePol;
    --
    FOR r IN c_Impresion(rD.IdePol, rD.NumCert, rD.NumOper, rD.FecInicio, rD.FecFin) LOOP
      p_cError := '0,OK';
      --
      SELECT MAX(DescPlanProd)
      INTO cDescPlan
      FROM Plan_Prod
      WHERE CodProd = r.CodProd
        AND CodPlan = cCodPlan
        AND RevPlan = cRevPlan;
      --
      SELECT MAX(CodUsuario)
      INTO cCodUsuario
      FROM Operacion
      WHERE IdeOp = r.NumOper;
      --
      cCodAut := pr_Interfase_AX.CodAutorizador(r.IdePol, r.NumOper);
      --
      SELECT MAX(NumTramite)
      INTO cNumTramite
      FROM Poliza_Tramite
      WHERE IdePol = r.IdePol
        AND NumOper = r.NumOper
        AND ROWNUM < 2;
      --
      IF rD.tipoRep IN ('DG', 'DGC', 'LA', 'LQ') THEN
        --Desc Producto
        cTitulo := pr_Plan_Prod.Titulo_Prod_Plan(r.CodProd, cCodPlan, cRevPlan ,'POLIZA','ENCABEZADO') ;
        --Se obtiene descripciÛn del producto
        IF rD.tipoRep = 'DGC' OR cTitulo IS NULL THEN
          cTitulo := r.DescProd;
        END IF;
        --
        cTitConsPrima := pr.Busca_LVal('PRMIMPRE', 'TITCPRIM'); --'CONSOLIDADO DE PRIMA';
        cTotConsPrima := pr.Busca_LVal('PRMIMPRE', 'TOTCPRIM'); --'PRIMA TOTAL';
        --
        xXMLAux := NULL;
        --
        IF r.MtoOper > 0 THEN --Acreencia
          cNomRep := 'EMI00173';
          --
          FOR rF IN c_Financiamientos(r.IdePol, r.NumOper, rD.NumId) LOOP
            xXMLAux := NULL;
            nMtoComm := rF.MtoComm;
			      --
            SELECT f.NumFact,
              CodInter
            INTO cLiquid, cCodInter
            FROM Factura f
            WHERE NumOper = rD.NumOper
              AND NumId = rF.NumId;
            --
            SELECT NumId
            INTO nNumId
            FROM Intermediario
            WHERE CodInter = cCodInter;
            --
            FOR rT IN c_Tercero(nNumId) LOOP
              cNomInter := rT.nomTer;
              cDirecInter := rT.Direc;
              cDistInter := rT.Distrito;
            END LOOP;
            --
            nMtoPrimCom := 0;
            nMtoOper := 0;
            FOR rA IN c_DetAcreencia(rF.NumAcre) LOOP
              nMtoOper := nMtoOper + rA.MtoDetAcreMoneda;
              --
              IF rA.DescCptoAcre = 'PRIMA' THEN
                rA.DescCptoAcre := pr.Busca_LVal('PRMIMPRE', 'CPRIMNET'); --'PRIMA NETA'
              END IF;
              --
              IF rA.DescCptoAcre = 'I.G.V.' THEN
                SELECT XMLCONCAT(xXMLAux, XMLAGG(
                  XMLELEMENT("linea",
                    XMLFOREST(
                      rA.NumDetAcre AS "orden",
                      pr.Busca_LVal('PRMIMPRE', 'PRIMCOM') AS "concepto",  --'PRIMA COMERCIAL'
                      pr.Busca_LVal('SIMBMON', rA.CodMoneda) AS "moneda",
                      nMtoPrimCom AS "monto"))))
                INTO xXMLAux
                FROM Dual;
                --
                nMtoPrimCom := 0;
                rA.NumDetAcre := rA.NumDetAcre + 1;

              ELSE
                nMtoPrimCom := nMtoPrimCom + rA.MtoDetAcreMoneda;
              END IF;
              --
              SELECT XMLCONCAT(xXMLAux, XMLAGG(
                XMLELEMENT("linea",
                  XMLFOREST(
                    rA.NumDetAcre AS "orden",
                    rA.DescCptoAcre AS "concepto",
                    pr.Busca_LVal('SIMBMON', rA.CodMoneda) AS "moneda",
                    rA.MtoDetAcreMoneda AS "monto"))))
              INTO xXMLAux
              FROM Dual;
            END LOOP;
            --
            cCodFirma := acselx.pr_Gestion_Cobranza.fr_ImpreFirma(r.NumOper, rF.NumFinanc);
            --
            xXMLAux1 := NULL;
            FOR rG IN c_Giros(rF.NumFinanc) LOOP
              SELECT XMLCONCAT(xXMLAux1, XMLAGG(
                XMLELEMENT("documento",
                  XMLFOREST(
                    rG.NumGiro AS "numDoc",
                    rG.NumAcre AS "docGen",
                    rG.FecVct AS "fecVct",
                    rG.MtoGiroForaneo AS "importe"))))
              INTO xXMLAux1
              FROM Dual;
            END LOOP;

            FOR rT IN c_Tercero(rF.NumId) LOOP
              cNomClt := rT.nomTer;
              cDirec := rT.Direc;
              cTipoDoc := rT.TipoDoc;
              cNumIdDoc := rT.NumIdDoc;
            END LOOP;
            --
            IF rD.tipoRep = 'LQ' THEN
              DBMS_OUTPUT.Put_Line('>>> Reporte INI: ' || rD.tipoRep || ' No1: ' || c_ReporteEndoso%ROWCOUNT || ' - No2: ' || c_Impresion%ROWCOUNT);
              cNomRep := 'COB00164';
              --
              SELECT XMLCONCAT(xXMLOut, XMLAGG(
                XMLELEMENT("endoso",
                  XMLFOREST(
                    rD.tipoRep AS "tipoRep",
                    r.CodProd AS "codProd",
                    r.DescProd AS "descProd",
                    r.NumPol AS "numPol",
                    r.NumCert AS "numCert",
                    r.FecIniVig AS "fecIniVig",
                    r.FecFinVig AS "fecFinVig",
                    pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",
                    r.TipoOp AS "tipoOp",
                    r.FecMov AS "fecMov",
                    r.cFecMov AS "cFecMov",
                    cNomInter AS "nomInter",
                    cDirecInter AS "direcInter",
                    rF.NumId AS "codCli",
                    cNomClt AS "nomCliente",
                    cDirec AS "direcCliente",
                    cTipoDoc AS "tipoDoc",
                    cNumIdDoc AS "numDoc",
                    cLiquid AS "nroLiquidacion",
                    rF.TCEA AS "TCEA",
                    rF.Modalidad AS "modalidad",
                    cCodInter || ' ' || cNomInter AS "corredor",
                    rF.Moneda AS "moneda",
                    rF.FecEmi AS "fecEmi",
                    xXMLAux AS "secPrimas",
                    nMtoOper AS "primaTotal",
                    cTipoDocPag AS "tipoDocPago",
                    xXMLAux1 AS "secDocumentos",
                    r.NumOper AS "numOper",
                    cCodUsuario AS "codEmisor",
                    cCodAut AS "codAutorizador",
                    cNumTramite AS "numTramite"))))
              INTO xXMLOut
              FROM Dual;
            END IF;
          END LOOP;

        ELSIF r.MtoOper < 0 THEN --ObligaciÛn
          cTitConsPrima := pr.Busca_LVal('PRMIMPRE', 'TOTCPRIMD'); --'CONSOLIDADO DE PRIMA A DEVOLVER';
          cTotConsPrima := pr.Busca_LVal('PRMIMPRE', 'TOTCPRIMD'); --'PRIMA TOTAL A DEVOLVER';
          --
          cNomRep := 'EMI00287';
          --<I DIGB2B03-2877> / Christian Bayona / 17-08-2023 / TREC Mejoras
          SELECT SUM(MtoComMoneda)
          INTO nMtoComm
          FROM Recibo
          WHERE IdePol = r.idePol
            AND IdeOp = r.numOper;
          --<F DIGB2B03-2877>
          FOR rO IN c_Obligaciones(rD.NumOper, rD.NumId) LOOP
            cLiquid := TO_CHAR(rO.NumOblig);
            xXMLAux := NULL;
            nMtoOper := rO.MtoOper;
            --
            FOR rDO IN c_DetObligacion(rO.NumOblig) LOOP
              SELECT XMLCONCAT(xXMLAux, XMLAGG(
                XMLELEMENT("linea",
                  XMLFOREST(
                    rDO.NumDetOblig AS "orden",
                    rDO.DescCptoEgre AS "concepto",
                    pr.Busca_LVal('SIMBMON', rDO.CodMoneda) AS "moneda",
                    rDO.MtoDetObligMoneda AS "monto"))))
              INTO xXMLAux
              FROM Dual;
            END LOOP;
            --
            SELECT NumId
            INTO nNumId
            FROM Intermediario
            WHERE CodInter = rO.CodInterLider;
            --
            FOR rT IN c_Tercero(nNumId) LOOP
              cNomInter := rT.nomTer;
              cDirecInter := rT.Direc;
              cDistInter := rT.Distrito;
            END LOOP;
            --
            SELECT NVL(MAX(dr.DescRuta), pr.Busca_LVal('PRMIMPRE', 'NODEF'))  --'NO DEFINIDO'
            INTO cRuta
            FROM Resp_Pago_Mov rp,
              Ruta r,
              Desc_Ruta dr
            WHERE rp.IdePol = rD.IdePol
              AND rp.NumOper = rD.NumOper
              AND r.CodRuta = rp.CodRuta
              AND dr.CodRuta = r.CodRuta;
            --
            FOR rT IN c_Tercero(rO.NumId) LOOP
              cNomClt := rT.NomTer;
              cTipoDoc := rT.TipoDoc;
              cNumIdDoc := rT.NumIdDoc;
              cDirecClt := rT.Direc;
              cDistrito := rT.Distrito;
            END LOOP;
            --
            IF rD.tipoRep = 'LA' THEN
              DBMS_OUTPUT.Put_Line('>>> Reporte INI: ' || rD.tipoRep || ' No1: ' || c_ReporteEndoso%ROWCOUNT || ' - No2: ' || c_Impresion%ROWCOUNT);
              SELECT XMLCONCAT(xXMLOut, XMLAGG(
                XMLELEMENT("endoso",
                  XMLFOREST(
                    rD.tipoRep AS "tipoRep",
                    r.CodProd AS "codProd",
                    --cDescProd AS "descProd", --<DIGB2B03-2877> / Christian Bayona / 17-08-2023 / TREC Mejoras
                    r.DescProd AS "descProd", --<DIGB2B03-2877> / Christian Bayona / 17-08-2023 / TREC Mejoras
                    cDescPlan AS "descPlan",
                    r.NumPol AS "numPol",
                    r.FecIniVig AS "fecIniVig",
                    r.FecFinVig AS "fecFinVig",
                    pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",
                    r.TipoOp AS "tipoOp",
                    r.FecMov AS "fecMov",
                    r.cFecMov AS "cFecMov",
                    cNomInter AS "nomInter",
                    cDirecInter AS "direcInter",
                    cDistInter AS "distInter",
                    rO.NumId AS "codCli",
                    cNomClt AS "nomCliente",
                    rO.NumId AS "codAseg",
                    cNomClt AS "nomAseg",
                    cTipoDoc AS "tipoDoc",
                    cNumIdDoc AS "numDoc",
                    cLiquid AS "nroLiquidacion",
                    cDirecClt AS "direcCliente",
                    cDistrito AS "distCliente",
                    cRuta AS "Zona",
                    nMtoOper AS "primaTotal",
                    cTipoDocPag AS "tipoDocPago",
                    r.NumOper AS "numOper",
                    cCodUsuario AS "codEmisor",
                    cCodAut AS "codAutorizador",
                    cNumTramite AS "numTramite",
                    r.CodOfiEmi AS "codOfiEmi",
                    r.CodPol AS "codPol"))))
              INTO xXMLOut
              FROM Dual;
            END IF;
          END LOOP;

        ELSE --ESV
          nMtoOper := 0.00;
          nMtoComm := 0.00;
          cLiquid := '0';
          cNomRep := 'EMI00287';
          --
          SELECT XMLAGG(
            XMLELEMENT("linea",
              XMLFOREST(
                ROWNUM AS "orden",
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "moneda",
                l.Descrip AS "concepto",
                0 AS "monto")))
          INTO xXMLAux
          FROM LVal l
          WHERE l.TipoLVal = 'CONSPRIM';
          --
          SELECT XMLCONCAT(xXMLAux, XMLAGG(
            XMLELEMENT("linea",
              XMLFOREST(
                3 AS "orden",
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "moneda",
                pr.Busca_LVal('PRMIMPRE', 'TOTCPRIM') AS "concepto",  --'PRIMA COMERCIAL'
                0 AS "monto"))))
          INTO xXMLAux
          FROM Dual;
          --
          SELECT XMLCONCAT(xXMLAux, XMLAGG(
            XMLELEMENT("linea",
              XMLFOREST(
                4 AS "orden",
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "moneda",
                'I.G.V.' AS "concepto",
                0 AS "monto"))))
          INTO xXMLAux
          FROM Dual;
          --
          FOR rT IN c_Tercero(r.CodCli) LOOP
            cNomClt := rT.nomTer;
            cTipoDoc := rT.TipoDoc;
            cNumIdDoc := rT.NumIdDoc;
          END LOOP;
        END IF;
        --Obtenemos el texto anexo de la pÛliza
        FOR rA IN c_Anexo(r.IdePol, r.NumOper) LOOP
          IF LENGTH(rA.TextoAnexo) <= 4000 THEN  --<DIGB2B03-3505> / Christian Bayona / 13-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
            cAnexo := SUBSTR(rA.TextoAnexo, 1);
          END IF;  --<DIGB2B03-3505> / Christian Bayona / 13-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
        END LOOP;
        --Se obtiene el texto anexo del Certificado
        IF rD.tipoRep = 'DGC' THEN
          FOR rA IN c_AnexoCert(r.IdePol, rD.NumCert, r.NumOper) LOOP
            --cAnexoCert := SUBSTR(rA.TextoAnexo, 1);  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
            --<I DIGB2B03-2877> / Christian Bayona / 31-08-2023 / TREC Mejoras
            nNumIdP := rA.CodCli;
            --
            FOR rT IN c_Tercero(rA.CodCli) LOOP
              cNomAseg := rT.nomTer;
            END LOOP;
            --<F DIGB2B03-2877>

            --<I DIGB2B03-3505> / Christian Bayona / 13-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
            IF LENGTH(rA.TextoAnexo) <= 4000 THEN
              cAnexoCert := SUBSTR(rA.TextoAnexo, 1);
            END IF;
            --<F DIGB2B03-3505>
          END LOOP;
        END IF;

        --<DIGB2B03-2877> / Christian Bayona / 29-08-2023 / TREC Mejoras
        SELECT MAX(NumId)
        INTO nNumId
        FROM Intermediacion_Pol
        WHERE IdePol = r.IdePol
          AND IndLider = 'S'
          AND StsInter = 'ACT';
        --
        FOR rT IN c_Tercero(nNumId) LOOP
          cNomInter := rT.nomTer;
        END LOOP;
        --<F DIGB2B03-2877>
      END IF;
      --
      IF rD.tipoRep = 'CD' THEN
        cNomRep := 'EMI00292';
        --
        FOR rC IN c_Cesion(rD.IdePol, rD.NumOper, rD.NumOperBco) LOOP
          BEGIN  --<DIGB2B03-2877> / Christian Bayona / 17-08-2023 / TREC Mejoras
            SELECT LOWER(Correo)
            INTO cCorreo
            FROM Texto_Cesion_Correo
            WHERE CodTexto = rC.CodTexto
             AND StsCorreo = 'ACT'
             AND ROWNUM = 1
            ORDER BY Corr;
          --<I DIGB2B03-2877> / Christian Bayona / 17-08-2023 / TREC Mejoras
          EXCEPTION
            WHEN OTHERS THEN
              cCorreo := '';
          END;
          --<F DIGB2B03-2877>
          --
          FOR rT IN c_Tercero(r.CodCli) LOOP
            cNomClt := rT.NomTer;
          END LOOP;
          --
          FOR rT IN c_Tercero(rC.NumId) LOOP
            cNomEndosa := rT.NomTer;
          END LOOP;
          --
          DBMS_OUTPUT.Put_Line('>>> Reporte INI: ' || rD.tipoRep || ' No1: ' || c_ReporteEndoso%ROWCOUNT || ' - No2: ' || c_Impresion%ROWCOUNT);
          cAnexo := SUBSTR(rC.textoBien, 1);  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
          --
          SELECT XMLCONCAT(xXMLOut, XMLAGG(
            XMLELEMENT("endoso",
              XMLFOREST(
                rD.tipoRep AS "tipoRep",
                r.CodProd AS "codProd",
                r.NumPol AS "numPol",
                r.NumCert AS "numCert",
                rC.FecIniVig AS "fecIniVig",
                rC.FecFinVig AS "fecFinVig",
                r.CodCli AS "codCli",
                r.FecMov AS "fecMov",
                r.cFecMov  AS "cFecMov",
                cNomClt AS "nomCliente",
                cNomEndosa AS "endosatario",
                cNomClt AS "asegurado",
                cCorreo AS "correo",
                --rC.TextoBien AS "textoEndoso",  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
                cAnexo AS "textoEndoso",  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
                pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",
                rC.MtoEndoso AS "mtoEndoso",
                rC.NumOper AS "numOper",
                rC.NumOperBco AS "numOperBco",
                cCodUsuario AS "codEmisor",
                cCodAut AS "codAutorizador",
                cNumTramite AS "numTramite"))))
          INTO xXMLOut
          FROM Dual;

        END LOOP;
      END IF;
      --
      IF rD.tipoRep IN ('DG', 'DGC') THEN
        DBMS_OUTPUT.Put_Line('>>> Reporte INI: ' || rD.tipoRep || ' No1: ' || c_ReporteEndoso%ROWCOUNT || ' - No2: ' || c_Impresion%ROWCOUNT);
        --
        SELECT XMLCONCAT(xXMLOut, XMLAGG(
          XMLELEMENT("endoso",
            XMLFOREST(
              rD.tipoRep AS "tipoRep",
              r.CodProd AS "codProd",
              cTitulo AS "descProducto",
              r.NumPol AS "numPol",
              r.NumCert AS "numCert",
              r.FecIniVig AS "fecIniVig",
              r.FecFinVig AS "fecFinVig",
              r.CodCli AS "codCli",
              pr.Busca_LVal('SIMBMON', r.CodMoneda) AS "codMoneda",
              r.TipoOp AS "tipoOp",
              r.FecMov AS "fecMov",
              r.cFecMov AS "cFecMov",
              r.MtoOper AS "mtoOper",
              cNomClt AS "nomCliente",
              --<I DIGB2B03-2877> / Christian Bayona / 31-08-2023 / TREC Mejoras
              nNumIdP AS "codAseg",
              cNomAseg AS "nomAseg",
              --<F DIGB2B03-2877>
              cTipoDoc AS "tipoDoc",
              cNumIdDoc AS "numDoc",
              cAnexo AS "anexo",
              cAnexoCert AS "anexoCert",
              cLiquid AS "nroLiquidacion",
              r.FormaPago AS "formaPago",
              cTitConsPrima AS "etTituloConsPrima",
              cTotConsPrima AS "etTotalConsPrima",
              xXMLAux AS "secPrimas",
              nMtoOper AS "primaTotal",
              cNomInter AS "nomInter",
              nMtoComm AS "mtoComision",
              r.NumOper AS "numOper",
              cCodUsuario AS "codEmisor",
              cCodAut AS "codAutorizador",
              cNumTramite AS "numTramite"))))
        INTO xXMLOut
        FROM Dual;
      END IF;
      --Para obligaciones existen casos que se repiten tanto en oper_pol como obligacion
      --IF rD.tipoRep = 'LA' AND r.NumCert IS NOT NULL THEN --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
      IF (rD.tipoRep = 'LA' AND r.NumCert IS NOT NULL) OR (rD.tipoRep = 'CD' AND rD.NumCert IS NULL) THEN  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
        EXIT;
      END IF;
    END LOOP c_Impresion;
    --Firma
    pr_Interfase_AX.Obtener_Firma(rD.IdePol, cNomRep, cCodFirma, cFirma);
  END LOOP c_ReporteEndoso;
  --
  SELECT XMLSERIALIZE(CONTENT DELETEXML(XMLELEMENT("reporteEndoso",
    --XMLELEMENT("firma", cFirma), xXMLOut), '//*[.="" or contains(.,"?")]') INDENT)  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
    XMLELEMENT("firma", cFirma), xXMLOut), '//*[.=""]') INDENT)  --<DIGB2B03-3505> / Christian Bayona / 08-09-2023 / [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
  INTO p_cXMLOut
  FROM Dual;

EXCEPTION
  WHEN OTHERS THEN
    p_cError := SQLCODE || ',' || SQLERRM;
END Datos_Impresion;


/*-----------------------------------------------------------------------------
  Nombre      : maquinarias_trec
  Proposito   : Procedimiento para registrar los datos maquinarias
  Referencias : <DIGB2B03-2877> Mejora en Registro y ValidaciÛn de Maquinaria TREC
  Parametros  :
                p_cTag             VARCHAR2   Etiqueta XML principal del Endoso
                p_cXMLInput        XML (CLob) con los par·metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER     Identificador de PÛliza
                p_nNumCert         NUMBER     N˙mero de Certificado
                p_cCodUsr          VARCHAR2   CÛdigo de Usuario
                p_cProces          VARCHAR2   Tipo de operaciÛn (I - InclusiÛn, E - ExclusiÛn, M-ModiciÛn)
                p_cError           VARCHAR2   Resultado de operaciÛn
  Log de Cambios
    Fecha         Autor                    DescripciÛn
    11/08/2023    Manuel Navarro           Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE maquinarias_trec(p_cTag IN VARCHAR2,
                           p_cXMLInput IN CLOB,
                           p_nIdePol IN NUMBER,
                           p_nNumCert IN NUMBER,
                           p_cCodUsr IN VARCHAR2,
                           p_cProcess IN VARCHAR2,
                           p_cError OUT  VARCHAR2
                           ) IS
--
CURSOR c_datosMaquinariasList(cTag IN VARCHAR2) IS
  SELECT
    codserie,
    nummotor,
    numplaca,
    nummaqui,
    codmarca,
    codmodelo,
    annofabr,
    mtomaqui,
    fecini,
    ubicacion,
    numoper,
    idemaquinaria
  FROM XMLTABLE(REPLACE('/*/datosMaquinariasList/datosMaquinarias', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codserie    PATH 'codSerie',
      nummotor    PATH 'numMotor',
      numplaca    PATH 'numPlaca',
      nummaqui    PATH 'numMaqui',
      codmarca    PATH 'codMarca',
      codmodelo   PATH 'codModelo',
      annofabr    PATH 'annoFabr',
      mtomaqui    PATH 'mtoMaqui',
      fecini      Date    PATH 'fecIni',
      numoper     PATH 'numOper',
      idemaquinaria PATH 'ideMaquinaria',
      ubicacion   PATH 'ubicacion'
      );

CURSOR cMod_datosMaquinariasList(cTag IN VARCHAR2) IS
  SELECT
    codserie,
    nummotor,
    numplaca,
    nummaqui,
    codmarca,
    codmodelo,
    annofabr,
    mtomaqui,
    fecini,
    numoper,
    idemaquinaria,
    ubicacion,
    codserie_new,
    nummotor_new,
    numplaca_new,
    nummaqui_new,
    codmarca_new,
    codmodelo_new,
    annofabr_new,
    mtomaqui_new,
    fecini_new,
    ubicacion_new
  FROM XMLTABLE(REPLACE('/*/datosMaquinariasList/datosMaquinarias', '*', cTag) PASSING xmltype(p_cXMLInput)
    COLUMNS
      codserie    PATH 'codSerie',
      nummotor   PATH 'numMotor',
      numplaca   PATH 'numPlaca',
      nummaqui   PATH 'numMaqui',
      codmarca   PATH 'codMarca',
      codmodelo   PATH 'codModelo',
      annofabr    PATH 'annoFabr',
      mtomaqui    PATH 'mtoMaqui',
      fecini  Date PATH 'fecIni',
      numoper     PATH 'numOper',
      idemaquinaria PATH 'ideMaquinaria',
      ubicacion PATH 'ubicacion',
      codserie_new    PATH 'codSerie_New',
      nummotor_new    PATH 'numMotor_New',
      numplaca_new    PATH 'numPlaca_New',
      nummaqui_new    PATH 'numMaqui_New',
      codmarca_new    PATH 'codMarca_New',
      codmodelo_new   PATH 'codModelo_New',
      annofabr_new    PATH 'annoFabr_New',
      mtomaqui_new    PATH 'mtoMaqui_New',
      fecini_new  Date PATH 'fecIni_New',
      ubicacion_new PATH 'ubicacion_New'
      );
--
tDatosMaquinarias  TDU_TABLA_MAQUINARIA_TREC;
--
BEGIN
  p_cerror := '0,OK';
  IF p_cProcess IN ('I','E') THEN
    tDatosMaquinarias := TDU_TABLA_MAQUINARIA_TREC();
    FOR r IN c_datosMaquinariasList(p_cTag) LOOP
        tDatosMaquinarias.Extend;

        tDatosMaquinarias(tDatosMaquinarias.LAST) := TDU_MAQUINARIA_TREC(r.codserie,r.nummotor, r.numplaca, r.nummaqui, null, r.codmarca, null, r.codmodelo, r.annofabr, r.mtomaqui, r.ubicacion, r.fecini, r.numoper, r.idemaquinaria,
                              null, null, null, null, null, null, null, null, null, null, null, null);
    END LOOP;
    --
    IF tDatosMaquinarias.COUNT > 0 THEN
      acselx.pr_Interfase_AX.inc_exc_maquinaria_trec(p_nIdePol, p_nNumCert, tDatosMaquinarias, p_cCodUsr, p_cProcess, p_cError); --agregar ax manuel
    END IF;
  END IF;
  --
  IF p_cProcess IN ('M') THEN
    tDatosMaquinarias := TDU_TABLA_MAQUINARIA_TREC();
    FOR r IN cMod_datosMaquinariasList(p_cTag) LOOP
        tDatosMaquinarias.Extend;

        tDatosMaquinarias(tDatosMaquinarias.LAST) :=TDU_MAQUINARIA_TREC(r.codserie, r.nummotor, r.numplaca, r.nummaqui, null, r.codmarca, null, r.codmodelo, r.annofabr, r.mtomaqui, r.ubicacion, r.fecini, r.numoper, r.idemaquinaria,
                            r.codserie_new, r.nummotor_new, r.numplaca_new, r.nummaqui_new, null, r.codmarca_new, null, r.codmodelo_new, r.annofabr_new, r.mtomaqui_new, r.ubicacion_new, r.fecini_new);

    END LOOP;
    --
    IF tDatosMaquinarias.COUNT > 0 THEN
      acselx.pr_Interfase_AX.mod_maquinaria_trec(p_nIdePol, p_nNumCert, tDatosMaquinarias, p_cCodUsr, p_cProcess, p_cError); --ahgregar ax
    END IF;
  END IF;
END maquinarias_trec;

/*-----------------------------------------------------------------------------
  Nombre      : Genera_Movimiento_Prima
  Proposito   : Procedimiento para generar un cambio de prima y poner al estado INC
  Referencias : <DIGB2B03-3505> [PROYECTO AUTOMATIZACION DE EMISION Y ENDOSOS] CONDICION ESPECIAL CON PRIMA - LEGACY
  Parametros  :
                p_IdePol           Ide de la pÛliza
                p_NumCert          N˙mero de certificado
                p_Inc              Flag para Incluir(1) o no (0)
  Log de Cambios
    Fecha         Autor                    DescripciÛn
    12/09/2023    Edgard LÛpez             Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE Genera_Movimiento_Prima(p_IdePol certificado.idepol%type, p_NumCert certificado.numcert%type, p_Inc INTEGER) IS

  rMod_cobert       MOD_COBERT%rowtype;
  cCodRamoCert      COBERT_CERT.Codcobert%type;
  cCodCobert        COBERT_CERT.Codcobert%type;
  cTipoRamo         VARCHAR2(10);
  nIdeCobert        COBERT_CERT.IdeCobert%type;
BEGIN
  --buscar el ramo al que se va a aplicar
  BEGIN
    --COBERT_CERT
    cTipoRamo := 'COBE';
    dbms_output.put_line('Inicio GENERA_MOVIMIENTO_PRIMA');
    BEGIN
      SELECT CE.CodRamoCert, CE.codcobert, CE.IdeCobert
        INTO cCodRamoCert, cCodCobert, nIdeCobert
        FROM COBERT_CERT CE, COBERT_PLAN_PROD CR, POLIZA P
       WHERE CE.IdePol = p_IdePol
         AND CE.NumCert = p_NumCert
         AND CE.CodRamoCert in pr.BUSCA_LVAL('CONDESPP', p.codprod)
         AND P.IdePol = CE.IdePol
         AND CR.CodProd = p.CodProd
         AND CR.CodPlan = ce.CodPlan
         AND CR.RevPlan = ce.RevPlan
         AND CR.CodRamoPlan = ce.CodRamoCert
         AND CR.CodCobert = ce.CodCobert
         AND CE.StsCobert = ('ACT')
         AND CE.primamoneda = (SELECT min(cc1.primamoneda)
                                 FROM COBERT_CERT cc1
                                WHERE cc1.IDEPOL = p_IdePol
                                  AND cc1.NUMCERT = p_NumCert
                                  AND cc1.CodRamoCert = CE.CodRamoCert
                                  AND cc1.Stscobert = 'ACT')
         AND rownum = 1;
         --
    EXCEPTION WHEN OTHERS THEN
      cCodRamoCert := null;
      cCodCobert   := null;
    END;
    IF cCodRamoCert is NULL THEN
      --BIEN_CERT
      cTipoRamo := 'BIEN';
      BEGIN
        SELECT BC.CodRamoCert, CB.codcobert, CB.IdeCobert
          INTO cCodRamoCert, cCodCobert, nIdeCobert
          FROM BIEN_CERT BC, COBERT_BIEN CB, POLIZA P
         WHERE BC.IdePol = p_IdePol
           AND BC.NumCert = p_NumCert
           AND P.IdePol = BC.IdePol
           AND BC.CodRamoCert in pr.BUSCA_LVAL('CONDESPP', p.codprod)
           AND BC.IdeBien = CB.IdeBien
           AND rownum = 1;
      EXCEPTION WHEN OTHERS THEN
        cCodRamoCert := null;
        cCodCobert   := null;
      END;
    END IF;
    dbms_output.put_line('cCodRamoCert '||cCodRamoCert||' , cCodCobert '||cCodCobert||' , nIdeCobert '||nIdeCobert);
    --
    IF cCodRamoCert is not NULL THEN
      BEGIN
        SELECT mc.*
          INTO rMod_Cobert
          FROM MOD_COBERT mc
         WHERE mc.IdePol = p_IdePol
           AND mc.NumCert = p_NumCert
           AND mc.CodRamoCert = cCodRamoCert
           AND mc.IdeCobert = nIdeCobert
           AND mc.NumMod = (SELECT max(mc1.NumMod)
                              FROM MOD_COBERT mc1
                             WHERE mc1.IdePol = mc.IdePol
                               AND mc1.NumCert = mc.NumCert
                               AND mc1.CodRamoCert = mc.CodRamoCert
                               AND mc1.IdeCobert = mc.IdeCobert);
       EXCEPTION WHEN OTHERS THEN
         PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('GENERA_ENDOSO_MOV_PRIMA idecobert','VAL-0040',nIdeCobert,SQLERRM);  --No encontro movimiento de cobertura
       END;
    ELSE
      PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('GENERA_ENDOSO_MOV_PRIMA idecobert','VAL-0041',cCodRamoCert,SQLERRM);  --No encontro el Ramo para la pÛliza
    END IF;
  EXCEPTION WHEN OTHERS THEN
     PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('GENERA_ENDOSO_MOV_PRIMA cCodRamoCert','VAL-0042',SQLERRM); --No se modificÛ la cobertura con menor prima
  END;

  dbms_output.put_line('cCodRamoCert '||cCodRamoCert||' , cCodCobert '||cCodCobert||' , rMod_cobert.idecobert '||rMod_cobert.idecobert);
  --
  rMod_Cobert.NumMod := rMod_Cobert.NumMod + 1;
  rMod_Cobert.StsModCobert := 'VAL';
  rMod_Cobert.PrimaMoneda := rMod_Cobert.PrimaMoneda +1;
  rMod_Cobert.PrimaMonedaCob := rMod_Cobert.PrimaMoneda;
  --
  PR_Mod_Cobert.CREAR(rMod_Cobert);
  --
  IF cTipoRamo = 'COBE' AND p_Inc = 1 THEN
    PR_Mod_Cobert.Procesar(rMod_Cobert.idecobert, rMod_Cobert.nummod,'IM');
    dbms_output.put_line('Solo se ejecuto la modificaciÛn de cobertura sin Incluir');
  ELSIF cTipoRamo = 'BIEN' AND p_Inc = 1 THEN
    PR_MOD_COBERT.INCLUIR(rMod_Cobert.IdeCobert,rMod_Cobert.nummod);
  ELSE
    dbms_output.put_line('Solo se ejecuto la modificaciÛn de cobertura sin Incluir');
  END IF;
END Genera_Movimiento_Prima;

END pr_Interfase_API_Endoso2;
/
