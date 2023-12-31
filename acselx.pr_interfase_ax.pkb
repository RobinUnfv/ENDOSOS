create or replace package body acselx.PR_INTERFASE_AX is

/*-----------------------------------------------------------------------
    Nombre : PR_INTERFASE_AX
    Autor  : Jorge benel
    Proposito: Implementaci�n de l�gica para la Digitalizaci�n de Endosos
    Log de Cambios
    Fecha        Autor                        Descripcion
    11/03/2022   Jorge Benel                  RTC-313089 Creacion
    01/04/2022   Jorge Benel                  RTC-313089 Actualizaci�n del procedimiento Actualiza_comisi�n
    06/04/2022   Ronald Barreda               RTC-313092 Actualizaci�n del procedimiento ADD_COBERT_CERT_3D
    13/04/2022   Ronald Barreda               RTC-313344 Inserci�n procedimiento de inserci�n de endosatario
    22/04/2022   Ronald Barreda               RTC-313344 Inserci�n procedimiento de exclusi�n de endosatario
    27/04/2022   Ronald Barreda               RTC-313092 Se a�adio parametros Tasa y Suma Asegurada procedimiento ADD_COBERT_CERT_3D
    05/05/2022   Ronald Barreda               RTC-313089 Se a�adio procedimiento comun TramiteVincular y TramiteDesvincular
    12/05/2022   Ronald Barreda               RTC 313089 mejora en cursor para solo considerar Certificados en INC y MOD en "AJUSTE_COMISION"
    24/05/2022   Ronald Barreda               RTC-313092 Actualizaci�n de par�metros de procedimiento Add_Cobert_Cert
    14/06/2022   Ronald Barreda               RTC-313092 Se a�adio procedimiento para Modificaci� de Corredor
    16/06/2022   Ronald Barreda               RTC 313344 Se agrego parametros canal y tipo de canal a procedimiento ModificacionCorredor
    24/06/2022   Jorge Benel                  RTC 314175 proceso de Endoso de modificaci�n de valores declarados
    24/06/2022   Jorge Benel                  RTC 313346 Implementa validaci�n VALIDA_CONTRATANTE2 para el endoso de contratante
    28/06/2022   Mauro Zegarra                RTC-314182 Cambio de Comision
    28/06/2022   Jorge Benel                  RTC-313343 Se implementa validaci�n de cambio de asegurado
    30/06/2022   Ronald Barreda               RTC-313344 Se a�adio procedimiento REVERTIR_OPERACION como parte de anulaci�n de endoso
    08/07/2022   Jorge Benel                  RTC-314175 Creacion del procedimiento GENERA_BIEN_MASI2
    12/07/2022   Jorge Benel                  RTC-314179 Creacion del Endoso de Traslado de Bienes
    14/07/2022   Jorge Benel                  RTC-314179 Reinicio de variable
    19/07/2022   Mauro Zegarra                RTC-314183 Inclusion de Equipos TREC
    20/07/2022   Jorge Benel                  RTC-313346 Incluye logica exclusi�n
    20/07/2022   Jorge Benel                  RTC-313349 Creacion de Cambio de responsable de pago
    02/08/2022   Mauro Zegarra                RTC-314181 Modificaci�n de Deducible
    29/08/2022   Jorge Benel                  RTC-313349 Complementa logica de actualizacion de la tabla PORC_DER_EMI y RESPONSABLE DE PAGO.
    29/08/2022   Mauro Zegarra                RTC-313746 Validacion de clausula ya registrada
    06/09/2022   Jorge Benel                  RTC-314175 Implementaci�n de parametros de bienes y bienes x riesgo de Endoso de Valores Declarados
    12/09/2022   Jorge Benel                  RTC-314951 Creaci�n de Endoso de Anulaci�n
    12/09/2022   Jorge Benel                  RTC-314175 implementa logica de actualizaci�n Endoso Valores Declarados
    19/09/2022   Jorge Benel                  RTC 313349 Inclusi�n de cursor de salida en procedimiento CAMBIO_RESP_PAGO
    26/09/2022   Jorge Benel                  RTC 313742 Actualizaci�n de l�gica - Modificiaci�n de Locales
    11/10/2022   JORGE BENEL                  RTC 314175 Actualizacion tipo de cambio
    14/11/2022   Jorge Benel                  RTC 314175 Carga los locales que no se modifican
    23-11-2022   Jorge Benel                  RTC 314185 Actualiza campos
    25-11-2022   Jorge Benel                  RTC 314185 Actualiza logica para exclusi�n de locales.
    07-12-2022   Jorge benel                  RTC 314185 Implementaci�n de validaci�n en Endoso de Anulaci�n
    10/01/2023   CBayona                      RTC 316106 Actualizaci�n Endoso Modificaci�n de Locales
    17/02/2023   CBayona                      RTC 316449 Se agrega nuevo procedimiento
    12/05/2023   Christian Bayona             RTC 316878 CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
    26/05/2023   Manuel Navarro               RTC 316878 Suma Asegurada VB Core
    11/08/2023   Manuel Navarro               DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
    22/09/2023   Manuel Navarro               DIGB2B03-2815 Inclusi�n Locales Legacy Dubai Masivo
  -------------------------------------------------------------------------*/

    p_TABLA_AUTORIZA_VIOLACION   ACSELX.TDU_TABLA_AUTORIZA_VIOLACION;
    p_cTipoOper                  oper_pol_det.tipoop%type := NULL;
    cIndRef1                     RIM_TAB_GRAL.INDREF1%TYPE;
    kCodParametro                CONSTANT VARCHAR2(20) := 'PARAM_SAS';  --Parametros en RIM_TAB_GRAL
    kAPI                         CONSTANT VARCHAR2(1) := 'S';           -- jbenel [S= Origen API; N=flujo normal]  -- vale
    kCodMsgValidacion            CONSTANT VARCHAR2(10) := 'VALIDACION';
    kRecalculo                   CONSTANT VARCHAR2(1) := 'S';           --  Consultar si debe aplicar rec�lculo [S] [N]
    p_cError                     varchar2(2000);
    nCantReg                     Number(5) := 0;
    i                            Number(5) := 0;
    kEstExc                      CONSTANT VARCHAR2(3)  := 'EXC'; -- registro excluido
    kCodMsgError                 CONSTANT VARCHAR2(10) := 'ERROR';
    kValido                      CONSTANT VARCHAR2(3)  := 'VAL';
    kMotivoExclusion             CONSTANT VARCHAR2(4)  := '0001'; -- POR SOLICITUD DEL ASEGURADO


    /*-----------------------------------------------------------------------
       Nombre     : MONTO_TOTAL_ENDOSADO_X_CERT
       Proposito  : Obtiene el monto total endosado por Certificado
       Parametro  : p_nIdePol          Numero Interno de poliza de seguros
                    p_nNumcert         Numero de certificado
       Log de Cambios
       Fecha        Autor               Descripcion
       -------      --------            --------------
       26/05/2023   Manuel Navarro      <RTC 316878> Creaci�n
    -----------------------------------------------------------------------------*/
    FUNCTION MONTO_TOTAL_ENDOSADO_X_CERT(p_idepol CERTIFICADO.IDEPOL%type,
                                            p_numcert CERTIFICADO.NUMCERT%type) RETURN NUMBER IS
      n_MTOTOTENDOSADO      NUMBER;
      n_MTOTOTENDOSADO1     NUMBER;

    BEGIN
        --Sumar Montos Endosados del Certificado
        BEGIN
         SELECT NVL(SUM(me.mtoendoso), 0)
           INTO n_MTOTOTENDOSADO1
           FROM ENDOSATARIO_CERT me
          WHERE me.idepol = p_idepol
            AND me.numcert = p_numcert
            AND me.stsend in ('ACT', 'INC');

         SELECT NVL(SUM(me.mtoendoso), 0)
           INTO n_MTOTOTENDOSADO
           FROM MOD_ENDOSATARIO_CERT me
          WHERE me.idepol = p_idepol
            AND me.numcert = p_numcert
            AND me.stsend = 'INC'
            AND me.nummod = (SELECT MAX(me1.nummod)
                               FROM MOD_ENDOSATARIO_CERT me1
                              WHERE me1.idepol = me.idepol
                                AND me1.numcert = me.numcert
                                AND me1.ideendcert = me.ideendcert
                                AND me1.stsend = 'INC');
          n_MTOTOTENDOSADO := n_MTOTOTENDOSADO1 + n_MTOTOTENDOSADO;
        EXCEPTION WHEN OTHERS THEN
          n_MTOTOTENDOSADO := 0;
        END;
        RETURN n_MTOTOTENDOSADO;
    END MONTO_TOTAL_ENDOSADO_X_CERT;

    /*-----------------------------------------------------------------------
       Nombre     : VALIDA_SUMAASEGURADA_SUSTENTO
       Proposito  : Valida si el total de endoso es mayor a la suma asegurada real
       Parametro  : p_nIdePol          Numero Interno de poliza de seguros
                    p_nNumcert         Numero de certificado

                    Devuelve 1 si el valor es mayor y 0 en caso contrario

       Log de Cambios
       Fecha        Autor               Descripcion
       -------      --------            --------------
       26/05/2023   Manuel Navarro      <RTC 316878> Creaci�n
    -----------------------------------------------------------------------------*/

    FUNCTION VALIDA_SUMAASEGURADA_SUSTENTO(p_idepol    poliza.idepol%type,
                                           p_numcert   certificado.numcert%type,
                                           p_MontoEndoso Number,
                                           p_Aprobado Varchar2,
                                           p_Sustento Varchar2,
                                           p_usuarioaprob Varchar2,
                                           p_cError out Varchar2
                                           ) Return Number IS
        nFlag Integer;
        n_SumaAsegRP Number;
        n_TotalEndoso Number;
    BEGIN
        IF pr.BUSCA_LVAL('SWITCH','316474') = '0' THEN
           p_cError := '0,OK';
           Return 0;
        END IF;

        n_TotalEndoso := p_MontoEndoso + MONTO_TOTAL_ENDOSADO_X_CERT(p_idepol, p_numcert) ;

        IF pr.BUSCA_LVAL('SWITCH','316474_1') IN ('1','2') THEN
          nFlag := pr.BUSCA_LVAL('SWITCH','316474_1');
        ELSE
          nFlag := 1;
        END IF;

        n_SumaAsegRP := ACSELX.FN_SUMAASEG_RPRINCIPAL_X_CERT(p_idepol, p_numcert, nFlag);



        IF n_TotalEndoso > n_SumaAsegRP THEN
           IF p_usuarioaprob IS NULL OR NVL(LENGTH(trim(p_usuarioaprob)),0) = 0 THEN
               DBMS_OUTPUT.PUT_LINE('Se Necesita el c�digo de usuario de aprobaci�n');
               p_cError := '1,Se Necesita el c�digo de usuario de aprobaci�n';
               RETURN 1;
           END IF;
           IF (p_Aprobado IS NULL OR p_Aprobado <> 'S') OR
              (p_Sustento IS NULL OR NVL(LENGTH(trim(p_Sustento)),0) = 0) THEN
              DBMS_OUTPUT.PUT_LINE('Se Necesita Aprobaci�n y sustento cuando el Endoso es mayor a la Suma Asegurada');
              p_cError := '100,Se Necesita Aprobaci�n y sustento de la misma cuando el Endoso es mayor a la Suma Asegurada';
              RETURN 1;
           END IF;

           p_cError := '0,OK';
           RETURN 1;
        ELSE
           p_cError := '0,OK';
           RETURN 0;
        END IF;
    END VALIDA_SUMAASEGURADA_SUSTENTO;

    /*-----------------------------------------------------------------------
       Nombre     : INSERTA_AUTORIZACIONE
       Proposito  : Graba el sustento cuando el total de endoso es mayor a la suma asegurada real
       Parametro  : p_nIdePol          Numero Interno de poliza de seguros
                    p_nNumcert         Numero de certificado
                    p_ideendcert mod_endosatario_cert.IDEENDCERT%type,
                    p_useraut   varchar2,
                    p_sustento  varchar2,
                    p_nummod    varchar2,

       Retorno :   p_TABLA_AUTORIZA_VIOLACION  Array con las politicas violadas en el proceso
                   p_cursorMsgValida           Cursor con los mensajes de errores
                   p_cError                    Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                     0, OK
                     1, Error


       Log de Cambios
       Fecha        Autor               Descripcion
       -------      --------            --------------
       26/05/2023   Manuel Navarro      <RTC 316878> Creaci�n
    -----------------------------------------------------------------------------*/
    PROCEDURE INSERTA_AUTORIZACIONE (p_idepol    poliza.idepol%type,
                                     p_numcert   certificado.numcert%type,
                                     p_ideendcert mod_endosatario_cert.IDEENDCERT%type,
                                     p_useraut   varchar2,
                                     p_sustento  varchar2,
                                     p_nummod    varchar2,
                                     p_cError    out varchar2) IS
    BEGIN
      insert into autoriza_control_endo
      (ideaut, idepol, numcert, numcontrol, tipocontrol, codusu, codusuaut, stsaut, fecreg, fecaut, msjcontrol, observacion)
      values
      (sq_autoriza_control_endo.nextval, p_idepol, p_numcert, p_ideendcert, 'ENDOSO' , USER, p_useraut, 'ACT', sysdate, sysdate, p_sustento, p_nummod);
      p_cError := '0,OK';
    EXCEPTION WHEN OTHERS THEN
       p_cError := '1, ' || SQLERRM;
    END INSERTA_AUTORIZACIONE;



  PROCEDURE LIMPIAR_LOG_MENSAJES  IS
    PRAGMA AUTONOMOUS_TRANSACTION;
  BEGIN
    DELETE FROM acselx.GT_ITF_SAS_AX_MENSAJES;
    COMMIT;
  EXCEPTION
    WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR('-20002','Error en el paquete PR_INTERFASE_AX_SAS.LIMPIAR_LOG_MENSAJES '||SQLERRM);
  END LIMPIAR_LOG_MENSAJES;

/*-----------------------------------------------------------------------
   Nombre : CONFIRMAR_RECALCULO
   Proposito: En caso de existir pol�tica de rec�lculo, pedir confirmaci�n.
   Parametro :
                p_nIdeAut  Identificacion del bloqueo
                p_nIdePol  Numero Interno de poliza de seguros.

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
-----------------------------------------------------------------------------*/

PROCEDURE CONFIRMAR_RECALCULO (
    p_nIdeAut     AUTORIZA_VIOLACION.IdeAut%TYPE  ,
    p_nIdePol     AUTORIZA_VIOLACION.IdePol%TYPE  ,
    p_cRecalculo  Varchar2
    ) IS

  CCODBLOQ               AUTORIZA_VIOLACION.CODBLOQ%TYPE;
  cConfRedist            VARCHAR2(1) := NULL;
  cTextoTr               VARCHAR2(4000);
  ret_button             NUMBER;
  n_Alerta               NUMBER;
  cError                 VARCHAR2(200);


BEGIN

  BEGIN
    SELECT CODBLOQ
      INTO CCODBLOQ
      FROM AUTORIZA_VIOLACION
     WHERE IdePol      = p_nIdePol
       AND IdeAut      = p_nIdeAut;
  EXCEPTION WHEN OTHERS THEN
    CCODBLOQ := NULL;
  END;

  IF PR.BUSCA_LVAL('BLQRECAL', CCODBLOQ) != 'INVALIDO' THEN -- CON MOVIMIENTO DE VALIDACI�N DE RECALCULO

    --<I RTC-93382> Brayan Pereda / 28-03-2017 / Solo se eliminan los registros de AUTORIZA_REDIST si la version es 1
    IF PR.BUSCA_LVAL('CFREAMIX','VERSION') = '1' THEN
    --<F RTC-93382>
      -- Limpiamos alguna confirmaci�n previa, en caso exista...
      DELETE AUTORIZA_REDIST
       WHERE IDEPOL = p_nIdePol
         AND NUMOPER IS NULL
         AND INDAPLIC = 'S'
         AND INDREDIST IS NOT NULL;
    END IF; --<RTC-93382> Brayan Pereda / 28-03-2017 / Solo se eliminan los registros de AUTORIZA_REDIST si la version es 1

    /*
    cTextoTr := 'Si elige Ok, se recalcular� la distribuci�n de reaseguros de movimientos anteriores. '|| CHR(13) ||
                'Si elige Cancelar, la Cuenta Corriente se generar� en base al nuevo movimiento.';
    ret_button := LR_ConfBox(cTextoTr,'CONFIRMACION DE RECALCULO');
    --
    IF ret_button = ALERT_BUTTON1 THEN
      cConfRedist := 'S';
    ELSIF ret_button = ALERT_BUTTON2 THEN
      cConfRedist := 'N';
    END IF;
    */
    --
    BEGIN
      UPDATE AUTORIZA_REDIST
         SET INDREDIST = p_cRecalculo --cConfRedist
       WHERE IDEPOL = p_nIdePol
         AND NUMOPER IS NULL
         AND INDAPLIC = 'S'
         AND INDREDIST IS NULL;

    END;

  END IF;

EXCEPTION
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR('-20002','Error al confirmar el rec�lculo. p_cRecalculo-->'||p_cRecalculo||SQLERRM);
END CONFIRMAR_RECALCULO;

/*-----------------------------------------------------------------------
   Nombre : CONFIRMAR_RECALCULO
   Proposito: En caso de existir pol�tica de rec�lculo, pedir confirmaci�n.
   Parametro :
                p_nIdePol  Numero Interno de poliza de seguros.

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.

-----------------------------------------------------------------------------*/

PROCEDURE FR_AUTORIZA_VIOLACION ( p_nIdePol                  IN  POLIZA.idepol%type,
                                  p_cTipoOper                 IN  OPER_POL_DET.tipoop%type,
                                  p_TABLA_AUTORIZA_VIOLACION  IN  ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                  p_cCodUsuario               IN  VARCHAR2,
                                  -- Variables de retorno
                                  p_cursorMsgValida           OUT SYS_REFCURSOR ,
                                  p_cError                    OUT VARCHAR2 ) Is

   nCantReg        Number(5) := 0;
  BEGIN

       LIMPIAR_LOG_MENSAJES;

       BEGIN
         nCantReg := p_TABLA_AUTORIZA_VIOLACION.COUNT;
       EXCEPTION
           WHEN OTHERS THEN
              nCantReg := 0;
              PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('FR_AUTORIZA_VIOLACION', 'VAL-9999'); --'No se han enviado los violaciones de la p�liza.

       END;

       IF nCantReg > 0 THEN
         FOR i in p_TABLA_AUTORIZA_VIOLACION.First .. p_TABLA_AUTORIZA_VIOLACION.Last LOOP
           IF p_TABLA_AUTORIZA_VIOLACION(i).Autoriza = 'S' THEN
             dbms_output.put_line('*** ingresa ****'||p_TABLA_AUTORIZA_VIOLACION(i).IdeAut||' , '||p_nIdePol||' , '|| p_cCodUsuario);

             CONFIRMAR_RECALCULO (p_TABLA_AUTORIZA_VIOLACION(i).IdeAut , p_nIdePol, kRecalculo);
             PR_EVALUA_POLITICA.AUTORIZA_BLOQUEO (p_TABLA_AUTORIZA_VIOLACION(i).IdeAut ,p_nIdePol ,p_cCodUsuario); -- aqui actualiza la tabla "autoriza_violacion"
           END IF;

         END LOOP;


      END IF;

      IF PR_INTERFASE_AX_SAS_UTIL.EXISTE_LOG_MENSAJES = 'S' THEN

          RAISE_APPLICATION_ERROR(-20100,'Existen violaciones no autorizadas');
       END IF;

 END FR_AUTORIZA_VIOLACION;

/*-----------------------------------------------------------------------
   Nombre : EJECUTA_POLITICAS_VIOLADAS
   Proposito: Ejecuta politicas previa  a la activaci�n de la poliza.
   Parametro :  p_nIdePol    Numero Interno de poliza de seguros.
                p_nNumPol    Numero de Poliza.
                p_cCodProd   Codigo de producto.
                p_cTipoOper   Tipo de operaci�n
                p_cCodUsuario    Codigo usuario de auditoria,
                p_TABLA_POLITICA_AUTORIZADA  Array con las politicas previamente autorizadas

   Retorno :   p_TABLA_AUTORIZA_VIOLACION  Array con las politicas violadas en el proceso
               p_cursorMsgValida           Cursor con los mensajes de errores
               p_cError                    Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                 0, OK
                 1, Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
   23/03/2022   Jorge Benel       <RTC 313089> actualizaci�n para pruebas en TEST.
   01/04/2022   Jorge Benel       <RTC 313089> Se actualiza logica para autorizar politicas violadas.
   26/04/2020   Jorge Benel       <RTC 313089> Se inserta la funcion autorizador

-----------------------------------------------------------------------------*/

PROCEDURE EJECUTA_POLITICAS_VIOLADAS ( p_nIdePol                   IN poliza.idepol%type,
                                       p_nNumPol                   IN poliza.numpol%type,
                                       p_cCodProd                  IN poliza.codprod%type,
                                       p_cTipoOper                 IN oper_pol_det.tipoop%type,
                                       p_cCodUsuario               IN VARCHAR2,
                                       p_TABLA_POLITICA_AUTORIZADA IN ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                       -- Variables de retorno
                                       p_TABLA_AUTORIZA_VIOLACION  OUT ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                       p_cursorMsgValida           OUT sys_refcursor ,
                                       p_cError                    OUT VARCHAR2 ) AS

   i          NUMBER(5);
   cPolitica   VARCHAR2(1);
   nSecuencia  LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;

-- <I RTC 313089 > 26-04-2022
FUNCTION AUTORIZADOR (   p_nIDEPOL   IN POLIZA.IDEPOL%TYPE,
                         p_cUSER    IN VARCHAR2,
                         p_cCODBLOQ IN ACSELX.AUTORIZADOR.CODBLOQ%TYPE,
                         p_cCODAUT  IN USUARIO_AUTORIZA.CODAUT%TYPE
                         ) RETURN VARCHAR2 is

  vAutoriza         VARCHAR2(1) := 'S';
  cUsuario          USUARIO_AUTORIZA.CodUsu%TYPE;

BEGIN
  BEGIN
    SELECT DISTINCT CodUsu
    INTO   cUsuario
    FROM   USUARIO_AUTORIZA
    WHERE  CodUsu   = p_cUSER
    AND    CodAut   IN ( SELECT CODAUT
                         FROM AUTORIZADOR
                         WHERE CODBLOQ = p_cCODBLOQ
                        )
    AND    StsAut   = 'ACT';

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
        BEGIN
          SELECT USER
          INTO cUsuario
          FROM DUAL
          WHERE EXISTS ( SELECT 1
                         FROM USUARIO_AUTORIZA
                         WHERE CodUsu    = p_cUSER
                         AND  CodAut     = p_cCODAUT
                         AND    StsAut   = 'ACT'
                         ) ;

        EXCEPTION
          WHEN OTHERS THEN
            cUsuario := 'INVALIDO';
        END ;
    WHEN OTHERS THEN
          cUsuario := 'INVALIDO';
  END;

  IF p_cUSER = cUsuario THEN
    vAutoriza := 'S';
  ELSE
    vAutoriza := 'N';
  END IF;

  RETURN vAutoriza;

end AUTORIZADOR;
-- <F RTC 313089 > 26-04-2022

 BEGIN

   p_cError := '0,OK'; --<RTC-313091> Jorge Benel /  24-06-2022 / Inicializaci�n de variable
   ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'EJECUTA_POLITICAS_VIOLADAS','',nSecuencia );

   EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION := acselx.TDU_TABLA_AUTORIZA_VIOLACION();
   -- Determinar si se validan las politicas del A/X --
   If kAPI = 'S' Then
      cIndRef1 := 'S';
   Else
      cIndRef1 := ACSELX.PR_INTERFASE_AX_SAS_UTIL.OBTENER_VALOR_RIM_TAB_GRAL(kCodParametro,'PARAM003','INDREF1');

   End if;

   IF cIndRef1 = 'S' THEN

     BEGIN
      dbms_output.put_line('p_nIdePol, p_nNumPol, p_cCodProd = '||p_nIdePol||' , '|| p_nNumPol||' , '|| p_cCodProd);
        -- Almacena la violacio�n de politicas en tabla autoriza_violacion
        cPolitica := PR_POLITICAS.PRINCIPAL(p_nIdePol, p_nNumPol, p_cCodProd, USER, 'AC');
     EXCEPTION
       WHEN OTHERS THEN
        dbms_output.put_line('Error en evaluacion de politicas');
        ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTIVAR_POLIZA', 'VAL-0570',SQLERRM);--Error en evaluacion de politicas - :mx1.
     END;

     dbms_output.put_line('cPolitica = '||cPolitica);

     IF cPolitica = 1 THEN
       -- <I RTC 313089 > 26-04-2022  Autoriza las violacione de politicas x USER de base de datos
       FOR c1 in ( SELECT IDEPOL, CODBLOQ, CODAUT, STSAUTORIZADO
                    FROM  AUTORIZA_VIOLACION
                    WHERE IDEPOL = p_nIdePol
                      AND STSAUTORIZADO <> 'ACT'  ) LOOP

         IF AUTORIZADOR ( c1.idepol, USER, c1.CODBLOQ, c1.CODAUT ) = 'S' THEN
            UPDATE AUTORIZA_VIOLACION
            SET STSAUTORIZADO    = 'ACT' ,
                AUTORIZADOPOR    = USER,
                FECAUTORIZADO    = TRUNC(SYSDATE),
                HORAUTORIZADO    = TO_CHAR(SYSDATE,'HH24:MI:SS'),
                CODAUT           = ( SELECT MIN(CodAut)
                                    FROM   USUARIO_AUTORIZA
                                    WHERE  CodUsu  = USER
                                      AND  StsAut = 'ACT'
                                      )
            WHERE IDEPOL         = c1.IDEPOL
            AND CODBLOQ          = c1.CODBLOQ
            AND STSAUTORIZADO    = c1.STSAUTORIZADO
            ;
         END IF;

       END LOOP;
       -- <F RTC 313089 > 26-04-2022
       -- <I RTC-313089 jbenel 01/04/2022 Digitalizaci�n de Endoso>
       IF p_TABLA_POLITICA_AUTORIZADA.COUNT > 0 THEN
         FOR i in p_TABLA_POLITICA_AUTORIZADA.First .. p_TABLA_POLITICA_AUTORIZADA.Last LOOP
           IF AUTORIZADOR ( p_TABLA_POLITICA_AUTORIZADA(i).idepol, p_TABLA_POLITICA_AUTORIZADA(i).autorizadopor, p_TABLA_POLITICA_AUTORIZADA(i).CODBLOQ, p_TABLA_POLITICA_AUTORIZADA(i).CODAUT ) = 'S' THEN -- <RTC 313089 > 26-04-2022
              UPDATE AUTORIZA_VIOLACION
              SET STSAUTORIZADO   = 'ACT' ,
                  AUTORIZADOPOR    = p_TABLA_POLITICA_AUTORIZADA(i).autorizadopor,
                  FECAUTORIZADO    = p_TABLA_POLITICA_AUTORIZADA(i).fecautorizado ,
                  HORAUTORIZADO    = p_TABLA_POLITICA_AUTORIZADA(i).horautorizado ,
                  CODAUT           = p_TABLA_POLITICA_AUTORIZADA(i).codaut -- <RTC 313089 > 26-04-2022
              WHERE IDEPOL         = p_TABLA_POLITICA_AUTORIZADA(i).idepol
              AND CODBLOQ          = p_TABLA_POLITICA_AUTORIZADA(i).CODBLOQ
              AND STSAUTORIZADO   <> 'ACT'
                  ;
            END IF; -- <RTC 313089 > 26-04-2022
         END LOOP;
       END IF;
        -- <F RTC-313089 jbenel 01/04/2022 Digitalizaci�n de Endoso>

        i := 0;

        FOR c1 in ( SELECT *
                    FROM  AUTORIZA_VIOLACION
                    WHERE IDEPOL = p_nIdePol
                      AND STSAUTORIZADO <> 'ACT'  ) LOOP

           -- <I RTC-313089 jbenel 01/04/2022 Digitalizaci�n de Endoso>
           IF i = 0 THEN
             ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTIVAR_POLIZA', 'VAL-0569'); --Existen politicas violadas. Verifique.:mx1
             p_cerror := 'VAL-0569,Existen politicas violadas';
           END IF;
           -- <F RTC-313089 jbenel 01/04/2022 Digitalizaci�n de Endoso>

           ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTIVAR_POLIZA', kCodMsgValidacion ,'Politica Violada : ' || c1.motivoviolacion );
           --
           i := i + 1;

             EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION.EXTEND(1);

             EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i) := acselx.TDU_AUTORIZA_VIOLACION(codprod         => NULL,
                                                                          numpol          => NULL,
                                                                          idepol          => NULL,
                                                                          idepolitica     => NULL,
                                                                          autorizadopor   => NULL,
                                                                          stsautorizado   => NULL,
                                                                          fecautorizado   => NULL,
                                                                          horautorizado   => NULL,
                                                                          ideaut          => NULL,
                                                                          codaut          => NULL,
                                                                          motivoviolacion => NULL,
                                                                          autoriza        => NULL,
                                                                          txtadicional    => NULL,
                                                                          CODBLOQ         => NULL ) ;

          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).numpol         := c1.numpol  ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).idepol         := c1.idepol  ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).idepolitica    := c1.idepolitica ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).autorizadopor  := c1.autorizadopor ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).stsautorizado  := c1.stsautorizado ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).fecautorizado  := c1.fecautorizado ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).horautorizado  := c1.horautorizado ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).ideaut         := c1.ideaut ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).codaut    := c1.codaut  ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).motivoviolacion := c1.motivoviolacion ;
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).autoriza       := 'N' ; -- indicador de politica que debe ser autorizada
          EJECUTA_POLITICAS_VIOLADAS.p_TABLA_AUTORIZA_VIOLACION(i).CODBLOQ       := c1.CODBLOQ ;
        END LOOP;
     END IF; -- cPolitica = 1

   END IF; -- cIndRef1 = 'S'

    -- p_cerror := '0,OK'; -- <RTC-313089 jbenel 01/04/2022 Se comenta>

 EXCEPTION
   WHEN OTHERS THEN
       p_cError := '1,'||SQLERRM;
       ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR',SYSDATE,SYSDATE,'EJECUTA_POLITICAS_VIOLADAS','',nSecuencia ); -- <N3041576 > 02.10.2013

 End EJECUTA_POLITICAS_VIOLADAS;


/*-----------------------------------------------------------------------
   Nombre : CARGA_POLITICAS_AUTORIZADA
   Proposito: Proceso de autorizaci�n de politicas.
   Parametro :
                p_nIdePol  Numero Interno de poliza de seguros.

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.

-----------------------------------------------------------------------------*/

 PROCEDURE CARGA_POLITICAS_AUTORIZADA (p_nIdePol               in poliza.idepol%type,
                                   nNumPol                     in poliza.numpol%type,
                                   cCodProd                    in poliza.codprod%type,
                                   p_cTipoOper                 in oper_pol_det.tipoop%type,
                                   p_cCodUsuario               in varchar2,
                                   -- Variables de retorno
                                   p_TABLA_AUTORIZA_VIOLACION  OUT ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                   p_cursorMsgValida           OUT sys_refcursor ,
                                   p_cError                    OUT varchar2 ) AS

   i number(5);
   cPolitica   VARCHAR2(1);
   vcount number(5);
   Begin

     p_TABLA_AUTORIZA_VIOLACION := acselx.TDU_TABLA_AUTORIZA_VIOLACION();

     i := 0;

     For x in (select *  from AUTORIZA_VIOLACION where idepol = p_nIdePol ) loop
       i := i + 1;
       p_TABLA_AUTORIZA_VIOLACION.EXTEND(1);

       p_TABLA_AUTORIZA_VIOLACION(i) := acselx.TDU_AUTORIZA_VIOLACION(codprod         => NULL,
                                                                      numpol          => NULL,
                                                                      idepol          => NULL,
                                                                      idepolitica     => NULL,
                                                                      autorizadopor   => NULL,
                                                                      stsautorizado   => NULL,
                                                                      fecautorizado   => NULL,
                                                                      horautorizado   => NULL,
                                                                      ideaut          => NULL,
                                                                      codaut          => NULL,
                                                                      motivoviolacion => NULL,
                                                                      autoriza        => NULL,
                                                                      txtadicional    => NULL,
                                                                      CODBLOQ         => NULL
                                                                       ) ;

    p_TABLA_AUTORIZA_VIOLACION(i).codprod := x.codprod;
    p_TABLA_AUTORIZA_VIOLACION(i).numpol    := x.numpol  ;
    p_TABLA_AUTORIZA_VIOLACION(i).idepol    := x.idepol  ;
    p_TABLA_AUTORIZA_VIOLACION(i).idepolitica := x.idepolitica ;
    p_TABLA_AUTORIZA_VIOLACION(i).autorizadopor  := x.autorizadopor ;
    p_TABLA_AUTORIZA_VIOLACION(i).stsautorizado  := x.stsautorizado ;
    p_TABLA_AUTORIZA_VIOLACION(i).fecautorizado  := x.fecautorizado ;
    p_TABLA_AUTORIZA_VIOLACION(i).horautorizado  := x.horautorizado ;
    p_TABLA_AUTORIZA_VIOLACION(i).ideaut         := x.ideaut ;
    p_TABLA_AUTORIZA_VIOLACION(i).codaut    := x.codaut  ;
    p_TABLA_AUTORIZA_VIOLACION(i).motivoviolacion := x.motivoviolacion ;
    p_TABLA_AUTORIZA_VIOLACION(i).autoriza       := 'S' ; -- indicador de politica autorizada
    p_TABLA_AUTORIZA_VIOLACION(i).txtadicional   := '' ;

  end loop;

  -- < INICIO > MUESTRA LOS RESULTADOS EN PANTALLA
       BEGIN
         vcount := p_TABLA_AUTORIZA_VIOLACION.COUNT;--<RTC100678>
       EXCEPTION
           WHEN OTHERS THEN
              vcount := 0;

             --  PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('FR_AUTORIZA_VIOLACION', 'VAL-9999'); --'No se han enviado los violaciones de la p�liza.
       END;

       --Verificar la existencia del anexo de la poliza
       IF vcount > 0 THEN

         FOR i in p_TABLA_AUTORIZA_VIOLACION.First .. p_TABLA_AUTORIZA_VIOLACION.Last LOOP

           dbms_output.put_line('----'||i||'-----');

           dbms_output.put_line('codprod -> '||p_TABLA_AUTORIZA_VIOLACION(i).codprod) ;
           dbms_output.put_line('numpol -> '||p_TABLA_AUTORIZA_VIOLACION(i).numpol )   ;
           dbms_output.put_line('idepol -> '||p_TABLA_AUTORIZA_VIOLACION(i).idepol )  ;
           dbms_output.put_line('idepolitica -> '||p_TABLA_AUTORIZA_VIOLACION(i).idepolitica );
           dbms_output.put_line('autorizadopor -> '||p_TABLA_AUTORIZA_VIOLACION(i).autorizadopor );
           dbms_output.put_line('stsautorizado -> '||p_TABLA_AUTORIZA_VIOLACION(i).stsautorizado );
           dbms_output.put_line('fecautorizado -> '||p_TABLA_AUTORIZA_VIOLACION(i).fecautorizado )  ;
           dbms_output.put_line('horautorizado -> '||p_TABLA_AUTORIZA_VIOLACION(i).horautorizado )  ;
           dbms_output.put_line('ideaut -> '||p_TABLA_AUTORIZA_VIOLACION(i).ideaut   )      ;
           dbms_output.put_line('codaut -> '||p_TABLA_AUTORIZA_VIOLACION(i).codaut  )  ;
           dbms_output.put_line('motivoviolacion -> '||p_TABLA_AUTORIZA_VIOLACION(i).motivoviolacion );
           dbms_output.put_line('autoriza -> '||p_TABLA_AUTORIZA_VIOLACION(i).autoriza  )  ;

         END LOOP;

      END IF;
   -- < FIN > MUESTRA LOS RESULTADOS EN PANTALLA

 End CARGA_POLITICAS_AUTORIZADA;


/*-----------------------------------------------------------------------
   Nombre : VALIDA_FACULTATIVO
   Proposito: V�lida si una poliza es facultativa o no.
   Parametro :
                p_nIdePol   Numero Interno de poliza de seguros.
   Retorno   : <> 0 'Es facultativo'
                = 0 'No es Facultativo'

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
   23/03/2022   Jorge Benel       <RTC 313089> actualizaci�n para pruebas en TEST.

-----------------------------------------------------------------------------*/

FUNCTION VALIDA_FACULTATIVO (p_nIdePol  Poliza.Idepol%TYPE ) RETURN NUMBER IS

v_nExiste   NUMBER(2) := 0;
v_nNumCert Certificado.Numcert%TYPE := 1;
v_cTipoOp  Oper_Pol.Tipoop%TYPE := 'EMI';

BEGIN

  BEGIN
    SELECT COUNT(1)
    INTO v_nExiste
    FROM GEN_REA  gr, DIST_REA dr, DIST_FACULT df
    WHERE gr.IdePol  = p_nIdePol
    AND   gr.NumCert = v_nNumCert
    AND   gr.IdeRec IN (SELECT IdeRec
                     FROM   RECIBO r
                     WHERE  r.IdeOp   = (
                             SELECT numoper
                             FROM oper_pol
                             WHERE idepol = p_nIdePol
                             AND numcert = v_nNumCert
                             AND tipoop = v_cTipoOp
                             )
                     )
    AND dr.IdeRec     = gr.IdeRec
    AND dr.CodRamoRea = gr.CodRamorea
    AND NVL(dr.NumModGenRea,0) = NVL(NVL(gr.NumModGenRea,dr.NumModGenRea),0)
    AND dr.NumModDistRea = 0
    --
    AND df.IdeRec = gr.IdeRec
    AND df.CodRamoRea= gr.CodRamoRea
    AND df.CodRamoCert= gr.CodRamoCert
    AND df.NumModDistRea=dr.NumModDistRea
    AND NVL(df.NumModGenRea,0)=NVL(gr.NumModGenrea,0)
    ;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
        v_nExiste := 0;
    WHEN OTHERS THEN
        v_nExiste := 0;
  END;
  RETURN v_nExiste;

END VALIDA_FACULTATIVO;


/*-----------------------------------------------------------------------
   Nombre     : ACTUALIZA_COMISION
   Proposito  : Actualizaci�n la comisi�n de la prorroga con la comisi�n que se calcula en el proceso de activaci�n de la poliza
   Referencia : Formulario EMI00139.FMB y CAJ00308
   Parametro  :
                p_nIdePol   Numero Interno de poliza de seguros.
                p_nNumcert  Numero de certificado.


   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
   23/03/2022   Jorge Benel       <RTC 313089> actualizaci�n para pruebas en TEST.
   12/05/2022   Ronald Barreda    <RTC 313089> mejora en cursor para solo considerar Certificados en INC y MOD
   30/03/2023   Christian Bayona  <RTC 316747> Modificaci�n, inicializar variable
-----------------------------------------------------------------------------*/
 PROCEDURE ACTUALIZA_COMISION(   p_nIdePol         IN POLIZA.IDEPOL%TYPE ,
                                 p_nNumOper        IN oper_pol.Numoper%TYPE
                               ) AS

    nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;

    nPorcComOrig      RECIBO.PORCCOM%TYPE;
    nPorcCom          RECIBO.PORCCOM%TYPE;
    nnumcert          CERTIFICADO.NUMCERT%TYPE;
    nIndicador        NUMBER;
    nAutorizado       NUMBER;
    p_nindpaso        NUMBER(1) := 0;
    p_ninderr         NUMBER(1) := 0;
    i                 NUMBER(3) := 0;
    vExiste           NUMBER(3) := 0;


    BEGIN

       acselx.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'ACTUALIZA_COMISION','',nSecuencia );

       -- lee la comisi�n de la activaci�n de la poliza
       FOR x IN (  SELECT R.NumCert, RA.CodRamo,
                          NVL(SUM(R.MtoMoneda),0) Prima,
                          NVL(SUM(R.MtoComMoneda),0)Comision,
                          R.PorcCom
                   FROM   RECIBO R, CERTIFICADO C, RAMO RA
                   WHERE  R.IdePol  = C.Idepol
                     AND  R.NumCert = C.NumCert
                     AND  C.StsCert IN ( 'INC', 'MOD' )
                     AND  R.IdeOp   IN ( SELECT op.numoper
                                         FROM  oper_pol op
                                         WHERE op.idepol = p_nIdePol
                                         AND tipoop = 'EMI' )
                   AND    R.CodRamoCert = RA.CodRamo
                   GROUP BY R.NumCert, RA.DescRamo, RA.CodRamo, R.PorcCom

         ) LOOP

         --Verificamos los permisos correspondientes

          nIndicador   := PR_Comision_Especial.Validar_Cambio_Comision(p_nIdePol, x.numcert);

          IF nIndicador <> 1 THEN
            nAutorizado := PR_Acceso_Usuario.Valida_Autorizacion('202');
            IF nAutorizado = 0 THEN
              p_ninderr := ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTUALIZA_COMISION', 'VAL-0491', PR_SESION.USUARIO_APP, '202','MODIF. DE COMISION TEMPORAL DEVOL.PRIMA' ); --'El Usuario :mx1 no tiene permiso para realizar operacion :mx2. :mx3'
              p_nindpaso := p_nindpaso + p_ninderr;
            END IF;
          END IF;
           --Generamos informacion de la comision
          BEGIN
            PR_Recibo_T.Generar_Comision_Temp(p_nIdePol,x.numcert,p_nNumOper);
          EXCEPTION
            WHEN OTHERS THEN
              ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTUALIZA_COMISION', 'VAL-1017', p_nIdePol||'-'||x.numcert, SQLERRM ); --'Para IdePol-NumCert :mx1, presento error al ejecutar PR_RECIBO_T.GENERAR_COMISION_TEMP. :mx2'
              p_nindpaso := 1;
          END;

          --Obtenemos el monto de comision original (la primera incidencia)

          BEGIN
            SELECT NVL(SUM(NVL(t.porccom,0)),0)
            INTO nPorcComOrig
            FROM TEMP_CAL_COM t
            WHERE t.idepol = p_nIdePol
            AND t.numcert = x.numcert
            -- AND t.codramocert =  -- no se considera segun lo especificado
            AND rownum = 1;

            IF nPorcComOrig > x.PorcCom THEN
              nAutorizado := PR_Acceso_Usuario.Valida_Autorizacion('088');
              IF nAutorizado = 0 THEN
                p_ninderr := ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTUALIZA_COMISION', 'VAL-0491', PR_SESION.USUARIO_APP, '088', 'MODIFICACION DE COMISION TEMPORAL (ACTIVA POLIZA)' ); --'El Usuario :mx1 no tiene permiso para realizar operacion :mx2. :mx3'
                p_nindpaso := p_nindpaso + p_ninderr;
              END IF;
            END IF;
          EXCEPTION
            WHEN OTHERS THEN
              ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTUALIZA_COMISION', 'VAL-0492', p_nIdePol||'-'||x.numcert, SQLERRM ); --'Para IdePol-NumCert :mx1, se presento un error al buscar en la tabla TEMP_CAL_COM. :mx2'
              p_nindpaso := 1;
          END ;
          -- Si existen errores no actaulizamos
          IF p_nindpaso != 0 THEN
            NULL;
          ELSE
            IF nPorcComOrig != x.PorcCom THEN

              BEGIN
                nPorcCom := x.PorcCom; --<RTC 316747> / CBayona / 30-03-2023 / Se inicializa variable
                UPDATE TEMP_CAL_COM t
                SET t.Mtocommoneda = t.MtoMoneda * nPorcCom /100,
                    t.MtoComLocal  = t.MtoLocal  * nPorcCom /100,
                    t.porccom = nPorcCom
                WHERE t.idepol = p_nIdePol
                AND t.numcert = x.numcert;
              EXCEPTION
                WHEN OTHERS THEN
                  ACSELX.PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('ACTUALIZA_COMISION', 'VAL-0493', p_nIdePol||'-'||x.numcert, SQLERRM); --'Para IdePol-NumCert :mx1, se presento un error al actualizar la tabla TEMP_CAL_COM. :mx2'
                  p_nindpaso := 1;
              END;
            END IF;
          END IF;

          p_nindpaso := 0;

      END LOOP;

      acselx.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'ACTUALIZA_COMISION','',nSecuencia );
  EXCEPTION
    WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;
          ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR',SYSDATE,SYSDATE,'ACTUALIZA_COMISION','',nSecuencia ); -- <N3041576 > 02.10.2013
  END ACTUALIZA_COMISION;


/*-----------------------------------------------------------------------
   Nombre     : PRIMA_PACTADA
   Proposito  : Actualizaci�n de la prima que se calcula en el proceso de activaci�n de la poliza
   Referencia : Formulario EMI00139.FMB
   Parametro  : p_nIdePol          Numero Interno de poliza de seguros
                p_nNumcert         Numero de certificado
                p_nMtoPrimaPactada Monto de la prima pactada
                p_cCodReca         Codigo de recargo
                p_cCodDcto         Codigo de descuento
                p_cTxtAdicional    Texto Adicional
   Retorno :    p_cRetorno         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
               0, OK
               1, Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
    23/03/2022   Jorge Benel       <RTC 313089> actualizaci�n para pruebas en TEST.

-----------------------------------------------------------------------------*/

 PROCEDURE PRIMA_PACTADA( p_nIdePol           IN POLIZA.IDEPOL%TYPE,
                           p_nNumCert         IN CERTIFICADO.NUMCERT%TYPE,
                           p_nMtoPrimaPactada IN OPER_POL.MTOOPER%TYPE,
                           p_cCodReca         IN RECA_DCTO.CODRECADCTO%TYPE,
                           p_cCodDcto         IN RECA_DCTO.CODRECADCTO%TYPE,
                           p_cTxtAdicional    IN VARCHAR2,
                           p_cRetorno         OUT VARCHAR2
                          ) AS

   nDummy          NUMBER;
   nIndPrima       NUMBER(1) := 0;
   nButton         NUMBER;
   nResultado      NUMBER(12,2);
   nNumOper        OPER_POL.NumOper%TYPE;
   nSumRecaDcto    DET_OPER_POL_T.MtoDetOper%TYPE;
   nPrimaNeta      DET_OPER_POL_T.MtoDetOper%TYPE;
   cTipoRecaDcto   RECA_DCTO_CERT.TIPORECADCTO%TYPE;
   nSumRecaDctoPol RECA_DCTO_CERT.TIPORECADCTO%TYPE;
   nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;


  PROCEDURE FR_ELIMINAR_RECADCTO_PACTADA IS
  BEGIN

    FOR x IN ( SELECT idepol, numcert , numoper , fecmov , mtooper, tipoop
          ,mtooperanu , p_nMtoPrimaPactada nmtoprima
      FROM OPER_POL_T OP
      WHERE IdePol = p_nIdePol
      AND NOT EXISTS (SELECT 1
                      FROM   RECA_DCTO_CERTIF
                      WHERE  OP.IdePol  = IdePol
                        AND  OP.NumCert = NumCert
                        AND  StsRecaDCto IN ('VAL','INC')) )
    LOOP
      BEGIN
        PR_RECA_DCTO_CERTIF.Elimina_Reca_Dcto_Pactada(p_nIdePol, x.NumCert);
      EXCEPTION
        WHEN OTHERS THEN
          p_cRetorno := '1,'||SQLERRM;

      END;
    END LOOP;

  END FR_ELIMINAR_RECADCTO_PACTADA;
  --
  PROCEDURE FR_ELIMINAR_RECADCTO_CERTIF IS
  BEGIN

    FOR x IN ( SELECT idepol, numcert , numoper, fecmov
          ,mtooper, tipoop, mtooperanu, p_nMtoPrimaPactada nmtoprima
      FROM OPER_POL_T OP
      WHERE IdePol = p_nIdePol
      AND NOT EXISTS (SELECT 1
                      FROM   RECA_DCTO_CERTIF
                      WHERE  OP.IdePol  = IdePol
                        AND  OP.NumCert = NumCert
                        AND  StsRecaDCto IN ('VAL','INC')) )

    LOOP

      BEGIN
        PR_RECA_DCTO_CERTIF.Elimina_Reca_Dcto_Certif(p_nIdePol, x.NumCert);
      EXCEPTION
        WHEN OTHERS THEN
           p_cRetorno := '1,'||SQLERRM;
      END;

    END LOOP;

  END FR_ELIMINAR_RECADCTO_CERTIF;


  BEGIN -- MAIN

    p_cRetorno := '0,OK';

    ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'PRIMA_PACTADA','',nSecuencia );

    DBMS_OUTPUT.PUT_LINE('Se proceder� a asignar los recargos / descuentos de acuerdo a la prima pactada. Los recargos � descuentos que haya definido para �sta operaci�n, ser�n eliminados');

    -- Elimina recargos y descuentos de la tabla RECA_DCTO_CERT
    FR_Eliminar_RecaDcto_Pactada;
    -- Elimina recargos y descuentos de la tabla RECA_DCTO_CERTIF
    FR_Eliminar_RecaDcto_Certif;
    --

    FOR x IN ( SELECT idepol
          ,numcert
          ,numoper
          ,fecmov
          ,mtooper
          ,tipoop
          ,mtooperanu
          ,p_nMtoPrimaPactada nMtoPrima
      FROM OPER_POL_T OP
      WHERE IdePol = p_nIdePol
      AND NOT EXISTS (SELECT 1
                      FROM   RECA_DCTO_CERTIF
                      WHERE  OP.IdePol  = IdePol
                        AND  OP.NumCert = NumCert
                        AND  StsRecaDCto IN ('VAL','INC')) )

    LOOP

      IF x.nMtoPrima > 0 THEN

        nSumRecaDcto   := PR_Det_Oper_Pol_T.Obtener_Monto_Total(p_nIdePol, x.NumCert, x.NumOper);
        nPrimaNeta     := NVL(x.MtoOper,0) - NVL(nSumRecaDcto,0);

        BEGIN
          SELECT TipoRecaDcto, NVL(SUM(MtoRecaDcto),0)
          INTO   cTipoRecaDcto, nSumRecaDctoPol
          FROM   RECA_DCTO_CERT
          WHERE  IdePol  = p_nIdePol
            AND  NumCert = x.NumCert
            AND  NumOper IS NULL
          GROUP BY TipoRecaDcto
          ;
        EXCEPTION
          WHEN OTHERS THEN
            nSumRecaDctoPol := 0;
            cTipoRecaDcto   := NULL;
        END;
        IF cTipoRecaDcto = 'R' THEN
          nResultado := nPrimaNeta - nSumRecaDctoPol;
        ELSIF cTipoRecaDcto = 'D' THEN
          nResultado := nPrimaNeta + nSumRecaDctoPol;
        ELSIF cTipoRecaDcto IS NULL THEN
          nResultado := 0;
        END IF;

        IF x.nMtoPrima <> nResultado THEN
          BEGIN
            PR_Reca_Dcto_Certif.Reca_Dcto_Prima_Pactada(p_nIdePol, x.NumCert, nPrimaNeta, x.nMtoPrima, p_cCodReca, p_cCodDcto);
            -- <I N3020793> Nestor Vergaray / 26-11-2004 / CFT-054
            IF x.nMtoPrima > x.MtoOper THEN -- Recargo
              PR_Reca_Dcto_Certif.Act_Mtos_Reca_Dcto_Cert(p_nIdePol, x.NumCert,'R',p_cCodReca, x.MtoOper, x.nMtoPrima);
            ELSE
              --<I N3041960> Aldo Teixeira / 11-02-2014 / Error en CodRecaDcto al aplicar descuento mediante Prima Pactada
              --PR_Reca_Dcto_Certif.Act_Mtos_Reca_Dcto_Cert(:C00.nIdePol,:B02_1.NumCert,'D',:C02.cCodReca,:B02_1.MtoOper,:B02_1.nMtoPrima);
              PR_Reca_Dcto_Certif.Act_Mtos_Reca_Dcto_Cert(p_nIdePol, x.NumCert,'D', p_cCodDcto, x.MtoOper, x.nMtoPrima);
              --<F N3041960>
            END IF;
            -- <F N3020793>
          EXCEPTION
            WHEN OTHERS THEN
              p_cError := '1,'||'Error - ' || SQLERRM;
          END;
        END IF;
      END IF; -- x.nMtoPrima
    END LOOP;

    --
    BEGIN
        PR_Mov_Prima.Eliminar_Mov_T(p_nIdePol);
        nNumOper := PR_POLIZA.Activar(p_nIdePol, 'T');
    EXCEPTION
      WHEN OTHERS THEN
          p_cError := '1,'||'Error preactivaci�n - ' || SQLERRM;
    END;

    ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'PRIMA_PACTADA','',nSecuencia );

    p_cError := '0,OK';

  EXCEPTION
    WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;
          ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR',SYSDATE,SYSDATE,'PRIMA_PACTADA','',nSecuencia );
          --ROLLBACK;

  END PRIMA_PACTADA;


FUNCTION DEV_CODPLAN_MODPLAN ( p_nIdePol POLIZA.IDEPOL%TYPE,
--<I RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
                               p_nNumCert Certificado.NumCert%TYPE DEFAULT 1 )
                               RETURN VARCHAR2 IS
--<F RTC 316878>
  cPlan VARCHAR2(9);

BEGIN
  BEGIN
    SELECT CODPLAN ||MODPLAN
    INTO cPlan
    FROM cond_financiamiento
    WHERE idepol = p_nIdePol
      AND NumCert = p_nNumCert  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
    AND ROWNUM < 2
    ;
  EXCEPTION
    WHEN OTHERS THEN
       cPlan := '';
  END;

  RETURN cPlan;

END DEV_CODPLAN_MODPLAN
;


FUNCTION DEV_TIPOCLI ( p_cCodPlan PLAN_FINANCIAMIENTO.CODPLAN%TYPE,
                       p_cModPlan PLAN_FINANCIAMIENTO.MODPLAN%TYPE ) RETURN VARCHAR2 IS

vTipoCli PLAN_FINANCIAMIENTO.TIPOCLI%TYPE;

BEGIN

  BEGIN
    SELECT TIPOCLI
    INTO vTipoCli
    FROM PLAN_FINANCIAMIENTO
    WHERE CODPLAN = p_cCodPlan
    AND MODPLAN   = p_cModPlan
    ;
  EXCEPTION
    WHEN OTHERS THEN
       vTipoCli := '';
  END;

  RETURN vTipoCli;

END DEV_TIPOCLI;


/*-----------------------------------------------------------------------
   Nombre : DEV_RESP_PAGO
   Proposito: Devolver lleno el TDU_TABLA_RESP_PAGO para ser enviado en la Activaci�n de Poliza.
   Referencia :
   Parametro :  p_nIdePol   Numero Interno de poliza de seguros.
                p_nNumCert  N�mero de Certificado

   Retorno :   p_cError         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
               0, OK
               1, Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   30/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
   29/08/2022   Jorge Benel       <RTC 313349> Lee el responsable de pago del TDU p_TABLA_RESP_PAGO.
   24/05/2023   Christian Bayona  <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
-----------------------------------------------------------------------------*/

PROCEDURE DEV_RESP_PAGO ( p_nIdePol         IN POLIZA.IDEPOL%TYPE,
                          p_nNumCert        IN Certificado.NumCert%TYPE DEFAULT 1,  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
                          p_TABLA_RESP_PAGO OUT ACSELX.TDU_TABLA_RESP_PAGO,
                          p_cError          OUT VARCHAR2
                        )      AS

  cPlan                  VARCHAR2(9);
  cCodPlan               COND_FINANCIAMIENTO.CODPLAN%TYPE;
  cModPlan               COND_FINANCIAMIENTO.MODPLAN%TYPE;
  vTipoCli               PLAN_FINANCIAMIENTO.TIPOCLI%TYPE;
  cNumID                 CLIENTE.NUMID%TYPE ;
  i                      NUMBER(5);
  --<I RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
  cCodPlanIni            Cond_Financiamiento.CodPlan%TYPE;
  cModPlanIni            Cond_Financiamiento.ModPlan%TYPE;
  --<F RTC 316878>

BEGIN -- main

   p_cError := '0,OK';

   DEV_RESP_PAGO.p_TABLA_RESP_PAGO := acselx.TDU_TABLA_RESP_PAGO();

   cPlan := DEV_CODPLAN_MODPLAN ( p_nIdePol => p_nIdePol,
                                  p_nNumCert => p_nNumCert );  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B

   SELECT SUBSTR(cPlan,1,6), SUBSTR(cPlan,7,3)
   INTO cCodPlan, cModPlan
   FROM DUAL;

   --<I RTC 316878> / Christian Bayona / 19-04-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
   --Guardamos el plan obtenido
   cCodPlanIni := cCodPlan;
   cModPlanIni := cModPlan;
   --<F RTC 316878>

   dbms_output.put_line(cCodPlan ||','|| cModPlan);

   vTipoCli := DEV_TIPOCLI ( p_cCodPlan => cCodPlan,
                             p_cModPlan => cModPlan ) ;

   dbms_output.put_line(vTipoCli);

   --<I RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
   /* --Se comenta este c�digo observaci�n de est�ndares
   -- Lee el plan a usar
   IF vTipoCli = 'P' THEN   -- CONTADO PERSONA NATURAL
     cCodPlan := 'CON001';
     cModPlan := '0';
   ELSIF vTipoCli = 'E' THEN  -- CONTADO PERSONA JURIDICA
     cCodPlan := 'CON002';
     cModPlan := '0';
   ELSIF vTipoCli = 'G' THEN     -- CONTADO PERSONA JURIDICA PUBLICA
     cCodPlan := 'CON017';
     cModPlan := '0';
   ELSE
     cCodPlan := 'CON017';
     cModPlan := '0';
   END IF;
   */

   SELECT COUNT(1)
   INTO i
   FROM Plan_Financiamiento_Table
   WHERE CodPlan = cCodPlanIni
     AND ModPlan = cModPlanIni
     AND TipoFin = 'CON';
   --
   IF i > 0 THEN --Es plan al contado
     cCodPlan := cCodPlanIni;
     cModPlan := cModPlanIni;

   ELSE
     SELECT MIN(CodPlan), MIN(ModPlan)
     INTO cCodPlan, cModPlan
     FROM Plan_Financiamiento_Table
     WHERE StsPlan = 'ACT'
       AND TipoFin = 'CON'
       AND TipoCli = vTipoCli;
   END IF;
   --<F RTC 316878>

   -- lee el numid del responsable de pago
   SELECT NUMID
   INTO cNumID
   FROM CLIENTE
   WHERE CODCLI = (SELECT codcli
                   FROM POLIZA
                   WHERE  IdePol = p_nIdePol ) ;

   dbms_output.put_line(cNumID);

   i := 0;

   FOR c1 IN ( SELECT *
              FROM RESP_PAGO
              WHERE IdePol   = p_nIdePol
              --AND   NumCert  = 1 ) LOOP  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
              AND   NumCert  = p_nNumCert ) LOOP  --<RTC 316878> / Christian Bayona / 24-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B

     i := i + 1;

             DEV_RESP_PAGO.p_TABLA_RESP_PAGO.EXTEND(1);

             DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i) := acselx.TDU_RESP_PAGO(   IdePol              => NULL ,
                                                                                 Numcert             => NULL ,
                                                                                 NumIdPerRespPago    => NULL ,
                                                                                 PorcPago            => NULL ,
                                                                                 CodPlanFracc        => NULL ,
                                                                                 ModPlanFracc        => NULL ,
                                                                                 NumdocRef           => NULL ,
                                                                                 CanCuotas           => NULL ,
                                                                                 TipoRedondeo        => NULL ,
                                                                                 CodAgencia          => NULL ,
                                                                                 CodPais             => NULL ,
                                                                                 CodEstado           => NULL ,
                                                                                 CodCiudad           => NULL ,
                                                                                 CodMunicipio        => NULL ,
                                                                                 Direc               => NULL ,
                                                                                 Telef1              => NULL ,
                                                                                 Telef2              => NULL ,
                                                                                 Telef3              => NULL ,
                                                                                 TeleX               => NULL ,
                                                                                 Zip                 => NULL ,
                                                                                 CodZonaPostal       => NULL ,
                                                                                 CodRuta             => NULL ,
                                                                                 FormFact            => NULL ,
                                                                                 NumComprob          => NULL ,
                                                                                 tippagfac           => NULL ,
                                                                                 txtTitular          => NULL ,
                                                                                 tipdoccargo         => NULL ,
                                                                                 NumIdEntFinanc      => NULL ,
                                                                                 fecvenc             => NULL ,
                                                                                 codmoneda           => NULL ,
                                                                                 codriesgo           => NULL ,
                                                                                 fecprimervenc       => NULL ,
                                                                                 TXTADICIONAL        => NULL

                                                                           ) ;

          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Idepol             := c1.idepol  ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Numcert            := c1.NumCert ;
          -- <I RTC-313349>  Jorge Benel / 29-08-2022 / Lee el responsable de pago del TDU p_TABLA_RESP_PAGO.
          -- DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).NumIdPerRespPago   := cNumID;
          -- DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).PorcPago           := 100.0000;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).NumIdPerRespPago   := c1.codcli;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).PorcPago           := c1.PorcPago;
          -- <F RTC-313349> Jorge Benel / 29-08-2022
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodPlanFracc        := cCodPlan ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).ModPlanFracc        := cModPlan ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).NumdocRef           := c1.NumdocRef ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CanCuotas           := 1 ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).TipoRedondeo        := c1.tipored ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodAgencia          := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodPais             := c1.codpais;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodEstado           := c1.codestado;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodCiudad           := c1.codciudad;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodMunicipio        := c1.codmunicipio;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Direc               := c1.direc ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Telef1              := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Telef2              := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Telef3              := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).TeleX               := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).Zip                 := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodZonaPostal       := c1.CodZonaPostal ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).CodRuta             := c1.CodRuta ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).FormFact            := c1.FormFact ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).NumComprob          := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).tippagfac           := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).txtTitular          := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).tipdoccargo         := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).NumIdEntFinanc      := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).fecvenc             := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).codmoneda           := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).codriesgo           := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).fecprimervenc       := NULL ;
          DEV_RESP_PAGO.p_TABLA_RESP_PAGO(i).TXTADICIONAL        := NULL ;

   END LOOP;

 EXCEPTION
    WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;

END DEV_RESP_PAGO;


/*-----------------------------------------------------------------------
   Nombre : PRORROGA_VIGENCIA
   Proposito: Ejecuta el endoso de prorroga de vigencia.
   Referencia : pr_interfase_ax_sas.prorrogar_poliza
   Parametro :  p_nIdePol   Numero Interno de poliza de seguros.
                p_cNumTramite  Numero de tramite.
                p_dfecfinvig   fecha de la nueva vigencia.
                p_cCodUsuario  codigo de usuario de auditoria.
   Retorno :    p_cError         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
               0, OK
               1, Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/03/2022   Jorge Benel       <RTC 313089> Creaci�n.
   23/03/2022   Jorge Benel       <RTC 313089> actualizaci�n para pruebas en TEST.
-----------------------------------------------------------------------------*/

PROCEDURE PRORROGA_VIGENCIA ( p_nIdePol               IN Poliza.Idepol%TYPE,
                              p_cNumTramite           IN POLIZA_TRAMITE.NUMTRAMITE%TYPE,
                              p_dfecfinvig            IN POLIZA.FECFINVIG%TYPE,
                              p_cCodUsuario           IN VARCHAR2,
                              p_cError                OUT varchar2

  ) AS

  v_cerror                 VARCHAR2(500);

  cStsPol         poliza.stspol%type;
  nNumPol         POLIZA.NUMPOL%TYPE;
  cCodProd        POLIZA.CODPROD%TYPE;
  dFecIniVigPol   poliza.fecinivig%type;
  dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
  bExistePoliza   BOOLEAN := TRUE;
  nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;

 PROCEDURE VALIDA AS
   v NUMBER(3);
 BEGIN
   -- Obtenemos informacion de la poliza
   bExistePoliza  := ACSELX.PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );

 END VALIDA;

BEGIN  -- main

  p_cError := '0,OK';

  ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'PRORROGA_VIGENCIA','',nSecuencia );

  VALIDA;

  pr_interfase_ax_sas.prorrogar_poliza(p_nidepol     => PRORROGA_VIGENCIA.p_nIdePol,
                                       p_cnumtramite => PRORROGA_VIGENCIA.p_cnumtramite,
                                       p_dfecfinvig  => PRORROGA_VIGENCIA.p_dfecfinvig,
                                       p_ccodusuario => PRORROGA_VIGENCIA.p_ccodusuario,
                                       p_cerror      => p_cError );

 EXCEPTION
    WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;
          ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR',SYSDATE,SYSDATE,'PRORROGA_VIGENCIA','',nSecuencia ); -- <N3041576 > 02.10.2013
          --ROLLBACK;
END PRORROGA_VIGENCIA;

-- <INI RTC 313092>

/*-----------------------------------------------------------------------------
Nombre      : ACT_NUMOPER_AUTORIZA_VIOLACION
Proposito   : Actualizar la tabla AUTORIZA_VIOLACION con el NUMOPER generado al activar la poliza
Referencias :
Parametros  : p_nIdePol         Identificador de la poliza
              p_nNumOper        Numero de operaci�n

LOG de CAMBIO!
Fecha           Autor                     Descripcion
29/04/2022      Jorge Benel               <RTC 313346> Creaci�n
-------------------------------------------------------------------------------*/

 PROCEDURE ACT_NUMOPER_AUTORIZA_VIOLACION(p_nIdepol POLIZA.IDEPOL%TYPE, p_nNumOper OPER_POL.NUMOPER%TYPE) AS
 BEGIN
   UPDATE AUTORIZA_VIOLACION
   SET    NumOper   = p_nNumOper
   WHERE  IdePol    = p_nIdepol
   AND  NumOper IS NULL
   ;
 EXCEPTION
   WHEN OTHERS THEN
     NULL;
 END ACT_NUMOPER_AUTORIZA_VIOLACION;


/*-----------------------------------------------------------------------------
Nombre      : FR_MOVIMIENTO_SALIDA
Proposito   : Genera Clausulas de Certificado/Ramo + Criterios
Referencias : form              TRAMITE.FMB
              program unit      FR_MOVIMIENTO_SALIDA
Parametros  : p_nIdePol         Identificador de la poliza
              p_nNumCert        Numero del certificado
              p_cCodRamoCert    Codigo de Ramo/Certificado
              p_cCodPlan        Codigo del plan
              p_cRevPlan        Revision del plan
              p_dFecIniVig      Fecha Inicio de Vigencia de Endoso
              p_dFecFinVig      Fecha Fin    de Vigencia de Endoso
              p_cCodProd        Codigo de Producto
              p_nIndModCobert   Indicador de modificaci�n de Cobertura
LOG de CAMBIO!
Fecha        Autor                        Descripcion
2022/03      Ronald Barreda               Creaci�n
-------------------------------------------------------------------------------*/
PROCEDURE FR_MOVIMIENTO_SALIDA(
  p_nIdePol         POLIZA.IdePol%TYPE,
  p_nNumCert        CERT_RAMO.NumCert%TYPE,
  p_cCodRamoCert    CERT_RAMO.CodRamoCert%TYPE,
  p_cCodPlan        CERT_RAMO.CodPlan%TYPE,
  p_cCodRevPlan     CERT_RAMO.RevPlan%TYPE,
  p_dFecIniVig      DATE,
  p_dFecFinVig      DATE,
  p_cCodProd        POLIZA.CodProd%TYPE,
  p_nIndModCobert   NUMBER
) IS

nError         NUMBER;
nRetButton1    NUMBER;
nExiste        PLS_INTEGER;
nIndDeducible  NUMBER;
--
BEGIN
 dbms_output.put_line('~ ~ ~ ~ ~ BEGIN ! FR_MOVIMIENTO_SALIDA');

 nError := 80;
 IF p_nIndModCobert = 1 THEN
   dbms_output.put_line('IF p_nIndModCobert = 1 THEN ');
   dbms_output.put_line('antes PR_COBERT_CERT.COBERT_AGRUPADAS');
   PR_COBERT_CERT.COBERT_AGRUPADAS(p_nIdePol, p_nNumCert, p_cCodPlan,
                                   p_cCodRevPlan, p_cCodRamoCert, p_cCodProd);

 END IF;
 BEGIN
   DELETE FROM MOD_CLAU_CERT
    WHERE IdePol      = p_nIdePol
      AND NumCert     = p_nNumCert
      AND CodRamoCert = p_cCodRamoCert
      AND NumOper IS NULL;
  dbms_output.put_line('sql%rowcount:'||to_char(sql%rowcount));
 EXCEPTION
   WHEN OTHERS THEN
     --FR_ERROR('FRM-TARIFA Error al invocar MOD_CLAU_CERT - '|| SQLERRM) ;
     RAISE_APPLICATION_ERROR(-20101, 'RAE!!! FRM-TARIFA Error al invocar MOD_CLAU_CERT '|| SQLERRM) ;
 END;
 -- Fin Modificacion
 nError := 90;
 IF p_nIndModCobert = 1 THEN
  dbms_output.put_line('IF p_nIndModCobert = 1 THEN');
  dbms_output.put_line('antes PR_CERT_RAMO.GENERAR_CLAUSULAS_ASOCIADAS');
  PR_CERT_RAMO.GENERAR_CLAUSULAS_ASOCIADAS(p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan,
                p_cCodRevPlan, p_cCodRamoCert, p_dFecIniVig, p_dFecFinVig);
 END IF;
 --FR_MESSAGE('Actualizando ramos...');
 -- Inserta o Actualiza cuando no existen valores en los otros ramos
 nError := 100;
  IF p_cCodProd = '2001' THEN
    dbms_output.put_line('IF p_cCodProd = 2001 THEN');
    BEGIN
     SELECT 1
     INTO   nExiste
     FROM   PLAN_PROD
     WHERE  CodProd = p_cCodProd
       AND  CodPlan = p_cCodPlan
       AND  DescPlanProd LIKE 'COLECTIVAS%';
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
         nExiste:=0;
       WHEN TOO_MANY_ROWS THEN
         nExiste:=1;
    END ;
  END IF;
  IF nExiste=1 THEN
    dbms_output.put_line('PR_CRITERIO.CRITERIO_MULTIRAMO A');
    PR_CRITERIO.CRITERIO_MULTIRAMO(p_nIdePol, p_nNumCert, p_cCodPlan, p_cCodRevPlan,
                p_cCodRamoCert, p_cCodProd, 'N', nIndDeducible);
  ELSE
    IF p_nIndModCobert = 1 THEN
    dbms_output.put_line('PR_CRITERIO.CRITERIO_MULTIRAMO B');
    PR_CRITERIO.CRITERIO_MULTIRAMO(p_nIdePol, p_nNumCert, p_cCodPlan, p_cCodRevPlan,
                                     p_cCodRamoCert, p_cCodProd, 'S', nIndDeducible);
    ELSE
    dbms_output.put_line('PR_CRITERIO.CRITERIO_MULTIRAMO C');
    PR_CRITERIO.CRITERIO_MULTIRAMO(p_nIdePol, p_nNumCert, p_cCodPlan, p_cCodRevPlan,
                                     p_cCodRamoCert, p_cCodProd, 'N', nIndDeducible);
    END IF;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20101, 'RAE!!! FR_MOVIMIENTO_SALIDA ' || nError || ' : ' ||SQLERRM);
END FR_MOVIMIENTO_SALIDA;

/*-----------------------------------------------------------------------------
Nombre      : FR_GENERA_ENDOSO
Proposito   : Generar Endoso
Referencias : form              TRAMITE.FMB
              program unit      FR_GENERA_ENDOSO
Parametros  : p_nIdePol         Identificador de la poliza
              p_nNumCert        Numero del certificado
              p_cCodRamoCert    Codigo de Ramo/Certificado
              p_cCodPlan        Codigo del plan
              p_cRevPlan        Revision del plan
              p_dFecIniVig      Fecha Inicio de Vigencia de Endoso
              p_dFecFinVig      Fecha Fin    de Vigencia de Endoso
              p_cEndoso         Indicador de Endoso (S:SI / N:NO)
              p_cCodProd        Codigo de Producto
LOG de CAMBIO!
Fecha        Autor                        Descripcion
2022/03      Ronald Barreda               Creaci�n
20/01/2023   Christian Bayona             Se modifica indicador
-------------------------------------------------------------------------------*/
PROCEDURE FR_GENERA_ENDOSO (
  p_nIdePol          POLIZA.IdePol%TYPE,
  p_nNumCert        CERT_RAMO.NumCert%TYPE,
  p_cCodRamoCert    CERT_RAMO.CodRamoCert%TYPE,
  p_cCodPlan        CERT_RAMO.CodPlan%TYPE,
  p_cCodRevPlan      CERT_RAMO.RevPlan%TYPE,
  p_dFecIniVig      DATE,
  p_dFecFinVig      DATE,
  p_cEndoso         CHAR,
  p_cCodProd        POLIZA.CodProd%TYPE
)  IS

 nExiste          NUMBER;
 nError           NUMBER;
 nNumOp           NUMBER;
 nRetButton1      NUMBER;
 nRetButton2      NUMBER;
 cExistePoliMasi  CHAR;

 cExisteDeducible CHAR;
 cGenCobOblig     CHAR;

 NEXISTEUSU       NUMBER;
 NSINIDAT         NUMBER;

 nIndRecaDcto     NUMBER;
 cStsCert         NUMBER;
 nIndModCobert    NUMBER;

 dFecIniVig       DATE;
 dFecFinVig       DATE;

 nIndDeducible    NUMBER;
BEGIN
  dbms_output.put_line('~ ~ ~ ~ ~ BEGIN ! FR_GENERA_ENDOSO');
  IF p_cEndoso = 'S' THEN -- Se realizo cambios y fue por endoso.
    -- FR_MESSAGE('Generando endoso...');
  dbms_output.put_line('IF p_cEndoso = S THEN');
    -- Se determina si solo hay un ramo o varios
     SELECT COUNT(1)
       INTO nExiste
       FROM DUAL
      WHERE EXISTS ( SELECT 1
                       FROM MOD_COBERT
                       WHERE IdePol    = p_nIdePol
                        AND NumCert   = p_nNumCert
                        AND StsModCobert = 'INC' );
    dbms_output.put_line('nExiste:'||to_char(nExiste));
    --IF EXISTE_MOD_COBERT (p_nIdePol, p_nNumCert ) = 'S' THEN
    --  nExiste := 1 ;
    --ELSE
    --  nExiste := 0 ;
    --END IF;
  ELSIF p_cEndoso = 'N' THEN -- Se realizo cambios y No fue por endoso.
    --FR_MESSAGE('Generando movimiento inicial...');
  dbms_output.put_line('IF p_cEndoso = N THEN');
    nExiste := 1 ;
  END IF;

  -- Si nExiste = 0 , Se genera endoso sin movimiento.
  nError := 50;
  dbms_output.put_line('go PR_MOD_DATOS_PARTICULARES');
  PR_MOD_DATOS_PARTICULARES.CREAR(p_nIdePol, p_nNumCert, p_cCodPlan, p_cCodRevPlan,
                            p_cCodRamoCert, p_dFECINIVIG, p_dFecFinVig, nExiste);

  IF nIndRecaDcto = 1 AND cStsCert <> 'VAL' THEN
--    nRetButton1 := FR_CONFBOX ('Se Modificar�n las coberturas de acuerdo al tarifario?. El proceso es IRREVERSIBLE.');
  dbms_output.put_line('IF nIndRecaDcto = 1 AND cStsCert <> VAL THEN');
    --IF nRetButton1 = ALERT_BUTTON1 THEN
      nIndModCobert := 1;
    --ELSIF nRetButton1 = ALERT_BUTTON2 THEN
    --  nIndModCobert := 0;
    --END IF;
  END IF;
  nIndModCobert := 1;  --<RTC-316106> / CBayona / 20-01-2023 / Se establece el indicador de Mod Cobert en 1

  IF p_cCodPlan != '900' THEN  -- Plan de Migraci�n
    nError := 60;
  dbms_output.put_line('IF p_cCodPlan != 900 THEN');
    IF p_dFecIniVig IS NULL OR p_dFecFinVig IS NULL THEN
    dbms_output.put_line('IF p_dFecIniVig IS NULL OR p_dFecFinVig IS NULL THEN');
      BEGIN
        SELECT FecIniValid,FecFinValid
        INTO   dFecIniVig, dFecFinVig
        FROM   MOD_COBERT
        WHERE  IdePol = p_nIdePol
          AND  NumCert = p_nNumCert
          AND  StsModCobert = 'INC'
          AND  NumMod = (SELECT NVL(MAX(NumMod),0)
                         FROM   MOD_COBERT
                         WHERE  IdePol = p_nIdePol
                           AND  NumCert = p_nNumCert
                           AND  StsModCobert = 'INC')
          AND ROWNUM = 1;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          dFecIniVig := NULL;
          dFecFinVig := NULL;
      END;
    dbms_output.put_line('dFecIniVig:'||to_char(dFecIniVig,'yyyy mm dd'));
    dbms_output.put_line('dFecFinVig:'||to_char(dFecFinVig,'yyyy mm dd'));

    END IF;

    IF nIndModCobert = 1 THEN
    dbms_output.put_line('IF nIndModCobert = 1 THEN');
      PR_MOD_COBERT.DATOS_PARTICULARES(p_nIdePol,p_nNumCert,p_cCodRamoCert,
                                       p_cCodPlan, p_cCodRevPlan, p_dFecIniVig, p_dFecFinVig);
    END IF;
  END IF;
  IF p_cEndoso = 'S' THEN
    dbms_output.put_line('IF p_cEndoso = S THEN');
    IF NEXISTEUSU = 1 AND NSINIDAT = 1 THEN
  dbms_output.put_line('IF NEXISTEUSU = 1 AND NSINIDAT = 1 THEN');
      nNumOp := PR_OPER_POL.ULTIMA_OPERACION (p_nIdePol, p_nNumCert) ;
    dbms_output.put_line('nNumOp:'||nNumOp);
      BEGIN
      dbms_output.put_line('antes INSERT INTO TEXTO_ENDOSOS');
        INSERT INTO TEXTO_ENDOSOS(IDEPOL, NUMCERT, NUMOPER, TIPO_ENDOSO, FEC_ENDOSO, DESC_CAMBIO)
             VALUES (p_nIdePol, p_nNumCert, nNumOp, 'ESV', SYSDATE, 'Cambio de Placa Realizado por Siniestros.');
    dbms_output.put_line('sql%rowcount:'||to_char(sql%rowcount));
      EXCEPTION
        WHEN OTHERS THEN
          --FR_ERROR('ERROR FRM-TARIFA Insertar TEXTO_ENDOSOS - '|| SQLERRM);
          RAISE_APPLICATION_ERROR(-20101, 'RAE!!! ERROR FRM-TARIFA Insertar TEXTO_ENDOSOS - '|| SQLERRM);
      END;
    END IF;
  ELSIF p_cEndoso = 'N' THEN
  dbms_output.put_line('IF p_cEndoso = N THEN');
    cExistePoliMasi := PR_POLIZA_MASIVA.EXISTE_POLI_MASI (p_nIdePol) ;
  dbms_output.put_line('cExistePoliMasi:'||cExistePoliMasi);
    IF cExistePoliMasi = 'S' THEN
    dbms_output.put_line('IF cExistePoliMasi = S THEN');
      --cExisteDeducible := EXISTE_DEDUCIBLE_POLIZA (p_nIdePol);
      SELECT COUNT(1)
      INTO cExisteDeducible
      FROM DUAL
      WHERE EXISTS ( SELECT 1
                      FROM DEDUCIBLE_COBERT D, DEDUCIBLE_COBERT_GRUPO DC
                     WHERE DC.IdeCobert IN (SELECT IdeCobert
                                              FROM COBERT_CERT
                                             WHERE IdePol = p_nIdePol
                                             UNION
                                            SELECT DISTINCT CB.IdeCobert
                                              FROM COBERT_BIEN CB, BIEN_CERT BC
                                             WHERE BC.IdePol  = p_nIdePol
                                               AND BC.IdeBien = CB.IdeBien)
                       AND DC.IdeCobert   = D.IdeCobert
                       AND DC.NumGrupoDed = D.NumGrupoDed
                       AND DC.StsMod      IN ('VAL','INC'));
    dbms_output.put_line('cExisteDeducible:'||cExisteDeducible);
    END IF;
    IF cExistePoliMasi = 'S' AND cExisteDeducible = 'S' THEN
    dbms_output.put_line('IF cExistePoliMasi = S AND cExisteDeducible = S THEN');
      --nRetButton1 := SHOW_ALERT('Alert_Deducible') ;
      --IF nRetButton1 = ALERT_BUTTON2 THEN -- Deducibles del Tarificador Dinamico ....
      nRetButton1 := 1;
      --
      IF nRetButton1 = 2 THEN
--        nRetButton2 := FR_CONFBOX ('Se reasignar�n los deducibles a las coberturas. Se perder�n los deducibles asignados en la emisi�n masiva. El proceso es IRREVERSIBLE.') ;
        nRetButton2 := 1;
        --IF nRetButton2 = ALERT_BUTTON1 THEN
        IF nRetButton2 = 1 THEN
          nIndDeducible := 0 ;
          cGenCobOblig := 'S' ;
        --ELSIF nRetButton2 = ALERT_BUTTON2 THEN
        ELSIF nRetButton2 = 2 THEN
          cGenCobOblig := 'N' ;
        END IF;
      --ELSIF nRetButton1 = ALERT_BUTTON1 THEN -- Deducibles de la Carga Masiva
      ELSIF nRetButton1 = 1 THEN -- Deducibles de la Carga Masiva
        nIndDeducible := 1 ;
         cGenCobOblig := 'S' ;
      --ELSIF nRetButton1 = ALERT_BUTTON3 THEN -- Cancelar
      ELSIF nRetButton1 = 3 THEN -- Cancelar
        cGenCobOblig := 'N' ;
      END IF;
    ELSE
--      IF EXISTE_DEDUCIBLE_COBERT (p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan, p_cCodRevPlan, p_cCodRamoCert ) = ' S' THEN
--        nIndDeducible := 1 ;
--      ELSE
--        nIndDeducible := 0 ;
--      END IF ;
     SELECT SIGN(COUNT(1))
       INTO nIndDeducible
       FROM DUAL
      WHERE EXISTS ( SELECT 1
                       FROM DEDUCIBLE_COBERT D, DEDUCIBLE_COBERT_GRUPO DC
                      WHERE DC.IdeCobert IN (SELECT IdeCobert
                                                 FROM COBERT_CERT
                                                WHERE IdePol      = p_nIdePol
                                                  AND NumCert     = p_nNumCert
                                                  AND CodRamoCert = p_cCodRamoCert
                                             UNION
                                             SELECT DISTINCT CB.IdeCobert
                                                FROM COBERT_BIEN CB, BIEN_CERT BC
                                               WHERE BC.IdeBien     = CB.IdeBien
                                                 AND BC.IdePol      = p_nIdePol
                                                 AND BC.NumCert     = p_nNumCert
                                                 AND BC.CodRamoCert = p_cCodRamoCert)
                        AND DC.IdeCobert   = D.IdeCobert
                        AND DC.NumGrupoDed = D.NumGrupoDed
                        AND DC.StsMod      IN ('VAL','INC'));

    dbms_output.put_line('nIndDeducible:'||TO_CHAR(nIndDeducible));
      cGenCobOblig := 'S';

      IF nIndModCobert = 1 THEN
        cGenCobOblig := 'S';
      ELSE
        cGenCobOblig := 'N';
      END IF;

    END IF;
    IF cGenCobOblig = 'S' THEN
      dbms_output.put_line('IF cGenCobOblig = S THEN');
      nError := 70;
      dbms_output.put_line('antes PR_CERT_RAMO.GENERAR_COBERTURA_OBLIG');
        PR_CERT_RAMO.GENERAR_COBERTURA_OBLIG(p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodProd,
                                             p_cCodPlan, p_cCodRevPlan, nIndDeducible,NULL,NULL,NULL);
    END IF;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20101, 'RAE!!! FR_GENERA_ENDOSO ' || nError || ' : ' || SQLERRM);
END FR_GENERA_ENDOSO;

/*-----------------------------------------------------------------------------
Nombre      : FR_KEY_EXIT
Proposito   : Generar Endoso
Referencias : form              TRAMITE.FMB
              trigger form      KEY_EXIT
Parametros  : p_nIdePol               Identificador de la poliza
              p_nNumCert              Numero del certificado
              p_cCodRamoCert          Codigo de Ramo/Certificado
              p_cCodPlan              Codigo del plan
              p_cRevPlan              Revision del plan
              p_dFecIniVig            Fecha Inicio de Vigencia de Endoso
              p_dFecFinVig            Fecha Fin    de Vigencia de Endoso
              p_TABLA_DATOS_PARTIC    Array de Datos Particulares
              p_cCodProd              Codigo de Producto
              p_nNumSesion            Numero de Sesion para T_DATOS_PARTICULARES
              -- OUT --
              p_cError                Codigo de Error en caso se encuentre un error
                                      0 : OK  /  1 : ERROR
LOG de CAMBIO!
Fecha        Autor                        Descripcion
2022/03      Ronald Barreda               Creaci�n
-------------------------------------------------------------------------------*/
PROCEDURE FR_KEY_EXIT(
  p_nIdePol               POLIZA.IdePol%TYPE,
  p_nNumCert              CERT_RAMO.NumCert%TYPE,
  p_cCodRamoCert          CERT_RAMO.CodRamoCert%TYPE,
  p_cCodPlan              CERT_RAMO.CodPlan%TYPE,
  p_cCodRevPlan           CERT_RAMO.RevPlan%TYPE,
  p_dFecIniVig            DATE,
  p_dFecFinVig            DATE,
  p_TABLA_DATOS_PARTIC    TDU_TABLA_DATOS_PARTIC,
  p_cCodProd              POLIZA.Codprod%TYPE,
  p_nNumSesion            VARCHAR
  ,
  p_cError                OUT varchar2
) IS
  cCodProd          POLIZA.CodProd%TYPE;
  cExisteMod       CHAR;
  cExisteOperAnul  CHAR;
  cCambioValor     CHAR;
  nError           NUMBER;
  nValor           NUMBER;
  existe_Mod       CHAR;
  nEdad             NUMBER := 0;
  nEdadMaxAsegu     NUMBER := 0;
  nFactor           NUMBER := 0;
  cCodFormaPago     CHAR;
  dFecNacAseg       DATE;
  cValor            RIM_TAB_GRAL.VALOREF3%TYPE;
  cValTrasf         DATOS_PARTICULARES.VALTRASF%TYPE;
  cIndVigLib       PLAN_PROD.INDVIGLIB%TYPE;
  cCodTrasf        VARCHAR2(4);
  nAnioFabr        NUMBER(4);
  nAnioAntig        NUMBER(4);

  nIndDeducible     NUMBER;
  cEndoso           VARCHAR2(1);
  nIndRecaDcto      NUMBER;
  nIndModCobert     NUMBER;
  nAnoAntiguedad    NUMBER;

  cMarca            VARCHAR2(50);
  cModelo           VARCHAR2(50);
  cClase            VARCHAR2(50);
  cTipo             VARCHAR2(1);
  cMoneda           VARCHAR2(3);
  nSumaAsegurada    NUMBER;

  nExiste           NUMBER;

   CURSOR  C_CLAUCERT  (p_cIdePol  ACSELX.CLAU_CERT.IDEPOL%TYPE, p_cNumCert ACSELX.CLAU_CERT.NUMCERT%TYPE) IS
   SELECT  C.CodClau, C.IdePol, C.NumCert, C.CodRamoCert, C.StsClau
   FROM    CLAU_CERT C
   WHERE   C.IDEPOL = p_cIdePol
   AND     C.NUMCERT = p_cNumCert;

BEGIN
  p_cError := '0,OK';
  --
  dbms_output.put_line('~ ~ ~ ~ ~ BEGIN ! FR_KEY_EXIT');
  SELECT MAX(CodProd) INTO cCodProd FROM POLIZA WHERE IdePol = p_nIdePol;
  dbms_output.put_line('cCodProd:'||cCodProd);

  -- VALIDACION SOLO SI ES VIGENCIA LIBRE
  BEGIN
    SELECT INDVIGLIB
    INTO cIndVigLib
    FROM PLAN_PROD
    WHERE CODPROD = cCodProd
      AND CODPLAN = p_cCodplan
      AND REVPLAN = p_cCodRevplan;
  EXCEPTION
    WHEN OTHERS THEN
       cIndVigLib := 'N';
  END;
  dbms_output.put_line('cIndVigLib:'||cIndVigLib);

  IF cIndVigLib = 'S' AND PR.BUSCA_LVAL('RENOVDPP',cCodProd || p_cCodplan || p_cCodRevplan) != 'INVALIDO' THEN
    dbms_output.put_line('cIndVigLib = S AND PR.BUSCA_LVAL(RENOVDPP,cCodProd || p_cCodplan || p_cCodRevplan) != INVALIDO');
    cIndVigLib := 'N';
    DECLARE
      --nIdepol    POLIZA.IDEPOL%TYPE;
      nExiste    NUMBER;
    BEGIN
--      SELECT IDEPOL INTO nIdePol FROM POLIZA WHERE CODPROD = :C01.CCODPROD AND NUMPOL = :C01.NNUMPOL;
      BEGIN
        SELECT 1 INTO nExiste FROM POLIZA_REVISION WHERE IDEPOL = p_nIdePol AND NUMSEC = 1;
      EXCEPTION WHEN OTHERS THEN
        nExiste := 0;
      END;
      IF nExiste = 0 THEN
      dbms_output.put_line('IF nExiste = 0 THEN ... antes INSERT');
        INSERT INTO POLIZA_REVISION VALUES (p_nIdePol, 1, p_dFecIniVig, Add_months(p_dFecIniVig, 12*50));
    dbms_output.put_line('SQL%ROWCOUNT:'||SQL%ROWCOUNT);
      END IF;
    EXCEPTION WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20101, 'RAE!!! Error en RENOVDPP: ' || nError || ' : ' ||SQLERRM);
    END;
  END IF;

--  cValTrasf   := LTRIM(RTRIM(:B01_4.ValTrasf));

  IF cIndVigLib = 'S' THEN
    -- NRO_VIGILANTES / VIGILANTES_3D -- Criterios
  dbms_output.put_line('IF cIndVigLib = S THEN');
    IF ACSELX.PR_RENOV_AUTOM.ES_CRITERIO_FECHA('NRO_VIGILANTES') = 'S'
      --AND :C01.CSTSCERT IN ('VAL','INC')
      THEN
      BEGIN
      dbms_output.put_line('antes ACSELX.PR_RENOV_AUTOM.VALIDA_FECHA_DATOS_PART(cValTrasf, cCodProd, p_cCodplan, p_cCodRevplan )');
        ACSELX.PR_RENOV_AUTOM.VALIDA_FECHA_DATOS_PART(cValTrasf, cCodProd, p_cCodplan, p_cCodRevplan );
      EXCEPTION
        WHEN OTHERS THEN
          RAISE_APPLICATION_ERROR(-20101, 'RAE!!! ' || SQLERRM);
      END;
    END IF;
  END IF;

  IF 'N' = 'S' THEN   --IF :GLOBAL.MOSTRAR = 'S' THEN
    RAISE_APPLICATION_ERROR(-20101, 'RAE!!! El registro de una Tasa menor'|| chr(10)||'a la Tasa M�nima del Producto,'|| chr(10)||'requiere de una autorizaci�n especial');
    --GO_BLOCK('AUTORIZA'); :GLOBAL.MOSTRAR := 'N'; :GLOBAL.CRITERIO := '';
    --LR_Error('Registrar un valor menor a la Tasa M�nima del Producto, requiere del ingreso de una clave');
  END IF;
  /*
  IF :GLOBAL.MUESTRA_MENSAJE = 'N' THEN
    IF EXISTE_CAMBIO_ALTORIESGO('MARCAS_DE_VEHICULOS') OR
       EXISTE_CAMBIO_ALTORIESGO('MODELOS_DE_VEHICULOS') OR
       EXISTE_CAMBIO_ALTORIESGO('ANOS_DE_FABRICACION')
       OR EXISTE_CAMBIO_ALTORIESGO('CLASES_DE_VEHICULOS')
       OR EXISTE_CAMBIO_ALTORIESGO('TIPOS_DE_VEHICULOS')
       OR EXISTE_CAMBIO_ALTORIESGO('SUMA_ASEGURADA')
       THEN
       existe_Mod := 'S';
  ELSE
    existe_Mod := 'N';
  END IF;
  */
  existe_Mod := 'N';

  --FR_GRABA_DATOS(nValor);  -- COMMIT_FORM
  nValor := 0; --NRBA
  IF nValor = 0 THEN
    IF PR_PRODUCTO.ES_NATIVO(cCodProd)='N' THEN
      RAISE_APPLICATION_ERROR(-20101, 'RAE!!! PRODUCTO ES NATIVO'); --EXIT_FORM(NO_VALIDATE);
    END IF;
    nIndDeducible := 1; --:C00.nIndDeducible := 1;
    --FR_CURSOR_OCUPADO; SYNCHRONIZE; FR_MESSAGE('Grabando...'); DO_KEY('COMMIT_FORM'); FR_MESSAGE('Evaluando cambios...');
    --cExisteOperAnul := EXISTE_OPER_ANUL (p_nIdePol, p_nNumCert);
       SELECT COUNT(1)
         INTO nExiste
         FROM DUAL
        WHERE EXISTS ( SELECT 1
                         FROM OPER_POL
                        WHERE IdePol  = p_nIdePol
                          AND NumCert = p_nNumCert
                          AND IndAnul IS NULL );
      --
    dbms_output.put_line('nExiste:'||nExiste);
     IF nExiste = 0 THEN cExisteOperAnul := 'N'; ELSE cExisteOperAnul := 'S'; END IF ;

    IF cExisteOperAnul = 'S' THEN
    dbms_output.put_line('IF cExisteOperAnul = S THEN');
        -- cExisteMod := EXISTE_ENDOSO_PENDIENTE (p_nIdePol); --NRBA
        SELECT NVL(MAX('S'),'N')
        INTO   cExisteMod
        FROM   DUAL
        WHERE  EXISTS (
                   SELECT NumCert
                      FROM CERTIFICADO
                     WHERE IdePol  = p_nIdePol
                       AND StsCert = 'MOD'
                    INTERSECT
                    SELECT DISTINCT NumCert
                      FROM MOD_DATOS_PARTICULARES
                     WHERE IdePol  = p_nIdePol
                       AND NumOper IS NULL) ;
    dbms_output.put_line('cExisteMod:'||cExisteMod);
    END IF;
    IF cExisteMod = 'S' THEN
      cEndoso := 'S';
    END IF;
--    cCambioValor := EVALUA_CAMBIO_CRITERIO ( p_nIdePol, p_nNumCert, p_cCodPlan,p_cRevPLan, p_cCodRamoCert, USERENV('SESSIONID'));

   SELECT DECODE ( SIGN( COUNT(1) ) , 1, 'S', 'N' )
   INTO   cCambioValor
   FROM   DUAL
   WHERE  EXISTS
                (SELECT CodCrit, CodTrasf, ValTrasf, NumOrd, ValCodTab
                    FROM DATOS_PARTICULARES
                   WHERE IdePol      = p_nIdePol
                     AND NumCert     = p_nNumCert
                     AND CodPlan     = p_cCodPlan
                     AND RevPlan     = p_cCodRevPlan
                     AND CodRamoCert = p_cCodRamoCert
                     AND CODCRIT    NOT IN ('GPS_INSTALADO_PROV','WEB_PROVEEDOR_GPS')
                 MINUS
                  SELECT CodCrit, CodTrasf, ValTrasf, NumOrd, ValCodTab
                    FROM T_DATOS_PARTICULARES
                   WHERE IdePol      = p_nIdePol
                     AND NumCert     = p_nNumCert
                     AND CodPlan     = p_cCodPlan
                     AND RevPlan     = p_cCodRevPlan
                     AND CodRamoCert = p_cCodRamoCert
                     AND CODCRIT     NOT IN ('GPS_INSTALADO_PROV','WEB_PROVEEDOR_GPS')
                     AND NumSesion   = p_nNumSesion) ;
    dbms_output.put_line('cCambioValor:'||cCambioValor);

    IF cCambioValor = 'S' THEN
      --:C00.nIndRecaDcto  := FR_EXISTE_RECA_DCTO(p_nIdePol,p_nNumCert); :C00.nIndModCobert := 1;
    dbms_output.put_line('IF cCambioValor = S THEN');
      SELECT COUNT(1)
      INTO   nIndRecaDcto
      FROM   (
        SELECT NumCert
        FROM   POLIZA_OPERACION
        WHERE  IdePol = p_nIdePol
          AND  CodAccion IN ('RECADCTO','PRIMAPACT')
          AND  NumCert = p_nNumCert
        UNION ALL
        SELECT NumCert
        FROM   RECA_DCTO_CERT
        WHERE  IdePol  = p_nIdePol
          AND  NumCert = p_nNumCert
        UNION ALL
        SELECT NumCert
        FROM   RECA_DCTO_CERTIF
        WHERE  IdePol  = p_nIdePol
          AND  NumCert = p_nNumCert);

    dbms_output.put_line('nIndRecaDcto:'||TO_CHAR(nIndRecaDcto));

      nIndModCobert := 1;

      dbms_output.put_line('go FR_GENERA_ENDOSO');
    FR_GENERA_ENDOSO (
        p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodPlan, p_cCodRevPlan,
        p_dFecIniVig, p_dFecFinVig,
        'S', --cEndoso,
        p_cCodProd);

      dbms_output.put_line('go FR_MOVIMIENTO_SALIDA'); --NRBA
    FR_MOVIMIENTO_SALIDA(
        p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodPlan, p_cCodRevPlan,
        p_dFecIniVig, p_dFecFinVig, p_cCodProd,
        1 -- nIndModCobert
        );

      --FR_SUMA_ASEG_MASIVA (p_nIdePol, p_nNumCert );

    END IF;

    nError := 110;
    BEGIN
    dbms_output.put_line('go PR_MOD_DATOS_PARTICULARES.BORRA_T_DATOS_PARTICULARES(USERENV(SESSIONID))');
      PR_MOD_DATOS_PARTICULARES.BORRA_T_DATOS_PARTICULARES(USERENV('SESSIONID'));
    EXCEPTION
    WHEN OTHERS THEN
      --STANDARD.ROLLBACK_NR; FR_CURSOR_NORMAL;
      --FR_MENSAJE('AL_ALERTA','Tarificaci�n ' || nError || ' : ' ||SQLERRM); EXIT_FORM(NO_VALIDATE);
      RAISE_APPLICATION_ERROR(-20101, 'RAE!!! Tarificaci�n ' || nError || ' : ' ||SQLERRM);
    END;

    IF existe_Mod = 'S' THEN
    dbms_output.put_line('IF existe_Mod = S THEN');
      BEGIN
        SELECT  d.CodTrasf
        INTO cCodTrasf
        FROM DATOS_PARTICULARES d
        WHERE d.IdePol = p_nIdePol
          AND d.NumCert = p_nNumCert
          AND d.CodPlan = p_cCodPlan
          AND d.RevPlan =p_cCodRevPlan
          AND d.CodRamoCert = 'VEHI'
          AND d.CodCrit = 'ANOS_DE_FABRICACION';
      EXCEPTION
        WHEN OTHERS THEN
          cCodTrasf:='000';
      END;
    dbms_output.put_line('cCodTrasf:'||cCodTrasf);

      BEGIN
        SELECT TO_NUMBER(DescTrasf)
        INTO nAnioFabr
        FROM TRANSFORMADOR
        WHERE CodCrit = 'ANOS_DE_FABRICACION'
          AND CodTrasf = cCodTrasf;
      EXCEPTION
        WHEN OTHERS THEN
          nAnioFabr:=NULL;
      END;
    dbms_output.put_line('nAnioFabr:'|| to_char(nAnioFabr));

      nAnioAntig := TO_NUMBER(TO_CHAR(p_dFecIniVig,'RRRR')) - nAnioFabr;
      nAnoAntiguedad := nAnioAntig; -- :C00.nAnoAntiguedad := nAnioAntig;

    END IF;

    BEGIN
      SELECT  d.CodTrasf
      INTO cCodTrasf
      FROM DATOS_PARTICULARES d
      WHERE d.IdePol = p_nIdePol
        AND d.NumCert = p_nNumCert
        AND d.CodPlan = p_cCodPlan
        AND d.RevPlan =p_cCodRevPlan
        AND d.CodRamoCert = 'VEHI'
        AND d.CodCrit = 'ALTO_RIESGO';
    EXCEPTION
        WHEN OTHERS THEN
          cCodTrasf := '000';
    END;
  dbms_output.put_line('cCodTrasf:'||cCodTrasf);

    IF (cCodTrasf = '001') THEN

      BEGIN
    dbms_output.put_line('go PR_CERT_RAMO.INSERTAR_CLAU_ALTORIESGO(');
        PR_CERT_RAMO.INSERTAR_CLAU_ALTORIESGO(p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan, p_cCodRevPlan,
                               'VEHI', cMarca, cModelo, nAnoAntiguedad
                               ,cClase, cTipo, cMoneda, nSumaAsegurada);
      EXCEPTION
        WHEN OTHERS THEN
          --FR_MENSAJE('AL_ALERTA','Error en Ins Clausas AR ' || nError || ' : ' ||SQLERRM);
          RAISE_APPLICATION_ERROR(-20101, 'RAE!!! Error en Ins Clausas AR ' || nError || ' : ' ||SQLERRM);
      END;
    END IF;

    FOR cClauCert IN C_CLAUCERT(p_nIdePol, p_nNumCert)  LOOP
    dbms_output.put_line('go ACSELX.PR_CERT_RAMO.EXCLUIR_CLAU_CERT_X_MAR_MOD( cClauCert.IdePol, cClauCert.NumCert, cClauCert.CodClau, 2)');
        ACSELX.PR_CERT_RAMO.EXCLUIR_CLAU_CERT_X_MAR_MOD( cClauCert.IdePol, cClauCert.NumCert, cClauCert.CodClau, 2) ;
    END LOOP;


    IF cIndVigLib = 'S' THEN
    dbms_output.put_line('IF cIndVigLib = S THEN');
    dbms_output.put_line('go ACSELX.PR_RENOV_AUTOM.ACT_FEC_FIN_CERT');
        BEGIN
        ACSELX.PR_RENOV_AUTOM.ACT_FEC_FIN_CERT(p_nIDEPOL, cCODPROD, p_nNUMCERT,
               p_cCODPLAN, p_cCODREVPLAN, p_cCODRAMOCERT);
      EXCEPTION
        WHEN OTHERS THEN
          --FR_ERROR(SQLERRM);
          RAISE_APPLICATION_ERROR(-20101, 'RAE!!! ' || SQLERRM);
      END;
    END IF;

    --EXIT_FORM(NO_VALIDATE);
  END IF;
--  END IF;
END FR_KEY_EXIT;

/*-----------------------------------------------------------------------------
  Nombre      : GRABAR_DATOS_PARTICULARES
  Proposito   : Creaci�n o modificaci�n de los Datos Particulares (DATOS_PARTICULARES).
  Referencias : Ninguna
  Parametros  : p_nIdepol      in datos_particulares.idepol%type,
                p_nNumCert     in datos_particulares.numcert%type,
                p_cCodplan     in datos_particulares.codplan%type,
                p_crevplan     in datos_particulares.revplan%type,
                p_cCodramocert in datos_particulares.codramocert%type,
                p_cCodCrit     in datos_particulares.codcrit%type,
                p_cCodTrasf    in datos_particulares.codtrasf%type,
                p_cValTrasf    in datos_particulares.valtrasf%type,
                p_nNumOrd      in datos_particulares.numord%type,
                p_cValCodTab   in datos_particulares.valcodtab%type,
  Retorno       p_cError      Codigo de Error en caso se encuentre un error
                              0 : OK  , 1 : Error
  ----------------------------------------------------------------------------*/
  PROCEDURE GRABAR_DATOS_PARTICULARES(
    p_nIdepol      in datos_particulares.idepol%type,
    p_nNumCert     in datos_particulares.numcert%type,
    p_cCodplan     in datos_particulares.codplan%type,
    p_crevplan     in datos_particulares.revplan%type,
    p_cCodramocert in datos_particulares.codramocert%type,
    p_cCodCrit     in datos_particulares.codcrit%type,
    p_cCodTrasf    in datos_particulares.codtrasf%type,
    p_cValTrasf    in datos_particulares.valtrasf%type,
    p_nNumOrd      in datos_particulares.numord%type,
    p_cValCodTab   in datos_particulares.valcodtab%type
    ,
    p_cError       out  varchar2 )  IS
  BEGIN
     dbms_output.put_line('~ ~ ~ ~ ~ BEGIN ! GRABAR_DATOS_PARTICULARES');
     UPDATE DATOS_PARTICULARES
     SET   codtrasf = p_cCodTrasf ,
           valtrasf = p_cValTrasf ,
           numord   = NVL(p_nNumOrd,numord) ,              --N3041367
           ValCodTab = p_cValCodTab
     WHERE idepol  = p_nIdepol
       and numcert = p_nNumCert
       and codplan = p_cCodplan
       and revplan = p_crevplan
       -- Actualizar� para todos los ramos del certificado.
       --  and codramocert= p_cCodramocert
       and codcrit    = p_ccodcrit;

     dbms_output.put_line('sql%rowcount:'|| to_char(sql%rowcount));

    IF sql%rowcount = 0 THEN
    dbms_output.put_line('INSERT...');
        INSERT INTO DATOS_PARTICULARES
      (IdePol,  NumCert , CodPlan , RevPlan , CodRamoCert,
      CodCrit  , CodTrasf , ValTrasf, NumOrd  , ValCodTab)
        VALUES
      (p_nIdepol, p_nNumCert, p_cCodplan, p_crevplan, p_cCodramocert,
            p_cCodCrit, p_cCodTrasf   , p_cValTrasf    , p_nNumOrd , p_cValCodTab );
    END IF;
    p_cError := '0,OK';
  EXCEPTION
     WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;
          RAISE_APPLICATION_ERROR(-20100,'GRABAR_DATOS_PARTICULARES '||SQLERRM);
END GRABAR_DATOS_PARTICULARES;
/*-----------------------------------------------------------------------------
  Nombre      : GRABAR_TABLA_DATOS_PARTIC
  Proposito   : Grabacion del arreglo de las datos particulares del certificado
                Invocado desde la creacion/cambio de los datos particulares del certificado
                  PR_INTERFASE_AX_SAS.EMISION_CERTIFICADO
                  PR_INTERFASE_AX_SAS.CAMBIO_DATOS_PARTIC_CERT
  Referencias : Ninguna
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodPlan              codigo del plan
                p_cRevPlan              revision del plan
                p_dfecing               fecha inicio de vigencia del certificado
                p_dfecfin               fecha fin de vigencia del certificado
                p_TABLA_DATOS_PARTIC    Tabla de los datos particulares
                p_bGrabarModCobert      Indicador para grabar Mod Cobert.

  Retorno       p_cError      Codigo de Error en caso se encuentre un error
                              0 : OK  , 1 : Error
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  2022/04/06   Ronald Barreda           Modificaci�n : Mejora en variable de retorno p_cError
  ----------------------------------------------------------------------------*/
  PROCEDURE GRABAR_TABLA_DATOS_PARTIC(
    p_nIdePol             IN poliza.idepol%type,
    p_nNumcert            IN certificado.numcert%type,
    p_cCodPlan            IN COBERT_CERT.CODPLAN%TYPE,
    p_cRevPlan            IN COBERT_CERT.REVPLAN%TYPE,
    p_dfecing             IN DATE,
    p_dfecfin             IN DATE,
    p_TABLA_DATOS_PARTIC  IN TDU_TABLA_DATOS_PARTIC,
    p_bGrabarModCobert    IN BOOLEAN DEFAULT TRUE ,
    p_cError              OUT varchar2
    )  IS
     nCantReg      integer(5);
     -- Datos particulares
     cCodramoCert       CERT_RAMO.codramocert%type;
     cCodCrit           datos_particulares.codcrit%type;
     cCodTrasf          datos_particulares.codtrasf%type;
     cValTrasf          datos_particulares.valtrasf%type;
     nNumOrd            datos_particulares.numord%type;
     cValCodTab         datos_particulares.valcodtab%type;
     cCodProd           poliza.codprod%type;
     bExisteInspeccion  boolean:=FALSE;  -- 20.02.2013
     cNumInspeccion     datos_particulares.valtrasf%type;

     -- <I N3041367>
     nIdemodelo         marca_modelo.idemodelo%TYPE;
     cCodTrasfMarca     datos_particulares.codtrasf%type;
     cDesctrasfModelo   transformador.desctrasf%type;
     nExisteMM          NUMBER;
     -- <F N3041367>
     nSecuencia         LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
  BEGIN
     p_cError := '0,OK';
       dbms_output.put_line('~ ~ ~ ~ ~ BEGIN 03 ! GRABAR_TABLA_DATOS_PARTIC');
       ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX(
          'I',p_nIdePol,p_nNumcert,NULL,'LOG',NULL,SYSDATE,SYSDATE,'GRABAR_TABLA_DATOS_PARTIC','',nSecuencia );
       BEGIN
          nCantReg := p_TABLA_DATOS_PARTIC.COUNT;
       EXCEPTION
         WHEN OTHERS THEN
            nCantReg := 0;
       END;
       IF nCantReg > 0 THEN
       dbms_output.put_line('nCantReg > 0');
           FOR i in 1..p_TABLA_DATOS_PARTIC.count loop
              IF NVL(trim(substr(p_TABLA_DATOS_PARTIC(i).txtadicional,1,7)),p_nNumcert ) = p_nNumcert THEN
                 --Se asignan a variables debido a que no se pueden enviar por parametro el elemento de la matriz
                 cCodramoCert := p_TABLA_DATOS_PARTIC(i).CodramoCert;
                 cCodCrit     := p_TABLA_DATOS_PARTIC(i).CodCrit ;
                 cCodTrasf    := p_TABLA_DATOS_PARTIC(i).CodTrasf;
                 cValTrasf    := p_TABLA_DATOS_PARTIC(i).ValTrasf;
                 nNumOrd      := p_TABLA_DATOS_PARTIC(i).NumOrd;
                 --cValCodTab   := OBTENER_VALCODTAB (cCodCrit ,cCodTrasf, cValTrasf); -- p_TABLA_DATOS_PARTIC(i).ValCodTab;
                 cValCodTab   := p_TABLA_DATOS_PARTIC(i).ValCodTab;
                 -- Este datos se obtiene del A/X ya no se utiliza lo que se envia.
                 -- Grabar los datos particulares
         dbms_output.put_line('go GRABAR_DATOS_PARTICULARES');
                 GRABAR_DATOS_PARTICULARES( p_nIdepol    , p_nNumCert   , p_cCodplan   , p_crevplan  ,
                                            cCodramoCert , cCodCrit     , cCodTrasf    , cValTrasf    ,
                                            nNumOrd      , cValCodTab   , p_cError     );

                 /*
                 IF cCodCrit = kCodCriterioWebMarcaMod THEN
                    IF NVL(cCodTrasf,'000') != '000' THEN
                    BEGIN
                       -- Grabar para el caso WEB_MARCA_MODELO
                       GRABAR_DATOS_WEB_MARCA_MODELO(p_nIdepol , p_nNumCert  , p_cCodplan   , p_crevplan  ,
                                                     cCodramocert , cCodTrasf , cValTrasf , nNumOrd , p_cError);
                    EXCEPTION
                       WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20100,'Error en GRABAR_DATOS_WEB_MARCA_MODELO '||SQLERRM);
                    END;
                    END IF;
                 END IF;
                 IF cCodCrit = kCodCriterioWebEdad THEN
                    BEGIN
                       -- Grabar para el caso WEB_EDAD
                       GRABAR_DATOS_WEB_EDAD (p_nIdepol , p_nNumCert  , p_cCodplan   , p_crevplan  ,
                                              cCodramocert , cCodTrasf , cValTrasf , nNumOrd , p_cError);
                    EXCEPTION
                       WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20100,'Error en GRABAR_DATOS_WEB_EDAD '||SQLERRM);
                    END;
                 END IF;
                 IF cCodCrit = kCodCriterioWebAnoFabric THEN
                    BEGIN
                       -- Grabar para el caso WEB_ANOS_DE_FABRICACION
                       GRABAR_DATOS_WEB_ANOS_FABRIC (p_nIdepol , p_nNumCert  , p_cCodplan   , p_crevplan  ,
                                                     cCodramocert , cCodTrasf , cValTrasf , nNumOrd , p_cError);
                    EXCEPTION
                       WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR(-20100,'Error en GRABAR_DATOS_WEB_ANOS_FABRIC '||SQLERRM);
                    END;
                 END IF;
                 IF cCodCrit = kCodCriterioInspeccion THEN  -- Criterio: 'NUMERO_INSPECCION'
                    bExisteInspeccion := TRUE;
                    cNumInspeccion := cValTrasf;
                 END IF;

                 IF cCodCrit = 'MARCAS_DE_VEHICULOS' THEN
                   cCodTrasfMarca := cCodTrasf;
                 END IF;

                 IF cCodCrit = 'MODELOS_DE_VEHICULOS' THEN
                   PR_DATOS_PARTICULARES.BUSCA_TR(cCodCrit,cCodTrasf,cDesctrasfModelo);
                 END IF;
                 */

              END IF;
           END LOOP;

           /*
           SELECT COUNT(*)
           INTO nExisteMM
           FROM datos_particulares dp, cert_ramo cr
           WHERE 1 = 1
           AND dp.idepol = p_nIdePol
           AND dp.numcert = p_nNumcert
           AND dp.codcrit = 'WEB_MARCA_MODELO'
           AND cr.idepol = dp.idepol
           AND cr.numcert = dp.numcert
           AND cr.codramocert = dp.codramocert
           AND cr.codplan = dp.codplan
           AND cr.revplan = dp.revplan
           AND cCodTrasfMarca IS NOT NULL
           AND cDesctrasfModelo IS NOT NULL
           AND ( (NVL(dp.codtrasf,'000') = '000')
                 OR
                 (NVL(dp.codtrasf,'000') != '000' AND cr.stscertramo IN ('ACT','MOD'))
               );

           IF nExisteMM > 0 THEN
             BEGIN
               SELECT idemodelo
               INTO nidemodelo
               FROM MARCA_MODELO MM
               WHERE 1 = 1
               AND MM.CODTRASFMARCA     = cCodTrasfMarca
               AND UPPER(MM.DESCMODELO) = UPPER(cDesctrasfModelo)
               AND ROWNUM = 1;
               cCodramoCert := NULL;
               cCodCrit     := 'WEB_MARCA_MODELO';
               cCodTrasf    := TO_CHAR(nidemodelo);
               cValTrasf    := UPPER(cDesctrasfModelo);
               nNumOrd      := NULL;
               cValCodTab   := OBTENER_VALCODTAB (cCodCrit ,cCodTrasf, cValTrasf);
               GRABAR_DATOS_PARTICULARES( p_nIdepol    , p_nNumCert   , p_cCodplan   , p_crevplan  ,
                                          cCodramoCert , cCodCrit     , cCodTrasf    , cValTrasf    ,
                                          nNumOrd      , cValCodTab   , p_cError     );
             EXCEPTION
               WHEN OTHERS THEN
                 UPDATE DATOS_PARTICULARES
                 SET valtrasf = UPPER(cDesctrasfModelo)
                 WHERE idepol = p_nIdepol
                 AND numcert = p_nNumCert
                 AND codplan = p_cCodplan
                 AND revplan = p_crevplan
                 AND codcrit = 'WEB_MARCA_MODELO';
             END ;
           END IF;
           */

           /*
           -- Se debe invocar la carga de datos particulares si existe la inspeccion
           IF bExisteInspeccion THEN
              --Obtener el producto de la poliza
              SELECT codprod into cCodProd  FROM POLIZA WHERE idepol = p_nIdePol;

              FOR C1 IN  ( SELECT DISTINCT codramocert
                           FROM TABLE (p_TABLA_DATOS_PARTIC) ) LOOP
                  -- Grabar los datos de la inspecciona
                  BEGIN
                     PR_INSPECCION.CARGA_DATOS_PARTIC(p_nIdePol,p_nNumCert, cCodProd, p_cCodPlan,
                                                      p_cRevPlan, c1.CodRamoCert, cNumInspeccion);
                  EXCEPTION
                     WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR(-20100,'Error en PR_INSPECCION.CARGA_DATOS_PARTIC '||SQLERRM);
                  END;
              END LOOP;
           END IF;
           */

           -- creamos los mod_datos_particulares

           --NRBA -- NO HAY -- GRABAR_MOD_DATOS_PARTICULARES
           --GRABAR_MOD_DATOS_PARTICULARES(p_nIdepol, p_nNumCert , p_cCodplan , p_crevplan ,
           --                              p_TABLA_DATOS_PARTIC(1).CodramoCert,
           --                              TRUNC(p_dfecing), TRUNC(p_dfecfin) ,  p_cError );

           IF p_bGrabarModCobert THEN
             --actualizamos mod_cobert de existir
             --NRBA -- NO HAY -- GRABAR_MOD_COBERT_DP
             --GRABAR_MOD_COBERT_DP( p_nIdepol  , p_nNumCert ,  p_TABLA_DATOS_PARTIC(1).CodramoCert,
             --                      p_cCodplan , p_crevplan ,  TRUNC(p_dfecing), TRUNC(p_dfecfin) ,  p_cError );
             NULL;
           END IF;

       END IF;
       ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','OK',SYSDATE,SYSDATE,'GRABAR_TABLA_DATOS_PARTIC','',nSecuencia );
       p_cError := '0,OK';

  EXCEPTION
     WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;
          ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','ERR',SYSDATE,SYSDATE,'GRABAR_TABLA_DATOS_PARTIC','',nSecuencia );
          RAISE_APPLICATION_ERROR(-20100,'GRABAR_TABLA_DATOS_PARTIC '||SQLERRM);
END GRABAR_TABLA_DATOS_PARTIC;


/*-----------------------------------------------------------------------------
  Nombre      : GRABAR_TABLA_DATOS_PARTIC
  Proposito   : Grabacion del arreglo de las datos particulares del certificado
                Invocado desde la creacion/cambio de los datos particulares del certificado
                  PR_INTERFASE_AX_SAS.EMISION_CERTIFICADO
                  PR_INTERFASE_AX_SAS.CAMBIO_DATOS_PARTIC_CERT
  Referencias : Form                    TARIFA.FMB
                Program Unit            FR_CARGA_BLOQUE_CONTROL (T00.BT_ENDOSO)
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodRamoCert          Codigo de Ramo/Certificado
                p_cCodPlan              codigo del plan
                p_cRevPlan              revision del plan
                p_dFecIniVig            fecha inicio de vigencia del certificado
                p_dFecFinVig            fecha fin de vigencia del certificado
                p_TABLA_DATOS_PARTIC    Tabla de los datos particulares

  Retorno       p_cError      Codigo de Error en caso se encuentre un error
                              0 : OK  , 1 : Error
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  2022/04/06   Ronald Barreda           Modificaci�n : Mejora en variable de retorno p_cError
  ----------------------------------------------------------------------------*/
PROCEDURE FR_INVOCAR_DATOS_PARTICULARES(
  p_nIdePol               POLIZA.IdePol%TYPE,
  p_nNumCert              CERT_RAMO.NumCert%TYPE,
  p_cCodRamoCert          CERT_RAMO.CodRamoCert%TYPE,
  p_cCodPlan              CERT_RAMO.CodPlan%TYPE,
  p_cCodRevPlan           CERT_RAMO.RevPlan%TYPE,
  p_dFecIniVig            DATE,
  p_dFecFinVig            DATE,
  p_TABLA_DATOS_PARTIC    TDU_TABLA_DATOS_PARTIC,
  p_cError                OUT varchar2
) IS
--
cIndAct           VARCHAR2(1);
nNumOperEmi       NUMBER;
cMensajeRestriccionSAS  VARCHAR2(400);
--
rP POLIZA%ROWTYPE;
rC CERTIFICADO%ROWTYPE;
rR CERT_RAMO%ROWTYPE;
--
BEGIN
  p_cError := '0,OK';
  dbms_output.put_line('~ ~ ~ ~ ~ BEGIN 02 ! FR_INVOCAR_DATOS_PARTICULARES');
  --IF :B01_2.MtoDedMin IS NULL AND :B01_2.PorcDed IS NULL THEN
  cIndAct:='S';
  --ELSE cIndAct:='N'; END IF;
  --
  --IF :B01_2.cNomFormaDatos IS NOT NULL THEN
  --LR_Invocar_Datos_Particulares(:B01_2.cNomFormaDatos,:B01_2.IdePol,:B01_2.NumCert,:B01_2.CodRamoCert,cIndAct);
  --FR_Refrescar_Bloq_Cert_Ramo;
  --END IF;
  rP := PR_POLIZA.DATOS_POLIZA      (p_nIdePol);
  rC := PR_CERTIFICADO.DATOS_CERT   (p_nIdePol, p_nNumCert);
  rR := PR_CERT_RAMO.DATOS_CERT_RAMO(p_nIdePol, p_nNumCert, p_cCodRamoCert);
  --
  --:C00.nNumExp         := NULL;  p_cCodProd        := rP.CodProd;   :C00.nNumPol         := rP.NumPol; :C00.cCodPol         := rP.CodPol;
  -- Se debe usar codigo de asegurado o titular para obtener lista de Empleador y grupo para decuento por planilla de SOAT, pero se muestra contratante en la pantalla.
  --:C00.cCodCli         := rC.CodCli;   :C00.cDescCli        := PR_TERCERO.Desc_Ape_Nom ('C',rP.CodCli);
  --:C00.cDescCert       := rC.DescCert; p_cCodPlan        := rR.CodPlan;    p_cCodRevPlan        := rR.RevPlan;    :C00.cStsCert        := rC.StsCert;
  --:C00.dFecIng         := rC.FecIng;   :C00.dFecIniValid    := rR.FecIniValid; :C00.dFecFinValid    := rR.FecFinValid;
  --IF :C00.nExisteUsu  IS NULL THEN :C00.nExisteUsu := 1; END IF;
  --IF :C00.nSiniDat    IS NULL THEN :C00.nSiniDat   := 0; END IF;
  --
  BEGIN
    SELECT COUNT(1)
    INTO   nNumOperEmi
  FROM   OPER_POL
  WHERE  IdePol  = p_nIdePol
    AND  NumCert = p_nNumCert
    AND  TipoOp  = 'EMI'
    AND  NVL(IndAnul,'N') <> 'S';
  EXCEPTION
    WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20101,'RAE!!! FR_INVOCAR_DATOS_PARTICULARES'); -- FR_ERROR (SQLERRM);
    END;
  --
  -- T00.BT_ENDOSO
  dbms_output.put_line('ANTESDE pr_poliza_util.RESTRINGE_ENDOSO_POLIZA( p_nIdePol )');
  cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA( p_nIdePol );
  dbms_output.put_line('cMensajeRestriccionSAS:'||cMensajeRestriccionSAS);
  IF NOT cMensajeRestriccionSAS IS NULL THEN --AND (NVL(:B01_4.CODCRIT,'XXXXX') NOT IN ('CERTIFICADO_SOAT','NRO_LOTE_SOAT') OR :SYSTEM.CURRENT_BLOCK != 'B01_4' )then
   RAISE_APPLICATION_ERROR(-20101, 'Esta p�liza fue emitida en SAS MOBILE. S�lo puede editar el certificado SOAT y el N�mero de Lote.');
  END IF;
  --
  /*
  B01_4 : DATOS_PARTICULARES
  WHERE
  IDEPOL    = p_nIdePol  AND
  NUMCERT   = p_nNumCert AND
  CODPLAN   = p_cCodPlan AND
  REVPLAN   = p_cCodRevPlan AND
  CODRAMOCERT = p_cCodRamoCert AND
  CODCRIT    IN (SELECT CODCRIT FROM CRITERIO WHERE TIPOCRIT = 'S' AND USOCRIT = 'T')
  */
  dbms_output.put_line('GO GRABAR_TABLA_DATOS_PARTIC');
  GRABAR_TABLA_DATOS_PARTIC(
      p_nIdePol, p_nNumCert, p_cCodPlan, p_cCodRevPlan,
      p_dFecIniVig, p_dFecFinVig, p_TABLA_DATOS_PARTIC, TRUE, p_cError);

  -- KEY-EXIT
  dbms_output.put_line('GO FR_KEY_EXIT');
  FR_KEY_EXIT(
      p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodPlan, p_cCodRevPlan,
      p_dFecIniVig, p_dFecFinVig, p_TABLA_DATOS_PARTIC, rP.CodProd, '', p_cError);

EXCEPTION
  WHEN OTHERS THEN
    p_cError := '1,' || SUBSTR(SQLERRM,1,498);
    RAISE_APPLICATION_ERROR(-20100,'FR_INVOCAR_DATOS_PARTICULARES:'||SQLERRM);
END FR_INVOCAR_DATOS_PARTICULARES;
  /*-----------------------------------------------------------------------------
  Nombre      : mod_datparticulares (overloading)
  Proposito   : Sobrecarga de procedimiento para modificacion de datos particulares
  Referencias : <RTC-314183> - Inclusion de Equipos TREC
  Parametros  :
                p_idepol        NUMBER                      Identificador de Poliza
                p_numcert       NUMBER                      Identificador de Certificado
                p_fec_ini_end   DATE                        Fecha de inicio de Vigencia del Endoso
                p_fec_fin_end   DATE                        Fecha de fin de Vigencia del Endoso
                p_codramocert   VARCHAR2                    Codigo del Ramo
                p_datos_partic  TDU_TABLA_DATOS_PARTIC      Arreglo de Datos Particulares
                p_numid         VARCHAR2                    NumId del Usuario
                p_mensajes      REF CURSOR                  Mensajes de error
                p_error         VARCHAR2                    Mensaje de error

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/10/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure mod_datparticulares(p_idepol       poliza.idepol%type,
                                p_numcert      cert_ramo.numcert%type,
                                p_fec_ini_end  date,
                                p_fec_fin_end  date,
                                p_codramocert  cert_ramo.codramocert%type,
                                p_datos_partic tdu_tabla_datos_partic,
                                p_numid        usuario.numid%type,
                                p_mensajes     out sys_refcursor,
                                p_error        out varchar2) is
    c_name constant varchar2(30) := 'mod_datparticulares';
    c_ok   constant varchar2(30) := '0,OK';
    l_codplan   cert_ramo.codplan%type;
    l_revplan   cert_ramo.revplan%type;
    l_secuencia log_poliza_sas_ax.secuencia%type;
    l_error     varchar2(2000);
    ------------------------------------------------
    procedure validar is
      c_vd   constant datos_particulares.codcrit%type := 'VALOR_DECLARADO.';
      c_sa   constant datos_particulares.codcrit%type := 'SUMA_ASEGURADA..';
      l_sas_mobile  varchar2(400);
      l_codramocert cert_ramo.codramocert%type;
      l_codcrit     criterio.codcrit%type;
      l_vd          datos_particulares.valtrasf%type;
      l_sa          datos_particulares.valtrasf%type;
    begin
      pr_interfase_ax.limpiar_log_mensajes();

      l_sas_mobile := pr_poliza_util.restringe_endoso_poliza(p_idepol);

      if l_sas_mobile is not null then
        -- La poliza: :mx1 fue emitida en SAS MOBILE por lo que debe ser endosada o modificada en SAS
        pr_interfase_ax_sas_util.insertar_log_errores(c_name, 'VAL-0297', p_idepol);
        raise_application_error(-20100, 'SAS MOBILE');
      end if;

      val_certificado(p_numcert);

      -- validar array
      for idx in 1 .. p_datos_partic.count loop
        l_codramocert := p_datos_partic(idx).codramocert;
        l_codcrit     := p_datos_partic(idx).codcrit;

        val_ramo(p_idepol, p_numcert, l_codramocert);
        val_criterio(p_idepol, p_numcert, l_codramocert, l_codcrit);
      end loop;

      begin
        select t.valtrasf into l_vd from table(p_datos_partic) t where t.codcrit = c_vd;
      exception
        when others then
          l_vd := '0';
      end;

      begin
        select t.valtrasf into l_sa from table(p_datos_partic) t where t.codcrit = c_sa;
      exception
        when others then
          l_sa := '0';
      end;

      if to_number(l_sa) > to_number(l_vd) then
        -- 'La Suma Asegurada no debe ser mayor al Valor Declarado'
        pr_interfase_ax_sas_util.insertar_log_errores(c_name, 'VAL-1557');
        raise_application_error(-20100, 'SA MAYOR VD');
      end if;

      -- llamado despues de haber validado los 3 primeros parametros de entrada
      val_vigencia_endoso(p_idepol, p_numcert, p_codramocert, p_fec_ini_end, p_fec_fin_end);
    end;
    ------------------------------------------------
  begin
    p_error := '1';
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('I',
                                                      p_idepol,
                                                      p_numcert,
                                                      null,
                                                      'LOG',
                                                      null,
                                                      sysdate,
                                                      sysdate,
                                                      c_name,
                                                      '',
                                                      l_secuencia);
    validar();
    pr_interfase_ax_sas_util.cargar_usuario_sesion(p_numid);

    begin
      select codplan, revplan
        into l_codplan, l_revplan
        from cert_ramo
       where idepol = p_idepol
         and numcert = p_numcert
         AND StsCertRamo NOT IN ('ANU', 'EXC') --<RTC 313742> / CBayona / 19-10-2022 / Se excluyen los siguientes estados
         and codramocert = p_codramocert;

    exception
      when no_data_found then
        l_codplan := '';
        l_revplan := '';
    end;

    -- si no cumple criterios de poliza de reaseguro facultativo, que realice lo que siempre hace
    if p_codramocert is not null then
      dbms_output.put_line('antes de fr_invocar_datos_particulares');
      pr_interfase_ax.fr_invocar_datos_particulares(p_idepol,
                                                    p_numcert,
                                                    p_codramocert,
                                                    l_codplan,
                                                    l_revplan,
                                                    p_fec_ini_end,
                                                    p_fec_fin_end,
                                                    p_datos_partic,
                                                    l_error);
    end if;

    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F',
                                                      p_idepol,
                                                      p_numcert,
                                                      null,
                                                      'LOG',
                                                      'OK',
                                                      sysdate,
                                                      sysdate,
                                                      c_name,
                                                      '',
                                                      l_secuencia);

    p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
    p_error    := c_ok;

    dbms_output.put_line(c_name || ' fin');
  exception
    when others then
      p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes();
      p_error    := '1,' || sqlerrm;
      acselx.pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F',
                                                               p_idepol,
                                                               null,
                                                               null,
                                                               'LOG',
                                                               'ERR',
                                                               sysdate,
                                                               sysdate,
                                                               c_name,
                                                               null,
                                                               l_secuencia);
  end;

/*
FORM  EMI00137        Mantenimiento de Ramos por Certificado
    C01.BT_DAT_PART   When-Button-Pressed
              MOD_DATOS_PARTICULOS_COBERTURAS
*/
/*-----------------------------------------------------------------------------
  Nombre      : MOD_DATPARTICULARES
  Proposito   : Endoso de Modificaci�n de Numero de Trabajadores 3D
  Referencias : Form                    EMI00137
                When-Button-Pressed     C01.BT_DAT_PART
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_dFecIniVig            fecha inicio de vigencia del certificado
                p_dFecFinVig            fecha fin de vigencia del certificado
                p_cCodRamoCert          Codigo de Ramo
                p_TABLA_DATOS_PARTIC    Tabla de los datos particulares

  Retorno       p_cError      Codigo de Error en caso se encuentre un error
                              0 : OK  , 1 : Error
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           <RTC 313092> Creaci�n
  2022/04/06   Ronald Barreda           <RTC 313092> Mejora variable de Retorno p_cError
  ----------------------------------------------------------------------------*/
PROCEDURE MOD_DATPARTICULARES(
  p_nIdePol               POLIZA.IdePol%TYPE,
  p_nNumCert              CERT_RAMO.NumCert%TYPE,
  p_dFecIniVig            DATE,
  p_dFecFinVig            DATE,
  p_cCodRamoCert          CERT_RAMO.CodRamoCert%TYPE,
  p_TABLA_DATOS_PARTIC    TDU_TABLA_DATOS_PARTIC,
  p_cError                OUT varchar2
) IS
  --p_cCodOperAcceso    VARCHAR2(5):= '427';
  --
  rPoliza         pr_Poliza.g_rPoliza%ROWTYPE;
  --
  nIdePol         Poliza.IdePol%TYPE;
  nNumPol         POLIZA.NUMPOL%TYPE;
  cStsPol         POLIZA.STSPOL%TYPE;
  cCodProd        POLIZA.CODPROD%TYPE;
  dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
  dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
  nIndFac         NUMBER;
  cIndPol         VARCHAR2(5);
  nExist          NUMBER;
  cIndAct         VARCHAR2(1);
  RPP             Ramo_Plan_Prod%ROWTYPE;
  --
  p_cCodPlan        CERT_RAMO.CodPlan%TYPE;
  p_cCodRevPlan     CERT_RAMO.RevPlan%TYPE;
  --
  nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
  bExiste         BOOLEAN := TRUE;
  cRestricSAS     VARCHAR2(1000);
  --
  cError          VARCHAR2(2000);
BEGIN
  --Seteo de variables globales -- :GLOBAL.cCodOperAcceso := '427'; :GLOBAL.cErrorAcceso   := NULL; :GLOBAL.cCodUsr        := NULL;
  p_cError := '0,OK';
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,p_nNumcert,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'MOD_DATPARTICULARES','',nSecuencia );
  --
  -- Validacion del estado la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de poliza'; RETURN; END IF;
  -- Validacion del estado de la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de estado de poliza'; RETURN; END IF;
  -- Validar si existe el certificado
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_EXISTE_CERTIFICADO( p_nIdePol , p_nNumCert );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de certificado'; RETURN; END IF;
  --
  IF (p_dFecFinVig > dFecFinVigPol) OR (p_dFecIniVig > dFecFinVigPol) THEN
    p_cError := '1,Fecha de Endoso supera a fecha fin de vigencia de poliza'; RETURN;
  END IF;
  --
  cRestricSAS := pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
  --
  IF NOT cRestricSAS IS NULL THEN
     p_cError := '1,' || cRestricSAS;
     RETURN;
  END IF;
  --
  --
  nIdePol := p_nIdePol;
  rPoliza := PR_POLIZA.DATOS_POLIZA(nIdePol);
  --
  BEGIN
    SELECT CodPlan, RevPlan
    INTO   p_cCodPlan, p_cCodRevPlan
    FROM   CERT_RAMO
    WHERE  idepol  = p_nIdePol
    AND  numcert = p_nNumCert
    AND StsCertRamo NOT IN ('ANU', 'EXC') --<RTC 313742> / CBayona / 19-10-2022 / Se excluyen los siguientes estados
    AND  codRamoCert = p_cCodRamoCert

    ;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
       p_cCodPlan    := '';
       p_cCodRevPlan := '';
  END;

  --
  BEGIN
    SELECT COUNT(1)
    INTO   nExist
    FROM   LVal
    WHERE  TipoLval = 'STDPOL'
      AND  CodLval  = rPoliza.StsPol;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      nExist:= 0;
  END;
  --
  cIndPol := PR_POLITICAS.Existe_Politicas_Autorizadas(nIdePol);
  nIndFac := PR_GEN_REA_T.VALIDA_FACULTATIVO_MOD(nIdePol);
  --
  dbms_output.put_line('nExist:' || TO_CHAR(nExist) || '|' || 'cIndPol : ' || cIndPol || '|' || 'nIndFac : ' || nIndFac);
  --
  IF (cIndPol = 'S' AND nIndFac = 1 AND nExist > 0) THEN --  Es p�liza facultativa
    dbms_output.put_line('MDPP : IF (cIndPol = S AND nIndFac = 1 AND nExist > 0) THEN');
    --
    --Validamos si usuario consulta o modifica
    --cStr := 'Desea modificar los datos de la p�liza Facultativa?' || CHR(13) ||'( OK -> Modificar datos, NO -> Consulta)';
    --SET_ALERT_PROPERTY('CONSULTA',ALERT_MESSAGE_TEXT,cStr);   --  cambiar
    --nButton := SHOW_ALERT('CONSULTA'); -- cambiar
    --
  IF 1 = 1 THEN -- IF nButton = ALERT_BUTTON1 THEN -- OK
      -- Modifica datos
      --cStr := 'Se borrar� la colocaci�n temporal de reaseguro facultativo ' || CHR(13) || ' y las autorizaciones de la p�liza';
      --SET_ALERT_PROPERTY('POLITICAS',ALERT_MESSAGE_TEXT,cStr);
      --nButton := SHOW_ALERT('POLITICAS');
      --
    IF 1 = 1 THEN -- IF nButton = ALERT_BUTTON1 THEN -- OK
        --
        --CALL_FORM('AUT00009',NO_HIDE);
        --c_Error  := NVL(:GLOBAL.cErrorAcceso,'S'); c_CodUsr := :GLOBAL.cCodUsr;
        --ERASE('GLOBAL.cCodOperAcceso'); ERASE('GLOBAL.cErrorAcceso'); ERASE('GLOBAL.cCodUsr');
        --
        IF 1 = 1 THEN -- IF c_Error = 'N' THEN
          ----
          -- Elimina datos de colocaci�n y politicas
          BEGIN
            DBMS_OUTPUT.PUT_LINE('MDPP antes de PR_EVALUA_POLITICA.Elimina_Politica(nIdePol)');
            --PR_EVALUA_POLITICA.Elimina_Politica(nIdePol);
            --PR_GEN_REA_T.Eliminar_Rea_Temporal(nIdePol,NULL,NULL,NULL);
          EXCEPTION
            WHEN OTHERS THEN
              DBMS_OUTPUT.PUT_LINE('FRM-EMI00137 error al invocar PR_EVALUA_POLITICA.ELIMINA_POLITICA - '||SQLERRM);
          END;
          ----
          -- Realiza la acci�n normal del bot�n.
          IF p_cCodRamoCert IS NOT NULL THEN --:B01_2.CodRamoCert
            DBMS_OUTPUT.PUT_LINE('MDPP antes de RPP := PR_RAMO_PLAN_PROD.Datos_Ramo_Plan_Prodi');
            RPP := PR_RAMO_PLAN_PROD.Datos_Ramo_Plan_Prod(
            cCodProd, p_cCodPlan, p_cCodRevPlan, p_cCodRamoCert);
            --:B01_2.cEntradaDatos1 := RPP.EntradaDatos1; :B01_2.cNomFormaDatos := RPP.NomFormaDatos; :B01_2.cIndClauOblig := RPP.IndClauOblig;
            --Se invoca datos datos particulares. -- RPP.NomFormaDatos = TARIFA.FMB
            --FR_Invocar_Datos_Particulares(RPP.EntradaDatos1, RPP.NomFormaDatos, RPP.IndClauOblig);
      DBMS_OUTPUT.PUT_LINE('MDPP antes de FR_Invocar_Datos_Particulares B');
      FR_Invocar_Datos_Particulares(
              p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodPlan, p_cCodRevPlan,
              p_dFecIniVig, p_dFecFinVig, p_TABLA_DATOS_PARTIC, p_cError);
          END IF;
          ----
        END IF;
        ----
      ELSE
        ----
        RAISE_APPLICATION_ERROR(-20101, 'RAE!!! MOD_DATPARTICULARES');
        ----
      END IF;
    END IF;
  ELSE
    -- Si no cumple criterios de poliza de reaseguro facultativo, que realice lo que siempre hace
    IF p_cCodRamoCert IS NOT NULL THEN
      RPP := PR_RAMO_PLAN_PROD.Datos_Ramo_Plan_Prod(
        cCodProd, p_cCodPlan, p_cCodRevPlan, p_cCodRamoCert);
      --:B01_2.cEntradaDatos1 := RPP.EntradaDatos1; :B01_2.cNomFormaDatos := RPP.NomFormaDatos; :B01_2.cIndClauOblig := RPP.IndClauOblig;
      -- Se invoca datos datos particulares.
    DBMS_OUTPUT.PUT_LINE('MDPP antes de FR_Invocar_Datos_Particulares C');
      FR_Invocar_Datos_Particulares(
        p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodPlan, p_cCodRevPlan,
        p_dFecIniVig, p_dFecFinVig, p_TABLA_DATOS_PARTIC, p_cError);
    END IF;
  END IF;
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','OK',SYSDATE,SYSDATE,'MOD_DATPARTICULAREScin','',nSecuencia );
  --
  DBMS_OUTPUT.PUT_LINE('~ ~ ~ ~ ~ ~ ~ ~ ~ ~ MOD_DATPARTICULARES fin');
EXCEPTION
  WHEN OTHERS THEN
       p_cError := '1,' || SUBSTR(SQLERRM, 1, 498); --<RTC 313350> / CBayona / 12-10-2022 / Se modifico valores SUBSTR
       ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR',SYSDATE,SYSDATE,'MOD_DATPARTICULARES','',nSecuencia );
END MOD_DATPARTICULARES;

PROCEDURE GRABAR_ANEXO_POLIZA(
  p_nIdePol              in  poliza.idepol%type,
  p_TABLA_ANEXO_POL      in  TDU_TABLA_ANEXO_POL,
  p_cCodUsuario          in  varchar2,
  p_cursorMsgValida      OUT sys_refcursor ,
  p_cError               OUT varchar2
) IS
BEGIN
  pr_Interfase_ax_sas.GRABAR_ANEXO_POLIZA(
    p_nIdePol              ,
    p_TABLA_ANEXO_POL      ,
    p_cCodUsuario          ,
    p_cursorMsgValida      ,
    p_cError
  );
END;

/*-----------------------------------------------------------------------------
  Nombre      : Activar_Mod_Cobert
  Proposito   : Endoso de Modificaci�n de Numero de Trabajadores 3D
  Referencias : Form                    EMI00137
                When-Button-Pressed     C01.BT_DAT_PART
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodRamoCert          Ramo del certificado
                p_dFecFinValid          fecha fin de vigencia
                p_nIdeCobert            Id de Cobertura
                p_nNumMod               Numero de Modificaci�n de Cobertura
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  ----------------------------------------------------------------------------*/

PROCEDURE Activar_Mod_Cobert(
  p_nIdePol         MOD_COBERT.IdePol%TYPE,
  p_cCodProd        POLIZA.CodProd%TYPE,
  p_nNumCert        MOD_COBERT.NumCert%TYPE,
  p_cCodRamoCert    MOD_COBERT.CodRamoCert%TYPE,
  p_dFecFinValid    DATE,
  p_nIdeCobert      MOD_COBERT.IdeCobert%TYPE,
  p_NumMod          MOD_COBERT.NumMod%TYPE
) IS
  nIndCoa                NUMBER(1) := 0;
  nNumModCob             PLS_INTEGER;
  dfecfinvalid_coa       DATE;
  nValidCoa              INTEGER;
  nAlerta                NUMBER;
  nReg                   INTEGER;
  cMensajeRestriccionSAS VARCHAR2(400);
  nDummy                 pls_integer;
BEGIN
  cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
  if not cMensajeRestriccionSAS is null then
      --nDummy := LR_ALERTA(cMensajeRestriccionSAS);
      NULL;
  end if;

  nIndCoa := PR_Poliza_Util.Existe_Mov_Coaseguro(p_nIdePol);

  BEGIN
    SELECT MAX(dc.fecfinvalid)
      INTO dfecfinvalid_coa
      FROM dist_coa dc
     WHERE dc.idepol      = p_nIdePol
       AND CodAcepRiesgo != '000001'
       AND dc.stscoa NOT IN ('ANU', 'EXC');

    nValidCoa := 0;
    IF dfecfinvalid_coa IS NOT NULL THEN
--      GO_BLOCK('B02_1');      nReg := :System.Cursor_Record;       FIRST_RECORD;
--      LOOP
        IF (p_dFecFinValid > dfecfinvalid_coa) AND
            p_dFecFinValid IS NOT NULL         AND
            PR.BUSCA_LVAL('EXCPAGUE','RAMO_' || p_cCodRamoCert ) = 'INVALIDO' AND
            PR.BUSCA_LVAL('EXCPAGUE','PROD_' || p_cCODPROD )     = 'INVALIDO' THEN
            -- nAlerta:= FR_ALERTA('Verificar: La Fecha Fin de Vigencia de las Coberturas, excede el Fin de Vigencia del Coaseguro '||TO_CHAR(dfecfinvalid_coa, 'DD/MM/YYYY'));
            nValidCoa := 1;
--          EXIT;
        END IF;
--      EXIT WHEN :SYSTEM.LAST_RECORD='TRUE'; NEXT_RECORD;
--      END LOOP;
/*
      FIRST_RECORD;
      LOOP
        EXIT WHEN (:SYSTEM.LAST_RECORD = 'TRUE' OR :System.Cursor_Record = nReg);
        NEXT_RECORD;
      END LOOP;
*/
    END IF;
  EXCEPTION WHEN OTHERS THEN
    nValidCoa := 0;
  END;
  --
  IF nValidCoa = 0 THEN

  IF nIndCoa = 0 THEN
    --FR_COMMIT_FORM;
    PR_Mod_Cobert.Procesar(p_nIdeCobert, p_NumMod, 'IM');
    nNumModCob := PR_MOD_COBERT.CANTIDAD_MODIFICACIONES(p_nIdeCobert,'ALL');

    DELETE POLIZA_OPERACION
     WHERE IDEPOL  = p_nIdePol
       AND NUMCERT = p_nNumCert
       AND NUMOPER IS NULL;

    /*IF nNumModCob > 0 THEN
      GO_BLOCK('B02_1');LR_Refrescar_Bloque('B02_1');
    ELSE
      :C00.cSalir := 'S'; GO_BLOCK('B01_1'); LR_Refrescar_Bloque('B01_1');
    END IF ;*/
  ELSE
    --LR_Error('Se han realizado Movimientos de Coaseguro. No es posible Incluir la Cobertura');
    RAISE_APPLICATION_ERROR(-20101, 'Se han realizado Movimientos de Coaseguro. No es posible Incluir la Cobertura');
  END IF;
  END IF;
END Activar_Mod_Cobert;

/*-----------------------------------------------------------------------------
  Nombre      : Calcular Montos
  Proposito   : Calcular Montos
  Referencias : Form                    EMI00137
                When-Button-Pressed     C01.BT_DAT_PART
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodProd              Codigo de producto
                p_cCodPlan              Codigo de Plan
                p_cRevPlan              Revisi�n del Plan
                p_cCodRamoCert          Ramo del certificado
                p_nTasa                 Tasa
                p_nPrimaMoneda          Prima
                p_cParam                Parametro Pre Cobertura
                p_cCodCobert            Codigo de Cobertura
                p_nSumaAsegMoneda       Suma Asegurada
                p_nTipTasa              Tipo de Tasa
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  ----------------------------------------------------------------------------*/
  PROCEDURE Calcula_Montos (
    p_nIdePol              COBERT_CERT.IdePol%TYPE,
    p_nNumCert             COBERT_CERT.NumCert%TYPE,
    p_cCodProd             POLIZA.CodProd%TYPE,
    p_cCodPlan             COBERT_CERT.CodPlan%TYPE,
    p_cRevPlan             COBERT_CERT.RevPlan%TYPE,
    p_cCodRamoCert         COBERT_CERT.CodRamoCert%TYPE,
    p_nTasa         IN OUT MOD_COBERT.Tasa%TYPE,
    p_nPrimaMoneda  IN OUT MOD_COBERT.PrimaMoneda%TYPE,
    p_cParam               COBERT_PLAN_PROD.ParamPreCobert%TYPE,
    p_cCodCobert           COBERT_CERT.CodCobert%TYPE,
    p_nSumaAsegMoneda      COBERT_CERT.SumaAsegMoneda%TYPE,
    p_nTipTasa             NUMBER) IS

  BEGIN
    IF p_cParam IS NOT NULL THEN
      IF p_nTasa IS NULL OR p_nTasa = 0 THEN
        p_nTasa := NVL(PR_PRE_COBERT.PRE_TASA (p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan,
                                               p_cRevPlan, p_cCodRamoCert, p_cCodCobert, p_cParam, NULL),0);
      END IF;

      IF p_nPrimaMoneda IS NULL OR p_nPrimaMoneda = 0 THEN
        p_nPrimaMoneda := NVL(PR_PRE_COBERT.PRE_PRIMA(p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan,
                                                   p_cRevPlan, p_cCodRamoCert, p_cCodCobert, p_cParam,NULL),0);
      END IF;

      IF p_nPrimaMoneda IS NULL OR p_nPrimaMoneda = 0 THEN
        p_nPrimaMoneda := PR_PRE_COBERT.POST_PRIMA(p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan,
                                                   p_cRevPlan, p_cCodRamoCert, p_cCodCobert, p_cParam,NULL,p_nSumaAsegMoneda);
      END IF;
    END IF;

    IF (p_nPrimaMoneda IS NULL OR p_nPrimaMoneda = 0) AND (p_nTasa > 0 OR p_nTasa IS NOT NULL)THEN
        p_nPrimaMoneda := p_nSumaAsegMoneda * p_nTasa / p_nTipTasa;
    END IF;

    IF p_nTasa IS NULL OR p_nTasa = 0 AND (p_nSumaAsegMoneda <> 0  AND p_nSumaAsegMoneda IS NOT NULL) THEN
        p_nTasa := p_nPrimaMoneda * p_nTipTasa / p_nSumaAsegMoneda;
    END IF;

    IF p_nTasa <> 0 AND p_nSumaAsegMoneda <> 0 AND p_nPrimaMoneda <> 0 THEN
       p_nPrimaMoneda := p_nSumaAsegMoneda * p_nTasa / p_nTipTasa;
    END IF;
  END Calcula_Montos;

/*-----------------------------------------------------------------------------
  Nombre      : Actualizar
  Proposito   : Actualizar Cobert_Cert y Mod_Cobert
  Referencias : Form                    EMI00137
                When-Button-Pressed     C01.BT_DAT_PART
  Parametros  :
                p_nPrima                Prima
                p_nPrimaMoneda          Prima
                p_nPrimaMonedaCob       Prima
                p_nTasa                 Tasa
                p_nSumaAseg             Suma Asegurada
                p_nSumaAsegMoneda       Suma Asegurada
                p_nNumMod               Numero de Modificaci�n de Cobertura
                p_cCodCobert            Codigo de Cobertura
                p_cCodCobert            Id de Cobertura
                p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodRamoCert          Ramo del certificado
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  ----------------------------------------------------------------------------*/

  PROCEDURE Actualizar(
    p_nPrima          COBERT_CERT.Prima%TYPE,
    p_nPrimaMoneda    COBERT_CERT.PrimaMoneda%TYPE,
    p_nPrimaMonedaCob COBERT_CERT.PrimaMonedaCob%TYPE,
    p_nTasa           COBERT_CERT.Tasa%TYPE,
    p_nSumaAseg       COBERT_CERT.SumaAseg%TYPE,
    P_nSumaAsegMoneda COBERT_CERT.SumaAsegMoneda%TYPE,
    p_nNumMod         MOD_COBERT.NumMod%TYPE,
    p_cCodCobert      COBERT_CERT.CodCobert%TYPE,
    p_nIdeCobert      COBERT_CERT.IdeCobert%TYPE,
    p_nIdePol         COBERT_CERT.IdePol%TYPE,
    p_nNumCert        COBERT_CERT.NumCert%TYPE,
    p_cCodRamoCert    COBERT_CERT.CodRamoCert%TYPE
  ) IS
  BEGIN
    IF p_nNumMod IS NULL THEN
      UPDATE COBERT_CERT
      SET Prima          = p_nPrima,
          PrimaMoneda    = p_nPrimaMoneda,
           PrimaMonedaCob = p_nPrimaMonedaCob,
           Tasa           = p_nTasa ,
           SumaAseg       = p_nSumaAseg,
           SumaAsegMoneda = p_nSumaAsegMoneda
      WHERE IdePol      = p_nIdePol
        AND NumCert     = p_nNumCert
        AND CodRamoCert = p_cCodRamoCert
        AND CodCobert   = p_cCodCobert;
    ELSE
      UPDATE MOD_COBERT
      SET Tasa           = NVL(p_nTasa,0),
           PrimaMoneda    = NVL(p_nPrimaMoneda,0),
           PrimaMonedaCob = NVL(p_nPrimaMonedaCob,0),
           Prima          = NVL(p_nPrima,0),
           SumaAseg       = NVL(p_nSumaAseg,0),
           SumaAsegMoneda = NVL(p_nSumaAsegMoneda,0)
      WHERE IdeCobert    = p_nIdeCobert
        AND NumMod       = p_nNumMod
        AND StsModCobert <> 'ACT';
    END IF;
  END;

/*-----------------------------------------------------------------------------
  Nombre      : Tarifa_M
  Proposito   : Calculo de tarifa
  Referencias : Form                    EMI00137
                When-Button-Pressed     C01.BT_DAT_PART
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodProd              Codigo de producto
                p_cCodPlan              Codigo de Plan
                p_cRevPlan              Revisi�n del Plan
                p_cCodRamoCert          Ramo del certificado
                p_cCodCobertRef         Codigo de Cobertura
                p_nSumaAsegMonedaCob    Suma Asegurada
                p_nPrimaMonedaCob       Prima
                p_nTasa                 Tasa
                p_nNumMod               Numero de Modificaci�n
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  ----------------------------------------------------------------------------*/
  PROCEDURE Tarifa_M(
    p_nIdePol              COBERT_CERT.IdePol%TYPE,
    p_nNumCert             COBERT_CERT.NumCert%TYPE,
    p_cCodProd             POLIZA.CodProd%TYPE,
    p_cCodPlan             COBERT_CERT.CodPlan%TYPE,
    p_cRevPlan             COBERT_CERT.RevPlan%TYPE,
    p_cCodRamoCert         COBERT_CERT.CodRamoCert%TYPE,
    p_cCodCobertRef        COBERT_CERT.CodCobert%TYPE,
    p_nSumaAsegMonedaCob   MOD_COBERT.SumaAsegMonedaCob%TYPE,
    p_nPrimaMonedaCob      MOD_COBERT.PrimaMonedaCob%TYPE,
    p_nTasa                MOD_COBERT.Tasa%TYPE,
    p_nNumMod              NUMBER
    ) IS
  cParam             COBERT_PLAN_PROD.ParamPreCobert%TYPE;
  nSumaAseg            COBERT_CERT.SumaAseg%TYPE;
  nPrima              COBERT_CERT.Prima%TYPE;
  nTasa              MOD_COBERT.Tasa%TYPE;
  nPrimaMonedaCob     MOD_COBERT.PrimaMonedaCob%TYPE;
  cCodMoneda         COBERT_CERT.CodMonedaCob%TYPE;
  nPrimaMoneda        MOD_COBERT.PrimaMoneda%TYPE;
  nSumaAsegMoneda    COBERT_CERT.SumaAsegMoneda%TYPE;
  cCodMonedaAnt      COBERT_CERT.CodMonedaCob%TYPE := 'X';
  nTipTasa            NUMBER(4);
  nTasaCambio         NUMBER(11,6);
  nTasaCambioPol     NUMBER(11,6);
  nTasaCambioCob     NUMBER(11,6);
  nSumaAsegMonedaCob COBERT_CERT.SumaAsegMonedaCob%TYPE;
  nNumModMax         MOD_COBERT.NumMod%TYPE;

  M                  MOD_COBERT%ROWTYPE;
  P                  COBERT_PLAN_PROD%ROWTYPE;
  cC00_cCodMonedaLocal       MOD_COBERT.CodMoneda%TYPE;
  cC00_cCodMoneda            MOD_COBERT.CodMoneda%TYPE;

  CURSOR c_Cob_Tar IS
    SELECT IdeCobert,CodCobert,SumaAsegMoneda,SumaAsegMonedaCob,CodMonedaCob
    FROM   COBERT_CERT
    WHERE  IdePol      = p_nIdePol
      AND  NumCert     = p_nNumCert
      AND  CodRamoCert = p_cCodRamoCert
      AND  CodCobert   = p_cCodCobertRef;
  BEGIN

    cC00_cCodMonedaLocal := PR_VINISUSC.CODIGO_MONEDA;
    --
    SELECT MAX(CodMoneda)
    INTO   cC00_cCodMoneda
    FROM   POLIZA
    WHERE  IdePol = p_nIdePol;

    FOR C IN c_Cob_Tar LOOP
      IF p_nNumMod IS NULL THEN
        nSumaAsegMonedaCob := C.SumaAsegMonedaCob;
        nSumaAsegMoneda    := C.SumaAsegMoneda;
      ELSE
        nSumaAsegMonedaCob := nSumaAsegMonedaCob;
        nSumaAsegMoneda    := nSumaAsegMonedaCob;
      END IF;
      nNumModMax := PR_Mod_Cobert.Max_Mod_Cobert(C.IdeCobert);
      M := PR_Mod_Cobert.Datos_Mod_Cobert(C.IdeCobert,nNumModMax);
      nTasa           := M.Tasa;
      nPrimaMonedaCob := M.PrimaMonedaCob;
      nPrimaMoneda    := M.PrimaMoneda;

      cCodMoneda := C.CodMonedaCob;

      --cParam     := :B01_1.cParam;
      P := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(p_cCodProd, p_cCodPlan, p_cRevPlan,
                                                      p_cCodRamoCert, p_cCodCobertRef);
      cParam        := P.ParamPreCobert;

      IF cCodMoneda <> cCodMonedaAnt THEN
        nTasaCambio    := PR.TASA_CAMBIO(cCodMoneda,cC00_cCodMonedaLocal,SYSDATE,'D');
        nTasaCambioPol := PR.TASA_CAMBIO(cCodMoneda,cC00_cCodMoneda,SYSDATE,'D');
        nTasaCambioCob := PR.TASA_CAMBIO(cC00_cCodMoneda,cCodMoneda,SYSDATE,'D');
      END IF;

      nTipTasa := PR.BUSCA_TIPO_TASA_PROD(p_cCodProd , p_cCodRamoCert, p_cCodPlan, p_cRevPlan, C.CodCobert);
      IF cCodMoneda <> 'UIT' THEN
        Calcula_Montos( p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan, p_cRevPlan, p_cCodRamoCert,
            nTasa,nPrimaMonedaCob,cParam,C.CodCobert,nSumaAsegMonedaCob,nTipTasa);
        nPrima          := nPrimaMonedaCob * nTasaCambio;
        nSumaAseg       := nSumaasegMonedaCob * nTasaCambio;
        nSumaasegMoneda := nSumaAsegMonedaCob * nTasaCambioPol;
        nPrimaMoneda    := nPrimaMonedaCob * nTasaCambioPol;
      ELSE ------------------ DE LA MONEDA UIT -------------------------
        Calcula_Montos(p_nIdePol, p_nNumCert, p_cCodProd, p_cCodPlan, p_cRevPlan, p_cCodRamoCert,
            nTasa,nPrimaMoneda,cParam,C.CodCobert,nSumaAsegMoneda,nTipTasa);
        nSumaasegMoneda := nSumaAsegMonedaCob * nTasaCambioPol;
        nPrimaMonedaCob := nPrimaMoneda * nTasaCambioCob;
        nPrima          := nPrimaMonedaCob * nTasaCambio;
        nSumaAseg       := nSumaasegMonedaCob * nTasaCambio;
      END IF;
      Actualizar(nPrima, nPrimaMoneda, nPrimaMonedaCob, nTasa, nSumaAseg, nSumaAsegMoneda,
                 p_nNumMod, C.CodCobert, C.IdeCobert,
                 p_nIdePol, p_nNumCert, p_cCodRamoCert
      );
      cCodMonedaAnt := cCodMoneda;
    END LOOP;
  END Tarifa_M;



  --/**************** POST INSERT EN MOD_COBERT ******************************************/

/*
  FUNCTION Tasa_Cobert(p_cCodCobert COBERT_CERT.CodCobert%TYPE) RETURN NUMBER IS
    nTasaCobert  NUMBER(14,6);
    C            COBERT_PLAN_PROD%ROWTYPE;
  BEGIN
    C := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(:C00.cCodProd,:C00.cCodPlan,:C00.cRevPlan,:C00.cCodRamoCert,p_cCodCobert);
    IF C.TipoRegistroCobert = 'T' THEN
      nTasaCobert  := 1000;
    ELSIF C.TipoRegistroCobert = 'C' THEN
      nTasaCobert  := 100;
    ELSE
      nTasaCobert  := 100;
    END IF;
    RETURN nTasaCobert;
  END;
*/

/*-----------------------------------------------------------------------------
  Nombre      : Cobertura_Referencial
  Proposito   : Cobertura_Referencial
  Referencias : Form                    EMI00137
                When-Button-Pressed     C01.BT_DAT_PART
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_cCodProd              Codigo de producto
                p_cCodPlan              Codigo de Plan
                p_cRevPlan              Revisi�n del Plan
                p_cCodRamoCert          Ramo del certificado
                p_cCodCobertRef         Codigo de Cobertura
                p_nSumaAsegMonedaCob    Suma Asegurada
                p_nPrimaMonedaCob       Prima
                p_nTasa                 Tasa
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           Creaci�n
  ----------------------------------------------------------------------------*/
  PROCEDURE Cobertura_Referencial(
    p_nIdePol              POLIZA.IdePol%TYPE,
    p_nNumCert             COBERT_CERT.NumCert%TYPE,
    p_cCodProd             POLIZA.CodProd%TYPE,
    p_cCodPlan             COBERT_CERT.CodPlan%TYPE,
    p_cRevPlan             COBERT_CERT.RevPlan%TYPE,
    p_cCodRamoCert         COBERT_CERT.CodRamoCert%TYPE,
    p_cCodCobertRef        COBERT_CERT.CodCobert%TYPE,
    p_nSumaAsegMonedaCob   MOD_COBERT.SumaAsegMonedaCob%TYPE,
    p_nPrimaMonedaCob      MOD_COBERT.PrimaMonedaCob%TYPE,
    p_nTasa                MOD_COBERT.Tasa%TYPE
  )
   IS
  nDummy              NUMBER;
  cCodGrupoCobert     GRUPO_COBERT_SUMA_PROD.CodGrupoCobert%TYPE;
  nSumaAsegMoneda     COBERT_CERT.SumaAsegMoneda%TYPE;
  nSumaAsegMonedaCob  COBERT_CERT.SumaAsegMonedaCob%TYPE;
  nPrimaMoneda        MOD_COBERT.PrimaMoneda%TYPE;
  nNumMod             MOD_COBERT.NumMod%TYPE:=0;
  nNumModMax          MOD_COBERT.NumMod%TYPE;
  nSumaAseg            MOD_COBERT.SumaAseg%TYPE;
  nPrima              MOD_COBERT.Prima%TYPE;
  nPrimaMonedaCob     MOD_COBERT.PrimaMonedaCob%TYPE;
  cCodMoneda          MOD_COBERT.CodMoneda%TYPE;
  cCodMonedaAnt        MOD_COBERT.CodMoneda%TYPE := 'X';
  nTasa               MOD_COBERT.Tasa%TYPE;
  nTasaCobert         NUMBER(14,6);
  nExiste             NUMBER(2) := 0;
  nTipoCambio          NUMBER(11,6);
  nTipoCambioPol      NUMBER(11,6);
  nTipoCambioCob      NUMBER(11,6);
  nModAntRev          NUMBER := 0;
  A                   MOD_COBERT%ROWTYPE;
  M                   MOD_COBERT%ROWTYPE;
  B                   COBERT_CERT%ROWTYPE;

  cC00_cCodMonedaLocal       MOD_COBERT.CodMoneda%TYPE;
  cC00_cCodMoneda            MOD_COBERT.CodMoneda%TYPE;
  C                          COBERT_PLAN_PROD%ROWTYPE;

  CURSOR c_Cobert_Grupo IS          -- JF
    SELECT CodGrupoCobert,TipoGruposuma
    FROM   GRUPO_COBERT_SUMA_PROD
    WHERE  CodProd      = p_cCodProd
    AND    CodPlan      = p_cCodPlan
    AND    RevPlan      = p_cRevPlan
    AND    CodRamoPlan  = p_cCodRamoCert
    AND    CodCobertRef = p_cCodCobertRef;
  CURSOR c_Cobert_Suma IS
    SELECT CodCobert,PorcCobertRef
    FROM   COBERT_GRUPO_SUMA_PROD
    WHERE  CodProd        = p_cCodProd
    AND    CodPlan        = p_cCodPlan
    AND    RevPlan        = p_cRevPlan
    AND    CodRamoPlan    = p_cCodRamoCert
    AND    CodGrupoCobert = cCodGrupoCobert
    AND    CodCobert      <> p_cCodCobertRef;

  BEGIN
    cC00_cCodMonedaLocal := PR_VINISUSC.CODIGO_MONEDA;
    --
    SELECT MAX(CodMoneda)
    INTO   cC00_cCodMoneda
    FROM   POLIZA
    WHERE  IdePol = p_nIdePol;
    --
    FOR X IN c_Cobert_Grupo LOOP
      cCodGrupoCobert := X.CodGrupoCobert;
      FOR R IN c_Cobert_Suma LOOP
        B := PR_Cobert_Cert.Datos_Cobert_Cert_Codigo(p_nIdePol, p_nNumCert, p_cCodRamoCert, R.CodCobert);
        nSumaAsegMoneda    := B.SumaAsegMoneda;
        nSumaAsegMonedaCob := B.SumaAsegMonedaCob;

        --nTasaCobert        := Tasa_Cobert(R.CodCobert); --FR_Calc_Tar.Tasa_Cobert(R.CodCobert);
        C := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(p_cCodProd, p_cCodPlan, p_cRevPlan, p_cCodRamoCert, p_cCodCobertRef );
        IF C.TipoRegistroCobert = 'T' THEN
          nTasaCobert  := 1000;
        ELSIF C.TipoRegistroCobert = 'C' THEN
          nTasaCobert  := 100;
        ELSE
          nTasaCobert  := 100;
        END IF;
        --RETURN nTasaCobert;

        --
        IF B.IdeCobert IS NOT NULL THEN
          nExiste := 1;
          nNumModMax := PR_Mod_Cobert.Max_Mod_Cobert(B.IdeCobert);
          IF nNumModMax IS NOT NULL THEN
             M := PR_Mod_Cobert.Datos_Mod_Cobert(B.IdeCobert,nNumModMax);
             nTasa := M.tasa;
             nPrimaMonedaCob := M.PrimaMonedaCob;
             nNumMod         := M.NumMod;
             cCodMoneda      := M.CodMoneda;
             nPrimaMoneda    := M.PrimaMoneda;
          ELSE
             nExiste := 0;
          END IF;
          nModAntRev := 0;
          IF nNumMod > 0 THEN
            BEGIN
              SELECT NVL(MAX(1),0)
              INTO   nModAntRev
              FROM   DUAL
              WHERE  EXISTS ( SELECT 1
                              FROM   MOD_COBERT
                              WHERE  IdeCobert = B.IdeCobert
                                AND  NumMod = nNumMod - 1
                                AND  StsModCobert = 'REV');
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                nModAntRev := 0;
            END;
          END IF;
          IF cCodMoneda <> cCodMonedaAnt THEN
            nTipoCambio    := PR.TASA_CAMBIO(cCodMoneda, cC00_cCodMonedaLocal, SYSDATE, 'D');
            nTipoCambioPol := PR.TASA_CAMBIO(cCodMoneda, cC00_cCodMoneda, SYSDATE, 'D');
            nTipoCambioCob := PR.TASA_CAMBIO(cC00_cCodMoneda, cCodMoneda, SYSDATE, 'D');
          END IF;

          IF X.TipoGrupoSuma = 'S' THEN
             nSumaAsegMonedaCob := p_nSumaAsegMonedaCob * R.PorcCobertRef / 100;
          ELSIF X.TipoGrupoSuma = 'T' THEN
             nTasa := p_nTasa * R.PorcCobertRef / 100;
          END IF;

          IF cCodMoneda <> 'UIT' THEN
            IF X.TipoGrupoSuma = 'P' THEN                                        -- jf
              nPrimaMonedaCob := p_nPrimaMonedaCob * R.PorcCobertRef / nTasaCobert;    -- jf
            END IF;                                                                 -- jf
            IF X.TipoGrupoSuma <>'P' THEN                                            -- jf
              nPrimaMonedaCob := nSumaAsegMonedaCob * NVL(nTasa,0) / nTasaCobert;           -- jf
            END IF;                                                                 -- jf
            nSumaAsegMoneda := nSumaAsegMonedaCob * nTipoCambioPol;
             nPrimaMoneda    := nPrimaMonedaCob * nTipoCambioPol;
            nSumaAseg        := nSumaAsegMonedaCob * nTipoCambio;
            nPrima          := nPrimaMonedaCob * nTipoCambio;
          ELSE
            IF X.TipoGrupoSuma = 'P' THEN
              nPrimaMoneda := p_nPrimaMonedaCob * R.PorcCobertRef / nTasaCobert;
            END IF;
            IF X.TipoGrupoSuma <>'P' THEN
              nPrimaMoneda := nSumaAsegMoneda * NVL(nTasa,0) / nTasaCobert;
            END IF;
            nSumaAsegMoneda := nSumaAsegMonedaCob * nTipoCambioPol;
             nPrimaMonedaCob := nPrimaMoneda * nTipoCambioCob;
            nSumaAseg        := nSumaAsegMonedaCob * nTipoCambio;
            nPrima          := nPrimaMonedaCob * nTipoCambio;
          END IF;

          IF nExiste = 1  AND  B.StsCobert in ('ACT','MOD') OR nModAntRev = 1 THEN  -- Si el valor de nModAntRev = 1, quiere decir que ha sido una reversion y                                                                        -- debe de insertar tambien el movimiento en las coberturas interdependientes
            NULL;
            /*
            BEGIN
              A.IdePol         := :C00.nIdePol;
              A.NumCert        := :C00.nNumCert;
              A.CodRamoCert    := :C00.cCodRamoCert;
              A.NumMod         := :B02_1.NumMod;
              A.SumaAsegMoneda := nSumaAsegMoneda;
              A.Tasa           := NVL(nTasa,0);
              A.PrimaMoneda    := NVL(nPrimaMoneda,0);
              A.FecIniValid    := :B02_1.FecIniValid;
              A.FecFinValid    := :B02_1.FecFinValid;
              A.StsModCobert   := 'VAL';
              A.IdeCobert      := B.IdeCobert;
              A.IndIncRen      := :B02_1.IndIncRen;
              A.OrigModCobert  := 'C';
              A.CodMoneda      := :B02_1.CodMoneda;
              A.CodMonedaCob   := :B02_1.CodMonedaCob;
              A.Prima          := NVL(nPrima,0);
              A.SumaAseg       := NVL(nSumaAseg,0);
              A.SumaAsegMonedaCob := NVL(nSumaAsegMonedaCob,0);
              A.PrimaMonedaCob    := NVL(nPrimaMonedaCob,0);
              PR_Mod_Cobert.Crear(A);
            EXCEPTION
              WHEN DUP_VAL_ON_INDEX THEN
                BEGIN
                  UPDATE MOD_COBERT
                  SET SumaAsegMonedaCob = DECODE(X.TipoGrupoSuma, 'S', NVL(nSumaAsegMonedaCob,0),SumaAsegMonedaCob),
                      Tasa              = DECODE(X.TipoGrupoSuma, 'T', NVL(nTasa,0), Tasa),
                      PrimaMoneda       = DECODE(X.TipoGrupoSuma, 'P', NVL(nPrimaMoneda,0), PrimaMoneda),
                      Prima              = DECODE(X.TipoGrupoSuma, 'P', NVL(nPrima,0), Prima),
                      SumaAseg          = DECODE(X.TipoGrupoSuma, 'S', NVL(nSumaAseg,0), SumaAseg),
                      SumaAsegMoneda    = DECODE(X.TipoGrupoSuma, 'S', NVL(nSumaAsegMoneda,0), SumaAsegMoneda),
                      PrimaMonedaCob    = DECODE(X.TipoGrupoSuma, 'P', NVL(nPrimaMonedaCob,0), PrimaMonedaCob)
                  WHERE  IdeCobert = B.IdeCobert
                    AND  NumMod    = nNumMod;
                EXCEPTION
                  WHEN OTHERS THEN
                    LR_Error('Error al actualizar en MOD_COBERT(3) '||SQLERRM);
                END;
              WHEN OTHERS THEN
                LR_Error('Error al insertar en MOD_COBERT '||SQLERRM);
            END;
            */
          ELSIF nExiste = 1  AND  B.StsCobert = 'VAL' THEN
            BEGIN
              UPDATE COBERT_CERT
              SET SumaAsegMoneda    = NVL(nSumaAsegMoneda,0),
                  Tasa              = NVL(nTasa,0),
                  PrimaMoneda       = NVL(nPrimaMoneda,0),
                  Prima              = NVL(nPrima,0),
                  SumaAseg          = NVL(nSumaAseg,0),
                  SumaAsegMonedaCob = NVL(nSumaAsegMonedaCob,0),
                  PrimaMonedaCob     = NVL(nPrimaMonedaCob,0)
              WHERE  IdeCobert = B.IdeCobert
              AND    StsCobert <> 'ACT';
            EXCEPTION
               WHEN OTHERS THEN
                 --LR_Error('Error al actualizar en MOD_COBERT '||SQLERRM);
                 RAISE_APPLICATION_ERROR(-20101, 'RAE!!! Error al actualizar en MOD_COBERT '||SQLERRM);
            END;
          ELSIF nExiste = 1  AND  B.StsCobert = 'MOD' THEN
            BEGIN
              UPDATE MOD_COBERT
              SET    Prima             = NVL(nPrima,0),
                     PrimaMoneda       = NVL(nPrimaMoneda,0),
                     PrimaMonedaCob    = NVL(nPrimaMonedaCob,0),
                     Tasa              = NVL(nTasa,0),
                     SumaAseg           = NVL(nSumaAseg,0),
                     SumaAsegMoneda    = NVL(nSumaAsegMoneda,0),
                     SumaAsegMonedaCob = NVL(nSumaAsegMonedaCob,0)     -- jf
              WHERE  IdeCobert = B.IdeCobert
                AND  NumMod    = nNumMod
                AND  StsModCobert <> 'ACT';
            EXCEPTION
               WHEN OTHERS THEN
                --LR_Error('Error al actualizar en MOD_COBERT(2) '||SQLERRM);
                RAISE_APPLICATION_ERROR(-20101, 'RAE!!! Error al actualizar en MOD_COBERT(2) '||SQLERRM);
            END;
          END IF;
        END IF;
      END LOOP;
    END LOOP;
  END Cobertura_Referencial;
/*-----------------------------------------------------------------------------
  Nombre      : ADD_COBERT_CERT
  Proposito   : A�adir Coberturas del Certificado para Trabajadores 3D
  Referencias : Form                    MANTCOBC
                When-Button-Pressed     TB_V02.BT_INCLUIRCOB
  Parametros  : p_nIdePol               Identificador de la poliza
                p_nNumcert              Numero del certificado
                p_dFecIniVig            fecha inicio de vigencia del certificado
                p_dFecFinVig            fecha fin de vigencia del certificado
                p_nPrima                Prima
        p_nTasa                 Tasa
        p_nSumaAsegurada        Suma Asegurada
                p_cCodRamoCert          Codigo de Ramo
                p_cCodCobert            Codigo de Cobertura

  Retorno       p_cError      Codigo de Error en caso se encuentre un error
                              0 : OK  , 1 : Error
  LOGdeCAMBIO!
  Fecha        Autor                    Descripcion
  2022/03      Ronald Barreda           <RTC 313092> Creaci�n
  2022/04/06   Ronald Barreda           <RTC 313092> Modificaci�n : Mejora en variable de retorno p_cError
  2022/05/24   Ronald Barreda           <RTC-313092> Actualizaci�n de par�metros de procedimiento Add_Cobert_Cert
  22/08/2022   Christian Bayona         <RTC-314183> Se adicion� validaci�n de coberturas
  ----------------------------------------------------------------------------*/
PROCEDURE Add_Cobert_Cert(
  p_nIdePol               POLIZA.IdePol%TYPE,
  p_nNumCert              CERT_RAMO.NumCert%TYPE,
  p_dFecIniVig            DATE,
  p_dFecFinVig            DATE,
  p_nPrima                IN OUT MOD_COBERT.PrimaFact%TYPE,
  p_nTasa                 IN OUT MOD_COBERT.Tasa%TYPE,
  p_nSumaAsegurada        IN OUT MOD_COBERT.SumaAsegMoneda%TYPE,
  p_cCodRamoCert          CERT_RAMO.CodRamoCert%TYPE,
  p_cCodCobert            COBERT_CERT.CodCobert%TYPE,
  p_cError                OUT varchar2
) IS
--
nIdeCobert                NUMBER;
nMaxNumMod                NUMBER;
nExiste                   NUMBER;
cCodProd                  POLIZA.CodProd%TYPE;
cCodPlan                  Cobert_Cert.CodPlan%TYPE;
cRevPlan                  Cobert_Cert.RevPlan%TYPE;
--
rCobert_Cert              COBERT_CERT%ROWTYPE;
rMod_Cobert               MOD_COBERT%ROWTYPE;
--
nSecuencia                LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
--
nTasaModCobert            MOD_COBERT.Tasa%TYPE;
nSAmodCobert              MOD_COBERT.SUMAASEGMONEDAcob%TYPE;
nPrimaCobert              MOD_COBERT.PrimaMoneda%TYPE;
bSA_Tasa_Prima            BOOLEAN := FALSE;
--
nTasaCambio               NUMBER(14,9);
nTasaCambioCob            NUMBER(14,9);
nTasaCambioPol            NUMBER(14,9);
nTipTasa                  NUMBER(4);
cCodMonPol                POLIZA.CodMoneda%TYPE;
--
P                         COBERT_PLAN_PROD%ROWTYPE;
--
--
PROCEDURE FR_RECALC_PRIMA_MONEDA(
  p_rCC               IN       COBERT_CERT%ROWTYPE,
  p_cCodProd          IN       POLIZA.CodProd%TYPE,
  p_nPrimaMoneda      IN OUT   MOD_COBERT.PrimaMoneda%TYPE,
  p_nSumaAsegMoneda   IN OUT   MOD_COBERT.SumaAsegMoneda%TYPE,
  p_nTasa             IN OUT   MOD_COBERT.Tasa%TYPE,
  p_cParam            IN       VARCHAR2,
  p_nTipTasa          IN       NUMBER
) IS
BEGIN
  DBMS_OUTPUT.PUT_LINE('p_nPrimaMoneda:'||p_nPrimaMoneda);
  DBMS_OUTPUT.PUT_LINE('p_nSumaAsegMoneda:'||p_nSumaAsegMoneda);
  DBMS_OUTPUT.PUT_LINE('p_nTasa:'||p_nTasa);
  DBMS_OUTPUT.PUT_LINE('p_cParam:'||p_cParam);
  DBMS_OUTPUT.PUT_LINE('p_nTipTasa:'||p_nTipTasa);
  --
  IF NVL(p_nSumaAsegMoneda,0) <> 0 THEN
    DBMS_OUTPUT.PUT_LINE('a');
    IF NVL(p_nTasa,0) = 0   AND NVL(p_nPrimaMoneda,0) <> 0  THEN
      p_nTasa:= (p_nPrimaMoneda * p_nTipTasa) / p_nSumaAsegMoneda;
      DBMS_OUTPUT.PUT_LINE('aa p_nTasa:'||p_nTasa);
    ELSE
      p_nPrimaMoneda := (p_nSumaAsegMoneda* p_nTasa) / p_nTipTasa;
      DBMS_OUTPUT.PUT_LINE('aaa p_nPrimaMoneda:'||p_nPrimaMoneda);
    END IF;
  ELSE
    p_nPrimaMoneda := 0;
  END IF;

  IF p_cParam IS NOT NULL THEN
    DBMS_OUTPUT.PUT_LINE('IF p_cParam IS NOT NULL THEN >>>>>>>');
    IF p_nPrimaMoneda IS NULL OR p_nPrimaMoneda = 0 THEN
      DBMS_OUTPUT.PUT_LINE('b');
      p_nPrimaMoneda := PR_PRE_COBERT.POST_PRIMA(p_rCC.IdePol, p_rCC.NumCert, p_cCodProd, p_rCC.CodPlan,
                        p_rCC.RevPlan, p_rCC.CodRamoCert, p_rCC.CodCobert, p_cParam, NULL, p_nSumaAsegMoneda);
    END IF;

    IF p_nTasa IS NULL OR p_nTasa = 0 THEN
       DBMS_OUTPUT.PUT_LINE('bb');
       p_nTasa := NVL(PR_PRE_COBERT.PRE_TASA (p_rCC.IdePol, p_rCC.NumCert, p_cCodProd, p_rCC.CodPlan,
                      p_rCC.RevPlan, p_rCC.CodRamoCert, p_rCC.CodCobert, p_cParam, NULL), 0);
    END IF;

    IF p_nPrimaMoneda IS NULL OR p_nPrimaMoneda = 0 THEN
       DBMS_OUTPUT.PUT_LINE('bbb');
       p_nPrimaMoneda := NVL(PR_PRE_COBERT.PRE_PRIMA(p_rCC.IdePol, p_rCC.NumCert, p_cCodProd, p_rCC.CodPlan,
                            p_rCC.RevPlan, p_rCC.CodRamoCert, p_rCC.CodCobert, p_cParam, NULL), 0);
    END IF;

    IF (p_nTasa IS NULL OR p_nTasa = 0 AND p_nSumaAsegMoneda <> 0) AND
       (p_nPrimaMoneda IS NOT NULL OR p_nPrimaMoneda <> 0 ) THEN
        DBMS_OUTPUT.PUT_LINE('bbbb');
        p_nTasa := (p_nPrimaMoneda * p_nTipTasa) / p_nSumaAsegMoneda;
    END IF;
  END IF;
END;
--
--
BEGIN
  --<I RTC-313752> CBayona / 22-08-2022 / Se agreg� validaci�n de Coberturas
  p_cError := Valida_Cobertura(p_nIdePol, p_nNumCert, p_cCodRamoCert, p_cCodCobert);
  DBMS_OUTPUT.Put_Line('>>>> Validaci�n de Cobertura: ' || p_cCodCobert || ' => ' || p_cError);
  --
  IF p_cError != '0,OK' THEN
    RETURN;
  END IF;
  --p_cError := '0,OK';
  --<F RTC-313752>
  --
  dbms_output.put_line('~ ~ ~ ~ ~ BEGIN 01 ! Add_Cobert_Cert');
  --
  dbms_output.put_line('p_nIdePol:'||p_nIdePol);
  dbms_output.put_line('p_nNumCert:'||p_nNumCert);
  --
  SELECT MAX(CodProd), MAX(CodMoneda)
  INTO   cCodProd, cCodMonPol
  FROM   POLIZA
  WHERE  IdePol = p_nIdePol;
  --
  dbms_output.put_line('cCodProd:'||cCodProd);
  --
  SELECT MAX(idecobert)
  INTO   nIdeCobert
  FROM   cobert_cert
  WHERE  idepol  = p_nIdePol
    AND  numcert = p_nNumCert
    AND  stscobert in ('ACT', 'MOD')
    AND  codramocert = p_cCodRamoCert
    AND  codcobert   = p_cCodCobert;
  --
  dbms_output.put_line('nIdeCobert:'||nIdeCobert);
  --
  SELECT *
  INTO   rCobert_Cert
  FROM   Cobert_Cert
  WHERE  IdeCobert = nIdeCobert;
  --
  dbms_output.put_line('cCodPlan:'||cCodPlan);
  dbms_output.put_line('cRevPlan:'||cRevPlan);
  --
  P := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(
         cCodProd, rCobert_Cert.CodPlan, rCobert_Cert.RevPlan, p_cCodRamoCert, p_cCodCobert);
  --
  BEGIN
    nMaxNumMod := PR_Mod_Cobert.Max_Mod_Cobert(nIdeCobert);
  EXCEPTION
    WHEN OTHERS THEN
      dbms_output.put_line('WOT');
      nMaxNumMod := 0;
  END;
  --
  --select NVL(PR_Mod_Cobert.Max_Mod_Cobert(nIdeCobert),0)+1 into   nMaxNumMod from   dual;
  nMaxNumMod := nvl(nMaxNumMod,0) + 1;
  dbms_output.put_line('nMaxNumMod:'||nMaxNumMod);
  --
  IF p_nSumaAsegurada IS NOT NULL AND
     p_nTasa          IS NOT NULL AND
     p_nPrima         IS NOT NULL THEN
       bSA_Tasa_Prima := TRUE;
  END IF;
  --
  IF NOT bSA_Tasa_Prima THEN
      SELECT MAX(Tasa), MAX(SUMAASEGMONEDAcob), MAX(PrimaMoneda)
      INTO   nTasaModCobert, nSAmodCobert, nPrimaCobert
      FROM   MOD_COBERT
      WHERE  IdeCobert = nIdeCobert;
      dbms_output.put_line('nPrimaCobert:'||nPrimaCobert);
      --
      IF NVL(p_nPrima,0) <> 0 AND NVL(p_nTasa,0) = 0 AND NVL(p_nSumaAsegurada,0) = 0 THEN
         dbms_output.put_line('IF NVL(p_nPrima,0) <> 0 AND IF NVL(p_nTasa,0) = 0 AND IF NVL(p_nSumaAsegurada,0) = 0 THEN');
         p_nSumaAsegurada := nSAmodCobert;
         dbms_output.put_line('nSAmodCobert:'||nSAmodCobert);
      END IF;
      --
      IF NVL(p_nPrima,0) = 0 AND NVL(p_nTasa,0) = 0 AND NVL(p_nSumaAsegurada,0) <> 0 THEN
         dbms_output.put_line('IF NVL(p_nPrima,0) = 0 AND IF NVL(p_nTasa,0) = 0 AND IF NVL(p_nSumaAsegurada,0) <> 0 THEN');
         p_nTasa := nTasaModCobert;
         dbms_output.put_line('nTasaModCobert:'||nTasaModCobert);
      END IF;
  END IF;
  --
  select *
  into   rMod_Cobert
  from   MOD_COBERT
  where  idecobert = nIdeCobert
    and  rownum    < 2;
  --
  PR_VINISUSC.CARGAR;
  nTasaCambio    := PR.TASA_CAMBIO(rMod_Cobert.CodMonedaCob, PR_VINISUSC.CODIGO_MONEDA, SYSDATE,'D');
  dbms_output.put_line('nTasaCambio:'||nTasaCambio);
  nTasaCambioCob := PR.TASA_CAMBIO(cCodMonPol, rMod_Cobert.CodMonedaCob, SYSDATE,'D');
  dbms_output.put_line('nTasaCambioCob:'||nTasaCambioCob);
  nTasaCambioPol := PR.TASA_CAMBIO(rMod_Cobert.CodMonedaCob, cCodMonPol, SYSDATE,'D');
  dbms_output.put_line('nTasaCambioPol:'||nTasaCambioPol);
  --
  nTipTasa       := PR.BUSCA_TIPO_TASA_PROD(
                    cCodProd, rMod_Cobert.CodRamoCert, rCobert_Cert.CodPlan, rCobert_Cert.RevPlan, p_cCodCobert);
  dbms_output.put_line('nTipTasa:'||nTipTasa);
  --
  dbms_output.put_line('P.ParamPreCobert:'||P.ParamPreCobert);
  --
  IF rMod_Cobert.CodMonedaCob = 'UIT' THEN
    dbms_output.put_line('IF rMod_Cobert.CodMonedaCob = UIT THEN');
    rMod_Cobert.SumaAsegMoneda    := p_nSumaAsegurada * nTasaCambioPol;
    IF NOT bSA_Tasa_Prima THEN
       FR_RECALC_PRIMA_MONEDA(rCobert_Cert, cCodProd, p_nPrima, p_nSumaAsegurada, p_nTasa, P.ParamPreCobert, nTipTasa);
    END IF;
    rMod_Cobert.Tasa              := p_nTasa;
    rMod_Cobert.SumaAseg          := p_nSumaAsegurada * nTasaCambio;
    rMod_Cobert.SumaAsegMonedaCob := p_nSumaAsegurada;
    rMod_Cobert.PrimaMoneda       := p_nPrima; --<--
    rMod_Cobert.PrimaMonedaCob := rMod_Cobert.PrimaMoneda    * nTasaCambioCob;
    rMod_Cobert.Prima          := rMod_Cobert.PrimaMonedaCob * nTasaCambio;
  END IF;
  --
  IF rMod_Cobert.CodMonedaCob <> 'UIT' THEN
    dbms_output.put_line('IF rMod_Cobert.CodMonedaCob <> UIT THEN');
    IF NOT bSA_Tasa_Prima THEN
       FR_RECALC_PRIMA_MONEDA(rCobert_Cert, cCodProd, p_nPrima, p_nSumaAsegurada, p_nTasa, P.ParamPreCobert, nTipTasa);
       dbms_output.put_line('p_nPrima:'||p_nPrima);
       dbms_output.put_line('p_nTasa:'||p_nTasa);
    END IF;
    rMod_Cobert.Tasa              := p_nTasa;
    dbms_output.put_line('p_nSumaAsegurada:'||p_nSumaAsegurada);
    dbms_output.put_line('nTasaCambio:'||nTasaCambio);
    dbms_output.put_line('nTasaCambioPol:'||nTasaCambioPol);
    rMod_Cobert.SumaAseg          := p_nSumaAsegurada * nTasaCambio;
    dbms_output.put_line('rMod_Cobert.SumaAseg:'||rMod_Cobert.SumaAseg);
    rMod_Cobert.SumaAsegMoneda    := p_nSumaAsegurada * nTasaCambioPol;
    dbms_output.put_line('rMod_Cobert.SumaAsegMoneda:'||rMod_Cobert.SumaAsegMoneda);
    rMod_Cobert.SumaAsegMonedaCob := p_nSumaAsegurada;
    rMod_Cobert.PrimaMonedaCob    := p_nPrima; --<--
    rMod_Cobert.PrimaMoneda := rMod_Cobert.PrimaMonedaCob * nTasaCambioPol;
    rMod_Cobert.Prima       := rMod_Cobert.PrimaMonedaCob * nTasaCambio;
  END IF;
  --
  insert into mod_cobert(
    primafactmoneda, idepol, numcert, codramocert, idecobert, NumMod, origmodcobert,
    stsmodcobert, codoper, sumaasegmoneda, tasa, primamoneda,
    codmoneda, FecIniValid, FecFinValid, indincren,
    indsiniestro, primamovfactor, sumaaseg, prima,
    codmonedacob, primafactmonedacob, sumaasegmonedacob, primamonedacob,sumaasegmonedacobtotal
  )
  values (
    rMod_Cobert.primamoneda, rMod_Cobert.idepol, rMod_Cobert.numcert, rMod_Cobert.codramocert, rMod_Cobert.idecobert, nMaxNumMod, rMod_Cobert.origmodcobert,
    'VAL', rMod_Cobert.codoper, rMod_Cobert.sumaasegmoneda, rMod_Cobert.tasa, rMod_Cobert.primamoneda,
    rMod_Cobert.codmoneda, p_dFecIniVig, p_dFecFinVig, rMod_Cobert.indincren,
    rMod_Cobert.indsiniestro, rMod_Cobert.primamoneda, rMod_Cobert.sumaaseg, rMod_Cobert.Prima,
    rMod_Cobert.codmonedacob, rMod_Cobert.primamonedacob, rMod_Cobert.sumaasegmonedacob, rMod_Cobert.primamonedacob,
    rMod_Cobert.sumaasegmonedacobtotal
    );
  dbms_output.put_line('insert into mod_cobert:' || sql%rowcount);
  --
  SELECT NVL(MAX(1),0)
  INTO   nExiste
  FROM   DUAL
  WHERE EXISTS (SELECT 1
        FROM   GRUPO_COBERT_SUMA_PROD
        WHERE  CodProd      = cCodProd
        AND    CodPlan      = rCobert_Cert.CodPlan
        AND    RevPlan      = rCobert_Cert.RevPlan
        AND    CodRamoPlan  = p_cCodRamoCert
        AND    CodCobertRef = p_cCodCobert);
  --
  dbms_output.put_line('nExiste:'||nExiste);
  --
  IF nExiste > 0 THEN
    dbms_output.put_line('go : PR_INTERFASE_AX.Cobertura_Referencial');
    PR_INTERFASE_AX.Cobertura_Referencial( -- FR_Calc_Tar.Cobertura_Referencial;
      p_nIdePol,
      p_nNumCert,
      cCodProd,
      rCobert_Cert.CodPlan,
      rCobert_Cert.RevPlan,
      p_cCodRamoCert,
      p_cCodCobert,
      rMod_Cobert.SumaAsegMonedaCob   ,
      rMod_Cobert.PrimaMonedaCob      ,
      rMod_Cobert.Tasa
    );
  END IF;
  --
  dbms_output.put_line('go : Activar_Mod_Cobert');
  PR_INTERFASE_AX.Activar_Mod_Cobert(
      p_nIdePol,
      cCodProd,
      p_nNumCert,
      p_cCodRamoCert,
      rMod_Cobert.FecFinValid,
      nIdeCobert,
      nMaxNumMod
    );
EXCEPTION
  WHEN OTHERS THEN
       p_cError := '1,'||substr(SQLERRM,1,498);
       ACSELX.PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR',SYSDATE,SYSDATE,'Add_Cobert_Cert','',nSecuencia );
END Add_Cobert_Cert;
-- <FIN RTC 313092>


-- <INI RTC 313091> FSalinas

/*-----------------------------------------------------------------------------
  Nombre      : Mod_Nuevo_Formato
  Proposito   : Componente para registro de anexo de polizas con plan nuevo formato
  Referencias : <RTC-313091> Endosos de Condiciones Especiales
  Parametros  : p_nIdePol                Identificador de la poliza
                p_TABLA_POLIZA_OPERACION Tabla con datos del formato

  Retorno       p_cError      Codigo de Error en caso se encuentre un error
                              0 : OK  , 1 : Error
  Log de Cambios
    Fecha         Autor                      Descripci�n
    08/04/2022    Franklin Salinas           Creaci�n
    20/04/2022    Jorge Benel                <RTC 313343> Actualizaci�n de parametros y logica integral del paquete
  ----------------------------------------------------------------------------*/
PROCEDURE Mod_Nuevo_Formato(  p_nIdePol                 IN Poliza.IdePol%TYPE,
                              p_TABLA_POLIZA_OPERACION  IN ACSELX.TDU_TABLA_POLIZA_OPERACION,
                              p_cError                  OUT VARCHAR2) IS

    vStsPol      POLIZA.StsPol%TYPE;
    P_NTEXTO     number;
    vdOPerac     varchar2(1000);
    vnOrigen     number;
    VIndCreacion varchar2(10);
    VCodSec      varchar2(5);
    VCodEndoso   varchar2(250);
    VFECFIN      POLIZA.FECFINVIG%type;
    cValor       varchar2(5);
    vcDesSecc    varchar2(50);
    PcodEndoso  POLIZA_OPERACION.CodEndoso%TYPE;
    nFlag             varchar2(1);
    nNumOper          OPER_POL_T.NUMOPER%TYPE;
    rPoliza_Operacion POLIZA_OPERACION%ROWTYPE;
    vCodAccion        VARCHAR2(50);
    pNumOper POLIZA_OPERACION.NumOper%TYPE;
    nOper number;
    nPlan number;
    VOPER number;
    vDetEndos  POLIZA_OPERACION.DetEndos%TYPE;
    k number(5);

    -- validar
    cStsPol         poliza.stspol%type;
    nNumPol         POLIZA.NUMPOL%TYPE;
    cCodProd        POLIZA.CODPROD%TYPE;
    dFecIniVigPol   poliza.fecinivig%type;
    dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
    bExistePoliza   BOOLEAN := TRUE;

    l_text VARCHAR2(210) ;
    l_idx NUMBER;
    l_seq           log_poliza_sas_ax.secuencia%type;

     CURSOR c_pol_oper IS
      SELECT IdePolOper,
             IdePol,
             NumCert,
             CodAccion,
             CodEndoso,
             CodSec,
             IndCreacion,
             FecIni,
             FecFin,
             DetEndos
        FROM POLIZA_OPERACION
       WHERE IdePol = p_nIdePol
         AND NumOper IS NULL
         AND CodEndoso = PcodEndoso;


 PROCEDURE VALIDA AS
   v NUMBER(3);
 BEGIN
   -- Obtenemos informacion de la poliza
   bExistePoliza  := ACSELX.PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );

 END VALIDA;

  BEGIN
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('I' ,p_nidepol, null, null, 'LOG', null, sysdate, sysdate, 'MOD_NUEVO_FORMATO', '', l_seq);

    valida;

    p_cError := '0,OK';
     BEGIN
      SELECT COUNT(1)
        INTO nOper
        FROM OPER_POL
       WHERE IdePol = p_nIdePol
         AND TipoOp = 'EMI';
    EXCEPTION
      WHEN OTHERS THEN
        p_cError := '1,' ||SQLERRM;
    END;

    BEGIN
      SELECT COUNT(1)
        INTO nPlan
        FROM CERT_RAMO C, RAMO_PLAN_PROD R
       WHERE C.IdePol = p_nIdePol
         AND R.CodProd = PR.BUSCA_LVAL('PRTIENDO', '01')
         AND C.CodPlan = PR.BUSCA_LVAL('PLTIENDO', '01')
         AND C.RevPlan = PR.BUSCA_LVAL('RVTIENDO', '01')
         AND C.CodPlan = R.CodPlan
         AND C.RevPlan = R.RevPlan
         AND C.CodRamoCert = R.CodRamoPlan;
    EXCEPTION
      WHEN OTHERS THEN
        p_cError := '1,' ||SQLERRM;
    END;

    DBMS_OUTPUT.PUT_LINE('nOper = '|| nOper ||' ,  nPlan = '||  nPlan );

  IF nPlan > 0 AND nOper > 0 THEN

    IF Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION.COUNT > 0 THEN

      FOR i in Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION.First .. Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION.Last LOOP

        k := 0;
        l_text     := Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).CODSEC ||',';

        LOOP

          l_idx := INSTR(l_text,',');

          SELECT TRIM(SUBSTR(l_text,1,l_idx -1)) INTO vcodsec FROM DUAL;

          l_text := SUBSTR(l_text, l_idx + 1);
          EXIT WHEN NVL(l_idx,0) = 0;

          PcodEndoso := Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).CODENDOSO;

          BEGIN
            SELECT DesEndoso, Origen
            INTO vdOPerac, vnOrigen
            FROM TIPO_ENDOSO
            WHERE CodEndoso = PcodEndoso;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              vdOPerac := ' ';
              vnOrigen := null;
            WHEN OTHERS THEN
              p_cError := '1,'||SQLERRM;
          END;

          IF PR.EXISTE_LVAL('TXTENDLB', PcodEndoso) = 'S' THEN
            VdOperac     := PR.BUSCA_LVAL('TXTENDLB', PcodEndoso);
            VIndCreacion := 'S';
            VCodEndoso   := PcodEndoso;
            VnOrigen     := VnOrigen;
          ELSE
            VdOperac     := PR_POLIZA_OPERACION.Generar_Texto_Endoso(PcodEndoso,vCodSec );
            VIndCreacion := 'S';
            VCodEndoso   := PcodEndoso;
            VnOrigen     := vnOrigen;
          END IF;

          IF PR.EXISTE_LVAL('TXTENDLB', PcodEndoso) = 'S' AND P_NTEXTO = 3 THEN
            VIndCreacion := 'N';
            P_NTEXTO     := 0;
          ELSE
            VIndCreacion := 'S';
          END IF;

          IF VNORIGEN = 7 THEN
            SELECT FECFINVIG
            INTO VFECFIN
            FROM POLIZA
            WHERE IDEPOL = p_nIdePol;
          END IF;

          BEGIN
            SELECT COUNT(*)
            INTO VOPER
            FROM POLIZA_OPERACION
            WHERE IdePol = p_nIdePol
             AND NumOper IS NULL
             ;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
                VOPER := 0;
          END;
           VOPER := 0;

          IF VOPER > 0 THEN
            FOR z IN c_pol_oper LOOP
              IF VNORIGEN = 7 AND (z.FecIni IS NULL OR z.FecFin IS NULL) THEN
                cValor    := 'N';
                p_cError := '1,Las fechas de vigencia no pueden estar vacias si el tipo de endoso lo requiere';
              ELSE
                cValor := 'S';
              END IF;

              IF cValor = 'S' Then
                IF z.IdePolOper IS NULL THEN
                  IF PR.EXISTE_LVAL('TXTENDLB', PcodEndoso) = 'S' THEN
                    vDetEndos := VdOperac || ' :' || CHR(10) || p_TABLA_POLIZA_OPERACION(i).TEXTOANEXO; --
                  ELSE
                    vDetEndos := Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).TEXTOANEXO; --
                  END IF;
                ELSE
                   vDetEndos := Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).TEXTOANEXO; --
                END IF;

                IF z.IndCreacion = 'N' THEN
                  nFlag := 1;
                  BEGIN
                    UPDATE POLIZA_OPERACION
                       SET IndCreacion = 'N',
                           DetEndos = vDetEndos,
                           fecini  = Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).FECHAINI
                     WHERE IdePolOper = z.IdePolOper
                       AND IdePol = p_nIdePol
                       AND NumCert = NVL(z.NumCert, NumCert)
                       AND CodAccion = 'ENDOSO'
                       AND IndCreacion = 'S';
                  EXCEPTION
                    WHEN OTHERS THEN
                      p_cError := '1,' || SQLERRM;
                  END;

                ELSIF z.IndCreacion = 'S' AND pNumOper IS NULL AND z.IdePolOper IS NOT NULL THEN
                  nFlag := 1;
                  BEGIN

                    UPDATE POLIZA_OPERACION
                    SET DetEndos = vDetEndos,
                           fecini  = Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).FECHAINI
                    WHERE IdePolOper = z.IdePolOper
                       AND IdePol = p_nIdePol
                       AND NumCert = NVL(z.NumCert, NumCert)
                       AND CodAccion = 'ENDOSO'
                       AND IndCreacion = 'S';

                  EXCEPTION
                    WHEN OTHERS THEN
                      p_cError := '1,' ||SQLERRM;
                  END;
                END IF;
              END IF; --  cValor = 'S'
            END LOOP; -- z

          ELSE
             dbms_output.put_line( 'VnOrigen = '||VnOrigen);
            IF VnOrigen <> 7 THEN
              BEGIN
                SELECT NVL(MAX(NumOper), 0)
                INTO nNumOper
                FROM OPER_POL_T
                WHERE IdePol = p_nIdePol;
              EXCEPTION
                WHEN OTHERS THEN
                  p_cError := '1, ' ||SQLERRM;
              END;

              IF nNumOper = 0 THEN
                BEGIN
                  SELECT Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).FECHAINI,  Mod_Nuevo_Formato.p_TABLA_POLIZA_OPERACION(i).FECHAFIN
                      FecFinVig
                  INTO rPoliza_Operacion.FecIni, rPoliza_Operacion.FecFin
                  FROM POLIZA
                  WHERE IdePol = p_nIdePol;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    p_cError := '1, ' ||to_char(p_nIdePol) || ' - ' || SQLERRM;
                END;
              ELSE
                BEGIN
                  SELECT MAX(FecIniVig), MAX(FecFinVig)
                  INTO rPoliza_Operacion.FecIni, rPoliza_Operacion.FecFin
                  FROM RECIBO_T
                  WHERE IdeOP = nNumOper;
                EXCEPTION
                  WHEN OTHERS THEN
                     p_cError := '1,' ||SQLERRM;
                END;
              END IF;
            ELSE
              rPoliza_Operacion.FecIni := p_TABLA_POLIZA_OPERACION(i).FECHAINI;
              rPoliza_Operacion.FecFin := VFECFIN;
            END IF;

            rPoliza_Operacion.IdePolOper  := '';
            rPoliza_Operacion.IdePol      := p_nIdePol;
            rPoliza_Operacion.NumCert     := 0;
            rPoliza_Operacion.CodAccion   := 'ENDOSO';
            rPoliza_Operacion.CodEndoso   := PcodEndoso;
            rPoliza_Operacion.CodSec      := VCodSec;
            rPoliza_Operacion.IndCreacion := VIndCreacion;
            rPoliza_Operacion.DetEndos    := p_TABLA_POLIZA_OPERACION(i).TEXTOANEXO;

            IF rPoliza_Operacion.IdePolOper IS NULL AND rPoliza_Operacion.CodEndoso IS NOT NULL THEN
              BEGIN
                PR_POLIZA_OPERACION.Crear_Pol_Oper(rPoliza_Operacion);
              EXCEPTION
                WHEN OTHERS THEN
                  p_cError := '1,ERROR EN EL INGRESO DEL REGISTRO ' || SQLERRM;
              END;
            END IF;
          END IF; -- IF VOPER > 0 THEN
        END LOOP; -- p_TABLA_CODSEC.COUNT
      END LOOP;   -- p_TABLA_POLIZA_OPERACION(i).CODENDOSO;
    END IF; -- p_TABLA_POLIZA_OPERACION.COUNT
  END IF;

  pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F', p_nidepol, null, null, 'LOG', 'OK', sysdate, sysdate, 'MOD_NUEVO_FORMATO', '', l_seq);

  exception
    when others then
      p_cerror := '1,' || sqlerrm;
      pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F', p_nidepol, null, null, 'LOG', 'ERR', sysdate, sysdate, 'MOD_NUEVO_FORMATO', '', l_seq);
END Mod_Nuevo_Formato;

-- <FIN RTC 313091> FSalinas


--
-- <INI RTC 313344 > RBarreda
--
/*-----------------------------------------------------------------------
   Nombre     : INSERTA_ENDOSATARIO_CERT
   Proposito  : Inserci�n de Endosatario a nivel de Certificado/Ramo
   Referencia : Formulario EMI00012.FMB
   Parametro  : p_nIdePol          Numero Interno de poliza de seguros
                p_nNumcert         Numero de certificado
                p_nNumId           TipoId del Endosatario
                p_dFecIniVig       Fecha Inicio de Vigencia del Endoso
                p_dFecFinVig       Fecha Fin    de Vigencia del Endoso
                p_nMtoEndoso       Monto de Endoso
                p_cTxtEndoso       Texto de Endoso
                p_cTipoEndoso      Tipo de Endoso
                p_cCodTexto        Codigo de Texto del Endoso
                p_nPorcPart        Porcentaje de Participaci�n del Monto de Endoso
                p_cCodCli          Codigo de Cliente/Asegurado
                p_Aprobado         Flag de Aprobacion
                p_Sustento         Texto del sustento
                p_usuarioaprob      Codigo de usuario autorizador del sustento

   Retorno :    p_cError         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                                   0, OK
                                   1, Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   13/04/2022   Ronald Barreda      <RTC 313344> Creaci�n.
   20/04/2022   Ronald Barreda      <RTC 313344> Se quita TipoId y DvId como parametro
   22/08/2022   Christian Bayona    <RTC 314183> Se agregan validaciones
   16/09/2022   Christian Bayona    <RTC 314175> Se agrega validaci�n
   10/01/2023   Christian Bayona    <RTC 316106> Se actualiza validaci�n
   26/05/2023   Manuel Navarro      <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B, Suma Asegurada VB Tech Core
-----------------------------------------------------------------------------*/
PROCEDURE INSERTA_ENDOSATARIO_CERT (
  p_nIdePol     ENDOSATARIO_CERT.IdePol%TYPE,
  p_nNumCert    ENDOSATARIO_CERT.NumCert%TYPE,
  p_nNumId      ENDOSATARIO_CERT.NumId%TYPE,
  p_dFecIniVig  DATE,
  p_dFecFinVig  DATE,
  p_nMtoEndoso  ENDOSATARIO_CERT.MtoEndoso%TYPE,
  p_cTxtEndoso  ENDOSATARIO_CERT.TextoBien%TYPE,
  p_cTipoEndoso ENDOSATARIO_CERT.TipoEnd%TYPE,
  p_cCodTexto   ENDOSATARIO_CERT.CodTexto%TYPE,
  p_nPorcPart   ENDOSATARIO_CERT.PorcPart%TYPE   DEFAULT NULL,
  p_cCodCli     ENDOSATARIO_CERT.CodCli%TYPE     DEFAULT NULL,     -- Form Label Item : Asegurado
--<I RTC 316878> Manuel Navarro / 26-05-2023: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B, Suma Asegurada VB Tech Core
  p_Aprobado    Varchar2,
  p_Sustento    Varchar2 ,
  p_usuarioaprob Varchar2 ,
--<F RTC 316878>

  p_cError      OUT VARCHAR2
) IS
--
cTipoId     ENDOSATARIO_CERT.TipoId%TYPE;
cDvId       ENDOSATARIO_CERT.TipoId%TYPE;
cCodCli     ENDOSATARIO_CERT.CodCli%TYPE;
dFecIng     DATE := SYSDATE;
cRestricSAS VARCHAR2(1000);
cCodTexto   ENDOSATARIO_CERT.CodTexto%TYPE;
nIdeEndCert ENDOSATARIO_CERT.IdeEndCert%TYPE;
cNomter     TERCERO.NomTer%TYPE;
nPorcPart   ENDOSATARIO_CERT.PorcPart%TYPE;
nSumaAseg   NUMBER(21,2);
nSecuencia  LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
--
nNumPol         POLIZA.NUMPOL%TYPE;
cStsPol         POLIZA.STSPOL%TYPE;
cCodProd        POLIZA.CODPROD%TYPE;
dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
bExiste         BOOLEAN := TRUE;

--<I RTC 316878> Manuel Navarro / 26-05-2023: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B, Suma Asegurada VB Tech Core
nFlagSustento        INTEGER:=0;
n_nummod             NUMBER;
--<F RTC 316878>

--
BEGIN
  p_cError := '0,OK';
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,p_nNumcert,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'CAMBIO_ENDOSATARIO_CERT','',nSecuencia );
  --
  -- Validacion del estado la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de poliza'; RETURN; END IF;
  -- Validacion del estado de la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de estado de poliza'; RETURN; END IF;
  -- Validar si existe el certificado
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_EXISTE_CERTIFICADO( p_nIdePol , p_nNumCert );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de certificado'; RETURN; END IF;
  --
  IF (p_dFecFinVig > dFecFinVigPol) OR (p_dFecIniVig > dFecFinVigPol) THEN
    p_cError := '1,Fecha de Endoso supera a fecha fin de vigencia de poliza'; RETURN;
  END IF;
  --<I RTC 313344> CBayona / 12-09-2022 / Se agrega validaci�n
  IF (p_dFecFinVig < dFecIniVigPol) OR (p_dFecIniVig < dFecIniVigPol) THEN
    p_cError := '1,Fecha de Endoso es anterior a fecha inicio de vigencia de p�liza'; RETURN;
  END IF;
  --<F RTC 313344>
  --
  --<I RTC 313344> CBayona / 22-08-2022 / Validar que monto del endoso no sea nulo
  IF p_nMtoEndoso IS NULL THEN
    p_cError := '1,Monto del Endoso no puede ser nulo';
    RETURN;
  END IF;
  --<F RTC 313344>
  --
  cRestricSAS := pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
  --
  IF NOT cRestricSAS IS NULL THEN
     p_cError := '1,' || cRestricSAS;
     RETURN;
  END IF;
  --


  --<I RTC 316878> Manuel Navarro: / 26-05-2023 CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B, Suma Asegurada VB Tech Core
  nFlagSustento := VALIDA_SUMAASEGURADA_SUSTENTO(p_nIdePol, p_nNumCert, p_nMtoEndoso, p_Aprobado, p_Sustento, p_usuarioaprob, p_cError);
  IF nFlagSustento = 1 THEN -- NECESITA APROBACION
     DBMS_OUTPUT.PUT_LINE('Se Necesita Aprobaci�n y sustento cuando el Endoso es mayor a la Suma Asegurada');
  else
      dbms_output.put_line('No necesita Aprobaci�n');
  END IF;
  IF p_cError <> '0,OK' THEN
     RETURN;
  END IF;

  --<F RTC 316878>

  IF p_cCodCli IS NULL THEN
    BEGIN
      SELECT CodCli
      INTO cCodCli
      FROM CERTIFICADO
     WHERE IdePol  = p_nIdePol
       AND NumCert = p_nNumCert;
    EXCEPTION
      WHEN OTHERS THEN
        cCodCli := 'INVALIDO';
    END;
  END IF;
  --
  SELECT MAX(NomTer), MAX(tipoid), MAX(dvid)
  INTO   cNomter, cTipoId, cDvId
  FROM   TERCERO
  WHERE  NumId = p_nNumId;
  --
  --<I RTC 313344> CBayona / 22-08-2022 / Validar que exista NumId
  IF cNomTer IS NULL AND cTipoId IS NULL THEN
    p_cError := '1,NumID [ ' || p_nNumId|| ' ] no esta registrado';
    RETURN;
  END IF;
  --<F RTC 313344>
  --
  --<I RTC 313344> CBayona / 16-09-2022 / Validar que el CodTexto correspond al numId del Banco
  SELECT NVL(MAX(tg.ValoRef3), '000015') AS "CodTexto" --<RTC 316106> CBayona / 10-01-2023 / Considerar valor por defecto cuando no este registrado en la configuraci�n
  INTO cCodTexto
  FROM Ent_Finan ef,
    Docs_Identidad de,
    Rim_Tab_gral tg
  WHERE ef.NumId = p_nNumId
    AND ef.TipoEntFinan = 'BCO'
    AND de.NumId = ef.NumId
    AND de.TipoIdDoc = 1
    AND tg.CodTab = 'PARAMETRO'
    AND tg.CodModulo = 'INT_SAS_AX'
    AND tg.LLaveTab = 'CD' || de.NumIdDoc;
  --
  IF NVL(cCodTexto, 'X') != NVL(p_cCodTexto, 'Y') THEN
    p_cError := '1,CodTexto [' || NVL(p_cCodTexto, 'NULO') || '] no corresponde al Banco (numId=' || p_nNumId || ') remitido en la trama';
    RETURN;
  END IF;
  --<F RTC 313344>
  --
  IF p_cCodTexto IS NULL THEN
    SELECT NVL(MAX(CodTexto),'000015') -- 000015 RIMAC
    INTO   cCodTexto
    FROM   TEXTO_CESION
    WHERE  STSTEXTO='ACT' AND SOUNDEX(TITULO) = SOUNDEX(cNomter);
  ELSE
    cCodTexto := p_cCodTexto;
    --
    --<I RTC 313344> CBayona / 22-08-2022 / Validar que exista CodTexto
    SELECT COUNT(1)
    INTO nIdeEndCert
    FROM Texto_Cesion
    WHERE CodTexto = cCodTexto
      AND StsTexto = 'ACT';
    --
    IF nIdeEndCert = 0 THEN
      p_cError := '1,CodTexto [ ' || cCodTexto || ' ] no esta registrado';
      RETURN;
    END IF;
    --<F RTC 313344>
  END IF;
  --
  SELECT SQ_ENDOSATARIO_CERT.NEXTVAL
  INTO   nIdeEndCert
  FROM   DUAL;
  --
  nSumaAseg := PR_GEN_REA_UTIL.SumaAseg_Total_X_Cert(p_nIdePol, cCodProd, p_nNumCert);
  --
  /* <RTC 314951> / CBayona / 27-10-2022 / Se comenta validaci�n
  IF p_nMtoEndoso > nSumaAseg THEN
     p_cError := '1,Monto de Endoso supera el de la suma asegurada';
     RETURN;
  END IF; */
  --
  IF NVL(p_nPorcPart,0) = 0 THEN
    IF NVL(nSumaAseg,0) <> 0 THEN
       nPorcPart := (p_nMtoEndoso * 100)/nSumaAseg;
    END IF;
  ELSE
    nPorcPart := p_nPorcPart;
  END IF;
  --
  INSERT INTO ENDOSATARIO_CERT (
         idepol, numcert, tipoid, numid, dvid, tipoend, numprestamo, mtoendoso,
         porcpart, fecing, codtexto, fecexc, codmotvexclu, textomotvexclu, stsend,
         textobenef, numoper, codcli, fecinivig, fecfinvig, ideendcert, textobien
  ) VALUES (
         p_nIdePol, p_nNumCert, cTipoId, p_nNumId, cDvId, p_cTipoEndoso, NULL, p_nMtoEndoso,
         nPorcPart, dFecIng, cCodTexto, NULL, NULL, NULL, 'VAL',
         NULL, NULL, NVL(p_cCodCli, cCodCli), p_dFecIniVig, NVL(p_dFecFinVig, dFecFinVigPol), nIdeEndCert, p_cTxtEndoso
  );
  --
  PR_MOD_ENDOSATARIO_CERT.CREAR_MOD_ENDOSATARIO(nIdeEndCert);
   --<I RTC 316878> Manuel Navarro: / 26-05-2023 CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B, Suma Asegurada VB Tech Core
  IF nFlagSustento = 1 THEN
     BEGIN
         SELECT NVL(MAX(c.NUMMOD),0)
         INTO n_nummod
         FROM MOD_ENDOSATARIO_CERT c
         WHERE c.idepol = p_nIdePol
         AND c.numcert= p_nNumCert
         AND c.ideendcert = nIdeEndCert;
     EXCEPTION WHEN OTHERS THEN
         n_nummod := 0;
     END;
     INSERTA_AUTORIZACIONE (p_nIdePol, p_nNumCert, nIdeEndCert, p_usuarioaprob, p_Sustento, n_nummod, p_cError);
  end if ;
   --<F RTC 316878>
  --
  BEGIN
    PR_ENDOSATARIOS.INCLUIR_ENDOSATARIO_CERT (nIdeEndCert, p_nIdePol, p_nNumCert);
  EXCEPTION
    WHEN OTHERS THEN
      p_cError := SQLCODE || ',' ||SQLERRM;
  END;
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','OK',SYSDATE,SYSDATE,'CAMBIO_ENDOSATARIO_CERT','',nSecuencia );
  --
EXCEPTION
    WHEN OTHERS THEN
        p_cError := '1,'|| SQLERRM;
        PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','ERR',SYSDATE,SYSDATE,'CAMBIO_ENDOSATARIO_CERT','',nSecuencia );
END INSERTA_ENDOSATARIO_CERT;


/*-----------------------------------------------------------------------
   Nombre     : EXCLUYE_ENDOSATARIO_CERT
   Proposito  : Excluye Endosatario a nivel de Certificado/Ramo
   Referencia : Formulario EMI00012.FMB
   Parametro  :
                p_nIdePol                 Id Poliza
                p_nNumCert                Numero de Certificado
                p_TABLA_ENDOSATARIO_CERT  Tabla de Endosatarios
                p_dFecExc                 Fecha de exclusi�n
                p_cCodMotExc              Codigo de Motivo de Exclusi�n
                p_cTxtMotExc              Texto de Motivo de Exclusi�n

   Retorno :    p_cError         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                                   0, OK
                                   1, Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   22/04/2022   Ronald Barreda      <RTC 313344> Creaci�n.
   24/08/2022   Christian Bayona    <RTC 314183> Modificaci�n. Se agrega validaci�n
   01/09/2022   Christian Bayona    <RTC 314183> Modificaci�n. Se modifica mensaje de error
   12/09/2022   Christian Bayona    <RTC 314183> Modificaci�n. Se modifica condici�n y mensaje de error
   06/02/2023   Christian Bayona    <RTC 316106> Modificaci�n. Se adiciona recepcionar el Identificador de Endosatario
-----------------------------------------------------------------------------*/
PROCEDURE EXCLUYE_ENDOSATARIO_CERT (
  p_nIdePol                        ENDOSATARIO_CERT.idePol%TYPE,
  p_nNumCert                       ENDOSATARIO_CERT.numCert%TYPE,
  p_TABLA_ENDOSATARIO_CERT         acselx.TDU_TABLA_ENDOSATARIO_CERT,
  p_dFecExc                        DATE,
  p_cCodMotExc                     ENDOSATARIO_CERT.CODMOTVEXCLU%TYPE,
  p_cTxtMotExc                     ENDOSATARIO_CERT.TEXTOMOTVEXCLU%TYPE
  ,
  p_cError                         IN OUT VARCHAR2
) IS
--
p_nIdeEndCert   ENDOSATARIO_CERT.ideEndCert%TYPE;
p_nSumaAseg     COBERT_CERT.SumaAsegMoneda%TYPE;
cRestricSAS     VARCHAR2(1000);
nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
--
nNumPol         POLIZA.NUMPOL%TYPE;
cStsPol         POLIZA.STSPOL%TYPE;
cCodProd        POLIZA.CODPROD%TYPE;
dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
bExiste         BOOLEAN := TRUE;
--
Dummy           NUMBER;
nNumMod         MOD_ENDOSATARIO_CERT.NumMod%TYPE;
nCantReg        NUMBER;
--
BEGIN
  p_cError := '0,OK';
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,p_nNumcert,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'CAMBIO_ENDOSATARIO_CERT','',nSecuencia );
  --
  -- Validacion del estado la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de p�liza'; RETURN; END IF;
  -- Validacion del estado de la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de estado de p�liza'; RETURN; END IF;
  -- Validar si existe el certificado
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_EXISTE_CERTIFICADO( p_nIdePol , p_nNumCert );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de certificado'; RETURN; END IF;
  --
  DBMS_OUTPUT.PUT_LINE('Fin de validaciones');
  --
  cRestricSAS := pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
  --
  IF NOT cRestricSAS IS NULL THEN
     p_cError := '1,' || cRestricSAS;
     DBMS_OUTPUT.PUT_LINE('cRestricSAS:'||cRestricSAS);
     RETURN;
  END IF;
  --
  PR_INTERFASE_AX_SAS_UTIL.VALIDA_ENDOSATARIO_CERT(
                           p_nIdePol, p_nNumCert, cCodProd, p_TABLA_ENDOSATARIO_CERT, 'S', p_nSumaAseg);
  DBMS_OUTPUT.PUT_LINE('VALIDA_ENDOSATARIO_CERT fin');
  --
  IF (PR.EXISTE_LVAL('PRODEXCE', cCodProd)='S') THEN
    --<I RTC 313344> CBayona / 24-08-2022 / Agregar validaciones
    IF p_dFecExc IS NULL THEN
      p_cError := '1,La Fecha de Exclusi�n no puede ser nula';
      RETURN;
    END IF;
    --<F RTC 313344>
    IF TRUNC(p_dFecExc) NOT BETWEEN dFecIniVigPol AND dFecFinVigPol THEN  --<RTC 313344> CBayona / 12-09-2022 / Se modifica condici�n de validaci�n
       p_cError := '1,La Fecha de Exclusi�n del Endosatario no est� dentro de la vigencia de la p�liza';  --<RTC 313344> CBayona / 12-09-2022 / Se modifica mensaje de validaci�n
       DBMS_OUTPUT.PUT_LINE('cError:'||p_cError);
       RETURN;
    END IF;
    --


    BEGIN
        nCantReg := p_TABLA_ENDOSATARIO_CERT.COUNT;
    EXCEPTION
       WHEN OTHERS THEN
          nCantReg := 0;
    END;




    IF nCantReg > 0 THEN
       FOR i in 1..p_TABLA_ENDOSATARIO_CERT.count loop
          --<I RTC 316106> / CBayona / 06-02-2023 / S� se remitio el Identificador en el TDU se obtiene para realizar la exclusi�n
          DBMS_OUTPUT.PUT_LINE('p_TABLA_ENDOSATARIO_CERT(i).txtAdicional:'||p_Tabla_Endosatario_Cert(i).txtAdicional);
          IF p_Tabla_Endosatario_Cert(i).txtAdicional IS NOT NULL THEN
            p_nIdeEndCert := TO_NUMBER(p_Tabla_Endosatario_Cert(i).txtAdicional);

          ELSE
          --<F RTC 316106>
            DBMS_OUTPUT.PUT_LINE('p_TABLA_ENDOSATARIO_CERT(i).NumId:'||p_TABLA_ENDOSATARIO_CERT(i).NumId);
            --
            SELECT MAX(IdeEndCert)
            INTO   p_nIdeEndCert
            FROM   ENDOSATARIO_CERT
            WHERE  IdePol     = p_TABLA_ENDOSATARIO_CERT(i).Idepol
              AND  NumCert    = p_TABLA_ENDOSATARIO_CERT(i).NumCert
              AND  TipoId     = '0'
              AND  NumId      = p_TABLA_ENDOSATARIO_CERT(i).NumId
              AND  DvId       = '0'
              AND  StsEnd     = 'ACT'
              ;

            DBMS_OUTPUT.PUT_LINE('p_nIdeEndCert:'||p_nIdeEndCert);
            --<I RTC 313344> CBayona / 24-08-2022 / Agregar validaciones
            IF p_nIdeEndCert IS NULL THEN
              p_cError := '1,Endosatario [ ' || p_Tabla_Endosatario_Cert(i).NumId || ' ] no registrado o estado diferente ACT'; --<RTC-313344> / CBayona / 01-09-2022 / Se modifico mensaje de error
              RETURN;
            END IF;

            IF pr.Existe_LVal('MOTVANUL', p_cCodMotExc) = 'N' THEN -- Verificar si el motivo de EXCLUSION existe
               p_cError := '1,' || pr_Interfase_AX_SAS_Util.Mensaje_Error('VAL-0136', p_cCodMotExc);
               RETURN;
            END IF;
          END IF; --<RTC 316106> / CBayona / 06-02-2023 / Fin IF
          --<F RTC 313344>

          UPDATE ENDOSATARIO_CERT
          SET    FECEXC         = p_dFecExc,
                 CODMOTVEXCLU   = p_cCodMotExc,
                 TEXTOMOTVEXCLU = p_cTxtMotExc
          WHERE  IDEENDCERT     = p_nIdeEndCert;
          --
          DBMS_OUTPUT.PUT_LINE('UPDATE ENDOSATARIO_CERT:'||SQL%ROWCOUNT);
          --
          -- FR_EXCLU_ENDOSAT_CERT ini
          PR_ENDOSATARIOS.EXCLUIR_ENDOSATARIO_CERT(p_nIdeEndCert);
          DBMS_OUTPUT.PUT_LINE('PR_ENDOSATARIOS.EXCLUIR_ENDOSATARIO_CERT post');
          --
          BEGIN
            SELECT 1
            INTO   Dummy
            FROM   ENDOSATARIO_CERT
            WHERE  IDEENDCERT = p_nIdeEndCert
              AND  STSEND     = 'EXC';
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              Dummy := 0;
            WHEN TOO_MANY_ROWS THEN
              Dummy := 1;
          END;
          DBMS_OUTPUT.PUT_LINE('Dummy:'||Dummy);
          IF Dummy = 1 THEN
            --Dummy := FR_ALERTA('El proceso de Exclusi�n, eliminara las relaciones Ubicaci�n de bienes.');
            DELETE FROM ENDO_CERT_RIESGO WHERE IDEENDCERT = p_nIdeEndCert;
            DBMS_OUTPUT.PUT_LINE('DELETE:'||SQL%ROWCOUNT);
          END IF;
          --
          -- call ACTUALIZA_FECHA_EXCLU !
          --
          -- FR_EXCLU_ENDOSAT_CERT fin
          --
          --
          -- ACTUALIZA_FECHA_EXCLU ini
          --
          SELECT MAX(NumMod)
          INTO   nNumMod
          FROM   MOD_ENDOSATARIO_CERT
          WHERE  IdeEndCert = p_nIdeEndCert
            AND  StsEnd     = 'INC';
          --
          DBMS_OUTPUT.PUT_LINE('nNumMod:'||nNumMod);
          --
          IF nNumMod IS NOT NULL THEN
             UPDATE MOD_ENDOSATARIO_CERT D
                SET D.FecExc             = p_dFecExc,
                    D.CodMotvExclu       = p_cCodMotExc,
                    D.TextoMotvExclu     = p_cTxtMotExc
             WHERE D.IdeEndCert          = p_nIdeEndCert
               AND D.NumMod              = nNumMod;
             --
             DBMS_OUTPUT.PUT_LINE('UPDATE MOD_ENDOSATARIO_CERT:'||SQL%ROWCOUNT);
          END IF;
          --
          UPDATE ENDOSATARIO_CERT
          SET    NumOper    = NULL
          WHERE  IdeEndCert = p_nIdeEndCert;
          --
          DBMS_OUTPUT.PUT_LINE('UPDATE ENDOSATARIO_CERT:'||SQL%ROWCOUNT);
          --
          -- ACTUALIZA_FECHA_EXCLU fin
          --
       END LOOP;
    END IF;




  ELSE
    NULL; --
  END IF;
  --
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','OK',SYSDATE,SYSDATE,'CAMBIO_ENDOSATARIO_CERT','',nSecuencia );
  --
EXCEPTION
    WHEN OTHERS THEN
        p_cError := '1,'|| SQLERRM;
        PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','ERR',SYSDATE,SYSDATE,'CAMBIO_ENDOSATARIO_CERT','',nSecuencia );
END EXCLUYE_ENDOSATARIO_CERT;
--
-- <FIN RTC 313344 > RBarreda
--
-- <INI RTC 313089> rbarreda

/*-----------------------------------------------------------------------
   Nombre     : TramiteVincular
   Proposito  : Vincular numero de Tramite a poliza por numero de operaci�n
   Referencia : Formulario DISTREA.FMB
   Parametro  :
                p_nIdePol                 Id Poliza
                p_nNumOper                Numero de Operaci�n
                p_cNumTramite             Numero de Tramite

   Retorno :    p_cError         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                                   0,OK
                                   1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   05/05/2022   Ronald Barreda      <RTC 313344> Creaci�n.
-----------------------------------------------------------------------------*/
PROCEDURE TramiteVincular(
  p_nIdePol       POLIZA_TRAMITE.IdePol%TYPE,
  p_nNumOper      POLIZA_TRAMITE.NumOper%TYPE,
  p_cNumTramite   POLIZA_TRAMITE.NumTramite%TYPE
  ,
  p_cError        OUT VARCHAR2
) IS
--
nDummy      NUMBER;
nTotTra     NUMBER;
cOperAsoc    VARCHAR2(500) := '';
cCodUsuario USUARIO.CODUSR%TYPE;
dFechaIni   DATE;
dFechaFin   DATE;
nButton     NUMBER;
cValidaTram VARCHAR2(1);
cDatosLotus VARCHAR2(50);
cEstTra     VARCHAR2(50);
cDoc        NUMBER;
--
nDoc        CHAR(2);
nEstado     CHAR(2);
nMensaje    VARCHAR2(1000);
--
nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
nNumPol         POLIZA.NUMPOL%TYPE;
cStsPol         POLIZA.STSPOL%TYPE;
cCodProd        POLIZA.CODPROD%TYPE;
dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
bExiste         BOOLEAN := TRUE;
nIdePol         POLIZA.IDEPOL%TYPE;
--
CURSOR cTramite IS
  SELECT DISTINCT PT.NUMOPER
    FROM POLIZA_TRAMITE PT,
         OPER_POL OP,
         POLIZA P
   WHERE OP.NUMOPER = PT.NUMOPER
     AND OP.IDEPOL = PT.IDEPOL
     AND P.IDEPOL = OP.IDEPOL
     AND PT.NUMTRAMITE = p_cNumTramite
     AND PT.ESTADOACT IN ('ACT', 'VAL')
     AND OP.INDANUL IS NULL;
--
BEGIN
    p_cError := '0,OK';
    --
    PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,0,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'TRAMITE VINCULAR','',nSecuencia );
    --
    -- Validacion del estado la poliza
    bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
    IF NOT bExiste THEN p_cError := '1,Error en validaci�n de poliza'; RETURN; END IF;
    --
    IF p_cNumTramite IS NULL THEN
      p_cError := '1,Es obligatorio el numero de tramite para vincular ';
      RETURN;
    ELSE
      IF (NVL(p_nIdePol,0) = 0) OR (NVL(p_nNumOper,0) = 0) THEN
        p_cError := '1,Es obligatorio el numero de poliza y operaci�n para vincular ';
        RETURN;
      END IF;
      --
      -- Valida que el tramite exista en el Lotus Notes (NO APLICA EN PACKAGE)
      --
      IF PR.EXISTE_LVAL('COTPROD', cCodProd ) = 'S' THEN
         dbms_output.put_line('PR.EXISTE_LVAL(COTPROD,cCodProd ) = S');
         ACSELX.PR_SERVICIOS_WEB.SP_VALIDA_COTIZACION_SAS( p_cNumTramite, nDoc, nEstado, nMensaje);
         IF nDoc = '-1' THEN
            p_cError := '1,No existe el numero de tramite en SAS para producto ' || cCodProd;
            dbms_output.put_line(p_cError);
            RETURN;
         END IF;
      ELSE
        dbms_output.put_line('Validaci�n en LOTUS NOTES no se realiza en package');
      END IF;
      --
      SELECT COUNT(DISTINCT PT.NUMOPER)
        INTO nTotTra
        FROM POLIZA_TRAMITE PT,
             OPER_POL OP,
             POLIZA P
       WHERE OP.NUMOPER = PT.NUMOPER
         AND OP.IDEPOL = PT.IDEPOL
         AND P.IDEPOL = OP.IDEPOL
         AND PT.NUMTRAMITE = p_cNumTramite
         AND PT.ESTADOACT IN ('ACT', 'VAL')
         AND OP.INDANUL IS NULL;
      --
      IF nTotTra = 0 THEN
        dbms_output.put_line('nTotTra:'||nTotTra);

    BEGIN
      SELECT OP.CODUSUARIO, OP.FECHAHORAINI, OP.FECHAHORAFIN
      INTO cCodUsuario, dFechaIni, dFechaFin
      FROM OPERACION OP
      WHERE IDEOP = p_nNumOper;
    EXCEPTION
    WHEN OTHERS THEN
      p_cError := '1,Numero de operaci�n (IdeOp) NO existe en OPERACION';
      dbms_output.put_line(p_cError);
      RETURN;
    END;
    --
    dbms_output.put_line('cCodUsuario/FecIni/FecFin:'||cCodUsuario||'/'||TO_CHAR(dFechaIni)||'/'||TO_CHAR(dFechaFin));
    --
    BEGIN
      INSERT INTO POLIZA_TRAMITE
        (IDEPOL, NUMTRAMITE, NUMOPER, FECHACRE, FECHAACT, USUARIO, ESTADOACT)
      VALUES
        (p_nIdePol, p_cNumTramite, p_nNumOper, dFechaIni, dFechaFin, cCodUsuario, 'ACT');
      --
      dbms_output.put_line('insert 1:'||SQL%ROWCOUNT);
      --
      SELECT NVL(MAX(SECUENCIA),0)+1 INTO nSecuencia FROM LOG_POLIZA_TRAMITE;
      --
      INSERT INTO LOG_POLIZA_TRAMITE
        (SECUENCIA, IDEPOL, NUMOPER, NUMTRAMITE, PROCESO)
      VALUES
        (nSecuencia, p_nIdePol, p_nNumOper, p_cNumTramite, 'V');
      dbms_output.put_line('insert 2:'||SQL%ROWCOUNT);
      --
    EXCEPTION
      WHEN OTHERS THEN
        p_cError := '1,Error en asociar tramite ' || SQLERRM;
        dbms_output.put_line(p_cError);
    END;

      ELSE
        FOR T IN cTramite LOOP cOperAsoc := cOperAsoc || T.NUMOPER || ', '; END LOOP;
        --
        SELECT SUBSTR(cOperAsoc,1,LENGTH(cOperAsoc)-1) INTO cOperAsoc FROM DUAL;
        p_cError := 'El Tr�mite ingresado ya se encuentra asociado a la(s) operaci�n(es): ' || cOperAsoc;
        dbms_output.put_line(p_cError);
      END IF;
    END IF;
    --
    PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,0,NULL,'LOG','OK',SYSDATE,SYSDATE,'TRAMITE VINCULAR','',nSecuencia );
EXCEPTION
  WHEN OTHERS THEN
    p_cError := '1,'|| SQLERRM;
    PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,0,NULL,'LOG','ERR',SYSDATE,SYSDATE,'TRAMITE VINCULAR','',nSecuencia );
END TramiteVincular;
--
--
--
/*-----------------------------------------------------------------------
   Nombre     : TramiteDesvincular
   Proposito  : Desvincular numero de Tramite a poliza por numero de operaci�n
   Referencia : Formulario DISTREA.FMB
   Parametro  :
                p_tTablaPolizaTramite   Tabla de Poliza Tramite (id poliza, numero de operaci�n, numero de tramite)

   Retorno :    p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                        0,OK
                        1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   05/05/2022   Ronald Barreda      <RTC 313344> Creaci�n.
-----------------------------------------------------------------------------*/
PROCEDURE TramiteDesvincular(
  p_tTablaPolizaTramite   acselx.TDU_TABLA_POLIZA_TRAMITE
  ,
  p_cError              OUT VARCHAR2
) IS
  nDummy     NUMBER;
  nButton    NUMBER;
  nCantSel   PLS_INTEGER := 0;
  cTramites  VARCHAR2(500) := '';
  cValTram   VARCHAR2(50);
  cIndImp    VARCHAR2(1);
  --
  nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
  nNumPol         POLIZA.NUMPOL%TYPE;
  cStsPol         POLIZA.STSPOL%TYPE;
  cCodProd        POLIZA.CODPROD%TYPE;
  dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
  dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
  bExiste         BOOLEAN := TRUE;
  nIdePol         POLIZA.IDEPOL%TYPE;
  --
BEGIN
  p_cError := '0,OK';
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_tTablaPolizaTramite(i).IdePol,0,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'TRAMITE DESVINCULAR','',nSecuencia );
  --
  FOR i in 1..p_tTablaPolizaTramite.count loop
    nCantSel  := nCantSel + 1;
    cTramites := cTramites || p_tTablaPolizaTramite(i).NumTramite || ', ';
    --
    -- Validacion del estado la poliza
    bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_tTablaPolizaTramite(i).IdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
    IF NOT bExiste THEN
      p_cError := '1,Error en validaci�n de poliza';
      RETURN;
    END IF;
    --
    nIdePol := p_tTablaPolizaTramite(i).IdePol;
  END LOOP;
  --
  --
  IF nCantSel = 0 THEN
    p_cError := '1,Seleccione el o los tr�mites a desvincular (array vacio)';
    dbms_output.put_line(p_cError);
    RETURN;
  ELSE
    --
    cTramites := '';
    --
    FOR i in 1..p_tTablaPolizaTramite.count loop
      cIndImp := PR_POLIZA_TRAMITE.EsTramiteImpreso(
                 p_tTablaPolizaTramite(i).NumTramite,
                 p_tTablaPolizaTramite(i).NumOper);
      dbms_output.put_line('cIndImp:'||cIndImp);
      --
      IF NVL(cIndImp, 'N') = 'S' THEN
        cTramites := cTramites || p_tTablaPolizaTramite(i).NumTramite || ', ';
      ELSE
        SELECT (NVL(MAX(SECUENCIA), 0) + 1)
          INTO nSecuencia
          FROM LOG_POLIZA_TRAMITE;
        dbms_output.put_line('nSecuencia:'||nSecuencia);
        --
        INSERT INTO LOG_POLIZA_TRAMITE
          (SECUENCIA, IDEPOL, NUMOPER, NUMTRAMITE,
          PROCESO, CODUSR, FECHA)
        VALUES
          (nSecuencia, p_tTablaPolizaTramite(i).IdePol, p_tTablaPolizaTramite(i).NumOper, p_tTablaPolizaTramite(i).NumTramite,
          'D', USER, SYSDATE);
        dbms_output.put_line('insert:'||SQL%ROWCOUNT);
        --
        DELETE POLIZA_TRAMITE
         WHERE NUMTRAMITE = p_tTablaPolizaTramite(i).NumTramite
           AND NUMOPER    = p_tTablaPolizaTramite(i).NumOper;
        dbms_output.put_line('delete:'||SQL%ROWCOUNT);
      END IF;
    END LOOP;
    --
    IF LENGTH(cTramites) > 0 THEN
      SELECT SUBSTR(cTramites, 1, LENGTH(cTramites) - 2)
      INTO cTramites
      FROM DUAL;
      --
      p_cError := '1,El(los) tr�mite(s) '||cTramites||' no pudo(pudieron) ser desviculado(s) porque ya se encontraba(n) impreso(s)';
      dbms_output.put_line(p_cError);
    END IF;
  END IF;
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',nIdePol,0,NULL,'LOG','OK',SYSDATE,SYSDATE,'TRAMITE DESVINCULAR','',nSecuencia );
EXCEPTION
  WHEN OTHERS THEN
    p_cError := '1,'|| SQLERRM;
    PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',nIdePol,0,NULL,'LOG','ERR',SYSDATE,SYSDATE,'TRAMITE DESVINCULAR','',nSecuencia );
END TramiteDesvincular;


-- <FIN RTC 313089> rbarreda

  -- <ini RTC 313746> mzegarra -----------------------------------------------------------------------
  procedure modificar_clausula(p_idepol    poliza.idepol%type,
                               p_numcert   certificado.numcert%type,
                               p_clausulas tdu_tabla_clau_cert,
                               p_numid     usuario.numid%type,
                               p_mensajes  out sys_refcursor,
                               p_error     out varchar2) is
    ----
    l_seq           log_poliza_sas_ax.secuencia%type;
    l_txt_adicional varchar2(200) := ''; -- unused
    l_stsclau       clau_cert.stsclau%type; --<RTC 313746> Mauro Zegarra - 29/08/2022 - Modificacion de Clausulas
    ----------------------------------------------
    procedure validar is
      l_sas_mobile varchar2(200);

    begin
      limpiar_log_mensajes();

      l_sas_mobile := pr_poliza_util.restringe_endoso_poliza(p_idepol);
      if l_sas_mobile is not null then
        -- La poliza: :mx1 fue emitida en SAS MOBILE por lo que debe ser endosada o modificada en SAS
        pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                      'VAL-0297',
                                                      p_idepol);
        raise_application_error(-20100, 'VALIDAR');
      end if;

      -- validar clausulas
      for idx in 1 .. p_clausulas.count loop

        -- El codigo de ramo es un dato obligatorio
        if p_clausulas(idx).codramocert is null then
          pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                        'VAL-0299');
          raise_application_error(-20100, 'VALIDAR');
        end if;

        -- El codigo de clausula es un dato obligatorio
        if p_clausulas(idx).codclau is null then
          pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                        'VAL-0336');
          raise_application_error(-20100, 'VALIDAR');
        end if;

        -- El estado de la clausula es un dato obligatorio
        if p_clausulas(idx).stsclau is null then
          pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                        'VAL-0337');
          raise_application_error(-20100, 'VALIDAR');
        end if;

        -- La clausula: :mx1 NO aplica a la Marca y Modelo del vehiculo asegurado en el plan
        if pr_cert_ramo.valida_clau_exc_x_mar_mod(p_idepol,
                                                  p_numcert,
                                                  p_clausulas(idx).codramocert,
                                                  p_clausulas(idx).codclau) = 'S' then
          pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                        'VAL-0298',
                                                        p_clausulas(idx).codclau);
          raise_application_error(-20100, 'VALIDAR');
        end if;

        --<I RTC 313746> Mauro Zegarra - 29/08/2022 - Modificacion de Clausulas
        if pr_interfase_ax_sas_util.existe_clau_cert(p_idepol,
                                                     p_numcert,
                                                     p_clausulas(idx).codramocert,
                                                     p_clausulas(idx).codclau,
                                                     l_stsclau) then
          -- si quiero incluir: 'VAL'
          if p_clausulas(idx).stsclau = 'VAL' then
            -- si la clausula esta 'ACT'
            if l_stsclau = 'ACT' then
              -- 'La Clausula (:mx1) del Ramo (:mx2) ya se encuentra registrada'
              pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                          'VAL-1538',
                                                          p_clausulas(idx).codclau,
                                                          p_clausulas(idx).codramocert);
              raise_application_error(-20100, 'VALIDAR');
            end if;
          end if;

          --<I RTC 313746> Mauro Zegarra - 05/09/2022 - Modificacion de Clausulas
          -- si quiero excluir: 'ANU'
          if p_clausulas(idx).stsclau = 'ANU' then
            -- si la clausula esta 'EXC'
            if l_stsclau = 'EXC' then
              -- 'La Clausula (:mx1) del Ramo (:mx2) ya se encuentra excluida'
              pr_interfase_ax_sas_util.insertar_log_errores('MODIFICAR_CLAUSULA.VALIDAR',
                                                          'VAL-1539',
                                                          p_clausulas(idx).codclau,
                                                          p_clausulas(idx).codramocert);
              raise_application_error(-20100, 'VALIDAR');
            end if;
          end if;
          --<F RTC 313746> Mauro Zegarra - 05/09/2022 - Modificacion de Clausulas
        end if;
        --<F RTC 313746> Mauro Zegarra - 29/08/2022 - Modificacion de Clausulas
      end loop;
    end;
    ----------------------------------------------
  begin
    -- noFormat start
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('I', p_idepol, p_numcert, null, 'LOG', null, sysdate, sysdate, 'MODIFICAR_CLAUSULA', '', l_seq);

    validar();

    pr_interfase_ax_sas.incluir_excluir_clausula(p_idepol,
                                                 p_numcert,
                                                 p_clausulas,
                                                 l_txt_adicional,
                                                 p_numid,
                                                 p_mensajes, --<<--
                                                 p_error); --<<--

    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F', p_idepol, p_numcert, null, 'LOG', 'OK', sysdate, sysdate, 'MODIFICAR_CLAUSULA', '', l_seq);
    -- noFormat end
  exception
    when others then
      p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
      p_error    := '1,' || sqlerrm;
      -- noFormat start
      pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F', p_idepol, null, null, 'LOG', 'ERR', sysdate, sysdate, 'MODIFICAR_CLAUSULA', '', l_seq);
      -- noFormat end
  end;
  -- <fin RTC 313746> mzegarra -----------------------------------------------------------------------


/*-----------------------------------------------------------------------------
Nombre      : AJUSTE_PORC_SUMA_LOCAL
Proposito   : Actualizaci�n de los porcentajes por inclusi�n de nuevos locales
Referencias :

Parametros  : p_nIdePol         Identificador de la poliza
              p_nNumCert        Numero del certificado

   Retorno :


LOG de CAMBIO!
Fecha        Autor                        Descripcion
14/06/2022      Jorge Benel               <RTC 313742>  Creaci�n

-------------------------------------------------------------------------------*/

  PROCEDURE AJUSTE_PORC_SUMA_LOCAL( p_nIdePol CERTIFICADO.IDEPOL%TYPE,
                                    p_nNumCert CERTIFICADO.NUMCERT%TYPE) AS

    nMTOVALDECLARDIREC    MOD_DIREC_RIESGO.MTOVALDECLARDIREC%TYPE := 0;


  BEGIN

  SELECT  NVL(SUM(NVL(mtovaldeclardirec,0)),0) -- NVL(SUM(NVL(PorcSuma,0)),0)
           INTO   nMTOVALDECLARDIREC
          FROM   MOD_DIREC_RIESGO M
          WHERE  IdePol    =   p_nIdePol
           AND   NumCert   =   p_nNumCert
           AND   StsDirec IN ( 'ACT','INC')
           AND   M.NUMMOD  = ( SELECT MAX(MD.NUMMOD)
                               FROM   MOD_DIREC_RIESGO MD
                               WHERE  mD.IdeDirec = M.IDEDIREC)
           AND  EXISTS (SELECT 1
                      FROM   Direc_Riesgo_Cert DR
                      WHERE  DR.Idedirec = m.IdediRec
                      AND    DR.Stsdirec != 'ANU');

    FOR x IN ( SELECT  *
          FROM   MOD_DIREC_RIESGO M
          WHERE  IdePol    =   p_nIdePol
           AND   NumCert   =   p_nNumCert
           AND   StsDirec IN ( 'ACT','INC')
           AND   M.NUMMOD  = ( SELECT MAX(MD.NUMMOD)
                               FROM   MOD_DIREC_RIESGO MD
                               WHERE  mD.IdeDirec = M.IDEDIREC)
           AND  EXISTS (SELECT 1
                      FROM   Direc_Riesgo_Cert DR
                      WHERE  DR.Idedirec = m.IdediRec
                      AND    DR.Stsdirec != 'ANU') ) LOOP

         UPDATE MOD_DIREC_RIESGO
         SET PORCSUMA = ROUND((MTOVALDECLARDIREC/ nMTOVALDECLARDIREC) * 100, 2),
           STSDIREC   = 'INC'
         WHERE IDEDIREC = x.IDEDIREC
         AND NUMMOD = x.NUMMOD
         AND IDEPOL = p_nIdePol
         ;

         UPDATE DIREC_RIESGO_CERT
         SET PORCSUMA = ROUND((MTOVALDECLARDIREC/ nMTOVALDECLARDIREC) * 100, 2),
           STSDIREC   = 'INC'
         WHERE IDEDIREC = x.IDEDIREC
         AND IDEPOL = p_nIdePol
         ;

    END LOOP;

  END AJUSTE_PORC_SUMA_LOCAL;



/*-----------------------------------------------------------------------------
Nombre      : MODIFICACION DE LOCALES
Proposito   : Inclusi�n de locales e exclusi�n e inserci�n de bienes del certificado
Referencias :
              program unit
Parametros  : p_nIdePol         Identificador de la poliza
              p_nNumCert        Numero del certificado
              p_cCodRamoCert    Codigo de Ramo/Certificado
              p_nSumAsegNew          Nueva Suma Asegurada
              p_TABLA_DATOS_PARTIC     TABLA de datos particuales
              p_TABLA_DIREC_RIESGO_CERT TABLA de direcciones de riesgo
              p_tTABLA_BIENESXCERT     TABLA de bienes por certificado

   Retorno :    p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                        0,OK
                        1,Error


LOG de CAMBIO!
Fecha        Autor                        Descripcion
06/06/2022      Jorge Benel               <RTC 313742>  Creaci�n
14/06/2022      Jorge Benel               <RTC 313742>  Invocaci�n de funcion
26/09/2022   Jorge Benel                  <RTC 313742>  Actualizaci�n de l�gica
08/11/2022  David Yupanqui        <RTC-315519>  Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
20/01/2023   Christian Bayona             <RTC-316106>  Se actualiza procedimiento de recepci�n de archivos
21/09/2023    Manuel Navarro              <DIGB2B03-2815> Inclusi�n Locales Legacy Dubai Masivo
-------------------------------------------------------------------------------*/

-- <I jbenel 26-09-2022>

/*
  PROCEDURE MODIFICACION_LOCALES( p_nIDEPOL                 POLIZA.IDEPOL%TYPE,
                                  p_nNUMCERT                CERTIFICADO.NUMCERT%TYPE,
                                  p_cCodRamoCert            CERT_RAMO.CodRamoCert%TYPE,
                                  p_nSumAsegNew             NUMBER,
                                  p_TABLA_DATOS_PARTIC      IN TDU_TABLA_DATOS_PARTIC,
                                  p_TABLA_DIREC_RIESGO_CERT IN TDU_TABLA_DIREC_RIESGO_CERT,
                                  p_tTABLA_BIENESXCERT      IN TDU_T_TABLA_BIENESXCERT,
                                  p_cCodUsuario             IN varchar2,
                                  -- Variables de retorno
                                  p_cursorMsgValida         OUT sys_refcursor ,
                                  p_cError                  OUT varchar2

                                  ) AS

    dfecing     DATE;
    dfecfin     DATE;
    nCantRegDir NUMBER(5);
    nCantRegBienes NUMBER(5);
    nIdeBien           BIEN_CERT.IDEBIEN%TYPE := NULL;
    p_TDU_T_TABLA_LISTA_DIRECC  TDU_T_TABLA_LISTA_DIRECCIONES;

    cCodPlan      CERT_RAMO.CodPlan%TYPE;
    cCodRevPlan   CERT_RAMO.RevPlan%TYPE;


    FUNCTION n_MOD_DIREC_RIESGO(vpol POLIZA.IDEPOL%TYPE) RETURN number IS
      n NUMBER(5) := 0;
    BEGIN
      select count(1)
      into n
      from acselx.MOD_DIREC_RIESGO
      WHERE IDEPOL = vpol
      ;

        RETURN n;

    EXCEPTION
      WHEN OTHERS THEN
        RETURN 0;
    END   n_MOD_DIREC_RIESGO;

  BEGIN

    p_cError := '0,OK';

    BEGIN
      SELECT fecing, fecfin
      INTO dfecing, dfecfin
      FROM certificado c
      WHERE idepol   = p_nIDEPOL
      AND numcert  = p_nNUMCERT
      ;
    EXCEPTION
       WHEN OTHERS THEN
           dfecing := SYSDATE;
           dfecfin := SYSDATE;
    END;

    BEGIN

      SELECT CodPlan, RevPlan
      INTO   cCodPlan, cCodRevPlan
      FROM   CERT_RAMO
      WHERE  idepol  = p_nIdePol
      AND  numcert = p_nNumCert
      AND  stsCertRamo in ('ACT')
      AND  codRamoCert = p_cCodRamoCert
      ;
    EXCEPTION
      WHEN OTHERS THEN
         cCodPlan    := '';
         cCodRevPlan := '';
    END;

   -- actualiza datos particulares --

   ACSELX.pr_interfase_ax.FR_INVOCAR_DATOS_PARTICULARES(
      p_nIdePol               => p_nIdepol,
      p_nNumCert              => p_nNumCert,
      p_cCodRamoCert          => p_cCodRamoCert,
      p_cCodPlan              => cCodPlan,
      p_cCodRevPlan           => cCodRevPlan,
      p_dFecIniVig            => dFecIng, -- del certificado
      p_dFecFinVig            => dFecFin, -- del certificado
      p_TABLA_DATOS_PARTIC    => p_tabla_datos_partic,
      p_cError                => p_cError
    );

   dbms_output.put_line('p_cError (datos particulares) -> '||p_cError);

   -- modifica direcci�n de riesg --

   PR_INTERFASE_AX_SAS2.ENDOSO_DIREC_RIESGO2(p_nIdepol,
                      p_nNumCert,
                      p_tabla_direc_riesgo_cert,
                      NULL, -- p_ctxtAdicional
                      p_cCodUsuario,
                      -- Variables de retorno
                      p_cursorMsgValida,
                      p_cError,
                      p_TDU_T_TABLA_LISTA_DIRECC);--salida con las direcciones SAS y AX


    AJUSTE_PORC_SUMA_LOCAL(p_nIdePol, p_nNumCert); -- <RTC 313742 14/06/2022>

    BEGIN
       nCantRegDir := p_TDU_T_TABLA_LISTA_DIRECC.COUNT;
    EXCEPTION
       WHEN OTHERS THEN
          nCantRegDir := 0;
    END;

    dbms_output.put_line('[p_TDU_T_TABLA_LISTA_DIRECC]-nCantRegDir: '||nCantRegDir);

    FOR j in 1..nCantRegDir LOOP
      --Actualiza bien_cert_riesgo
      DBMS_OUTPUT.PUT_LINE( p_TDU_T_TABLA_LISTA_DIRECC(j).IdeDirec);
      DBMS_OUTPUT.PUT_LINE( p_TDU_T_TABLA_LISTA_DIRECC(j).IdeDirecOrig);

    END LOOP;

   -- modifica los Bienes x certificado --

    BEGIN
       nCantRegBienes := p_tTABLA_BIENESXCERT.COUNT;
    EXCEPTION
       WHEN OTHERS THEN
          nCantRegBienes := 0;
    END;
    dbms_output.put_line('[p_tTABLA_BIENESXCERT]-nCantRegBienes: '||nCantRegBienes);

    --Se coloca como excluidos los bienes del certificado

    IF nCantRegBienes > 0 THEN
      UPDATE bien_cert b
      SET b.stsbien  = kEstExc
      WHERE b.idepol = p_nIdePol
      AND b.numcert  = p_nNumcert
      ;
    END IF;

    FOR i in 1..nCantRegBienes LOOP
        nIdeBien := p_tTABLA_BIENESXCERT(i).TxtAdicional;
        dbms_output.put_line('[ENDOSO_DIREC_Y_BIENES]-nIdeBien('||i||'): '||nIdeBien);
        dbms_output.put_line('<3> N� reg MOD_DIREC_RIESGO = '||n_MOD_DIREC_RIESGO(p_nIdePol));

        PR_INTERFASE_AX_SAS2.ENDOSO_BIEN_CERT2(
            nIdeBien, --Idebien AX
            p_nIdepol,
            p_tTABLA_BIENESXCERT(i).NumCert,
            p_tTABLA_BIENESXCERT(i).Fecinivalid,
            p_tTABLA_BIENESXCERT(i).Fecfinvalid,
            p_tTABLA_BIENESXCERT(i).MtoValDeclarbien,
            p_tTABLA_BIENESXCERT(i).Porcriesgo,
            p_tTABLA_BIENESXCERT(i).CodRamoCert,
            p_tTABLA_BIENESXCERT(i).ClaseBien,
            p_tTABLA_BIENESXCERT(i).CodBien,
            p_tTABLA_BIENESXCERT(i).CodMonedabien,
            p_tTABLA_BIENESXCERT(i).Indimprdeclar,
            p_tTABLA_BIENESXCERT(i).TABLA_BIEN_CERT_RIESGO,
            p_tTABLA_BIENESXCERT(i).TABLA_ENDOSATARIO_BIEN,
            p_tTABLA_BIENESXCERT(i).TABLA_COBERT_BIEN,
            p_tTABLA_BIENESXCERT(i).TABLA_DEDUCIBLE_COBERT,
            p_cCodUsuario,
            p_cursorMsgValida,
            p_cError,
            p_TDU_T_TABLA_LISTA_DIRECC
            );

        BEGIN
           nCantRegDir := p_TDU_T_TABLA_LISTA_DIRECC.COUNT;
        EXCEPTION
           WHEN OTHERS THEN
              nCantRegDir := 0;
        END;

        dbms_output.put_line('[p_TDU_T_TABLA_LISTA_DIRECC]-nCantRegDir('||i||'): '||nCantRegDir);

    END LOOP;

  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,'|| SQLERRM;

  END MODIFICACION_LOCALES;

*/


PROCEDURE MODIFICACION_LOCALES ( p_nIDEPOL                IN POLIZA.IDEPOL%TYPE,
                                  p_cCODPROD               IN POLIZA.CODPROD%TYPE,
                                  p_cCodMoneda             IN POLIZA.CODMONEDA%TYPE,
                                  P_TDU_TABLA_IMP_LOCAL    IN ACSELX.TDU_TABLA_IMP_LOCAL ,
                                  p_tTABLA_BIENESXCERT     IN ACSELX.TDU_T_TABLA_BIENESXCERT,
                                  p_tabla_bien_cert_riesgo IN ACSELX.tdu_tabla_bien_cert_riesgo ,
                                  p_cCodUsuario             IN varchar2,
                                  -- Variables de retorno
                                  p_cursorMsgValida         OUT sys_refcursor ,
                                  p_cError               OUT varchar2) AS
  cRuta VARCHAR2(4000);
  nCant NUMBER(2);
  nVal  NUMBER(1);

FUNCTION FR_RECEPCIONAR_ARCHIVO RETURN NUMBER IS

  cRuta              VARCHAR2(1000);
  cFileok            VARCHAR2(1) := 'S';
  nNumLinea          NUMBER;
  nNumBien           NUMBER;
  bVal               BOOLEAN;
  nPos               NUMBER(10);
  nPosIni            NUMBER(10);
  nPosIniAnt         NUMBER(10);
  nPosFin            NUMBER(10);
  nMonto             NUMBER(38, 20);
  nMtoTotal          NUMBER(38, 20);
  cCad               VARCHAR2(4000);
  cClaseTipo         VARCHAR2(7);
  cMensaje           VARCHAR2(4000);
  nIdeDirec          DIREC_RIESGO_CERT.IDEDIREC%TYPE;
  cClaseBien         BIEN_CERT.CLASEBIEN%TYPE;
  cCodBien           BIEN_CERT.CODBIEN%TYPE;
  cIndBien           RAMO_PLAN_PROD.INDBEN%TYPE;
  nNumcert           CERTIFICADO.NumCert%TYPE;
  nNumcertOld        CERTIFICADO.NumCert%TYPE;
  cStsCert           CERTIFICADO.StsCert%TYPE;
  nNumcertSig        CERTIFICADO.NumCert%TYPE;
  nMtoValDeclarDirec DIREC_RIESGO_CERT.MtoValDeclarDirec%TYPE;
  nAlerta            NUMBER;
  --
  rdirec            DIREC_RIESGO_CERT%ROWTYPE;
  dFecIniVig        CERTIFICADO.FecIng%TYPE;
  dFecFinVig        CERTIFICADO.FecFin%TYPE;
  cIndBienValDeclar VARCHAR2(1);
  --<I RTC-316106> / CBayona / 20-01-2023 /Se agregan nuevas variables
  Aux_TDU_Tabla_Imp_Local   acselx.TDU_TABLA_IMP_LOCAL;
  i                 INTEGER;
  c                 INTEGER;
  nCuentaLoc2       INTEGER;
  nExiste           INTEGER;
  nCuentaLoc        INTEGER;
  nMTotal           DIREC_RIESGO_CERT.MTOVALDECLARDIREC%TYPE;
  nPorcLoc          DIREC_RIESGO_CERT.PORCSUMA%TYPE;
  nPorcLocAcu       DIREC_RIESGO_CERT.PORCSUMA%TYPE;
  nDif              DIREC_RIESGO_CERT.PORCSUMA%TYPE;
  --<F RTC-316106> / CBayona / 20-01-2023

  CURSOR c_Cert(p_nNumcert NUMBER) IS
    SELECT NumCert, StsCert
      FROM CERTIFICADO
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumcert
       AND StsCert NOT IN ('ACT', 'ANU', 'EXC', 'NRE', 'NSR', 'REN');

  CURSOR c_Ramos IS
    SELECT CodPlan,
           RevPlan,
           CodRamoCert,
           FecIniValid,
           FecFinValid,
           StsCertRamo
      FROM CERT_RAMO
     WHERE IdePol = p_nIDEPOL
       AND NumCert = nNumcert
     ORDER BY NumOrdVis;

  CURSOR c_Direcciones(p_nNumCert NUMBER) IS
    SELECT IdeDirec, IdePol, NumCert, StsDirec
      FROM DIREC_RIESGO_CERT
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumCert;

  CURSOR c_bien(p_nNumcert BIEN_CERT.NumCert%TYPE) IS
    SELECT IdeBien
      FROM BIEN_CERT
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumcert
       AND StsBien = 'VAL';

BEGIN
  nNumcert       := 0;
  p_cError := '0,OK';

    BEGIN
      nNumLinea := 0;
      nNumBien  := 0;
      nMtoTotal := 0;
      --<I RTC-316106> / CBayona / 20-01-2023 / Se realiza adaptaci�n al procedimiento en base al procedimiento de Valores Declarados
      BEGIN
	  nCuentaLoc := P_TDU_TABLA_IMP_LOCAL.COUNT;--<RTC100678>
      EXCEPTION
        WHEN OTHERS THEN
          nCuentaLoc := 0;
      END;

      IF P_TDU_TABLA_IMP_LOCAL.COUNT > 0 THEN
        i := nCuentaLoc;
        AUX_TDU_TABLA_IMP_LOCAL := P_TDU_TABLA_IMP_LOCAL;

        FOR c1 in (SELECT * FROM DIREC_RIESGO_CERT
                   WHERE idepol = p_nIdePol AND (stsdirec <> 'EXC' OR stsdirec <> 'ANU' ) ) LOOP

          BEGIN
            SELECT 1
            INTO nExiste
            FROM TABLE(P_TDU_TABLA_IMP_LOCAL)
            WHERE IDEDIREC= c1.IdeDirec
            ;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              i := i + 1;
              AUX_TDU_TABLA_IMP_LOCAL.EXTEND(1);

              AUX_TDU_TABLA_IMP_LOCAL(i) := acselx.TDU_IMP_LOCAL(   CODPAIS =>  NULL,     CODESTADO   =>  NULL   ,  CODCIUDAD  =>   NULL ,  CODMUNICIPIO  =>  NULL   ,
                                                                          UBIGEO  =>  NULL ,    TIPOVIA     => NULL    ,  NOMVIA     => NULL   ,  NUMVIA        => NULL    ,
                                                                          TIPOINT => NULL  ,    NUMINT      => NULL    ,  MANZANA    => NULL   ,  LOTE          => NULL    ,
                                                                          KILOME  => NULL ,     TIPOCHA     => NULL    ,  NOMCHA     => NULL   ,  TIPOSDI       => NULL    ,
                                                                          NOMSDI  => NULL ,     REFERENCIA  => NULL    ,  RESTO      => NULL   ,  PORCSUMA      => NULL    ,
                                                                          IDEGIRO => NULL,      MTOVALDECLARDIREC => NULL   ,       TEP            => NULL  ,       TEE            => NULL   ,
                                                                          ANOCONSTRUCC   => NULL   ,    NUMPISOS  => NULL   ,       NUMSOTANOS     => NULL      ,   FECINSPECCION  => NULL   ,
                                                                          NUMCERT         => NULL ,     IDEDIREC  => NULL   ,       INDRIESGOINS   => NULL,         INDINSPECCION  => NULL   ,
                                                                          NOTAINSPECCION  => NULL   ,   INDSININSPECCION => NULL  , USUAUTORIZADOR => NULL   ,      CLASETIPO      => NULL   ,
                                                                          PROPIEEDIFICACION  => NULL,
                                                                          --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
                                                                          USOINMUEBLE => NULL, 
                                                                          edificio => NULL, edificOficina => NULL, edificotro1 => NULL, edificotro2 => NULL, edificotro3 => NULL,
                                                                          contExistencias => NULL, contMaquinFija => NULL, contMaqMovilAcces => NULL, contEquipoElec => NULL, contMobiliarioGral => NULL,
                                                                          contOtro1 => NULL, contOtro2 => NULL, contOtro3 => NULL, contOtro4 => NULL, contOtro5 => NULL,  contOtro6 => NULL,
                                                                          benefBruto => NULL,  gastosEstable => NULL
                                                                          ) ;
                                                                          --<F DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo

              AUX_TDU_TABLA_IMP_LOCAL(i).CODPAIS    := c1.CODPAIS  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).CODESTADO   := c1.CODESTADO  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).CODCIUDAD   := c1.CODCIUDAD ;
              AUX_TDU_TABLA_IMP_LOCAL(i).CODMUNICIPIO  := c1.CODMUNICIPIO   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).UBIGEO := c1.UBIGEO ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOVIA  := c1.TIPOVIA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOMVIA   := c1.NOMVIA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMVIA   := c1.NUMVIA;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOINT  := c1.TIPOINT ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMINT   := c1.NUMINT ;
              AUX_TDU_TABLA_IMP_LOCAL(i).MANZANA  := c1.MANZANA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).LOTE     := c1.LOTE ;
              AUX_TDU_TABLA_IMP_LOCAL(i).KILOME   := c1.KILOME ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOCHA  := c1.TIPOCHA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOMCHA   := c1.NOMCHA   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOSDI  := c1.TIPOSDI  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOMSDI   := c1.NOMSDI;
              AUX_TDU_TABLA_IMP_LOCAL(i).REFERENCIA  := c1.REFERENCIA   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).RESTO       := c1.RESTO ;
              AUX_TDU_TABLA_IMP_LOCAL(i).PORCSUMA    := c1.PORCSUMA       ;
              AUX_TDU_TABLA_IMP_LOCAL(i).IDEGIRO     := c1.IDEGIRO ;
              AUX_TDU_TABLA_IMP_LOCAL(i).MTOVALDECLARDIREC := c1.MTOVALDECLARDIREC    ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TEP   := c1.TEP   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TEE := c1.TEE      ;
              AUX_TDU_TABLA_IMP_LOCAL(i).ANOCONSTRUCC   := c1.ANOCONSTRUCC    ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMPISOS          := c1.NUMPISOS      ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMSOTANOS        := c1.NUMSOTANOS     ;
              AUX_TDU_TABLA_IMP_LOCAL(i).FECINSPECCION  := c1.FECINSPECCION      ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMCERT       := c1.NUMCERT  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).IDEDIREC      := c1.IDEDIREC  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).INDRIESGOINS   := c1.INDRIESGOINS ;
              AUX_TDU_TABLA_IMP_LOCAL(i).INDINSPECCION   := c1.INDINSPECCION ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOTAINSPECCION := c1.NOTAINSPECCION  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).INDSININSPECCION  := c1.INDSININSPECCION ;
              AUX_TDU_TABLA_IMP_LOCAL(i).USUAUTORIZADOR    := c1.USUAUTORIZADOR ;
              AUX_TDU_TABLA_IMP_LOCAL(i).PROPIEEDIFICACION    := c1.PROPIEEDIFICACION ;
              
              --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
              AUX_TDU_Tabla_Imp_Local(i).usoinmueble := c1.USOINMUEBLE;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificio := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificOficina := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro1 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro2 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro3 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contExistencias := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMaquinFija := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMaqMovilAcces := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contEquipoElec := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMobiliarioGral := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro1 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro2 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro3 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro4 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro5 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro6 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).benefBruto := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).gastosEstable := NULL;
              --<F DIGB2B03-2815> 
           END;

        END LOOP;

        BEGIN
          BEGIN
          SELECT SUM(MTOVALDECLARDIREC)
          INTO nMTotal
          FROM TABLE(AUX_TDU_TABLA_IMP_LOCAL)
          ;
          EXCEPTION
            WHEN OTHERS THEN
                nMTotal:= 1.00;
          END;

        END;


        nPorcLocAcu := 0;
        c           := 0;

        BEGIN
           nCuentaLoc2 := AUX_TDU_TABLA_IMP_LOCAL.COUNT;
        EXCEPTION
          WHEN OTHERS THEN
            nCuentaLoc2 := 0;
        END;

        FOR i in AUX_TDU_TABLA_IMP_LOCAL.First .. AUX_TDU_TABLA_IMP_LOCAL.Last LOOP
            ------------------------------------------
            -- PASO 1 - generar direccion de riesgo --
            ------------------------------------------

            c := c + 1;

            SELECT (AUX_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec / nMTOTAL) * 100
            INTO nPorcLoc
            FROM DUAL;

            nPorcLocAcu := nPorcLocAcu + nPorcLoc;

            IF nCuentaLoc2 = c THEN
              nDif := 100 - nPorcLocAcu;
              IF nDif <> 0 THEN
                nPorcLoc := nPorcLoc + nDif;
              END IF;
            END IF;

            BEGIN

nIdeDirec :=   pr_interfase_AX.GRABA_LINEA_DIR_MASI2( p_nIDEPOL,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codpais ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codestado,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codciudad,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codmunicipio ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).ubigeo ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tipovia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).nomvia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numvia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tipoint ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numint ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).manzana ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).lote ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).kilome,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tipocha,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).nomcha ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tiposdi ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).nomsdi ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).referencia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).resto , --  <RTC 314185> Jorge Benel 23-11-2022
                                                                        nPorcLoc, -- <RTC 314175> Jorge Benel 12-11-2022
                                                                        -- AUX_TDU_TABLA_IMP_LOCAL(i).porcsuma, --  <RTC 314185> Jorge Benel 23-11-2022
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).idegiro  ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec ,

                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tep ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tee ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).Anoconstrucc ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numpisos     ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numsotanos   ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).fecinspeccion ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).NumCert      ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).IdeDirec     ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).IndRiesgoIns ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).IndInspeccion ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).NOTAINSPECCION ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).INDSININSPECCION,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).USUAUTORIZADOR   ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).CLASETIPO   ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).PROPIEEDIFICACION,

                                                                        -- out
                                                                        nNumcert,
                                                                        p_cError  --<RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
                                                                       );

            --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
            IF p_cError != '0,OK' THEN
              RETURN 0;
            END IF;
            --<F RTC-316106> / CBayona / 23-01-2023

            EXCEPTION
              WHEN OTHERS THEN

                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Direcciones : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;
                RETURN 0;
            END;

            ---------------------------
            -- PASO 2 generar bienes --
            ---------------------------
            dbms_output.put_line('nIdeDirec = '||nIdeDirec || ' - nDirAUX: ' || AUX_TDU_TABLA_IMP_LOCAL(i).IdeDirec);

            IF AUX_TDU_TABLA_IMP_LOCAL(i).IdeDirec IS NULL THEN --CB

            BEGIN
              IF nIdeDirec > 0 THEN
                --validar estado del certificado
                BEGIN
                  SELECT StsCert, FecIng, FecFin
                    INTO cStsCert, dFecIniVig, dFecFinVig
                    FROM CERTIFICADO
                   WHERE IdePol = p_nIDEPOL
                     AND NumCert = nNumcert;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    cStsCert := '';
                END;

                 dbms_output.put_line('cStsCert = '||cStsCert);

                IF cStsCert IN ('VAL', 'INC') THEN
                  -- PASO 2.1  eliminar los bienes por cada direccion
                  BEGIN
                    PR_BIEN_CERT_UTIL.ELIMINAR_BIEN_RIESGO(nIdeDirec);
                  EXCEPTION
                    WHEN OTHERS THEN

                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Eliminar las Direcciones Riesgo : ' ||cMensaje ;
                      p_cError := '1, '||cMensaje;

                      RETURN 0;

                  END;
                END IF;

                FOR X IN c_Ramos LOOP
                  DBMS_OUTPUT.Put_Line('>>>> c_Ramos: ' || c_Ramos%ROWCOUNT || ' Ramo: ' || X.CodRamoCert ||
                    ' Plan: ' || X.CodPlan || ' Rev: ' || X.RevPlan);
                  BEGIN
                    SELECT IndBien
                      INTO cIndBien
                      FROM RAMO_PLAN_PROD
                     WHERE CodProd = p_cCODPROD
                       AND CodPlan = X.CodPlan
                       AND RevPlan = X.RevPlan
                       AND CodRamoPlan = X.CodRamoCert
                     ORDER BY NumOrdImp;
                  EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                      cIndBien := 'N';
                  END;

                  IF cIndBien = 'S' THEN
                       nMtoTotal  := 0;
                       DBMS_OUTPUT.Put_Line('>>> p_tTABLA_BIENESXCERT.COUNT: ' || p_tTABLA_BIENESXCERT.COUNT);
                      IF p_tTABLA_BIENESXCERT.COUNT > 0 THEN
                        FOR i in p_tTABLA_BIENESXCERT.First .. p_tTABLA_BIENESXCERT.Last LOOP

                          cClaseBien := p_tTABLA_BIENESXCERT(i).ClaseBien;
                          cCodBien   := p_tTABLA_BIENESXCERT(i).CodBien;
                          nMonto     := NVL( p_tTABLA_BIENESXCERT(i).MtoValDeclarbien ,0); -- / 100 JBENEL
                          DBMS_OUTPUT.Put_Line('>>> ' || nMonto);
                          nMtoTotal  := nMtoTotal + nMonto;

                          BEGIN

                            -- PASO 2.3 GENERAR BIENES

                            -- pr_Interfase_AX.
                            GENERA_BIEN_MASI3(p_nIDEPOL,
                                              nNumcert,
                                              p_cCODPROD,
                                              X.CodRamoCert,
                                              X.CodPlan,
                                              X.RevPlan,
                                              nIdeDirec,
                                              cClaseBien,
                                              cCodBien,
                                              nMonto,
                                              p_cCodMoneda,
                                              p_tTABLA_BIENESXCERT(i).Fecinivalid,
                                              p_tTABLA_BIENESXCERT(i).Fecfinvalid);

                          EXCEPTION
                            WHEN OTHERS THEN

                              cMensaje := cMensaje || ' ' || SQLERRM;
                              cMensaje := ' al Procesar la Carga de Bienes : ' ||cMensaje ;
                              p_cError := '1, '||cMensaje;
                              RETURN 0;

                          END;

                        nNumBien := nNumBien + 1;
                      END LOOP; --
                    END IF;  -- p_tTABLA_BIENESXCERT.COUNT
                  END IF; -- cIndBien = 'S'

                END LOOP; -- X -- c_Ramos

                nMtoTotal  := 0;
                IF p_tabla_bien_cert_riesgo.COUNT > 0 THEN
                  FOR i in p_tabla_bien_cert_riesgo.First .. p_tabla_bien_cert_riesgo.Last LOOP

                     nMtoTotal     := nMtoTotal + NVL( p_tabla_bien_cert_riesgo(i).mtovaldeclarbien,0);

                      UPDATE   BIEN_CERT_RIESGO
                      SET MTOVALDECLARBIENDIREC = p_tabla_bien_cert_riesgo(i).mtovaldeclarbien ,
                         PORCRIESGO = 0
                      WHERE IDEDIREC = p_tabla_bien_cert_riesgo(i).IDEDIREC
                      AND IDEBIEN = TO_NUMBER(p_tabla_bien_cert_riesgo(i).TXTADICIONAL)
                      ;

                  END LOOP;

                  BEGIN
                    FOR X IN(  SELECT IDEBIEN, COUNT(1) cuenta , SUM(MTOVALDECLARBIENDIREC)  montototal
                              FROM BIEN_CERT_RIESGO
                              WHERE IDEBIEN IN (SELECT IDEBIEN
                                                FROM BIEN_CERT
                                                WHERE IDEPOL = p_nIDEPOL)
                              GROUP BY IDEBIEN
                               ) LOOP


                        UPDATE   BIEN_CERT_RIESGO
                        SET PORCRIESGO = DECODE(x.montototal,0,0,  (MTOVALDECLARBIENDIREC / x.montototal ) ) * 100
                        WHERE IDEBIEN = X.IDEBIEN
                        ;
                    END LOOP;
                  END;

                END IF;

                nNumLinea := nNumLinea + 1;

            END IF; -- IF nIdeDirec > 0 THEN

           EXCEPTION
              WHEN OTHERS THEN

                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Bienes : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;

                RETURN 0;
            END;

         END IF; --<F RTC-316106> / CBayona / 20-01-2023

         END LOOP;
       END IF;

    END ;

    RETURN 0;

EXCEPTION
  WHEN OTHERS THEN
    cMensaje := cMensaje || ' ' || SQLERRM;
     p_cError := '1, '||cMensaje;
    RETURN 0;
END FR_RECEPCIONAR_ARCHIVO;

BEGIN

  nVal :=FR_RECEPCIONAR_ARCHIVO;

  dbms_output.put_line('nVal = '|| nVal ||' - p_cError = '||p_cError);

END MODIFICACION_LOCALES;

-- <F jbenel 26-09-2022>


/*-----------------------------------------------------------------------------
  Nombre      :  CAMBIO_DATOS_GRALES_CERT2
  Proposito   :  Actualiza los datos relacionados al cambio de Asegurado y/o Cambio de Responsable de pago.
  Referencias :  PR_INTERFASE_AX_SAS.CAMBIO_DATOS_GRALES_CERT

  Parametros  :  p_nIdePol              Identificador de  la p�liza
                 p_nNumcert             Numero del certificado
                 p_cTipoModif           Tipo de modificacion : 'I'-Inclusion  'E'-Exclusion
                 p_TABLA_POLIZA_CLIENTE Arreglo con los contratantes de la p�liza a incluir o excluir
                 p_dFecExc              Fecha de exclusion del contratante
                 p_cCodMotvExc          Codigo del motivo de exclusion
                 p_TABLA_RESP_PAGO      Arreglo de los responsables de pago
                 p_cCodUsuario          NumId del usuario de Auditoria

  Retorno        p_cursorMsgValida      Cursor con los mensajes de validacion
                 p_cError                     Codigo de Error en caso se encuentre un error
                                              0 : OK  , 1 : Error

LOG de CAMBIO!
   Fecha           Autor                      Descripcion
   06/06/2022      Jorge Benel                <RTC 313742>  Creaci�n
   28/06/2022      Jorge Benel                <RTC 313343>  Actualizaci�n de logica de cambio de asegurado
  ----------------------------------------------------------------------------*/

  PROCEDURE CAMBIO_DATOS_GRALES_CERT2 (p_nIdePol     IN POLIZA.IDEPOL%type,
                                      p_nNumcert     IN certificado.numcert%type,
                                      p_ctipomodif   IN VARCHAR2,
                                      p_tabla_poliza_cliente IN TDU_TABLA_POLIZA_CLIENTE,
                                      p_dfecexc         IN poliza_cliente.fecexc%type,
                                      p_ccodmotvexc     IN poliza_cliente.CodMotvExc%type,
                                      p_TABLA_RESP_PAGO IN TDU_TABLA_RESP_PAGO,
                                      --
                                      p_cCodUsuario     IN  Varchar2,
                                      p_cursorMsgValida OUT sys_refcursor ,
                                      p_cError          OUT VARCHAR2) IS

     nExisteValidacion       NUMBER(1):= 0;
     cStsPol                 POLIZA.STSPOL%TYPE;
     cCodCli                 CERTIFICADO.CODCLI%TYPE;
     cCodCliActual           CERTIFICADO.CODCLI%TYPE;
     nSecuencia              LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
     cStsPolCli              POLIZA.STSPOL%TYPE;
     nNumMod                 MOD_POL_CERT_CLI.NUMMOD%TYPE;
     nEstaExcluido           NUMBER(5);
     nEstaActivo             NUMBER(1):=0;    -- <RTC-313343> -- Jorge Benel / 28-06-2022 / Declaraci�n de variable

    -- Validaciones
    PROCEDURE VALIDAR  IS
       nNumPol         POLIZA.NUMPOL%TYPE;
       cCodProd        POLIZA.CODPROD%TYPE;
       dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
       dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
       bExiste         BOOLEAN;
    BEGIN
       -- Eliminar los datos del Log de mensajes de validaci�n
       LIMPIAR_LOG_MENSAJES;
       -- Obtenemos informacion de la poliza
       bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
       -- Validacion del estado de la poliza
       bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
       -- Validar si existe el certificado
       bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_EXISTE_CERTIFICADO (p_nIdePol , p_nNumCert ) ;
       -- Verificacion del responsable de pago
       PR_INTERFASE_AX_SAS_UTIL.VALIDA_RESP_PAGO ( p_TABLA_RESP_PAGO ,'N');  -- Se envia 'N' para que sea obligatorio recibir los datos del arreglo
       -- Validar el usuario
       PR_INTERFASE_AX_SAS_UTIL.VALIDA_NUMID_USUARIO( p_cCodUsuario );

       -- Si existen errores retornar un RAISE
       IF PR_INTERFASE_AX_SAS_UTIL.EXISTE_LOG_MENSAJES = 'S' THEN
          nExisteValidacion := 1;
          RAISE_APPLICATION_ERROR(-20100,'Existen validaciones encontradas.');
       END IF;
    END VALIDAR;

  BEGIN
     PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,p_nNumcert,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'CAMBIO_DATOS_GRALES_CERT','',nSecuencia ); -- <N3041576 > 02.10.2013
     -- Validaciones Previas
     VALIDAR;
     -- Cargar el usuario de la APP
     p_cError := '0,OK'; -- <RTC-313343> -- Jorge Benel / 28-06-2022 / Inicializaci�n de variable

     FOR i in 1..p_TABLA_POLIZA_CLIENTE.Count  LOOP
         cCodCli := PR_ADA_EMI_QRY.EQUIV_NUMID_CLIENTE(p_TABLA_POLIZA_CLIENTE(i).NumIdCliente);
         -- Obtener el estado
         BEGIN
           -- <I RTC 313343> -- Jorge Benel / 28-06-2022 / Se reemplaza tabla de consulta
           /*
           SELECT STSPOLCERTCLI
           INTO cStsPolCli
           FROM MOD_POL_CERT_CLI
           WHERE IDEPOL = p_nIdepol
           AND NUMCERT = p_nNumCert
           AND  CODCLI = cCodCli;
           */
           SELECT STSPOLCERTCLI
           INTO cStsPolCli
           FROM POLIZA_CERT_CLIENTE
           WHERE IDEPOL = p_nIdepol
           AND NUMCERT = p_nNumCert
           AND  CODCLI = cCodCli;
            -- <F RTC 313343>
         EXCEPTION
             WHEN OTHERS THEN
                cStsPolCli := 'INC'; -- considerarlo como INC para los casos que no exista
         END;

         dbms_output.put_line( 'cCodCli : ' || cCodCli || ' ,  STSPOLCERTCLI => ' || cStsPolCli );

          -- Verificar datos del titula Asegurado -  el NumId del asegurado exista ( campo CODCLI )

          IF NOT PR_INTERFASE_AX_SAS_UTIL.VALIDA_ASEGURADO( p_TABLA_POLIZA_CLIENTE(i).NumIdCliente ) THEN
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('CAMBIO_DATOS_GRALES_CERT2', 'VAL-0051'); --'El Identificador del asegurado no existe en Acsel/X.'
          END IF;

         -- Si es una inclusion
         IF p_cTipoModif = 'I'  THEN
            --<I RTC 313343> --  Jorge Benel / 28-06-2022 / Validar si el cliente ya se encuentra en estado ACT
            IF cStsPolCli = 'ACT' THEN
              PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('CAMBIO_DATOS_GRALES_CERT2', 'VALIDACION','EL ASEGURADO '||cCodCli ||' YA SE ENCUENTRA REGISTRADO EN ESTADO ACTIVO');
              p_cError := '1,Error';
              nEstaActivo := 1;
            END IF;

            IF nEstaActivo = 0 THEN
            --<F RTC 313343>

              IF cStsPolCli <> kEstExc  THEN
                 -- Actualizar el Contratante del certificado (tabla: POLIZA_CERT_CLIENTE)
                 PR_INTERFASE_AX_SAS_UTIL.GRABAR_POLIZA_CERT_CLIENTE( p_nIdepol        ,
                                                                p_nNumcert       ,
                                                                p_TABLA_POLIZA_CLIENTE(i).NumIdCliente   ,
                                                                null ,    --p_cIndRespMov
                                                                kValido , --p_cstspolcertcli
                                                                NULL , --p_dfecexc
                                                                NULL , --p_cCodMotexc
                                                                p_cError);
                 -- Incluir el asegurado actual
                 BEGIN
                    PR_POLIZA_CERT_CLIENTE.INCLUIR( p_nIdePol, p_nNumCert, cCodCli); -- cCodCli = p_nNumIdContratante / inserta en la tabla MOD_POL_CERT_CLI
                 EXCEPTION
                    WHEN OTHERS THEN
                       RAISE_APPLICATION_ERROR(-20100,PR_INTERFASE_AX_SAS_UTIL.MENSAJE_ERROR( 'VAL-0181') || SQLERRM); -- 'Error al incluir al cliente.'
                 END;

               ELSE
                  -- REHABILITA UN ASEGURADO EXCLUIDO
                  DBMS_OUTPUT.PUT_LINE('REHABILITA UN ASEGURADO EXCLUIDO -- PENDIENTE');

                  SELECT NVL(MAX(NumMod), 0) + 1
                  INTO nNumMod
                  FROM MOD_POL_CERT_CLI
                  WHERE IdePol = p_nIdepol
                  AND NumCert = p_nNumCert
                  AND CodCli = cCodCli;

                  -- Insertando la modificaci�n


                  INSERT INTO MOD_POL_CERT_CLI
                    (IdePol, NumCert, CodCli,
                     IndRespMov,
                     StsPolCertCli, NumMod)
                  VALUES
                    (p_nIdepol, p_nNumCert,cCodCli,
                     p_TABLA_POLIZA_CLIENTE(i).indrespmov,
                     'INC', nNumMod);
                  -- Rehabilitando al contratante
                  UPDATE POLIZA_CERT_CLIENTE
                     SET StsPolCertCli = 'MOD',
                         FecExc = NULL,
                         CodMotvExc = NULL
                   WHERE IdePol = p_nIdePol
                     AND NumCert = p_nNumCert
                     AND CodCli = cCodCli;


              END IF;
          END IF ; --<RTC-313343> -- Jorge Benel / 28-06-2022 / Fin de condicionante

        ELSE -- p_cTipoModif = 'E'

            --<I 24.04.2013> -- Determinar si se encuentra excluido
            BEGIN
              --<I RTC- 313343> -- Jorge Benel / 28-06-2022 / Se cambia tabla del select
              /*
              SELECT 1
              INTO nEstaExcluido
              FROM POLIZA_CLIENTE
              WHERE IDEPOL = p_nIdepol
              AND  CODCLI = cCodCli
              AND  STSPOLCLI = 'EXC' ;
              */

              SELECT 1
              INTO nEstaExcluido
              FROM POLIZA_CERT_CLIENTE
              WHERE IDEPOL = p_nIdepol
              AND  CODCLI = cCodCli
              AND  StsPolCertCli = 'EXC' ;

             --<F RTC 313343>
            EXCEPTION
               WHEN OTHERS THEN
                  nEstaExcluido := 0;
            END;
            IF nEstaExcluido = 0 THEN   --<F 24.04.2013>

              --Actualizar la fecha de exclusion y motivo de exclusion
              UPDATE POLIZA_CERT_CLIENTE
              SET CODMOTVEXC =  kMotivoExclusion ,
                 FECEXC     = SYSDATE
              WHERE IDEPOL  = p_nIdePol
              AND  NUMCERT = p_nNumCert
              AND  CODCLI  = cCodCli;
              -- Excluir
              BEGIN
                PR_POLIZA_CERT_CLIENTE.EXCLUIR( p_nIdePol, p_nNumCert, cCodCli);
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR(-20100,PR_INTERFASE_AX_SAS_UTIL.MENSAJE_ERROR( 'VAL-0182')|| SQLERRM); -- 'Error al excluir al cliente.'
              END;
            END IF;

         END IF; -- p_cTipoModif = 'E'

       END LOOP;

     -- END IF;

     -- Actualizar el responsable de pago del certificado
     --**************************************************
     -- Eliminar los responsables de pago existentes , con el objetivo que
     -- siempre sume 100% la informacion a grabar.

     IF  p_cError = '0,OK' THEN  -- <RTC 313343> -- Jorge Benel / 28-06-2022 / Lee la variable del error
       DELETE RESP_PAGO
       WHERE IDEPOL  = p_nIdepol
        AND  NUMCERT = p_nNumcert;
       -- Actualizar el responsable del certificado
       PR_INTERFASE_AX_SAS_UTIL.GRABAR_TABLA_RESP_PAGO ( p_nIdePol , p_nNumcert , p_TABLA_RESP_PAGO , p_cError);

       -- <15.04.2013> Reflejar la modificacion en la poliza.
       PR_CERTIFICADO.REFLEJAR_MODIFICACION(p_nIdepol,p_nNumcert);
     END IF;  -- <RTC 313343> -- Jorge Benel / 28-06-2022 / Fin de quiebre

     -- p_cError := '0,OK'; -- <RTC 313343> Jorge Benel / 28-06-2022 / Se comenta

     p_cursorMsgValida := acselx.PR_INTERFASE_AX_SAS.MOSTRAR_CURSOR_MENSAJES;  -- devuelve un cursor abierto y vacio

     PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'CAMBIO_DATOS_GRALES_CERT2','',nSecuencia ); -- <N3041576 > 02.10.2013

  EXCEPTION
     WHEN OTHERS THEN
         p_cError := '1,'|| SQLERRM;
         -- ROLLBACK; -- <RTC 313343> Jorge Benel / 28-06-2022 / Se comenta
         IF nExisteValidacion > 0 THEN   -- Insertar el ultimo mensaje de error en el LOG.
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('CAMBIO_DATOS_GRALES_CERT2', kCodMsgValidacion ,SQLERRM );
         ELSE
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('CAMBIO_DATOS_GRALES_CERT2', kCodMsgError , SQLERRM );
         END IF;
         -- Devolver los mensajes de validacion
         p_cursorMsgValida := acselx.PR_INTERFASE_AX_SAS.MOSTRAR_CURSOR_MENSAJES;
         PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','ERR',SYSDATE,SYSDATE,'CAMBIO_DATOS_GRALES_CERT2','',nSecuencia ); -- <N3041576 > 02.10.2013
  END CAMBIO_DATOS_GRALES_CERT2;

-- <ini RTC 313345> mzegarra -----------------------------------------------------------------------
  procedure incluir_anexo_certificado(p_idepol   poliza.idepol%type,
                                      p_numcert  certificado.numcert%type,
                                      p_anexos   tdu_tabla_anexo_cert,
                                      p_numid    usuario.numid%type,
                                      p_mensajes out sys_refcursor,
                                      p_error    out varchar2) is
    l_seq      log_poliza_sas_ax.secuencia%type;
    l_stsanexo anexo_cert.stsanexo%type;
    ------------------------------------------------------------------------------------------------
    procedure validar is
      l_sas_mobile varchar2(200);

    begin
      pr_interfase_ax.limpiar_log_mensajes();

      l_sas_mobile := pr_poliza_util.restringe_endoso_poliza(p_idepol);
      if l_sas_mobile is not null then
        -- La poliza: :mx1 fue emitida en SAS MOBILE por lo que debe ser endosada o modificada en SAS
        pr_interfase_ax_sas_util.insertar_log_errores('INCLUIR_ANEXO_CERTIFICADO.VALIDAR',
                                                      'VAL-0297',
                                                      p_idepol);
        raise_application_error(-20100, 'VALIDAR');
      end if;

      -- validar anexos
      for idx in 1 .. p_anexos.count loop

        begin
          select t.stsanexo
            into l_stsanexo
            from anexo_cert t
           where t.idepol = p_idepol
             and t.codanexo = p_anexos(idx).codanexo;

        exception
          when others then
            l_stsanexo := null;
        end;

        if l_stsanexo in ('ANU', 'EXC') then
          -- No puede activar la modificaci�n de texto en el estado: :mx1 actual del Anexo :mx2
          pr_interfase_ax_sas_util.insertar_log_errores('INCLUIR_ANEXO_CERTIFICADO.VALIDAR',
                                                        'VAL-1505',
                                                        l_stsanexo,
                                                        p_anexos(idx).codanexo);
          raise_application_error(-20100, 'VALIDAR');
        end if;

      end loop;
    end;
    ------------------------------------------------------------------------------------------------
  begin
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('I',
                                                      p_idepol,
                                                      p_numcert,
                                                      null,
                                                      'LOG',
                                                      null,
                                                      sysdate,
                                                      sysdate,
                                                      'INCLUIR_ANEXO_CERTIFICADO',
                                                      '',
                                                      l_seq);
    validar();

    pr_interfase_ax_sas.grabar_anexo_certificado(p_idepol,
                                                 p_numcert,
                                                 p_anexos,
                                                 p_numid,
                                                 p_mensajes,
                                                 p_error);

    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F',
                                                      p_idepol,
                                                      p_numcert,
                                                      null,
                                                      'LOG',
                                                      'OK',
                                                      sysdate,
                                                      sysdate,
                                                      'INCLUIR_ANEXO_CERTIFICADO',
                                                      '',
                                                      l_seq);

  exception
    when others then
      p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
      p_error    := '1,' || sqlerrm;
      pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F',
                                                        p_idepol,
                                                        null,
                                                        null,
                                                        'LOG',
                                                        'ERR',
                                                        sysdate,
                                                        sysdate,
                                                        'INCLUIR_ANEXO_CERTIFICADO',
                                                        '',
                                                        l_seq);
  end;
-- <fin RTC 313345> mzegarra -----------------------------------------------------------------------

-- <ini RTC 313350> rbarreda -----------------------------------------------------------------------

/*-----------------------------------------------------------------------
   Nombre     : ModificacionCorredor
   Proposito  : Asociar Corredor a poliza reemplazandolo
   Referencia : Formulario EMI00134.fmb
        :FORM.KEY-COMMIT        ModCorredorGrabar
        :B01_2.PRE-INSERT         ModCorredorPreInsert
        :FORM.WHEN-NEW-FORM-INSTANCE  ModCorredorInicio
   Parametro  :
        p_nIdePol     Id de la poliza
        p_nNumId      Id del corredor
        p_cCodAgencia Codigo de agencia del corredor
        p_cCodOfiSusc Codigo de oficina/sucursal del corredor
        p_cIndLider   Indicador de lider S/N, predeterminadamente = S
        p_nPorcPart   Porcentaje de participaci�n del corredor, , predeterminadamente = 100%

   Retorno :    p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                        0,OK
                        1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   14/06/2022   Ronald Barreda      <RTC 313344> Creaci�n.
   16/06/2022   Ronald Barreda      <RTC 313344> Se agrego parametros canal y tipo de canal a procedimiento ModificacionCorredor
   29/09/2022   Ronald Barreda      <RTC 313350> Se agrego validaciones
-----------------------------------------------------------------------------*/

PROCEDURE ModificacionCorredor(
  p_nIdePol     POLIZA.IdePol%TYPE,
  p_nNumId      INTERMEDIACION_POL.NUMID%TYPE,
  p_cCodAgencia INTERMEDIACION_POL.CODAGENCIA%TYPE,
  p_cCodOfiSusc INTERMEDIACION_POL.CODOFISUSC%TYPE,
  p_cIndLider   INTERMEDIACION_POL.INDLIDER%TYPE DEFAULT 'S',
  p_nPorcPart   INTERMEDIACION_POL.PORCPART%TYPE DEFAULT 100,
  p_cCodCanal INTERMEDIACION_POL.CodCanal%TYPE DEFAULT NULL,
  p_cTipoCanal  INTERMEDIACION_POL.TipoCanal_MC%TYPE DEFAULT NULL,
  p_cError      OUT VARCHAR2
) IS
--
cRestricSAS   VARCHAR2(1000);
nNumPol       POLIZA.NUMPOL%TYPE;
cStsPol       POLIZA.STSPOL%TYPE;
cCodProd      POLIZA.CODPROD%TYPE;
dFecIniVigPol POLIZA.FECINIVIG%TYPE;
dFecFinVigPol POLIZA.FECFINVIG%TYPE;
bExiste       BOOLEAN := TRUE;
nSecuencia    LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
--
rInterPol     INTERMEDIACION_POL%ROWTYPE;
cCodCanal     INTERMEDIACION_POL.CodCanal%TYPE;
cTPCanal      INTERMEDIACION_POL.TIPOCANAL_MC%TYPE;
--
bParametro    BOOLEAN := FALSE;
--
--<I RTC 313350> / CBayona / 29-09-2022 / Nuevas variables
nExiste       PLS_INTEGER;
nNumIdDest    Intermediacion_Pol.NumId%TYPE;
--<F RTC 313350>
--
PROCEDURE ModCorredorGrabar IS
--
cCODAGEINTER_MC INTERMEDIACION_POL.CODAGEINTER_MC%type;
nNUMIDINTER_MC  INTERMEDIACION_POL.NUMIDINTER_MC%type;
--
BEGIN
  IF cCodCanal = '2' OR cCodCanal = '7' THEN
    BEGIN
      SELECT CODAGEINTER_MC, NUMIDINTER_MC
      INTO cCODAGEINTER_MC, nNUMIDINTER_MC
      FROM INTERMEDIACION_POL
      WHERE IDEPOL = p_nIdePol;
    EXCEPTION
      WHEN OTHERS THEN
      NULL;
    END;
  END IF;
  --
  PR_Intermediacion.Copia_Part_Inter_Pol(p_nIdePol); -- PART_INTER_POL
  --
  IF cCodCanal = '3' OR cCodCanal = '8' THEN
    UPDATE INTERMEDIACION_POL
    SET    CODAGEINTER_MC = CODAGENCIA,
           NUMIDINTER_MC  = NUMID
    WHERE  IDEPOL         = p_nIdePol;
  END IF;
  --
  IF cCodCanal = '2' OR cCodCanal = '7' THEN
    UPDATE INTERMEDIACION_POL
    SET    CODAGEINTER_MC = cCODAGEINTER_MC,
           NUMIDINTER_MC  = nNUMIDINTER_MC
    WHERE IDEPOL = p_nIdePol;
  END IF;
  --
  PR_INTERMEDIACION.INSERTA_POLIZA_MODCOM(p_nIdePol, cCodCANAL, cTPCanal, p_nNumId); -- POLIZA_MODCOM POLIZA_MODCOM_LOG
  --
  PR_Intermediacion.Copia_Part_Inter_Pol(p_nIdePol);
  --
END ModCorredorGrabar;


PROCEDURE ModCorredorPreInsert IS
  dFecha  DATE := SYSDATE;
BEGIN
  dbms_output.put_line('cCodCanal2:'||cCodCanal);
  --
  rInterPol.IdePol          := p_nIdePol;
  rInterPol.CodCanal        := cCodCanal;
  rInterPol.NumId           := p_nNumId;
  rInterPol.CodAgencia      := p_cCodAgencia;

  IF p_cCodOfiSusc IS NULL  THEN
    rInterPol.CodOfiSusc    := PR_Intermediacion.Obtener_OfiSusc( p_nNumid, p_cCodAgencia);
  ELSE
  rInterPol.CodOfiSusc    := p_cCodOfiSusc;
  END IF;
  --
  rInterPol.CodPuestoDepend := PR_Intermediacion.Obtener_PuestoDepend(cCodCanal, p_nNumId, rInterPol.CodOfiSusc);
  rInterPol.NumIdSup        := PR_Intermediacion.Obtener_NumIdSup(rInterPol.CodPuestoDepend, cCodCanal, p_nNumId);
  rInterPol.IndLider        := p_cIndLider;
  rInterPol.PorcPart        := p_nPorcPart;
  rInterPol.StsInter        := 'ACT';
  rInterPol.FecIngInter     := dFecha;
  rInterPol.FecStsInter     := dFecha;
  --
  rInterPol.TipoCanal_MC    := cTPCanal;
  --
  PR_Intermediacion.Elimina_Intermediacion_Pol(p_nIdePol);
END ModCorredorPreInsert;


PROCEDURE ModCorredorInsert IS
BEGIN
  INSERT INTO INTERMEDIACION_POL(idepol,
    codcanal, numid, codagencia, codofisusc, codpuestodepend,
    numidsup, indlider, porcpart, stsinter, fecinginter,
    fecstsinter, tipocanal_mc )
  VALUES ( rInterPol.idepol,
    rInterPol.codcanal, rInterPol.numid, rInterPol.codagencia, rInterPol.codofisusc, rInterPol.codpuestodepend,
    rInterPol.numidsup, rInterPol.indlider, rInterPol.porcpart, rInterPol.stsinter, rInterPol.fecinginter,
    rInterPol.fecstsinter, rInterPol.TipoCanal_MC);
END;


PROCEDURE ModCorredorInicio IS
--
nExiste       PLS_INTEGER;
--
BEGIN
  IF p_cCodCanal IS NOT NULL AND p_cTipoCanal IS NULL THEN
     bParametro := TRUE;
     cCodCanal  := p_cCodCanal;
     cTPCanal   := NULL;
  END IF;
  --
  IF p_cCodCanal IS NOT NULL AND p_cTipoCanal IS NOT NULL THEN
     bParametro := TRUE;
     cCodCanal  := p_cCodCanal;
     cTPCanal   := p_cTipoCanal;
  END IF;
  --
  IF NOT bParametro THEN
    BEGIN
      SELECT CodCanal, 1, TIPOCANAL_MC
      INTO   cCodCanal, nExiste, cTPCanal
      FROM   INTERMEDIACION_POL
      WHERE  IdePol   = p_nIdePol
        AND  IndLider = 'S';
    EXCEPTION
      WHEN OTHERS THEN
        nExiste   := 0;
        cCodCanal := '3';
    END;
    --
    IF ( cCodCanal ='8' AND cTPCanal is NULL) THEN
    cTPCanal := '802';
    --
    UPDATE INTERMEDIACION_POL
    SET    TIPOCANAL_MC = cTPCanal
    where  CodCanal     = '8'
      and  TIPOCANAL_MC is NULL
      and  idepol    = p_nIdePol;
    END IF;
    --
    IF cCodCanal != '8' THEN
       cTPCanal := NULL;
    END IF;
  END IF;
  --
  dbms_output.put_line('cCodCanal1:'||cCodCanal);
END ModCorredorInicio;
--
BEGIN
  dbms_output.put_line('~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ');
  p_cError := '0,OK';
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'ModificacionCorredor','',nSecuencia );
  --
  -- Validacion del estado la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de p�liza'; RETURN; END IF;
  -- Validacion del estado de la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de estado de p�liza'; RETURN; END IF;
  --
  --<I RTC 313350> / CBayona / 29-09-2022 / Validaci�n de agencia correcta para nuevo corredor
  SELECT COUNT(1)
  INTO nExiste
  FROM Agencias
  WHERE TipoId = '0'
    AND NumId  = p_nNumId
    AND DvId   = '0'
    AND NumDetAgencia = p_cCodAgencia;
  --
  IF nExiste = 0 THEN
    p_cError := '1,C�digo de agencia no corresponde al corredor';
    RETURN;
  END IF;
  --<F RTC 313350>
  --
  cRestricSAS := pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
  --
  IF NOT cRestricSAS IS NULL THEN
     p_cError := '1,' || cRestricSAS;
     RETURN;
  END IF;
  --
  ModCorredorInicio;
  --
  --<I RTC 313350> / CBayona / 29-09-2022 / Validaci�n si nuevo corredor es v�lido para la p�liza
  SELECT MAX(NumId)
  INTO nNumIdDest
  FROM Cliente
  WHERE CodCli = (SELECT CodCli
                  FROM Poliza
                  WHERE IdePol = p_nIdePol);
  --
  SELECT COUNT(1)
  INTO nExiste
  FROM Relaciones_Terceros rt,
    Tercero t,
    Intermediario i
  WHERE rt.TipoRel IN ('RELACLIE', 'RELAINVE')
    AND rt.CodRel = SUBSTR(pr.Busca_Lval('RELTERFV', cCodCanal), 1, 4)
    AND rt.TipoIdDest = '0'
    AND rt.NumIdDest = nNumIdDest
    AND rt.DvIdDest = '0'
    AND rt.StsRel = 'ACT'
    AND rt.NumIdOrig = p_nNumId
    AND t.TipoId = rt.TipoIdOrig
    AND t.NumId = rt.NumIdOrig
    AND t.DvId = rt.DvIdOrig
    AND i.TipoId = t.TipoId
    AND i.NumId = t.NumId
    AND i.DvId = t.DvId
    AND i.StsInter ='ACT'
    AND NOT EXISTS (SELECT 1
                     FROM   Intermediacion_Pol ip
                     WHERE  IdePol = p_nIdePol
                       AND  ip.NumId = i.NumId )
    AND NOT EXISTS (SELECT 1
                    FROM   Tercero_Fonetico f
                    WHERE  f.NumId = i.NumId
                      AND  f.TipoReg = 'D')
    AND i.TipoInter != 'D';
  --
  IF nExiste = 0 THEN
    p_cError := '1,No puede utilizarse este corredor porque no est� relacionado o ya esta registrado en la p�liza';
    RETURN;
  END IF;
  --<F RTC 313350>

  ModCorredorPreInsert;
  ModCorredorInsert;
  ModCorredorGrabar;
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'ModificacionCorredor','',nSecuencia );
EXCEPTION
    WHEN OTHERS THEN
        p_cError := '1,'|| SQLERRM;
    PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,NULL,NULL,'LOG','ERR' ,SYSDATE,SYSDATE,'ModificacionCorredor','',nSecuencia );
END ModificacionCorredor;

-- <fin RTC 313350> rbarreda -----------------------------------------------------------------------

  /*------------------------------------------------------------------------------------------------
  Nombre      : modificar_comision
  Proposito   : Procedimiento principal para modificar comision a nivel de Poliza
  Referencias : RTC-314182 Cambio de Comision
  Parametros  :
                p_idepol        NUMBER        Identificador de Poliza
                p_porccom       NUMBER        Porcentaje de comision
                p_numtramite    VARCHAR2      Numero de Tramite
                p_autorizador   VARCHAR2      Usuario que autoriza la comision
                p_codusuario    VARCHAR2      NumId del Usuario
                p_mensajes      REF CURSOR    Mensajes de error
                p_error         VARCHAR2      Mensaje de error

  Log de Cambios
    Fecha         Autor            Descripcion
    28/06/2022    Mauro Zegarra    Creacion
    26/09/2022    Mauro Zegarra    Se agrega parametro p_codusuario
  ------------------------------------------------------------------------------------------------*/
  PROCEDURE modificar_comision(p_idepol      poliza.idepol%type,
                               p_porccom     comision_especial.porccom%type,
                               p_numtramite  com_esp_tramite.numtramite%type,
                               p_autorizador comision_especial.usuautor%type,
                               p_codusuario  varchar2,
                               p_mensajes    out sys_refcursor,
                               p_error       out varchar2) is
    l_seq log_poliza_sas_ax.secuencia%type;

  begin
    p_error := '1';
    pr_interfase_ax.limpiar_log_mensajes;
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('I',
                                                      p_idepol,
                                                      null,
                                                      null,
                                                      'LOG',
                                                      null,
                                                      sysdate,
                                                      sysdate,
                                                      'modificar_comision',
                                                      '',
                                                      l_seq);

    pr_interfase_ax_sas_util.cargar_usuario_sesion(p_codusuario);

    revertir_emision(p_idepol);
    cambiar_comision(p_idepol, p_porccom, p_numtramite, p_autorizador);

    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F',
                                                      p_idepol,
                                                      null,
                                                      null,
                                                      'LOG',
                                                      'OK',
                                                      sysdate,
                                                      sysdate,
                                                      'modificar_comision',
                                                      '',
                                                      l_seq);

    p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
    p_error    := '0,OK';

  exception
    when others then
      p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
      p_error    := '1,' || sqlerrm;
      pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax('F',
                                                        p_idepol,
                                                        null,
                                                        null,
                                                        'LOG',
                                                        'ERR',
                                                        sysdate,
                                                        sysdate,
                                                        'modificar_comision',
                                                        '',
                                                        l_seq);
  end;
  /*------------------------------------------------------------------------------------------------
  Nombre      : revertir_emision
  Proposito   : Revertir operacion de 'EMI' en estado 'ACTIVA'
  Referencias : <RTC-314182> Cambio de Comision
                DISTREA.PAGE01_1: Button "0. Revertir Operacion"
  Parametros  :
                p_idepol    NUMBER    Identificador de Poliza

  Log de Cambios
    Fecha         Autor            Descripcion
    02/08/2022    Mauro Zegarra    Creacion
    14/09/2022    Mauro Zegarra    Adecuaciones para mas de un certificado
    26/09/2022    Mauro Zegarra    Uso de pr_sesion.usuario_app
    02/06/2023    Christian Bayona <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
  ------------------------------------------------------------------------------------------------*/
  procedure revertir_emision(p_idepol oper_pol.idepol%type) is
    l_numoper oper_pol.numoper%type;
    ----------------------------------------------
    procedure validar(p_idepol poliza.idepol%type, p_numoper out oper_pol.numoper%type) is
      C_REVERTIR constant oper_usuario_acceso.codoper%type := '035';
      l_count             pls_integer := 0;
      l_numcert           certificado.numcert%type;
      l_ideremesa         oper_pol_remesa_bco.ideremesa%type;
      l_indproc           oper_pol_remesa_bco.indproc%type;
      l_direc_riesgo      varchar2(1) := 'N';
      l_direc_riesgo_oper varchar2(1) := 'N';
      l_cta_cte_rea       pls_integer := 0;
      l_existe_prv        pls_integer := 0;
      r_poliza            poliza%rowtype;
      l_indmovpolren      poliza.indmovpolren%type;
      l_politica          poliza.indmovpolren%type;

      cursor c_remesa_bco is
        select ideremesa, indproc from oper_pol_remesa_bco where numoper = p_numoper;

      cursor c_operaciones(p_numcert oper_pol.numcert%type) is
        select tipoop, numoper
          from oper_pol
         where idepol = p_idepol
           and numcert = p_numcert
           and numoper > p_numoper -- operaciones posteriores!
           and fecanul is null
           and indanul is null;

      cursor c_facturas is
        select stsfact
          from factura
         where numoper = p_numoper
           and rownum < 500
         order by stsfact;

      cursor c_obligaciones is
        select stsoblig from obligacion where numoper = p_numoper;

      cursor c_certificados is
        select c.numcert
          from certificado c
         where c.idepol = p_idepol
           and c.stscert = 'ACT';
      --------------------------------------------
      function existe_direccion_riesgo(p_numoper oper_pol.numoper%type) return varchar2 is
        l_valor varchar2(1);

      begin
        dbms_output.put_line(':mz existe_direccion_riesgo');

        select 'S'
          into l_valor
          from direc_riesgo_cert dr, mod_direc_riesgo md
         where md.numoper = p_numoper
           and dr.idedirec = md.idedirec
           and dr.stsdirec <> 'ANU';

        return l_valor;

      exception
        when too_many_rows then
          return 'S';
        when others then
          return 'N';
      end;
      --------------------------------------------
      -- llamado despues de validar que la poliza tiene un solo certificado activo
      -- al anular la factura el stscert pasa de ACT->INC
      function get_numcert(p_idepol poliza.idepol%type) return certificado.numcert%type is
        l_numcert certificado.numcert%type;

      begin
        dbms_output.put_line(':mz get_numcert:ACT');

        select c.numcert
          into l_numcert
          from certificado c
         where c.idepol = p_idepol
           and c.stscert = 'ACT';

        return l_numcert;

      exception
        when others then
          return null;
      end;
      --------------------------------------------
    begin
      dbms_output.put_line(':mz revertir_emision.validar:ini');

      if pr_acceso_usuario.existe_operusuario(pr_sesion.usuario_app, C_REVERTIR) = 'N' then
        -- 'Usuario no tiene acceso para realizar la operacion de anulacion de operacion'
        pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-0174');
        raise_application_error(-20100, 'USUARIO SIN ACCESO');
      end if;

      begin
        -- obtener operacion 'EMI' 'ACTIVA'
        select distinct t.numoper
          into p_numoper
          from oper_pol t
         where t.idepol = p_idepol
           and t.tipoop = 'EMI'
           and t.indanul is null
           and t.fecanul is null;

      exception
        when others then
          -- 'la poliza no tiene una operacion EMI ACTIVA'
          pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1508');
          raise_application_error(-20100, 'VALIDAR');
      end;

      for r_certificado in c_certificados loop
        select count(*)
          into l_count
          from oper_pol t
         where t.idepol = p_idepol
           and t.numcert = r_certificado.numcert
           and t.numoper > p_numoper -- operaciones posteriores!
           and t.fecanul is null
           and t.indanul is null
           and t.tipoop <> 'ESV';

        if l_count > 0 then
          -- No puede revertir esta operacion porque posee operaciones posteriores de tipo diferente a ESV
          pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1509');
          raise_application_error(-20100, 'VALIDAR');
        end if;
      end loop;

      select count(*)
        into l_count
        from factura t
       where t.numoper = p_numoper
         and t.stsfact <> 'INC';

      if l_count > 0 then
        -- No puede revertir factura con estado diferente a 'INC'
        pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1521');
        raise_application_error(-20100, 'FACTURA NO INC');
      end if;

      for r_factura in c_facturas loop
        if r_factura.stsfact in ('COB', 'ANU') then
          -- No puede revertir esta operacion porque posee facturas cobradas o anuladas
          pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1511');
          raise_application_error(-20100, 'VALIDAR');
        end if;
      end loop;

      open c_remesa_bco;
      fetch c_remesa_bco
        into l_ideremesa, l_indproc;
      close c_remesa_bco;

      if l_ideremesa is not null and l_indproc = 'S' then
        -- No se puede revertir la operacion, ya se genero Remesa del Bco. Continental asociada a la misma.
        pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-0178');
        raise_application_error(-20100, 'VALIDAR');
      end if;

      -- Validando que la operacion a revertir no tenga Direcciones de Riesgo activas
      l_direc_riesgo := existe_direccion_riesgo(p_numoper);

      for r_certificado in c_certificados loop
        for r_operacion in c_operaciones(r_certificado.numcert) loop
          -- Validando las operaciones posteriores ESV que no tengan direcciones de riesgo
          if l_direc_riesgo = 'S' then
            l_direc_riesgo_oper := existe_direccion_riesgo(r_operacion.numoper);

            if l_direc_riesgo_oper = 'S' then
              -- No puede revertir esta operacion porque posee Direcciones de Riesgo en estado ACT al igual que sus posteriores
              pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1510');
              raise_application_error(-20100, 'VALIDAR');
            end if;
          end if;
        end loop;
      end loop;

      for r_obligacion in c_obligaciones loop
        if r_obligacion.stsoblig not in ('ACT') then
          -- No puede revertir esta operacion porque posee Obligaciones pagadas o anuladas
          pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1512');
          raise_application_error(-20100, 'VALIDAR');
        end if;
      end loop;

      l_cta_cte_rea := pr_cta_cte_rea.doc_cta_cte_pag(p_numoper, 'ACTPOL');
      if l_cta_cte_rea > 0 then
        -- No puede revertir esta operacion porque tiene Cuenta Corriente de Reaseguro*
        pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1513');
        raise_application_error(-20100, 'VALIDAR');
      end if;

      r_poliza := pr_poliza.datos_poliza(p_idepol);

      if pr_producto.es_nativo(r_poliza.codprod) = 'N' then
        -- No puede revertir esta operacion porque el producto no es nativo*
        pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1514');
        raise_application_error(-20100, 'VALIDAR');
      end if;

      begin
        select 1
          into l_existe_prv
          from prv_dinero pd
         where pd.ideprg in
               (select pe.ideprg
                  from prv_dinero_elem pe
                 where pe.numdoc in
                       (select ct.numdocmov from cta_cte_rea ct where ct.numoper = p_numoper))
           and pd.origprv = 'REA'
           and pd.stsprv in ('PRO', 'ABO')
           and rownum = 1;

      exception
        when others then
          l_existe_prv := 0;
      end;

      if l_existe_prv > 0 then
        -- 'No puede revertir la operacion, pues tiene liquidaciones de Reaseguros con Abono en Proceso'
        pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1515');
        raise_application_error(-20100, 'VALIDAR');
      end if;

      if r_poliza.indfacult = 'S' then
        l_indmovpolren := r_poliza.indmovpolren;

        if l_indmovpolren in ('0', '2') or l_indmovpolren is null then
          l_politica := pr_politicas.principal(p_idepol,
                                               r_poliza.numpol,
                                               r_poliza.codprod,
                                               pr_sesion.usuario_app,
                                               'AC');
          begin
            select indmovpolren
              into l_indmovpolren
              from poliza
             where idepol = p_idepol
               and codprod = r_poliza.codprod;

          exception
            when no_data_found then
              l_indmovpolren := '0';
            when too_many_rows then
              l_indmovpolren := '0';
          end;

          if l_indmovpolren = '1' or l_politica = '1' then
            -- 'Existen Politicas Violadas... Verifique...'
            pr_interfase_ax_sas_util.insertar_log_errores('revertir_emision_validar', 'VAL-1516');
            raise_application_error(-20100, 'VALIDAR');
          end if;
        end if;
      end if;

      dbms_output.put_line(':mz revertir_emision.validar:fin');
    end;
    ----------------------------------------------
    procedure revertir_docs(p_idepol poliza.idepol%type, p_numoper oper_pol.numoper%type) is
      l_numtramite      poliza_tramite.numtramite%type;
      l_es_unif         poliza.tipofactunif%type;
      l_idepol          oper_pol.idepol%type;
      l_numcert         oper_pol.numcert%type;
      l_existe_oper_act pls_integer := 0;

      cursor c_obliganu is
        select o.numoblig from obligacion o where o.numoper = p_numoper;

      cursor c_operaciones is
        select idepol, numcert
          from oper_pol op
         where op.numoper = p_numoper
         order by idepol, numcert;

      cursor resp is
        select codcli
          from resp_pago_mov b
         where b.idepol = l_idepol + 0
           and b.numcert = l_numcert + 0
           and b.numoper = p_numoper;

      --<I RTC 316878> / Christian Bayona / 02-06-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
      CURSOR c_ComEsp IS
        SELECT IdeComision
        FROM Comision_Especial
        WHERE IdePol = p_idepol
          AND StsTasa = 'ACT';
      --<F RTC 316878>

    begin
      dbms_output.put_line(':mz revertir_docs:ini');

      l_es_unif := pr_poliza.verifica_fact_unificada(p_idepol);

      if l_es_unif = 'S' then
        pr_dec_transporte.anu_oper_declar_unif(p_numoper);
      else
        begin
          pr_factura.fact_oper_anu(p_numoper);
        exception
          when others then
            raise_application_error(-20100, 'pr_factura.fact_oper_anu: ' || sqlerrm);
        end;

        pr_det_acre_rea_coa.revertir_operacion(p_numoper);

        for roa in c_obliganu loop
          pr_registro_primas_emi.anular_docs('O', roa.numoblig, 'ANULA', p_numoper, null);
        end loop;
      end if;

      delete from oper_pol_acsel_macsi where numoper = p_numoper;

      begin
        pr_reca_dcto_certif.rever_reca_dcto_cert(p_numoper);
      exception
        when others then
          raise_application_error(-20100, 'pr_reca_dcto_certif.rever_reca_dcto_cert: ' || sqlerrm);
      end;

      if (pr.busca_lval('SWITCH', 'GCONTREA') = 'S') then
        begin
          pr_cta_cte_rea_proc.reversa_anula_cta_cte(p_idepol, p_numoper, 'REVER');
        exception
          when others then
            raise_application_error(-20100,
                                    'pr_cta_cte_rea_proc.reversa_anula_cta_cte: ' || sqlerrm);
        end;
      else
        begin
          pr_cta_cte_rea.reversa_anula_cta_cte(p_idepol, p_numoper, 'REVER');
        exception
          when others then
            raise_application_error(-20100, 'pr_cta_cte_rea.reversa_anula_cta_cte: ' || sqlerrm);
        end;
      end if;

      begin
        pr_genera_prv.reviertedocprovision(p_numoper, p_idepol);
      exception
        when others then
          raise_application_error(-20100, 'pr_genera_prv.reviertedocprovision: ' || sqlerrm);
      end;

      begin
        select numtramite
          into l_numtramite
          from poliza_tramite
         where idepol = p_idepol
           and numoper = p_numoper
           and rownum < 2;

      exception
        when no_data_found then
          l_numtramite := null;
      end;

      if l_numtramite is not null then
        begin
          pr_poliza_tramite.revertiroperacion(p_idepol, l_numtramite, p_numoper);

          delete log_ccm_tramites
           where numtramite = l_numtramite
             and numoper = p_numoper;

        exception
          when others then
            raise_application_error(-20100, 'pr_poliza_tramite.revertiroperacion: ' || sqlerrm);
        end;
      end if;

      if pr.busca_lval('CMBDEVPR', 'ESTADO') = 'ACTIVADO' then
        for operacion in c_operaciones loop
          l_idepol  := operacion.idepol;
          l_numcert := operacion.numcert;

          select count(*)
            into l_existe_oper_act
            from oper_pol
           where idepol = l_idepol + 0
             and numcert = l_numcert + 0
             and numoper <> p_numoper
             and nvl(indanul, 'N') = 'N'
             and fecanul is null;

          -- no existen otras operaciones activas
          if l_existe_oper_act = 0 then
            for r in resp loop
              -- eliminar el registro del % gasto emision para la poliza, certificado, resp.pago
              delete from porc_der_emi
               where idepol = l_idepol
                 and numcert = l_numcert
                 and codcli = r.codcli;
            end loop;
          end if;
        end loop;
      end if;

      --<I RTC 316878> / Christian Bayona / 02-06-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
      FOR rCE IN c_ComEsp LOOP
        pr_Comision_Especial.Anular(rCE.IdeComision, 'ANULA');
      END LOOP;
      --<F RTC 316878>

      dbms_output.put_line(':mz revertir_docs:fin');
    end;
    ----------------------------------------------
  begin
    dbms_output.put_line(':mz revertir_emision:ini');

    validar(p_idepol, l_numoper);
    revertir_docs(p_idepol, l_numoper);

    dbms_output.put_line(':mz revertir_emision:fin');

  exception
    when others then
      pr_interfase_ax_sas_util.insertar_log_errores('pr_interfase_ax.revertir_emision',
                                                    pr_interfase_ax_sas.kcodmsgerror,
                                                    sqlerrm);
      raise_application_error(-20100, 'revertir_emision: ' || sqlerrm);
  end;
  /*------------------------------------------------------------------------------------------------
  Nombre      : cambiar_comision
  Proposito   : Cambiar la comision a la poliza
  Referencias : <RTC-314182> Cambio de Comision

  Parametros  :
                p_idepol         NUMBER      Identificador de Poliza
                p_porccom        NUMBER      Porcentaje de comision
                p_numtramite     VARCHAR2    Numero de Tramite
                p_autorizador    VARCHAR2    Usuario que autoriza la comision

  Log de Cambios
    Fecha         Autor            Descripcion
    02/08/2022    Mauro Zegarra    Creacion
    14/09/2022    Mauro Zegarra    Adecuaciones para mas de un certificado
    26/09/2022    Mauro Zegarra    Uso de pr_sesion.usuario_app
  ------------------------------------------------------------------------------------------------*/
  procedure cambiar_comision(p_idepol      poliza.idepol%type,
                             p_porccom     comision_especial.porccom%type,
                             p_numtramite  com_esp_tramite.numtramite%type,
                             p_autorizador comision_especial.usuautor%type) is
    l_idecomision comision_especial.idecomision%type;
    ----------------------------------------------
    procedure validar(p_porccom     comision_especial.porccom%type,
                      p_numtramite  com_esp_tramite.numtramite%type,
                      p_autorizador comision_especial.usuautor%type) is
      C_COMISION_ESPECIAL constant oper_usuario_acceso.codoper%type := '050';
      l_com_max comision_especial.porccom%type;

    begin
      dbms_output.put_line(':mz cambiar_comision.validar:ini');

      if pr_acceso_usuario.existe_operusuario(pr_sesion.usuario_app, C_COMISION_ESPECIAL) = 'N' then
        -- 'Usuario no tiene acceso para Comisiones Especiales'
        pr_interfase_ax_sas_util.insertar_log_errores('CAMBIAR_COMISION_VALIDAR', 'VAL-1517');
        raise_application_error(-20100, 'USUARIO SIN ACCESO');
      end if;

      if p_porccom is null then
        -- 'Debe ingresar un valor para el porcentaje de comision'
        pr_interfase_ax_sas_util.insertar_log_errores('CAMBIAR_COMISION_VALIDAR', 'VAL-1518');
        raise_application_error(-20100, 'PORCENTAJE NULO');
      end if;

      if p_porccom < 0.00 then
        -- 'No esta permitido ingresar porcentaje negativo'
        pr_interfase_ax_sas_util.insertar_log_errores('CAMBIAR_COMISION_VALIDAR', 'VAL-1519');
        raise_application_error(-20100, 'PORCENTAJE NEGATIVO');
      end if;

      l_com_max := to_number(pr.busca_lval('COMITOPE', '01'));

      if p_porccom > l_com_max then
        -- 'El porcentaje ingresado: :mx1% supera el m�ximo permitido: :mx2%'
        pr_interfase_ax_sas_util.insertar_log_errores('CAMBIAR_COMISION_VALIDAR',
                                                      'VAL-1520',
                                                      p_porccom,
                                                      l_com_max);
        raise_application_error(-20100, 'PORCENTAJE MAYOR AL MAXIMO');
      end if;

      if p_numtramite is null then
        -- 'Debe ingresar el numero de tramite de la comision especial'
        pr_interfase_ax_sas_util.insertar_log_errores('CAMBIAR_COMISION_VALIDAR', 'VAL-1522');
        raise_application_error(-20100, 'TRAMITE NULO');
      end if;

      if p_autorizador is null then
        -- 'Debe ingresar el usuario autorizador de la comision especial'
        pr_interfase_ax_sas_util.insertar_log_errores('CAMBIAR_COMISION_VALIDAR', 'VAL-1523');
        raise_application_error(-20100, 'AUTORIZADOR NULO');
      end if;

      dbms_output.put_line(':mz cambiar_comision.validar:fin');
    end;
    ----------------------------------------------
    procedure guardar_comision(p_idepol      poliza.idepol%type,
                               p_porccom     comision_especial.porccom%type,
                               p_numtramite  com_esp_tramite.numtramite%type,
                               p_autorizador comision_especial.usuautor%type,
                               p_idecomision out comision_especial.idecomision%type) is
      --------------------------------------------
      procedure registrar_comision(p_idepol      poliza.idepol%type,
                                   p_porccom     comision_especial.porccom%type,
                                   p_autorizador comision_especial.usuautor%type,
                                   p_idecomision out comision_especial.idecomision%type) is
        l_numcert     certificado.numcert%type;
        r_poliza      poliza%rowtype;
        r_certificado certificado%rowtype;
        r_comision    comision_especial%rowtype;
        -----------------------------------------
        procedure insert_comision_especial(p_comision comision_especial%rowtype) is
        begin
          insert into comision_especial values p_comision;
        exception
          when others then
            raise_application_error(-20100, 'insert_comision_especial: ' || sqlerrm);
        end;
        -----------------------------------------
        -- llamado despues de validar que la poliza tiene un solo certificado activo
        -- al anular la factura el stscert pasa de ACT->INC
        function get_numcert(p_idepol poliza.idepol%type) return certificado.numcert%type is
          l_numcert certificado.numcert%type;

        begin
          dbms_output.put_line(':mz get_numcert:INC');

          select c.numcert
            into l_numcert
            from certificado c
           where c.idepol = p_idepol
             and c.stscert = 'INC';

          return l_numcert;

        exception
          -- una operacion puede tener varios certificados asociados, al anular
          -- la factura, estos certificados cambian al estado INC
          when too_many_rows then
            select max(c.numcert)
              into l_numcert
              from certificado c
             where c.idepol = p_idepol
               and c.stscert = 'INC';

            return l_numcert;
          when others then
            return null;
        end;
        -----------------------------------------
      begin
        dbms_output.put_line(':mz registrar_comision:ini');

        r_poliza := pr_poliza.datos_poliza(p_idepol);

        l_numcert     := get_numcert(p_idepol);
        r_certificado := pr_certificado.datos_cert(p_idepol, l_numcert);

        select sq_idecomi.nextval into p_idecomision from sys.dual;

        r_comision.idecomision := p_idecomision;
        r_comision.idepol      := p_idepol;
        r_comision.indaplica   := 'P';
        r_comision.codigo      := r_poliza.numpol;
        r_comision.induso      := 'X';
        r_comision.porccom     := p_porccom;

        r_comision.fecinitasa := r_certificado.fecing;
        r_comision.fecfintasa := r_certificado.fecfin;
        r_comision.usuautor   := p_autorizador;
        r_comision.usuaing    := pr_sesion.usuario_app;
        r_comision.fecing     := sysdate;
        r_comision.ststasa    := 'VAL';
        r_comision.fecststasa := sysdate;
        r_comision.sisorig    := '01';

        insert_comision_especial(r_comision);

        dbms_output.put_line(':mz registrar_comision:fin');
      exception
        when others then
          raise_application_error(-20100, 'registrar_comision: ' || sqlerrm);
      end;
      -------------------------------------------
      procedure registrar_tramite(p_idecomision comision_especial.idecomision%type,
                                  p_numtramite  com_esp_tramite.numtramite%type) is
        r_com_tramite com_esp_tramite%rowtype;
        ------------------------------------------
        procedure insert_com_esp_tramite(p_com_tramite com_esp_tramite%rowtype) is
        begin
          insert into com_esp_tramite values p_com_tramite;
        exception
          when others then
            raise_application_error(-20100, 'insert_com_esp_tramite: ' || sqlerrm);
        end;
        ------------------------------------------
      begin
        dbms_output.put_line(':mz registrar_tramite:ini');

        r_com_tramite.idecomision := p_idecomision;
        r_com_tramite.numtramite  := p_numtramite;

        insert_com_esp_tramite(r_com_tramite);

        dbms_output.put_line(':mz registrartramite:fin');
      end;
      --------------------------------------------
    begin
      dbms_output.put_line(':mz guardar_comision:ini');

      registrar_comision(p_idepol, p_porccom, p_autorizador, p_idecomision);
      registrar_tramite(p_idecomision, p_numtramite);

      dbms_output.put_line(':mz guardar_comision:fin');
    exception
      when others then
        raise_application_error(-20100, 'guardar_comision: ' || sqlerrm);
    end;
    ------------------------------------------------
    procedure activar_comision(p_idecomision comision_especial.idecomision%type) is
      --------------------------------------------
      procedure registrar_log(p_idecomision comision_especial.idecomision%type) is
        l_ide     number;
        r_control eli_act_no_control%rowtype;
        ------------------------------------------
        procedure insert_eli_act_no_control(p_control eli_act_no_control%rowtype) is
        begin
          dbms_output.put_line(':mz insert_eli_act_no_control:ini');

          insert into eli_act_no_control values p_control;

          dbms_output.put_line(':mz insert_eli_act_no_control:fin');
        exception
          when others then
            raise_application_error(-20100, 'insert_eli_act_no_control: ' || sqlerrm);
        end;
        ------------------------------------------
      begin
        dbms_output.put_line(':mz registrar_log:ini');

        select acselx.sq_eli_act_no_control.nextval into l_ide from sys.dual;

        r_control.ide     := l_ide;
        r_control.origen  := 'COMISION_ESPECIAL';
        r_control.accion  := 'B';
        r_control.clave   := p_idecomision;
        r_control.usuario := pr_sesion.usuario_app;
        r_control.fecha   := sysdate;

        insert_eli_act_no_control(r_control);

        dbms_output.put_line(':mz registrar_log:fin');
      exception
        when others then
          raise_application_error(-20100, 'registrar_log: ' || sqlerrm);
      end;
      --------------------------------------------
    begin
      dbms_output.put_line(':mz activar_comision:ini');

      pr_comision_especial.activar(p_idecomision, 'ACTIV');
      registrar_log(p_idecomision);

      dbms_output.put_line(':mz activar_comision:fin');
    exception
      when others then
        raise_application_error(-20100, 'activar_comision: ' || sqlerrm);
    end;
    ----------------------------------------------
  begin
    validar(p_porccom, p_numtramite, p_autorizador);
    guardar_comision(p_idepol, p_porccom, p_numtramite, p_autorizador, l_idecomision);
    activar_comision(l_idecomision);

  exception
    when others then
      pr_interfase_ax_sas_util.insertar_log_errores('pr_interfase_ax.cambiar_comision',
                                                    pr_interfase_ax_sas.kcodmsgerror,
                                                    sqlerrm);
      raise_application_error(-20100, 'cambiar_comision: ' || sqlerrm);
  end;
  -- <F RTC 314182>

  -- <I RTC 314175 > -- Jorge Benel / 24-06-2022 / Inicio proceso de modificaci�n de valores declarados


/*-----------------------------------------------------------------------
   Nombre     : EXPORTA_ARCHIVO_LOCALES
   Proposito  : Exporta de locales para su modificaci�n
   Referencia : Formulario MANTCERT.fmb

        :C04_1.BT_EXPORTA

   Parametro  :
        p_nIdePol     Id de la poliza
        p_nNUMCERTINI  Numero de certificado inicio
        p_nNUMCERTFIN  Numero de certificado final
        p_cCODPROD     Codigo de producto
        p_cESTADO      Estado

   Retorno :       p_TDU_TABLA_EXP_LOCAL tabla de locales registrados de la poliza
                   p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                        0,OK
                        1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   24/06/2022   Jorge Benel         <RTC-314175  > Creaci�n
-----------------------------------------------------------------------------*/


PROCEDURE EXPORTA_ARCHIVO_LOCALES (p_nIDEPOL      POLIZA.IDEPOL%TYPE,
                                   p_nNUMCERTINI  CERTIFICADO.NUMCERT%TYPE,
                                   p_nNUMCERTFIN  CERTIFICADO.NUMCERT%TYPE,
                                   p_cCODPROD     POLIZA.CODPROD%TYPE,
                                   p_cESTADO      POLIZA.STSPOL%TYPE,
                                   -- Variables de retorno
                                   p_TDU_TABLA_EXP_LOCAL     OUT TDU_TABLA_EXP_LOCAL,
                                   p_cError                  OUT varchar2
                                   ) AS



FUNCTION GetToken(stringvalues  VARCHAR2,
                  indice        NUMBER,
                  delim         VARCHAR2
                 )
RETURN VARCHAR2

IS

   start_pos NUMBER; -- Posici�n inicial de cada substring
   end_pos   NUMBER; -- Posici�n final de cada substring

BEGIN

   IF indice = 1 THEN
         start_pos := 1; -- La posici�n inicial sera 1
   ELSE
         start_pos := instr(stringvalues, delim, 1, indice - 1);

         IF start_pos = 0 THEN
             RETURN NULL;
         ELSE
             start_pos := start_pos + length(delim);
         END IF;

   END IF;
   end_pos := instr(stringvalues, delim, start_pos, 1);

   IF end_pos = 0 THEN -- Se retorna el ultimo valor del arreglo
         RETURN substr(stringvalues, start_pos);
   ELSE -- Se retorna el valor del arreglo segun el inidice y delim indicado
         RETURN substr(stringvalues, start_pos, end_pos - start_pos);
   END IF;

END GetToken;

PROCEDURE FR_GENERAR_ARCHIVO_CUR IS
  cLinea          VARCHAR2(8000);
  cLinea2         varchar2(1000);
  nPos            NUMBER := 0;
  TYPE ref_cur    IS REF CURSOR;
  c_bien          ref_cur;
  i               NUMBER(5);
  vExiste           NUMBER(5);

  CURSOR CUR_CERT IS
    SELECT NumCert
      FROM CERTIFICADO C
     WHERE C.IdePol = p_nIDEPOL
       AND C.NumCert BETWEEN DECODE(p_nNUMCERTINI,NULL,C.NumCert, p_nNUMCERTINI)
       AND DECODE(p_nNUMCERTFIN,NULL,C.NumCert, p_nNUMCERTFIN)
       AND C.StsCert = DECODE(p_cESTADO,'%', C.StsCert, p_cESTADO)
     ORDER BY 1;

  CURSOR cur(nNUmCert NUMBER) IS
    SELECT DRC.IDEDIREC
    FROM DIREC_RIESGO_CERT DRC
    WHERE DRC.IDEPOL = p_nIDEPOL
    AND DRC.NUMCERT = nNumCert
    ORDER BY 1;
BEGIN

    DBMS_OUTPUT.PUT_LINE('Inicio de proceso: '||TO_CHAR(SYSDATE,'DD/MM/YYYY HH:MI:SS'));

    i := 0;
    p_TDU_TABLA_EXP_LOCAL := acselx.TDU_TABLA_EXP_LOCAL();

    FOR B IN CUR_CERT LOOP
      --
      FOR C IN CUR(B.NumCert) LOOP

        OPEN c_bien
        FOR SELECT
           B.clasebien, B.CodBien
              FROM BIEN_CERT BC, BIEN B, CERTIFICADO C
             WHERE BC.IdePol = p_nIDEPOL
               AND BC.ClaseBien = B.ClaseBien
               AND BC.CodBien = B.CodBien
               AND C.IDEPOL = p_nIDEPOL
               AND C.IDEPOL = BC.IDEPOL
               AND C.NUMCERT = BC.NUMCERT
               AND C.NumCert BETWEEN DECODE(p_nNUMCERTINI, NULL, C.NumCert, p_nNUMCERTINI)
               AND DECODE(p_nNUMCERTFIN, NULL, C.NumCert, p_nNUMCERTFIN)
               AND C.StsCert = DECODE(p_cESTADO, '%', C.StsCert, p_cESTADO)
             GROUP BY b.clasebien, B.CodBien
             ORDER BY B.clasebien, B.CodBien;

        cLinea := PR_BIEN_CERT_UTIL.OBTIENE_LINEA_DIREC(p_nIDEPOL, B.NumCert, p_cCODPROD, C.IdeDirec, c_bien);

        IF LENGTH(cLinea) > 0 THEN
          i := i + 1;

          select replace(cLinea,'"','') INTO cLinea2 from dual;

          p_TDU_TABLA_EXP_LOCAL.EXTEND(1);


          p_TDU_TABLA_EXP_LOCAL(i) := acselx.TDU_EXP_LOCAL(Tipovia  => NULL,
                                            Nomvia  => NULL,
                                            Numvia  => NULL,
                                            Tipoint  => NULL,
                                            Numint  => NULL,
                                            Manzana  => NULL,
                                            Lote  => NULL,
                                            Kilome  => NULL,
                                            Tipocha  => NULL,
                                            --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusión Locales Legacy Dubai Masivo
                                            Nomcha  => NULL,
                                            Tiposdi  => NULL,
                                            --SUBDIV_CH   => NULL,
                                            --NOMBRE_SUBDIV_CH    => NULL,
                                            --<F DIGB2B03-2815>
                                            Nomsdi  => NULL,
                                            Referencia  => NULL,
                                            Resto  => NULL,
                                            Codpais  => NULL,
                                            Codestado  => NULL,
                                            Codciudad  => NULL,
                                            Codmunicipio  => NULL,
                                            Idegiro  => NULL,
                                            Numpisos  => NULL,
                                            Numsotanos  => NULL,
                                            Anoconstrucc  => NULL,
                                            Usoinmueblesbs  => NULL,
                                            Tep  => NULL,
                                            Tee  => NULL,
                                            --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusión Locales Legacy Dubai Masivo
                                            Propieedificacion  => NULL,
                                            Subdiv_Ch  => NULL,
                                            Nombre_Subdiv_Ch  => NULL,
                                            --<F DIGB2B03-2815> 
                                            Descbien1  => NULL,
                                            Mtovaldeclardirec1  => NULL,
                                            Descbien2  => NULL,
                                            Mtovaldeclardirec2  => NULL,
                                            Descbien3  => NULL,
                                            Mtovaldeclardirec3  => NULL,
                                            Descbien4  => NULL,
                                            Mtovaldeclardirec4  => NULL,
                                            Descbien5  => NULL,
                                            Mtovaldeclardirec5  => NULL,
                                            Descbien6  => NULL,
                                            Mtovaldeclardirec6  => NULL,
                                            Descbien7  => NULL,
                                            Mtovaldeclardirec7  => NULL,
                                            Descbien8  => NULL,
                                            Mtovaldeclardirec8  => NULL,
                                            Descbien9  => NULL,
                                            Mtovaldeclardirec9  => NULL,
                                            Descbien10  => NULL,
                                            Mtovaldeclardirec10  => NULL,
                                            Descbien11  => NULL,
                                            Mtovaldeclardirec11  => NULL,
                                            Descbien12  => NULL,
                                            Mtovaldeclardirec12  => NULL,
                                            Descbien13  => NULL,
                                            Mtovaldeclardirec13  => NULL,
                                            Descbien14  => NULL,
                                            Mtovaldeclardirec14  => NULL,
                                            Descbien15  => NULL,
                                            Mtovaldeclardirec15  => NULL,
                                            Descbien16  => NULL,
                                            Mtovaldeclardirec16  => NULL,
                                            Descbien17  => NULL,
                                            Mtovaldeclardirec17  => NULL,
                                            Descbien18  => NULL,
                                            Mtovaldeclardirec18  => NULL,
                                            Valor_Declarado_Total  => NULL,
                                            Porcsuma  => NULL,
                                            Fecinspeccion  => NULL,
                                            Numcert  => NULL,
                                            Cod_Local  => NULL,
                                            --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusión Locales Legacy Dubai Masivo
                                            Indriesgoins  => NULL, 
                                            --IndInspeccion     => NULL,
                                            --SUJETO_INSPECCION    => NULL,
                                            --RER        => NULL,
                                            --INDSININSPECCION    => NULL,
                                            --usuautorizador    => NULL,
                                            --stsdirec           => NULL,
                                            --CUMPLE_NORMA_SBS     => NULL,
                                            --indgeoref  => NULL,
                                            --CLASIFICACION      => NULL   ) ;
                                            Indinspeccion  => NULL,
                                            Notainspeccion  => NULL,
                                            Indsininspeccion  => NULL,
                                            Usuautorizador  => NULL,
                                            Stsdirec  => NULL,
                                            Sujeto_Inspeccion  => NULL,
                                            Indgeoref  => NULL,
                                            Clasificacion  => NULL,
                                            Rer  => NULL,
                                            Cumple_Norma_Sbs  => NULL  ) ;

                p_TDU_TABLA_EXP_LOCAL(i).Tipovia := GetToken(cLinea2,1,',');
                p_TDU_TABLA_EXP_LOCAL(i).Nomvia := GetToken(cLinea2,2,',');
                p_TDU_TABLA_EXP_LOCAL(i).Numvia := GetToken(cLinea2,3,',');
                p_TDU_TABLA_EXP_LOCAL(i).Tipoint := GetToken(cLinea2,4,',');
                p_TDU_TABLA_EXP_LOCAL(i).Numint := GetToken(cLinea2,5,',');
                p_TDU_TABLA_EXP_LOCAL(i).Manzana := GetToken(cLinea2,6,',');
                p_TDU_TABLA_EXP_LOCAL(i).Lote := GetToken(cLinea2,7,',');
                p_TDU_TABLA_EXP_LOCAL(i).Kilome := GetToken(cLinea2,8,',');
                p_TDU_TABLA_EXP_LOCAL(i).Tipocha := GetToken(cLinea2,9,',');

                --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
                --p_TDU_TABLA_EXP_LOCAL(i).nomsdi      := GetToken(cLinea2,10,',');
                --p_TDU_TABLA_EXP_LOCAL(i).SUBDIV_CH   := GetToken(cLinea2,11,',');
                --p_TDU_TABLA_EXP_LOCAL(i).NOMBRE_SUBDIV_CH    := GetToken(cLinea2,12,',');
                p_TDU_TABLA_EXP_LOCAL(i).Nomcha := GetToken(cLinea2,10,',');
                p_TDU_TABLA_EXP_LOCAL(i).Nomcha := GetToken(cLinea2,10,',');
                p_TDU_TABLA_EXP_LOCAL(i).Subdiv_Ch := GetToken(cLinea2,11,',');
                p_TDU_TABLA_EXP_LOCAL(i).Nomsdi := GetToken(cLinea2,12,',');
                --<F DIGB2B03-2815> 

                p_TDU_TABLA_EXP_LOCAL(i).Referencia := GetToken(cLinea2,13,',');
                p_TDU_TABLA_EXP_LOCAL(i).Resto := GetToken(cLinea2,14,',');
                p_TDU_TABLA_EXP_LOCAL(i).Codpais := GetToken(cLinea2,15,',');
                p_TDU_TABLA_EXP_LOCAL(i).Codestado := GetToken(cLinea2,16,',');
                p_TDU_TABLA_EXP_LOCAL(i).Codciudad := GetToken(cLinea2,17,',');
                p_TDU_TABLA_EXP_LOCAL(i).Codmunicipio := GetToken(cLinea2,18,',');
                p_TDU_TABLA_EXP_LOCAL(i).Idegiro := GetToken(cLinea2,19,',');
                p_TDU_TABLA_EXP_LOCAL(i).Numpisos := GetToken(cLinea2,20,',');
                p_TDU_TABLA_EXP_LOCAL(i).Numsotanos := GetToken(cLinea2,21,',');
                p_TDU_TABLA_EXP_LOCAL(i).Anoconstrucc := GetToken(cLinea2,22,',');
                p_TDU_TABLA_EXP_LOCAL(i).Usoinmueblesbs := GetToken(cLinea2,23,',');
                p_TDU_TABLA_EXP_LOCAL(i).Tep := GetToken(cLinea2,24,',');
                p_TDU_TABLA_EXP_LOCAL(i).Tee := GetToken(cLinea2,25,',');
                
                --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
                /*
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN1     := GetToken(cLinea2,26,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec1  := GetToken(cLinea2,27,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN2      := GetToken(cLinea2,28,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec2  := GetToken(cLinea2,29,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN3       := GetToken(cLinea2,30,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec3  := GetToken(cLinea2,31,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN4       := GetToken(cLinea2,32,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec4  := GetToken(cLinea2,33,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN5       := GetToken(cLinea2,34,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec5  := GetToken(cLinea2,35,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN6       := GetToken(cLinea2,36,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec6  := GetToken(cLinea2,37,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN7       := GetToken(cLinea2,38,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec7  := GetToken(cLinea2,39,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN8       := GetToken(cLinea2,40,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec8  := GetToken(cLinea2,41,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN9       := GetToken(cLinea2,42,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec9  := GetToken(cLinea2,43,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN10       := GetToken(cLinea2,44,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec10  := GetToken(cLinea2,45,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN11       := GetToken(cLinea2,46,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec11  := GetToken(cLinea2,47,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN12       := GetToken(cLinea2,48,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec12  := GetToken(cLinea2,49,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN13       := GetToken(cLinea2,50,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec13  := GetToken(cLinea2,51,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN14       := GetToken(cLinea2,52,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec14  := GetToken(cLinea2,53,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN15       := GetToken(cLinea2,54,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec15  := GetToken(cLinea2,55,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN16      := GetToken(cLinea2,56,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec16  := GetToken(cLinea2,57,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN17       := GetToken(cLinea2,58,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec17  := GetToken(cLinea2,59,',');
                p_TDU_TABLA_EXP_LOCAL(i).DESCBIEN18       := GetToken(cLinea2,60,',');
                p_TDU_TABLA_EXP_LOCAL(i).mtovaldeclardirec18  := GetToken(cLinea2,61,',');
                p_TDU_TABLA_EXP_LOCAL(i).VALOR_DECLARADO_TOTAL    := GetToken(cLinea2,62,',');
                p_TDU_TABLA_EXP_LOCAL(i).PORCSUMA       := GetToken(cLinea2,63,',');
                p_TDU_TABLA_EXP_LOCAL(i).fecinspeccion   := TO_DATE(GetToken(cLinea2,64,','),'DD/MM/YYYY');
                p_TDU_TABLA_EXP_LOCAL(i).NumCert       := GetToken(cLinea2,65,',');
                p_TDU_TABLA_EXP_LOCAL(i).COD_LOCAL      := GetToken(cLinea2,66,',');
                p_TDU_TABLA_EXP_LOCAL(i).IndInspeccion     := GetToken(cLinea2,67,',');
                p_TDU_TABLA_EXP_LOCAL(i).SUJETO_INSPECCION    := GetToken(cLinea2,68,',');
                p_TDU_TABLA_EXP_LOCAL(i).RER        := GetToken(cLinea2,69,',');
                p_TDU_TABLA_EXP_LOCAL(i).INDSININSPECCION    := GetToken(cLinea2,70,',');
                p_TDU_TABLA_EXP_LOCAL(i).usuautorizador    := GetToken(cLinea2,71,',');
                p_TDU_TABLA_EXP_LOCAL(i).stsdirec           := GetToken(cLinea2,72,',');
                p_TDU_TABLA_EXP_LOCAL(i).CUMPLE_NORMA_SBS     := GetToken(cLinea2,73,',');
                p_TDU_TABLA_EXP_LOCAL(i).indgeoref  := GetToken(cLinea2,74,',');
                p_TDU_TABLA_EXP_LOCAL(i).CLASIFICACION      := GetToken(cLinea2,75,',');
                */
                p_TDU_TABLA_EXP_LOCAL(i).Propieedificacion := GetToken(cLinea2,26,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien1 := GetToken(cLinea2,27,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec1 := GetToken(cLinea2,28,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien2 := GetToken(cLinea2,29,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec2 := GetToken(cLinea2,30,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien3 := GetToken(cLinea2,31,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec3 := GetToken(cLinea2,32,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien4 := GetToken(cLinea2,33,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec4 := GetToken(cLinea2,34,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien5 := GetToken(cLinea2,35,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec5 := GetToken(cLinea2,36,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien6 := GetToken(cLinea2,37,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec6 := GetToken(cLinea2,38,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien7 := GetToken(cLinea2,39,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec7 := GetToken(cLinea2,40,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien8 := GetToken(cLinea2,41,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec8 := GetToken(cLinea2,42,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien9 := GetToken(cLinea2,43,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec9 := GetToken(cLinea2,44,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien10 := GetToken(cLinea2,45,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec10 := GetToken(cLinea2,46,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien11 := GetToken(cLinea2,47,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec11 := GetToken(cLinea2,48,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien12 := GetToken(cLinea2,49,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec12 := GetToken(cLinea2,50,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien13 := GetToken(cLinea2,51,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec13 := GetToken(cLinea2,52,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien14 := GetToken(cLinea2,53,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec14 := GetToken(cLinea2,54,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien15 := GetToken(cLinea2,55,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec15 := GetToken(cLinea2,56,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien16 := GetToken(cLinea2,57,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec16 := GetToken(cLinea2,58,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien17 := GetToken(cLinea2,59,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec17 := GetToken(cLinea2,60,',');
                p_TDU_TABLA_EXP_LOCAL(i).Descbien18 := GetToken(cLinea2,61,',');
                p_TDU_TABLA_EXP_LOCAL(i).Mtovaldeclardirec18 := GetToken(cLinea2,62,',');
                p_TDU_TABLA_EXP_LOCAL(i).Valor_Declarado_Total := GetToken(cLinea2,63,',');
                p_TDU_TABLA_EXP_LOCAL(i).Porcsuma := GetToken(cLinea2,64,',');
                p_TDU_TABLA_EXP_LOCAL(i).Fecinspeccion := TO_DATE(GetToken(cLinea2,65,','),'DD/MM/YYYY');
                p_TDU_TABLA_EXP_LOCAL(i).Numcert := GetToken(cLinea2,66,',');
                p_TDU_TABLA_EXP_LOCAL(i).Cod_Local := GetToken(cLinea2,67,',');
                p_TDU_TABLA_EXP_LOCAL(i).Indinspeccion := GetToken(cLinea2,68,',');
                p_TDU_TABLA_EXP_LOCAL(i).Sujeto_Inspeccion := GetToken(cLinea2,68,',');
                p_TDU_TABLA_EXP_LOCAL(i).Rer := GetToken(cLinea2,70,',');
                p_TDU_TABLA_EXP_LOCAL(i).Indsininspeccion := GetToken(cLinea2,71,',');
                p_TDU_TABLA_EXP_LOCAL(i).Usuautorizador := GetToken(cLinea2,72,',');
                p_TDU_TABLA_EXP_LOCAL(i).Stsdirec := GetToken(cLinea2,73,',');
                p_TDU_TABLA_EXP_LOCAL(i).Cumple_Norma_Sbs := GetToken(cLinea2,74,',');
                p_TDU_TABLA_EXP_LOCAL(i).Indgeoref := GetToken(cLinea2,75,',');
                p_TDU_TABLA_EXP_LOCAL(i).Clasificacion := GetToken(cLinea2,76,',');
                p_TDU_TABLA_EXP_LOCAL(i).Indriesgoins := null ;
                p_TDU_TABLA_EXP_LOCAL(i).Notainspeccion := null ;
                --<F DIGB2B03-2815> 

        END IF;
      END LOOP;
    END LOOP;

    CLOSE c_bien;

EXCEPTION
  WHEN OTHERS THEN
    p_cError := '1,'||SQLERRM;

END FR_GENERAR_ARCHIVO_CUR;

BEGIN
  p_cError := '0,OK';

  IF p_nNUMCERTINI IS NOT NULL THEN
    IF p_nNUMCERTFIN IS NULL THEN

      p_cError := '1,'||'Debe ingresar un Numero de Certificado Final.';

      RETURN;
    END IF;
    IF p_nNUMCERTINI > p_nNUMCERTFIN THEN
      p_cError := '1,'||'Debe ingresar un Numero de Certificado Inicial menor al Certificado Final.';
      RETURN;
    END IF;
  END IF;
  IF p_cESTADO IS NULL THEN
    p_cError := '1,'||'Debe ingresar al menos un Estado';
    RETURN;
  END IF;

  IF p_cError ='0,OK' THEN
    FR_GENERAR_ARCHIVO_CUR;
  END IF;

END EXPORTA_ARCHIVO_LOCALES;

/*-----------------------------------------------------------------------------
    Nombre      : GRABA_LINEA_DIR_MASI2
    Proposito   : Importaci�n de Direcciones de Riesgo y Bienes Masivo
    Referencias : ACSELX.PR_BIEN_CERT_UTIL.GRABA_LINEA_DIR_MASI
    Parametros :
                 p_nNumCert     Identificador de Quiebre por Certificado
    Log de Cambios

    Fecha       Autor            Descripcion
    24/06/2022  Jorge Benel      <RTC-314175> Digitalaci�n de Endoso de Valores Declarados
    06/09/2022  Jorge Benel      <RTC-314175> Actualizaci�n logica
    08/11/2022  David Yupanqui        <RTC-315519>  Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
    23/01/2023  Christian Bayona <RTC-316106> Agregar par�metro para remitir error
    29/08/2023  Christian Bayona <DIGB2B03-2877> TREC Mejoras
  ----------------------------------------------------------------------------*/
  FUNCTION GRABA_LINEA_DIR_MASI2 (
      p_nIdePol   IN DIREC_RIESGO_CERT.IDEPOL%TYPE,
      p_cCODPAIS     IN DIREC_RIESGO_CERT.CODPAIS%TYPE,
      p_cCODESTADO   IN DIREC_RIESGO_CERT.CODESTADO%TYPE    ,
      p_cCODCIUDAD   IN DIREC_RIESGO_CERT.CODCIUDAD%TYPE     ,
      p_cCODMUNICIPIO  IN DIREC_RIESGO_CERT.CODMUNICIPIO%TYPE      ,
      p_cUBIGEO     IN DIREC_RIESGO_CERT.UBIGEO%TYPE     ,
      p_cTIPOVIA    IN DIREC_RIESGO_CERT.TIPOVIA%TYPE  ,
      p_cNOMVIA     IN DIREC_RIESGO_CERT.NOMVIA%TYPE  ,
      p_cNUMVIA     IN DIREC_RIESGO_CERT.NUMVIA%TYPE  ,
      p_cTIPOINT    IN DIREC_RIESGO_CERT.TIPOINT%TYPE   ,
      p_cNUMINT     IN DIREC_RIESGO_CERT.NUMINT%TYPE   ,
      p_cMANZANA    IN DIREC_RIESGO_CERT.MANZANA%TYPE   ,
      p_cLOTE       IN DIREC_RIESGO_CERT.LOTE%TYPE  ,
      p_cKILOME     IN DIREC_RIESGO_CERT.KILOME%TYPE   ,
      p_cTIPOCHA    IN DIREC_RIESGO_CERT.TIPOCHA%TYPE   ,
      p_cNOMCHA     IN DIREC_RIESGO_CERT.NOMCHA%TYPE     ,
      p_cTIPOSDI    IN DIREC_RIESGO_CERT.TIPOSDI%TYPE     ,
      p_cNOMSDI     IN DIREC_RIESGO_CERT.NOMSDI%TYPE    ,
      p_cREFERENCIA  IN DIREC_RIESGO_CERT.REFERENCIA%TYPE     ,
      p_cRESTO       IN DIREC_RIESGO_CERT.RESTO%TYPE  ,
      p_nPORCSUMA    IN DIREC_RIESGO_CERT.PORCSUMA%TYPE         ,
      p_nIDEGIRO     IN DIREC_RIESGO_CERT.IDEGIRO%TYPE       ,
      p_nMTOVALDECLARDIREC IN DIREC_RIESGO_CERT.MTOVALDECLARDIREC%TYPE     ,
      p_ctep IN DIREC_RIESGO_CERT.TEP%TYPE,
      p_ctee  IN DIREC_RIESGO_CERT.TEE%TYPE  ,
      p_cAnoconstrucc IN DIREC_RIESGO_CERT.ANOCONSTRUCC%TYPE,
      p_cnumpisos     IN DIREC_RIESGO_CERT.NUMPISOS%TYPE,
      p_cnumsotanos   IN DIREC_RIESGO_CERT.NUMSOTANOS%TYPE,
      p_cfecinspeccion IN DIREC_RIESGO_CERT.FECINSPECCION%TYPE,
      p_cNumCert      IN DIREC_RIESGO_CERT.NUMCERT%TYPE,
      p_cIdeDirec     IN DIREC_RIESGO_CERT.IDEDIREC%TYPE,
      p_cIndRiesgoIns IN DIREC_RIESGO_CERT.INDRIESGOINS%TYPE,
      p_cIndInspeccion    IN DIREC_RIESGO_CERT.INDINSPECCION%TYPE,
      p_cNOTAINSPECCION   IN DIREC_RIESGO_CERT.NOTAINSPECCION%TYPE,
      p_cINDSININSPECCION IN DIREC_RIESGO_CERT.INDSININSPECCION%TYPE,
      p_cUSUAUTORIZADOR    IN DIREC_RIESGO_CERT.USUAUTORIZADOR%TYPE,
      p_cCLASETIPO       IN   VARCHAR2,
      p_cPropieedificacion IN DIREC_RIESGO_CERT.PROPIEEDIFICACION%TYPE, --<RTC-315519> David Yupanqui / 08-11-2022 / Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
      --
      p_nNumCert IN OUT DIREC_RIESGO_CERT.NumCert%TYPE,
      p_cError IN OUT VARCHAR2 --<RTC-316106> / CBayona / 23-01-2023 / Nuevo par�metro de salida
    ) RETURN NUMBER IS

    rDir      DIREC_RIESGO_CERT%ROWTYPE;
    Error_Certificado EXCEPTION;
    Error_Direc_Riesgo EXCEPTION;
    nPosIni   NUMBER(5);
    nPosFin   NUMBER(5);
    cStsCert  CERTIFICADO.StsCert%TYPE;
    cStsDirec DIREC_RIESGO_CERT.StsDirec%TYPE;
    cIndBienValDeclar VARCHAR2(1);
    nExiste   NUMBER;
    cSwNuevaCargaSbs varchar2(10):= PR.BUSCA_LVAL('SWITCH', 'SWNCASBS');--<RTC 313005> Brayan Pereda 28/03/2022 Switch que activa el cambio

  BEGIN
    rDir.codpais   := p_cCODPAIS;
    rDir.codestado := p_ccodestado ;
    rDir.codciudad := p_ccodciudad;
    rDir.codmunicipio := p_ccodmunicipio;
    rDir.ubigeo   := p_cubigeo ;
    rDir.tipovia  :=p_ctipovia ;
    rDir.nomvia   := p_cnomvia;
    rDir.numvia   := p_cnumvia;
    rDir.tipoint  := p_ctipoint;
    rDir.numint   := p_cnumint;
    rDir.manzana  := p_cmanzana;
    rDir.lote     := p_clote;
    rDir.kilome   := p_ckilome ;
    rDir.tipocha  := p_ctipocha;
    rDir.nomcha   := p_cnomcha;
    rDir.tiposdi  := p_ctiposdi;
    rDir.nomsdi     := p_cnomsdi;
    rDir.referencia := p_creferencia;
    rDir.resto      := p_cresto;
    rDir.porcsuma   :=p_nporcsuma;
    rDir.idegiro    := p_nIdegiro;
    rDir.mtovaldeclardirec := p_nMtovaldeclardirec;

    BEGIN
      SELECT Uso, UsoSbs
      INTO rDir.UsoInmueble, rDir.UsoInmuebleSBS
      FROM GIRO_NEGOCIO
      WHERE IdeGiro = rDir.idegiro;

    EXCEPTION
      WHEN OTHERS THEN
        rDir.UsoInmueble:= NULL;
        rDir.UsoInmuebleSBS:=NULL;
    END;

    rDir.tep := p_ctep;

    IF cSwNuevaCargaSbs = 'S' THEN
        rDir.tee := p_ctee;
        IF (rDir.tep = '08' and LENGTH(TRIM(rDir.tee)) = 0) OR (rDir.tep = '08' and rDir.tee IS NULL) THEN
          --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
          --p_cError := '-1,Error en carga de archivo, TEP igual a 12 por favor indicar el TEE correspondiente.';  --<DIGB2B03-2877> / Christian Bayona / 29-08-2023 / TREC Mejoras
          p_cError := '-1,Error en carga de archivo, TEP igual a 08 por favor indicar el TEE correspondiente.';  --<DIGB2B03-2877> / Christian Bayona / 29-08-2023 / TREC Mejoras
          DBMS_OUTPUT.Put_Line('>>> rDir.tep: ' || rDir.tep || ' - ' || p_cError);
          --<F RTC-316106> / CBayona / 23-01-2023
          RETURN -1; --Error en carga de archivo, TEP igual a 12 por favor indicar el TEE correspondiente.
        END IF;
       IF (rDir.tep <> '08' and LENGTH(TRIM(rDir.tee)) > 0) THEN
          --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
          p_cError := '-2,Error en carga de archivo, TEE con valor en TEP distinto de 12.';
          DBMS_OUTPUT.Put_Line('>>> rDir.tep: ' || rDir.tep || ' - ' || p_cError);
          --<F RTC-316106> / CBayona / 23-01-2023
          RETURN -2; --Error en carga de archivo, TEE con valor en TEP distinto de 12.
        END IF;

       IF ( (rDir.tep = '08' and PR.BUSCA_LVAL ('TEESBS', TRIM(rDir.tee))= 'N') OR (rDir.tep = '08' and PR.BUSCA_LVAL ('TEESBS', TRIM(rDir.tee))='INVALIDO')) THEN
         --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
         p_cError := '-3,Codigo TEE invalido.';
         DBMS_OUTPUT.Put_Line('>>> rDir.tep: ' || rDir.tep || ' - ' || p_cError);
         --<F RTC-316106> / CBayona / 23-01-2023
         RETURN -3;--Codigo TEE invalido.
       END IF;

    END IF;
    --<I RTC-315519> David Yupanqui / 07-11-2022 / Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
    rDir.propieedificacion := p_cPropieedificacion;
    IF rDir.propieedificacion IS NOT NULL THEN
      IF PR.BUSCA_LVAL('PESBS', rDir.propieedificacion) = 'INVALIDO' THEN
        --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
        p_cError := '-4,Error en carga de archivo, Propietario Edificacion con valor invalido.';
        DBMS_OUTPUT.Put_Line('>>> rDir.tep: ' || rDir.tep || ' - ' || p_cError);
        --<F RTC-316106> / CBayona / 23-01-2023
        RETURN -4; --Error en carga de archivo, Propietario Edificacion con valor invalido.
      END IF;
    END IF;
  --<F RTC-315519>

    rDir.anoconstrucc  := p_cAnoconstrucc;
    rDir.numpisos      := p_cnumpisos;
    rDir.numsotanos    := p_cnumsotanos;
    rDir.fecinspeccion := p_cfecinspeccion;
    rDir.NumCert       := p_cNumCert;
    rDir.IdeDirec      := p_cIdeDirec;
    rDir.IndRiesgoIns  := p_cIndRiesgoIns;
    rDir.IndInspeccion := p_cIndInspeccion;
    rDir.NOTAINSPECCION   := p_cNOTAINSPECCION;
    rDir.INDSININSPECCION := p_cINDSININSPECCION;
    rDir.USUAUTORIZADOR   := p_cUSUAUTORIZADOR;

    rDir.IdePol := p_nIdePol;

    -- Desde aqui empieza la logica --

    --obtiene zona sismica
    BEGIN
      SELECT CodZona,  CodSubZona
      INTO rDir.ZonSis, rDir.ZonaCiudad
      FROM  ZONASISM
      WHERE CodPais     = rDir.CodPais
        AND CodEstado   = rDir.CodEstado
        AND CodCiudad   = rDir.CodCiudad
        AND CodMunicipio= rDir.CodMunicipio;
    EXCEPTION
      WHEN OTHERS THEN
        rDir.ZonSis := null;
        rDir.ZonaCiudad := null;
    END;
    --validar estado del certificado
    BEGIN
      SELECT StsCert
        INTO cStsCert
        FROM CERTIFICADO
       WHERE IdePol = p_nIdePol
         AND NumCert = rDir.NumCert;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100,'ERROR, CERTIFICADO: ' || rDir.NumCert ||' NO EXISTE.');
        RETURN 0;
    END;

    IF cStsCert IN ('VAL', 'INC') THEN

      IF rDir.IdeDirec IS NULL THEN
        -- Revierte direcciones de riesgo
        dbms_output.put_line('certif Anterior: ' || p_nNumCert);
        IF rDir.NumCert <> p_nNumCert THEN
          dbms_output.put_line('certif : ' || rDir.NumCert);
          p_nNumCert := rDir.NumCert;
          PR_BIEN_CERT_UTIL.REVIERTE_DIREC_RIESGO(p_nIdePol, rDir.NumCert); -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II
        END IF;
        -- creo direccion de riesgo
        rDir.Idedirec := PR_BIEN_CERT_UTIL.GENERA_DIRECRIE(rDir);
        -- incluir
        IF cStsCert = 'INC' THEN
          PR_DIREC_RIESGO_CERT.INCLUIR(p_nIdePol, rDir.NumCert, rDir.Idedirec);
        END IF;

      ELSE
        IF rDir.NumCert <> p_nNumCert THEN
          --valido si existe direccion de riesgo
          BEGIN
            SELECT COUNT(1)
              INTO nExiste
              FROM DIREC_RIESGO_CERT DRC
             WHERE DRC.IdePol = rDir.IdePol
               AND DRC.NumCert = rDir.NumCert;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              nExiste := 0;
          END;
          IF nExiste >= 1 THEN
            BEGIN
              DELETE MOD_DIREC_RIESGO MDR
               WHERE MDR.IdePol = rDir.IdePol
                 AND MDR.Numcert = rDir.NumCert
                 AND MDR.StsDirec IN ('VAL','INC');

             IF SQL%FOUND THEN
                dbms_output.put_line('ELIMINO MOD direci�n 1: ' || SQL%ROWCOUNT);
              END IF;
            END;
            BEGIN
              DELETE DIREC_RIESGO_CERT DRC
               WHERE DRC.IdePol = rDir.IdePol
                 AND DRC.Numcert = rDir.NumCert
                 AND drc.StsDirec IN ('VAL','INC');
              IF SQL%FOUND THEN
                dbms_output.put_line('ELIMINO direci�n 1: ' || SQL%ROWCOUNT);
              END IF;
            END;
          END IF;
        END IF;

        -- creo direccion de riesgo
        rDir.Idedirec := PR_BIEN_CERT_UTIL.GENERA_DIRECRIE(rDir);

        -- incluir
        IF cStsCert = 'INC' THEN
          dbms_output.put_line('INCLUSION direci�n 1: ' || cStsCert);
          PR_DIREC_RIESGO_CERT.INCLUIR(p_nIdePol, rDir.NumCert, rDir.Idedirec);
        END IF;

      END IF;

    ELSIF cStsCert IN('ACT','MOD') THEN

      IF rDir.IdeDirec IS NOT NULL THEN
       --valido si existe
        BEGIN
          SELECT COUNT(1)
            INTO nExiste
            FROM DIREC_RIESGO_CERT DRC
           WHERE DRC.IdeDirec = rDir.IdeDirec;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            nExiste := 0;
        END;
        IF nExiste = 1 THEN
          -- Revierte direcciones de riesgo
          dbms_output.put_line('certif Anterior 1: ' || p_nNumCert);

          IF rDir.NumCert <> p_nNumCert THEN

            dbms_output.put_line('certif 1 : ' || rDir.NumCert);
            p_nNumCert := rDir.NumCert;
            PR_BIEN_CERT_UTIL.REVIERTE_DIREC_RIESGO(p_nIdePol, rDir.NumCert); -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II

          END IF;
          --
        END IF;
        PR_BIEN_CERT_UTIL.CARGA_DIREC_RIESGO(rDir); -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II

        -- < I RTC-314175> Jorge Benel / 31-08-2022 / Actualiza datos de la tabla DIREC_RIESGO_CERT

        UPDATE DIREC_RIESGO_CERT
         SET    CodPais      = p_cCodPais,
         CodEstado    = p_cCodEstado,
         CodCiudad    = p_cCodCiudad,
         CodMunicipio = p_cCodMunicipio,
         CodManzana   = p_cManzana,
         PorcSuma     = p_nPorcSuma,
         IdeGiro      = p_nIdeGiro,
         MtoValDeclarDirec = p_nMtoValDeclarDirec,
         AnoConstrucc = p_cAnoConstrucc,
         NumPisos     = p_cNumPisos,
         NumSotanos   = p_cNumSotanos,
         FecInspeccion = p_cFecInspeccion,
         ubigeo       = p_cubigeo,
         tipovia      = p_ctipovia,
         nomvia      = p_cnomvia,
         numvia      = p_cnumvia,
         tipoint      = p_ctipoint,
         numint      = p_cnumint,
         manzana      = p_cmanzana,
         lote      = p_clote,
         kilome      = p_ckilome,
         tipocha      = p_ctipocha,
         nomcha      = p_cnomcha,
         tiposdi      = p_ctiposdi,
         nomsdi      = p_cnomsdi,
         resto      = p_cresto,
         referencia      = p_creferencia,
         tee  = p_cTee ,
         tep        = p_cTep ,
         IndInspeccion = p_cIndInspeccion,
         IndRiesgoIns = p_cIndRiesgoIns,
         NOTAINSPECCION = p_cNOTAINSPECCION,
         INDSININSPECCION = p_cINDSININSPECCION,
         USUAUTORIZADOR = p_cUSUAUTORIZADOR
         ,PROPIEEDIFICACION = p_cPropieedificacion --<RTC-315519> David Yupanqui / 08-11-2022 / Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
        WHERE  IdeDirec = rDir.Idedirec;
        -- < F RTC-314175> Jorge Benel / 31-08-2022

      ELSE
        rDir.Idedirec := PR_BIEN_CERT_UTIL.GENERA_DIRECRIE(rDir);
        -- incluir
        PR_DIREC_RIESGO_CERT.INCLUIR(p_nIdePol, rDir.NumCert, rDir.Idedirec);
      END IF;
    END IF;

    PR_DIREC_RIESGO_CERT.ACTUALIZA_USOSBS_TREC(p_nIdePol,rDir.NumCert); --<N3040468> Noema Paredes / 13-02-2013 / Se agreg� validaciones para ramo TREC - RQ: REA-01917

    p_nNumCert := rDir.NumCert;

    RETURN rDir.Idedirec;

  END GRABA_LINEA_DIR_MASI2;
/*-----------------------------------------------------------------------
   Nombre     : MODICAR_VALORES_DECLARADOS
   Proposito  : Endoso de modificaci�n de valores declarados
   Referencia : Formulario MANTCERT.fmb

        :C04_1.BT_IMPORTA

   Parametro  :
        p_nIdePol              Id de la poliza
        p_cCODPROD             Codigo de producto
        p_cCodMoneda           Codigo de moneda
        P_TDU_TABLA_IMP_LOCAL  Tabla de locales a cargar

   Retorno :       p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                        0,OK
                        1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   24/06/2022   Jorge Benel         <RTC 314175> Creaci�n
   06/09/2022   Jorge Benel         <RTC 314175> Actualizaci�n de l�gica
   08/11/2022   David Yupanqui      <RTC-315519>  Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
   14/11/2022   Jorge Benel         <RTC 314175> Carga los locales que no se modifican
   23-11-2022   Jorge Benel         <RTC 314185> Actualiza campos
   25-11-2022   Jorge Benel         <RTC 314185> Actualiza logica para exclusi�n de locales.
   23/01/2023   Christian Bayona    <RTC-316106> Modificaci�n de factor para calcular porcentaje de distribuci�n
   03/02/2023   Christian Bayona    <RTC-316106> Modificaci�n se actualizaba m�s de una vez los bienes
   21/09/2023    Manuel Navarro     <DIGB2B03-2815> Inclusi�n Locales Legacy Dubai Masivo
-----------------------------------------------------------------------------*/
PROCEDURE MODICAR_VALORES_DECLARADOS ( p_nIDEPOL              IN POLIZA.IDEPOL%TYPE,
                                     p_cCODPROD             IN POLIZA.CODPROD%TYPE,
                                     p_cCodMoneda           IN POLIZA.CODMONEDA%TYPE,
                                     P_TDU_TABLA_IMP_LOCAL  IN TDU_TABLA_IMP_LOCAL ,
                                     p_tTABLA_BIENESXCERT     IN ACSELX.TDU_T_TABLA_BIENESXCERT,   --<RTC 314175>  Jorge Benel 06-09-2022
                                     p_tabla_bien_cert_riesgo IN ACSELX.tdu_tabla_bien_cert_riesgo , -- <RTC 314175> Jorge Benel  06-09-2022
                                     -- Variables de retorno
                                     p_cError               OUT varchar2) AS
  cRuta VARCHAR2(4000);
  nCant NUMBER(2);
  nVal  NUMBER(1);

FUNCTION FR_RECEPCIONAR_ARCHIVO RETURN NUMBER IS

  cRuta              VARCHAR2(1000);
  cFileok            VARCHAR2(1) := 'S';
  nNumLinea          NUMBER;
  nNumBien           NUMBER;
  bVal               BOOLEAN;
  nPos               NUMBER(10);
  nPosIni            NUMBER(10);
  nPosIniAnt         NUMBER(10);
  nPosFin            NUMBER(10);
  nMonto             NUMBER(38, 20);
  nMtoTotal          NUMBER(38, 20);
  cCad               VARCHAR2(4000);
  cClaseTipo         VARCHAR2(7);
  cMensaje           VARCHAR2(4000);
  nIdeDirec          DIREC_RIESGO_CERT.IDEDIREC%TYPE;
  nIdeDirec_aux      Direc_Riesgo_Cert.IdeDirec%TYPE; --<RTC 316106> / CBayona / 03-02-2023 / Nueva variable
  cClaseBien         BIEN_CERT.CLASEBIEN%TYPE;
  cCodBien           BIEN_CERT.CODBIEN%TYPE;
  cIndBien           RAMO_PLAN_PROD.INDBEN%TYPE;
  nNumcert           CERTIFICADO.NumCert%TYPE;
  nNumcertOld        CERTIFICADO.NumCert%TYPE;
  cStsCert           CERTIFICADO.StsCert%TYPE;
  nNumcertSig        CERTIFICADO.NumCert%TYPE;
  nMtoValDeclarDirec DIREC_RIESGO_CERT.MtoValDeclarDirec%TYPE;
  nAlerta            NUMBER;
  --
  rdirec            DIREC_RIESGO_CERT%ROWTYPE;
  dFecIniVig        CERTIFICADO.FecIng%TYPE;
  dFecFinVig        CERTIFICADO.FecFin%TYPE;
  cIndBienValDeclar VARCHAR2(1);
  --<I RTC 314175>  Jorge Benel 14-11-2022
  Aux_TDU_Tabla_Imp_Local   acselx.TDU_TABLA_IMP_LOCAL;
  i                 INTEGER;
  c                 INTEGER;
  nCuentaLoc2       INTEGER;
  nExiste           INTEGER;
  nCuentaLoc        INTEGER;
  nMTotal           DIREC_RIESGO_CERT.MTOVALDECLARDIREC%TYPE;
  nPorcLoc          DIREC_RIESGO_CERT.PORCSUMA%TYPE;
  nPorcLocAcu       DIREC_RIESGO_CERT.PORCSUMA%TYPE;
  nDif              DIREC_RIESGO_CERT.PORCSUMA%TYPE;
  -- <F jbenel 12-11-2022>


  CURSOR c_Cert(p_nNumcert NUMBER) IS
    SELECT NumCert, StsCert
      FROM CERTIFICADO
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumcert
       AND StsCert NOT IN ('ACT', 'ANU', 'EXC', 'NRE', 'NSR', 'REN');

  CURSOR c_Ramos IS
    SELECT CodPlan,
           RevPlan,
           CodRamoCert,
           FecIniValid,
           FecFinValid,
           StsCertRamo
      FROM CERT_RAMO
     WHERE IdePol = p_nIDEPOL
       AND NumCert = nNumcert
     ORDER BY NumOrdVis;

  CURSOR c_Direcciones(p_nNumCert NUMBER) IS
    SELECT IdeDirec, IdePol, NumCert, StsDirec
      FROM DIREC_RIESGO_CERT
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumCert;

  CURSOR c_bien(p_nNumcert BIEN_CERT.NumCert%TYPE) IS
    SELECT IdeBien
      FROM BIEN_CERT
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumcert
       AND StsBien = 'VAL';

BEGIN
  nNumcert       := 0;
  p_cError := '0,OK';
  nIdeDirec_aux := 0;  --<RTC 316106> / CBayona / 03-02-2023 / Inicializar variable

    BEGIN
      nNumLinea := 0;
      nNumBien  := 0;
      nMtoTotal := 0;
      --<I RTC 314175>  Jorge Benel 14-11-2022
      BEGIN
	  nCuentaLoc := P_TDU_TABLA_IMP_LOCAL.COUNT;--<RTC100678>
      EXCEPTION
        WHEN OTHERS THEN
          nCuentaLoc := 0;
      END;
      --<F RTC 314175>  Jorge Benel 14-11-2022



      IF P_TDU_TABLA_IMP_LOCAL.COUNT > 0 THEN
        -- <I RTC 314175> Jorge Benel 14-11-2022
        i := nCuentaLoc;
        AUX_TDU_TABLA_IMP_LOCAL := P_TDU_TABLA_IMP_LOCAL;

        FOR c1 in (SELECT * FROM DIREC_RIESGO_CERT
                   WHERE idepol = p_nIdePol AND (stsdirec <> 'EXC' OR stsdirec <> 'ANU' ) ) LOOP

          BEGIN
            SELECT 1
            INTO nExiste
            FROM TABLE(P_TDU_TABLA_IMP_LOCAL)
            WHERE IDEDIREC= c1.IdeDirec
            ;
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              i := i + 1;
              AUX_TDU_TABLA_IMP_LOCAL.EXTEND(1);

              AUX_TDU_TABLA_IMP_LOCAL(i) := acselx.TDU_IMP_LOCAL(   CODPAIS =>  NULL,     CODESTADO   =>  NULL   ,  CODCIUDAD  =>   NULL ,  CODMUNICIPIO  =>  NULL   ,
                                                                          UBIGEO  =>  NULL ,    TIPOVIA     => NULL    ,  NOMVIA     => NULL   ,  NUMVIA        => NULL    ,
                                                                          TIPOINT => NULL  ,    NUMINT      => NULL    ,  MANZANA    => NULL   ,  LOTE          => NULL    ,
                                                                          KILOME  => NULL ,     TIPOCHA     => NULL    ,  NOMCHA     => NULL   ,  TIPOSDI       => NULL    ,
                                                                          NOMSDI  => NULL ,     REFERENCIA  => NULL    ,  RESTO      => NULL   ,  PORCSUMA      => NULL    ,
                                                                          IDEGIRO => NULL,      MTOVALDECLARDIREC => NULL   ,       TEP            => NULL  ,       TEE            => NULL   ,
                                                                          ANOCONSTRUCC   => NULL   ,    NUMPISOS  => NULL   ,       NUMSOTANOS     => NULL      ,   FECINSPECCION  => NULL   ,
                                                                          NUMCERT         => NULL ,     IDEDIREC  => NULL   ,       INDRIESGOINS   => NULL,         INDINSPECCION  => NULL   ,
                                                                          NOTAINSPECCION  => NULL   ,   INDSININSPECCION => NULL  , USUAUTORIZADOR => NULL   ,      CLASETIPO      => NULL   ,
                                                                          PROPIEEDIFICACION  => NULL
                                                                          --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
                                                                          ,USOINMUEBLE => NULL,
                                                                          edificio => NULL,  edificOficina => NULL, edificotro1 => NULL, edificotro2 => NULL, edificotro3 => NULL,
                                                                          contExistencias => NULL, contMaquinFija => NULL, contMaqMovilAcces => NULL, contEquipoElec => NULL, contMobiliarioGral => NULL,
                                                                          contOtro1 => NULL, contOtro2 => NULL, contOtro3 => NULL, contOtro4 => NULL, contOtro5 => NULL, contOtro6 => NULL,
                                                                          benefBruto => NULL,
                                                                          gastosEstable => NULL
                                                                          ) ;
                                                                          --<F DIGB2B03-2815>


              AUX_TDU_TABLA_IMP_LOCAL(i).CODPAIS    := c1.CODPAIS  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).CODESTADO   := c1.CODESTADO  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).CODCIUDAD   := c1.CODCIUDAD ;
              AUX_TDU_TABLA_IMP_LOCAL(i).CODMUNICIPIO  := c1.CODMUNICIPIO   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).UBIGEO := c1.UBIGEO ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOVIA  := c1.TIPOVIA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOMVIA   := c1.NOMVIA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMVIA   := c1.NUMVIA;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOINT  := c1.TIPOINT ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMINT   := c1.NUMINT ;
              AUX_TDU_TABLA_IMP_LOCAL(i).MANZANA  := c1.MANZANA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).LOTE     := c1.LOTE ;
              AUX_TDU_TABLA_IMP_LOCAL(i).KILOME   := c1.KILOME ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOCHA  := c1.TIPOCHA ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOMCHA   := c1.NOMCHA   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TIPOSDI  := c1.TIPOSDI  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOMSDI   := c1.NOMSDI;
              AUX_TDU_TABLA_IMP_LOCAL(i).REFERENCIA  := c1.REFERENCIA   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).RESTO       := c1.RESTO ;
              AUX_TDU_TABLA_IMP_LOCAL(i).PORCSUMA    := c1.PORCSUMA       ;
              AUX_TDU_TABLA_IMP_LOCAL(i).IDEGIRO     := c1.IDEGIRO ;
              AUX_TDU_TABLA_IMP_LOCAL(i).MTOVALDECLARDIREC := c1.MTOVALDECLARDIREC    ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TEP   := c1.TEP   ;
              AUX_TDU_TABLA_IMP_LOCAL(i).TEE := c1.TEE      ;
              AUX_TDU_TABLA_IMP_LOCAL(i).ANOCONSTRUCC   := c1.ANOCONSTRUCC    ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMPISOS          := c1.NUMPISOS      ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMSOTANOS        := c1.NUMSOTANOS     ;
              AUX_TDU_TABLA_IMP_LOCAL(i).FECINSPECCION  := c1.FECINSPECCION      ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NUMCERT       := c1.NUMCERT  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).IDEDIREC      := c1.IDEDIREC  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).INDRIESGOINS   := c1.INDRIESGOINS ;
              AUX_TDU_TABLA_IMP_LOCAL(i).INDINSPECCION   := c1.INDINSPECCION ;
              AUX_TDU_TABLA_IMP_LOCAL(i).NOTAINSPECCION := c1.NOTAINSPECCION  ;
              AUX_TDU_TABLA_IMP_LOCAL(i).INDSININSPECCION  := c1.INDSININSPECCION ;
              AUX_TDU_TABLA_IMP_LOCAL(i).USUAUTORIZADOR    := c1.USUAUTORIZADOR ;
              AUX_TDU_TABLA_IMP_LOCAL(i).PROPIEEDIFICACION    := c1.PROPIEEDIFICACION ;

              --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
              AUX_TDU_Tabla_Imp_Local(i).USOINMUEBLE := c1.USOINMUEBLE;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificio := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificOficina := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro1 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro2 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro3 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contExistencias := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMaquinFija := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMaqMovilAcces := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contEquipoElec := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMobiliarioGral := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro1 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro2 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro3 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro4 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro5 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro6 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).benefBruto := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).gastosEstable := NULL;
              --<F DIGB2B03-2815> 

           END;

        END LOOP;

        BEGIN
          BEGIN
          SELECT SUM(MTOVALDECLARDIREC)
          INTO nMTotal
          FROM TABLE(AUX_TDU_TABLA_IMP_LOCAL)
          ;
          EXCEPTION
            WHEN OTHERS THEN
                nMTotal:= 1.00;
          END;

        END;


        nPorcLocAcu := 0;
        c           := 0;

        BEGIN
           nCuentaLoc2 := AUX_TDU_TABLA_IMP_LOCAL.COUNT;
        EXCEPTION
          WHEN OTHERS THEN
            nCuentaLoc2 := 0;
        END;

        -- <F RTC 314175> 14-11-2022

        -- FOR i in P_TDU_TABLA_IMP_LOCAL.First .. P_TDU_TABLA_IMP_LOCAL.Last LOOP -- <RTC 314175> Jorge Benel 14-11-2022
        FOR i in AUX_TDU_TABLA_IMP_LOCAL.First .. AUX_TDU_TABLA_IMP_LOCAL.Last LOOP
            ------------------------------------------
            -- PASO 1 - generar direccion de riesgo --
            ------------------------------------------

            -- <I RTC 314175> Jorge Benel 14-11-2022
            c := c + 1;

            SELECT (AUX_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec / nMTOTAL) * 100
            INTO nPorcLoc
            FROM DUAL;

            nPorcLocAcu := nPorcLocAcu + nPorcLoc;

            IF nCuentaLoc2 = c THEN
              nDif := 100 - nPorcLocAcu;
              IF nDif <> 0 THEN
                nPorcLoc := nPorcLoc + nDif;
              END IF;
            END IF;

            -- <F RTC 314175> Jorge Benel 14-11-2022

            BEGIN
              -- <I RTC 314175> Jorge Benel 14-11-2022
              /*
              nIdeDirec :=   GRABA_LINEA_DIR_MASI2( p_nIDEPOL,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codpais ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codestado,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codciudad,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codmunicipio ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).ubigeo ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tipovia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).nomvia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numvia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tipoint ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numint ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).manzana ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).lote ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).kilome,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tipocha,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).nomcha ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tiposdi ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).nomsdi ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).referencia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).resto ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).porcsuma,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).idegiro  ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec ,

                                                                        P_TDU_TABLA_IMP_LOCAL(i).tep ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tee ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).Anoconstrucc ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numpisos     ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numsotanos   ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).fecinspeccion ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).NumCert      ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).IdeDirec     ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).IndRiesgoIns ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).IndInspeccion ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).NOTAINSPECCION ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).INDSININSPECCION,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).USUAUTORIZADOR   ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).CLASETIPO   ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).PROPIEEDIFICACION, --<RTC-315519> David Yupanqui / 08-11-2022 / Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
                                                                        -- out
                                                                        nNumcert
                                                                       );


               */

nIdeDirec :=   GRABA_LINEA_DIR_MASI2( p_nIDEPOL,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codpais ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codestado,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codciudad,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).codmunicipio ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).ubigeo ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tipovia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).nomvia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numvia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tipoint ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numint ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).manzana ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).lote ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).kilome,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tipocha,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).nomcha ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tiposdi ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).nomsdi ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).referencia ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).resto , --  <RTC 314185> Jorge Benel 23-11-2022
                                                                        nPorcLoc, -- <RTC 314175> Jorge Benel 12-11-2022
                                                                        -- AUX_TDU_TABLA_IMP_LOCAL(i).porcsuma, --  <RTC 314185> Jorge Benel 23-11-2022
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).idegiro  ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec ,

                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tep ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).tee ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).Anoconstrucc ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numpisos     ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).numsotanos   ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).fecinspeccion ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).NumCert      ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).IdeDirec     ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).IndRiesgoIns ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).IndInspeccion ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).NOTAINSPECCION ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).INDSININSPECCION,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).USUAUTORIZADOR   ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).CLASETIPO   ,
                                                                        AUX_TDU_TABLA_IMP_LOCAL(i).PROPIEEDIFICACION,

                                                                        -- out
                                                                        nNumcert,
                                                                        p_cError --<RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
                                                                       );
             -- <F RTC 314175> Jorge Benel 14-11-2022

             --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
             IF p_cError != '0,OK' THEN
               RETURN 0;
             END IF;
             --<F RTC-316106> / CBayona / 23-01-2023

            EXCEPTION
              WHEN OTHERS THEN

                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Direcciones : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;
                RETURN 0;
            END;

            ---------------------------
            -- PASO 2 generar bienes --
            ---------------------------
            dbms_output.put_line('nIdeDirec = '||nIdeDirec);

            BEGIN
              IF nIdeDirec > 0 THEN
                --validar estado del certificado
                BEGIN
                  SELECT StsCert, FecIng, FecFin
                    INTO cStsCert, dFecIniVig, dFecFinVig
                    FROM CERTIFICADO
                   WHERE IdePol = p_nIDEPOL
                     AND NumCert = nNumcert;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    cStsCert := '';
                END;

                 dbms_output.put_line('cStsCert = '||cStsCert);

                IF cStsCert IN ('VAL', 'INC') THEN
                  -- PASO 2.1  eliminar los bienes por cada direccion
                  BEGIN
                    PR_BIEN_CERT_UTIL.ELIMINAR_BIEN_RIESGO(nIdeDirec);
                  EXCEPTION
                    WHEN OTHERS THEN

                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Eliminar las Direcciones Riesgo : ' ||cMensaje ;
                      p_cError := '1, '||cMensaje;

                      RETURN 0;

                  END;
                END IF;

                --<I RTC 314175>  Jorge Benel 06-09-2022 -- Se comenta la eliminaci�n de locales
                /*

                IF cStsCert IN ('MOD') THEN -- NUEVO CONSIDERAR LA ELIMINACION DE TODAS LAS DIRECCIONES ASOCIADAS A LOS BIENES

                  -- PASO 2.2  eliminar los bienes por cada direccion
                  BEGIN
                    PR_BIEN_CERT_UTIL.ELIMINAR_BIEN_RIESGO(nIdeDirec);
                  EXCEPTION
                    WHEN OTHERS THEN

                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Eliminar las Direcciones Riesgo : ' || cMensaje ;
                      p_cError := '1, '||cMensaje;

                      RETURN 0;
                  END;
                END IF;
                */
                -- <F RTC 314175>  Jorge Benel 06-09-2022



                FOR X IN c_Ramos LOOP
                  BEGIN
                    SELECT IndBien
                      INTO cIndBien
                      FROM RAMO_PLAN_PROD
                     WHERE CodProd = p_cCODPROD
                       AND CodPlan = X.CodPlan
                       AND RevPlan = X.RevPlan
                       AND CodRamoPlan = X.CodRamoCert
                     ORDER BY NumOrdImp;
                  EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                      cIndBien := 'N';
                  END;

                  -- <I RTC 314175>  Jorge Benel 06-09-2022  Actualizaci�n e logica
/*
                  IF cIndBien = 'S' THEN
                    --nPosIniAnt := nPosIni;
                    nMtoTotal  := 0;
                    cClaseBien := '';
                    cCodBien   := '';

                    cClaseTipo := P_TDU_TABLA_IMP_LOCAL(i).CLASETIPO;
                    nMonto     := NVL( P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec ,0); -- <RTC 314175 08-07-2022>
                    nMtoTotal  := nMtoTotal + nMonto;

                    IF cClaseTipo IS NOT NULL AND nMonto > 0 THEN
                      cClaseBien := SUBSTR(cClaseTipo, 1, 3);
                      cCodBien   := SUBSTR(cClaseTipo, 4, 4);
                      BEGIN
                        -- PASO 2.3 GENERAR BIENES
                        GENERA_BIEN_MASI2(p_nIDEPOL,
                                          nNumcert,
                                          p_cCODPROD,
                                          X.CodRamoCert,
                                          X.CodPlan,
                                          X.RevPlan,
                                          nIdeDirec,
                                          cClaseBien,
                                          cCodBien,
                                          nMonto,
                                          p_cCodMoneda,
                                          dFecIniVig,
                                          dFecFinVig);
                      EXCEPTION
                        WHEN OTHERS THEN

                          cMensaje := cMensaje || ' ' || SQLERRM;
                          cMensaje := ' al Procesar la Carga de Bienes : ' ||cMensaje ;
                          p_cError := '1, '||cMensaje;
                          RETURN 0;

                      END;
                    END IF;
                      nNumBien := nNumBien + 1;

                  END IF; -- cIndBien = 'S'

                  */

                    IF cIndBien = 'S' THEN
                       nMtoTotal  := 0;
                      IF p_tTABLA_BIENESXCERT.COUNT > 0 THEN
                        FOR i in p_tTABLA_BIENESXCERT.First .. p_tTABLA_BIENESXCERT.Last LOOP

                          cClaseBien := p_tTABLA_BIENESXCERT(i).ClaseBien;
                          cCodBien   := p_tTABLA_BIENESXCERT(i).CodBien;
                          nMonto     := NVL( p_tTABLA_BIENESXCERT(i).MtoValDeclarbien ,0); -- / 100 JBENEL
                          nMtoTotal  := nMtoTotal + nMonto;

                          BEGIN

                            -- PASO 2.3 GENERAR BIENES
                            --- PR_BIEN_CERT_UTIL. -- jbenel se comenta para hacerlo local

                            IF nIdeDirec_aux = 0 THEN  --<RTC 316106> / CBayona / 03-02-2023 / Condici�n de quiebre
                              GENERA_BIEN_MASI2(p_nIDEPOL,
                                                nNumcert,
                                                p_cCODPROD,
                                                X.CodRamoCert,
                                                X.CodPlan,
                                                X.RevPlan,
                                                nIdeDirec,
                                                cClaseBien,
                                                cCodBien,
                                                nMonto,
                                                p_cCodMoneda,
                                                p_tTABLA_BIENESXCERT(i).Fecinivalid,
                                                p_tTABLA_BIENESXCERT(i).Fecfinvalid);
                            END IF;  --<RTC 316106> / CBayona / 03-02-2023 / Fin condici�n

                          EXCEPTION
                            WHEN OTHERS THEN

                              cMensaje := cMensaje || ' ' || SQLERRM;
                              cMensaje := ' al Procesar la Carga de Bienes : ' ||cMensaje ;
                              p_cError := '1, '||cMensaje;
                              RETURN 0;

                          END;

                        nNumBien := nNumBien + 1;
                      END LOOP; --
                    END IF;  -- p_tTABLA_BIENESXCERT.COUNT
                  END IF; -- cIndBien = 'S'

                END LOOP; -- X -- c_Ramos
                nIdeDirec_aux := nIdeDirec;  --<RTC 316106> / CBayona / 03-02-2023 / Asignar valor a var quiebre

                ---- <I RTC-314175> Jorge Benel / 12-09-2022 / implementa logica de actualizaci�n

                nMtoTotal  := 0;
                IF p_tabla_bien_cert_riesgo.COUNT > 0 THEN
                  FOR i in p_tabla_bien_cert_riesgo.First .. p_tabla_bien_cert_riesgo.Last LOOP

                     nMtoTotal     := nMtoTotal + NVL( p_tabla_bien_cert_riesgo(i).mtovaldeclarbien,0);

                      UPDATE   BIEN_CERT_RIESGO
                      SET MTOVALDECLARBIENDIREC = p_tabla_bien_cert_riesgo(i).mtovaldeclarbien ,
                         PORCRIESGO = 0
                      WHERE IDEDIREC = p_tabla_bien_cert_riesgo(i).IDEDIREC
                      AND IDEBIEN = TO_NUMBER(p_tabla_bien_cert_riesgo(i).TXTADICIONAL)
                      ;

                  END LOOP;

                  BEGIN
                    FOR X IN(  SELECT IDEBIEN, COUNT(1) cuenta , SUM(MTOVALDECLARBIENDIREC)  montototal
                              FROM BIEN_CERT_RIESGO
                              WHERE IDEBIEN IN (SELECT IDEBIEN
                                                FROM BIEN_CERT
                                                WHERE IDEPOL = p_nIDEPOL)
                              GROUP BY IDEBIEN
                               ) LOOP


                        UPDATE   BIEN_CERT_RIESGO
                        -- SET PORCRIESGO = (MTOVALDECLARBIENDIREC / x.montototal ) *1000 -- <RTC 314185> Jorge Benel 25-11-2022 se comenta
                        SET PORCRIESGO = DECODE(x.montototal,0,0,  (MTOVALDECLARBIENDIREC / x.montototal ) ) * 100 -- <RTC 314185> Jorge Benel 25-11-2022 --<RTC-316106> / CBayona / 23-01-2023 / Se cambia % de 1000 a 100
                        WHERE IDEBIEN = X.IDEBIEN
                        ;
                    END LOOP;
                  END;

                END IF;

                -- <F RTC-314175>

                nNumLinea := nNumLinea + 1;

            END IF; -- IF nIdeDirec > 0 THEN

           EXCEPTION
              WHEN OTHERS THEN

                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Bienes : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;

                RETURN 0;
            END;
         END LOOP;
       END IF;

    END ;

    RETURN 0;

EXCEPTION
  WHEN OTHERS THEN
    cMensaje := cMensaje || ' ' || SQLERRM;
     p_cError := '1, '||cMensaje;
    RETURN 0;
END FR_RECEPCIONAR_ARCHIVO;

BEGIN

  nVal :=FR_RECEPCIONAR_ARCHIVO;

  dbms_output.put_line('nVal = '|| nVal);

END MODICAR_VALORES_DECLARADOS;

PROCEDURE ACT_DATOS_PARTICULARES_VALDEC (  p_nIdePol               POLIZA.IdePol%TYPE,
                                           p_nNumCert              CERT_RAMO.NumCert%TYPE,
                                           p_cCodRamoCert          CERT_RAMO.CodRamoCert%TYPE,
                                           --<I RTC-316106> / CBayona / 23-01-2023 / Nuevos par�metros
                                           p_dFecIniVig            DATE,
                                           p_dFecFinVig            DATE,
                                           --<F RTC-316106> / CBayona / 23-01-2023
                                           p_TABLA_DATOS_PARTIC    TDU_TABLA_DATOS_PARTIC,
                                           p_cError                OUT varchar2 ) AS

  v_cError VARCHAR2(500);
  dfecing certificado.fecing%type;
  dfecfin certificado.fecing%type;

  cCodPlan     CERT_RAMO.CodPlan%type;
  cCodRevPlan  CERT_RAMO.RevPlan%type;

  BEGIN -- main

    p_cError := '0,OK';
    --<I RTC-316106> / CBayona / 23-01-2023 / Inicializar variables
    dFecIng := p_dFecIniVig;
    dFecFin := p_dFecFinVig;
    --
    IF dFecIng IS NULL OR dFecFin IS NULL THEN
    --<F RTC-316106> / CBayona / 23-01-2023
      BEGIN
        SELECT fecing, fecfin
        INTO dfecing, dfecfin
        FROM certificado c
        WHERE idepol   = p_nIdePol
        AND numcert  = p_nNumCert
        ;
      EXCEPTION
         WHEN OTHERS THEN
             dfecing := SYSDATE;
             dfecfin := SYSDATE;
      END;
    END IF; --<RTC-316106> / CBayona / 23-01-2023

    BEGIN

      SELECT CodPlan, RevPlan
      INTO   cCodPlan, cCodRevPlan
      FROM   CERT_RAMO
      WHERE  idepol    = p_nIdePol
      AND  numcert     = p_nNumCert
      AND  stsCertRamo = 'ACT'
      AND  codRamoCert = p_cCodRamoCert
      ;
    EXCEPTION
      WHEN OTHERS THEN
         cCodPlan    := '';
         cCodRevPlan := '';
    END;

    pr_interfase_ax.FR_INVOCAR_DATOS_PARTICULARES(
        p_nIdePol               => p_nIdePol,
        p_nNumCert              => p_nNumCert,
        p_cCodRamoCert          => p_cCodRamoCert,
        p_cCodPlan              => cCodPlan,
        p_cCodRevPlan           => cCodRevPlan,
        p_dFecIniVig            => dFecIng,
        p_dFecFinVig            => dFecFin,
        p_TABLA_DATOS_PARTIC    => p_TABLA_DATOS_PARTIC,
        p_cError                => v_cError
      );

     p_cError := v_cError;

    dbms_output.put_line('p_cError (datos particulares) -> '||p_cError);
  EXCEPTION
    WHEN OTHERS THEN
         p_cError := '1,'||SQLERRM;
  END ACT_DATOS_PARTICULARES_VALDEC;

--<F RTC 314175 >

/*-----------------------------------------------------------------------------
  Nombre      : VALIDA_CONTRATANTE2
  Proposito   : Validar la existencia del contratante o titular en la tabla cliente / Verificar si el Contratante se encuentra Activo
  Referencias : PR_INTERFASE_AX.ENDOSO_CONTRATANTES
  Parametros  : p_nIdepol              Identificador de la p�liza
                p_cTipoModif           Tipo de modificacion : 'I'-Inclusion  'E'-Exclusion
                p_TABLA_POLIZA_CLIENTE Arreglo con los contratantes de la p�liza a incluir o excluir

  Retorno       p_cursorMsgValida   Cursor con los mensajes de validacion
                p_cError            Codigo de Error en caso se encuentre un error
                                    0 : OK  , 1 : Error  Log de Cambios

    Fecha        Autor                        Descripcion
    21/06/2022   Jorge Benel                  <RTC-313346 > Creacion
    20/07/2022   Jorge Benel                  <RTC-313346 > Actualizaci�n con logica de exclusion de contratante

  ----------------------------------------------------------------------------*/
  PROCEDURE VALIDA_CONTRATANTE2 ( p_nIdePol              IN poliza.idepol%type,
                                  p_cTipoModif           IN varchar2,
                                  p_TABLA_POLIZA_CLIENTE IN TDU_TABLA_POLIZA_CLIENTE,
                                  --
                                  p_cursorMsgValida      OUT sys_refcursor ,
                                  p_cError               OUT varchar2 )  IS


    nExiste         Number(1):= 0;
    cStsPolCli      POLIZA_CLIENTE.STSPOLCLI%TYPE;
    cCodCli         POLIZA.CODCLI%TYPE;

  BEGIN
    p_cError := '0,OK';

    FOR i in 1..p_TABLA_POLIZA_CLIENTE.Count  LOOP
      cCodCli := PR_ADA_EMI_QRY.EQUIV_NUMID_CLIENTE(p_TABLA_POLIZA_CLIENTE(i).NumIdCliente);

      BEGIN
         SELECT STSPOLCLI
         INTO cStsPolCli
         FROM POLIZA_CLIENTE
         WHERE IDEPOL = p_nIdepol
          AND  CODCLI = cCodCli;
      EXCEPTION
         WHEN OTHERS THEN
            cStsPolCli := 'INC'; -- considerarlo como INC para los casos que no exista
      END;

      IF p_cTipoModif = 'I'  THEN
          IF cStsPolCli = 'ACT' THEN
            DBMS_OUTPUT.PUT_LINE('**');
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('VALIDA_CONTRATANTE', 'VALIDACION','EL CONTRATANTE '||cCodCli ||' YA SE ENCUENTRA REGISTRADO EN ESTADO ACTIVO');
            p_cError := '1,Error';

         END IF;
      -- <I RTC-313346> Jorge Benel / 20-07-2022 / incluye logica exclusi�n
      ELSIF  p_cTipoModif = 'E'  THEN
         IF cStsPolCli = 'INC' THEN
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('VALIDA_CONTRATANTE', 'VALIDACION','EL CONTRATANTE '||cCodCli ||' NO SE ENCUENTRA REGISTRADO EN LA POLIZA');
            p_cError := '1,Error';
         ELSIF  cStsPolCli = 'EXC' THEN
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('VALIDA_CONTRATANTE', 'VALIDACION','EL CONTRATANTE '||cCodCli ||' YA SE ENCUENTRA EXCLUIDO');
            p_cError := '1,Error';
         END IF;

      -- <F RTC-313346>
      END IF;

    END LOOP;
    p_cursorMsgValida := pr_interfase_ax_sas.MOSTRAR_CURSOR_MENSAJES;  -- devuelve un cursor abierto y vacio
  EXCEPTION
    WHEN OTHERS THEN
       p_cError := '1,'|| SQLERRM;
       p_cursorMsgValida := pr_interfase_ax_sas.MOSTRAR_CURSOR_MENSAJES;  -- devuelve un cursor abierto y vacio
  END VALIDA_CONTRATANTE2;

/*
 Nombre     : REVERTIR_OPERACION
 Proposito  : Asociar Corredor a poliza reemplazandolo
 Referencia : Formulario DISTREA.fmb
      PROCEDURE REVERTIR_ANULA            -- :C04.BT_ANOKULA_OPER when-Button-Pressed
      PROCEDURE REVERTIR_GEN_DAT_REVER    -- Program Unit         fr_GENERA_DATOS_REVER
      PROCEDURE REVERTIR_ANULAES          -- Program Unit         fr_OPERACIONES('035','REVER');
      PROCEDURE REVERTIR_PORCDEREMI       -- Program Unit         fr_REVERTIR_PORCDEREMI
      PROCEDURE REVERTIR_EVENTO_SAS       -- Program Unit         fr_EVENTO_SAS
      PROCEDURE REVERTIR_AUTORIZA         -- :AUTORIZA.BT_OK      When-Button-Pressed
      PROCEDURE REVERTIR_PROCESAR         -- :C06.BT_ACEPTAR      When-Button-Pressed
 Parametro  :
      p_nNumOper    Numero de Operaci�n
      p_nIdePol     Id de la poliza
      p_nNumCert    Numero de Certificado

 Retorno :    p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                      0,OK
                      1,Error

 Log de Cambios
 Fecha        Autor               Descripcion
 -------      --------            --------------
 30/06/2022   Ronald Barreda      <RTC 313344> Creaci�n.

 */
PROCEDURE REVERTIR_OPERACION(
  p_nNumOper  OPER_POL.NUMOPER%TYPE,
  p_nIdepol   POLIZA.IdePol%TYPE,
  p_nNumCert  CERT_RAMO.NumCert%TYPE,
  p_cError    OUT VARCHAR2
  ) IS
--
--
cRestricSAS     VARCHAR2(1000);
nNumPol         POLIZA.NUMPOL%TYPE;
cStsPol         POLIZA.STSPOL%TYPE;
cCodProd        POLIZA.CODPROD%TYPE;
dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
bExiste         BOOLEAN := TRUE;
nSecuencia      LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
--
rOper_Pol       OPER_POL%ROWTYPE;
nNumTramite     POLIZA_TRAMITE.NUMTRAMITE%TYPE;
cNomUsuario     USUARIO.NOMUSR%TYPE;
dHoraIni        DATE;
dHoraFin        DATE;
cSTATUSv        OPER_POL_STATUS.STATUS%TYPE;
cDESCSTATUS     LVAL.DESCRIP%TYPE;
cSTATUS         VARCHAR2(50);
cDESCRIPCION    LVAL.DESCRIP%TYPE;
--
cError          VARCHAR2(20000);
cDatos_Rever    VARCHAR2(20000);
--
--
--
--
PROCEDURE REVERTIR_GEN_DAT_REVER IS
-- Program Unit         FR_GENERA_DATOS_REVER
cCodProd       VARCHAR2(4);
nTipoOp       VARCHAR2(4);
nNumPol        NUMBER(10);
dFecIniVig     DATE;
dFecFinVig     DATE;
cIndTxt        VARCHAR2(1);
nNumMaxOper   NUMBER(14);
cIndNo         VARCHAR2(1);
nExiste        NUMBER(1);
nNumMaxOper2  OPER_POL.NumOper%TYPE;
nNroCOB       NUMBER(9);
nCantCert     NUMBER;
nCertmAX      NUMBER;
cIndDirRie   VARCHAR2(1);
cIndDirRieOP VARCHAR2(1);

CURSOR CERTIFICADO_Q IS
  SELECT C.NUMCERT,FECING,FECFIN,DESCCERT,ROWNUM NUMFILA
    FROM CERTIFICADO C ,OPER_POL OP
    WHERE C.IdePol   = OP.IdePol
      AND C.NumCert  = OP.NumCert
      AND OP.NumOper = p_nNumOper
    ORDER BY C.NumCert;

CURSOR FACTURA_Q IS
  SELECT STSFACT,CODFACT,NUMFACT,FECSTS,MTOFACTMONEDA,FECANUL
  FROM   FACTURA
  WHERE  NumOper = p_nNumOper
    AND ROWNUM <500
  ORDER BY StsFact,NumFact;

CURSOR OBLIGACION_Q IS
  SELECT STSOBLIG,NUMOBLIG,FECSTS,MTONETOOBLIGMONEDA,FECANUL
  FROM   OBLIGACION
  WHERE  NumOper = p_nNumOper;

CURSOR OPER_POL_POST_Q(p_nNumCert  OPER_POL.NUMCERT%TYPE) IS
  SELECT *
    FROM OPER_POL
   WHERE IDEPOL = rOper_Pol.IDEPOL
     AND NUMCERT = p_nNumCert
     AND NUMOPER > p_nNumOper
     AND FECANUL IS NULL
     AND NVL(IndAnul, 'N') = 'N';

FUNCTION EXISTE_DIRECCION_RIESGO(p_nNumOper OPER_POL.NUMOPER%TYPE) RETURN VARCHAR2 IS
  cValor VARCHAR2(1);
BEGIN
  BEGIN
    SELECT 'S'
      INTO cValor
      FROM DIREC_RIESGO_CERT DR, MOD_DIREC_RIESGO MD
     WHERE MD.NUMOPER = p_nNumOper
       AND DR.IDEDIREC = MD.IDEDIREC
       AND DR.STSDIREC <> 'ANU';
  EXCEPTION
    WHEN TOO_MANY_ROWS THEN
      cValor := 'S';
    WHEN OTHERS THEN
      cValor := 'N';
  END;
  RETURN(cValor);
END EXISTE_DIRECCION_RIESGO;

BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_GEN_DAT_REVER ini');
  --
  SELECT COUNT(1),MAX(C.NUMCERT)
  INTO nCantCert, nCertMax
  FROM CERTIFICADO C ,OPER_POL OP
  WHERE C.IdePol   = OP.IdePol
    AND C.NumCert  = OP.NumCert
    AND OP.NumOper = p_nNumOper;

  BEGIN
    SELECT CODPROD,NUMPOL,FECINIVIG,FECFINVIG
    INTO   cCodProd,nNumPol,dFecIniVig,dFecFinVig
    FROM   POLIZA
    WHERE  IdePol = rOper_Pol.IdePol;
  END;
  --
  cIndNo       := 'N';
  cIndDirRie   := 'N';
  cIndDirRieOP := 'N';
  -- Validando que la operaci�n a revertir no tenga Direcciones de Riesgo activas
  cIndDirRie := EXISTE_DIRECCION_RIESGO(p_nNumOper);
  -- Validando Operaciones posterirores que incluyen al(los) certificado(s)
  FOR X IN CERTIFICADO_Q LOOP
    FOR Y IN OPER_POL_POST_Q(X.NUMCERT) LOOP
      IF Y.TIPOOP <> 'ESV' THEN
        cDatos_Rever := CHR(10) || '** ADVERTENCIA ** Usted no puede revertir esta operaci�n porque posee operaciones posteriores de tipo diferente a ESV ' || CHR(10) || CHR(10);
        cIndNo := 'S';
        EXIT;
      END IF;
      -- Validando las operaciones posteriores ESV que no tengan direcciones de riesgo
      IF cIndDirRie = 'S' THEN
        cIndDirRieOP := EXISTE_DIRECCION_RIESGO(Y.NUMOPER);
        IF cIndDirRieOP = 'S' THEN
          cDatos_Rever := CHR(10) || '** ADVERTENCIA ** Usted no puede revertir esta operaci�n porque posee Direcciones de Riesgo en estado ACT al igual que sus posteriores ' || CHR(10) || CHR(10);
          cIndNo := 'S';
          EXIT;
        END IF;
      END IF;
    END LOOP;
    --
    IF cIndNo = 'S' THEN
      EXIT;
    END IF;
  END LOOP;
  --
  IF rOper_Pol.TipoOp IN( 'ANU') THEN
       cIndNo:='S';
  END IF;
  --
  IF rOper_Pol.IndAnul ='S' THEN
    cDatos_Rever:='** Operacion Anulada **  '||TO_CHAR(rOper_Pol.NumOperAnu)||'-'||TO_CHAR(rOper_Pol.FecAnul,'DD/MM/YYYY')||CHR(10)||CHR(10);
  END IF;
  --
  cDatos_Rever:=cDatos_Rever||'Operacion: '||TO_CHAR(p_nNumOper)||'-'||TO_CHAR(rOper_Pol.FecMov,'DD/MM/YYYY')||CHR(10)||'Usuario: '||cNomUsuario||' / '||'Fecha: '||TO_CHAR(dHoraIni,'DD/MM/YYYY')||CHR(10);
  cDatos_Rever:=cDatos_Rever||'Poliza:    '||cCodProd||'-'||TO_CHAR(nNumPol)||CHR(10);
  cDatos_Rever:=cDatos_Rever||'Vigencia:  '||TO_CHAR(dFecIniVig,'DD/MM/YYYY')||'-'||TO_CHAR(dFecFinVig,'DD/MM/YYYY')||CHR(10)||CHR(10);
  cIndTxt:='N';
  cDatos_Rever:=cDatos_Rever||'Facturas: '||CHR(10);
  --
  FOR X IN FACTURA_Q LOOP
      cDatos_Rever:=cDatos_Rever||' '||X.StsFact||' '||X.CodFact||'-'||TO_CHAR(X.NumFact)||' '||TO_CHAR(X.FecSts,'DD/MM/YYYY')||' '||TO_CHAR(X.MtoFactMoneda,'999,999,999.99')||' '||TO_CHAR(X.FecAnul,'DD/MM/YYYY')||CHR(10);
      cIndTxt:='S';
      IF X.StsFact IN ('COB','ANU') THEN
         cIndNo := 'S';
      END IF;
  END LOOP;
  --
  IF   cIndTxt = 'N' THEN
      cDatos_Rever:=cDatos_Rever||' No Existen Facturas '||CHR(10);
  ELSE
      IF cIndNo = 'S' THEN
         cDatos_Rever:=cDatos_Rever||CHR(10)||'** ADVERTENCIA ** Usted no puede revertir esta operacion porque posee facturas cobradas o anuladas'||CHR(10)||CHR(10);
      END IF;
  END IF;
  --
  cDatos_Rever:=cDatos_Rever||CHR(10);
  cIndTxt:='N';
  cDatos_Rever:=cDatos_Rever||'Obligaciones: '||CHR(10);
  --
  FOR X IN OBLIGACION_Q LOOP
      cDatos_Rever:=cDatos_Rever||' '||X.StsOblig||' '||TO_CHAR(X.NumOblig)||' '||TO_CHAR(X.FecSts,'DD/MM/YYYY')||' '||TO_CHAR(X.MtoNetoObligMoneda,'999,999,999.99')||' '||TO_CHAR(X.FecAnul,'DD/MM/YYYY')||CHR(10);
      cIndTxt:='S';
      --
      IF X.StsOblig NOT IN ('ACT') THEN
         cIndNo := 'S';
         EXIT;
      ELSE
         cIndNo := 'N';
      END IF;
  END LOOP;
  --
  IF   cIndTxt = 'N' THEN
       cDatos_Rever:=cDatos_Rever||' No Existen Obligaciones '||CHR(10);
  ELSE
       IF cIndNo = 'S' THEN
          cDatos_Rever:=cDatos_Rever||CHR(10)||'** ADVERTENCIA ** Usted no puede revertir esta operacion porque posee Obligaciones pagadas o anuladas'||CHR(10)||CHR(10);
       END IF;
  END IF;
  --
  cDatos_Rever:=cDatos_Rever||CHR(10);
  IF rOper_Pol.TipoOp ='ESC' THEN
       cDatos_Rever:=cDatos_Rever||'Reversi�n de Endoso de Coaseguro: '||CHR(10);
  END IF;
  cDatos_Rever:=cDatos_Rever||CHR(10);
  cDatos_Rever:=cDatos_Rever||'Certificados Afectados en la Operacion: '||CHR(10);
  cDatos_Rever:=cDatos_Rever||CHR(10);
  --
  FOR X IN CERTIFICADO_Q LOOP
    IF nCantCert >= 1000 AND X.NUMFILA >= 1000 THEN
      EXIT;
    ELSE
      IF nCantCert != X.NUMFILA THEN
        cDatos_Rever:=cDatos_Rever||TO_CHAR(X.Numcert)||',';
      ELSE
        cDatos_Rever:=cDatos_Rever||TO_CHAR(X.Numcert);
      END IF;
    END IF;
  END LOOP;
  --
  IF nCantCert >= 1000 THEN
    cDatos_Rever:=cDatos_Rever||' ... '||TO_CHAR(nCertMax);
  END IF;
  --
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_GEN_DAT_REVER fin');
END REVERTIR_GEN_DAT_REVER;
--
--
PROCEDURE REVERTIR_REVER_DOCS(
  p_nIdePol  OPER_POL.IdePol%TYPE,
  p_nNumOper OPER_POL.NumOper%TYPE,
  p_cTipoOp  OPER_POL.TipoOp%TYPE) IS
  -- Program Unit : FR_REVER_DOCS
  --
  cVersion  VARCHAR2(3);
  procname  VARCHAR2(50);
  cEs_Unif  VARCHAR2(1);
  --
  CURSOR  c_ObligAnu IS
    SELECT O.NumOblig
    FROM   OBLIGACION O
    WHERE  O.NumOper = p_nNumOper;
  --
  CURSOR C_ENDOSATARIO_CERT IS
     SELECT DISTINCT IdePol, NumCert, NumOper
     FROM   ENDOSATARIO_CERT
     WHERE  IdePol = p_nIdePol
     AND    NumOper = p_nNumOper
     AND    StsEnd IN ('ACT','ANU','EXC');
  --
  CURSOR C_ACREE_ENDCOA IS
    SELECT F.IDEFACT, A.NUMACRE
    FROM FACTURA F, ACREENCIA A
    WHERE F.IDEFACT = A.IDEFACT
    AND F.NUMOPER = p_nNumOper;
BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_REVER_DOCS inicio');
  --
  cVersion := PR.VERSION_BASE_DATOS;
  IF cVersion <= '8' THEN
    --set transaction use rollback segment RBS_BIG01;
    DBMS_OUTPUT.PUT_LINE('set transaction use rollback segment RBS_BIG01');
  END IF;
  --
  cEs_Unif := PR_POLIZA.VERIFICA_FACT_UNIFICADA(p_nIdepol);
  DBMS_OUTPUT.PUT_LINE('cEs_Unif:'||cEs_Unif);
  --
  IF NVL(cEs_Unif, 'N') = 'S' THEN
    DBMS_OUTPUT.PUT_LINE('go PR_DEC_TRANSPORTE.ANU_OPER_DECLAR_UNIF:'||p_nNumOper);
    PR_DEC_TRANSPORTE.ANU_OPER_DECLAR_UNIF(p_nNumOper);
  ELSE
    DBMS_OUTPUT.PUT_LINE('go PR_FACTURA.FACT_OPER_ANU:'||p_nNumOper);
    PR_FACTURA.FACT_OPER_ANU(p_nNumOper);
    --
    DBMS_OUTPUT.PUT_LINE('go PR_DET_ACRE_REA_COA.REVERTIR_ANULA:'||p_nNumOper);
    PR_DET_ACRE_REA_COA.REVERTIR_OPERACION(p_nNumOper);
    --
    FOR rOA IN c_ObligAnu LOOP
      DBMS_OUTPUT.PUT_LINE('go PR_REGISTRO_PRIMAS_EMI.Anular_Docs:'||rOA.NumOblig);
      PR_REGISTRO_PRIMAS_EMI.Anular_Docs('O',rOA.NumOblig,'ANULA', p_nNumOper,NULL);
    END LOOP;
  END IF;
  --
  IF p_cTipoOp = 'ESC' THEN
    DBMS_OUTPUT.PUT_LINE('go PR_GEN_REA.REVER_OPER_REA_COA:'||p_nNumOper);
    PR_GEN_REA.REVER_OPER_REA_COA(p_nNumOper);
    --
    IF PR.BUSCA_LVAL('PROVREC','RECCOA') = 'S' THEN
      DBMS_OUTPUT.PUT_LINE('go PR_FACTURA.FACT_OPER_ANU:'||p_nNumOper);
      PR_FACTURA.FACT_OPER_ANU(p_nNumOper);
      --
      DBMS_OUTPUT.PUT_LINE('go PR_DET_ACRE_REA_COA.REVERTIR_ANULA:'||p_nNumOper);
      PR_DET_ACRE_REA_COA.REVERTIR_OPERACION(p_nNumOper);
      --
      -- Revierte RegPrimaEndoso Factura
      FOR rACEND IN C_ACREE_ENDCOA LOOP
        DBMS_OUTPUT.PUT_LINE('go PR_GEN_REA_UTIL_RM.REVIERTE_REG_PRIMA_ENDOSO'||rACEND.IDEFACT);
        PR_GEN_REA_UTIL_RM.REVIERTE_REG_PRIMA_ENDOSO('A',rACEND.IDEFACT,'ANULA', p_nNumOper,rACEND.NumAcre, NULL);
      END LOOP;
      --
      FOR rOA IN c_ObligAnu LOOP
        DBMS_OUTPUT.PUT_LINE('go PR_GEN_REA_UTIL_RM.REVIERTE_REG_PRIMA_ENDOSO'||rOA.NumOblig);
        PR_GEN_REA_UTIL_RM.REVIERTE_REG_PRIMA_ENDOSO('O',rOA.NumOblig,'ANULA', p_nNumOper,NULL);
      END LOOP;
    END IF;
  END IF;
  --
  IF p_cTipoOp = 'ESV' THEN
    FOR I IN C_ENDOSATARIO_CERT LOOP
      DBMS_OUTPUT.PUT_LINE('PR_ENDOSATARIOS.Oper_Anu:'||p_nIdePol||':'||I.NumCert||':'||I.NumOper);
      PR_ENDOSATARIOS.Oper_Anu(p_nIdePol, I.NumCert, I.NumOper);
    END LOOP;
  END IF;
  --
  BEGIN
    DELETE FROM OPER_POL_ACSEL_MACSI
    WHERE  NumOper = p_nNumOper;
    --
    DBMS_OUTPUT.PUT_LINE('DELETE OPER_POL_ACSEL_MACSI:'||sql%rowcount);
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
  END;
  --
  BEGIN
    DBMS_OUTPUT.PUT_LINE('PR_RECA_DCTO_CERTIF.REVER_RECA_DCTO_CERT:'||p_nNumOper);
    PR_RECA_DCTO_CERTIF.REVER_RECA_DCTO_CERT(p_nNumOper);
  EXCEPTION
    WHEN OTHERS THEN
      cError := '1,Error en PR_RECA_DCTO_CERTIF.REVER_RECA_DCTO_CERT - '||SQLERRM;
      DBMS_OUTPUT.PUT_LINE('cError A:'||cError);
      RETURN;
  END;
  BEGIN
    IF (PR.BUSCA_LVAL('SWITCH', 'GCONTREA') = 'S') THEN
      DBMS_OUTPUT.PUT_LINE('PR_CTA_CTE_REA_PROC.Reversa_Anula_Cta_Cte:'||p_nIdePol||':'||p_nNumOper);
      PR_CTA_CTE_REA_PROC.Reversa_Anula_Cta_Cte(p_nIdePol,p_nNumOper,'REVER');
    ELSE
      DBMS_OUTPUT.PUT_LINE('PR_CTA_CTE_REA.Reversa_Anula_Cta_Cte:'||p_nIdePol||':'||p_nNumOper);
      PR_CTA_CTE_REA.Reversa_Anula_Cta_Cte(p_nIdePol,p_nNumOper,'REVER');
    END IF;
    --
    DBMS_OUTPUT.PUT_LINE('PR_GENERA_PRV.RevierteDocProvision:'||p_nIdePol||':'||p_nNumOper);
    PR_GENERA_PRV.RevierteDocProvision(p_nNumOper, p_nIdePol);
    --
  EXCEPTION
    WHEN OTHERS THEN
      cError := '1,FRM-DISTREA Error en PR_CTA_CTE_REA.Reversa_Anula_Cta_Cte - '||SQLERRM;
      DBMS_OUTPUT.PUT_LINE('cError B:'||cError);
      RETURN;
  END;
EXCEPTION
  WHEN OTHERS THEN
    cError := '1,'||SQLERRM;
    DBMS_OUTPUT.PUT_LINE('cError C:'||cError);
END REVERTIR_REVER_DOCS;
--
--
PROCEDURE REVERTIR_PORCDEREMI IS
  -- Program Unit : FR_REVERTIR_PORCDEREMI
  nExisteOpAct NUMBER;
BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_PORCDEREMI ini');
  --
  IF PR.BUSCA_LVAL('CMBDEVPR','ESTADO') = 'ACTIVADO' THEN
    DECLARE
      nIdepol      OPER_POL.IDEPOL%TYPE;
      nNumcert     OPER_POL.NUMCERT%TYPE;
      nPorcDerEmi  CONC_FINANCIAMIENTO.PORCCPTOF%TYPE;

      CURSOR OPER IS
        SELECT idepol, numcert
        FROM  OPER_POL OP
        WHERE op.numoper = p_nNumOper
        ORDER BY Idepol, Numcert;

      CURSOR RESP IS
        SELECT CodCli
        FROM RESP_PAGO_MOV B
        WHERE B.idepol  = nIdepol + 0
          AND B.Numcert = nNumcert + 0
          AND B.Numoper = p_nNumOper;

    BEGIN
      FOR O IN OPER LOOP
        nIdepol  := O.Idepol;
        nNumcert := O.Numcert;

        SELECT NVL(COUNT(*),0) INTO nExisteOpAct
        FROM  OPER_POL
        WHERE Idepol  = nIdepol + 0
          AND Numcert = nNumcert + 0
          AND Numoper <> p_nNumOper
          AND NVL(IndAnul, 'N') = 'N'
          AND FecAnul IS NULL;

        -- No existen Otras operaciones activas
        IF nExisteOpAct = 0 THEN
          FOR R IN RESP LOOP
            -- Eliminar el registro del % Gto Emision para la Poliza, Certificado, Resp.Pago
            DELETE FROM PORC_DER_EMI
            WHERE IDEPOL  = nIdepol
              AND NUMCERT = nNumcert
              AND CODCLI  = R.CodCli;
          END LOOP;
        END IF;
      END LOOP;
    END;
  END IF;
  --
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_PORCDEREMI fin');
END REVERTIR_PORCDEREMI;
--
--
FUNCTION REVERTIR_EVENTO_SAS
  RETURN NUMBER IS
  -- Program Unit : FR_EVENTO_SAS
  nError               NUMBER := 0;
  nExiste              NUMBER;
  nDoc                 CHAR(2);
  nEstado              CHAR(2);
  nMensaje             VARCHAR2(1000);
  cNumTramite      POLIZA_TRAMITE.NumTramite%TYPE ;
  cNumTramite2     POLIZA_TRAMITE.NumTramite%TYPE ;
BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_EVENTO_SAS ini');
  --
  cNumTramite := nNumTramite ;
  cNumTramite2 := NULL; --:C06_1.CNumTramite
  IF cNumTramite IS NOT NULL THEN
     ACSELX.PR_SERVICIOS_WEB.SP_VALIDA_COTIZACION_SAS(cNumTramite,nDoc,nEstado,nMensaje);
     IF nDoc = '-1' THEN
       nError := 0;
       cError := '1,NO EXISTE EL TRAMITE '||cNumTramite||' EN SAS';
     ELSE
       nError := 1;
     END IF;
  ELSIF cNumTramite2 IS NOT NULL THEN
     ACSELX.PR_SERVICIOS_WEB.SP_VALIDA_COTIZACION_SAS(cNumTramite2,nDoc,nEstado,nMensaje);
     IF nDoc = '-1' THEN
       nError := 0;
       cError := '1,NO EXISTE EL TRAMITE '||cNumTramite2||' EN SAS';
     ELSE
       nError := 1;
     END IF;
  END IF;
  --
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_EVENTO_SAS fin : ' || nError);
  --
  RETURN(nError);
EXCEPTION
    WHEN OTHERS THEN
      nError := 0;
      DBMS_OUTPUT.PUT_LINE('cError D:'||SQLERRM);
      RETURN(nError);
END REVERTIR_EVENTO_SAS;
--
--
PROCEDURE REVERTIR_ANULAES(
  c_TipoAut  VARCHAR2,
  c_TipoOper VARCHAR2
) IS
-- Program Unit         FR_OPERACIONES('035','REVER');
  dummy       NUMBER;
  ret_button  NUMBER;
  cPassWord   VARCHAR2(1300);
  cTipoOrd    VARCHAR2(1);
  nNumOper    NUMBER(14);
  nExiste     NUMBER(1);
  nPasa       NUMBER := 1;
  cVersion    VARCHAR2(3);
  procname    VARCHAR2(50);
  cCodProd    POLIZA.CODPROD%TYPE;
  cMessage    VARCHAR2(50);
  bOK         BOOLEAN := TRUE;
BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_ANULAES ini');
  --
  BEGIN
    IF c_TipoOper ='REVER' THEN
      /*
      Se guardara un registro historico del proceso que va a ejecutar - Si no esta seguro del proceso por favor consulte'
      --
      Reversar OPERACION Este proceso anulara las facturas/Obligaciones relacionas a la Operaci�n anulada y modifica la poliza
      */
      IF bOK THEN --ret_button = ALERT_BUTTON1
        REVERTIR_REVER_DOCS(rOper_Pol.Idepol,p_nNumOper,rOper_Pol.TipoOp); -- FR_REVER_DOCS
        IF nNumTramite IS NOT NULL THEN
          BEGIN
            PR_Poliza_Tramite.RevertirOperacion(rOper_Pol.IdePol,nNumTramite,p_nNumOper);
            BEGIN
              DELETE LOG_CCM_TRAMITES WHERE NumTramite = nNumTramite AND NumOper = p_nNumOper;
            END;
          EXCEPTION
            WHEN OTHERS THEN
              cError := '1,FRM-DISTREA Error en PASSEMI. Actualizacion en Poliza_Tramite ''REV'' '||SQLERRM;
              DBMS_OUTPUT.PUT_LINE('cError F:'||cError);
              RETURN;
          END;
          BEGIN
            SELECT CODPROD
            INTO   cCodProd
            FROM   POLIZA
            WHERE  IDEPOL = rOper_Pol.Idepol;
          END ;
          --
          IF PR.EXISTE_LVAL('COTPROD',cCodProd) = 'S' THEN
            nPasa := REVERTIR_EVENTO_SAS; --FR_EVENTO_SAS;
            cMessage := 'SAS.';
          ELSE
            IF PR.EXISTE_LVAL('CCMPRODX',cCodProd) = 'N' THEN
              IF PR.EXISTE_LVAL('CCMLVGRL','SWICTHCCM') = 'N' THEN
                 --nPasa := FR_Evento_Notes;
                 cMessage := 'NOTES.';
                 DBMS_OUTPUT.PUT_LINE('FR_Evento_Notes');
              END IF;
            END IF;
          END IF;
          --
          IF nPasa = 0 THEN
            cError := '1,FRM-DISTREA Error en TRANSACCIONES '||cMessage||' ''REV'' '||SQLERRM;
            DBMS_OUTPUT.PUT_LINE('cError G:'||cError);
            RETURN;
          END IF ;
        END IF ;
        --
        REVERTIR_PORCDEREMI; --FR_Revertir_PorcDerEmi;
      END IF;
    ELSIF  c_TipoOper ='GDOCS' THEN
      cVersion := PR.VERSION_BASE_DATOS;
      IF cVersion <= '8' THEN
        --set transaction use rollback segment RBS_BIG01;
        DBMS_OUTPUT.PUT_LINE('set transaction use rollback segment RBS_BIG01;');
      END IF;
      --
      --FR_Confirmar('FACTURA',rOper_Pol.IdePol);
      DBMS_OUTPUT.PUT_LINE('<<< FR_Confirmar(''FACTURA'',rOper_Pol.IdePol) >>>');
      --GO_BLOCK('B01_1');
      --
    END IF;
    --
    DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_ANULAES fin');
    --
  EXCEPTION
    WHEN OTHERS THEN
      cError := '1,' || SQLERRM;
      DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_ANULAES error H:'||cError);
  END;
END REVERTIR_ANULAES;
--
--
PROCEDURE REVERTIR_ANULA IS
--:C04.BT_ANULA_OPER When-Button-Pressed
  nDummy      NUMBER;
  nIdeRemesa  OPER_POL_REMESA_BCO.IdeRemesa%TYPE;
  cIndProc    OPER_POL_REMESA_BCO.IndProc%TYPE;

  CURSOR c_RemesaBco IS
    SELECT IDEREMESA, INDPROC
    FROM   OPER_POL_REMESA_BCO
    WHERE  NumOper = p_nNumOper;

  p_P    POLIZA%ROWTYPE;
  kSi    CONSTANT VARCHAR2(1) := 'S';

BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_ANULA ini');
  --
  IF rOper_Pol.TipoOp = 'EXC' AND rOper_Pol.MtoOper = 0 THEN
    cError := '1,No se puede revertir una Operacion de EXCLUSION con Monto de Prima en Cero, Generar�a inconsistencia de Estatus en los Ramos del Certificado';
    DBMS_OUTPUT.PUT_LINE('cError I:'||cError);
    RETURN;
  ELSE
    nIdeRemesa := NULL;
    cIndProc   := 'N';
    --
    OPEN c_RemesaBco;
    FETCH c_RemesaBco INTO nIdeRemesa, cIndProc;
    CLOSE c_RemesaBco;
    --
    IF nIdeRemesa IS NOT NULL AND cIndProc = 'S' THEN
      cError := '1,No se puede revertir la operaci�n, pues ya se genero Remesa del Bco. Continental asociada a la misma';
      DBMS_OUTPUT.PUT_LINE('cError J:'||cError);
    ELSE
      p_P := PR_POLIZA.Datos_Poliza(rOper_Pol.IdePol);
      IF p_P.StsPol = 'NSR' THEN
          BEGIN
            PR_RENOVACION_MASIVA.Revert_No_Nunca_Renovar_Poliza(rOper_Pol.IdePol, 'RPNSR', KSi);
          EXCEPTION
            WHEN OTHERS THEN
              cError := '1,PR_RENOVACION_MASIVA.Revert_No_Nunca_Renovar_Poliza. Proceso: RPNSR '||SQLERRM;
              RETURN;
              DBMS_OUTPUT.PUT_LINE('cError K:'||cError);
          END;
      ELSIF p_P.StsPol = 'NRE' THEN
        BEGIN
          PR_RENOVACION_MASIVA.Revert_No_Nunca_Renovar_Poliza(rOper_Pol.IdePol, 'RPNRE', kSi);
        EXCEPTION
          WHEN OTHERS THEN
            cError := '1,PR_RENOVACION_MASIVA.Revert_No_Nunca_Renovar_Poliza. Proceso: RPNRE '||SQLERRM;
            RETURN;
            DBMS_OUTPUT.PUT_LINE('cError L:'||cError);
        END;
      ELSE
        REVERTIR_GEN_DAT_REVER; -- FR_Genera_Datos_Rever; -- ventana 'Datos de la Operaci�n'
      END IF;
    END IF;
  END IF;
  --
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_ANULA fin');
  --
EXCEPTION
  WHEN OTHERS THEN
    cError := '1,REVERTIR_ANULA:'||SQLERRM;
    DBMS_OUTPUT.PUT_LINE('cError M:'||cError);
END REVERTIR_ANULA;
--
--
PROCEDURE REVERTIR_AUTORIZA IS
-- :AUTORIZA.BT_OK      When-Button-Pressed
  DUMMY       NUMBER;
  cPassWord   VARCHAR2(300);
  cDescOper   OPER_USUARIO_ACCESO.DescOper%TYPE;
  cStatusv    VARCHAR2(3);
  rpst        OPER_POL_STATUS%ROWTYPE;
  vcExiste    NUMBER:=0;
  bPassword   BOOLEAN := TRUE;
BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_AUTORIZA ini');
  --
  --cPassWord:=NVL(PR_ACCESO_USUARIO.TRAE_PASSWORD('464'),'SIN PASSWORD');
  IF NOT bPassword THEN --(cPassWord <> NVL(:AUTORIZA.cPassWord,'INVALIDO'))
    BEGIN
      SELECT DescOper
      INTO cDescOper
      FROM OPER_USUARIO_ACCESO
      WHERE Codoper = '464';
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        cDescOper := 'INVALIDO';
    END;
    cError := '1,Debe ingresar correctamente la palabra clave para la operaci�n: 464 - ' || InitCap(cDescOper);
    DBMS_OUTPUT.PUT_LINE('cError M:'||cError);
    RETURN;
  ELSE
    BEGIN
      cDESCSTATUS:=' ';
      --
      SELECT PR.BUSCA_LVAL ('INDVAL', NULL) --, :C09.LS_STATUS)
      INTO   cDESCSTATUS
      FROM   DUAL;
      --
      if NULL is not null then -- :C09.LS_STATUS
        DBMS_OUTPUT.PUT_LINE('C09.LS_STATUS');
        cStatusv := NULL;      -- :C09.LS_STATUS
        cSTATUS := cStatusv;
        cDESCRIPCION := cStatusv || ' - ' || cDESCSTATUS;
        --
        BEGIN
          SELECT  1
            INTO  vcExiste
            FROM  OPER_POL_STATUS
           WHERE  Idepol  = rOper_Pol.IdePol
             AND  Numoper = p_nNumOper;
        EXCEPTION
           WHEN NO_DATA_FOUND THEN
              vcExiste:= 0;
           WHEN OTHERS THEN
              vcExiste:= 0;
        END;
        --
        IF NVL(vcExiste,0) = 1 THEN
           BEGIN
              UPDATE  OPER_POL_STATUS
                 SET  STATUS = cStatusv
               WHERE  Idepol = rOper_Pol.IdePol
                 AND  Numoper = p_nNumOper;
           EXCEPTION
              WHEN NO_DATA_FOUND THEN
                 cSTATUSv:= '0';
           END;
        ELSE
            INSERT INTO OPER_POL_STATUS (idePol, NumOper, Status, CodUsr, Fecha)
                 VALUES (rOper_Pol.IdePol, p_nNumOper, cStatusv, PR_SESION.USUARIO_APP, sysdate);
        END IF;
      end if;
    END;
  END IF;
  --
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_AUTORIZA fin');
  --
EXCEPTION
  WHEN OTHERS THEN
    cError := '1,EndosoRevertirAutoriza:' || SQLERRM;
    DBMS_OUTPUT.PUT_LINE('cError N:'||cError);
END REVERTIR_AUTORIZA;
--
--
PROCEDURE REVERTIR_PROCESAR IS
-- :C06.BT_ACEPTAR  When-Button-Pressed
  dummy             NUMBER;
  c_existe_politica NUMBER;
  cPolitica         POLIZA.IndMovPolRen%TYPE;
  dFecOper          VARCHAR2(8);
  cIndFacult        POLIZA.IndFacult%TYPE;
  cPolitica1        POLIZA.IndMovPolRen%TYPE;
  cValid            char(1):= 'N';
  LN_ExistePrv      NUMBER;
BEGIN
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_PROCESAR ini');
  --
  BEGIN
    SELECT NVL(INDFACULT,'N')
    INTO   cIndFacult
    FROM   POLIZA
    WHERE  IdePol=rOper_Pol.IdePol;
  END;
  --<I RTC-150906> Gianpiere Morales / 28-12-2018 /  Proyecto Redise�o de Procesos de Reaseguros - Pagos al exterior
        BEGIN
          SELECT 1
            INTO LN_ExistePrv
            FROM PRV_DINERO PD
           WHERE PD.IDEPRG IN (SELECT pe.IDEPRG
                                FROM PRV_DINERO_ELEM PE
                                WHERE PE.NUMDOC IN
                                (
                                   SELECT CT.NUMDOCMOV
                                   FROM CTA_CTE_REA CT
                                   WHERE CT.NUMOPER = p_nNumOper
                                   ))
             AND PD.ORIGPRV = 'REA'
             AND PD.STSPRV in ('PRO','ABO')
             AND ROWNUM = 1;
        EXCEPTIOn
          WHEN OTHERS THEN
            LN_ExistePrv := 0;
        END;

  IF LN_ExistePrv > 0 THEN
    cError := '1,No se puede revertir la operaci�n, pues tiene liquidaciones de Reaseguros con Abono en Proceso, comunicarse con el �rea de Reaseguros/Tesorer�a';
    DBMS_OUTPUT.PUT_LINE('cError O:'||cError);
    RETURN;
  END IF;

  IF cIndFacult = 'S' THEN
    BEGIN
      SELECT INDMOVPOLREN
      INTO   cPolitica
      FROM   POLIZA
      WHERE  IdePol = rOper_Pol.IdePol
        AND  CodProd = cCodProd;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        cPolitica := '0';
      WHEN TOO_MANY_ROWS THEN
        cPolitica := '0';
    END;
    IF cPolitica='2' THEN
      cPolitica:='0';
    END IF;
    --
    IF cPolitica = '0' OR cPolitica IS NULL THEN
      cPolitica1 := PR_POLITICAS.PRINCIPAL(rOper_Pol.IdePol,nNumPol,cCodProd,USER,'AC');
      --
      BEGIN
        SELECT INDMOVPOLREN
        INTO   cPolitica
        FROM   POLIZA
        WHERE  IdePol  = rOper_Pol.IdePol
          AND  CodProd = cCodProd;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            cPolitica := '0';
          WHEN TOO_MANY_ROWS THEN
            cPolitica := '0';
      END;
      IF cPolitica = '1' or cPolitica1 = '1' THEN
        cError := '1,Existen Politicas Violadas ...Verifique...';
        DBMS_OUTPUT.PUT_LINE('cError P:'||cError);
        RETURN;
      ELSE
        cValid := 'S'; --FR_Invocar_Autorizacion('035',USER);
        IF cValid = 'N' THEN
          cError := '1,Usted no posee autorizaci�n para realizar este tipo de operaciones';
          DBMS_OUTPUT.PUT_LINE('cError Q:'||cError);
          RETURN;
        ELSE
          REVERTIR_ANULAES('035','REVER'); -- FR_OPERACIONES('035','REVER');
        END IF;
      END IF;
    END IF;
  ELSIF cIndFacult='N' THEN
    cValid := 'S'; --FR_Invocar_Autorizacion('035',USER);
    IF cValid = 'N' THEN
      cError := '1,Usted no posee autorizaci�n para realizar este tipo de operaciones';
      DBMS_OUTPUT.PUT_LINE('cError R:'||cError);
      RETURN;
    ELSE
      REVERTIR_ANULAES('035','REVER'); -- FR_OPERACIONES('035','REVER');
    END IF;
  END IF;
  --
  DBMS_OUTPUT.PUT_LINE('~~~~~REVERTIR_PROCESAR fin');
  --
END REVERTIR_PROCESAR;
--
--
BEGIN
  cError := '0,OK';
  --
  SELECT  *
  INTO    rOper_Pol
  FROM    OPER_POL
  WHERE   NumOper = p_nNumOper
    AND   IdePol  = p_nIdePol
    AND   NumCert = p_nNumCert;
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',rOper_Pol.IdePol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'revertirOPERACION','',nSecuencia );
  --
  -- Validacion del estado la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( rOper_Pol.IdePol, cStsPol, cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
  IF NOT bExiste THEN cError := '1,Error en validaci�n de poliza'; RETURN; END IF;
  -- Validacion del estado de la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
  IF NOT bExiste THEN cError := '1,Error en validaci�n de estado de poliza'; RETURN; END IF;
  --
  cRestricSAS := pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(rOper_Pol.IdePol);
  --
  IF NOT cRestricSAS IS NULL THEN
     cError := '1,' || cRestricSAS;
     RETURN;
  END IF;
  --
  BEGIN
    SELECT NUMTRAMITE
    INTO   nNumTramite
    FROM   POLIZA_TRAMITE
    WHERE  Idepol  = rOper_Pol.IdePol
      AND  Numoper = p_nNumOper
      AND  ROWNUM < 2 ;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      nNumTramite := null;
    WHEN TOO_MANY_ROWS THEN
      nNumTramite := 'INVALIDO';
      cError := '1,Existen m�s de dos tramites para la operaci�n '|| p_nNumOper ||' en la tabla POLIZA_TRAMITE, comuniquese con Sistemas';
    WHEN OTHERS THEN
      nNumTramite := null;
      cError := '1,FRM-DISTREA Error OTHERS en Query a POLIZA_TRAMITE';
  END;
  --
  BEGIN
    SELECT USR.NOMUSR, OPE.FECHAHORAINI, OPE.FECHAHORAFIN
    INTO   cNomUsuario, dHoraIni, dHoraFin
    FROM   OPERACION  OPE,
           USUARIO    USR
    WHERE  OPE.Ideop  = p_nNumOper
      AND  USR.CodUsr = OPE.CodUsuario;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
         cNomUsuario := 'Usuario No Existe';
  END;
  --
  BEGIN
    SELECT STATUS
    INTO   cSTATUSv
    FROM   OPER_POL_STATUS
    WHERE  Idepol  = rOper_Pol.IdePol
      AND  Numoper = p_nNumOper;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
         cSTATUSv:= '0';
  END;
  --
  cDESCSTATUS := ' ';
  SELECT PR.BUSCA_LVAL ('INDVAL', cSTATUSv)
  INTO   cDESCSTATUS
  FROM   DUAL;
  --
  DBMS_OUTPUT.PUT_LINE('go REVERTIR_ANULA!');
  REVERTIR_ANULA;
  --
  IF cError = '0,OK' THEN
      DBMS_OUTPUT.PUT_LINE('go REVERTIR_AUTORIZA!');
      REVERTIR_AUTORIZA;
      --
      IF cError = '0,OK' THEN
          DBMS_OUTPUT.PUT_LINE('go REVERTIR_PROCESAR!');
          REVERTIR_PROCESAR;
      END IF;
  END IF;
  DBMS_OUTPUT.PUT_LINE('end');
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',rOper_Pol.IdePol,NULL,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'revertirOPERACION','',nSecuencia );
  --
  p_cError := cError;
EXCEPTION
    WHEN OTHERS THEN
        p_cError := '1,'|| SQLERRM;
    PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',rOper_Pol.IdePol,NULL,NULL,'LOG','ERR' ,SYSDATE,SYSDATE,'revertirOPERACION','',nSecuencia );
END REVERTIR_OPERACION;


/*-----------------------------------------------------------------------------
    Nombre : GENERA_BIEN_MASI2
    Proposito : Estructura la informacion para la generacion de Bienes Masivo
    Referencias : PR_BIEN_CERT_UTIL.GENERA_BIEN_MASI
    Parametros :
      p_nIdePol          Identificador de Poliza
      p_nNumCert         Numero de Certificado
      p_cCodProd         Codigo de Producto
      p_cCodRamo         Codigo de ramo del certificado
      p_cCodPlan         Codigo de plan del certificado
      p_cRevPlan         Codigo de revision del certificado
      p_nIdeDirec        Identificador de la direccion de riesgo
      p_cClaseBien       Codigo de clase
      p_cCodBien         Codigo de tipo bien
      p_nMtoDeclar       Monto declarado
      p_cCodMoneda       Codigo de moneda
      p_dFecIniValid     Fecha de inicio de validez
      p_dFecFinValid     Fecha de fin de validez

    Log de Cambios
      Fecha       Autor            Descripcion
      08/07/2022  JORGE BENEL      <RTC 314175> Creacion
      06/09/2022  JORGE BENEL      <RTC 314175> Creacion
      11/10/2022  JORGE BENEL      <RTC 314175> Actualizacion tipo de cambio
  ----------------------------------------------------------------------------*/
  PROCEDURE GENERA_BIEN_MASI2 (
    p_nIdePol           POLIZA.IDEPOL%TYPE,
    p_nNumCert          CERTIFICADO.NUMCERT%TYPE,
    p_cCodProd          PRODUCTO.CODPROD%TYPE,
    p_cCodRamo          BIEN_CERT.CODRAMOCERT%TYPE,
    p_cCodPlan          PLAN_PROD.CODPLAN%TYPE,
    p_cRevPlan          PLAN_PROD.REVPLAN%TYPE,
    p_nIdeDirec         DIREC_RIESGO_CERT.IDEDIREC%TYPE,
    p_cClaseBien        BIEN_CERT.CLASEBIEN%TYPE,
    p_cCodBien          BIEN_CERT.CODBIEN%TYPE,
    p_nMtoDeclar        DIREC_RIESGO_CERT.MTOVALDECLARDIREC%TYPE,
    p_cCodMoneda        POLIZA.CODMONEDA%TYPE,
    p_dFecIniValid      BIEN_CERT.FECINIVALID%TYPE,
    p_dFecFinValid      BIEN_CERT.FECFINVALID%TYPE
  ) IS

    CURSOR DIREC_Q IS
      SELECT CodPais, CodEstado, CodCiudad, CodMunicipio, Direc, CodManzana, Zonsis, ZonaCiudad
      FROM DIREC_RIESGO_CERT
      WHERE IdePol  = p_nIdePol
        AND  NumCert= p_nNumCert
      ORDER BY PorcSuma DESC,CodEstado;

    rCur                BIEN_CERT%ROWTYPE;
    cCambio              VARCHAR2(1);
    cCodMonedaLocal     TASA_CAMBIO.CODMONEDA%TYPE;
    cIndCoaAcep         POLIZA.TIPOPDCION%TYPE;
    cIndBienValDeclar   VARCHAR2(1);
    nCant               NUMBER(1):=0;
    cIndCobert          RAMO_PLAN_PROD.INDCOBERT%TYPE;
    nIdeBien            BIEN_CERT.Idebien%TYPE;
    nNroBien            BIEN_CERT.Nrobien%TYPE;
    cIndBien            RAMO_PLAN_PROD.INDBEN%TYPE;
    nExiste             NUMBER:=0;
    cStsBien            BIEN_CERT.StsBien%TYPE;
    cStsCert            certificado.StsCert%TYPE;
    nTasaCambio         TASA_CAMBIO.TASACAMBIO%TYPE := 1; -- <RTC 314175> jbenel 11-10-2022
  BEGIN
     --validar estado del certificado
    BEGIN
      SELECT StsCert
        INTO cStsCert
        FROM CERTIFICADO
       WHERE IdePol = p_nIdePol
         AND NumCert = p_nNumCert;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100,'ERROR, CERTIFICADO: ' || p_nNumCert ||' NO EXISTE.');
    END;
    BEGIN
      SELECT IndPriPerdida, IndCobert, IndBien
      INTO    cCambio, cIndCobert, cIndBien
      FROM   RAMO_PLAN_PROD
      WHERE   CodProd    = p_cCodProd
        AND  CodRamoPlan = p_cCodRamo
        AND  CodPlan     = p_cCodPlan
        AND  RevPlan     = p_cRevPlan;
    EXCEPTION
      WHEN OTHERS THEN
        cCambio := 'N';
        cIndCobert := NULL;
        cIndBien := 'N';
    END;
    DBMS_OUTPUT.PUT_LINE('1.- IndBien '||cIndBien);
    IF NVL(cIndBien,'N') = 'N' THEN
      RETURN;
    END IF;

    cIndBienValDeclar := PR_POLIZA_INCONSISTENCIA.PROD_CON_VALOR_DECLARADO (p_nIdePol, p_cCodRamo, p_cCodPlan, p_cRevPlan, 'PRODUCTO') ;
    nExiste := 0;

    DBMS_OUTPUT.PUT_LINE('2.- cIndBien '||cIndBienValDeclar);

    IF NVL(cIndBienValDeclar,'N') != 'S' THEN
      RETURN;
    END IF;
    -- Verifica si ya existe bien_cert para el codbien leido
    BEGIN
      BEGIN
        SELECT IDEBIEN, StsBien, 1
        INTO   nIdeBien, cStsBien, nExiste
        FROM  BIEN_CERT A
        WHERE A.IDEPOL = p_nIdePol
          AND A.NUMCERT = p_nNumCert
          AND A.CODRAMOCERT = p_cCodRamo
          AND A.CLASEBIEN = p_cClaseBien
          AND A.CODBIEN = p_cCodBien;

        -- <I RTC 314175> jbenel 10-10-2022
        BEGIN
         cCodMonedaLocal:=PR_VINISUSC.CODIGO_MONEDA;
         nTasaCambio := PR.TASA_CAMBIO(p_cCodMoneda, cCodMonedaLocal,SYSDATE,'D');

        EXCEPTION
          WHEN OTHERS THEN
            RAISE_APPLICATION_ERROR(-20100, SQLERRM||'No se puee obtener tipo cambio. '||SQLERRM||CHR(10));
        END;
        -- <F RTC 314175> jbenel 10-10-2022

      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          nIdeBien := NULL;
          cStsBien := NULL;
          nExiste  := 0;
      END;

      DBMS_OUTPUT.PUT_LINE('3.- nExiste / cStsBien / nIdeBien / cStsCert : '||nExiste ||' / '|| cStsBien ||' / '||nIdeBien||' / '||cStsCert );

      IF nExiste <> 0 THEN
        IF cStsBien IN ('VAL','INC') THEN
          PR_BIEN_CERT_UTIL.INSERTA_BIEN_RIESGO(nIdeBien, p_nIdeDirec, p_nMtoDeclar);

          BEGIN
              UPDATE BIEN_CERT A
                 SET mtovalbien = mtovalbien + p_nMtoDeclar * nTasaCambio, -- <RTC 314175> jbenel 11-10-2022
                     MtoValDeclaBienAnt = MtoValDeclarBien, --<RTC-316106> / CBayona / 23-01-2023 / Guardar el VD anterior
                     mtovalbienmoneda = mtovalbienmoneda + p_nMtoDeclar,
                     mtovaldeclarbien = mtovaldeclarbien + p_nMtoDeclar
               WHERE A.IDEPOL = p_nIdePol
                 AND A.NUMCERT = p_nNumCert
                 AND A.CODRAMOCERT = p_cCodRamo
                 AND A.CLASEBIEN = p_cClaseBien
                 AND A.CODBIEN = p_cCodBien
                 AND A.Stsbien IN ('INC');
              IF SQL%FOUND THEN
                dbms_output.put_line('actualizo GENERA_BIEN_MASI numero de registros: '||SQL%ROWCOUNT );
              END IF;
            END;

          IF cStsCert IN('INC','MOD') THEN
            PR_BIEN_CERT.GENERAR_TRANSACCION(nIdeBien, 'INCLU');
          END IF;
        ELSIF cStsBien IN( 'ACT', 'MOD' ) THEN
          IF nExiste <> 0 THEN
            -- PR_BIEN_CERT_UTIL.ELIMINAR_BIEN_RIESGO(nIdeBien, p_nIdeDirec); -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II
            -- PR_BIEN_CERT_UTIL.INSERTA_BIEN_RIESGO(nIdeBien, p_nIdeDirec, p_nMtoDeclar); --  <RTC 314175> Jorge Benel / 06-09-2022 / Se implementa la actualizaci�n
            BEGIN
              UPDATE BIEN_CERT A
                 SET AnoFab = -1,
                 MtoValDeclaBienAnt = MtoValDeclarBien, --<RTC-316106> / CBayona / 23-01-2023 / Guardar el VD anterior
                 -- <I 314175> Jorge Benel /  06-09-2022
                 mtovalbien =  p_nMtoDeclar * nTasaCambio, -- <RTC 314175> jbenel 11-10-2022
                 mtovalbienmoneda =  p_nMtoDeclar,
                 mtovalbienmonedacob =  p_nMtoDeclar,
                 mtovaldeclarbien = p_nMtoDeclar,
                 FECINIVALID      = p_dFecIniValid,
                 FECFINVALID      = p_dFecFinValid
                 -- <F 314175> Jorge Benel / 06-09-2022

               WHERE A.IDEPOL = p_nIdePol
                 AND A.NUMCERT = p_nNumCert
                 AND A.CODRAMOCERT = p_cCodRamo
                 AND A.CLASEBIEN = p_cClaseBien
                 AND A.CODBIEN = p_cCodBien
                 AND A.Stsbien IN ('ACT','MOD');
              IF SQL%FOUND THEN
                dbms_output.put_line('actualizo GENERA_BIEN_MASI numero de registros: '||SQL%ROWCOUNT );
              END IF;
            END;
            --END IF; -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II
            IF cStsBien = 'ACT' THEN -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II
              PR_BIEN_CERT.GENERAR_TRANSACCION(nIdeBien, 'MODIF');
              PR_CERT_RAMO.REFLEJAR_MODIFICACION(p_nIdePol, p_nNumCert, p_cCodRamo);
            END IF; -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II
          END IF; -- <N3039087> Franck Ponte T. / 29-05-2012 / Proyecto Mejoras Acselx MEJ-001-II
        END IF; -- cStsBien
        nCant := 1;
      END IF; -- nExiste <> 0

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        nCant := 0;
      WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20100, SQLERRM||'Error al buscar existencia de bien. '||SQLERRM||CHR(10));
    END ;

    DBMS_OUTPUT.PUT_LINE('4.- nCant / cCambio : '||nCant||'/'||cCambio);

    IF nCant=1 THEN
      RETURN;
    END IF;

    FOR X IN DIREC_Q LOOP
      rCur.CodPais      := X.CodPais;
      rCur.CodEstado    := X.CodEstado;
      rCur.CodCiudad    := X.CodCiudad;
      rCur.CodMunicipio := X.CodMunicipio;
      rCur.Direc        := X.Direc;
      rCur.CodManzana    := X.CodManzana;
      rCur.Zonsis        := X.Zonsis;
      rCur.ZonaCiudad    := X.ZonaCiudad;
      EXIT;
    END LOOP;

    IF cCambio = 'S' THEN
      rCur.PorcRiesgo := 100.00;
    ELSE
      IF rCur.PorcRiesgo = 0 OR rCur.PorcRiesgo IS NULL THEN
         rCur.PorcRiesgo := 100.00;
      ELSE
        NULL;
      END IF;
    END IF;
--
    rCur.CodMonedaBien := p_cCodMoneda;
    rCur.CodMoneda := p_cCodMoneda;
    rCur.Clasebien := p_cClaseBien;
    rCur.Codbien := p_cCodBien;
    rCur.Fecinivalid := p_dFecIniValid;
    rCur.FecFinvalid := p_dFecFinValid;
    rCur.Stsbien := 'VAL';
    rCur.Indimprdeclar := 'S';
    rCur.MtoValBien := p_nMtoDeclar;        -- <I RTC 314175> 08-07-2022  Se modifical el valor 0;
    rCur.MtoValBienMoneda := p_nMtoDeclar;  -- <I RTC 314175> 08-07-2022  Se modifical el valor 0;
    rCur.Mtovaldeclarbien := p_nMtoDeclar;  -- <I RTC 314175> 08-07-2022  Se modifical el valor 0;
    rCur.Mtovaldeclabienant :=0;

    DBMS_OUTPUT.PUT_LINE('5. PR_BIEN_CERT_UTIL.INSERTA_BIEN ');
    -- <I RTC 314175> Jorge Benel / 06-09-2022

    PR_BIEN_CERT_UTIL.INSERTA_BIEN (rCur, p_nIdePol, p_nNumCert, p_cCodProd, p_cCodRamo, nIdeBien, nNroBien); -- OUT nIdeBien, nNroBien
    PR_BIEN_CERT_UTIL.INSERTA_BIEN_RIESGO(nIdeBien, p_nIdeDirec, p_nMtoDeclar);
    PR_BIEN_CERT_UTIL.VALIDA_PORCENTAJES(nIdeBien,cIndBienValDeclar);

    -- <F RTC 314175> Jorge Benel / 06-09-2022

    BEGIN
      UPDATE BIEN_CERT A
         SET AnoFab = -1
       WHERE A.IDEPOL = p_nIdePol
         AND A.NUMCERT = p_nNumCert
         AND A.CODRAMOCERT = p_cCodRamo
         AND A.CLASEBIEN = p_cClaseBien
         AND A.CODBIEN = p_cCodBien;
      IF SQL%FOUND THEN
        dbms_output.put_line('actualizo GENERA_BIEN_MASI numero de registros: '||SQL%ROWCOUNT );
      END IF;
    END;

    IF cStsCert IN('INC','MOD') THEN
      PR_BIEN_CERT.GENERAR_TRANSACCION(nIdeBien, 'INCLU');
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20200,'ERROR GENERA_BIEN PARA IDEBIEN: '|| nIdeBien || ' IDEDIREC: '||p_nIdeDirec||' p_nMtoDeclar: '|| p_nMtoDeclar ||SQLERRM);  -- <N3039024> Franck Ponte T. / 23-05-2012 / Proyecto Mejoras Acselx MEJ-001-II (HD-105807)
  END GENERA_BIEN_MASI2;


/*-----------------------------------------------------------------------------
    Nombre : GENERA_BIEN_MASI3
    Proposito : Estructura la informacion para la generacion de Bienes Masivo
    Referencias : PR_BIEN_CERT_UTIL.GENERA_BIEN_MASI
    Parametros :
      p_nIdePol          Identificador de Poliza
      p_nNumCert         Numero de Certificado
      p_cCodProd         Codigo de Producto
      p_cCodRamo         Codigo de ramo del certificado
      p_cCodPlan         Codigo de plan del certificado
      p_cRevPlan         Codigo de revision del certificado
      p_nIdeDirec        Identificador de la direccion de riesgo
      p_cClaseBien       Codigo de clase
      p_cCodBien         Codigo de tipo bien
      p_nMtoDeclar       Monto declarado
      p_cCodMoneda       Codigo de moneda
      p_dFecIniValid     Fecha de inicio de validez
      p_dFecFinValid     Fecha de fin de validez

    Log de Cambios
      Fecha       Autor            Descripcion
      20/01/2023  Christian Bayona <RTC 316106> Creaci�n
      31/01/2023  Christian Bayona <RTC 316106> Se agrega VD a campo faltante
  ----------------------------------------------------------------------------*/
PROCEDURE GENERA_BIEN_MASI3 (
    p_nIdePol           POLIZA.IDEPOL%TYPE,
    p_nNumCert          CERTIFICADO.NUMCERT%TYPE,
    p_cCodProd          PRODUCTO.CODPROD%TYPE,
    p_cCodRamo          BIEN_CERT.CODRAMOCERT%TYPE,
    p_cCodPlan          PLAN_PROD.CODPLAN%TYPE,
    p_cRevPlan          PLAN_PROD.REVPLAN%TYPE,
    p_nIdeDirec         DIREC_RIESGO_CERT.IDEDIREC%TYPE,
    p_cClaseBien        BIEN_CERT.CLASEBIEN%TYPE,
    p_cCodBien          BIEN_CERT.CODBIEN%TYPE,
    p_nMtoDeclar        DIREC_RIESGO_CERT.MTOVALDECLARDIREC%TYPE,
    p_cCodMoneda        POLIZA.CODMONEDA%TYPE,
    p_dFecIniValid      BIEN_CERT.FECINIVALID%TYPE,
    p_dFecFinValid      BIEN_CERT.FECFINVALID%TYPE
  ) IS

    CURSOR DIREC_Q IS
      SELECT CodPais, CodEstado, CodCiudad, CodMunicipio, Direc, CodManzana, Zonsis, ZonaCiudad
      FROM DIREC_RIESGO_CERT
      WHERE IdePol  = p_nIdePol
        AND  NumCert= p_nNumCert
      ORDER BY PorcSuma DESC,CodEstado;
    --
    CURSOR cur_bien IS
      SELECT A.*
      FROM BIEN_CERT A
      WHERE A.IDEPOL = p_nIdePol
        AND A.NUMCERT = p_nNumCert
        AND a.codramocert = p_cCodRamo;

    CURSOR cur_riesgo (nIdeBien BIEN_CERT.IDEBIEN%TYPE) IS
     SELECT B.*
     FROM BIEN_CERT_RIESGO B
     WHERE B.IDEBIEN = nIdeBien;
    --
    rCur                BIEN_CERT%ROWTYPE;
    cCambio              VARCHAR2(1);
    cCodMonedaLocal     TASA_CAMBIO.CODMONEDA%TYPE;
    cIndCoaAcep         POLIZA.TIPOPDCION%TYPE;
    cIndBienValDeclar   VARCHAR2(1);
    nCant               NUMBER(1):=0;
    cIndCobert          RAMO_PLAN_PROD.INDCOBERT%TYPE;
    nIdeBien            BIEN_CERT.Idebien%TYPE;
    nNroBien            BIEN_CERT.Nrobien%TYPE;
    cIndBien            RAMO_PLAN_PROD.INDBEN%TYPE;
    nExiste             NUMBER:=0;
    cStsBien            BIEN_CERT.StsBien%TYPE;
    cStsCert            certificado.StsCert%TYPE;
    nTasaCambio         TASA_CAMBIO.TASACAMBIO%TYPE := 1; -- <RTC 314175> jbenel 11-10-2022
    nMTOVALDECLARBIEN   NUMBER;
  BEGIN
    DBMS_OUTPUT.Put_Line('>>>>>>>>>> GENERA_BIEN_MASI3 <<<<<<<<<<<<<<< ');
     --validar estado del certificado
    BEGIN
      SELECT StsCert
        INTO cStsCert
        FROM CERTIFICADO
       WHERE IdePol = p_nIdePol
         AND NumCert = p_nNumCert;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20100,'ERROR, CERTIFICADO: ' || p_nNumCert ||' NO EXISTE.');
    END;
    BEGIN
      SELECT IndPriPerdida, IndCobert, IndBien
      INTO    cCambio, cIndCobert, cIndBien
      FROM   RAMO_PLAN_PROD
      WHERE   CodProd    = p_cCodProd
        AND  CodRamoPlan = p_cCodRamo
        AND  CodPlan     = p_cCodPlan
        AND  RevPlan     = p_cRevPlan;
    EXCEPTION
      WHEN OTHERS THEN
        cCambio := 'N';
        cIndCobert := NULL;
        cIndBien := 'N';
    END;
    DBMS_OUTPUT.PUT_LINE('1.- IndBien '||cIndBien);
    IF NVL(cIndBien,'N') = 'N' THEN
      RETURN;
    END IF;

    cIndBienValDeclar := PR_POLIZA_INCONSISTENCIA.PROD_CON_VALOR_DECLARADO (p_nIdePol, p_cCodRamo, p_cCodPlan, p_cRevPlan, 'PRODUCTO') ;
    nExiste := 0;

    DBMS_OUTPUT.PUT_LINE('2.- cIndBien '||cIndBienValDeclar);

    IF NVL(cIndBienValDeclar,'N') != 'S' THEN
      RETURN;
    END IF;
    -- Verifica si ya existe bien_cert para el codbien leido
    BEGIN
      SELECT IDEBIEN, StsBien, 1, MTOVALDECLARBIEN
      INTO   nIdeBien, cStsBien, nExiste, nMTOVALDECLARBIEN
      FROM  BIEN_CERT A
      WHERE A.IDEPOL = p_nIdePol
        AND A.NUMCERT = p_nNumCert
        AND A.CODRAMOCERT = p_cCodRamo
        AND A.CLASEBIEN = p_cClaseBien
        AND A.CODBIEN = p_cCodBien;

      BEGIN
        cCodMonedaLocal:=PR_VINISUSC.CODIGO_MONEDA;
        nTasaCambio := PR.TASA_CAMBIO(p_cCodMoneda, cCodMonedaLocal,SYSDATE,'D');

      EXCEPTION
         WHEN OTHERS THEN
           RAISE_APPLICATION_ERROR(-20100, SQLERRM||'No se puee obtener tipo cambio. '||SQLERRM||CHR(10));
      END;

    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        nIdeBien := NULL;
        cStsBien := NULL;
        nExiste  := 0;
    END;

  IF cStsBien IN( 'ACT', 'MOD' ) THEN
    IF nExiste > 0 THEN
      BEGIN
        UPDATE BIEN_CERT A
           SET AnoFab = -1,
           MtoValDeclaBienAnt = MtoValDeclarBien,
           mtovalbien =  mtovalbien + (p_nMtoDeclar * nTasaCambio),
           mtovalbienmoneda =  mtovalbienmoneda + p_nMtoDeclar,
           mtovalbienmonedacob = mtovalbienmonedacob + p_nMtoDeclar,
           mtovaldeclarbien = mtovaldeclarbien + p_nMtoDeclar,
           FECINIVALID      = p_dFecIniValid,
           FECFINVALID      = p_dFecFinValid
         WHERE A.IDEPOL = p_nIdePol
           AND A.NUMCERT = p_nNumCert
           AND A.CODRAMOCERT = p_cCodRamo
           AND A.IdeBIEN = nIdeBien
           AND A.Stsbien IN ('ACT','MOD');
        --
        IF SQL%FOUND THEN
          PR_BIEN_CERT_UTIL.INSERTA_BIEN_RIESGO(nIdeBien, p_nIdeDirec, p_nMtoDeclar);
          PR_BIEN_CERT_UTIL.VALIDA_PORCENTAJES(nIdeBien, cIndBienValDeclar);
          --
          FOR c IN cur_bien LOOP
            SELECT SUM(mtovaldeclarbiendirec)
            INTO nMTOVALDECLARBIEN
            FROM BIEN_CERT_RIESGO
            WHERE IDEBIEN = C.IdeBien;
            --
            FOR R IN cur_riesgo(c.IDEBIEN) LOOP
              UPDATE BIEN_CERT_RIESGO
              SET PORCRIESGO = (mtovaldeclarbiendirec / nMTOVALDECLARBIEN) * 100
              WHERE IDEBIEN = c.IdeBien
                AND IDEDIREC = r.IdeDirec;
            END LOOP;
          END LOOP;
          --
          dbms_output.put_line('actualizo GENERA_BIEN_MASI numero de registros: '||SQL%ROWCOUNT );
        END IF;
      END;
      --
      IF cStsBien = 'ACT' THEN
        PR_BIEN_CERT.GENERAR_TRANSACCION(nIdeBien, 'MODIF');
        PR_CERT_RAMO.REFLEJAR_MODIFICACION(p_nIdePol, p_nNumCert, p_cCodRamo);
      END IF;
      --
      RETURN;
    END IF;
  END IF;
---
  IF cCambio = 'S' THEN
    rCur.PorcRiesgo := 100.00;
  ELSE
    IF rCur.PorcRiesgo = 0 OR rCur.PorcRiesgo IS NULL THEN
       rCur.PorcRiesgo := 100.00;
    ELSE
      NULL;
    END IF;
  END IF;
--
    rCur.CodMonedaBien := p_cCodMoneda;
    rCur.CodMoneda := p_cCodMoneda;
    rCur.Clasebien := p_cClaseBien;
    rCur.Codbien := p_cCodBien;
    rCur.Fecinivalid := p_dFecIniValid;
    rCur.FecFinvalid := p_dFecFinValid;
    rCur.Stsbien := 'VAL';
    rCur.Indimprdeclar := 'S';
    rCur.MtoValBien := p_nMtoDeclar;
    rCur.MtoValBienMoneda := p_nMtoDeclar;
    rCur.MtoValBienMonedaCob := p_nMtoDeclar; --<RTC 316106> / CBayona / 31-01-2023 / Se consigna el mismo VD
    rCur.Mtovaldeclarbien := p_nMtoDeclar;
    rCur.Mtovaldeclabienant := 0;
    DBMS_OUTPUT.PUT_LINE('5. PR_BIEN_CERT_UTIL.INSERTA_BIEN ');

    PR_BIEN_CERT_UTIL.INSERTA_BIEN (rCur, p_nIdePol, p_nNumCert, p_cCodProd, p_cCodRamo, nIdeBien, nNroBien); -- OUT nIdeBien, nNroBien
    PR_BIEN_CERT_UTIL.INSERTA_BIEN_RIESGO(nIdeBien, p_nIdeDirec, p_nMtoDeclar);
    PR_BIEN_CERT_UTIL.VALIDA_PORCENTAJES(nIdeBien,cIndBienValDeclar);
    --
    SELECT MTOVALDECLARBIEN
    INTO   nMTOVALDECLARBIEN
    FROM  BIEN_CERT A
    WHERE A.IDEbien= nIdeBien;
    DBMS_OUTPUT.Put_Line('>>> IdeBien: ' || nIdeBien || 'Ramo: ' || p_cCodRamo || ' - MTOVALDECLARBIEN: ' || nMTOVALDECLARBIEN);

    BEGIN
      UPDATE BIEN_CERT A
         SET AnoFab = -1
       WHERE A.IDEPOL = p_nIdePol
         AND A.NUMCERT = p_nNumCert
         AND A.CODRAMOCERT = p_cCodRamo
         AND A.CLASEBIEN = p_cClaseBien
         AND A.CODBIEN = p_cCodBien;
      IF SQL%FOUND THEN
        dbms_output.put_line('actualizo GENERA_BIEN_MASI numero de registros: '||SQL%ROWCOUNT );
      END IF;
    END;

    --
    FOR R IN cur_riesgo(nIDEBIEN) LOOP
      UPDATE BIEN_CERT_RIESGO
      SET PORCRIESGO = (mtovaldeclarbiendirec / nMTOVALDECLARBIEN)*100
      WHERE IDEBIEN = nIdeBien
        AND IDEDIREC = R.idedirec;
    END LOOP;
    --

    IF cStsCert IN('INC','MOD') THEN
      PR_BIEN_CERT.GENERAR_TRANSACCION(nIdeBien, 'INCLU');
    END IF;

  EXCEPTION
    WHEN OTHERS THEN
      RAISE_APPLICATION_ERROR(-20200,'ERROR GENERA_BIEN PARA IDEBIEN: '|| nIdeBien || ' IDEDIREC: '||p_nIdeDirec||' p_nMtoDeclar: '|| p_nMtoDeclar ||SQLERRM);  -- <N3039024> Franck Ponte T. / 23-05-2012 / Proyecto Mejoras Acselx MEJ-001-II (HD-105807)
  END GENERA_BIEN_MASI3;


/*-----------------------------------------------------------------------
   Nombre     : ENDOSO_TRASLADO_BIENES
   Proposito  : Endoso de traslado de bienes
   Referencia : Formulario MANTCERT.fmb

        :C04_1.BT_IMPORTA

   Parametro  :
        p_nIdePol              Id de la poliza
        p_cCODPROD             Codigo de producto
        p_cCodMoneda           Codigo de moneda
        P_TDU_TABLA_IMP_LOCAL  Tabla de locales a cargar

   Retorno :       p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                        0,OK
                        1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   12/07/2022   Jorge Benel      <RTC 314179> Creacion
   08/11/2022  David Yupanqui        <RTC-315519>  Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
-----------------------------------------------------------------------------*/


PROCEDURE ENDOSO_TRASLADO_BIENES ( p_nIDEPOL              IN POLIZA.IDEPOL%TYPE,
                                   p_cCODPROD             IN POLIZA.CODPROD%TYPE,
                                   p_cCodMoneda           IN POLIZA.CODMONEDA%TYPE,
                                   P_TDU_TABLA_IMP_LOCAL  IN TDU_TABLA_IMP_LOCAL ,
                                   -- Variables de retorno
                                   p_cError               OUT varchar2) AS
  cRuta  VARCHAR2(4000);
  nCant NUMBER(2);
  nVal  NUMBER(1);


FUNCTION FR_RECEPCIONAR_ARCHIVO RETURN NUMBER IS

  cRuta              VARCHAR2(1000);
  cFileok            VARCHAR2(1) := 'S';
  nNumLinea          NUMBER;
  nNumBien           NUMBER;
  bVal               BOOLEAN;
  nPos               NUMBER(10);
  nPosIni            NUMBER(10);
  nPosIniAnt         NUMBER(10);
  nPosFin            NUMBER(10);
  nMonto             NUMBER(38, 20);
  nMtoTotal          NUMBER(38, 20);
  cCad               VARCHAR2(4000);
  cClaseTipo         VARCHAR2(7);
  cMensaje           VARCHAR2(4000);
  nIdeDirec          DIREC_RIESGO_CERT.IDEDIREC%TYPE;
  cClaseBien         BIEN_CERT.CLASEBIEN%TYPE;
  cCodBien           BIEN_CERT.CODBIEN%TYPE;
  cIndBien           RAMO_PLAN_PROD.INDBEN%TYPE;
  nNumcert           CERTIFICADO.NumCert%TYPE;
  nNumcertOld        CERTIFICADO.NumCert%TYPE;
  cStsCert           CERTIFICADO.StsCert%TYPE;
  nNumcertSig        CERTIFICADO.NumCert%TYPE;
  nMtoValDeclarDirec DIREC_RIESGO_CERT.MtoValDeclarDirec%TYPE;
  nAlerta            NUMBER;
  --
  rdirec            DIREC_RIESGO_CERT%ROWTYPE;
  dFecIniVig        CERTIFICADO.FecIng%TYPE;
  dFecFinVig        CERTIFICADO.FecFin%TYPE;
  cIndBienValDeclar VARCHAR2(1);

  CURSOR c_Cert(p_nNumcert NUMBER) IS
    SELECT NumCert, StsCert
      FROM CERTIFICADO
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumcert
       AND StsCert NOT IN ('ACT', 'ANU', 'EXC', 'NRE', 'NSR', 'REN');

  CURSOR c_Ramos IS
    SELECT CodPlan,
           RevPlan,
           CodRamoCert,
           FecIniValid,
           FecFinValid,
           StsCertRamo
      FROM CERT_RAMO
     WHERE IdePol = p_nIDEPOL
       AND NumCert = nNumcert
     ORDER BY NumOrdVis;

  CURSOR c_Direcciones(p_nNumCert NUMBER) IS
    SELECT IdeDirec, IdePol, NumCert, StsDirec
      FROM DIREC_RIESGO_CERT
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumCert;

  CURSOR c_bien(p_nNumcert BIEN_CERT.NumCert%TYPE) IS
    SELECT IdeBien
      FROM BIEN_CERT
     WHERE IdePol = p_nIDEPOL
       AND NumCert = p_nNumcert
       AND StsBien = 'VAL';

BEGIN
  nNumcert       := 0;
  p_cError := '0,OK'; -- <RTC-314179> Jorge Benel /14-07-2022/Reinicio de variable


    BEGIN
      nNumLinea := 0;
      nNumBien  := 0;
      nMtoTotal := 0;



      IF P_TDU_TABLA_IMP_LOCAL.COUNT > 0 THEN
        FOR i in P_TDU_TABLA_IMP_LOCAL.First .. P_TDU_TABLA_IMP_LOCAL.Last LOOP
             dbms_output.put_line('Inicio paso 1');
             dbms_output.put_line(P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec);
             ------------------------------------------
             -- PASO 1 - generar direccion de riesgo --
             ------------------------------------------

            BEGIN
              nIdeDirec :=   GRABA_LINEA_DIR_MASI2( p_nIDEPOL,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codpais ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codestado,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codciudad,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).codmunicipio ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).ubigeo ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tipovia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).nomvia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numvia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tipoint ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numint ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).manzana ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).lote ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).kilome,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tipocha,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).nomcha ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tiposdi ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).nomsdi ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).referencia ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).resto ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).porcsuma,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).idegiro  ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec ,

                                                                        P_TDU_TABLA_IMP_LOCAL(i).tep ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).tee ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).Anoconstrucc ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numpisos     ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).numsotanos   ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).fecinspeccion ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).NumCert      ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).IdeDirec     ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).IndRiesgoIns ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).IndInspeccion ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).NOTAINSPECCION ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).INDSININSPECCION,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).USUAUTORIZADOR   ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).CLASETIPO   ,
                                                                        P_TDU_TABLA_IMP_LOCAL(i).PROPIEEDIFICACION, --<RTC-315519> David Yupanqui / 08-11-2022 / Mejoras en Acsel/x para soportar cambios nueva versi�n Carga SBS
                                                                        -- out
                                                                        nNumcert,
                                                                        p_cError --<RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
                                                                       );

            --<I RTC-316106> / CBayona / 23-01-2023 / Mensaje de error
            IF p_cError != '0,OK' THEN
              RETURN 0;
            END IF;
            --<F RTC-316106> / CBayona / 23-01-2023

            EXCEPTION
              WHEN OTHERS THEN

                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Direcciones : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;
                RETURN 0;
            END;

            dbms_output.put_line('Fin paso 1 / Inicio paso 2');

            ---------------------------
            -- PASO 2 generar bienes --
            ---------------------------
            dbms_output.put_line('nIdeDirec = '||nIdeDirec);

            BEGIN
              IF nIdeDirec > 0 THEN
                --validar estado del certificado
                BEGIN
                  SELECT StsCert, FecIng, FecFin
                    INTO cStsCert, dFecIniVig, dFecFinVig
                    FROM CERTIFICADO
                   WHERE IdePol = p_nIDEPOL
                     AND NumCert = nNumcert;
                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    cStsCert := '';
                END;

                 dbms_output.put_line('** cStsCert = '||cStsCert);

                IF cStsCert IN ('VAL', 'INC') THEN
                  -- PASO 2.1  eliminar los bienes por cada direccion
                  BEGIN
                    PR_BIEN_CERT_UTIL.ELIMINAR_BIEN_RIESGO(nIdeDirec);
                  EXCEPTION
                    WHEN OTHERS THEN

                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Eliminar las Direcciones Riesgo : ' ||cMensaje ;
                      p_cError := '1, '||cMensaje;
                      --LR_ROLLBACK(NULL);
                      RETURN 0;

                  END;
                END IF;

                IF cStsCert IN ('MOD') THEN -- NUEVO CONSIDERAR LA ELIMINACION DE TODAS LAS DIRECCIONES ASOCIADAS A LOS BIENES

                  -- PASO 2.2  eliminar los bienes por cada direccion
                  BEGIN
                    PR_BIEN_CERT_UTIL.ELIMINAR_BIEN_RIESGO(nIdeDirec);
                  EXCEPTION
                    WHEN OTHERS THEN

                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Eliminar las Direcciones Riesgo : ' || cMensaje ;
                      p_cError := '1, '||cMensaje;

                      RETURN 0;
                  END;
                END IF;


                FOR X IN c_Ramos LOOP
                  BEGIN
                    SELECT IndBien
                      INTO cIndBien
                      FROM RAMO_PLAN_PROD
                     WHERE CodProd = p_cCODPROD
                       AND CodPlan = X.CodPlan
                       AND RevPlan = X.RevPlan
                       AND CodRamoPlan = X.CodRamoCert
                     ORDER BY NumOrdImp;
                  EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                      cIndBien := 'N';
                  END;


                  IF cIndBien = 'S' THEN
                    --nPosIniAnt := nPosIni;
                    nMtoTotal  := 0;
                    cClaseBien := '';
                    cCodBien   := '';

                    cClaseTipo := P_TDU_TABLA_IMP_LOCAL(i).CLASETIPO;
                    nMonto     := NVL( P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec ,0); -- / 100 JBENEL
                    nMtoTotal  := nMtoTotal + nMonto;

                     DBMS_OUTPUT.PUT_LINE('P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec = '||P_TDU_TABLA_IMP_LOCAL(i).mtovaldeclardirec);

                    IF cClaseTipo IS NOT NULL AND nMonto > 0 THEN
                      cClaseBien := SUBSTR(cClaseTipo, 1, 3);
                      cCodBien   := SUBSTR(cClaseTipo, 4, 4);
                      BEGIN
                        DBMS_OUTPUT.PUT_LINE('****  GENERA_BIEN_MASI ***' );
                        DBMS_OUTPUT.PUT_LINE(p_nIDEPOL);
                        DBMS_OUTPUT.PUT_LINE(nNumcert);
                        DBMS_OUTPUT.PUT_LINE(p_cCODPROD);
                        DBMS_OUTPUT.PUT_LINE(X.CodRamoCert);
                        DBMS_OUTPUT.PUT_LINE(X.CodPlan);
                        DBMS_OUTPUT.PUT_LINE(X.RevPlan);
                        DBMS_OUTPUT.PUT_LINE(nIdeDirec);
                        DBMS_OUTPUT.PUT_LINE(cClaseBien);
                        DBMS_OUTPUT.PUT_LINE(cCodBien);
                        DBMS_OUTPUT.PUT_LINE(nMonto);
                        DBMS_OUTPUT.PUT_LINE(p_cCodMoneda);
                        DBMS_OUTPUT.PUT_LINE(TO_CHAR(dFecIniVig,'DD/MM/YYYY'));
                        DBMS_OUTPUT.PUT_LINE(TO_CHAR(dFecFinVig,'DD/MM/YYYY'));



                        -- PASO 2.3 GENERAR BIENES
                        --- PR_BIEN_CERT_UTIL. -- jbenel se comenta para hacerlo local

                        GENERA_BIEN_MASI2(p_nIDEPOL,
                                                           nNumcert,
                                                           p_cCODPROD,
                                                           X.CodRamoCert,
                                                           X.CodPlan,
                                                           X.RevPlan,
                                                           nIdeDirec,
                                                           cClaseBien,
                                                           cCodBien,
                                                           nMonto,
                                                           p_cCodMoneda,
                                                           dFecIniVig,
                                                           dFecFinVig);
                      EXCEPTION
                        WHEN OTHERS THEN

                          cMensaje := cMensaje || ' ' || SQLERRM;
                          cMensaje := ' al Procesar la Carga de Bienes : ' ||cMensaje ;
                          p_cError := '1, '||cMensaje;
                          RETURN 0;

                      END;
                    END IF;
                      nNumBien := nNumBien + 1;

                  END IF; -- cIndBien = 'S'

                END LOOP; -- X -- c_Ramos

                DELETE BIEN_CERT BC
                WHERE  BC.IdePol   = p_nIDEPOL
                AND  BC.NumCert    = nNumcert
                AND STSBIEN        = 'ACT';

                nNumLinea := nNumLinea + 1;

            END IF; -- IF nIdeDirec > 0 THEN

           EXCEPTION
              WHEN OTHERS THEN

                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Bienes : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;

                RETURN 0;


            END;

         END LOOP; -- P_TDU_TABLA_IMP_LOCAL
       END IF;

    END ; -- JBENEL


      /*
    EXCEPTION
      WHEN NO_DATA_FOUND THEN

        IF cMensaje IS NULL THEN
          DBMS_OUTPUT.PUT_LINE('Total Registros Procesados en la Carga : ' ||TO_CHAR(nNumLinea) );
        ELSE
          p_cError := '1, '||' Error al procesar la Carga : ';
          RETURN 0;
        END IF;

        BEGIN
          nNumLinea         := 0;
          cIndBienValDeclar := PR_POLIZA_INCONSISTENCIA.PROD_CON_VALOR_DECLARADO(p_nIDEPOL, NULL,NULL,NULL,'PRODUCTO');
          LOOP
            UTL_FILE.Get_Line(in_file, linebuf);
            nPosIni  := INSTR(linebuf, '|', 0 + 1, 28);
            nPosFin  := INSTR(linebuf, '|', 0 + 1, 29);
            nNumcert := SUBSTR(linebuf, nPosIni + 1, nPosFin - nPosIni - 1);
            IF nNumcert <> NVL(nNumcertOld, 0) THEN
              nNumcertOld := nNumcert;
              OPEN c_Cert(nNumcert);

              LOOP
                FETCH c_Cert
                  INTO nNumcert, cStsCert;
                EXIT WHEN c_Cert%NOTFOUND;

                --SE ADICIONO PARA UN TEMA DE CALCULO DE PORCENTAJES CUANDO ESTA LA CARGA EN VAL
                --NORMALMENETE ESTO SE DEBE HACE EN CUANDO SE ACTIVA LA POLIZA Y CALCULA LOS PORCENTAJES

                IF cStsCert = 'INC' THEN
                  IF cIndBienValDeclar = 'S' THEN
                    BEGIN
                      PR_MOD_DIREC_RIESGO.ACTUAL_PORCENT_PARTICIP(p_nIDEPOL, nNumcert);
                    EXCEPTION
                      WHEN OTHERS THEN

                        cMensaje := cMensaje || ' ' || SQLERRM;
                        cMensaje := ' al Actualizar Porcentajes Direcciones de Riesgo : ' ||cMensaje;
                        p_cError := '1, '||cMensaje;
                        RETURN 0;
                    END;
                    nNumLinea := nNumLinea + 1;
                  END IF;
                END IF;
                IF cStsCert = 'MOD' THEN
                  -- incluir las demas direcciones
                  BEGIN
                    PR_MOD_DIREC_RIESGO.ADICION_REGISTROS_POR_INCLUIR(p_nIDEPOL,nNumcert,99999);
                  EXCEPTION
                    WHEN OTHERS THEN

                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Incluir Direcciones de Riesgo : ' ||cMensaje ;
                      p_cError := '1, '||cMensaje;
                      RETURN 0;
                  END;

                  --actualiza porcentajes de direccion
                  IF VALIDA_EXISTE_BIEN(p_nIDEPOL, nNumcert) = 1 THEN
                    BEGIN
                      PR_MOD_DIREC_RIESGO.ACTUAL_PORCENT_PARTICIP(p_nIDEPOL, nNumcert);
                    EXCEPTION
                      WHEN OTHERS THEN

                        cMensaje := cMensaje || ' ' || SQLERRM;
                        cMensaje := ' al Actualizar Porcentaje de Modificacion de Direcciones : ' ||cMensaje ;
                        p_cError := '1, '||cMensaje;

                        RETURN 0;

                    END;
                  END IF;
                  nNumLinea := nNumLinea + 1;
                END IF;
              END LOOP;
              CLOSE c_Cert;
            END IF;
          END LOOP;
        EXCEPTION
          WHEN OTHERS THEN
            IF cMensaje IS NOT NULL THEN
              p_cError := '1, '||' Error al procesar Direcciones : ' || filename;
              RETURN 0;
            END IF;

            in_file := UTL_FILE.Fopen(filename, 'r');
            BEGIN
              nNumLinea   := 0;
              nNumcertOld := NULL;
              LOOP

                nNumcert := SUBSTR(linebuf,nPosIni + 1,nPosFin - nPosIni - 1);

                IF nNumcert <> NVL(nNumcertOld, 0) THEN
                  nNumcertOld := nNumcert;
                  OPEN c_Cert(nNumcert);

                  LOOP
                    FETCH c_Cert
                      INTO nNumcert, cStsCert;
                    EXIT WHEN c_Cert%NOTFOUND;
                    BEGIN
                      SELECT NVL(SUM(MtoValDeclarDirec), 0) MtoValDeclarDirec
                        INTO nMtoValDeclarDirec
                        FROM DIREC_RIESGO_CERT
                       WHERE IdePol = p_nIDEPOL
                         AND NumCert = nNumcert
                         AND StsDirec != 'ACT';
                    EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                        nMtoValDeclarDirec := 0;
                    END;
                    -- eliminar bienes que no tengan direcciones
                    BEGIN
                      PR_BIEN_CERT_UTIL.ELIMINAR_BIEN(p_nIDEPOL,
                                                      nNumcert);
                    EXCEPTION
                      WHEN OTHERS THEN

                        cMensaje := cMensaje || ' ' || SQLERRM;
                        cMensaje := ' al Eliminar Bienes que no tienen Direcciones Asociadas : ' ||cMensaje;
                         p_cError := '1, '||cMensaje;
                        RETURN 0;

                    END;
                    IF nMtoValDeclarDirec != 0 THEN
                      FOR X IN c_ramos LOOP
                        --Verifica indicador de declaracion de bien
                        BEGIN
                          SELECT IndBien
                            INTO cIndBien
                            FROM RAMO_PLAN_PROD
                           WHERE CodProd = :B04_Q.cCodProd
                             AND CodRamoPlan = X.CodRamoCert
                             AND CodPlan = X.CodPlan
                             AND RevPlan = X.RevPlan;
                        EXCEPTION
                          WHEN OTHERS THEN
                            cIndBien := 'N';
                        END;
                        IF cIndBien = 'S' THEN
                          BEGIN
                            PR_DIREC_RIESGO_CERT.ACTUALIZAR(p_nIDEPOL,
                                                            nNumcert,
                                                            nMtoValDeclarDirec,
                                                            'PORCSUMA');
                          EXCEPTION
                            WHEN OTHERS THEN

                              cMensaje := cMensaje || ' ' || SQLERRM;
                              cMensaje := ' al Actualizar Direcciones : ' ||cMensaje ;
                               p_cError := '1, '||cMensaje;
                              RETURN 0;

                          END;
                          IF nNumBien > 0 THEN
                            BEGIN
                              --ACTUALIZA TABLAS BIEN_CERT Y BIEN_CERT_RIESGO
                              PR_BIEN_CERT_UTIL.ACTUALIZA_BIEN_RIESGO(p_nIDEPOL,
                                                                      nNumcert,
                                                                      X.CodRamoCert,
                                                                      :B04_Q.cCodProd,
                                                                      X.CodPlan,
                                                                      X.RevPlan,
                                                                      'VAL');
                            EXCEPTION
                              WHEN OTHERS THEN

                                cMensaje := cMensaje || ' ' || SQLERRM;
                                cMensaje := ' al Actualiza Bienes : ' ||cMensaje ;
                                 p_cError := '1, '||cMensaje;
                                RETURN 0;

                            END;
                          END IF;
                        END IF;
                        -- EXCLUIR por cada quiebre de Ramo
                        BEGIN
                          PR_BIEN_CERT_UTIL.EXCLUIR_BIEN_CERT(p_nIDEPOL,
                                                              nNumcert,
                                                              X.CodRamoCert);
                        EXCEPTION
                          WHEN OTHERS THEN

                            cMensaje := cMensaje || ' ' || SQLERRM;
                            cMensaje := ' al Excluir Bienes : ' || cMensaje ;
                             p_cError := '1, '||cMensaje;
                            RETURN 0;
                        END;
                      END LOOP;
                    END IF;
                    BEGIN
                      UPDATE BIEN_CERT
                         SET AnoFab = ''
                       WHERE IdePol = p_nIDEPOL
                         AND NumCert = nNumcert
                         AND AnoFab = -1;
                    END;
                    nNumLinea := nNumLinea + 1;
                  END LOOP;
                  CLOSE c_Cert;
                END IF;
              END LOOP;
            EXCEPTION
              WHEN OTHERS THEN
                IF cMensaje IS NOT NULL THEN
                   p_cError := '1, '||' Error al procesar Bienes : ' || filename ;

                  RETURN 0;
                END IF;
            END;
        END;


        DBMS_OUTPUT.PUT_LINE('Proceso Culminado: ' ||TO_CHAR(SYSDATE, 'DD/MM/YYYY HH:MI:SS') );

        RETURN 1;
      WHEN OTHERS THEN

        p_cError := '1, '||' Error al procesar el archivo : ' || filename;

        RETURN 0;
    END;
    */
    RETURN 0;

EXCEPTION
  WHEN OTHERS THEN
    cMensaje := cMensaje || ' ' || SQLERRM;
     p_cError := '1, '||cMensaje;
    RETURN 0;
END FR_RECEPCIONAR_ARCHIVO;


BEGIN -- MAIN

  nVal :=FR_RECEPCIONAR_ARCHIVO;

  dbms_output.put_line('nVal = '|| nVal);

END ENDOSO_TRASLADO_BIENES;


/*-----------------------------------------------------------------------------
  Nombre      : ExisteBien
  Proposito   : Funci�n que valida que exista un bien en una p�liza
  Referencias : <RTC-314184> Endosos Declaraci�n de Existencias Tipo A y B
  Parametros  :
                p_nIdePol          NUMBER     Identificador de P�liza
                p_nNumCert         NUMBER     N�mero de Certificado
                p_cDescBien        VARCHAR2   Descripci�n del Bien
  Retorno     :                    BOOLEAN    TRUE si existe el Bien, caso contrario FALSE


  Log de Cambios
    Fecha         Autor                      Descripci�n
    14/07/2022    Christian Bayona           Creaci�n
    02/08/2022    Christian Bayona           <RTC-313091> Modificaci�n, se adicion� registro de LOG
  ----------------------------------------------------------------------------*/
FUNCTION ExisteBien (p_nIdePol IN NUMBER,
                     p_nNumCert IN NUMBER,
                     p_cDescBien IN VARCHAR2 DEFAULT 'EXISTENCIAS')
                     RETURN BOOLEAN IS
--
nSec    Log_Poliza_SAS_AX.Secuencia%TYPE; --<RTC-314184> CBayona / 02-08-2022 /  Se agreg� registro de log
nCount  NUMBER(3);
--
BEGIN
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePol, NULL, NULL, 'LOG', NULL, SYSDATE, SYSDATE, 'ExisteBien', '', nSec); --<RTC-314184> CBayona / 02-08-2022 /  Se agreg� registro de log
  --
  SELECT COUNT(1)
  INTO nCount
  FROM Bien b,
    Bien_Cert bc
  WHERE bc.IdePol = p_nIdePol
    AND bc.NumCert = p_nNumCert
    AND bc.FecExc IS NULL
    AND bc.CodBien = b.CodBien
    AND bc.ClaseBien = b.ClaseBien
    AND bc.StsBien = 'ACT'
    AND b.DescBien = p_cDescBien;
  --
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'OK', SYSDATE, SYSDATE, 'ExisteBien', '', nSec); --<RTC-314184> CBayona / 02-08-2022 /  Se agreg� registro de log
  RETURN (nCount > 0);

--<I RTC-314184> CBayona / 02-08-2022 /  Se agreg� excepci�n y registro de log
EXCEPTION
  WHEN OTHERS THEN
    pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'ExisteBien', '', nSec);
    RETURN FALSE;
--<F RTC-314184>
END ExisteBien;


  /*-----------------------------------------------------------------------------
  Nombre      : modificar_prima
  Proposito   : Procedimiento que modifica prima
  Referencias : <RTC-314183> Inclusion de Equipos TREC
  Parametros  :
                p_idepol     NUMBER    Identificador de Poliza
                p_prima      NUMBER    Prima a cobrar

  Log de Cambios
    Fecha         Autor                      Descripcion
    19/07/2022    Mauro Zegarra              Creacion
    24/10/2022    Mauro Zegarra              <RTC 314951> Soporte para polizas con varios certificados
    10/01/2023   Christian Bayona            <RTC 316106> Se actualiza excepci�n
    17/04/2023   Christian Bayona            <RTC 316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B
  -----------------------------------------------------------------------------*/
  function modificar_prima(p_idepol poliza.idepol%type, p_prima oper_pol.mtooper%type)
    return oper_pol.numoper%type is
    C_SI constant varchar2(1) := 'S';
    r_poliza  poliza%rowtype;
    l_seq     log_poliza_sas_ax.secuencia%type;
    l_numoper oper_pol.numoper%type;
    l_numcert certificado.numcert%type;
    ----------------------------------------------
    function es_truncante return boolean is
    begin
      return pr.existe_lval('PRIMAMIN', 'TRUNCANTE') = 'S';
    end;
    ----------------------------------------------
    procedure validar is
      l_prima_cal oper_pol.mtooper%type;
      l_prima_min oper_pol.mtooper%type;
      l_mto_ajust oper_ajust_pol.mtoajuste%type;
      l_seq_val   log_poliza_sas_ax.secuencia%type;
      l_count     pls_integer := 0;
    begin
      pr_interfase_ax.limpiar_log_mensajes();

      -- mtooper de la activacion temporal
      l_prima_cal := pr_oper_pol.obtener_mto_oper_total_t(p_idepol);

      -- solo aplica para endosos, no para emisiones
      if pr_oper_pol.existe_oper_pol_idepol(p_idepol) = C_SI then
        r_poliza := pr_poliza.datos_poliza(p_idepol);
        -- EM: Emision Modificada
        l_prima_min := pr_vinisusc.val_min(r_poliza.codprod, 'EM');
      end if;

      begin
        select nvl(sum(mtoajuste), 0)
          into l_mto_ajust
          from oper_ajust_pol
         where idepol = p_idepol
           and inddefintivo = 'N';

      exception
        when no_data_found then
          l_mto_ajust := 0;
      end;

      if l_prima_min - l_prima_cal - l_mto_ajust > 0 then
        -- se cumple cuando la prima calculada es menor a la prima minima
        -- eg: 50 - 40 - 0 =  30 : menor a la prima minima
        -- cuando no se cumple
        -- eg: 50 - 60 - 0 = -10 : mayor a la prima minima

        if es_truncante() then
          -- 'La prima minima para el producto: :mx1 es de: :mx2'
          pr_interfase_ax_sas_util.insertar_log_errores('VALIDAR',
                                                        'VAL-1524',
                                                        r_poliza.codprod,
                                                        l_prima_min);
          raise_application_error(-20100, 'PRIMA MENOR A LA MINIMA');
        end if;

        -- se registra trazabilidad de polizas con prima menor a la minima
        pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax(p_cAccion     => 'I',
                                                          p_nIdePol     => p_idepol,
                                                          p_nNumcert    => null,
                                                          p_nNumOper    => null,
                                                          p_cTipoMov    => 'MIN',
                                                          p_cTipoEnd    => 'PRIMA',
                                                          p_dFecIniProc => sysdate,
                                                          p_dFecFinProc => null,
                                                          p_cProceso    => 'MODIFICAR_PRIMA',
                                                          p_cSubProceso => 'TRAZABILIDAD',
                                                          p_nSecuencia  => l_seq_val);
      end if;
    end;
    ----------------------------------------------
    function get_numcert(p_idepol poliza.idepol%type) return certificado.numcert%type is
      l_numcert certificado.numcert%type;
    begin
      select max(t.numcert) --<RTC 314951> Mauro Zegarra / 24-10-2022 / Soporte para polizas con varios certificados
        into l_numcert
        from certificado t
       where t.idepol = p_idepol
         --and t.stscert = pr_interfase_ax_sas_util.kestmod; --<RTC 316878> / Christian Bayona / 17-04-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B / Se reemplaza esta l�nea
         and t.stscert IN ('INC', 'MOD'); --<RTC 316878> / Christian Bayona / 17-04-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B

      return l_numcert;
    exception
      when others then
        return 1;  --<RTC 316106> CBayona / 10-01-2023 / Considerar valor por omisi�n para el certificado
    end;
    ----------------------------------------------
  begin
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax(p_cAccion     => 'I',
                                                      p_nIdePol     => p_idepol,
                                                      p_nNumcert    => null,
                                                      p_nNumOper    => null,
                                                      p_cTipoMov    => 'LOG',
                                                      p_cTipoEnd    => null,
                                                      p_dFecIniProc => sysdate,
                                                      p_dFecFinProc => null,
                                                      p_cProceso    => 'MODIFICAR_PRIMA',
                                                      p_cSubProceso => null,
                                                      p_nSecuencia  => l_seq);
    -->>
    validar();
    l_numcert := get_numcert(p_idepol);
    recargar_descontar(p_idepol, l_numcert, p_prima);

    -- se vuelve a llamar a la activacion temporal y continua con la activacion
    begin
      pr_mov_prima.eliminar_mov_t(p_idepol);
      l_numoper := pr_poliza.activar(p_idepol, 'T');
    exception
      when others then
        raise_application_error(-20100,
                                pr_interfase_ax_sas_util.mensaje_error('VAL-0108') || sqlerrm);
    end;
    --<<

    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax(p_cAccion     => 'F',
                                                      p_nIdePol     => p_idepol,
                                                      p_nNumcert    => null,
                                                      p_nNumOper    => null,
                                                      p_cTipoMov    => 'LOG',
                                                      p_cTipoEnd    => 'OK',
                                                      p_dFecIniProc => null,
                                                      p_dFecFinProc => sysdate,
                                                      p_cProceso    => 'MODIFICAR_PRIMA',
                                                      p_cSubProceso => null,
                                                      p_nSecuencia  => l_seq);

    return l_numoper;

  exception
    when others then
      raise_application_error(-20100, 'modificar_prima: ' || sqlerrm);
  end;

  /*-----------------------------------------------------------------------------
  Nombre      : recargar_descontar
  Proposito   : Procedimiento para recargos y descuentos de prima por certificado
  Referencias : <RTC-314183> Inclusion de Equipos TREC
  Parametros  :
                p_idepol     NUMBER    Identificador de Poliza
                p_numcert    NUMBER    Identificador de Certificado
                p_prima      NUMBER    Prima a cobrar

  Log de Cambios
    Fecha         Autor                      Descripci?n
    19/07/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure recargar_descontar(p_idepol  poliza.idepol%type,
                               p_numcert certificado.numcert%type,
                               p_prima   oper_pol.mtooper%type) is
    r_reca reca_dcto_certif%rowtype;
    r_cert certificado%rowtype;

    l_mtoprimaemi  reca_dcto_certif.mtoprimaemi%type;
    l_mtorecadcto  reca_dcto_certif.mtorecadcto%type;
    l_porcrecadcto reca_dcto_certif.porcrecadcto%type;
    ----------------------------------------------
    procedure vip_plan_prod(p_idepol poliza.idepol%type, p_numcert certificado.numcert%type) is
      l_codprod       poliza.codprod%type;
      l_cobert_config varchar2(1);
      l_codplan       cobert_cert.codplan%type;
      l_revplan       cobert_cert.revplan%type;
      l_existe        pls_integer := 0;
      l_mtorecadcto   reca_dcto_certif.mtorecadcto%type;

    begin
      begin
        select codprod into l_codprod from poliza where idepol = p_idepol;
      exception
        when others then
          l_codprod := null;
      end;

      /*todo: text="mover a validar()"*/
      -- 1.- verificar existen coberturas configuradas para recargos / descuentos
      l_cobert_config := pr_reca_dcto_certif.existe_cobert_config(p_idepol, p_numcert, l_codprod);

      if l_cobert_config = 'N' then
        if l_codprod <> '3001' then
          -- 'No existen Coberturas Configuradas para su aplicaci�n'
          pr_interfase_ax_sas_util.insertar_log_errores('VALIDAR', 'VAL-1525');
          raise_application_error(-20100, 'COBERTURA NO CONFIGURADA');
        end if;
      end if;

      begin
        select distinct cc.codplan, cc.revplan
          into l_codplan, l_revplan
          from cobert_cert cc
         where cc.idepol = p_idepol
           and cc.numcert = p_numcert;

      exception
        when others then
          l_codplan := null;
          l_revplan := null;
          l_codprod := null;
      end;

      select count(1)
        into l_existe
        from vip_plan_prod vp
       where vp.codprod = l_codprod
         and vp.codplan = l_codplan
         and vp.revplan = l_revplan
         and vp.codgrpvip || vp.codprod in
             (select lv.descrip
                from lval lv
               where lv.tipolval = 'SEGVIPD'
                 and lv.codlval like 'DOMIC%');

      if l_existe > 0 then
        select sum(a.mtorecadcto)
          into l_mtorecadcto
          from reca_dcto_certif a
         where a.idepol = p_idepol
           and a.numcert = p_numcert
           and a.stsrecadcto = 'VAL';

        update mod_texto_anexo_cert mc
           set mc.primace = l_mtorecadcto
         where mc.idepol = p_idepol
           and mc.numcert = p_numcert
           and mc.stsmod not in ('ACT', 'ANU');

        begin
          declare
            l_endoso     pls_integer := 0;
            l_dia_endoso number;
            l_dia_emi    number;
            l_prmprorat  number;

          begin
            select count(1)
              into l_endoso
              from mod_cobert c
             where c.idepol = p_idepol
               and c.numcert = p_numcert
               and c.stsmodcobert in ('INC');

            if l_endoso > 0 then
              begin
                select distinct c.fecfinvalid - c.fecinivalid
                  into l_dia_endoso
                  from mod_cobert c
                 where c.idepol = p_idepol
                   and c.numcert = p_numcert
                   and c.stsmodcobert in ('INC');
              exception
                when others then
                  l_dia_endoso := 0;
              end;

              begin
                select distinct c.fecfinvalid - c.fecinivalid
                  into l_dia_emi
                  from mod_cobert c
                 where c.idepol = p_idepol
                   and c.numcert = p_numcert
                   and c.stsmodcobert not in ('INC', 'VAL', 'MOD', 'ANU')
                   and c.nummod =
                       (select min(e.nummod)
                          from mod_cobert e
                         where e.idepol = c.idepol
                           and e.numcert = c.numcert
                           and e.stsmodcobert not in ('INC', 'VAL', 'MOD', 'ANU'));

              exception
                when others then
                  l_dia_emi := 0;
              end;

              if l_dia_endoso > 0 and l_dia_emi > 0 then
                begin
                  l_prmprorat := round((l_dia_endoso * l_mtorecadcto) / l_dia_emi, 2);
                exception
                  when others then
                    l_prmprorat := 0;
                end;
              end if;

              if l_prmprorat > 0 then
                if pr.busca_lval('SEGVIPD', 'ACTUJETFORM') = 'S' then
                  update mod_texto_anexo_cert mc
                     set mc.primace = l_prmprorat
                   where mc.idepol = p_idepol
                     and mc.numcert = p_numcert
                     and mc.stsmod not in ('ACT', 'ANU');
                end if;

                if pr.busca_lval('SEGVIPD', 'ACTURECARGO') = 'S' then
                  update reca_dcto_certif a
                     set a.mtorecadcto = l_prmprorat
                   where a.idepol = p_idepol
                     and a.numcert = p_numcert
                     and a.stsrecadcto = 'VAL';
                end if;
              end if;
            end if;
          end;
        end;
      end if;
    end;
    ------------------------------------------------
  begin
    l_mtoprimaemi := pr_reca_dcto_certif.prima_por_emitir_certif(p_idepol, p_numcert);

    if p_prima = l_mtoprimaemi then
      return;
    end if;

    if l_mtoprimaemi <> 0 then
      l_mtorecadcto  := abs(p_prima - l_mtoprimaemi);
      l_porcrecadcto := abs(pr_reca_dcto_certif.porc_reca_dcto(p_idepol,
                                                               p_numcert,
                                                               l_mtoprimaemi,
                                                               l_mtorecadcto));
    end if;

    r_cert := pr_certificado.datos_cert(p_idepol, p_numcert);

    r_reca.idepol          := p_idepol;
    r_reca.numcert         := p_numcert;
    r_reca.stsrecadcto     := 'VAL';
    r_reca.fecinivalid     := r_cert.fecing;
    r_reca.fecfinvalid     := r_cert.fecfin;
    r_reca.porcrecadcto    := l_porcrecadcto;
    r_reca.baserecadcto    := 'T';
    r_reca.mtorecadcto     := l_mtorecadcto;
    r_reca.tipousorecadcto := 'G';
    r_reca.numrecadcto     := 1;
    r_reca.indprorrata     := 'N';
    r_reca.mtoprimaemi     := l_mtoprimaemi;
    r_reca.mtoprimacob     := p_prima;

    -- recargo
    if p_prima > l_mtoprimaemi then
      r_reca.tiporecadcto := 'R';
      r_reca.codrecadcto  := '00002'; --'Recargo Tecnico'
    end if;

    -- descuento
    if p_prima < l_mtoprimaemi then
      r_reca.tiporecadcto := 'D';
      r_reca.codrecadcto  := '00001'; --'Descuento Comercial'
    end if;

    insert into reca_dcto_certif values r_reca;

    vip_plan_prod(p_idepol, p_numcert);

    -- VAL->INC
    pr_det_oper_pol.incluir(p_idepol, r_reca.numcert, r_reca.tiporecadcto, r_reca.codrecadcto);
  end;


/*-----------------------------------------------------------------------------
  Nombre      : CAMBIO_RESP_PAGO
  Proposito   : Procedimiento para cambiar un responsable de pago
  Referencias : <RTC-313349> Cambio de responsable de pago
  Parametros  :
                p_nIdePol     NUMBER    Identificador de Poliza
                p_nNumcert    NUMBER    Identificador de Certificado
                p_TABLA_RESP_PAGO       Arreglo de Responsable de Pago
                p_cCodUsuario VARCHAR2  NumId del usuario de Auditoria

      Retorno :    p_cursorMsgValida      Cursor con los mensajes de validacion
                   p_cError         Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                   0, OK
                   1, Error

  Log de Cambios
    Fecha         Autor                      Descripci?n
    20/07/2022    Jorge Benel                Creacion
    29/08/2022    Jorge Benel                <RTC 313349> Complementa logica de actualizacion de la tabla PORC_DER_EMI.
    19/09/2022    Jorge Benel                <RTC 313349> Inclusi�n de cursor de salida
 -----------------------------------------------------------------------------*/

 PROCEDURE CAMBIO_RESP_PAGO(p_nIdePol     IN POLIZA.IDEPOL%type,
                            p_nNumcert    IN certificado.numcert%type,
                            p_TABLA_RESP_PAGO IN TDU_TABLA_RESP_PAGO,
                            p_cCodUsuario     IN  Varchar2,
                            --
                            p_cursorMsgValida      OUT sys_refcursor , -- <RTC 313349> jorge benel / 19-09-2022 / Inclusi�n de cursor de salida
                            p_cError          OUT VARCHAR2) IS

     nExisteValidacion       NUMBER(1):= 0;
     cStsPol                 POLIZA.STSPOL%TYPE;
     cCodCli                 CERTIFICADO.CODCLI%TYPE;
     cCodCliActual           CERTIFICADO.CODCLI%TYPE;
     nSecuencia              LOG_POLIZA_SAS_AX.SECUENCIA%TYPE;
     cStsPolCli              POLIZA.STSPOL%TYPE;
     nNumMod                  MOD_POL_CERT_CLI.NUMMOD%TYPE;
     --<I RTC 313349> Jorge Benel / 29-08-2022
     nCantReg             INTEGER;
     nPorcDerEmi          PORC_DER_EMI.PORCDEREMI%TYPE;
     nMtoDerEmi           PORC_DER_EMI.MTODEREMI%TYPE;
     --<F RTC 313349> Jorge Benel / 29-08-2022

    -- Validaciones
    PROCEDURE VALIDAR  IS
       nNumPol         POLIZA.NUMPOL%TYPE;
       cCodProd        POLIZA.CODPROD%TYPE;
       dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
       dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
       bExiste         BOOLEAN;
    BEGIN
       -- Eliminar los datos del Log de mensajes de validaci�n
       LIMPIAR_LOG_MENSAJES;
       -- Obtenemos informacion de la poliza
       bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
       -- Validacion del estado de la poliza
       bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
       -- Validar si existe el certificado
       bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_EXISTE_CERTIFICADO (p_nIdePol , p_nNumCert ) ;
       -- Verificacion del responsable de pago
       PR_INTERFASE_AX_SAS_UTIL.VALIDA_RESP_PAGO ( p_TABLA_RESP_PAGO ,'N');  -- Se envia 'N' para que sea obligatorio recibir los datos del arreglo
       -- Validar el usuario
       PR_INTERFASE_AX_SAS_UTIL.VALIDA_NUMID_USUARIO( p_cCodUsuario );

       -- Si existen errores retornar un RAISE
       IF PR_INTERFASE_AX_SAS_UTIL.EXISTE_LOG_MENSAJES = 'S' THEN
          nExisteValidacion := 1;
          p_cError := '1,'||'Existen validaciones encontradas.';
          p_cursorMsgValida := pr_interfase_ax_sas.MOSTRAR_CURSOR_MENSAJES;  -- <RTC 313349> jorge benel / 19-09-2022 / Inclusi�n de cursor de salida
          RAISE_APPLICATION_ERROR(-20100,'Existen validaciones encontradas.');
       END IF;
    END VALIDAR;

  BEGIN
     p_cError := '0,OK';
     PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdePol,p_nNumcert,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'PR_INTERFASE_AX.CAMBIO_RESP_PAGO','',nSecuencia );
     -- Validaciones Previas
     VALIDAR;

     IF  p_cError = '0,OK' THEN
       -- Actualizar el responsable de pago del certificado
       --**************************************************
       -- Eliminar los responsables de pago existentes , con el objetivo que
       -- siempre sume 100% la informacion a grabar.
       DELETE RESP_PAGO
       WHERE IDEPOL  = p_nIdepol
        AND  NUMCERT = p_nNumcert;
       -- Actualizar el responsable del certificado
       PR_INTERFASE_AX_SAS_UTIL.GRABAR_TABLA_RESP_PAGO ( p_nIdePol , p_nNumcert , p_TABLA_RESP_PAGO , p_cError);

       -- Reflejar la modificacion en la poliza.
       PR_CERTIFICADO.REFLEJAR_MODIFICACION(p_nIdepol,p_nNumcert);
       ---<I RTC 313349> Jorge Benel / 29-08-2022 / Complementa l?gica de actualizaci?n de la tabla PORC_DER_EMI.
       BEGIN
           nCantReg := p_TABLA_RESP_PAGO.COUNT;
       EXCEPTION
            WHEN OTHERS THEN
              nCantReg := 0;
       END;
       FOR  i in 1..nCantReg LOOP
           BEGIN
              SELECT NVL(porcderemi, 0) INTO nPorcDerEmi
                FROM PORC_DER_EMI
                WHERE IDEPOL = p_nIdePol
                  AND NUMCERT = p_nNumCert
                  AND CODCLI = p_TABLA_RESP_PAGO(i).numIdPerRespPago;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                -- REGISTRAR
                BEGIN
                  SELECT PORCCPTOF INTO nPorcDerEmi
                  FROM CONC_FINANCIAMIENTO C
                  WHERE C.CODPLAN = p_TABLA_RESP_PAGO(i).CODPLANFRACC
                    AND C.MODPLAN = p_TABLA_RESP_PAGO(i).MODPLANFRACC
                    AND C.CODGRUPOACRE = 'GRUFIN'
                    AND C.CODCPTOACRE = 'GTOEMI';

                  nMtoDerEmi  := 0;
                EXCEPTION
                  -- Existen Planes que no tienen % Gasto de Emision
                  WHEN NO_DATA_FOUND THEN
                    nPorcDerEmi := 0;
                    nMtoDerEmi  := 0;

                  WHEN OTHERS THEN
                     p_cError :=   '1,'||'Error al obtener el % Gasto de Emision para el Plan Financiamiento: '||p_TABLA_RESP_PAGO(i).CODPLANFRACC||'-'||p_TABLA_RESP_PAGO(i).MODPLANFRACC;

                END;

                cCodCli := PR_ADA_EMI_QRY.EQUIV_NUMID_CLIENTE(p_TABLA_RESP_PAGO(i).numIdPerRespPago);

                INSERT INTO PORC_DER_EMI    (idepol, numcert, codcli, codplanfracc, nummodplanfracc, porcderemi, mtoderemi)
                VALUES (p_nIdePol, p_nNumCert,cCodCli ,p_TABLA_RESP_PAGO(i).CODPLANFRACC,p_TABLA_RESP_PAGO(i).MODPLANFRACC,nPorcDerEmi,nMtoDerEmi);

              WHEN OTHERS THEN
                p_cError :=   '1,'||SQLERRM;
            END;
         END LOOP;
         --<F RTC 313349> Jorge Benel / 29-08-2022
     END IF;

     p_cursorMsgValida := pr_interfase_ax_sas.MOSTRAR_CURSOR_MENSAJES;   -- <RTC 313349> jorge benel / 19-09-2022 / Inclusi�n de cursor de salida
     PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'PR_INTERFASE_AX.CAMBIO_RESP_PAGO','',nSecuencia );

  EXCEPTION
     WHEN OTHERS THEN
         p_cError := '1,'|| SQLERRM;
         IF nExisteValidacion > 0 THEN   -- Insertar el ultimo mensaje de error en el LOG.
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('PR_INTERFASE_AX.CAMBIO_RESP_PAGO', kCodMsgValidacion ,SQLERRM );
         ELSE
            PR_INTERFASE_AX_SAS_UTIL.INSERTAR_LOG_ERRORES('PR_INTERFASE_AX.CAMBIO_RESP_PAGO', kCodMsgError , SQLERRM );
         END IF;
         -- Devolver los mensajes de validacion
         p_cursorMsgValida := pr_interfase_ax_sas.MOSTRAR_CURSOR_MENSAJES;   -- <RTC 313349> jorge benel / 19-09-2022 / Inclusi�n de cursor de salida
         PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdePol,p_nNumcert,NULL,'LOG','ERR',SYSDATE,SYSDATE,'PR_INTERFASE_AX.CAMBIO_RESP_PAGO','',nSecuencia );
  END CAMBIO_RESP_PAGO;
  /*-----------------------------------------------------------------------------
  Nombre      : modificar_deducible
  Proposito   : Procedimiento para modificacion de deducibles
  Referencias : <RTC-314181> Modificaci�n de Deducible
  Parametros  :
                p_idepol        NUMBER                      Identificador de Poliza
                p_numcert       NUMBER                      Identificador de Certificado
                p_fecini        DATE                        Fecha de inicio de Vigencia del Certificado
                p_fecfin        DATE                        Fecha de fin de Vigencia del Certificado
                p_deducibles    TDU_TABLA_DEDUCIBLE_COBERT  Arreglo de Deducibles
                p_txtadicional  VARCHAR2                    Texto Adicional
                p_codusuario    VARCHAR2                    NumId del Usuario
                p_mensajes      REF CURSOR                  Mensajes de error
                p_error         VARCHAR2                    Mensaje de error

  Log de Cambios
    Fecha         Autor                      Descripcion
    02/08/2022    Mauro Zegarra              Creacion
    13/09/2022    Mauro Zegarra              Obtener certificado
    20/09/2022    Mauro Zegarra              Validaciones/Mejoras
    26/09/2022    Mauro Zegarra              Mejoras
    27/06/2023    Christian Bayona           <RTC 317178> Error en la obtenci�n de datos para impresi�n para los casos con varios responsables de pago no est� obteniendo todos los datos requeridos
  -----------------------------------------------------------------------------*/
  procedure modificar_deducible(p_idepol       poliza.idepol%type,
                                p_numcert      certificado.numcert%type,
                                p_fecini       certificado.fecing%type,
                                p_fecfin       certificado.fecfin%type,
                                p_deducibles   tdu_tabla_deducible_cobert,
                                p_txtadicional varchar2,
                                p_codusuario   varchar2,
                                p_mensajes     out sys_refcursor,
                                p_error        out varchar2) is
    c_ok constant varchar2(30) := '0,OK';
    l_seq         log_poliza_sas_ax.secuencia%type;
    l_stscobert   cobert_cert.stscobert%type;
    l_idecobert   cobert_cert.idecobert%type;
    l_stsmod      deducible_cobert_grupo.stsmod%type;
    l_numgrupoded deducible_cobert_grupo.numgrupoded%type;
    r_certificado certificado%rowtype;
    l_existe      boolean := false;
    l_codplan     cert_ramo.codplan%type;
    l_revplan     cert_ramo.revplan%type;
    l_stscertramo cert_ramo.stscertramo%type;

    cursor c_no_tiene_def is
      select (select d.codramocert
                from table(p_deducibles) d
               where d.codcobert = t.codcobert
                 and rownum = 1) codramocert,
             t.codcobert
        from table(p_deducibles) t
       where not exists (select 1
                from table(p_deducibles)
               where codcobert = t.codcobert
                 and valest = 'DEF')
       group by t.codcobert;

    cursor c_mas_de_un_def is
      select (select d.codramocert
                from table(p_deducibles) d
               where d.codcobert = t.codcobert
                 and rownum = 1) codramocert,
             t.codcobert,
             count(*)
        from table(p_deducibles) t
       where t.valest = 'DEF'
       group by t.codcobert
      having count(*) > 1;

    cursor c_repetidos is
      select t.codramocert,
             t.codcobert,
             t.basededcalculo,
             t.porcdedcalculo,
             t.mtodedfijo,
             t.basededminimo,
             t.porcdedminimo,
             t.mtodedminimo,
             t.basededmaximo,
             t.porcdedmaximo,
             t.mtodedmaximo,
             t.codest,
             t.valest,
             t.textodeduc,
             t.idededuc,
             t.txtadicional,
             count(*)
        from table(p_deducibles) t
       group by t.codramocert,
                t.codcobert,
                t.basededcalculo,
                t.porcdedcalculo,
                t.mtodedfijo,
                t.basededminimo,
                t.porcdedminimo,
                t.mtodedminimo,
                t.basededmaximo,
                t.porcdedmaximo,
                t.mtodedmaximo,
                t.codest,
                t.valest,
                t.textodeduc,
                t.idededuc,
                t.txtadicional
      having count(*) > 1;
    ----------------------------------------------
    procedure validar is
    begin
      pr_interfase_ax.limpiar_log_mensajes();

      -- validar deducibles
      for idx in 1 .. p_deducibles.count loop
        if p_deducibles(idx).codramocert is null then
          -- 'El codigo de Ramo es obligatorio'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible', 'VAL-1529');
          raise_application_error(-20100, 'RAMO OBLIGATORIO');
        end if;

        if p_deducibles(idx).codcobert is null then
          -- 'El codigo de Cobertura del Ramo (:mx1) es obligatorio'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1530',
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'COBERTURA OBLIGATORIO');
        end if;

        if p_deducibles(idx).numded is null then
          -- 'El numero de Deducible de la Cobertura (:mx1) del Ramo (:mx2) es obligatorio'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1537',
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'NUMDED OBLIGATORIO');
        end if;

        -- ref: CONFDEDP.PAGE02.DEDUCIBLE_COBERT.PRE-INSERT
        if p_deducibles(idx).codest is null then
          -- 'El Codigo Estadistico de la Cobertura (:mx1) del Ramo (:mx2) es obligatorio'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1531',
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'CODEST OBLIGATORIO');
        end if;

        -- ref: CONFDEDP.PAGE02.DEDUCIBLE_COBERT.PRE-INSERT
        if p_deducibles(idx).valest is null then
          -- 'El Valor Estadistico de la Cobertura (:mx1) del Ramo (:mx2) es obligatorio'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1532',
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'VALEST OBLIGATORIO');
        end if;

        if p_deducibles(idx).mtodedminimo < 0 then
          -- 'El MTODEDMINIMO de la cobertura (:mx1) del Ramo (:mx2) no puede ser negativo'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1540',
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'MTODEDMINIMO NEGATIVO');
        end if;

        if p_deducibles(idx).mtodedmaximo < 0 then
          -- 'El MTODEDMAXIMO de la cobertura (:mx1) del Ramo (:mx2) no puede ser negativo'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1541',
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'MTODEDMAXIMO NEGATIVO');
        end if;

        if p_deducibles(idx).mtodedfijo < 0 then
          -- 'El MTODEDFIJO de la cobertura (:mx1) del Ramo (:mx2) no puede ser negativo'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1542',
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'MTODEDFIJO NEGATIVO');
        end if;

        l_existe := pr_interfase_ax_sas_util.existe_cert_ramo(p_idepol,
                                                              p_numcert,
                                                              p_deducibles(idx).codramocert,
                                                              l_codplan,
                                                              l_revplan,
                                                              l_stscertramo);

        if not l_existe then
          -- 'No existe el ramo :mx1 en cert_ramo'
          raise_application_error(-20100, 'RAMO NO EXISTE');
        end if;

        if not pr_interfase_ax_sas_util.existe_cobertura(p_deducibles(idx).codramocert,
                                                         p_deducibles(idx).codcobert) then
          -- 'La cobertura :mx1 no existe.:mx2'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-0068',
                                                        p_deducibles(idx).codcobert,
                                                        ' Ramo: ' || p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'COBERTURA NO EXISTE');
        end if;

        select t.stscobert, t.idecobert
          into l_stscobert, l_idecobert
          from cobert_cert t
         where t.idepol = p_idepol
           and t.numcert = p_numcert
           and t.codramocert = p_deducibles(idx).codramocert
           and t.codcobert = p_deducibles(idx).codcobert;

        -- si: stscobert in ('ANU', 'EXC', 'REN')
        if pr.existe_lval('APIENDRG', l_stscobert) = 'S' then
          -- 'El estado (:mx1) de la Cobertura (:mx2) del Ramo (:mx3) no es valido'
          pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                        'VAL-1533',
                                                        l_stscobert,
                                                        p_deducibles(idx).codcobert,
                                                        p_deducibles(idx).codramocert);
          raise_application_error(-20100, 'STSCOBERT INVALIDO');
        end if;

        select nvl(max(numgrupoded), 0)
          into l_numgrupoded
          from deducible_cobert_grupo t
         where t.idecobert = l_idecobert;

        -- si existe grupo de deducibles
        if l_numgrupoded > 0 then
          select t.stsmod
            into l_stsmod
            from deducible_cobert_grupo t
           where t.idecobert = l_idecobert
             and t.numgrupoded = l_numgrupoded;

          -- ref: CONFDEDP.TB_CTRLV.COPIAR
          if l_stsmod <> 'ACT' then
            -- 'El estado (:mx1) del Grupo de Deducibles de la Cobertura (:mx2) del Ramo (:mx3) no es valido'
            pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                          'VAL-1534',
                                                          l_stsmod,
                                                          p_deducibles(idx).codcobert,
                                                          p_deducibles(idx).codramocert);
            raise_application_error(-20100, 'STSMOD INVALIDO');
          end if;
        end if;
      end loop;

      -- Verificamos que exista un DEF por cobertura
      for r in c_no_tiene_def loop
        -- 'No existe Deducible DEF para la Cobertura (:mx1) del Ramo (:mx2)'
        pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                      'VAL-1535',
                                                      r.codcobert,
                                                      r.codramocert);
      end loop;

      if pr_interfase_ax_sas_util.existe_log_mensajes = 'S' then
        raise_application_error(-20100, 'COBERTURA SIN DEF');
      end if;

      -- Verificamos que no exista mas de un DEF por cobertura
      for r in c_mas_de_un_def loop
        -- 'Existe mas de un Deducible DEF para la Cobertura (:mx1) del Ramo (:mx2)'
        pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                      'VAL-1536',
                                                      r.codcobert,
                                                      r.codramocert);
      end loop;

      if pr_interfase_ax_sas_util.existe_log_mensajes = 'S' then
        raise_application_error(-20100, 'COBERTURA CON MAS DE UN DEF');
      end if;

      -- Verificamos que no exista deducibles repetidos
      for r_repetido in c_repetidos loop
        -- 'Existen Deducibles repetidos en el Ramo (:mx1) Cobertura (:mx2)'
        pr_interfase_ax_sas_util.insertar_log_errores('modificar_deducible',
                                                      'VAL-1543',
                                                      r_repetido.codramocert,
                                                      r_repetido.codcobert);
        raise_application_error(-20100, 'DEDUCIBLE REPETIDO');
      end loop;

    exception
      when others then
        pr_interfase_ax_sas_util.insertar_log_errores('pr_interfase_ax.modificar_deducible',
                                                      pr_interfase_ax_sas.kcodmsgerror,
                                                      'validar: ' || sqlerrm);
        raise_application_error(-20100, 'validar: ' || sqlerrm);
    end;
    ----------------------------------------------
  begin
    p_error := '1';
    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax(p_cAccion     => 'I',
                                                      p_nIdePol     => p_idepol,
                                                      p_nNumcert    => null,
                                                      p_nNumOper    => null,
                                                      p_cTipoMov    => 'LOG',
                                                      p_cTipoEnd    => null,
                                                      p_dFecIniProc => sysdate,
                                                      p_dFecFinProc => null,
                                                      p_cProceso    => 'modificar_deducible',
                                                      p_cSubProceso => null,
                                                      p_nSecuencia  => l_seq);
    -->>
    validar();

    r_certificado := pr_certificado.datos_cert(p_idepol, p_numcert);

    pr_interfase_ax_sas.incluir_deducible_cobert(p_nidepol                => p_idepol,
                                                 p_nnumcert               => p_numcert,
                                                 --<I RTC 317178> / Christian Bayona / 27-06-2023 / Error en la obtenci�n de datos para impresi�n para los casos con varios responsables de pago no est� obteniendo todos los datos requeridos
                                                 -- p_dfecinivig             => r_certificado.fecing,
                                                 -- p_dfecfinvig             => r_certificado.fecfin,
                                                 p_dfecinivig             => NVL(p_fecini, r_certificado.fecing),
                                                 p_dfecfinvig             => NVL(p_fecfin, r_certificado.fecfin),
                                                 --<F RTC 317178>
                                                 p_tabla_deducible_cobert => p_deducibles,
                                                 p_ctxtadicional          => p_txtadicional,
                                                 p_ccodusuario            => p_codusuario,
                                                 p_cursormsgvalida        => p_mensajes, --<<
                                                 p_cerror                 => p_error); --<<
    --<<

    if p_error <> c_ok then
      raise_application_error(-20100, 'incluir_deducible_cobert: ' || p_error);
    end if;

    pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax(p_cAccion     => 'F',
                                                      p_nIdePol     => p_idepol,
                                                      p_nNumcert    => null,
                                                      p_nNumOper    => null,
                                                      p_cTipoMov    => 'LOG',
                                                      p_cTipoEnd    => 'OK',
                                                      p_dFecIniProc => null,
                                                      p_dFecFinProc => sysdate,
                                                      p_cProceso    => 'modificar_deducible',
                                                      p_cSubProceso => null,
                                                      p_nSecuencia  => l_seq);

    p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
    p_error    := c_ok;

  exception
    when others then
      p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes;
      p_error    := '1,' || sqlerrm;
      pr_interfase_ax_sas_util.grabar_log_poliza_sas_ax(p_cAccion     => 'F',
                                                        p_nIdePol     => p_idepol,
                                                        p_nNumcert    => null,
                                                        p_nNumOper    => null,
                                                        p_cTipoMov    => 'LOG',
                                                        p_cTipoEnd    => 'ERR',
                                                        p_dFecIniProc => null,
                                                        p_dFecFinProc => sysdate,
                                                        p_cProceso    => 'modificar_deducible',
                                                        p_cSubProceso => null,
                                                        p_nSecuencia  => l_seq);
  end modificar_deducible;


/*-----------------------------------------------------------------------------
  Nombre      : Valida_Cobertura
  Proposito   : Funci�n que valida que exista un bien en una p�liza
  Referencias : <RTC-313752> Endosos Modificaci�n INCREMENTO de suma asegurada y sub l�mites
  Parametros  :
                p_nIdePol          NUMBER     Identificador de P�liza
                p_nNumCert         NUMBER     N�mero de Certificado
                p_cCodRamo         VARCHAR2   C�digo del Ramo
                p_cCodCobert       VARCHAR2   C�digo de la Cobertura
  Retorno     :                    VARCHAR2   Si es correcta 0,OK, caso contrario retorna un mensaje de error


  Log de Cambios
    Fecha         Autor                      Descripci�n
    22/08/2022    Christian Bayona           <RTC-314183> Creaci�n
  ----------------------------------------------------------------------------*/
  FUNCTION Valida_Cobertura (p_nIdePol IN Poliza.IdePol%TYPE,
                             p_nNumCert IN Certificado.NumCert%TYPE,
                             p_cCodRamo IN Cobert_Plan_Prod.CodRamoPlan%TYPE,
                             p_cCodCobert IN Cobert_Plan_Prod.CodCobert%TYPE)
                             RETURN VARCHAR2 IS
  --
  cCodProd    Producto.CodProd%TYPE;
  cCodPlan    Cobert_Plan_Prod.CodPlan%TYPE;
  cRevPlan    Cobert_Plan_Prod.RevPlan%TYPE;
  nSec        Log_Poliza_SAS_AX.Secuencia%TYPE;
  cError      VARCHAR2(2000);
  nVal        NUMBER(2);
  --
  BEGIN
    pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePol, NULL, NULL, 'LOG', NULL, SYSDATE, SYSDATE, 'ValCobertura', '', nSec);
    --
    cError := 'VAL-0050';
    SELECT CodProd
    INTO cCodProd
    FROM Poliza
    WHERE IdePol = p_nIdePol;
    --
    cError := 'VAL-0185';
    SELECT CodPlan, RevPlan
    INTO cCodPlan, cRevPlan
    FROM Cert_Ramo
    WHERE IdePol = p_nIdePol
      AND NumCert = p_nNumCert
      AND CodRamoCert = p_cCodRamo;
    --
    SELECT COUNT(1)
    INTO nVal
    FROM (
      SELECT CodCobert, TipoCobert
      FROM Cobert_Plan_Prod
      WHERE CodProd = cCodProd
        AND CodRamoPlan = p_cCodRamo
        AND CodPlan = cCodPlan
        AND RevPlan = cRevPlan
        AND CodCobert = p_cCodCobert
      UNION
      SELECT CodCobert, TipoCobert
      FROM Cobert_Plan_Pol
      WHERE IdePol = p_nIdePol
        AND CodProd = cCodProd
        AND CodRamoPlan = p_cCodRamo
        AND CodPlan = cCodPlan
        AND RevPlan = cRevPlan
        AND CodCobert = p_cCodCobert);
    --
    pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'OK', SYSDATE, SYSDATE, 'ValCobertura', '', nSec);
    --
    IF nVal > 0 THEN
      cError := '0,OK';
    ELSE
      cError := 'VAL-0069';
      cError := '1,' || pr_Interfase_AX_SAS_Util.Mensaje_Error(cError, p_cCodCobert, cCodPlan, cRevPlan);
    END IF;
    --
    RETURN cError;

  EXCEPTION
    WHEN OTHERS THEN
      pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'ValCobertura', '', nSec);
      cError := '1,' || pr_Interfase_AX_SAS_Util.Mensaje_Error(cError, p_cCodRamo);
      RETURN cError;
  END Valida_Cobertura;


/*-----------------------------------------------------------------------------
 Nombre     : EXISTE_NUMOPER
 Proposito  : Validar Existencia de Numero de operaci�n de Poliza
 Referencia :
 Parametro  : p_nIdePol     Id de la poliza
              p_nNumOper    Numero de Operaci�n

 Retorno :

 Log de Cambios
 Fecha        Autor               Descripcion
 -------      --------            --------------
 12/09/2022   Jorge Benel        <RTC 314951> Creaci�n.


 ----------------------------------------------------------------------------- */

  FUNCTION EXISTE_NUMOPER (p_nIdepol poliza.idepol%TYPE,
                           p_nNumoper oper_pol.numoper%TYPE) RETURN BOOLEAN IS

  vExiste INTEGER;
  nSec    INTEGER;

  BEGIN

    SELECT 1
    INTO vExiste
    FROM OPER_POL
    WHERE IDEPOL = p_nIdepol
    AND NUMOPER  = p_nNumoper
    AND ROWNUM < 2
    ;

    IF vExiste = 1 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END IF;

  EXCEPTION
    WHEN OTHERS THEN

      pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'EXISTE_NUMOPER', '', nSec);
      -- cError := '1,' || SQLERRM;
      RETURN FALSE;

  END EXISTE_NUMOPER;


/*-----------------------------------------------------------------------------
 Nombre     : Anular_Endoso
 Proposito  : Anulaci�n de Endoso
 Referencia :
 Parametro  :
              p_nNumOper    Numero de Operaci�n
              p_nIdePol     Id de la poliza
              p_cTextoAnexo Texto del Anexo por la anulaci�n

 Retorno :    p_cError            Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                      0,OK
                      1,Error

 Log de Cambios
 Fecha        Autor               Descripcion
 -------      --------            --------------
 12/09/2022   Jorge Benel        <RTC 314951> Creaci�n.


 ----------------------------------------------------------------------------- */

PROCEDURE ANULAR_ENDOSO(
  p_nIdepol          POLIZA.IdePol%TYPE,
  p_nNumOper         OPER_POL.NUMOPER%TYPE,
  p_cTextoAnexo      VARCHAR2 ,
  p_cCodUsuario      VARCHAR2,
  p_cursorMsgValida  OUT sys_refcursor ,
  p_cError           OUT VARCHAR2
  ) IS

  nNumPol         POLIZA.NUMPOL%TYPE;
  cStsPol         POLIZA.STSPOL%TYPE;
  cCodProd        POLIZA.CODPROD%TYPE;
  dFecIniVigPol   POLIZA.FECINIVIG%TYPE;
  dFecFinVigPol   POLIZA.FECFINVIG%TYPE;
  bExiste         BOOLEAN := TRUE;

  nSecuencia          LOG_POLIZA_SAS_AX.SECUENCIA%TYPE := 1;
  vp_TABLA_RESP_PAGO  ACSELX.TDU_TABLA_RESP_PAGO;

  p_tabla_anexo_pol   TDU_TABLA_ANEXO_POL;
  v_Texto_Anexo       VARCHAR2(2000);



FUNCTION LEE_ANEXO_OPERACION RETURN MOD_TEXTO_ANEXO_POL.TEXTOANEXO%TYPE AS
  cText MOD_TEXTO_ANEXO_POL.TEXTOANEXO%TYPE;
  BEGIN

    BEGIN
      SELECT TEXTOANEXO
      INTO   cText
      FROM   MOD_TEXTO_ANEXO_POL
      WHERE  IDEPOL =  ANULAR_ENDOSO.p_nIdepol
      AND    NUMOPER =  ANULAR_ENDOSO.p_nNumOper
      ;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        cText := '';
      WHEN TOO_MANY_ROWS THEN
        cText := '*** La consulta devuelve mas de un registro ***';
      WHEN OTHERS THEN
         cText := 'Error al leer el anexo de la operaci�n'|| sqlerrm;
    END;
    RETURN cText;

  END LEE_ANEXO_OPERACION;


BEGIN -- main
  p_cError:= '0,OK';
  --
  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('I',p_nIdepol,NULL,NULL,'LOG',NULL ,SYSDATE,SYSDATE,'Anular_Endoso','',nSecuencia );
  --

  -- Validaci�n de texto vacio
  IF p_cTextoAnexo IS NULL THEN p_cError := '1,Error en validaci�n del texto del anulaci�n, no deber ser vacio'; RETURN; END IF; --  <RTC 314185> Jorge Benel 07-12-2022
  -- Validacion del estado la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_POLIZA_XIDEPOL( p_nIdePol ,cStsPol , cCodProd , nNumPol, dFecIniVigPol , dFecFinVigPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de p�liza'; RETURN; END IF;
  -- Validacion del estado de la poliza
  bExiste := PR_INTERFASE_AX_SAS_UTIL.VALIDA_ESTADO_POLIZA_ENDOSO( cStsPol );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de estado de p�liza'; RETURN; END IF;
  -- Validacion del numero de operaci�n
  bExiste := EXISTE_NUMOPER(p_nIdePol, p_nNumOper );
  IF NOT bExiste THEN p_cError := '1,Error en validaci�n de existencia del numero de operaci�n'; RETURN; END IF;


  v_Texto_Anexo  := ANULAR_ENDOSO.p_cTextoAnexo;
  v_Texto_Anexo  := REPLACE(v_Texto_Anexo,'?nNumOper',TO_CHAR(Anular_Endoso.p_nNumOper));
  v_Texto_Anexo  := REPLACE(v_Texto_Anexo,'?cCuerpo',lee_anexo_operacion);

  p_tabla_anexo_pol := TDU_TABLA_ANEXO_POL( TDU_ANEXO_POL( codanexo     => 'SPECIF',
                                                           textoanexo   => v_Texto_Anexo,
                                                           TXTADICIONAL => '')
                                           );
  pr_interfase_ax_sas.grabar_anexo_poliza(p_nidepol         => ANULAR_ENDOSO.p_nIdePol,
                                          p_tabla_anexo_pol => p_tabla_anexo_pol,
                                          p_ccodusuario     => p_cCodUsuario,
                                          p_cursormsgvalida => p_cursorMsgValida,
                                          p_cerror          => p_cError
                                          );

  PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdepol,NULL,NULL,'LOG','OK' ,SYSDATE,SYSDATE,'Anular_Endoso','',nSecuencia );
  --

EXCEPTION
    WHEN OTHERS THEN
      p_cError := '1,'|| SQLERRM;
      PR_INTERFASE_AX_SAS_UTIL.GRABAR_LOG_POLIZA_SAS_AX('F',p_nIdepol,NULL,NULL,'LOG','ERR' ,SYSDATE,SYSDATE,'Anular_Endoso','',nSecuencia );
END ANULAR_ENDOSO;
  /*-----------------------------------------------------------------------------
  Nombre      : val_certificado
  Proposito   : Validaciones del Certificado
  Referencias : <RTC-314183> - Inclusion de Equipos TREC
  Parametros  :
                p_numcert    NUMBER    Identificador de Certificado

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/10/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure val_certificado(p_numcert certificado.numcert%type) is
  begin
    if p_numcert is null then
      -- 'El n�mero del certificado es obligatorio.'
      pr_interfase_ax_sas_util.insertar_log_errores('val_certificado', 'VAL-0048');
      raise_application_error(-20100, 'CERTIFICADO OBLIGATORIO');
    end if;
  end;
  /*-----------------------------------------------------------------------------
  Nombre      : val_ramo
  Proposito   : Validaciones del Ramo
  Referencias : <RTC-314183> - Inclusion de Equipos TREC
  Parametros  :
                p_idepol         NUMBER      Identificador de Poliza
                p_numcert        NUMBER      Identificador de Certificado
                p_codramocert    VARCHAR2    Codigo del Ramo

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/10/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure val_ramo(p_idepol      poliza.idepol%type,
                     p_numcert     certificado.numcert%type,
                     p_codramocert cert_ramo.codramocert%type) is
    l_existe pls_integer := 0;
  begin
    if p_codramocert is null then
      -- 'El Ramo es un dato obligatorio'
      pr_interfase_ax_sas_util.insertar_log_errores('val_ramo', 'VAL-1545');
      raise_application_error(-20100, 'RAMO OBLIGATORIO');
    end if;

    begin
      select 1
        into l_existe
        from dual
       where exists (select 1
                from poliza p, cert_ramo r, ramo_plan_prod c
               where p.idepol = p_idepol
                 and p.idepol = r.idepol
                 and p.codprod = c.codprod
                 and r.numcert = p_numcert
                 and r.codramocert = c.codramoplan
                 and r.codplan = c.codplan
                 and r.revplan = c.revplan
                 and c.codramoplan = p_codramocert);

    exception
      when no_data_found then
        -- 'El Ramo (:mx1) no pertenece al Certificado'
        pr_interfase_ax_sas_util.insertar_log_errores('val_criterio', 'VAL-1546', p_codramocert);
        raise_application_error(-20100, 'RAMO NO PERTENECE AL CERTIFICADO');
    end;
  end;
  /*-----------------------------------------------------------------------------
  Nombre      : val_criterio
  Proposito   : Validaciones del Criterio del Dato Particular
  Referencias : <RTC-314183> - Inclusion de Equipos TREC
  Parametros  :
                p_idepol         NUMBER      Identificador de Poliza
                p_numcert        NUMBER      Identificador de Certificado
                p_codramocert    VARCHAR2    Codigo del Ramo
                p_codcrit        VARCHAR2    Codigo del Criterio

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/10/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure val_criterio(p_idepol      poliza.idepol%type,
                         p_numcert     certificado.numcert%type,
                         p_codramocert cert_ramo.codramocert%type,
                         p_codcrit     criterio.codcrit%type) is
    l_existe pls_integer := 0;
  begin
    if p_codcrit is null then
      -- 'El Criterio es un dato obligatorio'
      pr_interfase_ax_sas_util.insertar_log_errores('val_criterio', 'VAL-1547');
      raise_application_error(-20100, 'CRITERIO OBLIGATORIO');
    end if;

    begin
      select 1
        into l_existe
        from dual
       where exists (select 1
                from poliza p, cert_ramo r, criterio_plan_prod c
               where p.idepol = p_idepol
                 and p.idepol = r.idepol
                 and p.codprod = c.codprod
                 and r.numcert = p_numcert
                 and r.codramocert = c.codramoplan
                 and r.codramocert = p_codramocert
                 and r.codplan = c.codplan
                 and r.revplan = c.revplan
                 and c.codcrit = p_codcrit);

    exception
      when no_data_found then
        -- 'El criterio (:mx1) no pertenece al Ramo (:mx2)'
        pr_interfase_ax_sas_util.insertar_log_errores('val_criterio',
                                                      'VAL-1548',
                                                      p_codcrit,
                                                      p_codramocert);
        raise_application_error(-20100, 'CRITERIO NO PERTENECE AL RAMO');
    end;
  end;
  /*-----------------------------------------------------------------------------
  Nombre      : val_vigencia_endoso
  Proposito   : Validaciones de la Vigencia del Endoso
  Referencias : <RTC-314183> - Inclusion de Equipos TREC
  Parametros  :
                p_idepol         NUMBER      Identificador de Poliza
                p_numcert        NUMBER      Identificador de Certificado
                p_codramocert    VARCHAR2    Codigo del Ramo
                p_fec_ini_end    DATE        Fecha Inicio de Endoso
                p_fec_fin_end    DATE        Fecha Fin de Endoso

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/10/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure val_vigencia_endoso(p_idepol      poliza.idepol%type,
                                p_numcert     certificado.numcert%type,
                                p_codramocert cert_ramo.codramocert%type,
                                p_fec_ini_end date,
                                p_fec_fin_end date) is
    r_cert certificado%rowtype;
    r_ramo cert_ramo%rowtype;
  begin
    if p_fec_ini_end is null then
      -- 'La fecha inicio del Endoso no puede ser nulo'
      pr_interfase_ax_sas_util.insertar_log_errores('val_vigencia_endoso', 'VAL-1549');
      raise_application_error(-20100, 'FEC_INI_END OBLIGATORIO');
    end if;

    if p_fec_fin_end is null then
      -- 'La fecha fin del Endoso no puede ser nulo'
      pr_interfase_ax_sas_util.insertar_log_errores('val_vigencia_endoso', 'VAL-1550');
      raise_application_error(-20100, 'FEC_FIN_END OBLIGATORIO');
    end if;

    r_cert := pr_certificado.datos_cert(p_idepol, p_numcert);
    r_ramo := pr_cert_ramo.datos_cert_ramo(p_idepol, p_numcert, p_codramocert);

    if p_fec_ini_end < trunc(r_cert.fecing) then
      -- 'La fecha inicio del Endoso no puede ser menor a la fecha de inicio del Certificado'
      pr_interfase_ax_sas_util.insertar_log_errores('val_vigencia_endoso', 'VAL-1551');
      raise_application_error(-20100, 'FECHA FUERA DE VIGENCIA');
    end if;

    if p_fec_ini_end >= trunc(r_ramo.fecfinvalid) then
      -- 'La fecha inicio del Endoso no puede ser mayor o igual a la fecha de fin del Ramo'
      pr_interfase_ax_sas_util.insertar_log_errores('val_vigencia_endoso', 'VAL-1552');
      raise_application_error(-20100, 'FECHA FUERA DE VIGENCIA');
    end if;

    if p_fec_fin_end <= trunc(r_cert.fecing) then
      -- 'La fecha fin del Endoso no puede ser menor o igual a la fecha de inicio del Certificado'
      pr_interfase_ax_sas_util.insertar_log_errores('val_vigencia_endoso', 'VAL-1553');
      raise_application_error(-20100, 'FECHA FUERA DE VIGENCIA');
    end if;

    if p_fec_fin_end > trunc(r_ramo.fecfinvalid) then
      -- 'La fecha fin del Endoso no puede ser mayor a la fecha de fin del Ramo'
      pr_interfase_ax_sas_util.insertar_log_errores('val_vigencia_endoso', 'VAL-1554');
      raise_application_error(-20100, 'FECHA FUERA DE VIGENCIA');
    end if;
  end;
  /*-----------------------------------------------------------------------------
  Nombre      : val_mto_operacion
  Proposito   : Validaciones del Monto de Operacion
  Referencias : <RTC-314183> - Inclusion de Equipos TREC
  Parametros  :
                p_mtooper     NUMBER        Monto a Cobrar por Recargo/Descuento
                p_proceso     VARCHAR2      Identificador de Proceso:
                                                (I): Inclusion
                                                (E): Exclusion
                p_mensajes    REF CURSOR    Mensajes de error
                p_error       VARCHAR2      Mensaje de error

  Log de Cambios
    Fecha         Autor                      Descripcion
    11/10/2022    Mauro Zegarra              Creacion
  -----------------------------------------------------------------------------*/
  procedure val_mto_operacion(p_mtooper  oper_pol.mtooper%type,
                              p_proceso  varchar2,
                              p_mensajes out sys_refcursor,
                              p_error    out varchar2) is
    c_inclusion constant varchar2(1) := 'I';
    c_exclusion constant varchar2(1) := 'E';
  begin
    p_error := '0,OK';

    if p_proceso = c_inclusion and p_mtooper < 0 then
      -- 'El monto de operacion debe ser positivo'
      pr_interfase_ax_sas_util.insertar_log_errores('val_mto_positivo', 'VAL-1555');
      p_error := '1, Monto Negativo';
    end if;

    if p_proceso = c_exclusion and p_mtooper > 0 then
      -- 'El monto de operacion debe ser negativo'
      pr_interfase_ax_sas_util.insertar_log_errores('val_mto_negativo', 'VAL-1556');
      p_error := '1, Monto Positivo';
    end if;

    p_mensajes := pr_interfase_ax_sas.mostrar_cursor_mensajes();
  end;


/*------------------------------------------------------------------------
  Nombre     : Actualizar_Cert_Relacionados
  Prop�sito  : Actualizar las fechas de inicio y fin de vigencia los relacionados despues
               de hacer la copia de un certificado.
  Referencias: <RTC-314184> Endosos de Declaraci�n de existencias tipo "A y B"
  Par�metros :
                 p_nIdePolOrig     NUMBER       Identificador de P�liza
                 p_nNumCertOrig    NUMBER       N�mero de Certificado
                 p_dFecIni         DATE         Fecha de Inicio del Certificado
                 p_dFecFin         DATE         Fecha de Fin del Certificado
                 p_nNumCertMax     NUMBER       N�mero M�ximo del Certificado
  Retorno    :

  Log de Cambios:
    Fecha         Autor               Descripci�n
    12/08/2022    Christian Bayona    Creaci�n
 -----------------------------------------------------------------------------*/
PROCEDURE Actualizar_Cert_Relacionados (p_nIdePolOrig  IN Poliza.IdePol%TYPE,
                                        p_nNumCertOrig IN Certificado.NumCert%TYPE,
                                        p_dFecIni      IN Certificado.FecIng%TYPE,
                                        p_dFecFin      IN Certificado.FecFin%TYPE,
                                        p_nNumCertMax  IN Certificado.NumCert%TYPE) IS

cTipoRecaDcto   Reca_Dcto_Certif.TipoRecaDcto%TYPE;
Ccodrecadcto    Reca_Dcto_Certif.CodRecaDcto%TYPE;
nSec            Log_Poliza_SAS_AX.Secuencia%TYPE;

BEGIN
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePolOrig, NULL, NULL, 'LOG', NULL, SYSDATE, SYSDATE, 'ActCertRelacionado', '', nSec);
  --
  UPDATE Endosatario_Cert
  SET FecIng = TRUNC(SYSDATE),
    FecIniVig = p_dFecIni,
    FecFinVig = p_dFecFin
  WHERE IdePol  = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  BEGIN
    SELECT TipoRecaDcto, CodRecaDcto
    INTO cTipoRecaDcto, cCodRecaDcto
    FROM Reca_Dcto_Certif
    WHERE IdePol  = p_nIdePolOrig
      AND NumCert = p_nNumCertOrig;

  EXCEPTION
    WHEN OTHERS THEN
      cTipoRecaDcto:= '';
      cCodRecaDcto:= '';
  END;

  UPDATE Reca_Dcto_Certif
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol = p_nIdePolOrig
    AND NumCert > p_nNumCertMax
    AND TipoRecaDcto = cTipoRecaDcto
    AND CodRecaDcto = cCodRecaDcto;

  UPDATE Cert_Ramo
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  UPDATE Anexo_Cert
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  UPDATE Cobert_Cert
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol  = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  UPDATE Deducible_Cobert_Grupo
  SET FecIniVig = p_dFecIni,
    FecFinVig = p_dFecFin
  WHERE IdeCobert IN (SELECT IdeCobert
                      FROM Cobert_Cert
                      WHERE IdePol  = p_nIdePolOrig
                        AND NumCert > p_nNumCertMax);

  UPDATE Est_Cert
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol  = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  UPDATE Clau_Cert
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol  = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  UPDATE Bien_Cert
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol  = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  UPDATE Cobert_Bien
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdeCobert IN (SELECT IdeCobert
                      FROM Cobert_Cert
                      WHERE IdePol  = p_nIdePolOrig
                        AND NumCert = p_nNumCertOrig);

  UPDATE Dat_Transporte_Flotante
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;

  BEGIN
    UPDATE Merca_Embala_Tasa
    SET FecIniTasa = p_dFecIni,
      FecFinTasa = p_dFecFin
    WHERE IdePol = p_nIdePolOrig
      AND NumCert > p_nNumCertMax;

  EXCEPTION
    WHEN DUP_VAL_ON_INDEX THEN
      DECLARE
        CURSOR cMerca_Embala_Tasa IS
          SELECT CodRamoCert, TipoMercancia, TipoEmbalaje, TipoTransporte, CodContinente, MAX(FecProxRev) Fecha
          FROM Merca_Embala_Tasa
          WHERE IdePol  = p_nIdePolOrig
            AND NumCert = p_nNumCertOrig
          GROUP BY CodRamoCert, TipoMercancia, TipoEmbalaje, TipoTransporte, CodContinente
          ORDER BY TipoMercancia;
        --
        dFecha   DATE;
        --
      BEGIN
        FOR reg IN cMerca_Embala_Tasa LOOP
          EXIT WHEN cMerca_Embala_Tasa%NOTFOUND;
          --
          dFecha := reg.Fecha;
          UPDATE Merca_Embala_Tasa m
          SET m.FecIniTasa = p_dFecIni,
            m.FecFinTasa = p_dFecFin
          WHERE m.IdePol        = p_nIdePolOrig
            AND m.CodRamoCert   = reg.CodRamoCert
            AND m.TipoMercancia = reg.TipoMercancia
            AND m.TipoEmbalaje  = reg.TipoEmbalaje
            AND m.TipoTransporte = reg.TipoTransporte
            AND m.CodContinente = reg.CodContinente
            AND m.FecProxRev    = reg.Fecha
            AND NumCert         > p_nNumCertMax;
        END LOOP;
        --
        DELETE Merca_Embala_Tasa m
        WHERE m.IdePol     = p_nIdePolOrig
          AND m.NumCert    > p_nNumCertMax
          AND m.FecProxRev <> dfecha;
      END;

    WHEN OTHERS THEN
      pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePolOrig, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'ActCertRelacionado', '', nSec);
      RAISE_APPLICATION_ERROR(-20100, 'Actualizar_Cert_Relacionados - ' || SQLERRM);
  END;

  UPDATE Mod_Datos_Particulares
  SET FecIniValid = p_dFecIni,
    FecFinValid = p_dFecFin
  WHERE IdePol = p_nIdePolOrig
    AND NumCert > p_nNumCertMax;
  --
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePolOrig, NULL, NULL, 'LOG', 'OK', SYSDATE, SYSDATE, 'ActCertRelacionado', '', nSec);
END Actualizar_Cert_Relacionados;


/*------------------------------------------------------------------------
  Nombre     : CopiaCertificado
  Prop�sito  : Procedimiento para hacer la copia de un certificado.
  Referencias: <RTC-314184> Endosos de Declaraci�n de existencias tipo "A y B"
  Par�metros :
                 p_nIdePol         NUMBER       Identificador de P�liza
                 p_nNumCert        NUMBER       N�mero de Certificado
                 p_cCodCli         VARCHAR2     C�digo del Cliente
                 p_dFecIniVig      DATE         Fecha de Inicio de Vigencia
                 p_dFecFinVig      DATE         Fecha de Fin de Vigencia
                 p_nNumCopias      NUMBER       N�mero de copias del Certificado
  Retorno    :   p_nNewNumCert     NUMBER       N�mero de Certificado inicial generedo
                 p_cError          VARCHAR2     Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripci�n
    10/08/2022    Christian Bayona    Creaci�n
 -----------------------------------------------------------------------------*/
PROCEDURE CopiaCertificado(
  p_nIdePol IN Poliza.IdePol%TYPE,
  p_nNumCert IN Certificado.NumCert%TYPE,
  p_cCodCli IN Cliente.CodCli%TYPE,
  p_dFecIniVig IN DATE,
  p_dFecFinVig IN DATE,
  p_nNumCopias IN NUMBER DEFAULT 1,
  --
  p_nNewNumCert OUT NUMBER,
  p_cError IN OUT VARCHAR2) IS
--
  nNumCertMax      Certificado.NumCert%TYPE;
  nSec            Log_Poliza_SAS_AX.Secuencia%TYPE;
--
BEGIN
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('I', p_nIdePol, NULL, NULL, 'LOG', NULL, SYSDATE, SYSDATE, 'CopiaCertificado', '', nSec);
  --
  p_cError := '0,OK';
  --
  IF p_dFecIniVig IS NULL THEN
    p_cError := '1,Fecha Inicio de Vigencia no puede ser nula';
    RETURN;
  END IF;
  --
  IF p_dFecFinVig IS NULL THEN
    p_cError := '1,Fecha Fin de Vigencia no puede ser nula';
    RETURN;
  END IF;
  --
  IF p_cCodCli IS NULL THEN
    p_cError := '1,Cliente/Asegurado no puede ser nulo';
    RETURN;
  END IF;
  --
  SELECT NVL(MAX(NumCert), 9999999999)
  INTO nNumCertMax
  FROM Certificado
  WHERE IdePol = p_nIdePol;
--
  BEGIN
    pr_Certificado.Duplica_Certificado(p_nIdePol, p_nNumCert, p_nNumCopias,
                                       p_nIdePol, NULL,
                                       p_dFecIniVig, p_dFecFinVig, p_cCodCli);
    p_nNewNumCert := nNumCertMax + 1;

  EXCEPTION
    WHEN OTHERS THEN
      p_cError := '1,Error en Duplica_Certificado - (' || SQLCODE || '-' || SQLERRM || ')';
      pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'CopiaCertificado', '', nSec);
      RETURN;
  END;
--
  BEGIN
    pr_Interfase_AX.Actualizar_Cert_Relacionados(p_nIdePol, p_nNumCert, p_dFecIniVig, p_dFecFinVig, nNumCertMax);

  EXCEPTION
    WHEN OTHERS THEN
      p_cError := '1,Error en Actualizar_Cert_Relacionados - (' || SQLCODE || '-' || SQLERRM || ')';
      pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'ERR', SYSDATE, SYSDATE, 'CopiaCertificado', '', nSec);
  END;
  --
  pr_Interfase_AX_SAS_Util.Grabar_Log_Poliza_SAS_AX('F', p_nIdePol, NULL, NULL, 'LOG', 'OK', SYSDATE, SYSDATE, 'CopiaCertificado', '', nSec);
END CopiaCertificado;


/*------------------------------------------------------------------------
  Nombre     : Existe_Cert_DE_Tipo_B
  Prop�sito  : Funci�n que valida que existe un certificado existente de Tipo B (vigencia mensual).
  Referencias: <RTC-314184> Endosos de Declaraci�n de existencias tipo "A y B"
  Par�metros :
                 p_nIdePol         NUMBER       Identificador de P�liza
                 p_nNumCert        NUMBER       N�mero de Certificado
                 p_cStsCert        VARCHAR2     Estado del Certificado
  Retorno    :                     NUMBER       N�mero de Certificado con vigencia mensual; -1 Si error

  Log de Cambios:
    Fecha         Autor               Descripci�n
    14/09/2022    Christian Bayona    Creaci�n
 -----------------------------------------------------------------------------*/
FUNCTION Existe_Cert_DE_Tipo_B(p_nIdePol IN Poliza.IdePol%TYPE,
                               p_nNumCert IN Certificado.NumCert%TYPE DEFAULT NULL,
                               p_cStsCert IN VARCHAR2 DEFAULT 'ACT')
                               RETURN NUMBER IS
--
nNumCert Certificado.NumCert%TYPE;
--
BEGIN
  SELECT NVL(MAX(NumCert), -1)
  INTO nNumCert
  FROM Certificado
  WHERE IdePol = p_nIdePol
    AND NumCert = NVL(p_nNumCert, NumCert)
    AND StsCert = NVL(p_cStsCert, 'ACT')
    AND (TRUNC(FecFin) - TRUNC(FecIng)) BETWEEN 28 AND 31;
  --
  RETURN nNumCert;
END Existe_Cert_DE_Tipo_B;


/*------------------------------------------------------------------------
  Nombre     : CodAutorizador
  Prop�sito  : Funci�n que devuelve el c�digo del usuario autorizador
  Referencias: <RTC-316106> Actualizaci�n Endoso Modificaci�n de Locales
  Par�metros :
                 p_nIdePol         NUMBER       Identificador de P�liza
                 p_nNumOper        NUMBER       N�mero de Certificado
  Retorno    :                     VARCHAR2     C�digo de Usuario

  Log de Cambios:
    Fecha         Autor               Descripci�n
    31/01/2023    Christian Bayona    Creaci�n
 -----------------------------------------------------------------------------*/
FUNCTION CodAutorizador(p_nIdePol POLIZA.IdePol%TYPE,
                        p_nNumOper OPERACION.IdeOp%TYPE)
RETURN USUARIO.CodUsr%TYPE IS
--
cAutorizadoPor  Autoriza_Violacion.AutorizadoPor%TYPE;
cCodUsr         Usuario.CodUsr%TYPE;
cCodCargo       Usuario.CodCargo%TYPE;
nNumUsr         NUMBER(5) := 0;
--
CURSOR c_Autoriza_Violacion IS
  SELECT DISTINCT AutorizadoPor
  FROM Autoriza_Violacion
  WHERE IdePol  = p_nIdePol
    AND NumOper = p_nNumOper;
--
CURSOR c_Usuario IS
  SELECT CodCargo, CodUsr
  FROM Usuario
  WHERE CodUsr = cAutorizadoPor;
--
BEGIN
  FOR i IN c_Autoriza_Violacion LOOP
    cAutorizadoPor := i.AutorizadoPor;
    nNumUsr        := nNumUsr + 1;
    --
    FOR j IN c_Usuario LOOP
      IF nNumUsr = 1 THEN
        cCodUsr   := j.codusr;
        cCodCargo := j.CodCargo;
      ELSE
        IF j.CodCargo IS NOT NULL THEN
          IF cCodCargo IS NULL OR j.CodCargo < cCodCargo THEN
            cCodUsr   := j.codusr;
            cCodCargo := j.CodCargo;
          END IF;
        END IF;
      END IF;
    END LOOP;
  END LOOP;

  RETURN (cCodUsr);
END CodAutorizador;


/*------------------------------------------------------------------------
  Nombre     : Obtener_Firma
  Prop�sito  : Procedimiento que devuelve la firma en base al reporte indicado
  Referencias: <RTC-316449> Endoso Declaraci�n de existencias tipo "A y B"

  Par�metros :
                 p_nIdePol         NUMBER       Identificador de P�liza
                 p_cNomRpt         VARCHAR2     N�mero de Certificado
                 p_cCodFirma       VARCHAR2     C�digo de Firma
  Retorno    :   p_cFirma          CLOB         Firma obtenia en BASE64

  Log de Cambios:
    Fecha         Autor               Descripci�n
    17/02/2023    Christian Bayona    Creaci�n
    22/02/2023    Christian Bayona    Modificaci�n, se agrega nuevo par�metro
 -----------------------------------------------------------------------------*/
PROCEDURE Obtener_Firma(p_nIdePol IN Poliza.IdePol%TYPE,
                        p_cNomRpt IN VARCHAR2,
                        p_cCodFirma IN VARCHAR2, --<RTC 316449> / CBayona / 22-02-2023 / Se agrega nuevo par�metro
                        --
                        p_cFirma OUT NOCOPY CLOB) IS
--
lrFirma  Long Raw;
bFirma   BLob;
--
BEGIN
  IF p_cNomRpt IS NOT NULL THEN
    --<I RTC 316449> / CBayona / 22-02-2023 / Se obtiene la firma para la LQ
    IF p_cCodFirma IS NOT NULL THEN
      BEGIN
        SELECT ua.Firma
        INTO lrFirma
        FROM Usuario_Autoriza ua,
          LVal l
        WHERE ua.CodUsu = l.CodLVal
          AND l.CodLVal = DECODE(p_cCodFirma, '01', 'MDIAZR', '02', 'JCHACONL', '03', 'CSEMINAS')
          AND ua.CodAut = DECODE(p_cCodFirma, '01', '73', '02', '74', '03', '01')
          AND ua.StsAut = 'ACT'
          AND l.TipoLVal = 'FIRPCONV';
        --
        bFirma := TO_BLOB(lrFirma);
        p_cFirma := UTL_RAW.Cast_to_Varchar2(UTL_ENCODE.Base64_Encode(bFirma));

      EXCEPTION
        WHEN OTHERS THEN
          NULL;
      END;

    ELSE
      BEGIN
        SELECT fus.firma IMGFIrma
        INTO lrFirma
        FROM Firmas_Usuarios fus,
          Firmas_Usuarios_Reportes fur
        WHERE fus.NumId = fur.NumId
          AND fur.CodAplic = p_cNomRpt
          AND fur.Estado = '1'
          AND ROWNUM = 1
          AND fur.CodProdRela IN (
            SELECT DECODE(pro.CodProdRela, 'VIDA',
              DECODE(pro.Grupo, '0061', 'ACP', pro.CodProdRela),
                pro.CodProdRela)
              FROM Poliza pol,
               Producto pro
             WHERE pol.CodProd = pro.CodProd
               AND pol.IdePol = p_nIdePol);
        --
        bFirma := TO_BLOB(lrFirma);
        p_cFirma := UTL_RAW.Cast_to_Varchar2(UTL_ENCODE.Base64_Encode(bFirma));

      EXCEPTION
        WHEN OTHERS THEN
          NULL;
      END;
    END IF;
    --<F RTC 316449>
    --
    lrFirma := NULL;
    bFirma := NULL;
  END IF;
END Obtener_Firma;


/*------------------------------------------------------------------------
  Nombre     : Corregir_Coberturas
  Prop�sito  : Procedimiento que corrige las coberturas que no debieron ser movidas por los datos particulares (simil a lo que hace el emisor)
  Referencias: <RTC-316878> CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaraci�n de Existencias Tipo B

  Par�metros :
                 p_nIdePol         NUMBER       Identificador de P�liza
                 p_cNumCert        NUMBER       N�mero de Certificado
  Retorno    :

  Log de Cambios:
    Fecha         Autor               Descripci�n
    12/05/2023    Christian Bayona    Creaci�n
 -----------------------------------------------------------------------------*/
PROCEDURE Corregir_Coberturas (p_nIdePol IN Poliza.IdePol%TYPE, p_nNumCert IN Certificado.NumCert%TYPE) IS
--
CURSOR c_Coberts IS
  SELECT IdeCobert, NumMod
  FROM Mod_Cobert mc
  WHERE IdePol = p_nIdePol
    AND NumCert = p_nNumCert
    AND StsModCobert = 'INC';
--
bFlag BOOLEAN := FALSE;
--
BEGIN
  FOR r IN c_Coberts LOOP
    pr_Mod_Cobert.Revertir(r.IdeCobert, r.NumMod);
    pr_Cobert_Cert.Revertir(r.IdeCobert);
    --
    bFlag := TRUE;
  END LOOP;
  --
  IF bFlag THEN
    DELETE Mod_Cobert
    WHERE IdePol = p_nIdePol
      AND NumCert = p_nNumCert
      AND StsModCobert = 'VAL';
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    DBMS_OUTPUT.Put_Line('>>> Error Corregir_Coberturas ' || SQLERRM);
END Corregir_Coberturas;


/*-----------------------------------------------------------------------
   Nombre     : Endoso_Traslado_Bienes
   Proposito  : Endoso de traslado de bienes
   Referencia : Formulario MANTCERT.fmb

   Parametro  :
        p_nIdePol                  Id de la poliza
        p_cCodProd                 Codigo de producto
        p_cCodMoneda               Codigo de moneda
        P_TDU_Tabla_Imp_Local      Tabla de locales a cargar
        p_tTabla_BienesxCert       Tabla de bienes por certificado
        p_Tabla_Bien_Cert_Riesgo   Tabla de direcciones de riesgo

   Retorno :       p_cError        Valor de retorno del proceso. En caso de error se concatena el mensaje de error.
                                    0,OK
                                    1,Error

   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   07/07/2022   Christian Bayona    <RTC 317178> Error en la obtenci�n de datos para impresi�n para los casos con varios responsables de pago no est� obteniendo todos los datos requeridos
   21/09/2023   Manuel Navarro      <DIGB2B03-2815> Inclusi�n Locales Legacy Dubai Masivo
-----------------------------------------------------------------------------*/
PROCEDURE Endoso_Traslado_Bienes ( p_nIdePol                IN Poliza.IdePol%TYPE,
                                   p_cCodProd               IN Poliza.CodProd%TYPE,
                                   p_cCodMoneda             IN Poliza.CodMoneda%TYPE,
                                   P_TDU_Tabla_Imp_Local    IN ACSELX.TDU_Tabla_Imp_Local,
                                   p_tTabla_BienesxCert     IN ACSELX.TDU_T_Tabla_BienesxCert,
                                   p_Tabla_Bien_Cert_Riesgo IN ACSELX.tDU_Tabla_Bien_Cert_Riesgo,
                                   -- Variables de retorno
                                   p_cError                OUT VARCHAR2) AS
  nCant NUMBER(2);
  nVal  NUMBER(1);

FUNCTION Recepcionar_Archivo RETURN NUMBER IS
  nNumLinea          NUMBER;
  nNumBien           NUMBER;
  bVal               BOOLEAN;
  nMonto             NUMBER(38, 20);
  nMtoTotal          NUMBER(38, 20);
  cCad               VARCHAR2(4000);
  cClaseTipo         VARCHAR2(7);
  cMensaje           VARCHAR2(4000);
  nIdeDirec          Direc_Riesgo_Cert.IdeDirec%TYPE;
  nIdeDirec_aux      Direc_Riesgo_Cert.IdeDirec%TYPE;
  cClaseBien         Bien_Cert.ClaseBien%TYPE;
  cCodBien           Bien_Cert.CodBien%TYPE;
  cIndBien           Ramo_Plan_Prod.IndBen%TYPE;
  nNumcert           Certificado.NumCert%TYPE;
  nNumcertOld        Certificado.NumCert%TYPE;
  cStsCert           Certificado.StsCert%TYPE;
  nNumcertSig        Certificado.NumCert%TYPE;
  nMtoValDeclarDirec Direc_Riesgo_Cert.MtoValDeclarDirec%TYPE;
  nAlerta            NUMBER;
  --
  rdirec            Direc_Riesgo_Cert%ROWTYPE;
  dFecIniVig        Certificado.FecIng%TYPE;
  dFecFinVig        Certificado.FecFin%TYPE;
  cIndBienValDeclar VARCHAR2(1);
  Aux_TDU_Tabla_Imp_Local   acselx.TDU_Tabla_Imp_Local;
  i                 INTEGER;
  c                 INTEGER;
  nCuentaLoc2       INTEGER;
  nExiste           INTEGER;
  nCuentaLoc        INTEGER;
  nMTotal           Direc_Riesgo_Cert.MtoValDeclarDirec%TYPE;
  nPorcLoc          Direc_Riesgo_Cert.PorcSuma%TYPE;
  nPorcLocAcu       Direc_Riesgo_Cert.PorcSuma%TYPE;
  nDif              Direc_Riesgo_Cert.PorcSuma%TYPE;

  CURSOR c_Cert(p_nNumcert NUMBER) IS
    SELECT NumCert, StsCert
    FROM Certificado
    WHERE IdePol = p_nIdePol
      AND NumCert = p_nNumcert
      AND StsCert NOT IN ('ACT', 'ANU', 'EXC', 'NRE', 'NSR', 'REN');

  CURSOR c_Ramos IS
    SELECT CodPlan,
      RevPlan,
      CodRamoCert,
      FecIniValid,
      FecFinValid,
      StsCertRamo
    FROM Cert_Ramo
    WHERE IdePol = p_nIdePol
      AND NumCert = nNumcert
     ORDER BY NumOrdVis;

  CURSOR c_Direcciones(p_nNumCert NUMBER) IS
    SELECT IdeDirec, IdePol, NumCert, StsDirec
    FROM Direc_Riesgo_Cert
    WHERE IdePol = p_nIdePol
      AND NumCert = p_nNumCert;

  CURSOR c_Bien(p_nNumCert Bien_Cert.NumCert%Type) Is
    SELECT IdeBien
    FROM Bien_Cert
    WHERE IdePol = p_nIdePol
      AND NumCert = p_nNumCert
      AND StsBien = 'VAL';

BEGIN
  nNumCert      := 0;
  p_cError      := '0,OK';
  nIdeDirec_Aux := 0;

    BEGIN
      nNumLinea := 0;
      nNumBien  := 0;
      nMtoTotal := 0;
      --
      BEGIN
        nCuentaLoc := P_TDU_Tabla_Imp_Local.COUNT;
      EXCEPTION
        WHEN OTHERS THEN
          nCuentaLoc := 0;
      END;
      --
      IF P_TDU_Tabla_Imp_Local.COUNT > 0 THEN
        i := nCuentaLoc;
        AUX_TDU_Tabla_Imp_Local := P_TDU_Tabla_Imp_Local;

        FOR c1 in (SELECT *
                   FROM Direc_Riesgo_Cert
                   WHERE IdePol = p_nIdePol
                     AND (StsDirec <> 'EXC' OR StsDirec <> 'ANU' )) LOOP
          BEGIN
            SELECT 1
            INTO nExiste
            FROM TABLE(P_TDU_Tabla_Imp_Local)
            WHERE IdeDirec = c1.IdeDirec;

          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              i := i + 1;
              AUX_TDU_TABLA_Imp_Local.EXTEND(1);
              AUX_TDU_TABLA_Imp_Local(i) := acselx.TDU_Imp_Local(
                Codpais => NULL,          Codestado   => NULL,      Codciudad  =>  NULL,     Codmunicipio  => NULL,
                Ubigeo  => NULL,          Tipovia     => NULL,       Nomvia     => NULL,     Numvia        => NULL,
                Tipoint => NULL,          Numint      => NULL,       Manzana    => NULL,     Lote          => NULL,
                Kilome  => NULL,          Tipocha     => NULL,       Nomcha     => NULL,     Tiposdi       => NULL,
                Nomsdi  => NULL,          Referencia  => NULL,       Resto      => NULL,     Porcsuma      => NULL,
                Idegiro => NULL,          Mtovaldeclardirec => NULL, Tep            => NULL, Tee           => NULL,
                Anoconstrucc   => NULL,   Numpisos  => NULL,         Numsotanos     => NULL, Fecinspeccion => NULL,
                Numcert         => NULL,  Idedirec  => NULL,         Indriesgoins   => NULL, Indinspeccion => NULL,
                Notainspeccion  => NULL,  Indsininspeccion => NULL,  Usuautorizador => NULL, Clasetipo     => NULL,
                Propieedificacion  => NULL
                --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
                ,USOINMUEBLE => NULL, 
                edificio => NULL, edificOficina => NULL, edificotro1 => NULL, edificotro2 => NULL, edificotro3 => NULL,
                contExistencias => NULL, contMaquinFija => NULL, contMaqMovilAcces => NULL, contEquipoElec => NULL, contMobiliarioGral => NULL, 
                contOtro1 => NULL, contOtro2 => NULL, contOtro3 => NULL, contOtro4 => NULL, contOtro5 => NULL, contOtro6 => NULL,
                benefBruto => NULL,
                gastosEstable => NULL
                ) ;
                --<F DIGB2B03-2815> 


              AUX_TDU_Tabla_Imp_Local(i).CodPais           := c1.CodPais;
              AUX_TDU_Tabla_Imp_Local(i).CodEstado         := c1.CodEstado;
              AUX_TDU_Tabla_Imp_Local(i).CodCiudad         := c1.CodCiudad;
              AUX_TDU_Tabla_Imp_Local(i).CodMunicipio      := c1.CodMunicipio;
              AUX_TDU_Tabla_Imp_Local(i).UbiGeo            := c1.UbiGeo;
              AUX_TDU_Tabla_Imp_Local(i).Tipovia           := c1.TipOvia;
              AUX_TDU_Tabla_Imp_Local(i).NomVia            := c1.NomVia;
              AUX_TDU_Tabla_Imp_Local(i).NumVia            := c1.NumVia;
              AUX_TDU_Tabla_Imp_Local(i).TipoInt           := c1.TipoInt;
              AUX_TDU_Tabla_Imp_Local(i).NumInt            := c1.NumInt;
              AUX_TDU_Tabla_Imp_Local(i).Manzana           := c1.Manzana;
              AUX_TDU_Tabla_Imp_Local(i).Lote              := c1.Lote;
              AUX_TDU_Tabla_Imp_Local(i).Kilome            := c1.Kilome;
              AUX_TDU_Tabla_Imp_Local(i).TipoCha           := c1.TipoCha;
              AUX_TDU_Tabla_Imp_Local(i).NomCha            := c1.NomCha;
              AUX_TDU_Tabla_Imp_Local(i).TipoSdi           := c1.Tiposdi;
              AUX_TDU_Tabla_Imp_Local(i).NomSdi            := c1.NomSdi;
              AUX_TDU_Tabla_Imp_Local(i).Referencia        := c1.Referencia;
              AUX_TDU_Tabla_Imp_Local(i).Resto             := c1.Resto;
              AUX_TDU_Tabla_Imp_Local(i).PorcSuma          := c1.PorcSuma;
              AUX_TDU_Tabla_Imp_Local(i).IdeGiro           := c1.IdeGiro;
              AUX_TDU_Tabla_Imp_Local(i).MtoValDeclarDirec := c1.MtoValDeclarDirec;
              AUX_TDU_Tabla_Imp_Local(i).Tep               := c1.Tep;
              AUX_TDU_Tabla_Imp_Local(i).Tee               := c1.Tee;
              AUX_TDU_Tabla_Imp_Local(i).AnoConstrucc      := c1.AnoConstrucc;
              AUX_TDU_Tabla_Imp_Local(i).NumPisos          := c1.NumPisos;
              AUX_TDU_Tabla_Imp_Local(i).NumSotanos        := c1.NumSotanos;
              AUX_TDU_Tabla_Imp_Local(i).FecInspeccion     := c1.FecInspeccion;
              AUX_TDU_Tabla_Imp_Local(i).NumCert           := c1.NumCert;
              AUX_TDU_Tabla_Imp_Local(i).IdeDirec          := c1.IdeDirec;
              AUX_TDU_Tabla_Imp_Local(i).IndRiesgoins      := c1.IndRiesgoins;
              AUX_TDU_Tabla_Imp_Local(i).IndInspeccion     := c1.IndInspeccion;
              AUX_TDU_Tabla_Imp_Local(i).NotaInspeccion    := c1.NotaInspeccion;
              AUX_TDU_Tabla_Imp_Local(i).IndSinInspeccion  := c1.IndSinInspeccion;
              AUX_TDU_Tabla_Imp_Local(i).UsuAutorizador    := c1.UsuAutorizador;
              AUX_TDU_Tabla_Imp_Local(i).PropieEdificacion := c1.PropieEdificacion;
              
              --<I DIGB2B03-2815> / Manuel Navarro / 21-09-2023 / Inclusi�n Locales Legacy Dubai Masivo
              AUX_TDU_Tabla_Imp_Local(i).USOINMUEBLE := c1.USOINMUEBLE;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificio := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificOficina := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro1 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro2 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).edificotro3 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contExistencias := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMaquinFija := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMaqMovilAcces := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contEquipoElec := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contMobiliarioGral := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro1 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro2 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro3 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro4 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro5 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).contOtro6 := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).benefBruto := NULL;
              AUX_TDU_TABLA_IMP_LOCAL(i).gastosEstable := NULL;
              --<F DIGB2B03-2815> 
              
           END;
        END LOOP;

        BEGIN
          BEGIN
          SELECT SUM(MtoValDeclarDirec)
          INTO nMTotal
          FROM TABLE(AUX_TDU_TABLA_Imp_Local);

          EXCEPTION
            WHEN OTHERS THEN
              nMTotal:= 1.00;
          END;
        END;

        nPorcLocAcu := 0;
        c           := 0;

        BEGIN
          nCuentaLoc2 := AUX_TDU_Tabla_Imp_Local.COUNT;

        EXCEPTION
          WHEN OTHERS THEN
            nCuentaLoc2 := 0;
        END;

        FOR i in AUX_TDU_Tabla_Imp_Local.First .. AUX_TDU_Tabla_Imp_Local.Last LOOP
          ------------------------------------------
          -- PASO 1 - generar direccion de riesgo --
          ------------------------------------------
          c := c + 1;
          SELECT (AUX_TDU_Tabla_Imp_Local(i).MtoValDeclarDirec / nMTotal) * 100
          INTO nPorcLoc
          FROM DUAL;

          nPorcLocAcu := nPorcLocAcu + nPorcLoc;
          IF nCuentaLoc2 = c THEN
            nDif := 100 - nPorcLocAcu;
            IF nDif <> 0 THEN
              nPorcLoc := nPorcLoc + nDif;
            END IF;
          END IF;

          BEGIN
            nIdeDirec := pr_Interfase_AX.Graba_Linea_Dir_Masi2( p_nIdePol,
                                                                AUX_TDU_Tabla_Imp_Local(i).CodPais ,
                                                                AUX_TDU_Tabla_Imp_Local(i).CodEstado,
                                                                AUX_TDU_Tabla_Imp_Local(i).CodCiudad,
                                                                AUX_TDU_Tabla_Imp_Local(i).CodMunicipio,
                                                                AUX_TDU_Tabla_Imp_Local(i).UbiGeo,
                                                                AUX_TDU_Tabla_Imp_Local(i).TipoVia,
                                                                AUX_TDU_Tabla_Imp_Local(i).NomVia,
                                                                AUX_TDU_Tabla_Imp_Local(i).NumVia,
                                                                AUX_TDU_Tabla_Imp_Local(i).TipoInt,
                                                                AUX_TDU_Tabla_Imp_Local(i).NumInt,
                                                                AUX_TDU_Tabla_Imp_Local(i).Manzana,
                                                                AUX_TDU_Tabla_Imp_Local(i).Lote,
                                                                AUX_TDU_Tabla_Imp_Local(i).Kilome,
                                                                AUX_TDU_Tabla_Imp_Local(i).TipoCha,
                                                                AUX_TDU_Tabla_Imp_Local(i).NomCha,
                                                                AUX_TDU_Tabla_Imp_Local(i).TipoSdi,
                                                                AUX_TDU_Tabla_Imp_Local(i).NomSdi,
                                                                AUX_TDU_Tabla_Imp_Local(i).Referencia,
                                                                AUX_TDU_Tabla_Imp_Local(i).Resto,
                                                                nPorcLoc,
                                                                AUX_TDU_Tabla_Imp_Local(i).IdeGiro,
                                                                AUX_TDU_Tabla_Imp_Local(i).MtoValDeclarDirec,
                                                                AUX_TDU_Tabla_Imp_Local(i).Tep,
                                                                AUX_TDU_Tabla_Imp_Local(i).Tee,
                                                                AUX_TDU_Tabla_Imp_Local(i).AnoConstrucc,
                                                                AUX_TDU_Tabla_Imp_Local(i).NumPisos,
                                                                AUX_TDU_Tabla_Imp_Local(i).NumSotanos,
                                                                AUX_TDU_Tabla_Imp_Local(i).FecInspeccion,
                                                                AUX_TDU_Tabla_Imp_Local(i).NumCert,
                                                                AUX_TDU_Tabla_Imp_Local(i).IdeDirec,
                                                                AUX_TDU_Tabla_Imp_Local(i).IndRiesgoIns,
                                                                AUX_TDU_Tabla_Imp_Local(i).IndInspeccion,
                                                                AUX_TDU_Tabla_Imp_Local(i).NotaInspeccion,
                                                                AUX_TDU_Tabla_Imp_Local(i).IndSinInspeccion,
                                                                AUX_TDU_Tabla_Imp_Local(i).UsuAutorizador,
                                                                AUX_TDU_Tabla_Imp_Local(i).ClaseTipo,
                                                                AUX_TDU_Tabla_Imp_Local(i).PropieEdificacion,
                                                                -- out
                                                                nNumCert,
                                                                p_cError);
             --
             IF p_cError != '0,OK' THEN
               RETURN 0;
             END IF;

            EXCEPTION
              WHEN OTHERS THEN
                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Direcciones : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;
                RETURN 0;
            END;
            ---------------------------
            -- PASO 2 generar bienes --
            ---------------------------
            DBMS_OUTPUT.Put_Line('nIdeDirec = '||nIdeDirec);
            BEGIN
              IF nIdeDirec > 0 THEN
                --validar estado del certificado
                BEGIN
                  SELECT StsCert, FecIng, FecFin
                  INTO cStsCert, dFecIniVig, dFecFinVig
                  FROM Certificado
                  WHERE IdePol = p_nIdePol
                     AND NumCert = nNumCert;

                EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                    cStsCert := '';
                END;
                DBMS_OUTPUT.Put_Line('cStsCert = ' || cStsCert);

                IF cStsCert IN ('VAL', 'INC') THEN
                  -- PASO 2.1  eliminar los bienes por cada direccion
                  BEGIN
                    pr_Bien_Cert_Util.Eliminar_Bien_Riesgo(nIdeDirec);

                  EXCEPTION
                    WHEN OTHERS THEN
                      cMensaje := cMensaje || ' ' || SQLERRM;
                      cMensaje := ' al Eliminar las Direcciones Riesgo : ' || cMensaje;
                      p_cError := '1, ' || cMensaje;

                      RETURN 0;
                  END;
                END IF;

                FOR X IN c_Ramos LOOP
                  BEGIN
                    SELECT IndBien
                    INTO cIndBien
                    FROM Ramo_Plan_Prod
                    WHERE CodProd = p_cCodProd
                      AND CodPlan = X.CodPlan
                      AND RevPlan = X.RevPlan
                      AND CodRamoPlan = X.CodRamoCert
                    ORDER BY NumOrdImp;

                  EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                      cIndBien := 'N';
                  END;

                  IF cIndBien = 'S' THEN
                    nMtoTotal  := 0;
                    IF p_tTabla_BienesxCert.COUNT > 0 THEN
                      FOR i in p_tTabla_BienesxCert.First .. p_tTabla_BienesxCert.Last LOOP
                        cClaseBien := p_tTabla_BienesxCert(i).ClaseBien;
                        cCodBien   := p_tTabla_BienesxCert(i).CodBien;
                        nMonto     := NVL(p_tTabla_BienesxCert(i).MtoValDeclarBien, 0);
                        nMtoTotal  := nMtoTotal + nMonto;
                        --
                        BEGIN
                          -- PASO 2.3 GENERAR BIENES
                          IF nIdeDirec_Aux = 0 THEN
                            pr_Interfase_AX.Genera_Bien_Masi2(p_nIdePol,
                                              nNumCert,
                                              p_cCodProd,
                                              X.CodRamoCert,
                                              X.CodPlan,
                                              X.RevPlan,
                                              nIdeDirec,
                                              cClaseBien,
                                              cCodBien,
                                              nMonto,
                                              p_cCodMoneda,
                                              p_tTabla_BienesxCert(i).FecIniValid,
                                              p_tTabla_BienesxCert(i).FecFinValid);
                          END IF;

                        EXCEPTION
                          WHEN OTHERS THEN
                            cMensaje := cMensaje || ' ' || SQLERRM;
                            cMensaje := ' al Procesar la Carga de Bienes : ' ||cMensaje ;
                            p_cError := '1, '||cMensaje;

                            RETURN 0;
                        END;

                        nNumBien := nNumBien + 1;
                      END LOOP;
                    END IF;
                  END IF; -- cIndBien = 'S'

                END LOOP; -- X -- c_Ramos
                --
                nIdeDirec_Aux := nIdeDirec;
                nMtoTotal  := 0;
                IF p_Tabla_Bien_Cert_Riesgo.COUNT > 0 THEN
                  FOR i in p_Tabla_Bien_Cert_Riesgo.First .. p_Tabla_Bien_Cert_Riesgo.Last LOOP
                    nMtoTotal := nMtoTotal + NVL(p_Tabla_Bien_Cert_Riesgo(i).MtoValDeclarBien, 0);
                    --
                    UPDATE Bien_Cert_Riesgo
                    SET MtoValDeclarBienDirec = p_Tabla_Bien_Cert_Riesgo(i).MtoValDeclarBien,
                      PorcRiesgo = 0
                    WHERE IdeDirec = p_Tabla_Bien_Cert_Riesgo(i).IdeDirec
                      AND IdeBien = TO_NUMBER(p_Tabla_Bien_Cert_Riesgo(i).TxtAdicional);
                  END LOOP;

                  BEGIN
                    FOR X IN (SELECT IdeBien, COUNT(1) Cuenta, SUM(MtoValDeclarBienDirec) MontoTotal
                              FROM Bien_Cert_Riesgo
                              WHERE IdeBien IN (SELECT IdeBien
                                                FROM Bien_Cert
                                                WHERE IdePol = p_nIdePol)
                              GROUP BY IdeBien) LOOP
                      UPDATE Bien_Cert_Riesgo
                      SET PorcRiesgo = DECODE(x.MontoTotal, 0, 0, (MtoValDeclarBienDirec / x.MontoTotal)) * 100
                      WHERE IdeBien = X.IdeBien;
                    END LOOP;
                  END;
                END IF;
                nNumLinea := nNumLinea + 1;
            END IF; -- IF nIdeDirec > 0 THEN

           EXCEPTION
              WHEN OTHERS THEN
                cMensaje := cMensaje || ' ' || SQLERRM;
                cMensaje := ' al procesar Carga de Bienes : ' || cMensaje ;
                p_cError := '1, '|| cMensaje;

                RETURN 0;
            END;
         END LOOP;
       END IF;
    END;

    RETURN 0;

EXCEPTION
  WHEN OTHERS THEN
    cMensaje := cMensaje || ' ' || SQLERRM;
    p_cError := '1, ' || cMensaje;
    RETURN 0;
END Recepcionar_Archivo;

BEGIN
  nVal := Recepcionar_Archivo;
  DBMS_OUTPUT.Put_Line('nVal = '|| nVal);
END Endoso_Traslado_Bienes;


/*--------------------------------------------------------------------
  Nombre      : Caract_Extran_Trec
  Proposito   : Valida que los caracteres dentro del texto sean validos en los campos de maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
  Parametros  :
                p_Valor            VARCHAR2                  Texto a validar
  Fecha         Autor             Descripcion
  11/08/2023    Manuel Navarro             Creacion
--------------------------------------------------------------------*/
FUNCTION Caract_Extran_Trec(p_Valor VARCHAR2) RETURN NUMBER IS

cCadena    VARCHAR2(50) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ';
nLong       NUMBER(6);
nCount     NUMBER(2) := 0;

BEGIN
  IF p_Valor IS NOT NULL THEN
    nLong := LENGTH(p_Valor);
    FOR I IN 1..nLong LOOP
      IF INSTR(cCadena,SUBSTR(p_Valor,I,1)) = 0 THEN
        nCount := nCount + 1;
      END IF;
    END LOOP;
  END IF;
  IF NVL(nCount,0)>0 THEN
    RETURN 1;
  ELSE
    RETURN 0;
  END IF;
END Caract_Extran_Trec;


/*--------------------------------------------------------------------
  Nombre      : NumOper_Update_Trec
  Proposito   : Actualiza el nro. de operacion despues de activar la poliza de maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
  Parametros  :
                p_cTag             VARCHAR2                  Etiqueta XML principal
                p_cXMLInput        XML (CLob)                con los par�metros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER                    Identificador de P�liza
                p_nNumCert         NUMBER                    N�mero de Certificado
                p_datos_maquinaria TDU_TABLA_MAQUINARIA_TREC Arreglo de Datos Maquinarias
                p_cCodUsr          VARCHAR2                  C�digo de Usuario
                p_proceso          VARCHAR2                  Tipo de operaci�n (I-Inclusion, E-Exclusion, M-Modificacion)
                p_error            VARCHAR2                  Error
  Fecha       Autor             Descripcion
  11/08/2023    Manuel Navarro             Creacion
--------------------------------------------------------------------*/
PROCEDURE NumOper_Update_Trec(p_cTag IN VARCHAR2,
                              p_cXMLInput IN CLOB,
                              p_idepol             POLIZA.IDEPOL%TYPE,
                              p_numcert            CERT_RAMO.NUMCERT%TYPE,
                              p_numOper            OPER_POL.Numoper%TYPE,
                              p_proceso            VARCHAR2,
                              p_error              OUT VARCHAR2) IS

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
  nCantReg      INTEGER(5);
  vCodSerie      MAQUINARIA_TREC.CODSERIE%TYPE;
  vNumMotor      MAQUINARIA_TREC.NUMMOTOR%TYPE;
  vNumPlaca      MAQUINARIA_TREC.NUMPLACA%TYPE;
  vNumCert       MAQUINARIA_TREC.NUMCERT%TYPE;
  vNumMaqui      MAQUINARIA_TREC.NUMMAQUI%TYPE;
  vCodMarca      MAQUINARIA_TREC.CODMARCA%TYPE;
  vCodModelo     MAQUINARIA_TREC.CODMODELO%TYPE;
  vAnnoFabr      MAQUINARIA_TREC.ANNOFABR%TYPE;
  vMtoMaqui      MAQUINARIA_TREC.MTOMAQUI%TYPE;
  vIndIncluido   MAQUINARIA_TREC.INDINCLUIDO%TYPE;
  vUbicacion     MAQUINARIA_TREC.UBICACION%TYPE;
  vFecIni        MAQUINARIA_TREC.FECINI%TYPE;
  vIdeMaquinaria MAQUINARIA_TREC.IDEMAQUINARIA%TYPE;

    --
  BEGIN
    p_error := '0,OK';
    --
       vIndIncluido := 1;
       FOR r IN c_datosMaquinariasList(p_cTag) LOOP
           IF P_PROCESO IN ('I','E')THEN
              vCodSerie     := r.CodSerie;
              vNumMotor     := r.NumMotor;
              vNumPlaca     := r.NumPlaca;
              vnumMaqui     := r.NumMaqui;
              vcodMarca     := r.CodMarca;
              vCodModelo    := r.CodModelo;
              vAnnoFabr     := r.AnnoFabr;
              vMtoMaqui     := r.MtoMaqui;
              vUbicacion    := r.Ubicacion;
              vFecIni       := r.FecIni;
           ELSE
              vCodSerie     := r.CodSerie_New;
              vNumMotor     := r.NumMotor_New;
              vNumPlaca     := r.NumPlaca_New;
              vnumMaqui     := r.NumMaqui_New;
              vcodMarca     := r.CodMarca_New;
              vCodModelo    := r.CodModelo_New;
              vAnnoFabr     := r.AnnoFabr_New;
              vMtoMaqui     := r.MtoMaqui_New;
              vUbicacion    := r.Ubicacion_New;
              vFecIni       := r.FecIni_New;
           END IF;
           IF P_PROCESO = 'E' THEN
              vIndIncluido := 0;
           END IF;
           BEGIN
               IF p_numcert = 0 THEN
                   UPDATE MAQUINARIA_TREC SET NUMOPER = p_numOper
                   WHERE  idepol      = p_idepol
                      AND numcert     != 0
                      AND codserie    = vCodSerie
                      AND nummotor    = vNumMotor
                      AND numplaca    = vNumPlaca
                      AND NumMaqui    = vnumMaqui
                      AND CodMarca    = vcodMarca
                      AND CodModelo   = vCodModelo
                      AND AnnoFabr    = vAnnoFabr
                      AND MtoMaqui    = vMtoMaqui
                      AND Ubicacion   = vUbicacion
                      AND FecIni      = vFecIni
                      AND INDINCLUIDO = vIndIncluido;
               ELSE
                   UPDATE MAQUINARIA_TREC SET NUMOPER = p_numOper
                   WHERE  idepol      = p_idepol
                      AND numcert     = p_numcert
                      AND codserie    = vCodSerie
                      AND nummotor    = vNumMotor
                      AND numplaca    = vNumPlaca
                      AND NumMaqui    = vnumMaqui
                      AND CodMarca    = vcodMarca
                      AND CodModelo   = vCodModelo
                      AND AnnoFabr    = vAnnoFabr
                      AND MtoMaqui    = vMtoMaqui
                      AND Ubicacion   = vUbicacion
                      AND FecIni      = vFecIni
                      AND INDINCLUIDO = vIndIncluido;
               END IF;
           EXCEPTION WHEN OTHERS THEN
               p_error := P_PROCESO || ', No se pudo actualizar el n�mero de operaci�n '|| sqlerrm;
               EXIT;
           END;
       END LOOP;
  END NumOper_Update_Trec;


/*--------------------------------------------------------------------
  Nombre      : DATOS_MAQ_MARCA_TREC
  Proposito   : Devuelve el codigo o descripcion de la maquinaria o Marca de TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
  Parametros  : p_cBuscarPor    VARCHAR2(1)    Porque campo buscar  ('C' por c�digo, 'D' por descripci�n)
                p_cCodCrit      VARCHAR2(100)  Criterio de b�squeda ('EQUIPOS_TREC' Maquinarias, 'MARCAS_TREC' Marca)
                p_cvalorBuscado VARCHAR2(2000) C�digo de Producto
  Fecha       Autor             Descripcion
  11/08/2023    Manuel Navarro             Creacion
--------------------------------------------------------------------*/

FUNCTION Datos_Maq_Marca_trec(p_cBuscarPor IN VARCHAR2,
                              p_cCodCrit IN VARCHAR2,
                              p_cvalorBuscado IN VARCHAR2)
         RETURN VARCHAR2 IS
  cValor VARCHAR2(2000);
BEGIN
   cValor := null;
   IF p_cBuscarPor = 'C' THEN --Buscar por Codigo
      BEGIN
          SELECT TR.DESCTRASF
            INTO cValor
            FROM TRANSFORMADOR TR
           WHERE TR.CODCRIT = p_cCodCrit
             AND NVL(TR.STSTRASF, 'ACT') = 'ACT'
             AND TR.CODTRASF = p_cvalorBuscado;
      EXCEPTION WHEN OTHERS THEN
          cValor := null;
      END;
   END IF;
   IF p_cBuscarPor = 'D' THEN --Buscar por Descripci�n
      BEGIN
          SELECT TR.CODTRASF
            INTO cValor
            FROM TRANSFORMADOR TR
           WHERE TR.CODCRIT = p_cCodCrit
             AND NVL(TR.STSTRASF, 'ACT') = 'ACT'
             AND TR.DESCTRASF = p_cvalorBuscado;
      EXCEPTION WHEN OTHERS THEN
          cValor := null;
      END;
   END IF;
   RETURN cValor;
END Datos_Maq_Marca_trec;

/*-----------------------------------------------------------------------------
  Nombre      : inc_exc_maquinaria_trec
  Proposito   : Procedimiento para registrar la inclusi�n/exclusi�n de los datos maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
  Parametros  :
                p_nIdePol          NUMBER                    Identificador de P�liza
                p_nNumCert         NUMBER                    N�mero de Certificado
                p_datos_maquinaria TDU_TABLA_MAQUINARIA_TREC Arreglo de Datos Maquinarias
                p_cCodUsr          VARCHAR2                  C�digo de Usuario
                p_proceso          VARCHAR2                  Tipo de operaci�n (I-Inclusion, E-Exclusion, M-Modificacion)
                p_error            VARCHAR2                  Error
  Log de Cambios
    Fecha         Autor                      Descripci�n
    11/08/2023    Manuel Navarro             Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE inc_exc_maquinaria_trec(p_idepol             POLIZA.IDEPOL%TYPE,
                                p_numcert              CERT_RAMO.NUMCERT%TYPE,
                                p_datos_maquinaria     tdu_tabla_maquinaria_trec,
                                p_codusr               USUARIO.CODUSR%TYPE,
                                p_proceso              VARCHAR2,
                                p_error                OUT VARCHAR2)IS

  nCantReg      INTEGER(5);
  nExiste       NUMBER := 0;

  vCodSerie     MAQUINARIA_TREC.CODSERIE%TYPE;
  vNumMotor     MAQUINARIA_TREC.NUMMOTOR%TYPE;
  vNumPlaca     MAQUINARIA_TREC.NUMPLACA%TYPE;
  vNumCert      MAQUINARIA_TREC.NUMCERT%TYPE;
  vNumMaqui     MAQUINARIA_TREC.NUMMAQUI%TYPE;
  vDescMaqui    MAQUINARIA_TREC.DESCMAQUI%TYPE;
  vCodMarca     MAQUINARIA_TREC.CODMARCA%TYPE;
  vDescMarca    MAQUINARIA_TREC.DESCMARCA%TYPE;
  vCodModelo    MAQUINARIA_TREC.CODMODELO%TYPE;
  vAnnoFabr     MAQUINARIA_TREC.ANNOFABR%TYPE;
  vMtoMaqui     MAQUINARIA_TREC.MTOMAQUI%TYPE;
  vIndIncluido  MAQUINARIA_TREC.INDINCLUIDO%TYPE;
  vUbicacion    MAQUINARIA_TREC.UBICACION%TYPE;
  vFecIni       MAQUINARIA_TREC.FECINI%TYPE;
  vIdeMaquinaria MAQUINARIA_TREC.IDEMAQUINARIA%TYPE;
  --
  BEGIN
    p_error := '0,OK';
    --
    nCantReg := P_DATOS_MAQUINARIA.COUNT;
    --
    IF nCantReg > 0 THEN
      dbms_output.put_line('nCantReg > 0');
      FOR i in 1..P_DATOS_MAQUINARIA.count loop
        --
        vCodSerie     := P_DATOS_MAQUINARIA(i).CodSerie ;
        vNumMotor     := P_DATOS_MAQUINARIA(i).NumMotor;
        vNumPlaca     := P_DATOS_MAQUINARIA(i).NumPlaca;
        vNumMaqui     := P_DATOS_MAQUINARIA(i).NumMaqui;
        vDescMaqui    := Datos_Maq_Marca_trec('C', 'EQUIPOS_TREC', vNumMaqui);
        vCodMarca     := P_DATOS_MAQUINARIA(i).CodMarca;
        vDescMarca    := Datos_Maq_Marca_trec('C', 'MARCAS_TREC', vCodMarca);
        vCodModelo    := P_DATOS_MAQUINARIA(i).CodModelo;
        vAnnoFabr     := P_DATOS_MAQUINARIA(i).AnnoFabr;
        vMtoMaqui     := P_DATOS_MAQUINARIA(i).MtoMaqui;
        vUbicacion    := P_DATOS_MAQUINARIA(i).Ubicacion;
        vFecIni       := P_DATOS_MAQUINARIA(i).FecIni;

        IF vDescMaqui IS NULL THEN
           p_error := '1, C�digo de M�quina ' || vNumMaqui || ' no existe en la tabla Transformador';
           exit;
        END IF;
        IF vDescMarca IS NULL THEN
           p_error := '1, C�digo de Marca ' || vCodMarca || ' no existe en la tabla Transformador';
           exit;
        END IF;
        IF vCodSerie is null THEN
           p_error := '1, El nro. de serie no puede ser nulo';
           exit;
        END IF;
        IF vNumMotor is null THEN
           p_error := '1, El n�mero de motor no puede ser nulo';
           exit;
        END IF;
        IF vNumPlaca is null THEN
           p_error := '1, El n�mero de placa no puede ser nulo';
           exit;
        END IF;
        IF vAnnoFabr is null THEN
           p_error := '1, El a�o de fabricaci�n no puede ser nulo';
           exit;
        END IF;
        --

        BEGIN
         SELECT 1 INTO nExiste FROM MAQUINARIA_TREC
          WHERE idepol = P_IDEPOL
            AND codserie = vCodSerie
            AND nummotor = vNumMotor
            AND numplaca = vNumPlaca;
       EXCEPTION
         WHEN OTHERS THEN
           nExiste := 0;
       END;
       --
        dbms_output.put_line('go GRABAR_MAQUINARIA_TREC');
        -- Exclusion --
         IF P_PROCESO = 'E' THEN
           IF nExiste > 0 THEN
             BEGIN
               UPDATE MAQUINARIA_TREC SET INDINCLUIDO = '0'
                WHERE idepol = p_idepol
                  AND numcert = p_numcert
                  AND codserie = vCodSerie
                  AND nummotor = vNumMotor
                  AND numplaca = vNumPlaca;
             EXCEPTION WHEN OTHERS THEN
               p_error := 'E: No se pudo actualizar '|| sqlerrm;
             END;
           ELSE
             BEGIN
              SELECT NVL(MAX(IdeMaquinaria),0) + 1
                INTO  vIdeMaquinaria
                FROM  Maquinaria_Trec
               WHERE  IdePol = p_idepol
                 AND  NumCert = p_numcert;
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  vIdeMaquinaria := 1;
            END;
            --
             BEGIN
               INSERT INTO MAQUINARIA_TREC (IDEPOL, CODSERIE, NUMMOTOR, NUMPLACA, NUMCERT, NUMMAQUI, DESCMAQUI, CODMARCA,
                                            DESCMARCA, CODMODELO, ANNOFABR, MTOMAQUI, CODENDOSO, UBICACION, FECINI, INDINCLUIDO, IDEMAQUINARIA, USUOPER, FECOPER)
                    VALUES (p_idepol, vCodSerie, vNumMotor, vNumPlaca, p_numcert, vNumMaqui, vDescMaqui, vCodMarca,
                            vDescMarca, vCodModelo, vAnnoFabr, vMtoMaqui,null,vUbicacion, vFecIni,'0', vIdeMaquinaria, p_codusr, sysdate);
             EXCEPTION WHEN OTHERS THEN
               p_error := 'E: No se pudo registrar '|| sqlerrm;
             END;
           END IF;
         END IF;
         --
         -- Inclusion --
         IF P_PROCESO = 'I' THEN
           IF nExiste > 0 THEN
             BEGIN
               UPDATE MAQUINARIA_TREC SET INDINCLUIDO = '1'
                WHERE idepol = P_IDEPOL
                  AND codserie = vCodSerie
                  AND nummotor = vNumMotor
                  AND numplaca = vNumPlaca;
             EXCEPTION WHEN OTHERS THEN
               p_error := 'I: No se pudo actualizar '|| sqlerrm;
             END;
           ELSE
             BEGIN
              SELECT NVL(MAX(IdeMaquinaria),0) + 1
                INTO  vIdeMaquinaria
                FROM  Maquinaria_Trec
               WHERE  IdePol = p_idepol
                 AND  NumCert = p_numcert;
             EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  vIdeMaquinaria := 1;
             END;
             --
             BEGIN
               INSERT INTO MAQUINARIA_TREC (IDEPOL, CODSERIE, NUMMOTOR, NUMPLACA, NUMCERT, NUMMAQUI, DESCMAQUI, CODMARCA,
                                            DESCMARCA, CODMODELO, ANNOFABR, MTOMAQUI, CODENDOSO, UBICACION, FECINI, INDINCLUIDO, IDEMAQUINARIA, USUOPER, FECOPER)
                    VALUES (P_IDEPOL, vCodSerie, vNumMotor, vNumPlaca, P_NUMCERT, vNumMaqui, vDescMaqui, vCodMarca,
                            vDescMarca, vCodModelo, vAnnoFabr, vMtoMaqui, null, vUbicacion, vFecIni,'1', vIdeMaquinaria, p_codusr, sysdate);
             EXCEPTION WHEN OTHERS THEN
               p_error := 'I: No se pudo registrar '|| sqlerrm;
             END;
           END IF;
         END IF;
         --
       END LOOP;
    END IF;
  END inc_exc_maquinaria_trec;

/*-----------------------------------------------------------------------------
  Nombre      : mod_maquinaria_trec
  Proposito   : Procedimiento para registrar las modificaciones de datos maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
  Parametros  :
                p_nIdePol          NUMBER                    Identificador de P�liza
                p_nNumCert         NUMBER                    N�mero de Certificado
                p_datos_maquinaria TDU_TABLA_MAQUINARIA_TREC Arreglo de Datos Maquinarias
                p_cCodUsr          VARCHAR2                  C�digo de Usuario
                p_proceso          VARCHAR2                  Tipo de operaci�n (I-Inclusion, E-Exclusion, M-Modificacion)
                p_error            VARCHAR2                  Error

  Log de Cambios
    Fecha         Autor                      Descripci�n
    11/08/2023    Manuel Navarro             Creacion
----------------------------------------------------------------------------*/

PROCEDURE mod_maquinaria_trec(p_idepol             POLIZA.IDEPOL%TYPE,
                              p_numcert              CERT_RAMO.NUMCERT%TYPE,
                              p_datos_maquinaria     tdu_tabla_maquinaria_trec,
                              p_codusr               USUARIO.CODUSR%TYPE,
                              p_proceso              VARCHAR2,
                              p_error                out varchar2)is

  nCantReg      INTEGER(5);
  nExiste       NUMBER := 0;
  vIdeMaquinaria MAQUINARIA_TREC.IDEMAQUINARIA%TYPE;

  vCodSerie     MAQUINARIA_TREC.CODSERIE%TYPE;
  vNumMotor     MAQUINARIA_TREC.NUMMOTOR%TYPE;
  vNumPlaca     MAQUINARIA_TREC.NUMPLACA%TYPE;
  vNumCert      MAQUINARIA_TREC.NUMCERT%TYPE;
  vNumMaqui     MAQUINARIA_TREC.NUMMAQUI%TYPE;
  vDescMaqui    MAQUINARIA_TREC.DESCMAQUI%TYPE;
  vCodMarca     MAQUINARIA_TREC.CODMARCA%TYPE;
  vDescMarca    MAQUINARIA_TREC.DESCMARCA%TYPE;
  vCodModelo    MAQUINARIA_TREC.CODMODELO%TYPE;
  vAnnoFabr     MAQUINARIA_TREC.ANNOFABR%TYPE;
  vMtoMaqui     MAQUINARIA_TREC.MTOMAQUI%TYPE;
  vUbicacion    MAQUINARIA_TREC.UBICACION%TYPE;
  vFecIni       MAQUINARIA_TREC.UBICACION%TYPE;
  --
  vCodSerie_new     MAQUINARIA_TREC.CODSERIE%TYPE;
  vNumMotor_new     MAQUINARIA_TREC.NUMMOTOR%TYPE;
  vNumPlaca_new     MAQUINARIA_TREC.NUMPLACA%TYPE;
  vNumCert_new      MAQUINARIA_TREC.NUMCERT%TYPE;
  vNumMaqui_new     MAQUINARIA_TREC.NUMMAQUI%TYPE;
  vDescMaqui_new    MAQUINARIA_TREC.DESCMAQUI%TYPE;
  vCodMarca_new     MAQUINARIA_TREC.CODMARCA%TYPE;
  vDescMarca_new    MAQUINARIA_TREC.DESCMARCA%TYPE;
  vCodModelo_new    MAQUINARIA_TREC.CODMODELO%TYPE;
  vAnnoFabr_new     MAQUINARIA_TREC.ANNOFABR%TYPE;
  vMtoMaqui_new     MAQUINARIA_TREC.MTOMAQUI%TYPE;
  vUbicacion_new    MAQUINARIA_TREC.UBICACION%TYPE;
  vFecIni_new       MAQUINARIA_TREC.UBICACION%TYPE;

BEGIN
  --
  p_error := '0,OK';
  nCantReg := P_DATOS_MAQUINARIA.COUNT;
  --
  IF nCantReg > 0 THEN
    dbms_output.put_line('nCantReg > 0');
    FOR i in 1..P_DATOS_MAQUINARIA.count loop
       --Se asignan a variables debido a que no se pueden enviar por parametro el elemento de la matriz
       vCodSerie     := P_DATOS_MAQUINARIA(i).CodSerie ;
       vNumMotor     := P_DATOS_MAQUINARIA(i).NumMotor;
       vNumPlaca     := P_DATOS_MAQUINARIA(i).NumPlaca;
       vNumMaqui     := P_DATOS_MAQUINARIA(i).NumMaqui;
       vDescMaqui    := Datos_Maq_Marca_trec('C', 'EQUIPOS_TREC', vNumMaqui);
       vCodMarca     := P_DATOS_MAQUINARIA(i).CodMarca;
       vDescMarca    := Datos_Maq_Marca_trec('C', 'MARCAS_TREC', vCodMarca);
       vCodModelo    := P_DATOS_MAQUINARIA(i).CodModelo;
       vAnnoFabr     := P_DATOS_MAQUINARIA(i).AnnoFabr;
       vMtoMaqui     := P_DATOS_MAQUINARIA(i).MtoMaqui;
       vUbicacion    := P_DATOS_MAQUINARIA(i).Ubicacion;
       vFecIni       := P_DATOS_MAQUINARIA(i).FecIni;
       --
       vCodSerie_new     := P_DATOS_MAQUINARIA(i).CodSerie_new;
       vNumMotor_new     := P_DATOS_MAQUINARIA(i).NumMotor_new;
       vNumPlaca_new     := P_DATOS_MAQUINARIA(i).NumPlaca_new;
       vNumMaqui_new     := P_DATOS_MAQUINARIA(i).NumMaqui_new;
       vDescMaqui_new    := Datos_Maq_Marca_trec('C', 'EQUIPOS_TREC', vNumMaqui);
       vCodMarca_new     := P_DATOS_MAQUINARIA(i).CodMarca_new;
       vDescMarca_new    := Datos_Maq_Marca_trec('C', 'MARCAS_TREC', vCodMarca);
       vCodModelo_new    := P_DATOS_MAQUINARIA(i).CodModelo_new;
       vAnnoFabr_new     := P_DATOS_MAQUINARIA(i).AnnoFabr_new;
       vMtoMaqui_new     := P_DATOS_MAQUINARIA(i).MtoMaqui_new;
       vUbicacion_new    := P_DATOS_MAQUINARIA(i).Ubicacion_new;
       vFecIni_new       := P_DATOS_MAQUINARIA(i).FecIni_new;

       IF vDescMaqui IS NULL THEN
          p_error := '1, C�digo de M�quina ' || vNumMaqui || ' en la tabla Transformador';
          exit;
       end if;
       IF vDescMarca IS NULL THEN
          p_error := '1, C�digo de Marca ' || vCodMarca || ' en la tabla Transformador';
          exit;
       end if;
       IF vDescMaqui_new IS NULL THEN
           p_error := '1, C�digo de M�quina ' || vNumMaqui || ' en la tabla Transformador';
           exit;
       end if;
       IF vDescMarca_new IS NULL THEN
          p_error := '1, C�digo de Marca ' || vCodMarca || ' en la tabla Transformador';
          exit;
       end if;

        IF vCodSerie_new is null THEN
           p_error := '1, El nro. de serie no puede ser nulo';
           exit;
        END IF;
        IF vNumMotor_new is null THEN
           p_error := '1, El n�mero de motor no puede ser nulo';
           exit;
        END IF;
        IF vNumPlaca_new is null THEN
           p_error := '1, El n�mero de placa no puede ser nulo';
           exit;
        END IF;
        IF vAnnoFabr_new is null THEN
           p_error := '1, El a�o de fabricaci�n no puede ser nulo';
           exit;
        END IF;

       -- Este datos se obtiene del A/X ya no se utiliza lo que se envia.
       -- Grabar los datos particulares
       dbms_output.put_line('go GRABAR_MAQUINARIA_TREC');

       -- Modificacion --
       IF P_PROCESO = 'M' THEN
         BEGIN
           SELECT 1 INTO nExiste FROM MAQUINARIA_TREC
            WHERE idepol = P_IDEPOL
              AND codserie = vCodSerie
              AND nummotor = vNumMotor
              AND numplaca = vNumPlaca;
         EXCEPTION
           WHEN OTHERS THEN
             nExiste := 0;
         END;
         IF nExiste > 0 THEN
           BEGIN
             UPDATE MAQUINARIA_TREC
                SET CODSERIE = vCodSerie_new, NUMMOTOR = vNumMotor_new, NUMPLACA = vNumPlaca_new, NUMMAQUI = vNumMaqui_new, DESCMAQUI = vDescMaqui_new,
                    CODMARCA = vCodMarca_new, DESCMARCA = vDescMarca_new, CODMODELO = vCodModelo_new, ANNOFABR = vAnnoFabr_new, UBICACION = vUbicacion_new
              WHERE idepol = P_IDEPOL
                AND codserie = vCodSerie
                AND nummotor = vNumMotor
                AND numplaca = vNumPlaca;
           EXCEPTION WHEN OTHERS THEN
             p_error := 'M: No se pudo actualizar '|| sqlerrm;
           END;
         ELSE
           BEGIN
            SELECT NVL(MAX(IdeMaquinaria),0) + 1
              INTO  vIdeMaquinaria
              FROM  Maquinaria_Trec
             WHERE  IdePol = p_idepol
               AND  NumCert = p_numcert;
           EXCEPTION
              WHEN NO_DATA_FOUND THEN
                vIdeMaquinaria := 1;
           END;
           BEGIN
           INSERT INTO MAQUINARIA_TREC (IDEPOL, CODSERIE, NUMMOTOR, NUMPLACA, NUMCERT, NUMMAQUI, DESCMAQUI, CODMARCA,
                                          DESCMARCA, CODMODELO, ANNOFABR, MTOMAQUI, CODENDOSO, UBICACION, FECINI, INDINCLUIDO, IDEMAQUINARIA, USUOPER, FECOPER)
                  VALUES (P_IDEPOL, vCodSerie_new, vNumMotor_new, vNumPlaca_new, P_NUMCERT, vNumMaqui_new, vDescMaqui_new, vCodMarca_new,
                          vDescMarca_new, vCodModelo_new, vAnnoFabr_new, vMtoMaqui_new, null, vUbicacion_new, vFecIni_new,'1', vIdeMaquinaria, p_codusr, sysdate);
           EXCEPTION WHEN OTHERS THEN
             p_error := 'M: No se pudo registrar/actualizar '|| sqlerrm;
           END;
         END IF;
       END IF;
     END LOOP;
  END IF;
END mod_maquinaria_trec;


/*--------------------------------------------------------------------
  Nombre      : Valida_Suma_Trec
  Proposito   : Validar el valor declarado contra monto ingreso maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validaci�n de Maquinaria TREC
  Parametros  : p_nIdepol   NUMBER   N�mero de Idepol
                p_cCodProd  VARCHAR  C�digo de Producto
  Fecha       Autor             Descripcion
  11/08/2023  Manuel Navarro    Creacion
--------------------------------------------------------------------*/
FUNCTION Valida_Suma_Trec(p_nIdePol IN POLIZA.IdePol%TYPE,
                          p_cCodProd IN Poliza.CodProd%TYPE)
  RETURN VARCHAR2 IS

  --
  CURSOR c_Data IS
    SELECT DISTINCT NumCert
      FROM CERTIFICADO
     WHERE IdePol = p_nIdePol;
  --
  nNumCertC NUMBER:=0;
  nNumCertM NUMBER:=0;
  cValTrasf DATOS_PARTICULARES.ValTrasf%TYPE;
  nMtoMaqui MAQUINARIA_TREC.MtoMaqui%TYPE;
  nValTrasf NUMBER:=0;
  cValCas VARCHAR2(1):='0';
  nCantReg NUMBER:=0;
BEGIN
  -- Valida numero de certificado registrados
  IF PR.BUSCA_LVAL('MAQTRECW', p_cCodProd) = '1' THEN
      BEGIN
         SELECT COUNT(1)
         INTO nCantReg
         FROM Maquinaria_Trec
         WHERE IdePol = p_nIdePol;
       EXCEPTION WHEN OTHERS THEN
         nCantReg := 0;
      END;
      IF nCantReg = 0 THEN
         RETURN '0';
      END IF;

      -- Valida numero de certificado registrados
      BEGIN
        SELECT COUNT(DISTINCT NumCert)
          INTO nNumCertC
          FROM CERTIFICADO
         WHERE IdePol = p_nIdePol;
      EXCEPTION
        WHEN OTHERS THEN
          nNumCertC:= 0;
      END;
      -- Valida numero certificado registrado tabla TREC
       BEGIN
        SELECT COUNT(DISTINCT NumCert)
          INTO nNumCertM
          FROM MAQUINARIA_TREC
         WHERE IdePol = p_nIdePol
           AND IndIncluido = '1';
      EXCEPTION
        WHEN OTHERS THEN
          nNumCertM:= 0;
      END;
      --
      dbms_output.put_line('Valida_Suma_Trec NUM CERT ' || nNumCertC);
      dbms_output.put_line('Valida_Suma_Trec NUM MAQUI ' || nNumCertM);
      IF NVL(nNumCertC,0) = NVL(nNumCertM,0) AND NVL(nNumCertM,0) > 0
        AND NVL(nNumCertC,0) > 0 THEN
        cValCas:= '0';
      ELSE
        cValCas:= '1';
      END IF;
      --
      IF cValCas = '0' THEN
        FOR x IN c_Data LOOP
          --
          BEGIN
            SELECT SUM(ValTrasf)
              INTO cValTrasf
              FROM DATOS_PARTICULARES
             WHERE IdePol = p_nIdePol
               AND NumCert = x.NumCert
               AND CodCrit LIKE 'VALOR_DECLARADO%'
               AND CODRAMOCERT = 'TREC'; 
          EXCEPTION
            WHEN OTHERS THEN
              cValTrasf:= NULL;
          END;
          --
          BEGIN
            SELECT SUM(NVL(MTOMAQUI,0))
              INTO nMtoMaqui
              FROM MAQUINARIA_TREC
             WHERE IdePol = p_nIdePol
               AND NumCert = x.NumCert
               AND IndIncluido = '1';
          EXCEPTION
            WHEN OTHERS THEN
              nMtoMaqui:= NULL;
          END;
          --
          IF cValTrasf IS NOT NULL THEN
            BEGIN
            SELECT TO_NUMBER(cValTrasf)
              INTO nValTrasf
              FROM DUAL;
            EXCEPTION
              WHEN OTHERS THEN
               cValCas:= '1';
            END;
          END IF;
          --
          dbms_output.put_line('Valida_Suma_Trec CVALCAS ' || cValCas);
          dbms_output.put_line('Valida_Suma_Trec Suma Asegurada ' || cValTrasf);
          dbms_output.put_line('Valida_Suma_Trec Maquinaria ' || nMtoMaqui);
          IF cValCas = '0' THEN
            IF NVL(nMtoMaqui,0) = NVL(nValTrasf,0) AND NVL(nValTrasf,0) > 0
               AND NVL(nMtoMaqui,0) > 0 THEN
              cValCas:= '0';
            ELSE
              cValCas:= '1';
              EXIT;
            END IF;
          END IF;
        END LOOP;
        --
        IF cValCas = '0' THEN
          RETURN '0';
        ELSE
          RETURN '1';
        END IF;
      ELSE
        RETURN '1';
      END IF;
   ELSE
      RETURN '0';
   END IF;
END Valida_Suma_Trec;



end;
/
