create or replace package PR_INTERFASE_AX_B2B is

/*-----------------------------------------------------------------------
    Nombre : PR_INTERFASE_AX_B2B
    Autor  : Jorge benel
    Proposito: Implementación de lógica para la Digitalización de Endosos
    Log de Cambios
    Fecha        Autor                        Descripcion
    11/03/2022   Jorge Benel                  RTC-313089 Creacion
    01/04/2022   Jorge Benel                  RTC-313089 Actualización del procedimiento Actualiza_comisión
    24/03/2022   Ronald Barreda               RTC-313092 Inserción procedimientos para Modificación de Trabajadores 3D
    13/04/2022   Ronald Barreda               RTC-313344 Inserción procedimiento de inserción de endosatario
    22/04/2022   Ronald Barreda               RTC-313344 Inserción procedimiento de exclusión de endosatario
    27/04/2022   Ronald Barreda               RTC-313092 Se añadio parametros Tasa y Suma Asegurada procedimiento ADD_COBERT_CERT_3D
    05/05/2022   Ronald Barreda               RTC-313089 Se añadio procedimiento comun TramiteVincular y TramiteDesvincular
    24/05/2022   Ronald Barreda               RTC-313092 Actualización de parámetros
    16/06/2022   Ronald Barreda               RTC 313344 Se agrego parametros canal y tipo de canal a procedimiento ModificacionCorredor
    24/06/2022   Jorge Benel                  RTC 314175 proceso de Endoso de modificación de valores declarados
    24/06/2022   Jorge Benel                  RTC 313346 Implementa validación VALIDA_CONTRATANTE2 para el endoso de contratante
    28/06/2022   Mauro Zegarra                RTC-314182 Cambio de Comision
    30/06/2022   Ronald Barreda               RTC-313091 Se añadio procedimiento REVERTIR_OPERACION como parte de anulación de endoso
    08/07/2022   Jorge Benel                  RTC-314175 Creacion del procedimiento GENERA_BIEN_MASI2
    12/07/2022   Jorge Benel                  RTC-314179 Creacion del Endoso de Traslado de Bienes
    14/07/2022   Christian Bayona             RTC-314184 Creacion del Endoso de Declaración de Existencias Tipo A - Mensual
    19/07/2022   Mauro Zegarra                RTC-314183 Inclusion de Equipos TREC
    20/07/2022   Jorge Benel                  RTC-313349 Creacion de Cambio de responsable de pago
    02/08/2022   Mauro Zegarra                RTC-314181 Modificación de Deducible
    22/08/2022   Christian Bayona             RTC-313752 Se agregó procedimiento para validar coberturas
    06/09/2022   Jorge Benel                  RTC-314175 Implementación de parametros Endoso de Valores Declarados
    12/09/2022   Jorge Benel                  RTC-314951 Creación de Endoso de Anulación
    19/09/2022   Jorge Benel                  RTC-313349 Inclusión de cursor de salida en procedimiento CAMBIO_RESP_PAGO
    26/09/2022   Jorge Benel                  RTC-313742 Actualización de lógica - Modificiación de Locales
    26/09/2022   Mauro Zegarra                RTC-314182 Se agrega parametro p_codusuario a procedimiento modificar_comision
    11/10/2022   Mauro Zegarra                RTC-314183 Sobrecarga de Metodo y Validaciones
    19/10/2022   Christian Bayona             RTC-314184 Se agregó procedimientos para Endoso DecExist Tipo B
    23/01/2023   Christian Bayona             RTC-316106 Se agregó nuevo parámetro
    31/01/2023   Christian Bayona             RTC-316106 Se agregó nueva función
    17/02/2023   Christian Bayona             RTC-316449 Se agregó nuevo procedimiento
    22/02/2023   Christian Bayona             RTC-316449 Se modifica procedimiento, se agrega nuevo parámetro
    12/05/2023   Christian Bayona             RTC-316878 CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaración de Existencias Tipo B
    26/05/2023   Manuel Navarro               RTC 316878 Suma Asegurada VB Core
    07/07/2023   Christian Bayona             RTC 317178 Error en la obtención de datos para impresión para los casos con varios responsables de pago no está obteniendo todos los datos requeridos
    11/08/2023   Manuel Navarro               DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
 -------------------------------------------------------------------------*/

/*--------------------------------------------------------------------
  Nombre      : Caract_Extran_Trec
  Proposito   : Valida que los caracteres dentro del texto sean validos en los campos de maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
  Parametros  :
                p_Valor            VARCHAR2                  Texto a validar
  Fecha         Autor             Descripcion
  11/08/2023    Manuel Navarro             Creacion
--------------------------------------------------------------------*/
FUNCTION Caract_Extran_Trec(p_Valor VARCHAR2) RETURN NUMBER;

/*--------------------------------------------------------------------
  Nombre      : NumOper_Update_Trec
  Proposito   : Actualiza el nro. de operacion despues de activar la poliza de maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
  Parametros  :
                p_cTag             VARCHAR2                  Etiqueta XML principal
                p_cXMLInput        XML (CLob)                con los parámetros de entrada necesario para crear el endoso
                p_nIdePol          NUMBER                    Identificador de Póliza
                p_nNumCert         NUMBER                    Número de Certificado
                p_datos_maquinaria TDU_TABLA_MAQUINARIA_TREC Arreglo de Datos Maquinarias
                p_cCodUsr          VARCHAR2                  Código de Usuario
                p_proceso          VARCHAR2                  Tipo de operación (I-Inclusion, E-Exclusion, M-Modificacion)
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
                              p_error              OUT VARCHAR2);


/*--------------------------------------------------------------------
  Nombre      : DATOS_MAQ_MARCA_TREC
  Proposito   : Devuelve el codigo o descripcion de la maquinaria o Marca de TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
  Parametros  : p_cBuscarPor    VARCHAR2(1)    Porque campo buscar  ('C' por código, 'D' por descripción)
                p_cCodCrit      VARCHAR2(100)  Criterio de búsqueda ('EQUIPOS_TREC' Maquinarias, 'MARCAS_TREC' Marca)
                p_cvalorBuscado VARCHAR2(2000) Código de Producto
  Fecha       Autor             Descripcion
  11/08/2023    Manuel Navarro             Creacion
--------------------------------------------------------------------*/
FUNCTION Datos_Maq_Marca_trec(p_cBuscarPor IN VARCHAR2,
                              p_cCodCrit IN VARCHAR2,
                              p_cvalorBuscado IN VARCHAR2)
         RETURN VARCHAR2;

/*-----------------------------------------------------------------------------
  Nombre      : inc_exc_maquinaria_trec
  Proposito   : Procedimiento para registrar la inclusión/exclusión de los datos maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
  Parametros  :
                p_nIdePol          NUMBER                    Identificador de Póliza
                p_nNumCert         NUMBER                    Número de Certificado
                p_datos_maquinaria TDU_TABLA_MAQUINARIA_TREC Arreglo de Datos Maquinarias
                p_cCodUsr          VARCHAR2                  Código de Usuario
                p_proceso          VARCHAR2                  Tipo de operación (I-Inclusion, E-Exclusion, M-Modificacion)
                p_error            VARCHAR2                  Error
  Log de Cambios
    Fecha         Autor                      Descripción
    11/08/2023    Manuel Navarro             Creacion
  ----------------------------------------------------------------------------*/
PROCEDURE inc_exc_maquinaria_trec(p_idepol             POLIZA.IDEPOL%TYPE,
                                p_numcert              CERT_RAMO.NUMCERT%TYPE,
                                p_datos_maquinaria     tdu_tabla_maquinaria_trec,
                                p_codusr               USUARIO.CODUSR%TYPE,
                                p_proceso              VARCHAR2,
                                p_error                OUT VARCHAR2);

/*-----------------------------------------------------------------------------
  Nombre      : mod_maquinaria_trec
  Proposito   : Procedimiento para registrar las modificaciones de datos maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
  Parametros  :
                p_nIdePol          NUMBER                    Identificador de Póliza
                p_nNumCert         NUMBER                    Número de Certificado
                p_datos_maquinaria TDU_TABLA_MAQUINARIA_TREC Arreglo de Datos Maquinarias
                p_cCodUsr          VARCHAR2                  Código de Usuario
                p_proceso          VARCHAR2                  Tipo de operación (I-Inclusion, E-Exclusion, M-Modificacion)
                p_error            VARCHAR2                  Error

  Log de Cambios
    Fecha         Autor                      Descripción
    11/08/2023    Manuel Navarro             Creacion
----------------------------------------------------------------------------*/

PROCEDURE mod_maquinaria_trec(p_idepol             POLIZA.IDEPOL%TYPE,
                              p_numcert              CERT_RAMO.NUMCERT%TYPE,
                              p_datos_maquinaria     tdu_tabla_maquinaria_trec,
                              p_codusr               USUARIO.CODUSR%TYPE,
                              p_proceso              VARCHAR2,
                              p_error                out varchar2);

/*--------------------------------------------------------------------
  Nombre      : Valida_Suma_Trec
  Proposito   : Validar el valor declarado contra monto ingreso maquinarias TREC
  Referencias : DIGB2B03-2877 Mejora en Registro y Validación de Maquinaria TREC
  Parametros  : p_nIdepol   NUMBER   Número de Idepol
                p_cCodProd  VARCHAR  Código de Producto
  Fecha       Autor             Descripcion
  11/08/2023  Manuel Navarro    Creacion
--------------------------------------------------------------------*/
FUNCTION Valida_Suma_Trec(p_nIdePol IN POLIZA.IdePol%TYPE,
                          p_cCodProd IN Poliza.CodProd%TYPE)
  RETURN VARCHAR2 ;



FUNCTION MONTO_TOTAL_ENDOSADO_X_CERT(p_idepol CERTIFICADO.IDEPOL%type, p_numcert CERTIFICADO.NUMCERT%type) RETURN NUMBER;

FUNCTION VALIDA_SUMAASEGURADA_SUSTENTO(p_idepol    poliza.idepol%type,
                                       p_numcert   certificado.numcert%type,
                                       p_MontoEndoso Number,
                                       p_Aprobado Varchar2,
                                       p_Sustento Varchar2,
                                       p_usuarioaprob Varchar2,
                                       p_cError out Varchar2
                                       ) Return Number;

PROCEDURE INSERTA_AUTORIZACIONE (p_idepol    poliza.idepol%type,
                                 p_numcert   certificado.numcert%type,
                                 p_ideendcert mod_endosatario_cert.IDEENDCERT%type,
                                 p_useraut   varchar2,
                                 p_sustento  varchar2,
                                 p_nummod    varchar2,
                                 p_cError    out varchar2);


  PROCEDURE LIMPIAR_LOG_MENSAJES  ;

-- <I RTC 313089> jbenel
  PROCEDURE CONFIRMAR_RECALCULO (
    p_nIdeAut     AUTORIZA_VIOLACION.IdeAut%TYPE  ,
    p_nIdePol     AUTORIZA_VIOLACION.IdePol%TYPE  ,
    p_cRecalculo  Varchar2
    ) ;

  PROCEDURE FR_AUTORIZA_VIOLACION ( p_nIdePol                  IN  POLIZA.idepol%type,
                                   p_cTipoOper                 IN  OPER_POL_DET.tipoop%type,
                                   p_TABLA_AUTORIZA_VIOLACION  IN  ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                   p_cCodUsuario               IN  VARCHAR2,
                                   -- Variables de retorno
                                   p_cursorMsgValida           OUT SYS_REFCURSOR ,
                                   p_cError                    OUT VARCHAR2 ) ;



PROCEDURE EJECUTA_POLITICAS_VIOLADAS ( p_nIdePol                   IN poliza.idepol%type,
                                       p_nNumPol                   IN poliza.numpol%type,
                                       p_cCodProd                  IN poliza.codprod%type,
                                       p_cTipoOper                 IN oper_pol_det.tipoop%type,
                                       p_cCodUsuario               IN VARCHAR2,
                                       p_TABLA_POLITICA_AUTORIZADA IN ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                       -- Variables de retorno
                                       p_TABLA_AUTORIZA_VIOLACION  OUT ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                       p_cursorMsgValida           OUT sys_refcursor ,
                                       p_cError                    OUT VARCHAR2 ) ;

 PROCEDURE CARGA_POLITICAS_AUTORIZADA (p_nIdePol                   in poliza.idepol%type,
                                   nNumPol                     in poliza.numpol%type,
                                   cCodProd                    in poliza.codprod%type,
                                   p_cTipoOper                 in oper_pol_det.tipoop%type,
                                   p_cCodUsuario               in varchar2,
                                   -- Variables de retorno
                                   p_TABLA_AUTORIZA_VIOLACION  OUT ACSELX.TDU_TABLA_AUTORIZA_VIOLACION,
                                   p_cursorMsgValida           OUT sys_refcursor ,
                                   p_cError                    OUT varchar2 ) ;

 FUNCTION VALIDA_FACULTATIVO (p_nIdePol  Poliza.Idepol%TYPE) RETURN NUMBER ;


  PROCEDURE ACTUALIZA_COMISION(  p_nIdePol         IN POLIZA.IDEPOL%TYPE ,
                                 p_nNumOper        IN oper_pol.Numoper%TYPE
                               ) ;

 PROCEDURE PRIMA_PACTADA( p_nIdePol           IN POLIZA.IDEPOL%TYPE,
                           p_nNumCert         IN CERTIFICADO.NUMCERT%TYPE,
                           p_nMtoPrimaPactada IN OPER_POL.MTOOPER%TYPE,
                           p_cCodReca         IN RECA_DCTO.CODRECADCTO%TYPE,
                           p_cCodDcto         IN RECA_DCTO.CODRECADCTO%TYPE,
                           p_cTxtAdicional    IN VARCHAR2,
                           p_cRetorno         OUT VARCHAR2
                          ) ;

  PROCEDURE DEV_RESP_PAGO ( p_nIdePol         IN POLIZA.IDEPOL%TYPE,
                            p_nNumCert        IN Certificado.NumCert%TYPE DEFAULT 1,  --<RTC 316878> / CBayona / 26-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaración de Existencias Tipo B
                            p_TABLA_RESP_PAGO OUT ACSELX.TDU_TABLA_RESP_PAGO,
                            p_cError          OUT VARCHAR2
                        )     ;

  PROCEDURE PRORROGA_VIGENCIA ( p_nIdePol               IN Poliza.Idepol%TYPE,
                              p_cNumTramite           IN POLIZA_TRAMITE.NUMTRAMITE%TYPE,
                              p_dfecfinvig            IN POLIZA.FECFINVIG%TYPE,
                              p_cCodUsuario           IN VARCHAR2,
                              p_cError                OUT varchar2) ;


-- <F RTC 313089> jbenel

  PROCEDURE ACT_NUMOPER_AUTORIZA_VIOLACION(p_nIdepol POLIZA.IDEPOL%TYPE, p_nNumOper OPER_POL.NUMOPER%TYPE) ; -- <RTC 313346>

-- <I RTC 313092> rbarreda
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
);

PROCEDURE FR_GENERA_ENDOSO (
  p_nIdePol         POLIZA.IdePol%TYPE,
  p_nNumCert        CERT_RAMO.NumCert%TYPE,
  p_cCodRamoCert    CERT_RAMO.CodRamoCert%TYPE,
  p_cCodPlan        CERT_RAMO.CodPlan%TYPE,
  p_cCodRevPlan     CERT_RAMO.RevPlan%TYPE,
  p_dFecIniVig      DATE,
  p_dFecFinVig      DATE,
  p_cEndoso         CHAR,
  p_cCodProd        POLIZA.CodProd%TYPE
);

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
  p_nNumSesion            VARCHAR,
  p_cError                OUT varchar2
);

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
  p_cValCodTab   in datos_particulares.valcodtab%type,
  p_cError       out  varchar2
);

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
);

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
);
  --<I RTC-314183> Mauro Zegarra - 11/10/2022 - Inclusion de Equipos TREC
  procedure mod_datparticulares(p_idepol       poliza.idepol%type,
                                p_numcert      cert_ramo.numcert%type,
                                p_fec_ini_end  date,
                                p_fec_fin_end  date,
                                p_codramocert  cert_ramo.codramocert%type,
                                p_datos_partic tdu_tabla_datos_partic,
                                p_numid        usuario.numid%type,
                                p_mensajes     out sys_refcursor,
                                p_error        out varchar2);
  --<F RTC-314183>

PROCEDURE MOD_DATPARTICULARES(
  p_nIdePol               POLIZA.IdePol%TYPE,
  p_nNumCert              CERT_RAMO.NumCert%TYPE,
  p_dFecIniVig            DATE,
  p_dFecFinVig            DATE,
  p_cCodRamoCert        CERT_RAMO.CodRamoCert%TYPE,
  p_TABLA_DATOS_PARTIC    TDU_TABLA_DATOS_PARTIC,
  p_cError                OUT varchar2
);

PROCEDURE GRABAR_ANEXO_POLIZA(
  p_nIdePol              in  poliza.idepol%type,
  p_TABLA_ANEXO_POL      in  TDU_TABLA_ANEXO_POL,
  p_cCodUsuario          in  varchar2,
  p_cursorMsgValida      OUT sys_refcursor ,
  p_cError               OUT varchar2
);

PROCEDURE Activar_Mod_Cobert(
  p_nIdePol         MOD_COBERT.IdePol%TYPE,
  p_cCodProd        POLIZA.CodProd%TYPE,
  p_nNumCert        MOD_COBERT.NumCert%TYPE,
  p_cCodRamoCert    MOD_COBERT.CodRamoCert%TYPE,
  p_dFecFinValid    DATE,
  p_nIdeCobert      MOD_COBERT.IdeCobert%TYPE,
  p_NumMod          MOD_COBERT.NumMod%TYPE
);

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
  p_nTipTasa             NUMBER);

PROCEDURE Actualizar(p_nPrima COBERT_CERT.Prima%TYPE,
                       p_nPrimaMoneda COBERT_CERT.PrimaMoneda%TYPE,
                       p_nPrimaMonedaCob COBERT_CERT.PrimaMonedaCob%TYPE,
                       p_nTasa COBERT_CERT.Tasa%TYPE,
                       p_nSumaAseg COBERT_CERT.SumaAseg%TYPE,
                       P_nSumaAsegMoneda COBERT_CERT.SumaAsegMoneda%TYPE,
                       p_nNumMod MOD_COBERT.NumMod%TYPE,
                       p_cCodCobert COBERT_CERT.CodCobert%TYPE,
                       p_nIdeCobert COBERT_CERT.IdeCobert%TYPE,
                       p_nIdePol    COBERT_CERT.IdePol%TYPE,
                       p_nNumCert   COBERT_CERT.NumCert%TYPE,
                       p_cCodRamoCert COBERT_CERT.CodRamoCert%TYPE
                       );

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
    );

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
  );

-- <I RTC 313092> rbarreda 2022.05.24 Actualización de parámetros
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
  );
-- <F RTC 313092> rbarreda 2022.05.24 Actualización de parámetros
-- <F RTC 313092> rbarreda

-- <INI RTC 313091> FSalinas
-- <I RTC 313089> jbenel 20-04-2022 Actualización de parámetros
PROCEDURE Mod_Nuevo_Formato(  p_nIdePol                 IN Poliza.IdePol%TYPE,
                              p_TABLA_POLIZA_OPERACION  IN ACSELX.TDU_TABLA_POLIZA_OPERACION,
                              p_cError                  OUT VARCHAR2) ;

-- <F RTC 313089> jbenel
-- <FIN RTC 313091> FSalinas


-- <INI RTC 313344 > RBarreda
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
--<I RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaración de Existencias Tipo B, Suma Asegurada VB Tech Core
  p_Aprobado    Varchar2,
  p_Sustento    Varchar2 ,
  p_usuarioaprob Varchar2 ,
--<F RTC 316878> Manuel Navarro: CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaración de Existencias Tipo B, Suma Asegurada VB Tech Core
  p_cError      OUT VARCHAR2
);

PROCEDURE EXCLUYE_ENDOSATARIO_CERT (
  p_nIdePol                        ENDOSATARIO_CERT.idePol%TYPE,
  p_nNumCert                       ENDOSATARIO_CERT.numCert%TYPE,
  p_TABLA_ENDOSATARIO_CERT         acselx.TDU_TABLA_ENDOSATARIO_CERT,
  p_dFecExc                        DATE,
  p_cCodMotExc                     ENDOSATARIO_CERT.CODMOTVEXCLU%TYPE,
  p_cTxtMotExc                     ENDOSATARIO_CERT.TEXTOMOTVEXCLU%TYPE
  ,
  p_cError                         IN OUT VARCHAR2
);
-- <FIN RTC 313344 > RBarreda

-- <INI RTC 313089> rbarreda
PROCEDURE TramiteVincular(
  p_nIdePol       POLIZA_TRAMITE.IdePol%TYPE,
  p_nNumOper      POLIZA_TRAMITE.NumOper%TYPE,
  p_cNumTramite   POLIZA_TRAMITE.NumTramite%TYPE
  ,
  p_cError        OUT VARCHAR2
);
--
PROCEDURE TramiteDesvincular(
  p_tTablaPolizaTramite   acselx.TDU_TABLA_POLIZA_TRAMITE
  ,
  p_cError                OUT VARCHAR2
);
-- <FIN RTC 313089> rbarreda

-- <ini RTC 313746> mzegarra -----------------------------------------------------------------------
procedure modificar_clausula(p_idepol    poliza.idepol%type,
                             p_numcert   certificado.numcert%type,
                             p_clausulas tdu_tabla_clau_cert,
                             p_numid     usuario.numid%type,
                             p_mensajes  out sys_refcursor,
                             p_error     out varchar2);
-- <fin RTC 313746> mzegarra -----------------------------------------------------------------------

-- <I RTC 313742 > jbenel 14-06-2022
  PROCEDURE AJUSTE_PORC_SUMA_LOCAL( p_nIdePol CERTIFICADO.IDEPOL%TYPE,
                                    p_nNumCert CERTIFICADO.NUMCERT%TYPE) ;

-- <F RTC 313742 > jbenel 14-06-2022

-- <I RTC 313742 > jbenel 06-06-2022

-- <I RTC 313742> jbenel 26-09-2022
/*
  PROCEDURE MODIFICACION_LOCALES( p_nIDEPOL                 POLIZA.IDEPOL%TYPE,
                                  p_nNUMCERT                CERTIFICADO.NUMCERT%TYPE,
                                  p_cCodRamoCert            CERT_RAMO.CodRamoCert%TYPE,
                                  p_nSumAsegNew             NUMBER,
                                  p_TABLA_DATOS_PARTIC      IN TDU_TABLA_DATOS_PARTIC,
                                  p_TABLA_DIREC_RIESGO_CERT IN TDU_TABLA_DIREC_RIESGO_CERT,
                                  p_tTABLA_BIENESXCERT      IN TDU_T_TABLA_BIENESXCERT,
                                  p_cCodUsuario             IN varchar2,
                                  --
                                  p_cursorMsgValida         OUT sys_refcursor ,
                                  p_cError                  OUT varchar2

                                  ) ;
*/

  PROCEDURE MODIFICACION_LOCALES ( p_nIDEPOL               IN POLIZA.IDEPOL%TYPE,
                                  p_cCODPROD               IN POLIZA.CODPROD%TYPE,
                                  p_cCodMoneda             IN POLIZA.CODMONEDA%TYPE,
                                  P_TDU_TABLA_IMP_LOCAL    IN ACSELX.TDU_TABLA_IMP_LOCAL ,
                                  p_tTABLA_BIENESXCERT     IN ACSELX.TDU_T_TABLA_BIENESXCERT,
                                  p_tabla_bien_cert_riesgo IN ACSELX.tdu_tabla_bien_cert_riesgo ,
                                  p_cCodUsuario            IN varchar2,
                                  -- Variables de retorno
                                  p_cursorMsgValida         OUT sys_refcursor ,
                                  p_cError               OUT varchar2) ;

-- <I RTC 313742> jbenel 26-09-2022

  PROCEDURE CAMBIO_DATOS_GRALES_CERT2 (p_nIdePol     IN POLIZA.IDEPOL%type,
                                      p_nNumcert    IN certificado.numcert%type,
                                      p_ctipomodif  IN VARCHAR2,
                                      p_tabla_poliza_cliente IN TDU_TABLA_POLIZA_CLIENTE,
                                      p_dfecexc         IN poliza_cliente.fecexc%type,
                                      p_ccodmotvexc     IN poliza_cliente.CodMotvExc%type,
                                      p_TABLA_RESP_PAGO IN TDU_TABLA_RESP_PAGO,
                                      --
                                      p_cCodUsuario     IN  Varchar2,
                                      p_cursorMsgValida OUT sys_refcursor ,
                                      p_cError          OUT VARCHAR2)
                                      ;
-- <F RTC 313742 > jbenel 06-06-2022

-- <ini RTC 313345> mzegarra -----------------------------------------------------------------------
  procedure incluir_anexo_certificado(p_idepol   poliza.idepol%type,
                                      p_numcert  certificado.numcert%type,
                                      p_anexos   tdu_tabla_anexo_cert,
                                      p_numid    usuario.numid%type,
                                      p_mensajes out sys_refcursor,
                                      p_error    out varchar2);
-- <fin RTC 313345> mzegarra -----------------------------------------------------------------------

-- <ini RTC 313350> rbarreda -----------------------------------------------------------------------
PROCEDURE ModificacionCorredor(
  p_nIdePol     POLIZA.IdePol%TYPE,
  p_nNumId      INTERMEDIACION_POL.NUMID%TYPE,
  p_cCodAgencia INTERMEDIACION_POL.CODAGENCIA%TYPE,
  p_cCodOfiSusc INTERMEDIACION_POL.CODOFISUSC%TYPE,
  p_cIndLider   INTERMEDIACION_POL.INDLIDER%TYPE DEFAULT 'S',
  p_nPorcPart   INTERMEDIACION_POL.PORCPART%TYPE DEFAULT 100,
  p_cCodCanal  INTERMEDIACION_POL.CodCanal%TYPE DEFAULT NULL,
  p_cTipoCanal  INTERMEDIACION_POL.TipoCanal_MC%TYPE DEFAULT NULL,
  p_cError      OUT VARCHAR2
);
-- <ini RTC 313350> rbarreda -----------------------------------------------------------------------
  -- <I RTC 314182> Mauro Zegarra / 28-06-2022 / Endoso de Cambio de Comisión
  procedure modificar_comision(p_idepol      poliza.idepol%type,
                               p_porccom     comision_especial.porccom%type,
                               p_numtramite  com_esp_tramite.numtramite%type,
                               p_autorizador comision_especial.usuautor%type,
                               p_codusuario  varchar2, -- <RTC 314182> Mauro Zegarra
                               p_mensajes    out sys_refcursor,
                               p_error       out varchar2);
  --------------------------------------------------------------------------------------------------
  procedure revertir_emision(p_idepol oper_pol.idepol%type);
  --------------------------------------------------------------------------------------------------
  procedure cambiar_comision(p_idepol      poliza.idepol%type,
                             p_porccom     comision_especial.porccom%type,
                             p_numtramite  com_esp_tramite.numtramite%type,
                             p_autorizador comision_especial.usuautor%type);
  -- <F RTC 314182> mzegarra

  --<I RTC 314175 > Jorge Benel / 24-06-2022 / Endoso de Cambio de Valores Declarados

  PROCEDURE EXPORTA_ARCHIVO_LOCALES (p_nIDEPOL      POLIZA.IDEPOL%TYPE,
                                     p_nNUMCERTINI  CERTIFICADO.NUMCERT%TYPE,
                                     p_nNUMCERTFIN  CERTIFICADO.NUMCERT%TYPE,
                                     p_cCODPROD     POLIZA.CODPROD%TYPE,
                                     p_cESTADO      POLIZA.STSPOL%TYPE,
                                     -- Variables de retorno
                                     p_TDU_TABLA_EXP_LOCAL     OUT TDU_TABLA_EXP_LOCAL,
                                     p_cError                  OUT varchar2
                                     );

  PROCEDURE MODICAR_VALORES_DECLARADOS ( p_nIDEPOL      IN POLIZA.IDEPOL%TYPE,
                                       p_cCODPROD             IN POLIZA.CODPROD%TYPE,
                                       p_cCodMoneda           IN POLIZA.CODMONEDA%TYPE,
                                       P_TDU_TABLA_IMP_LOCAL  IN TDU_TABLA_IMP_LOCAL ,
                                       p_tTABLA_BIENESXCERT     IN ACSELX.TDU_T_TABLA_BIENESXCERT,   --<RTC 314175>  Jorge Benel 06-09-2022
                                       p_tabla_bien_cert_riesgo IN ACSELX.tdu_tabla_bien_cert_riesgo , -- <RTC 314175> Jorge Benel  06-09-2022

                                       -- Variables de retorno
                                       p_cError               OUT varchar2) ;

   PROCEDURE ACT_DATOS_PARTICULARES_VALDEC (  p_nIdePol               POLIZA.IdePol%TYPE,
                                            p_nNumCert              CERT_RAMO.NumCert%TYPE,
                                            p_cCodRamoCert          CERT_RAMO.CodRamoCert%TYPE,
                                            --<I RTC-316106> / CBayona / 23-01-2023 / Nuevos parámetros
                                            p_dFecIniVig            DATE,
                                            p_dFecFinVig            DATE,
                                           --<F RTC-316106> / CBayona / 23-01-2023
                                            p_TABLA_DATOS_PARTIC    TDU_TABLA_DATOS_PARTIC,
                                            p_cError                OUT varchar2 ) ;
  --<F RTC 314175 >

  -- <I RTC 313346 > -- jbenel / 24-06-2022 / Nueva Validación para el Endoso por Cambio de Contratante

  PROCEDURE VALIDA_CONTRATANTE2 ( p_nIdePol              IN poliza.idepol%type,
                                  p_cTipoModif           IN varchar2,
                                  p_TABLA_POLIZA_CLIENTE IN TDU_TABLA_POLIZA_CLIENTE,
                                  --
                                  p_cursorMsgValida      OUT sys_refcursor ,
                                  p_cError               OUT varchar2 )  ;
  -- <F RTC 313346 >

-- <I RTC 313344> Ronald Barreda / 30-06-2022/ Se añadio procedimiento REVERTIR_OPERACION como parte de anulación de endoso
PROCEDURE REVERTIR_OPERACION(
  p_nNumOper  OPER_POL.NUMOPER%TYPE,
  p_nIdepol   POLIZA.IdePol%TYPE,
  p_nNumCert  CERT_RAMO.NumCert%TYPE,
    p_cError    OUT VARCHAR2
);
-- <F RTC 313344 >

--<I RTC 314175>  Jorge Benel / 08-07-2022 / Endoso de Valores Declarados
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
  );
--<F RTC 314175>

--<I RTC-316106> / CBayona / 23-01-2023 / Genera bienes
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
  );
--<F RTC-316106> / CBayona / 23-01-2023

--<I RTC 314179> Jorge Benel / 12-07-2022 / Endoso de Traslado de Bienes
PROCEDURE ENDOSO_TRASLADO_BIENES ( p_nIDEPOL              IN POLIZA.IDEPOL%TYPE,
                                   p_cCODPROD             IN POLIZA.CODPROD%TYPE,
                                   p_cCodMoneda           IN POLIZA.CODMONEDA%TYPE,
                                   P_TDU_TABLA_IMP_LOCAL  IN TDU_TABLA_IMP_LOCAL ,
                                   -- Variables de retorno
                                   p_cError               OUT varchar2) ;
--<F RTC 314179>

--<I RTC 314184> CBayona / 14-07-2022 / Endosos Declaración de Existencias Tipo A y B
FUNCTION ExisteBien (p_nIdePol IN NUMBER,
                     p_nNumCert IN NUMBER,
                     p_cDescBien IN VARCHAR2 DEFAULT 'EXISTENCIAS')
                     RETURN BOOLEAN;
--<F RTC 314184>

--<I RTC 314183> Mauro Zegarra - 19/07/2022 - Inclusion de Equipos TREC
  function modificar_prima(p_idepol poliza.idepol%type, p_prima oper_pol.mtooper%type)
    return oper_pol.numoper%type;

  procedure recargar_descontar(p_idepol  poliza.idepol%type,
                               p_numcert certificado.numcert%type,
                               p_prima   oper_pol.mtooper%type);
--<F RTC 314183>

--<I RTC-313349> Jorge Benel / 20-07-2022 / Creacion de Cambio de responsable de pago
 PROCEDURE CAMBIO_RESP_PAGO(p_nIdePol     IN POLIZA.IDEPOL%type,
                            p_nNumcert    IN certificado.numcert%type,
                            p_TABLA_RESP_PAGO IN TDU_TABLA_RESP_PAGO,
                            p_cCodUsuario     IN  Varchar2,
                            --
                            p_cursorMsgValida      OUT sys_refcursor , -- <RTC 313349> jorge benel / 19-09-2022 / Inclusión de cursor de salida
                            p_cError          OUT VARCHAR2) ;
--<F RTC-313349>
--<I RTC-314181> Mauro Zegarra - 02/08/2022 - Modificación de Deducible
  procedure modificar_deducible(p_idepol       poliza.idepol%type,
                                p_numcert      certificado.numcert%type,
                                p_fecini       certificado.fecing%type,
                                p_fecfin       certificado.fecfin%type,
                                p_deducibles   tdu_tabla_deducible_cobert,
                                p_txtadicional varchar2,
                                p_codusuario   varchar2,
                                p_mensajes     out sys_refcursor,
                                p_error        out varchar2);
--<F RTC-314181>

--<I RTC 313752> CBayona / 22-08-2022 / Se agregó procedimiento para validar coberturas
  FUNCTION Valida_Cobertura (p_nIdePol IN Poliza.IdePol%TYPE,
                             p_nNumCert IN Certificado.NumCert%TYPE,
                             p_cCodRamo IN Cobert_Plan_Prod.CodRamoPlan%TYPE,
                             p_cCodCobert IN Cobert_Plan_Prod.CodCobert%TYPE)
                             RETURN VARCHAR2;
--<F RTC 313752>

--  <I RTC-314951>  Jorge Benel /  12-09-2022 / Creación de Endoso de Anulación

FUNCTION EXISTE_NUMOPER ( p_nIdepol  poliza.idepol%TYPE,
                          p_nNumoper oper_pol.numoper%TYPE) RETURN BOOLEAN;

PROCEDURE ANULAR_ENDOSO(
  p_nIdepol          POLIZA.IdePol%TYPE,
  p_nNumOper         OPER_POL.NUMOPER%TYPE,
  p_cTextoAnexo      VARCHAR2 ,
  p_cCodUsuario      VARCHAR2,
  p_cursorMsgValida  OUT sys_refcursor ,
  p_cError           OUT VARCHAR2
  ) ;

--  <I RTC-314951>

-- <I RTC 313742> jbenel 26-09-2022
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
      p_cPropieedificacion IN DIREC_RIESGO_CERT.PROPIEEDIFICACION%TYPE, --<RTC-315519> David Yupanqui / 08-11-2022 / Mejoras en Acsel/x para soportar cambios nueva versión Carga SBS
      --
      p_nNumCert IN OUT DIREC_RIESGO_CERT.NumCert%TYPE,
      p_cError IN OUT VARCHAR2 --<RTC-316106> / CBayona / 23-01-2023 / Mensaje de error


    ) RETURN NUMBER ;

-- <F RTC 313742> jbenel 26-09-2022
  --<I RTC-314183> Mauro Zegarra - 11/10/2022 - Inclusion de Equipos TREC
  procedure val_certificado(p_numcert certificado.numcert%type);

  procedure val_ramo(p_idepol      poliza.idepol%type,
                     p_numcert     certificado.numcert%type,
                     p_codramocert cert_ramo.codramocert%type);

  procedure val_criterio(p_idepol      poliza.idepol%type,
                         p_numcert     certificado.numcert%type,
                         p_codramocert cert_ramo.codramocert%type,
                         p_codcrit     criterio.codcrit%type);

  procedure val_vigencia_endoso(p_idepol      poliza.idepol%type,
                                p_numcert     certificado.numcert%type,
                                p_codramocert cert_ramo.codramocert%type,
                                p_fec_ini_end date,
                                p_fec_fin_end date);

  procedure val_mto_operacion(p_mtooper  oper_pol.mtooper%type,
                              p_proceso  varchar2,
                              p_mensajes out sys_refcursor,
                              p_error    out varchar2);
  --<F RTC-314183>

  --<I RTC 314184> / CBayona / 19-10-2022 / Procedimiento para Tipo B
  --Actualizar las fechas de inicio y fin de vigencia los relacionados despues de hacer la copia de un certificado.
  PROCEDURE Actualizar_Cert_Relacionados (p_nIdePolOrig  IN Poliza.IdePol%TYPE,
                                          p_nNumCertOrig IN Certificado.NumCert%TYPE,
                                          p_dFecIni      IN Certificado.FecIng%TYPE,
                                          p_dFecFin      IN Certificado.FecFin%TYPE,
                                          p_nNumCertMax  IN Certificado.NumCert%TYPE);

  --Procedimiento para hacer la copia de un certificado.
  PROCEDURE CopiaCertificado(
    p_nIdePol IN Poliza.IdePol%TYPE,
    p_nNumCert IN Certificado.NumCert%TYPE,
    p_cCodCli IN Cliente.CodCli%TYPE,
    p_dFecIniVig IN DATE,
    p_dFecFinVig IN DATE,
    p_nNumCopias IN NUMBER DEFAULT 1,
    --
    p_nNewNumCert OUT NUMBER,
    p_cError IN OUT VARCHAR2);

  --Función que valida que existe un certificado existente de Tipo B (vigencia mensual).
  FUNCTION Existe_Cert_DE_Tipo_B(p_nIdePol IN Poliza.IdePol%TYPE,
                                 p_nNumCert IN Certificado.NumCert%TYPE DEFAULT NULL,
                                 p_cStsCert IN VARCHAR2 DEFAULT 'ACT')
                                 RETURN NUMBER;
  --<F RTC 314184>

  --<I RTC 316106> / CBayona / 31-01-2023 / Función CodAutorizador
  --Función que devuelve el código del usuario autorizador
  FUNCTION CodAutorizador(p_nIdePol POLIZA.IdePol%TYPE,
                        p_nNumOper OPERACION.IdeOp%TYPE)
  RETURN USUARIO.CodUsr%TYPE;
  --<F RTC 316106>

  --<I RTC 316449> / CBayona / 17-02-2023 / Procedimiento Obtener_Firma
  --Procedimiento que devuelve la firma en base al reporte indicado
  PROCEDURE Obtener_Firma(p_nIdePol IN Poliza.IdePol%TYPE,
                          p_cNomRpt IN VARCHAR2,
                          p_cCodFirma IN VARCHAR2, --<RTC 316449> / CBayona / 22-02-2023 / Se agrega nuevo parámetro
                          --
                          p_cFirma OUT NOCOPY CLOB);
  --<F RTC 316449>

  --<I RTC 316878> / CBayona / 12-05-2023 / CORRECTIVO: Error al aplicar recargo/descuento en el Endoso de Declaración de Existencias Tipo B
  --Procedimiento que corrige las coberturas que no debieron ser movidas por los datos particulares (simil a lo que hace el emisor)
  PROCEDURE Corregir_Coberturas (p_nIdePol IN Poliza.IdePol%TYPE,
                                 p_nNumCert IN Certificado.NumCert%TYPE);
  --<F RTC 316878>

  --<I RTC 317178> / Christian Bayona / 13-06-2023 / Error en la obtención de datos para impresión para los casos con varios responsables de pago no está obteniendo todos los datos requeridos
  PROCEDURE Endoso_Traslado_Bienes ( p_nIdePol                IN Poliza.IdePol%TYPE,
                                     p_cCodProd               IN Poliza.CodProd%TYPE,
                                     p_cCodMoneda             IN Poliza.CodMoneda%TYPE,
                                     P_TDU_Tabla_Imp_Local    IN ACSELX.TDU_Tabla_Imp_Local,
                                     p_tTabla_BienesxCert     IN ACSELX.TDU_T_Tabla_BienesxCert,
                                     p_Tabla_Bien_Cert_Riesgo IN ACSELX.tDU_Tabla_Bien_Cert_Riesgo,
                                     -- Variables de retorno
                                     p_cError                OUT VARCHAR2);
  --<F RTC 317178>
  -- --------------------------------------------------------------------------------------------
  -- CLAUSULA C
  PROCEDURE SP_MNT_COBERT_CERT_C( p_nIdePol      IN Poliza.IdePol%TYPE,
                                  p_nNumCert     IN Certificado.NumCert%TYPE,
                                  p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
                                  p_cTipo        IN CHAR(1),
                                  p_cError       OUT VARCHAR2(3000));
  --
  PROCEDURE SP_ELIM_CLAU_CERT_C(p_nIdePol IN Poliza.IdePol%TYPE,
                                p_nNumCert     IN Certificado.NumCert%TYPE,
                                p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
                                p_cTipo        IN CHAR(1),
                                p_cError       OUT VARCHAR2(3000));
  --
  PROCEDURE SP_INCLUIR_CERTI_CLAU_C( p_nIdePol  IN Poliza.IdePol%TYPE,
                                     p_nNumCert IN Certificado.NumCert%TYPE,
                                     p_cError   OUT VARCHAR2(3000));
  --
  PROCEDURE SP_VALIDA_COBERT_PROV(p_nIdePol IN POLIZA.Idepol%TYPE,
                                  p_cMsjCobProv OUT VARCHAR2 );
  --
  FUNCTION FN_VALIDA_TREC(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2;
  --
  PROCEDURE SP_VALIDAR_DATOS(p_nIdePol IN POLIZA.Idepol%TYPE);
  --
  FUNCTION FN_MENSAJE_ACSEL(p_cTipo   VARCHAR2, 
                           p_nCodigo NUMBER, 
                           p_cParam1 VARCHAR2,       
                           p_cParam2 VARCHAR2,
                           p_cParam3 VARCHAR2) RETURN VARCHAR2;
  --
  PROCEDURE SP_ACTIVA_GARANTIAS(p_stspol POLIZA.StsPol%TYPE, p_nIdePol IN Poliza.IdePol%TYPE);
  --
  PROCEDURE SP_AUTORIZA_RESTRIC_EMI_POLIZA(p_nIdePol IN Poliza.IdePol%TYPE);
  --
  PROCEDURE SP_ACTU_OPERA_MOT_ENDOSO(p_nIdePol IN Poliza.IdePol%TYPE);
  --
  FUNCTION FN_OBTCONFREDIST(p_nIdePol IN POLIZA.IdePol%TYPE ) RETURN VARCHAR2;
  --
  FUNCTION FN_EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE, , p_cTipoOper IN LVAL.CODLVAL%TYPE) RETURN OPER_POL.NumOper%TYPE;
  --
  PROCEDURE SP_ACTUALIZAR_INDCOBESP(p_nNumOper IN OPER_POL.NumOper%TYPE);
  --
  PROCEDURE SP_ACTUALIZAR_BASERECADTCO(p_nNumOper IN OPER_POL.NumOper%TYPE);
  --
  PROCEDURE SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(p_nIdePol   IN Poliza.IdePol%TYPE,
                                            p_cTipoOper IN LVAL.CODLVAL%TYPE);
  --
  PROCEDURE SP_ACTIVAR_ACTUALIZAR_POLIZA( p_nIdePol IN Poliza.IdePol%TYPE);
  --
  PROCEDURE SP_ACTUALIZAR_POLIZA( p_nIdePol      IN Poliza.IdePol%TYPE,
                                  p_cTipoOper    IN LVAL.CODLVAL%TYPE,
                                  p_cError       OUT VARCHAR2(3000));
  -- FIN CLAUSULA C

END;
