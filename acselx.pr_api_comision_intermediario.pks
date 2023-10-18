CREATE OR REPLACE PACKAGE PR_API_COMISION_INTERMEDIARIO IS


  TYPE TRANSFER_REF IS REF CURSOR;

  FUNCTION BUSCAR_COMISIONES(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    P_cTIPOCONSULTA IN VARCHAR2,
    P_cCODPROD IN VARCHAR2,
    P_cNUMPOL IN VARCHAR2,
    P_cFECINI IN VARCHAR2,
    P_cFECFIN IN VARCHAR2,
    P_nNUMIDCLIENTE IN NUMBER,
    P_nCANTIDAD IN NUMBER,
    p_cASES IN VARCHAR2   DEFAULT 'N'-- <N3028515> Agregado por Jagner Hinostroza / CDC-00039
    ,p_nRegistros IN NUMBER   DEFAULT 100-- <N3039797> Agregado por Christian R. / CDC-00091
    ,p_nIndImp IN VARCHAR2   DEFAULT NULL-- <N3039797> Agregado por Christian R. / CDC-00091
    ,p_cCodCanal IN VARCHAR2   DEFAULT '0'-- RTC 18505 / Agregado por Jose Angeles / 23-01-2015
  ) RETURN TRANSFER_REF;

  FUNCTION NUM_PRELIQUIDACION_PENDIENTE(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    p_cASES IN VARCHAR2 -- <N3028515> Agregado por Jagner Hinostroza / CDC-00039
  ) RETURN NUMBER;

  PROCEDURE CREA_PRELIQUIDACION(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    P_cFECINI IN VARCHAR2,
    P_cFECFIN IN VARCHAR2,
    P_nNUMPRELIQ OUT NUMBER,
    p_cASES IN VARCHAR2 -- <N3028515> Agregado por Jagner Hinostroza / CDC-00039
    --<INI RTC 100411>  - Jose Angeles - 12/05/2017
    ,P_cCODPROD IN VARCHAR2 DEFAULT null,
    P_cNUMPOL IN VARCHAR2 DEFAULT null,
    P_nNUMIDCLIENTE IN NUMBER DEFAULT null,
    p_cCodCanal IN VARCHAR2   DEFAULT '0',
    p_nCantidadMax IN NUMBER DEFAULT NULL
    --<FIN RTC 100411>
    
  );

  --<I N3035973> Marlon Castro G. - 14/10/2010 - Referencia CDC-00068
  PROCEDURE CREA_OPER_INTER_PRELIQ(
    P_nNUMPRELIQ IN NUMBER,
    P_cCODUSER IN VARCHAR2,
    P_nNUMIDUSER IN VARCHAR2,
    P_nNUMIDINTER IN VARCHAR2,
    P_cORIGEN IN VARCHAR2,
    P_cTIPOPRELIQ IN VARCHAR2,
    P_CODCIA IN VARCHAR2
  );

 PROCEDURE OBTENER_FECHA_CREA_PRELIQ(
      P_nNUMPRELIQ IN NUMBER,
      P_dFecCrea    OUT VARCHAR2
    );
  --<F N3035973>

  FUNCTION DETALLE_PRELIQUIDACION(
    P_nNUMPRELIQ IN NUMBER
  ) RETURN TRANSFER_REF;

  PROCEDURE ANULA_PRELIQUIDACION
  (
    p_nNUMPRELIQ NUMBER
  );

  FUNCTION ESTADO_PRELIQUIDACION
  (
    p_nNUMPRELIQ NUMBER
  ) RETURN VARCHAR2;

  FUNCTION BUSCA_PRELIQ_PENDIENTES
  (
    P_nNUMIDINTER IN NUMBER,
    P_cFECINI IN VARCHAR2,
    P_cFECFIN IN VARCHAR2
  )  RETURN TRANSFER_REF;

  -- Create Carlos Riguetti - Requerimiento: DEFPROC-00171 - Reserva: N3035540
  FUNCTION CONSULTA_CHEQUES(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN  VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    P_cFECINI IN  VARCHAR2,
    P_cFECFIN IN  VARCHAR2,
    P_cSTSCHQ IN  VARCHAR2,
    P_cSerie  IN  VARCHAR2,
    P_nNumDoc IN  NUMBER,
    P_nNumLiq IN  NUMBER
  ) RETURN TRANSFER_REF;

  PROCEDURE OBTENER_IDDOCS_CONTENT (
    p_nNumLote NUMBER,
    p_nNumIdProv NUMBER,
    p_nCiaSeguros VARCHAR2,
    p_nSistOrig NUMERIC,
    p_cIdeDocFact OUT VARCHAR2 ,
    p_cIdeDocCert OUT VARCHAR2,
    p_cEmail OUT VARCHAR2
    ,p_cNomDocFact OUT VARCHAR2 --<N3038971><CDC-00089> MARCOS GOMEZ JIMENEZ  08/05/2012
    ,p_cNomDocCert OUT VARCHAR2 --<N3038971><CDC-00089> MARCOS GOMEZ JIMENEZ  08/05/2012
    ,p_nNumLiq VARCHAR2 DEFAULT NULL --<N3040668> Modificaciones CDC-00096 / Italo Rivas / 20-03-2013
  );

  --<I N3036860> Marlon Castro G. - 31/03/2011 - Referencia COM-00736
  PROCEDURE OBTENER_TEXTO_DOCUMENTO(p_cCodCia  IN VARCHAR2,
                                    p_cIndAse  IN VARCHAR2,
                                    p_cTipoDoc IN VARCHAR2,
                                    P_cTexto   OUT VARCHAR2);
  --<F N3036860> Marlon Castro G. - 31/03/2011 - Referencia COM-00736

  --<I N3037391> Marlon Castro G. - 04/05/2011 - Referencia CDC-00076
  FUNCTION GET_NUM_OPER_INTER(p_nNumOper    IN NUMBER,
                              p_nIdeFact    IN NUMBER,
                              p_cFecOper    IN VARCHAR2,
                              p_cTipoOper   IN VARCHAR2,
                              p_cIndAsesor  IN VARCHAR2,
                              p_cCodCia     IN VARCHAR2,
                              p_cCodMoneda  IN VARCHAR2,
                              p_cStsOper    IN VARCHAR2,
                              p_nCodInterm  IN NUMBER,
                              p_nNumOperInter  IN NUMBER
                                         ) RETURN TRANSFER_REF;

  PROCEDURE OBTENER_NUM_PRELIQ(p_nNumPreliq OUT NUMBER);

  PROCEDURE CREA_NUEVA_PRELIQ(P_nNUMPRELIQ    IN NUMBER,
                              P_nNUMOPERINTER IN NUMBER
  );
  --<F N3037391> Marlon Castro G. - 04/05/2011 - Referencia CDC-00076
  --<INI> RTC 18505 - Jose Angeles - 26/01/2014 
  FUNCTION BUSCAR_OPERACION(P_nIDEFACT  FACTURA.IDEFACT%TYPE,
                       P_nNUMOBLIG OBLIGACION_TABLE.NUMOBLIG%TYPE,
                       p_nNumOper  OPER_POL.NUMOPER%TYPE
  )RETURN OPER_POL.NUMOPER%TYPE;
  --<FIN> RTC 18505
  
  /*-----------------------------------------------------------------------------------
    Nombre       : BUSCA_PRIMA_NETA_TOTAL
    Prop?sito    : Obtiene la prima neta total
    Referencia   : RTC 30769
    Parametros   :       
              p_nNumOper    IN NUMBER          Numero de Operacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10/03/2015       Eddy Cordova           Creaci?n 
    -----------------------------------------------------------------------------------*/  
  FUNCTION BUSCA_PRIMA_NETA_TOTAL(
           p_codProd VARCHAR2,
           p_numPol VARCHAR2
  )RETURN NUMBER;

  --<INI> RTC 48851 - Jose Angeles - 04/11/2015 
  FUNCTION BUSCAR_PRODUCTOS(
           p_codProd  PRODUCTO.CODPROD%TYPE,
           p_desProd  PRODUCTO.Descprod%TYPE
  )RETURN TRANSFER_REF;
  --<FIN> RTC 48851

  --<I RTC 71288> Gabriel D?az Diez - 07/06/2016 - Suspensi?n de pago a Corredores por Norma SUSALUD 134-2015-SUSALUDS - AX
  FUNCTION FN_TRAESUSPEN_RS (p_NumIdAseg TERCERO.NUMID%TYPE,
                             p_NumIdBrok TERCERO.NUMID%TYPE,
                             p_CodProd   POLIZA.CODPROD%TYPE,
                             p_NumPol    VARCHAR2) RETURN VARCHAR2;
  --<F RTC 71288>

 /*-----------------------------------------------------------------------------------
   Nombre       : FR_VAL_BLOQ_SBS_INTER
   Prop?sito    : Valida Bloqueo por la SBS
   Referencia   : RTC 82636
   Parametros   :       
             p_nNumId    IN NUMBER          N?mero de Identificacion Unico del corredor
             p_ccodCia   IN VARCHAR2        C?digo de Compa?ia

   Log de Cambios
   Fecha            Autor                  Descripcion
   ----------       -----------------      -------------------------------------------
   20/10/2016       Jos? Delgado           <RTC-82636> Creaci?n
   -----------------------------------------------------------------------------------*/  
  FUNCTION FR_VAL_BLOQ_SBS_INTER(
                                  p_nNumId   NUMBER
                                 ,p_ccodCia  VARCHAR2
                                ) RETURN NUMBER;


 /*-----------------------------------------------------------------------------------
   Nombre       : FR_VAL_OTR_BLOQ_INTER
   Prop?sito    : Valida Otros Bloqueos del Intermediario
   Referencia   : RTC 82636
   Parametros   :       
             p_nNumId    IN NUMBER          N?mero de Identificacion Unico del corredor
   Log de Cambios
   Fecha            Autor                  Descripcion
   ----------       -----------------      -------------------------------------------
   20/10/2016       Jos? Delgado           <RTC-82636> Creaci?n
   -----------------------------------------------------------------------------------*/  
  FUNCTION FR_VAL_OTR_BLOQ_INTER(
                                  p_nNumId   NUMBER
                                 ) RETURN NUMBER;
--<INI RTC 100411>  - Jose Angeles - 10/05/2017
  FUNCTION DETALLE_PRELIQUIDACION_CORTO(
    P_nNUMPRELIQ IN NUMBER
  ) RETURN TRANSFER_REF;
--<FIN RTC 100411>

  -- --------------------------------------------------------------------------------------------
  -- CLAUSULA C
    /*------------------------------------------------------------------------
  Nombre     : SP_MNT_COBERT_CERT_C
  Propósito  : Procedimiento para eliminar coberturas y agregar la nueva cobertura con clausula C
  Referencia : Formulario MANTCOBC.fmb
  Parámetros :
               Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
               p_cCodRamoCert VARCHAR2  Codigo de ramo asociado al certificado de una poliza.
               p_cTipo        VARCHAR2  Tipo PAÍSES EXCLUIDOS(P) o RETROACTIVIDAD(R)
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción 
    22/09/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
  PROCEDURE SP_MNT_COBERT_CERT_C( p_nIdePol      IN Poliza.IdePol%TYPE,
                                  p_nNumCert     IN Certificado.NumCert%TYPE,
                                  p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
                                  p_cTipo        IN CHAR(1),
                                  p_cError       OUT VARCHAR2(3000));
  --
  
    /*------------------------------------------------------------------------
  Nombre     : SP_ELIM_CLAU_CERT_C
  Propósito  : Procedimiento para agregar nueva cobertura con clausula C
  Parámetros :
               Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
               p_cCodRamoCert VARCHAR2  Codigo de ramo asociado al certificado de una poliza.
               p_cTipo        VARCHAR2  Tipo PAÍSES EXCLUIDOS(P) o RETROACTIVIDAD(R)
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    02/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
  PROCEDURE SP_ELIM_CLAU_CERT_C(p_nIdePol IN Poliza.IdePol%TYPE,
                                p_nNumCert     IN Certificado.NumCert%TYPE,
                                p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
                                p_cTipo        IN CHAR(1),
                                p_cError       OUT VARCHAR2(3000));
  --
 /*------------------------------------------------------------------------
  Nombre     : SP_INCLUIR_CERTI_CLAU_C
  Propósito  : Procedimiento para cambiar de estado al certificado con clausula C
  Parámetros :
               Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    02/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
  PROCEDURE SP_INCLUIR_CERTI_CLAU_C( p_nIdePol  IN Poliza.IdePol%TYPE,
                                     p_nNumCert IN Certificado.NumCert%TYPE,
                                     p_cError   OUT VARCHAR2(3000));
  --
    /*-------------------------------------------------------------------------
  Nombre      : SP_VALIDA_COBERT_PROV
  Proposito   : Verificar si la poliza se encuentra con cobertura provisional
  Referencia : Formulario MANTPOL.fmb - Tribu Empresas Bloqueo en la Emisión Vehicular que tengan Coberturas Provisionales Activas
  Parámetros : p_nIdePol     NUMBER    Identificador de Póliza
  Retorno    : p_cMsjCobProv VARCHAR2  Retorno de resultado del mensaje Valida Cobertura
  Log de Cambios
    Fecha          Autor            Descripcion
  11/10/2023    Robinzon Santana   Creacion
---------------------------------------------------------------------------*/
  PROCEDURE SP_VALIDA_COBERT_PROV(p_nIdePol IN POLIZA.Idepol%TYPE,
                                  p_cMsjCobProv OUT VARCHAR2 );
  --
    /*--------------------------------------------------------------------
  Nombre      : FN_VALIDA_TREC
  Proposito   : Validar el valor declarado contra monto ingreso maquinarias TREC
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
              p_nIdePol  NUMBER    Identificador de Póliza

  Fecha       Autor             Descripcion
  11/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
  FUNCTION FN_VALIDA_TREC(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2;
  --
     /*--------------------------------------------------------------------
  Nombre      : SP_VALIDAR_DATOS
  Proposito   : Validar datos para la activacion que no exista duplicida de datos particulares.
                Sol si son nuevas , son renovaciones , inclusiones , rehabilitacioines pero no 
                prorrogas
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
              p_nIdePol  NUMBER    Identificador de Póliza

  Fecha       Autor             Descripcion
  11/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
  PROCEDURE SP_VALIDAR_DATOS(p_nIdePol IN POLIZA.Idepol%TYPE);
  --
   /*--------------------------------------------------------------------
  Nombre      : FN_MENSAJE_ACSEL
  Proposito   : Enviar el mensaje de error
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
               p_cTipo    VARCHAR2 Tipo de mensaje
               p_nCodigo  NUMBER  Codigo del mensaje
               p_cParam1  VARCHAR2 Parametro 1
               p_cParam2  VARCHAR2 Parametro 2
               p_cParam3  VARCHAR2 Parametro 2

  Fecha       Autor             Descripcion
  14/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
  FUNCTION FN_MENSAJE_ACSEL(p_cTipo   VARCHAR2, 
                           p_nCodigo NUMBER, 
                           p_cParam1 VARCHAR2,       
                           p_cParam2 VARCHAR2,
                           p_cParam3 VARCHAR2) RETURN VARCHAR2;
--
  /*---------------------------------------------------------------------------------------------
  Nombre      : [PROYECTO TECH CORE] COR103-925 - Integración con Satelites InsPAT
  Autor       : B2B - Werner Tito
  Fecha       : 29-08-2023
  Proposito   : Activar las Garantias de la Poliza
  Referencias : Reprogramacion AcselX
  Log de Cambios
  Fecha           Autor                       Descripcion
  29/08/2023          Werner Tito   [PROYECTO TECH CORE] COR103-925 - Integración con Satelites InsPAT
----------------------------------------------------------------------------------------------*/
  PROCEDURE SP_ACTIVA_GARANTIAS(p_stspol POLIZA.StsPol%TYPE, p_nIdePol IN Poliza.IdePol%TYPE);
  --
  /*------------------------------------------------------------------------
  Nombre     : SP_AUTORIZA_RESTRIC_EMI_POLIZA
  Propósito  : Procedimiento para dar permisos a la autorización de recctricciones para la emision de la póliza.
  Referencia : EMI00005.FMB
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
  Log de Cambios:
    Fecha         Autor               Descripción 
    10/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
  PROCEDURE SP_AUTORIZA_RESTRIC_EMI_POLIZA(p_nIdePol IN Poliza.IdePol%TYPE);
    --
  /*-------------------------------------------------------------------------------
  Nombre      : FN_IND_FACULTATIVO
  Proposito   : Valida si existe recalculo para el Facultativo.
  Log de Cambios 
    Fecha            Autor           Descripciión
  17/08/2017    Brayan Pereda   Creación <RTC-103795>
  -------------------------------------------------------------------------------*/
  FUNCTION FN_IND_FACULTATIVO (p_nIdePol  CERTIFICADO.IdePol%TYPE, 
                                  p_nNumCert CERTIFICADO.NumCert%TYPE,
                                  p_cCodRamoCert dist_rea.codramocert%TYPE,
                                  p_cCodRamoRea dist_rea.codramorea%TYPE, 
                                  p_nIdeRecAnt RECIBO.IdeRec%TYPE,
                                  p_nIdeRec RECIBO.IdeRec%TYPE
                                  ) RETURN NUMBER;
  --
    /*-----------------------------------------------------------------------
     Nombre     : SP_VALIDA_RECALCULO_RM 
     Proposito  : Procedimiento que se utiliza para validar si el movimiento tiene recálculo
       segun las condiciones del Reaseguro Mixto - II Fase
     Referencia : MODCOM.FMB
     Parametro  :
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     09/10/2023   ROBINZON SANTANA    Creación.
  -----------------------------------------------------------------------------*/     
  PROCEDURE SP_VALIDA_RECALCULO_RM(p_nIdePol IN Poliza.IdePol%TYPE);
  --
    /*-----------------------------------------------------------------------
     Nombre : FN_APLICA_REDIST_FINAL 
     Proposito: Genera la redistribución del reaseguro.
     Parametro :
                 p_nIdePol          Identificador de póliza.
                 p_nNumCert         Número de certificado.
                 p_cCodRamoCert     Código del ramo técnico.
                 p_nIdeRec          Identificador de recibo.
                 p_dFecIniVig       Fecha de inicio de vigencia.
                 p_dFecFinVig       Fecha final de vigencia.
                 p_cCodRamoRea      Código del ramo reaseguro.
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            Creación
     23/08/2013   Claudia Yugar       <N3041427> Creación. CNT-01702
  -----------------------------------------------------------------------------*/          
  FUNCTION FN_APLICA_REDIST_FINAL( p_nIdePol  IN POLIZA.IdePol%TYPE, 
                                    p_nNumCert      IN CERTIFICADO.NumCert%TYPE, 
                                    p_cCodRamoCert  IN RAMO.CodRamo%TYPE, 
                                    p_nIdeRec       IN RECIBO.IdeRec%TYPE, 
                                    p_dFecIniVig    IN DATE, 
                                    p_dFecFinVig    IN DATE, 
                                    p_cCodRamoRea   IN DIST_ACEP_RIESGO.CodRamoRea%TYPE
                                  ) RETURN VARCHAR2;
  --
   /*-----------------------------------------------------------------------
     Nombre     : FN_CAL_APLICA_RECALCULO 
     Proposito  : Funcion que se utiliza para saber si se Redistribuye el movimiento de la Póliza.
     Referencia : MODCOM.FMB
     Parametro  :
                 p_nIdePol         Identificador de póliza.
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     09/10/2023   Robinzon Santana     Creación
  -----------------------------------------------------------------------------*/     
  FUNCTION FN_CAL_APLICA_RECALCULO(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2;
  --
/*-----------------------------------------------------------------------
     Nombre : SP_VALIDA_RECALCULO 
     Proposito: Procedimiento que se utiliza para validar si el movimiento tiene recálculo
     Referencia : MODCOM.FMB
     Parametro :
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     09/10/2023   Robinzon Santana    Creación.
  -----------------------------------------------------------------------------*/     
  PROCEDURE SP_VALIDA_RECALCULO(p_nIdePol IN Poliza.IdePol%TYPE);
    --
  /*-------------------------------------------------------------------------------
  Nombre      : PR_API_COMISION_INTERMEDIARIO.FN_OBT_CERT_RAMO_DIST_FACULT
  Proposito   : Obtener certificado y ramo en donde no se haya colocado detalle facultativo.
  Referencia : MODCOM.FMB
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
  Log de Cambios 
    Fecha	            Autor			     Descripción
  10/10/2023       Robinzon Santana          Creación
-------------------------------------------------------------------------------*/
FUNCTION FN_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol POLIZA.IdePol%TYPE) RETURN VARCHAR2;
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACTU_OPERA_MOT_ENDOSO
  Propósito  : Procedimiento para actualizar operacion de motivo endoso
  Referencia : MODCOM.fmb
  Parámetros :
               p_nIdePol NUMBER Identificador de Póliza
  Log de Cambios:
    Fecha         Autor               Descripción 
    09/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTU_OPERA_MOT_ENDOSO(p_nIdePol IN Poliza.IdePol%TYPE);
--
  /*-----------------------------------------------------------------------
     Nombre : FN_OBTCONFREDIST 
     Proposito: Si el analista de reaseguros respondió a la validación del
                recálculo, obtendremos la respuesta (S/N).
                Si no aplica el recálculo, obtendremos por defecto N.
     Referencia : Formulario MANTPOL.fmb                       
     Parametro :
                 p_nIdePol         Identificador de póliza.
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     15/10/2023   Robinzon Santana    Creación
  -----------------------------------------------------------------------------*/ 
 FUNCTION FN_OBTCONFREDIST ( p_nIdePol     IN    POLIZA.IdePol%TYPE ) RETURN VARCHAR2;
 --
 /*-----------------------------------------------------------------------
     Nombre : FN_EJECUTAR_ACTIVACION 
     Proposito   : Funsión que ejecuta la activación de la póliza
     Referencias : MANTPOL.fmb
     Parametro :
                 p_nIdePol    NUMBER   Identificador de póliza.
				 p_cTipoOper  VARCHAR  Tipo motivo del endoso
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     Fecha         Autor               Descripción
     14/10/2023    Robinzon Santana     Creación
  -----------------------------------------------------------------------------*/   
FUNCTION FN_EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE, , p_cTipoOper IN LVAL.CODLVAL%TYPE) RETURN OPER_POL.NumOper%TYPE;
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACTUALIZAR_INDCOBESP
  Propósito  : 
  Referencia : Formulario MANTPOL.fmb
  Parámetros :
               p_nNumOper  NUMBER    Numero de operación

  Log de Cambios:
    Fecha         Autor               Descripción
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTUALIZAR_INDCOBESP(p_nNumOper   IN   OPER_POL.NumOper%TYPE);
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACTUALIZAR_BASERECADTCO
  Propósito  : 
  Referencia : Formulario MANTPOL.fmb
  Parámetros :
               p_nNumOper  NUMBER    Numero de operación

  Log de Cambios:
    Fecha         Autor               Descripción
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTUALIZAR_BASERECADTCO(p_nNumOper IN OPER_POL.NumOper%TYPE);
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACTIVAR_ACTUALIZAR_POLIZA
  Propósito  : Activar actualizar la Póliza
  Referencia : Formulario MANTPOL.fmb
                FR_ACTIVAR.Actualizar_Poliza
  Parámetros :
               p_nIdePol  NUMBER    Identificador de Póliza

  Log de Cambios:
    Fecha         Autor               Descripción
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTIVAR_ACTUALIZAR_POLIZA( p_nIdePol IN Poliza.IdePol%TYPE);
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA
  Propósito  : ACTIVAR MOTIVOS ENDOSOS VIGENCIA OPERACIO
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_cTipoOper    VARCHAR   Tipo motivo del endoso

  Referencia : MANTPOL.FMB

  Log de Cambios:
    Fecha         Autor               Descripción 
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(p_nIdePol   IN Poliza.IdePol%TYPE,p_cTipoOper IN LVAL.CODLVAL%TYPE);
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACTUALIZAR_POLIZA
  Propósito  : Actualizar la Póliza y Generar el Movimiento de Prima
  Referencia : Formulario MANTPOL.fmb
                FR_ACTIVAR.Actualizar_Poliza
  Parámetros :
               p_nIdePol   NUMBER    Identificador de Póliza
               p_cTipoOper VARCHAR   Tipo motivo del endoso
  Retorno    : p_cError   VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    04/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTUALIZAR_POLIZA( 
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_cTipoOper    IN LVAL.CODLVAL%TYPE,
  p_cError       OUT VARCHAR2(3000));
-- FIN CLAUSULA C

END PR_API_COMISION_INTERMEDIARIO;
