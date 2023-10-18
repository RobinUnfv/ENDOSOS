CREATE OR REPLACE PACKAGE PR_API_EMI_TRAN IS

  /*-----------------------------------------------------------------------
    Nombre : PR_API_EMI_TRAN
    Archivo: BWEBEMITRAN.SQL
    Autor  : Jose Arce
    Fecha  : 28-11-2003
    Proposito: Administrar extraccion de informacion para servicion WEB
    Referencia : Poryecto Fuerza de Venta MultiProducto
    Log de Cambios
           Fecha            Autor     Descripcion
           28/11/2003    Jose Arce    Creacion de Funciones :Lista_Vinculos
                                                             Lista_Docs_Identidad
                                                             Lista_Relaciones
                                                             Lista_Bloqueos
                                                             Lista_Productos
                                                             Detalle_Clientes
                                                             Lista_Direccion
                                                             Lista_Telefonos
           17/10/2005   Jose Porlles  Creacion de Funciones :LISTA_DOCUMENTOS_REL
                                                             NUM_REG_DOCUMENTOS_REL
           25/08/2006   Jorge D?az    Creaci?n de Funci?n   :CONSULTAR_CABECERA_PLANILLA
           05/09/2006   Daniel Quintana  Creacion           :DATOS_PART_TRANS_POL
                                                             DATOS_PART_TRANS_TASAS_POL
                                                             DATOS_PART_TRANS_GUERRA_POL
           28/09/2006   Jorge D?az          Adici?n de Par?metro  :CONSULTAR_CABECERA_PLANILLA
           10/10/2006   Nelson D?az         N3026089     Emi-02511 Nueva Funcion
           21/06/2010   Jose Porlles        <N3035129>   Adici?n par?metro p_ncodAseg en PROCESAR_APLICACION
           02/05/2011   Marlon Castro G.    <N3036969>   Muestra al responsable de pago como cliente - Reqm TRANS-00015
           30/05/2011   Marlon Castro G.    <N3037168>   Adecuaciones Grupo Gloria
           13/02/2012   Jhonatan Rojas T.   <N3038435>   REQ. TRANS-00027
           10/09/2012   Italo Rivas         <N3039625>   EMI-14941 Modificaciones
           14/09/2012   Italo Rivas         <N3039669>   TRANS-00033 Modificaciones
           12/10/2012   Renzo Yacarini      <N3039818>   CNT-01590
           17/01/2013   Chrisitan R.        <N3040340>   WRK-01213
           21/09/2020   Jhonatan Ninanya    <RTC-243489> Modificacion de Funciones para inclusion de datos de intermediario:
                                                         DETALLE_APLICACION
                                                         CONSULTAR_CABECERA_PLANILLA
           22/01/2021   Carlos Veliz        <RTC-284065> Modificacion en la funcion LISTA_APLICACIONES para devolver derecho de emision cero en vez de null
-------------------------------------------------------------------------*/

  -- Author  : RCARRANZA
  -- Created : 25/03/2004 12:49:25 PM
  -- Purpose : Extractores para proyecto web transportes.

  TYPE c_Cursor IS REF CURSOR ;
  --
  FUNCTION POLIZA_CERT_RAMO   (p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_cCodPar  VARCHAR2,
                               p_cCodPol  VARCHAR2,
                               p_nNumPol  NUMBER,
                               p_nNumCer  NUMBER,
                               p_cTipDoc  VARCHAR2,
                               p_cNumDoc  VARCHAR2) RETURN PR_API_EMI_TRAN.c_Cursor;
  --
   FUNCTION LISTA_APLICACIONES(p_cCodTra VARCHAR2,
                               p_cUsrLog VARCHAR2,
                               p_nUsrId  NUMBER,
                               p_cUsrRol VARCHAR2,
                               p_cUsrPer VARCHAR2,
                               p_cCodPar VARCHAR2,
                               p_nNumPol NUMBER,
                               p_nNumCer NUMBER,
                               p_cNumPla VARCHAR2,
                               p_nNumCar NUMBER,
                               p_cNumLiq VARCHAR2,
                               p_dFecIni VARCHAR2,
                               p_dFecFin VARCHAR2,
                               p_cNumTra VARCHAR2,
                               p_cTipDoc VARCHAR2,
                               p_cNumDoc VARCHAR2,
                               p_cApePat VARCHAR2,
                               p_cApeMat VARCHAR2,
                               p_cNomAse VARCHAR2,
                               p_cIndOri VARCHAR2,
                               P_nIdePol NUMBER,
                               p_cStsDec VARCHAR2 ) RETURN PR_API_EMI_TRAN.c_Cursor;
  --
   FUNCTION DETALLE_APLICACION(p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_nIdedec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
  --
   FUNCTION LISTA_COBERTURAS  (p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_nIdedec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION CONSOLIDADO_PRIMAS(p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_nIdedec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DOCUMENTOS_PAGO   (p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_nIdedec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION PROCESAR_APLICACION (p_cCodTra  VARCHAR2,
                                 p_cUsrLog  VARCHAR2,
                                 p_nUsrId   NUMBER,
                                 p_cUsrRol  VARCHAR2,
                                 p_cUsrPer  VARCHAR2,
                                 p_cTipApl  VARCHAR2,
                                 p_nIdePol  NUMBER,
                                 p_nNumCer  NUMBER,
                                 p_cCodRam  VARCHAR2,
                                 p_cNumTra  VARCHAR2,
                                 p_nNumCar  NUMBER,
                                 p_cBasAva  VARCHAR2,
                                 p_cCodMon  VARCHAR2,
                                 p_cCodEst  VARCHAR2,
                                 p_nIdeDec  NUMBER,
                                 p_cNumPla  VARCHAR2,
                                 p_cNumApl  VARCHAR2,
                                 p_dFecVia  VARCHAR2,
                                 p_dFecApl  VARCHAR2,
                                 p_cPreven  VARCHAR2,
                                 p_nPriMin  NUMBER,
                                 p_cConPro  VARCHAR2,
                                 p_cPaiPro  VARCHAR2,
                                 p_cDepPro  VARCHAR2,
                                 p_cConDes  VARCHAR2,
                                 p_cPaiDes  VARCHAR2,
                                 p_cCodDep  VARCHAR2,
                                 p_cCodTr   VARCHAR2,
                                 p_cTipTra  VARCHAR2,
                                 p_cTipMer  VARCHAR2,
                                 p_cDesMer  VARCHAR2,
                                 p_cTipEmb  VARCHAR2,
                                 p_cDesEmb  VARCHAR2,
                                 p_nTasBas  NUMBER,
                                 p_cDeduci  VARCHAR2,
                                 p_cEndosa  VARCHAR2,
                                 p_cMedTra  VARCHAR2,
                                 p_cCodBuq  VARCHAR2,
                                 p_nNumVia  NUMBER,
                                 p_nPorSeg  NUMBER,
                                 p_nDerAdu  NUMBER,
                                 p_cTransb  VARCHAR2,
                                 p_cSobCub  VARCHAR2,
                                 p_nAntBuq  NUMBER,
                                 p_cGueHue  VARCHAR2,
                                 p_cCodDto  VARCHAR2,
                                 p_nMonApl  NUMBER,
                                 p_nTasTot  NUMBER,
                                 p_nPriTot  NUMBER,
                                 p_cResPag  VARCHAR2,
                                 p_cDesApl  VARCHAR2,
                                 p_cDocRef  VARCHAR2,
                                 p_cProvee  VARCHAR2,
                                 p_cPedFac  VARCHAR2,
                                 p_cDesDes  VARCHAR2,
                                 p_nPesBru  VARCHAR2,
                                 p_ncodAseg DEC_TRANSPORTE_FLOTANTE.NUMIDASEGWEB%TYPE DEFAULT NULL
                               --<I N3039669> TRANS-00033 / Italo Rivas / 13-09-2012
                               ,p_nNumFactura      VARCHAR2 DEFAULT NULL
                               ,p_nDescNave        VARCHAR2 DEFAULT NULL
                               ,p_nPuerAerOrigen   VARCHAR2 DEFAULT NULL
                               --<F N3039669> TRANS-00033 / Italo Rivas / 13-09-2012
                                 ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DETALLE_LIQUIDACION (p_cCodTra  VARCHAR2,
                                 p_cUsrLog  VARCHAR2,
                                 p_nUsrId   NUMBER,
                                 p_cUsrRol  VARCHAR2,
                                 p_cUsrPer  VARCHAR2,
                                 p_nNumLiq  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION LIQUIDACION_PLANILLA (p_cCodTra  VARCHAR2,
                                  p_cUsrLog  VARCHAR2,
                                  p_nUsrId   NUMBER,
                                  p_cUsrRol  VARCHAR2,
                                  p_cUsrPer  VARCHAR2,
                                  p_cNumPla  VARCHAR2,
                                  p_cCodRes  VARCHAR2,
                                  p_nIdePol  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION LISTA_VALORES (p_cCodTra  VARCHAR2,
                           p_cUsrLog  VARCHAR2,
                           p_nUsrId   NUMBER,
                           p_cUsrRol  VARCHAR2,
                           p_cUsrPer  VARCHAR2,
                           p_cTipLis  VARCHAR2 ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_PART_TRANS (p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nIdePol  NUMBER,
                              p_nNumCer  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_PART_TRANS_TASAS (p_cCodTra  VARCHAR2,
                                    p_cUsrLog  VARCHAR2,
                                    p_nUsrId   NUMBER,
                                    p_cUsrRol  VARCHAR2,
                                    p_cUsrPer  VARCHAR2,
                                    p_nIdePol  NUMBER,
                                    p_nNumCer  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_PART_TRANS_GUERRA (p_cCodTra  VARCHAR2,
                                     p_cUsrLog  VARCHAR2,
                                     p_nUsrId   NUMBER,
                                     p_cUsrRol  VARCHAR2,
                                     p_cUsrPer  VARCHAR2,
                                     p_nIdePol  NUMBER,
                                     p_nNumCer  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_PART_TRANS_POL (p_cCodTra  VARCHAR2,
                                  p_cUsrLog  VARCHAR2,
                                  p_nUsrId   NUMBER,
                                  p_cUsrRol  VARCHAR2,
                                  p_cUsrPer  VARCHAR2,
                                  p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_PART_TRANS_TASAS_POL (p_cCodTra  VARCHAR2,
                                        p_cUsrLog  VARCHAR2,
                                        p_nUsrId   NUMBER,
                                        p_cUsrRol  VARCHAR2,
                                        p_cUsrPer  VARCHAR2,
                                        p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_PART_TRANS_GUERRA_POL (p_cCodTra  VARCHAR2,
                                         p_cUsrLog  VARCHAR2,
                                         p_nUsrId   NUMBER,
                                         p_cUsrRol  VARCHAR2,
                                         p_cUsrPer  VARCHAR2,
                                     p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION CONSULTA_PLANILLA (p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_cStsPla  VARCHAR2) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION RESPONSABLE_PAGO (p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION VALIDA_COASEGURO_P (p_cCodTra  VARCHAR2,
                                p_cUsrLog  VARCHAR2,
                                p_nUsrId   NUMBER,
                                p_cUsrRol  VARCHAR2,
                                p_cUsrPer  VARCHAR2,
                                p_nIdePol  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION VIAJE_BUQUES (p_cCodTra  VARCHAR2,
                          p_cUsrLog  VARCHAR2,
                          p_nUsrId   NUMBER,
                          p_cUsrRol  VARCHAR2,
                          p_cUsrPer  VARCHAR2,
                          p_cCodBuq  VARCHAR2,
                          p_cFecVia  VARCHAR2,  -- Fecha en formato yyyymmdd
                          p_cCodPai  VARCHAR2
                         ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION DATOS_ASEGURADO ( p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nNumPol  NUMBER,
                              p_cCodPro  VARCHAR2
                              ) RETURN PR_API_EMI_TRAN.c_Cursor;
   --
   FUNCTION FECHA_INI (p_nIdePol NUMBER ) RETURN DATE;
   --
   FUNCTION FECHA_FIN (p_nIdePol  NUMBER ) RETURN DATE;
   --
   FUNCTION BUSCA_BUQUE(p_cCodBuq VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION FECHA_EMISION(p_nIdeDec NUMBER) RETURN DATE;
   --
   FUNCTION INCLUIR_POLIZA(p_cUsrLog VARCHAR2,
                           p_nIdePol NUMBER,
                           p_nNumCer NUMBER,
                           p_cCodRam VARCHAR2,
                           p_cStsDec VARCHAR2,
                           p_nIdeDec NUMBER,
                           p_cResPag VARCHAR2,
                           cNumTra   VARCHAR2
                           ) RETURN VARCHAR2;
   --
   PROCEDURE REALIZAR_INCLUSION(p_cUsrLog VARCHAR2,
                                p_nIdePol NUMBER,
                                p_nNumCer NUMBER,
                                p_cCodRam VARCHAR2,
                                p_cNumTra VARCHAR2,
                                p_cStsDec VARCHAR2,
                                p_nIndDec NUMBER,
                                p_cIndPol OUT VARCHAR2,
                                p_cResPag VARCHAR2);
   --
   PROCEDURE VALIDA_POLITICAS(p_cUsrLog VARCHAR2,
                              p_nIdePol NUMBER,
                              p_nNumPol NUMBER,
                              p_cCodPro VARCHAR2,
                              p_cIndPol OUT VARCHAR2,
                              p_nIdeDec NUMBER DEFAULT NULL); -- <N3038630> HD- 103924/ Renzo Yacarini / 23-03-2012
   --
   PROCEDURE ACTIVAR_POLIZA(p_cUsrLog VARCHAR2,
                            p_nIdePol NUMBER);
   --
   FUNCTION VALIDA_PARAMETROS_PRINCIPALES(p_cCodTra VARCHAR2,
                                          p_cUsrLog VARCHAR2,
                                          p_nUsrId  NUMBER,
                                          p_cUsrRol VARCHAR2,
                                          p_cUsrPer VARCHAR2) RETURN BOOLEAN;
   --
   FUNCTION DISTRITO_CONTRATANTE(p_cCodPai  VARCHAR2, --<N3021343> se cambio los tipo de datos de todos los parametros de number a varchar2
                                 p_cCodEst  VARCHAR2,
                                 p_cCodCiu  VARCHAR2,
                                 p_cCodMun  VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION NUMERO_TRAMITE(p_nIdePol NUMBER,
                           p_nNumOpe NUMBER) RETURN VARCHAR2;
   --
   FUNCTION NOMBRE_ASEGURADO(p_nIdePol NUMBER,
                             p_nNumCer NUMBER,
                             p_nIdeRec NUMBER, -- < N3019636 > Agregar el parametro Numero de recibo
                             p_cTipDat VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION DES_RAMO(p_cCodRam VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION VALIDA_RESP_PAGO(p_cStsDec VARCHAR2,
                             p_nIdePol NUMBER,
                             p_cCodPla VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION NUMERO_OPERACION(p_nIdeRec NUMBER) RETURN NUMBER;
   --
   FUNCTION TIPO_APLICACION(p_nIdePol NUMBER,
                            p_nNumcer NUMBER,
                            p_cCodRam VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION RUC_CONTRATANTE(p_cCodCli VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION NUM_DOCUMENTO(p_nIdeFac NUMBER) RETURN VARCHAR2;
   --
   FUNCTION CONTRATANTE(p_nNumDoc NUMBER) RETURN VARCHAR2;
   --
   FUNCTION FEC_PRIMER_PAGO(p_FecIni DATE,
                            p_FecFin DATE,
                            p_FecOpe DATE) RETURN DATE;
   --
   FUNCTION DIRECCION_ASEGURADO(p_NumOpe NUMBER) RETURN VARCHAR2;
   --
   FUNCTION DISTRITO_ASEGURADO(p_NumOpe NUMBER) RETURN VARCHAR2;
   --
   FUNCTION PROVINCIA_ASEGURADO(p_NumOpe NUMBER) RETURN VARCHAR2;
   --
   FUNCTION CODIGO_BARRA(p_cCodMon VARCHAR2,
                         p_nPriPag NUMBER,
                         p_cNumDoc VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION DIGITO_CHEQUEO(codigo VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION FECHA_EN_LETRAS (dfecha DATE) RETURN VARCHAR2;
   --
   FUNCTION CODIGO_CLI_INI (p_cRolUsr VARCHAR2,
                            p_nUsrId NUMBER) RETURN VARCHAR2;
   --
   FUNCTION FORMA_PAGO(p_nIdeFac NUMBER) RETURN VARCHAR2;
   --
   FUNCTION VALIDA_CERTIFICADOS(p_nIdePol NUMBER,
                                p_nNumOpe NUMBER) RETURN VARCHAR2;
   --
   FUNCTION NUMERO_ID(p_nIdePol NUMBER,
                      p_nNumCer NUMBER ) RETURN VARCHAR2;
   --
   FUNCTION F_PAGO( p_nIdeDec NUMBER ) RETURN VARCHAR2;
   --
   FUNCTION PLAZO_VENCIDO( p_nNumPla VARCHAR2,p_cIndOri VARCHAR2 ) RETURN NUMBER;
   --
   FUNCTION DESCRIPCION_AGENCIA( p_cCodAge VARCHAR2 ) RETURN VARCHAR2;
   --
   FUNCTION NUMERO_LIQUIDACION( p_nIdeRec VARCHAR2 ) RETURN NUMBER;
   --
   PROCEDURE FR_BACKUP_RESPONSABLES(p_session NUMBER,
                                    p_cInd    VARCHAR2,
                                    p_nIdePol NUMBER,
                                    p_nNumCer NUMBER);
   --
   PROCEDURE FR_CARGA_RESP(p_nIdePol NUMBER,
                           p_NumCer  NUMBER,
                           p_CodCli  VARCHAR2,
                           p_CodPro  VARCHAR2);
   --
   PROCEDURE FR_BORRAR_TEMPORAL(p_session NUMBER);
   --
   PROCEDURE FR_BORRAR_TMP(p_IdePol NUMBER);
   --
   FUNCTION EXISTE_TEMPORAL(p_IdePol NUMBER) RETURN BOOLEAN;
   --
   FUNCTION POLITICA(p_cNumPla VARCHAR2,p_nIdePol NUMBER,p_cStsDec VARCHAR2,p_cIndOri VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION INDICADOR_PROCESO(p_nIdePol NUMBER,p_nNumCer NUMBER) RETURN VARCHAR2;
   --
   FUNCTION IND_PROCESO_PLANILLA(p_nIdePol NUMBER,p_cNumPla VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION DESC_COBERTURA(p_nIdeCob NUMBER,p_cCodRam VARCHAR2) RETURN VARCHAR2;
   --
   FUNCTION TITULO_DEC(p_nIdePol NUMBER) RETURN VARCHAR2;
   --
   FUNCTION ES_CONTRATANTE(p_nIdePol NUMBER, p_cResPag VARCHAR2) RETURN VARCHAR2;
   --
   PROCEDURE AGREGAR_CONTRATANTE(p_nIdepol NUMBER, p_cResPag VARCHAR2);

   PROCEDURE CREA_DIST_FACUL(
     p_cUsrLog VARCHAR2,
     p_nIdePol NUMBER,
     p_nNumCer NUMBER,
     p_cCodRam VARCHAR2,
     p_nIndDec NUMBER);
--<I N3026089>  Nelson D?az 10.10.2006 Emi-02511
   FUNCTION CONSULTAR_CABECERA_PLANILLA (
            p_nIdePol   POLIZA.IDEPOL%TYPE,
            p_nNumCert  DEC_TRANSPORTE_FLOTANTE.Numcert%TYPE, --<N3026002> Se agreg? el parametro n?mero de certificado
            p_nNumLiq   FACTURA.NUMFACT%TYPE,
             -- <I N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Nuevo parametro de entrada IdeDec
               p_nIdeDec DEC_TRANSPORTE_FLOTANTE.Idedec%TYPE DEFAULT NULL
              -- <F N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Nuevo parametro de entrada IdeDec
            )  RETURN PR_API_EMI_TRAN.c_Cursor; --<N3036969> Marlon Castro G. - 02/05/2011 - Muestra al responsable de pago como cliente
   FUNCTION INTERM_BLOQUEADO (p_nIdepol   poliza.Idepol%TYPE)
                            RETURN c_cursor;
--<F N3026089>  Nelson D?az 10.10.2006 Emi-02511
--<I N3026368>  Jorge D?az 27.11.2006
   FUNCTION LISTAR_EQUIVALENCIAS (
           p_nCodCorredor INTERMEDIARIO.CODINTER%TYPE
            ) RETURN PR_API_EMI_TRAN.c_Cursor;
--<F N3026368>  Jorge D?az 27.11.2006

   --<I N3037168> Marlon Castro G. - 30/05/2011
   FUNCTION GET_DATOS_ASEGURADO (
            p_cCodCli   DEC_TRANSPORTE_FLOTANTE.CODCLI%TYPE,
            p_nIdePol   DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
            p_nNumCert  DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPE,
            p_nIdeRec   DEC_TRANSPORTE_FLOTANTE.IDEREC%TYPE,
            p_cNumIdWeb DEC_TRANSPORTE_FLOTANTE.NUMIDASEGWEB%TYPE,
            p_cTipoDato VARCHAR2)
            RETURN VARCHAR2;
   FUNCTION GET_VALOR_RIM_TAB (
            p_cCodTab   RIM_TAB_GRAL.CODTAB%TYPE,
            p_cLlaveTab RIM_TAB_GRAL.LLAVETAB%TYPE)
            RETURN VARCHAR2;

   FUNCTION GET_APLICACION_TRAN(p_cIdePol POLIZA.IDEPOL%TYPE
                               )
            RETURN VARCHAR2;

   FUNCTION ES_GRUPO_GLORIA (
            p_cIdePoliza   RIM_TAB_GRAL.CODTAB%TYPE)
            RETURN VARCHAR2;
   --<I N3037168> Marlon Castro G. - 30/05/2011

-- <I N3038435> TRANS-00027 / Jhonatan Rojas T. / 07-02-2012
  FUNCTION BUSCA_ASEG_RESPAGO_CONTR(p_nIndicador VARCHAR2, p_nValor NUMBER,
           p_numCert NUMBER DEFAULT NULL)--<N3039625> EMI-14941 / Italo Rivas / 10-09-2012
    RETURN VARCHAR2;

   FUNCTION  BUSCA_DIR_TELF_DIST_RUC_ASEG(p_nIndicador VARCHAR2,p_nValor NUMBER) RETURN VARCHAR2;

   FUNCTION  GET_DETALLE_DATOS_ASEGURADO(p_nIndicador VARCHAR2,p_nValor NUMBER) RETURN VARCHAR2;
-- <F N3038435> TRANS-00027 / Jhonatan Rojas T. / 07-02-2012

 -- <I N3038620> Jhonatan Rojas Terrones / 21-03-2012
   FUNCTION BUSCA_ASEGURADO_PLANILLA(p_nValor NUMBER, p_nNumCert NUMBER,p_indicador VARCHAR2) RETURN VARCHAR2;
 -- <F N3038620> Jhonatan Rojas Terrones / 21-03-2012

 -- <I N3039818> CNT-01590 / Renzo Yacarini / 12-10-2012
 FUNCTION VALIDA_DECLARACION_FACULTATIVA(
   p_nIdePol      POLIZA.IDEPOL%TYPE,
   p_nNumcert     CERTIFICADO.NUMCERT%TYPE,
   p_cNumPla      DEC_TRANSPORTE_FLOTANTE.PLANILLA%TYPE
 ) RETURN VARCHAR2;
 -- <F N3039818> CNT-01590 / Renzo Yacarini / 12-10-2012

  -- <I N3040340> WRK-01213 / Christian R. / 17-01-2013
FUNCTION OBTENER_DATOS_TRAMA(
    idePol          DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
    numCert         DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPe,
    ideDec          DEC_TRANSPORTE_FLOTANTE.IDEDEC%TYPE,
    optipo          in varchar2,
    nrocolocacion   in varchar2,
    numidbroker     in varchar2,
    apecont         in varchar2,
    apematcont      in varchar2,
    apebroker       in varchar2,
    apematbroker    in varchar2,
    nombroker       in varchar2,
    tipoterbroker   in varchar2,
    numidgestor     in varchar2,
    numpol          in varchar2,
    nomprod         in varchar2,
    totcert         in varchar2,
    indprov         in varchar2,
    indinsp         in varchar2,
    indanexos       in varchar2,
    arearecepcion   in varchar2,
    autor           in varchar2
  ) RETURN c_Cursor;
 FUNCTION ACTUALIZAR_TRAMITE(P_ideDec          DEC_TRANSPORTE_FLOTANTE.IDEDEC%TYPE,
    p_NumCert         DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPE,
    p_idePol          DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
    p_NumTramite      DEC_TRANSPORTE_FLOTANTE.NUMTRAMITE%TYPE) RETURN VARCHAR2;
FUNCTION OBTENER_DATOS_LIQUIDACION(
    p_idePol          DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
    p_planilla         DEC_TRANSPORTE_FLOTANTE.PLANILLA%TYPE
  ) RETURN c_Cursor;
 -- <F N3040340> WRK-01213 / Christian R. / 17-01-2013
--<I N3040848> HD-117200 / Jhonatan Rojas T. / 02/04/2013
  FUNCTION OBTENER_DERECHO_EMISION(p_nIdedec  NUMBER ) RETURN NUMBER;
--<F N3040848> HD-117200 / Jhonatan Rojas T. / 02/04/2013
--<I N3041755> TRANS-00037 / Italo Rivas / 25-11-2013
  FUNCTION OBTENER_DIRECCION_RESPAGO(p_nInd   VARCHAR2,
                                     p_nNumLq NUMBER) RETURN VARCHAR2;
--<F N3041755> TRANS-00037 / Italo Rivas / 25-11-2013

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
                                  p_cTipo        IN VARCHAR2,
                                  p_cError       OUT VARCHAR2);
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
  PROCEDURE SP_ELIM_CLAU_CERT_C(
    p_nIdePol      IN Poliza.IdePol%TYPE,
    p_nNumCert     IN Certificado.NumCert%TYPE,
    p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
    p_cTipo        IN VARCHAR2,
    p_cError       OUT VARCHAR2);
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
PROCEDURE SP_INCLUIR_CERTI_CLAU_C( 
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cError       OUT VARCHAR2);
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
FUNCTION FN_MENSAJE_ACSEL(p_cTipo VARCHAR2, 
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
29/08/2023    Werner Tito   [PROYECTO TECH CORE] COR103-925 - Integración con Satelites InsPAT
----------------------------------------------------------------------------------------------*/
PROCEDURE SP_ACTIVA_GARANTIAS (p_stspol POLIZA.StsPol%TYPE, p_nIdePol IN Poliza.IdePol%TYPE);
--
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
    Fecha           Autor          Descripciión
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
/*-------------------------------------------------------------------------------
  Nombre      : FN_EXISTE_FACULTATIVO
  Proposito   : Valida si el Certificado es Facultativo y tiene Distribución
  Log de Cambios 
    Fecha	          Autor			     Descripciión
  31/10/2011    Noema Paredes   Creación <N3037805>
-------------------------------------------------------------------------------*/
FUNCTION FN_EXISTE_FACULTATIVO (p_nIdePol  CERTIFICADO.IdePol%TYPE, 
                                p_nNumCert CERTIFICADO.NumCert%TYPE) RETURN NUMBER;
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
  Nombre      : PR_API_EMI_TRAN.FN_OBT_CERT_RAMO_DIST_FACULT
  Proposito   : Obtener certificado y ramo en donde no se haya colocado detalle facultativo.
  Referencia : MODCOM.FMB
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
  Log de Cambios 
    Fecha             Autor          Descripción
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
 FUNCTION FN_OBTCONFREDIST ( p_nIdePol IN POLIZA.IdePol%TYPE ) RETURN VARCHAR2;
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
FUNCTION FN_EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE , p_cTipoOper IN LVAL.CODLVAL%TYPE) RETURN OPER_POL.NumOper%TYPE;
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
PROCEDURE SP_ACTUALIZAR_INDCOBESP(p_nNumOper IN OPER_POL.NumOper%TYPE);
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
PROCEDURE SP_ACTIVAR_ACTUALIZAR_POLIZA( p_nIdePol IN Poliza.IdePol%TYPE, p_cTipoOper IN LVAL.CODLVAL%TYPE);
--
/*------------------------------------------------------------------------
  Nombre     : SP_ACT_ENDOSO_VIG_OPE
  Propósito  : ACTIVAR MOTIVOS ENDOSOS VIGENCIA OPERACIO
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_cTipoOper    VARCHAR   Tipo motivo del endoso

  Referencia : MANTPOL.FMB

  Log de Cambios:
    Fecha         Autor               Descripción 
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACT_ENDOSO_VIG_OPE(
  p_nIdePol   IN Poliza.IdePol%TYPE,
  p_cTipoOper IN LVAL.CODLVAL%TYPE
);
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
  p_cError       OUT VARCHAR2);
 -- FIN CLAUSULA C

END PR_API_EMI_TRAN;
