CREATE OR REPLACE PACKAGE BODY ACSELX.PR_API_COMISION_INTERMEDIARIO IS
  /*-----------------------------------------------------------------------------------
    Nombre : BUSCAR_COMISIONES
    Prop�sito: Retorna una lista de Comisiones que cumplen con un criterio determinado.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         C�digo de la Compa��a
                P_cCODMONEDA IN VARCHAR2      C�digo de la moneda
                P_cTIPOCONSULTA IN VARCHAR2   Tipo de Consulta: Por cobrar(ACT),Cobrados(LIQ)
                P_cCODPROD IN VARCHAR2        C�digo del Producto
                P_cNUMPOL IN VARCHAR2         N�mero de la P�liza
                P_cFECINI IN VARCHAR2         Fecha inicial
                P_cFECFIN IN VARCHAR2         Fecha final
                P_nNUMIDCLIENTE IN NUMBER     Numid del Cliente
                P_nCANTIDAD IN NUMBER         Tipo de respuesta: Detalle(-1), Cantidad(0)
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaci�n
    01/10/2007       Guillermo Vergara      Modificaci�n (R1.1.1.1)  Caso de Prima Nula. <N3028549>
    27/09/2007       Marko Chu              Modificacion <N3028514> para separar busqeudas por
                                            Comisiones y asesor�as
    31/10/2007       Marko Chu              Modificacion <N3028741> para eliminar la columna
                                            FORMAPAGO en las funciones Buscar_Comisiones y
                                            Detalle_Preliquidaci�n.
    12/08/2010       Carlos Riguetti        Create Procedimientos y Funci�n Carlos Riguetti
                                            Requerimiento: DEFPROC-00171 Reserva: N3035540
    02/05/2011       Marlon Castro G.       Modificaci�n <N3037392> Mejoras en Preliquidaciones
                                            Referencia Reqm CDC-00076
    08/05/2012       Marcos Gomez Jimenez   Modificacion en la obtencion de documentos de descarga
              en funcion al requerimiento CDC-00089  Reserva: N3038970.
    23/10/2012       Christian R.           CDC-00091 <N3039796>
    20/03/2013       Italo Rivas            <N3040669> CDC-00096
    23/01/2014       Jose Angeles           RTC 18505 Se agrega parametro para el Canal
    23/03/2015       Eddy Cordova           RTC 30769 Se modifica la busqueda de la prima neta
    13/06/2016       Alister Arnedo         RTC 71480 Suspensi�n de pago a Corredores por Norma SUSALUD 134-2015-SUSALUD/S Web-REL
    10/05/2017       Jose Angeles           RTC 100411 Optimizaci�n del modulo de Pre liquidaci�n
    23/02/2018       Jose Angeles           RTC 125554 Se agrega validaci�n de la suspensi�n del RTC 71480
    15/09/2018       Alister Arnedo         RTC 150595  Se agrega los siguientes estados de extorno �EXMO� y �REMO�.
    -----------------------------------------------------------------------------------*/

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
    p_cASES IN VARCHAR2   DEFAULT 'N'-- <N3028514> Agregado por Jagner Hinostroza / CDC-00039
    ,p_nRegistros IN NUMBER   DEFAULT 100-- <N3039796> Agregado por Christian R. / CDC-00091
    ,p_nIndImp IN VARCHAR2   DEFAULT NULL-- <N3039796> Agregado por Christian R. / CDC-00091
    ,p_cCodCanal IN VARCHAR2   DEFAULT '0'-- RTC 18505 / Agregado por Jose Angeles / 23-01-2015
  ) RETURN TRANSFER_REF IS

  TRANSFER_CUR TRANSFER_REF;

  BEGIN
             
    IF P_cTIPOCONSULTA = 'ACT' AND P_nCANTIDAD = 0
       THEN  OPEN TRANSFER_CUR FOR 
         --<INI RTC 100411>  - Jose Angeles - 10/05/2017
         --SELECT /*+ ALL_ROWS */ COUNT(1) N-- RTC 30769 - Jose Angeles - 24/03/2015
         --<INI RTC 125554> - Jose Angeles - 23/02/2018 Se agrega validaci�n de la suspensi�n del RTC 71480
         --SELECT /*+ ALL_ROWS */ COUNT(1) N, SUM(MONTO) MONTO_TOTAL
         SELECT /*+ ALL_ROWS */ COUNT(1) N, 
                NVL(SUM(CASE WHEN X.CODPROD IS NULL THEN X.MONTO
                         WHEN X.CODPROD='6101' 
                              AND (PR_POLIZA.F_OBTIENE_STATUS_SUSALUD@DBL_BDAPPPROD(X.NUMIDCLIENTE,
                                                                                    (SELECT max(T.NUMPOL) 
                                                                                     FROM   TRANSACRE T 
                                                                                     WHERE  T.NUMACRE IN (SELECT A.NUMACRE 
                                                                                                          FROM   ACREENCIA A 
                                                                                                          WHERE  A.IDEFACT = X.IDEFACT)),
                                                                                    P_nNUMIDINTER,
                                                                                     1,
                                                                                     4,
                                                                                    'S')) != 'S' THEN 0 
                         ELSE X.MONTO
                     END),0) MONTO_TOTAL
         --<FIN RTC 125554>
         --<FIN RTC 100411>  
         FROM   (SELECT /*+ ALL_ROWS */
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) CODPROD,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMIDCLIENTE,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMPOL
                         --<INI RTC 100411>  - Jose Angeles - 10/05/2017
                         ,MONTO
                         --<FIN RTC 100411>
                         --<INI RTC 125554> - Jose Angeles - 26/02/2018
                         ,IDEFACT
                         --<FIN RTC 125554>
                 FROM   (SELECT /*+ INDEX(OI IX_OPER_INTER_01) */
                                OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA,
                                SUM(OI.MONTOOPERINTERFORANEA) MONTO
                                
                         FROM   OPER_INTER OI
                         WHERE  OI.CODINTER = (SELECT I.CODINTER
                                               FROM   INTERMEDIARIO I
                                               WHERE  I.NUMID = P_nNUMIDINTER)
                         AND    OI.CODCIA = P_cCODCIA
                         AND    OI.CODMONEDA = P_cCODMONEDA
                         --[INI] RTC 150595  - Alister Arnedo - 15/09/2018           
                         --AND    OI.TIPOOPERINTER IN('COMI','EXTR')
                         AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
                         --[FIN] RTC 150595  - Alister Arnedo - 15/09/2018 
                         AND    OI.STSOPERINTER = 'ACT'
                         AND    OI.FOPERINTER BETWEEN TO_DATE(P_CFECINI,'YYYY-MM-DD')
                                                  AND TO_DATE(P_CFECFIN,'YYYY-MM-DD')
                         -- <I: N3028514>  Marko Chu / 27-09-2007
                         AND    OI.INDASESOR = p_cASES
                         -- <F: N3028514>
                         -- <I N3033740> / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
                          AND OI.INDPRELIQ = 'N'
                          AND OI.NUMPRELIQ IS NULL
                          -- <F N3033740> / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
                          --<INI> RTC 18505 - Jose Angeles - 23/01/2014
                          AND (NVL(p_cCodCanal,'0') = '0' OR
                               EXISTS(SELECT IP.IDEPOL
                                      FROM INTERMEDIACION_POL IP, OPER_POL OP
                                      WHERE IP.Idepol= OP.Idepol
                                      AND OP.NUMOPER=BUSCAR_OPERACION(OI.IDEFACT,OI.OBLIG_PRI,OI.NUMOPER)
                                      AND IP.Codcanal = p_cCodCanal
                                      AND IP.Stsinter = 'ACT' ))            
                          --<FIN> RTC 18505 
                          
                          
                         GROUP BY OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                  OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA) OI2
                ) X
         --<INI> RTC 18505 - Jose Angeles - 06/11/2015       
         --WHERE  (P_cCODPROD IS NULL OR X.CODPROD = P_cCODPROD)
         WHERE  (P_cCODPROD IS NULL OR  (P_cCODPROD  like '%' || X.CODPROD || '%' AND X.CODPROD IS NOT NULL ))
         --<FIN> RTC 18505 
         AND    (P_cNUMPOL IS NULL OR X.NUMPOL LIKE P_cNUMPOL||'%')
         AND    (P_nNUMIDCLIENTE IS NULL OR X.NUMIDCLIENTE = P_nNUMIDCLIENTE);

    ELSIF P_cTIPOCONSULTA = 'ACT' AND P_nCANTIDAD = -1
       THEN  OPEN TRANSFER_CUR FOR
       SELECT * FROM ( -- <N3039796> Agregado por Christian R. / CDC-00091
         SELECT /*+ ALL_ROWS */
                X.CODPROD CODPROD,
                PR_EDOCTA_INTERMEDIARIO_R1420.DESCRIPCION_PRODUCTO(X.CODPROD,15) NOMPROD,
                X.NUMIDCLIENTE NUMIDCLIE,
                (SELECT CASE WHEN T.TIPOTER = 'P'
                                  THEN T.APETER||' '||T.APEMATTER|| ' '||T.NOMTER
                                  ELSE T.NOMTER
                        END
                 FROM   TERCERO T
                 WHERE  T.NUMID = X.NUMIDCLIENTE) NOMCLIE,
                X.NUMPOL NUMPOL,
                X.NUMDOCCOM NUMDOCCOM,
                X.NUMDOCSUNAT NUMDOCSUNAT,
                X.TIPOOPERINTER TIPOOPERINTER,
                TO_CHAR(X.FOPERINTER,'DD/MM/YYYY') FECCREA,
                TO_CHAR(X.FECHA_LIQUIDACION,'DD/MM/YYYY') FECLIQ,
                --<I N3028549> / Guillermo Vergara / 02-Octubre-2007
                CASE WHEN ABS(X.MONTO) > ABS(X.PRIMA) OR X.PRIMA = 0
                          THEN 0
                          ELSE ROUND((X.MONTO / X.PRIMA) * 100,2)
                END PORCOM,
                --<F N3028549>
                X.PRIMA PRIMA,
                --<INI> / Eddy Cordova E. / RTC-30769 / 23-03-2015
                (SELECT CASE WHEN (X.CODPROD IS NOT NULL AND P_cCODCIA='01')
                                          THEN 
                                            BUSCA_PRIMA_NETA_TOTAL(X.CODPROD, X.NUMPOL)
                               ELSE 0
                                END
                        FROM   DUAL)
                 PRIMATOTAL,
                --<FIN> / Eddy Cordova E. / RTC-30769 / 23-03-2015                 
                X.MONTO MONTO,
                X.NUMFACT NUMFACT,
                X.NUMLIQ NUMLIQ,
                X.NUMOPER NUMOPER,
                -- <I: N3028514>  Marko Chu / 27-09-2007
                TO_CHAR(X.FECPAGO,'DD/MM/YYYY') FECPAGO,
                -- <F: N3028514>
                ' ' FORMAPAGO,  -- <N3028741> / Marko Chu / 08-11-2007
                -- <I N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                X.NUMOPERINTER NUMOPERINTER,
                X.NUMOPEROPER NUMOPEROPER,
                X.IDEFACT,
                'N' INDDETALLE,
                P_cCODCIA CODCIA,
                P_cCODMONEDA CODMONEDA,
                P_cTIPOCONSULTA STSOPERINTER,
                P_nNUMIDINTER CODINTER,
                p_cASES INDASESOR
                -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                                -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                --<INI> RTC 71480 - Alister Arnedo - 13/06/2016
                ,(SELECT CASE WHEN X.CODPROD IS NOT NULL AND X.CODPROD='6101' THEN 
                (SELECT CASE WHEN (PR_POLIZA.F_OBTIENE_STATUS_SUSALUD@DBL_BDAPPPROD(X.NUMIDCLIENTE,
                                                              (SELECT max(T.NUMPOL) FROM   TRANSACRE T 
                                                              WHERE  T.NUMACRE IN (SELECT A.NUMACRE 
                                                              FROM   ACREENCIA A 
                                                              WHERE  A.IDEFACT = X.IDEFACT)),
                                                              P_nNUMIDINTER,
                                                               1,
                                                               4,
                                                              'S')) = 'S' THEN 'N' ELSE 'S' END FROM DUAL)
                 ELSE 'N' END FROM DUAL)                                                           
                ESTATUS --Estado de suspencion de pago S = bloqueado , N = desbloqueado
                --<FIN> RTC 71480 - Alister Arnedo - 13/06/2016

         FROM   (SELECT /*+ ALL_ROWS */
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) CODPROD,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMIDCLIENTE,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMPOL,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOC||'-'||F.NUMFACT
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOC||'-'||O.NUMOBLIG
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM DUAL) NUMDOCCOM,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOCPAG||'-'||F.SERIE||'-'||F.NUMDOC
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOCPAG||'-'||O.SERIE||'-'||O.NUMDOC
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMDOCSUNAT,
                        OI2.TIPOOPERINTER TIPOOPERINTER,
                        OI2.FOPERINTER FOPERINTER,
                        OI2.FECHA_LIQUIDACION FECHA_LIQUIDACION,
                        (SELECT SUM(DA.MTODETACREMONEDA)
                         FROM   DET_ACRE DA
                         WHERE  DA.NUMACRE IN(SELECT A.NUMACRE
                                              FROM   ACREENCIA_TABLE A
                                              WHERE  A.IDEFACT = OI2.IDEFACT)
                         AND    DA.CODGRUPOACRE = 'PRIMAS') PRIMA,
                        OI2.MONTO MONTO,
                        (SELECT FE.TIPOFACTEXT||'-'||FE.SERIEFACTEXT||'-'||FE.NUMFACTEXT
                         FROM   FACTURA_EXT FE
                         WHERE  FE.NUMOBLIG = OI2.NUMOBLIG
                         AND    ROWNUM = 1) NUMFACT,
                        OI2.NUMLIQ NUMLIQ,
                        OI2.NUMOPERCAJA NUMOPER,
                        -- <I: N3028514>  Marko Chu / 27-09-2007
                        (
                          SELECT RI.FECSTSRELING
                          FROM REL_ING RI
                          WHERE RI.NUMRELING = (
                                               SELECT F.NUMRELING
                                               FROM   FACTURA F
                                               WHERE F.IDEFACT = OI2.IDEFACT
                                             )
                        ) FECPAGO,
                        -- <F: N3028514>
                        -- <I N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                        OI2.NUMOPERINTER,
                        OI2.NUMOPEROPER,
                        OI2.IDEFACT
                        -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                 FROM   (SELECT /*+ INDEX(OI IX_OPER_INTER_01) */
                                OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA,
                                SUM(OI.MONTOOPERINTERFORANEA) MONTO,
                                -- <I N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                                TO_CHAR(MAX(OI.NUMOPERINTER)) NUMOPERINTER,
                                MAX(OI.NUMOPER) NUMOPEROPER
                                -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                         FROM   OPER_INTER OI
                         WHERE  OI.CODINTER = (SELECT I.CODINTER
                                               FROM   INTERMEDIARIO I
                                               WHERE  I.NUMID =  P_nNUMIDINTER)
                         AND    OI.CODCIA = P_cCODCIA
                         AND    OI.CODMONEDA = P_cCODMONEDA
                         --[INI] RTC 150595  - Alister Arnedo - 15/09/2018           
                         --AND    OI.TIPOOPERINTER IN('COMI','EXTR')
                         AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
                         --[FIN] RTC 150595  - Alister Arnedo - 15/09/2018
                         AND    OI.STSOPERINTER = 'ACT'
                         AND    OI.FOPERINTER BETWEEN TO_DATE(P_CFECINI,'YYYY-MM-DD')
                                                  AND TO_DATE(P_CFECFIN,'YYYY-MM-DD')
                         -- <I: N3028514> Marko Chu / 27-09-2007 / CDC - 00039
                         --Se agrega condicion para tipo de busqueda
                         AND    OI.INDASESOR = p_cASES
                         -- <F: N3028514>
                         -- <I N3033740> / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
                         AND OI.INDPRELIQ = 'N'
                         AND OI.NUMPRELIQ IS NULL
                         -- <F N3033740> / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
                         --<INI> RTC 18505 - Jose Angeles - 23/01/2014
                         AND (NVL(p_cCodCanal,'0') = '0' OR
                              EXISTS(SELECT IP.IDEPOL
                                     FROM INTERMEDIACION_POL IP, OPER_POL OP
                                     WHERE IP.Idepol= OP.Idepol
                                     AND OP.NUMOPER=BUSCAR_OPERACION(OI.IDEFACT,OI.OBLIG_PRI,OI.NUMOPER)
                                     AND IP.Codcanal = p_cCodCanal
                                     AND IP.Stsinter = 'ACT' ))            
                         --<FIN> RTC 18505 
                         GROUP BY OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                  OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA) OI2
                ) X
         --<INI> RTC 18505 - Jose Angeles - 06/11/2015       
         --WHERE  (P_cCODPROD IS NULL OR X.CODPROD = P_cCODPROD)
         WHERE  (P_cCODPROD IS NULL OR  (P_cCODPROD  like '%' || X.CODPROD || '%' AND X.CODPROD IS NOT NULL ))
         --<FIN> RTC 18505
         AND    (P_cNUMPOL IS NULL OR X.NUMPOL LIKE P_cNUMPOL||'%')
         AND    (P_nNUMIDCLIENTE IS NULL OR X.NUMIDCLIENTE = P_nNUMIDCLIENTE)
         --ORDER BY NOMCLIE,NOMPROD,NUMPOL,FECCREA,NUMLIQ,TIPOOPERINTER-- <N3039796> Modificado por Christian R. / CDC-00091 -- RTC 30769 - Jose Angeles - 24/03/2015
         ) WHERE (p_nIndImp IS NULL OR p_nIndImp='R' OR  ROWNUM<=100);-- <N3039796> Agregado por Christian R. / CDC-00091

    ELSIF P_cTIPOCONSULTA = 'LIQ' AND P_nCANTIDAD = 0
       THEN  OPEN TRANSFER_CUR FOR
         --<INI RTC 100411>  - Jose Angeles - 10/05/2017
         --SELECT /*+ ALL_ROWS */ COUNT(1) N-- RTC 30769 - Jose Angeles - 24/03/2015
         SELECT /*+ ALL_ROWS */ COUNT(1) N, SUM(MONTO) MONTO_TOTAL
         --<FIN RTC 100411>
         FROM   (SELECT /*+ ALL_ROWS */
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) CODPROD,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMIDCLIENTE,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMPOL
                         --<INI RTC 100411>  - Jose Angeles - 10/05/2017
                         ,MONTO
                         --<FIN RTC 100411>
                 FROM   (SELECT /*+ INDEX(OI IX_OPER_INTER_02) */
                                OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA,
                                SUM(OI.MONTOOPERINTERFORANEA) MONTO
                         FROM   OPER_INTER OI
                         WHERE  OI.CODINTER = (SELECT I.CODINTER
                                               FROM   INTERMEDIARIO I
                                               WHERE  I.NUMID = P_nNUMIDINTER)
                         AND    OI.CODCIA = P_cCODCIA
                         AND    OI.CODMONEDA = P_cCODMONEDA
                         --[INI] RTC 150595  - Alister Arnedo - 15/09/2018           
                         --AND    OI.TIPOOPERINTER IN('COMI','EXTR')
                         AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
                         --[FIN] RTC 150595  - Alister Arnedo - 15/09/2018
                         AND    OI.STSOPERINTER = 'LIQ'
                         AND    OI.FECHA_LIQUIDACION BETWEEN TO_DATE(P_CFECINI,'YYYY-MM-DD')
                                                         AND TO_DATE(P_CFECFIN,'YYYY-MM-DD')
                         -- <I: N3028514>  Marko Chu / 27-09-2007
                         AND    OI.INDASESOR = p_cASES
                         -- <F: N3028514>
                         --<INI> RTC 18505 - Jose Angeles - 23/01/2014
                         AND (NVL(p_cCodCanal,'0') = '0' OR
                              EXISTS(SELECT IP.IDEPOL
                                     FROM INTERMEDIACION_POL IP, OPER_POL OP
                                     WHERE IP.Idepol= OP.Idepol
                                     AND OP.NUMOPER=BUSCAR_OPERACION(OI.IDEFACT,OI.OBLIG_PRI,OI.NUMOPER)
                                     AND IP.Codcanal = p_cCodCanal
                                     AND IP.Stsinter = 'ACT' ))  
                         --<FIN> RTC 18505 
                         GROUP BY OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                  OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA) OI2
                ) X
         --<INI> RTC 18505 - Jose Angeles - 06/11/2015       
         --WHERE  (P_cCODPROD IS NULL OR X.CODPROD = P_cCODPROD)
         WHERE  (P_cCODPROD IS NULL OR  (P_cCODPROD  like '%' || X.CODPROD || '%' AND X.CODPROD IS NOT NULL ))
         --<FIN> RTC 18505
         AND    (P_cNUMPOL IS NULL OR X.NUMPOL LIKE P_cNUMPOL||'%')
         AND    (P_nNUMIDCLIENTE IS NULL OR X.NUMIDCLIENTE = P_nNUMIDCLIENTE);

    ELSIF P_cTIPOCONSULTA = 'LIQ' AND P_nCANTIDAD = -1
       THEN  OPEN TRANSFER_CUR FOR
       SELECT * FROM ( -- <N3039796> Agregado por Christian R. / CDC-00091
         SELECT /*+ ALL_ROWS */
                X.CODPROD CODPROD,
                PR_EDOCTA_INTERMEDIARIO_R1420.DESCRIPCION_PRODUCTO(X.CODPROD,15) NOMPROD,
                X.NUMIDCLIENTE NUMIDCLIE,
                (SELECT CASE WHEN T.TIPOTER = 'P'
                                  THEN T.APETER||' '||T.APEMATTER|| ' '||T.NOMTER
                                  ELSE T.NOMTER
                        END
                 FROM   TERCERO T
                 WHERE  T.NUMID = X.NUMIDCLIENTE) NOMCLIE,
                X.NUMPOL NUMPOL,
                X.NUMDOCCOM NUMDOCCOM,
                X.NUMDOCSUNAT NUMDOCSUNAT,
                X.TIPOOPERINTER TIPOOPERINTER,
                TO_CHAR(X.FOPERINTER,'DD/MM/YYYY') FECCREA,
                TO_CHAR(X.FECHA_LIQUIDACION,'DD/MM/YYYY') FECLIQ,
                -- <I N3028549> / Guillermo Vergara /  02-Octubre-2007
                CASE WHEN ABS(X.MONTO) > ABS(X.PRIMA) OR X.PRIMA = 0
                          THEN 0
                          ELSE ROUND((X.MONTO / X.PRIMA) * 100,2)
                END PORCOM,
                -- <F N3028549>
                X.PRIMA PRIMA,
                --<INI> / Eddy Cordova E. / RTC-30769 / 10-03-2015                
                (SELECT CASE WHEN (X.CODPROD IS NOT NULL AND P_cCODCIA='01')
                                          THEN 
                                            BUSCA_PRIMA_NETA_TOTAL(X.CODPROD, X.NUMPOL)
                               ELSE 0
                                END
                        FROM   DUAL)
                 PRIMATOTAL,
                --<FIN> / Eddy Cordova E. / RTC-30769 / 10-03-2015                 
                X.MONTO MONTO,
                X.NUMFACT NUMFACT,
                X.NUMLIQ NUMLIQ,
                X.NUMOPER NUMOPER,
                -- <I: N3028514>  Marko Chu / 27-09-2007
                TO_CHAR(X.FECPAGO,'DD/MM/YYYY') FECPAGO,
                -- <F: N3028514>
                ' ' FORMAPAGO, -- <N3028741> / Marko Chu / 08-11-2007
                -- <I N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                X.NUMOPERINTER NUMOPERINTER,
                X.NUMOPEROPER NUMOPEROPER,
                X.IDEFACT,
                'N' INDDETALLE,
                P_cCODCIA CODCIA,
                P_cCODMONEDA CODMONEDA,
                P_cTIPOCONSULTA STSOPERINTER,
                P_nNUMIDINTER CODINTER,
                p_cASES INDASESOR
                --<INI> RTC 71480 - Alister Arnedo - 13/06/2016
                ,(SELECT CASE WHEN X.CODPROD IS NOT NULL AND X.CODPROD='6101' THEN 
                (SELECT CASE WHEN (PR_POLIZA.F_OBTIENE_STATUS_SUSALUD@DBL_BDAPPPROD(X.NUMIDCLIENTE,
                                                              (SELECT max(T.NUMPOL) FROM   TRANSACRE T 
                                                              WHERE  T.NUMACRE IN (SELECT A.NUMACRE 
                                                              FROM   ACREENCIA A 
                                                              WHERE  A.IDEFACT = X.IDEFACT)),
                                                              P_nNUMIDINTER,
                                                               1,
                                                               4,
                                                              'S')) = 'S' THEN 'N' ELSE 'S' END FROM DUAL)
                 ELSE 'N' END FROM DUAL)                                                           
                ESTATUS --Estado de suspencion de pago S = bloqueado , N = desbloqueado
                --<FIN> RTC 71480 - Alister Arnedo - 13/06/2016
                -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
         FROM   (SELECT /*+ ALL_ROWS */
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) CODPROD,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMIDCLIENTE,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMPOL,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOC||'-'||F.NUMFACT
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOC||'-'||O.NUMOBLIG
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM DUAL) NUMDOCCOM,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOCPAG||'-'||F.SERIE||'-'||F.NUMDOC
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOCPAG||'-'||O.SERIE||'-'||O.NUMDOC
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMDOCSUNAT,
                        OI2.TIPOOPERINTER TIPOOPERINTER,
                        OI2.FOPERINTER FOPERINTER,
                        OI2.FECHA_LIQUIDACION FECHA_LIQUIDACION,
                        (SELECT SUM(DA.MTODETACREMONEDA)
                         FROM   DET_ACRE DA
                         WHERE  DA.NUMACRE IN(SELECT A.NUMACRE
                                              FROM   ACREENCIA_TABLE A
                                              WHERE  A.IDEFACT = OI2.IDEFACT)
                         AND    DA.CODGRUPOACRE = 'PRIMAS') PRIMA,
                        OI2.MONTO MONTO,
                        (SELECT FE.TIPOFACTEXT||'-'||FE.SERIEFACTEXT||'-'||FE.NUMFACTEXT
                         FROM   FACTURA_EXT FE
                         WHERE  FE.NUMOBLIG = OI2.NUMOBLIG
                         AND    ROWNUM = 1) NUMFACT,
                        OI2.NUMLIQ NUMLIQ,
                        OI2.NUMOPERCAJA NUMOPER,
                        -- <I: N3028514>  Marko Chu / 27-09-2007
                        (
                          SELECT RI.FECSTSRELING
                          FROM REL_ING RI
                          WHERE RI.NUMRELING = (
                                               SELECT F.NUMRELING
                                               FROM   FACTURA F
                                               WHERE F.IDEFACT = OI2.IDEFACT
                                             )
                        ) FECPAGO,
                        -- <F: N3028514>
                        -- <I N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                        OI2.NUMOPERINTER,
                        OI2.NUMOPEROPER,
                        OI2.IDEFACT
                        -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                 FROM   (SELECT /*+ INDEX(OI IX_OPER_INTER_02) */
                                OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA,
                                SUM(OI.MONTOOPERINTERFORANEA) MONTO,
                                -- <I N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                                TO_CHAR(MAX(OI.NUMOPERINTER)) NUMOPERINTER,
                                MAX(OI.NUMOPER) NUMOPEROPER
                                -- <F N3037392> Marlon Castro G. - 02/05/2011 - Mejoras en Preliquidaciones
                         FROM   OPER_INTER OI
                         WHERE  OI.CODINTER = (SELECT I.CODINTER
                                               FROM   INTERMEDIARIO I
                                               WHERE  I.NUMID = P_nNUMIDINTER)
                         AND    OI.CODCIA = P_cCODCIA
                         AND    OI.CODMONEDA = P_cCODMONEDA
                         --[INI] RTC 150595  - Alister Arnedo - 15/09/2018           
                         --AND    OI.TIPOOPERINTER IN('COMI','EXTR')
                         AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
                         --[FIN] RTC 150595  - Alister Arnedo - 15/09/2018
                         AND    OI.STSOPERINTER = 'LIQ'
                         AND    OI.FECHA_LIQUIDACION BETWEEN TO_DATE(P_CFECINI,'YYYY-MM-DD')
                                                         AND TO_DATE(P_CFECFIN,'YYYY-MM-DD')
                         -- <I: N3028514>  Marko Chu / 27-09-2007
                         AND    OI.INDASESOR = p_cASES
                         -- <F: N3028514>
                         --<INI> RTC 18505 - Jose Angeles - 23/01/2014
                         AND (NVL(p_cCodCanal,'0') = '0' OR
                              EXISTS(SELECT IP.IDEPOL
                                     FROM INTERMEDIACION_POL IP, OPER_POL OP
                                     WHERE IP.Idepol= OP.Idepol
                                     AND OP.NUMOPER=BUSCAR_OPERACION(OI.IDEFACT,OI.OBLIG_PRI,OI.NUMOPER)
                                     AND IP.Codcanal = p_cCodCanal
                                     AND IP.Stsinter = 'ACT' ))    
                         --<FIN> RTC 18505
                        GROUP BY OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                  OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA) OI2
                ) X
         --<INI> RTC 18505 - Jose Angeles - 06/11/2015       
         --WHERE  (P_cCODPROD IS NULL OR X.CODPROD = P_cCODPROD)
         WHERE  (P_cCODPROD IS NULL OR  (P_cCODPROD  like '%' || X.CODPROD || '%' AND X.CODPROD IS NOT NULL ))
         --<FIN> RTC 18505
         AND    (P_cNUMPOL IS NULL OR X.NUMPOL LIKE P_cNUMPOL||'%')
         AND    (P_nNUMIDCLIENTE IS NULL OR X.NUMIDCLIENTE = P_nNUMIDCLIENTE)
         --ORDER BY NOMCLIE,NOMPROD,NUMPOL,FECLIQ,NUMLIQ,TIPOOPERINTER-- <N3039796> Modificado por Christian R. / CDC-00091 -- RTC 30769 - Jose Angeles - 24/03/2015
         ) WHERE (p_nIndImp IS NULL OR p_nIndImp='R' OR  ROWNUM<=100);-- <N3039796> Agregado por Christian R. / CDC-00091

    END IF;

    --<INI RTC 100411>  - Jose Angeles - 10/05/2017
    COMMIT;
    --<FIN RTC 100411>
    RETURN(TRANSFER_CUR);
  END BUSCAR_COMISIONES;

  /*-----------------------------------------------------------------------------------
    Nombre : NUM_PRELIQUIDACION_PENDIENTE
    Prop�sito: Retorna la cantidad de Pre-liquidaciones pendientes de un Corredor.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         C�digo de la Compa��a
                P_cCODMONEDA IN VARCHAR2      C�digo de la moneda
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaci�n
    27/09/2007       Jagner Hinostroza      Modificacion  <N3028514> Modificaion para separar
                                            Comisiones y asesorias
    -----------------------------------------------------------------------------------*/

  FUNCTION NUM_PRELIQUIDACION_PENDIENTE(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    p_cASES IN VARCHAR2 -- <N3028514> Agregado por Jagner Hinostroza / CDC-00039
  ) RETURN NUMBER IS

  W_CANTPRELIQ NUMBER;

  BEGIN
    SELECT /*+ INDEX(OI IX_OPER_INTER_CODINTER_INDP) */
           COUNT(DISTINCT OI.NUMPRELIQ)
    INTO   W_CANTPRELIQ
    FROM   OPER_INTER OI
    WHERE  OI.CODINTER = (SELECT I.CODINTER
                          FROM   INTERMEDIARIO I
                          WHERE  I.NUMID = P_nNUMIDINTER)
    AND    OI.INDPRELIQ = 'S'
    AND    OI.CODCIA = P_cCODCIA
    AND    OI.CODMONEDA = P_cCODMONEDA
    -- <I: N3028514>  Jagner Hinostroza/ 27-09-2007
    AND    OI.INDASESOR = p_cASES
    -- <F: N3028514>
    AND    OI.NUMLIQ IS NULL;

    RETURN(W_CANTPRELIQ);

  END NUM_PRELIQUIDACION_PENDIENTE;

  /*-----------------------------------------------------------------------------------
    Nombre : CREAR_PRELIQUIDACION
    Prop�sito: Crea una Pre-liquidaci�n y retorna su c�digo.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         C�digo de la Compa��a
                P_cCODMONEDA IN VARCHAR2      C�digo de la moneda
                P_cFECINI IN NUMBER           Fecha inicial
                P_cFECFIN IN VARCHAR2         Fecha final
                P_nNUMPRELIQ OUT NUMBER       C�digo de la Pre-liquidaci�n
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaci�n
    27/09/2007       Jagner Hinostroza      Modificacion  <N3028514> Modificaion para separar
                                            Comisiones y asesorias
    10/11/2009      Marko Chu               <N3033740> Ya no se anulan las preliquidaciones existentes
    12/05/2017       Jose Angeles           RTC 100411 Optimizaci�n del modulo de Pre liquidaci�n  
    -----------------------------------------------------------------------------------*/

  PROCEDURE CREA_PRELIQUIDACION(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    P_cFECINI IN VARCHAR2,
    P_cFECFIN IN VARCHAR2,
    P_nNUMPRELIQ OUT NUMBER,
    p_cASES IN VARCHAR2 -- <N3028514> Agregado por Jagner Hinostroza / CDC-00039
    --<INI RTC 100411>  - Jose Angeles - 12/05/2017
    ,P_cCODPROD IN VARCHAR2 DEFAULT null,
    P_cNUMPOL IN VARCHAR2 DEFAULT null,
    P_nNUMIDCLIENTE IN NUMBER DEFAULT null,
    p_cCodCanal IN VARCHAR2   DEFAULT '0',
     p_nCantidadMax IN NUMBER DEFAULT NULL
    --<FIN RTC 100411>
  ) IS

  W_NUMPRELIQ NUMBER;
  BEGIN W_NUMPRELIQ := 0;
    BEGIN

      -- <I > / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
      -- Ya no se anulan las preliquidaciones existentes

      -- UPDATE /*+ INDEX(OI IX_OPER_INTER_CODINTER_INDP) */
      --       OPER_INTER OI
      -- SET    OI.INDPRELIQ = 'N',
      --        OI.NUMPRELIQ = NULL
      -- WHERE  OI.CODINTER = (SELECT I.CODINTER
      --                       FROM   INTERMEDIARIO I
      --                       WHERE  I.NUMID = P_nNUMIDINTER)
      -- AND    OI.INDPRELIQ = 'S'
      -- AND    OI.CODMONEDA = P_cCODMONEDA
      -- AND    OI.NUMLIQ IS NULL
      -- <I: N3028514>  Jagner Hinostroza/ 27-09-2007
      -- AND    OI.INDASESOR = p_cASES ;
      -- <F: N3028514>

      -- <F > / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
      
      
      SELECT SQ_NUMPRELIQ.NEXTVAL
      INTO   W_NUMPRELIQ
      FROM   DUAL;

      --<INI RTC 100411>  - Jose Angeles - 12/05/2017
      IF P_cCODPROD is null and P_cNUMPOL is null  and P_nNUMIDCLIENTE is null  and (p_cCodCanal is null or p_cCodCanal='0') THEN
      --<FIN RTC 100411>
        UPDATE /*+ INDEX(OI IX_OPER_INTER_01) */
               OPER_INTER OI
        SET    OI.INDPRELIQ = 'S',
               OI.NUMPRELIQ = W_NUMPRELIQ
        WHERE  OI.CODINTER = (SELECT I.CODINTER
                              FROM   INTERMEDIARIO I
                              WHERE  I.NUMID = P_nNUMIDINTER)
        AND    OI.CODCIA = P_cCODCIA
        AND    OI.CODMONEDA = P_cCODMONEDA
        --<Inicio - Jhon Delgado Rivera - 13/10/2022 - RTC 315250 CORRECTIVO Pantalla de liquidaciones de comisiones>
        --AND    OI.TIPOOPERINTER IN('COMI','EXTR')
        AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
        --<Fin - RTC 315250 >
        AND    OI.STSOPERINTER = 'ACT'
        AND    OI.FOPERINTER BETWEEN TO_DATE(P_cFECINI,'YYYY-MM-DD')
                                 AND TO_DATE(P_cFECFIN,'YYYY-MM-DD')
        -- <I: N3028514>  Jagner Hinostroza / 27-09-2007
        AND    OI.INDASESOR = p_cASES
        -- <F: N3028514>
        -- <I N3033740> / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
        AND OI.INDPRELIQ = 'N'
        AND OI.NUMPRELIQ IS NULL
        -- <F N3033740> / Fin de Modificaciones / Marko Chu / 10-11-2009 / CDC-00054
        --<INI RTC 100411>  - Jose Angeles - 30/05/2017
        AND (ROWNUM <= p_nCantidadMax OR p_nCantidadMax is null);
        --<FIN RTC 100411>
      --<INI RTC 100411>  - Jose Angeles - 12/05/2017
      ELSE
        UPDATE /*+ INDEX(OI IX_OPER_INTER_01) */
               OPER_INTER OI
        SET    OI.INDPRELIQ = 'S',
               OI.NUMPRELIQ = W_NUMPRELIQ
        WHERE  OI.CODINTER = (SELECT I.CODINTER
                              FROM   INTERMEDIARIO I
                              WHERE  I.NUMID = P_nNUMIDINTER)
        AND    OI.CODCIA = P_cCODCIA
        AND    OI.CODMONEDA = P_cCODMONEDA
        --<Inicio - Jhon Delgado Rivera - 13/10/2022 - RTC 315250 CORRECTIVO Pantalla de liquidaciones de comisiones>
        --AND    OI.TIPOOPERINTER IN('COMI','EXTR')
        AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
        --<Fin - RTC 315250 >
        AND    OI.STSOPERINTER = 'ACT'
        AND    OI.FOPERINTER BETWEEN TO_DATE(P_cFECINI,'YYYY-MM-DD')
                                 AND TO_DATE(P_cFECFIN,'YYYY-MM-DD')
        AND    OI.INDASESOR = p_cASES
        AND OI.INDPRELIQ = 'N'
        AND OI.NUMPRELIQ IS NULL
        AND (P_cCODPROD is null or P_cCODPROD like '%' || CASE WHEN OI.IDEFACT IS NOT NULL
                                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI.IDEFACT)
                                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI.OBLIG_PRI)
                                                          END || '%' )
        AND (P_cNUMPOL is null or CASE WHEN OI.IDEFACT IS NOT NULL
                                  THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI.IDEFACT)
                                  ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI.OBLIG_PRI)
                                  END  like P_cNUMPOL || '%' )
        AND (P_nNUMIDCLIENTE IS NULL OR CASE WHEN OI.IDEFACT IS NOT NULL
                                        THEN (SELECT F.NUMID
                                              FROM   FACTURA F
                                              WHERE  F.IDEFACT = OI.IDEFACT)
                                        ELSE (SELECT O.NUMID
                                              FROM   OBLIGACION_TABLE O
                                              WHERE  O.NUMOBLIG = OI.OBLIG_PRI)
                                        END = P_nNUMIDCLIENTE)
        AND (NVL(p_cCodCanal,'0') = '0' OR EXISTS(SELECT IP.IDEPOL
                                                  FROM INTERMEDIACION_POL IP, OPER_POL OP
                                                  WHERE IP.Idepol= OP.Idepol
                                                  AND OP.NUMOPER=BUSCAR_OPERACION(OI.IDEFACT,OI.OBLIG_PRI,OI.NUMOPER)
                                                  AND IP.Codcanal = p_cCodCanal
                                                  AND IP.Stsinter = 'ACT' ))                                                
        AND (ROWNUM <= p_nCantidadMax OR p_nCantidadMax is null);                                                                                                                                                                        
      END IF; 
      --<FIN RTC 100411>  
     
    EXCEPTION
      WHEN OTHERS THEN
             ROLLBACK;
             W_NUMPRELIQ := 0;
    END;

    P_nNUMPRELIQ := W_NUMPRELIQ;
  END CREA_PRELIQUIDACION;

  /*-----------------------------------------------------------------------------------
    Nombre : DETALLE_PRELIQUIDACION
    Prop�sito: Dado un c�digo de pre-liquidaci�n, presenta el detalle correspondiente.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMPRELIQ IN NUMBER       C�digo de la Pre-liquidaci�n
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaci�n
    27/09/2007       Jagner Hinostroza      Modificacion  <N3028514> Modificaion para separar
                                            Comisiones y asesorias
    02/05/2011       Marlon Castro G.       Modificaci�n <N3037392> Mejoras en Preliquidaciones
                                            Referencia Reqm CDC-00076
    -----------------------------------------------------------------------------------*/

  FUNCTION DETALLE_PRELIQUIDACION(
    P_nNUMPRELIQ IN NUMBER
  ) RETURN TRANSFER_REF IS

  TRANSFER_CUR TRANSFER_REF;
  BEGIN
    OPEN TRANSFER_CUR FOR
         SELECT /*+ ALL_ROWS */
                X.CODPROD CODPROD,
                PR_EDOCTA_INTERMEDIARIO_R1420.DESCRIPCION_PRODUCTO(X.CODPROD,15) NOMPROD,
                X.NUMIDCLIENTE NUMIDCLIE,
                (SELECT CASE WHEN T.TIPOTER = 'P'
                                  THEN T.APETER||' '||T.APEMATTER|| ' '||T.NOMTER
                                  ELSE T.NOMTER
                        END
                 FROM   TERCERO T
                 WHERE  T.NUMID = X.NUMIDCLIENTE) NOMCLIE,
                X.NUMPOL NUMPOL,
                X.NUMDOCCOM NUMDOCCOM,
                X.NUMDOCSUNAT NUMDOCSUNAT,
                X.TIPOOPERINTER TIPOOPERINTER,
                TO_CHAR(X.FOPERINTER,'DD/MM/YYYY') FECCREA,
                TO_CHAR(X.FECHA_LIQUIDACION,'DD/MM/YYYY') FECLIQ,
                -- <I N3028549> / Guillermo Vergar / 02-Octuber-2007
                CASE WHEN ABS(X.MONTO) > ABS(X.PRIMA) OR X.PRIMA = 0
                          THEN 0
                          ELSE ROUND((X.MONTO / X.PRIMA) * 100,2)
                END PORCOM,
                -- <F N3028549>
                X.PRIMA PRIMA,
                X.MONTO MONTO,
                X.NUMFACT NUMFACT,
                X.NUMLIQ NUMLIQ,
                X.NUMOPER NUMOPER,
                TO_CHAR(X.FECPAGO,'DD/MM/YYYY') FECPAGO,
                ' ' FORMAPAGO, -- <N3028741> / Marko Chu / 08-11-2007
                'S' INDDETALLE -- <N3037392> Marlon Castro G. - 06/05/2011 - Mejoras en Preliquidaciones
         FROM   (SELECT /*+ ALL_ROWS */
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) CODPROD,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMIDCLIENTE,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMPOL,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOC||'-'||F.NUMFACT
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOC||'-'||O.NUMOBLIG
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM DUAL) NUMDOCCOM,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOCPAG||'-'||F.SERIE||'-'||F.NUMDOC
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOCPAG||'-'||O.SERIE||'-'||O.NUMDOC
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMDOCSUNAT,
                        OI2.TIPOOPERINTER TIPOOPERINTER,
                        OI2.FOPERINTER FOPERINTER,
                        OI2.FECHA_LIQUIDACION FECHA_LIQUIDACION,
                        (SELECT SUM(DA.MTODETACREMONEDA)
                         FROM   DET_ACRE DA
                         WHERE  DA.NUMACRE IN(SELECT A.NUMACRE
                                              FROM   ACREENCIA_TABLE A
                                              WHERE  A.IDEFACT = OI2.IDEFACT)
                         AND    DA.CODGRUPOACRE = 'PRIMAS') PRIMA,
                        OI2.MONTO MONTO,
                        (SELECT FE.TIPOFACTEXT||'-'||FE.SERIEFACTEXT||'-'||FE.NUMFACTEXT
                         FROM   FACTURA_EXT FE
                         WHERE  FE.NUMOBLIG = OI2.NUMOBLIG
                         AND    ROWNUM = 1) NUMFACT,
                        OI2.NUMLIQ NUMLIQ,
                        OI2.NUMOPERCAJA NUMOPER,
                        -- <I: N3028514>  Jagner Hinostroza / 27-09-2007
                        (
                          SELECT RI.FECSTSRELING
                          FROM REL_ING RI
                          WHERE RI.NUMRELING = (
                                               SELECT F.NUMRELING
                                               FROM   FACTURA F
                                               WHERE F.IDEFACT = OI2.IDEFACT
                                             )
                        ) FECPAGO
                        -- <F: N3028514>
                 FROM   (SELECT /*+ INDEX(OI IDX_NUMPRELIQ) */
                                OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA,
                                SUM(OI.MONTOOPERINTERFORANEA) MONTO
                         FROM   OPER_INTER OI
                         WHERE  OI.NUMPRELIQ = P_nNUMPRELIQ
                         GROUP BY OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                  OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA) OI2
                ) X
         ORDER BY NOMCLIE,NOMPROD,NUMPOL,FECCREA,NUMLIQ,TIPOOPERINTER;

    RETURN(TRANSFER_CUR);
  END DETALLE_PRELIQUIDACION;

/*-----------------------------------------------------------------------------------
    Nombre : CREAR_PRELIQUIDACION
    Prop�sito: Crea una Pre-liquidaci�n y retorna su c�digo.
    Referencia : Proyecto Portal
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         C�digo de la Compa��a
                P_cCODMONEDA IN VARCHAR2      C�digo de la moneda
                P_cFECINI IN NUMBER           Fecha inicial
                P_cFECFIN IN VARCHAR2         Fecha final
                p_cASES IN VARCHAR2           Identificador de Comisiones (N) y asesor�as (S)
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    21/12/2007       Jose Porlles C.        Creaci�n
    -----------------------------------------------------------------------------------*/

  FUNCTION CREA_PRELIQUIDACION(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA IN VARCHAR2,
    P_cCODMONEDA IN VARCHAR2,
    P_cFECINI IN VARCHAR2,
    P_cFECFIN IN VARCHAR2,
    p_cASES IN VARCHAR2
  ) RETURN TRANSFER_REF IS

   nNUMPRELIQ NUMBER;
   TRANSFER_CUR TRANSFER_REF;
   PRAGMA AUTONOMOUS_TRANSACTION;

   BEGIN

     CREA_PRELIQUIDACION(P_nNUMIDINTER,P_cCODCIA,P_cCODMONEDA,P_cFECINI,P_cFECFIN,nNUMPRELIQ,p_cASES);

    OPEN TRANSFER_CUR FOR
      SELECT nNUMPRELIQ FROM DUAL;

    RETURN(TRANSFER_CUR);

   END CREA_PRELIQUIDACION;


  /*-----------------------------------------------------------------------------------
    Nombre : ANULA_PRELIQUIDACION
    Prop�sito: Anula una preliquidacion, liberando sus operaciones asignadas
    Referencia : CDC-00054
    Parametros :
                P_nNUMPRELIQ IN NUMBER       C�digo de la Pre-liquidaci�n

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10-11-2009       Marko Chu               Creaci�n <N3033740>

    -----------------------------------------------------------------------------------*/
  PROCEDURE ANULA_PRELIQUIDACION
  (
    p_nNUMPRELIQ NUMBER
  ) IS
  BEGIN

    UPDATE  OPER_INTER OI
      SET OI.INDPRELIQ = 'N', OI.NUMPRELIQ = NULL
    WHERE numpreliq = p_nNUMPRELIQ
      AND OI.INDPRELIQ = 'S'
      AND OI.NUMLIQ IS NULL;

  END ANULA_PRELIQUIDACION;

  /*-----------------------------------------------------------------------------------
    Nombre : ESTADO_PRELIQUIDACION
    Prop�sito:  Retorna el estado de una preliquidacion
                El estado puede ser :
                  ACT en caso tenga OPER_INTER no pagadas
                  COB en caso todas las oper inters esten pagadas
                  ANU si no existe la preliquidacion

    Referencia : CDC-00054
    Parametros :
                P_nNUMPRELIQ IN NUMBER       C�digo de la Pre-liquidaci�n

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10-11-2009       Marko Chu               Creaci�n <N3033740>

  -----------------------------------------------------------------------------------*/
  FUNCTION ESTADO_PRELIQUIDACION
  (
    p_nNUMPRELIQ NUMBER
  ) RETURN VARCHAR2
  IS
    nNumPendientes NUMERIC;
    nNumPagadas NUMERIC;
    nComisiones NUMERIC;
    cEstado VARCHAR2(3);

  BEGIN

    SELECT count(1)
    INTO nComisiones
    FROM OPER_INTER
    WHERE numpreliq = p_nNUMPRELIQ
      AND INDPRELIQ = 'S';

    SELECT count(1)
    INTO nNumPagadas
    FROM OPER_INTER
    WHERE numpreliq = p_nNUMPRELIQ
      AND NOT NUMLIQ IS NULL
      AND INDPRELIQ = 'S';

    SELECT count(1)
    INTO nNumPendientes
    FROM OPER_INTER
    WHERE numpreliq = p_nNUMPRELIQ
      AND NUMLIQ IS NULL
      AND INDPRELIQ = 'S';

    IF nComisiones = 0 THEN
      cEstado := 'ANU';
    ELSE

      IF nNumPendientes > 0 THEN
        cEstado := 'ACT';
      ELSE
        cEstado := 'COB';
      END IF;

    END IF;

    RETURN cEstado;

  END ESTADO_PRELIQUIDACION;


/*-----------------------------------------------------------------------------------
    Nombre : BUSCA_PRELIQ_PENDIENTES
    Prop�sito: Anula una preliquidacion, liberando sus operaciones asignadas
    Referencia : CDC-00054
    Parametros :
                P_nNUMIDINTER     NUMID del Intermediario
                P_cFECINI         Fecha inicial del rango de busqueda en formato YYYY-MM-DD
                P_cFECFIN         Fecha final del rango de busqueda en formato YYYY-MM-DD

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10-11-2009       Marko Chu               Creaci�n <N3034247>
    06-09-2022       David Yupanqui          Modificacion <RTC 314968>
    13-12-2022       Marcopolo Quispe H.     Modificacion <RTC 315747> CORRECTIVO Visualizaci�n de pre liquidaciones con antiguedad menor a 1 a�o
    -----------------------------------------------------------------------------------*/
  FUNCTION BUSCA_PRELIQ_PENDIENTES
  (
    P_nNUMIDINTER IN NUMBER,
    P_cFECINI IN VARCHAR2,
    P_cFECFIN IN VARCHAR2
  )  RETURN TRANSFER_REF IS

    cCodInter VARCHAR2(50);

    TRANSFER_CUR TRANSFER_REF;

  BEGIN

    select codinter
    INTO cCodInter
    from intermediario
    where numid = P_nNUMIDINTER;

    OPEN TRANSFER_CUR FOR   
  select oi.numpreliq NROPRELIQ, oi.codmoneda CODMONEDA, oi.codcia CODCIA, decode(oi.indasesor,'N','COM','ASE') TIPOPRELIQ,  sum(oi.montooperinterforanea) MONTO
    from oper_inter oi
    where oi.numpreliq IS NOT NULL
    and  OI.STSOPERINTER = 'ACT'
    and oi.numliq is null
    and oi.codinter = cCodInter
    and not exists (select 1
      from oper_inter op
      where op.numpreliq = oi.numpreliq
      and op.tipooperinter not in ('COMI','EXTR','EXMO','REMO')) --<RTC 315747> Marcopolo Quispe H. / 13-12-2022 / CORRECTIVO Visualizaci�n de pre liquidaciones con antiguedad menor a 1 a�o
    group by oi.numpreliq, oi.codmoneda, oi.codcia, oi.indasesor
    order by oi.numpreliq;    

      RETURN TRANSFER_CUR;

  END BUSCA_PRELIQ_PENDIENTES;

  -- Create Carlos Riguetti - Requerimiento: DEFPROC-00171 - Reserva: N3035540
  --
  --   05/10/2010  Heli rojas HelpDesk N� 87108
  --   16/02/2017  Italo Rivas  REQ. 93777
  --   18/10/2022  Carlos Torres -<RTC 292359> Mejoras abono cuenta Acsel/X - Proyecto H2H
  --
  FUNCTION CONSULTA_CHEQUES(
    P_nNUMIDINTER IN NUMBER,
    P_cCODCIA     IN VARCHAR2,
    P_cCODMONEDA  IN VARCHAR2,
    P_cFECINI     IN VARCHAR2,
    P_cFECFIN     IN VARCHAR2,
    P_cSTSCHQ     IN VARCHAR2,
    P_cSerie      IN VARCHAR2,
    P_nNumDoc     IN NUMBER,
    P_nNumLiq     IN NUMBER) RETURN TRANSFER_REF IS

    TRANSFER_CUR PR_API_COMISION_INTERMEDIARIO.TRANSFER_REF;
    cCodInter     OPER_INTER.CODINTER%TYPE := NULL;
    cCodCia       OPER_INTER.CODCIA%TYPE;
    cCodMoneda    OPER_INTER.CODMONEDA%TYPE;
    dFecIni       DATE;
    dFecFin       DATE;
    cStsChq       VARCHAR2(1); --N ABONADO S EN PROCESO ;
    cSerie        FACTURA_EXT.SERIEFACTEXT%TYPE;
    nNumDoc       FACTURA_EXT.NUMFACTEXT%TYPE;
    nNumLiq       OPER_INTER.NumLiq%TYPE;
    nNumOblig      OPER_INTER.NumOblig%TYPE  := NULL;
    v_fecha_ppl   DATE; -- Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
    
    cIndConH2H       VARCHAR2(1) := PR.BUSCA_LVAL('ACTH2H', 'SW_CPROV'); --<RTC-292359> - Desarrollo REL - Billy Fernandez / 16/03/2023
  BEGIN
    --<Ini> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
    BEGIN
      SELECT to_date(lval_val,'dd/mm/yyyy')
        INTO v_fecha_ppl
        FROM acselx.pivotal_lval l  
       WHERE l.lval_cod='FEC_PPL' AND l.lval_est=1;
    EXCEPTION WHEN OTHERS THEN 
      v_fecha_ppl:=to_date('01/01/2017','dd/mm/yyyy');
    END;
    --<Fin> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
      
    IF p_nNumIdInter IS NOT NULL THEN
      --<I N3035773> Katia Le�n/10-09-2010/Hallar C�digo de Intermediario
      --cCodInter     := LPAD(TO_CHAR(p_nNumIdInter),6,'0');
      SELECT CodInter
      INTO cCodInter
      FROM Intermediario
      WHERE NumId = p_nNumIdInter;
      --<F N3035773>
    END IF;
    if p_cCodCia = '00' then
      cCodCia := null;
    else
      cCodCia       := p_cCodCia;
    end if;
    cCodMoneda    := p_cCodMoneda;
    dFecIni       := TO_DATE(p_cFecIni,'YYYY-MM-DD');
    dFecFin       := TO_DATE(p_cFecFin,'YYYY-MM-DD')+1;
    cStsChq       := p_cStsChq;
    cSerie        := p_cSerie ;
    nNumDoc       := p_nNumDoc;
    nNumLiq       := p_nNumLiq;

    IF p_cSerie IS NOT NULL AND NVL(p_nNumDoc,0) != 0 THEN
      BEGIN
        SELECT MIN(NUMOBLIG)
        INTO nNumOblig
        FROM FACTURA_EXT
        WHERE NUMIDEXT     = p_nNumIdInter
          AND TIPOFACTEXT  = '01' --Factura
          --<Ini> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
          --AND SERIEFACTEXT = p_cSerie
          AND lpad(SERIEFACTEXT,4,0) = lpad(p_cSerie,4,0)
          --<Fin> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
          AND NUMFACTEXT   = p_nNumDoc;
      EXCEPTION
        WHEN OTHERS THEN
          nNumOblig := -99999999999999;
      END;
    END IF;

    IF nNumLiq IS NOT NULL OR nNumOblig IS NOT NULL THEN  --<I N3035773> Katia Le�n/10-09-2010/Se adiciona IF y QUERY- Optimizaci�n de tiempo de respuesta
      OPEN TRANSFER_CUR FOR
      SELECT /*+ CHOOSE*/ O.CODCIA CODCIA,
             DECODE(O.CODCIA,'01','RIMAC SEGUROS','RIMAC EPS') COMPANIA,
             DECODE(L.FECABONOBCO,NULL,'EN PROCESO DE ABONO','ABONADO') ESTADO,
             --O.CODINTER   NROCORREDOR,  HelpDesk N� 87108
             p_nNumIdInter NROCORREDOR, --HelpDesk N� 87108
             PR_TERCERO.DESC_APE_NOM('N',(SELECT NUMID FROM INTERMEDIARIO WHERE CODINTER=O.CODINTER)) NOMCORREDOR,
             O.NUMLIQ NROLIQUIDACION,
             TO_CHAR(O.FECHA_LIQUIDACION,'DD/MM/YYYY') FECLIQUIDACION,
             TO_CHAR(L.FECABONOBCO,'DD/MM/YYYY') FECABONO,
             (SELECT SERIEFACTEXT||'-'||NUMFACTEXT FROM FACTURA_EXT WHERE NUMOBLIG=O.NUMOBLIG) NROFACTURAPAGO,
             L.NUMCTABCO NROCHEQUE,
             O.CODMONEDA MONEDA,
             OB.MTONETO MONTO,
             (SELECT DESCENTFINAN FROM ENT_FINAN WHERE CODENTFINAN=L.CODENTFINAN) NOMBANCO,
             (SELECT MAX(EMAIL_INTER) FROM INTER_LOTE_PAGO WHERE CODINTER=O.CODINTER) EMAIL,
             L.NUMLOTE LOTE
             --<Ini> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
             , case 
               when (nvl(l.fecaplica,to_date('01/01/2200','dd/mm/yyyy')) - v_fecha_ppl )>=0  
               then 1
               else 0 end as indPPL
             --<Fin> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17        
      FROM OPER_INTER O,
           OBLIG_PROV_LOTEBBVA OB,
           LOTE_PAGO_BBVA L
      WHERE OB.NUMOBLIG           = O.NUMOBLIG+0
        AND OB.STSOBLIG           = 'INC'
        AND L.NUMLOTE             = OB.NUMLOTE
        AND (cStsChq IS NULL OR DECODE(L.FECABONOBCO,NULL,'S','N')=cStsChq )
        AND O.CODCIA||''          = NVL(cCodCia,O.CODCIA||'')
        AND O.CODINTER||''        = NVL(cCodInter,O.CODINTER||'')
        AND O.CODMONEDA||''       = NVL(cCodMoneda,O.CODMONEDA||'')
        AND (O.NUMOBLIG  = nNumOblig OR O.NUMLIQ = nNumLiq)
      GROUP BY O.CODCIA,O.CODINTER,O.NUMOBLIG,O.NUMLIQ,O.FECHA_LIQUIDACION,O.CODMONEDA,L.FECABONOBCO,L.NUMLOTE,L.NUMCTABCO,L.CODENTFINAN, OB.MTONETO
      --<Ini> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
      ,l.fecaplica
      --<Fin> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
      --<I RTC 292359> Carlos Torres / 18-10-2022 / Mejoras abono cuenta Acsel/X - Proyecto H2H
      --ORDER BY O.FECHA_LIQUIDACION DESC;
      UNION
      SELECT /*+ CHOOSE*/ 
             o.CODCIA CODCIA,
             DECODE(o.CODCIA,'01','RIMAC SEGUROS','RIMAC EPS') COMPANIA,
             DECODE(pa.FecPago,NULL,'EN PROCESO DE ABONO','ABONADO') ESTADO,
             p_nNumIdInter NROCORREDOR,
             PR_TERCERO.DESC_APE_NOM('N',(SELECT NUMID FROM INTERMEDIARIO WHERE CODINTER=o.CODINTER)) NOMCORREDOR,
             o.NUMLIQ NROLIQUIDACION,
             TO_CHAR(o.FECHA_LIQUIDACION,'DD/MM/YYYY') FECLIQUIDACION,
             TO_CHAR(pa.FecPago,'DD/MM/YYYY') FECABONO,
             (SELECT SERIEFACTEXT||'-'||NUMFACTEXT FROM FACTURA_EXT WHERE NUMOBLIG=o.NUMOBLIG) NROFACTURAPAGO,
             (SELECT NUMCTAABONO
                FROM CTAS_ABONO_H2H ca
               WHERE ca.IDENVIO = pa.IDENVIO
                 AND ca.NUMIDABONO = pa.NUMIDABONO
                 AND ca.CODBCOABONO= pa.CODBCOABONO
                 AND ca.IDCTAABONO = pa.IDCTAABONO
             ) NROCHEQUE,
             o.CODMONEDA MONEDA,
             pa.MONTONETO MONTO,
             PR_ABONO_H2H.OBTENER_NOM_BCO(CODBCOABONO) NOMBANCO,
             (SELECT MAX(EMAIL_INTER) FROM INTER_LOTE_PAGO WHERE CODINTER=o.CODINTER) EMAIL,
             pa.NUMLOTE LOTE,
             CASE 
             --<I RTC-292359> - Desarrollo REL - Billy Fernandez / 01/03/2023
             WHEN (NVL(pa.FecPago, to_date('01/01/2200','dd/mm/yyyy')) - v_fecha_ppl )>=0  
             --<F RTC-292359>
             THEN 1
             ELSE 0 END AS IndPPL
        FROM OPER_INTER O, PAGO_OBLIG_H2H pa
       WHERE 
         --<I RTC-292359> - Desarrollo REL - Billy Fernandez / 16/03/2023
             cIndConH2H = 'S' 
         AND PA.IDEPAGO IN (SELECT F1.IDEPAGO 
                              FROM ORDEN_PAGO F1 
                             WHERE F1.NUMOBLIG = O.NUMOBLIG)
         --<F RTC-292359>
         AND pa.NUMOBLIG = o.NUMOBLIG+0
         AND o.CODCIA||'' = NVL(cCodCia,o.CODCIA||'')
         AND o.CODINTER||'' = NVL(cCodInter,o.CODINTER||'')
         AND o.CODMONEDA||''= NVL(cCodMoneda,o.CODMONEDA||'')
         AND (o.NUMOBLIG  = nNumOblig OR o.NUMLIQ = nNumLiq)
         AND pa.StsPago NOT IN ('ANU', 'RCH')
         AND pa.IdPagoEnvio = (SELECT MAX(IdPagoEnvio)
                                 FROM PAGO_OBLIG_H2H po
                                WHERE po.IdePago = pa.IdePago
                                  AND po.StsPago NOT IN ('ANU', 'RCH')
                              )
       GROUP BY o.CODCIA,o.CODINTER,o.NUMOBLIG,o.NUMLIQ,o.FECHA_LIQUIDACION,o.CODMONEDA, pa.NUMLOTE, 
                pa.FecPago, --<RTC-292359> - Desarrollo REL - Billy Fernandez / 01/03/2023
                pa.IDENVIO, pa.NUMIDABONO, pa.CODBCOABONO, pa.IDCTAABONO, pa.MONTONETO
       order by  FECLIQUIDACION desc;
      --<F RTC 292359>
    --
    ELSE  --<F N3035773>

      OPEN TRANSFER_CUR FOR
      SELECT /*+ CHOOSE*/ /*<N3035773> no se requiere INDEX IX_OPER_INTER_02, OPER_INTER_OBLIG */
           O.CODCIA CODCIA,
           DECODE(O.CODCIA,'01','RIMAC SEGUROS','RIMAC EPS') COMPANIA,
           DECODE(L.FECABONOBCO,NULL,'EN PROCESO DE ABONO','ABONADO') ESTADO,
            --O.CODINTER   NROCORREDOR,  HelpDesk N� 87108
           p_nNumIdInter NROCORREDOR, --HelpDesk N� 87108
           PR_TERCERO.DESC_APE_NOM('N',(SELECT NUMID FROM INTERMEDIARIO WHERE CODINTER=O.CODINTER)) NOMCORREDOR,
           O.NUMLIQ NROLIQUIDACION,
           to_char(O.FECHA_LIQUIDACION,'dd/mm/yyyy') FECLIQUIDACION,
        to_char(L.FECABONOBCO,'dd/mm/yyyy') FECABONO,
        (SELECT SERIEFACTEXT||'-'||NUMFACTEXT FROM FACTURA_EXT WHERE NUMOBLIG=O.NUMOBLIG) NROFACTURAPAGO,
           L.NUMCTABCO NROCHEQUE,
           O.CODMONEDA MONEDA,
           OB.MTONETO MONTO, --SUM(O.MONTOOPERINTERFORANEA) MONTO,
           (SELECT DESCENTFINAN FROM ENT_FINAN WHERE CODENTFINAN=L.CODENTFINAN) NOMBANCO,
           (SELECT MAX(EMAIL_INTER) FROM INTER_LOTE_PAGO WHERE CODINTER=O.CODINTER) EMAIL,
        L.NUMLOTE LOTE
        --<Ini> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
        ,case 
         when (nvl(l.fecaplica,to_date('01/01/2200','dd/mm/yyyy')) - v_fecha_ppl )>=0  
         then 1
         else 0 end as indPPL
       --<Fin> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17 
      FROM OPER_INTER O ,
           OBLIG_PROV_LOTEBBVA OB,
           LOTE_PAGO_BBVA L
    --WHERE O.FECHA_LIQUIDACION  >= NVL(dFecIni,O.FECHA_LIQUIDACION) --<N3035773>no se requiere
    --  AND O.FECHA_LIQUIDACION   <= NVL(dFecFin,O.FECHA_LIQUIDACION) --<N3035773>no se requiere
      WHERE O.FECHA_LIQUIDACION  >=  dFecIni --<N3035773> Katia Le�n/13-09-2010/optimizacion de query
        AND O.FECHA_LIQUIDACION   <  dFecFin --<N3035773> Katia Le�n/13-09-2010/optimizacion de query
        AND O.CODCIA||''          = NVL(cCodCia,O.CODCIA)
        AND O.CODINTER||''        = NVL(cCodInter,O.CODINTER)
        AND O.CODMONEDA           = NVL(cCodMoneda,O.CODMONEDA)
        AND O.NUMOBLIG            = NVL(nNumOblig,O.NUMOBLIG)
        AND O.NUMLIQ              = NVL(nNumLiq,O.NUMLIQ)
        AND OB.NUMOBLIG           = O.NUMOBLIG
        AND OB.STSOBLIG           = 'INC'
        AND L.NUMLOTE             = OB.NUMLOTE
        AND (cStsChq IS NULL OR DECODE(L.FECABONOBCO,NULL,'S','N')=cStsChq )
      GROUP BY O.CODCIA,O.CODINTER,O.NUMOBLIG,O.NUMLIQ,O.FECHA_LIQUIDACION,O.CODMONEDA,L.FECABONOBCO,L.NUMLOTE,L.NUMCTABCO,L.CODENTFINAN, OB.MTONETO
      --<Ini> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
      ,l.fecaplica
      --<Fin> Modificaciones REQ. 93777 / Italo Rivas / 16-02-17
      --<I RTC 292359> Carlos Torres / 18-10-2022 / Mejoras abono cuenta Acsel/X - Proyecto H2H
    --order by  O.FECHA_LIQUIDACION desc;
    UNION
     SELECT /*+ CHOOSE*/ 
            o.CODCIA CODCIA,
            DECODE(o.CODCIA,'01','RIMAC SEGUROS','RIMAC EPS') COMPANIA,
            DECODE(pa.FecPago,NULL,'EN PROCESO DE ABONO','ABONADO') ESTADO,
            p_nNumIdInter NROCORREDOR,
            PR_TERCERO.DESC_APE_NOM('N',(SELECT NUMID FROM INTERMEDIARIO WHERE CODINTER=o.CODINTER)) NOMCORREDOR,
            o.NUMLIQ NROLIQUIDACION,
            to_char(o.FECHA_LIQUIDACION,'dd/mm/yyyy') FECLIQUIDACION,
            TO_CHAR(pa.FecPago,'DD/MM/YYYY') FECABONO,
            (SELECT SERIEFACTEXT||'-'||NUMFACTEXT FROM FACTURA_EXT WHERE NUMOBLIG=o.NUMOBLIG) NROFACTURAPAGO,
            (SELECT NUMCTAABONO
                FROM CTAS_ABONO_H2H ca
               WHERE ca.IDENVIO = pa.IDENVIO
                 AND ca.NUMIDABONO = pa.NUMIDABONO
                 AND ca.CODBCOABONO= pa.CODBCOABONO
                 AND ca.IDCTAABONO = pa.IDCTAABONO
            ) NROCHEQUE,
            o.CODMONEDA MONEDA,
            pa.MONTONETO MONTO,
            PR_ABONO_H2H.OBTENER_NOM_BCO(CODBCOABONO) NOMBANCO,
            (SELECT MAX(EMAIL_INTER) FROM INTER_LOTE_PAGO WHERE CODINTER=o.CODINTER) EMAIL,
            pa.NUMLOTE LOTE,
            CASE 
             --<I RTC-292359> - Desarrollo REL - Billy Fernandez / 01/03/2023
             WHEN (NVL(pa.FecPago, to_date('01/01/2200','dd/mm/yyyy')) - v_fecha_ppl )>=0  
             --<F RTC-292359>
             THEN 1
             ELSE 0 END AS IndPPL
       FROM OPER_INTER o, PAGO_OBLIG_H2H pa
      WHERE 
        --<I RTC-292359> - Desarrollo REL - Billy Fernandez / 16/03/2023
            cIndConH2H = 'S' 
        AND PA.IDEPAGO IN (SELECT F1.IDEPAGO 
                             FROM ORDEN_PAGO F1 
                            WHERE F1.NUMOBLIG = O.NUMOBLIG)
        --<F RTC-292359>
        AND o.FECHA_LIQUIDACION >= dFecIni
        AND o.FECHA_LIQUIDACION < dFecFin
        AND o.CODCIA||''  = NVL(cCodCia,o.CODCIA)
        AND o.CODINTER||''= NVL(cCodInter,o.CODINTER)
        AND o.CODMONEDA = NVL(cCodMoneda,o.CODMONEDA)
        AND o.NUMOBLIG  = NVL(nNumOblig,o.NUMOBLIG)
        AND o.NUMLIQ    = NVL(nNumLiq,o.NUMLIQ)
        AND pa.NUMOBLIG = o.NUMOBLIG
        AND pa.StsPago NOT IN ('ANU', 'RCH')
        AND pa.IdPagoEnvio = (SELECT MAX(IdPagoEnvio)
                                FROM PAGO_OBLIG_H2H po
                               WHERE po.IdePago = pa.IdePago
                                 AND po.StsPago NOT IN ('ANU', 'RCH')
                              )
      GROUP BY O.CODCIA,O.CODINTER,O.NUMOBLIG,O.NUMLIQ,O.FECHA_LIQUIDACION,O.CODMONEDA, pa.NUMLOTE, 
               pa.FecPago, --<RTC-292359> - Desarrollo REL - Billy Fernandez / 01/03/2023
               pa.IDENVIO, pa.NUMIDABONO, pa.CODBCOABONO, pa.IDCTAABONO, pa.MONTONETO
      order by  FECLIQUIDACION desc;
      --<F RTC 292359>

    END IF;  --<N3035773>

    RETURN TRANSFER_CUR;
  END CONSULTA_CHEQUES;
  -- Create Carlos Riguetti - Requerimiento: DEFPROC-00171 - Reserva: N3035540
  PROCEDURE OBTENER_IDDOCS_CONTENT (
    p_nNumLote NUMBER,
    p_nNumIdProv NUMBER,
    p_nCiaSeguros VARCHAR2,
    p_nSistOrig NUMERIC,
    p_cIdeDocFact OUT VARCHAR2 ,
    p_cIdeDocCert OUT VARCHAR2,
    p_cEmail OUT VARCHAR2
    ,p_cNomDocFact OUT VARCHAR2 --<N3038970><CDC-00089> MARCOS GOMEZ JIMENEZ  08/05/2012
    ,p_cNomDocCert OUT VARCHAR2 --<N3038970><CDC-00089> MARCOS GOMEZ JIMENEZ  08/05/2012
    ,p_nNumLiq VARCHAR2 DEFAULT NULL --<N3040669> Modificaciones CDC-00096 / Italo Rivas / 20-03-2013
  ) IS
  BEGIN
    --<INI><N3038970> CDC-00089 MARCOS GOMEZ JIMENEZ  08/05/2012
    --PR_ENVIOCORREOS.OBTENER_IDDOCS_CONTENT@DBL_BDWEB(p_nNumLote,p_nNumIdProv,p_nCiaSeguros,p_nSistOrig,p_cIdeDocFact, p_cIdeDocCert, p_cEmail);
    --<I N3040669> Modificaciones CDC-00096 / Italo Rivas / 20-03-2013
    --PR_ENVIOCORREOS.OBTENER_IDDOCS_CONTENT@DBL_BDWEB(p_nNumLote,p_nNumIdProv,p_nCiaSeguros,p_nSistOrig,p_cIdeDocFact, p_cIdeDocCert, p_cEmail, null , p_cNomDocFact , p_cNomDocCert);
      PR_ENVIOCORREOS.OBTENER_IDDOCS_CONTENT@DBL_BDWEB(p_nNumLote,p_nNumIdProv,p_nCiaSeguros,p_nSistOrig,p_cIdeDocFact, p_cIdeDocCert, p_cEmail, null , p_cNomDocFact , p_cNomDocCert, null, p_nNumLiq);
    --<F N3040669> Modificaciones CDC-00096 / Italo Rivas / 20-03-2013
    --<FIN><N3038970> CDC-00089 MARCOS GOMEZ JIMENEZ  08/05/2012

    BEGIN
      EXECUTE IMMEDIATE 'ALTER SESSION CLOSE DATABASE LINK DBL_BDWEB';
    EXCEPTION WHEN OTHERS THEN
      NULL;
    END;
  END OBTENER_IDDOCS_CONTENT;


/*-----------------------------------------------------------------------------------
    Nombre : CREA_OPER_INTER_PRELIQ
    Prop�sito: Registra el numero de preliquidaci�n, fecha, usuario y origen
    Referencia : CDC-00068
    Parametros :
                P_nNUMPRELIQ  IN NUMBER       N�mero de Preliquidaci�n
                P_cCODUSER    IN VARCHAR2     C�digo del usuario
                P_nNUMIDUSER  IN VARCHAR2     Numid  del usuario
                P_nNUMIDINTER IN VARCHAR2     Numid  del corredor
                P_cORIGEN     IN VARCHAR2     origen de la preliquidaci�n [W] Web / [A] ACSELX
                P_cTIPOPRELIQ IN VARCHAR2     Tipo de Preliquidaci�n [COM] Comisiones / [ASE] Asesor�as
                P_CODCIA      IN VARCHAR2     C�digo de la compa��a

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    14/10/2010       Marlon Castro G.        Creaci�n <N3035972>
    -----------------------------------------------------------------------------------*/

  PROCEDURE CREA_OPER_INTER_PRELIQ(
    P_nNUMPRELIQ IN NUMBER,
    P_cCODUSER IN VARCHAR2,
    P_nNUMIDUSER IN VARCHAR2,
    P_nNUMIDINTER IN VARCHAR2,
    P_cORIGEN IN VARCHAR2,
    P_cTIPOPRELIQ IN VARCHAR2,
    P_CODCIA IN VARCHAR2
  ) IS

  BEGIN

    INSERT INTO OPER_INTER_PRELIQ
      (numpreliq, coduser, numiduser, numidinter, feccrea, origen, tipopreliq, codcia)
    VALUES
      (P_nNUMPRELIQ, P_cCODUSER, P_nNUMIDUSER, P_nNUMIDINTER, SYSDATE, P_cORIGEN, P_cTIPOPRELIQ, P_CODCIA);

  END CREA_OPER_INTER_PRELIQ;



/*-----------------------------------------------------------------------------------
    Nombre : OBTENER_FECHA_CREA_PRELIQ
    Prop�sito: Obtiene la fecha de creaci�n de una preliquidaci�n
    Referencia : CDC-00068
    Parametros :
                P_nNUMPRELIQ  IN   NUMBER       N�mero de Preliquidaci�n
                P_dFecCrea  OUT  VARCHAR2       Fecha de Creaci�n formateada
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    14/10/2010       Marlon Castro G.        Creaci�n <N3035972>
    -----------------------------------------------------------------------------------*/
  PROCEDURE OBTENER_FECHA_CREA_PRELIQ(
      P_nNUMPRELIQ IN NUMBER,
      P_dFecCrea    OUT VARCHAR2
    ) IS

    dFechaCrea VARCHAR2(50);

    BEGIN dFechaCrea := '';
      BEGIN

      SELECT TO_CHAR(FECCREA,'DD/MM/YYYYHH24:MI:SS')
        INTO dFechaCrea
        FROM OPER_INTER_PRELIQ
       WHERE NUMPRELIQ = P_nNUMPRELIQ;

      EXCEPTION
        WHEN OTHERS THEN
          dFechaCrea := TO_CHAR(SYSDATE,'DD/MM/YYYYHH24:MI:SS');
      END;

      P_dFecCrea := dFechaCrea;
    END OBTENER_FECHA_CREA_PRELIQ;


/*-----------------------------------------------------------------------------------
    Nombre       : OBTENER_TEXTO_DOCUMENTO
    Prop�sito    : Obtiene el texto para el pie de pagina de un documento
    Referencia   : COM-00736
    Parametros   :
                p_cCodCia     IN VARCHAR2   Codigo de Compania
                p_cIndAse     IN VARCHAR2   Indicador de Asesoria
                p_cTipoDoc    IN VARCHAR2   Tipo de Documento PRELIQ[01], LIQ[02]
                P_cTexto      OUT VARCHAR2  Texto de salida
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    31/03/2011       Marlon Castro G.       Creaci�n <N3036861>
    -----------------------------------------------------------------------------------*/
  PROCEDURE OBTENER_TEXTO_DOCUMENTO(p_cCodCia  IN VARCHAR2,
                                    p_cIndAse  IN VARCHAR2,
                                    p_cTipoDoc IN VARCHAR2,
                                    P_cTexto   OUT VARCHAR2) IS

    cRpta VARCHAR2(4000) := '';

  BEGIN

    BEGIN

      SELECT DESCRIP
        INTO cRpta
        FROM TEXTO_LIQ
       WHERE CODCIA = p_cCodCia
         AND INDASESORIA = p_cIndAse
         AND CODREP = p_cTipoDoc;

    EXCEPTION
      WHEN OTHERS THEN
        cRpta := ' ';
    END;

    P_cTexto := cRpta;

  END OBTENER_TEXTO_DOCUMENTO;


/*-----------------------------------------------------------------------------------
    Nombre       : GET_NUM_OPER_INTER
    Prop�sito    : Obtiene un listado de NumOperInter
    Referencia   : CDC-00076
    Parametros   :
              p_nNumOper    IN NUMBER          Numero de Operacion,
              p_nIdeFact    IN NUMBER          Ide de Factura,
              p_cFecOper    IN VARCHAR2        Fecha de Operacion,
              p_cTipoOper   IN VARCHAR2        Tipo de Operacion,
              p_cIndAsesor  IN VARCHAR2        Indicador de asesoria,
              p_cCodCia     IN VARCHAR2        Codigo de compa�ia,
              p_cStsOper    IN VARCHAR2        Estado de Operacion,
              p_nCodInterm  IN NUMBER          Codigo de Intermediario
              p_nNumOperInter  IN NUMBER       Numero Maximo de Oper Inter

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    04/05/2011       Marlon Castro G.       Creaci�n <N3037392>
    08/08/2017       Wilmer Ponce de Leon   Modificaci�n <RTC 108953>
    -----------------------------------------------------------------------------------*/

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
                                         ) RETURN TRANSFER_REF IS

  TRANSFER_CUR TRANSFER_REF;

  BEGIN

  OPEN TRANSFER_CUR FOR

    SELECT NUMOPERINTER
      FROM OPER_INTER
     WHERE NumOper = p_nNumOper
       AND IdeFact = p_nIdeFact
       --INI RTC 108953 Wilmer Ponce de Leon 08/08/2017
       --AND FOperInter = TO_DATE(p_cFecOper, 'dd/MM/yyyy')
       AND TRUNC(FOperInter) = TO_DATE(p_cFecOper, 'dd/MM/yyyy')
       --FIN RTC 108953 Wilmer Ponce de Leon 08/08/2017
       AND TipoOperInter = p_cTipoOper
       AND IndAsesor = p_cIndAsesor
       AND CodCia = p_cCodCia
       AND CodMoneda = p_cCodMoneda
       AND StsOperInter = 'ACT'
       AND CodInter = (SELECT I.CODINTER
                         FROM INTERMEDIARIO I
                        WHERE  I.NUMID = p_nCodInterm)

    UNION

    SELECT NUMOPERINTER
      FROM OPER_INTER
     WHERE NumOper = p_nNumOper
       AND IdeFact IS NULL
       AND TipoOperInter = p_cTipoOper
       AND IdeRec IS NOT NULL
       AND CodInter = (SELECT I.CODINTER
                         FROM INTERMEDIARIO I
                        WHERE I.NUMID = p_nCodInterm)

    UNION

    SELECT NUMOPERINTER
      FROM OPER_INTER
     WHERE NumOperInter = p_nNumOperInter
       AND TipoOperInter = p_cTipoOper
       AND CodInter = (SELECT I.CODINTER
                         FROM INTERMEDIARIO I
                        WHERE  I.NUMID = p_nCodInterm);

  RETURN (TRANSFER_CUR);

  END;


/*-----------------------------------------------------------------------------------
    Nombre       : OBTENER_NUM_PRELIQ
    Prop�sito    : Obtiene un numero de preliquidacion
    Referencia   : CDC-00076
    Parametros   :
                   p_nNumPreliq  OUT      Nuevo numero de preliquidacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    04/05/2011       Marlon Castro G.       Creaci�n <N3037392>
    -----------------------------------------------------------------------------------*/
  PROCEDURE OBTENER_NUM_PRELIQ(p_nNumPreliq OUT NUMBER) IS

    nRpta OPER_INTER.NUMPRELIQ%TYPE;

  BEGIN

    BEGIN

      SELECT SQ_NUMPRELIQ.NEXTVAL
      INTO   nRpta
      FROM   DUAL;

    EXCEPTION
      WHEN OTHERS THEN
        nRpta := 0;
    END;

    p_nNumPreliq := nRpta;

  END OBTENER_NUM_PRELIQ;

/*-----------------------------------------------------------------------------------
    Nombre       : CREA_NUEVA_PRELIQ
    Prop�sito    : Crea una nueva preliquidacion a partir de la comisiones dadas
    Referencia   : CDC-00076
    Parametros   :
                   p_nNumPreliq     IN NUMBER      Nuevo numero de preliquidacion
                   p_nNumOperInter  IN NUMBER      Id de la tabla OPER_INTER

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    04/05/2011       Marlon Castro G.       Creaci�n <N3037392>
    15/09/2018       Alister Arnedo         RTC 150595  Se agrega los siguientes estados de extorno �EXMO� y �REMO�.
    -----------------------------------------------------------------------------------*/
  PROCEDURE CREA_NUEVA_PRELIQ(
    P_nNUMPRELIQ    IN NUMBER,
    P_nNUMOPERINTER IN NUMBER
  ) IS

  BEGIN

    UPDATE OPER_INTER OI
       SET OI.INDPRELIQ = 'S', OI.NUMPRELIQ = P_nNUMPRELIQ
       
     WHERE OI.NUMOPERINTER = P_nNUMOPERINTER
     --[INI] RTC 150595  - Alister Arnedo - 15/09/2018           
     --AND OI.TIPOOPERINTER IN ('COMI', 'EXTR')
      AND OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
      --[FIN] RTC 150595  - Alister Arnedo - 15/09/2018
       AND OI.STSOPERINTER = 'ACT'
       AND OI.INDPRELIQ = 'N'
       AND OI.NUMPRELIQ IS NULL;
      COMMIT;

  EXCEPTION WHEN OTHERS THEN
    ROLLBACK;

  END CREA_NUEVA_PRELIQ;
/*-----------------------------------------------------------------------------------
    Nombre       : BUSCA_CANAL
    Prop�sito    : Obtiene el la operaci�n de la comisi�n
    Referencia   : RTC 18505
    Parametros   :       
              p_nIdeFact    IN NUMBER          Ide de Factura,
              P_nNUMOBLIG   IN NUMBER          Numero de Obligacion,
              p_nNumOper    IN NUMBER          Numero de Operacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    26/01/2014       Jose Angeles           Creaci�n 
    -----------------------------------------------------------------------------------*/
  FUNCTION BUSCAR_OPERACION(
    P_nIDEFACT  FACTURA.IDEFACT%TYPE,
    P_nNUMOBLIG OBLIGACION_TABLE.NUMOBLIG%TYPE,
    p_nNumOper  OPER_POL.NUMOPER%TYPE
  ) RETURN OPER_POL.NUMOPER%TYPE IS
  
   nNumOper        OPER_POL.NUMOPER%TYPE;  
  BEGIN
    BEGIN
      IF(p_nIDEFACT IS NOT NULL) THEN
        SELECT CASE WHEN F.IDEFACTORIG IS NULL THEN F.NUMOPER
               ELSE (SELECT F2.NUMOPER
                     FROM   FACTURA F2
                     WHERE  F2.IDEFACT = F.IDEFACTORIG)
               END
               INTO nNumOper
        FROM FACTURA F 
        WHERE F.IDEFACT = p_nIDEFACT;      
      ELSIF(P_nNUMOBLIG IS NOT NULL) THEN
        SELECT O.NUMOPER
        INTO nNumOper
        FROM OBLIGACION_TABLE O
        WHERE  O.NUMOBLIG = P_nNUMOBLIG;      
      ELSE
        nNumOper:=p_nNumOper;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        nNumOper:=NULL;
    END;    
    RETURN nNumOper;
  END BUSCAR_OPERACION;
  
  
  

/*-----------------------------------------------------------------------------------
    Nombre       : BUSCA_PRIMA_NETA_TOTAL
    Prop�sito    : Obtiene la prima neta total
    Referencia   : RTC 30769
    Parametros   :       
              p_nNumOper    IN NUMBER          Numero de Operacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10/03/2015       Eddy Cordova           Creaci�n 
    -----------------------------------------------------------------------------------*/  
  FUNCTION BUSCA_PRIMA_NETA_TOTAL(
           p_codProd VARCHAR2,
           p_numPol VARCHAR2
  )RETURN NUMBER IS
          nMonto NUMBER;
  BEGIN
      BEGIN
        
select NVL(SUM(MTOMONEDA),0)
INTO nMonto
  from recibo
 where idepol = (select idepol
                   from poliza p
                  where p.codprod = p_codProd
                    and numpol = p_numPol
                    and numren = (select max(numren)
                                    from poliza p
                                   where p.codprod = p_codProd
                                     and numpol = p_numPol));
                                      EXCEPTION
          WHEN OTHERS THEN
            nMonto:=0;
        END;


        RETURN nMonto;
    END BUSCA_PRIMA_NETA_TOTAL;
  
/* -----------------------------------------------------------------------------------------------------
  Nombre      : FN_OBTENER_PRODUCTO
  Propsito    : Funcion obtiene Listado de productos
  Referencias : Requerimiento 48851
  Parametros  :
                p_nCodProd     --> Codigo del Producto
                p_cNombreProd  --> nombre del producto

  Log de Cambios
  Fecha       Autor                   Descripcion
  04/11/2015  Jose Angeles        Creaci�n.
  ----------------------------------------------------------------------------------------------------- */
FUNCTION BUSCAR_PRODUCTOS( 
         p_codProd  PRODUCTO.CODPROD%TYPE,
         p_desProd  PRODUCTO.Descprod%TYPE)
RETURN TRANSFER_REF IS

   cDatos TRANSFER_REF;
   BEGIN

     OPEN cDatos FOR
       SELECT p.codprod,p.descprod
       FROM producto p
       WHERE p.stsprod='ACT'
       AND ((p_codProd is null AND p_desProd is null)
       OR  ( p_desProd IS NULL  AND p.codprod like UPPER(p_codProd) || '%' )
       OR  ( p_codProd IS NULL  AND p.descprod like UPPER(p_desProd) || '%' ))
       
       ORDER BY p.codprod;
     RETURN cDatos;

END BUSCAR_PRODUCTOS;  

  /* -----------------------------------------------------------------------------------------------------
  Nombre      : FN_TRAESUSPEN_RS
  Propsito    : Funcion que Obtiene el Indicador de RimacSalud : N - No Suspendido / S - Suspendido.
  Referencias : Requerimiento 48851
  Parametros  : p_NumIdAseg -> Id del Asegurado.
                p_NumIdBrok -> Id del Broker.
                p_CodProd   -> C�digo de Producto.
                p_NumPol    -> Nro. de P�liza.

  Log de Cambios
  Fecha       Autor                   Descripcion
  07/06/2016  Gabriel D�az Diez       Creaci�n. <RTC 71288> - Suspensi�n de pago a Corredores por Norma SUSALUD 134-2015-SUSALUDS - AX.
  ----------------------------------------------------------------------------------------------------- */
  FUNCTION FN_TRAESUSPEN_RS (p_NumIdAseg TERCERO.NUMID%TYPE,
                             p_NumIdBrok TERCERO.NUMID%TYPE,
                             p_CodProd   POLIZA.CODPROD%TYPE,
                             p_NumPol    VARCHAR2) RETURN VARCHAR2 IS

  cRpta VARCHAR2(1) := PR.BUSCA_LVAL('SUSRSCO', 'VALNEG');
  BEGIN
    IF INSTR(PR.BUSCA_LVAL('SUSRSCO', 'CODPROD'), p_CodProd) != 0 THEN
      --Funci�n que verifica en RimacSalud
      cRpta := PR_POLIZA.F_OBTIENE_STATUS_SUSALUD@DBL_BDAPPPROD(p_ncodcliente => p_NumIdAseg,
                                                                p_cnropoliza  => p_NumPol,
                                                                p_ncodbroker  => p_NumIdBrok,
                                                                p_ntes        => 1,
                                                                p_nces        => 4,
                                                                p_ccodprod    => 'S');
      COMMIT;
      BEGIN
        EXECUTE IMMEDIATE 'ALTER SESSION CLOSE DATABASE LINK DBL_BDAPPPROD';
      EXCEPTION WHEN OTHERS THEN
        NULL;
      END;
      COMMIT;
    ELSE
      cRpta := PR.BUSCA_LVAL('SUSRSCO', 'VALNEG');
    END IF;

    cRpta := SUBSTR(NVL(cRpta, PR.BUSCA_LVAL('SUSRSCO', 'VALNEG')), 1, 1);
    RETURN ( cRpta );
  EXCEPTION WHEN OTHERS THEN
     Raise_application_error(-20000, 'PR_API_COMISION_INTERMEDIARIO.FN_TRAESUSPEN_RS : ' || SQLERRM);
     RETURN ( PR.BUSCA_LVAL('SUSRSCO', 'VALNEG') );
  END FN_TRAESUSPEN_RS;
  
  /*-----------------------------------------------------------------------------------
   Nombre       : FR_VAL_BLOQ_SBS_INTER
   Prop�sito    : Valida Bloqueo por la SBS
   Referencia   : RTC 82636
   Parametros   :       
             p_nNumId    IN NUMBER          N�mero de Identificacion Unico del corredor
             p_ccodCia   IN VARCHAR2        C�digo de Compa�ia

   Log de Cambios
   Fecha            Autor                  Descripcion
   ----------       -----------------      -------------------------------------------
   20/10/2016       Jos� Delgado           <RTC-82636> Creaci�n
   -----------------------------------------------------------------------------------*/   
  FUNCTION FR_VAL_BLOQ_SBS_INTER(
                                  p_nNumId   NUMBER
                                 ,p_ccodCia  VARCHAR2
                                )
  RETURN NUMBER
  IS
     nExiste NUMBER;
  BEGIN
      
      BEGIN
        SELECT 1
          INTO nExiste
          FROM BLOQUEOS_TERCEROS B
         WHERE numid   = p_nNumId
           AND StsBloq = 'ACT'
           AND (    ( FECFINBLOQ   IS NULL   AND FECINIBLOQ < SYSDATE ) 
                 OR 
                    ( TRUNC(FECFINBLOQ) + 1 > SYSDATE AND FECINIBLOQ < SYSDATE )  
               )
           AND EXISTS ( SELECT 1 
                          FROM LVAL A
                         WHERE A.TIPOLVAL = 'VINBLOBR'
                           AND A.CODLVAL  = B.CODVINC 
                      )
           AND EXISTS ( SELECT 1 
                          FROM LVAL A
                         WHERE A.TIPOLVAL = 'CODBLBRO'
                           AND A.CODLVAL  = B.CODBLOQ 
                      )
           AND EXISTS ( SELECT 1 
                          FROM LVAL C
                         WHERE C.TIPOLVAL = 'CIABLBRO'
                           AND C.CODLVAL  =  p_ccodCia
                       )
           AND NOT EXISTS ( SELECT 1
                              FROM BLOQUEOS_TERCEROS C
                             WHERE C.numid   = p_nNumId
                               AND C.numid   = B.numid
                               AND C.StsBloq = 'ACT'
                               AND (    ( C.FECFINBLOQ   IS NULL   AND C.FECINIBLOQ < SYSDATE ) 
                                     OR 
                                        ( TRUNC(C.FECFINBLOQ) + 1 > SYSDATE AND C.FECINIBLOQ < SYSDATE )  
                                   )
                               AND EXISTS ( SELECT 1 
                                              FROM LVAL A
                                             WHERE A.TIPOLVAL = 'VINBLOBR'
                                               AND A.CODLVAL  = C.CODVINC 
                                          )
                               AND NOT EXISTS ( SELECT 1 
                                                  FROM LVAL A
                                                 WHERE A.TIPOLVAL = 'CODBLBRO'
                                                   AND A.CODLVAL  = C.CODBLOQ 
                                              )
                         )
           ;
      
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           nExiste := 0;
        WHEN TOO_MANY_ROWS THEN
           nExiste := 1;
        WHEN OTHERS THEN
           nExiste := 0;
      END;
  

    RETURN  nExiste;
  END;

 /*-----------------------------------------------------------------------------------
   Nombre       : FR_VAL_OTR_BLOQ_INTER
   Prop�sito    : Valida Otros Bloqueos del Intermediario
   Referencia   : RTC 82636
   Parametros   :       
             p_nNumId    IN NUMBER          N�mero de Identificacion Unico del corredor
   Log de Cambios
   Fecha            Autor                  Descripcion
   ----------       -----------------      -------------------------------------------
   20/10/2016       Jos� Delgado           <RTC-82636> Creaci�n
   -----------------------------------------------------------------------------------*/  
  FUNCTION FR_VAL_OTR_BLOQ_INTER(
                                  p_nNumId   NUMBER
                                 )
  RETURN NUMBER
  IS
     nExiste NUMBER;
  BEGIN
      BEGIN
        SELECT 1
          INTO nExiste
          FROM BLOQUEOS_TERCEROS B
         WHERE NumId   = p_nNumId
           AND StsBloq = 'ACT'
           AND EXISTS ( SELECT 1 
                          FROM LVAL A
                         WHERE A.TIPOLVAL = 'VINBLOBR'
                           AND A.CODLVAL  = B.CODVINC 
                       )
          ;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          nExiste :=  0;
        WHEN TOO_MANY_ROWS THEN
          nExiste :=  1;
        WHEN OTHERS THEN
          nExiste :=  0;
      END;
     RETURN nExiste;
  END;

  /*-----------------------------------------------------------------------------------
    Nombre : DETALLE_PRELIQUIDACION_CORTO
    Prop�sito: Dado un c�digo de pre-liquidaci�n, presenta el detalle correspondiente solo con los campos necesarios para generar pdf de liquidaci�n.
    Referencia : Requerimiento 100411
    Parametros :
                P_nNUMPRELIQ IN NUMBER       C�digo de la Pre-liquidaci�n
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    12/05/2017       Jose Angeles      Creaci�n
    -----------------------------------------------------------------------------------*/

  FUNCTION DETALLE_PRELIQUIDACION_CORTO(
    P_nNUMPRELIQ IN NUMBER
  ) RETURN TRANSFER_REF IS

  TRANSFER_CUR TRANSFER_REF;
  BEGIN
    OPEN TRANSFER_CUR FOR
         SELECT /*+ ALL_ROWS */
                PR_EDOCTA_INTERMEDIARIO_R1420.DESCRIPCION_PRODUCTO(X.CODPROD,15) NOMPROD,
                (SELECT CASE WHEN T.TIPOTER = 'P'
                                  THEN T.APETER||' '||T.APEMATTER|| ' '||T.NOMTER
                                  ELSE T.NOMTER
                        END
                 FROM   TERCERO T
                 WHERE  T.NUMID = X.NUMIDCLIENTE) NOMCLIE,
                X.NUMPOL NUMPOL,
                X.NUMDOCCOM NUMDOCCOM,
                X.NUMDOCSUNAT NUMDOCSUNAT,
                X.TIPOOPERINTER TIPOOPERINTER,
                CASE WHEN ABS(X.MONTO) > ABS(X.PRIMA) OR X.PRIMA = 0
                          THEN 0
                          ELSE ROUND((X.MONTO / X.PRIMA) * 100,2)
                END PORCOM,
                X.PRIMA PRIMA,
                X.MONTO MONTO,
                TO_CHAR(X.FECPAGO,'DD/MM/YYYY') FECPAGO
         FROM   (SELECT /*+ ALL_ROWS */
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) CODPROD,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMIDCLIENTE,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FNUMPOL(OI2.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BNUMPOL(OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMPOL,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOC||'-'||F.NUMFACT
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOC||'-'||O.NUMOBLIG
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM DUAL) NUMDOCCOM,
                        (SELECT CASE WHEN OI2.IDEFACT IS NOT NULL
                                          THEN (SELECT F.TIPODOCPAG||'-'||F.SERIE||'-'||F.NUMDOC
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI2.IDEFACT)
                                          ELSE (SELECT O.TIPODOCPAG||'-'||O.SERIE||'-'||O.NUMDOC
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI2.OBLIG_PRI)
                                END
                         FROM   DUAL) NUMDOCSUNAT,
                        OI2.TIPOOPERINTER TIPOOPERINTER,
                        (SELECT SUM(DA.MTODETACREMONEDA)
                         FROM   DET_ACRE DA
                         WHERE  DA.NUMACRE IN(SELECT A.NUMACRE
                                              FROM   ACREENCIA_TABLE A
                                              WHERE  A.IDEFACT = OI2.IDEFACT)
                         AND    DA.CODGRUPOACRE = 'PRIMAS') PRIMA,
                        OI2.MONTO MONTO,
                        (
                          SELECT RI.FECSTSRELING
                          FROM REL_ING RI
                          WHERE RI.NUMRELING = (
                                               SELECT F.NUMRELING
                                               FROM   FACTURA F
                                               WHERE F.IDEFACT = OI2.IDEFACT
                                             )
                        ) FECPAGO
                 FROM   (SELECT /*+ INDEX(OI IDX_NUMPRELIQ) */
                                OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,
                                OI.NUMOBLIG,
                                SUM(OI.MONTOOPERINTERFORANEA) MONTO
                         FROM   OPER_INTER OI
                         WHERE  OI.NUMPRELIQ = P_nNUMPRELIQ
                         GROUP BY OI.IDEFACT,OI.OBLIG_PRI,OI.TIPOOPERINTER,OI.FOPERINTER,
                                  OI.FECHA_LIQUIDACION,OI.NUMOBLIG,OI.NUMLIQ,OI.NUMOPERCAJA) OI2
                ) X;

    RETURN(TRANSFER_CUR);
  END DETALLE_PRELIQUIDACION_CORTO;


--------------------------------------------------------------------------------------------------------------
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
PROCEDURE SP_MNT_COBERT_CERT_C(
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
  p_cTipo        IN CHAR(1),
  p_cError       OUT VARCHAR2(3000)
) IS
    -- ---------------
  TYPE Cursor_Cobert IS REF CURSOR;
  c_Cobert Cursor_Cobert;
  
  P   POLIZA%ROWTYPE;
  R   CERT_RAMO%ROWTYPE;
  CPP COBERT_PLAN_PROD%ROWTYPE;
  
  TYPE COBERTCERT IS TABLE OF COBERT_CERT%ROWTYPE INDEX BY PLS_INTEGER;
  rB01_1 COBERTCERT;
  POSICION PLS_INTEGER := 1;
  
  nTipTasa NUMBER(4);
  -- ------------------------------------------------------------------
  n_Existe NUMBER;  

  -- CPPD COBERT_PLAN_PROD%ROWTYPE;
  cMensajeRestriccionSAS VARCHAR2(400);

  CURSOR C_COBERTCERT IS
    SELECT * FROM COBERT_CERT CC
    WHERE CC.IDEPOL    = p_nIdePol
    AND CC.NUMCERT     = p_nNumCert
    AND CC.CODRAMOCERT = p_cCodRamoCert;
    
  CP COBERT_CERT%ROWTYPE;

  cCodCobertDep VARCHAR2(4);
  nPORCPRIMADEP NUMBER(11);

  nTasaCambio    NUMBER(30);
  nTasaCambioPol NUMBER(30);
  cEliminar      CHAR(1) := 'N';
  cCODCOBERT     COBERT_PLAN_PROD.CODCOBERT%TYPE := 'TR12';
  
BEGIN
    p_cError := '0,OK';
    
    P := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||P.IdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||P.IdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||P.IdePol);
    END IF;

    R := PR_CERT_RAMO.Datos_Cert_Ramo(p_nIdePol,p_nNumCert,p_cCodRamoCert);
    IF R.CodRamoCert IS NULL THEN
       p_cError := '1,'||'No existe el Ramo :'||p_cCodRamoCert;
       DBMS_OUTPUT.PUT_LINE('No existe el Ramo :'||p_cCodRamoCert);
       RAISE_APPLICATION_ERROR(-20100,'No existe el Ramo '||p_cCodRamoCert);
    END IF;

    
  -- ============================================================
  -- ELIMINAR LAS COBERTURAS
  -- ============================================================
  cMensajeRestriccionSAS := pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
   
  IF NOT cMensajeRestriccionSAS IS NULL THEN    
      p_cError := '1,'||cMensajeRestriccionSAS;
      DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
     -- RAISE_APPLICATION_ERROR(-20104,cMensajeRestriccionSAS);
  END IF;
  
  OPEN C_COBERTCERT;
  FETCH C_COBERTCERT INTO CP;
  WHILE C_COBERTCERT%FOUND LOOP
    IF CP.STSCOBERT IN ('VAL','INC') THEN       
       IF CP.STSCOBERT = 'VAL' THEN
            BEGIN
              SELECT NVL(MAX(1),0)
              INTO   n_Existe
              FROM   DUAL
              WHERE  EXISTS ( SELECT 1 
                              FROM   MOD_COBERT
                              WHERE  IdeCobert  = CP.IdeCobert
                              AND    StsModCobert NOT IN ('VAL'));                        
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                n_Existe := 0;
            END;
       END IF;
       IF n_Existe = 1 THEN
            p_cError := '1,'||'Hay registros de modificaciones de la cobertura ,NO debe ser eliminada...';
            DBMS_OUTPUT.PUT_LINE('Hay registros de modificaciones de la cobertura ,NO debe ser eliminada...');
            RAISE_APPLICATION_ERROR(-20104,'Hay registros de modificaciones de la cobertura ,NO debe ser eliminada...');
       END IF;

       CPP := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(P.codprod,CP.CodPlan,CP.RevPlan,CP.CodRamoCert,CP.CodCobert);
       
       IF CPP.CodCobert IS NOT NULL AND CPP.IndCobertOblig = 'S' THEN
              p_cError := '1,'||' La Cobertura '||CP.CodCobert||' esta CONFIGURADA como obligatoria por lo cual NO puede ser eliminada...';
              DBMS_OUTPUT.PUT_LINE(' La Cobertura '||CP.CodCobert||' esta CONFIGURADA como obligatoria por lo cual NO puede ser eliminada...');
              RAISE_APPLICATION_ERROR(-20104,' La Cobertura '||CP.CodCobert||' esta CONFIGURADA como obligatoria por lo cual NO puede ser eliminada...');
       END IF;

       IF p_cTipo = 'P' THEN
            cEliminar := 'S';
       ELSIF p_cTipo = 'R' AND CP.CODCOBERT NOT IN('TR13','TR14') THEN
            cEliminar := 'S';
       END IF;

       IF cEliminar = 'S' THEN
          DELETE FROM COBERT_CERT C
          WHERE C.IDECOBERT = CP.IDECOBERT
          AND C.STSCOBERT IN('INC','VAL');
      
          PR_CERT_RAMO.GENERAR_CLAUSULAS_ASOCIADAS(CP.IdePol,CP.NumCert, P.codprod,
                                              R.CodPlan,R.RevPlan,CP.CodRamoCert,
                                               CP.FecIniValid,CP.FecFinValid);
       
          cEliminar := 'N';
       END IF;
       
    END IF;
    FETCH C_COBERTCERT INTO CP;
  END LOOP;
  CLOSE C_COBERTCERT;
  -- ============================================================
  -- FIN ELIMINAR LAS COBERTURAS
  -- ============================================================
    
    OPEN c_Cobert FOR
        SELECT * FROM COBERT_PLAN_PROD 
        WHERE CodProd   = P.CodProd 
        AND CodPlan     = R.CodPlan
        AND RevPlan     = R.RevPlan
        AND CodRamoPlan =  p_cCodRamoCert
        AND TipoCobert  = 'G'
        AND CODCOBERT   = cCODCOBERT
        AND (IndCobPoliza <> 'S' OR IndCobPoliza IS NULL)
        AND NOT EXISTS (SELECT 1 
                        FROM COBERT_CERT
                        WHERE IdePol     = p_nIdePol
                        AND  NumCert     = p_nNumCert
                        AND  CodRamoCert = p_cCodRamoCert
                        AND  CodCobert   = COBERT_PLAN_PROD.CodCobert )
                        AND NOT EXISTS  (SELECT 1
                                        FROM  RAMO_PLAN_PROD RPP
                                        WHERE RPP.CodProd     = P.CodProd
                                        AND   RPP.CodPlan     = R.CodPlan
                                        AND   RPP.RevPlan     = R.RevPlan
                                        AND   RPP.CodRamoPlan = p_cCodRamoCert
                                        AND   NVL(RPP.CodCobertDec,'*')   = COBERT_PLAN_PROD.CodCobert
                                        UNION
                                        SELECT 1
                                        FROM  RAMO_PLAN_PROD RPP
                                        WHERE RPP.CodProd     = P.CodProd
                                        AND   RPP.CodPlan     = R.CodPlan
                                        AND   RPP.RevPlan     = R.RevPlan
                                        AND   RPP.CodRamoPlan = p_cCodRamoCert
                                        AND   NVL(RPP.CodCobertDep,'*') = COBERT_PLAN_PROD.CodCobert
                                        UNION
                                        SELECT 1
                                        FROM  RAMO_PLAN_PROD RPP
                                        WHERE RPP.CodProd    = P.CodProd
                                        AND   RPP.CodPlan     = R.CodPlan
                                        AND   RPP.RevPlan     = R.RevPlan
                                        AND   RPP.CodRamoPlan = p_cCodRamoCert
                                        AND   NVL(RPP.CodCobertLiq,'*') = COBERT_PLAN_PROD.CodCobert);
 
    
    FETCH c_Cobert INTO CPP;
    CLOSE c_Cobert;

  -----------------------------------------------------------------------  
  -- .:. FIN DE SELECCIONAR LA COBERTURA DEL CERTIFICADO "C"
  -----------------------------------------------------------------------
    
    rB01_1(POSICION).codcobert  := CPP.CodCobert;
    rB01_1(POSICION).porcded    := CPP.PorcDed;
    rB01_1(POSICION).mtodedmin  := CPP.MtoDedMin;
    rB01_1(POSICION).mtodedmax  := CPP.MtoDedMax;
    rB01_1(POSICION).baseded    := CPP.BaseDed;
    rB01_1(POSICION).indsumaded := CPP.IndSumaDed;
    rB01_1(POSICION).numordvis  := CPP.NumOrdVis;
    
    BEGIN 
        PR_CERT_RAMO.GENERA_CLAUSULAS_X_COBERTURA(p_nIdePol,p_nNumCert,p_cCodRamoCert,
                                                  rB01_1(POSICION).codcobert,R.fecinivalid,
                                                  R.fecfinvalid,R.stscertramo,P.codprod,
                                                  R.codplan,R.revplan);                           
    EXCEPTION
         WHEN OTHERS THEN
          p_cError := '1,'||SQLERRM;
         DBMS_OUTPUT.PUT_LINE('1,'||SQLERRM);
         RAISE_APPLICATION_ERROR(-20100,SQLERRM);
    END;
    
    BEGIN
        SELECT CodCobertDep
        INTO   cCodCobertDep
        FROM   RAMO_PLAN_PROD
        WHERE  CodProd   = P.codprod
        AND  CodPlan     = R.codplan 
        AND  RevPlan     = R.revplan 
        AND  CodRamoPlan = p_cCodRamoCert;
    EXCEPTION
          WHEN NO_DATA_FOUND THEN 
            NULL;
    END;
    -- :C00.nPorcPrimaDep := NVL(R.PorcPrimaDep,0);
    IF cCodCobertDep = rB01_1(POSICION).codcobert THEN
          nPORCPRIMADEP := NVL(R.PorcPrimaDep,0);
    ELSE
          nPORCPRIMADEP :=0;
    END IF;

    BEGIN
      SELECT SQ_COBERT.NEXTVAL
      INTO   rB01_1(POSICION).idecobert
      FROM   SYS.DUAL;
    END;
  -----------------------------------------------------------------------  
  -- .:. FIN DEL EVENTO DEL BOTON ACEPTAR SELECCIÓN MULTIPLE
  -----------------------------------------------------------------------

  rB01_1(POSICION).idepol       := p_nIdePol;
  rB01_1(POSICION).numcert      := p_nNumCert;
  rB01_1(POSICION).codramocert  := p_cCodRamoCert;
  rB01_1(POSICION).codplan      := R.CodPlan;
  rB01_1(POSICION).revplan      := R.RevPlan;
  rB01_1(POSICION).stscobert    := 'VAL';
  rB01_1(POSICION).codmonedacob := P.CodMoneda;
  rB01_1(POSICION).codmoneda    := P.CodMoneda;
  rB01_1(POSICION).fecinivalid  := R.FecIniValid;
  rB01_1(POSICION).fecfinvalid  := R.FecFinValid;
  
  PR_ANEXO_CERT.ANEXO_X_COBERTURA(p_nIdePol, p_nNumCert, P.CodProd, R.CodPlan, R.RevPlan,
      p_cCodRamoCert, rB01_1(POSICION).codcobert, R.FecIniValid, R.FecFinValid);

    -- FR_TARIFA_MULT
    CPP := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod(P.CodProd,R.CodPlan,R.RevPlan,p_cCodRamoCert,rB01_1(POSICION).codcobert);                  
    IF CPP.ParamPreCobert IS NOT NULL THEN
        rB01_1(POSICION).sumaasegmonedacob := NVL(PR_PRE_COBERT.PRE_SUMA(p_nIdePol,p_nNumCert,P.CodProd,R.CodPlan,
                                                            R.RevPlan,p_cCodRamoCert,rB01_1(POSICION).codcobert,
                                                            CPP.ParamPreCobert,NULL),0);
        rB01_1(POSICION).tasa := NVL(PR_PRE_COBERT.PRE_TASA(p_nIdePol,p_nNumCert,P.CodProd,R.CodPlan,
                                                R.RevPlan,p_cCodRamoCert,rB01_1(POSICION).codcobert,
                                                CPP.ParamPreCobert,NULL),0);
        rB01_1(POSICION).primamonedacob := NVL(PR_PRE_COBERT.PRE_PRIMA(p_nIdePol,p_nNumCert,P.CodProd,R.CodPlan,
                                                            R.RevPlan,p_cCodRamoCert,rB01_1(POSICION).CodCobert,
                                                            CPP.ParamPreCobert,NULL),0);        
    END IF;
    
    IF CPP.SumaAsegMax = CPP.SumaAsegMin AND CPP.SumaAsegMax != 0 THEN
       rB01_1(POSICION).sumaasegmonedacob := CPP.SumaAsegMin;
    END IF;

    IF CPP.TasaMax = CPP.TasaMin AND CPP.TasaMax != 0 THEN
        rB01_1(POSICION).tasa := CPP.TasaMin;
    END IF;

    IF CPP.PrimaMax = CPP.PrimaMin AND CPP.PrimaMax != 0 THEN
        rB01_1(POSICION).primamonedacob := CPP.PrimaMin;
    END IF; -- mtodeducible
    
    IF rB01_1(POSICION).sumaasegmonedacob > 0 AND rB01_1(POSICION).tasa > 0 THEN
        nTipTasa := PR.BUSCA_TIPO_TASA_PROD(P.CodProd,p_cCodRamoCert,R.CodPlan,R.RevPlan,rB01_1(POSICION).codcobert); 
        rB01_1(POSICION).primamonedacob := rB01_1(POSICION).sumaasegmonedacob * rB01_1(POSICION).Tasa / nTipTasa;
    END IF;

  nTasaCambio    := PR.TASA_CAMBIO(RB01_1(POSICION).codmonedacob,PR_VINISUSC.CODIGO_MONEDA,SYSDATE,'D');
  nTasaCambioPol := PR.TASA_CAMBIO(RB01_1(POSICION).codmonedacob,P.codmoneda,SYSDATE,'D');

  RB01_1(POSICION).prima          := RB01_1(POSICION).primamonedacob * nTasaCambio;
  RB01_1(POSICION).sumaaseg       := RB01_1(POSICION).sumaasegmonedacob * nTasaCambio;
  RB01_1(POSICION).sumaasegmoneda := RB01_1(POSICION).sumaasegmonedacob * nTasaCambioPol;
  RB01_1(POSICION).primamoneda    := RB01_1(POSICION).primamonedacob * nTasaCambioPol;
    
    INSERT INTO COBERT_CERT(
        tasa, -- 1
        prima, -- 2
        indmod, -- 3
        indsumaded, -- 4 
        idecobert, -- 5
        idepol, -- 6
        numcert, -- 7
        codramocert, -- 8 
        codplan, -- 9
        revplan, -- 10
        codcobert, -- 11
        stscobert, -- 12
        sumaasegmoneda, -- 13 
        primamoneda, -- 14
        mtototrecamoneda, -- 15 
        mtototdctomoneda, -- 16
        codmoneda, -- 17
        sumaaseg, -- 18
        mtototreca, -- 19
        mtototdcto, -- 20
        fecinivalid, -- 21
        fecfinvalid, -- 22
        indincren, -- 23
        indplazoespera, -- 24 
        feciniplazoespera, -- 25 
        fecfinplazoespera, -- 26
        fecexc, -- 27
        codmotvexc, -- 28 
        text1motvexc, -- 29
        text2motvexc, -- 30
        text3motvexc, -- 31
        text4motvexc, -- 32
        porcded, -- 33
        mtodedmin, -- 34
        baseded, -- 35
        porcdesc, -- 36
        mtodeducible, -- 37 
        mtodedmax, -- 38
        numdiaiden, -- 39
        numdiatemp, -- 40
        indpriper, -- 41
        indcobesp, -- 42
        mtodedmincob, -- 43
        mtodedmaxcob, -- 44
        mtodeduciblecob, -- 45 
        mtodedminlocal, -- 46
        mtodedmaxlocal, -- 47
        mtodeduciblelocal, -- 48 
        numordvis, -- 49
        codmonedacob, -- 50 
        sumaasegmonedacob, -- 51 
        primamonedacob, --  52
        mtototrecamonedacob, -- 53 
        mtototdctomonedacob, -- 54
        indcobpoliza -- 55
    )VALUES (
        NVL(rB01_1(POSICION).tasa,0), -- 1
        NVL(rB01_1(POSICION).prima,0), -- 2
        NVL(rB01_1(POSICION).indmod,'*'), -- 3
        rB01_1(POSICION).indsumaded, -- 4 
        rB01_1(POSICION).idecobert, -- 5
        rB01_1(POSICION).idepol, -- 6
        rB01_1(POSICION).numcert, -- 7
        rB01_1(POSICION).codramocert, -- 8 
        rB01_1(POSICION).codplan, -- 9
        rB01_1(POSICION).revplan, -- 10
        rB01_1(POSICION).codcobert, -- 11
        rB01_1(POSICION).stscobert, -- 12
        NVL(rB01_1(POSICION).sumaasegmoneda,0), -- 13 
        NVL(rB01_1(POSICION).primamoneda,0), -- 14
        rB01_1(POSICION).mtototrecamoneda, -- 15 
        rB01_1(POSICION).mtototdctomoneda, -- 16
        rB01_1(POSICION).codmoneda, -- 17
        NVL(rB01_1(POSICION).sumaaseg,0), -- 18
        NVL(rB01_1(POSICION).mtototreca,0), -- 19
        NVL(rB01_1(POSICION).mtototdcto,0), -- 20
        rB01_1(POSICION).fecinivalid, -- 21
        rB01_1(POSICION).fecfinvalid, -- 22
        rB01_1(POSICION).indincren, -- 23
        rB01_1(POSICION).indplazoespera, -- 24 
        rB01_1(POSICION).feciniplazoespera, -- 25 
        rB01_1(POSICION).fecfinplazoespera, -- 26
        rB01_1(POSICION).fecexc, -- 27
        rB01_1(POSICION).codmotvexc, -- 28 
        rB01_1(POSICION).text1motvexc, -- 29
        rB01_1(POSICION).text2motvexc, -- 30
        rB01_1(POSICION).text3motvexc, -- 31
        rB01_1(POSICION).text4motvexc, -- 32
        rB01_1(POSICION).porcded, -- 33
        rB01_1(POSICION).mtodedmin, -- 34
        rB01_1(POSICION).baseded, -- 35
        rB01_1(POSICION).porcdesc, -- 36
        NVL(rB01_1(POSICION).mtodeducible, 0), -- 37 indpriper    
        rB01_1(POSICION).mtodedmax, -- 38
        rB01_1(POSICION).numdiaiden, -- 39
        rB01_1(POSICION).numdiatemp, -- 40
        NVL(rB01_1(POSICION).indpriper, 'N'), -- 41
        rB01_1(POSICION).indcobesp, -- 42
        rB01_1(POSICION).mtodedmincob, -- 43
        rB01_1(POSICION).mtodedmaxcob, -- 44
        rB01_1(POSICION).mtodeduciblecob, -- 45 
        rB01_1(POSICION).mtodedminlocal, -- 46
        rB01_1(POSICION).mtodedmaxlocal, -- 47
        rB01_1(POSICION).mtodeduciblelocal, -- 48 
        rB01_1(POSICION).numordvis, -- 49
        rB01_1(POSICION).codmonedacob, -- 50 
        NVL(rB01_1(POSICION).sumaasegmonedacob,0), -- 51 
        NVL(rB01_1(POSICION).primamonedacob,0), --  52
        rB01_1(POSICION).mtototrecamonedacob, -- 53 
        rB01_1(POSICION).mtototdctomonedacob, -- 54
        rB01_1(POSICION).indcobpoliza -- 55
    );
    
    BEGIN
         PR_DED_COBERT.CARGA_DEDUCIBLES(p_nIdePol,p_nNumCert,p_cCodRamoCert,
                                        rB01_1(POSICION).codcobert,CPP.RevPlan,CPP.CodPlan,
                                        CPP.CodProd,rB01_1(POSICION).idecobert);
    EXCEPTION
         WHEN OTHERS THEN            
          p_cError := '1,'||SQLERRM;
          DBMS_OUTPUT.PUT_LINE('1,'||SQLERRM);
          RAISE_APPLICATION_ERROR(-20101,SQLERRM);
    END;

END SP_MNT_COBERT_CERT_C;
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
  p_cTipo        IN CHAR(1),
  p_cError       OUT VARCHAR2(3000)
) IS
  cIndAsoc               NUMBER(1);
  nRowsClau              NUMBER(1) := 0;  
  cMensajeRestriccionSAS VARCHAR2(400);

    CURSOR C_CLAUCERT IS
        SELECT * FROM CLAU_CERT CC
        WHERE CC.IDEPOL    = p_nIdePol
        AND CC.NUMCERT     = p_nNumCert
        AND CC.CODRAMOCERT = p_cCodRamoCert
        AND CC.CODCLAU IN('TRA382','TRA385','TRA386','TRA257');

    CLC CLAU_CERT%ROWTYPE;

    P   POLIZA%ROWTYPE;
    R   CERT_RAMO%ROWTYPE;

BEGIN
    p_cError := '0,OK';
    -- CLAU_CERT --> CLC
   P := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||P.IdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||P.IdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||P.IdePol);
    END IF;

    R := PR_CERT_RAMO.Datos_Cert_Ramo(p_nIdePol,p_nNumCert,p_cCodRamoCert);
    IF R.CodRamoCert IS NULL THEN
       p_cError := '1,'||'No existe el Ramo :'||p_cCodRamoCert;
       DBMS_OUTPUT.PUT_LINE('No existe el Ramo :'||p_cCodRamoCert);
       RAISE_APPLICATION_ERROR(-20100,'No existe el Ramo '||p_cCodRamoCert);
    END IF;

    cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(P.IdePol);
    if not cMensajeRestriccionSAS is null then
            p_cError := '1,'||cMensajeRestriccionSAS;
            DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
            RAISE_APPLICATION_ERROR(-20100,cMensajeRestriccionSAS);  
    end if;
   
   OPEN C_CLAUCERT;
   FETCH C_CLAUCERT INTO CLC;
   WHILE C_CLAUCERT%FOUND LOOP
     IF p_cTipo = 'P' THEN
        IF CLC.CODCLAU IN('TRA382','TRA385','TRA386') THEN

            IF CLC.INDPROCLA IN ('U','I') THEN              
                    BEGIN
                        SELECT 1 
                        INTO  nRowsClau
                        FROM  CLAU_CERT CC
                        WHERE CC.Idepol  = CLC.idepol
                            AND CC.NumCert = CLC.numcert
                            AND CC.CodClau = CLC.codclau
                            AND CC.CodClau = PR_CLAU_CERT.CLAUREP_VALIDO       
                            AND CC.NumVers = PR_CLAU_CERT.VERSION_CLAUSULA(CLC.idepol, PR_CLAU_CERT.CLAUREP_VALIDO, CLC.numcert)
                            AND ROWNUM = 1;
                    EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                            nRowsClau := 0;
                    END;
                    
                    IF nRowsClau = 1 THEN     
                        INSERT INTO OPER_CLAU_GARANTIA
                        (IdePol, NumCert, CodRamoCert, CodClau, NumOper, Fecha, CodUsr)
                        VALUES (CLC.IdePol, CLC.NumCert, CLC.CodRamoCert, CLC.CodClau, 0, SYSDATE, USER);
                    END IF;

                    DELETE FROM CLAU_CERT CC
                    WHERE CC.IDEPOL = p_nIdePol
                    AND CC.NUMCERT  = p_nNumCert
                    AND CC.CODCLAU  =  CLC.CODCLAU
                    AND CC.CODRAMOCERT = p_cCodRamoCert;

            ELSE
                    IF CLC.INDPROCLA = 'O' THEN
                    -- MESSAGE('No se puede Eliminar, es una Cláusula Obligatoria');
                        p_cError := '1,'||'No se puede Eliminar, es una Cláusula Obligatoria';
                        DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Obligatoria');
                        RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Obligatoria');
                    ELSIF CLC.indprocla = 'C' THEN
                        BEGIN
                            SELECT 1
                            INTO cIndAsoc 
                            FROM CLAU_COBERT CC
                            WHERE CodProd   = P.codprod
                                AND CodPlan = R.codplan
                                AND RevPlan = R.revplan
                                AND CodClau = CLC.codclau
                                AND CodRamo = CLC.codramocert
                                AND EXISTS (SELECT 1 FROM COBERT_CERT
                                            WHERE IdePol        = CLC.IDEPOL
                                                AND NumCert     = CLC.NUMCERT
                                                AND CodRamoCert = CLC.CODRAMOCERT  
                                                AND CodCobert   = CC.CODCOBERT);
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                cIndAsoc := 0;
                        END;

                        IF cIndAsoc = 1 THEN
                            --    MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Cobertura';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                        ELSE
                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        END IF;

                    ELSIF CLC.INDPROCLA = 'E' THEN
                        --  MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Estadística';
                        DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                    
                    ELSIF CLC.INDPROCLA = 'D' THEN
                        
                        BEGIN
                            SELECT 1
                                INTO cIndAsoc           
                                FROM CLAU_VALCRIT  CV
                                WHERE CodProd   = P.codprod
                                AND CodPlan     = R.codplan
                                AND RevPlan     = R.revplan
                                AND CodRamoPlan = CLC.CodRamoCert
                                AND CodClau     = CLC.CodClau
                                AND EXISTS (SELECT 1 FROM DATOS_PARTICULARES
                                            WHERE IdePol      = CLC.IdePol
                                                AND NumCert     = CLC.NumCert
                                                AND CodRamoCert = CLC.CodRamoCert
                                                AND CodCrit     = CV.CodCrit
                                                AND CodTrasf    = CV.CodTrasf);   
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                cIndAsoc := 0;
                        END;       

                        IF cIndAsoc = 1 THEN
                            -- MESSAGE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            p_cError := '1,'||'No se puede Excluir, es una Cláusula Asociada a un Dato Particular';
                            DBMS_OUTPUT.PUT_LINE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                        ELSE
                                IF CLC.STSCLAU = 'ACT' THEN
                                    PR_CLAU_CERT.STATUS_CLAU_CERT(CLC.IdePol,CLC.NumCert,CLC.CodRamoCert,
                                                                CLC.CodClau,'EXCLU');    
                                    -- COMMIT;
                                END IF;
                        END IF;

                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;
                    
                    ELSIF CLC.STSCLAU = 'VAL' THEN
                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;
                    END IF;
            END IF;
            
        END IF;
           
     ELSIF p_cTipo = 'R' THEN
        IF CLC.CODCLAU IN('TRA382','TRA257') THEN

                IF CLC.INDPROCLA IN ('U','I') THEN              
                        BEGIN
                            SELECT 1 
                            INTO  nRowsClau
                            FROM  CLAU_CERT CC
                            WHERE CC.Idepol  = CLC.idepol
                                AND CC.NumCert = CLC.numcert
                                AND CC.CodClau = CLC.codclau
                                AND CC.CodClau = PR_CLAU_CERT.CLAUREP_VALIDO       
                                AND CC.NumVers = PR_CLAU_CERT.VERSION_CLAUSULA(CLC.idepol, PR_CLAU_CERT.CLAUREP_VALIDO, CLC.numcert)
                                AND ROWNUM = 1;
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                nRowsClau := 0;
                        END;
                        
                        IF nRowsClau = 1 THEN     
                            INSERT INTO OPER_CLAU_GARANTIA
                            (IdePol, NumCert, CodRamoCert, CodClau, NumOper, Fecha, CodUsr)
                            VALUES (CLC.IdePol, CLC.NumCert, CLC.CodRamoCert, CLC.CodClau, 0, SYSDATE, USER);
                        END IF;

                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;

                ELSE
                        IF CLC.INDPROCLA = 'O' THEN
                        -- MESSAGE('No se puede Eliminar, es una Cláusula Obligatoria');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Obligatoria';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Obligatoria');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Obligatoria');
                        ELSIF CLC.indprocla = 'C' THEN
                            BEGIN
                                SELECT 1
                                INTO cIndAsoc 
                                FROM CLAU_COBERT CC
                                WHERE CodProd   = P.codprod
                                    AND CodPlan = R.codplan
                                    AND RevPlan = R.revplan
                                    AND CodClau = CLC.codclau
                                    AND CodRamo = CLC.codramocert
                                    AND EXISTS (SELECT 1 FROM COBERT_CERT
                                                WHERE IdePol        = CLC.IDEPOL
                                                    AND NumCert     = CLC.NUMCERT
                                                    AND CodRamoCert = CLC.CODRAMOCERT  
                                                    AND CodCobert   = CC.CODCOBERT);
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    cIndAsoc := 0;
                            END;

                            IF cIndAsoc = 1 THEN
                                --    MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                                p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Cobertura';
                                DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                                RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            ELSE
                                DELETE FROM CLAU_CERT CC
                                WHERE CC.IDEPOL = p_nIdePol
                                AND CC.NUMCERT  = p_nNumCert
                                AND CC.CODCLAU  =  CLC.CODCLAU
                                AND CC.CODRAMOCERT = p_cCodRamoCert;
                            END IF;

                        ELSIF CLC.INDPROCLA = 'E' THEN
                            --  MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Estadística';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        
                        ELSIF CLC.INDPROCLA = 'D' THEN
                            
                            BEGIN
                                SELECT 1
                                    INTO cIndAsoc           
                                    FROM CLAU_VALCRIT  CV
                                    WHERE CodProd   = P.codprod
                                    AND CodPlan     = R.codplan
                                    AND RevPlan     = R.revplan
                                    AND CodRamoPlan = CLC.CodRamoCert
                                    AND CodClau     = CLC.CodClau
                                    AND EXISTS (SELECT 1 FROM DATOS_PARTICULARES
                                                WHERE IdePol      = CLC.IdePol
                                                    AND NumCert     = CLC.NumCert
                                                    AND CodRamoCert = CLC.CodRamoCert
                                                    AND CodCrit     = CV.CodCrit
                                                    AND CodTrasf    = CV.CodTrasf);   
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    cIndAsoc := 0;
                            END;       

                            IF cIndAsoc = 1 THEN
                                -- MESSAGE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                                p_cError := '1,'||'No se puede Excluir, es una Cláusula Asociada a un Dato Particular';
                                DBMS_OUTPUT.PUT_LINE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                                RAISE_APPLICATION_ERROR(-20100,'No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            ELSE
                                    IF CLC.STSCLAU = 'ACT' THEN
                                        PR_CLAU_CERT.STATUS_CLAU_CERT(CLC.IdePol,CLC.NumCert,CLC.CodRamoCert,
                                                                    CLC.CodClau,'EXCLU');    
                                        -- COMMIT;
                                    END IF;
                            END IF;

                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        
                        ELSIF CLC.STSCLAU = 'VAL' THEN
                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        END IF;
                END IF;
            
        END IF;   
     END IF;
     FETCH C_CLAUCERT INTO CLC; 
   END LOOP;
   CLOSE C_CLAUCERT;

END SP_ELIMINAR_CLAU_CERT_C;
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
  p_cTipo        IN CHAR(1),
  p_cError       OUT VARCHAR2(3000)
) IS
  cIndAsoc               NUMBER(1);
  nRowsClau              NUMBER(1) := 0;  
  cMensajeRestriccionSAS VARCHAR2(400);

    CURSOR C_CLAUCERT IS
        SELECT * FROM CLAU_CERT CC
        WHERE CC.IDEPOL    = p_nIdePol
        AND CC.NUMCERT     = p_nNumCert
        AND CC.CODRAMOCERT = p_cCodRamoCert
        AND CC.CODCLAU IN('TRA382','TRA385','TRA386','TRA257');

    CLC CLAU_CERT%ROWTYPE;

    P   POLIZA%ROWTYPE;
    R   CERT_RAMO%ROWTYPE;

BEGIN
    p_cError := '0,OK';
    -- CLAU_CERT --> CLC
   P := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||P.IdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||P.IdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||P.IdePol);
    END IF;

    R := PR_CERT_RAMO.Datos_Cert_Ramo(p_nIdePol,p_nNumCert,p_cCodRamoCert);
    IF R.CodRamoCert IS NULL THEN
       p_cError := '1,'||'No existe el Ramo :'||p_cCodRamoCert;
       DBMS_OUTPUT.PUT_LINE('No existe el Ramo :'||p_cCodRamoCert);
       RAISE_APPLICATION_ERROR(-20100,'No existe el Ramo '||p_cCodRamoCert);
    END IF;

    cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(P.IdePol);
    if not cMensajeRestriccionSAS is null then
            p_cError := '1,'||cMensajeRestriccionSAS;
            DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
            RAISE_APPLICATION_ERROR(-20100,cMensajeRestriccionSAS);  
    end if;
   
   OPEN C_CLAUCERT;
   FETCH C_CLAUCERT INTO CLC;
   WHILE C_CLAUCERT%FOUND LOOP
     IF p_cTipo = 'P' THEN
        IF CLC.CODCLAU IN('TRA382','TRA385','TRA386') THEN

            IF CLC.INDPROCLA IN ('U','I') THEN              
                    BEGIN
                        SELECT 1 
                        INTO  nRowsClau
                        FROM  CLAU_CERT CC
                        WHERE CC.Idepol  = CLC.idepol
                            AND CC.NumCert = CLC.numcert
                            AND CC.CodClau = CLC.codclau
                            AND CC.CodClau = PR_CLAU_CERT.CLAUREP_VALIDO       
                            AND CC.NumVers = PR_CLAU_CERT.VERSION_CLAUSULA(CLC.idepol, PR_CLAU_CERT.CLAUREP_VALIDO, CLC.numcert)
                            AND ROWNUM = 1;
                    EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                            nRowsClau := 0;
                    END;
                    
                    IF nRowsClau = 1 THEN     
                        INSERT INTO OPER_CLAU_GARANTIA
                        (IdePol, NumCert, CodRamoCert, CodClau, NumOper, Fecha, CodUsr)
                        VALUES (CLC.IdePol, CLC.NumCert, CLC.CodRamoCert, CLC.CodClau, 0, SYSDATE, USER);
                    END IF;

                    DELETE FROM CLAU_CERT CC
                    WHERE CC.IDEPOL = p_nIdePol
                    AND CC.NUMCERT  = p_nNumCert
                    AND CC.CODCLAU  =  CLC.CODCLAU
                    AND CC.CODRAMOCERT = p_cCodRamoCert;

            ELSE
                    IF CLC.INDPROCLA = 'O' THEN
                    -- MESSAGE('No se puede Eliminar, es una Cláusula Obligatoria');
                        p_cError := '1,'||'No se puede Eliminar, es una Cláusula Obligatoria';
                        DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Obligatoria');
                        RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Obligatoria');
                    ELSIF CLC.indprocla = 'C' THEN
                        BEGIN
                            SELECT 1
                            INTO cIndAsoc 
                            FROM CLAU_COBERT CC
                            WHERE CodProd   = P.codprod
                                AND CodPlan = R.codplan
                                AND RevPlan = R.revplan
                                AND CodClau = CLC.codclau
                                AND CodRamo = CLC.codramocert
                                AND EXISTS (SELECT 1 FROM COBERT_CERT
                                            WHERE IdePol        = CLC.IDEPOL
                                                AND NumCert     = CLC.NUMCERT
                                                AND CodRamoCert = CLC.CODRAMOCERT  
                                                AND CodCobert   = CC.CODCOBERT);
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                cIndAsoc := 0;
                        END;

                        IF cIndAsoc = 1 THEN
                            --    MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Cobertura';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                        ELSE
                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        END IF;

                    ELSIF CLC.INDPROCLA = 'E' THEN
                        --  MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Estadística';
                        DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                    
                    ELSIF CLC.INDPROCLA = 'D' THEN
                        
                        BEGIN
                            SELECT 1
                                INTO cIndAsoc           
                                FROM CLAU_VALCRIT  CV
                                WHERE CodProd   = P.codprod
                                AND CodPlan     = R.codplan
                                AND RevPlan     = R.revplan
                                AND CodRamoPlan = CLC.CodRamoCert
                                AND CodClau     = CLC.CodClau
                                AND EXISTS (SELECT 1 FROM DATOS_PARTICULARES
                                            WHERE IdePol      = CLC.IdePol
                                                AND NumCert     = CLC.NumCert
                                                AND CodRamoCert = CLC.CodRamoCert
                                                AND CodCrit     = CV.CodCrit
                                                AND CodTrasf    = CV.CodTrasf);   
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                cIndAsoc := 0;
                        END;       

                        IF cIndAsoc = 1 THEN
                            -- MESSAGE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            p_cError := '1,'||'No se puede Excluir, es una Cláusula Asociada a un Dato Particular';
                            DBMS_OUTPUT.PUT_LINE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                        ELSE
                                IF CLC.STSCLAU = 'ACT' THEN
                                    PR_CLAU_CERT.STATUS_CLAU_CERT(CLC.IdePol,CLC.NumCert,CLC.CodRamoCert,
                                                                CLC.CodClau,'EXCLU');    
                                    -- COMMIT;
                                END IF;
                        END IF;

                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;
                    
                    ELSIF CLC.STSCLAU = 'VAL' THEN
                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;
                    END IF;
            END IF;
            
        END IF;
           
     ELSIF p_cTipo = 'R' THEN
        IF CLC.CODCLAU IN('TRA382','TRA257') THEN

                IF CLC.INDPROCLA IN ('U','I') THEN              
                        BEGIN
                            SELECT 1 
                            INTO  nRowsClau
                            FROM  CLAU_CERT CC
                            WHERE CC.Idepol  = CLC.idepol
                                AND CC.NumCert = CLC.numcert
                                AND CC.CodClau = CLC.codclau
                                AND CC.CodClau = PR_CLAU_CERT.CLAUREP_VALIDO       
                                AND CC.NumVers = PR_CLAU_CERT.VERSION_CLAUSULA(CLC.idepol, PR_CLAU_CERT.CLAUREP_VALIDO, CLC.numcert)
                                AND ROWNUM = 1;
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                nRowsClau := 0;
                        END;
                        
                        IF nRowsClau = 1 THEN     
                            INSERT INTO OPER_CLAU_GARANTIA
                            (IdePol, NumCert, CodRamoCert, CodClau, NumOper, Fecha, CodUsr)
                            VALUES (CLC.IdePol, CLC.NumCert, CLC.CodRamoCert, CLC.CodClau, 0, SYSDATE, USER);
                        END IF;

                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;

                ELSE
                        IF CLC.INDPROCLA = 'O' THEN
                        -- MESSAGE('No se puede Eliminar, es una Cláusula Obligatoria');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Obligatoria';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Obligatoria');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Obligatoria');
                        ELSIF CLC.indprocla = 'C' THEN
                            BEGIN
                                SELECT 1
                                INTO cIndAsoc 
                                FROM CLAU_COBERT CC
                                WHERE CodProd   = P.codprod
                                    AND CodPlan = R.codplan
                                    AND RevPlan = R.revplan
                                    AND CodClau = CLC.codclau
                                    AND CodRamo = CLC.codramocert
                                    AND EXISTS (SELECT 1 FROM COBERT_CERT
                                                WHERE IdePol        = CLC.IDEPOL
                                                    AND NumCert     = CLC.NUMCERT
                                                    AND CodRamoCert = CLC.CODRAMOCERT  
                                                    AND CodCobert   = CC.CODCOBERT);
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    cIndAsoc := 0;
                            END;

                            IF cIndAsoc = 1 THEN
                                --    MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                                p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Cobertura';
                                DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                                RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            ELSE
                                DELETE FROM CLAU_CERT CC
                                WHERE CC.IDEPOL = p_nIdePol
                                AND CC.NUMCERT  = p_nNumCert
                                AND CC.CODCLAU  =  CLC.CODCLAU
                                AND CC.CODRAMOCERT = p_cCodRamoCert;
                            END IF;

                        ELSIF CLC.INDPROCLA = 'E' THEN
                            --  MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Estadística';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        
                        ELSIF CLC.INDPROCLA = 'D' THEN
                            
                            BEGIN
                                SELECT 1
                                    INTO cIndAsoc           
                                    FROM CLAU_VALCRIT  CV
                                    WHERE CodProd   = P.codprod
                                    AND CodPlan     = R.codplan
                                    AND RevPlan     = R.revplan
                                    AND CodRamoPlan = CLC.CodRamoCert
                                    AND CodClau     = CLC.CodClau
                                    AND EXISTS (SELECT 1 FROM DATOS_PARTICULARES
                                                WHERE IdePol      = CLC.IdePol
                                                    AND NumCert     = CLC.NumCert
                                                    AND CodRamoCert = CLC.CodRamoCert
                                                    AND CodCrit     = CV.CodCrit
                                                    AND CodTrasf    = CV.CodTrasf);   
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    cIndAsoc := 0;
                            END;       

                            IF cIndAsoc = 1 THEN
                                -- MESSAGE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                                p_cError := '1,'||'No se puede Excluir, es una Cláusula Asociada a un Dato Particular';
                                DBMS_OUTPUT.PUT_LINE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                                RAISE_APPLICATION_ERROR(-20100,'No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            ELSE
                                    IF CLC.STSCLAU = 'ACT' THEN
                                        PR_CLAU_CERT.STATUS_CLAU_CERT(CLC.IdePol,CLC.NumCert,CLC.CodRamoCert,
                                                                    CLC.CodClau,'EXCLU');    
                                        -- COMMIT;
                                    END IF;
                            END IF;

                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        
                        ELSIF CLC.STSCLAU = 'VAL' THEN
                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        END IF;
                END IF;
            
        END IF;   
     END IF;
     FETCH C_CLAUCERT INTO CLC; 
   END LOOP;
   CLOSE C_CLAUCERT;

END SP_ELIMINAR_CLAU_CERT_C;
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
PROCEDURE SP_VALIDA_COBERT_PROV(p_nIdePol     IN POLIZA.Idepol%TYPE,
                                p_cMsjCobProv OUT VARCHAR2 ) IS

  cNumPlaca  VARCHAR2(40)  := NULL;
  cNumSerie  VARCHAR2(100) := NULL;
  cNumMotor  VARCHAR2(100) := NULL;
  nSinPlaca  NUMBER:= 0;
  cMsjError  VARCHAR2(1000) := NULL;
  
  CURSOR C_Certificado IS
    SELECT CE.Numcert
      FROM CERTIFICADO CE
     WHERE CE.IDEPOL = p_nIdePol
       AND EXISTS (SELECT 1
                       FROM LVAL LV
                       WHERE LV.Tipolval = 'ESACPOCP'
                        AND LV.Codlval LIKE 'CDP%' 
                        AND LV.DESCRIP = CE.Stscert);

BEGIN
  FOR rC IN C_Certificado LOOP
    BEGIN  
            SELECT DISTINCT DP.Valtrasf
            INTO cNumSerie
            FROM POLIZA PO, 
                CERTIFICADO CE, 
                CERT_RAMO CR, 
                DATOS_PARTICULARES DP
            WHERE PO.IdePol      = p_nIdePol
            AND PO.IdePol      = CE.IdePol
            AND CE.IdePol      = CR.IdePol
            AND CE.NumCert     = CR.NumCert
            AND CR.IdePol      = DP.IdePol
            AND CR.NumCert     = DP.NumCert
            AND CR.CodRamoCert = DP.CodRamoCert
            AND CE.Numcert     = rC.Numcert
            AND DP.CodCrit     = 'NUMERO_DE_SERIE'
                AND EXISTS (SELECT 1
                            FROM LVAL LV
                            WHERE LV.Tipolval = 'ESACPOCP'
                            AND LV.Codlval LIKE 'CDP%' 
                            AND LV.DESCRIP = CE.Stscert
                            )
            AND EXISTS (SELECT 1
                        FROM LVAL LV
                        WHERE LV.Tipolval = 'PROVEHCP'
                        AND LV.Codlval = PO.Codprod);

    EXCEPTION
      WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Serie para el certificado.');
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Serie para el certificado.');
        WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Número de Serie: '||SQLCODE||' - '||SQLERRM);
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Número de Serie: '||SQLCODE||' - '||SQLERRM);
      END;
    BEGIN  
        SELECT DISTINCT DP.Valtrasf
        INTO cNumMotor
        FROM POLIZA PO, 
             CERTIFICADO CE, 
             CERT_RAMO CR, 
             DATOS_PARTICULARES DP
       WHERE PO.IdePol      = p_nIdePol
         AND PO.IdePol      = CE.IdePol
         AND CE.IdePol      = CR.IdePol
         AND CE.NumCert     = CR.NumCert
         AND CR.IdePol      = DP.IdePol
         AND CR.NumCert     = DP.NumCert
         AND CR.CodRamoCert = DP.CodRamoCert
         AND CE.Numcert     = rC.Numcert
         AND EXISTS (SELECT 1
                         FROM LVAL LV
                         WHERE LV.Tipolval = 'ESACPOCP'
                         AND LV.Codlval LIKE 'CDP%' 
                         AND LV.DESCRIP = CE.Stscert)
         AND DP.CodCrit     = 'NUMERO_DE_MOTOR'
         AND EXISTS (SELECT 1
                     FROM LVAL LV
                     WHERE LV.Tipolval = 'PROVEHCP'
                     AND LV.Codlval = PO.Codprod);
    
    EXCEPTION
      WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Motor para el certificado.');
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Motor para el certificado.');
        WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Número de Motor: '||SQLCODE||' - '||SQLERRM);
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Número de Motor: '||SQLCODE||' - '||SQLERRM);

    END;
   
    -- Placa
    BEGIN  
      SELECT DISTINCT DP.Valtrasf
        INTO cNumPlaca
        FROM POLIZA PO, 
             CERTIFICADO CE, 
             CERT_RAMO CR, 
             DATOS_PARTICULARES DP
       WHERE PO.IdePol      = p_nIdePol
         AND PO.IdePol      = CE.IdePol
         AND CE.IdePol      = CR.IdePol
         AND CE.NumCert     = CR.NumCert
         AND CR.IdePol      = DP.IdePol
         AND CR.NumCert     = DP.NumCert
         AND CR.CodRamoCert = DP.CodRamoCert
         AND CE.Numcert     = rC.Numcert
         AND EXISTS (
                      SELECT 1
                       FROM LVAL LV
                      WHERE LV.Tipolval = 'ESACPOCP'
                        AND LV.Codlval LIKE 'CDP%' 
                        AND LV.DESCRIP = CE.Stscert
                        )
         AND DP.CodCrit = 'PLACA_DE_RODAJE'
         AND EXISTS (SELECT 1
                       FROM LVAL LV
                      WHERE LV.Tipolval = 'PROVEHCP'
                        AND LV.Codlval = PO.Codprod);

    EXCEPTION
      WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Se tiene más de una Placa de Rodaje para el certificado.');
              RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Se tiene más de una Placa de Rodaje para el certificado.');
      WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Placa de Rodaje: '||SQLCODE||' - '||SQLERRM);
              RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Placa de Rodaje: '||SQLCODE||' - '||SQLERRM);
    END;

    BEGIN
      PR_CARGA_MASIVA_DATPAR.SP_VALIDA_COBERT_PROV(cNumSerie,
                                                   cNumMotor,
                                                   cNumPlaca,
                                                   cMsjError);
    EXCEPTION
      WHEN OTHERS THEN
        cMsjError := 'Error en Valida Cobertura - '||SQLERRM;
    END;
    IF cMsjError IS NOT NULL THEN
      p_cMsjCobProv  := cMsjError;
    ELSE
      p_cMsjCobProv  := NULL;
    END IF;
    EXIT WHEN p_cMsjCobProv IS NOT NULL;
  END LOOP;
END PR_VALIDA_COBERT_PROV;
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
FUNCTION FN_VALIDA_TREC(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2 IS
  --
  CURSOR c_Data IS 
    SELECT DISTINCT NumCert
    FROM CERTIFICADO
    WHERE IdePol = p_nIdePol;
  --
  nNumCertC NUMBER := 0;
  nNumCertM NUMBER := 0;
  cValTrasf DATOS_PARTICULARES.ValTrasf%TYPE;
  nMtoMaqui MAQUINARIA_TREC.MtoMaqui%TYPE;
  nValTrasf NUMBER := 0;
  cValCas   VARCHAR2(1) := '0';
  
BEGIN
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
  IF NVL(nNumCertC,0) = NVL(nNumCertM,0) AND NVL(nNumCertM,0) > 0 AND NVL(nNumCertC,0) > 0 THEN
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
      IF cValCas = '0' THEN
        IF NVL(nMtoMaqui,0) = NVL(nValTrasf,0) AND NVL(nValTrasf,0) > 0  AND NVL(nMtoMaqui,0) > 0 THEN
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

END FN_VALIDA_TREC;
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
PROCEDURE SP_VALIDAR_DATOS(p_nIdePol IN POLIZA.Idepol%TYPE) IS

  kHoy       CONSTANT DATE := TRUNC(SYSDATE);
  cError     VARCHAR2(300) := NULL;
  nExiste    PLS_INTEGER;
  bValida    BOOLEAN;

  PZ   POLIZA%ROWTYPE;
BEGIN
  -- Se valida si las fechas equivalentes han sido actualizadas.
  -- :C00.dFecOper := PR.Fecha_Movimiento(kHoy);
    IF PR.Fecha_Movimiento(kHoy) IS NULL THEN
            DBMS_OUTPUT.PUT_LINE('Favor actualizar la tabla de fechas de equivalencias');
            RAISE_APPLICATION_ERROR(-20100,'Favor actualizar la tabla de fechas de equivalencias');
    END IF; 
  --Verificar si se valida datos particulares
  --asumo que no
  bValida := FALSE;

    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;

  --se validaran las nuevas
  IF (PZ.StsPol IN ('VAL','INC','ANU')) THEN 
    bValida := TRUE; 
  ELSE
    nExiste := 0 ; 
    BEGIN --1.verifico si hay inclusiones
      SELECT 1
      INTO nExiste
      FROM DUAL 
      WHERE EXISTS (SELECT 1
                    FROM Certificado
                    WHERE IdePol = PZ.IDEPOL
                      AND StsCert IN ('INC','MOD') 
                      AND FecExc IS NULL);
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        nExiste := 0 ;
      WHEN OTHERS THEN 
         -- LR_Error(SQLERRM);
               DBMS_OUTPUT.PUT_LINE(SQLERRM);
               RAISE_APPLICATION_ERROR(-20100,SQLERRM);
    END ; --1.
    IF nExiste = 1 THEN
      bValida := TRUE ; 
    END IF ;         
  END IF ;
  IF bValida = TRUE THEN
    cError := PR_POLIZA.ValidaDatoParticular(PZ.IDEPOL);
  END IF;
  
  IF cError IS NOT NULL THEN 
        DBMS_OUTPUT.PUT_LINE(cError);
        RAISE_APPLICATION_ERROR(-20100,cError);
  END IF;

END SP_VALIDAR_DATOS;
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
                           p_cParam3 VARCHAR2) RETURN VARCHAR2 IS                                   
  cMensaje VARCHAR2 (500);                          
BEGIN                                                  
  --
  BEGIN                                                
    SELECT Mensaje                                     
    INTO   cMensaje                                    
    FROM   msg_acsel
    WHERE  TipoMsg = p_cTipo                          
      AND  CodMsg  = p_nCodigo;                          
  EXCEPTION                                            
    WHEN NO_DATA_FOUND THEN                            
      IF p_cParam1 IS NOT NULL THEN                       
        cMensaje := p_cParam1;                            
      ELSE                                             
        cMensaje := 'Mensaje desconocido ';            
      END IF;                                          
  END;                                                 
  --
  IF p_cParam1 != '  ' THEN                               
    cMensaje := RTRIM(cMensaje);                                
    cMensaje := REPLACE(cMensaje,'&1',p_cParam1);                  
  ELSE                                                          
    cMensaje := REPLACE(cMensaje,'&1','');                      
  END IF;                                                       
  --
  IF p_cParam2 != '  ' THEN                               
    cMensaje := RTRIM(cMensaje);                                
    cMensaje := REPLACE(cMensaje,'&2',p_cParam2);                  
  ELSE                                                          
    cMensaje := REPLACE(cMensaje,'&2','');                      
  END IF;                                                       
  --
  IF p_cParam3 != '  ' THEN                                        
    cMensaje := RTRIM(cMensaje);                               
    cMensaje := REPLACE(cMensaje,'&3',p_cParam3);                  
  ELSE                                                          
    cMensaje := REPLACE(cMensaje,'&3','');                      
  END IF;                                                       
  --
  cMensaje := p_cTipo||'-'||TO_CHAR(p_nCodigo)||': '||RTRIM(cMensaje);         
  RETURN (cMensaje); 

END FN_MENSAJE_ACSEL;
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
PROCEDURE SP_ACTIVA_GARANTIAS (p_stspol POLIZA.StsPol%TYPE, p_nIdePol IN Poliza.IdePol%TYPE) IS
  nCantReg   NUMBER := 0;
  v_NumOper  Oper_Pol.NumOper%TYPE; 
  nStsPol  POLIZA.StsPol%TYPE;  
  nStsPol_1  POLIZA.StsPol%TYPE;
  PZ         POLIZA%ROWTYPE;
BEGIN 
  
  nStsPol:=p_stspol;

    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN            
        DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
        RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;
  nStsPol_1 := PZ.STSPOL;
    
  -- Si la poliza esta en estados 'VAL','MOD','INC' permite activar las garantias
  IF nStsPol IN ('VAL','MOD','INC') AND nStsPol_1 = 'ACT'  THEN     
    
    -- Valida si Producto es de INSPAT
    IF PR.BUSCA_LVAL('SLIP_CER', PZ.CodPol) != 'INVALIDO' THEN      
    BEGIN  
      -- CUANDO LA POLIZA ESTA EN EMISION
        SELECT COUNT(1) 
          INTO nCantReg
          FROM DIREC_GARANTIAS
         WHERE IdePol = PZ.IdePol
           AND STSGARANTIA  = 'VAL'
           and nStsPol = 'VAL';
      EXCEPTION WHEN OTHERS THEN
        nCantReg := 0;
      END; 
        
      IF(nCantReg=0) then 
      BEGIN  
         
        -- CUANDO LA POLIZA YA EXISTE
            SELECT COUNT(1) 
              INTO nCantReg
              FROM DIREC_GARANTIAS
             WHERE IdePol = PZ.IdePol
               AND STSGARANTIA  IN ('VAL', 'INC')
               and nStsPol IN ('INC','MOD');
          EXCEPTION WHEN OTHERS THEN
            nCantReg := 0;
          END;        
     end if;
   
      -- Si existen Garantias para la Poliza
      IF nCantReg > 0 THEN
      BEGIN 
          -- SE OBTIENE EL NUMERO DE OPERACION
          SELECT max(NumOper) 
            INTO v_NumOper
            FROM Oper_Pol
           WHERE IdePol = PZ.IdePol
             AND NVL(INDANUL,'N') = 'N';
        EXCEPTION WHEN OTHERS THEN
          v_NumOper := NULL;
        END;
   
        -- SE ACTUALIZA EL NUMERO DE OPERACION EN TODAS LAS GARANTIAS
        IF v_NumOper IS NOT NULL THEN
        BEGIN
           
            IF nStsPol = 'VAL'  THEN  
                
              UPDATE DIREC_GARANTIAS
                 SET NumOper = v_NumOper,
                     STSGARANTIA = 'ACT'
               WHERE IdePol = PZ.IdePol
                 AND STSGARANTIA IN ('VAL' ,'INC')
                 and nStsPol = 'VAL';
            ELSE   
              UPDATE DIREC_GARANTIAS
              SET NumOper = v_NumOper,STSGARANTIA = 'ACT'
               WHERE IdePol = PZ.IdePol
                 AND STSGARANTIA IN ('VAL' ,'INC')
                 and nStsPol IN ('MOD', 'INC');                
            END IF;  
            -- STANDARD.COMMIT;
            
          EXCEPTION WHEN OTHERS THEN
            NULL;
          END;
        END IF;
      END IF;
    END IF; 
  END IF;     
END SP_ACTIVA_GARANTIAS;
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
PROCEDURE SP_AUTORIZA_RESTRIC_EMI_POLIZA(
  p_nIdePol      IN Poliza.IdePol%TYPE
) IS

  cUsuario     USUARIO_AUTORIZA.CodUsu%TYPE;
  n_Alerta     NUMBER;
  c_Existe_Usuario NUMBER(6) := 0;

    CURSOR C_AUTOVIALA IS
        SELECT * 
        FROM AUTORIZA_VIOLACION
        WHERE IDEPOL = p_nIdePol 
        AND STSAUTORIZADO = 'VAL';

    AUVIO AUTORIZA_VIOLACION%ROWTYPE;

    B01_Autoriza CHAR(1);

BEGIN

   OPEN C_AUTOVIALA;
   FETCH C_AUTOVIALA INTO AUVIO;
   WHILE C_AUTOVIALA%FOUND LOOP
        BEGIN
            SELECT DISTINCT CodUsu
            INTO   cUsuario 
            FROM   USUARIO_AUTORIZA 
            WHERE  CodUsu = USER
            AND    CodAut   IN (SELECT CODAUT
                                FROM AUTORIZADOR
                                WHERE CODBLOQ = AUVIO.CODBLOQ) 
            AND    StsAut   = 'ACT';
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
                BEGIN   
                    SELECT USER
                    INTO cUsuario
                    FROM DUAL
                    WHERE EXISTS (SELECT 1
                                    FROM USUARIO_AUTORIZA 
                                    WHERE CodUsu = USER 
                                    AND  CodAut = AUVIO.CODAUT) ;
                EXCEPTION
                    WHEN OTHERS THEN
                cUsuario := 'INVALIDO';
        END ;
      WHEN OTHERS THEN
            /* 
            n_Alerta := ALERTA('Fallo SELECT a la tabla USUARIO_AUTORIZA'||chr(13)||' Error '||SQLERRM);
            cUsuario := 'INVALIDO';
        :B01.Autoriza := 'N';
            RAISE FORM_TRIGGER_FAILURE;
            */
            cUsuario     := 'INVALIDO';
            B01_Autoriza := 'N';
            DBMS_OUTPUT.PUT_LINE('Fallo SELECT a la tabla USUARIO_AUTORIZA'||chr(13)||' Error '||SQLERRM);
    END;
        IF USER = cUsuario THEN
            B01_Autoriza := 'S';
        ELSE
            -- n_Alerta :=ALERTA('Usuario No Existe..ó Inactivo Para Autorizar La Política...Verifique..!!!');
            DBMS_OUTPUT.PUT_LINE('Usuario No Existe..ó Inactivo Para Autorizar La Política...Verifique..!!!');
            B01_Autoriza := 'N';
            /*
            BEGIN
                SELECT Count(1)
                INTO c_Existe_Usuario
                FROM USUARIO_AUTORIZA 
                WHERE CodAut IN (SELECT CODAUT
                                FROM AUTORIZADOR
                                WHERE CODBLOQ= AUVIO.CODBLOQ) 
                AND StsAut    = 'ACT';
            END;
            IF c_Existe_Usuario > 0 THEN
                    GO_BLOCK('B03');
                    LAST_RECORD;
            END IF;
            */
        END IF;

        IF p_nIdePol IS NOT NULL AND B01_Autoriza = 'S' THEN
        BEGIN
                -- FR_AUTORIZA_VIOLACION;
                -- CONFIRMAR_RECALCULO (AUVIO.IDEAUT ,AUVIO.IDEPOL); .:. VALIDAR .:.
                PR_EVALUA_POLITICA.AUTORIZA_BLOQUEO (AUVIO.IDEAUT ,AUVIO.IDEPOL ,USER);
                DBMS_OUTPUT.PUT_LINE('Violaciones Autorizadas...!!!');
                -- FIN
                -- FR_UPDATE;
                DECLARE
                    nExiste_Violacion  NUMBER(1) :=0 ;
                BEGIN
                        BEGIN
                            SELECT 1
                            INTO nExiste_Violacion
                            FROM AUTORIZA_VIOLACION
                            WHERE IdePol = AUVIO.IDEPOL
                            AND   StsAutorizado = 'VAL';
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN 
                                nExiste_Violacion  := 0;
                            --Se agrega lo siguiente.
                            WHEN TOO_MANY_ROWS THEN       
                                    nExiste_Violacion  := 1;  
                            WHEN OTHERS THEN 
                                    nExiste_Violacion  := 1;
                        END;
                        --Fin de codigo agregado por Jsanchez.
                        --Ojo que al poner el estado a 2 , se indica que la poliza
                        --no tiene violacion y por tanto se actualizara.
                        IF nExiste_Violacion = 0 THEN 
                                UPDATE POLIZA 
                                SET    IndMovPolRen = '2' 
                                WHERE  IdePol       = AUVIO.IDEPOL;
                        END IF;          
                        
                END;
                -- FIN
            EXCEPTION 
                WHEN OTHERS THEN 
                  DBMS_OUTPUT.PUT_LINE('Error'||SQLERRM);
                 RAISE_APPLICATION_ERROR(-20100,'Error'||SQLERRM);
            END;
        ELSE
            DBMS_OUTPUT.PUT_LINE('No Existen Registros Seleccionados Para Autorizar...!!!');
            RAISE_APPLICATION_ERROR(-20100,'No Existen Registros Seleccionados Para Autorizar...!!!');
        END IF;

      FETCH C_AUTOVIALA INTO AUVIO;
   END LOOP;
   CLOSE C_CLAUCERT;

END SP_AUTORIZA_RESTRIC_EMI_POLIZA;
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
                                ) RETURN NUMBER IS
  nExiste NUMBER(1) := 0;
BEGIN

  SELECT DISTINCT D.P1 INTO nExiste 
  FROM (
    SELECT 1 AS P1
    FROM TEMP_DIST_REA DR
    WHERE DR.IDEREC = p_nIdeRec
    AND DR.CODRAMOREA = p_cCodRamoRea
    AND DR.IDEPOL = p_nIdePol
    AND DR.NUMCERT = p_nNumCert
    AND DR.CODRAMOCERT = p_cCodRamoCert
    AND dr.NumModDistRea = 0 
    AND dr.ideCttoRea = 99999999999999
    AND EXISTS (
        SELECT 1
        FROM TEMP_DIST_FACULT df
        WHERE df.IdeRec = dr.IdeRec
          AND df.CodRamoRea = dr.CodRamoRea
          AND df.IdePol = dr.IdePol
          AND df.NumCert = dr.NumCert
          AND df.CodRamoCert = dr.CodRamoCert
          AND df.NumModDistRea =0
          AND df.ideCttoRea = 99999999999999)
   UNION ALL
   SELECT 1 AS P1
    FROM DIST_REA DR
    WHERE DR.IDEREC = p_nIdeRecAnt
    AND DR.CODRAMOREA = p_cCodRamoRea
    AND DR.IDEPOL = p_nIdePol
    AND DR.NUMCERT = p_nNumCert
    AND DR.CODRAMOCERT = p_cCodRamoCert
    AND dr.NumModDistRea = 0 
    AND dr.ideCttoRea = 99999999999999
    AND EXISTS (
      SELECT 1
      FROM DIST_FACULT df
      WHERE df.IdeRec = dr.IdeRec
        AND df.CodRamoRea = dr.CodRamoRea
        AND df.IdePol = dr.IdePol
        AND df.NumCert = dr.NumCert
        AND df.CodRamoCert= dr.CodRamoCert
        AND df.NumModDistRea =0
        AND df.ideCttoRea = 99999999999999)
  ) D;
  
  RETURN (nExiste);
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN (0);
  WHEN TOO_MANY_ROWS THEN
    RETURN (1);

END FN_IND_FACULTATIVO;
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
  PROCEDURE SP_VALIDA_RECALCULO_RM(p_nIdePol IN Poliza.IdePol%TYPE)
    IS 
        nIdePol           POLIZA.IdePol%TYPE;
        nFormPagoInv      NUMBER := 0;
        nExisteEmision    NUMBER := 0;
        cIndAplica        VARCHAR2(1);
        cIndFacultativo   VARCHAR2(1);
        cIndRedist        VARCHAR2(1);
        nProdInvalido     NUMBER := 0;
        cCodFormPago      VARCHAR2(1);
        nIdeRecAnt        RECIBO.IDEREC%TYPE;
        nNumOper        oper_pol_t.numoper%type;
        nExisteProd       NUMBER(4);
        kNO               VARCHAR2(1):='N';
        kSI               VARCHAR2(1):='S';
        cAvanzaValidacion VARCHAR2(1) := 'S';
        nExisteAutRedist  NUMBER:= 0;
        nExisteRedistApt  NUMBER:= 0;
        nCantRegRec       NUMBER:= 0;
        
        CURSOR c_Recibo (p_nNumOper oper_pol_t.numoper%type) IS
                SELECT R.IdePol, R.NumCert, R.IdeRec, R.CodRamoCert, 
                    TRUNC(R.FecIniVig) FecIniVig, TRUNC(R.FecFinVig) FecFinVig, 
                    GR.CodRamoRea, GR.SumaAseg
                FROM   recibo_t R,  temp_gen_rea GR
                WHERE  R.IdeRec  =  GR.IdeRec
                AND  R.IdeOp   =  p_nNumOper
                ORDER BY R.IdeRec;

        CURSOR c_DatosPoliza (p_nIdePol POLIZA.IdePol%TYPE) IS
            SELECT DISTINCT P.CodFormPago, PR.BUSCA_LVAL('LISTRANS',P.CodProd) DESCPROD
            FROM   POLIZA P
            WHERE  P.IdePol = p_nIdePol; 
            
  BEGIN
        
    nIdePol := p_nIdePol; --:C01.nIdePol;
    
    --VALIDA SI SE ESTA REALIZANDO UN ENDOSO
    select count(*)  
    INTO nExisteAutRedist
    from poliza 
    where idepol = nIdePol
    and stspol = 'MOD'; 
    
    cAvanzaValidacion := kSI;
    
    IF nExisteAutRedist = 0 THEN
      cAvanzaValidacion := kNO;
    ELSE
      --VALIDA REGISTRADOS VS COMPLETOS
      SELECT COUNT(*) INTO nExisteRedistApt
        FROM AUTORIZA_REDIST
      WHERE IDEPOL = nIdePol
      AND NUMOPER IS NULL
      AND INDREDIST IS NOT NULL;
      
      SELECT COUNT(*) INTO nCantRegRec
          FROM AUTORIZA_REDIST
      WHERE IDEPOL = nIdePol
      AND NUMOPER IS NULL;
      
      IF nExisteRedistApt > 0 AND nCantRegRec > 0 THEN
        IF nExisteRedistApt = nCantRegRec THEN
          cAvanzaValidacion := kNO;
        END IF;
      END IF;
    END IF;
    
    IF cAvanzaValidacion = kSI THEN
      
      FOR dp IN c_DatosPoliza(nIdePol)  LOOP 
                cCodFormPago := dp.CodFormPago;
                
                IF cCodFormPago NOT IN('A', 'E')  THEN
                    nFormPagoInv := nFormPagoInv + 1;
                END IF;
                    
                IF dp.DESCPROD <> 'INVALIDO' THEN
                    nProdInvalido := nProdInvalido + 1;
                END IF; 
                
            END LOOP;
      
      --VALIDACION PRODUCTO DE TRANSPORTE FLOTANTE
      IF nProdInvalido = 0 THEN     
        
        --VALIDACION FORMA DE PAGO INVALIDA
        IF nFormPagoInv = 0 THEN
          BEGIN
            select max(numoper) 
              into nNumOper 
              from oper_pol_t 
             where idepol = nIdePol;
          EXCEPTION 
            WHEN OTHERS THEN
              nNumOper := 0;
          END;
    
          FOR XX  IN c_Recibo(nNumOper) LOOP
            
            BEGIN
              SELECT MAX(IdeRec)
              INTO   nIdeRecAnt
              FROM   RECIBO R
              WHERE  R.IdePol   = XX.IdePol
                AND  R.NumCert  = XX.NumCert
                AND  R.CodRamoCert = XX.CodRamoCert
                AND  R.IdeRec   <> XX.IdeRec
                AND  R.IdeOp    < nNumOper
                AND  StsRec     <> 'REV'
                AND PR_GEN_REA_UTIL_RM.EVALUA_TRASLAPE_FECHAS(TRUNC(R.FecIniVig),TRUNC(R.FecFinVig),XX.FecIniVig,XX.FecFinVig) = 'S'
                AND NOT EXISTS(SELECT OP.NumOper
                               FROM   OPER_POL OP
                               WHERE  OP.IdePol  = R.IdePol
                                 AND  OP.NumOper = R.IdeOp
                                 AND  OP.TipoOp  = 'ESV');
            EXCEPTION
              WHEN OTHERS THEN
                nIdeRecAnt := 0;
            END;
            --
            
            --SI TIENE UN IDERECANT ES UN ENDOSO
            IF nIdeRecAnt <> 0 THEN
              
              --EVALUAMOS EL ESQUEMA ANTERIOR Y EL ESQUEMA NUEVO
              cIndAplica := PR_GEN_REA_UTIL_RM.EVALUA_ESQUEMAS_REASEG(XX.IdePol, XX.NumCert, XX.CodRamoCert,XX.CodRamoRea,nIdeRecAnt, XX.IdeRec);
              
              --EVALUAMOS SI ES FACULTATIVO PARA COLOCAR EL INDREDIST COMO NULO O CON UN VALOR 'S'
              --<I RTC-103795> Brayan Pereda 17-08-2017 Modificacion de la funcion para obtener el indicador facultativo
              --IF fr_Existe_Facultativo (XX.IdePol, XX.NumCert) = 1 THEN 
              IF PR_API_COMISION_INTERMEDIARIO.FN_IND_FACULTATIVO(XX.IdePol, XX.NumCert, XX.CodRamoCert, XX.CodRamoRea, nIdeRecAnt, XX.IdeRec) = 1 THEN
              --<F RTC-103795>
                cIndFacultativo := kSI;
                cIndRedist := NULL;
              ELSE
                cIndFacultativo := kNO;
                cIndRedist := kSI;
              END IF;
              
              DELETE FROM AUTORIZA_REDIST
              WHERE IDEPOL = XX.IdePol
              AND ATRIBUTO1 = XX.NumCert
              AND ATRIBUTO2 = XX.CodRamoCert
              AND NUMOPER IS NULL;
                
              IF cIndAplica = kSI THEN 
                INSERT INTO AUTORIZA_REDIST(IDEPOL,NUMOPER,INDAPLIC,INDREDIST,ATRIBUTO1,ATRIBUTO2,ATRIBUTO3)
            VALUES(XX.IdePol,NULL,cIndAplica,cIndRedist,XX.NumCert,XX.CodRamoCert,cIndFacultativo); 
              END IF;             
              --
            END IF;
            --
          END LOOP;
          --  
        END IF;
        --
      END IF;
      --  
    END IF;
        
  END SP_VALIDA_RECALCULO_RM;
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
                                  ) RETURN VARCHAR2 IS                   
                               
    g_P             POLIZA%ROWTYPE;
    g_R             RECIBO%ROWTYPE;
    g_C             CERTIFICADO%ROWTYPE;
    kSI             CONSTANT VARCHAR2(1) := 'S';
    kNO             CONSTANT VARCHAR2(1) := 'N';  
    kErrorPermitido CONSTANT NUMBER   := 0.1;
    kIdeCttoFPU     CONSTANT CTTO_REA.IDECTTOREA%TYPE := 99999999999999;                                 
                               
    nIdeRecGen          RECIBO.IDEREC%TYPE;
    nIdeCttoReaFacul    NUMBER;
    nDiasAno            NUMBER(4):= 365;
    nDias               NUMBER(4);
    nPrimaDistRea1      DIST_REA.PRIMADISTREA%TYPE;
    nNumModDistRea      DIST_REA.NUMMODDISTREA%TYPE;
    nPrimaDistRea       DIST_REA.PRIMADISTREA%TYPE;
    nNumMaxMod          DIST_REA.NUMMODDISTREA%TYPE;
    nIdeRecFacul        DIST_REA.IDEREC%TYPE;
    dFecIniVigMov       DATE;
    n_FactorFecha       NUMBER(12,9);
    nPorcFacult         NUMBER;
    nPrimaDistReaFacult DIST_FACULT.PRIMACEDIDA%TYPE;
    GR                  GEN_REA%ROWTYPE;
    DRM                 DIST_REA_MOD%ROWTYPE;
    DR                  DIST_REA%ROWTYPE;
    DF                  DIST_FACULT%ROWTYPE;

    nImpIgv             NUMBER(20,2);   
    nImpRem             NUMBER(20,2);
    
    cDivision    VARCHAR2(5);
    cIndDivRecal VARCHAR2(2);    
     
    CURSOR c_Gen_Rea IS
      SELECT IdePol, NumCert, CodRamoCert, CodRamoRea, 
             ABS(SumaAseg) SumaAseg, PrimaPro, PrimaAjus, PrimaMov, 
             FecIniVig, FecFinVig, IdeRec
      FROM   GEN_REA
      WHERE  IdeRec = (SELECT MAX(IdeRec)
                       FROM  RECIBO R
                       WHERE R.IdePol = p_nIdePol
                         AND R.NumCert = p_nNumCert
                         AND R.CodRamoCert = p_cCodRamoCert
                         AND R.IdeRec < p_nIdeRec
                         AND R.StsRec <> 'REV'
                         AND NOT EXISTS(SELECT OP.NumOper
                                        FROM   OPER_POL OP
                                        WHERE  OP.IdePol  = R.IdePol
                                         AND   OP.NumOper = R.IdeOp
                                         AND   OP.TipoOp  = 'ESV'))
        AND IdePol = p_nIdePol
        AND NumCert = p_nNumCert
        AND CodRamoCert = p_cCodRamoCert
        AND CodRamoRea = p_cCodRamoRea
        AND FecFinVig <= p_dFecFinVig
        AND FecAnul IS NULL
        AND PrimaPro <> 0;
    --         
    CURSOR c_Dist_Rea_Mod IS
      SELECT NumModDistRea
      FROM   DIST_REA_MOD
       WHERE IdePol = p_nIdePol
         AND NumCert = p_nNumCert
         AND CodRamoCert = p_cCodRamoCert
         AND CodRamoRea = p_cCodRamoRea
         AND NumModDistRea = 0
         AND IdeRec = nIdeRecGen;
    --         
    CURSOR c_Gen_Rea_Nuevo IS
      SELECT IdePol, NumCert, CodRamoCert, CodRamoRea, 
             ABS(SumaAseg) SumaAseg, PrimaPro, PrimaAjus, PrimaMov, 
             FecIniVig, FecFinVig, IdeRec, PorcCoa
       FROM  GEN_REA
       WHERE IdeRec = (SELECT MAX(IdeRec)
                       FROM   RECIBO R
                       WHERE  R.IdePol    =   p_nIdePol
                         AND  R.NumCert   =   p_nNumCert
                         AND  R.CodRamoCert = p_cCodRamoCert
                         AND  R.IdeRec    <   p_nIdeRec
                         AND  R.StsRec    <>  'REV'
                         AND NOT EXISTS(SELECT OP.NumOper
                                        FROM   OPER_POL OP
                                        WHERE  OP.IdePol  = R.IdePol
                                         AND   OP.NumOper = R.IdeOp
                                         AND   OP.TipoOp  = 'ESV'))
         AND IdePol = p_nIdePol
         AND NumCert = p_nNumCert
         AND CodRamoCert = p_cCodRamoCert
         AND CodRamoRea = p_cCodRamoRea
         AND FecFinVig <= p_dFecFinVig
         AND FecAnul IS NULL
         AND PrimaPro <> 0;
    --     

  BEGIN
    --
    g_P := PR_POLIZA.Carga_Poliza_Datos_Globales(p_nIdePol, g_P);
    g_R := PR_RECIBO.Carga_Recibo_Datos_Globales(p_nIdeRec, g_R);
    IF g_R.MtoLocal < 0  THEN
      dFecIniVigMov := p_dFecIniVig;
    ELSE
      dFecIniVigMov := g_P.FecIniVig;
    END IF;
    FOR yy IN c_Gen_Rea  LOOP
      nDiasAno := (yy.FecFinVig - yy.FecIniVig);
      nIdeRecGen := yy.IdeRec;
      nDias := (yy.FecFinVig - p_dFecIniVig);
      IF p_dFecIniVig = yy.FecIniVig   AND p_dFecFinVig = yy.FecFinVig THEN
        nDias := 0;
      END IF;
      FOR Z IN c_Dist_Rea_Mod LOOP
        --aplica nummoddistrea 1
        nNumModDistRea := NVL(PR_GEN_REA_UTIL.Max_Dist_Rea_Mod(p_nIdeRec, p_cCodRamoRea),0) + 1;
        DBMS_OUTPUT.PUT_LINE('Aplica reversión de Movimientos anteriores. nNumModDistRea = '||nNumModDistRea);
      END LOOP;
    END LOOP;
    FOR GR IN c_Gen_Rea_Nuevo  LOOP
      nDiasAno := (GR.FecFinVig - GR.FecIniVig);
      IF nDiasAno = 0  THEN
         nDiasAno := 1;
      END IF;
        --aplica nummoddistrea 2
        nNumModDistRea := nNumModDistRea + 1;
        DBMS_OUTPUT.PUT_LINE('Aplica distribución de Movimientos anteriores con nuevo esquema. nNumModDistRea = '||nNumModDistRea);
        --    
        BEGIN
          SELECT d.division INTO cDivision
            FROM (SELECT l.TipoLVal Division
                    FROM LVAL L
                   WHERE L.TipoLVal = 'TRAN'
                     AND CODLVAL = p_cCodRamoRea
                  UNION ALL
                  SELECT l.TipoLVal
                    FROM LVAL L
                   WHERE L.TipoLVal = 'RRGG'
                     AND CODLVAL = p_cCodRamoRea) d;
          
        EXCEPTION 
          WHEN OTHERS THEN
            cDivision := NULL;
        END;  
        
        SELECT DECODE(PR.BUSCA_LVAL('INREDIST', cDivision),'INVALIDO','NO',
                      PR.BUSCA_LVAL('INREDIST', cDivision)) 
          INTO cIndDivRecal FROM DUAL;            
                    
        RETURN cIndDivRecal;        
        --
    END LOOP;
    RETURN 'NO';
    --
END FN_APLICA_REDIST_FINAL;
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
  FUNCTION FN_CAL_APLICA_RECALCULO(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2 
  
  IS
                              
    cCodFormPago VARCHAR2(1);
    nIdeRecAnt   RECIBO.IDEREC%TYPE;
    nSumaAsegAnt GEN_REA.SUMAASEG%TYPE;
    GR           GEN_REA%ROWTYPE;
    
    cDivision  VARCHAR2(5);
    cIndDivRecal VARCHAR2(2);
    nnumoper   oper_pol_t.numoper%type;
    
    CURSOR c_Recibo (p_nNumOper oper_pol_t.numoper%type) IS
      SELECT R.IdePol, R.NumCert, R.IdeRec, R.CodRamoCert, 
             TRUNC(R.FecIniVig) FecIniVig, TRUNC(R.FecFinVig) FecFinVig, 
             GR.CodRamoRea, GR.SumaAseg
      FROM   recibo_t R,  temp_gen_rea GR
      WHERE  R.IdeRec  =  GR.IdeRec
        AND  R.IdeOp   =  p_nNumOper
      ORDER BY R.IdeRec;

    CURSOR c_FormaPago  IS
        SELECT DISTINCT P.CodFormPago
        FROM   POLIZA P
        WHERE  P.IdePol = p_nIdePol; 
  
  BEGIN
    -- Buscamos la forma de pago para determinar si se redistribuyen las polizas o no
    -- Solo se redistribuye las de forma A y E
    FOR cf IN c_FormaPago  LOOP
      cCodFormPago := cf.CodFormPago;
      IF cCodFormPago NOT IN('A', 'E')  THEN
        cCodFormPago := NULL;
        RETURN 'NO'; -- NO APLICA RECÁLCULO
      END IF;
    END LOOP;
    --
    IF cCodFormPago IN('A', 'E') THEN
      
    BEGIN
      select max(numoper) 
      into nNumOper 
      from oper_pol_t 
      where idepol = p_nIdePol;
    EXCEPTION 
      WHEN OTHERS THEN
        nNumOper := 0;
    END;
    
      FOR XX  IN c_Recibo(nNumOper) LOOP
        --
        BEGIN
          SELECT MAX(IdeRec)
          INTO   nIdeRecAnt
          FROM   RECIBO R
          WHERE  R.IdePol   = XX.IdePol
            AND  R.NumCert  = XX.NumCert
            AND  R.CodRamoCert = XX.CodRamoCert
            AND  R.IdeRec   <> XX.IdeRec
            AND  R.IdeOp    < nNumOper
            AND  StsRec     <> 'REV'
            AND NOT EXISTS(SELECT OP.NumOper
                           FROM   OPER_POL OP
                           WHERE  OP.IdePol  = R.IdePol
                             AND  OP.NumOper = R.IdeOp
                             AND  OP.TipoOp  = 'ESV');
        EXCEPTION
          WHEN OTHERS THEN
            nIdeRecAnt := 0;
        END;
        --
        IF nIdeRecAnt <> 0   THEN
          GR := PR_GEN_REA.Datos_Gen_Rea(XX.IdePol, XX.NumCert, XX.CodRamoCert,XX.CodRamoRea,nIdeRecAnt);
          nSumaAsegAnt:=GR.SumaAseg;
          IF gr.IDEPOL IS NULL THEN
            nSumaAsegAnt := 0;
          END IF;
          --
          IF ABS(nSumaAsegAnt) <> ABS(XX.SumaAseg)  THEN
            
       cIndDivRecal := PR_API_COMISION_INTERMEDIARIO.FN_APLICA_REDIST_FINAL(XX.IdePol, XX.NumCert, XX.CodRamoCert, XX.IdeRec, XX.FecIniVig, XX.FecFinVig, XX.CodRamoRea);
       IF cIndDivRecal = 'SI' AND fr_Existe_Facultativo (XX.IdePol, XX.NumCert) = 1 THEN
        RETURN cIndDivRecal;
       END IF;
            
          END IF;
          --
        END IF;
        --
      END LOOP; -- c_Recibo
      --
    END IF;   
    
    RETURN 'NO'; -- NO APLICA RECÁLCULO
        
 END FN_CAL_APLICA_RECALCULO;
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
PROCEDURE SP_VALIDA_RECALCULO(p_nIdePol IN Poliza.IdePol%TYPE) IS
  
    cAplicaRedist    VARCHAR2(2);
    nExisteAutRedist VARCHAR2(1) := NULL; 
    
  BEGIN
    --
      BEGIN
      cAplicaRedist := 'NO';
      DBMS_OUTPUT.PUT_LINE('Verificando si aplica recálculo ...');
      cAplicaRedist := PR_API_COMISION_INTERMEDIARIO.FN_CAL_APLICA_RECALCULO(p_nIdePol); -- FR_APLICA_RECALCULO(:C01.nIdePol);
    EXCEPTION
      WHEN OTHERS THEN
        cAplicaRedist := 'NO';
    END;
    --
    IF cAplicaRedist = 'SI' THEN

      DBMS_OUTPUT.PUT_LINE('Aplica recálculo');
      BEGIN
        SELECT NVL(COUNT(1),0)
        INTO nExisteAutRedist
          FROM AUTORIZA_REDIST
        WHERE IDEPOL = p_nIdePol
        AND NUMOPER IS NULL
        AND INDAPLIC = 'S';         
      EXCEPTION WHEN OTHERS THEN
        nExisteAutRedist := 0;
      END;  

      IF nExisteAutRedist = 0 THEN
        DBMS_OUTPUT.PUT_LINE('Registrando en AUTORIZA_REDIST');
        INSERT INTO AUTORIZA_REDIST (IDEPOL, NUMOPER, INDAPLIC, INDREDIST ) 
        VALUES (p_nIdePol, NULL, 'S', NULL);
      ELSE
        DBMS_OUTPUT.PUT_LINE('Ya existe un registro en AUTORIZA_REDIST');
      END IF;   

    ELSE                                                    

      DBMS_OUTPUT.PUT_LINE('Elimina registro de AUTORIZA_REDIST');
          DELETE AUTORIZA_REDIST
      WHERE IDEPOL = p_nIdePol -- :C01.nIdePol
      AND NUMOPER IS NULL
      AND INDAPLIC = 'S';

      DBMS_OUTPUT.PUT_LINE('No aplica recálculo');
  
    END IF;  

  END SP_VALIDA_RECALCULO;
--
/*-------------------------------------------------------------------------------
  Nombre      : PR_API_COMISION_INTERMEDIARIO.FN_OBT_CERT_RAMO_DIST_FACULT
  Proposito   : Obtener certificado y ramo en donde no se haya colocado detalle facultativo.
  Referencia : MODCOM.FMB
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
  Log de Cambios 
    Fecha             Autor          Descripción
  10/10/2023       Robinzon Santana          Creación
-------------------------------------------------------------------------------*/
FUNCTION FN_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol POLIZA.IdePol%TYPE) RETURN VARCHAR2 IS

    cMensaje VARCHAR2(1000):= '';

  CURSOR DATA_FACULT_INVALIDO IS
    SELECT DISTINCT 'Cert:'||DR.NUMCERT || '-RamoTec:' ||DR.CODRAMOCERT||';' AS MSJE 
    FROM TEMP_DIST_REA DR
    WHERE DR.IDEPOL = p_nIdePol
    AND dr.NumModDistRea = 0 
    AND dr.ideCttoRea = 99999999999999
    AND NOT EXISTS (
        SELECT 1
        FROM TEMP_DIST_FACULT df
        WHERE df.IdeRec = dr.IdeRec
          AND df.CodRamoRea = dr.CodRamoRea
          AND df.IdePol = dr.IdePol
          AND df.NumCert = dr.NumCert
          AND df.CodRamoCert = dr.CodRamoCert
          AND df.NumModDistRea =0
          AND df.ideCttoRea = 99999999999999);
   
BEGIN
  
  cMensaje:= '';
  
  FOR R IN DATA_FACULT_INVALIDO LOOP
    cMensaje := cMensaje || R.MSJE;
  END LOOP;
  RETURN cMensaje;
  
EXCEPTION
  WHEN OTHERS THEN
    RETURN '';
END FN_OBT_CERT_RAMO_DIST_FACULT;
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
PROCEDURE SP_ACTU_OPERA_MOT_ENDOSO(p_nIdePol IN Poliza.IdePol%TYPE) IS

  POL       POLIZA%ROWTYPE;
  RPST      POLIZA_STATUS%ROWTYPE;
  -- BTN ACTUALIZAR
  cValida VARCHAR2(1);
  --
  -- FR_ACTUALIZAR
  kSI               CONSTANT VARCHAR2(1) := '1' ; 
  kNO               CONSTANT VARCHAR2(1) := '0' ; 
  cPolitica         VARCHAR2(1);
  nIndicador        NUMBER(1);
  nValor            NUMBER;
  nEstadoValGeneral NUMBER := 0;
  --
  nExiste NUMBER(1) := 0;

BEGIN

    POL := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN

       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;

    IF PR_CNT_UTIL.POLIZA_CNF_PRIMAPACTADA(p_nIdePol) = 'S' THEN
      BEGIN
        PR_BBVA_UTIL.ProcesaPrimaPactada(p_nIdePol);
      EXCEPTION
        WHEN OTHERS THEN

          DBMS_OUTPUT.PUT_LINE('Error ' || SQLERRM);
          RAISE_APPLICATION_ERROR(-20100,'Error ' || SQLERRM);
      END;
    END IF;

    cExPSt := PR_POLIZA_MASIVA.Existe_PS(POL.IDEPOL);

  IF (cExPSt = 'N') THEN
    BEGIN
      SELECT DISTINCT OPS.Status
      INTO   cStatusP
      FROM   OPER_POL_STATUS OPS
      WHERE  OPS.IdePol  = POL.IDEPOL
        AND  OPS.NUMOPER = (SELECT MAX (OPS2.NUMOPER)
                              FROM OPER_POL_STATUS OPS2,OPER_POL OP
                              WHERE OPS2.IdePol = POL.IDEPOL 
                              AND NVL(OP.INDANUL,'N') = 'N'
                              AND OPS2.IdePol = OP.IdePol
                              AND OPS2.NumOper = OP.NumOper);
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        cStatusP := 0;
    END;
    RPST.IdePol := P.IdePol;
    RPST.StsPol := cStatusP;
    RPST.CodUsr := PR_SESION.USUARIO_APP;
    RPST.Fecha  := sysdate;
    IF RPST.StsPol != 0 THEN
      PR_POLIZA_MASIVA.Crear_Poliza_Status( RPST );
    END IF;
  END IF;

    -- cValida := FR_VALIDA_SERIE;
    -- FUNCTION FR_VALIDA_SERIE RETURN VARCHAR2 IS
    DECLARE

        cReturn  VARCHAR2(1) := 'N';
        nSerie   NUMBER := 2004;
        nCont    NUMBER := 0;
    
        CURSOR C_REA Is
            SELECT B.CODRAMOREA,A.IDECTTOREA,B.SERIE
            FROM TEMP_DIST_REA A,CTTO_REA B
            WHERE A.IDEPOL   = p_nIdePol  --:C01.nIdePol
            AND B.IDECTTOREA = A.IDECTTOREA;
    
    BEGIN
    
        -- IF :C01.FecIni = :C01.FecPro THEN '30-JUN-2004'
        IF P.FecIniVig = TO_DATE('30-JUN-2004','DD/MON/YYYY') THEN
            FOR R_REA IN C_REA LOOP
                IF PR.EXISTE_LVAL('RAMORENO',R_REA.CodRamoRea) = 'S' THEN
                    IF R_REA.Serie != nSerie THEN
                        nCont := nCont + 1;
                    END IF;
                END IF;
            END LOOP;
            
            IF nCont > 0 THEN
                cReturn := 'S';
            END IF;
            
        END IF;
        cValida := cReturn;
    END;
    /*
    IF cValida = 'S' THEN
        nDummy:= LR_ConfBox('No ha sido cambiada la fecha de contrato de reaseguro. Desea continuar con la activación?');
        IF nDummy = ALERT_BUTTON1 THEN
          FR_Actualizar;
        END IF;
    ELSE
      FR_Actualizar;
    END IF;
    */
    --
    IF PR.BUSCA_LVAL('CFREAMIX','VERSION') = '2' THEN
      --FR_VALIDA_RECALCULO_RM;
      PR_API_COMISION_INTERMEDIARIO.SP_VALIDA_RECALCULO_RM(p_nIdePol);
      PR_GEN_REA_UTIL_RM.VALIDAR_POLITICAS(p_nIdePol);
    ELSE
      -- FR_VALIDA_RECALCULO;
      PR_API_COMISION_INTERMEDIARIO.SP_VALIDA_RECALCULO(p_nIdePol);
    END IF;

    cPolitica := kNO;

    BEGIN
        BEGIN
          pr_valida_emision.inicio(p_nIdePol, nValor);
        END;

        IF nValor = 1 THEN
            --LR_Cursor_Ocupado('<<< EVALUACION DE POLITICAS DE SUBSCRIPCION INICIADA>>>');
            DBMS_OUTPUT.PUT_LINE('<<< EVALUACION DE POLITICAS DE SUBSCRIPCION INICIADA>>>');
            cPolitica := PR_Politicas.Principal(POL.IDEPOL,POL.NUMPOL, POL.CODPROD,USER,NVL(POL.CODPROD,'AC'));
        ELSIF nValor  != 1 THEN
            cPolitica := PR_EVALUA_POLITICA.EVALUA_VALOR_ENDOSADO(POL.IDEPOL,POL.NUMPOL, POL.CODPROD,USER);     
        END IF;

    EXCEPTION
      WHEN OTHERS THEN
        -- LR_Error('<<< ERROR EVALUANDO POLITICAS >>> '|| DBMS_ERROR_TEXT);
          DBMS_OUTPUT.PUT_LINE('<<< ERROR EVALUANDO POLITICAS >>> ');
          RAISE_APPLICATION_ERROR(-20100,'<<< ERROR EVALUANDO POLITICAS >>> ');
    END;
    --LR_Message('<<< EVALUACION DE POLITICAS DE SUBSCRIPCION CONCLUIDA>>>');
    DBMS_OUTPUT.PUT_LINE('<<<EVALUACION DE POLITICAS DE SUBSCRIPCION CONCLUIDA>>>');
    IF cPolitica = kSI THEN
          /*        
          IF :GLOBAL.PROCESO ='CVI' THEN
             DBMS_OUTPUT.PUT_LINE('CVI Existen Politicas Violadas ...Verifique...');
             RAISE_APPLICATION_ERROR(-20100,'CVI Existen Politicas Violadas ...Verifique...');
          ELSE
            -- LR_ERROR('Existen Politicas Violadas ...Verifique...');
            DBMS_OUTPUT.PUT_LINE('Existen Politicas Violadas ...Verifique...');
            SP_AUTORIZA_RESTRIC_EMI_POLIZA(p_nIdePol);
          END IF;
          */
          DBMS_OUTPUT.PUT_LINE('Existen Politicas Violadas ...Verifique...');
          PR_API_COMISION_INTERMEDIARIO.SP_AUTORIZA_RESTRIC_EMI_POLIZA(p_nIdePol);
    END IF;

  BEGIN
    nEstadoValGeneral := PR_GEN_REA_UTIL_RM.VALIDAR_FACULTATIVO(p_nIdePol); -- 1:OK, 0:HAY ERRORES
  EXCEPTION
    WHEN OTHERS THEN
      nEstadoValGeneral := 0;
      DBMS_OUTPUT.PUT_LINE('Error: ' 
                || DBMS_ERROR_TEXT
                || 'Para : '
                || PR_API_COMISION_INTERMEDIARIO.FN_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol)
                ||' <Informe al Area de Reaseguro>');
      RAISE_APPLICATION_ERROR(-20100,'Error: ' 
                || DBMS_ERROR_TEXT
                || 'Para : '
                || PR_API_COMISION_INTERMEDIARIO.FN_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol)
                ||' <Informe al Area de Reaseguro>');
      
  END;

  IF nEstadoValGeneral = 1 THEN
      DECLARE
         nNUMOPER OPER_POL_T.NUMOPER%TYPE;
      BEGIN
        BEGIN
           SELECT NUMOPER 
           INTO  nNUMOPER
           FROM OPER_POL_T 
           WHERE IdePol = p_nIdePol;
        EXCEPTION
          WHEN OTHERS THEN         
            DBMS_OUTPUT.PUT_LINE('Error'||SQLERRM);
            RAISE_APPLICATION_ERROR(-20100,'Error'||SQLERRM);
        END;
        PR_GEN_REA_T.Copia_Esq_Cert_Anul_T(nNUMOPER); --(:B01_1.NUMOPER);
        nIndicador := PR_GEN_REA_T.Valida_Facultativo(POL.IDEPOL);    
      EXCEPTION
        WHEN OTHERS THEN
         -- LR_Error('Error' || DBMS_ERROR_TEXT);
            DBMS_OUTPUT.PUT_LINE('Error'||SQLERRM);
            RAISE_APPLICATION_ERROR(-20100,'Error'||SQLERRM);
      END;    
  END IF;

END SP_ACTU_OPERA_MOT_ENDOSO;
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
 FUNCTION FN_OBTCONFREDIST ( p_nIdePol     IN    POLIZA.IdePol%TYPE ) RETURN VARCHAR2 IS  
    cConfRedist            VARCHAR2(1) := 'N';    
  BEGIN   
    SELECT NVL(MAX(INDREDIST),'N')
      INTO cConfRedist
      FROM AUTORIZA_REDIST
     WHERE IDEPOL = p_nIdePol 
       AND NUMOPER IS NULL
       AND INDAPLIC = 'S'
       AND INDREDIST IS NOT NULL;    
    RETURN cConfRedist;
       
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 'N';
      
  END FN_OBTCONFREDIST;
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
FUNCTION FN_EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE, , p_cTipoOper IN LVAL.CODLVAL%TYPE) RETURN OPER_POL.NumOper%TYPE 
IS

    kSI             CONSTANT VARCHAR2(1) := 'S';
    kNO             CONSTANT VARCHAR2(1) := 'N';     
    kActivar        CONSTANT VARCHAR2(2) := 'AC';
    nDummy      NUMBER;
    cFirma      VARCHAR2(1);
    nNumOper      OPER_POL.NumOper%TYPE;
    nExisteOper     VARCHAR2(1);
    cProcOper       REGLAS_STS.CodProceso%TYPE;  
    nNumOperendoso  OPER_POL.NumOper%TYPE;
    cIndAnul        OPER_POL.IndAnul%TYPE;

    cIndAct         VARCHAR2(1);

    P               POLIZA%ROWTYPE;
    nOper           NUMBER;
    nPlan           NUMBER(3);
    nMensaje        NUMBER;
    nMtoOper        OPER_POL.MtoOper%TYPE;
    cIndOperValida  VARCHAR2(1);

    cIndProc       VARCHAR2(1); 
    nNumOperAux    OPER_POL.NumOper%TYPE;

    pl_id         PARAMLIST;
    cStsPol       POLIZA.STSPOL%TYPE;   
    nIdImpresion  NUMBER;
    cCodProd      POLIZA.CODPROD%TYPE;
    nNumPol       POLIZA.NUMPOL%TYPE;
    nIndCNT       VARCHAR2(1);
    cTipoOper     OPER_POL.TIPOOP%TYPE;
    nNumFinanc    COND_FINANCIAMIENTO.NUMFINANC%TYPE;
    WSESION       NUMBER(20);

      nNumFact       FACTURA.NUMFACT%TYPE;
      cStsCert       CERTIFICADO.STSCERT%TYPE;
      nMtoFactMoneda FACTURA.MTOFACTMONEDA%TYPE;
      cCodMoneda     FACTURA.CODMONEDA%TYPE;

      rops           OPER_POL_STATUS%ROWTYPE;
      cstspolv       POLIZA_STATUS.STSPOL%TYPE;
    
    cConfRedist  VARCHAR2(1) := 'N';

      cMensajePol   varchar2(100);

       -- Generación de marcas automáticas en la exclusión de certificados desde SAS
      vcMarca         ACSELX.CONTADOR_ELIMINACION_LAS.CODNECESIDAD%TYPE;
    vnNumCert       ACSELX.CERTIFICADO.NUMCERT%TYPE;
    vdFecExc        ACSELX.CERTIFICADO.FECEXC%TYPE;
    vcMotExc        ACSELX.CERTIFICADO.CODMOTVEXC%TYPE;
      -- Generación de marcas automáticas en la exclusión de certificados desde SAS 
    --En esta tabla se guardan los certificados de trama de periodo corto
    --Se actualizará el numero de liquidacion que ha generado por la activación
  CURSOR cActualiza IS
    SELECT T.IDECOBERT, T.NUMMOD, T.NUMCERT, T.NUMPOL, T.CODPROD, T.IDEPOL, T.NUMCERTEXT, T.NUMCARGA, 
           T.NUMCORRDATA, T.MTOTRAMA, T.TIPSEG, T.FECCARGA, T.NUMFACT, T.MTOFACTMONEDA, T.CODMONEDA, T.STSLOG
    FROM   LOG_EXCLUYE_CERT T, CERTIFICADO C, POLIZA P
    WHERE  P.IDEPOL = p_nIdePol
      AND  P.STSPOL = 'ACT'
      AND  T.IDEPOL = P.IdePol
      AND  T.IDEPOL = C.IDEPOL
      AND  T.NUMCERT = C.NUMCERT
      AND  T.STSLOG = 'PEN'--ESTUVO PENDIENTE
      AND  C.STSCERT = 'ACT'--EL CERTIFICADO ESTA ACTIVO
      AND  T.TIPODOC = 'LQ'--  La tabla tiene LQ y LA
      AND  T.NUMFACT IS NULL;

  CURSOR cActu_DatPart (nNumOper OPER_POL.NumOper%TYPE) IS
      SELECT op.idepol, op.numcert, op.mtooper 
        FROM oper_pol op 
       WHERE numoper = nNumOper 
         AND tipoop = 'EMI';

  nAnualiz PRODUCTO_ENT_EXT_CARGA.Anualizacion%TYPE;


    -- Actualiza indicador de Facturado IndFact en la póliza.
    PROCEDURE Actualiza_Ind_Fact IS
    BEGIN
      UPDATE POLIZA
         SET IndFact = 'N'
       WHERE IdePol = p_nIdePol;     
    END Actualiza_Ind_Fact;   
    -- Devuelve indicador IndEsqReaPol de la póliza.
    FUNCTION TIENE_REASEGURO
      RETURN POLIZA.IndEsqReaPol%TYPE IS
      cIndCobPoliza  POLIZA.IndEsqReaPol%TYPE;
    BEGIN
      SELECT NVL(IndEsqReaPol,'N') IndCobPoliza
      INTO   cIndCobPoliza
      FROM   POLIZA
      WHERE  IdePol = p_nIdePol;   
      --
      RETURN cIndCobPoliza;
    END TIENE_REASEGURO;
    -- Elimina datos de rehabilitación de la póliza.
    PROCEDURE ELIMINAR_REHAB IS
    BEGIN
      DELETE POLIZA_REHAB 
      WHERE  IdePol = p_nIdePol;  
    END ELIMINAR_REHAB;

      ---------------------------------
      cAlcance VARCHAR2(8);
    PZ POLIZA%ROWTYPE;
BEGIN
      PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
      IF PZ.IDEPOL IS NULL THEN            
          DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
          RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
      END IF;
    -- PR_Obligatoriedad_correo('EMISION INDIVIDUAL POLIZA');
      PR_POL_CERT_ELECT.INICIAR('EMISION INDIVIDUAL POLIZA', cAlcance); --  Configuración Obligatoriedad del ingreso de correo electrónico
    IF TIENE_REASEGURO = kSI THEN
         -- Se invoca a los procedimientos de base de datos.
         SUMA_MAYOR_REA(p_nIdePol);
         PR_POLIZA.CERTIFICADO_CERO(p_nIdePol);
         -- STANDARD.COMMIT;
    END IF;

    IF PZ.STSPOL = 'VAL' THEN       
      -- LR_Cursor_Ocupado('Trabajando... Validando Poliza');
        DBMS_OUTPUT.PUT_LINE('Trabajando... Validando Poliza');
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Incluir: IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      PR_POLIZA.Incluir(p_nIdePol);
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Incluir: IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      -- STANDARD.COMMIT;
    END IF;
    
    
    -- LR_Cursor_Ocupado('Trabajando... Activando Poliza');
      DBMS_OUTPUT.PUT_LINE('Trabajando... Validando Poliza');
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(T): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
    
   
    nNumOper := PR_POLIZA.Activar(p_nIdePol,'T');
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(T): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
    -- STANDARD.COMMIT;
    -- LR_Cursor_Normal;

    -- Invocar formulario de PreActivación.
    PR_API_COMISION_INTERMEDIARIO.SP_ACTU_OPERA_MOT_ENDOSO(p_nIdePol);
    cIndAct := 'S';  -- .:. LR_Invocar_PreACT(kActivar,p_nIdePol,1,'N'); -- kActivar = 'AC'
    --
    LR_Cursor_Normal('Trabajando... Activando Poliza (Terminado)');

    IF cIndAct = kSI THEN
      nNumOper:=0;      
      LR_Cursor_Ocupado('Trabajando... Activando Poliza Definitiva - 1');
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(D): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      -- <I N3041386> Claudia Yugar Ladines / 14-08-2013 / CNT-01702: Manejo de re-cálculo de la distribución de reaseguros     
      cConfRedist := PR_API_COMISION_INTERMEDIARIO.FN_OBTCONFREDIST(p_nIdePol);-- FR_OBTCONFREDIST ( p_nIdePol );
      --<I RTC-313128> Luis Venancio / 24-05-2022 / Generación de marcas automáticas en la exclusión de certificados desde SAS
      BEGIN
        SELECT numcert,
               fecexc,
               codmotvexc
          INTO vnNumCert,
               vdFecExc,
               vcMotExc
          FROM certificado
         WHERE idepol = p_nIdePol
           AND stscert = 'MOD'
           AND ROWNUM = 1;
      EXCEPTION WHEN OTHERS THEN
                vnNumCert := NULL;
                vdFecExc  := NULL;
                vcMotExc  := NULL;
      END;
      
      IF PR.Busca_Lval('PROANULA', '313126SWITCH') = 'ON' THEN
         vcMarca := PR_MARCAS_ANULACION.FR_OBTENER_MARCA (p_nIdePol,TRUNC(vdFecExc),vcMotExc,vnNumCert,'CER');
      END IF;
      --<F RTC-313128> Luis Venancio / 24-05-2022 / Generación de marcas automáticas en la exclusión de certificados desde SAS
      nNumOper    := PR_POLIZA.Activar ( p_nIdePol, 'D', cConfRedist);
      --<I RTC-313128> Luis Venancio / 24-05-2022 / Generación de marcas automáticas en la exclusión de certificados desde SAS
      IF PR.Busca_Lval('PROANULA', '313126SWITCH') = 'ON' THEN
         IF vcMarca IS NOT NULL AND nNumOper IS NOT NULL THEN
            PR_MARCAS_ANULACION.PR_GRABAR_MARCA(p_nIdePol,nNumOper,vnNumCert,'CER',vcMarca);
         END IF;
      END IF;
      --<F RTC-313128> Luis Venancio / 24-05-2022 / Generación de marcas automáticas en la exclusión de certificados desde SAS
      --nNumOper := PR_POLIZA.Activar(p_nIdePol,'D');
      -- <F N3041386>     
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(D): IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal            
      --<I N3026404> Nelson Díaz 27.11.2006 EMI-02491
      BEGIN
        SELECT 'S' 
        INTO   cIndOperValida
        FROM   dual
        WHERE EXISTS ( SELECT 1
                       FROM   Oper_Pol op
                       WHERE  op.Idepol  = p_nIdePol
                         AND  op.Numoper <> nNumOper);
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            cIndOperValida := 'N';
      END;
      
      IF  cIndOperValida = 'S' THEN
        BEGIN
          SELECT 'S' 
          INTO   cIndOperValida
          FROM   dual
          WHERE EXISTS ( SELECT 1
                         FROM   Oper_Pol op
                         WHERE  op.Idepol  = p_nIdePol
                           AND  op.Numoper = nNumOper
                           AND  op.Tipoop  NOT IN ('ESV'));
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              cIndOperValida := 'N';
        END;
        --<I N3038776> Edgard López / 12-04-2012 / RQ:NNTEMIAX-00845 Credito Vehicular Anualización
        nAnualiz := 'N';
        BEGIN
         SELECT anualizacion
           INTO nAnualiz
           FROM PRODUCTO_ENT_EXT_CARGA 
          WHERE codprodext = PR_BBVA_UTIL.DEVOLVER_PRODEXT(p_nIdePol);
        EXCEPTION
         WHEN OTHERS THEN
           nAnualiz := 'N';
        END;
        IF nAnualiz = 'S' THEN
          FOR cADP IN cActu_DatPart (nNumOper) LOOP
           UPDATE DATOS_PARTICULARES 
              SET valtrasf = cADP.MtoOper
            WHERE IdePol   = cADP.IdePol 
              AND NumCert  = cADP.NumCert 
              AND CodCrit  = 'PRIMA_PLAN';
          END LOOP;
        END IF;
         --<F N3038776>         
      END IF;
      
      IF cIndOperValida = 'S' THEN
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_OPER_POL_DET.CREAR_MOV: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      PR_OPER_POL_DET.CREAR_MOV(nNumOper, p_cTipoOper, PZ.FecIniVig, PZ.FecFinVig); -- PR_OPER_POL_DET.CREAR_MOV(nNumOper, :B07_1.TipoOper, :B07_1.FecIniVig, :B07_1.FecFinVig);
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_OPER_POL_DET.CREAR_MOV: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      END IF;   
      --<F N3026404>  
      --<I N3016652< O. Marin/15.10.2003/EMI-176
      PR_OPER_POL.Existe_Oper_No_Nunca_Ren(p_nIdePol,cProcOper,nExisteOper,nNumOperEndoso);
      IF nExisteOper = kSI THEN
        cIndAnul := PR_OPER_POL.Obtener_IndAnul(nNumOperEndoso);
        IF cIndAnul = kNo THEN
          PR_POLIZA.Endoso_Ren(p_nIdePol, 'D');
        END IF;        
      END IF;
      --<F N3016652>        
      DBMS_OUTPUT.PUT_LINE('Trabajando... Activando Poliza Definitiva - 2'); 
      -- <> Elí Segura / 06-04-2004 / Reprogramación.
      PR_EVALUA_POLITICA.Inicializa_Autoriza_Violacion(p_nIdePol,nNumOper);
      --
      cFirma := PR_EVALUA_POLITICA.Obtiene_Firma_Digital(p_nIdePol, nNumOper);
      DBMS_OUTPUT.PUT_LINE('Trabajando... Activando Poliza Definitiva - 3'); 
      -- 
      Actualiza_Ind_Fact;    
      --
      BEGIN
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_RECA_DCTO_CERTIF.Actualiza_Reca_Dcto_Cert: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
        PR_RECA_DCTO_CERTIF.Actualiza_Reca_Dcto_Cert(p_nIdePol,nNumOper);
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_RECA_DCTO_CERTIF.Actualiza_Reca_Dcto_Cert: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0,p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      EXCEPTION
        WHEN OTHERS THEN
          -- LR_error('FRM-MANTPOL Error en package PR_RECA_DCTO_CERT.ACTUALIZA_RECA_DCTO_CERT - '||SQLERRM);
      DBMS_OUTPUT.PUT_LINE('FN_EJECUTAR_ACTIVACION Error en package PR_RECA_DCTO_CERT.ACTUALIZA_RECA_DCTO_CERT - '||SQLERRM);
          RAISE_APPLICATION_ERROR(-20100,'FN_EJECUTAR_ACTIVACION Error en package PR_RECA_DCTO_CERT.ACTUALIZA_RECA_DCTO_CERT - '||SQLERRM);
      END;
      -- Actualiza número de operación de las tablas de 
      -- remesas a enviarse a Bancos.
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','FR_ACTIVAR.Actualizar_Oper_Remesa_Bco: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      FR_ACTIVAR.Actualizar_Oper_Remesa_Bco(p_nIdePol,nNumOper);
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','FR_ACTIVAR.Actualizar_Oper_Remesa_Bco: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      --<I N3020582>Esegura/ 06-11-2004 / Proyecto Help Desk.
      -- Reemplazo de lo que estaba en DIST_REA
      SELECT NVL(SUM(MtoOper),0)
      INTO   nMtoOper
      FROM   OPER_POL
      WHERE  IdePol = p_nIdePol
      AND    NumOper = nNumOper;
      --
      IF nMtoOper != 0 THEN
        -- Si mueve prima se generan documentos.
        LR_Cursor_Normal;
        DBMS_OUTPUT.PUT_LINE('Se procederá a generarár Documentos de Cobranza,segun el Fraccionamiento Calculado,una vez Concluido este Proceso para Realizar cualquier Modificación debera Anular dichos Documentos');
        DBMS_OUTPUT.PUT_LINE('Generando Documentos');
        cEvaluarCorreo:=TRUE;--<RTC-154713>13/03/2019   Milton Lope BBVA - Mejoras al proceso de emisión de G&L P9: Envío de correos 
      
      END IF; 
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA_UTIL.FINALIZAR_ACTIVACION: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      PR_POLIZA_UTIL.FINALIZAR_ACTIVACION(p_nIdePol,nNumOper);            
      --<I RTC-154713>13/03/2019   Milton Lopez BBVA - Mejoras al proceso de emisión de G&L P9: Envío de correos automáticos de respuesta de las emisiones manuales
      IF PR.BUSCA_LVAL('PCBBVA','SWITCH') ='TRUE' THEN
        cSwitchCorreo := TRUE;
      ELSE          
        cSwitchCorreo := FALSE;
      END IF;
    
      --<F RTC-154713>
      -- <I N3030497> Miguel Wilson / 11-09-2008 / Req. BSCOM-00103, Requerimiento CAMPAÑA ACP 500  
      cIndProc := PR_COBRANZA_PPR.Gen_Cobert_Dscto(p_nIdePol);
      
      --<I N3039004> - Carlos Torres - 21/05/2012 - Mejoras Contabilidad
      -- Se invoca al procedimiento para actualizar el estado poliza
      -- para los casos de polizas masivas
      PR_POLIZA_MASIVA.Actualiza_Masiva(p_nIdePol);
      --<F N3039004>      
      
      IF cIndProc = 'S' THEN
          PR_GEN_REA_T.Eliminar_Rea_Temporal(p_nIdePol,NULL,NULL,NULL);       
        nNumOperAux := PR_POLIZA.Activar(p_nIdePol,'D');
        PR_POLIZA_UTIL.Finalizar_Activacion(p_nIdePol,nNumOperAux);
        PR_COBRANZA_PPR.Compensar_Documentos(nNumOperAux);
      END IF;
      -- <F N3030497>    
      
      -- <I N3030785> Lourdes Valdivia G. (CONASTEC) / 09-10-2008 / Procesa Renovaciòn Cuotas Cero
      --Consultamos Tipo de Producto Externo(CENCOSUB)
      --IF PR_RENOV_AUTCOBERT.DEVOLVER_TIPSEG(p_nIdePol) = '241' THEN
      IF PR.BUSCA_LVAL('PROCENCO',PR_RENOV_AUTCOBERT.DEVOLVER_TIPSEG(p_nIdePol)) <> 'INVALIDO' THEN --<N3034656> Lourdes Valdivia Glvez / 09.04.2010          
         PR_RENOV_AUTCOBERT.GEN_RENCERO_DOCSUNAT(p_nIdePol, nNumOper);
      END IF; 
      -- <F N3030785>     
       
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_POLIZA_UTIL.FINALIZAR_ACTIVACION: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
    -- STANDARD.COMMIT;
      
      
        -- <I N3039737> Néstor Cerón - 31/10/2012 REQ. REA-01816 Optimizar cálculo de impuestos a Sunat.
        BEGIN   --<N3040035> - Néstor Ceron - 04/12/2012 - HD 112175 - Optimizar calculo Impuesto Sunat
      select ps.stspol
      into cstspolv
      from poliza_status ps
      where ps.idepol = p_nIdePol;
      cstspolv := nvl(cstspolv,'0');
        --<I N3040035> - Néstor Ceron - 04/12/2012 - HD 112175 - Optimizar calculo Impuesto Sunat
    EXCEPTION WHEN OTHERS THEN
          cstspolv := '0';
        END;
        --<F N3040035>        
        
        rops.IdePol := p_nIdePol;
        rops.NumOper := nNumOper;
        rops.Status := cstspolv;
        rops.CodUsr := PR_SESION.USUARIO_APP;
        rops.Fecha  := sysdate;
        IF rops.Status != '0' THEN   --<N3040035> - Néstor Ceron - 04/12/2012 - HD 112175 - Optimizar calculo Impuesto Sunat
          PR_POLIZA_MASIVA.Crear_Oper_Pol_st(rops);
        END IF;  --<N3040035> - Néstor Ceron - 04/12/2012 - HD 112175 - Optimizar calculo Impuesto Sunat
        BEGIN    --<N3040035> - Néstor Ceron - 04/12/2012 - HD 112175 - Optimizar calculo Impuesto Sunat
           DELETE FROM poliza_status where idepol = p_nIdePol; 
        --<I N3040035> - Néstor Ceron - 04/12/2012 - HD 112175 - Optimizar calculo Impuesto Sunat
        EXCEPTION WHEN OTHERS THEN
          cstspolv := '0';
        END;
        --<F N3040035>        
       -- STANDARD.COMMIT;
        --
-- <F N3039737>

      --<F N3020582>
    DBMS_OUTPUT.PUT_LINE('Invocando a Distribución de Reaseguros');   
      --<I N3030565> 15.06.2008 / Max Canal Salas /Proyecto Impresiones CCM
      BEGIN
        SELECT STSPOL,CODPROD ,NUMPOL
        INTO   cStsPol,cCodProd,nNumPol
        FROM   POLIZA P
        WHERE  P.IDEPOL = p_nIdePol;
      EXCEPTION  
        WHEN OTHERS THEN 
           DBMS_OUTPUT.PUT_LINE('Error al obtener Numero de poliza');
       RAISE_APPLICATION_ERROR(-20100,'Error al obtener Numero de poliza');
      END ;  
      nIndCNT := PR_IMP_POL_CCM_CON.VALIDA_CNT(nNumPol,cCodProd);
      IF PR.BUSCA_LVAL('CCMLVGRL','SWICTHCCM') = 'S'  AND  nIndCNT ='N' AND PR.EXISTE_LVAL('CCMPRODX',PZ.CODPROD) = 'S' THEN  --<N3030980>/ 05.11.2008/Max Canal Salas /Proyecto CCM  impresion de polizas
        nIdImpresion := PR_IMP_POL_CCM.DEVUELVE_IND_GRUPO_IMP(cCodProd,nNumOper,p_nIdePol); 
        BEGIN
          SELECT DISTINCT OP.TIPOOP 
          INTO   cTipoOper
          FROM   OPER_POL OP
          WHERE  IdePol = p_nIdePol
          AND    NumOper = nNumOper;
        EXCEPTION 
          WHEN OTHERS THEN 
            cTipoOper  := NULL;
        END ;  
          
      
      END IF;
      --<F N3030565>
      
      --<I N3043352> Juan Villaorduña / 17-11-2014 / 5791
      IF nNumOper IS NOT NULL THEN
        -- FR_ModificaCorredor(nNumOper);
                DECLARE
          nCodInterPol  acselx.intermediacion_pol.numid%TYPE DEFAULT NULL;
          nCodInterFact acselx.factura.codinter%TYPE DEFAULT NULL;
        BEGIN
            BEGIN       
              SELECT inter.codinter
              INTO   nCodInterPol
              FROM   acselx.poliza pol,
                acselx.intermediacion_pol inp,
                acselx.intermediario      inter
              WHERE  pol.idepol = inp.idepol
              -- AND    pol.idepol = :B01_1.idepol p_nIdePol
              AND    pol.idepol = p_nIdePol
              AND    inp.indlider = 'S'
              AND    inter.numid = inp.numid;
            EXCEPTION
              WHEN no_data_found THEN
              nCodInterPol := NULL;
            END;
            
            BEGIN
              SELECT codinter
              INTO nCodInterFact
              FROM acselx.factura
              WHERE numoper = nNumOper;
            EXCEPTION
            WHEN no_data_found THEN
              nCodInterFact := NULL;
            END;
            
            IF nCodInterPol IS NOT NULL AND TO_NUMBER(nCodInterFact) IS NOT NULL THEN
              IF nCodInterPol != nCodInterFact THEN
                UPDATE acselx.factura
                SET codinter = LPAD(nCodInterPol, 6, '0')
                WHERE numoper = nNumOper;
                
                UPDATE acselx.acreencia
                SET codinterlider = LPAD(nCodInterPol, 6, '0')
                WHERE numacre IN (SELECT gif.numacre
                        FROM acselx.cond_financiamiento cof, acselx.giros_financiamiento gif
                        WHERE cof.numfinanc = gif.numfinanc
                        AND numoper = nNumOper);
              END IF;
            END IF;
        EXCEPTION
          WHEN OTHERS THEN NULL;
        END;
        -- FIN
      END IF;
      --<F N3043352> Juan Villaorduña / 17-11-2014 / 5791
            
      -- .:. LR_Invocar_DistRea(p_nIdePol,nNumOper,0,'N');                            
      --
      DBMS_OUTPUT.PUT_LINE('Eliminando Temporales de Reaseguro.');
      PR_GEN_REA_T.Eliminar_Rea_Temporal(p_nIdePol,NULL,NULL,NULL);
      -- STANDARD.COMMIT;
      -- Obtenemos datos actualizados de la póliza para hacer validaciones.
      -- .:. P := PR_POLIZA.Datos_Poliza(p_nIdePol);
      IF PZ.StsPol = 'ACT' THEN     
        -- Poner la actualizacion de inspecciones en notes
        IF PR_POLIZA_UTIL.Maneja_Insp_Vehiculos_Pol(p_nIdePol) = kSI THEN    
          DBMS_OUTPUT.PUT_LINE('Actualizando Número de Inspección en Notes');
          -- .:. FR_ACTIVAR.Actualizar_Inspeccion_Notes(p_nIdePol,PZ.NUMPOL);                      
        END IF;
        -- Elimina datos posibles de rehabilitación si hubiera.
        ELIMINAR_REHAB;
        -- .:. STANDARD.COMMIT;
      END IF;    
      DBMS_OUTPUT.PUT_LINE('Trabajando... Eliminando temporales de Mov.Prima');
      PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);    
      -- .:. STANDARD.COMMIT;       
      FR_UPDAUTORIZAREDIST(p_nIdePol, nNumOper); -- <N3041386> Claudia Yugar Ladines / 23-08-2013 / CNT-01702: Manejo de re-cálculo de la distribución de reaseguros
   ELSE
      DBMS_OUTPUT.PUT_LINE('Trabajando... Reversando Activación');
      PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);
      -- .:. STANDARD.COMMIT;
   END IF;

    DBMS_OUTPUT.PUT_LINE('Proceso ejecutado satisfactoriamente');
--<I N3033323>
  FOR C IN cActualiza LOOP
    BEGIN
      SELECT F.NUMFACT, F.MTOFACTMONEDA, F.CODMONEDA
      INTO nNumFact, nMtoFactMoneda, cCodMoneda
      FROM FACTURA F
      WHERE F.IDEFACT IN (SELECT A.IDEFACT
                          FROM ACREENCIA A
                          WHERE A.NUMACRE IN (SELECT G.NUMACRE
                                              FROM GIROS_FINANCIAMIENTO G
                                              WHERE G.NUMFINANC IN (SELECT RE.NUMFINANC
                                                                    FROM REC_FINANCIAMIENTO RE
                                                                    WHERE RE.IDEREC IN (SELECT MAX(R.IDEREC) IDEREC 
                                                                                        FROM RECIBO R
                                                                                        WHERE R.IDEMOVPRIMA IN ( SELECT M.IDEMOVPRIMA
                                                                                                                 FROM   MOD_COBERT M
                                                                                                                 WHERE  M.NUMMOD = C.NUMMOD
                                                                                                                   AND  M.IDECOBERT = C.IDECOBERT)))));
    EXCEPTION
      WHEN OTHERS THEN 
        nNumFact := 0;
        nMtoFactMoneda := NULL;
        cCodMoneda := C.CODMONEDA;
    END;

    SELECT CER.STSCERT
    INTO   cStsCert
    FROM   CERTIFICADO CER
    WHERE  CER.IDEPOL = C.IDEPOL
      AND  CER.NUMCERT = C.NUMCERT;

    UPDATE LOG_EXCLUYE_CERT T
    SET    T.NUMFACT = nNumFact,
           T.STSLOG = decode(nNumFact,0,'ERR',cStsCert),
           T.MTOFACTMONEDA = nMtoFactMoneda,
           T.CODMONEDA = cCodMoneda,
           T.NUMOPER = nNumOper
    WHERE  T.IDEPOL = C.IDEPOL
      AND  T.NUMCERT = C.NUMCERT
      AND  T.STSLOG = 'PEN'
      AND  T.IDECOBERT = C.IDECOBERT
      AND  T.NUMMOD = C.NUMMOD
      AND  T.NUMFACT IS NULL;
    -- .:. COMMIT;  
  END LOOP;
--<N3035871> NNTCOB-00015. Roberto Escudero / 23-09-2010 / Se mueve la baja de certificado a otro proceso 
    -- Se devuelve la operación procesada.
  RETURN nNumOper;
EXCEPTION
  WHEN OTHERS THEN
        PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);
    -- .:. STANDARD.COMMIT;   
    DBMS_OUTPUT.PUT_LINE('Error al Activar: '||SQLERRM);     
    -- Devuelve nulo en señal de error.
    RETURN TO_NUMBER(NULL);
END FN_EJECUTAR_ACTIVACION;
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
PROCEDURE SP_ACTUALIZAR_INDCOBESP(p_nNumOper   IN   OPER_POL.NumOper%TYPE) IS
  CURSOR c_Cobert_Cert IS
    SELECT IdeCobert
    FROM   MOD_COBERT
    WHERE  IdeMovPrima IN (SELECT IdeMovPrima 
                           FROM   RECIBO 
                           WHERE  IdeOp = p_nNumOper
                          );  
BEGIN
  FOR C IN c_Cobert_Cert LOOP
    UPDATE MOD_COBERT
    SET    IndCobEsp = 'S'
    WHERE  IdeCobert = C.IdeCobert;
  END LOOP;
END SP_ACTUALIZAR_INDCOBESP;
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
PROCEDURE SP_ACTUALIZAR_BASERECADTCO(p_nNumOper IN OPER_POL.NumOper%TYPE) IS
    CURSOR c_Reca_Dcto_Esp  IS
      SELECT CodRecaDcto,TipoRecaDcto,NumCert,IdePol
      FROM   DET_OPER_POL
      WHERE  NumOper = p_nNumOper;
  
  BEGIN
    FOR R IN c_Reca_Dcto_Esp LOOP
      UPDATE RECA_DCTO_CERTIF
      SET    BaseRecaDcto = 'P'
      WHERE  CodRecaDcto  = R.CodRecaDcto
        AND  TipoRecaDcto = R.TipoRecaDcto
        AND  IdePol = R.IdePol
        AND  NumCert = R.NumCert;
    END LOOP;
END SP_ACTUALIZAR_BASERECADTCO;
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
PROCEDURE SP_ACTIVAR_ACTUALIZAR_POLIZA( p_nIdePol IN Poliza.IdePol%TYPE)
IS
  
      kActivo       CONSTANT VARCHAR2(3) := 'ACT';
    nButton       NUMBER;
    cClasePol     POLIZA.ClasePol%TYPE;
    cCodFormPago  POLIZA.CodFormPago%TYPE;
    nIndFact      POLIZA.IndFact%TYPE;
    nNumOper      OPER_POL.NumOper%TYPE;    

    nNumFact       FACTURA.NUMFACT%TYPE;
    cStsCert       CERTIFICADO.STSCERT%TYPE;
    nMtoFactMoneda FACTURA.MTOFACTMONEDA%TYPE;
    cCodMoneda     FACTURA.CODMONEDA%TYPE;
    cVar           VARCHAR2(1):= 0;
    nNumOblig      OBLIGACION.NUMOBLIG%TYPE;
    cInd         NUMBER := 0;
    nNumOperAnu    OPER_POL.NUMOPER%TYPE;

    CURSOR cExcluye IS
      SELECT T.IDECOBERT, T.NUMMOD, T.NUMCERT, T.NUMPOL, T.CODPROD, T.IDEPOL, T.NUMCERTEXT, T.NUMCARGA, 
             T.NUMCORRDATA, T.MTOTRAMA, T.TIPSEG, T.FECCARGA, T.NUMFACT, T.MTOFACTMONEDA, T.CODMONEDA, T.STSLOG
      FROM   acselx.LOG_EXCLUYE_CERT T, CERTIFICADO C, POLIZA P
      WHERE  P.IDEPOL = p_nIdePol
      AND  P.STSPOL = 'ACT'
      AND  T.IDEPOL = P.IdePol
      AND  T.IDEPOL = C.IDEPOL
      AND  T.NUMCERT = C.NUMCERT
      AND  T.STSLOG = 'ACT'--  ESTUVO ACTIVO
      AND  C.STSCERT = 'ACT'-- EL CERTIFICADO ESTA ACTIVO
      AND  T.TIPODOC = 'LQ'--  En la tabla se guarda LQ y LA
      AND  T.NUMFACT IS NOT NULL;
    
    CURSOR cCertif_Anular (p_nNumOper OPER_POL.NUMOPER%TYPE)IS
      SELECT IDECOBERT, NUMMOD, NUMCERT, NUMPOL, CODPROD, IDEPOL, NUMCERTEXT, NUMCARGA, 
             NUMCORRDATA, MTOTRAMA, TIPSEG, FECCARGA, NUMFACT, MTOFACTMONEDA, CODMONEDA, 
             STSLOG, TIPODOC, NUMOPER, FECFIN, FECANULACION, MTOOBLIG 
      FROM  LOG_EXCLUYE_CERT L
          WHERE L.NUMOPER = p_nNumOper
          AND  L.TIPODOC = 'LQ'
          AND  L.MTOOBLIG IS NOT NULL;

    CURSOR cActObligs (p_nNumOperAnu OPER_POL.NUMOPER%TYPE)IS
      SELECT DISTINCT NUMOBLIG, CODMONEDA, MTONETOOBLIGMONEDA
      FROM   OPER_POL OP, OBLIGACION OB
      WHERE  OP.IDEPOL = p_nIdePol
      AND  OP.NUMOPER = p_nNumOperAnu
      AND  OB.NUMOPER = OP.NUMOPER
      AND  OB.STSOBLIG = 'ACT';     
        
        PZ POLIZA%ROWTYPE;
    
BEGIN
        PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
        IF PZ.IDEPOL IS NULL THEN            
            DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
            RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
        END IF;
     -- Exclusión de Certificados Opción: Anulando Facturas
    IF PR_VALIDA_EXC_CERT.FR_VERIF_TEMPORAL(PZ.IDEPOL) > 0 AND PR.BUSCA_LVAL('PRODEXCL',PZ.CODPROD)<>'INVALIDO' THEN
      FR_EXCLUSION_MASIVA.EXCLUIR_ANULANDO_FACTURAS(PZ.IDEPOL);
    ELSE
    
        IF PZ.STSPOL = kActivo AND PZ.CODFORMPAGO IN ('A','E') THEN
              -- LR_Error(LR_Mensaje_Acsel('CSG',52,'  ','  ','  '));
                    DBMS_OUTPUT.PUT_LINE(PR_API_COMISION_INTERMEDIARIO.FN_Mensaje_ACSEL('CSG',52,'  ','  ','  '));
                    RAISE_APPLICATION_ERROR(-20100,PR_API_COMISION_INTERMEDIARIO.FN_Mensaje_ACSEL('CSG',52,'  ','  ','  '));
          END IF;
          -- Actualizamos fecha de operación.
          UPDATE POLIZA
          SET    FecOper = PR.Fecha_Movimiento(TRUNC(SYSDATE)) -- :C00.dFecOper
          WHERE  IdePol = PZ.IDEPOL;
          IF PZ.CODFORMPAGO IN ('A','E') THEN
                nNumOper := PR_API_COMISION_INTERMEDIARIO.FN_EJECUTAR_ACTIVACION(PZ.IDEPOL); --FR_ACTIVAR.Ejecutar_Activacion(p_nIdePol);       
            IF nNumOper IS NULL THEN
              -- LR_Rollback('Error al ejecutar activación de póliza.');
              DBMS_OUTPUT.PUT_LINE('Error al ejecutar activación de póliza.');
                            RAISE_APPLICATION_ERROR(-20100,'Error al ejecutar activación de póliza.');
            END IF;  
          ELSE
            IF TRUNC(ADD_MONTHS(PZ.FECULTFACT,-1)) > TRUNC(PZ.FECFINVIG) THEN
              -- LR_Error('A la Póliza ya se le realizo su ultima facturación');
              DBMS_OUTPUT.PUT_LINE('A la Póliza ya se le realizo su ultima facturación');
                            RAISE_APPLICATION_ERROR(-20100,'A la Póliza ya se le realizo su ultima facturación');
            END IF;

            cClasePol    := PZ.CLASEPOL;
            cCodFormPago := PZ.CODFORMPAGO;
            nIndFact     := NVL(PZ.INDFACT,'N');
            
            IF nIndFact = 'N' AND cClasePol = '4' THEN
              UPDATE POLIZA
              SET    ClasePol = '3',
                CodFormPago = 'A'
              WHERE  IdePol = p_nIdePol;
            ELSIF nIndFact != 'N' AND cClasePol = '4' THEN
              -- LR_Error('No se puede actualizar movimientos despues de haber Pre-Liquidado');
              DBMS_OUTPUT.PUT_LINE('No se puede actualizar movimientos despues de haber Pre-Liquidado');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede actualizar movimientos despues de haber Pre-Liquidado');
            END IF;
            nNumOper := PR_API_COMISION_INTERMEDIARIO.FN_EJECUTAR_ACTIVACION(p_nIdePol);  -- FR_ACTIVAR.Ejecutar_Activacion(p_nIdePol);
            IF nNumOper IS NULL THEN
              -- LR_Rollback('Error al ejecutar activación de póliza.');
              DBMS_OUTPUT.PUT_LINE('Error al ejecutar activación de póliza.');
                            RAISE_APPLICATION_ERROR(-20100,'Error al ejecutar activación de póliza.');
            END IF;  
            IF nIndFact = 'N' AND cClasePol = '4' THEN
                UPDATE POLIZA
                SET    ClasePol = cClasePol, CodFormPago = cCodFormPago
                WHERE  IdePol = p_nIdePol;        
                PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE('I','ACTIVAR POLIZA - MANTPOL','FR_ACTIVAR.Actualizar_BaseRecaDtco: IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, PZ.IdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT
                PR_API_COMISION_INTERMEDIARIO.SP_ACTUALIZAR_BASERECADTCO(nNumOper); -- FR_ACTIVAR.Actualizar_BaseRecaDtco(nNumOper);            
                PR_API_COMISION_INTERMEDIARIO.SP_ACTUALIZAR_INDCOBESP(nNumOper); -- FR_ACTIVAR.Actualizar_IndCobEsp(nNumOper);
                PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE('F','ACTIVAR POLIZA - MANTPOL','FR_ACTIVAR.Actualizar_BaseRecaDtco: IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, PZ.IdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT Log de proceso. Pase temporal
                -- STANDARD.COMMIT;
            END IF;       
          END IF;
          --<I N3035871> NNTCOB-00015. Roberto Escudero / 23-09-2010 / Se crea los procesos de generación de obligaciones por CC pendiente
          --1)Valida si cualquier certificado de esta poliza para esta operacion, tiene documentos pendientes para generar una LA
          PR_BBVA_01.OBTIENE_MTO_OBLIGACION (p_nIdePol,nNumOper,cInd);
    
          IF cInd = 1 THEN 
          --2)Para cada una de los certificados por poliza, genera la LA y anula el certificado
            FOR cR IN cCertif_Anular (nNumOper) LOOP
              PR_BBVA_01.GENERA_OBLIG_ANULA_CERT(cR,nNumOperAnu);
              --Por cada operacion se puede haber generado mas de una LA          
              FOR cObl IN cActObligs (nNumOperAnu) LOOP
                IF cObl.NumOblig IS NOT NULL THEN 
                  INSERT INTO LOG_EXCLUYE_CERT (IDECOBERT, NUMMOD, NUMCERT, NUMPOL, CODPROD, IDEPOL, NUMCERTEXT, NUMCARGA, 
                     NUMCORRDATA, MTOTRAMA, TIPSEG, FECCARGA, NUMFACT, MTOFACTMONEDA, CODMONEDA, 
                     STSLOG, TIPODOC, NUMOPER, FECFIN, FECANULACION, MTOOBLIG )
                     VALUES (cR.IDECOBERT, cR.NUMMOD, cR.NUMCERT, cR.NUMPOL, cR.CODPROD, cR.IDEPOL, cR.NUMCERTEXT,cR.NUMCARGA,
                     cR.NUMCORRDATA, cR.MTOTRAMA, cR.TIPSEG, cR.FECCARGA, cObl.NumOblig, cObl.MTONETOOBLIGMONEDA, cObl.CODMONEDA, 
                     cR.STSLOG, 'LA', nNumOperAnu, cR.FECFIN, cR.FECANULACION, cR.MTOOBLIG);
                ELSE 
                  UPDATE LOG_EXCLUYE_CERT
                  SET    MTOOBLIG = NULL
                  WHERE  IDECOBERT = cR.IDECOBERT
                    AND  NUMMOD = cR.NUMMOD
                    AND  NUMCERT = cR.NUMCERT
                    AND  NUMPOL = cR.NUMPOL
                    AND  IDEPOL = cR.IDEPOL
                    AND  TIPODOC = 'LQ';
                END IF;
              END LOOP;
              --Actualiza los datos del certificados
              SELECT CER.STSCERT
              INTO   cStsCert
              FROM   CERTIFICADO CER
              WHERE  CER.IDEPOL = p_nIdePol
                AND  CER.NUMCERT = cR.NUMCERT;
                
              IF cStsCert IN ('EXC','ANU') THEN
                UPDATE acselx.LOG_EXCLUYE_CERT T
                SET    T.STSLOG = 'EXC'
                WHERE  T.IDEPOL = p_nIdePol
                  AND  T.NUMCERT = cR.NUMCERT
                  AND  T.STSLOG = 'PEN'
                  AND  TIPODOC = 'LA';
                COMMIT;
              END IF;
            END LOOP; 
          ELSE

            FOR C IN cExcluye LOOP
              BEGIN 
                PR_BBVA_01.PROCESA_BAJAS_PERIODO_CORTO(C.IDEPOL,C.NUMPOL,C.NUMCERT, C.TIPSEG, C.FECCARGA, C.NUMCARGA,C.NUMCORRDATA);
              EXCEPTION 
                WHEN OTHERS THEN
                  NULL;
              END;
              
              SELECT CER.STSCERT
              INTO   cStsCert
              FROM   CERTIFICADO CER
              WHERE  CER.IDEPOL = C.IDEPOL
                AND  CER.NUMCERT = C.NUMCERT;
                
              IF cStsCert IN ('EXC','ANU') THEN
                UPDATE acselx.LOG_EXCLUYE_CERT T
                SET    T.STSLOG = 'EXC'
                WHERE  T.IDEPOL = C.IDEPOL
                  AND  T.NUMCERT = C.NUMCERT
                  AND  T.STSLOG != 'PEN'
                  AND  T.IDECOBERT = C.IDECOBERT
                  AND  T.NUMMOD = C.NUMMOD        
                  AND  T.NUMFACT IS NOT NULL;
                -- COMMIT;
              END IF;
            END LOOP;
          END IF;

    END IF;
    

END SP_ACTIVAR_ACTUALIZAR_POLIZA;
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
PROCEDURE SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(
  p_nIdePol   IN Poliza.IdePol%TYPE,
  p_cTipoOper IN LVAL.CODLVAL%TYPE
) IS

	nCantReg  NUMBER := 0;
	v_NumOper Oper_Pol.NumOper%TYPE;	
	nStsPol   POLIZA.StsPol%TYPE;
    PZ        POLIZA%ROWTYPE;
    
BEGIN
    BEGIN
      SELECT 1 
      FROM   lval l 
      WHERE  l.Tipolval = 'TIPOEND'
      AND l.Codlval = p_cTipoOper;
    EXCEPTION
      WHEN OTHERS THEN
		DBMS_OUTPUT.PUT_LINE('El motivo de endoso no valido.');
        RAISE_APPLICATION_ERROR(-20100,'El motivo de endoso no valido.');
    END;

    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;
	
    IF p_cTipoOper IS NOT NULL THEN

        PR_API_COMISION_INTERMEDIARIO.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;	
        PR_API_COMISION_INTERMEDIARIO.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);  

        --<I RTC 316343> David Yupanqui / 24-02-2023 / [PROYECTO TECH CORE] Mejora en Registro y Validación de Maquinaria TREC
        IF PR.BUSCA_LVAL('MAQTREC', PZ.CODPOL)='1' THEN
            BEGIN
                SELECT COUNT(1) 
                INTO nCantReg
                FROM Maquinaria_Trec
                WHERE IdePol = PZ.IDEPOL;
            EXCEPTION WHEN OTHERS THEN
                nCantReg := 0;
            END;
            IF nCantReg > 0 THEN
                BEGIN
                    SELECT max(NumOper)
                    INTO v_NumOper
                    FROM Oper_Pol
                    WHERE IdePol = PZ.IDEPOL
                    AND TipoOp <> ('EMI')
                    AND NVL(INDANUL,'N') = 'N';
                EXCEPTION WHEN OTHERS THEN
                    v_NumOper := NULL;
                END;
                IF v_NumOper IS NOT NULL THEN
                    BEGIN
                        UPDATE Maquinaria_Trec
                        SET NumOper = v_NumOper
                        WHERE IdePol = PZ.IDEPOL
                        AND TO_CHAR(FecOper,'DD/MM/YYYY') = TO_CHAR(SYSDATE,'DD/MM/YYYY');
                        -- STANDARD.COMMIT;
                    EXCEPTION WHEN OTHERS THEN
                        NULL;
                    END;
                END IF;
            END IF;
        END IF;

        PR_API_COMISION_INTERMEDIARIO.SP_ACTIVA_GARANTIAS(PZ.STSPOL, PZ.IDEPOL);  -- FR_ACTIVA_GARANTIAS(nStsPOL);

        --<I RTC 316472> David Yupanqui / 27-04-2023 / [PROYECTO TECH CORE] Armado Automático del Anexo 7
        if pr.busca_lval('SLIP_AUT',PZ.CODPROD) <> 'INVALIDO' then
            acselx.pr_slip_auto.texto_prima(PZ.IDEPOL);
        end if;
        --<F RTC 316472>
    
    END IF;
END SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA; -- Mejora en Registro y Validación de Maquinaria TREC
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
  p_cError       OUT VARCHAR2(3000)
) IS

  cTextoTr       VARCHAR2(200);
  cCantOpera     NUMBER;
  cTipo          VARCHAR2(10); 
  nIdepol        NUMBER(14);
  nNumOper       NUMBER(14);
  cNumPol        POLIZA.NUMPOL%TYPE;
  cExistTramit   NUMBER;
  nNumTram       Poliza_Tramite.Numtramite%TYPE;

  cTexto         VARCHAR2(200);
  cOperEnd       VARCHAR2(1); 
  nDummy         pls_integer;
  cValidaTram    VARCHAR2(1);

  nNumIdInterm   INTERMEDIACION_POL.NumIdInter_MC%Type;
  cCodAgInterm   INTERMEDIACION_POL.CodAgeInter_MC%Type;

  c_Codplanf     DATOS_PARTICULARES.CodPlan%type;
  c_Revplanf     DATOS_PARTICULARES.RevPlan%type;
  n_NumIDViaCob  AUTORIZA_DOC_PAGO_CLIENTE.NumIdViaCob%type;
  c_CodDep       AUTORIZA_DOC_PAGO_CLIENTE.CodDep%type;
  n_NumIdDocf    DOCS_IDENTIDAD.NumIdDoc%type;
  c_cResp        VARCHAR2(100);
  cExiste        VARCHAR2(1);
  cDescbloq      VARCHAR2(100);

  cExisteBloqMor VARCHAR2(1);
  cDescbloqMor   VARCHAR2(100);

  cMsjCobProv    VARCHAR2(1000):= NULL;
  cValTrec       VARCHAR2(1):= '0';
  cMsjPolSBS     VARCHAR2(1000):= NULL; 
  cMsjError      VARCHAR2(1000):= NULL; 
  
  nCantReg       NUMBER := 0;
  v_NumOper      Oper_Pol.NumOper%TYPE;

  PZ             POLIZA%ROWTYPE;
  cNumTramite    VARCHAR2(30);

BEGIN
    p_cError := '0,OK';
    
    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||p_nIdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;
    
    BEGIN
      SELECT NumIdInter_MC, CodAgeInter_MC
      INTO nNumIdInterm, cCodAgInterm
      FROM INTERMEDIACION_POL
      WHERE IDEPOL = PZ.IDEPOL
      AND INDLIDER = 'S';
    EXCEPTION
      WHEN OTHERS THEN
        nNumIdInterm := null;
        cCodAgInterm := null;
    END;
    
    IF nNumIdInterm IS NULL THEN
        -- nDummy := LR_ALERTA('No se puede activar la Póliza, no se ha consignado el Intermediario.');
        p_cError := '1,'||'No se puede activar la Póliza, no se ha consignado el Intermediario.';
        DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, no se ha consignado el Intermediario.');
        RAISE_APPLICATION_ERROR(-20100,'No se puede activar la Póliza, no se ha consignado el Intermediario.');
    END IF;
  
    IF cCodAgInterm IS NULL THEN
      -- nDummy := LR_ALERTA('No se puede activar la Póliza, no se ha consignado la Agencia de Intermediario.');
        p_cError := '1,'||'No se puede activar la Póliza, no se ha consignado la Agencia de Intermediario.';
        DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, no se ha consignado la Agencia de Intermediario.');
        RAISE_APPLICATION_ERROR(-20100,'No se puede activar la Póliza, no se ha consignado la Agencia de Intermediario.');
    END IF;

    cExisteBloqMor:= ACSELX.PR_BLOQUEO_MOROSIDAD.VALIDAR_MOROSIDAD_POLIZA(PZ.IDEPOL);
    
    IF cExisteBloqMor <> 'N' THEN
      BEGIN 
          SELECT NVL(B.DescBloq,'MOROSIDAD')
          INTO cDescbloqMor
          FROM BLOQUEOS B
          WHERE B.Codbloq = PR.BUSCA_LVAL('CFANMSV', 'CODBLOQ')
          AND ROWNUM = 1;
      EXCEPTION WHEN OTHERS THEN  
        cDescbloqMor:='MOROSIDAD';
      END;
    END IF;
    
    IF cExisteBloqMor = 'S' THEN
        -- nDummy := LR_ALERTA('No se puede activar la Póliza, el Cliente/Contratante se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
        p_cError := '1,'||'No se puede activar la Póliza, el Cliente/Contratante se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')';
        DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, el Cliente/Contratante se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
        RAISE_APPLICATION_ERROR(-20100,'No se puede activar la Póliza, el Cliente/Contratante se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
    ELSIF cExisteBloqMor = 'X' THEN
        -- nDummy := LR_ALERTA('No se puede activar la Póliza, existe un Responsable de Pago que se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
         p_cError := '1,'||'No se puede activar la Póliza, existe un Responsable de Pago que se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')';
         DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, existe un Responsable de Pago que se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
         RAISE_APPLICATION_ERROR(-20100,'No se puede activar la Póliza, existe un Responsable de Pago que se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
    END IF;
    
    IF PR.BUSCA_LVAL('POLDASBS', 'SWITCH') = 'S' AND PR.BUSCA_LVAL('PROVEHPO', PZ.CodProd) != 'INVALIDO' THEN
        PR_VALIDAR_POLITICAS_SBS.PR_VALIDA_POLITICA_SBS(PZ.IDEPOL, cMsjPolSBS, cMsjError); -- Politica de Datos SBS 
        
        IF cMsjPolSBS IS NOT NULL THEN
            p_cError := '1,'||cMsjPolSBS;
            DBMS_OUTPUT.PUT_LINE(cMsjPolSBS);
            RAISE_APPLICATION_ERROR(-20101,cMsjPolSBS);
        END IF;
      
        --  Politica de Datos SBS 
        IF cMsjError IS NOT NULL THEN
          p_cError := '1,'||cMsjError;
          DBMS_OUTPUT.PUT_LINE(cMsjError);
          RAISE_APPLICATION_ERROR(-20102,cMsjError);
        END IF;
        --Politica de Datos SBS 
        --Necesidad 2 Nuevo procedimiento para EN TRAMITE y SIN DATO 
        --Se setea el mensaje de error para validar la necesidad 2
        cMsjPolSBS:= null;    
        PR_VALIDAR_POLITICAS_SBS.PR_VALIDA_POLITICA_SBS_N2(PZ.IDEPOL, PZ.CODPROD, PZ.NUMPOL, cMsjPolSBS, cMsjError); --<RTC 288374> Juan Villaorduña - 10/06/2021 - Politica de Datos SBS 
    
        IF cMsjPolSBS IS NOT NULL THEN
          p_cError := '1,'||cMsjPolSBS;
          DBMS_OUTPUT.PUT_LINE(cMsjPolSBS);
          RAISE_APPLICATION_ERROR(-20103,cMsjPolSBS);
        END IF;
      
        IF cMsjError IS NOT NULL THEN
          p_cError := '1,'||cMsjError;
          DBMS_OUTPUT.PUT_LINE(cMsjError);
          RAISE_APPLICATION_ERROR(-20104,cMsjError);
        END IF;

    End If;
    
    If PR.BUSCA_LVAL('POLDASBS', 'SWITCH') = 'S' AND PR.BUSCA_LVAL('PROVEHTR', PZ.CodProd) != 'INVALIDO' THEN 
      
      cMsjPolSBS:= null;
      PR_VALIDAR_POLITICAS_SBS.PR_VALIDA_POLITICA_SBS_N3(PZ.IDEPOL, PZ.FECINIVIG , PZ.FecFinVig, PZ.CodProd, cMsjPolSBS, cMsjError); --<RTC 288374> Juan Villaorduña - 10/06/2021 - Politica de Datos SBS 
    
      IF cMsjPolSBS IS NOT NULL and cMsjPolSBS <> '.' THEN       
         p_cError := '1,'||cMsjPolSBS;
         DBMS_OUTPUT.PUT_LINE(cMsjPolSBS);
         RAISE_APPLICATION_ERROR(-20105,cMsjPolSBS);
      END IF;
      
      --Politica de Datos SBS 
      IF cMsjError IS NOT NULL THEN
        p_cError := '1,'||cMsjError;
        DBMS_OUTPUT.PUT_LINE(cMsjError);
        RAISE_APPLICATION_ERROR(-20106,cMsjError);
      END IF;
      
    END IF;
    
    IF PR.BUSCA_LVAL('PROVEHCP', PZ.CODPROD) != 'INVALIDO' THEN      
      PR_API_COMISION_INTERMEDIARIO.SP_VALIDA_COBERT_PROV(PZ.IDEPOL, cMsjCobProv); -- PR_VALIDA_COBERT_PROV(PZ.IDEPOL, cMsjCobProv);
      IF cMsjCobProv IS NOT NULL THEN
          p_cError := '1,'||cMsjCobProv;
          DBMS_OUTPUT.PUT_LINE(cMsjCobProv);
          RAISE_APPLICATION_ERROR(-20107,cMsjCobProv);
      END IF;
    END IF;
    
    BEGIN 
      SELECT dp.CodPlan, dp.RevPlan
        INTO c_Codplanf, c_Revplanf
        FROM DATOS_PARTICULARES dp
       WHERE dp.idepol = PZ.IDEPOL
         AND dp.numcert = 1
         AND rownum = 1;
    EXCEPTION WHEN OTHERS THEN
      c_Codplanf := NULL;
      c_Revplanf := NULL;
    END;
    
    IF c_Codplanf IS NOT NULL AND c_Revplanf IS NOT NULL AND PR.BUSCA_LVAL('RENOVDPP',PZ.CODPROD||c_Codplanf||c_Revplanf) != 'INVALIDO' THEN
      --Validación de la vía de cobro en AS400.
      BEGIN
          SELECT AD.NumIDViaCob, AD.CodDep
          INTO n_NumIDViaCob, c_CodDep
          FROM AUTORIZA_DOC_PAGO_CLIENTE AD 
          WHERE AD.numautoriza = (SELECT rp.numautoriza 
                                   FROM RESP_PAGO rp 
                                  WHERE rp.idepol = PZ.IDEPOL 
                                    AND rp.numcert = 1)
            AND AD.Tipomodcob = 'DPP';
      EXCEPTION WHEN OTHERS THEN
        p_cError := '1,'||'No se puede activar la Póliza, No se ha ingresado el Nro. de Pago Fácil.';
        DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, No se ha ingresado el Nro. de Pago Fácil.');
        RAISE_APPLICATION_ERROR(-20106,'No se puede activar la Póliza, No se ha ingresado el Nro. de Pago Fácil.');
      END;

      BEGIN
         SELECT di.NumIdDoc
         INTO n_NumIdDocf 
         FROM DOCS_IDENTIDAD di 
         WHERE di.tipoiddoc = '1' 
         AND di.numid = n_NumIDViaCob;  
      EXCEPTION WHEN OTHERS THEN
        p_cError := '1,'||'No se puede activar la Póliza, No se ha encontrado el RUC de la Vía de Cobro.';
        DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, No se ha encontrado el RUC de la Vía de Cobro.');
        RAISE_APPLICATION_ERROR(-20106,'No se puede activar la Póliza, No se ha encontrado el RUC de la Vía de Cobro.');
      END;
    
        PR_PREAFILIACION_AM.F_VALIDA_VIA_COBRO(n_NumIdDocf, '1' , c_cResp);
      IF c_cResp IS NOT NULL THEN
          p_cError := '1,'||'No se puede activar la Póliza, '||c_cResp;
          DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, '||c_cResp);
          RAISE_APPLICATION_ERROR(-20106,'No se puede activar la Póliza, '||c_cResp);
      END IF;
      
      --Validación de Cliente A/x.
      BEGIN
        SELECT 'S', B.Descbloq
        INTO  cExiste, cDescbloq 
          FROM  BLOQUEOS_TERCEROS BT, BLOQUEOS B
          WHERE BT.Codbloq = B.Codbloq
          AND BT.Codvinc = B.Codvinc 
          AND TipoId =   '0'
          AND  NumId =   PZ.CODCLI
          AND  DvId  =   '0';
      EXCEPTION WHEN OTHERS THEN
        cExiste := 'N';
      END;
      
      IF cExiste='S' THEN
          p_cError := '1,'||'No se puede activar la Póliza, el Cliente se encuentra Bloqueado. (Motivo: '||cDescbloq||')';
          DBMS_OUTPUT.PUT_LINE('No se puede activar la Póliza, el Cliente se encuentra Bloqueado. (Motivo: '||cDescbloq||')');
          RAISE_APPLICATION_ERROR(-20106,'No se puede activar la Póliza, el Cliente se encuentra Bloqueado. (Motivo: '||cDescbloq||')');
      END IF;

    END IF;
    
    IF PR.BUSCA_LVAL('MAQTREC', PZ.CODPOL)='1' THEN
      BEGIN
        SELECT COUNT(1) 
          INTO nCantReg
          FROM Maquinaria_Trec
         WHERE IdePol = PZ.IDEPOL;
      EXCEPTION WHEN OTHERS THEN
        nCantReg := 0;
      END;
      IF nCantReg > 0 THEN
        cValTrec := PR_API_COMISION_INTERMEDIARIO.FN_VALIDA_TREC(PZ.IDEPOL); -- FR_VALIDA_TREC(PZ.IdePol);
        IF NVL(cValTrec,'0') = '1' THEN 
           p_cError := '1,'||'El valor declarado en Datos Particulares no coincide con la sumatoria valores declarados Registro Maquinarias TREC';
           DBMS_OUTPUT.PUT_LINE('El valor declarado en Datos Particulares no coincide con la sumatoria valores declarados Registro Maquinarias TREC');
           RAISE_APPLICATION_ERROR(-20106,'El valor declarado en Datos Particulares no coincide con la sumatoria valores declarados Registro Maquinarias TREC');
        END IF;
      END IF;
    END IF;
    
    /*
     --------
     B02_1: POST-QUERY
     :B02_1.cNumTramite := PR_POLIZA_TRAMITE.ObtenerUltimoTramite(:B02_1.IdePol);
     --------
     IF :B02_1.cNumTramite IS NULL THEN 
      nNumTram := PR_POLIZA_TRAMITE.ObtenerUltimoTramite(:B01_1.IdePol);
     END IF;
    */
    nNumTram := PR_POLIZA_TRAMITE.ObtenerUltimoTramite(PZ.IDEPOL);
   
    BEGIN
      SELECT 1
      into  cExistTramit
      FROM DUAL 
      WHERE EXISTS (SELECT 1
                    FROM   POLIZA_TRAMITE PT
                    WHERE  PT.IDEPOL = PZ.IDEPOL
                      AND  NUMOPER  = 0 );
    EXCEPTION 
      WHEN OTHERS THEN
        cExistTramit :=0;
    END;
    
    IF nNumTram IS NULL THEN -- IF NVL(:B02_1.cNumTramite,nNumTram) IS NULL THEN
        cTextoTr := 'Confirma ACTIVAR la póliza sin Numero de Trámite';
        DBMS_OUTPUT.PUT_LINE(cTextoTr);
        PR_API_COMISION_INTERMEDIARIO.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;
        cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
        DBMS_OUTPUT.PUT_LINE(cTexto);
        IF PZ.STSPOL <> 'MOD' THEN
            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
            PR_API_COMISION_INTERMEDIARIO.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
        ELSE 
          cOperEnd := 'N';
          BEGIN 
            SELECT 'S' 
            INTO   cOperEnd
            FROM   dual
            WHERE EXISTS (SELECT 1
                          FROM   POLIZA P 
                          WHERE  P.IDEPOL = PZ.IDEPOL
                          AND    P.Stspol = 'MOD'
                          AND    EXISTS ( SELECT * 
                                          FROM   Mod_Cobert C
                                          WHERE  C.Idepol = P.Idepol
                                          AND    C.Stsmodcobert = 'INC'
                                          ));
          EXCEPTION 
              WHEN NO_DATA_FOUND THEN
                 cOperEnd := 'N';
          END;
          IF  cOperEnd = 'S' THEN
            /*
            GO_BLOCK('B07_1');
            CLEAR_BLOCK;
            :B07_1.TXT_EMI    := cTexto;
            :B07_1.CodProd    := :B01_1.CodProd;
            :B07_1.NumPol     := :B01_1.NumPol;
            :B07_1.FecIniVig  := :B01_1.FecIniVig;
            :B07_1.FecFinVig  := :B01_1.FecFinVig;
            */
            /*
             ---------------------------------------------------------------------------------
             .:.SE INVOCA AL FORMULARIO MOTIVOS DE ENDOSOS Y VIGENCIA DE OPERACIÓN.:.
             ----------------------------------------------------------------------------------
            */
            DBMS_OUTPUT.PUT_LINE(cTexto);
            PR_API_COMISION_INTERMEDIARIO.SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(PZ.IDEPOL,p_cTipoOper);
          ELSE

            SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;  
            PR_API_COMISION_INTERMEDIARIO.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);

          END IF;
        END IF;     

     
                    
    ELSE
 
      IF PR.EXISTE_LVAL('CCMPRODX',PZ.CODPROD) = 'S' THEN --Productos configurados para CCM
        BEGIN
          SELECT NUMTRAMITE 
          INTO   nNumTram
          FROM   POLIZA_TRAMITE PT
          WHERE  PT.IDEPOL = PZ.IdePol
            AND  NUMOPER  = 0 --:B02_1.cNumtramite,
            AND ROWNUM=1;
        EXCEPTION 
          WHEN OTHERS THEN 
            nNumTram := NULL;
        END;  
        --<F N3030980>27.10.2008
        --<I N3026404> Nelson Díaz EMI-02491
        BEGIN 
          SELECT DISTINCT TIPOOP,IDEPOL,NUMOPER ,1
          INTO cTipo, nIdepol, nNumOper,cCantOpera
          FROM oper_pol op
          WHERE op.numoper IN  (SELECT p.numoper 
                                FROM   poliza_tramite  p
                                WHERE  p.numtramite =  NVL(PR_POLIZA_TRAMITE.ObtenerUltimoTramite(PZ.IDEPOL),nNumTram)
                                  AND  p.numoper != 0)
          AND    FECANUL IS NULL ;
        EXCEPTION 
          WHEN NO_DATA_FOUND THEN 
            cCantOpera:=0;
          WHEN TOO_MANY_ROWS THEN
            cCantOpera:=2;
        END;
        IF cCantOpera = 0 THEN
          PR_API_COMISION_INTERMEDIARIO.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_VALIDAR_DATOS;
          cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
          DBMS_OUTPUT.PUT_LINE(cTexto);
          IF PZ.STSPOL <> 'MOD' THEN            
              cValidaTram := FR_VALIDA_TRAMITE(NVL(PR_POLIZA_TRAMITE.OBTENERULTIMOTRAMITE(PZ.IDEPOL),nNumTram));
              IF cValidaTram = 'N' THEN
                 DBMS_OUTPUT.PUT_LINE(cValidaTram);
                 RAISE_APPLICATION_ERROR(-20106,cValidaTram);
              END IF; 
    
              -- Si todo está correcto se continuamos con la actualización.
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT
              PR_API_COMISION_INTERMEDIARIO.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);  -- <N3028071> Giancarlo Ramirez - Optimizacion CNT                      
          END IF;
        ELSE
          -- nDummy := FR_ALERTA('El trámite '||NVL(:B02_1.cNumtramite,nNumTram)|| ' con el que se esta activando la póliza pertenece a otra operación '||cTipo||' , '||PZ.CODPROD||'-'||PZ.NUMPOL ||', NumOper '|| nNumOper);
          DBMS_OUTPUT.PUT_LINE('El trámite '|| ' con el que se esta activando la póliza pertenece a otra operación '||cTipo||' , '||PZ.CODPROD||'-'||PZ.NUMPOL ||', NumOper '|| nNumOper);
        END IF;

      ELSE 

          PR_API_COMISION_INTERMEDIARIO.SP_VALIDAR_DATOS(PZ.IDEPOL); -- FR_Validar_Datos;
          cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
          DBMS_OUTPUT.PUT_LINE(cTexto);
          IF PZ.STSPOL <> 'MOD' THEN
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
              PR_API_COMISION_INTERMEDIARIO.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
          ELSE 
              cOperEnd := 'N';
              BEGIN 
                SELECT 'S' 
                INTO   cOperEnd
                FROM   dual
                WHERE EXISTS (SELECT 1
                              FROM   POLIZA P 
                              WHERE  P.IDEPOL = PZ.IdePol
                              AND    P.Stspol = 'MOD'
                              AND    EXISTS ( SELECT * 
                                              FROM   Mod_Cobert C
                                              WHERE  C.Idepol = P.Idepol
                                              AND    C.Stsmodcobert = 'INC'
                                            ));
              EXCEPTION 
                WHEN NO_DATA_FOUND THEN
                    cOperEnd := 'N';
              END;
              IF  cOperEnd = 'S' THEN
                /*
                GO_BLOCK('B07_1');
                CLEAR_BLOCK;
                :B07_1.TXT_EMI    := cTexto;
                :B07_1.CodProd    := :B01_1.CodProd;
                :B07_1.NumPol     := :B01_1.NumPol;
                :B07_1.FecIniVig  := :B01_1.FecIniVig;
                :B07_1.FecFinVig  := :B01_1.FecFinVig;
                */
                /*
                ---------------------------------------------------------------------------------
                SE INVOCA AL FORMULARIO MOTIVOS DE ENDOSOS Y VIGENCIA DE OPERACIÓN
                ----------------------------------------------------------------------------------
                */
                PR_API_COMISION_INTERMEDIARIO.SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(PZ.IDEPOL,p_cTipoOper);
              ELSE

                PR_API_COMISION_INTERMEDIARIO.SP_VALIDAR_DATOS(PZ.IDEPOL); -- FR_Validar_Datos;
                PR_API_COMISION_INTERMEDIARIO.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);

              END IF;
     
      END IF; --F Confirmar trámite
            
    END IF;   

  END IF;
 
  --<I COR103-925> Werner Tito / 19-09-2023 / [PROYECTO TECH CORE] Integración con Satelites InsPAT
  -- Activa Garantias
  PR_API_COMISION_INTERMEDIARIO.SP_ACTIVA_GARANTIAS(nStsPOL, PZ.IDEPOL); -- FR_ACTIVA_GARANTIAS(nStsPOL);
  --<F COR103-925>            
 
  --<I RTC 316472> David Yupanqui / 27-04-2023 / [PROYECTO TECH CORE] Armado Automático del Anexo 7
  if pr.busca_lval('SLIP_AUT',PZ.CodPol) <> 'INVALIDO' then
    acselx.pr_slip_auto.texto_prima(PZ.IdePol);
  end if;
  --<F RTC 316472>
EXCEPTION
  WHEN OTHERS THEN 
    DBMS_OUTPUT.PUT_LINE(SQLERRM);
    RAISE_APPLICATION_ERROR(-20106,SQLERRM);

END SP_ACTUALIZAR_POLIZA;

-- FIN CLAUSULA C

END PR_API_COMISION_INTERMEDIARIO;
/