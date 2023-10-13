CREATE OR REPLACE PACKAGE BODY ACSELX.PR_API_COMISION_INTERMEDIARIO IS
  /*-----------------------------------------------------------------------------------
    Nombre : BUSCAR_COMISIONES
    Propósito: Retorna una lista de Comisiones que cumplen con un criterio determinado.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         Código de la Compañía
                P_cCODMONEDA IN VARCHAR2      Código de la moneda
                P_cTIPOCONSULTA IN VARCHAR2   Tipo de Consulta: Por cobrar(ACT),Cobrados(LIQ)
                P_cCODPROD IN VARCHAR2        Código del Producto
                P_cNUMPOL IN VARCHAR2         Número de la Póliza
                P_cFECINI IN VARCHAR2         Fecha inicial
                P_cFECFIN IN VARCHAR2         Fecha final
                P_nNUMIDCLIENTE IN NUMBER     Numid del Cliente
                P_nCANTIDAD IN NUMBER         Tipo de respuesta: Detalle(-1), Cantidad(0)
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaciòn
    01/10/2007       Guillermo Vergara      Modificación (R1.1.1.1)  Caso de Prima Nula. <N3028549>
    27/09/2007       Marko Chu              Modificacion <N3028514> para separar busqeudas por
                                            Comisiones y asesorías
    31/10/2007       Marko Chu              Modificacion <N3028741> para eliminar la columna
                                            FORMAPAGO en las funciones Buscar_Comisiones y
                                            Detalle_Preliquidación.
    12/08/2010       Carlos Riguetti        Create Procedimientos y Función Carlos Riguetti
                                            Requerimiento: DEFPROC-00171 Reserva: N3035540
    02/05/2011       Marlon Castro G.       Modificación <N3037392> Mejoras en Preliquidaciones
                                            Referencia Reqm CDC-00076
    08/05/2012       Marcos Gomez Jimenez   Modificacion en la obtencion de documentos de descarga
              en funcion al requerimiento CDC-00089  Reserva: N3038970.
    23/10/2012       Christian R.           CDC-00091 <N3039796>
    20/03/2013       Italo Rivas            <N3040669> CDC-00096
    23/01/2014       Jose Angeles           RTC 18505 Se agrega parametro para el Canal
    23/03/2015       Eddy Cordova           RTC 30769 Se modifica la busqueda de la prima neta
    13/06/2016       Alister Arnedo         RTC 71480 Suspensión de pago a Corredores por Norma SUSALUD 134-2015-SUSALUD/S Web-REL
    10/05/2017       Jose Angeles           RTC 100411 Optimización del modulo de Pre liquidación
    23/02/2018       Jose Angeles           RTC 125554 Se agrega validación de la suspensión del RTC 71480
    15/09/2018       Alister Arnedo         RTC 150595  Se agrega los siguientes estados de extorno “EXMO” y “REMO”.
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
         --<INI RTC 125554> - Jose Angeles - 23/02/2018 Se agrega validación de la suspensión del RTC 71480
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
    Propósito: Retorna la cantidad de Pre-liquidaciones pendientes de un Corredor.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         Código de la Compañía
                P_cCODMONEDA IN VARCHAR2      Código de la moneda
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaciòn
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
    Propósito: Crea una Pre-liquidación y retorna su código.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         Código de la Compañía
                P_cCODMONEDA IN VARCHAR2      Código de la moneda
                P_cFECINI IN NUMBER           Fecha inicial
                P_cFECFIN IN VARCHAR2         Fecha final
                P_nNUMPRELIQ OUT NUMBER       Código de la Pre-liquidación
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    19/09/2023      Robinzon Santana        Modificación para que no preliquide a los estado de suspencion de pago bloqueado(S)
                                            y si los desbloqueado(N)
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
      
      
      SELECT SQ_NUMPRELIQ.NEXTVAL
      INTO   W_NUMPRELIQ
      FROM   DUAL;

      IF P_cCODPROD is null and P_cNUMPOL is null  and P_nNUMIDCLIENTE is null  and (p_cCodCanal is null or p_cCodCanal='0') THEN

        UPDATE
               OPER_INTER OI
        SET    OI.INDPRELIQ = 'S',
               OI.NUMPRELIQ = W_NUMPRELIQ
        WHERE  OI.CODINTER = (SELECT I.CODINTER
                              FROM   INTERMEDIARIO I
                              WHERE  I.NUMID = P_nNUMIDINTER)
        AND    OI.CODCIA = P_cCODCIA
        AND    OI.CODMONEDA = P_cCODMONEDA
        AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
        AND    OI.STSOPERINTER = 'ACT'
        AND    OI.FOPERINTER BETWEEN TO_DATE(P_cFECINI,'YYYY-MM-DD')
                                 AND TO_DATE(P_cFECFIN,'YYYY-MM-DD')
        AND    OI.INDASESOR = p_cASES
        AND OI.INDPRELIQ = 'N'
        AND OI.NUMPRELIQ IS NULL
        AND 'N' =  (SELECT CASE WHEN (SELECT CASE WHEN OI.IDEFACT IS NOT NULL
                                          THEN PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_FCODPROD(OI.IDEFACT)
                                          ELSE PR_EDOCTA_INTERMEDIARIO_R1420.BUSCA_BCODPROD(OI.OBLIG_PRI)
                                END
                         FROM   DUAL) = '6101' THEN 
                (SELECT CASE WHEN (PR_POLIZA.F_OBTIENE_STATUS_SUSALUD@DBL_BDAPPPROD( 
                   (SELECT CASE WHEN OI.IDEFACT IS NOT NULL
                                          THEN (SELECT F.NUMID
                                                FROM   FACTURA F
                                                WHERE  F.IDEFACT = OI.IDEFACT)
                                          ELSE (SELECT O.NUMID
                                                FROM   OBLIGACION_TABLE O
                                                WHERE  O.NUMOBLIG = OI.OBLIG_PRI)
                                END
                         FROM   DUAL),
                                                              (SELECT max(T.NUMPOL) FROM   TRANSACRE T 
                                                              WHERE  T.NUMACRE IN (SELECT A.NUMACRE 
                                                              FROM   ACREENCIA A 
                                                              WHERE  A.IDEFACT = OI.IDEFACT)),
                                                              P_nNUMIDINTER,
                                                               1,
                                                               4,
                                                              'S')) = 'S' THEN 'N' ELSE 'S' END FROM DUAL)
                 ELSE 'N' END FROM DUAL )
        AND (ROWNUM <= p_nCantidadMax OR p_nCantidadMax is null);

      ELSE
        UPDATE
               OPER_INTER OI
        SET    OI.INDPRELIQ = 'S',
               OI.NUMPRELIQ = W_NUMPRELIQ
        WHERE  OI.CODINTER = (SELECT I.CODINTER
                              FROM   INTERMEDIARIO I
                              WHERE  I.NUMID = P_nNUMIDINTER)
        AND    OI.CODCIA = P_cCODCIA
        AND    OI.CODMONEDA = P_cCODMONEDA
        AND    OI.TIPOOPERINTER IN('COMI','EXTR','EXMO','REMO')
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
     
    EXCEPTION
      WHEN OTHERS THEN
             ROLLBACK;
             W_NUMPRELIQ := 0;
    END;

    P_nNUMPRELIQ := W_NUMPRELIQ;
  END CREA_PRELIQUIDACION;

  /*-----------------------------------------------------------------------------------
    Nombre : DETALLE_PRELIQUIDACION
    Propósito: Dado un código de pre-liquidación, presenta el detalle correspondiente.
    Referencia : Proyecto PR-00004
    Parametros :
                P_nNUMPRELIQ IN NUMBER       Código de la Pre-liquidación
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    22/08/2007       Guillermo Vergara      Creaciòn
    27/09/2007       Jagner Hinostroza      Modificacion  <N3028514> Modificaion para separar
                                            Comisiones y asesorias
    02/05/2011       Marlon Castro G.       Modificación <N3037392> Mejoras en Preliquidaciones
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
    Propósito: Crea una Pre-liquidación y retorna su código.
    Referencia : Proyecto Portal
    Parametros :
                P_nNUMIDINTER IN NUMBER       Numid del Corredor
                P_cCODCIA IN VARCHAR2         Código de la Compañía
                P_cCODMONEDA IN VARCHAR2      Código de la moneda
                P_cFECINI IN NUMBER           Fecha inicial
                P_cFECFIN IN VARCHAR2         Fecha final
                p_cASES IN VARCHAR2           Identificador de Comisiones (N) y asesorías (S)
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    21/12/2007       Jose Porlles C.        Creación
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
    Propósito: Anula una preliquidacion, liberando sus operaciones asignadas
    Referencia : CDC-00054
    Parametros :
                P_nNUMPRELIQ IN NUMBER       Código de la Pre-liquidación

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10-11-2009       Marko Chu               Creaciòn <N3033740>

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
    Propósito:  Retorna el estado de una preliquidacion
                El estado puede ser :
                  ACT en caso tenga OPER_INTER no pagadas
                  COB en caso todas las oper inters esten pagadas
                  ANU si no existe la preliquidacion

    Referencia : CDC-00054
    Parametros :
                P_nNUMPRELIQ IN NUMBER       Código de la Pre-liquidación

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10-11-2009       Marko Chu               Creaciòn <N3033740>

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
    Propósito: Anula una preliquidacion, liberando sus operaciones asignadas
    Referencia : CDC-00054
    Parametros :
                P_nNUMIDINTER     NUMID del Intermediario
                P_cFECINI         Fecha inicial del rango de busqueda en formato YYYY-MM-DD
                P_cFECFIN         Fecha final del rango de busqueda en formato YYYY-MM-DD

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10-11-2009       Marko Chu               Creaciòn <N3034247>
    06-09-2022       David Yupanqui          Modificacion <RTC 314968>
    13-12-2022       Marcopolo Quispe H.     Modificacion <RTC 315747> CORRECTIVO Visualización de pre liquidaciones con antiguedad menor a 1 año
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
      and op.tipooperinter not in ('COMI','EXTR','EXMO','REMO')) --<RTC 315747> Marcopolo Quispe H. / 13-12-2022 / CORRECTIVO Visualización de pre liquidaciones con antiguedad menor a 1 año
    group by oi.numpreliq, oi.codmoneda, oi.codcia, oi.indasesor
    order by oi.numpreliq;    

      RETURN TRANSFER_CUR;

  END BUSCA_PRELIQ_PENDIENTES;

  -- Create Carlos Riguetti - Requerimiento: DEFPROC-00171 - Reserva: N3035540
  --
  --   05/10/2010  Heli rojas HelpDesk N° 87108
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
      --<I N3035773> Katia León/10-09-2010/Hallar Código de Intermediario
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

    IF nNumLiq IS NOT NULL OR nNumOblig IS NOT NULL THEN  --<I N3035773> Katia León/10-09-2010/Se adiciona IF y QUERY- Optimización de tiempo de respuesta
      OPEN TRANSFER_CUR FOR
      SELECT /*+ CHOOSE*/ O.CODCIA CODCIA,
             DECODE(O.CODCIA,'01','RIMAC SEGUROS','RIMAC EPS') COMPANIA,
             DECODE(L.FECABONOBCO,NULL,'EN PROCESO DE ABONO','ABONADO') ESTADO,
             --O.CODINTER   NROCORREDOR,  HelpDesk N° 87108
             p_nNumIdInter NROCORREDOR, --HelpDesk N° 87108
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
            --O.CODINTER   NROCORREDOR,  HelpDesk N° 87108
           p_nNumIdInter NROCORREDOR, --HelpDesk N° 87108
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
      WHERE O.FECHA_LIQUIDACION  >=  dFecIni --<N3035773> Katia León/13-09-2010/optimizacion de query
        AND O.FECHA_LIQUIDACION   <  dFecFin --<N3035773> Katia León/13-09-2010/optimizacion de query
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
    Propósito: Registra el numero de preliquidación, fecha, usuario y origen
    Referencia : CDC-00068
    Parametros :
                P_nNUMPRELIQ  IN NUMBER       Número de Preliquidación
                P_cCODUSER    IN VARCHAR2     Código del usuario
                P_nNUMIDUSER  IN VARCHAR2     Numid  del usuario
                P_nNUMIDINTER IN VARCHAR2     Numid  del corredor
                P_cORIGEN     IN VARCHAR2     origen de la preliquidación [W] Web / [A] ACSELX
                P_cTIPOPRELIQ IN VARCHAR2     Tipo de Preliquidación [COM] Comisiones / [ASE] Asesorías
                P_CODCIA      IN VARCHAR2     Código de la compañía

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    14/10/2010       Marlon Castro G.        Creación <N3035972>
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
    Propósito: Obtiene la fecha de creación de una preliquidación
    Referencia : CDC-00068
    Parametros :
                P_nNUMPRELIQ  IN   NUMBER       Número de Preliquidación
                P_dFecCrea  OUT  VARCHAR2       Fecha de Creación formateada
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    14/10/2010       Marlon Castro G.        Creación <N3035972>
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
    Propósito    : Obtiene el texto para el pie de pagina de un documento
    Referencia   : COM-00736
    Parametros   :
                p_cCodCia     IN VARCHAR2   Codigo de Compania
                p_cIndAse     IN VARCHAR2   Indicador de Asesoria
                p_cTipoDoc    IN VARCHAR2   Tipo de Documento PRELIQ[01], LIQ[02]
                P_cTexto      OUT VARCHAR2  Texto de salida
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    31/03/2011       Marlon Castro G.       Creación <N3036861>
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
    Propósito    : Obtiene un listado de NumOperInter
    Referencia   : CDC-00076
    Parametros   :
              p_nNumOper    IN NUMBER          Numero de Operacion,
              p_nIdeFact    IN NUMBER          Ide de Factura,
              p_cFecOper    IN VARCHAR2        Fecha de Operacion,
              p_cTipoOper   IN VARCHAR2        Tipo de Operacion,
              p_cIndAsesor  IN VARCHAR2        Indicador de asesoria,
              p_cCodCia     IN VARCHAR2        Codigo de compañia,
              p_cStsOper    IN VARCHAR2        Estado de Operacion,
              p_nCodInterm  IN NUMBER          Codigo de Intermediario
              p_nNumOperInter  IN NUMBER       Numero Maximo de Oper Inter

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    04/05/2011       Marlon Castro G.       Creación <N3037392>
    08/08/2017       Wilmer Ponce de Leon   Modificación <RTC 108953>
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
    Propósito    : Obtiene un numero de preliquidacion
    Referencia   : CDC-00076
    Parametros   :
                   p_nNumPreliq  OUT      Nuevo numero de preliquidacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    04/05/2011       Marlon Castro G.       Creación <N3037392>
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
    Propósito    : Crea una nueva preliquidacion a partir de la comisiones dadas
    Referencia   : CDC-00076
    Parametros   :
                   p_nNumPreliq     IN NUMBER      Nuevo numero de preliquidacion
                   p_nNumOperInter  IN NUMBER      Id de la tabla OPER_INTER

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    04/05/2011       Marlon Castro G.       Creación <N3037392>
    15/09/2018       Alister Arnedo         RTC 150595  Se agrega los siguientes estados de extorno “EXMO” y “REMO”.
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
    Propósito    : Obtiene el la operación de la comisión
    Referencia   : RTC 18505
    Parametros   :       
              p_nIdeFact    IN NUMBER          Ide de Factura,
              P_nNUMOBLIG   IN NUMBER          Numero de Obligacion,
              p_nNumOper    IN NUMBER          Numero de Operacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    26/01/2014       Jose Angeles           Creación 
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
    Propósito    : Obtiene la prima neta total
    Referencia   : RTC 30769
    Parametros   :       
              p_nNumOper    IN NUMBER          Numero de Operacion

    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    10/03/2015       Eddy Cordova           Creación 
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
  04/11/2015  Jose Angeles        Creación.
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
                p_CodProd   -> Código de Producto.
                p_NumPol    -> Nro. de Póliza.

  Log de Cambios
  Fecha       Autor                   Descripcion
  07/06/2016  Gabriel Díaz Diez       Creación. <RTC 71288> - Suspensión de pago a Corredores por Norma SUSALUD 134-2015-SUSALUDS - AX.
  ----------------------------------------------------------------------------------------------------- */
  FUNCTION FN_TRAESUSPEN_RS (p_NumIdAseg TERCERO.NUMID%TYPE,
                             p_NumIdBrok TERCERO.NUMID%TYPE,
                             p_CodProd   POLIZA.CODPROD%TYPE,
                             p_NumPol    VARCHAR2) RETURN VARCHAR2 IS

  cRpta VARCHAR2(1) := PR.BUSCA_LVAL('SUSRSCO', 'VALNEG');
  BEGIN
    IF INSTR(PR.BUSCA_LVAL('SUSRSCO', 'CODPROD'), p_CodProd) != 0 THEN
      --Función que verifica en RimacSalud
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
   Propósito    : Valida Bloqueo por la SBS
   Referencia   : RTC 82636
   Parametros   :       
             p_nNumId    IN NUMBER          Número de Identificacion Unico del corredor
             p_ccodCia   IN VARCHAR2        Código de Compañia

   Log de Cambios
   Fecha            Autor                  Descripcion
   ----------       -----------------      -------------------------------------------
   20/10/2016       José Delgado           <RTC-82636> Creación
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
   Propósito    : Valida Otros Bloqueos del Intermediario
   Referencia   : RTC 82636
   Parametros   :       
             p_nNumId    IN NUMBER          Número de Identificacion Unico del corredor
   Log de Cambios
   Fecha            Autor                  Descripcion
   ----------       -----------------      -------------------------------------------
   20/10/2016       José Delgado           <RTC-82636> Creación
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
    Propósito: Dado un código de pre-liquidación, presenta el detalle correspondiente solo con los campos necesarios para generar pdf de liquidación.
    Referencia : Requerimiento 100411
    Parametros :
                P_nNUMPRELIQ IN NUMBER       Código de la Pre-liquidación
    Log de Cambios
    Fecha            Autor                  Descripcion
    ----------       -----------------      -------------------------------------------
    12/05/2017       Jose Angeles      Creaciòn
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

END PR_API_COMISION_INTERMEDIARIO;
/
