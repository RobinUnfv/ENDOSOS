CREATE OR REPLACE PACKAGE BODY PR_API_EMI_TRAN IS
/*---------------------------------------------------------------------------------
    Nombre     : BUSCAR_POLIZA_CERT_RAMO
    Proposito  : Devuelve lista de certificados
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_cCodPar  Identifica el grupo de par?metros pasados a la funci?n
                  p_cCodPol  C?digo de p?liza
                  p_nNumPol  N?mero de p?liza
                  p_nNumCer  N?mero de certificado
                  p_cTipDoc  Tipo de documento
                  p_cNumDoc  N?mero de documento
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           05/50/2004    Roberto Carranza    Optimizaci?n de c?digo para rol 0002
           10/05/2004    Roberto Carranza    Se agreg? el campo IndProceso
           14/07/2004    Miguel Wilson       Condicionar m?s de una relaci?n laboral
           10/10/2006    Nelson D?az         N3026088  Emi-02511, Nueva funcion
       02/09/2020  Joseph Fajardo    <RTC 266133> - Compilaci?n de Objetos con caracteres extra?os de AX
  ---------------------------------------------------------------------------------*/
  FUNCTION POLIZA_CERT_RAMO (p_cCodTra  VARCHAR2,
                             p_cUsrLog  VARCHAR2,
                             p_nUsrId   NUMBER,
                             p_cUsrRol  VARCHAR2,
                             p_cUsrPer  VARCHAR2,
                             p_cCodPar  VARCHAR2,
                             p_cCodPol  VARCHAR2,
                             p_nNumPol  NUMBER,
                             p_nNumCer  NUMBER,
                             p_cTipDoc  VARCHAR2,
                             p_cNumDoc  VARCHAR2) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
  --<I N3019624> Miguel Wilson / 14-07-2004 / Condicionar m?s de una relaci?n laboral del intermediario o cliente
   cCod          VARCHAR2(1000);
   --<F N3019624>
   cCodCli       CLIENTE.codcli%TYPE;
   --
   nIdePol       NUMBER;
   --
   cWhere        VARCHAR2(1000);
   cWhere2       VARCHAR2(1000);
   --
   BEGIN
    IF Valida_Parametros_Principales(p_cCodTra,p_cUsrLog,p_nUsrId,p_cUsrRol,p_cUsrPer) THEN
       --
       cWhere := 'SELECT p.numpol,p.idepol,p.stspol,p.indfacult,';
       cWhere := cWhere || 'PR_IMPRIME_POLIZA.nombre_persona(' || '''' ||'C'|| '''' ||', P.codcli) NomCliente,';
       cWhere := cWhere || 'c.numcert,c.desccert,c.fecing,c.fecfin,c.stscert,r.codramocert,p.codmoneda,PR_API_EMI_TRAN.indicador_proceso(P.IdePol,C.NumCert) IndProceso';
       cWhere := cWhere || '  FROM poliza p,certificado c,cert_ramo r ';
       cWhere := cWhere || ' WHERE p.idepol = c.idepol';
       cWhere := cWhere || '   AND r.idepol = c.idepol';
       cWhere := cWhere || '   AND r.numcert = c.numcert';
       cWhere := cWhere || '   AND r.codramocert = ' || '''' ||'TRAN'||'''';
       cWhere := cWhere || '   AND p.codprod = ' || '''' ||'3001'||'''';
       --
       cCod := PR_API_EMI_TRAN.Codigo_Cli_Ini(p_cUsrRol,p_nUsrId);
       --
       IF UPPER(p_cCodPar) = 'P_POL_CERT' THEN
          -- <I N3019132> Ricardo Larrabure / 02-07-2004 / Reemplazo b?squeda por CodProd y no CodPol
          cWhere := cWhere || ' AND p.codprod = ' || '''' || p_cCodpol || '''';
          -- <F N3019132>
          cWhere := cWhere || ' AND p.numpol = ' || p_nNumPol;
          --
          IF p_nNumCer IS NOT NULL THEN
             cWhere := cWhere || ' AND c.numcert = ' || p_nNumCer;
          END IF;
          --
       ELSIF UPPER(p_cCodPar) = 'P_RUC' THEN
             BEGIN
               SELECT b.codcli
                 INTO cCodCli
                 FROM docs_identidad a, cliente b
                WHERE a.tipoid    = b.tipoid
                  AND a.numid     = b.numid
                  AND a.dvid      = b.dvid
                  AND a.NumIdDoc  = p_cNumDoc
                  AND a.TipoIdDoc = p_cTipDoc;
             EXCEPTION
                WHEN OTHERS THEN
                     cCodCli := NULL;
             END;
             --
             cWhere := cWhere || ' AND p.codcli = ' || '''' || cCodCli || '''';
             --
       END IF;
       --
       IF p_cUsrRol = '0002' THEN
          cWhere := cWhere || ' AND EXISTS (SELECT ''X'' FROM part_inter_pol n WHERE N.idepol = P.Idepol AND n.codinter IN ' || cCod || ')'; --< N3019624 > Considerar mas de un cliente
       ELSIF p_cUsrRol = '0001' AND p_cCodPar = 'P_POL_CERT' THEN
             cWhere := cWhere || ' AND EXISTS (SELECT ''X'' FROM POLIZA_CLIENTE T WHERE T.IdePol = P.IdePol AND T.CodCli IN '|| cCod || ')'; --< N3019624 > Considerar mas de un cliente
       END IF;
       --
    ELSE
       RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
    END IF;
    --
    OPEN c_Certificado FOR cWhere;
    RETURN (c_Certificado);
   END POLIZA_CERT_RAMO;
   /*-----------------------------------------------------------------------
    Nombre     : LISTA APLICACIONES
    Proposito  : Devuelve Lista de Aplicaciones (Declaraciones)
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_cCodPar  Identifica los par?metros pasados a la funci?n
                  p_nNumPol  N?mero de p?liza
                  p_nNumCer  N?mero de certificado
                  p_nNumPla  N?mero de planilla
                  p_nNumCar  N?mero de carta
                  p_nNumLiq  N?mero de liquidaci?n
                  p_dFecIni  Fecha de registro inicio
                  p_dFecFin  Fecha de registro fin
                  p_cNumTra  N?mero de tr?mite
                  p_cTipDoc  Tipo de documento del cliente
                  p_cNumDoc  N?mero de documento
                  p_cApePat  Apellido Paterno del asegurado
                  p_cApeMat  Apellido Materno del asegurado
                  p_cNomAse  Nombre del asegurado
                  p_cIndOri  Indicador de sistema de origen
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           18/05/2004    Roberto Carranza    Se corrigi? la forma de invocar los
                                             datos en base a una liquidaci?n
           14/07/2004    Miguel Wilson       Condicionar m?s de una relaci?n laboral
           03/08/2004    Ricardo Larrabure   Cambio de nombre de asegurado por la
                                             funci?n respectiva (N3019780)
           28/09/2005    Jose Porlles        Cambio de asegurado por el responsable
                                             de pago seleccionado <N3023360>
           25/08/2006    Jorge Diaz          Agregar columnas en el resultado de la
                                             consulta generada. <N3025601>
           25/09/2006    Jorge Diaz          Cambiar la suma asegurada a devolver, ahora
                                             devuelve la suma con la tasa de sobreseguro
                                             aplicada. <N3025977>
           03/08/2010    Randy Alderete      Se adiciono restricci?n para la extracci?n
                       de aplicaciones por criterio de P_PLANILLA_ESTADO
                                             aplicada. <N3035448>
           23/04/2012    Ronald Or?          HelpDesk 104893 <N3038797>
     17/05/2012   Italo Rivas          Modificaciones TRANS-00031 <N3038987>
           17/09/2012   Italo Rivas          Modificaciones TRANS-00033 <N3039670>
           05/02/2013   Christian R.         Modificaciones WRK-01213 <N3040337>
           02/04/2013  Jhonatan Rojas T.     Modificaciones HD-117200 <N3040849>
           10/05/2013  Renzo Yacarini        <N3040860> HD 117200
           30/05/2013  Renzo Yacarini        <N3041051> HD 117806
           22/01/2021  Carlos Veliz          Modificaciones Req. RTC - 284065
   -------------------------------------------------------------------------*/
   FUNCTION LISTA_APLICACIONES(p_cCodTra VARCHAR2,  -- 1
                               p_cUsrLog VARCHAR2,  -- 2
                               p_nUsrId  NUMBER,    -- 3
                               p_cUsrRol VARCHAR2,  -- 4
                               p_cUsrPer VARCHAR2,  -- 5
                               p_cCodPar VARCHAR2,  -- 6
                               p_nNumPol NUMBER,    -- 7
                               p_nNumCer NUMBER,    -- 8
                               p_cNumPla VARCHAR2,  -- 9
                               p_nNumCar NUMBER,    -- 10
                               p_cNumLiq VARCHAR2,  -- 11
                               p_dFecIni VARCHAR2,  -- 12
                               p_dFecFin VARCHAR2,  -- 13
                               p_cNumTra VARCHAR2,  -- 14
                               p_cTipDoc VARCHAR2,  -- 15
                               p_cNumDoc VARCHAR2,  -- 16
                               p_cApePat VARCHAR2,  -- 17
                               p_cApeMat VARCHAR2,  -- 18
                               p_cNomAse VARCHAR2,  -- 19
                               p_cIndOri VARCHAR2,  -- 20
                               p_nIdePoL NUMBER,    -- 21
                               p_cStsDec VARCHAR2   -- 22
                               ) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   cWhere        VARCHAR2(2500); --<N3026001> Se aument? el tama?o de la variable
   cWhere1       VARCHAR2(2500); --<N3026001> Se aument? el tama?o de la variable
   cWhere2       VARCHAR2(2500) := ''; --<N3026001> Se aument? el tama?o de la variable
   cIndOri       VARCHAR2(1);
   --
   cCodCli       POLIZA.codcli%TYPE;
   --<I N3019624> Miguel Wilson / 14-07-2004 / Condicionar a m?s de un intermediario o cliente
   cCod          VARCHAR2(1000);
   --<F N3019624>
   xTipoId       DOCS_IDENTIDAD.tipoid%TYPE;
   xNumId        DOCS_IDENTIDAD.numid%TYPE;
   xDvId         DOCS_IDENTIDAD.Dvid%TYPE;
   nIdeRec       DEC_TRANSPORTE_FLOTANTE.Iderec%TYPE;

   cIdePol       VARCHAR2(200); -- <N3040860> HD 117200 / Renzo Yacarini / 13-05-2013

   --
   BEGIN
    --
    -- <I N3040860> HD 117200 / Renzo Yacarini / 13-05-2013
    IF p_cCodPar = 'P_POL_CERT' THEN
      FOR CC IN (SELECT PO.IDEPOL
                 FROM POLIZA PO
                 WHERE PO.CODPROD = '3001'
                  AND PO.NUMPOL = p_nNumPol) LOOP
         IF cIdePol IS NULL THEN
           cIdePol := CC.IDEPOL;
         ELSE
           cIdePol := cIdePol||','||CC.IDEPOL;
         END IF;
      END LOOP;
    END IF;

    IF cIdePol is null then
      cIdePol := -1;
    END IF;
    -- <F N3040860> HD 117200 / Renzo Yacarini / 13-05-2013


    cCod    := PR_API_EMI_TRAN.Codigo_Cli_Ini(p_cUsrRol,p_nUsrId);
    cIndOri := p_cIndOri;
    cWhere := 'SELECT /*+ choose */ D.idedec,D.aplicacion,D.planilla,P.numpol,C.numcert,D.numcarta,';
    cWhere := cWhere || 'NVL(PR_IMPRIME_POLIZA.nombre_persona(''C'', D.CodCli),PR_API_EMI_TRAN.Nombre_Asegurado(D.IdePol,D.NumCert,D.IdeRec,''A''))  NomAsegurado, ';-- <N3023360> Jose Porlles / 28-09-05 / Reemplazo de consulta de asegurado
    cWhere := cWhere || 'PR_UBIGEO.BUSCA_PAIS(D.codpais) Descpais, ';
    cWhere := cWhere || 'D.descmercancia DescMerc,D.primamoneda,D.mtoliqmoneda SumAsegMoneda,D.fecfinvalid,';
    --<I N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    --cWhere := cWhere || 'PR_API_EMI_TRAN.NUMERO_LIQUIDACION(D.IdeRec) NumFact,D.stsdec,D.IdePol,D.IndPolitica, ';
    cWhere := cWhere || 'NVL(DA.NUMLIQ, PR_API_EMI_TRAN.NUMERO_LIQUIDACION(D.IdeRec)) NumFact,D.stsdec,D.IdePol,D.IndPolitica, '; -- <N3041051> HD 117806 / Renzo Yacarini / 30-05-2013
    --<F N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    cWhere := cWhere || 'PR_API_EMI_TRAN.INDICADOR_PROCESO(P.IdePol,C.NumCert) IndProceso, D.NumTramite NumTramite, ';
    --<I N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    --cWhere := cWhere || 'PR_API_EMI_TRAN.FECHA_EMISION(D.IdeDec) Fecha_Operacion ';
    cWhere := cWhere || 'NVL(DA.FECEMI, PR_API_EMI_TRAN.FECHA_EMISION(D.IdeDec))  Fecha_Operacion '; -- <N3041051> HD 117806 / Renzo Yacarini / 30-05-2013
    --<F N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
   -- <I N3025601> Jorge Diaz/ 09-08-2006 / Aumentar Columnas del resultado
    cWhere := cWhere || ', pr.busca_lval(''TIPTRAN'', D.TipoTransporte) , pr.busca_lval(''TIPOMERC'', D.TipoMercancia), pr.busca_lval(''TIPOEMBA'', D.TipoEmbalaje), D.MtoDerAduana,';
     -- <I N3025977> Jorge Diaz/ 25-09-2006 / Devolver la suma asegurada con la tasa de sobreseguro aplicada
    cWhere := cWhere || ' ROUND(D.MtoLiqMoneda * (1 + D.sobreseg / 100), 2) SumaAseguradaConSobreseguro';
     -- <F N3025977>
   -- <F N3025601>
   -- <I N3038987> TRANS-00031 / Italo Rivas / 17-05-2012
  -- <I N3039670> TRANS-00033 / Italo Rivas / 17-09-2012 -- Se comenta c?digo
    /*cWhere := cWhere || ', PR_UBIGEO.BUSCA_PAIS(D.codpais) DescPaisOrigen, PR_UBIGEO.BUSCA_PAIS(D.Codpaisdest) DescPaisDestino,
                         D.Desctran DescNave, D.Numpedido NroPedido, D.BaseAvaluo BaseAvaluo,
                         (D.Mtoliqmoneda * (D.Sobreseg/100)) Sobreseguro';*/
    cWhere := cWhere || ' , PR_UBIGEO.BUSCA_PAIS(D.codpais) DescPaisOrigen, PR_UBIGEO.BUSCA_PAIS(D.Codpaisdest) DescPaisDestino,
                            D.DescNave, D.Numfact, D.Numpedido, D.Mtoliqmoneda montoDeclaracion,
                         (D.Mtoliqmoneda * (D.Sobreseg/100)) Sobreseguro';
    --<I N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    --<I N3040849> HD-117200 / Jhonatan Rojas T. / 02/04/2013
    --<I> Modificaciones Req. RTC - 284065 / Carlos Veliz / 22-01-2021
    --cWhere := cWhere || ' ,PR_API_EMI_TRAN.OBTENER_DERECHO_EMISION(D.idedec) ';
    --cWhere := cWhere || ' ,NVL(DA.deremi, PR_API_EMI_TRAN.OBTENER_DERECHO_EMISION(D.idedec)) deremi '; -- <N3041051> HD 117806 / Renzo Yacarini / 30-05-2013
    cWhere := cWhere || ' ,NVL(NVL(DA.deremi, PR_API_EMI_TRAN.OBTENER_DERECHO_EMISION(D.idedec)), 0) deremi ';
    --<F> Modificaciones Req. RTC - 284065 / Carlos Veliz / 22-01-2021
    --<F N3040849> HD-117200 / Jhonatan Rojas T. / 02/04/2013
    --<F N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
  -- <F N3039670> TRANS-00033 / Italo Rivas / 17-09-2012
   -- <F N3038987> TRANS-00031 / Italo Rivas / 17-05-2012
    cWhere := cWhere || ' FROM DEC_TRANSPORTE_FLOTANTE D, POLIZA P, CERTIFICADO C ';--, PART_INTER_POL I '; -- <N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
    cWhere := cWhere || ' ,DEC_TRANSPORTE_FLOTANTE_ADIC DA '; -- <N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    cWhere := cWhere || ' WHERE D.idepol = C.idepol AND D.numcert = C.numcert ';
    cWhere := cWhere || '   AND P.idepol = C.idepol ';-- AND P.IdePol  = I.Idepol(+) '; -- <N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
    cWhere := cWhere || '   AND D.IDEDEC = DA.IDEDEC(+)'; -- <N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    cWhere := cWhere || '   and exists (select 1 from PART_INTER_POL I where P.IdePol = I.Idepol(+) '; -- <N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
    --
    IF p_cUsrRol = '0001' THEN
       cWhere := cWhere || ') AND p.codcli IN ' || cCod ; --< N3019624 > Conciderar mas de un cliente -- <N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
    ELSIF p_cUsrRol = '0002' THEN
         cWhere  := cWhere || ' AND I.codinter IN ' || cCod ||' ) ';--< N3019624 > Conciderar mas de un cliente -- <N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
    --<I N3038797>  Ronald Or? - 23/04/2012 - HelpDesk 104893
    ELSIF p_cUsrRol = '0003' THEN
         cWhere  := cWhere || ' AND I.INDLIDER =''S'' )'  ;       -- <N3040860> HD 117200/ Renzo Yacarini / 10-05-2013
    --<F N3038797>  Ronald Or? - 23/04/2012 - HelpDesk 104893
    END IF;
    --
    IF p_cCodPar = 'P_IDEPOL_CERT' THEN
       cWhere := cWhere || ' AND D.Idepol  = ' || nvl(p_nIdePol,0);
       IF p_nNumcer IS NOT NULL THEN
          cWhere := cWhere || ' AND D.Numcert = ' || nvl(p_nNumCer,0);
       END IF;
    ELSIF p_cCodPar = 'P_POL_CERT' THEN
          --<I N3040860> HD 117200 / Renzo Yacarini / 13-05-2013
          --cWhere := cWhere  || ' AND P.numpol  = ' || nvl(p_nNumPol,0);
          cWhere := cWhere || ' AND P.IDEPOL  IN (' || cIdePol || ')';
          --<F N3040860> HD 117200 / Renzo Yacarini / 13-05-2013
          IF p_nNumCer IS NOT NULL THEN
             cWhere := cWhere  || ' AND C.numcert = ' || nvl(p_nNumCer,0);
          END IF;
    ELSIF p_cCodPar = 'P_LIQUIDACION' THEN
          cWhere := cWhere || ' AND EXISTS (SELECT ''X'' FROM Factura F, Recibo R ';
          cWhere := cWhere || ' WHERE F.Numoper = R.Ideop AND F.NumFact = ' || p_cNumLiq;
          cWhere := cWhere || ' AND R.Iderec  = D.Iderec)';
          --
/*          BEGIN
            SELECT R.Iderec
              INTO nIdeRec
              FROM Factura F, Recibo R
             WHERE F.Numoper = R.Ideop
               AND F.NumFact = p_cNumLiq;
          EXCEPTION
             WHEN OTHERS THEN
                  nIdeRec := NULL;
          END;
          --
          cWhere := cWhere  || ' AND D.IdeRec = '|| NVL(nIdeRec,0);*/
    ELSIF p_cCodPar = 'P_CARTA' THEN
          cWhere := cWhere || ' AND D.numcarta = ' || nvl(p_nNumCar,0);
    ELSIF p_cCodPar = 'P_PLANILLA' THEN
          cWhere := cWhere || ' AND D.planilla = ' || '''' || p_cNumPla || '''';
    ELSIF p_cCodPar = 'P_PLANILLA_ESTADO' THEN
          cWhere := cWhere || ' AND D.Idepol  = ' || nvl(p_nIdePol,0);--> RLPW
          cWhere := cWhere || ' AND D.planilla = ''' || p_cNumPla || '''';
          cWhere := cWhere || ' AND D.stsdec   = ''' || p_cStsDec || '''';
          --<I N3035448> Randy Alderete/ 03-08-2010
    IF p_cNumLiq IS NOT NULL THEN
    cWhere := cWhere || ' AND nvl(DA.numliq, PR_API_EMI_TRAN.NUMERO_LIQUIDACION(D.IdeRec)) = ''' || nvl(p_cNumLiq,0) || ''''; -- || ' AND PR_API_EMI_TRAN.NUMERO_LIQUIDACION(D.IdeRec) = ''' || nvl(p_cNumLiq,0) || ''''; -- <N3040860> HD 117200 / Renzo Yacarini / 10-05-2013  -- <N3041051> HD 117806 / Renzo Yacarini / 30-05-2013
    END IF;
          --<F N3035448> Randy Alderete/ 03-08-2010
    ELSIF p_cCodPar = 'P_TRAMITE' THEN
    -- <I N3040337> WRK-01213 / Christian R. / 17-01-2013
        IF p_cUsrRol = '0001' OR p_cUsrRol = '0002' THEN
        cWhere := cWhere || ' AND D.NUMTRAMITE=' || '''' || p_cNumTra || '''' || '';
        ELSE
    -- <F N3040337> WRK-01213 / Christian R. / 17-01-2013
          cWhere := cWhere || ' AND (EXISTS (SELECT ''X'' FROM poliza_tramite TR WHERE TR.idepol = P.Idepol AND TR.NumTramite = ' || '''' || p_cNumTra || '''' || '))' ;
        END IF;   -- <Modificacion N3040337> WRK-01213 / Christian R. / 17-01-2013

    END IF;
    -- Hasta aqui OK
    IF p_cCodPar IN ('P_ASEGURADO','P_CORREDOR') THEN
       --  Esto es comun para Asegurado y Corredor
       IF p_cNomAse IS NOT NULL THEN
          cWhere2 := cWhere2 || ' AND T.NomTer LIKE ' || '''' || p_cNomAse || '%' ||'''';
       END IF;
       --
       IF p_cApePat IS NOT NULL THEN
          cWhere2 := cWhere2 || ' AND T.apeter LIKE ' || '''' || p_cApePat || '%' ||'''';
       END IF;
       --
       IF p_cApeMat IS NOT NULL THEN
          cWhere2 := cWhere2 || ' AND T.apematter LIKE ' || '''' || p_cApeMat || '%' ||'''';
       END IF;
       -- Si busca datos del asegurado
       cWhere1 := 'SELECT ';
       IF p_cCodPar = 'P_ASEGURADO' THEN
          cWhere1 := cWhere1 || 'C.codcli FROM Tercero T, Cliente C, Docs_Identidad D ';
       ELSIF p_cCodPar = 'P_CORREDOR' THEN
          cWhere1 := cWhere1 || 'C.codinter FROM Tercero T, Intermediario C, Docs_Identidad D ';
       END IF;
       --
       cWhere1 := cWhere1 || 'WHERE T.tipoid = C.tipoid AND T.numid = C.numid AND T.dvid = C.dvid ';
       cWhere1 := cWhere1 || '  AND C.tipoid = D.tipoid AND C.numid = D.numid AND C.dvid = D.dvid ';
       --
       IF p_cNumDoc IS NOT NULL THEN
          cWhere1 := cWhere1 || ' AND D.tipoiddoc = 1 AND D.numiddoc  = ' || '''' || p_cNumDoc || '''';
       END IF;
       --
       cWhere1 := cWhere1 || cWhere2;
       IF p_cCodPar = 'P_CORREDOR' THEN
          cWhere := cWhere || ' AND  I.CodInter IN ';
       ELSIF p_cCodPar = 'P_ASEGURADO' THEN
          cWhere := cWhere || ' AND  P.CodCli IN ';
       END IF;
       cWhere  := cWhere  || '(' || cWhere1 || ')';
    END IF;
    -- El rango de fechas solo se incluye cuando no tiene estos parametros (OK)
    IF p_cCodPar NOT IN ('P_IDEPOL_CERT','P_PLANILLA_ESTADO') THEN
       --
       cWhere := cWhere || ' AND EXISTS (SELECT ''X'' FROM dec_transporte_flotante W,recibo X, operacion Y ';
       cWhere := cWhere || ' WHERE W.IdeRec = X.IdeRec AND X.ideop  = Y.ideop AND W.IdeDec = D.IdeDec ';
       cWhere := cWhere || ' AND trunc(Y.fechahoraini) ';
       cWhere := cWhere || ' BETWEEN TO_DATE(' || '''' || p_dFecIni || '''' || ',' || '''' || 'YYYYMMDD' || '''' || ')';
       cWhere := cWhere || ' AND TO_DATE(' || '''' || p_dFecFin || '''' || ',' || '''' || 'YYYYMMDD' || '''' || ') )';
       --
    END IF;
    --
    cWhere := cWhere || ' AND D.indsisorig = ' || '''' || NVL(p_cIndOri,'W') || '''';
    -- Ordenar por IdeDec en forma descendente.
    cWhere := cWhere || ' Order by D.IdeDec DESC ';
    --
    OPEN c_Certificado FOR cWhere;
    --
    RETURN c_Certificado;
   END LISTA_APLICACIONES;
  /*------------------------------------------------------------------------
    Nombre     : DETALLE APLICACION
    Proposito  : Devuelve el detalle de la aplicaci?n
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdeDec  Identificaci?n de aplicaci?n
    Log de Cambios
        Fecha        Autor               Descripcion
        29/03/2004   Roberto Carranza    Creaci?n
        10/05/2004   Roberto Carranza    Se agreg? campo IndProceso.
                                         Se agreg? t?tulo para el reporte.
        10/01/2005   Karen Munayco       Se modifico el dato de provincia para que muestre el correcto <N3021341>
        28/09/2005   Jose Porlles        Se modifico para obtener como Asegurado al Responsable de Pago seleccionado <N3023360>
        30/05/2011   Marlon Castro G.    Modificaci?n <N3037167> Adecuaciones Grupo Gloria
        17/10/2011   Italo Rivas T.      <N3037736> COM-00767
        08/02/2012   Jhonatan Rojas T    <N3038436> TRANS-00027 / Se modifico el obtener asegurado.
        28/02/2012   Jhonatan Rojas T    <N3038436> TRANS-00027 / Se modifico el obtener contrante.
        10/09/2012   Italo Rivas         <N3039615> EMI-14941 / Modificaci?n
        14/09/2012   Italo Rivas         <N3039670> TRANS-00033 / Modificaci?n
        10/05/2013   Renzo Yacarini      <N3040860> HD 117200
        30/05/2013   Renzo Yacarini      <N3041051> HD 117806
        21/09/2020   Jhonatan Ninanya    <RTC-243489> Se agrego el codigo de producto de la poliza, nombre, codigo SBS y monto comision del intermediario
   --------------------------------------------------------------------------*/
   FUNCTION DETALLE_APLICACION (p_cCodTra  VARCHAR2,
                                p_cUsrLog  VARCHAR2,
                                p_nUsrId   NUMBER,
                                p_cUsrRol  VARCHAR2,
                                p_cUsrPer  VARCHAR2,
                                p_nIdeDec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   vError   VARCHAR2(300);
   --<I N3037736> COM-00767 / Italo Rivas T. / 17-10-2011
   codLlave1 VARCHAR2(3);
   codLlave2 VARCHAR2(3);
   codLlave3 VARCHAR2(3);
   codLlave4 VARCHAR2(3);
   cPoliza   POLIZA.NUMPOL%TYPE;
   nExiste   NUMBER := 0;
   --<F N3037736> COM-00767 / Italo Rivas T. / 17-10-2011

   -- <I N3038436> TRANS-00027 Jhonatan Rojas Terrones / 09-02-2012
   c_numIdAsegWeb NUMBER;
   c_DireccionAseg VARCHAR2(100);
   c_DistritoAseg VARCHAR2(50);
   c_TelefonoAseg VARCHAR2(25);
   c_RUCAseg VARCHAR2(15);
   -- <F N3038436> TRANS-00027 Jhonatan Rojas Terrones / 09-02-2012

   -- <I RTC-243489> Jhonatan Ninanya / 21-09-2020
   cNomIntermediario VARCHAR2(1000);
   cCodSBSIntermediario INTERMEDIARIO.CODSUPERINT%TYPE;
   nMtoComIntermediario RECIBO.MTOCOMMONEDA%TYPE;
   nOperacion RECIBO.IDEOP%TYPE;
   -- <F RTC-243489> Jhonatan Ninanya / 21-09-2020

   BEGIN
    IF p_nIdeDec IS NOT NULL THEN -- <RTC-243489> Jhonatan Ninanya / 21-09-2020
    --<I N3037736> COM-00767 / Italo Rivas T. / 17-10-2011
    SELECT Z.Numpol INTO cPoliza
    FROM dec_transporte_flotante D,
         proveedor P,
         dat_transporte_flotante F,
         poliza Z,
         cliente C,
         tercero T
    WHERE D.IdeDec       = p_nIdedec
      AND D.Codproveedor = P.Codproveedor(+)
      AND D.Idepol       = F.idepol
      AND D.numcert      = F.numcert
      AND D.idepol       = Z.Idepol
      AND C.CodCli       = Z.CodCli
      AND T.NumId        = C.NumId+0
      AND T.TipoId || '' = C.TipoId
      AND T.DvId         = C.DvId;

    BEGIN
      SELECT COUNT(1)
      INTO nExiste
      FROM RIM_TAB_GRAL
      WHERE CODTAB = 'POLPREVGLO'
        AND LLAVETAB = cPoliza;
    EXCEPTION
      WHEN OTHERS THEN
        nExiste := 0;
    END;

    IF nExiste = 0 THEN
       codLlave1 := '001';
       codLlave2 := '002';
       codLlave3 := '003';
       codLlave4 := '004';
    ELSE
       codLlave1 := '005';
       codLlave2 := '006';
       codLlave3 := '007';
       codLlave4 := '008';
    END IF;
    --<F N3037736> COM-00767 / Italo Rivas T. / 17-10-2011

  -- <I N3038436> TRANS-00027 Jhonatan Rojas Terrones / 09-02-2012
    BEGIN

      SELECT Numidasegweb
        INTO c_numIdAsegWeb
        FROM DEC_TRANSPORTE_FLOTANTE x
       WHERE IDEDEC = p_nIdeDec;
    EXCEPTION
      WHEN OTHERS THEN
        c_numIdAsegWeb := NULL;
    END;
     IF  c_numIdAsegWeb IS NOT NULL THEN

        c_DireccionAseg := PR_API_EMI_TRAN.BUSCA_DIR_TELF_DIST_RUC_ASEG('D',c_numIdAsegWeb);
        c_DistritoAseg:=PR_API_EMI_TRAN.BUSCA_DIR_TELF_DIST_RUC_ASEG('DIST',c_numIdAsegWeb);
        c_TelefonoAseg:=PR_API_EMI_TRAN.BUSCA_DIR_TELF_DIST_RUC_ASEG('T',c_numIdAsegWeb);
        c_RUCAseg:=PR_API_EMI_TRAN.BUSCA_DIR_TELF_DIST_RUC_ASEG('R',c_numIdAsegWeb);

     ELSE
       c_DireccionAseg := NULL;
        c_DistritoAseg:=NULL;
        c_TelefonoAseg:=NULL;
        c_RUCAseg:=NULL;
        END IF;
    -- <F N3038436> TRANS-00027 Jhonatan Rojas Terrones / 09-02-2012

    --IF p_nIdeDec IS NOT NULL THEN -- <RTC-243489> Jhonatan Ninanya / 21-09-2020
    -- <I RTC-243489> Jhonatan Ninanya / 21-09-2020
    BEGIN
      SELECT DECODE(T.TipoTer,
                    'P',
                    LTRIM(RTRIM(T.ApeTer)) || ' ' ||
                    LTRIM(RTRIM(T.ApeMatter)) || ' ' ||
                    LTRIM(RTRIM(T.NomTer)),
                    LTRIM(RTRIM(T.NomTer))),
             I.CodSuperInt
        INTO cNomIntermediario, cCodSBSIntermediario
        FROM TERCERO                 T,
             INTERMEDIARIO           I,
             PART_INTER_POL          P,
             dec_transporte_flotante D
       WHERE T.NumId = I.NumId
         AND I.CodInter = P.CodInter
         AND P.IdePol = D.IdePol
         AND D.IdeDec = p_nIdedec;
    EXCEPTION
      WHEN OTHERS THEN
        cNomIntermediario    := '';
        cCodSBSIntermediario := '';
    END;
    
    BEGIN
    
      SELECT R.Ideop
        INTO nOperacion
        FROM RECIBO R, dec_transporte_flotante D
       WHERE R.Iderec = D.Iderec
         AND D.Idedec = p_nIdedec;
    
      SELECT SUM(R.Mtocommoneda)
        INTO nMtoComIntermediario
        FROM RECIBO R
       WHERE R.Ideop = nOperacion;
    
    EXCEPTION
      WHEN OTHERS THEN
        nMtoComIntermediario := NULL;
    END;
    -- <F RTC-243489> Jhonatan Ninanya / 21-09-2020

       OPEN c_Certificado FOR
             SELECT Z.Numpol,                                                                      -- 1
                    D.IdePol,                                                                      -- 2
                    D.Numcert,                                                                     -- 3
                    DECODE(TipoTer,'J',T.NomTer||' '||T.ApeTer||' '||T.ApeMatTer,
                                   'P',T.NomTer||' '||T.ApeTer||' '||T.ApeMatTer,
                                   'G',T.NomTer,'E',T.Nomter) NomContratante,                      -- 4

                    -- <I N3038436> TRANS-00027 / Jhonatan Rojas Terrones / 09-02-2012

                    --  DECODE(T.IndDirDesp,'D', T.Direc,'T', T.Directrab) Direc_Contratante,
                    --PR_API_EMI_TRAN.distrito_contratante( DECODE(T.IndDirDesp,'D',T.CodPais,'T', T.CodPaisTrab),
                    --                                      DECODE(T.IndDirDesp,'D',T.CodEstado,'T', T.CodEstadoTrab),
                    --                                      DECODE(T.IndDirDesp,'D',T.CodCiudad,'T', T.CodCiudadTrab),
                    --                                      DECODE(T.IndDirDesp,'D',T.CodMunicipio,'T', T.CodMunicipioTrab) ) Distrito_Contratante
                    -- T.Telef1 TelefContratante
                    -- PR_API_EMI_TRAN.Ruc_Contratante(Z.Codcli) Ruc_Contratante
                   NVL(c_DireccionAseg, PR_API_EMI_TRAN.GET_DETALLE_DATOS_ASEGURADO('D',Z.IDEPOL)) Direc_Contratante,          -- 5
                   NVL(c_DistritoAseg, PR_API_EMI_TRAN.GET_DETALLE_DATOS_ASEGURADO('DIST',Z.IDEPOL)) Distrito_Contratante, -- 6
                   NVL(c_TelefonoAseg, PR_API_EMI_TRAN.GET_DETALLE_DATOS_ASEGURADO('T',Z.IDEPOL)) TelefContratante,         -- 7
                   NVL(c_RUCAseg, PR_API_EMI_TRAN.GET_DETALLE_DATOS_ASEGURADO('R',Z.IDEPOL)) Ruc_Contratante,                      -- 8
                    -- <F N3038436> TRANS-00027 / Jhonatan Rojas Terrones / 09-02-2012

                    NVL(PR_IMPRIME_POLIZA.nombre_persona('C', D.CodCli),PR_API_EMI_TRAN.Nombre_Asegurado(D.IdePol,D.NumCert,D.IdeRec,'A')) Nombre_Asegurado,  -- 9 < N3023360 > Reemplazo de consulta de asegurado
                    D.codmoneda,                                                                    -- 10
                    PR_API_EMI_TRAN.Numero_Operacion(D.IdeRec) Num_Operacion,                       -- 11
                    D.Codramocert,                                                                  -- 12
                    PR_API_EMI_TRAN.Des_Ramo(D.Codramocert) Des_Ramo,                               -- 13
                    D.numTramite NumTramite,                                                        -- 14
                    D.Numcarta,                                                                     -- 15
                    PR_API_EMI_TRAN.tipo_aplicacion(D.IdePol,D.Numcert,D.Codramocert) DescNacional, -- 16
                    D.baseavaluo,                                                                   -- 17
                    D.stsdec,                                                                       -- 18
                    D.IdeDec,                                                                       -- 19
                    D.planilla,                                                                     -- 20
                    D.Aplicacion,                                                                   -- 21
                    D.Fecfinvalid FecFinVig,                                                        -- 22
                    D.fechado Fechado,                                                              -- 23
                    PR_IMPRIME_POLIZA.nombre_persona('N', P.numid) NomPreventor,                    -- 24
                    F.Primamin,                                                                     -- 25
                    D.codcontinente,                                                                -- 26
                    PR.busca_lval('TIPOCONT',D.Codcontinente) desccontinente,                       -- 27
                    D.codpais,                                                                      -- 28
                    PR_UBIGEO.busca_pais(D.codpais) Descpais,                                       -- 29
                    D.codestado,                                                                    -- 30
                    DECODE(NVL(D.codestado,'1'),'1','',PR_UBIGEO.busca_estado(D.codpais,D.codestado)) DescEstado,   -- 31
                    D.codcontinentedest,                                                            -- 32
                    PR.busca_lval('TIPOCONT',D.Codcontinentedest) desccontinentedest,               -- 33
                    D.codpaisdest,                                                                  -- 34
                    PR_UBIGEO.busca_pais(D.codpaisdest) DescPaisDest,                               -- 35
                    D.codestadodest,                                                                -- 36
                    DECODE(PR_UBIGEO.busca_estado(D.codpaisdest,D.codestadodest),'INVALIDO','',PR_UBIGEO.busca_estado(D.codpaisdest,D.codestadodest)) DescEstadoDest,           -- 37 <HD: N?30471> no debe mostrar dato "invalido"
                    D.tipotransporte,                                                               -- 38
                    PR.busca_lval('TIPTRAN',D.tipotransporte) DescTipoTran,                         -- 39
                    d.tipomercancia,                                                                -- 40
                    PR.BUSCA_LVAL('TIPOMERC', D.TipoMercancia) DescTipoMerc,                        -- 41
                    D.tipoembalaje,                                                                 -- 42
                    PR.BUSCA_LVAL('TIPOEMBA', D.tipoembalaje) DescTipoEmba,                         -- 43
                    D.tasa,                                                                         -- 44
                    D.deducible,                                                                    -- 45
                    d.Descbenef DescBenef,                                                          -- 46
                    D.Desctran,                                                                     -- 47
                    D.codbuque,                                                                     -- 48
                    decode(D.tipotransporte,'002',PR.BUSCA_LVAL('TIPOLINE', D.CodBuque),'003',PR_API_EMI_TRAN.Busca_Buque(D.CodBuque) ) DescBuque, -- 49
                    D.numviaje,                                                                     -- 50
                    D.sobreseg,                                                                     -- 51
                    ( D.MtoLiqMoneda * D.SobreSeg ) / 100 Mto_SobreSeguro,                          -- 52
                    D.mtoderaduana,                                                                 -- 53
                    D.indtransbordo,                                                                -- 54
                    D.indcubierta,                                                                  -- 55
                    D.porcantiguedad,                                                               -- 56
                    D.indclauguerra,                                                                -- 57
                    D.inddctocont,                                                                  -- 58
                    D.mtoliqmoneda,                                                                 -- 59
                    D.MtoLiqMoneda + (( D.MtoLiqMoneda * D.SobreSeg ) / 100) Suma_Asegurada,        -- 60
                    D.tasatotal,                                                                    -- 61
                    D.primamoneda,                                                                  -- 62
                    D.codcli CodRespPago,                                                           -- 63
                    PR_IMPRIME_POLIZA.nombre_persona('C', D.CodCli) NomResPago,                     -- 64
                    D.descdec,                                                                      -- 65
                    D.docreferencia,                                                                -- 66
                    D.proveedor,                                                                    -- 67
                    D.numpedido,                                                                    -- 68
                    D.descdestino,                                                                  -- 69
                    Z.Codofiemi codagencia,                                                         -- 70
                    D.pesobruto,                                                                    -- 71
                    (D.MtoLiqMoneda + (( D.MtoLiqMoneda * D.SobreSeg ) / 100)  + D.mtoderaduana) SumAseTotal,  --72
                    nvl(dA.fecemi, PR_API_EMI_TRAN.Fecha_Emision(d.IdeDec)) Fec_Operacion,                          -- 73 -- <N3040860> HD 117200 / Renzo Yacarini / 10-05-2013 -- <N3041051> HD 117806 / Renzo Yacarini / 30-05-2013
                    PR_API_EMI_TRAN.f_Pago(D.idedec) FormaPago,                                     -- 74
                    D.Descmercancia,                                                                -- 75
                    D.Descembalaje,                                                                 -- 76
                    PR_API_EMI_TRAN.descripcion_agencia(z.Codofiemi) DescAgencia,                   -- 77
                    PR.BUSCA_LVAL('TIPOMON',D.Codmoneda) DescMoneda,                                -- 78
                    PR_UBIGEO.busca_ciudad(DECODE(T.IndDirDesp,'D',T.CodPais,'T', T.CodPaisTrab),
                                           DECODE(T.IndDirDesp,'D',T.CodEstado,'T', T.CodEstadoTrab),
                                           DECODE(T.IndDirDesp,'D',T.CodCiudad,'T', T.CodCiudadTrab)) DescProvincia,        -- 79 <N3021341> se modifico para que mostrara la provincia correcta
                    D.IndPolitica,                                                                  -- 80
                    PR_API_EMI_TRAN.Indicador_Proceso(D.IdePol,D.NumCert) IndProceso,               -- 81 -- Z.StsPol
                    PR_API_EMI_TRAN.Titulo_Dec(D.IdePol) TitReporte,                                -- 82
                    D.NUMIDASEGWEB,                                                                 -- 83 --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106

                    -- <I N3038436> TRANS-00027 / Jhonatan Rojas Terrones / 08-02-2012 / Traer nombre del asegurado en vez del nombre del cliente
                     --PR_IMPRIME_POLIZA.nombre_persona('N', D.NUMIDASEGWEB)  NombreAsegurado,         -- 84 --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106
                      NVL(PR_IMPRIME_POLIZA.nombre_persona('N', D.NUMIDASEGWEB),PR_API_EMI_TRAN.BUSCA_ASEG_RESPAGO_CONTR('A',Z.Idepol, D.NUMCERT)) NombreAsegurado,  -- <N3039615> EMI-14941 / Italo Rivas / 11-09-2012
                   -- <F N3038436> TRANS-00027 / Jhonatan Rojas Terrones / 08-02-2012 / Traer nombre del asegurado en vez del nombre del cliente
                    --<I N3037167> Marlon Castro G. - 30/05/2011
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('GRUPOGLORIA',''||T.NUMID),'N') INDGRUPOGLORIA,                                             -- 85
                    NVL(PR_API_EMI_TRAN.GET_DATOS_ASEGURADO(D.CodCli, D.IdePol, D.NumCert, D.IdeRec, D.NUMIDASEGWEB, 'T'), ' ') TELEFONO_ASEGURADO,   -- 86
                    NVL(PR_API_EMI_TRAN.GET_DATOS_ASEGURADO(D.CodCli, D.IdePol, D.NumCert, D.IdeRec, D.NUMIDASEGWEB, 'D'), ' ') DIRECCION_ASEGURADO,  -- 87
                    NVL(PR_API_EMI_TRAN.GET_DATOS_ASEGURADO(D.CodCli, D.IdePol, D.NumCert, D.IdeRec, D.NUMIDASEGWEB, 'L'), ' ') DISTRITO_ASEGURADO,   -- 88
                    NVL(PR_API_EMI_TRAN.GET_DATOS_ASEGURADO(D.CodCli, D.IdePol, D.NumCert, D.IdeRec, D.NUMIDASEGWEB, 'R'), ' ') RUC_ASEGURADO,        -- 89
                    --<I N3037736> COM-00767 / Italo Rivas Torres.  17-10-2011
                    /*
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN','001'),' ') TEXTO_FOOTER1,                                                    -- 90
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN','002'),' ') TEXTO_FOOTER2,                                                    -- 91
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN','003'),' ') TEXTO_FOOTER3,                                                    -- 92
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN','004'),' ') TEXTO_FOOTER4                                                   -- 93
                    */
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN',codLlave1),' ') TEXTO_FOOTER1,                                                  -- 90
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN',codLlave2),' ') TEXTO_FOOTER2,                                                  -- 91
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN',codLlave3),' ') TEXTO_FOOTER3,                                                  -- 92
                    NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('TXTFOOTERTRAN',codLlave4),' ') TEXTO_FOOTER4,                                                   -- 93
                    --<F N3037167> Marlon Castro G. - 30/05/2011
                    --<F N3037736> COM-00767 / Italo Rivas Torres.  17-10-2011
                    -- <I N3038436> TRANS-00027 / Jhonatan Rojas Terrones / 28-02-2012
                    DECODE(T.IndDirDesp,'D', T.Direc,'T', T.Directrab) Direc_ContratanteOriginal,                                                                                       --94
                    PR_API_EMI_TRAN.distrito_contratante( DECODE(T.IndDirDesp,'D',T.CodPais,'T', T.CodPaisTrab),
                    DECODE(T.IndDirDesp,'D',T.CodEstado,'T', T.CodEstadoTrab),
                    DECODE(T.IndDirDesp,'D',T.CodCiudad,'T', T.CodCiudadTrab),
                    DECODE(T.IndDirDesp,'D',T.CodMunicipio,'T', T.CodMunicipioTrab) ) Distrito_ContratanteOriginal,                                                            --95
                    T.Telef1 TelefContratanteOriginal,                                                                                                                                                     --96
                    PR_API_EMI_TRAN.Ruc_Contratante(Z.Codcli) Ruc_ContratanteOriginal                                                                                              --97
                    -- <F N3038436> TRANS-00027 / Jhonatan Rojas Terrones / 28-02-2012
                    --<I N3039670> TRANS-00033 / Italo Rivas / 14-09-2012
                    ,D.Numfact                                           --98
                    ,D.Descnave                                          --99
                    ,D.Puertoaeropuertoorigen                            --100
                    --<F N3039670> TRANS-00033 / Italo Rivas / 14-09-2012
                    -- <I RTC-243489> Jhonatan Ninanya / 21-09-2020
                    ,Z.Codprod Codigo_Producto                          --101
                    ,cNomIntermediario Nombre_Intermediario             --102
                    ,cCodSBSIntermediario CodSBS_Intermediario          --103
                    ,nMtoComIntermediario Monto_Comision_Int            --104
                    -- <F RTC-243489> Jhonatan Ninanya / 21-09-2020
               FROM dec_transporte_flotante D,
                    proveedor P,
                    dat_transporte_flotante F,
                    poliza Z,
                    cliente C,
                    tercero T,
                    DEC_TRANSPORTE_FLOTANTE_ADIC DA  -- <N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
              WHERE D.IdeDec       = p_nIdedec
                AND D.Codproveedor = P.Codproveedor(+)
                AND D.Idepol       = F.idepol
                AND D.numcert      = F.numcert
                AND D.idepol       = Z.Idepol
                AND C.CodCli       = Z.CodCli
                AND T.NumId        = C.NumId+0
                AND T.TipoId || '' = C.TipoId
                AND T.DvId         = C.DvId
                AND D.IDEDEC       = DA.IDEDEC(+);  -- <N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    ELSE
      RAISE_APPLICATION_ERROR(-20100,'Identificador de Declaraci?n no puede ser Nulo');
    END IF;
      --
    RETURN c_Certificado;
   END DETALLE_APLICACION;

  /*------------------------------------------------------------------------
    Nombre     : GET_VALOR_RIM_TAB
    Proposito  : Devuelve la descripci?n de RIM_TAB_GRAL seg?n la llave
    Referencia : COM-00747
    Parametros :
                  p_cCodTab    C?digo de Tabla
                  p_cLlaveTab  Llave  de Tabla

    Log de Cambios
           Fecha        Autor              Descripcion
           27/05/2011   Marlon Castro G.   Creaci?n <N3037167> Adecuaciones Grupo Gloria
   --------------------------------------------------------------------------*/
   FUNCTION GET_VALOR_RIM_TAB (p_cCodTab   RIM_TAB_GRAL.CODTAB%TYPE,
                               p_cLlaveTab RIM_TAB_GRAL.LLAVETAB%TYPE
                               )
                               RETURN VARCHAR2 IS

   cSalida   RIM_TAB_GRAL.DESCLARGA%TYPE;

   BEGIN

     BEGIN

       SELECT DESCLARGA INTO cSalida
         FROM RIM_TAB_GRAL
        WHERE CODTAB = p_cCodTab
          AND LLAVETAB = p_cLlaveTab;

     EXCEPTION WHEN OTHERS THEN
       cSalida := NULL;
     END;

   RETURN cSalida;

   END GET_VALOR_RIM_TAB;

  /*------------------------------------------------------------------------
    Nombre     : GET_DATOS_ASEGURADO
    Proposito  : Devuelve datos del asegurado
    Referencia : COM-00747
    Parametros :
                  p_cCodCli    C?digo de Cliente
                  p_nIdePol    Id de la P?liza
                  p_nNumCert   N?mero de Certificado
                  p_nIdeRec    Id Recibo
                  p_cNumIdWeb  NumId Usuario Web
                  p_cTipoDato  Tipo de Dato a devolver
                               T -> Tel?fono
                               D -> Direcci?n
                               L -> Distrito
                               R -> R.U.C.
    Log de Cambios
           Fecha        Autor              Descripcion
           27/05/2011   Marlon Castro G.   Creaci?n <N3037167> Adecuaciones Grupo Gloria
   --------------------------------------------------------------------------*/
   FUNCTION GET_DATOS_ASEGURADO (p_cCodCli   DEC_TRANSPORTE_FLOTANTE.CODCLI%TYPE,
                                 p_nIdePol   DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
                                 p_nNumCert  DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPE,
                                 p_nIdeRec   DEC_TRANSPORTE_FLOTANTE.IDEREC%TYPE,
                                 p_cNumIdWeb DEC_TRANSPORTE_FLOTANTE.NUMIDASEGWEB%TYPE,
                                 p_cTipoDato VARCHAR2)
                                 RETURN VARCHAR2 IS

   cSalida   VARCHAR2(1000);
   cCodCli   DEC_TRANSPORTE_FLOTANTE.CODCLI%TYPE;
   cCursor   PR_API_EMI_TRAN.c_Cursor;
   cTercero  TERCERO%ROWTYPE;

   BEGIN

   IF(p_cNumIdWeb IS NOT NULL) THEN

     OPEN  cCursor FOR
           SELECT * FROM TERCERO WHERE NUMID = p_cNumIdWeb;

   ELSE

     IF(p_cCodCli IS NOT NULL) THEN
       cCodCli := p_cCodCli;
     ELSE
       BEGIN
         SELECT C.CodCli
           INTO cCodCli
           FROM RECIBO R, COND_FINANCIAMIENTO F, CLIENTE C
          WHERE R.IdePol  = p_nIdePol
            AND R.NumCert = p_nNumCert
            AND R.IdeRec  = p_nIdeRec
            AND F.IdePol  = R.IdePol
            AND F.NumCert = R.NumCert
            AND F.NumOper = R.IdeOp
            AND C.NumId=F.NumId;
       EXCEPTION
         WHEN OTHERS THEN
           cCodCli := NULL;
       END;
     END IF;

     OPEN  cCursor FOR
           SELECT T.*
             FROM TERCERO T, CLIENTE C
            WHERE C.CODCLI = cCodCli AND T.NUMID = C.NUMID AND T.TIPOID = C.TIPOID AND T.DVID = C.DVID;
   END IF;

   FETCH cCursor INTO cTercero;

   BEGIN

     IF    (p_cTipoDato = 'T') THEN
            cSalida := cTercero.Telef1;

     ELSIF (p_cTipoDato = 'D') THEN
            SELECT DECODE(cTercero.IndDirDesp,
                          'D', cTercero.Direc,
                          'T', cTercero.Directrab)
              INTO cSalida
              FROM DUAL;

     ELSIF (p_cTipoDato = 'L') THEN

            SELECT DescMunicipio
              INTO cSalida
              FROM MUNICIPIO
             WHERE CodPais      = DECODE(cTercero.IndDirDesp,'D',cTercero.CodPais,'T', cTercero.CodPaisTrab)
               AND CodEstado    = DECODE(cTercero.IndDirDesp,'D',cTercero.CodEstado,'T', cTercero.CodEstadoTrab)
               AND CodCiudad    = DECODE(cTercero.IndDirDesp,'D',cTercero.CodCiudad,'T', cTercero.CodCiudadTrab)
               AND CodMunicipio = DECODE(cTercero.IndDirDesp,'D',cTercero.CodMunicipio,'T', cTercero.CodMunicipioTrab);

     ELSIF (p_cTipoDato = 'R') THEN
            SELECT NumIdDoc
              INTO cSalida
              FROM DOCS_IDENTIDAD
             WHERE NUMID = cTercero.Numid;
     END IF;

   EXCEPTION WHEN OTHERS THEN
     cSalida := ' ';
   END;

   RETURN cSalida;

   END GET_DATOS_ASEGURADO;

  /*------------------------------------------------------------------------
    Nombre     : GET_APLICACION_TRAN
    Proposito  : Devuelve un n?mero de aplicaci?n correlativo
    Referencia : COM-00747
    Par?metros :
                 p_cIdePol   Id de P?liza
                 p_nSw       1 -> Insert

    Log de Cambios
           Fecha        Autor              Descripcion
           27/05/2011   Marlon Castro G.   Creaci?n <N3037167> Adecuaciones Grupo Gloria
   --------------------------------------------------------------------------*/
   FUNCTION GET_APLICACION_TRAN(p_cIdePol POLIZA.IDEPOL%TYPE
                               )
                               RETURN VARCHAR2 IS

   cSalida         DEC_TRANSPORTE_FLOTANTE.APLICACION%TYPE;
   cAnioActual     NUMBER;
   cAnioAplicacion NUMBER;
   cSecuencia      NUMBER;
   cNumPol         POLIZA.NUMPOL%TYPE;

   BEGIN

        BEGIN

           SELECT TO_NUMBER(SUBSTR(DESCLARGA, 0, 5)),
                  TO_NUMBER(SUBSTR(DESCLARGA, 7))

             INTO cSecuencia, cAnioAplicacion
             FROM RIM_TAB_GRAL
            WHERE CODTAB = 'LASTAPLICATION'
            AND LLAVETAB = (SELECT NUMPOL FROM POLIZA WHERE IDEPOL = p_cIdePol);

           SELECT TO_NUMBER(TO_CHAR(SYSDATE, 'YYYY'))
             INTO cAnioActual
             FROM DUAL;

           IF(cAnioActual > cAnioAplicacion) THEN
             cSecuencia := 1;

           ELSE
             cSecuencia := cSecuencia + 1;

           END IF;

           cSalida := LPAD(cSecuencia, '5', '0')||'/'||cAnioActual;

         UPDATE RIM_TAB_GRAL --Actualizamos la ?ltima aplicaci?n del Grupo Gloria
            SET DESCLARGA = cSalida
          WHERE CODTAB = 'LASTAPLICATION'
            AND LLAVETAB = (SELECT NUMPOL FROM POLIZA WHERE IDEPOL = p_cIdePol);

         COMMIT;

         EXCEPTION WHEN OTHERS THEN
          DBMS_OUTPUT.put_line('Error al generar el correlativo de aplicaci?n para Idepol: '||p_cIdePol);
          ROLLBACK;

         END;

   RETURN cSalida;

   END GET_APLICACION_TRAN;


  /*------------------------------------------------------------------------
    Nombre     : ES_GRUPO_GLORIA
    Proposito  : Devuelve si el contratante es del Grupo Gloria
    Referencia : COM-00747
    Parametros :
                  p_cIdePoliza    Id de P?liza

    Log de Cambios
           Fecha        Autor              Descripcion
           27/05/2011   Marlon Castro G.   Creaci?n <N3037167> Adecuaciones Grupo Gloria
   --------------------------------------------------------------------------*/
   FUNCTION ES_GRUPO_GLORIA (p_cIdePoliza   RIM_TAB_GRAL.CODTAB%TYPE
                               )
                               RETURN VARCHAR2 IS

   cSalida  VARCHAR2(1);
   cNumId   CLIENTE.NUMID%TYPE;

   BEGIN

     BEGIN

       SELECT B.NUMID INTO cNumId
         FROM POLIZA A, CLIENTE B
        WHERE A.CODCLI = B.CODCLI
          AND A.IDEPOL = p_cIdePoliza;

       cSalida := NVL(PR_API_EMI_TRAN.GET_VALOR_RIM_TAB('GRUPOGLORIA', ''||cNumId), 'N');


     EXCEPTION WHEN OTHERS THEN
       cSalida := 'N';
     END;

   RETURN cSalida;

   END ES_GRUPO_GLORIA;

  /*-----------------------------------------------------------------------
    Nombre     : LISTA_COBERTURAS
    Proposito  : Devuelve la lista de coberturas
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdeDec  Identificaci?n de aplicaci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           05/04/2004    Roberto Carranza    Creaci?n
           18/05/2004    Roberto Carranza    Se modific? el select
           03/08/2007    Jos? Collantes      <N3028459> EMI-02894. Implementaci?n
                                             de tabla DEC_TRANSPORTE_COBERT_IND.
   ------------------------------------------------------------------------*/
   FUNCTION LISTA_COBERTURAS (p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nIdeDec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   nIdePol       DEC_TRANSPORTE_FLOTANTE.Idepol%TYPE;
   nIdeRec       DEC_TRANSPORTE_FLOTANTE.IdeRec%TYPE;
   cCodPro       POLIZA.Codprod%TYPE;
   cCodPla       CERT_RAMO.Codplan%TYPE;
   cRevPla       CERT_RAMO.Revplan%TYPE;
   cCodRam       CERT_RAMO.codramocert%TYPE;
   --
   vError        VARCHAR2(300);
   --
   nNumOpe       NUMBER;
   --
   BEGIN
      --
      BEGIN
       SELECT D.idepol, D.IdeRec, P.CodProd, C.CodPlan, C.RevPlan, C.CodRamoCert
         INTO nIdePol , nIdeRec , cCodPro  , cCodPla  , cRevPla  , cCodRam
         FROM POLIZA P, CERT_RAMO C, DEC_TRANSPORTE_FLOTANTE D
        WHERE P.IdePol      = C.IdePol
          AND C.IdePol      = D.IdePol
          AND C.NumCert     = D.NumCert
          AND C.CodRamoCert = D.CodRamoCert
          AND D.idedec      = p_nIdeDec;
      EXCEPTION
        WHEN OTHERS THEN
             nIdePol := 0;
             nIdeRec := 0;
             cCodPro := NULL;
             cCodPla := NULL;
             cRevPla := NULL;
             cCodRam := NULL;
      END;
      --
      nNumOpe := PR_API_EMI_TRAN.numero_operacion(nIdeRec);
      --
      --<I N3028459> Jos? Collantes / 03-08-2007 / EMI-02894. Implementaci?n de tabla MOD_COBERT_DEC_TRANSPORTE.
      OPEN c_Certificado FOR
           SELECT PR_API_EMI_TRAN.desc_cobertura(mc.idecobert,mc.codramocert) DescCobert,
                 MC.CodMonedaCob CodMonedaCob,
                 MC.SumaAsegMonedaCob SumaAsegMonedaCob
           FROM cobert_cert C, mod_cobert MC, stke_mod_cobert S, dec_transporte_cobert DT
           WHERE S.IdeOp      = nNumOpe
           AND MC.IdePol+0  = nIdePol
           AND S.IdeCobert  = MC.IdeCobert
           AND S.NumMod     = MC.NumMod
           AND C.IdeCobert  = MC.IdeCobert
           AND DT.IdeCobert = MC.Idecobert
           AND DT.NumMod    = MC.NumMod
           AND DT.IdeDec    = p_nIdeDec
           AND C.CodCobert <> (SELECT CodCobertDec
                               FROM RAMO_PLAN_PROD R
                               WHERE R.CodProd     = cCodPro
                               AND R.CodPlan     = cCodPla
                               AND R.RevPlan     = cRevPla
                               AND R.CodRamoPlan = cCodRam)
           AND NOT EXISTS (SELECT 1 FROM MOD_COBERT_DEC_TRANSPORTE M
                           WHERE M.IdeCobert= MC.IdeCobert
                           AND   M.NumMod   = MC.NumMod
                           AND   M.IdeDec   = DT.IdeDec)

           UNION ALL
           SELECT PR_API_EMI_TRAN.desc_cobertura(mc.idecobert,mc.codramocert) DescCobert,
                 MC.CodMonedaCob CodMonedaCob,
                 MC.SumaAsegMonedaCob SumaAsegMonedaCob
           FROM cobert_cert C, mod_cobert_dec_transporte MC, stke_mod_cobert S, dec_transporte_cobert DT
           WHERE S.IdeOp      = nNumOpe
           AND MC.IdePol+0  = nIdePol
           AND S.IdeCobert  = MC.IdeCobert
           AND S.NumMod     = MC.NumMod
           AND C.IdeCobert  = MC.IdeCobert
           AND DT.IdeCobert = MC.Idecobert
           AND DT.NumMod    = MC.NumMod
           AND DT.IdeDec    = MC.IdeDec
           AND DT.IdeDec    = p_nIdeDec
           AND C.CodCobert <> (SELECT CodCobertDec
                               FROM RAMO_PLAN_PROD R
                               WHERE R.CodProd     = cCodPro
                               AND R.CodPlan     = cCodPla
                               AND R.RevPlan     = cRevPla
                               AND R.CodRamoPlan = cCodRam);
           --<F N3028459>
/*          SELECT PR_API_EMI_TRAN.desc_cobertura(mc.idecobert,mc.codramocert) DescCobert,
                 MC.CodMonedaCob CodMonedaCob,
                 MC.SumaAsegMonedaCob SumaAsegMonedaCob
            FROM RECIBO R, COBERT_CERT C, MOD_COBERT MC, STKE_MOD_COBERT S
           WHERE R.IdeOp     = nNumOpe
             AND R.IdePol+0  = nIdePol
             AND S.IdeOp     = nNumOpe
             AND S.idecobert = MC.idecobert
             AND S.nummod    = MC.nummod
             AND C.IdeCobert = MC.IdeCobert
             AND C.CodCobert <> (SELECT CodCobertDec
                                  FROM RAMO_PLAN_PROD R
                                 WHERE R.CodProd     = cCodPro
                                   AND R.CodPlan     = cCodPla
                                   AND R.RevPlan     = cRevPla
                                   AND R.CodRamoPlan = cCodRam);*/
      RETURN(c_Certificado);
   END LISTA_COBERTURAS;
  /*-----------------------------------------------------------------------
    Nombre     : CONSOLIDADO_PRIMAS
    Proposito  : Devuelve lista conteniendo el consolidado de primas
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdeDec  Identificaci?n de aplicaci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   ------------------------------------------------------------------------*/
   FUNCTION CONSOLIDADO_PRIMAS(p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_nIdeDec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   nNumOpe       OPER_POL.NumOper%TYPE;
   --
   BEGIN
     --
     BEGIN
       SELECT r.ideop
         INTO nNumOpe
         FROM Dec_Transporte_Flotante F, recibo R
        WHERE F.IdeRec = R.IdeRec
          AND F.IdeDec = p_nIdeDec;
     EXCEPTION
        WHEN OTHERS THEN
             nNumOpe := NULL;
     END;
     --
     OPEN c_Certificado FOR
        SELECT INITCAP(CA.DescCptoAcre)  DescCptoAcre,
               AC.CodMoneda CodMoneda,
               SUM(DA.MtoDetAcreMoneda) Importe
         FROM FACTURA FA,
              ACREENCIA AC,
              DET_ACRE DA,
              CPTO_ACRE CA
        WHERE FA.NumOper      = nNumOpe
          AND AC.IdeFact      = FA.IdeFact
          AND DA.NumAcre      = AC.NumAcre
          AND CA.codgrupoacre = DA.codgrupoacre
          AND CA.CodCptoAcre  = DA.CodCptoAcre
        GROUP BY CA.IndClase,CA.DescCptoAcre,AC.CodMoneda
        ORDER BY CA.IndClase;
     --
     RETURN(c_Certificado);
     --
   END CONSOLIDADO_PRIMAS;
  /*-----------------------------------------------------------------------
    Nombre     : DOCUMENTOS_PAGO
    Proposito  : Devuelve lista de documentos de pago
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdeDec  Identificaci?n de aplicaci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           05/04/2004    Roberto Carranza    Creaci?n
   ------------------------------------------------------------------------*/
   FUNCTION DOCUMENTOS_PAGO (p_cCodTra  VARCHAR2,
                             p_cUsrLog  VARCHAR2,
                             p_nUsrId   NUMBER,
                             p_cUsrRol  VARCHAR2,
                             p_cUsrPer  VARCHAR2,
                             p_nIdeDec  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   nIdePol       DEC_TRANSPORTE_FLOTANTE.IdePol%TYPE;
   nNumCer       DEC_TRANSPORTE_FLOTANTE.NumCert%TYPE;
   nNumOpe       OPER_POL.NumOper%TYPE;
   --
   BEGIN
     --
     BEGIN
       SELECT r.ideop
         INTO nNumOpe
         FROM Dec_Transporte_Flotante F, recibo R
        WHERE F.IdeRec = R.IdeRec
          AND F.IdeDec = p_nIdeDec;
     EXCEPTION
        WHEN OTHERS THEN
             nNumOpe := NULL;
     END;
     --
     OPEN c_Certificado FOR
       SELECT ROWNUM Secuencia, A.CodMoneda, A.NumDoc, A.Mto_Giro, A.FecVencFact
         FROM ( SELECT DISTINCT F.codmoneda     CodMoneda,
                                F.NumFact       NumDoc,
                                F.MtoFactMoneda Mto_Giro,
                                F.FecVencFact   FecVencFact
                           FROM factura F,acreencia A,giros_financiamiento G
                          WHERE F.NumOper  = nNumOpe
                            AND A.IdeFact  = F.IdeFact
                            AND G.NumAcre  = A.NumAcre) A;
     ---
     RETURN(c_Certificado);
   END DOCUMENTOS_PAGO;
  /*-----------------------------------------------------------------------
    Nombre     : PROCESAR_APLICACION
    Proposito  : Insertar/Eliminar de la tabla DEC_TRANSPORTE_FLOTANTE, Validar politicas/Activar poliza
                 p_cCodTra: 004 Insertar
                            005 Modificar/Actualizar
                            006 Eliminar
                            008 Emitir
                            009 Insertar/Modificar/Actualizar y Emitir
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_cTipApl  Tipo de aplicaci?n
                  p_nIdePol  Identificador de p?liza
                  p_nNumCer  N?mero de certificado
                  p_cCodRam  C?digo de ramo
                  p_nNumTra  N?mero de tr?mite
                  p_nNumCar  N?mero de carta
                  p_cBasAva  Base de Avaluo
                  p_cCodMon  C?digo de moneda
                  p_cCodEst  C?digo de Estado
                  p_nNumDec  N?mero de declaraci?n
                  p_cNumPla  N?mero de planilla
                  p_cNumApl  N?mero de aplicaci?n
                  p_dFecVia  Fecha de viaje
                  p_dFecApl  Fecha de aplicaci?n
                  p_cPreven  Preventor
                  p_nPriMin  Prima m?nima
                  p_cConPro  Continente Procedencia
                  p_cPaiPro  Pais de procedencia
                  p_cDepPro  Departamento de procedencia
                  p_cConDes  Continente Destino
                  p_cPaiDes  Pais destino
                  p_cCodDep  Departamento destino
                  p_cCodTra  C?digo Tipo Transporte
                  p_cTipTra  Tipo Transporte
                  p_cTipMer  Tipo mercancia
                  p_cDesMer  Descripcion mercancia
                  p_cTipEmb  Tipo Embalaje
                  p_cDesEmb  Descripci?n de tipo de embalaje
                  p_nTasBas  Tasa B?sica
                  p_cDeduci  Deducible
                  p_cEndosa  Endosatario
                  p_cMedTra  Medio Transportador
                  p_nNumVia  N?mero de viaje
                  p_nPorSeg  Porcentaje sobre seguro
                  p_nDerAdu  Derecho de aduana
                  p_cTransb  Transborde
                  p_cSobCub  Sobrecubierta
                  p_nAntBuq  Porcentaje de Antiguedad del buque
                  p_cGueHue  Guerra y Huelga
                  p_cCodDto  C?digo de Descuento
                  p_nMonApl  Monto de Aplicaci?n
                  p_nTasTot  Tasa Total
                  p_nPriTot  Prima Total
                  p_cResPag  Responsable de pago
                  p_cDesApl  Descripci?n de la aplicaci?n
                  p_cDocRef  Documento de referencia
                  p_cProvee  Proveedor
                  p_cPedFac  Pedido / Factura
                  p_cDesDes  Descripci?n Destino
                  p_nPesBru  Peso Bruto
    Log de Cambios
           Fecha            Autor            Descripcion
           05/04/2004    Roberto Carranza    Creaci?n
           14/07/2004    Miguel Wilson       Permitir recibir nuevos responsables de pago
           31/08/2004    Ricardo Larrabure   Devolver descripci?n del error (N3019957)
           03/06/2006    Jose Porlles C.     <N3025449> Se setea a null la variable de sesi?n
           30/07/2010    Ronald Or?          <N3035448> Se impide reprocesar aplicaciones
           09/08/2010    Marlon Castro G.    <N3035534>  REQM MEJPROC-00156 - Se graba el id, numid, y rol del usuario que emite la declaraci?n
           30/05/2011    Marlon Castro G.    <N3037167>   Adecuaciones Grupo Gloria
           03/04/2012    Ronald Ore          <N3038699> HelpDesk 104296
           13/09/2012    Italo Rivas         <N3039670> TRANS-00033 - Modificaciones
           25/10/2012    Renzo Yacarini      <N3039819> CNT-01590
           10/05/2013    Renzo Yacarini      <N3040860> HD 117200
   ------------------------------------------------------------------------*/
   FUNCTION PROCESAR_APLICACION (p_cCodTra  VARCHAR2,       -- 1
                                 p_cUsrLog  VARCHAR2,       -- 2
                                 p_nUsrId   NUMBER,         -- 3
                                 p_cUsrRol  VARCHAR2,       -- 4
                                 p_cUsrPer  VARCHAR2,       -- 5
                                 p_cTipApl  VARCHAR2,       -- 6
                                 p_nIdePol  NUMBER,         -- 7
                                 p_nNumCer  NUMBER,         -- 8
                                 p_cCodRam  VARCHAR2,       -- 9
                                 p_cNumTra  VARCHAR2,       -- 10
                                 p_nNumCar  NUMBER,         -- 11
                                 p_cBasAva  VARCHAR2,       -- 12
                                 p_cCodMon  VARCHAR2,       -- 13
                                 p_cCodEst  VARCHAR2,       -- 14
                                 p_nIdeDec  NUMBER,         -- 15
                                 p_cNumPla  VARCHAR2,       -- 16
                                 p_cNumApl  VARCHAR2,       -- 17
                                 p_dFecVia  VARCHAR2,       -- 18  fecha en formato (yyyymmdd)
                                 p_dFecApl  VARCHAR2,       -- 19  fecha en formato (yyyymmdd)
                                 p_cPreven  VARCHAR2,       -- 20
                                 p_nPriMin  NUMBER,         -- 21
                                 p_cConPro  VARCHAR2,       -- 22
                                 p_cPaiPro  VARCHAR2,       -- 23
                                 p_cDepPro  VARCHAR2,       -- 24
                                 p_cConDes  VARCHAR2,       -- 25
                                 p_cPaiDes  VARCHAR2,       -- 26
                                 p_cCodDep  VARCHAR2,       -- 27
                                 p_cCodTr   VARCHAR2,       -- 28
                                 p_cTipTra  VARCHAR2,       -- 29
                                 p_cTipMer  VARCHAR2,       -- 30
                                 p_cDesMer  VARCHAR2,       -- 31
                                 p_cTipEmb  VARCHAR2,       -- 32
                                 p_cDesEmb  VARCHAR2,       -- 33
                                 p_nTasBas  NUMBER,         -- 34
                                 p_cDeduci  VARCHAR2,       -- 35
                                 p_cEndosa  VARCHAR2,       -- 36
                                 p_cMedTra  VARCHAR2,       -- 37
                                 p_cCodBuq  VARCHAR2,       -- 38
                                 p_nNumVia  NUMBER,         -- 39
                                 p_nPorSeg  NUMBER,         -- 40
                                 p_nDerAdu  NUMBER,         -- 41
                                 p_cTransb  VARCHAR2,       -- 42
                                 p_cSobCub  VARCHAR2,       -- 43
                                 p_nAntBuq  NUMBER,         -- 44
                                 p_cGueHue  VARCHAR2,       -- 45
                                 p_cCodDto  VARCHAR2,       -- 46
                                 p_nMonApl  NUMBER,         -- 47
                                 p_nTasTot  NUMBER,         -- 48
                                 p_nPriTot  NUMBER,         -- 49
                                 p_cResPag  VARCHAR2,       -- 50
                                 p_cDesApl  VARCHAR2,       -- 51
                                 p_cDocRef  VARCHAR2,       -- 52
                                 p_cProvee  VARCHAR2,       -- 53
                                 p_cPedFac  VARCHAR2,       -- 54
                                 p_cDesDes  VARCHAR2,       -- 55
                                 p_nPesBru  VARCHAR2,       -- 56
                                 p_ncodAseg DEC_TRANSPORTE_FLOTANTE.NUMIDASEGWEB%TYPE DEFAULT NULL --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106
                                 --<I N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
                                 ,p_nNumFactura      VARCHAR2 DEFAULT NULL
                                 ,p_nDescNave        VARCHAR2 DEFAULT NULL
                                 ,p_nPuerAerOrigen   VARCHAR2 DEFAULT NULL
                                 --<F N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
                                 ) RETURN PR_API_EMI_TRAN.c_Cursor IS
--
c_Certificado c_Cursor;
--
cIndPol        VARCHAR2(1) DEFAULT 'N'; -- indicador de pol?ticas
cCodMon        VARCHAR2(3);
--
nTasCam        NUMBER;
nSobSeg        NUMBER;
nSumAse        NUMBER;
--
nSw            PLS_INTEGER DEFAULT 0;
n              PLS_INTEGER;
--
nIdeDec        DEC_TRANSPORTE_FLOTANTE.idedec%TYPE;
cCodPro        DAT_TRANSPORTE_FLOTANTE.codproveedor%TYPE;
--<I N3019624> Miguel Wilson / 14-07-2004 / Funcionalidad de nuevo responsable de pago
cResPag        DEC_TRANSPORTE_FLOTANTE.codcli%TYPE;
--<F N3019624>
cCodError      VARCHAR2(5) := '0';
cDesError      VARCHAR2(2000) := '';
cIndExisteOper VARCHAR(1);
nIdeRec DEC_TRANSPORTE_FLOTANTE.IDEREC%TYPE;--<N3035448> Ronald Or? - 30/07/2010 - Se impide reprocesar aplicaciones
cAplicacionGloria DEC_TRANSPORTE_FLOTANTE.APLICACION%TYPE;--<N3037167> Marlon Castro G. - 30/05/2011 - Adecuaciones Grupo Gloria
cContratGloria VARCHAR2(1);--<N3037167> Marlon Castro G. - 30/05/2011 - Adecuaciones Grupo Gloria
--<I N3038699> Ronald Ore - 03/04/2012 - HelpDesk 104296
cIndValDec     varchar2(2);
nContDec       number;
--<F N3038699> Ronald Ore - 03/04/2012 - HelpDesk 104296

-- <I N3039819> CNT-01590 / Renzo Yacarini / 19-09-2012
nNumPol        POLIZA.NUMPOL%TYPE;
bContinuar     BOOLEAN;
cMensaje       RIM_TAB_GRAL.DESCADIC%TYPE;
-- <F N3039819> CNT-01590 / Renzo Yacarini / 19-09-2012

-- <I N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
nNumLiq         NUMBER;
dFecEmi         DATE;
nDerEmi         NUMBER(14,2);
nExiste         NUMBER;
-- <F N3040860> HD 117200 / Renzo Yacarini / 10-05-2013

BEGIN
   -- <I N3019132> Ricardo Larrabure / 02-06-2004 / Reemplazo var.USER por usuario de la aplicaci?n
   PR_SESION.CARGAR_USUARIO_APP(p_nUsrId);
   -- <F N3019132>
   nIdeDec := p_nIdeDec;
   --
   IF SUBSTR(p_cCodTra,1,3) = '009' THEN
      IF p_nIdeDec IS NULL THEN  -- Insertar y Emitir
         nSw := 1;
      ELSE                       -- Actualizar y Emitir
         nSw := 2;
      END IF;
   ELSIF SUBSTR(p_cCodTra,1,3) = '004' THEN
      nSw := 1;
   ELSIF SUBSTR(p_cCodTra,1,3) = '005' THEN
      nSw := 2;
   END IF;

   PR_VINISUSC.cargar;
   cCodMon := PR_VINISUSC.Codigo_Moneda;
   nTasCam := PR.TASA_CAMBIO(p_cCodMon, cCodMon, SYSDATE,'D');
   nSobSeg := p_nMonApl * p_nPorSeg / 100;
   nSumAse := p_nMonApl + nSobSeg;
   BEGIN
      SELECT CodProveedor
       INTO cCodPro
       FROM DAT_TRANSPORTE_FLOTANTE
      WHERE IdePol      = p_nIdePol
        AND NumCert     = p_nNumCer
        AND CodRamoCert = p_cCodRam;
   EXCEPTION
      WHEN OTHERS THEN
         cCodPro := NULL;
   END;

   --<I N3019624> Miguel Wilson / 14-07-2004 / Agregar contratante nuevo
   IF SUBSTR(p_cCodTra,1,3) IN ('004','005','009') THEN
      IF p_cResPag IS NOT NULL THEN
         IF ES_CONTRATANTE(p_nIdePol,p_cResPag) = 'N' THEN
            AGREGAR_CONTRATANTE(p_nIdePol,p_cResPag);
         END IF;
      END IF;
   END IF;
   --<F N3019624>

   --<I N3037167> Marlon Castro G. - 30/05/2011 - Adecuaciones Grupo Gloria
   cContratGloria := PR_API_EMI_TRAN.ES_GRUPO_GLORIA(p_nIdePol);
   --<F N3037167> Marlon Castro G. - 30/05/2011 - Adecuaciones Grupo Gloria

   IF nSw = 1 THEN
      -- Aqui hacer el insert
      SELECT SQ_DECLARACION.NEXTVAL
        INTO nIdeDec
        FROM SYS.DUAL;

      BEGIN
         INSERT INTO dec_transporte_flotante
              (idedec,                                             -- 1
               idepol,                                             -- 2
               numcert,                                            -- 3
               codramocert,                                        -- 4
               stsdec,                                             -- 5
               fecsts,                                             -- 6
               numdec,                                             -- 7
               tipodec,                                            -- 8
               fecinivalid,                                        -- 9
               fecfinvalid,                                        -- 10
               fechado,                                            -- 11
               origen,                                             -- 12
               destino,                                            -- 13
               tipobien,                                           -- 14
               via,                                                -- 15
               mtoliqlocal,                                        -- 16
               tasa,                                               -- 17
               prima,                                              -- 18
               primamoneda,                                        -- 19
               mtoliqmoneda,                                       -- 20
               codmoneda,                                          -- 21
               transpor,                                           -- 22
               consignatario,                                      -- 23
               consignador,                                        -- 24
               beneficiario,                                       -- 25
               iderec,                                             -- 26
               vencpoliza,                                         -- 27
               tipotransporte,                                     -- 28
               tipomercancia,                                      -- 29
               tipoembalaje,                                       -- 30
               planilla,                                           -- 31
               codcontinente,                                      -- 32
               codpais,                                            -- 33
               codestado,                                          -- 34
               codbuque,                                           -- 35
               numviaje,                                           -- 36
               descdec,                                            -- 37
               desctran,                                           -- 38
               sobreseg,                                           -- 39
               mtoderaduana,                                       -- 40
               indcubierta,                                        -- 41
               indtransbordo,                                      -- 42
               porcantiguedad,                                     -- 43
               inddctocont,                                        -- 44
               numpedido,                                          -- 45
               proveedor,                                          -- 46
               docreferencia,                                      -- 47
               baseavaluo,                                         -- 48
               codproveedor,                                       -- 49
               pesobruto,                                          -- 50
               descmercancia,                                      -- 51
               descembalaje,                                       -- 52
               codcontinentedest,                                  -- 53
               codpaisdest,                                        -- 54
               codestadodest,                                      -- 55
               descdestino,                                        -- 56
               indclauguerra,                                      -- 57
               tasatotal,                                          -- 58
               aplicacion,                                         -- 59
               mtoliqmonedadec,                                    -- 60
               deducible,                                          -- 61
               indsisorig,                                         -- 62
               descbenef,                                          -- 63
               numcarta,                                           -- 64
               codcli,                                             -- 65
               NumTramite,                                         -- 66
               NUMIDASEGWEB  --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106                                      -- 67
               --<I N3035534> Marlon Castro G. - 02/08/2010 - MEJPROC-00156
               ,NUMIDUSER                                          -- 68
               ,ROLUSER                                            -- 69
               ,IDUSER                                             -- 70
               --<F N3035534>
               --<I N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
               ,NUMFACT                                            -- 71
               ,DESCNAVE                                           -- 72
               ,PUERTOAEROPUERTOORIGEN                             -- 73
               --<F N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
              )
         VALUES
            (nIdeDec,                                              -- 1
             p_nIdePol,                                            -- 2
             p_nNumCer,                                            -- 3
             p_cCodRam,                                            -- 4
             'VAL',                                                -- 5
             trunc(SYSDATE),                                       -- 6
             nIdeDec,                                              -- 7
             'D',                                                  -- 8
             NULL,                                                 -- 9
             to_date(p_dFecVia,'yyyymmdd'),                        -- 10
             to_date(p_dFecApl,'yyyymmdd'),                        -- 11
             NULL,                                                 -- 12
             NULL,                                                 -- 13
             NULL,                                                 -- 14
             NULL,                                                 -- 15
             p_nMonApl * nTasCam,                                  -- 16
             p_nTasBas,                                            -- 17
             p_nPriTot * nTasCam,                                  -- 18
             p_nPriTot,                                            -- 19
             p_nMonApl,                                            -- 20
             p_cCodMon,                                            -- 21
             NULL,                                                 -- 22
             NULL,                                                 -- 23
             NULL,                                                 -- 24
             p_cPreVen,                                            -- 25
             NULL,                                                 -- 26
             NULL,                                                 -- 27
             p_cCodTr,                                             -- 28
             p_cTipMer,                                            -- 29
             p_cTipEmb,                                            -- 30
             p_cNumPla,                                            -- 31
             p_cConPro,                                            -- 32
             p_cPaiPro,                                            -- 33
             p_cDepPro,                                            -- 34
             p_cCodBuq,                                            -- 35
             decode(p_nNumVia,0,NULL,p_nNumVia),                   -- 36
             p_cDesApl,                                            -- 37
             p_cMedTra,                                            -- 38
             p_nPorSeg,                                            -- 39
             p_nDerAdu,                                            -- 40
             p_cSobCub,                                            -- 41
             p_cTransb,                                            -- 42
             p_nAntBuq,                                            -- 43
             p_cCodDto,                                            -- 44
             p_cPedFac,                                            -- 45
             p_cProvee,                                            -- 46
             p_cDocRef,                                            -- 47
             p_cBasAva,                                            -- 48
             cCodPro,                                              -- 49
             p_nPesBru,                                            -- 50
             p_cDesMer,                                            -- 51
             p_cDesEmb,                                            -- 52
             p_cConDes,                                            -- 53
             p_cPaiDes,                                            -- 54
             p_cCodDep,                                            -- 55
             p_cDesDes,                                            -- 56
             p_cGueHue,                                            -- 57
             p_nTasTot,                                            -- 58
             p_cNumApl,                                            -- 59
             nvl(nSumAse,0) + nvl(p_nDerAdu,0),                    -- 60
             p_cDeduci,                                            -- 61
             'W',                                                  -- 62
             p_cEndosa,                                            -- 63
             p_nNumCar,                                            -- 64
             p_cResPag,                                            -- 65
             p_cNumTra,                                            -- 66
             p_ncodAseg  --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106                                            -- 67 --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106
             --<I N3035534> Marlon Castro G. - 02/08/2010 - MEJPROC-00156
             ,p_nUsrId                                             -- 68
             ,p_cUsrRol                                            -- 69
             ,p_cUsrLog                                            -- 70
             --<F N3035534>
             --<I N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
             ,p_nNumFactura                                        -- 71
             ,p_nDescNave                                          -- 72
             ,p_nPuerAerOrigen                                     -- 73
             --<F N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
            );
      EXCEPTION
         WHEN OTHERS THEN
            RAISE_APPLICATION_ERROR(-20100,'No se pudo insertar el registro' || SQLERRM);
      END;

   ELSIF nSw = 2 THEN
      -- Modificar / Actualizar
      UPDATE dec_transporte_flotante
         SET idepol            = p_nIdePol,
             numcert           = p_nNumCer,
             codramocert       = p_cCodRam,
             stsdec            = 'VAL',
             fecsts            = trunc(SYSDATE),
             numdec            = nIdeDec,
             tipodec           = 'D',
             fecfinvalid       = to_date(p_dFecVia,'yyyymmdd'),
             fechado           = to_date(p_dFecApl,'yyyymmdd'),
             mtoliqlocal       = p_nMonApl * nTasCam,
             tasa              = p_nTasBas,
             prima             = p_nPriTot * nTasCam,
             primamoneda       = p_nPriTot,
             mtoliqmoneda      = p_nMonApl,
             codmoneda         = p_cCodMon,
             beneficiario      = p_cPreVen,
             tipotransporte    = p_cCodTr,
             tipomercancia     = p_cTipMer,
             tipoembalaje      = p_cTipEmb,
             planilla          = p_cNumPla,
             codcontinente     = p_cConPro,
             codpais           = p_cPaiPro,
             codestado         = p_cDepPro,
             codbuque          = p_cCodBuq,
             numviaje          = p_nNumVia,
             descdec           = p_cDesApl,
             desctran          = p_cMedTra,
             sobreseg          = p_nPorSeg,
             mtoderaduana      = p_nDerAdu,
             indcubierta       = p_cSobCub,
             indtransbordo     = p_cTransb,
             porcantiguedad    = p_nAntBuq,
             inddctocont       = p_cCodDto,
             numpedido         = p_cPedFac,
             proveedor         = p_cProvee,
             docreferencia     = p_cDocRef,
             baseavaluo        = p_cBasAva,
             codproveedor      = cCodPro,
             pesobruto         = p_nPesBru,
             descmercancia     = p_cDesMer,
             descembalaje      = p_cDesEmb,
             codcontinentedest = p_cConDes,
             codpaisdest       = p_cPaiDes,
             codestadodest     = p_cCodDep,
             descdestino       = p_cDesDes,
             indclauguerra     = p_cGueHue,
             tasatotal         = p_nTasTot,
             aplicacion        = p_cNumApl,
             mtoliqmonedadec   = nvl(nSumAse,0) + nvl(p_nDerAdu,0),
             deducible         = p_cDeduci,
             indsisorig        = 'W',
             descbenef         = p_cEndosa,
             numcarta          = p_nNumCar,
             codcli            = p_cResPag,
             NumTramite        = p_cNumTra,
             NUMIDASEGWEB      = p_ncodAseg --<N3034931> Jose Porlles 27/05/2010  MEJPROC-00106
             --<I N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
             ,NUMFACT           = p_nNumFactura
             ,DESCNAVE          = p_nDescNave
             ,PUERTOAEROPUERTOORIGEN = p_nPuerAerOrigen
             --<F N3039670> TRANS-00033 / Italo Rivas / 13-09-2012
       WHERE IdeDec = nIdeDec
       AND IDEREC IS NULL;--<N3035448> Ronald Or? - 30/07/2010 - Se impide reprocesar aplicaciones

   END IF;

   IF SUBSTR(p_cCodTra,1,3) = '006' THEN
      IF p_cCodEst = 'VAL' THEN
        -- <I N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
         DELETE DEC_TRANSPORTE_FLOTANTE_ADIC
         WHERE IdeDec = p_nIdeDec;
        -- <F N3040860> HD 117200 / Renzo Yacarini / 10-05-2013

         DELETE FROM dec_transporte_flotante
               WHERE IdeDec = p_nIdeDec;
      ELSE
         cCodError := '1';
         cDesError := 'No es posible eliminar el registro debido a que se encuentra en estado ' || P_cCodEst;
      END IF;
   END IF;

   --<I N3035448> Ronald Or? - 30/07/2010 - Se impide reprocesar aplicaciones
   IF SUBSTR(p_cCodTra,1,3) IN ('005','008','009') AND p_nIdeDec IS NOT NULL THEN
     BEGIN
      IF SUBSTR(p_cCodTra,1,3) = '008' THEN
        UPDATE DEC_TRANSPORTE_FLOTANTE
        SET    fecsts = trunc(SYSDATE)
        WHERE  IdeDec = p_nIdeDec;
      END IF;

      SELECT IdeRec INTO nIdeRec
      FROM dec_transporte_flotante d where D.Idedec=p_nIdeDec;

      IF(nIdeRec IS NOT NULL) THEN
        cCodError:= -4;
        cDesError:= 'La aplicaci?n esta siendo procesada. Por favor, volver a consultar el certificado.';
      END IF;
     EXCEPTION
      WHEN OTHERS THEN
        nIdeRec:=NULL;
     END;
   END IF;
   --<F N3035448> Ronald Or? - 30/07/2010 - Se impide reprocesar aplicaciones

   IF SUBSTR(p_cCodTra,1,3) IN ('008','009') AND cCodError = '0' THEN  -- Emitir
      -- Validar que el usuario tenga las operaciones para activar una declaraci?n
      BEGIN
        SELECT 'S'
        INTO   cIndExisteOper
        FROM   OPER_USUARIO OU, USUARIO U
        WHERE  OU.CodUsr    = U.CodUsr
        AND    OU.CodGrpUsr = U.CodGrpUsr
        AND    U.CodUsr     = PR_SESION.USUARIO_APP
        AND    OU.CodOper   = '015';
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           cCodError := '1';
           cDesError := 'El usuario no cuenta con la permisos necesarios para activar aplicaciones. Favor comun?quese con Rimac y solicite los accesos (operaciones) respectivos.';
        WHEN OTHERS THEN
           NULL;
      END;

      IF cCodError = '0' THEN

         --<I N3039819> CNT-01590 / Renzo Yacarini / 19-09-2012

         bContinuar := TRUE;

         -- validacion de declaracion para poliza facultativa
         cMensaje := PR_API_EMI_TRAN.VALIDA_DECLARACION_FACULTATIVA(p_nIdePol, p_nNumCer, NULL);

         IF (cMensaje IS NOT NULL) THEN
           cCodError := '1';
           cDesError := cMensaje;
           bContinuar := FALSE;

         END IF;

         IF (bContinuar) THEN
           --<F N3039819> CNT-01590 / Renzo Yacarini / 19-09-2012

         -- Validar Inconsistencias
         n := PR_POLIZA_INCONSISTENCIA.VALIDAR_ACTIVACION(p_nIdePol,'D');
         IF n = 0 THEN
            SAVEPOINT PROCESO_POLIZA;
            -- <I N3019624> Miguel Wilson / 16-07-2004 / Obtener el codigo del contratante
            BEGIN
              SELECT CodCli
                INTO cResPag
                FROM DEC_TRANSPORTE_FLOTANTE
               WHERE Idedec = nIdeDec;

            EXCEPTION
               WHEN OTHERS THEN
                 cResPag := NULL;
            END;
            -- <F N3019624>
            BEGIN
               --Generar la data para el facultativo
               --CREA_DIST_FACUL(p_cUsrLog,p_nIdePol,p_nNumCer,NVL(p_cCodRam,'TRAN'),nIdeDec); --<N3039670> TRANS-00033 / Renzo Yacarini / 19-09-2012
               --Activar aplicaci?n de transportes (devuelve si se violaron pol?ticas)
               cIndPol := INCLUIR_POLIZA(p_cUsrLog,p_nIdePol,p_nNumCer,NVL(p_cCodRam,'TRAN'),p_cCodEst,nIdeDec,cResPag,p_cNumTra);
               IF cIndPol = 'S' THEN
                  ROLLBACK TO Proceso_Poliza;
                  cCodError := '1';
                  cDesError := 'La declaraci?n no cumple con las pol?ticas de subscripci?n requeridas. Favor comun?quese con Rimac para su activaci?n';
               END IF;
               --<I N3038699> Ronald Ore - 03/04/2012 - HelpDesk 104296
               SELECT VALOREF1 INTO cIndValDec
               FROM RIM_TAB_GRAL
               WHERE CODTAB='VALDEC' AND LLAVETAB='VALOP';

               IF(cIndValDec = 'S') THEN
                 select count(1) into nContDec
                   from dec_transporte_flotante
                  where iderec in
                        (select iderec
                           from recibo
                          where ideop in
                                (select ideop
                                   from recibo r, dec_transporte_flotante d
                                  where d.idedec = nIdeDec
                                    and d.iderec = r.iderec));
                 IF(nContDec>1) THEN
                    ROLLBACK TO PROCESO_POLIZA;
                    cCodError := '1';
                    cDesError := 'Error al procesar la aplicaci?n, hay mas de una declaracion en la activacion, favor comun?quese con Rimac.';
                 END IF;
               END IF;
               --<F N3038699> Ronald Ore - 03/04/2012 - HelpDesk 104296
            EXCEPTION
               WHEN OTHERS THEN
                  ROLLBACK TO PROCESO_POLIZA;
                  cCodError := '1';
                  cDesError := 'Error al procesar la aplicaci?n, favor comun?quese con Rimac'||CHR(13)||SUBSTR(SQLERRM,1,400);
            END;
         ELSE
            cCodError := '1';
            cDesError := 'La p?liza/certificado no est? correctamente configurada. Favor comun?quese con Rimac para su activaci?n y correcci?n';
         END IF;

         END IF; --<N3039819> CNT-01590 / Renzo Yacarini / 19-09-2012

      END IF;

      --<I N3037167> Marlon Castro G. - 31/05/2011
      IF(cCodError = '0' AND cContratGloria = 'S') THEN

        cAplicacionGloria := PR_API_EMI_TRAN.GET_APLICACION_TRAN(p_nIdePol);

        UPDATE DEC_TRANSPORTE_FLOTANTE --Actualizamos la aplicaci?n correlativa
           SET APLICACION = cAplicacionGloria
         WHERE IDEDEC = nIdeDec;

      END IF;
      --<F N3037167> Marlon Castro G. - 31/05/2011

   END IF;

   IF cCodError > '0' THEN
      cIndPol := 'S';
   ELSE
      cIndPol := 'N';
   END IF;
   --Actualizar indicador de pol?ticas violadas en la aplicaci?n
   UPDATE DEC_TRANSPORTE_FLOTANTE
   SET    IndPolitica = cIndPol
   WHERE  IdeDec = nIdeDec;

    -- <I N3040860> HD 117200 / Renzo Yacarini / 10-05-2013
    BEGIN
      SELECT IdeRec
      INTO nIdeRec
      FROM dec_transporte_flotante d
      where D.Idedec=nIdeDec;
    EXCEPTION
      WHEN OTHERS THEN
        nIdeRec := NULL;
    END;

    nNumLiq := PR_API_EMI_TRAN.NUMERO_LIQUIDACION(nIdeRec);
    dFecEmi := PR_API_EMI_TRAN.FECHA_EMISION(nIdeDec);
    nDerEmi := PR_API_EMI_TRAN.OBTENER_DERECHO_EMISION(nIdeDec);

    BEGIN
      SELECT COUNT(1)
      INTO nExiste
      FROM DEC_TRANSPORTE_FLOTANTE_ADIC T
      WHERE T.IDEDEC = nIdeDec;

    EXCEPTION
      WHEN OTHERS THEN
        nExiste := 0;
    END;

    BEGIN
      IF nExiste = 0 THEN
        INSERT INTO DEC_TRANSPORTE_FLOTANTE_ADIC (IDEDEC, NUMLIQ, FECEMI, DEREMI)
        VALUES (nIdeDec, nNumLiq, dFecEmi, nDerEmi);
      ELSE
        UPDATE DEC_TRANSPORTE_FLOTANTE_ADIC
           SET NUMLIQ = nNumLiq,
               FECEMI = dFecEmi,
               DEREMI = nDerEmi
         WHERE IDEDEC = nIdedec;
      END IF;

    EXCEPTION
      WHEN OTHERS THEN
        NULL;
    END;
    -- <F N3040860> HD 117200 / Renzo Yacarini / 10-05-2013

   PR_SESION.Cargar_Usuario_App(NULL); -- <N3025449> Se setea a null la variable de sesi?n

   OPEN c_Certificado FOR
      SELECT nIdeDec IdeDec,
             nNumLiq NumLiq,
             cCodError CodError,
             cDesError DesError
        FROM DUAL;

   RETURN( c_Certificado );
END PROCESAR_APLICACION;

  /*-----------------------------------------------------------------------
    Nombre     : CONSULTA_DETALLE_LIQUIDACION
    Proposito  : Devuelve informaci?n detallada de la liquidaci?n
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nNumLiq  N?mero de liquidaci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           05/04/2004    Roberto Carranza    Creaci?n
           10/05/2004    Roberto Carranza    Se quit? la inserci?n a la
                                             tabla Transac_Tramite
           09/07/2004    Ricardo Larrabure   Correcci?n c?digo de barra (N3019580)
           08/02/2012   Jhonatan Rojas       <N3038436> TRANS-00027 / T. Modificaci?n(N3038436)
           23/02/2012   Jhonatan Rojas       <N3038436> TRANS-00027 / T. Modificaci?n(N3038436)
           25/11/2013   Italo Rivas          <N3041754> TRANS-00037 / Modificaciones
   ------------------------------------------------------------------------*/
   FUNCTION DETALLE_LIQUIDACION (p_cCodTra  VARCHAR2,
                                 p_cUsrLog  VARCHAR2,
                                 p_nUsrId   NUMBER,
                                 p_cUsrRol  VARCHAR2,
                                 p_cUsrPer  VARCHAR2,
                                 p_nNumLiq  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor IS
    --
    c_Certificado c_Cursor;
    --
    BEGIN
     --
     OPEN c_Certificado FOR
          SELECT PR_API_EMI_TRAN.fecha_en_letras(nvl(p.FecOper,p.FecEmi)) FECHA_LIQUIDACION,
                 --PR_IMPRIME_POLIZA.nombre_persona('N',F.numid) Asegurado,
                 -- <I N3038436> Jhonatan Rojas Terrones / 07-02-2012 / Traer nombre del responsable de pago en vez del nombre del asegurado
                 --PR_IMPRIME_POLIZA.nombre_persona('C',P.CodCli) Asegurado, --<I N3019624> Ricardo Larrabure / 14-07-2004 / Nombre de contratante
                 PR_API_EMI_TRAN.BUSCA_ASEG_RESPAGO_CONTR('RP',p_nNumLiq) Asegurado,
                 -- <F N3038436> Jhonatan Rojas Terrones / 07-02-2012 / Traer nombre del responsable de pago en vez del nombre del asegurado
                 --<I N3041754> Modificaciones TRANS-00037 / Italo Rivas / 25-11-2013
                 --PR_API_EMI_TRAN.Direccion_Asegurado(F.numoper) Direccion,
                 --PR_API_EMI_TRAN.Distrito_Asegurado(F.numoper)  Distrito,
                 --PR_API_EMI_TRAN.provincia_asegurado(F.numoper) Provincia,
                 PR_API_EMI_TRAN.OBTENER_DIRECCION_RESPAGO('DR',p_nNumLiq) Direccion,
                 PR_API_EMI_TRAN.OBTENER_DIRECCION_RESPAGO('DS',p_nNumLiq) Distrito,
                 PR_API_EMI_TRAN.OBTENER_DIRECCION_RESPAGO('PR',p_nNumLiq) Provincia,
                 --<F N3041754> Modificaciones TRANS-00037 / Italo Rivas / 25-11-2013
                 P.Numpol,
                 PR_API_EMI_TRAN.valida_certificados(P.IdePol,F.NumOper) NumCert,
                 PR_API_EMI_TRAN.Num_Documento(F.idefact) Num_Documento,
                 F.numfact Num_Liquidacion,
                 --[INI] Modificaciones Marlon Castro G. 05/04/2013 HD-116054
                 --TRUNC(O.fechahoraini) Fecha_Operacion,
                 TRUNC(O.Fecmov) Fecha_Operacion,
                 --[FIN] Modificaciones Marlon Castro G. 05/04/2013 HD-116054
                 P.codprod,
                 PR_PRODUCTO.desc_prod(P.Codprod) Des_Producto,
                 -- <I N3038436> Jhonatan Rojas Terrones / 07-02-2012 / Traer nombre del contratante en vez del nombre del asegurado
                 -- PR_API_EMI_TRAN.contratante(F.NumFact) Contratante,
                 --PR_API_EMI_TRAN.BUSCA_ASEG_RESPAGO_CONTR('C',P.IDEPOL) Contratante,
                  -- <I N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Traer nombre del responsable de pago en vez del nombre del asegurado
                  PR_API_EMI_TRAN.BUSCA_ASEG_RESPAGO_CONTR('RP',p_nNumLiq) Contratante,
                  -- <F N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Traer nombre del responsable de pago en vez del nombre del asegurado
                  -- <F N3038436> Jhonatan Rojas Terrones / 07-02-2012 / Traer nombre del responsable de pago en vez del nombre del asegurado
                 P.codmoneda     Mon_Prima_Pagar,
                 F.mtofactmoneda Prima_Pagar,
                 PR_API_EMI_TRAN.forma_pago(F.Idefact) Forma_Pago,
                 P.codmoneda     Moneda,
                 F.mtofactmoneda Importe,
                 PR_API_EMI_TRAN.fec_primer_pago(P.Fecinivig,P.Fecfinvig,nvl(p.FecOper,p.FecEmi)) Fecha_Pago,
                 PR_API_EMI_TRAN.codigo_barra(P.CodMoneda,F.mtofactmoneda,F.NumFact) Codigo_Barra

            --[INI] Modificaciones Marlon Castro G. 05/04/2013 HD-116054
            /*
            FROM poliza P, operacion O, factura F
           WHERE P.idepol  = O.idobjetoop
             AND F.NumOper = O.IdeOp
             AND F.NumFact = p_nNumLiq;
            */
            FROM poliza P, oper_pol O, factura F
           WHERE P.idepol  = O.idepol
             AND F.NumOper = O.numoper
             AND F.NumFact = p_nNumLiq
             AND ROWNUM = 1;
            --[FIN] Modificaciones Marlon Castro G. 05/04/2013 HD-116054

     --
     RETURN(c_Certificado);
    END DETALLE_LIQUIDACION;
   /*-----------------------------------------------------------------------
    Nombre     : LIQUIDACION_PLANILLA
    Proposito  : Proceso de liquidaci?n de planilla
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_cNumPla  N?mero de planilla
                  p_cCodRes  C?digo de responsale de pago
                  p_nIdePol  Identificador de poliza
    Log de Cambios
           Fecha            Autor            Descripcion
           31/03/2004    Roberto Carranza    Creaci?n
           14/07/2004    Miguel Wilson       Validar inconsistencias de la p?liza
           03/06/2006    Jose Porlles C.     <N3025449> Se setea a null la variable de sesi?n
           16/02/2007    Jose Collantes      <N3026906> Se agreg? RESUMIR_MOD_COBERT_DEC_TRAN. HD-47878
           21/05/2013    Renzo Yacarini      <N3040916> HD 117806
   -------------------------------------------------------------------------*/
   FUNCTION LIQUIDACION_PLANILLA (p_cCodTra  VARCHAR2,
                                  p_cUsrLog  VARCHAR2,
                                  p_nUsrId   NUMBER,
                                  p_cUsrRol  VARCHAR2,
                                  p_cUsrPer  VARCHAR2,
                                  p_cNumPla  VARCHAR2,
                                  p_cCodRes  VARCHAR2,
                                  p_nIdePol  NUMBER ) RETURN PR_API_EMI_TRAN.c_Cursor IS

    --
    p_cIndPol VARCHAR2(1);
    --
    nNumPol   NUMBER;
    nSw       PLS_INTEGER := 0;
    --<I N3019624> Miguel Wilson / 14-07-2004 / Validar inconsistencias de la p?liza
    nNumIncon         PLS_INTEGER;
    --<F N3019624>
    --
    cCodPro   POLIZA.codprod%TYPE;

    -- <I N3040916> HD 117806 / Renzo Yacarini / 21-05-2013
    nNumLiq         NUMBER;
    dFecEmi         DATE;
    nDerEmi         NUMBER(14,2);
    nExiste         NUMBER;
    nIdeRec         NUMBER;
    -- <F N3040916> HD 117806 / Renzo Yacarini / 21-05-2013
    --
    c_Certificado c_Cursor;
    --
    CURSOR cLiquida(p_cEstado IN VARCHAR2) IS -- <N3040916> HD 117806 / Renzo Yacarini / 21-05-2013
      SELECT A.rowid,A.idedec,A.idepol,A.numcert,A.codramocert,A.stsdec,
             A.fecsts,A.numdec,A.tipodec,A.fecinivalid,A.fecfinvalid,
             A.fechado,A.origen,A.destino,A.tipobien,A.via,
             A.mtoliqlocal,A.tasa,A.prima,A.primamoneda,A.mtoliqmoneda,
             A.codmoneda,A.transpor,A.consignatario,A.consignador,
             A.beneficiario,A.iderec,A.vencpoliza,A.tipotransporte,
             A.tipomercancia,A.tipoembalaje,A.planilla,A.codcontinente,
             A.codpais,A.codestado,A.codbuque,A.numviaje,A.descdec,
             A.desctran,A.sobreseg,A.mtoderaduana,A.indcubierta,A.indtransbordo,
             A.porcantiguedad,A.inddctocont,A.numpedido,A.proveedor,A.docreferencia,
             A.baseavaluo,A.codproveedor,A.pesobruto,A.descmercancia,A.descembalaje,
             A.codcontinentedest,A.codpaisdest,A.codestadodest,A.descdestino,A.indclauguerra,
             A.tasatotal,A.aplicacion,A.mtoliqmonedadec,A.deducible,A.indsisorig,
             A.descbenef,A.numcarta,A.codcli,
             b.codprod,
             b.NumPol
        FROM dec_transporte_flotante a,poliza b
       WHERE a.idepol   = b.idepol
         AND a.idepol   = p_nIdePol
         AND a.planilla = p_cNumPla
         AND a.stsdec   = p_cEstado; -- 'VAL'; -- <N3040916> HD 117806 / Renzo Yacarini / 21-05-2013

    --
    BEGIN
      --
      -- <I N3019132> Ricardo Larrabure / 02-06-2004 / Reemplazo var.USER por usuario de la aplicaci?n
      PR_SESION.CARGAR_USUARIO_APP(p_nUsrId);
      -- <F N3019132>

      IF Valida_Parametros_Principales(p_cCodTra,p_cUsrLog,p_nUsrId,p_cUsrRol,p_cUsrPer) THEN
         --
         IF p_cNumPla IS NOT NULL AND p_nIdePol > 0 THEN
            --
            SAVEPOINT Procesa_Politica;
            FOR x IN cLiquida('VAL') LOOP -- <N3040916> HD 117806 / Renzo Yacarini / 21-05-2013
               IF TRIM(p_cCodRes) IS NOT NULL THEN
                  PR_API_EMI_TRAN.fr_backup_responsables(USERENV('sessionid'),'01',p_nIdePol,X.NumCert);
                  PR_API_EMI_TRAN.fr_carga_resp(p_nIdePol,X.NumCert,p_cCodRes,X.CodProd);
               END IF;
              -- Si la Declaraci?n esta en Estado VAL hay que incluirla...
              IF X.StsDec = 'VAL' THEN
                  PR_DEC_TRANSPORTE.incluir_f(X.IdeDec);
               END IF;
               -- Hay que generar el Movimiento de Coberturas para la Declaraci?n
               PR_DEC_TRANSPORTE.gen_mov_fg(p_nIdePol, X.numcert, X.codramocert, X.idedec);
               IF nSw = 0 THEN
                  nNumPol := X.NumPol;
                  cCodPro := X.CodProd;
                  nSw := 1;
               END IF;
               --
            END LOOP;
            PR_COBERT_CERT.RESUMIR_MOD_COBERT_DEC_TRAN(p_nIdePol,p_cNumPla); --<N3026906> Jose Collantes / 16-02-2007 / HD-47878
            -- <I N3019624> Miguel Wilson / 14-07-2004 / Agregar validacion de inconsistencias
            nNumIncon := PR_POLIZA_INCONSISTENCIA.VALIDAR_ACTIVACION(p_nIdePol,'D');
            IF nNumIncon > 0 THEN
               p_cIndPol := 'S';
            ELSE
            -- <F N3019624>
               -- Activaci?n de la P?liza
               VALIDA_POLITICAS(p_cUsrLog,p_nIdePol,nNumPol,cCodPro,p_cIndPol);
               -- Restaurar los valores
               IF TRIM(p_cCodRes) IS NOT NULL THEN
               PR_API_EMI_TRAN.fr_backup_responsables(USERENV('sessionid'),'02',NULL,NULL);
               END IF;
               --
            END IF;-- <N3019624>
            IF p_cIndPol = 'S' THEN
               ROLLBACK TO Procesa_Politica;
               FOR x IN cLiquida('VAL') LOOP -- <N3040916> HD 117806 / Renzo Yacarini / 21-05-2013
                 UPDATE dec_transporte_flotante
                    SET indpolitica = 'S'
                  WHERE ROWID = X.ROWID;
               END LOOP;
            END IF;
            --
         END IF;
         --
      END IF;

      PR_SESION.Cargar_Usuario_App(NULL); -- <N3025449> Se setea a null la variable de sesi?n

      -- <I N3040916> HD 117806 / Renzo Yacarini / 21-05-2013
      FOR x IN cLiquida('ACT') LOOP

        BEGIN
          SELECT IdeRec
          INTO nIdeRec
          FROM dec_transporte_flotante d
          where D.Idedec = x.idedec;
        EXCEPTION
          WHEN OTHERS THEN
            nIdeRec := NULL;
        END;

        nNumLiq := PR_API_EMI_TRAN.NUMERO_LIQUIDACION(nIdeRec);
        dFecEmi := PR_API_EMI_TRAN.FECHA_EMISION(x.idedec);
        nDerEmi := PR_API_EMI_TRAN.OBTENER_DERECHO_EMISION(x.idedec);

        BEGIN
          SELECT COUNT(1)
          INTO nExiste
          FROM DEC_TRANSPORTE_FLOTANTE_ADIC T
          WHERE T.IDEDEC = x.idedec;

        EXCEPTION
          WHEN OTHERS THEN
            nExiste := 0;
        END;

        BEGIN
          IF nExiste = 0 THEN
            INSERT INTO DEC_TRANSPORTE_FLOTANTE_ADIC (IDEDEC, NUMLIQ, FECEMI, DEREMI)
            VALUES (x.idedec, nNumLiq, dFecEmi, nDerEmi);
          ELSE
            UPDATE DEC_TRANSPORTE_FLOTANTE_ADIC
               SET NUMLIQ = nNumLiq,
                   FECEMI = dFecEmi,
                   DEREMI = nDerEmi
             WHERE IDEDEC = x.idedec;
          END IF;

        EXCEPTION
          WHEN OTHERS THEN
            NULL;
        END;
      END LOOP;
      -- <F N3040916> HD 117806 / Renzo Yacarini / 21-05-2013

      --
      OPEN c_Certificado FOR
       SELECT p_cIndPol IndPolitica
         FROM dual;
      --
      RETURN c_Certificado;
   END LIQUIDACION_PLANILLA;
   /*-----------------------------------------------------------------------
    Nombre     : LISTA_VALORES
    Proposito  : Devuelve la lista de valores seg?n criterio
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_TipLis   Tipo de lista de valores
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           16/03/2011    Renzo Yacarini      <N3036781> Req. TRANS-00016
   -------------------------------------------------------------------------*/
   FUNCTION LISTA_VALORES (p_cCodTra  VARCHAR2,
                           p_cUsrLog  VARCHAR2,
                           p_nUsrId   NUMBER,
                           p_cUsrRol  VARCHAR2,
                           p_cUsrPer  VARCHAR2,
                           p_cTipLis  VARCHAR2 ) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
    IF p_cTipLis IS NOT NULL THEN
       --
       IF p_cTipLis = 'TIPOBUQU' THEN
          OPEN c_Certificado FOR
            --
            SELECT L.TipoLval,
                   B.CodBuque CodlVal,
                   TRIM(B.NomBuque) || ' ' || B.AnoFab || ' ' || L.Descrip Descrip
              FROM LVAL L,BUQUE B
             WHERE L.TipoLval = 'TIPOBUQU'
               AND L.CodLval  = B.TipoBuque;
            --
       --<I N3036781> Renzo Yacarini / 16-03-2011
       ELSIF p_cTipLis = 'CORRPRIMREC' THEN
          OPEN c_Certificado FOR
            --
            SELECT L.CODTAB TipoLval,
                   SUBSTR(LLAVETAB,1, INSTR(LLAVETAB,'-')-1) CodlVal,
                   P.IDEPOL Descrip
            FROM RIM_TAB_GRAL L, POLIZA P, CLIENTE C
            WHERE L.CODTAB = 'CORRPRIMREC'
              AND L.STSTAB = 'ACT'
              AND C.NUMID = SUBSTR(LLAVETAB,INSTR(LLAVETAB,'-')+1)
              AND P.CODCLI = C.CODCLI
              AND P.CODPROD||'' = '3001';
            --
       --<F N3036781> Renzo Yacarini / 16-03-2011
       ELSE
         OPEN c_Certificado FOR
           --
           SELECT L.tipolval,
                  L.codlval,
                  L.descrip
             FROM lVal L
            WHERE L.tipolval = p_cTipLis;
           --
       END IF;
       --
    ELSE
      RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
    END IF;
    RETURN c_Certificado;
   END LISTA_VALORES;
   /*-----------------------------------------------------------------------
    Nombre     : DATOS_PART_TRANS
    Proposito  : Devuelve los datos particulares del certificado
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza
                  p_nNumCer  N?mero de certificado
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DATOS_PART_TRANS (p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nIdePol  NUMBER,
                              p_nNumCer  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
    IF p_nIdePol IS NOT NULL AND p_nNumCer IS NOT NULL THEN
        OPEN c_Certificado FOR
          --
          SELECT A.baseavaluo,
                 A.indderaduana,
                 A.maxembarque,
                 A.indnacional,
                 PR_IMPRIME_POLIZA.nombre_persona('N', B.numid) DescPreventor,
                 A.primamin,
                 A.porcderaduana,
                 A.porcalmalm,
                 A.porcalmpue,
                 A.maxsobreseg,
                 A.minsobreseg,
                 A.porccubierta,
                 A.porctransbordo,
                 A.codmoneda,
                 A.codramocert,
                 PR_API_EMI_TRAN.indicador_proceso(A.IdePol,A.NumCert) IndProceso
            FROM dat_transporte_flotante A, proveedor B
           WHERE A.codproveedor = B.codproveedor(+)
             AND A.idepol  = p_nIdePol
             AND A.numcert = p_nNumCer;
          --
    ELSE
      RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
    END IF;
    --
    RETURN c_Certificado;
   END DATOS_PART_TRANS;
   /*-----------------------------------------------------------------------
    Nombre     : DATOS_PART_TRANS_TASAS
    Proposito  : Devuelve los datos de tasa de mercader?a
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza
                  p_nNumCer  N?mero de certificado
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           14/07/2004    Miguel Wilson       Adicionar filtro de antiguedad de la tasa
   -------------------------------------------------------------------------*/
   FUNCTION DATOS_PART_TRANS_TASAS (p_cCodTra  VARCHAR2,
                                    p_cUsrLog  VARCHAR2,
                                    p_nUsrId   NUMBER,
                                    p_cUsrRol  VARCHAR2,
                                    p_cUsrPer  VARCHAR2,
                                    p_nIdePol  NUMBER,
                                    p_nNumCer  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
    -- <I N3019624> Miguel Wilson / 14-07-2004 / N?mero de meses a restringir la tasa
   nMes NUMBER;
   -- <F N3019624>
   c_Certificado c_Cursor;
   --
   BEGIN
    IF p_nIdePol IS NOT NULL AND p_nNumCer IS NOT NULL THEN
       -- <I N3019624> Miguel Wilson / 14-07-2004 / Obtiene el # de meses para el filtro de busqueda por fecha final
       BEGIN
         SELECT CodLval
           INTO nMes
           FROM LVAL
          WHERE TipoLval = 'LIMTASAT';
          nMes := nMes * -1;

         EXCEPTION
           WHEN NO_DATA_FOUND THEN
             nMes := 0;
           WHEN OTHERS THEN
             nMes := 0;
       END;
       -- <F N3019624>

       OPEN c_Certificado FOR
          --
          SELECT A.CodContinente,
                 PR.busca_lval('TIPOCONT',A.codcontinente) DescContinente,
                 A.TipoTransporte,
                 PR.busca_lval('TIPTRAN',A.TipoTransporte) DescTransporte,
                 A.TipoMercancia,
                 PR.busca_lval('TIPOMERC',A.TipoMercancia) DescMercancia,
                 A.TipoEmbalaje,
                 PR.busca_lval('TIPOEMBA',A.TipoEmbalaje) DescEmbalaje,
                 A.FecIniTasa,
                 A.FecFinTasa,
                 A.Tasa,
                 A.Deducible,
                 A.Descripcion
            FROM merca_embala_tasa A
           WHERE A.idepol  = p_nIdePol
             AND A.numcert = p_nNumCer
             AND A.FecFinTasa >= ADD_MONTHS(TRUNC(SYSDATE),nMes);-- < N3019624 > Filtro por fecha final
          --
    ELSE
      RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
    END IF;
    RETURN c_Certificado;
   END DATOS_PART_TRANS_TASAS;
   /*-----------------------------------------------------------------------
    Nombre     : DATOS_PART_TRANS_GUERRA
    Proposito  : Devuelve los datos de lista de guerra - huelga
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza
                  p_nNumCer  N?mero de certificado
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DATOS_PART_TRANS_GUERRA (p_cCodTra  VARCHAR2,
                                     p_cUsrLog  VARCHAR2,
                                     p_nUsrId   NUMBER,
                                     p_cUsrRol  VARCHAR2,
                                     p_cUsrPer  VARCHAR2,
                                     p_nIdePol  NUMBER,
                                     p_nNumCer  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
     IF p_nIdePol IS NOT NULL AND p_nNumCer IS NOT NULL THEN
         OPEN c_Certificado FOR
            --
            SELECT A.codContinente,
                   A.tasa
              FROM clausula_guerra A
             WHERE A.idepol  = p_nIdePol
               AND A.numcert = p_nNumCer;
              --
        ELSE
          RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
     END IF;
     RETURN c_Certificado;
   END DATOS_PART_TRANS_GUERRA;
   /*-----------------------------------------------------------------------
    Nombre     : DATOS_PART_TRANS_POL
    Proposito  : Devuelve los datos particulares de los certificados por poliza
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza

    Log de Cambios
           Fecha            Autor            Descripcion
           05-09-2006    Daniel Quintana     Creacion
   -------------------------------------------------------------------------*/
   FUNCTION DATOS_PART_TRANS_POL (p_cCodTra  VARCHAR2,
                                  p_cUsrLog  VARCHAR2,
                                  p_nUsrId   NUMBER,
                                  p_cUsrRol  VARCHAR2,
                                  p_cUsrPer  VARCHAR2,
                                  p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
    IF p_nIdePol IS NOT NULL THEN
        OPEN c_Certificado FOR
          --
          SELECT A.numcert NumCertificado,
                 A.baseavaluo,
                 A.indderaduana,
                 A.maxembarque,
                 A.indnacional,
                 PR_IMPRIME_POLIZA.nombre_persona('N', B.numid) DescPreventor,
                 A.primamin,
                 A.porcderaduana,
                 A.porcalmalm,
                 A.porcalmpue,
                 A.maxsobreseg,
                 A.minsobreseg,
                 A.porccubierta,
                 A.porctransbordo,
                 A.codmoneda,
                 A.codramocert,
                 PR_API_EMI_TRAN.indicador_proceso(A.IdePol,A.NumCert) IndProceso
            FROM dat_transporte_flotante A, proveedor B
           WHERE A.codproveedor = B.codproveedor(+)
             AND A.idepol  = p_nIdePol;
          --
    ELSE
      RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
    END IF;
    --
    RETURN c_Certificado;
   END DATOS_PART_TRANS_POL;
   /*-----------------------------------------------------------------------
    Nombre     : DATOS_PART_TRANS_TASAS_POL
    Proposito  : Devuelve los datos de tasa de mercader?a
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza

    Log de Cambios
           Fecha            Autor            Descripcion
           05-09-2006    Daniel Quintana    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DATOS_PART_TRANS_TASAS_POL (p_cCodTra  VARCHAR2,
                                        p_cUsrLog  VARCHAR2,
                                        p_nUsrId   NUMBER,
                                        p_cUsrRol  VARCHAR2,
                                        p_cUsrPer  VARCHAR2,
                                        p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   nMes NUMBER;
   c_Certificado c_Cursor;
   --
   BEGIN
    IF p_nIdePol IS NOT NULL THEN
       BEGIN
         SELECT CodLval
           INTO nMes
           FROM LVAL
          WHERE TipoLval = 'LIMTASAT';
          nMes := nMes * -1;

         EXCEPTION
           WHEN NO_DATA_FOUND THEN
             nMes := 0;
           WHEN OTHERS THEN
             nMes := 0;
       END;

       OPEN c_Certificado FOR
          --
          SELECT A.numcert NumCertificado,
                 A.CodContinente,
                 PR.busca_lval('TIPOCONT',A.codcontinente) DescContinente,
                 A.TipoTransporte,
                 PR.busca_lval('TIPTRAN',A.TipoTransporte) DescTransporte,
                 A.TipoMercancia,
                 PR.busca_lval('TIPOMERC',A.TipoMercancia) DescMercancia,
                 A.TipoEmbalaje,
                 PR.busca_lval('TIPOEMBA',A.TipoEmbalaje) DescEmbalaje,
                 A.FecIniTasa,
                 A.FecFinTasa,
                 A.Tasa,
                 A.Deducible,
                 A.Descripcion
            FROM merca_embala_tasa A
           WHERE A.idepol  = p_nIdePol
             AND A.FecFinTasa >= ADD_MONTHS(TRUNC(SYSDATE),nMes);
          --
    ELSE
      RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
    END IF;
    RETURN c_Certificado;
   END DATOS_PART_TRANS_TASAS_POL;
   /*-----------------------------------------------------------------------
    Nombre     : DATOS_PART_TRANS_GUERRA_POL
    Proposito  : Devuelve los datos de lista de guerra - huelga
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza

    Log de Cambios
           Fecha            Autor            Descripcion
           05-09-2006    Daniel Quintana    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DATOS_PART_TRANS_GUERRA_POL (p_cCodTra  VARCHAR2,
                                         p_cUsrLog  VARCHAR2,
                                         p_nUsrId   NUMBER,
                                         p_cUsrRol  VARCHAR2,
                                         p_cUsrPer  VARCHAR2,
                                         p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
     IF p_nIdePol IS NOT NULL THEN
         OPEN c_Certificado FOR
            --
            SELECT A.numcert NumCertificado,
                   A.codContinente,
                   A.tasa
              FROM clausula_guerra A
             WHERE A.idepol  = p_nIdePol;
              --
        ELSE
          RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
     END IF;
     RETURN c_Certificado;
   END DATOS_PART_TRANS_GUERRA_POL;
   /*------------------------------------------------------------------------------------
    Nombre     : CONSULTA_PLANILLA
    Proposito  : Devuelve los datos de planilla
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_StsPla   Estado de planilla
                  p_nIdePol  Identificador de la poliza
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n.
           29/04/2004    Roberto Carranza    Se Quito el campo Fecha Operacion.
           30/04/2004    Roberto Carranza    Se optimiz? el Qry.
           17/05/2004    Roberto Carranza    Se agreg? estado de declaraci?n en los datos
                                             devueltos para planilla.
                                             Se recibe un nuevo estado "PEN", Cuando este
                                             estado se reciba debe devolver los estados
                                             VAL e INC.
           14/07/2004    Miguel Wilson       Condicionar m?s de una relaci?n laboral del
                                             intermediario o cliente
   ---------------------------------------------------------------------------------------*/
   FUNCTION CONSULTA_PLANILLA (p_cCodTra  VARCHAR2,
                               p_cUsrLog  VARCHAR2,
                               p_nUsrId   NUMBER,
                               p_cUsrRol  VARCHAR2,
                               p_cUsrPer  VARCHAR2,
                               p_cStsPla  VARCHAR2) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   cCodRes       POLIZA.CodCli%TYPE; -- Codigo de responsable de pago
    -- <I N3019624> Miguel Wilson / 14-07-2004 / Condicionar m?s de una relaci?n laboral del intermediario o cliente
   cCod          VARCHAR2(1000);
    -- <F N3019624>
   cDesRes       VARCHAR2(100);      -- Nombre del responsable de pago
   cIndRes       VARCHAR2(1);        -- Indicador de responsable de pago
   cWhere        VARCHAR2(3000);
   BEGIN
    --
    cCod := PR_API_EMI_TRAN.Codigo_Cli_Ini(p_cUsrRol,p_nUsrId);
    --
    cWhere := cWhere || 'SELECT A.NumCarta,A.NumPol,A.IdePol,A.Planilla,A.CantAplic,';
    cWhere := cWhere || 'PR_API_EMI_TRAN.Valida_Resp_Pago(A.stsdec,A.Idepol,A.Planilla) ResponsablePago,';
    cWhere := cWhere || 'A.PrimaMoneda,A.CodMoneda,A.SumAsegMoneda SumaAsegMoneda,PR_API_EMI_TRAN.plazo_vencido(A.Planilla,A.indsisorig) PlazoVencido,';
    cWhere := cWhere || 'PR_API_EMI_TRAN.Politica(A.Planilla,A.IdePol,A.StsDec,A.IndSisOrig) IndPolitica,PR_API_EMI_TRAN.ind_proceso_planilla(A.IdePol,A.Planilla) IndProceso, A.StsDec StsPla';
    cWhere := cWhere || ' FROM ( SELECT D.numcarta,P.numpol,P.IdePol,P.StsPol,D.planilla,D.codmoneda,D.stsdec,D.Indsisorig,';
    cWhere := cWhere || 'SUM(D.primamoneda) PrimaMoneda,SUM(D.mtoliqmoneda) SumAsegMoneda,COUNT(1) CantAplic';
    cWhere := cWhere || ' FROM poliza P,dec_transporte_flotante D';
    --
    IF p_cStsPla = 'PEN' THEN
      cWhere := cWhere || ' WHERE P.idepol = D.IdePol AND D.StsDec IN (''VAL'',''INC'')';
    ELSE
      cWhere := cWhere || ' WHERE P.idepol = D.IdePol AND D.StsDec = ' || '''' || p_cStsPla || '''';
    END IF;
    --
    cWhere := cWhere || ' AND d.Indsisorig = ''W''';
    --
    IF p_cUsrRol = '0001' THEN
       IF cCod IS NOT NULL THEN
          cWhere := cWhere || ' AND P.codcli IN ' || cCod; --< N3019624 > Conciderar mas de un cliente
       END IF;
    END IF;
    --
    cWhere := cWhere || '   AND P.tipofactunif = ''S''';
    cWhere := cWhere || ' GROUP BY D.numcarta,P.numpol,P.IdePol,P.StsPol,D.planilla,D.codmoneda,d.stsdec,D.Indsisorig) A';
    cWhere := cWhere || ' WHERE 1 = 1 ';
    --
    IF p_cUsrRol = '0002' THEN
       cWhere := cWhere || ' AND A.idepol IN (SELECT idepol FROM part_inter_pol WHERE codinter IN ' || cCod || ')'; --< N3019624 > Conciderar mas de un cliente
    END IF;
    --
    OPEN c_Certificado FOR cWhere;
    RETURN (c_Certificado);
   END CONSULTA_PLANILLA;
   /*-----------------------------------------------------------------------
    Nombre     : RESPONSABLE_PAGO
    Proposito  : Devuelve los datos del responsable de pago
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza
                  p_nNumCer  N?mero de certificado
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           21/04/2004    Roberto Carranza    Se Quit? par?metro p_NumCer, no se
                                             estaba usando en el procedimiento
           09/12/2009    Jose Porlles C.     <N3033924> Se filtran los Activos
   -------------------------------------------------------------------------*/
   FUNCTION RESPONSABLE_PAGO (p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
     IF p_nIdePol IS NOT NULL THEN
        OPEN c_Certificado FOR
          --
          SELECT B.codcli,
                 PR_IMPRIME_POLIZA.nombre_persona('C', b.CodCli) NomCliente
            FROM poliza A,poliza_cliente B
           WHERE A.idepol = B.idepol
             AND A.idepol = p_nIdePol
             AND B.STSPOLCLI = 'ACT'; --<N3033924> 09/12/2009  Jose Porlles C. DEFPROC-00045
          --
     ELSE
       RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
     END IF;
     RETURN c_Certificado;
   END RESPONSABLE_PAGO;
   /*-----------------------------------------------------------------------
    Nombre     : VALIDA_COASEGURO_P
    Proposito  : Informaci?n de validaci?n de coaseguro de p?liza
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_nIdePol  Identificador de P?liza
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION VALIDA_COASEGURO_P (p_cCodTra  VARCHAR2,
                                p_cUsrLog  VARCHAR2,
                                p_nUsrId   NUMBER,
                                p_cUsrRol  VARCHAR2,
                                p_cUsrPer  VARCHAR2,
                                p_nIdePol  NUMBER) RETURN PR_API_EMI_TRAN.c_Cursor IS
   --
   c_Certificado c_Cursor;
   --
   BEGIN
     IF p_nIdePol IS NOT NULL THEN
        OPEN c_Certificado FOR
          --
          SELECT decode(A.tipopdcion,'C','S','N') IndValidCoaseg,
                 PR_API_EMI_TRAN.Fecha_Ini(A.Idepol) FecIniVigCoaseg,
                 PR_API_EMI_TRAN.Fecha_Fin(A.idepol) FecFinVigCoaseg
            FROM poliza A
           WHERE A.idepol  = p_nIdePol;
          --
     ELSE
       RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
     END IF;
     RETURN c_Certificado;
   END VALIDA_COASEGURO_P;
   /*-----------------------------------------------------------------------
    Nombre     : VIAJE_BUQUES
    Proposito  : Devuelve o Inserta informaci?n de datos de viaje de buques,
                 C?digo de transaccion 001 - Consulta, 004 - Inserta
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
                  p_CodBuq   C?digo de buque
                  p_AnoVia   A?o de viaje
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           08/05/2004    Roberto Carranza    Se modifico el proceso, si no encuentra
                                             el viaje entonces lo crea.
           15/05/2004    Ricardo Larrabure   Se quit? el pa?s de la b?squeda
   -------------------------------------------------------------------------*/
   FUNCTION VIAJE_BUQUES (p_cCodTra  VARCHAR2,
                          p_cUsrLog  VARCHAR2,
                          p_nUsrId   NUMBER,
                          p_cUsrRol  VARCHAR2,
                          p_cUsrPer  VARCHAR2,
                          p_cCodBuq  VARCHAR2,
                          p_cFecVia  VARCHAR2,  -- Fecha en formato yyyymmdd
                          p_cCodPai  VARCHAR2
                         ) RETURN PR_API_EMI_TRAN.c_Cursor IS
    --
    c_Certificado c_Cursor;
    --
    nNumVia    PLS_INTEGER;
    nSw        PLS_INTEGER;
    nInd       PLS_INTEGER;
    --
    cAnoIni    VARCHAR2(4);
    --
    dFecIni    DATE;
    --
    nRow       ROWID;
    --
    cPaiPro    CUMULO.paisprocedencia%TYPE;
    BEGIN
      --
       IF p_cCodBuq IS NOT NULL AND p_cFecVia IS NOT NULL THEN
          nSw := 0;
       ELSE
          nSw := 1;
          RAISE_APPLICATION_ERROR(-20100,'Error en los par?metros ingresados');
       END IF;
      --
      IF nSw = 0 THEN
         BEGIN
            SELECT C.ROWID
              INTO nRow
              FROM cumulo C
             WHERE C.CodBuque        = p_cCodBuq
               AND C.AnoViaje        = substr(p_cFecVia,1,4);
--               AND C.PaisProcedencia = p_cCodPai;   RLPW
            nInd := 1;
         EXCEPTION
            WHEN TOO_MANY_ROWS THEN
                 nInd := 0;
            WHEN NO_DATA_FOUND THEN
                 nInd := 1;
                 SELECT NVL(MAX(numviaje),0) + 1
                   INTO nNumVia
                   FROM cumulo
                  WHERE CodBuque = p_cCodBuq
                    AND AnoViaje = Substr(p_cFecVia,1,4);
                 --
                 BEGIN
                  INSERT INTO cumulo
                              (codbuque,
                               numviaje,
                               anoviaje,
                               fecinivia,
                               paisprocedencia,
                               montocumulo
                              )
                       VALUES (p_cCodbuq,
                               nNumVia,
                               substr(p_cFecVia,1,4),
                               to_date(p_cFecVia,'yyyymmdd'),
                               p_cCodPai,
                               0
                              ) RETURNING ROWID INTO nRow;
                 EXCEPTION
                  WHEN OTHERS THEN
                       RAISE_APPLICATION_ERROR(-20100,SQLERRM);
                 END;
         END;
         --
         IF nInd = 0 THEN
            OPEN c_Certificado FOR
              SELECT C.NumViaje,
                     C.FecIniVia FecViaje,
                     C.PaisProcedencia CodPais,
                     PR_UBIGEO.busca_pais(c.paisprocedencia) DescPais
                FROM cumulo C
               WHERE C.CodBuque        = p_cCodBuq
                 AND C.AnoViaje        = substr(p_cFecVia,1,4);
--                 AND C.PaisProcedencia = p_cCodPai;    RLPW
         ELSIF nInd = 1 THEN
               OPEN c_Certificado FOR
                  SELECT C.NumViaje,
                         C.FecIniVia FecViaje,
                         C.PaisProcedencia CodPais,
                         PR_UBIGEO.busca_pais(c.paisprocedencia) DescPais
                    FROM cumulo C
                   WHERE c.ROWID = nRow;
         END IF;
         --
      END IF;
      RETURN c_Certificado;
   END VIAJE_BUQUES;
  /*-------------------------------------------------------------------------------
    Nombre     : DATOS_ASEGURADO
    Proposito  : Devuelve los datos de c?digo y nombre del asegurado
    Referencia : Transportes WEB
    Parametros :
                 p_nNumPol  N?mero de poliza
    Log de Cambios
           Fecha            Autor            Descripcion
           12/04/2004    Roberto Carranza    Creaci?n.
           12/05/2004    Roberto Carranza    Se agreg? c?digo de producto
                                             para filtro.
           15/05/2004    Ricardo Larrabure   Se modific? la b?squeda del asegurado.
                                             se toma los datos de la p?liza.
   -------------------------------------------------------------------------------*/
   FUNCTION DATOS_ASEGURADO ( p_cCodTra  VARCHAR2,
                              p_cUsrLog  VARCHAR2,
                              p_nUsrId   NUMBER,
                              p_cUsrRol  VARCHAR2,
                              p_cUsrPer  VARCHAR2,
                              p_nNumPol  NUMBER,
                              p_cCodPro  VARCHAR2
                              ) RETURN PR_API_EMI_TRAN.c_Cursor IS
      c_PolCliente c_Cursor;
   BEGIN
      OPEN c_PolCliente FOR
       --
         SELECT P.IdePol,
               P.CodCli  CodCli,
               T.NumId NumId,
               T.NomTer Nombre,
               DECODE(T.TipoTer, 'P', T.ApeTer) ApePat,
               T.ApeMatTer ApeMat
          FROM TERCERO T, CLIENTE C, POLIZA P
         WHERE C.CodCli = P.CodCli
           AND C.NumId  = T.NumId
           AND P.NumPol  = p_nNumPol
           AND P.Codprod = p_cCodPro
     AND P.STSPOL = 'ACT'; --<N3034800> Jose Porlles C. 04/05/2010  HelpDesk 82751

        /*SELECT DISTINCT P.IdePol,  --> RLPW
               C.CodCli  CodCli,
               PR_API_EMI_TRAN.Numero_ID(C.idepol,C.numcert) NumId,
               PR_API_EMI_TRAN.nombre_asegurado(C.idepol,C.numcert,'N') Nombre,
               PR_API_EMI_TRAN.nombre_asegurado(C.idepol,C.numcert,'P') ApePat,
               PR_API_EMI_TRAN.nombre_asegurado(C.idepol,C.numcert,'M') ApeMat
          FROM certificado C, poliza P
         WHERE C.IdePol  = P.idepol
           AND P.NumPol  = p_nNumPol
           AND P.Codprod = p_cCodPro;*/
       --
      RETURN( c_PolCliente );
   END DATOS_ASEGURADO;
   /*-----------------------------------------------------------------------
    Nombre     : FECHA_INI
    Proposito  : Devuelve fecha de inicio
    Referencia : Transportes WEB
    Parametros :
                 p_nIdePol  Identificador de poliza
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION FECHA_INI (p_nIdePol  NUMBER ) RETURN DATE IS
      dResult DATE;
   BEGIN
      SELECT TRUNC(MIN(D.FecIniValid))
        INTO dResult
        FROM DIST_COA D
       WHERE D.IdePol = p_nIdePol
         AND D.StsCoa = 'ACT';
      RETURN(dResult);
   END FECHA_INI;
   /*-----------------------------------------------------------------------
    Nombre     : FECHA_FIN
    Proposito  : Devuelve fecha de fin
    Referencia : Transportes WEB
    Parametros :
                 p_nIdePol  Identificador de poliza
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION FECHA_FIN (p_nIdePol  NUMBER ) RETURN DATE IS
      dResult DATE;
   BEGIN
      SELECT TRUNC(MAX(FecFinValid))
        INTO dResult
        FROM DIST_COA D
       WHERE D.IdePol = p_nIdePol
         AND D.StsCoa = 'ACT';
      RETURN(dResult);
   END FECHA_FIN;
   /*-----------------------------------------------------------------------
    Nombre     : BUSCA_BUQUE
    Proposito  : Devuelve la descripci?n del buque
    Referencia : Transportes WEB
    Parametros :
                 p_CodBuq  C?digo de buque
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION BUSCA_BUQUE(p_cCodBuq VARCHAR2) RETURN VARCHAR2 IS
     cNomBuq LVAL.descrip%TYPE;
   BEGIN
     --
     BEGIN
       SELECT B.nombuque
         INTO cNomBuq
         FROM buque B, lval L
        WHERE L.TipoLval  = 'TIPOBUQU'
          AND B.TipoBuque = L.CodLval
          AND B.CodBuque  = p_cCodBuq;
     EXCEPTION
       WHEN OTHERS THEN
            cNomBuq := '';
     END;
     --
     RETURN(cNomBuq);
   END BUSCA_BUQUE;
   /*-----------------------------------------------------------------------
    Nombre     : FECHA_EMISION
    Proposito  : Devuelve la fecha de emision
    Referencia : Transportes WEB
    Parametros :
                 p_IdeRec  Indicador de recibo
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION FECHA_EMISION(p_nIdeDec NUMBER) RETURN DATE IS
     dFecEmi DATE;
   BEGIN
     --
     BEGIN
        SELECT trunc(c.fechahoraini)
          INTO dFecEmi
          FROM dec_transporte_flotante A,recibo B, operacion C
         WHERE A.IdeRec = B.IdeRec
           AND B.ideop  = C.ideop
           AND A.IdeDec = p_nIdeDec;
     EXCEPTION
       WHEN OTHERS THEN
            dFecEmi := NULL;
     END;
     --
     RETURN(dFecEmi);
   END FECHA_EMISION;
   /*-----------------------------------------------------------------------
    Nombre     : INCLUIR_POLIZA
    Proposito  : Activar Poliza
    Referencia : Transportes WEB
    Parametros :
                  p_nIdePol  Identificador de poliza
                  p_nNumCer  N?mero de certificado
                  p_cCodRam  C?digo de ramo
                  p_cStsDec  Estado de la declaraci?n
                  p_nIdeDec  Identificador de la declaraci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION INCLUIR_POLIZA(p_cUsrLog VARCHAR2,
                           p_nIdePol NUMBER,
                           p_nNumCer NUMBER,
                           p_cCodRam VARCHAR2,
                           p_cStsDec VARCHAR2,
                           p_nIdeDec NUMBER,
                           p_cResPag VARCHAR2,
                           cNumTra   VARCHAR2
                           ) RETURN VARCHAR2 IS
     --
    bHabili     BOOLEAN;
     --
     cIndPol     VARCHAR2(1);
   BEGIN
/*    cNumTra := PR_POLIZA_TRAMITE.ObtenerUltimoTramite(p_nIdePol);
      bHabili := PR_POLIZA_TRAMITE.HabilitarCambio(p_nIdePol);*/
      --
      REALIZAR_INCLUSION(p_cUsrLog,p_nIdePol,p_nNumCer,p_cCodRam,cNumTra,p_cStsDec,p_nIdeDec,cIndPol,p_cResPag);
      --
      RETURN(cIndPol);
   END INCLUIR_POLIZA;
   /*-----------------------------------------------------------------------
    Nombre     : REALIZAR_INCLUSION
    Proposito  : Incluir declaraci?n
    Referencia : Transportes WEB - Se tom? el procedimiento de la forma DECLTRAF
    Parametros :
                  p_nIdePol  Indicador de recibo
                  p_nNumCer  N?mero de certificado
                  p_cCodRam  C?digo de Ramo
                  p_cNumTra  N?mero de tr?mite
                  p_cStsDec  Estado de declaraci?n
                  p_nIndDec  Indicador de declaraci?n
                  p_cIndPol  Indicador de pol?ticas (Par?metro de Salida)
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           10/05/2004    Roberto Carranza    Se quit? la inserci?n a la
                                             tabla Transac_Tramite
           11/05/2004    Roberto Carranza    Se quito la llamada a Backup_Responsable
           04/07/2007    Jos? Collantes      <N3027782> Inclusi?n de llamada a procedimiento Resumir_Mod_cobert_dec_tran.HD-56139
       23/03/2012  Renzo Yacarini      <N3038631> HD 103924
   -------------------------------------------------------------------------*/
   PROCEDURE REALIZAR_INCLUSION(p_cUsrLog VARCHAR2,
                                p_nIdePol NUMBER,
                                p_nNumCer NUMBER,
                                p_cCodRam VARCHAR2,
                                p_cNumTra VARCHAR2,
                                p_cStsDec VARCHAR2,
                                p_nIndDec NUMBER,
                                p_cIndPol OUT VARCHAR2,
                                p_cResPag VARCHAR2
                                ) IS
      --
      ncuenta  NUMBER(5);
      ncantmov NUMBER(6);
      nNumPol  NUMBER(10);
      --
      nExiste  VARCHAR2(1);
      cIndPol  VARCHAR2(1);
      --
      cStsCer  CERT_RAMO.stscertramo%TYPE;
      cCodPro  POLIZA.codprod%TYPE;
   BEGIN
      --
      IF p_cNumTra IS NOT NULL THEN
      nExiste := '0';
      BEGIN
       nExiste := PR_POLIZA_TRAMITE.ExistePendiente (p_nIdePol ,p_cNumTra);
            --
       IF nExiste = '0' THEN
        PR_POLIZA_TRAMITE.CrearTramite(p_nIdePol,p_cNumTra);
       ELSE
        PR_POLIZA_TRAMITE.ActualizarPendiente(p_nIdePol,p_cNumTra);
       END IF ;
            --
      END;
     END IF;
      --
      BEGIN
      SELECT nvl(stscertramo,'XXX')
        INTO cStsCer
        FROM cert_ramo
       WHERE idepol      = p_nIdePol
         AND numcert     = p_nNumCer
         AND codramocert = p_cCodRam;
      EXCEPTION
       WHEN no_data_found THEN
             cStsCer := 'XXX';
       WHEN too_many_rows THEN
             cStsCer := 'XXX';
       WHEN OTHERS THEN
          RAISE_APPLICATION_ERROR(-20100,'Error en select a cert_ramo '||SQLERRM);
      END;
      --
      BEGIN
       SELECT COUNT(1)
         INTO ncantmov
         FROM mod_cobert
        WHERE idepol       = p_nIdePol
         AND numcert      = p_nNumCer
         AND codramocert  = p_cCodRam
         AND stsmodcobert = 'INC';
     END;
      --
      IF cStsCer <> 'ACT' AND nCantMov > 0 THEN
        RAISE_APPLICATION_ERROR(-20100,'Ya se gener? movimiento a la declaraci?n, debe de activar la p?liza');
      END IF;
      --
     IF p_cStsDec = 'VAL' THEN
         --
        PR_DEC_TRANSPORTE.incluir_f(p_nIndDec);
       PR_DEC_TRANSPORTE.gen_mov_f(p_nIdePol,p_nNumCer,p_cCodRam);
       PR_COBERT_CERT.RESUMIR_MOD_COBERT_DEC_TRAN (p_nIdePol,p_nNumCer,p_cCodRam); --<N3027782> Jos? Collantes / 04-07-2007 / HD-56139
         --
         BEGIN
          SELECT p.NumPol,p.codprod
            INTO nNumPol,cCodPro
            FROM poliza p
           WHERE p.IdePol = p_nIdePol;
         EXCEPTION
           WHEN OTHERS THEN
                nNumPol := 0;
         END;
         --
         IF TRIM(p_cResPag) IS NOT NULL THEN
            PR_API_EMI_TRAN.fr_backup_responsables(USERENV('SESSIONID'),'01',p_nIdePol,p_nNumCer);
            PR_API_EMI_TRAN.fr_carga_resp(p_nIdePol,p_nNumCer,p_cResPag,cCodPro);
         END IF;
         --
        VALIDA_POLITICAS(p_cUsrLog,p_nIdePol,nNumPol,cCodPro,p_cIndPol, p_nIndDec); -- <N3038631> HD- 103924/ Renzo Yacarini / 23-03-2012
         --
         IF TRIM(p_cResPag) IS NOT NULL THEN
            PR_API_EMI_TRAN.fr_backup_responsables(USERENV('SESSIONID'),'02',p_nIdePol,p_nNumCer);
         END IF;
         --
      ELSIF p_cStsDec = 'INC' THEN
          PR_DEC_TRANSPORTE.gen_mov_f(p_nIdePol,p_nNumCer,p_cCodRam);
          PR_COBERT_CERT.RESUMIR_MOD_COBERT_DEC_TRAN (p_nIdePol,p_nNumCer,p_cCodRam); --<N3027782> Jos? Collantes / 04-07-2007 / HD-56139
      END IF;
   END REALIZAR_INCLUSION;
   /*-----------------------------------------------------------------------
    Nombre     : VALIDA_POLITICAS
    Proposito  : Valida politicas
    Referencia : Transportes WEB  - Se tom? el procedimiento de la forma DECLTRAF
    Parametros :
                  p_nIdePol  Indicador de recibo
                  p_nNumPol  N?mero de poliza
                  p_cCodPro  C?digo de producto
                  p_cIndPol  Indicador de pol?ticas (Par?metro de Salida)
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
           14/02/2006    Marko Chu           Modificacion para que autorice autmaticamente algunas politicas
                                             configuradas TRANS-00001 <N3024162>
           23/03/2012    Renzo Yacarini      <N3038631> HD- 103924
   -------------------------------------------------------------------------*/
   PROCEDURE VALIDA_POLITICAS(p_cUsrLog VARCHAR2,
                              p_nIdePol NUMBER,
                              p_nNumPol NUMBER,
                              p_cCodPro VARCHAR2,
                              p_cIndPol OUT VARCHAR2,
                              p_nIdeDec NUMBER DEFAULT NULL) -- <N3038631> HD- 103924/ Renzo Yacarini / 23-03-2012
                              IS
      --
      c_Existe_Politica   PLS_INTEGER;
      --
      cTipoPdCion         VARCHAR2(1);
      cPolitica          VARCHAR2(1);
      cPolitica2         VARCHAR2(1);
   BEGIN
      p_cIndPol := 'N';
      --
      BEGIN
      SELECT IndMovPolRen
        INTO cPolitica
        FROM POLIZA
       WHERE IdePol  = p_nIdePol
         AND CodProd  = p_cCodPro;
     EXCEPTION
       WHEN OTHERS THEN
            cPolitica := 0;
     END;
      --
      BEGIN
       SELECT TipoPdCion
         INTO cTipoPdCion
         FROM POLIZA
        WHERE IdePol = p_nIdePol;
      EXCEPTION
       WHEN OTHERS THEN
             RAISE_APPLICATION_ERROR(-20100,SQLERRM);
      END;
      --
     IF nvl(cPolitica,1) != 2 THEN
       IF cTipoPdCion <> 'A' THEN

         PR_POLITICAS.g_cTipoUsuario := 'WEB';  -- <N3024162> / Marko Chu / 14-02-2006 / TRANS-00001

               -- <I N3019132> Ricardo Larrabure / 02-06-2004 / Reemplazo var.USER por usuario de la aplicaci?n
         cPolitica2 := PR_POLITICAS.principal(p_nIdePol, p_nNumPol, p_cCodPro, PR_SESION.USUARIO_APP,'AC', p_nIdeDec); -- <N3038631> HD- 103924/ Renzo Yacarini / 23-03-2012
               -- <F N3019132>
       END IF;
       IF cPolitica2 = '1' THEN
         -- Existen Politicas Violadas
             p_cIndPol := 'S';
          ELSE
              --
         BEGIN
          SELECT IndMovPolRen
            INTO cPolitica
            FROM POLIZA
           WHERE IdePol  = p_nIdePol
             AND CodProd  = p_cCodPro;
         EXCEPTION
                WHEN OTHERS THEN
              cPolitica := 0;
         END;
              --
         IF cPolitica = 1 THEN
             -- Existen Politicas Violadas
                 p_cIndPol := 'S';
         ELSE
           BEGIN       -- actualiza la p?liza
              UPDATE POLIZA
                SET FECOPER = SYSDATE
              WHERE IDEPOL  = p_nIdePol;
           EXCEPTION
            WHEN OTHERS THEN
              RAISE_APPLICATION_ERROR(-20100,'Error actualizando POLIZA - '||SQLERRM);
           END;
           ACTIVAR_POLIZA(p_cUsrLog,p_nIdePol);
             END IF;
          END IF;
     END IF;
   END;
   /*-----------------------------------------------------------------------
    Nombre     : ACTIVAR_POLIZA
    Proposito  : Activar Poliza
    Referencia : Transportes WEB - Se tom? el procedimiento de la forma DECLTRAF
    Parametros :
                 p_nIdePol  Indicador de poliza
    Log de Cambios
           Fecha            Autor            Descripcion
           29/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   PROCEDURE ACTIVAR_POLIZA(p_cUsrLog VARCHAR2,
                            p_nIdePol NUMBER) IS
      --
      nCuenta           NUMBER;
      --
      cFirma      VARCHAR2(1);
      --
      cIndCobPoliza   POLIZA.IndEsqReaPol%TYPE;
      nNumOper     OPER_POL.NumOper%TYPE;
      cCodRamoCert   CERT_RAMO.CodRamoCert%TYPE;
      cCodPlan     CERT_RAMO.CodPlan%TYPE;
      cRevPlan     CERT_RAMO.CodPlan%TYPE;
      cValEst      VAL_EST.ValEst%TYPE;
      --
      CURSOR PROVISIONAL IS
      SELECT EC.IdePol, EC.NumCert, EC.CodRamoCert, EC.CodEst
        FROM est_cert EC, estadistica E, val_est VE
       WHERE EC.IdePol = p_nIdePol
         AND EC.CodEst = E.CodEst
         AND EC.CodEst = VE.CodEst
         AND EC.ValEst = VE.ValEst
         AND E.DescEst LIKE '%COBERTURA PROVISIONAL%'
         AND VE.DescValEst  = 'SI';
      --
   BEGIN
      --
      BEGIN
       SELECT NVL(IndEsqReaPol,'N')
         INTO cIndCobPoliza
         FROM POLIZA
        WHERE IdePol = p_nIdePol;
      END;
      --
      IF cIndCobPoliza = 'S' THEN
         SUMA_MAYOR_REA(p_nIdePol);
         PR_POLIZA.certificado_Cero(p_nIdePol);
      END IF;
      --
      nNumOper := PR_POLIZA.activar(p_nIdePol,'T');
      nNumOper := PR_POLIZA.activar(p_nIdePol,'D');
      --
      UPDATE POLIZA
         SET IndMovPolRen = '0'
       WHERE IdePol = p_nIdePol;
      --
      UPDATE AUTORIZA_VIOLACION
         SET NumOper = nNumOper
      WHERE IdePol  = p_nIdePol
        AND NumOper IS NULL;
      --
      cFirma := PR_EVALUA_POLITICA.obtiene_firma_Digital(p_nIdePol, nNumOper);
      --
      UPDATE POLIZA
         SET IndFact = 'N'
       WHERE IdePol = p_nIdePol;
      --
      FOR I IN PROVISIONAL LOOP
         cValEst := NULL;
          BEGIN
            SELECT ValEst
              INTO cValEst
           FROM VAL_EST VE
          WHERE VE.CodEst = I.CodEst
            AND VE.DescValEst LIKE '%FUE PROVISIONAL%';
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
               cValEst := NULL;
                  RAISE_APPLICATION_ERROR(-20100, 'La Estad?stica "Cobertura Provisional" no tiene Definido el Valor "Fue Provisional", Revise'||SQLERRM);
            WHEN TOO_MANY_ROWS THEN
               cValEst := NULL;
                  RAISE_APPLICATION_ERROR(-20100, 'La Estad?stica "Cobertura Provisional" tiene Definido el Valor "Fue Provisional" M?s de Una Vez, Revise'||SQLERRM);
            WHEN OTHERS THEN
                 cValEst := NULL;
                  RAISE_APPLICATION_ERROR(-20100, 'Error en el Valor de la Estad?stica "Cobertura Provisional", Revise'||SQLERRM);
          END;
          BEGIN
              UPDATE EST_CERT
                 SET ValEst      = cValEst
               WHERE IdePol      = I.IdePol
                 AND NumCert     = I.NumCert
                 AND CodRamoCert = I.CodRamoCert
                 AND CodEst      = I.CodEst;
          EXCEPTION
             WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20100, 'Error al Actualizar Estad?stica de "Cobertura Provisional", Revise');
          END;
      END LOOP;
      --
     PR_COND_FINANCIAMIENTO.Generar_Acreencia(nNumOper);
      PR_COND_FINANCIAMIENTO.Generar_Factura(nNumOper);
      PR_MOV_PRIMA.eliminar_mov_T(p_nIdePol);
      PR_GEN_REA_T.eliminar_rea_Temporal(p_nIdePol,NULL,NULL,NULL);
      DELETE POLIZA_REHAB WHERE IdePol = p_nIdePol;
   END ACTIVAR_POLIZA;
   /*-----------------------------------------------------------------------
    Nombre     : VALIDA_PARAMETROS_PRINCIPALES
    Proposito  : Validar los par?metros iniciales recibidos en cada funci?n
    Referencia : Transportes WEB
    Parametros :
                  p_cCodTra  C?digo de Transacci?n
                  p_cUsrLog  C?digo de usuario que se logueo al sistema
                  p_nUsrId   Id de usuario que se logueo al sistema
                  p_cUsrRol  Rol de usuario logueado
                  p_cUsrPer  Perfil de usuario loqueado
    Log de Cambios
           Fecha            Autor            Descripcion
           30/03/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION VALIDA_PARAMETROS_PRINCIPALES(p_cCodTra VARCHAR2,
                                          p_cUsrLog VARCHAR2,
                                          p_nUsrId  NUMBER,
                                          p_cUsrRol VARCHAR2,
                                          p_cUsrPer VARCHAR2) RETURN BOOLEAN IS
     lResult BOOLEAN :=  TRUE;
   BEGIN
     /*
     IF p_cCodTra IS NULL OR p_cUsrLog IS NULL OR
        p_nUsrId  IS NULL OR p_cUsrRol IS NULL OR
        p_cUsrPer IS NULL THEN
        lResult := FALSE;
     END IF;
     */
     RETURN(lResult);
   END VALIDA_PARAMETROS_PRINCIPALES;
   /*-----------------------------------------------------------------------
    Nombre     : DISTRITO_CONTRATANTE
    Prop?sito  : Devuelve la descripci?n del distrito del contratante
    Referencia : Transportes WEB
    Par?metros :
                 p_cCodPai  C?digo de pa?s
                 p_cCodEst  C?digo de Estado
                 p_cCodCiu  C?digo de Ciudad
                 p_cCodMun  C?digo de municipio
    Log de Cambios
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
           10/01/2005    Karen Munayco       Se cambio el tipo de datos de los parametros antes eran number <N3021341>
   -------------------------------------------------------------------------*/
   FUNCTION DISTRITO_CONTRATANTE(p_cCodPai  VARCHAR2,
                                 p_cCodEst  VARCHAR2,
                                 p_cCodCiu  VARCHAR2,
                                 p_cCodMun  VARCHAR2) RETURN VARCHAR2 IS
     lResult  VARCHAR2(50);
   BEGIN
     --
     BEGIN
        -- Distrito
        SELECT DescMunicipio
          INTO lResult
          FROM MUNICIPIO
         WHERE CodPais      = p_cCodPai
           AND CodEstado    = p_cCodEst
           AND CodCiudad    = p_cCodCiu
           AND CodMunicipio = p_cCodMun;
         EXCEPTION
            WHEN OTHERS THEN
                 lResult := '';
     END;
     --
     RETURN(lResult);
   END DISTRITO_CONTRATANTE;
  /*-----------------------------------------------------------------------
    Nombre     : NUMERO_TRAMITE
    Prop?sito  : Devuelve el n?mero de tr?mite
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdePol    Identificador de poliza
                 p_nNumOpe    N?mero de operaci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION NUMERO_TRAMITE(p_nIdePol NUMBER,
                           p_nNumOpe NUMBER) RETURN VARCHAR2 IS
     lResult  POLIZA_TRAMITE.numtramite%TYPE;
   BEGIN
     --
     BEGIN
        SELECT numtramite
          INTO lResult
          FROM poliza_tramite
         WHERE Idepol  = p_nIdePol
           AND NumOper = p_nNumOpe;
       EXCEPTION
          WHEN OTHERS THEN
               lResult := '';
     END;
     --
     RETURN(lResult);
   END NUMERO_TRAMITE;
  /*-----------------------------------------------------------------------
    Nombre     : NOMBRE_ASEGURADO
    Prop?sito  : Devuelve el nombre completo del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdePol    Identificador de p?liza
                 p_nNumCer    N?mero de certificado
                 p_nIdeRec    Numero de recibo
                 p_cTipDat    P   Devuelve Apellido Paterno
                              M   Devuelve Apellido Materno
                              N   Devuelve Nombre
                              A   Devuelve Concatenado Nombre + Paterno + Materno
    Log de Cambios
       Fecha            Autor            Descripcion
       06/04/2004    Roberto Carranza    Creaci?n
       13/04/2004    Roberto Carranza    Se agreg? Par?metro que indica dato a devolver
       14/07/2004    Miguel Wilson       Obtener el codigo de cliente de los Financiamientos
       31/08/2006    Marko Chu           Aumentar el tama?o de variables cResult y cApeNom
   -------------------------------------------------------------------------*/
   FUNCTION NOMBRE_ASEGURADO(p_nIdePol NUMBER,
                             p_nNumCer NUMBER,
                             p_nIdeRec NUMBER,
                             p_cTipDat VARCHAR2 ) RETURN VARCHAR2 IS
     --
     --<I 46347> / 31-08-2006 / Marko Chu /
     cResult  VARCHAR2(500);
     cApeNom  VARCHAR2(500);
     --<F 46347>
     cCliAse  CERTIFICADO.codcli%TYPE;
     cNombre  TERCERO.nomter%TYPE;
     cApePat  TERCERO.Apeter%TYPE;
     cApeMat  TERCERO.Apematter%TYPE;
     --
   BEGIN
     -- <I N3019624> Miguel Wilson / 16-07-2004 / Obtener el codigo de cliente de los Financiamientos
     BEGIN
       SELECT C.CodCli
         INTO cCliAse
         FROM RECIBO R, COND_FINANCIAMIENTO F, CLIENTE C
        WHERE R.IdePol  = p_nIdePol
          AND R.NumCert = p_nNumCer
          AND R.IdeRec  = p_nIdeRec
          AND F.IdePol  = R.IdePol
          AND F.NumCert = R.NumCert
          AND F.NumOper = R.IdeOp
          AND C.NumId=F.NumId;
     EXCEPTION
       WHEN OTHERS THEN
         cCliAse := NULL;
     END;
     -- <F N3019624>
     --
     BEGIN
       SELECT DECODE(T.TipoTer,'J',T.NomTer ||' '|| T.Apeter ||' '|| T.ApeMatTer,
                      'P',T.NomTer ||' '|| T.Apeter ||' '|| T.ApeMatTer,T.NomTer),
               DECODE(T.TipoTer,'J',T.NomTer,'P',T.NomTer,T.NomTer),
               DECODE(T.TipoTer,'J',T.Apeter,'P',T.Apeter,NULL),
               DECODE(T.TipoTer,'J',T.ApeMatTer,'P',T.ApeMatTer,NULL)
         INTO cApeNom,cNombre,cApePat,cApeMat
         FROM TERCERO T,CLIENTE C
        WHERE C.CodCli = cCliAse
           AND T.TipoId = C.TipoId
           AND T.NumId  = C.NumId
           AND T.DvID   = C.DvID;
     EXCEPTION
       WHEN OTHERS THEN
            cApeNom := NULL;
            cNombre := NULL;
            cApePat := NULL;
            cApeMat := NULL;
     END;
     --
     IF p_cTipDat = 'P' THEN
        cResult := cApePat;
     ELSIF p_cTipDat = 'M' THEN
        cResult := cApeMat;
     ELSIF p_cTipDat = 'N' THEN
        cResult := cNombre;
     ELSIF p_cTipDat = 'A' THEN
        cResult := cApeNom;
     END IF;
     --
     RETURN(cResult);
   END NOMBRE_ASEGURADO;
  /*-----------------------------------------------------------------------
    Nombre     : DES_RAMO
    Prop?sito  : Devuelve la descripci?n del ramo
    Referencia : Transportes WEB
    Par?metros :
                 p_cCodRam    C?digo de Ramo
    Log de Cambios
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DES_RAMO(p_cCodRam VARCHAR2) RETURN VARCHAR2 IS
     cResult  VARCHAR2(100);
   BEGIN
     --
     BEGIN
        SELECT R.DescRamo
          INTO cResult
         FROM RAMO R
        WHERE R.CodRamo = p_cCodRam;
     EXCEPTION
         WHEN OTHERS THEN
              cResult := NULL;
     END;
     --
     RETURN(cResult);
   END DES_RAMO;
  /*-----------------------------------------------------------------------
    Nombre     : VALIDA_RESP_PAGO
    Prop?sito  : Devuelve responsable de pago.
    Referencia : Transportes WEB
    Par?metros :
                 p_cStsDec   Estado
                 p_nIdePol   Identificador de poliza
                 p_cCodPla   N?mero de planilla
    Log de Cambios:
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION VALIDA_RESP_PAGO(p_cStsDec VARCHAR2,
                             p_nIdePol NUMBER,
                             p_cCodPla VARCHAR2) RETURN VARCHAR2 IS
      --
      cResult  VARCHAR2(200);
      --
      cNumPla  DEC_TRANSPORTE_FLOTANTE.planilla%TYPE;
      cCodMon  DEC_TRANSPORTE_FLOTANTE.codmoneda%TYPE;
      cCodRes  DEC_TRANSPORTE_FLOTANTE.codcli%TYPE;
      --
   BEGIN
     --
     BEGIN
       --
       SELECT DISTINCT D.planilla,D.codmoneda,D.codcli
                  INTO cNumPla,cCodMon,cCodRes
                  FROM poliza P,dec_transporte_flotante D
                 WHERE P.idepol = D.IdePol
                   AND D.stsdec = p_cStsDec
                   AND P.idePol = p_nIdePol
                   AND planilla = p_cCodPla;
       --
       cResult := 'N%%'|| NVL(cCodRes,' ') || '%%' || PR_IMPRIME_POLIZA.nombre_persona('C', cCodRes);
       --
     EXCEPTION
       WHEN too_many_rows THEN
            cResult := 'S%% %%';
     END;
     --
     RETURN(cResult);
   END VALIDA_RESP_PAGO;
  /*-----------------------------------------------------------------------
    Nombre     : NUMERO_OPERACION
    Prop?sito  : Devuelve n?mero de operaci?n.
    Referencia : Transportes WEB
    Par?metros :
                  p_nIdeRec  Identificador de recibo
    Log de Cambios:
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   ------------------------------------------------------------------------*/
   FUNCTION NUMERO_OPERACION(p_nIdeRec NUMBER) RETURN NUMBER IS
   --
   nResult  RECIBO.ideop%TYPE;
   --
   BEGIN
     BEGIN
       SELECT r.ideop
         INTO nResult
         FROM recibo r
        WHERE R.IdeRec = p_nIdeRec;
     EXCEPTION
       WHEN OTHERS THEN
            nResult := NULL;
     END;
     RETURN(nResult);
   END NUMERO_OPERACION;
  /*--------------------------------------------------------------------
    Nombre     : TIPO_APLICACION
    Prop?sito  : Devuelve el tipo de aplicaci?n. (Nacional o Importado)
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdePol  Identificador de poliza
                 p_nNumCer  N?mero de certificado
                 p_cCodRam  C?digo de ramo
    Log de Cambios:
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   ---------------------------------------------------------------------*/
   FUNCTION TIPO_APLICACION(p_nIdePol NUMBER,
                            p_nNumcer NUMBER,
                            p_cCodRam VARCHAR2) RETURN VARCHAR2 IS
   --
   cResult  VARCHAR2(1);
   --
   BEGIN
     --
     BEGIN
       SELECT DECODE(IndNacional,'N','Nacional','I','Importacion','E','Exportacion')
         INTO cResult
         FROM DAT_TRANSPORTE_FLOTANTE
        WHERE Idepol      = p_nIdePol
          AND NumCert     = p_nNumCer
          AND CodRamoCert = p_cCodRam;
     EXCEPTION
       WHEN OTHERS THEN
            cResult := NULL;
     END;
     --
     RETURN(cResult);
   END TIPO_APLICACION;
  /*-----------------------------------------------------------------------
    Nombre     : RUC_CONTRATANTE
    Prop?sito  : N?mero de RUC del contratante
    Referencia : Transportes WEB
    Par?metros :
                 p_cCodCli  C?digo de contratante
    Log de Cambios:
           Fecha            Autor            Descripcion
           06/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION RUC_CONTRATANTE(p_cCodCli VARCHAR2) RETURN VARCHAR2 IS
   --
   cResult  DOCS_IDENTIDAD.numiddoc%TYPE;
   --
   BEGIN
     --
     BEGIN
       SELECT DI.NumIdDoc
       INTO cResult
      FROM DOCS_IDENTIDAD DI,CLIENTE CL
       WHERE CL.CodCli    = p_cCodCli
       AND DI.TipoId    = CL.TipoId
       AND DI.NumId     = CL.NumId
       AND DI.DvId      = CL.DvId
       AND DI.TipoIdDoc = '1';
     EXCEPTION
       WHEN OTHERS THEN
            cResult := NULL;
     END;
     --
     RETURN(cResult);
   END RUC_CONTRATANTE;
  /*-----------------------------------------------------------------------
    Nombre     : NUM_DOCUMENTO
    Prop?sito  : Devuelve el n?mero de documento
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdeFac   Identificador de factura
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION NUM_DOCUMENTO(p_nIdeFac NUMBER) RETURN VARCHAR2 IS
   --
   nResult  GIROS_FINANCIAMIENTO.numfinanc%TYPE;
   --
   BEGIN
     --
     BEGIN
         SELECT DISTINCT NumFinanc
           INTO nResult
           FROM FACTURA f, ACREENCIA a, GIROS_FINANCIAMIENTO g
          WHERE f.IdeFact = p_nIdeFac
            AND f.IdeFact = a.IdeFact
            AND a.NumAcre = g.NumAcre;
     EXCEPTION
         WHEN OTHERS THEN
              nResult := NULL;
     END;
     --
     RETURN(nResult);
   END NUM_DOCUMENTO;
  /*-----------------------------------------------------------------------
    Nombre     : Contratante
    Prop?sito  : Devuelve el nombre completo del contratante
    Referencia : Transportes WEB
    Par?metros :
                 p_nNumDoc   N?mero de documento del contratante
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION CONTRATANTE(p_nNumDoc NUMBER) RETURN VARCHAR2 IS
   --
   cResult  VARCHAR2(100);
   --
   BEGIN
     --
     BEGIN
        SELECT DECODE(t.TipoTer,'J',t.NomTer||' '||t.ApeTer||' '||t.ApeMatTer,'P',t.NomTer||' '||t.ApeTer||' '||t.ApeMatTer,t.NomTer)
          INTO cResult
          FROM TERCERO t, FACTURA f
         WHERE f.NumFact = p_nNumDoc
           AND t.TipoId  = f.TipoId
           AND t.NumId   = f.NumId
           AND t.DvId    = f.DvId;
     END;
     --
     RETURN(cResult);
   END CONTRATANTE;
  /*-----------------------------------------------------------------------
    Nombre     : FecPrimerPago
    Prop?sito  : Devuelve la fecha del primer pago
    Referencia : Transportes WEB
    Par?metros :
                 p_Fecini   Fecha de Inicio
                 p_FecFin   Fecha de Fin
                 p_FecOpe   Fecha de Operacion
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
           04/05/2004    Roberto Carranza    Se modific? la forma en que la fecha es devuelta
   -------------------------------------------------------------------------*/
    FUNCTION FEC_PRIMER_PAGO(p_FecIni DATE,
                             p_FecFin DATE,
                             p_FecOpe DATE) RETURN DATE IS
      dFecDia  DATE;
    BEGIN
      --
      IF p_FecIni >= p_FecOpe THEN
        dFecDia := p_FecIni + 15;
      ELSE
        dFecDia := p_FecOpe + 7;
      END IF;
      --
      RETURN(dFecDia);
    END FEC_PRIMER_PAGO;
  /*-----------------------------------------------------------------------
    Nombre     : DIRECCION_ASEGURADO
    Prop?sito  : Devuelve la direcci?n completa del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nNumOpe  N?mero de operaci?n
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
    FUNCTION DIRECCION_ASEGURADO(p_NumOpe NUMBER) RETURN VARCHAR2 IS
      cDirRet  VARCHAR2(100);
      --
      cCodCli  CERTIFICADO.Codcli%TYPE;
    BEGIN
      --
        BEGIN
      SELECT DISTINCT CodCli
        INTO cCodCli
       FROM Certificado C, Oper_Pol OP
      WHERE OP.NumOper  = p_NumOpe
        AND OP.IdePol   = C.idepol
        AND OP.NumCert  = C.NumCert;
      EXCEPTION
       WHEN Too_Many_Rows THEN
            cCodCli := NULL;
      END;
        --
       BEGIN
        SELECT Substr(DECODE(T.IndDirDesp,'D','D'||T.Direc,'T'||T.DirecTrab),2,500)
          INTO cDirRet
          FROM TERCERO t
         WHERE t.TipoId = 0
           AND t.NumId  = cCodCli
           AND t.DvId   = 0;
        EXCEPTION
          WHEN OTHERS THEN
               cDirRet := NULL;
     END;
        --
        RETURN(cDirRet);
    END DIRECCION_ASEGURADO;
  /*-----------------------------------------------------------------------
    Nombre     : DISTRITO_ASEGURADO
    Prop?sito  : Devuelve la descripci?n del distrito del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nNumOpe  N?mero de operaci?n
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
    FUNCTION DISTRITO_ASEGURADO(p_NumOpe NUMBER) RETURN VARCHAR2 IS
      cDesMun  VARCHAR2(100);
      cIndDir  VARCHAR2(1);
      --
      cCodCli  CERTIFICADO.Codcli%TYPE;
    BEGIN
      --
        BEGIN
      SELECT DISTINCT CodCli
        INTO cCodCli
       FROM Certificado C, Oper_Pol OP
      WHERE OP.NumOper  = p_NumOpe
        AND OP.IdePol   = C.idepol
        AND OP.NumCert  = C.NumCert;
      EXCEPTION
       WHEN Too_Many_Rows THEN
            cCodCli := NULL;
      END;
        --
       BEGIN
        SELECT T.IndDirDesp
          INTO cIndDir
          FROM TERCERO t
         WHERE t.TipoId = 0
           AND t.NumId  = cCodCli
           AND t.DvId   = 0;
     END;
        --
    BEGIN   -- Obtiene Municipio
      SELECT DescMunicipio
        INTO cDesMun
        FROM TERCERO t, MUNICIPIO m
       WHERE t.TipoId       = 0
         AND t.NumId        = cCodCli
         AND t.DvId         = 0
         AND m.CodPais      = decode(cIndDir,'D',T.CodPais,t.CodPaisTrab)
         AND m.CodEstado    = decode(cIndDir,'D',t.CodEstado,t.CodEstadoTrab)
         AND m.CodCiudad    = decode(cIndDir,'D',t.CodCiudad,t.CodCiudadTrab)
         AND m.CodMunicipio = decode(cIndDir,'D',t.CodMunicipio,t.CodMunicipioTrab);
    EXCEPTION
     WHEN OTHERS THEN
                 cDesMun := '';
     END;
        --
        RETURN( cDesMun );
    END DISTRITO_ASEGURADO;
  /*-----------------------------------------------------------------------
    Nombre     : PROVINCIA_ASEGURADO
    Prop?sito  : Devuelve la descripci?n de la provincia del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nNumOpe  N?mero de Operaci?n
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
    FUNCTION PROVINCIA_ASEGURADO(p_NumOpe NUMBER) RETURN VARCHAR2 IS
      cDesPro  VARCHAR2(100);
      cIndDir  VARCHAR2(1);
      --
      cCodCli  CERTIFICADO.Codcli%TYPE;
    BEGIN
      --
        BEGIN
      SELECT DISTINCT CodCli
        INTO cCodCli
       FROM Certificado C, Oper_Pol OP
      WHERE OP.NumOper  = p_NumOpe
        AND OP.IdePol   = C.idepol
        AND OP.NumCert  = C.NumCert;
      EXCEPTION
       WHEN Too_Many_Rows THEN
            cCodCli := NULL;
      END;
        --
       BEGIN
        SELECT T.IndDirDesp
          INTO cIndDir
          FROM TERCERO t
         WHERE t.TipoId = 0
           AND t.NumId  = cCodCli
           AND t.DvId   = 0;
        EXCEPTION
           WHEN OTHERS THEN
                cIndDir := NULL;
     END;
        --
    BEGIN   -- Obtiene provincia
      SELECT DescCiudad
        INTO cDesPro
        FROM TERCERO t, CIUDAD m
       WHERE t.TipoID   = 0
         AND t.NumId    = cCodCli
         AND t.DvId     = 0
         AND m.CodPais   = decode(cIndDir,'D',T.CodPais,T.CodPaisTrab)
         AND m.CodEstado = decode(cIndDir,'D',T.CodEstado,T.CodEstadoTrab)
         AND m.CodCiudad = decode(cIndDir,'D',T.CodCiudad,T.CodCiudadTrab);
    EXCEPTION
     WHEN OTHERS THEN
               cDesPro := '';
     END;
        --
        RETURN( cDesPro );
    END PROVINCIA_ASEGURADO;
  /*---------------------------------------------------------------------------------
    Nombre     : CODIGO_BARRA
    Prop?sito  : Devuelve el c?digo de barra, Se tom? funci?n del reporte EMI029
    Referencia : Transportes WEB
    Par?metros :
                 p_cCodMon  C?digo de moneda
                 p_nPriPag  Prima pagada
                 p_cNumDoc  N?mero de documento
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
           04/05/2004    Roberto Carranza    Se modific? la funci?n para que devuelva
                                             El codigo en digitos concatenado a los
                                             caracteres de control de la impresi?n
           09/07/2004    Ricardo Larrabure   Correcci?n c?digo de barra (N3019580)
   ----------------------------------------------------------------------------------*/
    FUNCTION CODIGO_BARRA(p_cCodMon VARCHAR2,
                          p_nPriPag NUMBER,
                          p_cNumDoc VARCHAR2) RETURN VARCHAR2 IS
      cMoneda      VARCHAR2(1);
      cChequeo     VARCHAR2(3);
      cValorDos    VARCHAR2(2);
      cDigito      VARCHAR2(10);
      cDigitoBarra VARCHAR2(10);
      cBar         VARCHAR2(24);
      cValorBarra  VARCHAR2(1000);
      cReturn      VARCHAR2(1000);
      --
      i            PLS_INTEGER;
      cPos         PLS_INTEGER;
      --
    BEGIN
      --
      BEGIN
       SELECT DECODE(p_cCodMon,'USD','2','1')
         INTO cMoneda
         FROM DUAL;
     EXCEPTION
          WHEN OTHERS THEN
               cMoneda := '1';
      END;
      --
     cValorBarra := '2801' || SUBSTR(p_cNumDoc,-8) || cMoneda || lpad((p_nPriPag * 100),8,'0');
      cvalorbarra := rpad(cValorBarra,22,0);
      cChequeo    := digito_chequeo(cValorBarra);
      cBar        := chr(125); --Caracter de inicio
      i           := 1;
      --
      WHILE i <= length(cValorBarra) LOOP
        --
        BEGIN
         SELECT SUBSTR(cValorBarra,i,2)
           INTO cValorDos
           FROM DUAL;
         EXCEPTION
            WHEN OTHERS THEN
                 cValorDos := '00';
       END;
        --
        SELECT POSICION
          INTO cPos
          FROM BARRA
         WHERE numero = cValorDos;
        --
        cbar := cbar || chr(cpos);
        i    := i + 2;
      END LOOP;
      --
      SELECT POSICION
        INTO cPos
        FROM BARRA
       WHERE numero = DECODE(LENGTH(cchequeo),3,cchequeo,LPAD(cchequeo,2,'0'));
      --
      cBar := cBar || chr(cPos) || chr(126);
      cValorBarra := cValorBarra || cChequeo;
      --<I N3019580> 09-07-2004 Correcci?n env?o unicamente c?digo de barra
      cReturn     := cValorBarra;
      --<F N3019580>
      RETURN(cReturn);
  END CODIGO_BARRA;
  /*--------------------------------------------------------------------------
    Nombre     : DIGITO_CHEQUEO
    Prop?sito  : Digito de verificaci?n, se tom? funci?n del reporte EMI029
    Referencia : Transportes WEB
    Par?metros :
                 C?digo
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   ----------------------------------------------------------------------------*/
  FUNCTION DIGITO_CHEQUEO(codigo VARCHAR2) RETURN VARCHAR2 IS
    valor      NUMBER;
    --
    longitud   PLS_INTEGER;
    peso       PLS_INTEGER;
    chequeo    PLS_INTEGER;
    i          PLS_INTEGER;
    n       PLS_INTEGER;
    --
    cvalordos  VARCHAR2(2);
    --
  BEGIN
    longitud := length(codigo);
    i        :=   1;
    peso     :=   1;
    valor    := 105;
    WHILE i <= longitud LOOP
      BEGIN
       SELECT SUBSTR(codigo,i,2)
         INTO cValorDos
         FROM DUAL;
       EXCEPTION
          WHEN OTHERS THEN
               cValorDos := '00';
     END;
      valor := valor + peso * cValorDos;
      i     := i + 2;
      peso  := peso + 1;
    END LOOP;
    chequeo := MOD(valor,103);
    RETURN(chequeo);
  END DIGITO_CHEQUEO;
  /*-----------------------------------------------------------------------
    Nombre     : FECHA_EN_LETRAS
    Prop?sito  : Devuelve la fecha en letras
    Referencia : Transportes WEB
    Par?metros :
                 dFecha  Fecha que se desea convertir a letras
    Log de Cambios:
           Fecha            Autor            Descripcion
           07/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
  FUNCTION FECHA_EN_LETRAS (dfecha DATE) RETURN VARCHAR2 IS
    dia     VARCHAR2(2);
    pre_mes VARCHAR2(2);
    mes     VARCHAR2(30);
    ano     VARCHAR2(4);
    cfecha  VARCHAR2(30);
    BEGIN
      dia     := to_char(dFecha,'dd');
      pre_mes := to_char(dFecha,'mm');
      ano     := to_char(dFecha,'yyyy');
      --
      IF pre_mes  = '01' THEN
        mes := 'Enero';
      ELSIF  pre_mes  = '02' THEN
        mes := 'Febrero';
      ELSIF  pre_mes  = '03' THEN
        mes := 'Marzo';
      ELSIF  pre_mes  = '04' THEN
        mes := 'Abril';
      ELSIF  pre_mes  = '05' THEN
        mes := 'Mayo';
      ELSIF  pre_mes  = '06' THEN
        mes := 'Junio';
      ELSIF  pre_mes  = '07' THEN
        mes := 'Julio';
      ELSIF  pre_mes  = '08' THEN
       mes := 'Agosto';
      ELSIF  pre_mes  = '09' THEN
       mes := 'Setiembre';
      ELSIF  pre_mes  = '10' THEN
       mes := 'Octubre';
      ELSIF  pre_mes  = '11' THEN
       mes := 'Noviembre';
      ELSIF  pre_mes  = '12' THEN
       mes := 'Diciembre';
      END IF;
      --
      cFecha := dia||' de '||mes;
      --
      IF TO_NUMBER(ANO) < 2000 THEN
          cFecha := cFecha ||' de ';
      ELSE
           cFecha := cFecha ||' del ';
      END IF;
      --
      cFecha := cFecha || ano;
      RETURN(cfecha);
    END FECHA_EN_LETRAS;
  /*-----------------------------------------------------------------------
    Nombre     : CODIGO_CLI_INT
    Prop?sito  : Devuelve codigo de intermediario o codigo de cliente
    Referencia : Transportes WEB
    Par?metros :
                 p_cRolUsr  Rol de usuario que ingreso al sistema
                 p_nUsrId   Identificador del usuario
    Log de Cambios:
           Fecha            Autor            Descripcion
           13/04/2004    Roberto Carranza    Creaci?n
           12/05/2004    Roberto Carranza    Se modific? la forma de llegar al
                                             Codigo del Cliente o Intermediario
           14/07/2004    Miguel Wilson       Devolver las m?ltiples relaciones laborales
                                             para incluirlas en una sentencia "IN"
   -------------------------------------------------------------------------*/
   FUNCTION CODIGO_CLI_INI (p_cRolUsr VARCHAR2,p_nUsrId NUMBER) RETURN VARCHAR2 IS
    cCodigo VARCHAR2(1000);
    --
    CURSOR c_Cliente IS
      SELECT I.Codcli
        FROM relaciones_terceros R, Cliente I
       WHERE R.Tipoidorig = I.TIPOID
         AND R.Numiddest  = I.NumId
         AND R.dvidorig   = I.Dvid
         AND R.tipoidorig = '0'
         AND R.numidorig  = p_nUsrId
         AND R.dvidorig   = '0'
         AND R.Tiporel    = 'RELACLIE'
         AND R.codrel     = '0009'
         AND R.stsrel     = 'ACT';
    --
    CURSOR c_Corredor IS
      SELECT I.CodInter
        FROM relaciones_terceros R, Intermediario I
       WHERE R.Tipoidorig = I.TIPOID
         AND R.Numiddest  = I.NumId
         AND R.dvidorig   = I.Dvid
         AND R.tipoidorig = '0'
         AND R.numidorig  = p_nUsrId
         AND R.dvidorig   = '0'
         AND R.Tiporel    = 'RELACLIE'
         AND R.codrel     = '0009'
         AND R.stsrel     = 'ACT';
    --
   BEGIN
    -- <I N3019624> Miguel Wilson / 19-07-2004 / Considerar mas de un cliente
    IF p_cRolUsr = '0001' THEN  -- Cliente
       FOR X IN c_Cliente LOOP
         cCodigo := cCodigo||CHR(39)||X.CodCli||CHR(39)||',';
       END LOOP;
       IF cCodigo IS NOT NULL THEN
          cCodigo := '('||cCodigo||CHR(39)||'X'||CHR(39)||')';
       ELSE
          cCodigo := '('||CHR(39)||'X'||CHR(39)||')';
       END IF;
    ELSIF p_cRolUsr = '0002' THEN  -- Corredor
       FOR X IN c_Corredor LOOP
         cCodigo := cCodigo||CHR(39)||X.CodInter||CHR(39)||',';
       END LOOP;
       IF cCodigo IS NOT NULL THEN
          cCodigo := '('||cCodigo||CHR(39)||'X'||CHR(39)||')';
       ELSE
          cCodigo := '('||CHR(39)||'X'||CHR(39)||')';
       END IF;
    ELSIF p_cRolUsr = '0003' THEN  -- Trabajador Rimac
          cCodigo := '('||CHR(39)||'X'||CHR(39)||')';
    END IF;
    --<F N3019624>
    RETURN( cCodigo );
   END CODIGO_CLI_INI;
  /*-----------------------------------------------------------------------
    Nombre     : FORMA_PAGO
    Prop?sito  : Devuelve la forma de pago de la factura
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdeFac  N?mero de identificaci?n de la factura
    Log de Cambios:
           Fecha            Autor            Descripcion
           16/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION FORMA_PAGO(p_nIdeFac NUMBER) RETURN VARCHAR2 IS
     cForPag   VARCHAR2(50);
   BEGIN
    --
    BEGIN
      SELECT D.Descrip
        INTO cForPag
        FROM acreencia A,
             giros_financiamiento B,
             cond_financiamiento C,
             lval D
       WHERE A.numacre   = b.numacre
         AND B.Numfinanc = c.numfinanc
         AND D.Tipolval  = 'TIPOFIN'
         AND D.CodlVal   = C.tipofin
         AND A.idefact   = p_nIdeFac;
    EXCEPTION
      WHEN OTHERS THEN
           cForPag := NULL;
    END;
    --
    RETURN(cForPag);
   END FORMA_PAGO;
  /*-----------------------------------------------------------------------
    Nombre     : Valida_Certificados
    Prop?sito  : Devuelve mensaje 'VARIOS' si consulta devuelve varios registros
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdePol  Identificador de P?liza
                 p_nNumOpe  N?mero de Operaci?n
    Log de Cambios:
           Fecha            Autor            Descripcion
           16/04/2004    Roberto Carranza    Creaci?n
           20/04/2004    Roberto Carranza    Modificaci?n.
   -------------------------------------------------------------------------*/
   FUNCTION VALIDA_CERTIFICADOS(p_nIdePol NUMBER,p_nNumOpe NUMBER) RETURN VARCHAR2 IS
     cNumCert VARCHAR2(20);
   BEGIN
    --
    BEGIN
      SELECT TO_CHAR(NumCert)
        INTO cNumCert
        FROM OPER_POL
       WHERE IdePol  = p_nIdePol
         AND NumOper = p_nNumOpe;
    EXCEPTION
       WHEN TOO_MANY_ROWS THEN
            cNumCert := 'VARIOS';
      WHEN OTHERS THEN
            cNumCert := NULL;
    END;
    --
    RETURN(cNumCert);
   END VALIDA_CERTIFICADOS;
  /*-----------------------------------------------------------------------
    Nombre     : NUMERO_ID
    Prop?sito  : Devuelve el NUMID del Asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdePol    Identificador de p?liza
                 p_nNumCer    N?mero de certificado
    Log de Cambios
       Fecha            Autor            Descripcion
       22/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION NUMERO_ID(p_nIdePol NUMBER,
                      p_nNumCer NUMBER ) RETURN VARCHAR2 IS
     --
     nNumId  CLIENTE.numid%TYPE;
     cCliAse CERTIFICADO.codcli%TYPE;
     --
   BEGIN
     --
     BEGIN
         -- Primero el codcli de certificado
        SELECT CodCli
          INTO cCliAse
          FROM certificado
         WHERE IdePol  = p_nIdePol
           AND NumCert = p_nNumCer;
     EXCEPTION
         WHEN OTHERS THEN
              cCliAse := NULL;
     END;
     --
     BEGIN
       SELECT C.Numid
         INTO nNumId
         FROM CLIENTE C
        WHERE C.CodCli = cCliAse;
     EXCEPTION
       WHEN OTHERS THEN
            nNumId := NULL;
     END;
     --
     RETURN(nNumId);
   END Numero_ID;
  /*-----------------------------------------------------------------------
    Nombre     : F_PAGO
    Prop?sito  : Devuelve la forma de pago
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdeDec    Identificador de p?liza
    Log de Cambios
       Fecha            Autor            Descripcion
       29/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION F_PAGO( p_nIdeDec NUMBER ) RETURN VARCHAR2 IS
     --
     nNumOpe   RECIBO.ideop%TYPE;
     nIdeFac   FACTURA.idefact%TYPE;
     --
     cForPag   VARCHAR2(50) DEFAULT NULL;
   BEGIN
     --
     BEGIN
       SELECT r.ideop
         INTO nNumOpe
         FROM Dec_Transporte_Flotante F, recibo R
        WHERE F.IdeRec = R.IdeRec
          AND F.IdeDec = p_nIdeDec;
     EXCEPTION
       WHEN OTHERS THEN
            nNumOpe := NULL;
     END;
     --
     BEGIN
       SELECT fa.idefact
         INTO nIdeFac
         FROM FACTURA FA
        WHERE FA.NumOper = nNumOpe;
     EXCEPTION
       WHEN OTHERS THEN
            nIdeFac := NULL;
     END;
     --
     IF nIdeFac IS NOT NULL THEN
        cForPag := PR_API_EMI_TRAN.forma_pago( nIdeFac );
     ELSE
        cForPag := NULL;
     END IF;
     --
     RETURN(cForPag);
   END F_PAGO;
  /*-----------------------------------------------------------------------
    Nombre     : F_PLAZO_VENCIDO
    Prop?sito  : Devuelve el n?mero de d?as de vencimiento
    Referencia : Transportes WEB
    Par?metros :
                 p_nNumPla    N?mero de planilla
    Log de Cambios
       Fecha            Autor            Descripcion
       29/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION PLAZO_VENCIDO( p_nNumPla VARCHAR2,p_cIndOri VARCHAR2 ) RETURN NUMBER IS
   --
   nResult  NUMBER DEFAULT 0;
   --
   cNumDia  VARCHAR2(2);
   nFecha   VARCHAR2(20);
   --
   nYear    PLS_INTEGER;
   --
   BEGIN
       IF p_cIndOri = 'W' THEN
          --
          IF SUBSTR(p_nNumPla,1,2) IN ('01','03','05','07','08','10','12') THEN
             cNumDia := '31';
          ELSIF SUBSTR(p_nNumPla,1,2) = '02' THEN
                nYear := to_number(substr(4,4));
                -- Falta determinar si es 28 o 29
                IF MOD(nYear,4) = 0 THEN
                   -- Biciesto
                   cNumDia := '29';
                ELSE
                   cNumDia := '28';
                END IF;
                --
          ELSE
                cNumDia := '30';
          END IF;
          --
          BEGIN
            nFecha := cNumDia || '/' || TRIM(p_nNumPla);
            -- Asumo que el formato de Planilla es MM/YYYY
            SELECT trunc(SYSDATE) - to_date(nFecha,'dd/mm/yyyy')
              INTO nResult
              FROM dual;
            --
            IF nResult < 0 THEN
               nResult := 0;
            END IF;
            --
          EXCEPTION
             WHEN OTHERS THEN
                NULL;
          END;
          --
       END IF;
       RETURN( nResult );
   END PLAZO_VENCIDO;
  /*-----------------------------------------------------------------------
    Nombre     : DESCRIPCION_AGENCIA
    Prop?sito  : Devuelve la descripci?n de la Agencia
    Referencia : Transportes WEB
    Par?metros :
                 p_cCodAge    Codigo de Agencia
    Log de Cambios
       Fecha            Autor            Descripcion
       30/04/2004    Roberto Carranza    Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DESCRIPCION_AGENCIA( p_cCodAge VARCHAR2 ) RETURN VARCHAR2 IS
   --
   cResult VARCHAR2(100);
   cCadena VARCHAR2(20);
   --
   BEGIN
     --
     cCadena := SUBSTR(p_cCodAge,1,2) || '0000';
     --
     BEGIN
       SELECT INITCAP(Descrip)
         INTO cResult
         FROM LVAL
        WHERE TipoLval = 'OFICINAS'
          AND CodLval  = cCadena;
     EXCEPTION
        WHEN OTHERS THEN
             cResult := NULL;
     END;
     --
     RETURN(cResult);
   END DESCRIPCION_AGENCIA;
  /*-----------------------------------------------------------------------
    Nombre     : NUMERO_LIQUIDACION
    Prop?sito  : Devuelve el n?mero de liquidaci?n en base al IdeRec
    Referencia : Transportes WEB
    Par?metros :
                 p_nIdeRec    identificador de recibo
    Log de Cambios
       Fecha            Autor            Descripcion
       03/05/2004    Roberto Carranza    Creaci?n
       18/09/2008    Jose Porlles C.     EMI-03396
   -------------------------------------------------------------------------*/
   FUNCTION NUMERO_LIQUIDACION( p_nIdeRec VARCHAR2 ) RETURN NUMBER IS
   --
   nResult NUMBER DEFAULT NULL;
   nIdeFac FACTURA.idefact%TYPE;
   --
   BEGIN
     --
     BEGIN
      SELECT A.Idefact
        INTO nIdeFac
        FROM Rec_Financiamiento r,Giros_Financiamiento g, Acreencia A
       WHERE r.numfinanc = g.numfinanc
         AND g.numacre   = a.numacre
         AND r.iderec    = p_nIdeRec
         AND g.STSGIRO  != 'ANU'; --<N3030548> Jose Porlles C.      18/09/2008   EMI-03396
     EXCEPTION
       WHEN OTHERS THEN
            nIdeFac := NULL;
     END;
     IF nIdeFac IS NOT NULL THEN
        BEGIN
          SELECT f.numfact
            INTO nResult
            FROM factura f
           WHERE Idefact = nIdeFac;
        EXCEPTION
           WHEN OTHERS THEN
                nResult := NULL;
        END;
     END IF;
     RETURN(nResult);
   END NUMERO_LIQUIDACION;
   --
   /*-----------------------------------------------------------------------
      Nombre      : FR_BACKUP_RESPONSABLES
      Prop?sito   : Copia datos del Resp. pago al temporal y viceversa
      Par?metros  : p_session Sesion del usuario.
                    p_cInd    Indica la acci?n a realizar
                              01 Insertar datos al temporal
                              02 Insertar del temporal a la tabla Resp_pago
                    p_nIdePol identificador de p?liza.
                    p_nNumCer N?mero de certificado.
      Log de Cambios.
        Fecha       Autor                      Descripci?n
        17/03/2004  Roberto Carranza           Creaci?n
        20/01/2020  Julio Portugal             <RTC230951> Refinanciamiento Responsable Pago    
   -------------------------------------------------------------------------*/
   PROCEDURE FR_BACKUP_RESPONSABLES(p_session NUMBER,
                                    p_cInd    VARCHAR2,
                                    p_nIdePol NUMBER,
                                    p_nNumCer NUMBER) IS
    --
    cExiste VARCHAR2(1);
    --
    CURSOR c_resp_pago IS
      SELECT Idepol,
             Numcert,
             Codcli,
             Porcpago,
             Codplanfracc,
             Nummodplanfracc,
             Codpais,
             Codestado,
             Codciudad,
             Codmunicipio,
             Direc,
             Telef1,
             Telef2,
             Telef3,
             Fax,
             Telex,
             Zip,
             Numdocref,
             Codcobrador,
             Diacobro,
             Codformcobro,
             Codruta,
             Codzonapostal,
             Codinstit,
             Fecproxvisita,
             Cantcuo,
             Numdoc,
             Formfact,
             Codagencia
             ,NumSec --<RTC230951>/20ENE2020/Jportugal       
        FROM RESP_PAGO
       WHERE IdePol  = P_nIdePol
         AND NumCert = P_nNumCer;
    --
    CURSOR c_resp_pago_temp IS
      SELECT Idepol,
             Numcert,
             Codcli,
             Porcpago,
             Codplanfracc,
             Nummodplanfracc,
             Codpais,
             Codestado,
             Codciudad,
             Codmunicipio,
             Direc,
             Telef1,
             Telef2,
             Telef3,
             Fax,
             Telex,
             Zip,
             Numdocref,
             Codcobrador,
             Diacobro,
             Codformcobro,
             Codruta,
             Codzonapostal,
             Codinstit,
             Fecproxvisita,
             Cantcuo,
             Numdoc,
             Formfact,
             Codagencia,
             Session_Id
             ,NumSec --<RTC230951>/20ENE2020/Jportugal       
        FROM RESP_PAGO_TEMP
       WHERE session_id = p_session;
    --
   BEGIN
    --
    IF p_cInd = '01' THEN
      -- Insertar datos al temporal
      FOR X IN c_resp_pago LOOP
         --
         BEGIN
           SELECT distinct 'S' --<RTC230951>/20ENE2020/Jportugal
             INTO cExiste
             FROM RESP_PAGO_TEMP
            WHERE session_id = p_session
              AND idepol     = p_nIdePol
              AND numcert    = p_nNumCer;
         EXCEPTION
           WHEN NO_DATA_FOUND THEN
                cExiste := 'N';
         END;
         --
         IF cExiste = 'N' THEN
            --
           BEGIN
             INSERT INTO RESP_PAGO_TEMP
                         (session_id , idepol         , numcert          , codcli,
                          porcpago   , codplanfracc   , nummodplanfracc  , codpais,
                          codestado  , codciudad      , codmunicipio     , direc,
                          telef1     , telef2         , telef3           , fax,
                          telex      , zip            , numdocref        , codcobrador,
                          diacobro   , codformcobro   , codruta          , codzonapostal,
                          codinstit  , fecproxvisita  , cantcuo          , numdoc,
                          formfact   , codagencia 
                          ,NumSec)--<RTC230951>/20ENE2020/Jportugal             
                  VALUES(p_session  , X.idepol       , X.numcert        , X.codcli,
                          X.porcpago , X.codplanfracc , X.nummodplanfracc, X.codpais,
                          X.codestado, X.codciudad    , X.codmunicipio   , X.direc,
                          X.telef1   , X.telef2       , X.telef3         , X.fax,
                          X.telex    , X.zip          , X.numdocref      , X.codcobrador,
                          X.diacobro , X.codformcobro , X.codruta        , X.codzonapostal,
                          X.codinstit, X.fecproxvisita, X.cantcuo        , X.numdoc,
                          X.formfact , X.codagencia
                          ,X.NumSec);--<RTC230951>/20ENE2020/Jportugal              
           EXCEPTION
              WHEN OTHERS THEN
                    RAISE_APPLICATION_ERROR(-20100,SQLERRM);
           END;
            --
         END IF;
         --
      END LOOP;
       --
    ELSIF p_cInd = '02' THEN
         -- Actualiza o Inserta desde el temporal
         FOR X IN c_resp_pago_temp LOOP
              UPDATE RESP_PAGO
                 SET codcli          = X.codcli,
                     porcpago        = X.porcpago,
                     codplanfracc    = X.codplanfracc,
                     nummodplanfracc = X.nummodplanfracc,
                     codpais         = X.codpais,
                     codestado       = X.codestado,
                     codciudad       = X.codciudad,
                     codmunicipio    = X.codmunicipio,
                     direc           = X.direc,
                     telef1          = X.telef1,
                     telef2          = X.telef2,
                     telef3          = X.telef3,
                     fax             = X.fax,
                     telex           = X.telex,
                     zip             = X.zip,
                     numdocref       = X.numdocref,
                     codcobrador     = X.codcobrador,
                     diacobro        = X.diacobro,
                     codformcobro    = X.codformcobro,
                     codruta         = X.codruta,
                     codzonapostal   = X.codzonapostal,
                     codinstit       = X.codinstit,
                     fecproxvisita   = X.fecproxvisita,
                     cantcuo         = X.cantcuo,
                     numdoc          = X.numdoc,
                     formfact        = X.formfact,
                     codagencia      = X.codagencia
               WHERE idepol     = X.idePol
                 AND numcert    = X.numcert
                 AND CodCli     = X.CodCli --<RTC230951>/20ENE2020/Jportugal
                 AND NumSec     = X.NumSec;--<RTC230951>/20ENE2020/Jportugal         
              --
              IF SQL%NOTFOUND THEN
               BEGIN
                  INSERT INTO RESP_PAGO
                         ( idepol         ,numcert          ,codcli         ,porcpago,
                           codplanfracc   ,nummodplanfracc  ,codpais        ,codestado,
                           codciudad      ,codmunicipio     ,direc          ,telef1,
                           telef2         ,telef3           ,fax            ,telex,
                           zip            ,numdocref        ,codcobrador    ,diacobro,
                           codformcobro   ,codruta          ,codzonapostal  ,codinstit,
                           fecproxvisita  ,cantcuo          ,numdoc         ,formfact,
                           codagencia 
                           ,NumSec)--<RTC230951/20ENE2020/Jportugal Refinanciamiento de Responsable de Pago              
                   VALUES(X.idepol       ,X.numcert        ,X.codcli       ,X.porcpago,
                           X.codplanfracc ,X.nummodplanfracc,X.codpais      ,X.codestado,
                           X.codciudad    ,X.codmunicipio   ,X.direc        ,X.telef1,
                           X.telef2       ,X.telef3         ,X.fax          ,X.telex,
                           X.zip          ,X.numdocref      ,X.codcobrador  ,X.diacobro,
                           X.codformcobro ,X.codruta        ,X.codzonapostal,X.codinstit,
                           X.fecproxvisita,X.cantcuo        ,X.numdoc       ,X.formfact,
                           X.codagencia
                           ,X.NumSec);--<RTC230951/20ENE2020/Jportugal Refinanciamiento de Responsable de Pago               
               EXCEPTION
                  WHEN OTHERS THEN
                         RAISE_APPLICATION_ERROR(-20100,SQLERRM);
               END;
              END IF;
              --
         END LOOP;
          fr_borrar_temporal(p_session);
    END IF;
    --
   END;
   /*-----------------------------------------------------------------------
      Nombre      :   FR_CARGA_RESP
      Prop?sito   :   Captura datos del responsable seleccionado y reemplaza el existente
      Par?metros  :   p_nIdePol Identificador de p?liza.
                      p_NumCer  N?mero de certificado.
                      p_CodCli  C?digo de responsable
                      p_CodPro  C?digo de producto
      Log de Cambios
        Fecha        Autor                      Descripci?n
        17/03/2004   Roberto Carranza           Creaci?n
        09/07/2004   Ricardo Larrabure          Correcci?n obtener datos de la forma de pago (N3019580)
        12/08/2004   Ricardo Larrabure          Evaluar plan de financiamiento a tomar(N3019834)
        29/09/2005   Jose Porlles               Validaci?n en caso el C?digo de Ruta sea NULL <N3023360>
        23/02/2006   Marko Chu                  Validacion que si no existe la Ruta el codigo del cobrador sea NULL <N3024241>
   -------------------------------------------------------------------------*/
   PROCEDURE FR_CARGA_RESP(p_nIdePol NUMBER,
                           p_NumCer  NUMBER,
                           p_CodCli  VARCHAR2,
                           p_CodPro  VARCHAR2) IS
    resp_pago_rec    RESP_PAGO%ROWTYPE;
    cTipoFin         PRODUCTO.tipofin%TYPE;
    cIndContado      PRODUCTO.indcontado%TYPE;
    nCantGiros       PLAN_FINANCIAMIENTO.nrogiros%TYPE;
    cTipoCli         CLIENTE.tipocli%TYPE;
    cOtrCodPlan      PLAN_FINANCIAMIENTO.CodPlan%TYPE; -- <I N3019834> Ricardo Larrabure / 12-08-2004 / Mejorar criterio para obtener plan de financiamiento
    cOtrTipoCli       PLAN_FINANCIAMIENTO.TIpoCli%TYPE;
   BEGIN
   -- Primero capturar los datos
   BEGIN
     SELECT C.TipoCli,
            T.CodPais,
            T.CodEstado,
            T.CodCiudad,
            T.CodMunicipio,
            T.Direc,
            T.Telef1,
            T.Telef2,
            T.Telef3,
            T.Fax,
            T.Telex,
            T.Zip,
            T.Codzonapostal,
            T.ZonaCob
       INTO cTipoCli,
            RESP_PAGO_REC.CodPais,
            RESP_PAGO_REC.CodEstado,
            RESP_PAGO_REC.CodCiudad,
            RESP_PAGO_REC.CodMunicipio,
            RESP_PAGO_REC.Direc,
            RESP_PAGO_REC.Telef1,
            RESP_PAGO_REC.Telef2,
            RESP_PAGO_REC.Telef3,
            RESP_PAGO_REC.Fax,
            RESP_PAGO_REC.Telex,
            RESP_PAGO_REC.Zip,
            RESP_PAGO_REC.Codzonapostal,
            RESP_PAGO_REC.CodRuta
       FROM TERCERO T, CLIENTE C
      WHERE T.TipoId = C.TipoId
        AND T.NumId  = C.NumId
        AND T.DvId   = C.DvId
        AND C.CodCli = p_CodCli;
   EXCEPTION
        WHEN NO_DATA_FOUND THEN
             RAISE_APPLICATION_ERROR(-20100,'No Existe Responsable de Pago');
   END;

   --<I N3023360> Validaci?n en caso el C?digo de Ruta sea NULL
    IF RESP_PAGO_REC.CodRuta IS NOT NULL THEN
       BEGIN
         BEGIN
             SELECT CodCobrador
               INTO RESP_PAGO_REC.codcobrador
               FROM DESC_RUTA
              WHERE CodRuta = RESP_PAGO_REC.CodRuta;
         EXCEPTION
              WHEN NO_DATA_FOUND THEN
                   RESP_PAGO_REC.codcobrador := NULL;   -- <N3024241> Marko Chu / 23-02-2006
         END;
       END;
    ELSE
      RESP_PAGO_REC.codcobrador := NULL;
    END IF;
   --<F N3023360>

    BEGIN
     SELECT NVL(TipoFin,'OTR'),NVL(IndContado,'N')
       INTO cTipoFin,cIndContado
       FROM PRODUCTO
      WHERE CodProd = p_CodPro;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
            cTipoFin    := 'OTR';
            cIndContado := 'N';
    END;

   BEGIN
     SELECT CodPlan,
            ModPlan,

            NroGiros
       INTO RESP_PAGO_REC.CodPlanFracc,
            RESP_PAGO_REC.NumModPlanFracc,
            nCantGiros
       FROM PLAN_FINANCIAMIENTO P
      WHERE TipoFin = DECODE(cTipoFin,'OTR','CON',cTipoFin)
        AND TipoCli = DECODE(cTipoCli,'J','P','G','E',cTipoCli)
         AND ROWNUM = 1
         ORDER BY P.CodPlan; -- <I N3019834> Ricardo Larrabure / 12-08-2004 / Mejorar criterio para obtener plan de financiamiento
   EXCEPTION
     WHEN OTHERS THEN
            RESP_PAGO_REC.CodPlanFracc    := NULL;
           RESP_PAGO_REC.NumModPlanFracc := NULL;
           nCantGiros                    := 0;
   END;

   IF cIndContado = 'S' THEN
      RESP_PAGO_REC.CantCuo := 1;
   ELSE
      RESP_PAGO_REC.CantCuo := nCantGiros;
   END IF;
    --
    BEGIN
      SELECT NumDocRef,
             NumDoc
        INTO RESP_PAGO_REC.NumDocRef,
             RESP_PAGO_REC.NumDoc
        FROM DOC_PAGO_CLIENTE
       WHERE CodCli    = p_CodCli
         AND IndDocDef = 'S';
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
             RESP_PAGO_REC.NumDocRef := NULL;
             RESP_PAGO_REC.NumDoc    := NULL;
        WHEN TOO_MANY_ROWS THEN
             RESP_PAGO_REC.NumDocRef := NULL;
             RESP_PAGO_REC.NumDoc    := NULL;
    END;

    --<I N3019580> Obtener datos de la forma de pago del responsable de pago del certificado
    BEGIN
       SELECT R.DiaCobro, R.CodFormCobro, R.CodInstit,
              R.FecProxVisita, R.FormFact, R.CodAgencia,
              R.CodPlanFracc
       INTO   RESP_PAGO_REC.DiaCobro, RESP_PAGO_REC.CodFormCobro, RESP_PAGO_REC.CodInstit,
              RESP_PAGO_REC.FecProxVisita, RESP_PAGO_REC.FormFact, RESP_PAGO_REC.CodAgencia,
              cOtrCodPlan
       FROM   RESP_PAGO R
       WHERE  R.IdePol = p_nIdePol
          AND Numcert = p_NumCer
          AND RowNum  = 1;
    EXCEPTION
       WHEN NO_DATA_FOUND THEN
          RESP_PAGO_REC.DiaCobro      := NULL;
          RESP_PAGO_REC.CodFormCobro  := NULL;
          RESP_PAGO_REC.CodInstit     := NULL;
          RESP_PAGO_REC.FecProxVisita := NULL;
          RESP_PAGO_REC.FormFact      := NULL;
          RESP_PAGO_REC.CodAgencia    := NULL;
          cOtrCodPlan                 := NULL;
    END;
    --<F N3019580>

    -- <I N3019834> Ricardo Larrabure / 12-08-2004 / Mejorar criterio para obtener plan de financiamiento
    SELECT P.TipoCli
    INTO   cOtrTipoCli
    FROM   PLAN_FINANCIAMIENTO P
    WHERE  P.CodPlan = cOtrCodPlan;

    IF cOtrCodPlan NOT IN ('CON001','CON002') AND cTipoCli = cOtrTipoCli THEN
       RESP_PAGO_REC.CodPlanFracc := cOtrCodPlan;
    END IF;
    -- <F N3019834>

    -- Borrar los datos de responsable
    DELETE FROM RESP_PAGO
     WHERE IdePol  = p_nIdePol
       AND Numcert = p_NumCer;
    -- Ahora insertar en tabla de responsables de pago.
   RESP_PAGO_REC.idepol   := p_nIdePol;
   RESP_PAGO_REC.numcert  := p_NumCer;
   RESP_PAGO_REC.codcli   := p_CodCli;
   RESP_PAGO_REC.porcpago := 100;
   --
    BEGIN
    INSERT INTO RESP_PAGO
         ( idepol       , numcert,
             codcli       , porcpago,
             codplanfracc , nummodplanfracc,
             codpais      , codestado,
             codciudad    , codmunicipio,
             direc        , telef1,
             telef2       , telef3,
             fax          , telex,
             zip          , numdocref,
             codcobrador  , diacobro,
           codformcobro , codruta,
             codzonapostal, codinstit,
             fecproxvisita, cantcuo,
             numdoc       , formfact,
             codagencia )
    VALUES(RESP_PAGO_REC.idepol       , RESP_PAGO_REC.numcert,
             RESP_PAGO_REC.codcli       , RESP_PAGO_REC.porcpago,
             RESP_PAGO_REC.codplanfracc , RESP_PAGO_REC.nummodplanfracc,
             RESP_PAGO_REC.codpais      , RESP_PAGO_REC.codestado,
             RESP_PAGO_REC.codciudad    , RESP_PAGO_REC.codmunicipio,
           RESP_PAGO_REC.direc        , RESP_PAGO_REC.telef1,
             RESP_PAGO_REC.telef2       , RESP_PAGO_REC.telef3,
             RESP_PAGO_REC.fax          , RESP_PAGO_REC.telex,
             RESP_PAGO_REC.zip          , RESP_PAGO_REC.numdocref,
             RESP_PAGO_REC.codcobrador  , RESP_PAGO_REC.diacobro,
           RESP_PAGO_REC.codformcobro , RESP_PAGO_REC.codruta,
             RESP_PAGO_REC.codzonapostal, RESP_PAGO_REC.codinstit,
             RESP_PAGO_REC.fecproxvisita, RESP_PAGO_REC.cantcuo,
             RESP_PAGO_REC.numdoc       , RESP_PAGO_REC.formfact,
             RESP_PAGO_REC.codagencia);
    EXCEPTION
     WHEN OTHERS THEN
           RAISE_APPLICATION_ERROR(-20100,SQLERRM);
    END;
    --
   END;
   /*-----------------------------------------------------------------------
      Nombre      :   FR_BORRAR_TEMPORAL
      Proposito   :   Borra los datos del temporal en base la Sesi?n
      Par?metros  :   p_session  Identificador de la sesi?n
      Log de Cambios
        Fecha        Autor                      Descripci?n
        17/03/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   PROCEDURE FR_BORRAR_TEMPORAL(p_session NUMBER) IS
   BEGIN
     --
     DELETE
       FROM RESP_PAGO_TEMP
     WHERE session_id = P_Session;
     --
   END;
   /*-----------------------------------------------------------------------
      Nombre      :   FR_BORRAR_TMP
      Proposito   :   Borra los datos del temporal en base al IdePol
      Par?metros  :   p_IdePol  Identificador de p?liza
      Log de Cambios
        Fecha        Autor                      Descripci?n
        17/03/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   PROCEDURE FR_BORRAR_TMP(p_IdePol NUMBER) IS
   BEGIN
     --
     DELETE
       FROM RESP_PAGO_TEMP T
      WHERE T.idepol  = p_IdePol;
     --
   END;
   /*-----------------------------------------------------------------------
      Nombre      :   Existe_temporal
      Proposito   :   Verifica si existen datos del IdePol en el temporal
      Par?metros  :   p_IdePol  Identificador de p?liza
      Log de Cambios
        Fecha        Autor                      Descripci?n
        17/03/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION EXISTE_TEMPORAL(p_IdePol NUMBER) RETURN BOOLEAN IS
     cInd   CHAR(1);
     Result BOOLEAN := TRUE;
   BEGIN
     BEGIN
       --
       SELECT '1'
         INTO cInd
         FROM RESP_PAGO_TEMP T
        WHERE T.idepol  = p_IdePol
          AND ROWNUM    = 1;
       --
     EXCEPTION
       WHEN NO_DATA_FOUND THEN
            Result := FALSE;
     END;
     RETURN(Result);
   END Existe_Temporal;
   /*-------------------------------------------------------------------------------
      Nombre      :   Politica
      Proposito   :   Evalua si hubo violaci?n de pol?ticas
      Par?metros  :   p_cNumPla Numero de Planilla
                      p_IdePol  Identificador de p?liza
                      p_cStsDec Estado de la declaracion
      Log de Cambios
        Fecha        Autor                      Descripci?n
        07/05/2004   Roberto Carranza           Creaci?n
        18/05/2004   Roberto Carranza           Se adicion? el indicador de origen
   ---------------------------------------------------------------------------------*/
   FUNCTION POLITICA(p_cNumPla VARCHAR2,p_nIdePol NUMBER,p_cStsDec VARCHAR2,p_cIndOri VARCHAR2) RETURN VARCHAR2 IS
     --
     cIndPol   VARCHAR2(1) DEFAULT 'N';
     --
   BEGIN
     --
     BEGIN
       SELECT 'S'
         INTO cIndPol
         FROM DUAL
        WHERE EXISTS (SELECT 1
                        FROM dec_transporte_flotante d
                       WHERE D.Idepol     = p_nIdePol
                         AND D.Planilla   = p_cNumPla
                         AND D.Indsisorig = p_cIndOri
                         AND ((D.StsDec = 'INC') OR (D.Indpolitica = 'S' AND D.Stsdec = p_cStsDec)));
     EXCEPTION
       WHEN too_many_rows THEN
            cIndPol := 'S';
       WHEN no_data_found THEN
            cIndPol := 'N';
     END;
     --
     RETURN(cIndPol);
   END;
   /*-----------------------------------------------------------------------
      Nombre      :   INDICADOR_PROCESO
      Proposito   :   Devuelve un indicador en base al estado de la poliza y certificado
      Par?metros  :   p_nIdePol  Identificador de p?liza
                      p_nNumCer  N?mero de Certificado
      Log de Cambios
        Fecha        Autor                      Descripci?n
        08/05/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION INDICADOR_PROCESO(p_nIdePol NUMBER,p_nNumCer NUMBER) RETURN VARCHAR2 IS
     --
     cIndPro   VARCHAR2(1) DEFAULT '0';
     cEstPol   VARCHAR2(3);
     cEstCer   VARCHAR2(3);
     cIndFac   VARCHAR2(1);
     --
   BEGIN
     --
     BEGIN
       SELECT P.stspol,P.Indfacult
         INTO cEstPol,cIndFac
         FROM poliza P
        WHERE p.IdePol = p_nIdePol;
     EXCEPTION
       WHEN no_data_found THEN
        RAISE_APPLICATION_ERROR(-20100,'El Identificador de la P?liza no Existe' || p_nIdePol );
       WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20100,'Error: ' || SQLERRM );
     END;
     --
     IF cIndFac = 'S' THEN
        RETURN('4');
     END IF;
     --
     BEGIN
       SELECT C.Stscert
         INTO cEstCer
         FROM certificado C
        WHERE C.IdePol  = p_nIdePol
          AND C.Numcert = p_nNumCer;
     EXCEPTION
        WHEN no_data_found THEN
             cEstCer := 'XXX';
        WHEN OTHERS THEN
             RAISE_APPLICATION_ERROR(-20100,'El Certificado No Existe '|| p_nNumCer );
     END;
     --
     IF cEstPol = 'ACT' AND cEstCer = 'ACT' THEN
        cIndPro := '1';
     ELSIF cEstPol = 'MOD' AND cEstCer = 'ACT' THEN
           cIndPro := '2';
     ELSE
       cIndPro := '0';
     END IF;
     --
     RETURN(cIndPro);
   END INDICADOR_PROCESO;
   /*-----------------------------------------------------------------------
      Nombre      :   IND_PROCESO_PLANILLA
      Proposito   :   Devuelve un indicador en base al estado de la poliza y certificado
      Par?metros  :   p_nIdePol  Identificador de p?liza
                      p_cNumPla  N?mero de Planilla
      Log de Cambios
        Fecha        Autor                      Descripci?n
        08/05/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION IND_PROCESO_PLANILLA(p_nIdePol NUMBER,p_cNumPla VARCHAR2) RETURN VARCHAR2 IS
     --
     cIndPro   VARCHAR2(1) DEFAULT '0';
     cEstPol   VARCHAR2(3);
     cEstCer   VARCHAR2(3);
     cIndFac   VARCHAR2(1);
     --
   BEGIN
     --
     BEGIN
       SELECT P.stspol,P.Indfacult
         INTO cEstPol, cIndFac
         FROM poliza P
        WHERE p.IdePol  = p_nIdePol;
     EXCEPTION
        WHEN OTHERS THEN
             RAISE_APPLICATION_ERROR(-20100,'Error Recuperando los datos de estado de Poliza y Certificado');
     END;
     --
     IF cIndFac = 'S' THEN
        RETURN('4');
     END IF;
     --
     BEGIN
      SELECT DISTINCT StsCert
        INTO cEstCer
        FROM CERTIFICADO C
       WHERE IDePol = p_nIdePol
         AND NumCert IN (SELECT DISTINCT NumCert
                           FROM DEC_TRANSPORTE_FLOTANTE D
                          WHERE IdePol = C.IdePol
                            AND D.PLanilla = p_cNumPla);
    EXCEPTION
        WHEN too_many_rows THEN
             cEstCer := 'XXX';
        WHEN OTHERS THEN
             RAISE_APPLICATION_ERROR(-20100,'Error Recuperando los datos de estado de Poliza y Certificado');
    END;
    --
     IF cEstPol = 'ACT' AND cEstCer = 'ACT' THEN
        cIndPro := '1';
     ELSIF cEstPol = 'MOD' AND cEstCer = 'ACT' THEN
           cIndPro := '2';
     ELSE
        cIndPro := '0';
     END IF;
     --
     RETURN(cIndPro);
   END IND_PROCESO_PLANILLA;
   /*-----------------------------------------------------------------------
      Nombre      :   DESC_COBERTURA
      Proposito   :   Devuelve la descripcion de la cobertura
      Par?metros  :   p_nIdeCob  Identificador de la cobertura
                      p_cCodRam  Codigo de Ramo
      Log de Cambios
        Fecha        Autor                      Descripci?n
        13/05/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION DESC_COBERTURA(p_nIdeCob NUMBER,p_cCodRam VARCHAR2) RETURN VARCHAR2 IS
     --
     cDesCob  COBERT_RAMO.desccobert%TYPE;
     --
   BEGIN
     --
     BEGIN
        SELECT CR.DescCobert
          INTO cDesCob
          FROM COBERT_CERT CC,COBERT_RAMO CR
         WHERE CC.IdeCobert = p_nIdeCob
           AND CR.CodRamo   = p_cCodRam
           AND CR.CodCobert = CC.CodCobert;
     EXCEPTION
        WHEN OTHERS THEN
             cDesCob := NULL;
     END;
     --
     RETURN(cDesCob);
   END DESC_COBERTURA;
   /*-----------------------------------------------------------------------
      Nombre      :   TITULO_DEC
      Proposito   :   Devuelve el titulo para el reporte de declaraci?n
      Par?metros  :   p_nIdePol  Identificador de la poliza
      Log de Cambios
        Fecha        Autor                      Descripci?n
        14/05/2004   Roberto Carranza           Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION TITULO_DEC(p_nIdePol NUMBER) RETURN VARCHAR2 IS
      --
      cCodProd     POLIZA.codprod%TYPE;
      cCodPlan     CERT_RAMO.codplan%TYPE;
      cRevPlan     CERT_RAMO.revplan%TYPE;
      cCodRamoCert CERT_RAMO.codramocert%TYPE;
      nNumPol      POLIZA.numpol%TYPE;
      --
      cTitulo      VARCHAR2(200);
   BEGIN
      --
      BEGIN
         SELECT P.CodProd, CR.CodPlan, CR.RevPlan,P.NumPol
           INTO cCodProd, cCodPlan, cRevPlan, nNumPol
           FROM POLIZA P,CERTIFICADO C,CERT_RAMO CR
          WHERE P.IdePol  = p_nIdePol
            AND C.IdePol  = P.IdePol
            AND CR.IdePol = C.IdePol
            AND CR.NumCert= C.NumCert
            AND ROWNUM < 2;
      EXCEPTION
        WHEN OTHERS THEN
             cCodProd     := NULL;
             cCodPlan     := NULL;
             cRevPlan     := NULL;
             cCodRamoCert := NULL;
              nNumPol      := NULL;
      END;
      --
      BEGIN
        SELECT Titulo
          INTO cTitulo
          FROM IMP_RAMO_PLAN_PROD I
         WHERE I.CodProd     = cCodProd
           AND I.CodPlan     = cCodPlan
           AND I.RevPlan     = cRevPlan
         --AND I.CodRamoPlan = cCodRamoCert
           AND I.Nivel       = 'POLIZA'
           AND I.Seccion     = 'POLIZA';
      EXCEPTION
        WHEN OTHERS THEN
             cTitulo := NULL;
      END;
      --
      RETURN(cTitulo ||' ' ||cCodProd ||' - '|| lpad(nNumPol,6,'0'));
   END TITULO_DEC;
   /*-----------------------------------------------------------------------
      Nombre      :   ES_CONTRATANTE
      Proposito   :   Verifica si el contratante existe en la tabla
                      POLIZA_CLIENTE
      Par?metros  :   p_nIdePol  Identificador de la poliza
                      p_cResPag  Codigo del contratante
      Log de Cambios
        Fecha        Autor                      Descripci?n
        14/07/2004   Miguel Wilson              Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION ES_CONTRATANTE(p_nIdePol NUMBER, p_cResPag VARCHAR2) RETURN VARCHAR2 IS

     cEstado VARCHAR2(1);

   BEGIN
     BEGIN
       SELECT 'S'
         INTO cEstado
         FROM POLIZA_CLIENTE
        WHERE IdePol = p_nIdePol
          AND CodCli = p_cResPag;

     EXCEPTION
       WHEN NO_DATA_FOUND THEN
         cEstado := 'N';
       WHEN TOO_MANY_ROWS THEN
         cEstado := 'S';
     END;

     RETURN cEstado;
   END ES_CONTRATANTE;
   /*-----------------------------------------------------------------------
      Nombre      :   AGREGAR_CONTRATANTE
      Proposito   :   Inserta el contratante a las tablas POLIZA_CLIENTE y
                      MOD_POLIZA_CLIENTE
      Par?metros  :   p_nIdePol  Identificador de la poliza
                      p_cResPag  Codigo del contratante
      Log de Cambios
        Fecha        Autor                      Descripci?n
        14/07/2004   Miguel Wilson              Creaci?n
   -------------------------------------------------------------------------*/
   PROCEDURE AGREGAR_CONTRATANTE(p_nIdepol NUMBER, p_cResPag VARCHAR2) IS
   BEGIN
     BEGIN
       INSERT INTO POLIZA_CLIENTE
           (IdePol, CodCli, IndRespMov, StsPolCli)
       VALUES
           (p_nIdePol, p_cResPag, 'S', 'ACT');

     EXCEPTION
       WHEN OTHERS THEN
         RAISE_APPLICATION_ERROR(-20100,'PR_API_EMI_TRAN.AGREGAR_CONTRATANTE : Poliza: '||p_nIdePol||' Contratante: '||p_cResPag||', no se pudo insertar en la tabla POLIZA_CLIENTE ' || SQLERRM);
     END;

     BEGIN
       INSERT INTO MOD_POLIZA_CLIENTE
           (IdePol, CodCli, IndRespMov, StsPolCli, NumMod)
       VALUES
           (p_nIdePol, p_cResPag, 'S', 'ACT', 0);

     EXCEPTION
       WHEN OTHERS THEN
         RAISE_APPLICATION_ERROR(-20100,'PR_API_EMI_TRAN.AGREGAR_CONTRATANTE : Poliza: '||p_nIdePol||' Contratante: '||p_cResPag||', no se pudo insertar en la tabla MOD_POLIZA_CLIENTE' || SQLERRM);
     END;
   END AGREGAR_CONTRATANTE;


   /*-----------------------------------------------------------------------
      Nombre      :   CREA_DIST_FACUL
      Proposito   :   Inserta los datos en la tabla dist_facult
      Par?metros  :   p_cUsrLog  Usuario
                      p_nIdePol  Identificador de la poliza
                      p_nNumCer  N?mero de Certificado
                      p_cCodRam  C?digo de Ramo
                      p_nIndDec  Indicador de Declaraci?n
      Log de Cambios
        Fecha        Autor                      Descripci?n
        09/11/2005   Jose Porlles               Creaci?n
        12/01/2007   Marco  Geldres             <N3026686>Unificar Correlativo de Cesi?n de Coaseguro Proyecto Reaseguros
        25/06/2012   Javier La Chira            <N3039243> Se agreg? nuevo campo en tabla DIST_FACULT: PorcPrimaOtros
   -------------------------------------------------------------------------*/
   PROCEDURE CREA_DIST_FACUL(
     p_cUsrLog VARCHAR2,
     p_nIdePol NUMBER,
     p_nNumCer NUMBER,
     p_cCodRam VARCHAR2,
     p_nIndDec NUMBER
     ) IS
     --
     cIndFacult      POLIZA.Indfacult%TYPE;
     nIdeRec         DEC_TRANSPORTE_FLOTANTE.Iderec%TYPE;
     nPrima          DEC_TRANSPORTE_FLOTANTE.Prima%TYPE;
     dFecIniValid    DEC_TRANSPORTE_FLOTANTE.FecIniValid%TYPE;
     dFecFinValid    DEC_TRANSPORTE_FLOTANTE.FecFinValid%TYPE;
     nMtoLiqLocal    DEC_TRANSPORTE_FLOTANTE.Mtoliqlocal%TYPE;
     nNumModDistRea  DIST_REA_MOD.Nummoddistrea%TYPE;
     nIndExiRea      NUMBER(1) := 1;
     nIndExiFacu     NUMBER(1) := 1;
     kSI             CONSTANT VARCHAR2(1) := 'S';
     kNO             CONSTANT VARCHAR2(1) := 'N';
     kIdeCttoFPU     CONSTANT CTTO_REA.IDECTTOREA%TYPE := 99999999999999;
     ----------
     nPrimaDistRea   DIST_REA.PrimaDistRea%TYPE;
     nPorcPrimaRec   DIST_REA.Porcprimarec%TYPE;
     nPorcFacult     NUMBER;
     nPrimaDistReaFacult NUMBER;
     nIdePolAux      POLIZA.IdePol%TYPE;
     nNumCerAux      CERTIFICADO.NumCert%TYPE;
     nIdeRecAux      DEC_TRANSPORTE_FLOTANTE.IdeRec%TYPE;
     nIndDecAux      DEC_TRANSPORTE_FLOTANTE.IdeDec%TYPE;
     nExiste         NUMBER := 1;
     DF              DIST_FACULT%ROWTYPE;

     CURSOR c_DistReaRecOp IS
       SELECT /*+ ALL_ROWS */
              DA.IdePol, DA.NumCert, DA.CodRamoCert, DA.CodRamoRea,
              SUM(NVL(DA.SumaDistRea, 0)), DA.IdeRec, DA.IdeCttoRea
       FROM   DIST_REA DA
       WHERE  DA.IdePol = p_nIdePol
         AND  DA.IdeCttoRea = kIdeCttoFPU
         AND  DA.NumModDistRea = 0
         AND EXISTS(SELECT 1
                      FROM RECIBO R, OPER_POL OP
                     WHERE R.IdeRec    = DA.IdeRec
                       AND OP.NumOper  = R.IdeOp
                       AND NVL(OP.IndAnul, kNO) <> kSI)
                     GROUP BY DA.NumCert, DA.CodRamoCert, DA.CodRamoRea, DA.IdeRec,
                             DA.IdeCttoRea, DA.IdePol
                    HAVING SUM(NVL(SumaDistRea, 0)) <> 0
                       AND SUM(NVL(PrimaDistRea, 0)) <> 0;

   BEGIN
     BEGIN
       SELECT p.indfacult
         INTO cIndFacult
         FROM poliza p
        WHERE p.IdePol = p_nIdePol;
     EXCEPTION
       WHEN OTHERS THEN
         cIndFacult := kNO;
     END;

     IF cIndFacult = kSI THEN

       BEGIN
         SELECT d.iderec, d.prima, d.mtoliqlocal, d.fecinivalid, d.fecfinvalid
           INTO nIdeRec, nPrima, nMtoLiqLocal, dFecIniValid, dFecFinValid
           FROM dec_transporte_flotante d
          WHERE d.IdePol  = p_nIdePol AND
                d.numcert = p_nNumCer AND
                d.idedec  = p_nIndDec;
       END;

       IF (nIdeRec IS NULL) THEN

         FOR DR IN c_DistReaRecOp  LOOP
           BEGIN
             SELECT 0
              INTO  nExiste
              FROM  DIST_FACULT DF
             WHERE  DF.IdePol = DR.IdePol
               AND  DF.NumCert = DR.NumCert
               AND  DF.CodRamoCert = DR.CodRamoCert
               AND  DF.IdeRec = DR.IdeRec
               AND  DF.IdeCttoRea = DR.IdeCttoRea
               AND  DF.NumModDistRea = 0
               AND DF.CodRamoRea = DR.CodRamoRea;
           EXCEPTION
             WHEN NO_DATA_FOUND THEN
               nIdePolAux := DR.IdePol;
               nNumCerAux := DR.NumCert;
               nIdeRecAux := DR.IdeRec;
               nExiste    := 1;
             WHEN TOO_MANY_ROWS THEN
               nExiste := 0;
           END;

           IF nExiste = 1 THEN
             BEGIN --datos dec_transporte_flotante
               SELECT d.fecsts, d.fecinivalid, d.fecfinvalid
                 INTO DF.FecSts, DF.FecIniVig, DF.FecFinVig
                 FROM dec_transporte_flotante d
                WHERE d.IdePol  = nIdePolAux
                  AND d.numcert = nNumCerAux
                  AND d.iderec  = nIdeRecAux;
             END;

             BEGIN --datos de dist_rea y gen_rea
               SELECT DR.IdePol, DR.NumCert, DR.CodRamoCert, DR.CodCli, DR.IdeCttoRea, DR.ClaseRiesgo,
                      DR.IdeRec, DR.NumModDistRea, DR.SumaDistrea, DR.PrimaDistrea, DR.CodRamoRea,
                      G.FecIniVig, G.FecFinVig, NVL(G.PrimaMov,0)
                 INTO DF.IdePol, DF.NumCert, DF.CodRamoCert, DF.CodCli, DF.IdeCttoRea, DF.ClaseRiesgo,
                      DF.IdeRec, DF.NumModDistRea, DF.SumaCedida, DF.PrimaCedida, DF.CodRamoRea,
                      DF.FecIniVig, DF.FecFinVig, nPrimaDistRea
                 FROM GEN_REA G, DIST_REA DR
                WHERE G.IdeRec       = nIdeRecAux
                  AND G.IdePol       = nIdePolAux
                  AND G.NumCert      = nNumCerAux
                  AND G.CodRamoCert  = p_cCodRam
                  AND DR.IdeRec      = G.IdeRec
                  AND DR.IdePol      = G.IdePol
                  AND DR.NumCert     = G.NumCert
                  AND DR.CodRamoCert = G.CodRamoCert
                  AND DR.CodRamoRea  = G.CodRamoRea
                  AND DR.IdeCttoRea  = kIdeCttoFPU;
             END;

             BEGIN --datos DIST_FACULT
               SELECT DF1.CodAcepRiesgo, DF1.PorcCom, DF1.PorcDep, DF1.IntDep, DF1.ImpRet,
                      DF1.ImpRem, DF1.DiaPago, DF1.NumModGenRea, DF1.IndBenfPago, DF1.EntClasif,
                      DF1.Clasif, DF1.InterDistFacult, DF1.PorcIGV, DF1.PorcComInterRea,
                      DF1.PorcPrimaOtros --<N3039243> Javier La Chira 25-06-2012 / Se agrego campo prima otros
                 INTO DF.CodAcepRiesgo, DF.PorcCom, DF.PorcDep, DF.IntDep, DF.ImpRet,
                      DF.ImpRem, DF.DiaPago, DF.NumModGenRea, DF.IndBenfPago, DF.EntClasif,
                      DF.Clasif, DF.InterDistFacult, DF.PorcIGV, DF.PorcComInterRea,
                      DF.PorcPrimaOtros --<N3039243> Javier La Chira 25-06-2012 / Se agrego campo prima otros
                 FROM DIST_FACULT DF1, DEC_TRANSPORTE_FLOTANTE TF
                WHERE DF1.IdePol      = nIdePolAux
                  AND DF1.IdePol      = TF.IdePol
                  AND DF1.IdeRec      = TF.IdeRec
                  AND TF.IndSisOrig   = 'W'
                  AND ROWNUM          = 1;
             END;

             DF.StsDistFacult := 'ACT';
             DF.AnoCesion := TO_CHAR(DF.FecIniVig, 'YYYY');
           --<I N3026686>Marco Geldres/12-01-2007/Unificar Correlativo de Cesi?n de Coaseguro Proyecto Reaseguros
            --DF.NumCesion := PR_GEN_REA_UTIL.Numero_de_Cesion;
             SELECT SQ_NUMERO_CESION_T.NEXTVAL
             INTO   DF.NumCesion
             FROM   SYS.DUAL;
           --<F N3026686>


             DF.CodUsuario:= USER;

             --calculo de monto neto
             nPorcFacult := (DF.SumaCedida * 100) / DF.SumaCedida;
             nPrimaDistReaFacult := ((nPrimaDistRea * nPorcFacult) / 100);
             DF.PrimaNetaCesion := nPrimaDistReaFacult - ((nPrimaDistReaFacult * NVL(DF.PorcPrimaOtros,0)) / 100) - ((nPrimaDistReaFacult * NVL(DF.PorcCom,0)) / 100); --<N3039243> Javier La Chira 25-06-2012 / Se agrego campo prima otros

             PR_GEN_REA_UTIL.Crear_Dist_Facult(DF);
           END IF;
           nExiste :=1;
        END LOOP;
      END IF;
    END IF;
  END CREA_DIST_FACUL;

--
/*-----------------------------------------------------------------------
      Nombre      :   CONSULTAR_CABECERA_PLANILLA
      Proposito   :   Consulta los datos correspondientes a la cabecera
                      del documento PDF de planilla generado por el sistema.
      Par?metros  :   p_nIdePol  Identificador de la Poliza
                      p_nNumCert N?mero de Certificado
                      p_nNumLiq  N?mero de Liquidaci?n
      Log de Cambios
        Fecha        Autor               Descripci?n
        09/08/2006   Jorge D?az          Creaci?n
        28/09/2006   Jorge D?az          Modificaci?n: Traer el nombre del asegurado en vez
                                         del nombre del contratante
        02/05/2011   Marlon Castro G.    Modificaci?n: <N3036968> Muestra al responsable de pago como cliente
                                         Referencia:   Reqm TRANS-00015
        07/02/2012   Jhonatan Rojas T.   Modificaci?n: <N N3038436> Muestra el nombre de Asegurado por cliente
                                         Referencia:   Reqm TRANS-00027
        23/02/2012   Jhonatan Rojas T.   Modificaci?n: <N N3038436> Nuevo parametro de entrada IdeDec
                                         Referencia:   Reqm TRANS-00027
        21/03/2012   Jhonatan Rojas T.   Modificaci?n: <N 3038617> Modificaci?n en nombre de Asegurado
                                         Referencia:   Reqm TRANS-00027
        21/09/2020   Jhonatan Ninanya    <RTC-243489> Se agrego el codigo de producto de la poliza, codigo SBS y monto comision del intermediario
   -------------------------------------------------------------------------*/

  FUNCTION CONSULTAR_CABECERA_PLANILLA (
               p_nIdePol POLIZA.IDEPOL%TYPE,
               p_nNumCert  DEC_TRANSPORTE_FLOTANTE.Numcert%TYPE,
               p_nNumLiq   FACTURA.NUMFACT%TYPE, --<N3036968> Marlon Castro G. - 02/05/2011 - Muestra al responsable de pago como cliente
               -- <I N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Nuevo parametro de entrada IdeDec
               p_nIdeDec DEC_TRANSPORTE_FLOTANTE.Idedec%TYPE DEFAULT NULL
              -- <F N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Nuevo parametro de entrada IdeDec
          ) RETURN PR_API_EMI_TRAN.c_Cursor IS

    c_Cabecera PR_API_EMI_TRAN.c_Cursor;
    nCodCliente TERCERO.Numid%TYPE;
    cNomCliente varchar(400);
    -- <I N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente
    cNombreAseg VARCHAR(250);
    nNumIdeAsegWeb DEC_TRANSPORTE_FLOTANTE.Numidasegweb%TYPE;
    -- <F N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente
    -- <I RTC-243489> Jhonatan Ninanya / 21-09-2020
    nMtoComIntermediario RECIBO.MTOCOMMONEDA%TYPE;
    nOperacion RECIBO.IDEOP%TYPE;
    -- <F RTC-243489> Jhonatan Ninanya / 21-09-2020
  BEGIN
    BEGIN

    --<I N3036968> Marlon Castro G. - 02/05/2011 - Muestra al responsable de pago como cliente
    IF  p_nIdeDec IS NOT NULL THEN
    -- <I RTC-243489> Jhonatan Ninanya / 21-09-2020
    /*BEGIN
     -- <I N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

     SELECT dtf.numidasegweb INTO nNumIdeAsegWeb
                FROM DEC_TRANSPORTE_FLOTANTE dtf
                     WHERE dtf.idedec = p_nIdeDec
                                  AND ROWNUM=1;
    cNombreAseg:=PR_API_EMI_TRAN.BUSCA_DIR_TELF_DIST_RUC_ASEG('A',nNumIdeAsegWeb);
     EXCEPTION WHEN OTHERS THEN
               cNombreAseg:=NULL;
     END;
     -- <F N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

     BEGIN
       -- <I N3038617> Jhonatan Rojas Terrones / 21-03-2012 / Traer codigo del Asegurado
       \*  SELECT  t.numId INTO nCodCliente
           FROM  TERCERO t, FACTURA f
          WHERE  f.NumFact = p_nNumLiq
            AND  t.TipoId  = f.TipoId
            AND  t.NumId   = f.NumId
            AND  t.DvId    = f.DvId; *\
           nCodCliente:=PR_API_EMI_TRAN.BUSCA_ASEGURADO_PLANILLA(p_nIdePol,p_nNumCert,'C');
       -- <F N3038617> Jhonatan Rojas Terrones / 21-03-2012 / Traer codigo del Asegurado
     EXCEPTION WHEN OTHERS THEN
        nCodCliente := ' ';
     END;

     --cNomCliente := PR_API_EMI_TRAN.contratante(p_nNumLiq);

   -- <I N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente
    --cNomCliente :=  NVL(cNombreAseg, PR_API_EMI_TRAN.BUSCA_ASEG_RESPAGO_CONTR('A',p_nIdePol));
   -- <I N3038617> Jhonatan Rojas Terrones / 21-03-2012 / Traer nombre del asegurado en vez del nombre del cliente
       cNomCliente :=  NVL(cNombreAseg, PR_API_EMI_TRAN.BUSCA_ASEGURADO_PLANILLA(p_nIdePol,p_nNumCert,'A'));
   -- <F N3038617> Jhonatan Rojas Terrones / 21-03-2012 / Traer nombre del asegurado en vez del nombre del cliente
   -- <F N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

    ELSE
    */
      BEGIN
      
        SELECT R.Ideop
          INTO nOperacion
          FROM RECIBO R, dec_transporte_flotante D
         WHERE R.Iderec = D.Iderec
           AND D.Idedec = p_nIdeDec;
      
        SELECT SUM(R.Mtocommoneda)
          INTO nMtoComIntermediario
          FROM RECIBO R
         WHERE R.Ideop = nOperacion;
         
      EXCEPTION
        WHEN OTHERS THEN
          nMtoComIntermediario := NULL;
      END;
    END IF;
    -- <F RTC-243489> Jhonatan Ninanya / 21-09-2020

  -- <I N3026001> Jorge D?az / 28-09-2006 / Traer nombre del asegurado en vez del nombre del cliente
      SELECT TER.NumId,
        -- <I N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente
        -- <I N3038583> Jhonatan Rojas Terrones / 13-03-2012 / Se descomenta codigo
             DECODE( TER.TipoTer,
                    'P',
                     LTRIM(RTRIM(TER.ApeTer)) || ' ' || LTRIM(RTRIM(TER.ApeMatter)) || ' '|| LTRIM(RTRIM(TER.NomTer)),
                     LTRIM(RTRIM(TER.NomTer)))

                    cNomCliente
        -- <F N3038583> Jhonatan Rojas Terrones / 13-03-2012
        -- <F N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

      INTO   nCodCliente, cNomCliente
      FROM
             CERTIFICADO CERT,
             TERCERO TER,
             CLIENTE CLI
      WHERE  CERT.idepol = p_nIdePol
             AND CERT.codCli = CLI.codCli
             AND TER.numId = CLI.numId
             AND CERT.NumCert = p_nNumCert;
  -- <F N3026001>
     --END IF; <RTC-243489> Jhonatan Ninanya / 21-09-2020
    --<F N3036968> Marlon Castro G. - 02/05/2011 - Muestra al responsable de pago como cliente

      OPEN c_Cabecera FOR

        SELECT T2.NumId as codIntermediario,
               DECODE( T2.TipoTer,
                       'P',
                       LTRIM(RTRIM(T2.ApeTer)) || ' ' || LTRIM(RTRIM(T2.ApeMatter)) || ' ' || LTRIM(RTRIM(T2.NomTer)),
                       LTRIM(RTRIM(T2.NomTer))) as nomIntermediario,
  -- <I N3026001> Jorge D?az / 28-09-2006 / Traer nombre del asegurado en vez del nombre del cliente
               nCodCliente as codCliente,
               cNomCliente as nomCliente,
  -- <F N3026001>
               POL.CodMoneda as CodMoneda,
               MON.Descrip as DescMoneda
               -- <I RTC-243489> Jhonatan Ninanya / 21-09-2020
               ,POL.Codprod as Codigo_Producto
               ,I.Codsuperint as CodSBS_Intermediario
               ,nMtoComIntermediario as Monto_Comision_Int
               -- <F RTC-243489> Jhonatan Ninanya / 21-09-2020
        FROM   INTERMEDIARIO I,
               PART_INTER_POL P,
               TERCERO T2,
               POLIZA POL,
               LVAL MON
        WHERE  POL.IdePol = p.IdePol
          AND  I.CodInter = P.CodInter
          AND  T2.NumId = I.NumId
          AND  POL.CodMoneda = MON.CodLVal
          AND  P.IndLider = 'S'
          AND  POL.IdePol = p_nIdePol
          AND  MON.TipoLVal = 'TIPOMON';

    EXCEPTION
      WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20100, 'No se encontraron datos de cabecera para el nIdePol: '||p_nIdePol|| ' Error: '||SQLERRM);
    END;

  RETURN (c_Cabecera);

  END CONSULTAR_CABECERA_PLANILLA;

--<I N3026088>  Nelson D?az 10.10.2006 Emi-02511
/*-----------------------------------------------------------------------
  Nombre : INTERM_BLOQUEADO
  Proposito : Validar que el intermediario de la p?liza
              este o no bloqueado
  Par?metros:
    p_nIdePol              Numero de identificador de p?liza,
    p_cIndSoloValidaLider  Indicador de validar solo con el intermediario Lider
  Referencias : DBA-33. Reprogramaci?n.
  Log de Cambios
    Fecha        Autor          Descripcion
    10/10/2006   Nelson D?az    Creacion
-------------------------------------------------------------------------*/
FUNCTION INTERM_BLOQUEADO (p_nIdepol   poliza.Idepol%TYPE)
                          RETURN c_cursor IS
cIndBloq        VARCHAR2(1);
cvar            VARCHAR2(100);
c_Certificado   c_cursor;
cCodInterLider  INTERMEDIARIO.CodInter%TYPE;
cDescInter      VARCHAR2(50);

cWhere          VARCHAR2(2000);

BEGIN
  BEGIN
    cIndBloq := Pr_Bloqueo_Tercero.CHEQUEAR_BLOQUEO_INTER_POL(p_nIdepol,'N');
  EXCEPTION
    WHEN OTHERS THEN
      --Dbms_Output.put_line(cIndBloq);
      cCodInterLider := PR_INTERMEDIARIO.INTER_POL(p_nIdepol);
      cvar  := substr(' Validar Intermediario '||cCodInterLider||' '||SQLERRM,1,100);
      cIndBloq := '3';
  END;
  IF   cIndBloq = '0' THEN
    cvar  := ' Poliza sin intermediario bloquedo ';
  ELSIF  cIndBloq = '1' THEN
    cCodInterLider := PR_INTERMEDIARIO.INTER_POL(p_nIdepol);
    cDescInter     := SUBSTR(PR_INTERMEDIARIO.DESC_INTER(cCodInterLider),1,50);
    cvar           := ' Poliza con intermediario '||cCodInterLider||' '||cDescInter||' bloquedo';
  END IF;
  cWhere := 'SELECT '''||cIndBloq||''' , '''||cvar||'''  FROM dual';
    OPEN c_Certificado FOR cWhere;
    RETURN (c_Certificado);
END;
--<F N3026088>  Nelson D?az 10.10.2006 Emi-02511

--<I N3026367>  Jorge D?az 27.11.2006
/*-----------------------------------------------------------------------
Nombre      :   LISTAR_EQUIVALENCIAS
Proposito   :   Listar las tablas de equivalencias de valores correspondientes
          a pais de procedencia, pais de destino, mercaderia, embalaje y
          medio de transporte, de acuerdo a un c?digo de corredor asociado.
Par?metros  :   p_nCodCorredor  Codigo de Corredor del que se quiere consultar
Log de Cambios
Fecha        Autor          Descripci?n
27/11/2006   Jorge D?az     Creaci?n
-------------------------------------------------------------------------*/

FUNCTION LISTAR_EQUIVALENCIAS (
        p_nCodCorredor INTERMEDIARIO.CODINTER%TYPE
        ) RETURN PR_API_EMI_TRAN.c_Cursor IS

c_Equiv PR_API_EMI_TRAN.c_Cursor;

BEGIN
BEGIN

OPEN c_Equiv FOR

SELECT T.CODTAB, T.CODIGO, T.DESCRIP, T.CODRIMAC, T.DESRIMAC
FROM ACSELX.TAB001 T
WHERE T.CORREDOR = p_nCodCorredor
      AND T.CODTAB IN ('MERC', 'EMBA' , 'MTRA', 'NAVE');

EXCEPTION
WHEN OTHERS THEN
RAISE_APPLICATION_ERROR(-20100, 'No se encontraron tablas de equivalencia para el codigo de corredor: '||p_nCodCorredor|| ' Error: '||SQLERRM);
END;

RETURN (c_Equiv);

END LISTAR_EQUIVALENCIAS;

--<F N3026367>  Jorge D?az 27.11.2006

/*-----------------------------------------------------------------------
    Nombre     : BUSCA_ASEG_RESPAGO_CONTR
    Prop?sito  : Devuelve el nombre completo del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nIndicador Indicador de asegurado, responsable de pago o contratante
                 p_nValor    Identificador de p?liza
                      A   Devuelve Concatenado Nombre + Paterno + Materno
                          del asegurado , responsable de pago o contratante
                          de acuerdo al indicador
    Log de Cambios
       Fecha            Autor               Descripcion
       07/02/2012  Jhonatan Rojas T.        <N3038436> Creaci?n
       10/09/2012  Italo Rivas              <N3039615> EMI-14941 / Modificaci?n
-------------------------------------------------------------------------*/
   FUNCTION BUSCA_ASEG_RESPAGO_CONTR(p_nIndicador VARCHAR2,p_nValor NUMBER,
                  p_numCert NUMBER default null) RETURN VARCHAR2 IS --<N3039615> EMI-14941 / Italo Rivas / 10-09-2012
     cResult  VARCHAR2(500);
     cCodCli VARCHAR2(50);
     cNumId VARCHAR2(50);

   BEGIN

   /************************************************/
   /** OBTENEMOS EL NOMBRE COMPLETO DEL ASEGURADO **/
   /************************************************/

   IF p_nIndicador='A' THEN

     -- 1ER PASO : OBTENEMOS EL CODIGO DEL CLIENTE
   SELECT C.CODCLI INTO cCodCli
          FROM CERTIFICADO C
           --<I N3039615> EMI-14941 / Italo Rivas / 10-09-2012
               WHERE C.IDEPOL = p_nValor and C.NUMCERT = p_numCert;
                      --AND ROWNUM=1;
           --<F N3039615> EMI-14941 / Italo Rivas / 10-09-2012
   -- 2DO PASO : OBTENEMOS EL NUMERO DE ID DEL CLIENTE
   SELECT CLI.NUMID INTO cNumId
           FROM CLIENTE CLI
                WHERE CLI.CODCLI = cCodCli;
                      --AND ROWNUM=1; --<N3039615> EMI-14941 / Italo Rivas / 10-09-2012
   -- 3ER PASO : OBTENEMOS EL NOMBRE COMPLETO DEL ASEGURADO
   SELECT  DECODE (TipoTer, 'P',LTRIM(RTRIM(t.ApeTer)) || ' ' || LTRIM(RTRIM(t.ApeMatter))
                || ' '|| LTRIM(RTRIM(t.NomTer)), LTRIM(RTRIM(t.NomTer))) INTO cResult
                         FROM TERCERO T
                         --<I N3039615> EMI-14941 / Italo Rivas / 10-09-2012
                             WHERE T.NUMID = cNumId;
                                   --AND ROWNUM=1;
                         --<F N3039615> EMI-14941 / Italo Rivas / 10-09-2012

   /*********************************************************/
  /** OBTENEMOS EL NOMBRE COMPLETO DEL RESPONSABLE DE PAGO **/
  /*********************************************************/

   ELSIF p_nIndicador='RP' THEN

  -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE LA FACTURA
    SELECT FACT.NUMID INTO cNumId
                      FROM FACTURA FACT
                           WHERE  FACT.NUMFACT=p_nValor
                                       AND ROWNUM=1;
     -- 2DO PASO : OBTENEMOS EL NOMBRE COMPLETO DEL ASEGURADO
    SELECT  DECODE (TipoTer, 'P',LTRIM(RTRIM(t.ApeTer)) || ' ' || LTRIM(RTRIM(t.ApeMatter))
                  || ' '|| LTRIM(RTRIM(t.NomTer)), LTRIM(RTRIM(t.NomTer))) INTO cResult
                           FROM TERCERO T
                               WHERE T.NUMID = cNumId
                                     AND ROWNUM=1;

   /**************************************************/
   /** OBTENEMOS EL NOMBRE COMPLETO DEL CONTRATANTE **/
   /*************************************************/
   ELSIF p_nIndicador='C' THEN

   -- 1ER PASO : OBTENEMOS EL CODIGO DEL CLIENTE
   SELECT PCC.CODCLI INTO cCodCli
          FROM POLIZA_CERT_CLIENTE PCC
               WHERE PCC.IDEPOL = p_nValor
                     AND ROWNUM=1;
   -- 2DO PASO : OBTENEMOS EL NUMERO DE ID DEL CLIENTE
   SELECT CLI.NUMID INTO cNumId
           FROM CLIENTE CLI
                WHERE CLI.CODCLI = cCodCli
                      AND ROWNUM=1;
   -- 3ER PASO : OBTENEMOS EL NOMBRE COMPLETO DEL ASEGURADO
   SELECT  DECODE (TipoTer, 'P',LTRIM(RTRIM(t.ApeTer)) || ' ' || LTRIM(RTRIM(t.ApeMatter))
                || ' '|| LTRIM(RTRIM(t.NomTer)), LTRIM(RTRIM(t.NomTer))) INTO cResult
                         FROM TERCERO T
                             WHERE T.NUMID = cNumId
                                   AND ROWNUM=1;
   END IF;

   RETURN(cResult);
   EXCEPTION
   WHEN OTHERS THEN
        cResult:=' ';
   RAISE_APPLICATION_ERROR(-20100, 'No se encontraron nombres '||SQLERRM);
 END BUSCA_ASEG_RESPAGO_CONTR;

 /*-----------------------------------------------------------------------
    Nombre     : BUSCA_DIR_TELF_DIST_RUC_ASEG
    Prop?sito  : Devuelve los datos del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nIndicador Indicador de asegurado, responsable de pago o contratante
                 p_nValor    Identificador de p?liza
                      A   Devuelve Direccion, Distrito, Telefono y RUC
                          del asegurado
    Log de Cambios
       Fecha            Autor               Descripcion
       07/02/2012  Jhonatan Rojas T.        <N3038436> Creaci?n
       23/02/2012  Jhonatan Rojas T.        <N3038436> Modificaci?n
-------------------------------------------------------------------------*/
   FUNCTION BUSCA_DIR_TELF_DIST_RUC_ASEG(p_nIndicador VARCHAR2,p_nValor NUMBER) RETURN VARCHAR2 IS
     cResult  VARCHAR2(500);
   BEGIN

   /******************************************************/
   /** OBTENEMOS EL NOMBRE DE LA DIRECCION DEL ASEGURADO **/
   /****************************************************/

   IF p_nIndicador='D' THEN

     -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
   SELECT DECODE(T.INDDIRDESP, 'D', T.DIREC, 'T', T.DIRECTRAB) INTO cResult
           FROM TERCERO T
                 WHERE NUMID = p_nValor
                       AND ROWNUM=1;

  /*************************************************/
   /** OBTENEMOS EL NOMBRE DEL RESPONSABLE DE PAGO **/
   /***********************************************/
  -- <I N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

  --  ELSIF p_nIndicador='RP' THEN
   -- <I N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

     ELSIF p_nIndicador='A' THEN

   SELECT  DECODE (TipoTer, 'P',LTRIM(RTRIM(t.ApeTer)) || ' ' || LTRIM(RTRIM(t.ApeMatter))
                || ' '|| LTRIM(RTRIM(t.NomTer)), LTRIM(RTRIM(t.NomTer))) INTO cResult
                         FROM TERCERO T
                             WHERE T.NUMID = p_nValor
                                   AND ROWNUM=1;
 -- <I N3038436> Jhonatan Rojas Terrones / 23-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

  -- <F N3038436> Jhonatan Rojas Terrones / 17-02-2012 / Traer nombre del asegurado en vez del nombre del cliente

  /********************************************/
  /** OBTENEMOS EL # DE TELEFONO DEL ASEGURADO **/
  /********************************************/

   ELSIF p_nIndicador='T' THEN

  -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
     SELECT T.TELEF1 INTO cResult
            FROM TERCERO T
                 WHERE NUMID = p_nValor
                       AND ROWNUM=1;

   /***************************************************/
   /** OBTENEMOS EL NOMBRE DEL DISTRITO DEL ASEGURADO **/
   /**************************************************/
   ELSIF p_nIndicador='DIST' THEN

     -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
  SELECT M.DescMunicipio INTO cResult
      FROM MUNICIPIO M , TERCERO TER
     WHERE
                M.CodPais      = DECODE(TER.IndDirDesp,'D',TER.CodPais,'T', TER.CodPaisTrab)
         AND M.CodEstado    = DECODE(TER.IndDirDesp,'D',TER.CodEstado,'T', TER.CodEstadoTrab)
         AND M.CodCiudad    = DECODE(TER.IndDirDesp,'D',TER.CodCiudad,'T', TER.CodCiudadTrab)
         AND M.CodMunicipio = DECODE(TER.IndDirDesp,'D',TER.CodMunicipio,'T', TER.CodMunicipioTrab)
         AND TER.NUMID = p_nValor
         AND ROWNUM=1;

   /*****************************************/
   /** OBTENEMOS EL # DE RUC DEL ASEGURADO **/
   /***************************************/

  ELSIF p_nIndicador='R'   THEN

    -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
      SELECT NumIdDoc INTO cResult
                  FROM DOCS_IDENTIDAD
                       WHERE NUMID = p_nValor
                            AND ROWNUM=1;
   END IF;

  RETURN(cResult);
   EXCEPTION
   WHEN OTHERS THEN
        cResult:=' ';
   RAISE_APPLICATION_ERROR(-20100, 'No se encontraron datos del asegurado '||SQLERRM);
 END BUSCA_DIR_TELF_DIST_RUC_ASEG;


 /*-----------------------------------------------------------------------
    Nombre     : GET_DATOS_ASEGURADO
    Prop?sito  : Devuelve los datos del asegurado
    Referencia : Transportes WEB
    Par?metros :
                 p_nIndicador Indicador de asegurado, responsable de pago o contratante
                 p_nValor    Identificador de p?liza
                      A   Devuelve Direccion, Distrito, Telefono y RUC
                          del asegurado
    Log de Cambios
       Fecha            Autor               Descripcion
       13/02/2012  Jhonatan Rojas T.        <N3038436> Creaci?n
-------------------------------------------------------------------------*/

   FUNCTION GET_DETALLE_DATOS_ASEGURADO(p_nIndicador VARCHAR2,p_nValor NUMBER) RETURN VARCHAR2 IS
     cResult  VARCHAR2(500);
     cCodCli VARCHAR2(50);
     cNumId VARCHAR2(50);
   BEGIN

   /***********************************/
   /** OBTENEMOS DATOS DEL ASEGURADO **/
   /**********************************/

   IF p_nValor IS NOT NULL THEN
   -- 1ER PASO : OBTENEMOS EL CODIGO DEL CLIENTE
   SELECT C.CODCLI INTO cCodCli
          FROM CERTIFICADO C
               WHERE C.IDEPOL = p_nValor
                     AND ROWNUM=1;
   -- 2DO PASO : OBTENEMOS EL NUMERO DE ID DEL CLIENTE
   SELECT CLI.NUMID INTO cNumId
           FROM CLIENTE CLI
                WHERE CLI.CODCLI = cCodCli
                      AND ROWNUM=1;
   END IF;

   /******************************************************/
   /** OBTENEMOS EL NOMBRE DE LA DIRECCION DEL ASEGURADO **/
   /****************************************************/

   IF p_nIndicador='D' THEN

   -- 1ER PASO : OBTENEMOS EL NOMBRE DE LA DIRECCION DEL ASEGURADO
    SELECT DECODE(T.INDDIRDESP, 'D', T.DIREC, 'T', T.DIRECTRAB) INTO cResult
           FROM TERCERO T
                 WHERE NUMID = cNumId
                       AND ROWNUM=1;

  /********************************************/
  /** OBTENEMOS EL # DE TELEFONO DEL ASEGURADO **/
  /********************************************/

   ELSIF p_nIndicador='T' THEN

  -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
     SELECT T.TELEF1 INTO cResult
            FROM TERCERO T
                 WHERE NUMID = cNumId
                       AND ROWNUM=1;

   /***************************************************/
   /** OBTENEMOS EL NOMBRE DEL DISTRITO DEL ASEGURADO **/
   /**************************************************/
   ELSIF p_nIndicador='DIST' THEN

   -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
  SELECT M.DescMunicipio INTO cResult
      FROM MUNICIPIO M , TERCERO TER
     WHERE
                M.CodPais      = DECODE(TER.IndDirDesp,'D',TER.CodPais,'T', TER.CodPaisTrab)
         AND M.CodEstado    = DECODE(TER.IndDirDesp,'D',TER.CodEstado,'T', TER.CodEstadoTrab)
         AND M.CodCiudad    = DECODE(TER.IndDirDesp,'D',TER.CodCiudad,'T', TER.CodCiudadTrab)
         AND M.CodMunicipio = DECODE(TER.IndDirDesp,'D',TER.CodMunicipio,'T', TER.CodMunicipioTrab)
         AND TER.NUMID=cNumId
         AND ROWNUM=1;

   /*****************************************/
   /** OBTENEMOS EL # DE RUC DEL ASEGURADO **/
   /***************************************/

  ELSIF p_nIndicador='R'   THEN

   -- 1ER PASO : OBTENEMOS EL NUMERO DE ID DE TERCERO
      SELECT NumIdDoc INTO cResult
                  FROM DOCS_IDENTIDAD
                       WHERE NUMID = cNumId
                            AND ROWNUM=1;
   END IF;

   RETURN(cResult);
   EXCEPTION
   WHEN OTHERS THEN
        cResult:=' ';
   RAISE_APPLICATION_ERROR(-20100, 'No se encontraron datos del asegurado '||SQLERRM);
 END GET_DETALLE_DATOS_ASEGURADO;


 /*-----------------------------------------------------------------------
    Nombre     : BUSCA_ASEGURADO_PLANILLA
    Prop?sito  : Devuelve el nombre completo del asegurado
    Referencia : Transportes WEB
    Par?metros :
                p_nValor    Identificador de p?liza
                      A   Devuelve Concatenado Nombre + Paterno + Materno
                          del asegurado , responsable de pago o contratante
                          de acuerdo al indicador
               p_nNumCert Numero de Certificado
    Log de Cambios
       Fecha            Autor               Descripcion
       12/03/2012  Jhonatan Rojas T.        <N N3038617> Creaci?n
-------------------------------------------------------------------------*/
   FUNCTION BUSCA_ASEGURADO_PLANILLA(p_nValor NUMBER, p_nNumCert NUMBER, p_indicador VARCHAR2) RETURN VARCHAR2 IS
     cResult  VARCHAR2(500);
     cCodCli VARCHAR2(50);
     cNumId VARCHAR2(50);

   BEGIN

   /*************************************************************/
   /** OBTENEMOS EL NOMBRE COMPLETO DEL ASEGURADO  PARA PLANILLA**/
   /************************************************************/

     -- 1ER PASO : OBTENEMOS EL CODIGO DEL CLIENTE
   SELECT C.CODCLI INTO cCodCli
          FROM CERTIFICADO C
               WHERE C.IDEPOL = p_nValor
                           AND C.NUMCERT = NVL(p_nNumCert,C.NUMCERT);
   -- 2DO PASO : OBTENEMOS EL NUMERO DE ID DEL CLIENTE
   SELECT CLI.NUMID INTO cNumId
           FROM CLIENTE CLI
                WHERE CLI.CODCLI = cCodCli;
   -- 3ER PASO : OBTENEMOS EL NOMBRE COMPLETO DEL ASEGURADO
   SELECT  DECODE (TipoTer, 'P',LTRIM(RTRIM(t.ApeTer)) || ' ' || LTRIM(RTRIM(t.ApeMatter))
                || ' '|| LTRIM(RTRIM(t.NomTer)), LTRIM(RTRIM(t.NomTer))) INTO cResult
                         FROM TERCERO T
                             WHERE T.NUMID = cNumId;
    IF p_indicador = 'A' THEN
     RETURN(cResult);
    ELSIF p_indicador='C' THEN
    RETURN (cNumId);
    END IF;

   EXCEPTION
   WHEN OTHERS THEN
        cResult:=' ';
   RAISE_APPLICATION_ERROR(-20100, 'No se encontraron nombre de Asegurado '||SQLERRM);
 END BUSCA_ASEGURADO_PLANILLA;

 /*-----------------------------------------------------------------------
    Nombre     : VALIDA_DECLARACION_FACULTATIVA
    Prop?sito  : Valida si la poliza es facultativa, si lo es, entonces valida
                 que tenga al menos 1 declaracion activa.
                 Si no tiene, devuelve mensaje
    Referencia : CNT-01590
    Par?metros :
         p_nIdePol        Identificador de p?liza
         p_nNumCert       Numero de Certificado

    Log de Cambios
       Fecha            Autor               Descripcion
       12/10/2012       Renzo Yacarini      <N3039819> Creaci?n
-------------------------------------------------------------------------*/
 FUNCTION VALIDA_DECLARACION_FACULTATIVA(
   p_nIdePol      POLIZA.IDEPOL%TYPE,
   p_nNumcert     CERTIFICADO.NUMCERT%TYPE,
   p_cNumPla      DEC_TRANSPORTE_FLOTANTE.PLANILLA%TYPE
 ) RETURN VARCHAR2 IS

  cEsFacultativa VARCHAR2(1);
  cantDecActivas NUMBER;
  nNumPol        POLIZA.IDEPOL%TYPE;
  bContinuar     BOOLEAN;
  cMensaje       RIM_TAB_GRAL.DESCADIC%TYPE;

 BEGIN
     -- Se valida si la poliza es facultativa
     BEGIN
       SELECT NVL(PO.INDFACULT,'N'), numpol
       INTO cEsFacultativa, nNumPol
       FROM POLIZA PO
       WHERE PO.IDEPOL = p_nIdePol;
     EXCEPTION
       WHEN OTHERS THEN
         cEsFacultativa := 'N';
     END;

     -- si la poliza es facultativa, se debe validar si es la primera declaracion activa
     IF cEsFacultativa = 'S' THEN

       IF p_nNumcert IS NOT NULL THEN

         BEGIN
           SELECT COUNT(1)
           INTO cantDecActivas
           FROM DEC_TRANSPORTE_FLOTANTE DT
           WHERE DT.IDEPOL = p_nIdePol
             AND DT.NUMCERT = p_nNumCert
             AND DT.STSDEC = 'ACT';
         EXCEPTION
           WHEN OTHERS THEN
             cantDecActivas := 0;
         END;

         -- si no tiene declaraciones activas, se solicita que la primera debe estar activada por Acselx.
         IF cantDecActivas = 0 THEN

           BEGIN
             SELECT REPLACE(R.DESCADIC,'_NUMPOL_',nNumPol)
             INTO cMensaje
             FROM RIM_TAB_GRAL R
             WHERE R.CODTAB = 'MENSAJETRANFACUL'
               AND R.LLAVETAB = '0001'
               AND R.STSTAB = 'ACT';
           EXCEPTION
             WHEN OTHERS THEN
               cMensaje := 'XXX';
           END;

         END IF;

       ELSIF p_cNumPla IS NOT NULL THEN

         BEGIN
           SELECT COUNT(1)
           INTO cantDecActivas
            FROM (
            SELECT NUMCERT,
                   (SELECT COUNT(1)
                    FROM DEC_TRANSPORTE_FLOTANTE F
                    WHERE F.IDEPOL = C.IDEPOL
                      AND F.NUMCERT = C.NUMCERT
                      AND F.STSDEC = 'ACT') CANTIDAD
            FROM poliza p,
                 certificado c
            WHERE p.idepol = p_nIdePol
              AND c.idepol = p.idepol
              AND EXISTS (SELECT 1
                          FROM DEC_TRANSPORTE_FLOTANTE T
                          WHERE T.IDEPOL = P.IDEPOL
                            AND T.NUMCERT = C.NUMCERT
                            AND T.PLANILLA = p_cNumPla
                            AND T.STSDEC = 'VAL')) A
            WHERE CANTIDAD = 0;
         EXCEPTION
           WHEN OTHERS THEN
             cantDecActivas := 0;

         END;

         -- si no tiene declaraciones activas, se solicita que la primera debe estar activada por Acselx.
         IF cantDecActivas > 0 THEN

           BEGIN
             SELECT REPLACE(R.DESCADIC,'_NUMPOL_',nNumPol)
             INTO cMensaje
             FROM RIM_TAB_GRAL R
             WHERE R.CODTAB = 'MENSAJETRANFACUL'
               AND R.LLAVETAB = '0002'
               AND R.STSTAB = 'ACT';
           EXCEPTION
             WHEN OTHERS THEN
               cMensaje := 'XXX';
           END;

         END IF;

       END IF;

     END IF;

   RETURN cMensaje;
 END;

 -- <I N3040337> WRK-01213 / Christian R. / 17-01-2013
 /*---------------------------------------------------------------------------
  Nombre : OBTENER_DATOS_TRAMA
  Proposito : Obtiene los datos para armar la trama xml de generaci?n de Nro tr?mite
  Parametros :

  Log de Cambios
      Fecha          Autor                  Descripcion
  17/01/2013         Christian R.           <N3040337> Creaci?n
  ----------------------------------------------------------------------------*/
  FUNCTION OBTENER_DATOS_TRAMA(
    idePol          DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
    numCert         DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPE,
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
  ) RETURN c_Cursor IS
    cCursor        APP_EMISION.PR_EMI_VIDA_LEY.V_CURSOR;

    dirdespacho    TERCERO.DIREC%TYPE;
    numidcont      TERCERO.NUMID%TYPE;
    numiddoccont   DOCS_IDENTIDAD.NUMIDDOC%TYPE;
    tipoiddoc      TERCERO.TIPOTER%TYPE;
    tipotercont    TERCERO.TIPOTER%TYPE;
    nomcont        TERCERO.NOMTER%TYPE;
    codpol         EMI_CABLOGTRABAJADOR.TIPOPROD%TYPE;
    codprod        EMI_CABLOGTRABAJADOR.TIPOPROD%TYPE;
    vApepatcont     TERCERO.APETER%TYPE;
    vApematcont     TERCERO.APEMATTER%TYPE;
    vCodCli         DEC_TRANSPORTE_FLOTANTE.Codcli%type;
    vCodProveedor   DEC_TRANSPORTE_FLOTANTE.CODPROVEEDOR%TYPE;
    videDec         DEC_TRANSPORTE_FLOTANTE.IDEDEC%TYPE;
    videPol         DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE;
    vnumCert        DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPE;
    vnumPol        POLIZA.NUMPOL%TYPE;

  BEGIN

     videDec:=ideDec;
     videPol:=idePol;
     vnumCert:=numCert;

     SELECT p.CODPROD,p.codcli ,p.numpol
     into codprod,vCodCli,vnumPol
     FROM POLIZA p WHERE p.IDEPOL=videPol;

     select distinct ter.tipoter,d.numiddoc,TER.NOMTER,TER.APETER,TER.APEMATTER, TER.Directrab, TER.TIPOTER
     INTO tipoiddoc,numiddoccont,nomcont,vApepatcont,vApematcont,dirdespacho, tipotercont
     FROM TERCERO TER, DOCS_IDENTIDAD D
     WHERE TER.TIPOID = D.TIPOID
           AND TER.NUMID = D.NUMID
           AND TER.DVID = D.DVID
           AND TER.NUMID=vCodCli
       AND D.TIPOIDDOC=(SELECT MIN(DI.TIPOIDDOC) FROM DOCS_IDENTIDAD DI WHERE DI.NUMID=D.NUMID AND D.DVID=DI.DVID AND D.TIPOID=DI.TIPOID); --<N3040523> HD 114578/ Christian R. / 18-02-2013

     codpol:=codprod;
     numidcont:=vCodCli;

     OPEN cCursor FOR
     SELECT optipo "OPTIPO", nrocolocacion "NROCOLOCACION", dirdespacho "DIRDESPACHO", numidcont "NUMIDCONT", numiddoccont "NUMIDDOCCONT", tipoiddoc "TIPOIDDOC", tipotercont "TIPOTERCONT", numidbroker "NUMIDBROKER",
            vApepatcont "APECONT", vApematcont "APEMATCONT", nomcont "NOMCONT", apebroker "APEBROKER", apematbroker "APEMATBROKER", nombroker "NOMBROKER", tipoterbroker "TIPOTERBROKER", numidgestor "NUMIDGESTOR", --<N3039681> RSALUD-17651/ Christian R. / 24-09-2012
            idePol "IDEPOL", codpol "CODPOL", vnumPol "NUMPOL", codprod "CODPROD", nomprod "NOMPROD", totcert "TOTCERT", indprov "INDPROV", indinsp "INDINSP", indanexos "INDANEXOS", arearecepcion "AREARECEPCION", autor "AUTOR"
     FROM DUAL;

    RETURN cCursor;

  EXCEPTION
   WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20100, 'Error en PR_API_EMI_TRAN.OBTENER_DATOS_TRAMA: '||SQLERRM);

  END OBTENER_DATOS_TRAMA;
  /*-----------------------------------------------------------------------
    Nombre : ACTUALIZAR_TRAMITE
  Proposito : Obtiene los datos para armar la trama xml de generaci?n de Nro tr?mite
  Parametros :

  Log de Cambios
      Fecha          Autor                  Descripcion
  17/01/2013         Christian R.           <N3040337> Creaci?n
   -------------------------------------------------------------------------*/
   FUNCTION ACTUALIZAR_TRAMITE(P_ideDec          DEC_TRANSPORTE_FLOTANTE.IDEDEC%TYPE,
    p_NumCert         DEC_TRANSPORTE_FLOTANTE.NUMCERT%TYPE,
    p_idePol          DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
    p_NumTramite      DEC_TRANSPORTE_FLOTANTE.NUMTRAMITE%TYPE) RETURN VARCHAR2 IS
     resultado  VARCHAR2(1);
   BEGIN
     --
     BEGIN
        UPDATE DEC_TRANSPORTE_FLOTANTE
        SET NUMTRAMITE=p_NumTramite
        WHERE NUMCERT=p_NumCert AND IDEDEC=P_ideDec AND IDEPOL=p_idePol;
        resultado := 0;
       EXCEPTION
          WHEN OTHERS THEN
               resultado := 1;
     END;
     --
     RETURN(resultado);
   END ACTUALIZAR_TRAMITE;

/*---------------------------------------------------------------------------
  Nombre : OBTENER_DATOS_LIQUIDACION
  Proposito : Obtiene los datos para generar tramites a partir de una liquidacion
  Parametros :

  Log de Cambios
      Fecha          Autor                  Descripcion
  24/01/2013         Christian R.           <N3040337> Creaci?n
  ----------------------------------------------------------------------------*/
  FUNCTION OBTENER_DATOS_LIQUIDACION(
    p_idePol          DEC_TRANSPORTE_FLOTANTE.IDEPOL%TYPE,
    p_planilla         DEC_TRANSPORTE_FLOTANTE.PLANILLA%TYPE
  ) RETURN c_Cursor IS
    cCursor        APP_EMISION.PR_EMI_VIDA_LEY.V_CURSOR;

  BEGIN
     OPEN cCursor FOR
     select t.IDEDEC,T.NUMCERT,T.IDEPOL
     from dec_transporte_flotante t
     where idepol=p_idePol AND PLANILLA=p_planilla AND STSDEC='VAL';

    RETURN cCursor;

  EXCEPTION
   WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20100, 'Error en PR_API_EMI_TRAN.OBTENER_DATOS_LIQUIDACION: '||SQLERRM);

  END OBTENER_DATOS_LIQUIDACION;

-- <F N3040337> WRK-01213 / Christian R. / 17-01-2013

/*-----------------------------------------------------------------------
    Nombre     : OBTENER_DERECHO_EMISION
    Proposito  : Devuelve el derecho de emision de la poliza
    Referencia : Transportes WEB
    Parametros : p_nIdeDec  Identificaci?n de aplicaci?n
    Log de Cambios
           Fecha            Autor            Descripcion
           02/04/2013    Jhonatan Rojas T.   <N3040849> Creaci?n
   ------------------------------------------------------------------------*/
   FUNCTION OBTENER_DERECHO_EMISION(p_nIdeDec  NUMBER ) RETURN NUMBER IS
   --
   c_Certificado c_Cursor;
   --
   nMonto           DET_ACRE.MtoDetAcreMoneda%TYPE;
   nDesCtoAcre      CPTO_ACRE.DESCCPTOACRE%TYPE:='GASTOS DE EMISION';
   --
   BEGIN
     --
     BEGIN
        SELECT SUM(DA.MtoDetAcreMoneda) Importe
        INTO nMonto
         FROM FACTURA FA,
              ACREENCIA AC,
              DET_ACRE DA,
              CPTO_ACRE CA
        WHERE FA.NumOper      = (SELECT r.ideop
                                        FROM Dec_Transporte_Flotante F, recibo R
                                 WHERE F.IdeRec = R.IdeRec
                                 AND F.IdeDec = p_nIdeDec)
          AND AC.IdeFact      = FA.IdeFact
          AND DA.NumAcre      = AC.NumAcre
          AND CA.codgrupoacre = DA.codgrupoacre
          AND CA.CodCptoAcre  = DA.CodCptoAcre
          AND UPPER(CA.DescCptoAcre) = nDesCtoAcre;
     EXCEPTION
        WHEN OTHERS THEN
             nMonto := NULL;
     END;

     --
     RETURN(nMonto);
     --
   END OBTENER_DERECHO_EMISION;

   /*-----------------------------------------------------------------------
    Nombre     : OBTENER_DIRECCION_RESPAGO
    Prop?sito  : Devuelve la direcci?n completa del Respondable de Pago
    Referencia : RQ TRANS-00037
    Par?metros : p_nIndicador   Indicador de resultado
                 p_nNumLq       Numero de liquidaci?n

    Log de Cambios:
          Fecha            Autor            Descripcion
        25/11/2013      Italo Rivas      Creaci?n TRANS-00037 <N3041754>
   -------------------------------------------------------------------------*/
   FUNCTION OBTENER_DIRECCION_RESPAGO(p_nInd    VARCHAR2,
                                      p_nNumLq  NUMBER) RETURN VARCHAR2 IS
     cNumId      NUMBER(9);
     cIndDir     VARCHAR2(1);
     cDirResPa   VARCHAR2(100);

    BEGIN

       BEGIN
         --Obtenemos el NumId del Responsable de Pago
         SELECT F.NUMID INTO cNumId
         FROM FACTURA F
         WHERE F.NUMFACT = p_nNumLq;

       EXCEPTION
         WHEN OTHERS THEN
           cNumId := null;
       END;

       BEGIN
         --Obtenemos Indicador de Direccion
         SELECT T.INDDIRDESP
          INTO cIndDir
          FROM TERCERO T
          WHERE T.TIPOID = 0
            AND T.NUMID  = cNumId
            AND T.DVID   = 0;

       EXCEPTION
         WHEN OTHERS THEN
           cIndDir := null;
       END;

       IF p_nInd = 'DR' THEN
           BEGIN
             -- Direccion
             SELECT Substr(DECODE(T.INDDIRDESP,'D','D'||T.DIREC,'T'||T.DIRECTRAB),2,500)
             INTO cDirResPa
             FROM TERCERO T
             WHERE T.TIPOID = 0
               AND T.NUMID  = cNumId
               AND T.DVID   = 0;
           EXCEPTION
             WHEN OTHERS THEN
               cDirResPa := null;
           END;

       ELSIF p_nInd = 'DS' THEN
           BEGIN
              -- Distrito
              SELECT M.DESCMUNICIPIO
              INTO cDirResPa
              FROM TERCERO T, MUNICIPIO M
              WHERE T.TIPOID       = 0
                AND T.NUMID        = cNumId
                AND T.DVID         = 0
                AND M.CODPAIS      = decode(cIndDir,'D',T.CODPAIS,T.CODPAISTRAB)
                AND M.CODESTADO    = decode(cIndDir,'D',T.CODESTADO,T.CODESTADOTRAB)
                AND M.CODCIUDAD    = decode(cIndDir,'D',T.CODCIUDAD,T.CODCIUDADTRAB)
                AND M.CODMUNICIPIO = decode(cIndDir,'D',T.CODMUNICIPIO,T.CODMUNICIPIOTRAB);
            EXCEPTION
              WHEN OTHERS THEN
                cDirResPa := null;
            END;

       ELSIF p_nInd = 'PR' THEN
            BEGIN
              -- Provincia
              SELECT C.DESCCIUDAD
              INTO cDirResPa
              FROM TERCERO T, CIUDAD C
              WHERE T.TIPOID    = 0
                AND T.NUMID     = cNumId
                AND T.DVID      = 0
                AND C.CODPAIS   = decode(cIndDir,'D',T.CODPAIS,T.CODPAISTRAB)
                AND C.CODESTADO = decode(cIndDir,'D',T.CODESTADO,T.CODESTADOTRAB)
                AND C.CODCIUDAD = decode(cIndDir,'D',T.CODCIUDAD,T.CODCIUDADTRAB);
             EXCEPTION
               WHEN OTHERS THEN
                 cDirResPa := null;
             END;

       END IF;

       RETURN cDirResPa;

   END OBTENER_DIRECCION_RESPAGO;



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
  p_cTipo        IN VARCHAR2,
  p_cError       OUT VARCHAR2
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
  p_cTipo        IN VARCHAR2,
  p_cError       OUT VARCHAR2
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

END SP_ELIM_CLAU_CERT_C;
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
  p_cError       OUT VARCHAR2
) IS
  bExisteSelec BOOLEAN;
  n_IDLogLote  NUMBER;
  WSESION      NUMBER;
  cEmiPro      INTEGER;
  cEsMsv       INTEGER;
  nExiste      NUMBER := 0;
  nVal         NUMBER := 0;

  CURSOR C_IDTRANSF (p_CodPol POLIZA.CodProd%TYPE, p_NumPol POLIZA.NumPol%TYPE, p_WSesion NUMBER) IS 
    SELECT CL.ID_TRANSF
      FROM VNNT_CONTROL_LEGACY CL
     WHERE (CL.NUMCARGA) IN
           (SELECT NUMCARGA
              FROM DATOS_EXT_POLIZA
             WHERE (IDEPOL, NUMPOL, NUMCERT) IN
                   (SELECT P.IDEPOL, P.NUMPOL, C.NUMCERT
                      FROM POLIZA P, CERTIFICADO C
                     WHERE P.IDEPOL = C.IDEPOL
                       AND P.CODPROD = p_CodPol
                       AND P.NUMPOL = p_NumPol
                       AND C.NumCert IN (SELECT NumCert FROM TMP_CERTIF WHERE IdePol = P.IdePol AND IDSeq = p_WSesion)
                       AND C.STSCERT = 'INC'))
       AND NOT EXISTS (SELECT 1 
                         FROM VNNT_LOG_CONTROL_LEGACY CY 
                        WHERE CY.Id_Transf = CL.ID_TRANSF 
                          AND CY.Proceso = 'EMI' 
                          AND CY.Resultado = 'PEN');


  cMensajeRestriccionSAS VARCHAR2(400);

  P   POLIZA%ROWTYPE;
  cStsCert  CERTIFICADO.STSCERT%TYPE;
  
BEGIN
    p_cError := '0,OK';
    P := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IDEPOL IS NULL THEN
      p_cError := '1,'||'No existe la Póliza '||P.IdePol;
      DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||P.IdePol);
      RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||P.IdePol);
    END IF;

    cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
    if not cMensajeRestriccionSAS is null then
        p_cError := '1,'||cMensajeRestriccionSAS;
        DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
        RAISE_APPLICATION_ERROR(-20100,cMensajeRestriccionSAS);
    end if;

    DBMS_OUTPUT.PUT_LINE('Se incluirán los certificados seleccionados.');

    --.:. PR_VAL_BLOQ_SOAT(nExiste);    
    BEGIN
        SELECT NVL(MAX(1),0)
        INTO nVal
        FROM DUAL
        WHERE EXISTS ( SELECT 1
                       FROM POLIZA PO
                       WHERE PO.CodProd = P.CODPROD -- :C01_1.CCODPROD
                            AND PO.NumPol  = P.NUMPOL -- :C01_1.NNUMPOL
                            AND EXISTS ( SELECT 1
                                        FROM POL_BLOQUEO_ENDOSO B
                                        WHERE PO.IdePol   = B.IdePol
                                        AND B.indbloqEnd = 'S')) ;
    EXCEPTION
        WHEN OTHERS THEN
            nVal := 0;
    END;
    nExiste := nVal;
    -- FIN  PR_VAL_BLOQ_SOAT(nExiste);

    IF NVL(nExiste ,0) = 1 THEN
      --.:.LR_Error('Opción Bloqueada por impresión de Padrón de Certificados');
        p_cError := '1,'||'Opción Bloqueada por impresión de Padrón de Certificados';
        DBMS_OUTPUT.PUT_LINE('Opción Bloqueada por impresión de Padrón de Certificados');
        RAISE_APPLICATION_ERROR(-20100,'Opción Bloqueada por impresión de Padrón de Certificados');
    ELSE
        -- FR_Validar_Coaseguro(P_nIdePol);
        DECLARE
            kListaEstadoModificable CONSTANT VARCHAR2(8) := 'INC,MOD';
            kSI                     CONSTANT VARCHAR2(1) := 'S';
            cExiste                 VARCHAR2(1);
        BEGIN
            cExiste := PR_MOD_DIST_COA.Existe_Mod_Dist_Coa_Estado(p_nIdePol,kListaEstadoModificable);
            IF (cExiste = kSI AND P.STSPOL = 'MOD') THEN
                cExiste := PR_POLIZA_UTIL.Existe_Oper_Pol(p_nIdePol);
                IF cExiste = kSI THEN
                    -- R_Error('Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado');  
                    p_cError := '1,'||'Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado';
                    DBMS_OUTPUT.PUT_LINE('Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado');
                    RAISE_APPLICATION_ERROR(-20100,'Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado');
                END IF; 
            END IF; 
        END;
        -- FIN

        cEmiPro := 0;
        cEsMsv  := 0;
        BEGIN
          --es producto s.masivo
          BEGIN
            SELECT 1
              INTO cEsMsv
              FROM DUAL
            WHERE EXISTS (SELECT 1
                            FROM VNNT_CONTROL_POLIZA P
                            WHERE P.Idepol = p_nIdePol);
          EXCEPTION
            WHEN OTHERS THEN
              cEsMsv := 0;
          END;
          IF cEsMsv = 1 THEN
            --tiene emisión en proceso 
            BEGIN
              SELECT 1
              INTO cEmiPro
              FROM DUAL 
              WHERE EXISTS (SELECT 1
                      FROM VNNT_CONTROL_LEGACY CL
                      WHERE (CL.NUMCARGA) IN
                        (SELECT DISTINCT NUMCARGA
                          FROM DATOS_EXT_POLIZA
                          WHERE (IDEPOL, NUMPOL, NUMCERT) IN
                            (SELECT P.IDEPOL, P.NUMPOL, C.NUMCERT
                              FROM POLIZA P, CERTIFICADO C
                              WHERE P.IDEPOL = C.IDEPOL
                              AND P.Idepol = p_nIdePol))
                      AND EXISTS (SELECT 1 
                              FROM VNNT_LOG_CONTROL_LEGACY CY 
                            WHERE CY.Id_Transf = CL.ID_TRANSF 
                              AND CY.Proceso = 'EMI' 
                              AND CY.Resultado IN ('PRO')));
            EXCEPTION
              WHEN OTHERS THEN
              cEmiPro := 0;
            END;
            IF cEmiPro = 1 THEN
               --.:. nButton := LR_Alerta('La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.');
               p_cError := '1,'||'La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.';
               DBMS_OUTPUT.PUT_LINE('La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.');
               RAISE_APPLICATION_ERROR(-20100,'La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.');
            ELSE
              SELECT SQ_NUMCESION.NEXTVAL INTO WSESION FROM SYS.DUAL;
            END IF;
          END IF;
      END;

      IF cEmiPro = 0 THEN
            DBMS_OUTPUT.PUT_LINE('Incluyendo Certificados ...');
            BEGIN
                SELECT C.Stscert
                INTO cStsCert
                FROM CERTIFICADO C
                WHERE C.IDEPOL = p_nIdePol
                AND C.NUMCERT  = p_nNumCert;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                p_cError := '1,'||'El certificado no valido';
                DBMS_OUTPUT.PUT_LINE('El certificado no valido');
                RAISE_APPLICATION_ERROR(-20100,'El certificado no valido');
            END;
            IF cStsCert = 'VAL' THEN
                PR_CERTIFICADO.Incluir(P_nIdePol,P_nNumCert);
                
                PR_POL_CERT_ELECT.Incluir(P_nIdePol,P_nNumCert);
                
                IF cEsMsv = 1 AND cEmiPro = 0 THEN
                  INSERT INTO TMP_CERTIF VALUES (WSESION,P_nIdePol,P_nNumCert);
                END IF;
            ELSE
                p_cError := '1,'||'No se incluirá el certificado '||p_nNumCert||', no se encuentra en estado Válido.';
                DBMS_OUTPUT.PUT_LINE('No se incluirá el certificado '||p_nNumCert||', no se encuentra en estado Válido.');
                RAISE_APPLICATION_ERROR(-20100,'No se incluirá el certificado '||p_nNumCert||', no se encuentra en estado Válido.');
            END IF;
      END IF; 
      
      IF cEsMsv = 1 AND cEmiPro = 0 THEN
        BEGIN
            n_IDLogLote := 0;
            DBMS_OUTPUT.PUT_LINE('Pone en Cola de Emisión Pendiente...'); 
            FOR r_CIdTra IN C_IDTRANSF(P.CODPOL, P.NUMPOL,WSESION) LOOP
              /*Actualiza el Log*/
              SELECT PR_VNNT_CARGA_UTIL.REGISTRAR_LOG_LOTE_LEGACY( r_Cidtra.Id_Transf,'INC','TER') 
              INTO n_IDLogLote 
              FROM DUAL;
              /*Poner en Cola de Emisión */       
                PR_VNNT_CARGA.DEFINE_COLAS_AUTOMATICAS(r_Cidtra.Id_Transf,'EMI','1');
            END LOOP;
          DELETE FROM TMP_CERTIF WHERE IDSeq = WSESION;
        END;
        
      END IF;
      --<F N3040065>
      --habilitar trigger PRE-RECORD del bloque B01_1

      
      DBMS_OUTPUT.PUT_LINE('Proceso ejecutado satisfactoriamente.');

    END IF;
    
    DBMS_OUTPUT.PUT_LINE(p_cError);
EXCEPTION
  WHEN OTHERS THEN
        p_cError := '1,'||SQLERRM;
        DBMS_OUTPUT.PUT_LINE(SQLERRM);
        RAISE_APPLICATION_ERROR(-20100,SQLERRM);
END SP_INCLUIR_CERTI_CLAU_C;
--
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
END SP_VALIDA_COBERT_PROV;
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
FUNCTION FN_MENSAJE_ACSEL(p_cTipo VARCHAR2, 
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
   CLOSE C_AUTOVIALA;

END SP_AUTORIZA_RESTRIC_EMI_POLIZA;
--
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
        
    nIdePol := p_nIdePol;
    
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
              IF PR_API_EMI_TRAN.FN_IND_FACULTATIVO(XX.IdePol, XX.NumCert, XX.CodRamoCert, XX.CodRamoRea, nIdeRecAnt, XX.IdeRec) = 1 THEN
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
/*-------------------------------------------------------------------------------
  Nombre      : FN_EXISTE_FACULTATIVO
  Proposito   : Valida si el Certificado es Facultativo y tiene Distribución
  Log de Cambios 
    Fecha	          Autor			     Descripciión
  31/10/2011    Noema Paredes   Creación <N3037805>
-------------------------------------------------------------------------------*/
FUNCTION FN_EXISTE_FACULTATIVO (p_nIdePol  CERTIFICADO.IdePol%TYPE, 
                                p_nNumCert CERTIFICADO.NumCert%TYPE) RETURN NUMBER IS
  nExiste NUMBER(1);
BEGIN
  SELECT 1
  INTO nExiste
  FROM TEMP_DIST_REA dr, CERTIFICADO c
  WHERE dr.IdePol = c.IdePol
    AND dr.NumCert = c.NumCert
    AND dr.IdePol = p_nIdePol
    AND dr.NumCert = p_nNumCert
    AND dr.NumModDistRea =0      
    AND dr.ideCttoRea = 99999999999999
    AND c.StsCert IN('INC','MOD')
    AND EXISTS (
      SELECT 1
      FROM TEMP_DIST_FACULT df
      WHERE df.IdePol = dr.IdePol
        AND df.NumCert = dr.NumCert
        AND df.CodRamoCert= dr.CodRamoCert
        AND df.CodRamoRea= dr.CodRamoRea
        AND df.IdeRec= dr.IdeRec
        AND df.NumModDistRea =0
        AND df.ideCttoRea = 99999999999999);

  RETURN (nExiste);
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN (0);
  WHEN TOO_MANY_ROWS THEN
    RETURN (1);

END FN_EXISTE_FACULTATIVO;
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
            
       cIndDivRecal := PR_API_EMI_TRAN.FN_APLICA_REDIST_FINAL(XX.IdePol, XX.NumCert, XX.CodRamoCert, XX.IdeRec, XX.FecIniVig, XX.FecFinVig, XX.CodRamoRea);
       IF cIndDivRecal = 'SI' AND PR_API_EMI_TRAN.FN_EXISTE_FACULTATIVO(XX.IdePol, XX.NumCert) = 1 THEN
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
      cAplicaRedist := PR_API_EMI_TRAN.FN_CAL_APLICA_RECALCULO(p_nIdePol); -- FR_APLICA_RECALCULO(C01.nIdePol);
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
      WHERE IDEPOL = p_nIdePol
      AND NUMOPER IS NULL
      AND INDAPLIC = 'S';

      DBMS_OUTPUT.PUT_LINE('No aplica recálculo');
  
    END IF;  

  END SP_VALIDA_RECALCULO;
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
  cExPSt         VARCHAR2(1);
  cStatusP       OPER_POL_STATUS.Status%TYPE;
BEGIN

    POL := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF POL.IdePol IS NULL THEN
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
    RPST.IdePol := POL.IdePol;
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
            WHERE A.IDEPOL   = p_nIdePol
            AND B.IDECTTOREA = A.IDECTTOREA;
    
    BEGIN
    
        IF POL.FecIniVig = TO_DATE('30-JUN-2004','DD/MON/YYYY') THEN
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
      PR_API_EMI_TRAN.SP_VALIDA_RECALCULO_RM(p_nIdePol);
      PR_GEN_REA_UTIL_RM.VALIDAR_POLITICAS(p_nIdePol);
    ELSE
      -- FR_VALIDA_RECALCULO;
      PR_API_EMI_TRAN.SP_VALIDA_RECALCULO(p_nIdePol);
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
          PR_API_EMI_TRAN.SP_AUTORIZA_RESTRIC_EMI_POLIZA(p_nIdePol);
    END IF;

  BEGIN
    nEstadoValGeneral := PR_GEN_REA_UTIL_RM.VALIDAR_FACULTATIVO(p_nIdePol); -- 1:OK, 0:HAY ERRORES
  EXCEPTION
    WHEN OTHERS THEN
      nEstadoValGeneral := 0;
      DBMS_OUTPUT.PUT_LINE('Error: '
                || 'Para : '
                || PR_API_EMI_TRAN.FN_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol)
                ||' <Informe al Area de Reaseguro>');
      RAISE_APPLICATION_ERROR(-20100,'Error: '
                || 'Para : '
                || PR_API_EMI_TRAN.FN_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol)
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
 FUNCTION FN_OBTCONFREDIST ( p_nIdePol IN POLIZA.IdePol%TYPE ) RETURN VARCHAR2 IS  
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
FUNCTION FN_EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE , p_cTipoOper IN LVAL.CODLVAL%TYPE) RETURN OPER_POL.NumOper%TYPE 
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
          ---------------------------------
      cAlcance VARCHAR2(8);

    PZ POLIZA%ROWTYPE;
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
    PR_API_EMI_TRAN.SP_ACTU_OPERA_MOT_ENDOSO(p_nIdePol);
    cIndAct := 'S';  -- .:. LR_Invocar_PreACT(kActivar,p_nIdePol,1,'N'); -- kActivar = 'AC'
    --
    DBMS_OUTPUT.PUT_LINE('Trabajando... Activando Poliza (Terminado)');

    IF cIndAct = kSI THEN
      nNumOper:=0;      
      DBMS_OUTPUT.PUT_LINE('Trabajando... Activando Poliza Definitiva - 1');
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(D): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      -- <I N3041386> Claudia Yugar Ladines / 14-08-2013 / CNT-01702: Manejo de re-cálculo de la distribución de reaseguros     
      cConfRedist := PR_API_EMI_TRAN.FN_OBTCONFREDIST(p_nIdePol);-- FR_OBTCONFREDIST ( p_nIdePol );
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
          -- FR_ACTIVAR.Actualizar_Oper_Remesa_Bco(p_nIdePol,nNumOper);
            DECLARE
              nRemPend           NUMBER(3) := 0;
            BEGIN
              BEGIN
                SELECT NVL(MAX(1),0)
                INTO   nRemPend
                FROM   DUAL
                WHERE  EXISTS (SELECT 1
                              FROM   OPER_POL_REMESA_BCO
                              WHERE  IdePol  = p_nIdePol
                              AND    NumOper IS NULL
                              AND    IndProc = 'N'
                              );
              EXCEPTION
                WHEN OTHERS THEN
                  nRemPend := 0;
              END;
              
              IF nRemPend = 1 THEN
                UPDATE OPER_POL_REMESA_BCO
                SET    NumOper = nNumOper
                WHERE IdePol  = p_nIdePol
                  AND NumOper IS NULL
                  AND IndProc = 'N';       		
              END IF;   	  
            END;
          -- FIN FR_ACTIVAR.Actualizar_Oper_Remesa_Bco
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
    
        DBMS_OUTPUT.PUT_LINE('Se procederá a generarár Documentos de Cobranza,segun el Fraccionamiento Calculado,una vez Concluido este Proceso para Realizar cualquier Modificación debera Anular dichos Documentos');
        DBMS_OUTPUT.PUT_LINE('Generando Documentos');
        -- .:. cEvaluarCorreo:=TRUE;--<RTC-154713>13/03/2019   Milton Lope BBVA - Mejoras al proceso de emisión de G&L P9: Envío de correos 
      
      END IF; 
      PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA_UTIL.FINALIZAR_ACTIVACION: IDEPOL --> '||p_nIdePol ,
                                            SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
      PR_POLIZA_UTIL.FINALIZAR_ACTIVACION(p_nIdePol,nNumOper);            
      --<I RTC-154713>13/03/2019   Milton Lopez BBVA - Mejoras al proceso de emisión de G&L P9: Envío de correos automáticos de respuesta de las emisiones manuales
     /*  
     IF PR.BUSCA_LVAL('PCBBVA','SWITCH') ='TRUE' THEN
        cSwitchCorreo := TRUE;
      ELSE          
        cSwitchCorreo := FALSE;
      END IF;
      */
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
      -- FR_UPDAUTORIZAREDIST(p_nIdePol, nNumOper); -- <N3041386> Claudia Yugar Ladines / 23-08-2013 / CNT-01702: Manejo de re-cálculo de la distribución de reaseguros
          BEGIN
            --   
            UPDATE AUTORIZA_REDIST
               SET NUMOPER = nNumOper
             WHERE IDEPOL = p_nIdePol 
               AND NUMOPER IS NULL
               AND INDAPLIC = 'S' 
               AND INDREDIST IS NOT NULL;        
             --
          EXCEPTION 
            WHEN OTHERS THEN
              NULL;
          END;
      -- FIN
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
PROCEDURE SP_ACTUALIZAR_INDCOBESP(p_nNumOper IN OPER_POL.NumOper%TYPE) IS
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
PROCEDURE SP_ACTIVAR_ACTUALIZAR_POLIZA( p_nIdePol IN Poliza.IdePol%TYPE,  p_cTipoOper IN LVAL.CODLVAL%TYPE)
IS
  
      kActivo       CONSTANT VARCHAR2(3) := 'ACT';
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
                    DBMS_OUTPUT.PUT_LINE(PR_API_EMI_TRAN.FN_Mensaje_ACSEL('CSG',52,'  ','  ','  '));
                    RAISE_APPLICATION_ERROR(-20100,PR_API_EMI_TRAN.FN_Mensaje_ACSEL('CSG',52,'  ','  ','  '));
          END IF;
          -- Actualizamos fecha de operación.
          UPDATE POLIZA
          SET    FecOper = PR.Fecha_Movimiento(TRUNC(SYSDATE)) -- :C00.dFecOper
          WHERE  IdePol = PZ.IDEPOL;
          IF PZ.CODFORMPAGO IN ('A','E') THEN
                nNumOper := PR_API_EMI_TRAN.FN_EJECUTAR_ACTIVACION(PZ.IDEPOL,p_cTipoOper); --FR_ACTIVAR.Ejecutar_Activacion(p_nIdePol);       
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
            nNumOper := PR_API_EMI_TRAN.FN_EJECUTAR_ACTIVACION(p_nIdePol,p_cTipoOper);  -- FR_ACTIVAR.Ejecutar_Activacion(p_nIdePol);
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
                PR_API_EMI_TRAN.SP_ACTUALIZAR_BASERECADTCO(nNumOper); -- FR_ACTIVAR.Actualizar_BaseRecaDtco(nNumOper);            
                PR_API_EMI_TRAN.SP_ACTUALIZAR_INDCOBESP(nNumOper); -- FR_ACTIVAR.Actualizar_IndCobEsp(nNumOper);
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
) IS

	nCantReg  NUMBER := 0;
	v_NumOper Oper_Pol.NumOper%TYPE;	
	nStsPol   POLIZA.StsPol%TYPE;
    PZ        POLIZA%ROWTYPE;
    
BEGIN

    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;
	
    IF p_cTipoOper IS NOT NULL THEN

        PR_API_EMI_TRAN.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;	
        PR_API_EMI_TRAN.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL,p_cTipoOper); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);  

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

        PR_API_EMI_TRAN.SP_ACTIVA_GARANTIAS(PZ.STSPOL, PZ.IDEPOL);  -- FR_ACTIVA_GARANTIAS(nStsPOL);

        --<I RTC 316472> David Yupanqui / 27-04-2023 / [PROYECTO TECH CORE] Armado Automático del Anexo 7
        if pr.busca_lval('SLIP_AUT',PZ.CODPROD) <> 'INVALIDO' then
            acselx.pr_slip_auto.texto_prima(PZ.IDEPOL);
        end if;
        --<F RTC 316472>
    
    END IF;
END SP_ACT_ENDOSO_VIG_OPE;
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
  p_cError       OUT VARCHAR2
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
      PR_API_EMI_TRAN.SP_VALIDA_COBERT_PROV(PZ.IDEPOL, cMsjCobProv); -- PR_VALIDA_COBERT_PROV(PZ.IDEPOL, cMsjCobProv);
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
        cValTrec := PR_API_EMI_TRAN.FN_VALIDA_TREC(PZ.IDEPOL); -- FR_VALIDA_TREC(PZ.IdePol);
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
        PR_API_EMI_TRAN.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;
        cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
        DBMS_OUTPUT.PUT_LINE(cTexto);
        IF PZ.STSPOL <> 'MOD' THEN
            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
            PR_API_EMI_TRAN.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL,p_cTipoOper); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
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
            PR_API_EMI_TRAN.SP_ACT_ENDOSO_VIG_OPE(PZ.IDEPOL,p_cTipoOper);
          ELSE

            SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;  
            PR_API_EMI_TRAN.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL,p_cTipoOper); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);

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
          PR_API_EMI_TRAN.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_VALIDAR_DATOS;
          cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
          DBMS_OUTPUT.PUT_LINE(cTexto);
          IF PZ.STSPOL <> 'MOD' THEN            
              /*
              cValidaTram := FR_VALIDA_TRAMITE(NVL(PR_POLIZA_TRAMITE.OBTENERULTIMOTRAMITE(PZ.IDEPOL),nNumTram));
              IF cValidaTram = 'N' THEN
                 DBMS_OUTPUT.PUT_LINE(cValidaTram);
                 RAISE_APPLICATION_ERROR(-20106,cValidaTram);
              END IF;
              */
    
              -- Si todo está correcto se continuamos con la actualización.
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT
              PR_API_EMI_TRAN.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL,p_cTipoOper); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);  -- <N3028071> Giancarlo Ramirez - Optimizacion CNT                      
          END IF;
        ELSE
          -- nDummy := FR_ALERTA('El trámite '||NVL(:B02_1.cNumtramite,nNumTram)|| ' con el que se esta activando la póliza pertenece a otra operación '||cTipo||' , '||PZ.CODPROD||'-'||PZ.NUMPOL ||', NumOper '|| nNumOper);
          DBMS_OUTPUT.PUT_LINE('El trámite '|| ' con el que se esta activando la póliza pertenece a otra operación '||cTipo||' , '||PZ.CODPROD||'-'||PZ.NUMPOL ||', NumOper '|| nNumOper);
        END IF;

      ELSE 

          PR_API_EMI_TRAN.SP_VALIDAR_DATOS(PZ.IDEPOL); -- FR_Validar_Datos;
          cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
          DBMS_OUTPUT.PUT_LINE(cTexto);
          IF PZ.STSPOL <> 'MOD' THEN
              PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
              PR_API_EMI_TRAN.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL,p_cTipoOper); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
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
                PR_API_EMI_TRAN.SP_ACT_ENDOSO_VIG_OPE(PZ.IDEPOL,p_cTipoOper);
              ELSE

                PR_API_EMI_TRAN.SP_VALIDAR_DATOS(PZ.IDEPOL); -- FR_Validar_Datos;
                PR_API_EMI_TRAN.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL,p_cTipoOper); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);

              END IF;
     
      END IF; --F Confirmar trámite
            
    END IF;   

  END IF;
 
  --<I COR103-925> Werner Tito / 19-09-2023 / [PROYECTO TECH CORE] Integración con Satelites InsPAT
  -- Activa Garantias
  PR_API_EMI_TRAN.SP_ACTIVA_GARANTIAS(PZ.STSPOL , PZ.IDEPOL); -- FR_ACTIVA_GARANTIAS(nStsPOL);
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


END PR_API_EMI_TRAN;
