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
CREATE OR REPLACE PROCEDURE SP_MNT_COBERT_CERT_C(
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

DECLARE
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