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
    
		cDivision		 VARCHAR2(5);
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
						cDivision	:= NULL;
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