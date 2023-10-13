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
  CREATE OR REPLACE FUNCTION FN_CAL_APLICA_RECALCULO(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2 
  
  IS
  														
    cCodFormPago VARCHAR2(1);
    nIdeRecAnt   RECIBO.IDEREC%TYPE;
    nSumaAsegAnt GEN_REA.SUMAASEG%TYPE;
    GR           GEN_REA%ROWTYPE;
		
    cDivision	 VARCHAR2(5);
    cIndDivRecal VARCHAR2(2);
    nnumoper	 oper_pol_t.numoper%type;
		
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
          	
			 cIndDivRecal := FR_APLICA_REDIST_FINAL(XX.IdePol, XX.NumCert, XX.CodRamoCert, XX.IdeRec, XX.FecIniVig, XX.FecFinVig, XX.CodRamoRea);
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