/*---------------------------------------------------------------------------------------------
Nombre			: [PROYECTO TECH CORE] COR103-925 - Integración con Satelites InsPAT
Autor				: B2B - Werner Tito
Fecha				: 29-08-2023
Proposito		: Activar las Garantias de la Poliza
Referencias	: Reprogramacion AcselX
Log de Cambios
Fecha		  			Autor												Descripcion
29/08/2023		Werner Tito		[PROYECTO TECH CORE] COR103-925 - Integración con Satelites InsPAT
----------------------------------------------------------------------------------------------*/
PROCEDURE SP_ACTIVA_GARANTIAS (p_stspol POLIZA.StsPol%TYPE, p_nIdePol IN Poliza.IdePol%TYPE) IS
  nCantReg   NUMBER := 0;
  v_NumOper  Oper_Pol.NumOper%TYPE;	
  nStsPol	 POLIZA.StsPol%TYPE;	
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