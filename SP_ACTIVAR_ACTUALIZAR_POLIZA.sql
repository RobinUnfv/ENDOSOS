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
		cInd	       NUMBER := 0;
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