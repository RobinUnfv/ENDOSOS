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
        nIdePol	          POLIZA.IdePol%TYPE;
        nFormPagoInv      NUMBER := 0;
        nExisteEmision    NUMBER := 0;
        cIndAplica        VARCHAR2(1);
        cIndFacultativo   VARCHAR2(1);
        cIndRedist        VARCHAR2(1);
        nProdInvalido     NUMBER := 0;
        cCodFormPago      VARCHAR2(1);
        nIdeRecAnt        RECIBO.IDEREC%TYPE;
        nNumOper	      oper_pol_t.numoper%type;
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
	
	