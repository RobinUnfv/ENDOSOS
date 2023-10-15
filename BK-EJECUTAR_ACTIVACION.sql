  /*-----------------------------------------------------------------------------
  Nombre      : EJECUTAR_ACTIVACION
  Proposito   : Funsión que ejecuta la activación de la póliza
  Referencias : MANTPOL.fmb
  Parametros  :
                p_idepol     NUMBER    Identificador de Poliza

  Log de Cambios
    Fecha         Autor                      Descripcion
    05/10/2022    Santana Llacza Robinzon    Creacion
  -----------------------------------------------------------------------------*/
FUNCTION EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN OPER_POL.NumOper%TYPE IS
    RETURN oper_pol.numoper%type 
IS

	    kSI             CONSTANT VARCHAR2(1) := 'S';
	    kNO             CONSTANT VARCHAR2(1) := 'N';    

	    kActivar        CONSTANT VARCHAR2(2) := 'AC';
	    nDummy		  NUMBER;
	    cFirma 		  VARCHAR2(1);
	    nNumOper		  OPER_POL.NumOper%TYPE;

	    nExisteOper     VARCHAR2(1);
	    cProcOper       REGLAS_STS.CodProceso%TYPE;  
	    nNumOperendoso  OPER_POL.NumOper%TYPE;
	    cIndAnul        OPER_POL.IndAnul%TYPE;

	    cIndAct         VARCHAR2(1);
	    --
	    P               POLIZA%ROWTYPE;

		nOper   		NUMBER; 
		nPlan   		NUMBER(3);
		nMensaje      	NUMBER;

		nMtoOper       OPER_POL.MtoOper%TYPE;
		cIndOperValida VARCHAR2(1);
		cIndProc       VARCHAR2(1); 
		nNumOperAux    OPER_POL.NumOper%TYPE;

        pl_id         PARAMLIST ;
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
  
		rops                   OPER_POL_STATUS%ROWTYPE;
		cstspolv               POLIZA_STATUS.STSPOL%TYPE;    
		
		cConfRedist	VARCHAR2(1) := 'N';

        cMensajePol varchar2(100);

        vcMarca     ACSELX.CONTADOR_ELIMINACION_LAS.CODNECESIDAD%TYPE;
		vnNumCert   ACSELX.CERTIFICADO.NUMCERT%TYPE;
		vdFecExc    ACSELX.CERTIFICADO.FECEXC%TYPE;
		vcMotExc    ACSELX.CERTIFICADO.CODMOTVEXC%TYPE;
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
				AND  T.TIPODOC = 'LQ'
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
	    RETURN POLIZA.IndEsqReaPol%TYPE IS cIndCobPoliza  POLIZA.IndEsqReaPol%TYPE;
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

  	IF TIENE_REASEGURO = kSI THEN
	   SUMA_MAYOR_REA(p_nIdePol);
	   PR_POLIZA.Certificado_Cero(p_nIdePol);
	  -- STANDARD.COMMIT;
	END IF;

    IF POL.STSPOL = 'VAL' THEN
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Incluir: IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol);
	    PR_POLIZA.Incluir(p_nIdePol);
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Incluir: IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol);
	    -- STANDARD.COMMIT;
	END IF;

    PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(T): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol);
	  
	 
	nNumOper := PR_POLIZA.Activar(p_nIdePol,'T');
    PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(T): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol);
	
	-- LR_Cursor_Normal;
    IF cIndAct = kSI THEN
        nNumOper := 0;

        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
			'I',
			'ACTIVAR POLIZA - MANTPOL',
			'PR_POLIZA.Activar(D): IDEPOL --> ' || p_nIdePol,
			SYSDATE,
			SYSDATE,
			'01',
			0,
			p_nIdePol
		);

        cConfRedist := PR_INTERFASE_AX.FN_OBTCONFREDIST(p_nIdePol); -- cConfRedist := FR_OBTCONFREDIST (p_nIdePol);
        BEGIN		
            SELECT NVL(MAX(INDREDIST),'N')
            INTO cConfRedist
            FROM AUTORIZA_REDIST
            WHERE IDEPOL = p_nIdePol 
            AND NUMOPER IS NULL
            AND INDAPLIC = 'S'
            AND INDREDIST IS NOT NULL;
        EXCEPTION
            WHEN OTHERS THEN
                cConfRedist := 'N';
        END;

        BEGIN
			SELECT numcert,fecexc,codmotvexc 
            INTO vnNumCert,vdFecExc,vcMotExc
			FROM CERTIFICADO
			WHERE idepol = p_nIdePol
			AND stscert  = 'MOD'
			AND ROWNUM   = 1;
		EXCEPTION
			WHEN OTHERS THEN 
				vnNumCert := NULL;
				vdFecExc  := NULL;
				vcMotExc  := NULL;
		END;

        IF PR.Busca_Lval('PROANULA', '313126SWITCH') = 'ON' THEN 
				vcMarca := PR_MARCAS_ANULACION.FR_OBTENER_MARCA (
					p_nIdePol,
					TRUNC(vdFecExc),
					vcMotExc,
					vnNumCert,
					'CER');
		END IF;

        nNumOper := PR_POLIZA.Activar (
			p_nIdePol,
			'D',
			cConfRedist
		);

        IF PR.Busca_Lval('PROANULA', '313126SWITCH') = 'ON' THEN 
		    IF vcMarca IS NOT NULL AND nNumOper IS NOT NULL THEN 
		          PR_MARCAS_ANULACION.PR_GRABAR_MARCA(p_nIdePol, nNumOper, vnNumCert, 'CER', vcMarca);
            END IF;
        END IF;

        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
			'F',
			'ACTIVAR POLIZA - MANTPOL',
			'PR_POLIZA.Activar(D): IDEPOL --> ' || p_nIdePol,
			SYSDATE,
			SYSDATE,
			'01',
			0,
			p_nIdePol
		);

        BEGIN
			SELECT 'S' 
            INTO cIndOperValida
			FROM dual
			WHERE EXISTS (
					SELECT 1
					FROM Oper_Pol op
					WHERE op.Idepol = p_nIdePol
					AND op.Numoper <> nNumOper);
		EXCEPTION
			WHEN NO_DATA_FOUND THEN 
				cIndOperValida := 'N';
		END;

        IF cIndOperValida = 'S' THEN 
			BEGIN
				SELECT 'S' 
                INTO cIndOperValida
				FROM dual
				WHERE
					EXISTS (
						SELECT 1
						FROM Oper_Pol op
						WHERE op.Idepol = p_nIdePol
						AND op.Numoper  = nNumOper
						AND op.Tipoop NOT IN ('ESV')
					);

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
				FOR cADP IN cActu_DatPart(nNumOper) LOOP
					UPDATE DATOS_PARTICULARES
					SET valtrasf = cADP.MtoOper
					WHERE IdePol = cADP.IdePol
					AND NumCert  = cADP.NumCert
					AND CodCrit  = 'PRIMA_PLAN';
				END LOOP;

			END IF;
	      
        END IF;

        IF cIndOperValida = 'S' THEN 
			PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
				'I',
				'ACTIVAR POLIZA - MANTPOL',
				'PR_OPER_POL_DET.CREAR_MOV: IDEPOL --> ' || p_nIdePol,
				SYSDATE,
				SYSDATE,
				'01',
				0,
				p_nIdePol
			);

			PR_OPER_POL_DET.CREAR_MOV(
				nNumOper,
				:B07_1.TipoOper,
				:B07_1.FecIniVig,
				:B07_1.FecFinVig
			);

			PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
				'F',
				'ACTIVAR POLIZA - MANTPOL',
				'PR_OPER_POL_DET.CREAR_MOV: IDEPOL --> ' || p_nIdePol,
				SYSDATE,
				SYSDATE,
				'01',
				0,
				p_nIdePol
			);
        END IF;

        PR_OPER_POL.Existe_Oper_No_Nunca_Ren(p_nIdePol, cProcOper, nExisteOper, nNumOperEndoso);

        IF nExisteOper = kSI THEN 
		    cIndAnul := PR_OPER_POL.Obtener_IndAnul(nNumOperEndoso);

			IF cIndAnul = kNo THEN 
				PR_POLIZA.Endoso_Ren(p_nIdePol, 'D');
			END IF;
        END IF;

        PR_EVALUA_POLITICA.Inicializa_Autoriza_Violacion(p_nIdePol, nNumOper);

        cFirma := PR_EVALUA_POLITICA.Obtiene_Firma_Digital(p_nIdePol, nNumOper);

        Actualiza_Ind_Fact;

        BEGIN 
			PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
				'I',
				'ACTIVAR POLIZA - MANTPOL',
				'PR_RECA_DCTO_CERTIF.Actualiza_Reca_Dcto_Cert: IDEPOL --> ' || p_nIdePol,
				SYSDATE,
				SYSDATE,
				'01',
				0,
				p_nIdePol
			);

			PR_RECA_DCTO_CERTIF.Actualiza_Reca_Dcto_Cert(p_nIdePol, nNumOper);

			PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
				'F',
				'ACTIVAR POLIZA - MANTPOL',
				'PR_RECA_DCTO_CERTIF.Actualiza_Reca_Dcto_Cert: IDEPOL --> ' || p_nIdePol,
				SYSDATE,
				SYSDATE,
				'01',
				0,
				p_nIdePol
			);
		EXCEPTION
			WHEN OTHERS THEN 
                    DBMS_OUTPUT.PUT_LINE(' Error en package PR_RECA_DCTO_CERT.ACTUALIZA_RECA_DCTO_CERT - ' || SQLERRM);
                    RAISE_APPLICATION_ERROR(-20100,' Error en package PR_RECA_DCTO_CERT.ACTUALIZA_RECA_DCTO_CERT - ' || SQLERRM);
		END;

        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
			'I',
			'ACTIVAR POLIZA - MANTPOL',
			'FR_ACTIVAR.Actualizar_Oper_Remesa_Bco: IDEPOL --> ' || p_nIdePol,
			SYSDATE,
			SYSDATE,
			'01',
			0,
			p_nIdePol
		);

		FR_ACTIVAR.Actualizar_Oper_Remesa_Bco(p_nIdePol, nNumOper);

		PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
			'F',
			'ACTIVAR POLIZA - MANTPOL',
			'FR_ACTIVAR.Actualizar_Oper_Remesa_Bco: IDEPOL --> ' || p_nIdePol,
			SYSDATE,
			SYSDATE,
			'01',
			0,
			p_nIdePol
		);

        SELECT NVL(SUM(MtoOper), 0) 
        INTO nMtoOper
		FROM OPER_POL
		WHERE IdePol = p_nIdePol
		AND NumOper = nNumOper;

        IF nMtoOper != 0 THEN -- Si mueve prima se generan documentos.
				cEvaluarCorreo := TRUE;
        END IF;

        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
			'I',
			'ACTIVAR POLIZA - MANTPOL',
			'PR_POLIZA_UTIL.FINALIZAR_ACTIVACION: IDEPOL --> ' || p_nIdePol,
			SYSDATE,
			SYSDATE,
			'01',
			0,
			p_nIdePol
		);

		PR_POLIZA_UTIL.FINALIZAR_ACTIVACION(p_nIdePol, nNumOper);

        IF PR.BUSCA_LVAL('PCBBVA', 'SWITCH') = 'TRUE' THEN 
		   cSwitchCorreo := TRUE;
		ELSE 
		   cSwitchCorreo := FALSE;
		END IF;
        /* 
        IF (cEvaluarCorreo AND cSwitchCorreo) THEN 
			    IF PR.BUSCA_LVAL('PCBBVA', USER) = 'INVALIDO' AND PR.BUSCA_LVAL ('PCBBVA', 'VERMSJUSER') = '1' THEN 
						IF PR.BUSCA_LVAL ('PCBBVA', 'VERMSJUSER') = '1' THEN 
								SET_ALERT_PROPERTY(
									'AL_MENSAJE',
									ALERT_MESSAGE_TEXT,
									'Correo del usuario no esta configurado, consultar a TI.'
								);

								alert_button := SHOW_ALERT('AL_MENSAJE');
						END IF;
				ELSE 
					cEnviaCorreo := ACSELX.PR_GESTOR_CORREO_POLIZA_BBVA.ENVIO_CORREO_POLIZA(p_nIdePol, 1);

					IF(cEnviaCorreo = cOk) THEN 
						SET_ALERT_PROPERTY(
							'AL_MENSAJE',
							ALERT_MESSAGE_TEXT,
							'Correo enviado exitosamente'
						);

						alert_button := SHOW_ALERT('AL_MENSAJE');

					END IF;

					IF (cEnviaCorreo NOT IN (cNOk, cOk)) THEN 
							IF cEnviaCorreo = 'La poliza ya fue enviada por correo para su validación.' THEN 
									SET_ALERT_PROPERTY('AL_MENSAJE', ALERT_MESSAGE_TEXT, cEnviaCorreo);
							ELSIF cEnviaCorreo = 'Correo del usuario no esta configurado, consultar a TI.' THEN 
									SET_ALERT_PROPERTY('AL_MENSAJE', ALERT_MESSAGE_TEXT, cEnviaCorreo);
							ELSE 
								SET_ALERT_PROPERTY(
									'AL_MENSAJE',
									ALERT_MESSAGE_TEXT,
									'El correo no se ha generado correctamente, se volverá a enviar a las 00:00 hrs del día de hoy. Error: ' || cEnviaCorreo
								);

							END IF;

							alert_button := SHOW_ALERT('AL_MENSAJE');

					END IF;

				END IF;

        END IF;
		*/
		cIndProc := PR_COBRANZA_PPR.Gen_Cobert_Dscto(p_nIdePol);

		PR_POLIZA_MASIVA.Actualiza_Masiva(p_nIdePol);

		IF cIndProc = 'S' THEN 
				PR_GEN_REA_T.Eliminar_Rea_Temporal(p_nIdePol, NULL, NULL, NULL);

				nNumOperAux := PR_POLIZA.Activar(p_nIdePol, 'D');

				PR_POLIZA_UTIL.Finalizar_Activacion(p_nIdePol, nNumOperAux);

				PR_COBRANZA_PPR.Compensar_Documentos(nNumOperAux);

		END IF;

		IF PR.BUSCA_LVAL('PROCENCO',PR_RENOV_AUTCOBERT.DEVOLVER_TIPSEG(p_nIdePol)) <> 'INVALIDO' THEN 	
			PR_RENOV_AUTCOBERT.GEN_RENCERO_DOCSUNAT(p_nIdePol, nNumOper);
		END IF;

		PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE (
			'F',
			'ACTIVAR POLIZA - MANTPOL',
			'PR_POLIZA_UTIL.FINALIZAR_ACTIVACION: IDEPOL --> ' || p_nIdePol,
			SYSDATE,
			SYSDATE,
			'01',
			0,
			p_nIdePol
		);

		BEGIN -- Optimizar calculo Impuesto Sunat
			select ps.stspol 
			into cstspolv
			from poliza_status ps
			where ps.idepol = p_nIdePol;

			cstspolv := nvl(cstspolv, '0');
		EXCEPTION
			WHEN OTHERS THEN 
			cstspolv := '0';
		END;

		rops.IdePol  := p_nIdePol;
		rops.NumOper := nNumOper;
		rops.Status  := cstspolv;
		rops.CodUsr  := PR_SESION.USUARIO_APP;
		rops.Fecha   := sysdate;

		IF rops.Status != '0' THEN 
			PR_POLIZA_MASIVA.Crear_Oper_Pol_st(rops);
		END IF;

		BEGIN
		   DELETE FROM poliza_status where idepol = p_nIdePol;
		EXCEPTION
		  WHEN OTHERS THEN 
			cstspolv := '0';
		END;

		BEGIN
			SELECT STSPOL, CODPROD, NUMPOL 
			INTO cStsPol,cCodProd,nNumPol
			FROM POLIZA
			WHERE IDEPOL = p_nIdePol;
		EXCEPTION
			WHEN OTHERS THEN 
				DBMS_OUTPUT.PUT_LINE('Error al obtener Numero de poliza');
                RAISE_APPLICATION_ERROR(-20100,'Error al obtener Numero de poliza');
		END;

		nIndCNT := PR_IMP_POL_CCM_CON.VALIDA_CNT(nNumPol, cCodProd);
        /*
		IF PR.BUSCA_LVAL('CCMLVGRL', 'SWICTHCCM') = 'S' AND nIndCNT = 'N' AND PR.EXISTE_LVAL('CCMPRODX', :B01_1.CODPROD) = 'S' THEN
				nIdImpresion := PR_IMP_POL_CCM.DEVUELVE_IND_GRUPO_IMP(cCodProd, nNumOper, p_nIdePol);

				BEGIN
					SELECT DISTINCT OP.TIPOOP 
					INTO cTipoOper
					FROM OPER_POL OP
					WHERE IdePol = p_nIdePol
					AND NumOper  = nNumOper;
				EXCEPTION
					WHEN OTHERS THEN 
					  cTipoOper := NULL;
				END;

                IF cTipoOper != 'REM' THEN 
				        IF PR.BUSCA_LVAL('CCMPRODX', :B01_1.CODPROD) <> 'INVALIDO' THEN 
						        nIdImpresion := PR_IMP_POL_CCM.DEVUELVE_IND_GRUPO_IMP(cCodProd, nNumOper, p_nIdePol);

								pl_id := CREATE_PARAMETER_LIST('PARAM_LISTA');

								-- 
								ADD_PARAMETER(pl_id, 'p_nIdePol', TEXT_PARAMETER, p_nIdePol);

								ADD_PARAMETER(pl_id, 'p_nNumOper', TEXT_PARAMETER, nNumOper);

								ADD_PARAMETER(
									pl_id,
									'p_nIdImpresion',
									TEXT_PARAMETER,
									nIdImpresion
								);

								ADD_PARAMETER(
									pl_id,
									'p_cNumTramite',
									TEXT_PARAMETER,
									:B02_1.cNumTramite
								);

								--
								CALL_FORM('EMI00229', HIDE, NO_REPLACE, QUERY_ONLY, pl_id);

								--
								DESTROY_PARAMETER_LIST(pl_id);
                        END IF;   

						GO_BLOCK('B01_1');

						commit;

				ELSE 
						BEGIN
							SELECT
								NUMFINANC INTO nNumFinanc
							FROM
								COND_FINANCIAMIENTO F
							WHERE
								F.NUMOPER = nNumOper
								AND F.IDEPOL = p_nIdePol;

							BEGIN
								SELECT
									SQ_NUMCESION.NEXTVAL INTO WSESION
								FROM
									SYS.DUAL;
							END;

							pl_id := Create_Parameter_List('tmpdata');

							Add_Parameter(pl_id, 'PNUMFINANC', TEXT_PARAMETER, nNumFinanc);

							Add_Parameter(pl_id, 'PNUMOPER', TEXT_PARAMETER, nNumOper);

							Add_Parameter(pl_id, 'PSESION', TEXT_PARAMETER, WSESION);

							Add_Parameter(pl_id, 'PARAMFORM', TEXT_PARAMETER, 'NO');

							Run_Product(
								reports,
								'FIN00005.rep',
								SYNCHRONOUS,
								RUNTIME,
								filesystem,
								pl_id,
								null
							);

							Destroy_Parameter_List(pl_id);

                        END;

                END IF;
        END IF;
		*/
		IF nNumOper IS NOT NULL THEN 
		   -- FR_ModificaCorredor(nNumOper);
		    -- PROCEDURE FR_ModificaCorredor (nNumOper IN acselx.oper_pol.numoper%TYPE) IS
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
		END IF;

		PR_GEN_REA_T.Eliminar_Rea_Temporal(p_nIdePol, NULL, NULL, NULL);

		P := PR_POLIZA.Datos_Poliza(p_nIdePol);

		IF P.StsPol = 'ACT' THEN
				IF PR_POLIZA_UTIL.Maneja_Insp_Vehiculos_Pol(p_nIdePol) = kSI THEN 
                    DBMS_OUTPUT.PUT_LINE('Actualizando Número de Inspección en Notes');
				    FR_ACTIVAR.Actualizar_Inspeccion_Notes(p_nIdePol, :B01_1.NumPol);
				END IF;
				-- Elimina datos posibles de rehabilitación si hubiera.
				ELIMINAR_REHAB;
        END IF;
        
		DBMS_OUTPUT.PUT_LINE('Trabajando... Eliminando temporales de Mov.Prima');
		PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);
		-- FR_UPDAUTORIZAREDIST(p_nIdePol, nNumOper);
	    BEGIN
	       UPDATE AUTORIZA_REDIST
		   -- SET NUMOPER = p_nNumOper
		   SET NUMOPER  = nNumOper
		   WHERE IDEPOL = p_nIdePol 
		   AND NUMOPER IS NULL
		   AND INDAPLIC = 'S' 
		   AND INDREDIST IS NOT NULL;
		EXCEPTION 
			WHEN OTHERS THEN NULL;
	    END;


    ELSE
        DBMS_OUTPUT.PUT_LINE('Trabajando... Reversando Activación');
		PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);

    END IF;
    DBMS_OUTPUT.PUT_LINE('Proceso ejecutado satisfactoriamente');
	
	FOR C IN cActualiza LOOP

	    BEGIN
			SELECT F.NUMFACT,F.MTOFACTMONEDA,F.CODMONEDA 
            INTO nNumFact,nMtoFactMoneda,cCodMoneda
			FROM FACTURA F
			WHERE F.IDEFACT IN (
					SELECT A.IDEFACT
					FROM ACREENCIA A
					WHERE A.NUMACRE IN (SELECT G.NUMACRE
                                        FROM GIROS_FINANCIAMIENTO G
                                        WHERE G.NUMFINANC IN (SELECT RE.NUMFINANC
                                                            FROM REC_FINANCIAMIENTO RE
                                                            WHERE RE.IDEREC IN ( SELECT MAX(R.IDEREC) IDEREC
                                                                                FROM RECIBO R
                                                                                WHERE R.IDEMOVPRIMA IN (
                                                                                        SELECT M.IDEMOVPRIMA
                                                                                        FROM MOD_COBERT M
                                                                                        WHERE M.NUMMOD = C.NUMMOD
                                                                                        AND M.IDECOBERT = C.IDECOBERT)))
						)
				);

		EXCEPTION
		     WHEN OTHERS THEN 
                nNumFact := 0;
                nMtoFactMoneda := NULL;
                cCodMoneda := C.CODMONEDA;
        END;

		SELECT CER.STSCERT 
        INTO cStsCert
		FROM CERTIFICADO CER
		WHERE CER.IDEPOL = C.IDEPOL
		AND CER.NUMCERT  = C.NUMCERT;

		UPDATE LOG_EXCLUYE_CERT T
		SET T.NUMFACT       = nNumFact,
			T.STSLOG        = decode(nNumFact, 0, 'ERR', cStsCert),
			T.MTOFACTMONEDA = nMtoFactMoneda,
			T.CODMONEDA     = cCodMoneda,
			T.NUMOPER       = nNumOper
		WHERE T.IDEPOL      = C.IDEPOL
			AND T.NUMCERT   = C.NUMCERT
			AND T.STSLOG    = 'PEN'
			AND T.IDECOBERT = C.IDECOBERT
			AND T.NUMMOD    = C.NUMMOD
			AND T.NUMFACT IS NULL;

		-- COMMIT;

    END LOOP;
      
    RETURN nNumOper;
EXCEPTION
	WHEN OTHERS THEN 
    DBMS_OUTPUT.PUT_LINE('Error al Activar: ' || SQLERRM);
	-- ROLLBACK;

	PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);

	-- COMMIT;

	RETURN TO_NUMBER(NULL);
END EJECUTAR_ACTIVACION;