/*-----------------------------------------------------------------------
     Nombre : FN_EJECUTAR_ACTIVACION 
     Proposito:
     Referencia : Formulario MANTPOL.fmb
     Parametro :
                 p_nIdePol         Identificador de póliza.
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     Fecha         Autor               Descripción
     14/10/2023    Robinzon Santana     Creación
  -----------------------------------------------------------------------------*/   
FUNCTION FN_EJECUTAR_ACTIVACION(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN OPER_POL.NumOper%TYPE 
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

	  P               POLIZA%ROWTYPE;
	  nOper   	      NUMBER;
	  nPlan   	      NUMBER(3);
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
		
	  cConfRedist	 VARCHAR2(1) := 'N';

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
	  cIndAct := LR_Invocar_PreACT(kActivar,p_nIdePol,1,'N'); -- kActivar = 'AC'
	  --
	  LR_Cursor_Normal('Trabajando... Activando Poliza (Terminado)');

	  IF cIndAct = kSI THEN
	    nNumOper:=0;      
	    LR_Cursor_Ocupado('Trabajando... Activando Poliza Definitiva - 1');
        PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','PR_POLIZA.Activar(D): IDEPOL --> '||p_nIdePol ,SYSDATE, SYSDATE, '01', 0, p_nIdePol); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT. Pase temporal
	    -- <I N3041386> Claudia Yugar Ladines / 14-08-2013 / CNT-01702: Manejo de re-cálculo de la distribución de reaseguros	    
	    cConfRedist := FR_OBTCONFREDIST ( p_nIdePol );
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
	    nNumOper    := PR_POLIZA.Activar ( p_nIdePol
		    														   , 'D'
		    														   , cConfRedist
		    														   );
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
	      PR_OPER_POL_DET.CREAR_MOV(nNumOper, :B07_1.TipoOper, :B07_1.FecIniVig, :B07_1.FecFinVig);
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
	    Message('Trabajando... Activando Poliza Definitiva - 2', NO_ACKNOWLEDGE); 
	    -- <> Elí Segura / 06-04-2004 / Reprogramación.
	    PR_EVALUA_POLITICA.Inicializa_Autoriza_Violacion(p_nIdePol,nNumOper);
	    --
	    cFirma := PR_EVALUA_POLITICA.Obtiene_Firma_Digital(p_nIdePol, nNumOper);
	    Message('Trabajando... Activando Poliza Definitiva - 3', NO_ACKNOWLEDGE); 
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
          LR_error('FRM-MANTPOL Error en package PR_RECA_DCTO_CERT.ACTUALIZA_RECA_DCTO_CERT - '||SQLERRM);
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
	      nDummy:=Fr_Alerta('Se procederá a generarár Documentos de Cobranza,segun el Fraccionamiento Calculado,una vez Concluido este Proceso para Realizar cualquier Modificación debera Anular dichos Documentos');
	      LR_Cursor_Ocupado('Generando Documentos');
	      cEvaluarCorreo:=TRUE;--<RTC-154713>13/03/2019   Milton Lope BBVA - Mejoras al proceso de emisión de G&L P9: Envío de correos 
	      SYNCHRONIZE; 	    
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
    
      IF (cEvaluarCorreo AND cSwitchCorreo) THEN
      	IF PR.BUSCA_LVAL('PCBBVA', USER) = 'INVALIDO' AND PR.BUSCA_LVAL ('PCBBVA', 'VERMSJUSER') = '1'THEN
      		IF PR.BUSCA_LVAL ('PCBBVA', 'VERMSJUSER') = '1' THEN
      		  SET_ALERT_PROPERTY('AL_MENSAJE', ALERT_MESSAGE_TEXT, 'Correo del usuario no esta configurado, consultar a TI.');
	    			alert_button := SHOW_ALERT('AL_MENSAJE');
	    		END IF;
      	ELSE
		      cEnviaCorreo:=ACSELX.PR_GESTOR_CORREO_POLIZA_BBVA.ENVIO_CORREO_POLIZA(p_nIdePol,1);
		      IF(cEnviaCorreo=cOk) THEN
		     		SET_ALERT_PROPERTY('AL_MENSAJE',ALERT_MESSAGE_TEXT,'Correo enviado exitosamente');
		     		alert_button := SHOW_ALERT('AL_MENSAJE');
		      END IF;
		      
		      IF (cEnviaCorreo NOT IN (cNOk,cOk)) THEN
		      	IF cEnviaCorreo = 'La poliza ya fue enviada por correo para su validación.' THEN
		      		SET_ALERT_PROPERTY('AL_MENSAJE',ALERT_MESSAGE_TEXT, cEnviaCorreo);
		      	ELSIF cEnviaCorreo = 'Correo del usuario no esta configurado, consultar a TI.' THEN
		      		SET_ALERT_PROPERTY('AL_MENSAJE',ALERT_MESSAGE_TEXT, cEnviaCorreo);
		      	ELSE
		      	  SET_ALERT_PROPERTY('AL_MENSAJE',ALERT_MESSAGE_TEXT,'El correo no se ha generado correctamente, se volverá a enviar a las 00:00 hrs del día de hoy. Error: '||cEnviaCorreo);
		      	END IF;
		      	alert_button := SHOW_ALERT('AL_MENSAJE');
		      END IF;
		    END IF;
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
	    STANDARD.COMMIT;
	    
	    
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
        STANDARD.COMMIT;
        --
-- <F N3039737>

	    --<F N3020582>
	    LR_Cursor_Normal('Invocando a Distribución de Reaseguros');	  
      --<I N3030565> 15.06.2008 / Max Canal Salas /Proyecto Impresiones CCM
      BEGIN
        SELECT STSPOL,CODPROD ,NUMPOL
        INTO   cStsPol,cCodProd,nNumPol
        FROM   POLIZA P
        WHERE  P.IDEPOL = p_nIdePol;
      EXCEPTION  
      	WHEN OTHERS THEN 
      	  MESSAGE('Error al obtener Numero de poliza');
      END ;  
      nIndCNT := PR_IMP_POL_CCM_CON.VALIDA_CNT(nNumPol,cCodProd);
	    IF PR.BUSCA_LVAL('CCMLVGRL','SWICTHCCM') = 'S'  AND  nIndCNT ='N' AND PR.EXISTE_LVAL('CCMPRODX',:B01_1.CODPROD) = 'S' THEN  --<N3030980>/ 05.11.2008/Max Canal Salas /Proyecto CCM  impresion de polizas
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
        IF  cTipoOper != 'REM' THEN 
				  IF PR.BUSCA_LVAL('CCMPRODX',:B01_1.CODPROD) <> 'INVALIDO'  THEN 
            nIdImpresion := PR_IMP_POL_CCM.DEVUELVE_IND_GRUPO_IMP(cCodProd,nNumOper,p_nIdePol);
				    pl_id := CREATE_PARAMETER_LIST('PARAM_LISTA');  
				    -- 
				    ADD_PARAMETER(pl_id, 'p_nIdePol', TEXT_PARAMETER,p_nIdePol);
				    ADD_PARAMETER(pl_id, 'p_nNumOper', TEXT_PARAMETER,nNumOper);
				    ADD_PARAMETER(pl_id, 'p_nIdImpresion', TEXT_PARAMETER,nIdImpresion);
				    ADD_PARAMETER(pl_id, 'p_cNumTramite', TEXT_PARAMETER,:B02_1.cNumTramite);
				    --
		        CALL_FORM('EMI00229',HIDE,NO_REPLACE,QUERY_ONLY,pl_id);				    
				    --
				    DESTROY_PARAMETER_LIST(pl_id);  
				  END IF; 
				  GO_BLOCK('B01_1');
				  commit;
				ELSE
				  BEGIN
					  SELECT NUMFINANC 
					  INTO   nNumFinanc
					  FROM   COND_FINANCIAMIENTO F
					  WHERE  F.NUMOPER = nNumOper
					  AND    F.IDEPOL  = p_nIdePol;
						BEGIN
						  SELECT SQ_NUMCESION.NEXTVAL INTO WSESION FROM SYS.DUAL;
						END;
						pl_id := Create_Parameter_List('tmpdata'); 
					  Add_Parameter(pl_id,'PNUMFINANC',TEXT_PARAMETER,nNumFinanc);
					  Add_Parameter(pl_id,'PNUMOPER',TEXT_PARAMETER,nNumOper);
					  Add_Parameter(pl_id,'PSESION',TEXT_PARAMETER,WSESION);
					  Add_Parameter(pl_id,'PARAMFORM',TEXT_PARAMETER,'NO'); 
					  Run_Product(reports,'FIN00005.rep',SYNCHRONOUS,RUNTIME,filesystem,pl_id,null);
					  Destroy_Parameter_List( pl_id ); 
					END;
				END IF;  
			
	    END IF;
      --<F N3030565>
      
      --<I N3043352> Juan Villaorduña / 17-11-2014 / 5791
			IF nNumOper IS NOT NULL THEN
				FR_ModificaCorredor(nNumOper);
			END IF;
			--<F N3043352> Juan Villaorduña / 17-11-2014 / 5791
      	    
	    LR_Invocar_DistRea(p_nIdePol,nNumOper,0,'N');                            
	    --
	    LR_Cursor_Ocupado('Eliminando Temporales de Reaseguro.');
	    PR_GEN_REA_T.Eliminar_Rea_Temporal(p_nIdePol,NULL,NULL,NULL);
	    STANDARD.COMMIT;
	    -- Obtenemos datos actualizados de la póliza para hacer validaciones.
	    P := PR_POLIZA.Datos_Poliza(p_nIdePol);
	    IF P.StsPol = 'ACT' THEN    	
	    	-- Poner la actualizacion de inspecciones en notes
	      IF PR_POLIZA_UTIL.Maneja_Insp_Vehiculos_Pol(p_nIdePol) = kSI THEN    
	        Message('Actualizando Número de Inspección en Notes',NO_ACKNOWLEDGE);
	        FR_ACTIVAR.Actualizar_Inspeccion_Notes(p_nIdePol,:B01_1.NumPol);                      
	      END IF;
	      -- Elimina datos posibles de rehabilitación si hubiera.
	      Eliminar_Rehab;
	      STANDARD.COMMIT;
	    END IF;    
	    LR_Cursor_Ocupado('Trabajando... Eliminando temporales de Mov.Prima');
	    PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);    
	    STANDARD.COMMIT;   		
   		FR_UPDAUTORIZAREDIST(p_nIdePol, nNumOper); -- <N3041386> Claudia Yugar Ladines / 23-08-2013 / CNT-01702: Manejo de re-cálculo de la distribución de reaseguros
   ELSE
	    LR_Cursor_Ocupado('Trabajando... Reversando Activación');
	    PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);
	    STANDARD.COMMIT;
   END IF;

		LR_Cursor_Normal('Proceso ejecutado satisfactoriamente');
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
		       T.NUMOPER = nNumOper--<N3035871> NNTCOB-00015. Roberto Escudero / 23-09-2010 / Se agrega numero de operacion
		WHERE  T.IDEPOL = C.IDEPOL
			AND  T.NUMCERT = C.NUMCERT
			AND  T.STSLOG = 'PEN'
			AND  T.IDECOBERT = C.IDECOBERT
			AND  T.NUMMOD = C.NUMMOD
			AND  T.NUMFACT IS NULL;
		COMMIT;	
  END LOOP;
--<N3035871> NNTCOB-00015. Roberto Escudero / 23-09-2010 / Se mueve la baja de certificado a otro proceso 
	  -- Se devuelve la operación procesada.
	  RETURN nNumOper;
EXCEPTION
	WHEN OTHERS THEN
    LR_Cursor_Normal;
    nDummy := LR_Alerta('Error al Activar: '||SQLERRM);
    STANDARD.ROLLBACK_NR;
    PR_MOV_PRIMA.Eliminar_Mov_T(p_nIdePol);
    STANDARD.COMMIT;    
    -- Devuelve nulo en señal de error.
    RETURN TO_NUMBER(NULL);
END FN_EJECUTAR_ACTIVACION;