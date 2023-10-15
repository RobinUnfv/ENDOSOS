-- SP_ACTUALIZAR POLIZA
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
CREATE OR REPLACE PROCEDURE SP_ACTUALIZAR_POLIZA( 
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_cTipoOper    IN LVAL.CODLVAL%TYPE,
  p_cError       OUT VARCHAR2(3000)
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
	cExiste		     VARCHAR2(1);
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
    
    If PR.BUSCA_LVAL('POLDASBS', 'SWITCH') = 'S' AND PR.BUSCA_LVAL('PROVEHTR', :B01_1.CodProd) != 'INVALIDO' THEN 
      
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
      PR_INTERFASE_AX.SP_VALIDA_COBERT_PROV(PZ.IDEPOL, cMsjCobProv); -- PR_VALIDA_COBERT_PROV(PZ.IDEPOL, cMsjCobProv);
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
        cValTrec := PR_INTERFASE_AX.FN_VALIDA_TREC(PZ.IDEPOL); -- FR_VALIDA_TREC(PZ.IdePol);
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
        PR_INTERFASE_AX.SP_VALIDAR_DATOS;-- FR_Validar_Datos;
	      cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
        DBMS_OUTPUT.PUT_LINE(cTexto);
        IF PZ.STSPOL <> 'MOD' THEN
            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
	          PR_INTERFASE_AX.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
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
            PR_INTERFASE_AX.SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(PZ.IDEPOL,p_cTipoOper);
			    ELSE

			      SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;  
			      PR_INTERFASE_AX.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);

			    END IF;
	    	END IF;     
	    	--<F N3026404>	    	
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
	      END ;  
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
	      END ;  
	      IF  cCantOpera = 0 THEN
	        PR_INTERFASE_AX.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_VALIDAR_DATOS;
	        cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
	        DBMS_OUTPUT.PUT_LINE(cTexto);
          IF PZ.STSPOL <> 'MOD' THEN	          
	            cValidaTram := FR_VALIDA_TRAMITE(NVL(PR_POLIZA_TRAMITE.OBTENERULTIMOTRAMITE(PZ.IDEPOL),nNumTram));
	            IF cValidaTram = 'N' THEN
	            	 DBMS_OUTPUT.PUT_LINE(cValidaTram);
                 RAISE_APPLICATION_ERROR(-20106,cValidaTram);
	            END IF;	
		
	            -- Si todo está correcto se continuamos con la actualización.
	            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL); -- <N3028071> Giancarlo Ramirez - Optimizacion CNT
	            PR_INTERFASE_AX.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
	            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('F','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);  -- <N3028071> Giancarlo Ramirez - Optimizacion CNT	          	        
	        END IF;
	      ELSE
	        -- nDummy := FR_ALERTA('El trámite '||NVL(:B02_1.cNumtramite,nNumTram)|| ' con el que se esta activando la póliza pertenece a otra operación '||cTipo||' , '||PZ.CODPROD||'-'||PZ.NUMPOL ||', NumOper '|| nNumOper);
          DBMS_OUTPUT.PUT_LINE('El trámite '||NVL(:B02_1.cNumtramite,nNumTram)|| ' con el que se esta activando la póliza pertenece a otra operación '||cTipo||' , '||PZ.CODPROD||'-'||PZ.NUMPOL ||', NumOper '|| nNumOper);
			  END IF;

	    ELSE 

	   	    PR_INTERFASE_AX.SP_VALIDAR_DATOS(PZ.IDEPOL); -- FR_Validar_Datos;
		      cTexto := 'Confirma ACTIVAR la póliza con fecha: '||TO_CHAR(PR.Fecha_Movimiento(TRUNC(SYSDATE)),'DD/MM/YYYY');
          DBMS_OUTPUT.PUT_LINE(cTexto);
			    IF PZ.STSPOL <> 'MOD' THEN
	            PR_CONTROL_CONTABLE.REGISTRA_LOG_CIERRE ('I','ACTIVAR POLIZA - MANTPOL','ACTIVAR POLIZA: IDEPOL --> '||PZ.IDEPOL ,SYSDATE, SYSDATE, '01', 0, PZ.IDEPOL);
		          PR_INTERFASE_AX.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);
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
                PR_INTERFASE_AX.SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(PZ.IDEPOL,p_cTipoOper);
              ELSE

                PR_INTERFASE_AX.SP_VALIDAR_DATOS(PZ.IDEPOL); -- FR_Validar_Datos;
                PR_INTERFASE_AX.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);

              END IF;
     
	  	END IF; --F Confirmar trámite
   			    
	    END IF;   

	END IF;
 
	--<I COR103-925> Werner Tito / 19-09-2023 / [PROYECTO TECH CORE] Integración con Satelites InsPAT
	-- Activa Garantias
	PR_INTERFASE_AX.SP_ACTIVA_GARANTIAS(nStsPOL, PZ.IDEPOL); -- FR_ACTIVA_GARANTIAS(nStsPOL);
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

