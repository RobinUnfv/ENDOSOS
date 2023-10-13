/*------------------------------------------------------------------------
  Nombre     : SP_ACTU_OPERA_MOT_ENDOSO
  Propósito  : Procedimiento para actualizar operacion de motivo endoso
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
               p_cCodRamoCert VARCHAR2  Codigo de ramo asociado al certificado de una poliza.
  Referencia : MODCOM.FMB              
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción 
    09/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_ACTU_OPERA_MOT_ENDOSO(
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
  p_cError       OUT VARCHAR2
) IS

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

DECLARE
    p_cError := '0,OK';
    
    POL := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||p_nIdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;

    IF PR_CNT_UTIL.POLIZA_CNF_PRIMAPACTADA(p_nIdePol) = 'S' THEN
      BEGIN
        PR_BBVA_UTIL.ProcesaPrimaPactada(p_nIdePol);
      EXCEPTION
        WHEN OTHERS THEN
          -- LR_Error('Error ' || SQLERRM);
          p_cError := '1,'||'Error ' || SQLERRM;
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
	  RPST.IdePol := P.IdePol;
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
            WHERE A.IDEPOL   = p_nIdePol  --:C01.nIdePol
            AND B.IDECTTOREA = A.IDECTTOREA;
    
    BEGIN
    
        -- IF :C01.FecIni = :C01.FecPro THEN '30-JUN-2004'
        IF P.FecIniVig = TO_DATE('30-JUN-2004','DD/MON/YYYY') THEN
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
      SP_VALIDA_RECALCULO_RM(p_nIdePol);
      PR_GEN_REA_UTIL_RM.VALIDAR_POLITICAS(p_nIdePol);
    ELSE
      -- FR_VALIDA_RECALCULO;
      SP_VALIDA_RECALCULO(p_nIdePol);
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
          SP_AUTORIZA_RESTRIC_EMI_POLIZA(p_nIdePol);
	  END IF;

  BEGIN
		nEstadoValGeneral := PR_GEN_REA_UTIL_RM.VALIDAR_FACULTATIVO(p_nIdePol); -- 1:OK, 0:HAY ERRORES
	EXCEPTION
		WHEN OTHERS THEN
	 		nEstadoValGeneral := 0;
      DBMS_OUTPUT.PUT_LINE('Error: ' 
	 		          || DBMS_ERROR_TEXT
                || 'Para : '
                || FR_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol)
                ||' <Informe al Area de Reaseguro>');
      RAISE_APPLICATION_ERROR(-20100,'Error: ' 
	 		          || DBMS_ERROR_TEXT
                || 'Para : '
                || FR_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol)
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