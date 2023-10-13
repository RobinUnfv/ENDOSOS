 /*-------------------------------------------------------------------------
  Nombre      : SP_VALIDA_COBERT_PROV
  Proposito   : Verificar si la poliza se encuentra con cobertura provisional
  Referencia : Formulario MANTPOL.fmb - Tribu Empresas Bloqueo en la Emisión Vehicular que tengan Coberturas Provisionales Activas
  Log de Cambios
    Fecha          Autor            Descripcion
  11/10/2023    Robinzon Santana	 Creacion
---------------------------------------------------------------------------*/
PROCEDURE SP_VALIDA_COBERT_PROV(p_nIdePol     IN POLIZA.Idepol%TYPE,
                                p_cMsjCobProv OUT VARCHAR2 ) IS

  cNumPlaca  VARCHAR2(40)  := NULL;
  cNumSerie	 VARCHAR2(100) := NULL;
  cNumMotor  VARCHAR2(100) := NULL;
  nSinPlaca  NUMBER:= 0;
  cMsjError  VARCHAR2(1000) := NULL;
  
  CURSOR C_Certificado IS
    SELECT CE.Numcert
		  FROM CERTIFICADO CE
		 WHERE CE.IDEPOL = p_nIdePol
		   AND EXISTS (SELECT 1
                       FROM LVAL LV
                       WHERE LV.Tipolval = 'ESACPOCP'
                        AND LV.Codlval LIKE 'CDP%' 
                        AND LV.DESCRIP = CE.Stscert);

BEGIN
  FOR rC IN C_Certificado LOOP
	 	BEGIN  
            SELECT DISTINCT DP.Valtrasf
            INTO cNumSerie
            FROM POLIZA PO, 
                CERTIFICADO CE, 
                CERT_RAMO CR, 
                DATOS_PARTICULARES DP
            WHERE PO.IdePol      = p_nIdePol
            AND PO.IdePol      = CE.IdePol
            AND CE.IdePol      = CR.IdePol
            AND CE.NumCert     = CR.NumCert
            AND CR.IdePol      = DP.IdePol
            AND CR.NumCert     = DP.NumCert
            AND CR.CodRamoCert = DP.CodRamoCert
            AND CE.Numcert     = rC.Numcert
            AND DP.CodCrit     = 'NUMERO_DE_SERIE'
                AND EXISTS (SELECT 1
                            FROM LVAL LV
                            WHERE LV.Tipolval = 'ESACPOCP'
                            AND LV.Codlval LIKE 'CDP%' 
                            AND LV.DESCRIP = CE.Stscert
                            )
            AND EXISTS (SELECT 1
                        FROM LVAL LV
                        WHERE LV.Tipolval = 'PROVEHCP'
                        AND LV.Codlval = PO.Codprod);

	 	EXCEPTION
	 		WHEN TOO_MANY_ROWS THEN
	 			DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Serie para el certificado.');
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Serie para el certificado.');
		    WHEN OTHERS THEN
		 		DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Número de Serie: '||SQLCODE||' - '||SQLERRM);
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Número de Serie: '||SQLCODE||' - '||SQLERRM);
	    END;
	  BEGIN  
	      SELECT DISTINCT DP.Valtrasf
	      INTO cNumMotor
	      FROM POLIZA PO, 
	           CERTIFICADO CE, 
	           CERT_RAMO CR, 
	           DATOS_PARTICULARES DP
	     WHERE PO.IdePol      = p_nIdePol
	       AND PO.IdePol      = CE.IdePol
	       AND CE.IdePol      = CR.IdePol
	       AND CE.NumCert     = CR.NumCert
	       AND CR.IdePol      = DP.IdePol
	       AND CR.NumCert     = DP.NumCert
	       AND CR.CodRamoCert = DP.CodRamoCert
	       AND CE.Numcert     = rC.Numcert
		     AND EXISTS (SELECT 1
                         FROM LVAL LV
                         WHERE LV.Tipolval = 'ESACPOCP'
                         AND LV.Codlval LIKE 'CDP%' 
                         AND LV.DESCRIP = CE.Stscert)
	       AND DP.CodCrit     = 'NUMERO_DE_MOTOR'
	       AND EXISTS (SELECT 1
	                   FROM LVAL LV
	                   WHERE LV.Tipolval = 'PROVEHCP'
	                   AND LV.Codlval = PO.Codprod);
		
	  EXCEPTION
	 		WHEN TOO_MANY_ROWS THEN
	 			DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Motor para el certificado.');
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Se tiene más de un Número de Motor para el certificado.');
		    WHEN OTHERS THEN
		 		DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Número de Motor: '||SQLCODE||' - '||SQLERRM);
                RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Número de Motor: '||SQLCODE||' - '||SQLERRM);

	  END;
   
	  -- Placa
	  BEGIN  
	    SELECT DISTINCT DP.Valtrasf
	      INTO cNumPlaca
	      FROM POLIZA PO, 
	           CERTIFICADO CE, 
	           CERT_RAMO CR, 
	           DATOS_PARTICULARES DP
	     WHERE PO.IdePol      = p_nIdePol
	       AND PO.IdePol      = CE.IdePol
	       AND CE.IdePol      = CR.IdePol
	       AND CE.NumCert     = CR.NumCert
	       AND CR.IdePol      = DP.IdePol
	       AND CR.NumCert     = DP.NumCert
	       AND CR.CodRamoCert = DP.CodRamoCert
	       AND CE.Numcert     = rC.Numcert
		     AND EXISTS (
                      SELECT 1
                       FROM LVAL LV
                      WHERE LV.Tipolval = 'ESACPOCP'
                        AND LV.Codlval LIKE 'CDP%' 
                        AND LV.DESCRIP = CE.Stscert
                        )
	       AND DP.CodCrit = 'PLACA_DE_RODAJE'
	       AND EXISTS (SELECT 1
	                     FROM LVAL LV
	                    WHERE LV.Tipolval = 'PROVEHCP'
	                      AND LV.Codlval = PO.Codprod);

	  EXCEPTION
	 		WHEN TOO_MANY_ROWS THEN
	 		  DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Se tiene más de una Placa de Rodaje para el certificado.');
              RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Se tiene más de una Placa de Rodaje para el certificado.');
		  WHEN OTHERS THEN
		 	  DBMS_OUTPUT.PUT_LINE('Error en PR_VALIDA_COBERT_PROV - Placa de Rodaje: '||SQLCODE||' - '||SQLERRM);
              RAISE_APPLICATION_ERROR(-20100,'Error en PR_VALIDA_COBERT_PROV - Placa de Rodaje: '||SQLCODE||' - '||SQLERRM);
	  END;

    BEGIN
		  PR_CARGA_MASIVA_DATPAR.SP_VALIDA_COBERT_PROV(cNumSerie,
		                                               cNumMotor,
		                                               cNumPlaca,
		                                               cMsjError);
    EXCEPTION
    	WHEN OTHERS THEN
    	  cMsjError := 'Error en Valida Cobertura - '||SQLERRM;
    END;
		IF cMsjError IS NOT NULL THEN
			p_cMsjCobProv  := cMsjError;
		ELSE
		  p_cMsjCobProv  := NULL;
		END IF;
    EXIT WHEN p_cMsjCobProv IS NOT NULL;
  END LOOP;
END PR_VALIDA_COBERT_PROV;