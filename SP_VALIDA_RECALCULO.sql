/*-----------------------------------------------------------------------
     Nombre : SP_VALIDA_RECALCULO 
     Proposito: Procedimiento que se utiliza para validar si el movimiento tiene recálculo
     Referencia : MODCOM.FMB
     Parametro :
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     09/10/2023   Robinzon Santana    Creación.
  -----------------------------------------------------------------------------*/     
  CREATE OR REPLACE PROCEDURE SP_VALIDA_RECALCULO(p_nIdePol IN Poliza.IdePol%TYPE) IS
	
	  cAplicaRedist	   VARCHAR2(2);
	  nExisteAutRedist VARCHAR2(1) := NULL;	
		
	BEGIN
	  --
	    BEGIN
			cAplicaRedist := 'NO';
			DBMS_OUTPUT.PUT_LINE('Verificando si aplica recálculo ...');
			cAplicaRedist := FN_CAL_APLICA_RECALCULO(p_nIdePol); -- FR_APLICA_RECALCULO(:C01.nIdePol);
		EXCEPTION
			WHEN OTHERS THEN
				cAplicaRedist := 'NO';
		END;
		--
		IF cAplicaRedist = 'SI' THEN

			DBMS_OUTPUT.PUT_LINE('Aplica recálculo');
			BEGIN
				SELECT NVL(COUNT(1),0)
				INTO nExisteAutRedist
			  	FROM AUTORIZA_REDIST
			 	WHERE IDEPOL = :C01.nIdePol
				AND NUMOPER IS NULL
				AND INDAPLIC = 'S';			    
			EXCEPTION WHEN OTHERS THEN
				nExisteAutRedist := 0;
			END;	

			IF nExisteAutRedist = 0 THEN
				DBMS_OUTPUT.PUT_LINE('Registrando en AUTORIZA_REDIST');
				INSERT INTO AUTORIZA_REDIST (IDEPOL, NUMOPER, INDAPLIC, INDREDIST ) 
				VALUES (p_nIdePol, NULL, 'S', NULL);
			ELSE
				DBMS_OUTPUT.PUT_LINE('Ya existe un registro en AUTORIZA_REDIST');
			END IF;		

		ELSE                                          		    	

			DBMS_OUTPUT.PUT_LINE('Elimina registro de AUTORIZA_REDIST');
	        DELETE AUTORIZA_REDIST
		 	WHERE IDEPOL = p_nIdePol -- :C01.nIdePol
			AND NUMOPER IS NULL
			AND INDAPLIC = 'S';

			DBMS_OUTPUT.PUT_LINE('No aplica recálculo');
	
		END IF;	 

	END SP_VALIDA_RECALCULO;