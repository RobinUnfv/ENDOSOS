  /*-----------------------------------------------------------------------
     Nombre : FN_OBTCONFREDIST 
     Proposito: Si el analista de reaseguros respondió a la validación del
     						recálculo, obtendremos la respuesta (S/N).
     						Si no aplica el recálculo, obtendremos por defecto N.
     Referencia : Formulario MANTPOL.fmb                       
     Parametro :
                 p_nIdePol         Identificador de póliza.
     Log de Cambios
     Fecha        Autor               Descripcion
     -------      --------            --------------
     15/10/2023   Robinzon Santana    Creación
  -----------------------------------------------------------------------------*/ 
 FUNCTION FN_OBTCONFREDIST ( p_nIdePol     IN    POLIZA.IdePol%TYPE ) RETURN VARCHAR2 IS  
  	cConfRedist						 VARCHAR2(1) := 'N';  	
	BEGIN		
		SELECT NVL(MAX(INDREDIST),'N')
		  INTO cConfRedist
		  FROM AUTORIZA_REDIST
		 WHERE IDEPOL = p_nIdePol 
		   AND NUMOPER IS NULL
		   AND INDAPLIC = 'S'
		   AND INDREDIST IS NOT NULL;		 
		RETURN cConfRedist;
		   
	EXCEPTION
		WHEN OTHERS THEN
			RETURN 'N';
			
	END FN_OBTCONFREDIST;