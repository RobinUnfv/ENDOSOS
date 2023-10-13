DECLARE  --<I N3037156> German Silva / 27-05-2011
	nNumCertMax			CERTIFICADO.NumCert%TYPE;
BEGIN
  LR_Cursor_Ocupado;
  
    :C05.CODCLI := LPAD(:C05.CODCLI,14,'0');
  
	  BEGIN
	    SELECT  MAX(NumCert)
	    INTO nNumCertMax
	    FROM Certificado
	    WHERE IdePol = :B01_1.IdePol;
    EXCEPTION
  	  WHEN OTHERS THEN
  	    nNumCertMax := 9999999999;  -- No aplica actualizaciones
    END;  
    
    IF :C05.nNroCopias = 0 OR :C05.nNroCopias IS NULL THEN
      GO_ITEM('C05.nNroCopias');    	
      LR_ERROR('Debe indicar Nro de Copias');	
    END IF;
    
    IF :C05.dFecIniVIg IS NULL THEN
    	GO_ITEM('C05.dFecIniVIg');
    	LR_ERROR('Debe ingresar Feha Inicio de Vigencia');
    END IF;
    
    IF :C05.dFecFinVig IS NULL THEN
    	GO_ITEM('C05.dFecFinVIg');    	
    	LR_ERROR('Debe ingresar Feha Fin de Vigencia');
    END IF;
    
    IF :C05.CODCLI IS NULL THEN
    	GO_ITEM('C05.CODCLI');    	
    	LR_ERROR('Debe seleccionar un Asegurado');
    END IF;
    
    IF :C05.cNombAseg ='NO EXISTE' THEN
    	GO_ITEM('C05.CODCLI');    	    	
    	LR_ERROR('Asegurado NO EXISTE');
    END IF;
    	--<F N3037156> German Silva / 27-05-2011
    	
  BEGIN
    PR_CERTIFICADO.DUPLICA_CERTIFICADO(:B01_1.IdePol,:B01_1.NumCert,:C05.nNroCopias,
                                     :B01_1.IdePol,NULL,
                                     :C05.dFecIniVig, :C05.dFecFinVig, :C05.CodCli);  -- <N3037156> German Silva / 27-05-2011
  EXCEPTION
    WHEN OTHERS THEN
      LR_Rollback('Error en PR_CERTIFICADO.DUPLICA_CERTIFICADO - '||SQLERRM);
  END;
  
  BEGIN  --<I N3037156> German Silva / 27-05-2011
    FR_ACTUALIZAR_RELACIONADOS(:B01_1.IdePol,:B01_1.NumCert,:C05.dFecIniVig, :C05.dFecFinVig, nNumCertMax); 
  EXCEPTION
  	WHEN OTHERS THEN
  	  LR_Rollback('Error en FR_ACTUALIZAR_RELACIONADOS - '||SQLERRM);
  END;  --<F N3037156> German Silva / 27-05-2011
  
  STANDARD.COMMIT;
  LR_Cursor_Normal;  
  /* se comentó porque se movió lineas arriba
  EXCEPTION
    WHEN OTHERS THEN
      LR_Rollback('Error en PR_CERTIFICADO.DUPLICA_CERTIFICADO - '||SQLERRM);
  END;*/
  LR_Refrescar_Bloque('B01_1');
END;