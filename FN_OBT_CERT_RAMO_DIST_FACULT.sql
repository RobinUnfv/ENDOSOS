/*-------------------------------------------------------------------------------
  Nombre      : FR_OBT_CERT_RAMO_DIST_FACULT
  Proposito   : Obtener certificado y ramo en donde no se haya colocado detalle facultativo.
  Referencia : MODCOM.FMB
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
  Log de Cambios 
    Fecha	            Autor			     Descripción
  10/10/2023       Robinzon Santana          Creación
-------------------------------------------------------------------------------*/
FUNCTION FR_OBT_CERT_RAMO_DIST_FACULT(p_nIdePol POLIZA.IdePol%TYPE) RETURN VARCHAR2 IS

    cMensaje VARCHAR2(1000):= '';

	CURSOR DATA_FACULT_INVALIDO IS
	 	SELECT DISTINCT 'Cert:'||DR.NUMCERT || '-RamoTec:' ||DR.CODRAMOCERT||';' AS MSJE 
		FROM TEMP_DIST_REA DR
		WHERE DR.IDEPOL = p_nIdePol
		AND dr.NumModDistRea = 0 
		AND dr.ideCttoRea = 99999999999999
		AND NOT EXISTS (
	      SELECT 1
	      FROM TEMP_DIST_FACULT df
	      WHERE df.IdeRec = dr.IdeRec
	        AND df.CodRamoRea = dr.CodRamoRea
	        AND df.IdePol = dr.IdePol
	        AND df.NumCert = dr.NumCert
	        AND df.CodRamoCert = dr.CodRamoCert
	        AND df.NumModDistRea =0
	        AND df.ideCttoRea = 99999999999999);
	 
BEGIN
	
	cMensaje:= '';
	
	FOR R IN DATA_FACULT_INVALIDO LOOP
		cMensaje := cMensaje || R.MSJE;
	END LOOP;
	RETURN cMensaje;
	
EXCEPTION
  WHEN OTHERS THEN
    RETURN '';
END;