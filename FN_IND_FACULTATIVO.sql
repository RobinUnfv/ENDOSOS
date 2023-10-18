/*-------------------------------------------------------------------------------
  Nombre      : FN_IND_FACULTATIVO
  Proposito   : Valida si existe recalculo para el Facultativo.
  Log de Cambios 
    Fecha	          Autor			     Descripciión
  17/08/2017    Brayan Pereda   Creación <RTC-103795>
-------------------------------------------------------------------------------*/
FUNCTION FN_IND_FACULTATIVO (p_nIdePol  CERTIFICADO.IdePol%TYPE, 
                                p_nNumCert CERTIFICADO.NumCert%TYPE,
                                p_cCodRamoCert dist_rea.codramocert%TYPE,
                                p_cCodRamoRea dist_rea.codramorea%TYPE, 
                                p_nIdeRecAnt RECIBO.IdeRec%TYPE,
                                p_nIdeRec RECIBO.IdeRec%TYPE
                                ) RETURN NUMBER IS
  nExiste NUMBER(1) := 0;
BEGIN

	SELECT DISTINCT D.P1 INTO nExiste 
	FROM (
		SELECT 1 AS P1
		FROM TEMP_DIST_REA DR
		WHERE DR.IDEREC = p_nIdeRec
		AND DR.CODRAMOREA = p_cCodRamoRea
		AND DR.IDEPOL = p_nIdePol
		AND DR.NUMCERT = p_nNumCert
		AND DR.CODRAMOCERT = p_cCodRamoCert
		AND dr.NumModDistRea = 0 
		AND dr.ideCttoRea = 99999999999999
		AND EXISTS (
	      SELECT 1
	      FROM TEMP_DIST_FACULT df
	      WHERE df.IdeRec = dr.IdeRec
	        AND df.CodRamoRea = dr.CodRamoRea
	        AND df.IdePol = dr.IdePol
	        AND df.NumCert = dr.NumCert
	        AND df.CodRamoCert = dr.CodRamoCert
	        AND df.NumModDistRea =0
	        AND df.ideCttoRea = 99999999999999)
	 UNION ALL
	 SELECT 1 AS P1
		FROM DIST_REA DR
		WHERE DR.IDEREC = p_nIdeRecAnt
		AND DR.CODRAMOREA = p_cCodRamoRea
		AND DR.IDEPOL = p_nIdePol
		AND DR.NUMCERT = p_nNumCert
		AND DR.CODRAMOCERT = p_cCodRamoCert
		AND dr.NumModDistRea = 0 
		AND dr.ideCttoRea = 99999999999999
		AND EXISTS (
	    SELECT 1
	    FROM DIST_FACULT df
	    WHERE df.IdeRec = dr.IdeRec
	      AND df.CodRamoRea = dr.CodRamoRea
	      AND df.IdePol = dr.IdePol
	      AND df.NumCert = dr.NumCert
	      AND df.CodRamoCert= dr.CodRamoCert
	      AND df.NumModDistRea =0
	      AND df.ideCttoRea = 99999999999999)
	) D;
	
  RETURN (nExiste);
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN (0);
  WHEN TOO_MANY_ROWS THEN
    RETURN (1);

END FN_IND_FACULTATIVO;