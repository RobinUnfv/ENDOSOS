/*-------------------------------------------------------------------------------
  Nombre      : FN_EXISTE_FACULTATIVO
  Proposito   : Valida si el Certificado es Facultativo y tiene Distribución
  Log de Cambios 
    Fecha	          Autor			     Descripciión
  31/10/2011    Noema Paredes   Creación <N3037805>
-------------------------------------------------------------------------------*/
FUNCTION FN_EXISTE_FACULTATIVO (p_nIdePol  CERTIFICADO.IdePol%TYPE, 
                                p_nNumCert CERTIFICADO.NumCert%TYPE) RETURN NUMBER IS
  nExiste NUMBER(1);
BEGIN
  SELECT 1
  INTO nExiste
  FROM TEMP_DIST_REA dr, CERTIFICADO c
  WHERE dr.IdePol = c.IdePol
    AND dr.NumCert = c.NumCert
    AND dr.IdePol = p_nIdePol
    AND dr.NumCert = p_nNumCert
    AND dr.NumModDistRea =0      
    AND dr.ideCttoRea = 99999999999999
    AND c.StsCert IN('INC','MOD')
    AND EXISTS (
      SELECT 1
      FROM TEMP_DIST_FACULT df
      WHERE df.IdePol = dr.IdePol
        AND df.NumCert = dr.NumCert
        AND df.CodRamoCert= dr.CodRamoCert
        AND df.CodRamoRea= dr.CodRamoRea
        AND df.IdeRec= dr.IdeRec
        AND df.NumModDistRea =0
        AND df.ideCttoRea = 99999999999999);

  RETURN (nExiste);
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN (0);
  WHEN TOO_MANY_ROWS THEN
    RETURN (1);

END FN_EXISTE_FACULTATIVO;