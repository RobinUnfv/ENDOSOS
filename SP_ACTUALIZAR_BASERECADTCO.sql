/*------------------------------------------------------------------------
  Nombre     : SP_ACTUALIZAR_BASERECADTCO
  Propósito  : 
  Referencia : Formulario MANTPOL.fmb
  Parámetros :
               p_nNumOper  NUMBER    Numero de operación

  Log de Cambios:
    Fecha         Autor               Descripción
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTUALIZAR_BASERECADTCO(p_nNumOper IN OPER_POL.NumOper%TYPE) IS
	  CURSOR c_Reca_Dcto_Esp  IS
	    SELECT CodRecaDcto,TipoRecaDcto,NumCert,IdePol
	    FROM   DET_OPER_POL
	    WHERE  NumOper = p_nNumOper;
	
	BEGIN
	  FOR R IN c_Reca_Dcto_Esp LOOP
	    UPDATE RECA_DCTO_CERTIF
	    SET    BaseRecaDcto = 'P'
	    WHERE  CodRecaDcto  = R.CodRecaDcto
	      AND  TipoRecaDcto = R.TipoRecaDcto
	      AND  IdePol = R.IdePol
	      AND  NumCert = R.NumCert;
	  END LOOP;
END SP_ACTUALIZAR_BASERECADTCO;