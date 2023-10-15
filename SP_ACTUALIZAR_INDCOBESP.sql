/*------------------------------------------------------------------------
  Nombre     : SP_ACTUALIZAR_INDCOBESP
  Propósito  : 
  Referencia : Formulario MANTPOL.fmb
  Parámetros :
               p_nNumOper  NUMBER    Numero de operación

  Log de Cambios:
    Fecha         Autor               Descripción
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
PROCEDURE SP_ACTUALIZAR_INDCOBESP(p_nNumOper   IN   OPER_POL.NumOper%TYPE) IS
  CURSOR c_Cobert_Cert IS
    SELECT IdeCobert
    FROM   MOD_COBERT
    WHERE  IdeMovPrima IN (SELECT IdeMovPrima 
                           FROM   RECIBO 
                           WHERE  IdeOp = p_nNumOper
                          );  
BEGIN
  FOR C IN c_Cobert_Cert LOOP
    UPDATE MOD_COBERT
    SET    IndCobEsp = 'S'
    WHERE  IdeCobert = C.IdeCobert;
  END LOOP;
END SP_ACTUALIZAR_INDCOBESP;