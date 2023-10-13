/*------------------------------------------------------------------------
  Nombre     : FN_OBT_COBERT_CERTI
  Propósito  : Funsión para obtener los datos de cobertura de certificado
  Parámetros :
               p_nIdePol      NUMBER            Identificador de Póliza
               p_nNumCert     NUMBER            Número de Certificado
               p_cCodRamoCert VARCHAR2          Codigo de ramo asociado al certificado de una poliza.
               p_cCODCOBERT   VARCHAR2          Tipo de cobertura del certificado
  Retorno    :                COBERT_PLAN_PROD  Cobertura de Certificado

  Log de Cambios:
    Fecha         Autor               Descripción
    26/09/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
FUNCTION FN_OBT_COBERT_CERTI(p_nIdePol      IN Poliza.IdePol%TYPE,
                             p_nNumCert     IN Certificado.NumCert%TYPE,
                             p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
                             p_cCODCOBERT   IN COBERT_PLAN_PROD.CODCOBERT%TYPE ) RETURN COBERT_PLAN_PROD%ROWTYPE IS
                             
    TYPE Cursor_Cobert IS REF CURSOR;
    c_Cobert Cursor_Cobert;
    
    R   CERT_RAMO%ROWTYPE;
    CPP COBERT_PLAN_PROD%ROWTYPE;

BEGIN
    R := PR_CERT_RAMO.Datos_Cert_Ramo(p_nIdePol,p_nNumCert,p_cCodRamoCert);
    IF R.CodRamoCert IS NULL THEN
       DBMS_OUTPUT.PUT_LINE('No existe el Ramo :'||p_cCodRamoCert);
       RAISE_APPLICATION_ERROR(-20100,'No existe el Ramo '||p_cCodRamoCert);
    END IF;
    
    OPEN c_Cobert FOR
        SELECT * FROM COBERT_PLAN_PROD 
        WHERE CodProd   = '3001' 
        AND CodPlan     = R.CodPlan
        AND RevPlan     = R.RevPlan
        AND CodRamoPlan =  p_cCodRamoCert
        AND TipoCobert  = 'G'
        AND CODCOBERT   = p_cCODCOBERT
        AND (IndCobPoliza <> 'S' OR IndCobPoliza IS NULL)
        AND NOT EXISTS (SELECT 1 
                        FROM COBERT_CERT
                        WHERE IdePol     = p_nIdePol
                        AND  NumCert     = p_nNumCert
                        AND  CodRamoCert = p_cCodRamoCert
                        AND  CodCobert   = COBERT_PLAN_PROD.CodCobert )
                        AND NOT EXISTS  (SELECT 1
                                        FROM  RAMO_PLAN_PROD RPP
                                        WHERE RPP.CodProd     = '3001'
                                        AND   RPP.CodPlan     = R.CodPlan
                                        AND   RPP.RevPlan     = R.RevPlan
                                        AND   RPP.CodRamoPlan = p_cCodRamoCert
                                        AND   NVL(RPP.CodCobertDec,'*')   = COBERT_PLAN_PROD.CodCobert
                                        UNION
                                        SELECT 1
                                        FROM  RAMO_PLAN_PROD RPP
                                        WHERE RPP.CodProd     = '3001'
                                        AND   RPP.CodPlan     = R.CodPlan
                                        AND   RPP.RevPlan     = R.RevPlan
                                        AND   RPP.CodRamoPlan = p_cCodRamoCert
                                        AND   NVL(RPP.CodCobertDep,'*') = COBERT_PLAN_PROD.CodCobert
                                        UNION
                                        SELECT 1
                                        FROM  RAMO_PLAN_PROD RPP
                                        WHERE RPP.CodProd    = '3001'
                                        AND   RPP.CodPlan     = R.CodPlan
                                        AND   RPP.RevPlan     = R.RevPlan
                                        AND   RPP.CodRamoPlan = p_cCodRamoCert
                                        AND   NVL(RPP.CodCobertLiq,'*') = COBERT_PLAN_PROD.CodCobert);
 
    FETCH c_Cobert INTO CPP;
    CLOSE c_Cobert;
    --
    RETURN (CPP);
END FN_OBT_COBERT_CERTI;