/*------------------------------------------------------------------------
  Nombre     : SP_ELIM_COBER_CERTI
  Propósito  : Procedimiento para eliminar cobertura del certificado por tipo

  Parámetros :
               Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
               p_cCodRamoCert VARCHAR2  Codigo de ramo asociado al certificado de una poliza.
               p_cTipo        VARCHAR2  Eliminar por RETROACTIVIDAD(R) y PAÍSES EXCLUIDOS(P)
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    27/09/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_ELIM_COBER_CERTI(
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
  p_cTipo        IN VARCHAR2(2);
  p_cCodProd     IN Poliza.codprod%TYPE,
  p_cCodPlan     IN Certificado.CodPlan%TYPE,
  p_cRevPlan     IN Certificado.RevPlan%TYPE,
  p_cError       OUT VARCHAR2
) IS

  n_Existe NUMBER;  

  P COBERT_PLAN_PROD%ROWTYPE;
	cMensajeRestriccionSAS VARCHAR2(400);

  CURSOR C_COBERTCERT IS
      SELECT * FROM COBERT_CERT CC
      WHERE CC.IDEPOL    = p_nIdePol
      AND CC.NUMCERT     = p_nNumCert
      AND CC.CODRAMOCERT = p_cCodRamoCert;
  
  CP COBERT_CERT%ROWTYPE;

BEGIN
   cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
   if not cMensajeRestriccionSAS is null then
      p_cError := '1,'||cMensajeRestriccionSAS;
      DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
      RAISE_APPLICATION_ERROR(-20104,cMensajeRestriccionSAS);
	end if;

  OPEN C_COBERTCERT;
  FETCH C_COBERTCERT INTO CP;
  WHILE C_COBERTCERT%FOUND LOOP
      IF CP.StsCobert IN ('VAL','INC') THEN
          IF CP.StsCobert = 'VAL' THEN
            BEGIN
              SELECT NVL(MAX(1),0)
              INTO   n_Existe
              FROM   DUAL
              WHERE  EXISTS ( SELECT 1 
                              FROM   MOD_COBERT
                              WHERE  IdeCobert  = CP.IdeCobert
                              AND    StsModCobert NOT IN ('VAL'));	     	                
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                n_Existe := 0;
            END;
          END IF;

          IF n_Existe = 1 THEN
            p_cError := '1,'||'Hay registros de modificaciones de la cobertura ,NO debe ser eliminada...';
            DBMS_OUTPUT.PUT_LINE('Hay registros de modificaciones de la cobertura ,NO debe ser eliminada...');
            RAISE_APPLICATION_ERROR(-20104,'Hay registros de modificaciones de la cobertura ,NO debe ser eliminada...');
          END IF;

          P := PR_Cobert_Plan_Prod.Datos_Cobert_Plan_Prod('3001',CP.CodPlan,CP.RevPlan,CP.CodRamoCert,CP.CodCobert);
          IF P.CodCobert IS NOT NULL AND P.IndCobertOblig = 'S' THEN
              p_cError := '1,'||' La Cobertura '||CP.CodCobert||' esta CONFIGURADA como obligatoria por lo cual NO puede ser eliminada...';
              DBMS_OUTPUT.PUT_LINE(' La Cobertura '||CP.CodCobert||' esta CONFIGURADA como obligatoria por lo cual NO puede ser eliminada...');
              RAISE_APPLICATION_ERROR(-20104,' La Cobertura '||CP.CodCobert||' esta CONFIGURADA como obligatoria por lo cual NO puede ser eliminada...');
          END IF;

          IF CP.CODCOBERT = 'TR10' OR ( CP.CODCOBERT IN('TR13','TR14') AND p_cTipo = 'P' ) THEN
             DELETE FROM COBERT_CERT
             WHERE IDECOBERT = CP.IDECOBERT
             AND StsModCobert IN('INC','VAL');

             PR_CERT_RAMO.GENERAR_CLAUSULAS_ASOCIADAS(CP.IdePol,CP.NumCert,p_cCodProd,
			                                         p_cCodPlan,p_cRevPlan,CP.CodRamoCert,
			 	                                       CP.FecIniValid,CP.FecFinValid);
          END IF;

      END IF;

      FETCH C_COBERTCERT INTO CP;
  END LOOP;
  CLOSE C_COBERTCERT;

  IF p_cTipo = 'P' THEN
     DELETE FROM COBERT_CERT
     WHERE CC.IDEPOL    = p_nIdePol
     AND CC.NUMCERT     = p_nNumCert
     AND CC.CODCOBERT NOT IN('TR12')
     AND CC.CODRAMOCERT = p_cCodRamoCert;
  END IF;


END SP_ELIM_COBER_CERTI;