/*------------------------------------------------------------------------
  Nombre     : SP_ELIM_CLAU_CERT_C
  Propósito  : Procedimiento para agregar nueva cobertura con clausula C
  Parámetros :
               Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
               p_cCodRamoCert VARCHAR2  Codigo de ramo asociado al certificado de una poliza.
               p_cTipo        VARCHAR2  Tipo PAÍSES EXCLUIDOS(P) o RETROACTIVIDAD(R)
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    02/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_ELIM_CLAU_CERT_C(
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cCodRamoCert IN CERT_RAMO.CODRAMOCERT%TYPE,
  p_cTipo        IN CHAR(1),
  p_cError       OUT VARCHAR2(3000)
) IS
  cIndAsoc               NUMBER(1);
  nRowsClau              NUMBER(1) := 0;  
  cMensajeRestriccionSAS VARCHAR2(400);

    CURSOR C_CLAUCERT IS
        SELECT * FROM CLAU_CERT CC
        WHERE CC.IDEPOL    = p_nIdePol
        AND CC.NUMCERT     = p_nNumCert
        AND CC.CODRAMOCERT = p_cCodRamoCert
        AND CC.CODCLAU IN('TRA382','TRA385','TRA386','TRA257');

    CLC CLAU_CERT%ROWTYPE;

    P   POLIZA%ROWTYPE;
    R   CERT_RAMO%ROWTYPE;

DECLARE
    p_cError := '0,OK';
    -- CLAU_CERT --> CLC
   P := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||P.IdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||P.IdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||P.IdePol);
    END IF;

    R := PR_CERT_RAMO.Datos_Cert_Ramo(p_nIdePol,p_nNumCert,p_cCodRamoCert);
    IF R.CodRamoCert IS NULL THEN
       p_cError := '1,'||'No existe el Ramo :'||p_cCodRamoCert;
       DBMS_OUTPUT.PUT_LINE('No existe el Ramo :'||p_cCodRamoCert);
       RAISE_APPLICATION_ERROR(-20100,'No existe el Ramo '||p_cCodRamoCert);
    END IF;

    cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(P.IdePol);
    if not cMensajeRestriccionSAS is null then
            p_cError := '1,'||cMensajeRestriccionSAS;
            DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
            RAISE_APPLICATION_ERROR(-20100,cMensajeRestriccionSAS);  
    end if;
   
   OPEN C_CLAUCERT;
   FETCH C_CLAUCERT INTO CLC;
   WHILE C_CLAUCERT%FOUND LOOP
     IF p_cTipo = 'P' THEN
        IF CLC.CODCLAU IN('TRA382','TRA385','TRA386') THEN

            IF CLC.INDPROCLA IN ('U','I') THEN              
                    BEGIN
                        SELECT 1 
                        INTO  nRowsClau
                        FROM  CLAU_CERT CC
                        WHERE CC.Idepol  = CLC.idepol
                            AND CC.NumCert = CLC.numcert
                            AND CC.CodClau = CLC.codclau
                            AND CC.CodClau = PR_CLAU_CERT.CLAUREP_VALIDO       
                            AND CC.NumVers = PR_CLAU_CERT.VERSION_CLAUSULA(CLC.idepol, PR_CLAU_CERT.CLAUREP_VALIDO, CLC.numcert)
                            AND ROWNUM = 1;
                    EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                            nRowsClau := 0;
                    END;
                    
                    IF nRowsClau = 1 THEN     
                        INSERT INTO OPER_CLAU_GARANTIA
                        (IdePol, NumCert, CodRamoCert, CodClau, NumOper, Fecha, CodUsr)
                        VALUES (CLC.IdePol, CLC.NumCert, CLC.CodRamoCert, CLC.CodClau, 0, SYSDATE, USER);
                    END IF;

                    DELETE FROM CLAU_CERT CC
                    WHERE CC.IDEPOL = p_nIdePol
                    AND CC.NUMCERT  = p_nNumCert
                    AND CC.CODCLAU  =  CLC.CODCLAU
                    AND CC.CODRAMOCERT = p_cCodRamoCert;

            ELSE
                    IF CLC.INDPROCLA = 'O' THEN
                    -- MESSAGE('No se puede Eliminar, es una Cláusula Obligatoria');
                        p_cError := '1,'||'No se puede Eliminar, es una Cláusula Obligatoria';
                        DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Obligatoria');
                        RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Obligatoria');
                    ELSIF CLC.indprocla = 'C' THEN
                        BEGIN
                            SELECT 1
                            INTO cIndAsoc 
                            FROM CLAU_COBERT CC
                            WHERE CodProd   = P.codprod
                                AND CodPlan = R.codplan
                                AND RevPlan = R.revplan
                                AND CodClau = CLC.codclau
                                AND CodRamo = CLC.codramocert
                                AND EXISTS (SELECT 1 FROM COBERT_CERT
                                            WHERE IdePol        = CLC.IDEPOL
                                                AND NumCert     = CLC.NUMCERT
                                                AND CodRamoCert = CLC.CODRAMOCERT  
                                                AND CodCobert   = CC.CODCOBERT);
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                cIndAsoc := 0;
                        END;

                        IF cIndAsoc = 1 THEN
                            --    MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Cobertura';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                        ELSE
                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        END IF;

                    ELSIF CLC.INDPROCLA = 'E' THEN
                        --  MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Estadística';
                        DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                    
                    ELSIF CLC.INDPROCLA = 'D' THEN
                        
                        BEGIN
                            SELECT 1
                                INTO cIndAsoc           
                                FROM CLAU_VALCRIT  CV
                                WHERE CodProd   = P.codprod
                                AND CodPlan     = R.codplan
                                AND RevPlan     = R.revplan
                                AND CodRamoPlan = CLC.CodRamoCert
                                AND CodClau     = CLC.CodClau
                                AND EXISTS (SELECT 1 FROM DATOS_PARTICULARES
                                            WHERE IdePol      = CLC.IdePol
                                                AND NumCert     = CLC.NumCert
                                                AND CodRamoCert = CLC.CodRamoCert
                                                AND CodCrit     = CV.CodCrit
                                                AND CodTrasf    = CV.CodTrasf);   
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                cIndAsoc := 0;
                        END;       

                        IF cIndAsoc = 1 THEN
                            -- MESSAGE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            p_cError := '1,'||'No se puede Excluir, es una Cláusula Asociada a un Dato Particular';
                            DBMS_OUTPUT.PUT_LINE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                        ELSE
                                IF CLC.STSCLAU = 'ACT' THEN
                                    PR_CLAU_CERT.STATUS_CLAU_CERT(CLC.IdePol,CLC.NumCert,CLC.CodRamoCert,
                                                                CLC.CodClau,'EXCLU');    
                                    -- COMMIT;
                                END IF;
                        END IF;

                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;
                    
                    ELSIF CLC.STSCLAU = 'VAL' THEN
                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;
                    END IF;
            END IF;
            
        END IF;
           
     ELSIF p_cTipo = 'R' THEN
        IF CLC.CODCLAU IN('TRA382','TRA257') THEN

                IF CLC.INDPROCLA IN ('U','I') THEN              
                        BEGIN
                            SELECT 1 
                            INTO  nRowsClau
                            FROM  CLAU_CERT CC
                            WHERE CC.Idepol  = CLC.idepol
                                AND CC.NumCert = CLC.numcert
                                AND CC.CodClau = CLC.codclau
                                AND CC.CodClau = PR_CLAU_CERT.CLAUREP_VALIDO       
                                AND CC.NumVers = PR_CLAU_CERT.VERSION_CLAUSULA(CLC.idepol, PR_CLAU_CERT.CLAUREP_VALIDO, CLC.numcert)
                                AND ROWNUM = 1;
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN
                                nRowsClau := 0;
                        END;
                        
                        IF nRowsClau = 1 THEN     
                            INSERT INTO OPER_CLAU_GARANTIA
                            (IdePol, NumCert, CodRamoCert, CodClau, NumOper, Fecha, CodUsr)
                            VALUES (CLC.IdePol, CLC.NumCert, CLC.CodRamoCert, CLC.CodClau, 0, SYSDATE, USER);
                        END IF;

                        DELETE FROM CLAU_CERT CC
                        WHERE CC.IDEPOL = p_nIdePol
                        AND CC.NUMCERT  = p_nNumCert
                        AND CC.CODCLAU  =  CLC.CODCLAU
                        AND CC.CODRAMOCERT = p_cCodRamoCert;

                ELSE
                        IF CLC.INDPROCLA = 'O' THEN
                        -- MESSAGE('No se puede Eliminar, es una Cláusula Obligatoria');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Obligatoria';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Obligatoria');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Obligatoria');
                        ELSIF CLC.indprocla = 'C' THEN
                            BEGIN
                                SELECT 1
                                INTO cIndAsoc 
                                FROM CLAU_COBERT CC
                                WHERE CodProd   = P.codprod
                                    AND CodPlan = R.codplan
                                    AND RevPlan = R.revplan
                                    AND CodClau = CLC.codclau
                                    AND CodRamo = CLC.codramocert
                                    AND EXISTS (SELECT 1 FROM COBERT_CERT
                                                WHERE IdePol        = CLC.IDEPOL
                                                    AND NumCert     = CLC.NUMCERT
                                                    AND CodRamoCert = CLC.CODRAMOCERT  
                                                    AND CodCobert   = CC.CODCOBERT);
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    cIndAsoc := 0;
                            END;

                            IF cIndAsoc = 1 THEN
                                --    MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                                p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Cobertura';
                                DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                                RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Cobertura');
                            ELSE
                                DELETE FROM CLAU_CERT CC
                                WHERE CC.IDEPOL = p_nIdePol
                                AND CC.NUMCERT  = p_nNumCert
                                AND CC.CODCLAU  =  CLC.CODCLAU
                                AND CC.CODRAMOCERT = p_cCodRamoCert;
                            END IF;

                        ELSIF CLC.INDPROCLA = 'E' THEN
                            --  MESSAGE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                            p_cError := '1,'||'No se puede Eliminar, es una Cláusula Asociada a una Estadística';
                            DBMS_OUTPUT.PUT_LINE('No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                            RAISE_APPLICATION_ERROR(-20100,'No se puede Eliminar, es una Cláusula Asociada a una Estadística');
                        
                        ELSIF CLC.INDPROCLA = 'D' THEN
                            
                            BEGIN
                                SELECT 1
                                    INTO cIndAsoc           
                                    FROM CLAU_VALCRIT  CV
                                    WHERE CodProd   = P.codprod
                                    AND CodPlan     = R.codplan
                                    AND RevPlan     = R.revplan
                                    AND CodRamoPlan = CLC.CodRamoCert
                                    AND CodClau     = CLC.CodClau
                                    AND EXISTS (SELECT 1 FROM DATOS_PARTICULARES
                                                WHERE IdePol      = CLC.IdePol
                                                    AND NumCert     = CLC.NumCert
                                                    AND CodRamoCert = CLC.CodRamoCert
                                                    AND CodCrit     = CV.CodCrit
                                                    AND CodTrasf    = CV.CodTrasf);   
                            EXCEPTION
                                WHEN NO_DATA_FOUND THEN
                                    cIndAsoc := 0;
                            END;       

                            IF cIndAsoc = 1 THEN
                                -- MESSAGE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                                p_cError := '1,'||'No se puede Excluir, es una Cláusula Asociada a un Dato Particular';
                                DBMS_OUTPUT.PUT_LINE('No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                                RAISE_APPLICATION_ERROR(-20100,'No se puede Excluir, es una Cláusula Asociada a un Dato Particular');
                            ELSE
                                    IF CLC.STSCLAU = 'ACT' THEN
                                        PR_CLAU_CERT.STATUS_CLAU_CERT(CLC.IdePol,CLC.NumCert,CLC.CodRamoCert,
                                                                    CLC.CodClau,'EXCLU');    
                                        -- COMMIT;
                                    END IF;
                            END IF;

                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        
                        ELSIF CLC.STSCLAU = 'VAL' THEN
                            DELETE FROM CLAU_CERT CC
                            WHERE CC.IDEPOL = p_nIdePol
                            AND CC.NUMCERT  = p_nNumCert
                            AND CC.CODCLAU  =  CLC.CODCLAU
                            AND CC.CODRAMOCERT = p_cCodRamoCert;
                        END IF;
                END IF;
            
        END IF;   
     END IF;
     FETCH C_CLAUCERT INTO CLC; 
   END LOOP;
   CLOSE C_CLAUCERT;

END SP_ELIMINAR_CLAU_CERT_C;