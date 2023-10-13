/*------------------------------------------------------------------------
  Nombre     : SP_INCLUIR_CERTI_CLAU_C
  Propósito  : Procedimiento para cambiar de estado al certificado con clausula C
  Parámetros :
               Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    02/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_INCLUIR_CERTI_CLAU_C( 
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cError       OUT VARCHAR2(3000)
) IS
  bExisteSelec BOOLEAN;
  n_IDLogLote  NUMBER;
  WSESION      NUMBER;
  cEmiPro      INTEGER;
  cEsMsv  	   INTEGER;
  nExiste 	   NUMBER := 0;
  nVal         NUMBER := 0;

  cMensajeRestriccionSAS VARCHAR2(400);

  P   POLIZA%ROWTYPE;
  cStsCert  CERTIFICADO.STSCERT%TYPE;

DECLARE
       
       BEGIN
        SELECT C.Stscert
        INTO cStsCert
        FROM CERTIFICADO C
        WHERE C.IDEPOL = p_nIdePol
        AND C.NUMCERT  = p_nNumCert;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        p_cError := '1,'||'La póliza o el certificado no valido';
        DBMS_OUTPUT.PUT_LINE('La póliza o el certificado no valido');
        RAISE_APPLICATION_ERROR(-20100,'La póliza o el certificado no valido');
    END;

    P := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IDEPOL IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||P.IdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||P.IdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||P.IdePol);
    END IF;

    cMensajeRestriccionSAS:= pr_poliza_util.RESTRINGE_ENDOSO_POLIZA(p_nIdePol);
  if not cMensajeRestriccionSAS is null then
        p_cError := '1,'||cMensajeRestriccionSAS;
        DBMS_OUTPUT.PUT_LINE(cMensajeRestriccionSAS);
        RAISE_APPLICATION_ERROR(-20100,cMensajeRestriccionSAS);   
  end if;

    -- PR_VAL_BLOQ_SOAT(nExiste);    
    BEGIN
        SELECT NVL(MAX(1),0)
        INTO nVal
        FROM DUAL
        WHERE EXISTS ( SELECT 1
                       FROM POLIZA PO
                       WHERE PO.CodProd = P.CODPROD -- :C01_1.CCODPROD
                            AND PO.NumPol  = P.NUMPOL -- :C01_1.NNUMPOL
                            AND EXISTS ( SELECT 1
                                        FROM POL_BLOQUEO_ENDOSO B
                                        WHERE PO.IdePol   = B.IdePol
                                        AND B.indbloqEnd = 'S')
                        ) ;
    EXCEPTION
        WHEN OTHERS THEN
            nVal := 0;
    END;
    nExiste := nVal;

    -- FIN  PR_VAL_BLOQ_SOAT(nExiste);

    IF NVL(nExiste ,0) = 1 THEN
        p_cError := '1,'||'Opción Bloqueada por impresión de Padrón de Certificados';
        DBMS_OUTPUT.PUT_LINE('Opción Bloqueada por impresión de Padrón de Certificados');
        RAISE_APPLICATION_ERROR(-20100,'Opción Bloqueada por impresión de Padrón de Certificados');
    ELSE
        -- FR_Validar_Coaseguro(:B01_1.IdePol);
        DECLARE
            kListaEstadoModificable CONSTANT VARCHAR2(8) := 'INC,MOD';
            kSI                     CONSTANT VARCHAR2(1) := 'S';
          cExiste                 VARCHAR2(1);
        BEGIN
            cExiste := PR_MOD_DIST_COA.Existe_Mod_Dist_Coa_Estado(p_nIdePol,kListaEstadoModificable);
            IF (cExiste = kSI AND P.STSPOL = 'MOD') THEN
                cExiste := PR_POLIZA_UTIL.Existe_Oper_Pol(p_nIdePol);
                IF cExiste = kSI THEN
                    -- R_Error('Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado');  
                    p_cError := '1,'||'Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado';
                    DBMS_OUTPUT.PUT_LINE('Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado');
                    RAISE_APPLICATION_ERROR(-20100,'Se han realizado Movimientos de Coaseguro. No es posible Incluir el Certificado');
                END IF; 
            END IF; 
        END;
        -- FIN
        cEmiPro := 0;
      cEsMsv  := 0;
        BEGIN
      SELECT 1
      INTO cEsMsv
      FROM DUAL
      WHERE EXISTS (SELECT 1
            FROM VNNT_CONTROL_POLIZA PE
            WHERE PE.Idepol = p_nIdePol);
    EXCEPTION
      WHEN OTHERS THEN
      cEsMsv := 0;
    END;
        IF cEsMsv = 1 THEN
      BEGIN
        SELECT 1
        INTO cEmiPro
        FROM DUAL 
        WHERE EXISTS (SELECT 1
                FROM VNNT_CONTROL_LEGACY CL
                WHERE (CL.NUMCARGA) IN
                  (SELECT DISTINCT NUMCARGA
                    FROM DATOS_EXT_POLIZA
                    WHERE (IDEPOL, NUMPOL, NUMCERT) IN
                      (SELECT PV.IDEPOL, PV.NUMPOL, C.NUMCERT
                       FROM POLIZA PV, CERTIFICADO C
                       WHERE PV.IDEPOL = C.IDEPOL
                       AND PV.Idepol = p_nIdePol))
                AND EXISTS (SELECT 1 
                      FROM VNNT_LOG_CONTROL_LEGACY CY 
                      WHERE CY.Id_Transf = CL.ID_TRANSF 
                      AND CY.Proceso = 'EMI' 
                      AND CY.Resultado IN ('PRO')));
      EXCEPTION
        WHEN OTHERS THEN
        cEmiPro := 0;
      END;
      IF cEmiPro = 1 THEN
        -- nButton := LR_Alerta('La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.');
                p_cError := '1,'||'La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.';
                DBMS_OUTPUT.PUT_LINE('La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.');
                RAISE_APPLICATION_ERROR(-20100,'La póliza se encuentra en Proceso de Emisión Automática. Favor de ejecutar la inclusión cuando finalice.');
      ELSE
        SELECT SQ_NUMCESION.NEXTVAL INTO WSESION FROM SYS.DUAL;
      END IF;
    END IF;

        IF cEmiPro = 0 THEN
            
            IF cStsCert = 'VAL' THEN
              PR_CERTIFICADO.Incluir(p_nIdePol,p_nNumCert);
              
              PR_POL_CERT_ELECT.Incluir(p_nIdePol,p_nNumCert);
        
              IF cEsMsv = 1 AND cEmiPro = 0 THEN
                INSERT INTO TMP_CERTIF VALUES (WSESION,p_nIdePol,p_nNumCert);
              END IF;
            ELSE
                      p_cError := '1,'||'No se incluirá el certificado '||p_nNumCert||', no se encuentra en estado Válido.';
                      DBMS_OUTPUT.PUT_LINE('No se incluirá el certificado '||p_nNumCert||', no se encuentra en estado Válido.');
                      RAISE_APPLICATION_ERROR(-20100,'No se incluirá el certificado '||p_nNumCert||', no se encuentra en estado Válido.');
            END IF;

        END IF;

    END IF;

END SP_INCLUIR_CERTI_CLAU_C;