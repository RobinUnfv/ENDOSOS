/*--------------------------------------------------------------------
  Nombre      : FN_VALIDA_TREC
  Proposito   : Validar el valor declarado contra monto ingreso maquinarias TREC
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
              p_nIdePol  NUMBER    Identificador de Póliza

  Fecha       Autor             Descripcion
  11/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
FUNCTION FN_VALIDA_TREC(p_nIdePol IN POLIZA.IdePol%TYPE) RETURN VARCHAR2 IS
  --
  CURSOR c_Data IS 
    SELECT DISTINCT NumCert
    FROM CERTIFICADO
    WHERE IdePol = p_nIdePol;
  --
  nNumCertC NUMBER := 0;
  nNumCertM NUMBER := 0;
  cValTrasf DATOS_PARTICULARES.ValTrasf%TYPE;
  nMtoMaqui MAQUINARIA_TREC.MtoMaqui%TYPE;
  nValTrasf NUMBER := 0;
  cValCas   VARCHAR2(1) := '0';
  
BEGIN
  -- Valida numero de certificado registrados 
  BEGIN
    SELECT COUNT(DISTINCT NumCert)
      INTO nNumCertC
      FROM CERTIFICADO
     WHERE IdePol = p_nIdePol;
  EXCEPTION
    WHEN OTHERS THEN
      nNumCertC:= 0;      
  END;
  -- Valida numero certificado registrado tabla TREC
  BEGIN
    SELECT COUNT(DISTINCT NumCert)
      INTO nNumCertM
      FROM MAQUINARIA_TREC
     WHERE IdePol = p_nIdePol
       AND IndIncluido = '1';
  EXCEPTION
    WHEN OTHERS THEN
      nNumCertM:= 0;      
  END;
  -- 
  IF NVL(nNumCertC,0) = NVL(nNumCertM,0) AND NVL(nNumCertM,0) > 0 AND NVL(nNumCertC,0) > 0 THEN
    cValCas:= '0';
  ELSE
    cValCas:= '1';
  END IF;
  --
  IF cValCas = '0' THEN
    FOR x IN c_Data LOOP
      --
      BEGIN
          SELECT SUM(ValTrasf)
          INTO cValTrasf
          FROM DATOS_PARTICULARES
          WHERE IdePol = p_nIdePol
          AND NumCert = x.NumCert
          AND CodCrit LIKE 'VALOR_DECLARADO%'
            AND CODRAMOCERT = 'TREC';
      EXCEPTION
        WHEN OTHERS THEN
          cValTrasf:= NULL;
      END;
      --
      BEGIN
          SELECT SUM(NVL(MTOMAQUI,0))
          INTO nMtoMaqui
          FROM MAQUINARIA_TREC
          WHERE IdePol = p_nIdePol
          AND NumCert = x.NumCert
          AND IndIncluido = '1';
      EXCEPTION
        WHEN OTHERS THEN
          nMtoMaqui:= NULL;
      END;
      --
      IF cValTrasf IS NOT NULL THEN
            BEGIN
                SELECT TO_NUMBER(cValTrasf)
                INTO nValTrasf
                FROM DUAL;
            EXCEPTION
                WHEN OTHERS THEN
                cValCas:= '1';
            END;
      END IF;
      --
      IF cValCas = '0' THEN
        IF NVL(nMtoMaqui,0) = NVL(nValTrasf,0) AND NVL(nValTrasf,0) > 0  AND NVL(nMtoMaqui,0) > 0 THEN
          cValCas:= '0';
        ELSE   
          cValCas:= '1';
          EXIT;
        END IF;
      END IF;
    END LOOP;
    --
    IF cValCas = '0' THEN
      RETURN '0';
    ELSE
      RETURN '1';
    END IF;   
  ELSE  
    RETURN '1';
  END IF;

END FN_VALIDA_TREC;