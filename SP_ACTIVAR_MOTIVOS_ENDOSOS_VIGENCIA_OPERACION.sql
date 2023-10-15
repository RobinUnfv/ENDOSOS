/*------------------------------------------------------------------------
  Nombre     : SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA
  Propósito  : ACTIVAR MOTIVOS ENDOSOS VIGENCIA OPERACIO
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_cTipoOper    VARCHAR   Tipo motivo del endoso

  Referencia : MANTPOL.FMB

  Log de Cambios:
    Fecha         Autor               Descripción 
    14/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA(
  p_nIdePol   IN Poliza.IdePol%TYPE,
  p_cTipoOper IN LVAL.CODLVAL%TYPE
) IS

	nCantReg  NUMBER := 0;
	v_NumOper Oper_Pol.NumOper%TYPE;	
	nStsPol   POLIZA.StsPol%TYPE;
    PZ        POLIZA%ROWTYPE;
    
BEGIN
    BEGIN
      SELECT 1 
      FROM   lval l 
      WHERE  l.Tipolval = 'TIPOEND'
      AND l.Codlval = p_cTipoOper;
    EXCEPTION
      WHEN OTHERS THEN
		DBMS_OUTPUT.PUT_LINE('El motivo de endoso no valido.');
        RAISE_APPLICATION_ERROR(-20100,'El motivo de endoso no valido.');
    END;

    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;
	
    IF p_cTipoOper IS NOT NULL THEN

        PR_INTERFASE_AX.SP_VALIDAR_DATOS(PZ.IDEPOL);-- FR_Validar_Datos;	
        PR_INTERFASE_AX.SP_ACTIVAR_ACTUALIZAR_POLIZA(PZ.IDEPOL); -- FR_ACTIVAR.Actualizar_Poliza(:B01_1.IdePol);  

        --<I RTC 316343> David Yupanqui / 24-02-2023 / [PROYECTO TECH CORE] Mejora en Registro y Validación de Maquinaria TREC
        IF PR.BUSCA_LVAL('MAQTREC', PZ.CODPOL)='1' THEN
            BEGIN
                SELECT COUNT(1) 
                INTO nCantReg
                FROM Maquinaria_Trec
                WHERE IdePol = PZ.IDEPOL;
            EXCEPTION WHEN OTHERS THEN
                nCantReg := 0;
            END;
            IF nCantReg > 0 THEN
                BEGIN
                    SELECT max(NumOper)
                    INTO v_NumOper
                    FROM Oper_Pol
                    WHERE IdePol = PZ.IDEPOL
                    AND TipoOp <> ('EMI')
                    AND NVL(INDANUL,'N') = 'N';
                EXCEPTION WHEN OTHERS THEN
                    v_NumOper := NULL;
                END;
                IF v_NumOper IS NOT NULL THEN
                    BEGIN
                        UPDATE Maquinaria_Trec
                        SET NumOper = v_NumOper
                        WHERE IdePol = PZ.IDEPOL
                        AND TO_CHAR(FecOper,'DD/MM/YYYY') = TO_CHAR(SYSDATE,'DD/MM/YYYY');
                        -- STANDARD.COMMIT;
                    EXCEPTION WHEN OTHERS THEN
                        NULL;
                    END;
                END IF;
            END IF;
        END IF;

        PR_INTERFASE_AX.SP_ACTIVA_GARANTIAS(PZ.STSPOL, PZ.IDEPOL);  -- FR_ACTIVA_GARANTIAS(nStsPOL);

        --<I RTC 316472> David Yupanqui / 27-04-2023 / [PROYECTO TECH CORE] Armado Automático del Anexo 7
        if pr.busca_lval('SLIP_AUT',PZ.CODPROD) <> 'INVALIDO' then
            acselx.pr_slip_auto.texto_prima(PZ.IDEPOL);
        end if;
        --<F RTC 316472>
    
    END IF;
END SP_ACTI_MOT_ENDOSOS_VIGEN_OPERA; -- Mejora en Registro y Validación de Maquinaria TREC