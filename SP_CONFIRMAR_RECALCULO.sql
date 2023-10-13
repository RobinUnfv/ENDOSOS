/*-----------------------------------------------------------------------
   Nombre : SP_CONFIRMAR_RECALCULO 
   Proposito: En caso de existir política de recálculo, pedir confirmación.
   Parametro :
			  p_nIdePol  Numero Interno de poliza de seguros.
			  p_nIdeAut  Identificacion del bloqueo
   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/10/2023   Robinzon Santana    Creación.
-----------------------------------------------------------------------------*/     
PROCEDURE SP_CONFIRMAR_RECALCULO (
    p_nIdeAut   AUTORIZA_VIOLACION.IdeAut%TYPE,
    p_nIdePol   AUTORIZA_VIOLACION.IdePol%TYPE
    )
IS

    CCODBLOQ	AUTORIZA_VIOLACION.CODBLOQ%TYPE;
    cConfRedist	VARCHAR2(1) := NULL;
    cTextoTr    VARCHAR2(4000);
    ret_button 	NUMBER;

    n_Alerta 	NUMBER;
    cError		VARCHAR2(200);
    vError 		EXCEPTION; 

BEGIN
  
	BEGIN
		SELECT CODBLOQ 
		INTO CCODBLOQ
		FROM AUTORIZA_VIOLACION
		WHERE IdePol = p_nIdePol
        AND IdeAut   = p_nIdeAut;	
	EXCEPTION WHEN OTHERS THEN
		CCODBLOQ := NULL;		
	END;

	IF PR.BUSCA_LVAL('BLQRECAL', CCODBLOQ) != 'INVALIDO' THEN -- CON MOVIMIENTO DE VALIDACIÓN DE RECALCULO
  	
            IF PR.BUSCA_LVAL('CFREAMIX','VERSION') = '1' THEN
                -- Limpiamos alguna confirmación previa, en caso exista...
                DELETE AUTORIZA_REDIST
                WHERE IDEPOL = p_nIdePol
                AND NUMOPER IS NULL
                AND INDAPLIC = 'S' 
                AND INDREDIST IS NOT NULL; 
            END IF;
            -- Consultar si debe aplicar recálculo
            cTextoTr := 'Si elige Ok, se recalculará la distribución de reaseguros de movimientos anteriores. '|| CHR(13) ||
                                    'Si elige Cancelar, la Cuenta Corriente se generará en base al nuevo movimiento.';
            ret_button := LR_ConfBox(cTextoTr,'CONFIRMACION DE RECALCULO'); 
                --    
            IF ret_button = ALERT_BUTTON1 THEN
               cConfRedist := 'S';
            ELSIF ret_button = ALERT_BUTTON2 THEN
               cConfRedist := 'N';
            END IF;
                --
            BEGIN
                UPDATE AUTORIZA_REDIST
                SET INDREDIST = cConfRedist 
                WHERE IDEPOL = p_nIdePol
                AND NUMOPER IS NULL
                AND INDAPLIC = 'S'
                AND INDREDIST IS NULL;
            EXCEPTION 
                WHEN OTHERS THEN
                    cError := 'Error al confirmar el recálculo. cConfRedist-->'||cConfRedist;
                    RAISE vError;
            END;
    
	END IF;

EXCEPTION
  WHEN vError THEN 
		n_Alerta := ALERTA(cError||'-'||DBMS_ERROR_TEXT);
		RAISE FORM_TRIGGER_FAILURE;
	WHEN OTHERS THEN
		n_Alerta := ALERTA(DBMS_ERROR_TEXT);
		RAISE FORM_TRIGGER_FAILURE;
END;
