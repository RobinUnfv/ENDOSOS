/*-----------------------------------------------------------------------
   Nombre : SP_AUTORIZA_VIOLACION 
   Proposito: AUTORIZA VIOLACION
   Referencia : EMI00005.FMB
   Parametro :
               p_nIdePol         Identificador de póliza.
   Log de Cambios
   Fecha        Autor               Descripcion
   -------      --------            --------------
   10/10/2023   Robinzon Santana    Creación
-----------------------------------------------------------------------------*/     
PROCEDURE SP_AUTORIZA_VIOLACION(p_nIdePol IN Poliza.IdePol%TYPE) IS
  nPassAnu  VARCHAR2(15);
  n_Alerta 	NUMBER;
  cPassWord VARCHAR2(15);
  nIdeAut	NUMBER(10);
BEGIN 
    GO_BLOCK('B01');
    FIRST_RECORD;
	LOOP
		    IF :B01.Autoriza = 'S' THEN

                CONFIRMAR_RECALCULO (:B01.IdeAut ,:B01.IdePol);
                PR_EVALUA_POLITICA.AUTORIZA_BLOQUEO (:B01.IdeAut ,:B01.IdePol ,:B01_1.Usuario);

                IF :SYSTEM.LAST_RECORD = 'TRUE' THEN 
                    n_Alerta := ALERTA('Violaciones Autorizadas...!!!');
                    EXIT; 
                ELSE
                NEXT_RECORD;
                END IF;
            ELSE        
                IF :SYSTEM.LAST_RECORD = 'TRUE' THEN 
                    EXIT; 
                END IF;
                NEXT_RECORD; 	
            END IF;
	END LOOP;
EXCEPTION
       WHEN OTHERS THEN
            n_Alerta := ALERTA(DBMS_ERROR_TEXT);
            RAISE FORM_TRIGGER_FAILURE;
END SP_AUTORIZA_VIOLACION;