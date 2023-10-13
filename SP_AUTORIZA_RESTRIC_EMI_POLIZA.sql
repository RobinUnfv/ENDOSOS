/*------------------------------------------------------------------------
  Nombre     : SP_AUTORIZA_RESTRIC_EMI_POLIZA
  Propósito  : Procedimiento para dar permisos a la autorización de recctricciones para la emision de la póliza.
  Referencia : EMI00005.FMB
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
  Log de Cambios:
    Fecha         Autor               Descripción 
    10/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_AUTORIZA_RESTRIC_EMI_POLIZA(
  p_nIdePol      IN Poliza.IdePol%TYPE
) IS

	cUsuario		 USUARIO_AUTORIZA.CodUsu%TYPE;
	n_Alerta		 NUMBER;
	c_Existe_Usuario NUMBER(6) := 0;

    CURSOR C_AUTOVIALA IS
        SELECT * 
        FROM AUTORIZA_VIOLACION
        WHERE IDEPOL = p_nIdePol 
        AND STSAUTORIZADO = 'VAL';

    AUVIO AUTORIZA_VIOLACION%ROWTYPE;

    B01_Autoriza CHAR(1);

BEGIN

   OPEN C_AUTOVIALA;
   FETCH C_AUTOVIALA INTO AUVIO;
   WHILE C_AUTOVIALA%FOUND LOOP
      	BEGIN
            SELECT DISTINCT CodUsu
            INTO   cUsuario 
            FROM   USUARIO_AUTORIZA 
            WHERE  CodUsu	= USER
            AND    CodAut 	IN (SELECT CODAUT
                                FROM AUTORIZADOR
                                WHERE CODBLOQ = AUVIO.CODBLOQ) 
            AND    StsAut 	= 'ACT';
		EXCEPTION
			WHEN NO_DATA_FOUND THEN
                BEGIN		
                    SELECT USER
                    INTO cUsuario
                    FROM DUAL
                    WHERE EXISTS (SELECT 1
                                    FROM USUARIO_AUTORIZA 
                                    WHERE CodUsu = USER 
                                    AND  CodAut = AUVIO.CODAUT) ;
                EXCEPTION
                    WHEN OTHERS THEN
                cUsuario := 'INVALIDO';
        END ;
	    WHEN OTHERS THEN
            /* 
            n_Alerta := ALERTA('Fallo SELECT a la tabla USUARIO_AUTORIZA'||chr(13)||' Error '||SQLERRM);
            cUsuario := 'INVALIDO';
	  		:B01.Autoriza := 'N';
            RAISE FORM_TRIGGER_FAILURE;
            */
            cUsuario     := 'INVALIDO';
            B01_Autoriza := 'N';
            DBMS_OUTPUT.PUT_LINE('Fallo SELECT a la tabla USUARIO_AUTORIZA'||chr(13)||' Error '||SQLERRM);
		END;
        IF USER = cUsuario THEN
            B01_Autoriza := 'S';
        ELSE
            -- n_Alerta	:=ALERTA('Usuario No Existe..ó Inactivo Para Autorizar La Política...Verifique..!!!');
            DBMS_OUTPUT.PUT_LINE('Usuario No Existe..ó Inactivo Para Autorizar La Política...Verifique..!!!');
            B01_Autoriza := 'N';
            /*
            BEGIN
                SELECT Count(1)
                INTO c_Existe_Usuario
                FROM USUARIO_AUTORIZA 
                WHERE CodAut IN (SELECT CODAUT
                                FROM AUTORIZADOR
                                WHERE CODBLOQ= AUVIO.CODBLOQ) 
                AND StsAut 		= 'ACT';
            END;
            IF c_Existe_Usuario > 0 THEN
                    GO_BLOCK('B03');
                    LAST_RECORD;
            END IF;
            */
        END IF;

        IF p_nIdePol IS NOT NULL AND B01_Autoriza = 'S' THEN
	   	  BEGIN
                -- FR_AUTORIZA_VIOLACION;
                -- CONFIRMAR_RECALCULO (AUVIO.IDEAUT ,AUVIO.IDEPOL); .:. VALIDAR .:.
                PR_EVALUA_POLITICA.AUTORIZA_BLOQUEO (AUVIO.IDEAUT ,AUVIO.IDEPOL ,USER);
                DBMS_OUTPUT.PUT_LINE('Violaciones Autorizadas...!!!');
                -- FIN
                -- FR_UPDATE;
                DECLARE
                    nExiste_Violacion  NUMBER(1) :=0 ;
                BEGIN
                        BEGIN
                            SELECT 1
                            INTO nExiste_Violacion
                            FROM AUTORIZA_VIOLACION
                            WHERE IdePol = AUVIO.IDEPOL
                            AND   StsAutorizado = 'VAL';
                        EXCEPTION
                            WHEN NO_DATA_FOUND THEN 
                                nExiste_Violacion  := 0;
                            --Se agrega lo siguiente.
                            WHEN TOO_MANY_ROWS THEN       
                                    nExiste_Violacion  := 1;	
                            WHEN OTHERS THEN 
                                    nExiste_Violacion  := 1;
                        END;
                        --Fin de codigo agregado por Jsanchez.
                        --Ojo que al poner el estado a 2 , se indica que la poliza
                        --no tiene violacion y por tanto se actualizara.
                        IF nExiste_Violacion = 0 THEN 
                                UPDATE POLIZA 
                                SET    IndMovPolRen = '2' 
                                WHERE  IdePol       = AUVIO.IDEPOL;
                        END IF;		       
                        
                END;
                -- FIN
            EXCEPTION 
                WHEN OTHERS THEN 
                  DBMS_OUTPUT.PUT_LINE('Error'||SQLERRM);
                 RAISE_APPLICATION_ERROR(-20100,'Error'||SQLERRM);
            END;
        ELSE
            DBMS_OUTPUT.PUT_LINE('No Existen Registros Seleccionados Para Autorizar...!!!');
            RAISE_APPLICATION_ERROR(-20100,'No Existen Registros Seleccionados Para Autorizar...!!!');
        END IF;

      FETCH C_AUTOVIALA INTO AUVIO;
   END LOOP;
   CLOSE C_CLAUCERT;

END SP_AUTORIZA_RESTRIC_EMI_POLIZA;