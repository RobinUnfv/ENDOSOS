/*------------------------------------------------------------------------
  Nombre     : SP_ACTU_POLI_CON_CLAU_C
  Propósito  : Procedimiento para actualizar la póliza con certificado con clausula C
  Referencia : Formulario MANTPOL.fmb
  Parámetros :
               p_nIdePol      NUMBER    Identificador de Póliza
               p_nNumCert     NUMBER    Número de Certificado
  Retorno    : p_cError       VARCHAR2  Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    02/10/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_ACTU_POLI_CON_CLAU_C( 
  p_nIdePol      IN Poliza.IdePol%TYPE,
  p_nNumCert     IN Certificado.NumCert%TYPE,
  p_cError       OUT VARCHAR2
) IS
  POLI  POLIZA%ROWTYPE;
  -- FR_Validar_Datos
  kHoy CONSTANT DATE := TRUNC(SYSDATE);
  nExiste       PLS_INTEGER ;
  cError     VARCHAR2(300) := NULL;
  bValida       BOOLEAN ;
  -- FIN
DECLARE
    p_cError := '0,OK';
    
    POLI := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF P.IdePol IS NULL THEN
       p_cError := '1,'||'No existe la Póliza '||POLI.IdePol;
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||POLI.IdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||POLI.IdePol);
    END IF;

    -- FR_Validar_Datos

	IF PR.Fecha_Movimiento(kHoy) IS NULL THEN
        p_cError := '1,'||'Favor actualizar la tabla de fechas de equivalencias';
        DBMS_OUTPUT.PUT_LINE('Favor actualizar la tabla de fechas de equivalencias');
        RAISE_APPLICATION_ERROR(-20101,'Favor actualizar la tabla de fechas de equivalencias');
    END IF;

    bValida := FALSE ;

    IF (POLI.STSPOL IN ('VAL','INC','ANU')) THEN 
		bValida := TRUE; 
	ELSE
		nExiste := 0 ; 
		BEGIN --1.verifico si hay inclusiones
			SELECT 1
			INTO nExiste
			FROM DUAL 
			WHERE EXISTS (SELECT 1
			              FROM Certificado
			              WHERE IdePol = POLI.IDEPOL
                      AND StsCert IN ('INC','MOD') 
                      AND FecExc IS NULL);
		EXCEPTION
			WHEN NO_DATA_FOUND THEN
			  nExiste := 0 ;
			WHEN OTHERS THEN  -- 
			    p_cError := '1,'||SQLERRM;
                DBMS_OUTPUT.PUT_LINE(SQLERRM);
                RAISE_APPLICATION_ERROR(-20102,SQLERRM);
        END ; --1.
		IF nExiste = 1 THEN
			bValida := TRUE ; 
		END IF ;         
	END IF ;
    IF bValida = TRUE THEN
  	   cError := PR_POLIZA.ValidaDatoParticular(:B01_1.IdePol);
    END IF ;
    IF cError IS NOT NULL THEN 
		p_cError := '1,'||cError;
        DBMS_OUTPUT.PUT_LINE(cError);
        RAISE_APPLICATION_ERROR(-20103,cError);
    END IF;  
     

    -- FIN



END SP_ACTU_POLI_CON_CLAU_C;
  