
 /*--------------------------------------------------------------------
  Nombre      : SP_VALIDAR_DATOS
  Proposito   : Validar datos para la activacion que no exista duplicida de datos particulares.
                Sol si son nuevas , son renovaciones , inclusiones , rehabilitacioines pero no 
                prorrogas
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
              p_nIdePol  NUMBER    Identificador de Póliza

  Fecha       Autor             Descripcion
  11/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
PROCEDURE SP_VALIDAR_DATOS(p_nIdePol IN POLIZA.Idepol%TYPE) IS

  kHoy       CONSTANT DATE := TRUNC(SYSDATE);
  cError     VARCHAR2(300) := NULL;
  nExiste    PLS_INTEGER;
  bValida    BOOLEAN;

  PZ   POLIZA%ROWTYPE;
BEGIN
	-- Se valida si las fechas equivalentes han sido actualizadas.
	-- :C00.dFecOper := PR.Fecha_Movimiento(kHoy);
    IF PR.Fecha_Movimiento(kHoy) IS NULL THEN
            DBMS_OUTPUT.PUT_LINE('Favor actualizar la tabla de fechas de equivalencias');
            RAISE_APPLICATION_ERROR(-20100,'Favor actualizar la tabla de fechas de equivalencias');
    END IF;	
	--Verificar si se valida datos particulares
	--asumo que no
	bValida := FALSE;

    PZ := PR_Poliza.Datos_Poliza(p_nIdePol);
    IF PZ.IDEPOL IS NULL THEN
       DBMS_OUTPUT.PUT_LINE('No existe la Póliza '||p_nIdePol);
       RAISE_APPLICATION_ERROR(-20100,'No existe la Póliza '||p_nIdePol);
    END IF;

	--se validaran las nuevas
	IF (PZ.StsPol IN ('VAL','INC','ANU')) THEN 
		bValida := TRUE; 
	ELSE
		nExiste := 0 ; 
		BEGIN --1.verifico si hay inclusiones
			SELECT 1
			INTO nExiste
			FROM DUAL 
			WHERE EXISTS (SELECT 1
			              FROM Certificado
			              WHERE IdePol = PZ.IDEPOL
                      AND StsCert IN ('INC','MOD') 
                      AND FecExc IS NULL);
		EXCEPTION
			WHEN NO_DATA_FOUND THEN
			  nExiste := 0 ;
			WHEN OTHERS THEN 
			   -- LR_Error(SQLERRM);
               DBMS_OUTPUT.PUT_LINE(SQLERRM);
               RAISE_APPLICATION_ERROR(-20100,SQLERRM);
    END ; --1.
		IF nExiste = 1 THEN
			bValida := TRUE ; 
		END IF ;         
	END IF ;
  IF bValida = TRUE THEN
  	cError := PR_POLIZA.ValidaDatoParticular(PZ.IDEPOL);
  END IF;
  
  IF cError IS NOT NULL THEN 
        DBMS_OUTPUT.PUT_LINE(cError);
        RAISE_APPLICATION_ERROR(-20100,cError);
  END IF;

END SP_VALIDAR_DATOS;  
