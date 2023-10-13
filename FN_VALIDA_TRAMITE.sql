/*--------------------------------------------------------------------
  Nombre      : FN_VALIDA_TRAMITE
  Proposito   : Validar el tramite
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
              p_cNumTramite  POLIZA_TRAMITE.NumTramite%TYPE    NUMERO DE TRAMITE

  Fecha       Autor             Descripcion
  11/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
FUNCTION FN_VALIDA_TRAMITE(p_cNumTramite POLIZA_TRAMITE.NumTramite%TYPE)  RETURN VARCHAR2 IS
  cNombreServidor      CONEXION_NOTES.NombreServidor%TYPE;
  cRutayBDNotes        CONEXION_NOTES.RutayBDNotes%TYPE;
  cNombreVistaNotes    CONEXION_NOTES.NombreVistaNotes%TYPE;
  nError			   NUMBER := 0;
  nExiste 			   PLS_INTEGER;
  cDatoCampo           VARCHAR2(100);
  nDummy               PLS_INTEGER;
  cRetorno             VARCHAR2(1);
BEGIN
  IF p_cNumTramite IS NOT NULL THEN
			BEGIN
                SELECT NombreServidor,RutayBDNotes,NombreVistaNotes
                INTO   cNombreServidor,cRutayBDNotes,cNombreVistaNotes
                FROM   CONEXION_NOTES
                WHERE  Aplicacion = 'EVENTOS_EMISION';
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                -- LR_Error('NO ESTA CONFIGURADA LA CONEXION HACIA NOTES PARA: EVENTOS_EMISION');
                DBMS_OUTPUT.PUT_LINE('NO ESTA CONFIGURADA LA CONEXION HACIA NOTES PARA: EVENTOS_EMISION');
                RAISE_APPLICATION_ERROR(-20100,'NO ESTA CONFIGURADA LA CONEXION HACIA NOTES PARA: EVENTOS_EMISION');
            END;
			:Global.Notes := CreaObjeto;                -- Abre Sesión en Notes
			-- Lee Base de Datos
			Matriz (1,cNombreServidor);                 -- Nombre del Servidor Notes
			Matriz (2,cRutayBDNotes);                   -- Nombre de la B.D. Notes
			:Global.db := LEE_BD(:Global.Notes);
			-- Lee Vista en Notes
			Matriz (1,cNombreVistaNotes);               -- Nombre de la Vista Notes
			:Global.Vista:=LEE_TABLA(:Global.db);		
			-- Busca Documento de acuerdo a una Key
			Matriz (1,p_cNumTramite);              -- Carga la Llave de Busqueda	
			:Global.Doc:=LEE_DOCUMENTO(:Global.Vista);

			IF nvl(:Global.Doc,0) > 0 THEN               -- Si no lo Encuentra lo Crea
					Matriz (1,'ActivityOs');              -- Carga la Llave de Busqueda
					cDatoCampo:=LEE_CAMPO(:Global.Doc);
					IF cDatoCampo <> 'Emisión' THEN
						nDummy:= FR_ALERTA ('No es posible usar el trámite '||p_cNumTramite||', no está en emisión, se encuentra en '||cDatoCampo );
						--RAISE FORM_TRIGGER_FAILURE;
						FR_BORRA_TRAM_PENDIENTE (:B03_1.nIdePol,:B02_1.cNumtramite); --Borramos el trámite PQ ya está usado
						cRetorno := 'N';
					ELSE 
						cRetorno := 'S';
					END IF;
			ELSE
				nDummy:= FR_ALERTA ('No Existe el numero de trámite '||p_cNumTramite );
				cRetorno := 'N';
			END IF;
		  -- Dependiendo del Rol grabamos en respectiva base de datos
  ELSE
    cRetorno := 'S';
  END IF;

  RETURN (cRetorno);

EXCEPTION
  WHEN OTHERS THEN
     RETURN  'N';
END FN_VALIDA_TRAMITE;
