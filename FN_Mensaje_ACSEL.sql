/*--------------------------------------------------------------------
  Nombre      : FN_MENSAJE_ACSEL
  Proposito   : Enviar el mensaje de error
  Referencia  : Formulario MANTPOL.fmb
  Parametros  :
               p_cTipo    VARCHAR2 Tipo de mensaje
               p_nCodigo  NUMBER  Codigo del mensaje
               p_cParam1  VARCHAR2 Parametro 1
               p_cParam2  VARCHAR2 Parametro 2
               p_cParam3  VARCHAR2 Parametro 2

  Fecha       Autor             Descripcion
  14/10/2023  Robinzon Santana  Creacion
--------------------------------------------------------------------*/
FUNCTION FN_MENSAJE_ACSEL(p_cTipo   VARCHAR2, 
                           p_nCodigo NUMBER, 
                           p_cParam1 VARCHAR2,       
                           p_cParam2 VARCHAR2,
                           p_cParam3 VARCHAR2) RETURN VARCHAR2 IS                                   
  cMensaje VARCHAR2 (500);                          
BEGIN                                                  
  --
  BEGIN                                                
    SELECT Mensaje                                     
    INTO   cMensaje                                    
    FROM   msg_acsel
    WHERE  TipoMsg = p_cTipo                          
      AND  CodMsg  = p_nCodigo;                          
  EXCEPTION                                            
    WHEN NO_DATA_FOUND THEN                            
      IF p_cParam1 IS NOT NULL THEN                       
        cMensaje := p_cParam1;                            
      ELSE                                             
        cMensaje := 'Mensaje desconocido ';            
      END IF;                                          
  END;                                                 
  --
  IF p_cParam1 != '  ' THEN                               
    cMensaje := RTRIM(cMensaje);                                
    cMensaje := REPLACE(cMensaje,'&1',p_cParam1);                  
  ELSE                                                          
    cMensaje := REPLACE(cMensaje,'&1','');                      
  END IF;                                                       
  --
  IF p_cParam2 != '  ' THEN                               
    cMensaje := RTRIM(cMensaje);                                
    cMensaje := REPLACE(cMensaje,'&2',p_cParam2);                  
  ELSE                                                          
    cMensaje := REPLACE(cMensaje,'&2','');                      
  END IF;                                                       
  --
  IF p_cParam3 != '  ' THEN                                        
    cMensaje := RTRIM(cMensaje);                               
    cMensaje := REPLACE(cMensaje,'&3',p_cParam3);                  
  ELSE                                                          
    cMensaje := REPLACE(cMensaje,'&3','');                      
  END IF;                                                       
  --
  cMensaje := p_cTipo||'-'||TO_CHAR(p_nCodigo)||': '||RTRIM(cMensaje);         
  RETURN (cMensaje); 

END FN_MENSAJE_ACSEL;