/*------------------------------------------------------------------------
  Nombre     : SP_CAMBIO_RESP_PAGO
  Propósito  : Procedimiento para hacer cambio de responsable de pago
  Parámetros :
                 p_nIdePol         NUMBER       Identificador de Póliza
                 p_nNumCert        NUMBER       Número de Certificado
                 p_cCodCli         VARCHAR2     Código del Cliente
                 p_cEliResPag      VARCHAR2     Eliminar todos los responsable de pago anteriores
  Retorno    :   p_cError          VARCHAR2     Retorno de resultado del proceso (OK = 0,OK / ERROR = 1,DescError)

  Log de Cambios:
    Fecha         Autor               Descripción
    22/09/2023    Robinzon Santana    Creación
 -----------------------------------------------------------------------------*/
CREATE OR REPLACE PROCEDURE SP_CAMBIO_RESP_PAGO(
  p_nIdePol    IN  Poliza.IdePol%TYPE,
  p_nNumCert   IN  Certificado.NumCert%TYPE,
  p_cCodCli    IN  Cliente.CodCli%TYPE,
  p_cEliResPag IN  VARCHAR2 DEFAULT 'N',
  p_cError     OUT VARCHAR2
) IS
  cCodEntFinan                VARCHAR2(6);
  cTipoDoc                    VARCHAR2(3);
  cCodTarjCred                VARCHAR2(4);
  cNumDocRef                  DOC_PAGO_CLIENTE.NUMDOCREF%TYPE;
  cDescTarjCred               TARJ_CRED.DescTarjCred%TYPE;
  cTipoCli                    CLIENTE.TipoCli%TYPE;
  cTipoFin                    PRODUCTO.TipoFin%TYPE;
  cIndContado                 PRODUCTO.IndContado%TYPE;
  nCantGiros                  PLAN_FINANCIAMIENTO.NroGiros%TYPE;
  nExiste                     NUMBER;

  cExisteBloqMor  VARCHAR2(1):= 'N';
  cDescbloqMor    VARCHAR2(100);
 
  TYPE RESPPAGO IS TABLE OF RESP_PAGO%ROWTYPE INDEX BY PLS_INTEGER;
  rRP RESPPAGO;
  POSICION PLS_INTEGER := 1;  

BEGIN
   
   p_cError := '0,OK';

   IF p_nIdePol IS NULL THEN
      p_cError := '1,Identificador de Póliza no puede ser nula';
      RETURN;
   END IF;
    --
   IF p_nNumCert IS NULL THEN
      p_cError := '1,Número de Certificado';
      RETURN;
   END IF;
    --
   IF p_cCodCli IS NULL THEN
      p_cError := '1,Cliente/Responsable pago no puede ser nulo';
      RETURN;
   END IF;

   BEGIN
         SELECT 1
         INTO nExiste
         FROM ASEGURADO_ADICIONAL
         WHERE IDEPOL = p_nIdePol
         AND NUMCERT  = p_nNumCert;
   EXCEPTION
         WHEN NO_DATA_FOUND THEN
            nExiste := 0;
         WHEN OTHERS THEN
               p_cError := '1,'||'Error al leer ASEGURADO_ADICIONAL '||SQLERRM;
              DBMS_OUTPUT.PUT_LINE('Error al leer ASEGURADO_ADICIONAL '||SQLERRM);
              RAISE_APPLICATION_ERROR(-20103,'Error al leer ASEGURADO_ADICIONAL '||SQLERRM);
   END;

   IF nExiste = 0 THEN
         p_cError := '1,'||'Cliente no se encuentra registrado como asegurado adicional.';
         DBMS_OUTPUT.PUT_LINE('Cliente no se encuentra registrado como asegurado adicional.');
         RAISE_APPLICATION_ERROR(-20104,'Cliente no se encuentra registrado como asegurado adicional.');
   END IF;

   IF p_cEliResPag = 'S' THEN
         DELETE RESP_PAGO RP 
         WHERE RP.IDEPOL  = p_nIdePol
         AND   RP.NUMCERT = p_nNumCert;
   END IF;

     IF p_cCodCli IS NOT NULL THEN
        
      cExisteBloqMor:= ACSELX.PR_BLOQUEO_MOROSIDAD.VALIDAR_MOROSIDAD(p_cCodCli,'S');
      IF cExisteBloqMor = 'S' THEN
        BEGIN
            SELECT NVL(B.DescBloq,'MOROSIDAD')
             INTO cDescbloqMor
            FROM BLOQUEOS B
            WHERE B.Codbloq = PR.BUSCA_LVAL('CFANMSV', 'CODBLOQ')
            AND ROWNUM = 1;
        EXCEPTION WHEN OTHERS THEN  
          cDescbloqMor:='MOROSIDAD';
        END;
           p_cCodCli := NULL;
            p_cError := '1,'||'El responsable de pago seleccionado se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')';
            DBMS_OUTPUT.PUT_LINE('El responsable de pago seleccionado se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');
            RAISE_APPLICATION_ERROR(-20100,'El responsable de pago seleccionado se encuentra Bloqueado. (Motivo: '||cDescbloqMor||')');

    END IF;
    
    DECLARE
        nDummy NUMBER;
        cMensaje VARCHAR2(500);
    BEGIN    
        p_cCodCli := PR_TERCERO.NumId_Principal('CLI', p_cCodCli, cMensaje);
        IF cMensaje IS NOT NULL THEN
              p_cError := '1,'||cMensaje; 
              DBMS_OUTPUT.PUT_LINE(cMensaje);
              RAISE_APPLICATION_ERROR(-20101,cMensaje);
        END IF;
    EXCEPTION
        WHEN OTHERS THEN
              DBMS_OUTPUT.PUT_LINE(SQLERRM);
              RAISE_APPLICATION_ERROR(-20102,SQLERRM);
    END;  
     
      BEGIN
         SELECT 1 
         INTO   nExiste
         FROM   RESP_PAGO
         WHERE  IdePol  = p_nIdePol
         AND    NumCert = p_nNumCert
         AND    CodCli  = p_cCodCli;

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            nExiste := 0;
         WHEN OTHERS THEN
               p_cError := '1,'||'Error al leer RESP_PAGO '||SQLERRM;
              DBMS_OUTPUT.PUT_LINE('Error al leer RESP_PAGO '||SQLERRM);
              RAISE_APPLICATION_ERROR(-20103,'Error al leer RESP_PAGO '||SQLERRM);
      END;
      IF nExiste = 1 THEN
           p_cError := '1,'||'Error al leer RESP_PAGO '||SQLERRM;
           DBMS_OUTPUT.PUT_LINE('Cliente ya registrado como Responsable de Pago para este Financiamiento.');
           RAISE_APPLICATION_ERROR(-20104,'Cliente ya registrado como Responsable de Pago para este Financiamiento.');
      END IF;

      BEGIN

         SELECT C.TipoCli,DECODE(C.tipocli,'P','CON001','CON002') CODPLANFRACC, 
                T.CodPais,T.CodEstado,T.CodCiudad,
                T.CodMunicipio,T.Direc,T.Telef1,T.Telef2,T.Telef3,
                T.Fax,T.Telex,T.Zip,T.Codzonapostal, T.ZonaCob, D.CodCobrador
         INTO   cTipoCli,rRP(POSICION).codplanfracc,
                rRP(POSICION).codpais,rRP(POSICION).codestado,rRP(POSICION).codciudad,
                rRP(POSICION).codmunicipio,rRP(POSICION).direc,rRP(POSICION).telef1,rRP(POSICION).telef2,rRP(POSICION).telef3,
                rRP(POSICION).fax,rRP(POSICION).telex,rRP(POSICION).zip,rRP(POSICION).codzonapostal,rRP(POSICION).codruta,rRP(POSICION).codcobrador
         FROM TERCERO T 
         INNER JOIN CLIENTE C ON C.TipoId = T.TipoId AND C.NumId = T.NumId AND C.DvId = T.DvId
         INNER JOIN DESC_RUTA D ON D.CodRuta = T.ZonaCob
         WHERE C.CodCli = p_cCodCli;

      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            DBMS_OUTPUT.PUT_LINE('No existe CLIENTE/TERCERO para el código indicado');
            RAISE_APPLICATION_ERROR(-20105,'No existe CLIENTE/TERCERO para el código indicado');
      END;

      BEGIN
         SELECT NVL(TipoFin,'OTR'),NVL(IndContado,'N')
         INTO   cTipoFin,cIndContado -- INTO   :B01.cTipoFin,cIndContado
         FROM   PRODUCTO
         WHERE  CodProd = '3001';
      EXCEPTION
          WHEN NO_DATA_FOUND THEN
              DBMS_OUTPUT.PUT_LINE('No existe Producto para Código 3001');
              RAISE_APPLICATION_ERROR(-20106,'No existe Producto para Código 3001');
      END;
      
      BEGIN
            SELECT CodPlan,ModPlan,NroGiros,TIPOFIN
            INTO   rRP(POSICION).codplanfracc , rRP(POSICION).nummodplanfracc,nCantGiros,cTipoFin -- INTO   :B01.CodPlanFracc,:B01.NumModPlanFracc,nCantGiros,cTipoFin
            FROM   PLAN_FINANCIAMIENTO
            WHERE  TipoFin = DECODE(cTipoFin,'OTR','CON',cTipoFin) -- WHERE  TipoFin = DECODE(:B01.cTipoFin,'OTR','CON',:B01.cTipoFin)
            AND    TipoCli = DECODE(cTipoCli,'J','P',cTipoCli);           
      EXCEPTION
           WHEN NO_DATA_FOUND THEN
                DBMS_OUTPUT.PUT_LINE('No existe Plan de Financiamiento para Tipo '||cTipoFin||' Tipo de Cliente '||cTipoCli);
                RAISE_APPLICATION_ERROR(-20107,'No existe Plan de Financiamiento para Tipo '||cTipoFin||' Tipo de Cliente '||cTipoCli);
           WHEN TOO_MANY_ROWS THEN
                IF PR.BUSCA_LVAL('RESPAGCN','SWITCH')!= 'S' THEN
                        DBMS_OUTPUT.PUT_LINE('Existe mas de un Plan de Financiamiento para Tipo '||cTipoFin||' Tipo de Cliente '||cTipoCli||'. Escoja uno de la lista de valores.');
                        RAISE_APPLICATION_ERROR(-20108,'Existe mas de un Plan de Financiamiento para Tipo '||cTipoFin||' Tipo de Cliente '||cTipoCli||'. Escoja uno de la lista de valores.');
                ELSE
                        nCantGiros:=1;
                END IF;
      END;
      
      IF cIndContado = 'S' THEN
            rRP(POSICION).cantcuo := 1; --:B01.CantCuo := 1;
      ELSE
            rRP(POSICION).cantcuo := nCantGiros; -- :B01.CantCuo := nCantGiros;
      END IF;
         
      IF cTipoFin IN ('CON','LET') THEN
           rRP(POSICION).tipomodcob := 'EFE'; -- :B01.TIPOMODCOB :='EFE';       
      ELSIF cTipoFin ='CCA' THEN
          rRP(POSICION).tipomodcob := 'CCA';  -- :B01.TIPOMODCOB :='CCA';          
      ELSIF cTipoFin ='CUP' THEN
           rRP(POSICION).tipomodcob := 'EFE'; -- :B01.TIPOMODCOB :='EFE';              
      END IF;
       
  END IF;
  
  BEGIN
      SELECT NumDocRef,NumDoc
      INTO   rRP(POSICION).numdocref, rRP(POSICION).numdoc -- INTO   :B01.NumDocRef,:B01.NumDoc
      FROM   DOC_PAGO_CLIENTE
      WHERE  CodCli = p_cCodCli
      AND    IndDocDef = 'S';
  EXCEPTION
      WHEN NO_DATA_FOUND THEN
         rRP(POSICION).numdocref := NULL;
         rRP(POSICION).numdoc    := NULL;
      WHEN TOO_MANY_ROWS THEN
         rRP(POSICION).numdocref := NULL;
         rRP(POSICION).numdoc    := NULL;
  END;
      -- END IF;

  IF rRP(POSICION).numdoc IS NOT NULL THEN -- IF :B01.NumDoc IS NOT NULL THEN
         BEGIN
            SELECT CodEntFinan,TipoDoc,CodTarjCred,NumDocRef
            INTO   cCodEntFinan,cTipoDoc,cCodTarjCred,cNumDocRef
            FROM   DOC_PAGO_CLIENTE
            WHERE  CodCli = p_cCodCli 
            AND    NumDoc = rRP(POSICION).numdoc -- :B01.NumDoc
            AND    NumDocRef = rRP(POSICION).numdocref; -- :B01.NumDocRef;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               cCodEntFinan:=NULL;
         END;
         BEGIN
            SELECT CodCobrador
            INTO   rRP(POSICION).codcobrador -- :B01.CodCobrador
            FROM   TARJ_CRED
            WHERE  CodEntFinan = cCodEntFinan
            AND  CodTarjCred = cCodTarjCred;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               cDescTarjCred:=NULL;
         END;

  END IF;
   
   rRP(POSICION).idepol          := p_nIdePol;
   rRP(POSICION).numcert         := p_nNumCert;
   rRP(POSICION).codcli          := p_cCodCli;
   rRP(POSICION).porcpago        := 100.00;
   rRP(POSICION).formfact        := 'PRP';

   rRP(POSICION).nummodplanfracc := NVL(rRP(POSICION).nummodplanfracc,'0');
   rRP(POSICION).diacobro        := 1;
   rRP(POSICION).numsec          := PR_RESP_PAGO.DEVUELVE_CORRELATIVO(p_nIdePol, p_nNumCert, p_cCodCli);

   BEGIN
            INSERT INTO RESP_PAGO
               (idepol,
                  numcert,
                  codcli,
                  porcpago,
                  codplanfracc,
                  nummodplanfracc,
                  codpais,
                  codestado,
                  codciudad,
                  codmunicipio,
                  direc,
                  telef1,
                  telef2,
                  telef3,
                  fax,
                  telex,
                  zip,
                  numdocref,
                  codcobrador,
                  diacobro,
                  codformcobro,
                  codruta,
                  codzonapostal,
                  codinstit,
                  fecproxvisita,
                  cantcuo,
                  numdoc,
                  formfact,
                  codagencia,
                  NumAutoriza,
                  TipoModCob,
                  NumSec)
            VALUES(rRP(POSICION).idepol,
                  rRP(POSICION).numcert,
                  rRP(POSICION).codcli,
                  rRP(POSICION).porcpago,
                  rRP(POSICION).codplanfracc,
                  rRP(POSICION).nummodplanfracc,
                  rRP(POSICION).codpais,
                  rRP(POSICION).codestado,
                  rRP(POSICION).codciudad,
                  rRP(POSICION).codmunicipio,
                  rRP(POSICION).direc,
                  rRP(POSICION).telef1,
                  rRP(POSICION).telef2,
                  rRP(POSICION).telef3,
                  rRP(POSICION).fax,
                  rRP(POSICION).telex,
                  rRP(POSICION).zip,
                  rRP(POSICION).numdocref,
                  rRP(POSICION).codcobrador,
                  rRP(POSICION).diacobro,
                  rRP(POSICION).codformcobro,
                  rRP(POSICION).codruta,
                  rRP(POSICION).codzonapostal,
                  rRP(POSICION).codinstit,
                  rRP(POSICION).fecproxvisita,
                  rRP(POSICION).cantcuo,
                  rRP(POSICION).numdoc,
                  rRP(POSICION).formfact,
                  rRP(POSICION).codagencia,
                  rRP(POSICION).NumAutoriza,
                  rRP(POSICION).TipoModCob,              
                  rRP(POSICION).NumSec);

   EXCEPTION
         WHEN OTHERS THEN
            DBMS_OUTPUT.PUT_LINE('INSERT RESP_PAGO ' || SQLERRM);
            RAISE_APPLICATION_ERROR(-20109,'INSERT RESP_PAGO '|| SQLERRM);
         END;

   END;

   BEGIN
     UPDATE CERTIFICADO
     SET CODCLI = rRP(POSICION).codcli, 
        INDDESP = 'AS'
     WHERE idepol = rRP(POSICION).idepol
     AND numcert  = rRP(POSICION).numcert;
   EXCEPTION
         WHEN OTHERS THEN
            DBMS_OUTPUT.PUT_LINE('INSERT CERTIFICADO ' || SQLERRM);
            RAISE_APPLICATION_ERROR(-20110,'INSERT CERTIFICADO '|| SQLERRM);
         END;
   END;

END SP_CAMBIO_RESP_PAGO;