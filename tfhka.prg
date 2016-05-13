/*
 * Harbour Project source code:
 * the factory hka guide implementation
 *
 * Copyright 2016, Riztan Gutierrez <riztan [at] gmail [dot] com>
 * www - http://gtxbase.org
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */
/*
 * This require telepathy library of harbour 3.2
 *
 * Implementación inicial para Bixolon SRP-350 (Panamá)
 */


//#require "hbtpathy"

#include "tfhka.ch"
#include "telepath.ch" 
#include "hbclass.ch"
#include "common.ch"
//#ifdef __HARBOUR__
//   #include "hbcompat.ch"
//#endif

CREATE CLASS TFHKA

   CLASSDATA nPort      INIT 0
   CLASSDATA cPortName  INIT ""
   CLASSDATA nBaud      INIT 9600
   CLASSDATA nData      INIT 8
   CLASSDATA cParity    INIT "E"
   CLASSDATA nStop      INIT 1

   CLASSDATA nTimeOut   INIT _TPTIME_OUT_

   CLASSDATA nError     INIT 0
   CLASSDATA cError     INIT ""

   CLASSDATA cOS        INIT ""

   CLASSDATA hStatus    INIT Hash()

   CLASSDATA hS1        INIT Hash()
   CLASSDATA hS2        INIT Hash()
   CLASSDATA hS3        INIT Hash()
   CLASSDATA hS4        INIT Hash()
   CLASSDATA hS5        INIT Hash()


   METHOD New( nPort, nBaud, nData, cParity, nStop, cPortName, cError )

   METHOD SetPortName( nPort, cPortName, cError )  
   METHOD Check() VIRTUAL

   METHOD SetTimeOut( nTime )       INLINE IIF( ValType(nTime)="N" .and. nTime>=0 .and. nTime<=5, ;
                                                (::nTimeOut := nTime, .t.), .f.)  

   METHOD GetPortName()             INLINE ::cPortName
   METHOD Enquirement( cError )  // Envia ENQ y Muestra el status.
   METHOD Status()                  INLINE ::Enquirement()

   METHOD SendCmd( cCommand, cError )

   METHOD ReportX( cError )         INLINE ::SendCmd( "I0X", @cError )
   METHOD Resumen( cError )         INLINE ::SendCmd( "I1X", @cError )
   METHOD ReportZ( cError )         INLINE ::SendCmd( "I0Z", @cError )

   METHOD GetStatus( nStatus, cError )

   METHOD End()   INLINE  tp_Close( ::nPort )

ENDCLASS


METHOD NEW( nPort, nBaud, nData, cParity, nStop, cPortName, cError ) CLASS TFHKA
   local cOS := lower( OS() )
   local nError

   default nBaud to 9600, nData to 8, cParity to "E", nStop to 1

   cError := ""

   if ( "linux" $ cOS )
      ::cOS := "LINUX"
   elseif ( "windows" $ cOS )
      ::cOS := "WIN"
   else
      ::cOS := "OTHER"
   endif

   if ::cOS == "LINUX" 
      if !hb_IsString(cPortName)
         cError := "No se ha indicado ruta del puerto a utilizar. Ejemplo: '/dev/ttyS0'"
      else
         ::SetPortName( nPort, cPortName, @cError )
      endif
   elseif ::cOS == "WIN"
      //-- ver como verificar si el puerto es válido en windows.
   endif

   ::nBaud   := nBaud
   ::nData   := nData
   ::cParity := cParity
   ::nStop   := nStop

   nError := tp_Open( ::nPort,,, ::nBaud, ::nData, ::cParity, ::nStop )

   if nError != 0
      Do Case 
         Case nError == TE_NOPORT
              cError := "Puerto incorrecto"
         Case nError == TE_PARAM
              cError := "Parametro incorrecto"
         Case nError == TE_CONFL
              cError := "Conflicto en el puerto. "+hb_eol()
              cError += "Revise la conexion y/o si el dispositivo esta encendido"
      EndCase

//   else
   endif
   
   HSet( ::hStatus, "info", "Status y Error de la Impresora" )
   HSet( ::hStatus, "lTranFiscal" , { "title"=>"Transaccion Fiscal en curso"      , "value"=>""} )
   HSet( ::hStatus, "lTranNFiscal", { "title"=>"Transaccion No Fiscal en curso"   , "value"=>""} )
   HSet( ::hStatus, "lBufferLLeno", { "title"=>"Buffer lleno"                     , "value"=>""} )
   HSet( ::hStatus, "lMemFisAgot" , { "title"=>"Memoria Fiscal LLena"             , "value"=>""} )
   HSet( ::hStatus, "lMemPrxAgot" , { "title"=>"Memoria Fiscal Proxima a Agotarse", "value"=>""} )
   HSet( ::hStatus, "lModoFiscal" , { "title"=>"Modo Fiscal"                      , "value"=>""} )

   HSet( ::hStatus, "lErrPapel"    , { "title"=>"Error Papel"       , "value"=>""} )
   HSet( ::hStatus, "lErrImpresora", { "title"=>"Error en Impresora", "value"=>""} )
   HSet( ::hStatus, "lErrImpresor" , { "title"=>"Error Impresor"    , "value"=>""} )
   HSet( ::hStatus, "lErrGaveta"   , { "title"=>"Error Gaveta"      , "value"=>""} )
   HSet( ::hStatus, "lErrCritico"  , { "title"=>"Error Critico"     , "value"=>""} )

   
   HSet( ::hS1, "info", "Parametros de la impresora" )
   HSet( ::hS1, "hCajero"        , { "title"=>"Cajero Asignado"                    , "value"=>""} )
   HSet( ::hS1, "hVtasDiarias"   , { "title"=>"Total de Ventas Diarias"            , "value"=>""} )
   HSet( ::hS1, "hUltFactura"    , { "title"=>"Ultima Factura"                     , "value"=>""} )
   HSet( ::hS1, "hFacEmitidas"   , { "title"=>"Nro. Facturas Emitidas (dia)"       , "value"=>""} )
   HSet( ::hS1, "hUltNCredito"   , { "title"=>"Ultima Nota de Credito"             , "value"=>""} )
   HSet( ::hS1, "hNdCEmitidas"   , { "title"=>"Nro. N. de Credito Emitidas (dia)"  , "value"=>""} )
   HSet( ::hS1, "hUltNDebito"    , { "title"=>"Ultima Nota de Debito"              , "value"=>""} )
   HSet( ::hS1, "hNdDEmitidas"   , { "title"=>"Nro. N. de Debito Emitidas (dia) "  , "value"=>""} )
   HSet( ::hS1, "hUltNDNFis"     , { "title"=>"Ultimo Doc. No Fiscal"              , "value"=>""} )
   HSet( ::hS1, "hDNFEmitidos"   , { "title"=>"Nro. Doc. No Fiscales (dia) "       , "value"=>""} )
   HSet( ::hS1, "hContCierres"   , { "title"=>"Contador de cierres diarios"        , "value"=>""} )
   HSet( ::hS1, "hContRepMFiscal", { "title"=>"Contador de Reportes de Mem. Fiscal", "value"=>""} )
   HSet( ::hS1, "hRUC"           , { "title"=>"RUC"                                , "value"=>""} )
   HSet( ::hS1, "hDV"            , { "title"=>"DV"                                 , "value"=>""} )
   HSet( ::hS1, "hSerial"        , { "title"=>"Serial"                             , "value"=>""} )
   HSet( ::hS1, "hHoraImpFis"    , { "title"=>"Hora actual en la impresora"        , "value"=>""} )
   HSet( ::hS1, "hFechaImpFis"   , { "title"=>"Fecha actual en la impresora"       , "value"=>""} )


   HSet( ::hS2, "info", "Estado de la Factura, NC, ND en curso" )
   HSet( ::hS2, "hSbTBaseImp"   , { "title"=>"SubTotal de Bases Imponibles" , "value"=>""} )
   HSet( ::hS2, "hSbTImpuesto"  , { "title"=>"SubTotal de Impuesto"         , "value"=>""} )
   HSet( ::hS2, "hDataDummy1"   , { "title"=>"Data Dummy 1"                 , "value"=>""} )
   HSet( ::hS2, "hDataDummy2"   , { "title"=>"Data Dummy 2"                 , "value"=>""} )
   HSet( ::hS2, "hPorPagar"     , { "title"=>"Monto Por Pagar"              , "value"=>""} )
   HSet( ::hS2, "hNroPagos"     , { "title"=>"Nro. de pagos realizados"     , "value"=>""} )
   HSet( ::hS2, "hCondicion"    , { "title"=>"Condicion"                    , "value"=>""} )
                 /* 0 - No transaccion, 1 - En pago factura, 2 - En NDC, 3 - En NDD */
   
   HSet( ::hS3, "info", "Tasas y Flags")
   HSet( ::hS3, "hTipoTasa1"    , { "title"=>"Tipo de tasa 1"               , "value"=>""} )
   HSet( ::hS3, "hValorTasa1"   , { "title"=>"Valor de la tasa 1"           , "value"=>""} )
   HSet( ::hS3, "hTipoTasa2"    , { "title"=>"Tipo de tasa 2"               , "value"=>""} )
   HSet( ::hS3, "hValorTasa2"   , { "title"=>"Valor de la tasa 2"           , "value"=>""} )
   HSet( ::hS3, "hTipoTasa3"    , { "title"=>"Tipo de tasa 3"               , "value"=>""} )
   HSet( ::hS3, "hValorTasa3"   , { "title"=>"Valor de la tasa 3"           , "value"=>""} )
   HSet( ::hS3, "hFlags"        , { "title"=>"Flags del Sistema"            , "value"=>""} )

   HSet( ::hS4, "info", "Descriptores de Medios de Pago")
   HSet( ::hS4, "hMedPago1"     , { "title"=>"Medio de Pago 1" , "value"=>""} )
   HSet( ::hS4, "hMedPago2"     , { "title"=>"Medio de Pago 2" , "value"=>""} )
   HSet( ::hS4, "hMedPago3"     , { "title"=>"Medio de Pago 3" , "value"=>""} )
   HSet( ::hS4, "hMedPago4"     , { "title"=>"Medio de Pago 4" , "value"=>""} )
   HSet( ::hS4, "hMedPago5"     , { "title"=>"Medio de Pago 5" , "value"=>""} )
   HSet( ::hS4, "hMedPago6"     , { "title"=>"Medio de Pago 6" , "value"=>""} )
   HSet( ::hS4, "hMedPago7"     , { "title"=>"Medio de Pago 7" , "value"=>""} )
   HSet( ::hS4, "hMedPago8"     , { "title"=>"Medio de Pago 8" , "value"=>""} )
   HSet( ::hS4, "hMedPago9"     , { "title"=>"Medio de Pago 9" , "value"=>""} )
   HSet( ::hS4, "hMedPago10"    , { "title"=>"Medio de Pago 10", "value"=>""} )
   HSet( ::hS4, "hMedPago11"    , { "title"=>"Medio de Pago 11", "value"=>""} )
   HSet( ::hS4, "hMedPago12"    , { "title"=>"Medio de Pago 12", "value"=>""} )
   HSet( ::hS4, "hMedPago13"    , { "title"=>"Medio de Pago 13", "value"=>""} )
   HSet( ::hS4, "hMedPago14"    , { "title"=>"Medio de Pago 14", "value"=>""} )
   HSet( ::hS4, "hMedPago15"    , { "title"=>"Medio de Pago 15", "value"=>""} )
   HSet( ::hS4, "hMedPago16"    , { "title"=>"Medio de Pago 16", "value"=>""} )

   HSet( ::hS5, "info", "Estado de la memoria de auditoria")
   HSet( ::hS5, "hRUC"       , { "title"=>"RUC"                              , "value"=>""} )
   HSet( ::hS5, "hDV"        , { "title"=>"DV"                               , "value"=>""} )
   HSet( ::hS5, "hSerial"    , { "title"=>"Serial"                           , "value"=>""} )
   HSet( ::hS5, "hNumMemAud" , { "title"=>"Numero de la Memoria de Auditoria", "value"=>""} )
   HSet( ::hS5, "hTotalMem"  , { "title"=>"Capacidad de la Memoria en MB"    , "value"=>""} )
   HSet( ::hS5, "hLeftMem"   , { "title"=>"MB Disponible en la Memoria"      , "value"=>""} )
   HSet( ::hS5, "hNumDocReg" , { "title"=>"Nro. Documentos Registrados"      , "value"=>""} )

RETURN SELF



/** Asigna un puerto (nombre) al Nro de puerto.
 *  Necesario en sistemas GNU/Linux
 */
METHOD SetPortName( nPort, cPortName, cError )  CLASS TFHKA
   local lResult := .f.

   cError := ""

   if !hb_IsString( cPortName )
      cError := "No hay puerto especificado."
      return lResult
   endif

   if hb_comSetDevice( nPort, cPortName )
      ::nPort := nPort
      ::cPortName := cPortName
      lResult := .t.
   else
      cError := "No ha sido posible asignar el puerto ["+cPortName+"]"
   endif

return lResult



/**  Hace lectura del estado de la impresora fiscal 
 *   ( envia señal ENQ ) 
 *   Genera y retorna un Hash con los datos.
 */
METHOD ENQUIREMENT( cError )  CLASS TFHKA
  local cResp, lError:=.f.
  local nSTS1, nSTS2

  cError := "Recibido valor incorrecto"
  
  cResp := SendSignal( ::nPort, Hex_ENQ, @lError )
  if lError
     return ""
  endif

  // -- Revisamos los 2 bytes de estado (STS1 y STS2). 
  //    Trama recibida:  STX STS1 STS2 ETX LRC 
  nSTS1 := ASC( SubStr( cResp, 2, 1 ) )
  nSTS2 := ASC( SubStr( cResp, 3, 1 ) )

   HSet( ::hS5, "hNumDocReg" , { "title"=>"Nro. Documentos Registrados"      , "value"=>""} )

  ::hStatus["lTranFiscal"  ]["value"] := hb_BitTest( nSTS1, 0 )
  ::hStatus["lTranNFiscal" ]["value"] := hb_BitTest( nSTS1, 1 )
  ::hStatus["lBufferLLeno" ]["value"] := hb_BitTest( nSTS1, 2 )
  ::hStatus["lMemFisAgot"  ]["value"] := hb_BitTest( nSTS1, 3 )
  ::hStatus["lMemPrxAgot"  ]["value"] := hb_BitTest( nSTS1, 4 )
  ::hStatus["lModoFiscal"  ]["value"] := hb_BitTest( nSTS1, 5 )

  ::hStatus["lErrPapel"    ]["value"] := hb_BitTest( nSTS2, 0 )
  ::hStatus["lErrImpresora"]["value"] := hb_BitTest( nSTS2, 1 )
  ::hStatus["lErrImpresor" ]["value"] := hb_BitTest( nSTS2, 2 )
  ::hStatus["lErrGaveta"   ]["value"] := hb_BitTest( nSTS2, 3 )
  ::hStatus["lErrCritico"  ]["value"] := hb_BitTest( nSTS2, 4 )
/*
  hStatus["lTranFiscal" ] := hb_BitTest( nSTS1, 0 )
  hStatus["lTranNFiscal"] := hb_BitTest( nSTS1, 1 )
  hStatus["lBufferLLeno"] := hb_BitTest( nSTS1, 2 )
  hStatus["lMemFisAgot" ] := hb_BitTest( nSTS1, 3 )
  hStatus["lMemPrxAgot" ] := hb_BitTest( nSTS1, 4 )
  hStatus["lModoFiscal" ] := hb_BitTest( nSTS1, 5 )

  hStatus["lErrPapel"    ] := hb_BitTest( nSTS2, 0 )
  hStatus["lErrImpresora"] := hb_BitTest( nSTS2, 1 )
  hStatus["lErrImpresor" ] := hb_BitTest( nSTS2, 2 )
  hStatus["lErrGaveta"   ] := hb_BitTest( nSTS2, 3 )
  hStatus["lErrCritico"  ] := hb_BitTest( nSTS2, 4 )

  ::hStatus := hStatus
*/

/*
  ? "Modo fiscal                      : ", hb_BitTest( nSTS1, 5 )
  ? "Memoria fiscal cerca de agotarse : ", hb_BitTest( nSTS1, 4 )
  ? "Memoria fiscal agotada           : ", hb_BitTest( nSTS1, 3 )
  ? "Buffer lleno                     : ", hb_BitTest( nSTS1, 2 )
  ? "Transaccion no fiscal en curso   : ", hb_BitTest( nSTS1, 1 )
  ? "Transaccion fiscal en curso      : ", hb_BitTest( nSTS1, 0 )
  

  ? "Error critico                    : ", hb_BitTest( nSTS1, 4 )
  ? "Error gaveta                     : ", hb_BitTest( nSTS1, 3 )
  ? "Error impresor                   : ", hb_BitTest( nSTS1, 2 )
  ? "Error impresora                  : ", hb_BitTest( nSTS1, 1 )
  ? "Error papel                      : ", hb_BitTest( nSTS1, 0 )
*/

   cError := ""

RETURN ::hStatus



/** Envia un comando a la impresora fiscal.
 *
 */
METHOD SENDCMD( cCommand, cError )  CLASS TFHKA
   local lError, cResp
//? procname(), " - ", cSignal, " - ", FIS_CmdToHex(cSignal)
   lError := .f.
//? StrToHex(cCommand)
   cResp := SendSignal( ::nPort, FIS_CmdToHex(cCommand), @lError )
//? procname(), "aqui...", cCommand
//? StrToHex(cResp)
//nErrorCode := tp_send( nPort, FIS_CmdToHex( cCommand ), 1 )
   if lError
      ? procname(), "Error"
      ? HexToStr(cResp)
      if Empty(cResp)
         cError := "No se recibio respuesta."
         return cResp
      endif
      if cResp == Hex_NAK
         cError := "(NAK) El comando no es reconcido."
      endif
//   else
//      if cResp == Hex_ACK
//    ? procname(), " OK - Fino! "
//      endif
   endif
RETURN cResp



/** Lee el resultado de enviar 'S1' a la impresora fiscal.
 */
METHOD GetStatus( nStatus, cError )   CLASS TFHKA
   local cResp, cCommand, nLen, aData

   default nStatus to 1

   cError := ""

   if nStatus > 5 .or. nStatus < 1
      cError := "Estatus solicitado no existe."
      return ""
   endif

   cCommand := "S"+ALLTRIM(STR(nStatus))

   cResp := ::SendCmd( cCommand, @cError )
   if !Empty( cError )
      return cResp
   endif

   nLen  := Len(cResp)
   cResp := SubStr( cResp, 2, nLen - 4 )
   aData := hb_aTokens( cResp, HexToStr("0A") )
//? "aData longitud: ", LEN(aData)
//? hb_ValToExp(aData)

   if cCommand = "S1" // Parametros de la Impresora
      ::hS1["hCajero"        ]["value"] := RIGHT( aData[01], 2 )
      ::hS1["hVtasDiarias"   ]["value"] := aData[02]
      ::hS1["hUltFactura"    ]["value"] := aData[03]
      ::hS1["hFacEmitidas"   ]["value"] := aData[04]
      ::hS1["hUltNCredito"   ]["value"] := aData[05]
      ::hS1["hNdCEmitidas"   ]["value"] := aData[06]
      ::hS1["hUltNDebito"    ]["value"] := aData[07]
      ::hS1["hNdDEmitidas"   ]["value"] := aData[08]
      ::hS1["hUltNDNFis"     ]["value"] := aData[09]
      ::hS1["hDNFEmitidos"   ]["value"] := aData[10]
      ::hS1["hContCierres"   ]["value"] := aData[11]
      ::hS1["hContRepMFiscal"]["value"] := aData[12]
      ::hS1["hRUC"           ]["value"] := aData[13]
      ::hS1["hDV"            ]["value"] := aData[14]
      ::hS1["hSerial"        ]["value"] := aData[15]
      ::hS1["hHoraImpFis"    ]["value"] := aData[16]
      ::hS1["hFechaImpFis"   ]["value"] := aData[17]

      return ::hS1 

   elseif cCommand = "S2" // Status de la Factura, NC, ND en Curso.
      ::hS2["hSbTBaseImp" ]["value"] := RIGHT( aData[01], 13 )
      ::hS2["hSbTImpuesto"]["value"] := STRTRAN(aData[02]," ","")
      ::hS2["hDataDummy1" ]["value"] := STRTRAN(aData[03]," ","")
      ::hS2["hDataDummy2" ]["value"] := STRTRAN(aData[04]," ","")
      ::hS2["hPorPagar"   ]["value"] := STRTRAN(aData[05]," ","")
      ::hS2["hNroPagos"   ]["value"] := STRTRAN(aData[06]," ","")
      ::hS2["hCondicion"  ]["value"] := STRTRAN(aData[07]," ","")
                 /* 0 - No transaccion, 1 - En pago factura, 2 - En NDC, 3 - En NDD */

      return ::hS2
 
   elseif cCommand = "S3"  // Tasas y Flags
      ::hS3["hTipoTasa1"  ]["value"] := SubStr( aData[01], 3, 1 )
      ::hS3["hValorTasa1" ]["value"] := SubStr( aData[01], 4, 4 )
      ::hS3["hTipoTasa2"  ]["value"] := SubStr( aData[02], 1, 1 )
      ::hS3["hValorTasa2" ]["value"] := SubStr( aData[02], 2, 4 )
      ::hS3["hTipoTasa3"  ]["value"] := SubStr( aData[03], 1, 1 )
      ::hS3["hValorTasa3" ]["value"] := SubStr( aData[03], 2, 4 )
      ::hS3["hFlags"      ]["value"] := aData[04]

      return ::hS3

   elseif cCommand = "S4"  // Descriptores de Medios de Pago
      ::hS4["hMedPago1"   ]["value"] := SubStr( aData[01], 3, 10 )
      ::hS4["hMedPago2"   ]["value"] := aData[02]
      ::hS4["hMedPago3"   ]["value"] := aData[03]
      ::hS4["hMedPago4"   ]["value"] := aData[04]
      ::hS4["hMedPago5"   ]["value"] := aData[05]
      ::hS4["hMedPago6"   ]["value"] := aData[06]
      ::hS4["hMedPago7"   ]["value"] := aData[07]
      ::hS4["hMedPago8"   ]["value"] := aData[08]
      ::hS4["hMedPago9"   ]["value"] := aData[09]
      ::hS4["hMedPago10"  ]["value"] := aData[10]
      ::hS4["hMedPago11"  ]["value"] := aData[11]
      ::hS4["hMedPago12"  ]["value"] := aData[12]
      ::hS4["hMedPago13"  ]["value"] := aData[13]
      ::hS4["hMedPago14"  ]["value"] := aData[14]
      ::hS4["hMedPago15"  ]["value"] := aData[15]
      ::hS4["hMedPago16"  ]["value"] := aData[16]

      return ::hS4

   elseif cCommand = "S5"  // Estado de la Memoria de Auditoria
      ::hS5["hRUC"       ]["value"] := SubStr( aData[01], 3, 20 )
      ::hS5["hDV"        ]["value"] := aData[02]
      ::hS5["hSerial"    ]["value"] := aData[03]
      ::hS5["hNumMemAud" ]["value"] := aData[04]
      ::hS5["hTotalMem"  ]["value"] := aData[05]
      ::hS5["hLeftMem"   ]["value"] := aData[06]
      ::hS5["hNumDocReg" ]["value"] := aData[07]

      return ::hS5

   endif

Return cResp




/** Envia una señal a la impresora fiscal y si la
 *  trama recibida es correcta, retorna los valores. 
 *  Caso contrario, retorna cadena vacia.
 */
STATIC FUNCTION SendSignal( nPort, cHexSignal, lError )
  local cResp
  tp_send( nPort, cHexSignal, _TPTIME_OUT_ )
  tp_inkey(.5)
  lError := .F.
  cResp := tp_recv( nPort,,_TPTIME_OUT_ )
  if !CheckTrama( cResp )
      if cResp == Hex_ACK
//? procname(), "  OK"
         return cResp
      endif
      if cResp == Hex_NAK 
         lError := .t.
         return cResp
      endif
//      cResp := ""
? "aparentemente error... porque?"
? cResp
      lError := .t.
else
? "trama recibida es correcta."
  endif
RETURN cResp



/** Verifica si la trama indicada es correcta.
 *  Es utilizada cuando se recibe un valor desde la impresora fiscal.
 *  Si verdadero si la trama de datos es correcta, falso en caso contrario.
 */
STATIC FUNCTION CheckTrama( cCadena )
  local cText, cControl := "", nPos, nLen

  nLen := LEN(cCadena)
  if nLen < 5
     // no hay trama que revisar...
     return .f.
  endif

  FOR EACH cText IN cCadena
    nPos := cText:__EnumIndex()
    if nPos = 1
       if cText != Hex_STX
? procname(), " error "
          return .f.
       endif
    elseif nPos >=3 .and. nPos < nLen
//       ? StrToHex(cText), cText
       if empty( cControl )
          cControl:= StrToHex( CharXOR( SubStr(cCadena,2,1), cText ) )
       else
          cControl:= StrToHex( CharXOR( HexToStr(cControl), cText ) )
       endif
    elseif nPos = nLen
//       ? StrToHex(cText), cText
       if HexToStr(cControl) != cText
? procname(), " error "
          return .f.
       endif
    endif
  NEXT

RETURN .t.




/** Crea la secuencia hexadecimal para enviar al fiscalizador
 *  Incluye al comando dado:
 *  el codigo inicial "STX", final "ETX" y control "LRC"
 */
STATIC FUNCTION FIS_CmdToHex( cCommand )

   local cText:="", cResult:=""
   local cControl := ""

   cCommand := iif( Empty( cCommand) .or. hb_IsNIL( cCommand), "", cCommand )

   if empty( cCommand ) ; return ""; endif
   
   cResult := STR_STX + " "

   FOR EACH cText IN cCommand
//      ? cText
      cResult += StrToHex(cText) + " "
      if cText:__EnumIndex()>=2
         if empty( cControl )
            cControl:= StrToHex( CharXOR( SubStr(cCommand,1,1), cText ) )
         else
            cControl:= StrToHex( CharXOR( HexToStr(cControl), cText ) )
/*
  cControl:= StrToHex( CharXOR( SubStr(cCadena, 1, 1), SubStr(cCadena, i, 1) ) )
         Else
  cControl:= StrToHex( CharXOR( HexToStr(cControl), SubStr(cCadena, i, 1) ) )
*/
         endif
      endif
   NEXT 

   cResult += STR_ETX
   cText := STR_ETX
  // cControl:= ( CharXOR( HexToStr(cControl), cText ) )
   cControl:= StrToHex( CharXOR( HexToStr( cControl ) , HexToStr( cText ) ) )
   cResult += " "+cControl

//   ? "Control: " + cControl
//   ? "Resultado: STX + COMANDO ("+cCommand+") + ETX + LRC(control) "
//   ? "Resultado: " + cResult

return HexToStr( cResult )





//eof

