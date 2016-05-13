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

#require "tfhka"

//#include "tfhka.ch"
#include "common.ch"


PROCEDURE MAIN( cCommand )

   local oFiscal, cError:=""
   local hStatus, hResult
   local I, cI

   default cCommand to ""


   oFiscal := TFHKA():New( 2, 9600, 8, "E", 1, "/dev/ttyUSB0", @cError )

   if !Empty(cError)
      ? "Error: " + cError
      return
   else
      ? "Creado el objeto fiscal."
   endif
   ? "Sistema Operativo: ", oFiscal:cOS
   ? "Puerto: ", oFiscal:GetPortName()

   //oFiscal:SetTimeOut(1)

   hStatus := oFiscal:Status( @cError )
   if Empty( hStatus )
      ? "Error: ", cError
      oFiscal:End()
      return
   endif
   ? "Status: ", hb_ValToExp( hStatus )


   FOR I:= 1 TO 5
      cI := ALLTRIM(STR(I)) 
      ? STRTRAN("Obteniendo Status% 'S%' ","%",cI)
      hResult := oFiscal:GetStatus(I, cError)
      if !Empty( cError )
         ? "Error: ", cError
         oFiscal:End()
         return 
      endif
      ?
      ? "========================="
      ? STRTRAN("Valores de Status % (S%).","%",cI)
      ? hResult["info"]
      ? "========================="

      FOR EACH hStatus IN hResult
         if hb_IsHash( hStatus )
            ? '"'+hb_HKeyat( hResult, hStatus:__EnumIndex() )+'" => ', ;
              hStatus["title"], " => ", ;
              hStatus["value"]
         endif
      NEXT

   NEXT I

   ?

   oFiscal:End()

return

//eof
