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
 */


#define  STR_STX                "02"
#define  STR_ENQ                "05"
#define  STR_ETX                "03"
#define  STR_EOT                "04"
#define  STR_ACK                "06"
#define  STR_NAK                "15"
#define  STR_ETB                "17"
#define  STR_LRC(value)         FIS_StrXOR(value, STR_ETX)

#define  Hex_STX                HexToStr( STR_STX )
#define  Hex_ENQ                HexToStr( STR_ENQ )
#define  Hex_ETX                HexToStr( STR_ETX )
#define  Hex_EOT                HexToStr( STR_EOT )
#define  Hex_ACK                HexToStr( STR_ACK )
#define  Hex_NAK                HexToStr( STR_NAK )
#define  Hex_ETB                HexToStr( STR_ETB )
#define  LRC(value)             HexToStr( STR_LRC( value ) )

#define  _TPTIME_OUT_           2



