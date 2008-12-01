/* ÅÄÖåäö -- UTF-8 hint
 */
/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

LineBreak ([\n]".ensure-line-break"([ \t]|" ")*)+
ParagraphBreak ([\n]".ensure-line-break"([ \t]|" ")*)*[\n]+{LineBreak}

%%

[\n]+".br" { printf( "\n.br" ); }
{ParagraphBreak}".SH" { printf( "\n.SH" ); }
{ParagraphBreak}".SS" { printf( "\n.SS" ); }
{ParagraphBreak} { printf( "\n\n" ); }
{LineBreak} { printf( "\n" ); }
"\\\n" { printf( "\\\\\n.br\n" ); }
[\n]{3,} { printf( "\n\n" ); }

"&lt;" { printf( "<" ); }
"&gt;" { printf( ">" ); }
"&amp;" { printf( "&" ); }
"&apos;" { printf( "'" ); }
"&quot;" { printf( "\"" ); }

"—" { printf( "\\-" ); }
"…" { printf( "..." ); }
"⋅" { printf( "*" ); }


%%

int
main( )
{
	yylex( );
	return 0;
}
