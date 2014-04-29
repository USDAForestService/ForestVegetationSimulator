//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_ansi.c
* Desc:  These are stub functions for xstrupr() and xstrcmpi()
*        This file only gets compiled on Windows programs.
*        strupr() and strcmpi() won't port to Unix, so any code that
*        needs to be ANSI and go to Unix uses the 'x' functions.
*        See fof_unix.c file, it get use instead of this one for Unix compling
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


#include  "fof_ansi.h"

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: xstrcmpi
* Desc: compare strings, case insensitive
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int xstrcmpi  (char a[], char b[])
{
int i;
   i = stricmp (a,b);
   return i;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: xstrupr
* Desc: convert a string to upper case, strupr is not on Unix
* In/Out:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  xstrupr (char cr[])
{
  strupr (cr);
}
