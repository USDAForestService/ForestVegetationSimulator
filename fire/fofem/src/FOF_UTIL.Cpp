//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_util.c
* Date: 8-20-99
* Contains:
*  Basal_Area.....Get Basal Area
*  Calc_Scorch....Calc scorch height from flame length
*  Calc_Flame.....Calc flame length from scorch height
*  Blk_End_Line...Look for carriage other char fill line blanks
*  ToInt..........Get int from specified cols of a buffer
*  ToStr..........Get string from specified cols of a buffer
*  ToFlo..........Get float from specified cols of a buffer
*  isBlank........if a line contains only blanks or is empty.
*  isBlank_Tab....if a line contains only blanks, look for tabs
*  isBlankCR......if a line is empty, or only blank and/or carrage ret, other
*  Tab_Error......Display an Error Message say tabs characters
*  Trim_LT........Remove Leading & Trailing blanks from a string.
*  Left_Just.....Left Justify a null terminated string
*  StrRepChr......Replace all the characters with another character
*  Remove_Path....remove path and file extension
*  Rem_Path.......Remove path from a file name string
*  Remove_FN......Remove File name from a Path File Name string
*  App_Ext........Append a file extension onto a file name
*  Get_NumTyp.....is an Integer or Float or Invalid
*  Rem_LT_Blanks..Remove any Leading and Trailing blanks from a string
*  EndNull........Take the blank chars off the end of a string
*  isFile.........See if a path - file name is there,
*  TPA_To_KiSq....Convert Tons Per Acre to Kilograms per square meter
*  KgSq_To_TPA....Convert Killgrams Squared to Tons Per Acre
*  HrMinSec.......Covert a number of seconds to 'Hr:Mn:Se' as a string
*  xstrupr........Conver string to upper case,
*  Sqr............Square a number
*  InchToCent.....Covert inchs to centimeters
*
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "fof_gen.h"
#include "fof_util.h"


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Sqr
* Desc: Square a number
*   In: f...number to square
*  Ret: number
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  Sqr (float f)
{
   return f * f ;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SetLen7
* Desc: Take a 6 char string and put a single blank space on the end.
*       I use this for the Mortality species which a mostly 6 chars but
*       some are 7, so when I display them I need to make them all the same.
*   In: cr...string
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  SetLen7 (char cr[])
{
  if ( strlen (cr) == 6 )
    strcat (cr," ");
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Basal_Area
* Desc: Get the Basal Area of a number of trees
*   In: f_DBH.....diam breast height
*       f_Cnt.....number of trees to times by
*  Ret: total basal area
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float    Basal_Area  ( float  f_DBH, float  f_Cnt )
{
float f, r;
     r = f_DBH / 2;                          /* radius                       */
     f = 3.14159 * ( r * r );                /* area = pi r sqrd             */
     f = f * f_Cnt;                          /* Times how many trees         */
     return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Calc_Scorch
* Desc: Calculate the scorch height fromt the flame length
*      Orig FORTRAN statement 'FLAME_TO_SCORCH = ((XFI / 0.45)**2.174)**0.667'
*   In: f_flame....flame length
*  Ret: scorch height
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float Calc_Scorch (float f_flame )
{
float f,g, h;
    f =  f_flame / 0.45;
    g = pow ((float)f, (float)2.174);
    h = pow ((float)g,(float)0.667);
    return h;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Calc_Flame
* Desc: Calculate the flame length from the scorch height
*  Orig FORTRAN statement  'SCORCH_TO_FLAME = 0.45 * (XFI**1.5)**0.46'
*   In: f_XFI......scorch height
*  Ret: flame length
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float   Calc_Flame (float f_XFI)
{
float f,g;
   g =  pow ((float)f_XFI,(float)1.5);
   f =  0.45 * pow ((float)g, (float)0.46);
   return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Blk_End_Line
* Desc: Go thru a string looking for a carriage ret or other char that
*         specifies the end of a line (line of text from a file)
*         from that point fill the reset of the string with blank chars
*         and null term the string
*   In: cr_Line......String
*       i_Len........Length
*  Out: cr_Line......String with end blanked out
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Blk_End_Line (char cr_Line[], int i_Len)
{
int i,j;
   j = 0;
   for ( i = 0; i < i_Len; i++ ) {
     if ( cr_Line[i] < 25 )
       j = 1;
     if ( j == 1 )
        cr_Line[i] = ' '; }
   cr_Line[i-1] = 0;                /* null term at last position in string  */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: ToInt
* Desc: Get an integer from specified cols of a buffer, and convert it to an
*       integer.
*   In: cr_RB.....rec buffer
*       iA_Col....starting Col  NOTE first col is 1
*       iB_Col....ending Col
*  Out: ai........convert integer
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  ToInt (int *ai, char *cr_RB, int iA_Col, int iB_Col )
{
int  i;
char cr[100];

   ToStr  (cr,  cr_RB, iA_Col, iB_Col);    /* Pull sub string out */
   if ( isBlank (cr) )                     /* is it blank         */
      *ai = -1;                            /* Set to missing -1   */
   else {
      i = atoi(cr);
     *ai = i;  }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: ToStr
* Desc: Get a string from specified cols of a buffer, move and Null term it
*   In: cr_Int....rec buffer
*       iA_Col....starting Col  NOTE first col is 1
*       iB_Col....ending Col
*  Out: cr_Out....moved and Null terminated string
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void   ToStr (char *cr_Out, char *cr_In, int iA_Col, int iB_Col )
{
int i_Len, iA;
   i_Len = (iB_Col - iA_Col) + 1;
   iA = iA_Col - 1;
   if ( i_Len < 1 ) {
     strcpy (cr_Out,"");
     return; }
   else {
     memcpy (cr_Out, &cr_In[iA] , i_Len);
     cr_Out[i_Len] = (char) NULL;  }

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: ToFlo
* Desc: Get a float  from specified cols of a buffer, and convert it to a
*       float.
* NOTE: If missing set it to 0
*   In: cr_RB.....rec buffer
*       iA_Col....starting Col  NOTE first col is 1
*       iB_Col....ending Col
*  Out: af........convert float
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  ToFlo (float *af, char *cr_RB, int iA_Col, int iB_Col )
{
float  f;
char cr[100];
   ToStr  (cr,  cr_RB, iA_Col, iB_Col );
   if ( isBlank (cr) )                     /* is it blank         */
      *af =  0;                            /* Set as 0             */
   else {
      f = atof(cr);
     *af = f; }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: isBlank
* Desc: See if a line contains only blanks or is empty.
*   In: cr......string
*  Ret: 1....Blank Line
*       0....not Empty
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   isBlank (char  cr[] )
{
int i;
    for ( i = 0; i < 1000; i++ ) {
      if ( cr[i] == 0 )
        return 1;
      if ( cr[i] == ' ' )
        continue ;
      return 0;
    }
    return 0;   /* Shouldn't get here */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: isBlank_Tab
* Desc: See if a line contains only blanks, is empty
* NOTE: This also checks for tab charaters in the line, tabs will also
*       make the line appear to be empty.
*   In: cr......string
*  Ret: 1....Blank Line
*       0....not Empty
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   isBlank_Tab (char  cr[] )
{
int i;
    for ( i = 0; i < 1000; i++ ) {
      if ( cr[i] == 0 )
        return 1;
      if ( cr[i] == ' ' )
        continue ;
      if ( cr[i] == '\t' )
        continue ;
      return 0;
    }
    return 0;   /* Shouldn't get here */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: isBlankCR
* Desc: See if a line contains only blanks and or car ret, line feed
*   In: cr......string
*  Ret: 1....Blank Line
*       0....not Empty
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   isBlankCR (char  cr[] )
{
int i;
    for ( i = 0; i < 1000; i++ ) {
      if ( cr[i] == 0 )
        return 1;
      if ( cr[i] == ' ' )
        continue ;
      if ( cr[i] == '\n' )   /*  car ret */
        continue ;
      if ( cr[i] == '\r' )   /* line feed, i think */
        continue ;
      return 0;
    }
    return 0;   /* Shouldn't get here */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Trim_LT
* Desc: Remove Leading & Trailing blanks from a string.
* NOTE: This will leave any embedded blanks intact.
*       see the Rem_LT_Blanks function
*   In: cr.......String
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void     Trim_LT (char cr[])
{
int i,j;
   Left_Just (cr);                /* remove leading */
   j = strlen(cr);
   if ( j == 0 )
     goto Z;
   j--;
   for ( i = j; i > 0; i--) {     /* go to end of string and work */
     if ( cr[i] != ' ' )          /* back until a char is hit */
       break;                     /* then null term the string */
     if ( cr[i] == ' ' )
       cr[i] = 0;
   }
Z:;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
*  Name: Left_Just
*  Desc: Left Justify a null terminated string
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void    Left_Just ( char  cr[] )
{
int     i, j;
   if ( cr[1] == 0 ) goto Z;                 /* Empty String */
   for ( i = 0; i < 1000; i++ ) {            /* Find first non blank */
     if ( cr[i] != ' ' )
        break ;   }
   if ( i == 0 )  goto Z;                    /* Already Left Justified */
   for ( j = 0; j < 1000; j++ ) {            /* Left Justify it          */
    cr [j]  =  cr [i++];
    if ( cr[j] == 0 )
       break;     }
Z:;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: StrRepChr
* Desc: Replace all the characters in a string with another character
*       String must be NULL terminated
*   In: c_This....find this char in string
*       c_That....new char to be put into string
* In/Out:  cr.....null term string
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  StrRepChr (char cr[], char c_This,  char c_That )
{
int i;
    for ( i = 0; i < 20000; i++ ) {
      if ( cr[i] == 0 )
        break;
      if ( cr[i] == c_This )
        cr[i] = c_That;
    }
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
*  Name: Rem_Path
*  Desc: Remove the path from a file name string. This can be
*    In: cr_Arg......In string
*   Out: cr_New......Extracted file name
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Rem_Path ( char cr_Arg[], char  cr_New[] )
{
int iX, i;
   iX = 0;
   for ( i = 0; i < 100; i++ ) {
     if ( cr_Arg[i] == 0 )          /* Hit end of string                    */
         break;
     if ( cr_Arg[i] == '\\' )       /* look for the last back slash in path */
        iX = i;                     /* the file name will be right after it */
   }

   if ( iX != 0 )                  /* was there a path on front            */
     iX++;                          /* start of file name                   */
   strcpy (cr_New, &cr_Arg[iX]);
}
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Remove_FN
* Desc: Remove the File name from a Path File Name string
*   In: cr......Path File Name
*  Out: cr......Path with File Name removed
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Remove_FN (char cr[])
{
int i, i_Len;
   i_Len = strlen (cr);
   i_Len--;
   if ( i_Len < 1 )
     goto X;
   for ( i = i_Len; i >= 0; i-- ) {     /* go backward thru string           */
     if ( cr[i] == '\\' ) {             /* this is end of any path           */
       cr[i] = 0;                       /* term str                          */
       break; }
     if ( cr[i] == ':' ) {              /* if root drv  ie. 'c:filnam'       */
       cr[i+1]= 0;
       break; }  }
X:;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
*  Name: App_Ext
*  Desc: Append a file extension onto a file name. If the file name has a
*        period in it the new extension replaces the old one, but if
*        there is no extension the the new one will be add on.
*    In: cr_Ext.......extension to be appended
* In/Out: cr_FN.......File Name
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  App_Ext ( char cr_FN[], char  cr_Ext[] )
{
int i,i_Len ;

   i_Len = strlen(cr_FN);
   if ( i_Len == 0 ) {
     strcpy (cr_FN,"fofem");
     goto X; }

   for ( i = i_Len - 1; i > 0; i-- ) {
     if ( cr_FN[i] == '.' ) {                /* Hit the period in name       */
       strcpy ( &cr_FN[i+1], cr_Ext );       /* Tack on extension            */
       return; } }

X:
   strcat (cr_FN, ".");
   strcat (cr_FN, cr_Ext );
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Get_NumTyp
* Desc: See if a char string is an Integer or Float or Invalid
*   In: cr_Data....String to be checked
*  Ret: F,I, or X
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
char     Get_NumTyp (char cr_Data[])
{
char cr[30];
int  i_CntDig,i_CntDec, i, j;

   if ( strlen(cr_Data) >= sizeof (cr) )
      return 'X';

   strcpy (cr, cr_Data);                /* Make a copy                       */
   Rem_LT_Blanks (cr);                  /* Remove Leading and Trailing Blnks */
   i_CntDig = 0;
   i_CntDec = 0;
   if ( cr[0] == 0 )                    /* String is empty                   */
      return 'X';

   j = 0;
   if ( cr[j] == '+' || cr[j] == '-' )  /* ok on front of string             */
       j++;
   if ( cr[j] == '.' ) {
       i_CntDec++;
       j++; }
   for ( i = j; i < 1000; i++ ) {       /* go thru string                    */
     if ( cr[i] == 0 )                  /* end of string                     */
        break;
     if ( cr[i] >= '0' && cr[i] <= '9' ) {
        i_CntDig++;                     /* Count digits                      */
        continue; }
     if ( cr[i] == '.' ) {
        i_CntDec++;                     /* Count Decimal Points              */
        continue; }
     return 'X';  }                     /* Bad Char in string                */

   if ( i_CntDig == 0 )                 /* Need at least on digit            */
     return 'X';
   if ( i_CntDec > 1 )                  /* Can only have one Decimal         */
     return 'X';
   if ( i_CntDec == 1 )                 /* One Decimal Points, was Float     */
      return 'F' ;
   else
      return 'I' ;                      /* Was Integer                       */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Rem_LT_Blanks
* Desc: Remove any Leading and Trailing blanks from a string
* NOTE: This will not work on a string with embedded blanks, it will
*       terminate the string on the first embedded blank it finds in a
*       string. See the Trim_LT function
* In/Out: string
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void      Rem_LT_Blanks (char cr[])
{
   Left_Just (cr);                      /* Left Justify, remove lead blanks  */
   EndNull   (cr);                      /* Remove trailing blanks            */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: EndNull
* Desc: Take the blank chars off the end of a string by replacing the first
*       blank with a null char.
* In/Out: cr.....string
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void    EndNull (char cr[])
{
int i;
   for ( i = 0; i < 1000; i++ ) {         /* Go thru string                  */
     if ( cr[i] == 0 )                    /* Hit end of sting                */
        goto Z;
     if ( cr[i] == ' ' ) {                /* Look for first blank char       */
       cr[i]  = 0;                        /* Null term and beat it           */
       goto Z ; }
    }
Z:;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: isFile
* Desc: See if a path - file name is there,
*   In: cr_PathFN...Path and file name
*  Ret: 1  file is there,
*       0  No File exists
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int isFile (char cr_PathFN[])
{
FILE *fh;
  fh = fopen (cr_PathFN,"r");
  if ( fh == NULL )
    return 0;
  fclose (fh);
  return 1;
}




/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: InchMeter
* Desc: Conver Inches to Meters
*   In: inches
*  Ret: MilleMeters
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float  InchMeter (float f_Inch)
{
float f, f_InchinMeter;
  f_InchinMeter = 39.37;
  f = f_Inch / f_InchinMeter;
  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: InchToCent
* Desc: Convert Inches to Centimeters
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float InchToCent (float f_Inch)
{
   return ( f_Inch * 2.54 );
}


float CentToInch (float f_Cent)
{
float f;
  f = f_Cent *  0.393701;
  return f; 
}
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: TPA_To_KiSq
* Desc: Convert Tons Per Acre to Kilograms per square meter
*  Ret: answer
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  TPA_To_KiSq (float f_TPA)
{
float f,g;
    g = 4.46;
    f = f_TPA / g;
    return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: KgSq_To_TPA
* Desc: Convert Killgrams Squared to Tons Per Acre
*  Ret: answer
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  KgSq_To_TPA (float f_KgSq)
{
float f,g;
    f =  (float)1 / TPA_To_KiSq(1);
    g =  f_KgSq * f;
    return g;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: HrMinSec
* Desc: Covert a number of seconds to 'Hr:Mn:Se' as a string
*       Example: 4984 will get you  '01:23:04'
*   In: i_Seconds....total number of seconds
*  Out: cr_Out.......out string, see above comments
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void HrMinSec (int i_Seconds, char cr_Out[])
{
int i, i_Sec, i_Min, i_Hr;
  i = (int) i_Seconds;
  i_Sec = i % 60;
  i = i / 60;

  i_Min = i % 60;
  i = i / 60;

  i_Hr  = i % 60;
  sprintf (cr_Out, "%02d:%02d:%02d", i_Hr, i_Min, i_Sec);
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _GetLine
* Desc: Get a single line of data from a larger string, the larger string
*        is the text from an Window Edit Text Box on the screen, like the
*        Species window.
* NOTE:  Lines are serparated with carrige return and line feeds, that's
*         how text edit boxes end lines.
*   In:  cr_Bur....larger string, to get line from
*        i_Row.....row to extract, starting at 0
*  Out:  cr_Line...line of text
*  Ret:  1 ok,  0 no more lines...
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   _GetLine (char cr_Bur[], char cr_Line[], int i_Row)
{
int i,j,i_Cnt;

   i_Cnt = 0;
   j = -1;
   for ( i = 0; i < 20000; i++ ) {      /* do each char in full string       */
     if ( cr_Bur[i] == '\r')            /* ignore these line feeds           */
       continue;

     j++;
     cr_Line [j] = cr_Bur[i];           /* build up a single line            */

     if ( cr_Bur[i] == '\n'   ||        /* hit end of line                   */
          cr_Bur[i] == 0 ) {            /* hit end of string                 */
       cr_Line[j] = 0;                  /* NUll term out going string        */
       if ( i_Cnt == i_Row )            /* is this the one caller wants      */
          return 1;                     /*  yes                              */
       else {                           /* no, so                            */
          j = -1;                       /* reset index to get next line      */
          i_Cnt++; } }                  /* Count lines                       */

     if ( cr_Bur[i] == 0 )              /* hit end of input string without   */
       break;                           /*  reaching the line we want        */
   }
   return 0;
}
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: TPA_To_MTPH
* Desc: Convert Tons Per Acre to Metric Tons Per Hectare
*        A metric ton = a megagram (Mg)
*   In: tons per acre
*  Ret: metric tons per hectare
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float TPA_To_MTPH (float TPA)
{
float f ;
    f = 0.907184 * TPA;
    return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MTPH_To_TPA
* Desc: Convert Metric Tons Per Hectare to Tons Per Acre
*       A metric ton = a megagram (Mg)
*   In: metric tons per hactare
*  Ret: tons per acre
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float MTPH_To_TPA (float Mg)
{
float f ;
    f =  Mg / 2.242;
    return f;
}
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: InchToMilMeter
* Desc: Conver Inches to Millimeters
*   In: inches
*  Ret: MilleMeters
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float InchToMilMeter  (float f_Inch)
{
   return f_Inch * 25.4;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MilMeterToInch
* Desc: Convert Millimeters to Inches
*   In: MilleMeters
*  Ret: inches
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float MilMeterToInch  (float f)
{
   return f * 0.0393700787;
}
