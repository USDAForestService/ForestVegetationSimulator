
// C language routines that link ODBC to FVS
// NLCrookston -- RMRS -- Moscow -- October 2011 
// fvsSQL -- DATE OF LAST REVISION:  10/31/2011

// to compile: gcc -c fvsSQL.c

#include <windows.h>
#include <stdio.h>
#include <sql.h>
#include <sqlext.h>

void genTypes(void)
{
  /***** Run to generate the type definitions for DBSTYPEDEFS.F77
         Call it from fvssqldriverconnect_
  */
  SQLCHAR       SQLCHAR_KIND;      printf("\n      integer,parameter:: SQLCHAR_KIND=      %d\n",sizeof(SQLCHAR_KIND     ));             
  SQLSMALLINT   SQLSMALLINT_KIND;  printf("      integer,parameter:: SQLSMALLINT_KIND=  %d\n",sizeof(SQLSMALLINT_KIND));             
  SQLUSMALLINT  SQLUSMALLINT_KIND; printf("      integer,parameter:: SQLUSMALLINT_KIND= %d\n",sizeof(SQLUSMALLINT_KIND));
  SQLINTEGER    SQLINTEGER_KIND;   printf("      integer,parameter:: SQLINTEGER_KIND=   %d\n",sizeof(SQLINTEGER_KIND  ));             
  SQLUINTEGER   SQLUINTEGER_KIND;  printf("      integer,parameter:: SQLUINTEGER_KIND=  %d\n",sizeof(SQLUINTEGER_KIND ));
  SQLREAL       SQLREAL_KIND;      printf("      integer,parameter:: SQLREAL_KIND=      %d\n",sizeof(SQLREAL_KIND     ));             
  SQLDOUBLE     SQLDOUBLE_KIND;    printf("      integer,parameter:: SQLDOUBLE_KIND=    %d\n",sizeof(SQLDOUBLE_KIND   ));            
  SQLLEN        SQLLEN_KIND;       printf("      integer,parameter:: SQLLEN_KIND=       %d\n",sizeof(SQLLEN_KIND      ));
  SQLFLOAT      SQLFLOAT_KIND;     printf("      integer,parameter:: SQLFLOAT_KIND=     %d\n",sizeof(SQLFLOAT_KIND    ));
  SQLRETURN     SQLRETURN_KIND;    printf("      integer,parameter:: SQLRETURN_KIND=    %d\n",sizeof(SQLRETURN_KIND   ));

  SQLPOINTER    SQLPOINTER_KIND;   printf("\n      integer,parameter:: SQLPOINTER_KIND=   %d\n",sizeof(SQLPOINTER_KIND  ));
  SQLHANDLE     SQLHANDLE_KIND;    printf("      integer,parameter:: SQLHANDLE_KIND=    %d\n",sizeof(SQLHANDLE_KIND   ));
  SQLHENV       SQLHENV_KIND;      printf("      integer,parameter:: SQLHENV_KIND=      %d\n",sizeof(SQLHENV_KIND     ));
  SQLHDBC       SQLHDBC_KIND;      printf("      integer,parameter:: SQLHDBC_KIND=      %d\n",sizeof(SQLHDBC_KIND     ));
  SQLHSTMT      SQLHSTMT_KIND;     printf("      integer,parameter:: SQLHSTMT_KIND=     %d\n",sizeof(SQLHSTMT_KIND    ));
  SQLHDESC      SQLHDESC_KIND;     printf("      integer,parameter:: SQLHDESC_KIND=     %d\n",sizeof(SQLHDESC_KIND    ));
  SQLHWND       SQLHWND_KIND;      printf("      integer,parameter:: SQLHWND_KIND=      %d\n",sizeof(SQLHWND_KIND     ));
  SQLULEN       SQLULEN_KIND;      printf("      integer,parameter:: SQLULEN_KIND=      %d\n",sizeof(SQLULEN_KIND     ));
  printf("      integer,parameter:: SQL_OV_ODBC3_KIND= %d\n",sizeof(SQL_OV_ODBC3));
}


/**************************************
SQLRETURN SQLAllocHandle(
      SQLSMALLINT   HandleType,
      SQLHANDLE     InputHandle,
      SQLHANDLE *   OutputHandlePtr);
*/      
int fvssqlallochandle_(     
      SQLSMALLINT *HandleType,
      SQLHANDLE   *InputHandle,
      SQLHANDLE   *OutputHandlePtr)
{
 /*   genTypes();    */
   SQLHANDLE lOutputHandlePtr;
   int rtn = SQLAllocHandle(
     *HandleType,
     *InputHandle,
     &lOutputHandlePtr);
   *OutputHandlePtr = lOutputHandlePtr;   
   return rtn;
}      

/**************************************
SQLRETURN SQLBindCol(
      SQLHSTMT       StatementHandle,
      SQLUSMALLINT   ColumnNumber,
      SQLSMALLINT    TargetType,
      SQLPOINTER     TargetValuePtr,
      SQLLEN         BufferLength,
      SQLLEN *       StrLen_or_Ind);
*/      
int fvssqlbindcol_(
      SQLHSTMT       *StatementHandle,
      SQLUSMALLINT   *ColumnNumber,
      SQLSMALLINT    *TargetType,
      SQLPOINTER     *TargetValuePtr,
      SQLLEN         *BufferLength,
      SQLLEN         *StrLen_or_Ind)
{
  int rtn;
  rtn = SQLBindCol(
      *StatementHandle,
      *ColumnNumber,
      *TargetType,
       TargetValuePtr,
      *BufferLength,
       StrLen_or_Ind);
  return rtn;  
}      

/**************************************
SQLRETURN SQLBindParameter(
      SQLHSTMT        StatementHandle,
      SQLUSMALLINT    ParameterNumber,
      SQLSMALLINT     InputOutputType,
      SQLSMALLINT     ValueType,
      SQLSMALLINT     ParameterType,
      SQLULEN         ColumnSize,
      SQLSMALLINT     DecimalDigits,
      SQLPOINTER      ParameterValuePtr,
      SQLLEN          BufferLength,
      SQLLEN *        StrLen_or_IndPtr);
*/
int fvssqlbindparameter_(
      SQLHSTMT        *StatementHandle,
      SQLUSMALLINT    *ParameterNumber,
      SQLSMALLINT     *InputOutputType,
      SQLSMALLINT     *ValueType,
      SQLSMALLINT     *ParameterType,
      SQLULEN         *ColumnSize,
      SQLSMALLINT     *DecimalDigits,
      SQLPOINTER      *ParameterValuePtr,
      SQLLEN          *BufferLength,
      SQLLEN          *StrLen_or_IndPtr)
{
  int rtn;
  rtn = SQLBindParameter(
      *StatementHandle,
      *ParameterNumber,
      *InputOutputType,
      *ValueType,
      *ParameterType,
      *ColumnSize,
      *DecimalDigits,
       ParameterValuePtr,
      *BufferLength,
       StrLen_or_IndPtr);
  return rtn;   
}

/**************************************
SQLRETURN SQLCloseCursor(
     SQLHSTMT     StatementHandle);
*/
int fvssqlclosecursor_(
      SQLHSTMT        *StatementHandle)
{
  return SQLCloseCursor(
     *StatementHandle);  
}

/************************************** 
SQLRETURN SQLColAttribute (
      SQLHSTMT        StatementHandle,
      SQLUSMALLINT    ColumnNumber,
      SQLUSMALLINT    FieldIdentifier,
      SQLPOINTER      CharacterAttributePtr,
      SQLSMALLINT     BufferLength,
      SQLSMALLINT *   StringLengthPtr,
      SQLLEN *        NumericAttributePtr);
*/
int fvssqlcolattribute_(
      SQLHSTMT        *StatementHandle,
      SQLUSMALLINT    *ColumnNumber,
      SQLUSMALLINT    *FieldIdentifier,
      SQLPOINTER      *CharacterAttributePtr,
      SQLSMALLINT     *BufferLength,
      SQLSMALLINT     *StringLengthPtr,
      SQLLEN          *NumericAttributePtr)
{
  int rtn;
  rtn = SQLColAttribute(
      *StatementHandle,
      *ColumnNumber,
      *FieldIdentifier,
       CharacterAttributePtr,
      *BufferLength,
       StringLengthPtr,
       NumericAttributePtr);
  return rtn;  
}


/**************************************
SQLRETURN SQLDescribeCol(
      SQLHSTMT       StatementHandle,
      SQLUSMALLINT   ColumnNumber,
      SQLCHAR *      ColumnName,
      SQLSMALLINT    BufferLength,
      SQLSMALLINT *  NameLengthPtr,
      SQLSMALLINT *  DataTypePtr,
      SQLULEN *      ColumnSizePtr,
      SQLSMALLINT *  DecimalDigitsPtr,
      SQLSMALLINT *  NullablePtr);
*/
int fvssqldescribecol_(
      SQLHSTMT       *StatementHandle,
      SQLUSMALLINT   *ColumnNumber,
      SQLCHAR        *ColumnName,
      SQLSMALLINT    *BufferLength,
      SQLSMALLINT    *NameLengthPtr,
      SQLSMALLINT    *DataTypePtr,
      SQLULEN        *ColumnSizePtr,
      SQLSMALLINT    *DecimalDigitsPtr,
      SQLSMALLINT    *NullablePtr)
{
  int rtn;
  rtn = SQLDescribeCol(
      *StatementHandle,
      *ColumnNumber,
       ColumnName,
      *BufferLength,
       NameLengthPtr,
       DataTypePtr,
       ColumnSizePtr,
       DecimalDigitsPtr,
       NullablePtr);
  return rtn;  
}
        

/**************************************
SQLRETURN SQLDisconnect(
     SQLHDBC     ConnectionHandle);
*/
int fvssqldisconnect_(
     SQLHDBC     *ConnectionHandle)
{
  return SQLDisconnect(
     *ConnectionHandle);  
}
        

/************************************** 
SQLRETURN SQLDriverConnect(
     SQLHDBC         ConnectionHandle,
     SQLHWND         WindowHandle,
     SQLCHAR *       InConnectionString,
     SQLSMALLINT     StringLength1,
     SQLCHAR *       OutConnectionString,
     SQLSMALLINT     BufferLength,
     SQLSMALLINT *   StringLength2Ptr,
     SQLUSMALLINT    DriverCompletion);
*/


int fvssqldriverconnect_(
     SQLHDBC         *ConnectionHandle,
     SQLHWND         *WindowHandle,
     SQLCHAR         *InConnectionString,
     SQLSMALLINT     *StringLength1,
     SQLCHAR         *OutConnectionString,
     SQLSMALLINT     *BufferLength,
     SQLSMALLINT     *StringLength2Ptr,
     SQLUSMALLINT    *DriverCompletion)
{
  int rtn;
/********************** DEBUG
  *(InConnectionString + *StringLength1 + 1)=0;
  printf("\nSQLDriverConnect, InConnectionString= %s\n",InConnectionString);
  printf(" StringLength1= %d\n",*StringLength1);
  printf(" BufferLength= %d DriverCompletion = %d\n",*BufferLength,*DriverCompletion);
/**********************/
  /* blank out OutConnection String */
  int i;
  for (i=0; i<*BufferLength; i++) *(OutConnectionString+i)=' ';

  rtn = SQLDriverConnect(
     *ConnectionHandle,
     *WindowHandle,    
      InConnectionString,
     *StringLength1,
      OutConnectionString,
     *BufferLength,
      StringLength2Ptr,
     *DriverCompletion);

/********************** DEBUG
  printf("\nSQLDriverConnect, OutConnectionString2= %s\n",OutConnectionString);
  printf(" StringLength2Ptr= %d\n",*StringLength2Ptr);
  printf(" rtn SQLDriverConnect= %d\n",rtn);
/**********************/

  return rtn;
}


/************************************** Works in tests, but not yet used in FVS
SQLRETURN SQLDrivers(
     SQLHENV         EnvironmentHandle,
     SQLUSMALLINT    Direction,
     SQLCHAR *       DriverDescription,
     SQLSMALLINT     BufferLength1,
     SQLSMALLINT *   DescriptionLengthPtr,
     SQLCHAR *       DriverAttributes,
     SQLSMALLINT     BufferLength2,
     SQLSMALLINT *   AttributesLengthPtr);
*/
int fvssqldrivers_(
     SQLHENV        *EnvironmentHandle,
     SQLUSMALLINT   *Direction,
     SQLCHAR        *DriverDescription,
     SQLSMALLINT    *BufferLength1,
     SQLSMALLINT    *DescriptionLengthPtr,
     SQLCHAR        *DriverAttributes,
     SQLSMALLINT    *BufferLength2,
     SQLSMALLINT    *AttributesLengthPtr)
{ 
  int rtn;     
  rtn = SQLDrivers(
    *EnvironmentHandle, 
    *Direction, 
     DriverDescription,
    *BufferLength1, 
     DescriptionLengthPtr, 
     DriverAttributes,
    *BufferLength2, 
     AttributesLengthPtr); 
  return rtn;
}      
     

/**************************************
SQLRETURN SQLEndTran(
     SQLSMALLINT   HandleType,
     SQLHANDLE     Handle,
     SQLSMALLINT   CompletionType);
*/
int fvssqlendtran_(
     SQLSMALLINT   *HandleType,
     SQLHANDLE     *Handle,
     SQLSMALLINT   *CompletionType)
{
  int rtn;     
  rtn = SQLEndTran(
     *HandleType,
     *Handle,
     *CompletionType);
  return rtn;  
}

/**************************************
SQLRETURN SQLExecDirect(
     SQLHSTMT     StatementHandle,
     SQLCHAR *    StatementText,
     SQLINTEGER   TextLength);
*/
int fvssqlexecdirect_(
     SQLHSTMT     *StatementHandle,
     SQLCHAR      *StatementText,
     SQLINTEGER   *TextLength)
{
  int rtn;     
  rtn = SQLExecDirect(
     *StatementHandle,
      StatementText,
     *TextLength);
  return rtn;  
}

/**************************************
SQLRETURN SQLExecute(
     SQLHSTMT     StatementHandle);
*/
int fvssqlexecute_(
     SQLHSTMT     *StatementHandle)
{
  return SQLExecute(*StatementHandle);
}
        

/************************************** 
SQLRETURN SQLFetch(
     SQLHSTMT     StatementHandle);
*/
int fvssqlfetch_(
     SQLHSTMT     *StatementHandle)
{
  return SQLFetch(*StatementHandle);
}

        
/************************************** 
SQLRETURN SQLFetchScroll(
      SQLHSTMT      StatementHandle,
      SQLSMALLINT   FetchOrientation,
      SQLLEN        FetchOffset);
*/
int fvssqlfetchscroll_(
      SQLHSTMT      *StatementHandle,
      SQLSMALLINT   *FetchOrientation,
      SQLLEN        *FetchOffset)
{
  return SQLFetchScroll(
     *StatementHandle,
     *FetchOrientation,
     *FetchOffset);
}
        

/**************************************
SQLRETURN SQLFreeHandle(
     SQLSMALLINT   HandleType,
     SQLHANDLE     Handle);
*/
int fvssqlfreehandle_(
     SQLSMALLINT  *HandleType,
     SQLHANDLE    *Handle)
{      
 return SQLFreeHandle(*HandleType, *Handle);
}      


/************************************** 
SQLRETURN SQLFreeStmt(
     SQLHSTMT       StatementHandle,
     SQLUSMALLINT   Option);
*/
int fvssqlfreestmt_(
     SQLHSTMT       *StatementHandle,
     SQLUSMALLINT   *Option)
{
  return SQLFreeStmt(
     *StatementHandle,
     *Option);
}


/**************************************
SQLRETURN SQLGetData(
      SQLHSTMT       StatementHandle,
      SQLUSMALLINT   Col_or_Param_Num,
      SQLSMALLINT    TargetType,
      SQLPOINTER     TargetValuePtr,
      SQLLEN         BufferLength,
      SQLLEN *       StrLen_or_IndPtr);
*/
int fvssqlgetdata_(
      SQLHSTMT       *StatementHandle,
      SQLUSMALLINT   *Col_or_Param_Num,
      SQLSMALLINT    *TargetType,
      SQLPOINTER     *TargetValuePtr,
      SQLLEN         *BufferLength,
      SQLLEN         *StrLen_or_IndPtr)
{
  int rtn;     
  rtn = SQLGetData(
     *StatementHandle,
     *Col_or_Param_Num,
     *TargetType,
     *TargetValuePtr,
     *BufferLength,
      StrLen_or_IndPtr);
  return rtn;  
}     
      
      
      
/**************************************
SQLRETURN SQLGetDiagRec(
     SQLSMALLINT     HandleType,
     SQLHANDLE       Handle,
     SQLSMALLINT     RecNumber,
     SQLCHAR *       SQLState,
     SQLINTEGER *    NativeErrorPtr,
     SQLCHAR *       MessageText,
     SQLSMALLINT     BufferLength,
     SQLSMALLINT *   TextLengthPtr);
*/
int fvssqlgetdiagrec_(
     SQLSMALLINT     *HandleType,
     SQLHANDLE       *Handle,
     SQLSMALLINT     *RecNumber,
     SQLCHAR         *SQLState,
     SQLINTEGER      *NativeErrorPtr,
     SQLCHAR         *MessageText,
     SQLSMALLINT     *BufferLength,
     SQLSMALLINT     *TextLengthPtr)
{
  int rtn;
 
  rtn = SQLGetDiagRec(
     *HandleType,
     *Handle,
     *RecNumber,
      SQLState,
      NativeErrorPtr,
      MessageText,
     *BufferLength,
      TextLengthPtr);

  return rtn;  
}     

/************************************** 
SQLRETURN SQLGetInfo(
     SQLHDBC         ConnectionHandle,
     SQLUSMALLINT    InfoType,
     SQLPOINTER      InfoValuePtr,
     SQLSMALLINT     BufferLength,
     SQLSMALLINT *   StringLengthPtr);
*/
int fvssqlgetinfo_(
     SQLHDBC         *ConnectionHandle,
     SQLUSMALLINT    *InfoType,
     SQLPOINTER      *InfoValuePtr,
     SQLSMALLINT     *BufferLength,
     SQLSMALLINT     *StringLengthPtr)
{
  int rtn;     
  rtn = SQLGetInfo(
     *ConnectionHandle,
     *InfoType,
      InfoValuePtr,
     *BufferLength,
      StringLengthPtr);
  return rtn;  
}
        

/**************************************
SQLRETURN SQLNumResultCols(
     SQLHSTMT        StatementHandle,
     SQLSMALLINT *   ColumnCountPtr);
*/
int fvssqlnumresultcols_(
     SQLHSTMT        *StatementHandle,
     SQLSMALLINT     *ColumnCountPtr)
{
  return SQLNumResultCols(
     *StatementHandle,
      ColumnCountPtr);
}
        

/**************************************
SQLRETURN SQLPrepare(
     SQLHSTMT      StatementHandle,
     SQLCHAR *     StatementText,
     SQLINTEGER    TextLength);
*/
int fvssqlprepare_(
     SQLHSTMT      *StatementHandle,
     SQLCHAR       *StatementText,
     SQLINTEGER    *TextLength)
{
  int rtn;     
  rtn = SQLPrepare(
    *StatementHandle,
     StatementText,
    *TextLength);
  return rtn;  
}
        

/**************************************
SQLRETURN SQLSetConnectAttr(
     SQLHDBC       ConnectionHandle,
     SQLINTEGER    Attribute,
     SQLPOINTER    ValuePtr,
     SQLINTEGER    StringLength);
*/
int fvssqlsetconnectattr_(
     SQLHDBC       *ConnectionHandle,
     SQLINTEGER    *Attribute,
     SQLPOINTER    *ValuePtr,
     SQLINTEGER    *StringLength)
{
  int rtn;     
  rtn = SQLSetConnectAttr(
     *ConnectionHandle,
     *Attribute,
      ValuePtr,
     *StringLength);
  return rtn;  
}
        

/**************************************
SQLRETURN SQLSetEnvAttr(
     SQLHENV      EnvironmentHandle,
     SQLINTEGER   Attribute,
     SQLPOINTER   ValuePtr,
     SQLINTEGER   StringLength);
*/
int fvssqlsetenvattr_(
     SQLHENV    *EnvironmentHandle,
     SQLINTEGER *Attribute,
     SQLPOINTER *ValuePtr,
     SQLINTEGER *StringLength)
{     
 int rtn = SQLSetEnvAttr(
   *EnvironmentHandle, 
   *Attribute, 
   *ValuePtr, 
   0);             // works when ValuePtr points to a number. 
//  *StringLength);   should work   
 return rtn;
}      


