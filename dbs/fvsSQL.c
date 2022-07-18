// C language routines that link ODBC to FVS
// NLCrookston -- RMRS -- Moscow -- October 2011

//  $Id$

#ifdef WINDOWS
#include <windows.h>
#endif


#include <stdio.h>
#include <sql.h>
#include <sqlext.h>

/**************************************
SQLRETURN SQLAllocHandle(
      SQLSMALLINT   HandleType,
      SQLHANDLE     InputHandle,
      SQLHANDLE *   OutputHandlePtr);
*/
#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLALLOCHANDLE (
      SQLSMALLINT *HandleType,
      SQLHANDLE   *InputHandle,
      SQLHANDLE   *OutputHandlePtr);
#endif

#ifdef CMPgcc
int fvssqlallochandle_(
#else
int FVSSQLALLOCHANDLE(
#endif
      SQLSMALLINT *HandleType,
      SQLHANDLE   *InputHandle,
      SQLHANDLE   *OutputHandlePtr)
{
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
#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLBINDCOL (
      SQLHSTMT       *StatementHandle,
      SQLUSMALLINT   *ColumnNumber,
      SQLSMALLINT    *TargetType,
      SQLPOINTER     *TargetValuePtr,
      SQLLEN         *BufferLength,
      SQLLEN         *StrLen_or_Ind);
#endif

#ifdef CMPgcc
int fvssqlbindcol_(
#else
int FVSSQLBINDCOL(
#endif
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
#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLBINDPARAMETER(
      SQLHSTMT        *StatementHandle,
      SQLUSMALLINT    *ParameterNumber,
      SQLSMALLINT     *InputOutputType,
      SQLSMALLINT     *ValueType,
      SQLSMALLINT     *ParameterType,
      SQLULEN         *ColumnSize,
      SQLSMALLINT     *DecimalDigits,
      SQLPOINTER      *ParameterValuePtr,
      SQLLEN          *BufferLength,
      SQLLEN          *StrLen_or_IndPtr);
#endif

#ifdef CMPgcc
int fvssqlbindparameter_(
#else
int FVSSQLBINDPARAMETER(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLCLOSECURSOR(
        SQLHSTMT        *StatementHandle);
#endif

#ifdef CMPgcc
int fvssqlclosecursor_(
#else
int FVSSQLCLOSECURSOR(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLCOLATTRIBUTE(
      SQLHSTMT        *StatementHandle,
      SQLUSMALLINT    *ColumnNumber,
      SQLUSMALLINT    *FieldIdentifier,
      SQLPOINTER      *CharacterAttributePtr,
      SQLSMALLINT     *BufferLength,
      SQLSMALLINT     *StringLengthPtr,
      SQLLEN          *NumericAttributePtr);
#endif

#ifdef CMPgcc
int fvssqlcolattribute_(
#else
int FVSSQLCOLATTRIBUTE(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLDESCRIBECOL(
      SQLHSTMT       *StatementHandle,
      SQLUSMALLINT   *ColumnNumber,
      SQLCHAR        *ColumnName,
      SQLSMALLINT    *BufferLength,
      SQLSMALLINT    *NameLengthPtr,
      SQLSMALLINT    *DataTypePtr,
      SQLULEN        *ColumnSizePtr,
      SQLSMALLINT    *DecimalDigitsPtr,
      SQLSMALLINT    *NullablePtr);
#endif

#ifdef CMPgcc
int fvssqldescribecol_(
#else
int FVSSQLDESCRIBECOL(
#endif
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
/****************  DEBUG 
  printf("\nStatementHandle= %d\n",StatementHandle);
  printf(" ColumnNumber= %d\n",*ColumnNumber);
  printf(" ColumnName= %s\n",ColumnName);
  printf(" NameLengthPtr= %d\n",*NameLengthPtr);
  printf(" BufferLength= %d\n",*BufferLength);
 **************/
  return rtn;
}

/**************************************
SQLRETURN SQLDisconnect(
     SQLHDBC     ConnectionHandle);
*/

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLDISCONNECT(
    SQLHDBC     *ConnectionHandle);
#endif

#ifdef CMPgcc
int fvssqldisconnect_(
#else
int FVSSQLDISCONNECT(
#endif
    SQLHDBC     *ConnectionHandle)
{
  return SQLDisconnect(*ConnectionHandle);
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLDRIVERCONNECT(
     SQLHDBC         *ConnectionHandle,
     SQLHWND         *WindowHandle,
     SQLCHAR         *InConnectionString,
     SQLSMALLINT     *StringLength1,
     SQLCHAR         *OutConnectionString,
     SQLSMALLINT     *BufferLength,
     SQLSMALLINT     *StringLength2Ptr,
     SQLUSMALLINT    *DriverCompletion);
#endif

#ifdef CMPgcc
int fvssqldriverconnect_(
#else
int FVSSQLDRIVERCONNECT(
#endif
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
 **********************/
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
 **********************/

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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLDRIVERS(
     SQLHENV        *EnvironmentHandle,
     SQLUSMALLINT   *Direction,
     SQLCHAR        *DriverDescription,
     SQLSMALLINT    *BufferLength1,
     SQLSMALLINT    *DescriptionLengthPtr,
     SQLCHAR        *DriverAttributes,
     SQLSMALLINT    *BufferLength2,
     SQLSMALLINT    *AttributesLengthPtr);
#endif

#ifdef CMPgcc
int fvssqldrivers_(
#else
int FVSSQLDRIVERS(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLENDTRAN(
     SQLSMALLINT   *HandleType,
     SQLHANDLE     *Handle,
     SQLSMALLINT   *CompletionType);
#endif

#ifdef CMPgcc
int fvssqlendtran_(
#else
int FVSSQLENDTRAN(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLEXECDIRECT(
     SQLHSTMT     *StatementHandle,
     SQLCHAR      *StatementText,
     SQLINTEGER   *TextLength);
#endif

#ifdef CMPgcc
int fvssqlexecdirect_(
#else
int FVSSQLEXECDIRECT(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLEXECUTE(
     SQLHSTMT     *StatementHandle);
#endif

#ifdef CMPgcc
int fvssqlexecute_(
#else
int FVSSQLEXECUTE(
#endif
     SQLHSTMT     *StatementHandle)
{
  return SQLExecute(*StatementHandle);
}

/**************************************
SQLRETURN SQLFetch(
     SQLHSTMT     StatementHandle);
*/

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLFETCH(
     SQLHSTMT     *StatementHandle);
#endif

#ifdef CMPgcc
int fvssqlfetch_(
#else
int FVSSQLFETCH(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLFETCHSCROLL(
      SQLHSTMT      *StatementHandle,
      SQLSMALLINT   *FetchOrientation,
      SQLLEN        *FetchOffset);
#endif

#ifdef CMPgcc
int fvssqlfetchscroll_(
#else
int FVSSQLFETCHSCROLL(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLFREEHANDLE(
     SQLSMALLINT  *HandleType,
     SQLHANDLE    *Handle);
#endif

#ifdef CMPgcc
int fvssqlfreehandle_(
#else
int FVSSQLFREEHANDLE(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLFREESTMT(
     SQLHSTMT       *StatementHandle,
     SQLUSMALLINT   *Option);
#endif

#ifdef CMPgcc
int fvssqlfreestmt_(
#else
int FVSSQLFREESTMT(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLGETDATA(
      SQLHSTMT       *StatementHandle,
      SQLUSMALLINT   *Col_or_Param_Num,
      SQLSMALLINT    *TargetType,
      SQLPOINTER     *TargetValuePtr,
      SQLLEN         *BufferLength,
      SQLLEN         *StrLen_or_IndPtr);
#endif

#ifdef CMPgcc
int fvssqlgetdata_(
#else
int FVSSQLGETDATA(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLGETDIAGREC(
     SQLSMALLINT     *HandleType,
     SQLHANDLE       *Handle,
     SQLSMALLINT     *RecNumber,
     SQLCHAR         *SQLState,
     SQLINTEGER      *NativeErrorPtr,
     SQLCHAR         *MessageText,
     SQLSMALLINT     *BufferLength,
     SQLSMALLINT     *TextLengthPtr);
#endif

#ifdef CMPgcc
int fvssqlgetdiagrec_(
#else
int FVSSQLGETDIAGREC(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLGETINFO(
     SQLHDBC         *ConnectionHandle,
     SQLUSMALLINT    *InfoType,
     SQLPOINTER      *InfoValuePtr,
     SQLSMALLINT     *BufferLength,
     SQLSMALLINT     *StringLengthPtr);
#endif

#ifdef CMPgcc
int fvssqlgetinfo_(
#else
int FVSSQLGETINFO(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLNUMRESULTCOLS(
     SQLHSTMT        *StatementHandle,
     SQLSMALLINT     *ColumnCountPtr);
#endif

#ifdef CMPgcc
int fvssqlnumresultcols_(
#else
int FVSSQLNUMRESULTCOLS(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLPREPARE(
     SQLHSTMT      *StatementHandle,
     SQLCHAR       *StatementText,
     SQLINTEGER    *TextLength);
#endif

#ifdef CMPgcc
int fvssqlprepare_(
#else
int FVSSQLPREPARE(
#endif
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

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLSETCONNECTATTR(
     SQLHDBC       *ConnectionHandle,
     SQLINTEGER    *Attribute,
     SQLPOINTER    *ValuePtr,
     SQLINTEGER    *StringLength);
#endif

#ifdef CMPgcc
int fvssqlsetconnectattr_(
#else
int FVSSQLSETCONNECTATTR(
#endif
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

Note that the specification listed above and found at 
http://msdn.microsoft.com/en-us/library/ms709285%28v=vs.85%29.aspx
is not consistent with the calling arguments we use below. In our
implimentation, SQLSetEnvAttr is only called once to set the ODBC version
and we have had problems getting all this to work in general. So, we 
now use only 3 arguments and leave the 4th (*StringLength) as a zero. 

DLRobinson and NLCrookston May 2014.
*/

#ifdef _WINDLL
extern __declspec(dllexport) int FVSSQLSETENVATTR(
     SQLHENV    *EnvironmentHandle,
     SQLINTEGER *Attribute,
     SQLINTEGER *ValuePtr);
#endif

#ifdef CMPgcc
int fvssqlsetenvattr_(
#else
int FVSSQLSETENVATTR(
#endif
     SQLHENV    *EnvironmentHandle,
     SQLINTEGER *Attribute,
     SQLINTEGER *ValuePtr)
{
   int rtn = SQLSetEnvAttr(
   *EnvironmentHandle,
   *Attribute,
   (SQLPOINTER) *ValuePtr,
   (SQLINTEGER) 0);
 return rtn;
}

