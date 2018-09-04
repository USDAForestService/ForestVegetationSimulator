//  DBS $Id$

// c program to build DBSTYPEDEFS.F77 for the system on which it is run.

#ifdef WINDOWS
#include <windows.h>
#endif

#include <stdio.h>
#include <sql.h>
#include <sqlext.h>

int main(void)
{

  SQLCHAR       SQLCHAR_KIND;     
  SQLSMALLINT   SQLSMALLINT_KIND;  
  SQLUSMALLINT  SQLUSMALLINT_KIND; 
  SQLINTEGER    SQLINTEGER_KIND;   
  SQLUINTEGER   SQLUINTEGER_KIND; 
  SQLREAL       SQLREAL_KIND;      
  SQLDOUBLE     SQLDOUBLE_KIND;    
  SQLLEN        SQLLEN_KIND;       
  SQLFLOAT      SQLFLOAT_KIND;   
  SQLRETURN     SQLRETURN_KIND;   
  SQLPOINTER    SQLPOINTER_KIND;
  SQLHANDLE     SQLHANDLE_KIND;    
  SQLHENV       SQLHENV_KIND;      
  SQLHDBC       SQLHDBC_KIND;      
  SQLHSTMT      SQLHSTMT_KIND;     
  SQLHDESC      SQLHDESC_KIND;     
  SQLHWND       SQLHWND_KIND;      
  SQLULEN       SQLULEN_KIND;      
  
  FILE *out;
  out = fopen("DBSTYPEDEFS.F77","w");
  fprintf(out,"CODE SEGMENT DBSTYPEDEFS\n");
  fprintf(out,"C\n");
  fprintf(out,"C  generated using mkbsTypeDefs.c\n");
  fprintf(out,"C\n\n");
 
  fprintf(out,"      integer,parameter:: SQLCHAR_KIND=      %d\n",(int) sizeof (SQLCHAR_KIND     ));             
  fprintf(out,"      integer,parameter:: SQLSMALLINT_KIND=  %d\n",(int) sizeof (SQLSMALLINT_KIND));             
  fprintf(out,"      integer,parameter:: SQLUSMALLINT_KIND= %d\n",(int) sizeof (SQLUSMALLINT_KIND));
  fprintf(out,"      integer,parameter:: SQLINTEGER_KIND=   %d\n",(int) sizeof (SQLINTEGER_KIND  ));             
  fprintf(out,"      integer,parameter:: SQLUINTEGER_KIND=  %d\n",(int) sizeof (SQLUINTEGER_KIND ));
  fprintf(out,"      integer,parameter:: SQLREAL_KIND=      %d\n",(int) sizeof (SQLREAL_KIND     ));             
  fprintf(out,"      integer,parameter:: SQLDOUBLE_KIND=    %d\n",(int) sizeof (SQLDOUBLE_KIND   ));            
  fprintf(out,"      integer,parameter:: SQLLEN_KIND=       %d\n",(int) sizeof (SQLLEN_KIND      ));
  fprintf(out,"      integer,parameter:: SQLFLOAT_KIND=     %d\n",(int) sizeof (SQLFLOAT_KIND    ));
  fprintf(out,"      integer,parameter:: SQLRETURN_KIND=    %d\n",(int) sizeof (SQLRETURN_KIND   ));
  fprintf(out,"\n");
  fprintf(out,"      integer,parameter:: SQLPOINTER_KIND=   %d\n",(int) sizeof (SQLPOINTER_KIND  ));
  fprintf(out,"      integer,parameter:: SQLHANDLE_KIND=    %d\n",(int) sizeof (SQLHANDLE_KIND   ));
  fprintf(out,"      integer,parameter:: SQLHENV_KIND=      %d\n",(int) sizeof (SQLHENV_KIND     ));
  fprintf(out,"      integer,parameter:: SQLHDBC_KIND=      %d\n",(int) sizeof (SQLHDBC_KIND     ));
  fprintf(out,"      integer,parameter:: SQLHSTMT_KIND=     %d\n",(int) sizeof (SQLHSTMT_KIND    ));
  fprintf(out,"      integer,parameter:: SQLHDESC_KIND=     %d\n",(int) sizeof (SQLHDESC_KIND    ));
  fprintf(out,"      integer,parameter:: SQLHWND_KIND=      %d\n",(int) sizeof (SQLHWND_KIND     ));
  fprintf(out,"      integer,parameter:: SQLULEN_KIND=      %d\n",(int) sizeof (SQLULEN_KIND     ));
  fprintf(out,"\n");
  fprintf(out,"      integer,parameter:: SQL_OV_ODBC3_KIND= %d\n",(int) sizeof (SQL_OV_ODBC3));
  fprintf(out,"\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLAllocHandle\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLBindCol\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLBindParameter\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLCloseCursor\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLColAttribute\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLGetData\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLDescribeCol\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLDisconnect\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLDriverConnect\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLDrivers\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLEndTran\n");   
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLExecDirect\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLExecute\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLFetch\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLFetchScroll\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLFreeHandle\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLFreeStmt\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLGetDiagRec\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLGetInfo\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLNumResultCols\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLPrepare\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLSetConnectAttr\n");
  fprintf(out,"      integer(SQLRETURN_KIND) :: fvsSQLSetEnvAttr\n");
  fprintf(out,"\n");
  fprintf(out,"      integer(SQL_OV_ODBC3_KIND), parameter::\n");
  fprintf(out,"     -                         SQL_OV_ODBC3 = %d\n", (int) SQL_OV_ODBC3);
  fprintf(out,"      integer(SQLPOINTER_KIND),   parameter::\n");
  fprintf(out,"     -                         SQL_NULL_PTR = 0\n");
  fprintf(out,"      integer(SQLINTEGER_KIND),   parameter::\n");
  fprintf(out,"     -                        SQL_NULL_DATA = %d\n", (int) SQL_NULL_DATA );
  fprintf(out,"      integer(SQLINTEGER_KIND),   parameter::\n");
  fprintf(out,"     -                     SQL_DATA_AT_EXEC = %d\n", (int) SQL_DATA_AT_EXEC);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                          SQL_SUCCESS = %d\n", (int) SQL_SUCCESS);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                SQL_SUCCESS_WITH_INFO = %d\n", (int) SQL_SUCCESS_WITH_INFO);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                          SQL_NO_DATA = %d\n", (int) SQL_NO_DATA);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                            SQL_ERROR = %d\n", (int) SQL_ERROR);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                   SQL_INVALID_HANDLE = %d\n", (int) SQL_INVALID_HANDLE);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                  SQL_STILL_EXECUTING = %d\n", (int) SQL_STILL_EXECUTING);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                        SQL_NEED_DATA = %d\n", (int) SQL_NEED_DATA);
  fprintf(out,"      integer(SQLRETURN_KIND),    parameter::\n");
  fprintf(out,"     -                   SQL_NO_IMPLEMENTED = %d\n", (int) SQL_NEED_DATA);
  fprintf(out,"      integer(SQLINTEGER_KIND),   parameter::\n");
  fprintf(out,"     -                              SQL_NTS = %d\n", (int) SQL_NTS);
  fprintf(out,"      integer(SQLINTEGER_KIND),   parameter::\n");
  fprintf(out,"     -                             SQL_NTSL = %d\n", (int) SQL_NTSL);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -               SQL_MAX_MESSAGE_LENGTH = %d\n", (int) SQL_MAX_MESSAGE_LENGTH);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                       SQL_HANDLE_ENV = %d\n", (int) SQL_HANDLE_ENV);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                       SQL_HANDLE_DBC = %d\n", (int) SQL_HANDLE_DBC);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                      SQL_HANDLE_STMT = %d\n", (int) SQL_HANDLE_STMT);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                      SQL_HANDLE_DESC = %d\n", (int) SQL_HANDLE_DESC);
  fprintf(out,"      integer(SQLINTEGER_KIND),   parameter::\n");
  fprintf(out,"     -                SQL_ATTR_ODBC_VERSION = %d\n", (int) SQL_ATTR_ODBC_VERSION);
  fprintf(out,"      integer(SQLUSMALLINT_KIND), parameter::\n");
  fprintf(out,"     -                            SQL_CLOSE = %d\n", (int) SQL_CLOSE);
  fprintf(out,"      integer(SQLUSMALLINT_KIND), parameter::\n");
  fprintf(out,"     -                       SQL_AUTOCOMMIT = %d\n", (int) SQL_AUTOCOMMIT);
  fprintf(out,"      integer(SQLUINTEGER_KIND),  parameter::\n");
  fprintf(out,"     -                    SQL_AUTOCOMMIT_ON = %d\n", (int) SQL_AUTOCOMMIT); 
  fprintf(out,"      integer(SQLUINTEGER_KIND),  parameter::\n");
  fprintf(out,"     -                  SQL_DRIVER_NOPROMPT = %d\n", (int) SQL_DRIVER_NOPROMPT); 
  fprintf(out,"      integer(SQLUSMALLINT_KIND), parameter::\n");
  fprintf(out,"     -                  SQL_DRIVER_COMPLETE = %d\n", (int) SQL_DRIVER_COMPLETE);
  fprintf(out,"      integer(SQLUSMALLINT_KIND), parameter::\n");
  fprintf(out,"     -                      SQL_ACCESS_MODE = %d\n", (int) SQL_ACCESS_MODE);
  fprintf(out,"      integer(SQLUINTEGER_KIND),  parameter::\n");
  fprintf(out,"     -                  SQL_MODE_READ_WRITE = %d\n", (int) SQL_MODE_READ_WRITE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                             SQL_CHAR = %d\n", (int) SQL_CHAR);
  fprintf(out,"      integer(SQLUSMALLINT_KIND), parameter::\n");
  fprintf(out,"     -                        SQL_DBMS_NAME = %d\n", (int) SQL_DBMS_NAME);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                        SQL_DESC_NAME = %d\n", (int) SQL_DESC_NAME);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                        SQL_DESC_TYPE = %d\n", (int) SQL_DESC_TYPE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                           SQL_DOUBLE = %d\n", (int) SQL_DOUBLE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                          SQL_INTEGER = %d\n", (int) SQL_INTEGER);
  fprintf(out,"      integer(SQLHANDLE_KIND),    parameter::\n");
  fprintf(out,"     -                      SQL_NULL_HANDLE = %d\n", (int) SQL_NULL_HANDLE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                      SQL_PARAM_INPUT = %d\n", (int) SQL_PARAM_INPUT);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                             SQL_REAL = %d\n", (int) SQL_REAL);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                          SQL_VARCHAR = %d\n", (int) SQL_VARCHAR);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                      SQL_LONGVARCHAR = %d\n", (int) SQL_LONGVARCHAR);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                           SQL_C_LONG = %d\n", (int) SQL_INTEGER);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                           SQL_F_CHAR = %d\n", (int) SQL_CHAR);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                          SQL_F_FLOAT = %d\n", (int) SQL_REAL);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                         SQL_F_DOUBLE = %d\n", (int) SQL_DOUBLE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                        SQL_F_INTEGER = %d\n", (int) SQL_INTEGER);  
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                    SQL_SIGNED_OFFSET = %d\n", (int) SQL_SIGNED_OFFSET); 
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                  SQL_UNSIGNED_OFFSET = %d\n", (int) SQL_UNSIGNED_OFFSET);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                          SQL_F_SLONG = %d\n", (int) SQL_C_LONG+SQL_SIGNED_OFFSET);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                          SQL_F_ULONG = %d\n", (int) SQL_C_LONG+SQL_UNSIGNED_OFFSET); 
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                       SQL_FETCH_NEXT = %d\n", (int) SQL_FETCH_NEXT);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                      SQL_FETCH_FIRST = %d\n", (int) SQL_FETCH_FIRST);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                       SQL_FETCH_LAST = %d\n", (int) SQL_FETCH_LAST);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                      SQL_FETCH_PRIOR = %d\n", (int) SQL_FETCH_PRIOR);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                   SQL_FETCH_ABSOLUTE = %d\n", (int) SQL_FETCH_ABSOLUTE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                   SQL_FETCH_RELATIVE = %d\n", (int) SQL_FETCH_RELATIVE);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                           SQL_COMMIT = %d\n", (int) SQL_COMMIT);
  fprintf(out,"      integer(SQLSMALLINT_KIND),  parameter::\n");
  fprintf(out,"     -                         SQL_ROLLBACK = %d\n", (int) SQL_ROLLBACK);
  fprintf(out,"\nC----- END SEGMENT\n");
  fclose(out);
  
  /*
     Interface blocks for Fotran/C calls - this approach causes difficulties for some calls
     that return pointers to arrays of different types. The Intel ifort compiler complains
     about different types. It is being bypassed in favor of the compiler-dependent
     calling pattern shown in fvsSQL.c - DR/ESSA.
  */

  /*
  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLEndTran(HT,H,CT)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlendtran_'::fvsSQLEndTran\n");
  fprintf(out2,"           integer(%d) HT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) H\n",  sizeof(SQLHANDLE_KIND));
  fprintf(out2,"           integer(%d) CT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLDisconnect(H)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqldisconnect_'::fvsSQLDisconnect\n");
  fprintf(out2,"           integer(%d) H\n", sizeof(SQLHDBC_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLFreeHandle(HT,H)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlfreehandle_'::fvsSQLFreeHandle\n");
  fprintf(out2,"           integer(%d) HT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) H\n", sizeof(SQLHANDLE_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLAllocHandle(HT,IH,OHP)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlallochandle_'::fvsSQLAllocHandle\n");
  fprintf(out2,"           integer(%d) HT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) IH\n", sizeof(SQLHANDLE_KIND));
  fprintf(out2,"           integer(%d) OHP\n", sizeof(SQLHANDLE_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLExecDirect(SH,ST,TL)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlexecdirect_'::fvsSQLExecDirect\n");
  fprintf(out2,"           integer(%d) SH\n", sizeof(SQLHSTMT_KIND));
  fprintf(out2,"           character(%d) ST\n", sizeof(SQLCHAR_KIND));
  fprintf(out2,"           integer(%d) TL\n", sizeof(SQLINTEGER_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLCloseCursor(SH)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlclosecursor_'::fvsSQLCloseCursor\n");
  fprintf(out2,"           integer(%d) SH\n", sizeof(SQLHSTMT_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLBindParameter(SH,PN,IOT,VT,PT,CS,\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"     >    DD,PVP,BL,SLI)\n");
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlbindparameter_'::fvsSQLBindParameter\n");
  fprintf(out2,"           integer(%d) SH\n", sizeof(SQLHSTMT_KIND));
  fprintf(out2,"           integer(%d) PN\n", sizeof(SQLUSMALLINT_KIND));
  fprintf(out2,"           integer(%d) IOT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) VT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) PT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) CS\n", sizeof(SQLULEN_KIND));
  fprintf(out2,"           integer(%d) DD\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) PVP\n", sizeof(SQLPOINTER_KIND)); 
  fprintf(out2,"           integer(%d) BL\n", sizeof(SQLLEN_KIND));
  fprintf(out2,"           integer(%d) SLI\n", sizeof(SQLLEN_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");

  fprintf(out2,"      INTERFACE\n");
  fprintf(out2,"        integer(%d) function fvsSQLBindCol(SH,CN,TT,TVP,BL,SLI)\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"        !dec$ attributes alias:'_fvssqlbindcol_'::fvsSQLBindCol\n");
  fprintf(out2,"           integer(%d) SH\n", sizeof(SQLHSTMT_KIND));
  fprintf(out2,"           integer(%d) CN\n", sizeof(SQLUSMALLINT_KIND));
  fprintf(out2,"           integer(%d) TT\n", sizeof(SQLSMALLINT_KIND));
  fprintf(out2,"           integer(%d) TVP\n", sizeof(SQLPOINTER_KIND));
  fprintf(out2,"           integer(%d) BL\n", sizeof(SQLLEN_KIND));
  fprintf(out2,"           integer(%d) SLI\n", sizeof(SQLLEN_KIND));
  fprintf(out2,"        end function\n");
  fprintf(out2,"      end interface\n\n");
  
  fclose(out2);
  */
  return(0);
}

