// fsql3. is a C lanaguage shim that provides an interface between Fortran and 
// the Sqlite3 api. Nick Crookston, ncrookston.fs@gmail.com, Oct 2017

// DBSQLITE $Id$

// the basic goal is to provide an interface that can populate tables
// from fortran and get values from existing tables. 
// The datatype mapping (note, fortran real*4 is not supported for binds)
// sqlite3 Fortran
// ------- ---------
// int     integer
// double  real*8
// text    character
// blob    **NOT SUPPORTED**


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "sqlite3.h"

// fsql3_addcolifabsent
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_addcolifabsent_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_addcolifabsent_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_ADDCOLIFABSENT     // VS2010 compiler, Windows OS
#endif
(int *dbnum, char *tname, char *cname, char *cdef);

// fsql3_bind_double
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_bind_double_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_bind_double_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_BIND_DOUBLE     // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *order, double *realvalue);

//fsql3_bind_int
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_bind_int_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_bind_int_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_BIND_INT     // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *order, int *intvalue);
 
//fsql3_bind_text
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_bind_text                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_bind_text_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_BIND_TEXT     // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *order,char *txt, int *txtlen);

//fsql3_close
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_close_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_close_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_CLOSE     // VS2010 compiler, Windows OS
#endif
(int *dbnum);

//fsql3_colcnt
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_colcnt_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_colcnt_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_COLCNT     // VS2010 compiler, Windows OS
#endif
(int *dbnum);

//fsq3_coldouble
#ifdef CMPgcc
  #ifdef unix
    extern double fsql3_coldouble_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) double fsql3_coldouble_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) double FSQL3_COLDOUBLE     // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col, double *ifnull);

//fsq3_colint
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_colint_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_colint_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_COLINT    // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col, int *ifnull);

//fsq3_colisnull
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_colisnull_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_colisnull_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_COLISNULL    // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col);

//fsq3_colname
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_colname_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_colname_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_COLNAME    // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col, char *txt, int *mxlen);

//fsq3_colreal
#ifdef CMPgcc
  #ifdef unix
    extern float fsql3_colreal_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) float fsql3_colreal_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) float FSQL3_COLREAL    // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col, float *ifnull);

//fsq3_coltext
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_coltext_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_coltext_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_COLTEXT    // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col, char *txt, int *mxlen, char *ifnull);

//fsq3_coltype
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_coltype_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_coltype_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_COLTYPE    // VS2010 compiler, Windows OS
#endif
(int *dbnum, int *col);

//fsq3_errmsg
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_errmsg_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_errmsg_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_ERRMSG    // VS2010 compiler, Windows OS
#endif
(int *dbnum, char *msg, int  *mxlen);

//fsql3_exec
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_exec_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_exec_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_EXEC    // VS2010 compiler, Windows OS
#endif
(int *dbnum, char *sql);

//fsql3_finalize
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_finalize_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_finalize_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_FINALIZE     // VS2010 compiler, Windows OS
#endif
(int *dbnum);

//fsql3_open
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_open_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_open_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_OPEN     // VS2010 compiler, Windows OS
#endif
(int *dbnum, char *dbname);

//fsql3_prepare
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_prepare_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_prepare_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_PREPARE     // VS2010 compiler, Windows OS
#endif
(int *dbnum, char *sql);

//fsql3_reset
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_reset_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_reset_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_RESET    // VS2010 compiler, Windows OS
#endif
(int *dbnum);

//fsql3_step
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_step_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_step_  // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_STEP    // VS2010 compiler, Windows OS
#endif
(int *dbnum);

//fsql3_tableexists
#ifdef CMPgcc
  #ifdef unix
    extern int fsql3_tableexists_                        // GCC compiler, Unix OS
  #else
    extern __declspec(dllexport) int fsql3_tableexists_ // GCC compiler, Windows OS
  #endif
#else
  extern __declspec(dllexport) int FSQL3_TABLEEXISTS   // VS2010 compiler, Windows OS
#endif
(int *dbnum, char *tname);

sqlite3 *db = NULL;
sqlite3_stmt *stmt = NULL;

// set the maximum number of opened databases, for FVS, one input and one output
#define fsql3_MXDBS 2

//Visual Studio earlier than VS2015 does not have snprintf() or vsnprintf()
#if defined(_MSC_VER) && _MSC_VER < 1900

#define snprintf c99_snprintf
#define vsnprintf c99_vsnprintf

__inline int c99_vsnprintf(char *outBuf, size_t size, const char *format, va_list ap)
{
    int count = -1;
    if (size != 0)
        count = _vsnprintf_s(outBuf, size, _TRUNCATE, format, ap);
    if (count == -1)
        count = _vscprintf(format, ap);
    return count;
}

__inline int c99_snprintf(char *outBuf, size_t size, const char *format, ...)
{
    int count;
    va_list ap;
    va_start(ap, format);
    count = c99_vsnprintf(outBuf, size, format, ap);
    va_end(ap);
    return count;
}
#endif
///

int fsql3_dbsNum = -1;
sqlite3 *dbset[fsql3_MXDBS];
sqlite3_stmt *stmtset[fsql3_MXDBS];

static int busy_handler(void *data, int retry)
{
  static int sleep_ms=10;
  static int max_retry=2000;
  //fprintf(stderr,"DB SQLITE_BUSY hit %d times, max= %d, ms= %d\n",retry,max_retry,sleep_ms);
  if (retry < max_retry) {
		// Sleep a while and retry again. 
		//fprintf(stderr,"DB SQLITE_BUSY hit %d times (max= %d), retry.\n",retry,max_retry);
		sqlite3_sleep(sleep_ms); 
		// Return non-zero to let caller retry again. 
		return 1;
	}
	// Return zero to let caller return SQLITE_BUSY immediately.
	//fprintf(stderr,"Error: DB retried %d times and is exiting.\n",retry);
	return 0;
}

// returns the result of sqlite3_open, the database number is stored in dbnum
// these values are 0 based. 

#ifdef CMPgcc
int fsql3_open_
#else
int FSQL3_OPEN
#endif
(int *dbnum, char *dbname)
{
  int i,rc;
  *dbnum = -1;
  if (fsql3_dbsNum == -1) 
  {
    for (i=0; i<fsql3_MXDBS; i++) dbset[i] = NULL;
    fsql3_dbsNum=0;
    *dbnum = 0;
  } else {  
  // find an open slot if there is one.
    for (i=0; i<fsql3_MXDBS; i++) 
    {
      if (dbset[i] == NULL)
      {
        *dbnum = i;
        break;
      }
    }
  }
  rc = -1;
  if (*dbnum > -1) rc = sqlite3_open(dbname, &dbset[*dbnum]);
  if (rc == 0) 
  {
    sqlite3_busy_handler(dbset[*dbnum], busy_handler, NULL);
    sqlite3_exec(dbset[*dbnum],"PRAGMA synchronous=0;", NULL, NULL, NULL);
    sqlite3_exec(dbset[*dbnum],"PRAGMA temp_store=2;", NULL, NULL, NULL);
    sqlite3_exec(dbset[*dbnum],"PRAGMA journal_mode=MEMORY;", NULL, NULL, NULL);
  }
	return rc;
}
// close the dbnum and clear the slot for another open
#ifdef CMPgcc
int fsql3_close_
#else
int FSQL3_CLOSE
#endif
(int *dbnum)
{
  int rc = 0;
  if (dbset[*dbnum] != NULL) 
  {
    rc = sqlite3_close(dbset[*dbnum]);
    //if (rc > 0) printf("Database error: %s\n",sqlite3_errmsg(dbset[*dbnum]));
    dbset[*dbnum] = NULL;
  }
  return rc;
}
// query without returned values
#ifdef CMPgcc
int fsql3_exec_
#else
int FSQL3_EXEC
#endif
(int *dbnum, char *sql)
{ 
  int rc = sqlite3_exec(dbset[*dbnum], sql, NULL, NULL, NULL);
  //fprintf (stderr," exec rc= %d, SQLITE_BUSY= %d\n",rc,SQLITE_BUSY);
  if (rc == SQLITE_BUSY) fprintf(stderr, "Database locked\n");
  return rc;
}

// query with returned value passed back as a character string with max length
#define fsql3_MXCBSTR 2000
struct cbmsgstr {
  int len;
  char msg[fsql3_MXCBSTR];
};
// the callback for dbexec2.
int dbexeccallback(void *cb, 
    int argc, char **argv, char **azColName)
{
  int rnt = 0;
  int i;
  struct cbmsgstr *cbmsg = (struct cbmsgstr *) cb;
  char *ans = cbmsg->msg+cbmsg->len;
  for(i=0; i<argc; i++)
  {
    if (fsql3_MXCBSTR-strlen(cbmsg->msg)-1 <= 0) break;
    snprintf(ans, fsql3_MXCBSTR-strlen(cbmsg->msg)-1, "%s = %s\n", 
             azColName[i], argv[i] ? argv[i] : "NULL");
    //printf("msg callback=%s\n",cbmsg->msg);
    ans += strlen(ans);
  }
  cbmsg->len=strlen(cbmsg->msg);
  return 0;
}
// return the length of the returned string, might be an error message
int fsql3_exec2_(int *dbnum, char *sql, char *msg, int *txtlen)
{
  struct cbmsgstr cbmsg;
  char *zErrMsg = 0;
  int rc;
  cbmsg.len=0;
  cbmsg.msg[0]=0;
  rc = sqlite3_exec(dbset[*dbnum], /* An open database */
           sql,                        /* SQL to be evaluated */
           dbexeccallback,             /* Callback function */
           (void *) &cbmsg,            /* 1st argument to callback */
           &zErrMsg);                  /* Error msg written here */                           
  if(rc!=SQLITE_OK )       
  {
    fprintf(stderr, "SQL error: %s\n", zErrMsg);
    strncpy(msg,zErrMsg,*txtlen);
    sqlite3_free(zErrMsg);
  }
  strncpy(msg,cbmsg.msg,*txtlen);
  return strlen(msg);
} 
// standard prepare
#ifdef CMPgcc
int fsql3_prepare_
#else
int FSQL3_PREPARE
#endif
(int *dbnum, char *sql)
{//usleep(500000); = 1/2 second 
  int rc = sqlite3_prepare_v2(dbset[*dbnum], sql, -1, &stmtset[*dbnum], NULL);
  //fprintf (stderr," prepare rc= %d, SQLITE_BUSY= %d\n",rc,SQLITE_BUSY);
  if (rc == SQLITE_BUSY) fprintf(stderr, "Database locked\n");
  return rc;
}
// standard bind double
#ifdef CMPgcc
int fsql3_bind_double_
#else
int FSQL3_BIND_DOUBLE
#endif
(int *dbnum, int *order, double *realvalue)
{
	return sqlite3_bind_double(stmtset[*dbnum], *order, *realvalue);
}
// standard bind int
#ifdef CMPgcc
int fsql3_bind_int_
#else
int FSQL3_BIND_INT
#endif
(int *dbnum, int *order, int *intvalue)
{
	return sqlite3_bind_int(stmtset[*dbnum], *order, *intvalue);
}
// standard bind text
#ifdef CMPgcc
int fsql3_bind_text_
#else
int FSQL3_BIND_TEXT
#endif
(int *dbnum, int *order, char *txt, int *txtlen)
{
  // printf("order=%i, txt=%s, txtlen=%d\n",*order,txt,*txtlen);
	return sqlite3_bind_text(stmtset[*dbnum], *order, txt, *txtlen, NULL);
}
// standard step
#ifdef CMPgcc
int fsql3_step_
#else
int FSQL3_STEP
#endif
(int *dbnum)
{
  return SQLITE_ROW == sqlite3_step(stmtset[*dbnum]);
}
// standard reset
#ifdef CMPgcc
int fsql3_reset_
#else
int FSQL3_RESET
#endif
(int *dbnum)
{
  return sqlite3_reset(stmtset[*dbnum]);
}
// close the statement handle.
#ifdef CMPgcc
int fsql3_finalize_
#else
int FSQL3_FINALIZE
#endif
(int *dbnum)
{
  int rc = 0;
  if (stmtset[*dbnum] != NULL) 
  {
    rc = sqlite3_finalize(stmtset[*dbnum]);
    stmtset[*dbnum] = NULL;
  }
  return rc;
}
// get error current error message
#ifdef CMPgcc
int fsql3_errmsg_
#else
int FSQL3_ERRMSG
#endif
(int *dbnum, char *msg, int *mxlen)
{
  msg = strncpy(msg,sqlite3_errmsg(dbset[*dbnum]),*mxlen);
  return strlen(msg);
}
// get a column name
#ifdef CMPgcc
int fsql3_colname_
#else
int FSQL3_COLNAME
#endif
(int *dbnum, int *col, char *txt, int *mxlen)
{
  char *c;
  strncpy(txt,(const char *) sqlite3_column_name(stmtset[*dbnum], *col),*mxlen);
  // return col names in upper case.
  for(c=txt; *c != 0; ++c) *c=toupper(*c);
  return strlen(txt);
}
// get a column type
#ifdef CMPgcc
int fsql3_coltype_
#else
int FSQL3_COLTYPE
#endif
(int *dbnum, int *col)
{
  return sqlite3_column_type(stmtset[*dbnum], *col);
}
// get the number of columns
#ifdef CMPgcc
int fsql3_colcnt_
#else
int FSQL3_COLCNT
#endif
(int *dbnum)
{
  return sqlite3_column_count(stmtset[*dbnum]);
}
/*  Important constants taken from SQLite3 headers.
SQLITE_INTEGER  1
SQLITE_FLOAT    2
SQLITE3_TEXT    3
SQLITE_BLOB     4
SQLITE_NULL     5
Locked error code:
SQLITE_BUSY   6
*/

// get a integer, regardless of how the data are stored. Last argument
// is returned if the data is null or if conversion from text fails.
#ifdef CMPgcc
int fsql3_colint_
#else
int FSQL3_COLINT
#endif
(int *dbnum, int *col, int *ifnull)
{
  int rtn;
  switch (sqlite3_column_type(stmtset[*dbnum], *col))
  {
    case SQLITE_INTEGER : 
      return sqlite3_column_int(stmtset[*dbnum], *col);
    case SQLITE_FLOAT   :
      rtn = (int) sqlite3_column_double(stmtset[*dbnum], *col);
      return rtn;
    case SQLITE3_TEXT   :
      if (strlen((const char *) sqlite3_column_text(stmtset[*dbnum],*col)) > 0)
      {
        if (sscanf((const char *) sqlite3_column_text(stmtset[*dbnum], 
          *col),"%d",&rtn) == 1) return rtn;
      }
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      return *ifnull;
  }
  return *ifnull;
}
// get a double, see fsql3_colint
#ifdef CMPgcc
double fsql3_coldouble_
#else
double FSQL3_COLDOUBLE
#endif
(int *dbnum, int *col, double *ifnull)
{
  double rtn;
  switch (sqlite3_column_type(stmtset[*dbnum], *col))
  {
    case SQLITE_INTEGER : 
      rtn = (double) sqlite3_column_int(stmtset[*dbnum], *col);
      return rtn;
    case SQLITE_FLOAT   :
      return sqlite3_column_double(stmtset[*dbnum], *col);
    case SQLITE3_TEXT   :
      if (strlen((const char *) sqlite3_column_text(stmtset[*dbnum],*col)) > 0)
      {
        if (sscanf((const char *) sqlite3_column_text(stmtset[*dbnum], 
          *col),"%lf",&rtn) == 1) return rtn;
      }
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      return *ifnull;
  }
  return *ifnull;
}
// get a foat, see fsql3_colint
#ifdef CMPgcc
float fsql3_colreal_
#else
float FSQL3_COLREAL
#endif
(int *dbnum, int *col, float *ifnull)
{
  float rtn;
  switch (sqlite3_column_type(stmtset[*dbnum], *col))
  {
    case SQLITE_INTEGER : 
      rtn = (float) sqlite3_column_int(stmtset[*dbnum], *col);
      return rtn;
    case SQLITE_FLOAT   :
      rtn = (float) sqlite3_column_double(stmtset[*dbnum], *col);
      return rtn;
    case SQLITE3_TEXT   :
      if (strlen((const char *) sqlite3_column_text(stmtset[*dbnum],*col)) > 0)
      {
        if (sscanf((const char *) sqlite3_column_text(stmtset[*dbnum], 
          *col),"%f",&rtn) == 1) return rtn;
      }
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      return *ifnull;
  }
  return *ifnull;
}
// get text, see fsql3_colint, ifnull is returned if field is null
#ifdef CMPgcc
int fsql3_coltext_
#else
int FSQL3_COLTEXT
#endif
(int *dbnum, int *col, char *txt, int *mxlen, char *ifnull)
{
  txt[0]=(char)0;
  switch (sqlite3_column_type(stmtset[*dbnum], *col))
  {
    case SQLITE_INTEGER : 
      snprintf(txt,*mxlen,"%d",sqlite3_column_int(stmtset[*dbnum], *col));
      break;
    case SQLITE_FLOAT   :
      snprintf(txt,*mxlen,"%lf",sqlite3_column_double(stmtset[*dbnum], *col));
      break;
    case SQLITE3_TEXT   :
      if (strlen((const char *) sqlite3_column_text(stmtset[*dbnum],*col)) == 0) 
      {
        strncpy(txt,ifnull,*mxlen);
      } else {
        strncpy(txt,(const char *) sqlite3_column_text(stmtset[*dbnum], *col), *mxlen);
      }
      break;
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      strncpy(txt,ifnull,*mxlen);
  }
  return strlen(txt);
}
// is the column null
#ifdef CMPgcc
int fsql3_colisnull_
#else
int FSQL3_COLISNULL
#endif
(int *dbnum, int *col)
{
  int rtn = 0; // 0 if not null, 1 if null
  switch (sqlite3_column_type(stmtset[*dbnum], *col))
  {
    case SQLITE_INTEGER : 
    case SQLITE_FLOAT   :
      break;
    case SQLITE3_TEXT   :
      if (strlen((const char *) sqlite3_column_text(stmtset[*dbnum],*col)) == 0) rtn=1;
      break;
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      rtn=1;
  }
  return(rtn);
}

// If a col does not exist, then add it via alter table.
#ifdef CMPgcc
int fsql3_addcolifabsent_
#else
int FSQL3_ADDCOLIFABSENT
#endif
(int *dbnum, char *tname, char *cname, char *cdef)
{
  char sql[501];
  int rc;
  snprintf(sql,500,"select %s from %s limit 0;",cname,tname);
	rc = sqlite3_exec(dbset[*dbnum], sql, NULL, NULL, NULL);
	if (rc!=SQLITE_OK)       
  {
    snprintf(sql,500,"alter table %s add column %s %s;",tname,cname,cdef);
 	  rc = sqlite3_exec(dbset[*dbnum], sql, NULL, NULL, NULL);
  }
  return rc != SQLITE_OK;
}

// check to see if a table exists.
#ifdef CMPgcc
int fsql3_tableexists_
#else
int FSQL3_TABLEEXISTS
#endif
(int *dbnum, char *tname)
{
  char sql[501];
  int rc;
  snprintf(sql,500,"select _ROWID_ from sqlite_master where name = '%s';",tname);
  sqlite3_prepare_v2(dbset[*dbnum], sql, -1, &stmtset[*dbnum], NULL);
  rc = SQLITE_ROW == sqlite3_step(stmtset[*dbnum]);
  sqlite3_finalize(stmtset[*dbnum]);
  stmtset[*dbnum] = NULL;
  return rc;
}