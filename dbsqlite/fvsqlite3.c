// fsql3. is a C lanaguage shim that provides an interface between Fortran and 
// the Sqlite3 api. Nick Crookston, ncrookston.fs@gmail.com, Oct 2017

// $Id$

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

sqlite3 *db = NULL;
sqlite3_stmt *stmt = NULL;

// set the maximum number of opened databases.
#define fsql3_MXDBS 2

int fsql3_dbsNum = -1;
sqlite3 *dbset[fsql3_MXDBS];
sqlite3_stmt *stmtset[fsql3_MXDBS];

// returns the result of sqlite3_open, the database number is stored in dbnum
// these values are 0 based. 
int fsql3_open_(int *dbnum, char *dbname)
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
    sqlite3_exec(dbset[*dbnum],"PRAGMA synchronous=0;", NULL, NULL, NULL);
    sqlite3_exec(dbset[*dbnum],"PRAGMA temp_store=2;", NULL, NULL, NULL);
    sqlite3_exec(dbset[*dbnum],"PRAGMA journal_mode=MEMORY;", NULL, NULL, NULL);
  }
	return rc;
}
// close the dbnum and clear the slot for another open
int fsql3_close_(int *dbnum)
{
  int rc = 0;
  if (dbset[*dbnum] != NULL) 
  {
    rc = sqlite3_close(dbset[*dbnum]);
    dbset[*dbnum] = NULL;
  }
  return rc;
}
// query without returned values
int fsql3_exec_(int *dbnum, char *sql)
{  
	return sqlite3_exec(dbset[*dbnum], sql, NULL, NULL, NULL);
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
  cbmsg.len=0;
  cbmsg.msg[0]=0;
  char *zErrMsg = 0;
  int rc = sqlite3_exec(dbset[*dbnum], /* An open database */
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
int fsql3_prepare_(int *dbnum, char *sql)
{
  return sqlite3_prepare_v2(dbset[*dbnum], sql, -1, &stmtset[*dbnum], NULL);
}
// standard bind double
int fsql3_bind_double_(int *dbnum, int *order, double *realvalue)
{
	return sqlite3_bind_double(stmtset[*dbnum], *order, *realvalue);
}
// standard bind int
int fsql3_bind_int_(int *dbnum, int *order, int *intvalue)
{
	return sqlite3_bind_int(stmtset[*dbnum], *order, *intvalue);
}
// standard bind text
int fsql3_bind_text_(int *dbnum, int *order, char *txt, int *txtlen)
{
  // printf("order=%i, txt=%s, txtlen=%d\n",*order,txt,*txtlen);
	return sqlite3_bind_text(stmtset[*dbnum], *order, txt, *txtlen, NULL);
}
// standard step
int fsql3_step_(int *dbnum)
{
  return SQLITE_ROW == sqlite3_step(stmtset[*dbnum]);
}
// standard reset
int fsql3_reset_(int *dbnum)
{
  return sqlite3_reset(stmtset[*dbnum]);
}
// close the statement handle.
int fsql3_finalize_(int *dbnum)
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
int fsql3_errmsg_(int *dbnum, char *msg, int *mxlen)
{
  msg = strncpy(msg,sqlite3_errmsg(dbset[*dbnum]),*mxlen);
  return strlen(msg);
}
// get a column name
int fsql3_colname_(int *dbnum, int *col, char *txt, int *mxlen)
{
  char *c;
  strncpy(txt,(const char *) sqlite3_column_name(stmtset[*dbnum], *col),*mxlen);
  // return col names in upper case.
  for(c=txt; *c != 0; ++c) *c=toupper(*c);
  return strlen(txt);
}
// get a column type
int fsql3_coltype_(int *dbnum, int *col)
{
  return sqlite3_column_type(stmtset[*dbnum], *col);
}
// get the number of columns
int fsql3_colcnt_(int *dbnum)
{
  return sqlite3_column_count(stmtset[*dbnum]);
}
/*  Important constants taken from SQLite3 headers.
SQLITE_INTEGER  1
SQLITE_FLOAT    2
SQLITE3_TEXT    3
SQLITE_BLOB     4
SQLITE_NULL     5
*/

// get a integer, regardless of how the data are stored. Last argument
// is returned if the data is null or if conversion from text fails.
int fsql3_colint_(int *dbnum, int *col, int *ifnull)
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
      if (sscanf((const char *) sqlite3_column_text(stmtset[*dbnum], 
        *col),"%d",&rtn) == 1) return rtn; 
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      return *ifnull;
  }
  return *ifnull;
}
// get a double, see fsql3_colint
double fsql3_coldouble_(int *dbnum, int *col, double *ifnull)
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
      // did not use atod() because I want to capture an error if 
      // there is a conversion error. 
      if (sscanf((const char *) sqlite3_column_text(stmtset[*dbnum], 
        *col),"%lf",&rtn) == 1) return rtn;
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      return *ifnull;
  }
  return *ifnull;
}
// get a foat, see fsql3_colint
float fsql3_colreal_(int *dbnum, int *col, float *ifnull)
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
      // did not use atod() because I want to capture an error if
      // there is a conversion error. 
      if (sscanf((const char *) sqlite3_column_text(stmtset[*dbnum], 
        *col),"%f",&rtn) == 1) return rtn;
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      return *ifnull;
  }
  return *ifnull;
}
// get text, see fsql3_colint, ifnull is returned if field is null
int fsql3_coltext_(int *dbnum, int *col, char *txt, int *mxlen, char *ifnull)
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
      strncpy(txt,(const char *) sqlite3_column_text(stmtset[*dbnum], *col), *mxlen);
      break;
    case SQLITE_BLOB    :
    case SQLITE_NULL    :
    default:
      strncpy(txt,ifnull,*mxlen);
  }
  return strlen(txt);
}
// is the column null
int fsql3_colisnull_(int *dbnum, int *col)
{
  return sqlite3_column_type(stmtset[*dbnum], *col) == SQLITE_NULL;
}

// If a col does not exist, then add it via alter table.
int fsql3_addcolifabsent_(int *dbnum, char *tname, char *cname, char *cdef)
{
  char sql[501];
  snprintf(sql,500,"select %s from %s limit 0;",cname,tname);
	int rc = sqlite3_exec(dbset[*dbnum], sql, NULL, NULL, NULL);
	if (rc!=SQLITE_OK)       
  {
    snprintf(sql,500,"alter table %s add column %s %s;",tname,cname,cdef);
 	  rc = sqlite3_exec(dbset[*dbnum], sql, NULL, NULL, NULL);
  }
  return rc != SQLITE_OK;
}

// check to see if a table exists.
int fsql3_tableexists_(int *dbnum, char *tname)
{
  char sql[501];
  snprintf(sql,500,"select _ROWID_ from sqlite_master where name = '%s';",tname);
  sqlite3_prepare_v2(dbset[*dbnum], sql, -1, &stmtset[*dbnum], NULL);
  int rtn = SQLITE_ROW == sqlite3_step(stmtset[*dbnum]);
  sqlite3_finalize(stmtset[*dbnum]);
  stmtset[*dbnum] = NULL;
  return rtn;
}
