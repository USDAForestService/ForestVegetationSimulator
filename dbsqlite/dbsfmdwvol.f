      SUBROUTINE DBSFMDWVOL(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD VOLUME REPORT
C              INFORMATION
C     AUTH: S. REBAIN
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C       UNITS ARE CUFT/ACRE
C         1 = DOWN WOOD 0 - 3 HARD
C         2 = DOWN WOOD 3 - 6 HARD
C         3 = DOWN WOOD 6 - 12 HARD
C         4 = DOWN WOOD 12 - 20 HARD
C         5 = DOWN WOOD 20 - 35 HARD
C         6 = DOWN WOOD 35 - 50 HARD
C         7 = DOWN WOOD 50+ HARD
C         8 = DOWN WOOD TOTAL HARD
C         9 = DOWN WOOD 0 - 3 SOFT
C         10 = DOWN WOOD 3 - 6 SOFT
C         11 = DOWN WOOD 6 - 12 SOFT
C         12 = DOWN WOOD 12 - 20 SOFT
C         13 = DOWN WOOD 20 - 35 SOFT
C         14 = DOWN WOOD 35 - 50 SOFT
C         15 = DOWN WOOD 50+ SOFT
C         16 = DOWN WOOD TOTAL SOFT
C       KODE   - RETURN CODE

COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,KODE,VARDIM
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           iRet,I,ColNumber
      CHARACTER*2000    SQLStmtStr

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      IF(IDWDVOL.EQ.0) RETURN
      IF(IDWDVOL.EQ.2) KODE = 0

      CALL DBSCASE(1)

C     INSURE THE DOWN WOOD VOLUME TABLE EXISTS IN DATBASE
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Down_Wood_Vol"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Down_Wood_Vol('//
     -        'CaseID char(36) not null,'//
     -        'StandID char(26) not null,'//
     -        'Year Int null,'//
     -        'DWD_Volume_0to3_Hard real null,'//
     -        'DWD_Volume_3to6_Hard real null,'//
     -        'DWD_Volume_6to12_Hard real null,'//
     -        'DWD_Volume_12to20_Hard real null,'//
     -        'DWD_Volume_20to35_Hard real null,'//
     -        'DWD_Volume_35to50_Hard real null,'//
     -        'DWD_Volume_ge_50_Hard real null,'//
     -        'DWD_Volume_Total_Hard real null,'//
     -        'DWD_Volume_0to3_Soft real null,'//
     -        'DWD_Volume_3to6_Soft real null,'//
     -        'DWD_Volume_6to12_Soft real null,'//
     -        'DWD_Volume_12to20_Soft real null,'//
     -        'DWD_Volume_20to35_Soft real null,'//
     -        'DWD_Volume_35to50_Soft real null,'//
     -        'DWD_Volume_ge_50_Soft real null,'//
     -        'DWD_Volume_Total_Soft real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          IDWDVOL = 0
          RETURN
        ENDIF
      ENDIF
      
      VARD = VAR

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Down_Wood_Vol (CaseID,',
     >  'StandID,Year,DWD_Volume_0to3_Hard,DWD_Volume_3to6_Hard,',
     >  'DWD_Volume_6to12_Hard,DWD_Volume_12to20_Hard,',
     >  'DWD_Volume_20to35_Hard,DWD_Volume_35to50_Hard,',
     >  'DWD_Volume_ge_50_Hard,DWD_Volume_Total_Hard,',
     >  'DWD_Volume_0to3_Soft,DWD_Volume_3to6_Soft,',
     >  'DWD_Volume_6to12_Soft,DWD_Volume_12to20_Soft,',
     >  'DWD_Volume_20to35_Soft,DWD_Volume_35to50_Soft,',
     >  'DWD_Volume_ge_50_Soft,DWD_Volume_Total_Soft) VALUES (''',
     >  CASEID,''',''',TRIM(NPLT),
     >  ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

      iRet = fsql3_prepare(IoutDBref, SQLStmtStr)

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      DO I=1,VARDIM
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
      ENDDO
      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IDWDVOL = 0
      ENDIF
      RETURN
      END
