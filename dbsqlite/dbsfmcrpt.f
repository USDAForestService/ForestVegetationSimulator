      SUBROUTINE DBSFMCRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE MAIN CARBON REPORT
C              INFORMATION
C     AUTH: D. ROBINSON - ESSA
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C       UNITS ARE TONS/ACRE IF USER HAS REQUESTED IMPERIAL UNITS; TONNES/HA IF METRIC
C         1 = ABOVEGROUND TOTAL LIVE
C         2 = ABOVEGOURND MERCH LIVE
C         3 = BELOWGROUND LIVE
C         4 = BELOWGROUND DEAD (WHICH DECAYS)
C         5 = STANDING DEAD
C         6 = FOREST DOWN DEAD WOOD
C         7 = FOREST FLOOR (SOILS?)
C         8 = FOREST SHRUB+HERB
C         9 = TOTAL STAND CARBON
C        10 = TOTAL CARBON REMOVED THIS REPORTING PERIOD
C        11 = CARBON RELEASED THRU FIRE
C       KODE   - RETURN CODE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR, KODE,VARDIM
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           iRet,I,ColNumber
      CHARACTER*2000    SQLStmtStr

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

C     Initialize variables

      IF(ICMRPT.EQ.0) RETURN
      IF(ICMRPT.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

C     INSURE MAIN CARBON TABLE EXISTS IN DATBASE

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Carbon"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Carbon('//
     -         'CaseID char(36) not null,'//
     -         'StandID char(26) not null,'//
     -         'Year Int null,'//
     -         'Aboveground_Total_Live real null,'//
     -         'Aboveground_Merch_Live real null,'//
     -         'Belowground_Live real null,'//
     -         'Belowground_Dead real null,'//
     -         'Standing_Dead real null,'//
     -         'Forest_Down_Dead_Wood real null,'//
     -         'Forest_Floor real null,'//
     -         'Forest_Shrub_Herb real null,'//
     -         'Total_Stand_Carbon real null,'//
     -         'Total_Removed_Carbon real null,'//
     -         'Carbon_Released_From_Fire real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ICMRPT = 0
          RETURN
        ENDIF
      ENDIF

C     COPY INPUT VECTOR TO DOUBLE-PRECISION

      VARD = VAR

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Carbon (CaseID,',
     >  'StandID,Year,Aboveground_Total_Live,Aboveground_Merch_Live,',
     >  'Belowground_Live,Belowground_Dead,Standing_Dead,',
     >  'Forest_Down_Dead_Wood,Forest_Floor,Forest_Shrub_Herb,',
     >  'Total_Stand_Carbon,Total_Removed_Carbon,',
     >  'Carbon_Released_From_Fire) VALUES (''',
     >  CASEID,''',''',TRIM(NPLT),
     >  ''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

C     PREPARE THE SQL QUERY

      iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
      if (iRet.ne.0) then
         ICMRPT = 0
         RETURN
      ENDIF

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
         ICMRPT = 0
      ENDIF

      END
