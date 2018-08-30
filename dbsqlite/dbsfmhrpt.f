      SUBROUTINE DBSFMHRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE CARBON HARVEST
C              PRODUCTS REPORT INFORMATION
C     AUTH: D. ROBINSON - ESSA
C     INPUT:
C       IYEAR  - CALENDAR YEAR
C       NPLT   - CASE NUMBER
C       VAR    - ARRAY WITH VARIABLES TO REPORT
C       VARDIM - LENGTH OF VAR ARRAY
C         1 = PRODUCTS
C         2 = LANDFILL
C         3 = ENERGY
C         4 = EMISSIONS
C         5 = MERCH CARBON STORED
C         6 = MERCH CARBON REMOVED
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,KODE,VARDIM,iRet
      CHARACTER(len=26) NPLT
      REAL      VAR
      DIMENSION VAR(VARDIM)

      DOUBLE PRECISION  VARD(VARDIM)
      INTEGER           I,ColNumber
      CHARACTER*2000    SQLStmtStr

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      IF(ICHRPT.EQ.0) RETURN
      IF(ICHRPT.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)
      iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Hrv_Carbon"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Hrv_Carbon('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year int null,' //
     -      'Products real null,' //
     -      'Landfill real null,' //
     -      'Energy real null,' //
     -      'Emissions real null,' //
     -      'Merch_Carbon_Stored real null,' //
     -      'Merch_Carbon_Removed real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ICHRPT = 0
          RETURN
        ENDIF
      ENDIF

      VARD = VAR

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Hrv_Carbon (CaseID,',
     >  'StandID,Year,Products,Landfill,Energy,Emissions,',
     >  'Merch_Carbon_Stored,Merch_Carbon_Removed) VALUES(''',
     >  CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?)'
      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
        ICHRPT = 0
        RETURN
      ENDIF

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      DO I=1,VARDIM
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
      ENDDO
      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         ICHRPT = 0
      ENDIF
      iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
      RETURN
      END

