      SUBROUTINE DBSFMCANPR(IYEAR,CRFILL,NPLT)
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH CANOPY PROFILE
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- JUNE 2006
C     INPUT:
C              THE CANOPY PROFILE INFO FROM THE FIRE MODEL.
C              1: BIOMASS OF CANOPY FUELS AT VARIOUS HEIGHTS
C                  ABOVE THE GROUND IN LBS/ACRE/FOOT.
C
C     THIS TABLE IS UNIQUE IN THAT THERE IS NOT A CORRESPONDING
C     TEXT FILE TABLE.
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C---
      INTEGER IYEAR,iRet,I,ColNumber
      REAL CRFILL
      DOUBLE PRECISION CRFILLB, CRFILLKG, HTFT, HTM
      DIMENSION CRFILL(400)
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=26) NPLT
      
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_step,fsql3_reset

      IF(ICANPR.EQ.0) RETURN
      
      iRet = 0
      DO I = 1,400
        IF (CRFILL(I).GE.0) THEN
          iRet=1
          EXIT
        ENDIF
      ENDDO
      if (iRet.eq.0) RETURN
     
      CALL DBSCASE(1)
      iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))      
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_CanProfile"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_CanProfile ('//
     -         'CaseID char(36) not null,'//
     -         'StandID char(26) not null,'//
     -         'Year Int null,'//
     -         'Height_m real null,'//
     -         'Canopy_Fuel_kg_m3 real null,'//
     -         'Height_ft real null,'//
     -         'Canopy_Fuel_lbs_acre_ft real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ICANPR = 0
          RETURN
        ENDIF
      ENDIF

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_CanProfile (CaseID,',
     -    'StandID,Year,Height_m,Canopy_Fuel_kg_m3,Height_ft,',
     -    'Canopy_Fuel_lbs_acre_ft) VALUES (''',CASEID,
     -    ''',''',TRIM(NPLT),''',?,?,?,?,?);'
      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
        ICANPR = 0
        RETURN
      ENDIF

      DO I = 1,400
        IF (CRFILL(I).LE.0) CYCLE
C
C       ASSIGN VALUES TO DOUBLE PRECISION VARS
C
        CRFILLB = CRFILL(I)
        CRFILLKG = CRFILL(I)*0.45359237 / (4046.856422 * 0.3048)
        HTFT = I
        HTM = I*0.3048

        ColNumber=1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,HTM)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,CRFILLKG)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,HTFT)
        ColNumber=ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,CRFILLB)
       
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_reset(IoutDBref)
      enddo
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         ICANPR = 0
      ENDIF
      iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
      RETURN
      END


