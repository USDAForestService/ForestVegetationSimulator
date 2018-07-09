      SUBROUTINE DBSFMBURN(IYEAR,NPLT,ONEHR,TENHR,HUNDHR,THOUSHR,DUFF,
     -  LIVEW,LIVEH,MFWIND,SLOPE,FLAME,SCORCH,FTYPE,FM,WT,KODE)

      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE BURN CONDITIONS REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE BURN CONDITIONS OUTPUT FROM THE FIRE MODEL.
C              1: ONE HOUR FUEL MOISTURE
C              2: TEN HOUR FUEL MOISTURE
C              3: HUNDRED HOUR FUEL MOISTURE
C              4: THOUSAND HOUR FUEL MOISTURE
C              5: DUFF MOISTURE
C              6: LIVE WOODY MOISTURE
C              7: LIVE HERB MOISTURE
C              8: MID FLAME WIND SPEED
C              9: SLOPE
C             10: FLAME LENGTH
C             11: SCORCH HEIGHT
C             12: FIRE TYPE
C             13: FUEL MODEL
C             14: WEIGHT GIVEN EACH FUEL MODEL
C             15: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,iRet,KODE,FM,SLOPE
      INTEGER ColNumber
      REAL ONEHR, TENHR, HUNDHR, THOUSHR, DUFF, LIVEW, LIVEH, MFWIND,
     -     FLAME, SCORCH, WT
      DOUBLE PRECISION ONEHRB, TENHRB, HUNDHRB, THOUSHRB, DUFFB, LIVEWB,
     -     LIVEHB, MFWINDB, FLAMEB, SCORCHB
      DIMENSION FM(4), WT(4)
      DOUBLE PRECISION,DIMENSION(4)::WTB
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=26) NPLT
      CHARACTER(len=8) FTYPE
      
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

C     Initialize variables

      IF(IBURN.EQ.0) RETURN

      IF(IBURN.EQ.2) KODE = 0
      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,"FVS_BurnReport"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_BurnReport ('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year int null,'//
     -              'One_Hr_Moisture real null,'//
     -              'Ten_Hr_Moisture real null,'//
     -              'Hundred_Hr_Moisture real null,'//
     -              'Thousand_Hr_Moisture real null,'//
     -              'Duff_Moisture real null,'//
     -              'Live_Woody_Moisture real null,'//
     -              'Live_Herb_Moisture real null,'//
     -              'Midflame_Wind real null,'//
     -              'Slope int null,'//
     -              'Flame_length real null,'//
     -              'Scorch_height real null,'//
     -              'Fire_Type char(8) null,'//
     -              'FuelModl1 int null,'//
     -              'Weight1 real null,'//
     -              'FuelModl2 int null,'//
     -              'Weight2 real null,'//
     -              'FuelModl3 int null,'//
     -              'Weight3 real null,'//
     -              'FuelModl4 int null,'//
     -              'Weight4 real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,trim(SQLStmtStr))
        IF (iRet .NE. 0) THEN
          IBURN = 0
          RETURN
        ENDIF
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C

      ONEHRB = ONEHR
      TENHRB = TENHR
      HUNDHRB = HUNDHR
      THOUSHRB = THOUSHR
      DUFFB = DUFF
      LIVEWB = LIVEW
      LIVEHB = LIVEH
      MFWINDB = MFWIND
      FLAMEB = FLAME
      SCORCHB = SCORCH
      WTB(1) = INT((WT(1)*100.)+0.5)
      WTB(2) = INT((WT(2)*100.)+0.5)
      WTB(3) = INT((WT(3)*100.)+0.5)
      WTB(4) = INT((WT(4)*100.)+0.5)

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_BurnReport (CaseID,',
     -  'StandID,Year,One_Hr_Moisture,Ten_Hr_Moisture,',
     -  'Hundred_Hr_Moisture,Thousand_Hr_Moisture,Duff_Moisture,',
     -  'Live_Woody_Moisture,Live_Herb_Moisture,Midflame_Wind,Slope,',
     -  'Flame_length,Scorch_height,Fire_Type,FuelModl1,',
     -  'Weight1,FuelModl2,Weight2,FuelModl3,Weight3,',
     -  'FuelModl4,Weight4) VALUES(''',CASEID,''',''',
     -  TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,''',
     -  TRIM(FTYPE),''',?,?,?,?,?,?,?,?);'//CHAR(0)

      iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,ONEHRB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,TENHRB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HUNDHRB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,THOUSHRB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DUFFB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,LIVEWB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,LIVEHB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,MFWINDB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,SLOPE)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,FLAMEB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SCORCHB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FM(1))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,WTB(1))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FM(2))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,WTB(2))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FM(3))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,WTB(3))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FM(4))

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,WTB(4))
      
      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)

      RETURN
      END


