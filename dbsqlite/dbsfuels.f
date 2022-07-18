      SUBROUTINE DBSFUELS(IYEAR,NPLT,LITTER,DUFF,SDEADLT3,SDEADGT3,
     -  SDEAD3TO6,SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3,
     -  SNAGSGT3,FOLIAGE,STANDLT3,STANDGT3,STANDTOTAL,BIOMASS,CONSUMED,
     -  REMOVED,KODE)

      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS REPORT INFORMATION
C     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
C     INPUT:
C              THE ALL FUELS OUTPUT FROM THE FIRE MODEL.
C              1: SURFACE LITTER
C              2: SURFACE DUFF
C              3: SURFACE DEAD FUEL LESS THAN 3 INCHES
C              4: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 3 INCHES
C              5: SURFACE DEAD FUEL BETWEEN 3 AND 6
C              6: SURFACE DEAD FUEL BETWEEN 6 AND 12
C              7: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 12
C              8: SURFACE HERB FUEL
C              9: SURFACE SHRUB FUEL
C             10: SURFACE TOTAL FUEL
C             11: STANDING SNAGS LESS THAN 3
C             12: STANDING SNAGS GREATER THAN OR EQUAL TO 3
C             13: STANDING FOLIAGE
C             14: STANDING LIVE LESS THAN 3
C             15: STANDING LIVE GREATER THAN OR EQUAL TO 3
C             16: STANDING TOTAL
C             17: TOTAL BIOMASS
C             18: TOTAL CONSUMED
C             19: BIOMASS REMOVED
C             20: KODE FOR WHETHER REPORT ALSO DUMPS TO FILE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS
C

      INTEGER IYEAR,BIOMASS,CONSUMED,REMOVED,iRet,KODE,STANDGT3,
     >        STANDTOTAL,ColNumber
      REAL LITTER,DUFF,SDEADLT3,SDEADGT3,SDEAD3TO6,
     -  SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3,SNAGSGT3,
     -  FOLIAGE,STANDLT3
      DOUBLE PRECISION LITTERB,DUFFB,SDEADLT3B,SDEADGT3B,SDEAD3TO6B,
     -  SDEAD6TO12B,SDEADGT12B,HERBB,SHRUBB,SURFTOTALB,SNAGSLT3B,
     -  SNAGSGT3B,FOLIAGEB,STANDLT3B
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=26) NPLT

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

      IF(IFUELS.EQ.0) RETURN
      IF(IFUELS.EQ.2) KODE = 0
 
      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Fuels"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Fuels('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Year Int null,'//
     -              'Surface_Litter real null,'//
     -              'Surface_Duff real null,'//
     -              'Surface_lt3 real null,'//
     -              'Surface_ge3 real null,'//
     -              'Surface_3to6 real null,'//
     -              'Surface_6to12 real null,'//
     -              'Surface_ge12 real null,'//
     -              'Surface_Herb real null,'//
     -              'Surface_Shrub real null,'//
     -              'Surface_Total real null,'//
     -              'Standing_Snag_lt3 real null,'//
     -              'Standing_Snag_ge3 real null,'//
     -              'Standing_Foliage real null,'//
     -              'Standing_Live_lt3 real null,'//
     -              'Standing_Live_ge3 real null,'//
     -              'Standing_Total real null,'//
     -              'Total_Biomass Int null,'//
     -              'Total_Consumed Int null,'//
     -              'Biomass_Removed Int null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           IFUELS = 0
           RETURN
         ENDIF
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      LITTERB=LITTER
      DUFFB=DUFF
      SDEADLT3B=SDEADLT3
      SDEADGT3B=SDEADGT3
      SDEAD3TO6B=SDEAD3TO6
      SDEAD6TO12B=SDEAD6TO12
      SDEADGT12B=SDEADGT12
      HERBB=HERB
      SHRUBB=SHRUB
      SURFTOTALB=SURFTOTAL
      SNAGSLT3B=SNAGSLT3
      SNAGSGT3B=SNAGSGT3
      FOLIAGEB=FOLIAGE
      STANDLT3B=STANDLT3

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Fuels (CaseID,',
     -  'StandID,Year,Surface_Litter,Surface_Duff,Surface_lt3,',
     -  'Surface_ge3,Surface_3to6,Surface_6to12,Surface_ge12,',
     -  'Surface_Herb,Surface_Shrub,Surface_Total,Standing_Snag_lt3,',
     -  'Standing_Snag_ge3,Standing_Foliage,Standing_Live_lt3,',
     -  'Standing_Live_ge3,Standing_Total,Total_Biomass,',
     -  'Total_Consumed,Biomass_Removed) VALUES(''',CASEID,''',''',
     -  TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
      iRet = fsql3_prepare(IoutDBref, SQLStmtStr)
      IF (iRet .NE. 0) THEN
         IFUELS = 0
         RETURN
      ENDIF

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,LITTERB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DUFFB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEADLT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEADGT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEAD3TO6B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEAD6TO12B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SDEADGT12B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HERBB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SHRUBB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SURFTOTALB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SNAGSLT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SNAGSGT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,FOLIAGEB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,STANDLT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,STANDGT3)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,STANDTOTAL)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,BIOMASS)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,CONSUMED)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,REMOVED)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IFUELS = 0
      ENDIF
      RETURN

      END


