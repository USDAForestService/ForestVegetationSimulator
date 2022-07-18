      SUBROUTINE DBSFMFUEL(IYEAR,NPLT,MSE,LITTER,DUFF,CLT3,CGT3,
     -  C3TO6,C6TO12,CGT12,HERB,CROWN,CTOTAL,PERCDUFF,PERCGT3,
     -  PERTRCR,SM25,SM10,KODE)
      IMPLICIT NONE
C----------
C DBSQLITE $Id$
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS CONSUMPTION REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE FUEL CONSUMPTION OUTPUT FROM THE FIRE MODEL.
C              1: MINERAL SOIL EXPOSURE
C              2: LITTER CONSUMPTION
C              3: DUFF CONSUMPTION
C              4: CONSUMPTION 0 - 3
C              5: CONSUMPTION >= 3
C              6: CONSUMPTION 3 -  6
C              7: CONSUMPTION 6 - 12
C              8: CONSUMPTION >= 12
C              9: HERB / SHRUB CONSUMPTION
C             10: CROWN CONSUMPTION
C             11: TOTAL CONSUMPTION
C             12: % CONSUMPTION DUFF
C             13: % CONSUMPTION >= 3
C             14: % TREES WITH CROWNING
C             15: SMOKE PRODUCTION < 2.5
C             16: SMOKE PRODUCTION < 10
C             17: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
COMMONS
C
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER IYEAR,iRet,KODE,PERTRCR,ColNumber
      REAL MSE,LITTER,DUFF,CLT3,CGT3,C3TO6,C6TO12,CGT12,HERB,CROWN,
     -     CTOTAL,PERCDUFF,PERCGT3,SM25,SM10
      DOUBLE PRECISION MSEB,LITTERB,DUFFB,CLT3B,CGT3B,C3TO6B,C6TO12B,
     -     CGT12B,HERBB,CROWNB,CTOTALB,PERCDUFFB,PERCGT3B,
     -     SM25B,SM10B
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=26) NPLT

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize
   
      IF(IFUELC.EQ.0) RETURN
      IF(IFUELC.EQ.2) KODE = 0

      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_Consumption"//CHAR(0))
      IF(iRet.EQ.0) THEN
         SQLStmtStr='CREATE TABLE FVS_Consumption('//
     -       'CaseID text not null,'//
     -       'StandID text not null,'//
     -       'Year Int null,'//
     -       'Min_Soil_Exp real null,'//
     -       'Litter_Consumption real null,'//
     -       'Duff_Consumption real null,'//
     -       'Consumption_lt3 real null,'//
     -       'Consumption_ge3 real null,'//
     -       'Consumption_3to6 real null,'//
     -       'Consumption_6to12 real null,'//
     -       'Consumption_ge12 real null,'//
     -       'Consumption_Herb_Shrub real null,'//
     -       'Consumption_Crowns real null,'//
     -       'Total_Consumption real null,'//
     -       'Percent_Consumption_Duff real null,'//
     -       'Percent_Consumption_ge3 real null,'//
     -       'Percent_Trees_Crowning int null,'//
     -       'Smoke_Production_25 real null,'//
     -       'Smoke_Production_10 real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          IFUELC = 0
          RETURN
        ENDIF
      ENDIF
C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C
      MSEB = MSE
      LITTERB=LITTER
      DUFFB=DUFF
      CLT3B=CLT3
      CGT3B=CGT3
      C3TO6B=C3TO6
      C6TO12B=C6TO12
      CGT12B=CGT12
      HERBB=HERB
      CROWNB=CROWN
      CTOTALB=CTOTAL
      PERCDUFFB=PERCDUFF
      PERCGT3B=PERCGT3
      SM25B=SM25
      SM10B = SM10

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Consumption (CaseID,',
     -  'StandID,Year,Min_Soil_Exp,Litter_Consumption,',
     -  'Duff_Consumption,',
     -  'Consumption_lt3,Consumption_ge3,Consumption_3to6,',
     -  'Consumption_6to12,Consumption_ge12,Consumption_Herb_Shrub,',
     -  'Consumption_Crowns,Total_Consumption,',
     -  'Percent_Consumption_Duff,',
     -  'Percent_Consumption_ge3,Percent_Trees_Crowning,',
     -  'Smoke_Production_25,Smoke_Production_10) VALUES (''',CASEID,
     -  ''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'

      iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
C
C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
C
      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,MSEB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,LITTERB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DUFFB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,CLT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,CGT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,C3TO6B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,C6TO12B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,CGT12B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,HERBB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,CROWNB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,CTOTALB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,PERCDUFFB)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,PERCGT3B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,PERTRCR)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SM25B)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,SM10B)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IFUELC = 0
      ENDIF
      RETURN
      END

