      SUBROUTINE DBSFMPFC(NPLT,WINDSP,TEMP,MOIST1,MOIST2,MOIST3,MOIST4,
     &            MOIST5,MOIST6,MOIST7,KODE)    
      IMPLICIT NONE
C----------
C VDBSQLITE $Id$
C----------
C     PURPOSE: TO POPULATE A DATABASE WITH THE FIRE MODELS POTENTIAL FIRE
C              CONDITIONS OUTPUT.
C     AUTH: M.SHETTLES -- FMSC -- NOVEMBER 2020
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
C
COMMONS

      INTEGER, PARAMETER :: MxMsg = 500
      CHARACTER(LEN=MxMsg) Msg
      INTEGER TEMP,KODE,ColNumber,iRet
      REAL WINDSP,MOIST1,MOIST2,MOIST3,MOIST4,MOIST5,MOIST6,MOIST7
      DOUBLE PRECISION DWINDSP,DMOIST1,DMOIST2,DMOIST3,DMOIST4,DMOIST5
      DOUBLE PRECISION DMOIST6,DMOIST7
      CHARACTER*2000 SQLStmtStr
      CHARACTER*8 FCOND
      CHARACTER(len=26) NPLT

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >    fsql3_prepare,fsql3_bind_double,fsql3_finalize,fsql3_errmsg

C     Initialize variables

      IF(IPOTFIREC.EQ.0) RETURN

C---------
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C---------
      CALL DBSCASE(1)
      
        IF (KODE.EQ.1)THEN
          FCOND="Severe"
        ELSE
          FCOND="Moderate"
        ENDIF 

        iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_PotFire_Cond"//CHAR(0))
        IF(iRet.EQ.0) THEN   
           SQLStmtStr='CREATE TABLE FVS_PotFire_Cond ('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Fire_Condition text null,'//
     -              'Wind_Speed real null,'//
     -              'Temperature int null,'//
     -              'One_Hr_Moisture real null,'//
     -              'Ten_Hr_Moisture real null,'//
     -              'Hundred_Hr_Moisture real null,'//
     -              'Thousand_Hr_Moisture real null,'//
     -              'Duff_Moisture real null,'//
     -              'Live_Woody_Moisture real null,'//
     -              'Live_Herb_Moisture real null);'//CHAR(0)
           iRet = fsql3_exec(IoutDBref,SQLStmtStr)
           IF (iRet.NE.0) THEN
             iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
             print *,"FVS_PotFire_Cond exec direct error:",Msg(:iRet)
             IPOTFIREC = 0
             RETURN
           ENDIF
         ENDIF
               
        DWINDSP=WINDSP
        DMOIST1=MOIST1
        DMOIST2=MOIST2
        DMOIST3=MOIST3
        DMOIST4=MOIST4
        DMOIST5=MOIST5
        DMOIST6=MOIST6
        DMOIST7=MOIST7

        WRITE(SQLStmtStr,*)'INSERT INTO FVS_PotFire_Cond (CaseID,',
     -     'StandID,Fire_Condition,Wind_Speed,Temperature,',
     -     'One_Hr_Moisture,Ten_Hr_Moisture,',
     -     'Hundred_Hr_Moisture,Thousand_Hr_Moisture,Duff_Moisture,',
     -     'Live_Woody_Moisture,Live_Herb_Moisture) VALUES (''',
     -      CASEID,''',''',TRIM(NPLT),''',''',TRIM(FCOND),
     -      ''',?,?,?,?,?,?,?,?,?);'


      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))

      IF (iRet.NE.0) THEN
         iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
         print *,"FVS_PotFire_Cond prepare error:",Msg(:iRet)
         IPOTFIREC = 0
         RETURN
      ENDIF 

C      ColNumber=1
C      iRet = fsql3_bind_double(IoutDBref,ColNumber,FCOND)
      
      ColNumber=1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DWINDSP)
      
      ColNumber=ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,TEMP)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST1)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST2)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref, ColNumber,DMOIST3)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST4)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST5)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST6)

      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST7)

      iRet = fsql3_step(IoutDBref)
      if (iRet.ne.0) then
         iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
         print *,"FVS_PotFire_Cond step error:",Msg(:iRet)
         IPOTFIREC = 0
      ENDIF  
      WRITE(JOSTND,*)'FCOND= ',FCOND 
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
         print *,"FVS_PotFire_Cond finalize error:",Msg(:iRet)
         IPOTFIREC = 0
      ENDIF
      RETURN

      END


