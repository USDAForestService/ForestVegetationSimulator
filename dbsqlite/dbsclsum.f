      SUBROUTINE DBSCLSUM(NPLT,IYR,SP,SPVIAB,
     >          SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >          SPSITGM,MXDENMLT,POTESTAB)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

      INCLUDE 'DBSCOM.F77'
C
      INTEGER IYR,IRCODE
      CHARACTER(LEN=*) NPLT,SP
      REAL SPVIAB,SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >     SPSITGM,MXDENMLT,POTESTAB

      CHARACTER*2000 SQLStmtStr
      
      INTEGER fsql3_tableexists,fsql3_exec
      IF (ICLIM.EQ.0) RETURN

C     MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

      IRCODE = fsql3_tableexists(IoutDBref,"FVS_Climate"//CHAR(0))
      IF(IRCODE.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Climate('//
     -              'CaseID char(36) not null,'//
     -              'StandID char(26) not null,'//
     -              'Year Int null,'//
     -              'Species char(3) null,'//
     -              'Viability real null,'//
     -              'BA real null,'//
     -              'TPA real null,'//
     -              'ViabMort real null,'//
     -              'dClimMort real null,'//
     -              'GrowthMult real null,'//
     -              'SiteMult real null,'//
     -              'MxDenMult real null,'//
     -              'AutoEstbTPA real null)'//CHAR(0)
        IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (IRCODE .NE. 0) THEN
          ICLIM = 0
          RETURN
        ENDIF
      ENDIF

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Climate (CaseID,'//
     -   'StandID,Year,Species,Viability,BA,TPA,ViabMort,'//
     -   'dClimMort,GrowthMult,SiteMult,MxDenMult,AutoEstbTPA) '//
     -   'VALUES(''',CASEID,''',''',trim(NPLT),''',',IYR,',''',
     -   trim(SP),''',',SPVIAB,',',SPBA,',',SPTPA,',',
     -   SPMORT1,',',SPMORT2,',',SPGMULT,',',
     -   SPSITGM,',',MXDENMLT,',',POTESTAB,')'

      IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (IRCODE .NE. 0) ICLIM = 0
      RETURN
      END


