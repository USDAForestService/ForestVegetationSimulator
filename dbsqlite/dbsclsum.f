      SUBROUTINE DBSCLSUM(CNPLT,IYR,SP,SPVIAB,
     >          SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >          SPSITGM,MXDENMLT,POTESTAB)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'DBSCOM.F77'
C
      INTEGER I,IYR,IRCODE
      CHARACTER(LEN=*) CNPLT,SP
      REAL SPVIAB,SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,
     >     SPSITGM,MXDENMLT,POTESTAB

      CHARACTER*8    CSP1,CSP2,CSP3
      CHARACTER*2000 SQLStmtStr

      INTEGER fsql3_tableexists,fsql3_exec
      IF (ICLIM.EQ.0) RETURN

C     MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

      IRCODE = fsql3_tableexists(IoutDBref,"FVS_Climate"//CHAR(0))
      IF(IRCODE.EQ.0) THEN
        SQLStmtStr='CREATE TABLE FVS_Climate('//
     -              'CaseID text not null,'//
     -              'StandID text not null,'//
     -              'Year Int null,'//
     -              'SpeciesFVS    text null,'//
     -              'SpeciesPLANTS text null,'//
     -              'SpeciesFIA    text null,'//
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

C     ASSIGN FVS, PLANTS AND FIA SPECIES CODES
C
      DO I = 1,MAXSP
        IF (SP .EQ. JSP(I)) THEN
          CSP1 = JSP(I)
          CSP2 = PLNJSP(I)
          CSP3 = FIAJSP(I)
        ENDIF
      ENDDO

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Climate (CaseID,'//
     -   'StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,'//
     -   'Viability,BA,TPA,ViabMort,'//
     -   'dClimMort,GrowthMult,SiteMult,MxDenMult,AutoEstbTPA) '//
     -   'VALUES(''',CASEID,''',''',TRIM(CNPLT),''',',IYR,',',
     -   '''',TRIM(CSP1),''',',
     -   '''',TRIM(CSP2),''',',
     -   '''',TRIM(CSP3),''',',
     -   SPVIAB,',',SPBA,',',SPTPA,',',
     -   SPMORT1,',',SPMORT2,',',SPGMULT,',',
     -   SPSITGM,',',MXDENMLT,',',POTESTAB,')'

      IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (IRCODE .NE. 0) ICLIM = 0
      RETURN
      END

