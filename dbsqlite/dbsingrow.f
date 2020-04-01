      SUBROUTINE DBSINGROW(IYEAR,INGROW,CSP,TPAIN)
      IMPLICIT NONE
C----------
C DBSQLITE $Id$
C----------
C
C     PURPOSE: TO POPULATE A DATABASE WITH REGENERATION INGROWTH
C              SPECIES AND TPA INFORMATION.
C     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'DBSCOM.F77'
      INCLUDE 'PLOT.F77'
C
COMMONS
C
      INTEGER ColNumber,I,iret,IYEAR,IYEAR1
      REAL INGROW,TPAIN
      DOUBLE PRECISION INGROW1,TPAIN1
      CHARACTER*2000 SQLStmtStr
      CHARACTER*2 CSP
      CHARACTER*8 CSP1,CSP2,CSP3
C
COMMONS END

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_bind_text

      IF(IREG5.NE.1) RETURN
C
      CALL DBSCASE(1)
      
C     DEFINE TAABLENAME

      iRet=fsql3_tableexists(IoutDBref,'FVS_Regen_Ingrow'//CHAR(0))
C
C     SITE PREP SUMMARY
C                  
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_Regen_Ingrow('//
     -              'CaseID           text not null,'//
     -              'StandID          text not null,'//
     -              'Year             int  null,'//
     -              'IngrowthTotalTpa real null,'//
     -              'SpeciesFVS       text null,'//
     -              'SpeciesPLANTS    text null,'//
     -              'SpeciesFIA       text null,'//
     -              'IngrowthTpa      int  null);'//CHAR(0)
     
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          IREG5 = 0
          RETURN
        ENDIF
      ENDIF                
          
      WRITE(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow',
     -   ' (CaseID,StandID,Year,IngrowthTotalTpa,',
     -   'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,IngrowthTpa)',
     -   ' VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'

      iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IREG5 = 0                                    
         RETURN
      ENDIF

C     ASSIGN FVS, PLANTS AND FIA SPECIES CODE
C
      DO I = 1,MAXSP
        IF (CSP .EQ. JSP(I)) THEN
          CSP1 = JSP(I)
          CSP2 = PLNJSP(I)
          CSP3 = FIAJSP(2)
        ENDIF
      ENDDO

C
C     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS
C        

      INGROW1=INGROW
      TPAIN1=TPAIN
      IYEAR1=IYEAR

      ColNumber=1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,INGROW1) 
      ColNumber=ColNumber+1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,
     >                                  LEN_TRIM(CSP1))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,
     >                                  LEN_TRIM(CSP2))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,
     >                                  LEN_TRIM(CSP3))
      ColNumber=ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,TPAIN1)
      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      IF (iRet.ne.0) then
        IREG5 = 0 
      ENDIF
      RETURN
      END



