      SUBROUTINE DBSFMDSNAG(IYEAR,SDBH,SHTH,SHTS,SVLH,SVLS,
     -  SDH,SDS,YRLAST,KODE)
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE DETAILED SNAG REPORT
C              INFORMATION
C     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
C     INPUT:
C              THE DETAILED SNAG OUTPUT FROM THE FIRE MODEL.
C              1: AVERAGE SNAG DIAMETER FOR THE RECORD
C              2: CURRENT HEIGHT OF HARD SNAGS
C              3: CURRENT HEIGHT OF SOFT SNAGS
C              4: CURRENT VOLUME OF HARD SNAGS
C              5: CURRENT VOLUME OF SOFT SNAGS
C              6: DENSITY OF HARD SNAGS
C              7: DENSITY OF SOFT SNAGS
C              8: YRLAST
C              9: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS

      INTEGER IYEAR,iRet,KODE,YRDEAD,SVLH,SVLS,YRLAST,JYR,IDC,JCL
      INTEGER ColNumber
      REAL SDBH, SHTH, SHTS, SDH, SDS 
      REAL*8 SDBHB, SHTHB, SHTSB, SDHB, SDSB, SDTB, SVLHB, SVLSB, SVLTB
      DIMENSION SVLH(MAXSP,100,6), SVLS(MAXSP,100,6),
     -  SDBH(MAXSP,100,6), SHTH(MAXSP,100,6),SHTS(MAXSP,100,6),
     -  SDH(MAXSP,100,6), SDS(MAXSP,100,6)
      CHARACTER*2000 SQLStmtStr
      CHARACTER(len=4) CSP
      CHARACTER(LEN=8) CSPECIES

      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_step,fsql3_reset,fsql3_bind_text

      IF(ISDET.EQ.0) RETURN
      IF(ISDET.EQ.2) KODE = 0

      CALL DBSCASE(1)

      iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_SnagDet"//CHAR(0))
      IF(iRet.EQ.0) THEN 
          SQLStmtStr='CREATE TABLE FVS_SnagDet ('//
     -          'CaseID text not null,'//
     -          'StandID text not null,'//
     -          'Year Int null,'//
     -          'Species text null,'//
     -          'DBH_Class int null,'//
     -          'Death_DBH real null,'//
     -          'Current_Ht_Hard real null,'//
     -          'Current_Ht_Soft real null,'//
     -          'Current_Vol_Hard real null,'//
     -          'Current_Vol_Soft real null,'//
     -          'Total_Volume real null,'//
     -          'Year_Died int null,'//
     -          'Density_Hard real null,'//
     -          'Density_Soft real null,'//
     -          'Density_Total real null);'//CHAR(0)
 
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        IF (iRet .NE. 0) THEN
          ISDET = 0
          RETURN
        ENDIF
      ENDIF

      WRITE(SQLStmtStr,*)'INSERT INTO FVS_SnagDet ',
     -          '(CaseID,StandID,Year,Species,DBH_Class,Death_DBH,',
     -          'Current_Ht_Hard,Current_Ht_Soft,Current_Vol_Hard,',
     -          'Current_Vol_Soft,Total_Volume,Year_Died,Density_Hard,',
     -          'Density_Soft,Density_Total) VALUES (''',
     -          CASEID,''',''',TRIM(NPLT),
     -          ''',?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
      iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
      iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
      IF (iRet .NE. 0) THEN
        ISDET = 0
        RETURN
      ENDIF

      DO JYR= 1,YRLAST
         DO IDC= 1,MAXSP
            DO JCL= 1,6

              SDTB   = SDH(IDC,JYR,JCL) + SDS(IDC,JYR,JCL)
              IF (SDTB .LE. 0) CYCLE
              CSP = JSP(IDC)
              YRDEAD = IYEAR - JYR + 1
              SDBHB = SDBH(IDC,JYR,JCL)
              SHTHB = SHTH(IDC,JYR,JCL)
              SHTSB = SHTS(IDC,JYR,JCL)
              SDHB  = SDH (IDC,JYR,JCL)
              SDSB  = SDS (IDC,JYR,JCL)
              SVLHB = FLOAT(SVLH(IDC,JYR,JCL))
              SVLSB = FLOAT(SVLS(IDC,JYR,JCL))
              SVLTB = SVLHB + SVLSB

C             DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
C             KEYWORD OVER RIDES
C
              IF(JSPIN(IDC).EQ.1)THEN
                CSPECIES=ADJUSTL(JSP(IDC))
              ELSEIF(JSPIN(IDC).EQ.2)THEN
                CSPECIES=ADJUSTL(FIAJSP(IDC))
              ELSEIF(JSPIN(IDC).EQ.3)THEN
                CSPECIES=ADJUSTL(PLNJSP(IDC))
              ELSE
                CSPECIES=ADJUSTL(PLNJSP(IDC))
              ENDIF
C
              IF(ISPOUT23.EQ.1)CSPECIES=ADJUSTL(JSP(IDC))
              IF(ISPOUT23.EQ.2)CSPECIES=ADJUSTL(FIAJSP(IDC))
              IF(ISPOUT23.EQ.3)CSPECIES=ADJUSTL(PLNJSP(IDC))

              ColNumber=1
              iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)             

              ColNumber=ColNumber+1
              iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIES,
     >               len_trim(CSPECIES))              

              ColNumber=ColNumber+1
              iRet = fsql3_bind_int(IoutDBref,ColNumber,JCL)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SDBHB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SHTHB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SHTSB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SVLHB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SVLSB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SVLTB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_int(IoutDBref,ColNumber,YRDEAD)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SDHB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SDSB)

              ColNumber=ColNumber+1
              iRet = fsql3_bind_double(IoutDBref,ColNumber,SDTB)

              iRet = fsql3_step(IoutDBref)
              iRet = fsql3_reset(IoutDBref)

            ENDDO
         ENDDO
      ENDDO
      iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         ISDET = 0
      ENDIF

      RETURN

      END


