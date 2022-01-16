      SUBROUTINE DBSMIS1(IYEAR,NPLT,CSP,
     -  SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4,
     -  KODE)
     
      IMPLICIT NONE
C
C $Id$
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST MISTLETOE REPORT INFORMATION
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
C
C     1:     Year
C     2:     Mean DMR for species - SPDMR4
C     3:     Mean DMI for species - SPDMI4
C     4:     Infected trees/acre for species - SPINF4
C     5:     Mortality trees/acre for species - SPMRT4
C     6:     Infected trees/acre % for species - SPPIN4
C     7:     Mortality trees/acre % for species - SPPMR4
C     8:     Stand trees/acre % for species - SPPOC4


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      REAL    SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
      REAL    SPPIN4(4),SPPMR4(4),SPPOC4(4)
      INTEGER KODE,iRet
      
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_step,fsql3_reset,fsql3_bind_text

      INTEGER ColNumber,I

      DOUBLE PRECISION  SPDMR4B, SPDMI4B
      INTEGER           ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

      CHARACTER*2000    SQLStmtStr
      CHARACTER(LEN=26) NPLT
      CHARACTER(LEN=2)  CSP(4)

C     INITIALIZE VARIABLES

      IF(IDM1.EQ.0) RETURN
      IF(IDM1.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)
      
      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_DM_Spp_Sum"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Spp_Sum ('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year Int null,'//
     -      'Spp text null,'//
     -      'Mean_DMR real null,'//
     -      'Mean_DMI real null,'//
     -      'Inf_TPA int null,'//
     -      'Mort_TPA int  null,'//
     -      'Inf_TPA_Pct int null,'//
     -      'Mort_TPA_Pct int null,'//
     -      'Stnd_TPA_Pct int null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           IDM1 = 0
           RETURN
         ENDIF
      ENDIF

      WRITE(SQLStmtStr,*) 'INSERT INTO FVS_DM_Spp_Sum ',
     -  ' (CaseID,StandID,Year,Spp,Mean_DMR,Mean_DMI,Inf_TPA,',
     -  'Mort_TPA,Inf_TPA_Pct,Mort_TPA_Pct,Stnd_TPA_Pct) ',
     -  ' VALUES (''',CASEID,''',''',TRIM(NPLT),
     -  ''',?,?,?,?,?,?,?,?,?);'
      iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IDM1 = 0
         RETURN
      ENDIF
        
C     LOOP OVER 4 TOP SPECIES

      DO I = 1,4

C       IF THERE ARE LESS THAN 4 SPECIES INFECTED IN THE STAND,
C       DO NOT WRITE THE BLANK RECORDS TO THE DATABASE. 

        IF (CSP(I) .EQ. '**') CYCLE
        
C       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
C       INTEGER COPIES OF REAL VECTOR INPUTS

        SPDMR4B = SPDMR4(I)
        SPDMI4B = SPDMI4(I)

        ISPINF4 = NINT(SPINF4(I))
        ISPMRT4 = NINT(SPMRT4(I))
        ISPPIN4 = NINT(SPPIN4(I))
        ISPPMR4 = NINT(SPPMR4(I))
        ISPPOC4 = NINT(SPPOC4(I))


C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 YEAR
        iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
        
        ColNumber=ColNumber+1       ! Species
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP(I),
     >         len_trim(CSP(I)))              

        ColNumber=ColNumber+1       ! MEAN DMR
        iRet=fsql3_bind_double(IoutDBref,ColNumber,SPDMR4B)

        ColNumber=ColNumber+1       ! MEAN DMI
        iRet=fsql3_bind_double(IoutDBref,ColNumber,SPDMI4B)

        ColNumber=ColNumber+1       ! TPA INFECTED
        iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPINF4)

        ColNumber=ColNumber+1       ! TPA MORTALITY
        iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPMRT4)

        ColNumber=ColNumber+1       ! % TPA INFECTED
        iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPPIN4)

        ColNumber=ColNumber+1       ! % TPA MORTALITY
        iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPPMR4)

        ColNumber=ColNumber+1       ! % STAND TPA
        iRet=fsql3_bind_int(IoutDBref,ColNumber,ISPPOC4) 
        
        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_reset(IoutDBref) 
      ENDDO

      iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IDM1 = 0
         RETURN
      ENDIF

      RETURN
      END

C-------------------------------------------------------------------------------

      SUBROUTINE DBSMIS2(IYEAR,NPLT,NAGE,
     -  ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM,
     -  ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)
     
      IMPLICIT NONE
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND MISTLETOE REPORT INFORMATION
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
C
C     1:     Year - IYEAR
C     2:     Age - NAGE
C     3:     Stand trees/acre - ISTTPAT
C     4:     Stand basal area - IBA
C     5:     Stand volume - ISTVOL
C     6:     Infected trees/acre - ISTTPAI
C     7:     Infected basal area - ISTBAI
C     8:     Infected volume - ISTVOLI
C     9:     Mortality trees/acre - ISTTPAM
C     10:    Mortality basal area - ISTBAM
C     11:    Mortality volume - ISTVOLM
C     12:    Infected trees/acre % - ISTPIT
C     13:    Infected volume % - ISTPIV
C     14:    Mortality trees/acre % - ISTPMT
C     15:    Mortality volume % - ISTPMV
C     16:    Mean DMR - STDMR
C     17:    Mean DMI - STDMI

      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR,NAGE
      CHARACTER(LEN=26) NPLT
      INTEGER ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
      INTEGER ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
      REAL    STDMR,STDMI
      INTEGER KODE,iRet
      
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize

C     LOCAL VARIABLES

      INTEGER ColNumber

      DOUBLE PRECISION  STDMRB, STDMIB

      CHARACTER*1000    SQLStmtStr

C     INITIALIZE VARIABLES

      IF(IDM2.EQ.0) RETURN
      IF(IDM2.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_DM_Stnd_Sum"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Stnd_Sum ('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year int null,'//
     -      'Age int null,'//
     -      'Stnd_TPA int null,'//
     -      'Stnd_BA int null,'//
     -      'Stnd_Vol int null,'//
     -      'Inf_TPA int null,'//
     -      'Inf_BA int null,'//
     -      'Inf_Vol int null,'//
     -      'Mort_TPA int null,'//
     -      'Mort_BA int null,'//
     -      'Mort_Vol int null,'//
     -      'Inf_TPA_Pct int null,'//
     -      'Inf_Vol_Pct int null,'//
     -      'Mort_TPA_Pct int null,'//
     -      'Mort_Vol_Pct int null,'//
     -      'Mean_DMR real null,'//
     -      'Mean_DMI real null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           IDM2 = 0
           RETURN
         ENDIF
      ENDIF

C     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

      STDMRB = STDMR
      STDMIB = STDMI

      WRITE(SQLStmtStr,*) 'INSERT INTO FVS_DM_Stnd_Sum (CaseID,',
     -  'StandID,Year,Age,Stnd_TPA,Stnd_BA,Stnd_Vol,Inf_TPA,Inf_BA,',
     -  'Inf_Vol,Mort_TPA,Mort_BA,Mort_Vol,Inf_TPA_Pct,Inf_Vol_Pct,',
     -  'Mort_TPA_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)',
     -  'VALUES (''',CASEID,''',''',TRIM(NPLT),
     -  ''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'

      iRet=fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IDM2 = 0
         RETURN
      ENDIF

C     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

      ColNumber=1                 ! YEAR
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

      ColNumber=ColNumber+1       ! AGE
      iRet=fsql3_bind_int(IoutDBref,ColNumber,NAGE)

      ColNumber=ColNumber+1       ! STAND TPA
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTTPAT)

      ColNumber=ColNumber+1       ! STAND BA
      iRet=fsql3_bind_int(IoutDBref,ColNumber,IBA)

      ColNumber=ColNumber+1       ! STAND VOL
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTVOL)

      ColNumber=ColNumber+1       ! INF TPA
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTTPAI)

      ColNumber=ColNumber+1       ! INF BA
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTBAI)

      ColNumber=ColNumber+1       ! INF VOL
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTVOLI)

      ColNumber=ColNumber+1       ! MORT TPA
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTTPAM)

      ColNumber=ColNumber+1       ! MORT BA
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTBAM)

      ColNumber=ColNumber+1       ! MORT VOL
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTVOLM)

      ColNumber=ColNumber+1       ! INF TPA %
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPIT)

      ColNumber=ColNumber+1       ! INF VOL %
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPIV)

      ColNumber=ColNumber+1       ! 16 MORT TPA %
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPMT)

      ColNumber=ColNumber+1       ! MORT VOL %
      iRet=fsql3_bind_int(IoutDBref,ColNumber,ISTPMV)

      ColNumber=ColNumber+1       ! MEAN DMR
      iRet=fsql3_bind_double(IoutDBref,ColNumber,STDMRB)

      ColNumber=ColNumber+1       ! MEAN DMI
      iRet=fsql3_bind_double(IoutDBref,ColNumber,STDMIB)

      iRet = fsql3_step(IoutDBref)
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IDM2 = 0
      ENDIF
      RETURN

      RETURN
      END
C-------------------------------------------------------------------------------

      SUBROUTINE DBSMIS3(IYEAR,NPLT,NLABS,
     -  DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)
     
      IMPLICIT NONE

C     PURPOSE: TO POPULATE A DATABASE WITH THE 3RD MISTLETOE REPORT INFORMATION
C     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)
C
C     1:     Year - IYEAR
C     2:     Age - NAGE
C     3:     Stand trees/acre - ISTTPAT
C     4:     Stand basal area - IBA
C     5:     Stand volume - ISTVOL
C     6:     Infected trees/acre - ISTTPAI
C     7:     Infected basal area - ISTBAI
C     8:     Infected volume - ISTVOLI
C     9:     Mortality trees/acre - ISTTPAM
C     10:    Mortality basal area - ISTBAM
C     11:    Mortality volume - ISTVOLM
C     12:    Infected trees/acre % - ISTPIT
C     13:    Infected volume % - ISTPIV
C     14:    Mortality trees/acre % - ISTPMT
C     15:    Mortality volume % - ISTPMV
C     16:    Mean DMR - STDMR
C     17:    Mean DMI - STDMI


      INCLUDE 'DBSCOM.F77'

C     ARGUMENT LIST

      INTEGER IYEAR
      CHARACTER(LEN=26) NPLT
      CHARACTER(LEN=3)  NLABS(5)
      REAL    DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
      INTEGER KODE,iRet

C     LOCAL VARIABLES
      
      integer fsql3_tableexists,fsql3_exec,fsql3_bind_int,
     >        fsql3_prepare,fsql3_bind_double,fsql3_finalize,
     >        fsql3_step,fsql3_reset,fsql3_bind_text

      INTEGER ColNumber,I,J

      DOUBLE PRECISION  X(10)

      CHARACTER*1000    SQLStmtStr

C     INITIALIZE VARIABLES

      IF(IDM3.EQ.0) RETURN
      IF(IDM3.EQ.2) KODE = 0

C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

      CALL DBSCASE(1)

      iRet = fsql3_tableexists(IoutDBref,
     >       "FVS_DM_Sz_Sum"//CHAR(0))
      IF(iRet.EQ.0) THEN
          SQLStmtStr='CREATE TABLE FVS_DM_Sz_Sum ('//
     -      'CaseID text not null,'//
     -      'StandID text not null,'//
     -      'Year int null,'//
     -      'Type text not null,'//
     -      char(39)//'0-3in'   //char(39)//' real null,'//
     -      char(39)//'3-5in'  //char(39)//' real null,'//
     -      char(39)//'5-7in' //char(39)//' real null,'//
     -      char(39)//'7-9in' //char(39)//' real null,'//
     -      char(39)//'9-11in' //char(39)//' real null,'//
     -      char(39)//'11-13in' //char(39)//' real null,'//
     -      char(39)//'13-15in' //char(39)//' real null,'//
     -      char(39)//'15-17in' //char(39)//' real null,'//
     -      char(39)//'17-19in' //char(39)//' real null,'//
     -      char(39)//'gt19in'  //char(39)//' real null);'//CHAR(0)
         iRet = fsql3_exec(IoutDBref,SQLStmtStr)
         IF (iRet .NE. 0) THEN
           IDM3 = 0
           RETURN
         ENDIF
      ENDIF

      WRITE(SQLStmtStr,*) 'INSERT INTO FVS_DM_Sz_Sum',
     -    ' (CaseID,StandID,Year,Type,',
     -    char(39),'0-3in',  char(39),',',
     -    char(39),'3-5in', char(39),',',
     -    char(39),'5-7in',char(39),',',
     -    char(39),'7-9in',char(39),',',
     -    char(39),'9-11in',char(39),',',
     -    char(39),'11-13in',char(39),',',
     -    char(39),'13-15in',char(39),',',
     -    char(39),'15-17in',char(39),',',
     -    char(39),'17-19in',char(39),',',
     -    char(39),'gt19in', char(39),') VALUES ',
     -    '(''',CASEID,''',''',TRIM(NPLT),
     -    ''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
      iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
      iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
      IF (iRet .NE. 0) THEN
         IDM3 = 0
         RETURN
      ENDIF
            
C     LOOP OVER 5 TYPES

      DO I = 1,5

C     LOOP OVER 5 TYPES, MAKING DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

        SELECT CASE (I)
          CASE (1)
            DO J = 1,10
              X(J) = DCTPA(J)
            ENDDO
          CASE (2)
            DO J = 1,10
              X(J) = DCINF(J)
            ENDDO
          CASE (3)
            DO J = 1,10
              X(J) = DCMRT(J)
            ENDDO
          CASE (4)
            DO J = 1,10
              X(J) = DCDMR(J)
            ENDDO
          CASE (5)
            DO J = 1,10
              X(J) = DCDMI(J)
            ENDDO
        END SELECT

        iRet=fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))

C       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber=1                 ! 1 YEAR
        iRet=fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

        ColNumber=ColNumber+1       ! Label
        iRet = fsql3_bind_text(IoutDBref,ColNumber,NLABS(I),
     >         len_trim(NLABS(I)))              
     
        DO J = 1,10
          ColNumber=ColNumber+1     ! SIZE CLASSES 2-11
          iRet=fsql3_bind_double(IoutDBref,ColNumber,X(J))
        ENDDO

        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_reset(IoutDBref) 

      ENDDO
      
      iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
      iRet = fsql3_finalize(IoutDBref)
      if (iRet.ne.0) then
         IDM3 = 0
         RETURN
      ENDIF

      RETURN
      END
