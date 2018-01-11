      SUBROUTINE DBSCMPU
      IMPLICIT NONE
C
C $Id: dbscmpu.f 1934 2017-04-07 15:36:40Z lancedavid $
C
C     PURPOSE: TO POPULATE A DATABASE WITH THE COMPUTE TABLE
C              INFORMATION
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'KEYCOM.F77'
C
C
      INCLUDE 'DBSCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
      INTEGER MXLEN
      PARAMETER(MXLEN=2000)
      CHARACTER(LEN=MXLEN) SQLStmt
      CHARACTER(LEN=8),DIMENSION(MXTST5)::KWINSRT
      DOUBLE PRECISION,DIMENSION(MXTST5)::CURVAL
      LOGICAL LDUPKW
      INTEGER I,II,IX,I3,iRet,THISYR,ICY,NSRTNUM

      INTEGER fsql3_tableexists,fsql3_exec,fsql3_addcolifabsent
C
C     IF COMPUTE IS NOT TURNED ON OR THE NUMBER OF VARIABLES IS 0
C     THEN RETURN
C
      IF(ICOMPUTE.EQ.0.OR.ITST5.EQ.0) RETURN
C
C     Do we need the table, or all all variables with a _
C
      II=0
      DO I=1,ITST5
          IF(.NOT.(CTSTV5(I)(1:1).EQ.'_'.AND.I_CMPU.LT.1)) THEN
            II=1
            EXIT
          ENDIF
      ENDDO
      IF (II.EQ.0) RETURN
C
C     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID
C
      CALL DBSCASE(1)
C
C     CHECK TO SEE IF THE COMPUTE TABLE EXISTS IN DATBASE
C     IF IT DOES NOT THEN WE NEED TO CREATE IT
C
      iRet = fsql3_tableexists(IoutDBref,"FVS_Compute"//CHAR(0))
      IF(iRet.EQ.0) THEN
        SQLStmt='CREATE TABLE FVS_Compute('//
     -             'CaseID char(36) ,'//
     -             'StandID char(26) null,'//
     -             'Year int null'
        DO I=1,ITST5
          IF(.NOT.(CTSTV5(I)(1:1).EQ.'_'.AND.I_CMPU.LT.1)) THEN
            SQLStmt=TRIM(SQLStmt)//', '//
     -                 TRIM(CTSTV5(I))//' real null'
          ENDIF
        ENDDO
        iRet = fsql3_exec(IoutDBref,TRIM(SQLStmt)//CHAR(0))
        IF (iRet .NE. 0) THEN
          ICOMPUTE = 0
          RETURN
        ENDIF
      ELSE
C
C       MAKE SURE ALL THE NEEDED COLUMNS EXIST
C
        DO I=1,ITST5
          IF(.NOT.(CTSTV5(I)(1:1).EQ.'_'.AND.I_CMPU.LT.1)) THEN
          iRet = fsql3_addcolifabsent(IoutDBref, 
     >         "FVS_Compute"//CHAR(0), 
     >         TRIM(CTSTV5(I))//CHAR(0), "real"//CHAR(0))
          ENDIF
        ENDDO
      ENDIF
C
C     BUILD AND RUN THE INSERT COMMANDS
C
      DO ICY=1,NCYC
C
C     Make a list of variables and correspoinding values
C     for the current cycle
C
        NSRTNUM = 0
        THISYR = -1
        DO II=IMGPTS(ICY,1),IMGPTS(ICY,2)
          I=IOPSRT(II)
          IF(.NOT. (IACT(I,1).EQ.33 .AND. IACT(I,4).GT.0)) CYCLE
          IF(THISYR.EQ.-1) THISYR = IACT(I,4)
          IF(IACT(I,4).NE.THISYR) THEN
            IF(NSRTNUM.GT.0) THEN

C             Build and run an insert query

              CALL INSERTCMPU(IoutDBref,KWINSRT,CURVAL,THISYR,NPLT,
     >                        CASEID,NSRTNUM)

              NSRTNUM = 0
              THISYR = IACT(I,4)
            ENDIF
          ENDIF
          IX=IFIX(PARMS(IACT(I,2)+1))
          IF (IX.GT.500) IX=IX-500
C
C         CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
C
          IF(CTSTV5(IX)(1:1).EQ.'_'.AND.I_CMPU.LT.1) CYCLE
          IF (NSRTNUM.EQ.0) THEN
            KWINSRT(1) = CTSTV5(IX)
            CURVAL(1)=PARMS(IACT(I,2))
            NSRTNUM = 1
          ELSE
            LDUPKW =.FALSE.
            DO I3=1,NSRTNUM
              IF(TRIM(KWINSRT(I3)).EQ.TRIM(CTSTV5(IX))) THEN
                CURVAL(I3)=PARMS(IACT(I,2))
                LDUPKW =.TRUE.
                EXIT
              ENDIF
            ENDDO
            IF(.NOT.LDUPKW) THEN
              NSRTNUM = NSRTNUM + 1
              KWINSRT(NSRTNUM)=CTSTV5(IX)
              CURVAL(NSRTNUM)=PARMS(IACT(I,2))
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      
C     More to insert?

      IF(NSRTNUM.GT.0) 
     >    CALL INSERTCMPU(IoutDBref,KWINSRT,CURVAL,THISYR,NPLT,CASEID,
     >                    NSRTNUM)
      RETURN
      END
      
      SUBROUTINE INSERTCMPU(IoutDBref,KWINSRT,CURVAL,THISYR,STANDID,
     >                      CASEID,NSRTNUM)
      IMPLICIT NONE
C
C     INPUT: KWINSRT - THE ARRAY OF COMPUTE VARS TO INSERT
C            CURVAL  - THE ARRAY OF VALUES ASSOCIATED W/ THE VARS
C            NSRTNUM - THE NUMBER OF COMPUTE VARS TO INSERT
C
      INTEGER I,THISYR,IoutDBref,IRT,NSRTNUM
      CHARACTER(LEN=8) KWINSRT(NSRTNUM)
      DOUBLE PRECISION CURVAL(NSRTNUM)
      CHARACTER(LEN=26) STANDID
      CHARACTER(LEN=36) CASEID
      CHARACTER(LEN=3000) SQLStmt
      CHARACTER(LEN=200)  VALS
      INTEGER fsql3_prepare,fsql3_bind_int,fsql3_bind_double,
     >        fsql3_finalize,fsql3_errmsg,fsql3_step
      
      SQLStmt=" "
      IF(NSRTNUM.EQ.0) RETURN
      DO I=1,NSRTNUM
        SQLStmt = TRIM(SQLStmt) // "," // TRIM(KWINSRT(I))
        VALS    = TRIM(VALS)    // ",?"      
      ENDDO

      SQLStmt = "insert into FVS_Compute (CaseID,StandID,Year" //
     >   trim(SQLStmt) // ") values (" // CASEID // "," // 
     >   TRIM(STANDID) // ",?" // TRIM(VALS) // ");" // CHAR(0)
      
      IRT = fsql3_prepare(IoutDBref,SQLStmt)
      IF (IRT>0) THEN
        IRT = fsql3_errmsg(VALS, 200)
        PRINT *,"dbscmpu prepare error: ",TRIM(VALS)
      endif
      IRT = fsql3_bind_int(IoutDBref, 1, THISYR)
      DO I=1,NSRTNUM 
        IRT = fsql3_bind_double(IoutDBref,I+1,CURVAL(I))
      ENDDO
      IRT = fsql3_step(IoutDBref)
      IRT = fsql3_finalize(IoutDBref)
      IF (IRT>0) THEN
        IRT = fsql3_errmsg(IoutDBref,VALS,200)
        PRINT *,"dbscmpu finalize error: ",TRIM(VALS)
      endif
      RETURN
      END
      
      
      



