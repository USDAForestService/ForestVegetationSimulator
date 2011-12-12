      SUBROUTINE ESADDT(ICALL)
      IMPLICIT NONE
C----------
C  **ESADDT DATE OF LAST REVISION:  07/25/08
C----------

C     ADD NEW TREES FROM A FILE OR EXTERNAL DATA BASE.

C     COMMONS

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ESHAP.F77'
      INCLUDE 'ESHAP2.F77'
      INCLUDE 'VARCOM.F77'

      INTEGER       ICALL

      LOGICAL       DEBUG
      CHARACTER*250 CMDLN
      CHARACTER*250 FNAM
      CHARACTER*4   CYR
      CHARACTER*7   VVER
      INTEGER       I,J,KODE,IKEEP,IPYR
      INTEGER       MYACT(1),NTODO,ITODO,NP,IACTK,IDT,KDT,IMETH
      INTEGER       FNO
      REAL          PRMS(2),SUM,SUMSP(8)

C     ADDTREES CODE = 432

      DATA MYACT/432/
      DATA FNO  /101/

C     SET THE DEBUG OPTION

      CALL DBCHK (DEBUG,'ESADDT',6,ICYC)

C     PROCESS ADDTREES KEYWORDS

      CALL OPFIND (1,MYACT,NTODO)
      IF (DEBUG) WRITE (JOSTND,5) NTODO
    5 FORMAT (' IN ESADDT: OPTS NTODO=',I2)
      IF (NTODO .GT. 0) THEN
        KDT = IY(ICYC+1)-1
        DO ITODO = 1,NTODO
          CALL OPGET(ITODO,2,IDT,IACTK,NP,PRMS)

C         GET THE METHOD SWITCH

          IMETH=1
          IF (IABS(NP) .GE. 2) THEN
            IPYR =INT(PRMS(1))+KDT ! planting year
            IMETH=INT(PRMS(2))
          ENDIF
          IF (DEBUG) WRITE (JOSTND,6) IMETH
    6     FORMAT (' IN ESADDT, IMETH=',I2)

          SELECT CASE (IMETH)

c           Run external program
            CASE (1)

              CALL OPGETC (ITODO,CMDLN)
              IF (CMDLN .EQ. ' ') GOTO 101

c             Construct input filename: Keyfile_StandID_Year_BM.ES1
              J = 0
              DO I = 1,250
                FNAM(I:I) = CHAR(32)          ! put space code
                IF (KWDFIL(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = KWDFIL(I:I)
                ENDIF
              ENDDO
              J = J+1
              FNAM(J:J) = '_'
              DO I = 1,26
                IF (NPLT(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = NPLT(I:I)
                ENDIF
              ENDDO
              J = J+1
              FNAM(J:J) = '_'
              WRITE(CYR,'(I4)') KDT
              DO I = 1,4
                IF (CYR(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = CYR(I:I)
                ENDIF
              ENDDO
              J = J+1
              FNAM(J:J) = '_'
              CALL VARVER(VVER)
              DO I = 1,2
                IF (VVER(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = VVER(I:I)
                ENDIF
              ENDDO
              FNAM(J+1:J+5) = ".es1"

c             open and populate file with information to send to
c             external regeneration model: suffix = ".es1"

              CALL MYOPEN (FNO,FNAM(1:LEN_TRIM(FNAM)),
     >          5,150,0,1,1,0,KODE)
              IF (KODE .NE. 0) GOTO 101

              ! concatenate the name of the exe and input fn
              J = LEN_TRIM(CMDLN)+1
              CMDLN(J:J) = ' '
              DO I = 1,LEN_TRIM(FNAM)
                IF (FNAM(I:I) .NE. ' ' .AND. J .LE. 250) THEN
                  J = J+1
                  CMDLN(J:J) = FNAM(I:I)
                ENDIF
              ENDDO

              ! StandID (1)
              WRITE(FNO,'(A30)',ERR=101) NPLT(1:LEN_TRIM(NPLT))
              ! Year (2)
              WRITE(FNO,'(I30)',ERR=101) IPYR
              ! ADDTREES fld2 (3)
              WRITE(FNO,'(I30)',ERR=101) INT(PRMS(1))

c             Data requirements for Blue Mountains estab model
              WRITE(FNO,'(I30)',  ERR=101) KODTYP       !hab code (4)
              WRITE(FNO,'(I30)',  ERR=101) ISLOP        !slope    (5)
              WRITE(FNO,'(I30)',  ERR=101) IASPEC       !aspect   (6)
              WRITE(FNO,'(F30.1)',ERR=101) ELEV*100.    !elev     (7)
              WRITE(FNO,'(F30.1)',ERR=101) SDIBC/GROSPC !bsdi     (8)
              WRITE(FNO,'(F30.1)',ERR=101) SDIAC/GROSPC !asdi     (9)
              WRITE(FNO,'(F30.1)',ERR=101) OLDBA/GROSPC !ba       (10)
              WRITE(FNO,'(F30.1)',ERR=101) ATBA/GROSPC  !aba      (11)
              SUM = 0.0
              DO I = 1,8
                SUMSP = 0.0
              ENDDO
              DO I = 1,ITRN
                IF (DBH(I) .GT. 1.0) THEN
                  SUM = SUM + PROB(I)
                ELSE
                  SELECT CASE (ISP(I))
                    CASE (4)  ! ABGR
                      SUMSP(1) = SUMSP(1) + PROB(I)
                    CASE (9)  ! ABLA
                      SUMSP(2) = SUMSP(2) + PROB(I)
                    CASE (2)  ! LAOC
                      SUMSP(3) = SUMSP(3) + PROB(I)
                    CASE (7)  ! PICO
                      SUMSP(4) = SUMSP(4) + PROB(I)
                    CASE (1)  ! PIMO
                      SUMSP(5) = SUMSP(5) + PROB(I)
                    CASE (8)  ! PIEN
                      SUMSP(6) = SUMSP(6) + PROB(I)
                    CASE (10) ! PIPO
                      SUMSP(7) = SUMSP(7) + PROB(I)
                    CASE (5)  ! PSME
                      SUMSP(8) = SUMSP(8) + PROB(I)
                  END SELECT
                ENDIF
              ENDDO
              WRITE(FNO,'(F30.1)',ERR=101) SUM   ! stand tpa >1" (12)
              ! spp tpa <1" (13-20)
              DO I = 1,8
                WRITE(FNO,'(F30.1)',ERR=101) SUMSP(I)
              ENDDO

              CLOSE(FNO)
              CALL SYSTEM(CMDLN)

              J = LEN_TRIM(FNAM)
              FNAM(J-3:J) = ".es2"
              CALL MYOPEN (FNO,FNAM(1:LEN_TRIM(FNAM)),
     >          5,150,0,1,1,0,KODE)
              IF (KODE .NE. 0) GOTO 101
              READ(FNO,'(I10)',END=101) IKEEP
              CALL OPRDAT(FNO,KODE)
              IF (KODE .GE. 0) THEN
                CALL OPDONE(ITODO,IDT)
                GOTO 202
              ENDIF
  101         CALL OPDEL1(ITODO)
  202         IF (IKEEP .NE. 1) THEN
                CLOSE(FNO, STATUS = 'DELETE')
              ELSE
                CLOSE(FNO, STATUS = 'KEEP')
              ENDIF
          END SELECT
  404   ENDDO
      ENDIF

      RETURN
      END
