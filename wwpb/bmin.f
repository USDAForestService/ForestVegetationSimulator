      SUBROUTINE BMIN(LKECHO)
C      IMPLICIT NONE
C----------
C WWPB $Id$
C----------
C
C     OPTION PROCESSOR FOR STAND-LEVEL WWPB MODEL KEYWORDS
C     CREATED 8/18/05 AJ McMAHAN, ITX, Inc. Ft. Collins, CO
C     CALLED FROM: INITRE 
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'
C
COMMONS
C
      INTEGER    KWCNT
      PARAMETER (KWCNT = 10)

      CHARACTER*8  TABLE(KWCNT), KEYWRD, PASKEY
      CHARACTER*10 KARD(7)
      CHARACTER*250 OUTNAM
      LOGICAL      LNOTBK(7),LKECHO
      INTEGER      NPRMS,IDT,NUMBER,KODE,IPRMPT,MYACT,KEY
      REAL         PRMS(7), ARRAY(7)

      DATA TABLE /
     >     'MAINOUT ','TREEOUT ','BKPOUT  ','VOLOUT  ','END     ',
     >     '        ','        ','        ','        ','        '/

C------------------------------------------------------------------
C
C     **********            EXECUTION BEGINS            **********
C
   10 CONTINUE
      CALL KEYRDR (IREAD,JOSTND,.FALSE.,KEYWRD,LNOTBK,
     >             ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)
C
C  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
C               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.
C
      IF (KODE.LT.0) THEN
         IPRMPT=-KODE
      ELSE
         IPRMPT=0
      ENDIF
      IF (KODE .LE. 0) GO TO 30
      IF (KODE .EQ. 2) CALL ERRGRO(.FALSE.,2)
      CALL ERRGRO (.TRUE.,6)
      GOTO 10
   30 CONTINUE
      CALL FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.FALSE.,JOSTND)
C
C     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.
C
      IF (KODE .EQ. 0) GOTO 90
      IF (KODE .EQ. 1) THEN
         CALL ERRGRO (.TRUE.,1)
         GOTO 10
      ENDIF
      GOTO 90
C
C     SPECIAL END-OF-FILE TARGET
C
   80 CONTINUE
      CALL ERRGRO (.FALSE.,2)
   90 CONTINUE
C
C     PROCESS OPTIONS
C
      GO TO( 100,200,300,400,500,600,700,800,900,1000),NUMBER
C
  100 CONTINUE
C------------------------------------------------------------------------------
C                        OPTION NUMBER 1 -- MAINOUT
C
      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmm'
C
C     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
      IF(.NOT. LBMAIN)THEN
         CALL MYOPEN (JBMAIN,OUTNAM,1,133,0,1,1,0,KODE)
         IF (KODE.GT.0) THEN
            WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
         ENDIF
         LBMAIN = .TRUE.
      ENDIF
C
      IDT = 1
      PRMS(1) = 100
      PRMS(2) = 5.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,115) KEYWRD,OUTNAM,JBMAIN,IDT,INT(PRMS(1)),
     &                                            INT(PRMS(2))
  115 FORMAT(/1X,A8,'   THE WWPB MODEL *MAIN* OUTPUT FILE: ',A,
     &      '(UNIT REFERENCE #: ',I3,')'/T13,'WILL BEGIN',
     &      ' PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2701  !
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10

  200 CONTINUE
C------------------------------------------------------------------------------
C                        OPTION NUMBER 2 -- TREEOUT
C
      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmt'
C
C     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
      IF(.NOT. LBMTRE)THEN
         CALL MYOPEN (JBMTRE,OUTNAM,1,133,0,1,1,0,KODE)
         IF (KODE.GT.0) THEN
            WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
         ENDIF
         LBMTRE = .TRUE.
      ENDIF
C
      IDT = 1
      PRMS(1) = 100
      PRMS(2) = 5.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,215) KEYWRD,OUTNAM,JBMTRE,IDT,INT(PRMS(1)),
     &                                            INT(PRMS(2))
  215 FORMAT(/1X,A8,'   THE WWPB MODEL *DETAILED TREE* OUTPUT FILE: ',A,
     &      '(UNIT REFERENCE #: ',I3,')'/
     &  T13,'WILL BEGIN PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2702  !
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10

  300 CONTINUE
C------------------------------------------------------------------------------
C                        OPTION NUMBER 3 -- BKPOUT
C
      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmb'
C
C     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
      IF(.NOT. LBMBKP)THEN
         CALL MYOPEN (JBMBKP,OUTNAM,1,133,0,1,1,0,KODE)
         IF (KODE.GT.0) THEN
            WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
         ENDIF
         LBMBKP = .TRUE.
      ENDIF
C
      IDT = 1
      PRMS(1) = 100
      PRMS(2) = 5.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,315) KEYWRD,OUTNAM,JBMBKP,IDT,INT(PRMS(1)),
     &                                            INT(PRMS(2))
  315 FORMAT(/1X,A8,'   THE WWPB MODEL *DETAILED BKP* OUTPUT FILE: ',A,
     &      '(UNIT REFERENCE #: ',I3,')'/
     &  T13,'WILL BEGIN PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2703  !
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10

  400 CONTINUE
C------------------------------------------------------------------------------
C                        OPTION NUMBER 4 -- VOLOUT
C
      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmv'
C
C     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
      IF(.NOT. LBMVOL)THEN
         CALL MYOPEN (JBMVOL,OUTNAM,1,133,0,1,1,0,KODE)
         IF (KODE.GT.0) THEN
            WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
         ENDIF
         LBMVOL = .TRUE.
      ENDIF
C
      IDT = 1
      PRMS(1) = 100
      PRMS(2) = 5.0
      IF (LNOTBK(1)) IDT = INT(ARRAY(1))
      IF (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))
C
      IF(LKECHO)WRITE(JOSTND,415) KEYWRD,OUTNAM,JBMVOL,IDT,INT(PRMS(1)),
     &                                            INT(PRMS(2))
  415 FORMAT(/1X,A8,'   THE WWPB MODEL *DETAILED VOLUME* OUTPUT FILE: ',
     &      A,'(UNIT REFERENCE #: ',I3,')'/
     &  T13,'WILL BEGIN PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,
     &      ' YEARS USING ',I2,' YEAR INCREMENTS.')
C
      NPRMS = 2
      MYACT = 2704  !
      CALL OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
      GOTO 10

  500 CONTINUE
C------------------------------------------------------------------------------
C                        OPTION NUMBER 5 -- END
C
      IF(LKECHO)WRITE(JOSTND,510) KEYWRD
  510 FORMAT (/1X,A8,'   END OF SINGLE-STAND WWPB OPTIONS.')
      GOTO 2000
C
  600 CONTINUE
C                       OPTION NUMBER 6--

      GOTO 10
C
  700 CONTINUE
C                       OPTION NUMBER 7--
C
      GOTO 10
C
  800 CONTINUE
C                       OPTION NUMBER 8--
C
      GOTO 10
C
  900 CONTINUE
C                       OPTION NUMBER 9--
C
      GOTO 10
C
 1000 CONTINUE
C                       OPTION NUMBER 10--
C
      GOTO 10
C
C-----------------------------------------------------------------------------
 2000 CONTINUE
      RETURN
C------------------------------------------------------------------------------
C
C     CALL FROM OPLIST
C
      ENTRY BMKEY (KEY,PASKEY)
      PASKEY= TABLE(KEY)
      RETURN
 
      END
