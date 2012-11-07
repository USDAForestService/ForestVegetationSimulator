      SUBROUTINE SDEFET (LNOTBK,ARRAY,KEYWRD,
     >                   LOPEVN,IACTK,KARD,IPRMPT)
      IMPLICIT NONE
C----------
C  **SDEFET--BASE   DATE OF LAST REVISION:  07/23/08
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
C     PROCESSES BFDEFECT OR MCDEFECT KEYWORDS - CALLED FROM INITRE.
C
      LOGICAL LNOTBK(7),LOPEVN
      CHARACTER*8 KEYWRD
      CHARACTER*10 KARD(7)
      REAL ARRAY(7),XX(6),YY(6)
      INTEGER IPRMPT,IACTK,IDT,KODE,IS,N,I,ILEN,IGRP,IULIM,IG,IGSP
      INTEGER I1,I2,IRTNCD
      REAL ALGSLP
C
C     IF IPRMPT IS GT 0, THEN THE PARMS OPTION IS BEING USED.
C
      IF (IPRMPT.GT.0) THEN
         IF (LNOTBK(1)) THEN
            IDT=IFIX(ARRAY(1))
         ELSE
            IDT=1
         ENDIF
         IF (IPRMPT.NE.2) THEN
            CALL KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
            CALL ERRGRO (.TRUE.,25)
         ELSE
            CALL OPNEWC (KODE,JOSTND,IREAD,IDT,IACTK,KEYWRD,KARD,
     >                   IPRMPT,IRECNT,ICYC)
            CALL fvsGetRtnCode(IRTNCD)
            IF (IRTNCD.NE.0) RETURN
         ENDIF
      ELSE
C
C        CHECK THE SPECIES CODE.
C
         CALL SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,
     &                ARRAY,KARD)
         IF (IS.EQ.-999) RETURN
C
C        FIX UP MISSING NUMBERS.
C
         XX(1)=0.
         YY(1)=0.
         N=1
         DO 10 I=3,7
         IF (LNOTBK(I)) THEN
            N=N+1
            XX(N)=FLOAT(I-2)*5.0
            YY(N)=ARRAY(I)
         ENDIF
   10    CONTINUE
         IF (N.LE.1) THEN
            CALL KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
            CALL ERRGRO (.TRUE.,4)
            RETURN
         ENDIF
         IF (N.LT.6) THEN
            DO 20 I=3,7
            IF (.NOT.LNOTBK(I)) ARRAY(I)=
     >                          ALGSLP(FLOAT(I-2)*5.0,XX,YY,N)
   20       CONTINUE
         ENDIF
C
C        IF THE DATE IS DEFINED OR AN EVENT IS OPENED (KEYWORD
C        IS BETWEEN A THEN AND AN ENDIF, THEN SCHEDULE THE
C        DEFECT TERM CHANGE.
C
         IF (LNOTBK(1).OR.LOPEVN) THEN
            IDT=1
            IF (LNOTBK(1)) IDT=IFIX(ARRAY(1))
            CALL OPNEW (KODE,IDT,IACTK,6,ARRAY(2))
            WRITE (KARD(2)(7:10),'(I4)') IDT
            ILEN=3
            IF(IS.LT.0)ILEN=ISPGRP(-IS,52)
            WRITE (JOSTND,30) KEYWRD,'DATE/CYCLE= ' // KARD(2)(7:10),
     >                        KARD(2)(1:ILEN),IS,(ARRAY(I),I=3,7)
   30       FORMAT(/1X,A8,T13,A,'; SPECIES=',A,' (CODE=',
     >             I2,'); 5 INCH TREES=',F6.2,'; 10 INCH TREES=',F6.2,
     >             '; 15 INCH TREES=',F6.2,/T13,'20 INCH TREES=',F6.2,
     >             ';  25 INCH AND LARGER TREES=',F6.2)
         ELSE
            ILEN=3
            IF(IS.LT.0)ILEN=ISPGRP(-IS,52)
            WRITE (JOSTND,30) KEYWRD,'DEFECT CHANGED',
     >             KARD(2)(1:ILEN),IS,(ARRAY(I),I=3,7)
C
C           THE DATE IS NOT DEFINED, CHANGE THE DEFECT TERMS NOW.
C           IF THE SPECIES CODE IS:
C                <0 CHANGE ALL SPECIES IN THE GROUP,
C                =0 CHANGE ALL SPECIES,
C                >0 CHANGE THE ONE POINTED TO.
C
            IF(IS .LT. 0.) THEN
              IGRP = -IS
              IULIM = ISPGRP(IGRP,1)+1
              DO 4321 IG=2,IULIM
              IGSP = ISPGRP(IGRP,IG)
              IF(IACTK .EQ. 215) THEN
                CFDEFT(2,IGSP)=ARRAY(3)
                CFDEFT(3,IGSP)=ARRAY(4)
                CFDEFT(4,IGSP)=ARRAY(5)
                CFDEFT(5,IGSP)=ARRAY(6)
                CFDEFT(6,IGSP)=ARRAY(7)
                CFDEFT(7,IGSP)=ARRAY(7)
                CFDEFT(8,IGSP)=ARRAY(7)
                CFDEFT(9,IGSP)=ARRAY(7)
              ELSEIF(IACTK .EQ. 216)THEN
                BFDEFT(2,IGSP)=ARRAY(3)
                BFDEFT(3,IGSP)=ARRAY(4)
                BFDEFT(4,IGSP)=ARRAY(5)
                BFDEFT(5,IGSP)=ARRAY(6)
                BFDEFT(6,IGSP)=ARRAY(7)
                BFDEFT(7,IGSP)=ARRAY(7)
                BFDEFT(8,IGSP)=ARRAY(7)
                BFDEFT(9,IGSP)=ARRAY(7)
              ENDIF
 4321         CONTINUE
            GO TO 100
            ENDIF
C
            IF (IS.EQ.0) THEN
               I1=1
               I2=MAXSP
            ELSE
               I1=IS
               I2=IS
            ENDIF
C
C           LOAD THE COEFFICIENTS.
C
            DO 60 IS=I1,I2
            DO 50 I=2,6
            IF (IACTK.EQ.216) BFDEFT(I,IS)=ARRAY(I+1)
            IF (IACTK.EQ.215) CFDEFT(I,IS)=ARRAY(I+1)
   50       CONTINUE
            BFDEFT(7,IS)=ARRAY(7)
            BFDEFT(8,IS)=ARRAY(7)
            BFDEFT(9,IS)=ARRAY(7)
            CFDEFT(7,IS)=ARRAY(7)
            CFDEFT(8,IS)=ARRAY(7)
            CFDEFT(9,IS)=ARRAY(7)
   60       CONTINUE
  100       CONTINUE
         ENDIF
      ENDIF
      RETURN
      END
