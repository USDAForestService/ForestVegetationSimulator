      SUBROUTINE RDCMPR (NCLAS,PROB,IND,IND1)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  THIS SUBROUTINE IS USED TO COMPRESS THE TREE RECORDS ARRAYS FOR
C  THE ROOT DISEASE MODEL.
C
C  CALLED BY :
C     COMPRS  [PROGNOSIS]
C
C  CALLS     :
C     RDATV   (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     NCLAS  -
C     PROB   -
C     IND    -
C     IND1   -
CC  Revision History:
C    22-JUL-02 Lance R. David (FHTET)
C      Previous revision date noted was March 7, 1995.
C      Removed unused array PROBO. It was also unused in the old
C      annosus model.
C   08/27/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/2016 Lance R. David (FMSC)
C     Moved two conditions to exit to top.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDARRY.F77'  
      INCLUDE 'RDADD.F77'
C
COMMONS
C

      INTEGER  I, I1, I2, ICL, IDI, IENDYR, IND(MAXTRE), IND1(MAXTRE),
     &         IP, IREC, J, JJ, K, NCLAS
      REAL     PROB(MAXTRE), TPAREA, ROOT1, ROOT3,
     &         TXP, TXP1, TXP2, TXP3, TXP4, TXP5, TXP7, TXP10,
     &         TXP14(2), TXP15(3), XP
      LOGICAL  LGO, LTEE

      TXP7 = 0.0      

C     IENDYR IS THE LAST YEAR OF PROBI, PROPI THAT WILL HAVE INFO
C     WE DONT NEED TO DO ANY COMPRESSION ON LATER YEARS
      IENDYR = ICYC + 1
      
C     DO 557 I=1,ITRN
C     WRITE(JOSTND,777)  I,PROBI(I,1,1),PROBIU(I),PROBIT(I),
C    >      FPROB(I)
C 777 FORMAT(' IN RDCMPR BEFORE COMPRESS',I5,4F8.2)
C 557 CONTINUE
C

      CALL RDATV(LGO,LTEE)
      IF (.NOT. LGO) RETURN
      IF (.NOT. LTEE .AND. LSTART) RETURN
C
C     IF ROOT DISEASE NOT ACTIVE OR NO PATCH AREA THEN RETURN. ALSO
C     IF USING MANUAL DATA INITIALIZATION AND WE ARE IN CYCLE 0 ALSO
C     RETURN BECAUSE ROOT DISEASE ARRAYS ARE EMPTY. NOTE CODE IN RRMN1.
C
      TPAREA = 0.0
      DO 99 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
   99 CONTINUE     
      IF (TPAREA .EQ. 0.0) RETURN

C
C     COMPRESS THE ROOT DISEASE TREE RECORDS.
C
      I1 = 1
      DO 500 ICL=1,NCLAS
C
C        SET THE POINTERS TO THE TREES IN THE CLASS
C
         I2 = IND1(ICL)
C
C        IF THERE IS ONLY ONE RECORD IN CLASS ICL, THEN: SKIP THE CLASS
C
         IREC1 = IND(I1)

C        WRITE(JOSTND,333) I1,ICL,I2,IREC1
C  333    FORMAT(' RDCMPR I1,ICL,I2,IREC1',4I5)

         IF (I1 .EQ. I2) GOTO 480
C
C        CREATE A COMULATIVE PROB FOR ALL THE TREES IN THE CLASS.
C
         XP = PROB(IREC1)
         TXP = XP
         K = I1 + 1
         TXP1 = FPROB(IREC1)
         TXP2 = PROBIT(IREC1)
         TXP3 = PROBIU(IREC1)
         TXP4 = PROBL(IREC1)
         TXP5 = RRKILL(IREC1)
         TXP7 = RDKILL(IREC1)
         TXP10 = WK22(IREC1)
         TXP14(1) = FFPROB(IREC1,1)
         TXP14(2) = FFPROB(IREC1,2)

         DO 799 J=1,4
            IF (J .LT. 4) TXP15(J) = OAKL(J,IREC1)
            
            TXP12(J) = 0.0
            IF (XMTH(J,IREC1) .LT. 0.0) GOTO 799
            TXP12(J) = XMTH(J,IREC1)
  799    CONTINUE

         JJ = 0
         DO 899 J=1,IENDYR
           DO 890 IP=1,2
             JJ = JJ + 1
             TXP8(JJ) = PROBI(IREC1,J,IP)
  890      CONTINUE   
  899    CONTINUE

         DO 215 I=K,I2
            IREC = IND(I)
C           WRITE(JOSTND,334) IREC
C  334       FORMAT(' IREC',I5)
            XP = PROB(IREC)
            TXP = TXP + XP
            TXP1 = TXP1 + FPROB(IREC)
            TXP2 = TXP2 + PROBIT(IREC)
            TXP3 = TXP3 + PROBIU(IREC)
            TXP4 = TXP4 + PROBL(IREC)
            TXP5 = TXP5 + RRKILL(IREC)
            TXP7 = TXP7 + RDKILL(IREC)
            TXP10 = TXP10 + WK22(IREC)
            TXP14(1) = TXP14(1) + FFPROB(IREC,1)
            TXP14(2) = TXP14(2) + FFPROB(IREC,2)

            DO 699 J=1,4
               IF (J .LT. 4) TXP15(J) = TXP15(J) + OAKL(J,IREC)
               
               IF (XMTH(J,IREC) .LT. 0.0) GOTO 699
               TXP12(J) = TXP12(J) + XMTH(J,IREC)
  699       CONTINUE

            JJ = 0
            DO 888 J=1,IENDYR
              DO 880 IP=1,2
                JJ = JJ + 1
                TXP8(JJ) = TXP8(JJ) + PROBI(IREC,J,IP)
  880         CONTINUE
  888       CONTINUE
  215    CONTINUE

C
C        COMPUTE THE WEIGHTED AVERAGES OF ROOT RADIUS VARIABLES
C
         XP = PROB(IREC1)
         ROOT1 = ROOTL(IREC1) * XP
         ROOT3 = XP * RROOTT(IREC1)

         DO 499 J=1,4
            ROOT4(J) = 0.0
            IF (XMTH(J,IREC1) .LT. 0.0) GOTO 499
            ROOT4(J) = ROOTH(J,IREC1) * XMTH(J,IREC1)
  499    CONTINUE

         JJ = 0
         DO 905 J=1,IENDYR   
           DO 901 IP=1,2
             JJ = JJ + 1
             PROP1(JJ) = PROPI(IREC1,J,IP) * PROBI(IREC1,J,IP)
  901      CONTINUE
  905    CONTINUE

C
C        ADD IN THE OTHER TREES WHICH FORM THE CLASS.
C
         I1 = I1 + 1

         DO 300 I=I1,I2
            IREC = IND(I)
            XP = PROB(IREC)
            ROOT1 = ROOT1 + ROOTL(IREC) * XP
            ROOT3 = ROOT3 + RROOTT(IREC) * XP

            DO 498 J=1,4
               IF (XMTH(J,IREC) .LT. 0.0) GOTO 498
               ROOT4(J) = ROOT4(J) + ROOTH(J,IREC) * XMTH(J,IREC)
  498       CONTINUE

            JJ = 0
            DO 902 J=1,IENDYR
              DO 907 IP=1,2
                JJ = JJ + 1
                PROP1(JJ) = PROP1(JJ) + PROPI(IREC,J,IP) * 
     &                      PROBI(IREC,J,IP)
  907         CONTINUE
  902       CONTINUE

  300    CONTINUE

C
C        DIVIDE BY THE TOTAL PROB AND MOVE THE VALUES INTO
C        THE 'IREC1' POSITION IN THE ARRAYS.
C
         IF (TXP .EQ. 0.0) THEN
            ROOTL(IREC1) = 0.0
         ELSE
            ROOTL(IREC1) = ROOT1 / TXP
         ENDIF

         FPROB(IREC1) = TXP1
         PROBIT(IREC1) = TXP2
         WK22(IREC1) = TXP10
         FFPROB(IREC1,1) = TXP14(1) 
         FFPROB(IREC1,2) = TXP14(2) 

         DO 599 J=1,4
            IF (J .LT. 4) OAKL(J,IREC1) = TXP15(J)
            
            IF (TXP12(J) .LE. 0.0) GOTO 599
            XMTH(J,IREC1) = TXP12(J)
            ROOTH(J,IREC1) = ROOT4(J) / TXP12(J)
  599    CONTINUE

         IF (TXP .EQ. 0.0) THEN
            RROOTT(IREC1) = 0.0
         ELSE
            RROOTT(IREC1) = ROOT3 / TXP
         ENDIF

         PROBIU(IREC1) = TXP3
         PROBL(IREC1) = TXP4
         RRKILL(IREC1) = TXP5
         RDKILL(IREC1) = TXP7

         JJ = 0
         DO 889 J=1,IENDYR
           DO 885 IP=1,2
             JJ = JJ + 1
             PROBI(IREC1,J,IP) = TXP8(JJ)
             PROPI(IREC1,J,IP) = PROP1(JJ) / (TXP8(JJ) + .00001)
  885      CONTINUE           
  889    CONTINUE

C
C        END OF COMPRESSION LOOP
C
  480    CONTINUE
C
C        REDEFINE I1 SUCH THAT IT POINTS TO THE FIRST
C        INDEX IN THE NEXT CLASS.
C
         I1 = I2+1
  500 CONTINUE

C     DO 558 I=1,ITRN
C     WRITE(JOSTND,833)  I,PROBI(I,1),PROBIU(I),PROBIT(I),
C    >     FPROB(I)
C 833 FORMAT(' IN RDCMPR AFTER  COMPRESS',I5,4F8.2)
C 558 CONTINUE

      RETURN
      END
