      SUBROUTINE FMCMPR (NCLAS)
      IMPLICIT NONE
C----------
C  **FMCMPR  FIRE--DATE OF LAST REVISION:  06/21/00
C----------
C
C  THIS SUBROUTINE IS USED TO COMPRESS THE TREE RECORDS ARRAYS FOR
C  THE FIRE/SNAG MODEL.
C
C  CALLED BY :
C     COMPRS  [PROGNOSIS]
C
C  PARAMETERS :
C     NCLAS  -  NUMBER OF CLASSES (RECORDS) FOLLOWING COMPRESSION
C     PROB   -  TREES/ACRE FOR EACH RECORD
C     IND    -  INDEX OF RECORD BEING COMPRESSED
C     IND1   -  TARGET INDEX OF RECORD FOLLOWING COMPRESSION
C
COMMONS
C
      INCLUDE 'PRGPRM.F77' 
C      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'

      REAL TXP6(0:5), TXP7(0:5)
      LOGICAL DEBUG 
      INTEGER NCLAS,I1,ICL,I2,K,JJ,I,IREC
      REAL    XP,TXP,TXP4,TXP5,TXP8,TXP9
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCMPR',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON
 7    FORMAT(' ENTERING ROUTINE FMCMPR CYCLE = ',I2,' LFMON=',L2)	
      
C     NO COMPRESSION IS REQUIRED IF THE FIRE/SNAG MODEL IS ABSENT
      
      IF (.NOT. LFMON) RETURN
      
      I1 = 1
      DO 500 ICL = 1, NCLAS
         
C     SET THE POINTERS TO THE TREES IN THE CLASS
         
         I2 = IND1(ICL)
         
C       IF THERE IS ONLY ONE RECORD IN CLASS ICL, THEN: SKIP THE CLASS

         IREC1 = IND(I1)
         IF (I1 .EQ. I2) GOTO 480
         
C        CREATE CUMULATIVE GROUPS. THOSE NOT IN #/ACRE ARE WEIGHTED 
         
C         XP   = PROB(IREC1)

         XP   = FMPROB(IREC1)
         TXP  = XP
         K    = I1 + 1
         
         TXP4 = OLDHT(IREC1)  * XP
         TXP5 = OLDCRL(IREC1) * XP
         DO JJ = 0, 5
            TXP6(JJ) = OLDCRW(IREC1, JJ) * XP
            TXP7(JJ) = CROWNW(IREC1, JJ) * XP
         ENDDO
         TXP8 = GROW(IREC1) * XP

         TXP9  = FMICR(IREC1) * XP
         
         DO I = K, I2
            IREC = IND(I)
C            XP   = PROB(IREC)
            XP   = FMPROB(IREC)
            TXP  = TXP + XP
            TXP4 = TXP4 + OLDHT(IREC)  * XP
            TXP5 = TXP5 + OLDCRL(IREC) * XP
            DO JJ = 0, 5
               TXP6(JJ) = TXP6(JJ) + OLDCRW(IREC, JJ) * XP
               TXP7(JJ) = TXP7(JJ) + CROWNW(IREC, JJ) * XP
            ENDDO
            TXP8  = TXP8  + GROW(IREC)   * XP
            TXP9  = TXP9  + FMICR(IREC1) * XP
         ENDDO
         
C        MOVE COMPRESSED VALUES TO THE 'IREC1' POSITION
         
         FMPROB(IREC1) = TXP        
         IF (TXP .GT. 0.) THEN
            OLDHT(IREC1)  = TXP4 / TXP
            OLDCRL(IREC1) = TXP5 / TXP
            DO JJ = 0, 5
               OLDCRW(IREC1, JJ) = TXP6(JJ) / TXP
               CROWNW(IREC1, JJ) = TXP7(JJ) / TXP
            ENDDO
            GROW(IREC1)  = TXP8  / TXP
            FMICR(IREC1)  = IFIX(TXP9  / TXP + 0.5)
         ELSE
            OLDHT(IREC1)  = 0.
            OLDCRL(IREC1) = 0.
            DO JJ = 0, 5
               OLDCRW(IREC1, JJ) = 0.
               CROWNW(IREC1, JJ) = 0.
            ENDDO
            GROW(IREC1)   = 0.
            FMICR(IREC1)  = 0
         ENDIF

C        REDEFINE I1 SO THAT IT POINTS TO THE FIRST INDEX IN
C        THE NEXT CLASS.

 480     CONTINUE
         I1 = I2+1
         
 500  CONTINUE

      RETURN
      END

