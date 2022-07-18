      SUBROUTINE FMSVTOBJ(IFTYP)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     A.H.DALLMANN -- RMRS MOSCOW -- JANUARY 2000
C
C     COPIES THE CURRENT CONTENTS OF IOBJTP VECTOR INTO TIOBJTP VECTOR
C     SO THAT SVMORT (ETC) CAN MANIPULATE THE OBJECT LIST AND CAN BE COMPARED
C     AN ORIGINAL.
C     
C     CALLED FROM FMEFF. 
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'SVDATA.F77'
C
C
      INCLUDE 'SVDEAD.F77'
C
C
      INCLUDE 'FMSVCM.F77'
C
C
COMMONS
C
      INTEGER I,IFTYP
      
      DO I = 1,NSVOBJ 
         IOBJTPTMP(I) = IOBJTP(I)
         IS2FTMP(I) = IS2F(I)
      ENDDO

C     SINCE THIS IS ONLY CALLED FROM FMEFF.F, WE NEED TO
C     CONVERT STANDING "GREEN" AND "RED" SNAGS INTO STANDING BURNED
C     SNAGS (CROWNING AND TORCHING FIRES ONLY)

      DO I = 1,NSVOBJ 
         IF (IOBJTP(I) .EQ. 2) THEN
            IF (FALLDIR(IS2F(I)) .EQ. -1) THEN
               IF (IFTYP.EQ.1 .OR. IFTYP.EQ.2 ) THEN
                  IF (ISTATUS(IS2F(I)) .EQ. 2) THEN
                     ISTATUS(IS2F(I)) = 5
                  ELSE IF (ISTATUS(IS2F(I)) .EQ. 3) THEN
                     ISTATUS(IS2F(I)) = 6
                  ELSE IF (ISTATUS(IS2F(I)) .EQ. 4) THEN
                     ISTATUS(IS2F(I)) = 6
                  ENDIF
               ENDIF
            ELSE
C     
C              ALWAYS CONVERT LAYING SNAGS INTO BURNED SNAGS.
C     
               IF (ISTATUS(IS2F(I)) .EQ. 2) THEN
                  ISTATUS(IS2F(I)) = 5
               ELSE IF (ISTATUS(IS2F(I)) .EQ. 3) THEN
                  ISTATUS(IS2F(I)) = 6
               ELSE IF (ISTATUS(IS2F(I)) .EQ. 4) THEN
                  ISTATUS(IS2F(I)) = 6
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      RETURN
      END

















