      SUBROUTINE FMOLDC
      IMPLICIT NONE
C----------
C  **FMOLDC  FIRE--DATE OF LAST REVISION:  06/21/00
C----------
C
C     CALLED FROM: FMMAIN
C
C  Purpose:
C     This subroutine records some crown size information for use in
C     calculating the amount of litterfall due to crown lifting in the
C     next cycle.  The proportion of total crown weight that will fall
C     in the next cycle is equal to ratio of the increase in the bottom
C     height of the crown to the total length of the crown (i.e., [new
C     bottom - old bottom] / old length).  The material to fall will be
C     added to down debris at a constant rate throughout the next FVS
C     cycle.  (In 'reality', some of this material would not fall until
C     later cycles, and some of the slow-falling material from earlier
C     cycles would fall during the current cycle.  The two effects 
C     partially cancel each other out, so simply falling all of the 
C     dead material derived from the current crown lift in the current
C     cycle is not grossly inaccurate).
C
C
C  Local variable definitions:
C
C  Common block variables and parameters:
C
C
      
C.... Parameter statements.

C.... Parameter include files.   
      INCLUDE 'PRGPRM.F77'
C      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'FMCOM.F77'

C.... Variable declarations.
      
      INTEGER I, J    

C.... Begin routine.  

C     Loop through the tree list, recording the current height, crown
C     length, and crown weights of each record.  

      DO I=1,ITRN
         OLDHT(I) = HT(I)
         OLDCRL(I) = HT(I) * (FLOAT(FMICR(I)) / 100.0)
         DO J=0,5
            OLDCRW(I,J) = CROWNW(I,J)
         ENDDO
      ENDDO
      RETURN
      END

