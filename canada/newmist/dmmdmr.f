      SUBROUTINE DMMDMR(New, Old)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMMDMR -- NISI  Date of last revision: April 9 1994 
C----------------------------------------------------------------------
C Purpose:
C   In order to keep the interim model spread equations distinct from
C the NISI, a different DMR array is used for the NISI. This routine
C copies the NISI value into the Interim model array, so that the 
C Interim model routines can process the results in a consistent way.
C----------------------------------------------------------------------
C
C Called by:
C
C     MISTOE 
C
C Other routines called:
C
C     [none]
C
C Argument list definitions:                        
C
C     INTEGER New     (I) DMR computed by the NISI
C     INTEGER Old     (O) DMR array used by the Interim model
C
C Local variable definitions:
C
C     INTEGER i           Loop counter for the treelist records
C
C Common block variables and parameters:
C
C     MAXTRE  PRGPRM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'

      INTEGER New
      INTEGER Old
      
      DIMENSION New(MAXTRE)
      DIMENSION Old(MAXTRE)

      INTEGER i

      DO 100 i = 1, ITRN
         Old(i) = New(i)
  100 CONTINUE      

      RETURN
      END
