      SUBROUTINE RDOWI
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     Windthrow model subroutine.  This subroutine is used to
C     normalize the array that holds the relative susceptibility 
C     to windthrow for each tree species.
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Local Variables :
C     ISPI   - INT
C              Counter used to loop through the tree species.
C              Used as index to species arrays.
C     SUM    - REAL
C              Sum of all tree species susceptibilites to windthrow.
C
C  Common Block Variables Used :
C     IRTSPC - (RDCOM)   (I)
C     MAXSP  - (PRGPRM)  (I)
C     ROWIBP - (RDCOM)   (I/O)
C
C  Revision History :
C     06/12/96 - Matthew K. Thompson
C                Removed the entry point RDOWIN and put it into its
C                own subroutine.
C   08/29/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'RDCOM.F77'

      INTEGER  ISPI
      REAL     SUMX

C.... Normalize relative susceptability to windthrow     

      SUMX = 0.0
      DO 10 ISPI = 1,MAXSP
         SUMX = SUMX + ROWIBP(IRTSPC(ISPI),1)
   10 CONTINUE

      DO 20 ISPI = 1,MAXSP
         ROWIBP(IRTSPC(ISPI),2) = ROWIBP(IRTSPC(ISPI),1) / SUMX
   20 CONTINUE

      RETURN
      END

