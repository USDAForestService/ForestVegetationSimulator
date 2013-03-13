      SUBROUTINE DMSLOP(Dstnce, Offset)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMSLOP -- NISI  Date of last revision: April 14 1994 
C----------------------------------------------------------------------
C Purpose:
C   Trees that are uphill or downhill from the target may have a
C different effect upon spread, if their position is different enough
C to shift the frame of reference more than 1 (or more) MESH units.
C Given the distance between a target and a source (in MESH units),
C the routine simulates the difference in height (in MESH units)
C between the ground under the source and target trees. This involves
C knowing the site slope (0-1, in rise/run; 1 = 45 degrees) and
C choosing a random angle (0 - 2 pi radians) relative to the gradient
C of the slope. Azimuth angle 0 is 'uphill'.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     DMRANN
C
C Argument list definitions:                        
C
C     INTEGER Dstnce   (I) The sampling ring in which the target tree
C                           is located.
C     INTEGER Offset   (O) The MESH unit offset betwee source and
C                           target trees.
C
C Local variable definitions:
C
C     REAL    Rnd          A uniform {0,1} random number.
C     REAL    D            The distance between the target tree and
C                           the midpoint of the sampling ring.
C
C Common block variables and parameters:
C
C     SLOPE   PLOT
C
C*********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'

C Argument list variables.

      INTEGER Dstnce
      INTEGER Offset     

C Local variables.
      
      REAL    Rnd
      REAL    D
      
      D = FLOAT(Dstnce) - 0.5

C Choose a random position around the ring quadrat, then determine the
C integer amount of the difference.

      CALL DMRANN(Rnd)     
      Offset = INT(D * COS(6.283185 * Rnd) * SLOPE)
      
      RETURN
      END
