      SUBROUTINE DMFBRK
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  **DMFBRK -- NISI  Date of last revision April 7 1994
C----------------------------------------------------------------------
C Purpose:
C   This routine locates the four breakpoints that define the crown
C thirds of each tree record. Crown thirds are the basic vertical
C spatial unit used by the model, and are drawn from Hawksworth's
C rating system. BrkPnt(1) is the height at the TOP of the crown;
C BrkPnt(4) is the BOTTOM of the crown. BrkPnt(2) and BrkPnt(3)
C divide the crown thirds. The BrkPnt() units are MESH.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     [none]
C
C Argument list definitions:                        
C
C     [none]
C           
C Local variable definitions:
C     
C     INTEGER i           loop counter for tree records
C     INTEGER j           loop counter for crown thirds
C     REAL    x           scalar constant
C     REAL    y           scalar constant
C     REAL    z           MESH units per crown third
C
C Common block variables and parameters:
C
C     ITRN    CONTRL
C     HT      ARRAYS
C     ICR     ARRAYS
C     FPM     DMCOM
C     MESH    DMCOM
C     CRTHRD  DMCOM
C     BPCNT   DMCOM
C     BrkPnt  DMCOM
C      
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'

C Local variables.

      INTEGER i, j
      REAL    x, y, z

C These conversions are required because 'HT()' is measured in feet and
C 'ICR()' is a percentage. The model requires MESH units (usually 2
C meters.

      y= 1.0 / (FPM * MESH)
      x= 0.01 * y / FLOAT(CRTHRD)
      
      DO 100 i = 1, ITRN
          z = HT(i) * FLOAT(ICR(i)) * x 
          BrkPnt(i, 1) = HT(i) * y
          DO 200 j = 2, BPCNT
             BrkPnt(i, j) = BrkPnt(i, j - 1) - z
  200     CONTINUE
  100 CONTINUE

      RETURN
      END
