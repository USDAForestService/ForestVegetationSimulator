      SUBROUTINE BRUPDT
C**********************************************************************
C  **BRUPDT       DATE OF LAST REVISION:  05/09/2000
C----------------------------------------------------------------------
C  Purpose:
C  Updates tree ground diameter and converts it to centimeters
C  for each cycle in the Blister Rust model.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  15-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 45) and species temp index variable (I3)
C     are new.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C
C**********************************************************************

C.... Common include files

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'

C.... Local variables.
      INTEGER I1, I2, I3, J, K

C.... If no tree records then return.

      IF(ITRN.EQ.0) GO TO 50

C.... Process the host pines, if none then return.
C.... Start species loop.

      DO 45 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 45

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 45
      I2=ISCT(I3,2)
      DO 22 J=I1,I2
         K=IND1(J)

C....    Update the ground diameter of this tree.

         BRGD(K)=BRGD(K)+DG(K)*2.54
   22 CONTINUE

C.... End species loop.
   45 CONTINUE

C.... Common return.

   50 CONTINUE
      RETURN
      END
