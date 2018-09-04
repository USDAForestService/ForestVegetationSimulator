      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C
C FUNCTION TO COMPUTE BARK RATIOS. THIS ROUTINE IS VARIANT SPECIFIC
C AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'METRIC.F77'
C
      INTEGER IS
      REAL H,D,BRATIO
      REAL D_CM,H_M,HDRAT

      IF(IS .EQ. 0) THEN
        BRATIO=0.93
      ELSE
        BRATIO=BKRAT(IS)
      ENDIF

C     CORRECT FOR ONTARIO INFORMATION.
C        THE VALUES USED HERE ARE FROM Zakrzewski AS PART OF HIS
C        VOLUME EQUATIONS. 
C     
C     NOTE THAT THE CONSTANT VALUES SHOULD JUST REPLACE THE VALUES 
C     IN BKRAT, RATHER THAN BEING DONE HERE.

C     REPLACE BRATIO VALUES WITH CONSTANT VALUES, IF APPLICABLE

	IF (IS .EQ. 3 .OR. IS .EQ. 4) THEN
C       (RED PINE)
	  BRATIO = 0.92608

	ELSE IF (IS .EQ. 1 .OR. IS .EQ. 69 .OR. IS .EQ. 10 .OR.
     &         IS .EQ. 13) THEN
C       (JACK PINE NAT OR PLANT, TAMARAK OR "OTHER SOFTWOODS")
	  BRATIO = 0.9609

	ELSE IF (IS .EQ. 6 .OR. IS .EQ. 71 .OR. IS .EQ. 7) THEN
C       (WHITE SPRUCE OR NORWAY SPRUCE)
	  BRATIO = 0.955

	ELSE IF (IS .EQ. 8) THEN
C       (BALSAM FIR)
	  BRATIO = 0.9456

	ELSE IF (IS .EQ. 24 .OR. IS .EQ. 43) THEN
C       (BIRCH)
	  BRATIO = 0.9451

	ELSE IF (IS .EQ. 15 .OR. IS .EQ. 16 .OR. IS .EQ. 17 .OR.
     &         IS .EQ. 40 .OR. IS .EQ. 41 .OR. IS .EQ. 42) THEN
C       (ASPEN, ALSO USED FOR ASH & COTTONWOOD (Ontario sp. 45, 70-74))
	  BRATIO = 0.9365

	END IF

C     IF, FOR SOME STRANGE REASON THE DIAMETER IS ZERO, JUST USE THE 
C     VALUES FROM BEFORE.
      IF (D .EQ. 0) RETURN

C     THESE ARE THE EQUATIONS FROM ONTARIO, USED FOR SOME SPECIES.
C     THEY ALL RELY ON A NON-ZERO DIAMETER.
C     DO THE CALCULATIONS IN METRIC.
C     Note that several of these equations could easily go negative
C     if the ratio between height and diameter is too large. If this
C     happens, recalculate based on a smaller ratio.

      D_CM = D * INtoCM
	H_M  = H * FTtoM
	HDRAT = H_M / D_CM

      IF (IS .EQ. 5 .OR. IS .EQ. 70) THEN
C       (WHITE PINE)      
	  BRATIO = 1.0 - 0.04513 * D_CM ** (1.168567 - 1.0)

      ELSE IF (IS .EQ. 9 .OR. IS .EQ. 72) THEN
C       (BLACK SPRUCE)      
C        Note: equation goes negative at HDRAT>14.8679)
        IF (HDRAT .LT. 14.8) THEN
	    BRATIO = 1.0 - 0.067259 * HDRAT
	  ELSE
          BRATIO = 1.0 - 0.067259 * 14.0
	  END IF
      ELSE IF (IS .EQ. 11 .OR. IS .EQ. 14) THEN
C       (WHITE CEDAR, ALSO USED FOR NORTHERN RED CEDAR)      
C        Note: equation goes negative at HDRAT>7.31797)
        IF (HDRAT .LT. 7.3) THEN
	    BRATIO = 1.0 - 0.13665 * HDRAT
	  ELSE
	    BRATIO = 1.0 - 0.13665 * 7.0
        END IF

      ELSE IF (IS .EQ. 18 .OR. IS .EQ. 19 .OR. IS .EQ. 26 .OR. 
     &         IS .EQ. 27 .OR. IS .EQ. 44 .OR. IS .EQ. 46 .OR. 
     &         IS .EQ. 48 .OR. IS .EQ. 49 .OR. IS .EQ. 50 .OR. 
     &         IS .EQ. 51 .OR. IS .EQ. 52 .OR. IS .EQ. 54 .OR. 
     &         IS .EQ. 57 .OR. IS .EQ. 59 .OR. IS .EQ. 64 .OR. 
     &         IS .EQ. 65 .OR. IS .EQ. 66 .OR. IS .EQ. 67) THEN
C       SUGAR/RED MAPLE (ONT SP: 30,32)      
C           Note: equation goes negative at HDRAT>8.954)
        IF (HDRAT .LT. 8.9) THEN
	    BRATIO = 1.0 - 0.111681 * HDRAT
	  ELSE
          BRATIO = 1.0 - 0.111681 * 8.0
	  END IF

      END IF

      RETURN
      END
