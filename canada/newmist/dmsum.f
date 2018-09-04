      SUBROUTINE DMSUM(DMTRCW, IDMSHP)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMSUM --     DATE OF LAST REVISION:  02/23/96   
C This code was adapted from the CVSUM subroutine of the COVER model.
C----------
C  Purpose:
C    This routine computes the crown radius and volume (in MESH
C units) for each treelist record. Further documentation can be found
C in: Moeur, Melinda. 1981. Crown width and foliage weight of
C northern Rocky Mountain Confifers. USDA Forest Service Res. Pap.
C INT-283.
C
C
C Called by:
C
C     DMMTRX
C
C Other routines called:
C
C     DBCHK
C
C Argument list definitions:                        
C
C     REAL    DMTRCW  (I) Predicted maximum crown width (feet).
C     INTEGER IDMSHP  (I) Tree crown shape category.
C
C Local variable definitions (not complete):
C
C     REAL    HC          HEIGHT OF CROWN (CROWN LENGTH).
C     REAL    BASE        HEIGHT AT CROWN BASE (BOTTOM IF SHAPE =
C                          CONE, PARABOLOID OR NEILOID; MID-CROWN
C                          IF SHAPE = SPHERE OR ELLIPSOID).
C     REAL    RAD         CROWN RADIUS AT BASE.
C     REAL    BOT         HEIGHT AT CROWN BOTTOM.
C     INTEGER ITOP        INDEX TO HT CLASS CONTAINING CROWN TOP.
C     INTEGER IBOT        INDEX TO HT CLASS CONTAINING CROWN BOTTOM.
C     REAL    H1          DISTANCE FROM BASE TO LOWER PLANE OF
C                          FRUSTRUM.
C     REAL    H2          THICKNESS OF FRUSTRUM.
C     REAL    R1          RADIUS OF LOWER PLANE OF FRUSTRUM.
C     REAL    R2          RADIUS OF UPPER PLANE OF FRUSTRUM.
C     REAL    Y1          LOWER LIMIT OF INTEGRATION.
C     REAL    Y2          UPPER LIMIT OF INTEGRATION.
C     REAL    CNOP(1)     HEIGHT AT LOWER LIMIT OF CROWN CLASS.
C     REAL    CNOP(2)     HEIGHT AT UPPER LIMIT OF CROWN CLASS.
C     REAL    UPLIM       HEIGHT AT UPPER PLANE OF CROWN FRUSTRUM.
C     REAL    LOWLIM      HEIGHT AT LOWER PLANE OF CROWN FRUSTRUM.
C     REAL    PAREA       PROFILE AREA OF FRUSTRUM.
C
C     REAL    MSCL        Height increment (MESH scale).
C     INTEGER DMSHAP      DM shape category
C
C Common block variables and parameters:
C
C     DMRDMX  DMCOM                                 
C     FPM     DMCOM
C     MESH    DMCOM                       
C     MXHT    DMCOM
C     PIE     DMCOM
C
C     [FVS commons are not documented]
C
C********************************************************************
      
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'DMCOM.F77'                                       

C Argument list variables.

      REAL    DMTRCW      
      INTEGER IDMSHP

      DIMENSION DMTRCW(MAXTRE)
      DIMENSION IDMSHP(MAXTRE)      

C Local variables.

      LOGICAL DEBUG
      INTEGER DMSHAP, I, J, IBOT, ITOP, IICR, J1, K
      REAL    LOWLIM, MSCL, BOT, HC, BASE, H1, H2, R1, R2
      REAL    RAD, CNOP, Y1, Y2, B1, Z1, Z2, CONST, FRUST, PAREA
      REAL    UPLIM, RAD1, RAD2

      DIMENSION CNOP(2)

C The original model seems to work in units of feet. The unit we want
C to use here is MESH (normally 2 meters). 'MSCL' performs the scale
C conversion. Multiplying by 'MSCL' gives the value in feet; dividing
C by 'MSCL' gives a value in MESH.

      MSCL = FPM * MESH
C
C     CHECK FOR DEBUG.
C
      CALL DBCHK(DEBUG, 'DMSUM', 4, ICYC)
C
C     RETURN IF NOTREES OPTION IN EFFECT.
C
      IF (ITRN .GT. 0) GO TO 60
      IF (DEBUG) WRITE (JOSTND, 9001) ITRN
 9001 FORMAT (' ITRN =', I5,' : NOTREES : RETURN TO **DMMTRX**')
      RETURN
   60 CONTINUE

      IF (DEBUG) WRITE (JOSTND, 9010)
 9010 FORMAT ('      I  J     FRUST    ',
     &'TREVOL   CVOLUM    PAREA    TREAR    CAREA    BASE   UPLIM',
     &'  LOWLIM      H1      H2      Y1      Y1')

      do I=1,ITRN
	  do J=1,MXHT
          DMRDMX(I, J, RADIUS) = 0.
          DMRDMX(I, J, VOLUME) = 0.
	  enddo
	enddo
C
C     ENTER TREE LOOP.
C
      DO 300 I = 1, ITRN
C
C     COMPUTE HEIGHT CLASS (MESH BAND) OF TOP OF TREE.
C
      ITOP = IFIX(HT(I)/(MSCL+0.0001)) + 1
      IF (ITOP .GT. MXHT) ITOP = MXHT
C
C     COMPUTE HEIGHT CLASS (MESH BAND) OF CROWN BOTTOM.
C
      BOT = HT(I) - (HT(I)*ICR(I)/100.)
      IBOT = IFIX (BOT/MSCL) + 1
C
C     COMPUTE DIMENSIONS USED IN HEIGHT CLASS CALCULATIONS.
C
      DMSHAP = IDMSHP(I)
      IICR = ICR(I)
      HC = FLOAT(IICR) * HT(I) / 100.
      BASE = BOT
      IF ((DMSHAP.EQ.1) .OR. (DMSHAP.EQ.5)) BASE = BOT + HC / 2.
      RAD = DMTRCW(I) / 2.
C
C  SET INITIAL CANOPY HEIGHTS FOR CURRENT TREE.
C
      CNOP(1) = MSCL * IBOT - (2. * MSCL)
      CNOP(2) = CNOP(1) + MSCL
C
C  ENTER HEIGHT CLASS LOOP WITHIN CURRENT TREE.  J1 IS USED TO
C  DECREMENT THE INDEX BY HEIGHT CLASS IF THE SHAPE IS NEILOID
C  (I.E. DO IT UPSIDE-DOWN).
C
      J1 = ITOP + 1
      DO 290 J = IBOT, ITOP

      IF (DMSHAP .NE. 3) J1 = J
      IF (DMSHAP .EQ. 3) J1 = J1 - 1
      DO 230 K = 1, 2
      CNOP(K) = CNOP(K) + MSCL
  230 CONTINUE
      UPLIM = AMIN1(HT(I), CNOP(2))
      IF (J .EQ. ITOP) UPLIM = HT(I)
      LOWLIM = AMAX1(BOT, CNOP(1))
      H1 = LOWLIM - BASE
      H2 = UPLIM - LOWLIM
      R1 = (1. - H1 / HC) * RAD
      R2 = (1. - (H1 + H2) / HC) * RAD
      Y1 = LOWLIM - BASE
      Y2 = UPLIM - BASE
      IF (Y1 .GE. HC) Y1 = HC
      IF (Y2 .GE. HC) Y2 = HC
      IF (Y1 .LT. -HC) Y1 = -HC
      IF (Y2 .LT. -HC) Y2 = -HC
C
C  BRANCH ON SHAPE TO COMPUTE FRUSTRUM AND RADIUS
C
      GO TO (241, 242, 243, 244, 245), DMSHAP
C
C  DMSHAPE=1   SOLID FORM=SPHERE    PLANE FORM=CIRCLE
C
  241 CONTINUE
      IF (BASE.GE.LOWLIM) H1 = BASE - UPLIM
      B1 = HC / 2.
      IF (Y1 .GT. B1) Y1 = B1
      IF (Y1 .LT. -B1) Y1 = -B1
      IF (Y2 .GT. B1) Y2 = B1
      IF (Y2 .LT. -B1) Y2 = -B1
	Z1=B1*B1-Y1*Y1
	Z2=B1*B1-Y2*Y2
	IF(Z1.LT.0.0) Z1=0.0
      IF(Z2.LT.0.0) Z2=0.0
      CONST = 1.04720 * H2 * RAD * RAD / (HC * HC)
      FRUST = CONST* (3 * HC * HC - 12*H1*H1 - 12*H1*H2 - 4*H2*H2)
      PAREA = RAD/B1*(Y2*SQRT(Z2)+B1**2*ASIN(Y2/B1))-
     &        RAD/B1*(Y1*SQRT(Z1)+B1**2*ASIN(Y1/B1))
      GO TO 280
C
C  DMSHAPE=2   SOLID FORM=CONE   PLANE FORM=TRIANGLE
C
  242 CONTINUE
      CONST = 1.04720*H2
      FRUST = CONST*(R1*R1+R2*R2+R1*R2)
      PAREA = (R1+R2)*H2
      GO TO 280
C
C  DMSHAPE=3   SOLID FORM=NEILOID   PLANE FORM=NEILOID
C
  243 CONTINUE
      FRUST = (PIE*RAD*RAD*H2) -
     &        (HLFPIE*H2*RAD**2/HC)*(2*HC-2*H1-H2)
      PAREA = RAD*((Y2+.66667*HC*(1-Y2/HC)**1.5) -
     &             (Y1+.66667*HC*(1-Y1/HC)**1.5))
      GO TO 280
C
C  DMSHAPE=4   SOLID FORM=PARABOLOID   PLANE FORM=PARABOLA
C
  244 CONTINUE
      FRUST = (HLFPIE*H2*RAD**2/HC)*(2*HC-2*H1-H2)
      PAREA = -RAD*1.33333*HC*((1-(Y2/HC))**1.5 - (1-(Y1/HC))**1.5)
      GO TO 280
C
C  DMSHAPE=5   SOLID FORM=ELLIPSOID   PLANE FORM=ELLIPSE
C
  245 CONTINUE
      IF (BASE.GE.LOWLIM) H1 = BASE - UPLIM
      B1 = HC/2.
      IF (Y1 .GT. B1) Y1 = B1
      IF (Y1 .LT. -B1) Y1 = -B1
      IF (Y2 .GT. B1) Y2 = B1
      IF (Y2 .LT. -B1) Y2 = -B1
	Z1=B1*B1-Y1*Y1
	Z2=B1*B1-Y2*Y2
	IF(Z1.LT.0.0) Z1=0.0
	IF(Z2.LT.0.0) Z2=0.0
      CONST = 1.04720*H2*RAD*RAD/(HC*HC)
      FRUST = CONST*(3*HC*HC - 12*H1*H1 - 12*H1*H2 - 4*H2*H2)
      PAREA = RAD/B1*(Y2*SQRT(Z2)+B1**2*ASIN(Y2/B1))-
     &        RAD/B1*(Y1*SQRT(Z1)+B1**2*ASIN(Y1/B1))
  280 CONTINUE
 
C  Fill the RADIUS and VOLUME parts of 'DMRDMX()'.
C   J - is Height Class
C   I - is a Tree Record
C  RADIUS and VOLUME measures are in MESH units.

      RAD1 = PAREA / (H2 * 2)
      RAD2 = SQRT(FRUST / (PIE * H2))
      
c     if (j .gt. 4) then
c	  jjj=0
c	endif

      DMRDMX(I, J, RADIUS) = RAD2 / MSCL
      DMRDMX(I, J, VOLUME) = FRUST / MSCL**3
C
C  DEBUG OUTPUT
C
C      WRITE(*, 4000) I, IDMSHP(I), RAD1, RAD2
C 4000 FORMAT (I4, I4, 2F10.1)

      IF (DEBUG) WRITE (JOSTND, 7000) I, J1, UPLIM, LOWLIM, H1, H2,
     & BASE
 7000 FORMAT (I6, I3, F7.2, 5F10.2, 7F8.2)
C
C  END LAYER LOOP
C
  290 CONTINUE
C
C  END TREE LOOP
C
  300 CONTINUE
      RETURN
      END
