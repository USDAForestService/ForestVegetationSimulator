      SUBROUTINE FMCBIO (D, KSP, ABIO, MBIO, RBIO)
      IMPLICIT NONE
C----------
C  **FMCBIO--FIRE  DATE OF LAST REVISION:  09/26/06
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMSCUT
*                  FMCRBOUT
*  PURPOSE:
*     CALCULATE THE JENKINS BIOMASS EQUATIONS FOR ABOVEGROUND BIOMASS,
*     MERCHANTABLE BIOMASS, AND BELOWGROUND BIOMASS
***********************************************************************
*
*  CALL LIST DEFINITIONS:
*     D:    DBH (IN)
*     JSP:  SPECIES CODE
*     ABIO: (RETURNED) ABOVEGROUND BIOMASS (KG)
*     MBIO: (RETURNED) MERCHANTABLE
*     RBIO: (RETURNED) ROOT BIOMASS
*
***********************************************************************

C     PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     COMMON INCLUDE FILES

      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMPROP.F77'
      INCLUDE 'METRIC.F77'

C     VARIABLE DECLARATIONS

      LOGICAL   DEBUG
      REAL      ABIO, MBIO, RBIO, D, DCM, KGtoTI
      INTEGER   KSP, IGRP, JGRP
      REAL      B0A(10), B1A(10), B0M(2), B1M(2), B0B(2), B1B(2)

C     CEDAR/LARCH, DF, HEMLOCK, PINE, SPRUCE
C     ASPEN, MAPLE/BIRCH, MIXED, HARD MAPLE
C     WOODLAND JUNIPER.OAK, MESQUITE

      DATA B0A/
     &-2.0336, -2.2304, -2.5384, -2.5356, -2.0773,
     &-2.2094, -1.9123, -2.4800, -2.0127, -0.7152/
      DATA B1A/
     & 2.2592,  2.4435,  2.4814,  2.4349,  2.3323,
     & 2.3867,  2.3651,  2.4835,  2.4342,  1.7029/
      DATA B0M/ -0.3737, -0.3065/
      DATA B1M/ -1.8055, -5.4240/
      DATA B0B/ -1.5619, -1.6911/
      DATA B1B/  0.6614,  0.8160/


C     CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'FMCBIO',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,1) ICYC
    1 FORMAT(' ENTERING FMCBIO CYCLE = ',I2)

      IGRP = BIOGRP(KSP)
      JGRP = 1
      IF (IGRP .GT. 5) JGRP = 2

      DCM    = D * INtoCM
      KGtoTI = TMtoTI / 1000.

      ABIO = 0.0
      MBIO = 0.0
      RBIO = 0.0

C  1. ABOVEGROUND BIOMASS (ABM)
C  2. MERCH BIOMASS  (MBM) ("Stem wood" in Jenkin's paper -> defined as a proportion)
C                          (Note that merch is defined from a 12in stump height, so we
C                           will define it as being for trees above the merch limit.)
C  3. BELOWGROUND LIVE (BBM) ("Coarse roots" in Jenkin's paper -> defined as a proportion)

C  CALCULATE JENKINS EQUATIONS OF BIOMASS (GE 2.5 CM)

      IF (DCM .GT. 0.0) THEN
        IF (DCM .GE. 2.5) THEN
          ABIO = EXP(B0A(IGRP) + B1A(IGRP)*LOG(DCM))
        ELSE
C         IF TREE IS VERY SMALL, THEN CALCULATE THE VALUE AT 2.5CM, AND USE
C         ONLY A PROPORTION OF IT.
          ABIO = EXP(B0A(IGRP) + B1A(IGRP)*LOG(2.5))
          ABIO = ABIO * (DCM / 2.5)
        ENDIF

        RBIO = ABIO * EXP(B0B(JGRP) + (B1B(JGRP) / DCM))

        IF (D .GE. DBHMIN(KSP))
     &     MBIO = ABIO * EXP(B0M(JGRP) + (B1M(JGRP) / DCM))

        ABIO = ABIO * KGtoTI
        MBIO = MBIO * KGtoTI
        RBIO = RBIO * KGtoTI
      ENDIF

      RETURN
      END
