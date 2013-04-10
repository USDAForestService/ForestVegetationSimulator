      SUBROUTINE FMCFIM (IYR, FMD, UWIND, IBYRAM, FLAMEHT, CANBURN, ROS)
      IMPLICIT NONE
C
C  $Id$
C
C----------
C  **FMCFIM  FIRE-BC
C----------
C
C     CALLED FROM: FMBURN
C                  FMPOFL
C     CALLS cfim.cpp
C
C  PURPOSE:
C     THIS SUBROUTINE IS THE INTERFACE BETWEEN FFE AND THE CFIM FIRE
C     CALCULATIONS
C
C  CALL LIST DEFINITIONS:
C     IYR:  CURRENT YEAR
C     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
C
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C
      integer iyr, RC
      integer FMD
      integer CFIM_DRIVER
      character VVER*7
      real CFIM_OUTPUT(10)
      REAL FMINFO(13), LHV
      logical debug
      integer MYACTS(1)
      integer I,J,jrout, K, J1, L
      REAL MWIND, KTEMP, UWIND
      REAL IBYRAM, FLAMEHT, ROS
      REAL PRMS(10)
      INTEGER CANBURN, INB
      REAL ACTWFC, ACTFFC, FFC, WFC, SURFUEL

#ifdef _WINDLL
      !DEC$ ATTRIBUTES DLLIMPORT :: CFIM_DRIVER
#endif
      !DEC$ ATTRIBUTES ALIAS : '_CFIM_DRIVER' :: CFIM_DRIVER
      
C     check for debug

      call dbchk (debug,'FMCFIM',6,icyc)
      if (debug) write(jostnd,7) icyc, iyr
    7 format(' FMCFIM cycle=',i2,' iyr=',i5)

C     ALL CFIM_INPUT VARIABLES THAT ARE NOT SET HERE
C     ARE SET BY KEYWORD IN FMIN

C     CONSTANTS
C     ignition_temperature(K)---igtemp				600.0
      CFIM_INPUT(6) = 600.0

C     heat_content(J/kg)---hc						18600.0
      CFIM_INPUT(18) = 18600.0

C     DEPENDS ON STAND CONDITION OR USER-DEFINED FIRE VARIABLES
C     10m_wind(m/s)---u10   (test=7)   
      MWIND = UWIND * MItoKM * 1000. / (60. * 60.)
      CFIM_INPUT(1) = MWIND
      
C     slope - percent (test=0)      
      CFIM_INPUT(2) = FMSLOP

C     ambient_temperature(K)---Ta (test = 300)
      KTEMP = ATEMP*FtoC1+FtoC2 + 273.15
      CFIM_INPUT(3) = KTEMP

C     stand_height(m)---sh							13.0
      CFIM_INPUT(4) = AVH * FTtoM

C     wind_attenuation_coefficient---alpha			1.2
C     'HOW TO CALCULATE??
c       CURRENTLY SET TO 1, BECAUSE USING A WIND FACTOR THAT HAS ALREADY BEEN REDUCED
      CFIM_INPUT(5) = 1

C     SURFACE_INPUTS
C     fuel_model_number---FuelModelNumber					2
      CFIM_INPUT(10) = FMD

C     1_hour_fuel_moisture(fraction)---FuelMoisture[0]			0.05
C     10_hour_fuel_moisture(fraction)---FuelMoisture[1]			0.06
C     100_hour_fuel_moisture(fraction)---FuelMoisture[2]	   	0.07
C     live_herbaceous(fraction)---FuelMoisture[3]				0.3
C     live_woody(fraction)---FuelMoisture[4]					1.0
      I = 10
      DO J = 1, 3
          I = I + 1
          CFIM_INPUT(I) = MOIS(1,J)
      ENDDO
      CFIM_INPUT(14) = MOIS(2,1)
      CFIM_INPUT(15) = MOIS(2,2)

C     canopy_base_height(m)---canbaseht					5.0
      CFIM_INPUT(20) = ACTCBH * FTtoM

C     foliar_moisture_content---FMC						1.0
C       NOTE THAT FOLMC IS ON SCALE 0-100, AND WE NEED 0-1
      CFIM_INPUT(22) = FOLMC / 100.0

C     canopy_fuel_density(kg/m^3)---rho_can				398.0
C     DO NOT USE CROWN BULK DENSITY (already in kg/m3)
C       CBD IS NOT THE SAME AS CANOPY FUEL DENSITY
C       FOR NOW, KEEP CANOPY FUEL DENSITY AS A CONSTANT 
      CFIM_INPUT(24) = 398.0


c       PREDICTED CONSUMPTION OF DUFF, LITTER
C       THIS IS NOW DONE IN FMCONS, INCLUDING LOADING THE CFIM_INPUT ARRAY
C      CFIM_INPUT(25) = FFC + WFC
      
      FLAMEHT = 0.0
      IBYRAM = 0.0
      ROS = 0.0

C     The CFIM model does not have access to FM common blocks.
C     Therefore, load Fuel model information into an array to pass
C     to it.
      DO 800 INB = 1,MXFMOD
          IF (FMOD(INB).EQ.0 .OR. FWT(INB) .LE. 0.0) GOTO 800

C       LOAD UP THE FUEL MODEL VARIABLES 
C        FOR NOW, JUST USE THE MAIN FUEL MODEL - WE WILL NEED TO CHANGE THIS LATER
C        CALL FMGFMV(IYR, FMD)
          CALL FMGFMV(IYR, FMOD(INB))

c       FUEL LOADING (convert from LB/FT2 to T/A)
          FMINFO(1) = FWG(1,1) * 43560./2000.
          FMINFO(2) = FWG(1,2) * 43560./2000.
          FMINFO(3) = FWG(1,3) * 43560./2000.
          FMINFO(4) = FWG(1,4) * 43560./2000.
          FMINFO(5) = FWG(2,1) * 43560./2000.

c       SAV RATIO (1/FT)
          FMINFO(6) = MPS(1,1)
          FMINFO(7) = MPS(2,1)
          FMINFO(8) = MPS(2,2)

C       HEAT CONTENT (LOGIC COPIED FROM FMFINT)
            IF (FMD .EQ. 106) THEN
              LHV = 9000.0
            ELSEIF (IFLOGIC .EQ. 2) THEN
              LHV = ULHV
            ELSE
              LHV = 8000.0
            ENDIF
          FMINFO(9) = LHV
          FMINFO(10) = LHV
          FMINFO(11) = LHV

C       DEPTH AND MOIS OF EXTINCTION
          FMINFO(12) = DEPTH
          FMINFO(13) = MEXT(1)

        RC = 0
        RC = CFIM_DRIVER(CFIM_INPUT,CFIM_OUTPUT,FMINFO)

C	    CFIM_output[0] = maxparttemp;
C	    CFIM_output[1] = flag;0.

C	    CFIM_output[2] = iByram;
C	    CFIM_output[3] = taur;
C	    CFIM_output[4] = ROS;
C	    CFIM_output[5] = flamedepth;
C	    CFIM_output[6] = flameheight;
C	    CFIM_output[7] = FlameLength;

C       CHECK UNITS. IS THIS BTU\M\MIN?
C          IBYRAM = CFIM_OUTPUT(3) / MtoFT
C          FLAMEHT = CFIM_OUTPUT(7) * MtoFT
C       FLAG > 0 MEANS THAT THE CANOPY INGITED
C         DON'T USE WEIGHTING FOR THIS ONE - IF ANY FUEL
C         MODEL INGITES THE CANOPY, THEN SET THAT.
          IF (CANBURN .EQ. 0) CANBURN = CFIM_OUTPUT(2)

          FLAMEHT = FLAMEHT + CFIM_OUTPUT(7) * FWT(INB)
          IBYRAM = IBYRAM + CFIM_OUTPUT(3) * FWT(INB)
          ROS = ROS + CFIM_OUTPUT(5) * FWT(INB)
 
 800   CONTINUE
C       CHECK UNITS. IS THIS BTU\M\MIN?
          IBYRAM = IBYRAM / MtoFT
          FLAMEHT = FLAMEHT * MtoFT
C       CHECK UNITS. ROS IS IN M/S FROM CFIM. CHANGE TO FT/MIN (MtoFT / (1/60) = .00508)
          ROS = ROS / 0.00508       

      return
      end
