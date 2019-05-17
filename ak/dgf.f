      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DGPRED (CHANGE IN
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
C  ARGUMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
C  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
C  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
C  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
C  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.
C
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
C----------
C  VARIABLE DEFINITIONS:
C---------
C
C   DIAM   -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
C             ARGUEMENT).

C   DGLD   -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
C             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
C             SPECIES).
C   DGLNCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LOG(CROWN
C             RATIO) TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
C   DGLNSI -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LOG(SITE
C             INDEX) TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
C             EACH SPECIES).
C   DGSASP -- ARRAYT CONTAINING COEFFICIENT FOR SLOPE*COS(ASPECT) TERM
C   DGSLOP -- ARRAYT CONTAINING COEFFICIENT FOR SLOPE PERCENT TERM
C   DGDSQ  -- ARRAY CONTAINING THE COEFFICIENTS FOR THE DIAMETER
C             SQUARED TERM IN THE DDS MODEL (ONE
C             COEFFICIENT FOR EACH SPECIES).
C   DGBAL  -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
C             LARGER TREES TERM IN THE DDS MODEL
C             (ONE COEFFICIENT FOR EACH SPECIES).
C   DGRD   -- ARRAY CONTAINING COEFFICIENTS FOR RELATIVE DENSITY
C             (PLOT ZEIDE SDI / PLOT MAX SDI – DECIMAL)
C   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
C             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
C             COEFFICIENT PER SPECIES).
C   DGEL   -- CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
C             DIAMETER GROWTH EQUATION.
C----------
C SPECIES LIST FOR ALASKA VARIANT.
C
C Number Code  Common Name         FIA  PLANTS Scientific Name
C   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4     TA   tamarack            071  LALA   Larix laricina
C   5     WS   white spruce        094  PIGL   Picea glauca
C   6     LS   Lutz’s spruce            PILU   Picea lutzii
C   7     BE   black spruce        095  PIMA   Picea mariana
C   8     SS   Sitka spruce        098  PISI   Picea sitchensis
C   9     LP   lodgepole pine      108  PICO   Pinus contorta
C  10     RC   western redcedar    242  THPL   Thuja plicata
C  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     OS   other softwoods     298  2TE
C  14     AD   alder species       350  ALNUS  Alnus species
C  15     RA   red alder           351  ALRU2  Alnus rubra
C  16     PB   paper birch         375  BEPA   Betula papyrifera
C  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     WI   willow species      920  SALIX  Salix species
C  22     SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     OH   other hardwoods     998  2TD
C----------
C  VARIABLE DECLARATIONS:
C----------
      LOGICAL DEBUG

      INTEGER I,I1,I2,I3,IASP,INDXS,ISPC

      INTEGER MAPDSQ(7,MAXSP),MAPLOC(7,MAXSP),SMMAPS(MAXSP)

      REAL ALD,BAL,BARK,BRATIO,CONSPP,CONST,CR,D,DDS,DDSL,DDSS
      REAL DGBALS,DGCRS,DGCRS2,DGDBLS,DGDSQS,DGLDS,DIAGR
      REAL H,RELHT,SMCONS,SSITE,TEMEL,XWT

      REAL DGBA(MAXSP),DGBAL(MAXSP),DGCASP(MAXSP),DGCR(MAXSP)
      REAL DGCRSQ(MAXSP),DGDBAL(MAXSP),DGDS(4,MAXSP),DGEL(MAXSP)
      REAL DGEL2(MAXSP),DGFOR(6,MAXSP),DGHAH(MAXSP),DGLD(MAXSP)
      REAL DGLNBA(MAXSP),DGSASP(MAXSP),DGSITE(MAXSP)
      REAL DGSLOP(MAXSP),DGSLSQ(MAXSP)
      REAL DIAM(MAXTRE),OBSERV(MAXSP),SMCASP(3),SMCR(3),SMCR2(3)
      REAL SMDBAL(3),SMDS(3),SMEL(3),SMEL2(3),SMFOR(4,3)
      REAL SMLBA(3),SMLD(3),SMSASP(3),SMSITE(3),SMSL(3),SMSL2(3)

C----------
C  DATA STATEMENTS:
C----------
C *** BEGIN DIAMETER GROWTH EQUATION COEFFICIENTS ***
C DESCRIBED IN DOCUMENTATION AS
C INTERCEPT -- b1
      DATA DGCON /
    & -8.571521, -8.571521, -9.463168, -6.472259, -6.305809,
    & -6.305809, -6.472259, -8.571521, -9.301545, -9.415728, 
    & -9.432608, -9.000219, -6.305809, -5.054325, -5.054325, 
    & -6.155523, -6.155523, -5.762360, -6.848837, -5.762360, 
    & -5.762360, -5.762360, -5.762360 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C DBH^2 -- b2
      DATA DGDSQ /
    & -0.000436, -0.000436, -0.000436, -0.003125, -0.003125, 
    & -0.003125, -0.003125, -0.000436, -0.000436, -0.000436, 
    & -9.432608, -0.000436, -0.003125, -0.003125, -0.003125, 
    & -0.003125, -0.003125, -0.003125, -0.003125, -0.003125, 
    & -0.003125, -0.003125, -0.003125 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C ln(DBH) -- b3
      DATA DGLD /
    &  0.044114,  0.044114,  0.073453,  0.162477,  0.560619, 
    &  0.560619,  0.162477,  0.044114,  0.011996,  0.269429, 
    & -0.000436,  0.006253,  0.560619,  0.435894,  0.435894, 
    &  0.476613,  0.476613,  0.340293,  0.824344,  0.340293, 
    &  0.340293,  0.340293,  0.340293 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C BAL -- b4
      DATA DGDBAL /
    & -0.001899, -0.001899, -0.000430,  0.445278, -0.000366, 
    & -0.000366,  0.445278, -0.001899, -0.000430, -0.000430, 
    & -0.000402, -0.000713, -0.000366, -0.001244, -0.001244, 
    & -0.006573, -0.006573, -0.000472, -0.000407, -0.000472, 
    & -0.000472, -0.000472, -0.000472 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C RD -- b5
      DATA DGRD /
    &  0.0,       0.0,       0.0,      -0.001738, -0.394170,
    & -0.394170, -0.001738,  0.0,       0.0,       0.0, 
    &  0.0,       0.0,      -0.394170, -0.394507, -0.394507, 
    & -0.001738, -0.001738, -0.738629, -0.489654, -0.738629, 
    & -0.738629, -0.738629, -0.738629 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C ln(CR) -- b6
      DATA DGLNCR /
    & 0.826432, 0.826432, 0.826432, 0.703204, 0.703204, 
    & 0.703204, 0.703204, 0.826432, 0.826432, 0.826432, 
    & 0.826432, 0.826432, 0.703204, 0.703204, 0.703204, 
    & 0.703204, 0.703204, 0.703204, 0.703204, 0.703204, 
    & 0.703204, 0.703204, 0.703204 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C ELEV-- b7
      DATA DGEL /
    &  0.0,       0.0,       0.0,      -0.000181, -0.000181, 
    & -0.000181, -0.000181,  0.0,       0.0,       0.0,      
    &  0.0,       0.0,      -0.000181, -0.000181, -0.000181, 
    & -0.000181, -0.000181, -0.000181, -0.000181, -0.000181, 
    & -0.000181, -0.000181, -0.000181 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C SLOPE -- b8
      DATA DGSLOP /
    & 0.001180, 0.001180, 0.001180, 0.001562, 0.001562, 
    & 0.001562, 0.001562, 0.001180, 0.001180, 0.001180, 
    & 0.001180, 0.001180, 0.001562, 0.001562, 0.001562, 
    & 0.001562, 0.001562, 0.001562, 0.001562, 0.001562, 
    & 0.001562, 0.001562, 0.001562 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C SLOPE*cos(ASPECT) -- b9
      DATA DGSASP /
    & -0.000289, -0.000289, -0.000289, -0.002480, -0.002480, 
    & -0.002480, -0.002480, -0.000289, -0.000289, -0.000289, 
    & -0.000289, -0.000289, -0.002480, -0.002480, -0.002480, 
    & -0.002480, -0.002480, -0.002480, -0.002480, -0.002480, 
    & -0.002480, -0.002480, -0.002480 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C ln(SI) -- b10
      DATA DGLNSI /
    & 0.698202, 0.698202, 0.698202, 0.0,      0.0,      
    & 0.0,      0.0,      0.698202, 0.698202, 0.698202, 
    & 0.698202, 0.698202, 0.0,      0.0,      0.0,      
    & 0.0,      0.0,      0.0,      0.0,      0.0,      
    & 0.0,      0.0,      0.0 /

CC *** END DIAMETER GROWTH EQUATION COEFFICIENTS ***
C----------
C *** BEGIN PERMAFROST DIAMETER GROWTH MODIFIER COEFFICIENTS ***
C DESCRIBED IN DOCUMENTATION AS PERMAFROST PRESENCE/ABSENCE FACTOR
C IF PRESENT VALUE IS INCLUDED. IF ABSENT VALUE IS EXCLUDED.
C INTERCEPT -- b01
      DATA PMPRES / -0.355666 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFICIENT FOR
C INTERCEPT -- b1
      DATA PMCON /
    &       0.0,       0.0,       0.0,       0.0, -6.233612,
    &       0.0, -6.389518,       0.0,       0.0,       0.0,
    &       0.0,       0.0,       0.0,       0.0,       0.0,
    & -6.204504,       0.0,       0.0, -6.979727, -5.428688,
    &       0.0,       0.0,       0.0 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C DBH^2 -- b2
      DATA PMDSQ / -0.003665 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C ln(DBH) -- b3
      DATA PMLD /
    &       0.0,       0.0,       0.0,       0.0,  0.563513,
    &       0.0,  0.176255,       0.0,       0.0,       0.0,
    &       0.0,       0.0,       0.0,       0.0,       0.0,
    &  0.503683,       0.0,       0.0,  0.879614,  0.103326,
    &       0.0,       0.0,       0.0 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C BAL -- b4
      DATA PMDBAL /
    &       0.0,       0.0,       0.0,       0.0, -0.000803,
    &       0.0, -0.001738,       0.0,       0.0,       0.0,
    &       0.0,       0.0,       0.0,       0.0,       0.0,
    & -0.006774,       0.0,       0.0, -0.000335, -0.001313,
    &       0.0,       0.0,       0.0 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C RD -- b5
      DATA PMRD /
    &       0.0,       0.0,       0.0,       0.0, -0.436587,
    &       0.0, -0.053315,       0.0,       0.0,       0.0,
    &       0.0,       0.0,       0.0,       0.0,       0.0,
    & -0.311677,       0.0,       0.0, -0.506973, -0.761759,
    &       0.0,       0.0,       0.0 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C ln(CR) -- b6
      DATA PMLNCR / 0.70405 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C ELEV-- b7
      DATA PMEL / -0.000142 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C SLOPE -- b8
      DATA PMSLOP / 0.002143 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C SLOPE*cos(ASPECT) -- b9
      DATA PMSASP / -0.001601 /
C
C *** END PERMAFROST DIAMETER GROWTH MODIFIER COEFFICIENTS ***
C----------


C_Old      DATA DGLD/
C_Old     & 0.56846, 1.07184, 0.56846, 0.38233,  0.62381,  0.99465,
C_Old     & 0.99465, 0.68136, 0.56846, 0.511442, 0.889596, 0.99465, 0.56846/
C_Old
C_Old      DATA DGCR/
C_Old     & 4.60641, 2.46701, 4.60641, 3.28729,  3.06852,  2.39021,
C_Old     & 2.39021, 3.08338, 4.60641, 0.623093, 1.732535, 2.39021, 4.60641/
C_Old
C_Old      DATA DGCRSQ/
C_Old     & -3.33043, -1.86814, -3.33043, -2.36796, -2.07432, -1.60504,
C_Old     & -1.60504, -1.86516, -3.33043,  0.0, 0.0, -1.60504, -3.33043/
C_Old
C_Old      DATA DGBAL/
C_Old     & 0.00840, 0.00198, 0.00840, 0.00608,     0.0, 0.00402,
C_Old     & 0.00402,     0.0, 0.00840, 0.008903, 0.0, 0.00402, 0.00840/
C_Old
C_Old      DATA DGDBAL/
C_Old     & -0.02759, -0.00128, -0.02759, -0.02029, -0.00638, -0.00646,
C_Old     &-0.00646,-0.00881,-0.02759,-0.027074,-0.001265,-0.00646,-0.02759/
C_Old
C_Old      DATA DGLNBA/
C_Old     &      0.0, -0.44018,      0.0,      0.0, -0.14942, -0.20534,
C_Old     & -0.20534, -0.26754,      0.0, -0.481983, 0.0, -0.20534,   0.0/
C_Old
C_Old      DATA DGBA/
C_Old     & -0.00215,      0.0, -0.00215, -0.00137,      0.0,      0.0,
C_Old     &      0.0,      0.0, -0.00215,    0.0, -0.000981, 0.0, -0.00215/
C_Old
C_Old      DATA DGHAH/
C_Old     &      0.0,  0.29750,      0.0,      0.0,      0.0, -0.10963,
C_Old     & -0.10963,      0.0,      0.0, 0.0, 0.0, -0.10963,      0.0/
C----------
C  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
C  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
C  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
C  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).
C----------
      DATA  OBSERV/
     &  4279,  4279,  2499, 25371, 64609, 
     & 64609, 25371,  4279,   856,  1412, 
     &  9751,  5615, 64609,   187,   187,  
     & 31245, 31245,  4464, 21019,  4464, 
     &  4464,  4464,  4464 /
C_Old      DATA  OBSERV/
C_Old     & 2678.0,  301.0, 2678.0, 5000.0, 5000.0,  557.0,
C_Old     &  557.0, 2678.0, 2678.0,  1369., 220., 557.0, 2678.0/
C----------
C  DGSITE IS AN ARRAY THAT CONTAINS SITE CLASS INTERCEPTS FOR
C  EACH SPECIES.
C----------
      DATA DGSITE/
     & 1.12514, 0.00573, 1.12514, 0.74079, 0.83695, 0.20346,
     & 0.20346, 1.27029, 1.12514, 0.237269,0.227307,0.20346, 1.12514/
C----------
C  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
C  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
C----------
      DATA MAPLOC/
     & 1, 2, 3, 4, 0, 0, 0,
     & 1, 1, 2, 2, 0, 0, 0,
     & 1, 2, 3, 4, 0, 0, 0,
     & 1, 2, 3, 4, 0, 0, 0,
     & 1, 2, 3, 1, 0, 0, 0,
     & 1, 2, 3, 3, 0, 0, 0,
     & 1, 2, 3, 3, 0, 0, 0,
     & 1, 2, 3, 1, 0, 0, 0,
     & 1, 2, 3, 4, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 2, 3, 3, 0, 0, 0,
     & 1, 2, 3, 4, 0, 0, 0/

      DATA DGFOR/
     &  -3.46832,  -3.48281,  -3.23448,  -3.61663,     0.0,     0.0,
     &   1.10897,   1.52503,       0.0,       0.0,     0.0,     0.0,
     &  -3.46832,  -3.48281,  -3.23448,  -3.61663,     0.0,     0.0,
     &  -1.86637,  -1.71679,  -1.56184,  -1.99657,     0.0,     0.0,
     &  -2.23801,  -2.09016,  -1.94542,       0.0,     0.0,     0.0,
     &  -0.79657,  -1.14325,  -0.41948,       0.0,     0.0,     0.0,
     &  -0.79657,  -1.14325,  -0.41948,       0.0,     0.0,     0.0,
     &  -3.14889,  -3.20919,  -2.96203,       0.0,     0.0,     0.0,
     &  -3.46832,  -3.48281,  -3.23448,  -3.61663,     0.0,     0.0,
     &  4.253807,       0.0,       0.0,       0.0,     0.0,     0.0,
     & -0.107648,       0.0,       0.0,       0.0,     0.0,     0.0,
     &  -0.79657,  -1.14325,  -0.41948,       0.0,     0.0,     0.0,
     &  -3.46832,  -3.48281,  -3.23448,  -3.61663,     0.0,     0.0/
C----------
C  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
C  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
C  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
C  COEFFICIENT.
C----------
      DATA MAPDSQ/
     & 1, 1, 2, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 2, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 2, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 0,
     & 1, 1, 2, 1, 0, 0, 0/

      DATA DGDS/
     & -0.000165, -0.000042,       0.0,       0.0,
     & -0.000239,       0.0,       0.0,       0.0,
     & -0.000165, -0.000042,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,
     & -0.000124,       0.0,       0.0,       0.0,
     & -0.000003,       0.0,       0.0,       0.0,
     & -0.000003,       0.0,       0.0,       0.0,
     & -0.000703,       0.0,       0.0,       0.0,
     & -0.000165, -0.000042,       0.0,       0.0,
     &-0.0005099,       0.0,       0.0,       0.0,
     &       0.0,       0.0,       0.0,       0.0,
     & -0.000003,       0.0,       0.0,       0.0,
     & -0.000165, -0.000042,       0.0,       0.0/
C----------
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'DGF',3,ICYC)
      IF(DEBUG) WRITE(JOSTND,2)ICYC
    2 FORMAT(' ENTERING DGF  CYCLE =',I5)
C----------
C  DEBUG OUTPUT: MODEL COEFFICIENTS.
C----------
      IF(DEBUG)WRITE(JOSTND,*) 'IN DGF,HTCON=',HTCON,
     &'RMAI=',RMAI,'ELEV=',ELEV,'RELDEN=',RELDEN
      IF(DEBUG)
     & WRITE(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGBAL
 9000 FORMAT(/11(1X,F10.5))
C----------
C  DIAMETER GROWTH EQUATION
C
C  ADI = exp(X)
C
C  X = b1 + b2 * DBH^2 + b3 * ln(DBH) + b4 * BAL+ b5 * RD
C    + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
C    + b9 * SLOPE * cos(ASPECT) + b10*ln(SI)
C
C  Example:
C  X = b1 + b2*(10.0)^2 + b3*ln(10.0) + b4*(100.0) + b5*(0.35)
C    + b6*ln(45) + b7*1500 + b8*50
C    + b9*50*cos(3.14159) + b10*ln(75)
C
C  WHERE:
C  ADI = Annual diameter increment (in year-1)
C  DBH = Diameter at breast height (in)
C  BAL = Plot level basal area in larger trees (ft2 * acre-1)
C  RD = Relative density (plot Zeide SDI / plot Max SDI – decimal)
C  CR = Crown ratio (%)
C  ELEV = Elevation of plot (ft)
C  SLOPE = Slope of plot (%)
C  ASPECT = Aspect of plot (radians)
C  SI = site index (ft)
C
C----------
C
C  COMPUTE RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
C  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
C
      DLO = 0.0
      DHI = 500.0
      ISPC = 0
      IWHO = 1
      DO I1 = I, PI
         CALL SDICLS (ISPEC,DLO,DHI,IWHO,SDIC,SDIC2,A,B,I1)
         ZRD(I1) = SDIC2
      END DO

C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.EQ.0) GO TO 20
        I2=ISCT(ISPC,2)
        SSITE = SITEAR(ISPC)
        CONSPP= DGCON(ISPC) + COR(ISPC)
C
C       ELEVATION, SLOPE, ASPECT AND SITE INDEX COMPONENT OF
C       DIAMETER GROWTH EQUATION
C       THESE ARE THE COMPONENTS OF THE EQUATION THAT ARE NOT
C       TREE SPECIFIC
C
        DGCOMP1 = DGEL(ISPC) * TEMEL
       &        + DGSLOP(ISPC) * TEMSLP
       &        + DGSASP(ISPC) * TEMSASP
       &        + DGLNSI(ISPC) * LOG(SSITE)

C       IF PRESENCE OF PERMAFROST IS TRUE, SET UP FIRST COMPONENTS
c       OF MODIFIER VARIABLES FOR PERMAFROST AFFECTED SPECIES. 
C       THESE ARE NOT POINT OR TREE SPECIFIC: ELEVATION, SLOPE, ASPECT
C
        IF (LPERM .AND. (ISPC .EQ.  5 .OR. ISPC .EQ.  7 .OR.
       &                 ISPC .EQ. 16 .OR. ISPC .EQ. 19 .OR.
       &                 ISPC .EQ. 20)) THEN
       	  PMCOMP1 = PMEL * TEMEL
       &          + PMSLOP * TEMSLP
       &          + PMSASP * TEMSASP
        ELSE
          PMCOMP1 = 0.0
       	ENDIF

C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
        DO I3=I1,I2
          I=IND1(I3)
          D=DIAM(I)
          IF (D.LE.0.0) GOTO 10
          D2=D*D
          CR=FLOAT(ICR(I))*0.01

C         BASAL AREA IN LARGER TREES ON THE POINT
          BAL = PTBALT(I)

C         RELATIVE DENSITY (ZEIDI) ON THE POINT
          RDEN = ZRD(ITRE(I))) / SDIDEF(ISPC)
C
C         DIAMETER, BAL, RELATIVE DENSITY (ZEIDI) AND CROWN RATIO
C         COMPONENTS OF DIAMETER GROWTH EQUATION.
C         THESE COMPONENTS USE POINT OR TREE SPECIFIC VLUES.
C
          DGCOMP2 = DGDSQ(ISPC) * D2
     &            + DGLD(ISPC) * LOG(D)
     &            + DGDBAL(ISPC) * BAL
     &            + DGRD(ISPC) * RDEN
     &            + DGLNCR(ISPC) * LOG(CR)

C         IF PRESENCE OF PERMAFROST IS TRUE, SET UP SECOND COMPONENTS
C         OF MODIFIER VARIABLES FOR PERMAFROST AFFECTED SPECIES. 
C         THESE ARE POINT AND TREE SPECIFIC:
C         DIAMETER, POINT BAL, POINT RELATIVE DENSITY (ZEIDI) AND
C         CROWN RATIO
C
          IF (LPERM .AND. (ISPC .EQ.  5 .OR. ISPC .EQ.  7 .OR.
       &                   ISPC .EQ. 16 .OR. ISPC .EQ. 19 .OR.
       &                   ISPC .EQ. 20)) THEN
       	    PMCOMP2 = PMDSQ * D2
     &              + PMLD(ISPC) * LOG(D)
     &              + PMDBAL(ISPC) * BAL
     &              + PMRD(ISPC) * RDEN
     &              + PMLNCR * LOG(CR)
          ELSE
            PMCOMP2 = 0.0
       	  ENDIF

          H=HT(I)
          RELHT = 0.0
          IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
          IF(RELHT .GT. 1.5)RELHT=1.5
C
C         ANNUAL DIAMETER GROWTH = exp(X)
C         X = b1 + b2 * DBH^2 + b3 * ln(DBH) + b4 * BAL + b5 * RD
C             + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
C             + b9 * SLOPE * cos(ASPECT) + b10*ln(SI)
C
C         DGCOMP2 INCLUDES: b2, b3, b4, b5, b6
C         DGCOMP1 INCLUDES: b7, b8, b9, b10 
C         DIAMETER GROWTH FOR PERIOD OF LENGTH FINT YEARS.

          DGPRED = (EXP(DGCON(ISPC) + DGCOMP2 + DGCOMP1)) * FINT
          
C          ALD=ALOG(D)
C          DDSL=CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)
C     &              +DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))
C     &                        +DGHAH(ISPC)*RELHT
C     &     + DGLNBA(ISPC)*ALOG(BA) + DGBA(ISPC)*BA
          
          IF(DEBUG) WRITE(JOSTND,8000) ISPC, D, DGPRED
 8000     FORMAT('IN DGF ISPC, D, DGPRED= ',I3,2F12.5)
          
          WK2(I) = DGPRED
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
          IF(DEBUG) THEN
          WRITE(JOSTND,9001) I,ISPC,D,BAL,CR,RDEN,BA,DGPRED
 9001     FORMAT(' IN DGF, I=',I4,',  ISPC=',I3,',  D=',F7.2,
     &          ',  BAL=',F7.2,',  CR=',F7.4,
     &          ',  RELDEN=',F9.3,',  BA=',F9.3,',  DGPRED=',F7.4)
          ENDIF
   10     CONTINUE
        ENDDO
C----------
C  END OF SPECIES LOOP.
C----------
   20 CONTINUE
      ENDDO
      IF(DEBUG)WRITE(JOSTND,9002)ICYC
 9002 FORMAT(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
      RETURN

      ENTRY DGCONS
      CALL DBCHK (DEBUG,'DGCONS',6,ICYC)
C----------
C  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
C  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
C----------
C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C
C  CONSTRAIN ELEVATION TERM FOR BLACK COTTONWOOD TO BE LE 30
C  IN EQUATION ELEVATION WAS FIT IN 100'S OF FEET (PN&WC), 
C  USED HERE IN 10'S OF FEET SO IT NEEDS TO BE DIVIDED BY 10  
C  TO GET CORRECT ELEVATION EFFECT.
C----------
      TEMEL = ELEV * 100.0
      TEMSLP = SLOPE * 100.0
      TEMSASP = TEMSLP * COS(ASPECT)

      DO ISPC=1,MAXSP
        ATTEN(ISPC) = OBSERV(ISPC)
C----------
C  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
C  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
C  INITIALIZED TO 1.0 IN BLKDATA.
C----------
        IF (LDCOR2.AND.COR2(ISPC).GT.0.0) THEN
          DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
        ENDIF
        IF(DEBUG) WRITE(JOSTND,40) TEMEL, ASPECT, TEMSLP, ISPC,
     &  SITEAR(ISPC), DGCON(ISPC), COR2(ISPC)
   40   FORMAT(' IN DGF-DGCONS: TEMEL, ASPECT, TEMSLP, ISPC, ',
     &  'SITEAR(ISPC), DGCON(ISPC), COR2(ISPC): ',
     &  3F7.1,I5,F7.1,2F10.3)
      ENDDO
C
      RETURN
      END
