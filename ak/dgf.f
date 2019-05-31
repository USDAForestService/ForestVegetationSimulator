      SUBROUTINE DGF(DIAM)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF DGPRED (CHANGE IN
C  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
C  WK2. THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
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
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CALCOM.F77'

      INCLUDE 'COEFFS.F77'

     INCLUDE 'CONTRL.F77'

      INCLUDE 'OUTCOM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'VARCOM.F77'
C
COMMONS
C
C----------
C  VARIABLE DEFINITIONS:
C---------
C
C   DIAM   -- ARRAY LOADED WITH TREE DIAMETERS
C             (PASSED AS AN ARGUEMENT).
C   DGLD   -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
C             TERM IN THE DIAMETER GROWTH EQUATION (ONE COEFFICIENT FOR EACH
C             SPECIES).
C   DGLNCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LOG(CROWN
C             RATIO) TERM IN THE DIAMETER GROWTH EQUATION
C             (ONE COEFFICIENT FOR EACH SPECIES).
C   DGLNSI -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LOG(SITE
C             INDEX) TERM IN THE DIAMETER GROWTH EQUATION (ONE COEFFICIENT FOR
C             EACH SPECIES).
C   DGSASP -- ARRAYT CONTAINING COEFFICIENT FOR SLOPE*COS(ASPECT) TERM
C   DGSLOP -- ARRAYT CONTAINING COEFFICIENT FOR SLOPE PERCENT TERM
C   DGDSQ  -- ARRAY CONTAINING THE COEFFICIENTS FOR THE DIAMETER
C             SQUARED TERM IN THE DIAMETER GROWTH EQUATION (ONE
C             COEFFICIENT FOR EACH SPECIES).
C   DGBAL  -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
C             LARGER TREES TERM IN THE DIAMETER GROWTH EQUATION
C             (ONE COEFFICIENT FOR EACH SPECIES).
C   DGRD   -- ARRAY CONTAINING COEFFICIENTS FOR RELATIVE DENSITY
C             (PLOT ZEIDE SDI / PLOT MAX SDI – DECIMAL)
C   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
C             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
C             COEFFICIENT PER SPECIES).
C   DGEL   -- CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
C             DIAMETER GROWTH EQUATION.
C   OBSERV -- CONTAINS THE NUMBER OF OBSERVATIONS BY SPECIES FOR THE
C             GROWTH MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** 
C             FOR CALIBRATION).
C----------
C SPECIES LIST FOR ALASKA VARIANT.
C
C     PERMAFROST
C      AFFECTED
C         | 
C Number  V  Code  Common Name         FIA  PLANTS Scientific Name
C   1        SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2        AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3        YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4        TA   tamarack            071  LALA   Larix laricina
C   5     P  WS   white spruce        094  PIGL   Picea glauca
C   6     P  LS   Lutz’s spruce            PILU   Picea lutzii
C   7     P  BE   black spruce        095  PIMA   Picea mariana
C   8        SS   Sitka spruce        098  PISI   Picea sitchensis
C   9        LP   lodgepole pine      108  PICO   Pinus contorta
C  10        RC   western redcedar    242  THPL   Thuja plicata
C  11        WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12        MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     P  OS   other softwoods     298  2TE
C  14        AD   alder species       350  ALNUS  Alnus species
C  15        RA   red alder           351  ALRU2  Alnus rubra
C  16     P  PB   paper birch         375  BEPA   Betula papyrifera
C  17     P  AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     P  BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     P  AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     P  CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     P  WI   willow species      920  SALIX  Salix species
C  22     P  SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     P  OH   other hardwoods     998  2TD
C----------
C  VARIABLE DECLARATIONS:
C----------
      LOGICAL DEBUG, LPERM
      INTEGER I ,I1, I2, I3, ISPC, IWHO

      REAL BAL,BRATIO,CONSPP,CR,D

      REAL SSITE,TEMEL

      REAL DGCONB1(MAXSP), DGDBAL(MAXSP), DGDISQ(MAXSP), DGEL(MAXSP),
     &     DGLD(MAXSP), DGLNCR(MAXSP), DGLNSI(MAXSP), DGRD(MAXSP),
     &     DGSASP(MAXSP), DGSLOP(MAXSP)
      REAL A, B, D2, DGCOMP1, DGCOMP2, DGPRED, DHI, DLO, RDEN,
     &     SDICS, SDICZ, TEMSLP, TEMSASP, ZRD(MAXPLT)

      REAL PFPRES, PFCON(MAXSP), PFDBAL(MAXSP), PFDSQ, PFEL, 
     &     PFLD(MAXSP), PFLNCR, PFRD(MAXSP), PFSASP, PFSLOP
      REAL PFCOMP1, PFCOMP2, PFDENO, PFMOD, PFNUMR

      REAL DUP, TEMPD1, TEMPD2
      
      REAL DIAM(MAXTRE), OBSERV(MAXSP)

C----------
C  DATA STATEMENTS:
C----------
C *** BEGIN DIAMETER GROWTH EQUATION COEFFICIENTS ***
C DESCRIBED IN DOCUMENTATION AS
C INTERCEPT -- b1
      DATA DGCONB1 /
     & -8.571521, -8.571521, -9.463168, -6.472259, -6.305809,
     & -6.305809, -6.472259, -8.571521, -9.301545, -9.415728, 
     & -9.432608, -9.000219, -6.305809, -5.054325, -5.054325, 
     & -6.155523, -6.155523, -5.762360, -6.848837, -5.762360, 
     & -5.762360, -5.762360, -5.762360 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C DBH^2 -- b2
      DATA DGDISQ /
     & -0.000436, -0.000436, -0.000436, -0.003125, -0.003125, 
     & -0.003125, -0.003125, -0.000436, -0.000436, -0.000436, 
     & -0.000436, -0.000436, -0.003125, -0.003125, -0.003125, 
     & -0.003125, -0.003125, -0.003125, -0.003125, -0.003125, 
     & -0.003125, -0.003125, -0.003125 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C ln(DBH) -- b3
      DATA DGLD /
     &  0.044114,  0.044114,  0.073453,  0.162477,  0.560619, 
     &  0.560619,  0.162477,  0.044114,  0.011996,  0.269429, 
     &  0.180706,  0.006253,  0.560619,  0.435894,  0.435894, 
     &  0.476613,  0.476613,  0.340293,  0.824344,  0.340293, 
     &  0.340293,  0.340293,  0.340293 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C BAL -- b4
      DATA DGDBAL /
     & -0.001899, -0.001899, -0.000430,  0.445278, -0.000366, 
     & -0.000366, -0.001424, -0.001899, -0.000430, -0.000430, 
     & -0.000402, -0.000713, -0.000366, -0.001244, -0.001244, 
     & -0.006573, -0.006573, -0.000472, -0.000407, -0.000472, 
     & -0.000472, -0.000472, -0.000472 /

C DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
C RD -- b5
      DATA DGRD /
     &  0.0,       0.0,       0.0,      -0.001738, -0.394170,
     & -0.394170, -0.289979,  0.0,       0.0,       0.0, 
     &  0.0,       0.0,      -0.394170, -0.394507, -0.394507, 
     & -0.289979, -0.001738, -0.738629, -0.489654, -0.738629, 
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
C IN THE CALCULATION OF THE MODIFIER.
C ADDITIVE FACTOR -- b01
      DATA PFPRES / -0.355666 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFICIENT FOR
C INTERCEPT -- b1
      DATA PFCON /
     &       0.0,       0.0,       0.0,       0.0, -6.233612,
     & -6.233612, -6.389518,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -6.233612,       0.0,       0.0,
     & -6.204504, -6.204504, -5.428688, -6.979727, -5.428688,
     & -5.428688, -5.428688, -5.428688 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C DBH^2 -- b2
      DATA PFDSQ / -0.003665 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C ln(DBH) -- b3
      DATA PFLD /
     &      0.0,      0.0,      0.0,      0.0, 0.563513,
     & 0.563513, 0.176255,      0.0,      0.0,      0.0,
     &      0.0,      0.0, 0.563513,      0.0,      0.0,
     & 0.503683, 0.503683, 0.103326, 0.879614, 0.103326,
     & 0.103326, 0.103326, 0.103326 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C BAL -- b4
      DATA PFDBAL /
     &       0.0,       0.0,       0.0,       0.0, -0.000803,
     & -0.000803, -0.001738,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.000803,       0.0,       0.0,
     & -0.006774, -0.006774, -0.001313, -0.000335, -0.001313,
     & -0.001313, -0.001313, -0.001313 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C RD -- b5
      DATA PFRD /
     &       0.0,       0.0,       0.0,       0.0, -0.436587,
     & -0.436587, -0.053315,       0.0,       0.0,       0.0,
     &       0.0,       0.0, -0.436587,       0.0,       0.0,
     & -0.311677, -0.311677, -0.761759, -0.506973, -0.761759,
     & -0.761759, -0.761759, -0.761759 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C ln(CR) -- b6
      DATA PFLNCR / 0.70405 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C ELEV-- b7
      DATA PFEL / -0.000142 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C SLOPE -- b8
      DATA PFSLOP / 0.002143 /

C DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
C SLOPE*cos(ASPECT) -- b9
      DATA PFSASP / -0.001601 /
C
C *** END PERMAFROST DIAMETER GROWTH MODIFIER COEFFICIENTS ***
C----------
C  NUMBER OF DATA OBSERVATIONS BY SPECIES FOR GROWTH MODEL
C
      DATA OBSERV /
     &   4279.0,   4279.0,  2499.0,   25371.0,  64609.0,
     &  64609.0,  25371.0,  4279.0,     856.0,   1412.0,
     &   9751.0,   5615.0,  64609.0,    187.0,    187.0,
     &  31245.0,  31245.0,   4464.0,  21019.0,   4464.0,
     &   4464.0,   4464.0,   4464.0 /

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
     & WRITE(JOSTND,9000) DGCON,DGDSQ,DGLD,DGLNCR,DGDBAL
 9000 FORMAT(/11(1X,F10.5))
C----------
C  DIAMETER GROWTH EQUATION
C
C  THIS IS OUTSIDE BARK ANNUAL DIAMETER INCREMENT THAT WILL BE
C  CONVERTED TO INSIDE BARK BASAL AREA (DDS) INCREMENT WHEN
C  LOADED INTO WK2 ARRAY AT END OF TREE LOOP.
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
C  LOAD RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
C  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
C
      DLO = 0.0
      DHI = 500.0
      ISPC = 0
      IWHO = 1
      I2 = INT(PI)
      
      DO I1 = 1, I2
         CALL SDICLS (ISPC,DLO,DHI,IWHO,SDICS,SDICZ,A,B,I1)
         ZRD(I1) = SDICZ
      END DO

C----------
C  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT
C----------
      DO ISPC=1,MAXSP
        I1=ISCT(ISPC,1)
        IF(I1.EQ.0) GO TO 20
        I2=ISCT(ISPC,2)
        SSITE = SITEAR(ISPC)
        CONSPP= DGCONB1(ISPC) + COR(ISPC)
C
C       ELEVATION, SLOPE, ASPECT AND SITE INDEX COMPONENT OF
C       DIAMETER GROWTH EQUATION
C       THESE ARE THE COMPONENTS OF THE EQUATION THAT ARE NOT
C       TREE SPECIFIC
C
        DGCOMP1 = DGEL(ISPC) * TEMEL
     &          + DGSLOP(ISPC) * TEMSLP
     &          + DGSASP(ISPC) * TEMSASP
     &          + DGLNSI(ISPC) * LOG(SSITE)

C       IF PRESENCE OF PERMAFROST IS TRUE, SET UP FIRST COMPONENTS
c       OF MODIFIER VARIABLES FOR PERMAFROST AFFECTED SPECIES. 
C       THESE ARE NOT POINT OR TREE SPECIFIC: ELEVATION, SLOPE, ASPECT
C
        IF (LPERM) THEN
          SELECT CASE (ISPC)
            CASE (5:7, 13, 16:23)
       	      PFCOMP1 = PFEL * TEMEL
     &                + PFSLOP * TEMSLP
     &                + PFSASP * TEMSASP
            CASE DEFAULT
              PFCOMP1 = 0.0
          END SELECT
        ELSE
          PFCOMP1 = 0.0
       	ENDIF

C----------
C  BEGIN TREE LOOP WITHIN SPECIES ISPC.
C----------
        DO I3=I1,I2
          I = IND1(I3)
          D = DIAM(I)
          IF (D.LE.0.0) GOTO 10
          D2 = D*D
          CR = FLOAT(ICR(I))*0.01

C         BASAL AREA IN LARGER TREES ON THE POINT
          BAL = PTBALT(I)

C         RELATIVE DENSITY (ZEIDI) ON THE POINT
          RDEN = ZRD(ITRE(I)) / SDIDEF(ISPC)
C
C         DIAMETER, BAL, RELATIVE DENSITY (ZEIDI) AND CROWN RATIO
C         COMPONENTS OF DIAMETER GROWTH EQUATION.
C         THESE COMPONENTS USE POINT OR TREE SPECIFIC VLUES.
C
          DGCOMP2 = DGDISQ(ISPC) * D2
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
          IF (LPERM) THEN
            SELECT CASE (ISPC)
            CASE (5:7, 13, 16:23)

       	      PFCOMP2 = PFDSQ * D2
     &                + PFLD(ISPC) * LOG(D)
     &                + PFDBAL(ISPC) * BAL
     &                + PFRD(ISPC) * RDEN
     &                + PFLNCR * LOG(CR)
C
C             COMPUTE PERMAFROST MODIFIER BY COMPUTING THE NUMERATOR AND
C             DENOMINATOR WITH THE EQUATION BELOW. NUMERATOR INCLUDES PERM
C             FACTOR AND DENOMINATOR DOES NOT. THE RESULTING MODIFIER
C             SHOULD BE LESS THAN 1.0
C             IF PERMAFROST IS NOT PRESENT, PFMOD WILL BE 1.0, NO AFFECT.
C
C             ADI = exp(X)
C
C             X = b1 + PERM + b2 * DBH2 + b3 * ln(DBH) + b4 * BAL
C               + b5 * RD + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
C               + b9 * SLOPE * cos(ASPECT)
C             WHERE:
C             ADI = Annual diameter increment (in year-1)
C             PERM = permafrost presence variable
C             DBH = Diameter at breast height (in)
C             BAL = Plot level basal area in larger trees (ft2 * acre-1)
C             RD = Relative density (plot Zeide SDI / plot Max SDI - decimal)
C             CR = Crown ratio (%)
C             ELEV = Elevation of plot (ft)
C             SLOPE = Slope of plot (%)
C             ASPECT = Aspect of plot (radians)
C
              PFNUMR = (EXP(PFCON(ISPC) + PFPRES + PFCOMP2 + PFCOMP1))
              PFDENO = (EXP(PFCON(ISPC) + PFCOMP2 + PFCOMP1))
              PFMOD = PFNUMR / PFDENO
            CASE DEFAULT
              PFMOD = 1.0
            END SELECT
          ELSE
            PFMOD = 1.0
       	  ENDIF
C
C         ANNUAL DIAMETER GROWTH = exp(X)
C         X = b1 + b2 * DBH^2 + b3 * ln(DBH) + b4 * BAL + b5 * RD
C             + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
C             + b9 * SLOPE * cos(ASPECT) + b10*ln(SI)
C
C         DGCOMP2 INCLUDES: b2, b3, b4, b5, b6
C         DGCOMP1 INCLUDES: b7, b8, b9, b10 
C         ANNUAL DIAMETER GROWTH WITH PERMAFROST MODIFIER.

          DGPRED = (EXP(DGCONB1(ISPC) + DGCOMP2 + DGCOMP1)) * PFMOD

          IF(DEBUG) WRITE(JOSTND,8000) ISPC, D, DGPRED
 8000     FORMAT('IN DGF ISPC, D, DGPRED= ',I3,2F12.5)

C         CONVERT OUTSIDE BARK DIAMETER INCREMENT TO INSIDE BARK
C         CHANGE IN SQUARED DIAMETERS AND LOAD INTO WK2 ARRAY.
C
          TEMPD1 = D * BRATIO(ISPC,D,HT(I))           ! CURRENT DIB
          DUP = D + DGPRED                            ! WITH GROWTH DOB
          TEMPD2 = DUP * BRATIO(ISPC,DUP,HT(I))       ! WITH GROWTH DIB

          WK2(I) = TEMPD2**2 - TEMPD1**2              ! CHANGE IN DIB
C----------
C  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
          IF(DEBUG) THEN
          WRITE(JOSTND,9001) I,ISPC,D,BAL,CR,RDEN,PFMOD,DGPRED,WK2(I)
 9001     FORMAT(' IN DGF I=',I4,'  ISPC=',I3,'  D=',F7.2,
     &          '  BAL=',F7.2,'  CR=',F7.4,'  RDEN=',F9.3,
     &          '  PFMOD=',F7.5,'  DGPRED=',F7.4,'  DDS=',F7.4)
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
C  LOAD ELEVATION, SLOPE AND ASPEC.

C  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
C
C  SOME CALIBRATION RELATED VALUES MAY BE PROCESSED OR LOADED HERE,
C  BUT THAT HAS NOT BEEN WORKED OUT YET.
C
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
