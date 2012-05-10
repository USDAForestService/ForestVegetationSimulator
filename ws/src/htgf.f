      SUBROUTINE HTGF
      IMPLICIT NONE
C----------
C  **HTGF--WS    DATE OF LAST REVISION:  07/08/11
C-----------
C   THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
C   INCREMENT FOR EACH CYCLE AND LOADS IT INTO ARRAY HTG().
C   THIS ROUTINE IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.
C   ENTRY **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE 
C   DEPENDENT CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
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
      INCLUDE 'MULTCM.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'PDEN.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
C
COMMONS
C----------
      EXTERNAL RANN
      LOGICAL DEBUG
      INTEGER ISPC,I1,I2,I3,I,ITFN,INDX,IPASS,ILAT,ITLAT
      INTEGER IICR,KEYCR,K,L,IXAGE,J,ICLS
      REAL  HGLAT2(5,MAXSP),HGEL2(MAXSP),HGELQ2(MAXSP),HGSL2(MAXSP),
     &HGSI(MAXSP),HGDG2(MAXSP),HGRDG2(MAXSP),HGBAI2(MAXSP),HGBA2(MAXSP),
     &HGLBA2(MAXSP),HGBLT2(MAXSP),HGCR2(MAXSP),HGBAD2(MAXSP),
     &HGDSQ(MAXSP)
      REAL MAXHTG,MXHTG1(MAXSP),MXHTG2(MAXSP)
      REAL SCALE,ALBA,XHT,BAL,BAI,HTNEW,TEMHTG,DGI,SINDX,H,D
      REAL SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1,D2,XHT2
      REAL HHE1,HHE2,HHU1,HHU2,XWT,PCCFI,AP,TSITE,AGETEM,RATIO,AGEFUT,
     &DFUT,HGE,HGU,ZZRAN,POTHTG,AGP10,AG,HGUESS,RELHT,CRC,
     &CRB,CRA,HGMDCR,RHB,RHR,RHM,RHYXS,RHX,RHK,FCTRKX,FCTRRB,RHXS,
     &FCTRXB,FCTRM,HGMDRH,WTCR,WTRH,HTGMOD,HTNOW,TEMPH,BARK,BRATIO,
     &BAUTBA,HNOW,BACHLO,ADJUST,XMOD,CRATIO,CRMOD,RHMOD
      REAL MISHGF
C----------
C   MODEL COEFFICIENTS AND CONSTANTS:
C
C    IND2 -- ARRAY OF POINTERS TO SMALL TREES.
C
C   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
C            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
C            WHICH MODELS WERE DEVELOPED.
C
C------------
C
C     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
C
C     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
C     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
C     3 = WHITE FIR (WF)                    ABIES CONCOLOR
C     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
C     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
C     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
C     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
C     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
C     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
C    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
C    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
C    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
C    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
C    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
C    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
C    16 = COULTER PINE (CP)                 PINUS COULTERI
C    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
C    18 = MONTEREY PINE (MP)                PINUS RADIATA
C    19 = GRAY PINE (GP)                    PINUS SABINIANA
C         (OR CALIFORNIA FOOTHILL PINE)
C    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
C    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
C    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
C    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
C    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
C    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
C    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
C    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
C    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
C    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
C    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
C    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
C    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
C         (OR CALIFORNIA WHITE OAK)
C    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
C    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
C    35 = GIANT CHINKAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA
C    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
C    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
C    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
C    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
C    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
C    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
C    42 = OTHER SOFTWOODS (OS)
C    43 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C    FROM EXISTING WS EQUATIONS --
C      USE 1(SP) FOR 11(WP) AND 24(MH) 
C      USE 2(DF) FOR 22(BD)
C      USE 3(WF) FOR 13(SF)
C      USE 4(GS) FOR 23(RW)
C      USE 8(PP) FOR 18(MP)
C      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
C      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
C                     43(OH)
C
C    FROM CA VARIANT --
C      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE), 
C                       25(WJ), 26(WJ), AND 27(CJ)
C      USE CA12(LP) FOR 9(LP) AND 10(WB)
C
C    FROM SO VARIANT --
C      USE SO30(MC) FOR 41(MC)
C
C    FROM UT VARIANT --
C      USE UT17(GB) FOR 21(GB)
C----------
C
      DATA MXHTG1/
     &   5.4918,   5.4191,   5.4283,   5.4918,   5.2011,
     &   5.4916,   5.4283,   5.4916,       0.,       0.,
     &   5.4918,       0.,   5.4283,       0.,       0.,
     &       0.,       0.,   5.4916,       0.,       0.,
     &       0.,   5.4191,   5.4918,   5.4918,       0.,
     &       0.,       0.,   4.6570,   4.6570,   4.6570,
     &   4.6570,   4.6570,   4.6570,   5.2011,   5.2011,
     &   5.2011,   5.2011,   5.2011,   5.2011,   4.6570,
     &       0.,   5.4916,   4.6570/
C
      DATA MXHTG2/
     & -12.6438,  -8.8274,  -9.1641, -12.6438,  -7.7610,
     &  -9.5992,  -9.1641,  -9.5992,       0.,       0.,
     & -12.6438,       0.,  -9.1641,       0.,       0.,
     &       0.,       0.,  -9.5992,       0.,       0.,
     &       0.,  -8.8274, -12.6438, -12.6438,       0.,
     &       0.,       0., -21.9333, -21.9333, -21.9333,
     & -21.9333, -21.9333, -21.9333,  -7.7610,  -7.7610,
     &  -7.7610,  -7.7610,  -7.7610,  -7.7610, -21.9333,
     &       0.,  -9.5992, -21.9333/
C
      DATA HGDG2/
     &       0.,       0.,       0.,       0.,       0.,
     &   2.9620,       0.,   3.7974,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,   3.7974,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,   2.9620,       0./
C
      DATA HGRDG2/
     &  10.1890,  15.3820,   8.7587,  10.1890,   6.5757,
     &       0.,   8.7587,       0.,       0.,       0.,
     &  10.1890,       0.,   8.7587,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,  15.3820,  10.1890,  10.1890,       0.,
     &       0.,       0.,   6.5757,   6.5757,   6.5757,
     &   6.5757,   6.5757,   6.5757,   6.5757,   6.5757,
     &   6.5757,   6.5757,   6.5757,   6.5757,   6.5757,
     &       0.,       0.,   6.5757/
C
      DATA HGBAI2/
     &       0.,  -0.0694,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,  -0.0694,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0./
C
      DATA HGBA2/
     &       0.,  -0.0285,       0.,       0.,   0.0080,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,  -0.0285,       0.,       0.,       0.,
     &       0.,       0.,   0.0080,   0.0080,   0.0080,
     &   0.0080,   0.0080,   0.0080,   0.0080,   0.0080,
     &   0.0080,   0.0080,   0.0080,   0.0080,   0.0080,
     &       0.,       0.,   0.0080/
C
      DATA HGLBA2/
     &   1.2845,   6.1190,   1.2324,   1.2845,       0.,
     &       0.,   1.2324,   1.4843,       0.,       0.,
     &   1.2845,       0.,   1.2324,       0.,       0.,
     &       0.,       0.,   1.4843,       0.,       0.,
     &       0.,   6.1190,   1.2845,   1.2845,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0./
C
      DATA HGBLT2/
     &       0.,       0.,       0.,       0.,  -0.0079,
     &       0.,       0.,  -0.0142,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,  -0.0142,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,  -0.0079,  -0.0079,  -0.0079,
     &  -0.0079,  -0.0079,  -0.0079,  -0.0079,  -0.0079,
     &  -0.0079,  -0.0079,  -0.0079,  -0.0079,  -0.0079,
     &       0.,       0.,  -0.0079/
C
      DATA HGBAD2/
     &       0.,       0.,  -0.0399,       0.,       0.,
     &       0.,  -0.0399,       0.,       0.,       0.,
     &       0.,       0.,  -0.0399,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0./
C
      DATA HGCR2/
     &       0.,       0.,   0.0363,       0.,       0.,
     &       0.,   0.0363,       0.,       0.,       0.,
     &       0.,       0.,   0.0363,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0./
C
      DATA HGDSQ/
     &  -0.0098,       0.,  -0.0098,  -0.0098,       0.,
     &  -0.0082,  -0.0098,  -0.0086,       0.,       0.,
     &  -0.0098,       0.,  -0.0098,       0.,       0.,
     &       0.,       0.,  -0.0086,       0.,       0.,
     &       0.,       0.,  -0.0098,  -0.0098,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,  -0.0082,       0./
C
C----------
C  COEFFICIENTS--CROWN RATIO (CR) BASED HT. GRTH. MODIFIER
C----------
      DATA CRA /100.0/, CRB /3.0/, CRC /-5.0/
C----------
C  COEFFICIENTS--RELATIVE HEIGHT (RH) BASED HT. GRTH. MODIFIER
C----------
      DATA RHK /1.0/, RHXS /0.0/
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HTGF',4,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTGF  CYCLE =',I5)
      IF(DEBUG)WRITE(JOSTND,*) 'IN HTGF AT BEGINNING,HTCON=',
     *HTCON,'RMAI=',RMAI,'ELEV=',ELEV
      SCALE=FINT/YR
      ISMALL=0
      ALBA=0.0
      IF(BA.GT.0.0)ALBA=ALOG(BA)
C----------
C  GET THE HEIGHT GROWTH MULTIPLIERS.
C----------
      CALL MULTS (2,IY(ICYC),XHMULT)
      IF(DEBUG)WRITE(JOSTND,*)'HTGF- ISPC,IY(ICYC),XHMULT= ',ISPC,
     & IY(ICYC), XHMULT
C----------
C   BEGIN SPECIES LOOP:
C----------
      DO 40 ISPC=1,MAXSP
      I1 = ISCT(ISPC,1)
      IF (I1 .EQ. 0) GO TO 40
      I2 = ISCT(ISPC,2)
      IF(DEBUG)WRITE(JOSTND,100)HTCON(ISPC),ITLAT,ELEV,SLOPE,
     & SITEAR(ISPC)
  100 FORMAT(' IN HTGF 100F HTCON,ITLAT,ELEV,SLOPE,SITE=',F10.4,I5,
     & 2F10.4,F6.0)
      XHT=XHMULT(ISPC)
      IF(LHCOR2 .AND. HCOR2(ISPC).GT.0.0) THEN
        XHT2 = HCOR2(ISPC)
      ELSE
        XHT2=1.0
      ENDIF
      SINDX = SITEAR(ISPC)
C-----------
C   BEGIN TREE LOOP WITHIN SPECIES LOOP
C-----------
      DO 30 I3=I1,I2
      I=IND1(I3)
      HTG(I)=0.0
      IF (PROB(I).LE.0.0)THEN
        IF(LTRIP)THEN
          ITFN=ITRN+2*I-1
          HTG(ITFN)=0.
          HTG(ITFN+1)=0.
        ENDIF
        GO TO 30
      ENDIF
C
      SELECT CASE (ISPC)
C----------
C  SPECIES USING EQUATIONS FROM THE UT VARIANT (21=GB)
C
C  THESE ESTIMATES ARE NO LONGER USED; GB TREES OF ALL SIZES ARE GROWTH
C  WITH EQUATIONS IN REGENT
C----------
      CASE(21)
C----------
C   FOR 21=GB:
C   BYPASS CALCULATION IF DIAMETER IS LESS THAN LOWER LIMIT
C   SAVE POINTERS TO SMALL TREES
C----------
        IF (DBH(I).LT.0.5 .OR. HT(I).LE.4.5)THEN
          ISMALL=ISMALL + 1
          IND2(ISMALL) = I
          GO TO 201
        ENDIF
        ADJUST=1.0
        D=DBH(I)
        ICLS = IFIX(D + 1.0)
        IF(ICLS .GT. 41) ICLS = 41
        BARK=BRATIO(ISPC,D,HT(I))
        BAUTBA= BAU(ICLS)/BA
        HNOW=HT(I)
        HHE1 = 0.0
        HHE2 = 0.0
        HHU1 = 0.0
        HHU2 = 0.0
        XWT  = 0.0
        PCCFI=PCCF(ITRE(I))
        DGI=DG(I)
        AP=ABIRTH(I)
C----------
C  ADJUST AP FOR BREAST HIGH AGE CURVES IF NECESSARY
C----------
        TSITE=SITEAR(ISPC)
        IF(TSITE.LT.20.)TSITE=20.
        AP=AP-(4.5/(-0.22+0.0155*TSITE))
        IF(AP.LT.1.)AP=1.
C----------
C  USE SITE INDEX CURVES AS BASIS FOR HEIGHT IN EVEN-AGED STANDS
C  SITE CURVE BREAST HEIGHT AGE, BASE AGE 100 ENG SPRUCE/SALPINE FIR
C  ALEXANDER 1967.  RES PAPER RM32
C----------
        TSITE=SITEAR(ISPC)
        AGETEM = AP
        IF(AGETEM .LT. 30.0) AGETEM = 30.0
        HHE1 = (2.75780*TSITE**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))
     1      **(22.71944*TSITE**(-0.63557))) + 4.5
        IF(AP .LT. AGETEM) HHE1 = ((HHE1 - 4.5) / AGETEM) * AP+4.5
        RATIO = 1.0 - BAUTBA
        IF(RATIO .LT. 0.728) RATIO = 0.728
        HHE1 = HHE1 * RATIO
C
        HHU1 = 42.269377 * (1.0 - EXP(-0.165687 * D))
     &       ** 1.184734 + 4.5
C
        IF(DEBUG)WRITE(JOSTND,*)' IN HTGF 1ST CALL TO GEMHT= ',
     &  HHE1,HHU1,TSITE,D,AP,AGETEM,BAUTBA,RATIO
C
        AGEFUT = AP + 10.0
        AGETEM = AGEFUT
        IF(AGETEM .LT. 30.0) AGETEM = 30.0
        DFUT=D + DGI/BARK
        HHE2 = (2.75780*TSITE**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))
     1      **(22.71944*TSITE**(-0.63557))) + 4.5
        IF(AGEFUT .LT. AGETEM) HHE2=((HHE2 - 4.5) / AGETEM) * AGEFUT+4.5
        RATIO = 1.0 - BAUTBA
        IF(RATIO .LT. 0.728) RATIO = 0.728
        HHE2 = HHE2 * RATIO
C
        HHU2 = 42.269377 * (1.0 - EXP(-0.165687 * DFUT))
     &       ** 1.184734 + 4.5
C
        IF(DEBUG)WRITE(JOSTND,*)' IN HTGF 2ND CALL TO GEMHT= ',
     &  HHE2,HHU2,TSITE,DFUT,AGEFUT,AGETEM,BAUTBA,RATIO
C
C----------
C IF EVEN-AGED,UNEVEN-AGED AND BA .LT. 70, OR UNEVEN-AGED AND PCT(I)
C IS .GE.40 (OVERSTORY) THEN HTG(I) IS EVEN-AGED HT GROWTH ESTIMATE.
C----------
        HTG(I) = (HHE2 - HHE1)*ADJUST
C----------
C IF STAND IS UNEVEN-AGED AND BA IS .GE. TO 70 THEN USE A BLEND.
C TREES WITH PCT() .LE. 10 (OVERTOPPED TREES) GET UNEVEN-AGED
C HEIGHT GROWTH; TREES WITH PCT() .GT. 10 AND .LT. 40 GET A
C WEIGHTED AVERAGE OF EVEN/UNEVEN-AGED HEIGHT GROWTH.
C----------
        IF(AGERNG .GT. 40.0 .AND. BA .GE. 70.0) THEN
          IF(PCT(I) .LE. 10.)HTG(I) = HHU2 - HHU1
          IF((PCT(I) .GT. 10.0) .AND. (PCT(I) .LT. 40.0)) THEN
            HGE = (HHE2 - HHE1)*ADJUST
            HGU = HHU2 - HHU1
            XWT = ((PCT(I)-10.)*(10./3.))/100.
            HTG(I) = XWT*HGE + (1.0-XWT)*HGU
          ENDIF
        ENDIF
C----------
C     ADD RANDOM INCREMENT TO HTG.  GETS AWAY FROM ALL TREES HAVING
C     THE SAME INCREMENT (ESP UNDER EDMINSTERS EVEN-AGED LOGIC).
C----------
  190   CONTINUE
        ZZRAN = 0.0
        IF(DGSD .GT. 0.0) THEN
          ZZRAN=BACHLO(0.0,1.0,RANN)
          IF(ZZRAN .GT. DGSD .OR. ZZRAN .LT. (-DGSD)) GO TO 190
          IF(DEBUG)WRITE(JOSTND,9984) I,HTG(I),ZZRAN,SCALE
 9984     FORMAT(1H ,'IN HTGF 9984 FORMAT',I5,2X,3(F10.4,2X))
        ENDIF
        HTG(I) = HTG(I) + ZZRAN*0.1
C----------
C  IF STAGNATION EFFECT IS ON FOR THIS SPECIES,
C  ONLY REDUCE HT GROWTH BY HALF OF DSTAG, NOT DSTAG. DIXON 3-9-93
C----------
        IF(ISTAGF(ISPC).NE.0)HTG(I)=HTG(I)*(DSTAG+1.0)*.5
        IF(HTG(I) .LT. 0.1) HTG(I) = 0.1
        IF(DEBUG)
     &  WRITE(JOSTND,*)' HTGF I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT,HTG = ',
     &                I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT(I),HTG(I)
C
  201   CONTINUE
        HTG(I)=HTG(I)*SCALE*XHMULT(ISPC)*XHT2
C----------
C       APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT.
C----------
        HTG(I)=HTG(I)*MISHGF(I,ISPC)
C
        TEMHTG=HTG(I)
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(I)+HTG(I)).GT.SIZCAP(ISPC,4))THEN
          HTG(I)=SIZCAP(ISPC,4)-HT(I)
          IF(HTG(I) .LT. 0.1) HTG(I)=0.1
        ENDIF
C
        IF(.NOT. LTRIP) GO TO 30
        ITFN = ITRN + 2*I - 1
        HTG(ITFN) = TEMHTG
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(ITFN)+HTG(ITFN)).GT.SIZCAP(ISPC,4))THEN
          HTG(ITFN)=SIZCAP(ISPC,4)-HT(ITFN)
          IF(HTG(ITFN) .LT. 0.1) HTG(ITFN)=0.1
        ENDIF
C
        HTG(ITFN+1) = TEMHTG
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(ITFN+1)+HTG(ITFN+1)).GT.SIZCAP(ISPC,4))THEN
          HTG(ITFN+1)=SIZCAP(ISPC,4)-HT(ITFN+1)
          IF(HTG(ITFN+1) .LT. 0.1) HTG(ITFN+1)=0.1
        ENDIF
C
        IF(DEBUG)WRITE(JOSTND,9001)HTG(ITFN),HTG(ITFN+1)
 9001   FORMAT('  LOWER HTG = ',F8.4,'  UPPER HTG = ',F8.4)
C
C
C----------
C  SPECIES USING EQUATIONS FROM THE SO VARIANT (41=MC)
C  IN SO, MISC. SPECIES - USE CURTIS, FOR. SCI. 20:307-316.  
C  CURTIS CURVES ARE PRESENTED IN METRIC (3.2808 ?)
C
C EXCESSIVE HT GROWTH -- APPROX 30-40 FT/CYCLE, TAKE OUT METRIC MULT
C DIXON 11-05-92
C----------
      CASE(41)
        BAL=((100.0-PCT(I))/100.0)*BA
        H=HT(I)
        D=DBH(I)
C
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        D2 = 0.0
        CALL FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,DEBUG)
C
        IF(H .GE. HTMAX)THEN
          HTG(I)=0.1
          HTG(I)=SCALE*XHT*HTG(I)*XHT2
          GO TO 161
        ENDIF
        IF (SITAGE .GT. AGMAX) THEN
          POTHTG= 0.10
          GO TO 1319
        ELSE
          AGP10= SITAGE+10.0
        ENDIF
        HGUESS = (SINDX - 4.5) / ( 0.6192 - 5.3394/(SINDX - 4.5)
     &         + 240.29 * AGP10**(-1.4) +(3368.9/(SINDX - 4.5))
     &         *AGP10**(-1.4))
        HGUESS = HGUESS + 4.5
        IF(DEBUG)WRITE(JOSTND,*)' SINDX,ISPC,AGP10,I,HGUESS= '
        IF(DEBUG)WRITE(JOSTND,*) SINDX,ISPC,AGP10,I,HGUESS
C
        POTHTG= HGUESS-SITHT
C
        IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,AGP10,SITHT,HGUESS= ',
     &  I,ISPC,AGP10,SITHT,HGUESS
C
 1319   CONTINUE
C----------
C  HEIGHT GROWTH MODIFIERS
C----------
        IF(DEBUG)WRITE(JOSTND,*) ' AT 1319 CONTINUE FOR TREE',I,' HT= ',
     &  HT(I),' AVH= ',AVH 
        RELHT = 0.0
        IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
        IF(RELHT .GT. 1.5)RELHT=1.5
C-----------
C     REVISED HEIGHT GROWTH MODIFIER APPROACH.
C-----------
C     CROWN RATIO CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
C     GROWTH PEAKS IN MID-RANGE OF CR, DECREASES SOMEWHAT FOR LARGE
C     CROWN RATIOS DUE TO PHOTOSYNTHETIC ENERGY PUT INTO CROWN SUPPORT
C     RATHER THAN HT. GROWTH.  CROWN RATIO FOR THIS COMPUTATION NEEDS
C     TO BE IN (0-1) RANGE; DIVIDE BY 100.  FUNCTION IS HOERL'S
C     SPECIAL FUNCTION (REF. P.23, CUTHBERT&WOOD, FITTING EQNS. TO DATA
C     WILEY, 1971).  FUNCTION OUTPUT CONSTRAINED TO BE 1.0 OR LESS.
C-----------
        HGMDCR = (CRA * (ICR(I)/100.0)**CRB) * EXP(CRC*(ICR(I)/100.0))
        IF (HGMDCR .GT. 1.0) HGMDCR = 1.0
C-----------
C     RELATIVE HEIGHT CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
C     GROWTH IS ENHANCED BY STRONG TOP LIGHT AND HINDERED BY HIGH
C     SHADE EVEN IF SOME LIGHT FILTERS THROUGH.  ALSO RESPONSE IS
C     GREATER FOR GIVEN LIGHT AS SHADE TOLERANCE INCREASES.  FUNCTION
C     IS GENERALIZED CHAPMAN-RICHARDS (REF. P.2 DONNELLY ET AL. 1992.
C     THINNING EVEN-AGED FOREST STANDS...OPTIMAL CONTROL ANALYSES.
C     USDA FOR. SERV. RES. PAPER RM-307).
C     PARTS OF THE GENERALIZED CHAPMAN-RICHARDS FUNCTION USED TO
C     COMPUTE HGMDRH BELOW ARE SEGMENTED INTO FACTORS
C     FOR PROGRAMMING CONVENIENCE.
C-----------
        RHB = (-1.45)
        RHR = 15.0
        RHM = 1.10
        RHYXS = 0.10
        RHX = RELHT
        FCTRKX = ( (RHK/RHYXS)**(RHM-1.0) ) - 1.0
        FCTRRB = -1.0*( RHR/(1.0-RHB) )
        FCTRXB = RHX**(1.0-RHB) - RHXS**(1.0-RHB)
        FCTRM  = -1.0/(RHM-1.0)
C
        IF (DEBUG)
     &  WRITE(JOSTND,*) ' HTGF-HGMDRH FACTORS = ',
     &  ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM
C
        HGMDRH = RHK * ( 1.0 + FCTRKX*EXP(FCTRRB*FCTRXB) ) ** FCTRM
C-----------
C     APPLY WEIGHTED MODIFIER VALUES.
C-----------
        WTCR = .25
        WTRH = 1.0 - WTCR
        HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH
C----------
C    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
C    MULTIPLIED BY XHMULT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
C----------
        IF(DEBUG) THEN
          WRITE(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,'HTGMOD= ',
     &    HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
          WRITE(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',RELHT,
     &   ' HGMDRH= ',HGMDRH
        ENDIF
C
        IF (HTGMOD .GE. 2.0) HTGMOD= 2.0
        IF (HTGMOD .LE. 0.0) HTGMOD= 0.1
C
 1322   HTG(I) = POTHTG * HTGMOD
C
        HTNOW=HT(I)+POTHTG
        IF(DEBUG)WRITE(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),
     &  POTHTG,BAL,AVH,HTG(I),DBH(I),RMAI,HGUESS
  901   FORMAT(' HTGF',I5,13F9.2)
C
  999   CONTINUE
C-----------
C    HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
C    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
C    MULTIPLIED BY XHMULT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
C    CHECK FOR HT GT MAX HT FOR THE SITE AND SPECIES
C----------
        TEMPH=H + HTG(I)
        IF(TEMPH .GT. HTMAX)THEN
          HTG(I)=HTMAX - H
        ENDIF
        IF(HTG(I).LT.0.1)HTG(I)=0.1
C
  161   CONTINUE
        IF(DEBUG)WRITE(JOSTND,*)
     &  ' I,SCALE,HTG,HTMAX, H= ',I,SCALE,HTG(I),HTMAX,H
C
C
C----------
C  SPECIES USING EQUATIONS FROM THE CA VARIANT (9=LP, 10=WB, 12=PM,
C  14=KP, 15=FP, 16=CP, 17=LM, 19=GP, 20=WE, 25=WJ, 26=UT, 27=CJ)
C----------
      CASE(9:10,12,14:17,19:20,25:27)
        AGP10=0.0
        HGUESS=0.0
        H=HT(I)
C
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        D1 = DBH(I)
        D2 = 0.0
        CALL FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,
     &              DEBUG)
C
C----------
C  NORMAL HEIGHT INCREMENT CALCULATON BASED ON INCOMMING TREE AGE
C  FIRST CHECK FOR MAX, ASMYPTOTIC HEIGHT
C----------
        IF (SITAGE .GT. AGMAX) THEN
          POTHTG= 0.10
          GO TO 1320
        ELSE
          AGP10= SITAGE+10.0
        ENDIF
C----------
C R5 USE DUNNING/LEVITAN SITE CURVE.
C SPECIES DIFFERENCES ARE ARE ACCOUNTED FOR BY THE SPECIES
C SPECIFIC SITE INDEX VALUES WHICH ARE SET AFTER KEYWORD PROCESSING.
C----------
        CALL HTCALC(IFOR,SINDX,ISPC,AGP10,HGUESS,JOSTND,DEBUG)
C
        POTHTG= HGUESS - SITHT
C
        IF(DEBUG)WRITE(JOSTND,91200)I,ISPC,AGP10,HGUESS,H
91200   FORMAT(' IN GUESS AN AGE--I,ISPC,AGEP10,HGUESS,H ',2I5,3F10.2)
C----------
C ASSIGN A POTENTIAL HTG FOR THE ASYMPTOTIC AGE
C----------
 1320   CONTINUE
        XMOD=1.0
        CRATIO=ICR(I)/100.0
        RELHT=H/AVH
        IF(RELHT .GT. 1.0)RELHT=1.0
        IF(PCCF(ITRE(I)) .LT. 100.0)RELHT=1.0
C--------
C  THE TREE HEIGHT GROWTH MODIFIER (SMHMOD) IS BASED ON THE RITCHIE &
C  HANN WORK (FOR.ECOL.&MGMT. 1986. 15:135-145).  THE ORIGINAL COEFF.
C  (1.117148) IS CHANGED TO 1.016605 TO MAKE THE SMALL TREE HEIGHTS
C  CLOSE TO THE SITE INDEX CURVE.  THE MODIFIER HAS TWO PARTS, ONE
C  (CRMOD) FOR TREE VIGOR USING CROWN RATIO AS A SURROGATE; OTHER
C  (RHMOD) FOR COMPETITION FROM NEIGHBORING TREES USING RELATIVE TREE
C  HEIGHT AS A SURROGATE.
C----------
        CRMOD=(1.0-EXP(-4.26558*CRATIO))
        RHMOD=(EXP(2.54119*(RELHT**0.250537-1.0)))
        XMOD= 1.016605*CRMOD*RHMOD
        HTG(I) = POTHTG * XMOD
        IF(HTG(I) .LT. 0.1) HTG(I)=0.1
C
        IF(DEBUG)WRITE(JOSTND,902)ICR(I),PCT(I),BA,DG(I),HT(I),
     &   POTHTG,AVH,HTG(I),DBH(I),RMAI,HGUESS,AGP10,XMOD,ABIRTH(I)
  902   FORMAT(' HTGF',I5,14F9.2)
C----------
C  HTG IS MULTIPLIED BY SCALE TO CHANGE FROM A YR  PERIOD TO FINT AND
C  MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
C----------
        HTG(I)=SCALE*XHT*HTG(I)*XHT2
C
        IF(DEBUG)WRITE(JOSTND,*)' I=',I,
     &          ' D=',DBH(I),' DG=',DG(I),' H=',H,' HTG=',HTG(I)
C----------
C       APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT.
C----------
        HTG(I)=HTG(I)*MISHGF(I,ISPC)
C
        TEMHTG=HTG(I)
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(I)+HTG(I)).GT.SIZCAP(ISPC,4))THEN
          HTG(I)=SIZCAP(ISPC,4)-HT(I)
          IF(HTG(I) .LT. 0.1) HTG(I)=0.1
        ENDIF
C
        IF(.NOT.LTRIP) GO TO 30
        ITFN=ITRN+2*I-1
        HTG(ITFN)=TEMHTG
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(ITFN)+HTG(ITFN)).GT.SIZCAP(ISPC,4))THEN
          HTG(ITFN)=SIZCAP(ISPC,4)-HT(ITFN)
          IF(HTG(ITFN) .LT. 0.1) HTG(ITFN)=0.1
        ENDIF
C
        HTG(ITFN+1)=TEMHTG
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(ITFN+1)+HTG(ITFN+1)).GT.SIZCAP(ISPC,4))THEN
          HTG(ITFN+1)=SIZCAP(ISPC,4)-HT(ITFN+1)
          IF(HTG(ITFN+1) .LT. 0.1) HTG(ITFN+1)=0.1
        ENDIF
C
        IF(DEBUG) WRITE(JOSTND,9003) HTG(ITFN),HTG(ITFN+1)
 9003   FORMAT( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)
C
C
C----------
C  SPECIES USING EQUATIONS FROM THE WS VARIANT
C----------
      CASE DEFAULT
        BAL=((100.0-PCT(I))/100.0)*BA
        IF(BAL .LE. 0.0)BAL=.001
        HTG(I) = HTCON(ISPC)
     &   + HGDG2(ISPC)*DG(I)
     &   + HGRDG2(ISPC)*SQRT(DG(I)) + HGBA2(ISPC)*BA
     &   + HGBAI2(ISPC)*(((DBH(I)+DG(I))**2.0)- DBH(I)*DBH(I))
     &   + HGLBA2(ISPC)*ALBA + HGBLT2(ISPC)*BAL
     &   + HGBAD2(ISPC)*BAL/DBH(I) + HGCR2(ISPC) * FLOAT(ICR(I))
     &   + HGDSQ(ISPC)*DBH(I)*DBH(I)
C---------
C THE FOLLOWING MODIFIERS HAVE BEEN ADDED BECAUSE TREES WERE NOT
C HITTING SITE HEIGHT
C---------
        SELECT CASE (ISPC)
        CASE(42)
          HTG(I)=HTG(I) *1.5
        CASE(2,22)
          HTG(I)=HTG(I) *1.25
        CASE(3,13)
          HTG(I)=HTG(I) *1.5
        CASE(4,23)
          HTG(I)=HTG(I) *1.1
        CASE(5)
          HTG(I)=HTG(I) *1.2
        CASE(6)
          HTG(I)=HTG(I) *1.5
        CASE(7)
          HTG(I)=HTG(I) *1.2
        CASE(8,18)
          HTG(I)=HTG(I)*1.15
        CASE(34:39)
          HTG(I)=HTG(I)*0.75
        END SELECT
        IF(DEBUG)WRITE(JOSTND,130)I,ISPC,DBH(I),HT(I),
     &  ICR(I),DG(I),BA,BAL,HTG(I)
  130   FORMAT(' IN HTGF I,ISPC,DBH,HT,ICR,DG,BA,BAL,HTG=',
     &  2I5,2F7.2,I5,4F7.2)
C----------
C THIS NEXT TRAP IS TEMPORARY WHILE DOLPH DOES SOME FURTHER ANALYSES.
C THE DSQ TERM CAUSES NEGATIVE HTG ESTIMATES FOR LARGE TREES.
C DIXON 8-18-93
C----------
        IF(HTG(I) .LT. 0.5) HTG(I)=0.5
C----------
C THE FOLLOWING IS SO WE DON'T GET BIG HTG WHEN SMALL DG
C FOR LARGE TREES. DIXON 8-18-93
C----------
        IF(DG(I).LT.1.0 .AND. DBH(I).GT.30.) HTG(I)=HTG(I)*DG(I)
C
C CHECK TO SEE THAT PREDICTED HTG DOES NOT EXCEED MAXIMUM BOUND
C FOR JP,OS, AND PM. (DID NOT HAVE A MAX HT FN FOR THESE SPECIES.
C
        BAI = ((DBH(I) + DG(I))**2.0) - DBH(I)*DBH(I)
        IF(ISPC.EQ.42 .OR. ISPC.EQ.6) THEN
          MAXHTG = -2.16 + 4.22 * ALOG(BAI)
          IF(HTG(I) .GT. MAXHTG) HTG(I) = MAXHTG
        ENDIF
C
C CHECK TO SEE THAT HT + HTG DOES NOT EXCEED MAXIMUM ALLOWABLE HT.
C USE PP COEFFICIENTS FOR JP,OC AND LP FOR THIS CHECK.
C
        INDX = I
        IPASS = 0
  131   CONTINUE
        IPASS = IPASS + 1
        SELECT CASE (ISPC)
        CASE(28:33,40,43)
          HTMAX=EXP(MXHTG1(ISPC)+MXHTG2(ISPC)/
     &    ((DBH(I)+DG(INDX)+1.)**2.0))+4.5
        CASE DEFAULT
          HTMAX=EXP(MXHTG1(ISPC)+MXHTG2(ISPC)/(DBH(I)+DG(INDX)+1.))+4.5
        END SELECT
        HTNEW = HT(I)+HTG(INDX)

        IF(DEBUG)WRITE(JOSTND,9131)
     &  INDX,ISPC,HT(I),HTG(INDX),HTNEW,HTMAX,DBH(I),DG(INDX)
 9131   FORMAT(' IN HTGF 9131F ',2I5,6F10.3)
        IF(HTNEW .GT. HTMAX) HTG(INDX)=HTMAX-HT(I)
        IF(HTG(INDX) .LT. 0.1)HTG(INDX)=0.1
        IF(IPASS .EQ. 2) GO TO 141
        IF(IPASS .EQ. 3) GO TO 142
C
C END OF HTG AND MAX HT CHECK SEQUENCE
C
        IF(DEBUG)WRITE(JOSTND,140)I,ISPC,BAI,MAXHTG,HTG(I)
  140   FORMAT(1H ,'HTGF 140F I,ISPC,BAI,MAXHTG,HTG = ',2I5,3F10.4)
C
C ADJUST PREDICTED HTG FOR TIME INTERVAL AND USER SPECIFIED MULTIPLIER
C
        HTG(I)=HTG(I)*SCALE*XHT*XHT2
C----------
C       APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT.
C----------
        HTG(I)=HTG(I)*MISHGF(I,ISPC)
C
        TEMHTG=HTG(I)
        IF(DEBUG)WRITE(JOSTND,9225)ICYC,HTG(I),SCALE
 9225   FORMAT(' IN HTGF CYCLE ',I5,2F10.1)
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(I)+HTG(I)).GT.SIZCAP(ISPC,4))THEN
          HTG(I)=SIZCAP(ISPC,4)-HT(I)
          IF(HTG(I) .LT. 0.1) HTG(I)=0.1
        ENDIF
C
        IF(.NOT.LTRIP) GO TO 5
        ITFN=ITRN+2*I-1
        DGI=DG(I)
        IF(DGI .LT. 0.01) DGI=0.01
        HTG(ITFN)=TEMHTG * DG(ITFN)/DGI
        INDX = ITFN
        GO TO 131
  141   CONTINUE
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(ITFN)+HTG(ITFN)).GT.SIZCAP(ISPC,4))THEN
          HTG(ITFN)=SIZCAP(ISPC,4)-HT(ITFN)
          IF(HTG(ITFN) .LT. 0.1) HTG(ITFN)=0.1
        ENDIF
C
        DGI=DG(I)
        IF(DGI .LT. 0.01) DGI=0.01
        HTG(ITFN+1)=TEMHTG * DG(ITFN+1)/DGI
        INDX = ITFN + 1
        GO TO 131
  142   CONTINUE
C----------
C CHECK FOR SIZE CAP COMPLIANCE.
C----------
        IF((HT(ITFN+1)+HTG(ITFN+1)).GT.SIZCAP(ISPC,4))THEN
          HTG(ITFN+1)=SIZCAP(ISPC,4)-HT(ITFN+1)
          IF(HTG(ITFN+1) .LT. 0.1) HTG(ITFN+1)=0.1
        ENDIF
C
      END SELECT
C----------
C   END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.
C----------
    5 CONTINUE
C
C**   IF(DEBUG)THEN
C**   HTNEW=HT(I)+HTG(I)
C**   WRITE (JOSTND,9000) HTG(I),CON,XHT2,H2COF,D,
C**  & WK1(I),HTNEW,I,ISPC
C9000 FORMAT(' 9000 HTGF, HTG=',F8.4,' CON=',F12.4,' XHT2=',F8.4,
C**  & ' H2COF=',F12.8,' D=',F8.4/' WK1=',F8.4,
C**  & ' HTNEW=',F8.4,' I=',I4,' ISPC=',I2)
C**   ENDIF
C
      IF(.NOT.LTRIP) GO TO 30
      IF(DEBUG) WRITE(JOSTND,9002) HTG(ITFN),HTG(ITFN+1)
 9002 FORMAT( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)
C
   30 CONTINUE
C----------
C   END OF SPECIES LOOP
C----------
   40 CONTINUE
      IF(DEBUG)WRITE(JOSTND,60)ICYC
   60 FORMAT(' LEAVING SUBROUTINE HTGF   CYCLE =',I5)
      RETURN
C
      ENTRY HTCONS
C----------
C  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
C  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
C  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT
C  DEPENDENT COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2
C  CONTAINS HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED
C  TERM, AND HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT
C  TYPE IS INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).
C----------
      DATA HGLAT2/
     &-14.053,-14.053,-14.053,-14.053,-14.053,
     &-39.027,-39.027,-39.027,-39.027,-39.027,
     &-12.246,-10.685,-10.685,-10.685,-10.685,
     &-14.053,-14.053,-14.053,-14.053,-14.053,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -0.268, -0.268, -0.268,  3.097, -0.268,
     &-12.246,-10.685,-10.685,-10.685,-10.685,
     & -5.554, -5.554, -5.554, -4.282, -5.554,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &-14.053,-14.053,-14.053,-14.053,-14.053,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &-12.246,-10.685,-10.685,-10.685,-10.685,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     & -5.554, -5.554, -5.554, -4.282, -5.554,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &-39.027,-39.027,-39.027,-39.027,-39.027,
     &-14.053,-14.053,-14.053,-14.053,-14.053,
     &-14.053,-14.053,-14.053,-14.053,-14.053,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     & -3.831,  1.711, -3.831, -2.898, -3.831,
     &    0.0,    0.0,    0.0,    0.0,    0.0,
     & -0.268, -0.268, -0.268,  3.097, -0.268,
     & -3.831,  1.711, -3.831, -2.898, -3.831/
     
C
      DATA HGEL2/
     &       0.,       0.,  -0.0453,       0.,       0.,
     &       0.,  -0.0453,       0.,       0.,       0.,
     &       0.,       0.,  -0.0453,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0./
C
      DATA HGELQ2/
     &       0.,       0.,       0.,       0.,  -0.0007,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,  -0.0007,  -0.0007,  -0.0007,
     &  -0.0007,  -0.0007,  -0.0007,  -0.0007,  -0.0007,
     &  -0.0007,  -0.0007,  -0.0007,  -0.0007,  -0.0007,
     &       0.,       0.,  -0.0007/
C
      DATA HGSL2/
     &       0.,       0.,   3.2180,       0.,       0.,
     &       0.,   3.2180,       0.,       0.,       0.,
     &       0.,       0.,   3.2180,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0.,       0.,       0.,
     &       0.,       0.,       0./
C
      DATA HGSI/
     &   0.0881,   0.0974,   0.0713,   0.0881,   0.0643,
     &   0.0687,   0.0713,   0.0598,       0.,       0.,
     &   0.0881,       0.,   0.0713,       0.,       0.,
     &       0.,       0.,   0.0598,       0.,       0.,
     &       0.,   0.0974,   0.0881,   0.0881,       0.,
     &       0.,       0.,   0.0643,   0.0643,   0.0643,
     &   0.0643,   0.0643,   0.0643,   0.0643,   0.0643,
     &   0.0643,   0.0643,   0.0643,   0.0643,   0.0643,
     &       0.,   0.0687,   0.0643/
C----------
C  LOAD OVERALL INTERCEPT FOR EACH SPECIES.
C----------
      ITLAT = NINT(TLAT)
      IF(ITLAT .LE. 35.) ILAT=1
      IF(ITLAT .EQ. 36.) ILAT=2
      IF(ITLAT .EQ. 37.) ILAT=3
      IF(ITLAT .EQ. 38.) ILAT=4
      IF(ITLAT .GE. 39.) ILAT=5
C
      DO 50 ISPC =1,MAXSP
      SELECT CASE (ISPC)
C----------
C  SPECIES USING EQUATIONS FROM THE UT VARIANT (21=GB)
C----------
      CASE(21)
        HTCON(ISPC)=0.0
C----------
C  SPECIES USING EQUATIONS FROM THE SO VARIANT (41=MC)
C----------
      CASE(41)
        HTCON(ISPC)=0.0
C----------
C  SPECIES USING EQUATIONS FROM THE CA VARIANT (9=LP, 10=WB, 12=PM,
C  14=KP, 15=FP, 16=CP, 17=LM, 19=GP, 20=WE, 25=WJ, 26=UT, 27=CJ)
C----------
      CASE(9:10,12,14:17,19:20,25:27)
        HTCON(ISPC)=0.0
C----------
C  SPECIES USING EQUATIONS FROM THE WS VARIANT
C----------
      CASE DEFAULT
        HTCON(ISPC) = HGLAT2(ILAT,ISPC) + HGEL2(ISPC)*ELEV
     &                + HGELQ2(ISPC)*ELEV*ELEV + HGSL2(ISPC)*SLOPE
     &                + HGSI(ISPC)*SITEAR(ISPC)
      END SELECT
C
C  NOTE: CANNOT INCORPORATE HCOR2 ADJUSTMENT INTO HTCON( ) SINCE THE
C        LARGE TREE HTG EQUATION IS NOT A LOG-LINEAR REGRESSION
C
   50 CONTINUE
C
      RETURN
      END
