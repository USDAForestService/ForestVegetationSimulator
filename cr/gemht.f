      SUBROUTINE GEMHT (IMODTY,HHE,HHU,SI,DP,H,IS,BAT,AP,
     &                  BAUTBA,CCF,DGI,BARK,IHTG,HTGI)
      IMPLICIT NONE
C----------
C CR $Id$
C----------
C  FOR SPECIES WHICH HAVE A GENGYM BASES EQUATION,
C  THIS ROUTINE CALCULATES AN EVEN-AGED HEIGHT GROWTH ESTIMATE,
C  AND AN UNEVEN-AGED HEIGHT GROWTH ESTIMATE.  THEY GET MESHED
C  TOGETHER IN HTGF.
C  FOR SPECIES WHICH USE A SURROGATE EQUATION FROM ANOTHER
C  VARIANT, THIS ROUTINE CALCULATES A HEIGHT GROWTH ESTIMATE.
C----------
      REAL AGETEM,HG,HL,RATIO,HTMAX,TEMHT,AGE10,PHG,APTEM,CCFTEM
      REAL CON,TEMSI,ADJFAC,D10,Y1,Y2,FBY1,FBY2,Z,PSI,H2,HTGI
      REAL BARK,DGI,CCF,BAUTBA,AP,BAT,H,DP,SI,HHU,HHE,BATEM,HGE
      INTEGER IHTG,IS,IMODTY
      IHTG=0
C----------
C  SPECIES ORDER:
C   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
C  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
C  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
C  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
C
C  SPECIES EXPANSION:
C  UJ,AJ,RM,OJ,ER USE CR JU                              
C  NC,PW USE CR CO
C  GO,AW,EM,BK,SO USE CR OA                             
C  PB USES CR AS                              
C  PM,PD,AZ USE CR PI
C  CI USES CR PP                              
C----------
C  COMPUTE EVEN-AGED ESTIMATE AND SET APPROPRIATE BOUNDS
C  BY MODEL TYPE.
C----------
C SOUTHWEST MIXED CONIFER TYPE
C----------
      IF(IMODTY .EQ. 1) THEN
        BATEM = BAT * 0.01
        IF(BATEM .LT. 0.8) BATEM = 0.8
C----------
C COMPUTE EVEN-AGED HEIGHT GROWTH ESTIMATES -- SWMC
C SITE CURVE BREAST HEIGHT AGE, BASE AGE 100 DOUGLAS-FIR
C EDMINSTER, MATHIASEN, OLSEN 1991. RES NOTE RM-510.
C----------
        AGETEM = AP
        IF(AGETEM .GT. 210.) AGETEM = 210.
        IF(SI .LT. 80.) AGETEM = AMAX1(AGETEM,(110.0 - SI))
        HG=109.559129*(1.0-0.975884*EXP(-0.014377*AGETEM))**1.289266
     &     + 4.5
        HL= 72.512644*(1.0-0.876961*EXP(-0.020066*AGETEM))**2.016632
        HHE = -((HG - HL) * ((82.488 - SI) / 26.279)) + HG
        IF(SI .LT. 80.) THEN
          IF(AP .LT. AGETEM) HHE = ((HHE - 4.5) / AGETEM) * AP + 4.5
        ELSE
          IF(AP .LT. 20.) HHE = (0.02348 * SI - 0.93429) * AP + 4.5
        ENDIF
        RATIO = 1.0 - BAUTBA
        IF(RATIO .LT. 0.768) RATIO = 0.768
        HHE = HHE * RATIO
C----------
C SOUTHWEST PONDEROSA PINE TYPE
C----------
      ELSEIF(IMODTY .EQ. 2) THEN
        BATEM = BAT * 0.01
        IF(BATEM .LT. 0.8) BATEM = 0.8
C----------
C USE SITE INDEX CURVES FOR HEIGHT IN EVEN-AGED STANDS -- SWPP
C SITE CURVE BREAST HEIGHT AGE, BASE AGE 100 PONDEROSA PINE
C MINOR 1964.  RES NOTE RM-37.
C----------
        AGETEM = AP
        IF(AGETEM .GT. 210.) AGETEM = 210.
        IF(SI .LT. 80.) AGETEM = AMAX1(AGETEM,(120.0 - SI))
        HG=106.493954*(1.0-0.938775*EXP(-0.016066*AGETEM))**1.550720
     &     + 4.5
        HL= 78.078735*(1.0-0.843715*EXP(-0.020412*AGETEM))**2.280435
        HHE= -((HG - HL) * ((81.5585 - SI) / 21.7149)) + HG
        IF(SI .LT. 80.) THEN
          IF(AP .LT. AGETEM) HHE = ((HHE-4.5) / AGETEM) * AP + 4.5
        ELSE
          IF(AP .LT. 20.) HHE = (0.02463 * SI - 1.1025) * AP + 4.5
        ENDIF
        RATIO = 1.0 - BAUTBA
        IF(RATIO .LT. 0.768) RATIO = 0.768
        HHE = HHE * RATIO
C----------
C BLACK HILLS PONDEROSA PINE TYPE
C----------
      ELSEIF(IMODTY .EQ. 3) THEN
        BATEM = BAT
        IF(BATEM .LT. 20.0) BATEM = 20.0
C----------
C USE SITE INDEX CURVES AS BASIS FOR HEIGHT IN EVEN-AGED STANDS.
C CARL'S AMAX FUNCTION BLOWS UP AT AGE 180. THE CONSTANT
C 1.2999886 WHICH APPEARS BELOW IS CARL'S FUNCTION EVALUATED
C AT AGE 179.  SITE CURVES FLATTEN OFF AT AGE 180.
C
C HH IS A TEN YEAR HEIGHT GROWTH ESTIMATE AND IS SCALED
C TO A FINT YEAR BASIS IN HTGF.
C
C COMPUTE EVEN-AGED ESTIMATE OF HEIGHT GROWTH
C SITE CURVE TOTAL AGE, BASE AGE 100 PONDEROSA PINE
C MEYER 1961. USDA TECH BULL NO 630
C----------
        AGETEM = AP
        HTMAX = (SI + 0.3846) * 1.2999886
        IF(H .GE. HTMAX) THEN
          HGE = 0.1
          HHE = H + HGE
          GO TO 3005
        ENDIF
        IF(AGETEM .GT. 165.) THEN
          HGE = 0.1
          HHE = H + HGE
          GO TO 3005
        ENDIF
        TEMHT = (SI + 0.3846) * (-0.5234 + 1.8234 *
     &          EXP(-(1.0989 - 0.006105 * AGETEM)**2.35))
        AGE10 = AGETEM + 10.0
        HHE = (SI + 0.3846) * (-0.5234 + 1.8234 *
     &       EXP(-(1.0989 - 0.006105 * AGE10 )**2.35))
        RATIO = 1.0 - BAUTBA
        IF(RATIO .LT. 0.793) RATIO = 0.793
        PHG = HHE - TEMHT
        IF(PHG .LT. 0.0) PHG = 0.0
        HGE = PHG * RATIO
        HHE = H + HGE
 3005   CONTINUE
C----------
C  COMPUTE EVEN-AGED ASPEN ESTIMATE FOR ASPEN
C----------
C       IF(IS .EQ. 20) THEN
C         AGETEM=AP
C         IF(AGETEM .LT. 20.) AGETEM=20.
C         HHE = 4.5 + 2.07151*(SI-4.5)
C    &          * ((1.0 - EXP(-0.007719*AGETEM))**0.93972)
C         IF(AP .LT. AGETEM) HHE = ((HHE-4.5)/AGETEM)*AP + 4.5
C         RATIO = 1.0 - BAUTBA
C         IF(RATIO .LT. 0.75) RATIO=0.75
C         PHG = HHE - TEMHT
C         IF(PHG .LT. 0.0) PHG = 0.0
C         HGE = PHG * RATIO
C         HHE = H + HGE
C       ENDIF
C----------
C SPRUCE-FIR TYPE
C----------
      ELSEIF(IMODTY .EQ. 4) THEN
        BATEM = BAT
        IF(BATEM .LT. 20.0) BATEM = 20.0
C----------
C  USE SITE INDEX CURVES AS BASIS FOR HEIGHT IN EVEN-AGED STANDS
C  SITE CURVE BREAST HEIGHT AGE, BASE AGE 100 ENG SPRUCE/SALPINE FIR
C  ALEXANDER 1967.  RES PAPER RM32
C----------
         AGETEM = AP
         IF(AGETEM .LT. 30.0) AGETEM = 30.0
         HHE = (2.75780*SI**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))
     1      **(22.71944*SI**(-0.63557))) + 4.5
         IF(AP .LT. AGETEM) HHE = ((HHE - 4.5) / AGETEM) * AP+4.5
         RATIO = 1.0 - BAUTBA
         IF(RATIO .LT. 0.728) RATIO = 0.728
         HHE = HHE * RATIO
C----------
C  LODGEPOLE PINE TYPE
C----------
      ELSEIF(IMODTY .EQ. 5) THEN
        BATEM = BAT
        IF(BATEM .LT. 20.0) BATEM = 20.0
C----------
C  USE SITE INDEX CURVES AS BASIS FOR HEIGHT IN EVEN-AGED STANDS
C  SITE CURVE TOTAL AGE, BASE AGE 100 LODGEPOLE PINE
C  ALEXANDER, TACKLE, DAHMS 1967. RES PAPER RM-29.
C----------
        AGETEM = AP
        APTEM = AGETEM
        IF(AGETEM .LT. 30.0) AGETEM = 30.0
        IF(AGETEM .GT. 200.0) AGETEM = 200.0
        CCFTEM = CCF - 125.0
        IF(CCFTEM .LT. 0.0) CCFTEM = 0.0
        HHE = 9.89331 - 0.19177 * AGETEM + 0.00124 * AGETEM * AGETEM
     1      - 0.00082 * CCFTEM * SI + 0.01387 * AGETEM * SI
     2      - 0.0000455 * AGETEM * AGETEM * SI
        IF(APTEM .LE. 30.0) HHE = (HHE / AGETEM) * APTEM
        RATIO = 1.0 - BAUTBA
        IF(RATIO .LE. 0.742) RATIO = 0.742
        HHE = HHE * RATIO
C----------
C SPACE FOR FUTURE MODEL TYPES INCLUDING ASPEN.
C----------
      ELSE
        HHE = 0.
      ENDIF
C
C----------
C  BRANCH TO EQUATIONS FOR APPROPRIATE SPECIES.
C----------
      SELECT CASE(IS)
C----------
C  SUB-ALPINE FIR
C  CORKBARK FIR
C
C  SWMC,SWPP USE CORKBARK EQN FOR GENGYM SWMC
C  OTHER MODEL TYPES USE AF/CB EQN FROM GENGYM S-F & LP
C----------
      CASE(1:2)
        IF(IMODTY .LE. 2) THEN
          HHU = 4.514294 * (SI**0.755380) * ((1.0-EXP(-0.080869*DP))
     &        ** (1.409884 * BATEM ** (0.003919))) + 4.5
        ELSE
          HHU = (15.5 + 1.1 * SI) * ((1.0 - EXP(-0.097152 * DP))
     1        ** (4.698567 * BATEM ** (-0.252630))) + 4.5
        ENDIF
C----------
C  DOUGLAS-FIR
C----------
      CASE(3)
        BATEM=BAT
        IF(BATEM .LT. 0.8) BATEM=0.8
        HHU = 13.096420 * (SI**0.480509) * ((1.0-EXP(-0.077408*DP))
     &      ** (1.237589 * BATEM ** (-0.063297))) + 4.5
C----------
C  GRAND FIR
C  EQN FROM CENTRAL IDAHO VARIANT, HABITAT TYPE 520 (ABGR/CLUN)
C  SAWTOOTH NATIONAL FOREST.
C  SI ADJUSTMENT ADDED BY DIXON FOR ELEV = 6000 FT
C----------
      CASE(4)
        CON = 2.03035 - 0.6458 - 0.00013358*H*H - 0.09775*ALOG(DP)
     &      + 0.23315*ALOG(H)
        HTGI = EXP(CON + 0.62144*ALOG(DGI)) + 0.4809
        TEMSI=SI
        IF(TEMSI .LT. 40.)TEMSI=40.
        IF(TEMSI .GT. 120.)TEMSI=120.
        IF(TEMSI .LE. 80.) THEN
          ADJFAC = 0.1 + 0.0125*TEMSI
        ELSE
          ADJFAC = -0.7 + 0.0225*TEMSI
        ENDIF
        HTGI=HTGI*ADJFAC
        IHTG = 1
C----------
C  WHITE FIR
C----------
      CASE(5)
        BATEM=BAT
        IF(BATEM .LT. 0.8) BATEM=0.8
        HHU = 13.822088 * (SI**0.462393) * ((1.0-EXP(-0.075766*DP))
     &       ** (1.312638 * BATEM ** (-0.040708))) + 4.5
C----------
C  MOUNTAIN HEMLOCK
C  MH EQUATION FROM NI VARIANT, HABITAT TYPE 710 (TSME/XETE)
C  SI ADJUSTMENT ADDED BY DIXON, FOR ELEV = 6000 FT
C----------
      CASE(6)
        CON = 1.74090 - 0.6458 - 0.0000446*H*H - 0.09775*ALOG(DP)
     &      + 0.23315*ALOG(H)
        HTGI = EXP(CON + 0.34003*ALOG(DGI)) + 0.4809
        TEMSI=SI
        IF(TEMSI .LT. 40.)TEMSI=40.
        IF(TEMSI .GT. 70.)TEMSI=70.
        ADJFAC = 0.36 + 0.012*TEMSI
        HTGI=HTGI*ADJFAC
        IHTG = 1
C----------
C  WESTERN RED CEDAR
C  EQN FROM NI VARIANT, HABITAT TYPE 550 (THPL/OPHO)
C  SI ADJUSTMENT ADDED BY DIXON FOR ELEV = 6000 FT
C----------
      CASE(7)
        CON = 2.21104 - 0.9941 - 0.00003631*H*H - 0.1219*ALOG(DP)
     &      + 0.23315*ALOG(H)
        HTGI = EXP(CON + 0.37042*ALOG(DGI)) + 0.4809
        TEMSI=SI
        IF(TEMSI .LT. 40.)TEMSI=40.
        IF(TEMSI .GT. 110.)TEMSI=110.
        ADJFAC = 0.0875 + 0.01375*TEMSI
        IF(ADJFAC .LT. 1.0) ADJFAC=1.0
        HTGI=HTGI*ADJFAC
        IHTG = 1
C----------
C  WESTERN LARCH
C  EQN FROM NI VARIANT, 260 HABITAT TYPE, BITTERROOT NF
C  SI ADJUSTMENT ADDED BY DIXON FOR ELEV = 6000 FT
C----------
      CASE(8)
        CON = 1.81759 + 0.1433 - 0.00002607*H*H - 0.3899*ALOG(DP)
     &      + 0.23315*ALOG(H)
        HTGI = EXP(CON + 0.75756*ALOG(DGI)) + 0.4809
        TEMSI=SI
        IF(TEMSI .LT. 40.)TEMSI=40.
        IF(TEMSI .GT. 120.)TEMSI=120.
        ADJFAC = 0.23337 + 0.008333*TEMSI
        HTGI=HTGI*ADJFAC
        IHTG = 1
C----------
C  LIMBER PINE
C  EQN FROM UT VARIANT
C  SITE ADJUSTMENT ADDED BY DIXON
C----------
      CASE(10)
        IHTG = 1
        D10=DP+DGI/BARK
        IF(DP.GT.45.1 .OR. H.GT.94.5 .OR. D10.GT.45.1) THEN
          HTGI = 0.1
        ELSE
          Y1=(DP-0.1)/45.
          Y2=(H-4.5)/90.
          FBY1=ALOG(Y1/(1.0-Y1))
          FBY2=ALOG(Y2/(1.0-Y2))
          Z=(0.30546 + 0.94823*FBY2 - 0.70453*(1.64770+1.35015*FBY1))
     &      * (1.0 - 0.70453**2)**(-0.5)
          PSI=2.46480*((D10-0.1)/(45.1-D10))**1.00316
     &        *(EXP(Z*((1.0-0.70453**2))**0.5/0.94823))
          H2=((PSI/(1.0+PSI))*90.)+4.5
          IF(H2 .LT. H) H2=H+0.1
          HTGI=H2-H
          TEMSI=SI
          IF(TEMSI .LT. 20.)TEMSI=20.
          IF(TEMSI .GT.  60.)TEMSI=60.
          IF(TEMSI .LT. 40) THEN
            ADJFAC = 0.2 + 0.015*TEMSI
          ELSE
            ADJFAC = -0.1 + 0.0225*TEMSI
          ENDIF
          HTGI=HTGI * ADJFAC
        ENDIF
C----------
C  LODGEPOLE PINE
C  WHITEBARK PINE
C----------
      CASE(11,14)
        BATEM = BAT
        IF(BATEM .LT. 20.0) BATEM = 20.0
        HHU = (8.5 + 1.1 * SI) * ((1.0 - EXP(-0.085004 * DP))
     &      ** (1.709643 * BATEM ** (-0.163186))) + 4.5
C----------
C  BRISTLECONE PINE
C  PINYON PINE, SINGLELEAF PINYON, BORDER PINYON, ARIZONA PINYON PINE
C  UTAH JUNIPER, ALLIGATOR JUNIPER, ROCKY MTN JUNIPER, ONESEED JUNIPER
C  EASTERN REDCEDAR
C  GAMBEL OAK, ARIZONA WHITE OAK, EMORY OAK, BUR OAK, SILVERLEAF OAK
C  OTHER SOFTWOODS
C----------
      CASE(9,12,16,23:27,29:35,37)
        HHU = 42.269377 * (1.0 - EXP(-0.165687 * DP))
     &       ** 1.184734 + 4.5
C----------
C  PONDEROSA PINE, CHIHUAHUA PINE
C  SF AND LP MODEL TYPES USE BHPP EQN
C----------
      CASE(13,36)
        IF(IMODTY .EQ. 1) THEN
          HHU=24.244690 * (SI**0.343864) * ((1.0-EXP(-0.069180*DP))
     &        ** (1.251384 * BATEM ** (-0.272018))) + 4.5
        ELSEIF(IMODTY .EQ. 2) THEN
          HHU= 40.78321 * (SI**0.332614) * ((1.0-EXP(-0.021471*DP))
     &       ** (0.922811 * BATEM ** (-0.133923))) + 4.5
        ELSE
          BATEM = BAT * 0.01
          IF(BATEM .LT. 1.0) BATEM = 1.0
            HHU= 32.108633 * (SI**0.276926) * ((1.0-EXP(-0.057766*DP))
     &          ** (0.984340 * BATEM ** (-0.169876))) + 4.5
        ENDIF
C----------
C  WESTERN WHITE PINE
C----------
      CASE(15)
        BATEM=BAT
        IF(BATEM .LT. 0.8) BATEM=0.8
        HHU = 18.967185 * (SI**0.379790) * ((1.0-EXP(-0.071482*DP))
     &       ** (1.159608 * BATEM ** (-0.099449))) + 4.5
C----------
C  BLUE SPRUCE
C  WHITE SPRUCE
C----------
      CASE(17,19)
        BATEM=BAT
        IF(BATEM .LT. 0.8) BATEM=0.8
        HHU = 54.180173 * (SI**0.177962) * ((1.0-EXP(-0.089253*DP))
     &      ** (1.533535 * BATEM ** (-0.028852))) + 4.5
C----------
C  ENGELMANN SPRUCE
C  SWMC & SWPP USE SWMC EQN
C  ALL OTHER MODEL TYPES USE S-F EQN
C----------
      CASE(18)
        IF(IMODTY .LE. 2) THEN
          HHU = 10.616238 * (SI**0.549461) * ((1.0-EXP(-0.087283*DP))
     &        ** (1.488355 * BATEM ** (-0.027226))) + 4.5
        ELSE
          HHU = (15.5 + 1.1 * SI) * ((1.0 - EXP(-0.110383 * DP))
     1        ** (6.262899 * BATEM ** (-0.286055))) + 4.5
        ENDIF
C----------
C  ASPEN, PAPER BIRCH
C  NARROWLEAF COTTONWOOD, PLAINS COTTONWOOD
C  OTHER HARDWOODS
C
C  SWMC AND SWPP USE SWMC EQN
C  ALL OTHER MODEL TYPES USE ASPEN MODEL TYPE EQN
C----------
      CASE(20,21:22,28,38)
        IF(IMODTY .LE. 2) THEN
          HHU = 14.187987 * (SI**0.416525) * ((1.0-EXP(-0.126806*DP))
     &       ** (1.310744 * BATEM ** (-0.245126))) + 4.5
        ELSE
          BATEM=BAT
          IF(BATEM .LT. 10.) BATEM=10.
          HHU = (-2.04+1.4534*SI) * ((1.0-EXP(-0.058112*DP))
     &        ** (1.894400 * BATEM ** (-0.192979))) + 4.5
        ENDIF
C----------
C  PLACE FOR OTHER SPECIES
C----------
      CASE DEFAULT
        HHU = 0.
C
      END SELECT
C
      RETURN
      END
