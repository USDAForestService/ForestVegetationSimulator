      SUBROUTINE CCFCAL(ISPC,D,H,JCR,P,LTHIN,CCFT,CRWDTH,MODE)
      IMPLICIT NONE
C----------
C WC $Id$
C----------
C  THIS ROUTINE COMPUTES CROWN WIDTH AND CCF FOR INDIVIDUAL TREES.
C  CALLED FROM DENSE, PRTRLS, SSTAGE, AND CVCW.
C
C  ARGUMENT DEFINITIONS:
C    ISPC = NUMERIC SPECIES CODE
C       D = DIAMETER AT BREAST HEIGHT
C       H = TOTAL TREE HEIGHT
C     JCR = CROWN RATIO IN PERCENT (0-100)
C       P = TREES PER ACRE
C   LTHIN = .TRUE. IF THINNING HAS JUST OCCURRED
C         = .FALSE. OTHERWISE
C    CCFT = CCF REPRESENTED BY THIS TREE
C  CRWDTH = CROWN WIDTH OF THIS TREE
C    MODE = 1 IF ONLY NEED CCF RETURNED
C           2 IF ONLY NEED CRWDTH RETURNED
C----------
COMMONS
C
C
      INCLUDE  'PRGPRM.F77'
C
C
COMMONS
C----------
C  DIMENSION AND DATA STATEMENTS FOR INTERNAL VARIABLES.
C
C     CCF COEFFICIENTS FOR TREES THAT ARE GREATER THAN 10.0 IN. DBH:
C      RD1 -- CONSTANT TERM IN CROWN COMPETITION FACTOR EQUATION,
C             SUBSCRIPTED BY SPECIES
C      RD2 -- COEFFICIENT FOR SUM OF DIAMETERS TERM IN CROWN
C             COMPETITION FACTOR EQUATION,SUBSCRIPTED BY SPECIES
C      RD3 -- COEFFICIENT FOR SUM OF DIAMETER SQUARED TERM IN
C             CROWN COMPETITION EQUATION, SUBSCRIPTED BY SPECIES
C
C  CCF EQUATIONS DERIVED FROM PAINE AND HANN ORE STATE UNIV RP46
C
C  SPECIES ORDER:
C  1=SF  2=WF  3=GF  4=AF  5=RF  6=__  7=NF  8=YC  9=IC 10=ES
C 11=LP 12=JP 13=SP 14=WP 15=PP 16=DF 17=RW 18=RC 19=WH 20=MH
C 21=BM 22=RA 23=WA 24=PB 25=GC 26=AS 27=CW 28=WO 29=J  30=LL
C 31=WB 32=KP 33=PY 34=DG 35=HT 36=CH 37=WI 38=__ 39=OT
C
C  CCF EQUATIONS ORDER:
C     1 = PAINE AND HANN TABLE 2: SUGAR PINE
C     2 = PAINE AND HANN TABLE 2: TANOAK
C     3 = PAINE AND HANN TABLE 2: DOUGLAS-FIR
C     4 = PAINE AND HANN TABLE 2: WHITE FIR / GRAND FIR
C     5 = PAINE AND HANN TABLE 2: SUGAR PINE
C     6 = PAINE AND HANN TABLE 2: INCENSE CEDAR
C     7 = PAINE AND HANN TABLE 2: MADRONE
C     8 = PAINE AND HANN TABLE 2: CALIFORNIA BLACK OAK
C     9 = PAINE AND HANN TABLE 2: RED FIR
C    10 = PAINE AND HANN TABLE 2: PONDEROSA PINE
C    11 = PAINE AND HANN TABLE 2: DOUGLAS-FIR
C    12 = PAINE AND HANN TABLE 2: WESTERN HEMLOCK
C    13 = PAINE AND HANN TABLE 2: NOBLE FIR
C    14 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: SPRUCE
C    15 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
C    16 = PAINE AND HANN TABLE 2: CHINQUAPIN
C
C  SOURCES OF COEFFICIENTS:
C      PAINE AND HANN, 1982. MAXIMUM CROWN WIDTH EQUATIONS FOR
C        SOUTHWESTERN OREGON TREE SPECIES. RES PAP 46, FOR RES LAB
C        SCH FOR, OSU, CORVALLIS. 20PP.
C      RITCHIE AND HANN, 1985. EQUATIONS FOR PREDICTING BASAL AREA
C        INCREMENT IN DOUGLAS-FIR AND GRAND FIR. RES PAP 51, FOR RES
C        LAB SCH FOR, OSU, CORVALLIS. 9PP. (TABLE 2 PG 8)
C      SMITH 1966. STUDIES OF CROWN DEVELOPMENT ARE IMPROVING CANADIAN
C        FOREST MANAGEMENT. PROCEEDINGS, SIXTH WORLD FORESTRY CONGRESS.
C        MADRID, SPAIN. VOL 2:2309-2315. (TABLES 1 & 2, PG 2310)
C----------
C----------
      LOGICAL LTHIN
      REAL RD1(16),RD2(16),RD3(16),CRWDTH,CCFT,P,H,D
      INTEGER INDCCF(MAXSP),MODE,JCR,ISPC,IC
      INTEGER IDANUW
      LOGICAL LDANUW
C----------
C  DATA STATEMENTS
C----------
      DATA INDCCF/
     & 3*4,13,2*9,13,2*6,14,15,10,2*1,10,2*3,6,2*12,
     & 8,2*2,8,16,3*8,6, 6,2*15,7*8/
      DATA RD1/
     & 0.0392,0.03561,0.0388,0.0690,0.0392,0.0194,
     & 0.0212,0.0204, 0.0172,0.0219,0.0388,
     & 0.03758, 0.02453, 0.03, 0.01925, 0.0160/
      DATA RD2/
     & 0.0180,0.02731,0.0269,0.0225,0.0180,0.0142,
     & 0.0167,0.0246,0.00876,0.01676,0.0269,
     & 0.0233, 0.0115, 0.0173, 0.0168, 0.0167/
      DATA RD3/
     & 0.00207,0.00524,0.00466,0.00183,0.00207,0.00261,
     & 0.00330,0.0074,0.00112,0.00325,0.00466,
     & 0.00361, 0.00134, 0.00259, 0.00365, 0.00434/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = JCR
      LDANUW = LTHIN
C
C----------
C  INITIALIZE RETURN VARIABLES.
C----------
      CCFT = 0.
      CRWDTH = 0.
C----------
C  COMPUTE CCF
C----------
      IF(MODE.EQ.1) THEN
        IC = INDCCF(ISPC)
        IF (D .LT. 1.0) THEN
          CCFT = D * (RD1(IC)+RD2(IC)+RD3(IC))
        ELSE
          CCFT = RD1(IC) + RD2(IC)*D + RD3(IC)*D**2.0
        ENDIF
        CCFT = CCFT * P
      ENDIF
C----------
C  COMPUTE CROWN WIDTH
C----------
      IF(MODE.EQ.2) THEN
        CALL R6CRWD (ISPC,D,H,CRWDTH)
        IF(CRWDTH .GT. 99.9) CRWDTH=99.9
      ENDIF
C
      RETURN
      END
