      SUBROUTINE CWCALC(ISPC,P,D,H,CR,IICR,CW,IWHO,JOSTND)
      IMPLICIT NONE
C----------
C LS $Id$
C----------
C  THIS ROUTINE CONTAINS A LIBRARY OF CROWN WIDTH EQUATIONS AVAILABLE
C  FOR USE IN THE EASTERN UNITED STATES.
C  EQUATIONS ARE GROUPED BY SPECIES ACCORDING TO THEIR FIA CODE.
C  THIS ROUTINE COMPUTES LARGEST CROWN WIDTH.
C  IT IS CALLED FROM **CWIDTH** TO PRODUCE CROWTH WIDTH ESTIMATES FOR
C  OF FOREST GROWN TREES, AND IT IS CALLED FROM **CCFCAL** TO PRODUCE
C  CROWN WIDTH ESTIMATES OF OPEN GROWN TREES.
C
C  DEFINITION OF VARIABLES:
C JSP(ISPC) = FVS SPECIES ALPHA NUMBER
C         P = TREES PER ACRE
C         D = TREE DBH
C         H = TREE HEIGHT
C        CR = CROWN RATIO IN PERCENT.
C      IICR = CROWN RATIO - FLOAT
C     HILAT = LATITUDE IN DECIMAL DEGREES
C    HILONG = LONGITUDE (-) IN DECIMAL DEGREES
C    HIELEV = ELEVATION IN FEET
C        HI = HOPKINS BIOCLIMATIC INDEX, SEE BECHTOLD 2003.
C      CWEQ = CW EQUATION NUMBER (FIA # + EQN #)
C               BECHTOLD EQN # = 01
C               BRAGG EQN #    = 02
C               EK EQN #       = 03
C               KRAJICEK EQN # = 04
C               SMITH EQN #    = 05
C        CW = LARGEST CROWN WIDTH
C      IWHO = 1 IF CALLED FROM CCFCAL (USE OPEN GROWN CW EQUATIONS)
C             0 OTHERWISE (USE FOREST GROWN EQUATIONS)
C
C  SOURCES OF FOREST GROWN CROWN WIDTH EQUATIONS:
C  BECHTOLD, WILLIAM A. 2003. CROWN-DIAMETER PREDICTION MODELS FOR
C     87 SPECIES OF STAND-GROWN TREES IN THE EASTERN UNITED STATES.
C     SJAF. 27(4):269-278.
C  BRAGG, DON C. 2001. A LOCAL BASAL AREA ADJUSTMENT FOR CROWN WIDTH
C     PREDICTION. NJAF. 18(1):22-28.
C
C  OPEN GROWN CROWN WIDTH EQUATIONS FOR REGIONS 8 & 9:
C
C  BECHTOLD, WILLIAM A. 2003. CROWN-DIAMETER PREDICTION MODELS FOR
C     87 SPECIES OF STAND-GROWN TREES IN THE EASTERN UNITED STATES.
C     SJAF. 27(4):269-278. MODIFIED FOR CR = 90%
C  EK, ALAN. 1974. DIMENSIONAL RELATIONSHIPS OF FOREST AND OPEN GROWN
C     TREES IN WISCONSIN. UNIV. OF WISCONSIN.
C  KRAJICEK, JOHN E., KENNETH A BRINKMAN, AND SAMUEL F. GINGRICH. 1961.
C     CROWN COMPETITION - A MEASURE OF DENSITY. FOREST SCIENCE,
C     VOL 7(1).
C  SMITH, W.R., R.M. FARRAR, JR, P.A. MURPHY, J.L. YEISER, R.S. MELDAHL,
C     AND J.S. KUSH. 1992. CROWN AND BASAL AREA RELATIONSHIPS OF OPEN-
C     GROWN SOUTHERN PINES FOR MODELING COMPETITION AND GROWTH.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
      INTEGER ISPC,IWHO,IICR,JOSTND
      CHARACTER CWEQ*5
      REAL D, CW, HI, HILAT, HILONG, HIELEV,MIND,CR,OMIND
      REAL P,H
      REAL RDANUW
      INTEGER IDANUW
C----------
C  DATA STATEMENTS.
C----------
      DATA MIND/5./,OMIND/3./
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = H
      RDANUW = P
      IDANUW = IICR
      IDANUW = JOSTND
C----------
C  INITIALIZE RETURN VARIABLES.
C----------
      CW = 0.
C----------
C SET VARIABLES FOR CROWN MODELS.
C----------
      HILAT=TLAT
      HILONG = -1*ABS(TLONG)
      HIELEV=ELEV*100
C  COMPUTE HOPKINS INDEX
      HI = ((HIELEV-887.)/100.)*1.0 + (HILAT-39.54)*4.0 +
     &      (-82.52 - HILONG)*1.25
C----------
C  SET THE EQUATION NUMBER
C----------
      SELECT CASE (NSP(ISPC,1)(1:2))
      CASE('AB ')     !AMERICAN BEECH (531)
          CWEQ='53101'
      CASE('AC ')     !AMERICAN CHESTNUT (421)                        MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('AE ')     !AMERICAN ELM (972)
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('AH ')     !AMERICAN HORNBEAM (391)
          CWEQ='39101'
      CASE('AI ')     !AILANTHUS (341)                                MAPPED TO DW
          CWEQ='49101'
      CASE('AP ')     !APPLE (660)                                    MAPPED TO PR
          CWEQ='76102'
      CASE('AS ')     !ASH (540)                                      MAPPED TO GA
        IF(IWHO.EQ.1)THEN
          CWEQ='54403'
        ELSE
          CWEQ='54401'
        ENDIF
      CASE('AW ')     !ATLANTIC WHITE CEDAR (043)                     MAPPED TO WC
          CWEQ='24101'
      CASE('BA ')     !BLACK ASH (543)
          CWEQ='54301'
      CASE('BB ')     !BIRCH (370)                                    MAPPED TO RB
          CWEQ='37301'
      CASE('BC ')     !BLACK CHERRY (762)
        IF(IWHO.EQ.1)THEN
          CWEQ='76203'
        ELSE
          CWEQ='76201'
        ENDIF
      CASE('BE ')     !BOXELDER (313)
          CWEQ='31301'
      CASE('BF ')     !BALSAM FIR (012)
        IF(IWHO.EQ.1)THEN
          CWEQ='01203'
        ELSE
          CWEQ='01201'
        ENDIF
      CASE('BG ')     !BLACKGUM, BLACK TUPELO (693)
          CWEQ='69301'
      CASE('BH ')     !BITTERNUT HICKORY (402)
          CWEQ='40201'
      CASE('BI ')     !BLACK HICKORY (408)
          CWEQ='40801'
      CASE('BJ ')     !BLACKJACK OAK (824)
          CWEQ='82401'
      CASE('BK ')     !BLACK LOCUST (901)
          CWEQ='90101'
      CASE('BL ')     !BLACK WILLOW (922)                             MAPPED TO AE
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('BM ')     !BLACK MAPLE (314)                              MAPPED TO SM
        IF(IWHO.EQ.1)THEN
          CWEQ='31803'
        ELSE
          CWEQ='31801'
        ENDIF
      CASE('BN ')     !BUTTERNUT (601)                                MAPPED TO BW
          CWEQ='60201'
      CASE('BO ')     !BLACK OAK (837)
        IF(IWHO.EQ.1)THEN
          CWEQ='83704'
        ELSE
          CWEQ='83701'
        ENDIF
      CASE('BP ')     !BALSAM POPLAR (741)
          CWEQ='74101'
      CASE('BR ')     !BUR OAK (823)
        IF(IWHO.EQ.1)THEN
          CWEQ='82303'
        ELSE
          CWEQ='82301'
        ENDIF
      CASE('BS ')     !BLACK SPURCE (095)
        IF(IWHO.EQ.1)THEN
          CWEQ='09503'
        ELSE
          CWEQ='09501'
        ENDIF
      CASE('BT ')     !BIGTOOTH ASPEN (743)
          CWEQ='74301'
      CASE('BU ')     !BUCKEYE (330)                                  MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('BW ')     !AMERICAN BASSWOOD (951)
          CWEQ='95101'
      CASE('BY ')     !BALDCYPRESS (221)
          CWEQ='22101'
      CASE('CA ')     !CATALPA (450)                                  MAPPED TO SS
          CWEQ='93101'
      CASE('CB ')     !CHERRYBARK OAK (813)                           MAPPED TO SK
          CWEQ='81201'
      CASE('CC ')     !CHOKECHERRY (763)                              MAPPED TO PR
          CWEQ='76102'
      CASE('CH ')     !OTHER COMMERCIAL HARDWOODS (na)                MAPPED TO BW
          CWEQ='60201'
      CASE('CK ')     !CHINKAPIN OAK (826)
          CWEQ='82601'
      CASE('CM ')     !CHALK MAPLE (323)                              MAPPED TO SM
        IF(IWHO.EQ.1)THEN
          CWEQ='31803'
        ELSE
          CWEQ='31801'
        ENDIF
      CASE('CO ')     !CHESTNUT OAK (832)
          CWEQ='83201'
      CASE('CT ')     !CUCUMBER TREE (651)
          CWEQ='65101'
      CASE('CW ')     !COTTONWOOD (740)                               MAPPED TO EC
        IF(IWHO.EQ.1)THEN
          CWEQ='74203'
        ELSE
          CWEQ='74201'
        ENDIF
      CASE('DM ')     !DIAMOND WILLOW (923 obs)                       MAPPED TO BP
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('DO ')     !DELTA POST OAK (836)                           MAPPED TO PO
          CWEQ='83501'
      CASE('DP ')     !DWARF POST OAK (840)                           MAPPED TO PO
          CWEQ='83501'
      CASE('DW ')     !DOGWOOD (491)
          CWEQ='49101'
      CASE('EC ')     !EASTERN COTTONWOOD (742)
        IF(IWHO.EQ.1)THEN
          CWEQ='74203'
        ELSE
          CWEQ='74201'
        ENDIF
      CASE('EH ')     !EASTERN HEMLOCK (261)
          CWEQ='26101'
      CASE('EL ')     !ELM (970)                                      MAPPED TO AE
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('FM ')     !FLORIDA MAPLE (311)                            MAPPED TO SM
        IF(IWHO.EQ.1)THEN
          CWEQ='31803'
        ELSE
          CWEQ='31801'
        ENDIF
      CASE('FR ')     !FIR (010)                                      MAPPED TO BF
        IF(IWHO.EQ.1)THEN
          CWEQ='01203'
        ELSE
          CWEQ='01201'
        ENDIF
      CASE('GA ')     !GREEN ASH (544)
        IF(IWHO.EQ.1)THEN
          CWEQ='54403'
        ELSE
          CWEQ='54401'
        ENDIF
      CASE('GB ')     !GRAY BIRCH (379)                               MAPPED TO PB
        IF(IWHO.EQ.1)THEN
          CWEQ='37503'
        ELSE
          CWEQ='37501'
        ENDIF
      CASE('HA ')     !SILVERBELL (580)                               MAPPED TO DW
          CWEQ='49101'
      CASE('HB ')     !HACKBERRY SP. (460)                            MAPPED TO HK
          CWEQ='46201'
      CASE('HH ')     !EASTERN HOPHORNBEAM (701)
          CWEQ='70101'
      CASE('HI ')     !HICKORY (400)                                  MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('HK ')     !HACKBERRY (C.OCCIDEN.) (462)
          CWEQ='46201'
      CASE('HL ')     !HONEY LOCUST (552)
          CWEQ='55201'
      CASE('HM ')     !HEMLOCK (260)                                  MAPPED TO EH
        CWEQ='26101'
      CASE('HS ')     !SELECT HICKORY (na)                            MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('HT ')     !HAWTHORN (500)                                 MAPPED TO DW
          CWEQ='49101'
      CASE('HY ')     !AMERICAN HOLLY (591)
          CWEQ='59101'
      CASE('JP ')     !JACK PINE (105)
        IF(IWHO.EQ.1)THEN
          CWEQ='10503'
        ELSE
          CWEQ='10501'
        ENDIF
      CASE('JU ')     !JUNIPER (057)                                  MAPPED TO RC
          CWEQ='06801'
      CASE('KC ')     !KENTUCKY COFFEETREE (571)                      MAPPED TO BK
          CWEQ='90101'
      CASE('LB ')     !LOBLOLLY-BAY (555)                             MAPPED TO MV
        CWEQ='65301'
      CASE('LK ')     !LAUREL OAK (820)
        CWEQ='82001'
      CASE('LL ')     !LONGLEAF PINE (121)
        IF(IWHO.EQ.1)THEN
          CWEQ='12105'
        ELSE
          CWEQ='12101'
        ENDIF
      CASE('LO ')     !LIVE OAK (838)
        CWEQ='83801'
      CASE('LP ')     !LOBLOLLY PINE (131)
        IF(IWHO.EQ.1)THEN
          CWEQ='13105'
        ELSE
          CWEQ='13101'
        ENDIF
      CASE('MA ')     !AMERICAN MTN ASH (935)                         MAPPED TO HL
          CWEQ='55201'
      CASE('MB ')     !MULBERRY (680)                                 MAPPED TO RY
          CWEQ='68201'
      CASE('MG ')     !MAGNOLIA SP. (650)                             MAPPED TO MV
          CWEQ='65301'
      CASE('MH ')     !MOCKERNUT HICKORY (409)
          CWEQ='40901'
      CASE('ML ')     !BIGLEAF MAGNOLIA (654)                         MAPPED TO MV
          CWEQ='65301'
      CASE('MM ')     !MOUNTAIN MAPLE (319)                           MAPPED TO BE
          CWEQ='31301'
      CASE('MP ')     !MAPLE SP. (310)                                MAPPED TO SM
        IF(IWHO.EQ.1)THEN
          CWEQ='31803'
        ELSE
          CWEQ='31801'
        ENDIF
      CASE('MS ')     !SOUTHERN MAGNOLIA (652)                        MAPPED TO MV
          CWEQ='65301'
      CASE('MV ')     !SWEETBAY (653)
          CWEQ='65301'
      CASE('NC ')     !NON-COMMERCIAL SPECIES (na)                    MAPPED TO DW
          CWEQ='49101'
      CASE('NK ')     !NUTALL OAK (828)                               MAPPED TO SK
          CWEQ='81201'
      CASE('NP ')     !NORTHERN PIN OAK (809)
        CWEQ='80901'
      CASE('NS ')     !NORWAY SPRUCE (091)
        IF(IWHO.EQ.1)THEN
          CWEQ='09104'
        ELSE
          CWEQ='09101'
        ENDIF
      CASE('OB ')     !OHIO BUCKEYE (331)                             MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('OC ')     !OTHER CEDAR (240 obs)                          MAPPED TO RC
          CWEQ='06801'
      CASE('OG ')     !OGEECHEE TUPELO (692)                          MAPPED TO WT
          CWEQ='69101'
      CASE('OH ')     !OTHER HARDWOODS (998 fmsc)                     MAPPED TO SS
          CWEQ='93101'
      CASE('OK ')     !OAK SP. (800)                                  MAPPED TO WO
        IF(IWHO.EQ.1)THEN
          CWEQ='80204'
        ELSE
          CWEQ='80201'
        ENDIF
      CASE('OL ')     !OTHER LOWLAND SPECIES (na)                     MAPPED TO SY
          CWEQ='73101'
      CASE('OO ')     !OSAGE ORANGE (641)                             MAPPED TO SS
          CWEQ='93101'
      CASE('OP ')     !OTHER PINES (100 obs)                          MAPPED TO WP
        IF(IWHO.EQ.1)THEN
          CWEQ='12903'
        ELSE
          CWEQ='12901'
        ENDIF
      CASE('OS ')     !OTHER SOFTWOODS (298 fmsc)                     MAPPED TO RC
          CWEQ='06801'
      CASE('OT ')     !OTHER (999 FIA, UNKNOWN DEAD)                  MAPPED TO RM
        IF(IWHO.EQ.1)THEN
          CWEQ='31603'
        ELSE
          CWEQ='31601'
        ENDIF
      CASE('OV ')     !OVERCUP OAK (822)                              MAPPED TO BR
        IF(IWHO.EQ.1)THEN
          CWEQ='82303'
        ELSE
          CWEQ='82301'
        ENDIF
      CASE('PA ')     !PUMPKIN ASH (545)                              MAPPED TO WA
          CWEQ='54101'
      CASE('PB ')     !PAPER BIRCH (375)
        IF(IWHO.EQ.1)THEN
          CWEQ='37503'
        ELSE
          CWEQ='37501'
        ENDIF
      CASE('PC ')     !PONDCYPRESS (222)                              MAPPED TO BY
          CWEQ='22101'
      CASE('PD ')     !POND PINE (128)
          CWEQ='12801'
      CASE('PE ')     !PECAN (404)                                    MAPPED TO BH
          CWEQ='40201'
      CASE('PH ')     !PIGNUT HICKORY (403)
          CWEQ='40301'
      CASE('PI ')     !SPRUCE (090)                                   MAPPED TO WS
        IF(IWHO.EQ.1)THEN
          CWEQ='09403'
        ELSE
          CWEQ='09401'
        ENDIF
      CASE('PL ')     !PLUM (760)                                     MAPPED TO PR
          CWEQ='76102'
      CASE('PN ')     !PIN OAK (830)
          CWEQ='83001'
      CASE('PO ')     !POST OAK (835)
          CWEQ='83501'
      CASE('PP ')     !PITCH PINE (126)
          CWEQ='12601'
      CASE('PR ')     !PIN CHERRY (761)
          CWEQ='76102'
      CASE('PS ')     !PERSIMMON (521)
          CWEQ='52101'
      CASE('PU ')     !SAND PINE (107)                                MAPPED TO VP
          CWEQ='13201'
      CASE('PW ')     !PAULOWNIA (712)                                MAPPED TO SS
          CWEQ='93101'
      CASE('PY ')     !SWAMP COTTONWOOD (744)                         MAPPED TO EC
        IF(IWHO.EQ.1)THEN
          CWEQ='74203'
        ELSE
          CWEQ='74201'
        ENDIF
      CASE('PZ ')     !PONDEROSA PINE (122)                           MAPPED TO LP
        IF(IWHO.EQ.1)THEN
          CWEQ='13105'
        ELSE
          CWEQ='13101'
        ENDIF
      CASE('QA ')     !QUAKING ASPEN (746)
        IF(IWHO.EQ.1)THEN
          CWEQ='74603'
        ELSE
          CWEQ='74601'
        ENDIF
      CASE('QI ')     !SHINGLE OAK (817)
          CWEQ='81701'
      CASE('QN ')     !BLUEJACK OAK (842)                             MAPPED TO TO
          CWEQ='81901'
      CASE('QS ')     !SHUMARD OAK (834)                              MAPPED TO SK
          CWEQ='81201'
      CASE('RA ')     !REDBAY (721)
          CWEQ='72101'
      CASE('RB ')     !RIVER BIRCH (373)
          CWEQ='37301'
      CASE('RC ')     !EASTERN REDCEDAR (068)
          CWEQ='06801'
      CASE('RD ')     !REDBUD (471)                                   MAPPED TO DW
          CWEQ='49101'
      CASE('RE ')     !ROCK ELM (977)                                 MAPPED TO AE
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('RL ')     !SLIPPERY ELM (975)
          CWEQ='97501'
      CASE('RM ')     !RED MAPLE (316)
        IF(IWHO.EQ.1)THEN
          CWEQ='31603'
        ELSE
          CWEQ='31601'
        ENDIF
      CASE('RN ')     !RED PINE (NATURAL) (125 dup)
        IF(IWHO.EQ.1)THEN
          CWEQ='12503'
        ELSE
          CWEQ='12501'
        ENDIF
      CASE('RO ')     !NORTHERN RED OAK (833)
        IF(IWHO.EQ.1)THEN
          CWEQ='83303'
        ELSE
          CWEQ='83301'
        ENDIF
      CASE('RP ')     !RED PINE (PLANTATION)(125 dup)
        IF(IWHO.EQ.1)THEN
          CWEQ='12503'
        ELSE
          CWEQ='12501'
        ENDIF
      CASE('RS ')     !RED SPRUCE (097)
          CWEQ='09701'
      CASE('RY ')     !RED MULBERRY (682)
          CWEQ='68201'
      CASE('SA ')     !SLASH PINE (111)
          CWEQ='11101'
      CASE('SB ')     !SWEET BIRCH (372)
          CWEQ='37201'
      CASE('SC ')     !SCOTCH PINE (130)
          CWEQ='13001'
      CASE('SD ')     !SOURWOOD (711)
          CWEQ='71101'
      CASE('SE ')     !SERVICEBERRY (356)                             MAPPED TO DW
          CWEQ='35601'
      CASE('SG ')     !SUGARBERRY (461)
          CWEQ='46201'
      CASE('SH ')     !SHAGBARK HICKORY (407)
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('SI ')     !SIBERIAN ELM (974)                             MAPPED TO AE
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('SK ')     !SOUTHERN RED OAK (812)
          CWEQ='81201'
      CASE('SL ')     !SHELLBARK HICKORY (405)                        MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      CASE('SM ')     !SUGAR MAPLE (318)
        IF(IWHO.EQ.1)THEN
          CWEQ='31803'
        ELSE
          CWEQ='31801'
        ENDIF
      CASE('SN ')     !SWAMP CHESTNUT OAK (825)                       MAPPED TO CO
          CWEQ='83201'
      CASE('SO ')     !SCARLET OAK (806)
          CWEQ='80601'
      CASE('SP ')     !SHORTLEAF PINE (110)
        IF(IWHO.EQ.1)THEN
          CWEQ='11005'
        ELSE
          CWEQ='11001'
        ENDIF
      CASE('SR ')     !SPRUCE PINE (115)                              MAPPED TO SP
        IF(IWHO.EQ.1)THEN
          CWEQ='11005'
        ELSE
          CWEQ='11001'
        ENDIF
      CASE('SS ')     !SASSAFRAS (931)
          CWEQ='93101'
      CASE('ST ')     !STRIPED MAPLE (315)                            MAPPED TO BE
          CWEQ='31301'
      CASE('SU ')     !SWEETGUM (611)
          CWEQ='61101'
      CASE('SV ')     !SILVER MAPLE (317)
          CWEQ='31701'
      CASE('SW ')     !SWAMP WHITE OAK (804)                          MAPPED TO WO
        IF(IWHO.EQ.1)THEN
          CWEQ='80204'
        ELSE
          CWEQ='80201'
        ENDIF
      CASE('SY ')     !SYCAMORE (731)
        CWEQ='73101'
      CASE('TA ')     !TAMARACK (071)
        IF(IWHO.EQ.1)THEN
          CWEQ='07103'
        ELSE
          CWEQ='07101'
        ENDIF
      CASE('TL ')     !TUPELO (690)                                   MAPPED TO BG
          CWEQ='69301'
      CASE('TM ')     !TABLE MTN PINE (123)                           MAPPED TO PP
          CWEQ='12601'
      CASE('TO ')     !TURKEY OAK (819)
          CWEQ='81901'
      CASE('TS ')     !SWAMP TUPELO,SW BLACKGUM (694)
          CWEQ='69401'
      CASE('UA ')     !BLUE ASH (546)                                 MAPPED TO WA
          CWEQ='54101'
      CASE('UH ')     !OTHER UPLAND HARDWOODS (na)                    MAPPED TO SS
          CWEQ='93101'
      CASE('VP ')     !VIRGINIA PINE (132)
          CWEQ='13201'
      CASE('WA ')     !WHITE ASH (541)
          CWEQ='54101'
      CASE('WB ')     !WHITE BASSWOOD (952)                           MAPPED TO BW
          CWEQ='95101'
      CASE('WC ')     !NORTHERN WHITE CEDAR (241)
          CWEQ='24101'
      CASE('WE ')     !WINGED ELM (971)
          CWEQ='97101'
      CASE('WH ')     !WATER HICKORY (401)                            MAPPED TO BW
          CWEQ='40201'
      CASE('WI ')     !WILLOW (920)                                   MAPPED TO AE
        IF(IWHO.EQ.1)THEN
          CWEQ='97203'
        ELSE
          CWEQ='97201'
        ENDIF
      CASE('WK ')     !WATER OAK (827)
          CWEQ='82701'
      CASE('WL ')     !WILLOW OAK (831)
          CWEQ='83101'
      CASE('WM ')     !WHITE MULBERRY (681)                           MAPPED TO RY
          CWEQ='68201'
      CASE('WN ')     !BLACK WALNUT (602)
          CWEQ='60201'
      CASE('WO ')     !WHITE OAK (802)
        IF(IWHO.EQ.1)THEN
          CWEQ='80204'
        ELSE
          CWEQ='80201'
        ENDIF
      CASE('WP ')     !EASTERN WHITE PINE (129)
        IF(IWHO.EQ.1)THEN
          CWEQ='12903'
        ELSE
          CWEQ='12901'
        ENDIF
      CASE('WR ')     !WATER BIRCH (374)                              MAPPED TO RB
          CWEQ='37301'
      CASE('WS ')     !WHITE SPRUCE (094)
        IF(IWHO.EQ.1)THEN
          CWEQ='09403'
        ELSE
          CWEQ='09401'
        ENDIF
      CASE('WT ')     !WATER TUPELO (691)
          CWEQ='69101'
      CASE('YB ')     !YELLOW BIRCH (371)
          CWEQ='37101'
      CASE('YP ')     !YELLOW POPLAR (621)
          CWEQ='62101'
      CASE('YY ')     !YELLOW BUCKEYE (332)                           MAPPED TO SH
        IF(IWHO.EQ.1)THEN
          CWEQ='40703'
        ELSE
          CWEQ='40701'
        ENDIF
      END SELECT
C----------
C  CALCULATE CROWN WIDTH
C----------
      SELECT CASE (CWEQ)
C----------
C SOFTWOODS
C----------
C-----------------------------------------------------------------------
C  CASE 01001
C  010         ABIES SPP.                                       FIR SPP.
C         CASE ('01001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 01201 BECHTOLD 2003 MODEL 2
C  012         ABIES BALSAMEA                                 BALSAM FIR
         CASE ('01201')
           IF (D .GE. MIND) THEN
              CW = 0.6564 + 0.8403*D + 0.0792*CR
           ELSE
             CW = (0.6564 + 0.8403*MIND + 0.0792*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 34.) CW=34.
C
C  CASE 01202 BRAGG 2001 MODEL NLCW
C  012         ABIES BALSAMEA                                 BALSAM FIR
         CASE ('01202')
           CW = -3.746931 + 7.122778*D**0.396998
C
C  CASE 01203 EK, 1974
C  012         ABIES BALSAMEA                                 BALSAM FIR
         CASE ('01203')
           IF (D .GE. OMIND) THEN
             CW = 0.3270 + 5.1160*D**0.5035
           ELSE
             CW = (0.3270 + 5.1160*OMIND**0.5035)*(D/OMIND)
           ENDIF
           IF (CW .GT. 34.) CW=34.
C-----------------------------------------------------------------------
C  CASE 04301
C  043         CHAMAECYPARIS THYOIDES               ATLANTIC WHITE-CEDAR
C         CASE ('04301')
C           CW =
C-----------------------------------------------------------------------
C  CASE 05701
C  057         JUNIPERUS SPP.                    REDCEDAR / JUNIPER SPP.
C         CASE ('05701')
C           CW =
C-----------------------------------------------------------------------
C  CASE 06801 BECHTOLD 2003 MODEL 2
C  068         JUNIPERUS VIRGINIANA                     EASTERN REDCEDAR
         CASE ('06801')
           IF (D .GE. MIND) THEN
             CW = 1.2359 + 1.2962*D + 0.0545*CR
           ELSE
             CW = (1.2359 + 1.2962*MIND + 0.0545*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 33.) CW=33.
C ----------------------------------------------------------------------
C  CASE 07101 BECHTOLD 2003 MODEL 2
C  071         LARIX LARICINA                          TAMARACK (NATIVE)
         CASE ('07101')
           IF (D .GE. MIND) THEN
             CW = -0.3276 + 1.3865*D + 0.0517*CR
           ELSE
             CW = (-0.3276 + 1.3865*MIND + 0.0517*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 29.) CW=29.
C
C  CASE 07102 BRAGG 2001 MODEL NLCW
C  071         LARIX LARICINA                          TAMARACK (NATIVE)
         CASE ('07102')
           CW = 2.503585 + 1.100883*D**1.056165
C
C  CASE 07103 EK, 1974
C  071         LARIX LARICINA                          TAMARACK (NATIVE)
         CASE ('07103')
           IF (D .GE. OMIND) THEN
              CW = 2.205 + 3.475*D**0.7506
           ELSE
             CW = (2.205 + 3.475*OMIND**0.7506)*(D/OMIND)
           ENDIF
           IF (CW .GT. 29.) CW=29.
C-----------------------------------------------------------------------
C  CASE 09001
C  090         PICEA SPP.                                    SPRUCE SPP.
C         CASE ('09001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 09101 BECHTOLD 2003 MODEL 3
C  091         PICEA ABIES                                 NORWAY SPRUCE
         CASE ('09101')
           IF (D .GE. MIND) THEN
             CW = 1.8336 + 0.9932*D + 0.0431*CR + 0.1012*HI
           ELSE
             CW = (1.8336 + 0.9932*MIND + 0.0431*CR + 0.1012*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C
C  CASE 09104 KRAJICEK AND OTHERS 1961
C  091         PICEA ABIES                                 NORWAY SPRUCE
         CASE ('09104')
           IF (D .GE. OMIND) THEN
             CW = 5.0570 + 1.1313*D
           ELSE
             CW = (5.0570 + 1.1313*OMIND)*(D/OMIND)
           ENDIF
           IF (CW .GT. 47.) CW=47.
C-----------------------------------------------------------------------
C  CASE 09401 BECHTOLD 2003 MODEL 2
C  094         PICEA GLAUCA                                 WHITE SPRUCE
         CASE ('09401')
           IF (D .GE. MIND) THEN
             CW = 0.3789 + 0.8658*D + 0.0878*CR
           ELSE
             CW = (0.3789 + 0.8658*MIND + 0.0878*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 30.) CW=30.
C
C  CASE 09402 BRAGG 2001 MODEL NLCW
C  094         PICEA GLAUCA                                 WHITE SPRUCE
         CASE ('09402')
           CW = 3.067563 + 1.944947*D**0.718583
C
C  CASE 09403 EK 1974
C  094         PICEA GLAUCA                                 WHITE SPRUCE
         CASE ('09403')
           IF (D .GE. OMIND) THEN
             CW = 3.5940 + 1.9630*D**0.8820
           ELSE
             CW = (3.5940 + 1.9630*OMIND**0.8820)*(D/OMIND)
           ENDIF
           IF (CW .GT. 37.) CW=37.
C-----------------------------------------------------------------------
C  CASE 09501 BECHTOLD 2003 MODEL 2
C  095         PICEA MARIANA                                BLACK SPRUCE
         CASE ('09501')
           IF (D .GE. MIND) THEN
             CW = -0.8566 + 0.9693*D + 0.0573*CR
           ELSE
             CW = (-0.8566 + 0.9693*MIND + 0.0573*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C
C  CASE 09502 BRAGG 2001 MODEL NLCW
C  095         PICEA MARIANA                                BLACK SPRUCE
         CASE ('09502')
           CW = 4.281343 + 0.153325*D**1.787982
C
C  CASE 09503 EK 1974
C  095         PICEA MARIANA                                BLACK SPRUCE
         CASE ('09503')
           IF (D .GE. OMIND) THEN
             CW = 3.6550 + 1.398*D**1.000
           ELSE
             CW = (3.6550 + 1.398*OMIND**1.000)*(D/OMIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C-----------------------------------------------------------------------
C  CASE 09701 BECHTOLD 2003 MODEL 3
C  097         PICEA RUBENS                                   RED SPRUCE
         CASE ('09701')
           IF (D .GE. MIND) THEN
             IF (D .LT. 30.) THEN
               CW = -1.2151 + 1.6098*D - 0.0277*D*D + 0.0674*CR
     &            - 0.0474*HI
             ELSE
               CW = -1.2151 + 1.6098*30 - 0.0277*30*30 + 0.0674*CR
     &               - 0.0474*HI
             ENDIF
           ELSE
             CW = (-1.2151 + 1.6098*MIND - 0.0277*MIND*MIND + 0.0674*CR
     &            - 0.0474*HI)*(D/MIND)
           ENDIF
C-----------------------------------------------------------------------
C  CASE 10001
C  100         PINUS SPP.                                      PINE SPP.
C         CASE ('10001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 10501 BECHTOLD 2003 MODEL 2
C  105         PINUS BANKSIANA                                 JACK PINE
         CASE ('10501')
           IF (D .GE. MIND) THEN
             CW = 0.7478 + 0.8712*D + 0.0913*CR
           ELSE
             CW = (0.7478 + 0.8712*MIND + 0.0913*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 25.) CW=25.
C
C  CASE 10502 BRAGG 2001 MODEL NLCW
C  105         PINUS BANKSIANA                                 JACK PINE
         CASE ('10502')
           CW = 3.382473 + 0.529126*D**1.269473
C
C  CASE 10503 EK 1974
C  105         PINUS BANKSIANA                                 JACK PINE
         CASE ('10503')
           IF (D .GE. OMIND) THEN
             CW = 0.2990 + 5.6440*D**0.6036
           ELSE
             CW = (0.2990 + 5.6440*OMIND**0.6036)*(D/OMIND)
           ENDIF
           IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 10701
C  107         PINUS CLAUSA                                    SAND PINE
C         CASE ('10701')
C           CW =
C-----------------------------------------------------------------------
C  CASE 11001 BECHTOLD 2003 MODEL 3
C  110         PINUS ECHINATA                             SHORTLEAF PINE
         CASE ('11001')
           IF (D .GE. MIND) THEN
             CW = -2.2564 + 1.3004*D + 0.1031*CR - 0.0562*HI
           ELSE
             CW = (-2.2564 + 1.3004*MIND + 0.1031*CR - 0.0562*HI)
     &             *(D/MIND)
           ENDIF
             IF (CW .GT. 34.) CW=34.
C
C  CASE 11005 SMITH AND OTHERS 1992
C  110         PINUS ECHINATA                             SHORTLEAF PINE
         CASE ('11005')
           IF (D .GE. OMIND) THEN
             CW = (0.5830 + 0.2450*(D*2.54) + 0.0009*(D*2.54)**2)
     &               *3.28084
           ELSE
             CW = ((0.5830 + 0.2450*(OMIND*2.54) + 0.0009*
     &             (OMIND*2.54)**2)*3.28084)*(D/OMIND)
           ENDIF
             IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 11101 BECHTOLD 2003 MODEL 3
C  111         PINUS ELLIOTTII                                SLASH PINE
         CASE ('11101')
           IF (D .GE. MIND) THEN
             IF (D .LT. 30.) THEN
               CW = -6.9659 + 2.1192*D - 0.0333*D*D + 0.0587*CR -
     &              0.0959*HI
             ELSE
               CW = -6.9659 + 2.1192*30 - 0.0333*30*30 + 0.0587*CR -
     &              0.0959*HI
             ENDIF
           ELSE
             CW = (-6.9659 + 2.1192*MIND - 0.0333*MIND*MIND + 0.0587*CR
     &             - 0.0959*HI)*(D/MIND)
           ENDIF
C-----------------------------------------------------------------------
C  CASE 11501
C  115         PINUS GLABRA                                  SPRUCE PINE
C         CASE ('11501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 12101 BECHTOLD 2003 MODEL 3
C  121         PINUS PALUSTRIS                             LONGLEAF PINE
         CASE ('12101')
           IF (D .GE. MIND) THEN
             CW = -12.2105 + 1.3376*D + 0.1237*CR - 0.2759*HI
           ELSE
             CW = (-12.2105 + 1.3376*MIND + 0.1237*CR - 0.2759*HI)
     &             *(D/MIND)
           ENDIF
             IF (CW .GT. 50.) CW=50.
C
C  CASE 12105 SMITH AND OTHERS 1992
C  121         PINUS PALUSTRIS                             LONGLEAF PINE
         CASE ('12105')
           IF (D .GE. OMIND) THEN
             CW = (0.113 + 0.259*(D*2.54))*3.28084
           ELSE
             CW = ((0.113 + 0.259*(OMIND*2.54))*3.28084)*(D/OMIND)
           ENDIF
             IF (CW .GT. 50.) CW=50.
C-----------------------------------------------------------------------
C  CASE 12201
C  122         PINUS PONDEROSA                            PONDEROSA PINE
C         CASE ('12201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 12301
C  123         PINUS PUNGENS                         TABLE MOUNTAIN PINE
C         CASE ('12301')
C           CW =
C-----------------------------------------------------------------------
C  CASE 12501 BECHTOLD 2003 MODEL 2
C  125         PINUS RESINOSA                                   RED PINE
         CASE ('12501')
           IF (D .GE. MIND) THEN
             IF (D .LT. 24.) THEN
               CW = -3.6548 + 1.9565*D - 0.0409*D*D + 0.0577*CR
             ELSE
               CW = -3.6548 + 1.9565*24. - 0.0409*24.*24. + 0.0577*CR
             ENDIF
           ELSE
             CW = (-3.6548 + 1.9565*MIND - 0.0409*MIND*MIND + 0.0577*CR)
     &             *(D/MIND)
           ENDIF
C
C  CASE 12502 BRAGG 2001 MODEL NLCW
C  125         PINUS RESINOSA                                   RED PINE
         CASE ('12502')
           CW = 3.499341 + 0.806186*D**1.090937
C
C  CASE 12503 EK 1974
C  125         PINUS RESINOSA                                   RED PINE
         CASE ('12503')
           IF (D .GE. OMIND) THEN
             CW = 4.2330 + 1.4620*D**1.0000
           ELSE
             CW = (4.2330 + 1.4620*OMIND**1.0000)
     &             *(D/OMIND)
           ENDIF
             IF (CW .GT. 39.) CW=39.
C-----------------------------------------------------------------------
C  CASE 12601 BECHTOLD 2003 MODEL 3
C  126         PINUS RIGIDA                                   PITCH PINE
         CASE ('12601')
           IF (D .GE. MIND) THEN
             CW = -0.9442 + 1.4531*D + 0.0543*CR - 0.1144*HI
           ELSE
             CW = (-0.9442 + 1.4531*MIND + 0.0543*CR - 0.1144*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 34.) CW=34.
C-----------------------------------------------------------------------
C  CASE 12801 BECHTOLD 2003 MODEL 1
C  128         PINUS SEROTINA                                  POND PINE
         CASE ('12801')
           IF (D .GE. MIND) THEN
             IF (D .LT. 18.) THEN
               CW = -8.7711 + 3.7252*D - 0.1063*D*D
             ELSE
               CW = -8.7711 + 3.7252*18 - 0.1063*18*18
             ENDIF
           ELSE
             CW = (-8.7711 + 3.7252*MIND - 0.1063*MIND*MIND)
     &             *(D/MIND)
           ENDIF
C-----------------------------------------------------------------------
C  CASE 12901 BECHTOLD 2003 MODEL 2
C  129         PINUS STROBUS                          EASTERN WHITE PINE
         CASE ('12901')
           IF (D .GE. MIND) THEN
             CW = 0.3914 + 0.9923*D + 0.1080*CR
           ELSE
             CW = (0.3914 + 0.9923*MIND + 0.1080*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 45.) CW=45.
C
C  CASE 12902 BRAGG 2001 MODEL NLCW
C  129         PINUS STROBUS                          EASTERN WHITE PINE
         CASE ('12902')
           CW = 3.874199 + 1.062309*D**0.969580
C
C  CASE 12903 EK 1974
C  129         PINUS STROBUS                          EASTERN WHITE PINE
         CASE ('12903')
           IF (D .GE. OMIND) THEN
             CW = 1.6200 + 3.1970*D**0.7981
           ELSE
             CW = (1.6200 + 3.1970*OMIND**0.7981)*(D/OMIND)
           ENDIF
           IF (CW .GT. 58.) CW=58.
C-----------------------------------------------------------------------
C  CASE 13001 BECHTOLD 2003 MODEL 2
C  130         PINUS SYLVESTRIS                              SCOTCH PINE
         CASE ('13001')
           IF (D .GE. MIND) THEN
             CW = 3.5522 + 0.6742*D + 0.0985*CR
           ELSE
             CW = (3.5522 + 0.6742*MIND + 0.0985*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C-----------------------------------------------------------------------
C  CASE 13101 BECHTOLD 2003 MODEL 2
C  131         PINUS TAEDA                                 LOBLOLLY PINE
         CASE ('13101')
           IF (D .GE. MIND) THEN
             CW = -0.8277 + 1.3946*D + 0.0768*CR
           ELSE
             CW = (-0.8277 + 1.3946*MIND + 0.0768*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 55.) CW=55.
C
C  CASE 13105 SMITH 1992
C  131         PINUS TAEDA                                 LOBLOLLY PINE
         CASE ('13105')
           IF (D .GE. OMIND) THEN
             CW = (0.7380 + 0.2450*(D*2.54) + 0.000809*(D*2.54)**2)
     &               *3.28084
           ELSE
             CW = ((0.7380 + 0.2450*(OMIND*2.54) + 0.000809*
     &             (OMIND*2.54)**2)*3.28084)*(D/OMIND)
           ENDIF
           IF (CW .GT. 66.) CW=66.
C-----------------------------------------------------------------------
C  CASE 13201 BECHTOLD 2003 MODEL 2
C  132         PINUS VIRGINIANA                            VIRGINIA PINE
         CASE ('13201')
           IF (D .GE. MIND) THEN
             CW = -0.1211 + 1.2319*D + 0.1212*CR
           ELSE
             CW = (-0.1211 + 1.2319*MIND + 0.1212*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 34.) CW=34.
C-----------------------------------------------------------------------
C  CASE 22101 BECHTOLD 2003 MODEL 2
C  221         TAXODIUM DISTICHUM                            BALDCYPRESS
         CASE ('22101')
           IF (D .GE. MIND) THEN
             CW = -1.0183 + 0.8856*D + 0.1162*CR
           ELSE
             CW = (-1.0183 + 0.8856*MIND + 0.1162*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 37.) CW=37.
C-----------------------------------------------------------------------
C  CASE 22201
C  222         TAXODIUM ASCENDENS                            PONDCYPRESS
C         CASE ('22201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 24101 BECHTOLD 2003 MODEL 2
C  241         THUJA OCCIDENTALIS                   NORTHERN WHITE-CEDAR
         CASE ('24101')
           IF (D .GE. MIND) THEN
             CW = -0.0634 + 0.7057*D + 0.0837*CR
           ELSE
             CW = (-0.0634 + 0.7057*MIND + 0.0837*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C
C  CASE 24102 BRAGG 2001 MODEL NLCW
C  241         THUJA OCCIDENTALIS                   NORTHERN WHITE-CEDAR
         CASE ('24102')
           CW = 2.123722 + 1.898797*D**0.764193
C
C  CASE 24103 EK 1974
C  241         THUJA OCCIDENTALIS                   NORTHERN WHITE-CEDAR
         CASE ('24103')
           IF (D .GE. OMIND) THEN
             CW = 3.4960 + 1.0930*D**1.0000
           ELSE
             CW = (3.4960 + 1.0930*OMIND**1.0000)*(D/OMIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C-----------------------------------------------------------------------
C  CASE 26001
C  260         TSUGA SPP.                                   HEMLOCK SPP.
C         CASE ('26001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 26101 BECHTOLD 2003 MODEL 3
C  261         TSUGA CANADENSIS                          EASTERN HEMLOCK
         CASE ('26101')
           IF (D .GE. MIND) THEN
             IF (D .LT. 40.) THEN
               CW = 6.1924 + 1.4491*D - 0.0178*D*D - 0.0341*HI
             ELSE
               CW = 6.1924 + 1.4491*40 - 0.0178*40*40 - 0.0341*HI
             ENDIF
           ELSE
             CW = (6.1924 + 1.4491*MIND - 0.0178*MIND*MIND - 0.0341*HI)
     &             *(D/MIND)
           ENDIF
C
C  CASE 26102 BRAGG 2001 MODEL NLCW
C  261         TSUGA CANADENSIS                          EASTERN HEMLOCK
         CASE ('26102')
           CW = 0.868672 + 4.526525*D**0.589487
C
C  CASE 26103 EK 1974
C  261         TSUGA CANADENSIS                          EASTERN HEMLOCK
         CASE ('26103')
           IF (D .GE. OMIND) THEN
             CW = 0.5230 + 1.6320*D**1.0000
           ELSE
             CW = (0.5230 + 1.6320*OMIND**1.0000)*(D/OMIND)
           ENDIF
           IF (CW .GT. 39.) CW=39.
C-----------------------------------------------------------------------
C  CASE 29801
C  298         ---                                   OTHER SOFTWOOD SPP.
C         CASE ('29801')
C           CW =
C-----------------------------------------------------------------------
C----------
C HARDWOODS
C----------
C-----------------------------------------------------------------------
C  CASE 31001
C  310         ACER SPP.                                      MAPLE SPP.
C         CASE ('31001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 31101
C  311         ACER BARBATUM                               FLORIDA MAPLE
C         CASE ('31101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 31301 BECHTOLD 2003 MODEL 3
C  313         ACER NEGUNDO                                     BOXELDER
         CASE ('31301')
           IF (D .GE. MIND) THEN
             CW = 6.4741 + 1.0778*D + 0.0719*CR - 0.0637*HI
           ELSE
             CW = (6.4741 + 1.0778*MIND + 0.0719*CR - 0.0637*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 57.) CW=57.
C-----------------------------------------------------------------------
C  CASE 31401
C  314         ACER NIGRUM                                   BLACK MAPLE
C         CASE ('31401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 31501
C  315         ACER PENSYLVANICUM                          STRIPED MAPLE
C         CASE ('31501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 31601 BECHTOLD 2003 MODEL 3
C  316         ACER RUBRUM                                     RED MAPLE
         CASE ('31601')
           IF (D .GE. MIND) THEN
             IF (D .LT. 50.) THEN
               CW = 2.7563 + 1.4212*D - 0.0143*D*D + 0.0993*CR
     &              - 0.0276*HI
             ELSE
               CW = 2.7563 + 1.4212*50 - 0.0143*50*50 + 0.0993*CR
     &              - 0.0276*HI
             ENDIF
           ELSE
             CW = (2.7563 + 1.4212*MIND - 0.0143*MIND*MIND + 0.0993*CR
     &              - 0.0276*HI)*(D/MIND)
           ENDIF
C
C  CASE 31602 BRAGG 2001 MODEL NLCW
C  316         ACER RUBRUM                                     RED MAPLE
         CASE ('31602')
           CW = 5.394872 + 1.844592*D**0.875755
C
C  CASE 31603 EK 1974
C  316         ACER RUBRUM                                     RED MAPLE
         CASE ('31603')
           IF (D .GE. OMIND) THEN
             CW = 0.00 + 4.7760*D**0.7656
           ELSE
             CW = (0.00 + 4.7760*OMIND**0.7656)*(D/OMIND)
           ENDIF
           IF (CW .GT. 55.) CW=55.
C-----------------------------------------------------------------------
C  CASE 31701 BECHTOLD 2003 MODEL 3
C  317         ACER SACCHARINUM                             SILVER MAPLE
         CASE ('31701')
           IF (D .GE. MIND) THEN
             CW = 3.3576 + 1.1312*D + 0.1011*CR - 0.1730*HI
           ELSE
             CW = (3.3576 + 1.1312*MIND + 0.1011*CR - 0.1730*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 31801 BECHTOLD 2003 MODEL 3
C  318         ACER SACCHARUM                                SUGAR MAPLE
         CASE ('31801')
           IF (D .GE. MIND) THEN
            CW = 4.9399 + 1.0727*D + 0.1096*CR - 0.0493*HI
           ELSE
             CW = (4.9399 + 1.0727*MIND + 0.1096*CR - 0.0493*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C
C  CASE 31802 BRAGG 2001 MODEL NLCW
C  318         ACER SACCHARUM                                SUGAR MAPLE
         CASE ('31802')
           CW = 5.588697 + 2.302860*D**0.828795
C
C  CASE 31803 EK 1974
C  318         ACER SACCHARUM                                SUGAR MAPLE
         CASE ('31803')
           IF (D .GE. OMIND) THEN
            CW = 0.8680 + 4.1500*D**0.7514
           ELSE
             CW = (0.8680 + 4.1500*OMIND**0.7514)*(D/OMIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C-----------------------------------------------------------------------
C  CASE 31901
C  319         ACER SPICATUM                              MOUNTAIN MAPLE
C         CASE ('31901')
C           CW =
C-----------------------------------------------------------------------
C  CASE 32301
C  323         ACER LEUCODERME                               CHALK MAPLE
C         CASE ('32301')
C           CW =
C-----------------------------------------------------------------------
C  CASE 33001
C  330         AESCULUS SPP.                  BUCKEYE/HORSECHESTNUT SPP.
C         CASE ('33001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 33101
C  331         AESCULUS GLABRA                              OHIO BUCKEYE
C         CASE ('33101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 33201
C  332         AESCULUS FLAVA                             YELLOW BUCKEYE
C         CASE ('33201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 34101
C  341         AILANTHUS ALTISSIMA                             AILANTHUS
C         CASE ('34101')
C           CW =
C-----------------------------------------------------------------------
C  CASE  BECHTOLD 2003 MODEL 1
C  356         AMELANCHIER SPP.                        SERVICEBERRY SPP.
         CASE ('35601')
           IF (D .GE. MIND) THEN
             CW = 6.9814 + 1.6032*D
           ELSE
             CW = (6.9814 + 1.6032*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 27.) CW=27.
C-----------------------------------------------------------------------
C  CASE 37001
C  370         BETULA SPP.                                    BIRCH SPP.
C         CASE ('37001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 37101 BECHTOLD 2003 MODEL 3
C  371         BETULA ALLEGHANIENSIS                        YELLOW BIRCH
         CASE ('37101')
           IF (D .GE. MIND) THEN
             IF (D .LT. 24.) THEN
               CW = -1.1151 + 2.2888*D - 0.0493*D*D + 0.0985*CR
     &              - 0.0396*HI
             ELSE
               CW = -1.1151 + 2.2888*24 - 0.0493*24*24 + 0.0985*CR
     &              - 0.0396*HI
             ENDIF
           ELSE
             CW = (-1.1151 + 2.2888*MIND - 0.0493*MIND*MIND + 0.0985*CR
     &              - 0.0396*HI)*(D/MIND)
           ENDIF
C
C  CASE 37102 BRAGG 2001 MODEL NLCW
C  371         BETULA ALLEGHANIENSIS                        YELLOW BIRCH
         CASE ('37102')
           CW = 2.374661 + 4.110366*D**0.677280
C-----------------------------------------------------------------------
C  CASE 37201 BECHTOLD 2003 MODEL 2
C  372         BETULA LENTA                                  SWEET BIRCH
         CASE ('37201')
           IF (D .GE. MIND) THEN
             CW = 4.6725 + 1.2968*D + 0.0787*CR
           ELSE
             CW = (4.6725 + 1.2968*MIND + 0.0787*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C-----------------------------------------------------------------------
C  CASE 37301 BECHTOLD 2003 MODEL 1
C  373         BETULA NIGRA                                  RIVER BIRCH
         CASE ('37301')
           IF (D .GE. MIND) THEN
             CW = 11.6634 + 1.0028*D
           ELSE
             CW = (11.6634 + 1.0028*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 68.) CW=68.
C-----------------------------------------------------------------------
C  CASE 37401
C  374         BETULA OCCIDENTALIS                           WATER BIRCH
C         CASE ('37401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 37501 BECHTOLD 2003 MODEL 3
C  375         BETULA PAPYRIFERA                             PAPER BIRCH
         CASE ('37501')
           IF (D .GE. MIND) THEN
             CW = 2.8399 + 1.2398*D + 0.0855*CR - 0.0282*HI
           ELSE
             CW = (2.8399 + 1.2398*MIND + 0.0855*CR - 0.0282*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 42.) CW=42.
C
C  CASE 37502 BRAGG 2001 MODEL NLCW
C  375         BETULA PAPYRIFERA                             PAPER BIRCH
         CASE ('37502')
           CW = 6.342475 + 0.552092*D**1.325344
C
C  CASE 37503 EK 1974
C  375         BETULA PAPYRIFERA                             PAPER BIRCH
         CASE ('37503')
           IF (D .GE. OMIND) THEN
             CW = 3.6390 + 1.9530*D**1.0000
           ELSE
             CW = (3.6390 + 1.9530*OMIND**1.0000)*(D/OMIND)
           ENDIF
           IF (CW .GT. 42.) CW=42.
C-----------------------------------------------------------------------
C  CASE 37901
C  379         BETULA POPULIFOLIA                             GRAY BIRCH
C         CASE ('37901')
C           CW =
C-----------------------------------------------------------------------
C  CASE 39101 BECHTOLD 2003 MODEL 3
C  391         CARPINUS CAROLINIANA                    AMERICAN HORNBEAM
         CASE ('39101')
           IF (D .GE. MIND) THEN
             CW = 0.9219 + 1.6303*D + 0.1150*CR - 0.1113*HI
           ELSE
             CW = (0.9219 + 1.6303*MIND + 0.1150*CR - 0.1113*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 42.) CW=42.
C-----------------------------------------------------------------------
C  CASE 40001
C  400         CARYA SPP.                                   HICKORY SPP.
C         CASE ('40001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 40101
C  401         CARYA AQUATICA                              WATER HICKORY
C         CASE ('40101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 40201 BECHTOLD 2003 MODEL 1
C  402         CARYA CORDIFORMIS                       BITTERNUT HICKORY
         CASE ('40201')
           IF (D .GE. MIND) THEN
             CW = 8.0118 + 1.4212*D
           ELSE
             CW = (8.0118 + 1.4212*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 41.) CW=41.
C-----------------------------------------------------------------------
C  CASE 40301 BECHTOLD 2003 MODEL 2
C  403         CARYA GLABRA                               PIGNUT HICKORY
         CASE ('40301')
           IF (D .GE. MIND) THEN
             CW = 3.9234 + 1.5220*D + 0.0405*CR
           ELSE
             CW = (3.9234 + 1.5220*MIND + 0.0405*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 53.) CW=53.
C-----------------------------------------------------------------------
C  CASE 40401
C  404         CARYA ILLINOINENSIS                                 PECAN
C         CASE ('40401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 40501
C  405         CARYA LACINIOSA                         SHELLBARK HICKORY
C         CASE ('40501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 40701 BECHTOLD 2003 MODEL 2
C  407         CARYA OVATA                              SHAGBARK HICKORY
         CASE ('40701')
           IF (D .GE. MIND) THEN
             CW = 4.5453 + 1.3721*D + 0.0430*CR
           ELSE
             CW = (4.5453 + 1.3721*MIND + 0.0430*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C
C  CASE 40703 EK 1974
C  407         CARYA OVATA                              SHAGBARK HICKORY
         CASE ('40703')
           IF (D .GE. OMIND) THEN
             CW = 2.3600 + 3.5480*D**0.7986
           ELSE
             CW = (2.3600 + 3.5480*OMIND**0.7986)*(D/OMIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C
C  CASE 40704 KRAJICEK 1961
C  407         CARYA OVATA                              SHAGBARK HICKORY
         CASE ('40704')
           IF (D .GE. OMIND) THEN
             CW = 1.9310 + 1.9990*D
           ELSE
             CW = (1.9310 + 1.9990*OMIND)*(D/OMIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C-----------------------------------------------------------------------
C  CASE 40801 BECHTOLD 2003 MODEL 1
C  408         CARYA TEXANA                                BLACK HICKORY
         CASE ('40801')
           IF (D .GE. MIND) THEN
             IF (D .LT. 15.) THEN
               CW = -5.8749 + 4.1555*D - 0.1343*D*D
             ELSE
               CW = -5.8749 + 4.1555*15 - 0.1343*15*15
             ENDIF
           ELSE
             CW = (-5.8749 + 4.1555*MIND - 0.1343*MIND*MIND)*(D/MIND)
           ENDIF
C-----------------------------------------------------------------------
C  CASE 40901 BECHTOLD 2003 MODEL 2
C  409         CARYA ALBA                              MOCKERNUT HICKORY
         CASE ('40901')
           IF (D .GE. MIND) THEN
             CW = 1.5838 + 1.6318*D + 0.0721*CR
           ELSE
             CW = (1.5838 + 1.6318*MIND + 0.0721*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 55.) CW=55.
C-----------------------------------------------------------------------
C  CASE 42101
C  421         CASTANEA DENTATA                        AMERICAN CHESTNUT
C         CASE ('42101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 45001
C  450         CATALPA SPP.                                 CATALPA SPP.
C         CASE ('45001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 46001
C  460         CELTIS SPP.                                HACKBERRY SPP.
C         CASE ('46001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 46101
C  461         CELTIS LAEVIGATA                               SUGARBERRY
C         CASE ('46101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 46201 BECHTOLD 2003 MODEL 2
C  462         CELTIS OCCIDENTALIS                             HACKBERRY
         CASE ('46201')
           IF (D .GE. MIND) THEN
             CW = 7.1043 + 1.3041*D + 0.0456*CR
           ELSE
             CW = (7.1043 + 1.3041*MIND + 0.0456*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 51.) CW=51.
C-----------------------------------------------------------------------
C  CASE 47101
C  471         CERCIS CANADENSIS                          EASTERN REDBUD
C         CASE ('47101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 49101 BECHTOLD 2003 MODEL 2
C  491         CORNUS FLORIDA                          FLOWERING DOGWOOD
         CASE ('49101')
           IF (D .GE. MIND) THEN
             CW = 2.9646 + 1.9917*D + 0.0707*CR
           ELSE
             CW = (2.9646 + 1.9917*MIND + 0.0707*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 36.) CW=36.
C-----------------------------------------------------------------------
C  CASE 50001
C  500         CRATAEGUS SPP.                              HAWTHORN SPP.
C         CASE ('50001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 52101 BECHTOLD 2003 MODEL 2
C  521         DIOSPYROS VIRGINIANA                     COMMON PERSIMMON
         CASE ('52101')
           IF (D .GE. MIND) THEN
             CW = 3.5393 + 1.3939*D + 0.0625*CR
           ELSE
             CW = (3.5393 + 1.3939*MIND + 0.0625*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 36.) CW=36.
C-----------------------------------------------------------------------
C  CASE 53101 BECHTOLD 2003 MODEL 3
C  531         FAGUS GRANDIFOLIA                          AMERICAN BEECH
         CASE ('53101')
           IF (D .GE. MIND) THEN
             CW = 3.9361 + 1.1500*D + 0.1237*CR - 0.0691*HI
           ELSE
             CW = (3.9361 + 1.1500*MIND + 0.1237*CR - 0.0691*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 80.) CW=80.
C-----------------------------------------------------------------------
C  CASE 54001
C  540         FRAXINUS SPP.                                    ASH SPP.
C         CASE ('54001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 54101 BECHTOLD 2003 MODEL 2
C  541         FRAXINUS AMERICANA                              WHITE ASH
         CASE ('54101')
          IF (D .GE. MIND) THEN
             CW = 1.7625 + 1.3413*D + 0.0957*CR
           ELSE
             CW = (1.7625 + 1.3413*MIND + 0.0957*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 62.) CW=62.
C
C  CASE 54102 BRAGG 2001 MODEL NLCW
C  541         FRAXINUS AMERICANA                              WHITE ASH
         CASE ('54102')
           CW = 5.715288 + 0.914942*D**1.113606
C
C  CASE 54103 EK 1974
C  541         FRAXINUS AMERICANA                              WHITE ASH
         CASE ('54103')
           IF (D .GE. OMIND) THEN
             CW = 2.3260 + 2.8390*D**1.0000
           ELSE
             CW = (2.3260 + 2.8390*OMIND**1.0000)*(D/OMIND)
           ENDIF
           IF (CW .GT. 62.) CW=62.
C-----------------------------------------------------------------------
C  CASE 54301 BECHTOLD 2003 MODEL 1
C  543         FRAXINUS NIGRA                                  BLACK ASH
         CASE ('54301')
           IF (D .GE. MIND) THEN
             CW = 5.2824 + 1.1184*D
           ELSE
             CW = (5.2824 + 1.1184*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 34.) CW=34.
C
C  CASE 54302 BRAGG 2001 MODEL NLCW
C  543         FRAXINUS NIGRA                                  BLACK ASH
         CASE ('54302')
           CW = 2.761995 + 2.560977*D**0.742525
C-----------------------------------------------------------------------
C  CASE 54401 BECHTOLD 2003 MODEL 2
C  544         FRAXINUS PENNSYLVANICA                          GREEN ASH
         CASE ('54401')
           IF (D .GE. MIND) THEN
             CW = 2.9672 + 1.3066*D + 0.0585*CR
           ELSE
             CW = (2.9672 + 1.3066*MIND + 0.0585*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 61.) CW=61.
C
C  CASE 54403 EK 1974
C  544         FRAXINUS PENNSYLVANICA                          GREEN ASH
         CASE ('54403')
           IF (D .GE. OMIND) THEN
             CW = 0.0000 + 4.7550*D**0.7381
           ELSE
             CW = (0.0000 + 4.7550*OMIND**0.7381)*(D/OMIND)
           ENDIF
           IF (CW .GT. 61.) CW=61.
C-----------------------------------------------------------------------
C  CASE 54501
C  545         FRAXINUS PROFUNDA                             PUMPKIN ASH
C         CASE ('54501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 54601
C  546         FRAXINUS QUADRANGULATA                           BLUE ASH
C         CASE ('54601')
C           CW =
C-----------------------------------------------------------------------
C  CASE 55201 BECHTOLD 2003 MODEL 2
C  552         GLEDITSIA TRIACANTHOS                         HONEYLOCUST
         CASE ('55201')
           IF (D .GE. MIND) THEN
             CW = 4.1971 + 1.5567*D + 0.0880*CR
           ELSE
             CW = (4.1971 + 1.5567*MIND + 0.0880*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 46.) CW=46.
C-----------------------------------------------------------------------
C  CASE 55501
C  555         GORDONIA LASIANTHUS                          LOBLOLLY BAY
C         CASE ('55501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 57101
C  571         GYMNOCLADUS DIOICUS                   KENTUCKY COFFEETREE
C         CASE ('57101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 58001
C  580         HALESIA SPP.                              SILVERBELL SPP.
C         CASE ('58001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 59101 BECHTOLD 2003 MODEL 2
C  591         ILEX OPACA                                 AMERICAN HOLLY
         CASE ('59101')
           IF (D .GE. MIND) THEN
             CW = 4.5803 + 1.0747*D + 0.0661*CR
           ELSE
             CW = (4.5803 + 1.0747*MIND + 0.0661*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 31.) CW=31.
C-----------------------------------------------------------------------
C  CASE 60101
C  601         JUGLANS CINEREA                                 BUTTERNUT
C         CASE ('60101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 60201 BECHTOLD 2003 MODEL 2
C  602         JUGLANS NIGRA                                BLACK WALNUT
         CASE ('60201')
           IF (D .GE. MIND) THEN
             CW = 3.6031 + 1.1472*D + 0.1224*CR
           ELSE
             CW = (3.6031 + 1.1472*MIND + 0.1224*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 37.) CW=37.
C
C  CASE 60203 EK 1974
C  602         JUGLANS NIGRA                                BLACK WALNUT
         CASE ('60203')
           IF (D .GE. OMIND) THEN
             CW = 4.901 + 2.480*D**1.0000
           ELSE
             CW = (4.901 + 2.480*OMIND**1.0000)*(D/OMIND)
           ENDIF
           IF (CW .GT. 37.) CW=37.
C-----------------------------------------------------------------------
C  CASE 61101 BECHTOLD 2003 MODEL 3
C  611         LIQUIDAMBAR STYRACIFLUA                          SWEETGUM
         CASE ('61101')
           IF (D .GE. MIND) THEN
             CW = 1.8853 + 1.1625*D + 0.0656*CR - 0.0300*HI
           ELSE
             CW = (1.8853 + 1.1625*MIND + 0.0656*CR - 0.0300*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 50.) CW=50.
C-----------------------------------------------------------------------
C  CASE 62101 BECHTOLD 2003 MODEL 2
C  621         LIRIODENDRON TULIPIFERA                     YELLOW-POPLAR
         CASE ('62101')
           IF (D .GE. MIND) THEN
             CW = 3.3543 + 1.1627*D + 0.0857*CR
           ELSE
             CW = (3.3543 + 1.1627*MIND + 0.0857*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 61.) CW=61.
C-----------------------------------------------------------------------
C  CASE 64101
C  641         MACLURA POMIFERA                             OSAGE-ORANGE
C         CASE ('64101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 65001
C  650         MAGNOLIA SPP.                               MAGNOLIA SPP.
C         CASE ('65001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 65101 BECHTOLD 2003 MODEL 1
C  651         MAGNOLIA ACUMINATA                           CUCUMBERTREE
         CASE ('65101')
           IF (D .GE. MIND) THEN
             CW = 4.1711 + 1.6275*D
           ELSE
             CW = (4.1711 + 1.6275*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 39.) CW=39.
C-----------------------------------------------------------------------
C  CASE 65201
C  652         MAGNOLIA GRANDIFLORA                    SOUTHERN MAGNOLIA
C         CASE ('65201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 65301 BECHTOLD 2003 MODEL 1
C  653         MAGNOLIA VIRGINIANA                              SWEETBAY
         CASE ('65301')
           IF (D .GE. MIND) THEN
             CW = 8.2119 + 0.9708*D
           ELSE
             CW = (8.2119 + 0.9708*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 41.) CW=41.
C-----------------------------------------------------------------------
C  CASE 65401
C  654         MAGNOLIA MACROPHYLLA                     BIGLEAF MAGNOLIA
C         CASE ('65401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 66001
C  660         MALUS SPP.                                     APPLE SPP.
C         CASE ('66001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 68001
C  680         MORUS SPP.                                  MULBERRY SPP.
C         CASE ('68001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 68101
C  681         MORUS ALBA                                 WHITE MULBERRY
C         CASE ('68101')
C           CW =
C-----------------------------------------------------------------------
C  CASE 68201 BECHTOLD 2003 MODEL 1
C  682         MORUS RUBRA                                  RED MULBERRY
         CASE ('68201')
           IF (D .GE. MIND) THEN
             CW = 13.3255 + 1.0735*D
           ELSE
             CW = (13.3255 + 1.0735*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 46.) CW=46.
C-----------------------------------------------------------------------
C  CASE 69001
C  690         NYSSA SPP.                                    TUPELO SPP.
C         CASE ('69001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 69101 BECHTOLD 2003 MODEL 2
C  691         NYSSA AQUATICA                               WATER TUPELO
         CASE ('69101')
           IF (D .GE. MIND) THEN
             CW = 5.3409 + 0.7499*D + 0.1047*CR
           ELSE
             CW = (5.3409 + 0.7499*MIND + 0.1047*CR)*(D/MIND)
           ENDIF
             IF (CW .GT. 37.) CW=37.
C-----------------------------------------------------------------------
C  CASE 69201
C  692         NYSSA OCECHE                              OGEECHEE TUPELO
C         CASE ('69201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 69301 BECHTOLD 2003 MODEL 3
C  693         NYSSA SYLVATICA                                  BLACKGUM
         CASE ('69301')
           IF (D .GE. MIND) THEN
             CW = 5.5037 + 1.0567*D + 0.0880*CR + 0.0610*HI
           ELSE
             CW = (5.5037 + 1.0567*MIND + 0.0880*CR + 0.0610*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 50.) CW=50.
C-----------------------------------------------------------------------
C  CASE 69401 BECHTOLD 2003 MODEL 2
C  694         NYSSA BIFLORA                                SWAMP TUPELO
         CASE ('69401')
           IF (D .GE. MIND) THEN
             CW = 1.3564 + 1.0991*D + 0.1243*CR
           ELSE
             CW = (1.3564 + 1.0991*MIND + 0.1243*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 41.) CW=41.
C-----------------------------------------------------------------------
C  CASE 70101 BECHTOLD 2003 MODEL 3
C  701         OSTRYA VIRGINIANA                     EASTERN HOPHORNBEAM
         CASE ('70101')
           IF (D .GE. MIND) THEN
             CW = 7.8084 + 0.8129*D + 0.0941*CR - 0.0817*HI
           ELSE
             CW = (7.8084 + 0.8129*MIND + 0.0941*CR - 0.0817*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 39.) CW=39.
C
C  CASE 70102 BRAGG 2001 MODEL NLCW
C  701         OSTRYA VIRGINIANA                     EASTERN HOPHORNBEAM
         CASE ('70102')
           CW = -33.898790 + 38.731332*D**0.152718
C-----------------------------------------------------------------------
C  CASE 71101 BECHTOLD 2003 MODEL 3
C  711         OXYDENDRUM ARBOREUM                              SOURWOOD
         CASE ('71101')
           IF (D .GE. MIND) THEN
             CW = 7.9750 + 0.8303*D + 0.0423*CR - 0.0706*HI
           ELSE
             CW = (7.9750 + 0.8303*MIND + 0.0423*CR - 0.0706*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 36.) CW=36.
C-----------------------------------------------------------------------
C  CASE 71201
C  712         PAULOWNIA TOMENTOSA               PAULOWNIA, EMPRESS-TREE
C         CASE ('71201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 72101 BECHTOLD 2003 MODEL 3
C  721         PERSEA BORBONIA                                    REDBAY
         CASE ('72101')
           IF (D .GE. MIND) THEN
             CW = 4.2756 + 1.0773*D + 0.1526*CR + 0.1650*HI
           ELSE
             CW = (4.2756 + 1.0773*MIND + 0.1526*CR + 0.1650*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 25.) CW=25.
C-----------------------------------------------------------------------
C  CASE 73101 BECHTOLD 2003 MODEL 2
C  731         PLATANUS OCCIDENTALIS                   AMERICAN SYCAMORE
         CASE ('73101')
           IF (D .GE. MIND) THEN
             CW = -1.3973 + 1.3756*D + 0.1835*CR
           ELSE
             CW = (-1.3973 + 1.3756*MIND + 0.1835*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 66.) CW=66.
C-----------------------------------------------------------------------
C  CASE 74001
C  740         POPULUS SPP.                   COTTONWOOD AND POPLAR SPP.
C         CASE ('74001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 74101 BECHTOLD 2003 MODEL 1
C  741         POPULUS BALSAMIFERA                         BALSAM POPLAR
         CASE ('74101')
           IF (D .GE. MIND) THEN
             CW = 6.2498 + 0.8655*D
           ELSE
             CW = (6.2498 + 0.8655*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 25.) CW=25.
C
C  CASE 74102 BRAGG 2001 MODEL NLCW
C  741         POPULUS BALSAMIFERA                         BALSAM POPLAR
         CASE ('74102')
           CW = 7.522796 + 0.125282*D**1.855258
C-----------------------------------------------------------------------
C  CASE 74201 BECHTOLD 2003 MODEL 1
C  742         POPULUS DELTOIDES                      EASTERN COTTONWOOD
         CASE ('74201')
           IF (D .GE. MIND) THEN
             CW = 3.4375 + 1.4092*D
           ELSE
             CW = (3.4375 + 1.4092*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 80.) CW=80.
C
C  CASE 74203 EK 1974
C  742         POPULUS DELTOIDES                      EASTERN COTTONWOOD
         CASE ('74203')
           IF (D .GE. OMIND) THEN
             CW = 2.934+ 2.538*D**0.8617
           ELSE
             CW = (2.934+ 2.538*OMIND**0.8617)*(D/OMIND)
           ENDIF
           IF (CW .GT. 80.) CW=80.
C-----------------------------------------------------------------------
C  CASE 74301 BECHTOLD 2003 MODEL 3
C  743         POPULUS GRANDIDENTATA                      BIGTOOTH ASPEN
         CASE ('74301')
           IF (D .GE. MIND) THEN
             CW = 0.6847 + 1.1050*D + 0.1420*CR - 0.0265*HI
           ELSE
             CW = (0.6847 + 1.1050*MIND + 0.1420*CR - 0.0265*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 43.) CW=43.
C
C  CASE 74302 BRAGG 2001 MODEL NLCW
C  743         POPULUS GRANDIDENTATA                      BIGTOOTH ASPEN
         CASE ('74302')
           CW = 4.031684 + 1.132992*D**1.024800
C
C  CASE 74303 EK 1974
C  743         POPULUS GRANDIDENTATA                      BIGTOOTH ASPEN
         CASE ('74303')
           IF (D .GE. OMIND) THEN
             CW = 0.0750 + 5.5770*D**0.5996
           ELSE
             CW = (0.0750 + 5.5770*OMIND**0.5996)*(D/OMIND)
           ENDIF
           IF (CW .GT. 43.) CW=43.
C-----------------------------------------------------------------------
C  CASE 74401
C  744         POPULUS HETEROPHYLLA                     SWAMP COTTONWOOD
C         CASE ('74401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 74601 BECHTOLD 2003 MODEL 2
C  746         POPULUS TREMULOIDES                         QUAKING ASPEN
         CASE ('74601')
           IF (D .GE. MIND) THEN
             CW = 0.7315 + 1.3180*D + 0.0966*CR
           ELSE
             CW = (0.7315 + 1.3180*MIND + 0.0966*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 39.) CW=39.
C
C  CASE 74602 BRAGG 2001 MODEL NLCW
C  746         POPULUS TREMULOIDES                         QUAKING ASPEN
         CASE ('74602')
           CW = 2.303376 + 2.371714*D**0.807622
C
C  CASE 74603 EK 1974
C  746         POPULUS TREMULOIDES                         QUAKING ASPEN
         CASE ('74603')
           IF (D .GE. OMIND) THEN
             CW = 4.203 + 2.129*D**1.0000
           ELSE
             CW = (4.203 + 2.129*OMIND**1.0000)*(D/OMIND)
           ENDIF
          IF (CW .GT. 43.) CW=43.
C-----------------------------------------------------------------------
C  CASE 76001
C  760         PRUNUS SPP.                          CHERRY AND PLUM SPP.
C         CASE ('76001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 76102 BRAGG 2001 MODEL NLCW
C  761         PRUNUS PENNSYLVANICA                           PIN CHERRY
         CASE ('76102')
           IF (D .GE. MIND) THEN
             CW = 4.102718 + 1.396006*D**1.077474
           ELSE
             CW = (4.102718 + 1.396006*MIND**1.077474)*(D/MIND)
           ENDIF
           IF (CW .GT. 52.) CW=52.
C-----------------------------------------------------------------------
C  CASE 76201 BECHTOLD 2003 MODEL 3
C  762         PRUNUS SEROTINA                              BLACK CHERRY
         CASE ('76201')
           IF (D .GE. MIND) THEN
             CW = 3.0237 + 1.1119*D + 0.1112*CR - 0.0493*HI
           ELSE
             CW = (3.0237 + 1.1119*MIND + 0.1112*CR - 0.0493*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 52.) CW=52.
C
C  CASE 76202 BRAGG 2001 MODEL NLCW
C  762         PRUNUS SEROTINA                              BLACK CHERRY
         CASE ('76202')
           CW = 1.304425 + 4.592688*D**0.526895
C
C  CASE 76203 EK 1974
C  762         PRUNUS SEROTINA                              BLACK CHERRY
         CASE ('76203')
           IF (D .GE. OMIND) THEN
             CW = 0.621 + 7.059*D**0.5441
           ELSE
             CW = (0.621 + 7.059*OMIND**0.5441)*(D/OMIND)
           ENDIF
         IF (CW .GT. 52.) CW=52.
C-----------------------------------------------------------------------
C  CASE 76301
C  763         PRUNUS VIRGINIANA                      COMMON CHOKECHERRY
C         CASE ('76301')
C           CW =
C-----------------------------------------------------------------------
C  CASE 80001
C  800         QUERCUS SPP.                                     OAK SPP.
C         CASE ('80001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 80201 BECHTOLD 2003 MODEL 3
C  802         QUERCUS ALBA                                    WHITE OAK
         CASE ('80201')
           IF (D .GE. MIND) THEN
             CW = 3.2375 + 1.5234*D + 0.0455*CR - 0.0324*HI
           ELSE
             CW = (3.2375 + 1.5234*MIND + 0.0455*CR - 0.0324*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 69.) CW=69.
C
C  CASE 80203 EK 1974
C  802         QUERCUS ALBA                                    WHITE OAK
         CASE ('80203')
           IF (D .GE. OMIND) THEN
             CW = 3.689 + 1.838*D**1.0000
           ELSE
             CW = (3.689 + 1.838*OMIND**1.0000)*(D/OMIND)
           ENDIF
          IF (CW .GT. 69.) CW=69.
C
C  CASE 80204 KRAJICEK 1961
C  802         QUERCUS ALBA                                    WHITE OAK
         CASE ('80204')
           IF (D .GE. OMIND) THEN
             CW = 1.8000 + 1.8830*D
           ELSE
             CW = (1.8000 + 1.8830*OMIND)*(D/OMIND)
           ENDIF
          IF (CW .GT. 69.) CW=69.
C-----------------------------------------------------------------------
C  CASE 80401
C  804         QUERCUS BICOLOR                           SWAMP WHITE OAK
C         CASE ('80401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 80601 BECHTOLD 2003 MODEL 2
C  806         QUERCUS COCCINEA                              SCARLET OAK
         CASE ('80601')
           IF (D .GE. MIND) THEN
             CW = 0.5656 + 1.6766*D + 0.0739*CR
           ELSE
             CW = (0.5656 + 1.6766*MIND + 0.0739*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 66.) CW=66.
C-----------------------------------------------------------------------
C  CASE 80901 BECHTOLD 2003 MODEL 1
C  809         QUERCUS ELLIPSOIDALIS                    NORTHERN PIN OAK
         CASE ('80901')
           IF (D .GE. MIND) THEN
             CW = 4.8935 + 1.6069*D
           ELSE
             CW = (4.8935 + 1.6069*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 44.) CW=44.
C-----------------------------------------------------------------------
C  CASE 81201 BECHTOLD 2003 MODEL 2
C  812         QUERCUS FALCATA                          SOUTHERN RED OAK
         CASE ('81201')
           IF (D .GE. MIND) THEN
             CW = 2.1517 + 1.6064*D + 0.0609*CR
           ELSE
             CW = (2.1517 + 1.6064*MIND + 0.0609*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 56.) CW=56.
C-----------------------------------------------------------------------
C  CASE 81301
C  813         QUERCUS PAGODA                             CHERRYBARK OAK
C         CASE ('81301')
C           CW =
C-----------------------------------------------------------------------
C  CASE 81701 BECHTOLD 2003 MODEL 1
C  817         QUERCUS IMBRICARIA                            SHINGLE OAK
         CASE ('81701')
           IF (D .GE. MIND) THEN
             CW = 9.8187 + 1.1343*D
           ELSE
             CW = (9.8187 + 1.1343*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C-----------------------------------------------------------------------
C  CASE 81901 BECHTOLD 2003 MODEL 1
C  819         QUERCUS LAEVIS                                 TURKEY OAK
         CASE ('81901')
           IF (D .GE. MIND) THEN
             CW = 5.8858 + 1.4935*D
           ELSE
             CW = (5.8858 + 1.4935*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 29.) CW=29.
C-----------------------------------------------------------------------
C  CASE 82001 BECHTOLD 2003 MODEL 1
C  820         QUERCUS LAURIFOLIA                             LAUREL OAK
         CASE ('82001')
           IF (D .GE. MIND) THEN
             CW = 6.3149 + 1.6455*D
           ELSE
             CW = (6.3149 + 1.6455*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 54.) CW=54.
C-----------------------------------------------------------------------
C  CASE 82201
C  822         QUERCUS LYRATA                                OVERCUP OAK
C         CASE ('82201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 82301 BECHTOLD 2003 MODEL 2
C  823         QUERCUS MACROCARPA                                BUR OAK
         CASE ('82301')
           IF (D .GE. MIND) THEN
             CW = 1.7827 + 1.6549*D + 0.0343*CR
           ELSE
             CW = (1.7827 + 1.6549*MIND + 0.0343*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 61.) CW=61.
C
C  CASE 82303 EK 1974
C  823         QUERCUS MACROCARPA                                BUR OAK
         CASE ('82303')
           IF (D .GE. OMIND) THEN
             CW = 0.942 + 3.539*D**0.7952
           ELSE
             CW = (0.942 + 3.539*OMIND**0.7952)*(D/OMIND)
           ENDIF
          IF (CW .GT. 78.) CW=78.
C-----------------------------------------------------------------------
C  CASE 82401 BECHTOLD 2003 MODEL 2
C  824         QUERCUS MARILANDICA                         BLACKJACK OAK
         CASE ('82401')
           IF (D .GE. MIND) THEN
             CW = 0.5443 + 1.4882*D + 0.0565*CR
           ELSE
             CW = (0.5443 + 1.4882*MIND + 0.0565*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 37.) CW=37.
C-----------------------------------------------------------------------
C  CASE 82501
C  825         QUERCUS MICHAUXII                       SWAMP CHESNUT OAK
C         CASE ('82501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 82601 BECHTOLD 2003 MODEL 3
C  826         QUERCUS MUEHLENBERGII                       CHINKAPIN OAK
         CASE ('82601')
           IF (D .GE. MIND) THEN
             CW = 0.5189 + 1.4134*D + 0.1365*CR - 0.0806*HI
           ELSE
             CW = (0.5189 + 1.4134*MIND + 0.1365*CR - 0.0806*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 82701 BECHTOLD 2003 MODEL 3
C  827         QUERCUS NIGRA                                   WATER OAK
         CASE ('82701')
           IF (D .GE. MIND) THEN
             CW = 1.6349 + 1.5443*D + 0.0637*CR - 0.0764*HI
           ELSE
             CW = (1.6349 + 1.5443*MIND + 0.0637*CR - 0.0764*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 57.) CW=57.
C-----------------------------------------------------------------------
C  CASE 82801
C  828         QUERCUS BUCKLEYI                              NUTTALL OAK
C         CASE ('82801')
C           CW =
C-----------------------------------------------------------------------
C  CASE 83001 BECHTOLD 2003 MODEL 3
C  830         QUERCUS PALUSTRIS                                 PIN OAK
         CASE ('83001')
           IF (D .GE. MIND) THEN
             CW = -5.6268 + 1.7808*D + 0.1231*CR + 0.1578*HI
           ELSE
             CW = (-5.6268 + 1.7808*MIND + 0.1231*CR + 0.1578*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 63.) CW=63.
C-----------------------------------------------------------------------
C  CASE 83101 BECHTOLD 2003 MODEL 2
C  831         QUERCUS PHELLOS                                WILLOW OAK
         CASE ('83101')
           IF (D .GE. MIND) THEN
             CW = 1.6477 + 1.3672*D + 0.0846*CR
           ELSE
             CW = (1.6477 + 1.3672*MIND + 0.0846*CR)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 74.) CW=74.
C-----------------------------------------------------------------------
C  CASE 83201 BECHTOLD 2003 MODEL 2
C  832         QUERCUS PRINUS                                CHESNUT OAK
         CASE ('83201')
           IF (D .GE. MIND) THEN
             IF (D .LT. 50.) THEN
               CW = 2.1480 + 1.6928*D - 0.0176*D*D + 0.0569*CR
             ELSE
               CW = 2.1480 + 1.6928*50 - 0.0176*50*50 + 0.0569*CR
             ENDIF
           ELSE
             CW = (2.1480 + 1.6928*MIND - 0.0176*MIND*MIND + 0.0569*CR)
     &             *(D/MIND)
           ENDIF
C-----------------------------------------------------------------------
C  CASE 83301 BECHTOLD 2003 MODEL 2
C  833         QUERCUS RUBRA                            NORTHERN RED OAK
         CASE ('83301')
           IF (D .GE. MIND) THEN
             CW = 2.8908 + 1.4077*D + 0.0643*CR
           ELSE
             CW = (2.8908 + 1.4077*MIND + 0.0643*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 82.) CW=82.
C
C  CASE 83302 BRAGG 2001 MODEL NLCW
C  833         QUERCUS RUBRA                            NORTHERN RED OAK
         CASE ('83302')
           CW = 2.280575 + 2.679718*D**0.830741
C
C  CASE 83303 EK 1974
C  833         QUERCUS RUBRA                            NORTHERN RED OAK
         CASE ('83303')
           IF (D .GE. OMIND) THEN
             CW = 2.8500 + 3.7820*D**0.7968
           ELSE
             CW = (2.850 + 3.782*OMIND**0.7968)*(D/OMIND)
           ENDIF
          IF (CW .GT. 82.) CW=82.
C
C  CASE 83304 KRAJICEK 1961
C  833         QUERCUS RUBRA                            NORTHERN RED OAK
         CASE ('83304')
           IF (D .GE. OMIND) THEN
             CW = 4.5100 + 1.6700*D
           ELSE
             CW = (4.5100 + 1.6700*OMIND)*(D/OMIND)
           ENDIF
          IF (CW .GT. 82.) CW=82.
C-----------------------------------------------------------------------
C  CASE 83401
C  834         QUERCUS SHUMARDII                            SHUMARDS OAK
C         CASE ('83401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 83501 BECHTOLD 2003 MODEL 2
C  835         QUERCUS STELLATA                                 POST OAK
         CASE ('83501')
           IF (D .GE. MIND) THEN
             CW = 1.6125 + 1.6669*D + 0.0536*CR
           ELSE
             CW = (1.6125 + 1.6669*MIND + 0.0536*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 83601
C  836         QUERCUS SIMILIS                            DELTA POST OAK
C         CASE ('83601')
C           CW =
C-----------------------------------------------------------------------
C  CASE 83701 BECHTOLD 2003 MODEL 2
C  837         QUERCUS VELUTINA                                BLACK OAK
         CASE ('83701')
           IF (D .GE. MIND) THEN
             CW = 2.8974 + 1.3697*D + 0.0671*CR
           ELSE
             CW = (2.8974 + 1.3697*MIND + 0.0671*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 52.) CW=52.
C
C  CASE 83703 EK 1974
C  837         QUERCUS VELUTINA                                BLACK OAK
         CASE ('83703')
           IF (D .GE. OMIND) THEN
             CW = 4.5040 + 2.4170*D**1.0000
           ELSE
             CW = (4.5040 + 2.4170*OMIND**1.0000)*(D/OMIND)
           ENDIF
          IF (CW .GT. 52.) CW=52.
C
C  CASE 83704 KRAJICEK 1961
C  837         QUERCUS VELUTINA                                BLACK OAK
         CASE ('83704')
           IF (D .GE. OMIND) THEN
             CW = 4.5100 + 1.6700*D
           ELSE
             CW = (4.5100 + 1.6700*OMIND)*(D/OMIND)
           ENDIF
          IF (CW .GT. 52.) CW=52.
C-----------------------------------------------------------------------
C  CASE 83801 BECHTOLD 2003 MODEL 1
C  838         QUERCUS VIRGINIANA                               LIVE OAK
         CASE ('83801')
           IF (D .GE. MIND) THEN
             CW = 5.6694 + 1.6402*D
           ELSE
             CW = (5.6694 + 1.6402*MIND)*(D/MIND)
           ENDIF
           IF (CW .GT. 66.) CW=66.
C-----------------------------------------------------------------------
C  CASE 84001
C  840         QUERCUS MARGARETTIAE                       DWARF POST OAK
C         CASE ('84001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 84201
C  842         QUERCUS INCANA                               BLUEJACK OAK
C         CASE ('84201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 90101 BECHTOLD 2003 MODEL 2
C  901         ROBINIA PSEUDOACACIA                         BLACK LOCUST
         CASE ('90101')
           IF (D .GE. MIND) THEN
             CW = 3.0012 + 0.8165*D + 0.1395*CR
           ELSE
             CW = (3.0012 + 0.8165*MIND + 0.1395*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 48.) CW=48.
C-----------------------------------------------------------------------
C  CASE 92001
C  920         SALIX SPP.                                    WILLOW SPP.
C         CASE ('92001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 92201
C  922         SALIX NIGRA                                  BLACK WILLOW
C         CASE ('92201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 92301
C  923         SALIX BEBBIANA                                BEBB WILLOW
C         CASE ('92301')
C           CW =
C-----------------------------------------------------------------------
C  CASE 93101 BECHTOLD 2003 MODEL 2
C  931         SASSAFRAS ALBIDUM                               SASSAFRAS
         CASE ('93101')
           IF (D .GE. MIND) THEN
             CW = 4.6311 + 1.0108*D + 0.0564*CR
           ELSE
             CW = (4.6311 + 1.0108*MIND + 0.0564*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 29.) CW=29.
C-----------------------------------------------------------------------
C  CASE 93501
C  935         SORBUS AMERICANA                    AMERICAN MOUNTAIN ASH
C         CASE ('93501')
C           CW =
C-----------------------------------------------------------------------
C  CASE 95001
C  950         TILIA SPP.                                  BASSWOOD SPP.
C         CASE ('95001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 95101 BECHTOLD 2003 MODEL 3
C  951         TILIA AMERICANA                         AMERICAN BASSWOOD
         CASE ('95101')
           IF (D .GE. MIND) THEN
             CW = 1.6871 + 1.2110*D + 0.1194*CR - 0.0264*HI
           ELSE
             CW = (1.6871 + 1.2110*MIND + 0.1194*CR - 0.0264*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 61.) CW=61.
C
C  CASE 95102 BRAGG 2001 MODEL NLCW
C  951         TILIA AMERICANA                         AMERICAN BASSWOOD
         CASE ('95102')
           CW = 7.172413 + 0.662556*D**1.127814
C
C  CASE 95103 EK 1974
C  951         TILIA AMERICANA                         AMERICAN BASSWOOD
         CASE ('95103')
           IF (D .GE. OMIND) THEN
             CW = 0.1350 + 3.7030*D**0.7307
           ELSE
             CW = (0.1350 + 3.7030*OMIND**0.7307)*(D/OMIND)
           ENDIF
           IF (CW .GT. 61.) CW=61.
C-----------------------------------------------------------------------
C  CASE 95201
C  952         TILIA AMERICANA VAR. HETEROPHYLLA          WHITE BASSWOOD
C         CASE ('95201')
C           CW =
C-----------------------------------------------------------------------
C  CASE 97001
C  970         ULMUS SPP.                                       ELM SPP.
C         CASE ('97001')
C           CW =
C-----------------------------------------------------------------------
C  CASE 97101 BECHTOLD 2003 MODEL 2
C  971         ULMUS ALATA                                    WINGED ELM
         CASE ('97101')
           IF (D .GE. MIND) THEN
             CW = 4.3649 + 1.6612*D + 0.0643*CR
           ELSE
             CW = (4.3649 + 1.6612*MIND + 0.0643*CR)*(D/MIND)
           ENDIF
           IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 97201 BECHTOLD 2003 MODEL 3
C  972         ULMUS AMERICANA                              AMERICAN ELM
         CASE ('97201')
           IF (D .GE. MIND) THEN
             CW = 1.7296 + 2.0732*D + 0.0590*CR - 0.0869*HI
           ELSE
             CW = (1.7296 + 2.0732*MIND + 0.0590*CR - 0.0869*HI)
     &             *(D/MIND)
           ENDIF
           IF (CW .GT. 50.) CW=50.
C
C  CASE 97202 BRAGG 2001 MODEL NLCW
C  972         ULMUS AMERICANA                              AMERICAN ELM
         CASE ('97202')
           CW = -53.239079 + 61.327257*D**0.060166
C
C  CASE 97203 EK 1974
C  972         ULMUS AMERICANA                              AMERICAN ELM
         CASE ('97203')
           IF (D .GE. OMIND) THEN
             CW = 2.8290 + 3.4560*D**0.8575
           ELSE
             CW = (2.8290 + 3.4560*OMIND**0.8575)*(D/OMIND)
           ENDIF
           IF (CW .GT. 72.) CW=72.
C-----------------------------------------------------------------------
C  CASE 97401
C  974         ULMUS PUMILA                                 SIBERIAN ELM
C         CASE ('97401')
C           CW =
C-----------------------------------------------------------------------
C  CASE 97501 BECHTOLD 2003 MODEL 3
C  975         ULMUS RUBRA                                  SLIPPERY ELM
         CASE ('97501')
           IF (D .GE. MIND) THEN
             CW = 9.0023 + 1.3933*D - 0.0785*HI
           ELSE
             CW = (9.0023 + 1.3933*MIND - 0.0785*HI)*(D/MIND)
           ENDIF
           IF (CW .GT. 49.) CW=49.
C-----------------------------------------------------------------------
C  CASE 97701
C  977         ULMUS THOMASII                                   ROCK ELM
C         CASE ('97701')
C           CW =
C-----------------------------------------------------------------------
C  CASE 99801
C  998         ---                                   OTHER HARDWOOD SPP.
C         CASE ('99801')
C           CW =
C-----------------------------------------------------------------------
C  CASE 99901
C  999         ---                                  OTHER / UNKNOWN SPP.
C         CASE ('99901')
C           CW =
C-----------------------------------------------------------------------
C
      END SELECT
      IF (CW .LT. 0.5) CW=0.5
C----------
C  LIMIT CROWN WIDTH FOR PRINTING ON TREELIST.
C----------
      IF(CW .GT. 99.9) CW=99.9
C
      RETURN
      END