      SUBROUTINE CWCALC(ISPC,P,D,H,CR,IICR,CW,IWHO,JOSTND)
      IMPLICIT NONE
C----------
C NI $Id: cwcalc.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS
C----------
C  THIS ROUTINE CONTAINS A LIBRARY OF CROWN WIDTH EQUATIONS AVAILABLE
C  FOR USE IN THE WESTERN UNITED STATES.
C  EQUATIONS ARE GROUPED BY SPECIES ACCORDING TO THEIR FIA CODE.
C  THIS ROUTINE COMPUTES LARGEST CROWN WIDTH.
C  IT IS CALLED FROM **CWIDTH** TO PRODUCE CROWTH WIDTH ESTIMATES FOR
C  OF FOREST GROWN TREES.
C
C  DEFINITION OF VARIABLES:
C        CW = LARGEST CROWN WIDTH
C      IWHO = 0 IF CALLED FROM CWIDTH
C      ISPC = FVS SPECIES SEQUENCE NUMBER
C         P = TREES PER ACRE
C         D = TREE DBH
C        CR = CROWN RATIO IN PERCENT (REAL)
C      IICR = CROWN RATIO IN PERCENT (INTEGER))
C        CL = CROWN LENGTH
C         H = TREE HEIGHT
C     BAREA = BASAL AREA
C     HILAT = LATITUDE IN DECIMAL DEGREES
C    HILONG = LONGITUDE (-) IN DECIMAL DEGREES
C    HIELEV = ELEVATION IN FEET
C        HI = HOPKINS BIOCLIMATIC INDEX, SEE BECHTOLD 2003.
C        EL = ELEVATION IN 100's OF FEET
C       MIND= MIN. DBH FOR BECHTOLD EQN.
C      OMIND= MIN. DBH FOR OTHER AUTHORS EQN.
C      CWEQ = CW EQUATION NUMBER (FIA # + EQN #)
C               BECHTOLD MODEL 1 EQN#      = 01
C               BECHTOLD MODEL 2 EQN#      = 02
C               CROOKSTON(R1) EQN#         = 03
C               CROOKSTON(R6) MODEL 1 EQN# = 04
C               CROOKSTON(R6) MODEL 2 EQN# = 05
C               DONNELLY EQN#              = 06
C               MOEUR EQN#                 = 07
C
C  SOURCES OF FOREST GROWN CROWN WIDTH EQUATIONS:
C  BECHTOLD, WILLIAM A. 2004. LARGEST-CROWN-DIAMETER PREDICTION MODELS FOR
C     53 SPECIES IN THE WESTERN UNITED STATES. WJAF. 19(4):245-251.
C  CROOKSTON, NICHOLAS 2005. DRAFT: ALLOMETRIC CROWN WIDTH EQUATIONS FOR 34
C     NORTHWEST UNITED STATES TREE SPECIES ESTIMATED USING GENERALIZED LINEAR
C     MIXED EFFECTS MODELS.
C  CROOKSTON, NICHOLAS 2003. INTERNAL DOCUMENT ON FILE, MOSCOW IDAHO. DATA
C     PROVIDED FROM REGION 1.
C  DONNELLY, DENNIS 1996. INTERNAL DOCUMENT ON FILE, FORT COLLINS, CO. DATA
C     PROVIDED FROM REGION 6.
C  MOEUR, MELINDA 1981. CROWN WIDTH AND FOLIAGE WEIGHT OF NORTHERN
C     ROCKY MOUNTAIN CONIFERS. USDA-FS, INT-183.
C----------
      LOGICAL DEBUG
      CHARACTER CWEQN*5, VVER*7,FIASP*3
      CHARACTER AKMAP(13)*5, BMMAP(18)*5, CAMAP(49)*5, CIMAP(19)*5
      CHARACTER CRMAP(38)*5, ECMAP(32)*5, EMMAP(19)*5, IEMAP(23)*5
      CHARACTER KTMAP(11)*5, NCMAP(11)*5, NIMAP(11)*5, PNMAP(39)*5
      CHARACTER SOMAP(33)*5, TTMAP(18)*5, UTMAP(24)*5, WCMAP(39)*5
      CHARACTER WSMAP(43)*5, OCMAP(49)*5, OPMAP(39)*5
      INTEGER ISPC,IICR, IWHO, JOSTND
      INTEGER ICYC
      REAL D, H, CW, HI, HILAT,HILONG,HIELEV,EL,MIND,CR,CL,BAREA
      REAL BF,P,OMIND
      REAL RDANUW
      INTEGER IDANUW
C----------
C  DATA STATEMENTS
C----------
      DATA MIND/5./,OMIND/1./
C----------
C  MAP EQUATION NUMBERS FOR VARIANT
C----------
C----------
C  SOUTHEAST ALASKA
C----------
C                      WS       RC       SF       MH       WH       YC
       DATA AKMAP/ '09305', '24205', '01105', '26403', '26305', '04205',
C             LP       SS       AF       RA       CW   OHtoCW   OStoWS
     &    '10805', '09805', '01905', '35106', '74705', '74705', '09305'/
C----------
C  BLUE MOUNTAINS
C----------
C                      WP       WL       DF       GF       MH       WJ
       DATA BMMAP/ '11905', '07303', '20205', '01703', '26403', '06405',
C                      LP       ES       AF       PP       WB       LM
     &             '10805', '09305', '01905', '12205', '10105', '11301',
C                      PY       YC       AS       CW       OS       OH
     &             '23104', '04205', '74605', '74705', '12205', '31206'/
C----------
C  INLAND CALIFORNIA
C----------
C                      PC       IC       RC       WF       RF      SH
       DATA CAMAP/ '04105', '08105', '24205', '01505', '02006', '02105',
C             DF       WH       MH       WB       KP       LP   CPtoLP
     &    '20205', '26305', '26403', '10105', '10305', '10805', '10805',
C             LM       JP       SP       WP       PP   MPtoGP       GP
     &    '11301', '11605', '11705', '11905', '12205', '12702', '12702',
C             WJ       BR   GStoRW       PY   OStoJP       LO       CY
     &    '06405', '09204', '21104', '23104', '11605', '80102', '80502',
C             BL   EOtoBL       WO       BO       VO       IO       BM
     &    '80702', '80702', '81505', '81802', '82102', '83902', '31206',
C         BUtoBM       RA       MA   GCtoTO   DGtoRA   FLtoBM   WNtoBM
     &    '31206', '35106', '36102', '63102', '35106', '31206', '31206',
C             TO   SYtoTO       AS       CW   WItoBM   CNtoCL       CL
     &    '63102', '63102', '74605', '74705', '31206', '98102', '98102',
C             OH
     &    '31206'/
C----------
C  CENTRAL IDAHO
C----------
       DATA CIMAP/
C            WP       WL       DF       GF       WH  
     &   '11903',  '07303', '20203', '01703', '26305',
C            RC       LP       ES       AF       PP 
     &   '24203',  '10803', '09303', '01905', '12203',
C            WB       PY       AS       WJ       MC 
     &   '10105',  '23104', '74605', '06405', '47502',
C            LM       CW       OS       OH 
     &   '11301',  '74902', '12205', '74902'/
C----------
C  CENTRAL ROCKIES
C----------
C                      AF       CB       DF       GF       WF       MH
       DATA CRMAP/ '01905', '01801', '20205', '01703', '01505', '26403',
C             RC       WL       BC       LM       LP       PI       PP
     &    '24205', '07303', '10201', '11301', '10805', '10602', '12205',
C             WB       SW       UJ       BS       ES       WS       AS
     &    '10105', '11905', '06602', '09305', '09305', '09305', '74605',
C             NC       PW       GO       AW       EM       BK       SO
     &    '74902', '74902', '81402', '81402', '81402', '81402', '81402',
C             PB       AJ       RM       OJ       ER       PM       PD
     &    '74605', '06602', '06602', '06602', '06602', '10602', '10602',
C             AZ       CI       OS       OH
     &    '10602', '12205', '12205', '74902'/
C----------
C  EAST CASCADES
C----------
       DATA ECMAP/
C             WP       WL       DF       SF       RC 
     &    '11905', '07303', '20205', '01105', '24205',
C             GF       LP       ES       AF       PP
     &    '01703', '10805', '09305', '01905', '12205',
C             WH       MH       PY       WB       NF
     &    '26305', '26403', '23104', '10105', '02206',
C             WF       LL       YC       WJ       BM
     &    '01505', '07204', '04205', '06405', '31206',
C             VN       RA       PB   GCtoTO   DGtoRA
     &    '32102', '35106', '37506', '63102', '35106',
C             AS       CW       WO   PLtoRA   WItoBM
     &    '74605', '74705', '81505', '35106', '31206',
C             OS       OH 
     &    '26403', '74605'/
C----------
C  EASTERN MONTANA
C----------
C                      WB       WL       DF       LM       LL       RM
       DATA EMMAP/ '10105', '07303', '20203', '11301', '07204', '06602',
C             LP       ES       AF       PP       GA       AS       CW
     &    '10803', '09303', '01903', '12203', '74902', '74605', '74705',
C             BA       PW       NC       PB    OS(MH)      OH
     &    '74902', '74902', '74902', '37506', '26405', '74902'/
C----------
C  INLAND EMPIRE (23)
C----------
C                      WP       WL       DF       GF       WH       RC
       DATA IEMAP/ '11903', '07303', '20203', '01703', '26303', '24203',
C             LP       ES       AF       PP      MH        WB       LM
     &    '10803', '09303', '01903', '12203', '26405', '10105', '11301',
C             LL       PI       RM       PY       AS       CO       MM
     &    '07204', '10602', '06602', '23104', '74605', '74902', '32102',
C             PB       OH       OS
     &    '37506', '74902', '12205'/
C----------
C  NORTHERN IDAHO (11)
C----------
C                      WP       WL       DF       GF       WH       RC
       DATA NIMAP/ '11903', '07303', '20203', '01703', '26303', '24203',
C             LP       ES       AF       PP       OT(MH)
     &    '10803', '09303', '01903', '12203', '26405'/
C----------
C  KOOKANTL
C----------
C                      WP       WL       DF       GF       WH       RC
       DATA KTMAP/ '11903', '07303', '20203', '01703', '26303', '24203',
C             LP       ES       AF       PP       OT(MH)
     &    '10803', '09303', '01903', '12203', '26405'/
C----------
C  KLAMATH MOUNTAINS
C----------
C                      OC       SP       DF       WF       MA       IC
       DATA NCMAP/ '12205', '11705', '20205', '01505', '36102', '08105',
C             BO       TO       RF       PP       OH
     &    '81802', '63102', '02006', '12205', '81802'/
C----------
C  PACIFIC COAST
C----------
C                      SF       WF       GF       AF       RF      SS
       DATA PNMAP/ '01105', '01505', '01703', '01905', '02006', '09805',
C             NF       YC       IC       ES       LP       JP       SP
     &    '02206', '04205', '08105', '09305', '10805', '11605', '11705',
C             WP       PP       DF       RW       RC       WH       MH
     &    '11905', '12205', '20205', '21104', '24205', '26305', '26403',
C             BM       RA   WAtoBM       PB   GCtoTO       AS       CW
     &    '31206', '35106', '31206', '37506', '63102', '74605', '74705',
C             WO        J       LL       WB       KP       PY   DGtoRA
     &    '81505', '06405', '07204', '10105', '10305', '23104', '35106',
C         HTtoRA   CHtoRA   WItoBM       --       OT
     &    '35106', '35106', '31206', '12205', '12205'/
C----------
C  SOUTHERN OREGON/ NORTHERN CALIF
C----------
C                      WP       SP       DF       WF       MH       IC
       DATA SOMAP/ '11905', '11705', '20205', '01505', '26403', '08105',
C             LP       ES       SH       PP       WJ       GF       AF
     &    '10805', '09305', '02105', '12205', '06405', '01703', '01905',
C             SF       NF       WB       WL       RC       WH       PY
     &    '01105', '02206', '10105', '07303', '24205', '26305', '23104',
C         WAtoBM       RA       BM       AS       CW   CHtoRA       WO
     &    '31206', '35106', '31206', '74605', '74705', '35106', '81505',
C         WItoBM   GCtoTO       MC   MBtoMC       OS       OH
     &    '31206', '63102', '47502', '47502', '12205', '31206'/
C----------
C  TETONS
C----------
C                      WB       LM       DF       PM       BS       AS
       DATA TTMAP/ '10105', '11301', '20205', '10602', '09305', '74605',
C             LP       ES       AF       PP       UJ       RM       BI
     &    '10805', '09305', '01905', '12203', '06405', '06405', '31206',
C             MM       NC       MC       OS       OH
     &    '32102', '74902', '47502', '12205', '74902'/
C----------
C  UTAH
C----------
C                      WB       LM       DF       WF       BS       AS
       DATA UTMAP/ '10105', '11301', '20205', '01505', '09305', '74605',
C             LP       ES       AF       PP       PI       WJ       GO
     &    '10805', '09305', '01905', '12205', '10602', '06405', '81402',
C             PM       RM       UJ       GB       NC       FC       MC
     &    '10602', '06405', '06405', '10201', '74902', '74902', '47502',
C             BI       BE       OS       OH
     &    '31206', '74902', '12205', '81402'/
C----------
C  WEST CASCADES
C----------
C                      SF       WF       GF       AF       RF      SS
       DATA WCMAP/ '01105', '01505', '01703', '01905', '02006', '09805',
C             NF       YC       IC       ES       LP       JP       SP
     &    '02206', '04205', '08105', '09305', '10805', '11605', '11705',
C             WP       PP       DF       RW       RC       WH       MH
     &    '11905', '12205', '20205', '21104', '24205', '26305', '26403',
C             BM       RA   WAtoBM       PB   GCtoTO       AS       CW
     &    '31206', '35106', '31206', '37506', '63102', '74605', '74705',
C             WO       WJ       LL       WB       KP       PY   DGtoRA
     &    '81505', '06405', '07204', '10105', '10305', '23104', '35106',
C         HTtoRA   CHtoRA   WItoBM       --       OT
     &    '35106', '35106', '31206', '12205', '12205'/
C----------
C  WESTERN SIERRAS
C----------
       DATA WSMAP/
C          SP       DF       WF       GS       IC
     & '11705', '20205', '01505', '21104', '08105',   
C          JP       RF       PP       LP       WB
     & '11605', '02006', '12205', '10805', '10105',   
C          WP       PM       SF       KP       FP
     & '11905', '10602', '01105', '10305', '10305',   
C          CP       LM       MP       GP       WE
     & '10805', '11301', '12205', '12702', '10305',   
C          GB       BD       RW       MH       WJ
     & '10201', '20205', '21104', '26403', '06405',   
C          UJ       CJ       LO       CY       BL
     & '06405', '06405', '80102', '80502', '80702',   
C          BO       VO       IO       TO       GC
     & '81802', '82102', '83902', '63102', '63102',   
C          AS       CL       MA       DG       BM
     & '74605', '98102', '36102', '35106', '31206',   
C          MC       OS       OH
     & '47502', '12205', '81802'/  
C----------
C  FVS-ORGANON SWO  (OC VARIANT)
C----------
C                      PC       IC       RC       GF       RF      SH
       DATA OCMAP/ '04105', '08105', '24205', '01703', '02006', '02105',
C             DF       WH       MH       WB       KP       LP   CPtoLP
     &    '20205', '26305', '26403', '10105', '10305', '10805', '10805',
C             LM       JP       SP       WP       PP   MPtoGP       GP
     &    '11301', '11605', '11705', '11905', '12205', '12702', '12702',
C             WJ       BR   GStoRW       PY   OStoJP       LO       CY
     &    '06405', '09204', '21104', '23104', '11605', '80102', '80502',
C             BL   EOtoBL       WO       BO       VO       IO       BM
     &    '80702', '80702', '81505', '81802', '82102', '83902', '31206',
C         BUtoBM       RA       MA   GCtoTO   DGtoRA   FLtoBM   WNtoBM
     &    '31206', '35106', '36102', '63102', '35106', '31206', '31206',
C             TO   SYtoTO       AS       CW   WItoBM   CNtoCL       CL
     &    '63102', '63102', '74605', '74705', '31206', '98102', '98102',
C             OH
     &    '31206'/
C----------
C  FVS-ORGANON NWO & SMC  (OP VARIANT)
C----------
C                      SF       WF       GF       AF       RF      SS
       DATA OPMAP/ '01105', '01505', '01703', '01905', '02006', '09805',
C             NF       YC       IC       ES       LP       JP       SP
     &    '02206', '04205', '08105', '09305', '10805', '11605', '11705',
C             WP       PP       DF       RW       RC       WH       MH
     &    '11905', '12205', '20205', '21104', '24205', '26305', '26403',
C             BM       RA       MA       TO   GCtoTO       AS       CW
     &    '31206', '35106', '36102', '63102', '63102', '74605', '74705',
C             WO        J       LL       WB       KP       PY   DGtoRA
     &    '81505', '06405', '07204', '10105', '10305', '23104', '35106',
C         HTtoRA   CHtoRA   WItoBM       --       OT
     &    '35106', '35106', '31206', '12205', '12205'/
C
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = IICR
      IDANUW = IWHO
      RDANUW = P
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'CWCALC',6,ICYC)
C----------
C  SET THE EQUATION NUMBER
C  OR IF THIS IS AN R5 FOREST BRANCH TO THE R5CRWD ROUTINE
C
C  NOTES FOR NON-FS FOREST CODES (ALSO SEE SUBROUTINE **FORKOD**):
C  SO VARIANT IFOR=8=INDUSTRY IS IN R5
C             IFOR=10=WARM SPRINGS RESERVATION IS IN R6
C  NC VARIANT IFOR=5=HOOPA IS IN R5; IFOR=6=SIMPSON TIMBER IS IN R6;
C                 IFOR=7=BLM COOS BAY IS IN R6
C  PN&OP VARIANTS IFOR=3=QUINAULT IS IN R6; IFOR=4,5,6=BLM SALEM,EUGENE,COOS BAY
C                 ARE IN R6
C  WC VARIANT IFOR=7,8,9,10=BLM SALEM,EUGENE,ROSEBURG,MEDFORD ARE IN R6
C  CA VARIANT IFOR=8,9,10=BLM ROSEBURG,MEDFORD, COOS BAY ARE IN R6
C
C  ALSO BECAUSE OF FOREST CODE MAPPING IN **FORKOD**, IFOR WILL BE:
C  BM VARIANT  .LE. 4
C  CA VARIANT  .LE. 10
C  EC VARIANT  .LE. 4
C  NC VARIANT  .LE. 7
C  SO VARIANT  .LE. 10 BUT NOT 9
C  WC VARIANT  .LE. 10
C  WS VARIANT  .LE. 6
C  MAPPING IS ALWAYS WITHIN REGIONAL BOUNDARIES.
C----------
      CALL VARVER(VVER)
C
      IF((VVER(:2).EQ.'SO').AND.(IFOR.GE.4 .AND. IFOR.LE.9))THEN
        CALL R5CRWD(ISPC,D,H,CW)
        GO TO 9000
      ELSEIF (VVER(:2).EQ.'WS')THEN
        CALL R5CRWD(ISPC,D,H,CW)
        GO TO 9000
      ELSEIF ((VVER(:2).EQ.'NC').AND.((IFOR.LE.3).OR.(IFOR.EQ.5)))THEN
        CALL R5CRWD(ISPC,D,H,CW)
        GO TO 9000
      ELSEIF (((VVER(:2).EQ.'CA') .OR. (VVER(:2).EQ.'OC')).AND.
     &        (IFOR.LE.5))THEN
        CALL R5CRWD(ISPC,D,H,CW)
        GO TO 9000
      ENDIF
C
      SELECT CASE (VVER(:2))
        CASE('AK')
          CWEQN=AKMAP(ISPC)
        CASE('BM')
          CWEQN=BMMAP(ISPC)
        CASE('CA')
          CWEQN=CAMAP(ISPC)
        CASE('CI')
          CWEQN=CIMAP(ISPC)
        CASE('CR')
          CWEQN=CRMAP(ISPC)
        CASE('SM')
          CWEQN=CRMAP(ISPC)
        CASE('SP')
          CWEQN=CRMAP(ISPC)
        CASE('BP')
          CWEQN=CRMAP(ISPC)
        CASE('SF')
          CWEQN=CRMAP(ISPC)
        CASE('LP')
          CWEQN=CRMAP(ISPC)
        CASE('EC')
          CWEQN=ECMAP(ISPC)
        CASE('EM')
          CWEQN=EMMAP(ISPC)
        CASE('IE')
          CWEQN=IEMAP(ISPC)
        CASE('KT')
          CWEQN=KTMAP(ISPC)
        CASE('NC')
          CWEQN=NCMAP(ISPC)
        CASE('NI')
          CWEQN=NIMAP(ISPC)
        CASE('OC')
          CWEQN=OCMAP(ISPC)
        CASE('OP')
          CWEQN=OPMAP(ISPC)
        CASE('PN')
          CWEQN=PNMAP(ISPC)
        CASE('SO')
          CWEQN=SOMAP(ISPC)
        CASE('TT')
          CWEQN=TTMAP(ISPC)
        CASE('UT')
          CWEQN=UTMAP(ISPC)
        CASE('WC')
          CWEQN=WCMAP(ISPC)
        CASE('WS')
          CWEQN=WSMAP(ISPC)
      END SELECT
C----------
C      IF(DEBUG)WRITE(JOSTND,*)
C     &' ENTERING CWCALC: ISPC,CWEQN,VVER(:2)= ',ISPC,CWEQN,VVER(:2)
C      IF(DEBUG)WRITE(JOSTND,*)' ISPC,P,D,H,CR,IICR,CW,IWHO,JOSTND= ',
C     &ISPC,P,D,H,CR,IICR,CW,IWHO,JOSTND
C      IF(DEBUG)WRITE(JOSTND,*)' ISPC,IFOR,KODFOR= ',ISPC,IFOR,KODFOR
C----------
C SET R6 FOREST SPECIFIC CONSTANTS FOR CROOKSTON(R6) MODELS
C
C NOTES:
C CA/OC VARIANTS  710 BLM ROSEBURG USES 610 ROGUE RIVER
C                 711 BLM MEDFORD USES 610 ROGUE RIVER
C                 712 BLM COOS BAY USES 611 SISKIYOU
C NC VARIANT      712 BLM COOS BAY USES 611 SISKIYOU
C                 800 SIMPSON TIMBER USES 611 SISKIYOU
C PN/OP VARIANTS  708 BLM SALEM USES 606 MT HOOD
C                 709 BLM EUGENE USES 618 WILLAMETTE
C                 712 BLM COOS BAY USES 611 SISKIYOU
C SO VARIANT      799 WARM SPRINGS RESERVATION USES DESCHUTES
C WC VARIANT      708 BLM SALEM USES 606 MT HOOD
C                 709 BLM EUGENE USES 618 WILLAMETTE
C                 710 BLM ROSEBURG USES 610 ROGUE RIVER
C                 711 BLM MEDFORD USES 610 ROGUE RIVER
C----------
      FIASP=CWEQN(1:3)
      BF=1.0
C----------
C SKIP THIS SECTION FOR FORESTS IN REGIONS 1,2,3,4,5,10
C THIS VERSION OF CWCALC IS NOT USED IN REGIONS 8 OR 9
C NON-FS AND NON-BLM CODES 7xx WHICH USE **R5CRWD** FALL THROUGH THIS SECTION
C THE CODE FOR NC FOREST CODE 800 IS BECAUSE THERE IS ALSO A FOREST CODE 800
C IN THE PN AND OP VARIANTS. NC 800 USES SISKIYOU SETTINGS. PN/OP 800 USES
C OLYMPIC SETTINGS 
C----------
      IF(KODFOR .LT. 601 .OR. KODFOR.GE.1000)GO TO 10
      IF(VVER(:2).EQ.'NC' .AND. KODFOR.EQ.800)THEN
          SELECT CASE (FIASP)     !USE 611=SISKIYOU VALUES
            CASE('351')
              BF=0.810
            CASE('081')
              BF=0.821
            CASE('108')
              BF=0.944
            CASE('122')
              BF=0.951
            CASE('202')
              BF=0.961
            CASE('242')
              BF=0.973
            CASE('263')
              BF=1.028
            CASE('264')
              BF=0.900
          END SELECT
        GO TO 10
      ENDIF
C
      SELECT CASE (KODFOR)
        CASE(601, 799)                 !DESCHUTES
          SELECT CASE (FIASP)
            CASE('015')
              BF=1.044
            CASE('019')
              BF=0.936
            CASE('022')
              BF=1.301
            CASE('081')
              BF=0.837
            CASE('073')
              BF=0.818
            CASE('117')
              BF=1.048
            CASE('122')
              BF=0.918
            CASE('202')
              BF=1.055
            CASE('263')
              BF=1.097
          END SELECT
        CASE(602)                 !FREMONT
          SELECT CASE (FIASP)
            CASE('011')
              BF=1.032
            CASE('108')
              BF=1.114
            CASE('119')
              BF=1.090
            CASE('122')
              BF=0.946
            CASE('264')
              BF=1.257
          END SELECT
        CASE(603)                 !GIFFORD PINCHOT
          SELECT CASE (FIASP)
            CASE('011')
              BF=1.032
            CASE('019')
              BF=0.906
            CASE('022')
              BF=1.123
            CASE('073')
              BF=0.952
            CASE('119')
              BF=1.128
            CASE('242')
              BF=0.920
            CASE('263')
              BF=1.028
            CASE('264')
              BF=1.077
          END SELECT
        CASE(604)                 !MALHEUR
          SELECT CASE (FIASP)
            CASE('019')
              BF=1.110
            CASE('073')
              BF=0.818
            CASE('108')
              BF=1.196
            CASE('093')
              BF=1.121
            CASE('119')
              BF=1.081
            CASE('202')
              BF=1.058
          END SELECT
        CASE(605)                 !MT BAKER-SNOQUALMIE
          SELECT CASE (FIASP)
            CASE('019')
              BF=0.886
            CASE('022')
              BF=1.075
            CASE('073')
              BF=0.907
            CASE('093')
              BF=0.949
            CASE('119')
              BF=1.081
            CASE('202')
              BF=1.019
            CASE('242')
              BF=0.973
          END SELECT
        CASE(606, 708)            !MT HOOD, BLM SALEM
          SELECT CASE (FIASP)
            CASE('011')
              BF=1.296
            CASE('015')
              BF=1.130
            CASE('017')
              BF=1.086
            CASE('019')
              BF=1.038
            CASE('022')
              BF=1.301
            CASE('042')
              BF=1.493
            CASE('073')
              BF=0.907
            CASE('108')
              BF=0.944
            CASE('119')
              BF=1.081
            CASE('242')
              BF=1.115
            CASE('263')
              BF=1.260
            CASE('264')
              BF=1.106
          END SELECT
        CASE(607)                 !OCHOCO
          SELECT CASE (FIASP)
            CASE('019')
              BF=1.110
            CASE('073')
              BF=0.879
            CASE('108')
              BF=1.196
            CASE('093')
              BF=1.169
            CASE('202')
              BF=1.055
          END SELECT
        CASE(608)                 !OKANOGAN
          SELECT CASE (FIASP)
            CASE('073')
              BF=0.952
            CASE('108')
              BF=1.114
            CASE('119')
              BF=1.081
            CASE('242')
              BF=0.905
            CASE('264')
              BF=0.900
          END SELECT
        CASE(609, 800)            !OLYMPIC, QUINAULT IR
          SELECT CASE (FIASP)
            CASE('011')
              BF=1.032
            CASE('108')
              BF=1.114
            CASE('098')
              BF=1.146
            CASE('242')
              BF=0.941
          END SELECT
        CASE(610, 710, 711)       !ROGUE RIVER, BLM ROSEBURG AND MEDFORD
          SELECT CASE (FIASP)
            CASE('019')
              BF=0.886
            CASE('351')
              BF=0.810
            CASE('081')
              BF=0.903
            CASE('108')
              BF=0.944
            CASE('093')
              BF=0.949
            CASE('117')
              BF=1.048
            CASE('119')
              BF=1.081
            CASE('122')
              BF=0.918
            CASE('264')
              BF=0.900
          END SELECT
        CASE(611, 712)            !SISKIYOU, BLM COOS BAY
          SELECT CASE (FIASP)
            CASE('351')
              BF=0.810
            CASE('081')
              BF=0.821
            CASE('108')
              BF=0.944
            CASE('122')
              BF=0.951
            CASE('202')
              BF=0.961
            CASE('242')
              BF=0.973
            CASE('263')
              BF=1.028
            CASE('264')
              BF=0.900
          END SELECT
        CASE(612)                 !SIUSLAW
          SELECT CASE (FIASP)
            CASE('202')
              BF=0.977
            CASE('242')
              BF=0.905
            CASE('263')
              BF=0.924
          END SELECT
        CASE(614)                 !UMATILLA
          SELECT CASE (FIASP)
            CASE('017')
              BF=1.076
            CASE('019')
              BF=1.110
            CASE('073')
              BF=0.907
            CASE('108')
              BF=1.244
            CASE('093')
              BF=1.137
            CASE('117')
              BF=1.097
            CASE('119')
              BF=1.128
            CASE('122')
              BF=1.035
            CASE('202')
              BF=1.055
            CASE('242')
              BF=1.055
            CASE('263')
              BF=1.106
          END SELECT
        CASE(615)                 !UMPQUA
          SELECT CASE (FIASP)
            CASE('011')
              BF=1.032
            CASE('015')
              BF=1.130
            CASE('022')
              BF=1.043
            CASE('042')
              BF=1.295
            CASE('108')
              BF=1.050
            CASE('093')
              BF=1.325
            CASE('117')
              BF=1.097
            CASE('119')
              BF=1.128
            CASE('122')
              BF=1.035
            CASE('202')
              BF=1.055
            CASE('242')
              BF=1.049
            CASE('263')
              BF=1.106
          END SELECT
        CASE (616)                !WALLOWA-WHITMAN
           SELECT CASE (FIASP)
            CASE('073')
              BF=0.818
            CASE('108')
              BF=1.114
            CASE('093')
              BF=1.070
            CASE('264')
              BF=1.077
          END SELECT
        CASE(617)                 !WENATCHEE
          SELECT CASE (FIASP)
            CASE('017')
              BF=0.972
            CASE('019')
              BF=0.906
            CASE('073')
              BF=0.879
            CASE('108')
              BF=0.969
            CASE('093')
              BF=0.949
            CASE('117')
              BF=1.097
            CASE('122')
              BF=0.946
            CASE('202')
              BF=0.975
            CASE('242')
              BF=0.905
            CASE('263')
              BF=0.962
            CASE('264')
              BF=0.952
          END SELECT
        CASE(618, 709)            !WILLAMETTE, BLM EUGENE
          SELECT CASE (FIASP)
            CASE('017')
              BF=0.972
            CASE('019')
              BF=0.936
            CASE('042')
              BF=1.127
            CASE('108')
              BF=0.903
            CASE('093')
              BF=0.857
            CASE('117')
              BF=1.097
            CASE('119')
              BF=1.081
            CASE('122')
              BF=1.070
            CASE('263')
              BF=1.087
          END SELECT
        CASE(620)                 !WINEMA
          SELECT CASE (FIASP)
            CASE('015')
              BF=1.095
            CASE('022')
              BF=1.043
            CASE('108')
              BF=1.050
            CASE('117')
              BF=1.048
            CASE('119')
              BF=1.090
            CASE('122')
              BF=0.951
            CASE('202')
              BF=1.184
            CASE('264')
              BF=1.077
          END SELECT
        CASE(621)                 !COLVILLE
          SELECT CASE (FIASP)
            CASE('017')
              BF=1.130
            CASE('019')
              BF=1.038
            CASE('108')
              BF=1.216
            CASE('093')
              BF=1.137
            CASE('119')
              BF=1.206
            CASE('122')
              BF=1.035
            CASE('202')
              BF=1.055
            CASE('242')
              BF=0.973
            CASE('263')
              BF=1.097
          END SELECT
      END SELECT
C----------
C  INITIALIZE RETURN VARIABLES.
C----------
   10 CONTINUE
      CW = 0.
C----------
C SET OTHER VARIABLES FOR CROWN MODELS.
C----------
      CL= CR*H*0.01
      BAREA=BA
      IF(BAREA .LE. 1.) BAREA=1.
      HILAT=TLAT
      HILONG = -1*ABS(TLONG)
      HIELEV=ELEV*100
      EL=ELEV
C  COMPUTE HOPKINS INDEX
      HI = ((HIELEV-5449.)/100.)*1.0 + (HILAT-42.16)*4.0 +
     &      (-116.39 - HILONG)*1.25
C
      IF(DEBUG)WRITE(JOSTND,*)' IN CWCALC: ISPC,D,VVER,CWEQN= ',
     &ISPC,D,VVER(:2),CWEQN
C----------
C  CALCULATE CROWN WIDTH
C----------
      SELECT CASE (CWEQN)
C-----------------------------------------------------------------------
C  CASE 01102 BECHTOLD 2004 MODEL 2
C  011          ABIES AMABILIS                     PACIFIC SILVER FIR   
      CASE('01102')
        IF (HI .LT.  -9.) HI= -9.
        IF (HI .GT.  26.) HI= 26.
        IF (D .GE. MIND) THEN
          CW= 7.7763 + (0.5960*D) + (-0.0705*HI)
        ELSE
          CW= (7.7763 + (0.5960*MIND) + (-0.0705*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 33.) CW=33.
C
C  CASE 01105 CROOKSTON (R6) MODEL 2
C  011          ABIES AMABILIS                     PACIFIC SILVER FIR   
      CASE('01105')
        IF (EL .LT.   4.) EL=  4.
        IF (EL .GT.  72.) EL= 72.
        IF (D .GE. OMIND) THEN
          CW= 4.4799*BF*(D**0.45976)*(H**(-0.10425))*(CL**0.11866)*
     &       ((BAREA+1.0)**0.06762)*(EXP(EL)**(-0.00715))
        ELSE
          CW= (4.4799*BF*(OMIND**0.45976)*(H**(-0.10425))*
     &       (CL**0.11866)*((BAREA+1.0)**0.06762)*(EXP(EL)**(-0.00715)))
     &       *(D/OMIND)
        ENDIF
        IF (CW .GT. 33.) CW=33.
C-----------------------------------------------------------------------
C  CASE 01502 BECHTOLD 2004 MODEL 2
C  015          ABIES CONCOLOR                     WHITE FIR            
      CASE('01502')
        IF (HI .LT. -40.) HI=-40.
        IF (HI .GT.  19.) HI= 19.
        IF (D .GE. MIND) THEN
          CW= 2.4789 + (0.9317*D) + (- 0.0128*D*D) + (0.0327*CR) +
     &    (-0.1178*HI)
        ELSE
          CW= (2.4789 + (0.9317*MIND) + (-0.0128*D*D) + (0.0327*CR) + 
     &   (-0.1178*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C
C  CASE 01505 CROOKSTON (R6) MODEL 2
C  015          ABIES CONCOLOR                     WHITE FIR            
      CASE('01505')
        IF (EL .LT.   2.) EL=  2.
        IF (EL .GT.  75.) EL= 75.
        IF (D .GE. OMIND) THEN
          CW= 5.0312*BF*(D**0.53680)*(H**(-0.18957))*(CL**0.16199)*
     &         ((BAREA+1.0)**0.04385)*(EXP(EL)**(-0.00651))
        ELSE
          CW= (5.0312*BF*(OMIND**0.53680)*(H**(-0.18957))*
     &       (CL**0.16199)*((BAREA+1.0)**0.04385)*(EXP(EL)**(-0.00651)))
     &       *(D/OMIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C
C  CASE 01506 DONNELLY (R6)
C  015          ABIES CONCOLOR                     WHITE FIR            
      CASE('01506')
        IF (D .GE. OMIND) THEN
          CW= 3.8166*D**0.5229
        ELSE
          CW= (3.8166*OMIND**0.5229)*(D/OMIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C-----------------------------------------------------------------------
C  CASE 01702 BECHTOLD 2004 MODEL 2
C  017          ABIES GRANDIS                      GRAND FIR            
      CASE('01702')
        IF (HI .LT. -48.) HI=-48.
        IF (HI .GT.  20.) HI= 20.
        IF (D .GE. MIND) THEN
          CW= 3.0335 + (0.9752*D) + (-0.0113*D*D) + (0.0548*CR) + 
     &       (-0.0597*HI)
        ELSE
          CW= (3.0335 + (0.9752*MIND) + (-0.0113*D*D) + (0.0548*CR) + 
     &       (-0.0597*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 01703 CROOKSTON (R1)
C  017          ABIES GRANDIS                      GRAND FIR            
      CASE('01703')
        IF (D .GE. 1.0) THEN
          CW= 1.0303*EXP(1.14079 + 0.20904*ALOG(CL)+0.38787*ALOG(D))
        ELSE
          CW= (1.0303*EXP(1.14079 + 0.20904*ALOG(CL)+
     &        0.38787*ALOG(1.0))) * (D/1.0)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 01705 CROOKSTON (R6) MODEL 2
C  017          ABIES GRANDIS                      GRAND FIR            
      CASE('01705')
        IF (EL .LT.   3.) EL=  3.
        IF (EL .GT.  75.) EL= 75.
        IF (D .GE. OMIND) THEN
          CW= 6.0231*BF*(D**0.54674)*(H**(-0.19451))*(CL**0.15375)
     &       * ((BAREA+1.0)**0.02897)*(EXP(EL)**(-0.00512))
        ELSE
          CW= (6.0231*BF*(OMIND**0.54674)*(H**(-0.19451))*
     &       (CL**0.15375)*((BAREA+1.0)**0.02897)*(EXP(EL)**(-0.00512)))
     &        *(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 01706 DONNELLY (R6)
C  017          ABIES GRANDIS                      GRAND FIR            
      CASE('01706')
        IF (D .GE. OMIND) THEN
          CW= 4.1870*D**0.5341
        ELSE
          CW= (4.1870*OMIND**0.5341)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 01707 MOEUR
C  017          ABIES GRANDIS                      GRAND FIR            
      CASE('01707')
        CW= EXP(2.20611+ 1.08137*ALOG(D) + (-0.76936)*ALOG(H)
     &      + 0.29786*ALOG(CL))
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 01801 BECHTOLD 2004 MODEL 1
C  018          ABIES LASIOCARPA var. ARIZONICA    CORKBARK FIR         
      CASE('01801')
        IF (D .GE. MIND) THEN
          CW= 6.073 + 0.3756*D
        ELSE
          CW= (6.073 + 0.3756*MIND)*(D/MIND)
        ENDIF
        IF (CW .GT. 15.) CW=15.
C-----------------------------------------------------------------------
C  CASE 01901 BECHTOLD 2004 MODEL 2
C  019          ABIES LASIOCARPA                   SUBALPINE FIR        
      CASE('01901')
        IF (HI .LT. -14.) HI=-14.
        IF (HI .GT.  44.) HI= 44.
        IF (D .GE. MIND) THEN
          CW= 2.6068 + 0.6145*D + 0.0417*CR + (-0.0698)*HI
        ELSE
          CW= (2.6068+0.6145*MIND + 0.0417*CR+(-0.0698*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 01903 CROOKSTON (R1)
C  019          ABIES LASIOCARPA                   SUBALPINE FIR        
      CASE('01903')
        IF (D .GE. 0.1) THEN
          CW= 1.02886*EXP(1.01255 + 0.30374*ALOG(CL)+0.37093*ALOG(D)
     &        + (-0.13731*ALOG(H)))
        ELSE
          CW= (1.02886*EXP(1.01255 + 0.30374*ALOG(CL) +
     &         0.37093*ALOG(0.1) + (-0.13731*ALOG(H))))*(D/0.1)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 01905 CROOKSTON (R6) MODEL 2
C  019          ABIES LASIOCARPA                   SUBALPINE FIR        
      CASE('01905')
        IF (EL .LT.  10.) EL= 10.
        IF (EL .GT.  85.) EL= 85.
        IF (D .GE. OMIND) THEN
          CW= 5.8827*BF*(D**0.51479)*(H**(-0.21501))*(CL**0.17916)*
     &         ((BAREA+1.0)**0.03277)*(EXP(EL)**(-0.00828))
        ELSE
          CW= (5.8827*BF*(OMIND**0.51479)*(H**(-0.21501))*
     &       (CL**0.17916)*((BAREA+1.0)**0.03277)*(EXP(EL)**(-0.00828)))
     &        *(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 01906 DONNELLY (R6)
C  019          ABIES LASIOCARPA                   SUBALPINE FIR        
      CASE('01906')
        IF (D .GE. OMIND) THEN
          CW= 3.2348*D**0.5179
        ELSE
          CW= (3.2348*OMIND**0.5179)*(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 01907 MOEUR
C  019          ABIES LASIOCARPA                   SUBALPINE FIR        
      CASE('01907')
        CW= EXP(1.74558 + (1.08137*ALOG(D)) + (-0.73972*ALOG(H))
     &     + (0.29786*ALOG(CL)))
        IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 02002 BECHTOLD 2004 MODEL 2
C  020          ABIES MAGNIFICA VAR. MAGNIFICA   CALIFORNIA RED FIR
      CASE('02002')
        IF (HI .LT. -14.) HI=-14.
        IF (HI .GT.  44.) HI= 44.
        IF (D .GE. MIND) THEN
          CW= 2.3660 + 0.5472*D + 0.0316*CR + (-0.0702)*HI
        ELSE
          CW= (2.3660+0.5472*MIND+0.0316*CR+(-0.0702*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 36.) CW=36.
C
C  CASE 02006 DONNELLY (R6)
C  020          ABIES MAGNIFICA VAR. MAGNIFICA   CALIFORNIA RED FIR
      CASE('02006')
        IF (D .GE. OMIND) THEN
          CW= 3.1146*D**0.5780
        ELSE
          CW= (3.1146*OMIND**0.5780)*(D/OMIND)
        ENDIF
        IF (CW .GT. 65.) CW=65.
C-----------------------------------------------------------------------
C  CASE 02101 BECHTOLD 2004 MODEL 2
C  021          ABIES MAGNIFICA VAR. SHASTENSIS    SHASTA RED FIR
      CASE('02101')
        IF (D .GE. MIND) THEN
          CW= 4.0524 + 0.6423*D
        ELSE
          CW= (4.0524 + 0.6423*MIND)*(D/MIND)
        ENDIF
        IF (CW .GT. 26.) CW=26.
C
C  CASE 02105 CROOKSTON (R6) MODEL 2
C  021          ABIES MAGNIFICA VAR. SHASTENSIS    SHASTA RED FIR
      CASE('02105')
        IF (D .GE. OMIND) THEN
          CW= 2.3170*BF*(D**0.47880)*(H**(-0.06093))*(CL**0.15482)*
     &        ((BAREA+1.0)**0.05182)
        ELSE
          CW= (2.3170*BF*(OMIND**0.47880)*(H**(-0.06093))
     &       *(CL**0.15482)*((BAREA+1.0)**0.05182))*(D/OMIND)
        ENDIF
        IF (CW .GT. 65.) CW=65.
C-----------------------------------------------------------------------
C  CASE 02201 BECHTOLD 2004 MODEL 2
C  022          ABIES PROCERA                      NOBLE FIR
      CASE('02201')
        IF (HI .LT. -11.) HI=-11.
        IF (HI .GT.  32.) HI= 32.
        IF (D .GE. MIND) THEN
          CW= 2.7761 + 0.7311*D + 0.0476*CR + (-0.0756*HI)
        ELSE
          CW= (2.7761 + 0.7311*MIND + 0.0476*CR + (-0.0756*HI))*
     &      (D/MIND)
        ENDIF
        IF (CW .GT. 29.) CW=29.
C
C  CASE 02206 DONNELLY (R6)
C  022          ABIES PROCERA                      NOBLE FIR
      CASE('02206')
        IF (D .GE. OMIND) THEN
          CW= 3.0614*D**0.6276
        ELSE
          CW= (3.0614*OMIND**0.6276)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 04102 BECHTOLD 2004 MODEL 2
C  041          CHAMAECYPARIS LAWSONIANA           PORT ORFORD CEDAR
      CASE('04102')
        IF (D .GE. MIND) THEN
          CW= 1.0365 + (0.7943*D) + (0.0399*CR)
        ELSE
          CW= (1.0365 + (0.7943*MIND) + (0.0399*CR))*(D/MIND)
        ENDIF
        IF (CW .GT. 22.) CW=22.
C
C  CASE 04105 CROOKSTON (R6) MODEL 2
C  041          CHAMAECYPARIS LAWSONIANA           PORT ORFORD CEDAR
      CASE('04105')
        IF (EL .LT.   2.) EL=  2.
        IF (EL .GT.  52.) EL= 52.
        IF (D .GE. OMIND) THEN
          CW= 4.6387*BF*(D**0.50874)*(H**(-0.22111))*(CL**0.1755)*
     &       ((BAREA+1.0)**0.06447)*(EXP(EL)**(-0.00602))
        ELSE
          CW= (4.6387*BF*(OMIND**0.50874)*(H**(-0.22111))*
     &       (CL**0.1755)*((BAREA+1.0)**0.06447)*(EXP(EL)**(-0.00602)))*
     &       (D/OMIND)
        ENDIF
        IF (CW .GT. 49.) CW=49.
C
C  CASE 04106 DONNELLY (R6)
C  041          CHAMAECYPARIS LAWSONIANA           PORT ORFORD CEDAR
      CASE('04106')
        IF (D .GE. OMIND) THEN
          CW= 5.3864*D**0.4213
        ELSE
          CW= (5.3864*OMIND**0.4213)*(D/OMIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C-----------------------------------------------------------------------
C  CASE 04205 CROOKSTON (R6) MODEL 2
C  042          CHAMAECYPARIS NOOTKATENSIS          ALASKA YELLOW CEDAR
      CASE('04205')
        IF (EL .LT.  16.) EL= 16.
        IF (EL .GT.  62.) EL= 62.
        IF (D .GE. OMIND) THEN
          CW= 3.3756*BF*(D**0.45445)*(H**(-0.11523))*(CL**0.22547)*
     &     ((BAREA+1.0)**0.08756)*(EXP(EL)**(-0.00894))
        ELSE
          CW= (3.3756*BF*(OMIND**0.45445)*(H**(-0.11523))*
     &      (CL**0.22547)*((BAREA+1.0)**0.08756)*(EXP(EL)**(-0.00894)))*
     &      (D/OMIND)
        ENDIF
        IF (CW .GT. 59.) CW=59.
C
C  CASE 04206 DONNELLY (R6)
C  042          CHAMAECYPARIS NOOTKATENSIS          ALASKA YELLOW CEDAR
      CASE('04206')
        IF (D .GE. OMIND) THEN
          CW= 3.5341*D**0.5374
        ELSE
          CW= (3.5341*OMIND**0.5374)*(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 06402 BECHTOLD 2004 MODEL 2
C  064          JUNIPERUS OCCIDENTALIS              WESTERN JUNIPER
      CASE('06402')
        IF (D .GE. MIND) THEN
          CW= -0.0037 + (1.3526*D) + (-0.0165*D*D)
        ELSE
          CW= (-0.0037+(1.3526*MIND)+(-0.0165*MIND*MIND))*(D/MIND)
        ENDIF
        IF (CW .GT. 36.) CW=36.
C
C  CASE 06405 CROOKSTON (R6) MODEL 2
C  064          JUNIPERUS OCCIDENTALIS              WESTERN JUNIPER
      CASE('06405')
        IF (D .GE. OMIND) THEN
          CW= 5.1486*BF*(D**0.73636)*(H**(-0.46927))*(CL**0.39114)*
     &       ((BAREA+1.0)**(-0.05429))
        ELSE
          CW= (5.1486*BF*(OMIND**0.73636)*(H**(-0.46927))*
     &       (CL**0.39114)*((BAREA+1.0)**(-0.05429)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 36.) CW=36.
C-----------------------------------------------------------------------
C  CASE 06601 BECHTOLD 2004 MODEL 1
C  066          JUNIPERUS SCOPULORUM              ROCKY MOUNTAIN JUNIPER
      CASE('06601')
        IF (D .GE. MIND) THEN
          CW= 2.1431 + (1.3447*D) + (-0.0228*D*D)
        ELSE
          CW= (2.1431 + (1.3447*MIND) + (-0.0228*MIND*MIND))*(D/MIND)
        ENDIF
        IF (CW .GT. 29.) CW=29.
C
C  CASE 06602 BECHTOLD 2004 MODEL 2
C  066          JUNIPERUS SCOPULORUM              ROCKY MOUNTAIN JUNIPER
      CASE('06602')
        IF (HI .LT. -37.) HI=-37.
        IF (HI .GT.  19.) HI= 19.
        IF (D .GE. MIND) THEN
          CW= -4.1599 +(1.3528*D) + (-0.0233*D*D) + (0.0633*CR)
     &        + (-0.0423*HI)
        ELSE
          CW= (-4.1599 + (1.3528*MIND)+(-0.0233*MIND*MIND)+
     &        (0.0633*CR) + (-0.0423*HI))*(D/MIND)
        ENDIF
        IF (D .GE. 25) THEN
          CW= -4.1599 +(1.3528*25) + (-0.0233*25*25) + (0.0633*CR)
     &       + (-0.0423*HI)
        ENDIF
        IF (CW .GT. 29.) CW=29.
C-----------------------------------------------------------------------
C  CASE 07204 CROOKSTON (R6) MODEL 1
C  072          LARIX LYALLII                     SUBALPINE LARCH
      CASE('07204')
        IF (D .GE. OMIND) THEN
          CW= 2.2586*D**0.68532
        ELSE
          CW= (2.2586*OMIND**0.68532)*(D/OMIND)
        ENDIF
        IF (CW .GT. 33.) CW=33.
C-----------------------------------------------------------------------
C  CASE 07302 BECHTOLD 2004 MODEL 2
C  073          LARIX OCCIDENTALIS                 WESTERN LARCH        
      CASE('07302')
        IF (D .GE. MIND) THEN
          CW= 1.5995 + 0.7675*D + 0.075*CR
        ELSE
          CW= (1.5995 + 0.7675*MIND + 0.075*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 07303 CROOKSTON (R1)
C  073          LARIX OCCIDENTALIS                 WESTERN LARCH        
      CASE('07303')
        IF (D .GE. 1.0) THEN
          CW= 1.02478*EXP(0.99889 + 0.19422*ALOG(CL)+0.59423*ALOG(D)
     &       + (-0.09078*ALOG(H)) + (-0.02341*ALOG(BAREA)))
        ELSE
          CW= (1.02478*EXP(0.99889+0.19422*ALOG(CL)+0.59423*ALOG(1.0)
     &       + (-0.09078*ALOG(H)) + (-0.02341*ALOG(BAREA))))*(D/1.0)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 07305 CROOKSTON (R6) MODEL 2
C  073          LARIX OCCIDENTALIS                 WESTERN LARCH        
      CASE('07305')
        IF (EL .LT.  19.) EL= 19.
        IF (EL .GT.  72.) EL= 72.
        IF (D .GE. OMIND) THEN
          CW= 3.2548*BF*(D**0.60845)*(H**(-0.19146))*(CL**0.21051)*
     &        ((BAREA+1.0)**0.00972)*(EXP(EL)**(-0.00313))
        ELSE
          CW= (3.2548*BF*(OMIND**0.60845)*(H**(-0.19146))*
     &       (CL**0.21051)*((BAREA+1.0)**0.00972)*(EXP(EL)**(-0.00313)))
     &       *(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 07306 DONNELLY (R6)
C  073          LARIX OCCIDENTALIS                 WESTERN LARCH        
      CASE('07306')
        IF (D .GE. OMIND) THEN
          CW= 2.9571*D**0.6081
        ELSE
          CW= (2.9571*D**0.6081)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 07307 MOEUR
C  073          LARIX OCCIDENTALIS                 WESTERN LARCH        
      CASE('07307')
        CW= EXP(2.31359 + 1.08137*ALOG(D) + (-0.80919*ALOG(H))
     &      + 0.29786*ALOG(CL))
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 08105 CROOKSTON (R6) MODEL 2
C  081          LIBOCEDRUS DECURRENS                INCENSE CEDAR     
      CASE('08105')
        IF (EL .LT.   5.) EL=  5.
        IF (EL .GT.  62.) EL= 62.
        IF (D .GE. OMIND) THEN
          CW= 5.0446*BF*(D**0.47419)*(H**(-0.13917))*(CL**0.14230)
     &       *((BAREA+1.0)**0.04838)*(EXP(EL)**(-0.00616))
        ELSE
          CW= (5.0446*BF*(OMIND**0.47419)*(H**(-0.13917))*
     &       (CL**0.14230)*((BAREA+1.0)**0.04838)*(EXP(EL)**(-0.00616)))
     &        *(D/OMIND)
        ENDIF
        IF (CW .GT. 78.) CW=78.
C
C  CASE 08106 DONNELLY (R6)
C  081          LIBOCEDRUS DECURRENS                INCENSE CEDAR        
      CASE('08106')
        IF (D .GE. OMIND) THEN
          CW= 4.0920*D**0.4912
        ELSE
          CW= (4.0920*OMIND**0.4912)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 09204 CROOKSTON (R6) MODEL 1
C  092          PICEA BREWERIANA                    BREWER SPRUCE
      CASE('09204')
        IF (D .GE. OMIND) THEN
          CW= 2.8232*D**0.66326
        ELSE
          CW= (2.8232*OMIND**0.66326)*(D/OMIND)
        ENDIF
        IF (CW .GT. 38.) CW=38.
C-----------------------------------------------------------------------
C  CASE 09302 BECHTOLD 2004 MODEL 2
C  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE     
      CASE('09302')
        IF (HI .LT. -25.) HI=-25.
        IF (HI .GT.  44.) HI= 44.
        IF (D .GE. MIND) THEN
          CW= 4.1348 + 0.5694*D + 0.0403*CR + (-0.1014*HI)
        ELSE
          CW= (4.1348+0.5694*MIND+0.0403*CR +(-0.1014*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 09303 CROOKSTON (R1)
C  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE     
      CASE('09303')
        IF (D .GE. 0.1) THEN
          CW= 1.02687*EXP(1.28027 + 0.2249*ALOG(CL) + 0.47075*ALOG(D)
     &       + (-0.15911)*ALOG(H))
        ELSE
          CW= (1.02687*EXP(1.28027 + 0.2249*ALOG(CL) +
     &        0.47075*ALOG(0.1)+(-0.15911)*ALOG(H)))*(D/0.1)
        ENDIF
          IF (CW .GT. 40.) CW=40.
C
C  CASE 09305 CROOKSTON (R6) MODEL 2
C  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE     
      CASE('09305')
        IF (EL .LT.   1.) EL=  1.
        IF (EL .GT.  85.) EL= 85.
        IF (D .GE. OMIND) THEN
          CW= 6.7575*BF*(D**0.55048)*(H**(-0.25204))*(CL**0.19002)
     &       *(EXP(EL)**(-0.00313))
        ELSE
          CW= (6.7575*BF*(OMIND**0.55048)*(H**(-0.25204))*
     &        (CL**0.19002)*(EXP(EL)**(-0.00313)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 09306 DONNELLY (R6)
C  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE     
      CASE('09306')
        IF (D .GE. OMIND) THEN
          CW= 3.6802*D**0.4940
        ELSE
          CW= (3.6802*OMIND**0.4940)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 09307 MOEUR
C  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE     
      CASE('09307')
        CW= EXP(3.76535 + 1.08137*ALOG(D) + (-1.18257*ALOG(H))
     &       + 0.29786*ALOG(CL))
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 09802 BECHTOLD 2004 MODEL 2
C  098          PICEA SITCHENSIS                  SITKA SPRUCE
      CASE('09802')
        IF (D .GE. MIND) THEN
          CW= 8.8087 + (0.7825*D)
        ELSE
          CW= (8.8087 + 0.7825*MIND)*(D/MIND)
        ENDIF
        IF (CW .GT. 43.) CW=43.
C
C  CASE 09805 CROOKSTON (R6) MODEL 2
C  098          PICEA SITCHENSIS                  SITKA SPRUCE
      CASE('09805')
        IF (D .GE. OMIND) THEN
          CW= 8.48*BF*(D**0.70692)*(H**(-0.38812))*(CL**0.17127)
        ELSE
          CW= (8.48*BF*(OMIND**0.70692)*(H**(-0.38812))
     &         *(CL**0.17127))*(D/OMIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C
C  CASE 09806 DONNELLY (R6)
C  098          PICEA SITCHENSIS                  SITKA SPRUCE
      CASE('09806')
        IF (D .GE. OMIND) THEN
          CW= 4.2857*D**0.5940
        ELSE
          CW= (4.2857*OMIND**0.5940)*(D/OMIND)
        ENDIF
        IF (CW .GT. 60.) CW=60.
C-----------------------------------------------------------------------
C  CASE 10102 BECHTOLD 2004 MODEL 2
C  101          PINUS ALBICAULIS                   WHITEBARK PINE       
      CASE('010102')
        IF (HI .LT.   6.) HI=  6.
        IF (HI .GT.  44.) HI= 44.
        IF (D .GE. MIND) THEN
          CW= 0.5223 + 0.7432*D + (0.0829*HI)
        ELSE
          CW= (0.5223 + 0.7432*MIND + (0.0829*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 10103 CROOKSTON (R1)
C  101          PINUS ALBICAULIS                   WHITEBARK PINE       
      CASE('10103')
      IF (D .GE. 1.1) THEN
        CW= 1.0697*EXP(0.3007 + 0.2400*ALOG(CL) + 0.5696*ALOG(D))
      ELSE
        CW= (1.0697*EXP(0.3007+0.2400*ALOG(CL)+0.5696*ALOG(1.1)))
     &     *(D/1.1)
      ENDIF
      IF (CW .GT. 40.) CW=40.
C
C  CASE 10105 CROOKSTON (R6) MODEL 2
C  101          PINUS ALBICAULIS                   WHITEBARK PINE       
      CASE('10105')
        IF (D .GE. OMIND) THEN
          CW= 2.2354*BF*(D**0.66680)*(H**(-0.11658))*(CL**0.16927)
        ELSE
          CW= (2.2354*BF*(OMIND**0.66680)*(H**(-0.11658))*
     &        (CL**0.16927))*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 10106 DONNELLY (R6)
C  101          PINUS ALBICAULIS                   WHITEBARK PINE       
      CASE('10106')
        IF (D .GE. OMIND) THEN
          CW= 2.1606*D**0.6897
        ELSE
          CW= (2.1606*OMIND**0.6897)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 10107 MOEUR
C  101          PINUS ALBICAULIS                   WHITEBARK PINE       
      CASE('10107')
        CW= EXP(-.91984 + (1.08137*ALOG(D)) + (-0.07299*ALOG(H))
     &      + 0.29786*ALOG(CL))
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 10201 BECHTOLD 2004 MODEL 1
C  102          PINUS ARTISTA                      BRISTLECONE PINE     
      CASE('10201')
        IF (D .GE. MIND) THEN
          CW= (7.4251 + 0.8991*D)
        ELSE
          CW= (7.4251 + 0.8991*MIND)*(D/MIND)
        ENDIF
        IF (CW .GT. 25.) CW=25.
C-----------------------------------------------------------------------
C  CASE 10305 CROOKSTON (R6) MODEL 2
C  103          PINUS ATTENUATA                     KNOBCONE PINE       
      CASE('10305')
        IF (EL .LT.  12.) EL= 12.
        IF (EL .GT.  49.) EL= 49.
        IF (D .GE. OMIND) THEN
          CW= 4.0069*BF*(D**0.84628)*(H**(-0.29035))*(CL**0.13143)*
     &       *(EXP(EL)**(-0.00842))
        ELSE
          CW= (4.0069*BF*(OMIND**0.84628)*(H**(-0.29035))*
     &        (CL**0.13143)*(EXP(EL)**(-0.00842)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 46.) CW=46.
C-----------------------------------------------------------------------
C  CASE 10601 BECHTOLD 2004 MODEL 1
C  106          PINUS EDULIS                      PINYON PINE (EDULIS)  
      CASE('10601')
        IF (D .GE. MIND) THEN
          CW= -1.2638 + (1.9922*D) + (-0.0410*D*D)
        ELSE
          CW= (-1.2638+(1.9922*MIND)+(-0.0410*MIND*MIND))*(D/MIND)
        ENDIF
        IF (CW .GT. 25.) CW=25.
C
C  CASE 10602 BECHTOLD 2004 MODEL 2
C  106          PINUS EDULIS                      PINYON PINE (EDULIS)  
      CASE('10602')
        IF (HI .LT. -40.) HI=-40.
        IF (HI .GT.  11.) HI= 11.
        IF (D .GE. MIND) THEN
          CW= -5.4647 + (1.9660*D) + (-0.0395*D*D) + (0.0427*CR) + 
     &         (-0.0259*HI)
        ELSE
          CW= (-5.4647+(1.9660*MIND)+(-0.0395*MIND*MIND)+(0.0427*CR)+
     &        (-0.0259*HI))*(D/MIND)
        ENDIF
        IF (D .GE. 25) THEN
          CW= -5.4647 + (1.9660*25) + (-0.0395*25*25) + (0.0427*CR) +
     &       (-0.0259*HI)
        ENDIF
        IF (CW .GT. 25.) CW=25.
C-----------------------------------------------------------------------
C  CASE 10802 BECHTOLD 2004 MODEL 2
C  108          PINUS CONTORTA                     LODGEPOLE PINE       
      CASE('10202')
        IF (D .GE. MIND) THEN
          CW= -1.5440 + (1.3828*D) + (-0.0200*D*D) + (0.0396*CR) + 
     &        (-0.0083*BAREA)
        ELSE
          CW= (-1.5440 + 1.3828*MIND + (-0.0200*D*D) + 0.0396*CR + 
     &        (-0.0083*BAREA))*(D/MIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 10803 CROOKSTON (R1)
C  108          PINUS CONTORTA                     LODGEPOLE PINE       
      CASE('10803')
        IF (D .GE. 0.7) THEN
          CW= 1.03992*EXP(1.58777 + 0.30812*ALOG(CL)+0.64934*ALOG(D)
     &       + (-0.38964)*ALOG(H))
        ELSE
          CW= (1.03992*EXP(1.58777 + 0.30812*ALOG(CL)+
     &        0.64934*ALOG(0.7)+(-0.38964)*ALOG(H)))*(D/0.7)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 10805 CROOKSTON (R6) MODEL 2
C  108          PINUS CONTORTA                     LODGEPOLE PINE       
      CASE('10805')
        IF (EL .LT.   1.) EL=  1.
        IF (EL .GT.  79.) EL= 79.
        IF (D .GE. OMIND) THEN
          CW= 6.6941*BF*(D**0.81980)*(H**(-0.36992))*(CL**0.17722)*
     &       ((BAREA+1.0)**(-0.01202))*(EXP(EL)**(-0.00882))
        ELSE
          CW=(6.6941*BF*(OMIND**0.81980)*(H**(-0.36992))*
     &       (CL**0.17722)*((BAREA+1.0)**(-0.01202))*
     &       (EXP(EL)**(-0.00882)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 10806 DONNELLY (R6)
C  108          PINUS CONTORTA                     LODGEPOLE PINE       
      CASE('10806')
        IF (D .GE. OMIND) THEN
          CW= 2.4132*D**0.6403
        ELSE
          CW= (2.4132*OMIND**0.6403)*(D/OMIND)
        ENDIF
        IF (CW .GT. 40.) CW=40.
C
C  CASE 10807 MOEUR
C  108          PINUS CONTORTA                     LODGEPOLE PINE       
      CASE('10807')
        CW= EXP(1.06804 + 1.08137*ALOG(D) + (-0.55987)*ALOG(H)
     &      + 0.29786*ALOG(CL))
        IF (CW .GT. 40.) CW=40.
C-----------------------------------------------------------------------
C  CASE 11301 BECHTOLD 2004 MODEL 1
C  113          PINUS FLEXILIS                     LIMBER PINE          
      CASE('11301')
        IF (D .GE. MIND) THEN
          CW= 4.0181 + 0.8528*D
        ELSE
          CW= (4.0181 + 0.8528*MIND)*(D/MIND)
        ENDIF
        IF (CW .GT. 25.) CW=25.
C-----------------------------------------------------------------------
C  CASE 11602 BECHTOLD 2004 MODEL 2
C  116          PINUS JEFFREYI                    JEFFREY PINE        
      CASE('11602')
        IF (HI .LT. -38.) HI=-38.
        IF (HI .GT.  15.) HI= 15.
        IF (D .GE. MIND) THEN
          CW= 1.2784 + 0.7937*D + 0.0334*CR + (-0.0887*HI)
        ELSE
          CW= (1.2784 + 0.7937*MIND + 0.0334*CR + (-0.0887*HI))*
     &        (D/MIND)
        ENDIF
        IF (CW .GT. 44.) CW=44.
C
C  CASE 11605 CROOKSTON (R6) MODEL 2
C  116          PINUS JEFFREYI                    JEFFREY PINE        
      CASE ('11605') 
        IF (D .GE. OMIND) THEN
          CW= 4.0217*BF*(D**0.66815)*(H**(-0.11346))*(CL**0.09689)*
     &        ((BAREA+1.0)**(-0.06360))
        ELSE
          CW= (4.0217*BF*(OMIND**0.66815)*(H**(-0.11346))*
     &        (CL**0.09689)*((BAREA+1.0)**(-0.06360)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 39.) CW=39.
C
C  CASE 11606 DONNELLY (R6)
C  116          PINUS JEFFREYI                    JEFFREY PINE        
      CASE('11606')
        IF (D .GE. OMIND) THEN
          CW= 3.2367*D**0.6247
        ELSE
          CW= (3.2367*OMIND**0.6247)*(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 11702 BECHTOLD 2004 MODEL 2
C  117          PINUS LAMBERTIANA                  SUGAR PINE
      CASE('11702')
        IF (HI .LT. -47.) HI=-47.
        IF (HI .GT.  11.) HI= 11.
        IF (D .GE. MIND) THEN
          CW= (3.1052) + (0.8049*D) + (-0.1230*HI)
        ELSE
          CW= (3.1052 + 0.8049*MIND + (-0.1230*HI)) * (D/MIND)
        ENDIF
        IF (CW .GT. 49.) CW=49.
C
C  CASE 11705 CROOKSTON (R6) MODEL 2
C  117          PINUS LAMBERTIANA                  SUGAR PINE
      CASE ('11705')
       IF (EL .LT.  5.) EL=  5.
       IF (EL .GT. 75.) EL= 75.
       IF (D .GE. OMIND) THEN
         CW= 3.5930*BF*(D**0.63503)*(H**(-0.22766))*(CL**0.17827)*
     &      ((BAREA+1.0)**(0.04267))*(EXP(EL)**(-0.00290))
       ELSE
         CW=(3.5930*BF*(OMIND**0.63503)*(H**(-0.22766))*
     &      (CL**0.17827)*((BAREA+1.0)**(0.04267))*
     &      (EXP(EL)**(-0.00290)))*(D/OMIND)
       ENDIF
       IF (CW .GT. 56.) CW=56.
C
C  CASE 11706 DONNELLY (R6)
C  117          PINUS LAMBERTIANA                  SUGAR PINE
      CASE('11706')
        IF (D .GE. OMIND) THEN
          CW= 3.0610*D**0.6201
        ELSE
          CW= (3.0610*OMIND**0.6201)*(D/OMIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C-----------------------------------------------------------------------
C  CASE 11902 BECHTOLD 2004 MODEL 2
C  119          PINUS MONTICOLA                    WESTERN WHITE PINE  
      CASE('11902')
        IF (HI .LT. -25.) HI=-25.
        IF (HI .GT.  32.) HI= 32.
        IF (D .GE. MIND) THEN
          CW= 4.8643 + 0.6949*D + (-0.0974*HI)
        ELSE
          CW= (4.8643 + 0.6949*MIND + (-0.0974*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C
C  CASE 11903 CROOKSTON (R1)
C  119          PINUS MONTICOLA                    WESTERN WHITE PINE  
      CASE('11903')
        IF (D .GE. 1.0) THEN
          CW= 1.0405*EXP(1.2799 + 0.11941*ALOG(CL) + 0.42745*ALOG(D)
     &        + (-0.07182*ALOG(BAREA)))
        ELSE
          CW= (1.0405*EXP(1.2799+0.11941*ALOG(CL)+0.42745*ALOG(1.0)
     &         + (-0.07182*ALOG(BAREA))))*(D/1.0)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C
C  CASE 11905 CROOKSTON (R6) MODEL 2
C  119          PINUS MONTICOLA                    WESTERN WHITE PINE   
      CASE('11905')
        IF (EL .LT.  10.) EL= 10.
        IF (EL .GT.  75.) EL= 75.
        IF (D .GE. OMIND) THEN
          CW= 5.3822*BF*(D**0.57896)*(H**(-0.19579))*(CL**0.14875)*
     &        (EXP(EL)**(-0.00685))
        ELSE
          CW= (5.3822*BF*(OMIND**0.57896)*(H**(-0.19579))*
     &        (CL**0.14875)*(EXP(EL)**(-0.00685)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C
C  CASE 11906 DONNELLY (R6)
C  119          PINUS MONTICOLA                    WESTERN WHITE PINE   
      CASE('11906')
        IF (D .GE. OMIND) THEN
          CW= 3.4447*D**0.5185
        ELSE
          CW= (3.4447*OMIND**0.5185)*(D/OMIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C
C  CASE 11907 MOEUR
C  119          PINUS MONTICOLA                    WESTERN WHITE PINE   
      CASE('11907')
        CW= EXP(4.30800 + 1.08137*ALOG(D) + (-1.37265*ALOG(H))
     &       + 0.29786*ALOG(CL))
        IF (CW .GT. 35.) CW=35.
C-----------------------------------------------------------------------
C  CASE 12202 BECHTOLD 2004 MODEL 2
C  122          PINUS PONDEROSA                    PONDEROSA PINE        
      CASE('12202')
        IF (HI .LT. -56.) HI=-56.
        IF (HI .GT.  41.) HI= 41.
        IF (D .GE. MIND) THEN
          CW= (-0.3459) + (1.111*D) + (-0.008*D*D) + (0.0566*CR)
     &       + (-0.0094*BAREA)+(-0.0362*HI)
        ELSE
          CW= ((-0.3459)+(1.111*MIND)+(-0.008*MIND*MIND)+(0.0566*CR)
     &        + (-0.0094*BAREA) +(-0.0362*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 46.) CW=46.
C
C  CASE 12203 CROOKSTON (R1)
C  122          PINUS PONDEROSA                    PONDEROSA PINE       
      CASE('12203')
        IF (D .GE. 2.0) THEN
          CW= 1.02687*EXP(1.49085 + 0.1862*ALOG(CL) + 0.68272*ALOG(D)
     &        + (-0.28242*ALOG(H)))
        ELSE
          CW= (1.02687*EXP(1.49085 + 0.1862*ALOG(CL) +
     &        0.68272*ALOG(2.0)+ (-0.28242*ALOG(H))))*(D/2.0)
        ENDIF
        IF (CW .GT. 46.) CW=46.
C
C  CASE 12205 CROOKSTON (R6) MODEL 2
C  122          PINUS PONDEROSA                    PONDEROSA PINE       
      CASE ('12205') 
        IF (EL .LT.  13.) EL= 13.
        IF (EL .GT.  75.) EL= 75.
        IF (D .GE. OMIND) THEN
          CW= 4.7762*BF*(D**0.74126)*(H**(-0.28734))*(CL**0.17137)*
     &         ((BAREA+1.0)**(-0.00602))*(EXP(EL)**(-0.00209))
        ELSE
          CW=(4.7762*BF*(OMIND**0.74126)*(H**(-0.28734))*
     &       (CL**0.17137)*((BAREA+1.0)**(-0.00602))*
     &       (EXP(EL)**(-0.00209)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C
C  CASE 12206 DONNELLY (R6)
C  122          PINUS PONDEROSA                    PONDEROSA PINE       
      CASE('12206')
        IF (D .GE. OMIND) THEN
          CW= 2.8541*D**0.6400
        ELSE
          CW= (2.8541*OMIND**0.6400)*(D/OMIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C
C  CASE 12207 MOEUR
C  122          PINUS PONDEROSA                    PONDEROSA PINE       
      CASE('12207')
        CW= EXP(1.62365+ 1.08137*ALOG(D) + (-0.68098*ALOG(H))
     &       + 0.29786*ALOG(CL))
        IF (CW .GT. 50.) CW=50.
C-----------------------------------------------------------------------
C  CASE 12702 BECHTOLD 2004 MODEL 2
C  127          PINUS SABINIANA                    GRAY PINE
      CASE('12702')
        IF (HI .LT. -69.) HI=-69.
        IF (HI .GT.  -4.) HI= -4.
        IF (D .GE. MIND) THEN
          CW= (-2.4909) + (1.0716*D) + (0.0648*CR)
     &        +(-0.1127*HI)
        ELSE
          CW= ((-2.4909) + (1.0716*MIND) + (0.0648*CR)
     &        +(-0.1127*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 54.) CW=54.
C-----------------------------------------------------------------------
C  CASE 20202 BECHTOLD 2004 MODEL 2
C  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR          
      CASE('20202')
        IF (HI .LT. -49.) HI= -49.
        IF (HI .GT.  67.) HI=  67.
        IF (D .GE. MIND) THEN
          CW= 3.2346 + 1.1158*D + (-0.0112*D*D) + 0.0442*CR + 
     &        (-0.0057*BAREA) + (-0.0237*HI)
        ELSE
          CW= (3.2346+1.1158*MIND+(-0.0112*MIND*MIND)+0.0442*CR + 
     &        (-0.0057*BAREA) + (-0.0237*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 80.) CW=80.
C
C  CASE 20203 CROOKSTON (R1)
C  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR          
      CASE('20203')
        IF (D .GE. 1.0) THEN
          CW= 1.01685*EXP(1.48372 + 0.27378*ALOG(CL)+0.49646*ALOG(D)
     &       + (-0.18669*ALOG(H)) + (-0.01509*ALOG(BAREA)))
        ELSE
          CW=(1.01685*EXP(1.48372+0.27378*ALOG(CL)+0.49646*ALOG(1.0)
     &        + (-0.18669*ALOG(H)) + (-0.01509*ALOG(BAREA))))*(D/1.0)
        ENDIF
        IF (CW .GT. 80.) CW=80.
C
C  CASE 20205 CROOKSTON (R6) MODEL 2
C  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR          
      CASE('20205')
        IF (EL .LT.   1.) EL=  1.
        IF (EL .GT.  75.) EL= 75.
        IF (D .GE. OMIND) THEN
          CW= 6.0227*BF*(D**0.54361)*(H**(-0.20669))*(CL**0.20395)*
     &       ((BAREA+1.0)**(-0.00644))*(EXP(EL)**(-0.00378))
        ELSE
          CW=(6.0227*1.0*(OMIND**0.54361)*(H**(-0.20669))*
     &        (CL**0.20395)*((BAREA+1.0)**(-0.00644))*
     &       (EXP(EL)**(-0.00378)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 80.) CW=80.
C
C  CASE 20206 DONNELLY (R6)
C  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR          
      CASE('20206')
        IF (D .GE. OMIND) THEN
          CW= 4.4215*D**0.5329
        ELSE
          CW= (4.4215*OMIND**0.5329)*(D/OMIND)
        ENDIF
        IF (CW .GT. 80.) CW=80.
C
C  CASE 20207 MOEUR
C  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR          
      CASE('20207')
        CW= EXP(3.02271+ 1.08137*ALOG(D) + (-1.00486*ALOG(H))
     &      + 0.29786*ALOG(CL))
        IF (CW .GT. 80.) CW=80.
C-----------------------------------------------------------------------
C  CASE 21104 CROOKSTON (R6) MODEL 2
C  211          SEQUOIA SEMPERVIRENS               REDWOOD
      CASE('21104')
        IF (D .GE. OMIND) THEN
          CW= 3.7023*D**0.52618
        ELSE
          CW= (3.7023*OMIND**0.52618)*(D/OMIND)
        ENDIF
        IF (CW .GT. 39.) CW=39.
C-----------------------------------------------------------------------
C  CASE 23104 CROOKSTON (R6) MODEL 2
C  231          TAXUS PLICATA                      PACIFIC YEW
      CASE('23104')
        IF (D .GE. OMIND) THEN
          CW= 6.1297*D**0.45424
        ELSE
          CW= (6.1297*OMIND**0.45424)*(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 24202 BECHTOLD 2004 MODEL 2
C  242          THUJA PLICATA                      WESTERN REDCEDAR      
      CASE('24202')
        IF (D .GE. MIND) THEN
          CW= 5.2911 + 1.0612*D + (-0.0153*D*D) + 0.0469*CR
        ELSE
          CW= (5.2911+1.0612*MIND+(-0.0153*MIND*MIND)+0.0469*CR)*
     &        (D/MIND)
        ENDIF
        IF (CW .GT. 38.) CW=38.
C
C  CASE 24203 CROOKSTON (R1)
C  242          THUJA PLICATA                      WESTERN REDCEDAR     
      CASE('24203')
        IF (D .GE. 1.0) THEN
          CW= 1.03597*EXP(1.46111 + 0.26289*ALOG(CL)
     &       + 0.18779*ALOG(D))
        ELSE
          CW= (1.03597*EXP(1.46111 + 0.26289*ALOG(CL)
     &       + 0.18779*ALOG(1.0)))*(D/1.0)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 24205 CROOKSTON (R6) MODEL 2
C  242          THUJA PLICATA                      WESTERN REDCEDAR     
      CASE('24205')
        IF (EL .LT.  1.) EL=  1.
        IF (EL .GT. 72.) EL= 72.
        IF (D .GE. OMIND) THEN
          CW= 6.2382*BF*(D**0.29517)*(H**(-0.10673))*(CL**0.23219)*
     &        ((BAREA+1.0)**0.05341)*(EXP(EL)**(-0.00787))
        ELSE
          CW=(6.2382*BF*(OMIND**0.29517)*(H**(-0.10673))*
     &       (CL**0.23219)*((BAREA+1.0)**0.05341)*
     &       (EXP(EL)**(-0.00787)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 24206 DONNELLY (R6)
C  242          THUJA PLICATA                      WESTERN REDCEDAR     
      CASE('24206')
        IF (D .GE. OMIND) THEN
          CW= 6.2318*D**0.4259
        ELSE
          CW= (6.2318*OMIND**0.4259)*(D/OMIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 24207 MOEUR
C  242          THUJA PLICATA                      WESTERN REDCEDAR     
      CASE('24207')
        CW= EXP(2.79784 + 1.08137*ALOG(D) + (-0.89666*ALOG(H))
     &       + 0.29786*ALOG(CL))
        IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 26302 BECHTOLD 2004 MODEL 2
C  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK      
      CASE('26302')
        IF (HI .LT. -34.) HI= -34.
        IF (HI .GT.  49.) HI=  49.
        IF (D .GE. MIND) THEN
          CW= -0.4624 + 1.0429*D + (-0.0078*D*D)
     &        + 0.1018*CR + (-0.0271*HI)
        ELSE
          CW= (-0.4624 + 1.0429*MIND + (-0.0078*MIND*MIND)
     &         + 0.1018*CR + (-0.0271*HI))*(D/MIND)
        ENDIF
        IF (CW .GT.54.) CW=54.
C
C  CASE 26303 CROOKSTON (R1)
C  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK      
      CASE('26303')
        IF (D .GE. 0.1) THEN
          CW= 1.02460*EXP(1.3522 + 0.24844*ALOG(CL)+0.412117*ALOG(D)
     &        + (-0.104357*ALOG(H)) + (0.03538*ALOG(BAREA)))
        ELSE
          CW=(1.02460*EXP(1.3522+0.24844*ALOG(CL)+0.412117*ALOG(0.1)
     &       + (-0.104357*ALOG(H)) + (0.03538*ALOG(BAREA))))*(D/0.1)
        ENDIF
        IF (CW .GT.54.) CW=54.
C
C  CASE 26305 CROOKSTON (R6) MODEL 2
C  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK      
      CASE('26305')
        IF (EL .LT.   1.) EL=  1.
        IF (EL .GT.  72.) EL= 72.
        IF (D .GE. OMIND) THEN
          CW= 6.0384*BF*(D**0.51581)*(H**(-0.21349))*(CL**0.17468)*
     &       ((BAREA+1.0)**(0.06143))*(EXP(EL)**(-0.00571))
        ELSE
          CW=(6.0384*BF*(OMIND**0.51581)*(H**(-0.21349))*
     &        (CL**0.17468)*((BAREA+1.0)**(0.06143))*
     &        (EXP(EL)**(-0.00571)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 54.) CW=54.
C
C  CASE 26306 DONNELLY (R6)
C  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK      
      CASE('26306')
        IF (D .GE. OMIND) THEN
          CW= 5.4864*D**0.5144
        ELSE
          CW= (5.4864*OMIND**0.5144)*(D/OMIND)
        ENDIF
        IF (CW .GT. 55.) CW=55.
C-----------------------------------------------------------------------
C  CASE 26402 BECHTOLD 2004 MODEL 2
C  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK      
      CASE('26402')
        IF (D .GE. MIND) THEN
          CW= -0.3362 + 0.7142*D + 0.0414*CR
        ELSE
          CW= (-0.3362 + 0.7142*MIND + 0.0414*CR)*(D/MIND)
        ENDIF
        IF (CW .GT.45.) CW=45.
C
C  CASE 26403 CROOKSTON (R1)
C  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK      
      CASE('26403')
        IF (H .GE. 5) THEN
          CW=.8*H*MAX(0.5,CR*0.01)
        ELSE IF (H .GE. 15) THEN
          CW=6.90396*(D**0.55645)*(H**(-0.28509))*(CL**0.20430)
        ELSE 
          CW=(0.8*H*MAX(0.5,CR*0.01))*(1-(H-5)*0.1)*6.90396
     &       *(D**0.55645)*(H**(-0.28509))*(CL**0.20430)*(H-5)*0.1
        IF (CW .GT.45.) CW=45.
        ENDIF
C
C  CASE 26405 CROOKSTON (R6) MODEL 2
C  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK      
      CASE('26405')
        IF (EL .LT.  10.) EL= 10.
        IF (EL .GT.  79.) EL= 79.
        IF (D .GE. OMIND) THEN
          CW= 3.7854*BF*(D**0.54684)*(H**(-0.12954))*(CL**0.16151)*
     &       ((BAREA+1.0)**(0.03047))*(EXP(EL)**(-0.00561))
        ELSE
          CW=(3.7854*BF*(OMIND**0.54684)*(H**(-0.12954))*
     &       (CL**0.16151)*((BAREA+1.0)**(0.03047))*
     &       (EXP(EL)**(-0.00561)))*(D/OMIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 26406 DONNELLY (R6)
C  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK
      CASE('26406')
        IF (D .GE. OMIND) THEN
          CW= 2.9372*D**0.5878
        ELSE
          CW= (2.9372*D**0.5878)*(D/OMIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 31202 BECTHOLD 2004 MODEL 2
C  312          ACER MACROPHYLLUM                   BIGLEAF MAPLE
      CASE('31202')
        IF (HI .LT. -36.) HI= -36.
        IF (HI .GT.  49.) HI=  49.
        IF (D .GE. MIND) THEN
          CW= -1.9386 + 1.2250*D + 0.1622*CR + (-0.1417*HI)
        ELSE
          CW= (-1.9386+1.2250*MIND+0.1622*CR+(-0.1417*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 57.) CW=57.
C
C  CASE 31206 DONNELLY (R6)
C  312          ACER MACROPHYLLUM                   BIGLEAF MAPLE
      CASE('31206')
        IF (D .GE. OMIND) THEN
          CW= 7.5183*D**0.4461
        ELSE
          CW= (7.5183*OMIND**0.4461)*(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 32102 BECTHOLD 2004 MODEL 2
C  321          ACER GLABRUM                       ROCKY MOUNTAIN MAPLE
      CASE('32102')
        IF (D .GE. MIND) THEN
          CW= 5.9765 + 0.8648*D + 0.0675*CR
        ELSE
          CW= (5.9765 + 0.8648*MIND + 0.0675*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 39.) CW=39.
C-----------------------------------------------------------------------
C  CASE 35102 BECTHOLD 2004 MODEL 2
C  351          ALNUS RUBRA                       RED ALDER
      CASE('35102')
        IF (D .GE. MIND) THEN
          CW= -0.7294 + 1.2885*D + 0.1307*CR
        ELSE
          CW= (-0.7294 + 1.2885*MIND + 0.1307*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C
C  CASE 35105 CROOKSTON (R6) MODEL 2
C  351          ALNUS RUBRA                       RED ALDER
      CASE('35105')
        IF (D .GE. OMIND) THEN
          CW= 4.1194*BF*(D**0.46252)*(CL**0.16180)
        ELSE
          CW= (4.1194*BF*(OMIND**0.46252)*(CL**0.16180))*(D/OMIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C
C  CASE 35106 DONNELLY (R6)
C  351          ALNUS RUBRA                       RED ALDER
      CASE('35106')
        IF (D .GE. OMIND) THEN
          CW= 7.0806*D**0.4771
        ELSE
          CW= (7.0806*OMIND**0.4771)*(D/OMIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C-----------------------------------------------------------------------
C  CASE 35202 BECTHOLD 2004 MODEL 2
C  352          ALNUS RHOMBIFOLIA                 WHITE ALDER
      CASE('35202')
        IF (D .GE. MIND) THEN
          CW= 4.6188 + 0.9135*D + 0.1019*CR
        ELSE
          CW= (4.6188 + 0.9135*MIND + 0.1019*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 50.) CW=50.
C-----------------------------------------------------------------------
C  CASE 36102 BECTHOLD 2004 MODEL 2
C  361          ARBUTUS MENZIESII                  PACIFIC MADRONE
      CASE('36102')
        IF (HI .LT. -55.) HI= -55.
        IF (HI .GT.  15.) HI=  15.
        IF (D .GE. MIND) THEN
          CW= 4.9133 + 0.9459*D + 0.0611*CR + (0.0523*HI)
        ELSE
          CW= (4.9133+0.9459*MIND+0.0611*CR+(0.0523*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 43.) CW=43.
C-----------------------------------------------------------------------
C  CASE 37506 DONNELLY (R6)
C  375          BETULA PAPYRIFERA                   PAPER BIRCH
      CASE('37506')
         IF (D .GE. OMIND) THEN
           CW= 5.8980*D**0.4841
         ELSE
           CW= (5.8980*OMIND**0.4841)*(D/OMIND)
         ENDIF
         IF (CW .GT. 25.) CW=25.
C-----------------------------------------------------------------------
C  CASE 47502 BECTHOLD 2004 MODEL 2
C  475         CERCOCARPUS LEDIFOLIUS       CURLLEAF MOUNATIN MAHOGANY
      CASE('47502')
        IF (HI .LT. -37.) HI= -37.
        IF (HI .GT.  27.) HI=  27.
        IF (D .GE. MIND) THEN
          CW= 4.0105 + 0.8611*D + (-0.0431*HI)
        ELSE
          CW= (4.0105 + 0.8611*MIND + (-0.0431*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 29.) CW=29.
C-----------------------------------------------------------------------
C  CASE 63102 BECTHOLD 2004 MODEL 2
C  631          LITHOCARPUS DENSIFLORUSI           TANOAK
      CASE('63102')
        IF (HI .LT. -55.) HI= -55.
        IF (HI .GT.  15.) HI=  15.
        IF (D .GE. MIND) THEN
          CW= 3.1150 + 0.7966*D + 0.0745*CR + (-0.0053*BAREA) +
     &       (0.0523*HI)
        ELSE
          CW= (3.1150 + 0.7966*MIND + 0.0745*CR + (-0.0053*BAREA)+
     &        (0.0523*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 41.) CW=41.
C-----------------------------------------------------------------------
C  CASE 74602 BECTHOLD 2004 MODEL 2
C  746          POPULUS TREMULOIDES                QUAKING ASPEN        
      CASE('74602')
        IF (HI .LT. -9.) HI= -9.
        IF (HI .GT. 33.) HI=  33.
        IF (D .GE. MIND) THEN
          CW= -0.5095 + 1.2318*D + 0.0744*CR + 0.0233*HI
        ELSE
          CW= (-0.5095+1.2318*MIND+0.0744*CR+0.0233*HI)*(D/MIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 74603 CROOKSTON (R1)
C  746          POPULUS TREMULOIDES                QUAKING ASPEN        
      CASE('74603')
        IF (D .GE. 0.1) THEN
          CW= 1.0396728*EXP(1.45297 + 0.060132*ALOG(D))
        ELSE
          CW= (1.0396728*EXP(1.45297+0.060132*ALOG(0.1)))*(D/0.1)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 74605 CROOKSTON (R6) MODEL 2
C  746          POPULUS TREMULOIDES                QUAKING ASPEN        
      CASE('74605')
        IF (D .GE. OMIND) THEN
          CW= 4.7961*BF*(D**0.64167)*(H**(-0.18695))*(CL**0.18581)
        ELSE
          CW=(4.7961*BF*(OMIND**0.64167)*(H**(-0.18695))*
     &       (CL**0.18581))*(D/OMIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C
C  CASE 74606 DONNELLY (R6)
C  746          POPULUS TREMULOIDES                QUAKING ASPEN        
      CASE('74606')
        IF (D .GE. OMIND) THEN
          CW= 4.0910*D**0.5907
        ELSE
          CW= (4.0910*OMIND**0.5907)*(D/OMIND)
        ENDIF
        IF (CW .GT. 45.) CW=45.
C-----------------------------------------------------------------------
C  CASE 74705 CROOKSTON (R6) MODEL 2
C  747          POPULUS TRICHOCARPA                BLACK COTTONWOOD
      CASE('74705')
        IF (D .GE. OMIND) THEN
          CW= 4.4327*BF*(D**0.41505)*(H**(-0.23264))*(CL**0.41477)
        ELSE
          CW=(4.4327*BF*(OMIND**0.41505)*(H**(-0.23264))*
     &       (CL**0.41477))*(D/OMIND)
        ENDIF
        IF (CW .GT. 56.) CW=56.
C-----------------------------------------------------------------------
C  CASE 74902 BECHTOLD 2004 MODEL 2
C  749          POPULUS ANGUSTIFOLIA               NARROWLEAF COTTONWOOD
      CASE('74902')
        IF (HI .LT. -26.) HI= -26.
        IF (HI .GT.  -2.) HI=  -2.
        IF (D .GE. MIND) THEN
          CW= 4.1687 + 1.5355*D + 0.1275*HI
        ELSE
          CW= (4.1687 + 1.5355*MIND + 0.1275*HI)*(D/MIND)
        ENDIF
        IF (CW .GT. 35.) CW=35.
C-----------------------------------------------------------------------
C  CASE 80102 BECHTOLD 2004 MODEL 2
C  801          QUERCUS AGRIFOLIA                 COASTAL LIVE OAK
      CASE('80102')
        IF (HI .LT. -73.) HI= -73.
        IF (HI .GT. -54.) HI= -54.
        IF (D .GE. MIND) THEN
          CW= -16.1696 + 1.7456*D + 0.0925*CR + (-0.1956*HI)
        ELSE
          CW= (-16.1696+1.7456*MIND+0.0925*CR+(-0.1956*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 53.) CW=53.
C-----------------------------------------------------------------------
C  CASE 80502 BECHTOLD 2004 MODEL 2
C  805          QUERCUS CHRYSOLEPIS                CANYON LIVE OAK
      CASE('80502')
        IF (HI .LT. -60.) HI= -60.
        IF (HI .GT.  -5.) HI=  -5.
        IF (D .GE. MIND) THEN
          CW= 0.2738 + 1.0534*D + 0.035*CR + (-0.1385*HI)
        ELSE
          CW= (0.2738+1.0534*MIND+0.035*CR+(-0.1385*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 49.) CW=49.
C-----------------------------------------------------------------------
C  CASE 80702 BECHTOLD 2004 MODEL 2
C  807          QUERCUS DOUGLASII                  BLUE OAK
      CASE('80702')
        IF (D .GE. MIND) THEN
          CW= 2.7110 + 1.5159*D + 0.0415*CR+(-0.0271*BAREA)
        ELSE
          CW= (2.7110+1.5159*MIND+0.0415*CR+(-0.0271*BAREA))*(D/MIND)
        ENDIF
        IF (CW .GT. 61.) CW=61.
C-----------------------------------------------------------------------
C  CASE 81402 BECHTOLD 2004 MODEL 2
C  814          QUERCUS GAMBELII                   GAMBEL OAK
      CASE('81402')
        IF (D .GE. MIND) THEN
          CW= 0.3309 + 0.8918*D + 0.0510*CR
        ELSE
          CW= (0.3309 + 0.8918*MIND+ 0.0510*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 19.) CW=19.
C-----------------------------------------------------------------------
C  CASE 81502 BECTHOLD 2004 MODEL 2
C  815          QUERCUS GARRYANA                   OREGON WHITE OAK
      CASE('81502')
        IF (D .GE. MIND) THEN
          CW= -1.3160 + 2.9311*D + (-0.0866*D*D)
        ELSE
          CW= (-1.3160 + 2.9311*MIND+(-0.0866*MIND*MIND))*(D/MIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C
C  CASE 81505 CROOKSTON (R6) MODEL 2
C  815          QUERCUS GARRYANA                   OREGON WHITE OAK
      CASE('81505')
        IF (D .GE. OMIND) THEN
          CW= 2.4857*BF*(D**0.70862)*(CL**0.10168)
        ELSE
          CW= (2.4857*BF*(OMIND**0.70862)*(CL**0.10168))*(D/OMIND)
        ENDIF
        IF (CW .GT. 39.) CW=39.
C
C  CASE 81506 DONNELLY (R6)
C  815          QUERCUS GARRYANA                   OREGON WHITE OAK       
      CASE('81506')
        IF (D .GE. OMIND) THEN
          CW= 2.4922*D**0.8544
        ELSE
          CW= (2.4922*OMIND**0.8544)*(D/OMIND)
        ENDIF
        IF (CW .GT. 30.) CW=30.
C-----------------------------------------------------------------------
C  CASE 81802 BECTHOLD 2004 MODEL 2
C  818          QUERCUS KELLOGGII                  CALIFORNIA BLACK OAK
      CASE('81802')
        IF (HI .LT. -47.) HI= -47.
        IF (HI .GT.  -8.) HI=  -8.
        IF (D .GE. MIND) THEN
          CW= 1.6306 + 0.9867*D + 0.0556*CR + (-0.1199*HI)
        ELSE
          CW= (1.6306+0.9867*MIND+0.0556*CR+(-0.1199*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 52.) CW=52.
C-----------------------------------------------------------------------
C  CASE 82102 BECTHOLD 2004 MODEL 2
C  821          QUERCUS LOBATA                  VALLEY OAK
      CASE('82102')
        IF (D .GE. MIND) THEN
          CW= -2.1068 + 1.9385*D + 0.0860*CR
        ELSE
          CW= (-2.1068 + 1.9385*MIND + 0.0860*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 47.) CW=47.
C-----------------------------------------------------------------------
C  CASE 83902 BECHTOLD 2004 MODEL 2
C  839          QUERCUS WISLIZENII                 INTERIOR LIVE OAK
      CASE('83902')
        IF (HI .LT. -60.) HI= -60.
        IF (HI .GT.  -5.) HI= - 5.
        IF (D .GE. MIND) THEN
          CW= 0.7146 + 1.5460*D + (-0.1121*HI)
        ELSE
          CW= (0.7146 + 1.5460*MIND + (-0.1121*HI))*(D/MIND)
        ENDIF
        IF (CW .GT. 37.) CW=37.
C-----------------------------------------------------------------------
C  CASE 98102 BECTHOLD 2004 MODEL 2
C  981           UMBELLULARIA CALIFORNICA          CALIFORNIA LAUREL
      CASE('98102')
        IF (D .GE. MIND) THEN
          CW= 2.4247 + 1.3174*D + 0.0786*CR
        ELSE
          CW= (2.4247 + 1.3174*MIND + 0.0786*CR)*(D/MIND)
        ENDIF
        IF (CW .GT. 44.) CW=44.
C-----------------------------------------------------------------------
      END SELECT
 9000 CONTINUE
C----------
C  LIMIT CROWN WIDTH
C----------
      IF (CW .LT. 0.5) CW=0.5
      IF (CW .GT. 99.9) CW=99.9
      RETURN
      END