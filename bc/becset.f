      SUBROUTINE BECSET
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     THE PRINCIPAL TASK IS TO RESOLVE SITE AND BEC INFORMATION
C     PROVIDED BY THE BECINFO KEYWORD, DETERMINE WHICH OF THE
C     PROGNOSIS (NI-VARIANT) HABITAT CODES IS MATCHED WITH THE 
C     BEC VARIANT, AND PROVIDE SPECIES- AND BEC-SPECIFIC ADJUSTMENT
C     COEFFICIENTS TO BE APPLIED TO THE LARGE TREE DIAMETER GROWTH
C     MODEL. COEFFICIENTS ARE BASED ON FITTING A LINEAR REGRESSION
C     MODEL TO THE DBH SCALING TERM REPORTED BY PROGNOSIS, APPLIED
C     TO PERMANENT SAMPLE PLOT TREELISTS STRATIFIED BY BEC, IN THE 
C     KAMLOOPS AND NELSON FOREST REGIONS.
C
C     THE 'KAMCODE' AND 'NELCODE' STRINGS ARE SET UP SO THAT IT IS
C     EASY TO FIND THE BEC VARIANT WHEN READING THE SOURCE CODE.
C     LIKEWISE, THE BEC-SPECIFIC ADJUSTMENTS ARE SET UP IN THE SAME
C     PATTERN, SO THAT IT IS QUITE EASY TO CHECK THE VALUES AND ADD
C     NEW OR MODIFIED VALUES IN THE FUTURE. I ALSO KEPT THE NELSON
C     AND KAMLOOPS COEFFICIENTS SEPARATE, EVEN THOUGH THEY ARE 
C     IDENTICAL AT PRESENT. THIS WILL ALLOW REGION-SPECIFIC
C     COEFFICIENTS TO BE ADDED IN THE FUTURE, IF THAT IS DEEMED
C     NECESSARY.
C
C     THE KAMCOD2 AND NELCOD2 STRINGS ARE SET UP TO IDENTIFY THE
C     BEC SUBZONES WHERE PARAMETERIZATION IS VALID. SPECIES THAT 
C     ARE UNPARAMETERIZED FOR A PARTICULAR SUBZONE ARE FLAGGED .TRUE.
C     OR .FALSE. AND PASSED TO BECGET. THIS IS NECESSARY BECAUSE
C     SOME ZEROES IN THE NCOEF AND KCOEF ARRAYS ARE TRUE ZEROES --
C     THE MODEL USES ONLY THE INTERCEPT TERM -- WHILE OTHER ZEROES
C     ARE PLACEHOLDERS, AND EVEN THE INTERCEPT SHOULD NOT BE USED.
C
C     - DON ROBINSON; ESSA TECHNOLOGIES, VANCOUVER. DEC/97; JULY/99
C     - ABDEL-AZIM ZUMRAWI: MAY 18, 1999: CCF & XBEC7 ARRAYS
C     - DON ROBINSON:  JULY,        1999: HEIGHT DUBBING COEFFICIENTS
C                                         FROM T. HAILMARIAM
C     - DON ROBINSON:  NOVEMBER 23, 1999: ADDED BAMAX CHANGES
C     - DON ROBINSON:  NOVEMBER 24, 1999: HEIGHT GROWTH ADJUSTMENT
C     - DON ROBINSON:  DECEMBER  8, 1999: MORTALITY ADJUSTMENT
C     - ABDEL-AZIM ZUMRAWI: MARCH 15, 2000: IDFdk3 SUBZONE ADDED TO KAM
C     - DON ROBINSON:  OCTOBER 10, 2000: UPDATE FD,PL LARGE TREE
c     - DON ROBINSON:  FEBRUARY 12, 2001; ADD IDFdk SMALL TREE HEIGHT GROWTH FROM C. LENCAR
C----------

      INCLUDE  'PRGPRM.F77'
      INCLUDE  'PLOT.F77'
      INCLUDE  'COEFFS.F77'
      INCLUDE  'CONTRL.F77'
      INCLUDE  'METRIC.F77'
      INCLUDE  'BCPLOT.F77'

C     NUMBER OF ENTRIES FOR NELSON (NCNT) AND KAMLOOPS (KCNT)
C     FOR ALL SITE SERIES

      INTEGER   NCNT,KCNT
      INTEGER   NCNT2,KCNT2,ACNT2

      PARAMETER (NCNT=20)   ! was 52
      PARAMETER (KCNT=11)   ! was 37

C     NUMBER OF BEC/SUBZONE ENTRIES FOR NELSON (NCNT2) AND KAMLOOPS (KCNT2)
C     AND COMBINED (ACNT2)

      PARAMETER (NCNT2= 9) ! was 19
      PARAMETER (KCNT2= 6) ! was 18
      PARAMETER (ACNT2=12) ! was 27

C     DECLARE STRUCTURE FOR LARGE TREE HEIGHT GROWTH MODIFIER
C     AND MORTALITY MODIFIER

      TYPE LTHG_STR
        CHARACTER (LEN=7) :: BECszn    = " "
        LOGICAL           :: OK        = .FALSE.
        INTEGER           :: Lo(MAXSP)
        INTEGER           :: Hi(MAXSP)
      END TYPE

C     LBEC8 IS A COPY OF LTDGOK

      LOGICAL      LBEC8(MAXSP), LOK1, LOK2

      CHARACTER*1  CH(MAXSP)
      CHARACTER*44 SPLST
      CHARACTER*7  VVER

      CHARACTER*10 KAMCODE(KCNT),  NELCODE(NCNT)
      CHARACTER*7 KAMCOD2(KCNT2), NELCOD2(NCNT2)
      INTEGER     KAMINDX(KCNT), NELINDX(NCNT)
      INTEGER     I,J,IP
      INTEGER     IREGN, IBEC, INOSPP, IBECX, IBECY

C     NEED TO REMEMBER THIS BETWEEN CALLS
      INTEGER     JBECY
      COMMON  /BECREF/  JBECY
      
C     ENTRY SEISTHG VARS
      INTEGER     ISP1
      REAL        X01,X11,X21,X31,X41,X51,X61

C     ENTRY SEILTHG VARS
      INTEGER     ISP2
      REAL        X02,XHT22,ALGSLP

C     ENTRY SEIMORT VARS
      INTEGER     ISP3
      REAL        X03,XDIA3

      REAL        INTRCP(MAXSP),BECTERM(MAXSP)
      REAL        SEISLP(MAXSP),SEISAS(MAXSP),SEICAS(MAXSP),
     &            SEIELV(MAXSP),SEIEL2(MAXSP),SEICCF(MAXSP)
      REAL        XBEC1(MAXSP),XBEC2(MAXSP),XBEC3(MAXSP),
     &            XBEC4(MAXSP),XBEC5(MAXSP),XBEC6(MAXSP),
     &            XBEC7(MAXSP)
      REAL        BECADJ(3),XBEC0(3)
      REAL        XHT1(MAXSP), XHT2(MAXSP)
      REAL        XLTHG(4), YLTHG(4), XMORT(4), YMORT(4)

C     LOGICAL ARRAYS FOR SPECIES/REGION. FOR THE LARGE
C     TREE DIAMETER GROWTH MODEL. IF .FALSE. NO
C     BEC PARAMETERS EXIST FOR THE SPECIES, EVEN THOUGH
C     THERE MAY BE PARAMETERS FOR THE BEC

      LOGICAL     LNOK(NCNT2,MAXSP)
      LOGICAL     LNPW(NCNT2), LNLW(NCNT2), LNFD(NCNT2), LNBG(NCNT2),
     &            LNHW(NCNT2), LNCW(NCNT2), LNPL(NCNT2), LNSE(NCNT2),
     &            LNBL(NCNT2), LNPY(NCNT2), LNEP(NCNT2), LNAT(NCNT2),
     &            LNAC(NCNT2), LNOC(NCNT2), LNOH(NCNT2)
      LOGICAL     LKOK(KCNT2,MAXSP)     
      LOGICAL     LKPW(KCNT2), LKLW(KCNT2), LKFD(KCNT2), LKBG(KCNT2),
     &            LKHW(KCNT2), LKCW(KCNT2), LKPL(KCNT2), LKSE(KCNT2),
     &            LKBL(KCNT2), LKPY(KCNT2), LKEP(KCNT2), LKAT(KCNT2),
     &            LKAC(KCNT2), LKOC(KCNT2), LKOH(KCNT2)

      EQUIVALENCE (LNPW, LNOK(1, 1)), (LKPW, LKOK(1, 1))
      EQUIVALENCE (LNLW, LNOK(1, 2)), (LKLW, LKOK(1, 2))
      EQUIVALENCE (LNFD, LNOK(1, 3)), (LKFD, LKOK(1, 3))
      EQUIVALENCE (LNBG, LNOK(1, 4)), (LKBG, LKOK(1, 4))
      EQUIVALENCE (LNHW, LNOK(1, 5)), (LKHW, LKOK(1, 5))
      EQUIVALENCE (LNCW, LNOK(1, 6)), (LKCW, LKOK(1, 6))
      EQUIVALENCE (LNPL, LNOK(1, 7)), (LKPL, LKOK(1, 7))
      EQUIVALENCE (LNSE, LNOK(1, 8)), (LKSE, LKOK(1, 8))
      EQUIVALENCE (LNBL, LNOK(1, 9)), (LKBL, LKOK(1, 9))
      EQUIVALENCE (LNPY, LNOK(1,10)), (LKPY, LKOK(1,10))
      EQUIVALENCE (LNEP, LNOK(1,11)), (LKEP, LKOK(1,11))
      EQUIVALENCE (LNAT, LNOK(1,12)), (LKAT, LKOK(1,12))
      EQUIVALENCE (LNAC, LNOK(1,13)), (LKAC, LKOK(1,13))      
      EQUIVALENCE (LNOC, LNOK(1,14)), (LKOC, LKOK(1,14))
      EQUIVALENCE (LNOH, LNOK(1,15)), (LKOH, LKOK(1,15))

C     REAL ARRAYS FOR SPECIES/REGION FOR THE LARGE TREE
C     DIAMETER GROWTH MODEL

      REAL        NCOEF(NCNT,MAXSP)
      REAL        NPW(NCNT), NLW(NCNT), NFD(NCNT), NBG(NCNT),
     &            NHW(NCNT), NCW(NCNT), NPL(NCNT), NSE(NCNT),
     &            NBL(NCNT), NPY(NCNT), NEP(NCNT), NAT(NCNT),
     &            NAC(NCNT), NOC(NCNT), NOH(NCNT)
      REAL        KCOEF(KCNT,MAXSP)     
      REAL        KPW(KCNT), KLW(KCNT), KFD(KCNT), KBG(KCNT),
     &            KHW(KCNT), KCW(KCNT), KPL(KCNT), KSE(KCNT),
     &            KBL(KCNT), KPY(KCNT), KEP(KCNT), KAT(KCNT),
     &            KAC(KCNT), KOC(KCNT), KOH(KCNT)

      EQUIVALENCE (NPW, NCOEF(1, 1)), (KPW, KCOEF(1, 1))
      EQUIVALENCE (NLW, NCOEF(1, 2)), (KLW, KCOEF(1, 2))
      EQUIVALENCE (NFD, NCOEF(1, 3)), (KFD, KCOEF(1, 3))
      EQUIVALENCE (NBG, NCOEF(1, 4)), (KBG, KCOEF(1, 4))
      EQUIVALENCE (NHW, NCOEF(1, 5)), (KHW, KCOEF(1, 5))
      EQUIVALENCE (NCW, NCOEF(1, 6)), (KCW, KCOEF(1, 6))
      EQUIVALENCE (NPL, NCOEF(1, 7)), (KPL, KCOEF(1, 7))
      EQUIVALENCE (NSE, NCOEF(1, 8)), (KSE, KCOEF(1, 8))
      EQUIVALENCE (NBL, NCOEF(1, 9)), (KBL, KCOEF(1, 9))
      EQUIVALENCE (NPY, NCOEF(1,10)), (KPY, KCOEF(1,10))
      EQUIVALENCE (NEP, NCOEF(1,11)), (KEP, KCOEF(1,11))
      EQUIVALENCE (NAT, NCOEF(1,12)), (KAT, KCOEF(1,12))
      EQUIVALENCE (NAC, NCOEF(1,13)), (KAC, KCOEF(1,13))
      EQUIVALENCE (NOC, NCOEF(1,14)), (KOC, KCOEF(1,14))
      EQUIVALENCE (NOH, NCOEF(1,15)), (KOH, KCOEF(1,15))
      
      TYPE (LTHG_STR) LTHGMOD(ACNT2)
      TYPE (LTHG_STR) MORTMOD(ACNT2)

C     PARAMETERS TO SHIFT (SET TO CONSTANT VALUES): THE
C     SLOPE (37%), ASPECT (225 DEGREES) AND ELEVATION 
C     (1276 METRES) OF THE BASE MODEL. THIS IS REQUIRED 
C     BECAUSE THESE TERMS WERE ALL HELD FIXED AT THE NI-AVERAGE
C     VALUE WHEN THE ADJUSTMENT MODEL WAS FITTED. 

      DATA BECADJ / 37, 225, 1276 /

C     PARAMETERS FOR INTERCEPT, BY SPECIES (BOTH REGIONS) FOR
C     LARGE TREE DIAMETER GROWTH

      DATA INTRCP / -1.200116,  0.031636, -0.379727, -0.354087,
     &              -0.531580,  0.303820, -0.592774, -1.467097,
     &              -0.655108, -2.015720,  0.0,       0.0,
     &               0.0,      -0.379727,  0.0 /

C     PARAMETERS FOR SLOPE, SIN ASPECT, COS ASPECT, ELEVATION
C     ELEVATION**2 AND CCF (BOTH REGIONS) FOR LARGE TREE DIAMETER
C     GROWTH

      DATA SEISLP /  0.0     ,  0.0     ,  0.0     ,  0.0     ,
     &               0.0     ,  0.0     ,  0.0     , -0.819996,
     &               0.0     ,  0.0     ,  0.0     ,  0.0,
     &               0.0     ,  0.0     ,  0.0      /

      DATA SEISAS /  0.0     ,  0.0     ,  0.000728,  0.0     ,
     &               0.0     ,  0.0     ,  0.0     ,  0.0     ,
     &               0.0     , -1.114708,  0.0      ,  0.0,
     &               0.0     ,  0.000728,  0.0      /
     
      DATA SEICAS /  0.0     ,  0.0     ,  0.0     , -0.406713,
     &               0.0     ,  0.0     , -0.091611,  0.0     ,
     &              -0.746618,  0.0     ,  0.0      ,  0.0,
     &               0.0     ,  0.0     ,  0.0      /

      DATA SEIELV /  0.017243,  0.0     ,  0.000369,  0.057385,
     &               0.046852,  0.0     ,  0.010962,  0.072226,
     &               0.071289,  0.061512,  0.0     ,  0.0,
     &               0.0     ,  0.000369,  0.0      /

      DATA SEIEL2 /  0.0     ,  0.0     ,  0.0     , -0.000847,
     &              -0.000680,  0.0     ,  0.0     , -0.000898,
     &              -0.000987,  0.0     ,  0.0     ,  0.0,
     &               0.0     ,  0.0     ,  0.0      /

      DATA SEICCF /  0.0     ,  0.0     , -0.064699,  0.0     ,
     &               0.0     ,  0.0     , -0.160717,  0.0     ,
     &               0.0     ,  0.0     ,  0.0     ,  0.0,
     &               0.0     , -0.064699,  0.0      /

C     ALLOWABLE BEC VARIANTS FOR KAMLOOPS

      DATA KAMCODE /  'PPxh1/01', 'PPxh1/03',  'PPxh1/06',  'PPxh1/07',
     &                'PPxh2/03',
     &                 'MSdk/01',  'MSdk/03',  'MSdk/04',
     &                'MSdm1/04',
     &                 'MSxk/05',
     &              'ESSFdc1/03' /

C     BEC/VARIANT CODES (ONE FOR EACH LINE OF PRECEDING BLOCK)

      DATA KAMCOD2 /  'PPxh1',
     &                'PPxh2',
     &                'MSdk',
     &                'MSdm1',
     &                'MSxk',
     &              'ESSFdc1' /

C     CORRESPONDING HABITAT CODES FOR KAMLOOPS

      DATA KAMINDX / 130, 130, 310, 310,
     &               130,
     &               660, 470, 250,
     &               250,
     &               320,
     &               730 /

C     ALLOWABLE BEC VARIANTS FOR NELSON

      DATA NELCODE /  'PPdh1/01',
     &                'PPdh2/01',
     &                 'MSdk/01',   'MSdk/03',   'MSdk/04',  'MSdk/05',
     &                'MSdm1/01',  'MSdm1/02',  'MSdm1/03', 'MSdm1/04',
     &               'ESSFdk/01', 'ESSFdk/04',
     &              'ESSFdc1/03',
     &               'ESSFwm/01', 'ESSFwm/02', 'ESSFwm/03','ESSFwm/04',
     &              'ESSFwc1/02',
     &              'ESSFwc4/02','ESSFwc4/03' / 

C     BEC/VARIANT CODES (ONE FOR EACH LINE OF PRECEDING BLOCK)

      DATA NELCOD2 /  'PPdh1',
     &                'PPdh2',
     &                'MSdk',
     &                'MSdm1',
     &              'ESSFdk',
     &              'ESSFdc1',
     &              'ESSFwm',
     &              'ESSFwc1',
     &              'ESSFwc4' / 

C     CORRESPONDING HABITAT CODES FOR NELSON

      DATA NELINDX / 130,
     &               130,
     &               660, 470, 250, 420,
     &               620, 320, 730, 250,
     &               670, 670,
     &               730,
     &               620, 670, 670, 620,
     &               640,
     &               670, 670 /

C     BEC-SPECIFIC PARAMETERS FOR NELSON WHITE PINE (PW)

      DATA NPW /  0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNPW / .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .TRUE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON WESTERN LARCH (LW)

      DATA NLW /  0.0,
     &            0.0,
     &           -0.477190,-0.477190,-0.134774,-0.134774,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNLW / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON DOUGLAS-FIR (FD)

      DATA NFD /  0.0,
     &            0.0,
     &            0.216504, 0.216504, 0.216504, 0.216504,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNFD / .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON GRAND FIR (BG)

      DATA NBG /  0.0,
     &            0.0,
     &           -0.496461,-0.496461,-0.496461,-0.496461,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.268814, 0.268814, 0.268814, 0.268814,
     &            0.0,
     &           -0.680737,-0.680737 /

      DATA LNBG / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON WESTERN HEMLOCK (HW)

      DATA NHW /  0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNHW / .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON WESTERN REDCEDAR (CW)

      DATA NCW /  0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNCW / .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON LODGEPOLE PINE (PL)

      DATA NPL /  0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNPL / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .TRUE. /


C     BEC-SPECIFIC PARAMETERS FOR NELSON ENGLEMANN SPRUCE (SE)

      DATA NSE /  0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.265099, 0.0,
     &            0.0,
     &            0.720272, 0.720272, 0.720272, 0.720272,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNSE / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON SUBALPINE FIR (BL)

      DATA NBL /  0.0,
     &            0.0,
     &           -0.496461,-0.496461,-0.496461,-0.496461,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.268814, 0.268814, 0.268814, 0.268814,
     &            0.0,
     &           -0.680737,-0.680737 /

      DATA LNBL / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .TRUE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON PONDEROSA PINE (PY)

      DATA NPY /  0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNPY / .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON (EP,AT,AC)

      DATA NEP  /  NCNT * 0.0 /
      DATA LNEP / NCNT2 * .FALSE. /

      DATA NAT  /  NCNT * 0.0 /
      DATA LNAT / NCNT2 * .FALSE. /

      DATA NAC  /  NCNT * 0.0 /
      DATA LNAC / NCNT2 * .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON OTHER CONIFER (OC = FD)

      DATA NOC /  0.0,
     &            0.0,
     &            0.216504, 0.216504, 0.216504, 0.216504,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0 /

      DATA LNOC /  .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR NELSON OTHER HARDWOOD(OH)
      
      DATA NOH  /  NCNT * 0.0 /
      DATA LNOH / NCNT2 * .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WHITE PINE (PW)

      DATA KPW /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKPW / .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WESTERN LARCH (LW)

      DATA KLW /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &           -0.477190,-0.477190,-0.134774, 
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKLW / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS DOUGLAS-FIR (FD)

      DATA KFD /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.216504,0.216504,0.216504,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKFD / .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS GRAND FIR (BG)

      DATA KBG /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &           -0.496461,-0.496461,-0.496461,
     &            0.0,
     &            0.0,
     &            0.0  /
 
      DATA LKBG / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WESTERN HEMLOCK (HW)

      DATA KHW /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKHW / .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WESTERN REDCEDAR (CW)

      DATA KCW /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKCW / .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS LODGEPOLE PINE (PL)

      DATA KPL /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKPL / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS ENGLEMANN SPRUCE (SE)

      DATA KSE /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKSE / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS SUBALPINE FIR (BL)

      DATA KBL /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &           -0.496461,-0.496461,-0.496461,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKBL / .FALSE.,
     &            .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS PONDEROSA PINE (PY)

      DATA KPY /  0.0,      0.0,      0.0,      0.0,
     &           -0.597169,
     &            0.0,      0.0,      0.0,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKPY / .FALSE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS (EP,AT,AC)

      DATA KEP  /  KCNT * 0.0 /
      DATA LKEP /  KCNT2 * .FALSE. /

      DATA KAT  /  KCNT * 0.0 /
      DATA LKAT /  KCNT2 * .FALSE. /

      DATA KAC  /  KCNT * 0.0 /
      DATA LKAC /  KCNT2 * .FALSE. /
      
C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS OTHER CONIFER (OC=FD)

      DATA KOC /  0.0,      0.0,      0.0,      0.0,
     &            0.0,
     &            0.216504,0.216504,0.216504,
     &            0.0,
     &            0.0,
     &            0.0  /

      DATA LKFD / .FALSE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .TRUE.,
     &            .FALSE.,
     &            .FALSE. /

C     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS OTHER CONIFER (OC=FD)

      DATA KOH  /  KCNT * 0.0 /
      DATA LKOH /  KCNT2 * .FALSE. /

C     THIS STRUCTURE INITIALIZES THE HEIGHT GROWTH REPARAMETERIZATION
C     THE FIRST ELEMENT IS THE SUBZONE (WITHOUT REFERENCE TO THE KAM
C     OR NEL REGION); THE 2ND ITEM STATES WHETHER THERE IS A REPARAM-
C     ETERIZATION WITHIN THE SUBZONE; THE PAIR OF VECTORS CONTAINS
C     THE BREAKPOINTS (M HEIGHT )FOR THE REPARAMETERIZATION, GIVEN THAT
C     THE 2ND ITEM IS .TRUE. DECIDUOUS NOT ORIGINALLY INCLUDED IN v2 -
C     SPP 12,13,15 (AT,AC,OH) - USE THE SPP 11 VALUE (EP); SPP 14 (OC)
C     NOT IN v2 USES THE SPP 3 VALUE (FD).

      DATA LTHGMOD /

     &  LTHG_STR ('ESSFdc1', .FALSE., MAXSP * 0, MAXSP * 0),

     &  LTHG_STR ('ESSFdk', .TRUE.,
     >    (/ 0,31,31, 0, 0, 0,25,28,26, 0,25,25,25,31,25/),
     >    (/ 5,35,35, 5, 5, 5,32,37,32, 5,32,32,32,35,32/)),

     &  LTHG_STR ('ESSFwc1', .FALSE., MAXSP * 0, MAXSP * 0),

     &  LTHG_STR ('ESSFwc4', .TRUE.,
     >    (/ 0, 0, 0, 0, 0, 0,23,37,29, 0,23,23,23, 0,23/),
     >    (/ 5, 5, 5, 5, 5, 5,30,49,36, 5,30,30,30, 5,30/)),

     &  LTHG_STR ('ESSFwm', .TRUE.,
     >    (/31,31,32, 0,35, 0,24,35,30, 0,24,24,24,32,24/),
     >    (/35,35,36, 5,41, 5,27,47,41, 5,27,27,27,36,27/)),

     &  LTHG_STR ('MSdk', .TRUE.,
     >    (/ 0,36,27, 0, 0, 0,25,31,31, 0,25,25,25,27,25/),
     >    (/ 5,41,33, 5, 5, 5,30,35,35, 5,30,30,30,33,30/)),

     &  LTHG_STR ('MSdm1', .TRUE.,
     >    (/ 0,36,31, 0, 0, 0,23,31,28, 0,23,23,23,31,23/),
     >    (/ 5,45,37, 5, 5, 5,29,35,33, 5,29,29,29,37,29/)),

     &  LTHG_STR ('MSxk', .FALSE., MAXSP * 0, MAXSP * 0),

     >  LTHG_STR ('PPdh1', .FALSE., MAXSP * 0, MAXSP * 0),

     >  LTHG_STR ('PPdh2', .TRUE.,
     >    (/0,24,24, 0, 0, 0,24,36, 0,25,33,33,33,24,33/),
     >    (/5,24,24, 5, 5, 5,24,46, 5,31,42,42,42,24,42/)),

     >  LTHG_STR ('PPxh1', .FALSE., MAXSP * 0, MAXSP * 0),

     >  LTHG_STR ('PPxh2', .TRUE.,
     >    (/ 0, 0,28, 0, 0, 0, 0, 0, 0,25,33,33,33,28,33/),
     >    (/ 5, 5,34, 5, 5, 5, 5, 5, 5,31,42,42,42,34,42/))/

C     DEFAULT X AND Y BREAKPOINTS FOR THE LARGE TREE HEIGHT GROWTH
C     ADJUSTMENT INTERPOLATION

	DATA XLTHG / 0., 0., 5.,999. /
      DATA YLTHG / 1., 1., 0.,  0. /
C
C     THIS STRUCTURE INITIALIZES THE MORTALITY REPARAMETERIZATION
C     THE FIRST ELEMENT IS THE SUBZONE (WITHOUT REFERENCE TO THE KAM
C     OR NEL REGION); THE 2ND ITEM STATES WHETHER THERE IS A REPARAM-
C     ETERIZATION WITHIN THE SUBZONE; THE PAIR OF VECTORS CONTAINS
C     THE BREAKPOINTS (CM DIAMETER) FOR THE REPARAMETERIZATION, GIVEN
C     THAT THE 2ND ITEM IS .TRUE.
C     
      DATA MORTMOD /

     &  LTHG_STR ('ESSFdc1', .FALSE., MAXSP * 0, MAXSP * 0),

     &  LTHG_STR ('ESSFdk', .TRUE.,
     >    (/  0, 61, 59,  0,  0,  0, 31, 49, 39, 0, 0, 0, 0, 59, 0/),
     >    (/  5, 70,126,  5,  5,  5, 50,113, 57, 5, 5, 5, 5,126, 5/)),

     &  LTHG_STR ('ESSFwc1', .FALSE., MAXSP * 0, MAXSP * 0),

     &  LTHG_STR ('ESSFwc4', .TRUE.,
     >    (/  0,  0,  0,  0,  0,  0, 37, 71, 47, 0, 0, 0, 0, 0, 0/),
     >    (/  5,  5,  5,  5,  5,  5, 50,140, 86, 5, 5, 5, 5, 5, 5/)),

     &  LTHG_STR ('ESSFwm', .TRUE.,
     >    (/ 70, 58, 70,  0, 65,  0, 31, 65, 50,  0, 0, 0, 0,70, 0/),
     >    (/ 90, 73, 90,  5,125,  5, 50,125, 85,  5, 5, 5, 5,90, 5/)),

     &  LTHG_STR ('MSdk', .TRUE.,
     >    (/  0, 63, 60,  0,  0,  0, 31, 53, 38, 0, 0, 0, 0,60, 0/),
     >    (/  5, 94, 67,  5,  5,  5, 51, 74, 54, 5, 5, 5, 5,67, 5/)),

     &  LTHG_STR ('MSdm1', .TRUE.,
     >    (/  0, 70, 57,  0,  0,  0, 28, 48, 42, 0, 0, 0, 0, 57, 0/),
     >    (/  5, 89,102,  5,  5,  5, 51, 66, 73, 5, 5, 5, 5,102, 5/)),

     &  LTHG_STR ('MSxk', .FALSE., MAXSP * 0, MAXSP * 0),

     >  LTHG_STR ('PPdh1', .FALSE., MAXSP * 0, MAXSP * 0),

     >  LTHG_STR ('PPdh2', .TRUE.,
     >    (/  0, 51, 51,  0,  0,  0, 51, 51,  0,62, 0, 0, 0,51, 0/),
     >    (/  5, 65, 65,  5,  5,  5, 65, 65,  5,63, 5, 5, 5,65, 5/)),

     >  LTHG_STR ('PPxh1', .FALSE., MAXSP * 0, MAXSP * 0),

     >  LTHG_STR ('PPxh2', .TRUE.,
     >    (/  0,  0, 65,  0,  0,  0,  0,  0,  0,66, 0, 0, 0,65, 0/),
     >    (/  5,  5, 95,  5,  5,  5,  5,  5,  5,80, 5, 5, 5,95, 5/))/

C     DEFAULT X AND Y BREAKPOINTS FOR THE LARGE MORTALITY MODEL
C     ADJUSTMENT INTERPOLATION

	DATA XMORT / 0., 0., 5.,999. /
      DATA YMORT / 0., 0., 1.,  1. /
     
C     SEE IF THE STAND'S BEC ZONE IS ICH, IDF, SBPS OR SBS; IN WHICH CASE
C     THE STAND IS SIMULATED WITH THE VERSION 3 CALBIRATION AND THE REST
C     OF THE ROUTINE IS SKIPPED.

      LV2ATV = .TRUE.
      IF (INDEX(BEC%Zone,'ICH') .GT. 0 .OR.
     >    INDEX(BEC%Zone,'IDF') .GT. 0 .OR.
     >    INDEX(BEC%Zone,'SBS') .GT. 0 .OR.
     >    INDEX(BEC%Zone,'SBPS') .GT. 0 ) THEN
        LV2ATV = .FALSE.
        RETURN
      ENDIF

C     IF LV2HDR IS .TRUE. BECSET HAS ALREADY BEEN CALLED, ALL PARAMETERS ASSIGNED AND OUTPUTS WRITTEN

      IF (LV2ATV .AND. LV2HDR) RETURN
      LV2HDR = .TRUE.

C     LTDG (LARGE TREE DIAMETER GROWTH) REPARAM IS INITIALLY NOT DEFINED
ccc      DO I = 1,MAXSP
ccc        LLTDGOK(I)  = .FALSE.
ccc      ENDDO

      IBEC = 0
      IREGN = 0      
C      READ (BEC%Series,'(I4)') iSeries

C     ** LARGE TREE ADJUSTMENT **
C     IBECX IS THE INDEX TO THE EXISTENCE OF AN ADJUSTMENT PARAMETER AT THE
C     SITE SERIES. IBEC IS THE HABITAT CODE MATCHING THE STRING CONTAINING
C     THE SITE SERIES. NO MATCH GIVES IBECX=0, IBEC=0

      IBECX = 0
      IF (INDEX(BEC%Region,'KAM') .GT. 0 .OR.
     >    INDEX(BEC%Region,'CAR') .GT. 0) THEN
        IREGN = 1
        DO I = 1,KCNT
          IF (INDEX(BEC%PrettyName,KAMCODE(I)) .GT. 0) THEN
            IBEC = KAMINDX(I)
            IBECX = I            
            GOTO 22
          ENDIF
        ENDDO
      ELSEIF (INDEX(BEC%Region,'NEL') .GT. 0) THEN
        IREGN = 2
        DO I = 1,NCNT
          IF (INDEX(BEC%PrettyName,NELCODE(I)) .GT. 0) THEN
            IBEC = NELINDX(I)
            IBECX = I
            GOTO 22
          ENDIF
        ENDDO
      ENDIF

C     ** LARGE TREE ADJUSTMENT **
C     IBECY IS THE INDEX TO THE ADJUSTMENT PARAMETER AT THE
C     SUBZONE. NO MATCH GIVES IBECY=0

   22 IBECY = 0
      IF (IREGN .EQ. 1) THEN
        DO I = 1, KCNT2
          IF (INDEX(BEC%FullName,KAMCOD2(I)) .GT. 0) THEN
            IBECY = I
            GOTO 23
          ENDIF
        ENDDO
      ELSEIF (IREGN .EQ. 2) THEN
        DO I = 1, NCNT2
          IF (INDEX(BEC%FullName,NELCOD2(I)) .GT. 0) THEN
            IBECY = I
            GOTO 23
          ENDIF
        ENDDO
      ENDIF

C     HEIGHT GROWTH, MORTALITY MODEL ADJUSTMENT
C
C     FIND INDEX TO ALL-REGIONS SUBZONE. NO MATCH OR NO
C     PARAMETERIZATION GIVES JBECY=0. NOTE THAT LTHGMOD IS USED
C     IN THE TEST, BUT THAT IT APPLIES TO MORTMOD AS WELL, SINCE
C     THEY ARE IDENTICAL W.R.T. TO BECszn AND OK ELEMENTS.

   23 JBECY = 0
      DO I = 1,ACNT2
        IF (INDEX(BEC%FullName,LTHGMOD(I)%BECszn) .GT. 0 .AND.
     >     LTHGMOD(I)%OK) THEN
          JBECY = I
          GOTO 24
        ENDIF
      ENDDO

C     WRITE HEADER INFO
      
   24 CALL VARVER(VVER)
      WRITE (JOSTND, "(/1X,T13,80('='))")   
      WRITE (JOSTND, "(/1X,T13,A7,
     &  /1X,T13,'VERSION 2 CALIBRATION OF LARGE-TREE DIAMETER-GROWTH ',
     &   'LARGE TREE HEIGHT-DIAMETER',
     &  /1X,T13,'SMALL TREE HEIGHT-GROWTH, MAXIMUM BASAL AREA, ',
     &   'HEIGHT GROWTH MODIFICATION',
     &  /1X,T13,'AND MORTALITY MODIFICATION MODELS')") VVER

C     IF IBEC IS ZERO, THE USER HAS PROVIDED A BEC/SS WHICH CURRENTLY 
C     HAS NO MATCH AND THE DEFAULT HABITAT CODE IS APPLIED. OTHERWISE,
C     COMPARE WITH EXISTING HABITAT CODE.

      IF (IBEC .EQ. 0) THEN
        KODTYP = 260 ! use NI default
        WRITE (JOSTND, 28) BEC%PrettyName, BEC%Region, KODTYP
   28   FORMAT (/1X,T13,'THE BEC VARIANT ',A,' HAS NO EXACT LARGE-',
     &    'TREE RECALIBRATION',/1X,T13,'MAPPING IN THE ',A,
     &    'REGION; USING DEFAULT HABITAT TYPE= ',I)
        CALL ERRGRO(.TRUE.,3)
      ELSE
        WRITE (JOSTND, 30) BEC%PrettyName, IBEC, BEC%Region
   30   FORMAT (/1X,T13,'THE BEC VARIANT ',A,' MAPS TO HABITAT ',
     &    'TYPE ',I,' IN THE ',A,' REGION.')
        KODTYP = IBEC
      ENDIF

C     LARGE TREE VERSION 2 INTERIM CALIBRATION:
C
C     READ FLAG INDICATING IF BEC VARIANT IS VALID FOR SPECIES
C     IN THE SUBZONE (BUT PERHAPS NOT IN THE SITE SERIES)
C     READ SITE SERIES TERMS AND ADD TO INTERCEPT

      IF (INDEX(BEC%Region,'KAM') .GT. 0 .OR.
     >      INDEX(BEC%Region,'CAR') .GT. 0) THEN
        DO I = 1,MAXSP
          LLTDGOK(I) = LKOK(IBECY,I)
          IF (IBEC.GT.0) BECTERM(I) = KCOEF(IBECX,I)
          SEICON(I) = INTRCP(I) + BECTERM(I)
        ENDDO
      ELSEIF (INDEX(BEC%Region,'NEL') .GT. 0) THEN
        DO I = 1,MAXSP      
          LLTDGOK(I) = LNOK(IBECY,I)
          IF (IBEC.GT.0) BECTERM(I) = NCOEF(IBECX,I)
          SEICON(I) = INTRCP(I) + BECTERM(I)          
        ENDDO
      ENDIF

C     SEE IF THERE ARE ANY SPECIES FOR WHICH A PARAMETERIZATION IS
C     NOT AVAILABLE.

      DO I=1,44
        SPLST(I:I) = ' '
      ENDDO
      INOSPP = 0
      IP = 1
      DO I=1,MAXSP
        IF (.NOT. LLTDGOK(I)) THEN 
          IF (INOSPP .GT. 0) THEN
            SPLST(IP:IP) = ','    ! INSERT COMMAS IF MORE THAN 1 SPP
            IP = IP+1
          ENDIF
          INOSPP = INOSPP + 1
          DO J=1,3
            IF (JSP(I)(J:J) .NE. ' ') THEN
              SPLST(IP:IP) = JSP(I)(J:J)
              IP = IP+1
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      IF (INOSPP .GT. 0) THEN
        WRITE(JOSTND, "(/1X,T13,'LARGE-TREE DIAMETER-GROWTH ',
     >      'RE-PARAMETERIZATION IS NOT AVAILABLE FOR'/1X,T13,
     >      'THE FOLLOWING SPECIES:',2(/1X),
     >      T13,A)") SPLST(1:(IP-1))
      ENDIF

C      IF THERE ARE SOME SPP W/ REPARAMETERIZATION COEFFICIENTS

      IF (INOSPP .LT. MAXSP) THEN
        WRITE (JOSTND, 31) 
   31     FORMAT (/1X,T13,'THE FOLLOWING LARGE-TREE FACTOR '
     &      'COEFFICIENT ADJUSTMENTS WILL BE APPLIED:',
     &      2(/1X),T13,'SPP',T17,'INTERCEPT',T27,'   BEC/SS',
     &     /1X,T13,3("-"),T17,9("-"),T27,9("-"))

        DO I = 1,MAXSP
          IF (LLTDGOK(I))
     >      WRITE (JOSTND, 32) JSP(I), INTRCP(I), BECTERM(I)
   32       FORMAT(T13,A3,T17,F9.6,T27,F9.6)
        ENDDO

        WRITE (JOSTND, 34)
   34     FORMAT (/1X,T13,'THE FOLLOWING LARGE-TREE CONTINUOUS '
     &      'COEFFICIENT ADJUSTMENTS WILL BE APPLIED:',
     &      2(/1X),T13,'SPP',
     &      T17,'    SLOPE',T27,'  SIN ASP',T37,'  COS ASP',
     &      T47,'     ELEV',T57,'  ELEV**2',T67,'      CCF'/1X,
     &      T13,3("-"), T17,9("-"),T27,9("-"),T37,9("-"),
     &      T47,9("-"),T57,9("-"),T67,9("-"))

        DO I = 1,MAXSP
          IF (LLTDGOK(I)) 
     &        WRITE (JOSTND, 35) JSP(I), SEISLP(I),SEISAS(I),SEICAS(I),
     &        SEIELV(I), SEIEL2(I), SEICCF(I)
   35         FORMAT(T13,A3,T17,F9.6,T27,F9.6,T37,F9.6,
     &        T47,F9.6,T57,F9.6,T67,F9.6)
        ENDDO
      ENDIF
C--------------------------------------------------------------------
C
C     VERSION 2 - REPARAMETERIZATION OF HEIGHT DUBBING MODEL
C
C     FIND AND REPLACE HEIGHT COEFFICIENTS HELD IN PLOT.F77
C     ARRAYS HT1() AND HT2() (C_0 AND C_1 OF HT/DIAM EQN)
C     BECAUSE THE ORIGINAL ARRAYS NEED TO KEPT UNALTERED FOR MULTI-
C     STAND RUNS, AN ARCHIVE (XHT1B,XHT2B) OF THE COMPILE TIME
C     VARIABLES IS MAINTAINED.
C     COPY PERMANENT ARRAYS TO ARCHIVE

      IF (LV2HTCP) THEN
        LV2HTCP = .FALSE.
        DO I = 1,MAXSP
          XHT1B(I) = HT1(I)
          XHT2B(I) = HT2(I)
        ENDDO
      ENDIF

C     COPY TEMPORARY VALUES FROM ARCHIVE

      DO I = 1,MAXSP
        XHT1(I) = XHT1B(I)
        XHT2(I) = XHT2B(I)
      ENDDO

C     SEARCH FOR MATCHES AND REPLACE IF FOUND

      DO I = 1,MAXSP

        IF (I.EQ.2) THEN   ! western larch - LW
          IF (INDEX(BEC%Zone,'ESSF') .GT. 0 .OR.
     &        INDEX(BEC%Zone,'MS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0) THEN
              XHT1(I) =   3.9536
              XHT2(I) = -21.4361
            ENDIF
          ENDIF

        ELSEIF (I.EQ.3 .OR. I.EQ.14) THEN   ! Douglas-fir - FD; OC
          IF (INDEX(BEC%Zone,'ESSF') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mk') .GT. 0) THEN
              XHT1(I) =   3.9432
              XHT2(I) = -25.9091
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0) THEN
              XHT1(I) =   3.9274
              XHT2(I) = -20.1322
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   4.1218
              XHT2(I) = -29.6135
            ELSEIF (INDEX(BEC%SubZone,'dw') .GT. 0) THEN
              XHT1(I) =   3.7859
              XHT2(I) = -22.0166
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'PP') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dh') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'xh') .GT. 0) THEN
              XHT1(I) =   3.5967
              XHT2(I) = -22.0246
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'MS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0) THEN
              XHT1(I) =   3.8840
              XHT2(I) = -25.0499
            ENDIF
          ENDIF

        ELSEIF (I.EQ.5) THEN   ! western hemlock - HW
          IF (INDEX(BEC%Zone,'ESSF') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'mm') .GT. 0) THEN          
              XHT1(I) =   3.2884
              XHT2(I) = -12.9475
            ELSEIF (INDEX(BEC%SubZone,'wv') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'ww') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'wc') .GT. 0) THEN
              XHT1(I) =   4.0577
              XHT2(I) = -28.1854
            ENDIF
          ENDIF

        ELSEIF (I.EQ.6) THEN   ! western redcedar - CW
          IF (INDEX(BEC%Zone,'ESSF') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'MS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'wc') .GT. 0) THEN
              XHT1(I) =   4.0095
              XHT2(I) = -32.5478
            ELSEIF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   3.8792
              XHT2(I) = -28.1120
            ENDIF
          ENDIF

        ELSEIF (I.EQ.7) THEN   ! lodgepole pine - PL
          IF (INDEX(BEC%Zone,'ESSF') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'MS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'dc') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'dm') .GT. 0) THEN
              XHT1(I) =   3.6574
              XHT2(I) = -14.5566
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0) THEN            
              XHT1(I) =   3.7443
              XHT2(I) = -18.5685
            ELSEIF (INDEX(BEC%SubZone,'xv') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mv') .GT. 0) THEN
              XHT1(I) =   3.2342
              XHT2(I) = -11.6208
            ENDIF
          ENDIF

        ELSEIF (I.EQ.8) THEN   ! Engelmann spruce - SE
          IF (INDEX(BEC%Zone,'ESSF')) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mw') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'wc') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mv') .GT. 0) THEN
              XHT1(I) =   3.9041
              XHT2(I) = -22.0422
            ELSEIF (INDEX(BEC%SubZone,'dc') .GT. 0) THEN
              XHT1(I) =   4.0093
              XHT2(I) = -26.5437
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'mc') .GT. 0) THEN
              XHT1(I) =   3.6971
              XHT2(I) = -22.9202
            ENDIF
          ELSEIF (INDEX(BEC%Zone,'MS')) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0) THEN
              XHT1(I) =   4.0555
              XHT2(I) = -25.5775
            ELSEIF (INDEX(BEC%SubZone,'dm') .GT. 0) THEN
              XHT1(I) =   4.1206
              XHT2(I) = -29.9800
            ENDIF
          ENDIF

        ELSEIF (I.EQ.9 .OR. I.EQ.4) THEN   ! subalpine fir - BL (& BG)
          IF (INDEX(BEC%Zone,'ESSF') .GT. 0 .OR.
     >        INDEX(BEC%Zone,'MS') .GT. 0) THEN
            IF (INDEX(BEC%SubZone,'dk') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'wc') .GT. 0 .OR.
     >          INDEX(BEC%SubZone,'mc') .GT. 0 .OR.     
     >          INDEX(BEC%SubZone,'mk') .GT. 0 .OR.          
     >          INDEX(BEC%SubZone,'mv') .GT. 0) THEN
              XHT1(I) =   3.7405
              XHT2(I) = -21.4266
            ELSEIF (INDEX(BEC%SubZone,'mw') .GT. 0 .OR.
     >              INDEX(BEC%SubZone,'dc') .GT. 0) THEN
              XHT1(I) =   4.1376
              XHT2(I) = -32.5929
            ELSEIF (INDEX(BEC%SubZone,'wk') .GT. 0) THEN
              XHT1(I) =   3.6544
              XHT2(I) = -22.5426
            ENDIF
          ENDIF

        ELSEIF (I.EQ.10) THEN   ! ponderosa pine - PY
          IF (INDEX(BEC%Zone,'PP')) THEN
            IF (INDEX(BEC%SubZone,'dh') .GT. 0) THEN
              XHT1(I) =   3.8878
              XHT2(I) = -34.7046
            ELSEIF (INDEX(BEC%SubZone,'xh') .GT. 0) THEN
              XHT1(I) =   3.6298
              XHT2(I) = -31.3677
            ENDIF
          ENDIF
        ENDIF
      ENDDO

C     MARK COEFFICIENTS THAT ARE DIFFERENT FROM THE
C     NI VARIANT DEFAULTS AND

      DO I = 1,MAXSP
        IF((XHT1(I) .NE. XHT1B(I)) .OR. (XHT2(I) .NE. XHT2B(I))) THEN
          CH(I) = '*'
        ELSE
          CH(I) = ' '
        ENDIF
      ENDDO

C     COPY TEMPORARY TO PERMANENT ARRAYS

      DO I = 1,MAXSP
        HT1(I) = XHT1(I)
        HT2(I) = XHT2(I)
      ENDDO
      
C     SET FLAG FOR WHETHER METRIC DUB IS USED, OR NOT

      DO I = 1,MAXSP
	  IF ((HT1(I) .NE. XHT1B(I)) .OR.
     >      (HT2(I) .NE. XHT2B(I))) THEN
	    LMHTDUB(I) = .TRUE.
	  ELSE
	    LMHTDUB(I) = .FALSE.
	  ENDIF
      ENDDO

C     PRINT HEIGHT COEFFICIENTS IF DESIRED (NON-BLANK 7TH FIELD OF
C     BECINFO KEYWORD)

       WRITE (JOSTND,41)
   41  FORMAT (/1X,T13,'THE FOLLOWING DIAMETER-HEIGHT MODEL '
     &  'COEFFICIENTS WILL BE USED FOR DUBBING',/1X,T13,
     &  'MISSING HEIGHTS. MARKED COEFFICIENTS FIT A '
     &  'CM/M MODEL; OTHERS AN IN/FT MODEL:',2(/1X),T13,'SPP',
     &  T17,'       C0',T27,'       C1',T37,'NEW'/1X,
     &  T13,3("-"),T17,9("-"),T27,9("-"),T37,3("-"))

      DO I = 1,MAXSP
        WRITE (JOSTND, 42) JSP(I), HT1(I), HT2(I), CH(I)
   42   FORMAT(T13,A3,T17,F9.4,T27,F9.4,T39,A1)
      ENDDO

C--------------------------------------------------------------------
C
C     REPARAMETERIZATION OF SMALL TREE HEIGHT GROWTH MODEL
C
C     IN A MERGED V2/V3 MODEL, ALL THE SMALL TREE HEIGHT GROWTH
C     MODIFICATIONS WERE FOR SPECIES IN THE IDF. THESE SPECIES
C     ARE ALL HANDLED UNDER V3

C--------------------------------------------------------------------
C
C     VERSION 2 - REPARAMETERIZATION OF HEIGHT GROWTH MODEL
C
      IF (JBECY .EQ. 0) THEN
        WRITE(JOSTND, "(/1X,T13,'LARGE-TREE HEIGHT-GROWTH ',
     >    'ADJUSTMENT IS NOT CURRENTLY AVAILABLE FOR'/1X,T13,
     >    'THIS BEC/SUBZONE.')")
	ELSE

C     SEE IF THERE ARE ANY SPECIES FOR WHICH A PARAMETERIZATION IS
C     NOT AVAILABLE. THESE ALWAYS HAVE LO=0 AND HI=5.

        DO I=1,44
          SPLST(I:I) = ' '
        ENDDO
        INOSPP = 0
        IP = 1
        DO I=1,MAXSP
          LOK2 = ((LTHGMOD(JBECY).LO(I) .EQ. 0) .AND.
     >            (LTHGMOD(JBECY).HI(I) .EQ. 5))
          IF (LOK2) THEN 
            IF (INOSPP .GT. 0) THEN
              SPLST(IP:IP) = ','    ! INSERT COMMAS IF MORE THAN 1 SPP
              IP = IP+1
            ENDIF
            INOSPP = INOSPP + 1
            DO J=1,3
              IF (JSP(I)(J:J) .NE. ' ') THEN
                SPLST(IP:IP) = JSP(I)(J:J)
                IP = IP+1
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        IF (INOSPP .GT. 0)
     >  WRITE(JOSTND, "(/1X,T13,'LARGE TREE HEIGHT GROWTH ',
     >    'ADJUSTMENT IS NOT AVAILABLE FOR'/1X,T13,
     >    'THE FOLLOWING SPECIES:',2(/1X),T13,A)")
     >     SPLST(1:(IP-1))

	  IF (INOSPP .LT. MAXSP) THEN
          WRITE (JOSTND, 61)
   61     FORMAT (/1X,T13,'THE FOLLOWING BREAKPOINTS '
     &      'WILL BE USED FOR ADJUSTING HEIGHT GROWTH',
     &      2(/1X),T13,'SPP',T17,'  LOW (M)',T27,' HIGH (M)',
     &      /1X,T13,3("-"),T17,9("-"),T27,9("-"))

          DO I = 1,MAXSP
            LOK2 = ((LTHGMOD(JBECY).LO(I) .EQ. 0) .AND.
     >             (LTHGMOD(JBECY).HI(I) .EQ. 5))
            IF (.NOT.LOK2) THEN 
              WRITE (JOSTND, 62) JSP(I),
     >          LTHGMOD(JBECY).Lo(I),LTHGMOD(JBECY).Hi(I)
   62         FORMAT(T13,A3,T17,I9,T27,I9)
            ENDIF
          ENDDO
        ENDIF
      ENDIF

C--------------------------------------------------------------------
C
C     VERSION 2 - REPARAMETERIZATION OF MORTALITY MODEL

      IF (JBECY .EQ. 0) THEN
        WRITE(JOSTND, "(/1X,T13,'LARGE-TREE HEIGHT-GROWTH ',
     >    'ADJUSTMENT IS NOT CURRENTLY AVAILABLE FOR'/1X,T13,
     >    'THIS BEC/SUBZONE.')")
	ELSE

C       SEE IF THERE ARE ANY SPECIES FOR WHICH A PARAMETERIZATION IS
C       NOT AVAILABLE. THESE ALWAYS HAVE LO=0 AND HI=5.

        DO I=1,44
          SPLST(I:I) = ' '
        ENDDO
        INOSPP = 0
        IP = 1
        DO I=1,MAXSP
          LOK2 = ((MORTMOD(JBECY).LO(I) .EQ. 0) .AND.
     >            (MORTMOD(JBECY).HI(I) .EQ. 5))
          IF (LOK2) THEN 
            IF (INOSPP .GT. 0) THEN
              SPLST(IP:IP) = ','    ! INSERT COMMAS IF MORE THAN 1 SPP
              IP = IP+1
            ENDIF
            INOSPP = INOSPP + 1
            DO J=1,3
              IF (JSP(I)(J:J) .NE. ' ') THEN
                SPLST(IP:IP) = JSP(I)(J:J)
                IP = IP+1
              ENDIF
            ENDDO
          ENDIF
        ENDDO

        IF (INOSPP .GT. 0)
     >  WRITE(JOSTND, "(/1X,T13,'MORTALITY ',
     >    'ADJUSTMENT IS NOT AVAILABLE FOR'/1X,T13,
     >    'THE FOLLOWING SPECIES:',2(/1X),T13,A)")
     >     SPLST(1:(IP-1))

	  IF (INOSPP .LT. MAXSP) THEN
          WRITE (JOSTND, 71)
   71     FORMAT (/1X,T13,'THE FOLLOWING BREAKPOINTS '
     &      'WILL BE USED FOR ADJUSTING MORTALITY',
     &      2(/1X),T13,'SPP',T17,' LOW (CM)',T27,'HIGH (CM)',
     &      /1X,T13,3("-"),T17,9("-"),T27,9("-"))

          DO I = 1,MAXSP
            LOK2 = ((MORTMOD(JBECY).LO(I) .EQ. 0) .AND.
     >              (MORTMOD(JBECY).HI(I) .EQ. 5))
            IF (.NOT.LOK2) THEN 
              WRITE (JOSTND, 72) JSP(I),
     >          MORTMOD(JBECY).Lo(I),MORTMOD(JBECY).Hi(I)
   72         FORMAT(T13,A3,T17,I9,T27,I9)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
 
      WRITE (JOSTND, "(/1X,T13,80('='))")
           
      RETURN

C--------------------------------------------------------------------
C
C     COPY BEC TERMS REQUIRED TO ADJUST LARGE TREE DIAMETER
C     GROWTH MODEL (CALLED BY SUBROUTINE **DGF**). THE XBEC0 TERMS
C     ARE TRANSFORMED TO THE INTERNAL UNITS. IF THE SS IS NOT
C     SUPPORTED, SET LBEC8 TO .FALSE.

      ENTRY SEILTDG (XBEC0,XBEC1,XBEC2,XBEC3,XBEC4,XBEC5,XBEC6,
     >               XBEC7,LBEC8)

      DO I=1,MAXSP
        LBEC8(I) = .FALSE.
      ENDDO

      DO I=1,3
        XBEC0(I) = BECADJ(I)
      ENDDO

      XBEC0(1) = XBEC0(1) / 100.
      XBEC0(2) = XBEC0(2) * 3.14159 / 180.
      XBEC0(3) = XBEC0(3) * MtoFT / 100.
        
      DO I=1,MAXSP
        XBEC1(I) = SEICON(I)
        XBEC2(I) = SEISLP(I)
        XBEC3(I) = SEISAS(I)
        XBEC4(I) = SEICAS(I)
        XBEC5(I) = SEIELV(I)
        XBEC6(I) = SEIEL2(I)
        XBEC7(I) = SEICCF(I)
        LBEC8(I) = LLTDGOK(I)
      ENDDO

      RETURN

C--------------------------------------------------------------------
C     COPY TERMS REQUIRED TO ADJUST SMALL TREE HEIGHT GROWTH MODEL
C     (**REGENT**). V2 SMALL TREE ADJUSTMENT HAS BEEN SUPERCEDED BY
C     V3, WHICH MODELS ALL SPECIES IN THE IDF (THE ZONE THAT THE 
C     V2 ADJUSTMENT WAS APPLIED TO).

      ENTRY SEISTHG (ISP1,X01,X11,X21,X31,X41,X51,X61,LOK1)
      LOK1 = .FALSE.
      RETURN

C--------------------------------------------------------------------
C
C     COPY TERMS REQUIRED TO ADJUST LARGE TREE HEIGHT
C     GROWTH MODEL (CALLED BY SUBROUTINE **UPDATE**). IF THE
C     SS IS NOT SUPPORTED FOR A SUBZONE, RETURN 1.0 AS THE
C     MODIFIER; IF THE SPECIES IS NOT SUPPORTED, USE 0,5 M
C     BREAKPOINTS TO SUPRESS GROWTH. THE BREAKPOINTS FOR
C     THE YLTHG(4) VECTOR AND THE XLTHG(1,4) ARE CONSTANT
C     AND ARE DECLARED PREVIOUSLY. THE RETURNED VALUE, X0
C     IS MULTIPLIED BY THE PREDICTED HEIGHT GROWTH TO GIVE
C     AN ADJUSTED HEIGHT GROWTH.

      ENTRY SEILTHG (ISP2,XHT22,X02)

      IF (JBECY .EQ. 0 .OR. ISP2 .LT. 1 .OR. ISP2 .GT. MAXSP) THEN
        X02 = 1.
        RETURN
      ENDIF

	XLTHG(2) = LTHGMOD(JBECY).Lo(ISP2)
      XLTHG(3) = LTHGMOD(JBECY).Hi(ISP2)

	X02 = ALGSLP((XHT22*FTtoM),XLTHG,YLTHG,4)

      RETURN

C--------------------------------------------------------------------
C
C     COPY TERMS REQUIRED TO ADJUST MORTALITY MODEL
C     CALLED BY SUBROUTINE *DGDRIV*). IF THE
C     SS IS NOT SUPPORTED FOR A SUBZONE, RETURN 1.0 AS THE
C     MODIFIER; IF THE SPECIES IS NOT SUPPORTED, USE 0,5 M
C     BREAKPOINTS TO SUPRESS GROWTH. THE BREAKPOINTS FOR
C     THE YMORT(4) VECTOR AND THE XMORT(1,4) ARE CONSTANT
C     AND ARE DECLARED PREVIOUSLY. THE RETURNED VALUE, X0,
C     IS COMPARED TO THE MORTALITY PREDICTION, AND THE 
C     GREATER OF THE TWO IS USED.

      ENTRY SEIMORT (ISP3,XDIA3,X03)

      X03 = 1.0
      IF (JBECY .EQ. 0 .OR. ISP3 .LT. 1 .OR. ISP3 .GT. MAXSP) RETURN

	XMORT(2) = MORTMOD(JBECY).Lo(ISP3)
      XMORT(3) = MORTMOD(JBECY).Hi(ISP3)

	X03 = ALGSLP((XDIA3*INtoCM),XMORT,YMORT,4)

C     APPLY MORTALITY-EFFETS THROUGH MULTIPLICATIVE
C     CHANGE ON DIAMETER GROWTH: THEREFORE A ZERO
C     MORTALITY BECOMES A 1 FOR DG MULTIPLIER; A 1 FOR 
C     MORTALITY BECOMES A 0 FOR DG MULTIPLIER.
C     THE VALUE IS KEPT FROM BEING ZERO, SINCE LOG(DG) IS 
C     USED LATER

      X03 = MAX(0.0001, (1.0 - X03))

      RETURN
      END
