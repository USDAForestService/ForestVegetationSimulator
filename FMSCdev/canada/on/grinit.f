      SUBROUTINE GRINIT
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C
C  INITIALIZE PROGNOSIS MODEL VARIABLES
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'OPCOM.F77'
      INCLUDE 'WORKCM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'HTCAL.F77'
      INCLUDE 'ECON.F77'
      INCLUDE 'MULTCM.F77'
      INCLUDE 'SUMTAB.F77'
      INCLUDE 'VOLSTD.F77'
      INCLUDE 'VARCOM.F77'
      INCLUDE 'CWDCOM.F77'
      INCLUDE 'CALCOM.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C----------
      INTEGER I,J,K
      CHARACTER*26 DBLK
      CHARACTER*4 NONE
      CHARACTER*2 ANINDEX
      REAL ONMTD(MAXSP)
      DATA DBLK/'                          '/
      DATA NONE/'NONE'/
C ====================================================
C SPECIES LIST FOR ONTARIO - MINIMUM DIAM (CM)
C SET TO ZERO CM FOR ALL SPECIES. VALUES BELOW
C ARE OLD.
C MINIMUM TOP DIAMETER IS NOMINALLY 10 CM
C ====================================================
C     1 = JACK PINE (PJ) - 10 cm
C     2 = SCOTCH PINE (PS)- 10 cm
C     3 = RED PINE NATURAL (RN) -16 cm
C     4 = RED PINE PLANTATION (RP)- 16 cm
C     5 = WHITE PINE (PW)- 16 cm
C     6 = WHITE SPRUCE (SW) - 10 cm
C     7 = NORWAY SPRUCE (SN) - 10 cm
C     8 = BALSAM FIR (BF)- 10 cm
C     9 = BLACK SPRUCE (SB)- 10 cm
C    10 = TAMARACK (TA)- 10 cm
C    11 = N. WHITE CEDAR (CE)- 10 cm
C    12 = EASTERN HEMLOCK (HE)- 16 cm
C    13 = OTHER SOFTWOODS (SO) -10 cm
C    14 = E. REDCEDAR (CR)- 10 cm
C    15 = BLACK ASH (AB) - 20 cm
C    16 = GREEN ASH (AR)- 20 cm
C    17 = COTTONWOOD (CW)- 20 cm
C    18 = SILVER MAPLE (MV)- 20 cm
C    19 = RED MAPLE (MR)- 20 cm
C    20 = BLACK CHERRY (CB)- 20 cm
C    21 = AM. ELM (EW)- 20 cm
C    22 = SLIPPERY ELM (ES)- 20 cm
C    23 = ROCK ELM (ER)- 20 cm
C    24 = YELLOW BIRCH (BY)- 20 cm
C    25 = BASSWOOD (BD)- 20 cm
C    26 = SUGAR MAPLE (MH)- 20 cm
C    27 = BLACK MAPLE (MB)- 20 cm
C    28 = AM. BEECH (BE)- 20 cm
C    29 = WHITE ASH (AW)- 20 cm
C    30 = WHITE OAK (OW)- 20 cm
C    31 = SWAMP WHITE OAK (OP)- 20 cm
C    32 = BURR OAK (OB)- 20 cm
C    33 = CHINKAPIN OAK (OC)- 20 cm
C    34 = RED OAK (OR)- 20 cm
C    35 = BLACK OAK (BO)- 20 cm
C    36 = PIN OAK (PN)- 20 cm
C    37 = BITTERNUT HICKORY (HB)- 20 cm
C    38 = PIGNUT HICKORY (HP)- 20 cm
C    39 = SHAGBARK HICKORY (HU)- 20 cm
C    40 = BIGTOOTH ASPEN (PG) -16 cm
C    41 = QUAKING ASPEN (PT) - 16 cm
C    42 = BALSAM POPLAR (PB) -16 cm
C    43 = PAPER BIRCH (BW) - 16 cm
C    44 = COMMERCIAL HARDWOODS (CH)- 20 cm
C    45 = BUTTERNUT (BT)- 20 cm
C    46 = BLACK WALNUT (WB)- 20 cm
C    47 = E HOPHORNBEAM (IW)- 20 cm
C    48 = BLACK LOCUST (LB)- 20 cm
C    49 = NON-COMMERCIAL (NC)- 20 cm
C    50 = BOXELDER (MH)- 20 cm
C    51 = STRIPED MAPLE (MS)- 20 cm
C    52 = MTN. MAPLE (MT)- 20 cm
C    53 = AM. HORNBEAM (BB)- 20 cm
C    54 = AM. CHESTNUT (CA) - 20 cm
C    55 = HACKBERRY (BH)- 20 cm
C    56 = FLOWERING DOGWOOD (DF)- 20 cm
C    57 = HAWTHORN (HT)- 20 cm
C    58 = APPLE SP. (ML)- 20 cm
C    59 = BLACKGUM (GB)- 20 cm
C    60 = SYCAMORE (SY)- 20 cm
C    61 = PIN CHERRY (CP)- 20 cm
C    62 = CHOKECHERRY (CC)- 20 cm
C    63 = WHITE PLUM (PL)- 20 cm
C    64 = WILLOW (WI)- 20 cm
C    65 = BLACK WILLOW (WI)- 20 cm
C    66 = DIAMOND WILLOW (WI)- 20 cm
C    67 = SASSAFRAS (SS)- 20 cm
C    68 = MTN. ASH (AM)- 20 cm
C    69 = JACK PINE PLANTATION (JP) - 10 CM
C    70 = WHITE PINE PLANTATION (WP) - 16 CM
C    71 = WHITE SPRUCE PLANTATION (SP) - 10 CM
C    72 = BLACK SPRUCE PLANTATION (BP) - 10 CM
C
C
C   DATA ONMTD /
C     >  10., 10., 16., 16., 16., 10., 10., 10., 10., 10.,
C     >  10., 16., 10., 10., 20., 20., 20., 20., 20., 20.,
C     >  20., 20., 20., 20., 20., 20., 20., 20., 20., 20.,
C     >  20., 20., 20., 20., 20., 20., 20., 20., 20., 16.,
C     >  16., 16., 16., 20., 20., 20., 20., 20., 20., 20.,
C     >  20., 20., 20., 20., 20., 20., 20., 20., 20., 20.,
C     >  20., 20., 20., 20., 20., 20., 20., 20., 10., 16.,
C     >  10., 10. /
      DATA ONMTD / MAXSP * 0.0 /
C
C----------

      CALL LNKINT
      DO 5 I=1,MAXSP
      SDIDEF(I) = 0.
      IORDER(I) = 0
      COR(I) = 0.0
      XDMULT(I) = 1.
      XHMULT(I) = 1.
      XRDMLT(I) = 1.
      XRHMLT(I) = 1.
      XMMULT(I) = 1.
      XMDIA1(I) = 0.
      XMDIA2(I) = 99999.
      VEQNNB(I) = '          '
      VEQNNC(I) = '          '
      STMP(I)   = 30.0     * CMtoFT
      DBHMIN(I) = ONMTD(I) * CMtoIN
      TOPD(I)   = 10.0     * CMtoIN
      FRMCLS(I) = 80.0
      METHB(I) = 8
      METHC(I) = 8
      BFSTMP(I) = 30.0     * CMtoFT
      BFTOPD(I) = 10.0     * CMtoIN
      BFMIND(I) = ONMTD(I) * CMtoIN
      BFLA0(I) = 0.0
      BFLA1(I) = 1.0
      CFLA0(I) = 0.0
      CFLA1(I) = 1.0
      LDGCAL(I) = .TRUE.
      LHTCAL(I) = .TRUE.
      LHTDRG(I) = .FALSE.
      BARANK(I) = 0.0
      MAXSDI(I) = 0
      SITEAR(I) = 0.0
      IABFLG(I) = 1
      LSPCWE(I) = .FALSE.
      CWDS0(I) = 0.
      CWDS1(I) = 0.
      CWDS2(I) = 0.
      CWDS3(I) = 2.
      CWDL0(I) = 0.
      CWDL1(I) = 0.
      CWDL2(I) = 0.
      CWDL3(I) = 2.
      CWTDBH(I) = 0.
      SIZCAP(I,1) = 999.
      SIZCAP(I,2) = 1.
      SIZCAP(I,3) = 0.
      SIZCAP(I,4) = 999.
      JSPIN(I)=1
      LEAVESP(I)=.FALSE.
    5 CONTINUE
      LFLAGV = .FALSE.
      LBAMAX = .FALSE.
      LZEIDE = .FALSE.
      CFMIN = 0.
      TCFMIN = 0.
      BFMIN = 0.
      BAMIN = 0.
      TCWT = 0.
      SPCLWT = 0.
      PBAWT = 0.
      PCCFWT = 0.
      PTPAWT = 0.
      IREC1 = 0
      RMAI = 50.0
      IREC2 = MAXTP1
      ITHNPI = 1
      ITHNPN = -1
      ITHNPA = 0
C----------
      DO 10 I=1,MAXCYC
      IY(I) = -1
   10 CONTINUE
      DO 11 K=1,7
      ITABLE(K) = 0
   11 CONTINUE
C----------
      MANAGD = 0
      ALPHA = 0.05
      BJPHI = 0.74
      BJTHET = 0.42
      ASPECT = 0.
      LAUTON = .FALSE.
      LFIA = .FALSE.
      AUTMAX = 60.0
      AUTMIN = 45.0
      BAF = 40.
      BAMAX = 0.0
      BRK = 5.0
      DGSD = 2.0
      EFF = 1.0
      ELEV =  0.
      FINT = 10.
      FINTH = 5.
      FINTM = 5.0
      FPA = 300.
      IAGE = 0
      IWORK1(1) = 0
C----------
C  ICL4 IS USED TO IMPLEMENT A NEW OPTION WHICH ALLOWS THE USER TO
C  CONTROL THE NUMBER OF CYCLES A STAND WILL BE TRIPLED.
C  ICL5 IS USED TO CARRY THE ACTUAL INPUT HABITAT CODE.
C     IFOR CHANGED TO DEFAULT TO ONTARIO FOREST CODE
C----------
      ICL4 = 2
      ICL5 = 999
      ICL6 = 0
      IDG = 0
      IFINT = 10
      IFINTH = 5
      IFOR = 9
      KODFOR = 0
      IFST = 1
      IGL = 1
      IHTG = 0
      IPTINV = -9999
      ITYPE = 0
      KODTYP = 0
      PCOMX='        '
      CPVREF='          '
      IY(1) = 0
      IY(MAXCY1) = -1
      LDCOR2 = .FALSE.
      LDUBDG = .FALSE.
      LEVUSE = .TRUE.
      LFIXSD = .FALSE.
      LHCOR2 = .FALSE.
      LRCOR2 =.FALSE.
      LSUMRY = .FALSE.
      MGMID = NONE
      MORDAT = .FALSE.
      LSTATS = .FALSE.
      LMORT = .FALSE.
      LBVOLS = .FALSE.
      LCVOLS = .FALSE.
      LFIRE = .FALSE.
      LSITE = .FALSE.
      JOCALB = 0
      GROSPC = -1.0
      NCYC = 0
      NONSTK = -9999
      NOTRIP = .FALSE.
      NPLT = DBLK
      DBCN = ' '
      SAMWT = -1E25
      SLOPE = 5.0
      TLAT =  0.
      TLONG = 0.
      ISTATE = 0
      ICNTY = 0
      TFPA = 0.0
      TRM = 1.
      LBKDEN = .FALSE.
      RELDM1 = 0.
      OLDBA = 0.
      ATAVH = 0.
      ATBA = 0.
      ATCCF = 0.
      ORMSQD = 0.
      SDIMAX = 0.0
      SDIBC = 0.
      SDIAC = 0.
      ISISP = 0
      PMSDIL = 55.
      PMSDIU = 85.
      SLPMRT = 0.0
      CEPMRT = 0.0
      IBASP = 0
      IMODTY = 0
      IPHREG = 0
      IFORTP = 0
      FNMIN = 5.
      NCALHT = 5
      ISILFT = 0
      QMDMSB=999.
      SLPMSB=0.
      CEPMSB=0.
      EFFMSB=0.90
      DLOMSB=0.
      DHIMSB=999.
      MFLMSB=1
      DBHSDI=0.
C
      DO 30 J=1,9
      DO 20 K=1,MAXSP
      BFDEFT(J,K) = 0.0
      CFDEFT(J,K) = 0.0
   20 CONTINUE
   30 CONTINUE
      DO 40 I=1,21
      DO 35 J=1,3
      LOGDIA(I,J) = 0.
   35 CONTINUE
   40 CONTINUE
      DO 50 I=1,7
      DO 45 J=1,20
      LOGVOL(I,J) = 0.
   45 CONTINUE
   50 CONTINUE
C----------
C  INITIALIZE ECONOMIC VARIABLES.
C----------
      LECBUG = .FALSE.
      LECON  = .FALSE.
C----------
C     INITIALIZE SUBROUTINE SPECIFIC DEBUG.
C----------
      CALL DBINIT
C----------
C  INITIALIZE SUMTAB COMMON VARIABLES
C----------
      MAIFLG = 0
      NEWSTD = 0
      TOTREM = 0.
      AGELST = 0.
      DO 60 I=1,MAXCY1
      BCYMAI(I) = 0.
   60 CONTINUE
C----------
C     INITIALIZE SPECIES AND POINT GROUPS VARIABLES.
C----------
      NPTGRP = 0
      NSPGRP = 0
      DO 70 I=1,30
      DO 65 J=1,52
      ISPGRP(I,J)=0
      IPTGRP(I,J)=0
   65 CONTINUE
   70 CONTINUE
      DO I=1,30
      WRITE(ANINDEX,'(I2)') I
      NAMGRP(I) ='GROUP'//TRIM(ADJUSTL(ANINDEX))
      PTGNAME(I) ='PTGROUP'//TRIM(ADJUSTL(ANINDEX))
      ENDDO
C----------
C     INITIALIZE SITE TREE ARRAY AND COUNT
C----------
      NSITET = 0
      DO 75 I=1,MAXSTR
      DO 74 J=1,6
      SITETR(I,J)=0.
   74 CONTINUE
   75 CONTINUE
C
      RETURN
      END
