      SUBROUTINE GRINIT
      IMPLICIT NONE
C----------
C WS $Id$
C----------
C
C  INITIALIZE PROGNOSIS MODEL VARIABLES
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
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OPCOM.F77'
C
C
      INCLUDE 'WORKCM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
      INCLUDE 'ECON.F77'
C
C
      INCLUDE 'MULTCM.F77'
C
C
      INCLUDE 'SUMTAB.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'CWDCOM.F77'
C
C
      INCLUDE 'CALCOM.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
COMMONS
C----------
      CHARACTER*26 DBLK
      CHARACTER*4 NONE
      CHARACTER*2 ANINDEX
      INTEGER I,J,K
C----------
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
C    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
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
      DATA DBLK/'                          '/
      DATA NONE/'NONE'/
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
      STMP(I) = 1.0
      TOPD(I) = 4.5
      DBHMIN(I) = 7.0
      FRMCLS(I) = 80.0
      METHB(I) = 6
      METHC(I) = 6
      BFSTMP(I) = 1.0
      BFTOPD(I) = 6.0
      BFMIND(I) = 10.0
      BFLA0(I) = 0.0
      BFLA1(I) = 1.0
      CFLA0(I) = 0.0
      CFLA1(I) = 1.0
      LDGCAL(I) = .TRUE.
      LHTCAL(I) = .TRUE.
C
      SELECT CASE (I)
      CASE(9:10,12,14:17,19:20,25:27,41)
        LHTDRG(I) = .FALSE.
      CASE DEFAULT
        LHTDRG(I) = .TRUE.
      END SELECT
C
      BARANK(I) = 0.0
      MAXSDI(I) = 0
      SITEAR(I) = 0.0
      IABFLG(I) = 1
      ISTAGF(I) = 0
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
      JSPIN(I)=3
      LEAVESP(I)=.FALSE.
    5 CONTINUE
      LFLAGV = .FALSE.
      LBAMAX = .FALSE.
      LZEIDE = .FALSE.
      DSTAG = 1.0
      BAINIT = 0.0
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
      RMAI = 0.0
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
      ELEV = 45.
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
C----------
      ICL4 = 2
      ICL5 = 999
      ICL6 = 0
      IDG = 0
      IFINT = 10
      IFINTH = 5
      IFOR = 6
      KODFOR = 0
      IFST = 1
      IGL = 1
      IHTG = 0
      IPTINV = -9999
      ITYPE = 0
      KODTYP = 0
      CPVREF='          '
      PCOMX='        '
      IY(1) = 0
      IY(MAXCY1) = -1
      LDCOR2 = .FALSE.
      LDUBDG = .FALSE.
      LEVUSE = .TRUE.
      LFIXSD = .FALSE.
      LHCOR2 = .FALSE.
      LRCOR2 = .FALSE.
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
      TLAT = 39.
      TLONG = 120.
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
      DBHZEIDE=0.
      DBHSTAGE=0.
      DR016=0.
      DBHSDI=0.
      JSPINDEF=0
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
C----------
C     INITIALIZE VARIABLES CARRIED IN GGCOM.
C----------
      DO I=1,MAXSP
      TBA(I) = 0.0
      DO J=1,41
      IF(I .EQ. 1) BAU(J) = 0.0
      BCLAS(I,J) = 0.0
      TCLAS(I,J) = 0.0
      ENDDO
      ENDDO
      AGERNG=0.
      SEEDS=0.
      BARK1=0.
      BARK2=0.
      TPAT=0.
      DSTAG=0.
C
      RETURN
      END
