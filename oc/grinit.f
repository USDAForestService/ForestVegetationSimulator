      SUBROUTINE GRINIT
      IMPLICIT NONE
C----------
C OC $Id: grinit.f 1388 2014-12-19 16:34:36Z rhavis@msn.com $
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
      INCLUDE 'ORGANON.F77'
C
C
COMMONS
C----------
      INTEGER I,J,K
      CHARACTER*26 DBLK
      CHARACTER*4 NONE
      CHARACTER*2 ANINDEX
      DATA DBLK/'                          '/
      DATA NONE/'NONE'/
C----------
      CALL LNKINT
      DO 5 I=1,MAXSP
      SDIDEF(I) = 0.0
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
      TOPD(I) = 0.
      DBHMIN(I) = 7.0
      FRMCLS(I) = 0.0
      METHB(I) = 999
      METHC(I) = 999
      BFSTMP(I) = 1.0
      BFTOPD(I) = 0.
      BFMIND(I) = 7.0
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
      JSPIN(I)=3
      LEAVESP(I)=.FALSE.
   5  CONTINUE
      LFLAGV = .FALSE.
      LBAMAX = .FALSE.
      LZEIDE = .FALSE.
      DBHMIN(11) = 6.0
      BFMIND(11) = 6.0
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
      IY(I) = 5
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
      DGSD = 0
      EFF = 1.0
      ELEV = 35.
      FINT = 5.
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
      ICL4 = 0
      ICL5 = 999
      ICL6 = 0
      IDG = 0
      IFINT = 5
      IFINTH = 5
      IFOR = 9
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
      TLAT = 42.
      TLONG = 124.
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
      IMODTY = 1
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
      CCCOEF=1.0 
      CCCOEF2=1.0
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
C  INITIALIZE VARCOM COMMON VARIABLES
C----------
      DO I=1,MAXSP
        ISTAGF(I)=0
      ENDDO
C----------
C     INITIALIZE ORGANON VARIABLES
C----------
      LORGANON  = .TRUE.        ! THE GROWTH AND MORTALITY DLL IS ACTIVE
      LORGVOLS  = .FALSE.       ! THE VOLUME DLL IS NOT BEING USED.
      LORGPREP  = .FALSE.       ! TREE DATA HAS NOT GONE THROUGH THE PREPARATION PROCESS
      VERSION   = 1             ! ORGANON VERSION, DEFAULT SWO
      STAGE     = 0             ! total stand age (even aged only)
      BHAGE     = 0             ! breast height age            
      SITE_1    = 0.0           ! the default site index dependant on variant
      SITE_2    = 0.0           ! the default site index dependant on variant
      MSDI_1    = 0.0
      MSDI_2    = 0.0
      MSDI_2    = 0.0
C
C INITIALIZE ORGANON CONTROL VARIABLE DEFAULTS
C
      INDS(1) = 1     ! HEIGHT CALIBRATION ON
      INDS(2) = 1     ! HEIGHT-TO-CROWN BASE CALIBRATION ON
      INDS(3) = 1     ! DIAMETER GROWTH CALIBRATION ON
      INDS(4) = 1     ! STAND IS EVEN-AGED      
      INDS(5) = 0     ! ORGANON TRIPPLING OFF
      INDS(6) = 0     ! STAND HAS NOT BEEN PRUNED
      INDS(7) = 0     ! STAND HAS NOT BEEN THINNED
      INDS(8) = 0     ! STAND HAS NOT BEEN FERTILIZED      
      INDS(9) = 1     ! USE SDI-BASED MORTALITY      
      INDS(10)= 0     ! WOOD QUALITY VARIABLES ARE NOT BEING COMPUTED.
      INDS(11)= 0     ! OVERSTORY TREES WERE NOT REMOVED AT START OF CURRENT CYCLE
      INDS(12)= 0     ! INGROWTH WAS NOT ADDED AT START OF GROWTH CYCLE
      INDS(13)= 0     ! MAJOR CONIFER TREES WERE NOT CUT AT BEGINNING OF CYCLE
      INDS(14)= 0     ! NOT PLANTED WITH GENETIC IMPROVED STOCK
      INDS(15)= 0     ! STAND IS NOT INFECTED WITH SWISS NEEDLE CAST
      DO I=16,30
        INDS(I) = 0
      ENDDO
C
C INITIALIZE REAL VARIABLE ARRAYS, THESE GET ADJUSTED IN SITSET
C
      DO I=1,30
        RVARS(I) = 0.0
      ENDDO
C
C     FOR NOW, ASSIGN THE DBH, HEIGHT, AND CR CALIBRATIONS TO UNITY.
C     TODO: THESE GET UPDATED FROM FVS.
C
      DO I=1,18
         ACALIB(1,I)  = 1.0 
         ACALIB(2,I)  = 1.0
         ACALIB(3,I)  = 1.0
      ENDDO
C      
C     CLEAR OUT THE TREE VARIABLES
C     THIS SHOULD ALSO BE PERFORMED EACH CYCLE AS THE TREE
C     RECORDS ARE RE-INITIALIZED FROM THE NATIVE FVS ARRAYS
C
      DO I=1,2000
         IORG(I) = 0
         TREENO(I)   = 0
         PTNO(I)     = 0
         SPECIES(I)  = 0
         DBH1(I)     = 0.0 
         HT1OR(I)    = 0.0
         CR1(I)      = 0.0
         SCR1B(I)    = 0.0
         EXPAN1(I)   = 0.0
         MGEXP(I)    = 0.0
         USER(I)     = 0
         TWARNING(I) = 0
      ENDDO
C
C     INITIALIZE THE VOLUME VARIABLES NOT NATIVE TO FVS 
C     THESE ONLY APPLY TO THE ORGANON SUBROUTINE VOLCAL_
C     THE BF DEFAULTS CAN BE CHANGED USING THE ORGVOLS KEYWORD 
C
      CFTD=0.0               ! SOFTWOODS CF TOP DIAMETER, IN INCHES.
      CFSH=0.0               ! SOFTWOODS CF STUMP HEIGHT, IN FEET.
      CFTDHW=0.0             ! HARDWOODS CF TOP DIAMETER, IN INCHES.
      LOGTD=6.0              ! BF LOG TOP DIAMETER, IN INCHES.
      LOGTA=8.0              ! BF LOG TRIM, IN INCHES.
      LOGSH=0.5              ! BF STUMP HEIGHT, IN FEET.
      LOGLL=32.0             ! BF TARGET LOG LENGTH, IN FEET.
      LOGML=8.0              ! BF MINIMUM LOG LENGTH, IN FEET.
C
C     INITIALIZE THE STOR VARIABLES TO ZERO TO ENSURE 
C     THE VALUES ARE NOT CORRUPT FOR EVERY STAND
C
      DO I=1,30
         STOR(I) = 0.0
      ENDDO
C
C     NULL OUT THE THINNING VARIABLES.
C     MOVE THE VARIABLES FROM THE PREVIOUS THINNINGS, IF THERE ARE ANY
C
      DO I=1,5
        YST(I)  = 0.0
        BART(I) = 0.0
      END DO
      BABT    = 0.0
C      
C     NULL OUT THE FERTILIZATION VARIABLES TOO.
C
      DO I=1,5
        YSF(I) = 0.0
        PN(I)  = 0.0
      END DO
C
C     SET THE VALUES NEEDED BY GROUP 3 EVENT MONITOR VARIABLES.
C
      OCC = 0.
      OAHT = 0.
C----------
C  END OF ORGANON SECTION
C----------
C
      RETURN
      END
