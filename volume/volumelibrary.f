      SUBROUTINE VOLUMELIBRARY(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,
     +    DBHOB,
     &    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     &    BA,SI,CTYPE,ERRFLAG,IDIST)
     
! 04-19-2016     Added IDIST as input variable
! 09/21/2016  Added biomass calculation

! Expose subroutine VOLUMELIBRARY to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLUMELIBRARY
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLUMELIBRARY
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIBRARY'::VOLUMELIBRARY
      
      USE CHARMOD 
	USE DEBUG_MOD
		  
      IMPLICIT NONE
      
!     Parameters
      INTEGER         REGN
      CHARACTER*(*)   FORST, VOLEQ
      REAL            MTOPP, MTOPS, STUMP,DBHOB, DRCOB
      CHARACTER*(*)   HTTYPE
      REAL            HTTOT
      INTEGER         HTLOG
      REAL            HT1PRD, HT2PRD, UPSHT1, UPSHT2, UPSD1, UPSD2
      INTEGER         HTREF
      REAL            AVGZ1, AVGZ2
      INTEGER         FCLASS
      REAL            DBTBH, BTR
      INTEGER         I3, I7, I15, I20, I21
      REAL            LOGVOL(I7,I20), LOGDIA(I21,I3), LOGLEN(I20)
      REAL            BOLHT(I21)
      INTEGER         TLOGS
      REAL            NOLOGP,NOLOGS
      INTEGER         CUTFLG, BFPFLG, CUPFLG, CDPFLG, CUSFLG, CDSFLG
      CHARACTER*(*)   PROD
      CHARACTER*(*)   CONSPEC
      INTEGER         HTTFLL
      CHARACTER*(*)   LIVE, CTYPE
      INTEGER         ERRFLG
      CHARACTER*2     DIST, VAR
      INTEGER         IDIST
!     Local variables      
!     Variable required for call to VOLINIT      
      INTEGER         SPFLG
      REAL            VOL(15)
      INTEGER         BA, SI
      INTEGER         ERRFLAG
!     Variable for biomass      
      REAL    WF(3), BMS(8)
      INTEGER SPCD, BMSFLG
      CHARACTER*10 EQNUM
        
! 	    print *, '--> enter volume library'
!	    print *, '    regn = ',regn, 'forst = ', forst
!	    print *, '    dist = ', dist
!	    print *, '*****************************'
!	    print *, '   prod = ', prod, 'voleq = ', voleq 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 !      vol(2) = 17.3
 !      logvol(4,1) = 32.3
c      IF(BMSFLG.EQ.1.AND.VOLEQ.EQ."")THEN
c        VAR = '  '
c        CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPCD,PROD,EQNUM,ERRFLAG)
c        VOLEQ = EQNUM
c      ENDIF
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
 !      print *, 'vol(2) = ', vol(2)
 !      print *, 'logvol(4,1) = ', logvol(4,1)
 !    Added the following to calculat biomass (09/20/2016)
c      IF (BMSFLG.EQ.1) THEN
c        CALL CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB, HTTOT,FCLASS,
c     +  VOL,WF,BMS,ERRFLG)
c      ENDIF
 4000 RETURN
      
      END SUBROUTINE VOLUMELIBRARY
C ---------------------------------------------------------------------
      SUBROUTINE FIAVOL(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
     & STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,
     & FIAVTYPE,FIAEQVOL,BFMIND)
! Expose subroutine VOLUMELIBRARY to users of this DLL
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::FIAVOL
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: FIAVOL
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'FIAVOL'::FIAVOL
      IMPLICIT NONE
      CHARACTER*(*) VOLEQ,FIAVTYPE,GEOSUB
      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
      REAL DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAEQVOL
      INTEGER BA,SI,ERRFLG,SPN
      CALL FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
     & STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLG,
     & FIAVTYPE,FIAEQVOL,BFMIND)
      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE FIAVOLTYPE(VOLEQ, MTOPP, VOLTYPE, ERRFLAG)
! Expose subroutine VOLUMELIBRARY to users of this DLL
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::FIAVOLTYPE
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: FIAVOLTYPE
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'FIAVOLTYPE'::FIAVOLTYPE
      CHARACTER*(*)   VOLEQ,VOLTYPE
      CHARACTER*10 GEOSUB,NVELEQ
      REAL            MTOPP    
      INTEGER ERRFLAG, SPN
      SPN = 0
      GEOSUB = '0'
      NVELEQ = "          "
      IF(MTOPP.EQ.0.0) MTOPP = 4.0
      IF(MTOPP.GT.20.0) MTOPP = 99.0
      CALL FIAEQ2NVELEQ(VOLEQ,SPN,GEOSUB,MTOPP,NVELEQ,VOLTYPE,
     & ERRFLAG)  
      RETURN
      END   
C ---------------------------------------------------------------------   
      SUBROUTINE VOLLIBVB8(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  TOTCU, MERCHCU, BDFT, XINT)
!... 03-23-2015     This function is make the DLL be called from VB.NET

! Expose subroutine VOLLIBVB8 to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8'::VOLLIBVB8
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
      IMPLICIT NONE
      CHARACTER*10 EQNUM
      INTEGER      REGN, ERRFLG
      REAL DBHOB,HTTOT,TOPD,TOTCU,MERCHCU,BDFT,XINT,MHT
      REAL VOL(15)
      
      CALL VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT, ERRFLG) 

      TOTCU = VOL(1)
      MERCHCU = VOL(4)
      BDFT = VOL(2)
      XINT = VOL(10)

      RETURN
      
      END SUBROUTINE VOLLIBVB8

      SUBROUTINE VOLLIBVB8XHT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  TOTCU, MERCHCU, BDFT, XINT, MHT)
!... 12-01-2015     This function is make the DLL be called from VB.NET

! Expose subroutine VOLLIBVB8 to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8XHT
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8XHT
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8XHT'::VOLLIBVB8XHT
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
      IMPLICIT NONE
      CHARACTER*10 EQNUM
      INTEGER      REGN,ERRFLG
      REAL DBHOB,HTTOT,TOPD,TOTCU,MERCHCU,BDFT,XINT,MHT
      REAL VOL(15)
      
      CALL VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT,ERRFLG) 

      TOTCU = VOL(1)
      MERCHCU = VOL(4)
      BDFT = VOL(2)
      XINT = VOL(10)
      RETURN
      
      END SUBROUTINE VOLLIBVB8XHT
      
C -------------------------------------------------------------------------      
      SUBROUTINE VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT, ERRFLAG) 

      USE CHARMOD 
	USE DEBUG_MOD
		  
      IMPLICIT NONE
      
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC
      INTEGER      IDIST

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN, MHT
      INTEGER        CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 			HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 			DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
      INTEGER        FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL           UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 			 HTREF
    
!   OUTPUTS
      REAL           NOLOGP,NOLOGS
      INTEGER        TLOGS,IFORST
    
!   ARRAYS
      INTEGER        I15,I21,I20,I7,I3,I,J
      REAL 					 VOL(15),LOGVOL(7,20)
      REAL				   LOGDIA(21,3),LOGLEN(20),BOLHT(21)

!     Extra variable
!      INTEGER IFORST
      REAL TOPD, TOTCU, MERCHCU, BDFT, XINT
      
!     Set default value for unused variables
      IF(IFORST.GT.99) THEN
        FORST = '01'
      ELSE 
        WRITE (FORST, '(I2)') IFORST
      ENDIF
      IF(FORST(2:2) .LT. '0') THEN 
        FORST(2:2) = FORST(1:1)
        FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      HT1PRD=0.0
      HT2PRD=0.0
      FCLASS=0
      DBTBH=0.0
      BTR=0.0
      PROD='01'
      HTTYPE='F'
      HTLOG=0
      STUMP=0.0
      UPSHT1=0.0
      UPSD1=0.0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      CONSPEC='    '
      DRCOB=0.0
      HTTFLL=0
      BA=0
      SI=0
      CTYPE='F'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
      IF(TOPD.GT.0.0)THEN
        MTOPP=TOPD
        MTOPS=TOPD
      ELSE
        MTOPP=0.0
        MTOPS=0.0
      ENDIF
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21

      FORST = '01'
      VOLEQ=EQNUM
      
 
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)

      MHT = HT1PRD
      RETURN
      
      END SUBROUTINE VOLLIBVB8INIT
C ------------------------------------------------------------------------------
      SUBROUTINE EZVOLLIB(VOLEQI,DBHOB,HTTOT,VOL)
C ADD THIS EAZY LIBRARY FOR USER WITH ONLY DBH AND HEIGHT
C 2017/02/08

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::EZVOLLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: EZVOLLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'EZVOLLIB'::EZVOLLIB
      IMPLICIT NONE
      
      CHARACTER*(*)   VOLEQI
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,TOPD, VOL(15), MHT
      INTEGER REGN,ERRFLG
      
      VOLEQ   = VOLEQI(1:10)
      IF(VOLEQ(1:1).EQ.'A'.OR.VOLEQ(1:1).EQ.'a')THEN
        REGN = 10
      ELSEIF(VOLEQ(1:1).EQ.'B'.OR.VOLEQ(1:1).EQ.'B')THEN
        REGN = 7
      ELSEIF(VOLEQ(1:1).EQ.'I'.OR.VOLEQ(1:1).EQ.'i')THEN
        REGN = 1
      ELSEIF(VOLEQ(1:1).EQ.'H'.OR.VOLEQ(1:1).EQ.'h')THEN
        REGN = 5
      ELSEIF(VOLEQ(1:1).EQ.'F'.OR.VOLEQ(1:1).EQ.'f')THEN
        REGN = 6
      ELSE
        READ(VOLEQ(1:1),'(I1)') REGN
      ENDIF
      
      TOPD = 0.0
      CALL VOLLIBVB8INIT(VOLEQ, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT,ERRFLG)
      
      VOLEQI = VOLEQ // char(0)
      RETURN
      END SUBROUTINE EZVOLLIB   
C ************************************************************************
      subroutine fiavoltype_r(FIAVOLEQ,FIAVOLTYPE,ERRFLAG)
C This subroutine is for R user to get voltype for a FIA voleq      !
C YW 12/03/2018
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::fiavoltype_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'fiavoltype_r_'::fiavoltype_r
      IMPLICIT NONE
      CHARACTER*10 FIAVOLEQ, FIAVOLTYPE, GEOSUB, NVELEQ
      INTEGER ERRFLAG,SPN
      CHARACTER*2 EQTYPE
      REAL MTOPP
      SPN = 0
      GEOSUB = '0'
      NVELEQ = "          "
      MTOPP = 99.0
      EQTYPE = FIAVOLEQ(1:2)
      IF(EQTYPE.EQ.'CU'.OR.EQTYPE.EQ.'BD'.OR.
     &  EQTYPE.EQ.'cu'.OR.EQTYPE.EQ.'bd')THEN
        CALL FIAEQ2NVELEQ(FIAVOLEQ,SPN,GEOSUB,MTOPP,NVELEQ,FIAVOLTYPE,
     &   ERRFLAG)  
      ELSE
        ERRFLAG = 1
        FIAVOLTYPE = " "
      ENDIF
      RETURN
      end subroutine fiavoltype_r
C ************************************************************************
      subroutine fiavol_r(VOLEQ,SPN,DBHOB_d,HTTOT_d,MTOPP_d,VOLTYPE,
     & FIAVOL_d,ERRFLAG)
C This subroutine is for R user to calc FIA vol using FIA voleq or
C NVEL voleq. When using NVEL voleq, VOLTYPE is required !
C YW 12/03/2018
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::fiavol_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'fiavol_r_'::fiavol_r
      IMPLICIT NONE
      DOUBLE PRECISION DBHOB_d,HTTOT_d,MTOPP_d,FIAVOL_d
      CHARACTER*10 VOLEQ,VOLTYPE,GEOSUB,VOLEQ2
      INTEGER ERRFLAG
      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),BFMIND,STUMP
      REAL DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAEQVOL
      INTEGER BA,SI,SPN
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      MTOPP = REAL(MTOPP_d)
      GEOSUB = "0"
      VOLTYPE = "          "
      VOL = 0.0
      FIAEQVOL = 0.0
      HT1PRD = 0.0
      HT2PRD = 0.0
      MTOPS = 0.0
      BFMIND = 0.0
      STUMP = 1.0
      DRCOB = 0.0
      UPSHT1 = 0.0
      UPSD1 = 0.0
      UPSHT2 = 0.0
      UPSD2 = 0.0
      BA = 85
      SI = 65
      VOLEQ2 = VOLEQ
      CALL FIA_VOLINIT(VOLEQ2,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
     & STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,
     & VOLTYPE,FIAEQVOL,BFMIND)
      DBHOB_d = DBLE(DBHOB)
      HTTOT_d = DBLE(HTTOT)
      MTOPP_d = DBLE(MTOPP)
      FIAVOL_d = DBLE(FIAEQVOL)
      RETURN
      end subroutine fiavol_r
C ************************************************************************
      subroutine advfiavol_r(VOLEQ,SPN,DBHOB_d,HTTOT_d,HT1PRD_d,
     & HT2PRD_d,MTOPP_d,UPSTEMHT_d,UPSTEMDIA_d,BROKENHT_d,
     & CENTROIDHT_d,CENTROIDHTDIA_d,STANDBA,SI,GEOSUB,VOLTYPE,
     & FIAVOL_d,ERRFLAG)
C This subroutine is for R user to calc FIA vol using FIA voleq or
C NVEL voleq. When using NVEL voleq, VOLTYPE is required !
C This is the advanced version and has more input variables.
C YW 12/03/2018
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::advfiavol_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'advfiavol_r_'::advfiavol_r
      IMPLICIT NONE
      DOUBLE PRECISION DBHOB_d,HTTOT_d,HT1PRD_d,HT2PRD_d,MTOPP_d
      DOUBLE PRECISION UPSTEMHT_d,UPSTEMDIA_d,BROKENHT_d,FIAVOL_d
      DOUBLE PRECISION CENTROIDHT_d,CENTROIDHTDIA_d
      CHARACTER*10 VOLEQ,VOLTYPE,GEOSUB,VOLEQ2
      INTEGER SPN,ERRFLAG,STANDBA,SI,BA
      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,VOL(15),STUMP
      REAL DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,FIAVOL,BFMIND
      VOL = 0.0
      FIAVOL = 0.0
      ERRFLAG = 0
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      HT1PRD = REAL(HT1PRD_d)
      HT2PRD = REAL(HT2PRD_d)
      MTOPP = REAL(MTOPP_d)
      UPSHT1 = REAL(UPSTEMHT_d)
      UPSD1 = REAL(UPSTEMDIA_d)
      IF(CENTROIDHT_d.GT.0.0.AND.CENTROIDHTDIA_d.GT.0.0)THEN
        UPSHT2 = REAL(CENTROIDHT_d)
        UPSD2 = REAL(CENTROIDHTDIA_d)
      ELSEIF(BROKENHT_d.GT.0.0)THEN
        UPSHT2 = REAL(BROKENHT_d)
        UPSD2 = 0.0
      ELSE
        UPSHT2 = 0.0
        UPSD2 = 0.0
      ENDIF
      BA = STANDBA
      VOLEQ2 = VOLEQ
      CALL FIA_VOLINIT(VOLEQ2,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
     & STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,
     & VOLTYPE,FIAVOL,BFMIND)
      FIAVOL_d = DBLE(FIAVOL)
      RETURN
      end subroutine advfiavol_r
C *******************************************************************************
      subroutine vollib_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,HTTOT_d,
     + MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,
     + FCLASS,DBTBH_d,BTR_d,VOL_d, ERRFLAG)
C This subroutine is for R user to calculate volume from vollib      !
C YW 02/10/2017

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollib_r_'::vollib_r

	USE CHARMOD
	USE DEBUG_MOD
      USE VOLINPUT_MOD

      IMPLICIT NONE
      
      DOUBLE PRECISION DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
      DOUBLE PRECISION HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
      DOUBLE PRECISION DBTBH_d,BTR_d,VOL_d(15)
      
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      INTEGER      SPEC,TMPSPEC,NULEQ

!   MERCH VARIABLES 
      INTEGER      REGN,HTTFLL,BA,SI,IFORST,IDIST
      REAL         STUMP,MTOPP,MTOPS  !,THT1,MAXLEN
      INTEGER      CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 		HTTOT,HT1PRD,HT2PRD  !,LEFTOV 
      REAL 		DBHOB,DRCOB,DBTBH,BTR  !,CR,TRIM
      INTEGER   FCLASS,HTLOG  !,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS  !,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 		VOL(15),LOGVOL(7,20)
      REAL		LOGDIA(21,3),LOGLEN(20),BOLHT(21)
      
      
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      MTOPP = REAL(MTOPP_d)
      MTOPS = REAL(MTOPS_d)
      HT1PRD = REAL(HT1PRD_d)
      HT2PRD = REAL(HT2PRD_d)
      UPSHT1 = REAL(UPSHT1_d)
      UPSD1 = REAL(UPSD1_d)
      STUMP = REAL(STUMP_d)
      DBTBH = REAL(DBTBH_d)
      BTR = REAL(BTR_d)
      
      READ (DIST, '(I2)') IDIST
C     Set the default value for other variable
      PROD='01'
      HTTYPE='F'
      HTLOG=0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      CONSPEC='    '
      DRCOB=0.0
      HTTFLL=0
      BA=0
      SI=0
      CTYPE='F'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21
      
C     Check if the VOLEQ is valid. If not valid, return error flag 1      
c      NULEQ = INDEX(VOLEQ,' ')
c      IF(LEN_TRIM(VOLEQ).EQ.0.OR.NULEQ.GT.0)THEN
c        CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG) 
c      ENDIF
c      TMPSPEC = 9999
c      CALL VOLEQDEF(VAR,REGN,FORST,DIST,TMPSPEC,PROD,VOLEQ,ERRFLAG)
c      IF(TMPSPEC.NE.8888) GOTO 999
      
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      
      VOL_d = DBLE(VOL)
      DBHOB_d = DBLE(DBHOB)
      HTTOT_d = DBLE(HTTOT)
      MTOPP_d = DBLE(MTOPP)
      MTOPS_d = DBLE(MTOPS)
      HT1PRD_d = DBLE(HT1PRD)
      HT2PRD_d = DBLE(HT2PRD)
      UPSHT1_d = DBLE(UPSHT1)
      UPSD1_d = DBLE(UPSD1)
      STUMP_d = DBLE(STUMP)
      DBTBH_d = DBLE(DBTBH)
      BTR_d = DBLE(BTR)
      

999   CONTINUE
      RETURN
      end subroutine vollib_r   
C *******************************************************************************
      subroutine vollib2_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,HTTOT_d,
     + MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,
     + FCLASS,DBTBH_d,BTR_d,VOL_d,LOGVOL_d,LOGDIA_d,LOGLEN_d,BOLHT_d,
     + TLOGS,NOLOGP_d,NOLOGS_d,ERRFLAG)
C This subroutine is for R user to calculate volume from vollib      !
C with output variable for logs LOGDIA,LOGVOL,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS
C YW 07/29/2020

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollib2_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollib2_r_'::vollib2_r

	USE CHARMOD
	USE DEBUG_MOD
      USE VOLINPUT_MOD

      IMPLICIT NONE
      
      DOUBLE PRECISION DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
      DOUBLE PRECISION HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
      DOUBLE PRECISION DBTBH_d,BTR_d,VOL_d(15)
      DOUBLE PRECISION LOGVOL_d(7,20),LOGDIA_d(21,3),LOGLEN_d(20)
      DOUBLE PRECISION BOLHT_d(21),NOLOGP_d,NOLOGS_d
      
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      INTEGER      SPEC,TMPSPEC,NULEQ

!   MERCH VARIABLES 
      INTEGER      REGN,HTTFLL,BA,SI,IFORST,IDIST
      REAL         STUMP,MTOPP,MTOPS  !,THT1,MAXLEN
      INTEGER      CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 		HTTOT,HT1PRD,HT2PRD  !,LEFTOV 
      REAL 		DBHOB,DRCOB,DBTBH,BTR  !,CR,TRIM
      INTEGER   FCLASS,HTLOG  !,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS  !,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 		VOL(15),LOGVOL(7,20)
      REAL		LOGDIA(21,3),LOGLEN(20),BOLHT(21)
      
      
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      MTOPP = REAL(MTOPP_d)
      MTOPS = REAL(MTOPS_d)
      HT1PRD = REAL(HT1PRD_d)
      HT2PRD = REAL(HT2PRD_d)
      UPSHT1 = REAL(UPSHT1_d)
      UPSD1 = REAL(UPSD1_d)
      STUMP = REAL(STUMP_d)
      DBTBH = REAL(DBTBH_d)
      BTR = REAL(BTR_d)
      
      READ (DIST, '(I2)') IDIST
C     Set the default value for other variable
      PROD='01'
      HTTYPE='F'
      HTLOG=0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      CONSPEC='    '
      DRCOB=0.0
      HTTFLL=0
      BA=0
      SI=0
      CTYPE='F'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21
      
C     Check if the VOLEQ is valid. If not valid, return error flag 1      
c      NULEQ = INDEX(VOLEQ,' ')
c      IF(LEN_TRIM(VOLEQ).EQ.0.OR.NULEQ.GT.0)THEN
c        CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG) 
c      ENDIF
c      TMPSPEC = 9999
c      CALL VOLEQDEF(VAR,REGN,FORST,DIST,TMPSPEC,PROD,VOLEQ,ERRFLAG)
c      IF(TMPSPEC.NE.8888) GOTO 999
      
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      
      VOL_d = DBLE(VOL)
      DBHOB_d = DBLE(DBHOB)
      HTTOT_d = DBLE(HTTOT)
      MTOPP_d = DBLE(MTOPP)
      MTOPS_d = DBLE(MTOPS)
      HT1PRD_d = DBLE(HT1PRD)
      HT2PRD_d = DBLE(HT2PRD)
      UPSHT1_d = DBLE(UPSHT1)
      UPSD1_d = DBLE(UPSD1)
      STUMP_d = DBLE(STUMP)
      DBTBH_d = DBLE(DBTBH)
      BTR_d = DBLE(BTR)
      LOGVOL_d = DBLE(LOGVOL)
      LOGDIA_d = DBLE(LOGDIA)
      LOGLEN_d = DBLE(LOGLEN)
      BOLHT_d = DBLE(BOLHT)
      NOLOGP_d = DBLE(NOLOGP)
      NOLOGS_d = DBLE(NOLOGS)
      
      RETURN
      end subroutine vollib2_r   
C ************************************************************************
C YW 2019/05/07 ADDED INPUT VARIABLES HT1PRD,HT2PRD,HTTFLL
! YW 2019/08/05 Added input variable GROSUB. This is for pacific islands equation
      SUBROUTINE BIOLIB(REGN,FORST,SPEC,BIOEQ,DBHOB,HTTOT,VOL,
     +           BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUB)
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::BIOLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: BIOLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'BIOLIB'::BIOLIB

C     The biomass component calculated in BIOGRN and BIODRY as below
C     1 ABOVE GROUND TOTAL
C     2 MERCH STEM WOOD
C     3 MERCH STEM BARK
C     4 FOLIAGE
C     5 ROOTS
C     6 BRANCHES
C     7 CROWN
C     8 MERCH STEM WOOD AND BARK
C     9 Biomass calculated from the biomass Equation BIOEQ  
     
      IMPLICIT NONE
      INTEGER REGN,SPEC,ERRFLG
      CHARACTER*(*) FORST, BIOEQ,GEOSUB
      REAL DBHOB, HTTOT, VOL(15),BIOGRN(9),BIODRY(9)
      
      REAL BIOMS(8),SG(11),WF(3),MC,RATIO,STMGRNWT,STMDRYWT
      CHARACTER*12 BMSEQ(8),NVELBEQ,FIAEQ,GEOSUB2
      CHARACTER*40 REF(8)
      REAL HT1PRD, HT2PRD,TOPD,CR,BIOMASS,VOLM(15),HTTFLL
      CHARACTER(2) FORSTI,GEOSUBI
      CHARACTER(12) BIOEQI

      FORSTI = FORST(1:2)
      GEOSUBI = GEOSUB(1:2)
      BIOEQI = BIOEQ(1:12)
      CALL BIOLIB2(REGN,FORSTI,SPEC,BIOEQI,DBHOB,HTTOT,VOL,
     +           BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUBI)
     
C The following code has been moved to BIOLIB2
!      INTEGER STEMS, I, LEN,NOINT,FIAEQNUM,STAT
      
!      DO 100, I=1,9
!        BIOGRN(I) = 0.0
!        BIODRY(I) = 0.0
!100   CONTINUE      
!!     Call Jenkin's to calculate biomass
!      CALL JENKINS(SPEC, DBHOB, BIOMS)
!!     The elements in BIOMS are dry weight in pounds as below:
!C     1 ABOVE GROUND TOTAL
!C     2 MERCH STEM WOOD
!C     3 MERCH STEM BARK
!C     4 FOLIAGE
!C     5 ROOTS
!C     6 BRANCHES
!C     7 CROWN
!C     8 MERCH STEM WOOD AND BARK
!      
!C     GET REGIONAL OR NATIONAL DEFAULT weight factor
!      CALL CRZSPDFT(REGN,FORST,SPEC,WF,BMSEQ,REF)
!      
!C     Get the moisture content from Miles $ Smith 2009
!      IF(WF(3).EQ.0)THEN
!        CALL MILESDATA(SPEC,SG)
!        WF(3) = (SG(9)-SG(10))/SG(10)*100.0
!      ENDIF
!      MC = WF(3)/100.0
!C     Calculate merch stem green weight using cubic feet volume and weight factor
!      STMGRNWT = WF(1)*(VOL(4)+VOL(7))
!      STMDRYWT = STMGRNWT/(1.0+MC)
!C     GET the ratio for stem calculated from weight factor and Jenkins
!      IF(BIOMS(8).GT.0)THEN
!        RATIO = STMDRYWT/BIOMS(8)
!      ELSE
!        RATIO = 1.0
!      ENDIF
!      IF(RATIO.LE.0) RATIO = 1
!C     Apply the ratio to biomass calculated from Jenkins and also add MC to get green weight
!      DO 200, I=1,8
!        BIODRY(I) = BIOMS(I)*RATIO
!        BIOGRN(I) = BIODRY(I)*(1+MC)  
!200   CONTINUE
!C     If BIOEQ is provided, calculate biomass from it
!      LEN = LEN_TRIM(BIOEQ)
!      FIAEQNUM = -1
!      IF(LEN.GT.0)THEN
!        CALL str2int(BIOEQ, FIAEQNUM, STAT)
!        IF(STAT.EQ.0)THEN
!          GEOSUB2 = GEOSUB(1:1)
!          ERRFLG = 0
!          CALL FIABEQ2NVELBEQ(FIAEQNUM,SPEC,NVELBEQ,GEOSUB2,ERRFLG)
!          IF(FIAEQNUM.EQ.109)THEN
!            BIODRY(9) = 0.0
!            BIOGRN(9) = 0.0
!            RETURN
!          ENDIF
!        ELSE
!          NVELBEQ = BIOEQ
!        ENDIF
!
!C       Set default values
!        CR = (HTTOT-HTTFLL)/HTTOT
!        IF(CR.LE.0.0.OR.CR.GT.1.0) CR = 0.5
!C        HT1PRD = 0
!C        HT2PRD = 0
!        TOPD = 0
!        STEMS = 1
!        ERRFLG = 0   
!        VOLM = VOL   
!        CALL BiomassLibrary2(NVELBEQ,DBHOB,HTTOT,CR,HT1PRD, 
!     +       HT2PRD,TOPD,STEMS,VOLM,BIOMASS,ERRFLG,SPEC,GEOSUB)
!        IF(NVELBEQ(12:12).EQ.'D'.OR.NVELBEQ(12:12).EQ.'G')THEN
!        !The result returned from BiomassLibrary2 is dry biomass
!        !even though the equation is green. Here save the green and dry
!        !biomass into different variables.
!          BIODRY(9) = BIOMASS
!          BIOGRN(9) = BIOMASS*(1.0+MC)
!!        ELSEIF(NVELBEQ(12:12).EQ.'G')THEN
!!          BIOGRN(9) = BIOMASS
!!          BIODRY(9) = BIOMASS/(1.0+MC)
!        ELSE
!          BIODRY(9) = BIOMASS
!          BIOGRN(9) = BIOMASS
!        ENDIF
!      ENDIF
      RETURN
      END
C ----------------------------------------------------------------------------------
      SUBROUTINE BIOLIB2(REGN,FORST,SPEC,BIOEQ,DBHOB,HTTOT,VOL,
     +           BIOGRN, BIODRY,ERRFLG,HT1PRD,HT2PRD,HTTFLL,GEOSUB)
C This is duplicated subroutine BIOLIB, but not using pointer for FORST, BIOEQ, GEOSUB
      IMPLICIT NONE
      INTEGER REGN,SPEC,ERRFLG
      CHARACTER(2) FORST,GEOSUB
      CHARACTER(12) BIOEQ
      REAL DBHOB, HTTOT, VOL(15),BIOGRN(9),BIODRY(9)
      
      REAL BIOMS(8),SG(11),WF(3),MC,RATIO,STMGRNWT,STMDRYWT
      CHARACTER*12 BMSEQ(8),NVELBEQ,FIAEQ,GEOSUB2
      CHARACTER*40 REF(8)
      REAL HT1PRD, HT2PRD,TOPD,CR,BIOMASS,VOLM(15),HTTFLL
      INTEGER STEMS, I, LEN,NOINT,FIAEQNUM,STAT
      
      DO 100, I=1,9
        BIOGRN(I) = 0.0
        BIODRY(I) = 0.0
100   CONTINUE      
!     Call Jenkin's to calculate biomass
      CALL JENKINS(SPEC, DBHOB, BIOMS)
!     The elements in BIOMS are dry weight in pounds as below:
C     1 ABOVE GROUND TOTAL
C     2 MERCH STEM WOOD
C     3 MERCH STEM BARK
C     4 FOLIAGE
C     5 ROOTS
C     6 BRANCHES
C     7 CROWN
C     8 MERCH STEM WOOD AND BARK
      
C     GET REGIONAL OR NATIONAL DEFAULT weight factor
      CALL CRZSPDFT(REGN,FORST,SPEC,WF,BMSEQ,REF)
      
C     Get the moisture content from Miles $ Smith 2009
      IF(WF(3).EQ.0)THEN
        CALL MILESDATA(SPEC,SG)
        WF(3) = (SG(9)-SG(10))/SG(10)*100.0
      ENDIF
      MC = WF(3)/100.0
C     Calculate merch stem green weight using cubic feet volume and weight factor
      STMGRNWT = WF(1)*(VOL(4)+VOL(7))
      STMDRYWT = STMGRNWT/(1.0+MC)
C     GET the ratio for stem calculated from weight factor and Jenkins
      IF(BIOMS(8).GT.0)THEN
        RATIO = STMDRYWT/BIOMS(8)
      ELSE
        RATIO = 1.0
      ENDIF
      IF(RATIO.LE.0) RATIO = 1
C     Apply the ratio to biomass calculated from Jenkins and also add MC to get green weight
      DO 200, I=1,8
        BIODRY(I) = BIOMS(I)*RATIO
        BIOGRN(I) = BIODRY(I)*(1+MC)  
200   CONTINUE
C     If BIOEQ is provided, calculate biomass from it
      LEN = LEN_TRIM(BIOEQ)
      FIAEQNUM = -1
      IF(LEN.GT.0)THEN
        CALL str2int(BIOEQ, FIAEQNUM, STAT)
        IF(STAT.EQ.0)THEN
          GEOSUB2 = GEOSUB(1:1)
          ERRFLG = 0
          CALL FIABEQ2NVELBEQ(FIAEQNUM,SPEC,NVELBEQ,GEOSUB2,ERRFLG)
          IF(FIAEQNUM.EQ.109)THEN
            BIODRY(9) = 0.0
            BIOGRN(9) = 0.0
            RETURN
          ENDIF
        ELSE
          NVELBEQ = BIOEQ
        ENDIF

C       Set default values
        CR = (HTTOT-HTTFLL)/HTTOT
        IF(CR.LE.0.0.OR.CR.GT.1.0) CR = 0.5
C        HT1PRD = 0
C        HT2PRD = 0
        TOPD = 0
        STEMS = 1
        ERRFLG = 0   
        VOLM = VOL   
        GEOSUB2 = GEOSUB(1:2)
        CALL BiomassLibrary2(NVELBEQ,DBHOB,HTTOT,CR,HT1PRD, 
     +       HT2PRD,TOPD,STEMS,VOLM,BIOMASS,ERRFLG,SPEC,GEOSUB2)
        IF(NVELBEQ(12:12).EQ.'D'.OR.NVELBEQ(12:12).EQ.'G')THEN
        !The result returned from BiomassLibrary2 is dry biomass
        !even though the equation is green. Here save the green and dry
        !biomass into different variables.
          BIODRY(9) = BIOMASS
          BIOGRN(9) = BIOMASS*(1.0+MC)
!        ELSEIF(NVELBEQ(12:12).EQ.'G')THEN
!          BIOGRN(9) = BIOMASS
!          BIODRY(9) = BIOMASS/(1.0+MC)
        ELSE
          BIODRY(9) = BIOMASS
          BIOGRN(9) = BIOMASS
        ENDIF
      ENDIF
      RETURN
      END      
C ----------------------------------------------------------------------------------
      SUBROUTINE VOLLIBVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,
     +    DBHOB,
     &    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     &    BA,SI,CTYPE,ERRFLAG,IDIST,MERRULES)
     
! 2017/09/18 YW Created this subroutine for VB.NET to call the vollib with MRULES input
! Expose subroutine VOLUMELIBRARY to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB'::VOLLIBVB
      
      USE CHARMOD 
	USE DEBUG_MOD
	USE MRULES_MOD
	  
      IMPLICIT NONE
      
!     Parameters
      INTEGER         REGN
      CHARACTER*(*)   FORST, VOLEQ
      REAL            MTOPP, MTOPS, STUMP,DBHOB, DRCOB
      CHARACTER*(*)   HTTYPE
      REAL            HTTOT
      INTEGER         HTLOG
      REAL            HT1PRD, HT2PRD, UPSHT1, UPSHT2, UPSD1, UPSD2
      INTEGER         HTREF
      REAL            AVGZ1, AVGZ2
      INTEGER         FCLASS
      REAL            DBTBH, BTR
      INTEGER         I3, I7, I15, I20, I21
      REAL            LOGVOL(I7,I20), LOGDIA(I21,I3), LOGLEN(I20)
      REAL            BOLHT(I21)
      INTEGER         TLOGS
      REAL            NOLOGP,NOLOGS
      INTEGER         CUTFLG, BFPFLG, CUPFLG, CDPFLG, CUSFLG, CDSFLG
      CHARACTER*(*)   PROD
      CHARACTER*(*)   CONSPEC
      INTEGER         HTTFLL
      CHARACTER*(*)   LIVE, CTYPE
      INTEGER         ERRFLG
      CHARACTER*2     DIST, VAR
      INTEGER         IDIST
!     Local variables      
!     Variable required for call to VOLINIT      
      INTEGER         SPFLG
      REAL            VOL(15)
      INTEGER         BA, SI
      INTEGER         ERRFLAG
      TYPE(MERCHRULES):: MERRULES
        
          CALL VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)

c           CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
c     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
c     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
c     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
c     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
c     +    BA,SI,CTYPE,ERRFLAG,IDIST)

 1000 RETURN
      
      END SUBROUTINE VOLLIBVB   
! --------------------------------------------------------------------
      subroutine str2int(str,int,stat)
      implicit none
      ! Arguments
      ! stat = 0, convertion OK
      ! stat != 0, cinversion error
      character(len=*),intent(in) :: str
      integer,intent(out)         :: int
      integer,intent(out)         :: stat
      integer i
      !read(str,*,iostat=stat)  int
      stat = 0
      do i=1, len(str)
        if (str(i:i) < '0' .or. str(i:i) > '9') then
          stat = i
          exit
        end if
      end do
      if (stat.eq.0) read(str, *) int
      
      end subroutine str2int               

            
