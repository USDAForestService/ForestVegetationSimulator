      subroutine vollibfia(regn, iforst, voleqi, dbhob, httot,
     +                     ht1prd,ht2prd,stems,si,ba,QUAL,
     +                     ht_break,upsd1,upsht1,dia_centroid,
     +                     ht_centroid,ht_dbh,mtops,stump,
     +                     totcu, sawcu, mcu4, scrbbf, intlbf, errflag)

c regn     National forest admin region number
c iforst   National forest admin forest number
c voleqi   NVEL volume equation number (10 characters string)
c dbhob    Diameter at breast height (or DRC when ht_dbh < 4.5)
c httot    tree total height from ground to tip
c ht1prd   Sawtimber height
c ht2prd   bolht (height to 4 inch top)
c stems    woodland species stem count
c si       site index
c ba       stand basal area
c QUAL     First log merchanability (formcl in FIA tree table)
c ht_break Height to top break
c upsd1    Upper stem diameter
c upsht1   Upper stem height
c dia_centroid Centroid diameter
c ht_centroid  Height to centroid diameter
c ht_dbh   Height to dbhob
c mtops    Merch top diameter (4 inch)
c stump    Stump height
c---------------------------------------------------------------
c 11/07/2017 YW
c Currently there is no variable for QUAL and ht_break
c Need to create new variable for them!!!!!!
c--------------------------------------------------------------
      IMPLICIT NONE
      character*(*) voleqi
      real totcu,sawcu,mcu4,scrbbf,intlbf
c     variables from VOLINIT
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN
      INTEGER        CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 	HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 	DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
      INTEGER   FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 	VOL(15),LOGVOL(7,20)
      REAL	LOGDIA(21,3),LOGLEN(20),BOLHT(21)
C  variables for stump dia and vol
      INTEGER SPN
      REAL STUMPDIB, STUMPDOB, VOLIB, VOLOB    
      REAL DIB,DOB,HTUP,MHT  
      
c  test biomass calc variable
      REAL WF(3), BMS(8)
      INTEGER SPCD, FOREST   
c  FIA input variables
      REAL ht_break,dia_centroid,ht_centroid,ht_dbh
      INTEGER QUAL,stems
 !********************************************************************
      VOLEQ   = VOLEQI(1:10)
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
c      HT1PRD=0.0
c      HT2PRD=0.0
      FCLASS=0
c  NVEL use variable FCLASS to save woodland species stem count
      IF(stems.GT.1.AND.ht_dbh.LT.4.5) THEN
        FCLASS = stems
      ENDIF

      DBTBH=0.0
      BTR=0.0
      PROD='01'
      HTTYPE='F'
      HTLOG=0
c      STUMP=0.0
c      UPSHT1=0.0
c      UPSD1=0.0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      CONSPEC='    '
      DRCOB=0.0
      IF(ht_dbh.LT.4.5) DRCOB = dbhob
      HTTFLL=0
c      BA=0
c      SI=0
      CTYPE='F'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
      MTOPP=0.0
c      MTOPS=0.0
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21

      
 
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      IF(ERRFLAG.EQ.0)THEN
        totcu = VOL(1)
        sawcu = VOL(4)
        mcu4 = VOL(7)+VOL(4)
        scrbbf = VOL(2)
        intlbf = VOL(10)
      ELSE
        totcu = 0.0
        sawcu = 0.0
        mcu4 = 0.0
        scrbbf = 0.0
        intlbf = 0.0
      ENDIF
      RETURN
      END
c================================================================
      subroutine vollibfia2(regn, iforst, voleqi, dbhob, httot,
     +                     totcu, sawcu, mcu4, scrbbf, intlbf, errflag)
c--------------------------------------------------------------
      IMPLICIT NONE
      character*(*) voleqi
      real totcu,sawcu,mcu4,scrbbf,intlbf
c     variables from VOLINIT
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN
      INTEGER        CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 	HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 	DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
      INTEGER   FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 	VOL(15),LOGVOL(7,20)
      REAL	LOGDIA(21,3),LOGLEN(20),BOLHT(21)
C  variables for stump dia and vol
      INTEGER SPN
      REAL STUMPDIB, STUMPDOB, VOLIB, VOLOB    
      REAL DIB,DOB,HTUP,MHT  
      
c  test biomass calc variable
      REAL WF(3), BMS(8)
      INTEGER SPCD, FOREST   
 !********************************************************************
      VOLEQ   = VOLEQI(1:10)
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
      MTOPP=0.0
      MTOPS=0.0
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21

      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      IF(ERRFLAG.EQ.0)THEN
        totcu = VOL(1)
        sawcu = VOL(4)
        mcu4 = VOL(7)+VOL(4)
        scrbbf = VOL(2)
        intlbf = VOL(10)
      ELSE
        totcu = 0.0
        sawcu = 0.0
        mcu4 = 0.0
        scrbbf = 0.0
        intlbf = 0.0
      ENDIF
      RETURN
      END
c================================================================
      subroutine vollibfia3(regn, iforst, voleqi, dbhob, httot,
     +mtopp, mtops, prodi,totcu,primcu,mcu,scrbbf,intlbf,errflag)
c--------------------------------------------------------------
C This rubroutine is added for Android AddVol app to calculate volume
C It needs input for prod, mtopp, mtops.
C YW 2019/02/26
      IMPLICIT NONE
      character*(*) voleqi, prodi
      real totcu,primcu,mcu,scrbbf,intlbf
c     variables from VOLINIT
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN
      INTEGER        CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 	HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 	DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
      INTEGER   FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 	VOL(15),LOGVOL(7,20)
      REAL	LOGDIA(21,3),LOGLEN(20),BOLHT(21)
C  variables for stump dia and vol
      INTEGER SPN
      REAL STUMPDIB, STUMPDOB, VOLIB, VOLOB    
      REAL DIB,DOB,HTUP,MHT  
      
 !********************************************************************
      VOLEQ   = VOLEQI(1:10)
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
      PROD = prodi(1:2)
      IF(PROD(2:2).LT.'0') THEN
        PROD(2:2) = PROD(1:1)
        PROD(1:1) = '0'
        IF(PROD(2:2).LT.'0') PROD(2:2) = '1'
      ENDIF
      HT1PRD=0.0
      HT2PRD=0.0
      FCLASS=0
      DBTBH=0.0
      BTR=0.0
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
      IF(MTOPP.LT.0.1) MTOPP = 0.0
      IF(MTOPS.LT.0.1) MTOPS = 0.0
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21

      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      IF(ERRFLAG.EQ.0)THEN
        totcu = VOL(1)
        primcu = VOL(4)
        mcu = VOL(7)+VOL(4)
        scrbbf = VOL(2)
        intlbf = VOL(10)
      ELSE
        totcu = 0.0
        primcu = 0.0
        mcu = 0.0
        scrbbf = 0.0
        intlbf = 0.0
      ENDIF
      RETURN
      END
c================================================================
      subroutine vollibfsveg(regn,iforst,idist,voleqi,dbhob,httot,
     +       mtopp,stems,dbtbh,stump,ba,si,tcu, mcu, bdf, errflag)
c--------------------------------------------------------------
c    This subroutine is created for FSVeg to call the library
!  YW 20210719 Set FCLASS initial to 0 in order to pick the species default form class
!  YW 20210727 Added DBTBH as an input variable to the subroutine
!  YW 20200811 Added stump, ba and si as input variables
      IMPLICIT NONE
      character*(*) voleqi
      real tcu,mcu,bdf
      integer stems
c     variables from VOLINIT
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
c      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
c      CHARACTER*10 EQNUM
c      INTEGER      SPEC

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN
      INTEGER        CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
c      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 	HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 	DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
      INTEGER   FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 	VOL(15),LOGVOL(7,20)
      REAL	LOGDIA(21,3),LOGLEN(20),BOLHT(21)
C  variables for stump dia and vol
c      INTEGER SPN
c      REAL STUMPDIB, STUMPDOB, VOLIB, VOLOB    
c      REAL DIB,DOB,HTUP,MHT  
      
c  test biomass calc variable
c      REAL WF(3), BMS(8)
c      INTEGER SPCD, FOREST   
 !********************************************************************
      VOLEQ   = VOLEQI(1:10)
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
C     FOR WOODLAND SPECIES, NO-OF-STEMS IS PASSED IN USING FCLASS
      IF(VOLEQ(4:6).EQ.'DVE'.AND.
     &  (VOLEQ(1:1).EQ.'2'.OR.VOLEQ(1:1).EQ.'3'.OR.
     &   VOLEQ(1:1).EQ.'4'))THEN
        FCLASS = 1       
        IF(STEMS.GT.1) FCLASS=STEMS
      ENDIF
C      DBTBH=0.0
      BTR=0.0
      PROD='01'
      HTTYPE='F'
      HTLOG=0
c      STUMP=0.0
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
c      BA=0
c      SI=0
      CTYPE='F'
      LIVE='L'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
c      MTOPP=0.0
      MTOPS=0.0
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21

      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      IF(ERRFLAG.EQ.0)THEN
        tcu = VOL(1)
C R8 old clark equation does not have tatal cubic volume.
C Use volume to 4" top as total cubic volume        
        IF(tcu.EQ.0.0.AND.REGN.EQ.8.AND.
     +     VOLEQ(4:6).EQ.'CLK'.AND.VOLEQ(3:3).NE.'1')THEN
          tcu = VOL(4)+VOL(7)
        ENDIF
        mcu = VOL(4)
        bdf = VOL(2)
      ELSE
        tcu = 0.0
        mcu = 0.0
        bdf = 0.0
      ENDIF
      RETURN
      END