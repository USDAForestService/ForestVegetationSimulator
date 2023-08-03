C  Region 9 Profile Model Volume Calculation
C  revised TDH 01/12/2010  Added code for calculating log volumes and
C                          added the definfe type CLKCOEF for passing
C                          coefficients
C  revised RNH 11/19/2009  Added numerical error trap in equation
C                          for stmDib, subroutine r9dib
C  revised TDH 05/12/2009  Added debug code to first three subroutines.
C  revised TDH 08/12/2010  Fixed Log/BdFt error - initialize NOLOGP, 
C                          NOLOGS to zero.
C  revised MGV 12/10/2010  Fixed order of code in R9PREP so top DIB is
C                          read before assigning coefficients a17, b17
C  revised TDH 01/21/2011  Trying to fix bug that doesn't give volumes
C                          for trees with merch ht only. addded tcfvol
C                          code on line 194
C  revised TDH 02/04/2011  Fixed bug listed above by moving the merch
C                          initialization above the coeffs/ht logic
C                          in R9PREP.
C
C  revised TDH 03/03/2011  Add ABS(R) on line 904 since negative
C                          values for R would cause problems.

C  revised TDH 5/24/2011   Added comments and an ISNAN check to final
C                          stemHt in r9Ht
C  revised RNH 5/24/2011   Added subroutine IISNAN to replace ISNAN check
C                          which isn't available in Lahey Fortran
C
C  revised RNH 07/21/2011  reassigned sawHT to ht1prd
c
C  revised YW  11/15/2011  Added upsHt1 to r9clark, r9prep, and r9dia417.
C                          Also added logVol to r9bdft and cType to r9clark.
C
C          YW  12/08/2011  Added check number of logs not greater than 20.
C
C          YW  02/28/2012  Changed to calc HT for prod 1 when ht1prd = 0
C          YW  08/21/2012  Added error flag check and reset vol array.
C YW 01/18/2013 Added vol calculation for stump (VOL(14)) and tip VOL(15)
C YW 04/15/2015 Added to make prod 14 to be same as prod 01 for volume calculation.
C YW 10/06/2015 Added TLOGS,NOLOGP and NOLOGS to the R9CLARK subroutine
C YW 01/22/2016 Added extra NaN check in r9ht routine for stemHt.
C YW 03/02/2016 Removed the error check for small tree (<17.3) and calculation using small tree logic
c YW 04/13/2017 Moved the stump and tip volume calc to volinit subroutine.
c YW 12/20/2017 NCrookston modified R9HT on Sept 2017 to deal with degenerate 
c               math when trying to exponentiate negative numbers with real-valued powers.
c YW 01/25/2018 Removed the broken top calculation, which cause problem for CP.
c YW 07/13/2018 Modified R9cor to remove the adjustment factor for yellow-poplar (621).
C YW 02/14/2019 Check height greater than merchL (it was checking minLen before) for volume calculation
C YW 09/24/2019 R08 prod 08 uses MTOPP (DIB), so changed to use COEFFS to get sawHt (same as PROD8 subroutine)
! YW 2021/06/21 Comment out the subroutine iisnan(x,res). It is not used and cause compile error on some system (Mac).
! YW 2023/06/12 Modified r9cuft to avoid the number is off the limit
C-------------------------------------------------------------------------
C  This subroutine is designed for use with the VOLLIB routines in 
C  the National Volume Estimator Library.  It returns arrays filled 
C  with different types of volumes (vol), log lengths (logLen), 
C  and log volumes (logVol).  It uses a standard stump height (stump) 
C  if none was passed in.  An error flag (errFlg) is also returned 
C  indicating any specific error found.
C
C  Variable definitions are located at the end of this file.
C_______________________________________________________________________
C
      subroutine r9clark (volEq,stump,mTopP,mTopS,dbhOb,
     &                    ht1Prd,ht2Prd,htTot,logDia,bolHt,Loglen,
     &                    logVol,vol,cutFlg,bfpFlg,cupFlg,cdpFlg,
     &             spFlg,prod,errFlg,cType,upsHt1,TLOGS,NUMLOGP,NUMLOGS)
C_______________________________________________________________________
C
 
      USE DEBUG_MOD
      USE CLKCOEF_MOD
      
      implicit none
      
C     Shared variables
      character forst*2,prod*2,volEq*10
      integer   geog,iProd,cutFlg,bfpFlg,cupFlg,spFlg,cdpFlg
      integer   errFlg,REGN
      real      minBfD,maxLen,minLen,merchL,mTopP,mTopS,stump,trim
      real      dbhOb,ht1Prd,ht2Prd,htTot,topDib,upsHt1 
      real      logVol(7,20),logDia(21,3),bolHt(21),vol(15),logLen(20)
      character*1 cType
C     Internal variables
      integer   numSeg,spp,i,j
      real      plpDib,sawDib,plpHt,sawHt,topHt,totHt,dbhIb,dib17
      real      r,c,e,p,b,a,a4,b4,a17,b17,cfVol,tcfVol,shrtHt
      real      cf1,cf2,cf3
      REAL      TLOGVOL,dob17
      logical   short      
      
      INTEGER   TLOGS,NOLOGP,NOLOGS      
      REAL      NUMLOGP,NUMLOGS,brokHt 
      TYPE(CLKCOEF):: COEFFS
      TYPE(CLKCOEF):: COEFFSO
      real ht1,ht2,TEMPVOL(15)
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 10) ' -->Enter R9CLARK'
   10    FORMAT (A)   
   		END IF

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 100)'  STUMP HT1PRD HT2PRD'
100      FORMAT (A)
  			 WRITE  (LUDBG, 104)STUMP, HT1PRD, HT2PRD
104      FORMAT(1X, F5.1, 2X, F5.1, F5.1)
      END IF
      
C     Initialize output variables

      cfVol = 0
      TLOGVOL = 0
      numSeg=0
      errFlg=0
      tcfVol=0
      do 110,i=1,20
        logLen(i)=0.0
        do 105, j=1,7
          logVol(j,i)=0.0
105     continue
110   continue
      do 120,i=1,21
        bolHt(i)=0.0
        do 115,j=1,3
          logDia(i,j)=0.0
115     continue
120   continue
      do 130,i=1,15
         vol(i)=0.0
130   continue
C-----Check input values and prepare variables
C     region upper stem height field
C     R8 uses UPSHT1 for 4 or 7/9 HT depends on prod
c     this is checked in r8prep
c      IF(volEq(1:1).EQ.'8')THEN
c        IF(upsHt1 .gt. 0.0)THEN
c          IF(prod.EQ.'01')THEN
c            IF(ht1Prd.LE.0.0)THEN
c              ht1Prd = upsHt1
c            ENDIF
c          ELSE
c            IF(ht2Prd.LE.0.0)THEN
c              ht2Prd = upsHt1
c              upsHt1 = 0
c            ENDIF
c          ENDIF
c        ENDIF
c      ELSE
! 2021/02/25 The following height check looks not right. I comment out
!      IF(volEq(1:1).EQ.'9')THEN
C     Region 9      
!      if (upsHt1 .gt. 0.0 .and. ht1Prd .le. 0.0) then
!         if (htTot .le. 0 .and. ht2prd .le.0) then
!           ht1Prd = upsHt1
!         endif
!         upsHt1 = 0
!      endif
!      ENDIF
! End comment out
C  added on 04/15/2015 for prod 14 to be same as prod 01
      if (prod .eq. '14') prod = '01'
      IF(VOLEQ(1:1).EQ.'9')THEN      
      call r9Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &            spp,geog,COEFFS,forst,maxLen,
     &            minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &            prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,
     &            upsHt1)
      ELSEIF(VOLEQ(1:1).EQ.'8')THEN 
      call r8Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &            spp,geog,COEFFS,forst,maxLen,
     &            minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &            prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,
     &            upsHt1,COEFFSO) 
        !Treat prod 08 to be same as 01   
        !if(iProd.eq.8) iProd = 1  
      ELSE
        errFlg = 1
      ENDIF
      if(errFlg.ne.0) return
      
      IF(VOLEQ(1:1).EQ.'9')THEN
C-----Get DIBs at heights of 4.5' and 17.3'
      call r9dia417(COEFFS,topDib,dbhOb,topHt,ht1Prd,ht2Prd,
     &               htTot,sawDib,plpDib,errFlg,upsHt1,volEq)
      ENDIF
      if(errFlg.ne.0) return
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 200)'  DBHIB DIB17 SAWDIB PLPDIB'
  200    FORMAT (A)
  			 WRITE  (LUDBG, 220)coeffs%DBHIB,coeffs%DIB17, SAWDIB, PLPDIB
  220    FORMAT(1X, F5.1, 2X, F5.1, F5.1, 2X, F5.1)
      END IF

C-----Get total height
C    R9 uses inside bark coef, R8 using outside bark coef
      IF(VOLEQ(1:1).EQ.'9')THEN
      call r9totHt(COEFFS%totHt,htTot,COEFFS%dbhIb,COEFFS%dib17,topHt,
     &             topDib,COEFFS%a, COEFFS%b,errFlg)
      ELSE
      call r9totHt(COEFFS%totHt,htTot,dbhOb,COEFFSO%dib17,topHt,
     &             topDib,COEFFSO%a, COEFFSO%b,errFlg)
      COEFFSO%totHt = COEFFS%totHt
      ENDIF
      if(COEFFS%totHt.le.17.3) errFlg=8
      if(errFlg.ne.0) return
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 300)'  TOTHT HTTOT TOPHT TOPDIB STUMP'
  300    FORMAT (A)
  			 WRITE  (LUDBG, 320)coeffs%TOTHT, HTTOT, TOPHT, TOPDIB, STUMP
  320    FORMAT(1X, F5.1, 2X, F5.1, 1X, F5.1, 1X, F5.1, 1X, F5.1)
      END IF
      
!      COEFFS%DBHIB = DBHIB
!      COEFFS%DIB17 = DIB17
!      COEFFS%TOTHT = TOTHT

C-----Get total volume to the tip
      if(cutFlg.eq.1) then
        IF(VOLEQ(1:1).EQ.'8'.AND.
     &    (VOLEQ(7:7).EQ.'O'.OR.VOLEQ(7:7).EQ.'0'))THEN
!        R8 OUTSIDE BARK VOLUME (EQ 8*1CLKO***)
          CALL r9cuft(cfVol,COEFFSO,STUMP, COEFFS%TOTHT, errFlg)
        ELSE
          call r9cuft(cfVol,COEFFS,STUMP, COEFFS%TOTHT, errFlg)
        ENDIF
        if(errFlg.ne.0) return
        if(short) cfVol=cfVol*shrtHt/17.3
        vol(1)=cfVol
      endif

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 400)'  CFVOL '
  400    FORMAT (A)
  			 WRITE  (LUDBG, 420)CFVOL
  420    FORMAT(1X, F7.3)
      END IF
!***********************************************************************
C-----Get height to pulpwood top

      plpHt=0.0
      if(iProd.ne.1 .or. spFlg.eq.1) then
        if(htTot.gt.0.0 .and. ht2prd.gt.0) then
          plpHt=ht2prd
        !elseif(PLPDIB .GE. COEFFS%DBHIB)THEN
        !  PLPHT = TOPHT       !biomass tree
        else
          IF(VOLEQ(1:1).EQ.'9')THEN
            call r9ht(plpHt,COEFFS, plpDib,errFlg)
          ELSE
            !R8 uses outsidebark diameter to calculate HT
            IF(upsHt1.GT.0.0.AND.ht2Prd.GT.0.0)THEN
              plpHt = ht2Prd
            ELSE  
              call r9ht(plpHt,COEFFSO, plpDib,errFlg)
            ENDIF
          ENDIF
          if(errFlg.ne.0) return
        endif
        if(topDib.le.plpDib .and. topHt.lt.plpHt) plpHt=topHt
C       check height greater than merchL (20190214)        
        if(plpHt.lt.merchL+stump+trim) plpHt=0.0
        !reassign plpht to ht2prd 
        ht2prd = plpHt
C-----Get total merchantable product volumes
        if(plpHt-stump.ge.minLen) then
          IF(VOLEQ(1:1).EQ.'8'.AND.
     &      (VOLEQ(7:7).EQ.'O'.OR.VOLEQ(7:7).EQ.'0'))THEN
!        R8 OUTSIDE BARK VOLUME (EQ 8*1CLKO***)
            CALL r9cuft(cfVol,COEFFSO,STUMP,PLPHT,errFlg)
          ELSE
            call r9cuft(cfVol,COEFFS,STUMP,PLPHT,errFlg)
          ENDIF
          if(errFlg.ne.0) return
          if(short) cfVol=cfVol*shrtHt/17.3
          tcfVol=cfVol
          if(iProd.ne.1) then
            if(cupFlg.eq.1) vol(4)=cfVol
            if(cdpFlg.eq.1) vol(6)=cfVol/79.0
          endif
        endif
      endif
      
      TLOGS = 0
      NOLOGP = 0
      NOLOGS = 0
!***********************************************************************
C-----Get height to sawtimber top
      sawHt=0.0
      !Added R08 prod 08 here (2019/09/23)
      if(iProd.eq.1.OR.(VOLEQ(1:1).EQ.'8'.AND.iprod.EQ.8)) then
c       cType is checked to let Volume Tester program to calculate boardfoot
c       volume for FVS if only total height is provided. 11/15/2011 (yw)
        if(cType.eq.'F' .or. cType.eq.'f') then
          if(ht1Prd.gt.4.5) then
            sawHt=ht1Prd
          else
            IF(VOLEQ(1:1).EQ.'9'
     &       .OR.(VOLEQ(1:1).EQ.'8'.AND.iprod.EQ.8))THEN
              call r9ht(sawHt,COEFFS, sawDib,errFlg)
            ELSE
            !R8 uses outside bark to calc HT
              call r9ht(sawHt,COEFFSO, sawDib,errFlg)
            ENDIF
          endif
        else
          if(ht1Prd.gt.4.5) then
            sawHt=ht1Prd
          elseif(ht1prd.le.0.01) then
c         Saw height calc is requested by having 1 < ht1prd < 4.5'
            IF(VOLEQ(1:1).EQ.'9'
     &       .OR.(VOLEQ(1:1).EQ.'8'.AND.iprod.EQ.8))THEN
              call r9ht(sawHt,COEFFS, sawDib,errFlg)
            ELSE
              call r9ht(sawHt,COEFFSO, sawDib,errFlg)
            ENDIF
          endif
        endif
        if(errFlg.ne.0) return
        if(topDib.le.sawDib .and. topHt.lt.sawHt) sawHt=topHt
C       check height greater than merchL (20190214)      
        if(sawHt.lt.merchL+trim+stump) sawHt=0.0
				!reassign sawHT to ht1prd 
         ht1prd = sawHT
         
C-----Get sawtimber cubic volumes
        if(cupFlg.eq.1 .or. spFlg.eq.1) then
          IF(VOLEQ(1:1).EQ.'8'.AND.
     &      (VOLEQ(7:7).EQ.'O'.OR.VOLEQ(7:7).EQ.'0'))THEN
!        R8 OUTSIDE BARK VOLUME (EQ 8*1CLKO***)
            CALL r9cuft(cfVol,COEFFSO,STUMP,SAWHT,errFlg)
          ELSE
            !R8 Prod 08 uses PROD8 subroutine to calculate vol (2019/09/25)
            IF(VOLEQ(1:1).EQ.'8'.AND.iprod.EQ.8)THEN
              TEMPVOL = 0.0
C             Check if HTTOT is entered (2020/10/28)
              IF(HTTOT.LE.0.0)THEN
                IF(COEFFS%TOTHT.GT.0.0)THEN
                  HTTOT =  COEFFS%TOTHT
                ELSE
                  HTTOT = HT2PRD
                ENDIF
              ENDIF              
              CALL PROD8(TEMPVOL,DBHOB,HTTOT,COEFFS%FIXDI,SPP,      
     >        COEFFS%SPGRP,COEFFS%R,COEFFS%C,COEFFS%E,COEFFS%P,COEFFS%B,
     >        COEFFS%A,COEFFS%A4,COEFFS%B4,COEFFS%A17,COEFFS%B17,
     >        COEFFSO%R,COEFFSO%C,COEFFSO%E,COEFFSO%P,COEFFSO%B,
     >        COEFFSO%A,COEFFS%AFI,COEFFS%BFI,MTOPP)
              cfVol = TEMPVOL(4)
            ELSE
              call r9cuft(cfVol,COEFFS,STUMP,SAWHT,errFlg)
            ENDIF
          ENDIF
          if(errFlg.ne.0) return
          if(short) cfVol=cfVol*shrtHt/17.3
          if(cupFlg.eq.1) vol(4)=cfVol
          if(cdpFlg.eq.1) vol(6)=cfVol/79
          
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 400)'  CFVOL '
  500    FORMAT (A)
  			 WRITE  (LUDBG, 420)CFVOL
  520    FORMAT(1X, F5.1)
      END IF

C-----Get topwood cubic volumes
          if(spFlg.eq.1) vol(7)=max(tcfVol-cfVol,0.0)
          if(spFlg.eq.1) vol(9)=max(tcfVol-cfVol,0.0)/79.0
        endif
      
!...  get log lengths, dibs
        CALL R9LOGS(SAWHT, PLPHT, STUMP, MINLEN, MAXLEN, TRIM,
     &       LOGLEN,LOGDIA,NOLOGP,NOLOGS,TLOGS,COEFFS,ERRFLG,BOLHT) 
     
        IF(ERRFLG .NE. 0) THEN
          DO 525, I=1,15
             VOL(I) = 0.0
  525     CONTINUE
          RETURN
        ENDIF
        NUMSEG = NOLOGP
      
      IF (DEBUG%MODEL) THEN
         DO 550 I=1,TLOGS
         WRITE  (LUDBG, 530)'LOGDIA ', I, LOGDIA(I,2),'LOGLEN ', I,
     &     LOGLEN(I)
  530    FORMAT (A, I2, F6.1, 2X, A,I2, F6.1)
  550    CONTINUE
      END IF
      
!      vol,logLen,numSeg,logDia,bolHt,dbhOb,
!     &                  maxLen,minLen,trim,minBfD,merchL,
!     &                  stump,geog,sawHt,COFS,errFlg)
     
C-----Get board foot volumes
        if(bfpFlg.eq.1) then
          call r9bdft(vol,logLen,NUMSEG,logDia,errFlg,logVol)
          if(errFlg.ne.0) return
        endif
       
       IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 570)'vol(2) =  ', vol(2)
  570    FORMAT (A, F7.2)
       END IF
        
        
      ELSE  !prod ne 1
      
        CALL R9LOGS(SAWHT, PLPHT, STUMP, MINLEN, MAXLEN, TRIM,
     &       LOGLEN,LOGDIA,NOLOGP,NOLOGS,TLOGS,COEFFS,ERRFLG,BOLHT) 
!     Added calculate boardfoot volume if flag is turned on (YW 2019/05/29)    
        NUMSEG = NOLOGS
        if(bfpFlg.eq.1) then
          call r9bdft(vol,logLen,NUMSEG,logDia,errFlg,logVol)
          if(errFlg.ne.0) return
        endif
        IF(ERRFLG .NE. 0) THEN
          DO 575, I=1,15
             VOL(I) = 0.0
  575     CONTINUE
          RETURN
        ENDIF
      
      endif !prod eq 1
!***********************************************************************
         
!...  get cubic log volumes'
!      CALL R9LGCFT(TLOGS, LOGLEN, LOGDIA, LOGVOL, TLOGVOL, tcfVol)
!test I think it should use cfvol (5/21/2015)
      cfvol = VOL(4) + VOL(7)      
      CALL R9LGCFT(TLOGS, LOGLEN, LOGDIA, LOGVOL, TLOGVOL, cfVol)

      NUMLOGP = NOLOGP
      NUMLOGS = NOLOGS
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 580)'TCFVOL  = ', TCFVOL
  580    FORMAT (A, F6.1)
      END IF
     
      IF (DEBUG%MODEL) THEN
         DO 650 I=1,TLOGS
         WRITE  (LUDBG, 600)'LOGVOL ', I, LOGVOL(4,I)
  600    FORMAT (A, I2, F8.4)
  650    CONTINUE
  
      
      WRITE  (LUDBG, 675)'total cubic ', tcfvol
  675    FORMAT (A, F7.2)
!  I have no idea why the following statement causes some volumes
!  to not be caclulated but it does.  wtf?      
      WRITE  (LUDBG, 695)'total log vol ', tlogvol
  695    FORMAT (A, F7.2)
      END IF
      READ(VOLEQ(1:1),'(I1)')REGN
C-----Apply correction factors
      IF(REGN.EQ.9) call r9cor(vol,logVol,spp,iProd)
	
      if(vol(1).gt.0.0) vol(1)= nint(vol(1)*10.0)/10.0
	    if(vol(2).gt.0) vol(2)=  nint(vol(2))
	    if(vol(4).gt.0) vol(4)= nint(vol(4)*10.0)/10.0
	    if(vol(6).gt.0) vol(6)=  nint(vol(6)*10.0)/10.0
	    if(vol(7).gt.0) vol(7)=  nint(vol(7)*10.0)/10.0
	    if(vol(9).gt.0) vol(9)=  nint(vol(9)*10.0)/10.0
	    if(vol(10).gt.0) vol(10)=  nint(vol(10))
	    if(vol(12).gt.0) vol(12)=  nint(vol(12))
c   calc Tip Vol(15) and stump vol(14)
       vol(15)=vol(1)-vol(4)-vol(7)
       IF(vol(15).LT.0.0) vol(15)=0.0	
       ht1 = 0.0
       ht2 = stump
       call r9cuft(cfVol,COEFFS,ht1, ht2, errFlg)
       vol(14)=cfvol
       if(vol(14).LT.0.0) vol(14) = 0.0
       
	    IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 700)'CRDVOLPP  CRDVOLSP'
  700    FORMAT (A)
         WRITE  (LUDBG, 710)VOL(6), VOL(9)
  710    FORMAT (F6.2, 2X, F6.2)    
   		END IF


      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 800) ' <--Exit R9CLARK'
  800    FORMAT (A)   
      END IF
c     save the total HT (20180208)
      IF(htTot.LE.0.01) htTot=COEFFS%totHt
      return
      end
C_______________________________________________________________________
C
      subroutine r9Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &                  spp,geog,COEFFS,forst,maxLen,
     &                  minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &                  prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,
     &                  upsHt1)
C_______________________________________________________________________
C
C  Parse out the info in the volume equation (volEq).  Check to see if 
C  the specified measurements are reasonable.  Get coefficients and 
C  merchantability rules for the specified species and product.
      
      USE DEBUG_MOD
      USE CLKCOEF_MOD
      
      implicit none
      INCLUDE 'r9coeff.inc'    !'R9COEFF.INC'
      
      integer   errFlg,spp,sppGrp,geog,iProd,k,sppIdx
      real      dbhOb,topDib,topHt,sawHt,maxLen,minLen
      real      dib17,upprHt,lowrHt,merchL,stump,upsHt1
      real      mTopP,mTopS,trim,minBfD,ht1Prd,ht2Prd,htTot
      TYPE(CLKCOEF):: COEFFS
      real      plpDib,sawDib,shrtHt,dbhIb
      character volEq*10,forst*2,prod*2,tmpStr*2
      logical   short
      INTEGER REGN, OPT,EVOD
      REAL MINLENT,BTR,DBTBH
      CHARACTER*1 COR
!      real      r,c,e,p,b,a,a4,b4,a17,b17

C  DW 08/22 initializations of variables otherwise uninitialized prior to MRULES call
      OPT = 0
      EVOD = 0
      MINLENT = 0
      BTR = 0
      DBTBH = 0

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 10) ' -->Enter R9PREP'
   10    FORMAT (A)   
   		END IF
      REGN = 9
      if(volEq(10:10).lt.'0' .or. volEq(10:10).gt.'9') then
        tmpStr=volEq(8:9)
        volEq(8:10)='0'//tmpStr
      endif
      read(prod,'(i2)',err=210) iProd
      read(volEq(2:2),'(i1)',err=210) geog
      read(volEq(3:3),'(f1.0)',err=210) topDib
      read(volEq(8:10),'(i3)',err=211) spp

      goto 220
210   errFlg=1
      return
211   errFlg=6
      return
220   continue

C-----Check to determine if the heights and diameters are reasonable.
      if(dbhOb.le.0.0) then
        errFlg=3
      elseif(ht1Prd.lt.0.0) then
        errFlg=7
      elseif(ht2Prd.lt.0.0) then
        errFlg=8
      elseif(htTot.lt.0.0) then
        errFlg=10
c      elseif(ht2Prd.eq.0.0 .and. htTot.eq.0.0) then
      elseif(ht1Prd.le.0.01 .and. ht2Prd.le.0.01 
     &  .and. htTot.le.0.01 .and. upsHt1.le.0.01) then
        errFlg=10
c YW removed the following check to let small tree calc using short tree logic(02/12/2016)
c      elseif(htTot.gt.0.0.and.htTot.le.17.3) then
c        errFlg=10
      elseif(htTot.gt.0.0 .and. ht1Prd.gt.0.0 
     &  .and. htTot.lt.ht1Prd) then
        errFlg=7
      elseif(htTot.gt.0.0 .and. ht2Prd.gt.0.0 
     &  .and. htTot.lt.ht2Prd) then
        errFlg=8
      elseif(ht2Prd.gt.0.0 .and. ht1Prd.gt.0.0 
     &  .and. ht2Prd.lt.ht1Prd) then
        errFlg=7
      ELSEIF(prod.NE.'01'.AND.ht1Prd.GT.0.0.and. ht2Prd.le.0.01 
     &  .and. htTot.le.0.01 .and. upsHt1.le.0.01)THEN
        errFlg=7
c      elseif(htTot.gt.35.0*sqrt(dbhOb+3.0)) then
c        errFlg=5
c      elseif(ht2Prd.gt.35.0*sqrt(max(dbhOb,0.0))) then
c        errFlg=5
c      elseif(ht1Prd.gt.35.0*sqrt(max(dbhOb-4.0,0.0))) then
c        errFlg=5
c     check sawtimber reference height (upsHt1) and saw height ht1Prd
      elseif(upsHt1.gt.0 .and. ht1Prd.gt.0
     &  .and. upsHt1.lt.ht1Prd) then
        errFlg=10
      endif
      if(errFlg.ne.0) return

C-----Get species group to assign coefficients if there are none
C     specific to the particular species.
      sppGrp=0
      sppIdx=0
      k=1
      do 240, while(k.le.totSpp)
        if(spp.eq.coef0(k,1)) then
          sppIdx=k
          k=totSpp
        elseif(spp.lt.coef0(k,1)) then
          k=totSpp
        endif
        k=k+1
240   continue

      if(sppIdx.eq.0) then
        if(spp.lt.300) then
c         conifers
          sppGrp=1000
          if(spp.ge.90 .and. spp.le.99) then
c         spruces
              sppGrp=1090
          elseif(spp.ge.100 .and. spp.le.199) then
c         pines
              sppGrp=1100
          endif
        else
c         hardwoods
          sppGrp=1300
          if(spp.ge.310 .and. spp.le.329) then
c         maples
              sppGrp=1310
          elseif(spp.ge.370 .and. spp.le.379) then
c         birches
              sppGrp=1370
          elseif(spp.ge.400 .and. spp.le.410) then
c         hickories
              sppGrp=1400
          elseif(spp.ge.540 .and. spp.le.549) then
c         ashes
              sppGrp=1540
          elseif(spp.eq.740 .or. spp.eq.742 .or. spp.eq.744 
     &        .or. spp.eq.745 .or. spp.eq.753) then
c         cottonwoods
              sppGrp=1740
          elseif(spp.eq.741 .or. spp.eq.743 .or. spp.eq.746 
     &        .or. spp.eq.752) then
c         poplars
              sppGrp=1750
          elseif(spp.ge.760 .and. spp.le.769) then
c         cherries/plums
              sppGrp=1760
          elseif(spp.ge.800 .and. spp.le.899) then
c         oaks
              sppGrp=1800
          elseif(spp.ge.950 .and. spp.le.954) then
c         basswoods
              sppGrp=1950
          elseif(spp.ge.970 .and. spp.le.979) then
c         oaks
              sppGrp=1970
          endif
        endif
C       Get species group index to assign coefficients
        k=grpIdx
        do 250, while(k.le.totSpp)
          if(sppGrp.eq.coef0(k,1)) then
            sppIdx=k
            k=totSpp
          endif
          k=k+1
250     continue
      endif
      if(sppIdx.eq.0) then
        errFlg=1
        return
      endif
      
!-----Get merchantability rules
c      maxLen=8.0
c      minLen=4.0
c      trim=0.3
c      merchL=8.0
      if(mTopP.ne.0) then
        sawDib=mTopP
      else
        if(spp.lt.300) then
          sawDib=7.6
        else
          sawDib=9.6
        endif
      endif
      if(mTopS.ne.0) then
        plpDib=mTopS
      else
        plpDib=4.0
      endif
      if(stump.le.0.01) then
        if(iProd.eq.1) then
          stump=1.0
        else
          stump=0.5
        endif
      endif
      
      dbhIb = coefA(sppIdx,3)+coefA(sppIdx,4)*dbhOb
C-----Get top height and top DIB 10/2022 increase httot to 17.4 from 17.3 DW
      short=.false.
      if(htTot.gt.0.0) then
        topDib=0.0
        if(htTot.ge.17.4) then 
          topHt=htTot
        else
          short=.true.
          topHt=17.4
          shrtHt=htTot
        endif
      elseif(ht2Prd.gt.0) then
        topDib=4.0
        if(ht2Prd.ge.17.4) then
          topHt=ht2Prd
        else
            short=.true.
            topHt=17.4
            shrtHt=ht2Prd
        endif
      else
        if(spp.lt.300) then
          topDib=7.0
        else
          topDib=9.0
        endif
c       if sawtimber topHt is provided, no recalc topHt. 11/15/2011 (yw)
        if(upsHt1.gt.0) then
          if(upsHt1.ge.17.4) then
            topHt=upsHt1
          else
            short=.true.
            topHt=17.4
            shrtHt=upsHt1
c            shrtHt=ht1Prd
          endif
! For R9, HT1PRD is not same as UPSHT1. So comment out the foling line          
c          if(ht1prd.le.0) ht1Prd = upsHt1
        else        
          if(ht1Prd.ge.17.4) then
c         Use linear extrapolation from sawDib to topDib
              if((dbhOb-sawDib).GT.0) then
                topHt=4.5+(ht1prd-4.5)*(dbhOb-topDib)/(dbhOb-sawDib)
              else
                errFlg=13
                return
              endif
          else
              short=.true.
              topHt=17.4
              shrtHt=ht1Prd
          endif
        endif
      endif
      if(dbhOb.le.topDib) errFlg=11
      

c      if(errFlg.ne.0) return

C-----Get coefficients for inside-bark calculations.  All are for 
C     total height, except a17 and b17, which correspond to the top DIB.
      COEFFS%R=0.0
      COEFFS%C=0.0
      COEFFS%E=0.0
      COEFFS%P=0.0
      COEFFS%B=0.0
      COEFFS%A=0.0
      COEFFS%A4 = coefA(sppIdx,3)
      COEFFS%b4 = coefA(sppIdx,4)
      COEFFS%R  = coef0(sppIdx,4)
      COEFFS%C  = coef0(sppIdx,5)
      COEFFS%E  = coef0(sppIdx,6)
      COEFFS%P  = coef0(sppIdx,7)
      COEFFS%A  = coef0(sppIdx,8)
      COEFFS%B  = coef0(sppIdx,9)
      
      if(abs(topDib-0) .lt. 0.00001) then
        COEFFS%A17 = coef0(sppIdx,2)
        COEFFS%B17 = coef0(sppIdx,3)
      elseif(abs(topDib-4) .lt. 0.00001) then
        COEFFS%A17 = coef4(sppIdx,2)
        COEFFS%B17 = coef4(sppIdx,3)
      elseif((abs(topDib-7) .lt. 0.00001) 
     +  .or. (abs(topDib-9) .lt. 0.00001)) then
        COEFFS%A17 = coef79(sppIdx,2)
        COEFFS%B17 = coef79(sppIdx,3)
      else
        errFlg=1
        return
      endif
      
       CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,
     >           MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >           PROD)
      





      IF (DEBUG%MODEL) THEN
         WRITE (LUDBG, 100)'  DBHOB TOPDIB TOPHT HT1PRD HT2PRD HTTOT'
  100    FORMAT(A)
  			 WRITE (LUDBG, 120)DBHOB, TOPDIB, TOPHT, HT1PRD, HT2PRD, HTTOT
  120    FORMAT(2X, F5.1, F5.1, 2X, F5.1, 2X, F5.1, 2X, F5.1, 2X,F5.1)
         WRITE (LUDBG, 140)'  SPP MAXLEN MINLEN MERCHL MTOPP  MTOPS'
  140    FORMAT(A)
			   WRITE (LUDBG, 160)SPP, MAXLEN, MINLEN, MERCHL, MTOPP, MTOPS
  160    FORMAT(2X, I3, F5.1, 2X, F5.1, 2X, F5.1, 2X, F5.1, 2X,F5.1)
         WRITE  (LUDBG, 200) ' <--Exit R9PREP'
  200    FORMAT (A)   
   		END IF

      return
      end SUBROUTINE R9PREP
C_______________________________________________________________________
C
      subroutine r9dia417(COEFFS,topDib,dbhOb,topHt,ht1Prd,ht2Prd,
     &                    htTot,sawDib,plpDib,errFlg,upsHt1,volEq)
C_______________________________________________________________________
C
C  Calculates inside bark diameter at 4.5' (dbhIb), inside-bark diameter 
C  at 17.3' (dib17), given  species (spp), dbh (dbhOb), and height 
C  (topHt) to top diameter inside bark (topDib).  a17 and b17 are 
C  inside-bark coefficients corresponding to the specified top diameter.

			USE DEBUG_MOD
			USE CLKCOEF_MOD

      IMPLICIT NONE
!**********************************************************************
!...  Parameters    
      TYPE(CLKCOEF)::COEFFS
      integer   errFlg
      real      dbhOb,topHt,topDib,ht1Prd,ht2Prd,htTot,upsHt1
      real      sawDib,plpDib
      character*10 volEq
!...  Local variables
      INTEGER   I
      REAL      dbhIb,dib17,a4,b4,a17,b17
!======================================================================

			IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 10) ' -->Enter R9DIA417'
   10    FORMAT (A)   
   		END IF

      A4 =  COEFFS%A4
      B4 =  COEFFS%B4
      A17 = COEFFS%A17
      B17 = COEFFS%B17
      
      dbhIb=0.0
      dib17=0.0

C-----Calculate DIB at 4.5' from DBH (eqn 7)
      dbhIb=a4+b4*dbhOb
      if(dbhIb.ge.dbhOb.or. dbhIb.le.0.0) then
        dbhIb=max(dbhOb-0.1,0.1)
      endif
      
      IF (DEBUG%MODEL) THEN
         WRITE (LUDBG, 100)'  DBHOB TOPDIB DBHIB  ERRFLG'
  100    FORMAT(A)
         WRITE (LUDBG, 120)DBHOB, TOPDIB, DBHIB, ERRFLG
  120    FORMAT(2X, F5.1, F5.1, 2X, F5.1, 3X, I2)
         WRITE (LUDBG, 140)'  TOPHT HT1PRD HT2PRD HTTOT'
  140    FORMAT(A)
         WRITE (LUDBG, 160)TOPHT, HT1PRD, HT2PRD, HTTOT
  160    FORMAT(2X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1)
      END IF
      
      
      if((topDib.gt.dbhIb .and. topHt.gt.4.5)
     &  .or. (topDib.lt.dbhIb .and. topHt.lt.4.5)) then
        errFlg=11
        return
      endif
      
      

C-----Calculate DIB at 17.3' from top height and DBH (eqn 9)
      if(abs(ht2Prd-17.3) .lt. 0.00001) then
        dib17=plpDib
      elseif(abs(ht1Prd-17.3).lt.0.00001 .and. upsHt1.lt.0.01) then
        dib17=sawDib
      elseif(topHt.gt.17.3) then
        IF(volEq(1:1).EQ.'9')THEN
c       the regression for R9 is using dbhIb        
          dib17=dbhIb*(a17+b17*(17.3/topHt)**2)
        ELSE
c       the regression for R8 is using dbhOb
          dib17=dbhOb*(a17+b17*(17.3/topHt)**2)
        ENDIF
        dib17=max(dib17,topDib+0.1)
      else
        dib17=topDib-0.1
      endif
c     Make sure DIB  at 17.3 is large enough for product top diameters
      if(upsHt1.lt.0.01 .and. ht1prd.gt.17.3 .and. dib17.lt.sawDib) then
        dib17=sawDib+(dbhIb-sawDib)*(ht1prd-17.3)/(ht1prd-4.5)
      elseif(upsHt1.gt.17.3 .and. dib17.lt.sawDib) then
        dib17=sawDib+(dbhIb-sawDib)*(upsHt1-17.3)/(upsHt1-4.5)
      elseif(ht2prd.gt.17.3 .and. dib17.lt.plpDib) then
        dib17=plpDib+(dbhIb-plpDib)*(ht2prd-17.3)/(ht2prd-4.5)
      endif
      if(dib17.lt.0.1) dib17=0.1
      
      COEFFS%DBHIB = DBHIB
      COEFFS%DIB17 = DIB17
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 200) ' <--Exit R9DIA417'
  200    FORMAT (A)   
      END IF
      
      return
      end
C_______________________________________________________________________
C
      subroutine r9totHt(totHt,htTot,dbhIb,dib17,topHt,topDib,a,b,
     &                  errFlg)
C_______________________________________________________________________
C
C  Calculates total tree height from the specified inside-bark DBH 
C  (dbhIb), top height (topHt) and inside-bark top diameter (topDib). 
C  a and b are coefficients for inside-bark calculations.
      implicit none
      integer   errFlg
      real      totHt,htTot,dbhIb,dib17,topHt,topDib,a,b,Im,Qa,Qb,Qc

      totHt=0.0

      if(htTot.gt.0.0) then
        if(htTot.gt.17.4) then
          totHt=htTot
        else
c       short tree using the topHt (17.4) YW(02/12/2016)
          totHt = topHt
        endif
      elseif(topHt.gt.17.3) then
        if(topDib**2.gt.b*(a-1.0)**2*dib17**2) then
          Im=1.0
        else
          Im=0.0
        endif
        Qa=b+Im*(1.0-b)/a**2
        Qb=-2.0*b-Im*2.0*(1.0-b)/a
        Qc=b+(1.0-b)*Im-topDib**2/dib17**2
        totHt=17.3+(topHt-17.3)*(2.0*Qa)
     &        /(-1.0*Qb-(Qb**2-4.0*Qa*Qc)**0.5)
        totHt=max(totHt,topHt+topDib*2.0)
        totHt=min(totHt,topHt+topDib*8.0)
      else
c--     For short measured height, use a fixed taper
        totHt=17.3+dib17*3.0
      endif
      return
      end
C_______________________________________________________________________
C
      subroutine r9cuft(cfVol,COEFFS,lowrHt,
     &                  upprHt,errFlg)
C_______________________________________________________________________
C
C  Calculates cubic foot volume (cfVol) from the specified lower height 
C  or stump (lowrHt) to the specified upper height (upprHt), given 
C  inside-bark dbh (dbhIb), inside-bark diameter at 17.3' (dib17) and 
C  total height (totHt).  r, c, e, p, b, and a are the coefficients 
C  for inside-bark calculations.
      
      USE DEBUG_MOD
      USE CLKCOEF_MOD
      
      IMPLICIT NONE
!**********************************************************************      
!...  Parameters
      integer   errFlg,i,j
      real      lowrHt,upprHt,cfVol
      TYPE(CLKCOEF)::COEFFS
      real      G,W,X,Y,Z,T,L1,L2,L3,U1,U2,U3
      real      I1,I2,I3,I4,I5,I6
      real      vol(15)
      
!...  Local variables     
      real      r,c,e,p,b,a,totHt,dbhIb,dib17
      REAL V1,V2,V3
!======================================================================
      V1 = 0.0
      V2 = 0.0
      V3 = 0.0
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 10) ' -->Enter R9CUFT'
   10    FORMAT (A)   
   		END IF

!...  reassign the coefficients to local variables to keep things tidy   		
   		r = COEFFS%R
   		c = COEFFS%C
   		e = COEFFS%E
   		p = COEFFS%P
   		b = COEFFS%B
   		a = COEFFS%A
   		totHt = COEFFS%TOTHT
   		dbhIb = COEFFS%DBHIB
   		dib17 = COEFFS%DIB17

      cfVol=0.0
      if(upprHt.le.0.0) return

c-----Set combined variables
      G=(1.0-4.5/totht)**r
      W=(c+e/dbhib**3)/(1-G)
      X=(1.0-4.5/totht)**p
      !To avoid the Y is less and 2^-126 (YW 2023/06/12)
      IF((1.0-17.3/totht).LT.0.005748.AND.p.GT.14)THEN
          Y = 0
      ELSE
          Y=(1.0-17.3/totht)**p
      ENDIF
      Z=(dbhib**2-dib17**2)/(X-Y)
      T= dbhib**2 - Z * X
      L1=max(lowrHt,0.0)
      U1=min(upprHt,4.5)
      L2=max(lowrHt,4.5)
      U2=min(upprHt,17.3)
      L3=max(lowrHt,17.3)
      U3=min(totht,upprHt)

c-----Set indicator variables
      if(lowrHt.lt.4.5) then
        I1=1.0
      else
        I1=0.0
      endif
      if(lowrHt.lt.17.3) then
        I2=1.0
      else
        I2=0.0
      endif
      if(upprHt.gt.4.5) then
        I3=1.0
      else
        I3=0.0
      endif
      if(upprHt.gt.17.3) then
        I4=1.0
      else
        I4=0.0
      endif
      if((L3-17.3).lt. a *(totht - 17.3)) then
        I5=1.0
      else
        I5=0.0
      endif
      if((U3-17.3).lt. a * (totht - 17.3)) then
        I6=1.0
      else
        I6=0.0
      endif

C-----Calculate cubic volume between specified heights
C      cfVol=0.005454154*(I1 * dbhib**2 *((1-G*W)*(U1-L1)
C     &        +W*((1-L1/totht)**r * (totht - L1)
C     &        -(1-U1/totht)**r * (totht - U1))/(r+1))
C     &      +I2*I3*(T*(U2-L2)+Z*((1-L2/totHt)**p*(totHt-L2)
C     &        -(1-U2/totHt)**p*(totHt-U2))/(p+1))
C     &      +I4*dib17**2*(b*(U3-L3)-b*((U3-17.3)**2-(L3-17.3)**2)
C     &        /(totHt-17.3)+(b/3)*((U3-17.3)**3-(L3-17.3)**3)
C     &        /(totHt-17.3)**2
C     &      +I5*(1.0/3.0)*((1-b)/a**2)*(a*(totHt-17.3)-(L3-17.3))**3
C     &        /(totHt-17.3)**2
C     &      -I6*(1.0/3.0)*((1-b)/a**2)*(a*(totHt-17.3)-(U3-17.3))**3
C     &        /(totHt-17.3)**2))
      !The above calculation is seperated into three parts
      IF(I1.GT.0)THEN
        V1 = I1 * dbhib**2 *((1-G*W)*(U1-L1)
     &        +W*((1-L1/totht)**r * (totht - L1)
     &        -(1-U1/totht)**r * (totht - U1))/(r+1))
      ENDIF
      IF(I2.GT.0.AND.I3.GT.0) THEN
          !to avoid number (1-U2/totHt)**p off limit (YW 2023/06/12)
          IF((1-U2/totHt).LT.0.005748.AND.p.GT.14)THEN
              V2 = T*(U2-L2)+Z*((1-L2/totHt)**p*(totHt-L2))/(p+1)
          ELSE
              V2 = T*(U2-L2)+Z*((1-L2/totHt)**p*(totHt-L2)
     &        -(1-U2/totHt)**p*(totHt-U2))/(p+1)
          ENDIF
      ENDIF
      IF(I4.GT.0)THEN
          V3 = dib17**2*(b*(U3-L3)-b*((U3-17.3)**2-(L3-17.3)**2)
     &        /(totHt-17.3)+(b/3)*((U3-17.3)**3-(L3-17.3)**3)
     &        /(totHt-17.3)**2
     &      +I5*(1.0/3.0)*((1-b)/a**2)*(a*(totHt-17.3)-(L3-17.3))**3
     &        /(totHt-17.3)**2
     &      -I6*(1.0/3.0)*((1-b)/a**2)*(a*(totHt-17.3)-(U3-17.3))**3
     &        /(totHt-17.3)**2)   
      ENDIF    
      cfVol = 0.005454154*(V1+V2+V3)
      if(cfVol.lt.0.0) cfVol=0.0
      
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 400)'  CFVOL DBHIB TOTHT DIB17 LOWRHT UPPRHT'
  400    FORMAT (A)
  		 WRITE  (LUDBG, 420)CFVOL, DBHIB, TOTHT, DIB17, LOWRHT, UPPRHT
  420  FORMAT(1X, F5.1, 1X, F5.1, 1X, F5.1, 1X, F5.1, 1X, F5.1, 1X,F5.1)
     
         WRITE  (LUDBG, 2000) ' <--Exit R9CUFT'
 2000    FORMAT (A)   
   		END IF
   		
      return
      end SUBROUTINE R9CUFT
C_______________________________________________________________________
C
      subroutine r9dib(stmDib,stemHt,COEFFS)
C_______________________________________________________________________
C
C  Calculates inside-bark diameter (stmDib) at specified height (stemHt)
C  given inside-bark dbh (dbhIb), inside-bark diameter at 17.3' (dib17) 
C  and total height (totHt).  r, c, e, p, b, and a, are the coefficients 
C  for inside-bark calculations.

      USE CLKCOEF_MOD
      USE DEBUG_MOD
      
      implicit none
!**********************************************************************
!...  parameters
      
      real      stmDib,stemHt
      TYPE(CLKCOEF):: COEFFS
     
      
!...  Local variables
      integer   i
      real      r,c,e,p,b,a,totHt,dbhIb,dib17
      real      Is,Ib,It,Im,StTot
      REAL Ds,Db,Dt, Y
!======================================================================      

!...  reassign the coefficients to local variables to keep things tidy   		
   		r = COEFFS%R
   		c = COEFFS%C
   		e = COEFFS%E
   		p = COEFFS%P
   		b = COEFFS%B
   		a = COEFFS%A
   		totHt = COEFFS%TOTHT
   		dbhIb = COEFFS%DBHIB
   		dib17 = COEFFS%DIB17
   		
      stmDib=0.0

c--   Fix a potential problem when r coefficient is negative
      if(r .lt.0.0 .and. abs(stemHt - totHt).lt.0.00001) 
     &  stemHt=stemHt-0.1

c-----Set height indicator variables
      if(stemHt.lt.4.5) then
        Is=1.0
      else
        Is=0.0
      endif
      if(stemHt.ge.4.5 .and. stemHt.le.17.3) then
        Ib=1.0
      else
        Ib=0.0
      endif
      if(stemHt.gt.17.3) then
        It=1.0
      else
        It=0.0
      endif
      if(stemHt.lt.(17.3+ A *(TOTHT - 17.3))) then
        Im=1.0
      else
        Im=0.0
      endif
!...
!...  trap potential numerical error when the term (1-stemHt/totHt)**r
!...  gets too small
!...
!...  TDH testing the addition of ABS ro R since negative R's mess 
!...  things up
      StTot=stemHt/TOTHT
      IF(LOG(1.-StTot).LT.(-20./ABS(R)))StTot=1.

C The calculation needs to be done seperately. Otherwise it may cause calculation error 
C Replaced this piece code with the code below. (YW 2021/02/26)      
C-----Get DIB at specified height
!      stmDib=(Is*(DBHIB**2 * (1 + (C + E/DBHIB**3)*
!     &   ((1-StTot)**R - (1-4.5/TOTHT)**R)/
!     &   (1-(1-4.5/TOTHT)**R)))
!     &   +Ib*(DBHIB**2-(DBHIB**2-DIB17**2)*
!     &   ((1-4.5/TOTHT)**P
!     &     -(1-stemHt/TOTHT)**P)/((1-4.5/TOTHT)
!     &   **P-(1-17.3/TOTHT)**P))
!     &   +It*(DIB17**2*(B*(((stemHt-17.3)/
!     &   (TOTHT-17.3))-1)**2
!     &   +Im*((1-B)/A**2)*(A-(stemHt-17.3)/
!     &   (TOTHT-17.3))**2)))**0.5
      
      Ds = 0.0
      Db = 0.0
      Dt = 0.0

      IF((1.0-17.3/TOTHT).LT.0.005748.AND.P.GT.14)THEN
        Y = 0
      ELSE
        Y=(1.0-17.3/TOTHT)**P
      ENDIF

      IF(Is.EQ.1.0)THEN
        Ds = (DBHIB**2 * (1 + (C + E/DBHIB**3)*
     &   ((1-StTot)**R - (1-4.5/TOTHT)**R)/
     &   (1-(1-4.5/TOTHT)**R)))
      ENDIF
      IF(Ib.EQ.1.0)THEN
        Db = (DBHIB**2-(DBHIB**2-DIB17**2)*
     &   ((1-4.5/TOTHT)**P
     &     -(1-stemHt/TOTHT)**P)/((1-4.5/TOTHT)
     &   **P-Y))
      ENDIF
      IF(It.EQ.1.0)THEN
        Dt = (DIB17**2*(B*(((stemHt-17.3)/
     &   (TOTHT-17.3))-1)**2
     &   +Im*((1-B)/A**2)*(A-(stemHt-17.3)/
     &   (TOTHT-17.3))**2))
      ENDIF
      StmDib = (Ds+Db+Dt)**0.5
      
      if(stmDib.lt.0.0) stmDib=0.0
      
!      IF (DEBUG%MODEL) THEN
!         WRITE  (LUDBG, 400)'  r   c    e    p    b    a '
!  400    FORMAT (A)
!  		 WRITE  (LUDBG, 420)r, c, e,p,b,a
!  420  FORMAT(6F7.2)
  
!         WRITE  (LUDBG, 500)'  totht stmdDib dbhib dib17  stemht'
!  500    FORMAT (A)
!  		 WRITE (LUDBG, 520)totht,stmdib, dbhib,dib17,stemht
!  520  FORMAT(6F6.1)
!      endif
      
      return
      end
C_______________________________________________________________________
C
      subroutine r9ht(stemHt,COEFFS,stmDib,errFlg)
C_______________________________________________________________________
C
C  Calculates height (stemHt) at which the specified inside-bark 
C  diameter (stmDib) occurs, given inside-bark dbh (dbhIb), inside-bark 
C  diameter at 17.3' (dib17) and total height (totHt).  r, c, e, p, b,
C  and a are the coefficients for inside-bark calculations.

CDate 5/24/2011
CREV  TDH added comments and an ISNAN check to final stemHt
      USE CLKCOEF_MOD

      IMPLICIT NONE
      
!...  Parameters     
      integer   errFlg,i
      real      stemHt,stemHt1,stemHt2,stemHt3
      TYPE(CLKCOEF)::COEFFS
      REAL      stmDib
      
!... Local variables
      logical  res
      REAL      totHt,dbhIb,dib17,xxx
      real      r,c,e,p,b,a,G,W,X,Y,Z,Qa,Qb,Qc,Is,Ib,It,Im
!======================================================================

      totHt = COEFFS%TOTHT
      dbhIb = COEFFS%DBHIB
      dib17 = COEFFS%DIB17
      r = COEFFS%R
      c = COEFFS%C
      e = COEFFS%E
      p = COEFFS%P
      b = COEFFS%B 
      a = COEFFS%A

      stemHt=0.0

c-----Set combined variables
      G=(1.0-4.5/totHt)**r
      W=(c+e/dbhIb**3)/(1-G)
      X=(1.0-4.5/totHt)**p
      IF((1.0-17.3/totht).LT.0.005748.AND.p.GT.14)THEN
        Y = 0
      ELSE
        Y=(1.0-17.3/totht)**p
      ENDIF
      Z=(dbhIb**2-dib17**2)/(X-Y)

c-----Set height indicator variables
      if(stmDib.ge.dbhIb) then
      !not sure why we need to get the height if it is less than
      !4.5'(dbh) since there would be no pulp or saw volume anyway
        Is=1.0
      else
        Is=0.0
      endif
      if(stmDib.lt.dbhIb .and. stmDib.ge.dib17) then
        Ib=1.0
      else
        Ib=0.0
      endif
      if(stmDib.lt.dib17) then
        It=1.0
      else
        It=0.0
      endif
      if(stmDib**2.gt.b*(a-1)**2*dib17**2) then
        Im=1.0
      else
        Im=0.0
      endif
      Qa=b+Im*(1-b)/a**2
      Qb=-2.0*b-Im*2.0*(1-b)/a
      Qc=b+(1.0-b)*Im-stmDib**2/dib17**2

C-----Get height to specified DIB
C     Eqn 2: split since terms are undefined when evaluated together.
cc this section replaced by NCrookston, Sept 2017 to deal with degenerate 
c  math when trying to exponentiate negative numbers with real-valued powers.
c  *** note that an original version included the use of testing the results 
c  *** for NaN, but that idea isn't valid because it happens after the math
c  *** exception causeing an abnormal termination on some systems.
c      if(Is.eq.1)  then
c      !this equation causes illegal math for many species so we
c      !catch this at the bottom of this subroutine
c        stemHt=totHt*(1-((stmDib**2/dbhIb**2-1)/W+G)**(1/r))
c      elseif(Ib.eq.1) then
c        stemHt=totHt*(1-(X-(dbhIb**2-stmDib**2)/Z)**(1/p))
c      else
c        stemHt=17.3+(totHt-17.3)*((-Qb-(Qb**2-4*Qa*Qc)**0.5)/(2*Qa))
c      endif     
cc end of deleted code, start of replacement

      stemHt = 0.
      if(Is.eq.1)  then
        xxx = (stmDib**2/dbhIb**2-1)/W+G
        if (xxx .gt. 0.) then 
          stemHt=totHt*(1.-xxx**(1/r))
        endif
      elseif(Ib.eq.1) then
        xxx = X-(dbhIb**2-stmDib**2)/Z
        if (xxx .gt. 0.) then
          stemHt=totHt*(1-xxx**(1/p))
        endif
      else
        xxx = Qb**2-4*Qa*Qc
        if (xxx .gt. 0.) then
          stemHt=17.3+(totHt-17.3)*((-Qb-xxx**0.5)/(2*Qa))
        endif
      endif
      !added the ISNAN check as per the above comments
      ! Lahey doesn't provide the ISNAN intrinsic function, so
      ! we use the subroutine IISNAN (RNH)
c      call IISNAN(stemHt,res)
c      if((stemHt.lt.0.0).OR. res ) stemHt=0.0
C     The following line check if stemHt is a NaN 
c      if(stemHt.ne.stemHt) stemHt=0.0
      return
      end
C
!     Comment out the following subroutine. It is not used but caused compile error on some system (Mac)
!     YW 2021/06/21
!      subroutine iisnan(x,res)
!      real, intent(in) :: x
!      logical :: res
!      integer, parameter :: NaN = Z"7FC00000"
!      res = ieor(transfer(x,Nan), NaN) == 0
!      end subroutine

      subroutine r9bdft(vol,logLen,NUMSEG,logDia,errFlg,logVol)
C_______________________________________________________________________
C
C  Calculates board foot volumes (in the vol array) from the specified 
C  stump (stump) to the specified sawtimber height (sawHt), given 
C  inside-bark dbh (dbhIb), inside-bark diameter at 17.3' (dib17) and 
C  total height (totHt).  Intermediate calculations fill arrays for 
C  log lengths (logLen), log end diameters (logDia), and height to the 
C  log ends (bolHt).  Log specifications are maximum length (maxLen), 
C  minimum length (minLen), trim length (trim), minimum sawtimber DBH 
C  (minBfD), and default log length (merchL).  r, c, e, p, b, and a are 
C  the coefficients for inside-bark calculations.
      
      USE CLKCOEF_MOD
      USE DEBUG_MOD
      
      IMPLICIT NONE

!REV  Created MVD ??? 
!REV  Revised TDH 12/23/09

!REV  removed many unused variables

!**********************************************************************
!...  Parameters
      REAL      VOL(15),LOGLEN(20)
      INTEGER   NUMSEG
      REAL      LOGDIA(21,3) 
      INTEGER   ERRFLG
      
      
!..   Local variables
      INTEGER   iDib,i
      real      logV,dib
      real      bdft,len
      REAL      logVol(7,20)
      real      scrbnr(120)

      data (scrbnr(i),i=1,60) /
     &  0.000,0.143,0.390,0.676,1.070,1.160,
     &  1.400,1.501,2.084,3.126,3.749,4.900,
     &  6.043,7.140,8.880,10.000,11.528,13.290,
     &  14.990,17.499,18.990,20.880,23.510,25.218,
     &  28.677,31.249,34.220,36.376,38.040,41.060,
     &  44.376,45.975,48.990,50.000,54.688,57.660,
     &  64.319,66.730,70.000,75.240,79.480,83.910,
     &  87.190,92.501,94.990,99.075,103.501,107.970,
     &  112.292,116.990,121.650,126.525,131.510,136.510,
     &  141.610,146.912,152.210,157.710,163.288,168.990/
      data (scrbnr(i),i=61,120) /
     &  174.850,180.749,186.623,193.170,199.120,205.685,
     &  211.810,218.501,225.685,232.499,239.317,246.615,
     &  254.040,261.525,269.040,276.630,284.260,292.501,
     &  300.655,308.970,317.360,325.790,334.217,343.290,
     &  350.785,359.120,368.380,376.610,385.135,393.380,
     &  402.499,410.834,419.166,428.380,437.499,446.565,
     &  455.010,464.150,473.430,482.490,491.700,501.700,
     &  511.700,521.700,531.700,541.700,552.499,562.501,
     &  573.350,583.350,594.150,604.170,615.010,625.890,
     &  636.660,648.380,660.000,671.700,683.330,695.011/

!======================================================================

      vol(2)=0.0
      vol(10)=0.0
 
!...  need to zero out bd ft log volumes not cubic foot
      DO 150 I=1,NUMSEG
         logVol(1,I)=0.0
150   CONTINUE         

C-----Get board foot volumes
       IF(NUMSEG .GT. 0)THEN
        do 870 i=1,20   !numSeg
          len=logLen(i)
         IF(len.GT.0.0)THEN
c--       Scribner volume
          dib=logDia(i+1,1)

          IF (DEBUG%MODEL) THEN
           WRITE  (LUDBG, 850) ' R9BDFT LINE 100 ', numseg
850        FORMAT (A, 2x, i2)
          ENDIF

          if(dib.ge.1.0) then
            iDib=int(dib)
            logVol(1,i)=nint(len*scrbnr(iDib))
          else
            logVol(1,i)=0.0
          endif
          IF(i.LE.numSeg)THEN
            vol(2)=vol(2) + logVol(1,i)
          ELSE
            vol(12)=vol(12) + logVol(1,i)
          ENDIF
          IF (DEBUG%MODEL) THEN
           WRITE  (LUDBG, 860) ' R9BDFT', vol(2), ' ', logVol(1,i), dib
860        FORMAT (A, 2x,F6.1,A,F6.1, F6.1)
          ENDIF

c--       International 1/4 volume
          if(dib.ge.4.0) then
            bdft=0.04976191*len*dib**2 +0.006220239*len**2*dib
     &          -0.1854762*len*dib +0.0002591767*len**3
     &          +0.01159226*len**2 +0.04222222*len

          else
            bdft=0.0
          endif
          logVol(7,i)=nint(bdft/5.0)*5.0
          IF(i.LE.numSeg) vol(10)=vol(10) + logVol(7,i)
          
         ELSE   ! IF len =0, Then exit loop
            EXIT
         ENDIF
          
870     continue

        vol(2)=nint(vol(2))
        vol(10)=nint(vol(10))
        if (vol(10).lt.0.0) vol(10)=0.0
        if (vol(2).lt.0.0) vol(2)=0.0
      endif
      return
      end
C_______________________________________________________________________
C
      subroutine r9cor(vol,logVol,spp,iProd)
C_______________________________________________________________________
C
C  Correction factors to account for proper merchandising and
C  to bring volumes in line with mill studies and legacy system.

!REV  Revised MGV 12/10/10
!REV  updated correction factors

      implicit none
      integer   spp,iProd,i
      real      cf1,cf2,cf3,cf4,vol(15),logVol(7,20)
 
!      if(spp.lt.300) then
!        cf1=1.05
!        cf2=1.05
!        cf3=1.05
!        cf4=1.05
!      elseif(spp.ge.741 .and. spp.le.746) then
!        cf1=1.0
!        cf2=1.0
!        cf3=1.0
!        cf4=1.0
!      else
!        cf1=1.1
!        cf2=1.1
!        cf3=1.1
!        cf4=1.1
!      endif
      
       if(spp.lt.300) then
        cf1=1.04
        cf2=1.04
        cf3=1.04
        cf4=1.04
      elseif((spp.ge.741.and.spp.le.746) .or. spp.eq.621) then
C Added species 621 relow-poplar (YW 07/13/2018)      
        cf1=1.0
        cf2=1.0
        cf3=1.0
        cf4=1.0
      else
        cf1=1.1
        cf2=1.1
        cf3=1.1
        cf4=1.1
      endif
      
      
      vol(1)=vol(1)*cf2
      vol(2)=vol(2)*cf3
      vol(7)=vol(7)*cf2
      vol(9)=vol(9)*cf2
      vol(10)=vol(10)*cf4
      if(iProd.eq.1) then
        vol(4)=vol(4)*cf1
        vol(6)=vol(6)*cf1
      else
        vol(4)=vol(4)*cf2
        vol(6)=vol(6)*cf2
      endif
      do 920, i=1,20
        logVol(1,i)=logVol(1,i)*cf4
        logVol(7,i)=logVol(7,i)*cf3
920   continue

      return
      end

C_______________________________________________________________________
C
C  VARIABLE DEFINITIONS
C_______________________________________________________________________
C
C       GLOBAL
C
C  r,c,e,p,a,b real       Clark coefficients for total height model
C  a4,b4       real       Clark coefficients for DIB at DBH
C  a17,b17     real       Clark coefficients for DIB at 17.3'
C  COEFFS      CLKCOEF    Type(structure) for passing coefficients
C  bfpFlg      integer    flag for sawtimber board foot volume
C  bolHt       real(21)   array of heights of sawlog ends
C  cdpFlg      integer    flag for sawtimber cord volume
C  cfVol       real       calculated cubic foot volume
C  cupFlg      integer    flag for swtimber cubic foot volume
C  cutFlg      integer    flag for total cubic foot volume
C  dbhIb       real       diameter at breast height inside bark
C  dbhOb       real       diameter at breast height outside bark
C  dib17       real       diameter inside bark at a height of 17.3'
C  errFlg      integer    error flag code
C  forst       char*2     forest number
C  geog        integer    geographic area code
C  ht1Prd      real       sawtimber height
C  ht2Prd      real       pulp or topwood height
C  htTot       real       total height to tree tip
C  iProd       integer    numeric product code
C  logDia      real(7,21) array of end diameters for sawlogs
C  logLen      real(20)   array of log lengths for sawtimber
C  logVol      real(7,20) array of volumes for sawlogs
C  maxLen      real       maximum allowable length of a log
C  merchL      real       length of standard logs
C  minBfD      real       minimum board foot diameter
C  minLen      real       minimum allowable length of a log
C  mTopP       real       merchantable top diameter for sawtimber
C  mTopS       real       merchantable top diameter for pulp and topwood
C  numSeg      integer    number of sawlogs in the tree
C  plpDib      real       top DIB for pulp or topwood volume calculation
C  plpHt       real       height for pulp or topwood volume calculation
C  prod        char*2     product code
C  sawDib      real       top DIB for sawtimber volume calculation
C  sawHt       real       height for sawtimber volume calculation
C  short       logical    flag to indicate a short tree
C  shrtHt      real       height used for calculations when tree is short
C  spFlg       integer    flag for topwood volumes
C  spp         integer    species code
C  stump       real       stump height
C  tcfVol      real       calculated cubic foot volume for all products
C  topDib      real       top DIB to which ht2Prd was measured
C  topHt       real       uppermost height measured
C  totHt       real       total tree height for volume calculations
C  trim        real       trim allowance between logs
C  vol         real(15)   array of volumes
C  volEq       char*10    volume equation string
C_______________________________________________________________________
C
C       R9PREP
C  
C  lowrHt      real       lower height for a volume calculation
C  match       logical    flag to indicate a species code match
C  sppGrp      integer    species group code for assigning coefficients
C  upprHt      real       upper height for a volume calculation
C_______________________________________________________________________
C
C       R9TOTHT
C  
C  Im          real       stem location flag for Clark equations
C  Qa,Qb,Qc    real       intermediate calculations for Clark equation
C_______________________________________________________________________
C
C       R9CUFT
C  
C  I1-I6       real       indicator used in Clark volume equation
C  G,W,X,Y,Z,T real       intermediate calculations for Clark equation
C  L1-L3,U1-U3 real       intermediate calculations for Clark equation
C_______________________________________________________________________
C
C       R9DIB
C  
C  Is,Ib,It,Im real       stem location flag for Clark equations
C_______________________________________________________________________
C
C       R9HT
C  
C  Is,Ib,It,Im real       stem location flag for Clark equations
C  G,W,X,Y,Z   real       intermediate calculations for Clark equation
C  Qa,Qb,Qc    real       intermediate calculations for Clark equation
C_______________________________________________________________________
C
C       R9BDFT
C  
C  bdft        real       board foot volume for a log
C  dib         real       DIB at the top end of the current log
C  ht          real       height to the top of the current log
C  iDib        integer    1" diameter class for Scribner calculations
C  leftov      real       length left over above full-length logs
C  len         real       length of a log
C  logV        real       volume calculated for a log
C  scrbnr      real(120)  array of factors used in Scribner calculations
C
C_______________________________________________________________________
C

