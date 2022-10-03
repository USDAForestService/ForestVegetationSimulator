C  Region 9 Profile Model Volume Calculation
C
C  created TDH 01/13/2011  
C
C  revised *** **/**/****  
C
C  This subroutine is parted from the r9clark.f file and is used for
C  calculating DIB at a user entered height (DIBHT).
C_______________________________________________________________________
C
      subroutine r9clarkdib (volEq,stump,mTopP,mTopS,dbhOb,
     &                    ht1Prd,ht2Prd,htTot,DIBHT,dib,prod,errFlg,
     &                    upsHt1)
C_______________________________________________________________________
C
      !DEC$ ATTRIBUTES DLLEXPORT::R9CLARKDIB
 
      USE DEBUG_MOD
      USE CLKCOEF_MOD
      
      implicit none
      
C     Shared variables
      character forst*2,prod*2,volEq*10
      integer   geog,iProd,cutFlg,bfpFlg,cupFlg,spFlg,cdpFlg
      integer   errFlg
      real      minBfD,maxLen,minLen,merchL,mTopP,mTopS,stump,trim
      real      dbhOb,ht1Prd,ht2Prd,htTot,topDib,upsHt1 
      real      vol(15),logLen(20),logVol(7,20),logDia(21,3)
      real      DIBHT, dib, brokHt
C     Internal variables
      integer   numSeg,spp,i,j
      real      plpDib,sawDib,plpHt,sawHt,topHt,totHt,dbhIb,dib17
      real      r,c,e,p,b,a,a4,b4,a17,b17,cfVol,tcfVol,shrtHt
      real      cf1,cf2,cf3
      logical   short
      
      TYPE(CLKCOEF):: COEFFS
      TYPE(CLKCOEF):: COEFFSO

C     Initialize output variables
      numSeg=0
      dib=0
      errFlg=0
      
C-----Check input values and prepare variables
      IF(VOLEQ(1:1).EQ.'8')THEN
      call r8Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &            spp,geog,COEFFS,forst,maxLen,
     &            minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &            prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,
     &            upsHt1,COEFFSO)
       call r9totHt(COEFFS%totHt,htTot,dbhOb,COEFFSO%dib17,topHt,
     &             topDib,COEFFSO%a, COEFFSO%b,errFlg)
       COEFFSO%totHt = COEFFS%totHt

      ELSEIF(VOLEQ(1:1).EQ.'9')THEN
      call r9Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &            spp,geog,COEFFS,forst,maxLen,
     &            minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &            prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,
     &            upsHt1)
C-----Get DIBs at heights of 4.5' and 17.3'
      call r9dia417(COEFFS,topDib,dbhOb,topHt,ht1Prd,ht2Prd,
     &              htTot,sawDib,plpDib,errFlg,upsHt1,volEq)
      if(errFlg.ne.0) return
C-----Get total height
      call r9totHt(COEFFS%totHt,htTot,COEFFS%dbhIb,COEFFS%dib17,topHt,
     &             topDib,COEFFS%a, COEFFS%b,errFlg)
      ELSE
        errFlg = 1
      ENDIF
      if(errFlg.ne.0) return


      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 200)'  DBHIB DIB17 SAWDIB PLPDIB'
  200    FORMAT (A)
  			 WRITE  (LUDBG, 220)coeffs%DBHIB,coeffs%DIB17, SAWDIB, PLPDIB
  220    FORMAT(1X, F5.1, 2X, F5.1, F5.1, 2X, F5.1)
      END IF

      if(COEFFS%totHt.le.17.3) errFlg=8
      if(errFlg.ne.0) return
      
      call r9dib(DIB,DIBHT,COEFFS)
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 300)'  TOTHT HTTOT TOPHT TOPDIB STUMP'
  300    FORMAT (A)
  			 WRITE  (LUDBG, 320)coeffs%TOTHT, HTTOT, TOPHT, TOPDIB, STUMP
  320    FORMAT(1X, F5.1, 2X, F5.1, 1X, F5.1, 1X, F5.1, 1X, F5.1)
      END IF
      
      return
      
      end