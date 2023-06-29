C_______________________________________________________________________
C
      subroutine r8Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &                  spp,geog,COEFFS,forst,maxLen,
     &                  minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &                  prod,iProd,sawDib,plpDib,short,shrtHt,errFlg,
     &                  upsHt1,COEFFSO)
C_______________________________________________________________________
C
C  Parse out the info in the volume equation (volEq).  Check to see if 
C  the specified measurements are reasonable.  Get coefficients and 
C  merchantability rules for the specified species and product.
!  2023/06/09 YW Set DIB17 = 0.1 when DIB17 is <= 0 to avoid divided by 0 error      
      
      USE DEBUG_MOD
      USE CLKCOEF_MOD
      
      implicit none
      INCLUDE 'r8clkcoef.inc'      !'R8CLKCOEF.INC'
      INCLUDE 'r8dib.inc'          !'R8dib.inc'
      INCLUDE 'r8cfo.inc'
      
      integer   errFlg,spp,sppGrp,geog,iProd,k,sppIdx
      real      dbhOb,topDib,topHt,sawHt,maxLen,minLen
      real      dib17,upprHt,lowrHt,merchL,stump,upsHt1
      real      mTopP,mTopS,trim,minBfD,ht1Prd,ht2Prd,htTot
      TYPE(CLKCOEF):: COEFFS
      TYPE(CLKCOEF):: COEFFSO
      real      plpDib,sawDib,shrtHt,dbhIb
      character volEq*10,forst*2,prod*2,tmpStr*2
      logical   short
      INTEGER   TOTSPP /49/
      INTEGER GEOA,GSPEC,DONEFLAG,LASTFLAG,FIRST,LAST,HALF,CHECK,PTR
      INTEGER SPGRP,REGN,SPEC
      REAL FIXDI4,FIXDI79,BTR, DBTBH,MINLENT
      CHARACTER*1 COR
      INTEGER OPT,EVOD
      REAL FCLSS,THT,FCDIB,FCMIN,DBH,HT2

C  DW 08/22 initializations of variables otherwise uninitialized prior to MRULES call
      OPT = 0
      EVOD = 0
      MINLENT = 0
      BTR = 0
      DBTBH = 0

      if(volEq(10:10).lt.'0' .or. volEq(10:10).gt.'9') then
        tmpStr=volEq(8:9)
        volEq(8:10)='0'//tmpStr
      endif
      read(prod,'(i2)',err=210) iProd
      READ(VOLEQ(8:10),'(I3)',err=211)SPEC
      READ(VOLEQ(2:2),'(I1)',err=211)GEOA
      READ(VOLEQ(1:1),'(I1)',err=211)REGN
	spp = SPEC
      IF (GEOA.LT.1 .OR. GEOA.GT.9 .OR. GEOA.EQ.8)THEN
         ERRFLG = 1
         GO TO 999
      ENDIF

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
      elseif(ht1Prd.le.0.01 .and. ht2Prd.le.0.01 
     &  .and. htTot.le.0.01 .and. upsHt1.le.0.01) then
        errFlg=10
      elseif(htTot.gt.0.0 .and. ht1Prd.gt.0.0 
     &  .and. htTot.lt.ht1Prd) then
        errFlg=7
      elseif(htTot.gt.0.0 .and. ht2Prd.gt.0.0 
     &  .and. htTot.lt.ht2Prd) then
        errFlg=8
      elseif(ht2Prd.gt.0.0 .and. ht1Prd.gt.0.0 
     &  .and. ht2Prd.lt.ht1Prd) then
        errFlg=7
      ELSEIF(prod.NE.'01'.AND.ht1Prd.GT.0.0)THEN
c        errFlg=7
c       R8 uses ht1Prd pulpwood broken HT
        ht2Prd = ht1Prd
      elseif(upsHt1.gt.0 .and. ht1Prd.gt.0
     &  .and. upsHt1.lt.ht1Prd) then
        errFlg=10
      endif
      if(errFlg.ne.0) return

      IF (SPEC.EQ.123 .OR. SPEC.EQ.197) THEN
         SPEC = 100
      ELSEIF  (SPEC.EQ.268) THEN       
         SPEC = 261                     
      ELSEIF  (SPEC.EQ.313 .OR. SPEC.EQ.314 .OR. SPEC.EQ.317 .OR.
     >          SPEC.EQ.650 .OR. SPEC.EQ.651. OR.
     >          SPEC.EQ.691 .OR. SPEC.EQ.711 .OR. SPEC.EQ.742 .OR.
     >          SPEC.EQ.762 .OR. SPEC.EQ.920 .OR. SPEC.EQ.930 .OR.
     >          SPEC.EQ.545 .OR. SPEC.EQ.546) THEN
         SPEC = 300
      ELSEIF  (SPEC.EQ.521 .OR. SPEC.EQ.550 .OR. SPEC.EQ.580 .OR.
     >          SPEC.EQ.601 .OR. SPEC.EQ.602 .OR. SPEC.EQ.318) THEN
         SPEC = 500
      ELSEIF  (SPEC.EQ.804 .OR. SPEC.EQ.817 .OR. SPEC.EQ.820 .OR.
     >          SPEC.EQ.823 .OR. SPEC.EQ.825. OR.
     >          SPEC.EQ.826 .OR. SPEC.EQ.830 .OR. SPEC.EQ.834) THEN
         SPEC = 800
      ENDIF

      GSPEC = GEOA*1000 + SPEC

C-----Get species group to assign coefficients if there are none
C     specific to the particular species.
      sppGrp=0
      sppIdx=0
      k=1
      do 240, while(k.le.totSpp)
        if(spec.eq.DIBMEN(k,1)) then
          sppIdx=k
          k=totSpp
        elseif(spec.lt.DIBMEN(k,1)) then
          k=totSpp
        endif
        k=k+1
240   continue
      IF(sppIdx.eq.0)THEN
        errflg = 1
        return
      ELSE
        FIXDI4 = DIBMEN(sppIdx,2)
        FIXDI79 = DIBMEN(sppIdx,3)
      ENDIF
      
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 182
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
	!DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST   

          CHECK=R8CF(HALF,1)*1000+R8CF(HALF,2)
	!FOUND THE COEFFECIENTS
          IF(GSPEC.EQ.CHECK)THEN      
             PTR = HALF
             DONEFLAG=1
	!MOVE DOWN THE LIST
          ELSEIF(GSPEC.GT.CHECK)THEN  
             FIRST = HALF
	!MOVE UP THE LIST
          ELSEIF(GSPEC.LT.CHECK)THEN   
             LAST = HALF - 1
          ENDIF
	!DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN   
	!END ROUTINE, NO MATCH
	       IF(GEOA.EQ.9) THEN   
c                SPECPR = 0
c                EQNPR = 0
c                GEOAPR = 0
                ERRFLG = 6
                GO TO 999
	!SET GEOAPR TO 9 AND RETRY THE SEARCH
	       ELSE                  
                GEOA = 9
                GSPEC = GEOA*1000 + SPEC
                !FIRST = 1
                !geocode 9 start at 135
                FIRST = 135
                LAST = 182
                LASTFLAG = 0
             ENDIF
          ENDIF     
   5  CONTINUE

C     END BINARY SEARCH

      SPGRP = R8CF(PTR,3) + .5                 
      IF (SPGRP.NE.100 .AND. SPGRP.NE.300 .AND. SPGRP.NE.500)THEN
        ERRFLG = 6
        GO TO 999
      ENDIF
      dbhIb = R8CF(PTR,4)+R8CF(PTR,5)*dbhOb

C-----Get coefficients for inside-bark calculations.  All are for 
C     total height, except a17 and b17, which correspond to the top DIB.
        COEFFS%R=0.0
        COEFFS%C=0.0
        COEFFS%E=0.0
        COEFFS%P=0.0
        COEFFS%B=0.0
        COEFFS%A=0.0
        COEFFS%A4=0.0
        COEFFS%B4=0.0
        COEFFS%A17=0.0
        COEFFS%B17=0.0
        COEFFS%DBHIB=0.0
        COEFFS%DIB17=0.0
        COEFFS%TOTHT=0.0

        COEFFS%R  = TOTAL(sppIdx,2)
        COEFFS%C  = TOTAL(sppIdx,3)
        COEFFS%E  = TOTAL(sppIdx,4)
        COEFFS%P  = TOTAL(sppIdx,5)
        COEFFS%B = TOTAL(sppIdx,6)
        COEFFS%A = TOTAL(sppIdx,7)
        COEFFS%A4 = R8CF(PTR,4)
        COEFFS%b4 = R8CF(PTR,5)
        COEFFS%FIXDI = FIXDI4
        COEFFS%SPGRP = SPGRP
        
C-----Get coefficients for outside-bark calculations. All are for 
C     total height
        COEFFSO%R=0.0
        COEFFSO%C=0.0
        COEFFSO%E=0.0
        COEFFSO%P=0.0
        COEFFSO%B=0.0
        COEFFSO%A=0.0
        COEFFSO%A4=0.0
        COEFFSO%B4=0.0
        COEFFSO%A17=0.0
        COEFFSO%B17=0.0
        COEFFSO%DBHIB=0.0
        COEFFSO%DIB17=0.0
        COEFFSO%TOTHT=0.0

        COEFFSO%R  = OTOTAL(sppIdx,2)
        COEFFSO%C  = OTOTAL(sppIdx,3)
        COEFFSO%E  = OTOTAL(sppIdx,4)
        COEFFSO%P  = OTOTAL(sppIdx,5)
        COEFFSO%B = OTOTAL(sppIdx,6)
        COEFFSO%A = OTOTAL(sppIdx,7)

666   IF(htTot.gt.0.0) then
        COEFFS%A17 = R8CF(PTR,14)
        COEFFS%B17 = R8CF(PTR,15)
        COEFFS%AFI = R8CF(PTR,6)
        COEFFS%BFI = R8CF(PTR,7)   
        COEFFSO%A17 = R8CFO(PTR,4)
        COEFFSO%B17 = R8CFO(PTR,5) 
      ELSEIF(upsHt1.GT.0.0)THEN
        IF(PROD.EQ.'01')THEN
c       upsHT1 is the height to 7/9 top
          IF(SPEC.LT.300)THEN
            COEFFS%A17 = R8CF(PTR,10)
            COEFFS%B17 = R8CF(PTR,11)   
          ELSE
            COEFFS%A17 = R8CF(PTR,12)
            COEFFS%B17 = R8CF(PTR,13)           
          ENDIF   
          COEFFSO%A17 = R8CFO(PTR,8)
          COEFFSO%B17 = R8CFO(PTR,9)
        ELSE
c       upsHT1 is the height to 4" top 
          COEFFS%A17 = R8CF(PTR,8)
          COEFFS%B17 = R8CF(PTR,9) 
          COEFFSO%A17 = R8CFO(PTR,6)
          COEFFSO%B17 = R8CFO(PTR,7)            
        ENDIF
      elseif(ht2Prd.gt.0) then
        COEFFS%A17 = R8CF(PTR,8)
        COEFFS%B17 = R8CF(PTR,9) 
        COEFFSO%A17 = R8CFO(PTR,6)
        COEFFSO%B17 = R8CFO(PTR,7)     
      elseif(ht1Prd.gt.0) then
        IF(SPEC.LT.300)THEN
          COEFFS%A17 = R8CF(PTR,10)
          COEFFS%B17 = R8CF(PTR,11)   
        ELSE
          COEFFS%A17 = R8CF(PTR,12)
          COEFFS%B17 = R8CF(PTR,13)           
        ENDIF   
        COEFFSO%A17 = R8CFO(PTR,8)
        COEFFSO%B17 = R8CFO(PTR,9)
      else
        errFlg=1
        return
      endif 
      
      IF(dbhIb.lt.FIXDI4) dbhib = FIXDI4
      COEFFS%DBHIB = dbhIb
c      COEFFS%DIB17=dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
c      IF(COEFFS%DIB17.LT.0.0) COEFFS%DIB17=0.0
      
!-----Get merchantability rules
       CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,
     >           MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >           PROD)
C     R8 use UPSHT1 for HT2PRD in the old Clark equation   
C     we need to put UPSHT1 to HT2PRD for the new Clark eq
      IF(prod.NE.'01'.AND.UPSHT1.GT.0.AND.
     &  HT2PRD.LE.0.AND.MTOPS.EQ.4.0)THEN
        HT2PRD = UPSHT1
      ENDIF   

C    R8 Clark uses diameter outside bark for top diameter. 
C    This includes MTOPP, MTOPS, sawDib, plpDib, topDib
      IF(SPEC.LT.300)THEN
        if(mTopP.ne.0) then
          sawDib=mTopP
        else
          sawDib = 7
        endif
      ELSE
        if(mTopP.ne.0) then
          sawDib=mTopP
        else
          sawDib=9
        endif
      ENDIF      
      if(mTopS.ne.0) then
        plpDib=mTopS
      else
        plpDib = 4
      endif

C-----Get top height and top DIB Oct 2022 Increase filter to 17.4' from 17.3 DW
      short=.false.
      IF(htTot.gt.0.0) then
        topDib=0.0
        if(htTot.ge.17.4) then
          topHt=htTot
        else
          short=.true.
          topHt=17.4
          shrtHt=htTot
        endif
C               
        COEFFS%DIB17=dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
        IF(COEFFS%DIB17.LT.0.0) COEFFS%DIB17=0.1

        FCLSS = COEFFS%DIB17
        DBH = dbhOb
        THT = htTot
        IF(SPEC.NE.221 .AND. SPEC.NE.222 .AND. SPEC.NE.544)THEN
          IF (SPGRP.EQ.100) THEN
             IF (THT.LT.32.5)                   FCMIN = 56
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 64
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 66
             IF (THT.GE.42.5)                   FCMIN = 67
          ELSEIF (SPGRP.EQ.300) THEN
             IF (THT.LT.32.5)                   FCMIN = 57
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 60
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 64
             IF (THT.GE.42.5)                   FCMIN = 67
          ELSE
             IF (THT.LT.32.5)                   FCMIN = 58
             IF (THT.GE.32.5 .AND. THT.LT.37.5) FCMIN = 65
             IF (THT.GE.37.5 .AND. THT.LT.42.5) FCMIN = 67
             IF (THT.GE.42.5)                   FCMIN = 69
          ENDIF
          FCDIB = DBH * FCMIN * .01
          IF (THT.LT.47.5 .AND. FCLSS.LT.FCDIB) FCLSS = FCDIB
          COEFFS%DIB17 = FCLSS
          COEFFSO%DIB17 = (FCLSS - COEFFS%AFI)/COEFFS%BFI
        ENDIF
      ELSEIF(upsHt1.GT.0.0)THEN
c 02/24/2021 not necessary to check Prod. HT to 4 is already set to HT2PRD      
c        IF(PROD.EQ.'01')THEN
          GOTO 888
!        ELSEIF(PROD.EQ.'08')THEN
!        !Old equation may enter TotalHt into UPSHT1 field
!          htTot = upsHt1
!          GOTO 666
c        ELSE
c          GOTO 777
c        ENDIF  
      elseif(ht2Prd.gt.0) then
777       topDib=4
          !topDib=FIXDI4
          IF(upsHT1.GT.0.0)THEN
            if(upsHt1.ge.17.4) then
              topHt=upsHt1
            else
              short=.true.
              topHt=17.4
              shrtHt=upsHt1
            endif
          ELSE
            if(ht2Prd.ge.17.4) then
              topHt=ht2Prd
            else
              short=.true.
              topHt=17.4
              shrtHt=ht2Prd
            endif
          ENDIF
         COEFFS%DIB17=dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
         IF(COEFFS%DIB17.LT.0.0) COEFFS%DIB17=0.1
          
         DBH=dbhOb
         HT2=topHt
         FCLSS = COEFFS%DIB17
         IF (FCLSS.LT.FIXDI4) FCLSS = FIXDI4

         IF(SPEC.NE.221 .AND. SPEC.NE.222 .AND. SPEC.NE.544) THEN
           IF (SPGRP.EQ.100) THEN
             IF (DBH.LT.5.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 70
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 75
                IF (HT2.GE.27.5)                   FCMIN = 80
             ELSEIF (DBH.LT.6.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 66
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 70
                IF (HT2.GE.27.5)                   FCMIN = 74
             ELSEIF (DBH.LT.7.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 64
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 72
             ELSEIF (DBH.LT.8.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 60
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 69
             ELSE
                IF (HT2.LT.22.5)                   FCMIN = 59
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 69
             ENDIF
           ELSEIF (SPGRP.EQ.300) THEN
             IF (DBH.LT.5.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 74
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 76
                IF (HT2.GE.27.5)                   FCMIN = 76
             ELSEIF (DBH.LT.6.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 65
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 69
                IF (HT2.GE.27.5)                   FCMIN = 74
             ELSEIF (DBH.LT.7.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 61
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 65
                IF (HT2.GE.27.5)                   FCMIN = 68
             ELSE
                IF (HT2.LT.22.5)                   FCMIN = 60
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 62
                IF (HT2.GE.27.5)                   FCMIN = 68
             ENDIF
           ELSE
             IF (DBH.LT.5.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 71
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 72
                IF (HT2.GE.27.5)                   FCMIN = 76
             ELSEIF (DBH.LT.6.5) THEN
                IF (HT2.LT.22.5)                   FCMIN = 68
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 71
                IF (HT2.GE.27.5)                   FCMIN = 74
             ELSE
                IF (HT2.LT.22.5)                   FCMIN = 63
                IF (HT2.GE.22.5 .AND. HT2.LT.27.5) FCMIN = 67
                IF (HT2.GE.27.5)                   FCMIN = 70
             ENDIF
           ENDIF
           FCDIB = DBH * FCMIN * .01
           IF (HT2.LT.32.5 .AND. FCLSS.LT.FCDIB) FCLSS = FCDIB
         ENDIF
         COEFFS%DIB17=FCLSS
        
      else
888     if(spec.lt.300)then
          topDib = 7
        else
          topDib = 9
        endif
        !topDib=FIXDI79
c       if sawtimber topHt is provided, no recalc topHt. 11/15/2011 (yw)
          if(upsHt1.gt.0) then
            if(upsHt1.ge.17.4) then
              topHt=upsHt1
            else
              short=.true.
              topHt=17.4
              shrtHt=ht1Prd
            endif
            if(ht1prd.le.0.AND.MTOPP.EQ.topDib) ht1Prd = upsHt1
          else        
            if(ht1Prd.ge.17.4) then
              topHt = ht1Prd
            else
              short=.true.
              topHt=17.4
              shrtHt=ht1Prd
            endif
          endif
        COEFFS%DIB17=dbhOb*(COEFFS%A17+COEFFS%B17*(17.3/topHt)**2)
        FCLSS = COEFFS%DIB17
        IF (FCLSS.LT.FIXDI79) FCLSS = FIXDI79
        COEFFS%DIB17 = FCLSS
        IF(COEFFS%DIB17.LT.0.0) COEFFS%DIB17=0.1
        
      endif
      if(dbhOb.le.topDib) errFlg=11

      COEFFSO%DBHIB = dbhOb
      IF(htTot.LE.0.0)THEN
        COEFFSO%DIB17=dbhOb*(COEFFSO%A17+COEFFSO%B17*(17.3/topHt)**2)
      ENDIF
      IF(COEFFSO%DIB17.LT.COEFFS%DIB17) COEFFSO%DIB17=COEFFS%DIB17
      IF(COEFFSO%DIB17.LT.topDib) then
        !COEFFSO%DIB17=topDib
        IF(topHt.GT.17.2)THEN
          COEFFSO%DIB17=topDib+(dbhOb-topDib)*(topHt-17.3)/(topHt-4.5)
        ENDIF
      ENDIF
999   return
      end SUBROUTINE R8PREP
