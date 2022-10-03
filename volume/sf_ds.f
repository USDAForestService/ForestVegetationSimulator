!== last modified  4-9-2002
      SUBROUTINE SF_DS(JSP,GEOSUB,NEXTRA,SETOPT,ineedsl,slope,DBH,
     >                 TOTALH,HTUP,HEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,
     >                 PINV_Z,DIB)
c     Estimate Dib at any height  (any species, any method)
c     Optionally estimate slope   d(DIB)/d(H)
C
c     HTUP     input     R*4   Section height      
c     DIB   output    R*4   Diameter inside bark
c
      CHARACTER*2 GEOSUB
      REAL*4 HTUP,DIB,DBH,TOTALH, HEX(2),ZEX(2),slope,HH1,HH2,RH
      REAL*4 RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2),D1
      REAL SF_YHAT,SL1,D1REV,OFF1,D2,OFF2,D2REV,SL2
      
      INTEGER JSP, NEXTRA, SETOPT(6), ineedsl
      
       if(HTUP.gt. totalh) then
         dib=0.
         if (ineedsl.eq.1) slope = -1
         return
        ENDIF

      HH1=HTUP
c              if systems other than Flewelling & Raynes are wanted,
c              they should be inserted here.   
      RH = HH1/TOTALH
      d1=sf_yhat(JSP,RH,totalh,ineedsl,slope,RHFW,RFLW,TAPCOE,F)
      SL1=SLOPE
c      if (supplement) then
c          RH=HTUP/TOTALH
c          CALL SF_MODR3(RH,FX)
c          SLOPE = SLOPE*FX + D1 * (DFR/TOTALH)
c          DIB = D1*FX 
c          RETURN         
c         ENDIF
      IF(NEXTRA.GT.0) THEN
c                 calculate slope as the 1st derivative of base function
c                        plus the first derivative of the offset.
        call sf_yhat3(JSP,GEOSUB,NEXTRA,SETOPT,DBH,TOTALH,HH1,D1,
     >                    HEX,ZEX,FMOD,PINV_Z,D1rev)
        DIB = D1REV
        off1 = d1rev - d1
        if(ineedsl.ne.0) then
           if(hh1.lt. 0.99*totalH) then
              HH2 = hh1 + totalH/800.0
           else                      
              HH2 = hh1 - totalH/800.0
           ENDIF
           d2=sf_yhat(JSP,RH,totalh,ineedsl,slope,RHFW,RFLW,TAPCOE,F)
           call sf_yhat3(JSP,GEOSUB,NEXTRA,SETOPT,DBH,TOTALH,HH2,D2,
     >                    HEX,ZEX,FMOD,PINV_Z,D2rev)
           off2 = d2rev - D2
           SL2 = (off2-off1) /(HH2-HH1)
           SLOPE = (SL1 + SL2)
        ENDIF
      
      ELSE
        DIB = D1 
      ENDIF
      
      RETURN
      END

