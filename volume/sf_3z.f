!== last modified  10-30-2003
      SUBROUTINE SF_3Z(JSP,GEOSUB,SETOPT,NEXTRA,DBH,TOTALH,HEX,ZEX,
     >                 RFLW,RHFW,TAPCOE,F,PINV_Z,FMOD)
c     given standardized errors at 1 or 2 heights besides BH, perform
c     necessary calculations prior to estimating all conditional diameters
c     This routine assumes that NEXTRA, hextra(*) and dobex(*) are filled
c           in with correct values.
c
c     NEXTRA    Input   I       Number of extra measurement points (besides HT, DBH)
c                            (currently must = 1 or 2)
c     HEX    Input   R*4()   Vector of extra measurement heights
c     ZEX    Input   R*4()   Standardized errors at those heights. 

C      LOGICAL METRIC
      CHARACTER*2 GEOSUB
      INTEGER SETOPT(6),JSP,NEXTRA, INEEDSL,JX
      REAL*4  DBH,TOTALH,BH,SLOPE
     
      REAL*4 HEX(2),ZEX(2),RH,DIBMOD,SF_YHAT,F,DIBACT,ABS_CHNG,SF_CORR
      REAL*8 P12, P12SQ
      REAL*8 P_INV(2,2)       
      REAL*4 RFLW(6),RHFW(4),TAPCOE(12)
      
      REAL FMODMAX, HFIRSTUP, FMODMAXU, FMOD(3)
      REAL HEXTRA(2), Z(2)        
      REAL pinv_z(2)                
c      Logical ivstat_2, IVSTAT_S

      INEEDSL = 0
      IF(NEXTRA.lt. 1  .or. NEXTRA.gt. 2) then
c            write(*,1) NEXTRA
c1           format(' Subroutine SF_3z given invalid NEXTRA value =',i4/
c     1             ' Please correct the calling program. STOP. ')
      ENDIF
      BH = 4.5
      IF( SETOPT(1).eq.1) then
            fmodmax=.15
            HFIRSTUP=0.0
      ENDIF  

      DO 100 JX=1,NEXTRA
         HEXTRA(jx)=HEX(jx)
         Z(jx) = ZEX(jx)
         IF(setopt(1) .eq. 1) then 
            RH = HEXTRA(JX)/TOTALH
            DIBmod = SF_YHAT(JSP,RH,TOTALH,ineedsl,slope,RHFW,RFLW,
     >                                                      TAPCOE,F)
            CALL SF_DFZ(JSP,GEOSUB,DBH,TOTALH,HEXTRA(JX),DIBmod,
     >                                            Z(JX),DIBact)
            ABS_CHNG = ABS(DIBmod - DIBact)
            FMODMAX = MAX(FMODMAX, ABS_CHNG / DIBmod)
            IF (HEX(jx).gt. bh ) then
               IF(hfirstup .eq. 0.0  .or. HEX(jx).lt. hfirstup) then
                   fmodmaxu = min(dibmod, 2.0*abs_chng)/dibmod 
                   Hfirstup=HEX(jx)
               ENDIF
            ENDIF     
            FMOD(1) = FMODMAX
            FMOD(2) = FMODMAXU
            FMOD(3) = HFIRSTUP
         ENDIF
100   continue                

      if(NEXTRA.eq.2) then
           P12=SF_corr(JSP,GEOSUB,TOTALH, HEXTRA(1), HEXTRA(2))
           P12SQ=P12*P12
           if(p12.ne.0.) then
               P_INV(1,1) = 1.0d0/(1.0d0-P12SQ)
               P_INV(2,2) = 1.0d0/(1.0d0-P12SQ)
               P_INV(1,2) = -P12/ (1.0d0-P12SQ)
               P_INV(2,1) = -P12/ (1.0d0-P12SQ)
           else
               P_INV(1,1)=1.
               P_INV(2,2)=1.
               P_INV(1,2)=0.
               P_INV(2,1)=0.
           ENDIF
c                              multiply inverse corr matrix by (z1 z2)'
           PINV_Z(1) = p_inv(1,1)*z(1) + p_inv(1,2)*z(2)            
           PINV_Z(2) = p_inv(2,1)*z(1) + p_inv(2,2)*z(2)            
      ENDIF
c        IVSTAT_S = .false.
      Return
      END
