!== last modified  4-9-2002
      SUBROUTINE SF_YHAT3(JSP,geosub,NEXTRA,SETOPT,DBH,THT,H,DIB,
     >                    HEX,ZEX,FMOD,PINV_Z,EDIB)
c      given 1 or 2 upper extra measurements, compute the new
c        conditional expectation.
c
c     H     input    R*4  section height of interest
c     DIB   input    R*4  original estimate of diameter inside bark
c     EDIB  output   R*4  conditional expectation of dib

      character*2 GEOSUB
      INTEGER SETOPT(6), NEXTRA,JSP
      REAL*4 DBH, THT, H, DIB, EDIB
      REAL HEX(2), BH, PINV_Z(2),FMOD(3), ZEX(2)    
      REAL FMODMAX, hfirstup, fmodmaxu
      REAL P12,SF_CORR,EZ,P13,P23,DIBACT,CHANGE,FULIMIT

      FMODMAX = FMOD(1)
      FMODMAXU = FMOD(2)
      HFIRSTUP = FMOD(3)
  
      bh = 4.5    
      if (dib.le. 0.0 .or. H .ge. tht) then
           Edib=0.
           Return
      ENDIF 
c                              apply formulas for 1 or 2 extra points
      if (NEXTRA.eq.1) then
           p12= SF_corr(JSP,GEOSUB,THT,HEX(1), h)
           EZ = p12*ZEX(1)
      else
           P13 = SF_corr(JSP,GEOSUB,THT,HEX(1), h)
           P23 = SF_corr(JSP,GEOSUB,THT,HEX(2), h)
           EZ =  p13*pinv_z(1) + p23*pinv_z(2)
      ENDIF           
c                                            estimate std err at h
      CALL SF_DFZ(JSP,GEOSUB,DBH,THT, H, DIB, EZ, DIBact)
      CHANGE = DIBact - DIB

      if(setopt(1).eq.1) then
          if(abs(change)/dib .gt. fmodmax)
     >           change= sign(fmodmax,change) * dib 
          if (h.gt. bh  .and. h.lt. hfirstup) then
                 fUlimit = (h-bh)/(hfirstup-bh) * FMODMAXU
                 if (abs(change)/dib .gt. FULIMIT) 
     >             change=sign(FULIMIT,change)*dib
          ENDIF
      ENDIF 

      EDIB = DIB + CHANGE
      RETURN
      END   
