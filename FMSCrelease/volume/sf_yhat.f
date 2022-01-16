C----------
C VOLUME $Id$
C----------
!== last modified  12-08-2003
      FUNCTION SF_YHAT(JSP,RH,totalh,ineedsl,slope,RHFW,RFLW,TAPCOE,F)
c                                        STEP 6 of the 2-pt application.

c            calculate the predicted  dib, given relative height
c            this is based on Eqns 1 thru 6 of the draft report.
c
c            This routine may also takes the optional step of
c            calculating 1st derivatives.
C  REVISED TO CORRECT NUMERICAL UNDERFLOW PROBLEM (30SEP99)
C          
      REAL SF_YHAT
      INTEGER INEEDSL,JSP,I_SEG
      REAL RHI1, RHI2, RHC, RHLONGI, totalh
      REAL R1, R2, R3, R4, R5, A3                       
      REAL A0, A1, A2,  A4
      REAL B0, B1, B2, B4
      REAL E1, E2
      REAL C1 , C2
      REAL RH,RHFW(4),RFLW(6),TAPCOE(12)
      REAL SLOPE , F,RH_LENGTH,DY_DX,DD_DH
      REAL*8 X,Y,SUS1,SUS2,SUS3
C
      SLOPE = 0
      R1=RFLW(1)
      R2=RFLW(2)
      R3=RFLW(3)
      R4=RFLW(4)
      R5=RFLW(5)
      A3 = RFLW(6)                                                 

      RHI1 = RHFW(1)
      RHI2 = RHFW(2)
      RHC = RHFW(3)
      RHLONGI = RHFW(4)
          
      A0 = TAPCOE(1)
      A1 = TAPCOE(2)
      A2 = TAPCOE(3)
      A4 = TAPCOE(4)
      B0 = TAPCOE(5)
      B1 = TAPCOE(6)
      B2 = TAPCOE(7)
      B4 = TAPCOE(8)
      C1 = TAPCOE(9)
      C2 = TAPCOE(10)
      E1 = TAPCOE(11)
      E2 = TAPCOE(12)

c                              Tests for invalid input
      if (RH .gt. 1.0) then
          SF_YHAT = 0.0
          if (INEEDSL .eq. 1) SLOPE = 0.0
          RETURN
                       end if

      if (RH .lt. 0.0) then
          SF_YHAT = F                  
          if (INEEDSL .eq. 1) SLOPE = 0.0
          RETURN
                       end if

c                                                  upper segment
       if (rh.ge. rhc) then
          x=(1.0 - rh)/(1.0-rhc)
C                           Black Hills uses diff form
          IF(JSP.EQ.22) THEN   
             y  = c2*x + (c1/2)*x*x - (c1/6)*x*x*x
          ELSE
             y = x * ( c2 +x * ( (c1/2.0) - (C1/6.0)*x)) 
          ENDIF

          RH_LENGTH= 1-rhc
          I_SEG=1
          if(ineedsl.eq.1) DY_DX = REAL(C2 + X*(C1 - C1/2*X))
          GO TO 100
       end if                 
c                                                     middle segment
      if (rh.ge. rhi2) then
          X = (rh - rhi2)/( rhc - rhi2)
          if (x.gt.0.0) then
             IF(JSP.EQ.22) THEN
C----------
C  TEST FOR POTENTIAL NUMERICAL UNDERFLOW FROM EXPONENTAL TERM b.havis 9/99
c  if x^(b1+2) < E-20, set to zero
C----------
                    IF (B1*LOG10(X) .LE. -20.) THEN
                         SUS1= 0.0
                    ELSE
                         SUS1= x**(b1+2)
                    ENDIF
C
C                                  Black Hills uses diff form
               y = b0 +b4*x - b2/((b1+1)*(b1+2))*SUS1 +
     >                    b2/6.0D0*x*x*x
             ELSE
C----------
C  TEST FOR POTENTIAL NUMERICAL UNDERFLOW FROM EXPONENTAL TERM b.havis 9/99
c  if x^(b1) < E-20, set to zero
C----------
                    IF (B1*LOG10(X) .LE. -20.) THEN
                         SUS2= 0.0
                    ELSE
                         SUS2= X**(B1)
                    ENDIF
C 
                y = b0 +x*(b4 + x*(-b2/((b1+1)*(b1+2))*SUS2 +
     >                    b2/6.0D0*x ))
             ENDIF
          else
             y = b0
          end if

          if(ineedsl.eq.1) then
             RH_LENGTH=rhc-rhi2
             I_SEG=2
             if (x.gt.0.) then
C
C----------
C  TEST FOR POTENTIAL NUMERICAL OVERFLOW FROM EXPONENTAL TERM b.havis 9/99
c  if x^(b1+1) < E-20, set to zero
C----------             
               IF (B1*LOG10(X) .LE. -20.) THEN
                    SUS3= 0.0
               ELSE
                    SUS3= x**(b1+1)
               ENDIF             
C             
                 dy_dx   =REAL(b4 -b2/(b1+1.0d0)*SUS3 + b2/2.0d0*x*x)
             else
                 dy_dx   = b4
             end if           
          end if
          GO TO 100
      end if    
c                                                     straight segment
      if (rhlongi.gt. 0.0  .and. rh .gt. rhi1) then
        y = e1 + e2*rh
        if(ineedsl.eq.1) then
          I_SEG=3
          dy_dx=e2
          RH_LENGTH=1.0d0
        end if
        GO TO 100
      end if     

c                                                     lower segment
      x = (rhi1-rh)/rhi1
      IF (JSP.EQ.22) THEN 
c                         Black Hills uses diff form
         y = a0 + (a4+a2/a3)*x + a2/(2*a3*a3)*x*x + 
     >             a1*x*x*x+a2*log(1.0D0 - x/a3)
      ELSE
         y = a0 + x*((a4+a2/a3) + x*(a2/(2*a3*a3) + a1*x)) +
     1             a2*log(1.0D0 - dble(x)/dble(a3))                 
      ENDIF
      
      if(ineedsl.eq.1) then
          RH_LENGTH=rhi1
          I_SEG=4
          dy_dx =REAL(a4 + a2/a3 + a2/(a3*a3)*x + 3*a1*x*x - a2/(a3-x))
      end if
      
100   SF_YHAT=REAL(F*Y)
        
      if (INEEDSL .eq. 1) then     
c                   the derivatives were calculated based on y and x
c                   lets also calculate them based on dib and H.
            dd_dH = dy_dx     * F /( RH_LENGTH * TOTALH )
            if (I_SEG .ne. 2 .and. i_seg.ne. 3)   dd_dH=-dd_dH
            SLOPE = DD_DH
      end if

      RETURN
      END
