!== last modified  01-10-2003
      SUBROUTINE SF_TAPER (RHFW,RFLW,TAPCOE)
c             calculater taper coefficients, given geometric properties
c           this is step 4 of application;  calculate taper coefficients
                                                                       
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                             
      REAL*8 K, K2
      REAL*4 RHI1, RHI2, RHC, RHLONGI
      REAL*8 R1, R2, R3, R4, R5, A3
      REAL*8 A0, A1, A2, A4
      REAL*8 B0, B1, B2, B4
      REAL*8 C1, C2
      REAL*8 E1, E2     
      REAL*4 RHFW(4),RFLW(6),TAPCOE(12)
      REAL*8 S3,F_A3,G_A3,YB_MIN,YB_MAX
      REAL*8 YC,SLOPE,S1,YI_MIN,YI_MAX,YI2,S0,SLOPE_RHI,YI1,YB
c                            convert to double precison
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
          
c                 Upper Segment                  Appendix A, Eqns A1 thru A4
c                      (note: k is an arbitrary positive constant.
c                            changing it will affect some parameters,
c                            but will not affect predicted tree form.)
      K = 1.0
       
      YC = k*(1.0-rhc)
      C2 = R5 * YC
      C1 = 3.0* (YC - C2)
      SLOPE = -(3.0-R5)*K/2.0

c                 Middle Segment                 Appendix A, Eqns A6 thru A15
      
      S1= SLOPE*(RHC-RHI2)
      YI_MIN = YC - S1*(1.0+2.0*R3)/3.0
      YI_MAX = YC - S1*(5.0+4.0*R3)/9.0
      YI2    = YI_MIN  + R4 * (YI_MAX - YI_MIN)
      S0    = R3*S1

      B1 = ( 6.*YC -6.*YI2 -2.*S0-4.*S1) / (-3.*YC +3.*YI2 +2.*S0 +S1)
      B2 = S1 * (1-R3)/ ( 0.5 -1.0/(B1+1.0))
      B4 = S0
      B0 = YI2
      
      SLOPE_RHI = R3*S1/(RHC-RHI2)

c                            straight segment at inflections
      YI1 = YI2 - slope_RHI * RHLONGI    
c             y = E1 + E2*(RH)
      if (RHLONGI.gt. 0.0d0 ) then
            E2= (YI2-YI1)/RHLONGI
            E1 = YI1 - (E2*RHI1)
          else
            E1=YI2
            E2=0.0
          endif 

c                 Lower Segment            Appendix A, Eqns A16 thru A26
      S3 = -SLOPE_RHI * RHI1
      K2 =S3/R1
      
      F_A3 = 1./(6.*a3*a3)+log(1.-1./a3)+1./(3.*(a3-1.))+2./(3.*a3)

      G_A3 = (1./(a3-1.)-1./a3-1./(a3*a3) - 1./(a3-1.)**3.) / F_A3
      
      YB_MIN    = YI1 + (2.*S3+K2)/3. + (s3-k2)*F_A3 /
     1    (+1./(a3-1.) -1./a3 -1./(a3**2.) -1./(a3*a3*a3)  )
      YB_MAX = YI1 + (2.*S3+K2)/3. + (S3-K2)/g_A3

      YB = yb_MIN + R2*(YB_MAX - YB_MIN)
      
      A0=YI1
      A2=( YB - YI1 -(2.*s3+k2)/3.)/F_A3
      A1 = ( K2 - s3 + a2 *(1./(a3-1.) -1./a3 -1./a3**2.)) / 3.
      A4 = S3
                                     
c     convert to single precision
      TAPCOE(1) = A0
      TAPCOE(2) = A1
      TAPCOE(3) = A2
      TAPCOE(4) = A4
      TAPCOE(5) = B0
      TAPCOE(6) = B1
      TAPCOE(7) = B2
      TAPCOE(8) = B4
      TAPCOE(9) = C1
      TAPCOE(10) = C2
      TAPCOE(11) = E1
      TAPCOE(12) = E2
        
      RETURN
      END           

