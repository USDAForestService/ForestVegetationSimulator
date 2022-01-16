C----------
C VOLUME $Id$
C----------
!== last modified  09-14-2007
c== Test for New Inside Bark Models
C     FILE CONTAINS THE FOLLOWING ROUTINES:
C          SHP_AK
C          COR_AK
C          VAR_AK
c***********************************************************************
c      SUBROUTINE BRK_AK(JSP,geosub,DBHOB,DOB,HT2,DBTBH,DIB,dbt)
c***********************************************************************
c                                       ALASKA YC MODEL
C       CHARACTER*2 GEOSUB
c      REAL DBHOB,HT2,DBTBH,DBT,DIB
c      REAL DOB
c      INTEGER JSP
cc          add upper stem bark model here 
c      RETURN (0.0)
c      END
c***********************************************************************
c***********************************************************************
      SUBROUTINE SHP_AK(JSP,GEOCODE,DBHIB,HT,RFLW,RHFW)
c                Stem form  SHAPE parameters for COOP #2 (INGY, east-side)
c***********************************************************************
c                   
c                         J. W. Flewelling, July, 1996 
c                                   NOTE:  F(49) reference  REMOVED
      IMPLICIT DOUBLE PRECISION (A-H, O-Z) 

      INTEGER JSP,JRSP
      CHARACTER*2 GEOCODE
      REAL*8 F(47,4),SUBF(3)
      REAL*4 RFLW(6),RHFW(4)
      REAL*4 RHI1, RHI2, RHC, RHLONGI                                   
      REAL*4 DBHIB, HT, DMEDIAN,DFORM
      REAL*4 R1, R2, R3, R4, R5, A3
      REAL*8 U7,U1,U9T,U9,U8,U2,U3,U4,U5,U6
           
C     ALASKA CEDAR   JSP = 31
      DATA (F(I,1),I=10,47)/  0.36073443D+01, 0.28808170D+00,
     >   0.36880717D-03,  -0.59261770D+00,  -0.26176422D+00,
     >   0.21473459D+00,   0.10424453D-04,   0.00000000D+00,
     >   0.12753659D+05,  -0.29418473D+04,  -0.59464947D-01,
     >  -0.52356508D+00,   0.31885182D+01,   0.44300416D+00,
     >  -0.11222337D+00,   0.62259640D+01,  -0.24733297D+01,
     >  -0.80698148D+01,   0.23730057D+01,   0.69999943D+01,
     >  -0.97392805D+01,  -0.17557578D+01,   0.30946244D+01,
     >  -0.88493982D-01,   0.23735651D+01,   0.12393680D+01,
     >  -0.74687251D+00,  -0.11540908D+00,   0.41429549D+01,
     >   0.18799039D+01,  -0.70710595D+00,  -0.59270766D-01,
     >  -0.17168611D+04,   0.35569956D+03,   0.00000000D+00,
     >   0.10969542D+01,  -0.28489405D-01,  -0.87696686D-01/

C     WESTERN RED CEDAR  JSP = 32          
      DATA (F(I,2),I=10,47)/  0.98477853D+00, 0.58511441D+00,
     >   0.30909028D-03,  -0.89204162D+00,  -0.23390452D+00,
     >  -0.49998897D-02,   0.54604625D-01,   0.00000000D+00,
     >   0.12769089D+05,  -0.28750172D+04,   0.23208522D+01,
     >   0.97290535D+00,   0.21836353D+01,   0.96352866D-01,
     >  -0.60131935D-01,   0.46427702D+00,  -0.78654704D+00,
     >   0.30346464D+01,  -0.50430550D+00,  -0.25389446D+01,
     >  -0.21025226D+02,   0.28035137D+01,   0.87909814D+01,
     >  -0.86687109D+00,   0.73773176D+00,  -0.13553797D+01,
     >  -0.24498232D+00,   0.50321072D+00,   0.17521623D+01,
     >   0.35018471D+01,   0.14592921D+00,  -0.12726548D+00,
     >  -0.17269922D+04,   0.35994895D+03,   0.00000000D+00,
     >   0.88681845D+01,   0.58686630D+02,   0.51018047D+00/

C     SPRUCE AND HEMLOCK  JSP 33 OR 34
      DATA (F(I,3),I=10,47)/   0.49336868D+01,   0.40232019D-01,
     >   0.83224395D-03,   0.90107598D+00,  -0.90527505D+00,
     >   0.89268065D+00,   0.14535135D+00,   0.00000000D+00,
     >   0.78000000D+01,   0.00000000D+00,   0.00000000D+00,
     >   0.99000000D+00,   0.50129548D+01,  -0.32927757D+00,
     >  -0.82895182D+00,   0.46568444D+01,  -0.16673502D+01,
     >  -0.54307696D+01,   0.12439151D+01,   0.13910769D+02,
     >  -0.56519913D+01,   0.11187845D+02,   0.69281311D+01,
     >   0.23890379D+02,   0.96523635D+01,  -0.95228393D+01,
     >  -0.20216598D+01,   0.19022197D+01,   0.63832200D+01,
     >   0.46545050D+01,   0.81554530D+01,   0.14744247D+02,
     >   0.00000000D+00,   0.00000000D+00,   0.00000000D+00,
     >   0.14356636D+01,  -0.50828256D+01,  -0.21749222D+02/

c     SPRUCE AND HEMLOCK Second Growth JSP = 35 OR 36
      DATA (F(I,4),I=10,47)/ 0.57392551D+00,  0.52192881D+00,
     >  0.72540826D-03, -0.36763113D+01,  0.27363686D+00,
     > -0.27523803D+00,  0.00000000D+00,  0.00000000D+00,
     >  0.64635211D+03, -0.27845408D+03,  0.80008578D+03,
     >  0.61787943D+00,  0.19381343D+01,  0.83437003D-01,
     >  0.16607331D-01,  0.39548950D+01, -0.23516392D+01,
     >  0.81537829D+01, -0.13385701D+01,  0.70000000D+01,
     >  0.11598864D+02, -0.26234962D+01, -0.18689695D+01,
     >  0.10535631D-01,  0.16926399D+01, -0.22896149D+00,
     > -0.61712415D+00,  0.14818512D+00,  0.64572326D+03,
     > -0.25009909D+03,  0.13805138D+03, -0.13979034D+03,
     >  0.46307044D+00,  0.22407584D+01,  0.00000000D+00,
     >  0.10050000D+01, -0.36363974D+00, -0.38757500D+01/

c     second growth submodel
      DATA (SUBF(I),I=1,3)/0.45284010D+01,0.93038602D+01,0.70000492D+01/
c                                            Set counter for regional adjs
c                               Check for regional modifications requested
      JRSP=JSP - 30
C !SPRUCE AND HEMLOCK USE SAME MODEL FORM
      IF(JRSP .EQ. 4) JRSP = 3  
      IF(JRSP .EQ. 5 .OR. JRSP.EQ.6) JRSP = 4  

c     Check for spruce hemlock second growth sub-model
      IF(JRSP.EQ.3 .AND. GEOCODE.EQ.'01') THEN
         F(25,JRSP) = SUBF(1)
         F(34,JRSP) = SUBF(2)
         F(42,JRSP) = SUBF(3)
      ENDIF

      DMEDIAN = REAL(f(10,JRSP) *(HT-4.5)**(f(11,JRSP)+f(12,JRSP)*HT))
      DFORM = REAL(DBHIB/DMEDIAN -1.0d0)
c                                                                RHI1
      U7 = F(13,JRSP) + f(14,JRSP)*log(ht) + f(15,JRSP)*DFORM 
c                                                                RHLONGI
      U9T = f(18,JRSP) + f(19,JRSP)*log(HT) +f(20,JRSP)*DFORM
      if (U9T .lt. -7.0d0) U9T=-7.0d0      
      if (U9T .gt.  7.0d0) U9T= 7.0d0    
      U9 = f(16,JRSP)* exp(U9T)/(1.0d0 +exp(U9T) )
c                                                                 RHC
      U8 =f(21,JRSP)+f(22,JRSP)*HT +f(23,JRSP)*log(HT)+f(24,JRSP)*DFORM
c                                                         U1-U5
      U1 = f(25,JRSP) + f(26,JRSP)*log(HT) +f(27,JRSP)*DFORM 
     >               + f(28,JRSP)*DFORM*log(HT)
      U2 = f(29,JRSP) + f(30,JRSP)*DFORM + f(31,JRSP)*log(ht)
     >               + f(32,JRSP)*dform*log(ht)
     >               + f(33,JRSP)*DBHIB                                               
      IF(JRSP .eq. 15) then
c                                                Lodgepole Pine (INGY)
        U3 = f(34,JRSP) + f(35,JRSP)*DFORM
     >     + f(36,JRSP) * (1.0D0 - exp(f(37,JRSP)*HT) )   
      else
        U3 = f(34,JRSP) + f(35,JRSP)*DFORM + f(36,JRSP)*log(HT) 
     >     + f(37,JRSP)*log(HT)*DFORM
      end if
      U4 = f(38,JRSP) +f(39,JRSP)*DFORM +f(40,JRSP)*log(ht) 
     >     +f(41,JRSP)*DBHIB
      U5 = f(42,JRSP) + f(43,JRSP)*log(HT)        
c                                        U6=A3 
      U6 = f(45,JRSP) + f(46,JRSP)*DFORM  +f(47,JRSP)*log(HT)
c                                              limits on U1 to U5
      if (U1.lt. -7.0d0) U1=-7.0d0                               
      if (U1.gt.  7.0d0) U1= 7.0d0
      if (U2.lt. -7.0d0) U2=-7.0d0                               
      if (U2.gt.  7.0d0) U2= 7.0d0                               
      if (U3.lt. -7.0d0) U3=-7.0d0                               
      if (U3.gt.  7.0d0) U3= 7.0d0                               
      if (U4.lt. -7.0d0) U4=-7.0d0                               
      if (U4.gt.  7.0d0) U4= 7.0d0                               
      if (U5.lt. -7.0d0) U5=-7.0d0                               
      if (U5.gt.  7.1d0) U5= 7.1d0                                     
      if(U6 .lt. 1.005) U6=1.005D0
      if(U6 .gt. 10.0d0) U6=10.0d0
      if (U7.lt. -7.0d0) U7=-7.0d0                               
      if (U7.gt.  7.0d0) U7= 7.0d0                                
      if (U8 .gt. 0.99) U8=0.99
      if (u9.gt. 0.3d0) u9=0.3d0
      if (u9.lt. 0.0d0) u9=0.0d0
c                                         Define geometric parameters
c                                        (These outcomes are SINGLE precision)
      R1= REAL(dexp(U1)/ (1.0d0 + dexp(U1)))                           
      R2= REAL(dexp(U2)/ (1.0d0 + dexp(U2)))                            
      R3= REAL(dexp(U3)/ (1.0d0 + dexp(U3)))                            
      R4= REAL(dexp(U4)/ (1.0d0 + dexp(U4)))                             
      IF (U5 .le. 7.0d0) then
          R5= REAL(0.5d0 + 0.5d0*dexp(U5)/ (1.0d0 + dexp(U5)))                        
      else
          R5=1.0
      end if
      A3=REAL(U6)
      RHI1 =  REAL(dexp(U7) / ( 1.0d0 + dexp(U7) ))
      if (RHI1.gt. 0.5 ) RHI1=0.5 
      RHLONGI= REAL(U9)  
      RHI2 = RHI1 + RHLONGI
c      RHC = RHI2 + (1.0D0 - RHI2) * dexp(U8)/( 1.0d0 + dexp(U8))
c      If (RHC .lt. RHI2 +0.01) then
c          RHC = MIN( RHI2+0.01, (RHI2+1.0)/2.0 )
c        end if
      RHC=REAL(U8) 
      if (RHC .lt. RHI2+.01) RHC= min( RHI2+.01, (RHI2+1.)/2.0 )
C     PUT COEFFICIENTS INTO AN ARRAY FOR EASIER PASSING
      RFLW(1) = R1
      RFLW(2) = R2
      RFLW(3) = R3
      RFLW(4) = R4
      RFLW(5) = R5
      RFLW(6) = A3     
      
      RHFW(1) = RHI1
      RHFW(2) = RHI2
      RHFW(3) = RHC
      RHFW(4) = RHLONGI
      
      RETURN
      END 

c***********************************************************************
c***********************************************************************
      FUNCTION COR_AK(JSP,TOTALH,HI,HJ)
c                                                               CORRC2
c***********************************************************************
c     given 2 heights (hi and hj), estimate the correlation of the
c       Z errors in the corresponding dib's
c               NW Taper coop #2, (INGY, east-side 1995-1996)
c       coefficients 81-85

      REAL COR_AK
      REAL*8 V(5,4), Q1,Q2,QS,Q4,Q5,Q3,CORR
      REAL TOTALH, HI, HJ,BH,H1,H2,T3,T4,T1,T2

      INTEGER JSP,JSPR

C     ALASKA CEDAR
      DATA (V(I,1),I=1,5)/-0.48680838D+01, 0.14663798D+02,
     >    -0.48216320D+01,-0.17800924D+01,-0.20276812D+00/

C     WESTERN RED CEDAR
      DATA (V(I,2),I=1,5)/-0.40872513D+01, 0.98400990D+01,
     >    -0.38399599D+01,-0.24301984D+01, 0.35024803D+00/

C     SPRUCE HEMLOCK
      DATA (V(I,3),I=1,5)/  -0.38994532D+01, 0.10094975D+02,
     >      -0.29557863D+01,-0.10967578D+01,-0.34281732D+00/

C     SPRUCE HEMLOCK SECOND GROWTH
      DATA (V(I,4),I=1,5)/  -0.63750351D+01, 0.15306425D+02,
     >      -0.26876808D+01,-0.10538661D+01,-0.41907036D+00/

      BH = 4.5
      JSPR = JSP - 30
      IF(JSPR .EQ. 4) JSPR = 3
      IF(JSPR .EQ. 5 .OR. JSPR.EQ.6) JSPR = 4  
      
      Q1=V(1, JSPR)
      Q2=V(2, JSPR)
      QS=V(3, JSPR)
      Q4=V(4, JSPR)
      Q5=V(5, JSPR)

      Q3 = QS - (Q1+Q2)

c                          any error is completely correlated with itself
      if(hi.eq.hj ) then
           COR_AK=1.0
           return
        end if
c                               There are no errors at breast height (Ha!)
c                               Correlations are irrelevant.
      IF(HI.EQ.BH.OR.HJ.EQ.BH) THEN
              COR_AK=0.5
              return
           end if

c                              define heights h1 and h2 with h2 > h1
      H1=MIN(HI,HJ)
      H2=MAX(HI,HJ)
                 
      IF(H1.GT.BH) THEN
c                                         both heights are above BH
        t3=(h1-bh)/(totalh-bh)
        t4=(h2-bh)/(totalh-bh)                                
        CORR = exp( q1*(t4-t3) + q2*(t4*t4-t3*t3)/2.0D0
     >                  +q3*(t4*t4*t4 - t3*t3*t3)/3.0D0)     
        GO TO 100
      ENDIF

      IF(H2.GT.BH) THEN
c                                              h1 < Bh  and h2 > BH
         t3=(h2-bh)/(totalh-bh)                             
         T2 = (BH-H1)/BH    
         CORR = q5*exp( q4*t2 + q1*t3 +q2*t3*t3/2.0d0 
     >                    +q3*t3**3 / 3.0d0 )       
         go to 100
      ENDIF
c                                              h1 < Bh and H2 < Bh       
c                                              note: t2 < t1
       T2 = (BH-H2)/BH                                                   
       T1 = (BH-H1)/BH                                                   
       CORR=exp( q4* (t1-t2))        
C100    IF (  abs(CORR) .gt. 1.0  .and.  ier_UNIT.ne.0) 
C     1     write(IER_UNIT,102) JSP, H1,H2, CORR
C102        FORMAT(' CORRC2 ERROR on JSP=',i3,'  H1,H2, corr =',3f8.2)

100   COR_AK = REAL(CORR)

      RETURN
      END


c***********************************************************************
c***********************************************************************
      SUBROUTINE VAR_AK(JSP,DBH,HT,H,SE_LNX)
c                                                             SF_VARLN
c***********************************************************************
c                Variance of dib  for species assuming lognormal errors.
c                NW Coop #2, INGY
c                   
c                         J. W. Flewelling, July, 1996 
c
c      H      input   R*4   Section height  , for h NE breast height.
c      SE_LNX output  R*4   SQRT{ VAR[ln(dib)]}

C      PARAMETER (MAXC=100)
      REAL*8 F(12,4), V(16,4)             
      REAL*4 LOGHT, LVARHAT, VARHAT
      REAL DBH, HT, BH, H, SE_LNX              
      logical vmod1, vmod2
      REAL VA00,VA01,VA02,VB00,VB01,VC0,VX1,VX2,VX3,VA03
      REAL VE00,VE01,VE02,VF00,VF01,VG0,VA0,VB0,VE0,VF0,VC,XU,X
      REAL STD_FRAC
      REAL DMEDIAN,DRATIO
      INTEGER JSP,JSPR


c                                   Note: the following assignments are
c                                   for clarity. Faster execution would
c                                   be obtained by using the F() coefients
c                                   directly, and omitting the V%%% variables.

C     ALASKA CEDAR
      DATA (F(I,1),I=10,12)/0.19750000D+00, 0.96630000D+00, 
     >                      0.36700000D-03/
      
      DATA (V(I,1),I= 1,16)/-0.87861916D+01,
     >       0.38942378D+00, 0.24151069D+01, 0.86871264D+01,
     >      -0.12932895D+01, 0.10448179D+01, 0.00000000D+00,
     >       0.96526647D-01, 0.13685097D+00,-0.44306740D+00,
     >      -0.63677273D+01,-0.28346734D+00, 0.30403741D+00,
     >       0.23458454D+01, 0.15233441D+00, 0.15821190D+01/

C     WESTERN RED CEDAR
      DATA (F(I,2),I=10,12)/0.55800000D-01, 0.12635000D+01, 
     >                     -0.31600000D-03/
      
      DATA (V(I,2),I= 1,16)/-0.52217635D+01,-0.23364372D+00,
     >       0.10581464D+00, 0.76117971D+01,-0.10588704D+01,
     >       0.11469169D+01, 0.16914337D+00, 0.35117997D-01,
     >       0.10000000D+01, 0.39438121D-01,-0.68717202D+01,
     >      -0.40369684D-01, 0.57719712D+00,-0.48188520D+00,
     >       0.77458363D+00, 0.29097150D+01/

C     SPRUCE HEMLOCK
      DATA (F(I,3),I=10,12)/0.49336868D+01,   0.40232019D-01,
     >                      0.83224395D-03/
      
      DATA (V(I,3),I= 1,16)/  -0.65056179D+01,  -0.27270953D+00,
     >       0.21140193D+01,   0.75614779D+01,  -0.71651812D+00,
     >       0.74179167D+00,   0.34846513D-01,   0.35421062D-01,
     >      -0.18935952D+00,  -0.36782454D+00,  -0.33555096D+02,
     >      -0.36226359D+02,   0.49485299D+00,   0.30907042D+02,
     >       0.35490428D+02,   0.12091440D-01/

C     SPRUCE HEMLOCK SECOND GROWTH
      DATA (F(I,4),I=10,12)/0.51542754D+00,   0.52156599D+00,          
     >                      0.71621730D-03/
      
      DATA (V(I,4),I= 1,16)/  -0.59805600D+01,  -0.24733469D+00,                   
     >       0.26188946D+00,   0.14285228D+02,  -0.21896412D+01, 
     >       0.67083289D+00,   0.16760600D+00,   0.48980591D-01, 
     >       0.64890061D+00,  -0.49655428D-01,  -0.10135682D+02, 
     >       0.35558601D-01,   0.32424523D+00,   0.42749494D+01, 
     >      -0.71915171D-01,   0.66192729D+00/                   


      JSPR = JSP - 30
      IF(JSPR .EQ. 4) JSPR = 3
      IF(JSPR .EQ. 5 .OR. JSPR.EQ.6) JSPR = 4  

c     coef 1-16 are coef 61 to 76 in coef file
      VA00 = REAL(V(1, JSPR))
      VA01 = REAL(V(2, JSPR))
      VA02 = REAL(V(3, JSPR))
      VB00 = REAL(V(4, JSPR))
      VB01 = REAL(V(5, JSPR))
      VC0  = REAL(V(6, JSPR))

      VX1 = REAL(V(7,JSPR))
      VX2 = REAL(V(8,JSPR))
      VX3 = REAL(V(9,JSPR))

      VA03 = REAL(V(10, JSPR))
      VE00 = REAL(V(11, JSPR))
      VE01 = REAL(V(12, JSPR))
      VE02 = REAL(V(13, JSPR))
      VF00 = REAL(V(14, JSPR))
      VF01 = REAL(V(15, JSPR))
      VG0  = REAL(V(16, JSPR))
      
      BH = 4.5
      
      DMEDIAN = REAL(F(10,JSPR) *(HT - BH)**(F(11,JSPR)+F(12,JSPR)*HT))
      DRATIO = DBH/DMEDIAN 
      LOGHT = log(HT)

       VA0 = VA00 + VA01*LOGHT + VA02*DRATIO  + VA03*LOGHT*DRATIO
       VB0 = VB00 + VB01*LOGHT

       VE0 = VE00 + VE01*LOGHT + VE02*DRATIO
       VF0 = VF00 + VF01*LOGHT

       VC = VC0 + VX1*LOGHT 
       if (h .lt. bh) then
c                                                below BH
             XU = (BH-H) / BH            
             IF (XU.LT. 0.111) THEN
C                                     Special adjustment from 4 ft to BH
                VMOD1 = .TRUE.
                X = XU
                XU = 0.111
             ELSE
                VMOD1 = .FALSE.
             ENDIF
                          
             LVARHAT = VE0 + VF0*XU**VG0 
                if (LVARHAT .gt. 15.0)   LVARHAT= 15.0
                if (LVARHAT .lt. -15.0)  LVARHAT= -15.0
             VARHAT  = exp(LVARHAT)
             SE_LNX  = SQRT(VARHAT)
             IF (VMOD1)  SE_LNX = SE_LNX * X/XU
                    
       else if (h.eq. BH)  then
c                                                at BH
             SE_LNX=0.0
       else if (h .lt. HT) then
c                                                above BH
             XU = (H - BH)/(HT - BH) 
             
             IF(XU .LT. 0.02) THEN
                VMOD2 = .TRUE.
                X = XU
                XU = 0.02
                STD_FRAC = (X/XU)
             ELSEIF (XU.GT. 0.96) THEN
                VMOD2 = .TRUE.
                X = XU
                XU = 0.96
                STD_FRAC = 1.0
             ELSE
                VMOD2 = .FALSE.
             ENDIF
              
             LVARHAT = VA0 + VB0*XU**VC + VX2*(HT/50.)**VX3 /(1.0-XU)
                if (LVARHAT .gt. 15.0)   LVARHAT= 15.0
                if (LVARHAT .lt. -15.0)  LVARHAT= -15.0  
             VARHAT = exp(LVARHAT) 
             SE_LNX  = SQRT(VARHAT)
             
             IF (VMOD2) SE_LNX = STD_FRAC * SE_LNX
       else
c                                This is above total ht - meaningless.
             VARHAT = 1.0 
             SE_LNX = SQRT(VARHAT)
       end if        
       
      RETURN
      END

c***********************************************************************
c***********************************************************************
      FUNCTION FDBT_AK(JSP,SETOPT,DBHOB,HTTOT)

c***********************************************************************
c                   given Diameter at breat height, outside bark
c                   calculate double bark thickness at breast height.
c                   NW Taper coop 2, INGY (east-side)
C              COEFFICIENT FILE - BARKBHC2.COE

c     DBH        INPUT     R*4  Diameter (o.b.) at Breast height (inches)
c     TOTHT      INPUT     R*4  Total tree height (ft)
c     FDBT_BC2   RETURNED  R*4  Double bark thickness
 
c     Note:  coefficients are from SF_SET2 (possibly file BARKBHC2.COE)
      REAL FDBT_AK
      INTEGER JSP,JSPR
      INTEGER setopt(6)
      REAL DBHIB,HTTOT,DBHOB
      INTEGER IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = SETOPT(1)
C
c                                       Use Global or regional Lead coef ?
      JSPR = JSP - 30

      IF(JSPR == 1) THEN
C      ALASKA YELLOW CEDAR
         DBHIB = -0.307528 + 0.982927*DBHOB
      ELSEIF(JSPR == 2) THEN
C      WESTERN RED CEDAR
         DBHIB = -0.451778 + 0.975611*DBHOB    
      ELSEIF(JSPR == 3) THEN
C      SPRUCE
c         DBHIB = -0.164009 + 0.980249*DBHOB - 0.001646*HTTOT
c         DBHIB = -0.189179 + 0.973778*DBHOB    
         DBHIB = -0.0171289+0.9511897*dbhob+0.000507*dbhob*dbhob
      ELSEIF(JSPR == 4) THEN
C      HEMLOCK
            DBHIB = -0.054306 + 0.916145*DBHOB + 0.004821*HTTOT
c            DBHIB = 0.040770 + 0.933471*DBHOB    

      ELSEIF(JSPR == 5) THEN
C      SPRUCE SECOND GROWTH
            DBHIB = -0.212445 + 0.983230*DBHOB

      ELSEIF(JSPR == 6) THEN
C      HEMLOCK SECOND GROWTH
            DBHIB = -0.088908 + 0.963193*DBHOB
      ENDIF

      FDBT_AK = DBHOB - DBHIB
      RETURN
      END 


C**********************************************************************
C**********************************************************************
