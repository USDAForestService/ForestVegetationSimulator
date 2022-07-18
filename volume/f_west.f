C----------
C VOLUME $Id$
C----------
C  FILE CONTAINS THE FOLLOWING ROUTINES
C        SHP_W3
C        SHP_W4
C        SHP_W5
C        COR_WS
C        VAR_C1
C        SF_ZFD3
C        SF_ZFD4
C        SF_ZFD5
C        FDBT_C1
C        BRK_WS
c***********************************************************************
      SUBROUTINE SHP_W3(DBHOB,HTTOT,GeoSUB,RFLW,RHFW)
c                      Stem form  SHAPE paraemeters for Doug Fir (English units)
c***********************************************************************
c                      Coefficients for NW TAPER Coop  April, 1994
c                         (REGN  CD17.coe)
c                       Copyright  J. W. Flewelling, 1994 

      IMPLICIT DOUBLE PRECISION (A-H, O-Z) 

      CHARACTER*2 GEOSUB, DFSUB(8)
      INTEGER ID
      
      REAL*4 RHI1, RHI2, RHC, RHLONGI                                   
      REAL*4 DBHOB, HTTOT                             
      REAL*4 R1, R2, R3, R4, R5, A3
      REAL*4 RFLW(6), RHFW(4) 
      REAL*8 U1,U2,U3,U4,U5,U6,U7,U8,U9,U9A
       
      real*4 m1, m2, m3   
      real*8 f(50),FR25,FR34,DMEDIAN,DFORM,R34(8),R25(8)

c              common SF_A provides DBHOB(ob), Total height and DBHOB(ib)
c                                                                Median     
      data m1, m2 ,m3 / .566,  .634, .00074  /
      data (F(I),I=13,15)/ -.07799267d0, .8096211D0, -.9247244D0 /
      data (F(I),I=17,19)/ 0.166749D0,  -9.20884D0 , .1212094D0 /  
      data (F(I),I=21,24)/ 5.0719922d0, -.12555733d0, -1.6408313d0,
     >                     -.3005388D0/             
      data (F(I),I=25,26)/ -2.000088D0 , -.410677D0/               
      data (F(I),I=29,31)/ -1.428671d0,-0.8660924d0, .0543758d0 /  
      data (F(I),I=33,37)/ 0.80981083D0, 5.1785820d0, 1.8935219d0,
     >                    -2.420031d0 ,  -.009232401D0/    
      data (F(I),I=38,40)/ -11.49305d0, 2.037438d0, -.0114178d0  /   
      data (F(I),I=42,44)/  2.246288d0, -1.1410822d0, 1.7293166d0 /
      data (F(I),I=45,47)/ -3.18221d0, -2.119838d0, 1.3756086d0/

c                                                 Regional coefficients   
c              (alpha to numeric transform is in SF_2PT)
c     region order is:  CO  EV  NO  RS  SO  WE  WV  WC
c                       OR  OR  WA  WA  WA  WA  OR  WA
c             IREGION:   1   2   3   4   5   6   7   8
      DATA (DFSUB(I),I=1,8)/'01','02','03','04','05','06','07','08'/ 
c             Region 8 (Washington West coast) uses Region 6 (WA. West) coef.
      data (r25(i),i=1,8)/   -2.0262D0, -1.7945d0, -2.0366d0,
     >         -2.0811d0, -1.9868d0, -2.0151d0, -1.9475d0, -2.0151d0 /
      data (r34(i),i=1,8)/ 5.2132d0, 5.1417d0, 5.1768d0 ,     
     >          5.1156d0, 5.1654d0, 5.2413d0, 5.1427d0, 5.2413d0 /

c                               Region modifications to coefficients
      FR25=f(25)
      FR34=f(34)
      if (GEOSUB.NE.'00') then
        DO 100, ID=1,8
          IF(DFSUB(ID).EQ.GEOSUB) THEN
             FR25=r25(ID)            
             FR34=r34(ID) 
          ENDIF
 100    CONTINUE
      endif       
          
      DMEDIAN = m1 *(HTTOT - 4.5)**(m2 + m3*HTTOT)
      DFORM = DBHOB/DMEDIAN -1.0d0
c                                                                RHI1
      U7 = F(13) + f(14)*log(DBHOB+1.) + f(15)*log(HTTOT)
c                                                                RHLONGI
      U9A = f(18) + f(19)*HTTOT 
      if(U9A .gt. 7.0d0) U9a=7.0d0
      if(U9A .lt. -7.0d0) U9a=-7.0d0
      U9 = f(17)* dexp(U9A) / (1.0d0+dexp(U9A))
c                                                                 RHC
      U8 = f(21)+f(22)*log(HTTOT) +f(23)*DFORM + 
     >                         f(24)*(DBHOB/10.0D0)**1.5d0

c                                                         U1-U5
      U1 = FR25  + f(26)*log(HTTOT)
      U2 = f(29) + f(30)*log(DBHOB) + f(31)*DBHOB
      U3 = FR34  + f(35)*log(DBHOB+1.0d0) + f(36)*log(HTTOT) 
     >           +f(37)*DFORM*HTTOT + f(33)*DFORM
      U4 = f(38) + f(39)*DBHOB  + f(40)*HTTOT*DBHOB 
      U5 = f(42) + f(43)*HTTOT   +f(44)*DFORM
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
      if (U5.gt.  7.0d0) U5= 7.0d0                                
      if (U7.lt. -7.0d0) U7=-7.0d0                               
      if (U7.gt.  7.0d0) U7= 7.0d0                                
      if (U8.lt. -7.0d0) U8=-7.0d0                               
      if (U8.gt.  7.0d0) U8= 7.0d0                               

c                                        U6=A3 (bounds: 1.005-100)
      U6 = f(45) + f(46)*log(DBHOB+1.0d0) + f(47)*log(HTTOT)
      if (U6 .lt. -6.0) U6=-6.0
      if(U6 .gt. 6.0  ) U6=6.0
      U6 = 1.0D0 + exp( U6)      
         if(U6 .lt. 1.005) U6=1.005D0
         if(U6 .gt. 100.0d0) U6=100.0d0

c                                         Define geometric parameters
      R1= REAL(dexp(U1)/ (1.0d0 + dexp(U1)))                             
      R2= REAL(dexp(U2)/ (1.0d0 + dexp(U2)))                             
      R3= REAL(dexp(U3)/ (1.0d0 + dexp(U3)))                            
      R4= REAL(dexp(U4)/ (1.0d0 + dexp(U4)))                             
      R5= REAL(0.5d0 + 0.5d0*dexp(U5)/ (1.0d0 + dexp(U5)))                        

      A3=REAL(U6)
      RHI1 =  REAL(dexp(U7) / ( 1.0d0 + dexp(U7) ))
      if (RHI1.gt. 0.5) RHI1=0.5

      RHLONGI= REAL(U9)    
      RHI2 = RHI1 + RHLONGI

      RHC = REAL(RHI2 + (1.0 - RHI2) * dexp(U8)/( 1.0d0 + dexp(U8)))

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
      SUBROUTINE SHP_W4(DBHOB,HTTOT,GeoSUB,RFLW,RHFW)
c         Stem form  SHAPE paraemeters for Western Hemlock (English units)
c***********************************************************************
                   
c                      Coefficients for NW TAPER Coop  April, 1994
c                         (REGH  TEST1929.coe, WH422R*.coe)
c                       Copyright  J. W. Flewelling, 1994 


      IMPLICIT DOUBLE PRECISION (A-H, O-Z) 

      CHARACTER*2 GEOSUB, WHSUB(8)
      
      REAL*4 RHI1, RHI2, RHC, RHLONGI                                   
      REAL*4 DBHOB, HTTOT
      REAL*4 R1, R2, R3, R4, R5, A3
      REAL*4 RFLW(6), RHFW(4) 
       
c      real*4 m1, m2, m3   
      real*8 f(50),FR25,FR34,DMEDIAN,DFORM
      REAL*8 U1,U2,U3,U4,U5,U6,U7,U8,U9,U9A
      REAL*8 r25(8), r34(8)
      INTEGER ID


c              common SF_A provides DBHOB(ob), Total height and DBHOB(ib)
           

      data (F(I),I=13,15)/ -.31137977D+01, .11996084D+01, -.1195901D-01/        
      data (F(I),I=17,19)/ -.66829984D-01, .26990398D-01, .24661021D+00/   
      data (F(I),I=21,24)/ -.83415555D+01, .24274384D+01 ,
     >                     -.56918023D+01, .56487213D+00  /
      data (F(I),I=25,28)/ -.76464554D+01 , .51709049D+01 ,
     >                     -.27133381D+01 , .38918349D+00 / 
      data f(29)        /  -7.0d0 /  
      data (F(I),I=34,37)/ -.12882571D+01 , .35688410D+02 ,
     >                      .17995769D+00 , .15565605D+01 /    
      data (F(I),I=38,40)/.64397446D+01, -.13439736D+01, .63442558D+01/    
      data (F(I),I=42,44)/.11898092D+02, -.36789851D+01, .15168209D+00/ 
      data (F(I),I=45,47)/-.13248733D+01,-.11788962D+00 ,
     >                    -.15909154D-01 /

c                                                 Regional coefficients   
c              (alpha to numeric transform is in SF_2PT)
c     region order is:  CO  EV  NO  RS  SO  WE  WV  WC
c                       OR  OR  WA  WA  WA  WA  OR  WA
c             IREGION:   1   2   3   4   5   6   7   8
      DATA (WHSUB(I),I=1,8)/'01','02','03','04','05','06','07','08'/ 
c             Region 2 (Oregon West Valley) uses Region 3 (Or. EV) coef.
      data (r25(i),i=1,8)/  -7.511d0, -7.687d0, -7.224d0, 
     >              -7.355d0, -7.632d0, -7.646d0, -7.687d0, -7.911d0/
      data (r34(i),i=1,8)/ -1.215d0, -1.355d0, -1.177d0,
     >              -1.373d0, -1.398d0, -1.188d0, -1.355d0, -1.449d0 /

c                               Region modifications to coefficients
      FR25=f(25)
      FR34=f(34)
      if (GEOSUB.NE.'00') then
        DO 100, ID=1,8
          IF(WHSUB(ID).EQ.GEOSUB) THEN
             FR25=r25(ID)            
             FR34=r34(ID) 
          ENDIF
 100    CONTINUE
      endif       

      DMEDIAN = .2855*(HTTOT-4.5)**(.307 - .00505*HTTOT + 
     >                          .00001745*HTTOT*HTTOT+0.19*log(HTTOT))
      DFORM = DBHOB/DMEDIAN -1.0d0

c                                                                RHI1
      U7 = F(13) + f(14)*( 1.0d0 - exp(F(15)*HTTOT))                     
      if (U7.lt. -7.0d0) U7=-7.0d0                               
      if (U7.gt.  7.0d0) U7= 7.0d0                
      RHI1 =  REAL(dexp(U7) / ( 1.0d0 + dexp(U7) ))
      if (RHI1.gt. 0.5) RHI1=0.5
                             
c                                                                RHLONGI
      U9A = f(17) + f(18)*log(HTTOT) + f(19)*DFORM
       if (RHI1 + U9A  .gt. 0.75) U9A =  0.75-RHI1
       if(u9a.gt.0.) then
         u9 = u9a
       else
         u9 = 0.
       endif
c     U9 = MAX( U9A , 0.)            
c                                                                 RHC
      U8 = f(21)+f(22)*log(HTTOT)+f(23)*DFORM+f(24)*log(HTTOT)*DFORM  

c                                                         U1-U5      
      U1 = FR25  + f(26)/(HTTOT/100.0D0) + f(27)/(HTTOT/100.0D0)**2
     >           + f(28)/(HTTOT/100.0D0)**3
      U2 = f(29) 
      U3 = FR34  + f(35)/HTTOT + f(36)*HTTOT*DBHOB/1000.0 + f(37)*DFORM
      U4 = f(38) + f(39)*log(HTTOT) + f(40)*DFORM    
      U5 = f(42) + f(43)*log(HTTOT) + f(44)*DBHOB                         
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
      if (U5.gt.  7.0d0) U5= 7.0d0                                
      if (U8.lt. -7.0d0) U8=-7.0d0                               
      if (U8.gt.  7.0d0) U8= 7.0d0                               

c                                        U6=A3 (bounds: 1.005-100)
      U6 = f(45) + f(46)*log(DBHOB) + f(47)*HTTOT + f(48)*log(HTTOT)
      if (U6 .lt. -6.0) U6=-6.0
      if(U6 .gt. 6.0  ) U6=6.0
      U6 = 1.0D0 + exp( U6)      
         if(U6 .lt. 1.005d0) U6=1.005D0
         if(U6 .gt. 100.0d0) U6=100.0d0

c                                         Define geometric parameters
      R1= REAL(dexp(U1)/ (1.0d0 + dexp(U1)))                             
      R2= REAL(dexp(U2)/ (1.0d0 + dexp(U2)))                             
      R3= REAL(dexp(U3)/ (1.0d0 + dexp(U3)))                            
      R4= REAL(dexp(U4)/ (1.0d0 + dexp(U4)))                             
      R5= REAL(0.5d0 + 0.5d0*dexp(U5)/ (1.0d0 + dexp(U5)))                        

      A3=REAL(U6)

      RHLONGI= REAL(U9)    
      RHI2 = RHI1 + RHLONGI

      RHC = REAL(RHI2 + (1.0 - RHI2) * dexp(U8)/( 1.0d0 + dexp(U8)))

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
      SUBROUTINE SHP_W5(DBHOB,HTTOT,GeoSUB,RFLW,RHFW)
c         Stem form  SHAPE paraemeters for Red Cedar (English units)
c***********************************************************************
                   
c                      Coefficients for NW TAPER Coop  April, 1994
c                         (REGC  RC41494.coe)
c                       Copyright  J. W. Flewelling, 1994 

      IMPLICIT DOUBLE PRECISION (A-H, O-Z) 

      CHARACTER*2 GEOSUB
      CHARACTER*2 CDANUW
      
      REAL*4 RHI1, RHI2, RHC, RHLONGI                                   
      REAL*4 DBHOB, HTTOT
      REAL*4 R1, R2, R3, R4, R5, A3
      REAL*4 RFLW(6), RHFW(4) 
      REAL*8 U1,U2,U3,U4,U5,U6,U7,U8,U9,U9A
       
      real*8 f(50),DMEDIAN,DFORM

c              common SF_A provides DBHOB(ob), Total height and D=DBHOB(ib)
           

      data F(13),F(15)   /   -.11917515D+01 , .15632952D+00 /        
      data (F(I),I=17,18)/    .44805624D+00 ,  -.10269221D+00     /  
      data (F(I),I=21,22)/    .82814200D+00 ,  .17750205D+02      / 
      data (F(I),I=25,27)/ -.17959260D+2, .38308966D+01,.45163788D+02 /

      data (f(I),I=29,31)/ -.17546090D+01,-.79230813D+01,-.11253761D+03/    
      data (F(I),I=34,36)/ -.17943249D+00,.15534485D+00, -.32777026D-01/
      data (F(I),I=38,40)/.85305448D+01, -.83350599D+00 , .12100013D+02/ 
      data (F(I),I=42,43)/   .83021283D+01,  -150.0D0 /
      data (F(I),I=45,47)/ -.71594841D+01, .11263465D+00, .22396603D+02/ 
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW(1:2) = GEOSUB(1:2)
C
C
C
c                               Region modifications to coefficients

      DMEDIAN = .11*(HTTOT-4.5)**( 1.08 + 0.0006*HTTOT)
      DFORM = DBHOB/DMEDIAN -1.0d0

c                                                                RHI1
      U7 = F(13)  +F(15)*DFORM                    
       if (U7.lt. -7.0d0) U7=-7.0d0                               
       if (U7.gt.  1.0d0) U7= 1.0d0                
      RHI1 = REAL(dexp(U7) / ( 1.0d0 + dexp(U7) ))
                             
c                                                                RHLONGI
      U9A = f(17) + f(18)*log(HTTOT) 
      if (RHI1 + U9A  .gt. 0.75) U9A =  0.75-RHI1
        if(u9a.gt.0.) then
          u9 = u9a
        else
          u9 = 0.
        endif
c      U9 = MAX( U9A , 0.)                                               
c                                                                 RHC
      U8 = f(21)+f(22)/DBHOB                                           

c                                                         U1-U5
      U1 = f(25) + f(26)*log(DBHOB) + f(27)/DBHOB 
      U2 = f(29) +f(30)/DBHOB +f(31)/(DBHOB*DBHOB)  
      U3 = f(34) + f(35)*DBHOB    + f(36)*HTTOT  
      U4 = f(38) + f(39)*DBHOB + f(40)*DFORM  
      U5 = f(42) + f(43)/DBHOB                                         
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
      if (U5.gt.  7.0d0) U5= 7.0d0                                
      if (U8.lt. -7.0d0) U8=-7.0d0                               
      if (U8.gt.  7.0d0) U8= 7.0d0                               

c                                        U6=A3 (bounds: 1.005-100)
      U6 = f(45) + f(46)*DBHOB + f(47)/DBHOB
      if (U6 .lt. -6.0) U6=-6.0
      if(U6 .gt. 6.0  ) U6=6.0
      U6 = 1.0D0 + exp( U6)      
         if(U6 .lt. 1.005d0) U6=1.005D0
         if(U6 .gt. 100.0d0) U6=100.0d0

c                                         Define geometric parameters
      R1= REAL(dexp(U1)/ (1.0d0 + dexp(U1)))                             
      R2= REAL(dexp(U2)/ (1.0d0 + dexp(U2)))                            
      R3= REAL(dexp(U3)/ (1.0d0 + dexp(U3)))                            
      R4= REAL(dexp(U4)/ (1.0d0 + dexp(U4)))                            
      R5= REAL(0.5d0 + 0.5d0*dexp(U5)/ (1.0d0 + dexp(U5)))                        

      A3=REAL(U6)

      RHLONGI= REAL(U9)    
      RHI2 = RHI1 + RHLONGI

      RHC = REAL(RHI2 + (1.0 - RHI2) * dexp(U8)/( 1.0d0 + dexp(U8)))

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
      FUNCTION COR_WS(JSP,HTTOT, HI , HJ)
C********************************************************************
c                                                               CORR
c     given 2 heights (hi and hj), estimate the correlation of the
c       Z errors in the corresponding dibs                                      
c               NW Taper Coop  Western Hemlock (WH41194.coe)
c                      Copyright, 1994  James W. Flewelling
      REAL COR_WS
      INTEGER JSP,JSPR
      REAL BH,HTTOT,HI,HJ, CORR,H1,H2,T3,T4,T2,T1
      REAL QH(3,5),Q1,Q2,Q3,Q4,Q5
      
c     DATA F80,F81/ 2.013, .4345/
c       Douglas fir
      DATA (QH(1,I),I=1,5)/ -4.9012, 14.0602, -14.98, -1.813, 0.249/
C       western hemlock
      DATA (QH(2,I),I=1,5)/ -4.1915, 10.026,  -11.536, -1.3865, -.338/
c       western red cedar
      DATA (QH(3,I),I=1,5)/ -5.6689, 22.896, -28.335, -1.9173, .242/
      
      JSPR=JSP-2
      
      Q1=QH(JSP,1)
      Q2=QH(JSP,2)
      Q3=QH(JSP,3)
      Q4=QH(JSP,4)
      Q5=QH(JSP,5)
      
      BH = 4.5
c                          any error is completely correlated with itself
      if(hi.eq.hj ) then
           cor_ws=1.0
           return
        endif
c                               There are no errors at breast height (Ha!)
c                               Correlations are irrelevant.
      if(hi.eq.bh .or. hj.eq.bh) then
              cor_ws=0.5
              return
           endif

c                             define heights h1 and h2 with h2 > h1
      h1=min(hi, hj)
      h2=max(hi,hj)
      

      if(h1.gt. bh) then
c                                         both heights are above BH
       t3=(h1-bh)/(HTTOT-bh)
       t4=(h2-bh)/(HTTOT-bh)                                
       CORR = REAL(exp( q1*(t4-t3) + q2*(t4*t4-t3*t3)/2.0D0
     >                  +q3*(t4*t4*t4 - t3*t3*t3)/3.0D0))     
       GO TO 100
      endif

      if(h2.gt. bh)   then
c                                              h1 < Bh  and h2 > BH
         t3=(h2-bh)/(HTTOT-bh)                             
         T2 = (BH-H1)/BH    
         CORR = REAL(q5*exp( q4*t2 + q1*t3 +q2*t3*t3/2.0d0 
     >                    +q3*t3**3 / 3.0d0 ))       
         go to 100
        endif

c                                              h1 < Bh and H2 < Bh       
c                                              note: t2 < t1
       T2 = (BH-H2)/BH                                                   
       T1 = (BH-H1)/BH                                                   
       CORR=exp( q4* (t1-t2))        

100    COR_WS = CORR
      return
      END

C********************************************************************
C********************************************************************
      SUBROUTINE VAR_C1(JSP,DBHOB,HTTOT,H,DIBmod,DIBact,Z)
c                                                   SF_ZFD
C********************************************************************
c                                                   Find Z, given d.
C                 INTERNAL ROUTINES:  SF_ZDF3
C                                     SF_ZDF4
C                                     SF_ZDF5

c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      DIBact  In    Real*4    Actual dib
c      Z       out   Real*4    Standardized offset

      INTEGER JSP
      REAL DBHOB, HTTOT, H, DIBmod, DIBact, Z

      if(JSP.eq. 1  .or. jsp.eq.2) then
C            CALL  VAR_STD( h, SE )   
c            z =  (dibact - DIBmod ) / se
      else if(JSP.eq.3) then
            CALL sf_zfd3(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
      else if(JSP.eq.4) then
            CALL sf_zfd4(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
      else if(JSP.eq.5) then
            CALL sf_zfd5(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
      else
             Z=0.
      endif
      RETURN
      END 



C********************************************************************
C********************************************************************
      SUBROUTINE SF_ZFD3(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
c                                                   SF_ZFD
C********************************************************************
c                                                   Find Z, given d.
c        Douglas Fir,  NW Taper Coop   April, 1994  (CD17.coe)
c        Copyright    James W. Flewelling, 1994
c
c      CHANGES_____________________________________________________________
c      23 JUL 1997  Mixed-mode (MU + .0005) arithmetic corrected.

c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      DIBact  In    Real*4    Actual dib
c      Z       out   Real*4    Standardized offset  

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      REAL*4 H , DIBMOD, DIBACT, Z                                    

      REAL*4  DBHOB, HTTOT, BH
      REAL*8 MU,LAMDA,BL1,BL4,S11,S12,S13,S21,S22,S3,S4,S5,S6
      REAL*8 SU5,SU6,S7,S8,S9,X,T,ALHT,ALAMDA,TR2,DZ_DX
      REAL*8 YM,DELTA,GAMMA,Y,YLOGIT
      INTEGER IENTRY
                 
      DATA BL1, BL4    / 9.2484016d0 ,19.051579d0 /
      DATA MU          / 0.01D0 /
      DATA S11,S12,S13 / 1.18456d0, -.02638802d0, .4293105d0/
      DATA S21,S22     / -3.638135d0, .44202834d0 /
      DATA S3, S4      / -1.7685049d0, -.066272536d0 /
      DATA S5, S6      / -11.061295d0, -.010578261d0 /
      DATA S7, S8, S9  / -.29628457d0, -.43808609d0, 0.74422178d0 /
      DATA SU5,SU6     / -.25196078d0, 1.7192804d0/

      IENTRY=1  
c                                         EQN. 1   X                
       BH=4.5
       X = DIBact/DIBmod                                            
       IF (X.le.mu +0.0005D0) then
          x=mu+0.0005D0             
         endif 

             
20     IF (H.gt. BH) then
             T = (H-BH)/(HTTOT-BH) 
           else
             T = (BH-H)/BH
          endif  
       alht=log(HTTOT)                                
c                                         LAMDA and log(dZETA dX)
        IF (H.gt. BH) then
            ALAMDA =  SU5 + SU6*T*T            
            if (alamda.gt. 12.0D0)  alamda= 12.0D0
            if (alamda.lt. -12.0D0) alamda=-12.0D0 
            LAMDA = 1.0 - MU + exp(ALAMDA)
               
            TR2 = (s11 + s12*DBHOB   + s13*ALHT  )
     >          +  (s21 + s22*ALHT)*T
     >          +   s3*T**4  + s4*log(T)      
            else
c                                                 below BH
               ALAMDA = bl1 + BL4*T*T
               TR2 =   s5 + s6*HTTOT + (s7+S9*HTTOT**0.3)*(T)**s8  
               if (alamda.gt. 12.0D0)  alamda=12.0D0
               if (alamda.lt. 0.0D0) alamda=0.0d0 
               LAMDA = exp(alamda)
               if (TR2.gt.  12.0D0)      tr2= 12.0d0     
               if (TR2.lt. -25.0D0)      tr2=-25.0d0     
            ENDIF
      DZ_DX = exp(TR2) * LAMDA   
c                                        find gamma and delta
c                                          EQN 4   Y(median)   
      YM = (1.0D0-MU)/LAMDA
c                                          EQN 8, Solved for DELTA
      DELTA = DZ_DX * LAMDA / 4.0D0 * 
     >        (1.0D0 - (2.0d0*YM-1.0D0)*(2.0d0*YM-1.0D0) )

c                                          EQN 7 GAMMA
      GAMMA = - DELTA*log( YM/(1.0D0-YM))  
                  
      IF (IENTRY.eq.2) go to 120
c                              
c                                          EQN 2   Y
      Y=(X-MU)/LAMDA                                
      IF (Y.gt. 0.999999D0) then 
           Y=0.999999D0 
      ENDIF
c                                          EQN 3  ZETA   
      Z  = REAL(GAMMA + DELTA*LOG(Y/(1.0d0-Y)))

      RETURN

      ENTRY SF_DFZ3(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
c                                                   SF_ZFD
c                                                   Find Z, given d.

c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      Z       In    Real*4    Standardized offset  
c      DIBact  Out   Real*4    Actual dib

       IENTRY=2 
       BH=4.5
       if(abs(bh - h).lt. 1.0E-6) then
              DIBact=DIBmod
              RETURN
             ENDIF  
       GO TO 20
c                                          reverses the Sb transform
120    YLOGIT = (Z - GAMMA)/DELTA
       
       Y = exp(ylogit)/(1.0d0 + exp(ylogit))
                         
       X = MU + LAMDA*Y                                             
       DIBact = REAL(X * DIBmod)

      RETURN
      END             


C********************************************************************
C********************************************************************
      SUBROUTINE SF_ZFD4(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
c                                                   SF_ZFD
C********************************************************************
c                                                   Find Z, given d.
c        Western Hemlock,  NW Taper Coop   April, 1994  (WH41194.coe.coe)
c        Copyright    James W. Flewelling, 1994

c      CHANGES___________________________________________
C      22JUL 1997              Move test of X vs MU. 
C                              Note: old postion could have caused a problem
C                                    for non-Microsoft compilers. 
C                              Mixed mode arithmetic (R*4 + R*8 corrected).

c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      DIBact  In    Real*4    Actual dib
c      Z       out   Real*4    Standardized offset  

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      REAL*4 H , DIBMOD, DIBACT, Z                                    

      
      REAL*4  DBHOB, HTTOT, BH
      REAL*8  MU, LAMDA 
      REAL*8 A11,A12,A13,A21,A22,A31,A32,A33,A40,A41,A42
      REAL*8 S1,S3,S5,S6,S7,U11,U12,U2
      REAL*8 T,A1,A2,A3,A4,TR2,U1,DZ_DX,YM,DELTA,GAMMA,X,Y,YLOGIT
      INTEGER IENTRY
     
      DATA A11,A12,A13/ 3.7244760d0, -.64373853d0, -2.5045615d0 /
      DATA A21,A22    / -7.6871598d0, 0.030248851d0 /
      DATA A31,A32,A33/ 1.2762845d0, .44828840d0, -.50630122d-2 /
      DATA A40,A41,A42/ .032150116d0, 4.0216438d0, -4.8014107d0 /           
      DATA S1, S3/ 0.01d0, .69481696d0/
      DATA S5, S6/ .70394499d0, 5.4510380d0 /
      DATA S7    / 2.8470272d0 /
      DATA U11, U12, U2/ 3.4018968d0, -.88734933d0, -2.334093d0/

      IENTRY=1                                               
       BH=4.5      
20     IF (H.gt. BH) then
             T = (H-BH)/(HTTOT-BH) 
           else
             T = (BH-H)/BH
          ENDIF                                
c                                         LAMDA and log(dZETA dX)
        IF (H.gt. BH) then
            MU=S1
            LAMDA =  1.0D0 - MU   +exp(S5 +S6*T*T)
            A1= A11+A12*DLOG(DBLE(DBHOB)+1.0D0) + A13/DBLE(DBHOB)
            A2 = MIN( A21+A22*DBLE(DBHOB), 0.0d0)
            A3 = A31 + A32*LOG(DBLE(DBHOB)+0.5d0) +
     >           A33* LOG(DBLE(DBHOB)+0.5d0)*LOG(DBLE(DBHOB)+0.5d0)
            A4 = A40 + A41*exp(A42*DBHOB)
            TR2 = A1 + A2*T**A3 + A4/T
            else
c                                                 below BH 
               U1= U11*(1.0d0 - exp(U12*DBHOB))
               MU=S3
               LAMDA=S7
               TR2 = U1 +U2*T
            ENDIF
      if (TR2.lt. -12.0d0) TR2=-12.0d0
      if (TR2.gt.  12.0d0) TR2= 12.0d0
      DZ_DX = exp(TR2) * LAMDA   
c                                        find gamma and delta
c                                          EQN 4   Y(median)   
      YM = (1.0D0-MU)/LAMDA
c                                          EQN 8, Solved for DELTA
      DELTA = DZ_DX * LAMDA / 4.0D0 * 
     >        (1.0D0 - (2.0d0*YM-1.0D0)*(2.0d0*YM-1.0D0) )

c                                          EQN 7 GAMMA
      GAMMA = - DELTA*log( YM/(1.0D0-YM))  
                  
      IF (IENTRY.eq.2) go to 120
c                                                            
c                                         EQN. 1   X                
       X = DIBact/DIBmod                                            
       IF (X.le.mu +0.0005D0) then
          x=mu+0.0005D0             
         ENDIF                                              
c                                          EQN 2   Y
      Y=(X-MU)/LAMDA                                
      IF (Y.gt. 0.999999D0) then 
           Y=0.999999D0 
      ENDIF
c                                          EQN 3  ZETA   
      Z  = REAL(GAMMA + DELTA*LOG(Y/(1.0d0-Y)))

      RETURN

      ENTRY SF_DFZ4(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
c                                                   SF_ZFD
c                                                   Find Z, given d.

c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      Z       In    Real*4    Standardized offset  
c      DIBact  Out   Real*4    Actual dib

       BH = 4.5
       IENTRY=2 
       if(abs(bh - h).lt. 1.0E-6) then
              DIBact=DIBmod
              RETURN
             ENDIF  
       GO TO 20
c                                          reverses the Sb transform
120    YLOGIT = (Z - GAMMA)/DELTA
       Y = exp(ylogit)/(1.0D0 + exp(ylogit))           
       X = MU + LAMDA*Y                                             
       DIBact = REAL(X * DIBmod)

      RETURN
      END             


C********************************************************************
C********************************************************************
      SUBROUTINE SF_ZFD5(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
c                                                   SF_ZFD
C********************************************************************
c                                                   Find Z, given d.
c        Red Cedar,  NW Taper Coop   April, 1994  (RC41194.coe.coe)
c        Copyright    James W. Flewelling, 1994
C        CHANGES______________________________________________________
C      JUL 23, 1993         Move test of X vs MU
c                            (potential problem for compilers that do not
c                             initilize variables to zero.)
C                           Fix mixed mode arithmetic (R*4 + R*8)


c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      DIBact  In    Real*4    Actual dib
c      Z       out   Real*4    Standardized offset  

C      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      REAL*4 H , DIBMOD, DIBACT, Z                                    

      INTEGER IENTRY
      REAL*4  DBHOB, HTTOT, BH
      REAL*8  MU, LAMDA,A11,A12,A13,A21,A22,A31,A32,A33
      REAL*8  A40,A41,A42,S1,S3,S5,S6,S7,U11,U12,U2
      REAL*8  T,A1,A2,A3,A4,TR2,U1,DZ_DX,YM,DELTA,GAMMA,X,Y,YLOGIT

      DATA A11,A12,A13/4.3050830d0, -.85232440d0, -2.3355492d0 /
      DATA A21,A22    / -7.5573025d0, 0.011360349d0 /
      DATA A31,A32,A33/-1.9172059d0, 3.1382338d0, -.53258164d0/
      DATA A40,A41,A42/ .019259274d0, 4.0216439d0, -4.7142447d0 /           
      DATA S1, S3/ 0.01d0, .78308913d0/
      DATA S5, S6/ .21140396d0, 6.4129714d0 /
      DATA S7    / 2.03990921d0 /
      DATA U11, U12, U2/ 3.3372438d0, -4.0010883d0, -2.2145198d0/

      IENTRY=1  
      BH = 4.5       
20     IF (H.gt. BH) then
             T = (H-BH)/(HTTOT-BH) 
           else
             T = (BH-H)/BH
          ENDIF  
c                                         LAMDA and log(dZETA dX)
        IF (H.gt. BH) then
            MU=S1
            LAMDA =  1.0D0 - MU   +exp(S5 +S6*T*T)
            A1= A11+A12*DLOG(DBLE(DBHOB)+1.0D0) + A13/DBLE(DBHOB)
            A2 = MIN( A21+A22*DBLE(DBHOB), 0.0d0)
            A3 = A31 + A32*LOG(DBLE(DBHOB)+0.5d0) +
     >           A33*LOG (DBLE(DBHOB)+0.5d0) * LOG(DBLE(DBHOB)+0.5d0)
            A4 = A40 + A41*exp(A42*DBHOB)
            TR2 = A1 + A2*T**A3 + A4/T
            else
c                                                 below BH 
               U1 = U11*(1.0d0 - exp(U12*DBHOB))
               MU=S3
               LAMDA=S7
               TR2 = U1 +U2*T
            ENDIF
      if (TR2.lt. -12.0d0) TR2=-12.0d0
      if (TR2.gt.  12.0d0) TR2= 12.0d0
      DZ_DX = exp(TR2) * LAMDA   
c                                        find gamma and delta
c                                          EQN 4   Y(median)   
      YM = (1.0D0-MU)/LAMDA
c                                          EQN 8, Solved for DELTA
      DELTA = DZ_DX * LAMDA / 4.0D0 * 
     >        (1.0D0 - (2.0d0*YM-1.0D0)*(2.0d0*YM-1.0D0) )

c                                          EQN 7 GAMMA
      GAMMA = - DELTA*log( YM/(1.0D0-YM))  
                  
      IF (IENTRY.eq.2) go to 120
c                                                           
c                                         EQN. 1   X                
       X = DIBact/DIBmod                                            
       IF (X.le.mu +0.0005D0) then
          x=mu+0.0005D0             
         ENDIF 
                                                            
c                                          EQN 2   Y
      Y=(X-MU)/LAMDA                                
      IF (Y.gt. 0.999999D0) then 
           Y=0.999999D0 
      ENDIF
c                                          EQN 3  ZETA   
      Z  = REAL(GAMMA + DELTA*LOG(Y/(1.0d0-Y)))

      RETURN

      ENTRY SF_DFZ5(DBHOB,HTTOT,h,DIBMOD,DIBACT,Z)
c                                                   SF_ZFD
c                                                   Find Z, given d.

c      H       In    REAL*4    Section HTTOT (0<= h < HTTOT,  h not= BH)
c      DIBmod  In    Real*4    Base model prediction of dib (expectation)
c      Z       In    Real*4    Standardized offset  
c      DIBact  Out   Real*4    Actual dib

       BH = 4.5
       IENTRY=2 
       if(abs(bh - h).lt. 1.0E-6) then
              DIBact=DIBmod
              RETURN
             ENDIF  
       GO TO 20
c                                          reverses the Sb transform
120    YLOGIT = (Z - GAMMA)/DELTA
       Y = exp(ylogit)/(1.0D0 + exp(ylogit))           
       X = MU + LAMDA*Y                                             
       DIBact = REAL(X * DIBmod)

      RETURN
      END             


C********************************************************************
C********************************************************************
      FUNCTION FDBT_C1(JSP,GEOSUB,DBHOB,HTTOT)

C********************************************************************
c                   given Diameter at breat height, outside bark
c                   calculate double bark thickness at breast height.
c                   NW Taper Coop Doug Fir  (fit on APR 14, 1994)
c                   NW Taper Coop -  Western Hemlock  (fit on APR 14, 1994)
c                   NW Taper Coop -  Red Cedar  (fit on APR 14, 1994)
c                   COPYRIGHT J. Flewelling, 1994.  

c     DBHOB        INPUT     R*4  Diameter (o.b.) at Breast height (inches)
c     HTTOT         INPUT     R*4  Total tree height (ft)
c     FDBT_BH3   RETURNED  R*4  Double bark thickness
 
      CHARACTER*2 GEOSUB, GCODE(8)
      INTEGER JSP, ID
      real*4 ROFF(8),ROFF1(8), DMEDIAN, DFORM, RATIO
      REAL DBHOB,HTTOT,D_USE
      REAL FDBT_C1
     
      DATA (GCODE(I),I=1,8)/'01','02','03','04','05','06','07','08'/ 

      Data(ROFF(I),I=1,8)/
     >                 0.0, .117, .121, .133, .025, .028, .088, .028/
      Data (ROFF1(I),I=1,8)/
     >                 0.0, .118, .204, .105, .058, .061, .118, .071 /
c   Above offsets for   CO    EV    NO    RS    SO    WE    WV   WC(=WE)
c        note: WV has no hemlock data; uses EV coefficient

c      note:   English-only on input and output
                                                                 
      IF(JSP.EQ.3) THEN
        DMEDIAN = .566*(HTTOT-4.5)**( .634 + .00074*HTTOT)
        DFORM = DBHOB/DMEDIAN -1.0                                  

        if (GEOSUB.EQ.'00') then
           RATIO = exp( -2.4641 + .04393*log(DBHOB) -.2922*dform
     >                 +.05964 * DFORM*log(DBHOB))
        else
          DO 100, ID=1,8
            IF(GCODE(ID).EQ.GEOSUB) THEN
               RATIO = exp( -2.5087 + .03600*log(DBHOB) -.4086*dform
     >                 +.10120* DFORM*log(DBHOB) +Roff(id))
            ENDIF
  100     CONTINUE    
        ENDIF
        
        FDBT_C1 = RATIO*DBHOB

      ELSEIF(JSP.EQ.4) THEN
        if (GEOSUB.EQ.'00') then
           RATIO = .04504*(1.0 +0.8307*exp(-.2048*DBHOB))  
        else                                           
          DO 200, ID=1,8
            IF(GCODE(ID).EQ.GEOSUB) THEN
               RATIO = .04221*(1.0 +0.8836*exp(-.2145*DBHOB))  
     >            *(1.0 + ROFF1(ID))
            ENDIF
  200     CONTINUE
        ENDIF
C        write(*,*)GEOSUB,DBHOB,ratio,DBHOB*ratio
        FDBT_C1 = RATIO*DBHOB

      ELSEIF(JSP.EQ.5) THEN
       
        D_USE = MAX(DBHOB, 3.8)

        RATIO = 0.01949*( 1.0 + 15.599/D_USE -29.212/D_USE**2)
        
        FDBT_C1 = RATIO*DBHOB
        
      ELSE
        FDBT_C1 = 0

      ENDIF
      
      RETURN
      END 


C********************************************************************
C**********************************************************************
      FUNCTION BRK_WS(JSP,DBHOB,HTTOT,DBTBH,h)

C********************************************************************
c                      given a section height (h), calculate the ratio:
c                            DBT/DOB
c                      for Red Cedar.  NW Taper Coop  4/15/94
c                      copyright, James Flewelling  1994
      integer jsp
      real DBHOB,HTTOT,DBTBH,h
      real a1,a2,a3,a4,b2,FACTOR
      real c1,c2,c3,c4,R1,T,H75,R
      REAL E0,E1,E2,T1,TBH,X,XBH
      REAL BRK_WS      
      data a1, a2, a3,  a4, b2/ 1.8773 , -3.15524, -.06725 , 0.68229 ,
     1                          0.2553 /

      data C1, C2, C3,  C4/ 1.1309 , -2.021, -.03323 , 0.8167 /

      IF(JSP.EQ.3) THEN
c      Theory: t is a monotonic transformation of h, goverened by parm B2.
c              r(t) is a quadratic function that takes on the value 1 at BH.
c              t1 is the value of t where r(t) is at a minimum.
c              r1  = r(t1)
c              adjusted r = specified above + correction in upper 25% of stem. 
c              DBT/DOB  = [ DBT/DOB at breast height] * [ adjusted r]

c      write(*,*)' ws',DBHOB,HTTOT,h
        E2 = .51705 + 0.483*exp(-.251*DBHOB)
        T1 = .6406 + 0.0512*(1. - exp(-.0201*DBHOB))
        TBH =(4.5/HTTOT)**E2
        R1 = .6294 + 0.7901*exp(-0.3*DBHOB)
        IF (R1.gt. 0.99)  R1=0.99
        E1 = (R1-1)/(-2*t1*(T1-TBH) + (T1*T1 -TBH*TBH))
        E0 = -2*t1*E1  
        if(h.le.0.0d0) then 
            T=0.
           else
            T = (h/HTTOT)**E2
           ENDIF
        H75 = MAX( 0.75*HTTOT, 4.5 + 0.5*(HTTOT - 4.5))
       
        R = 1.0 + E0*(T-tbh)  + E1*(T*T - TBH*TBH)

        If (H.gt. H75)  R = R + 0.5983*( (H-H75)/(HTTOT-H75))**2

        BRK_WS = DBTBH / DBHOB * R
            
      ELSEIF(JSP.EQ.4) THEN
        x=h/HTTOT
        xbh=4.5/HTTOT

        factor = 1.0+(C1 + C2*exp(C3*DBHOB))*(x-xbh) + C4*(x*x-xbh*xbh)
C        DBTBH = DBHOB - dbh_ib
        BRK_WS = (DBTBH / DBHOB) * factor

      ELSEIF(JSP.EQ.5) THEN
        if (h.gt.0.0) then
          x=(h/HTTOT)**B2
         else
          x=0.0
         ENDIF
        xbh=(4.5/HTTOT)**B2

        factor = 1.0+(a1 + a2*exp(a3*DBHOB))*(x-xbh) + a4*(x*x-xbh*xbh)
c       DBTBH = DBHOB - dbh_ib
        BRK_WS = (DBTBH / DBHOB) * factor
  
      ENDIF
      return
      end

