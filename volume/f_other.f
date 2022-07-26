!== last modified  09-14-2007
C  FILE CONTAINS THE FOLLOWING ROUTINES:
C         BRK_OT
C         SHP_OT
C         COR_OT
C         VAR_OT
C         SHP_BH
C         COR_BH
C         VAR_BH
c***********************************************************************
      SUBROUTINE BRK_OT(JSP,geosub,DBHOB,DOB,HT2,DBTBH,DIB,dbt)
c***********************************************************************
c     Calculates double bark thickness for PP on the BHNF, PP on the SJNF, ES on the DNF
c     Variables passed:
c         DBHOB     real    in  diameter breash HTTOT
c         THT     real    in  height of measurement point
c         DOB     real    in  diameter at measurement point
c         DIB     real    out calculated diameter inside bark
c         DBTBH     real    bark thickness ratio (optional input)

      character*2 geosub
      INTEGER JSP,JSPR
      REAL DBHOB, DOB, HT2, DBTBH, DIB, DBHIB,dbt
      REAL*8 BK(9,8)
      REAL*8 DR, PY, B2, B3, B5, C1, C2, C3, D0, D1, D2, CLX
c         Use Coeffs developed using stump and > BH models;
c*********************************************************************
c                                       BLACK HILLS PP MODEL
      DATA (BK(I,1),I=1,9)/-0.310745,-5.267465, 4.056924,
     >            1.1159037603, -0.082066096, 0.424652164,
     >            0.699678, 0.067394, 0.0/
c                                       SAN JUAN PP MODEL;
      DATA (BK(I,2),I=1,9)/12.88990159, 17.90876955,
     >        0.05237532, 0.0145082565,  0.0027058753,
     >        0.1022613683, 0.702268,    0.050063, 0.0/
c                                       DIXIE ES MODEL;
      DATA (BK(I,3),I=1,9)/-0.8527459114, 0.9336248438,
     >        0.2809059946, 0.0029765304, 0.0535125376,
     >        0.3088136745, 0.7630215, 0.05957, 0.0/
c                                       R2 LODGEPOLE MODEL;
      DATA (BK(I,4),I=1,9)/-4.053133738, -3.891047743,
     >        0.380866177,  0.7617392463, 0.2071264529,
     >        0.6491861037, 0.8793398, 0.03020202, 0.0/
c                                       R2 DOUGLAS FIR MODEL
      DATA (BK(I,5),I=1,9)/  2.0518, 0.5569, 0.2009,
     >                       1.1163, 0.2194, 0.2525,
     >                      -0.121321,  0.861817, 0.0/
c                                       R2 WHITE FIR MODEL
      DATA (BK(I,6),I=1,9)/-0.3114, -0.5979,-0.00029,
     >                      0.4661,  0.2448, 0.1619,
     >                      -0.155160, 0.875930, 0.0/

c                                       R2 ASPEN
      DATA (BK(I,7),I=1,9)/20.1848, 111.927,0.0808502,
     >                      2.24620, 0.336067, 0.486677,
     >                      0.0142862, 0.919056, 0.0/
c                                      R3 PONDEROSA PINE
      DATA (BK(I,8),I=1,9)/ 2.67998, 3.49916, 0.111418,
     >                      2.28510, 0.374458, 0.450724,
     >                      -0.0113180, 0.801889, 0.00308737/
      JSPR = JSP-21
      B2 = BK(1,JSPR)
      B3 = BK(2,JSPR)
      B5 = BK(3,JSPR)
      C1 = BK(4,JSPR)
      C2 = BK(5,JSPR)
      C3 = BK(6,JSPR)
      D0 = BK(7,JSPR)
      D1 = BK(8,JSPR)
	D2 = BK(9,JSPR)

c**********************************************************************
C      IF(HT2.LT.(THT-2.0)) THEN
C       HTTOT = THT 
c   use wensels bark model
      IF (DOB.GT.0.0) THEN
           DR = DOB/DBHOB
      ELSE
         DOB = 0.0
         DR = 0.0
      ENDIF
c         if DBTBH is missing, then calculate
      IF (DBTBH.LE.0) THEN
        IF(JSPR.LE.1) THEN
c                      Black Hills
           DBHIB = DBHOB*(D0+D1*ALOG(DBHOB))
        ELSE IF(JSPR.EQ.2) THEN
c                      San Juan Model
           IF(GEOSUB.EQ.'07') THEN
C                 Use Dixie bark model           
               DBHIB = 0.4065547+0.7794452*DBHOB+0.0035815*DBHOB*DBHOB
C               DBHIB = -0.789251+0.922976*DBHOB
           ELSEIF(GEOSUB.EQ.'13')THEN
C                 Use San Juan bark model 
               DBHIB = DBHOB*(D0+D1*ALOG(DBHOB))
           ELSE IF(GEOSUB.EQ.'01') THEN
C                 Use A-S, Gila bark model           
               DBHIB = -0.649171195+0.925582344*DBHOB
           ELSE
C                 Use region wide bark model
               DBHIB = -1.024742 + 0.933772*DBHOB
           ENDIF
        ELSE IF(JSPR.EQ.3) THEN
c                      Dixie Model
           DBHIB = DBHOB*(D0+D1*ALOG(DBHOB))

        ELSEIF (JSPR.EQ.4) THEN
c                            R2 Lodgepole model
           IF(GEOSUB.EQ.'02') THEN
               DBHIB = DBHOB*(0.925915+0.0153361 * alog(DBHOB))
           ELSE
C                          Northern Wyoming Bark Model
               DBHIB = DBHOB*(D0 + D1*ALOG(DBHOB))
           ENDIF
        ELSE IF(JSPR.EQ.5)THEN
C                     Douglas Fir
               DBHIB = D0 + D1*DBHOB
        ELSE IF(JSPR.EQ.6)THEN
C                     White fir
               DBHIB = D0 + D1*DBHOB
        ELSE IF(JSPR.EQ.7)THEN
c                     Aspen Model
               DBHIB = D0 + D1*DBHOB
        ELSE IF(JSPR.EQ.8)THEN
C                     r3 ponderosa pine
               DBHIB = D0 + D1*DBHOB + D2*DBHOB*DBHOB
        ENDIF
        DBTBH = DBHOB - DBHIB
      ELSE
        DBHIB = DBHOB - DBTBH
      ENDIF

c         Use Wenzel modified hyperbolic model for greater than BH
C     IF(JSPR.EQ.5)THEN

CELSE
      IF (HT2.GT.4.5) THEN
         IF(DR.GT.0.0) THEN
            PY = (DR * ((B2-1)/(B2-DR**B3))) - ((DR**B5 -1)/DBTBH)
         ELSE
            PY = 0.0
         ENDIF
c         at BH, use what was given or calculated
      ELSE IF (HT2 .EQ. 4.5) THEN
         PY = 1
c         Use Wenzel non-lin stump model
      ELSE 
         CLX = C1*(DR-1)
         IF (CLX.GE.0) THEN
            PY = 1+ (CLX)**(C2+C3*DBTBH)
         ELSE
            PY=1
         ENDIF
      ENDIF
C     ENDIF

      DBT = PY * DBTBH

      DIB = DOB - DBT
         
c      write(*,*)jsp,dbtbh,dbt,ht2
      IF(DIB.LT.0.0) DIB = 0.0

      RETURN
      END


c***********************************************************************
c***********************************************************************
      SUBROUTINE SHP_OT(JSP,DBHOB,HTTOT,RFLW,RHFW)
c                Stem form SHAPE parameters for San Juan PP, Dixie ES
c***********************************************************************

c                         added by K.Cormier March 1997
c                               original code by
c                         J. W. Flewelling, July 1996 
c                                   NOTE:  F(49) reference  REMOVED

      IMPLICIT DOUBLE PRECISION (A-H, O-Z) 

      INTEGER JSP, JRSP

      REAL*8 F(50,7),DMEDIAN,DFORM
      REAL*4 RFLW(6),RHFW(4)
      REAL*4 RHI1, RHI2, RHC, RHLONGI                                   
      REAL*4 DBHOB, HTTOT         
      REAL*8 U7,U9,U8,U1,U2,U3,U4,U5,U6,U9T
      REAL*4 R1, R2, R3, R4, R5, A3
           
C     Ponderosa Pine - San Juan     
      DATA (F(I,1),I=10,47)/0.66384514D+01, 0.10000000D-01,
     >      0.29774591D-03,-0.18446658D+01,-0.55118989D-01, 
     >     -0.74385560D-01, 0.30000000D+00, 0.00000000D+00,
     >     -0.17255592D+02, 0.51670207D+01,-0.25236591D+01, 
     >      0.98999999D+00,-0.57094504D-03, 0.00000000D+00, 
     >      0.00000000D+00,-0.51665343D+01, 0.45788409D+00,
     >      0.19458313D+01,-0.37597099D+00, 0.56129833D+01,
     >      0.87688658D+00, 0.19543242D+01, 0.33588504D+01, 
     >     -0.19439190D+01, 0.24023940D+01, 0.11663144D+01,
     >     -0.79579201D+00,-0.23476445D+00, 0.70000000D+01,
     >     -0.56845618D+01,-0.34524730D+01, 0.82674905D+00,
     >      0.28111083D+04,-0.65957263D+03, 0.0,
     >      0.13536317D+01, 0.12522892D+00,-0.10620164D+00/

C     Engelmann Spruce - Dixie
      DATA (F(I,2),I=10,47)/0.21020000D+00, 0.91540000D+00, 
     >      0.37900000D-03, 0.83407587D-01,-0.47540269D+00,
     >      0.00000000D+00, 0.00000000D+00, 0.0,
     >      0.47833781D+01,
     >      0.00000000D+00, 0.00000000D+00, 0.15166254D+01,
     >      0.28673546D-02,-0.18629429D+00,-0.21157380D-01,
     >     -0.27327897D+01,-0.55096620D+00, 0.15292779D+01,
     >      0.11925600D+00,-0.28241505D+01, 0.19292205D+01,
     >      0.00000000D+00, 0.00000000D+00, 0.00000000D+00, 
     >     -0.15111026D+01, 0.16888014D+00, 0.78151554D-01,
     >      0.25617800D+00,-0.21827157D+03, 0.41820883D+03,
     >     -0.95642541D+02, 0.00000000D+00, 0.70100000D+01,
     >      0.00000000D+00, 0.0,            0.10050006D+01, 
     >      0.16477663D-03, 0.00000000D+00/

C     Region 2 Lodgepole Pine  
      DATA (F(I,3),I=10,47)/ 0.12277881D+00, 0.10000000D+01,
     >      0.13175957D-03,-0.21710377D+01,-0.44758909D-02,
     >     -0.59374635D+00, 0.30000000D+00, 0.0,
     >     -0.49070444D+01,
     >      0.83648614D+01, 0.35928132D+01, 0.99000000D+00,
     >      0.00000000D+00, 0.0,              0.0,
     >      0.39980891D+01,-0.14697324D+01,-0.66686989D+01,
     >      0.14455750D+01, 0.14036713D+03,-0.11691175D+03,
     >     -0.21054623D+02, 0.52685501D+02,-0.75914201D+01,
     >     -0.13026460D+01, 0.51614127D+00,-0.75910037D+00,
     >     -0.37292625D-02,-0.26357015D+01, 0.86492252D+01,
     >     -0.43578083D+01,-0.13528119D+02, 0.71000000D+01,
     >      0.00000000D+00, 0.0,            0.44900047D+03,
     >     -0.27127806D+03,-0.10709067D+03/

C     Region 2 Douglas Fir
      DATA (F(I,4),I=10,47)/ 0.35453368D+01,   0.11656118D+00,
     >     0.12768626D-02,  -0.82336483D+00,  -0.18045135D+00,
     >    -0.10960950D+00,   0.12065535D+00,   0.00000000D+00,
     >    -0.41477138D+02,   0.10000000D+02,   0.41373390D+01,
     >     0.84912210D+00,   0.73487234D-02,   0.00000000D+00,
     >     0.00000000D+00,  -0.67146666D+00,  -0.90296321D+00,
     >     0.28985499D+01,  -0.44090573D+00,   0.12308011D+01,
     >     0.19896131D+01,  -0.74943659D+00,   0.42028371D+00,
     >    -0.20747272D+00,   0.44655919D+01,   0.41265480D+00,
     >    -0.13682142D+01,   0.50005514D-01,  -0.49809702D+01,
     >     0.14255895D+02,   0.33688349D+01,  -0.11815386D+01,
     >     0.26327501D+02,  -0.42846455D+01,   0.00000000D+00,
     >     0.93254686D+00,   0.31997936D-01,   0.24617779D-01/

C     Region 2 White Fir
      DATA (F(I,5),I=10,47)/ 0.73716080D+00,   0.68027288D+00,
     >     0.00000000D+00,  -0.41913785D+01,   0.40269673D+00,
     >     0.76854192D+00,   0.30000000D+00,   0.00000000D+00,
     >     0.12084891D+04,  -0.23035742D+03,  -0.14224932D+04,
     >     0.99000000D+00,   0.24609061D+01,   0.16246653D+00,
     >    -0.21733263D+00,   0.45308167D+01,  -0.24261969D+01,
     >    -0.80556820D+01,   0.26254201D+01,   0.26041993D+02,
     >    -0.62710195D+02,  -0.66443455D+01,   0.17177653D+02,
     >    -0.30576099D+00,  -0.80175217D+00,   0.36295626D+01,
     >     0.45934604D-01,  -0.85721306D+00,  -0.70000000D+01,
     >    -0.56835846D-05,   0.10025049D-04,  -0.33941424D-02,
     >     0.70100000D+01,   0.00000000D+00,   0.00000000D+00,
     >     0.11178757D+01,   0.33317466D-01,  -0.24842882D-01/

c     Region 2 Aspen
      DATA (F(I,6),I=10,47)/ 0.36691430D+01,   0.18992789D+00,        
     >     0.33176964D-04,   0.20425823D+01,  -0.91938264D+00,        
     >     0.69861030D+00,   0.17854632D+00,   0.00000000D+00,
     >    -0.21631789D+02,   0.81713597D+01,  -0.65661454D+00,
     >     0.84912210D+00,   0.73487234D-02,   0.00000000D+00,
     >     0.00000000D+00,   0.76233739D+01,  -0.30679918D+01,
     >    -0.90811463D+01,   0.22859620D+01,  -0.24531176D+01,
     >    -0.19210099D+02,  -0.26435859D+02,  -0.21150535D+02,
     >     0.13348151D+02,  -0.67819116D+00,   0.24581387D+00,
     >    -0.71930984D-01,   0.21472602D-01,  -0.70000000D+01,
     >    -0.31137219D+02,  -0.85945910D+01,   0.49707724D+01,
     >     0.50837363D+00,   0.19343103D+01,   0.00000000D+00,
     >     0.10000000D+01,   0.96689437D+00,  -0.25611878D+00/
C     Ponderosa Pine - Region 3
      DATA (F(I,7),I=10,47)/ 0.92005972D+01,   0.39365313D-01,
     >     0.29661933D-02,  -0.14129679D+01,  -0.39945941D+02,
     >    -0.34558457D+02,   0.29799395D+00,   0.00000000D+00,
     >    -0.20514434D+02,   0.55752354D+01,  -0.11620111D+02,
     >     0.99000000D+00,   0.99999933D+00,   0.00000000D+00,
     >     0.00000000D+00,  -0.30107023D+01,  -0.65811245D+00,
     >     0.89956194D+01,  -0.21424741D+01,  -0.70000000D+01,
     >    -0.31755663D+02,  -0.25160313D+01,   0.85136197D+01,
     >    -0.81654808D+00,   0.23973015D+01,  -0.17097653D+01,
     >    -0.56230536D+00,   0.91076182D+00,  -0.70000000D+01,
     >     0.00000000D+00,   0.00000000D+00,  -0.25306440D-01,
     >     0.70100000D+01,   0.00000000D+00,   0.00000000D+00,
     >     0.11301947D+01,  -0.10235126D-01,  -0.25704531D-01/

c                                            Set counter for regional adjs
c                               Check for regional modifications requested
      JRSP=JSP-22

      DMEDIAN = f(10,JRSP) *(HTTOT-4.5)**(f(11,JRSP)+f(12,JRSP)*HTTOT)
      DFORM = DBHOB/DMEDIAN -1.0d0
c                                                                RHI1
      U7 = F(13,JRSP) + f(14,JRSP)*log(HTTOT) + f(15,JRSP)*DFORM 
c                                                                RHLONGI
      U9T = f(18,JRSP) + f(19,JRSP)*log(HTTOT) +f(20,JRSP)*DFORM
      if (U9T .lt. -7.0d0) U9T=-7.0d0    
      if (U9T .gt.  7.0d0) U9T= 7.0d0    
      U9 = f(16,JRSP)* exp(U9T)/(1.0d0 +exp(U9T) )
c                                                                 RHC
      U8 =f(21,JRSP)+f(22,JRSP)*HTTOT+f(23,JRSP)*log(HTTOT)+
     >                      f(24,JRSP)*DFORM

c                                                         U1-U5
      U1 = f(25,JRSP) + f(26,JRSP)*log(HTTOT) +f(27,JRSP)*DFORM 
     >                + f(28,JRSP)*DFORM*log(HTTOT)
      U2 = f(29,JRSP) + f(30,JRSP)*DFORM + f(31,JRSP)*log(HTTOT)
     >                + f(32,JRSP)*dform*log(HTTOT)
     >                + f(33,JRSP)*DBHOB                                               
      IF(JRSP .eq. 3) then
c                                                Lodgepole Pine (REGION 2)
        U3 = f(34,JRSP) + f(35,JRSP)*DFORM
     >     + f(36,JRSP) * (1.0D0 - exp(f(37,JRSP)*HTTOT) )   
      else
        U3 = f(34,JRSP) + f(35,JRSP)*DFORM + f(36,JRSP)*log(HTTOT) 
     >     + f(37,JRSP)*log(HTTOT)*DFORM
      endif
      U4 = f(38,JRSP) +f(39,JRSP)*DFORM +f(40,JRSP)*log(HTTOT) 
     >     +f(41,JRSP)*DBHOB
      U5 = f(42,JRSP) + f(43,JRSP)*log(HTTOT)        
c                                        U6=A3 
      U6 = f(45,JRSP) + f(46,JRSP)*DFORM  +f(47,JRSP)*log(HTTOT)


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
      R1= dexp(U1)/ (1.0d0 + dexp(U1))                             
      R2= dexp(U2)/ (1.0d0 + dexp(U2))                             
      R3= dexp(U3)/ (1.0d0 + dexp(U3))                             
      R4= dexp(U4)/ (1.0d0 + dexp(U4))                             
      IF (U5 .le. 7.0d0) then
          R5= 0.5d0 + 0.5d0*dexp(U5)/ (1.0d0 + dexp(U5))                        
        else
          R5=1.0
        endif

      A3=U6
      RHI1 =  dexp(U7) / ( 1.0d0 + dexp(U7) )
      if (RHI1.gt. 0.5 ) RHI1=0.5 

      RHLONGI= U9    
      RHI2 = RHI1 + RHLONGI

c      RHC = RHI2 + (1.0D0 - RHI2) * dexp(U8)/( 1.0d0 + dexp(U8))
c      If (RHC .lt. RHI2 +0.01) then
c          RHC = MIN( RHI2+0.01, (RHI2+1.0)/2.0 )
c        endif
      RHC=U8 
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
      FUNCTION COR_OT(JSP,HTTOT,HI,HJ)
c                                                               CORRC2
c***********************************************************************
c     given 2 heights (hi and hj), estimate the correlation of the
c       Z errors in the corresponding dib's
c               NW Taper coop #2, (INGY, east-side 1995-1996)

      REAL COR_OT,CORR
      REAL*8 V(5,7), Q1,Q2,QS,Q4,Q5,Q3
      REAL HTTOT, HI, HJ,BH,H1,H2,T3,T4,T2,T1
      INTEGER JSPR,JSP

C     COEF:  81-85
C     PONDEROSA PINE (SAN JUAN NF)
      DATA (V(I,1),I=1,5)/-0.37047662D+01, 0.36383106D+01,
     >                    -0.24995390D+01,-0.20054612D+01,
     >                    -0.34460970D+00/

C     ENGELMANN SPRUCE (DIXIE NF)
      DATA (V(I,2),I=1,5)/-0.54787570D+01, 0.96890017D+01,
     >                    -0.53401299D+01,-0.13433096D+01,
     >                    -0.20405779D+00/

C     REGION 2 LODGEPOLE PINE
      DATA (V(I,3),I=1,5)/-0.47777690D+01, 0.12360133D+02,
     >                    -0.76787940D+01,-0.15295707D+01,
     >                    -0.16751541D+00/

C     REGION 2 DOUGLAS FIR
      DATA (V(I,4),I=1,5)/-0.24676508D+01,-0.18606299D+01,
     >                    -0.45496728D+01,-0.12369208D+01,
     >                     0.19863544D+00/

C     REGION 2 WHITE FIR
      DATA (V(I,5),I=1,5)/-0.12659920D+02, 0.41776970D+02,
     >                    -0.12349876D+02,-0.13485158D+01,
     >                    -0.97359467D+00/

C     REGION 2 ASPEN
      DATA (V(I,6),I=1,5)/-0.24901327D+01, 0.18981262D+01,
     >                   -0.34884538D+01,-0.13900552D+01,
     >                    0.14103043D-02/

C     REGION 3 PONDEROSA PINE
      DATA (V(I,7),I=1,5)/ -0.81723321D+01,  0.19235015D+02,
     >                    -0.63296283D+01, -0.23078864D+01,
     >                     0.20674878D+00/

      BH = 4.5
      JSPR = JSP - 22
      Q1=V(1, JSPR)
      Q2=V(2, JSPR)
      QS=V(3, JSPR)
      Q4=V(4, JSPR)
      Q5=V(5, JSPR)

      Q3 = QS - (Q1+Q2)

c                          any error is completely correlated with itself
      if(hi.eq.hj ) then
           cor_ot=1.0
           return
        endif
c                               There are no errors at breast height (Ha!)
c                               Correlations are irrelevant.
      if(hi.eq.bh .or. hj.eq.bh) then
              cor_ot=0.5
              return
           endif

c                             define heights h1 and h2 with h2 > h1
      h1=min(hi, hj)
      h2=max(hi,hj)
      

      if(h1.gt. bh) then
c                                         both heights are above BH
       t3=(h1-bh)/(HTTOT-bh)
       t4=(h2-bh)/(HTTOT-bh)                                
       CORR = exp( q1*(t4-t3) + q2*(t4*t4-t3*t3)/2.0D0
     >                  +q3*(t4*t4*t4 - t3*t3*t3)/3.0D0)     
       GO TO 100
      endif

      if(h2.gt. bh)   then
c                                              h1 < Bh  and h2 > BH
         t3=(h2-bh)/(HTTOT-bh)                             
         T2 = (BH-H1)/BH    
         CORR = q5*exp( q4*t2 + q1*t3 +q2*t3*t3/2.0d0 
     >                    +q3*t3**3 / 3.0d0 )       
         go to 100
        endif

c                                              h1 < Bh and H2 < Bh       
c                                              note: t2 < t1
       T2 = (BH-H2)/BH                                                   
       T1 = (BH-H1)/BH                                                   
       CORR=exp( q4* (t1-t2))        

C100    IF (  abs(CORR) .gt. 1.0  .and.  ier_UNIT.ne.0) 
C     1     write(IER_UNIT,102) JSP, H1,H2, CORR
C102        FORMAT(' CORRC2 ERROR on JSP=',i3,'  H1,H2, corr =',3f8.2)

100   COR_OT = CORR

       return
       END

c***********************************************************************
c***********************************************************************
      SUBROUTINE VAR_OT(JSP,DBHOB,HTTOT,H,SE_LNX)
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

      REAL*8 F(13,7), V(20,7),DMEDIAN,DRATIO 
      REAL*4 LOGHT, LVARHAT, VARHAT,SE_LNX
      REAL DBHOB, HTTOT, BH, H 
      REAL*4 VA00,VA01,VA02,VB00,VB01,VC0,VX1,VX2,VX3,VA03
      REAL*4 VE00,VE01,VE02,VF00,VF01,VG0,VA0,VB0,VE0,VF0,VC,X
           
      INTEGER JSP,JSPR

c                                   Note: the following assignments are
c                                   for clarity. Faster execution would
c                                   be obtained by using the F() coefients
c                                   directly, and omitting the V%%% variables.
C     COEF: 61-76
C     SAN JUAN PONDEROSA PINE
      DATA (F(I,1),I=10,12)/0.66384514D+01, 0.10000000D-01,
     >               0.29774591D-03/
      
      DATA (V(I,1),I= 1,16)/  -0.11491009D+02,
     >                0.95333994D+00, 0.16501845D+01, 0.11286695D+02,
     >               -0.17267989D+01, 0.00000000D+00, 0.21717935D+00,
     >                0.79497708D-01, 0.10000000D+01,-0.36608798D+00,
     >               -0.66158695D+01,-0.36269972D+00,-0.14169144D+00,
     >                0.35434282D+01,-0.22939660D+00, 0.13578551D+01/
 

C     DIXIE ENGELMANN SPRUCE
      DATA (F(I,2),I=10,12)/0.21020000D+00, 0.91540000D+00, 
     >                0.37900000D-03/
      
      DATA (V(I,2),I= 1,16)/-0.11631559D+02, 0.10438290D+01,
     >                0.21132184D+01, 0.29726674D+01, 0.38794708D+00,
     >                0.84852726D+00, 0.25705963D-01, 0.84286599D-01, 
     >               -0.10000000D+01,-0.41862718D+00,-0.61674503D+01,
     >               -0.25117268D+00,-0.10512011D-01, 0.51224598D+01,
     >               -0.76280178D+00, 0.14363734D+01/

      DATA (F(I,3),I=10,12)/  0.12277881D+00, 0.10000000D+01,
     >                        0.13175957D-03/

      DATA (V(I,3),I= 1,16)/ -0.15550680D+02, 0.18607266D+01,
     >                 0.53949081D+01, 0.34883785D+01, 0.25891538D+00,
     >                 0.00000000D+00, 0.18218388D+00, 0.77716059D-01,
     >                -0.10000000D+01,-0.12490343D+01,-0.17214291D+01,
     >                -0.14819004D+01,-0.14800926D+00,-0.37137885D+01,
     >                 0.16299145D+01, 0.17250455D+01/

C     REGION 2 DOUGLAS FIR
      DATA (F(I,4),I=10,12)/ 0.35453368D+01, 0.11656118D+00,
     >                       0.12768626D-02/
      
      DATA (V(I,4),I= 1,16)/-0.24105465D+01,
     >              -0.12642787D+01,-0.27874506D+01, 0.11686963D+02,
     >              -0.18509747D+01, 0.17764737D-01, 0.27313143D+00,
     >               0.25755302D-01, 0.10000000D+01, 0.80356609D+00,
     >              -0.12781743D+02, 0.15577218D+01,-0.35755969D+00,
     >               0.11889490D+02,-0.24101283D+01, 0.24070251D+01/

C     REGION 2 WHITE PINE
      DATA (F(I,5),I=10,12)/ 0.73716080D+00, 0.68027288D+00,
     >                       0.00000000D-00/
      
      DATA (V(I,5),I= 1,16)/ 0.64719863D+01,
     >              -0.35842944D+01,-0.81824294D+01,-0.41853414D+01,
     >               0.21790612D+01, 0.15211185D+01,-0.10000000D+00,
     >               0.66609003D-01,-0.10000000D+01, 0.22112781D+01,
     >              -0.34405197D+01,-0.11414631D+01, 0.12167945D+00,
     >              -0.40868589D+01, 0.24337754D+01, 0.58663676D+01/

C     REGION 2 ASPEN
      DATA (F(I,6),I=10,12)/ .36691430D+01,  .18992789D+00,        
     >                       .33176964D-04/
      
      DATA (V(I,6),I= 1,16)/ -0.16511297D+01,
     >              -0.13388430D+01,-0.21100633D+01,-0.71713056D+00,
     >               0.13137397D+01, 0.70666496D+00, 0.33437187D+00,
     >               0.46455296D-01,-0.10000000D+01, 0.64999079D+00,
     >               0.15524976D+03,-0.77626650D+02, 0.58213345D+00,
     >              -0.15840278D+03, 0.76819165D+02, 0.13116911D-01/

C     REGION 3 PONDEROSA PINE
      DATA (F(I,7),I=10,12)/ 0.92005972D+01, 0.39365313D-01,
     >                       0.29661933D-02/
      
      DATA (V(I,7),I= 1,16)/ -0.79890640D+01,
     >              -0.59433730D-01, 0.41539599D+01, 0.76398811D+01,
     >              -0.96260263D+00, 0.35888329D+00, 0.13432608D+00,
     >               0.63964977D-01, 0.47319344D+00,-0.60513918D+00,
     >               0.11725801D+01,-0.21999221D+01,-0.12620490D+01,
     >              -0.10542030D+02, 0.40156677D+01, 0.17008028D+01/

      JSPR = JSP - 22

      VA00 = V(1, JSPR)
      VA01 = V(2, JSPR)
      VA02 = V(3, JSPR)
      VB00 = V(4, JSPR)
      VB01 = V(5, JSPR)
      VC0  = V(6, JSPR)
      
      VX1  = V(7,JSPR)
      VX2  = V(8,JSPR)
      VX3  = V(9,JSPR)

      VA03 = V(10, JSPR)
      VE00 = V(11, JSPR)
      VE01 = V(12, JSPR)
      VE02 = V(13, JSPR)
      VF00 = V(14, JSPR)
      VF01 = V(15, JSPR)
      VG0  = V(16, JSPR) 
                          
      BH = 4.5
      
      DMEDIAN = F(10,JSPR) *(HTTOT - BH)**(F(11,JSPR)+F(12,JSPR)*HTTOT)
      DRATIO = DBHOB/DMEDIAN 
      LOGHT = log(HTTOT)

       VA0 = VA00 + VA01*LOGHT + VA02*DRATIO  + VA03*LOGHT*DRATIO
       VB0 = VB00 + VB01*LOGHT

       VE0 = VE00 + VE01*LOGHT + VE02*DRATIO
       VF0 = VF00 + VF01*LOGHT

       VC = VC0 + VX1*LOGHT 
   
       if (h .lt. bh) then
c                                                below BH
             X = (BH-H) / BH
             LVARHAT = VE0 + VF0*X**VG0 
                if (LVARHAT .gt. 15.0)   LVARHAT= 15.0
                if (LVARHAT .lt. -15.0)  LVARHAT= -15.0
             VARHAT  = exp(LVARHAT)
             SE_LNX  = SQRT(VARHAT)
        else if (h.eq. BH)  then
c                                                at BH
             SE_LNX=0.0
        else if (h .lt. HTTOT) then
c                                                above BH
             X = (H - BH)/(HTTOT - BH) 
             LVARHAT = VA0 + VB0*X**VC + VX2*(HTTOT/50.)**VX3 /(1.0-X)
                if (LVARHAT .gt. 15.0)   LVARHAT= 15.0
                if (LVARHAT .lt. -15.0)  LVARHAT= -15.0  
             VARHAT = exp(LVARHAT) 
             SE_LNX  = SQRT(VARHAT)
          else
c                                This is above total HTTOT - meaningless.
             VARHAT = 1.0
          endif

      RETURN
      END



C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c!!!!!!!!!!!!!!!!!!!!BLACK HILLS ROUTINES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


c***********************************************************************
c***********************************************************************
      SUBROUTINE SHP_BH(DBHOB,HTTOT,RFLW,RHFW)

c***********************************************************************
c       SF  SHAPE SUBROUTINE   BHNF Ponderosa Pine, English Units
c              coefficients from BHNF model effort JWF & RLE
                      
c                                  This is steps 2 and 3 of application

      REAL*4 RHFW(4),RFLW(6)
      REAL H,HTTOT,DBHOB,BH 
      REAL*8 U1,U2,U3,U4,U5,U6,U7,U8,U9,DMEDIAN,DFORM
      REAL*4 R1,R2,R3,R4,R5,A3,RHI1,RHLONGI,RHI2,RHC
      INTEGER I
C      INTEGER INEEDSL

      DO 101 I=1,4
         RFLW(I) = 0.0
 101  CONTINUE

      DO 102, I=1,6
         RFLW(I) = 0.0
 102  CONTINUE

C      INEEDSL = 0
      BH = 4.5
      H = HTTOT

c  Code from COEMOD4, with coefficients from PP14
      DMEDIAN = 0.16802000D+01 *(H-BH)**(.40850000D+00+.16900000D-02*H)
      DFORM = DBHOB/DMEDIAN -1.0d0
c                                                                RHI1
      U7 = -.12726446D+01  -.48259438D-02*H
c                                                                RHLONGI
      U9 = .18219470D+00 + .00000000D+00*H
c                                                                 RHC
      U8 = .99000000D+00+.00000000D+00*H
c                                                              U1-U5
      U1 = -.15505171D+01 -.17174522D-01*H
      U2 = .27722769D+00 -.00000000D+00*DFORM
     >   -.00000000D+00*log(H) + .00000000D+00*dform*(H) 
     >   -.21540189D+00*DBHOB            

      U3 = .20426515D+01 + .00000000D-00*DFORM  -.83434213D+00*log(H)
      U4 = -.70000000D+01 + .00000000D+00*DFORM 
      U5 =  .77448837D+01
c                                        U6=A3 
      U6 = .13766370D+01 -.47598661D+00*DFORM
c                                              limits on U1 to U5
      if (U1.lt.-7.0d0)  U1 =-7.0d0                               
      if (U1.gt. 7.0d0)  U1 = 7.0d0
      if (U2.lt.-7.0d0)  U2 =-7.0d0                               
      if (U2.gt. 7.0d0)  U2 = 7.0d0                               
      if (U3.lt.-7.0d0)  U3 =-7.0d0                               
      if (U3.gt. 7.0d0)  U3 = 7.0d0                               
      if (U4.lt.-7.0d0)  U4 =-7.0d0                               
      if (U4.gt. 7.0d0)  U4 = 7.0d0                               
      if (U5.lt.-7.0d0)  U5 =-7.0d0                               
      if (U5.gt. 7.1d0)  U5 = 7.1d0                                     
      if (U6.lt. 1.005)  U6 = 1.005D0
      if (U6.gt. 100.0d0)U6 = 100.0d0
      if (U7.lt.-7.0d0)  U7 =-7.0d0                               
      if (U7.gt. 7.0d0)  U7 = 7.0d0                                
      if (U8.gt. 0.99)   U8 = 0.99
      if (u9.gt. 0.3d0)  U9 = 0.3d0
      if (u9.lt. 0.0d0)  U9 = 0.0d0

c     This code taken from RF_SHP2 from the REG1 program
      R1 =  dexp(U1) / ( 1.0d0 + dexp(U1) )
      R2 =  dexp(U2) / ( 1.0d0 + dexp(U2) )
      R3 =  dexp(U3) / ( 1.0d0 + dexp(U3) )
      R4 =  dexp(U4) / ( 1.0d0 + dexp(U4) )
      R5 =  0.5 + 0.5*dexp(U5) / ( 1.0d0 + dexp(U5) )
      
      IF (U5 .gt. 7.0) R5 = 1.0
      
      a3 = U6
      RHI1 =  dexp(U7) / ( 1.0d0 + dexp(U7) )
      
      IF (RHI1.GT. 0.5) RHI1=0.5
      
      RHLONGI = U9    
      RHI2 = RHI1 + RHLONGI
      RHC = U8
      IF (RHC.LT.RHI2+0.01) RHC = MIN(RHI2 + 0.01,
     >                                         (RHI2 + 1.0)/2.0)
      RHFW(1) = RHI1     
      RHFW(2) = RHI2
      RHFW(3) = RHC
      RHFW(4) = RHLONGI
      
      RFLW(1) = R1
      RFLW(2) = R2
      RFLW(3) = R3
      RFLW(4) = R4
      RFLW(5) = R5
      RFLW(6) = A3
      
      
      RETURN
      END                             


c***********************************************************************
c***********************************************************************
      FUNCTION COR_BH(HTTOT,H30,HT2)
c                                                               CORR
C********************************************************************
c     given 2 heights (hi and hj), estimate the correlation of the
c       errors in the corresponding dib's
c        Correlations from prelim model by JWF & RLE
      
      REAL HTTOT,H30,HT2,COR_BH
      REAL*8 T1,T2,T3,T4,H1,H2,CORR
      REAL*8 Q1,Q2,Q3,Q4,Q5,QS
    
      Q1 = -0.42141136D+01
      Q2 =  0.36157646D+01
      QS =  0.00000000D+00
      Q4 = -0.15164459D+01
      Q5 =  0.28261064D+00

      Q3 = QS - (Q1 + Q2)

c                          any error is completely correlated with itself
      IF(H30.EQ.HT2) THEN
           COR_BH=1.0
           RETURN
      ENDIF
c                               there are no errors at breast height
c                               correlations are irrelevant
      IF(H30.EQ.4.5 .OR. HT2.EQ.4.5) THEN
          COR_BH = 0.5
          RETURN
      ENDIF

c                             define heights h1 and h2 with h2 > h1
      H1=MIN(H30, HT2)
      H2=MAX(H30,HT2)
      
      IF(H1.GT.4.5) THEN
c                                         both heights are above BH
          T3=(H1-4.5)/(HTTOT-4.5)
          T4=(H2-4.5)/(HTTOT-4.5)                                
          COR_BH = exp( Q1 * (T4 - T3) + Q2 * (T4*T4 - T3*T3)/2.0d0 +
     >          Q3 * (T4*T4*T4 - T3*T3*T3)/3.0d0) 
          IF(COR_BH.GT.0.999)  COR_BH = 0.999
          RETURN
      ENDIF

      IF(H2.GT.4.5) THEN
c                                              h1 < Bh  and h2 > BH
         T3 = (H2 - 4.5)/(HTTOT - 4.5)                             
         T2 = (4.5 - H1)/4.5   
         COR_BH = Q5 * exp( Q4 * T2 + Q1 * T3 + Q2*T3*T3/2.0d0 +
     >            Q3*T3*T3*T3 / 3.0d0)       
         IF(COR_BH .GT. 0.999) COR_BH = 0.999
         IF(COR_BH .LT.-0.999) COR_BH = -0.999
         RETURN
      ENDIF

c                                              h1 < Bh and H2 < Bh
c                                              note: t2 < t1
       T2 = (4.5 - H2)/4.5                                                   
       T1 = (4.5 - H1)/4.5                                                   
       CORR = exp( Q4 * (T1-T2))        
      
       COR_BH = CORR
      RETURN

      END    


C***********************************************************************
C***********************************************************************
      SUBROUTINE VAR_BH (DBHOB,HTTOT,HUP,SE)
C                                                   VAR_STDpp   BHNF PP
C***********************************************************************
c                   calculates standard error for 2-point stem-form system
c       H     input    r*8    section height (ft or meters )
c       SE    output   r*8    standard error
c
c         Note: this follows Flewelling and Raynes (Part I), except:
c               special interpolation in 3 segments:
c                  1.  3.0 ft to 4.5 ft  (linear on SE)
c                  2.  4.5 ft to BH + 0.1*(Total height - BH) (linear on SE)
c                  3.  BH + 0.9*(HTTOT - BH) to Totalht  (quadradic on SE)
c                  


      REAL DBHOB,HTTOT,HUP
      REAL*4 RH,LVARHAT,VARHAT,SE
                 
c                                                     below BH
      IF (HUP .LT. 4.5) THEN 
         LVARHAT = -.15512542D+01-.82366251*HUP +.10190777*DBHOB
         VARHAT = exp(LVARHAT)
c                                                     above breast height
      ELSEIF (HUP .GT. 4.5) THEN
         
         RH = (HUP - 4.5)/(HTTOT - 4.5)
         LVARHAT = (-.72335700D+01 + .22333875D+01*LOG(DBHOB)) +
     >             (.25870429D+01-.43950365D-01*LOG(DBHOB)) *
     >             RH**(-.57411597D+00 + .67914946D+00*log(DBHOB))
         VARHAT = exp(LVARHAT)
c                                                     at breast height
      ELSE
c         VARHAT = 1.0
          SE = 0.0
          GO TO 120
      ENDIF

c     calculate SE
      IF (VARHAT.GT.0.) THEN
          SE=SQRT(VARHAT)
      ELSE
          SE=0.0
      ENDIF 

  120 CONTINUE

      
      RETURN
      END

