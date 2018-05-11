C----------
C VOLUME $Id: r10tapo.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  4-9-2002
      SUBROUTINE R10TAPO(DBHOB,HT2,LTYPE,MTOPP,CHK,NLOG,HMERCH,CL,SH,
     >                                                    DIB,VS,V)

C     CALCULATES FOR HT IN NUMBER OF LOGS ONLY

************************************************************************
*                              ARGUMENTS                               *
************************************************************************
*                                                                      *
*  DBHOB      - (Input)  - DBH                                             *
*  HT2    - (Input)  - Number 16' Logs                                 *
*  TYPE   - (Input)  - L if height is in logs                          *
*  NLOG   - (Output) - Number 16' Logs (Calculated)                    *
*  HMERCH - (Output) - Height, Merch                                   *
*  DIB    - (Output) - Calculateed Top Dia IB (small end)              *
*  CL     - (Output) - Chunk Length                         ajk/3de91  *
************************************************************************

      REAL SH(21),DIB(21),TOPDIA(2),MMH(2),VS(21),
     >       DST(2),DSLO(2),DSHI(2),XLL(21),DS(21),
     >       V(21),HH(21)     

      REAL HT2,HT2TEMP,DBHOB,HMERCH,NLOG,MTOPP,ST
      REAL BK,BB,H,FV,HEST,RXL,TAPER,CL,FF,DD2MI,DXL,R32,RH
      CHARACTER*1 LTYPE
      INTEGER CHK,I,NOL16,NOL32,K,N,NN,KBD,KB,NLOG3,JJ

      TOPDIA(1) = MTOPP
      TOPDIA(2) = 4.0
     
      DO 15, I=1,2 
        MMH(I) = 1.0  
   15 CONTINUE    
    
    
      DO 16, I=1,21
        VS(I) = 0.0   
        DS(I) = 0.0   
        DIB(I) = 0.0  
        XLL(I) = 0.0  
        V(I) = 0.0    
   16 CONTINUE    

      ST = 1.0
    
      IF(DBHOB.GT.36) ST = DBHOB/36.     

C********************************************
C****   CHECK FOR HEIGHT OF TREE IN LOGS  ***
C********************************************

      IF (LTYPE.EQ.'L' .OR. LTYPE.EQ.'l') THEN

        HT2TEMP = HT2
        NOL16 = INT(HT2TEMP)   
        NOL32 = 0     

        IF(NOL32.EQ.0.AND.NOL16.EQ.0) THEN
           BK = BB(DBHOB,H)   
           FV= 0.005454154 * DBHOB**2 * (H - 4.5) * BK  
        ENDIF

        IF(NOL32.GT.0) NOL16 = NOL32 * 2   

        IF(NOL16.GT.0) THEN
          HH(1) = ST + NOL16 * 16.3   
          H = 0.0     
        ENDIF

        DO 20, I = 1,2   

        XLL(I) = 1. - (2./3.) * (TOPDIA(I)/DBHOB) 
        IF(H.LT.0.01) HEST = HH(1)/XLL(I)

        IF(HEST.GT.0.01) BK = BB(DBHOB,HEST) 

        DST(I) = (TOPDIA(I)**2)/(BK * DBHOB**2) 
        DSLO(I) = DST(I) - 0.00001   
        DSHI(I) = DST(I) + 0.00001   

        IF(H.LE.0.01) THEN   
          RH = (HEST - HH(1))/(HEST-4.5)     
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RH.LE.0.0)THEN
             RH = 0
             R32 = 0
         ELSEIF(RH .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RH
          ENDIF

          DS(I) = DD2MI(RH,R32,DBHOB,HEST)   
          RXL = 0.9 * RH    
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C---------
          IF(RXL.LE.0.0)THEN
            RXL = 0
            R32 = 0
         ELSEIF(RXL .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RXL
          ENDIF

          DXL = DD2MI(RXL,R32,DBHOB,HEST)

          TAPER = (DS(I)-DXL)/(0.1*RH)
        ELSE         
          HH(I) = H * XLL(I)    
          RH = (H - HH(I))/(H - 4.5)     
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RH.LE.0.0)THEN
             RH = 0
             R32 = 0
         ELSEIF(RH .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RH
          ENDIF

          DS(I) = DD2MI(RH,R32,DBHOB,H)
     
C ***     ESTIMATE TAPER NEAR R  
     
          RXL = 0.9 * RH    
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RXL.LE.0.0)THEN
              RXL = 0
              R32 = 0
         ELSEIF(RXL .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RXL
          ENDIF

          DXL = DD2MI(RXL,R32,DBHOB,H)
          TAPER = (DS(I) - DXL)/(0.1 * RH)
        ENDIF 
    
        DO 30 K=1,10     

          IF(DS(I).GT.DSLO(I).AND.DS(I).LT.DSHI(I))  GO TO 40     

          RXL = RH + (DST(I) - DS(I))/TAPER    
          RH = RXL 
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RH.LE.0.0)THEN
              RH = 0
              R32 = 0
         ELSEIF(RH .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RH
          ENDIF

          IF(H.LE.0.01) THEN  
            HEST = (HH(I) - 4.5 * RH)/(1. - RH)     

            DS(I) = DD2MI(RH,R32,DBHOB,HEST)   
          ELSE   
            DS(I) = DD2MI(RH,R32,DBHOB,H)
          ENDIF

   30   CONTINUE    

   40   CONTINUE    

        IF(NOL16.LE.0.AND.H.GE.0.01) THEN
           HH(I)=H-(RH*(H-4.5))     
        ENDIF
    
        IF(H.LT.0.01) H = HEST     

C ***   FIND NUMBER (NN) OF LOGS AND CHUNKS AND FIND CHUNK LENGTH   
C ***   FOR TOPDIA(1) ONLY (I.E., DIB TOP=6.0 INCHES).  

        IF(CHK.EQ.1) GO TO 25
   20   CONTINUE

   25   DO 50, N=1,21
          MMH(1) = 16.3 * N + ST + 0.0001  
          NN = N  
          IF(MMH(1) .GE. HH(1)) GO TO 60   
   50   CONTINUE    

   60   CONTINUE    
        KBD = NN - 1    
     
C ***   CALCULATE CHUNK LENGTH  
     
        CL = HH(1) - 16.3 * KBD - ST    
     

C ***   NEXT, FIND SCALING DIAMETERS AND VOLUMES FOR ALL LOG HEIGHTS TO   
C ***   TIP OF TREE.  START WITH SCALING HEIGHT LIST. 
     
        SH(1)=4.5    
C        SH(2) = 4.5    

        DO 70, KB=1,KBD    
          SH(KB+1) = ST + KB * 16.3     
   70   CONTINUE 

        SH(KBD+2) = HH(1)   
        SH(KBD+3) = HH(2)   
        SH(KBD+4) = H 
        NLOG3 = NN + 3    
        NLOG = NN     
        BK = BB(DBHOB,H)  
        FV = 0.005454154 * DBHOB**2 * (H-4.5) * BK 

        DO 80, JJ=1,NLOG3    
          RH = (H - SH(JJ))/(H - 4.5)    
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RH.LE.0.0)THEN
              RH = 0
              R32 = 0
         ELSEIF(RH .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RH
          ENDIF
          DIB(JJ) = DD2MI(RH,R32,DBHOB,H)    
          DIB(JJ) = (DIB(JJ) * BK)**0.5 * DBHOB    
          VS(JJ) = FF(RH,R32,DBHOB,H) * FV     
   80   CONTINUE    
     
C ***   NEXT, GET LOG AND TREE VOLUMES
     
        DO 85, JJ=1,NLOG3    
          V(JJ) = VS(JJ) - VS(JJ+1)   
   85   CONTINUE    

        HMERCH = H  


C***********************************************
C*****     IF TOTAL HEIGHT IS IN FEET        ***
C***********************************************

      ELSE
        H = HT2  
        BK = BB(DBHOB,H)  
        FV = 0.005454154 * DBHOB**2 * (H - 4.5) * BK 
        DO 200, I=1,2  
          DST(I) = (TOPDIA(I)**2)/(BK * DBHOB**2) 
          DSLO(I) = DST(I) - 0.00001   
          DSHI(I) = DST(I) + 0.00001   

          XLL(I) = 1. - (2./3.) * (TOPDIA(I)/DBHOB) 
          HH(I) = H * XLL(I)    
          RH = (H - HH(I))/(H - 4.5)     
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RH.LE.0.0)THEN
             RH = 0
             R32 = 0
         ELSEIF(RH .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RH
          ENDIF

          DS(I) = DD2MI(RH,R32,DBHOB,H)
     
C ***     ESTIMATE TAPER NEAR R  
     
          RXL = 0.9 * RH    
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
          IF(RXL.LE.0.0)THEN
              RXL = 0
              R32 = 0
         ELSEIF(RXL .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RXL
          ENDIF
          DXL = DD2MI(RXL,R32,DBHOB,H)
          TAPER = (DS(I) - DXL)/(0.1 * RH)

          DO 100 K = 1,10     
            IF(DS(I).GT.DSLO(I).AND.DS(I).LT.DSHI(I)) GO TO 110     
            RXL = RH + (DST(I) - DS(I))/TAPER    
            RH = RXL 
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
            IF(RH.LE.0.0)THEN
               RH = 0
               R32 = 0
           ELSEIF(RH .LT. 0.078) THEN
               R32= 0.078
            ELSE
               R32= RH
            ENDIF
            DS(I) = DD2MI(RH,R32,DBHOB,H)
          
  100     CONTINUE    

  110     CONTINUE    
          HH(I) = H - (RH * (H - 4.5))     
     
  200   CONTINUE    

        DO 250, N=1,21
          MMH(1) = 16.3 * N + ST  
          NN = N  
          IF(MMH(1).GE.HH(1))  GO TO 260   
  250   CONTINUE    

  260   CONTINUE    
        KBD = NN - 1    

     
C ***   CALCULATE CHUNK LENGTH  
     
        CL = HH(1) - 16.3 * KBD - ST    
     
C ***   NEXT, FIND SCALING DIAMETERS AND VOLUMES FOR ALL LOG HEIGHTS TO   
C ***   TIP OF TREE.  START WITH SCALING HEIGHT LIST. 
     
        SH(1) = 4.5    
C        SH(2) = 4.5    

        DO 300, KB = 1,KBD    
          SH(KB+1) = ST + KB * 16.3     
  300   CONTINUE    

        SH(KBD+2) = HH(1)   
        SH(KBD+3) = HH(2)   
        SH(KBD+4) = H 
        NLOG3 = NN + 3    
        NLOG = NN     

        DO 400, JJ=1,NLOG3    
           RH = (H - SH(JJ))/(H - 4.5)    
C----------
C      IF RH<0.078 (RH^32<10^-38), RH32 IS TRUNCATED AND USED IN
C      THE (.)^32 TERM OF THE DVA FUNCTION. THIS SOLVES NDP ERROR,
C      NUMERICAL UNDERFLOW, IN THE DVA FUNCTION.
C----------
           IF(RH.LE.0.0)THEN
              RH = 0
              R32 = 0
         ELSEIF(RH .LT. 0.078) THEN
             R32= 0.078
          ELSE
             R32= RH
          ENDIF
          DIB(JJ) = DD2MI(RH,R32,DBHOB,H)    
          If (dib(jj) .lt. 0.0) dib(jj) = 0.0
          DIB(JJ) = (DIB(JJ) * BK)**.5 * DBHOB
          VS(JJ) = FF(RH,R32,DBHOB,H) * FV     
  400   CONTINUE    
     
C ***   NEXT, GET LOG AND TREE VOLUMES
     
        DO 450, JJ=1,NLOG3    
          V(JJ) = VS(JJ) - VS(JJ+1)   
  450   CONTINUE    

        HMERCH = MMH(1)   

      ENDIF

      RETURN
      END   



C************************************************************
C***  PROFILE EQUATION  (USED FOR DIAMETER CALCULATIONS)  ***
C************************************************************

      FUNCTION DD2MI(RH,R32,DBHOB,H)
      REAL RH,DBHOB,H,DD2MI,R32

      DD2MI = RH**1.5 +
     > ((-0.0052554*H + 0.000034947*H**2 + (0.104477*H)/DBHOB) *
     >        (RH**1.5 - RH**3)) +
     > ((7.76807/DBHOB**2 - 0.0000094852*H**2 - (0.011351*H)/DBHOB) *
     >        (RH**1.5 - R32**32))

C     XA = R**1.5
C     XB = R**3
C     XC = R**32
C     XD = DBHOB**2
C     XE = H**2
C     XF = -0.0052554 * H 
C     XG = 0.000034947 * XE
C     XH = (0.104477 * H) / DBHOB
C     XI = XA - XB
C     XJ = 7.76807 / XD
C     XK = 0.0000094852 * XE
C     XL = (0.011351 * H) / DBHOB
C     XM = XA - XC
C     XN = XF + XG + XH
C     XO = XJ - XK - XL
C     XP = XN * XI
C     XQ = XO * XM

C     WRITE (*,*) '   FUNCTION DD2MI'
C     WRITE (*,*) '      R = ',R,'  DBHOB = ',DBHOB,'  H = ',H
C     WRITE (*,*) '      R**1.5 =                                 ',XA
C     WRITE (*,*) '      R**3 =                                   ',XB
C     WRITE (*,*) '      R**32 =                                  ',XC
C     WRITE (*,*) '      DBHOB**2 =                                   ',XD
C     WRITE (*,*) '      H**2 =                                   ',XE
C     WRITE (*,*) '      -0.0052554 * H =                         ',XF
C     WRITE (*,*) '      0.000034947 * H**2 =                     ',XG
C     WRITE (*,*) '      (0.104477 * H) / DBHOB =                     ',XH
C     WRITE (*,*) '      R**1.5 - R**3 =                          ',XI
C     WRITE (*,*) '      7.76807 / DBHOB**2 =                         ',XJ
C     WRITE (*,*) '      0.0000094852 * H**2 =                    ',XK
C     WRITE (*,*) '      (0.011351 * H) / DBHOB =                     ',XL
C     WRITE (*,*) '      R**1.5 - R**32 =                         ',XM
C     WRITE (*,*) '      (-0.0052554 * H) + (0.000034947 * H**2) +'
C     WRITE (*,*) '         ((0.104477 * H) / DBHOB) =                ',XN
C     WRITE (*,*) '      (7.76807 / DBHOB**2) - (0.0000094852 * H**2) -'
C     WRITE (*,*) '         ((0.011351 * H) / DBHOB) =                ',XO
C     WRITE (*,*) '      ((-0.0052554 * H) + (0.000034947 * H**2) +'
C     WRITE (*,*) '         ((0.104477 * H) / DBHOB)) *'
C     WRITE (*,*) '         (R**1.5 - R**3) =                     ',XP
C     WRITE (*,*) '      ((7.76807 / DBHOB**2) - (0.0000094852 * H**2) -'
C     WRITE (*,*) '         ((0.011351 * H) / DBHOB)) * '
C     WRITE (*,*) '         (R**1.5 - R**32) =                    ',XQ
C     WRITE (*,*) '   END FUNCTION DD2MI'

      RETURN
      END


C******************************************************************
C***  INTEGRAL OF PROFILE EQUATION (USED IN VOLUME CALCULATION) ***
C******************************************************************

      FUNCTION FF(RH,R32,DBHOB,H)
      REAL RH,DBHOB,H,FF,R32
      FF = 0.4 * RH**2.5 + (-0.0052554 * H + 0.000034947 * H**2
     >     + 0.104477 * H/DBHOB) * (0.4 * RH**2.5 - 0.25 * RH**4.) + 
     >     (7.76807/DBHOB**2 - 0.0000094852*H**2 - 0.011351*H/DBHOB)*
     >     (0.4 * RH**2.5 - (1./33.) * R32**33)
      RETURN
      END


C*********************************
C***  BARK THICKNESS EQUATION  ***
C*********************************

      FUNCTION BB(DBHOB,H)
      REAL DBHOB,H,BB
      BB = (0.8467 + 0.0009144 * DBHOB + 0.0003568 * H)     
      RETURN
      END
