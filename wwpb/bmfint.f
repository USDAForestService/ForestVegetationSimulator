      SUBROUTINE BMFINT (BYRAM, FLAME, ISTD) 
C----------
C WWPB $Id$
C----------
*     CALLED FROM BMFIRE      
***********************************************************************
*  **BMFINT--WS  DATE OF LAST REVISION:  JUNE 13, 1994
*----------------------------------------------------------------------
*  PURPOSE:
*    CALCULATION OF BYRAMS INTENSITY FOR USE BY THE MORTALITY MODEL.
*    THIS SUBROUTINE HAS BEEN COPIED FROM THE SUBROUTINE
*    FIREMD IN THE MODEL FIRESUM. KEANE ET AL, 1989
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     BYRAM:  BYRAMS INTENSITY, BTU/MIN/FT OF FIRELINE LEGTH
*     FLAME:  FLAME LENGTH IN METERS
*
*  LOCAL VARIABLE DEFINITIONS: 
*     I:      FUEL CATEGORY: 1=DEAD, 2=LIVE
*     J:      SIZE CLASS WITHIN CATEGORY (<100)      
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER INCLUDE FILES.
      INCLUDE 'PPEPRM.F77'

C.... COMMON INCLUDE FILES.
      INCLUDE 'BMFCOM.F77'

C.... VARIABLE DECLARATIONS.

      INTEGER ISIZE(2, 7), IFINES(2), LARGE1, LARGE2  
      REAL LHV, ST, SE 
      REAL AI(2), BSE(2), SIGMA1(2), WO1(2), A(2, 7), F(2, 7)
      REAL QIG(2, 7), BARNS(2), FX(2), WO(2, 7)
      REAL LHV1, BETA1, SIGMA, GAMMA, XIR, RHOBQIG, PHIS
      REAL XIO, MCSA(2), MDCSA(2), IR(2)
      REAL G(2, 7), GS(2, 7), NOCLAS(2), BULK(2, 7)

C.... DATA STATEMENTS.

C.... CHECK FOR DEBUG.


C.... BEGIN ROUTINE



C.... INITIALIZE ALL VARIABLES AND PARAMETERS

C     TURN SLOPE INTO A PROPORTION IF NECESSARY
      IF (SLP(ISTD) .GT. 1.0) THEN
         TTHETA = SLP(ISTD) / 100.0  
      ELSE
         TTHETA = SLP(ISTD)                    
      ENDIF
         
C.... THESE NEXT FOUR PARAMETERS ARE THE SAME FOR ALL FIRE MODELS
C     IN THIS VERSION
C     [MAYBE THEY SHOULD BE SET IN THEIR OWN PARAMETER COMMON?]

      RHOP = 32
      LHV = 8000
      ST = .0555
      SE = .01

      LARGE1 = ND
      LARGE2 = NL
      IFINES(1) = 1

      IF (NL .GT. 0) THEN
         IFINES(2) = 1
      ELSE 
         IFINES(2) = 0
      ENDIF

      NOCLAS(1) = ND
      NOCLAS(2) = NL
      
C     ZERO ALL WORKING VARIABLES
      DO 100 I=1,2
        DO 110 J=1,7
           BULK(I, J) = FWG(I, J) / DEPTH
           ISIZE(I, J) = J
           G(I,J)=0.0
           GS(I,J)=0.0
           A(I,J)=0.0
           F(I,J)=0.0
           WO(I,J)=0.0
           QIG(I,J)=0.0
  110   CONTINUE  
        AI(I)=0.0
        MCSA(I)=0.0
        BSE(I)=0.0
        SIGMA1(I)=0.0
        WO1(I)=0.0
        IR(I)=0.0
        BARNS(I)=0.0
        FX(I)=0.0
  100 CONTINUE
      BYRAM=0.0
      RATE=0.0
      GAMMA=0.0  
      SIGMA=0.0
      AT=0.0
      LHV1=0.0
      FLAG(1)=0.0
      FLAG(2)=0.0
      FLAG(3)=0.0
      SUM1=0.0
      SUM2=0.0
      SUM3=0.0
      SUM4=0.0

C.... SORT FUEL COMPONENTS BY SIZE, FINEST FUELS FIRST

       DO 200 I=1,2
         JMAX = NOCLAS(I)
         IF (JMAX .GT. 1) THEN
           JMM = JMAX - 1
           DO 210 J=1,JMM
              KM = JMAX - J
              DO 215 K=1,KM
                 IDA = ISIZE(I, K)
                 IDB = ISIZE(I, K + 1)
                 SIZA = MPS(I, IDA)
                 SIZB = MPS(I, IDB)
                 IF (SIZA .LT. SIZB) THEN
                    ISIZE(I, K + 1) = IDA
                    ISIZE(I, K) = IDB
                 ENDIF
  215         CONTINUE
  210      CONTINUE
         ENDIF
  200  CONTINUE

C     DELETE LARGE LOGS FROM FIRESPREAD CONSIDERATIONS
      DO 205 I=1,2
          KMAX = NOCLAS(I)
          IF (KMAX .LT. 1) GOTO 205
          DO 202 K=1,KMAX
            J= ISIZE(I,K)
            IF (MPS(I,J) .GE. 16.0) GOTO 202
            NOCLAS(I) = K-1
            GOTO 205
  202     CONTINUE
  205 CONTINUE
      
C     CALCULATE WEIGHTING FACTORS FIRST FOR DEAD FUELS, THEN FOR LIVE FUELS
      N1 = NOCLAS(1)
      N2 = NOCLAS(2)
      NOCLAS(1) = MIN0(LARGE1,N1)
      NOCLAS(2) = MIN0(LARGE2,N2)

      DO 300 I=1,2
         KMIN = IFINES(I)
         KMAX = NOCLAS(I)
         IF ((KMAX .NE. 0) .AND. (KMIN .LE. KMAX)) THEN
           DO 310 K=KMIN,KMAX
             J = ISIZE(I, K)
             GS(I, J) = MPS(I, J) / RHOP
             A(I, J) = FWG(I, J) * GS(I, J)
             GS(I, J) = EXP(-138.0 / (MPS(I, J) + 1E-9))
             AI(I) = AI(I) + A(I, J)
             WO(I, J) = FWG(I, J) * (1 - ST)
  310      CONTINUE
           DO 320 K=KMIN,KMAX
             J = ISIZE(I, K)
             IF (AI(I) .NE. 0) F(I, J) = A(I, J) / AI(I)
  320      CONTINUE
         ENDIF
  300 CONTINUE
      AT = AI(1) + AI(2)
      FX(1) = AI(1) / (AT + 1E-9)
      FX(2) = 1 - FX(1)
      
C.... FIND WEIGHT LOADING OF DEAD AND LIVE FINES, MOISTURE EXTINCT
C     NOTE DEAD AND LIVE FUELS WTD BY EXP(-C/SIGMA) WHERE C=138 OR 500
      FINED = 0.0
      FINEL = 0.0
      WDFMN = 0.0
      FINDM = 0.0
      DO 400 I=1,2
         N = IFINES(I)
         JM = NOCLAS(I)
         IF ((JM .GT. 0) .AND. (N .LE. JM)) THEN
            IF (I .EQ. 1) THEN
               DO 410 J=N,JM
                  JJ = ISIZE(I, J)
                  SA = MPS(I, JJ)
                  IF (SA .NE. 0) THEN 
                     EP = EXP(-138.0 / SA)
                  ELSE
                     EP = 0
                  ENDIF
                  WTFAC = FWG(I, JJ) * EP
                  WMFAC = WTFAC * MOIS(I, JJ)
                  FINED = FINED + WTFAC
                  WDFMN = WDFMN + WMFAC
  410          CONTINUE
               IF (FINED .NE. 0) FINDM = WDFMN / FINED
            ELSEIF (I .EQ. 2) THEN
               DO 420 J=N,JM
                  JJ = ISIZE(I, J)
                  SA = MPS(I, JJ)
                  IF (SA .NE. 0) THEN
                     EP = EXP(-500.0 / SA)
                  ELSE
                     EP = 0.0
                  ENDIF
                  FINEL = FINEL + FWG(I, JJ) * EP
  420          CONTINUE
            ENDIF
         ENDIF
  400 CONTINUE
      IF (FINEL .NE. 0.0) THEN
         FACTOR = FINED / FINEL
         XMOISL = 2.9 * FACTOR * (1 - FINDM / (MEXT(1) + 1E-9)) - 0.226
         IF (XMOISL .LT. MEXT(1)) XMOISL = MEXT(1)
      ELSE
         XMOISL = 100
      ENDIF
      MEXT(2) = XMOISL
      
C.... INTERMEDIATE COMPUTATIONS FOR EACH CATEGORY OF FUEL (DEAD, LIVE)
      DO 500 I=1,2
         AA1 = 0.0
         AA2 = 0.0
         AA3 = 0.0
         AA4 = 0.0
         AA5 = 0.0
         JM = NOCLAS(I)
         N = IFINES(I)
         IF ((JM .NE. 0) .AND. (N .LE. JM)) THEN
            DO 510 K=N,JM
               J = ISIZE(I, K)
               AX = F(I, J)
               SIGM = MPS(I, J)
               IF (SIGM .LT. 48.0) THEN
                  AA5 = AA5 + A(I, J)
               ELSEIF (SIGM .LT. 96.0) THEN
                  AA4 = AA4 + A(I, J)
               ELSEIF (SIGM .LT. 192.0) THEN
                  AA3 = AA3 + A(I, J)
               ELSEIF (SIGM .LT. 1200.0) THEN
                  AA2 = AA2 + A(I, J)
               ELSE
                  AA1 = AA1 + A(I, J)
               ENDIF
               QIG(I, J) = 250.0 + 1116.0 * MOIS(I, J)
               MCSA(I) = MCSA(I) + AX * MOIS(I, J)
               BSE(I) = BSE(I) + AX * SE
               SIGMA1(I) = SIGMA1(I) + AX * MPS(I, J)
               LHV1 = LHV1 + AX * LHV
               SUM4 = SUM4 + BULK(I, J) * FWG(I, J)
               SUM1 = SUM1 + FWG(I, J)
               SUM2 = SUM2 + FWG(I, J) / RHOP
               SUM3 = SUM3 + FX(I) * F(I, J) * QIG(I, J) * GS(I, J)
  510       CONTINUE
            DO 520 K=N,JM
               J = ISIZE(I, K)
               SIGM = MPS(I, J)
               IF (AI(I) .NE. 0.0) AI(I) = AI(I) + 1E-9
               IF (SIGM .LT. 48.0) THEN
                  G(I, J) = AA5 / AI(I)
               ELSEIF (SIGM .LT. 96.0) THEN
                  G(I, J) = AA4 / AI(I)
               ELSEIF (SIGM .LT. 192.0) THEN
                  G(I, J) = AA3 / AI(I)
               ELSEIF (SIGM .LT. 1200.0) THEN
                  G(I, J) = AA2 / AI(I)
               ELSE
                  G(I, J) = AA1 / AI(I)
               ENDIF
               WO1(I) = WO1(I) + G(I, J) * WO(I, J)
  520     CONTINUE
          BETA = MCSA(I) / (MEXT(I) + 1E-9)
          MDCSA(I) = 1 - BETA * (2.59 - BETA
     &                 * (5.11 - BETA * 3.52))
          IF (MEXT(I) .LT. MCSA(I)) MDCSA(I) = 0
          IF (BSE(I) .NE. 0.0) BARNS(I) = .174 / (BSE(I) ** .19)
          IF (BARNS(I) .GT. 1.0) BARNS(I) = 1.0
          SIGMA = SIGMA + FX(I) * SIGMA1(I)
          IR(I) = WO1(I) * LHV1 * MDCSA(I) * BARNS(I)
         ENDIF
  500 CONTINUE

C.... DEAD FUEL TOO MOIST TO SPREAD FLAME

      IF (MDCSA(1) .LE. 0.0) FLAG(1) = 1
        
C.... BEGIN FINAL COMPUTATIONS
C     ONLY DO FINAL CALCS IF MDCSA(1) > 0

      IF (MDCSA(1) .GT. 0.0) THEN
      
C....    BULK DENSITY
         RHOP1 = SUM4 / (SUM1 + 1E-9)

C....    PACKING RATIO
         BETA1 = SUM2 * RHOP1 / (SUM1 + 1E-9)

C....    OPTIMUM PACKING RATIO      
         IF (SIGMA .EQ. 0.0) SIGMA = 1E-9
         BEST = 3.348 / (SIGMA ** 0.8189)
         RAT = BETA1 / (BEST + 1E-9)
      
C....    REACTION INTENSITY WEIGHTED BY SURFACE AREA FRACTION
         A1 = 133.0 / (SIGMA ** 0.7913)
         V = SIGMA ** 1.5
         GAMMA = (V * (RAT ** A1) * EXP(A1 * (1 - RAT)))
     &            / (495.0 + .0594 * V)
         IR(1) = GAMMA * IR(1)
         IR(2) = GAMMA * IR(2)
         XIR = IR(1) + IR(2)

C....    HEAT SINK TERMS

         RHOBQIG = RHOP1 * SUM3

C....    PROPAGATING INTENSITY

         B = (0.792 + 0.681 * SQRT(SIGMA)) * (0.1 + BETA1)
         XIO = (XIR * EXP(B)) / (192.0 + 0.2595 * SIGMA)

C....    SLOPE FACTOR PHIS

         IF (BETA1 .NE. 0.0) PHIS = 5.275 * TTHETA * TTHETA 
     &                              / (BETA1 ** 0.3)

C....    PARAMETERS FOR DETERMINING WIND FACTOR PHIW

         XM1 = 0.02526 * (SIGMA ** 0.54)
         XN1 = 0.715 * EXP(-0.000359 * SIGMA)
         C1 = 7.47 * EXP(-0.133 * (SIGMA ** 0.55))
         IF (RAT .NE. 0.0) C1 = C1 / (RAT ** XN1)

C....    A FEW LINES OF MAXES WERE LEFT OUT BECAUSE
C        NOT NEEDED IN THIS VERSION

         WMAX = 0.9 * XIR
         W = FWIND * 88.0
         PHIW = C1 * (W ** XM1)
         R = XIO * (1.0 + PHIS + PHIW) / (RHOBQIG + 1E-9)
         BYRAM = XIR * R * 384.0 / SIGMA

C....    SURF/VOL TOO SMALL

         IF ((W .NE. 0.0) .AND. (SIGMA .LT. 175.0)) FLAG(3) = 1

C....    WIND SPEED EXCEEDS RELIABLE EXTRAPOLATION

         IF (W .GT. WMAX) FLAG(2) = 1

      ENDIF
      FLAME = 0.45 * (BYRAM / 60.0) ** 0.46
      

      RETURN
      END
