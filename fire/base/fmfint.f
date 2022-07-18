      SUBROUTINE FMFINT (IYR, BYRAM, FLAME, FTYP, HPA, ICALL)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
*     CALLED FROM FMBURN
*                 FMPOFL
*  PURPOSE:
*    CALCULATION OF BYRAMS INTENSITY FOR USE BY THE MORTALITY MODEL.
*    THIS SUBROUTINE HAS BEEN COPIED FROM THE SUBROUTINE
*    FIREMD IN THE MODEL FIRESUM. KEANE ET AL, 1989
*
*    Modifications have been made to allow the routine to loop over the
*     possible fuel models and to calculate the appropriate exit variables
*     based on the weightings from the nearby fuel models. See FMCFMD
*     for details on the calculations of weightings and nearby fuel models
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     IYR:    CALENDAR YEAR
*     BYRAM:  BYRAMS INTENSITY, BTU/MIN/FT OF FIRELINE LENGTH
*     FLAME:  FLAME LENGTH IN FEET
*     FTYP:   ARRAY ELEMENT TO SAVE SOME INFO TO (2=FM10, 1,3=ACTUAL)
*     HPA:    HEAT PER UNIT AREA (BTU/SQFT)
*     ICALL:  1 = NORMAL CALL, 2 = CALL TO CALCULATE CFB
*
*  LOCAL VARIABLE DEFINITIONS:
*     I:      FUEL CATEGORY: 1=DEAD, 2=LIVE
*     J:      SIZE CLASS WITHIN CATEGORY (<100)
*     TMIN:   TOTAL MINERAL CONTENT, FRACTION DRY WEIGHT (ST)
*     SILFRE: SILICA-FREE MINERAL CONTENT, FRACTION DRY WEIGHT (SE)
*     RHOP:   DRY DENSITY
*     LHV:    HEAT OF COMBUSTION (BTU/LB)
*     XIR:    REACTION INTENSITY (BTU/MIN/FT2)
*     XIO:    PROPAGATING FLUX RATIO TIMES REACTION INTENSITY
*     SIGMA:  SURFACE AREA TO VOLUME RATIO OF FUEL PARTICLES (1/FT)
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER INCLUDE FILES.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'


C.... VARIABLE DECLARATIONS.

      INTEGER ISIZE(2, 7), IFINES(2), LARGE1, LARGE2, FTYP
      REAL    LHV, TMIN, SILFRE
      REAL    AI(2), BSE(2), SIGMA1(2), WO1(2), A(2, 7), F(2, 7)
      REAL    QIG(2, 7), BARNS(2), FX(2), WO(2, 7)
      REAL    LHV1, BETA1, SIGMA, GAMMA, XIR, RHOBQIG, PHIS
      REAL    XIO, MCSA(2), MDCSA(2), IR(2)
      REAL    G(2, 7), GS(2, 7), NOCLAS(2), BULK(2, 7)
      LOGICAL DEBUG
      INTEGER IYR, ICALL, IRTNCD
      INTEGER JMAX,JMM,KM,K,IDA,IDB,KMAX,N1,N2,KMIN,N,JM,JJ,INB,I,J
      REAL    SUM1,SUM2,SUM3,SUM4,SIZA,SIZB,FINED,FINEL,WDFMN,FINDM,
     &        SA,EP,WTFAC,WMFAC,FACTOR,XMOISL,AA1,AA2,AA3,AA4,AA5,AX,
     &        SIGM,BETA,RHOP1,BEST,RAT,A1,V,B,XM1,XN1,C1,WMAX,W,PHIW,
     &        R,FLAME,BYRAM,TTHETA,RHOP,BYRAMT,RATE,AT,HPA
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMFINT',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,FTYP,FWIND,(MOIS(1,I),I=1,5),
     >  MOIS(2,1), MOIS(2,2)
    7 FORMAT(' ENTERING FMFINT CYCLE = ',I2,' FTYP=',I2,' FWIND=',F7.3/
     >  ' MOIS=',7F10.3)

C     INITIALIZATION

C     TURN SLOPE INTO A PROPORTION IF NECESSARY

      IF (FMSLOP .GT. 1.0) THEN
        TTHETA = FMSLOP / 100.0
      ELSE
        TTHETA = FMSLOP
      ENDIF

C.... THESE NEXT FOUR PARAMETERS ARE THE SAME FOR ALL FIRE MODELS
C     IN THIS VERSION
C     [MAYBE THEY SHOULD BE SET IN THEIR OWN PARAMETER COMMON?]

      RHOP         =   32.0
      TMIN         =    0.0555
      SILFRE       =    0.01

C     ZERO OUT THE VARIABLES THAT WE CARE ABOUT OUTSIDE THIS

      BYRAM        =    0.0
      FLAME        =    0.0
      SSIGMA(FTYP) =    0.0
      SRHOBQ(FTYP) =    0.0
      SXIR(FTYP)   =    0.0
      SIRXI(FTYP)  =    0.0
      SPHIS(FTYP)  =    0.0
      SFRATE(FTYP) =    0.0
      SCBE(FTYP)   =    0.0

C     Start the loop for the fire models defined in fmcfmd

      DO 800 INB = 1,MXFMOD

C    one new fuel model has a different heat content value (106-GR06)
        IF (FMOD(INB) .EQ. 106) THEN
          LHV = 9000.0
        ELSEIF (IFLOGIC .EQ. 2) THEN
          LHV = ULHV
        ELSE
          LHV = 8000.0
        ENDIF

        IF ((FTYP .EQ. 2) .AND. (ICALL .EQ. 1)) THEN

c       if FTYP = 2 and we are calling to get fm 10 info,
c       then we don't want to do any interpolation.

          IF (INB .GE. 2) GOTO 810
          CALL FMGFMV(IYR, 10)
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN
        ELSE
          IF (FMOD(INB).EQ.0 .OR. FWT(INB) .LE. 0.0) GOTO 800
          CALL FMGFMV(IYR, FMOD(INB))
          CALL fvsGetRtnCode(IRTNCD)
          IF (IRTNCD.NE.0) RETURN
        ENDIF

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
C
C       ZERO ALL WORKING VARIABLES
C
        DO I=1,2
          DO J=1,7
            BULK(I, J) = FWG(I, J) / DEPTH
            ISIZE(I, J) = J
            G(I,J)=0.0
            GS(I,J)=0.0
            A(I,J)=0.0
            F(I,J)=0.0
            WO(I,J)=0.0
            QIG(I,J)=0.0
          ENDDO
          AI(I)=0.0
          MCSA(I)=0.0
          BSE(I)=0.0
          SIGMA1(I)=0.0
          WO1(I)=0.0
          IR(I)=0.0
          BARNS(I)=0.0
          FX(I)=0.0
        ENDDO

        XIR = 0.0
        RHOBQIG = 0.0
        XIO = 0.0
        PHIS = 0.0
        R = 0.0
        C1 = 0.0

        BYRAMT=0.0
        RATE=0.0
        GAMMA=0.0
        SIGMA=0.0
        AT=0.0
        LHV1=0.0
        FLAG(1)=0
        FLAG(2)=0
        FLAG(3)=0
        SUM1=0.0
        SUM2=0.0
        SUM3=0.0
        SUM4=0.0
C
C       SORT FUEL COMPONENTS BY SIZE, FINEST FUELS FIRST
C
        DO I=1,2
          JMAX = INT(NOCLAS(I))

          IF (JMAX .GT. 1) THEN
            JMM = JMAX - 1
            DO J=1,JMM
              KM = JMAX - J
               DO K=1,KM
                 IDA = ISIZE(I, K)
                 IDB = ISIZE(I, K + 1)
                 SIZA = MPS(I, IDA)
                 SIZB = MPS(I, IDB)
                 IF (SIZA .LT. SIZB) THEN
                   ISIZE(I, K + 1) = IDA
                   ISIZE(I, K) = IDB
                 ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
C       ALSO SORT IF THE LIVE WOODY CATEGORY IS EMPTY BUT THERE ARE LIVE HERBS
C       OR IF THERE ARE DEAD HERBS AND SOME OF THE DEAD CATEGORIES ARE EMPTY.
C       IT SEEMS SILLY TO HAVE TO DO THIS, BUT I DON'T KNOW ANOTHER WAY BESIDES
C       TOTALLY REVAMPING THE CODE BELOW.  SAR
C
        IDA = ISIZE(2, 1)
        IDB = ISIZE(2, 2)
        IF (FWG(2,IDA) .LE. 0) THEN
          ISIZE(2, 2) = IDA
          ISIZE(2, 1) = IDB
        ENDIF

        IDA = ISIZE(1, 3)
        IDB = ISIZE(1, 4)
        IF (FWG(1,IDA) .LE. 0) THEN
          ISIZE(1, 4) = IDA
          ISIZE(1, 3) = IDB
        ENDIF

        IDA = ISIZE(1, 2)
        IDB = ISIZE(1, 3)
        IF (FWG(1,IDA) .LE. 0) THEN
          ISIZE(1, 3) = IDA
          ISIZE(1, 2) = IDB
        ENDIF

C
C       DELETE LARGE LOGS FROM FIRESPREAD CONSIDERATIONS
C
        DO 205 I=1,2
          KMAX = INT(NOCLAS(I))
          IF (KMAX .LT. 1) GOTO 205
          DO 202 K=1,KMAX
            J= ISIZE(I,K)
            IF (MPS(I,J) .GE. 16.0) GOTO 202
            NOCLAS(I) = K-1
            GOTO 205
  202     CONTINUE
  205   CONTINUE
C
C       CALCULATE WEIGHTING FACTORS FIRST FOR DEAD FUELS, THEN FOR LIVE FUELS
C
        N1 = INT(NOCLAS(1))
        N2 = INT(NOCLAS(2))
        NOCLAS(1) = MIN0(LARGE1,N1)
        NOCLAS(2) = MIN0(LARGE2,N2)

      IF (DEBUG) THEN
        WRITE(JOSTND,*) 'ND=',ND
        WRITE(JOSTND,*) 'NL=',NL
        WRITE(JOSTND,*) 'LARGE1=',LARGE1
        WRITE(JOSTND,*) 'LARGE2=',LARGE2
        WRITE(JOSTND,*) 'NOCLAS1=',NOCLAS(1)
        WRITE(JOSTND,*) 'NOCLAS2=',NOCLAS(2)
        WRITE(JOSTND,*) 'IFINES1=',IFINES(1)
        WRITE(JOSTND,*) 'IFINES2=',IFINES(2)
      ENDIF

        DO 300 I=1,2
          KMIN = IFINES(I)
          KMAX = INT(NOCLAS(I))
          IF ((KMAX .NE. 0) .AND. (KMIN .LE. KMAX)) THEN
            DO 310 K=KMIN,KMAX
              J = ISIZE(I, K)
              GS(I, J) = MPS(I, J) / RHOP
              A(I, J) = FWG(I, J) * GS(I, J)
              GS(I, J) = EXP(-138.0 / (MPS(I, J) + 1E-9))
              AI(I) = AI(I) + A(I, J)
              WO(I, J) = FWG(I, J) * (1.0 - TMIN)
               IF (DEBUG) THEN
                 WRITE(JOSTND,*) 'I=',I
                 WRITE(JOSTND,*) 'J=',J
                 WRITE(JOSTND,*) 'GS=',GS(I,J)
                 WRITE(JOSTND,*) 'AI=',AI(I)
                 WRITE(JOSTND,*) 'WO=',WO(I,J)
              ENDIF
  310       CONTINUE
            DO 320 K=KMIN,KMAX
              J = ISIZE(I, K)
              IF (AI(I) .NE. 0) F(I, J) = A(I, J) / AI(I)
  320       CONTINUE
          ENDIF
  300   CONTINUE

        AT = AI(1) + AI(2)
        FX(1) = AI(1) / (AT + 1E-9)
        FX(2) = 1.0 - FX(1)
C
C       FIND WEIGHT LOADING OF DEAD AND LIVE FINES, MOISTURE EXTINCT
C       NOTE DEAD AND LIVE FUELS WTD BY EXP(-C/SIGMA) WHERE C=138 OR 500
C
        FINED = 0.0
        FINEL = 0.0
        WDFMN = 0.0
        FINDM = 0.0
        DO 400 I=1,2
          N = IFINES(I)
          JM = INT(NOCLAS(I))
          IF ((JM .GT. 0) .AND. (N .LE. JM)) THEN
            IF (I .EQ. 1) THEN
              DO 410 J=N,JM
                JJ = ISIZE(I, J)
                SA = MPS(I, JJ)
                IF (SA .NE. 0) THEN
                  EP = EXP(-138.0 / SA)
                ELSE
                  EP = 0.0
                ENDIF
                WTFAC = FWG(I, JJ) * EP
C       WHEN DOING THE DEAD HERB CATEGORY, USE MOISTURE FOR DEAD 1-HR FUELS
                IF (JJ .EQ. 4) THEN
                  WMFAC = WTFAC * MOIS(I,1)
                ELSE
                  WMFAC = WTFAC * MOIS(I, JJ)
                ENDIF
                FINED = FINED + WTFAC
                WDFMN = WDFMN + WMFAC
  410         CONTINUE
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
  420         CONTINUE
            ENDIF
          ENDIF
  400   CONTINUE

        IF (FINEL .NE. 0.0) THEN
          FACTOR = FINED / FINEL
          XMOISL = 2.9 * FACTOR * (1.0 - FINDM / MEXT(1)) - 0.226
          IF (XMOISL .LT. MEXT(1)) XMOISL = MEXT(1)
        ELSE
          XMOISL = 100.0
        ENDIF

        MEXT(2) = XMOISL
C
C       INTERMEDIATE COMPUTATIONS FOR EACH CATEGORY OF FUEL (DEAD, LIVE)
C
        DO 500 I=1,2
          AA1 = 0.0
          AA2 = 0.0
          AA3 = 0.0
          AA4 = 0.0
          AA5 = 0.0
          LHV1 = 0.0
          JM = INT(NOCLAS(I))
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
C       WHEN DOING THE DEAD HERB CATEGORY, USE MOISTURE FOR DEAD 1-HR FUELS
              IF ((I .EQ. 1) .AND. (J .EQ. 4)) THEN
                QIG(I, J) = 250.0 + 1116.0 * MOIS(I, 1)
                MCSA(I) = MCSA(I) + AX * MOIS(I, 1)
              ELSE
                QIG(I, J) = 250.0 + 1116.0 * MOIS(I, J)
                MCSA(I) = MCSA(I) + AX * MOIS(I, J)
              ENDIF

              BSE(I) = BSE(I) + AX * SILFRE
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
              IF (AI(I) .EQ. 0.0) AI(I) = AI(I) + 1E-9
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
  520       CONTINUE
            BETA = MCSA(I) / (MEXT(I) + 1E-9)
            MDCSA(I) = 1.0 - BETA * (2.59 - BETA
     &           * (5.11 - BETA * 3.52))
            IF (MEXT(I) .LT. MCSA(I)) MDCSA(I) = 0
            IF (BSE(I) .NE. 0.0) BARNS(I) = .174 / (BSE(I) ** .19)
            IF (BARNS(I) .GT. 1.0) BARNS(I) = 1.0
            SIGMA = SIGMA + FX(I) * SIGMA1(I)
            IR(I) = WO1(I) * LHV1 * MDCSA(I) * BARNS(I)
          ENDIF
  500   CONTINUE
C
C       DEAD FUEL TOO MOIST TO SPREAD FLAME
C
        IF (MDCSA(1) .LE. 0.0) FLAG(1) = 1
C
C       BEGIN FINAL COMPUTATIONS
C       ONLY DO FINAL CALCS IF MDCSA(1) > 0

        IF (MDCSA(1) .GT. 0.0) THEN
C
C         BULK DENSITY
C         RHOP1 = SUM4 / (SUM1 + 1E-9)

          RHOP1 = SUM1 / DEPTH
C
C         PACKING RATIO
C         BETA1 = SUM2 * RHOP1 / (SUM1 + 1E-9)

          BETA1 = SUM2 / DEPTH
C
C         OPTIMUM PACKING RATIO
C
          IF (SIGMA .EQ. 0.0) SIGMA = 1E-9
          BEST = 3.348 / (SIGMA ** 0.8189)
          RAT = BETA1 / (BEST + 1E-9)
C
C         REACTION INTENSITY WEIGHTED BY SURFACE AREA FRACTION
C
          A1 = 133.0 / (SIGMA ** 0.7913)
          V = SIGMA ** 1.5
          GAMMA = (V * (RAT ** A1) * EXP(A1 * (1.0 - RAT)))
     &         / (495.0 + .0594 * V)
          IR(1) = GAMMA * IR(1)
          IR(2) = GAMMA * IR(2)
          XIR = IR(1) + IR(2)
C
C         HEAT SINK TERMS
C
          RHOBQIG = RHOP1 * SUM3
C
C         PROPAGATING INTENSITY
C
          B = (0.792 + 0.681 * SQRT(SIGMA)) * (0.1 + BETA1)
          XIO = (XIR * EXP(B)) / (192.0 + 0.2595 * SIGMA)
C
C         SLOPE FACTOR PHIS
C
          IF (BETA1 .NE. 0.0) PHIS = 5.275 * TTHETA * TTHETA
     &         / (BETA1 ** 0.3)
C
C         PARAMETERS FOR DETERMINING WIND FACTOR PHIW
C
          XM1 = 0.02526 * (SIGMA ** 0.54)
          XN1 = 0.715 * EXP(-0.000359 * SIGMA)
          C1 = 7.47 * EXP(-0.133 * (SIGMA ** 0.55))
          IF (RAT .NE. 0.0) C1 = C1 / (RAT ** XN1)
C
C         A FEW LINES OF MAXES WERE LEFT OUT BECAUSE
C         NOT NEEDED IN THIS VERSION

          WMAX = 0.9 * XIR
          W = FWIND * 88.0
          PHIW = C1 * (W ** XM1)
          R = XIO * (1.0 + PHIS + PHIW) / (RHOBQIG + 1E-9)
          BYRAMT = XIR * R * 384.0 / SIGMA
C
C         SURF/VOL TOO SMALL
C
          IF ((W .NE. 0.0) .AND. (SIGMA .LT. 175.0)) FLAG(3) = 1
C
C         WIND SPEED EXCEEDS RELIABLE EXTRAPOLATION
C
          IF (W .GT. WMAX) FLAG(2) = 1

        ENDIF

        IF ((FTYP .NE. 2) .OR. (ICALL .EQ. 2)) THEN
          FLAME = FLAME + (0.45 * (BYRAMT / 60.0) ** 0.46) * FWT(INB)
          BYRAM = BYRAM + BYRAMT * FWT(INB)
C
C         SAVE SOME VALUES TO ARRAYS FOR USE IN CALC CROWN FIRES
C
          SSIGMA(FTYP) = SSIGMA(FTYP) + SIGMA * FWT(INB)
          SRHOBQ(FTYP) = SRHOBQ(FTYP) + RHOBQIG * FWT(INB)
          SXIR(FTYP) = SXIR(FTYP) + XIR * FWT(INB)
          SIRXI(FTYP) = SIRXI(FTYP) + XIO * FWT(INB)
          SPHIS(FTYP) = SPHIS(FTYP) + PHIS * FWT(INB)
          SFRATE(FTYP) = SFRATE(FTYP) + R * FWT(INB)
          SCBE(FTYP) = SCBE(FTYP) + C1 * FWT(INB)
        ELSE
          FLAME = 0.45 * (BYRAMT / 60.0) ** 0.46
          BYRAM = BYRAMT

          SSIGMA(FTYP) = SIGMA
          SRHOBQ(FTYP) = RHOBQIG
          SXIR(FTYP) = XIR
          SIRXI(FTYP) = XIO
          SPHIS(FTYP) = PHIS
          SFRATE(FTYP) = R
          SCBE(FTYP) = C1

          GOTO 810
        ENDIF

  800 CONTINUE
  810 CONTINUE

C     COMPUTE FLAME LENGTH AS A FUNCTION OF BYRAM RATHER THAN USING
C     THE WEIGHTED AVARAGE AS COMPUTED IN THE BLOCK ABOVE (NLC 21 AUG 2003)

      FLAME = (0.45 * (BYRAM/ 60.0) ** 0.46)

      HPA = SXIR(FTYP)*384.0/SSIGMA(FTYP)

      IF (DEBUG) WRITE(JOSTND,*) 'FWG1 = ',
     &   FWG(1,1),FWG(1,2),FWG(1,3),FWG(1,4),FWG(1,5),FWG(1,6),FWG(1,7)
      IF (DEBUG) WRITE(JOSTND,*) 'FWG2 = ',FWG(2,1),FWG(2,2)
      IF (DEBUG) WRITE(JOSTND,*) 'SXIR=',SXIR(FTYP)
      IF (DEBUG) WRITE(JOSTND,*) 'SSIGMA=',SSIGMA(FTYP)

      IF (DEBUG) WRITE(JOSTND,900) ICYC,FLAME,BYRAM,HPA
  900 FORMAT(' EXIT FMFINT CYCLE = ',I2,' FLAME,BYRAM,HPA=',3E14.7)

      RETURN
      END

