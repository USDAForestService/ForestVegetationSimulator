      SUBROUTINE HTONT(ISPC,D_IN,Q_IN,BA_FT,HT_FT)
      IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C  THIS SUBROUTINE DOES THE ACTUAL HEIGHT CALCULATIONS FOR 
C  THE ONTARIO VARIANT. THE EQUATIONS WERE MOVED FROM
C  HTGF TO THIS ROUTINE SO THAT THEY COULD BE USED FOR 
C  HEIGHT DUBBING AS WELL.
C  CALLED FROM HTGF AND CRATET
C----------
COMMONS

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'METRIC.F77'

COMMONS

      INTEGER   MAXEQ_P, MAXEQ_SP
      PARAMETER (MAXEQ_P  = 27)
      PARAMETER (MAXEQ_SP =  8)

      INTEGER   ISPC,ISPC2
      REAL      D_IN,Q_IN,BA_FT,HT_FT
      REAL      D2_IN,Q2_IN, BA2_FT,HT2_FT,NEWDBH_IN

      LOGICAL   DEBUG,LSPOK
      INTEGER   OSPMAP_P(MAXSP),OSPMAP_SP(MAXSP)
      INTEGER   KSP
      REAL      BAM,DBHM,DBHQM,SIM,HTM,SHTM,TPHM,X
      REAL      NUM,DEN
    
      REAL      B0(MAXEQ_P),B1(MAXEQ_P),B2(MAXEQ_P)
      REAL      BSI(MAXEQ_P),BDBHQ(MAXEQ_P),BBA(MAXEQ_P)
    
      REAL      ALPHA(MAXEQ_SP),DELTA(MAXEQ_SP),BETA(MAXEQ_SP)
      REAL      PHI(MAXEQ_SP),GAMMA(MAXEQ_SP)

C  INDEX TO SPECIES SPECIFIC COEFFICIENTS FOR HEIGHT EQUATIONS
C  USING PENNER (_P) DIAMETER-HEIGHT MODELS. THESE CORRESPOND
C  TO THE DATA DATA STATEMENTS FOR B0,B1,B2,BSI,BDBHQ,BBA

      DATA OSPMAP_P /
     >  5,  4,  3,  4,  1,  7,  8, 12,  9, 14,  !10
     > 13, 11,  9,  9, 21, 22, 26, 16, 16, 25,  !20
     > 16, 16, 16, 17, 23, 15, 15, 20, 22, 20,  !30
     > 16, 20, 15, 19, 21, 21, 25, 25, 16, 26,  !40
     > 26, 26, 18, 15, 16, 16, 24, 16, 18, 16,  !50
     > 16, 16, 24, 16, 16, 16, 16, 16, 16, 16,  !60
     > 16, 16, 16, 16, 16, 16, 16, 16,  6,  2,  !70
     >  8, 10 /

C  INDEX TO SPECIES SPECIFIC COEFFICIENTS FOR HEIGHT EQUATIONS
C  USING SHARMA-PARTON (2007) (_SP) DIAMETER-HEIGHT MODELS. 
C  TEST: ONLY JACK PINE PLANTATION (69) GROWS LIKE SHARMA MODEL

      DATA OSPMAP_SP /
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !10
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !20
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !30
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !40
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !50
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !60
     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !70
     >  0,  0 /

C  INDEX TO SPECIES SPECIFIC COEFFICIENTS FOR HEIGHT EQUATIONS
C  USING SHARMA-PARTON (2007) (_SP) DIAMETER-HEIGHT MODELS. THESE CORRESPOND
C  TO THE DATA DATA STATEMENTS FOR ALPHA,DELTA,BETA,PHI,GAMMA

!      DATA OSPMAP_SP /
!     >  4,  0,  5,  5,  0,  8,  0,  1,  3,  0,  !10
!     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !20
!     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !30
!     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !40
!     >  6,  2,  7,  0,  0,  0,  0,  0,  0,  0,  !50
!     >  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  !60
!     >  0,  0,  0,  0,  0,  0,  0,  0,  4,  0,  !70
!     >  8,  3 /

C     PENNER MODEL PARAMETERS
                                        
      DATA B0 /
     > 18.1847,  5.3531, 16.8042, 15.8338,  4.8752,
     >  1.7254, 22.042,  14.7619, 16.8201,  5.6734, ! 10
     > 17.6292, 15.585,  11.9821,  9.1537, 17.6005,
     > 18.0739, 14.0772, 12.689,  16.6299, 15.5719, ! 20
     > 14.6377, 18.451,  20.4829, 13.8802, 17.3966,
     > 14.7273, 15.6942 /
      DATA B1 /
     >  0.0354,  0.0622,  0.032,   0.00746, 0.098,
     >  0.1515,  0.0175,  0.0264,  0.0277,  0.0572, ! 10
     >  0.0428,  0.0224,  0.038,   0.058,   0.0703,
     >  0.0724,  0.0689,  0.0589,  0.0658,  0.0313, ! 20
     >  0.0705,  0.0718,  0.0417,  0.0926,  0.0402,
     >  0.0619,  0.0641 /
      DATA B2 /
     >  1.044,   1.0429,  1.1956,  1.7776,  0.9494,
     >  0.7898,  1.2469,  1.0967,  1.2443,  0.9494, ! 10
     >  0.9593,  1.3855,  1.0663,  1.138,   1.0014,
     >  0.9472,  0.9642,  1.135,   0.8726,  1.2336, ! 20
     >  0.9744,  0.9302,  1.1079,  1.1378,  1.1548,
     >  1.0194,  0.9536 /
      DATA BSI /
     >  0.4402,  0.1260,  0.2008,  0.2322,  0.,
     >  0.,      0.1009,  0.0374,  0.,      0.1163, ! 10
     >  0.2185,  0.04,    0.2047,  0.0602,  0.1437,
     >  0.1274,  0.0819,  0.14,    0.3082,  0.2515, ! 20
     >  0.3886,  0.3932,  0.1059,  0.0842,  0.1733,
     >  0.1138,  0.1659 /
      DATA BDBHQ /
     >  0.,      0.6407,  0.,      0.,      0.5207,
     >  0.6616,  0.,      0.,      0.1608,  0.5513, ! 10
     >  0.,      0.,      0.,      0.4227,  0.,
     >  0.,      0.0868,  0.1616,  0.,      0.,     ! 20
     >  0.,      0.,      0.1319,  0.,      0.,
     >  0.3114,  0.1951 /
      DATA BBA /
     >  0.1191,  0.,      0.1271,  0.,      0.2505,
     >  0.2669,  0.0997,  0.2879,  0.1117,  0.1014, ! 10
     >  0.1159,  0.1147,  0.0794,  0.1285,  0.0884,
     >  0.0778,  0.1176,  0.06,    0.1219,  0.1312, ! 20
     >  0.048,   0.,      0.,      0.,      0.0815,
     >  0.0919,  0.0678 /
     
C     SHARMA-PARTON MODEL PARAMETERS; no random parameter

      DATA ALPHA /
     >  6.2496,  3.5881,  3.7096,  1.1806,  0.9701,
     >  2.0151,  2.8705,  9.4916 /
      DATA DELTA /
     >  0.4024,  0.6222,  0.6206,  0.9563,  1.0165,
     >  0.7915,  0.6426,  0.3717 /
      DATA BETA / 
     >  0.0650,  0.0426,  0.0451,  0.0507,  0.0510,
     >  0.0559,  0.0497,  0.0342 /
      DATA PHI /
     >  0.0604,  0.0979,  0.1239,  0.2097,  0.2686,
     >  0.1798,  0.1680,  0.0310 /
      DATA GAMMA /
     >  1.5017,  0.8198,  1.3222,  1.0523,  2.1086,
     >  1.1440,  1.0539,  1.1071 /
     
C     SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'HTONT',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTONT CYCLE =',I5)

C     VARIABLES TO CONVERT TO METRIC EQUIVALENTS

      DBHM   = D_IN  * INtoCM
      BAM    = BA_FT * FT2pACRtoM2pHA
      DBHQM  = Q_IN * INtoCM

      SIM    = SITEAR(ISISP) * FTtoM
      TPHM   = TPROB * HAtoACR
      SHTM   = AVH * FTtoM

C     NOTE THAT THE SHARMA-PARTON MODEL IS NOT USED IN CYCLE 1, AVOIDING
C     THE NON-DEFINITION OF STAND AVERAGE HEIGHT
    
      KSP = OSPMAP_SP(ISPC)
      IF (ICYC.GT.1 .AND. KSP.NE.0) THEN  ! Sharma-Parton height model
      HTM = 1.3 + (ALPHA(KSP)*(SHTM**DELTA(KSP)))
     >  * (1.0-EXP(-BETA(KSP)*((TPHM/BAM)**PHI(KSP))* DBHM))**GAMMA(KSP)
          HT_FT = HTM  * MtoFT
      ELSE ! Penner height model
        KSP = OSPMAP_P(ISPC)
        IF (KSP.GT.0) THEN
          HTM =   1.3
     >            + (B0(KSP)
     >            + (BSI(KSP)   * SIM)
     >            + (BDBHQ(KSP) * DBHQM )
     >            + (BBA(KSP)   * BAM ))
     >            * (1.0 - EXP(-B1(KSP) * DBHM**B2(KSP)))
          HT_FT = HTM  * MtoFT
        ELSE  ! don't grow
          HT_FT = HT_FT + 0.001
        ENDIF
      ENDIF
    
      RETURN
C
C     ENTRY POINT TO COMPUTE DIAMETER GIVEN HEIGHT (FROM RGNTSW)
C     THIS IS THE INVERSE OF THE USUAL RELATIONSHIP. NOTE THAT
C     THE SHARMA-PARTON MODEL IS NOT USED IN CYCLE 1, AVOIDING
C     THE NON-DEFINITION OF STAND AVERAGE HEIGHT
C
      ENTRY ONHTDBH (ISPC2,D2_IN,BA2_FT,HT2_FT,Q2_IN,  NEWDBH_IN)

      DBHM  = D2_IN  * INtoCM
      BAM   = BA2_FT * FT2pACRtoM2pHA
      HTM   = HT2_FT * FTtoM
      DBHQM = Q2_IN  * INtoCM
    
      SIM   = SITEAR(ISISP) * FTtoM
      TPHM  = TPROB * HAtoACR
      SHTM  = AVH   * FTtoM
    
c     Pre-compute numerator and denominator terms of inverted Sharma-Parton model.
c     If NUM is <0 the equation has broken down; revert to Penner model      
      KSP = OSPMAP_SP(ISPC2)
      IF (KSP.NE.0) THEN
        NUM = 1.0-
     >    ((HTM-1.3)/(ALPHA(KSP)*SHTM**DELTA(KSP)))**(1.0/GAMMA(KSP))
        DEN = (BETA(KSP)*(TPHM/BAM)**PHI(KSP))
        LSPOK = .TRUE.
        IF (NUM .LT. 1.0E-3) LSPOK = .FALSE.
      ENDIF

      IF (ICYC.GT.1 .AND. KSP.NE.0 .AND .LSPOK) THEN ! inverse S-P ht model
        X = (-LOG(NUM)/DEN) * CMtoIN
      ELSE ! Penner height model; denominator term constrained to 90%
c            of its assymptotic value, to prevent negative diameter prediction
        KSP = OSPMAP_P(ISPC2)
        IF (KSP.GT.0) THEN
          X = 1.0-(HTM-1.3)/MAX(HTM*1.1,(B0(KSP) + BSI(KSP)*SIM +
     >      BDBHQ(KSP)*DBHQM + BBA(KSP)*BAM))
          X = (-LOG(X)/B1(KSP))**(1.0/B2(KSP)) * CMtoIN
        ELSE
          X = D2_IN + 0.0001
        ENDIF
      ENDIF
      NEWDBH_IN = X

      RETURN
      END