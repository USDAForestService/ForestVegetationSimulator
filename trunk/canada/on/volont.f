        SUBROUTINE VOLONT
        IMPLICIT NONE
C----------
C CANADA-ON $Id$
C----------
C  THIS SUBROUTINE CALCULATES THE SPECIAL VOLUME EQUATIONS SUBMITTED
C  BY ONTARIO: ZAK AND HONER.
C
C  IT CONTAINS THREE ENTRY POINTS:
C     HONER    - CALCULATES VOLUME USING HONER EQUATIONS
C     ZAKVOL   - CALCULATES VOLUME USING ZAKRZWSKI EQUATIONS
C     ZAKHT    - CALCULATES HEIGHT FOR MERCH VOL ZAKVOL EQUATION
C
C  FOR EASTERN VARIANTS:
C    VN = TOTAL MERCH CUBIC (PULPWOOD AND SAWLOG)
C    VM = NET MERCH VOLUM (NMV) - MOWRASKI CULL, M.PENNER 2010
C    BBFV = MERCH SAWLOG BOARD FOOT VOLUME
C
C  THIS SUBROUTINE IS CALLED IF THE USER ENTERS METHOD 8 IN
C  THE VOLUME KEYWORD.
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'VOLSTD.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'METRIC.F77'

COMMONS

      LOGICAL DEBUG,LTOTAL
      INTEGER J,ISPC,IT
      REAL    D_CM,D_IN,H_M,H_FT,ST_M,U_FT
      REAL    TOTCU,SAWCU,X,C,BARK,DIB,ZHT,CV
      REAL    VN,VM,VMAX

      INTEGER ISPMPZ(MAXSP),ISPMPH(MAXSP),ISPMPM(MAXSP)
      REAL B1(26), C1(26), C2(26)
      REAL R1(26), R2(26), R3(26)
      REAL B2(12), G2(12)
      REAL AM(14), BM(14)

C     VARIABLES USED BY ZAK CALCULATIONS

      REAL D, H, U, TD_CM, B
      REAL CA_H, Z0, S, K, R
      REAL T1, T3, T5, T6, T7, T9, T11, T13, T15, T17
      REAL T21, T22, T26, T30, T35, T43, T48, T56, T60
      REAL T62, T69, T71, T73, T74, T75, T79
      REAL T84, T86, T88, T90, T92, T99, T105, T118

      REAL A2, Y11, Y22, B12
      REAL H2, H3, H4, TT

C     DEFINE SPECIES MAPPING ARRAYS
C     MAP FROM FVS SPECIES TO DATA VALUES
C       ISPMPZ = SPECIES MAPPING FOR ZAK EQUATIONS
C       ISPMPH = SPECIES MAPPING FOR HONER EQUATIONS
C       ISPMPM = SPECIES MAPPING FOR MOWRASKI CULL EQUATIONS

      DATA ISPMPZ/ ! Zak
     &  3, 3, 2, 2, 1, 5, 4, 6, 4,12,
     &  7,12,12,12,11,11,11, 8, 8,11,
     & 11,11,11,10,11, 8, 8,11,11,11,
     & 11,11,11,11,11,11,11,11,11, 9,
     &  9, 9,10,11,11,11,11,11,11,11,
     & 11,11,11,11,11,11,11,11,11,11,
     & 11,11,11,11,11,11,11,11, 3, 1,
     &  5, 4/

      DATA ISPMPH/ ! Honer
     &   3, 4, 2, 2, 1, 5, 7, 9, 6,11,
     &  10, 8, 1,10,18,18,26,13,13,23,
     &  20,22,22,14,21,12,12,17,19,16,
     &  16,16,16,16,16,16,19,19,19,24,
     &  26,25,15,12,19,12,22,12,12,12,
     &  12,12,22,12,19,22,12,22,12,16,
     &  23,23,23,12,12,12,12,22, 3, 1,
     &  5, 6/

      DATA ISPMPM/ ! Mowraski
     &   3, 3, 2, 2, 1, 5, 5, 6, 4, 8,
     &   7, 9, 4, 7,14,14,10,13,14,13,
     &   4,14,14,13,13,12,13,13,13,13,
     &  14,13,13,13,14,13,13,13,13,10,
     &  10,10,11,13,13,13,13,13,14,14,
     &  13,13,13,13,13,13,13,13,13,13,
     &  13,13,13,14,14,14,13,13, 3, 1,
     &   5, 4 /

C     PARAMETERS OF THE HONER EQUATIONS
C     =================================
C     B1 here is B2 in documentation
C     1=white pine, 2=red pine, 3=jack pine, 4=Scotch pine
C     5=white spruce, 6=black spruce, 7=red spruce, 8=Eastern hemlock
C     9=Balsam fir, 10=Eastern/northern white cedar, 11=tamarack
C     12=Sugar maple, 13=red maple, 14=yellow birch,
C     15=white/paper birch, 16=red oak, 17=American beech, 18=black ash
C     19=white ash, 20=American elm, 21=basswood
C     22=Ironwood/Eastern hophornbeam & mountain ash & rock elm,
C     23=black/pin cherry,
C     24=largetooth/bigtooth aspen, 25=balsam poplar, 26=quaking aspen
C
C     Note equal parameters for (12, 13, 18, 19), (6,11)

      DATA B1/
     &   0.184,0.151,0.151,0.151,0.176,0.164,0.169,0.155,0.152,0.155,
     &   0.164,0.145,0.145,0.181,0.176,0.145,0.145,0.145,0.145,0.145,
     &   0.145,0.145,0.145,0.127,0.127,0.127/
      DATA C1/
     &   0.691,0.71, 0.897,0.897,1.44, 1.588,1.226,1.112,2.139,4.167,
     &   1.588,1.046,1.046,1.449,2.222,1.512,0.959,1.046,1.046,0.634,
     &   0.948,1.877,0.033,-0.312,0.42,-0.312/
      DATA C2/
     &   363.676,355.623,348.53,348.53,342.175,333.364,315.832,
     &   350.092,301.634,244.906,333.364,383.972,383.972,344.754,
     &   300.373,336.509,334.829,383.972,383.972,440.496,401.456,
     &   332.585,393.336,436.683,394.644,436.683/
      DATA R1/
     &   0.9735,0.9672,0.9635,0.9672,0.9611,0.9526,0.9645,0.9645,
     &   0.9352,0.9645,0.9526,0.9057,0.9057,0.8778,0.9087,0.9057,
     &   0.9057,0.9057,0.9057,0.9057,0.9057,0.9057,0.9057,0.9087,
     &   0.9354,0.9354/
      DATA R2/
     &   -0.2348,-0.0393,-0.15,  -0.0393,-0.2456,-0.1027,-0.1616,
     &   -0.1616,-0.0395,-0.1616,-0.1027,-0.0708,-0.0708,-0.2417,
     &   -0.3049,-0.0708,-0.0708,-0.0708,-0.0708,-0.0708,-0.0708,
     &   -0.0708,-0.0708,-0.3049, 0.0957, 0.0957/
      DATA R3/
     &   -0.7378,-1.0523,-0.8081,-1.0523,-0.6801,-0.8199,-0.7945,
     &   -0.7945,-0.8147,-0.7945,-0.8199,-0.8375,-0.8375,-0.5247,
     &   -0.5107,-0.8375,-0.8375,-0.8375,-0.8375,-0.8375,-0.8375,
     &   -0.8375,-0.8375,-0.5107,-1.1613,-1.1613/

C     PARAMETERS OF THE ZAK EQUATIONS
C     ===============================
C     These are the current TCM/PAM coefficients
C     1=white pine, 2=red pine, 3=jack pine,
C     4=black spruce, 5=white spruce, 6=Balsam fir,
C     7=Eastern white cedar, 8=hard maple, 9=aspen,
C     10=white birch, 11=other hardwoods,12=Other softwoods

      DATA B2/
     &  -1.80769,-1.75095,-1.72170,-1.62979,-1.61060,
     &  -1.34130,-1.57624,-1.37784,-1.49699,-1.24598,
     &  -1.31327,-1.89146/

      DATA G2/
     &  0.90926,0.87184,0.84388,0.77509,0.82290,
     &  0.50717,0.74247,0.55538,0.63057,0.43204,
     &  0.45938,1.00000/

C     PARAMETERS OF THE MOWRASKI CULL EQUATIONS
C     Cull = (1-exp(-AM*age))^BM
C     SEE ISPMPM FOR SPECIES MAPPINGS
C     =========================================
      DATA AM /
     & 0.004286, 0.000101, 0.012639, 0.003614, 0.000364,
     & 0.003204, 0.00894,  0.003614, 0.000825, 0.005207,
     & 0.00329,  0.000324, 0.003759, 0.015715 /
      DATA BM /
     & 2.5398,   0.9831,   8.3752,   3.132,    1.1654,
     & 1.5584,   4.5996,   3.132,    1.0931,   1.4052,
     & 1.5971,   0.4106,   1.34313,  4.06754 /

C     =================================
C     ENTER HONER EQUATION HERE

      ENTRY HONER (VN,VM,ISPC,D_IN,H_FT,IT)

C     CHECK FOR DEBUG

      CALL DBCHK(DEBUG,'VOLONT',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)'IN VOLONT, HONER'

      TOTCU = 0.0
      SAWCU = 0.0

      J = ISPMPH(ISPC)

C     CHANGE TREE INFORMATION INTO METRIC FOR VOLUME CALCULATIONS

      D_CM = D_IN * INtoCM
      H_M = H_FT * FTtoM
      TD_CM = TOPD(ISPC) * INtoCM
      ST_M = STMP(ISPC) * FTtoM

C     CALCULATE THE TOTAL VOLUME

C     VN=0.0043891*DBH*DBH*(1-0.04365*B2)**2/(C1 + 0.3048*C2/HT)

      TOTCU = 0.0043891 * D_CM * D_CM *
     &       (1 - 0.04365*B1(J))**2 /
     &       (C1(J) + 0.3048*C2(J)/H_M)

C     CALCULATE THE MERCHANTABLE VOLUME
C
C     Vnm = Vm *(r1+r2x+r3x**2)
C     x=tm**2 * dm**(-2)*(1-.04365b2)**-2 * (1+sm/hm)
C     tm = top diameter inside bark = TOPD
C     dm = dbh outside bark = D
C     sm = stump height = STMP
C     hm = total height = H

      X = TD_CM**2 * (1 + ST_M/H_M) /
     &                 (D_CM**2 * (1 - 0.04365*B1(J))**2)
      SAWCU = TOTCU * (R1(J) + R2(J)*X + R3(J)*X*X)

C     CHANGE VOLUME INFORMATION INTO IMPERIAL

      TOTCU = TOTCU * M3toFT3
      SAWCU = SAWCU * M3toFT3

C  SET RETURN VALUES.

      IF(DEBUG)WRITE(JOSTND,*)' TOTCU=',TOTCU,' SAWCU=',SAWCU
      VN=TOTCU
      VMAX=VN
      VM=SAWCU
      IF(DEBUG)WRITE(JOSTND,*)' RETURNING VN,VMAX,VM= ',
     &VN,VMAX,VM

      RETURN

C     =================================
C     ENTER ZAK VOLUME CALCULATION HERE

      ENTRY ZAKVOL (VN,ISPC,D_IN,H_FT,U_FT,IT,BARK,LTOTAL)

C     CHECK FOR DEBUG

      CALL DBCHK(DEBUG,'VOLONT',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)'IN VOLONT, ZAKVOL'

C     CHANGE TREE INFORMATION INTO METRIC FOR VOLUME CALCULATIONS

      D = D_IN * INtoCM
      H = H_FT * FTtoM
      U = U_FT * FTtoM

      IF(H .LT. 1.40) THEN
        VN = 0.0
        RETURN
      ENDIF

C     WHEN LTOTAL IS TRUE, THEN A ZERO-HEIGHT STUMP IS NEEDED.
C     TOTAL VOLUME(GROUND TO TIP = GTV Gross Total Volume)ASSUMES NO STUMP

      IF (LTOTAL) THEN
        B = 0.0
      ELSE
        B = STMP(ISPC) * FTtoM
      ENDIF

C     SET INITIAL VALUES

      J = ISPMPZ(ISPC)

C     CALCULATE TOTAL CUBIC FOOT VOLUME (VN)

      IF (U .GT. H) U = H

      C = D * BARK

      A2 = 1. + (H / D)
      Z0 = 1. - 1.3 / H
      Y11 = (Z0 - A2)
      Y22 = (Z0**2 + B2(J) * Z0**3 + G2(J) * Z0**4)
      B12 = Y11 / Y22
      K = 7.853982E-05 * C**2 * B12
      H2 = H*H
      H3 = H2*H
      H4 = H3*H
      TT = (-1./12. * (12. * H4 * A2**3 * LOG(-H + U + A2 * H) * B2(J) +
     &      3. * G2(J) * U**4 + 6. * H2 * G2(J) * A2**2 * U**2 -
     &      12. * H * G2(J) * U**3 - 12. * H3 * G2(J) * A2 * U -
     &      4. * B2(J) * H * U**3 - 12. * H3 * A2 * B2(J) * U +
     &      12. * B2(J) * H2 * U**2 - 12. * H3 * A2**2 * G2(J) * U +
     &      18. * G2(J) * H2 * U**2 - 12. * H3 * A2**2 * B2(J) * U +
     &      12. * H4 * A2**4 * LOG(-H + U + A2 * H) * G2(J) -
     &      12. * H3 * A2**3 * G2(J) * U - 4. * H * G2(J) * A2 * U**3 -
     &      12. * H3 * G2(J) * U + 6. * H2 * B2(J) * A2 * U**2 -
     &      12. * H3 * A2 * U + 12. * H4 * A2**2 * LOG(-H + U + A2 * H)-
     &      12. * U * H3 - 12. * H3 * B2(J) * U +
     &      12. * H2 * G2(J) * A2 * U**2 + 6. * H2 * U**2) / H3 +
     &      1./12. * (-12. * H3 * B2(J) * B -
     &      4. * H * G2(J) * A2 * B**3 +
     &      12. * H4 * A2**3 * LOG(-H + B + A2 * H) * B2(J) -
     &      12. * H3 * A2 * B2(J) * B - 12. * H3 * A2**2 * G2(J) * B -
     &      12. * H3 * A2**2 * B2(J) * B - 12. * H3 * A2**3 * G2(J) * B-
     &      12. * H3 * G2(J) * A2 * B - 12. * H * G2(J) * B**3 -
     &      4. * B2(J) * H * B**3 + 12. * H2 * G2(J) * A2 * B**2 +
     &      18. * G2(J) * H2 * B**2 + 6. * H2 * B2(J) * A2 * B**2 +
     &      12. * H4 * A2**4 * LOG(-H + B + A2 * H) * G2(J) +
     &      6. * H2 * G2(J) * A2**2 * B**2 - 12. * H3 * A2 * B +
     &      6. * H2 * B**2 + 12. * H4 * A2**2 * LOG(-H + B + A2 * H) +
     &      12. * B2(J) * H2 * B**2 + 3. * G2(J) * B**4 - 12. * B * H3 -
     &      12. * H3 * G2(J) * B) / H3)

C     SET RETURN VALUES HERE

      VN = TT * K * M3toFT3

      RETURN

C     ================================
C     ENTER ZAK HEIGHT CALCULATION HERE

      ENTRY ZAKHT (D_IN,H_FT,BARK,ISPC,IT,ZHT)

C     THIS ROUTINE IS USED FOR DETERMINING THE HEIGHT THAT SHOULD BE USED
C     WHEN CALCULATING MERCHANTABLE VOLUME.
C
C     CALLED BY VARVOL

C     CHECK FOR DEBUG

      CALL DBCHK(DEBUG,'VOLONT',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)'IN VOLONT, ZAKHT'

      D = D_IN * INtoCM
      H = H_FT * FTtoM
      DIB = D * BARK
      U = MAX(1.0,TOPD(ISPC) * INtoCM)

      J = ISPMPZ(ISPC)

      CA_H = (7.853982E-05)*U*U
      Z0 = 1. - 1.3 / H
      S = 1. + (H / D)
      K = ((Z0-S)*(7.853982E-05)*DIB**2)/(Z0**2 + B2(J)*Z0**3 +
     >     G2(J)*Z0**4)
      T1 = 1./G2(J)
      T3 = SQRT(3.0)
      T5 = B2(J)*B2(J)
      T6 = T5*K
      T7 = K*CA_H
      T9 = S*G2(J)
      T11 = CA_H*CA_H
      T13 = S*T5
      T15 = K*K
      T17 = G2(J)*G2(J)
      T21 = K*T11
      T22 = T5*B2(J)
      T26 = CA_H*B2(J)
      T30 = S*S
      T35 = S*T15*K
      T43 = T17*G2(J)
      T48 = T15*CA_H
      T56 = T5*T5
      T60 = 27.0*T11*CA_H*T17 - CA_H*T5*T15 - 4.0*T21*T22 +
     &      4.0*CA_H*G2(J)*T15 - 80.*T26*T15*S*G2(J) +
     &    128.*CA_H*T30*T17*T15 - 16.0*T35*G2(J) + 6.*T21*T13*G2(J) -
     &    192.*T21*B2(J)*T30*T17 - 256.*T21*T30*S*T43 +
     &     18.*T21*B2(J)*G2(J) + 18.*T48*T22*S - 144.*T21*S*T17 -
     &    144.*T48*T30*G2(J)*T5 + 27.*T48*T30*T56 + 4.*T35*T5
      T62 = SQRT(CA_H * T60)
      T69 = ((9.*T7*B2(J) - 72.*T7*T9 + 27.*T11*G2(J) +
     &        27.*T7*T13 + 2.*T15 + 3.*T3*T62)/T15/T43)**(1.0/3.0)
      T71 = K*G2(J)
      T73 = 54.0**(1.0/3.0)
      T74 = T73*T73
      T75 = T69*T69
      T79 = T73*CA_H
      T84 = 1.0/K
      T86 = 1.0/T69
      T88 = SQRT((27*T6*T69 - 72*T71*T69 + 2*T74*T75*K*T17 +
     &            36*T79*B2(J) + 144*T79*T9 + 12*T73*K)*T84*T86)
      T90 = SQRT(6.0)
      T92 = T69*T88
      T99 = T88*T73
      T105 = T3*T69
      T118 = SQRT((27.*T6*T92 - 72.*T71*T92 - T88*T74*T75*K*T17 -
     &             18.*T99*T26 - 72.*T99*CA_H*S*G2(J) - 6.*T99*K -
     &           324.*T105*B2(J)*K*G2(J) - 648.*T105*CA_H*T17 +
     &             81.*T105*T22*K)*T84*T86/T88)
      R = -B2(J)*T1/4.0 - T3*T1*T88/36.0 + T90*T1*T118/36.0

      ZHT = H*(1-R)
      IF (ZHT .LT. 0.0 .OR. ZHT .GT. H) ZHT = 0.0
      ZHT = ZHT * MtoFT

      RETURN
C
C     ================================
C     ENTER MOWRASKI CULL CALCULATION HERE

      ENTRY MOWRASKI (VM,IT,ISPC,CV)

C     THIS ROUTINE IS USED FOR DETERMINING THE NET MERCHANTABLE
C     VOLUME, BASED ON INPUT VM (=GMV=GROSS MERCH VOL) AND INDIVIDUAL
C     AGE (IF MISSING, THEN STAND-AGE)
C     CALLED BY **VARVOL** (OBFVOL)

C     CHECK FOR DEBUG

      CALL DBCHK(DEBUG,'VOLONT',6,ICYC)
      IF(DEBUG)WRITE(JOSTND,*)'IN VOLONT, MOWRASKI'

      J = ISPMPM(ISPC)
      CV = MAX(0.0, VM)

      X = 1.0 - (1.0 - EXP(-AM(J)*ABIRTH(IT)))**BM(J)
      CV = VM * X
      RETURN

      END
