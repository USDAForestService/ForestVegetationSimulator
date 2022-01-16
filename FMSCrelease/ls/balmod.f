      SUBROUTINE BALMOD(ISPC,D,BA,RMSQD,GM,DEBUG)
      IMPLICIT NONE
C----------
C LS $Id$
C----------
C  THIS SUBROUTINE COMPUTES THE VALUE OF A GROWTH MODIFIER BASED
C  ON BAL. ORIGINALLY THIS WAS JUST PART OF THE LARGE TREE DIAMETER
C  GROWTH SEQUENCE. HOWEVER, THERE NEEDS TO BE A SIMILAR ACCOUNTING
C  OF STAND POSITION IN THE LARGE TREE AND SMALL TREE HEIGHT GROWTH
C  ESTIMATION SEQUENCE. THIS ROUTINE IS CALLED BY DGF, HTGF, AND
C  RGNTHW.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C----------
      LOGICAL DEBUG
      REAL CHECK(MAXSP),BAMAX1(MAXSP),
     & B1(MAXSP),B2(MAXSP),B3(MAXSP),B4(MAXSP),C1(MAXSP),C2(MAXSP)
      REAL GM,RMSQD,BA,D,BETA,BATEMP,ARG,OMEGA,EXPVAL
      INTEGER ISPC
C
C----------
C  VALUES FOR CHECKING D/RMSQD IN EXPRESSION FOR OMEGA
C----------
      DATA CHECK/
     & 0.33, 0.33, 0.50, 0.09, 0.28, 0.10, 0.10, 0.03, 0.21, 0.33,
     & 0.02, 0.00, 0.28, 0.28, 0.01, 0.01, 0.33, 0.33, 0.33, 0.33,
     & 0.16, 0.16, 0.16, 0.49, 0.43, 0.33, 0.33, 0.33, 0.31, 0.01,
     & 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.24, 0.24, 0.24, 0.68,
     & 0.59, 0.59, 0.07, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
     & 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,
     & 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01/
C----------
C  COEFFICIENTS FOR OMEGA PORTION OF THE MODIFIER EQUATION
C----------
      DATA B1/
     % 2*1.78,.719,2.31,1.36,2*5.0,1.76,3.8,1.78,2.54,1.27,2*1.360,
     % 2*5.0,4*1.400,3*5.0,0.679,1.590,3*1.170,5.000,4*1.980,
     % 3*1.980,3*1.660,1.130,2*1.080,1.980,25*1.980/
      DATA B2/
     %  2*3.0,10.9,1.67,2.64,2*1.01,1.51, 1.52,3.0, 1.140, 1.340,
     %  2*2.64,2*.568,4*2.030,3*.970,10.970, 3.270,3*4.590,
     %  1.380,7*.974,3*2.620, 4.640,2*6.600, 1.750,
     %  25*0.974/
      DATA B3/
     %  2*16.2,1688.0,3.94,11.5,2*3.64,2.63,6.54,16.2, 2.26, 1.05,
     %  2*11.5,2*1.83,4*10.40,3*4.40,1568.0,26.7,3*29.19,8.26,4*1.64,
     %  3*1.64,3*9.97,164.60,2*346.09, 3.67,25*1.64/
      DATA B4/
     %  2*.227,.375,.0,.386,2*.0,.233,.348,.227,2*.0,2*.386,2*.063,
     %  4*.694,3*.268,.483,.412,3*.430,.326,7*.000,3*.515,
     %  .648,2*.395,.232,25*.000/
C----------
C  COEFFICIENTS FOR THE BETA PORTION OF THE MODIFIER EQUATION.
C----------
      DATA C1/
     %  2*.402,2*2.030,.097,2*1.507,.927,.522,.039,.526,.046,2*.097,
     %  2*.260,4*.181,3*.100,0.202,0.353,3*.142,0.453,4*.051,
     %  0.278,2*1.365,3*.280,0.093,2*.209,0.110,25*0.278/
      DATA C2/
     %  2*.230,2*-.354,.755,2*-.520,-.299,.173,1.0,.136,1.0,2*.755,
     %  2*.419,4*.445,3*.629,.454,.182,3*.524,.340,4*1.000,
     %  .365,2*-.208,3*.228,1.000,2*.543,.678,25*.365/
C----------
C  COEFFICIENTS FOR MAXIMUM BASAL AREA.
C  (IF THESE CHANGE, ALSO CHANGE THEM IN **SITSET**)
C----------
      DATA BAMAX1/
     %  2*225,300,350,300,2*350,325,300,250,350,3*300,9*250,
     %  10*250,275,8*250,275,25*275/
C
      GM=1.
C----------
C  COMPUTE OMEGA PORTION OF THE MODIFIER
C----------
      IF (RMSQD .LE. 0.0) THEN
         OMEGA = B4(ISPC)
      ELSEIF (D/RMSQD.LT.CHECK(ISPC)) THEN
         OMEGA = B4(ISPC)
      ELSE
C        ----------
C        Computation of the value used in the EXP function was 
C        seperated from OMEGA calculation so that underflow of single
C        precision variable is avoided.                 LD 2/27/19
C                                     |--------------|
C        OMEGA = B1(ISPC)*(1-EXP(-1.0*B2(ISPC)*D/RMSQD))**B3(ISPC)
C    &           +B4(ISPC)
C        ----------
         EXPVAL = B2(ISPC)*D/RMSQD
         IF (EXPVAL .GT. 86) EXPVAL = 86.0
         OMEGA = B1(ISPC)*(1-EXP(-1.0*EXPVAL))**B3(ISPC)+B4(ISPC)
      ENDIF
      IF (DEBUG)WRITE(JOSTND,*)'ISPC=',ISPC,
     &    ' D=',D,' OMEGA=',OMEGA,' RMSQD=',RMSQD
C----------
C  COMPUTE BETA PORTION OF THE MODIFIER
C----------
      BETA = C1(ISPC)*(RMSQD+1.0)**C2(ISPC)
      IF(BA.LE.1.)THEN
        BATEMP=1.
      ELSE
        BATEMP=BA
      ENDIF
      ARG = (BAMAX1(ISPC)/BATEMP)-1.0
      IF (ARG .LT. 0.0) ARG=0.0
C----------
C  COMPLETE THE COMPUTATION OF THE MODIFIER
C----------
      GM = 1.0-EXP(-1.0*OMEGA*BETA*SQRT(ARG))
      IF(GM.LT.0.2)GM=0.2
      RETURN
      END
