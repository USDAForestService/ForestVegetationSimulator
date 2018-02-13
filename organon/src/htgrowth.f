C     ORGANON GROWTH AND YIELD MODEL
C     SUBROUTINES INCLUDED:
C         HTGRO1
C         HTGRO2
C         HS_HG
C         B_HG
C         F_HG
CC         NCHG
C         WHHLB_HG
C         WHHLB_SI_UC
C         WHHLB_GEA
C         WHHLB_H40
C         HG_SWO
C         HG_NWO
C         HG_SMC
C         HG_RAP
C         LIMIT
C         HD_NWO
C         HD_SMC
C         HD_RAP
C         HG_FERT
C         HG_THIN
C         RAGEA
C         RAH40
********************************************************************************
      SUBROUTINE HTGRO1(K,M,ON,VERSION,CYCLG,IB,TDATAI,TDATAR,SI_1,SI_2,
     1                  CCH,CALIB,PN,YF,BABT,BART,YT,OLD,PDEN,GROWTH)
      IMPLICIT NONE
C     CALCULATE 5-YEAR HEIGHT GROWTH
C
C     K = TREE NUMBER TO GROW HEIGHT
C
C     M = METHOD OF TRIPLING
C       0 = NO TRIPLING
C       1 = TRIPLE EVERY TREE
C       2 = TRIPLE EVERY OTHER TREE
C       3 = RANDOM ERROR
C
C     ON = EVERY OTHER TREE TRIPLING DESIGNATOR
C        1 = TRIPLE THIS TREE
C        0 = DON'T TRIPLE THIS TREE

      INTEGER*4 K,M,ON,VERSION,CYCLG,IB,TDATAI(2000,3),ISPGRP,I,
     1          ISISP
      REAL*4 TDATAR(2000,8),SI_1,SI_2,CCH(41),CALIB(6,18),PN(5),YF(5),
     1       BABT,BART(5),YT(5),OLD,GROWTH(2000,4),CR,DBH,H,XI,XXI,TCCH,
     2       SITE,GEAGE,GP,PHTGRO,IDXAGE,HG,FERTADJ,THINADJ,PDEN
C      INTEGER*4 IANS,IYN
C
C     CALCULATE HEIGHT GROWTH
C
      CR=TDATAR(K,3)
      DBH=TDATAR(K,1)
      H=TDATAR(K,2)
      ISPGRP=TDATAI(K,2)
C
C     FOR MAJOR SPECIES
C
      IF(TDATAI(K,2) .LE. IB)THEN
           XI=40.0*(TDATAR(K,2)/CCH(41))
           I=INT(XI)+2
           XXI=FLOAT(I)-1.0
           IF(TDATAR(K,2) .GE. CCH(41)) THEN
              TCCH=0.0
           ELSE IF(I.EQ.41) THEN
              TCCH=CCH(40)*(40.0-XI)
           ELSE
              TCCH=CCH(I)+(CCH(I-1)-CCH(I))*(XXI-XI)
           ENDIF
C
C     COMPUTE HEIGHT GROWTH OF UNTREATED TREES
C
           SELECT CASE(VERSION)
              CASE(1)
C
C                POTENTIAL HEIGHT GROWTH FROM HANN AND SCRIVANI'S (1987) DOMINANT
C                HEIGHT GROWTH EQUATION
C
                 IF(TDATAI(K,1) .EQ. 122)THEN
                    SITE=SI_2
                    ISISP=2
                 ELSE
                    SITE=SI_1
                    IF(TDATAI(K,1) .EQ. 81)SITE=(SI_1+4.5)*0.66-4.5
                    ISISP=1
                 ENDIF
                 CALL HS_HG(ISISP,SITE,TDATAR(K,2),GEAGE,PHTGRO)
                 IDXAGE=500.0
                 CALL HG_SWO(ISPGRP,PHTGRO,CR,TCCH,HG)
              CASE(2)
                 GP=5.0
                 IF(TDATAI(K,2) .EQ. 3)THEN
C
C                    POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
C                    DOMINANT HEIGHT GROWTH
C
                     SITE = SI_2+4.5
                     CALL F_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
                 ELSE
C
C                    POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
C                    GROWTH FOR DOUGLAS-FIR
C                    AND GRAND FIR
C
                     SITE=SI_1+4.5
                     CALL B_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
                 ENDIF
                 IDXAGE=120.0
                 CALL HG_NWO(ISPGRP,PHTGRO,CR,TCCH,HG)
              CASE(3)
                 GP=5.0
                 IF(TDATAI(K,2) .EQ. 3)THEN
C
C                    POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
C                    DOMINANT HEIGHT GROWTH
C
                     SITE = SI_2+4.5
                     CALL F_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
                 ELSE
C
C                    POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
C                    GROWTH FOR DOUGLAS-FIR
C                    AND GRAND FIR
C
                     SITE=SI_1+4.5
                     CALL B_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
                 ENDIF
                 IDXAGE=120.0
                 CALL HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
              CASE(4)
                 GP=1.0
                 IF(TDATAI(K,2) .EQ. 1)THEN
C                    POTENTIAL HEIGHT GROWTH FROM WEISKITTEL, HANN, HIBBS, LAM,
C                    AND BLUHM (2009) RED ALDER TOP HEIGHT GROWTH
C
                     SITE = SI_1+4.5
                     CALL WHHLB_HG(SITE,PDEN,TDATAR(K,2),GP,GEAGE,
     1                             PHTGRO)
C                 WRITE(60,1000) SITE,PDEN,TDATAR(K,2),GP,GEAGE,PHTGRO,
C     1                          DBH,H,CR,TCCH
C 1000            FORMAT(10F12.6)
                 ELSEIF(TDATAI(K,2) .EQ. 3)THEN
C
C                    POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
C                    DOMINANT HEIGHT GROWTH
C
                     SITE=-0.432 + 0.899 * (SI_2+4.5)
                     CALL F_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
                 ELSE
C
C                    POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
C                    GROWTH FOR DOUGLAS-FIR
C                    AND GRAND FIR
C
                     SITE=SI_2+4.5
                     CALL B_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
                 ENDIF
                 IDXAGE=30.0
                 CALL HG_RAP(ISPGRP,PHTGRO,CR,TCCH,HG)
C                 WRITE(60,1000) SITE,GEAGE,PHTGRO,DBH,H,CR,TCCH,HG
C 1000            FORMAT(2F7.2,F10.7,2F10.5,2F10.6,F10.7)
           ENDSELECT
           IF(TDATAI(K,2).LE.IB.AND.GEAGE.GT.IDXAGE)THEN
                OLD=OLD+1.
                IF(M .EQ. 1 .OR. (M .EQ. 2 .AND. ON .EQ. 1))OLD=OLD+2.
           ENDIF
           CALL HG_FERT(CYCLG,VERSION,ISPGRP,SI_1,PN,YF,FERTADJ)
           CALL HG_THIN(CYCLG,VERSION,ISPGRP,BABT,BART,YT,THINADJ)
C      IF(K.EQ.161) THEN
C        WRITE(*,*) ' PHTGRO = ',PHTGRO
C        WRITE(*,*) ' HG=',HG,' THINADJ=',THINADJ,' FERTADJ=',FERTADJ
C        WRITE(*,*) ' CR=',CR,' TCCH=',TCCH
C        WRITE(*,1600)
C 1600   FORMAT(1X,' '\)
C        IANS = IYN(2)
C      ENDIF
           GROWTH(K,1)=HG*THINADJ*FERTADJ
C           GROWTH(K,1)=HG
           CALL LIMIT(VERSION,TDATAI(K,1),TDATAR(K,1),TDATAR(K,2),
     1                GROWTH(K,2),GROWTH(K,1))
C           GROWTH(K,3)=GROWTH(K,3)+GROWTH(K,1)
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HTGRO2(K,VERSION,IB,TDATAI,TDATAR,RASI,CALIB,
     1                  GROWTH)
      IMPLICIT NONE
C     CALCULATE 5-YEAR HEIGHT GROWTH
C
C     K = TREE NUMBER TO GROW HEIGHT
C

      INTEGER*4 K,VERSION,IB,TDATAI(2000,3),ISPGRP
C      INTEGER*4 IANS,IYN
      REAL*4 TDATAR(2000,8),RASI,CALIB(6,18),GROWTH(2000,4),CR,DBH,
     1       PDBH,PRDHT1,PRDHT2,PRDHT,GEARA,RAH1,RAH2,RAHG
C
C     CALCULATE HEIGHT GROWTH FOR MINOR SPECIES
C
      CR=TDATAR(K,3)
      DBH=TDATAR(K,1)
      ISPGRP=TDATAI(K,2)
C      IF(K .GE. 50) THEN
C      WRITE(*,*) ' ISPGRP = ',ISPGRP,' RASI = ',RASI,' GEARA = ',GEARA
C      WRITE(*,*) ' CR = ',CR,' DBH = ',DBH,' EXPAN = ',TDATAR(K,4)
C      WRITE(*,1600)
C 1600 FORMAT(1X,' '\)
C      IANS = IYN(2)
C      ENDIF
      IF(TDATAI(K,2) .GT. IB)THEN
           PDBH=DBH-GROWTH(K,2)
           SELECT CASE(VERSION)
              CASE(1)
                  CALL HD_SWO(ISPGRP,DBH,PRDHT2)
                  CALL HD_SWO(ISPGRP,PDBH,PRDHT1)
              CASE(2)
                  CALL HD_NWO(ISPGRP,DBH,PRDHT2)
                  CALL HD_NWO(ISPGRP,PDBH,PRDHT1)
              CASE(3)
                  CALL HD_SMC(ISPGRP,DBH,PRDHT2)
                  CALL HD_SMC(ISPGRP,PDBH,PRDHT1)
              CASE(4)
                  CALL HD_RAP(ISPGRP,DBH,PRDHT2)
                  CALL HD_RAP(ISPGRP,PDBH,PRDHT1)
           ENDSELECT
           PRDHT1=4.5+CALIB(1,ISPGRP)*(PRDHT1-4.5)
           PRDHT2=4.5+CALIB(1,ISPGRP)*(PRDHT2-4.5)
           PRDHT=(PRDHT2/PRDHT1)*TDATAR(K,2)
C
C          RED ALDER HEIGHT GROWTH - ADDED 12/13/02
C
           IF(TDATAI(K,1) .EQ. 351 .AND. VERSION .LE. 3) THEN
             CALL RAGEA(TDATAR(K,2),RASI,GEARA)
C      WRITE(*,*) ' HT = ',TDATAR(K,2),' RASI = ',RASI,' GEARA = ',GEARA
C      WRITE(*,*) ' HG = ',HG,' THINADJ = ',THINADJ,' FERTADJ = ',FERTADJ
C      WRITE(*,1600)
C 1600 FORMAT(1X,' '\)
C       IANS = IYN(2)
             IF(GEARA .LE. 0.0) THEN
                GROWTH(K,1)=0.0
             ELSE
               CALL RAH40(GEARA,RASI,RAH1)
               CALL RAH40(GEARA+5.0,RASI,RAH2)
               RAHG=RAH2-RAH1
               GROWTH(K,1)=RAHG
             ENDIF
           ELSE
              GROWTH(K,1)=PRDHT-TDATAR(K,2)
           ENDIF
           GROWTH(K,3)=GROWTH(K,3)+GROWTH(K,1)
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HS_HG(ISP,SI,HT,GEAGE,PHTGRO)
      IMPLICIT NONE
      INTEGER*4 ISP
      REAL*4 SI,GEAGE,HT,PHTGRO,B0,B1,B2,BBC,X50,A1A,XAI,XAI5
C
C     Hann and Scrivani (1987) FRL Research Bulletin 59
C
      IF(ISP .EQ. 1)THEN
         B0=-6.21693
         B1=0.281176
         B2=1.14354
      ELSE
         B0=-6.54707
         B1=0.288169
         B2=1.21297
      ENDIF
      BBC=B0+B1*LOG(SI)
      X50=1.0-EXP((-1.)*EXP(BBC+B2*3.912023))
      A1A=1.0-(HT-4.5)*(X50/SI)
      IF(A1A .LE. 0.)THEN
         GEAGE=500.
         PHTGRO=0.
      ELSE
         GEAGE=((-1.0*LOG(A1A))/(EXP(B0)*SI**B1))**(1.0/B2)
         XAI=1.0-EXP(-1.0*EXP(BBC+B2*LOG(GEAGE)))
         XAI5=1.0-EXP((-1.)*EXP(BBC+B2*LOG(GEAGE+5.)))
         PHTGRO=(4.5+(HT-4.5)*(XAI5/XAI))-HT
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE B_HG(SI,HT,GP,GEAGE,PHTGRO)
C
C     Bruce (1981) Forest Science 27: 711-725
C
      IMPLICIT NONE
      REAL*4 SI,HT,X1,X2,B1,B2,GP,GEAGE,PHT,XX1,PHTGRO
      X1=13.25-SI/20.0
      X2=63.25-SI/20.0
      B2=-0.447762-0.894427*SI/100.0+0.793548*(SI/100.0)**2
     1   -0.171666*(SI/100.0)**3
      B1=ALOG(4.5/SI)/(X1**B2-X2**B2)
      XX1=ALOG(HT/SI)/B1+X2**B2
      IF(XX1 .GT. 0.0) THEN
         GEAGE=XX1**(1.0/B2)-X1
      ELSE
         GEAGE=500.0
      ENDIF
      PHT=SI*EXP(B1*((GEAGE+GP+X1)**B2-X2**B2))
      PHTGRO=PHT-HT
      RETURN
      END
**********************************************************************
      SUBROUTINE F_HG(SI,HT,GP,GEAGE,PHTGRO)
C     For Western Hemlock compute Growth Effective Age and 5-year potential
C     or 1-year height growth using the western hemlock top height curves of
C     Flewelling.  These subroutines are required:
C       SITECV_F   computes top height from site and age
C       SITEF_C    computes model parameters
C       SITEF_SI   calculates an approximate psi for a given site
C       Note: Flewelling's curves are metric.
C             Site Index is not adjusted for stump height.
C
      IMPLICIT NONE
      INTEGER*4 I
      REAL*4 SI,GEAGE,HT,GP,PHTGRO,SIM,HTM,AGE,HTOP,PHT,XHTOP1,XHTOP2
      SIM = SI * 0.3048
      HTM = HT * 0.3048
C
C     Compute growth effective age
C
      AGE = 1.0
      DO I=1,4
    5     AGE = AGE + 100./10.**I
          IF(AGE .GT. 500.0) THEN
             GEAGE = 500.0
             CALL SITECV_F(SIM,GEAGE,XHTOP1)
             CALL SITECV_F(SIM,GEAGE+GP,XHTOP2)
             PHTGRO = 3.2808*(XHTOP2-XHTOP1)
             RETURN
          ENDIF
          CALL SITECV_F(SIM,AGE,HTOP)
          IF (HTOP .LT. HTM) GO TO 5
          AGE = AGE - 100./10.**I
      ENDDO
      GEAGE = AGE
C
C     Compute top height and potential height growth
C
      CALL SITECV_F(SIM,GEAGE+GP,HTOP)
      PHT = HTOP*3.2808
      PHTGRO = PHT - HT
      RETURN
      END
C**********************************************************************
C      SUBROUTINE NC_HG(SI,HT,GEAGE,PHTGRO)
CC
CC     NIGH AND COURTIN (1998) RED ALDER
CC
C      IMPLICIT NONE
C      REAL*4 SI,HT,GEAGE,PHTGRO,MSI,MHT,X1,X2,AGE,FMHT
C      MSI=SI/3.28
C      MHT=HT/3.28
C      X1=1.693*(MSI-1.3)/(MHT-1.3)
C      IF(X1 .LT. 1.0001) X1=1.0001
C      X2=(3.600-ALOG(X1-1.0))/1.240
C      GEAGE=EXP(X2)+0.5
C      IF(GEAGE .GT. 150.0) GEAGE=150.0
C      AGE=GEAGE+1.0
C      FMHT=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
C      PHTGRO=3.28*(FMHT-MHT)
C      RETURN
C      END
C***********************************************************************
      SUBROUTINE WHHLB_HG(SI_C,PDEN,HT,GP,GEA,POTHGRO)
C
C     WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH INCREMENT
C     EQUATION FOR RED ALDER
C
      IMPLICIT NONE
      REAL*4 SI_C,PDEN,HT,GP,GEA,POTHGRO,SI_UC,PHT,A
      CALL WHHLB_SI_UC_RUN(SI_C,PDEN,SI_UC)
      CALL WHHLB_GEA_RUN(HT,SI_UC,GEA)
      A=GEA+GP
      CALL WHHLB_H40_RUN(HT,GEA,A,PHT)
      POTHGRO=PHT-HT
      RETURN
      END
C*******************************************************************************
      SUBROUTINE WHHLB_SI_UC_RUN(SI_C,PDEN,SI_UC)
C
C     UNCORRECTS THE DENSITY INPACT UPON THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHN
C          SITE INDEX FOR RED ALDER
C
      IMPLICIT NONE
      REAL*4 SI_C,PDEN,SI_UC
C
C     SITE INDEX UNCORRECTED FOR DENSITY EFFECT
C
      SI_UC=SI_C*(1.0-0.326480904*EXP(-0.000400268678*PDEN**1.5))
      RETURN
      END
C***********************************************************************
      SUBROUTINE WHHLB_GEA_RUN(H,SI_UC,GEA)
C
C     RED ALDER GROWTH EFFECTIVE AGE EQUATION BASED ON H40 EQUATION FROM
C         THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH
C         EQUATION
C
      IMPLICIT NONE
      REAL*4 H,SI_UC,GEA,B1,B2,X
      B1=-4.481266
      B2=-0.658884
      X=(1.0/B1)*ALOG(H/SI_UC)+20.0**B2
      IF(X .LT. 0.03) X=0.03
      GEA=X**(1.0/B2)
      RETURN
      END
C*******************************************************************************
      SUBROUTINE WHHLB_H40_RUN(H40M,TAGEM,TAGEP,PH40P)
C
C     WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH EQUATION FOR
C     RED ALDER
C
      IMPLICIT NONE
      REAL*4 H40M,TAGEM,TAGEP,PH40P
      REAL*4 B1,B2
      B1=-4.481266
      B2=-0.658884
      PH40P=H40M*EXP(B1*(TAGEP**B2-TAGEM**B2))
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_SWO(ISPGRP,PHTGRO,CR,TCCH,HG)
      IMPLICIT NONE
      REAL*4 PHTGRO,CR,TCCH,HG,HGPAR(5,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,
     1       B0,B1,MODIFER,CRADJ
      INTEGER*4 ISPGRP
C
C  HEIGHT GROWTH PARAMETERS (8 parameters - big 5 conifers only)
C
C     DF Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     GW Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     PP Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     SP Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C     IC Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
C
      DATA HGPAR/
     1  1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0       ,! DF,GW,PP,SP,IC
C
     2 -0.02457621, -0.01453250, -0.14889850, -0.14889850, -0.01453250,! DF,GW,PP,SP,IC
C
     3 -0.00407303, -0.00407303, -0.00322752, -0.00678955, -0.00637434,! DF,GW,PP,SP,IC
C
     4  1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0       ,! DF,GW,PP,SP,IC
C
     5  2.89556338,  7.69023575,  0.92071847,  0.92071847,  1.27228638,! DF,GW,PP,SP,IC
C
     6  2.0       ,  2.0       ,  2.0       ,  2.0       ,  2.0       ,! DF,GW,PP,SP,IC
C
     7  0.0       ,  0.0       ,  0.0       ,  0.0       ,  0.0       ,! DF,GW,PP,SP,IC
C
     8  1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0/       ! DF,GW,PP,SP,IC
C
      P1=HGPAR(ISPGRP,1)
      P2=HGPAR(ISPGRP,2)
      P3=HGPAR(ISPGRP,3)
      P4=HGPAR(ISPGRP,4)
      P5=HGPAR(ISPGRP,5)
      P6=HGPAR(ISPGRP,6)
      P7=HGPAR(ISPGRP,7)
      P8=HGPAR(ISPGRP,8)
      FCR=(-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
      B0=P1*EXP(P2*TCCH)
      B1=EXP(P3*TCCH**P4)
      IF (FCR .LT. -20.) THEN
        MODIFER=P8*B0
      ELSE
        MODIFER=P8*(B0+(B1-B0)*EXP(FCR))
      ENDIF
      CRADJ = 1.0
      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)      
      HG=PHTGRO*MODIFER*CRADJ
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_NWO(ISPGRP,PHTGRO,CR,TCCH,HG)
      IMPLICIT NONE
      REAL*4 PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,
     1       B0,B1,MODIFER,CRADJ
      INTEGER*4 ISPGRP
C
C  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)
C
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
C     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C
      DATA HGPAR/
     1           0.655258886 ,  1.0         ,  1.0         ,           !  DF,GF,WH
C
     2          -0.006322913 , -0.0328142   , -0.0384415   ,           !  DF,GF,WH
C
     3          -0.039409636 , -0.0127851   , -0.0144139   ,           !  DF,GF,WH
C
     4           0.5         ,  1.0         ,  0.5         ,           !  DF,GF,WH
C
     5           0.597617316 ,  6.19784     ,  1.04409     ,           !  DF,GF,WH
C
     6           2.0         ,  2.0         ,  2.0         ,           !  DF,GF,WH
C
     7           0.631643636 ,  0.0         ,  0.0         ,           !  DF,GF,WH
C
     8           1.010018427 ,  1.01        ,  1.03        /           !  DF,GF,WH
C
      P1=HGPAR(ISPGRP,1)
      P2=HGPAR(ISPGRP,2)
      P3=HGPAR(ISPGRP,3)
      P4=HGPAR(ISPGRP,4)
      P5=HGPAR(ISPGRP,5)
      P6=HGPAR(ISPGRP,6)
      P7=HGPAR(ISPGRP,7)
      P8=HGPAR(ISPGRP,8)
      FCR=(-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
      B0=P1*EXP(P2*TCCH)
      B1=EXP(P3*TCCH**P4)
      IF (FCR .LT. -20.) THEN
        MODIFER=P8*B0
      ELSE
        MODIFER=P8*(B0+(B1-B0)*EXP(FCR))
      ENDIF
      CRADJ = 1.0
      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
      HG=PHTGRO*MODIFER*CRADJ
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
      IMPLICIT NONE
      REAL*4 PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,
     1       B0,B1,MODIFER,CRADJ
      INTEGER*4 ISPGRP
CC
CC  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)
CC
CC     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
CC     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
CC     WH Coefficients from Hann, Marshall, and Hanus (2003) FRL Research Contribution 40
CC
C      DATA HGPAR/
C     1           0.655258886,  1.0      ,  1.0         ,               !  DF,GF,WH
CC
C     2          -0.006322913, -0.0328142, -0.0056949357,               !  DF,GF,WH
CC
C     3          -0.039409636, -0.0127851, -0.0018047267,               !  DF,GF,WH
CC
C     4           0.5        ,  1.0      ,  0.5         ,               !  DF,GF,WH
CC
C     5           0.597617316,  6.19784  ,  6.1978      ,               !  DF,GF,WH
CC
C     6           2.0        ,  2.0      ,  2.0         ,               !  DF,GF,WH
CC
C     7           0.631643636,  0.0      ,  0.0         ,               !  DF,GF,WH
CC
C     8           1.010018427,  1.01     ,  1.03        /               !  DF,GF,WH
C
C  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)
C
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
C     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C
      DATA HGPAR/
     1           0.655258886 ,  1.0         ,  1.0         ,           !  DF,GF,WH
C
     2          -0.006322913 , -0.0328142   , -0.0384415   ,           !  DF,GF,WH
C
     3          -0.039409636 , -0.0127851   , -0.0144139   ,           !  DF,GF,WH
C
     4           0.5         ,  1.0         ,  0.5         ,           !  DF,GF,WH
C
     5           0.597617316 ,  6.19784     ,  1.04409     ,           !  DF,GF,WH
C
     6           2.0         ,  2.0         ,  2.0         ,           !  DF,GF,WH
C
     7           0.631643636 ,  0.0         ,  0.0         ,           !  DF,GF,WH
C
     8           1.010018427 ,  1.01        ,  1.03        /           !  DF,GF,WH
C
      P1=HGPAR(ISPGRP,1)
      P2=HGPAR(ISPGRP,2)
      P3=HGPAR(ISPGRP,3)
      P4=HGPAR(ISPGRP,4)                
      P5=HGPAR(ISPGRP,5)
      P6=HGPAR(ISPGRP,6)
      P7=HGPAR(ISPGRP,7)         
      P8=HGPAR(ISPGRP,8)
      FCR=(-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
      B0=P1*EXP(P2*TCCH)
      B1=EXP(P3*TCCH**P4)
      IF (FCR .LT. -20.) THEN
        MODIFER=P8*B0
      ELSE
        MODIFER=P8*(B0+(B1-B0)*EXP(FCR))
      ENDIF
      CRADJ = 1.0
      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
      HG=PHTGRO*MODIFER*CRADJ
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_RAP(ISPGRP,PHTGRO,CR,TCCH,HG)
      IMPLICIT NONE
      REAL*4 PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,
     1       B0,B1,MODIFER,CRADJ
      INTEGER*4 ISPGRP
CC
CC  HEIGHT GROWTH PARAMETERS (8 parameters - 3 species only)
CC
C     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
C     WH Coefficients from Johnson (2002) Willamette Industries Report
C
C WEIGHTED SUMMATION PROCEDURE PARAMETERS FOR RED ALDER
C
      DATA HGPAR/
     1       0.809837005 ,  0.655258886 ,  1.0         ,   !  RA,DF,WH
C
     2      -0.0134163653, -0.006322913 , -0.0384415   ,   !  RA,DF,WH
C
     3      -0.0609398629, -0.039409636 , -0.0144139   ,   !  RA,DF,WH
C
     4       0.5         ,  0.5         ,  0.5         ,   !  RA,DF,WH
C
     5       1.0         ,  0.597617316 ,  1.04409     ,   !  RA,DF,WH
C
     6       2.0         ,  2.0         ,  2.0         ,   !  RA,DF,WH
C
     7       0.1469442410,  0.631643636 ,  0.0         ,   !  RA,DF,WH
C
     8       1.0476380753,  1.010018427 ,  1.03        /   !  RA,DF,WH
C
CC
CC WEIGHTED CENTRAL PAI PROCEDURE PARAMETERS FOR RED ALDER
CC
C      DATA HGPAR/
C     1       0.775118127 ,  0.655258886 ,  1.0         ,   !  RA,DF,WH
CC
C     2      -0.0128743358, -0.006322913 , -0.0384415   ,   !  RA,DF,WH
CC
C     3      -0.070294082 , -0.039409636 , -0.0144139   ,   !  RA,DF,WH
CC
C     4       0.5         ,  0.5         ,  0.5         ,   !  RA,DF,WH
CC
C     5       1.0         ,  0.597617316 ,  1.04409     ,   !  RA,DF,WH
CC
C     6       2.0         ,  2.0         ,  2.0         ,   !  RA,DF,WH
CC
C     7       0.120539836 ,  0.631643636 ,  0.0         ,   !  RA,DF,WH
CC
C     8       1.07563185  ,  1.010018427 ,  1.03        /   !  RA,DF,WH
CC
      P1=HGPAR(ISPGRP,1)
      P2=HGPAR(ISPGRP,2)
      P3=HGPAR(ISPGRP,3)
      P4=HGPAR(ISPGRP,4)
      P5=HGPAR(ISPGRP,5)
      P6=HGPAR(ISPGRP,6)
      P7=HGPAR(ISPGRP,7)
      P8=HGPAR(ISPGRP,8)
      FCR=(-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
      B0=P1*EXP(P2*TCCH)
      B1=EXP(P3*TCCH**P4)
      IF (FCR .LT. -20.) THEN
        MODIFER=P8*B0
      ELSE
        MODIFER=P8*(B0+(B1-B0)*EXP(FCR))
      ENDIF
      CRADJ = 1.0
      IF (CR.LE. 0.17) CRADJ=1.0-EXP(-(25.0*CR)**2.0)
      HG=PHTGRO*MODIFER*CRADJ
      RETURN
      END
**********************************************************************
      SUBROUTINE LIMIT(VERSION,ISP,DBH,HT,DG,HG)
      IMPLICIT NONE
      REAL*4 DBH,HT,DG,HG,A0,A1,A2,HT1,HT2,HT3,DBH1,DBH2,DBH3,PHT1,PHT2,
     1       PHT3,PHGR1,PHGR2
      INTEGER*4 VERSION,ISP,JSP
      JSP=ISP
      IF(ISP .EQ. 263 .AND. VERSION .EQ. 1) JSP=2631
      SELECT CASE(JSP)
        CASE(202,263)
           A0=19.04942539
           A1=-0.04484724
           A2=1.0
        CASE(17,15)
           A0=16.26279948
           A1=-0.04484724
           A2=1.0
        CASE(122)
           A0=17.11482201
           A1=-0.04484724
           A2=1.0
        CASE(117)
           A0=14.29011403
           A1=-0.04484724
           A2=1.0
        CASE(351)
           A0=60.619859
           A1=-1.59138564
           A2=0.496705997
        CASE DEFAULT
           A0=15.80319194
           A1=-0.04484724
           A2=1.0
      ENDSELECT
      HT1=HT-4.5
      HT2=HT1+HG
      HT3=HT2+HG
      DBH1=DBH
      DBH2=DBH1+DG
      DBH3=DBH2+DG
      PHT1=A0*DBH1/(1.0-A1*DBH1**A2)
      PHT2=A0*DBH2/(1.0-A1*DBH2**A2)
      PHT3=A0*DBH3/(1.0-A1*DBH3**A2)
      PHGR1=(PHT2-PHT1+HG)/2.0
      PHGR2=PHT2-HT1
      IF(HT2.GT.PHT2) THEN
         IF(PHGR1.LT.PHGR2) THEN
            HG=PHGR1
         ELSE
            HG=PHGR2
         ENDIF
      ELSE IF(HT3.GT.PHT3) THEN
         HG=PHGR1
      ENDIF
      IF(HG.LT.0.0) THEN
         HG=0.0
      ENDIF
      RETURN
      END
**********************************************************************
      SUBROUTINE HD_SWO(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(18,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  NEW HEIGHT/DIAMETER PARAMETERS FOR UNDAMAGED TREES. EXCEPT RC, WO, AND RA (3 parameters - all species)
C
C     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     SP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     IC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WH Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     MD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     GC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     TA Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     CL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     BL Coefficients Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     BO Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C     WI Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
C
      DATA HDPAR/
     1             7.133682298,  6.75286569,  6.27233557,  5.81876360, !  DF,GW,PP,SP
     1            10.04621768 ,  6.58804   ,  6.14817441,  5.10707208, !  IC,WH,RC,PY
     1             6.53558288 ,  9.2251518 ,  8.49655416,  9.01612971, !  MD,GC,TA,CL
     1             5.20018445 ,  4.69753118,  5.04832439,  5.59759126, !  BL,WO,BO,RA
     1             7.49095931 ,  3.26840527,                           !  PD,WI
C
     2            -5.433744897, -5.52614439, -5.57306985, -5.31082668, !  DF,GW,PP,SP
     2            -8.72915115,  -5.25312496, -5.40092761, -3.28638769, !  IC,WH,RC,PY
     2            -4.69059053,  -7.65310387, -6.68904033, -7.34813829, !  MD,GC,TA,CL
     2            -2.86671078,  -3.51586969, -3.32715915, -3.19942952, !  BL,WO,BO,RA
     2            -5.40872209,  -0.95270859,                           !  PD,WI
C
     3            -0.266398088, -0.33012156, -0.40384171, -0.47349388, !  DF,GW,PP,SP
     3            -0.14040106,  -0.31895401, -0.38922036, -0.24016101, !  IC,WH,RC,PY
     3            -0.24934807,  -0.15480725, -0.16105112, -0.134025626,!  MD,GC,TA,CL
     3            -0.42255220,  -0.57665068, -0.43456034, -0.38783403, !  BL,WO,BO,RA
     3            -0.16874962,  -0.98015696/                           !  PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE HD_NWO(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(11,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)
C
C     DF Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     GF Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WH Coefficients from Johnson (2000) Willamette Industries Report
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     MD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51
C
      DATA HDPAR/
     1            7.04524    ,  7.42808    ,  5.93792    ,  6.14817441,!  DF,GF,WH,RC
     1            9.30172    ,  5.84487    ,  5.21462    ,  4.69753118,!  PY,MD,BL,WO
     1            5.59759126 ,  4.49727    ,  4.88361    ,             !  RA,PD,WI
C
     2           -5.16836    , -5.80832    , -4.43822    , -5.40092761,!  DF,GF,WH,RC
     2           -7.50951    , -3.84795    , -2.70252    , -3.51586969,!  PY,MD,BL,WO
     2           -3.19942952 , -2.07667    , -2.47605    ,             !  RA,PD,WI
C
     3           -0.253869   ,  -0.240317  , -0.411373   , -0.38922036,!  DF,GF,WH,RC
     3           -0.100000   ,  -0.289213  , -0.354756   , -0.57665068,!  PY,MD,BL,WO
     3           -0.38783403 ,  -0.388650  , -0.309050  /              !  RA,PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END

**********************************************************************
      SUBROUTINE HD_SMC(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(11,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)
C
C     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     GF Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     PY Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     MD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
C     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
C     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51
C
      DATA HDPAR/
     1            7.262195456,  7.42808    ,  6.555344622,  6.14817441,!  DF,GF,WH,RC
     1            9.30172    ,  5.84487    ,  5.21462    ,  4.69753118,!  PY,MD,BL,WO
     1            5.59759126 ,  4.49727    ,  4.88361    ,             !  RA,PD,WI
C
     2           -5.899759104, -5.80832    , -5.137174162, -5.40092761,!  DF,GF,WH,RC
     2           -7.50951    , -3.84795    , -2.70252    , -3.51586969,!  PY,MD,BL,WO
     2           -3.19942952 , -2.07667    , -2.47605    ,             !  RA,PD,WI
C
     3           -0.287207389, -0.240317   , -0.364550800, -0.38922036,!  DF,GF,WH,RC
     3           -0.100000   ,  -0.289213  , -0.354756   , -0.57665068,!  PY,MD,BL,WO
     3           -0.38783403 ,  -0.388650  , -0.309050  /              !  RA,PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE HD_RAP(ISPGRP,DBH,PRDHT)
      IMPLICIT NONE
      REAL*4 DBH,PRDHT,HDPAR(7,3),B0,B1,B2
      INTEGER*4 ISPGRP
C
C  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)
C
C     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
C     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
C     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
C     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
C     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51
C
      DATA HDPAR/
     1            6.75650139 ,  7.262195456,  6.555344622,  6.14817441,!  RA,DF,WH,RC
     1            5.21462    ,  4.49727    ,  4.88361    ,             !  BL,PD,WI
C
     2           -4.6252377  , -5.899759104, -5.137174162, -5.40092761,!  RA,DF,WH,RC
     2           -2.70252    , -2.07667    , -2.47605    ,             !  BL,PD,WI
C
     3           -0.23208200 , -0.287207389, -0.364550800, -0.38922036,!  RA,DF,WH,RC
     3           -0.354756   ,  -0.388650  , -0.309050  /              !  BL,PD,WI
C
      B0=HDPAR(ISPGRP,1)
      B1=HDPAR(ISPGRP,2)
      B2=HDPAR(ISPGRP,3)
      PRDHT=4.5+EXP(B0+B1*DBH**B2)
      RETURN
      END
**********************************************************************
      SUBROUTINE HG_FERT(CYCLG,VERSION,ISPGRP,SI_1,PN,YF,FERTADJ)
      IMPLICIT NONE
      REAL*4 SI_1,PN(5),YF(5),FERTADJ,PF1,PF2,PF3,PF4,PF5,FALDWN,XTIME,
     1       FERTX1,FERTX2
      INTEGER*4 CYCLG,VERSION,ISPGRP,I
      IF(VERSION .LE. 3) THEN
         IF(ISPGRP .EQ. 1)THEN
            PF1=1.0
            PF2=0.333333333
            PF3=-1.107409443
            PF4=-2.133334346
            PF5=1.5
         ELSE
            PF1=0.0
            PF2=1.0
            PF3=0.0
            PF4=0.0
            PF5=1.0
         ENDIF
      ELSE
         PF1=0.0
         PF2=1.0
         PF3=0.0
         PF4=0.0
         PF5=1.0
      ENDIF
      FALDWN=1.0
      XTIME=FLOAT(CYCLG)*5.0
      FERTX1=0.0
      DO I=2,5
         FERTX1=FERTX1+(PN(I)/800.0)*EXP((PF3/PF2)*(YF(1)-YF(I)))
      ENDDO
      FERTX2=EXP(PF3*(XTIME-YF(1))+PF4*(SI_1/100.0)**PF5)
      FERTADJ=1.0+(PF1*((PN(1)/800.0)+FERTX1)**PF2*FERTX2)*FALDWN
      RETURN
      END

**********************************************************************
      SUBROUTINE HG_THIN(CYCLG,VERSION,ISPGRP,BABT,BART,YT,THINADJ)
      IMPLICIT NONE
      REAL*4 BABT,BART(5),YT(5),THINADJ,PT1,PT2,PT3,XTIME,THINX1,THINX2,
     1       THINX3,PREM,GP
      INTEGER*4 CYCLG,VERSION,ISPGRP,I
      IF(VERSION .LE. 3) THEN
         IF(ISPGRP .EQ. 1)THEN
            PT1=-0.3197415492
            PT2=0.7528887377
            PT3=-0.2268800162
         ELSE
            PT1=0.0
            PT2=1.0
            PT3=0.0
         ENDIF
         GP=5.0
      ELSE
         IF(ISPGRP .EQ. 1)THEN
            PT1=-0.613313694
            PT2=1.0
            PT3=-0.443824038
         ELSEIF(ISPGRP .EQ. 2)THEN
C            PT1=-0.3197415492
            PT1=-0.47842928                                         ! ANNUALIZED PARAMETER
            PT2=0.7528887377
            PT3=-0.2268800162
         ELSE
            PT1=0.0
            PT2=1.0
            PT3=0.0
         ENDIF
         GP=1.0
      ENDIF
      XTIME=FLOAT(CYCLG)*GP
      THINX1=0.0
      DO I=2,5
         THINX1=THINX1+BART(I)*EXP((PT3/PT2)*(YT(1)-YT(I)))
      ENDDO
      THINX2=THINX1+BART(1)
      THINX3=THINX1+BABT
      IF(THINX3 .LE. 0.0) THEN
         PREM=0.0
      ELSE
         PREM=THINX2/THINX3
      ENDIF
      IF(PREM .GT. 0.75) PREM=0.75
      THINADJ=1.0+PT1*PREM**PT2*EXP(PT3*(XTIME-YT(1)))
      RETURN
      END
C***********************************************************************
      SUBROUTINE RAGEA(H,SI,GEA)
C
C     RED ALDER GROWTH EFFECTIVE AGE EQUATION BASED ON H40 EQUATION FROM
C         WORTHINGTON, JOHNSON, STAEBLER AND LLOYD (1960) PNW RESEARCH
C         PAPER 36
C
      IMPLICIT NONE
      REAL*4 H,SI,GEA
      GEA=19.538*H/(SI-0.60924*H)
      RETURN
      END
C***********************************************************************
      SUBROUTINE RAH40(A,SI,H)
C
C     RED ALDER H40 EQUATION FROM FROM WORTHINGTON, JOHNSON, STAEBLER
C         AND LLOYD (1960) PNW RESEARCH PAPER 36
C
      IMPLICIT NONE
      REAL*4 A,SI,H
      H=SI/(0.60924+19.538/A)
      RETURN
      END
