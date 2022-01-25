C----------
C ORGANON $Id$
C----------
      SUBROUTINE XTRIP(K,J,M,ON,IWQ,WOODQ,IB,BIG6,POINT,TREENO,
     1                 TDATAI,PRAGE,BRCNT,BRHT,BRDIA,JCORE,NPR,PRLH,
     2                 PRDBH,PRHT,PRCR,PREXP,SCR,VOLTR,SYTVOL)
      IMPLICIT NONE
      INTEGER*4 K,J,M,ON,IWQ,IB,BIG6,POINT(2000),TREENO(2000),
     1          TDATAI(2000,3),PRAGE(2000,3),BRCNT(2000,3),
     2          BRHT(2000,40),BRDIA(2000,40),JCORE(2000,40),
     3          NPR(2000),ISPGRP,I
      REAL*4 PRLH(2000,3),PRDBH(2000,3),PRHT(2000,3),PRCR(2000,3),
     1       PREXP(2000,3),SCR(2000,3),VOLTR(2000,4),SYTVOL(2000,2)
      LOGICAL*2 WOODQ
      ISPGRP=TDATAI(K,2)
C
C     DO TRIPLING OF VALUES OTHER THAN BASIC TREE ATTRIBUTES
C
      IF(J .GT. 2000)THEN
         WRITE(*,*)' SYSTEM OVERFLOW...MUST BE REPORTED'
         M=0
         RETURN
      ENDIF

      IF(M .EQ. 0 .OR. (M .EQ. 2 .AND. ON .EQ. 0)) RETURN
C
C        TRIPLING PRUNING VARIABLES
C
      DO I=1,3
         PRLH(J,I)=PRLH(K,I)
         PRLH(J+1,I)=PRLH(K,I)
         PRAGE(J,I)=PRAGE(K,I)
         PRAGE(J+1,I)=PRAGE(K,I)
         PRDBH(J,I)=PRDBH(K,I)
         PRDBH(J+1,I)=PRDBH(K,I)
         PRHT(J,I)=PRHT(K,I)
         PRHT(J+1,I)=PRHT(K,I)
         PRCR(J,I)=PRCR(K,I)
         PRCR(J+1,I)=PRCR(K,I)
         PREXP(J,I)=PREXP(K,I)
         PREXP(J+1,I)=PREXP(K,I)
         SCR(J,I)=SCR(K,I)
         SCR(J+1,I)=SCR(K,I)
C
C        TRIPLING WOOD QUALITY VARIBLES
C
         IF(WOODQ.AND.ISPGRP.LE. IB) THEN
            BRCNT(BIG6+1,I)=BRCNT(IWQ,I)
            BRCNT(BIG6+2,I)=BRCNT(IWQ,I)
         ENDIF
      ENDDO
C
C     TRIPLING WOOD QUALITY VARIBLES
C
      IF(WOODQ.AND.ISPGRP.LE. IB) THEN
         DO I=1,40
            BRHT(BIG6+1,I)=BRHT(IWQ,I)
            BRHT(BIG6+2,I)=BRHT(IWQ,I)
            BRDIA(BIG6+1,I)=BRDIA(IWQ,I)
            BRDIA(BIG6+2,I)=BRDIA(IWQ,I)
            JCORE(BIG6+1,I)=JCORE(IWQ,I)
            JCORE(BIG6+2,I)=JCORE(IWQ,I)
         ENDDO
      ENDIF
C
C     TRIPLING VOLUMES
C
      VOLTR(J,2)=VOLTR(K,2)
      VOLTR(J,4)=VOLTR(K,4)
      VOLTR(J+1,2)=VOLTR(K,2)
      VOLTR(J+1,4)=VOLTR(K,4)
      SYTVOL(J,1)=SYTVOL(K,1)
      SYTVOL(J+1,1)=SYTVOL(K,1)
      SYTVOL(J,2)=SYTVOL(K,2)
      SYTVOL(J+1,2)=SYTVOL(K,2)
      NPR(J)=NPR(K)
      NPR(J+1)=NPR(K)
      POINT(J)=POINT(K)
      POINT(J+1)=POINT(K)
      TREENO(J)=TREENO(K)
      TREENO(J+1)=TREENO(K)
      RETURN
      END
********************************************************************************
      SUBROUTINE DGTRIP(K,J,M,ON,VERSION,IB,BIG6,OTHER,TDATAI,
     1                  TDATAR,GROWTH,MGEXP,DEADEXP)
      IMPLICIT NONE
      INTEGER*4 K,J,M,ON,VERSION,IB,BIG6,OTHER,TDATAI(2000,3),I
      REAL*4 TDATAR(2000,8),GROWTH(2000,4),MGEXP(2000),
     1       DEADEXP(2000),A,LRES,URES,DG,DGRO,DGRO1,DGRO2
C
C     DO TRIPLING
C
      IF(M .EQ. 0 .OR. (M .EQ. 2 .AND. ON .EQ. 0))THEN
           ON=1
      ELSE
           IF(J .GT. 2000)THEN
                WRITE(*,*)' SYSTEM OVERFLOW...MUST BE REPORTED'
                M=0
                RETURN
           ENDIF
C
C          SPECIES, GROUP, UC, DBH, HT, CR, @HT & @DIAM,
C          CUM @HT & @DIAM = ORIGINAL TREE
C
           DO I=1,3
              TDATAI(J,I)=TDATAI(K,I)
              TDATAI(J+1,I)=TDATAI(K,I)
              TDATAR(J,I)=TDATAR(K,I)
              TDATAR(J+1,I)=TDATAR(K,I)
         ENDDO
C
C     EXPANSION FACTORS ARE 1/3
C
           A=TDATAR(K,4)/3.0
           TDATAR(J,4)=A
           TDATAR(J+1,4)=A
           TDATAR(K,4)=A
           A=TDATAR(K,8)/3.0
           TDATAR(J,8)=A
           TDATAR(J+1,8)=A
           TDATAR(K,8)=A
           A=TDATAR(K,5)/3.0
           TDATAR(J,5)=A
           TDATAR(J+1,5)=A
           TDATAR(K,5)=A
           A=MGEXP(K)/3.0
           MGEXP(J)=A
           MGEXP(J+1)=A
           MGEXP(K)=A
           A=DEADEXP(K)/3.0
           DEADEXP(J)=A
           DEADEXP(J+1)=A
           DEADEXP(K)=A
           TDATAR(J,6)=TDATAR(K,6)
           TDATAR(J+1,6)=TDATAR(K,6)
           TDATAR(J,7)=TDATAR(K,7)
           TDATAR(J+1,7)=TDATAR(K,7)

C     INCREASE SPECIES COUNT
C
           IF(TDATAI(K,2) .LE. IB)THEN
                BIG6=BIG6+2
           ELSE
                OTHER=OTHER+2
           ENDIF
           SELECT CASE(VERSION)
              CASE(1)
                 CALL DGRES_SWO(TDATAI(K,2),LRES,URES)
              CASE(2)
                 CALL DGRES_NWO(TDATAI(K,2),LRES,URES)
              CASE(3)
                 CALL DGRES_SMC(TDATAI(K,2),LRES,URES)
              CASE(4)
                 CALL DGRES_RAP(TDATAI(K,2),LRES,URES)
           ENDSELECT
C
           DG=GROWTH(K,2)
           DGRO=DG-(LRES+URES)*SQRT(DG)
           IF(DGRO .LT. 0.0)DGRO=0.0
           DGRO1=DG+URES*SQRT(DG)
           DGRO2=DG+LRES*SQRT(DG)
           IF(DGRO2 .LT. 0.0)DGRO2=0.0
           GROWTH(K,2)=DGRO
           GROWTH(J,2)=DGRO1
           GROWTH(J,4)=GROWTH(K,4)+GROWTH(J,2)
           GROWTH(J+1,2)=DGRO2
           GROWTH(J+1,4)=GROWTH(K,4)+GROWTH(J+1,2)
           GROWTH(J,3)=GROWTH(K,3)
           GROWTH(J+1,3)=GROWTH(K,3)
           J=J+2
           ON=0
      ENDIF
      GROWTH(K,4)=GROWTH(K,4)+GROWTH(K,2)
      RETURN
      END
********************************************************************************
      SUBROUTINE DGRES_SWO(ISPGRP,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 LRES,URES,DGLRES(18),DGURES(18)
      DATA DGLRES/
     1            -0.440    , -0.4246576, -0.3934351, -0.4750919,      !  DF,GW,PP,SP
     1            -0.3819283, -0.4629461, -0.3733441, -0.2690408,      !  IC,WH,RC,PY
     1            -0.3155211, -0.3281635, -0.3407614, -0.2963305,      !  MD,GC,TA,CL
     1            -0.3663556, -0.38681203,-0.2698731, -0.4947154,      !  BL,WO,BO,RA
     1            -0.3679208, -0.3679208/                              !  PD,WI
C
      DATA DGURES/
     1             0.487    ,  0.4822343,  0.4619852,  0.4977667,      !  DF,GW,PP,SP
     1             0.4446445,  0.5300564,  0.4250894,  0.3367567,      !  IC,WH,RC,PY
     1             0.3843792,  0.3975281,  0.4190495,  0.3788400,      !  MD,GC,TA,CL
     1             0.4157338,  0.46098924, 0.3292178,  0.6171431,      !  BL,WO,BO,RA
     1             0.4444550,  0.4444550/                              !  PD,WI
      LRES=DGLRES(ISPGRP)
      URES=DGURES(ISPGRP)
      RETURN
      END
********************************************************************************
      SUBROUTINE DGRES_NWO(ISPGRP,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP

      REAL*4 LRES,URES,DGLRES(11),DGURES(11)
      DATA DGLRES/
     1             -0.440     , -0.60689712, -0.51832840, -0.3733441,  !  DF,GF,WH,RC
     1             -0.2690408 , -0.3155211 , -0.3663556 , -0.38681203, !  PY,MD,BL,WO
     1             -0.4947154 , -0.3679208 , -0.3679208/               !  RA,PD,WI
      DATA DGURES/
     1             0.487     , 0.66701064, 0.7452303 , 0.4250894,      !  DF,GF,WH,RC
     1             0.3367567 , 0.3843792 , 0.4157338 , 0.46098924,     !  PY,MD,BL,WO
     1             0.6171431 , 0.4444550 , 0.4444550/                  !  RA,PD,WI
      LRES=DGLRES(ISPGRP)
      URES=DGURES(ISPGRP)
      RETURN
      END

********************************************************************************
      SUBROUTINE DGRES_SMC(ISPGRP,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 LRES,URES,DGLRES(11),DGURES(11)
      DATA DGLRES/
     1             -0.440     , -0.60689712, -0.50104700, -0.3733441,  !  DF,GF,WH,RC
     1             -0.2690408 , -0.3155211 , -0.3663556 , -0.38681203, !  PY,MD,BL,WO
     1             -0.4947154 , -0.3679208 , -0.3679208/               !  RA,PD,WI
      DATA DGURES/
     1             0.487     , 0.66701064, 0.59381592, 0.4250894,      !  DF,GF,WH,RC
     1             0.3367567 , 0.3843792 , 0.4157338 , 0.46098924,     !  PY,MD,BL,WO
     1             0.6171431 , 0.4444550 , 0.4444550/                  !  RA,PD,WI
      LRES=DGLRES(ISPGRP)
      URES=DGURES(ISPGRP)
      RETURN
      END
********************************************************************************
      SUBROUTINE DGRES_RAP(ISPGRP,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 LRES,URES,DGLRES(7),DGURES(7)
      DATA DGLRES/
     1             -0.20610321, -0.1967740 , -0.22407503, -0.1669646,  !  RA,DF,WH,RC
     1             -0.16383920, -0.16453918, -0.16453918/              !  BL,PD,WI
      DATA DGURES/
     1             0.21278486, 0.2177930 , 0.26556256, 0.19010576,     !  RA,DF,WH,RC
     1             0.18592181, 0.19876632, 0.19876632/                 !  BL,PD,WI
      LRES=DGLRES(ISPGRP)
      URES=DGURES(ISPGRP)
      RETURN
      END
********************************************************************************
      SUBROUTINE HGTRIP(K,J,M,ON,VERSION,BIG6,OTHER,TDATAI,
     1                  TDATAR,GROWTH,MGEXP,DEADEXP)
      IMPLICIT NONE
      INTEGER*4 K,J,M,ON,VERSION,BIG6,OTHER,TDATAI(2000,3),I
      REAL*4 TDATAR(2000,8),GROWTH(2000,4),MGEXP(2000),
     1       DEADEXP(2000),A,LRES,URES
      INTEGER*4 IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = OTHER
C
C     DO TRIPLING
C
      IF(M .EQ. 0 .OR. (M .EQ. 2 .AND. ON .EQ. 0))THEN
           ON=1
      ELSE
           IF(J .GT. 2000)THEN
                WRITE(*,*)' SYSTEM OVERFLOW...MUST BE REPORTED'
                M=0
                RETURN
           ENDIF
C
C          DUPLICATE SPECIES, GROUP, CC, DBH, HT, CR, @HT & @DIAM
C          CUM @HT & @DIAM = ORIGINAL TREE
C

           SELECT CASE(VERSION)
              CASE(1)
                 CALL HGRES_SWO(TDATAI(K,2),GROWTH(K,1),LRES,URES)
              CASE(2)
                 CALL HGRES_NWO(TDATAI(K,2),GROWTH(K,1),LRES,URES)
              CASE(3)
                 CALL HGRES_SMC(TDATAI(K,2),GROWTH(K,1),LRES,URES)
              CASE(4)
                 CALL HGRES_RAP(TDATAI(K,2),GROWTH(K,1),LRES,URES)
           ENDSELECT
           DO I=1,3
              TDATAI(J,I)=TDATAI(K,I)
              TDATAI(J+1,I)=TDATAI(K,I)
              TDATAR(J,I)=TDATAR(K,I)
              TDATAR(J+1,I)=TDATAR(K,I)
           ENDDO
           GROWTH(J,1)=GROWTH(K,1)+URES
           GROWTH(J+1,1)=GROWTH(K,1)+LRES
           IF(GROWTH(J+1,1) .LT. 0.)GROWTH(J+1,1)=0.
           GROWTH(K,1)=GROWTH(K,1)-(URES+LRES)
           IF(GROWTH(K,1).LT.0.0) GROWTH(K,1)=0.0
           GROWTH(J,3)=GROWTH(K,3)+GROWTH(J,1)
           GROWTH(J+1,3)=GROWTH(K,3)+GROWTH(J+1,1)
C           GROWTH(K,3)=GROWTH(K,3)+GROWTH(K,1)
           GROWTH(J,4)=GROWTH(K,4)
           GROWTH(J+1,4)=GROWTH(K,4)
           GROWTH(J,2)=GROWTH(K,2)
           GROWTH(J+1,2)=GROWTH(K,2)
C
C     EXPANSION FACTOR IS 1/3
C
           A=TDATAR(K,4)/3.0
           TDATAR(J,4)=A
           TDATAR(J+1,4)=A
           TDATAR(K,4)=A
           A=TDATAR(K,8)/3.0
           TDATAR(J,8)=A
           TDATAR(J+1,8)=A
           TDATAR(K,8)=A
           A=TDATAR(K,5)/3.0
           TDATAR(J,5)=A
           TDATAR(J+1,5)=A
           TDATAR(K,5)=A
           A=MGEXP(K)/3.0
           MGEXP(J)=A
           MGEXP(J+1)=A
           MGEXP(K)=A
           A=DEADEXP(K)/3.0
           DEADEXP(J)=A
           DEADEXP(J+1)=A
           DEADEXP(K)=A
           TDATAR(J,6)=TDATAR(K,6)
           TDATAR(J+1,6)=TDATAR(K,6)
           TDATAR(J,7)=TDATAR(K,7)
           TDATAR(J+1,7)=TDATAR(K,7)
C
C     INCREASE TREE SPECIES COUNT
C
           BIG6=BIG6+2
           J=J+2
           ON=0
      ENDIF
      GROWTH(K,3)=GROWTH(K,3)+GROWTH(K,1)
      RETURN
      END

********************************************************************************
      SUBROUTINE HGRES_SWO(ISPGRP,HG,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HG,LRES,URES,HGLRES(5,3),HGURES(5,3),A0,A1,A2,C0,C1,C2
C
C  HEIGHT GROWTH LOWER AND UPPER RESIDUALS (3 parameters each - big 5 only)
C
      DATA HGLRES
     1           / -0.51303, -0.46026,  0.00000,  0.00000,  0.00000,   !  DF,GW,PP,SP,IC
     2             -0.35755, -0.31335, -0.53151, -0.59648, -0.80800,   !  DF,GW,PP,SP,IC
     3              0.01357,  0.00000,  0.02492,  0.03244,  0.07429/   !  DF,GW,PP,SP,IC
C
      DATA HGURES/
     1              1.09084,  1.32726,  1.20195,  0.00000,  1.29032,   !  DF,GW,PP,SP,IC
     2              0.31540,  0.17331,  0.11159,  0.84916,  0.12008,   !  DF,GW,PP,SP,IC
     3             -0.02196,  0.00000,  0.00000, -0.07683,  0.00000/   !  DF,GW,PP,SP,IC
C
      A0=HGLRES(ISPGRP,1)
      A1=HGLRES(ISPGRP,2)
      A2=HGLRES(ISPGRP,3)
      C0=HGURES(ISPGRP,1)
      C1=HGURES(ISPGRP,2)
      C2=HGURES(ISPGRP,3)
      LRES=A0+A1*HG+A2*HG**2
      URES=C0+C1*HG+C2*HG**2
      IF(LRES.GT.0.0) LRES=0.0
      IF(URES.LT.0.0) URES=0.0
      RETURN
      END
********************************************************************************
      SUBROUTINE HGRES_NWO(ISPGRP,HG,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HG,LRES,URES,HGLRES(3,3),HGURES(3,3),A0,A1,A2,C0,C1,C2
C
C  HEIGHT GROWTH LOWER AND UPPER RESIDUALS (3 parameters each - big 3 only)
C
      DATA HGLRES/
     1            0.0    ,  0.0    ,  0.0    ,                         !  DF,GF,WH
     2           -0.4961 , -0.4156 , -0.35487,                         !  DF,GF,WH
     3            0.02173,  0.00997,  0.0    /                         !  DF,GF,WH
      DATA HGURES/
     1            0.4956 ,  0.0   ,  0.0      ,                        !  DF,GF,WH
     2            0.5758 ,  0.9094,  0.2682203,                        !  DF,GF,WH
     3           -0.03378, -0.0554,  0.0       /                       !  DF,GF,WH
C
      A0=HGLRES(ISPGRP,1)
      A1=HGLRES(ISPGRP,2)
      A2=HGLRES(ISPGRP,3)
      C0=HGURES(ISPGRP,1)
      C1=HGURES(ISPGRP,2)
      C2=HGURES(ISPGRP,3)
      LRES=A0+A1*HG+A2*HG**2
      URES=C0+C1*HG+C2*HG**2
      IF(LRES.GT.0.0) LRES=0.0
      IF(URES.LT.0.0) URES=0.0
      RETURN
      END
********************************************************************************
      SUBROUTINE HGRES_SMC(ISPGRP,HG,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HG,LRES,URES,HGLRES(3,3),HGURES(3,3),A0,A1,A2,C0,C1,C2
C
C  HEIGHT GROWTH LOWER AND UPPER RESIDUALS (3 parameters each - big 3 only)
C
      DATA HGLRES/
     1            0.0    ,  0.0    ,  0.0    ,                         !  DF,GF,WH
     2           -0.4961 , -0.4156 , -0.4392 ,                         !  DF,GF,WH
     3            0.02173,  0.00997,  0.02751/                         !  DF,GF,WH
      DATA HGURES/
     1            0.4956 ,  0.0   ,  0.4388,                           !  DF,GF,WH
     2            0.5758 ,  0.9094,  0.5098,                           !  DF,GF,WH
     3           -0.03378, -0.0554, -0.02991/                          !  DF,GF,WH
C
      A0=HGLRES(ISPGRP,1)
      A1=HGLRES(ISPGRP,2)
      A2=HGLRES(ISPGRP,3)
      C0=HGURES(ISPGRP,1)
      C1=HGURES(ISPGRP,2)
      C2=HGURES(ISPGRP,3)
      LRES=A0+A1*HG+A2*HG**2
      URES=C0+C1*HG+C2*HG**2
      IF(LRES.GT.0.0) LRES=0.0
      IF(URES.LT.0.0) URES=0.0
      RETURN
      END
********************************************************************************
      SUBROUTINE HGRES_RAP(ISPGRP,HG,LRES,URES)
      IMPLICIT NONE
      INTEGER*4 ISPGRP
      REAL*4 HG,LRES,URES,HGLRES(3,3),HGURES(3,3),A0,A1,A2,C0,C1,C2
C
C  HEIGHT GROWTH LOWER AND UPPER RESIDUALS (3 parameters each - big 3 only)
C
      DATA HGLRES/
     1           -0.565597958    ,  0.0    ,  0.0    ,                         !  RA,DF,WH
     2           -0.259956282    , -0.4961 , -0.4392 ,                         !  RA,DF,WH
     3            0.0276968137   ,  0.02173,  0.02751/                         !  RA,DF,WH
      DATA HGURES/
     1            0.866609308    ,  0.4956 ,  0.4388,                          !  RA,DF,WH
     2            0.100265352    ,  0.5758 ,  0.5098,                          !  RA,DF,WH
     3           -0.0135706039   , -0.03378, -0.02991/                         !  RA,DF,WH
C
      A0=HGLRES(ISPGRP,1)
      A1=HGLRES(ISPGRP,2)
      A2=HGLRES(ISPGRP,3)
      C0=HGURES(ISPGRP,1)
      C1=HGURES(ISPGRP,2)
      C2=HGURES(ISPGRP,3)
      LRES=A0+A1*HG+A2*HG**2
      URES=C0+C1*HG+C2*HG**2
      IF(LRES.GT.0.0) LRES=0.0
      IF(URES.LT.0.0) URES=0.0
      RETURN
      END
