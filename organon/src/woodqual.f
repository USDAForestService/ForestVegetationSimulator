      SUBROUTINE WOODQUAL(IJCALC,IEVEN,IFINAL,ACTION,BHAGE,STAGE,NINGRO,
     1                    NPTS,NTREES,NWQT,VERSION,SPECIES,SITE_1,
     2                    SITE_2,PDEN,DBH,HT,CR,SCRX,EXPAN,MGEXP,DGRO,
     3                    HGRO,CRCHNG,SCRCHNG,BRCNT,BRDIA,BRHT,JCORE,
     4                    IDIB)
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SPGROUP TO SPGROUP_WOOD
C  CHANGED THE NAME OF SUBROUTINE - TAPER_RA TO TAPER_RA_WOOD
C
      IMPLICIT NONE
      INTEGER*4 IJCALC,IEVEN,IFINAL,ACTION,BHAGE,STAGE,NINGRO,NPTS,
     1          NTREES,NWQT,VERSION,SPECIES(2000)
      REAL*4 SITE_1,SITE_2,PDEN,DBH(2000),HT(2000),CR(2000),SCRX(2000),
     1       EXPAN(2000),MGEXP(2000),DGRO(2000),HGRO(2000),
     2       CRCHNG(2000),SCRCHNG(2000)
      INTEGER*4 TDATAI(2000,3),SPGRP(2000),IB2,NEXT,WQINIT,
     1          BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,40),
     2          JCORE(2000,40),IDIB(2000,40)
      REAL*4 TDATAR(2000,8),GROWTH(2000,4),SCR(2000,3),SI_1,SI_2,XSI50
      LOGICAL*2 JCALC,EVEN,FINAL
      INTEGER*4 I,II,J
      JCALC =  .FALSE.
      EVEN =   .FALSE.
      FINAL =  .FALSE.
      IF(IJCALC .EQ. 1) THEN
         JCALC = .TRUE.
      ENDIF
      IF(IEVEN .EQ. 1) THEN
         EVEN = .TRUE.
      ENDIF
      IF(IFINAL .EQ. 1) THEN
         FINAL = .TRUE.
      ENDIF
      SELECT CASE(VERSION)
        CASE(1)
          IB2=5
        CASE(2,3)
          IB2=2
        CASE(4)
          IB2=1
      ENDSELECT
      SI_1=SITE_1
      SI_2=SITE_2
      DO I=1,2000
        GROWTH(I,3)=0.0
        GROWTH(I,4)=0.0
        TDATAI(I,1)=0.0
        TDATAI(I,2)=0.0
        TDATAI(I,3)=0.0
        TDATAR(I,1)=0.0
        TDATAR(I,2)=0.0
        TDATAR(I,3)=0.0
        TDATAR(I,4)=0.0
        TDATAR(I,5)=0.0
        TDATAR(I,6)=0.0
        TDATAR(I,7)=0.0
        TDATAR(I,8)=0.0
        SCR(I,1)=0.0
        SCR(I,2)=0.0
        SCR(I,3)=0.0
      ENDDO
      DO I=1,NTREES
        GROWTH(I,1)=HGRO(I)
        GROWTH(I,2)=DGRO(I)
        TDATAI(I,1)=SPECIES(I)
        CALL SPGROUP_WOOD(VERSION,I,SPECIES,SPGRP)
        TDATAI(I,2)=SPGRP(I)
        TDATAR(I,1)=DBH(I)
        TDATAR(I,2)=HT(I)
        TDATAR(I,3)=CR(I)
        TDATAR(I,4)=EXPAN(I)
        SCR(I,1)=SCRX(I)
      ENDDO
      IF(VERSION .EQ. 1)THEN
        IF(SI_1 .LE. 0.0 .AND. SI_2 .GT. 0.0)THEN
          SI_1=1.062934*SI_2
        ELSEIF(SI_2 .LE. 0.0)THEN
          SI_2=0.940792*SI_1
        ENDIF
      ELSEIF(VERSION .EQ. 2 .OR. VERSION .EQ. 3)THEN
C  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
        IF(SI_1 .LE. 0.0 .AND. SI_2 .GT. 0.0)THEN
          SI_1=0.480 +( 1.110 * SI_2)
        ELSEIF(SI_2 .LE. 0.0)THEN
          SI_2=-0.432 +( 0.899 * SI_1)
        ENDIF
      ELSE
        IF(SI_2 .LE. 0.0) SI_2=125.0
      ENDIF
C
C     CHANGE TO EXPANSION FACTOR FOR STAND--NOT SAMPLE
C
      DO J=1,NTREES
        TDATAR(J,4)=TDATAR(J,4)/FLOAT(NPTS)
        MGEXP(J)=MGEXP(J)/FLOAT(NPTS)
        TDATAR(J,5)=TDATAR(J,4)
        IF(SCR(J,1) .GT. 0.0) THEN
          TDATAR(J,6)=SCR(J,1)
          SCR(J,2)=SCR(J,1)
        ELSE
          TDATAR(J,6)=TDATAR(J,3)
        ENDIF
        SCR(J,3)=SCR(J,1)-SCRCHNG(J)
        TDATAR(J,7)=TDATAR(J,3)-CRCHNG(J)
        IF(FINAL) THEN
           TDATAR(J,8)=MGEXP(J)
        ELSE
           TDATAR(J,8)=TDATAR(J,4)
        ENDIF
      ENDDO
      SI_1=SI_1-4.5
      SI_2=SI_2-4.5
      SELECT CASE(ACTION)
         CASE(1)
            WQINIT=-1
            CALL WQ_FILL(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI,
     1                   VERSION,WQINIT,SI_1,SI_2,PDEN,TDATAR,SCR,BRCNT,
     2                   BRDIA,BRHT,JCORE)
         CASE(2)
            WQINIT=NINGRO
            CALL WQ_FILL(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI,
     1                   VERSION,WQINIT,SI_1,SI_2,PDEN,TDATAR,SCR,BRCNT,
     2                   BRDIA,BRHT,JCORE)
         CASE(3)
            CALL WQ_CALC(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,VERSION,
     1                   TDATAI,GROWTH,TDATAR,SCR,BRCNT,BRDIA,BRHT,
     2                   JCORE)
         CASE(4)
            NEXT=4
            CALL WQ_END(FINAL,JCALC,NEXT,IB2,NTREES,VERSION,
     1                  TDATAI,MGEXP,TDATAR,SCR,BRCNT,BRDIA,BRHT,JCORE,
     2                  IDIB)
         CASE(5)
            NEXT=1
            CALL WQ_END(FINAL,JCALC,NEXT,IB2,NTREES,VERSION,
     1                  TDATAI,MGEXP,TDATAR,SCR,BRCNT,BRDIA,BRHT,JCORE,
     2                  IDIB)
      ENDSELECT
      RETURN
      END
C***********************************************************************
      SUBROUTINE SPGROUP_WOOD(VERSION,I,SPECIES,SPGRP)
C     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,I,SPECIES(2000),SPGRP(2000),ISX,J
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4  SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
C
      INTEGER*4   SCODE3(7)/
     1           351,202,263,242,312,492,920/
      ISX = -9999
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES(I).EQ. SCODE1(J))THEN
                ISX = J
                IF(ISX .GT.2) ISX=ISX-1
                EXIT
             ENDIF
           ENDDO
        CASE(2,3)
           DO J = 1, 11
              IF(SPECIES(I).EQ. SCODE2(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
        CASE(4)
           DO J = 1, 11
              IF(SPECIES(I).EQ. SCODE3(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
      ENDSELECT
      SPGRP(I)=ISX
      RETURN
      END
********************************************************************************
      SUBROUTINE WHHLB_SI_UC_WOOD(SI_C,PDEN,SI_UC)
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
C**********************************************************************
      SUBROUTINE TAPER_RA_WOOD(DBH,HT,CR,HI,DI)
      IMPLICIT NONE
      REAL*4 DBH,HT,CR,HI,X,Z,P,A1,A2,A3,A4,A5,A6,A7,C,DI,D140
      A1=0.9113
      A2=1.0160
      A3=0.2623
      A4=-18.7695
      A5=3.1931
      A6=0.1631
      A7=0.4180
      D140=0.000585+0.997212*DBH
      Z=HI/HT
      P=4.5/HT
      X=(1.0-Z**0.5)/(1.0-P**0.5)
      C=A3*(1.364409*D140**0.3333333*EXP(A4*Z)
     1 +EXP(A5*CR**A6*(D140/HT)**A7*Z))
      DI=A1*D140**A2*X**C
      RETURN
      END
