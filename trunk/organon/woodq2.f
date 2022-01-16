C----------
C ORGANON $Id$
C----------
      SUBROUTINE WQ_FILL(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI,
     1                   VERSION,WQINIT,SI_1,SI_2,PDEN,TDATAR,SCR,BRCNT,
     2                   BRDIA,BRHT,JCORE)
      IMPLICIT NONE
      REAL*4 SI_1,SI_2,PDEN,TDATAR(2000,8),SCR(2000,3)
      INTEGER*4 BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI(2000,3),VERSION,
     1          WQINIT,BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,40),
     2          JCORE(2000,40)
      LOGICAL*2 JCALC,EVEN
      INTEGER*4 II,III,WQAGE,ZBRDIA,ZJCORE,II1,JLAG,JJ,JK,JJJ,IKNT
      REAL*4    DINC,TOHT,HTCB,BDBH,RAT,HSH,HTCORE,X1,X2,A1,A2,
     1          B0,B1,B2,SITE,BBC,X50,AA1,BSH,BSHX,XAGE,SITE2,WHHLBH,
     2          WHHLBHX,XBRHT
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE CALL - TAPER_RA TO TAPER_RA_WOOD
C
C---------------------------
C
C     SUBROUTINE WQ_FILL HAS ONE FUNCTION:INITIATION OF WHORL BRANCH
C                    LOCATIONS, BRANCH DIAMETERS BELOW LIVE CROWN, AND
C                    JUVENILE WOOD CORE DIAMETER (IF LIVE CROWN IS ABOVE
C                    1 FT, OR IF BREAST HEIGHT AGE IS >20)
C---------------------------
C
C-----------------------------
C
C     ASSIGN WHORL BRANCH LOCATIONS, INITIAL BRANCH SIZES, AND
C        JUVENILE WOOD CORE IF NOT PREVIOUSLY DONE
C
C----------------------------
C
C     START INITIALIZATION
C
C       DETERMINE IF TREES BEING INITIALIZED ARE INGROWTH OR ORIGINAL
C         TREE LIST
C
C---------------------------
C
      IF(WQINIT .LT. 0) THEN
         II1=1
         III=0
      ELSE
         II1=NTREES-WQINIT+1
         III=NWQT
      ENDIF
C
C--------------------------
C
C     STEP THROUGH TREE LIST TO INITIALIZE
C
C-------------------------
C
      DO II=II1,NTREES
         IF(TDATAI(II,2) .GT. IB2) CYCLE
C         IF(VERSION .EQ. 4 .AND. TDATAI(II,2) .EQ. 1) CYCLE
         III=III+1
C Modified 10/27/94 for both
         SELECT CASE(VERSION)
           CASE(1)
             IF(TDATAI(II,1) .EQ. 202)THEN
                B0=-6.21693
                B1=0.281176
                B2=1.14354
                SITE=SI_1
             ELSE IF(TDATAI(II,1) .EQ. 122)THEN
                B0=-6.54707
                B1=0.288169
                B2=1.21297
                SITE=SI_2
             ELSE
                B0=-6.21693
                B1=0.281176
                B2=1.14354
                SITE=SI_1
                IF(TDATAI(II,1) .EQ. 81) SITE=((SITE+4.5)*0.7)-4.5
             ENDIF
             IF(EVEN .AND. WQINIT .LT. 0) THEN
                WQAGE=BHAGE
             ELSE
                BBC=B0+B1*LOG(SITE)
                X50=1.0-EXP((-1.)*EXP(BBC+B2*3.912023))
                AA1=1.0-(TDATAR(II,2)-4.5)*(X50/SITE)
                IF(AA1 .LT. 0.)THEN
                  WQAGE=500
                ELSE
                  XAGE=((-1.0*LOG(AA1))/(EXP(B0)*SITE**B1))**(1.0/B2)
                  WQAGE=(NINT(XAGE/5.0))*5
                ENDIF
             ENDIF
           CASE(2)
             SITE=SI_1+4.5
             X1=13.25-SITE/20.0
             X2=63.25-SITE/20.0
             B2=-0.447762-0.894427*SITE/100.0+0.793548*(SITE/100.0)**2
     1          -0.171666*(SITE/100.0)**3
             B1=ALOG(4.5/SITE)/(X1**B2-X2**B2)
             IF(EVEN .AND. WQINIT .LT. 0) THEN
                WQAGE=BHAGE
             ELSE
                XAGE=(ALOG(TDATAR(II,2)/SITE)/B1+X2**B2)**(1.0/B2)-X1
                WQAGE=(NINT(XAGE/5.0))*5
             ENDIF
           CASE(3)
             SITE=SI_1+4.5
             X1=13.25-SITE/20.0
             X2=63.25-SITE/20.0
             B2=-0.447762-0.894427*SITE/100.0+0.793548*(SITE/100.0)**2
     1          -0.171666*(SITE/100.0)**3
             B1=ALOG(4.5/SITE)/(X1**B2-X2**B2)
             IF(EVEN .AND. WQINIT.LT.0) THEN
                WQAGE=BHAGE
             ELSE
                XAGE=(ALOG(TDATAR(II,2)/SITE)/B1+X2**B2)**(1.0/B2)-X1
                WQAGE=(NINT(XAGE/5.0))*5
             ENDIF
           CASE(4)
             SITE=SI_1+4.5
             CALL WHHLB_SI_UC_WOOD(SITE,PDEN,SITE2)
C             IF(EVEN .AND. WQINIT.LT.0) THEN
             IF(WQINIT.LT.0) THEN
                WQAGE=STAGE
C             ELSE
C                CALL WHHLB_GEA(TDATAR(II,2),SITE2,XAGE)
C                WQAGE=NINT(XAGE)
             ENDIF
         ENDSELECT
         IF(VERSION .EQ. 4) THEN
            IF(WQAGE .LT. 3) WQAGE=3
         ELSE
            IF(WQAGE .LT. 5) WQAGE=5
         ENDIF
C
C               ------------
C                    JLAG = NUMBER OF YEARS INITIAL AGE IS PAST
C                            A MULTIPLE OF 5
C               ------------
C
         IF(VERSION .EQ. 4) THEN
C            JLAG = MOD(WQAGE,1)
            JLAG = 0
            BRCNT(III,1)=INT(WQAGE)
         ELSE
            JLAG = MOD(WQAGE,5)
            BRCNT(III,1)=INT(WQAGE/5)+2
         ENDIF
         TOHT=TDATAR(II,2)
         BDBH=TDATAR(II,1)
C         HTCB=(1.0-TDATAR(II,3))*TOHT
         IF(SCR(II,1) .GT. 0.0) THEN
            HTCB=(1.0-SCR(II,1))*TOHT
         ELSE
            HTCB=(1.0-TDATAR(II,3))*TOHT
         ENDIF
C
C        ----------------------
C
C        ASSIGN LOWEST BRANCH HEIGHT
C                    =  HEIGHT TO CROWN BASE IF HTCB < 4.5 FT
C                    =  2.3 FT               IF HTCB >= 4.5 FT
C        ---------------------
C
         IF(VERSION .LE. 3) THEN
            IF(HTCB .LT. 4.4)THEN
              BRHT(III,1)=INT(10*(HTCB+0.15))
              BRCNT(III,2)=0
              IF(JCALC) THEN
                BRCNT(III,3)=0
              ELSE
                IF(WQAGE .LT. 20) BRCNT(III,3)=0
              ENDIF
            ELSEIF(HTCB .EQ. 4.4) THEN
              BRHT(III,1)=23
              BRCNT(III,2)=1
              IF(JCALC) THEN
                BRCNT(III,3)=1
              ELSE
                IF(WQAGE .LT. 20) BRCNT(III,3)=0
              ENDIF
            ELSE
              BRHT(III,1)=23
              BRCNT(III,2)=2
              IF(JCALC) THEN
                BRCNT(III,3)=2
              ELSE
                IF(WQAGE .LT. 20) BRCNT(III,3)=0
              ENDIF
            ENDIF
            BRHT(III,2) = 45
         ENDIF
C
C         ----------------------
C
C         ASSIGN HEIGHTS TO INITIAL BRANCHES ABOVE DBH
C
C  Modified 10/27/94 for both models
C
         SELECT CASE(VERSION)
           CASE(1)
C
C               SCALED HANN-SCRIVANI HEIGHT GROWTH APPROACH FOR
C                    ASSIGNING WHORL HEIGHTS
C
             HSH=SITE*((1-EXP(-EXP(B0+B1*LOG(SITE)
     1          +B2*LOG(FLOAT(WQAGE)))))/
     2          (1-EXP(-EXP(B0+B1*LOG(SITE)+B2*LOG(50.0)))))
             RAT=(TDATAR(II,2)-4.5)/HSH
             DO JJ=3,BRCNT(III,1)
               BRHT(III,JJ)=INT(10*(4.5+RAT*SITE*
     1        ((1-EXP(-EXP(B0+B1*LOG(SITE)+B2*LOG(5.*(FLOAT(JJ)-2.)))))/
     2        (1-EXP(-EXP(B0+B1*LOG(SITE)+B2*LOG(50.0)))))))
             ENDDO
           CASE(2,3)
C
C               SCALED BRUCE HEIGHT GROWTH APPROACH FOR
C                    ASSIGNING WHORL HEIGHTS
C
             BSH=SITE*EXP(B1*((FLOAT(WQAGE)+X1)**B2-X2**B2))
             RAT=(TDATAR(II,2)-4.5)/(BSH-4.5)
             DO JJ=3,BRCNT(III,1)
                XAGE=5.0*(FLOAT(JJ)-2.0)
                BSHX=SITE*EXP(B1*((XAGE+X1)**B2-X2**B2))
                BRHT(III,JJ)=INT(10*(4.5+RAT*(BSHX-4.5)))
             ENDDO
           CASE(4)
C
C               SCALED WEISKITTEL,HANN.HIBBS,LAM,BLUHM HEIGHT GROWTH
C                    APPROACH FOR ASSIGNING WHORL HEIGHTS
C
             IKNT=0
             A1=-4.481266
             A2=-0.658884
             WHHLBH=SITE2*EXP(A1*(FLOAT(WQAGE)**A2-20.0**A2))
             RAT=TDATAR(II,2)/WHHLBH
             DO JJ=1,BRCNT(III,1)
                XAGE=FLOAT(JJ)
                WHHLBHX=SITE2*EXP(A1*(XAGE**A2-20.0**A2))
                XBRHT=RAT*WHHLBHX
                IF(XBRHT .LT. HTCB) THEN
                   IKNT=IKNT+1
                ENDIF
                BRHT(III,JJ)=INT(10*XBRHT)
             ENDDO
             BRCNT(III,2)=IKNT
        ENDSELECT
C
C          ----------------------
C
C             CHECK TO SEE IF WHORLS ARE DEAD, ASSIGN BRANCH DIAMETER
C                AND CROWN-DEFINED JUVENILE CORE IF SO
C
C          ----------------------
C
         IF(VERSION .LE. 3 .AND. BRCNT(III,2) .GT. 0) THEN
           DINC=TOHT-HTCB
           CALL BRANCH(TDATAI(II,1),DINC,BDBH,TOHT,HTCB,ZBRDIA)
           IF(JCALC)THEN
             CALL CORE(VERSION,TDATAI(II,2),HTCB,TOHT,BDBH,HTCB,ZJCORE)
           ENDIF
           DO JJ=2,BRCNT(III,1)
              IF(FLOAT(BRHT(III,JJ))/10 .GE. HTCB)THEN
                 BRCNT(III,2) = JJ-1
                 BRDIA(III,JJ-1)=ZBRDIA
                 IF(JCALC) THEN
                    JCORE(III,JJ-1)=ZJCORE
                    BRCNT(III,3) = JJ-1
                 ENDIF
                 EXIT
              ELSEIF(JJ .EQ. BRCNT(III,1)) THEN
                 BRDIA(III,JJ)=ZBRDIA
                 BRCNT(III,2)=JJ
                 IF(JCALC)THEN
                    JCORE(III,JJ)=ZJCORE
                    BRCNT(III,3)=JJ
                 ENDIF
                 EXIT
              ENDIF
           ENDDO
           DO JK=1,BRCNT(III,2)-1
              BRDIA(III,JK) = BRDIA(III,BRCNT(III,2))
              IF(JCALC) THEN
                 JCORE(III,JK) = JCORE(III,BRCNT(III,2))
              ENDIF
           ENDDO
         ELSEIF(VERSION .EQ. 4 .AND. BRCNT(III,2) .GT. 0) THEN
           DINC=TOHT-HTCB
           CALL BRANCH(TDATAI(II,1),DINC,BDBH,TOHT,HTCB,ZBRDIA)
           DO JJ=1,BRCNT(III,2)
                 BRDIA(III,JJ)=ZBRDIA
           ENDDO
         ENDIF
C
C             -----------------------
C
C             ASSIGN AGE-DEFINED JUVENILE CORE
C                                   IF BREAST HEIGHT AGE >= 20 YRS
C
C             -----------------------
C
         IF(.NOT. JCALC .AND. VERSION .LE. 3) THEN
           IF(5*(BRCNT(III,1)-2) .GE. 20) THEN
             HTCORE = FLOAT(BRHT(III,BRCNT(III,1)-4))/10. +
     1          (1.0-FLOAT(JLAG)/5)*(FLOAT(BRHT(III,BRCNT(III,1)-3)-
     2                       BRHT(III,BRCNT(III,1)-4))/10.)
             CALL CORE(VERSION,TDATAI(II,2),HTCORE,TOHT,BDBH,HTCB,
     1             JCORE(III,BRCNT(III,1)-4))
             BRCNT(III,3) = BRCNT(III,1) - 4
             DO JJJ=1,BRCNT(III,1)-5
                JCORE(III,JJJ) = JCORE(III,BRCNT(III,1)-4)
             ENDDO
           ENDIF
         ENDIF
      ENDDO
C
C     -------------------------
C
C       ESTABLISH NEW NUMBER OF WOOD QUALITY TREES (NWQT)
C
C     -------------------------
C
      NWQT=III
C
C     -------------------------
C
C          SET WQINIT TO 0 TO SIGNAL INITIALIZATION OF
C
C                                INITIAL TREE LIST OR
C                                         OR
C                                 NEW INGROWTH TREES
C
C     -------------------------
C
      WQINIT=0
C
      RETURN
      END
C
C*********************************************************************
      SUBROUTINE WQ_CALC(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,VERSION,
     1                   TDATAI,GROWTH,TDATAR,SCR,BRCNT,BRDIA,BRHT,
     2                   JCORE)
      IMPLICIT NONE
      REAL*4 GROWTH(2000,4),TDATAR(2000,8),SCR(2000,3)
      INTEGER*4 BHAGE,STAGE,IB2,NTREES,VERSION,TDATAI(2000,3),
     1          BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,40),
     2          JCORE(2000,40),GP
      LOGICAL*2 JCALC,EVEN
      INTEGER*4 II,III,JCOR1,JCOR2,WQAGE,JLAG,LL
      REAL*4      DINC,TOHT,HTCB,BDBH,FHCB,UD,UBH,UHC,SCALE,BHITE,UH
C
C---------------------------
C
C     SUBROUTINE WQ_CALC HAS ONE PRIMARY FUNCTION: UDATING MAXIMUM BRANCH
C                  DIAMETER AND JUVENILE WOOD CORE AS TREES GROW AND
C                  CROWNS RECEDE
C
C           EVERY 5TH WHORL IS TRACKED, AND WHENEVER A TREE IS CUT
C                EITHER BEFORE OR AFTER FINAL HARVEST, THE TREE SIZE
C                AND WHORL SPECIFIC INFORMATION IS WRITTEN TO A
C                SPECIFIED WOOD QUALITY OUTPUT FILE
C
C
C     START OF END-OF-GROWTH-PERIOD UPDATES FOR WOOD QUALITY
C
C     -----------------------------
C
C          STEP THROUGH TREE LIST TO UPDATE:
C
C                          MAXMIMUM BRANCH DIAMETER
C                          JUVENILE WOOD CORE
C
C     -----------------------------
C
      III=0
      DO II=1,NTREES
C
C     ---------------------------
C
C         BYPASS WOOD QUALITY PREDICITIONS IF TREE
C                                 IS NOT A BIG FIVE SPECIES
C
C     ----------------------------
C
        IF(TDATAI(II,2) .GT. IB2) CYCLE
C        IF(VERSION .EQ. 4 .AND. TDATAI(II,2) .EQ. 1) CYCLE
C
C     ----------------------------
C
C          INCREMENT WOOD QUALITY ARRAY INDEX (III)
C
C     ----------------------------
C
        III=III+1
C
C     ----------------------------
C
C          BYPASS WOOD QUALITY PREDICTIONS IF TREE HAS DIED
C
C     ----------------------------
C
        IF(TDATAR(II,4) .LE. 0.0) CYCLE
C
C     ----------------------------
C
C          BYPASS TREE IF CROWN HAS ALREADY RECEDED 200 WHORLS
C                            (THAT IS, 40 SETS OF 5 WHORLS)
C
C     ----------------------------
C
        IF(BRCNT(III,2) .GE. 40) CYCLE
C
C     ----------------------------
C
C         TDATAR(II,2) = ENDING HEIGHT
C         GROWTH(II,1) = 5 YEAR HEIGHT GROWTH
C         TDATAR(II,1) = ENDING DBH
C         GROWTH(II,2) = 5 YEAR DBH GROWTH
C
C         CALC BEGINNING TOHT, BDBH, AND HTCB
C
C         TOHT = BEGINNING TOTAL HEIGHT
C         BDBH = BEGINNING BREAST HT STEM DIAMETER
C         HTCB = BEGINNING HEIGHT TO CROWN BASE
C         FHCB = FINAL HEIGHT TO CROWN BASE
C
C         DEFINE VARIABLES FOR WOOD QUALITY PREDICTIONS BASED ON
C          CONDITIONS AT BEGINNING OF THE GROWTH PERIOD
C
C     -----------------------------
C
        TOHT=TDATAR(II,2) - GROWTH(II,1)
        BDBH=TDATAR(II,1) - GROWTH(II,2)
C        HTCB=TOHT*(1-TDATAR(II,7))
C        FHCB=TDATAR(II,2)*(1.-TDATAR(II,3))
        IF(SCR(II,3) .GT. 0.0) THEN
           HTCB=(1.0-SCR(II,3))*TOHT
        ELSE
           HTCB=(1.0-TDATAR(II,7))*TOHT
        ENDIF
        IF(SCR(II,1) .GT. 0.0) THEN
           FHCB=(1.0-SCR(II,1))*TDATAR(II,2)
        ELSE
           FHCB=(1.0-TDATAR(II,3))*TDATAR(II,2)
        ENDIF
C
C     ---------------------------
C
C      INCREMENT ASSIGNED WHORL COUNT BY 1 AND ASSIGN WHORL HEIGHT
C
C     --------------------------
C
        IF(EVEN) THEN
          IF(VERSION .EQ. 4) THEN
C             JLAG=MOD(STAGE,1)
             JLAG=0
             GP=1
          ELSE
             JLAG=MOD(BHAGE,5)
             GP=5
          ENDIF
        ELSE
          JLAG=0
        ENDIF
        IF(BRCNT(III,1) .LT. 40) THEN
          BRCNT(III,1)=BRCNT(III,1)+1
          IF(JLAG .EQ. 0) THEN
            BRHT(III,BRCNT(III,1))=INT(10*TDATAR(II,2))
          ELSE
            BRHT(III,BRCNT(III,1))=INT(10*( TDATAR(II,2) -
     1               (FLOAT(JLAG)/GP) * GROWTH(II,1) ))
          ENDIF
        ENDIF
        IF(VERSION .LE. 3) THEN
           WQAGE=(BRCNT(III,1)-2)*GP
        ELSE
           WQAGE=STAGE
        ENDIF
C
C     ----------------------------
C
C      PREDICT AGE-DEFINED JUVENILE WOOD CORE FOR ANOTHER WHORL
C
C     ----------------------------
C
        IF(.NOT. JCALC .AND. VERSION .LE. 3) THEN
          IF(BRCNT(III,3) .LT. 40) THEN
            IF(WQAGE .GE. 20) THEN
              BRCNT(III,3) = BRCNT(III,3) + 1
              IF(BRCNT(III,3).EQ.1) BRCNT(III,3) = BRCNT(III,3) + 1
              BHITE=FLOAT(BRHT(III,BRCNT(III,3)))/10.
              CALL CORE(VERSION,TDATAI(II,2),BHITE,TOHT,BDBH,HTCB,JCOR1)
              CALL CORE(VERSION,TDATAI(II,2),BHITE,TDATAR(II,2),
     1                  TDATAR(II,1),FHCB,JCOR2)
              IF(JLAG.EQ.0) THEN
                JCORE(III,BRCNT(III,3))=JCOR2
              ELSE
                JCORE(III,BRCNT(III,3)) = JCOR2 -
     1                 INT((FLOAT(JLAG)/GP)*FLOAT(JCOR2-JCOR1))
              ENDIF
C
C           ----------------------------
C
C              ASSIGN JUVENILE WOOD CORE DOWN TO WHORL 1 IF IT
C                   HAS NOT ALREADY BEEN DONE (WHEN INITIAL BREAST
C                   HEIGHT AGE FIRST EXCEEDS 20 YRS)
C
C           ----------------------------
C
              IF(WQAGE .EQ. 20) THEN
                JCORE(III,1)=JCORE(III,2)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
C     --------------------------
C
C        CHECK FOR CROWN RECESSION PAST LOWEST LIVE WHORL
C
C        IF CROWN BASE HAS PASSED ANY PREVIOUSLY LIVE WHORLS,
C          ESTIMATE MAXIMUM BRANCH DIAMETER AND CROWN WOOD CORE
C
C     --------------------------
C
        DO LL=BRCNT(III,2)+1,BRCNT(III,1)
          IF(FLOAT(BRHT(III,LL))/10. GE. FHCB) EXIT
C
C       ------------------------
C
C           COMPUTE SCALING FACTOR FOR COMPUTING DBH, RD, AND HCB
C             AT TIME OF CROWN RECESSION PAST SUBJECT WHORL
C
C       ------------------------
C
          SCALE=(FLOAT(BRHT(III,LL))/10.-HTCB)/(FHCB-HTCB)
          UHC=FLOAT(BRHT(III,LL))/10.
          UD = TDATAR(II,1) - (1.-SCALE)*GROWTH(II,2)
          UH = TDATAR(II,2) - (1.-SCALE)*GROWTH(II,1)
          UBH = FLOAT(BRHT(III,LL))/10.
          DINC = UH - FLOAT(BRHT(III,LL))/10.
          CALL BRANCH(TDATAI(II,1),DINC,UD,UH,UHC,BRDIA(III,LL))
          BRCNT(III,2)=LL
          IF(JCALC .AND. VERSION .LE. 3)THEN
            CALL CORE(VERSION,TDATAI(II,2),FLOAT(BRHT(III,LL))/10.,
     1                UH,UD,UBH,JCORE(III,LL))
            BRCNT(III,3)=LL
          ENDIF
        ENDDO
      ENDDO
C
C     --------------------
C
C     END OF END-OF-GROWTH-PERIOD UPDATES FOR WOOD QUALITY
C
C     --------------------
C
      RETURN
      END
C
C*********************************************************************
C
C
      SUBROUTINE BRANCH(SP,DINC,DBH,HT,HC,IBD)
      IMPLICIT NONE
C
C--------------------
C
C     SUBROUTINE BRANCH CALCULATES MAXIMUM BRANCH DIAMETER
C                                               NEAR CROWN BASE
C
C     BRANCH DIAMETER EQUATIONS ARE SPECIES SPECIFIC
C
C-------------------
C
      INTEGER*4 SP,IBD
      REAL*4  DINC,DBH,HT,HC
      REAL*4 CL,CR,RDINC,Z,MCW,LCW,XBD
      CL=HT-HC
      CR=CL/HT
      RDINC=DINC/CL
      Z=1.0-RDINC
      IF(Z .LT. 0.0) Z=0.0
      IF(Z .GT. 1.0) Z=1.0
      IF(SP .EQ. 202) THEN
C
C     ----------------------------------
C
C       DOUGLAS-FIR
C
C     ----------------------------------
C
C     ----------------------------------
C
C       BRANCH DIAMETER MODEL 6c FROM MAGUIRE, JOHNSON AND CAHILL (1999)
C              CANADIAN JOURNAL OF FOREST RESEARCH 29: 1829-1840
C
C     ----------------------------------
C
      MCW=4.6198+1.8426*DBH-0.011311*DBH*DBH
      LCW=MCW*CR**(0.004363240*CL+0.6020020*(DBH/HT))
      IF(Z .GE. 1.0) THEN
         XBD=0.0
      ELSE
         XBD=(0.03691*LCW**1.1501)*((1.0-SQRT(Z))**(0.5569*Z**2.7096))
      ENDIF
      IBD=INT(100*XBD)
C
C       --------------------------------
C
      ELSEIF(SP .EQ. 15 .OR. SP .EQ. 17) THEN
C
C     ----------------------------------
C
C       TRUE FIR (WHITE AND GRAND)
C
C     ----------------------------------
C
C
C     ----------------------------------
C
C             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92
C
C        -------------------------------
C
         IBD = INT(100*(0.18840809*(DINC**0.406434)*EXP(0.029701*DBH)))
      ELSEIF(SP .EQ. 122) THEN
C
C     ----------------------------------
C
C       PONDEROSA PINE
C
C     ----------------------------------
C
C
C     ----------------------------------
C
C             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92
C
C        -------------------------------
C
         IBD = INT(100*(0.07762242*(DINC**(0.479889-0.001042*RDINC))*
     1                  EXP(0.025218*DBH)*(HT**0.319545)))
      ELSEIF(SP .EQ. 117) THEN
C
C     ----------------------------------
C
C       SUGAR PINE
C
C     ----------------------------------
C
C
C     ----------------------------------
C
C             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92
C
C        -------------------------------
C
         IBD = INT(100*(0.05575434*(DINC**0.197098)*(HT**0.629666)))
      ELSEIF(SP .EQ. 81) THEN
C
C     ----------------------------------
C
C       INCENSE CEDAR
C
C     ----------------------------------
C
C
C     ----------------------------------
C
C             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92
C
C        -------------------------------
C
         IBD = INT(100*(0.11311752*(DINC**0.675786)*(CL**(-0.564461))))
      ELSEIF(SP .EQ. 351) THEN
C
C     ----------------------------------
C
C       RED ALDER
C
C     ----------------------------------
C
C
C     ----------------------------------
C
C             BRANCH DIAMETER MODEL LAST CHANGED 10-20-10
C
C        -------------------------------
C
         IBD = INT(100*(0.160884735*(DINC**(0.747251187
     1                  -0.132263075*RDINC))*EXP(0.024891787*DBH)))
      ENDIF
      RETURN
      END
************************************************************************
      SUBROUTINE CORE(VERSION,SP,BHT,TOHT,DBH,HTCB,ICR)
      IMPLICIT NONE
C
C----------------------------------------
C
C     SUBROUTINE CORE
C
C         CALCULATES INSIDE BARK DIAMETER FOR
C
C                         JUVENILE WOOD CORE DIAMETER AND
C                         FINAL DIBs FOR HARVESTED STEMS
C
C----------------------------------------
C
C
      INTEGER*4 VERSION,SP,ICR
      REAL*4    BHT,TOHT,DBH,HTCB
      REAL*4    CR,DIB,D1,AA1,AA2,A3,A4,ALP,BBH,DCB,HD,RH,WLTX,PX1,PX2,
     1          JP1,JP2,I1,I2,A,B,C
C
      CR=1.0-HTCB/TOHT
      HD=(TOHT-4.5)/DBH
      SELECT CASE(VERSION)
        CASE(1)                   ! Southwest Oregon
           CALL SWO_DIB(SP,DBH,CR,DIB)
           CALL SWO_DIB1FT(SP,DBH,CR,D1)
           CALL SWO_TAPER(SP,AA1,AA2,A3,A4,ALP)
        CASE(2,3)                   ! Western Willamette Valley, SMC
           CALL NWO_DIB(SP,DBH,CR,DIB)
           CALL NWO_DIB1FT(SP,DBH,CR,D1)
           CALL NWO_TAPER(SP,AA1,AA2,A3,A4,ALP)
        CASE(4)                   !Red Alder Plantations
           CALL TAPER_RA_WOOD(DBH,TOHT,CR,BHT,DCB)
           GO TO 10
      ENDSELECT
      IF(BHT .LE. 4.5) THEN
         BBH=((4.5-((DIB/D1)**(2./3.))-(BHT
     1      *(1.0-((DIB/D1)**(2./3.)))))/3.5)**(3./2.)
         DCB=BBH*D1
      ELSE
         RH=(BHT-4.5)/(TOHT-4.5)
         WLTX=((ALP*HTCB)-4.5)/(TOHT-4.5)
         PX1=AA1+AA2*EXP(A3*HD**2)
         PX2=A4
         JP1=(RH-1.0)/(WLTX-1.0)
         JP2=(WLTX-RH)/(WLTX-1.0)
         IF(RH .GE. 0.0 .AND. RH .LE. WLTX) THEN
            I1=0.0
         ELSE
            I1=1.0
         ENDIF
         IF(WLTX.LE.0.0) THEN
            I2=0.0
         ELSE
            I2=1.0
         ENDIF
         A=1.0-RH+I2*(RH+I1*(JP1*(1.0+JP2)-1.0))-(RH-1.0)*(RH-I2*RH)
         B=PX1*(I2*(RH+I1*(JP1*(RH+WLTX*JP2)-RH))-(RH-1.0)*(RH-I2*RH))
         C=PX2*I2*((RH**2)+I1*(JP1*WLTX*(2.0*RH-WLTX+WLTX*JP2)-RH**2))
         DCB=DIB*(A+B+C)
      ENDIF
   10 ICR=NINT(100.0*DCB)
      RETURN
      END
C*********************************************************************
      SUBROUTINE WQ_END(FINAL,JCALC,NEXT,IB2,NTREES,VERSION,
     1                  TDATAI,MGEXP,TDATAR,SCR,BRCNT,BRDIA,BRHT,JCORE,
     2                  IDIB)
      IMPLICIT NONE
      REAL*4 MGEXP(2000),TDATAR(2000,8),SCR(2000,3)
      INTEGER*4 NEXT,IB2,NTREES,VERSION,TDATAI(2000,3),
     1          BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,40),
     2          JCORE(2000,40),IDIB(2000,40)
      LOGICAL*2 FINAL,JCALC
      REAL*4 TOHT,BDBH,HTCB,DINC
      INTEGER*4  III,LL,JJ,J,II,XDIB,IB3
C
C   Calculates Branch data.  Used here, and in MANAGE1
C
      III = 0
      IF(FINAL) THEN
        JJ=8
      ELSE
        JJ=4
      ENDIF
      IB3=1
      DO LL=1,NTREES
        IF(TDATAI(LL,2) .GE. IB3 .AND. TDATAI(LL,2) .LE. IB2) THEN
           III=III+1
           IF(NEXT .EQ. 4)THEN
             IF(MGEXP(LL) .LE. 0.0001) CYCLE
           ELSE
             IF(TDATAR(LL,JJ) .LE. 0.00001) CYCLE
           ENDIF
        ELSE
           CYCLE
        ENDIF
C
C          CALCULATE WOOD QUALITY PARAMETERS FOR CANOPY
C
        TOHT=TDATAR(LL,2)
        BDBH=TDATAR(LL,1)
C        HTCB=(1-TDATAR(LL,3))*TOHT
        IF(SCR(LL,1) .GT. 0.0) THEN
           HTCB=(1.0-SCR(LL,1))*TOHT
        ELSE
           HTCB=(1.0-TDATAR(LL,3))*TOHT
        ENDIF
C
C          CALCULATE JUVENILE CORE IF USING 20-YR DEFINITION
C
        IF(.NOT. JCALC .AND. VERSION .LE. 3) THEN
          DO J=BRCNT(III,3)+1,BRCNT(III,1)
            CALL CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,J))/10.0,
     1                  TOHT,BDBH,HTCB,XDIB)
            JCORE(III,J)=XDIB
          ENDDO
        ENDIF
        DO II=BRCNT(III,2)+1,BRCNT(III,1)
           DINC=TOHT-FLOAT(BRHT(III,II))/10.0
C
C          CALCULATE BRANCH DIAMETER
C
           CALL BRANCH(TDATAI(LL,1),DINC,BDBH,TOHT,HTCB,BRDIA(III,II))
C
C          CALCULATE JUVENILE WOOD CORE IF DEFINED BY CROWN
C
           IF(JCALC .AND. VERSION .LE. 3) THEN
              CALL CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,II))/10.0,
     1                  TOHT,BDBH,HTCB,XDIB)
              JCORE(III,II)=XDIB
           ENDIF
        ENDDO
C
C             CALCULATE STEM DIAMETER AT EACH 5TH WHORL HEIGHT
C
        DO J=1,BRCNT(III,1)
          IDIB(III,J)=0
        ENDDO
        IF(VERSION .LE. 3) THEN
           IF(JCALC) THEN
             DO J=1,BRCNT(III,1)
               IF(BRHT(III,J) .LT. (INT(10*HTCB)))THEN
                 CALL CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,J))/10.0,
     1                TOHT,BDBH,HTCB,XDIB)
                 IDIB(III,J)=XDIB
               ELSE
                 IDIB(III,J)=JCORE(III,J)
               ENDIF
             ENDDO
           ELSE
             DO J=1,BRCNT(III,1)
               IF(BRCNT(III,3).GT.0) THEN
                 IF(BRHT(III,J) .LE. BRHT(III,BRCNT(III,3))) THEN
                   CALL CORE(VERSION,TDATAI(LL,2),
     1                       FLOAT(BRHT(III,J))/10.0,TOHT,BDBH,HTCB,
     2                       XDIB)
                   IDIB(III,J)=XDIB
                 ELSE
                   IDIB(III,J)=JCORE(III,J)
                 ENDIF
               ELSE
                 IDIB(III,J)=JCORE(III,J)
               ENDIF
             ENDDO
           ENDIF
        ELSE
          DO J=1,BRCNT(III,1)
             CALL CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,J))/10.0,
     1                 TOHT,BDBH,HTCB,XDIB)
             IDIB(III,J)=XDIB
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
