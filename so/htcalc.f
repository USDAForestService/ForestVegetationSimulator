      SUBROUTINE HTCALC(JFOR,SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
      IMPLICIT NONE
C----------
C  **HTCALC--SO   DATE OF LAST REVISION:  08/19/15
C----------
C THIS ROUTINE CALCULATES A POTENTIAL HT GIVEN AN SPECIES SITE AND AGE
C IT IS USED TO CAL POTHTG AND SITE
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C
C----------
      LOGICAL DEBUG
      INTEGER ISPC,JOSTND,INDX,JFOR
      REAL HGUESS,AG,B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13
      REAL SINDX,X1,X2,X3,TERM,B,TERM2,B50,Z
      REAL DUNL1(6),DUNL2(6),DUNL3(6)
C----------
C  SPECIES ORDER:
C  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
C 11=WJ, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
C 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
C 31=MB, 32=OS, 33=OH
C----------
C  DATA STATEMENTS:
C----------
      DATA DUNL1/ -88.9, -82.2, -78.3, -82.1, -56.0, -33.8 /
      DATA DUNL2/ 49.7067, 44.1147, 39.1441,
     &            35.4160, 26.7173, 18.6400 /
      DATA DUNL3/ 2.375, 2.025, 1.650, 1.225, 1.075, 0.875 /
C----------
      IF(DEBUG)WRITE(JOSTND,10)
   10 FORMAT(' ENTERING HTCALC')
C----------
C LOAD THE BETA COEFFICIENTS.
C----------
      B0=BB0(ISPC)
      B1=BB1(ISPC)
      B2=BB2(ISPC)
      B3=BB3(ISPC)
      B4=BB4(ISPC)
      B5=BB5(ISPC)
      B6=BB6(ISPC)
      B7=BB7(ISPC)
      B8=BB8(ISPC)
      B9=BB9(ISPC)
      B10=BB10(ISPC)
      B11=BB11(ISPC)
      B12=BB12(ISPC)
      B13=BB13(ISPC)
C
      SELECT CASE(ISPC)
C
C-------
C WHITE PINE USE BRICKELL EQUATIONS
C-------
      CASE(1)
        HGUESS=SINDX/(B0*(1.0-B1*EXP(B2*AG))**B3)
C------
C DOUGLAS FIR USE COCHRAN PNW 251. THIS EQUATION ALSO USED FOR 
C OTHER SOFTWOODS
C------
      CASE(3,32)
        HGUESS=4.5 + EXP(B1 + B2*ALOG(AG) + B3*(ALOG(AG))**4)
     &       + B4*(B5 + B6*(1.0 - EXP(B7*AG))**B8)
     &       + (SINDX - 4.5)*(B5 + B6*(1.0 - EXP(B7*AG)**B8))
C------
C WHITE FIR USE COCHRAN PNW 252. THIS EQUATION IS ALSO USED IN FOR
C INCENSE CEDAR, GRAND FIR, AND PACIFIC SILVER FIR
C------
      CASE(4,6,12,14)
        X2= B0 + B1*ALOG(AG) + B2*(ALOG(AG))**4
     &    + B3*(ALOG(AG))**9 + B4*(ALOG(AG))**11
     &    + B5*(ALOG(AG))**18
        X3= B6 + B7*ALOG(AG) + B8*(ALOG(AG))**2
     &    + B9*(ALOG(AG))**7 + B10*(ALOG(AG))**16
     &    + B11*(ALOG(AG))**24
        HGUESS=EXP(X2) + B12*EXP(X3) + (SINDX - 4.5)*EXP(X3)
     &       + 4.5
        IF(DEBUG)WRITE(JOSTND,9500)X2,X3,SINDX,AG,HGUESS
 9500   FORMAT(' X2,X3,SINDX,AG,HGUESS',5E12.4)
C------
C MTN HEMLOCK USE INTERIM MEANS PUB
C------
      CASE(5)
        HGUESS = (B0 + B1*SINDX)*(1.0 - EXP(B2*SQRT(SINDX)*AG))
     &         **(B4 + B5/SINDX)
        HGUESS = (HGUESS + 1.37)*3.281
        IF(DEBUG)WRITE(JOSTND,9501)ISPC,AG,SINDX,B0,B1,B2,B4,B5
 9501   FORMAT(' HTCALC HEMLOC SP AG SI B S',I5,7E12.4)
C------
C LODGEPOLE PINE USE DAHMS PNW 8
C------
      CASE(7)
        HGUESS = SINDX*(B0 + B1*AG + B2*AG*AG)
C------
C ENGLEMANN SPRUCE USE ALEXANDER
C------
      CASE(8)
        HGUESS = 4.5+((B0*SINDX**B1)*(1.0-EXP(-B2*AG))**(B3*SINDX**B4))
C----------
C SHASTA FIR: R6 USES DOLPH RED FIR CURVES, RES PAP PSW 206
C             R5 USES DUNNING-LEVITATN CURVES
C----------
      CASE(9)
        SELECT CASE (JFOR)
        CASE (1:3,10)
          TERM=AG*EXP(AG*B3)*B2
          B = SINDX*TERM + B4*TERM*TERM + B5
          TERM2 = 50.0 * EXP(50.0*B3) * B2
          B50 = SINDX*TERM2 + B4*TERM2*TERM2 + B5
          HGUESS = ((SINDX-4.5) * (1.0-EXP(-B*(AG**B1)))) /
     &             (1.0-EXP(-B50*(50.0**B1)))
          HGUESS = HGUESS + 4.5
          IF(DEBUG)WRITE(JOSTND,*)' SINDX,AG,TERM,B,TERM2,B50,HGUESS= ',
     &    SINDX,AG,TERM,B,TERM2,B50,HGUESS
C
        CASE DEFAULT
C----------
C SET UP MAPPING TO THE CORRECT DUNNING-LEVITAN SITE CURVE
C----------
          IF(SINDX .LE. 44.) THEN
            INDX=6
          ELSEIF (SINDX.GT.44. .AND. SINDX.LE.52.) THEN
            INDX=5
          ELSEIF (SINDX.GT.52. .AND. SINDX.LE.65.) THEN
            INDX=4
          ELSEIF (SINDX.GT.65. .AND. SINDX.LE.82.) THEN
            INDX=3
          ELSEIF (SINDX.GT.82. .AND. SINDX.LE.98.) THEN
            INDX=2
          ELSE
            INDX=1
          ENDIF
          IF(AG .LE. 40.) THEN
            HGUESS = DUNL3(INDX) * AG
          ELSE
            HGUESS = DUNL1(INDX) + DUNL2(INDX)*ALOG(AG)
          ENDIF
          IF(DEBUG)WRITE(JOSTND,*)' ISPC,SINDX,AG,INDX,HGUESS= ',
     &    ISPC,SINDX,AG,INDX,HGUESS
        END SELECT
C------
C PONDEROSA PINE USE BARRETT. THIS EQUATION ALSO USED FOR SUGAR PINE.
C------
       CASE(2,10)
        IF(DEBUG)WRITE(JOSTND,20)B0,B1,AG,B2,B3,B4,B5,B6,B7,SINDX
   20   FORMAT(' HTCALC 20F B0-1,AG,B2-7,SINDX = ',/,1H ,10F13.8)
        HGUESS =( B0*(1.0 -EXP(B1*AG))**B2)
     &         - ((B3 + B4*(1.0-EXP(B5*AG))**B6)*B7)
     &         + ((B3 + B4*(1.0 - EXP(B5*AG))**B6)*(SINDX - 4.5))
     &         + 4.5
C------
C SUB ALPINE FIR USES JOHNSON'S EQUIV OF DEMARS
C------
      CASE(13)
        HGUESS=SINDX*(B0 + B1*AG + B2*AG*AG)
C----------
C NOBLE FIR - HERMAN, PNW-243
C----------
      CASE(15)
        X1 = B0 + B1*(SINDX - 4.5) - B2*(SINDX - 4.5)**2
        X2 = B3 + B4*(SINDX - 4.5)**(-1) + B5*(SINDX - 4.5)**(-2)
        HGUESS = 4.5 + (SINDX - 4.5)/(X1*(1.0/AG)**2 +
     &   X2*(1.0/AG) + 1.0 - 0.0001*X1 - 0.01*X2)
C----------
C WESTERN LARCH USE COCHRAN PNW 424
C----------
      CASE(17)
        HGUESS=4.5 + B1*AG + B2*AG*AG + B3*AG**3  +
     &       B4*AG**4 + (SINDX -4.5)*(B5 + B6*AG
     &       + B7*AG*AG + B8*AG**3)-
     &       B9*(B10 + B11*AG + B12*AG*AG + B13*AG**3)
C----------
C RED CEDAR ---- USE HEGYI, JELINEK, VISZLAI,
C & CARPENTER 1981 FOR SITE REFERENCE
C----------
      CASE(18)
        IF(DEBUG)WRITE(JOSTND,9502)ISPC,AG,SINDX,B1,B2,B3
 9502   FORMAT(' HTCALC RED CEDAR ISPC,AG,SI,B1,B2,B3= ',I5,5E12.4)
        HGUESS = B1*SINDX * ((1.0 - EXP(B2*AG))**B3)
C----------
C WESTERN HEMLOCK - WILEY, 1978
C----------
      CASE(19)
        IF(DEBUG)WRITE(JOSTND,*)' WESTERN HEMLOCK EQUATION'
        Z = 2500./(SINDX-4.5)
        HGUESS = (AG*AG/(B0 + B1*Z + (B2 + B3*Z)*AG +
     &          (B4 + B5*Z)*(AG*AG))) + 4.5
C----------
C MISC. SPECIES - USE CURTIS, FOR. SCI. 20:307-316.  CURTIS CURVES
C ARE PRESENTED IN METRIC (3.2808 ?)
C
C EXCESSIVE HT GROWTH -- APPROX 30-40 FT/CYCLE, TAKE OUT METRIC MULT
C DIXON 11-05-92
C----------
      CASE(20,21,23,25,26,28:31,33)
        HGUESS = (SINDX - 4.5) / ( B0 + B1/(SINDX - 4.5)
     &         + B2 * AG**(-1.4) +(B3/(SINDX - 4.5))*AG**(-1.4))
        HGUESS = HGUESS + 4.5
C----------
C RED ALDER - HARRINGTON, PNW-358
C----------
      CASE(22)
        HGUESS = SINDX
     &         + (B0 + B1*SINDX)*(1.0-EXP((B2 + B3*SINDX)*AG))**B4
     &         - (B0 + B1*SINDX)*(1.0-EXP((B2 + B3*SINDX)*20.0))**B4
C----------
C R6 POWERS BLACK OAK RES NOTE PSW-262
C R5 DUNNING-LEVITAN SITE CURVES
C----------
      CASE(27)
        SELECT CASE (JFOR)
        CASE (1:3,10)
          TERM = SQRT(AG)-SQRT(50.)
          HGUESS = (SINDX * (1 + B1*TERM)) - B0*TERM
          IF(DEBUG)WRITE(JOSTND,*)' ISPC,B0,B1,SINDX,AG,TERM,HGUESS= ',
     &    ISPC,B0,B1,SINDX,AG,TERM,HGUESS
C
        CASE DEFAULT
C----------
C SET UP MAPPING TO THE CORRECT DUNNING-LEVITAN SITE CURVE
C----------
          IF(SINDX .LE. 44.) THEN
            INDX=6
          ELSEIF (SINDX.GT.44. .AND. SINDX.LE.52.) THEN
            INDX=5
          ELSEIF (SINDX.GT.52. .AND. SINDX.LE.65.) THEN
            INDX=4
          ELSEIF (SINDX.GT.65. .AND. SINDX.LE.82.) THEN
            INDX=3
          ELSEIF (SINDX.GT.82. .AND. SINDX.LE.98.) THEN
            INDX=2
          ELSE
            INDX=1
          ENDIF
          IF(AG .LE. 40.) THEN
            HGUESS = DUNL3(INDX) * AG
          ELSE
            HGUESS = DUNL1(INDX) + DUNL2(INDX)*ALOG(AG)
          ENDIF
          IF(DEBUG)WRITE(JOSTND,*)' ISPC,SINDX,AG,INDX,HGUESS= ',
     &    ISPC,SINDX,AG,INDX,HGUESS
        END SELECT
C----------
C WESTERN JUNIPER, WHITEBARK PINE, AND QUAKING ASPEN DO NOT HAVE SITE CURVES
C----------
      CASE(11,16,24)
        HGUESS=0.0
C
      END SELECT
C
      RETURN
      END
