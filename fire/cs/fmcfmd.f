      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C FIRE-CS $Id$
C----------
C  SINGLE-STAND VERSION
C  CALLED FROM: FMBURN
C  PURPOSE:
C     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
C     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
C     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
C     THE CLOSEST FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE
C     BY THE DYNAMIC FUEL MODEL OPTION
C----------
C  CALL LIST DEFINITIONS:
C     FMD:     FUEL MODEL NUMBER
C
C  COMMON BLOCK VARIABLES AND PARAMETERS:
C     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
C     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C      
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'    ! since using FIA algorithm info
C
C
COMMONS
C----------
C     LOCAL VARIABLE DECLARATIONS
C----------
      INTEGER ICLSS
      PARAMETER(ICLSS = 14)
C
      INTEGER  IYR,FMD
      INTEGER  IPTR(ICLSS), ITYP(ICLSS)
      INTEGER  J,I,IFFEFT
C
      REAL     XPTS(ICLSS,2),EQWT(ICLSS)
      REAL     X, Y, RCHT, RCTPA, MOISWT(ICLSS)
      LOGICAL  DEBUG
C----------
C  THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
C  CLASS. THEY ARE RETURNED WITH THE WEIGHT
C----------
      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /
C----------
C  THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
C  VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
C  AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
C  SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
C  SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.
C----------
      DATA ITYP / ICLSS * 0 /
C----------
C  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C  WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C  COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C  SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).
C----------
      DATA ((XPTS(I,J), J=1,2), I=1,ICLSS) /
     &   5., 15.,   ! FMD   1
     &   5., 15.,   ! FMD   2
     &   5., 15.,   ! FMD   3
     &   5., 15.,   ! FMD   4
     &   5., 15.,   ! FMD   5
     &   5., 15.,   ! FMD   6
     &   5., 15.,   ! FMD   7
     &   5., 15.,   ! FMD   8
     &   5., 15.,   ! FMD   9
     &  10., 30.,   ! FMD  10 ! moved line based on SN workshop input
     &  15., 30.,   ! FMD  11
     &  30., 60.,   ! FMD  12  
     &  45.,100.,   ! FMD  13
     &  30., 60./   ! FMD  14
C----------
C  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE
C  COLINEAR, AND COLINEARITY WEIGHTS ARE ZERO. IF TWO CANDIDATE
C  MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C  MUST SUM TO 1, WRT EACH OTHER
C----------
      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO
C----------
C  BEGIN ROUTINE
C----------
      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)
C
      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)
C----------
C  IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.
C----------
      IF (LUSRFM) RETURN
C
      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C----------
C  DETERMINE FFE FOREST TYPE (1 OF 8 CATEGORIES) FROM FIA FOR. TYPE
C----------     
      CALL FMCSFT(IFFEFT)
C----------   
C  LOW FUEL MODEL SELECTION
C  FIRST, WE NEED TO SEE WHICH FUEL MODELS SHOULD BE SELECTED
C  BASED ON THE ABOVE IFFEFT.
C  THE AMOUNT OF SMALL AND LARGE FUEL PRESENT CAN MODIFY THE CHOICE
C  OF MODEL.  (SEE CODE BELOW)
C----------
      SELECT CASE (IFFEFT)
      CASE (1, 2, 3) ! hardwood, hardwood/pine, or pine/hardwood 
        IF (SMALL .GT. 6) THEN
          EQWT(5) = 1.0
        ELSE     
          IF (MOIS(1,4) .LE. 0.15) THEN
            MOISWT(9) = 1.0
          ELSEIF (MOIS(1,4) .GT. 0.25) THEN
            MOISWT(8) = 1.0
          ELSE
            MOISWT(9) = 1.0 - (MOIS(1,4) - 0.15)/0.1
            MOISWT(8) = 1.0 - (0.25 - MOIS(1,4))/0.1        
          ENDIF
C       
          IF (SMALL .LE. 4) THEN
            EQWT(8) = MOISWT(8)
            EQWT(9) = MOISWT(9)
          ELSE        
            EQWT(8) = (1.0 - (SMALL - 4)/2)*MOISWT(8)
            EQWT(9) = (1.0 - (SMALL - 4)/2)*MOISWT(9)
            EQWT(5) = 1.0 - (6 - SMALL)/2
          ENDIF
        ENDIF
C      
      CASE (4, 8) ! pine or saint francis
        IF (MOIS(1,4) .LE. 0.15) THEN
          EQWT(9) = 1.0
        ELSEIF (MOIS(1,4) .GT. 0.25) THEN
          EQWT(8) = 1.0
        ELSE
          EQWT(9) = 1.0 - (MOIS(1,4) - 0.15)/0.1
          EQWT(8) = 1.0 - (0.25 - MOIS(1,4))/0.1        
        ENDIF
C      
      CASE (5, 6) ! pine bluestem or oak savannah
        EQWT (2) = 1.0
C      
      CASE (7) ! eastern redcedar
        RCHT = 0
        RCTPA = 0
        DO I = 1,ITRN
          X = HT(I)
          Y = FMPROB(I)
          SELECT CASE (ISP(I))
            CASE (1,2)  ! redcedar
              RCHT = RCHT + X
              RCTPA = RCTPA + Y
          END SELECT
        ENDDO
C        
        IF (RCTPA .GT. 0) RCHT = RCHT/RCTPA
C       
        IF (RCHT .GT. 7.5) THEN
          EQWT(4) = 1.0
        ELSEIF (RCHT .LE. 4.5) THEN
          EQWT(6) = 1.0
        ELSE  
          EQWT(6) = 1.0 - (RCHT - 4.5)/3
          EQWT(4) = 1.0 - (7.5 - RCHT)/3
        ENDIF
C        
      CASE (9)
        EQWT(6) = 1.0
      END SELECT
C----------
C  END OF DETAILED LOW FUEL MODEL SELECTION
C----------
C  MODELS 10,12 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
C  OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
C  MODELS 11, 13, AND 14 ARE NOT CANDIDATES IN THE OZARK FFE
C----------
      EQWT(10)  = 1.0
      EQWT(12)  = 1.0
      EQWT(11)  = 0.0
      EQWT(13)  = 0.0
      EQWT(14)  = 0.0
C----------
C  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)
C----------
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)
C
      IF (DEBUG) WRITE (JOSTND,8) FMD,LDYNFM
    8 FORMAT (' FMCFMD, FMD=',I4,' LDYNFM=',L2)
C
      RETURN
      END
