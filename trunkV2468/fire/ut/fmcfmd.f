      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C FIRE-UT $Id: fmcfmd.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
*  SINGLE-STAND VERSION
*  CALLED FROM: FMBURN
*  PURPOSE:
*     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
*     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
*     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
*     FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE BY THE DYNAMIC
*     FUEL MODEL
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FMD:     FUEL MODEL NUMBER
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
*     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
*
***********************************************************************
C----------
COMMONS
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
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'SSTGMC.F77'
C
COMMONS
C----------
C  LOCAL VARIABLE DECLARATIONS
C
C  NUMBER OF POTENTIAL FIRE MODELS
C----------
      INTEGER ICLSS
      PARAMETER(ICLSS = 12)
C----------
C  COVER METAGROUP ENUMS:
C
C     OBCT  =  1  ! OAK BRUSH COVER TYPE
C     PJCT  =  2  ! PINYON JUNIPER
C     PPCT  =  3  ! PONDEROSA PINE
C     WSCT  =  4  ! WHITE SPRUCE
C     SFCT  =  5  ! SPRUCE FIR
C     LPCT  =  6  ! LODGEPOLE PINE
C     MCCT  =  7  ! MIXED CONIFER
C     ASCT  =  8  ! ASPEN !! NOTE THAT SUBSEQUENT FOR-LOOPS USE THIS U-BOUND
C----------
      INTEGER OBCT, PJCT, PPCT, WSCT, SFCT, LPCT, MCCT, ASCT
      PARAMETER(
     &  OBCT  =  1,
     &  PJCT  =  2,
     &  PPCT  =  3,
     &  WSCT  =  4,
     &  SFCT  =  5,
     &  LPCT  =  6,
     &  MCCT  =  7,
     &  ASCT  =  8)
C----------
C  LOCAL VARIABLE DECLARATIONS:
C----------
      CHARACTER VVER*7
      INTEGER  FMD, ICT, USCT, SIFM, IS, II
      INTEGER  IYR,I,J,J2,I1,I2
      INTEGER  IPTR(ICLSS), ITYP(ICLSS)
      INTEGER  HPOINT(MAXTRE), IDUM(1), IFMST
      REAL     CTBA(ASCT), USBA(ASCT), STNDBA, XW, CVR10, AFWT
      REAL     SSUMN, P, FMAVH, XWID(MAXTRE), SIBRK
      REAL     XPTS(ICLSS,2)
      REAL     X, Y, X3, Y1, Y2, Y3, Y4, Y5, Y6
      REAL     BL, BD, SNVIH, SNVIS, VT, XVEC(2), YVEC(2), ALGSLP
      REAL     PSUM, USHT, TINY
      REAL     EQWT(ICLSS),XNEG1
      LOGICAL  DEBUG, LDRY, LUNDER, LCUNDR
      LOGICAL  LWFOK, LPPDOM, LMERCH
C----------
C  DUMMY ENTRY FOR **COVOLP**
C----------
      DATA     IDUM /0/
C----------
C  These are the integer tags associated with each fire model
C  class. They are returned with the weight
C----------
      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12 /
C----------
C  These are 0 for regular lines, -1 for horizontal and 1 for
C  vertical lines. If any of the lines defined by xpts() are of
C  an unusual variety, this must be entered here so that
C  special logic can be invoked.  In this case, all the line
C  segments have a |slope| that is > 0 and less than inif.
C----------
      DATA ITYP / ICLSS * 0 /
C----------
C  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C     WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C     COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C     SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).
C----------
      DATA ((XPTS(I,J), J=1,2), I=1,ICLSS) /
     >   5., 15.,   ! FMD   1
     >   5., 15.,   ! FMD   2
     >   5., 15.,   ! FMD   3
     >   5., 15.,   ! FMD   4
     >   5., 15.,   ! FMD   5
     >   5., 15.,   ! FMD   6
     >   5., 15.,   ! FMD   7
     >   5., 15.,   ! FMD   8
     >   5., 15.,   ! FMD   9
     >  15., 30.,   ! FMD  10 ! shares with 11
     >  15., 30.,   ! FMD  11
     >  30., 60./   ! FMD  12
C----------
C  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
C  WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C  MUST SUM TO 1, WRT EACH OTHER
C----------
      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO
C----------
C  ** END OF GENERAL FUEL MODEL SETUP **
C
C  BEGIN ROUTINE
C----------
      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)
C----------
C  IF THE USER SPECIFIED THE FM DEFINITIONS, THEN WE ARE DONE.
C----------
      IF (LUSRFM) RETURN
C
      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,
     >           SMALL,LARGE
    7 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C
      LDRY = .FALSE.
      IF (IYR .GE. IDRYB .AND. IYR .LE. IDRYE) LDRY = .TRUE.
C----------
C  FIND DESCENDING SORT ORDER WRT HEIGHT, THEN (COPIED
C  FROM **DENSE**) STEP FROM THE LARGEST DOWNWARD TO GET
C  THE 40/ACRE TALLEST THEN AVERAGE TO GET FM- TOPHEIGHT
C----------
      IF (ITRN .GT. 0) CALL RDPSRT(ITRN,HT,HPOINT,.TRUE.)
      SSUMN = 0.0
      FMAVH = 0.0
      DO I = 1,ITRN
        II = HPOINT(I)
        P = FMPROB(II)
        IF ((SSUMN + P) .GT. 40.0) P = 40. - SSUMN
        SSUMN = SSUMN + P
        FMAVH = FMAVH + (HT(II) * P)
        IF (SSUMN .GE. 40.0) GO TO 65
      ENDDO
   65 CONTINUE
      IF (SSUMN .GT. 0.0) FMAVH = FMAVH / SSUMN
C----------
C  UNDERSTORY HEIGHT IS HALF THE CURRENT TOP HEIGHT
C----------
      USHT = 0.5 * FMAVH
C----------
C  STEPS:
C
C  1 - FIND BASAL AREA IN EACH 'COVER TYPE' - BECAUSE OF SPECIES
C      POOLING, THIS CAN BE DIFFERENT FROM HIGHEST BA SPECIES
C      AT THE SAME TIME FIND THE >UNDERSTORY< BA FOR THE SAME
C      GROUPINGS; REQUIRED FOR A NUMBER OF COVER TYPE FUEL MODELS.
C----------
      DO I = 1,ASCT  ! ZERO OUT THE COVER SUPER-GROUPS
        CTBA(I) = 0.
        USBA(I) = 0.
      ENDDO
C
      CALL VARVER(VVER)
C----------
C  TETONS VARIANT
C----------
      IF (VVER(1:2) .EQ. 'TT') THEN
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          LUNDER = HT(I) .LE. USHT
C
          SELECT CASE (ISP(I))
C
            CASE (4,11,12)      ! PINYON JUNIPER
              CTBA(PJCT) = CTBA(PJCT) + X
              IF (LUNDER) USBA(PJCT) = USBA(PJCT) + X
C
            CASE (10)           ! PONDEROSA PINE
              CTBA(PPCT) = CTBA(PPCT) + X
              IF (LUNDER) USBA(PPCT) = USBA(PPCT) + X
C
            CASE (5,8,9)        ! SPRUCE FIR
              CTBA(SFCT) = CTBA(SFCT) + X
              IF (LUNDER) USBA(SFCT) = USBA(SFCT) + X
C
            CASE (7)            ! LODGEPOLE PINE
              CTBA(LPCT) = CTBA(LPCT) + X
              IF (LUNDER) USBA(LPCT) = USBA(LPCT) + X
C
            CASE (1,2,3,17)     ! MIXED CONIFER
              CTBA(MCCT) = CTBA(MCCT) + X
              IF (LUNDER) USBA(MCCT) = USBA(MCCT) + X
C
            CASE (6,13:16,18)   ! ASPEN, COTTONWOOD, OTHER HARDWOODS
              CTBA(ASCT) = CTBA(ASCT) + X
              IF (LUNDER) USBA(ASCT) = USBA(ASCT) + X
C
          END SELECT
        ENDDO
C----------
C  UTAH VARIANT
C----------
      ELSEIF (VVER(1:2) .EQ. 'UT') THEN
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          LUNDER = HT(I) .LE. USHT
C
          SELECT CASE (ISP(I))
C
            CASE (13)          ! OAK BRUSH
              CTBA(OBCT) = CTBA(OBCT) + X
              IF (LUNDER) USBA(OBCT) = USBA(OBCT) + X
C
            CASE (11,12,14:16) ! PINYON JUNIPER
              CTBA(PJCT) = CTBA(PJCT) + X
              IF (LUNDER) USBA(PJCT) = USBA(PJCT) + X
C
            CASE (10)          ! PONDEROSA PINE
              CTBA(PPCT) = CTBA(PPCT) + X
              IF (LUNDER) USBA(PPCT) = USBA(PPCT) + X
C
            CASE (5,8,9)       ! SPRUCE FIR
              CTBA(SFCT) = CTBA(SFCT) + X
              IF (LUNDER) USBA(SFCT) = USBA(SFCT) + X
C
            CASE (7)           ! LODGEPOLE PINE
              CTBA(LPCT) = CTBA(LPCT) + X
              IF (LUNDER) USBA(LPCT) = USBA(LPCT) + X
C
            CASE (1:4,17,23)   ! MIXED CONIFER
              CTBA(MCCT) = CTBA(MCCT) + X
              IF (LUNDER) USBA(MCCT) = USBA(MCCT) + X
C
            CASE (6,18:22,24)  ! ASPEN, COTTONWOOD, OTHER HARDWOODS
              CTBA(ASCT) = CTBA(ASCT) + X
              IF (LUNDER) USBA(ASCT) = USBA(ASCT) + X
C
          END SELECT
        ENDDO
C----------
C  CENTRAL ROCKIES (CR) VARIANT
C----------
      ELSE                       
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          LUNDER = HT(I) .LE. USHT
C
          SELECT CASE (ISP(I))
C
            CASE (23:27)         ! OAK BRUSH
              CTBA(OBCT) = CTBA(OBCT) + X
              IF (LUNDER) USBA(OBCT) = USBA(OBCT) + X
C
            CASE (12,16,29:35)   ! PINYON JUNIPER
              CTBA(PJCT) = CTBA(PJCT) + X
              IF (LUNDER) USBA(PJCT) = USBA(PJCT) + X
C
            CASE (13,36)         ! PONDEROSA PINE, CHIHUAHUA PINE
              CTBA(PPCT) = CTBA(PPCT) + X
              IF (LUNDER) USBA(PPCT) = USBA(PPCT) + X
C
            CASE (19)            ! WHITE SPRUCE
              CTBA(WSCT) = CTBA(WSCT) + X
              IF (LUNDER) USBA(WSCT) = USBA(WSCT) + X
C
            CASE (1,17,18)       ! SPRUCE FIR
              CTBA(SFCT) = CTBA(SFCT) + X
              IF (LUNDER) USBA(SFCT) = USBA(SFCT) + X
C
            CASE (11)            ! LODGEPOLE PINE
              CTBA(LPCT) = CTBA(LPCT) + X
              IF (LUNDER) USBA(LPCT) = USBA(LPCT) + X
C
            CASE (2:10,14,15,37) ! MIXED CONIFER
              CTBA(MCCT) = CTBA(MCCT) + X
              IF (LUNDER) USBA(MCCT) = USBA(MCCT) + X
C
            CASE (20:22,28,38)   ! ASPEN, COTTONWOOD, PAPER BIRCH,
              CTBA(ASCT) = CTBA(ASCT) + X
              IF (LUNDER) USBA(ASCT) = USBA(ASCT) + X
C
          END SELECT
        ENDDO
      ENDIF
C----------
C  2 - FIND COVER TYPE METAGROUP *ICT* - FIRST LOOK FOR >50% OF
C      BASAL AREA IN METAGROUP; FAILING THAT ASSIGN AS MIXED
C      CONIFER *MCCT* - IF THERE ARE NO TREES USED THE METAGROUP OF
C      THE PREVIOUS CYCLE, INITIALIZED TO *LPCT* IN **FMVINIT**
C----------
      STNDBA = 0.0
      DO I = 1,ASCT
        STNDBA = STNDBA + CTBA(I)
      ENDDO
      ICT = 0
      IF ((ITRN .GT. 0) .AND. (STNDBA.GT.0.001)) THEN
        DO I = 1,ASCT
          IF ((CTBA(I)/STNDBA) .GT. 0.50) THEN
            ICT = I
            GOTO 10
          ENDIF
        ENDDO
        ICT = MCCT
        GOTO 10
      ELSE
        ICT = OLDICT
      ENDIF
   10 OLDICT = ICT
C----------
C  3 - USING THE UNDERTSTORY COVER TYPE *USBA* SELECT THE
C      DOMINANT UNDERSTORY SPECIES GROUP *USCT*
C----------
      X = 0.0
      DO I = 1,ASCT  ! SUM FOR UNDERSTORY BA
        X = X + USBA(I)
      ENDDO
      USCT = 0
      Y = -1.        ! FIND MAX GROUP
      IF (X .GT. 0.0) THEN
        DO I = 1,ASCT
          IF (USBA(I) .GT. Y) THEN
            USCT = I
            Y = USBA(I)
          ENDIF
        ENDDO
      ELSE
        USCT = ICT
      ENDIF
C----------
C  SET FLAG FOR CONIFEROUS UNDERSTOREY PRESENT
C----------
      LCUNDR =  ((USBA(PPCT)+USBA(WSCT)+USBA(SFCT)+
     &           USBA(LPCT)+USBA(MCCT)) .GT. 1.)
C
      IF (DEBUG) WRITE (JOSTND,11) ICT,LCUNDR,CTBA,USBA
   11 FORMAT (' IN FMCFMD, ICT,LCUNDR=',I3,L2,' CTBA=',8F6.2/
     >        ' USBA=',8F6.2)
      TINY = 0.0
C----------
C  4 - DETERMINE THE STRUCTURE CLASS.  DIFFERENT PARAMETERS ARE USED
C      FOR THE CR VARIANT (VALIDATION WORKSHOP OCT. 04).
C
C  UTAH AND TETONS VARIANTS
C----------
      IF ((VVER(1:2) .EQ. 'UT') .OR. (VVER(1:2) .EQ. 'TT')) THEN
        CALL FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,IFMST,X3,
     &                FMPROB,FMICR)
C----------
C  CENTRAL ROCKIES VARIANT
C----------
      ELSE 
        y1 = 200
        y2 = 5
        y3 = 30
        IF (ICT .EQ. 6) THEN ! lodgepole
          y4 = 12
        ELSE
          y4 = 18
        ENDIF
        y5 = 5
        y6 = 20
        CALL FMSSTAGE(y1,y2,y3,y4,y5,y6,IFMST,X3,FMPROB,FMICR)
      ENDIF
C
      IF (DEBUG) WRITE (JOSTND,*) 'IFMST=',IFMST
C----------
C  5 - USING THE COVER TYPE METAGROUP *ICT* SELECT THE FUEL MODEL
C      SOME COVER TYPE MODELS CAN SOMETIMES LOOP BACK TO THIS POINT TO
C      USE ALTERNATIVE COVER-TYPE RULES
C----------
  111 CONTINUE
C
      SELECT CASE (ICT)
C----------
C  OAK BRUSH COVER TYPE
C----------
      CASE(OBCT)
        IF (FWIND .LE. 7.0) THEN
          EQWT(8) = 1.0
        ELSE
          IS = MAXSP
          IF (VVER(1:2) .EQ. 'UT') IS = 13
          IF ((VVER(1:2).NE.'UT') .AND. (VVER(1:2).NE.'TT')) IS = 23
          IF (CTBA(OBCT) .EQ. 0) THEN
            EQWT(5) = 1.0
          ELSE
C----------
C  AVERAGE HT OF OAK *X*
C----------
            X = 0.0
            PSUM = 0.0
C----------
C  UTAH VARIANT
C----------
            IF(VVER(1:2) .EQ. 'UT') THEN
              IS=13
              IF (ISCT(IS,1) .GT. 0) THEN
                DO J=ISCT(IS,1), ISCT(IS,2)
                  I = IND1(J)
                  X = X + HT(I) * FMPROB(I)
                  PSUM = PSUM + FMPROB(I)
                ENDDO
              ENDIF
C----------
C  CENTRAL ROCKIES VARIANT
C----------
            ELSEIF(VVER(1:2) .EQ. 'CR') THEN
              DO 59 J=1,MAXSP
              I1=ISCT(J,1)
              IF(I1 .EQ. 0) GO TO 59
              I2=ISCT(J,2)
              IF(J.GE.23 .AND. J.LE.27)THEN
                DO J2=I1,I2
                  I = IND1(J)
                  X = X + HT(I) * FMPROB(I)
                  PSUM = PSUM + FMPROB(I)
                ENDDO
              ENDIF
   59         CONTINUE
C----------
C  TETONS VARIANT -- DOESN'T HAVE OAK AS A SPECIES
C----------
            ELSE
              GO TO 60
            ENDIF
C
   60       CONTINUE
            IF (PSUM .GT. 1.0E-6) X = X / PSUM
C----------
C  DEAD BIOMASS: *Y*
C
C  LIVE BIOMASS INCLUDES CROWNS & BOLES
C  DEAD BIOMASS INCLUDES SNAGS & CWD
C----------
            BL = 0.0
            DO I = 1,ITRN
              BL = BL + (CROWNW(I,0) * FMPROB(I) * P2T)
              DO J = 1,5
                BL = BL + (CROWNW(I,J) + OLDCRW(I,J)) *
     &               P2T * FMPROB(I)
              ENDDO
              XNEG1 = -1.0
              LMERCH = .FALSE.
              CALL FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,VT,
     &            LMERCH,DEBUG,JOSTND)
              BL = BL + (FMPROB(I) * V2T(ISP(I)) * VT)
            ENDDO
            BD = 0.0
            DO I = 1,NSNAG
              SNVIS = 0.0
              SNVIH = 0.0
              IF (DENIH(I) .GT. 0.0) THEN
                CALL FMSVOL (I, HTIH(I), SNVIH, DEBUG, JOSTND)
                SNVIH = SNVIH * DENIH(I)
              ENDIF
              IF (DENIS(I) .GT. 0.0) THEN
                CALL FMSVOL (I, HTIS(I), SNVIS, DEBUG, JOSTND)
                SNVIS = SNVIS * DENIS(I)
              ENDIF
              BD = BD + (SNVIS + SNVIH) * V2T(SPS(I))
            ENDDO
            BD = BD + LARGE + SMALL
            Y = 0.
            IF ((BD+BL) .GT. 1.0E-6) Y = 100. * (BD/(BD+BL))
C----------
C  NOW THAT AVG HT AND % DEAD BIOMASS ARE KNOWN,
C  CHOOSE THE OAK COVER FIRE MODEL. IN THE 'ELSE'
C  CASE, MODEL 4/5 BLENDING IS DONE AND FMD=5
C----------
            IF (X .LE. 2.0 .OR. Y .LE. 50.0) THEN
              EQWT(5) = 1.0
            ELSEIF (X .GT. 6.0 .AND. Y .GT. 50.0) THEN
              EQWT(4) = 1.0
            ELSE
              XVEC(1) = 2.0
              XVEC(2) = 6.0
              YVEC(1) = 0.0
              YVEC(2) = 1.0
              EQWT(4) = ALGSLP(X,XVEC,YVEC,2)
              EQWT(5) = 1.0 - EQWT(4)
            ENDIF
          ENDIF
        ENDIF
C----------
C  PINYON JUNIPER COVER TYPE
C  NOTE *UNUSUAL* 'NO FIRE' OUTCOME IN UT DOCUMENT
C  AND ADDITION OF TINY WHEN CALLED FROM OTHER TYPES
C  FOR CR VARIANT, THIS WAS CHANGED AFTER VALIDATION MEETING (OCT 04)
C----------
      CASE(PJCT)
C----------
C  CENTRAL ROCKIES VARIANT
C----------
        IF ((VVER(1:2).NE.'TT') .AND. (VVER(1:2).NE.'UT')) THEN
          IF (PERCOV .LE. 25) THEN
            EQWT(2) = 1.0
          ELSEIF (PERCOV .LE. 35) THEN
            EQWT(2) = 1 - (PERCOV - 25)/10
            EQWT(5) = 1 - (35 - PERCOV)/10
          ELSEIF (PERCOV .LE. 45) THEN
            EQWT(5) = 1.0
          ELSEIF (PERCOV .LE. 55) THEN
            EQWT(5) = 1 - (PERCOV - 45)/10
            EQWT(6) = 1 - (55 - PERCOV)/10
          ELSE
            EQWT(6) = 1.0
          ENDIF
C----------
C  UTAH AND TETONS VARIANTS
C----------
        ELSE
          IF ((SMALL+LARGE+TINY) .GT. 0.0 .AND.
     &       FWIND .GT. 7.0 .AND. PERCOV .GT. 20.0) THEN
            IF (LDRY) THEN
              EQWT(6) = 1.0
            ELSE
              EQWT(5) = 1.0
            ENDIF
          ELSE
            EQWT(8) = 1.0
          ENDIF
        ENDIF
C----------
C  PONDEROSA PINE COVER TYPE
C----------
      CASE(PPCT)
C----------
C  DEFINE UNDERSTORY CHARACTERISTICS
C  DEFINE UNDERSTORY AS TREES/SNAGS WITH HEIGHT
C  < HALF OF THE STAND TOP HEIGHT.
C
C  LIVE UNDERSTORY BIOMASS: CROWNS & BOLES
C----------
        BL = 0.0
        DO I = 1,ITRN
          IF (HT(I) .LE. USHT) THEN
            BL = BL + (CROWNW(I,0) * FMPROB(I) * P2T)
            DO J = 1,5
              BL = BL + (CROWNW(I,J) + OLDCRW(I,J)) *
     &             P2T * FMPROB(I)
            ENDDO
            XNEG1  = -1.0
            LMERCH = .FALSE.
            CALL FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,VT,
     &           LMERCH,DEBUG,JOSTND)
            BL = BL + (FMPROB(I) * V2T(ISP(I)) * VT)
          ENDIF
        ENDDO
C----------
C  DEAD UNDERSTORY BIOMASS: SNAGS ONLY
C----------
        BD = 0.0
        DO I = 1,NSNAG
          SNVIS = 0.0
          SNVIH = 0.0
          IF (DENIH(I) .GT. 0.0 .AND. HTIH(I) .LE. USHT) THEN
            CALL FMSVOL (I, HTIH(I), SNVIH, DEBUG, JOSTND)
            SNVIH = SNVIH * DENIH(I)
          ENDIF
          IF (DENIS(I) .GT. 0.0 .AND. HTIS(I) .LE. USHT) THEN
            CALL FMSVOL (I, HTIS(I), SNVIS, DEBUG, JOSTND)
            SNVIS = SNVIS * DENIS(I)
          ENDIF
          BD = BD + (SNVIS + SNVIH) * V2T(SPS(I))
        ENDDO
C----------
C  Y IS PERCENT OF TOTAL BIOMASS THAT IS DEAD
C----------
        Y = 0.0
        IF ((BD+BL) .GT. 1.0E-6)  Y = 100. * (BD/(BD+BL))
C----------
C  NOW CHOOSE FUEL MODEL
C----------
        IF (PERCOV .LE. 60.0) THEN
C----------
C  LIVE+DEAD UNDERSTORY PRESENT
C----------
          IF ((BD+BL) .GT. 0.0) THEN
            IF (FWIND .GT. 7.0) THEN
              IF (Y .LE. 50.0) THEN
C----------
C  IF OA OR PJ, LOOP BACK AND USE OA OR PJ
C  (SURFACE FUELS) RULES
C----------
                IF (USCT .EQ. OBCT .OR. USCT .EQ. PJCT) THEN
                  ICT = USCT
                  IF (SMALL .LE. 0.0) TINY = 1.0E-3
                  GOTO 111
                ELSE
                  EQWT(5) = 1.0
                ENDIF
              ELSE
                EQWT(6) = 1.0
              ENDIF
            ELSE
              IF ((VVER(1:2) .EQ. 'UT') .OR.
     &            (VVER(1:2) .EQ. 'TT')) THEN
                 EQWT(8) = 1.0
               ELSE
                EQWT(5) = 1.0
              ENDIF
            ENDIF
          ELSE
            EQWT(2) = 1.0
          ENDIF
C
        ELSE
C----------
C  PERCOV > 60
C----------
          IF (FWIND .GT. 7.0) THEN
            IF ((CTBA(PJCT)/MAX(1.0E-3,STNDBA)) .GT. 0.2) THEN
              IF (LDRY) THEN
                EQWT(6) = 1.0
              ELSE
                EQWT(5) = 1.0
              ENDIF
            ELSE IF (IFMST .EQ. 6) THEN
              IF (LCUNDR) THEN
                EQWT(5) = 1.0
              ELSE
                EQWT(2) = 1.0
              ENDIF
            ELSE
              EQWT(9) = 1.0
            ENDIF
          ELSE
            EQWT(9) = 1.0
          ENDIF
        ENDIF
C----------
C  WHITE SPRUCE COVER TYPE
C  ONLY OCCURS IN THE CENTRAL ROCKIES VARIANT
C----------
      CASE(WSCT)
        IF (PERCOV .LE. 40.0) THEN
          EQWT(2) = 1.0
        ELSE
C----------
C  FIND AVERAGE DIAMETER
C----------
          X = 0.0
          PSUM = 0.0
          DO I = 1,ITRN
            X = X + (FMPROB(I) * DBH(I))
            PSUM = PSUM + FMPROB(I)
          ENDDO
          IF (PSUM .GT. 1.0E-6) X = X / PSUM
          IF (IFMST .GE. 4 .AND. X .GT. 12.0) THEN  ! ge 3
            EQWT(10) = 1.0
          ELSE
            EQWT(8) = 1.0
          ENDIF
        ENDIF
C----------
C  SPRUCE FIR COVER TYPE
C  THIS WAS CHANGED FOR CR VARIANT AFTER VALIDATION WORKSHOP (OCT 04)
C----------
      CASE(SFCT)
C----------
C  CENTRAL ROCKIES VARIANT
C----------
        IF ((VVER(1:2).NE.'TT') .AND. (VVER(1:2).NE.'UT')) THEN
C
          SELECT CASE (IFMST)
C
          CASE(0)
            EQWT(2) = 1.0
C
          CASE(1)
            ! CALCULATE QMD
            X = 0.0
            PSUM = 0.0
            DO I = 1,ITRN
              X = X + (FMPROB(I) * DBH(I)*DBH(I))
              PSUM = PSUM + FMPROB(I)
            ENDDO
            IF (PSUM .GT. 1.0E-6) X = SQRT(X / PSUM)
            IF (X .GT. 1)  THEN
              EQWT(5) = 1.0
            ELSE
              EQWT(2) = 1.0
            ENDIF
C
          CASE(2)
            EQWT(8) = 1.0
C
          CASE(3:6)
            EQWT(10) = 1.0
C
          END SELECT
C----------
C  UTAH AND TETONS VARIANTS
C----------
        ELSE
          IF (IFMST .LE. 2) THEN
            EQWT(2) = 1.0
          ELSE
            EQWT(8) = 1.0
          ENDIF
        ENDIF
C----------
C  LODGEPOLE PINE COVER TYPE
C  THIS WAS CHANGED FOR CR VARIANT AFTER VALIDATION WORKSHOP (OCT 04)
C----------
      CASE(LPCT)
C----------
C  CENTRAL ROCKIES VARIANT
C----------
        IF ((VVER(1:2) .NE. 'TT') .AND. (VVER(1:2) .NE. 'UT')) THEN
C
          SELECT CASE (IFMST)
C
          CASE(0,1,5)
            EQWT(5) = 1.0
C
          CASE(2)
            EQWT(8) = 1.0
C
          CASE(3,4,6)
            EQWT(10) = 1.0
C
          END SELECT
C----------
C  UTAH AND TETONS VARIANTS
C----------
        ELSE
C----------
C  COMPUTE STAND TPA
C----------
          X = 0.0
          DO I = 1,ITRN
            X = X + FMPROB(I)
          ENDDO
C----------
C  LP SITE INDEX
C----------
          SIFM = INT(SITEAR(7))
          SIBRK = 20
C
          IF (SIFM .LT. SIBRK) THEN
            EQWT(5) = 1.0
          ELSEIF (X .GT. 1000.0) THEN
            IF (FMAVH .LE. 10.0) THEN
              EQWT(5) = 1.0
            ELSE
              IF (FWIND .GT. 7.0) THEN
                EQWT(5) = 1.0
              ELSE
                EQWT(8) = 1.0
              ENDIF
            ENDIF
          ELSEIF (IFMST .LE. 2) THEN
            EQWT(2) = 1.0
          ELSE
            EQWT(8) = 1.0
          ENDIF
        ENDIF
C----------
C  MIXED CONIFER COVER TYPE
C  THIS WAS CHANGED FOR CR VARIANT AFTER VALIDATION WORKSHOP (OCT 04)
C----------
      CASE(MCCT)
C----------
C  DETERMINE IF PP IS HIGHEST RANK SPECIES WRT BA.
C----------
        IF (VVER(1:2) .EQ. 'TT') THEN
          LPPDOM = .TRUE.
          J = 10
          DO I = 1, MAXSP
            IF ((I .NE. J) .AND. (FMTBA(I) .GT. FMTBA(J))) THEN
              LPPDOM = .FALSE.
            ENDIF
          ENDDO
        ELSE
          LPPDOM = .TRUE.
          IF (VVER(1:2) .EQ. 'UT') THEN
            J = 10
          ELSE
            J = 13
          ENDIF
          DO I = 1, MAXSP
            IF ((I .NE. J) .AND. (FMTBA(I) .GT. FMTBA(J))) THEN
              LPPDOM = .FALSE.
            ENDIF
          ENDDO
        ENDIF
C----------
C  CENTRAL ROCKIES VARIANT
C----------
        IF ((VVER(1:2).NE.'TT') .AND. (VVER(1:2).NE.'UT')) THEN
          IF (LPPDOM) THEN
            EQWT(9) = 1.0
          ELSE
C
            SELECT CASE (IFMST)
C
            CASE(0)
              IF (LDRY) THEN
                EQWT(1) = 1.0
              ELSE
                EQWT(2) = 1.0
              ENDIF
C
            CASE(1)
              IF (LDRY) THEN
                EQWT(6) = 1.0
              ELSE
                EQWT(5) = 1.0
              ENDIF
C
            CASE(2)
              IF (LDRY) THEN
                EQWT(6) = 1.0
              ELSE
                EQWT(8) = 1.0
              ENDIF
C
            CASE(3,4,6)
              EQWT(10) = 1.0
C
            CASE(5)
              IF (PERCOV .GE. 55) THEN
                EQWT(8) = 1.0
              ELSEIF (PERCOV .LT. 45) THEN
                EQWT(2) = 1.0
              ELSE
                EQWT(2) = 1 - (PERCOV - 45)/10
                EQWT(8) = 1 - (55 - PERCOV)/10
              ENDIF
C
            END SELECT
          ENDIF
C----------
C  UTAH AND TETONS VARIANTS
C----------
        ELSE 
C----------
C  IS WHITE FIR PRESENT? ONLY PERTAINS TO UT VARIANT;
C  WHITE FIR IS NOT A VALID SPECIES IN TT VARIANT
C----------
          LWFOK = .FALSE.
          PSUM = 0.0
          IF (VVER(1:2).EQ.'UT' .AND. ISCT(4,1).GT.0) THEN
            DO J=ISCT(4,1), ISCT(4,2)
              PSUM = PSUM + FMPROB(IND1(J))
            ENDDO
          ENDIF
          IF (PSUM .GT. 0.0) LWFOK = .TRUE.
C----------
C  STAND TPA
C----------
          X = 0.0
          DO I = 1,ITRN
            X = X + FMPROB(I)
          ENDDO
C
          IF (PERCOV .LE. 50.0) THEN
C
            IF (X .GT. 1000.0) THEN
              IF (LDRY) THEN
                EQWT(2) = 1.0
              ELSE
                IF (FMAVH .LE. 10.0) THEN
                  EQWT(5) = 1.0
                ELSE
                  EQWT(8) = 1.0
                ENDIF
              ENDIF
            ELSE  ! < 1000
              IF ((VVER(1:2) .EQ. 'UT') .AND.
     &           (CTBA(PJCT) .GT. 0.0) .AND.
     &           ((CTBA(SFCT) .GT. 0.0) .OR. LWFOK)) THEN
                IF (LDRY) THEN
                  EQWT(6) = 1.0
                ELSE
                  EQWT(5) = 1.0
                ENDIF
              ELSE
                IF (LDRY) THEN
                  EQWT(2) = 1.0
                ELSE
                  EQWT(8) = 1.0
                ENDIF
              ENDIF
            ENDIF ! end of > 1000
C
          ELSE ! PERCOV > 50%
C
            IF (LPPDOM) THEN
              EQWT(9) = 1.0
            ELSE
              EQWT(8) = 1.0
            ENDIF
          ENDIF
C
        ENDIF
C----------
C  ASPEN COVER TYPE
C----------
      CASE(ASCT)
        IF ((CTBA(ASCT)/MAX(1.0E-3,STNDBA)) .GT. 0.80) THEN
          IF ((VVER(1:2) .NE. 'UT') .AND.
     &        (VVER(1:2) .NE. 'TT') .AND. (IMODTY .EQ. 1)) THEN
            EQWT(2) = 1.0
          ELSE
            EQWT(5) = 1.0
          ENDIF
C
        ELSEIF (LCUNDR) THEN
          J = 0
          IF (VVER(1:2) .EQ. 'UT') THEN
            DO I = 1,ITRN
C
              SELECT CASE (ISP(I))
C
                CASE(6,11:16,18:22,24)
                  J = J
C
                CASE DEFAULT
                  IF (HT(I) .GT. 10.0) THEN
                    J = J + 1
                    XW=CRWDTH(I)
                    XWID(J) = XW*XW*FMPROB(I)*0.785398
                  ENDIF
C
              END SELECT
            ENDDO
C----------
C  TETONS VARIANT
C----------
          ELSE IF (VVER(1:2) .EQ. 'TT') THEN
            DO I = 1,ITRN
C
              SELECT CASE (ISP(I))
C
                CASE(4,6,11:16,18)
                  J = J
C
                CASE DEFAULT
                  IF (HT(I) .GT. 10.0) THEN
                    J = J + 1
                    XW=CRWDTH(I)
                    XWID(J) = XW*XW*FMPROB(I)*0.785398
                  ENDIF
C
              END SELECT
            ENDDO
C----------
C  CENTRAL ROCKIES VARIANT
C----------
          ELSE
            DO I = 1,ITRN
              SELECT CASE (ISP(I))
                CASE(12,16,20:35,38)
                  J = J
                CASE DEFAULT
                  IF (HT(I) .GT. 10.0) THEN
                    J = J + 1
                    XW=CRWDTH(I)
                    XWID(J) = XW*XW*FMPROB(I)*0.785398
                  ENDIF
              END SELECT
            ENDDO
          ENDIF
          CALL COVOLP (DEBUG,JOSTND,J,IDUM,XWID,CVR10,CCCOEF)
C
          IF (CVR10 .GT. 40.0) THEN
            EQWT(8) = 1.0
          ELSE
            EQWT(2) = 1.0
          ENDIF
C
        ELSEIF ((CTBA(PPCT) + CTBA(SFCT) + CTBA(LPCT)
     &        + CTBA(MCCT) + CTBA(WSCT)) .GT. 1.0) THEN
          ICT = MCCT
          GOTO 111
        ELSEIF (CTBA(OBCT) .GT. 1.0) THEN
          ICT = OBCT
          GOTO 111
        ELSEIF (CTBA(PJCT) .GT. 1.0) THEN
          ICT = PJCT
          IF (SMALL .LE. 0.0) TINY = 1.0E-3
          GOTO 111
        ENDIF
C
      END SELECT
C----------
C  END OF DETAILED LOW FUEL MODEL SELECTION
C
C  DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C  ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
C  CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
C  RELATIONSHIP DECLINES FROM PURE 11 INITIALLY, TO PURE 10 AFTER
C  THE PERIOD EXPIRES.
C----------
      AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
      IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
        LATFUEL = .TRUE.
        EQWT(11)  = AFWT
        IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
      ENDIF
      IF (.NOT. LATFUEL) AFWT = 0.0
C----------
C  MODELS 10,12 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
C  OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
C  NO MODEL 13/14 IN UT
C----------
      EQWT(10) = 1.0 - AFWT
      EQWT(12) = 1.0
C----------
C  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)
C----------
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)
C
      IF (DEBUG) WRITE (JOSTND,8) FMD
    8 FORMAT (' FMCFMD, FMD=',I4)
C
      RETURN
      END
