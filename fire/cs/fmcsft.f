      SUBROUTINE FMCSFT(IFFEFT)
      IMPLICIT NONE
C----------
C FIRE-CS $Id$
C----------
C  SINGLE-STAND VERSION
C  CALLED FROM: FMCBA, FMCFMD
C  PURPOSE:
C     THIS SUBROUTINES CALCULATES A CATEGORICAL FOREST TYPE BASED ON
C     SN WORKSHOP INPUT.  THIS FOREST TYPE IS USED IN SETTING DEFAULT
C     SURFACE FUEL LEVELS AND IN THE FUEL MODEL LOGIC
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
      INTEGER IFFEFT, I
      REAL    PINEBA, NPINEBA, X
C----------
C  ROUTINE BEGINS.
C  DETERMINE FFE FOREST TYPE (1 OF 9 CATEGORIES) FROM FIA FOR. TYPE
C----------     
      IFFEFT = 0
      SELECT CASE (IFORTP)
      CASE (997, 504, 505, 510, 512, 515, 519, 520)
        IFFEFT = 1 ! hardwood           
      CASE (103, 104, 141, 142, 161:168, 996,401, 403:407, 409)
        PINEBA = 0
        NPINEBA = 0
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          SELECT CASE (ISP(I))
            CASE (3:7)  ! pine
              PINEBA = PINEBA + X
            CASE DEFAULT        ! non-pine
              NPINEBA = NPINEBA + X
          END SELECT
        ENDDO      
        IF ((PINEBA+NPINEBA) .GT. 0) THEN
          IF ((PINEBA/(PINEBA+NPINEBA)) .LE. 0.50) THEN
            IFFEFT = 2 ! hardwood/pine            
          ELSEIF ((PINEBA/(PINEBA+NPINEBA)) .LE. 0.70) THEN
            IFFEFT = 3 ! pine/hardwood
          ELSE
            IFFEFT = 4 ! pine
          ENDIF
        ENDIF  
        IF (IFORTP .EQ. 162) THEN
          IF ((ATAVH .GT. 50) .AND. (ISTCL .GE. 3)) THEN
            IFFEFT = 5 ! pine bluestem
          ENDIF
        ENDIF
      CASE (501, 503)
        IF ((ATAVH .GT. 30) .AND. (ISTCL .GE. 3)) THEN
          IFFEFT = 6 ! oak savannah
        ELSE
          IFFEFT = 1 ! hardwood
        ENDIF          
      CASE (181, 402)
        IFFEFT = 7 ! eastern redcedar       
      CASE (602, 605, 701, 706, 708, 807)
        IFFEFT = 8 ! saint francis types       
      CASE (999)
        IFFEFT = 9 ! nonstocked        
      CASE DEFAULT
        IFFEFT = 1 ! hardwood
      END SELECT
C      
      RETURN
      END
