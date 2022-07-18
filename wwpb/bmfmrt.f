      SUBROUTINE BMFMRT (ISTD, IYR, SH, FM)
C----------
C WWPB $Id$
C----------
*     CALLED FROM: BMFIRE
***********************************************************************
*  **BMFMRT  DATE OF LAST REVISION:  June 14, 1994
*----------------------------------------------------------------------
*  PURPOSE:
*     THIS SUBROUTINE CALCULATES THE PROBABILITY OF TREE MORTALITY.
*     FROM INFORMATION SENT FROM E. WEINHARDT
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     SH:   SCORCH HEIGHT (IN FEET)
*
*  LOCAL VARIABLE DEFINITIONS:
*     ISIZ:   LOOP COUNTER OVER DBH SIZE CLASSES 
*     ITYP:   LOOP COUNTER OVER HOST/NON-HOST TYPES
*     DBH:    MEAN DBH OF A SIZE CLASS
*     PMORT:  PROBABILITY (PROPORTION) OF MORTALITY
*
*  COMMON BLOCK VARIABLES AND PARAMETERS: 
*     ISPH:   FROM BMFCOM; MAIN SPECIES IN HOST AND NON-HOST CLASSES
*     NSCL:   FROM BMPRM; NUMBER OF DBH SIZE CLASSES
*     SCORCH: FROM BMCOM; PROPORTION OF TREES IN SIZE CLASS
*             SEVERELY SCORCHED BY FIRE
*
***********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMFCOM.F77'

C.... VARIABLE DECLARATIONS.
      
      REAL    CSLP
      INTEGER ISIZ, ITYP
      INTEGER J, JJ
      REAL    DBH(NSCL)
      REAL    PMORT(NSCL, 2)
      REAL    SH


C.... CHECK FOR DEBUG.

      IF(LBMDEB) WRITE(JBMBPR,10) ISTD,IYR
   10 FORMAT(' Begin BMFMRT: Stand = ', I8,' Year = ',I5)

C.... BEGIN ROUTINE  

      DBH(1) = UPSIZ(1) / 2.0
      DO 15 ISIZ=2,NSCL
         DBH(ISIZ)= UPSIZ(ISIZ-1) + (UPSIZ(ISIZ) - UPSIZ(ISIZ-1)) / 2.0
   15 CONTINUE
      
C.... 1=DORMANT SEASON,  -1 FOR GROWING SEASON

      SEAS = -1
      
C.... 1 = HOST, 2 = NONHOST

      DO 100 ITYP=1,2

C....    1 = SMALLEST, NSCL=LARGEST

         DO 110 ISIZ=1,NSCL
      
            IF (TREE(ISTD,ISIZ, ITYP) .GT. 0.0) THEN
      
C....          COMPUTE SCORCH LENGTH SL
      
C              CROWN LENGTH

               CRL = HTS(ISTD,ISIZ,ITYP) * (CRS(ISTD,ISIZ,ITYP) / 100.0)

C              SCORCH LENGTH

               SL = SH - (HTS(ISTD,ISIZ,ITYP) - CRL)

               IF (SL .LT. 0.0) SL = 0.0
               IF (SL .GT. CRL) SL = CRL

C              PERCENT OF CROWN LENGTH SCORCHED

               CSLP = 100.0 * (SL / CRL)
      
               IF (DBH(ISIZ) .GT. 1.0) THEN
      
C                 IF HOST TYPE IS PP

c                  IF ((ITYP .EQ. 1).AND.(HSPEC(PBSPEC,10) .EQ. 1)) THEN
                  IF (ISPH(ISTD,ITYP) .EQ. 10) THEN

                     IF (CSLP .LT. 90.0) THEN
                        ILS = 1
                        IHS = 0
                     ELSEIF (CSLP .LT. 100.0) THEN
                        ILS = 0
                        IHS = 1
                     ELSEIF (CSLP .EQ. 100.0) THEN
                        ILS = -1
                        IHS = -1
                     ENDIF
      
                     IF (DBH(ISIZ) .LE. 4.5) THEN
                        D = 3
                     ELSEIF (DBH(ISIZ) .LE. 7.5) THEN
                        D = 6
                     ELSEIF (DBH(ISIZ) .LE. 10.5) THEN
                        D = 9
                     ELSE
                        D = 12
                     ENDIF
      
                     PMORT(ISIZ,ITYP) = 1.0 / (1 + EXP(-1.16 + 1.04
     &                * SEAS + 1.94 * ILS - .12 * IHS + .14 * 2.54 * D))
      
                  ELSE
C....                ALL OTHER SPECIES
      
                     CSLP = 100. * (SL * (2.0 * CRL - SL) / (CRL * CRL))
      
C....                CALCULATE BARK THICKNESS

                     IF (ISPH(ISTD,ITYP) .EQ. 1) THEN
c                        white pine
                         BTH = .054 + .025 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 2) THEN
c                        larch
                         BTH = -0.045 + 0.0629 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 3) THEN
c                        douglas fir
                         BTH = 0.065 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 4) THEN
c                        grand fir
                         BTH = 0.043 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 5) THEN
c                        western hemlock
                         BTH = 0.022 + 0.043 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 6) THEN
c                        cedar
                         BTH = -0.152 + 0.021 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 7) THEN
c                        lodgepole pine
                         BTH = 0.0271 + 0.0143 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 8) THEN
c                        spruce
                         BTH = 0.126 + 0.025 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 9) THEN
c                        sub-alpine fir
                         BTH = 0.015 * DBH(ISIZ)
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 10) THEN
c                        ponderosa pine
                         BTH = -0.0376 + 0.0584 * DBH(ISIZ)
                     ELSE
c                        other (WPB)
                         BTH = 0.0271 + 0.022 * DBH(ISIZ)
                     ENDIF
                        
            
                     X = EXP(-1.941 + 6.316 * (1 - EXP(-BTH))
     &                     - .000535 * CSLP * CSLP)
                     PMORT(ISIZ, ITYP) = 1 / (1 + X)
      
                  ENDIF
      
               ELSE 
C                 DBH < 1
      
                  IF ((CSLP .GT. 50) .OR. ((FM .NE. 1)
     &               .AND. (FM .NE. 8) .AND. (FM .NE. 9))
     &               .OR. (HTS(ISTD,ISIZ,ITYP) .LT. 3)) THEN
                     PMORT(ISIZ, ITYP) = 1.0 
                     
                  ELSE
                     IF (ISPH(ISTD,ITYP) .EQ. 10) THEN

C....                   ASSUME DBH = 1 AND CSLP < 50

                        P1 = 1.0 / (1.0 + EXP(-1.16 + 1.04 * SEAS
     &                        + 1.94 + .14 * 2.54 * 3.0))
                     ELSE

C....                   CALCULATE BARK THICKNESS WITH DBH=1
                     IF (ISPH(ISTD,ITYP) .EQ. 1) THEN
c                        white pine
                         BTH = .054 + .025
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 2) THEN
c                        larch
                         BTH = -0.045 + 0.0629
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 3) THEN
c                        douglas fir
                         BTH = 0.065 
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 4) THEN
c                        grand fir
                         BTH = 0.043 
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 5) THEN
c                        western hemlock
                         BTH = 0.022 + 0.043
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 6) THEN
c                        cedar
                         BTH = -0.152 + 0.021
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 7) THEN
c                        lodgepole pine
                         BTH = 0.0271 + 0.0143
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 8) THEN
c                        spruce
                         BTH = 0.126 + 0.025
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 9) THEN
c                        sub-alpine fir
                         BTH = 0.015
                     ELSEIF (ISPH(ISTD,ITYP) .EQ. 10) THEN
c                        ponderosa pine
                         BTH = -0.0376 + 0.0584
                     ELSE
c                        other (WPB)
                         BTH = 0.0271 + 0.022 
                     ENDIF


                        P1 = 1.0 / (1.0 + EXP(-1.941 + 6.316 *
     &                    (1.0 - EXP(-BTH)) - .000535 * CSLP * CSLP))
                     ENDIF
      
C                ***NEED TO GET TRUE EXPRESSION HERE. 
C                    H1 IS HT OF TREE W DBH=1

                     H1 = 6.0
           
                     PMORT(ISIZ, ITYP) = P1 + (1.0 - P1)
     &                      * (1.0 - (HTS(ISTD,ISIZ,ITYP) - 3.0) 
     &                      / (H1 - 3.0)) 
                     IF (PMORT(ISIZ,ITYP).LT.0.0) PMORT(ISIZ,ITYP) = 0.0
                  ENDIF
      
               ENDIF
       
            ENDIF
      
  110    CONTINUE
  100 CONTINUE
      
      
C.... ---CALCULATE MORTALITY: PMORT PROP OF TREES DIE, REST MAY BE SEVERELY
C     ---SCORCHED (IF CLSP IS HIGH ENOUGH) AND BECOME "SPECIAL" TREES.
      
C     I ASSUME THAT ALL BURNED TREES REMAIN STANDING INITIALLY
      
      DO 200 ITYP=1,2
         DO 210 ISIZ=1,NSCL
            OAKILL(ISTD,ISIZ,ITYP) = OAKILL(ISTD,ISIZ,ITYP)
     &                                 + PMORT(ISIZ,ITYP)
           
C....       SOME BEETLE-KILLED TREES WILL BE BURNED BY FIRE SO BKP CANNOT
C             EMERGE.  THESE TREES ARE REMOVED FROM PBKILL AND ALLKLL ARRAYS
c             (note: pbkill (and allkll?) are in tpa here)

            IF (ITYP .EQ. 1) THEN
              PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) * (1-PMORT(ISIZ,1))
              ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * (1-PMORT(ISIZ,1))
            ENDIF
           
C           CREATE STANDING DEAD WOOD (WHY NOT DOWNED WOOD TOO?)
            J = L2D(ISIZ)                                   
            IF (ITYP .EQ. 1 .AND. J .GT. 0) 
     &          DWPHOS(ISTD,1,J) = DWPHOS(ISTD,1,J) + 
     &                    PMORT(ISIZ,ITYP) * TREE(ISTD,ISIZ,ITYP)
            J = J + 1
            JJ = IQPTYP(ISTD,ITYP)
            SDWP(ISTD,JJ,J,1) = SDWP(ISTD,JJ,J,1) + PMORT(ISIZ,ITYP) 
     &                    * TREE(ISTD,ISIZ,ITYP) * TVOL(ISTD,ISIZ,ITYP)
            
C....       IF MORE THAN 1/4 CROWN SCORCHED AND IS HOST THEN SCORCH
C           IS A PROPORTION
      
            IF ((ITYP .EQ. 1) .AND. (CSLP .GT. 25.0)) 
     &                SCORCH(ISTD,ISIZ) = 1.0 - PMORT(ISIZ, ITYP)

      
  210    CONTINUE
  200 CONTINUE

      IF(LBMDEB) WRITE(JBMBPR,99) ISTD,IYR
   99 FORMAT(' End BMFMRT: Stand = ', I8,' Year = ',I5)

      RETURN
      END
