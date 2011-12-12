      SUBROUTINE BMATCT(IYR)

C     CALLED FROM BMDRV
C     CALLS:  SPLAAR
C             SPLADS
C             SPLALO
C             GPGET2
C      
***********************************************************************
*  **BMATCT  Date of last revision:  09/28/05
*----------------------------------------------------------------------
*  Purpose:
*
*     This routine moves BKP between stands, and between each stand
*     and the Outside world. Each stand and the Outside is given a 
*     "score" based on how it looks from the source stand.
*     Then all the scores are scaled and the appropriate proportion of
*     BKP is moved from away from the source stand into the target.
*     This is done for both the primary pest and for Ips.
*     In cases where Ips is the primary pest, only one set
*     of calculations are done. The numerator of the score equation
*     is calculated in SUBROUTINE BMCNUM
*----------------------------------------------------------------------
*
*  Call list definitions:
*     IYR:    Current year.
*
*  Local variable definitions:
*     ASPEC:  Variable used to scale attractiveness of "special" trees
*     ATTOJ:  ATTractiveness of the Outside for each beetle type in the
*             current stand 
*     ATTWP:  average ATTractiveness of the World (area within RMAX) of
*             each point in the Outside
*     BAHO:   Basal Area of Host per acre in the Outside
*     BAO:    total Basal Area per acre in the Outside
*     BASP0:  Basal Area of Special trees per acre in the Outside
*     BKPO:   the amount of BKP produced per acre in the Outside this year,
*             for each beetle type
*     CDIST:  half the length of a square with area=CAREA
*     DDWT:   the variable known as 'c' in the dispersal equations
*     IPASS:  Loop counter for current "pass" over stands
*     KO:     K value (amount of nice host per acre) of the Outside, for
*             each beetle type
*     KAV:    AVerage K value (amount of nice host per acre) of the area
*             within RMAX of each point in the Outside
*     KL:     the area-weighted average K value (amount of nice host per
*             acre)in the Landscape
*     KLS:    the area-weighted average K value for the Stockable parts of
*             the Landscape
*     LOK:    Logical indicating if OW values are to change this year.  
*     NEWBKP: Array containing the new value for bkp for each stand and
*             beetle type
*     NPASS:  Number of "passes" to make over stands (will be 2 if both
*             MPB/WPB and Ips are simulated)
*     NZERO:  Scalar used to reduce attractiveness of stands
*     PROP:   Proportional attractivenss rating of each stand relative
*             to all other stands
*     RADO:   RADius of Outside world (with the landscape at its center)
*     RATTJO: Relative Attractiveness of current stand for outside beetles
*             of the current type.
*     RVO:    host Rating Value in the Outside (all effects combined)
*     RVOND:  host Rating Value Outside due to everything except Not Drought
*     SCORE:  Array containing the score for each stand's attractiveness
*             for each beetle type
*     SPO:    number of SPecial trees per acre in the Outside
*     TDIST:  half the length of a square with area=TAREA
*     TIPSDV: Total BKP for stand if Ips is a driving variable
*     TOTBKP: Accumulates total BKP across stands
*     TOTSC:  Array containing the total stand attractiveness for each 
*             beetle type 
*
*  Common block variables and parameters:
*     BKP:    From BMCOM; beetle kill pressure
*     BKPIps: From BMCOM; Ips beetle kill pressure
*     CBAHO:  'Constant' value of Basal Area per acre of Host in the Outside
*     CBAO:   'Constant' value of total Basal Area per acre in the Outside
*     CBASPO: 'Constant' value of Basal Area per acre of SPecial trees Outside
*     CBKPO:  'Constant' value of BKP produced per stockable acre in the Outside
*     CRVOND: 'Constant' value of host Rating Value Outside except Not Drought
*     CSPO:   'Constant' value of Basal Area per acre of Host in the Outside
*     IPSON:  From BMCOM; logical operator for Ips as a driving variable
*     NUMER:  (2,BMSTND) Numerator of scoring equation for # special trees
*              First element is for MBP/WPB; second is for Ips.
*     PBSPEC: From BMCOM; switch identifying which pine beetle species
*             are being modeled
*     RVOD:   host Rating Value Outside due to Drought effects
*     STOCK:  logical variable to indicate whether each stand is STOCKable   
*     STOCKO: proportion of the Outside that is STOCKable
*     TFOOD:  From BMCOM; total beetle attractive 'food'
*     ATTC:   the 'ATTractiveness Constant' of each stand (eqn.9)
*     ATTBYK: ATTractiveness of each stand to itself, divided by its K value
*     AREAL:  total area of the Landscape, in sq.mi.
*     AREALP: approximate area of the Landscape that is within RMAX of the
*             Outside (P for 'prime'), in sq.mi.
*     AREALS: total Stockable area of the Landscape, in sq.mi.
*     AREAO:  total area of the Outside, in sq.mi.                       
*     RADL:   RADius of a circle just large enough to enclose the Landscape
*     RADLC:  RADius that the Landscape would have if it was circular
*     SDD:    From BMCOM: 
*      
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77' 
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'
      
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'
      
C.... Call Statement Variable Declarations:
      INTEGER   IYR      

C Convert acres to square miles and meters to miles: All units are
C now in miles. (This is not true of all routines.) 
      
      REAL    AC2SQM, M2MI, MI2M
      
      PARAMETER (AC2SQM= 1./640.)
      PARAMETER (M2MI= 1./1609.34)
      PARAMETER (MI2M= 1609.34)
      
C.... Variable declarations.

      INTEGER IPASS, ISIZ
      INTEGER IRC, DUM(1)
      INTEGER TARG, CURR 
      
      LOGICAL LOK
      REAL    PRMS(7)
      
      REAL    DIST, TAREA, CAREA, XX, R2
      REAL    NEWBKP(2,MXSTND)
      REAL    PROP
      REAL    ATTOJ(2), RATTJO
      REAL    SCORE(2,MXSTND)
      REAL    TOTBKP(2)
      REAL    TOTSC(2)

      REAL    ASPEC(2), NZERO(2), DDWT(2), RMAX(2), DX, TX(2), SX(2)
      REAL    RADO(2), KL(2), KLS(2), KAV(2), KO(2), BKPO(2)
      REAL    ATTWP(2)                        
      REAL    XP, YP, XMIN, XMAX, YMIN, YMAX, XMID, YMID
      REAL    D, DMAX, CDIST, TDIST
      
      REAL    SPO(2), BAO, BAHO(2), BASPO(2)
      REAL    XSPO(2), XBAO, XBAHO(2), XBASPO(2), XBKPO(2), XRVO
      REAL    TBAHO, TBASPO, TSPO
      REAL    RVO, RVOND
      REAL    MIN
      REAL    ALPHA
      REAL    TESTVAR
      REAL    AREAOX(2)
C...AREAOX added for new BKPin from OW calculation. 9/2/99 (ajm)
      IF(LBMDEB) WRITE(JBMBPR,5) IYR
    5 FORMAT(' Begin BMATCT: Year= ',I5)

C Begin Routine

      NPASS= 1
      IF (IPSON) NPASS= 2
       
C     Initializations from ATTRACT keyword.  The first element of each array
C     gives the value for the main beetle, the second element gives the value
C     for Ips when it acts as a secondary beetle.
      ASPEC(1) = USERA(PBSPEC)
      NZERO(1) = SELFA(PBSPEC)
      DDWT(1) = USERC(PBSPEC)
      RMAX(1) = URMAX(PBSPEC)
      
      IF (NPASS .EQ. 2) THEN
        ASPEC(2) = USERA(3)
        NZERO(2) = SELFA(3)
        DDWT(2) = USERC(3)
        RMAX(2) = URMAX(3)
      END IF 
      
C     If the outside world is not being used, then don't bother calling the 
c     option processor.

      IF (OUTOFF) GOTO 3
      
C     Call the option processor to see if any values are going to change this
c     year. Once values are changed, they will remain that value until another
c     call to the option processor changes them again. Note that since we are 
c     not in a stand loop, we do not need the initial IF statement used in 
c     other calls.

      IYR1= IYR
      NPRMS= 6                                      
  
      CALL GPGET2 (310,IYR1,7,NPRMS,PRMS(1),1,I,DUM,LOK) 
  
      IF (LOK) THEN

        IF (PRMS(1) .GE. 0.0) CBKPO(1) = PRMS(1)
        IF (PRMS(2) .GE. 0.0) CBAHO(1) = PRMS(2)
        IF (PRMS(3) .GE. 0.0) CBASPO(1) = PRMS(3)
        IF (PRMS(4) .GE. 0.0) CSPO(1) = PRMS(4)
        IF (PRMS(5) .GE. 0.0) CBAO = PRMS(5)
        IF (PRMS(6) .GE. 0.0) CRVOND = PRMS(6)
      ENDIF   

c     Now call the option processor to see if values for Ips (as a driving 
c     variable) will change this year. Only bother calling if Ips is a DV.

      IF (IPSON) THEN
      
        IYR1= IYR
        NPRMS= 4                                      
        
        CALL GPGET2 (311,IYR1,7,NPRMS,PRMS(1),1,I,DUM,LOK) 
        
        IF (LOK) THEN
          IF (PRMS(1) .GE. 0.0) CBKPO(2) = PRMS(1)
          IF (PRMS(2) .GE. 0.0) CBAHO(2) = PRMS(2)
          IF (PRMS(3) .GE. 0.0) CBASPO(2) = PRMS(3)
          IF (PRMS(4) .GE. 0.0) CSPO(2) = PRMS(4)
        ENDIF   

      ENDIF
      
    3 CONTINUE
      
C***************************************************************************
C                      START HERE IN THE FIRST YEAR.
C***************************************************************************
                                           
C Skip all the spatial stuff and initial averaging if you've already done it.
      IF (ACDONE) GOTO 50
      
C         First, find ATTBYK, which is needed even if Outside is turned off.
C         This is found from equation 9, with R2 = the radius that the stand
C         would have if it was a circle, and R1 = 0.

          DO 8 CURR=1,BMSTND
            CALL SPLAAR(BMSDIX(CURR), CAREA, IRC)
            CAREA = CAREA * AC2SQM
            R2 = SQRT(CAREA / PIE)
            
            DO 7 IPASS=1,NPASS
              IF (R2 .GT. RMAX(IPASS)) R2 = RMAX(IPASS)
              ATTBYK(IPASS,CURR) = (PIE / DDWT(IPASS)) 
     &                 * (LOG(ABS(DDWT(IPASS) * R2 * R2 + NZERO(IPASS)))
     &                    - LOG(ABS(NZERO(IPASS))))
    7       CONTINUE 
    8     CONTINUE

C         If the Outside is turned off, this is all you need - go past Go 
C         and collect $200.
          IF (OUTOFF) THEN
            ACDONE = .TRUE.
            GOTO 50
          END IF                        
      
C         Zero some landscape-average counters that may be needed.
          XBAO = 0.0
          XRVO = 0.0
          DO 9 IPASS=1,NPASS
            XBKPO(IPASS) = 0.0
            XBAHO(IPASS) = 0.0
            XSPO(IPASS) = 0.0
            XBASPO(IPASS) = 0.0
    9     CONTINUE        
        
      
c         Figure out centre of landscape:
          XMAX = -1.0
          YMAX = -1.0
          XMIN=  1.e35
          YMIN=  1.e35             
          DO 10 CURR= 1,BMSTND
            CALL SPLALO(BMSDIX(CURR), XP, YP, IRC)
            IF (XP .GT. XMAX) XMAX= XP 
            IF (XP .LT. XMIN) XMIN= XP
            IF (YP .GT. YMAX) YMAX= YP
            IF (YP .LT. YMIN) YMIN= YP
   10     CONTINUE      

          XMID= (XMIN + XMAX) / 2.
          YMID= (YMIN + YMAX) / 2.

c         Find RADL, the greatest extent of Landscape, including stand area.
c         Also in this loop, add up any landscape averages that are needed
c         for calculating KO.  Values are needed if C[value] is less than
c         zero and the outside world is not floating.

          AREAL= 0.0
          AREALS = 0.0
          DMAX= -1.
          DO 13 CURR= 1,BMSTND
            CALL SPLAAR(BMSDIX(CURR), CAREA, IRC)
            CAREA= CAREA * AC2SQM
        
            AREAL= AREAL + CAREA
                   
            D= SQRT(CAREA / PIE) * MI2M
        
            CALL SPLALO(BMSDIX(CURR), XP, YP, IRC)
            D= D + SQRT((XMID-XP)**2 + (YMID-YP)**2)
            IF (D .GT. DMAX) DMAX= D
            
c           Do not include non-stockable stands in the following averages.
            IF (.NOT. STOCK(CURR)) GOTO 13
            AREALS = AREALS + CAREA            
            
C           If needed, add up the total BA of each current stand, and average RV.            
            IF ((CBAO .LT. 0.0) .AND. (UFLOAT .NE. -1.0)) 
     &          XBAO = XBAO + CAREA*(BAH(CURR,NSCL+1)+BANH(CURR,NSCL+1))
            IF ((CRVOND .LT. 0.0) .AND. (UFLOAT .NE. -1.0)) 
     &          XRVO = XRVO + CAREA * GRFSTD(CURR)
                                                                  
C           If needed, add up host BA, special trees and BKP of each current
C           stand, for each beetle type.            
            DO 12 IPASS=1,NPASS
            
              IF ((CBKPO(IPASS) .LT. 0.0) .AND. (UFLOAT .NE. -1.0)) THEN
                IF ((PBSPEC .NE. 3) .AND. (IPASS .EQ. 1)) THEN
                  XBKPO(IPASS)= XBKPO(IPASS)+(CAREA*BKP(CURR))
                ELSE
                  XBKPO(IPASS)= XBKPO(IPASS)+(CAREA*BKPIPS(CURR))
                ENDIF
              END IF
                
              IF (((CBAHO(IPASS) .GE. 0.0) .AND. (CSPO(IPASS) .GE. 0.0) 
     &          .AND. (CBASPO(IPASS) .GE. 0.0)) .OR. (UFLOAT .EQ. -1.0)) 
     &            GOTO 12

C             Find the host size-threshold for each pest species              
              IF (IPASS .EQ.1) THEN 
                MIN = ISCMIN(PBSPEC)
              ELSE 
                MIN = ISCMIN(3)
              END IF
              
C             Add up the amount of host and special trees in each size class 
              TBAHO = 0.0
              TBASPO = 0.0
              TSPO = 0.0
              DO 11 ISIZ=1,NSCL
                IF (ISIZ .GE. MIN) TBAHO = TBAHO + BAH(CURR,ISIZ)
                XX = TREE(CURR,ISIZ,1) * SPCLT(CURR,ISIZ,IPASS)
                TSPO = TSPO + XX       
                IF (ISIZ .GE. MIN) TBASPO = TBASPO + XX * MSBA(ISIZ)
   11         CONTINUE
   
              XBAHO(IPASS) = XBAHO(IPASS) + CAREA * TBAHO             
              XSPO(IPASS) = XSPO(IPASS) + CAREA * TSPO
              XBASPO(IPASS) = XBASPO(IPASS) + CAREA * TBASPO
   12       CONTINUE
            
   13     CONTINUE
                     
          RADL= DMAX * M2MI
          
c         If needed, divide landscape sums by AREALS to get the required averages.  
c         Re-initialize any array values that have been set to negative numbers
c         because the initial averages were missing.
          IF (UFLOAT .EQ. -1.0) GOTO 21
          
          IF (CBAO .LT. 0.0) CBAO = XBAO / AREALS
          IF (CRVOND .LT. 0.0) CRVOND = (XRVO / AREALS) / RVOD

          DO 20 IPASS=1,NPASS          
            IF (CBKPO(IPASS) .LT. 0.0) 
     &         CBKPO(IPASS) = XBKPO(IPASS) / AREALS
            IF (CBAHO(IPASS) .LT. 0.0)
     &         CBAHO(IPASS) = XBAHO(IPASS) / AREALS
            IF (CBASPO(IPASS) .LT. 0.0)
     &         CBASPO(IPASS) = XBASPO(IPASS) / AREALS
            IF (CSPO(IPASS) .LT. 0.0) 
     &         CSPO(IPASS) = XSPO(IPASS) / AREALS
   20     CONTINUE
   21     CONTINUE                              
          
c         equations 5, 11, 15 and 16:
          RADLC = SQRT(AREAL / PIE)
          
          DO 211 IPASS=1,NPASS
            RADO(IPASS) = RADL + RMAX(IPASS)
            AREAO(IPASS) = (PIE * RADO(IPASS) * RADO(IPASS)) - AREAL

            IF (RADLC .GT. RMAX(IPASS)) THEN
              AREALP(IPASS) = AREAL - (PIE * (RADLC - RMAX(IPASS))**2)
            ELSE
              AREALP(IPASS) = AREAL
            ENDIF
  211     CONTINUE

c       Find the 'attractiveness constant' of each stockable stand.  TX is the
c       logarithmic term in equation 9 (which is constant for all stands), and
c       SX is the stand-summation term.  Distances and areas are converted here  
c       to MILES or SQUARE MILES.  CDIST and TDIST are used to ensure that no
c       pair of current and target stands are considered to be at closer AVERAGE
c       distance than is physically possible (i.e. closer than the distance between
c       the centers of two squares with their areas).
        DO 22 IPASS=1,NPASS
          TX(IPASS)= (PIE / DDWT(IPASS)) 
     &     * (LOG(ABS(DDWT(IPASS)*RMAX(IPASS)*RMAX(IPASS)+NZERO(IPASS)))
     &        - LOG(ABS(NZERO(IPASS))))
   22   CONTINUE  
      
        DO 30 CURR= 1,BMSTND
          IF (.NOT. STOCK(CURR)) GOTO 30
          CALL SPLAAR(BMSDIX(CURR),CAREA,IRC)
          CDIST = 0.5 * SQRT(CAREA * AC2SQM)
          
          SX(1)= 0.0
          SX(2)= 0.0
          DO 25 TARG= 1,BMSTND
            
            IF (TARG .EQ. CURR) THEN
              DO 23 IPASS=1,NPASS
                SX(IPASS) = SX(IPASS) + ATTBYK(IPASS,CURR)
   23         CONTINUE
                
            ELSE                                        
              CALL SPLADS(BMSDIX(TARG),BMSDIX(CURR),DIST,IRC)
              DIST= DIST * M2MI
              CALL SPLAAR(BMSDIX(TARG),TAREA,IRC)
              TAREA= TAREA * AC2SQM
              TDIST = 0.5 * SQRT(TAREA)
              IF (DIST .LT. (CDIST+TDIST)) DIST = CDIST + TDIST
              
              DO 24 IPASS=1,NPASS
                IF (DIST .LE. RMAX(IPASS)) THEN
                  DX= DDWT(IPASS) * DIST * DIST       
                  SX(IPASS)= SX(IPASS) + (TAREA / (DX + NZERO(IPASS)))
                END IF
   24         CONTINUE               
            END IF
              
   25     CONTINUE

c         calculate ATTC from equation 9, and make sure it's not negative
          DO 27 IPASS=1,NPASS
            ATTC(IPASS,CURR)= TX(IPASS) - SX(IPASS)
            IF (ATTC(IPASS,CURR) .LT. 0.0) ATTC(IPASS,CURR) = 0.0
   27     CONTINUE 
           
   30   CONTINUE 
   
c This is the end of the "spatial stuff".
        ACDONE= .TRUE.
   50 CONTINUE
                  
                  
C***************************************************************************
C                     START HERE IN THE SECOND YEAR.
C***************************************************************************

c You can skip some stuff if the Outside is turned off.
      IF (OUTOFF) GOTO 91      
 
C Calculate KL.  If the Outside is floating, also find average BKP in stockable
C stands, and KLS.
      DO 60 IPASS=1,NPASS
        KL(IPASS) = 0.0
        KLS(IPASS) = 0.0
        XBKPO(IPASS) = 0.0
   60 CONTINUE
      
      DO 75 CURR= 1,BMSTND
        CALL SPLAAR(BMSDIX(CURR),CAREA,IRC)
        CAREA= CAREA * AC2SQM
            
        DO 70 IPASS= 1,NPASS
          XX = CAREA * NUMER(IPASS,CURR)
          KL(IPASS)= KL(IPASS) + XX
          
          IF ((UFLOAT .EQ. -1.0) .AND. (STOCK(CURR)))THEN
            KLS(IPASS) = KLS(IPASS) + XX
            
            IF ((PBSPEC .NE. 3) .AND. (IPASS .EQ. 1)) THEN
              XBKPO(IPASS)= XBKPO(IPASS)+(CAREA*BKP(CURR))
            ELSE
              XBKPO(IPASS)= XBKPO(IPASS)+(CAREA*BKPIPS(CURR))
            ENDIF
            
          END IF
          
   70   CONTINUE
   75 CONTINUE
   
      DO 76 IPASS= 1,NPASS
        KL(IPASS)=  KL(IPASS) / AREAL
   76 CONTINUE

 
C At this point, all initial values for the Outside are known.  Get KO
C and BKPO for the current year.  If the Outside is floating, these are just
C the landscape averages for stockable stands.  Otherwise, KO must be calculated
C from some component values.
      IF (UFLOAT .EQ. -1.0) THEN
      
        DO 80 IPASS=1,NPASS
          KO(IPASS) = KLS(IPASS) / AREALS
          BKPO(IPASS) = XBKPO(IPASS) / AREALS
   80   CONTINUE
       
      ELSE
      
        BAO = CBAO
        RVOND = CRVOND
        RVO = RVOND * RVOD
        DO 85 IPASS=1,NPASS
          BAHO(IPASS) = CBAHO(IPASS)
          BASPO(IPASS) = CBASPO(IPASS)
          SPO(IPASS) = CSPO(IPASS)
          BKPO(IPASS) = CBKPO(IPASS)
          KO(IPASS) = (ASPEC(IPASS)*SPO(IPASS) + 1) * BAO
     &              * (BAHO(IPASS)+BASPO(IPASS)) / RVO
   85   CONTINUE

C     If this is a bad reproductive year then reduce the BKP outside as well
C     Notice that it may only be a bad year for one beetle type.

      IF (LBAD) THEN
        IF (IBADBB .NE. 3 .OR. (IBADBB .EQ. 3 .AND. NPASS .EQ. 1))
     &      BKPO(1) = BKPO(1) * BADREP(PBSPEC)
        IF (IBADBB .GE. 3 .AND. NPASS .EQ. 2) 
     &      BKPO(2) = BKPO(2) * BADREP(3)
      ENDIF    
   
      END IF
              
C If parts of the Outside are non-stockable, reduce KO and BKPO accordingly.
      IF (STOCKO .LT. 1.0) THEN
        DO 87 IPASS=1,NPASS
          KO(IPASS) = KO(IPASS) * STOCKO
          BKPO(IPASS) = BKPO(IPASS) * STOCKO
   87   CONTINUE
      END IF  
C
C Calculate KAV from equation 17, and ATTWP from equation 18:
      DO 90 IPASS=1,NPASS
c        KAV(IPASS) = (KL(IPASS) + KO(IPASS)) / 2
c
        KAV(IPASS) = ((AREAO(IPASS) * KO(IPASS))
     &             + (AREALP(IPASS) * KL(IPASS)))
     &             / (AREAO(IPASS) + AREALP(IPASS))
c        ATTWP(IPASS) = (PIE / DDWT(IPASS)) * KL(IPASS) *
        ATTWP(IPASS) = (PIE / DDWT(IPASS)) * KAV(IPASS) *
     &    (LOG(ABS(DDWT(IPASS) *RMAX(IPASS) *RMAX(IPASS) +NZERO(IPASS)))
     &     - LOG(ABS(NZERO(IPASS))))
C...  KAV (above) changed to KL (ajm 8/30/99)
   90 CONTINUE

C***************************************************************************
C                   START HERE IF OUTSIDE IS TURNED OFF.
C***************************************************************************
   91 CONTINUE

C Zero some BKP counters
      DO 95 CURR=1,BMSTND
        IF (.NOT. STOCK(CURR)) GOTO 95
        DO 94 IPASS=1,NPASS
          NEWBKP(IPASS,CURR)= 0.0
          BKPOUT(IPASS,CURR)= 0.0
          BKPIN(IPASS,CURR)= 0.0
          SELFBKP(IPASS,CURR)=0.0                           !NEW AUG 05 AJM
   94   CONTINUE
   95 CONTINUE

C Loop over each source stand, dispersing the beetles from stockable stands.
      DO 100 CURR= 1,BMSTND
        IF (.NOT. STOCK(CURR)) GOTO 100
        CALL SPLAAR(BMSDIX(CURR),CAREA,IRC)

C       For each beetle type, zero TOTSC, find ATTOJ from equation 9, and
C       find the total BKP in the stand (while CAREA is still in acres).
        DO 101 IPASS= 1,NPASS
          TOTSC(IPASS)= 0.0
          IF (.NOT. OUTOFF) ATTOJ(IPASS) = KO(IPASS) * ATTC(IPASS,CURR)
          IF ((PBSPEC .NE. 3) .AND. (IPASS .EQ. 1)) THEN
            TOTBKP(IPASS)= CAREA * BKP(CURR)
          ELSE
            TOTBKP(IPASS)= CAREA * BKPIPS(CURR)
          ENDIF
  101   CONTINUE

        CAREA= CAREA * AC2SQM
        CDIST = 0.5 * SQRT(CAREA)

c       Loop over the target stands finding their total attractiveness
        DO 110 TARG= 1,BMSTND

C         For your own stand, attractiveness is given by equation 4
          IF (TARG .EQ. CURR) THEN
            DO 119 IPASS=1,NPASS
              SCORE(IPASS,TARG)= NUMER(IPASS,CURR) * ATTBYK(IPASS,CURR)
              TOTSC(IPASS)= TOTSC(IPASS) + SCORE(IPASS,TARG)
  119       CONTINUE

C         For others, attractiveness is approximated from equation 1,
C         except that stands further than RMAX have an attractiveness of 0.
c         Again, CDIST and TDIST are used to ensure that no pair of stands
c         are taken to be closer on average than is physically possible.
          ELSE
            CALL SPLADS(BMSDIX(TARG),BMSDIX(CURR),DIST,IRC)
            DIST= DIST * M2MI
            CALL SPLAAR(BMSDIX(TARG),TAREA,IRC)
            TAREA= TAREA * AC2SQM
            TDIST = 0.5 * SQRT(TAREA)
            IF (DIST .LT. (CDIST+TDIST)) DIST = CDIST + TDIST

            DO 120 IPASS=1,NPASS
              IF (DIST .LE. RMAX(IPASS)) THEN
                DX= DDWT(IPASS) * DIST * DIST
C                DX= (DDWT(IPASS) * DIST) ** 2
                SCORE(IPASS,TARG)= TAREA * NUMER(IPASS,TARG)
     &                / (DX + NZERO(IPASS))
                TOTSC(IPASS)= TOTSC(IPASS) + SCORE(IPASS,TARG)
              ELSE
                SCORE(IPASS,TARG)= 0.0
              END IF
  120       CONTINUE

          END IF
  110   CONTINUE
C
C       Calculate BKP flux between the current source stand and the
C       Outside world for each beetle type,unless the Outside world is
C       turned off.
        IF (OUTOFF) GOTO 124

        DO 123 IPASS= 1, NPASS

C         Add attractiveness of Outside to the total for the current stand,
C         and calculate BKP lost to the Outside (BKPOUT).
          TOTSC(IPASS)= TOTSC(IPASS) + ATTOJ(IPASS)
C
C  
        IF (TOTSC(IPASS) .GT. 0.) BKPOUT(IPASS,CURR) = TOTBKP(IPASS)
     &                              * (ATTOJ(IPASS) / TOTSC(IPASS))

C         Use eqn.24 to calculate RATTJO
          XX= KO(IPASS) * AREAO(IPASS) * ATTWP(IPASS)
        IF (XX .GT. 0.0) THEN
            RATTJO= (CAREA * NUMER(IPASS,CURR) * ATTOJ(IPASS)) / XX
c
        ELSE
            RATTJO= 0.0
        ENDIF
C
C         Find total BKP in Outside, and calculate BKP entering the current
C         stand from the outside world (BKPIN).  Add this amount to NEWBKP.
C         BKPIN(IPASS,CURR)= BKPO(IPASS) * (AREAO(IPASS) / AC2SQM) *RATTJO
          AREAOX(IPASS)= 2753.04 * (1- EXP(-0.06366 * RADLC)) **1.5938
     &     * (1 - EXP(-0.0687 * RMAX(IPASS))) ** 0.8827
c         BKPIN(IPASS,CURR)= BKPO(IPASS)* (AREAO(IPASS)/AC2SQM)* RATTJO
          BKPIN(IPASS,CURR)= BKPO(IPASS)* (AREAOX(IPASS)/AC2SQM)* RATTJO
          NEWBKP(IPASS,CURR)= NEWBKP(IPASS,CURR) + BKPIN(IPASS,CURR)
c          WRITE (38, '(35X, 2F9.2)') AREAOX(1), AREAO
  123   CONTINUE

  124   CONTINUE

C       Allocate BKP from the current stand to each target stand.
        DO 200 TARG= 1,BMSTND
          DO 201 IPASS= 1,NPASS
            PROP= 0.
            IF (TOTSC(IPASS) .GT. 0.0)
     &        PROP= SCORE(IPASS,TARG) / TOTSC(IPASS)
C    NEW VAR TO HOLD NEWBKP FROM SELF AJM 8.05
             IF (TARG.EQ.CURR)SELFBKP(IPASS,TARG)=PROP*TOTBKP(IPASS)
            NEWBKP(IPASS,TARG)=NEWBKP(IPASS,TARG)+(PROP*TOTBKP(IPASS))
C
  201     CONTINUE
  200   CONTINUE

  100 CONTINUE
C
      DO 300 CURR=1,BMSTND

C     ************************************************************
C     *** I THINK THERE SHOULD BE A STOCK() TEST HERE... SO I  ***
C     *** PUT ONE IN                                           ***
C     ************************************************************

        IF (.NOT. STOCK(CURR)) GOTO 300
        
c       Change back to PER AREA (Acres)

        CALL SPLAAR(BMSDIX(CURR),CAREA,IRC)
        
        DO 301, IPASS= 1,2
          NEWBKP(IPASS,CURR)= NEWBKP(IPASS,CURR) / CAREA
          BKPOUT(IPASS,CURR)= BKPOUT(IPASS,CURR) / CAREA
          BKPIN(IPASS,CURR)=  BKPIN(IPASS,CURR) / CAREA
          SELFBKP(IPASS,CURR)=SELFBKP(IPASS,CURR) / CAREA
  301   CONTINUE
        
c       Calculate how much BKP actually makes it into the stand.
c       Any remainder is lost and not reallocated to another stand.
c       (TFOOD includes a multiplier for prob host ba in stand -- see
c       BMCNUM for calculation.)

C...The 0.4 multiplier replaced by ALPHA. SDD is from varyrain (MJO, Zhang July98).

        IF (SDD .LT. 0) THEN
c           ALPHA = (0.565 - (0.085 * SDD))
           ALPHA = (0.365 - (0.178 * SDD))
        ELSE
c           ALPHA = (0.565 - (0.034 * SDD))
           ALPHA = (0.365 - (0.034 * SDD))
        ENDIF

        IF (ALPHA .GT. 0.90) ALPHA = 0.90
        IF (ALPHA .LT. 0.01) ALPHA = 0.01
        IF (PBSPEC .NE. 3) THEN
           IF ((NEWBKP(1,CURR) .GT. 0.0) 
     &          .AND. (TFOOD(CURR,1) .GT. 0.0)) THEN
              BKP(CURR) = TFOOD(CURR,1) * (1.0 - 
     &        EXP(-(ALPHA * NEWBKP(1,CURR) / (TFOOD(CURR,1) + 1E-6))))
              BKPS(CURR) = BKP(CURR) / NEWBKP(1,CURR)
           ELSE 
              BKP(CURR) = 0.0
              BKPS(CURR) = 1.0
           ENDIF
           BKPA(CURR) = BKP(CURR)

           IF (NEWBKP(2,CURR) .GT. 0.0) THEN
              BKPIPS(CURR) = TFOOD(CURR,2) * (1.0 - 
     &        EXP(-(ALPHA * NEWBKP(2,CURR) / (TFOOD(CURR,2) + 1E-6))))
           ELSE
              BKPIPS(CURR) = 0.0
           ENDIF

        ELSE
           IF (NEWBKP(1,CURR) .GT. 0.0) THEN
              BKPIPS(CURR) = TFOOD(CURR,1) * (1.0 - 
     &        EXP(-(ALPHA * NEWBKP(1,CURR) / (TFOOD(CURR,1) + 1E-6))))
              BKPS(CURR) = BKPIPS(CURR) / NEWBKP(1,CURR)
            ELSE
              BKPIPS(CURR)= 0.0
              BKPS(CURR)= 1.0
            ENDIF
            BKPA(CURR) = BKPIPS(CURR)
        ENDIF

C     Put a tiny amount of BKP in the stand as a background level (too little to 
c     do anything unless the small trees are really sick)
c
c        IF ((BKP(CURR) .EQ. 0.0) .AND. (BAH(CURR,NSCL+1) .GT. 0.0)) 
c     &         BKP(CURR)= 0.25 * MSBA(1) / CAREA
c      
c        IF ((BKPIPS(CURR) .EQ. 0.0) .AND. (BAH(CURR,NSCL+1) .GT. 0.0)) 
c     &         BKPIPS(CURR)= 0.25 * MSBA(1) / CAREA
      
  300 CONTINUE

      IF(LBMDEB) WRITE(JBMBPR,2) IYR
    2 FORMAT(' End BMATCT: Year= ',I5)

      RETURN
      END
