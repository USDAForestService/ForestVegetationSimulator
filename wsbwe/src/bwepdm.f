      SUBROUTINE BWEPDM
      IMPLICIT NONE
C----------
C  **BWEPDM                 DATE OF LAST REVISION:  07/14/10
C----------
C
C     PERIODIC DAMAGE.  TRANSLATES ANNUAL DAMAGE ACCUMULATORS INTO
C     DAMAGE DONE TO EACH TREE.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984
c
c     minor changes by K.Sheehan 7/96 to remove LBWDEB,JBWPL4
C
C     CALLED FROM :
C
C       BWECUP - SINGLE STAND BUDWORM MODEL LINK TO PROGNOSIS.
C
C     FUNCTIONS CALLED :
C
C       BWERNP  - GENERATE A BETA VARIATE WITH PARAMETERS A FUNCTION
C                OF THE MEAN.
C
C     SUBROUTINES CALLED :
C
C       BWECRC - DETERMINE CROWN CLASS OF TREE.
C
C  Revision History:
C
C     28-DEC-1999  Lance R. David (FHTET)
C       Update for expansion of FVS stand id (variable NPLT)
C       from 8 to 26 characters.
C     01-SEP-2000  Lance R. David (FHTET)
C       SDI-based mortality until stand reaches 10 inch QMD update
C       to FVS leaves BAMAX variable at zero, during these times.
C       BAMAX is needed by this subroutine and will be calculated from
C       FVS variable SDIMAX when variable BAMAX is zero. Look for local
C       variable TBAMAX to find the few lines of code for this update.
C     10-NOV-2000  Lance R. David (FHTET)
C       Added comments and species lits for IFIR array.
C     13-JUN-2001  Lance R. David (FHTET)
C       Added debug handling.
C       Random variations applied to height and diameter growth values
C       in the growth adjustments section sometimes resulted in negative
C       values larger than the current DBH and/or HT. This was specifically
C       encountered with natural regen trees that were very small (i.e.
C       0.1 DBH and 1 foot tall). A check was added to set any negative
C       increment values to zero.
C     16-AUG-2001 Lance R. David (FHTET)
C       Added calls to BWERPT and BWERGT to place and retrieve the damage
C       model random number seed into the generator so that random
C       numbers used in function BWERNP and subroutine BWEBET are from
C       the damage model random number series and not from one of the 
C       other two random number series used in the GenDefol model, 
C       weather and outbreak scheduling.
C     15-APR-2003 Lance R. David (FHTET)
C       Replaced old mortality process with new equations based on
C       Mike Marsden's analysis of Bruce Hostetler's data. Kathy
C       Sheehan interpreted Mike's memos and created the equation
C       put into the code. Several new category variables used in the
C       new mortality equation were also added. Those variables are
C       for topkill, missing foliage top, and missing foliage middle.
C       Values for basal area of all trees and basal area of host trees 
C       are also computed from begining of cycle FVS values for each
C       sample point (or plot) in the data set. Note that the mortality
C       rate applied is limited to 0.98 and this limit was already in
C       place before this update.
C     28-OCT-2003 Lance R. David (FHTET)
C       New topkill proportion function and coefficients from Mike's
C       analysis put into routine. Note that this is just the amount of
C       topkill and that the probability of topkill is unchanged.
C       There is a deviation from the analysis regarding the topkill
C       variable in the function. The analysis specifies topkill year
C       prior, but this routine does not cycle on a annual time step.
C       If this model is active and defoliation has occured, a tree's
C       existing topkill is assumed to have been the result of 
C       defoliation. What can not be determined is the length of time
C       that has past since topkill or if the current truncated height
C       is the result of multiple topkill events.
C       Added include for dwarf mistletoe model common file MISCOM.F77
C       because dwarf mistletoe rating is utilized in the topkill
C       proportion function.
C
C    21-DEC-2005 Lance R. David (FHTET)
C       Set surrogate species coefficients from existing DF and GF 
C       coefficients.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'BWESTD.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'MISCOM.F77'
C
COMMONS
C
      INTEGER I, IBWYR, IC, ICRC1, ICRC2, IFIR(6), IHOST, II, IOD,
     &        ISPI, ISZI, ITRC1, ITRC2, ITREE, NEW, NOBWYR
      REAL AF, AVDEF, BARK, BASE, BRATIO, BWERNP, CN, D, DBHI, DDS,
     &     DGI, DTK, FA, FTKILL, H, HTGI, ORGHT, PART, PRTOPK, SDEF,
     &     STREES, TOPH, X, XD, XH
      REAL    TBAMAX, PNTBA(MAXPLT), PNTHBA(MAXPLT),
     &        B0(6), B1(6), B2(6), B3(6), B4(6), B5(6), B6(6), B7(6),
     &        TK0(6), TK1(6), TK2(6), TK3(6), TK4(6), TK5(6),
     &        TK6(6), TK7(6), TK8(6),
     &        KTK, MFM, MFT, PR, XPR, PCTK
     
      LOGICAL DEBUG

C     fir/non-fir tree species index
C               WF DF GF AF ES WL
      DATA IFIR/ 0, 1, 0, 0, 1, 1/

C     DEFOLIATION MODEL SPECIES INDICES:
C       1 = WF - White fir      --> uses GF coefficients
C       2 = DF - Douglas fir    --> original coefficient from analysis
C       3 = GF - Grand fir      --> original coefficient from analysis
C       4 = AF - Subalpine fir  --> uses GF coefficients
C       5 = ES - Engelmann spruce --> uses DF coefficients
C       6 = WL - Western Larch  --> uses DF coefficients
C       7 = Not a host

C
C     Survival equation coefficients indexed by specie
C
C intercept
      DATA B0
     & /46.27900, 57.75010, 46.27900, 46.27900, 57.75010, 57.75010/
C elevation
      DATA B1
     & /-1.80810, -2.28210, -1.80810, -1.80810, -2.28210, -2.28210/
C elevation2
      DATA B2
     & / 0.01930,  0.02430,  0.01930,  0.01930, 0.02430,   0.02430/
C basal_area_at_point
      DATA B3
     & / 0.00550,      0.0,  0.00550,  0.00550,     0.0,       0.0/
C host_basal_area_at_point
      DATA B4
     & /-0.00808, -0.00793, -0.00808, -0.00808, -0.00793, -0.00793/
C missing_foliage_top
      DATA B5
     & / 0.57450,  0.92870,  0.57450,  0.57450,  0.92870,  0.92870/
C topkill category
      DATA B6
     & /-0.24050, -0.22330, -0.24050, -0.24050, -0.22330, -0.22330/
C missing_foliage_top * missing_foliage_middle
      DATA B7
     & /-0.09640, -0.13180, -0.09640, -0.09640, -0.13180, -0.13180/

C
C     Topkill equation coefficients indexed by specie
C
C intercept
      DATA TK0
     & /21.89880, 17.76920, 21.89880, 21.89880, 17.76920, 17.76920/
C elevation
      DATA TK1
     & /-0.00714, -0.00535, -0.00714, -0.00714, -0.00535, -0.00535/
C elevation2
      DATA TK2
     & / 6.99E-6,  5.22E-6,  6.99E-6,  6.99E-6,  5.22E-6,  5.22E-6/
C dwarf mistletoe rating
      DATA TK3
     & /     0.0, -0.01270,      0.0,      0.0, -0.01270, -0.01270/
C percent topkill for the tree in the year prior.
      DATA TK4
     & /-5.26520, -7.94630, -5.26520, -5.26520, -7.94630, -7.94630/
c missing foliage top
      DATA TK5
     & /-0.13630, -0.20360, -0.13630, -0.13630, -0.20360, -0.20360/
c missing foliage middle
      DATA TK6
     & /  0.28030,  0.12690,  0.28030,  0.28030,  0.12690,  0.12690/
c missing foliage middle X percent topkill last year
      DATA TK7
     & /  0.14030,  0.38010,  0.14030,  0.14030,  0.38010,  0.38010/
c missing foliage top X missing foliage middle
      DATA TK8
     & / -0.03770, -0.04120, -0.03770, -0.03770, -0.04120, -0.04120/


C
C.... Check for DEBUG
C
      CALL DBCHK(DEBUG,'BWEPDM',6,ICYC)

      IF (DEBUG) WRITE (JOSTND,*) 'ENTER BWEPDM: ICYC = ',ICYC
C
C
C     ********************** EXECUTION BEGINS **************************
C
C     COMPUTE FA TO BE THE FRACTION OF THE PERIOD WHERE THE BWMODEL
C     HAS NOT RUN AND IBWYR TO BE THE NUMBER OF YEARS THE MODEL HAS
C     RUN.  THESE VALUES WILL BE USED TO MODIFY THE GROWTH LOSS RATES.
C
      IF (IBWYR2+1.LT.IY(ICYC+1).AND.IBWYR2.GE.0) THEN
         NOBWYR=IY(ICYC+1)-(IBWYR2+1)
         IBWYR=IFINT-NOBWYR
         FA=FLOAT(NOBWYR)/FINT
      ELSE
         NOBWYR=0
         IBWYR=IFINT
         FA=0.0
      ENDIF
C
C     IF THE CYCLE IS LONGER THAN THE OUTBREAK, THEN: ADD THE
C     FRACTIONAL PART OF THE PERIOD TO THE EXPECTED INCREMENTS...
C     PEDDS AND PEHTG.
C
      IF (FA.GT.0.0) THEN
         DO 20 IHOST=1,6
            IF (IFHOST(IHOST).EQ.0) GOTO 20
            DO 10 ISZI=1,3
               PEDDS(IHOST,ISZI)=PEDDS(IHOST,ISZI)+FA
               PEHTG(IHOST,ISZI)=PEHTG(IHOST,ISZI)+FA
   10       CONTINUE
   20    CONTINUE
      ENDIF

      IF (DEBUG) THEN
         WRITE(JOSTND,*) 'IN BWEPDM: IBWYR=',IBWYR,' IBWYR2=',IBWYR2,
     &   ' NOBWYR=',NOBWYR,' FA=',FA
         WRITE(JOSTND,*) 'IN BWEPDM: PEDDS=',PEDDS
         WRITE(JOSTND,*) 'IN BWEPDM: PEHTG=',PEHTG
      ENDIF
C
C     RESTORE THE DAMAGE MODEL RANDOM NUMBER SEED TO THE GENERATOR.
C
      CALL BWERPT (DSEEDD)

C
C     APPLY THE DAMAGE TO THE TREE RECORDS.
C
C     write header for periodic damage table output if requested.
C
      IF (LBWPDM) WRITE (JOWSBW,30) IY(ICYC+1)-1,NPLT,MGMID
   30 FORMAT (/' PERDAM: ',I4,'; ',A26,'; ',A4,' MORTALITY',T52,
     >        'DBH GROWTH',T66,'HT GROWTH',T80,'PROB',T87,'TOP'/
     >        ' TREE',T15,'DBH  ORG HT',T28,'TREES',T36,'BASE',
     >        T45,'USED',T52,'BASE',T59,'USED',T66,'BASE',T73,'USED',
     >        T79,'OF TOP',T87,'KILL'/    ' NO.',T8,'SP.',
     >        T14,'(IN)',T21,'(FT)',T28,'/ACRE',T35,'(/ACRE)',
     >        T43,'(/ACRE)',T52,'(IN)',T59,'(IN)',T66,'(FT)',T73,'(FT)',
     >        T80,'KILL',T87,'(FT)'/
     >        1X,'----  --- ',2(' ------'),3(' -------'),6(' ------'))
C
C     Run through tree list to sum basal area for all trees and 
C     and basal area for host trees 3+ inch DBH on each sample point.
C     FVS array ITRE() holds point (or plot) number.
C
C     Note from Bruce indicated that Tommy included all trees when he
C     provided the point basal area values to Mike. So, condition is
C     commented out. 19-MAR-2003
C
      DO I = 1,IPTINV
         PNTBA(I) = 0.0
         PNTHBA(I) = 0.0
      ENDDO

      DO 40 I = 1,ITRN
CCC      IF (DBH(I) .GE. 3.0) THEN      ----see note above----
C           sum basal area for all trees on point
            PNTBA(ITRE(I)) = PNTBA(ITRE(I))
     >                     + DBH(I)*DBH(I)*0.005454154

            IF (IBWSPM(ISP(I)) .LT. 6) THEN
C              sum basal area for bw host trees on point
               PNTHBA(ITRE(I)) = PNTHBA(ITRE(I))
     >                         + DBH(I)*DBH(I)*0.005454154
            ENDIF
CCC      ENDIF                          ----see note above----
   40 CONTINUE
C
C     DO FOR ALL TREES.
C
      PRTOPK=0.0
      FTKILL=0.0
      IF (ITRN.LE.0.OR.IBWYR2.EQ.-1) GOTO 70

      DO 60 ISPI=1,MAXSP
         IF (ISCT(ISPI,1).EQ.0) GOTO 60
C
C        IF THE TREE IS NON-HOST OR LARCH, THEN: BYPASS THE CALCULATIONS.
C
         IHOST=IBWSPM(ISPI)
         IF (IHOST.GE.6) GOTO 60

         DO 50 II=ISCT(ISPI,1),ISCT(ISPI,2)
            I=IND1(II)
            H=HT(I)
            ORGHT=H
            BARK=BRATIO(ISPI,DBH(I),H)
C
C           FIND THE HEIGHT AND CROWN CLASS INDICIES FOR THE TREE RECORD.
C
            CALL BWECRC(H,ISZI,ICRC1,ICRC2)
C
C           Calculate variables use in the proportion of topkill and
C           probability of mortality equations.
C
C           Compute missing foliage categories (1-9) for top and middle
C           crown as average of all age classes from proportion of 
C           retained biomass.
C
            MFT = 10.0 - IFIX((((PRBIO(IHOST,ICRC1,1) * 0.25)
     &                 +(PRBIO(IHOST,ICRC1,2) * 0.25)
     &                 +(PRBIO(IHOST,ICRC1,3) * 0.25)
     &                 +(PRBIO(IHOST,ICRC1,4) * 0.25)) * 10.0) + 0.5)
C
C           Set index for middle crown third for current size class.
C
            IC = ICRC1 + 1
            MFM = 10.0 - IFIX((((PRBIO(IHOST,IC,1) * 0.25)
     &                 +(PRBIO(IHOST,IC,2) * 0.25)
     &                 +(PRBIO(IHOST,IC,3) * 0.25)
     &                 +(PRBIO(IHOST,IC,4) * 0.25)) * 10.0) + 0.5)

C
C           The calculated variable in Michael Marsden's analysis
C           was percent topkill year prior. This routine does not
C           process on a annual basis, but once per cycle. So the
C           existing topkill is being vaguely assumed to have
C           occurred in the cycle prior if the model is active and
C           defoliation has taken place.
C           
            IF (ITRUNC(I) .GT. 0) THEN
               PCTK = ITRUNC(I)/NORMHT(I)
            ELSE
               PCTK = 0.0
            ENDIF
            IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: CURRENT PCTK=',PCTK,
     &                 ' LTOPK=', LTOPK
C           initialize topkill variables
            FTKILL=0.0
            PRTOPK=0.0
            HTGI=HTG(I)
C
C           Calculate and apply topkill if option is on. (actually,
C           it is initialized "on" with no user option to turn it off)
C

            IF (LTOPK) THEN

C              COMPUTE TOP KILL.  THE TOPKILL MODELS FOR LARGE TREES ARE BASED
C              ON IDEAS ONLY...NOT DATA.  N.CROOKSTON, TOMMY GREG, AND BRUCE
C              HOSTETLER CAME UP WITH THEM.  FOR SMALL TREES, SEE FERGUSON
C              1988 RESEARCH PAPER INT-393.
C
  
               AVDEF=BWERNP(AVYRMX(IHOST,ISZI),.06)
               IF (ISZI.EQ.1) THEN
                  X=H
                  IF (H.GT.10.) X=10.
                  PRTOPK=1./(1.+EXP(-(-2.5817-.027635*FLOAT(ICR(I))+
     >                   3.709*SQRT(AVDEF)+.0488*X)))
               ELSE
                  IF (IFIR(IHOST).EQ.0) THEN
                     PRTOPK=.96*(1.-EXP(-((.65*(AVDEF+1.))**14.)))
                  ELSE
                     PRTOPK=.90*(1.-EXP(-((.60*(AVDEF+1.))**11.)))
                  ENDIF
               ENDIF

               CALL BWERAN (X)
C              write (JOSTND,*) 'in bwepdm: damage random number: ',x  ! TEMP DEBUG

               IF (DEBUG) WRITE (JOSTND,*) 'IN BWEPDM: I=',I,' HT=',H,
     &         ' HTG=',HTGI,' ICR=',ICR(I),' ISZI=',ISZI,' CRC1=',
     &         ICRC1,' CRC2=',ICRC2,' AVDEF=',AVDEF,' PRTOPK=',PRTOPK,
     &         ' RANX=',X
C
C              IF THE RANDOM NUMBER IS LESS THAN THE PRTOPK, SIMULATE THE
C              TOPKILLING.
C
               IF (X.LT.PRTOPK) THEN
C                 *************
C                 ************* begin original topkill proportion "PART"
C                 *************
C
C                 TOPKILL THESE TREES.  PROPORTION KILLED EQUATIONS OP.CITE.
C
CX                IF (ISZI.EQ.1) THEN
CX                   CALL BWERAN(X)
CX                   PART = .05684*((-ALOG(X))**1.7036)
C
C                    IF X<.0046, PART>1.0 SO BOUND PART TO .9 ... NOTE
C                    THAT .9<PART<1.0 OCCURS 0.6346 PERCENT OF THE TIME.
C                    THE BOUNDING OCCURS AFTER THE FOLLOWING MODELS, SO
C                    THEY ARE BOUNDED TOO.
C
CX                ELSE
CX                   IF (IFIR(IHOST).EQ.0) THEN
CX                     PART=.0960*(1.-EXP(-((.717*(AVDEF+1.))**7.92)))
CX                   ELSE
CX                     PART=.2545*(1.-EXP(-((.641*(AVDEF+1.))**6.60)))
CX                   ENDIF
CX                ENDIF
C                 *************
C                 ************* end original topkill proportion "PART"
C                 *************

C                 *************
C                 ************* begin new topkill proportion "PART"
C                 *************

C                 The function and coefficients for the proportion of
C                 topkill are the results of 2002 analysis by Michael
C                 Marsden of data in Bruce Hostetler's western spruce
C                 budworm study in the Blue Mountains.
C                 During the analysis it was necessary for Michael to
C                 bound the proportion between 0.01 and 0.99. That
C                 bounding was not carried forward here.
 
                  PART = 1.0/(1.0 + EXP(TK0(IHOST)
     &               +(TK1(IHOST)*ELEV)+(TK2(IHOST)*ELEV*ELEV)
     &               +(TK3(IHOST)*IMIST(I))+(TK4(IHOST)*PCTK)
     &               +(TK5(IHOST)*MFT)+(TK6(IHOST)*MFM)
     &               +(TK7(IHOST)*MFM*PCTK)+(TK8(IHOST)*MFT*MFM)))

                  IF (DEBUG) WRITE (JOSTND,*) 'IN BWEPDM: PART=',PART

C                 *************
C                 ************* end new topkill proportion "PART"
C                 *************

C                 Limiting the proportion of topkill to 0.9 is retained
C                 from the origingal topkill process.
C
                  IF (PART.GT.0.9) PART=.9

C                 The application of the topkill to the tree record is retained
C                 from the original process coded by Nick Crookston.
C
                  IF (PART.GT.0.0) THEN
                     FTKILL=H*PART
C
C                    TOP KILL THE TREE. USE LOGIC LIKE THAT FOUND IN HTGSTP.
C
                     TOPH=H-FTKILL
                     ITRC2=IFIX(TOPH*100.+.5)
                     ITRC1=ITRUNC(I)
                     IF (ITRC1.GT.0) THEN
                        IF (ITRC1.GT.ITRC2) ITRUNC(I)=ITRC2
                        HT(I)=TOPH
                     ELSE
                        D=DBH(I)*BARK
                        IF (H.GE. 25 .AND. D.GE.6.0) THEN
                           AF=CFV(I)/(.00545415*D*D*H)
                           AF=.44244-(.99167/AF)-(1.43237*ALOG(AF))+
     >                        (1.68581*SQRT(AF))-(.13611*AF*AF)
                           DTK=FTKILL/H
                           DTK=(DTK/((AF*DTK)+(1.-AF)))*D

                           IF (DTK.GT. 4.0) THEN
                              ITRUNC(I)=ITRC2
                              NORMHT(I)=IFIX(H*100.+.5)
                              IMC(I)=3
                           ELSE
                              IF (DTK.GT. 2.0 .AND. IMC(I).LT.2)
     >                        IMC(I)=2
                           ENDIF
                        ENDIF
                        HT(I)=TOPH
                        IOD=ICR(I)
                        IF (IOD.GE.0) THEN
                           CN=(FLOAT(IOD)/100.*H)-H+TOPH
                           NEW=IFIX(CN/TOPH*100.+.5)
                           IF (NEW.LT.5) NEW=5
                           ICR(I)=-NEW
                        ENDIF
                     ENDIF
                  ENDIF
                  HTG(I)=0.0

                  IF (DEBUG) WRITE(JOSTND,*)
     &            'IN BWEPDM: -TOPKILL- I=',I,' ISP=',ISPI,' H=',H,
     &            ' HT=',HT(I),' NORMHT=',NORMHT(I),
     &            ' ITRUNC=',ITRUNC(I),' DBH=',DBH(I),' BARK=',BARK,
     &            ' PART=',PART,' FTKILL=',FTKILL 

               ENDIF
            ENDIF
C
C           MODIFY THE DIAMETER GROWTH.
C           FIRST CONVERT DG TO INSIDE BARK DELTA DIAMETER SQUARED.
C
            DBHI=DBH(I)
            DGI=DG(I)
            DDS=DGI*(2.0*BARK*DBHI+DGI)
C
C           REDUCE DDS, THEN CONVERT BACK TO DG.
C           DO NOT ALLOW CALCULATED GROWTH VALUE < 0.
C
            XD=BWERNP(PEDDS(IHOST,ISZI),.03)
            DDS=DDS*XD
            DG(I)=SQRT((DBHI*BARK)**2+DDS)-BARK*DBHI
            IF (DG(I) .LT. 0.0) DG(I)=0.0

            IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: DG(OLD)=',DGI,
     &                 '  DG(NEW)=',DG(I)

C
C           REDUCE HEIGHT GROWTH, UNLESS THE TREE HAS BEEN TOPKILLED AS
C           INDICATED BY HTG IF 0. DO NOT ALLOW CALCULATED GROWTH VALUE < 0.
C
            IF (HTG(I).GT.0.0) THEN
C
C              COMPUTE THE AMOUNT OF DEVIATION BASED ON THE PEDDS RANDOM
C              VARIABLE.
C
               IF (PEDDS(IHOST,ISZI).LT.0.99) THEN
                  XH=PEHTG(IHOST,ISZI)+((1.-PEHTG(IHOST,ISZI))/
     >            (1.-PEDDS(IHOST,ISZI))*(XD-PEDDS(IHOST,ISZI)))
               ELSE
                  XH=BWERNP(PEHTG(IHOST,ISZI),.03)
               ENDIF
               HTG(I)=HTG(I)*XH
               IF (HTG(I) .LT. 0.0) HTG(I)=0.0
            ENDIF

            IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: I=',I,' ISP=',ISP(I),
     &      ' DBH,DG=',DBH(I),DG(I),' HT,HTG=',HT(I),HTG(I)
C*************
C************* beginning of original mortality process
C*************

C
C           APPLY MORTALITY. BWMXCD HOLDS MAXIMUM CUM DEFOLIATION PERCENT
C           FOR THE PERIOD WITHIN THE CYCLE THAT THE OUTBREAK WAS IN
C           PROGRESS.  USE CROOKSTON'S MORTALITY MODEL (A SET OF IDEAS
C           THAT WERE PARTLY DUE TO READING ALFARO'S CAN. J. FOR. RES.
C           (12:780-787) PAPER.  LET PR BE THE BUDWORM MODEL MORTALITY
C           RATE AND BASE BE THE BASE MODEL RATE.  RBAL IS THE BASAL AREA
C           IN LARGER TREES DIVIDED BY THE BAMAX.  A VALUE OF 1.0 SAYS
C           A LOT OF BASAL AREA 'ABOVE' IS ABOVE A TREE, AND THE STAND AT
C           MAX FOR THE SITE.  A VALUE OF 0.0 SAYS THAT THE TREE HAS NO
C           BA ABOVE IT (A DOMINATE TREE), REGARDLESS OF THE STANDS DENSITY.
C
C           COMPUTE A MODIFER OF THE BASE MORTALITY FUNCTION AS A FUNCTION
C           OF RBAL AND THE TREE DIAMETER.
C
C           If bamax has not been calculated by FVS, calculate value
C           from sdimax (lrd 01-sep-00).
CX
CX          IF (BAMAX .EQ. 0.0) THEN
CX             TBAMAX = SDIMAX * 0.5454154
CX          ELSE
CX             TBAMAX = BAMAX
CX          ENDIF

CX          RBAL = (1.0 - (PCT(I) / 100.0)) * BA / TBAMAX
CX          IF (RBAL .GT. 1.0) RBAL = 1.0
CX          D = DBH(I)
CX          IF (D .GT. 20.0) D = 20.0
CX          XM = 0.02 * D + (0.5 + 0.03 * D)*(1.0 - RBAL)
CX          IF (XM .GT. 1.0) XM = 1.0
C
C           COMPUTE THE CONSTANTS OF THE MORTALITY MODEL AS FUNCTIONS OF XM.
C
CX          C = XM * XM
CX          B = 0.00575 - 0.00645 * XM + 0.0031 * C
CX          C = 2.27 + 2.94 * XM + 1.84 * C
C
C           COMPUTE AN UPPER ASYMPOTE AS A FUNCTION OF DIAMETER.
C
CX          ASYM = .98 - 0.01 * D
C
C           COMPUTE PR...SCALED TO A 1-YEAR SURVIVAL RATE. THE RANDOM DEFOL
C           PROCESS IS INSERTED HERE.  SCALE THE MAX CUM DEF TO A ONE YEAR
C           DEFOLIATION PROPORTION.  GET THE RANDOM VARIABLE AND SCALE IT
C           BACK TO A 5 YEAR CUM.
C
CX          XD = BWERNP(BWMXCD(IHOST,ISZI) * 0.002, 0.03) * 500.0
CX          PR = ASYM * (1.0 - EXP(-((B * (XD + 1.0))**C)))
CX          PR = (1.0 - PR) ** 0.1
C*************
C************* end of original mortality process
C*************

C*************
C************* beginning of new mortality process
C*************

C           The mortality equation implemented here is from Mike Marsden's
C           analysis of Bruce Hostetler's WSB impact study data. The results
C           were reported in a memo dated 23-JUL-2003 and interpreted by
C           Kathy Sheehan in January, 2003 for use in the budworm model
C
C           Set up final computed variables used in mortality equation.
C           Topkill category - integer value 0 (no tk) to 10 (total tk)
C
            IF (ITRUNC(I) .EQ. 0.0 .OR. NORMHT(I) .EQ. 0.0) THEN
               KTK = 0.0
            ELSE
               KTK = 10.0 - IFIX( 
     &              (REAL(ITRUNC(I))/REAL(NORMHT(I)) * 10.0) + 0.5)
            ENDIF

C
C           Compute probability of mortality.
C 
            PR = 1.0/(1.0 + EXP(
     &          B0(IHOST)+(B1(IHOST)*ELEV)+
     &         (B2(IHOST)*ELEV*ELEV)+(B3(IHOST)*PNTBA(ITRE(I)))+
     &         (B4(IHOST)*PNTHBA(ITRE(I)))+(B5(IHOST)*MFT)+
     &         (B6(IHOST)*KTK)+(B7(IHOST)*MFT*MFM)) )

            IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: KTK=',KTK,
     &      ' ITRUNC=',ITRUNC(I),' NORMHT=',NORMHT(I),' MFT=',MFT,
     &      ' MFM=',MFM,' PR=',PR
C*************
C************* end of new mortality process
C*************

C
C           BASE IS THE BASE MODEL MORTALITY RATE.  USE IT FOR THE PART OF
C           THE CYCLE WHEN BW WAS NOT ACTIVE.
C
C ?         This piece of code is part of the original mortality process
C ?         that was actually a survival rate. Since this is where accounting
C ?         for bw outbreak years within a FVS cycle takes place, it is a
C ?         necessary detail to keep. Question is, is this a correct way to
C ?         do it even if we convert the new mortality rate back to survival
C ?         rate for this little piece of code? 
C ?         LRD 21-APR-03
C
            BASE = WK2(I) / PROB(I)

C           translate the mortailty rate to a survival rate for 
C           following equations. 10/31/03
            PR = 1.0 - PR

            IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: BASE=',BASE,
     &      ' NOBWYR=',NOBWYR,' IBWYR=',IBWYR,' FA=',FA,' PR=',PR

            IF (NOBWYR .GT. 0) THEN
               PR = (PR ** IBWYR)*((1.0 - BASE) ** FA)
            ELSE
               PR = PR ** IBWYR
            ENDIF
            IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: PR=',PR

C*************
C           CONVERT SURVIVAL RATE TO MORTALITY RATE.
C
            PR=1.0-PR
C           The rate calculated by the new process is mortality,
C           not survival so no conversion is necessary. LRD 17-APR-03
C           This conversion reactivated 10/31/03. see conversion above
C*************

C
C           IF THE NUMBER DIEING IS LESS THAN THE BACK GROUND RATE FOR
C           THIS TREE RECORD, USE THE BACK GROUND RATE.
C
            FA=WK2(I)
            IF (BASE.GT.PR) THEN
               PR=BASE
            ELSE
               IF (PR.GT. 0.98) PR=0.98
               PR=PROB(I)*PR
               WK2(I)=PR
            ENDIF
C
C           WRITE DAMAGE OUTPUT, IF REQUESTED.
C
            IF (LBWPDM) WRITE (JOWSBW,46) I,NSP(ISPI,IMC(I)),DBH(I),
     >      ORGHT,PROB(I),FA,WK2(I),DGI,DG(I),HTGI,HTG(I),PRTOPK,
     >      FTKILL
   46       FORMAT (1X,I4,2X,A3,1X,2F7.2,3F8.3,5F7.3,F7.1)
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
C
C     COMPUTE AVERAGE DEFOLIATION WEIGHTED BY TREES PER
C     HECTARE, AND PASS IT BACK TO THE EVENT MONITOR.
C
      STREES=0.
      SDEF=0.

      DO 90 IHOST=1,6
         IF (IFHOST(IHOST).NE.1) GOTO 90
         DO 80 ITREE=1,3
            STREES=STREES+BWTPHA(IHOST,ITREE)
            SDEF=SDEF+CDEF(IHOST,ITREE)*BWTPHA(IHOST,ITREE)
   80    CONTINUE
   90 CONTINUE
      IF (STREES.GT.0.0001) THEN
         SDEF=SDEF/STREES
      ELSE
         SDEF=0.0
      ENDIF

      IF (DEBUG) WRITE(JOSTND,*) 'IN BWEPDM: CALL EVSET4; SDEF=',SDEF
      CALL EVSET4 (5,SDEF)
C
C     RETRIEVE THE DAMAGE MODEL RANDOM NUMBER SEED FROM THE GENERATOR.
C
      CALL BWERGT (DSEEDD)

      IF (DEBUG) WRITE (JOSTND,*) 'EXIT BWEPDM: ICYC= ',ICYC

      RETURN
      END
