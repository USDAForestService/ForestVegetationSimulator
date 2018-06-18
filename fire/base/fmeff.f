      SUBROUTINE FMEFF (IYR, FM, FLAME, ICALL, POMORT, PVOLKL, MKODE,
     >                  PSBURN)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*                  FMPOFL
*     CALLS:   FMBRKT
*              FMSSEE
*              FMSCRO
*              FMSADD
*              FMSVTOBJ
*              SVMORT
*  PURPOSE:
*     THIS SUBROUTINE CALCULATES THE PROBABILITY OF TREE MORTALITY.
*     FROM SIMPLIFIED INFORMATION SENT BY E. REINHARDT
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     FM:      FUEL MODEL
*     FLAME:   FLAME LENGTH (FT)
*     ICALL:   WHERE IS THIS CALLED FROM? (0=FMBURN, 1=FMPOFL)
*     POMORT:  POTENTIAL MORTALITY (AS PROP OF BASAL AREA)
*     PVOLKL:  VOLUME KILLED.
*     MKODE:   MORTALITY CODE (0=TURN OFF FFE MORTALITY, 
*                              1=FFE ESTIMATES MORTALITY)
*     PSBURN:  PERCENTAGE OF STAND THAT IS BURNED 
*
*  LOCAL VARIABLE DEFINITIONS:
*     CLSP:    PROPORTION OF CROWN LENGTH SCORCHED
*     CRBNL:   CRown BurN Length - the length of crown that was below
*              the flame height
*     CRW1BN:  Weight of CRown component 1 (0-0.25") that was BurNed
*     CSV:     CROWN SCORCH VOLUME
*     CRL:     CROWN LENGTH (FT)
*     DTHISC:  Density of snags with THIS Crown size (depends on what
*              happened to different portions of the tree record during
*              the fire).
*     PMORT:   PROBABILITY (PROPORTION) OF MORTALITY
*     PROPCR:  PROPortion of the CRown to burn
*     SL:      SCORCH LENGTH (FT)
*     TCROWN:  Temporary CROWN weights array
*     TOLDCR:  Temporary OLD CRown weights array (material dead from
*              crown lifting)
*     YRSCYC:  YeaRS left in current FVS CYCle (including current year)
*     BCROWN:  Burned crown material
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*     SCH:   SCORCH HEIGHT (IN FEET)
*
***********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.
Cppe  INCLUDE 'PPEPRM.F77'

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

Cppe  INCLUDE 'PPCNTL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C.... VARIABLE DECLARATIONS.

      LOGICAL   DEBUG, LPSBURN
      INTEGER   I, FM, KSP, ICALL, IYR, ISZ, IDC, ITM, MORTGP, MKODE
      INTEGER   P, H, D
      REAL      CRW1BN, CRBNL, CRL, CSV, DTHISC, PROPCR, TCROWN
      REAL      BAMORT, TOTBA, POMORT,TOLDCR, PMORT, YRSCYC
      REAL      PVOLKL, FLAME, BCR, SL, CLSP, XM, FMBRKT, CRBOT
      REAL      BCROWN, MORTB0(5), MORTB1(5), MORTB2(5)
      REAL      CHARHT, MNMORT,PSBURN, XRAN
      DOUBLE PRECISION SAVESO
      DIMENSION TCROWN(0:5)
      DIMENSION TOLDCR(0:5)

      
C     COEFFICIENTS FOR SOME SN MORTALITY EQUATIONS

      DATA     MORTB0 / 1.0229, 0.1683, 1.2165, 0.8221, 2.775 /
      DATA     MORTB1 / -0.2646, -0.1332, -0.4758, -0.4098, -1.1224 /
      DATA     MORTB2 / 2.6232, 3.4152, 6.0415, 8.4682, 2.8312 /
      
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMEFF',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,FM,FLAME,CRBURN,ICALL
    7 FORMAT(' ENTERING FMEFF CYCLE = ',I2,
     >       ' IYR=',I5,' FM=',I3,' FLAME=',F14.4,
     >       ' CRBURN=',F10.4,' ICALL=',I2)

C.... BEGIN ROUTINE

Cppe  YRSCYC = FLOAT( MIY(MICYC)-IYR )
Csng  YRSCYC = FLOAT( IY(ICYC+1)-IYR )
      YRSCYC = FLOAT( IY(ICYC+1)-IYR )
      
      BAMORT = 0.0
      TOTBA  = 0.0
      POMORT = 0.0
      PVOLKL = 0.0

C
C     Burn the entire crown of pre-existing snags caught in the area with
C     crown fire (if crown fire occurred).  This is done here so that
C     CWD2B2 will not contain material from trees killed in the current
C     fire (only from fires earlier in this FVS cycle).
C     Note, because we want to get potential smoke production, we now need to
C     also calculate the potential amount of crowns burned (PBRNCR)
C
      BCROWN = 0
      IF (CRBURN .GT. 0.0) THEN
        DO ISZ = 0,5
          DO IDC = 1,4
            DO ITM = 1,TFMAX
              BCR = CRBURN * CWD2B(IDC,ISZ,ITM)*PSBURN/100
              IF (ICALL .EQ. 0)
     &          CWD2B(IDC,ISZ,ITM) = CWD2B(IDC,ISZ,ITM) - BCR
              BCROWN = BCROWN + BCR * P2T
              BCR = CRBURN * CWD2B2(IDC,ISZ,ITM)*PSBURN/100
              IF (ICALL .EQ. 0)
     &          CWD2B2(IDC,ISZ,ITM) = CWD2B2(IDC,ISZ,ITM) - BCR
              BCROWN = BCROWN + BCR * P2T

              IF (DEBUG) WRITE (JOSTND,10)ISZ,IDC,ITM,
     >          CWD2B(IDC,ISZ,ITM),CWD2B2(IDC,ISZ,ITM)
   10         FORMAT(' FMEFF: ISZ,IDC,ITM=',3I3,' CWD2B=',F12.2,
     >          ' CWD2B2=',F12.2)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
C     NOW LOOK AT WHAT HAPPENS TO THE LIVE TREES.
C
      CALL RANNGET(SAVESO)
      DO 100 I = 1,ITRN

C       CALL A RANDOM NUMBER AND COMPARE IT TO THE % OF THE STAND AREA
C       BURNED (PSBURN) TO SEE IF WE ARE IN THE BURNED PORTION OR NOT.
C       IF WE ARE IN THE UNBURNED PORTION, SKIP LOTS - MORTALITY IS 0.
C
        PMORT = 0.0
        LPSBURN = .TRUE.
        CALL RANN(XRAN)
      
        IF (DEBUG) WRITE(JOSTND,12) IYR,I,XRAN,ISP(I),DBH(I)
   12   FORMAT(' FMEFF YEAR = ',I5,
     >       ' I=',I5,' XRAN=',F6.4,' KSP=',I4,' DBH=',F6.2) 

        XRAN = XRAN*100
        IF (XRAN .GT. PSBURN) THEN
          LPSBURN = .FALSE.
          GOTO 90     
        ENDIF
        
        KSP = ISP(I)
C
C       COMPUTE SCORCH LENGTH
C       CRL = CROWN LENGTH
C       SL = SCORCH LENGTH
C
        CRL = HT(I) * (FLOAT(FMICR(I)) / 100.0)
        SL  = SCH - (HT(I) - CRL)

        IF (SL .LT. 0.0) SL = 0.0
        IF (SL .GT. CRL) SL = CRL
        
        IF ((FMPROB(I) .GT. 0.0) .AND. (MKODE .NE. 0)) THEN
C
C         PERCENT OF CROWN LENGTH SCORCHED & % CROWN VOLUME SCORCHED
C
          IF (CRL .GT. 0.0) THEN
            CLSP = 100.0 * (SL / CRL)
            IF (CLSP .GT. 100.0) CLSP = 100.0
            CSV = 100.0 * (SL * (2.0 * CRL - SL) / (CRL * CRL))
          ELSE
            CSV = 100.0
          ENDIF

          XM = EXP(-1.941 + 6.316 * (1 - EXP(-FMBRKT(DBH(I),KSP)))
     &          - .000535 * CSV * CSV)
     
          PMORT = (1.0 / (1.0 + XM))
  
C         ADJUST MORTALITY FOR SOME SOUTHERN SPECIES BASED ON
C         EQUATIONS IN REGELBRUGGE AND SMITH (NJAF 11(3) 1994)
       
          IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'CS')) THEN
            
C           THESE EQUATIONS USE MAX (UPHILL) CHAR HT.  OPINION AT 
C           MISSOULA FIRE LAB IS THAT DOWNHILL CHAR HT IS 70% OF FLAME
C           LENGTH AND THAT UPHILL CHAR HT APPROX. EQUALS FLAME LENGTH.
C           HOWEVER, AN ARTICLE BY MICHAEL CAIN IN FIRE MANAGEMENT NOTES
C           SUGGESTS THAT MAX CHAR HT IS 50-65% OF FLAME LENGTH.  I 
C           WENT IN BETWEEN WITH THE ASSUMPTION BELOW.
           
            CHARHT = FLAME*.7
            
            IF (VARACD .EQ. 'SN') THEN
              SELECT CASE (KSP)
                CASE (63,74)      ! white oak and chestnut oak
                  MORTGP = 1
                CASE (64,75,78)   ! scarlet, black, and northern red oak
                  MORTGP = 2
                CASE (27)         ! hickory
                  MORTGP = 3
                CASE (20)         ! red maple
                  MORTGP = 4
                CASE (54)         ! black gum
                  MORTGP = 5
                CASE DEFAULT         
                  MORTGP = 6               
              END SELECT  
            ELSE    !VARACD = cs
              SELECT CASE (KSP)
                CASE (47,59)      ! white oak and chestnut oak
                  MORTGP = 1
                CASE (48:51)      ! scarlet, black, and northern/southern red oak
                  MORTGP = 2
                CASE (14:23)      ! hickories
                  MORTGP = 3
                CASE (29)         ! red maple
                  MORTGP = 4
                CASE (11,13)      ! black and swamp tupelo
                  MORTGP = 5
                CASE DEFAULT         
                  MORTGP = 6               
              END SELECT            
            ENDIF
            
            SELECT CASE (MORTGP)
              CASE (1:5)
                XM = -1*(MORTB0(MORTGP) + 
     &                  MORTB1(MORTGP)*DBH(I)*2.54 + 
     &                  MORTB2(MORTGP)*CHARHT/3.28)
             ! see whether XM is so large that pmort will be less
             ! than .000001 and taking the exp of xm may cause a blowup 
                MNMORT = ALOG(1/.000001 - 1)       
                IF (XM .GE. MNMORT) THEN
                  PMORT = 0
                ELSE
                  XM = EXP(XM)
                  PMORT = (1.0 /(1.0 + XM)) 
                ENDIF           
              CASE (6)
                PMORT = PMORT  ! use old estimate from FOFEM
            END SELECT
          ENDIF
          
C
C         ENGELMANN SPRUCE HAS MINIMUM MORTALITY OF 0.8
C
          SELECT CASE (VARACD)
            CASE ('IE','EM','KT')
              IF (KSP .EQ. 8) PMORT = MAX(0.8, PMORT)
            CASE DEFAULT
              PMORT = PMORT
          END SELECT
C
C         MAKE SOME ADJUSTMENTS FOR THE LAKE STATES FFE 
C         CONIFERS GET THEIR MORTALITY REDUCED BY HALF IF 
C         THE BURN WAS BEFORE GREENUP.
C         BALSAM FIR GETS A MINIMUM MORTALITY OF 70%
C         ALL MAPLES UNDER 4" DIE.
C         HARDWOODS GET THEIR MORTALITY REDUCED TO 80% IF THE BURN WAS 
C         BEFORE GREENUP.  OAKS 2.5"+ GET THEIR MORTALITY REDUCED BY 
C         HALF BEFORE GREENUP SINCE THEY ARE ESPECIALLY RESISTANT.
C         ALL HARDWOODS LESS THAN 1" DIE
C
          IF (VARACD .EQ. 'LS') THEN
            IF ((BURNSEAS .LE. 2) .AND. (KSP .LE. 14)) PMORT = PMORT/2 
            IF (KSP .EQ. 8) PMORT = MAX(0.7, PMORT)            
            SELECT CASE (KSP)
              CASE (18,19,26,27,51,52)
                IF (DBH(I) .LT. 4) PMORT = 1.0
              CASE DEFAULT
                PMORT = PMORT
            END SELECT           
            IF ((BURNSEAS .LE. 2) .AND. (KSP .GT. 14)) THEN
              SELECT CASE (KSP)
                CASE (30:36)
                  IF (DBH(I) .GE. 2.5) THEN
                    PMORT=PMORT/2  
                  ELSE
                    PMORT=PMORT*0.8  
                  ENDIF
                CASE DEFAULT
                  PMORT=PMORT*0.8      
              END SELECT
            ENDIF
            IF ((KSP .GT. 14) .AND. (DBH(I) .LE. 1)) PMORT = 1.0 
          ENDIF
C
C         MAKE THE SAME ADJUSTMENTS FOR THE NE VARIANT
C
          IF (VARACD .EQ. 'NE') THEN
            IF ((BURNSEAS .LE. 2) .AND. (KSP .LE. 25)) PMORT = PMORT/2 
            IF (KSP .EQ. 1) PMORT = MAX(0.7, PMORT)            
            SELECT CASE (KSP)
              CASE (26:29,99:100)
                IF (DBH(I) .LT. 4) PMORT = 1.0
              CASE DEFAULT
                PMORT = PMORT
            END SELECT           
            IF ((BURNSEAS .LE. 2) .AND. (KSP .GT. 25)) THEN
              SELECT CASE (KSP)
                CASE (55:70,89)
                  IF (DBH(I) .GE. 2.5) THEN
                    PMORT=PMORT/2  
                  ELSE
                    PMORT=PMORT*0.8  
                  ENDIF
                CASE DEFAULT
                  PMORT=PMORT*0.8      
              END SELECT
            ENDIF
            IF ((KSP .GT. 25) .AND. (DBH(I) .LE. 1)) PMORT = 1.0 
          ENDIF
C
C          ADJUST MORTALITY FOR TREES WITH DBH < 1
C
          IF (DBH(I) .LE. 1.0 .AND. CSV .GT. 50.0) PMORT = 1.0

        ENDIF

      IF (DEBUG) WRITE(JOSTND,15) ICYC,IYR,PSBURN
   15 FORMAT(' ENTERING FMEFF CYCLE = ',I2,
     >       ' IYR=',I5,' PSBURN=',F6.1)
     
C       MODIFY PMORT BY THE MULTIPLIER

        PMORT = PMORT*FMORTMLT(I)
        
        IF (PMORT .GT. 1.0) PMORT = 1.0
        IF (PMORT .LT. 0.0) PMORT = 0.0
C
C        DETERMINE THE FATE OF CROWN MATERIAL. THIS CAN BE SKIPPED IF THERE
C        WEREN'T ANY TREES IN THE RECORD TO START WITH. MOST LINES CAN ALSO BE
C        SKIPPED IF WE ARE JUST WANTING TO GET POTENTIAL INFORMATION.
C
        IF (FMPROB(I) .LE. 0.0) GOTO 90
C
C       FIRST, CONSIDER THE PORTION OF THE STAND WITH CROWN FIRE (=CRBURN).
C
        IF (CRBURN .LE. 0.0 .OR. MKODE .EQ. 0) GOTO 40
C
C       USE TCROWN TO REMEMBER THE CROWN WEIGHTS OF THE TREES THAT
C       ESCAPED THE CROWN FIRE:
C
        TCROWN(0) = CROWNW(I,0)
        TCROWN(1) = CROWNW(I,1)
        TOLDCR(1) = OLDCRW(I,1)
C
C       BURN 100% OF THE FOLIAGE IN THIS PART OF THE STAND, AND 50%
C       OF THE 0-0.25" BRANCHES
C
C       NOTE1: CONVERT CROWNW TO TONS TO MATCH BURNCR & PBRNCR
C       NOTE2: THE CROWNW REDUCTION HERE IS JUST TEMPORARY, TO MAKE
C              FMSCRO USE THE RIGHT WEIGHTS
C       NOTE3: 0-0.25" BRANCHES INCLUDE OLDCRW MATERIAL, WHICH IS
C
C       KEPT AS THE AMOUNT SCHEDULED TO FALL IN EACH REMAINING YEAR OF
C       THE FVS CYCLE:
C
        BCROWN = BCROWN + CRBURN * FMPROB(I) * P2T * CROWNW(I,0)
        BCROWN = BCROWN + 0.5 * CRBURN * FMPROB(I) * P2T *
     &           (CROWNW(I,1) + YRSCYC * OLDCRW(I,1))

        IF (ICALL .EQ. 0) THEN
          CROWNW(I,0) = 0
          CROWNW(I,1) = 0.5 * CROWNW(I,1)
          OLDCRW(I,1) = 0.5 * OLDCRW(I,1)
C
C         ALL TREES INVOLVED IN THE CROWN FIRE WILL DIE, AND THEIR SNAGS
C         WILL HAVE THE CROWN WEIGHTS JUST DETERMINED.  CALL FMSCRO TO
C         PUT THEIR CROWNS INTO CWD2B2/CWD2B TO FALL AT APPROPRIATE TIMES IN
C         THE FUTURE (NOTE:  THE TIMES WILL NOT *REALLY* BE APPROPRIATE
C         IF PBTIME < THE NORMAL SNAG CANOPY LIFESPAN FOR THIS SPECIES,
C         BUT THIS TIMING IS NOT CRITICAL TO MODEL BEHAVIOUR).
C
          DTHISC = FMPROB(I) * CRBURN
          CALL FMSCRO(I,ISP(I),IYR,DTHISC,1)
C
C         RE-SET THE CROWN WEIGHTS OF TREES THAT ESCAPED CROWN FIRE:
C
          CROWNW(I,0) = TCROWN(0)
          CROWNW(I,1) = TCROWN(1)
          OLDCRW(I,1) = TOLDCR(1)
        ENDIF

   40   CONTINUE

        IF (CRBURN .GE. 1.0) GOTO 90
C
C        NOW CONSIDER THE PORTION OF THE STAND WITHOUT CROWN FIRE.  IF THE
C        TREES IN THIS RECORD HAD PART OF THEIR CROWN BELOW THE FLAME HEIGHT,
C        THEIR CROWNS SHOULD BE PARTIALLY BURNT.  CHANGE THE REAL CROWNW
C        WEIGHTS FOR THIS, BECAUSE IT AFFECTS BOTH THE STILL-LIVING AND THE
C        KILLED TREES.
         
C        FIRST, SAVE THE CROWN WEIGHTS.
         DO ISZ = 0, 5
           TCROWN(ISZ) = CROWNW(I,ISZ)
           TOLDCR(ISZ) = OLDCRW(I,ISZ)           
         ENDDO

C        CHANGED FROM FLAME HEIGHT TO SCORCH HEIGHT (SB & ER: 2/97)
C
         CRBOT = HT(I) - CRL

         IF (SCH .GT. CRBOT) THEN
C
C          THE AMOUNT OF CROWN TO BURN IS THE PROPORTION OF BURNED CROWN
C          LENGTH TO TOTAL CROWN LENGTH.  THIS PROPORTION IS EQUAL TO BOTH
C          THE VOLUME PROPORTION AND SURFACE-AREA PROPORTION THAT WOULD HAVE
C          BEEN BURNED IF THE CROWN WAS CYLINDRICAL.  THIS CYCLINDRICAL
C          APPROXIMATION SHOULD CAUSE MINIMAL ERROR BECAUSE:  FLAME HEIGHTS
C          ARE GENERALLY < 10 FEET, AND LARGE TREES WON'T HAVE ANY CROWN THAT
C          LOW, WHILE SMALL TREES WILL CONTRIBUTE LITTLE WEIGHT OF BURNED
C          MATERIAL EVEN IF WE DO BURN TOO MUCH, AND MEDIUM TREES MAY BE
C          ROUGHLY CYLINDRICAL OVER MUCH OF THEIR TOTAL CROWN LENGTH.
C
           CRBNL = SCH - CRBOT

           IF (CRBNL .GT. CRL) CRBNL = CRL
           PROPCR = 0.0
           IF (CRL.GT.0.) PROPCR = CRBNL / CRL
C
C          AS FOR CROWN FIRES, BURN ALL FOLIAGE REACHED BY THE FIRE, AND
C          HALF THE 0-0.25" MATERIAL.  CRW1BN IS USED BECAUSE IT'S NEEDED
C          LATER.  FOR OLDCRW MATERIAL (WHICH IS DEAD MATERIAL BELOW THE
C          BOTTOM OF THE CURRENT CROWN), JUST ASSUME THAT ALL OF IT WAS
C          IN WITHIN REACH OF THE FIRE.
C
           CRW1BN = 0.5 * PROPCR * CROWNW(I,1)
           
           IF (MKODE .NE. 0) THEN
             BCROWN = BCROWN + (1.0 - CRBURN) * FMPROB(I) * P2T *
     >              CROWNW(I,0) * PROPCR 
             BCROWN = BCROWN + (CRW1BN + 0.5*YRSCYC*OLDCRW(I,1))
     >              * (1.0 - CRBURN) * FMPROB(I) * P2T 
           ELSE ! MKODE .EQ. 0
             BCROWN = BCROWN + (1.0) * FMPROB(I) * P2T *
     >              CROWNW(I,0) * PROPCR 
             BCROWN = BCROWN + (CRW1BN + 0.5*YRSCYC*OLDCRW(I,1))
     >              * (1.0) * FMPROB(I) * P2T 
           ENDIF

           IF (ICALL .EQ. 0) THEN
             CROWNW(I,0) = CROWNW(I,0) - PROPCR * CROWNW(I,0)
             CROWNW(I,1) = CROWNW(I,1) - CRW1BN
             OLDCRW(I,1) = 0.5 * OLDCRW(I,1)
C
C            NOW CALL **FMSCRO** TO POOL THE CROWNS OF THE FIRE-KILLED TREES:
C
             DTHISC = (1.0 - CRBURN) * PMORT * FMPROB(I)
             CALL FMSCRO(I,ISP(I),IYR,DTHISC,1)
C
C            THE PARTS OF THE CROWN OF STILL-LIVING TREES THAT WERE TOO
C            LARGE TO BURN BUT WERE WITHIN THE FLAME HEIGHT ARE ALSO DEAD
C            AND SHOULD ALSO BE PUT IN CWD2B2/CWD2B POOLS. **FMSCRO** CAN DO THIS
C            IF YOU PASS IT ONLY THE WEIGHTS OF THE KILLED CROWN MATERIAL.
C            REMEMBER THAT HALF OF THE 0-0.25" KILLED MATERIAL HAS BEEN
C            CONSUMED.
C            
             DO ISZ = 0, 5
               IF (ISZ .EQ. 0) CROWNW(I,ISZ) = 0
               IF (ISZ .EQ. 1) THEN
                 CROWNW(I,ISZ) = PROPCR *(CROWNW(I,ISZ)+CRW1BN)
     >                - CRW1BN
               ELSE
                 CROWNW(I,ISZ) = PROPCR * CROWNW(I,ISZ)
               END IF
               OLDCRW(I,ISZ) = 0.0
             ENDDO
             
             IF (MKODE .NE. 0) THEN
               DTHISC = ((1.0-CRBURN) - (1-CRBURN)*PMORT) 
     &                 * FMPROB(I)
             ELSE ! MKODE .EQ. 0
               DTHISC = FMPROB(I)
             ENDIF
             CALL FMSCRO(I,ISP(I),IYR,DTHISC,1)  
C
C            RE-SET CROWNW TO BE THE ORIGINAL WEIGHTS, MINUS THE WEIGHT 
C            OF CONSUMED AND FIRE-KILLED MATERIAL.  
C
             DO ISZ = 0,5
               CROWNW(I,ISZ) = TCROWN(ISZ)*(1-PROPCR)
               IF (ISZ .EQ. 1) THEN
                 OLDCRW(I,ISZ) = TOLDCR(ISZ)*0.5
               ELSE
                 OLDCRW(I,ISZ) = TOLDCR(ISZ)
               ENDIF
             ENDDO
C
C            SET GROW = -1 SO THAT THE FULL CROWN WEIGHT WON'T BE RESTORED
C            UNTIL THE START OF THE 2ND TIME FROM NOW THAT A NEW FVS CYCLE
C            STARTS.
C
             GROW(I) = -1
C
C            SET THE NEW FIRE MODEL VERSION OF CROWN LENGTH.  
C
             FMICR(I) = IFIX(100.0 * (CRL - CRBNL) / HT(I))

           ENDIF
C
C         SCORCHING DOESN'T AFFECT THE CROWN IN THIS CASE. IF THESE TREES
C         DIDN'T HAVE PART OF THEIR CROWN BELOW THE FLAME HEIGHT, AND IF
C         THIS IS CALLED WHEN ACTUALLY APPLYING THE BURN, ALL YOU HAVE TO
C         DO IS CALL **FMSCRO** FOR THE DEAD TREES.
C
         ELSEIF (ICALL .EQ. 0) THEN

           DTHISC = (1.0 - CRBURN) * PMORT * FMPROB(I)
           CALL FMSCRO(I,ISP(I),IYR,DTHISC,1)

         ENDIF
C
C        THIS CONCLUDES THE DETERMINATION OF THE FATE OF CROWN MATERIAL.
C
   90    CONTINUE
C
C        **********
C
C        DETERMINE HOW MANY TREES DIE. WE ASSUME THAT PMORT IS THE
C        PROPORTION OF TREES IN THE RECORD WHICH DIE (RATHER THAN THE
C        PROBABILITY THAT ALL THE TREES IN THE RECORD DIE). ALSO,
C        CROWNING KILLS SOME ADDITIONAL TREES. LET CROWNING ACT ON THE
C        TREES REMAINING AFTER PMORT KILLS THE TREES.
C
         IF (ICALL .EQ. 0) THEN
           CURKIL(I) = PMORT * FMPROB(I)
           IF (MKODE .NE. 0 .AND. LPSBURN) THEN
             CURKIL(I) = CURKIL(I) + CRBURN * (FMPROB(I) - CURKIL(I))
           ENDIF
           FIRKIL(I) = FIRKIL(I) + CURKIL(I)
           FMPROB(I) = FMPROB(I) - CURKIL(I)
           IF (FMPROB(I) .LT. 0.0) FMPROB(I) = 0.0
C
C           STORE THE FIRE-CAUSED MORTALITY IN THE SNAG MGMT ROUTINES FOR
C           ADDITION TO THE SNAG POOLS.  R&C 07/11/96
C
           CALL FMSSEE (I,ISP(I),DBH(I),HT(I),CURKIL(I),1,DEBUG,JOSTND)

         ELSEIF (ICALL .NE. 0) THEN
           POMORT = PMORT * FMPROB(I)
           IF (LPSBURN) POMORT = POMORT + CRBURN * (FMPROB(I) - POMORT)
           BAMORT = BAMORT + POMORT * (DBH(I) / 24.0)**2
           TOTBA = TOTBA + FMPROB(I) * (DBH(I) / 24.0)**2
           PVOLKL = PVOLKL + (CFV(I) * POMORT)
         ENDIF

  100 CONTINUE
      CALL RANNPUT(SAVESO)
C
C     IF THIS CALL IS FROM FMBURN (A FIRE), THEN REPORT THE MORTAILITY
C     TO THE SV ROUTINES AND WRITE THE REPORT(S).
C
      IF (ICALL .EQ. 0) THEN
C
C       CREATE A TEMPORARY COPY OF THE OBJECT LIST, THEN
C       CALL SVMORT TO PROCESS THE NEW FIRE MORTALITIES AND OUTPUT THEM.
C
        CALL FMSVTOBJ(FIRTYPE)
        CALL SVMORT(1,FIRKIL,IYR)
C
C       MAKE ANOTHER COMPRESSED COPY OF CWD TO GIVE TO THE SVS ROUTINES.
C
        DO P=1,6
          TCWD2(P) = 0.0
        ENDDO
        DO P=1,2
          DO H=1,2
            DO D=1,4
              TCWD2(1)=TCWD2(1)+CWD(P,10,H,D)
              TCWD2(2)=TCWD2(2)+CWD(P,11,H,D)
              TCWD2(3)=TCWD2(3)+CWD(P,1,H,D)+CWD(P,2,H,D)+CWD(P,3,H,D)
              TCWD2(4)=TCWD2(4)+CWD(P,4,H,D)
              TCWD2(5)=TCWD2(5)+CWD(P,5,H,D)
              TCWD2(6)=TCWD2(6)+CWD(P,6,H,D)+CWD(P,7,H,D)+CWD(P,8,H,D)
     &                         +CWD(P,9,H,D)
            ENDDO
          ENDDO
        ENDDO
        CALL FMSVOUT(IYR,FLAME,FIRTYPE)
      ENDIF

      IF (ICALL .NE. 0 .AND. TOTBA .NE. 0) POMORT = BAMORT / TOTBA
C
C     STORE THE FIRE-CAUSED MORTALITY IN THE SNAG POOLS. R&C 07/11/96
C
      IF (ICALL .EQ. 0) THEN
        CALL FMSADD (IYR,1)
        BURNCR = BCROWN
      ELSE
        PBRNCR = BCROWN
      ENDIF

      RETURN
      END

