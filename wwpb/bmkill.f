      SUBROUTINE BMKILL
C----------
C  **BMKILL               DATE OF LAST REVISION:   12/08/05
C----------
C
C     CONVERT THE MORTALITY ESTIMATES PRODUCED BY THE PINE
C     BEETLE MODEL INTO MODIFIED PROGNOSIS MODEL RATES.
C
C     AN EXTENSION OF THE PARALLEL PROCESSING EXTENSION (PROGNOSIS)
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--MAY 1987
C
C     CALLED FROM:  PPMAIN
C
C Revision History:
C   08/05/98 Robert N. Havis (FHTET)
C      Modified to track Salvage removals in PPE (RNH)
C   07/19/05 Lance R. David (FHTET)
C      Added call to SVMORT and SVOUT for generation of Stand Visualization
C      images.
C   09/22/05 Andrew McMahan (FHTET)
C      Changed variable name used in OCVREM calculation at end of routine.
C      This is in parallel with new output generation, which "stole" variable
C      "VOLREM" (heretofore used herein).  We need volume removed over the 
C      course of the cycle, now kept in CVOLREM.
C   09/30/05 Lance R. David (FHTET)
C      Added BTKL array to hold host mortality for call to svmort.
C   11/08/05 Lance R. David (FHTET)
C      Changed how mortality is submitted to SVS when WWPB mortality does not
C      exceed FVS base mortality, enabling WWPB to get partial credit in SVS
C      images. Added new local variable BASMRT for this process.
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'OUTCOM.F77'

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BMCOM.F77'
C
COMMONS
C
      LOGICAL LX                 
      INTEGER IBMSTD
      INTEGER K
      INTEGER ISPC         
      INTEGER UPAGE(MXDWAG)
      INTEGER IAG, ISC, IPC, JSC
C  ** DECLARED DIF AS INTEGER (MJOMarch98), parameter array NRSC (RNHAug98)
      INTEGER DIF
      REAL    MFAST, MSLOW, MPRG, MBTL, MSUM 
      REAL    MEX, SMX, DMX
      REAL    X, BTKL(MAXTRE), BASMRT
      
      DATA UPAGE /0,1,5,10,300/
C
C     IF BMSTND IS ZERO, NO DISPERSAL WAS RUN, BRANCH TO EXIT.
C     Change test to use IBMYR1 instead of BMSTND.  ajm dec 2005
C
       IF(IBMYR1 .EQ. 0) GO TO 150
C      IF (BMSTND .LE. 0) GOTO 150  
C
C     FIND OUT IF THE CURRENT STAND IS IN THE DISPERSAL GROUP
C
      IBMSTD= 0 
      CALL OPBISR (BMSTND,BMSDIX,ISTND,IBMSTD) 
c
C     ajm i don't see how, if we got to here, a stand could NOT be in the dispersal group.
c     all stands in ppe run are in dispersal group!.
c     unless it is marked nonstocked, in which case this call will not help us.
C     

C     IF IBMSTD IS ZERO, THEN BRANCH TO EXIT.
C     OR, IF IBMSTD IS NON-STOCKED! (NEW CONDITION ADDED 12/05.  AJM)
C
C      IF (IBMSTD .LE. 0) GOTO 150 
      IF (IBMSTD .LE. 0 .OR. (.NOT.STOCK(IBMSTD))) GOTO 150 

C     For Stand Visualization process, initialize array that will hold
C     host tree mortaility.
C
      DO I = 1, MAXTREE
         BTKL(I)= 0.0
      ENDDO

      DO 20 ISPC= 1,MAXSP
        IF (ISCT(ISPC,1) .EQ. 0) GOTO 20

c       Determine whether tree species is host or non-host so mortality will be applied
c       appropriately.
 
        LX= .FALSE.
        IF (  (((PBSPEC .EQ. 1) .OR. (PBSPEC .EQ. 4))
     >         .AND. (HSPEC(1,ISPC) .EQ. 1)) 
     >   .OR. (((PBSPEC .EQ. 2) .OR. (PBSPEC .EQ. 4))
     >         .AND. (HSPEC(2,ISPC) .EQ. 1))
     >   .OR. ((PBSPEC .EQ. 2) .AND. (HSPEC(2,ISPC) .EQ. 1))
     >   .OR. ((PBSPEC .EQ. 3) .AND. (HSPEC(3,ISPC) .EQ. 1)) )
     >        LX= .TRUE.  
     

c     Fetch original TPA for each size class, and compute mortality for size class based on
c     the DEAD/LIVE ratio. Note that while not likely, MSUM could be greater than PROB().

        DO 10 II=ISCT(ISPC,1),ISCT(ISPC,2)
        
          I= IND1(II)              
C
          CALL BMDBHC(DBH(I),K)   
                                                 
          IF (LX) THEN
             IF (OTPA(IBMSTD,K,1) .GT. 1.0E-9) THEN
                X= 1.0 / OTPA(IBMSTD,K,1)
             ELSE
                X= 1.0E9
             ENDIF
                 
             MFAST= AMIN1(TPBK(IBMSTD,K,1,1) * X, 1.0) * PROB(I)
             MSLOW= AMIN1(TPBK(IBMSTD,K,1,2) * X, 1.0) * PROB(I)
             MBTL=  AMIN1(TPBK(IBMSTD,K,1,3) * X, 1.0) * PROB(I)

          ELSE
             IF (OTPA(IBMSTD,K,2) .GT. 1.0E-9) THEN
                X= 1.0 / OTPA(IBMSTD,K,2)
             ELSE
                X= 1.0E9
             ENDIF
                 
             MFAST= AMIN1(TPBK(IBMSTD,K,2,1) * X, 1.0) * PROB(I)
             MSLOW= AMIN1(TPBK(IBMSTD,K,2,2) * X, 1.0) * PROB(I)
             MBTL=  AMIN1(TPBK(IBMSTD,K,2,3) * X, 1.0) * PROB(I)
          ENDIF
           
          MSUM= MFAST + MSLOW + MBTL
          MPRG= WK2(I)
          BASMRT = WK2(I)
          
c         If the total model predicted mortality from all sources is less
c         than Prognosis predicts, nothing is done and this IF/ENDIF is
c         bypassed. If model-predicted mortality is greater, reconciliation
c         is done.

          IF (MSUM .GT. MPRG) THEN
          
c           IF fast and beetle mortality are less than Prognosis predicts,
c           adjust slow mortality so that the total agrees with Prognosis.
c           ELSE, adjust Prognosis mortality with fast and beetle 
c           mortality, and ignore slow altogether.

            IF ((MFAST + MBTL) .LT. MPRG) THEN 
              MSLOW= MSLOW - (MSUM - MPRG)
            ELSE
              MPRG= MFAST + MBTL
            ENDIF
                      
          ELSE
            
c           If Prognosis predicts higher mortality then the extra mortality needs to
c           be added to the dead tree lists. Assume that part all goes into standing
c           dead wood and that it is evenly divided between all age classes less than
c           the master cycle length old.   

C         MEX:    The amount of mortality predicted by prognosis that was not 
c                 predicted by the beetle model (in total CU FT)

c           DIF = MIY(ICYC) - MIY(ICYC-1)
            DIF=  IBMMRT
            MEX = (MPRG - MSUM) * CFV(I)
            SMX = MEX / DIF 
            
            IAG = 1       
            ISC = L2D(K) + 1
            JSC = MIN0(ISC,2)
            IPC = ISPFLL(ISPC)
            
            DO 200 J = 1, DIF
              IF (J .GT. UPAGE(IAG)) IAG = IAG + 1

              SMX = SMX * SDECRT(JSC)

C *********** Bound DMX to keep from getting too small (MJO March 1998)
              IF (DMX .LT. 1.0E-6) DMX = 1.0E-6

              DMX = DMX * DDECRT(JSC)
              DMX = DMX + SMX * FALLRT(IPC) * V2T
              SMX = SMX * (1 - FALLRT(IPC))
              
              SDWP(IBMSTD,IPC,ISC,IAG) = SDWP(IBMSTD,IPC,ISC,IAG) + SMX
              DDWP(IBMSTD,JSC,IAG) = DDWP(IBMSTD,JSC,IAG) + DMX
  200       CONTINUE   
          ENDIF
          
c         Transfer the (possibly) adjusted values back to their usual 
c         places, and let any output printing use those values. Note that
c         only slow mortality and Prognosis mortality are adjusted.         
          
          WK2(I)= MPRG
          
          IF ((PROB(I) - WK2(I)) .LT. 1.0E-6)
     >         WK2(I)= PROB(I) - 1.0E-6           
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     The following updates the WK2 array to account for Sanitaiton
C     removals (RNH Aug98)
C
C     Was Sanitization ndone this cycle?
C
      IF (LOKS) THEN
C     Set LOKS1 to FALSE to prevent cycles from being updated when
C     no Sanitization was done
C
      LOKS1 = .FALSE.      
C
C     Is the current tree a host tree?
C
       IF (LX) THEN
C
C     Check to see if current stand is subject to sanitization 
C
      DO 300 ISTD1= 1,BMSTND
C
        IF(IBMSTD .EQ. MYLSTS(ISTD1)) THEN
C
       DO 250 ISIZ1= MINSCS, MAXSCS
C
C     Check to see if tree is in size class that was samitized
C
         IF((K .GE. MINSCS) .AND. (K .LE. MAXSCS)) THEN
C
C     Adjust WK2 array
C
C      IF ((ATREEI(IBMSTD,ISIZ1) - AREMS(IBMSTD,ISIZ1)) .LE. 1.E-6) THEN
C      WK2(I) = PROB(I) - 1.0E-6
C      GO TO 400
C      ENDIF
C
C       WK2(I)= WK2(I) + AREMS(IBMSTD,ISIZ1)*PROB(I)
C
C
C      WK2(I)= 1./((ATREEI(IBMSTD,ISIZ1) - AREMS(IBMSTD,ISIZ1))/
C     1        ATREEI(IBMSTD,ISIZ1))*PROB(I)
C
      IF (ATREEI(IBMSTD,K) .LE. 1.0E-6) GO TO 400
C      IF (NRSC(K) .LE. 0) GO TO 400
C
C      WK2(I)= WK2(I)+AREMS(IBMSTD,K)/ATREEI(IBMSTD,K)*PROB(I)
C     1        /NRSC(K)*3.
C
      WK2(I)= WK2(I)+AREMS(IBMSTD,K)/ATREEI(IBMSTD,K)*PROB(I)
C      
C      write(29,*) ' bmstnd= ', bmstnd,' k=' ,k,' istd1= ', istd1,
C     1  ' NRSC(K)= ',NRSC(K),' K= ',K,' MPRG= ',MPRG
C      write(29,*) ' wk2= ', wk2(i),' arems= ', arems(ibmstd,isiz1),
C     1  ' atreei= ', atreei(ibmstd,isiz1)
C      write(29,*) ' I= ',I,' prob(i)= ', prob(i)
C
C     Check that removals pls mortality is less than total number of trees     
C
      IF((PROB(I)-WK2(I)) .LE. 1.0E-5) THEN
      WK2(I) = PROB(I) - 1.0E-6
      GO TO 400
      ENDIF
C
C	WK2(k) has been adjusted so branch out of logical structure
C
	GO TO 400
C
         ENDIF
  250  CONTINUE
        ENDIF
  300 CONTINUE
  400 CONTINUE
       ENDIF
      ENDIF

C
C       The SVS mortality processing for this tree record.
C       If it is a host species, retain value for svmort (SVS) process.
C       When the overall WWPB mortality is greater than FVS base mortality,
C       WWPB gets credit for all mortality. If FVS base mortality is greater,
C       WWPB gets credit for the portion of total WWPB mortality specifically
C       attributed to beetles
C       
        IF (LX) THEN
           IF (BASMRT .LE. WK2(I)) THEN
              BTKL(I) = WK2(I)
           ELSE
              BTKL(I) = MBTL
           ENDIF
        ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
   10   CONTINUE
   20 CONTINUE

C     DEBUG STATEMENT
C     WRITE (*,*) ' IN BMKILL: CALL SVMORT - 2  YEAR=',MIY(ICYC)
   
C     Stand Visualization process. Call svmort to add incremental mortality
C     attributed to WWPB.
      CALL SVMORT (2, BTKL, MIY(ICYC))

C     Attempt to put our volume removed information into the total volume
C     removed array from prognosis. I think OCVREM(1-6) are percentile classes.
C     Also note that since we're only putting our information in one place 
C     (total vol) that if there were other volumes from normal thinning, this
C     total vol would be greater than would be expected from the printed 
C     merchatable vols.

      OCVREM(7) = OCVREM(7) + CVOLREM(IBMSTD,1) + CVOLREM(IBMSTD,2) !USE NEW CYCLE-ACCUMULATOR VARS AJM 9/05
      
  150 CONTINUE
      IF (LBMDEB) WRITE (JBMBPR,160) ISTND,BMSTND,IBMSTD
  160 FORMAT (/' IN BMKILL: ISTND=',I4,' BMSTND=',I4,' IBMSTD=',I4)
   
      RETURN
      END
