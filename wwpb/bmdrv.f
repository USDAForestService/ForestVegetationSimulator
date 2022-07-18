      SUBROUTINE BMDRV
C----------
C WWPB $Id$
C----------
C
C     PART OF THE PINE BEETLE EXTENSION OF THE PPE.
C
C     CALLED FROM -- ALSTD2
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77' 
      INCLUDE 'PPEPRM.F77'  
      INCLUDE 'BMPRM.F77'
      INCLUDE 'PPCNTL.F77' 
      INCLUDE 'BMCOM.F77'
      
C Local variable definitions:
      
      INTEGER  IYR, ISTD, SINDX
****************************************
C...THE FOLLOWING ADDED FOR TEST 3/27/00
      REAL OLDGRF(NSCL)
****************************************
C     WRITE REQUESTED DEBUG MSG.
C
      IF (LBMDEB) WRITE (JBMBPR,1) LBMSPR,IBMYR1,IBMYR2
    1 FORMAT (/' IN BMDRV: LBMSPR=',L2,'; IBMYR1,IBMYR2=',2I6)
C
C     IF THE WPB MODE IS ON, THEN: PROCESS THE OUTBREAK.
C
C      IF (LBMSPR .AND. BMSTND .GT. 0) THEN
C      BETTER TO USE IBMYR1 THAN BMSTND.  COMMENT OUT ABOVE.  AJM 12/05
      IF (LBMSPR .AND. IBMYR1 .GT. 0) THEN

C     Zero arrays that store volume removal through sanitation and
C     salvage management actions.
      
        DO 10 ISTD = 1, BMSTND
          CVOLREM(ISTD,1) = 0.0      
          CVOLREM(ISTD,2) = 0.0      
          SREMOV(ISTD)   = 0.0
          MYLSTS(ISTD)   =0
   10   CONTINUE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     Initialize ATREEI and AREMS for cycle (RNH Aug98)
C
C      IF (LOKS) THEN
C
      DO 20 ISTD1= 1, BMSTND
       DO 15 ISIZ1= 1, NSCL+1
       ATREEI(ISTD1,ISIZ1)= TREE(ISTD1,ISIZ1,1)
       AREMS(ISTD1,ISIZ1)= 0.0
   15  CONTINUE
   20 CONTINUE
C      ENDIF
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C       Loop over the years within the cycle
                                            
        DO 30 IYR = IBMYR1,IBMYR2
        
c         CALL GPCYCL
          
          write(*,*) iyr
 
c         Collect pheromone management info.

          CALL BMPHER (IYR)
          CALL BMAPH (IYR)         
          
c         Other management (except sanitation cuts)

          CALL BMPSTC (IYR)
          CALL BMSALV (IYR) 
C
C         Moved BMSANI here to make stand level (RNH, May98)
C
          CALL BMSANI (IYR)
          CALL BMSMGT (IYR)

c         Determine the rating values due to 'drought' in this year.

          CALL BMDRGT (IYR)         

c         Do the first loop over the stands 
          
          DO 40 ISTD = 1, BMSTND 
            IF(.NOT. STOCK(ISTD)) GOTO 40
            
            SINDX= BMSDIX(ISTD)

            CALL BMCWIN(SINDX,IYR)

            CALL BMLITE (SINDX,IYR)

            CALL BMFIRE (SINDX,IYR)

C           Note that OBB and Defols have become "fast" for now because then
C           we don't have to worry about what happens over cycle boundaries
C           when the attacked trees grow. The calls are just commented out
C           below.

C           Other bark beetles

            CALL BMOBB (SINDX,IYR)

C           Defoliators

            CALL BMDFOL (SINDX,IYR)

C           Extra mortality calculations

            CALL BMQMRT (SINDX,IYR)

C           Remove trees killed by the fast acting agents: windthrow and fire
C           and extra mortality (Set slow toggle to false)

            CALL BMMORT(SINDX,IYR,.FALSE.)

C           Calculate GRF         
C            WRITE (*,*) STOCK(ISTD), SINDX, ISTD, BMSTND
C            CALL BMCGRF (SINDX,IYR)
            CALL BMCGRF (SINDX,IYR,OLDGRF)
C            WRITE (*,*) ISTD, RVDNST(ISTD)
C           Do a sanitation cut (note that this is lanscape-level)
C     Comment out BMSANI to make stand level (RNH, May 98)
C
C            CALL BMSANI (SINDX,IYR)
C
C           Calculate BKP

C            CALL BMCBKP (SINDX,IYR)
            CALL BMCBKP (SINDX,IYR,OLDGRF)

C           Calculate numerator of attractiveness eqn.         

            CALL BMCNUM (SINDX,IYR)             

   40     CONTINUE

c         Calculate and apply negative feedback if triggered by a high
c         landscape-level BKP (MJO April98). Added LFDBK flag (MJO July98).

c     The following call commented out 12/1/99. AJM.  This process now 
c     happens WITHIN BMCBKP, and is now a stand-level phenomena.

c          IF (LFDBK) CALL BMFDBK (IYR)

c         Compute attractiveness and move BKP around.
   
          CALL BMATCT (IYR)

c         Do the second loop over stands.
          
          DO 45 ISTD = 1, BMSTND 
            IF(.NOT. STOCK(ISTD)) GOTO 45
          
            SINDX= BMSDIX(ISTD)
            
C           These are now called above with the fast DVs. See comment there.
C           Other bark beetles
C           CALL BMOBB (SINDX,IYR)
C           Defoliators
C           CALL BMDFOL (SINDX,IYR)

C          Run Ips routine if Ips is a driving variable or if Ips is main beetle

           IF ((PBSPEC .EQ. 3) .OR. (IPSON))
     >           CALL BMIPS (SINDX,IYR)

c           Do the non-Ips within-stand beetle dynamics. 

            IF (PBSPEC .NE. 3) CALL BMISTD(SINDX,IYR)
            
C           Print out beetle specific output.

            CALL BMOUT (SINDX,IYR) ! replaces bmoutm ajm 9/05
c            CALL BMOUTM (SINDX,IYR) 
c            CALL BMDVO (SINDX,IYR) ! obsolete ajm 9/05
    
C           Remove beetle-killed (and slow DV killed) trees.

            CALL BMMORT(SINDX,IYR,.TRUE.)
            
c           Age dead woody pools one year

            CALL BMAGDW (SINDX,IYR)

   45     CONTINUE  
   
   30   CONTINUE
        
      ENDIF
      
      RETURN
      END
