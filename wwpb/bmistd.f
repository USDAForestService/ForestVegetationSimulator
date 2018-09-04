      SUBROUTINE BMISTD (ISTD,IYR)
C----------
C WWPB $Id$
C----------
*
* Calculates the within plot dynamics of choosing a host, and determining 
* whether it is a kill, strip-kill, or pitchout.  One or more special trees 
* should be killed first:  this part of the program is not currently implemented.
* The probability that a host is chosen from a particular size class depends on 
* the total basal area of the size class, the combined rating value (GRF) of the 
* size class, and the position of that size class under a beta distribution based 
* on total BKP in the stand.  Strip-kills result when the BKP available in the 
* stand is at least 50% (but less than 100%) of the BKP required to kill the tree 
* outright.  Pitchouts result whenever the BKP available is less than that 
* required to kill the tree.  When a tree is killed, the BKP available is reduced 
* by the amount that would be required to kill a tree of that size with a rating 
* value of 1.  When pitchouts occur, the BKP available is reduced to 0.
*----------------------------------------------------------------------
*
* Called from:   BMDRV
*
* Calls:         SPLAAR, BMRANN, BMCBET
*
* Revised 9/8/99 (AJM) New algorithm for calculating "A", a parameter in
* in the beta distribution (BMCBET).  A, and thus the beta distribution
* will now change as BKP per acre changes.  The mean size class preferred
* by beetles will be the largest size class able to be killed by the
* current bkp PER ACRE.
*
* Local variables:
*   ABETA:   the "A" parameter for the beta distribution function.
*   ACRES:   stand area rounded to an integer value
*   ATTP:    number of trees/acre to attack at any one time
*   ATTPRP:  proportion of trees in the size class that are attacked
*   AVKLBA:  the average basal area of the trees that will be killed by beetles
*   BETA:    a component of attractiveness to beetles, by size class
*   BETADIAM: Tree diameter associated with the basal area represented by the
*              current BKP/A
*   BKPKL:   amount of BKP needed to kill all "targeted" trees in size class
*   BKPUSE:  amount of BKP used on the targeted trees
*   HOST:    the density of live host trees remaining in a specified size class
*   ISIZ:    loop counter over size classes
*   MXISIZ:  max size class that had some trees present at start of subroutine
*   P:       cumulative distribution of attractiveness, by size class
*   PSCALE:  a component of attractiveness, by size class
*   SAREA:   the area of the stand in acres
*   SCALE:   the total attractiveness of all the size classes
*   SCKL:    the number of trees to be killed in the current size class across
*              the whole stand
*   SPKILL:  number of special trees killed in the stand so far
*   TBASP:   total basal area of live special trees
*   TOTKL:   the total number of trees that have to be killed to use up all the
*              the BKP in the stand
*   X:       Uniform random number used to decide what size class to attack (and
*              sometimes the temporary holder of the results of computation)
*
* Common block variables and parameters:
*   BKP:     From BMCOM:  the basal area of host per acre that can be killed by
*              the beetles present in each stand
*   GRF:     From BMCOM:  the rating value of the trees in each size class
*   MSBA:    From BMCOM;  the mean basal area of the trees in each size class
*   NSCL:    From BMPRM;  the number of dbh size classes in the model
*   PBKILL:  From BMCOM;  the proportion of Beetle-killed trees in each size
*              class, which is converted to the density of killed trees at the
*              end of the subroutine
*   PITCH:   From BMCOM;  the proportion of "pitch-out" trees, by size class
*   SPCLT:   From BMCOM:  the proportion of special trees in each size class,
*              dimensioned by Stand, Size-class, and Pest species, where 1 = the
*              driving beetle and 2 = Ips.
*   STRIP:   From BMCOM:  the number per acre of strip-killed trees, by size class
*   TREE:    From BMCOM;  the number per acre of trees in each size class at the
*              start of the subroutine, dimensioned by Stand, Size-class and
*              Host/Non-host (where 1 = host)
***********************************************************************

C Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C Common include files.

      INCLUDE 'BMCOM.F77'

C Variable declarations.

      INTEGER MXISIZ, ISIZ, I, J, K, ACRES, SPKILL, TEMPX
      REAL    ATTP, ATTPRP
      REAL    AVKLBA, TOTKL, SCKL
      REAL    BETA(NSCL)
      REAL    BKPKL, BKPUSE
      REAL    SAREA
      REAL    HOST
      REAL    P(NSCL), PSCALE(NSCL)
      REAL    SCALE, X, TBASP
      REAL    BETADIAM, ABETA
      REAL    NZERO, NUNIT
      PARAMETER (NZERO= 1.0e-06)
      PARAMETER (NUNIT= 1.0 - NZERO)

      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMISTD: Year= ',I5, 'Stand= ', I6)

C Reduce beetle numbers for the proportion killed by spraying
      BKP(ISTD)= BKP(ISTD) * (1 - SPRAY(ISTD,1))


C Jump to end if no more BKP in stand.
      IF (BKP(ISTD) .LT. NZERO) GOTO 9999


C Initializations
C Get stand area, determine max size class present, and get Beta-
C distribution.

      CALL SPLAAR(ISTD,SAREA,IRC)

      ATTP = 1.0 / SAREA

      DO 20 ISIZ= 1,NSCL
         PITCH(ISTD,ISIZ)= 0.0
   20 CONTINUE


      DO 30 ISIZ=NSCL,1,-1
        IF (TREE(ISTD,ISIZ,1) .GT. 0.0) THEN
           MXISIZ= ISIZ
           GOTO 35
        ENDIF
   30 CONTINUE
   35 CONTINUE

C     Find the biggest tree in the stand that can be killed with the available
C     BKP, and make that the mean of the beta distribution.
C
C     Comment out the following and replace with a single loop that finds
C     the maximum size class having a baaal area that can be killed with
C     teh existing BKP (RNH July98)
C
C      X = BKP(ISTD) * SAREA
C
c **** ANOTHER CONDITION IS ADDED HERE TO AVOID THE SITUATION WHEN DO 36 LOOP
C **** WILL NEVER BE EXCUTED.  S,ZHANG  4/23/98
C
C      IF (X .GE. MSBA(NSCL)) THEN
C         X=MSBA(NSCL)
C         JSIZ=NSCL
C         GOTO 38
C      ENDIF
C
C      DO 36 ISIZ = 1,NSCL
C        IF (MSBA(ISIZ) .GT. X) THEN
C      TEMPX = ISIZ
C  *** add another conditional statement to avoid that isiz large than nscl.
C          IF (ISIZ .GE. NSCL) TEMPX = ISIZ-1
C          DO 37 JSIZ = NSCL,TEMPX,-1
C            IF (MSBA(JSIZ) * GRF(ISTD,JSIZ) .LE. X) THEN
CC             X = MSBA(JSIZ)
C
C              GOTO 38
C            ENDIF
C   37     CONTINUE
C       ENDIF
C
C   36 CONTINUE
C
C     This new loop finds the max size class where the product of
C     the basal and the rating value for that size class
C     can be killed by the stand BKP (RNH July98)
C
      DO 37 ISIZ1= NSCL, 1, -1
C
      IF ((MSBA(ISIZ1)*GRF(ISTD, ISIZ1)) .LE. BKP(ISTD)) THEN
C      JSIZ= ISIZ1
      GO TO 38
      ENDIF
C
   37 CONTINUE
C
   38 CONTINUE
C
      IF (ISIZ1 .LE. 0) ISIZ1= 1
C
C      WRITE(28,*) 'GRF(ISTD,ISIZ1)= ',GRF(ISTD,ISIZ1),' ISIZ1= ',ISIZ1
C
C... The following added 9/8/99 (ajm).  BETADIAM is the diameter of a tree
c... capable of being killed by the current BKP/A.  ABETA is the value that
C...  will be assigned to "A" in BMCBET (determines shape of beta
c     distribution.
c     BETADIAM = SQRT(BKP(ISTD) * 183.3465)
c      IF (BETADIAM .GT. 30) ABETA = 5
c      IF ((BETADIAM .LE. 30) .AND. (BETADIAM .GT. 18))
c     &     ABETA = 2 + 0.25 * (BETADIAM - 18)
c      IF ((BETADIAM .LE. 18) .AND. (BETADIAM .GE. 9))
c     &     ABETA = 1 + ((1 / 9) * (BETADIAM - 9))
c      IF (BETADIAM .LT. 9) ABETA = 1

      IF (BKP(ISTD) .GT. 6) ABETA = 15
      IF ((BKP(ISTD) .LE. 6) .AND. (BKP(ISTD) .GT. 3.6))
     &     ABETA = 2.5 + 4.46 * (BKP(ISTD) - 3.6)
      IF ((BKP(ISTD) .LE. 3.6) .AND. (BKP(ISTD) .GE. 1.6))
     &     ABETA = 1.2 + (0.65 * (BKP(ISTD) - 1.6))
      IF (BKP(ISTD) .LT. 1.6) ABETA = 1
C *** MODIFICATION **********************
      CALL BMCBET(ABETA,1,ISIZ1,BETA)
C *** END MODIFICATION *******************

C      CALL BMCBET(X,MXISIZ,BETA)


C*****************************************************************************
C First kill some of the special trees, if there are any.
C*****************************************************************************

C     Find the TBASP, the total basal area of special trees.

      TBASP = 0.0
      DO 50 ISIZ = 1,MXISIZ
        IF ((SPCLT(ISTD,ISIZ,1)*TREE(ISTD,ISIZ,1)) .GT. NZERO) 
     &    TBASP=TBASP +(TREE(ISTD,ISIZ,1)*SPCLT(ISTD,ISIZ,1)*MSBA(ISIZ)) 
   50 CONTINUE
   
      IF (TBASP .LE. NZERO) GOTO 201
   
c     Calculate the cumulative attractiveness of the size classes, P,
c     based on their basal area of special trees.
      X = 0.0
      DO 60 ISIZ = 1,MXISIZ
        IF ((SPCLT(ISTD,ISIZ,1)*TREE(ISTD,ISIZ,1)) .GT. NZERO)
     &    X = X +(MSBA(ISIZ)*TREE(ISTD,ISIZ,1)*SPCLT(ISTD,ISIZ,1)/TBASP)
        IF (X .GT. NUNIT) X = 1.0
        P(ISIZ) = X                                             
   60 CONTINUE

C       Repeat the selection of special host trees until you have one for every
C       acre in the stand, or there is no BKP left or you run out of special trees.
        SPKILL = 0
        ACRES = MAX(NINT(SAREA),1)        
        DO 200 WHILE ((SPKILL .LT. ACRES) .AND. (BKP(ISTD) .GT. NZERO) 
     &                                    .AND. (TBASP .GT. NZERO))

C         Randomly select a size class of special tree to attack. 
          CALL BMRANN(X)                                                       
          DO 70 ISIZ = 1,MXISIZ
            IF ((X .LE. P(ISIZ)) .OR. (ISIZ .EQ. MXISIZ)) GOTO 72
   70     CONTINUE

C         Determine what proportion of the size class is under attack.  
   72     HOST=TREE(ISTD,ISIZ,1) *(SPCLT(ISTD,ISIZ,1)-PBKILL(ISTD,ISIZ)) 
          IF (ATTP .GE. HOST) THEN
            ATTPRP = HOST / TREE(ISTD,ISIZ,1)
          ELSE
            ATTPRP = ATTP / TREE(ISTD,ISIZ,1)
          END IF   
          
C         Find amount of BKP used on the attacks, and needed to kill.
          BKPUSE= ATTPRP * TREE(ISTD,ISIZ,1) * MSBA(ISIZ)
          BKPKL= BKPUSE * GRF(ISTD, ISIZ)

C         If there is enough BKP to kill the trees, then do.  If the trees are
C         killed but there is less BKP than could be used by these trees, then
C         remember some stuff that is needed to make sure reproduction is not
C         excessive.
          IF (BKP(ISTD) .GE. BKPKL) THEN                             

            IF (BKP(ISTD) .LT. BKPUSE) THEN
              FINAL(ISTD,1) = BKP(ISTD)

C         ******* begin modification******************
C         Another condition to address the situation when BKP is greater than BKPUSE
C         is added here  (S.Zhang, 4/13/98)
C         ********END MODIFICATION**********************

            ELSE
              FINAL(ISTD,1) = BKPUSE
            ENDIF

            FINAL(ISTD,2) = ISIZ
            FINAL(ISTD,3) = ATTPRP * TREE(ISTD,ISIZ,1)
              
C           ENDIF, this end if is for original codes
C           ****** end of modification ****************

            PBKILL(ISTD,ISIZ)= PBKILL(ISTD,ISIZ) + ATTPRP
            SPKILL = SPKILL + 1
            BKP(ISTD) = BKP(ISTD) - BKPUSE
            IF (BKP(ISTD) .LT. NZERO) BKP(ISTD) = 0.0
  
C           If killing these trees has eliminated all special trees in the size 
C           class, then remove the size class from the cumulative distribution.
            IF (PBKILL(ISTD,ISIZ) .GT. SPCLT(ISTD,ISIZ,1)) THEN                      
            
              PBKILL(ISTD,ISIZ)= SPCLT(ISTD,ISIZ,1)                                           
              TBASP = 0.0
              DO 80 ISIZ = 1,MXISIZ
                IF ((SPCLT(ISTD,ISIZ,1) .GT. PBKILL(ISTD,ISIZ)) 
     &            .AND. (TREE(ISTD,ISIZ,1) .GT. NZERO)) 
     &              TBASP = TBASP + TREE(ISTD,ISIZ,1) * MSBA(ISIZ) 
     &                        * (SPCLT(ISTD,ISIZ,1) -PBKILL(ISTD,ISIZ))  
   80         CONTINUE
              
              IF (TBASP .LE. NZERO) GOTO 201
              X = 0.0
              DO 90 ISIZ = 1,MXISIZ
                IF ((SPCLT(ISTD,ISIZ,1) .GT. PBKILL(ISTD,ISIZ)) 
     &            .AND. (TREE(ISTD,ISIZ,1) .GT. NZERO))
     &              X = X + (MSBA(ISIZ) *TREE(ISTD,ISIZ,1) 
     &                  *(SPCLT(ISTD,ISIZ,1) -PBKILL(ISTD,ISIZ)) /TBASP)
                IF (X .GT. NUNIT) X = 1.0
                P(ISIZ) = X                                             
   90         CONTINUE
            END IF

C         If there is not enough BKP to kill the trees, then make them pitch-
C         outs.  If there is 75% of enough BKP, then also consider the trees
C         to be strip-kills.  All BKP is now used up.
          ELSE                                                       
            X = BKP(ISTD) / BKPKL
            IF (X .GE. 0.75) THEN
              STRIP(ISTD,ISIZ) = STRIP(ISTD,ISIZ) 
     &                          + (ATTPRP * TREE(ISTD,ISIZ,1))
              PITCH(ISTD,ISIZ)= PITCH(ISTD,ISIZ) + ATTPRP

            ELSE
              PITCH(ISTD,ISIZ)= PITCH(ISTD,ISIZ) + ATTPRP

C        *******************************************************
C        Some modification to keep at least some beetles are available
C        for reproduction were suggested. S.Zhang, 4/10/98
C        **************************************************************
              FINAL(ISTD,1) =BKP(ISTD)*0.15
              FINAL(ISTD,2) = ISIZ
              FINAL(ISTD,3) = ATTPRP * TREE(ISTD,ISIZ,1)

C        *** End of modifications *************
            ENDIF

            BKP(ISTD) = 0.0


          END IF

C       Now repeat the process to attempt another kill.
  200 END DO
  
  201 CONTINUE 
     
     
C Jump to end if no more BKP in stand.
      IF (BKP(ISTD) .LT. NZERO) GOTO 9999 


C*******************************************************************************
C Kill remaining trees at random based on the attractiveness of each size class.
C*******************************************************************************

C First, calculate PSCALE for each size class, find the total SCALE, and predict
C the average basal area of the trees that will be killed.  Only continue if 
C suitable host is present (SCALE > 0).  If so, predict the total number of trees 
C that must be killed.  

      DO 300 ISIZ = 1,MXISIZ
C*************************************************************************
C some modification.  S.Zhang
C ************************************************************************
C        PSCALE(ISIZ) = TREE(ISTD,ISIZ, 1)
C     &                 * (1 - PBKILL(ISTD,ISIZ)) / GRF(ISTD, ISIZ)

        PSCALE(ISIZ) = MSBA(ISIZ) * TREE(ISTD,ISIZ, 1)
     &                 * (1 - PBKILL(ISTD,ISIZ)) / GRF(ISTD, ISIZ)
  300 CONTINUE

      SCALE = 0.0
      AVKLBA = 0.0
      DO 310 ISIZ = 1,MXISIZ

C ************************************************************************
C set BETA(i) to 1 to test the effect of beta distribution on
C beetle attack S.Zhang
C ************************************************************************
c        SCALE = SCALE + PSCALE(ISIZ)
c        AVKLBA = AVKLBA + PSCALE(ISIZ) * MSBA(ISIZ)

        SCALE = SCALE + BETA(ISIZ) * PSCALE(ISIZ)
        AVKLBA = AVKLBA + (BETA(ISIZ) * PSCALE(ISIZ) * MSBA(ISIZ))
  310 CONTINUE 
  
      IF (SCALE .LT. NZERO) GOTO 900
                                     
      AVKLBA = AVKLBA / SCALE
      TOTKL = BKP(ISTD) * SAREA / AVKLBA 


C********************************************************************************
C Use the following "group-kill" process if two or more kills are expected in at 
C least one size class.  The loop may be needed more than once because the first  
C run may allocate more kills to an attractive size class than the size class can 
C absorb.************************************************************************

      DO 555 WHILE (TOTKL .GT. (2 * MXISIZ))
      
C       Consider each size class.  See if there is suitable host in that size 
C       class.  If so, calculate how much live host there is, and how many kills
C       will be attracted to that size class.
        DO 400 ISIZ = 1,MXISIZ
C**************************************************************************
C  Delete BETA() to test beta function effect on beetle attack
C     S.Zhang, 4/15/97
C***************************************************************************

C          IF (PSCALE(ISIZ) .GT. NZERO) THEN

          IF (BETA(ISIZ) * PSCALE(ISIZ) .GT. NZERO) THEN
            HOST = TREE(ISTD,ISIZ, 1) * (1 - PBKILL(ISTD,ISIZ))            
C            SCKL = INT(TOTKL * PSCALE(ISIZ) / SCALE)
            SCKL = INT(TOTKL * BETA(ISIZ) * PSCALE(ISIZ) / SCALE)

C           Determine whether the size class can absorb the target # of kills,
C           and calculate ATTPRP accordingly.  Reduce BKP for the amount used.
C           Kill all the attacked trees.       
            IF (SCKL .GE. HOST * SAREA) THEN
              ATTPRP = HOST / TREE(ISTD,ISIZ, 1)                         
              BKPUSE = HOST * MSBA(ISIZ)
            ELSE
              ATTPRP = SCKL / (TREE(ISTD,ISIZ, 1) * SAREA)
              BKPUSE = (SCKL / SAREA) * MSBA(ISIZ)
            END IF

            BKP(ISTD) = BKP(ISTD) - BKPUSE
            
            PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) + ATTPRP                     
            IF (PBKILL(ISTD,ISIZ) .GT. NUNIT) PBKILL(ISTD,ISIZ) = 1.0 
            
          END IF 
  400   CONTINUE
      
C       Recalculate SCALE and AVKLBA to remove extinct size classes from the
C       cumulative distribution.  If there is no suitable host left, set BKP
C       to zero (all remaining BKP is lost).

        AVKLBA = AVKLBA * SCALE
        DO 500 ISIZ = 1,MXISIZ                                        
          IF (PBKILL(ISTD,ISIZ) .GE. 1.0) PSCALE(ISIZ) = 0.0
  500   CONTINUE

        SCALE= 0.0
        AVKLBA= 0.0
        DO 510 ISIZ= 1, MXISIZ
C ***********************************************************************
C Delete BETA() to test the effect of Beta function on beetle attacks
C      S.Zhang, 4/15/98
C ***********************************************************************

c          SCALE= SCALE + PSCALE(ISIZ)
c          AVKLBA= AVKLBA + PSCALE(ISIZ) * MSBA(ISIZ)

          SCALE= SCALE + BETA(ISIZ) * PSCALE(ISIZ)
          AVKLBA= AVKLBA + (BETA(ISIZ) * PSCALE(ISIZ) * MSBA(ISIZ))
  510   CONTINUE

        IF (SCALE .GT. NZERO) THEN
          AVKLBA= AVKLBA / SCALE
          TOTKL= BKP(ISTD) * SAREA / AVKLBA
        ELSE
          BKP(ISTD)= 0.0                                    
          TOTKL= 0.0
        END IF

  555 END DO


C***************************************************************************
C Use the following "individual-kill" process when multiple kills are not
C expected in each size class, but there is still some BKP left.
C***************************************************************************

C     Calculate the cumulative attractiveness of the size classes, P.
      IF ((SCALE .GT. NZERO) .AND. (BKP(ISTD) .GT. NZERO)) THEN                  
        X = 0.0
        DO 600 ISIZ = 1,MXISIZ
C***************************************************************************
C  Delete BETA() to test the effect of beta function on beetle attacks
C***************************************************************************
c          IF (PSCALE(ISIZ) .GT. NZERO)
c     &      X = X + PSCALE(ISIZ) / SCALE

          IF ((BETA(ISIZ) * PSCALE(ISIZ)) .GT. NZERO)
     &      X = X + (BETA(ISIZ) * PSCALE(ISIZ)) / SCALE
          IF (X .GT. NUNIT) X = 1.0
          P(ISIZ) = X                                             
  600   CONTINUE
      END IF

C     Repeat the following until there is no BKP left:        
      DO 888 WHILE (BKP(ISTD) .GT. NZERO)

C       First, check that there is suitable host.  If not, eliminate all
C       remaining BKP and skip the rest of the routine. 

        IF (SCALE .LE. NZERO) THEN
          BKP(ISTD) = 0.0                                   
          GOTO 800
        END IF                                         

C       Second, choose a random number and use it to determine which size class 
C       of tree to kill. 
        CALL BMRANN(X)                                                       
        DO 700 ISIZ = 1,MXISIZ
          IF ((X .LE. P(ISIZ)) .OR. (ISIZ .EQ. MXISIZ)) GOTO 702
  700   CONTINUE

C       Third, determine whether there are enough live trees left in this size
C       class to support an attack on ATTP trees.  If so, attack ATTP trees, and
C       if not, attack all remaining live trees.  Calculate how much BKP will be
C       used on this many attacks.

  702   HOST = TREE(ISTD,ISIZ, 1) * (1 - PBKILL(ISTD,ISIZ))                  
        IF (ATTP .GE. HOST) THEN
            ATTPRP = HOST / TREE(ISTD,ISIZ, 1)                          
            BKPUSE = HOST * MSBA(ISIZ)
        ELSE
            ATTPRP = ATTP / TREE(ISTD,ISIZ, 1)
            BKPUSE = ATTP * MSBA(ISIZ)
        END IF

C       Fourth, determine how much BKP is needed to kill all the attacked trees.
C       If this much BKP is available, then reduce the remaining BKP by the 
C       amount used in attacking this many trees. increment PBKILL to kill all 
C       the attacked trees.  If this makes the size class extinct, remove it 
C       from the cumulative distribution of attractiveness.

C       When there is enough BKP to kill the attacked trees, but no enough to entirely
C       fill them, then keep track of how much BKP there was, what size class, and 
C       the TPA attacked (this will be used by CBKP to make sure there are no 
C       unreasonable reproductive rates).
C       final(1) = BKP remaining
c       final(2) = size class killed
c       final(3) = TPA killed

        BKPKL = BKPUSE * GRF(ISTD, ISIZ)                     
        IF (BKP(ISTD) .GE. BKPKL) THEN                             

            IF (BKP(ISTD) .LT. BKPUSE) THEN
              FINAL(ISTD,1) = BKP(ISTD)

C       *********************Begin modefications*********************
C       Another condition when BKP is larger than BKPUSE is added here
C       to make a full condition search.  S.Zhang, 4/13/98
C       **************************************************************
            ELSE
              FINAL(ISTD,1) = BKPUSE
            ENDIF

              FINAL(ISTD,2) = ISIZ
              FINAL(ISTD,3) = ATTPRP * TREE(ISTD,ISIZ,1)

C        ENDIF, the original ENDIF is inactivated here
C        **************End modifications ******************************  
  
            BKP(ISTD) = BKP(ISTD) - BKPUSE
            IF (BKP(ISTD) .LT. NZERO) BKP(ISTD) = 0.0

            PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) + ATTPRP                  
c
            IF (PBKILL(ISTD,ISIZ) .GT. NUNIT) THEN                      
            
              PBKILL(ISTD,ISIZ)= 1.0                                           
              PSCALE(ISIZ)= 0.0
              
              SCALE= 0.0
              DO 740 I= 1, MXISIZ
C**********************************************************************
C Delete BETA() to test beta function effect on beetle attacks
C**********************************************************************
c                SCALE = SCALE + (PSCALE(I))

                SCALE = SCALE + (PSCALE(I) * BETA(I))
  740         CONTINUE    

              IF (SCALE .GT. NZERO) THEN
                X = 0.0
                DO 750 I = ISIZ,MXISIZ
c                  IF (PSCALE(I) .GT. NZERO) THEN
c                    X = X + PSCALE(I) / SCALE

                  IF ((BETA(I) * PSCALE(I)) .GT. NZERO) THEN
                    X = X + (BETA(I) * PSCALE(I)) / SCALE
                  END IF
                  IF (X .GT. NUNIT) X = 1.0
                  P(I) = X
  750           CONTINUE
              END IF
              
            END IF

C       If there was not enough BKP available to kill all attacked trees
C       but there was 75% of the required amount, then strip kill the attacked
C       trees.  In any case, all the attacked trees become pitch-outs and all 
C       the remaining BKP is used up.
        ELSE                                                       
        
            X = BKP(ISTD) / BKPKL
            IF (X .GE. 0.75) THEN
                STRIP(ISTD, ISIZ) = STRIP(ISTD, ISIZ) 
     &                    + (ATTPRP * TREE(ISTD,ISIZ, 1)) 
                PITCH(ISTD,ISIZ) = PITCH(ISTD,ISIZ) + ATTPRP

            ELSE
                PITCH(ISTD,ISIZ) = PITCH(ISTD,ISIZ) + ATTPRP

C       **************************************************
C       If current BKP is not enough to kill any trees, keep 10 percent for reproduction
C       Modifications were made on 4/10/97
C       ********************************************
                FINAL(ISTD,1) = 0.15*BKP(ISTD)
                FINAL(ISTD,2) = ISIZ
                FINAL(ISTD,3) = ATTPRP * TREE(ISTD,ISIZ,1)
            ENDIF

C       ***End of the modifications****

            BKP(ISTD) = 0.0
        END IF

  800 CONTINUE 

C       Now repeat the process to attempt another kill.
  888 END DO
                 
                 
C If there was no suitable host after special trees were dealt with, all remaining
C BKP is lost.
  900 continue
      BKP(ISTD) = 0.0                     
                       
                        
C Jump to here when BKP gets used up.
 9999 CONTINUE
       
                        
C For each size class, convert PBKILL(ISTD,) from a proportion to the number of 
C trees per acre, and add the killed trees to the standing dead wood pool. 
      DO 910 ISIZ= 1, NSCL 
         IF (PBKILL(ISTD,ISIZ) .LE. 0.0) GOTO 910
         PBKILL(ISTD,ISIZ)= PBKILL(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)
         J = L2D(ISIZ) + 1
         K = IQPTYP(ISTD,1)
         SDWP(ISTD,K,J,1) = SDWP(ISTD,K,J,1) + 
     >                        PBKILL(ISTD,ISIZ) * TVOL(ISTD,ISIZ,1)
  910 CONTINUE 

      
C Do some output stuff.       
      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMISTD: Year= ',I5, 'Stand= ', I6)
      
      RETURN
      END
