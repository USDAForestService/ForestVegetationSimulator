      SUBROUTINE BMIPS(ISTD,IYR)                                       
      
C     CALLED FROM: BMDRV
C     CALLS:  SPLAAR
C             BMRANN
C**********************************************************************
C  **BMIPS  Date of last revision:  SEPT 14, 1994
C----------------------------------------------------------------------
C  Purpose:
C     This subroutine deals with Ips. Currently, it fills up 90%
C     of the slash with Ips. Then any remaining BKP attack trees, either
C     top-killing them or totally killing them, depending on the size of 
C     the tree.
C
C     Routine is used for Ips as a Driving Variable or as the main beetle.
C----------------------------------------------------------------------
C
C  Call list definitions:
C
C  Local variable definitions:
C     ATTP:   Proportion of trees/record to attack at any one time
C     ATTPRP: Density? of trees in record to attack
C     BKPIP:  Temporary value containing BKP of Ips for stand
C     CHECK:  Counter that indicates how  many size classes have
C             been checked (for presence of host trees/Ips attacked trees?)
C     DEBUG:  Logical flag to turn debug on and off.
C     FULL:   Array with flag indicating whether it is has been
C             entirely attacked or not
C     IDSIZ:  Loop counter over current size class
C     ISIZ:   Loop counter for dbh size classes
C     NEED:   Host volume in current size class 
C             needed to accomodate the BKP for Ips 
C     PSCALE: Array (by size class) that scales the likelihood that trees
C             will be attacked depending on the cumulative host density 
C     TEMP:   Amount of Ips BKP left after available host dead wood (slash,
C             downed wood) is taken up by Ips
C     TOTHST: Total density of host trees for Ips
C     X:      Value of uniform random number
C  Common block variables and parameters:
C     TOPKLL: Array holding proportion of stems top-killed by Ips in
C             each size class
C     ALLKLL: Array holding number of stems totally killed by Ips in
C             each size class
C     DWPHOS: From BMCOM; Array to hold volumes of downed host tree 
C             volume for Ips (stratified by standing/dead and size class)
C     IPSMIN:
C     MSBA:   From BMCOM; Array containing the BA in each DBH size class 
C     MXDWHC: From BMPRM; Maximum number of dead woody pool host types
C             for Ips (1=standing; 2=downed)
C     MXDWSZ: From BMPRM; Number of dead woody pool size classes
C     NSCL:   From BMPRM; Number of dbh size classes
C     PFSLSH: From BMPRM; Prop'n of slash to fill with Ips before trying
C             to kill trees
C     PSLASH: From BMCOM; 
C     PIE:    From BMPRM; The value of pi
C     UPSIZ:  From BMCOM; Array containing the endpoints of each size class
C     WPBA:   From BMCOM; Array containing the BA in each Dead Woody Pool
C             size class
C**********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'BMCOM.F77'

C.... Variable declarations.

      INTEGER CHECK
      INTEGER IDSIZ
      INTEGER FULL(NSCL) 
      INTEGER SCKL
      REAL ADD 
      REAL ATTP 
      REAL ATTPRP
      REAL BKPIP   
      REAL NEED
      REAL PSCALE(NSCL) 
      REAL TEMP
      REAL TOTATT 
      REAL TOTSCL
      REAL X

C.... Initialize local variables

      CHECK= 0.0
      DO 5 ISIZ = 1,NSCL
          FULL(ISIZ) = 0
          PSCALE(ISIZ) = 0.0
          TOPKLL(ISTD,ISIZ)= 0.0
    5 CONTINUE              
    
C.... Check for debug.

      IF (LBMDEB) WRITE (JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMIPS: Year= ',I5, 'Stand= ', I6)
 
C.... Begin Routine

      BKPIP = BKPIPS(ISTD)
      
C.... only continue if have Ips present

      IF (BKPIP .LE. 1E-6) GOTO 9999
      
C     get the area of the current stand
   
      CALL SPLAAR(ISTD,CAREA,IRC)

C.... loop over the number of dead woody pool size classes

      DO 100 IDSIZ=1,MXDWSZ

C....   in this case, standing or down

        DO 110 IDTYP=1,MXDWHC

          IF (BKPIP .GT. 1E-6) THEN
            TEMP = BKPIP - (DWPHOS(ISTD,IDTYP,IDSIZ)
     &               * WPBA(IDSIZ) * PFSLSH)
            IF (TEMP .GT. 0.0) THEN
               PSLASH(ISTD,IDTYP,IDSIZ) = PFSLSH
               IF (DWPHOS(ISTD,IDTYP,IDSIZ) .EQ. 0.0) 
     &                            PSLASH(ISTD,IDTYP,IDSIZ) = 0.0
               BKPIP = TEMP
            ELSE
               PSLASH(ISTD,IDTYP,IDSIZ) = BKPIP
     &                   / (DWPHOS(ISTD,IDTYP,IDSIZ) * WPBA(IDSIZ))
               BKPIP = 0.0
            ENDIF
          ELSE

C....       if no BKP left then exit out of routine

            GOTO 9999
          ENDIF

  110   CONTINUE

c          if no BKP left then exit out of routine
      
        IF (BKPIP .LE. 1E-6) GOTO 9999

  100 CONTINUE

C.... Once the slash fills up, then any remaining Ips start attacking
C     trees randomly choose tree size to attack (Ips doesn't care what
C     size the tree is since it only likes the part less than
C     3 inches diam)

      TOTSCL = 0.0
      AVKLBA = 0.0
      MAXISZ = 0
      DO 300 ISIZ= IPSMIN,NSCL

         IF (TREE(ISTD,ISIZ,1) .EQ. 0.0) THEN
            CHECK = CHECK + 1
            GOTO 300
         ENDIF
         IF (ISIZ .GT. MAXISZ) MAXISZ = ISIZ

C....  scales the random variable by density and GRF (RV)

         PSCALE(ISIZ) = TREE(ISTD,ISIZ,1) / GRF(ISTD,ISIZ)
         TOTSCL = TOTSCL + PSCALE(ISIZ)

C         IF (ISIZ .GT. IPSMAX) THEN 
C            ISZ = IPSMAX
C         ELSE
C            ISZ = ISIZ
C         ENDIF
C         AVKLBA = AVKLBA + PSCALE(ISIZ) * MSBA(ISZ)
         AVKLBA = AVKLBA + PSCALE(ISIZ) * MSBA(IPSMIN)

  300  CONTINUE

C     If there is no host in the stand then exit out of routine

      IF (TOTSCL .LT. 1E-4) GOTO 9999
      
c     Calculate the average basal area that can be attacked and the
c     average number of attacks that can occur.

      AVKLBA = AVKLBA / TOTSCL
      TOTKL = BKPIP * CAREA / AVKLBA

C**********************************************************************
C Use the following "group-kill" process if two or more kills are
C expected in all eligible size classes.  The loop may be needed more
C than once.
C**********************************************************************

C     NUMSIZ is the number of size classes which have host and could
C     be attacked

      NUMSIZ = NSCL - CHECK - (IPSMIN - 1)

  330 CONTINUE

C       Consider each size class.  See if there is suitable host in
C       that size class. If so, calculate how much live host there is,
C       and how many kills will be attracted to that size class.  

        DO 350 ISIZ = IPSMIN,MAXISZ
          IF (PSCALE(ISIZ) .GT. 1E-4) THEN            
            HOST = TREE(ISTD,ISIZ,1) * (1 - ALLKLL(ISTD,ISIZ) 
     &                                     - TOPKLL(ISTD,ISIZ))            
            SCKL = INT(TOTKL * PSCALE(ISIZ) / TOTSCL) 

C           Determine whether the size class can absorb the target # of
C           kills, and calculate ATTPRP accordingly.  Reduce BKP for
C           the amount used.
     
            IF (SCKL .GE. HOST * CAREA) THEN
              ATTPRP = HOST / TREE(ISTD,ISIZ,1)                         
              BKPUSE = HOST * MSBA(IPSMIN)
            ELSE
              ATTPRP = SCKL / (TREE(ISTD,ISIZ,1) * CAREA)
              BKPUSE = (SCKL / CAREA) * MSBA(IPSMIN)
            ENDIF

            BKPIP = BKPIP - BKPUSE
            
C           If trees are larger than IPSMAX, they are only topkilled,
C           while if they are smaller, they are entirely killed.
C           Both arrays are a proportion.

            IF (ISIZ .GT. IPSMAX) THEN
               TOPKLL(ISTD,ISIZ) = TOPKLL(ISTD,ISIZ) + ATTPRP

               IF (TOPKLL(ISTD,ISIZ) .GT. 1.0) THEN
                 TOPKLL(ISTD,ISIZ) = 1.0 
               ENDIF
            ELSE
               ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) + ATTPRP                     

               IF (ALLKLL(ISTD,ISIZ) .GT. 1.0) THEN
                 ALLKLL(ISTD,ISIZ) = 1.0 
               ENDIF
            ENDIF   
            
          ENDIF 
  350   CONTINUE
      
C       Recalculate TOTSCL and AVKLBA to remove extinct size classes
C       from the cumulative distribution.  If there is no suitable 
C       host left, set BKP to zero (all remaining BKP is lost).

        DO 500 ISIZ = IPSMIN,MAXISZ                                        
          TOTATT = TOPKLL(ISTD, ISIZ) + ALLKLL(ISTD, ISIZ)
          IF (TOTATT .GE. 1.0) THEN
             PSCALE(ISIZ) = 0.0
             FULL(ISIZ) = 1
          ENDIF
  500   CONTINUE

        TOTSCL= 0.0
        AVKLBA= 0.0
        DO 510 ISIZ= IPSMIN, MAXISZ          
          TOTSCL= TOTSCL + PSCALE(ISIZ)
          AVKLBA= AVKLBA + (PSCALE(ISIZ) * MSBA(IPSMIN))
  510   CONTINUE

        IF (TOTSCL .GT. 1E-4) THEN
          AVKLBA= AVKLBA / TOTSCL
          TOTKL= BKPIP * CAREA / AVKLBA
        ELSE
          BKPIP= 0.0                                    
          TOTKL= 0.0
        ENDIF

      IF (TOTKL .GE. (2 * NUMSIZ)) THEN
C       There is enough remaining Beetle Kill Potential el ips to kill
C       two trees in each available size class so must perform the
C       group kill process again.

        GOTO 330
      ENDIF

  555 CONTINUE

C**********************************************************************
C If there is any BKP left, start using the individual kill routine.
C**********************************************************************

C.... 1/carea = 1 tree

      ATTP = (1.0 / CAREA)

C     First, turn PSCALE into a cummulative distribution

      IF (IPSMIN .EQ. 1) THEN
         PSCALE(1) = PSCALE(1) / TOTSCL
         IMIN = 2
      ELSE
         DO 425 ISIZ = 1, IPSMIN
            PSCALE(ISIZ) = 0.0
  425    CONTINUE
         IMIN = IPSMIN
      ENDIF

      DO 450 ISIZ= IMIN,NSCL
         PSCALE(ISIZ) = PSCALE(ISIZ-1) + PSCALE(ISIZ) / TOTSCL
  450 CONTINUE      
  

c     Choose a random number which will determine the size class
C     to attack.

  400 CONTINUE                    

       IF (BKPIP .GT. 1E-6) THEN

C       Determine which size class to attack

          CALL BMRANN(X)

          DO 475 ISIZ= IPSMIN, MAXISZ
            IF (X .LT. PSCALE(ISIZ)) GOTO 480
  475     CONTINUE 
          IF (ISIZ .GT. MAXISZ) ISIZ = MAXISZ
  480     CONTINUE

          IF (TREE(ISTD,ISIZ,1) .GT. 0.0) THEN
             ATTPRP = ATTP / TREE(ISTD,ISIZ,1)

C            Amount of BKP going into tree. Even if the tree is large,
c            more shouldn't go in than would kill a tree of IPSMAX
c            size.
C             IF (ISIZ .GT. IPSMAX) THEN 
C                ISZ = IPSMAX
C             ELSE
C                ISZ = ISIZ
C             ENDIF
C             ADD = MSBA(ISZ) * ATTP      

C            All trees absorb the same amount that the smallest tree does
             ADD = MSBA(IPSMIN) * ATTP      

C            Amount to kill (or top-kill)
             NEED = ADD * GRF(ISTD,ISIZ)
             
             IF (BKPIP .GE. NEED) THEN
                BKPIP = BKPIP - ADD

C            Topkill and Allkill are proportions

                IF (ISIZ .GT. IPSMAX) THEN
                   TOPKLL(ISTD,ISIZ) = TOPKLL(ISTD,ISIZ) + ATTPRP
                   IF (TOPKLL(ISTD,ISIZ) .GT. 1.0) 
     &                                   TOPKLL(ISTD,ISIZ) = 1.0
                ELSE
                   ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) + ATTPRP
                   IF (ALLKLL(ISTD,ISIZ) .GT. 1.0) 
     &                                   ALLKLL(ISTD,ISIZ) = 1.0
                ENDIF
         
             ELSE

                BKPIP = 0.0

             ENDIF
            
C....        Check if all classes are full:

             TOTATT = TOPKLL(ISTD, ISIZ) + ALLKLL(ISTD, ISIZ)
             IF ((TOTATT .GE. 1.0) .AND. (FULL(ISIZ) .EQ. 0)) THEN
                 FULL(ISIZ) = 1
                 CHECK = CHECK + 1  
          
C                If a new class is full, then remove it from pscale, to
C                make sure the program wont waste time picking it again.
                 PSCALE(ISIZ) = 0.0     
                 TEMP = 0.0                    
                 DO 625 ISZ = IPSMIN,NSCL
                    PSCALE(ISZ) = PSCALE(ISZ) * TOTSCL
                    TEMP = TEMP + TOTSCL
  625            CONTINUE             
                 TOTSCL = TEMP       
                 IF (IPSMIN .EQ. 1) THEN
                    PSCALE(1) = PSCALE(1) / TOTSCL
                    IMIN = 2
                 ELSE
                    IMIN = IPSMIN
                 ENDIF
                 DO 650 ISZ= IMIN,NSCL
                   PSCALE(ISZ) = PSCALE(ISZ-1) + PSCALE(ISZ) / TOTSCL
  650            CONTINUE      

             ENDIF
             IF (CHECK .EQ. (NSCL - IPSMIN + 1)) BKPIP = 0.0
      
          ENDIF
       ENDIF
      IF (BKPIP .GT. 1E-6) GOTO 400
      
      DO 600 ISIZ=1, NSCL

         IF (TOPKLL(ISTD,ISIZ) .GT. 1.0) TOPKLL(ISTD,ISIZ) = 1.0
         IF (ALLKLL(ISTD,ISIZ) .GT. 1.0) ALLKLL(ISTD,ISIZ) = 1.0

C        Change ALLKLL into stems/acre         

         ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)
         
C        Add killed trees to standing dead pools
         J = L2D(ISIZ) + 1    
         K = IQPTYP(ISTD,1)
         SDWP(ISTD,K,J,1) = SDWP(ISTD,K,J,1) + 
     &                         ALLKLL(ISTD,ISIZ) * TVOL(ISTD,ISIZ,1)

  600 CONTINUE
      
C.... Common Return

 9999 CONTINUE   
 
      BKPIPS(ISTD) = 0.0

      IF(LBMDEB) WRITE(JBMBPR,99) ISTD
   99 FORMAT(' End BMIPS: Stand= ', I6)

      RETURN
      END                                                              
