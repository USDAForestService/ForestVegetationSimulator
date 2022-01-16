      SUBROUTINE BMSLSH (ISPC,TTREE,TVL,ISTD)                              
      
*     CALLED FROM: CUTS   [PROGNOSIS]
*                  BMSANI [BEETLE]
*                  BMSALV [BEETLE]  
***********************************************************************
* **BMSLSH    Date of last revision: Dec 6, 2006 [AJM]
*             Date of last revision: July 8, 1994
*    
* Calculate tons/acre and eqivalent stems/acre slash produced as 
* a result of various thinning, salvage and sanitation cuts. Even if
* not a host type, slash is added to standing and dead wood.
*
* Definitions
*     VOLCUT  Volume harvested 
*     PDOWN   Prop slash down on ground        
*     PSLSH   Prop of standing and downed in each size class
*     T2STEM  Tons/ac -> stem/ac conversion factor             
*     VOL2SL  Total volume of harvest -> total weight of resulting slash
***********************************************************************
C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'BMCOM.F77'

C.... Variable declarations.

      LOGICAL LX 
      REAL    NEWSL
      REAL    PDOWN
      REAL    PSLSH(2,MXDWHZ+1)
      REAL    T2STEM(MXDWHZ) 
      REAL    VOL2SL 
      REAL    Z
      
      DATA T2STEM /12,1.4,0.33/  
C     Note: T2STEM is calculated based on 1t=83.3cu.ft and sample trees
c           with the following characteristics:  sc1: 6.5"dbh, 60'ht; 
c           sc2: 15"dbh, 95'ht; sc3: 25"dbh, 120'ht 
c          and a form factor = 0.5 for each.
      DATA ((PSLSH(I,J), J=1,MXDWHZ+1), I=1,2) 
     &       /0.1,0.3,0.6,0.0, 0.7,0.2,0.2,0.0/
C     Note: PSLSH assumes different proportions for standing & downed wood. 
c           Standing wood is the first 4 values (different size classes), 
c           and downed is last 4
      DATA VOL2SL /0.005/ 
c     Note: VOL2SL assumes that slash equals 40% of the weight of the 
c           total volume

      PDOWN = 0.95
      LX= .FALSE.
      
      IF (ISTD .EQ. 0) THEN
C        Routine was called from CUTS.

C        If WWPBM not invoked, bail [AJM 12/06] 
         IF (BMSTND .LE. 0) GOTO 100
         
C        FIND OUT IF THE CURRENT STAND IS IN THE DISPERSAL GROUP
         CALL OPBISR (BMSTND,BMSDIX,ISTND,ISTD)

C        IF IBMSTD IS ZERO, THEN BRANCH TO EXIT.

         IF (ISTD .LE. 0) GOTO 100

c        Determine whether tree species is host or non-host 
         IF (  (((PBSPEC .EQ. 1) .OR. (PBSPEC .EQ. 4))
     >         .AND. (HSPEC(1,ISPC) .EQ. 1)) 
     >    .OR. (((PBSPEC .EQ. 2) .OR. (PBSPEC .EQ. 4))
     >         .AND. (HSPEC(2,ISPC) .EQ. 1))
     >    .OR. ((PBSPEC .EQ. 2) .AND. (HSPEC(2,ISPC) .EQ. 1))
     >    .OR. ((PBSPEC .EQ. 3) .AND. (HSPEC(3,ISPC) .EQ. 1)) )
     >        LX= .TRUE.  
         
C        Determine how much volume was cut
         VOLCUT = TTREE * TVL
         IPC = ISPFLL(ISPC)
      ELSE
C        Routine called from salvage or sanitation cuts

C        Determine how much volume was cut
         VOLCUT = TTREE * TVL
         IPC = ISPC
         IF (IPC .EQ. IQPTYP(ISTD,1)) LX= .TRUE. 
      ENDIF
      
C     This is the new way of figuring out the amount of slash that is left behind
c     after a cut.  

      NEWSL = VOL2SL * VOLCUT
      
      Z = 1.0 - PDOWN
      IF (LX) THEN
        DO 40 I= 1, MXDWHZ
C         (need to make this J because class 1 of pslash is < 3in, while class 1 of
c             dwphos is 3-10)
          J = I + 1
          DWPHOS(ISTD,2,I) = DWPHOS(ISTD,2,I) + NEWSL * PSLSH(2,J) * 
     &                       PDOWN * T2STEM(I)
          DWPHOS(ISTD,1,I) = DWPHOS(ISTD,1,I) + NEWSL * PSLSH(1,J) * 
     &                       Z * T2STEM(I)

   40   CONTINUE
      ENDIF     
      DO 50 I= 1, (MXDWHZ + 1)
        JSC = MIN0(MXDWSZ,I)
                 
        DDWP(ISTD,JSC,1) = DDWP(ISTD,JSC,1) + NEWSL * PSLSH(2,I) * PDOWN
                 
        SDWP(ISTD,IPC,I,1) = SDWP(ISTD,IPC,I,1) + NEWSL * PSLSH(1,I) 
     &                                                  * Z / V2T
   50 CONTINUE      
      

C      IF(LBMDEB) WRITE(JBMBPR,90) IYR, ISTD
C   90 FORMAT(' End BMSLSH: Year = ',I5,' Stand = ',I6)

  100 CONTINUE
  
      RETURN
      END
