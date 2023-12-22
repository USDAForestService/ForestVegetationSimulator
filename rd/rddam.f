      SUBROUTINE RDDAM (II,ICODES)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     This routine processes the root disease damage codes.
C
C  Called By :
C     DAMCDS  [FVS]
C
C  Calls :
C     DBCHK   (SUBROUTINE)   [PROGNOSIS]
C     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
C     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
C     RDSTP   (SUBROUTINE)   [ROOT DISEASE]
C
C  Arguments : 
C     II     - INT, (I)
C              Tree pointer (tree record number).
C     ICODES - INT, (I)
C              Damage/Severity codes array.
C
C  FVS common tree level arrays used:
C     IMC    - Management Code replaced use of input Tree history code.
C     IDTREE - Tree identification.
C     PROB   - Tree tally ..not the sampling probability.
C     HT     - Recorded tree height.
C     DBH    - Recorded diameter.
C     ISP    - Tree species numberic code.
C     ITRE   - Plot identification.
C     ITRUNC - Recorded truncated height.
C
C  Local Variables : 
C     IDAMC  - INT
C              Array which holds the damage codes for each root disease type.
C     NSRD   - INT
C              index to IDAMC array holding non-specific root disease damage code.
C
C  Common block variables used :
C
C  Revision History :
C  04/18/97 - Matthew K. Thompson
C    If a tree record's tree species was not of a root disease
C    host the model was trying to place it in a plot as an
C    uninfected tree.  Now if it is of a non-host species the tree
C    record is skipped and the subroutine is exited.
C  04/15/04 - Lance R. David (FHTET)
C    Added recognition of damage code 60 (non-specific root disease)
C    so that tree records with code 60 will be included with any
C    disease type (RRTYPE) specified for the simulation.
C  03/25/05 - Lance R. David (FHTeT)
C    Parenthesis were added to the conditional statements in the
C    processing of the damage codes. The default order of evaluation
C    resulted in (A or (B and C and D)) in stead of the correct 
C    evaluation order of ((A or B) and C and D). This resulted in
C    infected trees being interpreted as uninfected.
C  07/10/07 - Lance R. David (FHTET)
C    Time at which damage codes are processed is now at the end of 
C    keyword processing instead of during the reading of tree data.
C    So, tree data items that were passed as arguments are now 
C    available from the FVS common area. Original arguments were:
C    (II,IITH,IDD,ICODES,PROBB,HHT,DDBH,IISPI,ITREII,TTHT)
C    FVS array IMC(II) is used as replacement for input tree history
C    code (IITH).
C    No need for special handling of dead tree index (IREC2) because
C    dead trees are already at the end of the arrays. Argument II is
C    correct index value for both live and dead.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C----------
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'HTCAL.F77'
      INCLUDE 'ESTREE.F77'
      INCLUDE 'RDADD.F77'

C.... Argument variable declarations.

      INTEGER ICODES(6), II

C.... Local variable declarations.

      INTEGER IDAMC(ITOTRR+1), J, K, NSRD
      INTEGER IFIADAM(ITOTRR+1)
      LOGICAL DEBUG          
     
C.... Data statements.
C.... Input tree data root disease damage codes
C.... RD RRTYPE        FVS CODE  FIA CODE 
C....   1-Annos-P         64     21010
C....   2-Annos-S         64     21010
C....   3-Armillaria      61     21001
C....   4-Phellinus       62     21017
C....   5-non-specified   60     21000
C....     root disease
C.... RRTYPE CODE 1  2  3  4
      DATA IDAMC /64,64,61,62,60/
      DATA IFIADAM /21010, 21010, 21001, 21017, 21000/
      DATA NSRD /5/

C.... See if we need to do some debug.

      CALL DBCHK (DEBUG,'RDDAM',5,ICYC)
      IF (DEBUG)
     &  WRITE (JOSTND,10) II,IMC(II),IDTREE(II),ICODES,PROB(II),
     &                    HT(II),DBH(II),ISP(II),ITRE(II),ITRUNC(II)
   10 FORMAT(' ** IN RDDAM: II=', I4,' IMC=',I1,' IDTREE=',I4,
     &       ' ICODES=',6(I3),' PROB=',F8.3,' HT=',F6.2,' DBH=',F5.2,
     &       ' ISP=',I2,' PLOT=',I3,' THT=',I8)
 
C.... If infected plots are not specified then branch to statement 200. 
C.... LONECT is initialized in RDBLK1 to 0 and when the PLREAD keyword
C.... is used it is set to 2 (multiple plots).

      IRRSP = MAXRR
      IF (MAXRR .LT. 3) IRRSP = IDITYP(IRTSPC(ISP(II)))

      IF (DEBUG) WRITE (JOSTND,*)
     & '              IRRSP=',IRRSP,' IDAMC=',IDAMC
      
C.... If the tree record is of a non-host species then exit.

      IF (IRRSP .EQ. 0) GOTO 9000

C.... Set the plot id for use if PLOTINF keyword is used.
C.... If trees are outside all plots of interest (and therefore outside centers)
C.... then a plot id is not assigned (IDPLOT = 0).
      
      IF (LPLINF) THEN
         IDPLOT(II) = 0

         DO 90 K=1,50
            IF (ITRE(II) .EQ. IANPLT(IRRSP,K)) THEN
               IDPLOT(II) = ITRE(II)    
               GOTO 95
            ENDIF
   90    CONTINUE

   95    CONTINUE
      ENDIF

C.... If PLREAD or PLOTINF are not being used then don't test the tree 
C.... record against plot information.

      IF (LONECT(IRRSP) .NE. 2) GOTO 200

C.... See if the tree is in a diseased sub-plot.

      DO 100 K=1,50
         IF (ITRE(II) .EQ. IRDPLT(IRRSP,K)) GOTO 200
  100 CONTINUE
      GOTO 9000

  200 CONTINUE

C.... Process tree record.  Check to see if tree record represents a 
C.... dead tree.
C....
C.... (Note : When we allow more than one root disease species, more
C.... than one damage code may be allowed to be read on a tree record.  

      IF (IMC(II) .EQ. 7 .OR. IMC(II) .EQ. 9) THEN

C....    Check to see if dead tree is a root diseased stump.
C....    It is a diseased stump if it has a root disease damage code or
C....    it has a height of 1.5 feet or less (0 < HT(II) <= 1.5).
C....    Trees that have been cut in previous rotations and have now
C....    been infected with root disease are flagged as stumps by
C....    recording them as dead and recording a value of 1.5 in the 
C....    variable HT(II).
C....
C....    FVS CODES CHECKED FIRST AND THEN FIA CODES
         IF (ICODES(1) .EQ. IDAMC(IRRSP) .OR. 
     &       ICODES(1) .EQ. IDAMC(NSRD) .OR.
     &       ICODES(3) .EQ. IDAMC(IRRSP) .OR. 
     &       ICODES(3) .EQ. IDAMC(NSRD) .OR. 
     &       ICODES(5) .EQ. IDAMC(IRRSP) .OR. 
     &       ICODES(5) .EQ. IDAMC(NSRD) .OR. 
     &       ICODES(1) .EQ. IFIADAM(IRRSP) .OR. 
     &       ICODES(1) .EQ. IFIADAM(NSRD) .OR.
     &       ICODES(3) .EQ. IFIADAM(IRRSP) .OR. 
     &       ICODES(3) .EQ. IFIADAM(NSRD) .OR. 
     &       ICODES(5) .EQ. IFIADAM(IRRSP) .OR. 
     &       ICODES(5) .EQ. IFIADAM(NSRD) .OR. 
     &       (HT(II) .GT. 0 .AND. HT(II) .LE. 1.5)) THEN
            
C....       Place a flag in the tree list that identifies this as an infected
C....       dead tree. The final stump calculations will now be done in RDPRIN
C....       once the density of stumps is calculated.

            IPRFL(II) = 4                    

            RISTU(IRRSP) = RISTU(IRRSP) + 1

            IF (DEBUG) WRITE (JOSTND,1350) IDTREE(II)
 1350       FORMAT (' RECORD NUMBER: ',I7,' IS AN INFECTED STUMP')
         ENDIF

      ELSE

C....    Process live trees to determine if infected or uninfected.
C....    'Suspect' trees are considered to be uninfected. These are 
C....    trees that are within 30 feet of a tree killed by root disease
C....    and whose DBH < 5".  This recommendation for 'suspect' trees was
C....    made by Byler and Goheen.

         DO 900 J=1,5,2
            IF((ICODES(J) .EQ. IDAMC(IRRSP) .OR.
     &          ICODES(J) .EQ. IDAMC(NSRD)  .OR.
     &          ICODES(J) .EQ. IFIADAM(IRRSP) .OR.
     &          ICODES(J) .EQ. IFIADAM(NSRD))
     &         .AND. ICODES(J+1) .EQ. 1 
     &         .AND. DBH(II) .LE. 5.0) THEN

C....          Tree is uninfected but within a center.

               IF (DEBUG) WRITE (JOSTND,1050) IDTREE(II), J
 1050          FORMAT (' RECORD NUMBER: ',I7,' IS UNINFECTED A, J=',I1)
               IPRFL(II) = 5
               RINUF(IRRSP) = RINUF(IRRSP) + 1.0
               GOTO 9000
             
            ELSEIF((ICODES(J) .EQ. IDAMC(IRRSP) .OR.
     &              ICODES(J) .EQ. IDAMC(NSRD)  .OR.
     &              ICODES(J) .EQ. IFIADAM(IRRSP) .OR.
     &              ICODES(J) .EQ. IFIADAM(NSRD))
     &             .AND. ICODES(J+1) .LE. 1
     &             .AND. DBH(II) .GT. 5.0) THEN

C....          Tree is within 30 feet of infected tree.

               IF (DEBUG) WRITE (JOSTND,1150) IDTREE(II), ICODES(J+1)
 1150          FORMAT (' RECORD #: ',I7,' IS INFECTED WITH SEVERITY ',
     &                 I5)
               IPRFL(II) = 1
               RINNF(IRRSP) = RINNF(IRRSP) + 1.0
               GOTO 9000

            ELSEIF((ICODES(J) .EQ. IDAMC(IRRSP) .OR. 
     &              ICODES(J) .EQ. IDAMC(NSRD)  .OR.
     &              ICODES(J) .EQ. IFIADAM(IRRSP) .OR.
     &              ICODES(J) .EQ. IFIADAM(NSRD))
     &             .AND. ICODES(J+1) .EQ. 2) THEN
           
C....          Pathogen or diagnostic symptoms detected.

               IF (DEBUG) WRITE (JOSTND,1250) IDTREE(II), ICODES(J+1)
 1250          FORMAT (' RECORD #: ',I7,' IS INFECTED WITH SEVERITY ',
     &                 I5)
               IPRFL(II) = 2
               RINNF(IRRSP) = RINNF(IRRSP) + 1.0
               GOTO 9000

            ELSEIF((ICODES(J) .EQ. IDAMC(IRRSP) .OR.
     &              ICODES(J) .EQ. IDAMC(NSRD)  .OR.
     &              ICODES(J) .EQ. IFIADAM(IRRSP) .OR.
     &              ICODES(J) .EQ. IFIADAM(NSRD))
     &             .AND. ICODES(J+1) .EQ. 3) THEN
           
C....          Crown deterioration.

               IF (DEBUG) WRITE (JOSTND,1750) IDTREE(II), ICODES(J+1)
 1750          FORMAT (' RECORD #: ',I7,' IS INFECTED WITH SEVERITY ',
     &                 I5)
               IPRFL(II) = 3
               RINNF(IRRSP) = RINNF(IRRSP) + 1.0
               GOTO 9000

            ENDIF
           
  900    CONTINUE

         IF (PAREA(IRRSP) .GT. 0.0) THEN

C....       Tree is uninfected but within a center.

            IF (DEBUG) WRITE (JOSTND,1850) IDTREE(II), J
 1850       FORMAT (' RECORD NUMBER: ',I7,' IS UNINFECTED B, J=',I1)
            IPRFL(II) = 5
            RINUF(IRRSP) = RINUF(IRRSP) + 1.0
            GOTO 9000
         ENDIF
           
      ENDIF

 9000 CONTINUE
      RETURN
      END
