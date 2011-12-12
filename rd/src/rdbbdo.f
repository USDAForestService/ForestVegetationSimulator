      SUBROUTINE RDBBDO
C----------
C  **RDBBDO       LAST REVISION:  07/12/02
C---------- 
C
C  Purpose :
C     Determines for each component of each tree record the maximum
C     mortality that any one of the bark beetles that are currently
C     eligible to act could inflict on that tree record, and the number
C     of trees killed by applying this mortality rate.  Reports the
C     total number of trees killed via :
C     OAKL(DSO/DSIU/DSII,#TREES), RRKILL(#TREES), and
C     RROBNK(#TREESPECIES).
C  
C      **************************************************************
C      ** DOES NOT RECALCULATE THE NUMBER OF LIVE TREES REMAINING, ** 
C      ** except in the case of PROBI(#TREES,#GROWTHCYCLES) and    **
C      ** PROBIT(#TREES).                                          **
C      **************************************************************
C
C  Calls:
C     RDSUM   [ROOT DISEASE]
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Local Variables :
C     DSF    - INTEGER
C              Dead Standing Fringe (only calculated when BB4 is active
C              (will DSF=0 if BB4 is not active, because FRINGE=0 unless
C              BB4 is active)) (note windthrow is included in all cases,
C              "standing" dead is not really accurate)
C     MAXKL  - REAL
C              The maximum mortality rate applicable to the current
C              component of the current tree record (other than in
C              'fringe' areas).
C     MAXFKL - REAL
C              The maximum fringe-specific mortality rate applicable
C              to the current component of the current tree record.
C     NUMDED - REAL
C              The number of trees in the current component of the
C              current tree record that just died from bark beetle
C              attacks.
C     NUMDEF - REAL
C              NUMDED for trees in the "fringe".
C
C  Revision History :
C    06/12/96 - Matthew K. Thompson
C      Moved the declaration of DSO, DSII, and DSIU to the
C      parameter include file RDPARM.
C    12-JUL-2002 Lance R. David
C      Added debug code.
C----------------------   

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'

C.... Local variable declarations.

      INTEGER DSF, RRTYPE, I,IK,BB, ISPI
      REAL    MAXKL, MAXFKL, NUMDED, NUMDEF
      LOGICAL DEBUG
      
C.... Data statements.

      DATA DSF /4/

C.... SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'RDBBDO',6,ICYC)
      IF (DEBUG) WRITE (JOSTND,*) 'ENTER: RDBBDO'

      IF (ITRN .LE. 0) RETURN
      
C.... Look at every tree record.  Figure out what tree species it is
C.... and what root rot type attacks it.
      
      DO 999 I = 1,ITRN
         ISPI = ISP(I)
         RRTYPE = MAXRR
         IF (MAXRR .LT. 3) RRTYPE = IDITYP(IRTSPC(ISPI))

         IF (DEBUG) WRITE (JOSTND,*) 'IN: RDBBDO I=',I,' ISPI=',ISPI,
     &   ' RRTYPE=',RRTYPE

C....    Kill Inside Infected trees:  look at the portion of the record
C....    infected in each time period, find the maximum
C....    bark-beetle-induced mortality that applies to it, and kill it
C....    accordingly.
        
         TOTDED = 0.0

         DO 333 IK = 1,ISTEP 
            DO 255 IP = 1,2
               IF (PROBI(I,IK,IP) .EQ. 0.0) GOTO 255
               MAXKL = 0.0
          
               DO 222 BB = 1,NUMBB
                  IF ((HOST(BB).EQ.ISPI) .AND.
     &                (MINDBH(BB).LE.DBH(I))) THEN 

                     IF (MININF(BB) .LE. PROPI(I,IK,IP)) THEN
                        IF (IIRATE(BB) .GT. MAXKL) MAXKL = IIRATE(BB)
                     ELSE 
                        IF (IURATE(BB) .GT. MAXKL) MAXKL = IURATE(BB)
                     ENDIF

                  ENDIF
  222          CONTINUE
          
               IF (MAXKL .GT. 0.0) THEN
                  NUMDED = PROBI(I,IK,IP) * MAXKL
                  PROBI(I,IK,IP) = PROBI(I,IK,IP) - NUMDED
                  OAKL(DSII,I) = OAKL(DSII,I) + NUMDED
                  BBKILL(DSII,I) = BBKILL(DSII,I) + NUMDED
                  RRKILL(I) = RRKILL(I) + NUMDED
                  RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
                  TOTDED = TOTDED + NUMDED
               ENDIF                  
  255       CONTINUE     
  333    CONTINUE
  
C....    Update the stump list with beetle-killed, infected trees.

         IF (TOTDED .GT. 0.0) THEN
            CALL RDSSIZ(ISPI,DBH(I),STCUT,ISL,ISPS,IRTSPC)
            CALL RDSTP (ISL,ISPI,TOTDED,DBH(I),ROOTL(I))
         ENDIF                                            

C....    Kill Inside Uninfected trees:  find the maximum applicable
C....    mortality, and apply it.
        
         IF (PROBIU(I) .EQ. 0.0) GOTO 555
         MAXKL = 0.0
        
         DO 444 BB = 1,NUMBB
            IF ((HOST(BB) .EQ. ISPI) 
     &          .AND. (MINDBH(BB) .LE. DBH(I)) 
     &          .AND. (IURATE(BB) .GT. MAXKL)) THEN
               MAXKL = IURATE(BB)
            ENDIF
  444    CONTINUE

         IF (DEBUG) WRITE (JOSTND,*) 'IN: RDBBDO PROBIU=',PROBIU(I),
     &   ' MAXKL=',MAXKL,' KILL INSIDE UNINFECTED'

         IF (MAXKL .GT. 0.0) THEN  
            NUMDED = PROBIU(I) * MAXKL
            OAKL(DSIU,I) = OAKL(DSIU,I) + NUMDED
            BBKILL(DSIU,I) = BBKILL(DSIU,I) + NUMDED
            RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
            IF (DEBUG) WRITE (JOSTND,*) 'IN: RDBBDO NUMDED=',NUMDED,
     &      ' DSIU=',DSIU,' OAKL=',OAKL(DSIU,I),' BBKILL=',
     &      BBKILL(DSIU,I),' RROBNK=',RROBNK(ISPI)
         ENDIF

  555    CONTINUE
  
C....    Kill Outside trees:  find the maximum mortality applicable
C....    to trees in the "fringe" and non-fringe areas of the stand,
C....    and apply these mortality rates in proportion to the number
C....    of trees in each area.   Note that FPROB is a density of trees,
C....    not a total number of trees.

         IF (FPROB(I) .EQ. 0.0) GOTO 777
         MAXKL = 0.0
         MAXFKL = 0.0
        
         DO 666 BB = 1, NUMBB
            IF ((HOST(BB) .EQ. ISPI) .AND.
     &          (MINDBH(BB) .LE. DBH(I))) THEN
               IF (ORATE(BB) .GT. MAXKL) MAXKL = ORATE(BB)
               IF (OFRATE(BB) .GT. MAXFKL) MAXFKL = OFRATE(BB)
            ENDIF 
  666    CONTINUE
        
         IF ((MAXKL .GT. 0.0) .OR. (MAXFKL .GT. 0.0)) THEN
            NUMDEF = FPROB(I) * FRINGE(RRTYPE) * MAXFKL  
            NUMDED = FPROB(I) *(SAREA-PAREA(RRTYPE)-FRINGE(RRTYPE)) *
     &               MAXKL + NUMDEF
            BBKILL(DSF,I) = BBKILL(DSF,I) + NUMDEF
            BBKILL(DSO,I) = BBKILL(DSO,I) + NUMDED
            OAKL(DSO,I) = OAKL(DSO,I) + NUMDED
            RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
            FFPROB(I,2) = FFPROB(I,2) - (FFPROB(I,2) * MAXFKL)
         ENDIF 

  777    CONTINUE        

C....    End of loop over tree records.

  999 CONTINUE
  
C.... Get PROBIT totalled up for the new PROBI values.

      CALL RDSUM (ITRN,PROBIT,PROBI,ISTEP)

      IF (DEBUG) WRITE (JOSTND,*) 'EXIT: RDBBDO'

      RETURN
      END
