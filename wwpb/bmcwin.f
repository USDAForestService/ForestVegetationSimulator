      SUBROUTINE BMCWIN (ISTD,IYR)
C----------
C WWPB $Id$
C----------
*     CALLED FROM:  BMDRV
*     CALLS   GPGET2
*             GPADD 
*             PCTILE
*             SPLAAR
***********************************************************************
*  **BMCWIN  Date of last revision:  June 9, 1994
*----------------------------------------------------------------------
*  Purpose:
*     Windthrow Model adapted from the windthrow model in the 
*     Western Root Disease Model.
*     Windthrow affects all trees equally, regardless of root infection
*       or stress.
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     CRASH:  Prop'n of eligible stems to windthrow
*     IPNT:   Pointer array for height order (for PCTILE)
*     ISIZ:   Loop counter for size classes
*     ITYP:   Loop counter over tree types
*     MWINHT  The minimum height that a tree must be before it can be windthrown
*     ROCR:   Sum of tree density*crown dominance
*     ROWIND: Proportion of eligible stems to knock over
*     SAREA:  Area of current stand
*     SUM:    Total susceptibility (used to normalize susceptibilities)
*     STEMS:  Stem frequency by tree type
*     TBPROB: Total # of trees to windthrow
*     TEMP:   Temporary array of heights (for passing to PCTILE)
*     TEMP2:  Temporary pointer (for sorting)            
*     TPCT:   Array holding height percentiles
*     TSTEMS: Total stem frequency
*     TT:     Density of trees windthrown
*     THRESH: Min # stems to windthrow
*     WDOM:   Min dom at which trees of each type are eliglible for windthrow
*     WINDN:  Total number of eligible stems to throw
*     WINDSP: Number of trees to windthrow
*     WINDW:  Tree density * crown dominance
*     WSUSC:  Susceptibility of tree types (i) to windthrow
*             (j=2=normalized susc)
*     WDOM:   Min dom at which trees of each type are eliglible for windthrow
*
*  Common block variables and parameters:
*     DWPHOS: From BMCOM; Array to hold density of downed host trees for
*             Ips (stratified by standing/dead and size class)
*     OAKILL: Array containing the proportion of trees in each size class 
*             and tree type killed by "other agents" (i.e. not pine beetles)
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.

      INTEGER ISIZ, ITYP, IJ
      INTEGER IEND
      INTEGER DUM(1)
      INTEGER IPNT(NSCL+NSCL)
      
      REAL SUM
      REAL TBPROB
      REAL TEMP(NSCL+NSCL), TEMP2
      REAL TT, TVAL
      REAL TSTEMS
      REAL TPCT(NSCL+NSCL)
      REAL WSUSC(2, 2)            
      
      REAL WINDSP(2), ROCR(2), STEMS(2)
      REAL WINDW(NSCL, 2), WDOM(2)

      DIMENSION PRMS(4)
      LOGICAL   LOK

      SAVE
      
C.... Initializations

C     maybe these should be initialized elsewhere? 


C     from root disease manual (.028 is white pine, lodgepole pine;i
C      0.111 is western & mountain hemlock, red cedar)

      WSUSC(1, 1) = .028
      WSUSC(2, 1) = .111

C     should be 80-100% (from root disease manual)

      WDOM(1) = 80
      WDOM(2) = 85

C.... Check for debug.

      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMCWIN: Year = ',I5, 'Stand = ', I6)
   
C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.   
   
      IF (ICNT .GE. BMEND) ICNT = 0      
      ICNT = ICNT + 1

C     Check if this is a year for windthrow

      IF (ICNT .EQ. 1) THEN

        IYR1= IYR
        NPRMS= 4

        CALL GPGET2 (305, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

        IF (LOK) THEN
          
          IYR2= IYR1 + IFIX(PRMS(1)) - 1
          MWINHT = PRMS(2)
          THRESH = PRMS(3)
          CRASH = PRMS(4)

          IF (LBMDEB) WRITE (JBMBPR,71) MICYC,IYR1,IYR2,PRMS(4),PRMS(3)
   71     FORMAT (/' BMCWIN: MICYC=', I5,' IYR1=',I5,' IYR2=',I5, 
     >      ' CRASH=',F7.4,' THRESH=',F7.4)

          IF (IYR2.GE.MIY(MICYC)) THEN
            PRMS(1) = IYR2 - MIY(MICYC) + 1
            CALL GPADD (KODE, MIY(MICYC), 305, NPRMS, PRMS(1), 1, DUM) 
            IYR2 = MIY(MICYC) - 1
            IF (LBMDEB) WRITE (JBMBPR,81) PRMS(1),IYR2
   81       FORMAT (/' IN BMCWIN:  WINDTHROW HAS BEEN SCHEDULED FOR ',
     >           'THE NEXT MASTER CYCLE. DURATION WILL BE ',F5.0,
     >           ' NEW IYR2=',I5)
          ENDIF
        ENDIF 
        IF (IYR .GT. IYR2) CRASH = 0.0
      ENDIF                           

C     If windthrow is not active this year then exit out of routine.

      IF (IYR .GT. IYR2 .OR. CRASH .LE. 0.0) RETURN

      CALL SPLAAR(ISTD,SAREA,IRC)
      
C.... -------------------- Begin Routine ----------------------------

C.... normalize relative susceptibility of different species to windthrow
      
C.... WSUSC(i,1)=susc of type i to windthrow (type 1=host, 2=nonhost)

      SUM = WSUSC(1, 1) + WSUSC(2, 1)

C.... WSUSC(i,2) = normalized susc.

      WSUSC(1, 2) = WSUSC(1, 1) / SUM
      WSUSC(2, 2) = WSUSC(2, 1) / SUM
      
C.... Sum stems by type (host,nonhost) and build pointer lists
C     to dominant/codominant classes
      
      IJ = 0
      DO 1000 ITYP=1,2
         DO 1100 ISIZ=1,NSCL
            IJ = IJ + 1
            TEMP(IJ) = HTS(ISTD,ISIZ,ITYP)
            IPNT(IJ) = IJ
 1100    CONTINUE
 1000 CONTINUE

C     Set up pointers to the hts in decending order for use by the PCTILE routine
      
      IEND= 2 * NSCL - 1
      DO 2000 IJ=1,IEND
         DO 2100 I=1,IEND
            IF (TEMP(IPNT(I)) .LT. TEMP(IPNT(I+1))) THEN
               TEMP2 = IPNT(I)
               IPNT(I) = IPNT(I+1)
               IPNT(I+1) = TEMP2
            ENDIF
 2100    CONTINUE
 2000 CONTINUE
      
      CALL PCTILE(NSCL+NSCL,IPNT,TEMP,TPCT,TVAL)
      
      IJ = 0
      DO 3000 ITYP=1,2
      
         WINDSP(ITYP) = 0.0
         STEMS(ITYP) = 0.0
         ROCR(ITYP) = 0.0
      
         DO 3100 ISIZ=1,NSCL
            IJ = IJ + 1
            
            IF ((TPCT(IJ) .GE. WDOM(ITYP)) .AND. 
     &                       (HTS(ISTD,ISIZ,ITYP) .GE. MWINHT)) THEN
               IF (TREE(ISTD,ISIZ,ITYP) .GT. 0.0) THEN
                  STEMS(ITYP) = STEMS(ITYP) + TREE(ISTD,ISIZ,ITYP)
                  WINDW(ISIZ,ITYP)= (CRS(ISTD,ISIZ,ITYP) / 100.0)
     &                                  * TREE(ISTD,ISIZ,ITYP)
                  ROCR(ITYP) = ROCR(ITYP) + WINDW(ISIZ,ITYP)
               ENDIF
            ENDIF
      
 3100    CONTINUE
 3000 CONTINUE
      
      TSTEMS = 0.0
      TBPROB = 0.0
      DO 4000 ITYP=1,2
         TSTEMS = TSTEMS + STEMS(ITYP)
         WINDSP(ITYP) = STEMS(ITYP) * WSUSC(ITYP, 2)
         TBPROB = TBPROB + WINDSP(ITYP)
 4000 CONTINUE
      
C...  Total number of stems to windthrow
      
      WINDN = TSTEMS * CRASH
      IF (WINDN .GT. TSTEMS) WINDN = TSTEMS
      
C.... Allocate windthrow to types, assuming that the density of stems
C.... is above min
      
      IF (WINDN .GE. THRESH) THEN
      
         DO 5000 ITYP=1,2
            WINDSP(ITYP) = WINDN * (WINDSP(ITYP) / TBPROB)
 5000    CONTINUE
      
C....    Now allocate these proportions into the "other agent kill" list
C        NB: BLOWDOWN IS NOT A PROPORTION!
C        I assume that no BLOWDN trees are left standing (pun, ahem!)
      
         DO 6000 ISIZ=1,NSCL
            DO 6100 ITYP=1,2
               IF ((TREE(ISTD,ISIZ,ITYP) .GT. 0.0)
     &            .AND. (ROCR(ITYP) .GT. 0.0)) THEN
                  TT = WINDSP(ITYP) * WINDW(ISIZ,ITYP) / ROCR(ITYP)
                  IF (TT .GT. (TREE(ISTD,ISIZ,ITYP) * .95)) 
     &               TT = TREE(ISTD,ISIZ,ITYP) * .95

                  J = L2D(ISIZ)
                  IF (ITYP .EQ. 1 .AND. J .GT. 0) 
     &                   DWPHOS(ISTD,2,J) = DWPHOS(ISTD,2,J) + TT

                  OAKILL(ISTD,ISIZ,ITYP) = OAKILL(ISTD,ISIZ,ITYP)
     &                                  + TT / TREE(ISTD,ISIZ,ITYP)
      
      
                  TT = TT * TVOL(ISTD,ISIZ,ITYP) * V2T
                  IF (J .GE. 1) THEN                
C                    large downed dead wood                     
                     DDWP(ISTD,2,1) = DDWP(ISTD,2,1) + TT
                  ELSE
C                    small downed dead wood                     
                     DDWP(ISTD,1,1) = DDWP(ISTD,1,1) + TT
                  ENDIF   
               ENDIF
 6100       CONTINUE
 6000    CONTINUE
      
      ELSE  

C....  if the windthrow event does not occur then reschedule the event for next year
      
         DO 7000 ITYP=1,2
            WINDSP(ITYP) = 0
 7000    CONTINUE
         WINDN = 0

      ENDIF
      
C.... Common Return

 9999 CONTINUE
      
      IF(LBMDEB) WRITE(JBMBPR,99) IYR
   99 FORMAT(' End BMCWIN: Year = ',I5)

      RETURN
      END
