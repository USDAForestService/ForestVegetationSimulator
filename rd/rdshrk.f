      SUBROUTINE RDSHRK
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     Reduces the radius of centers.  Once centers have started to
C     decay they are likely to continue decaying, unless there are
C     a significant number of new infections.  Centers decay in the  
C     same manner as the biggest stump present so the center radius
C     decays for a short time and then "sits" at some size for a 
C     while and then disappears.  Centers should therefor disappear
C     when there is no inculum left in the stand.
C
C  Called By :
C     RDCNTL  (ROUTINE)  [ROOT DISEASE]
C
C  Calls     :
C     RDSLP   (FUNCTION)   [ROOT DISEASE]
C
C  Common block variables Used :
C     SHCENT(,,1) = Root radius to decay immediately.
C     SHCENT(,,2) = Annual rate to decay that amount of root.
C     SHCENT(,,3) = Time to decay remaining amount of roots
C
C  Local Variables :
C
C     DEBUG:  Logical flag to turn debug on or off.
C
C Revision History:
C   24-FEB-2004 Lance R. David (FHTET)
C      At DO loop 700 a GOTO loop existed within it which lead to
C      an array out of bounds error.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77' 
      INCLUDE 'RDARRY.F77'
      INCLUDE 'PLOT.F77' 
      INCLUDE 'RDADD.F77'

C.... Local variables.

      LOGICAL DEBUG 
      INTEGER I, I1, I2, ICEN, IDI, IJ, IPNT(100), IRSP, ISHR, ITMP,
     &        J, KNT, KSP, NDIF
      REAL    DBHM, REDAMT, ROOTM, RSFAC, TCLAS, TMINLF, TSTU, YREM

C.... SEE IF WE NEED TO DO SOME DEBUG.

      CALL DBCHK (DEBUG,'RDSHRK',6,ICYC)

      IF (DEBUG) WRITE (JOSTND,999) ICYC
  999 FORMAT (' Begin RDSHRK, Cycle = ', I5)

C.... SEE IF THERE IS ANY REASON TO DECREASE THE SIZE OF CENTERS
C.... FIRST CHECK THE NUMBER OF LIVE (INFECTED AND UNINFECTED) INSIDE CENTERS

      IDI = IRRSP
      
      TCLAS = 0.0         
      IRSP = IDI

      DO 800 KSP = 1,MAXSP
         IF (IDI .LT. 3) IRSP = IDITYP(IRTSPC(KSP))
         IF ((ISCT(KSP,1) .EQ. 0) .OR. (IRSP .NE. IDI)) GOTO 800
            
         I1 = ISCT(KSP,1)
         I2 = ISCT(KSP,2)
            
         DO 750 J = I1,I2
            I = IND1(J)
            TCLAS = TCLAS + PROBIU(I) + PROBIT(I)
  750    CONTINUE
  800 CONTINUE

C.... IF THERE ARE MORE TREES THAN CENTERS THEN NO NEED TO SHRINK CENTERS
C.... AND ANY CENTERS THAT WERE SHRINKING STOP

      IF (TCLAS .GE. NCENTS(IDI)) THEN
         IF (NSCEN(IDI) .GT. 0.0) THEN

            DO 50 ICEN= 1,NCENTS(IDI)
               IF (SHCENT(IDI,ICEN,2) .GT. 0.0) THEN 
                  SHCENT(IDI,ICEN,1) = 0.0
                  SHCENT(IDI,ICEN,2) = 0.0
                  SHCENT(IDI,ICEN,3) = 0.0 
                  RRATES(IDI,ICEN) = 0.0
               ENDIF
   50       CONTINUE

            NSCEN(IDI) = 0
        ENDIF

        GOTO 9000
      ENDIF

C.... Number of centers to shrink.         

      ISHR = NCENTS(IDI) - INT(TCLAS)
         
C.... Determine which centers to shrink                           
         
      IF (ISHR .GT. NSCEN(IDI)) THEN

C....    There are too few shrinking centers so start picking smallest
C....    non-shrinking ones to start shrinking.  Set up a list of
C....    pointers which references these indices.

         DO 1000 ICEN= 1, NCENTS(IDI)
            IPNT(ICEN) = ICEN
 1000    CONTINUE

         IF (ISHR .LT. NCENTS(IDI)) THEN

C....       Set up pointers to the size of centers in ascending order if
C....       not all centers will start shrinking.
      
            DO 2100 IJ=1,NCENTS(IDI) - 1
               DO 2000 I=1,NCENTS(IDI) - 1
                  IF (PCENTS(IDI,IPNT(I), 3) .GT. 
     &                PCENTS(IDI,IPNT(I+1),3)) THEN
                     ITMP = IPNT(I)
                     IPNT(I) = IPNT(I+1)
                     IPNT(I+1) = ITMP
                  ENDIF
 2000          CONTINUE
 2100       CONTINUE
         ENDIF

C....    Find the parameters to use to shrink centers (They'll be
C....    the same for all centers). Choose the biggest stump present,
C....    choosing from resinous first (since they take longer to decay)

         DBHM = 0.0
         ROOTM = 0.0
         TSTU = 0.0   
         RSFAC = 1.0                                  

         DO 500 J=5, 1, -1
            DO 400 I=1, 2
               IF (PROBD(IDI,I,J) .NE. 0.0) THEN
                  DBHM = DBHM + DBHD(IDI,I,J) * PROBD(IDI,I,J)
                  ROOTM = ROOTM + ROOTD(IDI,I,J) * PROBD(IDI,I,J) 
                  RSFAC = DSFAC(I)
                  TSTU = TSTU + PROBD(IDI,I,J)
                  IF (TSTU .GT. 0.0) GOTO 550
               ENDIF
  400       CONTINUE
  500    CONTINUE                     

  550    CONTINUE

         DBHM = DBHM / (TSTU + 1E-6)
         ROOTM = ROOTM / (TSTU + 1E-6)
          
         ROTSIT = RSITFN(IDI,1) * DBHM + RSITFN(IDI,2)

         IF (DBHM .LE. 12) THEN
            JRSIT = INT(YRSITF(IDI,1,1) * DBHM + YRSITF(IDI,2,1))
         ELSE   
            JRSIT = INT(YRSITF(IDI,1,2) * DBHM + YRSITF(IDI,2,2))
         ENDIF
         
         KNT = 1

         DO 300 IJ=1,(ISHR-NSCEN(IDI))

  350       CONTINUE

            ICEN = IPNT(KNT)
            KNT = KNT + 1

            IF (SHCENT(IDI,ICEN,2) .EQ. 0.0) THEN 
           
C....          This center has not yet started shrinking so assign
C....          values.
           
               SHCENT(IDI,ICEN,1) = ROOTM - ROTSIT
               SHCENT(IDI,ICEN,3) = JRSIT
               IF (DBHM .LE. 12) THEN
                  SHCENT(IDI,ICEN,2) = (DECFN(IDI,1,1) * ROOTM + 
     &                                  DECFN(IDI,2,1)) / RSFAC
               ELSE
                  SHCENT(IDI,ICEN,2) = (DECFN(IDI,1,2) * ROOTM + 
     &                                  DECFN(IDI,2,2)) / RSFAC
               ENDIF
             
               IF (SHCENT(IDI,ICEN,1) .LT. 0.0) SHCENT(IDI,ICEN,1) = 0.0

C....          Modify the shrink rate to account for the minimum lifespan
C....          of inoculum.

               TMINLF = JRSIT + SHCENT(IDI,ICEN,1) / SHCENT(IDI,ICEN,2)

               IF (TMINLF .LT. XMINLF(IDI)) THEN
                  SHCENT(IDI,ICEN,2) = SHCENT(IDI,ICEN,1) /
     &                                 (XMINLF(IDI) - JRSIT)
               ENDIF

            ELSE
               GOTO 350
            ENDIF 

  300    CONTINUE 

         NSCEN(IDI) = ISHR 
          
      ELSEIF (ISHR .LT. NSCEN(IDI)) THEN

C....    There are too many shrinking centers so need to stop shrinking some.
C....    Choose biggest centers to stop shrinking, again, set up a list of pointers
C....    to the biggest centsrs.

         DO 4000 ICEN= 1,NCENTS(IDI)
            IPNT(ICEN) = ICEN
 4000    CONTINUE

         IF (ISHR .GT. 0) THEN

C....       Set up pointers to the size of centers in decending order if 
C....       only some centers will stop shrinking (since if all centers stop
C....       shrinking, it doesn't matter what order we stop them in)
      
            DO 5100 IJ = 1, NCENTS(IDI) - 1
               DO 5000 I=1, NCENTS(IDI) - 1
                  IF (PCENTS(IDI,IPNT(I),3) .LT.
     &                PCENTS(IDI,IPNT(I+1),3)) THEN
                     ITMP = IPNT(I)
                     IPNT(I) = IPNT(I+1)
                     IPNT(I+1) = ITMP
                  ENDIF
 5000          CONTINUE
 5100       CONTINUE
         ENDIF
         
C....    Loop through the centers and zero out the SHCENT arrays for those
C....    that will no longer shrink.  Also, set the spread rate to zero.

         KNT = 1
         NDIF = NSCEN(IDI) - ISHR

         IF (DEBUG) WRITE (JOSTND,*) 'IN RDSHRK: ISHR=',ISHR,
     &      ' NDIF=',NDIF,' NSCEN(IDI)=',NSCEN(IDI),' (AT 700 LOOP)'

         DO 700 I= 1,NDIF
CC  650       CONTINUE
            ICEN = IPNT(KNT)

           IF (DEBUG) WRITE (JOSTND,*) 'IN RDSHRK: I=',I,' KNT=',KNT,
     &         ' ICEN=',ICEN

            KNT = KNT + 1
            
             IF (SHCENT(IDI,ICEN,2) .GT. 0.0) THEN 
               SHCENT(IDI,ICEN,1) = 0.0
               SHCENT(IDI,ICEN,2) = 0.0
               SHCENT(IDI,ICEN,3) = 0.0 
               RRATES(IDI,ICEN) = 0.0
               NSCEN(IDI) = NSCEN(IDI) - 1
CC            ELSE
CC               GOTO 650
            ENDIF
  700    CONTINUE
      ENDIF

      IF (DEBUG) WRITE (JOSTND,*) 'IN RDSHRK: NCENTS(IDI)=',
     &   NCENTS(IDI),' (AT 900 LOOP)'
                  
      DO 900 I = 1, NCENTS(IDI)
         IF (SHCENT(IDI,I,2) .EQ. 0.0) GOTO 900
           
         IF (SHCENT(IDI,I,1) .GT. 0.0) THEN
            REDAMT = SHCENT(IDI,I,2) * FINT

            IF (SHCENT(IDI,I,1) .LT. REDAMT) THEN
             
C....          Only reduce this first amount in the initial timestep.
             
               REDAMT = SHCENT(IDI,I,1)         
                
C....          But since it didn't take all the timesteps to reduce the
C....          initial amount,  reduce the time remaining (INDX 3) by
C....          the rest of the timestep.
             
               YREM = FINT - SHCENT(IDI,I,1) / SHCENT(IDI,I,2)
               SHCENT(IDI,I,3) = SHCENT(IDI,I,3) - YREM       
            ENDIF

C....       Reduce the initial radius parameter here.            
             
            SHCENT(IDI,I,1) = SHCENT(IDI,I,1) - REDAMT
            SHCENT(IDI,I,1) = AMAX1(SHCENT(IDI,I,1),0.0)
             
C....       Set a negative rate for use later in RDCNTL.
             
            RRATES(IDI,I) = -REDAMT / FINT
         ELSE

C....       If the initial amount is gone, no shrinkage occurs.

            RRATES(IDI,I) = 0.0 
             
C....       But the time remaining to the center gets reduced and
C....       If no time left then set the rate so that the center
C....       disappears.

            SHCENT(IDI,I,3) = SHCENT(IDI,I,3) - FINT
            IF (SHCENT(IDI,I,3) .LE. 0.0) 
     &           RRATES(IDI,I) = -PCENTS(IDI,I,3) / FINT
         ENDIF
  900 CONTINUE
             
 9000 CONTINUE
 
      IF (DEBUG) WRITE (JOSTND,9100) ICYC
 9100 FORMAT (' End RDSHRK, Cycle = ', I5)

      RETURN
      END
