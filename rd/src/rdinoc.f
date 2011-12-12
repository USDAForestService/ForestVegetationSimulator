      SUBROUTINE RDINOC(LICALL)
C----------
C  **RDINOC      LAST REVISION:  07/30/02 
C----------
C
C  Purpose :
C     Decomposes infected root systems in the dead tree/stump list.
C
C  Called By :         
C     RDSETP  [ROOT DISEASE]
C     RDTREG  [ROOT DISEASE]
C
C  Calls :
C     RDSLP   (FUNCTION)   [ROOT DISEASE]
C
C  Common block variables used :                   
C     DECRAT - Decay rate of roots.
C     JRSIT  - Number of years to 'sit' without decreasing root
C              radius
C     ROTSIT - Root radius at which to begin 'sitting' without
C              decreasing (feet)
C
C  Local variables :
C
C     DEBUG  - Logical flag to turn debug on or off.
C     JINT   - Interval over which to decay roots.
C     LICALL - .TRUE. = Called during initialization, .FALSE. = Called
C              elsewhere.
c     RTODEC - Root radius to decay this cycle.
C     RTREM  - Root radius remaining after decay.
C
C Revision History:
C   26-JUN-2002 Lance David (FHTET)
C     (last revision date noted was 3/25/97)
C     Added debug code to track down devide by zero error that has
C     occurred.
C   30-JUL-2002 Lance David (FHTET)
C     Added check and reset of root radius when current radius is 
C     less than the value calculated as the radius at which the 
C     root system is suppose to sit for x years.
C-----------------------------------------
C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'PLOT.F77'

C.... Local variables.

      LOGICAL DEBUG  
      LOGICAL LICALL

C.... See if we need to do some debug. 

      CALL DBCHK (DEBUG,'RDINOC',6,ICYC)

      IF (DEBUG) THEN
         WRITE (JOSTND,900) ICYC,LICALL
  900    FORMAT (' Begin RDINOC, Cycle = ', I5,' LICALL=',L)
      ENDIF
      
      JINT = FINT
      
      DO 600 IDI=MINRR,MAXRR
         DO 500 I=1, 2
            DO 400 J=1, 5
               DO 300 K=1, ISTEP
                  IF (DEBUG) WRITE (JOSTND,*) 
     &            'IN RDINOC: IDI=',IDI,' I=',I,' J=',J,' K=',K

                  IF (PROBDA(IDI,I,J,K) .NE. 0.0 .AND.
     &                DBHDA(IDI,I,J,K) .NE. 0.0) THEN

                     IF (LICALL) THEN
                        JINT = ABS(JRAGED(IDI,I,J,K))
                        JRAGED(IDI,I,J,K) = 0
                     ENDIF           
                  
                     ROTSIT = RSITFN(IDI,1) * DBHDA(IDI,I,J,K) + 
     &                        RSITFN(IDI,2)

C....                If the current root radius ROOTDA stored for the class
C....                is less than that calculated as the radius at which
C....                the root systems sits ROTSIT for x years, set the
C....                ROOTDA to ROTSIT. So that the decay process does
C....                not try to grow the root system. The weighted averaging
C....                of individuals into the class may have caused this
C....                backward condition to occur. LRD 30JUL02

                     IF (ROOTDA(IDI,I,J,K) .LT. ROTSIT) THEN
                        ROOTDA(IDI,I,J,K) = ROTSIT
                     ENDIF
                     
C....                Armillaria & Phellinus change functions at
C....                12in DBH.

                     IF (DBHDA(IDI,I,J,K) .LE. 12.0) THEN
                        JRSIT = INT(YRSITF(IDI,1,1) *
     &                          DBHDA(IDI,I,J,K) +
     &                          YRSITF(IDI,2,1))
                     ELSE
                        JRSIT = INT(YRSITF(IDI,1,2) *
     &                          DBHDA(IDI,I,J,K) +
     &                          YRSITF(IDI,2,2))
                     ENDIF

                     IF (DEBUG) WRITE (JOSTND,*) 
     &               'IN RDINOC: DBHDA=',DBHDA(IDI,I,J,K),' JINT=',JINT,
     &               ' JRAGED=',JRAGED(IDI,I,J,K),' ROTSIT=',ROTSIT,
     &               ' JRSIT=',JRSIT,'PROBDA=',PROBDA(IDI,I,J,K),
     &               ' RSITFN1=',RSITFN(IDI,1),' RSITFN2=',RSITFN(IDI,2)

                     IF (JRAGED(IDI,I,J,K) .LE. 0) THEN
                    
                        IF (DECRAT(IDI,I,J,K) .LE. 0.0) THEN

C....                      If stump has not yet started decaying 
C....                      then find the decay rate.  The decay
C....                      rate is a function of root radius only.

                           IF (DBHDA(IDI,I,J,K) .LE. 12.0) THEN
                              DECRAT(IDI,I,J,K) = (DECFN(IDI,1,1) * 
     &                                            ROOTDA(IDI,I,J,K) +
     &                                            DECFN(IDI,2,1)) /
     &                                            DSFAC(I)  
                           ELSE
                              DECRAT(IDI,I,J,K) = (DECFN(IDI,1,2) * 
     &                                            ROOTDA(IDI,I,J,K) +
     &                                            DECFN(IDI,2,2)) /
     &                                            DSFAC(I)  
                           ENDIF
                      
C....                      Modify the decay rate to account for the
C....                      minimum lifespan of inoculum (default is 0
C....                      but user may change)

                           DROOTS = ROOTDA(IDI,I,J,K) - ROTSIT

                           TMINLF = JRSIT + DROOTS / DECRAT(IDI,I,J,K)

                           IF (DEBUG) WRITE (JOSTND,*)
     &                     'IN RDINOC: DECRAT=',DECRAT(IDI,I,J,K),
     &                     ' ROOTDA=',ROOTDA(IDI,I,J,K),
     &                     ' DROOTS=',DROOTS,' TMINLF=',TMINLF,
     &                     ' XMINLF=',XMINLF(IDI)

                           IF (TMINLF .LT. XMINLF(IDI)) THEN
                              DECRAT(IDI,I,J,K) = DROOTS /
     &                                            (XMINLF(IDI) - JRSIT)
                           ENDIF 
                        ENDIF

C....                   If stump decay has not yet reached the 'core'
C....                   part then keep decreasing the root radius.

                        RTODEC = DECRAT(IDI,I,J,K) * JINT
                        RTREM = ROOTDA(IDI,I,J,K) - RTODEC

                        IF (RTREM .LT. ROTSIT) THEN
                           RTODEC = ROOTDA(IDI,I,J,K) - ROTSIT
                           IF (RTODEC .LT. 0.0) RTODEC = 0.0
                           JRAGED(IDI,I,J,K) = JINT - INT(RTODEC /
     &                                         DECRAT(IDI,I,J,K))
                           RTREM = ROTSIT
                        ENDIF 

                        ROOTDA(IDI,I,J,K) = RTREM
                     ELSE
                  
                        JRAGED(IDI,I,J,K) = JRAGED(IDI,I,J,K) + JINT
                     ENDIF                           
                  
                     IF (JRAGED(IDI,I,J,K) .GT. JRSIT) THEN

C....                   If stump has sat with no apparent decay for
C....                   enough time then it disappears.

                        PROBDA(IDI,I,J,K) = 0.0
                        DBHDA(IDI,I,J,K)  = 0.0
                        JRAGED(IDI,I,J,K) = 0
                        ROOTDA(IDI,I,J,K) = 0.0
                     ENDIF
                  ENDIF   

  300          CONTINUE
  400       CONTINUE
  500    CONTINUE
  600 CONTINUE

      IF (DEBUG) THEN
         WRITE (JOSTND,910) ICYC
      ENDIF

  910 FORMAT (' End RDINOC, Cycle = ', I5)
  920 FORMAT (A5, 5F10.3)

      RETURN
      END
