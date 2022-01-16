      SUBROUTINE RDSPOR
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  Purpose :
C     Updates stump arrays and creates new disease centers from
C     stumps infected by spores in previous time step. No crossover
C     occurs and only stumps that are big enough are added to the stump list.
C
C  Called By :
C     RDCNTL  [ROOT DISEASE]
C
C  Calls :
C     DBCHK   [FVS]
C     RDAREA  [ROOT DISEASE]
C     RDINUP  [ROOT DISEASE]
C     RDSPL1  [ROOT DISEASE]
C     RDSPL2  [ROOT DISEASE]
C
C  Local Variables :
C
C  Common Block Variables Used :
C
C  Revision History :
C     03/21/97 - Matt Thompson (FHTET)
C                Deleted commented out code and cleaned up code.
C     04/03/97 - Matt Thompson (FHTET)
C                Modified the code to better handle moving spore
C                infected stumps to the stump lists and the 
C                new centers lists.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77'

      LOGICAL DEBUG
      INTEGER I, IDI, ISPINT, IST, J, JJ
      REAL    DEN, DIAM, RCEN, RTD, SPCEN, STNEW, TOTCEN, TST

      CALL DBCHK(DEBUG,'RDSPOR',6,ICYC)
      
      IST = INT(AMAX0(1,ISTEP))

C.... Calculate averages of infected root characteristics for each     
C.... root disease type.

      DO 50 IRRSP = MINRR, MAXRR
         CALL RDINUP
   50 CONTINUE

      DO 1000 ISPINT = 3, 2, -1

         IF (LSPFLG(ISPINT) .AND.
     &       (IY(ICYC) - ISDATE(ISPINT)) .GT. 5) THEN

C....       Clear the flag that says a stand entry occurred.

            LSPFLG(ISPINT) = .FALSE.

C....       Stand entry occurred greater then 5 years ago, so transfer
C....       spore infected stumps to stump list or start new centers.

            DO 900 IDI=1,2
               DO 850 I=1,2
                  DO 800 J=1,5

C....                Spore infected stumps inside of same disease type
C....                are added to inside center stump list (If stumps
C....                are of a large enough diameter.)

                     DEN = STUIN(IDI,I,J,ISPINT)
                     DIAM = DBHUIN(IDI,I,J,ISPINT)
                  
                     IF (DIAM .LT. SPDBH(IDI)) GOTO 650
                     IF (DEN .LE. 1E-4) GOTO 650

                     IF (DEBUG) THEN     
                        WRITE (JOSTND,*) 'RDSPOR : SAME TYPE, ',
     &                                   'INS STUMPS'
                        WRITE (JOSTND,*) 'IDI I J DEN', IDI,I,J,DEN
                     ENDIF   

                     TST = PROBDA(IDI,I,J,IST) + DEN
                     DBHDA(IDI,I,J,IST) = (DBHDA(IDI,I,J,IST) *
     &                   PROBDA(IDI,I,J,IST) + DIAM * DEN) / TST             

C....                Assume only 50% of root radius is infected and add
C....                only that much to the root list (to be consistent
C....                with the way infected tree roots are added to the
C....                list)     

                     ROOTDA(IDI,I,J,IST) = (ROOTDA(IDI,I,J,IST) *
     &                   PROBDA(IDI,I,J,IST) + 0.5 *
     &                   RTUIN(IDI,I,J,ISPINT) *
     &                   DEN) / TST

                     PROBDA(IDI,I,J,IST) = PROBDA(IDI,I,J,IST) + DEN

  650                CONTINUE

C....                Stumps that are infected outside of centers 
C....                create new centers if they are large enough.

                     RCEN = STOUT(IDI,I,J,ISPINT)
                     IF (RCEN .LE. 1E-4) GOTO 770

                     DIAM = DBHOUT(IDI,I,J,ISPINT)
                     RTD = RTOUT(IDI,I,J,ISPINT) 

                     IF (DEBUG) THEN       
                        WRITE (JOSTND,*) 'RDSPOR :  BEFORE RRSPL2'
                        WRITE (JOSTND,*) 'IDI I J', IDI,I,J
                        WRITE (JOSTND,*) 'DIAM RTD RCEN NCENTS(IDI)',
     &                                    DIAM,RTD,RCEN,NCENTS(IDI)
                     ENDIF 

                     CALL RDSPL2(RCEN,IDI,DIAM,RTD)

                     IF (DEBUG) THEN  
                        WRITE (JOSTND,*) 'RDSPOR :  AFTER RRSPL2'
                        WRITE (JOSTND,*) 'NCENTS(IDI)', NCENTS(IDI)
                        WRITE (JOSTND,*) 'PCENTS 1,2,3 ICENSP'

                        DO 756 JJ=1,NCENTS(IDI)
                           WRITE(JOSTND,*) PCENTS(IDI,JJ,1),
     &                                     PCENTS(IDI,JJ,2),
     &                                     PCENTS(IDI,JJ,3),
     &                                     ICENSP(IDI,JJ)
  756                   CONTINUE
                     ENDIF
                  
C....                Add all stumps that are large enough to create
C....                centers to stump list.                                      

                     IF (DIAM .GE. SPDBH(IDI)) STNEW = RCEN
                     IF (STNEW .LT. 1) GOTO 770
                  
                     TST = PROBDA(IDI,I,J,IST) + STNEW
                     DBHDA(IDI,I,J,IST) = (DBHDA(IDI,I,J,IST) *
     &                    PROBDA(IDI,I,J,IST) + DBHOUT(IDI,I,J,ISPINT) *
     &                    STNEW) / TST                

C....                Assume 50% of root radius is infected so set
C....                infection level to that amount.

                     ROOTDA(IDI,I,J,IST) = (ROOTDA(IDI,I,J,IST) *
     &                 PROBDA(IDI,I,J,IST) + 0.5 *
     &                 RTOUT(IDI,I,J,ISPINT) *
     &                 STNEW) / TST
                     PROBDA(IDI,I,J,IST) = PROBDA(IDI,I,J,IST) + STNEW
                          
  770                CONTINUE

C....                Zero arrays

                     STOUT(IDI,I,J,ISPINT)  = 0.0
                     DBHOUT(IDI,I,J,ISPINT) = 0.0
                     RTOUT(IDI,I,J,ISPINT)  = 0.0
                     STUIN(IDI,I,J,ISPINT)  = 0.0
                     DBHUIN(IDI,I,J,ISPINT) = 0.0
                     RTUIN(IDI,I,J,ISPINT)  = 0.0

  800             CONTINUE
  850          CONTINUE
  900       CONTINUE

            DO 950 IRRSP=1,2
               IF (NCENTS(IRRSP) .EQ. 0) GOTO 960

               SPCEN = 0.0
               TOTCEN = 0.0

               DO 970 I=1,NCENTS(IRRSP)
                  IF (ICENSP(IRRSP,I) .NE. 0) SPCEN = SPCEN +
     &                                        PCENTS(IRRSP,I,3) ** 2
                  TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2
  970          CONTINUE

               SPPROP(IRRSP) = 0.0

               IF (SPCEN .GT. 1E-4 .AND. TOTCEN .GT. 1E-4) THEN       
                  SPPROP(IRRSP) = SPCEN / TOTCEN
               ENDIF

  960          CONTINUE

               IF (DEBUG) WRITE(JOSTND,*)
     &                 'RDSPOR :  BEF PAREA', IRRSP, PAREA(IRRSP)

               CALL RDAREA                                           

               IF (DEBUG) WRITE(JOSTND,*)
     &                    'RDSPOR :  AFT PAREA', IRRSP, PAREA(IRRSP)
     
C....          If new infection type in stand, and user did not specify
C....          to model as one center then model stand as multiple centers.

               IF (LONECT(IRRSP) .EQ. 0 .AND. PAREA(IRRSP) .GT. 0) 
     &                    LONECT(IRRSP) = 2

  950       CONTINUE

         ENDIF

 1000 CONTINUE

      DO 1400 ISPINT = 2, 1, -1

         IF (LSPFLG(ISPINT)) THEN     

C....       A stand entry occurred in a previous timestep (if the cycle
C....       length was less than 5) and/or this time, so shift spore    
C....       infected stumps within their arrays.

            DO 1300 IDI=1,2
               DO 1200 I=1,2
                  DO 1100 J=1,5
                     STOUT(IDI,I,J,ISPINT+1)  = STOUT(IDI,I,J,ISPINT)
                     STOUT(IDI,I,J,ISPINT)    = 0.0

                     DBHOUT(IDI,I,J,ISPINT+1) = DBHOUT(IDI,I,J,ISPINT)
                     DBHOUT(IDI,I,J,ISPINT)   = 0.0

                     RTOUT(IDI,I,J,ISPINT+1)  = RTOUT(IDI,I,J,ISPINT)
                     RTOUT(IDI,I,J,ISPINT)    = 0.0

                     STUIN(IDI,I,J,ISPINT+1)  = STUIN(IDI,I,J,ISPINT)
                     STUIN(IDI,I,J,ISPINT)    = 0.0

                     DBHUIN(IDI,I,J,ISPINT+1) = DBHUIN(IDI,I,J,ISPINT)
                     DBHUIN(IDI,I,J,ISPINT)   = 0.0

                     RTUIN(IDI,I,J,ISPINT+1)  = RTUIN(IDI,I,J,ISPINT)
                     RTUIN(IDI,I,J,ISPINT)    = 0.0

                     ISDATE(ISPINT+1)         = ISDATE(ISPINT)
                     ISDATE(ISPINT)           = 0

                     LSPFLG(ISPINT+1)         = LSPFLG(ISPINT)
 1100             CONTINUE
 1200          CONTINUE
 1300       CONTINUE      

            LSPFLG(ISPINT) = .FALSE.
         ENDIF
 1400 CONTINUE

      RETURN
      END
