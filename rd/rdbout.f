      SUBROUTINE RDBOUT
      IMPLICIT NONE
C----------
C  **RDBOUT      LAST REVISION:  03/24/15
C----------
C
C  Purpose :
C     Produces optional output (in a separate file) of mortality due to 
C     bark beetles.
C
C  Called By :
C     RDOAGM  [ROOT DISEASE]
C
C  Calls :
C     GETID   (SUBROUTINE)   [FVS]
C     GETLUN  (SUBROUTINE)   [FVS]
C     DBSRD3  (SUBROUTINE)   [FVS]
C
C  Arguments :
C     none
C
C  Common Block Variables Used :
C     ISTEP
C     ICYC
C     DBH
C     ISP
C     IY     - translates ICYC and ISTEP into calendar years
C     BBKILL -  number of trees killed by bark beetles, for
C              DSII/DSIU/DSO, for each record.
C     RROBNK - total number of trees killed by bark beetles, for each
C              tree species.
C
C  Local Variables : 
C     CLKILL - REAL
C              Total number of trees killed in each type, by 5 inch
C              DBH size classes.
C     CMC    - CHARACTER
C              Name of output file.
C     IBBOUT - INTEGER
C              Unit number of output file.
C     NOKILL - LOGICAL 
C              FLAG - .TRUE. if there were no beetles active this year.
C     ROWSUM - REAL
C              Total number of trees killed in each type (inside,
C              I-uninf, outsied)
C     SUMLIV - REAL
C              Total number of trees alive in each type, before bark
C              beetles attacked.
C     TOLDYR - LOGICAL 
C              FLAG - .TRUE. if other species this year are already
C              printed.
C     TOTSPC - REAL
C              The total number of trees killed for each species.
C              This values is weighted by proportion of diseased (inside)
C              and undiseased (outside) stand area.
C
C  Revision History :
C     10/07/97 - Matthew K. Thompson (FHTET)
C        Fixed loop to calculate size class totals.
C     18-JUN-2001 Lance R. David (FHTET)
C        Added Stand ID and Management ID line to header.
C        Moved return condition check to after writing of headers.
C     16-AUG-2006 Lance R. David (FHTET)
C        Change of metric conversion factors variable names to match
C        variables in new \FVS\COMMON\METRIC.F77. rd\src\metric.f77
C        will be retired. (mods courtesy of Don Robinson, ESSA)
C   08/26/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   02/26/2015 Lance R. David (FMSC)
C     TOTSPC value was not properly weight based on diseased and 
C     undiseased proportional area of stand.
C     Implemented General Report Writer facility.
C     Auxiliary flat file from BBOUT keyword no longer available. This
C     report now available in output DB.
C----------------------------------------------------------------------

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'METRIC.F77'

C.... Local variable declarations.

      LOGICAL     TOLDYR, NOKILL
      INTEGER     CLASS, I, ICL, IDI, J, JYR, K, SP
      REAL        ROWSUM(3,MAXSP), CLKILL(3,MAXSP,7), SUMLIV(3,MAXSP)
      REAL        TOTSPC(MAXSP)
      REAL        MRTIN(7), MRTUN(7), MRTOUT(7)

C.... ISTEP=2 (set in RDCNTL?) is the first time it's called.  Write the
C.... table headers then.
C.... Since the report has been requested, need to get report ID and 
C.... logical unit number from general report facility and write headers
C.... whether there are any trees in the simulation at this time or not.

C
C     Get logical unit number and open genrpt file if it has been closed.
C
      CALL GETLUN(IBBOUT)

      IF (ISTEP .EQ. 2) THEN 
C
C     get report ID and logical unit number.
C
          CALL GETID(IDRDOUT(3))

          WRITE (IBBOUT,'(2(/1X,I5))') IDRDOUT(3),IDRDOUT(3)

          WRITE (IBBOUT,1100) IDRDOUT(3)
          WRITE (IBBOUT,1105) IDRDOUT(3)
          WRITE (IBBOUT,1100) IDRDOUT(3)
          WRITE (IBBOUT,1110) IDRDOUT(3), NPLT, MGMID

          IF (.NOT. LMTRIC) WRITE (IBBOUT,1115) IDRDOUT(3)
          IF (LMTRIC) WRITE (IBBOUT,1215) IDRDOUT(3)

          WRITE (IBBOUT,1111) IDRDOUT(3)
          WRITE (IBBOUT,1120) IDRDOUT(3)

          IF (.NOT. LMTRIC) WRITE (IBBOUT,1125) IDRDOUT(3)
          IF (LMTRIC) WRITE (IBBOUT,1225) IDRDOUT(3)

          WRITE (IBBOUT,1130) IDRDOUT(3)

          IF (.NOT. LMTRIC) WRITE (IBBOUT,1135) IDRDOUT(3)
          IF (LMTRIC) WRITE (IBBOUT,1235) IDRDOUT(3)

          WRITE (IBBOUT,1120) IDRDOUT(3)
      ENDIF
      
      IF (IROOT .EQ. 0) GOTO 1000
      
      JYR = IY(ISTEP)
      NOKILL = .TRUE.
      TOLDYR = .FALSE.
      
      DO 22 I=1,3
         DO 11 J=1,MAXSP
            ROWSUM(I,J) = 0.0
            SUMLIV(I,J) = 0.0
            TOTSPC(J) = 0.0

            DO 10 K=1,7
               CLKILL(I,J,K) = 0.0
   10       CONTINUE
   11    CONTINUE
   22 CONTINUE
    
      IDI = MAXRR

      DO 66 I=1,ITRN
         SP = ISP(I)
C         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(SP)) ! changed 11/18/2015

C....    If both P and S type Annosus is being used, set disease area
C....    to that of the disease type of the host tree species; otherwise,
C....    use the disease area of the one disease being simulated even though
C....    current tree may not be specified as host species.

         IF (MAXRR .LT. 3 .AND. MINRR .NE. MAXRR) THEN
            IDI = IDITYP(IRTSPC(SP))
         ENDIF

         CLASS = INT(DBH(I)/5.0) + 1
         IF (CLASS .GT. 7) CLASS = 7

         DO 33 J=1,3
            ROWSUM(J,SP) = ROWSUM(J,SP) + BBKILL(J,I)
            CLKILL(J,SP,CLASS) = CLKILL(J,SP,CLASS) + BBKILL(J,I)
   33    CONTINUE

C      IF (SP .EQ. 10)                                       ! Debug
C     & WRITE (*,*) 'IN RDBOUT: I, SP, BBKILL(x,I) ',I, SP,  ! Debug
C     & BBKILL(1,I),BBKILL(2,I),BBKILL(3,I)                  ! Debug

         SUMLIV(2,SP) = SUMLIV(2,SP) + PROBIT(I) + BBKILL(2,I)
         SUMLIV(3,SP) = SUMLIV(3,SP) + PROBIU(I)
         SUMLIV(1,SP) = SUMLIV(1,SP) + FPROB(I) * (SAREA - PAREA(IDI))
   66 CONTINUE
   
      IDI = MAXRR

      DO 99 SP=1,MAXSP 
         IF (RROBNK(SP) .LE. 0.0) GOTO 99
         NOKILL = .FALSE. 
         
C....    First change everything into per acre.

C         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(SP)) ! changed 11/18/2015

C....    If both P and S type Annosus is being used, set disease area
C....    to that of the disease type of the host tree species; otherwise,
C....    use the disease area of the one disease being simulated even though
C....    current tree may not be specified as host species.

         IF (MAXRR .LT. 3 .AND. MINRR .NE. MAXRR) THEN
            IDI = IDITYP(IRTSPC(SP))
         ENDIF

         DO 77 J=2,3
            SUMLIV(J,SP) = SUMLIV(J,SP) / PAREA(IDI)
            ROWSUM(J,SP) = ROWSUM(J,SP) / PAREA(IDI)
C
C           Value added to total for species must be weighted by
C           proportion of stand area that is diseased (inside).
C            TOTSPC(SP) = TOTSPC(SP) + ROWSUM(J,SP) 
            TOTSPC(SP) = TOTSPC(SP) 
     &      + (ROWSUM(J,SP) * (PAREA(IDI) / SAREA))
   77    CONTINUE         

C         WRITE (*,*) 'IN RDBOUT: SP, IDI, PAREA, SAREA',  ! Debug
C     &                           SP,IDI,PAREA(IDI),SAREA  ! Debug

         IF (PAREA(IDI) .LT. SAREA) THEN
            SUMLIV(1,SP) = SUMLIV(1,SP) / (SAREA-PAREA(IDI))
            ROWSUM(1,SP) = ROWSUM(1,SP) / (SAREA-PAREA(IDI))
C
C           Value added to total for species must be weighted by
C           proportion of stand area that is not diseased (outside).
C            TOTSPC(SP) = TOTSPC(SP) + ROWSUM(1,SP) 
            TOTSPC(SP) = TOTSPC(SP)
     &      + (ROWSUM(1,SP) * ((SAREA - PAREA(IDI)) / SAREA))
         ELSE
            SUMLIV(1,SP) = 0.0
            ROWSUM(1,SP) = 0.0
         ENDIF   

         DO 88 ICL = 1,7
            CLKILL(2,SP,ICL) = CLKILL(2,SP,ICL) / PAREA(IDI)
            CLKILL(3,SP,ICL) = CLKILL(3,SP,ICL) / PAREA(IDI)
            IF (PAREA(IDI) .LT. SAREA)
     &         CLKILL(1,SP,ICL) = CLKILL(1,SP,ICL)/(SAREA-PAREA(IDI))
   88    CONTINUE
         
         IF (LMTRIC) THEN
            IF (TOLDYR) THEN         
               WRITE(IBBOUT,2011) IDRDOUT(3), JSP(SP),
     &                            (CLKILL(2,SP,I)/ACRtoHA,I=1,7),
     &                            ROWSUM(2,SP)/ACRtoHA, 
     &                            SUMLIV(2,SP)/ACRtoHA
            ELSE
               WRITE(IBBOUT,2001) IDRDOUT(3), JYR, JSP(SP), 
     &                           (CLKILL(2,SP,I)/ACRtoHA,I=1,7),
     &                            ROWSUM(2,SP)/ACRtoHA, 
     &                            SUMLIV(2,SP)/ACRtoHA
               TOLDYR = .TRUE.
            ENDIF
         
            WRITE(IBBOUT,2002) IDRDOUT(3),
     &                         (CLKILL(3,SP,I)/ACRtoHA,I=1,7), 
     &                         ROWSUM(3,SP)/ACRtoHA, 
     &                         SUMLIV(3,SP)/ACRtoHA
            WRITE(IBBOUT,2004) IDRDOUT(3),
     &                         (CLKILL(1,SP,I)/ACRtoHA,I=1,7), 
     &                         ROWSUM(1,SP)/ACRtoHA, 
     &                         TOTSPC(SP)/ACRtoHA, 
     &                         SUMLIV(1,SP)/ACRtoHA
            WRITE(IBBOUT,1111) IDRDOUT(3)
         ELSE
            IF (TOLDYR) THEN         
               WRITE(IBBOUT,2011) IDRDOUT(3),
     &                            JSP(SP), (CLKILL(2,SP,I),I=1,7),
     &                            ROWSUM(2,SP), SUMLIV(2,SP)
            ELSE
               WRITE(IBBOUT,2001) IDRDOUT(3),
     &                            JYR, JSP(SP), (CLKILL(2,SP,I),I=1,7),
     &                            ROWSUM(2,SP), SUMLIV(2,SP)
               TOLDYR = .TRUE.
            ENDIF
         
            WRITE(IBBOUT,2002) IDRDOUT(3),
     &                         (CLKILL(3,SP,I),I=1,7), ROWSUM(3,SP), 
     &                         SUMLIV(3,SP)
            WRITE(IBBOUT,2004) IDRDOUT(3),
     &                         (CLKILL(1,SP,I),I=1,7), ROWSUM(1,SP), 
     &                         TOTSPC(SP), SUMLIV(1,SP)
            WRITE(IBBOUT,1111) IDRDOUT(3)
C
C           Call DBS for RD Bark Beetle output to database
C           Need to load 1-dimensional arrays first.
C
            DO I=1,7
              MRTIN(I)  = CLKILL(2,SP,I)
              MRTUN(I)  = CLKILL(3,SP,I)
              MRTOUT(I) = CLKILL(1,SP,I)
            ENDDO

            CALL DBSRD3 (JYR,NPLT,JSP(SP),
     &           MRTIN,ROWSUM(2,SP), SUMLIV(2,SP),
     &           MRTUN,ROWSUM(3,SP), SUMLIV(3,SP),
     &           MRTOUT,ROWSUM(1,SP), SUMLIV(1,SP),
     &           TOTSPC(SP))

         ENDIF   
   99 CONTINUE
 
      IF (NOKILL) WRITE(IBBOUT,2000) IDRDOUT(3), JYR
      WRITE(IBBOUT,1111) IDRDOUT(3)

 1000 RETURN

 1100 FORMAT (1X,I5,1X,63('* '))
 1105 FORMAT (1X,I5,51X,'WESTERN ROOT DISEASE MODEL')
 1110 FORMAT (1X,I5,34X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)
 1111 FORMAT (1X,I5)
 1115 FORMAT (1X,I5,36X,
     &   'TREES PER ACRE KILLED BY BARK BEETLES IN EACH TIME PERIOD')
 1120 FORMAT (1X,I5,1X,126('-'))
 1125 FORMAT (1X,I5,1X,'Period   Tree    General ',23X,
     &        'DBH Class (in inches)',27X,
     &        '     Row     Species   Total')
 1130 FORMAT (1X,I5,1X,'Start   Species  Status   ',69('-'),
     &        '     Total     Total    Before')
 1135 FORMAT (1X,I5,1X,'(year)',20X,'     0-<5     5-<10 ',
     &        '   10-<15    15-<20    20-<25    25-<30       30+',
     &        '                        Attack') 
     
 2000 FORMAT (1X,I5,2X,I4,21X,'no beetle kill')
 2001 FORMAT (1X,I5,2X,I4,5X,A3,'    in-inf   ',7(F9.2,1X),1X,F9.2,10X,
     &            1X,F9.2)
 2011 FORMAT (1X,I5,11X,A3,'    in-inf   ',7(F9.2,1X),1X,F9.2,10X,1X,
     &            F9.2)
 2002 FORMAT (1X,I5,18X,'in-uninf ',7(F9.2,1X),1X,F9.2,10X,1X,F9.2)
 2004 FORMAT (1X,I5,18X,'outside  ',7(F9.2,1X),3(1X,F9.2))

C.... Headers used when running the metric version.

 1215 FORMAT (1X,I5,37X,
     &   'TREES PER HA KILLED BY BARK BEETLES IN EACH TIME PERIOD')
 1225 FORMAT (1X,I5,1X,'Period   Tree    General ',25X,
     &        'DBH Class (in cm)  ',27X,
     &        '     Row     Species   Total')
 1235 FORMAT (1X,I5,1X,'(year)',20X,'    0-<13    13-<25 ',
     &        '   25-<38    38-<51    51-<64    64-<76       76+',
     &        '                        Attack') 

      END
