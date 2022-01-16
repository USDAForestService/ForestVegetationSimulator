      SUBROUTINE FMUSRFM (IYR, FMD)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
C     PROCESSES THE FUELMODL KEYWORD AND THE FUELTRET KEYWORD.
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C
      INTEGER IYR,FMD,MYACT(3),NTODO,ITODO,NPRM,IACTK,JYR,J,I,JDO
      INTEGER INDX(4)
      REAL PRMS(8),SUM
      REAL DPMULT(3,3)
      LOGICAL LOK

C     LOCAL VARIABLE DEFINITIONS:
C     DPMULT:  DEPTH MULTIPLIER FOR ACTIVITY FUELS, BASED ON HARVEST
C              TYPE(x,) AND FUEL TREATMENT TYPE(,x).
C              (1,)ground-based;(2,)high-lead;(3,)precomm/heli
C              (,1)none;(,2)lopping;(,3)trampling
C
C     Depth modifiers based on fuel treatment and harvest type
C
      DATA DPMULT /
     >  1.0,  1.3,  1.6,
     >  0.83, 0.83, 0.83,
     >  0.75, 0.75, 0.75 /
C
C     Initialize Weights of Dynamic Fire Models
C
      DO I = 1, MXFMOD
        FMOD(I) = 0
        FWT(I)  = 0.
      ENDDO
C
C     FUELMODL (2538) - FUEL MODELS TO USE IN PLACE OF AUTOMATIC ONES
C     FUELTRET(2525) - HARVEST TYPE, FUEL TREATMENT
C     FIRECALC(2549) - USE NEW FM LOGIC OR MODELLED LOADS
C
      DATA MYACT/2538,2525,2549/

C     CHECK WHETHER THE FIRECALC KEYWORD IS SCHEDULED FOR THIS YEAR 

      CALL OPFIND(1,MYACT(3),NTODO)
      IF (NTODO.GT.0) THEN
        DO 400 ITODO = 1,NTODO
          CALL OPGET(ITODO,8,JYR,IACTK,NPRM,PRMS)
C          IF (JYR .NE. IYR) GO TO 400
          CALL OPDONE (ITODO,IYR)

C         SET THE FIRECALC PARAMETER VALUES.

          IFLOGIC = INT(PRMS(1))
          IFMSET  = INT(PRMS(2))
          USAV(1) = PRMS(3)
          USAV(2) = PRMS(4)
          USAV(3) = PRMS(5)
          UBD(1)  = PRMS(6)          
          UBD(2)  = PRMS(7)
          ULHV    = PRMS(8)                    
  400   CONTINUE
      ENDIF

C     Check to see if the user has entered fuel models.
C     If so, we will use those in preference to anything else.

      CALL OPFIND(1,MYACT(1),NTODO)
      IF (NTODO.GT.0) THEN
         DO ITODO=1,NTODO         
            DO I=1,8
              PRMS(I) = 0
            ENDDO
            CALL OPGET(ITODO,8,JYR,IACTK,NPRM,PRMS)
C            IF (JYR .EQ. IYR) THEN
               IF (NPRM.LE.0) THEN
                  LUSRFM = .FALSE.
                  CALL OPDONE(ITODO,IYR)
               ELSE IF (PRMS(1).EQ.0.) THEN
                  LUSRFM = .FALSE.
                  CALL OPDONE(ITODO,IYR)                  
               ELSE IF (INT(PRMS(1)).GT.MXDFMD .OR.
     >            INT(PRMS(3)).GT.MXDFMD .OR. INT(PRMS(5)).GT.MXDFMD 
     >            .OR. INT(PRMS(7)).GT.MXDFMD) THEN
                  LUSRFM = .FALSE.
               ELSE               
                 SELECT CASE (INT(PRMS(1)))
                 CASE (31:100,110:120,125:140,150:160,166:180,190:200,
     >                205:256)
                    LUSRFM = .FALSE.
                 CASE DEFAULT                
                   SELECT CASE (INT(PRMS(3)))
                   CASE (31:100,110:120,125:140,150:160,166:180,190:200,
     >                  205:256)
                      LUSRFM = .FALSE.
                   CASE DEFAULT  
                     SELECT CASE (INT(PRMS(5)))
                     CASE (31:100,110:120,125:140,150:160,166:180,
     >                    190:200,205:256)
                        LUSRFM = .FALSE.
                     CASE DEFAULT  
                       SELECT CASE (INT(PRMS(7)))
                       CASE (31:100,110:120,125:140,150:160,166:180,
     >                      190:200,205:256)
                          LUSRFM = .FALSE.
                       CASE DEFAULT  
                          CALL OPDONE(ITODO,IYR)
                          LUSRFM=.TRUE.
                          IF (MOD(NPRM,2).EQ.1) THEN
                             NPRM=NPRM+1
                             PRMS(NPRM)=1. ! DEFAULT WEIGHT IS 1.0
                          ENDIF
                          SUM=0.
                          DO I=2,NPRM,2
                             IF (PRMS(I).LE.0.) PRMS(I)=1.
                             SUM=SUM+PRMS(I)
                          ENDDO
                          SUM=1./SUM
                          DO I=2,NPRM,2
                             PRMS(I)=PRMS(I)*SUM
                          ENDDO
                          DO J = 1,4
                             FMDUSR(J) = 0
                             FWTUSR(J) = 0.0
                          ENDDO
                          J=0
                          DO I = 1,NPRM-1,2
                             J=J+1
                             FMDUSR(J) = INT(PRMS(I))
                             FWTUSR(J) = PRMS(I+1)
                          ENDDO
                       END SELECT
                     END SELECT                        
                   END SELECT
                 END SELECT
               ENDIF
C            ENDIF
         ENDDO
      ENDIF

      IF (LUSRFM) THEN
C
C       SORT WEIGHTS SUCH THAT HIGHEST WEIGHT IS ALWAYS FIRST
C       WHAT ABOUT TIES?
C
        FMOD(MXFMOD) = 0
        FWT(MXFMOD)  = 0.0
        CALL RDPSRT (4,FWTUSR,INDX,.TRUE.)
        DO I = 1,4
          J = INDX(I)
          FMOD(I) = FMDUSR(J)
          FWT(I)  = FWTUSR(J)
        ENDDO
C
C     SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
C     FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING
C
        FMD = -1
        IF (FWT(1) .GT. 1.0E-6) FMD = FMOD(1)
        IF (FMD .LT. 0) THEN
          FMOD(1) = 8
          FWT(1)  = 1.0
          NFMODS  = 1
          DO I = 2,MXFMOD
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDDO
          CALL RCDSET(2, .TRUE.)
        ENDIF
C
C     COMPUTE THE NUMBER OF ACTIVE MODELS
C
        DO I = 1,MXFMOD
          IF (FWT(I) .LE. 1.0E-6) THEN
            NFMODS = I-1
            EXIT
          ENDIF
        ENDDO
        NFMODS = MIN(NFMODS,4)
C
C     IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS
C
        IF (.NOT. LDYNFM) THEN
          FMOD(1) = FMD
          FWT(1)  = 1.0
          NFMODS  = 1
          DO I = 2,MXFMOD
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDDO
        ENDIF

      ELSE ! NOT LUSRFM

C     IF A USER WANTS TO CALCULATE FIRE BEHAVIOR USING MODELLED FUEL LOADS
C     (FIRECALC KEYWORD) THEN FORCE THE SELECTION OF FM89.  THIS IS A CUSTOM
C     FUEL MODEL THAT IS FILLED WITH THE RIGHT PARAMETERS IN FMBURN.
C     ONLY DO THIS IF THE USER HASN'T SET THE FUEL MODEL WITH THE FUELMODEL
C     KEYWORD.

        IF (IFLOGIC .EQ. 2) THEN
          FMD = 89
          FMOD(1) = 89
          FWT(1)  = 1.0
          NFMODS  = 1
          DO I = 2,MXFMOD
            FMOD(I) = 0
            FWT(I)  = 0.0
          ENDDO
        ENDIF
      ENDIF
     
C
C     CHECK TO SEE IF THERE WAS ANY HARVEST TYPE OR FUEL TREATMENT INFO
C     ASSIGNED.
C
      CALL OPFIND(1,MYACT(2),NTODO)
      IF (NTODO.GT.0) THEN
        DO JDO = 1,NTODO
          CALL OPGET(JDO,3,JYR,IACTK,NPRM,PRMS)
C          IF (JYR .EQ. IYR) THEN

            LOK = .TRUE.
            IF (PRMS(1) .LT. 0.0) LOK = .FALSE.
            IF (PRMS(1) .GT. 2.0) LOK = .FALSE.
            IF (PRMS(2) .LT. 1.0) LOK = .FALSE.
            IF (PRMS(2) .GT. 3.0) LOK = .FALSE.

            IF (LOK) THEN
              FTREAT = INT(PRMS(1))
              HARTYP = INT(PRMS(2))
              DPMOD = PRMS(3)
              IF (DPMOD .LT. 0.0) DPMOD = DPMULT(HARTYP,FTREAT+1)
              IFTYR = IYR
              CALL OPDONE(JDO,IYR)
            ELSE
              CALL OPDEL1(JDO)
            ENDIF

C          ENDIF
        ENDDO
      ENDIF
      IF (IFTYR .GT. 0) THEN
        IF ((IYR-IFTYR) .GT. 5) THEN
          IFTYR = 0
          FTREAT = 0
          HARTYP = 0
          DPMOD = 1.0
        ENDIF
      ELSE
        FTREAT = 0
        HARTYP = 0
        DPMOD  = 1.0
      ENDIF

      RETURN
      END
