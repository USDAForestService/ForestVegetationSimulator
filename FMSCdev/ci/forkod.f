      SUBROUTINE FORKOD
      IMPLICIT NONE
C----------
C  **FORKOD--CI   DATE OF LAST REVISION:  06/20/11
C----------
C
C     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
C     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
C  ------------------------
C  NATIONAL FORESTS:
C  117 = NEZ PERCE
C  402 = BOISE
C  406 = CHALLIS
C  412 = PAYETTE
C  413 = SALMON
C  414 = SAWTOOTH
C  ------------------------
C  RESERVATION PSUEDO CODES:
C  7721 = DUCK VALLEY RESERVATION   (MAPPED TO 402 BOISE)
C  8107 = FORT HALL RESERVATION     (MAPPED TO 414 SAWTOOTH)
C  ------------------------
      INTEGER JFOR(6),KFOR(6),NUMFOR,I
      LOGICAL USEIGL, FORFOUND
C  ------------------------
C  DATA STATEMENTS
C  ------------------------
      DATA JFOR/117,402,406,412,413,414/, NUMFOR /6/
      DATA KFOR/1,2,2,3,2,2 /

      USEIGL = .TRUE.
      FORFOUND = .FALSE.


      SELECT CASE (KODFOR)

C       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
        CASE (7721)
          WRITE(JOSTND,70)
   70     FORMAT(/,'********',T12,'DUCK VALLEY RESERVATION (7721) ',
     &    'BEING MAPPED TO BOISE NF (402) FOR FURTHER PROCESSING.')
          IFOR = 3
        CASE (8107)
          WRITE(JOSTND,72)
   72     FORMAT(/,'********',T12,'FORT HALL RESERVATION (8107) ',
     &    'BEING MAPPED TO SAWTOOTH NF (414) FOR FURTHER PROCESSING.')
          IFOR = 3
C       END CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE

        CASE DEFAULT

C         CONFIRMS THAT KODFOR IS AN ACCEPTED FVS LOCATION CODE
C         FOR THIS VARIANT FOUND IN DATA ARRAY JFOR
          DO 10 I=1,NUMFOR
            IF (KODFOR .EQ. JFOR(I)) THEN
              IFOR = I
              FORFOUND = .TRUE.
              EXIT
            ENDIF
   10     CONTINUE

C         LOCATION CODE ERROR TRAP
          IF (.NOT. FORFOUND) THEN
            CALL ERRGRO (.TRUE.,3)
            WRITE(JOSTND,11) JFOR(IFOR)
   11       FORMAT(/,'********',T12,'FOREST CODE USED IN THIS ',
     &      'PROJECTION IS',I4)
            USEIGL = .FALSE.
          ENDIF

      END SELECT

C     FOREST MAPPING CORRECTION {NONE FOR THIS VARIANT}
C     SELECT CASE (IFOR)
C     END SELECT


C     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
C     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
C     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
C     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
      IF (USEIGL) IGL = KFOR(IFOR)

      KODFOR=JFOR(IFOR)
      RETURN
      END