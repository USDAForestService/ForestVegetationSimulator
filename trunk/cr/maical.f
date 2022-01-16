      SUBROUTINE MAICAL
      IMPLICIT NONE
C----------
C CR $Id$
C----------
C  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
C  FROM CRATET.
C----------
C
COMMONS
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'HTCAL.F77'
C
C
COMMONS
C
C----------
      LOGICAL DEBUG
      REAL ADJMAI,SSSI
      INTEGER ISPNUM(11),IMAP,ISICD,IERR
      INTEGER MAP1(MAXSP),MAP2(MAXSP),MAP3(MAXSP),MAP4(MAXSP),
     &          MAP5(MAXSP)
C----------
C  SPECIES ORDER:
C   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
C  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
C  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
C  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
C
C  SPECIES EXPANSION:
C  UJ,AJ,RM,OJ,ER USE CR JU                              
C  NC,PW USE CR CO
C  GO,AW,EM,BK,SO USE CR OA                             
C  PB USES CR AS                              
C  PM,PD,AZ USE CR PI
C  CI USES CR PP                              
C----------
C
      DATA MAP1/
     &  9,  9,  3,  9,  4,  2,  2,  2,  1,  1,
     &  2, 11, 10,  1,  1, 11,  7,  8,  8,  6,
     &  5,  5,  5,  5,  5,  5,  5,  6, 11, 11,
     & 11, 11, 11, 11, 11, 10,  2,  5/
      DATA MAP2/
     &  4,  4,  3,  4,  4,  2,  2,  2,  1,  1,
     &  2,  9, 10,  1,  1, 11,  7,  7,  7,  6,
     &  5,  5,  8,  8,  8,  8,  8,  6, 11, 11,
     & 11, 11,  9,  9,  9, 10,  2,  5/
      DATA MAP3/
     &  2,  2,  3,  2,  2,  2,  2,  2,  2,  2,
     &  7,  2, 10,  2,  2, 11,  8,  8,  8,  6,
     &  1,  1,  5,  5,  5,  5,  5,  6, 11, 11,
     & 11, 11,  2,  2,  2, 10,  2,  5/
      DATA MAP4/
     &  9,  9,  3,  9,  9,  2,  2,  2,  2,  2,
     &  7,  2,  2,  2,  2,  2,  8,  8,  8,  6,
     &  5,  5,  5,  5,  5,  5,  5,  6,  2,  2,
     &  2,  2,  2,  2,  2,  2,  2,  5/
      DATA MAP5/
     &  9,  9,  3,  9,  9,  2,  2,  2,  2,  2,
     &  7,  2, 10,  2,  2,  2,  8,  8,  8,  6,
     &  5,  5,  5,  5,  5,  5,  5,  6,  2,  2,
     &  2,  2,  2,  2,  2, 10,  2,  5/
C----------
C  INITIALIZE INTERNAL VARIABLES:
C----------
      DATA ISPNUM/101,101,202,015,101,101,108,093,019,122,101/
C
C     THE SPECIES ORDER IS AS FOLLOWS:
C     1 = WHITE BARK PINE (WB)
C     2 = LIMBER PINE (LM) USE WB
C     3 = DOUGLAS-FIR (DF)
C     4 = WHITE FIR (WF)
C     5 = DUMMY
C     6 = ASPEN (AS) USE WB
C     7 = LODGEPOLE PINE (LP)
C     8 = ENGLEMAN SPRUCE (ES)
C     9 = SUBALPINE FIR (AF)
C    10 = PONDEROSA PINE (PP)
C    11 = OTHER ()
C
C
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'MAICAL',6,ICYC)
C
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5)
C
      IF (ISISP .EQ. 0) ISISP=3
      SSSI=SITEAR(ISISP)
      IF (SSSI .EQ. 0.) SSSI=140.0
C----------
C  SET UP MAPPING INDEX BY  MODEL TYPE.
C----------
      IF(IMODTY .EQ. 1) THEN
        IMAP=MAP1(ISISP)
      ELSEIF(IMODTY .EQ. 2) THEN
        IMAP=MAP2(ISISP)
      ELSEIF(IMODTY .EQ. 3) THEN
        IMAP=MAP3(ISISP)
      ELSEIF(IMODTY .EQ. 4) THEN
        IMAP=MAP4(ISISP)
      ELSE
        IMAP=MAP5(ISISP)
      ENDIF
C-------
C   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.
C-------
      ISICD=ISPNUM(IMAP)
      RMAI=ADJMAI(ISICD,SSSI,10.0,IERR)
      IF(RMAI .GT. 128.0)RMAI=128.0
      RETURN
      END
