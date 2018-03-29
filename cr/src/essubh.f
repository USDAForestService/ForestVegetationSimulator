      SUBROUTINE ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,ISER,GENTIM,
     &  TRAGE)
      IMPLICIT NONE
C----------
C CR $Id: essubh.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
C     CREATED BY THE ESTABLISHMENT MODEL.
C
C
C     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
C     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
C     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
C     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
C     THE TREE.
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ESPARM.F77'
C
C
      INCLUDE 'ESCOMN.F77'
C----------
C  DECLARATIONS
C----------
      INTEGER I,N,ITIME,ISER
      REAL    AGE,HHT,EMSQR,DILATE,DELAY,ELEV,GENTIM,TRAGE
      REAL DANUW
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
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = DILATE
      DANUW = ELEV
      DANUW = EMSQR
      DANUW = REAL(ISER)
C
C----------
      N=INT(DELAY+0.5)
      IF(N.LT.-3) N=-3
      DELAY=FLOAT(N)
      ITIME=INT(TIME+0.5)
      IF(N.GT.ITIME) DELAY=TIME
      AGE=TIME-DELAY-GENTIM+TRAGE
      IF(AGE.LT.1.0) AGE=1.0
      TRAGE=TIME-DELAY
C
      SELECT CASE (I)
C----------
C     HEIGHT OF TALLEST SUBSEQUENT SPECIES 1=AF
C----------
      CASE (1)
        HHT = 0.75
C----------
C     HEIGHT OF TALLEST SUBS. SPECIES 2=CB
C----------
      CASE (2)
        HHT = 1.0
C----------
C     HT OF TALLEST SUBS. SPECIES 3=DF
C----------
      CASE (3)
        HHT = 2.0
C----------
C     HT OF TALLEST SUBS. SPECIES 4=GF
C----------
      CASE (4)
        HHT = 2.0
C----------
C     HT OF TALLEST SUBS. SPECIES 5=WF
C----------
      CASE (5)
        HHT = 2.0
C----------
C     HT OF TALLEST SUBS. SPECIES 6=MH
C----------
      CASE (6)
        HHT = 0.5
C----------
C     HT OF TALLEST SUBS. SPECIES 7=RC
C----------
      CASE (7)
        HHT = 1.5
C----------
C     HT OF TALLEST SUBS. SPECIES 8=WL
C----------
      CASE (8)
        HHT = 3.0
C----------
C     HT OF TALLEST SUBS. SPECIES 9=BC
C----------
      CASE (9)
        HHT = 0.5
C----------
C     HEIGHT OF TALLEST SUBS. SPECIES 10=LM
C----------
      CASE (10)
        HHT = 0.5
C----------
C     HT OF TALLEST SUBS. SPECIES 11=LP
C----------
      CASE (11)
        HHT = 3.0
C----------
C     HT OF TALLEST SUBS. SPECIES 12=PI, 33=PM, 34=PD, 35=AZ
C----------
      CASE (12,33:35)
        HHT = .5
C----------
C     HEIGHT OF TALLEST SUBS. SPECIES 13=PP, 36=CI
C----------
      CASE (13,36)
        HHT = 3.0
C----------
C     HT OF TALLEST SUBS. SPECIES 14=WB
C----------
      CASE (14)
        HHT = 1.0
C----------
C     HT OF TALLEST SUBS. SPECIES 15=SW
C----------
      CASE (15)
        HHT = 1.0
C----------
C     HT OF TALLEST SUBS. SPECIES 16=UJ, 29=AJ, 30=RM, 31=OJ, 32=ER
C----------
      CASE (16,29:32)
        HHT = 0.5
C----------
C     HT OF TALLEST SUBS. SPECIES 17=BS
C----------
      CASE (17)
        HHT = 2.0
C----------
C     HT OF TALLEST SUBS. SPECIES 18=ES
C----------
      CASE (18)
        HHT = 1.5
C----------
C     HT OF TALLEST SUBS. SPECIES 19=WS
C----------
      CASE (19)
        HHT = 1.0
C----------
C     HT OF TALLEST SUBS. SPECIES 20=AS, 28=PB
C----------
      CASE (20,28)
        HHT = 5.0
C----------
C     HEIGHT OF TALLEST SUBS. SPECIES 21=NC, 22=PW
C----------
      CASE (21:22)
        HHT = 10.0
C----------
C     HT OF TALLEST SUBS. SPECIES 23=GO, 24=AW, 25=EM, 26=BK, 27=SO
C----------
      CASE (23:27)
        HHT = 5.0
C----------
C     HT OF TALLEST SUBS. SPECIES 37=OS
C----------
      CASE (37)
        HHT = 1.0
C----------
C     HT OF TALLEST SUBS. SPECIES 38=OH
C----------
      CASE (38)
        HHT = 5.0
C
      END SELECT
C
      RETURN
      END
C**END OF CODE SEGMENT