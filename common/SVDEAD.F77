CODE SEGMENT SVDEAD
C----------
C  **SVDEAD   DATE OF LAST REVISION:  04/09/2013
C----------
      INTEGER    MXDEAD
      PARAMETER (MXDEAD=1000)

C     MXCWD is set to half of MXSVOB (see SVDATA)

      INTEGER    MXCWD
      PARAMETER (MXCWD=5000)

      INTEGER    ISNSP(MXDEAD),IYRCOD(MXDEAD),ISTATUS(MXDEAD),
     >           NCWD, NDEAD,ILYEAR,OIDTRE(MXDEAD)
      REAL       CRNDIA(MXDEAD),CRNRTO(MXDEAD),OLEN(MXDEAD),
     >           ODIA(MXDEAD),FALLDIR(MXDEAD),YHFHTS(MAXSP),
     >           SNGCNWT(MXDEAD,0:3),YHFHTH(MAXSP),HRATE(MAXSP),
     >           PBFALL(MXDEAD), SPROBS(MXDEAD,3),
     >           SNGDIA(MXDEAD), SNGLEN(MXDEAD),
     >           CWDDIA(MXCWD), CWDLEN(MXCWD), CWDPIL(MXCWD),
     >           CWDDIR(MXCWD), CWDWT(MXCWD)
      COMMON /SVDEAD/ ISNSP,CRNDIA,CRNRTO,OLEN,ODIA,FALLDIR,
     >                IYRCOD,ISTATUS,NDEAD,ILYEAR,YHFHTS,YHFHTH,
     >                HRATE,SNGCNWT,OIDTRE,PBFALL,SPROBS,SNGDIA,
     >                SNGLEN,NCWD,CWDDIA,CWDLEN,CWDPIL,CWDDIR,CWDWT
C
C     COMMON AREA FOR SNAGS
C
C     CWDDIA = DIAMETER OF THE CWD OBJECT (ZERO IF UNUSED)
C     CWDLEN = LENGTH OF THE CWD OBJECT
C     CWDPIL = PILED(1)/UNPILED(0) STATUS OF THE CWD OBJECT
C     CWDDIR = ORIENTATION OF THE CWD OBJECT
C     CWDWT  = WEIGHT (TONS) OF CWD OBJECT
C     ISNSP  = SPECIES CODE FOR SNAG
C     CRNDIA = CROWN DIAMETER OF SNAG AT TIME OF DEATH
C     CRNRTO = CROWN RATIO OF SNAG AT TIME OF DEATH
C     OLEN   = ORIGINAL TREE LENGTH
C     ODIA   = ORIGINAL TREE DIAMETER
C     SNGCNWT= SNAG CROWN WEIGHT BY SIZE CLASS (NOT ALL 
C              CLASSES ARE STORED): 0 : Foliage, 1 : <0.25
C              2 :  0.25 - 1, AND 3 : 1 - 3. THESE VALUES
C              ARE AS OF THE YEAR THE SNAG WAS BORN UNLESS
C              THAT YEAR IS PRIOR TO THE INVENTORY YEAR.
C     FALLDIR= A VECTOR TO DETERMINE IF A SNAG IS STANDING
C              0-360 = DIRECTION OF FALL
C              -1    = THE SNAG IS STANDING
C     IYRCOD = THE YEAR OF DEATH
C     ISTATUS= STATUS OF THE SNAG
C              (IF NEGATIVE, INDICATES THAT SNAG IS TO BE SALVAGED,
C               AND IS REMOVED IN THE NEXT CALL TO SVOUT)
C              0 = OPEN SLOT (NO SNAG)
C              1 = GREEN, HARD SNAG
C              2 = RED, HARD SNAG
C              3 = GREY, HARD SNAG
C              4 = GREY, SOFT SNAG
C              5 = BLACK, RECENTLY BURNED, STILL HAS CROWN
C              6 = BLACK, OLDER BURNED, NO CROWN
C             90 = WWPB MORTALITY, ONE OR LESS YEARS OLD (RED)
C             91 = WWPB MORTALITY, TWO YEARS OLD (DARK)
C             92 = WWPB MORTALITY, 3-4 YEARS OLD (GREY)
C     NCWD   = THE TOTAL NUMBER OF CWD RECORDS, FOR SVS
C     NDEAD  = THE TOTAL NUMBER OF SNAGS
C     ILYEAR = THE LAST YEAR ALL SNAGS WAS CALLED
C     YHFHTS = NUMBER OF YEARS AT WHICH HALF THE INITIAL
C        HEIGHT HAS BEEN LOST, FOR SOFT SNAGS
C     YHFHTH = NUMBER OF YEARS AT WHICH HALF THE INITIAL 
C        HEIGHT HAS BEEN LOST, FOR HARD SNAGS
C     HRATE  = THE RATE MODIFIER FOR HEIGHT LOSS
C     OIDTRE = Original IDTREE identifier that generated snag record
C     PBFALL = Post-burn fall rate
C     SPROBS = Trees/ac snag stockings:
C              SPROBS(I,1): snag recs, for original source tree rec and
C                           year of death, created at time of mortality.
C              SPROBS(I,2): snag recs, for original source tree rec and
C                           year of death, that are still standing.
C              SPROBS(I,3): expected snag/acre count for current snag
C                           record, if fractional fall probabilities
C                           were applied each year since death (as opposed
C                           to actual stochastic fall prediction)
C     SNGDIA = Current snag diameter.
C     SNGLEN = Current snag heights (lengths).
C
C-----END SEGMENT

