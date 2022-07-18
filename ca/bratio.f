      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C CA $Id$
C----------
C
C FUNCTION TO COMPUTE BARK RATIOS.  THIS ROUTINE IS VARIANT SPECIFIC
C AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
      REAL BARKB(5,29),H,D,BRATIO,DIB
      INTEGER JBARK(MAXSP),IS,J,I
      REAL RDANUW
C
      DATA JBARK/
     & 12,  5, 12,  2,  2,  2,  1, 13, 13, 10,
     & 27, 10,  3, 10,  4, 25, 25,  3,  3, 26,
     & 12,  9, 29, 10,  3, 19, 18, 17, 20,  8,
     & 16, 24, 21,  6, 16, 14, 23,  7, 16, 16,
     & 16, 28, 16, 14, 16, 16, 16, 22, 16, 29/
C----------
C  202=DF, 015=WF, 122=PP, 116=JP, 081=IC, 312=BM, 431=GC, 815=WO,
C  093=ES, 108=LP, 098=SS, 242=RC, 263=WH, 351=RA, 001=OTHER SOFTWOODS,
C  818=BLK OAK, 807=BLUE OAK, 805=CAN LIVE OAK, 801=COAST LIVE OAK,
C  811=ENGELMANN OAK, 839=INTERIOR LIVE OAK, 981=BAY LAURAL,
C  361=MADRONE, 821=CALIF WHITE OAK, 631=TANOAK
C  117=SUGAR PINE, 127=DIGGER PINE, 103=KNOBCONE PINE,
C  211=REDWOOD/GIANT SEQUIOA
C
C BARK COEFFICIENT SOURCES:
C  202 --------------- FROM WALTERS ET.AL. RES BULL 50  TABLE 2
C  807,805,801,811,
C  839,981,821,631,
C  818,631,361
C  312,431,815 ------- FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
C  93,108,242,263 ---- FROM WYKOFF ET.AL. RES PAPER INT 133  TABLE 7
C
C  98 ---------------- FROM HARLOW & HARRAR, TEXTBOOK OF DENDRO PG 129
C  351 --------------- AVERAGE OF 312,361,431 VALUES FROM PILLSBURY &
C                      KIRKLEY TABLE 2, PLUS INFO FROM HARLOW & HARRAR
C  001 --------------- AVERAGE OF 019,081,093,108,119
C  015,116,117,122 --- FROM DOLPH PSW-368
C  127,103,101 ------- FROM WYKOFF AVERAGE OF AF,IC,ES,LP,WP
C  81 ---------------- FROM Dolph PSW-368
C  211,212-------------Equations fit by Castle 2019
C
C  NOTE: COEFFICIENTS FOR SPECIES IN PILLSBURY & KIRKLEY ARE METRIC. 
C  INTERCEPT WERE DIVIDED BY 2.54 TO CONVERT THESE
C  EQUATIONS TO ENGLISH.
C----------
      DATA ((BARKB(I,J),I=1,5),J=1,14)/
     & 202.,  0.903563,  0.989388, 0.0    , 1.,
     &  15., -0.1593  ,  0.8911  , 0.0    , 2.,
     & 122., -0.4448  ,  0.8967  , 0.0    , 2.,
     & 116., -0.4448  ,  0.8967  , 0.0    , 2.,
     &  81., -0.0549  ,  0.8374  , 0.0    , 2.,
     & 312.,  0.08360 ,  0.94782 , 0.0    , 2.,
     & 431.,  0.15565 ,  0.90182 , 0.0    , 2.,
     & 815., -0.30722 ,  0.95956 , 0.0    , 2.,
     &  93.,  0.9     ,  0.0     , 0.0    , 3.,
     & 108.,  0.9     ,  0.0     , 0.0    , 3.,
     &  98.,  0.958330,  1.0     , 0.0    , 1.,
     & 242.,  0.949670,  1.0     , 0.0    , 1.,
     & 263.,  0.933710,  1.0     , 0.0    , 1.,
     & 351.,  0.075256,  0.94373 , 0.0    , 2./
C
      DATA ((BARKB(I,J),I=1,5),J=15,29)/
     & 001.,  0.933290,  1.0     , 0.0    , 1.,
     & 818., -0.26824 ,  0.95767 , 0.0    , 2.,
     & 807., -0.17324 ,  0.94403 , 0.0    , 2.,
     & 805., -0.19128 ,  0.96147 , 0.0    , 2.,
     & 801., -0.75739 ,  0.93475 , 0.0    , 2.,
     & 811., -0.78572 ,  0.92472 , 0.0    , 2.,
     & 839.,  0.04817 ,  0.92953 , 0.0    , 2.,
     & 981., -0.12791 ,  0.96579 , 0.0    , 2.,
     & 361., -0.013484,  0.98155 , 0.0    , 2.,
     & 821., -0.38289 ,  0.93545 , 0.0    , 2.,
     & 117., -0.1429  ,  0.8863  , 0.0    , 2.,
     & 127.,  0.93290 ,  0.0     , 0.0    , 3.,
     & 101.,  0.93290 ,  0.0     , 0.0    , 3.,
     & 631., -0.26824 ,  0.95354 , 0.0    , 2.,
     & 211.,  0.70120 ,  1.04862 , 0.0    , 1./
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      RDANUW = H
C----------
C  EQUATION TYPES:
C  1  DIB = a * DOB ** b
C  2  DIB = a + bDOB
C  3  DIB = a*DOB
C----------
      IF (D .GT. 0) THEN 
        IF(BARKB(5,JBARK(IS)) .EQ. 1.)THEN
          DIB=BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
          BRATIO=DIB/D
        ELSEIF (BARKB(5,JBARK(IS)) .EQ. 2.)THEN
          DIB=BARKB(2,JBARK(IS)) + BARKB(3,JBARK(IS))*D
          BRATIO=DIB/D
        ELSEIF (BARKB(5,JBARK(IS)) .EQ. 3.)THEN
          BRATIO=BARKB(2,JBARK(IS))
        ENDIF
      ELSE
        BRATIO = 0.99
      ENDIF
C
      IF(BRATIO .GT. 0.99) BRATIO= 0.99
      IF(BRATIO .LT. 0.80) BRATIO= 0.80
C
      RETURN
      END
