      SUBROUTINE TMCOUP
      IMPLICIT NONE
C---------- 
C  **TMCOUP DATE OF LAST REVISION:  04/01/13
C---------- 
C     
C     THIS SUBROUTINE CALCULATES THE EFFECTS OF A DFTM OUTBREAK   
C     AND ACTS AS THE INTERFACE ROUTINE TO THE STAND  
C     PROGNOSIS MODEL.  
C     
C     MOST RECENT REVISION: NOV 1981 BY R.A. MONSERUD AND   
C     AND N.L. CROOKSTON
C     
C     CALLING SEQUENCE ASSUMPTIONS:   NORMAL DIAMETER 
C        AND HEIGHT GROWTH AND NORMAL MORTALITY HAVE BEEN   
C        CALCULATED FOR THIS PERIOD, BUT HAVE NOT BEEN
C        USED TO UPDATE HT, DBH, OR PROB YET.   
C        NORMAL MORTALITY LOSSES ARE STORED IN WK2.   
C
C Revision History:
C   23-DEC-99 Lance R. David (FHTET)
C         Updated for expansion of FVS stand id (variable NPLT)
C         from 8 to 26 characters.
C   07-AUG-01 Lance R. David (FHTET)
C         Initialization of arrays ISC and ITABSV.
C   01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C
C**********************************************************************

C
COMMONS
C     
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'TMCOM1.F77'

      INCLUDE 'BIOMAS.F77'

      INCLUDE 'TMEGGS.F77'

      INCLUDE 'UPPER.F77'

      INCLUDE 'LOWER.F77'

      INCLUDE 'DFOL.F77'

      INCLUDE 'ICOND.F77'
C     
COMMONS     
C     
C TMCOUP internal variable descriptions.
C
C   AMORT  - Real (14,9).  Holds percentages and probabilities for
C            mortality and top-kill calculations and conditions.
C            Remember that with the data statement, the array is loaded
C            in a column major order, not row major.  The DATA statement
C            loads the array in the following order (1,1) (2,1) (3,1)
C            (4,1). . .(1,2) (2,2) (3,2). . .(14,9).
C                                                                         
C                                        rows
C                             Tree Percent Defoliation Classes
C            |                                                            
C            |----------DF Classes 1-7---------|--------GF Classes 1-7-------|
C columns    |_1____2____3____4____5____6____7____8____9__10__11__12__13__14__
C          1 |                                                            
C          2 |                                                            
C          3 |                                                            
C          4 |                                                            
C          5 |                                                            
C          6 |                                                            
C          7 |                                                            
C          8 |                                                            
C          9 |
C
C              col 1   -- Primary Mortality Percent
C              col 2   -- Secondary Mortality Percent
C              col 3   -- used in condition test with a random probability
C                         to determine if top-kill is to be applied to a 
C                         a tree defoliation class
C              col 4   -- not used
C              col 5-9 -- probabilities used in a accumulative manner
C                         to determine the top-kill class (1-5) the 
C                         tree will be in when top-kill has occured
C                         based on its tree defoliation class.
C                         Actual top-kill percentage is calculated from
C                         values in arrays TKBOT and TKTOP.
C
C
C   DGLOSS - Real.  Holds the calculated value of diameter growth loss.
C
C   DPMAX  - Real.  The maximum branch defoliation percentage for the
C            current tree class.  (from Phase II and Phase III defoliation
C            percentages stored in common variable DPCENT).
C
C   G19MAX - Real.  The maximum number of days for which food demands were
C            not fully satisfied for the current tree class.  (from Phase II
C            or Phase III values stored in common variable G19OUT).
C
C   ITABSV - Integer.  Holds the row index to the AMORT table.  This
C            value identifies the tree defoliation class (1-7 for DF
C            and 8-14 for GF).
C
C   RGLOSS - Real (14).  Holds the static percentages used for calculating 
C            the diameter growth loss for each tree defoliation class.
C            (1)-(7)  -- DF values
C            (8)-(14) -- GF values
C
C   TKBOT  - Real (5).  Bottom threshold percentages for top-kill
C            calculations of top-kill classes 1-5.
C
C   TKTOP  - Real (5).  Top threshold percentages for top-kill
C            calculations of top-kill classes 1-5.
C
      CHARACTER*4 KSPP(3)

      INTEGER I, I1, I2, I7, ICRI, IFIN,
     &        II, IJUMP, IP, IPT(MAXTRE), ISC(100,2), 
     &        ISPLIT, ITAB, ITABSV(100), ITWO, J, JCLAS2(100),
     &        JTRUNK, K, KEY(9), KODE, LEFT,
     &        MAXCLS, NACL, NRECS, NUM, NUMCLS(3)

      REAL A3(1), A4(1), A5(1), A6(1), A7(1), A8(1), ACBASE, ACR, ADBH,
     &     ADG, ADGLOS, AHT, AHTG, AHTLOS, AMORT(14,9), APCTKL, ATKILL,
     &     CBASE, CLAS(100,7), CR, CROWN, DELTA,
     &     DFMEAN, DGLOSS, DPMAX,
     &     EGGS(100), G19MAX, 
     &     GFMEAN, HTGLOS, HTRUNC
      REAL P1, P2, P3, PCKILL, PRMORT, PRMS(2), PRNORM,
     &     PRSUM, PRTOPK, RANDOM, RGLOSS(14), SALKOD, SPROB,
     &     T, TKBOT(5), TKILL, TKTOP(5), TMBCHL,
     &     TSAVE(100), WBDEF(3), WCBASE(3), WCR(3),
     &     WDBH(3), WDG(3), WDGLOS(3), WEGGS(3),  WHT(3),
     &     WHTG(3), WHTLOS(3), WPCTKL(3), WPMORT(3), WRECS(3), WTDEF(3),
     &     WTKILL(3), WZ2(3), WZ3(3), WZ4(3), 
     &     ZZ1, ZZ2

      EQUIVALENCE (CLAS(1,1), Z4(1)), (JCLASS(1), JCLAS2(1))
      EQUIVALENCE (IPT(1), WK6(1))  

      DATA AMORT/0.0000, 0.0000, 0.0090, 0.0280, 0.1730, 0.4770, 0.9230,
     >           0.0000, 0.0000, 0.0090, 0.0280, 0.1730, 0.4770, 0.9230,
     >           0.0760, 0.0760, 0.0760, 0.1075, 0.1999, 0.1144, 0.0168,
     >           0.0442, 0.0428, 0.0452, 0.0587, 0.0926, 0.0778, 0.0114,
     >           0.1158, 0.2081, 0.4163, 0.4820, 0.2888, 0.2005, 0.0295,
     >           0.0559, 0.1271, 0.2703, 0.3838, 0.3441, 0.2149, 0.0316,
     >           0.7570, 0.6562, 0.4736, 0.3825, 0.3383, 0.2081, 0.0306,
     >           0.9000, 0.8301, 0.6755, 0.5295, 0.3903, 0.2303, 0.0339,
     >           0.0694, 0.1360, 0.1800, 0.2376, 0.1376, 0.1532, 0.0226,
     >           0.0341, 0.0877, 0.1332, 0.1733, 0.1933, 0.0963, 0.0142,
     >           0.0464, 0.0696, 0.2125, 0.1606, 0.0599, 0.0384, 0.0057,
     >           0.0123, 0.0293, 0.0891, 0.1021, 0.0587, 0.0401, 0.0059,
     >           0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
     >           0.0039, 0.0078, 0.0317, 0.0434, 0.0225, 0.0274, 0.0040,
     >           0.0000, 0.0000, 0.0238, 0.0838, 0.0775, 0.0000, 0.0000,
     >           0.0038, 0.0019, 0.0122, 0.0479, 0.0157, 0.0000, 0.0000,
     >           0.0000, 0.0026, 0.0000, 0.0000, 0.0138, 0.0089, 0.0013,
     >           0.0018, 0.0005, 0.0040, 0.0172, 0.0539, 0.0511, 0.0075/

      DATA RGLOSS /0.846, 0.716, 0.648, 0.610, 0.569, 0.388, 0.388,     
     >             0.785, 0.693, 0.628, 0.596, 0.559, 0.455, 0.455 /    

      DATA TKBOT /0.0, 0.0, 0.10, 0.25, 0.50/   
      DATA TKTOP /0.0, 0.10, 0.25, 0.50, 0.99/  

      DATA MAXCLS / 100 /,  ITWO / 2 /    
      DATA ISPLIT / 3 / 

C     
C     SET PARAMETERS FOR CROWN DEFOLIATION EQUATION   
C     
      DATA P1 / -22.579678 /, P2/ -22.996253 /, P3/ -598181961.0 /

      DATA KEY / 3, 4, 6*0, 1  /    
      DATA KSPP / '    ', '    ', 'HOST' /

C
C     INITIALIZE KSPP.
C
      KSPP(1)(1:2) = NSP(IDFCOD,1)
      KSPP(2)(1:2) = NSP(IGFCOD,1)
      
      DO 5 I=1,100
        ISC(I,1)  = 0
        ISC(I,2)  = 0
        ITABSV(I) = 0
    5 CONTINUE

C
C     WRITE A HEADER    
C     
      WRITE(JODFTM,10) NPLT, MGMID
   10 FORMAT ('1',19('-'),'  DFTM DEFOLIATION STATISTICS FOR STAND ',
     >       A26,'; MANAGEMENT ID= ',A4,2X,23('-')/)

C
C     WRITE A WARNING IF IFINT IS NE TO 5
C
      IF (IFINT .NE. 5) WRITE (JODFTM,20) IFINT
   20 FORMAT (/,' ***** WARNING:  THE CALCULATIONS OF TUSSOCK',
     >        ' MOTH IMPACT ON TREE GROWTH ASSUME THAT A 5 YEAR',
     >        ' PROJECTION CYCLE IS BEING USED.',/,
     >        T18,'YOU ARE USING A ',I2,' YEAR CYCLE.')

      WRITE (JODFTM,30) TMYRS(ICYC), ICYC, IY(ICYC), IY(ICYC+1)
   30 FORMAT (' THE FIRST YEAR OF THIS SIMULATED TUSSOCK MOTH ',
     >        'OUTBREAK IS YEAR ',I5,'.  IT IS BEING SIMULATED',/,
     >        ' AS PART OF CYCLE',I3,' WHICH REPRESENTS YEARS',
     >        I5,' TO',I5,' IN THIS STANDS DEVELOPMENT.')

C
C     IF POST PROCESSOR OUTPUT THEN PRINT THE OUTBREAK YEAR.
C
      IF (JODFEC .GT. 0) WRITE (JODFEC,35) TMYRS(ICYC)
   35 FORMAT(I4)

      IF (TMDEBU) THEN
         WRITE (JODFTM,40) CNTDF, CNTGF
   40    FORMAT (//, 10('*'),'  TMCOUP . . .',/,
     >          ' CNTDF = ',F10.4,' CNTGF = ',F10.4)

         WRITE (JODFTM,50)
   50    FORMAT (' PROB . . .'//)

         WRITE (JODFTM,60) (PROB(I),I=1,ITRN)
   60    FORMAT (10F13.3)
      ENDIF
C
C     LOAD ARRAY 'IPT'
C
      DO 70 I=1,ITRN
        IPT(I) = IND1(I)
   70 CONTINUE

      IFIN = NACLAS(1) + NACLAS(2)
      NUMCLS(1) = NACLAS(1)
      NUMCLS(2) = NACLAS(2)
      NUMCLS(3) = IFIN

      IF (LDF) THEN
C
C        ** LOAD DOUGLAS FIR
C
         IF (TMDEBU) WRITE (JODFTM,80)
   80    FORMAT (' CALLING GARBEL (DF)')
C
C        CALL DATA COMPRESSION ROUTINE
C
         I = ISCT(IDFCOD,1)
         I7 = 7

         CALL GARBEL (IPT(I), (ISCT(IDFCOD,2)-I+1),
     >     KEY, WEIGHT, I7, NACLAS(1), TMPN1, CLAS, ISC, WK3, PCNEWF,
     >     FBIOMS, A3, A4, A5, A6, A7, A8, PROB, KODE, MAXCLS, ITWO)

         IF (KODE .NE. 0) WRITE (JODFTM,90)
   90    FORMAT (' COMPRESSION ROUTINE FAILED FOR DOUGLAS FIR')

C
C        ADD THE PROPER OFFSET TO SECTOR POINTERS
C
         I2 = NACLAS(1)
         DO 100 I = 1, I2
            ISC(I,1) = ISC(I,1) + ISCT(IDFCOD,1) - 1
            ISC(I,2) = ISC(I,2) + ISCT(IDFCOD,1) - 1
C
C           SET SPECIES CODE
C
            IZ6(I) = 1
  100    CONTINUE
      ENDIF

      IF (LGF) THEN
C
C        ** LOAD GRAND FIR
C
         IF (TMDEBU) WRITE (JODFTM,110)
  110    FORMAT (' CALLING GARBEL (GF)')

C
C        CALL DATA COMPRESSION ROUTINE
C
         I1 = NACLAS(1) + 1
         I = ISCT(IGFCOD,1)
         I7 = 7
         CALL GARBEL (IPT(I), (ISCT(IGFCOD,2)-I+1), KEY, WEIGHT,
     >        I7, NACLAS(2), TMPN1, CLAS(I1,1), ISC(I1,1), WK3, PCNEWF,
     >        FBIOMS, A3, A4, A5, A6, A7, A8, PROB, KODE, MAXCLS, ITWO)

         IF (KODE .NE. 0) WRITE (JODFTM,120)
  120    FORMAT (' COMPRESSION ROUTINE FAILED FOR GRAND FIR')

C
C        ADD THE PROPER OFFSET TO SECTOR POINTERS
C
         DO 130 I = I1, IFIN
            ISC(I,1) = ISC(I,1) + ISCT(IGFCOD,1) - 1
            ISC(I,2) = ISC(I,2) + ISCT(IGFCOD,1) - 1
C
C           SET SPECIES CODE
C
            IZ6(I) = 2
  130    CONTINUE
      ENDIF
C     
C     CREATE 'JCLAS2' AS A SORTED ORDER OF THE TREE CLASSES
C     IN DESENDING AVERAGE CLASS DIAMETER.
C     
C     STEP1: COMPUTE AVERAGE DBH FOR EACH TREE CLASS (STORE IN WK3)     
C     
      DO 150 I=1,IFIN
         I1 = ISC(I,1)
         I2 = ISC(I,2)
         ADBH = 0.0
         SPROB = 0.0

         DO 140 J=I1,I2
            IP = IPT(J)
            ADBH = ADBH + DBH(IP) * PROB(IP)
            SPROB = SPROB + PROB(IP)
  140    CONTINUE

         WK3(I) = ADBH / SPROB
         JCLAS2(I) = I
  150 CONTINUE

C     
C     STEP2: SORT JCLAS2 ON WK3     
C     
      CALL RDPSRT (NACLAS(1), WK3, JCLAS2, .FALSE.)   
      J = NACLAS(1) + 1 
      CALL RDPSRT (NACLAS(2), WK3, JCLAS2(J), .FALSE.)

      DFMEAN = DFREGG(1)

      IF (DFREGG(3) .GE. 0.001) THEN
C
C        RANDOMLY CHOOSE DF MEAN LARVAL DENSITY FOR EACH OUTBREAK
C        IF THE BETWEEN-OUTBREAK STANDARD DEVIATION IS POSITIVE.
C
  160    ZZ1 = TMBCHL(0.0,1.0)
         DFMEAN = DFREGG(1) + DFREGG(3) * ZZ1
         IF (DFMEAN .LT. 0.001) GOTO 160
      ENDIF

      GFMEAN = GFREGG(1)

      IF (GFREGG(3) .GE. 0.001) THEN
C
C        RANDOMLY CHOOSE GF MEAN LARVAL DENSITY FOR EACH OUTBREAK
C        IF THE BETWEEN-OUTBREAK STANDARD DEVIATION IS POSITIVE.
C
C        THE DEVIATES FOR BOTH HOST SPECIES SHOULD BE HIGHLY CORRELATED
C
         ZZ2 = TMBCHL(0.0, 0.1)
         GFMEAN = GFREGG(1) + GFREGG(3) * (ZZ1 + ZZ2)
         IF (GFMEAN .LT. 0.001) GFMEAN = 0.0
      ENDIF

C
C     FILL IN THE REST OF THE 'ICOND' COMMON AREA
C
      I1 = 0
      I2 = 0

      DO 170 II=1,IFIN
        I = JCLAS2(II)  
        X5(I) = Z2(I) * Z3(I) / 100.0     
        X6(I) = Z3(I) - X5(I) 
        Z5(I) = 1.0     
C     
C       ASSIGN EGGS TO TREE CLASSES 
C     
        IF (IEGTYP .EQ. 2) THEN
C     
C          DETERMINISTICALLY ALLOCATE EGGS BY CROWN (ACTUALLY DBH) CLASS
C          (NOTE THAT THE TREE CLASSES ARE IN DECENDING ORDER OF DBH,
C          SINCE CLASSIFICATION HAS BEEN ON A MONOTONIC FUNCTION OF DBH)   
C     
           NACL = NACLAS(1)
           IF (IZ6(I) .EQ. 2) NACL = NACLAS(2)     
           LEFT = II 
           IF (IZ6(I) .EQ. 2) LEFT = II - NACLAS(1)
           NUM = ((LEFT * ISPLIT - 1) / NACL) + 1  
           IF (IZ6(I) .EQ. 1) X7(I) = DFEGG(NUM)   
           IF (IZ6(I) .EQ. 2) X7(I) = GFEGG(NUM)   
        ELSE
C     
C          RANDOMLY ALLOCATE EGGS TO TREE CLASSES IF IEGTYP = 1
C     
           IF (IZ6(I) .EQ. 1) X7(I) = TMBCHL(DFMEAN, DFREGG(2))
           IF (IZ6(I) .EQ. 2) X7(I) = TMBCHL(GFMEAN, GFREGG(2))
           IF (X7(I) .LT. 0.0) X7(I) = 0.0   
        ENDIF     

C
C       SAVE THE EGG COUNTS FOR PRINTING  
C     
        EGGS(I) = X7(I) 
  170 CONTINUE

      IF (TMDEBU) THEN
C     
C        WRITE DEBUG OUTPUT
C     
         WRITE (JODFTM,180) (IPT(I),I=1,ITRN)
  180    FORMAT (//,' IPT . . .',//,135(10I13,/))

         WRITE (JODFTM,190) ((ISC(I1,I2),I2=1,2),I1=1,IFIN)
  190    FORMAT (//,' CLASS SECTOR POINTERS (ISC) . . .',//,10(10I13,/))

         WRITE (JODFTM,200) IFIN,NACLAS
  200    FORMAT (//,' IFIN= ',I6,'  NUMBER ACTUAL CLASSES (NACLAS)=',
     >          2I9,/,' DUMP OF COMMON AREA ''ICON''. . . ',//,
     >          T10,'IZ6',10X,'Z4',11X,'Z5',11X,'Z2',11X,'Z3',    
     >          11X,'X5',11X,'X6',11X,'X7',6X,'JCLASS',/,   
     >          6X,'SPECIES       PROB  BIOMAS/TREE       PCNEWF',
     >          7X,'FBIOMS     NEW BMAS     OLD BMAS',9X,'EGGS',/)

         DO 210 I=1,IFIN
            WRITE (JODFTM,220) IZ6(I), Z4(I), Z5(I), Z2(I), Z3(I),
     >                        X5(I), X6(I), X7(I), I
  210    CONTINUE
  220    FORMAT (1X,I10,7F13.3,I10)
      ENDIF

      IF (LPUNCH) THEN
C
C        WRITE DFTMOD INPUT TO DISK FOR LATER RETRIEVAL
C
         WRITE (JOTMDK,230)
  230    FORMAT (' LIST OF INPUT TO DFTMOD:')

         DO 240 I=1,IFIN
            WRITE (JOTMDK,250) IZ6(I), Z4(I), Z5(I), Z2(I), Z3(I),
     >                          X5(I), X6(I), X7(I), I, NPLT
  240    CONTINUE
  250    FORMAT (I2,7F9.4,I3,4X,A4,1X,A26)
      ENDIF

      IF (TMDEBU) WRITE (JODFTM,260)
  260 FORMAT (//,10('*'),' CALLING DFTMOD')

C
C     ** CALCULATE % BRANCH DEFOLIATION
C     Array DPCENT is loaded
C
      CALL DFTMOD (1,IFIN)

      IF (TMDEBU) WRITE (JODFTM,270)
  270 FORMAT (//,10('*'),' RETURN FROM DFTMOD')

C
C     IF ( LPUNCH ) WRITE (JOTMDK,280)
C 280 FORMAT (' LIST OF DEFOLIATION PERCENTS BY TREE CLASS:')
C

      DO 320 I = 1,IFIN
         I1 = ISC(I,1)
         I2 = ISC(I,2)
C
C        COMPUTE MAXIMUM BRANCH DEFOLIATION (EITHER PHASE II OR III)
C     
         DPMAX = DPCENT(I,1)   
         IF (DPCENT(I,2) .GT. DPMAX) DPMAX = DPCENT(I,2)     

C     
C        COMPUTE CROWN DEFOLIATION AND FIND ASSOCIATED LINE  
C        OF MORTALITY AND TOP-KILL TABLE (AMORT) 
C
         G19MAX = G19OUT(I,1)  
         IF (G19OUT(I,2) .GT. G19MAX) G19MAX = G19OUT(I,2)   

         IF (G19MAX .GT. 0.001) GOTO 290
C
C        T = PERCENT TREE DEFOLIATION
C
         T = (P1 / (P2 + P3 * EXP(P1 * DPMAX / 100.0))) * 100.0
         GOTO 300

  290    T = 97.7911 + (G19MAX / 5.0) * 2.2089
         IF (G19MAX .GE. 5.0) T = 100.0

  300    IF (T .LE. 15.0) ITAB = 1

C*************************************
C*  For Debug purposes only          *
C*************************************
C*       WRITE(*,*)
C*   & ' DPMAX (Branch % defol) = ',DPMAX,'   T (Tree % Defol) = ',T
C*************************************

C
C        Determine Tree Defoliation Class for references to AMORT table.
C
C
         IF (T .GT. 15.0 .AND. T .LE. 35.0) ITAB = 2
         IF (T .GT. 35.0 .AND. T .LE. 65.0) ITAB = 3   
         IF (T .GT. 65.0 .AND. T .LE. 85.0) ITAB = 4
         IF (T .GT. 85.0 .AND. T .LE. 95.0) ITAB = 5
         IF (T .GT. 95.0 .AND. T .LE. 99.5) ITAB = 6
         IF (T .GT. 99.5) ITAB = 7
         IF (IZ6(I) .EQ. 2) ITAB = ITAB + 7

C
C        IZ6 IS SPECIES CODE IN THE DFTM BRANCH MODEL
C        ( DFIR IF IZ6 = 1 AND GFIR IF IZ6 = 2 )
C
C        ITAB  WILL NOW BE THE PROPER ROW FOR TREE CLASS ICLASS
C        IN THE AMORT TABLE.
C
         ITABSV(I) = ITAB
         TSAVE(I) = T

         DO 310 J = I1,I2
            IP = IPT(J)
C
C           NOW CALCULATE PR(MORTALITY) FROM THE AMORT TABLE
C           NOTE THAT THIS INCLUDES DIRECT AND SECONDARY MORTALITY
C
            PRMORT = AMORT(ITAB,1) + AMORT(ITAB,2)
C
C           CALCULATE THE NORMAL MORTALITY RATE (PRNORM) APPLICABLE 
C           IF THERE HAD BEEN NO DFTM OUTBREAK.   
C     
            PRNORM = WK2(IP) / PROB(IP)     

            IF (PRMORT .LT. PRNORM) PRMORT = PRNORM     

            IF (ITAB .EQ. 1 .OR. ITAB .EQ. 8) THEN
C     
C              Use maximum branch defoliation to interpolate between
C              the normal mortality rate and the AMORT mortality rate
C              when defoliation is very light.   
C     

               DELTA = (PRMORT - PRNORM) * (1.0 - (DPMAX / 68.03))
               IF (DELTA .GE. 0.0) PRMORT = PRMORT - DELTA 
            ENDIF
C     
C           Save DFTM mortality losses in WK2.
C
            WK2(IP) = PROB(IP) * PRMORT

  310    CONTINUE
  320 CONTINUE

      IF (ITMSLV .EQ. 1) THEN
C
C        IF SALVAGE METHOD (ITMSLV) IS 1 THEN:
C        SET SALVAGE OPTION FOR ICYC+1
C        THE SALVAGE CODE (KUTKOD FOR THINPRSC) IS 73
C
         SALKOD = 73

         PRMS(1) = EFF
         PRMS(2) = SALKOD
         CALL OPADD (IY(ICYC+1),229,0,2,PRMS,K)
         IF (K .NE. 0) ITMSLV = 0
         CALL OPINCR (IY, ICYC, NCYC)
      ENDIF

      IF (ITMREP .GT. 1) WRITE (JODFTM,340)
  340 FORMAT(//1X,44('-'),'  SUMMARY OF TREE CLASS CHARACTERISTICS  ',
     >      46('-')//1X,23('-'),' BEFORE OUTBREAK ',30('-'),2X,21('-'), 
     >      ' AFTER OUTBREAK ',22('-')/T49,'MIDCROWN SAMPLE BRANCH',    
     >      T75,'PRECENTAGE',T96,'--- 5 YEAR CHANGE IN: ---'/T15, 
     >      'RECORDS',T49,23('-'),3X,'DEFOLIATION    FIVE   DIAMETER',  
     >      7X,'HEIGHT',6X,'AMOUNT OF'/' TREE',T15,'PER',4X,'TREES',    
     >    T42,'LIVE   PERCENT  FOLIAGE FIRST',T74,13('-'),'   YEAR  ',  
     >      10('-'),3X,12('-'),'   TOP-KILL'/' CLASS  HOST  TREE   ',   
     >   'PER    DBH  HEIGHT  CROWN    NEW    BIOMASS INSTAR  BRANCH ', 
     >     '  TREE   MORT  NET   LOSS    NET    LOSS  ',10('-')/  
     >     ' NO.    SPP.  CLASS  ACRE   (IN)   (FT)  RATIO  FOLIAGE',   
     >     '  (GRAMS) LARVAE    (%)    (%)    RATE  (IN)  (IN)    ',    
     >     '(FT)   (FT)  (FT)   (%)'/1X,70('-'),2X,59('-')) 

C     
C     CALCULATE GROWTH LOSSES AND TOP-KILL DAMAGES DUE TO DFTM    
C     

      IF (ITMREP .GT. 1) THEN
C     
C        INITIALIZE WEIGHTED AVERAGES FOR SPECIES AND HOST SUMMARIES 
C        ( USE PREFIX W )  
C     
C        ( I=1: DF   I=2: GF     I=3: ALL HOST ) 
C
         DO 350 I = 1,3

            WDBH(I)   = 0.0   
            WHTG(I)   = 0.0   
            WHT(I)    = 0.0    
            WDG(I)    = 0.0    
            WCR(I)    = 0.0    
            WCBASE(I) = 0.0 
            WHTLOS(I) = 0.0 
            WDGLOS(I) = 0.0 
            WPCTKL(I) = 0.0 
            WTKILL(I) = 0.0 
            WEGGS(I)  = 0.0  
            WRECS(I)  = 0.0  
            WZ4(I)    = 0.0    
            WZ2(I)    = 0.0    
            WZ3(I)    = 0.0    
            WBDEF(I)  = 0.0  
            WTDEF(I)  = 0.0  
            WPMORT(I) = 0.0 
  350    CONTINUE
      ENDIF

      DO 430 I = 1,IFIN
        I1 = ISC(I,1)
        I2 = ISC(I,2)

        IF (ITMREP .GT. 1) THEN
C
C          INITIALIZE WEIGHTED AVERAGES FOR TREE CLASS SUMMARY STATISTICS
C
           ADBH   = 0.0
           AHTG   = 0.0
           AHT    = 0.0 
           ADG    = 0.0 
           ACR    = 0.0 
           ACBASE = 0.0    
           AHTLOS = 0.0    
           ADGLOS = 0.0    
           APCTKL = 0.0    
           ATKILL = 0.0    
        ENDIF 
C
C       COMPUTE MAXIMUM BRANCH DEFOLIATION (EITHER PHASE II OR III)     
C     
        DPMAX = DPCENT(I,1)   
        IF (DPCENT(I,2) .GT. DPMAX) DPMAX = DPCENT(I,2)     
C
C       Set AMORT table tree defoliation class index (ITAB)
C       and percent total tree defoliation for the tree class (T)
C
        ITAB = ITABSV(I)
        T = TSAVE(I)    

C     
C       ALLOCATE DAMAGE TO ALL TREES IN CLASS I 
C     
        DO 380 J = I1,I2
          IP = IPT(J)
          NRECS = I2 - I1 + 1 
          K = 0   
          PCKILL = 0.0  
          HTGLOS = 0.0
          TKILL  = 0.0   
          DGLOSS = 0.0    
          PRSUM  = 0.0     
C     
C         CALCULATE HEIGHT TO CROWN BASE BEFORE ANY HEIGHT REDUCTION.   
C     
          ICRI = ICR(IP)
          IF (ICRI .LT. 0) ICRI = -ICRI   
          CBASE = HT(IP) * (100 - ICRI) / 100.0 
          CROWN = HT(IP) - CBASE    
          CR = ICRI / 100.0   
C     
C         RECONSTRUCT PRMORT  
C     
          PRMORT = WK2(IP) / PROB(IP)     
C     
          ACBASE = ACBASE + CBASE * PROB(IP)    
          ADBH = ADBH + DBH(IP) * PROB(IP)
          AHT = AHT + HT(IP) * PROB(IP)   
          ACR = ACR + CR * PROB(IP)
C     
C         DETERMINE TOP-KILL DAMAGE 
C     
          CALL TMRANN(PRTOPK) 

          IF (PRTOPK .LT. AMORT(ITAB,3) .AND. DPMAX .GT. 0.0001) THEN
C
C            Top-Kill has occured.  DETERMINE THE TOP-KILL CLASS: K 
C     
             PRSUM = 0.0   

             DO 360 K = 1,5
C
C              Accumulate top-kill probabilities from AMORT table until
C              the sum is greater than the probability returned by the
C              random number generator.  The loop (1 thru 5) during
C              which this happens determines the top-kill class and the 
C              loop is exited.
C
               PRSUM = PRSUM + AMORT(ITAB,4+K)

               IF (PRTOPK .LE. PRSUM) THEN
C
C                 Process the tree for the current top-kill class (K)
C                 and exit the loop.

                  IF (K .EQ. 1) THEN
C     
C                    Kill one year of leader growth and exit loop
C     
                     HTGLOS = HTG(IP) * (1.0 / FINT)     
                     HTG(IP) = HTG(IP) - HTGLOS    
                  ELSE
C     
C                    Use random linear interpolation to compute percent of
C                    total crown top-killed  
C     
                     CALL TMRANN(RANDOM)     
                     PCKILL = TKBOT(K) + RANDOM * (TKTOP(K) - TKBOT(K))    
C
C                    NORMHT CONTAINS THE HEIGHT OF THE TREE JUST BEFORE    
C                    TRUNCATION (TOP-KILL), AND ITRUNC CONTAINS THE HEIGHT 
C                    OF TRUNCATION.  THESE VARIABLES MUST BE USED    
C                    IN ALL SUBSEQUENT VOLUME CALCULATIONS SO THAT THE     
C                    PROPER FORM OF THE TREE IS MAINTAINED. TO REMOVE
C                    THIS CULL AT THE TOP, CALCULATE VOLUME OF A BEHRE
C                    HYPERBOLOID WITH TOTAL HEIGHT, NORMHT, AND TRUNCATION
C                    HEIGHT AT ITRUNC.  NORMHT AND ITRUNC ARE NON-ZERO ONLY
C                    FOR TRUNCATED TREES; THUS NORMHT AND ITRUNC ARE ZEROED
C                    OUT AT RUN INITIATION.  THESE TWO VARIABLES ARE CHANGED
C                    ONLY IF JTRUNK = 1.  SEE VOLS FOR FURTHER DESCRIPTION.   
C     
                     HTGLOS = CROWN * PCKILL 
                     TKILL = HTGLOS    
                     HTRUNC = HT(IP) - HTGLOS
                     JTRUNK = 0

                     IF (HTGLOS .GT. 5.0 .AND. HT(IP) .GT. 20.0) THEN
                        JTRUNK = 1
                     ENDIF
C     
C                    TEST TO SEE IF THE TREE HAS BEEN PREVIOUSLY TOP-KILLED
C                    BELOW THE CURRENT HEIGHT OF TRUNCATION.   
C     
                     IF ((ITRUNC(IP) / 100.0) .LT. HTRUNC
     &                  .AND. ITRUNC(IP) .GT. 0) JTRUNK = 0   

                     IF (JTRUNK .EQ. 1) THEN
C     
C                       Truncate the top-killed trees.
C     
                        NORMHT(IP) = IFIX(HT(IP) * 100.0 + 0.5)  
                        ITRUNC(IP) = IFIX(HTRUNC * 100.0 + 0.5)  
                     ENDIF
C
C                    Adjust height and height growth to reflect truncation.
C
                     HT(IP) = HTRUNC
                     HTG(IP) = HTG(IP) - HTGLOS
                  ENDIF
C
C                 Exit loop
C
                  GOTO 370
               ENDIF
C
C            End of DO loop to determine top-kill class
C
  360        CONTINUE
  370        CONTINUE
          ELSE
C
C            No top-kill has occured.
C            Calculate annual height reduction.
C     
             HTGLOS = HTG(IP) * DPMAX / 100.0 * (1.0 / FINT)   
             HTG(IP) = HTG(IP) - HTGLOS
          ENDIF

C     
C         REDUCE DIAMETER GROWTH    
C     
C         NOTE THAT DBH HAS NOT BEEN UPDATED IN TREGRO YET, SO    
C         DO NOT SUBTRACT DGLOSS FROM DBH.
C
          DGLOSS = DG(IP) * (1.0 - RGLOSS(ITAB))
          IF (DGLOSS .GT. DG(IP)) DGLOSS = DG(IP)     
          DG(IP) = DG(IP) - DGLOSS  
C     
C         MODIFY CROWN RATIO FOR THE REVISED HT.
C     
          CR = (1.0 - (CBASE / HT(IP))) * 100.0 
          ICRI = IFIX(CR + 0.5)     
C     
C         CLASSIFY TREES AS LIVE CULLS IF CROWN RATIO IS < 5% OR  
C         IF THE HEIGHT OF TRUNCATION IS LESS THAN 17 FT.   
C     
          IF (ITRUNC(IP) .GT. 0 .AND. ITRUNC(IP) .LT. 1700) IMC(IP) = 3 
          IF (ICRI .LT. 5) IMC(IP) = 3    
          IF (ICRI .LT. 5) ICRI = 5 
          ICR(IP) = ICRI

C     
C         SET ICR NEGATIVE IF TREE HAS BEEN TOP-KILLED;     
C         THIS SIGNALS CROWN NOT TO CALCULATE A CHANGE
C         IN CROWN RATIO.     
C     
          IF (JTRUNK .EQ. 1 .AND. ICR(IP) .GT. 0) ICR(IP) = -ICR(IP)    

          IF (ITMREP .LT. 2) THEN
             CONTINUE
          ELSE
C
C            Accumulate weighted averages
C
             ADG = ADG + DG(IP) * PROB(IP)   
             AHTG = AHTG + HTG(IP) * PROB(IP)
             ADGLOS = ADGLOS + DGLOSS * PROB(IP)   
             AHTLOS = AHTLOS + HTGLOS * PROB(IP)   
             APCTKL = APCTKL + PCKILL * PROB(IP)   
             ATKILL = ATKILL + TKILL * PROB(IP)    
          ENDIF

          IF (HTG(IP) .LT. 0.0) HTG(IP) = 0.0

C
C         IF A SALVAGE CUT IS TO BE PERFORMED (ITMSLV = 1) AND
C         DEFOLIATION IS ABOVE THE MINIMUM SPECIFIED (T >= TMDEFL)
C         THEN SET KUTKOD TO THE SALVAGE CODE (SALKOD)
C

          IF (ITMSLV .EQ. 1 .AND. T .GE. TMDEFL) THEN
             KUTKOD(IP) = SALKOD
          ENDIF

C
C       End of DO loop to allocate damage to trees.
C
  380   CONTINUE

        IF (ITMREP .GT. 1) THEN

           K = IZ6(I)

  390      CONTINUE
C
C          Accumulate weighted averages for species and host summaries
C          (1) DF  (2) GF  (3) All Hosts
C
           WDBH(K) = WDBH(K) + ADBH
           WHTG(K) = WHTG(K) + AHTG    
           WHT(K) = WHT(K) + AHT 
           WDG(K) = WDG(K) + ADG 
           WCR(K) = WCR(K) + ACR 
           WCBASE(K) = WCBASE(K) + ACBASE    
           WDGLOS(K) = WDGLOS(K) + ADGLOS    
           WHTLOS(K) = WHTLOS(K) + AHTLOS    
           WPCTKL(K) = WPCTKL(K) + APCTKL    
           WTKILL(K) = WTKILL(K) + ATKILL    
           WRECS(K) = WRECS(K) + NRECS 
           WZ4(K) = WZ4(K) + Z4(I)     
           WZ2(K) = WZ2(K) + Z2(I) * Z4(I)   
           WZ3(K) = WZ3(K) + Z3(I) * Z4(I)   
           WEGGS(K) = WEGGS(K) + EGGS(I) * Z4(I)   
           WBDEF(K) = WBDEF(K) + DPMAX * Z4(I)     
           WTDEF(K) = WTDEF(K) + T * Z4(I)   
           WPMORT(K) = WPMORT(K) + PRMORT * Z4(I)  

           IF (K .LT. 3) THEN
C
C             Set array index for ALL HOSTS and accumulate the values.
C
              IJUMP = 1
              K     = 3
              GOTO 390
           ELSE
              IJUMP = 0 
           ENDIF

C     
C          NOTE: Z4(I) = SUM(OVER TREE RECORDS IN CLASS I) OF PROB(IP) =   
C                        NUMBER OF T/A IN CLASS I. 
C     
           ADBH = ADBH / Z4(I)   
           AHTG = AHTG / Z4(I)   
           AHT = AHT / Z4(I)     
           ADG = ADG / Z4(I)     
           ACR = ACR / Z4(I)     
           ACBASE = ACBASE / Z4(I)     
           ADGLOS = ADGLOS / Z4(I)
           AHTLOS = AHTLOS / Z4(I)     
           APCTKL = 100.0 * APCTKL / Z4(I)   
           ATKILL = ATKILL / Z4(I)     
C   
C          All trees in class I have been assessed for DFTM damage   
C     
C          SKIP A LINE IF A NEW SPECIES IS ENCOUNTERED.  
C     
           IF (I .GT. 1 .AND. IZ6(I) .NE. IZ6(I-1)) WRITE(JODFTM,*)

  410      FORMAT (I4)
           WRITE(JODFTM,420) I,KSPP(IZ6(I)),NRECS,Z4(I),ADBH,AHT,ACR,
     >        Z2(I),Z3(I),EGGS(I),DPMAX,T,PRMORT,ADG,ADGLOS,
     >        AHTG,AHTLOS,ATKILL,APCTKL
           IF (JODFEC .GT. 0) WRITE (JODFEC,420) I,KSPP(IZ6(I)),NRECS,
     >        Z4(I),ADBH,AHT,ACR,Z2(I),Z3(I),EGGS(I),DPMAX,T,PRMORT,
     >        ADG,ADGLOS,AHTG,AHTLOS,ATKILL,APCTKL
  420      FORMAT(1X,I4,4X,A2,I7,F7.1,2F7.1,F7.2,F9.1,F8.1,F7.1,F9.2,
     >            F7.2,F7.3,2F6.2,F8.2,F7.2,2F6.1)
        ENDIF

  430 CONTINUE

      IF (ITMREP .GT. 1) THEN
C
C        Compute DF, GF and ALL HOST grand averages.
C
         WRITE (JODFTM,*)
         IF (JODFEC .GT. 0) WRITE (JODFEC,*)

         DO 450 K=1,3
            IF (K .EQ. 1 .AND. .NOT. LDF) GOTO 450
            IF (K .EQ. 2 .AND. .NOT. LGF) GOTO 450
            IF (K .EQ. 3 .AND. .NOT. (LDF .AND. LGF)) GOTO 450
            WDBH(K) = WDBH(K) / WZ4(K)
            WHTG(K) = WHTG(K) / WZ4(K)
            WHT(K) = WHT(K) / WZ4(K)    
            WDG(K) = WDG(K) / WZ4(K)    
            WCR(K) = WCR(K) / WZ4(K)    
            WCBASE(K) = WCBASE(K) / WZ4(K)    
            WDGLOS(K) = WDGLOS(K) / WZ4(K)    
            WHTLOS(K) = WHTLOS(K) / WZ4(K)    
            WPCTKL(K) = WPCTKL(K) / WZ4(K) * 100.0  
            WTKILL(K) = WTKILL(K) / WZ4(K)    
            WRECS(K) = WRECS(K) / NUMCLS(K)   
            WZ2(K) = WZ2(K) / WZ4(K)    
            WZ3(K) = WZ3(K) / WZ4(K)    
            WEGGS(K) = WEGGS(K) / WZ4(K)
            WBDEF(K) = WBDEF(K) / WZ4(K)
            WTDEF(K) = WTDEF(K) / WZ4(K)
            WPMORT(K) = WPMORT(K) / WZ4(K)    
            WZ4(K) = WZ4(K) / NUMCLS(K) 

            WRITE (JODFTM,440) KSPP(K), WRECS(K), WZ4(K), WDBH(K),
     &         WHT(K), WCR(K), WZ2(K), WZ3(K), WEGGS(K), WBDEF(K),
     &         WTDEF(K), WPMORT(K), WDG(K), WDGLOS(K), WHTG(K),
     &         WHTLOS(K), WTKILL(K), WPCTKL(K)
            IF (JODFEC .GT. 0) WRITE (JODFEC,440) KSPP(K), WRECS(K),
     &         WZ4(K), WDBH(K), WHT(K), WCR(K), WZ2(K), WZ3(K),
     &         WEGGS(K), WBDEF(K), WTDEF(K), WPMORT(K), WDG(K),
     &         WDGLOS(K), WHTG(K), WHTLOS(K), WTKILL(K), WPCTKL(K)
  440       FORMAT(' AVERAGE ',A4,F5.1,F7.1,2F7.1,F7.2,F9.1,F8.1,
     >            F7.1,F9.2,F7.2,F7.3,2F6.2,F8.2,F7.2,2F6.1)

  450    CONTINUE
         IF (JODFEC .GT. 0) WRITE (JODFEC,*)
      ENDIF
C
C     WRITE OUTPUT
C
      IF (LPUNCH) WRITE (JOTMDK,460)
  460 FORMAT (' END OF PUNCH OUTPUT')

      IF (TMDTRE) THEN
         WRITE (JODFTM,470)
  470    FORMAT ('0DUMP OF INDIVIDUAL TREE RECORDS:'//,'   IPT IMC '
     >      ,'SPECIES',5X,'WK2',8X,'PROB',11X,'HT',10X,'HTG',10X,'DBH',
     >       11X,'DG',4X,'ICR',4X,'NORMHT',4X,'ITRUNC KUTKOD',//)

         DO 490 II=1, ITRN
            I = IPT(II)
            WRITE (JODFTM,480) I,IMC(I),ISP(I),WK2(I),PROB(I),HT(I),
     >          HTG(I),DBH(I),DG(I),ICR(I),NORMHT(I),ITRUNC(I),KUTKOD(I)
  480       FORMAT (1X,I5,2I4,6F13.3,I4,2I10,I3)
  490    CONTINUE
      ENDIF

      RETURN
      END   
