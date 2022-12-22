!== last modified  12-12-2002
C YW 04/13/2017 Added 260, 240 and 090 to be valid species code for R1ALLENB
C               This is to make species code match with R1ALLENC.
      SUBROUTINE R1ALLENC(VOLEQ,DBHOB,HTTOT,TOPD,BTR,CUVOL,TCVOL,CFSTMP,
     >                    ERRFLAG)
C----------
C  **CFVOL--BS  DATE OF LAST REVISION:  04/20/95
C----------
C  DIMENSION STATEMENT FOR INTERNAL ARRAYS.
C----------
      CHARACTER*10 VOLEQ
      REAL BRATIO(11),BTR,D2H,BARK,VMAX,DBHMIN,HTRUNC,PHT,CFSTMP
      REAL DBHOB,HTTOT,CUVOL, TCVOL,TOTVOL,BHAT,AHAT,VOLT,BHRE,VOLM
      REAL DTRUNC,VOLTK,STUMP,TOPD,HTMRCH,S3,DMRCH
      INTEGER ISPC,ITHT,ERRFLAG

      LOGICAL TKILL,LCONE

C----------
C---- BARK RATIO LOOK-UP TABLE
C-----
      DATA BRATIO /0.964, 0.851, 0.867, 0.915, 0.934, 0.950,
     >             0.969, 0.956, 0.937, 0.890, 0.934/
C
C  MAIN LOGIC
C
      TKILL = .FALSE.
      ITHT = 0
      ERRFLAG = 0

      IF(VOLEQ(8:10).EQ.'119')THEN
          ISPC = 1
      ELSEIF(VOLEQ(8:10).EQ.'073')THEN
          ISPC = 2
      ELSEIF(VOLEQ(8:10).EQ.'202')THEN
          ISPC = 3
      ELSEIF(VOLEQ(8:10).EQ.'017')THEN
          ISPC = 4
      ELSEIF(VOLEQ(8:10).EQ.'263'.or.VOLEQ(8:10).EQ.'260')THEN
          ISPC = 5
      ELSEIF(VOLEQ(8:10).EQ.'242'.or.VOLEQ(8:10).EQ.'240')THEN
          ISPC = 6
      ELSEIF(VOLEQ(8:10).EQ.'108')THEN
          ISPC = 7
      ELSEIF(VOLEQ(8:10).EQ.'093'.or.VOLEQ(8:10).EQ.'090')THEN
          ISPC = 8
      ELSEIF(VOLEQ(8:10).EQ.'019')THEN
          ISPC = 9
      ELSEIF(VOLEQ(8:10).EQ.'122')THEN
          ISPC = 10
      ELSEIF(VOLEQ(8:10).EQ.'999')THEN
          ISPC = 11
      ELSEIF(VOLEQ(8:10).EQ.'375')THEN
          ISPC = 12
      ELSEIF(VOLEQ(8:10).EQ.'740')THEN
          ISPC = 13
      ELSE
        ERRFLAG = 1
         CUVOL = 0.0
         TCVOL = 0.0
         RETURN
      ENDIF

      
      D2H = DBHOB*DBHOB*HTTOT
C PAPER BIRCH (NORTH CENTRAL STATION EQUATION 7/2001)
      if(ISPC.eq.12)THEN
         IF(DBHOB.LT.5.0)THEN
            CUVOL = 0.0
         ELSEIF(DBHOB.GE.5.0 .AND. DBHOB.LT.11.0)THEN
            CUVOL = 0.988264+0.002732*D2H
         ELSEIF(DBHOB.GE.11.0)THEN
            CUVOL = 2.512836+0.002446*D2H
         ENDIF
         TCVOL = CUVOL
C COTTONWOOD EDMINSTER RESEARCH NOTE RM-351  7/2001)
      elseif(ISPC.eq.13)THEN
            CUVOL = 0.00142526*(D2H**1.0636)
            TCVOL = CUVOL
C ALL OTHER SPECIES
      ELSE

         DBHMIN = 6.0
         if(btr.gt.0.0) then
            bark = btr
         else
            BARK = BRATIO(ISPC)
         endif
      
         VMAX = TOTVOL(ISPC,DBHOB,HTTOT,D2H)
C---------
C  IF TOP IS DEAD OR MISSING AND VOLUME IS GREATER THAN ZERO,
C  ADJUST VOLUME ESTIMATE FOR TOPKILL; OTHERWISE RETURN.
C----------
         IF(VMAX.LT.0.0) THEN
            CUVOL = 0.0
            TCVOL = 0.0
            RETURN
         ELSE
C----------
C  INITIALIZE TOTAL CUBIC FOOT VOLUME ESTIMATE TO VMAX.
C----------
            TCVOL = VMAX
C----------
C  THE BEHRE HYPERBOLA TAPER MODEL IS USED TO COMPUTE VOLUME TO POINT 
C  OF TOPKILL OR TO A MINIMUM TOP DIAMETER.  BEHRE HYPERBOLA PARAMETERS 
C  ARE COMPUTED HERE.  BHAT IS INITIALIZED WITH VALUE OF CYLINDRICAL 
C  FORM FACTOR.   
C----------
            BHAT = VMAX / (.00545415*DBHOB*DBHOB*BARK*BARK*HTTOT)
            IF(BHAT.GT.0.95) BHAT = 0.95
C----------
C        AHAT = AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'A'
C----------
            AHAT = 0.44277 - 0.99167/BHAT - 1.43237*ALOG(BHAT) +
     >         1.68581*SQRT(BHAT) - 0.13611*BHAT*BHAT
            LCONE=.FALSE.
            IF(ABS(AHAT) .LT. 0.05) THEN
               LCONE=.TRUE.
               IF(AHAT.LT. 0.) THEN
                  AHAT = -0.05
               ELSE
                  AHAT = 0.05
               ENDIF
            ENDIF
            BHAT = 1.-AHAT
            IF (BHAT .LT. 0.0001) BHAT=0.0001
            VOLT=bhre(0.0,1.0,AHAT,BHAT)
C----------
C       COMPUTE TOTAL CUBIC VOLUME LOSS DUE TO TOPKILL.
C----------
            IF(TKILL) THEN 
C----------
C         A TRUNCATED TREE HAS BEEN DETECTED.  HEIGHT AT POINT OF TRUNCATION 
C         (HTRUNC) IS COMPUTED FROM ITRUNC.  HEIGHT AND DIAMETER AT THE 
C         POINT OF TRUNCATION ARE THEN EXPRESSED AS RATIOS OF "NORMAL" HEIGHT 
C         AND DBH (PHT AND DTRUNC, RESPECTIVELY). 
C------- ---
               HTRUNC = ITHT / 100.0
               PHT = 1. - (HTRUNC/HTTOT)
               DTRUNC = PHT / (AHAT*PHT + BHAT)
C----------
C         CORRECT FOR THE TRUNCATED TOP IN THE VN ESTIMATE USING THE  
C         RATIO OF VOLUME BELOW TRUNCATION TO TOTAL VOLUME.
C----------
               IF(.NOT.LCONE) THEN
                  VOLTK = bhre(PHT,1.0,AHAT,BHAT)
C              VN = TCVOL*VOLTK/VOLT
C----------
C         PROCESS CONES.
C----------
               ELSE
C              VN = VMAX*(1.0-PHT**3)
               ENDIF
            ENDIF
         ENDIF
C----------
C  COMPUTE MERCHANTABLE CUBIC VOLUME USING TOP DIAMETER, MINIMUM 
C  DBH, AND STUMP HEIGHT SPECIFIED BY THE USER.  CORRECT FOR TOPKILL.
C----------

         STUMP=1.-CFSTMP/HTTOT

         IF(DBHOB.LT.DBHMIN .OR. DBHOB.LT.TOPD) THEN
             CUVOL = 0.0
         ELSE
            DMRCH = TOPD/DBHOB
            HTMRCH = ((BHAT*DMRCH)/(1.0-(AHAT*DMRCH)))
            IF(.NOT.LCONE) THEN
               VOLM = bhre(HTMRCH,STUMP,AHAT,BHAT)
               CUVOL = VMAX*VOLM/VOLT
C           WRITE(7,*)VOLM,VMAX,VOLT,HTMRCH,CUVOL,AHAT,BHAT
               IF(TKILL.AND.DTRUNC.GT.DMRCH) THEN
C----------
C           CORRECT MERCHANTABLE VOLUME FOR TOP KILL.
C----------
                  VOLTK = bhre(PHT,STUMP,AHAT,BHAT)
C             VM = VMAX*VOLTK/VOLT
               ENDIF
            ELSE
C----------
C         PROCESS CONES.
C----------
               S3 = STUMP**3
               VOLM = S3-HTMRCH**3
               CUVOL = VMAX*VOLM
C           WRITE(*,*)'PROCESS CONES',CUVOL
               IF(TKILL.AND.DTRUNC.GT.DMRCH) THEN
C----------
C           CORRECT MERCHANTABLE VOLUME FOR TOP KILL (CONES ONLY).
C----------
                  VOLTK=S3-PHT**3
                  CUVOL = CUVOL*VOLTK/VOLM
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END

C**********************************************************************
C**********************************************************************
      SUBROUTINE R1ALLENB (VOLEQ,DBHOB,HTTOT,TOPD,BTR,BFVOL,BFSTMP,
     >                    ERRFLAG)
C**********************************************************************
C----------
C  **BFVOL--BS   DATE OF LAST REVISION:   07/10/95
C----------
C
C  ************** BOARD FOOT MERCHANTABILITY SPECIFICATIONS ********
C
C  BFVOL CALCULATES BOARD FOOT VOLUME OF ANY TREE LARGER THAN A MINIMUM 
C  DBH SPECIFIED BY THE USER.  MINIMUM DBH CAN VARY BY SPECIES, 
C  BUT CANNOT BE LESS THAN 2 INCHES.  DEFAULTS ARE 6 IN. FOR LODGEPOLE 
C  PINE AND 7 IN. FOR ALL OTHER SPECIES.  MINIMUM MERCHANTABLE DBH IS 
C  SET WITH THE BFVOLUME KEYWORD.  FOR METHB = 1, 3, OR 4, MERCHANTABLE
C  TOP DIAMETER CAN BE SET TO ANY VALUE BETWEEN 2 IN. AND MINIMUM DBH. 
C  MINIMUM DBH AND TOP DIAMETER ARE ASSUMED TO BE MEASURED OUTSIDE 
C  BARK--IF DIB IS DESIRED, ALLOW FOR DOUBLE BARK THICKNESS IN
C  SPECIFICATIONS.
C
C  VOLUME IS BE COMPUTED BY FORMULA.  THE VOLUME FORMULA IS BASED ON 
C  KEMP'S EQUATIONS, BUT ALL PARAMETERS CAN BE REPLACED BY THE USER WITH 
C  THE BFVOLEQ KEYWORD.  KEMP'S EQUATIONS IMPLY SPECIFIC MERCHANTABILITY 
C  STANDARDS (9" MINIMUM DBH, 8" MINIMUM TOP DIAMETER, AND 1' STUMP).  
C  WHEN METHB=1, EQUATIONS DEVELOPED BY  ALLEN, ADAMS, AND PRAUSA (1974: 
C  IDAHO FORESTRY, WILDLIFE, AND RANGE EXPERIMENT STATION NOTE # 21) 
C  ARE APPLIED TO COMPUTE VOLUMES FOR THE PORTION OF THE TREE BETWEEN 
C  AN 8" TOP DIAMETER AND THE USER SPECIFIED MERCHANTABILITY LIMIT. THE
C  VALUE OF D2H FOR WHICH ALLEN'S AND KEMP'S EQUATIONS ARE EQUAL IS 
C  CALLED D2HBRK, AND VARIES BY SPECIES.  FOR TREES WITH DBH < D2HBRK,
C  ALLEN'S EQUATIONS ARE USED; FOR LARGER TREES, KEMP'S EQUATIONS ARE
C  USED.  THE TRANSITION IS SMOOTH AND TREES DO NOT LOSE VOLUME AS THEY
C  INCREASE IN SIZE.  FOR METHB=2, ONLY THE KEMP EQUATIONS ARE USED,
C  AND VOLUME ABOVE THE 8" TOP IS IGNORED.
C
C  ASSUMED SPECIES CORRESPONDENCE IS AS FOLLOWS:
C
C                   ACTUAL SPECIES     ALLEN'S EQUATIONS
C              -------------------  ----------------------
C         1     WESTERN WHITE PINE   LODGEPOLE PINE
C         2     WESTERN LARCH        WESTERN LARCH
C         3     DOUGLAS-FIR          DOUGLAS-FIR
C         4     GRAND FIR            GRAND FIR
C         5     WESTERN HEMLOCK      GRAND FIR
C         6     WESTERN REDCEDAR     WESTERN LARCH
C         7     LODGEPOLE PINE       LODGEPOLE PINE
C         8     ENGELMANN SPRUCE     GRAND FIR
C         9     SUBALPINE FIR        GRAND FIR
C        10     PONDEROSA PINE       DOUGLAS-FIR
C        11     MOUNTAIN HEMLOCK     GRAND FIR
C
C  VOLUME LOSS DUE TO TOP DAMAGE (TKILL=.TRUE.) IS ESTIMATED WITH A 
C  BEHRE HYPERBOLA TAPER MODEL, WITH PARAMETERS ESTIMATED FROM TOTAL 
C  CUBIC FOOT VOLUME, HEIGHT AND DIAMETER.
C
C----------
C  COFBVS -- COEFFICIENT IN BOARD FOOT VOLUME EQUATION FOR TREES
C            HAVING D2H LESS THAN D2HBRK(ISPC)
C  D2HBRK -- THE VALUE OF D2H BELOW WHICH ALLEN'S EQUATIONS
C            ARE USED TO COMPUTE BOARD FOOT VOLUME.
C  HDRATM -- ESTIMATED HEIGHT/DIAMETER RATIO THAT IS INDEXED BY
C            EITHER DBH OR TOP DIAMETER.
C  HDRATA -- ACTUAL HEIGHT/DIAMETER RATIO FOR THE SUBJECT TREE.
C----------
      CHARACTER*10 VOLEQ
      INTEGER ISPC, ITHT,ERRFLAG,ID,ITD,IVTD

      REAL DBHOB,HTTOT, TOPD, BFVOL,btr,D2H,BARK,TSIZE,VT,VOLM
      REAL COFBVS(11),HDRATM(100),D2HBRK(11),BRATIO(11),STUMP,BFSTMP
      REAL BFVEQS(7,11),BFVEQL(7,11),IBTRAN(11), BTRAN(11),BHRE,HTMRCH
      REAL HDRATA,DTOPK,VMAX,TOTVOL,BHAT,AHAT,HTRUNC,PHT,DTRUNC,VOLTK

      LOGICAL LCONE,TKILL

      DATA D2HBRK/
     >  16917.1, 19231.8, 15763.5, 10016.7, 14883.9, 29922.0,
     >   4567.8,  6024.9, 19425.9, 14034.0, 14883.9/

      DATA HDRATM/
     >  10.0, 10.0, 9.0, 8.0, 7.8, 7.65, 7.5, 7.25, 7.0, 6.75,
     >   6.5, 6.25, 18*6.0, 10*5.5, 10*5.0, 10*4.5, 10*4.0,
     >  10*3.5, 10*3.0, 10*2.5/

      DATA COFBVS/
     > 0.01031,0.008423,0.008423,0.009523,0.009523,0.008421,
     > 0.01031,0.009523,0.009523,0.008423,0.009523/

C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN 
C  THE TRANSITION SIZE
C----------
      DATA BFVEQS/
     >  -26.729,      0.0,     0.0, 0.01189,     0.0,     0.0,    0.0,
     >  -29.790,      0.0,     0.0, 0.00997,     0.0,     0.0,    0.0,
     >  -25.332,      0.0,     0.0, 0.01003,     0.0,     0.0,    0.0,
     >  -34.127,      0.0,     0.0, 0.01293,     0.0,     0.0,    0.0,
     >  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0,
     >  -10.742,      0.0,     0.0, 0.00878,     0.0,     0.0,    0.0,
     >   -8.085,      0.0,     0.0, 0.01208,     0.0,     0.0,    0.0,
     >  -11.851,      0.0,     0.0, 0.01149,     0.0,     0.0,    0.0,
     >  -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     >  -50.340,      0.0,     0.0, 0.01201,     0.0,     0.0,    0.0,
     >  -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0/
C----------
C  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN 
C  THE TRANSITION SIZE
C----------
      DATA BFVEQL/
     >  -32.516,      0.0,     0.0, 0.01181,     0.0,     0.0,    0.0,
     >   85.150,      0.0,     0.0, 0.00841,     0.0,     0.0,    0.0,
     >   -9.522,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0,
     >   10.603,      0.0,     0.0, 0.01218,     0.0,     0.0,    0.0,
     >  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0,
     >   -4.064,      0.0,     0.0, 0.00799,     0.0,     0.0,    0.0,
     >   14.111,      0.0,     0.0, 0.01103,     0.0,     0.0,    0.0,
     >    1.620,      0.0,     0.0, 0.01158,     0.0,     0.0,    0.0,
     >  124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0,
     > -298.784,      0.0,     0.0, 0.01595,     0.0,     0.0,    0.0,
     >  -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=DBHOB, 1=D2H
C----------
      DATA IBTRAN/0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C----------
C  TRANSITION SIZE.  TREES OF LARGER SIZE (DBHOB OR D2H) WILL USE COEFFICIENTS 
C  FOR LARGER SIZE TREES.
C---------- 
      DATA BTRAN/
     >     20.5,     20.5,    20.5,    20.5,    20.5,    20.5,   20.5,
     >     20.5,     20.5,    20.5,    20.5/
C------
C---- BARK RATIO LOOK-UP TABLE
C-----
      DATA BRATIO /0.964, 0.851, 0.867, 0.915, 0.934, 0.950,
     >             0.969, 0.956, 0.937, 0.890, 0.934/


C----------
C  INITIALIZE VOLUME ESTIMATE.
C----------
      ITHT = 0
      TKILL = .FALSE.
      BFVOL=0.0
      ERRFLAG = 0

      IF(VOLEQ(8:10).EQ.'119')THEN
          ISPC = 1
      ELSEIF(VOLEQ(8:10).EQ.'073')THEN
          ISPC = 2
      ELSEIF(VOLEQ(8:10).EQ.'202')THEN
          ISPC = 3
      ELSEIF(VOLEQ(8:10).EQ.'017')THEN
          ISPC = 4
      ELSEIF(VOLEQ(8:10).EQ.'263'.OR.VOLEQ(8:10).EQ.'260')THEN
          ISPC = 5
      ELSEIF(VOLEQ(8:10).EQ.'242'.OR.VOLEQ(8:10).EQ.'240')THEN
          ISPC = 6
      ELSEIF(VOLEQ(8:10).EQ.'108')THEN
          ISPC = 7
      ELSEIF(VOLEQ(8:10).EQ.'093'.OR.VOLEQ(8:10).EQ.'090')THEN
          ISPC = 8
      ELSEIF(VOLEQ(8:10).EQ.'019')THEN
          ISPC = 9
      ELSEIF(VOLEQ(8:10).EQ.'122')THEN
          ISPC = 10
      ELSEIF(VOLEQ(8:10).EQ.'999')THEN
          ISPC = 11
      ELSEIF(VOLEQ(8:10).EQ.'375')THEN
         BFVOL = 0.0
         RETURN
      ELSEIF(VOLEQ(8:10).EQ.'740')THEN
         BFVOL = 0.0
         RETURN
      ELSE
        ERRFLAG = 1
         BFVOL = 0.0
         RETURN
      ENDIF

      D2H = DBHOB*DBHOB*HTTOT
      IF(BTR.GT.0.0) THEN
        BARK = BTR
      ELSE
        BARK = BRATIO(ISPC)
      ENDIF
C----------
C  ASSIGN TRANSITION SIZE.
C----------
      TSIZE=DBHOB
      IF(IBTRAN(ISPC).GT.0) TSIZE=D2H
C----------
C  SET TOP VOLUME (VT) TO 0.  BYPASS ALLEN EQUATIONS FOR TOP = 8.
C---------- 
      VT=0.0
      IF(TOPD.EQ.8.0) GO TO 15
      HDRATA=HTTOT/DBHOB
      ID = DBHOB - 0.5

      IF(ID.GT.100) ID = 100
      IF(TOPD.LT.1.0) TOPD = 6.0   !ADDED TO AVOID ITD = 0
      ITD = TOPD - 0.5
      IF(ITD.GT.100) ITD = 100

      IF(D2H.GT.D2HBRK(ISPC)) GO TO 10
C----------
C  D2H LESS THAN D2H BREAKPOINT: COMPUTE TOP AND STEM VOLUME WITH 
C  ALLEN-ADAMS-PRAUSA EQUATIONS.
C----------
      VT = -COFBVS(ISPC)*(TOPD**3*HDRATM(ITD)-80.0) *
     >  SQRT(HDRATA/HDRATM(ID)) - (TOPD**2-4.0)*0.12153

      BFVOL = COFBVS(ISPC)*D2H + VT
      GO TO 30
C----------
C  D2H GREATER THAN D2H BREAKPOINT:  COMPUTE TOP VOLUME WITH ALLEN-
C  ADAMS-PRAUSA EQUATIONS AND STEM VOLUME WITH KEMP EQUATIONS.
C----------
   10 DTOPK = 0.4*DBHOB
      IF(DTOPK.LT.4.0) DTOPK = 4.0
      IF(DTOPK.GT.8.0) DTOPK = 8.0
      IVTD = DTOPK - 0.5
      VT = COFBVS(ISPC) * SQRT(HDRATA/HDRATM(ID)) * 
     >     (DTOPK**3*HDRATM(IVTD) - TOPD**3*HDRATM(ITD)) -
     >     (DTOPK**2-TOPD**2)*0.12153

   15 CONTINUE
      IF (TSIZE.GE.BTRAN(ISPC)) GO TO 20

      BFVOL = VT + BFVEQS(1,ISPC) + BFVEQS(2,ISPC)*DBHOB +            
     >        BFVEQS(3,ISPC)*DBHOB*HTTOT + BFVEQS(4,ISPC)*D2H +
     >        BFVEQS(5,ISPC)*DBHOB**BFVEQS(6,ISPC)*HTTOT**BFVEQS(7,ISPC)
      GO TO 30
   20 CONTINUE
      BFVOL = VT + BFVEQL(1,ISPC) + BFVEQL(2,ISPC)*DBHOB +
     >        BFVEQL(3,ISPC)*DBHOB*HTTOT + BFVEQL(4,ISPC)*D2H +
     >        BFVEQL(5,ISPC)*DBHOB**BFVEQL(6,ISPC)*HTTOT**BFVEQL(7,ISPC)
C----------
C  THE FOLLOWING PIECE OF FOOLISHNESS (J.E.B.) IS REQUIRED
C  BY NATIONAL FOREST SCALING AND CRUISING RULES.  BE CAREFUL IF
C  YOU BUY A CORRAL POLE SALE BASED ON A SCRIBNER CRUISE VOLUME.
C----------
   30 CONTINUE
      IF(BFVOL.LT.10.0) BFVOL = 10.0
C----------
C  BRANCH TO STMT 600 TO COMPUTE VOLUME LOSS DUE TO TOPKILL.
C----------
C----------
C  THE BEHRE HYPERBOLA TAPER MODEL IS USED TO COMPUTE VOLUME TO POINT 
C  OF TOPKILL.  BEHRE HYPERBOLA PARAMETERS ARE COMPUTED HERE.  BHAT IS 
C  INITIALIZED WITH VALUE OF CYLINDRICAL FORM FACTOR BASED ON TOTAL 
C  CUBIC FOOT VOLUME (PASSED IN FROM VOLS).   
C----------
      IF(TKILL) THEN
         VMAX = TOTVOL(ISPC,DBHOB,HTTOT,D2H)
         BHAT = VMAX / (.00545415*DBHOB*DBHOB*BARK*BARK*HTTOT)
         IF(BHAT.GT.0.95) BHAT = 0.95
C----------
C        AHAT = AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'A'
C----------
         AHAT = .44277 - .99167/BHAT - 1.43237*ALOG(BHAT)
     >        + 1.68581*SQRT(BHAT) - .13611*BHAT*BHAT
         LCONE=.FALSE.
         IF(ABS(AHAT) .LT. 0.05) THEN
            LCONE=.TRUE.
            IF(AHAT.LT. 0.) THEN
               AHAT = -0.05
            ELSE
               AHAT = 0.05
            ENDIF
         ENDIF
         BHAT = 1.-AHAT
         IF (BHAT .LT. 0.0001) BHAT=0.0001
C----------
C       COMPUTE TOTAL BOARD FOOT VOLUME LOSS DUE TO TOPKILL.
C       HEIGHT AND DIAMETER AT THE POINT OF TRUNCATION ARE EXPRESSED 
C       AS RATIOS OF "NORMAL" HEIGHT AND DBH (PHT AND DTRUNC, 
C       RESPECTIVELY). 
C----------
         HTRUNC = ITHT / 100.0
         PHT = 1. - (HTRUNC/HTTOT)
         DTRUNC = PHT / (AHAT*PHT + BHAT)
C----------
C       IF THE TOP IS DAMAGED AND THE DIAMETER AT THE POINT OF 
C       TRUNCATION IS GREATER THAN THE MERCHANTABLE TOP DIAMETER,
C       CORRECT BOARD FOOT VOLUME ESTIMATE. 
C----------
         IF(DTRUNC .GT. TOPD/DBHOB) THEN
            VOLTK = bhre(PHT,1.0,AHAT,BHAT)
C----------
C         HTMRCH = RATIO OF MERCH. HT (HEIGHT WHERE DBHOB=TOPD) TO TOTAL 
C         HEIGHT.
C----------
            HTMRCH = ((BHAT*TOPD)/DBHOB)/(1. - (AHAT*TOPD/DBHOB))
C----------
C         ADJUST THE STUMP HEIGHT.
C----------
            STUMP=1.0-BFSTMP/HTTOT
C----------
C         CORRECT FOR THE TRUNCATED TOP IN THE BFV ESTIMATE
C----------
            IF (LCONE) THEN
               VOLM = STUMP**3-HTMRCH**3.
               VOLTK = STUMP**3. - PHT**3.
               BFVOL = BFVOL*VOLTK/VOLM
            ELSE
               BFVOL = (BFVOL*VOLTK) / bhre(HTMRCH,STUMP,AHAT,BHAT)
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END

C***********************************************************
C***********************************************************
      FUNCTION bhre(L1,L2,AHAT,BHAT)
C***********************************************************
C  **BEHRE  DATE OF LAST REVISION:  01/16/91
C----------
C
C  THIS FUNCTION CALCULATES THE VOLUME OF A SOLID OF REVOLUTION
C  DESCRIBED BY A BEHRE TAPER CURVE USING PARAMETERS 'AHAT' AND 'BHAT'.
C  THE LIMITS OF INTEGRATION ARE 'L1' AND 'L2'.
C
C  IT SHOULD BE NOTED THAT THE VOLUME CALCULATED IS OFF BY A FACTOR
C  OF PI/(A CUBED) - THIS VALUE WOULD CANCEL OUT IN THE RATIOS OF
C  BEHRE VOLUMES USED IN CALLING ROUTINE 'VOLS'.
C
C
      REAL L1, L2,BHRE,ALB1,ALB2,BHAT,AHAT
C
      ALB1 = AHAT*L1 + BHAT
      ALB2 = AHAT*L2 + BHAT
      bhre = ALB2 - ALB1 - 2.0*BHAT*(ALOG(ALB2) - ALOG(ALB1))
     &        - BHAT*BHAT/ALB2 + BHAT*BHAT/ALB1
      RETURN
      END



C***********************************************************
C***********************************************************
      FUNCTION TOTVOL(ISPC,DBHOB,HTTOT,D2H)
C***********************************************************
      
      REAL CFVEQS(7,11), CFVEQL(7,11),TOTVOL
      REAL ICTRAN(11), CTRAN(11)
      REAL DBHOB, HTTOT, TSIZE, D2H, TERM1
      INTEGER ISPC
      
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN 
C  THE TRANSITION SIZE
C----------
      DATA CFVEQS/
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     > 0.030288,      0.0,     0.0,0.002213,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0/
C----------
C  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN 
C  THE TRANSITION SIZE
C----------
      DATA CFVEQL/
     >      0.0,      0.0,     0.0, 0.00233,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0, 0.00184,     0.0,     0.0,    0.0,
     >      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0, 0.00234,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0, 0.00205,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488,
     >      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     >      0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0,
     >-1.557103,      0.0,     0.0,0.002474,     0.0,     0.0,    0.0,
     >      0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0/
C----------
C  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=DBHOB, 1=D2H
C----------
      DATA ICTRAN/0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/
C----------
C  TRANSITION SIZE.  
C---------- 
      DATA CTRAN/
     >      0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0,
     >      0.0,      0.0,  6000.0,     0.0/
C----      
C---- MAIN LOGIC
      
      TSIZE = DBHOB


      IF(ICTRAN(ISPC).GT.0) TSIZE=DBHOB*DBHOB*HTTOT
      
      IF(TSIZE.LT.CTRAN(ISPC)) THEN
        IF(CFVEQS(5,ISPC) .GT. 0) THEN
           TERM1 = CFVEQS(5,ISPC)*DBHOB**CFVEQS(6,ISPC)*
     >                                           HTTOT**CFVEQS(7,ISPC)
        ELSE 
           TERM1 = 0
        ENDIF
        TOTVOL = CFVEQS(1,ISPC) + CFVEQS(2,ISPC)*DBHOB +
     >        CFVEQS(3,ISPC)*DBHOB*HTTOT + CFVEQS(4,ISPC)*D2H + TERM1
      ELSE
        IF(CFVEQL(5,ISPC) .GT. 0) THEN
           TERM1 = CFVEQL(5,ISPC)*DBHOB**CFVEQL(6,ISPC)*
     >                                           HTTOT**CFVEQL(7,ISPC)
        ELSE 
           TERM1 = 0
        ENDIF
        TOTVOL = CFVEQL(1,ISPC) + CFVEQL(2,ISPC)*DBHOB +
     >        CFVEQL(3,ISPC)*DBHOB*HTTOT + CFVEQL(4,ISPC)*D2H + TERM1
      ENDIF

      RETURN
      END

