!== last modified  05-16-2013
      SUBROUTINE GROSSVOL(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     >     DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     >     UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,VOL,LOGVOL,LOGDIA,
     >     LOGLEN,TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,
     >     SPFLG,CONSPEC,PROD,HTTFLL,live,BA,SI,CTYPE,ERRFLAG)
C
C--   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
C--     PROFILE - EXTERNAL 
C--     DVE     - EXTERNAL
C--     R4VOL  - EXTERNAL 
C--     R6VOL  - EXTERNAL 
C--     BLMVOL - EXTERNAL
C--     R8VOL  - EXTERNAL
C--     R10VOL - EXTERNAL
C--     R12VOL - EXTERNAL 
C******DECLARE VARIABLES

      CHARACTER*1 HTTYPE,LIVE,CTYPE
      CHARACTER*2 FORST,PROD
      CHARACTER*3 MDL
      character*4 conspec
      CHARACTER*10 VOLEQ

C     MERCH VARIABLES 
      INTEGER REGN,HTTFLL,BA,SI
      REAL STUMP,MTOPP,MTOPS
      INTEGER CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
C     TREE VARIABLES
      REAL HTTOT,HT1PRD,HT2PRD,THT1
      REAL DBHOB,DRCOB,DBTBH,BTR
      INTEGER FCLASS,HTLOG
C     3RD POINT VARIABLES
      REAL UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER HTREF,I,J
C     OUTPUTS
      REAL VOL(15),LOGVOL(7,20),NOLOGP,NOLOGS
      REAL LOGDIA(21,3),LOGLEN(20),BOLHT(21)
      INTEGER TLOGS

C     TLOGS =  0
      MDL = VOLEQ(4:6)
! When total height is entered, the height type has to be feet. (2013/05/16)
      IF(HTTOT.GT.0) HTTYPE = 'F'              
      
      IF(MDL.EQ.'FW2' .OR. MDL.EQ.'fw2' .OR. MDL.EQ.'FW3' .OR. 
     >   MDL.EQ.'fw3' .OR. MDL.EQ.'CZ2' .OR. MDL.EQ.'cz2' .OR.
     >   MDL.EQ.'CZ3' .OR. MDL.EQ.'cz3' .OR. MDL.EQ.'WO2' .OR.      
     >   MDL.EQ.'wo2' .OR. MDL.EQ.'F32' .OR. MDL.EQ.'f32' .OR.
     >   MDL.EQ.'F33' .OR. MDL.EQ.'f33' .OR. MDL.EQ.'JB2' .OR.
     >   MDL.EQ.'jb2') THEN
C***********************
C      INGY MODELS     *
C      REGION 2 MODELS *
C      REGION 5 MODELS * 
C***********************

        CALL PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,
     >       HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,
     >       AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,TLOGS,
     >       NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,
     >       CTYPE,FCLASS,PROD,ERRFLAG)
      
      ELSEIF (MDL.EQ.'MAT' .OR. MDL.EQ.'mat') THEN
C***********************
C      REGION 4 MODEL  * 
C***********************

        CALL R4VOL(REGN,VOLEQ,MTOPP,HTTOT,DBHOB,HT1PRD,VOL,NOLOGP,
     >               NOLOGS,LOGDIA,LOGLEN,LOGVOL,BOLHT,
     >             CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG)
          TLOGS = ANINT(NOLOGP + NOLOGS)
      ELSEIF (MDL.EQ.'TRF' .OR. MDL.EQ.'trf')THEN
C********************************
C      PNW terif VOLUME EQUATION
C*******************************
        CALL PNWTARIF(VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,
     >                    ERRFLAG)

      ELSEIF (VOLEQ(1:1).EQ.'6') THEN
C*******************************
C     REGION 6 VOLUME ROUTINES *
C*******************************
          tht1 = 0.0
          if(httot.gt.0) then
              tht1 = httot
          elseif(HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') then
              tht1 = ht1prd
          else  
              tht1 = ht1prd
          endif

          nologp=0.0
          nologs=0.0
c          do 66 i=1,20
c            lggrd(i)=' '
c   66     continue

c ***   If not top dib is specified then
c ***   use the one from the vol equation

          IF((MDL.EQ.'BEH' .OR. MDL.EQ.'beh').AND.
     *                            (bfpflg.eq.1 .or. cupflg.eq.1)) then
             IF(MTOPP.EQ.0) MTOPP = 6.0
             CALL R6VOL(VOLEQ,FORST,DBHOB,BTR,FCLASS,MTOPP,THT1,
     *            HTTYPE,VOL,LOGVOL,NOLOGP,LOGDIA,LOGLEN,DBTBH,HT1PRD,
     *            CTYPE,ERRFLAG)
             TLOGS = ANINT(NOLOGP)
          ELSEIF((MDL.EQ.'DVE'.OR.MDL.EQ.'dve') .AND. BFPFLG.EQ.1) THEN
             CALL R6VOL2(VOLEQ,DBHOB,HTTOT,VOL,ERRFLAG)
          ELSE
            ERRFLAG = 1
          ENDIF


      ELSEIF(VOLEQ(1:1).EQ.'B' .or. voleq(1:1).eq.'b') THEN
C**************************
C     BLM VOLUME ROUTINES *
C**************************
          IF(fclass.eq.0) THEN
               ERRFLAG = 2
          ELSE
               call BLMVOL(VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,
     *             VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,
     *             BFPFLG,CUPFLG,ERRFLAG)

          ENDIF

      ELSEIF (VOLEQ(1:1).EQ.'8') THEN
C*********************
C      REGION 8 MODEL  * 
C*********************
      
          CALL R8VOL (VOLEQ,DBHOB,HTTOT,UPSHT1,HT1PRD,MTOPP,PROD,VOL,
     >                FORST,SI,BA,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG)

      ELSEIF (MDL.EQ.'DEM' .OR. MDL.EQ.'dem' .OR. 
     >        MDL.EQ.'CUR' .OR. MDL.EQ.'cur' .OR.
     >        MDL.EQ.'BRU' .OR. MDL.EQ.'bru') THEN
C************************
C      REGION 10 MODEL  * 
C************************
         IF (VOLEQ(1:3).EQ.'A01'.OR.VOLEQ(1:3).EQ.'A02' .or.
     >        VOLEQ(1:3).EQ.'a01'.OR.VOLEQ(1:3).EQ.'a02' ) THEN
      
            CALL R10VOL(VOLEQ,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,
     >           HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,
     >           BFPFLG,CUPFLG,SPFLG,ERRFLAG)
         ELSE
    
            CALL PROFILE(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     >         HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     >         UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,
     >         LOGVOL,VOL,TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,
     >         CDPFLG,SPFLG,DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)

         ENDIF

      ELSEIF(MDL.EQ.'SN2') THEN
C*********************
C      HAWAII MODEL  * 
C*********************
        if(FCLASS.eq.0) then
             ERRFLAG = 2
        else
          call R12VOL(VOLEQ,MTOPP,HT1PRD,DBHOB,HTTOT,VOL,NOLOGP,
     *                NOLOGS,FCLASS,CUTFLG,BFPFLG,CUPFLG,ERRFLAG)
          TLOGS = ANINT(NOLOGP + NOLOGS)
        endif

      ELSEIF (MDL.EQ.'DVE' .OR. MDL.EQ.'dve') THEN
C*********************
C    DVE MODELS FOR  * 
C      REGION 1      *
C      REGION 2      *
C      REGION 3      *
C      REGION 5      *
C      REGION 9      *
C      ARMY BASE     * 
C*********************
      
         CALL DVEST (VOLEQ,DBHOB,DRCOB,HTTOT,MTOPP,FCLASS,HTLOG,HT1PRD,
     >        HT2PRD,FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,
     >        PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,SI,CTYPE,ERRFLAG,MTOPS)
      
      ELSE 
C          ERROR MESSAGE
         ERRFLAG = 1
         DO 5 I=1,15
            VOL(I)=0.0
 5       CONTINUE
         DO 10 I=1,21
            BOLHT(I)=0.0
 10      CONTINUE
         DO 15 I=1,7
            DO 16 J=1,20
               LOGVOL(I,J)=0.0
 16         CONTINUE
 15      CONTINUE
         DO 17,I=1,3
            DO 18,J=1,21
               LOGDIA(J,I)=0.0
 18         CONTINUE
 17      CONTINUE
         DO 19, I=1,20
            LOGLEN(I) = 0.0
 19      CONTINUE 
      ENDIF
      
 4000 RETURN
      END
C********************************************************************
C********************************************************************
C--  VARIABLES OF INTEREST ARE:
C--******************************************************************
C--  Following variables have been be passed to the model *******
C--            optional variables are identified
c
c--  MERCHANDIZING VARIABLES
C***************************
c--  REGION - INTEGER - Region number used to set Regional Merchandizing Rules
C--  COR - CHARACTER - Flag to indicate Scribner table or Scribner
C--                 factor volumes. "Y" = table volumes, "N" = factor volumes
C--  EVOD - INTEGER - allow even or odd segment lengths
C--         segment options 11-14 allow odd lengths by definition
C--        1 = odd segment lengths allowed
C--        2 = only even segment lengths will be allowed
C--  MAXLEN - REAL - Maximum segment length
C--  MINLEN - REAL - Minimum segment length
C--  MERCHL - REAL - Minimum length of primary product a tree must have
C--                  must have be merchantable
C--  **TOP DIB TO USE**
C--  MTOPP - REAL - BdFt, CuFt and Cord Wood merch top for primary product
C--  MTOPS - REAL - CuFt and Cord Wood merch top for secondary product
C
C--  STUMP - REAL - height of stump in feet or fractions thereof.
C
C--  INTEGERS - Indicating which volumes are desired
C--    BFPFLG - Board foot primary product
C--    CUPFLG - Cubic foot primary product
C--    CDPFLG - Cord wood primary product
C--    SPFLG - Secondary product

C****** End of Merchandising parameters.
C
C    TREE CHARACTERISTICS
C************************
C--  VOLEQ - INTEGER - The volume equation number for this tree
C--  LIVE - CHAR -  Live/Dead code.  L=Live tree, D=Dead tree
c--  PROD - CHAR - Product code.  01 = sawtimber, 03 = pulp tree
C
C--  DBHOB - REAL - Diameter Breast Height Outside Bark
c--  DRCOB - REAL - Diameter Root Collar Outside Bark
c
c--  HTTYPE - CHAR - Tree height measured in feet (F) or logs (L)
C--  HTTOT - REAL - Tree height from the ground to the tip
C--  LOGHT - REAL - Tree height in number of logs to MTOPP
c--  HT1PRD - REAL - Tree height in feet to MTOPP
c--  HT2PRD - REAL - Tree height in feet to MTOPS
c
C--  Note measurements of bark and form are optional entries
C--  DBTBH - REAL - Double bark thickness at breast height
C--  FCLASS - INTEGER - Girard form class
C--  BTR - INTEGER - Bark thickness ratio
C--  UPSHT1 - REAL - The height of the first upper stem form measurement
C--  UPSHT2 - REAL - The height of the second upper stem form measurement
C--  UPSD1 - REAL - The measured diameter outisde bark at point UPSHT1.
C--  UPSD2 - REAL - The measured diameter outisde bark at point UPSHT2 .
c--  HTREF - REAL - The percent reference height for AVGZ1
C--  AVGZ1 - REAL - An average Z correction factor to use for point UPSHT1.
C--  AVGZ2 - REAL - An average Z correction factor to use for point UPSHT2.
C
C--  PCTDF - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
C--          IN THE MAIN STEM PORTION OF A TREE - THIS DEFECT %
C--          WILL BE APPLIED TO BOTH BDFT AND CUFT PRODUCTS.
C--  PCTDF2 - REAL - THE INTEGER VALUE REPRESENTING THE % OF DEFECT
C--          IN THE TOPWOOD PORTION OF A TREE - THIS DEFECT %
C--          WILL BE APPLIED TO CUFT PRODUCTS.
c
C--  RETURNED VARIABLES
C--********************
C--  Following variables will be returned to the calling routine ******
C--             when requested
C--  VOL - REAL
C--    VOL(1) - Total volume for the tree in cubic feet, ground to tip.
C--              Does not include the limbs and roots.
C--    VOL(2) - Gross amount of board foot as primary product - Scribner.
C--    VOL(3) - Net amount of board foot as primary product - Scribner.
C--    VOL(4) - Gross amount of cubic foot as primary product - Smalian.
C--    VOL(5) - Net amount of cubic foot as primary product - Smalian.
C--    VOL(6) - Amount of cord wood as primary product.
C--    VOL(7) - Gross amount of cubic foot as secondary product - Smalian.
C--    VOL(8) - Net amount of cubic foot as secondary product - Smalian.
C--    VOL(9) - Amount of cord wood as secondary product.
C--    VOL(10) - Gross amount of board foot as primary product - INT'L 1/4"
C--    VOL(11) - Net amount of board foot as primary product - INT'L 1/4"
C--
C--  LOGLEN - REAL(20) - Log segment lengths computed by
C--                      subroutine "SEGMENT".  Does not include trim.
C--  LOGVOL(7,20) - REAL - The product volume of a log
C--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
C
C--  LOGDIA - REAL(21,3) - Log end diameters 
C       LOGDIA(x,1) = scaling dib
C       LOGDIA(x,2) = actual dib 
c       LOGDIA(x,3) = actual dob
c
C--  BOLHT - REAL(21) - point on the bole where diameters were estimated
C--  *** Note LOGDIB and BOLHT are dimensioned 1 larger then the number of
C--     logs.  This is to hold the values for the small end of the top log.
c
C--  NOLOGP - integer - Average number of 16 ft logs, primary product.
C--  NOLOGS - integer - Average number of 16 ft logs, secondary product.
c
C--******************************************************************
c
C--  VARIABLES OF INTEREST USED WITHIN THIS ROUTINE ARE:
c
C--  LENMS - REAL - Length of primary product if a secondary product
C--                 is desired.
C--  LMERCH - REAL - Total merchantable length of stem returned
C--             from subroutine "MERLEN" and the merchantable
C--             length of stem with trim and unusable top
C--             removed returned from subroutine "SEGMNT"
C--  NUMSEG - INTEGER - The computed number of segments from 
C--           subroutine "NUMSEG"
C--
C**************************************************************
C**************************************************************


