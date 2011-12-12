      SUBROUTINE BMSDIT
C----------
C  **BMSDIT  DATE OF LAST REVISION:   09/28/05
C
C----------
C
C     SET UP ONE STAND FOR PINE BEETLE RUN.
C
C     AN EXTENSION OF THE PARALLEL PROCESSING EXTENSION (PROGNOSIS)
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--MAY 1987
C
C     CALLED FROM: PPMAIN
C
COMMONS
C                                         
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'
      INCLUDE 'PPCNTL.F77'

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'

      INCLUDE 'MISCOM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMFCOM.F77'
      INCLUDE 'BMRRCM.F77'
      INCLUDE 'BMPCOM.F77'

      DIMENSION DWT(NSCL)

      REAL    BATEMP
      REAL    DWT
      LOGICAL LX
      INTEGER K, LCUT, IBMSTD,JYR,IACTK,NPRM,CASEID
      REAL    MAX, MAXH,PRMS
      DIMENSION PRMS(2) 
      REAL    PI24                     
      REAL    TOTBAS(MAXSP,2)
      INTEGER MYACT(4),ITODO
      DATA    MYACT/2701,2702,2703,2704/ !MAINOUT,TREEOUT,BKPOUT,VOLOUT, RESPECTIVELY
C
C     IF BMSTND IS ZERO, NO DISPERSAL WAS RUN, BRANCH TO EXIT.
C
      IF (BMSTND.LE.0) RETURN
C
C     FIND OUT IF THE CURRENT STAND IS IN THE DISPERSAL GROUP
C
      CALL OPBISR (BMSTND,BMSDIX,ISTND,IBMSTD)
C
C     IF THERE ARE NO MPB STANDS, THEN RETURN.
C
      IF ((IBMSTD .LE. 0) .OR. (.NOT.STOCK(IBMSTD))) RETURN
C
C     LOAD THE STAND INTO THE BEETLE MODEL ARRAYS. 

      DO 100 ISIZ= 1,NSCL 
        BAH(IBMSTD,ISIZ)= 0.0
        BANH(IBMSTD,ISIZ)= 0.0
        CRS(IBMSTD,ISIZ,1)= 0.0
        CRS(IBMSTD,ISIZ,2)= 0.0
        DNSTD(IBMSTD,ISIZ)= 0.0 
        DWT(ISIZ)= 0.0          
        HGS(IBMSTD,ISIZ,1)= 0.0
        HGS(IBMSTD,ISIZ,1)= 0.0     
        HTS(IBMSTD,ISIZ,1)= 0.0
        HTS(IBMSTD,ISIZ,2)= 0.0
        TREE(IBMSTD,ISIZ,1)= 0.0
        TREE(IBMSTD,ISIZ,2)= 0.0
        OTPA(IBMSTD,ISIZ,1)= 0.0
        OTPA(IBMSTD,ISIZ,2)= 0.0
        TVOL(IBMSTD,ISIZ,1)= 0.0
        TVOL(IBMSTD,ISIZ,2)= 0.0
        
        DO 101 K= 1,3
          TPBK(IBMSTD,ISIZ,1,K)= 0.0
          TPBK(IBMSTD,ISIZ,2,K)= 0.0
  101   CONTINUE
  
        SDMR(IBMSTD,ISIZ)= 0.0
        SRR(IBMSTD,ISIZ)= 0.0 
        SSR(IBMSTD,ISIZ)= 0.0 
  100 CONTINUE
  
      DO 500 ISPC= 1,MAXSP
         TOTBAS(ISPC,1) = 0.0
         TOTBAS(ISPC,2) = 0.0
  500 CONTINUE
  
      BAH(IBMSTD,NSCL+1)= 0.0
      BANH(IBMSTD,NSCL+1)= 0.0
      TREE(IBMSTD,NSCL+1,1)=0.0 !HOST.  NEW AJM 9/05
      TREE(IBMSTD,NSCL+1,2)=0.0 !NON-HOST.  NEW AJM 9/05
      
      MAX= 0.0
      MAXH= 0 .0  
                               
      PI24= PIE / (24.0 * 24.0)
      
      IF (PBSPEC .EQ. 4) THEN
        LCUT= MIN0(ISCMIN(1),ISCMIN(2))
      ELSE
        LCUT= ISCMIN(PBSPEC)
      ENDIF
          
C     STAND-LEVEL IDENTIFIERS FOR USE BY THE FIRE MODEL:

      HABTYP(IBMSTD) = KODTYP
      
C     STCCF(IBMSTD) = ATCCF

      SLP(IBMSTD) = SLOPE
      
      DO 20 ISPC=1,MAXSP
        IF (ISCT(ISPC,1).EQ.0) GOTO 20

        LX= .FALSE.
        IF ((((PBSPEC .EQ. 1) .OR. (PBSPEC .EQ. 4))
     >         .AND. (HSPEC(1,ISPC).EQ. 1)) 
     >   .OR. (((PBSPEC .EQ. 2) .OR. (PBSPEC .EQ. 4))
     >         .AND. (HSPEC(2,ISPC) .EQ. 1))
     >   .OR. ((PBSPEC .EQ. 2) .AND. (HSPEC(2,ISPC) .EQ. 1))            ! i THINK THIS LINE IS REDUNDANT AJM 9/05
     >   .OR. ((PBSPEC .EQ. 3).AND.(HSPEC(3,ISPC) .EQ. 1))) LX=.TRUE.

C Compute the basal area present in each host and non-host size class.
C The final category (NSCL+1) is for summaries.
C
C Size-class level GRF for density is computed here, since the IND(II) 
C array is already in use, and points to the individual GRF computed
C elsewhere.

        DO 10 II=ISCT(ISPC,1),ISCT(ISPC,2)
        
          I=IND1(II)
          BATEMP= DBH(I) * DBH(I) * PI24
          
          CALL BMDBHC(DBH(I), K)
          
          X = (BATEMP * PROB(I))

C     BAH, BANH are total BA within the size class.

          IF (LX) THEN
            BAH(IBMSTD,K)= BAH(IBMSTD,K) + X
            BAH(IBMSTD,NSCL+1)= BAH(IBMSTD,NSCL+1) + X

            TREE(IBMSTD,K,1)= TREE(IBMSTD,K,1) + PROB(I)    

            TREE(IBMSTD,NSCL+1,1)=TREE(IBMSTD,NSCL+1,1)+PROB(I) ! AJM 9/05

            HTS(IBMSTD,K,1)= HTS(IBMSTD,K,1) + HT(I) * PROB(I)
            CRS(IBMSTD,K,1)= CRS(IBMSTD,K,1) + ICR(I) * PROB(I)

            HGS(IBMSTD,K,1)= HGS(IBMSTD,K,1) + HTG(I) * PROB(I)
            TVOL(IBMSTD,K,1)= TVOL(IBMSTD,K,1) + CFV(I) * PROB(I) 
            
            SDMR(IBMSTD,K)= SDMR(IBMSTD,K) + IMIST(I) * X
            SRR(IBMSTD,K)= SRR(IBMSTD,K) + BMRR(I) * X
            SSR(IBMSTD,K)= SSR(IBMSTD,K) + BMSR(I) * X

c   Size class level GRFs (i.e. Rating Values) will not be calculated.
c   GRF due to stand density effects has been modified in routine
c   BMCGRF.  Modified 6/25/99, AJM.  See also lines 237, below.

c	    DNSTD(IBMSTD,K)= DNSTD(IBMSTD,K) + AMIN1(GRFDEN(I), 1.0) * X
c           DWT(K)= DWT(K) + X
            
            TOTBAS(ISPC,1) = TOTBAS(ISPC,1) + X
          ELSE                                               
                  
            BANH(IBMSTD,K)= BANH(IBMSTD,K) + X
            BANH(IBMSTD,NSCL+1)= BANH(IBMSTD,NSCL+1) + X
                    
            TREE(IBMSTD,K,2)= TREE(IBMSTD,K,2) + PROB(I)

            TREE(IBMSTD,NSCL+1,2)=TREE(IBMSTD,NSCL+1,2)+PROB(I) !AJM 9/05

            HTS(IBMSTD,K,2)= HTS(IBMSTD,K,2) + HT(I) * PROB(I)
            CRS(IBMSTD,K,2)= CRS(IBMSTD,K,2) + ICR(I) * PROB(I)
            HGS(IBMSTD,K,2)= HGS(IBMSTD,K,2) + HTG(I) * PROB(I)
            TVOL(IBMSTD,K,2)= TVOL(IBMSTD,K,2) + CFV(I) * PROB(I) 

            TOTBAS(ISPC,2) = TOTBAS(ISPC,2) + X
          ENDIF
   10   CONTINUE
   20 CONTINUE
      
      DO 25 ISPC= 1,MAXSP
        IF (ISCT(ISPC,1).EQ.0) GOTO 25
        
        IF (TOTBAS(ISPC,2) .GT. MAX) THEN
           MAX = TOTBAS(ISPC,2)
           ISPH(IBMSTD,2) = ISPC
        ENDIF
        IF (TOTBAS(ISPC,1) .GT. MAXH) THEN
           MAXH = TOTBAS(ISPC,1)
           ISPH(IBMSTD,1) = ISPC
        ENDIF
        
   25 CONTINUE

      DO 26 I= 1,2
        IF (ISPH(IBMSTD,I) .GT. 0) THEN
          IQPTYP(IBMSTD,I)= ISPFLL(ISPH(IBMSTD,I))
        ELSE  
          IQPTYP(IBMSTD,I)= 1
        ENDIF  
   26 CONTINUE

c     Record the initial TPA for each size class of each stand. This could be
c     done in the DA file rather than memory, I think.

      DO 30 ISCL= 1, NSCL
        OTPA(IBMSTD,ISCL,1)= TREE(IBMSTD,ISCL,1)
        OTPA(IBMSTD,ISCL,2)= TREE(IBMSTD,ISCL,2)
   30 CONTINUE
   
C     Transform density-dependent GRF, HTS into size-class averages. 
      
      DO 40 ISCL=1,NSCL
        IF (TREE(IBMSTD,ISCL,1) .GT. 1.0E-9) THEN 
          HTS(IBMSTD,ISCL,1) = HTS(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
          CRS(IBMSTD,ISCL,1)= CRS(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
          HGS(IBMSTD,ISCL,1) = HGS(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
          TVOL(IBMSTD,ISCL,1)= TVOL(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
        ELSE
          HTS(IBMSTD,ISCL,1) = 0.0
          CRS(IBMSTD,ISCL,1)= 0.0
          HGS(IBMSTD,ISCL,1) = 0.0
          TVOL(IBMSTD,ISCL,1)= 0.0
        ENDIF   
        
        IF (TREE(IBMSTD,ISCL,2) .GT. 1.0E-9) THEN 
          HTS(IBMSTD,ISCL,2) = HTS(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
          CRS(IBMSTD,ISCL,2)= CRS(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
          HGS(IBMSTD,ISCL,2) = HGS(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
          TVOL(IBMSTD,ISCL,2)= TVOL(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
        ELSE
          HTS(IBMSTD,ISCL,2) = 0.0
          CRS(IBMSTD,ISCL,2)= 0.0
          HGS(IBMSTD,ISCL,2) = 0.0
          TVOL(IBMSTD,ISCL,2)= 0.0
        ENDIF                        
        
        IF (DWT(ISCL) .GT. 1.0E-9) THEN
c  see note above.  The following two lines commented out 6/25/99, AJM.
c
c         DNSTD(IBMSTD,ISCL)= DNSTD(IBMSTD,ISCL) / DWT(ISCL)
          SDMR(IBMSTD,ISCL)=  SDMR(IBMSTD,ISCL) / DWT(ISCL)
          SRR(IBMSTD,ISCL)= SRR(IBMSTD,ISCL) / DWT(ISCL)
          SSR(IBMSTD,ISCL)= SSR(IBMSTD,ISCL) / DWT(ISCL)
        ELSE
c         DNSTD(IBMSTD,ISCL)= 1.0
          SDMR(IBMSTD,ISCL)=  0.0
          SRR(IBMSTD,ISCL)= 0.0
          SSR(IBMSTD,ISCL)= 0.0
        ENDIF
   40 CONTINUE
C
C     IF THIS IS THE FIRST MASTER CYCLE (MICYC=2), THEN INITIALIZE
C     ANY DAMAGE READ FROM INVENTORY (ASSUMING THE USER HAS GIVEN
C     THE RIGHT KEYWORDS.
C
      IF (MICYC .EQ. 2) THEN 
        DO 1000 ISPC = 1, MAXSP
        
          IF (ISCT(ISPC,1) .EQ. 0) GOTO 1000
          
          LX = .FALSE.
          IF ((((PBSPEC .EQ. 1) .OR. (PBSPEC .EQ. 4))
     >       .AND. (HSPEC(1,ISPC) .EQ. 1)) 
     >     .OR. (((PBSPEC .EQ. 2) .OR. (PBSPEC .EQ. 4))
     >       .AND. (HSPEC(2,ISPC) .EQ. 1))
     >     .OR. ((PBSPEC .EQ. 2) .AND. (HSPEC(2,ISPC) .EQ. 1))   !REDUNDANT?
     >     .OR. ((PBSPEC .EQ. 3) .AND. (HSPEC(3,ISPC) .EQ. 1)))
     >         LX = .TRUE.
          
          IF (.NOT. LX) GOTO 1000
          
          DO 1010 II = ISCT(ISPC,1), ISCT(ISPC,2)
            I = IND1(II)
            IF (LBMDAM(I)) THEN
              IF (PBSPEC .LT. 3) PBKILL(IBMSTD,K) = PBKILL(IBMSTD,K)
     >                                              + PROB(I)
              IF (PBSPEC .EQ. 3) ALLKLL(IBMSTD,K) = ALLKLL(IBMSTD,K)
     >                                              + PROB(I)
            ENDIF
 1010     CONTINUE
 1000   CONTINUE
      ENDIF
C
C GET THE ACTIVITY INFO PERTAINING TO OUTPUT WRITING.
C ADDED 9/1/05 AJM
C NOTE IBEG, IEND,ISTP ARE INITIALIZED IN BMSETP IN FIRST MASTER CYCLE
C
C FIND OUT IF STAND-LEVEL OUTPUT--IS REQUESTED FOR THIS STAND.
C CODE ADAPTED FROM FMDOUT AJM 8/05
C MYACT(1)=MAINOUT; MYACT(2)=TREEOUT;MYACT(3)=BKPOUT,MYACT(4)=VOLOUT
C EACH ACTIVITY AND EACH STAND GETS ITS OWN SPACE IN PARMETER ARRAYS 
C IBEG, IEND, & ISTP.

      DO 2000 I=1,4
      CALL OPFIND(1,MYACT(I),ITODO)
      IF (ITODO.GT.0) THEN
         CALL OPGET(ITODO,2,JYR,IACTK,NPRM,PRMS)
         IBEG(I,IBMSTD) = JYR             !YEAR TO BEGIN REPORTING
         IEND(I,IBMSTD) = JYR + PRMS(1)   !ENDING YEAR OF REPORTING
         ISTP(I,IBMSTD) = INT(PRMS(2))    !FREQUENCY INTERVAL TO REPORT
         IF (ISTP(I,IBMSTD).EQ.0) ISTP(I,IBMSTD)=1
         CALL OPDONE (ITODO,JYR)
      ENDIF
 2000 CONTINUE
C
C THE DBSLOGICAL FLAGGING WWPBM-DB WRITING (1=DB+STD OUTPUT; 2==>DB ONLY)

C NOTE THESE VARS ARE SET VIA DBS KEYWORDS & SUPPL RECORDS, WHICH 
C INVOKE WRITING TO DB, BUT BY THEMSELVES WON'T ENSURE THEIR WRITING.
C THE APPROPRIATE WWPBM OUTPUT REQUEST MUST BE MADE, WITH YEARS 
C SPECIFIED.  THE DBS OUTPUT REQUEST MERELY SAYS: IF WE REQUESTED THE 
C WWPBM OUTPUT (VIA WWPBM KEYWORD) THEN WRITE IT TO DB, EITHER SOLELY
C (VAR = 2), OR IN ADDITION TO (VAR=1) THE STD OUTPUT.
C VARIABLES SET IN DBSIN, WHERE THIS DBSWW IS HAS AN ENTRY.
C RETURN FROM EXDBS RETURNS ZEROS.
C
      IF(MICYC .EQ. 2) THEN
         CALL DBSWW(J1,J2,J3,J4) !ENTRY IN DBSIN AND EXDBS, 
         JBMDB(1,IBMSTD)=J1      !DBSIN RETURNS A 1 OR 2, IF CORRESPONDING 
         JBMDB(2,IBMSTD)=J2      !  DB TABLE WAS REQUESTED, ELSE -1 (AS INITIALIZED) 
         JBMDB(3,IBMSTD)=J3      !EXDBS RETURNS ZERO.
         JBMDB(4,IBMSTD)=J4
      ENDIF
C
C     TEST: CALL DBSCASE TO CREATE THIS STAND'S CASEID
C
      IF(MICYC .GE.2 .AND. 
     >  (JBMDB(1,IBMSTD).GE.1 .OR.
     >  JBMDB(2,IBMSTD).GE.1 .OR.
     >  JBMDB(3,IBMSTD).GE.1 .OR.
     >  JBMDB(4,IBMSTD).GE.1)) CALL DBSCASE(1) !CREATE A CASE ID (Note: it'll bail if its already got one.)
C
      IF (BMCASEID(IBMSTD) .LT.0) THEN
         CALL DBSWW2(CASEID)           !FETCH A CASE ID (ENTRY IN DBSCASE)
         BMCASEID(IBMSTD)=CASEID       !ASSIGN IT TO A WWPBM VARIABLE
      ENDIF
C
C*****************************************************************************
      IF (LBMDEB) WRITE (JBMBPR,310) ISTND,IBMSTD,
     >            BAH(IBMSTD,NSCL+1),BANH(IBMSTD,NSCL+1)
  310 FORMAT (' IN BMSDIT: ISTND=',I4,'; IBMSTD=',I4,'; TOT BAH=',
     >        F10.3,'; TOT BANH=',F10.4)
     
      RETURN
      END
