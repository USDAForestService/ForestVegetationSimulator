      SUBROUTINE FMCFMD (IYR, FMD)
      IMPLICIT NONE
C----------
C  **FMCFMD FIRE-LS-DATE OF LAST REVISION:  02/28/08
C----------
C  SINGLE-STAND VERSION
C  CALLED FROM: FMBURN
C  PURPOSE:
C     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
C     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
C     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
C     THE CLOSEST FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE
C     BY THE DYNAMIC FUEL MODEL OPTION
C----------
C  CALL LIST DEFINITIONS:
C     FMD:     FUEL MODEL NUMBER
C
C  COMMON BLOCK VARIABLES AND PARAMETERS:
C     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
C     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C      
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'    
C
C
COMMONS
C----------
C     LOCAL VARIABLE DECLARATIONS
C----------
      INTEGER ICLSS
      PARAMETER(ICLSS = 22)
C
C
C     COVER METAGROUP ENUMS
C
C     JPCT  =  1  ! JACK PINE COVER TYPE
C     NHCT  =  2  ! NORTHERN HARDWOOD
C     RPCT  =  3  ! RED AND WHITE PINE
C     MWCT  =  4  ! MIXED WOOD
C     OACT  =  5  ! OAK
C     POCT  =  6  ! PINE OAK
C     ABCT  =  7  ! ASPEN BIRCH !! NOTE THAT SUBSEQUENT FOR-LOOPS USE THIS U-BOUND
C
      INTEGER JPCT, NHCT, RPCT, MWCT, OACT, POCT, ABCT
      PARAMETER(
     &  JPCT  =  1,
     &  NHCT  =  2,
     &  RPCT  =  3,
     &  MWCT  =  4,
     &  OACT  =  5,
     &  POCT  =  6,
     &  ABCT  =  7)

      INTEGER  IYR,FMD,ICT
      INTEGER  IPTR(ICLSS), ITYP(ICLSS)
      INTEGER  J,I,K,L, FINDMOD
      REAL     CAREA, HEMCRA, CWIDTH, HEMCOV, CONCOV, OVERCOV, OVERCRA
      REAL     CONCRA, BIRBA, ASPBA, RPBA, WPBA, PINECRA, PINECOV
      REAL     FMAVH, HARDBA, HPOINT(MAXTRE), BAMOST
      REAL     XPTS(ICLSS,2),EQWT(ICLSS), AFWT, BFWPCNT
      REAL     X, Y, CTBA(ABCT),STNDBA, BFCOUNT, SMCOUNT, CONCOUNT
      REAL     MAPBASBA, OAKBA, ASBIBA, HEMBA, LT3
      LOGICAL  DEBUG, LBFUNDER, LSMTREES, LCNUNDER, LBFWPUND, LDRY
C----------
C  THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
C  CLASS. THEY ARE RETURNED WITH THE WEIGHT
C----------
      DATA IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,
     &           105,142,143,146,161,162,164,186,189 /
C----------
C  THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
C  VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
C  AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
C  SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
C  SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.
C----------
      DATA ITYP / ICLSS * 0 /
C----------
C  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
C  WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
C  COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
C  SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).
C----------
      DATA ((XPTS(I,J), J=1,2), I=1,ICLSS) /
     &   5., 15.,   ! FMD   1
     &   5., 15.,   ! FMD   2
     &   5., 15.,   ! FMD   3
     &   5., 15.,   ! FMD   4
     &   5., 15.,   ! FMD   5
     &   5., 15.,   ! FMD   6
     &   5., 15.,   ! FMD   7
     &   5., 15.,   ! FMD   8
     &   5., 15.,   ! FMD   9
     &  15., 30.,   ! FMD  10 
     &  15., 30.,   ! FMD  11
     &  30., 60.,   ! FMD  12  
     &  45.,100.,   ! FMD  13
     &   5., 15.,   ! FMD 105
     &   5., 15.,   ! FMD 142       
     &   5., 15.,   ! FMD 143    
     &   5., 15.,   ! FMD 146   
     &   5., 15.,   ! FMD 161   
     &   5., 15.,   ! FMD 162   
     &   5., 15.,   ! FMD 164   
     &   5., 15.,   ! FMD 186   
     &   5., 15./   ! FMD 189                                          
    
C----------
C  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE
C  COLINEAR, AND COLINEARITY WEIGHTS ARE ZERO. IF TWO CANDIDATE
C  MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
C  MUST SUM TO 1, WRT EACH OTHER
C----------
      DO I = 1,ICLSS
        EQWT(I)  = 0.0
      ENDDO
C----------
C  BEGIN ROUTINE
C----------
      CALL DBCHK (DEBUG,'FMCFMD',6,ICYC)
C
      IF (DEBUG) WRITE(JOSTND,1) ICYC,IYR,LUSRFM
    1 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)
C----------
C  IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.
C----------
      IF (LUSRFM) RETURN
C
      IF (DEBUG) WRITE(JOSTND,6) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,
     >           SMALL,LARGE
    6 FORMAT(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,
     >       ' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,
     >       ' SMALL=',F7.2,' LARGE=',F7.2)
C----------   
C  LOW FUEL MODEL SELECTION
C----------
      FMAVH = ATAVH
      IF (DEBUG) WRITE(JOSTND,8) FMAVH
    8 FORMAT(' FMCFMD FMAVH= ',F4.0)
C----------
C  DETERMINE IF THERE IS A BALSAM FIR UNDERSTORY (ATLEAST 500 BF PER
C  ACRE 1 - 3").  DETERMINE IF THERE ARE MANY SMALL TREES (ATLEAST 500
C  TPA 0 - 2").  DETERMINE IF THERE IS A BALSAM FIR / WHITE PINE
C  UNDERSTORY (ATLEAST 500 BF OR WP 1 - 3").  DETERMINE IF THERE IS A
C  CONIFER UNDERSTORY (ATLEAST 500 CONIFERS 1 - 3").
C----------
      LBFUNDER = .FALSE.
      LSMTREES = .FALSE.
      LCNUNDER = .FALSE.
      LBFWPUND = .FALSE.
      BFCOUNT = 0
      SMCOUNT = 0
      CONCOUNT = 0
      BFWPCNT = 0
      DO I = 1,ITRN
        IF ((ISP(I) .EQ. 8) .AND. (DBH(I) .GE. 1) .AND. 
     &     (DBH(I) .LE. 3)) BFCOUNT = BFCOUNT + FMPROB(I)
        IF (DBH(I) .LE. 2) SMCOUNT = SMCOUNT + FMPROB(I)
        IF ((ISP(I) .LE. 14) .AND. (DBH(I) .GE. 1) .AND. 
     &     (DBH(I) .LE. 3)) CONCOUNT = CONCOUNT + FMPROB(I) 
        IF ((ISP(I) .EQ. 8) .OR. (ISP(I) .EQ. 5)) THEN
          IF ((DBH(I) .GE. 1) .AND. (DBH(I) .LE. 3)) THEN
          	BFWPCNT = BFWPCNT + FMPROB(I)      	
          ENDIF
        ENDIF   
      ENDDO
      IF (BFCOUNT .GE. 500) LBFUNDER = .TRUE.
      IF (SMCOUNT .GE. 500) LSMTREES = .TRUE.
      IF (CONCOUNT .GE. 500) LCNUNDER = .TRUE.
      IF (BFWPCNT .GE. 500) LBFWPUND = .TRUE.
C----------
C  CHECK TO SEE IF THERE IS A DROUGHT
C----------
      LDRY = .FALSE.
      IF (IYR .GE. IDRYB .AND. IYR .LE. IDRYE) LDRY = .TRUE. 
      
      IF (DEBUG) WRITE (JOSTND,9) LDRY,LBFUNDER,LSMTREES
    9 FORMAT (' FMCFMD, LDRY=',L2,' LBFUNDER=',L2,' LSMTREES=',L2)
C----------
C  FIND THE TONS/ACRE IN DEAD SURFACE FUEL 0 - 3"
C----------
        LT3 = 0.0
        DO I = 1,2
          DO K = 1,2
            DO L = 1,4
              DO J = 1,3
                LT3 = LT3 + CWD(I,J,K,L)
              ENDDO
            ENDDO
          ENDDO
        ENDDO 
C----------
C  FIND THE BASAL AREA IN EACH COVER TYPE
C----------
      DO I = 1,ABCT  ! ZERO OUT THE COVER SUPER-GROUPS
        CTBA(I) = 0.
      ENDDO
      
      STNDBA = 0.0
      
      DO I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        SELECT CASE (ISP(I))
          CASE (1)       ! JACK PINE
            CTBA(JPCT) = CTBA(JPCT) + X
            STNDBA = STNDBA + X
          CASE (12,18,19,25,26,27,28)  ! NORTHERN HARDWOOD
            CTBA(NHCT) = CTBA(NHCT) + X
            STNDBA = STNDBA + X
          CASE (3,4,5)   ! RED AND WHITE PINE
            CTBA(RPCT) = CTBA(RPCT) + X
            STNDBA = STNDBA + X
          CASE (24,40,41,43)  ! MIXED WOOD AND ASPEN BIRCH
            CTBA(MWCT) = CTBA(MWCT) + X
            CTBA(ABCT) = CTBA(ABCT) + X 
            STNDBA = STNDBA + X           
          CASE (6,7,8,9)  ! MIXED WOOD HAS LARGE SPRUCE AND FIR
                          ! ASPEN BIRCH HAS SMALL SPRUCE AND FIR
            IF (DBH(I) .GE. 5) THEN
              CTBA(MWCT) = CTBA(MWCT) + X
            ELSE
              CTBA(ABCT) = CTBA(ABCT) + X 
            ENDIF
            STNDBA = STNDBA + X
          CASE (30:33,35,36)  ! OAK
            CTBA(OACT) = CTBA(OACT) + X    
            STNDBA = STNDBA + X                   
          CASE (34)  ! RED OAK GOES IN BOTH OAK AND NORTHERN HARDWOODS
            CTBA(NHCT) = CTBA(NHCT) + X           
            CTBA(OACT) = CTBA(OACT) + X    
            STNDBA = STNDBA + X         
        END SELECT
      ENDDO

C----------
C  FIND COVER TYPE METAGROUP *ICT* - FIRST LOOK FOR GROUP WITH THE MOST
C  BASAL AREA; IF OAK AND PINE COMBINED ARE HIGHER, USE OAK OR PINE
C  COVER TYPES.  IF THERE ARE NO TREES USED THE METAGROUP OF
C  THE PREVIOUS CYCLE, INITIALIZED TO *RPCT* IN **FMVINIT**
C----------
      ICT = 0
      IF ((ITRN .GT. 0) .AND. (STNDBA.GT.0.001)) THEN
     	  BAMOST = 0
     	  DO I=1,ABCT
          IF (CTBA(I) .GT. BAMOST) THEN
             BAMOST = CTBA(I)      
             ICT = I
          ENDIF 
        ENDDO
        CTBA(POCT) = CTBA(OACT) + CTBA(RPCT)
        IF (CTBA(POCT) .GT. BAMOST) THEN
     	    ICT = OACT
     	    IF (CTBA(RPCT) .GT. CTBA(OACT)) ICT = RPCT
        ENDIF
      ELSE
        ICT = OLDICT
      ENDIF
      OLDICT = ICT
      
      SELECT CASE (ICT)
      CASE (JPCT) !  jack pine
        IF (PERCOV .LE. 70) THEN      
        	IF (FMAVH .LE. 25) THEN
        	  EQWT(4) = 1.0
        	ELSE ! stand height gt 25
        	  IF (LBFUNDER) THEN ! balsam fir understory
        	  	IF (BURNSEAS .LE. 2) THEN ! before greenup
        	  		EQWT(10) = 1.0
        	  	ELSE ! after greenup
        	      EQWT(FINDMOD(162,IPTR,ICLSS)) = 1.0
        	    ENDIF
        	  ELSEIF ((ITYPE .EQ. 6) .OR. (ITYPE .EQ. 7)) THEN !grass understory, FDc12 or FDc23      	    
        	    EQWT(2) = 1.0
        	  ELSE ! must be deciduous shrubs
        	  	IF (BURNSEAS .LE. 2) THEN ! before greenup
        	  		EQWT(10) = 1.0
        	  	ELSE ! after greenup
        	      EQWT(FINDMOD(161,IPTR,ICLSS)) = 1.0
        	    ENDIF        	    
        	  ENDIF
        	ENDIF       	
        ELSE ! percov gt 70%
         	IF (FMAVH .LE. 15) THEN
         		EQWT(4) = 1.0
         	ELSE !stand height gt 15
         	  IF (FWIND .LE. 4) THEN
         	    EQWT(8) = 1.0
         	  ELSE
         	    EQWT(10) = 1.0
         	  ENDIF
         	ENDIF	       	
        ENDIF		
        	
      CASE (NHCT) ! northern hardwoods
        STNDBA = 0.0
        MAPBASBA = 0.0
        OAKBA = 0.0
        ASBIBA = 0.0
        HEMBA = 0.0
        HEMCRA = 0.0
        
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542    
          SELECT CASE (ISP(I))
            CASE (18,19,25,26,27) ! maple and basswood
              MAPBASBA = MAPBASBA + X
             STNDBA = STNDBA + X
            CASE (30:36) ! oak
              OAKBA = OAKBA + X
              STNDBA = STNDBA + X 
            CASE (24,40,41,43) ! aspen and birch
              ASBIBA = ASBIBA + X
              STNDBA = STNDBA + X 
            CASE (12)  ! hemlock
              HEMBA = HEMBA + X
              STNDBA = STNDBA + X                             
              CWIDTH=CRWDTH(I)
              CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
              HEMCRA = HEMCRA + CAREA*FMPROB(I)   
            CASE (28)  ! beech
              STNDBA = STNDBA + X                          
          END SELECT
        ENDDO          
        HEMCOV = 1.0 - EXP(-HEMCRA/43560.)
        HEMCOV = HEMCOV * 100.0     
        IF ((HEMCOV .GE. 30) .OR. (ASBIBA .GT. 0)) THEN
          EQWT(8) = 1.0
        ELSEIF (STNDBA .GT. 0) THEN
        	IF ((MAPBASBA/STNDBA) .GE. 0.5) THEN
        	  EQWT(FINDMOD(186,IPTR,ICLSS)) = 1.0
          ELSE
        	  IF (BURNSEAS .EQ. 4) THEN ! FALL
        	    EQWT(9) = 1.0
        	  ELSE
        	    EQWT(8) = 1.0
        	  ENDIF
        	ENDIF
        ELSE
        	EQWT(8) = 1.0	
        ENDIF    

      CASE (RPCT) ! red and white pine (two slight difference for white pine)
        STNDBA = 0.0
        RPBA = 0.0
        WPBA = 0.0
        PINECRA = 0.0
        HARDBA = 0.0
        
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542    
          STNDBA = STNDBA + X
          IF ((ISP(I) .GE. 3) .AND. (ISP(I) .LE. 5)) THEN
              CWIDTH=CRWDTH(I)
              CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
              PINECRA = PINECRA + CAREA*FMPROB(I)
          ENDIF              	
          SELECT CASE (ISP(I))
            CASE (3,4) ! red pine
              RPBA = RPBA + X
            CASE (5)  ! white pine
              WPBA = WPBA + X                                     
            CASE (15:68) ! hardwoods
              HARDBA = HARDBA + X
          END SELECT
        ENDDO
        PINECOV = 1.0 - EXP(-PINECRA/43560.)
        PINECOV = PINECOV * 100.0   

        IF (LT3 .GE. 5) THEN
        	EQWT(10) = 1.0
        ELSEIF (PERCOV .LE. 0) THEN
        	EQWT(8) = 1.0
        ELSEIF (((PINECOV/PERCOV) .LT. 0.5) .AND. (HARDBA .GT. 0)) THEN
        	EQWT(8) = 1.0
        ELSEIF ((ITYPE .EQ. 4) .OR. (ITYPE .EQ. 10)) THEN 
          ! FDn33 or FDc34, hazel underbrush is present       
          IF (LDRY) EQWT(FINDMOD(146,IPTR,ICLSS)) = 1.0
          IF (.NOT. LDRY) EQWT(FINDMOD(143,IPTR,ICLSS)) = 1.0
        ELSEIF ((LBFUNDER) .OR. (LBFWPUND)) THEN
          IF (FWIND .LE. 4) THEN
          	EQWT(10) = 1.0
          ELSE
            EQWT(FINDMOD(146,IPTR,ICLSS)) = 1.0
          ENDIF
        ELSEIF (PERCOV .GE. 50) THEN
          EQWT(9) = 1.0
        ELSEIF ((PERCOV .LE. 30) .AND. (RPBA .GT. WPBA)) THEN
          EQWT(2) = 1.0 ! this grass type only occurs with young red pine
                        ! plantations or red pine savannas
        ELSE
        	EQWT(9) = 1.0 ! not sure in this case
        ENDIF

      CASE (MWCT) ! mixed wood
        STNDBA = 0.0
        BIRBA = 0.0
        CONCRA = 0.0
        OVERCRA = 0.0
        
        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542    
          IF (DBH(I) .GE. 5) THEN
              CWIDTH=CRWDTH(I)
              CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
              OVERCRA = OVERCRA + CAREA*FMPROB(I)
          ENDIF              	
          SELECT CASE (ISP(I))
            CASE (24,43) ! birch
              BIRBA = BIRBA + X
              STNDBA = STNDBA + X
            CASE (1:14)  ! conifers
              STNDBA = STNDBA + X                             
              IF (DBH(I) .GE. 5) THEN              
                CWIDTH=CRWDTH(I)
                CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
                CONCRA = CONCRA + CAREA*FMPROB(I)  
              ENDIF  
            CASE (40:41)  ! aspen    
              STNDBA = STNDBA + X           
          END SELECT
        ENDDO    
        OVERCOV = 1.0 - EXP(-OVERCRA/43560.)
        OVERCOV = OVERCOV * 100.0
        CONCOV = 1.0 - EXP(-CONCRA/43560.)
        CONCOV = CONCOV * 100.0
        IF (STNDBA .LE. 0) THEN
        	EQWT(8) = 1.0
        ELSEIF ((BIRBA/STNDBA) .GE. 0.50) THEN
          EQWT(9) = 1.0
        ELSEIF (OVERCOV .GT. 0) THEN
        	IF ((CONCOV/OVERCOV) .GE. 0.30) THEN
        		EQWT(10) = 1.0
          ELSE
            EQWT(8) = 1.0        	
          ENDIF
        ELSE
          EQWT(8) = 1.0
        ENDIF
        
      CASE (OACT) ! oak
        STNDBA = 0.0
        OAKBA = 0.0
        OVERCRA = 0.0

        DO I = 1,ITRN
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542 
          STNDBA = STNDBA + X
          IF (DBH(I) .GE. 5) THEN
              CWIDTH=CRWDTH(I)
              CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
              OVERCRA = OVERCRA + CAREA*FMPROB(I)
          ENDIF              	
          IF ((ISP(I) .EQ. 30) .OR. (ISP(I) .EQ. 35)) THEN
             ! white oak and black oak
              OAKBA = OAKBA + X       
          ENDIF
        ENDDO  
        OVERCOV = 1.0 - EXP(-OVERCRA/43560.)
        OVERCOV = OVERCOV * 100.0
      
        IF (OVERCOV .GE. 45) THEN
        	IF (BURNSEAS .EQ. 1) THEN ! early spring
        	  IF (MOIS(1,1) .LT. 0.08) THEN
        	  	EQWT(FINDMOD(186,IPTR,ICLSS)) = 1.0
        	  ELSE
        	  	EQWT(8) = 1.0
        	  ENDIF
        	ELSE ! not early spring
        	  IF (STNDBA .LE. 0) THEN
        	  	EQWT(9) = 1.0
        	  ELSEIF ((OAKBA/STNDBA) .GE. 0.30) THEN
        	    EQWT(FINDMOD(189,IPTR,ICLSS)) = 1.0
        	  ELSE
        	  	EQWT(9) = 1.0
        	  ENDIF
        	ENDIF	
        ELSEIF (OVERCOV .GE. 15) THEN
        	IF (.NOT. LSMTREES) EQWT(2) = 1.0
        	IF (LSMTREES) EQWT(FINDMOD(142,IPTR,ICLSS)) = 1.0        	
        ELSE
        	IF (.NOT. LSMTREES) EQWT(FINDMOD(105,IPTR,ICLSS)) = 1.0
        	IF (LSMTREES) EQWT(FINDMOD(142,IPTR,ICLSS)) = 1.0
        ENDIF

      CASE (ABCT) ! aspen birch
        ASPBA = 0.0
        BIRBA = 0.0

        DO I = 1,ITRN              	
          X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542 
          IF ((ISP(I) .EQ. 40) .OR. (ISP(I) .EQ. 41)) ASPBA = ASPBA + X
          IF ((ISP(I) .EQ. 24) .OR. (ISP(I) .EQ. 43)) BIRBA = BIRBA + X 
        ENDDO
        
        IF (LT3 .GE. 5) THEN
          EQWT(10) = 1.0
        ELSE
        	IF (.NOT. LCNUNDER) THEN
        	  IF (BIRBA .GT. ASPBA) THEN
        	  	EQWT(9) = 1.0
        	  ELSE
        	  	EQWT(8) = 1.0
        	  ENDIF
        	ELSE
        	  IF (FWIND .GT. 4) THEN
        	  	EQWT(FINDMOD(164,IPTR,ICLSS)) = 1.0 
        	  ELSE
         	    IF (BIRBA .GT. ASPBA) THEN
        	  	  EQWT(9) = 1.0
        	    ELSE
        	  	  EQWT(8) = 1.0
        	    ENDIF       	    
        	  ENDIF	
        	ENDIF
        ENDIF
      
      END SELECT
C----------
C  END OF DETAILED LOW FUEL MODEL SELECTION
C----------
      SELECT CASE (ICT)
      CASE (JPCT, MWCT, OACT)
C----------
C        DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
C        ACTIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
C        CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
C        RELATIONSHIP DECLINES FROM PURE 11 INITIALLY, TO PURE 10 AFTER
C        THE PERIOD EXPIRES.
C----------
        AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
        IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
          LATFUEL = .TRUE.
          EQWT(11)  = AFWT
          IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
        ENDIF
        IF (.NOT. LATFUEL) AFWT = 0.0
C----------
C        MODELS 10,12, AND 13 ARE CANDIDATE MODELS FOR NATURAL FUELS
C----------
        EQWT(10) = 1.0 - AFWT
        EQWT(12) = 1.0
        EQWT(13) = 1.0      
        ! oak never gets 12 or 13
        IF (ICT .EQ. OACT) THEN
          EQWT(12) = 0.0
          EQWT(13) = 0.0
        ENDIF
     
      CASE (NHCT) ! northern hardwoods never get fm 10, 12, or 13, and 
                  ! only gets fm 11 if there is activity fuel
        AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
        IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
          LATFUEL = .TRUE.
          IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
        ENDIF
        
        IF (LATFUEL) EQWT(11) = 1.0 
        IF (.NOT. LATFUEL) EQWT(11) = 0.0 
        
        EQWT(10) = 0.0         
        EQWT(12) = 0.0
        EQWT(13) = 0.0  
               
      CASE (RPCT) ! red and white pine 
        ! with natural fuels, fm 10,12,13 possible
        ! with activity fuels, fm 12 used if red slash and fm 11 used if older slash
        AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
        IF (SLCHNG .GE. SLCRIT .OR. LATFUEL) THEN
          LATFUEL = .TRUE.
          IF ((IYR - HARVYR) .LE. 2) THEN
            EQWT(12) = 1.0
            EQWT(11) = 0.0 
            EQWT(10) = 0.0    
            EQWT(13) = 0.0     
          ELSEIF (((IYR - HARVYR) .GT. 2) 
     &            .AND. ((IYR - HARVYR) .LE. 5)) THEN          	
            EQWT(11) = 1.0
            EQWT(12) = 0.0 
            EQWT(10) = 0.0 
            EQWT(13) = 0.0                        
          ELSE
            EQWT(10) = 1.0
            EQWT(11) = 0.0
            EQWT(12) = 1.0 
            EQWT(13) = 1.0          
          ENDIF         
          IF (AFWT .LE. 0.0) LATFUEL = .FALSE.
        ELSE
          EQWT(10) = 1.0
          EQWT(11) = 0.0
          EQWT(12) = 1.0
          EQWT(13) = 1.0
        ENDIF

      CASE (ABCT) ! aspen birch never gets fm 11, 12, or 13
        EQWT(10) = 1.0 
        EQWT(11) = 0.0 
        EQWT(12) = 0.0
        EQWT(13) = 0.0      
      END SELECT
C----------
C  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
C  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)
C----------
      CALL FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)
C
      IF (DEBUG) WRITE (JOSTND,10) FMD,LDYNFM,ICT,ITYPE,FWIND
   10 FORMAT (' FMCFMD, FMD=',I4,' LDYNFM=',L2,' ICT=',I4,' ITYPE='
     &        ,I4,' FWIND=',F4.1)
C
      RETURN
      END

C     "LOCAL SUBROUTINE"
C     FIND FUEL MODEL NUMBER (FOR LABELLING PURPOSES USUALLY,
C     GIVEN THE INDEX POSITION IN THE IPTR VECTOR. THIS NEED
C     ARISES WHEN FUEL MODEL LABELS DO NOT MATCH THEIR INDEX
C     E.G. MODELS 25 & 26, WHICH ARE IN POSITIONS 14 & 15 IN
C     THIS VARIANT. 8 IS RETURNED IF THE MODEL LABEL IS NOT FOUND,

      INTEGER FUNCTION FINDMOD(JMOD,IPTR,ICLSS)

      INTEGER JMOD, K, IPTR
      DIMENSION IPTR(ICLSS)

      DO K=1,ICLSS
        IF (JMOD .EQ. IPTR(K)) THEN
          GOTO 11
        ENDIF
      ENDDO
      K = 8
   11 CONTINUE
      FINDMOD = K

      RETURN
      END