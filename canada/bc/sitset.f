      SUBROUTINE SITSET
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  THIS SUBROUTINE IS USED TO SET SIMULATION CONTROLLING VALUES
C  THAT HAVE NOT BEEN SET USING THE KEYWORDS --- SDIMAX, BAMAX.
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'VOLSTD.F77'
      INCLUDE 'BCPLOT.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C
      INTEGER IFIASP, ERRFLAG
      CHARACTER FORST*2,DIST*2,PROD*2,VAR*2,VOLEQ*10
      LOGICAL LBAMX
      INTEGER I,ISPC,INTFOR,IREGN,J,JJ,K,iSeries
        
C     ASSIGN VERSION 2 PARAMETERS (MS,ESSF,PP)
      CALL BECSET
      
      LBAMX = .TRUE.
      IF (BAMAX .GT. 0.) THEN
        LBAMX = .FALSE.
        GOTO 40
      ENDIF

C  SET SDIDEF AND BAMAX VALUES WHICH HAVE NOT BEEN SET BY KEYWORD.

      BAMAX = -1.

C     SOME ODD SITE SERIES CASES "01-ys, 01a, 01b" NEED SPECIAL ATTENTION
      IF (INDEX(BEC%Series,'-') .GT. 0) THEN
        iSeries = 1
      ELSEIF (INDEX(BEC%Series,'a') .GT. 0) THEN
        iSeries = 1
      ELSEIF (INDEX(BEC%Series,'b') .GT. 0) THEN
        iSeries = 1
      ELSE
        READ (BEC%Series,'(I4)') iSeries
      ENDIF
     
      SELECT CASE (BEC%Zone)

	  CASE ("IDF")                      ! IDF: dk differ in V2 - becset.for
	    SELECT CASE (BEC%SubZone)
	      CASE ("dk1")
	        SELECT CASE (iSeries)    
	          CASE (1)
	            BAMAX = 60.
                CASE (2,4:7)
	            BAMAX = 58.
                CASE (3)
	            BAMAX = 55.
	        END SELECT
	      CASE ("dk2")
	        SELECT CASE (iSeries)
	          CASE (1,3:7)
	            BAMAX = 89.
	        END SELECT
	      CASE ("dk3")
	        SELECT CASE (iSeries)
                CASE DEFAULT
	            BAMAX = 53.
	        END SELECT
	      CASE ("dm1")
	        SELECT CASE (iSeries)
	          CASE (1,3:7)
	            BAMAX = 43.
	        END SELECT
	      CASE ("dm2")
	        SELECT CASE (iSeries)
	          CASE (1,3,5:7)
	            BAMAX = 53.
                CASE (4)
	            BAMAX = 52.
	        END SELECT
            CASE ("mw1","mw2")
              BAMAX = 47.
            CASE ("xh1","xh2")
	        BAMAX = 48.
          END SELECT 
          
	  CASE ("ICH")                   ! ICH
	    SELECT CASE (BEC%SubZone)
	      CASE ("mk1")
	        SELECT CASE (iSeries)
	          CASE (1)
	            BAMAX = 59.
                CASE (2)
	            BAMAX = 50.
                CASE (3)
	            BAMAX = 46.
                CASE (4)
	            BAMAX = 42.
                CASE (5:7)
	            BAMAX = 50.
	        END SELECT
	      CASE ("mw2")
	        SELECT CASE (iSeries)
	          CASE (1:7)
	            BAMAX = 89.
	        END SELECT
	      CASE ("mw3")
	        SELECT CASE (iSeries)
	          CASE (1)
	            BAMAX = 81.
                CASE (2,3,6,7)
	            BAMAX = 76.
                CASE (4,5)
	            BAMAX = 69.
	        END SELECT
	      CASE ("wk1")
	        SELECT CASE (iSeries)
	          CASE (1,3:7)
	            BAMAX = 67.
	        END SELECT
	      CASE ("dw")
	        BAMAX = 50.
          END SELECT
          
	  CASE ("ESSF")                         ! ESSF
		  SELECT CASE (BEC%SubZone)
	    CASE ("dk")
	      SELECT CASE (iSeries)
	        CASE (1)
	          BAMAX = 64.
              CASE (2)
	          BAMAX = 62.
              CASE (3)
	          BAMAX = 61.
              CASE (4:7)
	          BAMAX = 62.
	      END SELECT
	    CASE ("wc4","wm")
	      BAMAX =  62.
          END SELECT 
          
	  CASE ("MS")                          ! MS
		  SELECT CASE (BEC%SubZone)
	    CASE ("dk")
	      SELECT CASE (iSeries)
	        CASE (1)
	          BAMAX = 57.
              CASE (2,3)
	          BAMAX = 55.
              CASE (4)
	          BAMAX = 46.
              CASE (5:7)
	          BAMAX = 55.
	      END SELECT
	    CASE ("dm1")
	      SELECT CASE (iSeries)
	        CASE (1,4)
	          BAMAX = 63.
              CASE (2,3,7)
	          BAMAX = 62.
              CASE (5)
	          BAMAX = 57.
              CASE (6)
	          BAMAX = 64.
	      END SELECT
          END SELECT 

	  CASE ("PP")                             ! PP
		  SELECT CASE (BEC%SubZone)
	    CASE ("dh2")
	      SELECT CASE (iSeries)
	        CASE (1,3:7)
	          BAMAX = 32.
	      END SELECT
	    CASE ("xh2")
	      SELECT CASE (iSeries)
	        CASE (1:4)
	          BAMAX = 32.
              CASE (6,7)
	          BAMAX = 21.
	      END SELECT
          END SELECT

	  CASE ("SBS")               ! SBS
          BAMAX = 55.
		  SELECT CASE (BEC%SubZone)
	    CASE ("dw1","dw2")
            BAMAX = 55.
	    CASE ("mh")
	      BAMAX = 60.
          END SELECT 

	  CASE ("SBPS")              ! SBPS
          BAMAX = 50.
		  SELECT CASE (BEC%SubZone)
	    CASE ("dc")
            BAMAX = 50.
	    CASE ("mk")
	      BAMAX = 55.
          END SELECT
        END SELECT 
      
      IF (BAMAX .LE. 0.) THEN
        BAMAX = 10.
        CALL RCDSET(2,.TRUE.)
        WRITE(JOSTND, "(/1X,' ******** WARNING: ',
     >    ' MAXIMUM BASAL AREA UNDEFINED FOR ', A,': SET T0 ', F6.1)")
     >    BEC%prettyname, BAMAX
      ENDIF
	BAMAX = BAMAX * M2pHAtoFT2pACR
C
   40 IF (LBAMX) THEN
        WRITE(JOSTND, 56) BEC%prettyname, BAMAX * FT2pACRtoM2pHA
   56   FORMAT(/1X,'MAXIMUM BASAL AREA FOR ',A,
     &      'IS SET BY DEFAULT TO:', F6.1, ' SQ M/HA.')
      ELSE
        WRITE(JOSTND, 57) BAMAX * FT2pACRtoM2pHA
   57   FORMAT(/1X,'MAXIMUM BASAL AREA FOR THIS STAND '
     &    'HAS BEEN CHANGED BY KEYWORD TO:', F6.1, ' SQ M/HA.')
      ENDIF

      DO 10 I=1,MAXSP
      IF(SDIDEF(I).LE.0.) SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
   10 CONTINUE
C
      DO 92 I=1,15
      J=(I-1)*10 + 1
      JJ=J+9
      IF(JJ.GT.MAXSP)JJ=MAXSP
      WRITE(JOSTND,90)(NSP(K,1)(1:2),K=J,JJ)
   90 FORMAT(/' SPECIES ',5X,10(A2,6X))
      WRITE(JOSTND,91)(SDIDEF(K)/ACRtoHA,K=J,JJ )
   91 FORMAT(' SDI MAX ',   10F8.0)
      IF(JJ .EQ. MAXSP)GO TO 93
   92 CONTINUE
   93 CONTINUE
C
C     RESOLVE BEC/SITE INFORMATION (VERSION 2)
C      
      CALL BECSET      
         
C----------
C  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
C  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
C  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.
C----------
      DO 50 ISPC=1,MAXSP
        IF(METHB(ISPC).EQ.999)METHB(ISPC)=6
        IF(METHC(ISPC).EQ.999)METHC(ISPC)=6
   50 CONTINUE
C----------
C  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES
C----------
      INTFOR = KODFOR - (KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
      IF(INTFOR.LT.10)FORST(1:1)='0'
      IREGN = KODFOR/100
      DIST='  '
      PROD='  '
      VAR=VARACD
      DO ISPC=1,MAXSP
      READ(FIAJSP(ISPC),'(I4)')IFIASP
      IF(((METHC(ISPC).EQ.6).OR.(METHC(ISPC).EQ.9)).AND.
     &     (VEQNNC(ISPC).EQ.'          '))THEN
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC)=VOLEQ
      ENDIF
      IF(((METHB(ISPC).EQ.6).OR.(METHB(ISPC).EQ.9)).AND.
     &     (VEQNNB(ISPC).EQ.'          '))THEN
        CALL VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC)=VOLEQ
      ENDIF
      ENDDO
C----------
C  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE
C---------
      IF(LFIA) THEN
        CALL FIAHEAD(JOSTND)
        WRITE(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I=1,MAXSP)
 211    FORMAT ((T13,8(A3,'=',A6,:,'; '),A,'=',A6))
      ENDIF
C----------
C  WRITE VOLUME EQUATION NUMBER TABLE
C----------
      CALL VOLEQHEAD(JOSTND)
      WRITE(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J=1,MAXSP)
 230  FORMAT(4(3X,A2,4X,A10,1X,A10))
C     
      RETURN
      END
