      SUBROUTINE FMSVOL (II, XHT, VOL2HT, DEBUG, IOUT)
      IMPLICIT NONE
C----------
C CANADA-FIRE-BC $Id$
C----------
C  **FMSVOL FIRE-BC
C----------
*     CALLED FROM: FMSOUT
*                  FMDOUT
*                  FMHIDE
*                  FMCWD
*                  FMSALV
*
*     CALLS:       CFVOL
*                  CFTOPK
*
*  PURPOSE:                                 
*     Calculates the volume up to height HT of each snag in record II.
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     II       SNAG RECORD NUMBER
*     XHT      HEIGHT UP TO WHICH VOLUME SHOULD BE CALCULATED
*     VOL2HT   VOLUME UP TO HEIGHT HT OF EACH SNAG IN RECORD II
*     MVOL     MERCH VOLUME UP TO HEIGHT
*
*  LOCAL VARIABLE DEFINITIONS:   
*     VM = MERCH. VOLUME
*     JS = SNAG SPECIES
*     IHT = AN INDEX OF HT 
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'

C.... VARIABLE DECLARATIONS.  

      REAL     VM, VMAX, VN
      LOGICAL  LTKIL, LC, LCONE, CTKFLG, BTKFLG, DEBUG
	LOGICAL  LMERCHIN, LMERCH
      INTEGER  JS, ISPC, IT
      REAL     D, H, BARK, XHT, VOL2HT   
      INTEGER  IOUT,II,JSP,IHT
      REAL     XH,XD,BRATIO,D2H,BBFV

C     CALCULATE THE VOLUME

      JS = SPS(II)                          
      D = DBHS(II)
      H = HTDEAD(II)
      LMERCH = .FALSE.
               
      GOTO 1000

C     ENTRY POINT FOR SNAGS CREATED BY **CUTS**.
            
      ENTRY FMSVL2(JSP,XD,XH,XHT,VOL2HT,LMERCHIN,DEBUG,IOUT)
      
      JS = JSP
      D  = XD
      H  = XH
      LMERCH = LMERCHIN

 1000 CONTINUE

      LC    = .FALSE.
      IF (XHT .GT. -1) THEN
         LTKIL = .TRUE.
      ELSE
         LTKIL = .FALSE.
         XHT = H      
      ENDIF
     
      BARK = BRATIO(JS,D,H)
      D2H = D * D * H
      IHT = INT(XHT * 100.0)

c     Actually do the call to calculate the volumes. Note that this 
C     section of the code may have to change if there are any changes
c     made within the base model routine VOLS or CFVOL
C     Also note the following variable equivalencies:
c     ITRUNC(I) = IHT
C	TKILL = LTKIL

      ISPC = JS
      LCONE = LC
      IT = 0
      CTKFLG = LTKIL

C     SEI VARIANT DOES NOT RECOGNIZE METHC=6 OR 8, SO JUST USE
C        THE CALL TO CFVOL.
C           NOTE: THIS CALL TO CFVOL DOES NOT SET VMAX, SO IF
C                 WE NEED IT, THEN WE WILL JUST SET IT TO VN
      CALL CFVOL (ISPC,D,H,D2H,VN,VM,VMAX,LTKIL,LCONE,BARK,IHT,
     1        CTKFLG)
      IF(CTKFLG .AND. LTKIL) THEN
	   VMAX = VN      
         CALL CFTOPK (ISPC,D,H,VN,VM,VMAX,LCONE,BARK,IHT)
	ENDIF

      VOL2HT = VN
	IF (LMERCH) VOL2HT = VM
 
      IF (DEBUG) WRITE(IOUT,40)ISPC,D,H,LCONE,VN
   40 FORMAT(' FMSVOL ISPC=',I3,' D=',F7.3,' H=',F7.3,
     >     ' LCONE=',L2,' VN=',F7.3)

      RETURN
      END

