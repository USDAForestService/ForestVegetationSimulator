      SUBROUTINE FMSVOL (II, XHT, VOL2HT, DEBUG, IOUT)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
*     CALLED FROM: FMSOUT
*                  FMDOUT
*                  FMHIDE
*                  FMCWD
*                  FMSALV
*
*     CALLS:       CFVOL
*                  NATCRS
*                  OCFVOL
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
*     MVOL     MERCH VOLUMNE UP TO HEIGHT
*
*  LOCAL VARIABLE DEFINITIONS:
*     MCF = MERCH. VOLUME
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

      REAL     MCF, VMAX, BFMAX, TCF, SCF
      LOGICAL  LTKIL, LC, LCONE, CTKFLG, BTKFLG, DEBUG
      LOGICAL  LMERCHIN, LMERCH
      INTEGER  JS, ISPC, IT
      REAL     D, H, BARK, XHT, VOL2HT
      INTEGER  IOUT,II,JSP,IHT
      INTEGER  CRWNRTO,CWN,DCY,WSTM
      REAL     XH,XD,BRATIO,D2H,BBFV,X,CL,BIODRY(15)
      CHARACTER LIVEDEAD,LVD

C     CALCULATE THE VOLUME

      JS = SPS(II)
      D = DBHS(II)
      H = HTDEAD(II)
      LMERCH = .FALSE.
      CWN = 0
      CL = 0
      DCY = 0
      WSTM = 0
      IF(LFIANVB) THEN
        LVD = 'D'
      ELSE
        LVD = ' '
      ENDIF

      GOTO 1000

C     ENTRY POINT FOR SNAGS CREATED BY **CUTS**.

      ENTRY FMSVL2(JSP,XD,XH,XHT,VOL2HT,CRWNRTO,
     1             LIVEDEAD,LMERCHIN,DEBUG,IOUT)

      JS = JSP
      D  = XD
      H  = XH
      LMERCH = LMERCHIN

      CWN = CRWNRTO
      CL     = 0
      DCY    = 0
      WSTM   = 0
      BIODRY = 0
      IF(LFIANVB) THEN
        LVD = LIVEDEAD
      ELSE
        LVD = ' '
      ENDIF

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

C      This call was replaced by the code below (NATCRS, etc.).
C      CALL CFVOL (JS,D,H,D2H,VOL2HT,VM,VMAX,LTKIL,LC,BARK,IHT,LDUM)

c     Actually do the call to calculate the volumes. Note that this
C     section of the code may have to change if there are any changes
c     made within the base model routine VOLS or CFVOL
C     Also note the following variable equivalencies:
c     ITRUNC(I) = IHT
C     TKILL = LTKIL

      ISPC   = JS
      LCONE  = LC
      IT     = 0
      CTKFLG = LTKIL
      BTKFLG = .FALSE.

      IF(METHC(ISPC).EQ.6 .OR. METHC(ISPC).EQ.10) THEN
         CALL NATCRS (TCF,MCF,SCF,BBFV,ISPC,D,H,LTKIL,
     1                CWN,BARK,IHT,VMAX,BFMAX,
     1                CL,DCY,WSTM,BIODRY,
     1                LVD,CTKFLG,BTKFLG,-1)
      ELSEIF ((METHC(ISPC).EQ.8).OR.(METHC(ISPC).EQ.5)) THEN
         CALL OCFVOL (TCF,MCF,ISPC,D,H,LTKIL,BARK,IHT,VMAX,LCONE,
     1        CTKFLG,IT)
      ELSE
         CALL CFVOL (ISPC,D,H,D2H,TCF,MCF,VMAX,LTKIL,LCONE,BARK,IHT,
     1        CTKFLG)
      ENDIF
      IF(CTKFLG .AND. LTKIL)
     1     CALL CFTOPK (ISPC,D,H,TCF,MCF,SCF,VMAX,LCONE,BARK,IHT)

C     Give some small volume to very tiny trees.
C     based on cone with D = 1 inch
      X = 0.005454154 * H

      IF (VARACD .EQ. 'CS' .OR. VARACD .EQ. 'LS'
     >    .OR. VARACD .EQ. 'NE' .OR. VARACD .EQ. 'SN') THEN
        VOL2HT = MAX(X,MCF)
        IF (LMERCH) VOL2HT = SCF
      ELSE
        VOL2HT = MAX(X,TCF)
        IF (LMERCH) VOL2HT = MCF
      ENDIF

      IF (DEBUG) WRITE(IOUT,40)ISPC,D,H,LCONE,TCF,VOL2HT
 40   FORMAT(' FMSVOL ISPC=',I3,' D=',F7.3,' H=',F7.3,
     >     ' LCONE=',L2,' VN=',F7.3,' VOL2HT=',F10.3)

      RETURN
      END

