      SUBROUTINE FMCWD(IYR)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C     CALLED FROM: FMSNAG
C                  FMMAIN
C     CALLS:       FMSVL2
C                  FMSVOL

C     This routine contains CWD1, CWD2, and CWD3

*  Purpose:
*     This subroutine calculates annual changes in each coarse woody
*     debris pool,
*----------------------------------------------------------------------
*
*  Local variable definitions:
*
*  Common block variables and parameters:
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
C      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.

      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'

C.... Variable declarations.

      LOGICAL LCUTS, DEBUG, LMERCH
      INTEGER I, J, K, L
      INTEGER SP, IDCL, IYR
      INTEGER ISNG, KSP
      REAL    D, DIAM, HTD, XGET, AMT
      REAL    DISIN, HTH, X, Y, Z, Q, ADD
      REAL    TVOLI, R1, R1SQ, R2SQ, P1, P2, SDIFF, S2
      REAL    HIHT(2), LOHT(2), DIS, DIH, OLDHTH, OLDHTS
      REAL    VHI(2), VLO(2), RHRAT, DIF, HICUT, LOCUT
      REAL    BP(0:9), BPH(0:9), SCNV(2), TOSOFT
      REAL    DIF3, SDIF3, BARK, VOL3, VOL4, BEHRE, BRATIO
      LOGICAL LCONE


C     Conventional breakpoints for fuel size categories.

      DATA BP   /0.0, 0.25, 1., 3., 6., 12., 20, 35, 50, 9999. /

C     Assume that soft material is 80% of the density of hard
C     Totally data-free

      DATA SCNV / 0.80, 1.00 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCWD',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) 'FMCWD',ICYC
    7 FORMAT(' ENTERING ',A,' CYCLE = ',I2)
      
C     Apply respiration decay losses and duff-conversion transfer
C     to the CWD array. These losses are not tracked anywhere yet;
C     they simply disappear. SOFT material is assumed to disappear
C     10% faster than hard. All duff goes into the hard pool

      DO 5 I = 1, 2
        DO 8 L = 1, 4

C         First decay duff so can add stuff to it later

          CWD(I,11,1,L) = CWD(I,11,1,L) * (1.0 - (DKR(11,L)*1.1))**NYRS
          CWD(I,11,2,L) = CWD(I,11,2,L) * (1.0 - DKR(11,L))**NYRS
          IF (CWD(I,11,1,L) .LT. 0.0) CWD(I,11,1,L) = 0.0
          IF (CWD(I,11,2,L) .LT. 0.0) CWD(I,11,2,L) = 0.0

          DO 6 J = 1, 10
C           To work on cycle boundaries, we need to figure out how much
C           material gets decayed, and then take a proportion of that and
C           move it to duff (i.e., use PRDUFF directly, instead of TODUFF
C           which combines PRDUFF and DKR together). (April 2013)

C           Determine amount by the pools are reduced, and take a proportion
C           to turn material into duff
            AMT = CWD(I,J,1,L) - 
     &                 CWD(I,J,1,L) * (1.0-(DKR(J,L)*1.1))**NYRS
            IF (AMT .LT. 1.0E-9) AMT = 0.0
            CWD(I,11,2,L) = CWD(I,11,2,L) + AMT * PRDUFF(J,L)
            AMT = CWD(I,J,2,L) - CWD(I,J,2,L) * (1.0-(DKR(J,L)))**NYRS
            IF (AMT .LT. 1.0E-9) AMT = 0.0
            CWD(I,11,2,L) = CWD(I,11,2,L) + AMT * PRDUFF(J,L)
            
c            CWD(I,11,2,L) = CWD(I,11,2,L) +                             
c     &                      CWD(I,J,1,L) * (1.1 * TODUFF(J,L))
c            CWD(I,11,2,L) = CWD(I,11,2,L) + 
c     &                             CWD(I,J,2,L) * TODUFF(J,L)

C           Now actually decrease the pools
            CWD(I,J,1,L) = CWD(I,J,1,L) * (1.0 - (DKR(J,L) * 1.1))**NYRS
            CWD(I,J,2,L) = CWD(I,J,2,L) * (1.0 -  DKR(J,L))**NYRS
            IF (CWD(I,J,1,L) .LT. 1.0E-9) CWD(I,J,1,L) = 0.0
            IF (CWD(I,J,2,L) .LT. 1.0E-9) CWD(I,J,2,L) = 0.0
            
C           Now move material from hard to soft
C           This is new as of Nov 2010.  Assuming a log is soft when it is at
C           64% of its original density (Kim Mellen-McLean), we estimate the 
C           years until soft and then take the inverse to determine what 
C           proportion of the hard pool moves to the soft pool.

            IF (J .LT. 10) THEN
              TOSOFT = NYRS*(LOG(1-DKR(J,L)))/(LOG(0.64))
              IF (TOSOFT .LT. 0) TOSOFT = 0
              IF (TOSOFT .GT. 1) TOSOFT = 1              
              TOSOFT = TOSOFT * CWD(I,J,2,L)
              CWD(I,J,1,L) = CWD(I,J,1,L) + TOSOFT
              CWD(I,J,2,L) = CWD(I,J,2,L) - TOSOFT
              IF (CWD(I,J,1,L) .LT. 1.0E-9) CWD(I,J,1,L) = 0.0
              IF (CWD(I,J,2,L) .LT. 1.0E-9) CWD(I,J,2,L) = 0.0
            ENDIF
                      
    6     CONTINUE
    8   CONTINUE
    5 CONTINUE

C     NOTE: FuelMove logic was moved to new routine: FMFMOV
      RETURN
C *******************************************************************

C     CWD1:

C     CALLED FROM FMSNAG

C     ENTRY POINT FOR ADDITION OF NEW CWD VIA FALLING OF SNAGS.

C     ISNG  = INDEX IN SNAG LIST
C     KSP   = SPECIES OF SNAG
C     D     = DBH OF SNAG
C     DIH   = DENSITY (#/AC) OF INITIALLY-HARD SNAGS FALLEN
C     DIS   = DENSITY (#/AC) OF INITIALLY-SOFT SNAGS FALLEN

      ENTRY CWD1(ISNG, DIH, DISIN)

      CALL DBCHK (DEBUG,'FMCWD',5,ICYC)
      
      IF (DEBUG) WRITE(JOSTND,7) 'FM-CWD1',ICYC

      IF (DEBUG) WRITE (JOSTND,*) 'ISNG=',ISNG,' DIH=',DIH,
     >                            ' DISIN=',DISIN

      IF ((DIH+DISIN) .LE. 0.0) RETURN
      I = ISNG
      DIS = DISIN

C     These snags are not created by **CUTS**

      LCUTS = .FALSE.

C     Height of top and bottom limits for volume integration;
C     1=soft and 2=hard

      HIHT(1) = HTIS(I)
      HIHT(2) = HTIH(I)

      LOHT(1) = 1.0
cc1      LOHT(2) = 1.0
      LOHT(2) = 0.10
      
      DIAM = DBHS(I)
      HTD  = HTDEAD(I)
      SP   = SPS(I)

C     GET A TOTAL VOLUME FOR THIS SNAG

      TVOLI=0.
      CALL FMSVOL(I,HTD,TVOLI,.false.,JOSTND)
      IF (DEBUG) WRITE (JOSTND,*) 'I(CWD1)=',I,' HTD=',HTD,
     >                            ' TVOLI=',TVOLI

      GOTO 1000

C *******************************************************************

C     CWD2:

C     CALLED FROM FMSNAG

C     ISNG    = INDEX IN SNAG LIST
C     DIH     = DENSITY (#/AC) OF INITIALLY-HARD SNAGS BROKEN
C     DIS     = DENSITY (#/AC) OF INITIALLY-SOFT SNAGS BROKEN
C     OLDHTS  = HEIGHT (FT) BEFORE BREAKAGE OF INITIALLY-SOFT SNAGS
C     OLDHTH  = HEIGHT (FT) BEFORE BREAKAGE OF INITIALLY-HARD SNAGS

      ENTRY CWD2(ISNG, DIH, DISIN, OLDHTH, OLDHTS)

      CALL DBCHK (DEBUG,'FMCWD',5,ICYC)
      
      IF (DEBUG) WRITE(JOSTND,7) 'FM-CWD2',ICYC

      IF (DEBUG) WRITE (JOSTND,*) 'ISNG=',ISNG,' DIH=',DIH,
     >                            ' DISIN=',DISIN


      IF ((DIH+DISIN) .LE. 0.0) RETURN
      I = ISNG
      DIS = DISIN

C     These snags are not created by **CUTS**

      LCUTS = .FALSE.

C     Height of top and bottom limits for volume integration;
C     soft and hard

      HIHT(1) = OLDHTS
      HIHT(2) = OLDHTH

      LOHT(1) = HTIS(I)
      LOHT(2) = HTIH(I)

      DIAM = DBHS(I)
      HTD  = HTDEAD(I)
      SP   = SPS(I)

C     GET A TOTAL VOLUME FOR THIS SNAG

      TVOLI=0.
      CALL FMSVOL(I,HTD,TVOLI,.false.,JOSTND)

      GOTO 1000

C *******************************************************************
C     CWD3:

C     CALLED FROM FMSCUT

C     ISP     = SPECIES INDEX
C     D       = DBH
C     DIH     = DENSITY (#/AC) OF DOWNED (HARD) SNAGS FROM **CUTS**
C     HT      = HEIGHT (FT) OF TREE JUST DOWNED

      ENTRY CWD3(KSP, D, DIH, HTH)

      DEBUG = .FALSE.

      IF (DIH .LE. 0.0) RETURN
C     This snag-material is created by **CUTS**. It is all hard.

      DIS  = 0.
      LCUTS = .TRUE.

C     Height of top and bottom limits for volume integration;
C     soft are ignored and set to zero

      HIHT(1) = 0.
      HIHT(2) = HTH

      LOHT(1) = 0.
cc1      LOHT(2) = 1.
      LOHT(2) = .1

      DIAM = D
      HTD  = HTH
      SP   = KSP
      
C     GET A TOTAL VOLUME FOR THIS TREE (created by cuts)

      TVOLI=-1      
      CALL FMSVL2(SP,DIAM,HTD,TVOLI,TVOLI,.false.,.false.,JOSTND)


C *******************************************************************

 1000 CONTINUE

C     Use a conical taper to find the largest possible diameter of the debris
C     54 is the height for DBH measurements, HT0 is the height of the original
C     tree (uncorrected for topkill, so far). X is the diameter of the large end
C     of the fragment or the base of the tipped snag, depending on context. Use
C     that value to find the size class.

C      X = (HIHTH - LOHTH) * 12. * DBHS(I) / ((LOHTH * 12.) - 54.)

C     RADIUS/HEIGHT RATIO FOR TRIANGLE (CONE MODEL OF TREE)

      IF(DIAM .LE. 0.1) DIAM=0.1
ccccccc sdiff and s2 are only for debugging new code
      SDIFF = 0.
      S2 = 0.
           
      RHRAT =   ((HTD * 12.) - 54.) / (0.5 * DIAM)

      IDCL = DKRCLS(SP)

C     FIND HEIGHTS AT WHICH THE BREAKPOINTS WILL LIE. A -1 MARKS
C     A BREAKPOINT OUTSIDE THE HEIGHT OF THE SNAG. THE BOTTOM OF THE
C     SNAG IS AT 1 FOOT.

      DO 10 J = 0, 9

        X = (0.5 * BP(J) * RHRAT) / 12.0
        Y = HTD - X

cc1       BPH(J) = MAX(1.0, Y)
        BPH(J) = MAX(0.10, Y)

   10 CONTINUE
cc      WRITE (JOSTND,*) 'BPH=',BPH

C     WALK THROUGH ALL THE BREAKPOINTS. IF ANY INTERVAL CONTAINS
C     A PIECE OF THE TREE OF INTEREST, DO SOME CALCS.

      DO 20 K = 1, 2
        VHI(K) = 0.
        VLO(K) = 0.

C       skip the loop if there are no snag pieces of K-type initial hardness

        IF (K .EQ. 1) THEN
          IF (DIS .LE. 0.0) GOTO 20
        ELSE
          IF (DIH .LE. 0.0) GOTO 20
        END IF

cc1        LOHT(K) = MAX(1.0, LOHT(K))
        LOHT(K) = MAX(0.10, LOHT(K))

        R1 = DIAM*0.0416666667  ! (= 1/12 * .5)
        IF (HTD .GT. 4.5) THEN  ! compute r1 at base of stem.
          R1 = R1 + (LOHT(K) * ((R1*HTD)/(HTD-4.5)))
        ENDIF
        R1SQ = R1*R1

        DO 21 J = 1, 9

C         skip loop if none of the broken snag falls in current size category

          IF (HIHT(K) .LE. BPH(J))    GOTO 21
          IF (LOHT(K) .GT. BPH(J-1))  GOTO 21

C         set HICUT to top of the broken snag or top of current size category,
C         whichever is less

          HICUT = HIHT(K)
          IF (HIHT(K) .GT. BPH(J-1))  HICUT = BPH(J-1)

C         set LOCUT to bottom of the broken snag or bottom of current size
C         category, whichever is greater

          LOCUT = LOHT(K)
          IF (LOHT(K) .LE. BPH(J)) LOCUT = BPH(J)

C         if the low and high points are the same, then we have reached the end and 
C         do not need to calculate volumes          
          IF (LOCUT .EQ. HICUT) GOTO 21
         
C         get the TOTAL volume-per-snag up to HICUT and up to LOCUT.  Set DIF to the
C         volume between them - i.e., the vol. in the current size category -
C         and convert it to volume-per-acre.

ccccccccccc This block can be deleted if the new code is used
c          IF (LCUTS) THEN
c            LMERCH = .FALSE.
c            CALL FMSVL2(SP,DIAM,HTD,HICUT,VHI(K),LMERCH,.false.,JOSTND)
c            CALL FMSVL2(SP,DIAM,HTD,LOCUT,VLO(K),LMERCH,.false.,JOSTND)
c          ELSE
c            CALL FMSVOL(I,HICUT,VHI(K),.false.,JOSTND)
c            CALL FMSVOL(I,LOCUT,VLO(K),.false.,JOSTND)
c          ENDIF
ccccccccccc above .........

          R2SQ = R1 * (1. - (HICUT/HTD))
          R2SQ = R2SQ * R2SQ
          P1 = (R2SQ*(HTD-HICUT))/(R1SQ*HTD)
          R2SQ = R1 * (1. - (LOCUT/HTD))
          R2SQ = R2SQ * R2SQ
          P2 = (R2SQ*(HTD-LOCUT))/(R1SQ*HTD)
          DIF = MAX(0.,(P2-P1)) * TVOLI
          SDIFF = SDIFF+DIF
          S2 = S2 + (VHI(K) - VLO(K))
          IF (DEBUG) WRITE (JOSTND,*) 'R2=',R2SQ**.5,'P1=',
     >        P1,'P2=',P2,' DIF=',DIF,
     >      ' OLDDIF=',VHI(K) - VLO(K),' SDIFF=',SDIFF,' S2=',S2
                     
c***uncomment to use old DIF:          DIF = VHI(K) - VLO(K)

          IF (K .EQ.1) THEN
            DIF = DIF * DIS
          ELSE
            DIF = DIF * DIH
          END IF

C         allocate the volume DIF of material to the various CWD categories.

          ADD = 0.
          IF (DIF .GT. 1.E-6) THEN
            ADD = DIF * V2T(SP) * SCNV(K)
            CWD(1,J,K,IDCL) = CWD(1,J,K,IDCL) + ADD

            CWDNEW(2,J) = CWDNEW(2,J) + ADD
          ENDIF

          IF (DEBUG) WRITE (JOSTND,16) I,K,LOCUT,
     >        HICUT, VHI(K), VLO(K),DIF,ADD,TVOLI
   16     FORMAT(' I=',I4,' K=',I2,' LOCUT=',F7.3,' HICUT=',
     >          F7.3,' VHI=',F7.3,' VLO=',F7.3,' DIF=',F8.5,
     >          ' ADD=',F10.6,' TVOLI=',F10.6)

   21   CONTINUE
   20 CONTINUE

      RETURN
      END
