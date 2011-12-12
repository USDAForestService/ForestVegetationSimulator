      SUBROUTINE SVCWD(IYEAR)
      IMPLICIT NONE
C----------
C  **SVCWD--BASE  DATE OF LAST REVISION: 12/08/09
C----------
C
C     STAND VISUALIZATION GENERATION
C     S.N.SCHAROSCH -- Abacus -- APR 2008
C
C   Purpose:
C     This routine creates SVS objects for the coarse woody debris
C     pools predicted by FFE.
C
C   Called from: SVOUT
C   Calls:       FMSVL2
C                RANN
C                SVGTPT
C
C   Local variable definitions:
C     NSVCWD   Existing SVS CWD objects, by piled status, by TCWD3
C              sizeclass.
C     NSVCHG   The computed change in SVS object count that needs to
C              be applied, by piled status, by TCWD3 sizeclass.
C     SNGVOL:  Array of fallen snag volumes to be subtracted from
C              the TCWD3 array.
C     TCWD3:   Compressed version of Coarse Woody Debris array CWD:
C              For TCWD3(i,j):
C                    i=piled status:
C                      1=unpiled
C                      2=piled
C                      3=summation
C                    j=size class:
C                      1="<0.25in" & "0.25-1in" & "1-3in"
C                        (litter & duff omitted)
C                      2="3-6in"
C                      3="6-12in"
C                      4=">12in"
C     BPH:     Height on the current snag, where each of the fuelsize
C              breakpoints for the TCWD3 array occurr.
C
C   Common variable definitions:
C     V2T:     Volume (cuft) to Tons conversion factor (in /FMCOM/)
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'SVDATA.F77'
C
C
      INCLUDE 'SVDEAD.F77'
C
C
COMMONS
C
      INTEGER CWDDL1, CWDDL2, I, IBP, ICWD, ID, IH, IP, IPC, IPCNT,
     &        IPS, IPUT, IOBJ, ISVOBJ, ISZCLS, IYEAR, J, JBP, KSP,
     &        NSVCHG(2,4), NSVCWD(2,4), NSVNEW(2,4),
     &        NUM2DEL, SP
      LOGICAL DEBUG
      REAL    AVGDIA, AVGLEN, AVGV2T, DIAM, HTD, RHRAT, DIAM2, HTD2
      REAL    BP(0:4), BPH(0:4)
      REAL    DIF, HICUT, HIHT, LGRNUM, LGRTIO, LOCUT, LOHT,
     &        PCOUNT(2,4), PCSIZE(2,4), SNGVOL(4), TOTBA, TCWD3(3,4),
     &        TREEBA, VHI, VLO, X, XTRACW, XX, Y
      DOUBLE PRECISION SAVESO, SAVESVSO

C
C     Upper breakpoints for TCWD3 fuel size categories:
      DATA BP   / 0.000, 3.000, 6.000, 12.000, 999.000 /
C
C
C----------
C  Check for debug:
C----------

      CALL DBCHK (DEBUG,'SVCWD',5,ICYC)
      IF (DEBUG) THEN
        WRITE(JOSTND,1010) ICYC, IYEAR, LFMON
 1010   FORMAT (' ','ENTERING SVCWD, ICYC=',I2,', IYEAR=',I4,
     &              ', LFMON=',L1,'.', / )
      ENDIF

C----------
C  Check to see if the FFE is active: if not, there's no CWD to process,
C  so return.
C----------

      IF ( .NOT. LFMON ) RETURN

C----------
C  Create a compressed version of the CWD (coarse woody debris) array.
C  CWD (tons/ac) array indices:
C    (1:3,  -,  -,  -) 1 = Unpiled, 2 = Piled, 3 = summation
C    (  -,1:11,  -,  -) MXFLCL categories defined in FMPARM.F77:
C     1 : <0.25"
C     2 :  0.25" -   1"
C     3 :  1"    -   3"
C     4 :  3"    -   6"
C     5 :  6"    -  12"
C     6 :  12"   -  20"
C     7 :  20"   -  35"
C     8 :  35    -  50"
C     9 :          >50"
C     10:  Litter
C     11:  Duff
C    (  -,  -,1:2,  -) 1 = SOFT, 2 = HARD
C    (  -,  -,  -,1:4) 1 = V.Slow,..., 4 = Fast decay rate
C
C  The "Litter" and "Duff" categories of the base CWD array are ignored,
C  since they can potentially generate a very large number of SVS
C  objects that can potentially displace more significant SVS objects.
C----------

      DO IP=1,3
        DO J=1,4
          TCWD3(IP,J) = 0.0
        ENDDO
      ENDDO
      DO IP=1,3
        DO IH=1,2
          DO ID=1,4
            TCWD3(IP,1) = TCWD3(IP,1) + CWD(IP,1,IH,ID)
     &                                + CWD(IP,2,IH,ID)
     &                                + CWD(IP,3,IH,ID)
            TCWD3(IP,2) = TCWD3(IP,2) + CWD(IP,4,IH,ID)
            TCWD3(IP,3) = TCWD3(IP,3) + CWD(IP,5,IH,ID)
            TCWD3(IP,4) = TCWD3(IP,4) + CWD(IP,6,IH,ID) +CWD(IP,7,IH,ID)
     &                                + CWD(IP,8,IH,ID) +CWD(IP,9,IH,ID)
          ENDDO
        ENDDO
      ENDDO

C----------
C  Accumulate a Ht/radius ratio for large snags (>6 in diam, both
C  standing & fallen), for use later in CWD piecesize calculations:
C----------

      LGRTIO = 0.0
      LGRNUM = 0.0
      IF ( NSVOBJ .GT. 0 ) THEN
        DO IOBJ=1,NSVOBJ
          IF (IOBJTP(IOBJ).NE.2) CYCLE
          IF (ODIA(IS2F(IOBJ)).GE.6. ) THEN
            RHRAT = ((OLEN(IS2F(IOBJ)) * 12.) - 54.) /
     &              (0.5 * ODIA(IS2F(IOBJ)))
            LGRTIO = LGRTIO + RHRAT
            LGRNUM = LGRNUM + 1
          ENDIF
        ENDDO
      ENDIF
      IF ( LGRTIO .EQ. 0.0 ) THEN
        LGRTIO = 48.
        LGRNUM = 1.
      ENDIF

C----------
C  Recompute piled status=3, since base CWD array doesn't always
C  have that status totalled:
C----------

      DO ISZCLS=1,4
        TCWD3(3,ISZCLS) = TCWD3(1,ISZCLS) + TCWD3(2,ISZCLS)
      ENDDO

      IF (DEBUG) THEN
        WRITE(JOSTND,1020) ICYC, IYEAR,
     &                     (TCWD3(3,I),I=1,4)
 1020   FORMAT (' ','IN SVCWD, ICYC=',I2,', IYEAR=',I4,':', / ,
     &          ' ',T5,'TCWD3 ARRAY (PILED & UNPILED):',/,
     &          ' ',T5,' 0-3in   3-6in   6-12in  12+in ',/,
     &          ' ',T5,'------- ------- ------- -------',/,
     &          ' ',T5,4(F7.3,1X),'BEFORE SNAG REMOVAL')
C                       XXX.XXX XXX.XXX XXX.XXX XXX.XXX
      ENDIF

C----------
C  Now we need to subtract the fallen snags out of the CWD pools,
C  since the fallen snags have already been assigned SVS objects.
C
C  First, find all fallen snags in the SVS object arrays (have to
C  look in the SVS object arrays since fallen snags get immediately
C  removed from the FFE snag arrays).
C
C  Ignore salvaged snags which are being held temporarily until
C  the next call to SVOUT (ISTATUS is negative).
C----------

      IF ( NSVOBJ .GT. 0 ) THEN
        DO J=1,4
          SNGVOL(J) = 0.0
        ENDDO
        DO 100 IOBJ=1,NSVOBJ
          IF (IOBJTP(IOBJ).NE.2) CYCLE
          IF (FALLDIR(IS2F(IOBJ)).EQ.-1 .OR.
     &        ISTATUS(IS2F(IOBJ)).LE. 0 )
     &      GOTO 100

C----------
C  The boles of fallen snags were previously split apart and tallied
C  into the various CWD size classes in FMCWD.
C  We need to reverse that process, and remove the parcelized fallen
C  snag volume from the various CWD sizeclasses.
C
C  Set top/bottom height limits for volume integration
C----------

          HIHT = SNGLEN(IS2F(IOBJ))
          LOHT = 1.0

C----------
C  Use a conical taper equation to find heights to various upper-stem
C  diameters for the current snag:
C
C  The following logic, taken from FMCWD, uses a cone profile of the tree
C  to find heights that correspond to the various CWD fuelsize breakpoints.
C  The conical taper profile is represented by a triangle:
C    base(in) = 0.5 * diam
C    length(in) = ht(ft) * 12 - 54   (where 54 is DBH ht)
C
C    HTD:   Original height of the snag at time of death (uncorrected for
C           subsequent top breakage)
C    RHRAT: Radius/height ratio
C    54:    Height (in inches) for DBH measurement
C----------

          DIAM = ODIA(IS2F(IOBJ))
          HTD  = OLEN(IS2F(IOBJ))
          SP   = ISNSP(IS2F(IOBJ))
          IF (DIAM .LE. 0.1) DIAM=0.1
          RHRAT = ((HTD * 12.) - 54.) / (0.5 * DIAM)

C----------
C  Find heights at which the fuelsize breakpoints will lie.
C  A value of 1 indicates that the snag diameter doesn't reach the
C  specified breakpoint size.
C  The bottom of the snag is at 1 foot.
C----------

          DO IBP=0,4
            X = (0.5 * BP(IBP) * RHRAT) / 12.0
            Y = HTD - X
            BPH(IBP) = MAX(1.0, Y)
          ENDDO

C----------
C  Progress down through the CWD sizeclass breakpoints.
C  If any sizeclass contains a piece of the current snag, do some calcs.
C
C  Skip calculations for current sizeclass if:
C    1) Current hard/soft snag height is less than the computed height to the
C       upper stem diameter that corresponds to the current CWD breakpoint
C       (ie snag has broken off at a height lower than current CWD diam
C        breakpoint):
C         BPH(0) = ht to 0" top
C         BPH(1) = ht to 3" top
C         BPH(2) = ht to 6" top
C         BPH(3) = ht to 12" top
C         BPH(4) = ht to 999" top
C    2) Current snag stump ht (1') is greater than the computed height to the
C       next largest CWD breakpoint ht
C----------

          DO 50 IBP=1,4
            IF (HIHT .LE. BPH(IBP))    GOTO 50
            IF (LOHT .GT. BPH(IBP-1))  GOTO 50

C----------
C  Set HICUT to top of the broken snag or top of current size category,
C  whichever is less.
C  Set LOCUT to bottom of the broken snag or bottom of current size
C  category, whichever is greater.
C----------

            HICUT = HIHT
            IF (HIHT .GT. BPH(IBP-1))  HICUT = BPH(IBP-1)
            LOCUT = LOHT
            IF (LOHT .LE. BPH(IBP))    LOCUT = BPH(IBP)

C----------
C  Get the TOTAL volume-per-snag up to HICUT and up to LOCUT.
C  Set DIF to the volume between them - i.e., the volume in the current
C  size category. (FMSVL2 is entry point in FMSVOL, that uses passed-in
C  sp,d,ht values instead of using snag index number to retrieve)
C----------
            DIAM2 = SNGDIA(IS2F(IOBJ))
            HTD2  = SNGLEN(IS2F(IOBJ))
            CALL FMSVL2(SP,DIAM2,HTD2,HICUT,VHI,.FALSE.,.FALSE.,JOSTND)
            CALL FMSVL2(SP,DIAM2,HTD2,LOCUT,VLO,.FALSE.,.FALSE.,JOSTND)
            DIF = VHI - VLO

C----------
C  Convert the fallen snag volume in the current sizeclass to tons, then
C  tally into total for size class.
C----------

            DIF = DIF * V2T(SP)
            IF ( ISTATUS(IS2F(IOBJ)) .EQ. 4 ) THEN
              DIF = DIF * 0.80
            ENDIF
            SNGVOL(IBP) = SNGVOL(IBP) + DIF
   50     CONTINUE
  100   CONTINUE

        IF (DEBUG) THEN
          WRITE(JOSTND,1030) (SNGVOL(I),I=1,4)
 1030     FORMAT (' ',T5,4(F7.3,1X),'FALLEN SNAG PARECELIZED TONS')
        ENDIF

C----------
C  Now we need to subtract fallen snag volumes proportionally out of
C  the piled/unpiled TCWD3 pools.
C  If FFE fuel treatments have been done (EG chipping), there may be
C  insufficient volume in a given CWD pool to accomodate the
C  computed reduction (since the fuel treatments don't impact the
C  base svs snag records, there may be more volume in the snag
C  records than is represented in the CWD arrays). If so, then
C  reallocate the snag volume removal to smaller (or larger) CWD
C  classes.
C----------

        DO IBP=4,1,-1
          XTRACW = SNGVOL(IBP) - TCWD3(3,IBP)
          IF ( XTRACW .GT. 0.0 ) THEN
            IF ( IBP .GT. 1 ) THEN
              SNGVOL(IBP) = TCWD3(3,IBP)
              SNGVOL(IBP-1) = SNGVOL(IBP-1) + XTRACW
            ELSE

C             We have an excess in the smallest CWD category.
C             The excess may have originated here, or it may
C             have been moved down from larger categories.
C             In either case, try to move it upwards through
C             the size classes; quit when you reach the top.

              DO JBP=1,4
                XTRACW = SNGVOL(JBP) - TCWD3(3,JBP)
                IF ( XTRACW .GT. 0.0 ) THEN
                  IF ( JBP .LT. 4 ) THEN
                    SNGVOL(JBP) = TCWD3(3,JBP)
                    SNGVOL(JBP+1) = SNGVOL(JBP+1) + XTRACW
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDDO

        IF (DEBUG) THEN
          WRITE(JOSTND,1035) (SNGVOL(I),I=1,4)
 1035     FORMAT (' ',T5,4(F7.3,1X),'FALLEN SNAG PARECELIZED TONS, ',
     &                'ADJUSTED')
        ENDIF

C----------
C  Done reallocating overages in snag CWD reductions.
C  Now we need to subtract fallen snag volumes proportionally out of
C  piled and unpiled CWD pools.
C----------

        DO IBP=1,4
          DO IP=1,3
            IF ( TCWD3(3,IBP) .GT. 0.0 ) THEN
              TCWD3(IP,IBP) = TCWD3(IP,IBP) -
     &                      ( SNGVOL(IBP)*(TCWD3(IP,IBP)/TCWD3(3,IBP)) )
            ELSE
              TCWD3(IP,IBP) = 0.0
            ENDIF
            IF ( TCWD3(IP,IBP) .LT. 0.0 ) TCWD3(IP,IBP)=0.0
          ENDDO
        ENDDO
      ENDIF

      IF (DEBUG) THEN
        WRITE(JOSTND,1040) (TCWD3(3,I),I=1,4)
 1040   FORMAT (' ',T5,4(F7.3,1X),'AFTER SNAG REMOVAL')
      ENDIF

C----------
C  Now that the fallen snags have been removed from the TCWD3 array,
C  convert the remaining CWD loadings from tons to cuft.
C  Since the cuft/ton conversion factors are species-specific, and the
C  CWD array is not; we need to compute an average cuft/ton conversion
C  factor, based on the current stand composition.
C  If the stand has been clearcut, and there are no trees for computing
C  a weighted average conversion factor, use a simple arithmetic mean
C  of the species-specific conversion factors.
C----------

      AVGV2T = 0.0
      TOTBA  = 0.0
      DO I=1,ITRN
        IF (FMPROB(I) .GT. 0.0) THEN
          KSP = ISP(I)
          TREEBA = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          TOTBA = TOTBA + TREEBA
          AVGV2T = AVGV2T + (TREEBA*V2T(KSP))
        ENDIF
      ENDDO
      IF ( TOTBA .GT. 0.0 ) THEN
        AVGV2T = AVGV2T/TOTBA
      ELSE
        AVGV2T = 0.0
        DO I=1,MAXSP
          AVGV2T = AVGV2T + V2T(I)
        ENDDO
        AVGV2T = AVGV2T/MAXSP
      ENDIF

      DO IP=1,3
        DO ISZCLS=1,4
          IF ( AVGV2T .GT. 0.0 ) THEN
            TCWD3(IP,ISZCLS) = TCWD3(IP,ISZCLS) / AVGV2T
          ELSE
            TCWD3(IP,ISZCLS) = 0.0
          ENDIF
        ENDDO
      ENDDO

      IF (DEBUG) THEN
        WRITE(JOSTND,1060) (TCWD3(3,I),I=1,4), AVGV2T
 1060   FORMAT (' ',T5,4(F7.1,1X),'CONVERTED TO CUFT (AVGV2T=',F6.4,')')
      ENDIF

C----------
C  The TCWD3 array has now been adjusted to remove fallen snags,
C  and converted from tons to cuft.
C  We need to compute the number of pieces the TCWD3 volumes
C  represent, assuming an average piecesize for each TCWD3 sizeclass:
C  (piece diameters are set at the maximums for the fuel class in order
C   to decrease the total CWD piececounts)
C    Sizeclass 1= "<0.25in" + "0.25-1in" + "1-3in":
C      2.99in piece diameter
C     144.0in piece length (48xdiam)
C    Sizeclass 2="3-6in":
C      5.99in piece diameter
C     288.0in piece length (48xdiam)
C    Sizeclass 3="6-12in":
C      11.99in piece diameter
C      compute piece length from avg length/radius ratio computed above.
C      (use half of expected length, since these pieces are typcially
C       broken snag tops--not whole snags)
C    Sizeclass 4=">12in":
C       16.0in piece diameter
C      compute piece length from avg length/radius ratio computed above.
C      (use half of expected length, since these pieces are typcially
C       broken snag tops--not whole snags)
C
C  Using above piecesizes, compute the number of CWD pieces represented by
C  the current CWD loadings, versus the number of SVS CWD objects that are
C  currently defined.
C----------

      DO IPS=1,2
        DO ISZCLS=1,4
          SELECT CASE (ISZCLS)
            CASE (1)
              AVGDIA = 2.99
              AVGLEN = AVGDIA * 48.0
            CASE (2)
              AVGDIA = 5.99
              AVGLEN = AVGDIA * 48.0
            CASE (3)
              AVGDIA = 11.99
              AVGLEN = (AVGDIA * 0.5) * (LGRTIO/LGRNUM) * 0.5
            CASE (4)
              AVGDIA = 16.0
              AVGLEN = (AVGDIA * 0.5) * (LGRTIO/LGRNUM) * 0.5
          END SELECT

C----------
C  Compute average piecesize (cuft), as a cylinder, then,
C  compute expected piececount for the current CWD pool.
C----------

          PCSIZE(IPS,ISZCLS) = 3.1416 * ((AVGDIA/2)**2) * AVGLEN / 1728.
          PCOUNT(IPS,ISZCLS) = TCWD3(IPS,ISZCLS) / PCSIZE(IPS,ISZCLS)
          IPCNT = INT(PCOUNT(IPS,ISZCLS)+0.5)

C----------
C  Count up existing SVS objects that match current CWD sizeclass.
C----------

          NSVCWD(IPS,ISZCLS) = 0
          DO ISVOBJ=1,MXSVOB
            IF (IOBJTP(ISVOBJ).NE.4) CYCLE 
            IF ( CWDPIL(IS2F(ISVOBJ)) .EQ. (IPS-1) .AND.
     &           CWDDIA(IS2F(ISVOBJ)) .LE. BP(ISZCLS) .AND.
     &           CWDDIA(IS2F(ISVOBJ)) .GT. BP(ISZCLS-1) ) THEN
              NSVCWD(IPS,ISZCLS) = NSVCWD(IPS,ISZCLS) + 1
            ENDIF
          ENDDO
          NSVCHG(IPS,ISZCLS) = IPCNT - NSVCWD(IPS,ISZCLS)
        ENDDO
      ENDDO

C----------
C  We have the SVS CWD object count changes that need to be applied,
C  by piled status, by sizeclass. Loop through them to first do any
C  object deletions, so that we'll have space available for any
C  subsequent object additions.
C----------

      DO IPS=1,2
        DO ISZCLS=1,4
          IF ( NSVCHG(IPS,ISZCLS) .LT. 0 ) THEN

C----------
C  Too many SVS CWD objects exist for current category.
C  Flag objects for removal in the CWD and SVS arrays.
C----------

            NUM2DEL = NSVCHG(IPS,ISZCLS)*(-1)
            IF (DEBUG) THEN
              WRITE(JOSTND,1070) ISZCLS, BP(ISZCLS-1), BP(ISZCLS),
     &                           NUM2DEL
 1070         FORMAT(/,' ',T5,'DELETING CWD OBJECTS FOR ISZCLS=',
     &                         I1,':',/,
     &                 ' ',T5,'   MIN DIAMETER FOR SIZECLASS=',F7.3,/,
     &                 ' ',T5,'   MAX DIAMETER FOR SIZECLASS=',F7.3,/,
     &                 ' ',T5,'   OBJECT DELETIONS SCHEDULED=',I4)
            ENDIF
            CWDDL1 = 0
            CWDDL2 = 0
            DO 200 ICWD=1,NCWD
              IF ( CWDPIL(ICWD) .EQ. (IPS-1) .AND.
     &             CWDDIA(ICWD) .LE. BP(ISZCLS) .AND.
     &             CWDDIA(ICWD) .GT. BP(ISZCLS-1) ) THEN
                CWDDIA(ICWD) = 0.0
                CWDDL1 = CWDDL1 + 1
                DO ISVOBJ=1,NSVOBJ
                  IF (IOBJTP(ISVOBJ).EQ.4 .AND.
     &                IS2F(ISVOBJ).EQ.ICWD) THEN
                    IOBJTP(ISVOBJ) = 0
                    IS2F(ISVOBJ) = 0
                    CWDDL2 = CWDDL2 + 1
                    EXIT
                  ENDIF
                ENDDO
                NUM2DEL = NUM2DEL - 1
                IF ( NUM2DEL .EQ. 0 ) EXIT
              ENDIF
  200       CONTINUE
            IF (DEBUG) THEN
              WRITE(JOSTND,1080) CWDDL1, CWDDL2
 1080         FORMAT(' ',T5,'   CWD DELETIONS DONE        =',I4,/,
     &               ' ',T5,'   OBJECT DELETIONS DONE     =',I4)
            ENDIF
          ENDIF
        ENDDO
      ENDDO

C----------
C  Compress the SVS object arrays for the CWD objects flagged for
C  removal.
C----------

      IPUT = 0
      DO 250 ISVOBJ=1,NSVOBJ
        IF (IOBJTP(ISVOBJ).EQ.0) THEN
          IF(IPUT.EQ.0) IPUT=ISVOBJ
        ELSE
          IF (IPUT.GT.0 .AND. IPUT.LT.ISVOBJ) THEN
            IOBJTP(IPUT)=IOBJTP(ISVOBJ)
            IS2F(IPUT)=IS2F(ISVOBJ)
            XSLOC(IPUT)=XSLOC(ISVOBJ)
            YSLOC(IPUT)=YSLOC(ISVOBJ)
            IOBJTP(ISVOBJ)=0
            IS2F(ISVOBJ)=0
            IPUT=IPUT+1
          ENDIF
        ENDIF
  250 CONTINUE
      IF (IPUT.GT.0) NSVOBJ=IPUT-1

C----------
C  Compress the CWD arrays for removed CWD.
C  Need to update any associated SVS object pointers at the same time.
C----------

      IPUT = 0
      DO 300 ICWD=1,NCWD
        IF (CWDDIA(ICWD).EQ.0.0) THEN
          IF(IPUT.EQ.0) IPUT=ICWD
        ELSE
          IF (IPUT.GT.0 .AND. IPUT.LT.ICWD) THEN
            CWDPIL(IPUT) = CWDPIL(ICWD)
            CWDDIA(IPUT) = CWDDIA(ICWD)
            CWDLEN(IPUT) = CWDLEN(ICWD)
            CWDDIR(IPUT) = CWDDIR(ICWD)
            CWDDIA(ICWD) = 0
            DO 260 ISVOBJ=1,NSVOBJ
              IF (IOBJTP(ISVOBJ).EQ.4 .AND.
     &           IS2F(ISVOBJ).EQ.ICWD) THEN
                 IS2F(ISVOBJ) = IPUT
                 EXIT
              ENDIF
  260       CONTINUE
            IPUT=IPUT+1
          ENDIF
        ENDIF
  300 CONTINUE
      IF (IPUT.GT.0) NCWD=IPUT-1

C----------
C  All the SVS CWD object deletions are done.
C  Now loop by sizeclass to add any required CWD objects.
C  Loop from largest to smallest sizeclass, unpiled only.
C
C  The orientation of CWD objects will be set using a random number
C  generator. We want the SVS trees/snags to look the same, whether
C  FFE (and therefore CWD generation) is active or not, so use RANN
C  instead of SVRANN, and save the current RANN value, so we don't
C  disturb the base random number sequence. We also need to save the
C  current SVRANN number, since SVGTPT is called to get random CWD
C  point locations, and SVGTPT calls SVRANN.
C----------

      CALL RANNGET(SAVESO)
      CALL SVRANNGET(SAVESVSO)
      IPS = 1
      DO ISZCLS=4,1,-1
        IF ( NSVCHG(IPS,ISZCLS) .GT. 0 ) THEN
          IF (DEBUG) THEN
            WRITE(JOSTND,3070) ISZCLS
 3070       FORMAT(/,' ',T5,'ADDING CWD OBJECTS FOR ISZCLS=',I1)
          ENDIF
C----------
C  New CWD objects are to be added. Set dimensions.
C----------

          SELECT CASE (ISZCLS)
            CASE (1)
              AVGDIA = 2.99
              AVGLEN = AVGDIA * 48.0
            CASE (2)
              AVGDIA = 5.99
              AVGLEN = AVGDIA * 48.0
            CASE (3)
              AVGDIA = 11.99
              AVGLEN = (AVGDIA * 0.5) * (LGRTIO/LGRNUM) * 0.5
            CASE (4)
              AVGDIA = 16.0
              AVGLEN = (AVGDIA * 0.5) * (LGRTIO/LGRNUM) * 0.5
          END SELECT

C----------
C  Check for sufficient space in both the CWD object arrays, and the
C  SVS object arrays. If there's no room--don't add any CWD objects.
C----------

          DO IPC=1,NSVCHG(IPS,ISZCLS)
            IF ( MXCWD .GT. NCWD .AND. MXSVOB .GT. NSVOBJ ) THEN
              NCWD = NCWD + 1
              CWDPIL(NCWD) = IPS - 1
              CWDDIA(NCWD) = AVGDIA
              CWDLEN(NCWD) = AVGLEN / 12.0
              CALL RANN(XX)
              CWDDIR(NCWD) = IFIX(360. *XX +.5)
              NSVOBJ = NSVOBJ + 1
              IS2F(NSVOBJ) = NCWD
              IOBJTP(NSVOBJ) = 4

C----------
C  Call SVGTPT to get point location for new object.
C  Set plot geometry code to ignore point identifications, and
C  scatter CWD over entire grid.
C----------

              IF (IPLGEM.LT.2) THEN
                IF (IMETRIC.EQ.0) THEN
                  CALL SVGTPT(0,0.0,208.7103,0.0,208.7103,X,Y,IMETRIC)
                ELSE
                  CALL SVGTPT(0,0.0,100.0,0.0,100.0,X,Y,IMETRIC)
                ENDIF
              ELSE
                IF (IMETRIC.EQ.0) THEN
                  CALL SVGTPT(2,0.0,117.7522,0.0,2*3.14159,X,Y,IMETRIC)
                ELSE
                  CALL SVGTPT(2,0.0,56.42,0.0,2*3.14159,X,Y,IMETRIC)
                ENDIF
              ENDIF

C----------
C  Check for object overlap--skip. Allow CWD to overlap.
C----------

C>>>>>              CALL SVOBOL(

C----------
C  Place the CWD object at location X,Y
C----------

              XSLOC(NSVOBJ) = X
              YSLOC(NSVOBJ) = Y
            ENDIF
          ENDDO
        ENDIF
      ENDDO

C----------
C  Count up new CWD SVS objects.
C----------

      DO ISZCLS=1,4
        NSVNEW(IPS,ISZCLS) = 0
        DO ISVOBJ=1,MXSVOB
          IF (IOBJTP(ISVOBJ).NE.4) CYCLE  
          IF ( CWDPIL(IS2F(ISVOBJ)) .EQ. (IPS-1) .AND.
     &         CWDDIA(IS2F(ISVOBJ)) .LE. BP(ISZCLS) .AND.
     &         CWDDIA(IS2F(ISVOBJ)) .GT. BP(ISZCLS-1) ) THEN
            NSVNEW(IPS,ISZCLS) = NSVNEW(IPS,ISZCLS) + 1
          ENDIF
        ENDDO
      ENDDO

      IF (DEBUG) THEN
        WRITE(JOSTND,1110) (PCSIZE(IPS,I),I=1,4),
     &                     (PCOUNT(IPS,I),I=1,4),
     &                     (NSVCWD(IPS,I),I=1,4),
     &                     (NSVCHG(IPS,I),I=1,4),
     &                     (NSVNEW(IPS,I),I=1,4)
 1110   FORMAT(/,' ',T5,'TCWD3 ARRAY, UNPILED ONLY:',/,
     &           ' ',T5,' 1-3in   3-6in   6-12in  12+in ',/,
     &           ' ',T5,'------- ------- ------- -------',/,
     &           ' ',T5,F7.3,1X,3(F7.2,1X)'AVERAGE PIECESIZE',/,
     &           ' ',T5,4(F7.1,1X),'EXPECTED PIECECOUNT',/,
     &           ' ',T5,4(I7,1X),'EXISING SVS CWD OBJECTS',/,
     &           ' ',T5,4(I7,1X),'SVS OBJECT CHANGE',/,
     &           ' ',T5,4(I7,1X),'NEW SVS CWD COUNT',/)
C                        XXX.XXX XXX.XXX XXX.XXX XXX.XXX
      ENDIF

C----------
C  A second loop could be placed here to generate SVS objects for piled CWD.
C  That isn't done now, since CWD currently exists in a piled status only
C  for the duration of a burn, after which unburned CWD is immediately
C  returned to the unpiled CWD pools.
C----------

C
      CALL RANNPUT(SAVESO)
      CALL SVRANNPUT(SAVESVSO)
      RETURN
      END

