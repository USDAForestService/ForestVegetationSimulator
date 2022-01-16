      SUBROUTINE FMSVTREE (NOUT,ISVOBJ)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     FIRE MODEL STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- JANUARY 2000
C     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
C     D.GAMMEL      -- SEM MOSCOW  -- JUNE 2002
C     D. ROBINSON   -- ESSA        -- MAY 2005
C
C     INPUT:
C     NOUT  =THE OUTPUT FILE REFERENCE NUMBER
C     ISVOBJ=THE OBJECT BEING PROCESSED
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'FMPARM.F77'

      INCLUDE 'FMCOM.F77'

      INCLUDE 'FMFCOM.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'SVDEAD.F77'

      INCLUDE 'FMSVCM.F77'

      INCLUDE 'METRIC.F77'

COMMONS

      EXTERNAL SVRANN
      INTEGER  ISVOBJ,NOUT,II,NFLMS,IFLR0T,IMAXFLAMES
      REAL     FLMX,FLMY,X,CALC,BASECRWN,HALFCRWN,Z,FLMZ,
     &         TILTBASE,FLMTILT,BACHLO,FLMHT,FMWDTH,FLMWDTH,
     &         MIDCRWN,FLMCALC,FLMCRAWL

      II = IFIX( XSLOC(ISVOBJ) / FLPART ) + 1

      IF (YSLOC(ISVOBJ).LT.FMY2(II)) THEN

C       PRELIMINARY CALCS

        BASECRWN = ((100. - CRNRTO(IS2F(ISVOBJ))) / 100.)
     >    * OLEN(IS2F(ISVOBJ))
        HALFCRWN = ((CRNRTO(IS2F(ISVOBJ)) / 100.) *
     >    OLEN(IS2F(ISVOBJ))) / 2.

        MIDCRWN =  BASECRWN + HALFCRWN
        FLMCRAWL = 0.0
        CALC = 0.0

        IF (IFMTYP .EQ. 1) THEN

C         THIS IS A CROWNING FIRE
C         DO SOME PRELIMINARY CROWN FIRE CALCS

          IF (YSLOC(ISVOBJ).GT.(FMY2(II)-150)) THEN
            IF (YSLOC(ISVOBJ).GT.(FMY2(II)-15.)) THEN
              CALC = 0.75
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-30.)) THEN
              CALC = 0.8
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-45.)) THEN
              CALC = 0.9
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-60.)) THEN
              CALC = 1.0
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-75.)) THEN
              CALC = 0.9
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-90.)) THEN
              CALC = 0.8
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-105.)) THEN
              CALC = 0.75
            ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-120.)) THEN
              CALC = 0.1
            ELSE
              CALC = 0.
            ENDIF

            FLMCRAWL = 3. * (HALFCRWN*2/OLEN(IS2F(ISVOBJ))) + 1.5
          ELSE
             CALC = 0.
          ENDIF
        ELSEIF (IFMTYP .EQ. 2) THEN

C         TORCHING FIRE

C         PRELIMINARY CALCULATIONS

          IF (YSLOC(ISVOBJ).GT.(FMY2(II)-15.)) THEN
            CALC = 0.25
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-30.)) THEN
            CALC = 0.50
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-45.)) THEN
            CALC = 0.75
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-60.)) THEN
            CALC = 1.0
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-75.)) THEN
            CALC = 0.75
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-90.)) THEN
            CALC = 0.50
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-105.)) THEN
            CALC = 0.25
          ELSEIF (YSLOC(ISVOBJ).GT.(FMY2(II)-120.)) THEN
            CALC = 0.1
          ELSE
            CALC = 0.
          ENDIF

          FLMCRAWL = 4. * (HALFCRWN*2/OLEN(IS2F(ISVOBJ))) + 2.

C         FOR 80% OF THE TREES, DO NOT ALLOW THE FLAMES INTO
C         THE CROWN OF THE TREE IF THE GROUND FIRE CAN NOT
C         REACH IT.

          IF((BASECRWN).GT.(FLAMEHT * 1.2)) THEN
            CALL SVRANN(X)
            IF (X.LT.0.8) THEN
              CALC = 0.
            ENDIF
          ENDIF
        ENDIF

C       DETERMINE X,Y,Z FOR FLAME

        CALL SVRANN(Z)
        FLMZ = (Z * HALFCRWN) + BASECRWN
        FLMX = XSLOC(ISVOBJ)
        FLMY = YSLOC(ISVOBJ)

C       CREATE THE FLAMES

        IMAXFLAMES = 0
        NFLMS = 0
        DO WHILE(IMAXFLAMES .EQ. 0)
          NFLMS = NFLMS + 1

          FLMZ = FLMZ + FLMCRAWL
          TILTBASE = MAX(MIN(FWIND*.5,40.),5.)
          FLMTILT = MAX(BACHLO(TILTBASE, 5., SVRANN),0.)
          FLMCALC = (OLEN(IS2F(ISVOBJ)) - FLMZ) * 0.65

          IF(IMETRIC.EQ.0) THEN
            FLMHT = BACHLO( FLMCALC, 0.5, SVRANN )
          ELSE
            FLMHT = BACHLO( FLMCALC, 0.5*FTtoM, SVRANN )
          ENDIF

          CALL SVRANN( FMWDTH )
          FLMWDTH = CRNDIA(IS2F(ISVOBJ)) * (1 -
     >      ((FLMZ - BASECRWN)/(HALFCRWN*2))) * CALC
          CALL SVRANN(X)
          IFLR0T=INT(X*90. + 270.)

          IF (FLMHT.GT.0. .AND. FLMWDTH.GT.0) THEN

            IF (IMETRIC.EQ.0) THEN
              WRITE (NOUT,30) NFLMS+NSVOBJ,
     >          FLMHT,FLMTILT,IFLR0T,FLMWDTH,FLMX,FLMY,FLMZ
            ELSE
              WRITE (NOUT,30) NFLMS+NSVOBJ,
     >          FLMHT*FTtoM,FLMTILT,IFLR0T,FLMWDTH*FTtoM,
     >          FLMX,FLMY,FLMZ*FTtoM
            ENDIF

   30       FORMAT ('@flame.eob',T16,I5,' 99 0 1 0',2F6.0,I5,
     >              ' 0',F6.1,' 0 0 0 0 0 0 0 1 0',3F8.2)
          ENDIF
          IF((FLMHT + FLMZ).GE.(OLEN(IS2F(ISVOBJ))*CALC)) THEN
            IMAXFLAMES = 1
          ENDIF
        ENDDO
      ENDIF
      END
