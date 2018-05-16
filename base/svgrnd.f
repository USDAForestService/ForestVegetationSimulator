      SUBROUTINE SVGRND (NOUT,KYLAST,KYFRST,IFIREFLG)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C     SVS BASIC GROUND DEFINITION GENERATION
C     D.L.GAMMEL    -- SEM         -- JUNE 2002
C     D. ROBINSON   -- ESSA        -- MAY 2005
C     S.SCHAROSCH   -- Abacus      -- MAR 2008
C
C     INPUT:
C     NOUT - THE OUTPUT FILE REFERENCE NUMBER
C     KYFRST - THE INDEX FOR THE FIRST CHAR NEEDED FROM
C              FROM THE KEYWORD FOR GENERATING THE FILE NAME
C     KYLAST - THE INDEX OF THE LAST CHAR NEEDED FOR THE
C              FILE NAME
C     IFIREFLG- THE IDENTIFIER OF THE CALLING CONTEXT
C               0=INVENTORY TIME
C               1=START OF CYCLE
C               2=AFTER CUTS
C               3=END OF PROJECTION (CALLED FROM MAIN)
C               4=CALLED FROM FIRE MODEL, INCLUDES FLAMES
C               5=CALLED FROM FIRE MODEL, NO FLAMES
C
C     BASIC GROUND COLOR CODES (SET VIA SVS KEYWORD FIELD 5):
C     ICOLIDX  COLOR SCHEME      COLOR CODES
C              BLACK               0
C              BLUE                1
C              GREEN               2
C              CYAN                3
C              RED                 4
C              MAGENTA             5
C              BROWN               6
C              GRAY                7
C              DARK GRAY           8
C              LIGHT BLUE          9
C              LIGHT GREEN        10
C              LIGHT CYAN         11
C              LIGHT RED          12
C              LIGHT MAGENTA      13
C              YELLOW             14
C              WHITE              15
C        1     REDS/ORANGES       16 to 27
C        2     TANS               28 to 39
C        3     REDDISH BROWNS     40 to 51
C        4     LT GREYS           52 to 63
C        5     LIME GREENS        64 to 75
C        6     DARK GREENS        76 to 87
C        7     MEDIUM BLUEGREENS  88 to 99
C        8     DARK GREENS       100 to 111
C        9     DARK GREYS        112 to 123
C       10     LIGHT BROWNS      124 to 135
C       11     KHAKI/BROWNS      136 to 147
C       12     LIGHT GREENS      148 to 159
C       13     DARK BLUEGREENS   160 to 171
C       14     CHOCOLATE BROWNS  172 to 183
C       15     DARK TANS         184 to 195
C       16     YELLOWS           196 to 207
C       17     MEDIUM GREYS      208 to 219
C       18     BROWNS            220 to 231
C       19     GREENS            232 to 243
C       20     DARK BROWNS       244 to 255

COMMONS

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'CONTRL.F77'

COMMONS

      CHARACTER*256 GRIDLINE
      CHARACTER*100  FILENAME
      CHARACTER*30 COLORCODE
      INTEGER ICOLOR,GRNDOUT,NOUT,I,J,X,Y,INTCOLOR,KYFRST,KYLAST,KODE,
     >        IFIREFLG
      LOGICAL LMONO
      REAL    K,FLINE,FLNY

      DIMENSION FLINE(256)
      DIMENSION FLNY(20)

      ICOLOR = ICOLIDX*12+4
      GRNDOUT = 222
      COLORCODE = "ABCDEFGHIJ0123456789QRSTUVWXYZ"

C     If the base color set was input as a negative value,
C     interpret it as a request to generate a monochrome ground
C     surface of the indicated color (so CWD objects are more visible)

      IF ( ICOLIDX .LT. 0 ) THEN
        LMONO = .TRUE.
        ICOLIDX = (-1) * ICOLIDX
        ICOLOR = ICOLIDX*12+14        
      ELSE
        LMONO = .FALSE.
      ENDIF

C     CREATE THE GROUND FILE NAME

      IF(IFIREFLG.GT.3) THEN
        IF (NIMAGE.LT.1000) THEN
          WRITE(FILENAME,10)KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX,NIMAGE
   10     FORMAT(A,'_g',I3.3,'_c',I3.3,'_',I3.3,'.grd')
        ELSE
          WRITE(FILENAME,20)KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX,NIMAGE
   20     FORMAT(A,'_g',I3.3,'_c',I3.3,'_',I6.6,'.grd')
        ENDIF
      ELSE
        WRITE(FILENAME,'(A,''_g'',I3.3,''_c'',I3.3,''.grd'')')
     >        KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX
      ENDIF

C     ADD GROUNDFILE LINE TO SVS FILE

      WRITE(NOUT, '(''#GROUNDFILE '',A/)') TRIM(FILENAME)

C     IF THIS IS NOT A FIRE FILE OR THE BEGINNING OF THE INVENTORY
C     THEN WE ARE FINISHED

      IF(IFIREFLG.GE.1.AND.IFIREFLG.LE.3) RETURN

C     TRY OPENING FILE WITH PATH NAME INCLUDED

      CALL MYOPEN(GRNDOUT,TRIM(KWDFIL(:KYLAST)//'/'//FILENAME),
     >  5,120,0,1,1,0,KODE)

C     IF OPEN FAILS THEN TRY OPENING FILE W/OUT THE PATH

      IF(KODE.GT.0) THEN
        CALL MYOPEN(GRNDOUT,TRIM(FILENAME),5,120,0,1,1,0,KODE)
        IF(KODE.GT.0) THEN
         IGRID=0
         RETURN
        ENDIF
      ENDIF

C     PRINT OUT THE BASE COLOR SET
C     The base color set is comprised of:
C       1) User-specified color scheme from the SVS keyword
C       2) Colors 16-25 (red/orange): used for red fire line
C       3) Colors 113-122 (dark greys): used for black fire line

      WRITE(GRNDOUT, '(2I5)') IGRID, IGRID
      IF ( LMONO ) THEN
        DO I = 1, 10
          WRITE(GRNDOUT,30) COLORCODE(I:I),(ICOLOR)
        ENDDO
      ELSE
        DO I = 1, 10
          WRITE(GRNDOUT,30) COLORCODE(I:I),(ICOLOR+I-0)
        ENDDO
   30   FORMAT(A1,I3)
      ENDIF

      DO I = 11, 20
        WRITE(GRNDOUT,30) COLORCODE(I:I),(I+5)
      ENDDO
      DO I = 21, 30
        WRITE(GRNDOUT,30) COLORCODE(I:I),(I+92)
      ENDDO
      WRITE(GRNDOUT, '(''* 7'')')

C     IF THERE IS A FIRE THEN GRAB THE FIRE LINE

      IF(IFIREFLG.EQ.4) THEN
        CALL FMGETFL(20, FLNY)
        DO I = 1,IGRID
          J = INT(I*20/IGRID+1)
          IF(J.GT.20) J = 20

          IF (IMETRIC.EQ.0) THEN
            FLINE(I)=IGRID - INT((FLNY(J) / 208.7 * IGRID) + 0.5)
          ELSE
            FLINE(I)=IGRID - INT((FLNY(J) / 100.0 * IGRID) + 0.5)
          ENDIF

        ENDDO
      ENDIF

C     RANDOMLY GENERATE THE GROUND FILE

      DO Y = 1, IGRID
        DO X = 1, IGRID
          CALL SVRANN(K)

C         IF THERE IS A FIRE THEN GENERATE THE FIRE EFFECTS

          IF(IFIREFLG.EQ.4) THEN
C           CREATE RED LINE
            IF(Y.GT.FLINE(X).AND.Y.LE.(FLINE(X)+(IGRID/10))) THEN
              INTCOLOR = INT(K * 10) + 11
C           CREATE RED OR BLACK LINE
            ELSEIF(Y.GT.(FLINE(X)+IGRID/10).AND.
     >             Y.LE.(FLINE(X)+IGRID/5)) THEN
              INTCOLOR = INT(K * 20) + 11
C           CREATE BLACK LINE
            ELSEIF(Y.GT.(FLINE(X)+IGRID/5)) THEN
              INTCOLOR = INT(K * 10) + 21
            ELSE
              INTCOLOR = INT(K * 10) + 1
            ENDIF

C         NO FIRE

          ELSEIF(IFIREFLG.EQ.5) THEN
C           AFTER A FIRE
            INTCOLOR = INT(K * 10) + 21
          ELSE
C           NO FIRE EFFECTS
            INTCOLOR = INT(K * 10) + 1
          ENDIF
          GRIDLINE(X:X)=COLORCODE(INTCOLOR:INTCOLOR)
        ENDDO
        WRITE(GRNDOUT, '(A)') GRIDLINE(:IGRID)
      ENDDO
      CLOSE(GRNDOUT)
      END

