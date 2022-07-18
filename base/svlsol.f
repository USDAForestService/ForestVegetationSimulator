      SUBROUTINE SVLSOL (XJ1,XJ2,XK1,XK2,XS,KODE)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C
C     RETURN KODE = 1 IF THE 2 SEGMENTS OF THE SAME LINE OVERLAP
C     RETURN KODE = 0 IF THE 2 SEGMENTS OF THE SAME LINE DO NOT OVERLAP
C
C     RETURN XS IS ONE POINT WITHIN THE OVERLAPING SEGMENTS, UNDEFINED
C     (SET TO ZERO) IF THE SEGMENTS DO NOT OVERLAP
C
      INTEGER KODE
      REAL XJ1,XJ2,XK1,XK2,XS

      KODE = 1
C
C     FIND THE LONGEST LINE.
C
      IF (ABS(XJ2-XJ1) .GT. ABS(XK2-XK1)) THEN
C
C        LINE J IS LONGEST.
C
         IF (XJ1.LT.XJ2) THEN
            XS = XK1
            IF (XJ1.LE.XK1 .AND. XK1.LE.XJ2) RETURN
            XS = XK2
            IF (XJ1.LE.XK2 .AND. XK2.LE.XJ2) RETURN
         ELSE
            XS = XK1
            IF (XJ2.LE.XK1 .AND. XK1.LE.XJ1) RETURN
            XS = XK2
            IF (XJ2.LE.XK2 .AND. XK2.LE.XJ1) RETURN
         ENDIF
      ELSE
C
C        LINE K IS LONGEST.
C     
         IF (XK1.LT.XK2) THEN
            XS = XJ1
            IF (XK1.LE.XJ1 .AND. XJ1.LE.XK2) RETURN
            XS = XJ2
            IF (XK1.LE.XJ2 .AND. XJ2.LE.XK2) RETURN
         ELSE
            XS = XJ1
            IF (XK2.LE.XJ1 .AND. XJ1.LE.XK1) RETURN
            XS = XJ2
            IF (XK2.LE.XJ2 .AND. XJ2.LE.XK1) RETURN
         ENDIF
      ENDIF
      XS = 0.
      KODE = 0
      RETURN
      END
