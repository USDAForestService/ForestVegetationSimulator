      FUNCTION ADJMAI(INSPEC,SINDEX,POINTS,IERROR)
      IMPLICIT NONE
C----------
C  **ADJMAI--SO   DATE OF LAST REVISION:  04/24/08
C----------
C
C         THIS FUNCTION COMPUTES THE ADJUSTED MAI FOR AN NFI PLOT USING
C         THREE DATA ITEMS FROM THE NFI PLOT SUMMARY FILE...
C         THE INPUT ITEMS ARE INSPEC, THE SITE SPECIES CODE,
C                             SINDEX, THE SITE INDEX,
C                             POINTS, THE NUMBER OF STOCKABLE POINTS.
C         THE VARIABLE IERROR RETURNS A VALUE OF 1 IF A SPECIES
C         CODE IS UNACCEPTABLE. OTHERWISE, IERROR IS SET TO ZERO.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C
C
      INTEGER IMAP(32),ISP(32),IERROR,INSPEC,NCALLS,I,INUM
      REAL POINTS,SINDEX,ADJMAI
C
C              I------ EQUATION GROUP 1 ------I------- GROUP 2 -------I
      DATA ISP/263,019,242,264,098,081,041,042,108,109,101,120,124,103,
C
C     I-3-I---- 4 ----I-- 5 --I-6-I-- 7 --I-------- GROUP 9 ------I----
     & 119,122,116,117,201,202,073,092,093,011,015,017,020,021,022,211,
C
C     - 10 ---I
     & 212,213/
      DATA IMAP/8*1,6*2,3,3*4,2*5,6,2*7,6*9,3*10/
C
C
      NCALLS=1
      IERROR = 0
      ADJMAI = 0.0
C
C   IF SPECIES CODE IS > 300 THEN SPECIES IS HARDWOOD. GO TO GROUP 8.
C
      IF(INSPEC.GE.300) GO TO 80
C
C      CHECK FOR A LEGAL SPECIES CODE...
C
      DO 2 I=1,32
         IF(INSPEC.EQ.ISP(I))GO TO 9
    2 CONTINUE
C
      WRITE (JOSTND,3) INSPEC,NCALLS
    3 FORMAT(/,' ******** WARNING:  ILLEGAL SITE INDEX SPECIES (',I3,
     >       ') WAS DETECTED FOR RECORD NUMBER ',I5,'.'/T21,
     >       'NO ADJUSTED MAI WAS CALCULATED.')
      IERROR = 1
      RETURN
C
C        LEGAL SPECIES FOUND - FIND EQUATION GROUP USING COMPUTED GOTO
C        STATEMENT
C
    9 CONTINUE
      INUM=IMAP(I)
C
      GO TO (10,20,30,40,50,60,70,80,90,100),INUM
C
C================================================================
C                   GROUP 1...
  10  CONTINUE
      IF(SINDEX.LT.33.0) RETURN
      ADJMAI = -63.689706 + (1.9402941 * SINDEX)
      GO TO 200
C================================================================
C                   GROUP 2...
   20 CONTINUE
      IF(SINDEX.LT.11.0) RETURN
      ADJMAI = -12.0388 + (1.18672 * SINDEX)
      GO TO 200
C================================================================
C                   GROUP 3...
   30 CONTINUE
      ADJMAI = 5.972615 + (1.857675 * SINDEX)
      GO TO 200
C================================================================
C                   GROUP 4...
   40 CONTINUE
      ADJMAI=2.305357  + 0.033890056 * SINDEX + 0.0090108543 * SINDEX**2
      GO TO 200
C================================================================
C                   GROUP 5...
   50 CONTINUE
      IF(SINDEX.LT.29.0) RETURN
      ADJMAI = -10.303313 + .032929911 * SINDEX + .012207163 * SINDEX**2
     &               + (-.00003543129 * SINDEX**3)
      GO TO 200
C================================================================
C                   GROUP 6...
   60 CONTINUE
      IF(SINDEX.LT.11.0) RETURN
      ADJMAI = -6.0892857 + .45178571 * SINDEX + .014464286 * SINDEX**2
      GO TO 200
C================================================================
C                   GROUP 7...
   70 CONTINUE
      IF(SINDEX.LT.10.0) RETURN
      ADJMAI = -18.4 + 1.92 * SINDEX
      GO TO 200
C================================================================
C                   GROUP 8 ...
   80 CONTINUE
      IF(SINDEX.LT.32.0) RETURN
      ADJMAI = -53.892857 + 1.7178571 * SINDEX
      GO TO 200
C================================================================
C                   GROUP 9...
   90 CONTINUE
      ADJMAI = -4.89001 + 311.29546 * ((EXP( (SINDEX / 170.0 - 1.0)**3
     &                                    / 0.343) - 0.055) / 0.95)
      GO TO 200
C================================================================
C                    GROUP 10 ...
  100 CONTINUE
      IF(SINDEX.LT.62.0) RETURN
      ADJMAI = 157.94643 - 1.78125 * SINDEX + .014330357 * SINDEX**2
  200 CONTINUE
      ADJMAI = ADJMAI * POINTS/10.0
      IF(ADJMAI.LT.0.0) ADJMAI = 0.0
      RETURN
      END
