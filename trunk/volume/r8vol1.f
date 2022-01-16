C----------
C VOLUME $Id$
C----------
!== last modified  10-26-2005
      SUBROUTINE R8VOL1(VOLEQ,DBHOB,HTONE,HTTWO,PROD,VOL,ERRFLAG)

C  CREATED:   05-28-91
C  PURPOSE:   THIS ROUTINE IS FOR CALCULATING REGION 8 VOLUMES FOR
C             EQUATION NUMBERS 1 THROUGH 799.

     
      IMPLICIT NONE

C  DECLARE VARIABLES

      CHARACTER*2 PROD
C      CHARACTER*4 SPC
C     CHARACTER*8 READIT
      CHARACTER*10 VOLEQ
      !character*19 atmp1(2592)

      INTEGER ERRFLAG,DONEFLAG,LAST,HALF,LASTFLAG
      INTEGER GC1
      INTEGER SPEC,VSPEC,FIRST
      INTEGER COEFEQ(4)

      REAL DBHOB, HTONE, HTTWO,VEQ
      REAL VOL(15)
      !COFARR(162,10)

      include 'R8CLIST.INC'
      include 'R8VLIST.INC'

c      INCLUDE 'COMM0'

c      DATA READIT / ' ' /
c
c      IF (READIT .NE. 'COEFREAD') THEN
c        tmp1 = 10
c        OPEN (TMP1,FILE='R8CLIST')
C       OPEN (TMP1,FILE='R8CLIST',PAD='YES')
c
c        DO 10 I = 1,162
c          READ (TMP1,*) (COFARR(I,J),J=1,10)
c 10     CONTINUE
c
c        CLOSE (TMP1)
c
c        READIT = 'COEFREAD'
c
c        OPEN (TMP1,FILE='R8VLIST',ACCESS='DIRECT',STATUS='OLD',
c     >        FORM='FORMATTED',RECL=21)

c      ENDIF

      READ(VOLEQ(8:10),'(I3)') VSPEC

        if(vspec.eq.268) vspec=261

      IF(VOLEQ(4:6).EQ.'DVE' .OR. VOLEQ(4:6).EQ.'dve')THEN
          READ (VOLEQ(2:3),'(I2)')GC1
      ELSE
          GC1 = 0
      ENDIF
      
      DONEFLAG = 0
      LASTFLAG = 0
      IF (GC1.EQ.0.OR.GC1.GE.80) GO TO 998

      FIRST = ((GC1-1)*81)+2
      LAST = FIRST + 80
      DO 5, WHILE (DONEFLAG.EQ.0)
C  !DID NOT FIND A MATCH
          IF(FIRST.EQ.LAST)LASTFLAG = 1   
C  !DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST   
          READ (ATMP1(HALF),'(I3,4I4)') SPEC,
     >         (COEFEQ(J),J=1,4)
C  !FOUND THE COEFFECIENTS
          IF(VSPEC.EQ.SPEC)THEN      
             DONEFLAG=1
C  !MOVE DOWN THE LIST
          ELSEIF(VSPEC.GT.SPEC)THEN  
             FIRST = HALF
C  !MOVE UP THE LIST
          ELSEIF(VSPEC.LT.SPEC)THEN   
             LAST = HALF - 1
          ENDIF
C  !DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0) GO TO 998   
   5  CONTINUE

C     END BINARY SEARCH
      IF (PROD.EQ.'01') THEN
         VOL(2) = VEQ (DBHOB,HTONE,COFARR(COEFEQ(1),1),
     >      COFARR(COEFEQ(1),2),COFARR(COEFEQ(1),3),
     >      COFARR(COEFEQ(1),4),COFARR(COEFEQ(1),5),
     >      COFARR(COEFEQ(1),6),COFARR(COEFEQ(1),7),
     >      COFARR(COEFEQ(1),8),COFARR(COEFEQ(1),9),
     >      COFARR(COEFEQ(1),10),ERRFLAG)
         IF (VOL(2).LT.10.0) VOL(2) = 10.0
         VOL(3) = VOL(2)

         VOL(4) = VEQ (DBHOB,HTONE,COFARR(COEFEQ(2),1),
     >      COFARR(COEFEQ(2),2),COFARR(COEFEQ(2),3),
     >      COFARR(COEFEQ(2),4),COFARR(COEFEQ(2),5),
     >      COFARR(COEFEQ(2),6),COFARR(COEFEQ(2),7),
     >      COFARR(COEFEQ(2),8),COFARR(COEFEQ(2),9),
     >      COFARR(COEFEQ(2),10),ERRFLAG)
         VOL(5) = VOL (4)

         VOL(7) = VOL(4) *(VEQ (DBHOB,HTONE,COFARR(COEFEQ(4),1),
     >      COFARR(COEFEQ(4),2),COFARR(COEFEQ(4),3),
     >      COFARR(COEFEQ(4),4),COFARR(COEFEQ(4),5),
     >      COFARR(COEFEQ(4),6),COFARR(COEFEQ(4),7),
     >      COFARR(COEFEQ(4),8),COFARR(COEFEQ(4),9),
     >      COFARR(COEFEQ(4),10),ERRFLAG) - 1.0)
        VOL(8) = VOL (7)

      ELSEIF (PROD.EQ.'02') THEN

         VOL(4) = VEQ (DBHOB,HTTWO, COFARR(COEFEQ(3),1),
     >      COFARR(COEFEQ(3),2),COFARR(COEFEQ(3),3),
     >      COFARR(COEFEQ(3),4),COFARR(COEFEQ(3),5),
     >      COFARR(COEFEQ(3),6),COFARR(COEFEQ(3),7),
     >      COFARR(COEFEQ(3),8),COFARR(COEFEQ(3),9),
     >      COFARR(COEFEQ(3),10),ERRFLAG)
         VOL(5) = VOL (4)
         GO TO 999
      ELSE
        GO TO 998
      ENDIF

      GO TO 999

 998  ERRFLAG = 6
 
 999  RETURN
      END


      FUNCTION VEQ (DBHOB,HT,MODEL,B0,B1,B2,B3,B4,B5,B6,B7,B8,ERRFLAG)

C      CHARACTER*2 ST
      INTEGER ERRFLAG
      REAL D, DBHOB, HT, D2H, VTEMP,VEQ
      REAL MODEL,B0,B1,B2,B3,B4,B5,B6,B7,B8

c      INCLUDE 'COMM0'

      VEQ = 0.0

      D2H = DBHOB * DBHOB * HT
      IF (D2H.LE.0) THEN
        ERRFLAG = 5
        GO TO 999
      ENDIF

      VTEMP = 0.0
      D = DBHOB

      IF (MODEL.EQ.1) VTEMP = B0 + B1*D2H
      IF (MODEL.EQ.2) VTEMP = B0 + B1/D + B2*D*HT +
     >                            B3*D2H
      IF (MODEL.EQ.3) VTEMP = B0 + B1*D*D + B2*D*HT*HT +
     >                            B3*D2H + B4*HT*HT*HT
      IF (MODEL.EQ.5) VTEMP = B0 + B1*D + B2*D*D + B3*HT*HT +
     >                            B4*D2H + B5*D*D2H
      IF (MODEL.EQ.6) VTEMP = B0 + B1*D*D +
     >                            B2*HT + B3*D2H
      IF (MODEL.EQ.7) VTEMP = EXP (B0 * (HT ** B1) *
     >         ((1.0 - (4.0 / (0.78 * D)) ** 2.0) ** 2.0) ** B2)
      IF (MODEL.EQ.9) VTEMP = B0 + B1*D2H/1000.0 +
     >         B2*D*HT*HT/1000.0 + B3*D*D + B4*HT*HT*HT/10000.0 +
     >         B5*D*D/HT + B6*D*D*HT*HT/100000.0 +
     >         B7*D2H*HT*HT/100000000 + B8*D*HT/100.0

      VEQ = VTEMP

 999  RETURN
      END
