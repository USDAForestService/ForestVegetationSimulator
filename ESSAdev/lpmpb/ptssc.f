      SUBROUTINE PTSSC(IPLTNO)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C     LIBRARY ROUTINE TO SET SCALES
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
C
COMMONS
C
C
      INCLUDE 'PT.F77'
C
C
COMMONS
C
      INTEGER IVR,CASE,FNSH1
      INTEGER LOG,TRUE,FALSE
      INTEGER IPLTNO,START,FINSH,J
      REAL  AMAX, AMIN, DVLN(3), DVLN2(3), PTBNO, PTSRG, RANGE,
     &      RT, T, TEMP, UPFCT(3), VR, MXFCT(3)


      EXTERNAL PTBNO
C
C        DIVIDING LINE DATA
      DATA  DVLN/2.0,0.5,0.0/
      DATA  DVLN2/3.,1.,.33333/
      DATA MXFCT/1.3333,2.,4./
      DATA UPFCT/.75,.5,.25/
      DATA  TRUE/-1/, FALSE/0/
C
      START=1
C
C     DETERMINE GROUP WITH SAME SCALES
   1  FINSH=START
    4 IF(IPTCOD(IPLTNO,FINSH) .NE. 100) GO TO 3
      FINSH=FINSH+1
      GO TO 4
    3 CONTINUE
C             HERE START IS BEGINNING OF GROUP
C                     FINSH  END OF GROUP
C
      IF(IPTCOD(IPLTNO,FINSH) .EQ. 11) GO TO 5
C                     IPTCOD=011  BOTH UPPER/LOWER USER SPEC
C
C------------USER HAS NOT SPECIFIED BOTH SCALES
C
C             GET MAX OF MAX, MIN OF MINS FOR THIS GROUP
      AMAX=-1E35
      AMIN=+1E35
      DO 10 J=START,FINSH
      IF(PTMAX(IPLTNO,J) .GT. AMAX) AMAX=PTMAX(IPLTNO,J)
      IF(PTMIN(IPLTNO,J) .LT. AMIN) AMIN=PTMIN(IPLTNO,J)
   10 CONTINUE
C
C             WHAT SCALES NEED BE CACULATED
      IF(IPTCOD(IPLTNO,FINSH) .EQ. 0) GO TO 100
      IF(IPTCOD(IPLTNO,FINSH) .EQ. 10) GO TO 200
        GO TO 300
C
C------------SET BOTH SCALES-----------------
  100 IF(AMIN .LE. .999*AMAX) GO TO 110
C
C        AMAX EQUALS AMIN
              IF(AMAX .NE. 0.) GO TO 101
                      PTL(IPLTNO,FINSH)=0.
                      PTU(IPLTNO,FINSH)=1.
                      GO TO 5
C
  101 IF(AMAX .LE. 0.) GO TO 102
                      PTL(IPLTNO,FINSH)=0.
                      GO TO 200
C
  102                 PTU(IPLTNO,FINSH)=0.
                      GO TO 300
C
C        AMAX GREATER THAN AMIN
 110          IF(AMIN .LT. 0. .AND. AMAX .GT. 0.) GO TO 120
C
C     AMAX,AMIN THE SAME SIGN
      LOG=FALSE
              IF(AMAX .GT. 0.) GO TO 111
                      LOG=TRUE
                      TEMP=AMAX
                      AMAX=-AMIN
                      AMIN=-TEMP
  111 IF(AMAX .LE. 2.*AMIN) GO TO 113
C
C     CHOOSE 0. AS LOWER SCALE
              PTL(IPLTNO,FINSH)=0.
              PTU(IPLTNO,FINSH)=PTSRG(AMAX,PTBNO(AMAX))
              GO TO 112
C
C     CHOOSE SCALES WITH 0. OFF SCALE
  113 T=PTBNO(AMAX-AMIN)
              IVR=AMAX/T
              VR=IVR
              TEMP=VR*T
              IF(TEMP .LE. AMAX) TEMP=TEMP+T
              PTL(IPLTNO,FINSH)=TEMP-PTSRG(TEMP-AMIN,T)
              PTU(IPLTNO,FINSH)=TEMP
C
  112         IF(LOG .EQ. FALSE) GO TO 5
                      TEMP=PTU(IPLTNO,FINSH)
                      PTU(IPLTNO,FINSH)=-PTL(IPLTNO,FINSH)
                      PTL(IPLTNO,FINSH)=-TEMP
                      GO TO 5
C
C     AMAX,AMIN SIGNS DIFFER
  120 RT=-AMAX/AMIN
              DO 130 CASE=1,3
              IF(RT .GT. DVLN(CASE)) GO TO 140
  130         CONTINUE
  140 J=4-CASE
      IF(RT .LE. DVLN2(CASE)) GO TO 141
              T=AMAX*MXFCT(CASE)
              GO TO 142
  141 T=-AMIN*MXFCT(J)
  142 TEMP=PTSRG(T,PTBNO(T))
              PTU(IPLTNO,FINSH)=UPFCT(CASE)*TEMP
              PTL(IPLTNO,FINSH)=-UPFCT(J)*TEMP
              GO TO 5
C
C------------USER SPECIFIED LOWER SCALE, SET UPPER
  200 IF(AMAX.GT. 0. .OR. AMAX.LT. .50*PTL(IPLTNO,FINSH))GO TO 201
C
C             CHOOSE 0. AS UPPER SCALE
              PTU(IPLTNO,FINSH)=0.
              GO TO 5
C
  201 RANGE=AMAX-PTL(IPLTNO,FINSH)
C
C             CHOOSE UPPER SCALE AS REQUIRED
      PTU(IPLTNO,FINSH)=PTL(IPLTNO,FINSH)+PTSRG(RANGE,PTBNO(RANGE))
              GO TO 5
C
C--------USER SPECIFIED UPPER SCALE, SET LOWER--------
  300 IF(AMIN.LT.0. .OR. AMIN.GT..50*PTU(IPLTNO,FINSH))GO TO 301
C
C             CHOOSE 0. AS LOWER SCALE
              PTL(IPLTNO,FINSH)=0.
              GO TO 5
C
  301 RANGE=PTU(IPLTNO,FINSH)-AMIN
C
C             CHOOSE LOWER SCALE AS REQUIRED
      PTL(IPLTNO,FINSH)=PTU(IPLTNO,FINSH)-PTSRG(RANGE,PTBNO(RANGE))
C
C-----STORE SCALES
    5 IF(FINSH .EQ. START) GO TO 8
      FNSH1=FINSH-1
      DO 7 J=START,FNSH1
      PTU(IPLTNO,J)=PTU(IPLTNO,FINSH)
    7 PTL(IPLTNO,J)=PTL(IPLTNO,FINSH)
C
C             HAVE ALL SCALES BEEN SET
    8 IF(FINSH .EQ. IPTVAR(IPLTNO)) RETURN
      START=FINSH+1
      GO TO 1
      END
