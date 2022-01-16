C----------
C ORGANON $Id$
C----------
      SUBROUTINE VOLCAL(VERSION,SPP,CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,
     1                  LOGTA,DBH,HT,CR,VERROR,TERROR,VWARNING,
     2                  TWARNING,IERROR,CFVOL,BFVOL)
C
C  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
C               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
C               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
C               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.
C
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - EDIT TO EDIT_VOL
C  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SPGROUP TO SPGROUP_VOL
C
      IMPLICIT NONE
      INTEGER*4   VERSION,IERROR,SPP
      INTEGER*4   SPGRP,VERROR(5),TERROR(4),VWARNING(5),TWARNING,
     1            ILOGLL,VERS,SVOL
      REAL*4      CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,LOGTA,DBH,HT,CR,
     1            XLOGTA,CFVOL,BFVOL
      LOGICAL*2   ERROR
      IF(CFTD .LT. 0.0) CFTD=0.0
      IF(CFSH .LT. 0.0) CFSH=0.0
      IF(LOGTD .LE. 0.0) LOGTD=6.0
      IF(LOGSH .LE. 0.0) LOGSH=0.5
      IF(LOGTA .LE. 0.0) LOGTA=8.0
      IF(LOGLL .LE. 0.0) LOGLL=32.0
      IF(LOGML .LE. 0.0) LOGML=8.0
      VERS=VERSION
      IF(VERS .LT. 1 .OR. VERS .GT. 4) VERS=1
      CALL EDIT_VOL(VERS,SPP,CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,LOGTA,
     1         DBH,HT,CR,SPGRP,VERROR,TERROR,VWARNING,TWARNING,ERROR)
      IERROR =   0
      IF(ERROR) THEN
         IERROR = 1
         RETURN
      ENDIF
C
C        CALCULATE CF VOLUME FOR TREE
C
      CALL CF(VERS,SPP,SPGRP,DBH,HT,CR,CFTD,CFSH,CFVOL)
      SVOL=1
      ILOGLL=INT(LOGLL)
      XLOGTA=LOGTA/12.0
      CALL SCRIB_VOL(VERS,SPP,SPGRP,SVOL,ILOGLL,LOGTD,LOGSH,XLOGTA,
     1           LOGML,DBH,HT,CR,BFVOL)
      RETURN
      END
C***********************************************************************
      SUBROUTINE EDIT_VOL(VERS,SPP,CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,
     1                LOGTA,DBH,HT,CR,SPGRP,VERROR,TERROR,VWARNING,
     2                TWARNING,ERROR)
      IMPLICIT NONE
      INTEGER*4 VERS,SPP
      INTEGER*4 I,SPGRP,VERROR(5),TERROR(4),VWARNING(5),TWARNING,TEMP
      REAL*4 CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,LOGTA,DBH,HT,CR
      REAL*4 B0,B1,PHT
      LOGICAL*2 ERROR
      ERROR = .FALSE.
      DO I=1,4
         TERROR(I)=0
      ENDDO
      TWARNING=0
      DO I=1,5
         VERROR(I)=0
         VWARNING(I)=0
      ENDDO
C
C     EDIT VOLUME SPECIFICATIONS FOR ERRORS
C
      IF(CFSH .GT. 4.5) THEN
         VERROR(1)=1
      ENDIF
      IF(LOGML .GT. LOGLL) THEN
         VERROR(2)=1
      ENDIF
      IF(LOGTD .GT. 0.0 .AND. LOGTD .LT. 1.0) THEN
         VERROR(3)=1
      ENDIF
      IF(LOGSH .GT. 4.5) THEN
         VERROR(4)=1
      ENDIF
      IF(LOGTA .GT. 0.0 .AND. LOGTA .LT. 1.0) THEN
         VERROR(5)=1
      ENDIF
      DO I=1,5
         IF(VERROR(I) .EQ. 1) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDDO
C
C     EDIT TREE ATTRIBUTES FOR ERRORS
C
      CALL CKSP(VERS,SPP,TEMP)
      TERROR(1)=TEMP
      IF(DBH .LE. 0.09)THEN
         TERROR(2) = 1
      ENDIF
      IF(HT .LE. 4.5)THEN
         TERROR(3) = 1
      ENDIF
      IF(CR .LT. 0.0 .OR. CR .GT. 1.0)THEN
         TERROR(4) = 1
      ENDIF
      DO I=1,4
         IF(TERROR(I) .EQ. 1) THEN
            ERROR = .TRUE.
            RETURN
         ENDIF
      ENDDO
      CALL SPGROUP_VOL(VERS,SPP,SPGRP)
C
C     DETERMINE WARNINGS (IF ANY)
C
C
C     EDIT VOLUME SPECIFICATIONS FOR ERRORS
C
      IF(CFTD .GT. 12.0) THEN
         VWARNING(1)=1
      ENDIF
      IF(LOGLL .LT. 8.0 .OR. LOGLL .GT. 40.0) THEN
         VWARNING(2)=1
      ENDIF
      IF(LOGML .LT. 8.0 .OR. LOGML .GT. 40.0) THEN
         VWARNING(3)=1
      ENDIF
      IF(LOGTD .GT. 12.0) THEN
         VWARNING(4)=1
      ENDIF
      IF(LOGTA .GT. 12.0) THEN
         VWARNING(5)=1
      ENDIF
      B1=-0.04484724
      SELECT CASE(SPP)
        CASE(202)
           B0=19.04942539
        CASE(263)
           IF(VERS .GE. 2)B0=19.04942539
        CASE(17,15)
           B0=16.26279948
        CASE(122)
           B0=17.11482201
        CASE(117)
           B0=14.29011403
        CASE DEFAULT
           B0=15.80319194
      ENDSELECT
      IF(HT .GT. 4.5) THEN
         PHT=4.5+B0*DBH/(1.0-B1*DBH)
         IF(HT .GT. PHT) THEN
            TWARNING = 1
         ENDIF
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE SPGROUP_VOL(VERSION,SPECIES,SPGRP)
C     DETERMINE SPECIES GROUP THE TREE
C
C     I = TREE INDICATOR
C
C
      IMPLICIT NONE
      INTEGER*4 VERSION,SPECIES,SPGRP,ISX,J
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4  SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
C
      INTEGER*4   SCODE3(7)/
     1           351,202,263,242,312,492,920/
      ISX = -9999
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES .EQ. SCODE1(J))THEN
                ISX = J
                IF(ISX .GT.2) ISX=ISX-1
                EXIT
             ENDIF
           ENDDO
        CASE(2,3)
           DO J = 1, 11
              IF(SPECIES .EQ. SCODE2(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
        CASE(4)
           DO J = 1, 7
              IF(SPECIES .EQ. SCODE3(J))THEN
                 ISX = J
                 EXIT
             ENDIF
           ENDDO
      ENDSELECT
      SPGRP=ISX
      RETURN
      END
C***********************************************************************
      SUBROUTINE CKSP(VERSION,SPECIES,TEMP)
C     CHECK SPECIES CODE FOR THE TREE
C
      IMPLICIT NONE
      INTEGER*4 VERSION,SPECIES,J,TEMP
      LOGICAL*2 BAD
C
      INTEGER*4  SCODE1(19)/
     1           202,15,17,122,117,81,263,242,231,361,431,631,805,312,
     2           815,818,351,492,920/
C
      INTEGER*4   SCODE2(11)/
     1           202,17,263,242,231,361,312,815,351,492,920/
C
      INTEGER*4   SCODE3(7)/
     1           351,202,263,242,312,492,920/
      BAD=.TRUE.
      TEMP=0
      SELECT CASE (VERSION)
        CASE(1)
           DO J = 1, 19
             IF(SPECIES .EQ. SCODE1(J))THEN
                BAD =.FALSE.
                EXIT
             ENDIF
           ENDDO
        CASE(2,3)
           DO J = 1, 11
              IF(SPECIES .EQ. SCODE2(J))THEN
                 BAD =.FALSE.
             ENDIF
           ENDDO
        CASE(4)
           DO J = 1, 7
              IF(SPECIES .EQ. SCODE3(J))THEN
                 BAD =.FALSE.
             ENDIF
           ENDDO
      ENDSELECT
      IF(BAD) THEN
         TEMP = 1
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE GET_ORGVOL_EDITION(EDITION)
      REAL*4 EDITION
      EDITION=9.1
      RETURN
      END
C***********************************************************************
      SUBROUTINE LOG_TABLE(VERSION,NTREES,XLOGLL,LOGML,LOGTD,LOGSH,
     1                     XLOGTA,SPP,DBH,XHT,XCR,EXPAN,NL,LVOL)
      IMPLICIT NONE
      REAL*4  XLOGLL,LOGML,LOGTD,LOGSH,XLOGTA,DBH(2000),XHT(2000),
     1        XCR(2000),EXPAN(2000)
      REAL*4  NL(40,4),LVOL(40,4),H,WLT,PP1,PP2,PDIB,DI,X,TOTS(2,4),TLL,
     1        DOB,HT,CR,HCB,LOGTA
      INTEGER*4 VERSION,NTREES,SPP(2000)
      INTEGER*4 ISP,ISPGRP,LOGLL
      INTEGER*4 II,SVOL,I,J,NW,NDI,NLOGTD
      REAL*4    EX,MH,AA1,AA2,A3,A4,A,B,C,V,ALP,D,ROOT,HM1,HM2
C
      LOGTA=XLOGTA/12.0
      LOGLL=INT(XLOGLL)
      DO I=1,40
         DO J=1,4
            NL(I,J)=0.
            LVOL(I,J)=0.
         ENDDO
      ENDDO
C
      DO I=1,NTREES
C
C        GET LOG TABLE SPECIES GROUP AND EXP FACTOR
C
         IF(DBH(I) .LE. LOGTD) CYCLE
      IF(VERSION .LE. 3) THEN
         IF(SPP(I) .LE. 300)THEN
           IF(SPP(I) .EQ. 202)THEN
             SVOL=1
           ELSE IF(SPP(I) .EQ. 15 .OR. SPP(I) .EQ. 17)THEN
             SVOL=2
           ELSE IF((SPP(I) .EQ. 117 .OR. SPP(I) .EQ. 122)
     1              .AND. VERSION .EQ. 1)THEN
             SVOL=3
           ELSE IF(SPP(I) .EQ. 263 .AND. VERSION .GE. 2)THEN
             SVOL=3
           ELSE
             SVOL=4
           ENDIF
         ELSE
           CYCLE
         ENDIF
      ELSE
         IF(SPP(I) .LE. 300 .OR. SPP(I) .EQ. 351)THEN
           IF(SPP(I) .EQ. 351)THEN
             SVOL=1
           ELSE IF(SPP(I) .EQ. 202)THEN
             SVOL=2
           ELSE IF(SPP(I) .EQ. 263)THEN
             SVOL=3
           ELSE
             SVOL=4
           ENDIF
         ELSE
           CYCLE
         ENDIF
      ENDIF
C
         DOB=DBH(I)
         HT=XHT(I)
         CR=XCR(I)
         EX=EXPAN(I)
         ISP=SPP(I)
         CALL SPGROUP_VOL(VERSION,ISP,ISPGRP)
         X=HT-4.5
         HCB=(1.0-CR)*HT
C

         SELECT CASE(VERSION)
            CASE(1)                   ! Southwest Oregon
               CALL SWO_DIB(ISPGRP,DOB,CR,PDIB)
               CALL SWO_TAPER(ISPGRP,AA1,AA2,A3,A4,ALP)
            CASE(2,3)                   ! Northwest Oregon, SMC
               CALL NWO_DIB(ISPGRP,DOB,CR,PDIB)
               CALL NWO_TAPER(ISPGRP,AA1,AA2,A3,A4,ALP)
            CASE(4)                   ! Red Alder Plantations
               CALL RAP_DIB(ISPGRP,DOB,PDIB)
               CALL RAP_TAPER(ISPGRP,AA1,AA2,A3,A4,ALP)
         ENDSELECT
         WLT=(ALP*HCB-4.5)/X
         PP1=AA1+AA2*(X/DOB)+A3*(X/DOB)**2
         PP2=A4
         IF(PDIB .LE. LOGTD) CYCLE
C
C        COMPUTE MERCHANTABLE HEIGHT
C
         IF(VERSION .EQ. 4 .AND. ISP .EQ. 351) THEN
            CALL RA_MH(DOB,HT,CR,LOGTD,MH)
            GO TO 21
         ENDIF
         D=0.0
         H=ALP*HCB
         CALL LOGVOL(1,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,DI,V,
     1               NL,LVOL,TOTS)
         IF(WLT .LE. 0.0)THEN
            A=-(PP1+1.0)/X**2
            B=PP1/X
            C=1.0-LOGTD/PDIB
         ELSE IF(WLT .GT. 0. .AND. DI .LE. LOGTD)THEN
            A=PP2/X**2
            B=PP1/X
            C=1.0-LOGTD/PDIB
         ELSE
            A=(PP2*WLT**2-PP1-2*PP2*WLT-1.0)/(X**2*(WLT-1.0)**2)
            B=((2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)-(PP2*WLT**2-PP1-
     1         2*PP2*WLT-1.0))/(X*(WLT-1.0)**2)
            C=-(LOGTD/PDIB+(2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)/
     1         (WLT-1.0)**2)
         ENDIF
         ROOT=B**2-4*A*C
         IF(ROOT .LT. 0.0)THEN
            MH=0.
            GO TO 21
         ENDIF
         HM1=(-B+SQRT(ROOT))/(2*A)
         HM2=(-B-SQRT(ROOT))/(2*A)
C
C        CHECK MERCHANTABLE LOG DIAMETERS
C
         IF(HM1 .GT. 0.0) THEN
            D=1.0
            H=HM1+4.5

            CALL LOGVOL(4,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,DI,
     1                  V,NL,LVOL,TOTS)
            NDI=NINT(DI*10.0)
            NLOGTD=NINT(LOGTD*10.0)
            IF(NDI .NE. NLOGTD) HM1=0.0
         ELSE
            HM1=0.0
         ENDIF
         IF(HM2 .GT. 0.0) THEN
            D=1.0
            H=HM2+4.5
            CALL LOGVOL(4,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,DI,
     1                  V,NL,LVOL,TOTS)
            NDI=NINT(DI*10.0)
            NLOGTD=NINT(LOGTD*10.0)
            IF(NDI .NE. NLOGTD) HM2=0.0
         ELSE
            HM2=0.0
         ENDIF
         IF(HM1 .LT. 0. .OR. HM1 .GT.X)THEN
            IF(HM2 .LT. 0. .OR. HM2 .GT.X)THEN
               MH=0.
               GO TO 21
            ELSE
               MH=HM2
            ENDIF
         ELSE IF(HM2 .LT. 0. .OR. HM2 .GT.X)THEN
            MH=HM1
         ELSE
            MH=MAX(HM1,HM2)
         ENDIF
         MH=MH+4.5
   21    CONTINUE
C
C        CALCULATE LOG VOLUMES
C
         NW=INT((MH-LOGSH)/(FLOAT(LOGLL)+LOGTA))
         IF(NW .LT. 0) NW=0
         TLL=MH-LOGSH-FLOAT(NW)*(FLOAT(LOGLL)+LOGTA)
C
         H=LOGSH
         DO II=1,NW
            D=1.0
            H=H+FLOAT(LOGLL)+LOGTA
            IF(VERSION .EQ. 4 .AND. ISP .EQ. 351) THEN
               CALL RA_LOGVOL(2,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,V,NL,
     1                        LVOL,TOTS)
            ELSE
               CALL LOGVOL(2,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,
     1                     DI,V,NL,LVOL,TOTS)
            ENDIF
         ENDDO
C
C        COMPUTE VOLUME OF TOP LOG
C
         IF(TLL .GE. (LOGML+LOGTA))THEN
            J=INT(TLL-LOGTA)
            TLL=FLOAT(J)+LOGTA
            D=TLL/FLOAT(LOGLL)
            H=H+TLL
            IF(VERSION .EQ. 4 .AND. ISP .EQ. 351) THEN
               CALL RA_LOGVOL(2,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,V,NL,
     1                        LVOL,TOTS)
            ELSE
               CALL LOGVOL(2,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,
     1                     DI,V,NL,LVOL,TOTS)
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
