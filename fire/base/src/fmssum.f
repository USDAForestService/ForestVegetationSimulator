      SUBROUTINE FMSSUM (IYR)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C  Purpose:
C     Reports a summary of snag statistics for all years that
C     coinside with a FVS cycle boundary.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
      INTEGER I, II, JOUT, K
      REAL    THD(7),TSF(7),THDSF
      INTEGER IYR, DBSKODE

      IF (ISNGSM .EQ. -1 .OR. IYR.NE.IFMYR1) RETURN

      DO I=1,7
         THD(I)=0.
         TSF(I)=0.
      ENDDO
      THDSF=0.

      DO II=1,NSNAG
         TSF(7)=TSF(7)+DENIS(II)
         IF (HARD(II)) THEN
            THD(7)=THD(7)+DENIH(II)
         ELSE
            TSF(7)=TSF(7)+DENIH(II)
         ENDIF

         DO I=1,6
            IF (DBHS(II).GE.SNPRCL(I)) THEN
               TSF(I)=TSF(I)+DENIS(II)
               IF (HARD(II)) THEN
                  THD(I)=THD(I)+DENIH(II)
               ELSE
                  TSF(I)=TSF(I)+DENIH(II)  ! adding DENIH is CORRECT
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      THDSF=THD(7)+TSF(7)

C
C     CALL THE DBS MODULE TO OUTPUT SUMMARY SNAG REPORT TO A DATABASE
C
      DBSKODE = 1
      CALL DBSFMSSNAG(IYR,NPLT,THD(1),THD(2),THD(3),THD(4),THD(5),
     &  THD(6),THD(7),TSF(1),TSF(2),TSF(3),TSF(4),TSF(5),TSF(6),TSF(7),
     &  THDSF,DBSKODE)
      IF(DBSKODE.EQ.0) RETURN

      CALL GETLUN (JOUT)

      IF (ISNGSM .EQ. 0) THEN
         CALL GETID (ISNGSM)
         WRITE (JOUT,10) ISNGSM,NPLT,MGMID,
     >                   ((INT(SNPRCL(I)),I=1,6),K=1,2)
 10      FORMAT (/I6,' $#*%'//46('-'),' SNAG SUMMARY REPORT '
     >        '(BASED ON STOCKABLE AREA) ',21('-')/,
     >        ' STAND ID: ',A26,4X,'MGMT ID: ',A4/
     >        7X,15('-'),' HARD SNAGS/ACRE ',16('-'),
     >        2X,15('-'),' SOFT SNAGS/ACRE ',16('-'),'   GRAND'/
     >        'YEAR  ',2(1X,6(' >=',I2.2,'" '),' TOTAL '),'  TOTAL'/
     >        '---- ',2(1X,7(' ------')),'  ------'/
     >        '$#*%')
      ENDIF

      WRITE (JOUT,20) ISNGSM,IYR,(THD(I),I=1,7),(TSF(I),I=1,7),THDSF
 20   FORMAT (1X,I5,1X,I4,1X,2(1X,7(1X,F6.1)),2X,F6.1)

      RETURN
      END
