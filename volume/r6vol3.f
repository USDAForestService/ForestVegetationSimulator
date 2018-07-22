!== last modified  01-11-2013
C 01/11/2013 added calculation for stump vol.
c***********************************************************************
C----------
C VOLUME $Id$
C----------
c***********************************************************************
      SUBROUTINE R6VOL3(DBHOB,DBTBH,FCLASS,HTTOT,ZONE,VOL)
c***********************************************************************
      REAL DBHOB, DBTBH,HTTOT,VOL(15),DBHIB,HTUP,HRATIO,S,DR
      REAL HX,H
      INTEGER FCLASS,I,ZONE
      REAL D(21),LOGVOL,D17,TOPD,A,B,H17

      DO 25, I=1,20
          D(I) = 0.0
  25  CONTINUE

      TOPD = 4.0
      D17 = FCLASS/100.0 *DBHOB
      IF(ZONE .EQ. 1) H17 = 17.3
      IF(ZONE .EQ. 2) H17 = 33.6
      DBHIB = DBHOB-DBTBH
      IF(DBHIB.LE.0 .OR. DBHIB.GT.DBHOB) DBHIB = DBHOB
      
      IF(DBHIB .LT. TOPD)THEN
C          SMAILIANS FOR WHOLE TREE
         VOL(1)=0.00272708*(DBHIB*DBHIB)*HTTOT
      ELSE IF(D17 .LT. TOPD)THEN
C        SMALL TREE - USE D17 FOR BUTT LOG VOLUME
         LOGVOL = 0.00272708*(DBHIB*DBHIB + D17*D17)*H17
         VOL(1) = LOGVOL
C        FIND VOLUME TO TOP OF TREE
         LOGVOL = 0.00272708*(D17*D17)*(HTTOT-H17)
         VOL(1) = VOL(1) + LOGVOL
      ELSE
         A=0.62
         B = 1.0 - A
C        FIND CUBIC VOLUME FOR BUTT LOG
         LOGVOL = 0.00272708*(DBHIB*DBHIB + D17*D17)*H17
         VOL(1) = LOGVOL
C        FIND DIAMETER FOR UPPER STEMS
         HTUP = HTTOT - H17
         D(1) = D17
         DO 90 I=2,20
            HRATIO = (HTUP - ((I-1) * 16.3))/HTUP
            IF (HRATIO.LE.0.0) GO TO 100
            
            DR = HRATIO / (A*HRATIO+B)
            D(I) = DR * D(1)
               
            IF (D(I).LT.TOPD)GO TO 100
C           FIND VOLUME FOR THE SECTION
            LOGVOL = 0.00272708*(D(I-1)*D(I-1) + D(I)*D(I))*16.3
            VOL(1) = VOL(1) + LOGVOL
            IF(D(I) .EQ.TOPD) THEN
               S = 16.3
               GO TO 130
            ENDIF
   90   CONTINUE

  100   DR = TOPD / D17
        HX = (DR*B*HTUP) / (1.0 - (A*DR))
        H = (I-2) * 16.3
        S = HTUP - HX - H
        LOGVOL = 0.00272708*(D(I-1)*D(I-1) + TOPD*TOPD)*S
        VOL(1) = VOL(1) + LOGVOL
C       FIND VOLUME FOR TOP OF TREE
  130   HTUP = HTTOT - (16.3 * (I-2) + H17) - S
        LOGVOL = 0.00272708*(TOPD*TOPD)*HTUP
        VOL(1) = VOL(1) + LOGVOL
C       calculate stump volume
        VOL(14)=0.005454154*DBHIB**2          
      ENDIF

      RETURN
      END
