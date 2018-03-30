C----------
C VOLUME $Id: r10vol.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
!== last modified  06-29-2004
      SUBROUTINE R10VOL(VOLEQ,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,
     >           HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,
     >           BFPFLG,CUPFLG,SPFLG,ERRFLAG)

C*********************************************************************
C*                 LOCAL VARIABLES                                   *
C*********************************************************************

      CHARACTER*10 VOLEQ
      CHARACTER*1 HTTYPE

      INTEGER TLOGS,I,ERRFLAG
      INTEGER CUPFLG,BFPFLG,SPFLG

      REAL MTOPP,MTOPS
      REAL DBHOB,HTTOT,THT,LHT,HT1PRD
      REAL LOGDIA(21,3),LOGLEN(20)
      REAL NOLOGP,NOLOGS,CUBVOL
      REAL VOL(15),LOGVOL(7,20)
      REAL DANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = NOLOGS
      DANUW = REAL(SPFLG)
C     

      DO 15 I = 1, 21
         LOGDIA(I,1)=0.0
         LOGDIA(I,2)=0.0
   15 CONTINUE
      
      DO 20 I=1,20
         LOGVOL(1,I)=0.0
         LOGVOL(2,I)=0.0
         LOGVOL(3,I)=0.0
         LOGVOL(4,I)=0.0
         LOGVOL(5,I)=0.0
         LOGVOL(6,I)=0.0
         LOGVOL(7,I)=0.0
         LOGLEN(I)=0.0
   20 CONTINUE

      IF(MTOPP .LE. 0.0) MTOPP = 6.0
      IF(MTOPS .LE. 0.0) MTOPS = 4.0
      
      IF (HTTYPE.EQ.'L'.OR.HTTYPE.EQ.'l') THEN
         IF (VOLEQ(2:3).EQ.'32'.or.VOLEQ(2:3).EQ.'62') THEN
            LHT = HT1PRD/10.0
         ELSE
            LHT=HT1PRD
         ENDIF
         THT=0.0
      ELSE
         THT=HTTOT
         LHT=0.0
c smaltree check.  If tree is le 40 or dbhob lt 9, small tree logic
         IF (HTTOT.LE.40 .OR. DBHOB.LT.9.0) THEN
C--      USE D SQUARED H EQUATIONS FOR SMALL TREES  
            IF (DBHOB.LE.3.5 .OR. HTTOT.LT.18.0) THEN
               CALL FSTGRO (DBHOB,HTTOT,CUBVOL)
               VOL(1) = ANINT(cubvol*10.0)/10.0
            ELSE
               CALL SECGRO (DBHOB,HTTOT,CUBVOL)
               logvol(4,1) = ANINT(cubvol*10.0)/10.0
               logdia(2,1) = mtopp
               NOLOGP = 1.
               TLOGS = 1
               VOL(1) = ANINT(cubvol*10.0)/10.0
               VOL(4) = ANINT(cubvol*10.0)/10.0
            ENDIF
            IF (VOL(4) .LT. 0.0) THEN
               VOL(4) = 0.0
            ENDIF
            IF (VOL(1) .LT. 0.0) THEN
               VOL(1) = 0.0
            ENDIF
            GO TO 1000
         ENDIF
      ENDIF
    
      CALL R10VOLO(VOLEQ,DBHOB,THT,LHT,HTTYPE,MTOPP,NOLOGP,VOL,
     >                LOGLEN,LOGDIA,logvol,BFPFLG,CUPFLG,ERRFLAG)
      TLOGS = INT(ANINT(NOLOGP))
      
 1000 RETURN
      END 

C**************************************************************
C**************************************************************
      SUBROUTINE FSTGRO(D,H,VN)
C**************************************************************
C
C      WHERE:  
C              D      = DBHOB
C              H      = TOTAL TREE HEIGHT FROM GROUND TO TIP
C              VN     = CUBIC-FOOT VOLUME ESTIMATED BY THIS SUBROUTINE
C--------------
C   **FSTGRO DATE OF LAST REVISION:   11:06:49  06/20/89  
C--------------
C FSTGRO COMPUTES CUBIC-FOOT VOLUMES FOR TREES WITH (D LT 4 OR H LT 18)

      
      REAL D, H, VN, TERM1, TERM2, FORM

      IF(H .LE. 4.5)THEN        !3/22/02  small height check
        VN=0.
        RETURN
      ENDIF

      IF(H.LE.18) THEN
         TERM1 = ((H - 0.9)*(H - 0.9)) / ((H - 4.5)*(H - 4.5))
         TERM2=TERM1*(H - 0.9)/(H - 4.5)
         FORM = 0.406098*TERM1 - 0.0762998*D*TERM2 + 
     >                                      0.00262615*D*H*TERM2
      ELSE
         FORM = 0.480961 + 42.46542/(H*H) - 10.99643*D/(H*H) -
     >                                 0.107809*D/H - 0.00409083*D
      ENDIF
      VN = 0.005454154*FORM*D*D*H
     
      IF(VN.LT.0.0)  VN = 0.0

      RETURN
      END

C**************************************************************
C**************************************************************
      SUBROUTINE SECGRO(D,H,VN)
C**************************************************************
C
C       WHERE     
C                 D      = DBHOB
C                 H      = TOTAL TREE HEIGHT FROM GROUND TO TIP
C                 VN     = CUBIC_FOOT VOLUME ESTIMATED BY THIS SUBROUTINE
C---------------
C     **SECGRO DATE OF LAST REVISION:   11:06:49  06/22/89 
C---------------
C     ENTRY SECGRO COMPUTES VOLUMES FOR SECOND GROWTH TREES
C     (D GE 4 AND H GE 18)  UP TO (D LE 9 AND H LT 40)
C---------------

      REAL D, H, VN

      VN = -5.577 + 1.9067 * ALOG(D) + 0.9416 * ALOG(H)

      IF (VN.GT.0.0) THEN
         VN = EXP(VN)
      ELSE
        VN=0.0
      ENDIF

      RETURN
      END

