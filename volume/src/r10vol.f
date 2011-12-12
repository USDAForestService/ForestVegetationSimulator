!== last modified  03-29-2004
      SUBROUTINE R10VOL(VOLEQ,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,
     >           HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,
     >           BFPFLG,CUPFLG,SPFLG,ERRFLAG)

C*********************************************************************
C*                 LOCAL VARIABLES                                   *
C*********************************************************************

      CHARACTER*10 VOLEQ,tmpveq
      CHARACTER*1 HTTYPE

      INTEGER TLOGS,I,ERRFLAG
      INTEGER CUPFLG,BFPFLG,SPFLG

      REAL MTOPP,MTOPS,tmpht2,holdv(2),holdlv(3,20)
      REAL DBHOB,HTTOT,THT,LHT,HT1PRD
      REAL LOGDIA(21,3),LOGLEN(20)
      REAL NOLOGP,NOLOGS,CUBVOL
      REAL VOL(15),LOGVOL(7,20)
     
      DO 10 I=1,15
          VOL(I) = 0.0
   10 CONTINUE

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
c small tree check.  If tree is le 40 or dbhob lt 9, small tree logic
         IF (HTTOT.LE.40 .OR. DBHOB.LT.9.0) THEN
C--         USE D SQUARED H EQUATIONS FOR SMALL TREES  
            IF (DBHOB.LE.3.5 .OR. HTTOT.LT.18.0) THEN
               CALL FSTGRO (DBHOB,HTTOT,CUBVOL)
               VOL(1) = CUBVOL
            ELSE
               CALL SECGRO (DBHOB,HTTOT,CUBVOL)
               logvol(4,1) = cubvol
               logdia(2,1) = mtopp
               NOLOGP = 1.
               TLOGS = 1
               VOL(1) = CUBVOL
               VOL(4) = CUBVOL
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
     
      IF (VOLEQ(1:3).EQ.'A01'.OR.VOLEQ(1:3).EQ.'A02' .or.
     >     VOLEQ(1:3).EQ.'a01'.OR.VOLEQ(1:3).EQ.'a02' ) THEN

         CALL R10VOLO(VOLEQ,DBHOB,THT,LHT,HTTYPE,MTOPP,NOLOGP,VOL,
     >                LOGLEN,LOGDIA,logvol,BFPFLG,CUPFLG,ERRFLAG)
         TLOGS = ANINT(NOLOGP)
      ELSE
         IF(voleq(2:3).eq.'16'.or.voleq(2:3).eq.'32') then
c                                                 no change in logic
            CALL R10VOL1(VOLEQ,MTOPP,MTOPS,THT,LHT,DBHOB,
     >                  HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,
     >                  LOGVOL,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
C**********************************************************************
c new logic for 16' cubic with 32' boards
         ELSEIF (voleq(2:3).eq.'61') then
c                            if ht is measured in 16 foot logs (equ A61)
c                            find board foot volume - 32' foot
            TMPHT2 = LHT/2.0
            tmpveq = 'A32'//voleq(4:10)
            CALL R10VOL1(TMPVEQ,MTOPP,MTOPS,THT,TMPHT2,DBHOB,
     >                  HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,
     >                  LOGVOL,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
           holdv(1) = vol(2)
           holdv(2) = vol(3)
           do 158, i=1,20
             holdlv(1,i) = logvol(1,i)
             holdlv(2,i) = logvol(2,i)
             holdlv(3,i) = logvol(3,i)
  158      continue
C                            find cubic volume - 16' foot
           tmpveq = 'A16'//voleq(4:10)
           CALL R10VOL1(TMPVEQ,MTOPP,MTOPS,THT,LHT,DBHOB,
     >                  HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,
     >                  LOGVOL,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
           vol(2) = holdv(1)
           vol(3) = holdv(2)
           do 157,i=1,20
             logvol(1,i) = holdlv(1,i)
             logvol(2,i) = holdlv(2,i)
             logvol(3,i) = holdlv(3,i)
  157      continue               
         ELSEIF (voleq(2:3).eq.'62') then
c                            if ht is measured in 32 foot logs (equ A62)
c                            find board foot volume - 32' foot
            tmpveq = 'A32'//voleq(4:10)
            CALL R10VOL1(TMPVEQ,MTOPP,MTOPS,THT,LHT,DBHOB,
     >                  HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,
     >                  LOGVOL,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
           holdv(1) = vol(2)
           holdv(2) = vol(3)
           do 168, i=1,20
             holdlv(1,i) = logvol(1,i)
             holdlv(2,i) = logvol(2,i)
             holdlv(3,i) = logvol(3,i)
  168      continue
C                            find cubic volume - 16' foot
           tmpht2 = lht*2
           tmpveq = 'A16'//voleq(4:10)
           CALL R10VOL1(TMPVEQ,MTOPP,MTOPS,THT,TMPHT2,DBHOB,
     >                  HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,
     >                  LOGVOL,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
           vol(2) = holdv(1)
           vol(3) = holdv(2)
           do 167,i=1,20
             logvol(1,i) = holdlv(1,i)
             logvol(2,i) = holdlv(2,i)
             logvol(3,i) = holdlv(3,i)
  167      continue               
         else
           do 200, i=1,15
              vol(i) = 0.0
  200      continue
           errflag = 1
         ENDIF        
      ENDIF
      
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

