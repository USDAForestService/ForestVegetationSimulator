      SUBROUTINE FMEVMON
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     CALLED FROM: EVLDX
C
C  Purpose:
C     This subroutine brings into one place the procedures to summarize
C     information for the Event Monitor. The first entry point is for
C     coarse woody debris; the second is for snags.
C

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.

      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      

C.... Variable declarations.

      INTEGER I,J,K,M,IX,JX,KX,ILO,IHI,II,III
      REAL    RVAL, XLDBH, XHDBH, XLHT, XHHT, XDBH
      REAL    XTYPE,XBMOUT,XREM
      REAL    XS, XH, X, X1
      INTEGER IRC,IGRP,IULIM,IG
      REAL    TPA,H,VT,D,HS
      REAL    XD, XHT, XM1, HTD


      INTEGER ITYP,IPART,ISTAND,IT
      INTEGER KSP,ISZ,IDC,ITM,ISPC,ISPS
      REAL    SNGSTM,SNGCRN,SNBBA
      REAL    TOTBA,TOTSBA,SNBAIH,SNBAIS,SNVIH,SNVIS
      REAL    LIVSTM,LIVSRM,LIVSLV,LIVCRN,LIVFOL,TOTFOL,TOTSTD,LIVCRM
      REAL    SNGSRM,LIVSRMTEM
      REAL    TOTSTND,TOTSNG,TOTLIV,TOTFUL,TOTDEAD,LVCRNRMFOL
      INTEGER IFIRST
      LOGICAL LINCL,DEBUG,LMERCH
      REAL    SNGVOL1,SNGSALVOL
      REAL    CRREM(MAXTRE),DSNG1(MAXTRE),SSNG1(MAXTRE)
      INTEGER IJ
C
C********************************************************************
C
C     EVENT MONITOR POTFLEN FUNCTION.
C
C********************************************************************

      ENTRY FMEVFLM(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         SELECT CASE (II)
         CASE (1)
           RVAL = PFLAM(1) ! Total severe flame length
         CASE (2)
           RVAL = PFLAM(3) ! Total moderate flame length
         CASE (3)
           RVAL = PFLAM(2) ! Surface severe flame length
         CASE (4)
           RVAL = PFLAM(4) ! Surface moderate flame length
         END SELECT
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR POTFMORT FUNCTION.
C
C********************************************************************

      ENTRY FMEVMRT(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         SELECT CASE (II)
         CASE (1)
           RVAL = POTKIL(1)*100 ! potential severe fire mortality (%BA)
         CASE (2)
           RVAL = POTKIL(3)*100 ! pot. moderate fire mortality (%BA)
         CASE (3)
           RVAL = POTVOL(1) ! potential severe fire mortality (tot cuft)
         CASE (4)
           RVAL = POTVOL(2) ! pot. moderate fire mortality (tot cuft)
         END SELECT
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR FUELMODS FUNCTION.
C
C********************************************************************

      ENTRY FMEVFMD(RVAL, II, III, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         IF (III .EQ. 1) THEN
           RVAL = 0
           SELECT CASE (II)
           CASE (1)
             IF (FWT(1) .GT. 0) RVAL = FMOD(1) ! first fuel model
           CASE (2)
             IF (FWT(2) .GT. 0) RVAL = FMOD(2) ! second fuel model
           CASE (3)
             IF (FWT(3) .GT. 0) RVAL = FMOD(3) ! third fuel model
           CASE (4)
             IF (FWT(4) .GT. 0) RVAL = FMOD(4) ! fourth fuel model
           END SELECT
         ELSEIF (III .EQ. 2) THEN
           SELECT CASE (II)
           CASE (1)
             RVAL = FWT(1) ! first fuel model weight
           CASE (2)
             RVAL = FWT(2) ! second fuel model weight
           CASE (3)
             RVAL = FWT(3) ! third fuel model weight
           CASE (4)
             RVAL = FWT(4) ! fourth fuel model weight
           END SELECT
         ENDIF
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR FUELLOAD FUNCTION. ILO AND IHI MARK THE LOWER AND
C     UPPER INDICES FOR THE 2ND ELEMENT OF THE CWD ARRAY. SUMMATION
C     IS CARRIED OUT OVER ALL THE OTHER DIMENSIONS.
C
C********************************************************************

      ENTRY FMEVCWD(RVAL, ILO, IHI, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         RVAL = 0.0
         DO I = 1, 2
           DO J = ILO, IHI
             DO K = 1, 2
               DO M = 1, 4
                 RVAL = RVAL + CWD(I,J,K,M)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
      ENDIF

      RETURN

C********************************************************************
C
C     EVENT MONITOR SNAGS FUNCTION.
C
C     RVAL  VALUE RETURNED AFTER EVALUATING THE SNAG LIST
C     IX    INDEX FOR KIND OF RESULT: 1=TPA; 2=BA; 3=VOL
C     JX    SPECIES INDEX; <0=SPECIES GROUP; 0=ALL; 1...MAXSP
C     KX    HARD INDEX:    0=ALL; 1=HARD; 2=SOFT
C
C********************************************************************

      ENTRY FMEVSNG(RVAL, IX, JX, KX, XLDBH, XHDBH, XLHT, XHHT, IRC)
      IRC= 0
      XH = 0.
      XS = 0.
      RVAL = 0.
      IF (NSNAG.LT.1) RETURN

      DO 500 I = 1, NSNAG

        ISPS = SPS(I)
        D = DBHS(I)

        LINCL = .FALSE.
        IF(JX.EQ.0 .OR. JX.EQ.ISPS)THEN
          LINCL = .TRUE.
        ELSEIF(JX.LT.0)THEN
          IGRP = -JX
          IULIM = ISPGRP(IGRP,1)+1
          DO 90 IG=2,IULIM
          IF(ISPS .EQ. ISPGRP(IGRP,IG))THEN
            LINCL = .TRUE.
            GO TO 91
          ENDIF
   90     CONTINUE
        ENDIF
   91   CONTINUE

        IF (LINCL .AND.
     >    (D.GE.XLDBH .AND. D.LT.XHDBH)) THEN

C  PASS OVER THE INITIALLY-HARD SNAGS

          HS = HTIH(I)
          TPA = DENIH(I)
          IF (TPA .GT. 0. .AND. HS.GE.XLHT .AND. HS.LT.XHHT) THEN
            GOTO (211,212,213), IX
  211       CONTINUE
            X = TPA
            GOTO 120
  212       CONTINUE
            X = TPA * D * D * 0.005454154
            GOTO 120
  213       CONTINUE
            CALL FMSVOL(I,HS,X1,.FALSE.,0)
            X = TPA * X1
            GOTO 120
  120       CONTINUE

            IF (HARD(I)) THEN
              XH = XH + X
            ELSE
              XS = XS + X
            ENDIF
          ENDIF

C     PASS OVER THE INITIALLY-SOFT SNAGS

          HS = HTIS(I)
          TPA = DENIS(I)
          IF (TPA .GT. 0. .AND. HS.GE.XLHT .AND. HS.LT.XHHT) THEN
            GOTO (311,312,313), IX
  311       CONTINUE
            X = TPA
            GOTO 220
  312       CONTINUE
            X = TPA * D * D * 0.005454154
            GOTO 220
  313       CONTINUE
            CALL FMSVOL(I,HS,X1,.FALSE.,0)
            X = TPA * X1
            GOTO 220
  220       CONTINUE

            XS = XS + X
          ENDIF
        ENDIF

  500 CONTINUE

C TAKE HARD-COMPONENT, SOFT-COMPONENT, OR BOTH

      IF (KX .EQ. 1) THEN
        RVAL = XH
      ELSEIF (KX .EQ. 2) THEN
        RVAL = XS
      ELSE
        RVAL = XH + XS
      ENDIF

      RETURN
C********************************************************************
C
C     EVENT MONITOR SALVVOL FUNCTION.
C     JX    SPECIES INDEX; <0=SPECIES GROUP; 0=ALL; 1...MAXSP
C     XLDBH THE LOWEST DBH VALUE
C     XHDBH THE HIGHEST DBH VALUE
C
C********************************************************************

      ENTRY FMEVSAL(RVAL, JX, XLDBH, XHDBH, IRC)
      IRC= 0
      XH = 0.
      XS = 0.
      RVAL = 0.
      IF (NSNAGSALV.LT.1) RETURN

      DO 600 I = 1, NSNAGSALV

        ISPS = SPSSALV(I)
        XD = DBHSSALV(I)
        HTD=HTDEADSALV(I)
C
        LINCL = .FALSE.
        IF(JX.EQ.0 .OR. JX.EQ.ISPS)THEN
          LINCL = .TRUE.
        ELSEIF(JX.LT.0)THEN
          IGRP = -JX
          IULIM = ISPGRP(IGRP,1)+1
          DO 92 IG=2,IULIM
          IF(ISPS .EQ. ISPGRP(IGRP,IG))THEN
            LINCL = .TRUE.
            GO TO 93
          ENDIF
   92     CONTINUE
        ENDIF
   93   CONTINUE

        IF (LINCL .AND.
     >    (XD.GE.XLDBH .AND. XD.LT.XHDBH)) THEN

C  PASS OVER THE INITIALLY-HARD SNAGS

          HS = HTIHSALV(I)
          TPA = SALVSPA(I,1)
          IF (TPA .GT. 0.) THEN
            CALL FMSVL2(ISPS,XD,HTD,HS,X1,.FALSE.,DEBUG,JOSTND)
            X = TPA * X1
            IF (HARDSALV(I)) THEN
              XH = XH + X
            ELSE
              XS = XS + X
            ENDIF
          ENDIF

C     PASS OVER THE INITIALLY-SOFT SNAGS

          HS = HTISSALV(I)
          TPA = SALVSPA(I,2)
          IF (TPA .GT. 0. ) THEN
             CALL FMSVL2(ISPS,XD,HTD,HS,X1,.FALSE.,DEBUG,JOSTND)
            X = TPA * X1
            XS = XS + X
          ENDIF
        ENDIF
C        
  600 CONTINUE
C
      RVAL = XH + XS
C
      RETURN
C********************************************************************
C
C     EVENT MONITOR POTFTYPE FUNCTION.
C
C********************************************************************

      ENTRY FMEVTYP(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         SELECT CASE (II)
         CASE (1)
           RVAL = POTTYP(1) ! potential fire type for severe fires
         CASE (2)
           RVAL = POTTYP(2) ! potential fire type for moderate fires
         END SELECT
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR TREEBIO FUNCTION.
C
C     RVAL   VALUE RETURNED AFTER EVALUATING THE SNAG DATA
C     ISTAND TRREE STATUS -  STANDING(<0), REMOVED(0),OR BOTH(>0)
C     ITYP   TREE TYPE -  DEAD(<0), LIVE(0),OR BOTH(>0)
C     IPART  TREE PART -  STEM(<0), CROWN(0),OR BOTH-WHOLE TREE(>0)
C     JX     SPECIES INDEX; <0=SPECIES GROUP; 0=ALL; 1...MAXSP
C     XLDBH  LOWER RANGE DBH
C     XHDBH  UPPER RANGE DBH
C     XLHT   LOWER RANGE HEIGHT
C     XHHT   UPPER RANGE HEIGHT
C----------
C     LOCAL VARIABLES
C     SNGSTM   - SNAG STEM WEIGHT (TONS)
C     SNGCRN   - SNAG CROWN WEIGHT (TONS)
C
C********************************************************************
      ENTRY FMTREM(DSNG1,SSNG1,CRREM)
C
C     CHECK TO SEE IF THE FIRE MODEL IS ACTIVE
C
      IF (.NOT. LFMON) RETURN
      ITRNL=ITRN
      DO J=1,MAXTRE
      PREMST(J)=0.
      PREMCR(J)=0.
      DBHC(J) = 0.
      HTC(J) = 0.
      ISPCC(J)=0
      DO IJ=0,5
      CROWNWC(J,IJ)=0.
      ENDDO
      ENDDO
C
C  STORES THE FRACTION OF THE TREE RECORD REMOVED FROM THE STAND
C  CALLED FROM **CUTS**
C
      LREMT=.TRUE.
      ICYCRM= ICYC
C
      DO I=1,ITRNL
      PREMST(I)=AMAX1(0.,WK3(I)-SSNG1(I)-DSNG1(I))
C
C  LIVE REMOVED CROWN MATERIAL IS STORED IN PREMCR ARRAY
C  ASSUME THAT ALL DEAD CROWN MATERIAL IS LEFT IN STAND
C
      PREMCR(I)=CRREM(I)
C
      ISPCC(I) = ISP(I)
      DBHC(I) = DBH(I)
      HTC(I) = HT(I)
C
      DO J=0,5
      CROWNWC(I,J)=CROWNW(I,J)
      ENDDO
C
      IF(PREMCR(I).LT.0.00001)PREMCR(I)=0.
      IF(PREMST(I).LT.0.00001)PREMST(I)=0.
C
C      IF(DEBUG)WRITE(JOSTND,*)' ENTERING FMTREM,CYCLE=  ICYC,ITRNL,',
C     &'I,ISPC,DBH,DSNG1,SSNG1,CRREM,FMPROB(I),PREMST(I),',
C     &'PREMCR(I)= ',ICYC,ITRNL,I,ISP(I),DBH(I),DSNG1(I),
C     &SSNG1(I),CRREM(I),FMPROB(I),PREMST(I),PREMCR(I)
C
      ENDDO
C
      RETURN
C*********************************************************************
C
C  CALCULATES THE TREE BIOMASS EVENT MONITOR VARIABLE
C
      ENTRY FMEVTBM(RVAL,ISTAND,ITYP,IPART,JX,XLDBH,XHDBH,XLHT,XHHT,IRC)
C
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
        IRC=0
        RVAL = 0.0
        ITRNC=ITRN
C
C  IF THIS IS NOT A THINING CYCLE, SET LREMT TO FALSE SO REMOVALS ARE
C  NOT ACCOUNTED FOR UNTIL NEXT THINING, AND ZERO OUT PREMSTAND
C  PREMCR ARRAYS.
C

        IF((ICYCRM.NE.ICYC).OR.(.NOT.LREMT))THEN
          DO I=1,MAXTRE
          PREMST(I)=0.
          PREMCR(I)=0.
          ENDDO
        ENDIF
C
        CALL DBCHK (DEBUG,'FMEVMON',7,ICYC)
        IF (DEBUG) WRITE(JOSTND,7) ICYC,LFMON,NSNAG,ICYCRM
    7   FORMAT(' ENTERING FMEVMON CYC = ',I2,' LFMON= ',L2,
     &  ' NSNAG=',I4,' ICYCRM= ',I2)
        IF(DEBUG)WRITE(JOSTND,*)' RVAL,ISTAND,ITYP,IPART,JX,XLDBH,',
     &  'XHDBH,XLHT,XHHT,IRC= ',RVAL,ISTAND,ITYP,IPART,JX,XLDBH,
     &  XHDBH,XLHT,XHHT,IRC
C----------
C  INITALIZE VARIABLES
C----------

        SNGSTM=0.      !WEIGHT OF SELECTED SNAG STEMS (TONS)
        SNGCRN=0.      !WEIGHT OF SELECTED SNAG CROWN MATERIAL (TONS)
        SNGSRM=0.      !WEIGHT OF REMOVED SNAG STEMS
        TOTBA=0.       !TOTAL BASAL AREA OF ALL SNAGS
        TOTSBA=0.      !TOTAL BASAL AREA OF SELECTED SNAGS
        SNBAIH=0.      !BASAL AREA OF SELECTED INITIALLY HARD SNAGS
        SNBAIS=0.      !BASAL AREA OF SELECTED INITIALLY SOFT SNAGS
        XH = 0.
        XS = 0.
        SNGVOL1= 0.    !DEBUG VARIABLE
        SNGSALVOL=0.   !DEBUG VARIABLE
        XM1=-1.
C----------
C  SNAGS
C----------
C
        IF(NSNAG.LT.1)GOTO 100
        DO I = 1, NSNAG
C       
        ISPS = SPS(I)
        D = DBHS(I)
        LINCL = .FALSE.
C       
        IF(JX.EQ.0 .OR. JX.EQ.ISPS)THEN
          LINCL = .TRUE.
        ELSEIF(JX.LT.0)THEN
          IGRP = -JX
          IULIM = ISPGRP(IGRP,1)+1
          DO IG=2,IULIM
          IF(ISPS .EQ. ISPGRP(IGRP,IG))THEN
            LINCL = .TRUE.
            EXIT
          ENDIF
          ENDDO
        ENDIF
C
C  ESTIMATE TOTAL BASAL AREA OF SNAGS
C
        TOTBA=TOTBA+0.005454154*D*D*(DENIH(I)+DENIS(I))
C
        IF (LINCL)THEN
          IF((DENIS(I)+DENIH(I)).GT.0.)THEN
            SNVIH = 0.0
            SNVIS = 0.0
C
C  SNAG STEMS
C  CONSTRAIN TO DBH RANGE
C
            IF((D.GE.XLDBH).AND.(D.LT.XHDBH))THEN
C
C  CONSTRAIN TO HEIGHT RANGE
C
              IF((HTIH(I).GE.XLHT).AND.(HTIH(I).LT.XHHT))THEN
                IF (DENIH(I) .GT. 0.0) THEN
                  SNBAIH= SNBAIH+0.005454154*D*D*DENIH(I)
                  CALL FMSVOL (I, HTIH(I), SNVIH, .FALSE., JOSTND)
                  SNVIH = SNVIH*DENIH(I)
                ENDIF
              ENDIF
C      
              IF((HTIS(I).GE.XLHT).AND.(HTIS(I).LT.XHHT))THEN
                IF (DENIS(I) .GT. 0.0) THEN
                  SNBAIS= SNBAIS+0.005454154*D*D*DENIS(I)
                  CALL FMSVOL (I, HTIS(I), SNVIS,  .FALSE., JOSTND)
                  SNVIS = SNVIS*DENIS(I)
                ENDIF
              ENDIF
C 
             SNGVOL1= SNGVOL1+SNVIS+SNVIH
C
C  SUM SNAG STEM VOLUME AND CONVERT TO TONS 
C
              SNGSTM = SNGSTM + (SNVIS+SNVIH) * V2T(ISPS)
C
            ENDIF       !DBH
          ENDIF       !DENIS(I)+DENIH(I))
        ENDIF       !SPECIES (LINCL)
        ENDDO     !SNAG LOOP
  100   CONTINUE
        TOTSBA= SNBAIS + SNBAIH
C----------
C  ACCUMULATE DEAD CROWN MATERIAL
C  ADD SNAG CROWNS TO THE APPROPRIATE SIZE CLASS
C  INCLUDE LITTER IN THE 0-3 CLASS, JUST SO THAT IT IS REPORTED
C----------
        DO ISZ = 0,5
          DO IDC = 1,4
            DO ITM = 1,TFMAX
              SNGCRN = SNGCRN + P2T *
     &                 (CWD2B(IDC,ISZ,ITM) + CWD2B2(IDC,ISZ,ITM))
            ENDDO
          ENDDO
        ENDDO
C
C  ADJUST TOTAL SNAG CROWN WEIGHT BY THE PROPORTION OF SNAG BASAL AREA
C  SELECTED BY USER
C
        IF(TOTBA.GT.0.)SNGCRN=SNGCRN*TOTSBA/TOTBA
        TOTDEAD=SNGSTM+SNGCRN
        IF(DEBUG)WRITE(JOSTND,*)' ICYC,TOTDEAD,SNGCRN, TOTSBA,TOTBA=  ',
     &  ICYC,TOTDEAD,SNGCRN, TOTSBA,TOTBA
C
C  CALCUALTE THE SALVAGE VOLUME, SNAG LIST FOR SALVBAGE CALCULATIONS 
C  IS PASSED FROM FMSALV
C
C  PASS OVER THE INITIALLY-HARD SNAGS
C
        IF(DEBUG)WRITE(JOSTND,*)' NSNAGSALV= ',NSNAGSALV
C
        IF((ISTAND.GE.0).AND.(ITYP.NE.0).AND.(NSNAGSALV.GT.0))THEN     !SALVAGE
          DO I=1,NSNAGSALV
          XS=0.
          XH=0.
          ISPS = SPSSALV(I)
          XD = DBHSSALV(I)
          HTD=HTDEADSALV(I)
          IF(DEBUG)WRITE(JOSTND,*)' I,fmevmon-XD,HTD,HTDEAD,HTIHSALV,=',
     &    I,XD,HTDEADSALV(I),HTDEAD(I),HTIHSALV(I)
C
          LINCL = .FALSE.
C       
          IF(JX.EQ.0 .OR. JX.EQ.ISPS)THEN
            LINCL = .TRUE.
          ELSEIF(JX.LT.0)THEN
            IGRP = -JX
            IULIM = ISPGRP(IGRP,1)+1
            DO IG=2,IULIM
            IF(ISPS .EQ. ISPGRP(IGRP,IG))THEN
              LINCL = .TRUE.
              EXIT
            ENDIF
            ENDDO
          ENDIF
C
C  INITIALLY HARD SNAGS
C
          IF(LINCL.AND.
     >    (XD.GE.XLDBH .AND. XD.LT.XHDBH).AND.
     >    ((HTIHSALV(I).GE.XLHT).AND.
     >    (HTIHSALV(I).LT.XHHT))) THEN
            IF((SALVSPA(I,1)+SALVSPA(I,2)).GT.0.)THEN
              HS = HTIHSALV(I)
              TPA = SALVSPA(I,1)
              IF (TPA .GT. 0.) THEN
                CALL FMSVL2(ISPS,XD,HTD,HS,X1,.FALSE.,DEBUG,JOSTND)
                XH = TPA * X1
              ENDIF
C
C     PASS OVER THE INITIALLY-SOFT SNAGS
C
              HS = HTISSALV(I)
              TPA = SALVSPA(I,2)
              IF (TPA .GT. 0. ) THEN
                CALL FMSVL2(ISPS,XD,HTD,HS,X1,.FALSE.,DEBUG,JOSTND)
                XS = TPA * X1
              ENDIF
C
              SNGSRM=SNGSRM+(XS+XH)*V2T(ISPS)     !SNAG STEMS SALVAGED
                                                  !ASSUME THAT CROWNS
                                                  !STAY IN STAND
              SNGSALVOL= SNGSALVOL+XS+XH          !DEBUG VARIABLE
C
              IF (DEBUG) WRITE(JOSTND,50) I,DENIH(I),HTIH(I),XD,XS,
     >        DENIS(I),HTIS(I),XH,ISPS,V2T(ISPS),SNGSRM,SALVSPA(I,1),
     >        SALVSPA(I,2)
 50           FORMAT(' FMEVMON-SALVAGE:I=',I5,'DENIH,HTIH,XD,XS=',4F10.3,
     >             ' DENIS,HTIS,XH=',3F10.3,' KSP=',I3/T18,'V2T=',
     >             F6.3,' SNGSRM=',F10.4,' SALVSPA(I,1,2)= ',2F10.3)
C
            ENDIF      !TPA>0
          ENDIF      !LINCL - SPECIES
        ENDDO     !SALVAGE SNAG LOOP
C
        ENDIF      !SALVAGE
C
        IF(DEBUG)WRITE(JOSTND,*)'SNGSTM,SNGSRM,SNGVOL1,SNGSALVOL= ',
     &  SNGSTM,SNGSRM,SNGVOL1,SNGSALVOL
C
C  ACCUMULATE LIVE TREE INFORMATION
C  INITITIALIZE LOCAL VARIABLES
C
        LIVSRM = 0.        !USER SELECED LIVE STEM WEIGHT REMOVED(TONS)
        LIVSRMTEM=0.       !DEBUG VARIABLE
        LIVSLV = 0.        !USER SELECED LIVE STEM WEIGHT LEFT
                           !IN STAND(TONS)
        LIVCRM = 0.        !USER SELECED LIVE CROWN WEIGHT REMOVED(TONS)
        LIVCRN = 0.        !USER SELECED LIVE CROWN WEIGHT (TONS)
        LIVFOL = 0.        !USER SELECED FOLIAGE WEIGHT (TONS)
        TOTFOL = 0.        !TOTAL FOLIAGE WEIGHT (TONS)
        LVCRNRMFOL=0.
C  
C  LIVE REMOVALS
C
        IF((ICYCRM.EQ.ICYC).AND.(ISTAND.GE.0).AND.(ITYP.GE.0)) THEN
          DO I = 1,ITRNL
C
          ISPC = ISPCC(I)
          D = DBHC(I)
          H = HTC(I)
C     
          LINCL = .FALSE.
          IF(JX.EQ.0 .OR. JX.EQ.ISPC)THEN
            LINCL = .TRUE.
          ELSEIF(JX.LT.0)THEN
            IGRP = -JX
            IULIM = ISPGRP(IGRP,1)+1
            DO IG=2,IULIM
            IF(ISPC .EQ. ISPGRP(IGRP,IG))THEN
              LINCL = .TRUE.
              EXIT
            ENDIF
            ENDDO
          ENDIF
C
C  CONSTRAIN SPECIES
C
          IF(LINCL)THEN
          XM1=-1.
C
C  LIVE TREES
C  CONSTRAIN TO DBH RANGE
C
            IF((D.GE.XLDBH).AND.(D.LT.XHDBH))THEN
C
C  CONSTRAIN TO HEIGHT RANGE
C
              IF((H.GE.XLHT).AND.(H.LT.XHHT))THEN
                LMERCH = .FALSE.
                CALL FMSVL2(ISPC,D,H,XM1,VT,LMERCH,.FALSE.,JOSTND)
           
                IF (DEBUG) WRITE(JOSTND,60) I,FMPROB(I),PROB(I),
     >                                      ISP(I),D,H,VT
 60             FORMAT(' FMEVMON(LIVE): I=',I5,' FMPROB=',F10.3,
     >               ' PROB=',F10.3,' ISP=',I3,' D,H,VT=',3F10.3)
C
C  LIVE STEM
C
                LIVSRMTEM = PREMST(I) * VT * V2T(ISPC)
                LIVSRM=LIVSRM+LIVSRMTEM
C
                IF(DEBUG)WRITE(JOSTND,*)' I,PREMST(I),FMPROB(I),',
     &          'PROB(I)= ',PREMST(I),FMPROB(I),PROB(I)
                IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,LREMT,LIVSRM,LIVSLV= ',
     &                            I,ISPC,LREMT,LIVSRM,LIVSLV
C
C  LIVE CROWN
C
                DO J=0,5
                LIVCRM = LIVCRM+(CROWNWC(I,J))*P2T*PREMCR(I)
                ENDDO
C
                IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,LREMT,LIVCRM,LIVCRN= ',
     &                            I,ISPC,LREMT,LIVCRM,LIVCRN
C
              ENDIF             !HEIGHT
            ENDIF             !DBH
          ENDIF             !SPECIES
          ENDDO
        ENDIF             !LIVE REMOVALS
C
C  STANDING LIVE
C
        IF((ISTAND.NE.0).AND.(ITYP.GE.0))THEN

          DO I = 1,ITRN
C    
          ISPC = ISP(I)
          D = DBH(I)
          H = HT(I)
          XM1=-1.
C    
          LINCL = .FALSE.
          IF(JX.EQ.0 .OR. JX.EQ.ISPC)THEN
            LINCL = .TRUE.
          ELSEIF(JX.LT.0)THEN
            IGRP = -JX
            IULIM = ISPGRP(IGRP,1)+1
            DO IG=2,IULIM
            IF(ISPC .EQ. ISPGRP(IGRP,IG))THEN
              LINCL = .TRUE.
              EXIT
            ENDIF
            ENDDO
          ENDIF
C
C  CONSTRAIN SPECIES
C
          IF(LINCL)THEN
C
C  CONSTRAIN TO DBH RANGE
C
            IF((D.GE.XLDBH).AND.(D.LT.XHDBH))THEN
C
C  CONSTRAIN TO HEIGHT RANGE
C
              IF((H.GE.XLHT).AND.(H.LT.XHHT))THEN
                LMERCH = .FALSE.
           
                CALL FMSVL2(ISPC,D,H,XM1,VT,LMERCH,.FALSE.,JOSTND)
           
                IF (DEBUG) WRITE(JOSTND,61) I,FMPROB(I),PROB(I),
     >                                      ISP(I),D,H,VT
 61             FORMAT(' FMEVMON(LIVE): I=',I5,' FMPROB=',F10.3,
     >                 ' PROB=',F10.3,' ISP=',I3,' D,H,VT=',3F10.3)
C          
C  LIVE STEM
C
                LIVSLV = LIVSLV+FMPROB(I) * VT * V2T(ISPC)
C
                IF(DEBUG)WRITE(JOSTND,*)' I,PREMST(I),FMPROB(I),',
     &           'PROB(I)= ',PREMST(I),FMPROB(I),PROB(I)
                IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,LREMT,LIVSRM,LIVSLV= ',
     &                            I,ISPC,LREMT,LIVSRM,LIVSLV
C
C  LIVE CROWN
C
                DO J=0,5
                LIVCRN = LIVCRN+(CROWNW(I,J))*P2T*FMPROB(I)
                ENDDO
C
C  LIVE FOLIAGE
C
                TOTFOL=TOTFOL+CROWNW(I,0)*P2T*FMPROB(I)
C
                IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,LREMT,LIVCRM,LIVCRN,',
     &          'TOTFOL= ',I,ISPC,LREMT,LIVCRM,LIVCRN,TOTFOL
C
              ENDIF             !HEIGHT
            ENDIF             !DBH
          ENDIF             !SPECIES
          ENDDO
        ENDIF             !LIVE STANDING
C
        IF(DEBUG)WRITE(JOSTND,*)' LIVSRM, LIVSLV, LIVCRN LVCRNRMFOL= ',
     &  LIVSRM,LIVSLV, LIVCRN,LVCRNRMFOL
C
C  CALCULATE RETURN VARIABLES
C
C  ISTAND TRREE STATUS -  STANDING(<0), REMOVED(0),OR BOTH(>0)
C  ITYP  TREE TYPE -  DEAD(<0), LIVE(0),OR BOTH(>0)
C  IPART  TREE PART -  STEM(<0), CROWN(0),OR BOTH-WHOLE TREE(>0)
C
        IF(ISTAND.LT.0)THEN                 !STANDING
          IF(ITYP.LT.0)THEN                  !DEAD ONLY
            IF(IPART.LT.0)RVAL=SNGSTM           !STEM ONLY
            IF(IPART.EQ.0)RVAL=SNGCRN           !CROWN ONLY
            IF(IPART.GT.0)RVAL=SNGSTM+SNGCRN    !BOTH STEM AND CROWN
          ELSEIF(ITYP.EQ.0)THEN              !LIVE ONLY
            IF(IPART.LT.0)RVAL=LIVSLV           !STEM ONLY
            IF(IPART.EQ.0)RVAL=LIVCRN           !CROWN ONLY
            IF(IPART.GT.0)RVAL=LIVSLV+LIVCRN    !BOTH STEM AND CROWN
            IF(IPART.EQ.2)RVAL=TOTFOL           !FOLIAGE
          ELSEIF(ITYP.GT.0)THEN              !BOTH LIVE AND DEAD
            IF(IPART.LT.0)RVAL=LIVSLV+SNGSTM    !STEM ONLY
            IF(IPART.EQ.0)RVAL=LIVCRN+SNGCRN    !CROWN ONLY
            IF(IPART.GT.0)RVAL=LIVSLV+LIVCRN+
     &                       SNGSTM+SNGCRN    !BOTH STEM AND CROWN
          ENDIF           !ITYP
C
        ELSEIF(ISTAND.EQ.0)THEN             !REMOVED
          IF(ITYP.LT.0)THEN                   !DEAD ONLY
            IF(IPART.LT.0)RVAL=SNGSRM           !STEM ONLY
            IF(IPART.EQ.0)RVAL=0.               !+ SNGCRN ASSUME LEAVE SNG CR
            IF(IPART.GT.0)RVAL=SNGSRM           !+SNGCRN  ASSUME LEAVE SNG CR
          ELSEIF(ITYP.EQ.0)THEN              !LIVE ONLY
            IF(IPART.LT.0)RVAL=LIVSRM           !STEM ONLY
            IF(IPART.EQ.0)RVAL=LIVCRM           !CROWN ONLY
            IF(IPART.GT.0)RVAL=LIVSRM+LIVCRM    !BOTH STEM AND CROWN
          ELSEIF(ITYP.GT.0)THEN              !BOTH LIVE AND DEAD
            IF(IPART.LT.0)RVAL=LIVSRM+SNGSRM    !STEM ONLY
            IF(IPART.EQ.0)RVAL=LIVCRM           !+SNGCRN -ASSUME LEAVE SNG CROWN
            IF(IPART.GT.0)RVAL=LIVSRM+LIVCRM+   !BOTH STEM AND CROWN
     &                    SNGSRM                !+SNGCRN ASSUME LEAVE SNG CROWN
          ENDIF           !ITYP
C
        ELSEIF(ISTAND.GT.0)THEN             !BOTH STANDING & REMOVED
          IF(ITYP.LT.0)THEN                   !DEAD ONLY
            IF(IPART.LT.0)RVAL=SNGSTM+SNGSRM    !STEM ONLY
            IF(IPART.EQ.0)RVAL=SNGCRN           !CROWN ONLY
            IF(IPART.GT.0)RVAL=SNGSTM+SNGCRN+SNGSRM !BOTH STEM AND CROWN
          ELSEIF(ITYP.EQ.0)THEN               !LIVE ONLY
            IF(IPART.LT.0)RVAL=LIVSLV+LIVSRM    !STEM ONLY
            IF(IPART.EQ.0)RVAL=LIVCRN+LIVCRM    !CROWN ONLY
            IF(IPART.GT.0)RVAL=LIVSLV+LIVSRM+   !BOTH STEM AND CROWN
     &                       LIVCRN+LIVCRM
          ELSEIF(ITYP.GT.0)THEN               !BOTH LIVE AND DEAD
            IF(IPART.LT.0)RVAL=LIVSLV+LIVSRM+   !STEM ONLY
     &                       SNGSTM+SNGSRM
            IF(IPART.EQ.0)RVAL=LIVCRN+SNGCRN+   !CROWN ONLY
     &                       LIVCRM
            IF(IPART.GT.0)RVAL=LIVSLV+LIVSRM+   !BOTH STEM AND CROWN
     &                       LIVCRN+LIVCRM+
     &                       SNGSTM+SNGSRM+SNGCRN
          ENDIF           !ITYP
        ENDIF           !ISTAND
C
C  SET RETURN VARIABLE=0, IF F1=>0 OR F2=/0 AND F3=2
C
        IF(((ISTAND.GE.0).OR.(ITYP.NE.0)).AND.(IPART.EQ.2))RVAL=0.
C
        IF(DEBUG)WRITE(JOSTND,*)' RVAL,ISTAND,ITYP,IPART,JX,XLDBH,',
     &  'XHDBH,XLHT,XHHT,IRC= ',RVAL,ISTAND,ITYP,IPART,JX,XLDBH,
     &  XHDBH,XLHT,XHHT,IRC
        IF(DEBUG)WRITE(JOSTND,*)' ITRN,SNGSRM,SNGCRN,LIVSRM,LIVCRM= ',
     &  ITRN,SNGSRM,SNGCRN,LIVSRM,LIVCRM
C
      ENDIF      ! IFMYR1
C
      RETURN
C********************************************************************
C
C     EVENT MONITOR POTSRATE FUNCTION.
C
C********************************************************************

      ENTRY FMEVSRT(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         RVAL = POTFSR(II) ! potential spread rate
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR POTREINT FUNCTION.
C
C********************************************************************

      ENTRY FMEVRIN(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         RVAL = POTRINT(II) ! potential fire reaction intensity
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR CARBSTAT FUNCTION.
C
C********************************************************************

      ENTRY FMEVCARB(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         RVAL = 0
         IF (II .GE. 1 .AND. II .LE. 17) RVAL = CARBVAL(II)
      ENDIF
      RETURN
C********************************************************************
C
C     EVENT MONITOR DWDVAL FUNCTION.
C
C     RVAL  VALUE RETURNED AFTER EVALUATING DOWN WOOD
C     IX    INDEX FOR KIND OF RESULT: 1=VOLUME (CUFT/ACRE), 2=COVER (%)
C     JX    HARD/SOFT INDEX: 0=BOTH, 1=HARD, 2=SOFT
C     ILO   SIZE INDEX:    1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
C                          6=35-50", 7=50"+
C     IHI   SIZE INDEX:    1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
C                           6=35-50", 7=50"+
C
C********************************************************************

      ENTRY FMDWD(RVAL, IX, JX, ILO, IHI, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         RVAL = 0 
         IRC=0

C     ADJUST THE VALUES BECAUSE THE INDEX IS 1 - 7, BUT THE ARRAY SPOTS 
C     ARE 1 - 9, WITH 0-3" COVERING 3 CATEGORIES.

         IF (ILO .GT. 1) ILO = ILO + 2
         IF (IHI .GE. 1) IHI = IHI + 2

         SELECT CASE (IX)
         CASE (1)  ! volume
           DO I = ILO, IHI         
             SELECT CASE (JX)
             CASE(0) ! both
               RVAL = RVAL + CWDVOL(3,I,1,5) + CWDVOL(3,I,2,5)
             CASE(1) ! hard
               RVAL = RVAL + CWDVOL(3,I,2,5)
             CASE(2) ! soft
               RVAL = RVAL + CWDVOL(3,I,1,5) 
             END SELECT
           ENDDO 

         CASE (2)  ! cover
           DO I = ILO, IHI         
             SELECT CASE (JX)
             CASE(0) ! both
               RVAL = RVAL + CWDCOV(3,I,1,5) + CWDCOV(3,I,2,5)
             CASE(1) ! hard
               RVAL = RVAL + CWDCOV(3,I,2,5)
             CASE(2) ! soft
               RVAL = RVAL + CWDCOV(3,I,1,5) 
             END SELECT
           ENDDO 
         END SELECT
      ENDIF
      RETURN

C********************************************************************
C
C     EVENT MONITOR MAX CONIFER SNAG FUNCTION -- USED IN SUBROUTINE
C     EVPRED TO COMPUTE THE FISHER HABITAT INDEX.
C
C********************************************************************

      ENTRY FMEVMSN(RVAL)
      RVAL=0.
      DO 105 I=1,NSNAG
      IF(NSNAG .LT. 1) GO TO 110
      ISPC = SPS(I)
      IF(LSW(ISPC))THEN
        IF(DBHS(I) .GT. RVAL) RVAL=DBHS(I)
      ENDIF
  105 CONTINUE
  110 CONTINUE
      RETURN
C********************************************************************
C
C     EVENT MONITOR HERBSHRB FUNCTION (I.E. LIVE SURFACE FUELS)
C
C********************************************************************

      ENTRY FMEVLSF(RVAL, II, IRC)
      IF (IFMYR1.EQ.-1) THEN
         IRC=1
      ELSE
         IRC=0
         SELECT CASE (II)
         CASE (1)
           RVAL = FLIVE(1) ! live herbs in tons/acre
         CASE (2)
           RVAL = FLIVE(2) ! live shrubs in tons/acre
         CASE (3)
           RVAL = FLIVE(1) + FLIVE(2) ! total herbs and shrubs in tons/acre
         END SELECT
      ENDIF
      RETURN
      
      END


