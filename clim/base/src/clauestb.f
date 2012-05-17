      SUBROUTINE CLAUESTB
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     CLIMATE EXTENSION--AUTOMATIC ESTABLISHMENT ROUTINE 
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CLIMATE.F77'
C
COMMONS

      REAL ALGSLP,TMAXTRS,XX,XMAX,PRMS(6),THISYR,
     >     PTREES,TTOADD,SPBA(MAXSP),SPTPA(MAXSP),SPIMP(MAXSP)
      INTEGER I,I1,I2,ISPINDX(MAXSP),NSPEC,KODE,JOUT
      LOGICAL DEBUG
      
      CALL DBCHK (DEBUG,'CLAUESTB',8,ICYC)

      IF (DEBUG) WRITE (JOSTND,10) LCLIMATE,LAESTB,ICYC
   10 FORMAT (' IN CLAUESTB, LCLIMATE,LAESTB=',2L2,' ICYC=',I3)

      POTESTAB = 0.

C     IF THE OPTION IS TURNED OFF, THEN JUST RETURN.
      
      IF (.NOT.LCLIMATE) RETURN      
      
C     ARE WE BELOW AESTOCK PERCENT OF FULL STOCKING?

      CALL SDICAL (XMAX)
      XX=MAX(5.0,RMSQD)
      TMAXTRS = (XMAX/0.02483133)*(XX**(-1.605))

C     PTREES IS THE PROPORTION OF TREES TO ESTABLISH AS A FUNCTION
C     OF HOW LOW THE STOCKING IS (ANOTHER ADJUSTMENT IS BELOW).
      
      IF (TMAXTRS.GT. 1) THEN
        PTREES = 2. - (4.*(TPROB/TMAXTRS))
        IF (PTREES.LT.0.) PTREES=0.
        IF (PTREES.GT.1.) PTREES=1.
      ELSE
        PTREES=1.
      ENDIF

      IF (DEBUG) WRITE (JOSTND,16) TPROB,PTREES,TMAXTRS,AESNTREES,
     >        NESPECIES
   16 FORMAT (' IN CLAUESTB, TPROB,PTREES,TMAXTRS,AESNTREES=',4F12.5,
     >        ' NESPECIES=',I3)

      NSPEC=0
      THISYR = FLOAT(IY(ICYC))+FINT/2

      IF (PTREES*AESNTREES.GT.0.) THEN
                
        DO I=1,MAXSP
          IF (INDXSPECIES(I).EQ.0) THEN
            POTESTAB(I)=0.
          ELSE       
            I2 = INDXSPECIES(I)         
            POTESTAB(I)=ALGSLP(THISYR, FLOAT(YEARS),ATTRS(1,I2),NYEARS) 
          ENDIF
          IF (DEBUG) WRITE (JOSTND,30) I,JSP(I),INDXSPECIES(I),
     >               POTESTAB(I)
   30     FORMAT (' IN CLAUESTB, SP,INDXSPECIES,POTESTAB(I)=',
     >               I4,1X,A2,I3,F10.3)
        ENDDO 
        CALL RDPSRT(MAXSP,POTESTAB,ISPINDX,.TRUE.)
        NSPEC=0
        DO I=1,MAXSP
          IF (POTESTAB(ISPINDX(I)).LT. .40) EXIT
          NSPEC=NSPEC+1
        ENDDO
      ENDIF
           
      IF (DEBUG) WRITE (JOSTND,40) NSPEC
   40 FORMAT (' IN CLAUESTB, NSPEC=',I3)
   
    
      IF (NSPEC.GT.0) THEN 
        IF (NSPEC.GT.NESPECIES) NSPEC=NESPECIES
      
C       SCALE THE TOP NESPECIES POTESTAB SCORES

        DO I=1,NSPEC
          XX = -1. + 2.5*POTESTAB(ISPINDX(I))
          IF (XX.LT.0.) XX=0.
          IF (XX.GT.1.) XX=1.
          POTESTAB(ISPINDX(I)) = XX
        ENDDO
        TTOADD=POTESTAB(ISPINDX(1))
        IF (POTESTAB(ISPINDX(1)).GT. 0.8) THEN
          TTOADD = AESNTREES
        ELSE
          TTOADD = AESNTREES * POTESTAB(ISPINDX(1))
        ENDIF
        IF (DEBUG) WRITE (JOSTND,41) POTESTAB(ISPINDX(1)),
     >             JSP(ISPINDX(1)),TTOADD,NSPEC,NESPECIES
   41   FORMAT (' IN CLAUESTB, MAX POTESTAB=',F7.3,' SP=',A2,
     >          ' TTOADD=',F10.3,' NSPEC=',I2,' NESPECIES=',I2)
           
        XX=0.
        DO I=1,NSPEC
          XX=XX+POTESTAB(ISPINDX(I))
        ENDDO
        DO I=1,NSPEC
          POTESTAB(ISPINDX(I))=POTESTAB(ISPINDX(I))/XX
        ENDDO
        
        DO I=1,MAXSP
          IF (I.GT.NSPEC) THEN
            POTESTAB(ISPINDX(I)) = 0
          ELSE 
            XX = PTREES*TTOADD*POTESTAB(ISPINDX(I))
            IF (XX.LE.1.) XX=0.
            POTESTAB(ISPINDX(I)) = XX
          ENDIF
        ENDDO
      ELSE
        POTESTAB=0. 
        ISPINDX=1
      ENDIF
           
C     CURRENT STOCKING MUST BE BELOW THE THRESHOLD, IF NOT RETURN.
      
      IF (DEBUG) WRITE (JOSTND,42) TPROB,TMAXTRS,AESTOCK,
     >         TPROB .GT. TMAXTRS*AESTOCK*.01
   42 FORMAT (' IN CLAUESTB: TPROB,TMAXTRS,AESTOCK=',3F12.5,
     >        ' WILL RETURN=',L2)
     
C     WRITE THE REPORT, SKIP IF JCLREF IS -1 (A FLAG SUPPRESSING THE OUTPUT).

      I=1 
      IF (JCLREF.GE.0) THEN
        IF (JCLREF.EQ.0) THEN
          I=0
          CALL GETID (JCLREF)
          CALL GETLUN(JOUT)
          WRITE (JOUT,'(1X,I5," $#*%")') JCLREF
          WRITE (JOUT,43) 
   43     FORMAT(/T8,'CLIMATE-FVS VERSION 2.0 VIABILITY',
     >          ' AND EFFECTS REPORT'/,69('-')/T11,'SP VIA-',
     >          14X,'VIAB. dCLIM GROWTH SITE MXDEN AUTOESTB'/
     >          'YEAR SPEC BILITY  BA/A    TPA   MORT ',
     >          ' MORT MULT   MULT  MULT   TPA'/
     >          '---- ---- ------ ------ ------ ----- ',
     >          '----- ----- ----- ----- --------'/'$#*%')
         
        ENDIF
        IF (I.EQ.1) THEN
          CALL GETLUN(JOUT)
          WRITE (JOUT,44) JCLREF
   44     FORMAT (1X,I5,' ')
        ENDIF

        SPBA=0.
        SPTPA=0.
        DO I=1,ITRN
          I1=ISP(I)
          SPBA(I1)=SPBA(I1)+(DBH(I)*DBH(I)*PROB(I)*0.005454154)
          SPTPA(I1)=SPTPA(I1)+PROB(I)
        ENDDO
        SPIMP=0.
        XX=SUM(SPBA)
        IF (XX.GT.0) SPIMP=SPBA/XX
        XX=SUM(SPTPA)
        IF (XX.GT.0) SPIMP=SPIMP+(SPTPA/XX)
        XX=SUM(SPIMP)
        IF (XX.GT.0) SPIMP=SPIMP/XX
        DO I=1,MAXSP
          IF ((SPIMP(I).GT. 0.1 .OR. SPVIAB(I).GT. .4) .AND. 
     >        INDXSPECIES(I).GT.0) THEN
            WRITE (JOUT,45) JCLREF,IY(ICYC),JSP(I),SPVIAB(I),
     >          SPBA(I),SPTPA(I),SPMORT1(I),SPMORT2(I),SPGMULT(I),
     >          SPSITGM(I),MXDENMLT,POTESTAB(I)
   45       FORMAT (1X,I5,1X,I4,3X,A2,F7.3,F7.2,F7.1,5F6.3,F9.1)
          ENDIF
        ENDDO
      ENDIF
        
      IF (.NOT.LAESTB) RETURN
      IF (TPROB .GT. TMAXTRS*AESTOCK*.01 ) RETURN

C     ADD PLANT ACTIVITIES, CODE IS  430
      
      I2=0
      DO I=1,NSPEC
        IF (POTESTAB(ISPINDX(I)) .GT. 0) THEN
          PRMS(1)=ISPINDX(I)                           ! species
          PRMS(2)=POTESTAB(ISPINDX(I))                 ! trees/acre
          PRMS(3)=100.                                 ! % survival
          PRMS(4)=0.                                   ! age
          PRMS(5)=0.                                   ! height
          PRMS(6)=0.                                   ! shade code
          CALL OPADD (IY(ICYC+1)-1,430,0,6,PRMS,KODE)
          I2=I2+1
          IF (DEBUG) WRITE (JOSTND,50) I,KODE,PRMS
   50     FORMAT (' IN CLAUESTB, I=',I2,' KODE=',I2,' PRMS=',6F8.3)
        ELSE
          IF (DEBUG) WRITE (JOSTND,60) I,ISPINDX(I)
   60     FORMAT (' IN CLAUESTB, TOO FEW: I=',I2,'ISPINDX(I)=',I2)
        ENDIF
      ENDDO 
      IF (I2.GT.0) CALL OPINCR (IY,ICYC,NCYC)
      RETURN
      END
