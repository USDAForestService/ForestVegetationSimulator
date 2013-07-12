      SUBROUTINE BWEP3
      IMPLICIT NONE
C----------
C  **BWEP3                   DATE OF LAST REVISION:  06/18/13
C----------
C
C  PRINT ANNUAL OUTPUT RE: BUDWORM MORTALITY & FEEDING
C    ALSO PRINTS TABLE 5 - ANNUAL SUMMARY
C
C  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR
C
C   CALLED FROM: BWELIT	
C
C   SUBROUTINES AND FUNCTIONS CALLED:	NONE
C
C   PARAMETERS:
C
C   ITITLB - TITLE FOR BW MODEL
C   IYRCUR - CURRENT YEAR
C   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
C   OUT3(IC,IH,I), FOR I: 1= INITIAL NO. OF EGGS; 2-7= % OF TOTAL
C     (GENERATIONAL) MORTALITY DURING EGGS TO ESTABLISHED L2S (2), 
C     L2S TO MID-L4S (STARVATION =3, PREDATORS + OTHER =4),
C     MID-L4S TO L6 (STARV.+DISPERSAL = 5, PRED.S + OTHER = 6),
C     PUPAL PREDS. = 7); % DEFOLIATION OF NEW (=8) AND TOTAL (=9) FOLIAGE;
C     AMT. OF NEW (=10) AND TOTAL (=11) FOLIAGE; AMOUNT OF NEW
C     EGGS PRODUCED BY THIS GENERATION (=12); FINAL NO. OF ADULTS
C     (=13); NO. OF FEMALES (=14); FEMALE WT. (=15);
C     AND NO. EGGS PER FEMALE (=16);  % MORT. DUE TO SPRAY (=17);
C     AND PARASITES (L2-4 =18, L4-6=19, P=20). 
C
C  Revision History:
C    09-JUN-00 Lance David (FHTET)
C      .Minor adjustment to annual summary (JOBWP5) header spacing.
C    07-APR-2004 Lance David (FHTET)
C      Corrected two misspellings of variable SUMEL2.
C   02-JUN-2009 Lance R. David (FMSC)
C      Added Stand ID and comma delimiter to output tables, some header
C      and column labels modified.
C   14-JUL-2010 Lance R. David (FMSC)
C      Added IMPLICIT NONE and declared variables as needed.
C   11-APR-2013 Lance R. David (FMSC)
C      REAL variable DIFF was used in loop control at label 85 in error.
C      Changed to variable IDIFF and removed REAL declaration of DIFF.
C---------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'

      LOGICAL HEADER
      CHARACTER*3 TRESP(6),sumt1,sumt2
      INTEGER IAPPB, IAPPC, IAPPD, IAPPE, IAPPF, IAPPG, IAPPH, IAPPI,
     &        IAPPJ, IC, ICROWN, IDIFF, IEGGS, IH, INDEX, ISEL2,
     &        ISIZE, ISPAR, ISPNUM, ISPRE, ISSPR, ISSTV, IYEARK, K, 
     &        KCROWN(18), LH, LSIZE, NEWDEF
      INTEGER I, IYEAR, IYR, LASTYR 
      REAL APPB, APPC, APPD, APPE, APPF, APPG, APPH, APPI, APPJ,
     &     DEAD, DEFNEW, DEFOLD, EGGD, FECUND, GENMOR, OVWINT,
     &     RINC, RINC10, SPRING, SUMALE, SURV,
     &     TEMPH(20), TEMPO(20), TEMPS(20), X 
      REAL EGMASS, SNEWEG, SUMACT, SUMADT, SUMEGG, SUMEL2, SUMFEM,
     &     SUMFOL, SUMPAL, SUMPAP, SUMPAR, SUMPAS, SUMPOT, SUMPRE,
     &     SUMPRL, SUMPRP, SUMPRS, SUMSPR, SUMSTL, SUMSTS, SUMSTV 

      DATA HEADER/.TRUE./
      DATA TRESP/' WF',' DF',' GF','SAF',' ES',' WL'/
      DATA EGMASS/41.7/
C
C SOURCES: GPERM2 - SHEEHAN ET AL (1989) TABLE 9A (MEAN OF WF & DF,
C   ALL THREE CROWN THIRDS WT'D EQUALLY); EGMASS - SHEEHAN ET AL 
C   (1989) TABLE 2
C
      DO 10 I=1,20
      TEMPH(I)=0.0
      TEMPO(I)=0.0
   10 TEMPS(I)=0.0
C
      SUMACT=0.0
      SUMPOT=0.0
      SUMFOL=0.0
      SUMEGG=0.0
      SUMEL2=0.0
      SUMSTV=0.0
      SUMPRE=0.0
      SUMPAR=0.0
      SUMSPR=0.0
      SNEWEG=0.0
      SUMADT=0.0
      SUMPRS=0.0
      SUMPRL=0.0
      SUMPRP=0.0
      SUMPAS=0.0
      SUMPAL=0.0
      SUMPAP=0.0
      SUMSTS=0.0
      SUMSTL=0.0
      SUMFEM=0.0
C
C IF THIS IS THE FIRST CALL, PRINT THE HEADER - FIRST FOR 
C    TABLE 5, THEN FOR G_CROWN (LP2)
C
      IF (HEADER .AND. LP5) THEN
         WRITE (JOBWP5,15) MGMIDB,ITITLB
   15    FORMAT (A4,' -- ',A72/)
         WRITE (JOBWP5,40)
   40    FORMAT (51X,'% of total mortality',
     &   /,40X,41('-'),'  total'/,
     &   40X,'eggs   L2 to L4      L4 to L6',7X,'pupae  gener. ',
     &   ' init     % defol.   rate    no.of ',/,40X,
     &   ' to   ----------  ----------------  -----    %  ',
     &   '   egg     ----------   of     init',/,'STAND ID,',
     &   19X,'year, tree, ',
     &   ' L2,  strv,  NE,  strv,  NE,  spry,  NE,    mort, ',
     &   ' dens.,  new,  totl, incr.,   eggs,',/,
     &   '--------------------------, ----, ----, ',
     &   '----, ----, ----, ----, ----, ----, -----, -----, ------,',
     &   '  ----, ----, -----, --------,')
      ELSEIF (LP5) THEN
         WRITE (JOBWP5,45)
   45    FORMAT (1X)
      ENDIF
C
      IF (HEADER .AND. LP6) THEN
         IYR=IYRCUR-1
         WRITE (JOBWP6,50) NPLT,IYR,IYR
   50    FORMAT ('STAND ID,',19X,
     &      ' YEAR,  %_DEFO,  EGG_DN,  V_POOR,  V_GOOD, ',
     &      ' OTHER,   YEAR,   EM_L2,   STARV,   PREDS,   PARAS,',
     &      '   SPRAY,   TOTAL,',/,
     &      A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),1X,I5,',',
     &      6(7X,'0,'))
      ENDIF
      IF (HEADER .AND. LP7) THEN
         IYR=IYRCUR-1
         WRITE (JOBWP7,55) NPLT,IYR
   55    FORMAT ('STAND ID,',18X,
     &   ' YEAR,   A_MALE,   P_PARA,   P_PRED,   L_PARA,',
     &   '   L_PRED,   L_STRV,   L_SPRA,   S_PRED,   S_STRV,   SPRING,',
     &   '   OVWINT, INIT_EGG,   R_INCR,   EGGS/F,',/,
     &   A26,',',I5,',',14('          '))
         LASTYR=IYR
      ENDIF
C
      IF (HEADER .AND. LP2) THEN
         SUMT1=' DF'
         SUMT2=' GF'
         WRITE (JOBWP2,60) SUMT1,SUMT2

   60    FORMAT (26X,',',2(A3,',  SMALL,',3(5X,','),
     &   'MEDIUM,    ,',2(5X,','),'LARGE,',2(5X,',')),/,
     &   'Stand ID,',18X,6(' Year,  Top,  Mid,  Bot,'))
         IYEAR=0
         IF (IYRCUR.GT.IY(1)) THEN
           IDIFF=IYRCUR-IY(1)
           IYR=IY(1)-1	   
           DO 80 I=1,IDIFF
           IYEAR=IYEAR+1
           IYR=IYR+1
           WRITE (JOBWP2,75) NPLT,IYR,IYR,IYR,IYR,IYR,IYR
   75      FORMAT (A26,',',6(1X,I4,',',3('    0,')))
   80      CONTINUE
         ENDIF
      ENDIF 
C
      HEADER=.FALSE.
C
C  PRINT OUT THE WHOLE SHEBANG!
C
      LH=0
      LSIZE=0
      IF (LP2) THEN
         IYEAR=IYEAR+1
         IYR=IYR+1
         IF (IYR.LT.IYRCUR) THEN
            IDIFF=IYRCUR-IYR
            DO 85 I=1,IDIFF
            WRITE (JOBWP2,75) NPLT,IYR,IYR,IYR,IYR,IYR,IYR
            IYEAR=IYEAR+1
            IYR=IYR+1
   85       CONTINUE
         ENDIF
      ENDIF
      DO 87 K=1,18
      KCROWN(K)=0
   87 CONTINUE
      DO 300 IH=1,6
      ISPNUM=0
      IF (TRESP(IH).EQ.SUMT1) ISPNUM=1
      IF (TRESP(IH).EQ.SUMT2) ISPNUM=2
C
C  TRANSLATE CROWN LEVEL INTO TREE SIZE AND CROWN THIRD
C
      DO 200 IC=1,9
      IF (IC.LE.3) THEN
         ISIZE=1
      ELSEIF (IC.LE.6) THEN
         ISIZE=2
      ELSE
         ISIZE=3
      ENDIF
      ICROWN=MOD(IC,3)
      IF (ICROWN.EQ.0) ICROWN=3
      IF (OUT3(IC,IH,1).LT.1.0) GOTO 180
      IF (LH.NE.IH.OR.LSIZE.NE.ISIZE) THEN
        LH=IH
        LSIZE=ISIZE
      ENDIF
C
C  CALC. % BW MORTALITY AND % DEFOLIATION
C
C  SUM FOR TABLE 5
C
      IF (LP5 .OR. LP6 .OR. LP7) THEN
        TEMPH(1)=TEMPH(1)+OUT3(IC,IH,1)
        TEMPH(2)=TEMPH(2)+OUT3(IC,IH,2)
        TEMPH(3)=TEMPH(3)+OUT3(IC,IH,3)
        TEMPH(4)=TEMPH(4)+OUT3(IC,IH,4)
        TEMPH(5)=TEMPH(5)+OUT3(IC,IH,5)
        TEMPH(6)=TEMPH(6)+OUT3(IC,IH,6)
        TEMPH(7)=TEMPH(7)+OUT3(IC,IH,7)  
        TEMPH(8)=TEMPH(8)+OUT3(IC,IH,8)  
        TEMPH(9)=TEMPH(9)+OUT3(IC,IH,9)  
        TEMPH(10)=TEMPH(10)+OUT3(IC,IH,10)  
        TEMPH(11)=TEMPH(11)+OUT3(IC,IH,11)  
        TEMPH(12)=TEMPH(12)+OUT3(IC,IH,12)
        TEMPH(13)=TEMPH(13)+OUT3(IC,IH,13)
        TEMPH(14)=TEMPH(14)+OUT3(IC,IH,14)
        TEMPH(15)=TEMPH(15)+(OUT3(IC,IH,15)*OUT3(IC,IH,14))
        TEMPH(16)=TEMPH(16)+(OUT3(IC,IH,16)*OUT3(IC,IH,14))       
        TEMPH(17)=TEMPH(17)+OUT3(IC,IH,17)
        TEMPH(18)=TEMPH(18)+OUT3(IC,IH,18)
        TEMPH(19)=TEMPH(19)+OUT3(IC,IH,19)
        TEMPH(20)=TEMPH(20)+OUT3(IC,IH,20)
      ENDIF
C
C  PRINT NEW TABLE 2 (G_CROWN)
C
      IF (LP2 .AND. ISPNUM .NE. 0) THEN
         INDEX=((ISPNUM-1)*9)+IC
         DEFNEW=0.0
         IF (OUT3(IC,IH,10).GT.0.0) DEFNEW=100.0*(1.0-(OUT3(IC,IH,8)
     &      /OUT3(IC,IH,10)))
         IF (DEFNEW.GT.0.0) KCROWN(INDEX)=NINT(DEFNEW)              
      ENDIF
C
C  RESET THE ARRAY TO ZERO
C
  180 DO 190 I=1,20
      OUT3(IC,IH,I)=0.0
  190 CONTINUE
  200 CONTINUE
C
C  TABLE 5: PRINT RESULTS FOR THIS HOST, THEN RESET SUMMARY VAR.S
C
      IF (LP5 .OR. LP6 .OR. LP7) THEN
        IF (TEMPH(1).LT.0.0) THEN
          WRITE (JOBWP5,320) NPLT,IYRCUR
  320     FORMAT (' GAAACK! URP!! STAND ID= ',A26,' YEAR= ',I4)
        ELSEIF (TEMPH(1).GT.0.0) THEN
           SUMACT=SUMACT+TEMPH(8)
           SUMPOT=SUMPOT+TEMPH(10)
           SUMFOL=SUMFOL+TEMPH(11)
           SUMEGG=SUMEGG+TEMPH(1)  
           SUMEL2=SUMEL2+TEMPH(2)  
           SUMSTV=SUMSTV+TEMPH(3)+TEMPH(5)  
           SUMPRE=SUMPRE+TEMPH(4)+TEMPH(6)+TEMPH(7)  
           SUMPAR=SUMPAR+TEMPH(18)+TEMPH(19)+TEMPH(20)  
           SUMSPR=SUMSPR+TEMPH(17)  
           SNEWEG=SNEWEG+TEMPH(12)
           SUMADT=SUMADT+TEMPH(13)
           SUMPRS=SUMPRS+TEMPH(4)
           SUMPRL=SUMPRL+TEMPH(6)
           SUMPRP=SUMPRP+TEMPH(7)
           SUMPAS=SUMPAS+TEMPH(18)
           SUMPAL=SUMPAL+TEMPH(19)
           SUMPAP=SUMPAP+TEMPH(20)
           SUMSTS=SUMSTS+TEMPH(3)
           SUMSTL=SUMSTL+TEMPH(5)
           SUMFEM=SUMFEM+TEMPH(14)
C
           IF (LP5) THEN
              DEAD=TEMPH(1)-TEMPH(13)
C
              IF (DEAD.GT.0.0) THEN
                 TEMPH(2)=100.0*TEMPH(2)/DEAD
                 TEMPH(3)=100.0*TEMPH(3)/DEAD
                 TEMPH(4)=100.0*(TEMPH(4)+TEMPH(18))/DEAD
                 TEMPH(5)=100.0*TEMPH(5)/DEAD
                 TEMPH(6)=100.0*(TEMPH(6)+TEMPH(19))/DEAD
                 TEMPH(7)=100.0*(TEMPH(7)+TEMPH(20))/DEAD
                 TEMPH(17)=100.0*TEMPH(17)/DEAD
              ELSE
                 DO 350 K=2,7
                    TEMPH(K)=-0.0
  350            CONTINUE
                 TEMPH(17)=-0.0
              ENDIF
              GENMOR=100.0*DEAD/TEMPH(1)
              IF (TEMPH(14).GT.0.0) THEN
                 TEMPH(15)=TEMPH(15)/TEMPH(14)
                 TEMPH(16)=TEMPH(16)/TEMPH(14)
              ELSE
                 TEMPH(15)=0.0
                 TEMPH(16)=0.0
              ENDIF
              DEFNEW=-1.0
              IF (TEMPH(10).GT.0.0) DEFNEW=100.0*(1.0-(TEMPH(8)
     &            /TEMPH(10)))
              DEFOLD=-1.0
              IF (TEMPH(11).GT.0.0) DEFOLD=100.0*(1.0-(TEMPH(9)
     &            /TEMPH(11)))    
              RINC=TEMPH(12)/TEMPH(1)
              EGGD=(TEMPH(1)*GPERM2)/(EGMASS*TEMPH(11))
              WRITE (JOBWP5,380) NPLT,IYRCUR,TRESP(IH),(TEMPH(K),K=2,6),
     &           TEMPH(17),TEMPH(7),GENMOR,EGGD,DEFNEW,DEFOLD,RINC,
     &           TEMPH(1)
  380 FORMAT    (A26,',',1X,I4,',',1X,A3,',',1X,3(F5.1,','),
     &           3(F5.1,','),F5.1,',',2X,F5.1,',',2X,
     &           F5.1,',',1X,F5.0,',',F5.0,',',F6.2,',',F9.0,',')
           ENDIF
           DO 390 I=1,20
  390      TEMPH(I)=0.0
        ENDIF
      ENDIF
C
  300 CONTINUE
C
C  PRINT STRIPPED OUTPUT TABLE FOR GRAPHICS DISPLAY
C
      IF (LP6) THEN
C
C        APPARENT SURV. CALCS
C
         SURV=SUMEGG
         IF (SURV.GT.0.0) THEN
            APPB=100.0-(SUMEL2*100.0/SURV)
            IAPPB=NINT(APPB)
            RINC=SNEWEG/SUMEGG
            IF (SUMFEM.GT.0.0) THEN
               FECUND=SNEWEG/SUMFEM
            ELSE
               FECUND=0.0
            ENDIF
         ELSE
            IAPPB=0
            RINC=0.0  
         ENDIF
         SURV=SURV-SUMEL2
         IF (SURV.GT.0.0) THEN
            APPC=100.0-(SUMSTS*100.0/SURV)
            IAPPC=NINT(APPC)
            APPD=100.0-(SUMPRS*100.0/SURV)
            IAPPD=NINT(APPD)
         ELSE
            IAPPC=0
            IAPPD=0
         ENDIF
         SURV=SURV-SUMSTS-SUMPRS
         IF (SURV.GT.0.0) THEN
            APPE=100.0-(SUMSTL*100.0/SURV)
            IAPPE=NINT(APPE)
            APPF=100.0-(SUMPRL*100.0/SURV)
            IAPPF=NINT(APPF)
            APPG=100.0-(SUMPAL*100.0/SURV)
            IAPPG=NINT(APPG)
            APPH=100.0-(SUMSPR*100.0/SURV)
            IAPPH=NINT(APPH)
         ELSE
            IAPPE=0
            IAPPF=0
            IAPPG=0
            IAPPH=0
         ENDIF
         SURV=SURV-SUMSTL-SUMPRL-SUMPAL-SUMSPR
         IF (SURV.Gt.0.0) THEN
            APPI=100.0-(SUMPRP*100.0/SURV)
            IAPPI=NINT(APPI)
            APPJ=100.0-(SUMPAP*100.0/SURV)
            IAPPJ=NINT(APPJ)
         ELSE
            IAPPI=0
            IAPPJ=0
         ENDIF
C
C        actual mortality calculations
C
         IEGGS=NINT(SUMEGG)
         IF (SUMPOT.GT.0.0) THEN
            X=100*(1.0-(SUMACT/SUMPOT))
         ELSE
            X=0.0
         ENDIF
         NEWDEF=NINT(X)
         IF (EGMASS*SUMFOL.GT.0.0) THEN
            EGGD=(SUMEGG*GPERM2)/(EGMASS*SUMFOL)
         ELSE
            EGGD=0.0
         ENDIF
         DEAD=SUMEGG-SUMADT
         IF (DEAD.GT.0.0) THEN
           SUMEL2=100.0*SUMEL2/DEAD
           SUMSTV=100.0*SUMSTV/DEAD
           SUMPRE=100.0*SUMPRE/DEAD
           SUMPAR=100.0*SUMPAR/DEAD
           SUMSPR=100.0*SUMSPR/DEAD
         ELSE
           SUMEL2=-0.0
           SUMSTV=-0.0
           SUMPRE=-0.0
           SUMPAR=-0.0
           SUMSPR=-0.0
         ENDIF
         RINC10=-0.0
         IF (SUMEGG.GT.0.0) RINC10=10.0*SNEWEG/SUMEGG
         ISEL2=NINT(SUMEL2)
         ISSTV=NINT(SUMSTV)
         ISPRE=NINT(SUMPRE)
         ISPAR=NINT(SUMPAR)
         ISSPR=NINT(SUMSPR)

         WRITE (JOBWP6,600) NPLT,IYRCUR,NEWDEF,EGGD,(IOUT6A(I),I=1,3),
     &     IYRCUR,ISEL2,ISSTV,ISPRE,ISPAR,ISSPR,RINC10
  600    FORMAT (A26,', 'I5,',',2X,I6,',',2X,F6.1,',',3(5X,A3,','),
     &           1X,I5,',',5(5X,I3,','),2X,F6.2,',')
      ENDIF
C
C  PRINT MORTALITY/SURVIVAL SUMMARY FILE (FOR EXPORT)
C
      IF (LP7) THEN
         IF (LASTYR+1.EQ.IYRCUR) THEN
            LASTYR=IYRCUR
         ELSE
            IDIFF=IYRCUR-LASTYR-1
            DO 620 I=1,IDIFF
                IYEARK=LASTYR+I
                WRITE (JOBWP7,610) NPLT,IYEARK
  610           FORMAT (A26,',',I5,',',14('          '))
  620       CONTINUE
            LASTYR=IYRCUR
         ENDIF
         SUMALE=SUMADT-SUMFEM
         X=FWSURV*WCOLDW
         OVWINT=SUMEGG*(1.0-X)
         SPRING=SUMEL2-OVWINT
         RINC=0.0
         IF (SUMEGG.GT.0.0) RINC=SNEWEG/SUMEGG
         FECUND=0.0
         IF (SUMFEM.GT.0.0) FECUND=SNEWEG/SUMFEM
         WRITE (JOBWP7,640) NPLT,IYRCUR,SUMALE,SUMPAP,SUMPRP,SUMPAL,
     &     SUMPRL,SUMSTL,SUMSPR,SUMPRS,SUMSTS,SPRING,OVWINT,SUMEGG,
     &     RINC,FECUND
  640    FORMAT (A26,',',I5,',',14(F9.1,','))
      ENDIF
C
C  PRINT NEW TABLE 2 (G_CROWN) FOR THIS YEAR
C
      IF (LP2) WRITE (JOBWP2,650) NPLT,IYRCUR,(KCROWN(K),K=1,3),
     &  IYRCUR,(KCROWN(K),K=4,6),IYRCUR,(KCROWN(K),K=7,9),IYRCUR,
     &  (KCROWN(K),K=10,12),IYRCUR,(KCROWN(K),K=13,15),IYRCUR,
     &  (KCROWN(K),K=16,18)
  650 FORMAT (A26,',',6(1X,I4,',',3(2X,I3,',')))
C
      RETURN
      END
