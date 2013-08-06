      SUBROUTINE BRTREG
C**********************************************************************
C  **BRTREG       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRTREG drives the Blister Rust Model in the cycling process.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  20-APR-1999 Lance R. David (FHTET)
C     Syntax changes to eliminate warnings from Lahey FORTRAN 90 (LF90)
C     compiler.
C  13-MAY-1999 Lance R. David (FHTET)
C     The height to base of crown (BRHTBC) calculation corrected to use
C     FVS normal height and crown ratio for all trees, even topkilled.
C     So, actual crown length is based on normal height and crown ratio
C     less any topkill.
C  25-MAY-1999 Lance R. David (FHTET)
C     Added debug to activity processing.
C     The check to ensure that the new height to base of crown calculated
C     from FVS crown ratio is higher than the current height maintained
C     within the model (BRHTBC) before resetting had been removed.
C     I do not know why it was removed, but it eliminated the effects
C     of pruning. The height to base of crown is only allowed to increase
C     and since this model does not actually change the FVS crown ratio
C     variable, this check must be in place.
C  15-SEP-2000 Lance R. David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 40) and species temp index variable (I3)
C     are new.
C  14-DEC-2000 Lance R. David (FHTET)
C     Relocated call to BRIBA from within species loop to before loop.
C     Added call to BGRI subroutine to update tree growth index- GI(x).
C  06-MAR-2001 Lance R. David (FHTET)
C     Added processing of pathological pruning option to PRUNE activity.
C  15-MAR-2001 Lance R. David (FHTET)
C     Added DEVFACT activity (code 1010) processing.
C  22-MAR-2001 Lance R. David (FHTET)
C     Changed processing structure so that all trees are processed each
C     year of the cycle instead of processing a tree for all years of
C     the cycle and then going to next tree. This enables annual
C     monitoring of the stands infection level. Maximum stand infection
C     level PIMAX calculation was added and PIMAX was also added to
C     BRECAN parameter list. PIMAX is used for assigning escape (status
C     code 9) trees and truncating stand infection level.
C  24-APR-2001 Lance R. David (FHTET)
C     Added species dimension to PRPSTK and RESIST arrays in Stock
C     activity processing.
C  02-MAY-2001 Lance R. David (FHTET)
C     Added species dimension to calculations using BRNTRECS and
C     category variables.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C  10-MAY-2001 Lance R. David (FHTET)
C     Calculations of AVGRI and AVGGI were missing the cycle length IFINT
C     in the denominator, it was added.
C  15-MAY-2001 Lance R. David (FHTET)
C     Added last year of cycle mortality accounting at stmt lable 36
C  06-NOV-2002 Lance R. David
C     Added call to BRICAL, new routine for calculating Rust Index.
C  07-MAY-2013 Lance R. David (FMSC)
C     Added statement label 410 for exit from routine when model
C     is inactive.
C**********************************************************************

C.... Common include files

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations

      LOGICAL BRGO, DEBUG, LREDF
      REAL    CRATIO, HTBC, PRMS(6), RISUM(NBRSP), GISUM(NBRSP)
      INTEGER I1, I2, I3, I4, MYACTS(10)

C.... Activities and related codes are as follows:
C....
C.... prune----1001   exspecs-1004   brtlst-1007   devfact-1010
C.... prnspecs-1002   ribes---1005   brclst-1008
C...  excise---1003   inact---1006   stock--1009 

      DATA MYACTS/
     &     1001,1002,1003,1004,1005,1006,1007,1008,1009,1010/

C.... See if Blister Rust Model is being used in this sumulation.
C.... If not or there are no trees, return.

      CALL BRATV(BRGO)
      IF(.NOT. BRGO .OR. ITRN .EQ. 0) GO TO 410

C.... Is debug requested?

      CALL DBCHK(DEBUG,'BRTREG',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT('Entering subroutine BRTREG: cycle = ',I2)

C.... Initialize variables which control execution of activities.
C.... Find out if any of the following activities have been scheduled
C.... for this cycle:
C.... PRUNE, PRSPEC, EXCISE, EXSPEC, RIBES, INACT, BRTLST, BRCLST,
C.... STOCK, DEVFACT

      LPRGO  = .FALSE.
      LEXGO  = .FALSE.
      LPATPR = .FALSE.
      LPRUN  = .FALSE.
      LCLEN  = .FALSE.
      BRTL   = .FALSE.
      BRCL   = .FALSE.

C.... Find activities to be performed this cycle, if any.

      CALL OPFIND(10,MYACTS,NTODO)
      IF(DEBUG) WRITE(JOSTND,*) ' ACTIVITIES NTODO=',NTODO
      IF(NTODO.GT.0) THEN
         DO 195 K=1,NTODO

C....       Get activity

            CALL OPGET(K,6,KDT,IACTK,NP,PRMS)

            IF(DEBUG) WRITE(JOSTND,*)' IACTK=',IACTK,' PRMS=',PRMS

            IF(IACTK.EQ.1002) GO TO 110
            IF(IACTK.EQ.1003) GO TO 115
            IF(IACTK.EQ.1004) GO TO 120
            IF(IACTK.EQ.1005) GO TO 125
            IF(IACTK.EQ.1006) GO TO 130
            IF(IACTK.EQ.1007) GO TO 135
            IF(IACTK.EQ.1008) GO TO 140
            IF(IACTK.EQ.1009) GO TO 145
            IF(IACTK.EQ.1010) GO TO 150

C....       PRUNE keyword

            IF(DEBUG) WRITE(JOSTND,*) ' PRUNE: PRMS=',PRMS
            SRATE(1)=PRMS(1)
            LPRGO=.TRUE.
            IF(INT(PRMS(2)).EQ.1) LPRUN  = .TRUE.
            IF(INT(PRMS(3)).EQ.1) LCLEN  = .TRUE.
            IF(INT(PRMS(4)).EQ.1) LPATPR = .TRUE.
            GO TO 192

C....       PRNSPECS keyword

  110       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' PRNSPECS: PRMS=',PRMS
            HTPRPR=PRMS(1)
            HTMAX(1)=PRMS(2)*30.48
            OUTDST=PRMS(3)*2.54
            OUTNLD=PRMS(4)*2.54
            GO TO 192

C....       EXCISE keyword

  115       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' EXCISE: PRMS=',PRMS
            SRATE(2)=PRMS(1)
            LEXGO=.TRUE.
            GO TO 192

C....       EXSPECS keyword

  120       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' EXSPECS: PRMS=',PRMS
            EXDMIN=PRMS(1)
            HTMAX(2)=PRMS(2)*30.48
            GIRMAX=PRMS(3)
            GIRMRT=PRMS(4)
            HTMIN=PRMS(5)*2.54
            GO TO 192

C....       RIBES keyword

  125       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' RIBES: PRMS=',PRMS
            RIBUS(1,1)=PRMS(1)
            RIBUS(2,1)=PRMS(2)
            RIBUS(1,2)=PRMS(3)
            RIBUS(2,2)=PRMS(4)
            RIBUS(1,3)=PRMS(5)
            RIBUS(2,3)=PRMS(6)

C....       Call BRIBES to calculate the rust index reduction factor
C....       based on change in number of ribes bushes or a new stand
C....       rust index depending on whether a value for old number of
C....       bushes/acre has been provided.  Either the new rust index or
C....       reduction factor should remain in effect until changed by a
C....       subsequent RIBES keyword.

            CALL BRIBES(REDFAC,LREDF)
            GO TO 192

C....       INACT keyword

  130       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' INACT: PRMS=',PRMS
            RATINV(1)=PRMS(1)
            RATINV(2)=PRMS(2)
            GO TO 192

C....       BRTLST keyword

  135       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' BRTLST'
            BRTL=.TRUE.
            GO TO 192

C....       BRCLST keyword

  140       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' BRCLST'
            BRCL=.TRUE.
            GO TO 192

C....       STOCK keyword
C....             1-species, 2-stock type, 3-proportion, 4-resistance factor

  145       CONTINUE
            IF(DEBUG) WRITE(JOSTND,*) ' STOCK: PRMS=',PRMS
            I3 = PRMS(1)
            I4 = BRSPM(I3)
            ICLS = PRMS(2)
            PRPSTK(I4,ICLS) = PRMS(3)
            RESIST(I4,ICLS) = PRMS(4)
            GO TO 192

C....       DEVFACT keyword

  150       CONTINUE
            I3 = PRMS(1)
            I4 = BRSPM(I3)
            DFACT(I4,1) = PRMS(2)
            DFACT(I4,2) = PRMS(3)
            DFACT(I4,3) = PRMS(4)
            DFACT(I4,4) = PRMS(5)

C....       Done with that particular activity.

  192       CONTINUE
            CALL OPDONE(K,IY(ICYC))
  195    CONTINUE
      ENDIF

C.... If no tree records, then exit.

      IF(ITRN.EQ.0) GO TO 300

C.... Initialize tree category variables to be used in BRTSTA.
C.... Initialize other summing variables.

      DO 196 I3 = 1, MAXSP
         IF (BRSPM(I3) .EQ. 0) GO TO 196
         I4=BRSPM(I3)
         TBRHST(I4)=0.0
         TBRCLN(I4)=0.0
         TBRNOL(I4)=0.0
         TBRPRN(I4)=0.0
         TBREXC(I4)=0.0
         TBRNOS(I4)=0.0
         TBRGIR(I4)=0.0
         TBRMRT(I4)=0.0
         STSUM(I4)=0.0
         RISUM(I4)=0.0
         GISUM(I4)=0.0
  196 CONTINUE

      DO 200 II=1,10
         D2CLN(II)=0.0
         D2WP(II)=0.0
         D2NOL(II)=0.0
         D2PRN(II)=0.0
         D2EXC(II)=0.0
         D2NOS(II)=0.0
         D2GIR(II)=0.0
         D2DED(II)=0.0
  200 CONTINUE

C.... Call BRCREM to remove cankers in this cycle.

      CALL BRCREM

C.... Call BRSTYP to specify mix of stock types.

      CALL BRSTYP

C.... Calculate Rust Index (RIDEF) using Basal Area.
C.... The calculation of new rust index value is controlled by the
C.... rust index assignment method (RIMETH) chosen for the run using
C.... the RUSTINDX keyword.  RIMETH = 0 or 1 requests that RI be
C.... set at initialization only. RIMETH = 2 requests that RI be
C.... calculated every cycle based on the new stand BA.

      IF(RIMETH.EQ.2) CALL BRIBA

C.... 06-NOV-2002
C.... Calculate Rust Index based on exposure time (stand age).

      IF (RIMETH .GE. 3) CALL BRICAL

C.... Calculated the infection level maximum. When the proportion
C.... of infected trees reaches this point, clean trees will be
C.... tagged as reserve/escape trees.

      PIMAX=1.0-(EXP(-(100/(1+100*DFACT(1,1)))))

      IF(DEBUG) THEN
         WRITE(JOSTND,*) ' DFACT(1,1)=',DFACT(1,1),' PIMAX=',PIMAX
         WRITE(JOSTND,*)
     &   ' PITCA=',PITCA,' TRETN=',TRETN,' THPROB=',THPROB
      ENDIF

C.... Process host pines in the treelist.  If none, return.
C.... Set number of host trees counter.
C     BRNTRECS = 0

C.... Start species loop

      DO 40 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 40

      I1=ISCT(I3,1)
      IF(I1.EQ.0) GO TO 40
      I2=ISCT(I3,2)

C.... Set blister rust species index
      I4=BRSPM(I3)

      BRNTRECS(I4)=I2-I1+1

C.... Process trees in the treelist on a yearly basis over the
C.... cycle length period IFINT.

C.... Start of year loop.
      DO 34 K=1,IFINT

C.... Start of tree loop.
      DO 35 L=I1,I2
         J=IND1(L)
         IF(DEBUG) WRITE(JOSTND,*) ' J=',J,' L=',L,' SPC=',BRSPC(I4)
C....    Increment tree age.
         BRAGE(J)=BRAGE(J)+1.0
         NLCAN=ILCAN(J)
         BRDG=DG(J)*2.54

C....    Number of expected cankers is initialized first year.
         IF(K.EQ.1) ESTCAN(J)=0.0

         IF(IBRSTAT(J).EQ.7) THEN

C....       Tree is dead and will not be included in the calculation
C....       of averages for this cycle. Number of trees (divisor) is
C....       reduced by 1.
C....       Status code is temporarily set to 77 so that reduction to
C....       counter does not occur again in later years.

            IBRSTAT(J)=77
            BRNTRECS(I4)=BRNTRECS(I4)-1
            GO TO 35
         ELSE IF(IBRSTAT(J).EQ.77) THEN
            GO TO 35
         ENDIF

C....    Assign rust index value to the tree including the resistance
C....    factor for the stock type and the rust index adjustment factor
C....    from initial tree conditions.

         ICLS=ISTOTY(J)
         RI(J)=RIDEF*RESIST(I4,ICLS)*RIAF(I4)

         IF(DEBUG) WRITE(JOSTND,*) ' RI=',RI(J),' RIDEF=',RIDEF,
     &             ' RESIST=',RESIST(I4,ICLS),' RIAF=',RIAF(I4)

C....    If a reduction factor has been calculated, apply it.

         IF(LREDF) RI(J)=RI(J)*REDFAC

         GISUM(I4)=GISUM(I4)+GI(J)
         RISUM(I4)=RISUM(I4)+RI(J)

C....       Units of measure in following calculations are:
C....       feet for HT, HTG, HNEW; inches for DBH, DG, DNEW;
C....       meters for BRHT, BRHIN, BRHNU, BRHTOL, STHT;
C....       Top killed trees have their heights calculated based
C....       on the height to truncation.
C....       ITRUNC is stored in hundreds of feet.

            PROP=FLOAT(K)/IFINT
            HNEW=HT(J)+(HTG(J)*PROP)
            DNEW=DBH(J)+(DG(J)*PROP)

            IF(ITRUNC(J).EQ.0) THEN
               BRHT=HT(J)*.3048
               BRHIN=HTG(J)*.3048
               BRHNU=BRHT+BRHIN
               BRHTOL=BRHT
               STHT=BRHTOL+(BRHIN*PROP)
            ELSE
               BRHT=(FLOAT(ITRUNC(J))/100.0)*.3048
               BRHIN=0.0
               BRHNU=BRHT
               BRHTOL=BRHT
               STHT=BRHT
            ENDIF

C....       Calculate height to base of crown from FVS crown ratio.
C....       BRHTBC is in centimeters.
C....       Crown ratio and length is based on normal tree height in FVS,
C....       not truncated (topkill) height.
C....       To facilitate the activity of pruning, a new height to crown base
C....       is only applied when the new value calculated is greater than
C....       the current value. It was decided by the developing group that
C....       to actually change the crown ratio in FVS may have greater affect
C....       than expected; therefore, BRHTBC is maintained in this manner.

            CRATIO=(FLOAT(ICR(J)))/100.0
            HTBC=(HNEW*30.48)*(1.0-CRATIO)
            IF (BRHTBC(J) .LT. HTBC) BRHTBC(J)=HTBC

C....       Call BRGI to calculate tree growth index valuethis year using the
C....       tree's actual height and age at this point in time.
C....       Only the GI value is utilized from this call.

            IIAG = BRAGE(J)
            CALL BRGI(IIAG,STHT,GIBR,TBSUM)
            GI(J) = GIBR

C....       Call BRSTAR to calculate tree target for this year using the
C....       tree's actual height at this point in time.
C....       Accumulate individual tree targets and stand target.

            CALL BRSTAR(STHT,STAR)

            TSTARG(J)=TSTARG(J)+STAR
            STSUM(I4)=STSUM(I4)+TSTARG(J)

C....       No more processing for escape/reserve trees.
            IF(IBRSTAT(J).EQ.9) GO TO 36

C....       Call BRECAN to calculate # new cankers expected for tree
C....       this year and accumulate for cycle.


            CALL BRECAN(J,BRHNU,RI(J),STAR,STHT,PROP,PIMAX,CANEXP)
            ESTCAN(J)=ESTCAN(J)+CANEXP

C....       Call BRCGRO to grow cankers this year.
C....       Cankers added this cycle are not grown until next cycle;
C....       i.e. BRCGRO will grow NLCAN cankers where NLCAN has been set
C....       to what ILCAN was last cycle even though BRECAN has already
C....       updated ILCAN for this cycle with the expected new cankers -
C....       those will be grown next cycle, see???

            IF(NLCAN.GT.0)
     &         CALL BRCGRO(J,PROP,BRHT,BRHIN,BRDG,NLCAN,HNEW,DNEW)

C....       During the last year of the cycle, host trees that were
C....       not killed by BR this cycle need to have the mortality
C....       imposed by FVS or other model captured so that the total
C....       historic mortality of the host species is accurate.
C....       Also, the number of trees variable needs reduced for 
C....       trees killed during the last year of the cycle, the historical
C....       mortality will have already been addressed in this case.

   36       IF(K .EQ. IFINT) THEN
              IF(IBRSTAT(J) .EQ. 7) THEN
                BRNTRECS(I4) = BRNTRECS(I4) - 1
              ELSE
                TBRHMR(I4) = TBRHMR(I4) + WK2(J)
                IF(DEBUG) WRITE(JOSTND,*)
     &            'TBRHMR=',TBRHMR(I4),' WK2=',WK2(J)
              ENDIF
            ENDIF

C....    End of tree loop.
   35    CONTINUE

C.... End of year loop.

   34 CONTINUE

C.... End of species loop
   40 CONTINUE

C.... The reset dead tree status codes (IBRSTAT).

      DO 60 I3 = 1, MAXSP
         IF (BRSPM(I3) .EQ. 0) GO TO 60
         I1=ISCT(I3,1)
         IF(I1.EQ.0) GO TO 60
         I2=ISCT(I3,2)
         DO 55 L=I1,I2
            J = IND1(L)
            IF (IBRSTAT(J) .EQ. 77) IBRSTAT(J)=7
   55    CONTINUE
   60 CONTINUE

C.... If a rust index reduction factor has been calculated, apply it
C.... to the stand rust index.

      IF(LREDF) RIDEF=RIDEF*REDFAC

C.... Call BRCRED to reduce crowns on trees girdled by Blister Rust.

      CALL BRCRED

C.... If all host pine have been killed, set values to 0.  Otherwise,
C.... calculate average rust index, growth index, and stand target area.

      DO 80 I3 = 1, MAXSP
         IF(BRSPM(I3).EQ.0) GO TO 80
C....    Set blister rust species index
         I4=BRSPM(I3)
         IF(BRNTRECS(I4).EQ.0) THEN
            AVGRI(I4)=0.0
            AVGGI(I4)=0.0
            AVGSTS(I4)=0.0
         ELSE
            AVGRI(I4)=RISUM(I4)/FLOAT(BRNTRECS(I4)*IFINT)
            AVGGI(I4)=GISUM(I4)/FLOAT(BRNTRECS(I4)*IFINT)
            AVGSTS(I4)=STSUM(I4)/FLOAT(BRNTRECS(I4)*IFINT)
         ENDIF
   80 CONTINUE

C.... Call BRUPDT to update blister rust variables.

      CALL BRUPDT

C.... Common return.

  300 CONTINUE
      IF(DEBUG) WRITE(JOSTND,400) ICYC
  400 FORMAT ('Leaving subroutine BRTREG: cycle = ',I2)
  410 RETURN
      END
