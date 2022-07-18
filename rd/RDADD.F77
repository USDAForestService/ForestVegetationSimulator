CODE SEGMENT RDADD
C----------
C RD $Id$
C----------
      LOGICAL BB1GO, BB2GO, LPLINF, REINGO, RRMAN, RRTINV, WINGO,
     &        LBORAX(2), LPAREA(ITOTRR), LSPFLG(3)
      
      INTEGER DEDAGE(5), IANPLT(ITOTRR,50), ICENSP(ITOTRR,100), 
     &        IDPLOT(MAXTRE), INFISD(ITOTRR,50), IPRFL(MAXTRE),
     &        IRDPLT(ITOTRR,50), ISDATE(3), ISDOUT,
     &        JCENSP(ITOTRR,100), LONECT(ITOTRR), MINRR, MAXRR, PINT
      
      REAL    ANUINF(ITOTRR,50), BBKILL(4,IRRTRE), BODBH, BOTRT,
     &        DBHOUT(ITOTRR,2,5,3), DBHUIN(ITOTRR,2,5,3),
     &        DECFN(ITOTRR,2,2), DPROB(IRRTRE,4,2), FFPROB(IRRTRE,2),
     &        OAKL(3,IRRTRE), PLPROP(ITOTRR,50), PROAKL(3,IRRTRE),
     &        RINNF(ITOTRR), RINUF(ITOTRR), RISTU(ITOTRR), 
     &        RRNEW(ITOTRR), RSITFN(ITOTRR,2), 
     &        RTOUT(ITOTRR,2,5,3), RTUIN(ITOTRR,2,5,3), SDISLP,
     &        SDNORM, SPDBH(ITOTRR), SPINF(ITOTRR), SPPROP(ITOTRR),
     &        SPTRAN(ITOTRR), SPYTK(ITOTRR), SS, STOUT(ITOTRR,2,5,3),
     &        STUIN(ITOTRR,2,5,3), YINCPT, YRSITF(ITOTRR,2,2)

      DOUBLE PRECISION S0, S1

      COMMON /RDADD/ ANUINF, BB1GO, BB2GO, BBKILL, BODBH, BOTRT,
     &               DBHOUT, DBHUIN, DECFN, DEDAGE, DPROB, FFPROB,
     &               IANPLT, ICENSP, IDPLOT, INFISD, IPRFL, IRDPLT,
     &               ISDATE, ISDOUT, JCENSP, LBORAX, LONECT, LPAREA,
     &               LPLINF, LSPFLG, MINRR, MAXRR, OAKL, PINT,
     &               PLPROP, PROAKL, REINGO, RINNF, RINUF, RISTU,
     &               RRMAN, RRNEW, RRTINV, RSITFN, RTOUT, RTUIN,
     &               SDISLP, SDNORM, SPDBH, SPINF, SPPROP, SPTRAN,
     &               SPYTK, STOUT, STUIN, WINGO, YINCPT, YRSITF

      COMMON /RRANN/ S0, S1, SS
C
C     ANUINF  -- ALL NEW INFECTIONS OCCURING IN ONE INSIDE INFECTION
C                SIMULATION
C     BB1GO   --
C     BB2GO   --
C     BBKILL  -- TREES KILLED BY BARK BEETLES, BY TREE RECORD,TYPE
C     BODBH   -- MIN. DBH FOR BORAXING
C     BOTRT   -- PROPORTION BORAXED
C     DBHOUT(# root disease species, wood type: [resinous,
C            non-resionous], size class, 1) -- 
C                DBH of spore infected newly created stumps outside
C                centers.  The fourth element being set to 1 indicates
C                that the stumps were created in a short cycle (length
C                of cycle is less then 5 years) and the stumps need to 
C                wait one more cycle before they become centers. 
C     DBHOUT(# root disease species, wood type: [resinous,
C            non-resionous], size class, 2) --
C                DBH of spore infected newly created stumps outside
C                centers.  The fourth element being set to 2 indicates
C                that the cycle length was greater then 5 and that the
C                spore created stumps should become centers in the 
C                following cycle.
C     DBHUIN(# root disease species, wood type: [resinous,
C            non-resionous], size class, 1) --
C                DBH of spore infected newly created stumps inside
C                centers.  The fourth element being set to 1 indicates
C                that the stumps were created in a short cycle (length
C                of cycle is less then 5 years) and the stumps need to 
C                wait one more cycle before they are added to the
C                center stump list.
C     DBHUIN(# root disease species, wood type: [resinous,
C            non-resionous], size class, 2) --
C                DBH of spore infected newly created stumps inside
C                centers.  The fourth element being set to 2 indicates
C                that the cycle length was greater then 5 and that the
C                spore created stumps should be added to the center
C                stump list in the following cycle.
C     DECFN   -- SLOPE AND INTERCEPT OF THE ROOT DECAY FUNCTION
C                (FUNCT. OF INFECTED ROOT RADIUS)
C             -- (,,1) FOR DBH <= 12, (,,2) FOR DBH>12 (ONLY DIFFERENT
C                 FOR ARMIL.& PHELL.
C     DEDAGE  -- TIME SINCE DEATH OF INFECTED TREES AND STUMPS IN THE
C                INVENTORY (BY SIZE CLASS)
C     DPROB   -- DEAD TREE LIST:(i,,) = TREE RECORD, (,,1) = UNINFECTED,
C                (,,2) = INFECTED
C                (,1,) = ROOT DISEASE, (,2,) = BARK BEETLES,
C                (,3,) = WINDTHROW, (,4,) = NATURAL
C     FFPROB  -- The purpose of FFPROB for each tree record is to
C                provide the spread-rate model with a density of live
C                trees outside of root rot disease areas ("FPROB").
C                This takes into account the greater mortality caused
C                by BB4 in the "Fringe" area that is immediately
C                adjacent to disease areas (within a distance 'D' of
C                the root rot disease areas of the type to which the
C                tree species is susceptible.  The distance 'D'
C                is the average root diameter of all the trees outside
C                of those disease areas that are susceptible to that
C                root disease type).
C                FFPROB (RECORD,1) is the lower of a) the density of
C                live trees in the fringe area after the effects of
C                bark beetle activity and windthrow during the previous
C                growth cycle are removed, or b) the average density of
C                live trees outside of disease areas when the effects
C                of all sources of mortality in the last growth cycle
C                are removed.
C                FFPROB (RECORD,2) is the density of live trees in the
C                fringe area when the effects of the bark beetle
C                activity and windthrow that will occur in the current
C                growth cycle are removed.
C     IANPLT  -- PLOT ID FOR INFECTED PLOTS, BY DISEASE TYPE
C     ICENSP  --
C     IDPLOT  -- PLOT ID FOR EACH TREE RECORD THAT IS INSIDE AN
C                INFECTED PLOT
C     INFISD  -- # INFECTION SOURCES PLACED IN THE INSIDE CENTER SIM
C                OF INFECTION
C     IPRFL   --
C     IRDPLT  --
C     ISDATE(3) -- Array that holds the date on which stand entries
C                occur.  It only keeps track of the stand entries
C                for 3 cycles.  (1=current cycle, 2=previous cycle,
C                3=2 cycles before).  This is used along with the array
C                LSPFLG to test when stumps should move from the
C                spore infected stump list to the standard infected
C                stump list or the new centers list. 
C     ISDOUT  -- FILE NUMBER FOR INSIDE INFECTION SIMULATION OUTPUT
C     JCENSP  -- YEAR THAT A SPORE CREATED CENTER WAS CREATED
C     LBORAX  -- (,1) WAS THERE A KEYWORD?, (,2) HAS OPTION PROCESSOR
C                BEEN CHECKED YET THIS YEAR?
C     LONECT  -- 0=unassigned, 1=stand as one plot, 2=multiplots
C     LPAREA  --
C     LPLINF  -- INITIALIZE INFECTION USING PROP OF PLOTS INFECTED
C                (PLOTINF KEYWORD)
C     LSPFLG(3) -- Flag that specifies whether a stand entry occurred
C                in the current cycle or one of the previous 2 cycles.
C                (1=current cycle, 2=previous cycle, 3=2 cycles before).
C                This is used along with the array ISDATE to test when
C                stumps should move from the spore infected stump list
C                to the standard infected stump list or the new
C                centers list. 
C     MAXRR   -- HIGHEST NUMBER DISEASE USED IN CURRENT SIMULATION
C     MINRR   -- LOWEST NUMBER DISEASE USED IN CURRENT SIMULATION
C     OAKL    -- OTHER AGENT KILL TREELIST (BARK BEETLES AND WINDTHROW)
C     PINT    -- NUMBER OF YEARS THE PROBABILITY OF INFECTION IS
C                DEFINED FOR (10 YEARS)
C     PROAKL  -- Previous iteration OTHER AGENT killed treelist
C     REINGO  --
C     RINNF   -- NUMBER OF INFECTED TREE RECORDS READ FROM TREELIST
C     RINUF   -- NUMBER OF UNINFECTED TREE RECORDS READ FROM TREELIST
C     RISTU   -- NUMBER OF INFECTED STUMP RECORDS READ FROM TREELIST
C     RRMAN   -- FLAG THAT IS SET TO .TRUE. IF THE ROOT DISEASE MODEL
C                IS BEING USED.
C     RRNEW   -- MEAN LEVEL OF INFECTION FOR NEWLY INFECTED TREES
C     RRTINV  --
C     RSITFN  -- SLOPE AND INTERCEPT OF THE FUNCTION DEFINING ROOT
C                RADIUS AT WHICH TO STOP DECAYING FOR SOME TIME
C                (DEFINED BY YRSITF) (FUNCTION OF DBH)
C     RTOUT(# root disease species, wood type: [resinous,
C           non-resionous], size class, 1) --
C                Root radius of spore infected stumps outside centers.
C                The fourth element being set to 1 indicates
C                that the stumps were created in a short cycle (length
C                of cycle is less then 5 years) and the stumps need to 
C                wait one more cycle before they can become centers.
C     RTOUT(# root disease species, wood type: [resinous,
C           non-resionous], size class, 2) --
C                Root radius of spore infected stumps outside centers.
C                The fourth element being set to 2 indicates
C                that the cycle length was greater then 5 and that the
C                spore created stumps should become centers in the 
C                following cycle.
C     RTUIN(# root disease species, wood type: [resinous,
C           non-resionous], size class, 1) --
C                Root radius of spore infected stumps inside centers.
C                The fourth element being set to 1 indicates
C                that the stumps were created in a short cycle (length
C                of cycle is less then 5 years) and the stumps need to 
C                wait one more cycle before they are added to the
C                center stump list.
C     RTUIN(# root disease species, wood type: [resinous,
C           non-resionous], size class, 2) --
C                Root radius of spore infected stumps inside centers.
C                The fourth element being set to 2 indicates
C                that the cycle length was greater then 5 and that the
C                spore created stumps should be added to the center
C                stump list in the following cycle.
C     SDISLP  -- SLOPE OF THE RELATIONSHIP BETWEEN SDI AND A ROOT
C                MULTIPLIER
C     SDNORM  -- "NORMAL" SDI: SDI FOR WHICH THE MULTIPLIER IS 1
C     SPDBH   -- Minimum dbh for creating a center through spore
C                infection
C     SPINF   --
C     SPPROP  --
C     SPTRAN  --
C     SPYTK   --
C     STOUT(# root disease species, wood type: [resinous,
C           non-resionous], size class, 1) --
C                Number of new spore infected stumps outside centers.
C                The fourth element being set to 1 indicates
C                that the stumps were created in a short cycle (length
C                of cycle is less then 5 years) and the stumps need to 
C                wait one more cycle before they become a center. 
C     STOUT(# root disease species, wood type: [resinous,
C           non-resionous], size class, 2) --
C                Number of new spore infected stumps outside centers.
C                The fourth element being set to 2 indicates
C                that the cycle length was greater then 5 and that the
C                spore created stumps should become centers in the 
C                following cycle.
C     STUIN(# root disease species, wood type: [resinous,
C           non-resionous], size class, 1) --
C                Number of new spore infected stumps inside centers.
C                The fourth element being set to 1 indicates
C                that the stumps were created in a short cycle (length
C                of cycle is less then 5 years) and the stumps need to 
C                wait one more cycle before they are added to the
C                center stump list.
C     STUIN(# root disease species, wood type: [resinous,
C           non-resionous], size class, 2) --
C                Number of new spore infected stumps inside centers.
C                The fourth element being set to 2 indicates
C                that the cycle length was greater then 5 and that the
C                spore created stumps should be added to the center
C                stump list in the following cycle.
C     WINGO   --
C     YINCPT  -- Y INTERCEPT OF THE LINE RELATING SDI AND ROOT
C                MULTIPLIER
C     YRSITF  -- SLOPE AND INTERCEPT OF THE FUNCTION DEFINING HOW LONG
C                TO STOP THE DECAY OF THE ROOT RADIUS BEFORE THE DEAD
C                THING DISAPPEARS (FUNCTION OF DBH)
C             -- (,,1) FOR DBH <= 12, (,,2) FOR DBH>12 (ONLY DIFFERENT
C                FOR ARMIL.& PHELL.
C
C     S0      --
C     S1      --
C     SS      --
C
C-----END SEGMENT
