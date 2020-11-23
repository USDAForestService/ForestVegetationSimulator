      SUBROUTINE FORTYP(IXF,SDIFIA)
      IMPLICIT NONE
C----------
C VBASE $Id$
C----------
C  THIS ROUTINE EXECUTES A DECISION TREE TO ESTIMATE FOREST TYPE
C  BASED OF STOCKING LEVELS.  THE STOCKING LEVELS ARE CALCULATED
C  IN THE ROUTINE STKVAL, AND STOCKING VALUES FOR GROUPS OF INITAL
C  FOREST TYPES ARE CATAGORIZED AND SUMMED.  THIS DECISION
C  TREE IS BASED ON ARNER 2001.  THIS FORTRAN CODE WAS DEVELOPED
C  FROM THE FIA ALGORITHM PROTOCOLS.  TO PROVIDE ADEQUATE CODE
C  DOCUMENTATION, STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
C  THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
C  END OF VALID FORTRAN STATEMENTS.
C
C
C  VARIABLES
C  SDIFIA  --  SDIMAX BASED ON FIA FOREST TYPE
C  IXF     --  SOURCE OF CALL TO FORTYP
C              = 1 - CALL IS FROM INITRE
C              = 2 - CALL IS FROM DISPLY, OR LDSUM (PPE)
C              = 3 - CALL IS FROM CRATET (SN VARIANT ONLY)
C  IXF         = 4 - CALL IS FROM MORTS  (SN VARIANT ONLY)
C  FTSDMX  --  FOREST TYPE CODE (FIATYP) AND SN MAXIMUM SDI (SDIFIA)
C       S  --  THIS ARRAY IS LOADED WITH STOCKING VALUES FROM
C              THE STKVAL ROUTINE ACCORDING TO INITIAL FOREST TYPE.
C              DIMENSIONED AS 210 IN ARNER 2001
C  LFLAGV  --  LOGICAL VARIABLE FOR VARIANT-SPECIFIC USE
C              SN VARIANT -- USED TO PREVENT (=.T.) RESETING FOREST
C              TYPE CALCULATION FROM CYCLE TO CYCLE. SET IN IINITRE
C              AND CARRIED IN PLOT.F77
C----------
C
COMMON BLOCKS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
      INCLUDE 'CONTRL.F77'
C
      INCLUDE 'VARCOM.F77'
C
      INCLUDE 'ARRAYS.F77'
C
C
C----------
C  DECLARATIONS
C----------
      LOGICAL DEBUG
      LOGICAL LFORVP
      INTEGER IRGN,NINTY5,NINETY
      INTEGER IXF,J,I,IFT,JFT,ISNVAR
      REAL S(210)
      REAL FTSDMX(141,2)
      REAL INSCDR,INTMAPLW,JACKPIN,JAPBLKP,JFBCDGFR,JUNIPRW,KNBPIN,
     &     LMBRPIN,LNGLFP,LNGLFSLH,LOBP,LOBSHRTP,LODGPOLE,LRCHPIN,
     &     LSPRFIR,LWELASCW,LWOKGMCY,MANGRV,MBB,MELUCA,MESQUITW,
     &     MISCWHWD,MISCWSFW,MNTRYPIN,MPLBASWD,MPLBCHYB,MTNBRSHW,
     &     MTNHMLK,MXDHWD,NOBLFIR,NROAK,NWHCDR
      REAL QOAK2,UPLAND,P1,P2,P3,EIGHTY,FIFTN,TEN,FIVE,HALF,PCOND
      REAL UPOAKHIC,UPMPBCBR,TOTSTK,OTHSPC,OTHHRD,OTHSFT,OTHALL,PINOAK
      REAL SWEETGUM,HOLLY,BEECH,BLKGUM,ECTNWD,WHASH,SLVRMPL
      REAL TWENTY,QUARTR,WCELM,AELM,SROAK,UPLOHWD,UPLOOAK,OTHEXHWD
      REAL EUCLPT,PALONIA,EXHWDS,PALM,PACMDRN,OTHWHWD,GNTCHNK
      REAL TROPHWDS,CLFLREL,TANOAK,TANOKLRL,EOAKWDLD,DOAKWDLD,CANYLO
      REAL COSTLO,GRAYPINE,BLUEOAK,ORGWHO,CLFBO,WSTOAK,BGLFMPL,REDALDR
      REAL ALDRMAPL,PAPRBRCH,BLSMPOP,ASPEN,ASPBRCH,ELMASHLO,BCWAYP,BLCH
      REAL ORGNASH,CTNWWILO,SLVMAELM,SGBELMGA,BAELMMAP,HRDWDS,OAKPIN
      REAL OAKHCK,WHOAK,BUROAK,CHSTOAK,SCRLTOAK,YP,BWALNUT,BLCST
      REAL REDMAPL,PSTBLJKO,CHBLSC,WOROHK,YPWORO,SCRUBOAK,SWGYP
      REAL SASFPRSM,OKGMCYP,ATLWCDR,SBSTRM,SCHCHBO,SWGWILO,CYPWTUP
      REAL OVRCUPWH,ELMASHCW,CTNWD,WILLOW,RBRCHSYC,SYCPCELM,OTHEXSWD
      REAL SCOTCHP,EXSFTWD,TAMRCK,BLKSPR,WHSPR,REDSPR,BLSMFIR,BFIRSPR
      REAL USPRFIR,ESPRFIR,PINJUNW,WJUNIPR,RMJUNIPR,ERCEDAR,PNYNJN
      REAL SPRUCEP,PITCHP,PONDP,TBLMTNP,SANDP,VIRGP,SHRTLFP,SLASHP
      REAL REDPIN,EHMLCK,EWHPIN,WHPHEM,RWJPIN,EPINE,GNTSEQ,REDWOOD
      REAL REDWDS,WHBRKPIN,FXTLPIN,BSHPPIN,SWHTPIN,OTHWPIN,STKASPR
      REAL STKSPHM,SUGRPIN,PORFCDR,PNDROSA,DGFRPIN,WRCEDAR,WLRCH,DGFIR
      REAL DGFRLR,WWHPIN,AKYLCDR,REDFIR,GRNDFIR,WHTFIR,PSLVRFIR
      REAL TRUFIR,WHMLCK,WHMLCKS,BLUSPR,ENGSPR,SALPFIR,ENGSAFIR
      REAL SPSAFIR,TRFIRSPR,DFLRWP,SFTWDS,FIATYP,SOUTH,SDIFIA
C
C  BLOCK DATA STATEMENTS
      DATA ((FTSDMX(I,J),J= 1,1),I=  1,100) /
     & 101, 102, 103, 104, 105, 121, 122, 123, 124, 125,
     & 126, 127, 141, 142, 161, 162, 163, 164, 165, 166,
     & 167, 168, 181, 182, 183, 184, 185, 201, 202, 221,
     & 222, 223, 224, 241, 261, 262, 263, 264, 265, 266,
     & 267, 268, 269, 270, 271, 281, 301, 304, 305, 321,
     & 341, 342, 361, 362, 363, 364, 365, 366, 367, 368,
     & 371, 381, 383, 401, 402, 403, 404, 405, 406, 407,
     & 409, 501, 502, 503, 504, 505, 506, 507, 508, 509,
     & 510, 511, 512, 513, 514, 515, 519, 520, 601, 602,
     & 605, 606, 607, 608, 701, 702, 703, 704, 705, 706/
      DATA ((FTSDMX(I,J),J= 1,1),I=101,141) /
     & 707, 708, 709, 722, 801, 802, 803, 805, 807, 809,
     & 901, 902, 904, 911, 912, 921, 922, 923, 924, 925,
     & 926, 931, 932, 941, 942, 943, 951, 952, 953, 954,
     & 955, 981, 982, 991, 992, 993, 995, 996, 997, 998,
     & 999/
      DATA ((FTSDMX(I,J),J= 2,2),I=  1,100) /
     & 520, 520, 520, 535, 460, 460, 460, 460, 460, 460,
     & 460, 460, 390, 435, 505, 505, 495, 365, 415, 475,
     & 465, 350, 300, 415, 415, 415, 415, 560, 690, 530,
     & 690, 460, 460, 580, 735, 765, 735, 735, 735, 735,
     & 735, 735, 735, 735, 770, 675, 735, 770, 735, 650,
     & 730, 730, 460, 645, 460, 460, 470, 470, 470, 470,
     & 580, 520, 520, 460, 300, 360, 475, 480, 475, 555,
     & 495, 380, 380, 415, 430, 400, 440, 500, 440, 470,
     & 360, 455, 405, 295, 300, 420, 475, 440, 395, 460,
     & 425, 300, 785, 625, 415, 420, 450, 495, 470, 415/
      DATA ((FTSDMX(I,J),J= 2,2),I=101,141) /
     & 445, 445, 445, 550, 460, 325, 455, 485, 415, 555,
     & 725, 550, 725, 550, 550, 460, 400, 550, 550, 470,
     & 550, 550, 550, 550, 550, 525, 525, 525, 525, 525,
     & 550, 550, 550, 525, 525, 525, 525, 470, 470, 470,
     & 380/
C
C-----------
C  CHECK FOR DEBUG
C-----------
      CALL DBCHK (DEBUG,'FORTYP',6,ICYC)
      IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_1: ICYC,IXF,IFORTP = ',
     &ICYC,IXF,IFORTP
C
C----------
      P1=0.
      P2=0.
      P3=0.
      IFT=0
      JFT=0
      SOUTH=0.
C----------
C  THE FIRST CALL TO FORTYP IS FROM SITSET TO SET INITIAL FOREST TYPE
C  IF IFORTP = 0, IMPLIES NOT SET BY USER, OR INVALID FOREST TYPE.
C              IFORTP IS SET TO IFORTP= 0 IN CALL FROM SITSET.
C              THEN CALCULATE FOREST TYPE EVERY CYCLE.
C  IF FIATYP = VALID NUMBER SET BY USER AND LFLAGV= .F.
C              THEN INPUT FOREST TYPE FOR FIRST CYCLE AND
C              RECALCULATE FOR REMAINING CYCLES. SET SDIMAX FOR SN
C  IF FIATYP = VALID NUMBER SET BY USER AND LFLAGV= .T.
C              THEN INPUT FOREST TYPE AS CONSTANT USED FOR ALL CYCLES.
C              SET SDIMAX FOR SN.
C
C-----------
C  ESTABLISH FLAG FOR SN VARIANT
C-----------
      SELECT CASE (VARACD)
        CASE ('SN')
          ISNVAR=1
        CASE DEFAULT
          ISNVAR=0
      END SELECT
C-----------
C  CHECK FOR VALID INPUT FOREST TYPE
C-----------
      FIATYP= FLOAT(IFORTP)
      IF (IXF.EQ.1) THEN                                                        ! IXF=1 THEN CALL WAS FROM 'INITRE'
        IF (FIATYP.LE.0.) GO TO 9999                                            ! IF FIATYP IS 0 THEN CALCULATE FT EVERY CYCLE
        LFORVP= .FALSE.
        DO 40 I= 1,141
          IF (FTSDMX(I,1).EQ.FIATYP) LFORVP=.TRUE.
   40   CONTINUE
        IF (.NOT.LFORVP) THEN                                                   ! IF INVALID FOREST TYPE THEN WRITE WARNING MESSAGE
          WRITE (JOSTND,45)
   45     FORMAT(/' ********   WARNING, INVALID FOREST TYPE WAS INP',
     &          'UT BY USER, FOREST TYPE WILL BE CALCULATED BY MODEL')
          IFORTP= 0
          FIATYP= 0.
          LFLAGV=.FALSE.
        END IF
      GO TO 9999
      END IF
      JFT=IFORTP
      SDIFIA=0.
C----------
C  INITIALIZE S(210) ARRAY WHICH REPRESENTS ITG GROUPS
C----------
      DO I=1,210
        S(I)=0.
      END DO
C
C----------
C   CALL STKVAL SUBROUTINE TO CALCULATE STOCKING FOR THE STAND
C----------
      IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_2: CALLING STKVAL ...'
      CALL STKVAL(S)
C
      IF((ICYC.GT.1).AND.LFLAGV)GO TO 9999
C
C     *********************************************************************************************;
C     * BEGIN -- ASSIGNMENT OF INITIAL TYPES TO COMBINED TYPE GROUPS; (ARNER 2/13/2002 - TABLE 5)
C     *********************************************************************************************;
C --- SOFTWOODS
      SFTWDS=0.                                                                 ! A. SOFTWOODS (1-58,60,62-79,161,162)
      DO I= 1,58
        SFTWDS= SFTWDS + S(I)
      END DO
      SFTWDS=SFTWDS+S(60)
      DO I= 62,79
        SFTWDS= SFTWDS + S(I)
      END DO
      SFTWDS=SFTWDS+S(161)+S(162)+S(170)
      TRFIRSPR=0.                                                               ! A.B. TRUE FIRS AND SPRUCE (1-5,7,9,14,15,28,34,35)
      DO I= 1,5
        TRFIRSPR=TRFIRSPR + S(I)
      END DO
      TRFIRSPR=TRFIRSPR+S(7)+S(9)+S(14)+S(15)+S(28)+S(34)+S(35)
      SPSAFIR=S(4)+S(14)+S(15)                                                  ! A.B.C. SPRUCE-SUBALPINE FIR (4,14,15)
      ENGSAFIR=S(4)+S(14)                                                       ! A.B.C.D. ENGELMANN SPRUCE-SUBALPINE FIR (4,14)
      SALPFIR=S(4)                                                              ! A.B.C.D.E. SUBALPINE FIR (4)
      ENGSPR=S(14)                                                              ! A.B.C.D.E. ENGLEMAN SPRUCE (14)
      BLUSPR=S(15)                                                              ! A.B.C.D. BLUE SPRUCE (15)
      WHMLCKS=S(34)+S(35)                                                       ! A.B.C. WESTERN HEMLOCKS (34,35)
      WHMLCK=S(34)                                                              ! A.B.C.D. WESTERN HEMLOCK (34)
      MTNHMLK=S(35)                                                             ! A.B.C.D. MOUNTAIN HEMLOCK (35)
      TRUFIR=0.                                                                 ! A.B.C. TRUE FIRS (1-5,7)
      DO I= 1,5
        TRUFIR=TRUFIR+S(I)
      END DO
      TRUFIR=TRUFIR+S(7)
      PSLVRFIR=S(1)                                                             ! A.B.C.D. PACIFIC SILVER FIR (1)
      WHTFIR=S(2)                                                               ! A.B.C.D. WHITE FIR (2)
      GRNDFIR=S(3)                                                              ! A.B.C.D. GRAND FIR (3)
      SALPFIR=S(4)                                                              ! A.B.C.D. SUBALPINE FIR (4)
      REDFIR=S(5)                                                               ! A.B.C.D. RED FIR (5)
      NOBLFIR=S(7)                                                              ! A.B.C.D. NOBLE FIR (7)
      AKYLCDR=S(9)                                                              ! A.B.C. ALASKA YELLOW CEDAR (9)
      WWHPIN=S(28)                                                              ! A.B.C. WESTERN WHITE PINE (28)
      DFLRWP=S(8)+S(10)+S(11)+S(13)+S(23)+S(24)+
     &       S(26)+S(27)+S(30)+S(31)+S(36)                                      ! A.B. DF-LARCH-W.WHITE PINES (8,10,11,13,23,24,26,27,30,31,36)
      DGFRLR=S(11)+S(13)+S(31)                                                  ! A.B.C. DF-WESTERN LARCH (11,13,31)
      DGFIR=S(31)                                                               ! A.B.C.D. DOUG FIR (31)
      WLRCH=S(13)                                                               ! A.B.C.D. WESTERN LARCH (13)
      WRCEDAR=S(11)                                                             ! A.B.C.D. WESTERN REDCEDAR (11)
      DGFRPIN=S(8)+S(10)+S(23)+S(24)+S(26)+S(27)+S(30)+S(31)+S(36)              ! A.B.C. DF-WESTERN PINES (8,10,23,24,26,27,30,31,36)
      DGFIR=S(31)                                                               ! A.B.C.D. DOUG FIR (31)
      PNDROSA=S(26)+S(36)                                                       ! A.B.C.D. PONDEROSA PINE (26,36)
      LODGPOLE=S(23)                                                            ! A.B.C.D. LODGEPOLE PINE (23)
      PORFCDR=S(8)                                                              ! A.B.C.D. PORT ORFORD CEDAR (8)
      SUGRPIN=S(27)                                                             ! A.B.C.D. SUGAR PINE (27)
      INSCDR=S(10)                                                              ! A.B.C.D. INCENSE CEDAR (10)
      JFBCDGFR=S(24)+S(30)+S(36)                                                ! A.B.C.D. JEFFREY-COULTER PINE-BIGCONE DF (24,30,36)
      LRCHPIN=S(13)+S(23)+S(26)+S(36)                                           ! A.B.C. WESTERN LARCH-PINE (13,23,26,36)
      WLRCH=S(13)                                                               ! A.B.C.D. WESTERN LARCH (13)
      PNDROSA=S(26)+S(36)                                                       ! A.B.C.D. PONDEROSA PINE (26,36)
      LODGPOLE=S(23)                                                            ! A.B.C.D. LODGEPOLE PINE (23)
      STKSPHM=S(11)+S(18)+S(34)                                                 ! A.B. SITKA SPRUCE-HEMLOCK (11,18,34)
      WHMLCK=S(34)                                                              ! A.B.C. WESTERN HEMLOCK (34)
      STKASPR=S(18)                                                             ! A.B.C. SITKA SPRUCE (18)
      WRCEDAR=S(11)                                                             ! A.B.C. WESTERN REDCEDAR (11)
      OTHWPIN=S(6)+S(12)+S(19)+S(20)+S(21)+S(22)+S(25)+S(29)+S(40)              ! A.B. OTHER WESTERN PINES (6,12,19,20,21,22,25,29,40)
      KNBPIN=S(21)                                                              ! A.B.C. KNOBCONE PINE (21)
      SWHTPIN=S(22)                                                             ! A.B.C. SOUTHWEST WHITE PINE (22)
      BSHPPIN=S(6)                                                              ! A.B.C. BISHOP PINE (6)
      MNTRYPIN=S(29)                                                            ! A.B.C. MONTEREY PINE (29)
      FXTLPIN=S(20)                                                             ! A.B.C. FOXTAIL-BRISTLECONE PINE (20)
      LMBRPIN=S(25)                                                             ! A.B.C. LIMBER PINE (25)
      WHBRKPIN=S(19)                                                            ! A.B.C. WHITEBARK PINE (19)
      MISCWSFW=S(12)+S(40)                                                      ! A.B.C. MISC. W. SOFTWOODS (12,40)
      REDWDS=S(31)+S(32)+S(33)                                                  ! A.B. REDWOODS (31,32,33)
      REDWOOD=S(32)                                                             ! A.B.C. REDWOOD (32)
      GNTSEQ=S(33)                                                              ! A.B.C. GIANT SEQUOIA (33)
      DGFIR=S(31)                                                               ! A.B.C. DOUG FIR (31)
      EPINE=0.                                                                  ! A.B. EASTERN PINES (41-54,66)
      DO I= 41,54
        EPINE=EPINE+S(I)
      END DO
      EPINE=EPINE+S(66)
      RWJPIN=S(41)+S(42)+S(53)+S(66)                                            ! A.B.C. RED-WHITE-JACK PINE (41,42,53,66)
      WHPHEM=S(53)+S(66)                                                        ! A.B.C.D. WHITE PINE-HEMLOCK (53,66)
      EWHPIN=S(53)                                                              ! A.B.C.D.E. EASTERN WHITE PINE (53)
      EHMLCK=S(66)                                                              ! A.B.C.D.E. EASTERN HEMLOCK (66)
      REDPIN=S(42)                                                              ! A.B.C.D. RED PINE (42)
      JACKPIN=S(41)                                                             ! A.B.C.D. JACK PINE (41)
      LNGLFSLH=S(46)+S(48)                                                      ! A.B.C. LONGLEAF - SLASH PINE (46,48)
      LNGLFP=S(48)                                                              ! A.B.C.D. LONGLEAF PINE (48)
      SLASHP=S(46)                                                              ! A.B.C.D. SLASH PINE (46)
      LOBSHRTP=S(44)+S(45)+S(47)+S(49)+S(50)+S(51)+S(52)+S(54)                  ! A.B.C. LOBLOLLY-SHORTLEAF PINE (44,45,47,49-52,54)
      LOBP=S(52)                                                                ! A.B.C.D. LOBLOLLY PINE (52)
      SHRTLFP=S(45)                                                             ! A.B.C.D. SHORTLEAF PINE (45)
      VIRGP=S(54)                                                               ! A.B.C.D. VIRGINIA PINE (54)
      SANDP=S(44)                                                               ! A.B.C.D. SAND PINE (44)
      TBLMTNP=S(49)                                                             ! A.B.C.D. TABLE MOUNTAIN PINE (49)
      PONDP=S(51)                                                               ! A.B.C.D. POND PINE (51)
      PITCHP=S(50)                                                              ! A.B.C.D. PITCH PINE (50)
      SPRUCEP=S(47)                                                             ! A.B.C.D. SPRUCE PINE (47)
      PNYNJN=S(38)+S(63)+S(64)+S(161)+S(162)                                    ! A.B. PINYON-JUNIPER (38,63,64,161,162)
      ERCEDAR=S(64)                                                             ! A.B.C. EASTERN REDCEDAR (64)
      RMJUNIPR=S(63)                                                            ! A.B.C. ROCKY MOUNTAIN JUNIPER (63)
      WJUNIPR=S(38)                                                             ! A.B.C. WESTERN JUNIPER (38)
      JUNIPRW=S(161)                                                            ! A.B.C. JUNIPER WOODLAND (161)
      PINJUNW=S(161)+S(162)                                                     ! A.B.C. PINYON-JUNIPER WOODLAND (161,162)
      ESPRFIR=S(16)+S(17)+S(55)+S(58)+S(60)+S(65)                               ! A.B. EASTERN SPRUCE-FIR (16,17,55,58,60,65)
      USPRFIR=S(16)+S(55)+S(58)                                                 ! A.B.C. UPLAND SPRUCE-FIR (16,55,58)
      BFIRSPR=S(55)+S(58)                                                       ! A.B.C.D. BALSAM FIR - RED SPRUCE (55,58)
      BLSMFIR=S(55)                                                             ! A.B.C.D.E. BALSAM FIR (55)
      REDSPR=S(58)                                                              ! A.B.C.D.E. RED SPRUCE (58)
      WHSPR=S(16)                                                               ! A.B.C.D. WHITE SPRUCE (16)
      LSPRFIR=S(17)+S(60)+S(65)                                                 ! A.B.C. LOWLAND SPRUCE-FIR (17,60,65)
      BLKSPR=S(17)                                                              ! A.B.C.D. BLACK SPRUCE (17)
      TAMRCK=S(65)                                                              ! A.B.C.D. TAMARACK (65)
      NWHCDR=S(60)                                                              ! A.B.C.D. NORTHERN WHITE CEDAR (60)
      EXSFTWD=S(70)+S(71)+S(72)                                                 ! A.B. EXOTIC SOFTWOODS (70,71,72)
      SCOTCHP=S(71)                                                             ! A.B.C. SCOTCH PINE (71)
      JAPBLKP=S(72)                                                             ! A.B.C. JAPANESE BLACK PINE (72)
      OTHEXSWD=S(70)                                                            ! A.B.C. OTHER EXOTIC SOFTWOODS (70)
C --- HARDWOODS
      HRDWDS=0.                                                                 ! A. HARDWOODS (59,61,81-153,156-160,163,201-210)
      DO I= 81,153
        HRDWDS=HRDWDS+S(I)
      END DO
      DO I= 156,160
        HRDWDS=HRDWDS+S(I)
      END DO
      DO I= 201,210
        HRDWDS=HRDWDS+S(I)
      END DO
      HRDWDS=HRDWDS+S(59)+S(61)+S(163)+S(180)+S(190)
      OAKPIN=0.                                                                 ! A.B. OAK-PINE (41,42,44-54,64)
      DO I= 41,54
        OAKPIN=OAKPIN+S(I)
      END DO
      OAKPIN=OAKPIN+S(64)
      ERCEDAR=S(64)                                                             ! A.B.C. EASTERN REDCEDAR (64)
      SHRTLFP=S(45)                                                             ! A.B.C. SHORTLEAF PINE (45)
      EWHPIN=S(53)                                                              ! A.B.C. EASTERN WHITE PINE (53)
      LNGLFP=S(48)                                                              ! A.B.C. LONGLEAF PINE (48)
      VIRGP=S(54)                                                               ! A.B.C. VIRGINIA PINE (54)
      LOBP=S(52)                                                                ! A.B.C. LOBLOLLY PINE (52)
      SLASHP=S(46)                                                              ! A.B.C. SLASH PINE (46)
      JACKPIN=S(41)                                                             ! A.B.C. JACK PINE (41)
      REDPIN=S(42)                                                              ! A.B.C. RED PINE (42)
      SANDP=S(44)                                                               ! A.B.C. SAND PINE (44)
      SPRUCEP=S(47)                                                             ! A.B.C. SPRUCE PINE (47)
      TBLMTNP=S(49)                                                             ! A.B.C. TABLE MOUNTAIN PINE (49)
      PITCHP=S(50)                                                              ! A.B.C. PITCH PINE (50)
      PONDP=S(51)                                                               ! A.B.C. POND PINE (51)
      OAKHCK=0.                                                                 ! A.B. OAK-HICKORY (81-86,88,89,92,93,101,108,110,120,122,202,206,207)
      DO I= 81,86
        OAKHCK=OAKHCK+S(I)
      END DO
      OAKHCK=OAKHCK+S(88)+S(89)+S(92)+S(93)+S(101)+S(108)+S(110)
     &             +S(120)+S(122)+S(202)+S(206)+S(207)
      WHOAK=S(81)                                                               ! A.B.C. WHITE OAK (81)
      BUROAK=S(83)                                                              ! A.B.C. BUR OAK (83)
      CHSTOAK=S(84)                                                             ! A.B.C. CHESTNUT OAK (84)
      NROAK=S(85)                                                               ! A.B.C. NORTHERN RED OAK (85)
      SCRLTOAK=S(82)                                                            ! A.B.C. SCARLET OAK (82)
      YP=S(110)                                                                 ! A.B.C. YELLOW POPLAR (110)
      BWALNUT=S(108)                                                            ! A.B.C. BLACK WALNUT (108)
      BLCST=S(122)                                                              ! A.B.C. BLACK LOCUST (122)
      REDMAPL=S(95)                                                             ! A.B.C. RED MAPLE (95)
      PSTBLJKO=S(86)+S(206)                                                     ! A.B.C. POST-BLACKJACK OAK (86,206)
      CHBLSC=S(82)+S(84)+S(120)                                                 ! A.B.C. CHESTNUT-BLACK/SCARLET OAK (82,84,120)
      WOROHK=S(81)+S(85)+S(92)+S(94)+S(120)+S(207)                              ! A.B.C. WHITE OAK-RED OAK-HICKORY (81,85,92,94,120,207)
      YPWORO=S(81)+S(85)+S(110)                                                 ! A.B.C. YELLOW POPLAR-WH.OAK-RED OAK (81,85,110)
      SCRUBOAK=S(89)+S(203)+S(206)                                              ! A.B.C. SOUTHERN SCRUB OAK (89,203,206)
      SWGYP=S(109)+S(110)                                                       ! A.B.C. SWTGUM-YELLOW POPLAR (109,110)
      SASFPRSM=S(93)                                                            ! A.B.C. SASSAFRAS-PERSIMMON (93)
      MXDHWD=0.                                                                 ! A.B.C. MIXED UPL. HRDWDS (83,88,94,101,106,108,113,122,125,201-204)
      MXDHWD=MXDHWD+S(83)+S(88)+S(94)+S(101)+S(106)+S(108)
     &             +S(113)+S(122)+S(125)
      DO I=201,204
        MXDHWD=MXDHWD+S(I)
      END DO
      OKGMCYP=0.                                                                ! A.B. OAK-GUM-CYPRESS (59,61,87,90,111,112,114,127,128,143)
      OKGMCYP=S(59)+S(61)+S(87)+S(90)+S(111)+S(112)+S(114)
     &       +S(127)+S(128)+S(143)
      ATLWCDR=S(59)                                                             ! A.B.C. ATLANTIC WHITE CEDAR (59)
      SBSTRM=S(95)+S(111)+S(113)+S(114)+S(127)                                  ! A.B.C. SWEETBAY-SWAMP TUPELO-RED MAPLE (95,111,113,114,127)
      SCHCHBO=S(87)                                                             ! A.B.C. SWAMP CHESTNUT-CHERRYBARK OAK (87)
      SWGWILO=S(109)+S(125)+S(143)+S(201)+S(203)+S(204)                         ! A.B.C. SWTGUM-NUTTALL-WILLOW OAK (109,125,143,201,203,204)
      CYPWTUP=S(61)+S(112)                                                      ! A.B.C. CYPRESS-WATER TUPELO (61,112)
      OVRCUPWH=S(90)+S(128)                                                     ! A.B.C. OVERCUP OAK-WATER HICKORY (90,128)
      ELMASHCW=0.                                                               ! A.B. ELM-ASH-COTTONWOOD (91,97,100,104,115,116,118,123,129,135,137,208)
      ELMASHCW=S(91)+S(97)+S(100)+S(104)+S(115)+S(116)+S(118)+S(123)
     &        +S(129)+S(135)+S(137)+S(208)
      CTNWD=S(118)+S(137)                                                       ! A.B.C. COTTONWOOD (118,137)
      WILLOW=S(123)                                                             ! A.B.C. WILLOW (123)
      REDMAPL=S(95)                                                             ! A.B.C. RED MAPLE (95)
      RBRCHSYC=S(108)+S(116)+S(123)+S(129)                                      ! A.B.C. RIVER BIRCH-SYCAMORE (108,116,123,129)
      SYCPCELM= S(91)+S(94)+S(109)+S(116)                                       ! A.B.C. SYCAMORE-PECAN-ELM (91,94,109,116)
      BAELMMAP=S(104)                                                           ! A.B.C. BLACK ASH-ELM-RED MAPLE (104)
      SGBELMGA=S(94)+S(100)+S(105)+S(115)+S(208)+S(209)                         ! A.B.C. SUGARBERRY-HACKBERRY-ELM-GREEN ASH (94,100,105,115,208,209)
      SLVMAELM=S(94)+S(97)                                                      ! A.B.C. SILVER MAPLE-AMERICAN ELM (94,97)
      CTNWWILO=S(118)+S(123)+S(130)+S(131)+S(137)                               ! A.B.C. COTTONWOOD-WILLOW (118,123,130,131,137)
      ORGNASH=S(135)                                                            ! A.B.C. OREGON ASH (135)
      MBB=S(66)+S(96)+S(98)+S(107)+S(110)+S(122)+S(124)                         ! A.B. MAPLE-BEECH-BIRCH (66,96,98,107,110,122,124)
      BLCH=S(121)                                                               ! A.B.C. BLACK CHERRY (121)
      REDMAPL=S(95)                                                             ! A.B.C. RED MAPLE (95)
      BCWAYP=S(103)+S(110)+S(121)                                               ! A.B.C. BLACK CHERRY-WHITE ASH-YELLOW POPLAR (103,110,121)
      MPLBASWD=S(96)+S(124)                                                     ! A.B.C. MAPLE-BASSWOOD (96,124)
      ELMASHLO=S(94)+S(105)+S(122)                                              ! A.B.C. ELM-ASH-LOCUST (94,105,122)
      MPLBCHYB=S(66)+S(94)+S(95)+S(96)+S(98)+S(102)+S(105)+S(107)+S(108)        ! A.B.C. MAPLE-BEECH-BIRCH (66,94,95,96,98,102,105,107,108)
      ASPBRCH=S(99)+S(117)+S(119)                                               ! A.B. ASPEN-BIRCH (99,117,119)
      ASPEN=S(119)                                                              ! A.B.C. ASPEN (119)
      BLSMPOP=S(117)                                                            ! A.B.C. BALSAM POPLAR (117)
      PAPRBRCH=S(99)                                                            ! A.B.C. PAPER BIRCH (99)
      ALDRMAPL=S(130)+S(131)                                                    ! A.B. RED ALDER-MAPLE (130,131)
      REDALDR=S(131)                                                            ! A.B.C. RED ALDER (131)
      BGLFMPL=S(130)                                                            ! A.B.C. BIGLEAF MAPLE (130)
      WSTOAK=S(134)+S(138)+S(139)+S(140)+S(142)+S(158)+S(163)+S(210)            ! A.B. WESTERN OAKS (134,138,139,140,142,158,163,210)
      CLFBO=S(139)                                                              ! A.B.C. CALIFORNIA BLACK OAK (139)
      ORGWHO=S(140)                                                             ! A.B.C. OREGON WHITE OAK (140)
      BLUEOAK=S(134)                                                            ! A.B.C. BLUE OAK (134)
      GRAYPINE=S(163)                                                           ! A.B.C. GRAY PINE (163)
      COSTLO=S(138)                                                             ! A.B.C. COAST LIVE OAK (138)
      CANYLO=S(142)                                                             ! A.B.C. CANYON_INTERIOR LIVE OAK (142)
      DOAKWDLD=S(158)                                                           ! A.B.C. DECIDUOUS OAK-WOODLAND (158)
      EOAKWDLD=S(210)                                                           ! A.B.C. EVERGREEN OAK-WOODLAND (210)
      TANOKLRL=S(133)+S(136)+S(141)                                             ! A.B. TAN OAK-LAUREL (133,136,141)
      TANOAK=S(136)                                                             ! A.B.C. TAN OAK (136)
      CLFLREL=S(141)                                                            ! A.B.C. CALIFORNIA LAUREL (141)
      GNTCHNK=S(133)                                                            ! A.B.C. GIANT CHINKAPIN (133)
      OTHWHWD=S(132)+S(156)+S(157)+S(159)+S(160)                                ! A.B. OTHER WESTERN HARDWOODS (132,156,157,159,160)
      PACMDRN=S(132)                                                            ! A.B.C. PACIFIC MADRONE (132)
      MESQUITW=S(157)                                                           ! A.B.C. MESQUITE WOODLAND (157)
      MTNBRSHW=S(156)                                                           ! A.B.C. MOUNTAIN BRUSH WOODLAND (156)
      INTMAPLW=S(159)                                                           ! A.B.C. INTERMOUNTAIN MAPLE WOODLAND (159)
      MISCWHWD=S(160)                                                           ! A.B.C. MISC. WESTERN HARDWOODS (160)
      TROPHWDS=S(147)+S(149)                                                    ! A.B. TROPICAL HARDWOODS (147,149)
      PALM=S(147)                                                               ! A.B.C. SABAL PALM (147)
      MANGRV=S(149)                                                             ! A.B.C. MANGROVE (149)
      EXHWDS=S(144)+S(145)+S(146)+S(148)                                        ! A.B. EXOTIC HARDWOODS (144,145,146,148)
      PALONIA=S(144)                                                            ! A.B.C. ROYAL PAULOWNIA (144)
      MELUCA=S(145)                                                             ! A.B.C. MELALUCA (145)
      EUCLPT=S(148)                                                             ! A.B.C. EUCALYPTUS (148)
      OTHEXHWD=S(146)                                                           ! A.B.C. OTHER EXOTIC HARDWOODS (146)
C --- SPECIAL GROUPS
      UPLOOAK=S(125)+S(201)+S(203)+S(204)                                       ! A. UPLAND-LOWLAND OAKS (125,201,203,204)
      UPLOHWD=S(95)+S(103)+S(105)                                               ! A. UPLAND-LOWLAND HARDWOODS (95,103,105)
      SROAK=S(88)                                                               ! A. SOUTHERN RED OAK (88)
      AELM=S(94)                                                                ! A. AMERICAN/SLIPPERY/ROCK ELM (94)
      WCELM=S(209)                                                              ! A. WINGED/CEDAR ELM (209)
      SLVRMPL=S(97)                                                             ! A. SILVER MAPLE (97)
      WHASH=S(103)                                                              ! A. WHITE ASH (103)
      ECTNWD=S(118)                                                             ! A. EASTERN COTTONWOOD (118)
      BLCH=S(121)                                                               ! A. BLACK CHERRY (121)
      BLKGUM=S(113)                                                             ! A. BLACK GUM (113)
      BEECH=S(102)                                                              ! A. AMERICAN BEECH (102)
      HOLLY=S(106)                                                              ! A. AMERICAN HOLLY (106)
      SWEETGUM=S(109)                                                           ! A. SWEETGUM (109)
      PINOAK=S(205)                                                             ! A. PIN OAK (205)
C --- SPECIAL FVS GROUPS
      OTHALL=S(170)+S(180)+S(190)                                               ! A. ALL OTHER SPECIES
      OTHSFT=S(170)                                                             ! A. OTHER SOFTWOODS
      OTHHRD=S(180)                                                             ! A. OTHER HARDWOODS
      OTHSPC=S(190)                                                             ! A. OTHER SPECIES
C --- TOTAL ALL ITG GROUPS
      TOTSTK=0.                                                                 ! TOTAL FOREST STOCKING (1-210)
      DO I=1,210
        TOTSTK=TOTSTK+S(I)
      END DO
C     *********************************************************************************************;
C     * END -- ASSIGNMENT OF INITIAL TYPES TO COMBINED TYPE GROUPS;
C     *********************************************************************************************;
C
C----------
C  DEFINE SPECIAL CATETORIES FOR LATER USE IN TYPING
C----------
C    FIA DETERMINES 'UPLAND' OR 'LOWLAND' SITE FROM PHYSIOGRAPHIC
C    CODES.  IN FVS THAT INFO. IS NOT AVAILABLE.  FOR FVS USE,
C    DETERMINE UPLAND/LOWLAND BY SUMMING THE ELIGIBLE STOCKING
C    OF SPECIES THAT NORMALLY OCCUR IN THOSE SETTINGS.  THE TOTALS
C    COMPUTED HERE ARE USED FOR E. HARDWOOD FOR. TYPE DETERMINATION.
C----------
      UPMPBCBR=0.                                                               ! UPLAND - MAPLE-BEECH-BIRCH
      UPMPBCBR=S(66)+S(85)+S(94)+S(95)+S(96)+S(98)+S(102)+S(103)+
     &         S(105)+S(107)+S(108)+S(110)+S(121)+S(122)+S(124)
      UPOAKHIC=0.                                                               ! UPLAND - OAK-HICKORY
      UPOAKHIC=S(81)+S(82)+S(83)+S(84)+S(85)+S(86)+S(88)+S(89)+S(92)+
     &         S(93)+S(94)+S(95)+S(101)+S(102)+S(103)+S(105)+S(106)+
     &         S(108)+S(109)+S(110)+S(113)+S(120)+S(121)+S(122)+S(125)+
     &         S(201)+S(202)+S(203)+S(204)+S(206)+S(207)+S(209)
      LWELASCW=0.                                                               ! LOWLAND - ELM-ASH-COTTONWOOD
      LWELASCW=S(91)+S(94)+S(95)+S(97)+S(100)+S(103)+S(104)+S(105)+
     &         S(108)+S(109)+S(115)+S(116)+S(118)+S(123)+S(129)+
     &         S(130)+S(131)+S(135)+S(137)+S(208)+S(209)
      LWOKGMCY=0.                                                               ! LOWLAND - OAK-GUM-CYPRESS
      LWOKGMCY=S(59)+S(61)+S(87)+S(90)+S(94)+S(95)+S(102)+S(103)+
     &         S(105)+S(106)+S(109)+S(111)+S(112)+S(113)+S(114)+
     &         S(125)+S(127)+S(128)+S(143)+S(201)+S(203)+S(204)+S(205)
C
C----------
C  DEFINE PROPORTION-OF-TOTAL-STOCKING VARIABLES
C----------
      PCOND    = 1.00
      HALF     = 0.50 * TOTSTK
      QUARTR   = 0.25 * TOTSTK
      FIVE     = 0.05 * TOTSTK
      TEN      = 0.10 * TOTSTK
      FIFTN    = 0.15 * TOTSTK
      TWENTY   = 0.20 * TOTSTK
      EIGHTY   = 0.80 * TOTSTK
      NINETY   = INT(0.90 * TOTSTK)
      NINTY5   = INT(0.95 * TOTSTK)
C
C----------
C  DETERMINE FOREST SERVICE REGION NUMBER
C----------
      IRGN=0
      IF (KODFOR/10000000.GE.1) THEN
        IRGN=KODFOR/10000000
      ELSE IF (KODFOR/10000.GE.1) THEN
        IRGN=KODFOR/10000
      ELSE IF (KODFOR/100.GE.1) THEN
        IRGN=KODFOR/100
      END IF
C
C----------
C  BEGIN FOREST TYPE ALGORITHM
C----------
      IFT=0
      IF (TOTSTK/PCOND.LT.10.) THEN
        IFT=999                                                                 ! 999 = NONSTOCKED
        ISZCL=5
        ISTCL=5
        GO TO 999
      END IF
C
C----------------------------------------------------------------------
C  START SECTION TO DETERMINE FOREST TYPE BASED ON COMPUTED
C  AND TOTAL STOCKING VALUES FROM ABOVE.
C----------
      IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_3: SFTWDS,HRDWDS,',
     &'ISZCL,ISTCL = ', SFTWDS,HRDWDS,ISZCL,ISTCL
C----------
      IF (SFTWDS.GE.HRDWDS) THEN                                                ! A. SOFTWOODS
        IFT=996                                                                 ! FVS OTHER SOFTWOODS(996)
        IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_4: ICYC,TRFIRSPR,',
     &  'STKSPHM,REDWDS,DFLRWP,OTHWPIN,EPINE,ESPRFIR,PNYNJN,EXSFTWD,',
     &  'OTHSFT = '
        IF(DEBUG)WRITE(JOSTND,*)ICYC,TRFIRSPR,STKSPHM,REDWDS,DFLRWP,
     &  OTHWPIN,EPINE,ESPRFIR,PNYNJN,EXSFTWD,OTHSFT
        P1=MAX(TRFIRSPR,STKSPHM,REDWDS,DFLRWP,OTHWPIN,
     &         EPINE,ESPRFIR,PNYNJN,EXSFTWD,OTHSFT)-.001
        IF (P1.GT.0.) THEN
          IF (P1.LT.TRFIRSPR) THEN                                              ! A.B. TRUE FIRS AND SPRUCE
            P2=MAX(SPSAFIR,WHMLCKS,TRUFIR,AKYLCDR,WWHPIN)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.SPSAFIR) THEN
                IF (ENGSAFIR.GE.BLUSPR) THEN
                  IF ((SALPFIR.GE.FIVE.AND.SALPFIR.LT.HALF).AND.
     &                (ENGSPR.GE.FIVE.AND.ENGSPR.LT.HALF)) THEN
                    IFT=266                                                     ! ENGELMANN SPRUCE-SUBALPINE(266)
                  ELSE IF (ENGSPR.GE.SALPFIR) THEN
                    IFT=265                                                     ! ENGELMANN SPRUCE(265)
                  ELSE IF (SALPFIR.GT.0.) THEN
                    IFT=268                                                     ! SUBALPINE FIR(268)
                  END IF
                ELSE IF (BLUSPR.GT.0.) THEN
                  IFT=269                                                       ! BLUE SPRUCE(269)
                END IF
              ELSE IF (P2.LT.WHMLCKS) THEN
                IF (WHMLCK.GE.MTNHMLK) THEN
                  IFT=301                                                       ! WESTERN HEMLOCK(301)
                ELSE IF (MTNHMLK.GT.0.) THEN
                  IFT=270                                                       ! MOUNTAIN HEMLOCK(270)
                END IF
              ELSE IF (P2.LT.TRUFIR) THEN
                P3=MAX(PSLVRFIR,WHTFIR,GRNDFIR,SALPFIR,
     &                 REDFIR,NOBLFIR)-.001
                IF (P3.GT.0.) THEN
                  IF (P3.LT.PSLVRFIR) THEN
                    IFT=264                                                     ! PACIFIC SILVER FIR(264)
                  ELSE IF (P3.LT.WHTFIR) THEN
                    IFT=261                                                     ! WHITE FIR(261)
                  ELSE IF (P3.LT.GRNDFIR) THEN
                    IFT=267                                                     ! GRAND FIR(267)
                  ELSE IF (P3.LT.SALPFIR) THEN
                    IFT=268                                                     ! SUBALPINE FIR(268)
                  ELSE IF (P3.LT.REDFIR) THEN
                    IFT=262                                                     ! RED FIR(262)
                  ELSE IF (NOBLFIR.GT.0.) THEN
                    IFT=263                                                     ! NOBLE FIR(263)
                  END IF
                END IF
              ELSE IF (P2.LT.AKYLCDR) THEN
                IFT=271                                                         ! ALASKA YELLOW CEDAR(271)
              ELSE IF (WWHPIN.GT.0.) THEN
                IFT=241                                                         ! WESTERN WHITE PINE(241)
              END IF
            END IF
          ELSE IF (P1.LT.STKSPHM) THEN                                          ! A.B. SITKA SPRUCE-HEMLOCK
            P2=MAX(WHMLCK,STKASPR,WRCEDAR)-.001
            IF (P2.GT. 0.) THEN
              IF (P2.LT.WHMLCK) THEN
                IFT=301                                                         ! WESTERN HEMLOCK(301)
              ELSE IF (P2.LT.STKASPR) THEN
                IFT=305                                                         ! SITKA SPRUCE(305)
              ELSE IF (WRCEDAR.GT.0.) THEN
                IFT=304                                                         ! WESTERN REDCEDAR(304)
              END IF
            END IF
          ELSE IF (P1.LT.REDWDS) THEN                                           ! A.B. REDWOODS
            P2=MAX(REDWOOD,GNTSEQ,DGFIR)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.REDWOOD) THEN
                IFT=341                                                         ! REDWOOD(341)
              ELSE IF (P2.LT.GNTSEQ) THEN
                IFT=342                                                         ! GIANT SEQUOIA(342)
              ELSE IF (DGFIR.GT.0.) THEN
                IFT=201                                                         ! DOUGLAS-FIR(201)
              END IF
            END IF
          ELSE IF (P1.LT.DFLRWP) THEN                                           ! A.B. DF-LARCH-W.WHITE PINES
            P2=MAX(DGFRLR,DGFRPIN,LRCHPIN)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.DGFRLR) THEN
                P3=MAX(DGFIR,WLRCH,WRCEDAR)-.001
                IF (P3.GT.0.) THEN
                  IF (P3.LT.DGFIR) THEN
                    IFT=201                                                     ! DOUGLAS-FIR(201)
                  ELSE IF (P3.LT.WLRCH) THEN
                    IFT=321                                                     ! WESTERN LARCH(321)
                  ELSE IF (WRCEDAR.GT.0.) THEN
                    IFT=304                                                     ! WESTERN REDCEDAR(304)
                  END IF
                END IF
              ELSE IF (P2.LT.DGFRPIN) THEN
                P3=MAX(DGFIR,PNDROSA,LODGPOLE,PORFCDR,
     &                 SUGRPIN,INSCDR,JFBCDGFR)-.001
                IF (P3.GT.0.) THEN
                  IF (P3.LT.DGFIR) THEN
                    IFT=201                                                     ! DOUGLAS-FIR(201)
                  ELSE IF (P3.LT.PNDROSA) THEN
                    IFT=221                                                     ! PONDEROSA PINE(221)
                  ELSE IF (P3.LT.LODGPOLE) THEN
                    IFT=281                                                     ! LODGEPOLE PINE (281)
                  ELSE IF (P3.LT.PORFCDR) THEN
                    IFT=202                                                     ! PORT ORFORD CEDAR(202)
                  ELSE IF (P3.LT.SUGRPIN) THEN
                    IFT=224                                                     ! SUGAR PINE(224)
                  ELSE IF (P3.LT.INSCDR) THEN
                    IFT=222                                                     ! INCENSE CEDAR(222)
                  ELSE IF (JFBCDGFR.GT.0.) THEN
                    IFT=223                                                     ! JEFREY-COULTER PINE-BIGCONE DF(223)
                  END IF
                END IF
              ELSE IF (P2.LT.LRCHPIN) THEN
                P3=MAX(WLRCH,PNDROSA,LODGPOLE)-.001
                IF (P3.GT. 0.) THEN
                  IF (P3.LT.WLRCH) THEN
                    IFT=321                                                     ! WESTERN LARCH(321)
                  ELSE IF (P3.LT.PNDROSA) THEN
                    IFT=221                                                     ! PONDEROSA PINE(221)
                  ELSE IF (LODGPOLE.GT.0.) THEN
                    IFT=281                                                     ! LODGEPOLE PINE(281)
                  END IF
                END IF
              END IF
            END IF
          ELSE IF (P1.LT.OTHWPIN) THEN                                          ! A.B. OTHER WESTERN PINES
            P2=MAX(KNBPIN,SWHTPIN,BSHPPIN,MNTRYPIN,
     &             FXTLPIN,LMBRPIN,WHBRKPIN,MISCWSFW)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.KNBPIN) THEN
                IFT=361                                                         ! KNOBCONE PINE(361)
              ELSE IF (P2.LT.SWHTPIN) THEN
                IFT=362                                                         ! SW WHITE PINE(362)
              ELSE IF (P2.LT.BSHPPIN) THEN
                IFT=363                                                         ! BISHOP PINE(363)
              ELSE IF (P2.LT.MNTRYPIN) THEN
                IFT=364                                                         ! MONTEREY PINE(364)
              ELSE IF (P2.LT.FXTLPIN) THEN
                IFT=365                                                         ! FOXTAIL-BRISTLECONE PINE(365)
              ELSE IF (P2.LT.LMBRPIN) THEN
                IFT=366                                                         ! LIMBER PINE(366)
              ELSE IF (P2.LT.WHBRKPIN) THEN
                IFT=367                                                         ! WHITEBARK PINE(367)
              ELSE IF (MISCWSFW.GT.0.) THEN
                IFT=368                                                         ! MISC. WESTERN SOFTWOODS(368)
              END IF
            END IF
          ELSE IF (P1.LT.EPINE) THEN                                            ! A.B. EASTERN PINES
            P2=MAX(RWJPIN,LNGLFSLH,LOBSHRTP)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.RWJPIN) THEN
                IF ((WHPHEM.GE.HALF).AND.
     &              (EWHPIN.GE.FIVE.AND.EWHPIN.LT.HALF).AND.
     &              (EHMLCK.GE.FIVE.AND.EHMLCK.LT.HALF)) THEN
                   IFT=104                                                      ! E.WHITE PINE-HEMLOCK(104)
                ELSE
                  P3=MAX(EWHPIN,REDPIN,JACKPIN,EHMLCK)-0.001
                END IF
                IF (P3.GT.0.) THEN
                  IF (P3.LT.EWHPIN) THEN
                    IFT=103                                                     ! E.WHITE PINE(103)
                  ELSE IF (P3.LT.REDPIN) THEN
                    IFT=102                                                     ! RED PINE(102)
                  ELSE IF (P3.LT.JACKPIN) THEN
                    IFT=101                                                     ! JACK PINE(101)
                  ELSE IF (EHMLCK.GT.0.) THEN
                    IFT=105                                                     ! EASTERN HEMLOCK(105)
                  END IF
                END IF
              ELSE IF (P2.LT.LNGLFSLH) THEN
                IF (LNGLFP.GE.SLASHP) THEN
                  IFT=141                                                       ! LONGLEAF PINE(141)
                ELSE IF (SLASHP.GT.0.) THEN
                  IFT=142                                                       ! SLASH PINE(142)
                END IF
              ELSE IF (LOBSHRTP.GT.0.) THEN
                P3=MAX(LOBP,SHRTLFP,VIRGP,SANDP,
     &                 TBLMTNP,PONDP,PITCHP,SPRUCEP)-.001
                IF (P3.GT.0.) THEN
                  IF (P3.LT.LOBP) THEN
                    IFT=161                                                     ! LOBLOLLY PINE(161)
                  ELSE IF (P3.LT.SHRTLFP) THEN
                    IFT=162                                                     ! SHORTLEAF PINE(162)
                  ELSE IF (P3.LT.VIRGP) THEN
                    IFT=163                                                     ! VIRGINIA PINE(163)
                  ELSE IF (P3.LT.SANDP) THEN
                    IFT=164                                                     ! SAND PINE(164)
                  ELSE IF (P3.LT.TBLMTNP) THEN
                    IFT=165                                                     ! TABLE MOUNTAIN PINE(165)
                  ELSE IF (P3.LT.PONDP) THEN
                    IFT=166                                                     ! POND PINE(166)
                  ELSE IF (P3.LT.PITCHP) THEN
                    IFT=167                                                     ! PITCH PINE(167)
                  ELSE IF (SPRUCEP.GT.0.)THEN
                    IFT=168                                                     ! SPRUCE PINE(168)
                  END IF
                END IF
              END IF
            END IF
          ELSE IF (P1.LT.ESPRFIR) THEN                                          ! A.B. EASTERN SPRUCE-FIR
            IF (USPRFIR.GE.LSPRFIR) THEN                                        ! A.B.C. UPLAND E. SPRUCE-FIR
              IF ((BFIRSPR.GE.HALF).AND.
     &            (BLSMFIR.GE.FIVE.AND.BLSMFIR.LT.HALF).AND.
     &            (REDSPR.GE.FIVE.AND.REDSPR.LT.HALF)) THEN
                IFT=124                                                         ! RED SPRUCE-BALSAM FIR(124)
              ELSE
                P2=MAX(BLSMFIR,WHSPR,REDSPR)-.001
                IF (P2.GT.0.) THEN
                  IF (P2.LT.BLSMFIR) THEN
                    IFT=121                                                     ! BALSAM FIR(121)
                  ELSE IF (P2.LT.WHSPR) THEN
                    IFT=122                                                     ! WHITE SPRUCE(122)
                  ELSE IF (REDSPR.GT.0.) THEN
                    IFT=123                                                     ! RED SPRUCE(123)
                  END IF
                END IF
              END IF
            ELSE                                                                ! A.B.C. LOWLAND E. SPRUCE-FIR
              P2=MAX(BLKSPR,TAMRCK,NWHCDR)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.BLKSPR) THEN
                  IFT=125                                                       ! BLACK SPRUCE(125)
                ELSE IF (P2.LT.TAMRCK) THEN
                  IFT=126                                                       ! TAMARACK(126)
                ELSE IF (NWHCDR.GT.0.) THEN
                  IFT=127                                                       ! NORTHERN WHITE-CEDAR(127)
                END IF
              END IF
            END IF
          ELSE IF (P1.LT.PNYNJN) THEN                                           ! A.B. PINYON-JUNIPER
            P2=MAX(ERCEDAR,RMJUNIPR,WJUNIPR,JUNIPRW,PINJUNW)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.ERCEDAR) THEN
                IFT=181                                                         ! EASTERN REDCEDAR(181)
              ELSE IF (P2.LT.RMJUNIPR) THEN
                IFT=182                                                         ! ROCKY MTN. JUNIPER(182)
              ELSE IF (P2.LT.WJUNIPR) THEN
                IFT=183                                                         ! WESTERN JUNIPER(183)
              ELSE IF (P2.LT.JUNIPRW) THEN
                IFT=184                                                         ! JUNIPER WOODLAND(184)
              ELSE IF (PINJUNW.GT.0.) THEN
                IFT=185                                                         ! PINYON-JUNIPER WOODLAND(185)
              END IF
            END IF
          ELSE IF (P1.LT.EXSFTWD) THEN                                          ! A.B. EXOTIC SOFTWOODS
            P2=MAX(SCOTCHP,JAPBLKP,OTHEXSWD)-.001
            IF (P2.GT.0.) THEN
              IF (P2.LT.SCOTCHP) THEN
                IFT=381                                                         ! SCOTCH PINE(381)
              ELSE IF (P2.LT.JAPBLKP) THEN
                IFT=383                                                         ! JAPANESE BLACK PINE(382)/(383)
              ELSE IF (OTHEXSWD.GT.0.) THEN
                IFT=383                                                         ! OTHER EXOTIC SOFTWOODS(383)
              END IF
            END IF
          ELSE IF (P1.LT.OTHSFT) THEN                                           ! A.B. FVS - OTHER SOFTWOODS
            P2=MAX(OTHSFT,0.)-.001
            IF (P2.GT.0.) THEN
              IF (OTHSFT.GT.0.) THEN
                IFT=996                                                         ! FVS - OTHER SOFTWOODS(996)
              END IF
            END IF
          END IF
        END IF
      ELSE                                                                      ! HARDWOODS
        IFT=997                                                                 ! FVS OTHER HARDWOODS(997)
        IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_5: ICYC,OAKPIN = ',
     &  ICYC,OAKPIN
        IF (OAKPIN.GE.QUARTR) THEN                                              ! A.B. OAK-PINE
          P1=MAX(ERCEDAR,SHRTLFP,EWHPIN,LNGLFP,VIRGP,LOBP,SLASHP,
     &       JACKPIN,REDPIN,SANDP,TBLMTNP,PITCHP,PONDP,SPRUCEP)-.001
          IF (P1.GT.0.) THEN
            IF (P1.LT.ERCEDAR) THEN
              IFT=402                                                           ! E. REDCEDAR-HARDWOOD(402)
            ELSE IF (P1.LT.SHRTLFP) THEN
              IFT=404                                                           ! SHORTLEAF PINE-OAK(404)
            ELSE IF (P1.LT.EWHPIN) THEN
              IFT=401                                                           ! E. WHITE PINE-RED OAK-WHITE ASH(401)
            ELSE IF (P1.LT.LNGLFP) THEN
              IFT=403                                                           ! LONGLEAF PINE-OAK(403)
            ELSE IF (P1.LT.VIRGP) THEN
              IFT=405                                                           ! VIRGINIA PINE-S. RED OAK(405)
            ELSE IF (P1.LT.LOBP) THEN
              IFT=406                                                           ! LOBLOLLY PINE-HARDWOOD((406)
            ELSE IF (P1.LT.SLASHP) THEN
              IFT=407                                                           ! SLASH PINE-HARDWOOD(407)
            ELSE IF (JACKPIN+REDPIN+SANDP+TBLMTNP
     &              +PITCHP+PONDP+SPRUCEP.GT.0) THEN
              IFT=409                                                           ! OTHER PINE-HARDWOOD(409)
            END IF
          END IF
        ELSE
          IF (PSTBLJKO.GT.0.01) PSTBLJKO=PSTBLJKO+SROAK
          IF (CHSTOAK.GT.0.01) CHSTOAK=CHSTOAK+SROAK
          IF (BAELMMAP.GT.0.01) BAELMMAP=BAELMMAP+AELM+REDMAPL
     &                                  +SLVRMPL+WHASH+ECTNWD
          UPLAND=0.0                                                            ! SET FLAG FOR UPLAND SITE
          P1=MAX(LWELASCW,LWOKGMCY,UPMPBCBR,UPOAKHIC)-.001
          IF (P1.GT.0) THEN
            IF (P1.LT.LWELASCW) THEN
              UPLAND=0.0
            ELSE IF (P1.LT.LWOKGMCY) THEN
              UPLAND=0.0
            ELSE IF (P1.LT.UPMPBCBR) THEN
              UPLAND=1.0
            ELSE IF (UPOAKHIC.GT.0.) THEN
              UPLAND=1.0
            END IF
          END IF
          IF (UPLAND.EQ.1.0) THEN
            OAKHCK=OAKHCK+SWEETGUM+UPLOOAK+AELM+WCELM+HOLLY+BLKGUM
            SELECT CASE (VARACD)
              CASE ('SN')
                SOUTH=1.0
              CASE DEFAULT
                SOUTH=0.0
            END SELECT
            IF (SOUTH.EQ.1.0) THEN
              OAKHCK=OAKHCK+UPLOHWD+BEECH+BLCH
            ELSE
              IF (REDMAPL.GE.HALF) THEN
                OAKHCK=OAKHCK+UPLOHWD+BEECH+BLCH
              ELSE IF ((UPLOHWD+BEECH+BLCH).LT.HALF) THEN
                IF (OAKHCK.GT.FIVE) THEN
                  OAKHCK=OAKHCK+UPLOHWD+BEECH+BLCH
                END IF
              END IF
              MBB=MBB+UPLOHWD+BEECH+BLCH+AELM
              IF (MBB.GT.FIVE) THEN
                IF ((NROAK+YP+BWALNUT).LT.HALF) THEN
                  MBB=MBB+NROAK+YP+BWALNUT
                END IF
              END IF
            END IF
          ELSE
            UPLAND = 0.0                                                        ! SET FLAG FOR LOWLAND SITE
            OKGMCYP=OKGMCYP+UPLOOAK
            IF (OKGMCYP.GT.0.01) THEN
              OKGMCYP=OKGMCYP+UPLOHWD+SWEETGUM+AELM
     &                        +BEECH+HOLLY+BLKGUM+PINOAK
            END IF
            ELMASHCW=ELMASHCW+AELM+WCELM
            IF (ELMASHCW.GT.0.01) THEN
              ELMASHCW=ELMASHCW+UPLOHWD+SWEETGUM+BWALNUT
            ELSE IF (OKGMCYP.LT. 0.01) THEN
              IF (OAKHCK.GT.MBB) THEN
                OAKHCK=OAKHCK+UPLOHWD+BLCH+BEECH
              ELSE
                MBB=MBB+UPLOHWD+BLCH+BEECH
              END IF
            END IF
          END IF
          IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_6: ICYC,UPLAND,',
     &    'SOUTH = ',ICYC,UPLAND,SOUTH
          IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_7: ICYC,',
     &    'MBB,ELMASHCW,OAKHCK,OKGMCYP,ASPBRCH,ALDRMAPL,',
     &    'WSTOAK,TANOKLRL,OTHWHWD,TROPHWDS,EXHWDS,',
     &    'OTHHRD,OTHSPC = '
          IF(DEBUG)WRITE(JOSTND,*)ICYC,MBB,ELMASHCW,OAKHCK,OKGMCYP,
     &    ASPBRCH,ALDRMAPL,WSTOAK,TANOKLRL,OTHWHWD,TROPHWDS,EXHWDS,
     &    OTHHRD,OTHSPC
          P1=MAX(MBB,ELMASHCW,OAKHCK,OKGMCYP,ASPBRCH,ALDRMAPL,
     &           WSTOAK,TANOKLRL,OTHWHWD,TROPHWDS,EXHWDS,
     &           OTHHRD,OTHSPC)-.001
          IF (P1.GT.0.) THEN
            IF (P1.LT. MBB) THEN                                                ! A.B. MAPLE-BEECH-BIRCH
              IF (BLCH.GE. HALF) THEN
                IFT=802                                                         ! BLACK CHERRY(802)
              ELSE IF (REDMAPL.GE.HALF) THEN
                IFT=809                                                         ! RED MAPLE-UPLAND(809)
              ELSE
                P2=MAX(BCWAYP,MPLBASWD,ELMASHLO,MPLBCHYB)-.001
                IF (P2.GT.0.) THEN
                  IF (P2.LT.BCWAYP) THEN
                    IFT=803                                                     ! B.CHERRY-W.ASH-Y.POPLAR(803)
                  ELSE IF (P2.LT.MPLBASWD) THEN
                    IFT=805                                                     ! HARD MAPLE-BASSWOOD(805)
                  ELSE IF (P2.LT.ELMASHLO) THEN
                    IFT=807                                                     ! ELM-ASH-LOCUST(807)
                  ELSE IF (MPLBCHYB.GT.0.) THEN
                    IFT=801                                                     ! MAPLE-BEECH-YELLOW BIRCH(801)
                  END IF
                END IF
              END IF
            ELSE IF (P1.LT.ELMASHCW) THEN                                       ! A.B. ELM-ASH-COTTONWOOD
              IF (CTNWD.GE.HALF) THEN
                IFT=703                                                         ! COTTONWOOD(703)
              ELSE IF (WILLOW.GE.HALF) THEN
                IFT=704                                                         ! WILLOW(704)
              ELSE IF (REDMAPL.GE.HALF) THEN
                IFT=708                                                         ! RED MAPLE-LOWLAND(708)
              ELSE
                P2=MAX(RBRCHSYC,SYCPCELM,BAELMMAP,SGBELMGA,
     &                 SLVMAELM,CTNWWILO,ORGNASH)-.001
                IF (P2.GT.0.) THEN
                  IF (P2.LT.RBRCHSYC) THEN
                    IFT=702                                                     ! RIVER BIRCH-SYCAMORE(702)
                  ELSE IF (P2.LT.SYCPCELM) THEN
                    IFT=705                                                     ! SYCAMORE-PECAN-ELM(705)
                  ELSE IF (P2.LT.BAELMMAP) THEN
                    IFT=701                                                     ! B.ASH-ELM-MAPLE(701)
                  ELSE IF (P2.LT.SGBELMGA) THEN
                    IFT=706                                                     ! SUGARBERRY-HACKBERRY-ELM-GREEN ASH(706)
                  ELSE IF (P2.LT.SLVMAELM) THEN
                    IFT=707                                                     ! SILVER MAPLE-ELM(707)
                  ELSE IF (P2.LT.CTNWWILO) THEN
                    IFT=709                                                     ! COTTONWOOD-WILLOW(709)
                  ELSE IF (ORGNASH.GT.0.) THEN
                    IFT=722                                                     ! OREGON ASH(722)
                  END IF
                END IF
              END IF
            ELSE IF (P1.LT.OAKHCK) THEN                                         ! A.B. OAK-HICKORY
              IF (WHOAK.GE.HALF) THEN
                IFT=504                                                         ! WHITE OAK(504)
              ELSE IF (BUROAK.GE.HALF) THEN
                IFT=509                                                         ! BUR OAK(509)
              ELSE IF (CHSTOAK.GE.HALF) THEN
                IFT=502                                                         ! CHESTNUT OAK(502)
              ELSE IF (NROAK.GE.HALF) THEN
                IFT=505                                                         ! N.RED OAK(505)
              ELSE IF (SCRLTOAK.GE.HALF) THEN
                IFT=510                                                         ! SCARLET OAK(510)
              ELSE IF (YP.GE.HALF) THEN
                IFT=511                                                         ! YELLOW POPLAR(511)
              ELSE IF (BWALNUT.GE.HALF) THEN
                IFT=512                                                         ! BLACK WALNUT(512)
              ELSE IF (BLCST.GE.HALF) THEN
                IFT=513                                                         ! BLACK LOCUST(513)
              ELSE IF (REDMAPL.GE.HALF) THEN
                IFT=519                                                         ! RED MAPLE-OAK(519)
              ELSE
                QOAK2=.25*OAKHCK
                P2=MAX(PSTBLJKO,CHBLSC,WOROHK,YPWORO,
     &                 SCRUBOAK,SWGYP,SASFPRSM,MXDHWD)
                IF (P2.GE.QOAK2) THEN
                  P2=P2-.001
                  IF (P2.LT.PSTBLJKO) THEN
                    IFT=501                                                     ! POST-BLACKJACK OAK(501)
                  ELSE IF (P2.LT.CHBLSC) THEN
                    IFT=515                                                     ! CHSTNUT-BLCK-SCRLET OAK(515)
                  ELSE IF (P2.LT.WOROHK) THEN
                    IFT=503                                                     ! W.OAK-R.OAK-HICKORY(503)
                  ELSE IF (P2.LT.YPWORO) THEN
                    IFT=506                                                     ! Y.POPLAR-W.OAK-RED OAK(506)
                  ELSE IF (P2.LT.SCRUBOAK) THEN
                    IFT=514                                                     ! SOUTHERN SCRUB-OAK(514)
                  ELSE IF (P2.LT.SWGYP) THEN
                    IFT=508                                                     ! SWEETGUM-Y.POP(508)
                  ELSE IF (P2.LT.SASFPRSM)THEN
                    IFT=507                                                     ! SASAFRAS-PERSIMMON(507)
                  ELSE IF (MXDHWD.GT.0.) THEN
                    IFT=520                                                     ! MIXED UPLAND HARDWOODS(520)
                  END IF
                ELSE
                  IF (MXDHWD.GT.0.) THEN
                    IFT=520                                                     ! MIXED UPLAND HARDWOODS(520)
                  END IF
                END IF
              END IF
            ELSE IF (P1.LT.OKGMCYP) THEN                                        ! A.B. OAK-GUM-CYPRESS
              IF (ATLWCDR.GE.HALF) THEN
                IFT=606                                                         ! ATLANTIC WHITE CEDAR(606)
              ELSE
                P2=MAX(SBSTRM,SCHCHBO,SWGWILO,
     &                 CYPWTUP,OVRCUPWH)-.001
                IF (P2.GT.0.) THEN
                  IF (P2.LT.SBSTRM) THEN
                    IFT=608                                                     ! SWEETBAY,SWAMP TPLO,RED MPL(608)
                  ELSE IF (P2.LT.SCHCHBO) THEN
                    IFT=601                                                     ! SWAMP CHESTNUT-CHERRY.BRK OAK(601)
                  ELSE IF (P2.LT.SWGWILO) THEN
                    IFT=602                                                     ! SWEETGUM, NUTTALL-WILLOW-OAK(602)
                  ELSE IF (P2.LT.CYPWTUP) THEN
                    IFT=607                                                     ! CYPRESS-WATER TUPELO(607)
                  ELSE IF (OVRCUPWH.GT.0.) THEN
                    IFT=605                                                     ! OVRCUP,WATER OAK HICKORY(605)
                  END IF
                END IF
              END IF
            ELSE IF (P1.LT.ASPBRCH) THEN                                        ! A.B. ASPEN-BIRCH
              P2=MAX(ASPEN,BLSMPOP,PAPRBRCH)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.ASPEN) THEN
                  IFT=901                                                       ! ASPEN(901)
                ELSE IF (P2.LT.BLSMPOP) THEN
                  IFT=904                                                       ! BALSAM POPLAR(904)
                ELSE IF (PAPRBRCH.GT.0.) THEN
                  IFT=902                                                       ! PAPER BIRCH(902)
                END IF
              END IF
            ELSE IF (P1.LT.ALDRMAPL) THEN                                       ! A.B. RED ALDER-MAPLE
              P2=MAX(REDALDR,BGLFMPL)-.001
              IF (P2.LT.REDALDR) THEN
                IFT=911                                                         ! RED ALDER(911)
              ELSE IF (BGLFMPL.GT.0.) THEN
                IFT=912                                                         ! BIGLEAF MAPLE(912)
              END IF
            ELSE IF (P1.LT.WSTOAK) THEN                                         ! A.B. WESTERN OAKS
              P2=MAX(CLFBO,ORGWHO,BLUEOAK,GRAYPINE,
     &               COSTLO,CANYLO,DOAKWDLD,EOAKWDLD)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.CLFBO) THEN
                  IFT=922                                                       ! CALIFORNIA BLACK OAK(922)
                ELSE IF (P2.LT.ORGWHO) THEN
                  IFT=923                                                       ! OREGON WHITE OAK(923)
                ELSE IF (P2.LT.BLUEOAK) THEN
                  IFT=924                                                       ! BLUE OAK(924)
                ELSE IF (P2.LT.GRAYPINE) THEN
                  IFT=921                                                       ! GRAY PINE(921)
                ELSE IF (P2.LT.COSTLO) THEN
                  IFT=931                                                       ! COAST LIVE OAK(931)
                ELSE IF (P2.LT.CANYLO) THEN
                  IFT=932                                                       ! CANYON-INTERIOR LIVE OAK(932)
                ELSE IF (P2.LT.DOAKWDLD) THEN
                  IFT=925                                                       ! DECIDUOUS OAK WOODLAND(925)
                ELSE IF (EOAKWDLD.GT.0.) THEN
                  IFT=926                                                       ! EVERGREEN OAK WOODLAND(926)
                END IF
              END IF
            ELSE IF (P1.LT.TANOKLRL) THEN                                       ! A.B. TAN OAK-LAUREL
              P2=MAX(TANOAK,CLFLREL,GNTCHNK)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.TANOAK) THEN
                  IFT=941                                                       ! TAN OAK(941)
                ELSE IF (P2.LT.CLFLREL) THEN
                  IFT=942                                                       ! CALIFORNIA LAUREL(942)
                ELSE IF (GNTCHNK.GT.0.) THEN
                  IFT=943                                                       ! GIANT CHINKAPIN(943)
                END IF
              END IF
            ELSE IF (P1.LT.OTHWHWD) THEN                                        ! A.B. OTHER WESTERN HARDWOODS
              P2=MAX(PACMDRN,MESQUITW,MTNBRSHW,
     &               INTMAPLW,MISCWHWD)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.PACMDRN) THEN
                  IFT=951                                                       ! PACIFIC MADRONE(951)
                ELSE IF (P2.LT.MESQUITW) THEN
                  IFT=952                                                       ! MESQUITE-WOODLAND(952)
                ELSE IF (P2.LT.MTNBRSHW) THEN
                  IFT=953                                                       ! MTN. BRUSH WOODLAND(953)
                ELSE IF (P2.LT.INTMAPLW) THEN
                  IFT=954                                                       ! INT. MTN. MAPLE WOODLAND(954)
                ELSE IF (MISCWHWD.GT.0.) THEN
                  IFT=955                                                       ! MISC. W. HARDWOODS(955)
                END IF
              END IF
            ELSE IF (P1.LT.TROPHWDS) THEN                                       ! A.B. TROPICAL HARDWOODS
              P2=MAX(PALM,MANGRV)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.PALM) THEN
                  IFT=981                                                       ! SABAL PALM(981)
                ELSE IF (MANGRV.GT.0.) THEN
                  IFT=982                                                       ! MANGROVE(982)
                END IF
              END IF
            ELSE IF (P1.LT.EXHWDS) THEN                                         ! A.B. EXOTIC HARDWOODS
              P2=MAX(PALONIA,MELUCA,EUCLPT,OTHEXHWD)-.001
              IF (P2.GT.0.) THEN
                IF (P2.LT.PALONIA) THEN
                  IFT=991                                                       ! ROYAL PAULOWNIA(991)
                ELSE IF (P2.LT.MELUCA) THEN
                  IFT=992                                                       ! MELALUCA(992)
                ELSE IF (P2.LT.EUCLPT) THEN
                  IFT=993                                                       ! EUCALYPTUS(993)
                ELSE IF (OTHEXHWD.GT.0.) THEN
                  IFT=995                                                       ! OTHER EXOTIC HARDWOODS(995)
                END IF
              END IF
            ELSE IF (P1.LT.OTHHRD) THEN                                         ! A.B. FVS - OTHER HARDWOODS
              P2=MAX(OTHHRD,0.)-.001
              IF (P2.GT.0.) THEN
                IF (OTHHRD.GT.0.) THEN
                  IFT=997                                                       ! FVS - OTHER HARDWOODS(997)
                END IF
              END IF
            ELSE IF (P1.LT.OTHSPC) THEN                                         ! A.B. FVS - ALL OTHER SPECIES
              P2=MAX(OTHSPC,0.)-.001
              IF (P2.GT.0.) THEN
                IF (OTHSPC.GT.0.) THEN
                  IFT=998                                                       ! FVS - ALL OTHER SPECIES(998)
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
C --- SPECIAL MIXED CONIFER TEST FOR CERTAIN COUNTIES IN CALIFORNIA;
         IF (DEBUG) WRITE(JOSTND,*)' IN FORTYP_8: KODFOR,IRGN,ISTATE,',
     &   'ICNTY,TLAT,TLONG = ',KODFOR,IRGN,ISTATE,ICNTY,TLAT,TLONG
      IF (ISTATE.EQ.06.OR.IRGN.EQ.5) THEN                                       ! STATE IS CALIFORNIAT
        IF (ICNTY.NE.000.AND.
     &    ICNTY.NE.015.AND.ICNTY.NE.023.AND.ICNTY.NE.041.AND.
     &    ICNTY.NE.045.AND.ICNTY.NE.055.AND.ICNTY.NE.081.AND.
     &    ICNTY.NE.085.AND.ICNTY.NE.087.AND.ICNTY.NE.097) THEN                  ! N. CA. COUNTIES
          IF (IFT.EQ.201) IFT=371                                               ! DOUGLAS-FIR NOT ALONG N. CA COAST
        ELSE IF (TLAT.GT.0.AND.TLONG.GT.0) THEN
          IF (.NOT.((TLAT.GE.37.AND.TLAT.LE.42).AND.
     &              (TLONG.GE.123.AND.TLONG.LE.125))) THEN                      ! APPROXIMATION FOR N. CA. COUNTIES
            IF (IFT.EQ.201) IFT=371                                             ! DOUGLAS-FIR NOT ALONG N. CA COAST
          END IF
        END IF
        IF (IFT.EQ.224.OR.IFT.EQ.222) IFT=371                                   ! SUGAR PINE/INCENSE CEDAR
        IF (IFT.EQ.221.OR.IFT.EQ.223) THEN                                      ! PONDEROSA/JEFFREY PINE
          IF (PNDROSA.LT.EIGHTY) IFT=371
        END IF
        IF (IFT.EQ.261.OR.IFT.EQ.262) THEN                                      ! WHITE FIR/RED FIR
          IF (TRUFIR.LT.EIGHTY) IFT=371
        END IF
      END IF
C
  999 CONTINUE
         IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_9: IFT,JFT,LSTART = ',
     &   IFT,JFT,LSTART
      IF (IFT.EQ.0) IFT=998                                                     ! OTHER OR UNKNOWN
      IF (LSTART) THEN
        IF (JFT.GT.0.AND.JFT.NE.IFT) THEN
          WRITE (JOSTND,'(/10A)')
     &     ' ********   WARNING, INITIAL USER INPUT FOREST TYPE ',
     &            'DOES NOT MATCH FOREST TYPE CALCULATED BY MODEL'
        IFT=JFT
        END IF
      END IF
      IF (LFLAGV) IFT=JFT                                                       ! USER SET VALID FOREST TYPE FOR ALL CYCLES
      FIATYP = FLOAT(IFT)
C
C----------
      IF (DEBUG) WRITE (JOSTND,*) ' IN FORTYP_10: ICYC,IFT = ',
     &ICYC,IFT
C----------
C  SET FOREST TYPE VARIABLE (IFORTP) CARRIED IN PLOT.F77
C  INVENTORY FOREST TYPE IS SAVED FOR POTENTIAL USE IN CYCLES WHERE
C  FOREST TYPE CANNOT BE DETERMINED BY CALCULATION.
C----------
      IFORTP= INT(FIATYP)
      IF (ICYC .EQ. 0) IIFORTP=IFORTP

 9999 CONTINUE
C
      IF (DEBUG) WRITE (JOSTND,*)' IN FORTYP_10: FIATYP,SDIFIA,',
     &'ISZCL,ISTCL,IIFORTP = ',FIATYP,SDIFIA,ISZCL,ISTCL,IIFORTP
C
C
      RETURN
      END
