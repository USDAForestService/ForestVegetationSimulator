      SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
      IMPLICIT NONE
C----------
C  **ECOCLS---EC      DATE OF LAST REVISION:  05/09/12
C
C  SETS DEFAULT MAX SDI VALUES, SITE INDICIES, AND SITE SPECIES
C  BY PLANT ASSOCIATION (ECOCLASS CODE)
C
C  CALLED FROM SITSET
C----------
C DEFINITION OF VARIABLES AND TERMS:
C  APASS     ECOCLASS ALPHA CODE                           (RECEIVED)
C  ASPEC     ALPHA SPECIES CODE                            (RETURNED)
C  SDI       MAXIMUM SDI FOR THIS SPECIES                  (RETURNED)
C  SITE      SITE INDEX FOR THIS SPECIES                   (RETURNED)
C  ISIFLG    SITE SPECIES FLAG                             (RETURNED)
C            0 = NOT THE SITE SPECIES    1 = SITE SPECIES
C  NUM       NUMBER OF SPECIES SPECIFIED FOR THIS ECOCLASS (RETURNED)
C  INDEX     ARRAY INDEX OF THE VALUES BEING RETURNED      (RECEIVED &
C                                                             RETURNED)
C  ISEQ      FVS SEQUENCE NUMBER FOR THIS SPECIES          (RETURNED)
C
C  GBA       GROWTH BASAL AREA USED FOR COMPUTING MAX SDI.
C            MAX SDI = GBA * 1.5 * 1.84
C            (Personal comunication with Fred Hall, R6/NR)
C
C  PA(i)     ECOCLASS CODE FOR PLANT ASSOCIATION i
C  SPC(j)    SPECIES j FOR PLANT ASSOCIATION i
C  SITE(j)   SITE INDEX FOR SPECIES j
C            (BASED ON THE SITE REFERENCE USED IN THIS VARIANT)
C  SDIMX(j)  MAXIMUM SDI FOR SPECIES j
C  NUMBR(j)  NUMBER OF SPECIES SPECIFIED FOR PLANT ASSOCIATION i
C  FVSSEQ(j) FVS SEQUENCE NUMBER FOR SPECIES j
C
C  THE MAX SDI VALUES FOR THE FOLLOWING PLANT ASSOCIATIONS ARE FROM
C  AN R6 ALL CVS DATA STUDY, JUNE 2008: 1,2,5-8,11-13,15,17,21,22,
C  24-33,36-40,42,43,45-54,56,57,59-62,64-72,74,76-78,80,82,84-86,
C  88-95,97-99,101,102,104,106,107,109-111,114,117-119,123,125,126,
C  129-140,142-146,148-150,152,153
C----------
      INTEGER NENTRY
      PARAMETER (NENTRY=155)
      CHARACTER*4 SPC(NENTRY),ASPEC
      CHARACTER*6 SCIEN(NENTRY)
      CHARACTER*8 APASS,PA(NENTRY)
      INTEGER FVSSEQ(NENTRY)
      REAL SITE(NENTRY),SDIMX(NENTRY),RSI,RSDI
      INTEGER NUMBR(NENTRY),IFLAG(NENTRY),ISEQ,INDEX,NUM,ISFLAG
      INTEGER I,K
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=1,15) /
C-----------------------------------------------------------------------
C      ALPHA     SCIEN         ALPHA        NUM  SITE FVS  PLANT
C       ECO       SITE     MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
C      CLASS      SPEC     SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
C-----------------------------------------------------------------------
C    1 = PIAL/CARU                            GBA: 101   p. 262
C    Whitebark pine/pinegrass                            PNW-GTR-360
C
     &'CAG112  ','PSME  ', 625,'DF  ',  25,   1,   1,   3,
C-----------------------------------------------------------------------
C    2 = PIAL/VASC/LUHI                       GBA:  65   p. 248
C    Whitebark pine/grouse huckleberry/smooth woodrush   PNW-GTR-359
C
     &'CAS311  ','ABLA2 ', 700,'AF  ',  45,   1,   1,   9,
C----------------------------------------------------------------------
C    3 = THPL-ABGR/ACTR                       GBA: 308   p. 115
C    Western redcedar-grand fir/vanilla leaf             R6 E TP-004-88
C
     &'CCF211  ','PSME  ', 850,'DF  ',  72,   1,   1,   3,
C----------------------------------------------------------------------
C    4 = THPL/ACTR                            GBA: 368   p. 93
C    Western redcedar/vanilla leaf                       R6 E TP-006-88
C
     &'CCF212  ','ABGR  ',1016,'GF  ',  71,   1,   1,   6,
C----------------------------------------------------------------------
C    5 = THPL/CLUN                            GBA: 317   p. 246
C    Western redcedar/queencup beadily                   PNW-GTR-360
C
     &'CCF221  ','PSME  ', 840,'DF  ',  64,   1,   1,   3,
C-----------------------------------------------------------------------
C    6 = THPL/ARNU3                           GBA: 380   p. 240
C    Western redcedar/wild sarsaparilla                  PNW-GTR-360
C
     &'CCF222  ','PSME  ', 670,'DF  ',  69,   1,   1,   3,
C-----------------------------------------------------------------------
C    7 = THPL/OPHO                            GBA: 473   p. 251
C    Western redcedar/devil's club                       PNW-GTR-360
C
     &'CCS211  ','THPL  ', 775,'RC  ',  96,   1,   1,   5,
C-----------------------------------------------------------------------
C    8 = THPL/VAME                            GBA: 180   p. 256
C    Western redcedar/big huckleberry                    PNW-GTR-360
C
     &'CCS311  ','PSME  ', 815,'DF  ',  63,   1,   1,   3,
C-----------------------------------------------------------------------
C    9 = PSME/PEFR3                           GBA:  83   p. 82
C    Douglas-fir/shrubby penstemon                       PNW-GTR-359
C
     &'CDF411  ','PSME  ', 229,'DF  ',  58,   1,   1,   3,
C-----------------------------------------------------------------------
C   10 = PSME/ARUV-OKAN                       GBA: 120   p. 27
C    Douglas-fir/bearberry (Okanogan)                    R6 E 132b-83
C
     &'CDG123  ','PSME  ', 331,'DF  ',  38,   1,   1,   3,
C-----------------------------------------------------------------------
C   11 = PSME/CARU-O&C                        GBA: 173   p. 49
C    Douglas-fir/pinegrass (Okanogan & Colville)         PNW-GTR-360
C
     &'CDG131  ','PSME  ', 530,'DF  ',  58,   1,   1,   3,
C-----------------------------------------------------------------------
C   12 = PSME/CAGE-WEN                        GBA: 220   p. 60
C    Douglas-fir/elk sedge (Wenatchee)                   PNW-GTR-359
C
     &'CDG132  ','PSME  ', 550,'DF  ',  69,   1,   1,   3,
C-----------------------------------------------------------------------
C   13 = PSME/CARU-AGSP                       GBA: 115   p. 64
C    Douglas-fir/pinegrass-bluebunch wheatgrass          PNW-GTR-359
C
     &'CDG134  ','PSME  ', 430,'DF  ',  61,   1,   1,   3,
C-----------------------------------------------------------------------
C   14 = PSME/CAGE                            GBA: 160   p. 51
C    Douglas-fir/elk sedge                               R6 E TP-004-88
C
     &'CDG141  ','PSME  ', 442,'DF  ',  55,   1,   1,   3,
C-----------------------------------------------------------------------
C   15 = PIPO-PSME/AGSP                       GBA:  73   p. 44
C    Ponderosa pine-Douglas-fir/bluebunch wheatgrass     PNW-GTR-360
C
     &'CDG311  ','PIPO  ', 270,'PP  ',  79,   1,   1,  10/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=16,30) /
C-----------------------------------------------------------------------
C   16 = PSME/FEOC                            GBA: 235   p. 55
C    Douglas-fir/western fescue                          R6 E TP-004-88
C
     &'CDG321  ','PSME  ', 649,'DF  ',  67,   1,   1,   3,
C-----------------------------------------------------------------------
C   17 = PSME/AGSP-WEN                        GBA:  79   p. 58
C    Douglas-fir/bluebunch wheatgrass (Wenatchee)        PNW-GTR-359
C
     &'CDG322  ','PSME  ', 235,'DF  ',  39,   1,   1,   3,
C-----------------------------------------------------------------------
C   18 = PSME/AGSP-ASDE                       GBA:  68   p. 80
C    Douglas-fir/bluebunch wheatgrass-podfern            PNW-GTR-359
C
     &'CDG323  ','PSME  ', 188,'DF  ',  58,   1,   1,   3,
C-----------------------------------------------------------------------
C   19 = PSME/HODI/CAGE                       GBA: 245   p. 59
C    Douglas-fir/oceanspray/elk sedge                    R6 E TP-004-88
C
     &'CDS231  ','PSME  ', 676,'DF  ',  80,   1,   1,   3,
C-----------------------------------------------------------------------
C   20 = PSME/ACCI/FEOC                       GBA: 261   p. 45
C    Douglas-fir/vine maple/western fescue               R6 E TP-006-88
C
     &'CDS241  ','PSME  ', 720,'DF  ',  76,   1,   1,   3,
C-----------------------------------------------------------------------
C   21 = PSME/PAMY-OKAN                       GBA: 225   p. 41
C    Douglas-fir/pachistima (Okanogan)                   R6 E 132b-83
C
     &'CDS411  ','PSME  ', 630,'DF  ',  59,   1,   1,   3,
C-----------------------------------------------------------------------
C   22 = PSME/PAMY/CARU                       GBA: 173   p. 81
C    Douglas-fir/pachistima/pinegrass                    PNW-GTR-359
C
     &'CDS412  ','PSME  ', 450,'DF  ',  57,   1,   1,   3,
C----------------------------------------------------------------------
C   23 = PSME/ARUV-PUTR                       GBA:  84   p. 24
C    Douglas-fir/bearberry-bitterbrush                   R6 E 132b-83
C
     &'CDS631  ','PSME  ', 232,'DF  ',  45,   1,   1,   3,
C----------------------------------------------------------------------
C   24 = PSME/SYOR-O&C                        GBA: 126   p. 71
C    Douglas-fir/Mt. snowberry (Okanogan and Colville)   PNW-GTR-360
C
     &'CDS632  ','PSME  ', 400,'DF  ',  54,   1,   1,   3,
C----------------------------------------------------------------------
C   25 = PSME/SYAL                            GBA: 234   p. 66
C    Douglas-fir/common snowberry                        PNW-GTR-360
C
     &'CDS633  ','PSME  ', 475,'DF  ',  81,   1,   1,   3,
C---------------------------------------------------------------------
C   26 = PSME/SYAL-WEN                        GBA: 244   p. 72
C    Douglas-fir/common snowberry  (Wenatchee)           PNW-GTR-359
C
     &'CDS636  ','PSME  ', 580,'DF  ',  80,   1,   1,   3,
C---------------------------------------------------------------------
C   27 = PSME/SYAL/AGSP                       GBA: 123   p. 74
C    Douglas-fir/common snowberry/bluebunch wheatgrass   PNW-GTR-359
C
     &'CDS637  ','PSME  ', 325,'DF  ',  67,   1,   1,   3,
C---------------------------------------------------------------------
C   28 = PSME/SYAL/CARU                       GBA: 208   p. 76
C    Douglas-fir/common snowberry/pinegrass              PNW-GTR-359
C
     &'CDS638  ','PSME  ', 425,'DF  ',  77,   1,   1,   3,
C---------------------------------------------------------------------
C   29 = PSME/SPBEL/CARU                      GBA: 177   p. 70
C    Douglas-fir/shiny-leaf spirea/pinegrass             PNW-GTR-359
C
     &'CDS639  ','PSME  ', 550,'DF  ',  65,   1,   1,   3,
C---------------------------------------------------------------------
C   30 = PSME/SPBEL                           GBA: 151   p. 82
C    Douglas-fir/shiny-leaf spirea                       PNW-GTR-359
C
     &'CDS640  ','PSME  ', 555,'DF  ',  68,   1,   1,   3/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=31,45) /
C---------------------------------------------------------------------
C   31 = PSME/ARUV-WEN                        GBA: 173   p. 80
C    Douglas-fir/bearberry  (Wenatchee)                  PNW-GTR-359
C
     &'CDS653  ','PSME  ', 460,'DF  ',  37,   1,   1,   3,
C---------------------------------------------------------------------
C   32 = PSME/ARUV-PUTR                       GBA:  67   p. 81
C    Douglas-fir/bearberry-bitterbrush                   PNW-GTR-359
C
     &'CDS654  ','PSME  ', 375,'DF  ',  51,   1,   1,   3,
C---------------------------------------------------------------------
C   33 = PSME/ARUV/CARU                       GBA: 103   p. 80
C    Douglas-fir/bearberry/pinegrass                     PNW-GTR-359
C
     &'CDS655  ','PSME  ', 370,'DF  ',  40,   1,   1,   3,
C----------------------------------------------------------------------
C   34 = PSME/SYAL-MTH                        GBA: 278   p. 67
C    Douglas-fir/common snowberry (Mt Hood)              R6 E TP-004-88
C
     &'CDS661  ','PSME  ', 767,'DF  ',  84,   1,   1,   3,
C----------------------------------------------------------------------
C   35 = PSME/ARNE                            GBA: 405   p. 63
C    Douglas-fir/pinemat manzanita                       R6 E TP-004-88
C
     &'CDS662  ','PSME  ',1118,'DF  ',  51,   1,   1,   3,
C----------------------------------------------------------------------
C   36 = PSME/PUTR                            GBA: 101   p. 82
C    Douglas-fir/bitterbursh                             PNW-GTR-359
C
     &'CDS673  ','PSME  ', 525,'DF  ',  50,   1,   1,   3,
C----------------------------------------------------------------------
C   37 = PSME/PUTR/AGSP                       GBA: 145   p. 66
C    Douglas-fir/bitterbursh/bluebunch wheatgrass        PNW-GTR-359
C
     &'CDS674  ','PSME  ', 305,'DF  ',  62,   1,   1,   3,
C----------------------------------------------------------------------
C   38 = PSME/PUTR/CARU                       GBA: 153   p. 68
C    Douglas-fir/bitterbrush/pinegrass                   PNW-GTR-359
C
     &'CDS675  ','PSME  ', 370,'DF  ',  58,   1,   1,   3,
C---------------------------------------------------------------------
C   39 = PSME/PHMA-O&C                        GBA: 220   p. 55
C    Douglas-fir/ninebark (Okanogan & Colville)          PNW-GTR-360
C
     &'CDS715  ','PSME  ', 470,'DF  ',  63,   1,   1,   3,
C-----------------------------------------------------------------------
C   40 = PSME/PHMA-LIBOL                      GBA: 178   p. 61
C    Douglas-fir/ninebark-twinflower                     PNW-GTR-360
C
     &'CDS716  ','PSME  ', 600,'DF  ',  60,   1,   1,   3,
C-----------------------------------------------------------------------
C   41 = PSME/VACCI                           GBA: 144   p. 33
C    Douglas-fir/huckleberry                             R6 E 132b-83
C
     &'CDS811  ','PSME  ', 397,'DF  ',  51,   1,   1,   3,
C----------------------------------------------------------------------
C   42 = PSME/VACA-COL                        GBA: 191   p. 76
C    Douglas-fir/dwarf huckleberry (Colville)            PNW-GTR-360
C
     &'CDS813  ','LAOC  ', 600,'WL  ',  66,   1,   1,   2,
C---------------------------------------------------------------------
C   43 = PSME/VAME-COLV                       GBA: 185   p. 82
C    Douglas-fir/big huckleberry (Colville)              PNW-GTR-360
C
     &'CDS814  ','PSME  ', 585,'DF  ',  66,   1,   1,   3,
C----------------------------------------------------------------------
C   44 = PSME/VACA                            GBA: 131   p. 82
C    Douglas-fir/dwarf huckleberry                       PNW-GTR-359
C
     &'CDS831  ','PSME  ', 362,'DF  ',  60,   1,   1,   3,
C----------------------------------------------------------------------
C   45 = PSME/VAME-WEN                        GBA: 117   p. 83
C    Douglas-fir/big huckleberry (Wenatchee)             PNW-GTR-359
C
     &'CDS832  ','PSME  ', 530,'DF  ',  53,   1,   1,   3/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=46,60) /
C----------------------------------------------------------------------
C   46 = PSME/VAMY/CARU                       GBA: 166   p. 83
C    Douglas-fir/low huckleberry/pinegrass               PNW-GTR-359
C
     &'CDS833  ','PSME  ', 265,'DF  ',  48,   1,   1,   3,
C----------------------------------------------------------------------
C   47 = ABLA2/XETE                           GBA: 222   p. 178
C    Subalpine fir/beargrass                             PNW-GTR-360
C
     &'CEF111  ','ABLA2 ', 905,'AF  ',  54,   1,   1,   9,
C----------------------------------------------------------------------
C   48 = ABLA2/LIBOL-O&C                      GBA: 202   p. 141
C    Subalpine fir/twinflower (Okanogan & Colville)      PNW-GTR-360
C
     &'CEF211  ','ABLA2 ', 685,'AF  ',  80,   1,   1,   9,
C----------------------------------------------------------------------
C   49 = ABLA2/LIBOL-WEN                      GBA: 298   p. 234
C    Subalpine fir/twinflower (Wenatchee)                PNW-GTR-359
C
     &'CEF222  ','PIEN  ', 700,'ES  ',  90,   1,   1,   8,
C----------------------------------------------------------------------
C   50 = ABLA2/CLUN                           GBA: 278   p. 131
C    Subalpine fir/queencup beadily                      PNW-GTR-360
C
     &'CEF421  ','ABLA2 ', 650,'AF  ',  87,   1,   1,   9,
C----------------------------------------------------------------------
C   51 = ABLA2/TRCA3                          GBA: 242   p. 157
C    Subalpine fir/false bugbane                         PNW-GTR-360
C
     &'CEF422  ','ABLA2 ', 745,'AF  ',  87,   1,   1,   9,
C----------------------------------------------------------------------
C   52 = ABLA2/COCA                           GBA: 199   p. 136
C    Subalpine fir/bunchberry dogwood                    PNW-GTR-360
C
     &'CEF423  ','ABLA2 ', 675,'AF  ',  75,   1,   1,   9,
C----------------------------------------------------------------------
C   53 = ABLA2/ARLA-POPU                      GBA: 261   p. 214
C    Subalpine fir/broadleaf arnica-skunkleaf polemonium PNW-GTR-359
C
     &'CEF424  ','ABLA2 ', 880,'AF  ',  65,   1,   1,   9,
C----------------------------------------------------------------------
C   54 = ABLA2/LUHI-WEN                       GBA: 264   p. 218
C    Subalpine fir/smooth woodrush (Wenatchee)           PNW-GTR-359
C
     &'CEG121  ','ABLA2 ', 785,'AF  ',  65,   1,   1,   9,
C----------------------------------------------------------------------
C   55 = ABLA2/CARU-WEN                       GBA: 199   p. 216
C    Subalpine fir/pinegrass  (Wenatchee)                PNW-GTR-359
C
     &'CEG310  ','ABLA2 ', 549,'AF  ',  73,   1,   1,   9,
C----------------------------------------------------------------------
C   56 = ABLA2/CARU-O&C                       GBA: 199   p. 126
C    Subalpine fir/pinegrass (Okanogan & Colville)       PNW-GTR-360
C
     &'CEG311  ','ABLA2 ', 655,'AF  ',  77,   1,   1,   9,
C---------------------------------------------------------------------
C   57 = PIEN/EQAR                            GBA: 191   p. 184
C    Engelmann spruce/horsetail                          PNW-GTR-360
C
     &'CEM211  ','PIEN  ', 535,'ES  ',  72,   1,   1,   8,
C---------------------------------------------------------------------
C   58 = ABLA2/PAMY-OKAN                      GBA: 138   p. 52
C    Subalpine fir/pachistima (Okanogan)                 R6 E 132b-83
C
     &'CES111  ','ABLA2 ', 381,'AF  ',  90,   1,   1,   9,
C---------------------------------------------------------------------
C   59 = ABLA2/PAMY-WEN                       GBA: 254   p. 234
C    Subalpine fir/pachistima (Wenatchee)                PNW-GTR-359
C
     &'CES113  ','PIEN  ', 820,'ES  ', 111,   1,   1,   8,
C--------------------------------------------------------------------
C   60 = ABLA2/RHAL-XETE                      GBA: 211   p. 152
C    Subalpine fir/Cascades azalea-beargrass             PNW-GTR-360
C
     &'CES210  ','ABLA2 ', 790,'AF  ',  56,   1,   1,   9/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=61,75) /
C-------------------------------------------------------------------
C   61 = ABLA2/RHAL                           GBA: 176   p. 220
C    Subalpine fir/Cascade azalea                        PNW-GTR-359
C
     &'CES211  ','ABLA2 ', 790,'AF  ',  52,   1,   1,   9,
C-----------------------------------------------------------------------
C   62 = ABLA2/RHAL/LUHI                      GBA: 198   p. 222
C    Subalpine fir/Cascade azalea/smooth woodrush        PNW-GTR-359
C
     &'CES213  ','ABLA2 ', 665,'AF  ',  60,   1,   1,   9,
C-----------------------------------------------------------------------
C   63 = ABLA2/VACCI                          GBA: 185   p. 46
C    Subalpine fir/huckleberry                           R6 E 132b-83
C
     &'CES312  ','ABLA2 ', 511,'AF  ', 102,   1,   1,   9,
C----------------------------------------------------------------------
C   64 = ABLA2/VAME-COLV                      GBA: 259   p. 168
C    Subalpine fir/big huckleberry (Colville)            PNW-GTR-360
C
     &'CES313  ','ABLA2 ', 700,'AF  ',  76,   1,   1,   9,
C----------------------------------------------------------------------
C   65 = ABLA2/VAME-WEN                       GBA: 265   p. 235
C    Subalpine fir/big huckleberry (Wenatchee)           PNW-GTR-359
C
     &'CES342  ','PSME  ', 810,'DF  ',  73,   1,   1,   3,
C---------------------------------------------------------------------
C   66 = ABLA2/VASC-O&C                       GBA: 239   p. 173
C    Subalpine fir/grouse huckleberry (Okan & Colv)      PNW-GTR-360
C
     &'CES412  ','ABLA2 ', 780,'AF  ',  63,   1,   1,   9,
C---------------------------------------------------------------------
C   67 = ABLA2/VASC/CARU-OKAN                 GBA: 133   p. 236
C    Subalpine fir/grouse huckleberry/pinegrass (Okan)   PNW-GTR-359
C
     &'CES413  ','PIEN  ', 670,'ES  ',  62,   1,   1,   8,
C---------------------------------------------------------------------
C   68 = ABLA2/VACA                           GBA:  96   p. 235
C    Subalpine fir/dwarf huckleberry                     PNW-GTR-359
C
     &'CES422  ','PICO  ', 620,'LP  ',  94,   1,   1,   7,
C----------------------------------------------------------------------
C   69 = ABLA2/RULA                           GBA: 276   p. 224
C    Subalpine fir/dwarf bramble                         PNW-GTR-359
C
     &'CES423  ','ABLA2 ', 785,'AF  ',  90,   1,   1,   9,
C----------------------------------------------------------------------
C   70 = ABLA2/VASC/ARLA                      GBA: 249   p. 230
C    Subalpine fir/grouse huckleberry/broadleaf arnica   PNW-GTR-359
C
     &'CES424  ','ABLA2 ', 785,'AF  ',  51,   1,   1,   9,
C---------------------------------------------------------------------
C   71 = ABLA2/VASC/LUHI                      GBA: 146   p. 232
C    Subalpine fir/grouse huckleberry/smooth woodrush    PNW-GTR-359
C
     &'CES425  ','ABLA2 ', 720,'AF  ',  65,   1,   1,   9,
C---------------------------------------------------------------------
C   72 = ABLA2/VASC-WEN                       GBA: 423   p. 228
C    Subalpine fir/grouse huckleberry (Wenatchee)        PNW-GTR-359
C
     &'CES426  ','PSME  ', 720,'DF  ',  69,   1,   1,   3,
C---------------------------------------------------------------------
C   73 = ABAM/TITRU                           GBA: 447   p. 168
C    Pacific silver fir/coolwort foamflower              PNW-GTR-359
C
     &'CFF162  ','ABAM  ',1234,'SF  ', 143,   1,   1,   4,
C---------------------------------------------------------------------
C   74 = ABAM/ACTR-WEN                        GBA: 294   p. 158
C    Pacific silver fir/vanilla leaf  (Wenatchee)        PNW-GTR-359
C
     &'CFF254  ','ABAM  ', 935,'SF  ', 112,   1,   1,   4,
C--------------------------------------------------------------------
C   75 = ABAM/VAAL-WEN                        GBA: 316   p. 170
C    Pacific Silver fir/Alaska huckleberry (Wenatchee)   PNW-GTR-359
C
     &'CFS232  ','ABAM  ', 872,'SF  ', 104,   1,   1,   4/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=76,90) /
C---------------------------------------------------------------------
C   76 = ABAM/VAME/CLUN-WEN                   GBA: 254   p. 172
C    Silver fir/big huckleberry/queencup beadlily (Wen)  PNW-GTR-359
C
     &'CFS233  ','ABAM  ',1070,'SF  ',  79,   1,   1,   4,
C---------------------------------------------------------------------
C   77 = ABAM/VAME-PYSE                       GBA: 241   p. 174
C    Pacific silver fir/big huckleberry-sidebells pyrola PNW-GTR-359
C
     &'CFS234  ','ABAM  ', 840,'SF  ',  62,   1,   1,   4,
C--------------------------------------------------------------------
C   78 = ABAM/MEFE-WEN                        GBA: 342   p. 160
C    Pacific silver fir/rusty menziesia (Wenatchee)      PNW-GTR-359
C
     &'CFS542  ','ABAM  ', 915,'SF  ',  84,   1,   1,   4,
C---------------------------------------------------------------------
C   79 = ABAM/RHAL-OKAN                       GBA: 234   p. 75
C    Pacific silver fir/Cascade azalea (Okanogan)        R6 E 132b-83
C
     &'CFS553  ','ABAM  ', 646,'SF  ',  45,   1,   1,   4,
C---------------------------------------------------------------------
C   80 = ABAM/RHAL-VAME-WEN                   GBA: 268   p. 164
C    Pac silver fir/Cascade azalea-big huckleberry (Wen) PNW-GTR-359
C
     &'CFS556  ','ABLA2 ', 940,'AF  ',  40,   1,   1,   9,
C-----------------------------------------------------------------------
C   81 = ABAM/PAMY                            GBA: 281   p. 75
C    Pacific silver fir/pachistima                       R6 E 132b-83
C
     &'CFS558  ','PSME  ', 776,'DF  ',  65,   1,   1,   3,
C----------------------------------------------------------------------
C   82 = ABAM/ACCI                            GBA: 306   p. 156
C    Pacific silver fir/vine maple                       PNW-GTR-359
C
     &'CFS621  ','ABAM  ', 550,'SF  ', 104,   1,   1,   4,
C----------------------------------------------------------------------
C   83 = TSHE-ABGR/CLUN                       GBA: 289   p. 111
C    Western hemlock-grand fir/queencup beadlily         R6 E TP-004-88
C
     &'CHC311  ','ABGR  ', 798,'GF  ',  81,   1,   1,   6,
C----------------------------------------------------------------------
C   84 = TSHE/ACTR-WEN                        GBA: 271   p. 138
C   Western hemlock/vanilla leaf (Wenatchee)             PNW-GTR-359
C
     &'CHF223  ','PSME  ', 675,'DF  ',  73,   1,   1,   3,
C---------------------------------------------------------------------
C   85 = TSHE/CLUN                            GBA: 285   p. 204
C   Western hemlock/queencup beadlily                    PNW-GTR-360
C
     &'CHF311  ','PSME  ', 835,'DF  ',  69,   1,   1,   3,
C--------------------------------------------------------------------
C   86 = TSHE/ARNU3                           GBA: 287   p. 199
C    Western hemlock/wild sarsaparilla                   PNW-GTR-360
C
     &'CHF312  ','PSME  ', 775,'DF  ',  75,   1,   1,   3,
C--------------------------------------------------------------------
C   87 = TSHE/ASCA3                           GBA: 454   p. 142
C    Western hemlock/wild ginger                         PNW-GTR-359
C
     &'CHF313  ','PSME  ',1253,'DF  ',  85,   1,   1,   3,
C----------------------------------------------------------------------
C   88 = TSHE/GYDR                            GBA: 506   p. 209
C    Western hemlock/oak-fern                            PNW-GTR-360
C
     &'CHF422  ','PSME  ', 900,'DF  ',  83,   1,   1,   3,
C---------------------------------------------------------------------
C   89 = TSHE/XETE-COLV                       GBA: 362   p. 226
C    Western hemlock/beargrass (Colville)                PNW-GTR-360
C
     &'CHF521  ','PIEN  ', 830,'ES  ',  90,   1,   1,   8,
C----------------------------------------------------------------------
C   90 = TSHE/BENE-WEN                        GBA: 214   p. 144
C    Western hemlock/Cascade Oregon grape (Wenatchee)    PNW-GTR-359
C
     &'CHS142  ','PSME  ', 810,'DF  ',  82,   1,   1,   3/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=91,105) /
C---------------------------------------------------------------------
C   91 = TSHE/PAMY/CLUN                       GBA: 230   p. 146
C    Western hemlock/pachistima/queencup beadlily        PNW-GTR-359
C
     &'CHS143  ','PSME  ', 855,'DF  ',  74,   1,   1,   3,
C---------------------------------------------------------------------
C   92 = TSHE/ARNE                            GBA: 147   p. 140
C    Western hemlock/pinemat manzanita                   PNW-GTR-359
C
     &'CHS144  ','PSME  ', 705,'DF  ',  52,   1,   1,   3,
C---------------------------------------------------------------------
C   93 = TSHE/ACCI/ACTR-WEN                   GBA: 183   p. 132
C    Western hemlock/vine maple/vanilla leaf (Wenatchee) PNW-GTR-359
C
     &'CHS225  ','PSME  ', 565,'DF  ',  87,   1,   1,   3,
C-------------------------------------------------------------------
C   94 = TSHE/ACCI/ASCA3                      GBA: 315   p. 134
C    Western hemlock/vine maple/wild ginger              PNW-GTR-359
C
     &'CHS226  ','PSME  ', 720,'DF  ',  86,   1,   1,   3,
C------------------------------------------------------------------
C   95 = TSHE/ACCI/CLUN                       GBA: 249   p. 136
C    Western hemlock/vine maple/queencup beadlily        PNW-GTR-359
C
     &'CHS227  ','ABGR  ', 630,'GF  ',  86,   1,   1,   6,
C----------------------------------------------------------------
C   96 = TSHE/RUPE                            GBA: 409   p. 221
C    Western hemlock/five-leaved bramble                 PNW-GTR-360
C
     &'CHS411  ','PIEN  ',1129,'ES  ', 103,   1,   1,   8,
C--------------------------------------------------------------------
C   97 = TSHE/MEFE                            GBA: 310   p. 215
C    Western hemlock/rusty menziesia                     PNW-GTR-360
C
     &'CHS711  ','PSME  ', 765,'DF  ',  71,   1,   1,   3,
C---------------------------------------------------------------------
C   98 = PICO/SHCA                            GBA: 162   p. 267
C    Lodgepole pine/russet buffaloberry                  PNW-GTR-360
C
     &'CLS521  ','PICO  ', 530,'LP  ',  96,   1,   1,   7,
C--------------------------------------------------------------------
C   99 = TSME/XETE-VAMY                       GBA: 245   p. 202
C    Mountain hemlock/beargrass-low huckleberry          PNW-GTR-359
C
     &'CMF131  ','TSME  ', 775,'OT  ',  23,   1,   1,  11,
C--------------------------------------------------------------------
C  100 = TSME/LUHI                            GBA: 197   p. 184
C    Mountain hemlock/smooth woodrush                    PNW-GTR-359
C
     &'CMG221  ','TSME  ', 544,'OT  ',  24,   1,   1,  11,
C--------------------------------------------------------------------
C  101 = TSME/VASC/LUHI                       GBA: 530   p. 200
C    Mountain hemlock/grouse huckleberry/smooth woodrush PNW-GTR-359
C
     &'CMS121  ','TSME  ', 650,'OT  ',  23,   1,   1,  11,
C---------------------------------------------------------------------
C  102 = TSME/RULA                            GBA: 257   p. 194
C    Mountain hemlock/dwarf bramble                      PNW-GTR-359
C
     &'CMS122  ','ABAM  ', 940,'SF  ',  79,   1,   1,   4,
C-----------------------------------------------------------------------
C  103 = TSME/MEFE-VAAL                       GBA: 269   p. 186
C    Mountain hemlock/rusty menziesia-Alaska huckleberry PNW-GTR-359
C
     &'CMS256  ','ABAM  ', 742,'SF  ',  94,   1,   1,   4,
C-----------------------------------------------------------------------
C  104 = TSME/MEFE-VAME                       GBA: 302   p. 188
C    Mountain hemlock/rusty menziesia-big huckleberry    PNW-GTR-359
C
     &'CMS257  ','ABAM  ',1115,'SF  ', 102,   1,   1,   4,
C----------------------------------------------------------------------
C  105 = TSME/VAAL-WEN                        GBA: 410   p. 196
C    Mountain hemlock/Alaska huckleberry (Wenatchee)     PNW-GTR-359
C
     &'CMS258  ','TSME  ',1132,'OT  ',  28,   1,   1,  11/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=106,120) /
C-----------------------------------------------------------------------
C  106 = TSME/VAME-WEN                        GBA: 226   p. 198
C    Mountain hemlock/big huckleberry (Wenatchee)        PNW-GTR-359
C
     &'CMS259  ','TSME  ', 885,'OT  ',  20,   1,   1,  11,
C-----------------------------------------------------------------------
C  107 = TSME/PHEM-VADE                       GBA: 161   p. 190
C    Mtn hemlock/red mountain heath-Cascade huckleberry  PNW-GTR-359
C
     &'CMS354  ','ABLA2 ', 780,'AF  ',  53,   1,   1,   9,
C---------------------------------------------------------------------
C  108 = TSME/RHAL-VAAL                       GBA: 196   p. 204
C    Mountain hemlock/Cascade azalea-Alaska huckleberry  PNW-GTR-359
C
     &'CMS355  ','TSME  ', 541,'OT  ',  26,   1,   1,  11,
C----------------------------------------------------------------------
C  109 = TSME/RHAL-VAME                       GBA: 242   p. 192
C    Mountain hemlock/Cascades azalea-big huckleberry    PNW-GTR-359
C
     &'CMS356  ','TSME  ', 935,'OT  ',  20,   1,   1,  11,
C---------------------------------------------------------------------
C  110 = PIPO/AGSP-WEN                        GBA:  67   p. 42
C    Ponderosa pine/bluebunch wheatgrass (Wenatchee)     PNW-GTR-359
C
     &'CPG141  ','PIPO  ', 200,'PP  ',  81,   1,   1,  10,
C---------------------------------------------------------------------
C  111 = PIPO/CARU-AGSP                       GBA:  65   p. 44
C    Ponderosa pine/pinegrass-bluebunch wheatgrass       PNW-GTR-359
C
     &'CPG231  ','PIPO  ', 420,'PP  ',  49,   1,   1,  10,
C---------------------------------------------------------------------
C  112 = PIPO-QUGA/BASA                       GBA: 119   p. 43
C    Ponderosa pine-Or white oak/arrowleaf balsamroot    R6 E TP-004-88
C
     &'CPH211  ','PIPO  ', 328,'PP  ',  65,   1,   1,  10,
C--------------------------------------------------------------------
C  113 = PIPO-QUGA/PUTR                       GBA: 124   p. 47
C    Ponderosa pine-Oregon white oak/bitterbrush         R6 E TP-004-88
C
     &'CPH212  ','PIPO  ', 342,'PP  ',  63,   1,   1,  10,
C---------------------------------------------------------------------
C  114 = PIPO/PUTR/AGSP                       GBA:  86   p. 46
C    Ponderosa pine/bitterbursh/bluebunch wheatgrass     PNW-GTR-359
C
     &'CPS241  ','PIPO  ', 210,'PP  ',  75,   1,   1,  10,
C----------------------------------------------------------------------
C  115 = ABGR-PIEN/SMST                       GBA: 352   p. 107
C    Grand fir-Engelmann spruce/starry solomonseal       R6 E TP-004-88
C
     &'CWC511  ','ABGR  ', 972,'GF  ',  90,   1,   1,   6,
C---------------------------------------------------------------------
C  116 = ABGR/LIBO2                           GBA: 257   p. 87
C    Grand fir/twinflower                                R6 E TP-004-88
C
     &'CWF321  ','ABGR  ', 709,'GF  ',  83,   1,   1,   6,
C-------------------------------------------------------------------
C  117 = ABGR/ARCO                            GBA: 272   p. 102
C    Grand fir/heartleaf arnica                          PNW-GTR-359
C
     &'CWF444  ','ABGR  ', 785,'GF  ',  72,   1,   1,   6,
C---------------------------------------------------------------------
C  118 = ABGR/TRLA2                           GBA: 337   p. 83
C    Grand fir/starflower                                R6 E TP-004-88
C
     &'CWF521  ','ABGR  ', 810,'GF  ',  91,   1,   1,   6,
C---------------------------------------------------------------------
C  119 = ABGR/ACTR                            GBA: 298   p. 95
C    Grand fir/vanillaleaf                               R6 E TP-004-88
C
     &'CWF522  ','ABGR  ', 710,'GF  ', 100,   1,   1,   6,
C------------------------------------------------------------------
C  120 = ABGR/POPU                            GBA: 346   p. 103
C    Grand fir/skunk-leaved polemonium                   R6 E TP-004-88
C
     &'CWF523  ','ABGR  ', 955,'GF  ',  90,   1,   1,   6/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=121,135) /
C-------------------------------------------------------------------
C  121 = ABGR/ACTR-WEN                        GBA: 349   p. 100
C   Grand fir/vanilla leaf (Wenatchee)                   PNW-GTR-359
C
     &'CWF524  ','ABGR  ', 963,'GF  ',  86,   1,   1,   6,
C------------------------------------------------------------------
C  122 = ABGR/CAGE                            GBA: 258   p. 71
C    Grand fir/elk sedge                                 R6 E TP-004-88
C
     &'CWG121  ','ABGR  ', 712,'GF  ', 104,   1,   1,   6,
C-------------------------------------------------------------------
C  123 = ABGR/CAGE-GP                         GBA: 509   p. 53
C    Grand fir/elk sedge (Gifford Pinchot)               R6 E TP-006-88
C
     &'CWG122  ','ABGR  ', 810,'GF  ', 100,   1,   1,   6,
C-------------------------------------------------------------------
C  124 = ABGR/CARU                            GBA: 641   p. 49
C    Grand fir/pinegrass                                 R6 E TP-006-88
C
     &'CWG123  ','ABGR  ',1769,'GF  ', 112,   1,   1,   6,
C-------------------------------------------------------------------
C  125 = ABGR/CARU-WEN                        GBA: 175   p. 110
C    Grand fir/pinegrass (Wenatchee)                     PNW-GTR-359
C
     &'CWG124  ','ABGR  ', 635,'GF  ',  85,   1,   1,   6,
C-------------------------------------------------------------------
C  126 = ABGR/CARU-LUPIN                      GBA: 214   p. 112
C    Grand fir/pinegrass-lupine                          PNW-GTR-359
C
     &'CWG125  ','PSME  ', 750,'DF  ',  58,   1,   1,   3,
C-------------------------------------------------------------------
C  127 = ABGR/VAME/CLUN-COL                   GBA: 361   p. 110
C    Grand fir/big huckleberry/queencup beadlily (Colv)  PNW-GTR-360
C
     &'CWS214  ','ABGR  ', 996,'GF  ',  86,   1,   1,   6,
C--------------------------------------------------------------------
C  128 = ABGR/VAME/LIBO2                      GBA: 281   p. 85
C    Grand fir/big huckleberry/twinflower                R6 E TP-006-88
C
     &'CWS221  ','ABGR  ', 776,'GF  ', 100,   1,   1,   6,
C--------------------------------------------------------------------
C  129 = ABGR/VAME/CLUN                       GBA: 344   p. 89
C    Grand fir/big huckleberry/queencup beadlily         R6 E TP-006-88
C
     &'CWS222  ','ABGR  ', 745,'GF  ', 103,   1,   1,   6,
C----------------------------------------------------------------------
C  130 = ABGR/RUPA/DIHO                       GBA: 332   p. 81
C    Grand fir/thimbleberry/fairy bells                  R6 E TP-006-88
C
     &'CWS223  ','ABGR  ', 455,'GF  ', 108,   1,   1,   6,
C---------------------------------------------------------------------
C  131 = ABGR/BENE/ACTR                       GBA: 264   p. 73
C    Grand fir/dwarf Oregon grape/vanillaleaf            R6 E TP-006-88
C
     &'CWS224  ','PSME  ', 650,'DF  ',  69,   1,   1,   3,
C---------------------------------------------------------------------
C  132 = ABGR/BENE                            GBA: 249   p. 106
C    Grand fir/Cascade Oregon grape                      PNW-GTR-359
C
     &'CWS225  ','ABGR  ', 845,'GF  ',  77,   1,   1,   6,
C---------------------------------------------------------------------
C  133 = ABGR/BENE/CARU-WEN                   GBA: 242   p. 108
C    Grand fir/Cascade Oregon grape/pinegrass-Wenatchee  PNW-GTR-359
C
     &'CWS226  ','ABGR  ', 745,'GF  ',  85,   1,   1,   6,
C-----------------------------------------------------------------------
C  134 = ABGR/SYMPH                           GBA: 279   p. 79
C    Grand fir/snowberry                                 R6 E TP-004-88
C
     &'CWS331  ','ABGR  ', 695,'GF  ',  90,   1,   1,   6,
C----------------------------------------------------------------------
C  135 = ABGR/SYMO/ACTR                       GBA: 392   p. 65
C    Grand fir/creeping snowberry/vanillaleaf            R6 E TP-006-88
C
     &'CWS332  ','ABGR  ', 870,'GF  ', 108,   1,   1,   6/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=136,150) /
C----------------------------------------------------------------------
C  136 = ABGR/SPEBL/PTAQ                      GBA: 215   p. 116
C    Grand fir/shiny-leaf spirea/bracken fern            PNW-GTR-359
C
     &'CWS335  ','ABGR  ', 655,'GF  ',  74,   1,   1,   6,
C---------------------------------------------------------------------
C  137 = ABGR/SYAL/CARU                       GBA: 260   p. 118
C    Grand fir/common snowberry/pinegrass                PNW-GTR-359
C
     &'CWS336  ','ABGR  ', 580,'GF  ',  76,   1,   1,   6,
C---------------------------------------------------------------------
C  138 = ABGR/SYOR                            GBA: 233   p. 120
C    Grand fir/mountain snowberry                        PNW-GTR-359
C
     &'CWS337  ','PSME  ', 360,'DF  ',  70,   1,   1,   3,
C---------------------------------------------------------------------
C  139 = ABGR/ARNE                            GBA: 156   p. 104
C    Grand fir/pinemat manzanita                         PNW-GTR-359
C
     &'CWS338  ','PSME  ', 575,'DF  ',  49,   1,   1,   3,
C---------------------------------------------------------------------
C  140 = ABGR/PHMA                            GBA: 240   p. 100
C    Grand fir/ninebark                                  PNW-GTR-360
C
     &'CWS421  ','PSME  ', 575,'DF  ',  79,   1,   1,   3,
C--------------------------------------------------------------------
C  141 = ABGR/ACGLD/CLUN                      GBA: 456   p. 95
C    Grand fir/Douglas maple/queencup beadlilly          PNW-GTR-360
C
     &'CWS422  ','ABGR  ',1259,'GF  ',  73,   1,   1,   6,
C--------------------------------------------------------------------
C  142 = ABGR/HODI                            GBA: 405   p. 75
C    Grand fir/oceanspray                                R6 E TP-004-88
C
     &'CWS531  ','ABGR  ', 860,'GF  ',  95,   1,   1,   6,
C---------------------------------------------------------------------
C  143 = ABGR/ACCI/ACTR                       GBA: 264   p. 91
C    Grand fir/vine maple/vanillaleaf                    R6 E TP-004-88
C
     &'CWS532  ','ABGR  ', 780,'GF  ',  98,   1,   1,   6,
C--------------------------------------------------------------------
C  144 = ABGR/CACH                            GBA: 214   p. 99
C    Grand fir/chinquapin                                R6 E TP-004-88
C
     &'CWS533  ','PSME  ', 690,'DF  ',  57,   1,   1,   3,
C-------------------------------------------------------------------
C  145 = ABGR/HODI-GP                         GBA: 358   p. 61
C    Grand fir/oceanspray (Gifford Pinchot)              R6 E TP-006-88
C
     &'CWS534  ','ABGR  ', 585,'GF  ', 104,   1,   1,   6,
C------------------------------------------------------------------
C  146 = ABGR/ACCI-BEAQ/TRLA2                 GBA: 461   p. 57
C    Grand fir/vine maple-tall Oregongrape/starflower    R6 E TP-006-88
C
     &'CWS535  ','ABGR  ', 520,'GF  ', 116,   1,   1,   6,
C------------------------------------------------------------------
C  147 = ABGR/COCO2/ACTR                      GBA: 499   p. 69
C    Grand fir/California hazel/vanillaleaf              R6 E TP-006-88
C
     &'CWS536  ','ABGR  ',1377,'GF  ', 116,   1,   1,   6,
C-------------------------------------------------------------------
C  148 = ABGR/CONU/ACTR                       GBA: 329   p. 77
C    Grand fir/pacific dogwood/vanillaleaf               R6 E TP-006-88
C
     &'CWS537  ','PSME  ', 650,'DF  ',  64,   1,   1,   3,
C-------------------------------------------------------------------
C  149 = ABGR/ACCI-WEN                        GBA: 511   p. 94
C    Grand fir/vine maple (Wenatchee)                    PNW-GTR-359
C
     &'CWS551  ','ABGR  ', 740,'GF  ', 109,   1,   1,   6,
C--------------------------------------------------------------------
C  150 = ABGR/ACCI-CHUM                       GBA: 393   p. 96
C    Grand fir/vine maple-western prince's pine          PNW-GTR-359
C
     &'CWS552  ','ABGR  ', 695,'GF  ', 100,   1,   1,   6/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=151,155) /
C--------------------------------------------------------------------
C  151 = ABGR/ACCI/CLUN                       GBA: 395   p. 98
C    Grand fir/vine maple/queencup beadlily              PNW-GTR-359
C
     &'CWS553  ','ABGR  ',1090,'GF  ', 104,   1,   1,   6,
C--------------------------------------------------------------------
C  152 = ABGR/HODI/CARU                       GBA: 258   p. 114
C    Grand fir/ocean-spray/pinegrass                     PNW-GTR-359
C
     &'CWS554  ','PSME  ', 545,'DF  ',  70,   1,   1,   3,
C--------------------------------------------------------------------
C  153 = ABGR/VACA                            GBA: 205   p. 105
C    Grand fir/dwarf huckleberry                         PNW-GTR-360
C
     &'CWS821  ','PSME  ', 560,'DF  ',  74,   1,   1,   3,
C--------------------------------------------------------------------
C  154 = POTR/CARU                            GBA: 189   p. 75
C    Quaking aspen/pinegrass                             R6 E 132b-83
C
     &'HQG111  ','PICO  ', 522,'LP  ',  84,   1,   1,   7,
C--------------------------------------------------------------------
C  155 = POTR/SYAL                            GBA: 120   p. 75
C    Quaking aspen/common snowberry                      R6 E 132b-83
C
     &'HQS211  ','LAOC  ', 331,'WL  ',  68,   1,   1 ,  2/
C-----------------------------------------------------------------------
C  IF INDEX IS POSITIVE, THERE ARE MULTIPLE SPECIES FOR THE PLANT
C  ASSOCIATION, SO JUST RETURN THOSE VALUES.
C----------
      IF(INDEX .GT. 0) THEN
        ASPEC  = SPC(INDEX)
        RSDI   = SDIMX(INDEX)
        RSI    = SITE(INDEX)
        ISFLAG = IFLAG(INDEX)
        ISEQ   = FVSSEQ(INDEX)
C----------
C  FIRST OCCURANCE FOR THIS PLANT ASSOCIATION. GO THROUGH LIST, LOCATE
C  THE PLANT ASSOCIATION, AND RETURN THE APPROPRIATE VALUES.
C----------
      ELSE
        DO 10 K=1,NENTRY
        IF(APASS .EQ. PA(K)) THEN
          ASPEC  = SPC(K)
          RSDI   = SDIMX(K)
          RSI    = SITE(K)
          ISFLAG = IFLAG(K)
          ISEQ   = FVSSEQ(K)
          NUM    = NUMBR(K)
          INDEX  = K
          GO TO 20
        ENDIF
   10   CONTINUE
C----------
C  PLANT ASSOCIATION WAS NOT FOUND.
C----------
        ASPEC  = '    '
        RSDI   = 0
        RSI    = 0
        ISFLAG = 0
        ISEQ   = 0
        NUM    = 0
        INDEX  = 0
      ENDIF
C----------
C  RETURN TO CALLING PROGRAM.
C----------
   20 CONTINUE
      RETURN
      END
