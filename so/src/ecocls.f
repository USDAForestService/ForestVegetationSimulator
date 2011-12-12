      SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
      IMPLICIT NONE
C----------
C  **ECOCLS---SO  DATE OF LAST REVISION:  05/18/09
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
C  AN R6 ALL CVS DATA STUDY, JUNE 2008: 1-3,9-21,25,26,31-40,43-62,
C  64-71,73-75,78-80,82-89
C----------
      INTEGER ISEQ,INDEX,NUM,ISFLAG,NENTRY,I,K
      REAL RSI,RSDI
      PARAMETER (NENTRY=92)
      CHARACTER*4 SPC(NENTRY),ASPEC
      CHARACTER*6 SCIEN(NENTRY)
      CHARACTER*8 APASS,PA(NENTRY)
      INTEGER FVSSEQ(NENTRY)
      REAL SITE(NENTRY),SDIMX(NENTRY)
      INTEGER NUMBR(NENTRY),IFLAG(NENTRY)
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=1,15) /
C-----------------------------------------------------------------------
C      ALPHA     SCIEN         ALPHA        NUM  SITE FVS  PLANT
C       ECO       SITE     MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
C      CLASS      SPEC     SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
C-----------------------------------------------------------------------
C
C    1 = CDS612 = PSME-ABCO/SYAL/LIBO         GBA: 258   p. 78
C    Mixed conifer/snowberry/twinflower                  R6 E 104-85
C
     &'CDS612  ','PSME  ', 755,'DF  ',  85,   1,   1,   3,
C-----------------------------------------------------------------------
C    2 = CDS613 = PSME-ABCO/SYAL/FORB         GBA: 359   p. 77
C    Mixed conifer/snowberry/forb                        R6 E 104-85
C
     &'CDS613  ','ABCO  ', 810,'WF  ',  90,   1,   1,   4,
C-----------------------------------------------------------------------
C    3 = CDS614 = PSME-ABCO/SYAL/CARU         GBA: 161   p. 76
C    Mixed conifer/snowberry/pinegrass                   R6 E 104-85
C
     &'CDS614  ','PSME  ', 615,'DF  ',  78,   1,   1,   3,
C-----------------------------------------------------------------------
C    4 = CEM111 = PIEN/CAEU                   GBA: 230   p. 55
C    Engelmann spruce/widefruit sedge                    R6 E TP-279-87
C
     &'CEM111  ','PIEN  ', 635,'ES  ',  80,   1,   1,   8,
C-----------------------------------------------------------------------
C    5 = CEM221 = PIEN/EQAR-STRO              GBA: 258   p. 57
C    Engelmann spruce/common horsetail-twistedstalk      R6 E TP-279-87
C
     &'CEM221  ','PIEN  ', 712,'ES  ',  90,   1,   1,   8,
C-----------------------------------------------------------------------
C    6 = CEM222 = PIEN/CLUN                   GBA: 305   p. 49
C    Engelmann spruce/queencup beadlily                  R6 E TP-279-87
C
     &'CEM222  ','PIEN  ', 842,'ES  ', 105,   1,   1,   8,
C-----------------------------------------------------------------------
C    7 = CEM311 = PIEN/VAOC2-FORB             GBA: 233   p. 51
C    Engelmann spruce/bog blueberry/forb                 R6 E TP-279-87
C
     &'CEM311  ','PIEN  ', 643,'ES  ',  85,   1,   1,   8,
C-----------------------------------------------------------------------
C    8 = CEM312 = PIEN/VAOC2/CAEU             GBA: 161   p. 53
C    Engelmann spruce/bog blueberry/widefruit sedge      R6 E TP-279-87
C
     &'CEM312  ','PIEN  ', 444,'ES  ',  76,   1,   1,   8,
C-----------------------------------------------------------------------
C    9 = CLC111 = PICO-PIAL/PELA              GBA:  99   p. 19
C    Lodgepole pine-Whitebark pine/Gay penstemon         R6 E 79-004
C
     &'CLC111  ','PICO  ', 625,'LP  ',  30,   1,   1,   7,
C-----------------------------------------------------------------------
C   10 = CLC112 = PICO-PIAL/ARCO2             GBA:  84   p. 20
C    Lodgepole pine-Whitebark pine-W white pine/sandwort R6 E 79-004
C
     &'CLC112  ','PICO  ', 690,'LP  ',  25,   1,   1,   7,
C-----------------------------------------------------------------------
C   11 = CLF111 = PICO/FORB                   GBA:  94   p. 11
C    Lodgepole pine/forb                                 R6 E 79-005
C
     &'CLF111  ','PICO  ', 365,'LP  ',  43,   1,   1,   7,
C-----------------------------------------------------------------------
C   12 = CLG311 = PICO/STOC-BASIN             GBA:  68   p. 42
C    Lodgepole pine/needlegrass basins                   R6 E 104-85
C
     &'CLG311  ','PICO  ', 480,'LP  ',  38,   1,   1,   7,
C-----------------------------------------------------------------------
C   13 = CLG313 = PICO/STOC-LUCA-LINU         GBA: 126   p. 49
C    Lodgepole pine/needlegrass-lupine-linanthastrum     R6 E 104-85
C
     &'CLG313  ','PICO  ', 395,'LP  ',  45,   1,   1,   7,
C-----------------------------------------------------------------------
C   14 = CLG314 = PICO/STOC-LUCA              GBA: 106   p. 48
C    Lodgepole pine/needlegrass-lupine                   R6 E 104-85
C
     &'CLG314  ','PICO  ', 660,'LP  ',  52,   1,   1,   7,
C-----------------------------------------------------------------------
C   15 = CLG315 = PICO/FRVI-FEID              GBA: 135   p. 21
C    Lodgepole pine/strawberry-fescue                    R6 E 79-004
C
     &'CLG315  ','PICO  ', 510,'LP  ',  44,   1,   1,   7/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=16,30) /
C-----------------------------------------------------------------------
C   16 = CLG411 = PICO/CAPE-LUCA              GBA: 183   p. 46
C    Lodgepole pine/sedge-lupine                         R6 E 104-85
C
     &'CLG411  ','PICO  ', 680,'LP  ',  49,   1,   1,   7,
C-----------------------------------------------------------------------
C   17 = CLG412 = PICO/CAPE-LUCA-PEEU         GBA: 206   p. 47
C    Lodgepole pine/sedge-lupine-penstemon               R6 E 104-85
C
     &'CLG412  ','PICO  ', 635,'LP  ',  50,   1,   1,   7,
C-----------------------------------------------------------------------
C   18 = CLG413 = PICO/CAPE-STOC-BASIN        GBA:  49   p. 43
C    Lodgepole pine/sedge-needlegrass basins             R6 E 104-85
C
     &'CLG413  ','PICO  ', 590,'LP  ',  37,   1,   1,   7,
C-----------------------------------------------------------------------
C   19 = CLG415 = PICO/SIHY-CAPE              GBA:  79   p. 22
C    Lodgepole pine/squirreltail-long-stolon sedge       R6 E 79-004
C
     &'CLG415  ','PICO  ', 540,'LP  ',  40,   1,   1,   7,
C-----------------------------------------------------------------------
C   20 = CLH111 = PICO/POTR/FRVI              GBA: 180   p. 23
C    Lodgepole pine/quaking aspen/strawberry             R6 E 79-004
C
     &'CLH111  ','PICO  ', 345,'LP  ',  48,   1,   1,   7,
C-----------------------------------------------------------------------
C   21 = CLM111 = PICO/CANE-ELGL-WET          GBA: 168   p. 32
C    Lodgepole pine/sedge-grass wetland                  R6 E 104-85
C
     &'CLM111  ','PICO  ', 540,'LP  ',  51,   1,   1,   7,
C-----------------------------------------------------------------------
C   22 = CLM112 = PICO/POPR                   GBA: 195   p. 29
C    Lodgepole pine/Kentucky bluegrass                   R6 E TP-279-87
C
     &'CLM112  ','PICO  ', 538,'LP  ',  55,   1,   1,   7,
C-----------------------------------------------------------------------
C   23 = CLM113 = PICO/CAEU                   GBA: 178   p. 41
C    Lodgepole pine/widefruit sedge                      R6 E TP-279-87
C
     &'CLM113  ','PICO  ', 491,'LP  ',  57,   1,   1,   7,
C-----------------------------------------------------------------------
C   24 = CLM114 = PICO/CAAQ                   GBA: 199   p. 43
C    Lodgepole pine/aquatic sedge                        R6 E TP-279-87
C
     &'CLM114  ','PICO  ', 549,'LP  ',  45,   1,   1,   7,
C-----------------------------------------------------------------------
C   25 = CLM211 = PICO/ARUV                   GBA: 114   p. 31
C    Lodgepole pine/bearberry                            R6 E TP-279-87
C
     &'CLM211  ','PICO  ', 585,'LP  ',  48,   1,   1,   7,
C-----------------------------------------------------------------------
C   26 = CLM311 = PICO/VAOC2/FORB             GBA: 151   p. 37 p. 33
C    Lodgepole pine/blueberry/forb                       R6 E TP-279-87
C                                                        R6 E 104-85
     &'CLM311  ','PICO  ', 570,'LP  ',  47,   1,   1,   7,
C-----------------------------------------------------------------------
C   27 = CLM312 = PICO/VAOC2/CAEU             GBA: 169   p. 39
C    Lodgepole pine/bog blueberry/widefruit sedge        R6 E TP-279-87
C
     &'CLM312  ','PICO  ', 466,'LP  ',  54,   1,   1,   7,
C-----------------------------------------------------------------------
C   28 = CLM313 = PICO/SPDO/FORB              GBA: 202   p. 33
C    Lodgepole pine/Douglas spiraea/forb                 R6 E TP-279-87
C
     &'CLM313  ','PICO  ', 558,'LP  ',  51,   1,   1,   7,
C-----------------------------------------------------------------------
C   29 = CLM314 = PICO/SPDO/CAEU              GBA: 188   p. 35
C    Lodgepole pine/Douglas spiraea/widefruit sedge      R6 E TP-279-87
C
     &'CLM314  ','PICO  ', 519,'LP  ',  59,   1,   1,   7,
C-----------------------------------------------------------------------
C   30 = CLM411 = PICO/XETE                   GBA: 194   p. 52
C    Lodgepole pine/beargrass                            R6 E 104-85
C
     &'CLM411  ','PICO  ', 535,'LP  ',  56,   1,   1,   7/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=31,45) /
C-----------------------------------------------------------------------
C   31 = CLM911 = PICO/PIEN/ELPA2             GBA:  76   p. 45
C    Lodgepole pine-Engel spruce/few-flowered spikerush  R6 E TP-279-87
C
     &'CLM911  ','PICO  ', 495,'LP  ',  35,   1,   1,   7,
C-----------------------------------------------------------------------
C   32 = CLS112 = PICO/ARTR-RHYO              GBA:  83   p. 36
C    Lodgepole pine/sagebrush (rhyolite)                 R6 E 104-85
C
     &'CLS112  ','PICO  ', 180,'LP  ',  41,   1,   1,   7,
C-----------------------------------------------------------------------
C   33 = CLS211 = PICO/PUTR/STOC              GBA:  97   p. 40
C    Lodgepole pine/bitterbrush/needlegrass              R6 E 104-85
C
     &'CLS211  ','PICO  ', 405,'LP  ',  46,   1,   1,   7,
C-----------------------------------------------------------------------
C   34 = CLS212 = PICO/PUTR/CAPE              GBA: 165   p. 44
C    Lodgepole pine/bitterbrush/sedge                    R6 E 104-85
C
     &'CLS212  ','PICO  ', 405,'LP  ',  52,   1,   1,   7,
C-----------------------------------------------------------------------
C   35 = CLS213 = PICO/PUTR/FORB              GBA: 105   p. 35
C    Lodgepole pine/bitterbrush/forb                     R6 E 104-85
C
     &'CLS213  ','PICO  ', 400,'LP  ',  43,   1,   1,   7,
C-----------------------------------------------------------------------
C   36 = CLS214 = PICO/PUTR/FEID              GBA: 128   p. 39
C    Lodgepole pine/bitterbrush/fescue                   R6 E 104-85
C
     &'CLS214  ','PICO  ', 400,'LP  ',  45,   1,   1,   7,
C-----------------------------------------------------------------------
C   37 = CLS215 = PICO/RICE-PUTR/STOC         GBA:  92   p. 41
C    Lodgepole pine/current-bitterbrush/needlegrass      R6 E 104-85
C
     &'CLS215  ','PICO  ', 370,'LP  ',  41,   1,   1,   7,
C-----------------------------------------------------------------------
C   38 = CLS216 = PICO/PUTR-RHYO              GBA: 111   p. 38
C    Lodgepole pine/bitterbrush (rhyolite pumice)        R6 E 104-85
C
     &'CLS216  ','PICO  ', 345,'LP  ',  36,   1,   1,   7,
C-----------------------------------------------------------------------
C   39 = CLS311 = PICO/ARNE                   GBA:  55   p. 50
C    Lodgepole pine/pinemat manzanita                    R6 E 104-85
C
     &'CLS311  ','PICO  ', 575,'LP  ',  31,   1,   1,   7,
C-----------------------------------------------------------------------
C   40 = CLS412 = PICO/VASC                   GBA: 126   p. 51
C    Lodgepole pine/grouse huckleberry                   R6 E 104-85
C
     &'CLS412  ','PICO  ', 865,'LP  ',  45,   1,   1,   7,
C-----------------------------------------------------------------------
C   41 = CLS413 = PICO/VASC-FORB              GBA: 161   p. 12
C    Lodgepole pine/grouse huckleberry/forb              R6 E 79-005
C
     &'CLS413  ','PICO  ', 444,'LP  ',  55,   1,   1,   7,
C-----------------------------------------------------------------------
C   42 = CLS414 = PICO/VASC/CAPE              GBA: 105   p. 13
C    Lodgepole pine/grouse huckleberry/long-stolon sedge R6 E 79-005
C
     &'CLS414  ','PICO  ', 290,'LP  ',  43,   1,   1,   7,
C-----------------------------------------------------------------------
C   43 = CLS911 = PICO/CEVE-ARPA              GBA: 109   p. 45
C    Lodgepole pine/snowbrush-manzanita                  R6 E 104-85
C
     &'CLS911  ','PICO  ', 575,'LP  ',  44,   1,   1,   7,
C-----------------------------------------------------------------------
C   44 = CMS111 = TSME/VASC-DES               GBA: 189   p. 24 p. 80
C    Mountain hemlock/grouse huckleberry, Deschutes      R6 E 79-005
C                                                        R6 E 104-85
     &'CMS111  ','TSME  ', 895,'MH  ',  30,   1,   1,   5,
C-----------------------------------------------------------------------
C   45 = CPC211 = PIPO-JUOC/CELE/FEID         GBA: 108   p. 24
C    Ponderosa-juniper/mahogany-bitterb-big sage/fescue  R6 E 79-004
C
     &'CPC211  ','PIPO  ', 345,'PP  ',  82,   1,   1,  10/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=46,60) /
C-----------------------------------------------------------------------
C   46 = CPF111 = PIPO/WYMO                   GBA: 100   p. 27
C    Ponderosa pine/wooly wyethia                        R6 E 79-004
C
     &'CPF111  ','PIPO  ', 510,'PP  ',  84,   1,   1,  10,
C-----------------------------------------------------------------------
C   47 = CPG212 = PIPO/CAPE-FEID-LALA2        GBA: 189   p. 66
C    Ponderosa pine/sedge-fescue-peavine                 R6 E 104-85
C
     &'CPG212  ','PIPO  ', 575,'PP  ',  97,   1,   1,  10,
C-----------------------------------------------------------------------
C   48 = CPH311 = PIPO-POTR/PONE              GBA: 124   p. 28
C    Ponderosa pine/quaking aspen/bluegrass              R6 E 79-004
C
     &'CPH311  ','PIPO  ', 485,'PP  ',  84,   1,   1,  10,
C-----------------------------------------------------------------------
C   49 = CPS111 = PIPO/PUTR-ARTR/FEID         GBA:  91   p. 56
C    Ponderosa pine/bitterbrush sagebrush/fescue         R6 E 104-85
C
     &'CPS111  ','PIPO  ', 285,'PP  ',  70,   1,   1,  10,
C-----------------------------------------------------------------------
C   50 = CPS112 = PIPO/PUTR-ARTR/SIHY         GBA:  55   p. 55
C    Ponderosa pine/bitterbrush-sage/squirreltail (Rhyo) R6 E 104-85
C
     &'CPS112  ','PIPO  ', 335,'PP  ',  75,   1,   1,  10,
C-----------------------------------------------------------------------
C   51 = CPS121 = PIPO/ARTR/PONE              GBA:  99   p. 29
C    Ponderosa pine/mtn big sagebrush/bluegrass          R6 E 79-004
C
     &'CPS121  ','PIPO  ', 450,'PP  ',  82,   1,   1,  10,
C-----------------------------------------------------------------------
C   52 = CPS211 = PIPO/PUTR/FEID              GBA: 122   p. 57
C    Ponderosa pine/bitterbrush/fescue                   R6 E 104-85
C
     &'CPS211  ','PIPO  ', 460,'PP  ',  81,   1,   1,  10,
C-----------------------------------------------------------------------
C   53 = CPS212 = PIPO/PUTR/STOC              GBA: 108   p. 60b
C    Ponderosa pine/bitterbrush/needlegrass              R6 E 104-85
C
     &'CPS212  ','PIPO  ', 440,'PP  ',  85,   1,   1,  10,
C-----------------------------------------------------------------------
C   54 = CPS213 = PIPO/PUTR-ARPA/STOC         GBA:  95   p. 61
C    Ponderosa pine/bitterbrush-manzanita/needlegrass    R6 E 104-85
C
     &'CPS213  ','PIPO  ', 345,'PP  ',  81,   1,   1,  10,
C-----------------------------------------------------------------------
C   55 = CPS214 = PIPO/PUTR-ARPA/CAPE         GBA:  65   p. 64
C    Ponderosa pine/bitterbrush-manzanita/sedge          R6 E 104-85
C
     &'CPS214  ','PIPO  ', 450,'PP  ',  88,   1,   1,  10,
C-----------------------------------------------------------------------
C   56 = CPS215 = PIPO/PUTR/CAPE              GBA: 100   p. 63
C    Ponderosa pine/bitterbrush/sedge                    R6 E 104-85
C
     &'CPS215  ','PIPO  ', 425,'PP  ',  89,   1,   1,  10,
C-----------------------------------------------------------------------
C   57 = CPS216 = PIPO/PUTR/FEID-AGSP         GBA:  85   p. 53
C    Ponderosa pine/bitterbrush/bunchgrass               R6 E 104-85
C
     &'CPS216  ','PIPO  ', 225,'PP  ',  77,   1,   1,  10,
C-----------------------------------------------------------------------
C   58 = CPS217 = PIPO/PUTR-ARPA/FEID         GBA: 123   p. 58
C    Ponderosa pine/bitterbrush-manzanita/fescue         R6 E 104-85
C
     &'CPS217  ','PIPO  ', 425,'PP  ',  76,   1,   1,  10,
C-----------------------------------------------------------------------
C   59 = CPS218 = PIPO/PUTR/SIHY-RHYO         GBA: 111   p. 54
C    Ponderosa pine/bitterbrush/squirreltail (rhyolite)  R6 E 104-85
C
     &'CPS218  ','PIPO  ', 385,'PP  ',  80,   1,   1,  10,
C-----------------------------------------------------------------------
C   60 = CPS311 = PIPO/PUTR-CEVE/STOC         GBA: 143   p. 62
C     Ponderosa pine/bitterbrush-snowbrush/needlegrass   R6 E 104-85
C
     &'CPS311  ','PIPO  ', 550,'PP  ',  85,   1,   1,  10/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=61,75) /
C-----------------------------------------------------------------------
C   61 = CPS312 = PIPO/PUTR-CEVE/CAPE         GBA:  97   p. 65
C    Ponderosa pine/bitterbrush-snowbrush/sedge          R6 E 104-85
C
     &'CPS312  ','PIPO  ', 345,'PP  ',  89,   1,   1,  10,
C-----------------------------------------------------------------------
C   62 = CPS314 = PIPO/PUTR-CEVE/FEID         GBA: 145   p. 59
C    Ponderosa pine/bitterbrush-snowbrush/fescue         R6 E 104-85
C
     &'CPS314  ','PIPO  ', 365,'PP  ',  90,   1,   1,  10,
C-----------------------------------------------------------------------
C   63 = CPS511 = PIPO/SYAL-FLOOD             GBA: 187   p. 27
C    Ponderosa pine/common snowberry-floodplain          R6 E TP-279-87
C
     &'CPS511  ','PIPO  ', 516,'PP  ', 101,   1,   1,  10,
C-----------------------------------------------------------------------
C   64 = CRG111 = ABMAS/CAPE                  GBA: 288   p. 22
C    Shasta red fir/long-stolon sedge                    R6 E 79-005
C
     &'CRG111  ','ABMAS ', 745,'RF  ', 120,   1,   1,   9,
C-----------------------------------------------------------------------
C   65 = CRS111 = ABMAS/ARNE                  GBA: 128   p. 72
C    Mixed conifer/manzanita                             R6 E 104-85
C
     &'CRS111  ','ABMAS ', 725,'RF  ',  79,   1,   1,   9,
C-----------------------------------------------------------------------
C   66 = CRS112 = ABMAS-TSME/ARNE/CAPE        GBA: 215   p. 23
C    Shasta red fir-Mtn hemlock/pinemat manzanita/sedge  R6 E 79-005
C
     &'CRS112  ','ABMAS ', 910,'RF  ',  99,   1,   1,   9,
C-----------------------------------------------------------------------
C   67 = CRS311 = ABMAS-ABCO/CACH-CHUM/CAPE   GBA: 277   p. 21
C    Shasta red fir-white fir/chink-prince's pine/sedge  R6 E 79-005
C
     &'CRS311  ','ABMAS ', 775,'RF  ', 114,   1,   1,   9,
C-----------------------------------------------------------------------
C   68 = CWC111 = ABCO-PIPO-CADE/AMAL         GBA: 265   p. 34
C    White fir-ponderosa pine-incense cedar/serviceberry R6 E 79-004
C
     &'CWC111  ','ABCO  ', 665,'WF  ',  58,   1,   1,   4,
C-----------------------------------------------------------------------
C   69 = CWC211 = ABCO/CEVE-CACH/PTAQ         GBA: 159   p. 75
C    Mixed conifer/snowbrush-chinkquapin/brackenfern     R6 E 104-85
C
     &'CWC211  ','PIPO  ', 675,'PP  ',  96,   1,   1,  10,
C-----------------------------------------------------------------------
C   70 = CWC212 = ABCO/CEVE-CACH/CARU         GBA: 175   p. 74
C    Mixed conifer/snowbrush-chinkquapin/pinegrass       R6 E 104-85
C
     &'CWC212  ','PIPO  ', 630,'PP  ',  95,   1,   1,  10,
C-----------------------------------------------------------------------
C   71 = CWC213 = ABCO/CEVE/CAPE-PTAQ         GBA: 146   p. 69
C    Mixed conifer/snowbrush/sedge-bracken               R6 E 104-85
C
     &'CWC213  ','PIPO  ', 645,'PP  ',  87,   1,   1,  10,
C-----------------------------------------------------------------------
C   72 = CWC215 = ABCO-PSME/CEVE-ARUV         GBA: 243   p. 17
C    Mixed conifer/snowbrush-bearberry                   R6 E 79-005
C
     &'CWC215  ','ABCO  ', 671,'WF  ',  78,   1,   1,   4,
C-----------------------------------------------------------------------
C   73 = CWC311 = ABCO-PICO/CAPE5-STOC        GBA: 207   p. 30
C    White fir-lodgepole pine/long-stolon sedge-needlegr R6 E 79-004
C
     &'CWC311  ','ABCO  ', 770,'WF  ',  54,   1,   1,   4,
C-----------------------------------------------------------------------
C   74 = CWC411 = ABCO-PIPO-PILA/RIVI         GBA: 226   p. 35
C    White fir-ponderosa pine-white pine/sticky currant  R6 E 79-004
C
     &'CWC411  ','ABCO  ', 910,'WF  ',  56,   1,   1,   4,
C-----------------------------------------------------------------------
C   75 = CWC412 = ABCO-PIPO-PILA/ARPA         GBA: 241   p. 33
C    White fir-ponderosa pine-sugar pine/manzanita       R6 E 79-004
C
     &'CWC412  ','ABCO  ', 510,'WF  ',  68,   1,   1,   4/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=76,90) /
C-----------------------------------------------------------------------
C   76 = CWC911 = PIEN-BOTTOMS                GBA: 247   p. 79
C    Engelmann spruce bottomlands                        R6 E 104-85
C
     &'CWC911  ','PIEN  ', 682,'ES  ',  86,   1,   1,   8,
C-----------------------------------------------------------------------
C   77 = CWF431 = ABCO/CLUN                   GBA: 316   p. 47
C    White fir/queencup beadlily                         R6 E TP-279-87
C
     &'CWF431  ','ABCO  ', 872,'WF  ',  81,   1,   1,   4,
C-----------------------------------------------------------------------
C   78 = CWH111 = ABCO/CEVE-CACH              GBA: 140   p. 73
C    Mixed conifer/snowberry-chinquapin                  R6 E 104-85
C
     &'CWH111  ','PIPO  ', 675,'PP  ',  91,   1,   1,  10,
C-----------------------------------------------------------------------
C   79 = CWH112 = ABCO/CACH-PAMY-CHUM         GBA: 237   p. 20
C    White fir/chinquapin-boxwood-prince's pine          R6 E 79-005
C
     &'CWH112  ','ABCO  ', 750,'WF  ',  84,   1,   1,   4,
C-----------------------------------------------------------------------
C   80 = CWH211 = ABCO-PIPO-POTR/CAPE         GBA: 136   p. 36
C    White fir-ponderosa pine-aspen/long-stolon sedge    R6 E 79-004
C
     &'CWH211  ','PIPO  ', 270,'PP  ',  84,   1,   1,  10,
C-----------------------------------------------------------------------
C   81 = CWM111 = ABCO/ALTE                   GBA: 220   p. 16
C    White fir-alder/shrub meadow                        R6 E 79-005
C
     &'CWM111  ','ABCO  ', 607,'WF  ',  81,   1,   1,   4,
C-----------------------------------------------------------------------
C   82 = CWS112 = ABCO/CEVE-ARPA              GBA: 137   p. 70
C    Mixed conifer/snowbrush-manzanita                   R6 E 104-85
C
     &'CWS112  ','PIPO  ', 660,'PP  ',  85,   1,   1,  10,
C-----------------------------------------------------------------------
C   83 = CWS113 = ABCO/CEVE-ARPA/CAPE-PEEU    GBA: 254   p. 71
C    M conifer/manzanita-snowbrush/sedge-penstemon       R6 E 104-85
C
     &'CWS113  ','PIPO  ', 660,'PP  ',  88,   1,   1,  10,
C-----------------------------------------------------------------------
C   84 = CWS114 = ABCO/CEVE                   GBA: 118   p. 67
C    Mixed conifer/snowbrush                             R6 E 104-85
C
     &'CWS114  ','PIPO  ', 725,'PP  ',  86,   1,   1,  10,
C-----------------------------------------------------------------------
C   85 = CWS115 = ABCO/CEVE/CAPE              GBA: 149   p. 68
C    Mixed conifer/snowbrush/sedge                       R6 E 104-85
C
     &'CWS115  ','PIPO  ', 790,'PP  ',  88,   1,   1,  10,
C-----------------------------------------------------------------------
C   86 = CWS116 = ABCO/CEVE-CEPR/FRVI         GBA:  81   p. 18
C    Mixed conifer/snowbrush-squawcarpet/strawberry      R6 E 79-005
C
     &'CWS116  ','ABCO  ', 560,'WF  ',  65,   1,   1,   4,
C-----------------------------------------------------------------------
C   87 = CWS117 = ABCO-PIPO/ARPA-BERE         GBA: 103   p. 32
C    White fir-ponderosa pine/manzanita-Oregon grape     R6 E 79-004
C
     &'CWS117  ','PIPO  ', 570,'PP  ',  86,   1,   1,  10,
C-----------------------------------------------------------------------
C   88 = CWS312 = ABCO/SYAL/FRVI              GBA: 128   p. 19
C    White fir/snowberry/strawberry                      R6 E 79-005
C
     &'CWS312  ','ABCO  ', 485,'WF  ',  69,   1,   1,   4,
C-----------------------------------------------------------------------
C   89 = CWS313 = ABCO-PIPO/SYAL/STJA         GBA: 240   p. 31
C    White fir-ponderosa pine/snowberry/starwort         R6 E 79-004
C
     &'CWS313  ','ABCO  ', 810,'WF  ',  63,   1,   1,   4,
C-----------------------------------------------------------------------
C   90 = HQM121 = POTR/ELGL                   GBA: 168   p. 61
C    Quaking aspen/blue wildrye                          R6 E TP-279-87
C
     &'HQM121  ','PICO  ', 464,'LP  ',  55,   1,   1,   7/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=91,92) /
C-----------------------------------------------------------------------
C   91 = HQM411 = POTR-PICO/SPDO/CAEU         GBA: 232   p. 63
C    Q aspen-lodgepole pine/Doug spiraea/widefruit sedge R6 E TP-279-87
C
     &'HQM411  ','PICO  ', 640,'LP  ',  59,   1,   1,   7,
C-----------------------------------------------------------------------
C   92 = HQS221 = POTR/SYAL/ELGL              GBA: 216   p. 59
C    Quaking aspen/common snowberry/blue wildrye         R6 E TP-279-87
C
     &'HQS221  ','PIPO  ', 596,'PP  ', 101,   1,   1,  10/
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
