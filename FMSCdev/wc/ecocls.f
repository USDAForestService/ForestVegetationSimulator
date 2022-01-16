      SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
      IMPLICIT NONE
C----------
C WC $Id$
C----------
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
C  AN R6 ALL CVS DATA STUDY, JUNE 2008: 8-14,16-21,23-39,41-45,49-52,
C  54,56-60,62-67,69-74,76-83,86-89,91-103,105,107-139
C----------
      INTEGER ISEQ,INDEX,NUM,ISFLAG,NENTRY,I,K
      PARAMETER (NENTRY=139)
      CHARACTER*4 SPC(NENTRY),ASPEC
      CHARACTER*6 SCIEN(NENTRY)
      CHARACTER*8 APASS,PA(NENTRY)
      INTEGER FVSSEQ(NENTRY)
      REAL SITE(NENTRY),SDIMX(NENTRY)
      INTEGER NUMBR(NENTRY),IFLAG(NENTRY)
      REAL RSI,RSDI
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=1,15) /
C-----------------------------------------------------------------------
C      ALPHA    SCIEN          ALPHA        NUM  SITE FVS  PLANT
C       ECO      SITE      MAX  SITE  SITE  IN   SPP  SEQ  ASSOCIATION
C      CLASS     SPEC      SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE
C-----------------------------------------------------------------------
C    1 = TSME-ABLA2/PONE4                     GBA: 253   p. 31
C    Mountain hemlock-subalpine fir/Newberry's knotweed  R6 TP-08-95
C
     &'CAF211  ','TSME  ', 698.,'MH  ',  14.,   1,   1,  20,
C-----------------------------------------------------------------------
C    2 = TSME-ABLA2/ASLE2                     GBA: 196   p. 19
C    Mountain hemlock-subalpine fir/Cascades aster       R6 TP-08-95
C
     &'CAF311  ','TSME  ', 541.,'MH  ',  15.,   1,   1,  20,
C----------------------------------------------------------------------
C    3 = TSME-ABLA2/FEVI                      GBA: 135   p. 23
C    Mountain hemlock-subalpine fir/green fescue         R6 TP-08-95
C
     &'CAG211  ','TSME  ', 373.,'MH  ',  12.,   1,   1,  20,
C----------------------------------------------------------------------
C    4 = TSME/LUHI                            GBA: 297   p. 35
C    Mountain hemlock/Hitchcock's woodrush               R6 TP-08-95
C
     &'CAG311  ','TSME  ', 820.,'MH  ',  17.,   1,   1,  20,
C----------------------------------------------------------------------
C    5 = TSME-PIAL/LUHI                       GBA: 257   p. 47
C    Mountain hemlock-whitebark pine/Hitchc woodrush     R6 TP-08-95
C
     &'CAG312  ','TSME  ', 709.,'MH  ',  13.,   1,   1,  20,
C-----------------------------------------------------------------------
C    6 = TSME/PHEM-VADE                       GBA: 269   p. 43
C    Mountain hemlock/red mtn heather-delicious huckleb  R6 TP-08-95
C
     &'CAS211  ','TSME  ', 742.,'MH  ',  16.,   1,   1,  20,
C-----------------------------------------------------------------------
C    7 = TSME-ABLA2/JUOC4                     GBA: 466   p. 27
C    Mountain hemlock-subalpine fir/mtn juniper          R6 TP-08-95
C
     &'CAS411  ','TSME  ',1286.,'MH  ',  12.,   1,   1,  20,
C-----------------------------------------------------------------------
C    8 = PSME-TSHE/BENE                       GBA: 400   p. 78
C    Douglas-fir-western hemlock/dwarf Oregon grape      R6 E 257-B-86
C
     &'CDC711  ','PSME  ', 810.,'DF  ', 145.,   1,   1,  16,
C-----------------------------------------------------------------------
C    9 = PSME-TSHE/RHMA                       GBA: 317   p. 82
C    Douglas-fir-western hemlock/rhododendron            R6 E 257-B-86
C
     &'CDC712  ','PSME  ', 785.,'DF  ', 133.,   1,   1,  16,
C-----------------------------------------------------------------------
C   10 = PSME-TSHE/GASH                       GBA: 404   p. 86
C    Douglas-fir-western hemlock/salal                   R6 E 257-B-86
C
     &'CDC713  ','PSME  ', 685.,'DF  ', 138.,   1,   1,  16,
C-----------------------------------------------------------------------
C   11 = PSME/HODI-BENE                       GBA: 311   p. 62
C    Douglas-fir/oceanspray-dwarf Oregon grape           R6 E 257-B-86
C
     &'CDS211  ','PSME  ', 770.,'DF  ', 115.,   1,   1,  16,
C-----------------------------------------------------------------------
C   12 = PSME/HODI/GRASS                      GBA: 312   p. 66
C    Douglas-fir/oceanspray/grass                        R6 E 257-B-86
C
     &'CDS212  ','PSME  ', 565.,'DF  ', 121.,   1,   1,  16,
C-----------------------------------------------------------------------
C   13 = PSME/HODI-WHMO                       GBA: 290   p. 70
C    Douglas-fir/oceanspray-whipple vine                 R6 E 257-B-86
C
     &'CDS213  ','PSME  ', 670.,'DF  ', 106.,   1,   1,  16,
C-----------------------------------------------------------------------
C   14 = PSME/SYMO-WIL                        GBA: 496   p. 74
C    Douglas-fir/snowberry (Willamette)                  R6 E 257-B-86
C
     &'CDS641  ','PSME  ', 740.,'DF  ', 123.,   1,   1,  16,
C-----------------------------------------------------------------------
C   15 = ABAM-TSHE/RHMA-GASH                  GBA: 276   p. 49
C    Pac silver fir-W. hemlock/rhododendron-salal        R6 E 100-82
C
     &'CFC251  ','PSME  ', 762.,'DF  ', 101.,   1,   1,  16/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=16,30) /
C-----------------------------------------------------------------------
C   16 = ABAM-ABGR/SMST                       GBA: 496   p. 98
C    Pac silver fir-grand fir/false solomonseal          R6 E 257-B-86
C
     &'CFC311  ','PSME  ', 935.,'DF  ', 133.,   1,   1,  16,
C-----------------------------------------------------------------------
C   17 = ABAM/TIUN                            GBA: 315   p. 61
C    Pac silver fir/coolwort foamflower                  R6 E 130a-83
C
     &'CFF152  ','ABAM  ',1095.,'SF  ', 120.,   1,   1,   1,
C-----------------------------------------------------------------------
C   18 = ABAM/OXOR                            GBA: 500   p. 33
C   Pac silver fir/oxalis                                R6 E 100-82
C
     &'CFF153  ','ABPR  ',1050.,'NF  ', 135.,   1,   1,   7,
C-----------------------------------------------------------------------
C   19 = ABAM/TIUN-STRO                       GBA: 501   p. 100
C    Pac silver fir/foamflower-rosy twisted stalk        R6 E TP-028-91
C
     &'CFF154  ','ABAM  ', 960.,'SF  ', 134.,   1,   1,   1,
C-----------------------------------------------------------------------
C   20 = ABAM/ACTR-MBS                        GBA: 410   p. 84
C    Pac silver fir/vanilla leaf (Mt Baker/Snoq)         R6 E TP-028-91
C
     &'CFF250  ','PSME  ', 900.,'DF  ', 155.,   1,   1,  16,
C-----------------------------------------------------------------------
C   21 = ABAM/ACTR-CLUN                       GBA: 415   p. 57
C    Pac silver fir/vanilla leaf-queencup beadlily       R6 E 130a-83
C
     &'CFF253  ','ABPR  ', 955.,'NF  ', 134.,   1,   1,   7,
C-----------------------------------------------------------------------
C   22 = ABAM/XETE-MBS                        GBA: 507   p. 132
C   Pac silver fir/beargrass (Mt Baker/Snoq)             R6 E TP-028-91
C                                                        & Devlin memo
     &'CFF312  ','ABPR  ',1399.,'NF  ', 117.,   1,   1,   7,
C----------------------------------------------------------------------
C   23 = ABAM/RUPE-BLSP                       GBA: 627   p. 98
C    Pac silver fir/five-leaved bramble-deerfern         R6 E TP-028-91
C
     &'CFF450  ','ABAM  ',1110.,'SF  ', 142.,   1,   1,   1,
C----------------------------------------------------------------------
C   24 = ABAM/LYAM                            GBA: 744   p. 90
C    Pac silver fir/skunkcabbage                         R6 E TP-028-91
C
     &'CFM111  ','ABAM  ', 715.,'SF  ', 134.,   1,   1,   1,
C----------------------------------------------------------------------
C   25 = ABAM/BENE-MBS                        GBA: 242   p. 86
C    Pac silver fir/Oregon grape (Mt Baker/Snoqualamie)  R6 E TP-028-91
C
     &'CFS110  ','ABPR  ', 820.,'NF  ', 109.,   1,   1,   7,
C---------------------------------------------------------------------
C   26 = ABAM/BENE                            GBA: 274   p. 56
C   Pac silver fir/dwarf Oregon grape                    R6 E 130a-83
C
     &'CFS151  ','TSHE  ',1035.,'WH  ',  64.,   1,   1,  19,
C---------------------------------------------------------------------
C   27 = ABAM/GASH-GP                         GBA: 324   p. 55
C   Pac silver fir/Salal (Giff Pinchot)                  R6 E 130a-83
C
     &'CFS152  ','ABAM  ',1035.,'SF  ', 108.,   1,   1,   1,
C---------------------------------------------------------------------
C   28 = ABAM/GASH-BENE                       GBA: 210   p. 88
C   Pac silver fir/salal-Oregon grape                    R6 E TP-028-91
C
     &'CFS154  ','ABAM  ',1040.,'SF  ', 115.,   1,   1,   1,
C---------------------------------------------------------------------
C   29 = ABAM/VAAL-BENE                       GBA: 410   p. 104
C   Pac silver fir/Alaska huckleberry-Oregon grape       R6 E TP-028-91
C
     &'CFS216  ','ABAM  ',1015.,'SF  ', 124.,   1,   1,   1,
C---------------------------------------------------------------------
C   30 = ABAM/VAME-VASI                       GBA: 442   p. 128
C   Pac silver fir/big huckleberry-Sitka valerian        R6 E TP-028-91
C
     &'CFS221  ','ABAM  ', 900.,'SF  ',  99.,   1,   1,   1/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=31,45) /
C---------------------------------------------------------------------
C   31 = ABAM/VAME-STRO                       GBA: 546   p. 124
C   Pac silver fir/big huckleberry-rosy twisted stalk    R6 E TP-028-91
C
     &'CFS222  ','ABAM  ', 975.,'SF  ', 118.,   1,   1,   1,
C---------------------------------------------------------------------
C   32 = ABAM/VAME-VAAL                       GBA: 302   p. 126
C   Pac silver fir/big huckleberry-Alaska huckleberry    R6 E TP-028-91
C
     &'CFS223  ','ABAM  ', 935.,'SF  ', 102.,   1,   1,   1,
C---------------------------------------------------------------------
C   33 = ABAM/VAME                            GBA: 241   p. 120
C   Pac silver fir/big huckleberry                       R6 E TP-028-91
C
     &'CFS224  ','ABAM  ',1100.,'SF  ', 100.,   1,   1,   1,
C----------------------------------------------------------------------
C   34 = ABAM/VAAL-MADI2                      GBA: 643   p. 110
C   Pac silver fir/Ak huckleberry-false lily-of-the-val  R6 E TP-028-91
C
     &'CFS225  ','ABAM  ', 945.,'SF  ', 126.,   1,   1,   1,
C----------------------------------------------------------------------
C   35 = ABAM/VAAL-TIUN-MBS                   GBA: 517   p. 116
C   Pac silver fir/Alaska huckleberry-foamflower         R6 E TP-028-91
C
     &'CFS226  ','ABAM  ',1030.,'SF  ', 136.,   1,   1,   1,
C----------------------------------------------------------------------
C   36 = ABAM/VAME-PYSE                       GBA: 299   p. 122
C   Pac silver fir/big huckleberry-sidebells pyrola      R6 E TP-028-91
C
     &'CFS229  ','ABAM  ',1110.,'SF  ', 108.,   1,   1,   1,
C----------------------------------------------------------------------
C   37 = ABAM/VAAL-GASH-MBS                   GBA: 476   p. 108
C   Pac silver fir/Alaska huckleberry-salal (Mt B/Snoq)  R6 E TP-028-91
C
     &'CFS230  ','ABAM  ', 830.,'SF  ', 101.,   1,   1,   1,
C----------------------------------------------------------------------
C   38 = ABAM/VAAL-POMU                       GBA: 955   p. 112
C   Pac silver fir/Alaska huckleberry-swordfern          R6 E TP-028-91
C
     &'CFS231  ','ABAM  ',1035.,'SF  ', 148.,   1,   1,   1,
C---------------------------------------------------------------------
C   39 = ABAM/VAME/XETE                       GBA: 286   p. 66
C   Pac silver fir/big huckleberry/beargrass             R6 E 130a-83
C
     &'CFS251  ','ABAM  ', 955.,'SF  ',  94.,   1,   1,   1,
C-----------------------------------------------------------------------
C   40 = ABAM/VAME-XETE-MBS                   GBA: 386   p. 130
C   Pac silver fir/big huckleberry-beargrass (Mt B/Snoq) R6 E TP-028-91
C                                                        & Devlin memo
     &'CFS252  ','ABAM  ',1065.,'SF  ',  94.,   1,   1,   1,
C-----------------------------------------------------------------------
C   41 = ABAM/VAAL/COCA                       GBA: 407   p. 45
C   Pac silver fir/Alaska huckleberry/dogwood bunchberry R6 E 100-82
C
     &'CFS253  ','ABPR  ', 975.,'NF  ', 110.,   1,   1,   7,
C----------------------------------------------------------------------
C   42 = ABAM/MEFE                            GBA: 278   p. 64
C   Pac silver fir/fool's huckleberry                    R6 E 130a-83
C
     &'CFS254  ','ABAM  ',1035.,'SF  ', 103.,   1,   1,   1,
C---------------------------------------------------------------------
C   43 = ABAM/VAAL-GASH                       GBA: 294   p. 60
C   Pac silver fir/Alaska huckleberry-salal              R6 E 130a-83
C
     &'CFS255  ','ABAM  ', 880.,'SF  ', 113.,   1,   1,   1,
C----------------------------------------------------------------------
C   44 = ABAM/VAME/CLUN                       GBA: 243   p. 65
C   Pac silver fir/big huckleberry/queencup beadlily     R6 E 130a-83
C
     &'CFS256  ','ABAM  ', 980.,'SF  ', 113.,   1,   1,   1,
C----------------------------------------------------------------------
C   45 = ABAM/VAAL                            GBA: 250   p. 59
C   Pac silver fir/Alaska huckleberry                    R6 E 130a-83
C
     &'CFS257  ','ABAM  ', 985.,'SF  ', 111.,   1,   1,   1/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=46,60) /
C----------------------------------------------------------------------
C   46 = ABAM/VAAL-MBS                        GBA: 366   p. 102
C   Pac silver fir/Alaska huckleberry (Mt Baker/Snoq)    R6 E TP-028-91
C                                                        & Devlin memo
     &'CFS258  ','ABAM  ',1010.,'SF  ', 116.,   1,   1,   1,
C----------------------------------------------------------------------
C   47 = ABAM/VAAL-XETE-MBS                   GBA: 227   p. 118
C   Pac silver fir/Alaska huckleberry-beargrass (MB/SQ)  R6 E TP-028-91
C                                                        & Devlin memo
     &'CFS259  ','ABAM  ', 626.,'SF  ',  94.,   1,   1,   1,
C----------------------------------------------------------------------
C   48 = ABAM/VAAL-CLUN-MBS                   GBA: 556   p. 106
C   Pac silver fir/AK huckleberry-queen's cup (MB/SQ)    R6 E TP-028-91
C                                                        & Devlin memo
     &'CFS260  ','ABAM  ',1535.,'SF  ', 128.,   1,   1,   1,
C----------------------------------------------------------------------
C   49 = ABAM/OPHO                            GBA: 370   p. 62
C   Pac silver fir/devil's club                          R6 E 130a-83
C
     &'CFS351  ','ABAM  ', 825.,'SF  ', 130.,   1,   1,   1,
C----------------------------------------------------------------------
C   50 = ABAM/OPHO-VAAL                       GBA: 585   p. 92
C   Pac silver fir/devil's club-Alaska huckleberry       R6 E TP-028-91
C
     &'CFS352  ','ABAM  ',1030.,'SF  ', 133.,   1,   1,   1,
C----------------------------------------------------------------------
C   51 = ABAM/RHAL-GP                         GBA: 214   p. 63
C   Pac silver fir/Cascades azalea (Gifford Pinchot)     R6 E 130a-83
C
     &'CFS550  ','ABAM  ',1120.,'SF  ', 102.,   1,   1,   1,
C----------------------------------------------------------------------
C   52 = ABAM/RHAL/XETE                       GBA: 282   p. 37
C   Pac silver fir/Cascades azalea/beargrass             R6 E 100-82
C
     &'CFS551  ','PSME  ', 815.,'DF  ',  73.,   1,   1,  16,
C----------------------------------------------------------------------
C   53 = ABAM/RHAL/CLUN                       GBA: 282   p. 35
C   Pac silver fir/Cascades azalea/queencup beadlily     R6 E 100-82
C
     &'CFS552  ','PSME  ', 778.,'DF  ',  73.,   1,   1,  16,
C----------------------------------------------------------------------
C   54 = ABAM/RHAL-VAME                       GBA: 241   p. 96
C   Pac silver fir/white rhododendron-big huckleberry    R6 E TP-028-91
C
     &'CFS554  ','ABAM  ', 995.,'SF  ',  93.,   1,   1,   1,
C----------------------------------------------------------------------
C   55 = ABAM/RHAL-VAAL                       GBA: 259   p. 94
C   Pac silver fir/white rhododendron-Alaska huckleberry R6 E TP-028-91
C
     &'CFS555  ','ABAM  ', 715.,'SF  ',  98.,   1,   1,   1,
C----------------------------------------------------------------------
C   56 = ABAM/ACCI/TIUN                       GBA: 505   p. 43
C   Pac silver fir/vine maple/coolwort foamflower        R6 E 100-82
C
     &'CFS651  ','ABPR  ',1030.,'NF  ', 140.,   1,   1,   7,
C---------------------------------------------------------------------
C   57 = ABAM/RHMA-BENE                       GBA: 296   p. 55
C   Pac silver fir/rhododendron-dwarf Oregon grape       R6 E 100-82
C
     &'CFS652  ','PSME  ',1010.,'DF  ', 104.,   1,   1,  16,
C---------------------------------------------------------------------
C   58 = ABAM/RHMA/XETE                       GBA: 501   p. 57
C   Pac silver fir/rhododendron/beargrass                R6 E 100-82
C
     &'CFS653  ','ABPR  ', 910.,'NF  ',  96.,   1,   1,   7,
C---------------------------------------------------------------------
C   59 = ABAM/RHMA-VAAL/COCA                  GBA: 347   p. 47
C   Pac silver fir/rhododendron-Ak huckleb/dogwood bunch R6 E 100-82
C
     &'CFS654  ','PSME  ', 995.,'DF  ',  97.,   1,   1,  16,
C--------------------------------------------------------------------
C   60 = TSHE-PSME/HODI                       GBA: 372   p. 102
C   Western hemlock-Douglas-fir/oceanspray               R6 E 230A-86
C
     &'CHC212  ','PSME  ', 675.,'DF  ', 120.,   1,   1,  16/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=61,75) /
C-------------------------------------------------------------------
C   61 = TSHE-PSME-ARME                       GBA: 385   p. 105
C   Western hemlock-Douglas-fir-madrone                  R6 E 230A-86
C
     &'CHC213  ','PSME  ',1063.,'DF  ', 105.,   1,   1,  16,
C-----------------------------------------------------------------------
C   62 = TSHE/OXOR-WILL                       GBA: 467   p. 202
C   Western hemlock/Oregon oxalis (Willamette)           R6 E 257-86
C
     &'CHF111  ','PSME  ', 800.,'DF  ', 158.,   1,   1,  16,
C-----------------------------------------------------------------------
C   63 = TSHE/POMU-MTH                        GBA: 466   p. 73
C   Western hemlock/swordfern  (Mt Hood)                 R6 E 232A-86
C
     &'CHF123  ','TSHE  ', 770.,'WH  ',  95.,   1,   1,  19,
C----------------------------------------------------------------------
C   64 = TSHE/POMU-OXOR                       GBA: 527   p. 75
C   Western hemlock/swordfern-oxalis                     R6 E 232A-86
C
     &'CHF124  ','TSHE  ', 905.,'WH  ', 102.,   1,   1,  19,
C----------------------------------------------------------------------
C   65 = TSHE/POMU-GP                         GBA: 431   p. 82
C   Western hemlock/swordfern (Gifford Pinchot)          R6 E 230A-86
C
     &'CHF125  ','TSHE  ', 740.,'WH  ',  96.,   1,   1,  19,
C---------------------------------------------------------------------
C   66 = TSHE/POMU-GASH                       GBA: 311   p. 54
C   Western hemlock/swordfern-salal                      R6 E TP-028-91
C
     &'CHF133  ','PSME  ',1005.,'DF  ', 151.,   1,   1,  16,
C---------------------------------------------------------------------
C   67 = TSHE/POMU-BENE                       GBA: 543   p. 52
C   Western hemlock/swordfern-Oregon grape               R6 E TP-028-91
C
     &'CHF134  ','PSME  ',1090.,'DF  ', 154.,   1,   1,  16,
C---------------------------------------------------------------------
C   68 = TSHE/POMU-TITR-MBS                   GBA: 555   p. 56
C   Western hemlock/swordfern-foamflower                 R6 E TP-028-91
C                                                        & Devlin memo
     &'CHF135  ','TSHE  ',1532.,'WH  ', 123.,   1,   1,  19,
C----------------------------------------------------------------------
C   69 = TSHE/POMU-WILL                       GBA: 402   p. 234
C   Western hemlock/swordfern (Willamette)               R6 E 257-86
C
     &'CHF151  ','PSME  ', 870.,'DF  ', 159.,   1,   1,  16,
C----------------------------------------------------------------------
C   70 = TSHE/ACTR                            GBA: 402   p. 90
C   Western hemlock/vanilla leaf                         R6 E 230A-86
C
     &'CHF221  ','PSME  ', 960.,'DF  ', 147.,   1,   1,  16,
C---------------------------------------------------------------------
C   71 = TSHE/TITR                            GBA: 564   p. 80
C   Western hemlock/coolwort foamflower                  R6 E 230A-86
C
     &'CHF222  ','PSME  ', 975.,'DF  ', 170.,   1,   1,  16,
C---------------------------------------------------------------------
C   72 = TSHE/TITR-GYDR                       GBA: 1121  p. 58
C   Western hemlock/foamflower-oak fern                  R6 E TP-028-91
C
     &'CHF250  ','PSME  ', 965.,'DF  ', 164.,   1,   1,  16,
C---------------------------------------------------------------------
C   73 = TSHE/LIBO2                           GBA: 525   p. 238
C   Western hemlock/twinflower                           R6 E 257-86
C
     &'CHF321  ','PSME  ',1020.,'DF  ', 148.,   1,   1,  16,
C---------------------------------------------------------------------
C   74 = TSHE/ATFI                            GBA: 601   p. 72
C   Western hemlock/ladyfern                             R6 E 230A-86
C
     &'CHF421  ','PSME  ', 880.,'DF  ', 174.,   1,   1,  16,
C--------------------------------------------------------------------
C   75 = TSHE/LYAM                            GBA: 408   p. 68
C   Western hemlock/American yellow skunkcabbage         R6 E 232A-86
C
     &'CHM121  ','PSME  ',1126.,'DF  ', 128.,   1,   1,  16/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=76,90) /
C---------------------------------------------------------------------
C   76 = TSHE/GASH-WILL                       GBA: 334   p. 230
C   Western hemlock/salal (Willamette)                   R6 E 257-86
C
     &'CHS111  ','PSME  ', 740.,'DF  ', 137.,   1,   1,  16,
C---------------------------------------------------------------------
C   77 = TSHE/BENE/OXOR                       GBA: 514   p. 190
C   Western hemlock/dwarf Oregon grape/Oregon oxalis     R6 E 257-86
C
     &'CHS113  ','PSME  ', 770.,'DF  ', 159.,   1,   1,  16,
C--------------------------------------------------------------------
C   78 = TSHE/BENE/ACTR                       GBA: 476   p. 198
C   Western hemlock/dwarf Oregon grape/vanilla leaf      R6 E 257-86
C
     &'CHS114  ','PSME  ',1010.,'DF  ', 158.,   1,   1,  16,
C---------------------------------------------------------------------
C   79 = TSHE/BENE-GASH                       GBA: 440   p. 62
C   Western hemlock/dwarf Oregon grape-salal             R6 E 232A-86
C
     &'CHS124  ','TSHE  ', 845.,'WH  ',  93.,   1,   1,  19,
C---------------------------------------------------------------------
C   80 = TSHE/BENE                            GBA: 424   p. 93
C   Western hemlock/dwarf Oregon grape                   R6 E 230A-86
C
     &'CHS125  ','TSHE  ',1020.,'WH  ',  82.,   1,   1,  19,
C-----------------------------------------------------------------------
C   81 = TSHE/BENE/POMU                       GBA: 380   p. 64
C   Western hemlock/dwarf Oregon grape/swordfern         R6 E 232A-86
C
     &'CHS126  ','TSHE  ', 835.,'WH  ',  89.,   1,   1,  19,
C----------------------------------------------------------------------
C   82 = TSHE/BENE-GASH-GP                    GBA: 381   p. 95
C   Western hemlock/dwarf Oregon grape-salal (Giff Pin)  R6 E 230A-86
C
     &'CHS127  ','PSME  ', 925.,'DF  ', 134.,   1,   1,  16,
C----------------------------------------------------------------------
C   83 = TSHE/GASH-GP                         GBA: 317   p. 97
C   Western hemlock/salal (Gifford Pinchot)              R6 E 230A-86
C
     &'CHS128  ','PSME  ', 820.,'DF  ', 123.,   1,   1,  16,
C----------------------------------------------------------------------
C   84 = TSHE/GASH-MBS                        GBA: 286   p. 40
C   Western hemlock/salal (Mt Baker/Snoqual)             R6 E TP-028-91
C                                                        & Devlin memo
     &'CHS129  ','PSME  ', 789.,'DF  ', 100.,   1,   1,  16,
C---------------------------------------------------------------------
C   85 = TSHE/BENE-MBS                        GBA: 399   p. 36
C   Western hemlock/Oregon grape (Mt Baker/Snoq)         R6 E TP-028-91
C                                                        & Devlin memo
     &'CHS130  ','PSME  ',1101.,'DF  ', 122.,   1,   1,  16,
C--------------------------------------------------------------------
C   86 = TSHE/GASH-BENE                       GBA: 348   p. 42
C   Western hemlock/salal-Oregon grape                   R6 E TP-028-91
C
     &'CHS135  ','PSME  ',1225.,'DF  ', 117.,   1,   1,  16,
C--------------------------------------------------------------------
C   87 = TSHE/GASH-VAME                       GBA: 303   p. 44
C   Western hemlock/salal-big huckleberry                R6 E TP-028-91
C
     &'CHS140  ','PSME  ', 890.,'DF  ',  89.,   1,   1,  16,
C----------------------------------------------------------------------
C   88 = TSHE/BENE-CHME                       GBA: 273   p. 38
C   Western hemlock/Oregon grape-little prince's pine    R6 E TP-028-91
C
     &'CHS141  ','PSME  ',1070.,'DF  ', 103.,   1,   1,  16,
C---------------------------------------------------------------------
C   89 = TSHE/ACCI/ACTR                       GBA: 238   p. 56
C   Western hemlock/vine maple/vanilla leaf              R6 E 232A-86
C
     &'CHS223  ','PSME  ', 880.,'DF  ', 141.,   1,   1,  16,
C----------------------------------------------------------------------
C   90 = TSHE/CONU/ACTR                       GBA: 420   p. 100
C   Western hemlock/dogwood/vanilla leaf                 R6 E 230A-86
C
     &'CHS224  ','PSME  ',1159.,'DF  ', 142.,   1,   1,  16/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=91,105) /
C---------------------------------------------------------------------
C   91 = TSHE/ACCI-BENE                       GBA: 478   p. 34
C   Western hemlock/vine maple-Oregon grape              R6 E TP-028-91
C
     &'CHS251  ','PSME  ', 955.,'DF  ', 136.,   1,   1,  16,
C---------------------------------------------------------------------
C   92 = TSHE/RHMA/XETE-MTH                   GBA: 165   p. 83
C   Western hemlock/rhododendron/beargrass (Mt Hood)     R6 E 232A-86
C
     &'CHS325  ','PSME  ', 845.,'DF  ',  97.,   1,   1,  16,
C---------------------------------------------------------------------
C   93 = TSHE/RHMA-VAAL/COCA                  GBA: 229   p. 81
C   W hemlock/rhododendron-AK huckleberry/dogwood bunchb R6 E 232A-86
C
     &'CHS326  ','PSME  ', 885.,'DF  ', 130.,   1,   1,  16,
C-------------------------------------------------------------------
C   94 = TSHE/RHMA-GASH-MTH                   GBA: 299   p. 79
C   Western hemlock/rhododendron-salal (Mt Hood)         R6 E 232A-86
C
     &'CHS327  ','TSHE  ', 700.,'WH  ',  77.,   1,   1,  19,
C------------------------------------------------------------------
C   95 = TSHE/RHMA-BENE-MTH                   GBA: 388   p. 77
C   W hemlock/rhododendron-dwarf Oregon grape (Mt Hood)  R6 E 232A-86
C
     &'CHS328  ','TSHE  ', 835.,'WH  ',  82.,   1,   1,  19,
C----------------------------------------------------------------
C   96 = TSHE/RHMA-GASH-WILL                  GBA: 338   p. 222
C   Western hemlock/rhododendron-salal (Willamette)      R6 E 257-86
C
     &'CHS351  ','PSME  ', 890.,'DF  ', 128.,   1,   1,  16,
C--------------------------------------------------------------------
C   97 = TSHE/RHMA-BENE-WILL                  GBA: 367   p. 214
C    W hemlock/rhododendron-dwarf OR grape (Willamette)  R6 E 257-86
C
     &'CHS352  ','PSME  ', 930.,'DF  ', 136.,   1,   1,  16,
C---------------------------------------------------------------------
C   98 = TSHE/RHMA/XETE-WILL                  GBA: 298   p. 210
C   Western hemlock/rhododendron/beargrass (Willamette)  R6 E 257-86
C
     &'CHS353  ','PSME  ', 970.,'DF  ', 122.,   1,   1,  16,
C--------------------------------------------------------------------
C   99 = TSHE/RHMA/OXOR                       GBA: 495   p. 218
C   Western hemlock/rhododendron/Oregon oxalis           R6 E 257-86
C
     &'CHS354  ','PSME  ', 670.,'DF  ', 135.,   1,   1,  16,
C--------------------------------------------------------------------
C  100 = TSHE/RHMA/LIBO2                      GBA: 447   p. 226
C  Western hemlock/rhododendron/twinflower               R6 E 257-86
C
     &'CHS355  ','PSME  ',1100.,'DF  ', 130.,   1,   1,  16,
C--------------------------------------------------------------------
C  101 = TSHE/OPHO-WILL                       GBA: 413   p. 182
C  Western  hemlock/devil's club (Willamette)            R6 E 257-86
C
     &'CHS511  ','PSME  ', 685.,'DF  ', 168.,   1,   1,  16,
C---------------------------------------------------------------------
C  102 = TSHE/OPHO-ATFI                       GBA: 276   p. 50
C  Western  hemlock/devil's club-ladyfern                R6 E TP-028-91
C
     &'CHS513  ','TSHE  ', 980.,'WH  ', 101.,   1,   1,  19,
C-----------------------------------------------------------------------
C  103 = TSHE/OPHO/OXOR                       GBA: 288   p. 69
C  Western  hemlock/devil's club/Oregon oxalis           R6 E 232A-86
C
     &'CHS522  ','TSHE  ', 815.,'WH  ',  93.,   1,   1,  19,
C-----------------------------------------------------------------------
C  104 = TSHE/OPHO/SMST                       GBA: 212   p. 71
C  Western hemlock/devil's club/starry solomonseal       R6 E 232A-86
C
     &'CHS523  ','PSME  ', 585.,'DF  ', 156.,   1,   1,  16,
C----------------------------------------------------------------------
C  105 = TSHE/OPHO/POMU                       GBA: 579   p. 74
C  Western hemlock/devil's club/swordfern                R6 E 230A-86
C
     &'CHS524  ','TSHE  ', 965.,'WH  ',  88.,   1,   1,  19/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=106,120) /
C-----------------------------------------------------------------------
C  106 = TSHE/VAAL-OPHO                       GBA: 278   p. 90
C  Western hemlock/Alaska huckleberry-devil's club       R6 E 232A-86
C
     &'CHS611  ','PSME  ', 767.,'DF  ', 165.,   1,   1,  16,
C-----------------------------------------------------------------------
C  107 = TSHE/VAME/XETE                       GBA: 175   p. 93
C  Western hemlock/big huckleberry/beargrass             R6 E 232A-86
C
     &'CHS612  ','PSME  ', 795.,'DF  ',  90.,   1,   1,  16,
C---------------------------------------------------------------------
C  108 = TSHE/VAAL/OXOR                       GBA: 444   p. 78
C  Western hemlock/Alaska huckleberry/Oregon oxalis      R6 E 230A-86
C
     &'CHS613  ','TSHE  ', 985.,'WH  ',  84.,   1,   1,  19,
C----------------------------------------------------------------------
C  109 = TSHE/VAAL-GASH                       GBA: 295   p. 88
C  Western hemlock/Alaska huckleberry-salal              R6 E 230A-86
C
     &'CHS614  ','TSHE  ', 710.,'WH  ',  81.,   1,   1,  19,
C---------------------------------------------------------------------
C  110 = TSHE/VAAL/COCA                       GBA: 375   p. 86
C  Western hemlock/Alaska huckleberry/dogwood bunchberry R6 E 230A-86
C
     &'CHS615  ','TSHE  ', 770.,'WH  ',  87.,   1,   1,  19,
C---------------------------------------------------------------------
C  111 = TSHE/VAAL-POMU                       GBA: 842   p. 64
C  Western hemlock/Alaska huckleberry-swordfern          R6 E TP-028-91
C
     &'CHS625  ','PSME  ',1050.,'DF  ', 154.,   1,   1,  16,
C---------------------------------------------------------------------
C  112 = TSHE/VAAL-BENE                       GBA: 277   p. 62
C  Western hemlock/Alaska huckleberry-Oregon grape       R6 E TP-028-91
C
     &'CHS626  ','PSME  ', 940.,'DF  ', 110.,   1,   1,  16,
C--------------------------------------------------------------------
C  113 = TSME/TIUN-STRO                       GBA: 174   p. 162
C  M hemlock/foamflower-rosy twistedstalk                R6 E TP-028-91
C
     &'CMF250  ','TSME  ', 820.,'MH  ',  36.,   1,   1,  20,
C---------------------------------------------------------------------
C  114 = TSME/CABI                            GBA: 134   p. 150
C  Mountain hemlock/marshmarigold                        R6 E TP-028-91
C
     &'CMF251  ','TSME  ', 795.,'MH  ',  14.,   1,   1,  20,
C----------------------------------------------------------------------
C  115 = TSME/VASC                            GBA: 195   p. 73
C  Mountain hemlock/grouse huckleberry                   R6 E 08-95
C
     &'CMS114  ','TSME  ', 925.,'MH  ',  16.,   1,   1,  20,
C---------------------------------------------------------------------
C  116 = TSME/VAME-GP                         GBA: 221   p. 68
C  Mountain hemlock/big huckleberry (Gifford Pinchot)    R6 E 130-83
C
     &'CMS210  ','TSME  ', 970.,'MH  ',  25.,   1,   1,  20,
C-------------------------------------------------------------------
C  117 = TSME/VAME/XETE                       GBA: 278   p. 67
C  Mountain hemlock/big huckleberry/beargrass            R6 E 08-95
C
     &'CMS216  ','TSME  ', 880.,'MH  ',  19.,   1,   1,  20,
C---------------------------------------------------------------------
C  118 = TSME/VAME/CLUN                       GBA: 303   p. 61
C  Mountain hemlock/big huckleberry/queen's cup          R6 E 08-95
C
     &'CMS218  ','TSME  ', 955.,'MH  ',  20.,   1,   1,  20,
C---------------------------------------------------------------------
C  119 = TSME/MEFE                            GBA: 215   p. 39
C  Mountain hemlock/fool's huckleberry                   R6 E 08-95
C
     &'CMS221  ','TSME  ',1005.,'MH  ',  22.,   1,   1,  20,
C------------------------------------------------------------------
C  120 = TSME/RHAL                            GBA: 235   p. 51
C  Mountain hemlock/Cascades azalea                      R6 E 08-95
C
     &'CMS223  ','TSME  ', 955.,'MH  ',  21.,   1,   1,  20/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=121,135) /
C-------------------------------------------------------------------
C  121 = TSME/VAAL                            GBA: 185   p. 164
C  Mountain hemlock/Alaska huckleberry                   R6 E TP-028-91
C
     &'CMS241  ','TSME  ',1015.,'MH  ',  34.,   1,   1,  20,
C------------------------------------------------------------------
C  122 = TSME/VAME-VAAL                       GBA: 399   p. 178
C  Mountain hemlock/big huckleberry-Alaska huckleberry   R6 E TP-028-91
C
     &'CMS244  ','TSME  ', 995.,'MH  ',  29.,   1,   1,  20,
C-------------------------------------------------------------------
C  123 = TSME/VAME/XETE-WASH                  GBA: 308   p. 182
C  Mountain hemlock/big huckleberry/beargrass (MB/SQ)    R6 E TP-028-91
C
     &'CMS245  ','TSME  ', 935.,'MH  ',  25.,   1,   1,  20,
C-------------------------------------------------------------------
C  124 = TSME/VAME-MBS                        GBA: 363   p. 172
C  Mountain hemlock/big huckleberry (Mt Baker/Snoqual)   R6 E TP-028-91
C
     &'CMS246  ','TSME  ',1075.,'MH  ',  25.,   1,   1,  20,
C-------------------------------------------------------------------
C  125 = TSME/VAME-STRO                       GBA: 512   p. 176
C  Mountain hemlock/big huckleberry-rosy twistedstalk    R6 E TP-028-91
C
     &'CMS250  ','TSME  ', 780.,'MH  ',  31.,   1,   1,  20,
C-------------------------------------------------------------------
C  126 = TSME/VAME-VASI                       GBA: 238   p. 180
C  Mountain hemlock/big huckleberry-Sitka valerian       R6 E TP-028-91
C
     &'CMS251  ','TSME  ', 770.,'MH  ',  25.,   1,   1,  20,
C-------------------------------------------------------------------
C  127 = TSME/VAAL-STRO                       GBA: 402   p. 170
C  Mountain hemlock/Alaska huckleberry-rosy twistedstalk R6 E TP-028-91
C
     &'CMS252  ','TSME  ', 960.,'MH  ',  35.,   1,   1,  20,
C--------------------------------------------------------------------
C  128 = TSME/VAAL-CLUN                       GBA: 191   p. 166
C  Mountain hemlock/Alaska huckleberry-queen's cup       R6 E TP-028-91
C
     &'CMS253  ','TSME  ',1090.,'MH  ',  29.,   1,   1,  20,
C--------------------------------------------------------------------
C  129 = TSME/VAME-RULA                       GBA: 310   p. 174
C  Mountain hemlock/big huckleberry-trailing bramble     R6 E TP-028-91
C
     &'CMS254  ','TSME  ',1155.,'MH  ',  28.,   1,   1,  20,
C----------------------------------------------------------------------
C  130 = TSME/VAAL-MADI2                      GBA: 208   p. 168
C  M hemlock/Alaska huckleberry-false lily-of-the-valley R6 E TP-028-91
C
     &'CMS255  ','TSME  ', 710.,'MH  ',  29.,   1,   1,  20,
C---------------------------------------------------------------------
C  131 = TSME/PHEM-VADE                       GBA: 291   p. 156
C  M hemlock/red heather-blueleaf huckleberry            R6 E TP-028-91
C
     &'CMS350  ','TSME  ', 750.,'MH  ',  20.,   1,   1,  20,
C---------------------------------------------------------------------
C  132 = TSME/RHAL-VAAL                       GBA: 300   p. 158
C  M hemlock/white rhododendron-Alaska huckleberry       R6 E TP-028-91
C
     &'CMS351  ','TSME  ', 820.,'MH  ',  23.,   1,   1,  20,
C---------------------------------------------------------------------
C  133 = TSME/RHAL-VAME                       GBA: 210   p. 160
C  Mountain hemlock/white rhododendron-big huckleberry   R6 E TP-028-91
C
     &'CMS352  ','TSME  ', 970.,'MH  ',  23.,   1,   1,  20,
C-----------------------------------------------------------------------
C  134 = TSME/CLPY-RUPE                       GBA: 281   p. 152
C  Mountain hemlock/copperbush-five leaved bramble       R6 E TP-028-91
C
     &'CMS353  ','TSME  ', 675.,'MH  ',  20.,   1,   1,  20,
C----------------------------------------------------------------------
C  135 = TSME/OPHO-VAAL                       GBA: 291   p. 154
C  Mountain hemlock/devil's club-Alaska huckleberry      R6 E TP-028-91
C
     &'CMS450  ','ABAM  ', 855.,'SF  ', 138.,   1,   1,   1/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=136,139) /
C----------------------------------------------------------------------
C  136 = TSME/RHMA                            GBA: 249   p. 57
C  Mountain hemlock/rhododendron                         R6 E TP-08-95
C
     &'CMS612  ','ABAM  ',1010.,'SF  ',  78.,   1,   1,   1,
C---------------------------------------------------------------------
C  137 = ABGR/CHUM                            GBA: 475   p. 96
C  Grand fir/prince's pine                               R6 E 257-86
C
     &'CWF211  ','PSME  ', 730.,'DF  ', 132.,   1,   1,  16,
C---------------------------------------------------------------------
C  138 = ABGR/ARUV                            GBA: 213   p. 90
C  Grand fir/bearberry                                   R6 E 257-86
C
     &'CWS521  ','PSME  ', 820.,'DF  ',  86.,   1,   1,  16,
C---------------------------------------------------------------------
C  139 = ABGR/BENE                            GBA: 370   p. 92
C  Grand fir/dwarf Oregon grape                          R6 E 257-86
C
     &'CWS522  ','PSME  ', 860.,'DF  ', 131.,   1,   1,  16/
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
        RSDI   = 0.
        RSI    = 0.
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
