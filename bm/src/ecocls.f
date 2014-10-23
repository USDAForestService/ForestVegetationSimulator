      SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
      IMPLICIT NONE
C----------
C  **ECOCLS---BM      DATE OF LAST REVISION:  03/19/14
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
C  PLANT ASSOCIATION (PA) REFERENCE IS INDICATED FOR EACH FVS ECOCLASS
C  THE KEY TO THE REFERENCE FOR SDIMAX / SITE INDEX VALUES IS:
C  P = D.C.POWELL. DECEMBER 2009,UPDATES OF MAXIMUM STAND DENSITY INDEX
C      AND SITE INDEX FOR THE BLUE MOUNTAINS VARIANT OF THE FOREST
C      VEGETATION SIMULATOR. ON FILE WITH FMSC
C  C = N.CROOKSTON. JUNE 2008, MAX SDI VALUES FROM AN R6 ALL CVS DATA
C      STUDY
C  H = FRED HALL, R6/NR, PERSONAL COMMUNICATION, SDI MAX VALUES BASED
C      ON GBA
C----------
      INTEGER NENTRY
      PARAMETER (NENTRY=283)
      CHARACTER*4 SPC(NENTRY),ASPEC
      CHARACTER*6 SCIEN(NENTRY)
      CHARACTER*8 APASS,PA(NENTRY)
      INTEGER FVSSEQ(NENTRY)
C
      INTEGER ISEQ,INDEX,NUM,I,K,ISFLAG
      REAL RSI,RSDI
      REAL SITE(NENTRY),SDIMX(NENTRY)
      INTEGER NUMBR(NENTRY),IFLAG(NENTRY)
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=1,17) /
C-----------------------------------------------------------------------
C      ALPHA     SCIEN          ALPHA       NUM  SITE FVS  PA REFERENCE.
C       ECO      SITE      MAX  SITE  SITE  IN   SPP  SEQ  SDIMAX/SI
C      CLASS     SPEC      SDI  SPEC  INDX  ECO  FLAG NUM  REFERENCE. 
C-----------------------------------------------------------------------
C    1 = ABLA2/CAGE                                      p. 37                       
C    Subalpine fir/elk sedge                             R6 E TP-036-92              
C
     &'CAG111  ','PSME  ',  0.,'DF  ',  48,   5,   0,   3,       !  /P
     &'CAG111  ','LAOC  ',  0.,'WL  ',  65,   5,   0,   2,       !  /P
     &'CAG111  ','PICO  ', 346,'LP  ',  78,   5,   1,   7,       ! P/P
     &'CAG111  ','PIEN  ',  0.,'ES  ',  66,   5,   0,   8,       !  /P
     &'CAG111  ','ABLA2 ', 465,'AF  ',  62,   5,   0,   9,       ! P/P
C-----------------------------------------------------------------------
C    2 =ABLA2/STOC                                      XXXXXXX
C    Subalpine fir/elk sedge                            XXXXXXXXXXXXXXXX
C
     &'CAG4    ','PSME  ',  0.,'DF  ',  56,   4,   0,   3,       !  /P
     &'CAG4    ','PICO  ', 346,'LP  ',  78,   4,   1,   7,       ! P/P
     &'CAG4    ','PIEN  ',  0.,'ES  ',  64,   4,   0,   8,       !  /P     
     &'CAG4    ','ABLA2 ', 465,'AF  ',  48,   4,   0,   9,       ! P/P
C-----------------------------------------------------------------------
C    3 = PSME/CAGE-BLUE                                  p. 93          
C    Douglas-fir/Geyer's sedge (Blue Mtns)               R6 E TP-036-92 
C
     &'CDG111  ','PIPO  ', 278,'PP  ',  77,   4,   1,  10,       ! P/P
     &'CDG111  ','PSME  ', 351,'DF  ',  52,   4,   0,   3,       ! P/P
     &'CDG111  ','LAOC  ',  0.,'WL  ',  59,   4,   0,   2,       !  /P
     &'CDG111  ','ABGR  ',  0.,'GF  ',  62,   4,   0,   4,       !  /P
C----------------------------------------------------------------------
C    4 = PSME/CARU-BLUE                                  p. 91         
C    Douglas-fir/pinegrass (Blue Mtns)                   R6 E TP-036-92
C
     &'CDG112  ','PIPO  ', 329,'PP  ',  83,   4,   1,  10,       ! P/P
     &'CDG112  ','PSME  ', 330,'DF  ',  53,   4,   0,   3,       ! P/P
     &'CDG112  ','LAOC  ',  0.,'WL  ',  55,   4,   0,   2,       !  /P
     &'CDG112  ','ABGR  ',  0.,'GF  ',  48,   4,   0,   4/       !  /P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=18,34) /
C----------------------------------------------------------------------
C    5 = PSME/CARU                                       p. 332        
C    Douglas-fir/pinegrass                               R6 E TP-255-86
C
     &'CDG121  ','PIPO  ', 451,'PP  ',  86,   2,   1,  10,       ! P/P
     &'CDG121  ','PSME  ', 475,'DF  ',  55,   2,   0,   3,       ! P/P
C----------------------------------------------------------------------
C    6 = PSME/HODI                                       p. 85         
C    Douglas-fir/oceanspray                              R6 E TP-036-92
C
     &'CDS611  ','PIPO  ', 425,'PP  ',  86,   2,   0,  10,       ! P/P
     &'CDS611  ','PSME  ', 319,'DF  ',  64,   2,   1,   3,       ! P/P
C-----------------------------------------------------------------------
C    7 = PSME/SYAL-WALLO                                 p. 358        
C    Douglas-fir/common snowberry  (Wallowa)             R6 E TP-255-86
C
     &'CDS622  ','PIPO  ', 416,'PP  ',  84,   2,   1,  10,       ! P/P
     &'CDS622  ','PSME  ', 475,'DF  ',  60,   2,   0,   3,       ! P/P
C-----------------------------------------------------------------------
C    8 = PSME/SYOR-WALLO                                 p. 365        
C    Douglas-fir/mountain snowberry (Wallowa)            R6 E TP-255-86
C
     &'CDS623  ','PIPO  ', 451,'PP  ',  90,   2,   1,  10,       ! P/P
     &'CDS623  ','PSME  ',  0.,'DF  ',  55,   2,   0,   3,       !  /P
C-----------------------------------------------------------------------
C    9 = PSME/SYAL-BLUE                                  p. 87         
C    Douglas-fir/common snowberry (Blue Mtns)            R6 E TP-036-92
C
     &'CDS624  ','PIPO  ', 341,'PP  ',  81,   4,   0,  10,       ! P/P
     &'CDS624  ','PSME  ', 309,'DF  ',  61,   4,   1,   3,       ! P/P
     &'CDS624  ','LAOC  ', 256,'WL  ',  0.,   4,   0,   2,       ! P/ 
     &'CDS624  ','PSME  ',  0.,'GF  ',  70,   4,   0,   4,       !  /P
C-----------------------------------------------------------------------
C    10= PSME/SPBE                                       p. 352        
C    Douglas-fir/spiraea                                 R6 E TP-255-86
C
     &'CDS634  ','PIPO  ', 441,'PP  ',  82,   2,   1,  10,       ! P/P 
     &'CDS634  ','PSME  ', 464,'DF  ',  61,   2,   0,   3,       ! P/P 
C-----------------------------------------------------------------------
C   11 = PSME/PHMA-BLUE                                  p. 83         
C    Douglas-fir/ninebark                                R6 E TP-036-92
C
     &'CDS711  ','PIPO  ', 343,'PP  ',  87,   3,   0,  10,       ! P/P
     &'CDS711  ','PSME  ', 281,'DF  ',  59,   3,   1,   3,       ! P/P
     &'CDS711  ','LAOC  ', 320,'WL  ',  64,   3,   0,   2/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=35,49) /
C-----------------------------------------------------------------------
C   12 = PSME/ACGL-PHMA                                  p. 339        
C    Douglas-fir/Rocky Mtn maple-ninebark                R6 E TP-255-86
C
     &'CDS722  ','PIPO  ', 351,'PP  ',  96,   2,   0,  10,       ! P/P
     &'CDS722  ','PSME  ', 346,'DF  ',  64,   2,   1,   3,       ! P/P
C-----------------------------------------------------------------------
C   13 = PSME/VAME-BLUE                                  p. 81         
C    Douglas-fir/big huckleberry  (Blue Mtns)            R6 E TP-036-92
C
     &'CDS821  ','PIPO  ', 241,'PP  ',  92,   2,   0,  10,       ! P/P
     &'CDS821  ','PSME  ', 229,'DF  ',  53,   2,   1,   3,       ! P/P
C-----------------------------------------------------------------------
C   14 = ABLA2/LIBO2                                     p. 268        
C    Subalpine fir/twinflower                            R6 E TP-255-86
C
     &'CEF221  ','LAOC  ', 348,'WL  ',  62,   4,   0,   2,       ! P/P
     &'CEF221  ','PICO  ', 333,'LP  ',  65,   4,   1,   7,       ! P/P
     &'CEF221  ','PIEN  ', 538,'ES  ',  67,   4,   0,   8,       ! P/P
     &'CEF221  ','ABLA2 ', 488,'AF  ',  40,   4,   0,   9,       ! P/P
C-----------------------------------------------------------------------
C   15 = ABLA2/STAM                                      p. 275        
C    Subalpine fir/twisted stalk                         R6 E TP-255-86
C
     &'CEF311  ','PICO  ', 346,'LP  ',  65,   4,   1,   7,       ! P/P
     &'CEF311  ','PIEN  ', 586,'ES  ',  69,   4,   0,   8,       ! P/P
     &'CEF311  ','ABGR  ',  0.,'GF  ',  57,   4,   0,   4,       !  /P
     &'CEF311  ','ABLA2 ', 443,'AF  ',  65,   4,   0,   9,       ! P/P
C
C-----------------------------------------------------------------------
C   16 = ABLA2/TRCA3-BLUE                                p. 25
C    Subalpine fir/false bugbane (Blue Mtns)             R6 E TP-036-92
C
     &'CEF331  ','PICO  ', 346,'LP  ',  65,   3,   1,   7,       ! P/P
     &'CEF331  ','PIEN  ', 430,'ES  ',  60,   3,   0,   8,       ! P/P
     &'CEF331  ','ABLA2 ', 478,'AF  ',  0.,   3,   0,   9/       ! P/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=50,65) /
C----------------------------------------------------------------------
C   17 = ABLA2/POPU                                     XXXXXXX        
C    Subalpine fir/Jacob's-ladder                       XXXXXXXXXXXXXXX
C
     &'CEF411  ','PSME  ', 475,'DF  ',  59,   6,   0,   3,       ! P/P
     &'CEF411  ','LAOC  ', 513,'WL  ',  0.,   6,   0,   2,       ! P/
     &'CEF411  ','PICO  ', 346,'LP  ',  65,   6,   1,   7,       ! P/P
     &'CEF411  ','PIEN  ', 568,'ES  ',  58,   6,   0,   8,       ! P/P
     &'CEF411  ','ABGR  ',  0.,'GF  ',  54,   6,   0,   4,       !  /P
     &'CEF411  ','ABLA2 ', 483,'AF  ',  54,   6,   0,   9,       ! P/P
C-----------------------------------------------------------------------
C   18 = PIEN/CAEU                            GBA: 230   p. 55
C    Engelmann spruce/widefruit sedge                    R6 E TP-279-87
C
     &'CEM111  ','PIEN  ', 635,'ES  ',  80,   1,   1,   8,       ! H/H
C-----------------------------------------------------------------------
C   19 = PIEN/EQAR-STRO                       GBA: 258   p. 57
C    Engelmann spruce/common horsetail-rosy twistedstalk R6 E TP-279-87
C
     &'CEM221  ','PIEN  ', 712,'ES  ',  90,   1,   1,   8,       ! H/H
C-----------------------------------------------------------------------
C   20 = PIEN/CLUN                            GBA: 305   p. 49
C    Engelmann spruce/queencup beadlily                  R6 E TP-279-87
C
     &'CEM222  ','PIEN  ', 842,'ES  ', 105,   1,   1,   8,       ! H/H
C-----------------------------------------------------------------------
C   21 = PIEN/VAOC2-FORB                      GBA: 233   p. 51
C    Engelmann spruce/bog blueberry/forb                 R6 E TP-279-87
C
     &'CEM311  ','PIEN  ', 643,'ES  ',  85,   1,   1,   8,       ! H/H
C-----------------------------------------------------------------------
C   22 = PIEN/VAOC2/CAEU                      GBA: 161   p. 53
C    Engelmann spruce/bog blueberry/widefruit sedge      R6 E TP-279-87
C
     &'CEM312  ','PIEN  ', 444,'ES  ',  76,   1,   1,   8,       ! H/H
C-----------------------------------------------------------------------
C   23 = ABLA2/CLUN                                      p. 262        
C    Subalpine fir/queen's cup                           R6 E TP-255-86
C
     &'CES131  ','PIPO  ', 379,'PP  ',  0.,   5,   0,  10,       ! P/
     &'CES131  ','LAOC  ', 414,'WL  ',  83,   5,   1,   2,       ! P/P
     &'CES131  ','PIEN  ', 586,'ES  ',  72,   5,   0,   8,       ! P/P
     &'CES131  ','ABGR  ', 681,'GF  ',  77,   5,   0,   4,       ! P/P
     &'CES131  ','ABLA2 ', 429,'AF  ',  69,   5,   0,   9/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=66,84) /
C-----------------------------------------------------------------------
C   24 = ABLA2/MEFE                                      p. 238        
C    Subalpine fir/fool's huckleberry                    R6 E TP-255-86
C
     &'CES221  ','PSME  ',  0.,'DF  ',  56,   4,   0,   3,       !  /P
     &'CES221  ','PICO  ', 346,'LP  ',  65,   4,   1,   7,       ! P/P
     &'CES221  ','PIEN  ', 460,'ES  ',  0.,   4,   0,   8,       ! P/
     &'CES221  ','ABLA2 ', 410,'AF  ',  0.,   4,   0,   9,       ! P/
C----------------------------------------------------------------------
C   25 = ABLA2/VAME-BLUE                                 p. 33         
C    Subalpine fir/big huckleberry (Blue Mtns)           R6 E TP-036-92
C
     &'CES311  ','LAOC  ', 478,'WL  ',  63,   5,   0,   2,       ! P/P
     &'CES311  ','PICO  ', 319,'LP  ',  0.,   5,   0,   7,       ! P/
     &'CES311  ','PIEN  ', 478,'ES  ',  58,   5,   0,   8,       ! P/P
     &'CES311  ','ABGR  ',  0.,'GF  ',  72,   5,   0,   4,       !  /P
     &'CES311  ','ABLA2 ', 331,'AF  ',  51,   5,   1,   9,       ! P/P
C----------------------------------------------------------------------
C   26 = ABLA2/CLUN-BLUE                                 p. 27         
C    Subalpine fir/queen's cup beadlily                  R6 E TP-036-92
C
     &'CES314  ','LAOC  ', 513,'WL  ',  79,   4,   1,   2,       ! P/P
     &'CES314  ','PIEN  ', 586,'ES  ',  69,   4,   0,   8,       ! P/P
     &'CES314  ','ABGR  ',  0.,'GF  ',  69,   4,   0,   4,       !  /P
     &'CES314  ','ABLA2 ', 520,'AF  ',  53,   4,   0,   9,       ! P/P
C----------------------------------------------------------------------
C   27 = ABLA2/VAME-WALLO                                p. 253        
C    Subalpine fir/big huckleberry (Wallowa)             R6 E TP-255-86
C
     &'CES315  ','PSME  ', 475,'DF  ',  55,   6,   0,   3,       ! P/P
     &'CES315  ','LAOC  ', 460,'WL  ',  62,   6,   0,   2,       ! P/P
     &'CES315  ','PICO  ', 346,'LP  ',  82,   6,   1,   7,       ! P/P
     &'CES315  ','PIEN  ', 573,'ES  ',  65,   6,   0,   8,       ! P/P
     &'CES315  ','ABGR  ',  0.,'GF  ',  55,   6,   0,   4,       !  /P
     &'CES315  ','ABLA2 ', 425,'AF  ',  63,   6,   0,   9/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=85,97) /
C---------------------------------------------------------------------
C   28 = ABLA2/VASC-BLUE                                 p. 35         
C    Subalpine fir/grouse huckleberry (Blue Mtns)        R6 E TP-036-92
C
     &'CES411  ','PSME  ', 458,'DF  ',  0.,   7,   0,   3,       ! P/
     &'CES411  ','LAOC  ', 475,'WL  ',  46,   7,   0,   2,       ! P/P
     &'CES411  ','PICO  ', 346,'LP  ',  66,   7,   1,   7,       ! P/P
     &'CES411  ','PIEN  ', 458,'ES  ',  53,   7,   0,   8,       ! P/P
     &'CES411  ','ABGR  ',  0.,'GF  ',  61,   7,   0,   4,       !  /P
     &'CES411  ','ABLA2 ', 456,'AF  ',  44,   7,   0,   9,       ! P/P
     &'CES411  ','PIAL  ',  0.,'WB  ',  19,   7,   0,  11,       !  /P
C---------------------------------------------------------------------
C   29 = ABLA2/LIBO2                                     p. 29         
C    Subalpine fir/twinflower                            R6 E TP-036-92
C
     &'CES414  ','PSME  ',  0.,'DF  ',  64,   6,   0,   3,       !  /P
     &'CES414  ','LAOC  ', 513,'WL  ',  58,   6,   0,   2,       ! P/P
     &'CES414  ','PICO  ',  0.,'LP  ',  66,   6,   0,   7,       !  /P
     &'CES414  ','PIEN  ', 474,'ES  ',  60,   6,   0,   8,       ! P/P
     &'CES414  ','ABGR  ',  0.,'GF  ',  52,   6,   0,   4,       !  /P
     &'CES414  ','ABLA2 ', 419,'AF  ',  53,   6,   1,   9/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=98,114) /
C---------------------------------------------------------------------
C   30 = ABLA2/VASC/POPU                                 p. 244        
C    Subalpine fir/grouse huckleberry/skunk-leaved polem R6 E TP-255-86
C
     &'CES415  ','PSME  ', 475,'DF  ',  0.,   6,   0,   3,       ! P/
     &'CES415  ','LAOC  ', 513,'WL  ',  51,   6,   0,   2,       ! P/P
     &'CES415  ','PICO  ', 346,'LP  ',  70,   6,   1,   7,       ! P/P
     &'CES415  ','PIEN  ', 568,'ES  ',  57,   6,   0,   8,       ! P/P
     &'CES415  ','ABGR  ',  0.,'GF  ',  51,   6,   0,   4,       !  /P
     &'CES415  ','ABLA2 ', 483,'AF  ',  48,   6,   0,   9,       ! P/P
C---------------------------------------------------------------------
C   31 = PICO/LIBO2                                      p. 305        
C    Lodgepole pine/twinflower                           R6 E TP-255-86  
C
     &'CLF211  ','LACO  ',  0.,'WL  ',  55,   2,   0,   2,       !  /P
     &'CLF211  ','PICO  ', 690,'LP  ',  72,   2,   1,   7,       ! C/P
C---------------------------------------------------------------------
C   32 = PICO/CARU-VASC                       GBA: 143   p. 34
C    Lodgepole pine/pinegrass-grouse huckleberry         R6 AG 3-1-73
C
     &'CLG211  ','PICO  ', 395,'LP  ',  39,   1,   1,   7,       ! H/H
C---------------------------------------------------------------------
C   33 = PICO/POPR                            GBA: 195   p. 29
C    Lodgepole pine/Kentucky bluegrass                   R6 E TP-279-87
C
     &'CLM112  ','PIPO  ', 538,'PP  ',  97,   1,   1,  10,       ! H/H
C---------------------------------------------------------------------
C   34 = PICO/CAEU                            GBA: 178   p. 41
C    Lodgepole pine/widefruit sedge                      R6 E TP-279-87
C
     &'CLM113  ','PICO  ', 491,'LP  ',  57,   1,   1,   7,       ! H/H
C---------------------------------------------------------------------
C   35 = PICO/CAAQ                            GBA: 199   p. 43
C    Lodgepole pine/Aquatic sedge                        R6 E TP-279-87
C
     &'CLM114  ','PICO  ', 549,'LP  ',  45,   1,   1,   7,       ! H/H
C----------------------------------------------------------------------
C   36 = PICO/VAOC2/CAEU                      GBA: 169   p. 39
C    Lodgepole pine/bog blueberry/widefruit sedge        R6 E TP-279-87
C
     &'CLM312  ','PICO  ', 466,'LP  ',  54,   1,   1,   7,       ! H/H
C----------------------------------------------------------------------
C   37 = PICO/SPDO/FORB                       GBA: 202   p. 33
C    Lodgepole pine/Douglas spiraea/forb                 R6 E TP-279-87
C
     &'CLM313  ','PICO  ', 558,'LP  ',  51,   1,   1,   7,       ! H/H
C----------------------------------------------------------------------
C   38 = PICO/SPDO/CAEU                       GBA: 188   p. 35
C    Lodgepole pine/Douglas spiraea/widefruit sedge      R6 E TP-279-87
C
     &'CLM314  ','PICO  ', 519,'LP  ',  59,   1,   1,   7,       ! H/H
C----------------------------------------------------------------------
C   39 = PICO-PIEN/ELPA2                                 p. 45
C    Lodgepole pine-Engelmann spruce/few-flow spikerush  R6 E TP-279-87
C
     &'CLM911  ','PICO  ', 495,'LP  ',  35,   1,   1,   7,       ! C/H
C----------------------------------------------------------------------
C   40 = PICO/VASC-BLUE                       GBA: 120   p. 36
C    Lodgepole pine/grouse huckleberry (Blue Mtns)       R6 AG 3-1-73
C
     &'CLS411  ','PICO  ', 331,'LP  ',  34,   1,   1,   7/       ! H/H
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=115,129) /
C---------------------------------------------------------------------
C   41 = PICO/VASC/POPU-WALLO                            p. 250
C    Lodgepole pine/grouse huckleberry/skunk-leav polem  R6 E TP-255-86
C
     &'CLS415  ','LAOC  ',  0.,'WL  ',  45,   4,   0,   2,       !  /P
     &'CLS415  ','PICO  ', 785,'LP  ',  61,   4,   1,   7,       ! C/P
     &'CLS415  ','PIEN  ',  0.,'ES  ',  52,   4,   0,   8,       !  /P
     &'CLS415  ','ABLA  ',  0.,'AF  ',  42,   4,   0,   9,       !  /P
C-----------------------------------------------------------------------
C   42 = PICO/CARU                                       p. 79         
C    Lodgepole pine/pinegrass                            R6 E TP-036-92
C
     &'CLS416  ','PIPO  ',  0.,'PP  ',  78,   4,   0,  10,       !  /P
     &'CLS416  ','PICO  ',  0.,'DF  ',  53,   4,   0,   3,       !  /P
     &'CLS416  ','LAOC  ',  0.,'WL  ',  55,   4,   0,   2,       !  /P
     &'CLS416  ','PICO  ', 279,'LP  ',  66,   4,   1,   7,       ! P/P
C---------------------------------------------------------------------
C   43 = PICO(ABGR)/VAME-LIBO2                           XXXXXXX        
C    Lodgepole pine/thinleaf huckleberry/pinegrass       XXXXXXXXXXXXXXX
C
     &'CLS5    ','PIPO  ', 456,'PP  ',  0.,   7,   0,  10,       ! P/
     &'CLS5    ','PSME  ', 475,'DF  ',  55,   7,   0,   3,       ! P/P
     &'CLS5    ','LAOC  ', 463,'WL  ',  52,   7,   0,   2,       ! P/P
     &'CLS5    ','PICO  ', 346,'LP  ',  67,   7,   1,   7,       ! P/P
     &'CLS5    ','PIEN  ', 499,'ES  ',  56,   7,   0,   8,       ! P/P
     &'CLS5    ','ABGR  ', 645,'GF  ',  52,   7,   0,   4,       ! P/P
     &'CLS5    ','ABLA  ', 466,'AF  ',  0.,   7,   0,   9/       ! P/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=130,146) /
C-----------------------------------------------------------------------
C   44 = PICO/VAME-BLUE                       GBA: 126   p. 35
C    Lodgepole pine/big huckleberry (Blue Mtns)          R6 AG 3-1-73
C
     &'CLS511  ','PICO  ', 348,'LP  ',  30,   1,   1,   7,       ! H/H
C----------------------------------------------------------------------
C   45 = PICO/VAME-WALLO                      GBA: 150   p. 259
C    Lodgepole pine/big huckleberry (Wallowa)            R6 E TP-255-86
C
     &'CLS515  ','LAOC  ',  0.,'WL  ',  46,   3,   0,   2,       !  /P
     &'CLS515  ','PICO  ', 414,'LP  ',  65,   3,   1,   7,       ! H/P
     &'CLS515  ','PIEN  ',  0.,'ES  ',  46,   3,   0,   8,       !  /P
C---------------------------------------------------------------------
C   46 = PICO(ABGR)/ALSI                                 XXXXXXX        
C    Lodgepole pine/Sitka alders                         XXXXXXXXXXXXXXX
C
     &'CLS6    ','PSME  ', 475,'DF  ',  0.,   5,   0,   3,       ! P/ 
     &'CLS6    ','LAOC  ', 513,'WL  ',  59,   5,   0,   2,       ! P/P
     &'CLS6    ','PICO  ', 346,'LP  ',  65,   5,   1,   7,       ! P/P
     &'CLS6    ','PIEN  ', 586,'ES  ',  0.,   5,   0,   8,       ! P/ 
     &'CLS6    ','ABGR  ', 700,'GF  ',  0.,   5,   0,   4,       ! P/ 
C---------------------------------------------------------------------
C   47 = TSME/VASC-WALLO                                 p. 230
C    Mountain hemlock/grouse huckleberry (Wallowa)       R6 E TP-255-86
C
     &'CMS131  ','PICO  ', 283,'LP  ',  68,   4,   1,   7,       ! P/P
     &'CMS131  ','PIEN  ', 371,'ES  ',  0.,   4,   0,   8,       ! P/
     &'CMS131  ','ABLA  ', 520,'AF  ',  0.,   4,   0,   9,       ! P/
     &'CMS131  ','TSME  ', 610,'MH  ',  16,   4,   0,   5,       ! C/P
C----------------------------------------------------------------------
C   48 = TSME/VAME-WALLO                                 p. 230         
C    Mountain hemlock/big huckleberry (Wallowa)          R6 E TP-255-86
C
     &'CMS231  ','PICO  ', 283,'LP  ',  68,   4,   1,   7,       ! P/P
     &'CMS231  ','PIEN  ', 371,'ES  ',  0.,   4,   0,   8,       ! P/ 
     &'CMS231  ','ABLA  ', 520,'AF  ',  0.,   4,   0,   9,       ! P/ 
     &'CMS231  ','TSME  ', 745,'MH  ',  15,   4,   0,   5/       ! C/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=147,164) /
C----------------------------------------------------------------------
C   49 = PIPO/AGSP-BLUE                                  p. 121        
C    Ponderosa pine/bluebunch wheatgrass (Blue Mtns)     R6 E TP-036-92
C
     &'CPG111  ','PIPO  ', 166,'PP  ',  72,   3,   1,  10,       ! P/P
     &'CPG111  ','PSME  ',  0.,'DF  ',  52,   3,   0,   3,       !  /P
     &'CPG111  ','ABGR  ',  0.,'GF  ',  69,   3,   0,   4,       !  /P
C----------------------------------------------------------------------
C   50 = PIPO/FEID-BLUE                                  p. 119        
C    Ponderosa pine/Idaho fescue (Blue Mtns)             R6 E TP-036-92
C
     &'CPG112  ','PIPO  ', 243,'PP  ',  74,   2,   1,  10,       ! P/P
     &'CPG112  ','PSME  ',  0.,'DF  ',  59,   2,   0,   3,       !  /P
C----------------------------------------------------------------------
C   51 = PIPO/FEID-WALLO                                 p. 378        
C    Ponderosa pine/Idaho fescue (Wallowa)               R6 E TP-255-86
C
     &'CPG131  ','PIPO  ', 259,'PP  ',  79,   2,   1,  10,       ! P/P
     &'CPG131  ','PSME  ',  0.,'DF  ',  57,   2,   0,   3,       !  /P
C----------------------------------------------------------------------
C   52 = PIPO-AGSP-WALLO                                 p. 383        
C    Ponderosa pine/bluebunch wheatgrass (Wallowa)       R6 E TP-255-86
C
     &'CPG132  ','PIPO  ', 233,'PP  ',  77,   2,   1,  10,       ! P/P
     &'CPG132  ','PSME  ',  0.,'DF  ',  62,   2,   0,   3,       !  /P
C----------------------------------------------------------------------
C   53 = PIPO/CARU                                       p. 107        
C    Ponderosa pine/pinegrass                            R6 E TP-036-92
C
     &'CPG221  ','PIPO  ', 456,'PP  ',  77,   3,   1,  10,       ! P/P
     &'CPG221  ','PSME  ',  0.,'DF  ',  55,   3,   0,   3,       !  /P
     &'CPG221  ','ABGR  ',  0.,'GF  ',  66,   3,   0,   4,       !  /P
C----------------------------------------------------------------------
C   54 = PIPO/CAGE                                       p. 109        
C    Ponderosa pine/elk sedge                            R6 E TP-036-92
C
     &'CPG222  ','PIPO  ', 251,'PP  ',  73,   3,   1,  10,       ! P/P
     &'CPG222  ','PSME  ',  0.,'DF  ',  51,   3,   0,   3,       !  /P
     &'CPG222  ','PICO  ',  0.,'LP  ',  70,   3,   0,   7,       !  /P
C----------------------------------------------------------------------
C   55 = PIPO/ELGL                            GBA:  85   p. 28
C    Ponderosa pine/blue wildrye                         R6 AG 3-1-73
C
     &'CPM111  ','PIPO  ', 235,'PP  ',  80,   1,   1,  10,       ! H/H
C----------------------------------------------------------------------
C   56 = PIPO/ARTR/FEID-AGSP                             p. 117        
C    Pon pine/mtn big sagebrush/ID fescue-bluebunch whe  R6 E TP-036-92
C
     &'CPS131  ','PIPO  ', 238,'PP  ',  73,   1,   1,  10,       ! P/P
C----------------------------------------------------------------------
C   57 = PIPO/PUTR/CARO                                  p. 111        
C    Ponderosa pine/bitterbrush/Ross' sedge              R6 E TP-036-92
C
     &'CPS221  ','PIPO  ', 304,'PP  ',  74,   1,   1,  10/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=165,179) /

C----------------------------------------------------------------------
C   58 = PIPO/PUTR/CAGE                                  p. 113        
C    Ponderosa pine/bitterbrush/elk sedge                R6 E TP-036-92
C
     &'CPS222  ','PIPO  ', 255,'PP  ',  79,   1,   1,  10,       ! P/P
C----------------------------------------------------------------------
C   59 = PIPO/PUTR/FEID-AGSP                             p. 115        
C    Ponderosa pine/bitterbrush/ID fescue-bluebunch whea R6 E TP-036-92
C
     &'CPS226  ','PIPO  ', 231,'PP  ',  64,   1,   1,  10,       ! P/P
C----------------------------------------------------------------------
C   60 = PIPO/CELE/CAGE                                  p. 97         
C    Ponderosa pine/mountain-mahogany/elk sedge          R6 E TP-036-92
C
     &'CPS232  ','PIPO  ', 290,'PP  ',  65,   2,   1,  10,       ! P/P
     &'CPS232  ','PSME  ',  0.,'DF  ',  53,   2,   0,   3,       !  /P
C---------------------------------------------------------------------
C   61 = PIPO/CELE/PONE                                  p. 99         
C    Ponderosa pine/mtn-mahogany/Wheeler's bluegrass     R6 E TP-036-92
C
     &'CPS233  ','PIPO  ', 199,'PP  ',  67,   1,   1,  10,       ! P/P
C---------------------------------------------------------------------
C   62 = PIPO/CELE/FEID-AGSP                             p. 101        
C    Ponderosa pine/mtn-mahogany/ID fescue-bluebunch whe R6 E TP-036-92
C
     &'CPS234  ','PIPO  ', 196,'PP  ',  66,   2,   1,  10,       ! P/P
     &'CPS234  ','PSME  ',  0.,'DF  ',  51,   2,   0,   3,       !  /P
C---------------------------------------------------------------------
C   63 = PIPO/SYAL-FLOOD                      GBA: 187   p. 27
C    Ponderosa pine/common snowberry-floodplain          R6 E TP-279-87
C
     &'CPS511  ','PIPO  ', 516,'PP  ', 101,   1,   1,  10,       ! H/H
C---------------------------------------------------------------------
C   64 = PIPO/SYAL-WALLO                                 p. 372        
C    Ponderosa pine/common snowberry (Wallowa)           R6 E TP-255-86
C
     &'CPS522  ','PIPO  ', 301,'PP  ',  85,   2,   1,  10,       ! P/P
     &'CPS522  ','PSME  ',  0.,'DF  ',  70,   2,   0,   3,       !  /P
C-------------------------------------------------------------------
C   65 = PIPO/SPBE                                       p. 377        
C    Ponderosa pine/spiraea                              R6 E TP-255-86
C
     &'CPS523  ','PIPO  ', 276,'PP  ',  96,   2,   1,  10,       ! P/P
     &'CPS523  ','PSME  ',  0.,'DF  ',  71,   2,   0,   3,       !  /P
C-----------------------------------------------------------------------
C   66 = PIPO/SYAL                                       p. 103        
C    Ponderosa pine/common snowberry                     R6 E TP-036-92
C
     &'CPS524  ','PIPO  ', 398,'PP  ',  81,   2,   1,  10,       ! P/P
     &'CPS524  ','PSME  ',  0.,'DF  ',  56,   2,   0,   3,       !  /P
C-----------------------------------------------------------------------
C   67 = PIPO/SYOR                                       p. 105        
C    Ponderosa pine/mountain snowberry                   R6 E TP-036-92
C
     &'CPS525  ','PIPO  ', 325,'PP  ',  79,   1,   1,  10/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=180,191) /

C----------------------------------------------------------------------
C   68 = ABGR/TABR/CLUN                                  p. 51         
C    Grand fir/Pacific yew/queen's cup beadlily          R6 E TP-036-92
C
     &'CWC811  ','PIEN  ', 533,'ES  ',  76,   2,   1,   8,       ! P/P
     &'CWC811  ','ABGR  ', 700,'GF  ',  69,   2,   0,   4,       ! P/P
C----------------------------------------------------------------------
C   69 = ABGR/TABR/LIBO2                                 p. 53         
C    Grand fir/Pacific yew/twinflower                    R6 E TP-036-92
C
     &'CWC812  ','PSME  ', 475,'DF  ',  76,   4,   0,   3,       ! P/P
     &'CWC812  ','LAOC  ', 378,'WL  ',  0.,   4,   0,   2,       ! P/
     &'CWC812  ','PIEN  ', 374,'ES  ',  66,   4,   1,   8,       ! P/P
     &'CWC812  ','ABGR  ', 700,'GF  ',  90,   4,   0,   4,       ! P/P
C---------------------------------------------------------------------
C   70 = ABGR/LIBO2                                      p. 298        
C    Grand fir/twinflower                                R6 E TP-255-86
C
     &'CWF311  ','PIPO  ',  0.,'PP  ', 104,   6,   0,  10,       !  /P
     &'CWF311  ','PSME  ', 475,'DF  ',  60,   6,   0,   3,       ! P/P
     &'CWF311  ','LAOC  ', 511,'WL  ',  60,   6,   0,   2,       ! P/P
     &'CWF311  ','PICO  ', 346,'LP  ',  73,   6,   1,   7,       ! P/P
     &'CWF311  ','PIEN  ',  0.,'ES  ',  59,   6,   0,   8,       !  /P
     &'CWF311  ','ABGR  ', 700,'GF  ',  59,   6,   0,   4/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=192,205) /
C---------------------------------------------------------------------
C   71 = ABGR/LIBO2-BLUE                                 p. 59         
C    Grand fir/twinflower (Blue Mtns)                    R6 E TP-036-92
C
     &'CWF312  ','PIPO  ', 456,'PP  ',  92,   7,   0,  10,       ! P/P
     &'CWF312  ','PSME  ', 475,'DF  ',  62,   7,   0,   3,       ! P/P
     &'CWF312  ','LAOC  ', 463,'WL  ',  58,   7,   0,   2,       ! P/P
     &'CWF312  ','PICO  ', 346,'LP  ',  72,   7,   1,   7,       ! P/P
     &'CWF312  ','PIEN  ', 499,'ES  ',  53,   7,   0,   8,       ! P/P
     &'CWF312  ','ABGR  ', 645,'GF  ',  56,   7,   0,   4,       ! P/P
     &'CWF312  ','ABLA  ', 466,'AF  ',  0.,   7,   0,   9,       ! P/ 
C---------------------------------------------------------------------
C   72 = ABGR/CLUN-WALLO                                 p. 279        
C    Grand fir/queen's cup (Wallowa)                     R6 E TP-255-86
C
     &'CWF421  ','PIPO  ', 456,'PP  ', 111,   7,   0,  10,       ! P/P
     &'CWF421  ','PSME  ', 475,'DF  ',  69,   7,   0,   3,       ! P/P
     &'CWF421  ','LAOC  ', 455,'WL  ',  79,   7,   0,   2,       ! P/P
     &'CWF421  ','PICO  ', 346,'LP  ',  81,   7,   1,   7,       ! P/P
     &'CWF421  ','PIEN  ', 586,'ES  ',  72,   7,   0,   8,       ! P/P
     &'CWF421  ','ABGR  ', 700,'GF  ',  74,   7,   0,   4,       ! P/P
     &'CWF421  ','PIM03 ',  0.,'WP  ',  40,   7,   0,   1/       !  /P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=206,220) /
C----------------------------------------------------------------------
C   73 = ABCO/CLUN                            GBA: 316   p. 47
C    White fir/queencup beadlily                         R6 E TP-279-87
C
     &'CWF431  ','PSME  ', 872,'DF  ',  77,   1,   1,   3,       ! H/H
C----------------------------------------------------------------------
C   74 = ABGR/TRCA3                                      p. 49         
C    Grand fir/false bugbane                             R6 E TP-036-92
C
     &'CWF512  ','PSME  ',  0.,'DF  ',  75,   4,   0,   3,       !  /P
     &'CWF512  ','LAOC  ', 498,'WL  ',  0.,   4,   0,   2,       ! P/
     &'CWF512  ','PIEN  ', 485,'ES  ',  72,   4,   1,   8,       ! P/P
     &'CWF512  ','ABGR  ', 693,'GF  ',  79,   4,   0,   4,       ! P/P
C---------------------------------------------------------------------
C   75 = ABGR/GYDR                                       p. 45         
C    Grand fir/oakfern                                   R6 E TP-036-92
C
     &'CWF611  ','ABGR  ', 691,'GF  ',  79,   1,   1,   4,       ! P/P
C---------------------------------------------------------------------
C   76 = ABGR/POMU-ASCA3                                 p. 47         
C    Grand fir/sword fern-ginger                         R6 E TP-036-92
C
     &'CWF612  ','LAOC  ', 438,'WL  ',  79,   3,   1,   2,       ! P/P
     &'CWF612  ','PIEN  ', 586,'ES  ',  0.,   3,   0,   8,       ! P/
     &'CWF612  ','ABGR  ', 608,'GF  ',  78,   3,   0,   4,       ! P/P
C
C---------------------------------------------------------------------
C   77 = ABGR/CAGE-BLUE                                  p. 73         
C    Grand fir/elk sedge (Blue mtns)                     R6 E TP-036-92
C
     &'CWG111  ','PIPO  ', 263,'PP  ',  81,   6,   1,  10,       ! P/P
     &'CWG111  ','PSME  ', 376,'DF  ',  56,   6,   0,   3,       ! P/P
     &'CWG111  ','LAOC  ',  0.,'WL  ',  64,   6,   0,   2,       !  /P
     &'CWG111  ','PICO  ',  0.,'LP  ',  70,   6,   0,   7,       !  /P
     &'CWG111  ','PIEN  ',  0.,'ES  ',  68,   6,   0,   8,       !  /P
     &'CWG111  ','ABGR  ', 700,'GF  ',  50,   6,   0,   4/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=221,234) /
C---------------------------------------------------------------------
C   78 = ABGR/CARU                                       p. 320        
C    Grand fir/pinegrass                                 R6 E TP-255-86
C
     &'CWG112  ','PIPO  ', 456,'PP  ',  90,   5,   1,  10,       ! P/P
     &'CWG112  ','PSME  ', 475,'DF  ',  60,   5,   0,   3,       ! P/P
     &'CWG112  ','LAOC  ',  0.,'WL  ',  55,   5,   0,   2,       !  /P
     &'CWG112  ','PIEN  ',  0.,'ES  ',  75,   5,   0,   8,       !  /P
     &'CWG112  ','ABGR  ',  0.,'GF  ',  56,   5,   0,   4,       !  /P
C---------------------------------------------------------------------
C   79 = ABGR/CARU-BLUE                                  p. 71         
C    Grand fir/pinegrass (Blue mtns)                     R6 E TP-036-92
C
     &'CWG113  ','PIPO  ', 395,'PP  ',  80,   5,   1,  10,       ! P/P
     &'CWG113  ','PSME  ', 446,'DF  ',  56,   5,   0,   3,       ! P/P
     &'CWG113  ','LAOC  ', 384,'WL  ',  59,   5,   0,   2,       ! P/P
     &'CWG113  ','PICO  ', 346,'LP  ',  76,   5,   0,   7,       ! P/P
     &'CWG113  ','ABGR  ', 555,'GF  ',  52,   5,   0,   4,       ! P/P
C-------------------------------------------------------------------
C   80 = ABGR/BRVU                                       p. 67         
C    Grand fir/Columbia brome                            R6 E TP-036-92
C
     &'CWG211  ','LAOC  ', 513,'WL  ',  79,   4,   1,   2,       ! P/P
     &'CWG211  ','PIEN  ', 586,'ES  ',  0.,   4,   0,   8,       ! P/
     &'CWG211  ','ABGR  ', 700,'GF  ',  57,   4,   0,   4,       ! P/P
     &'CWG211  ','ABLA  ',  0.,'AF  ',  55,   4,   0,   9/       !  /P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=235,251) /
C-----------------------------------------------------------------------
C   81 = ABGR/VAME                                       p. 290        
C    Grand fir/big hucklberry                            R6 E TP-255-86
C
     &'CWS211  ','PIPO  ', 424,'PP  ',  86,   6,   0,  10,       ! P/P
     &'CWS211  ','PSME  ', 439,'DF  ',  66,   6,   0,   3,       ! P/P
     &'CWS211  ','LAOC  ', 464,'WL  ',  84,   6,   0,   2,       ! P/P
     &'CWS211  ','PICO  ', 331,'LP  ',  54,   6,   1,   7,       ! P/P
     &'CWS211  ','PIEN  ', 586,'ES  ',  66,   6,   0,   8,       ! P/P
     &'CWS211  ','ABGR  ', 700,'GF  ',  61,   6,   0,   4,       ! P/P
C-----------------------------------------------------------------------
C   82 = ABGR/VAME-BLUE                                  p. 61         
C    Grand fir/big huckleberry                           R6 E TP-036-92
C
     &'CWS212  ','PIPO  ', 365,'PP  ',  79,   7,   0,  10,       ! P/P
     &'CWS212  ','PSME  ', 475,'DF  ',  61,   7,   0,   3,       ! P/P
     &'CWS212  ','LAOC  ', 513,'WL  ',  57,   7,   0,   2,       ! P/P
     &'CWS212  ','PICO  ', 298,'LP  ',  68,   7,   1,   7,       ! P/P
     &'CWS212  ','PIEN  ', 426,'ES  ',  67,   7,   0,   8,       ! P/P
     &'CWS212  ','ABGR  ', 569,'GF  ',  60,   7,   0,   4,       ! P/P
     &'CWS212  ','ABLA  ', 515,'AF  ',  0.,   7,   0,   9,       ! P/
C----------------------------------------------------------------------
C   83 = ABGR/SPBE                                       p. 315        
C    Grand fir/spiraea                                   R6 E TP-255-86
C
     &'CWS321  ','PIPO  ', 456,'PP  ',  92,   4,   1,  10,       ! P/P
     &'CWS321  ','PSME  ', 475,'DF  ',  58,   4,   0,   3,       ! P/P
     &'CWS321  ','PICO  ',  0.,'LP  ',  74,   4,   0,   7,       !  /P
     &'CWS321  ','ABGR  ',  0.,'GF  ',  65,   4,   0,   4/       !  /P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=252,263) /
C----------------------------------------------------------------------
C   84 = ABGR/SPBE-BLUE                                  p. 69         
C    Grand fir/birchleaf spiraea                         R6 E TP-036-92
C
     &'CWS322  ','PIPO  ', 319,'PP  ',  82,   4,   0,  10,       ! P/P
     &'CWS322  ','PSME  ', 248,'DF  ',  57,   4,   1,   3,       ! P/P
     &'CWS322  ','PICO  ',  0.,'LP  ',  60,   4,   0,   7,       !  /P
     &'CWS322  ','ABGR  ', 443,'GF  ',  49,   4,   0,   4,       ! P/P
C---------------------------------------------------------------------
C   85 = ABGR/AGGL-PHMA                                  p. 325        
C    Grand fir/Rocky Mountain maple-ninebark             R6 E TP-255-86
C
     &'CWS412  ','PIPO  ',  0.,'PP  ', 107,   4,   0,  10,       !  /P
     &'CWS412  ','PSME  ', 475,'DF  ',  66,   4,   1,   3,       ! P/P
     &'CWS412  ','LAOC  ', 444,'WL  ',  79,   4,   0,   2,       ! P/P
     &'CWS412  ','ABGR  ', 628,'GF  ',  65,   4,   0,   4,       ! P/P
C---------------------------------------------------------------------
C   86 = ABGR/ACGL                                       p. 55         
C    Grand fir/Rocky Mountain maple                      R6 E TP-036-92
C
     &'CWS541  ','PSME  ', 301,'DF  ',  70,   4,   1,   3,       ! P/P
     &'CWS541  ','LAOC  ', 439,'WL  ',  0.,   4,   0,   2,       ! P/
     &'CWS541  ','PIEN  ', 405,'ES  ',  0.,   4,   0,   8,       ! P/
     &'CWS541  ','ABGR  ', 576,'GF  ',  71,   4,   0,   4/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=264,280) /
C---------------------------------------------------------------------
C   87 = ABGR/VASC                                       p. 65         
C    Grand fir/grouse huckleberry                        R6 E TP-036-92
C
     &'CWS811  ','PIPO  ', 215,'PP  ', 101,   6,   0,  10,       ! P/P
     &'CWS811  ','PSME  ', 343,'DF  ',  59,   6,   0,   3,       ! P/P
     &'CWS811  ','LAOC  ', 380,'WL  ',  61,   6,   0,   2,       ! P/P
     &'CWS811  ','PICO  ', 346,'LP  ',  65,   6,   1,   7,       ! P/P
     &'CWS811  ','PIEN  ',  0.,'ES  ',  43,   6,   0,   8,       ! P/P
     &'CWS811  ','ABGR  ', 460,'GF  ',  48,   6,   0,   4,       ! P/P
C----------------------------------------------------------------------
C   88 = ABGR/VASC-LIBO2                                 p. 63         
C    Grand fir/grouse huckleberry-twinflower             R6 E TP-036-92
C
     &'CWS812  ','PIPO  ',  0.,'PP  ',  81,   7,   0,  10,       !  /P
     &'CWS812  ','PSME  ', 434,'DF  ',  56,   7,   0,   3,       ! P/P
     &'CWS812  ','LAOC  ', 316,'WL  ',  56,   7,   1,   2,       ! P/P
     &'CWS812  ','PICO  ', 346,'LP  ',  75,   7,   0,   7,       ! P/P
     &'CWS812  ','PIEN  ', 436,'ES  ',  70,   7,   0,   8,       ! P/P
     &'CWS812  ','ABGR  ', 618,'GF  ',  56,   7,   0,   4,       ! P/P
     &'CWS812  ','ABLA  ', 230,'AF  ',  0.,   7,   0,   9,       ! P/
C----------------------------------------------------------------------
C   89 = ABGR/ACGL                                       p. 310        
C    Grand fir/Rocky Mountain maple                      R6 E TP-255-86
C
     &'CWS912  ','PIPO  ', 456,'PP  ',  0.,   4,   0,  10,       ! P/P
     &'CWS912  ','PSME  ', 475,'DF  ',  67,   4,   1,   3,       ! P/P
     &'CWS912  ','LAOC  ',  0.,'WL  ',  64,   4,   0,   2,       !  /P
     &'CWS912  ','ABGR  ', 700,'GF  ',  69,   4,   0,   4/       ! P/P
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=281,283) /
C---------------------------------------------------------------------
C   90 = POTR/ELGL                            GBA: 168   p. 61
C    Quaking aspen/blue wildrye                          R6 E TP-279-87
C
     &'HQM121  ','PICO  ', 464,'LP  ',  55,   1,   1,   7,       ! H/H
C---------------------------------------------------------------------
C   91 = POTR-PICO/SPDO/CAEU                  GBA: 232   p. 63
C    Q aspen-lodgepole pine/Doug spiraea/widefruit sedge R6 E TP-279-87
C
     &'HQM411  ','PICO  ', 640,'LP  ',  59,   1,   1,   7,       ! H/H
C---------------------------------------------------------------------
C   92 = POTR/SYAL/ELGL                       GBA: 216   p. 59
C    Quaking aspen/common snowberry/blue wildrye         R6 E TP-279-87
C
     &'HQS221  ','PIPO  ', 596,'PP  ', 101,   1,   1,  10/       ! H/H
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
