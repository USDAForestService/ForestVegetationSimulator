      SUBROUTINE ECOCLS(APASS,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
      IMPLICIT NONE
C----------
C CA $Id$
C----------
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
C  AN R6 ALL CVS DATA STUDY, JUNE 2008: 3,4,6-8,10,11,13,14,16-24,
C  26-28,30-32,36,38,39,41,43-48,50,53,56,58,59,61,62,65-90
C----------
      INTEGER ISEQ,INDEX,NUM,ISFLAG,NENTRY,I,K
      REAL RSI,RSDI
      PARAMETER (NENTRY=90)
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
C    1 =  CDC411 = PSME-ABCO-PIJE             GBA:       p.
C    Douglas-fir-white fir-Jeffrey pine
C
     &'CDC411  ','PSME  ', 899.,'DF  ',  85.,   1,   1,   7,
C-----------------------------------------------------------------------
C    2 =  CDC412 = PSME-ABCO-PIPO             GBA:       p.
C    Douglas-fir-white fir-ponderosa pine
C
     &'CDC412  ','PSME  ',1155.,'DF  ',  87.,   1,   1,   7,
C-----------------------------------------------------------------------
C    3 =  CDC421 = PSME-ABCO                  GBA:       p.
C    Douglas-fir-white fir
C
     &'CDC421  ','PSME  ', 720.,'DF  ',  72.,   1,   1,   7,
C-----------------------------------------------------------------------
C    4 =  CDC431 = PSME-ABCO/HODI             GBA:       p.
C    Douglas-fir-white fir/creambush oceanspray
C
     &'CDC431  ','PSME  ', 765.,'DF  ',  96.,   1,   1,   7,
C-----------------------------------------------------------------------
C    5 =  CDC432 = PSME-ABCO/BENE             GBA:       p.
C    Douglas-fir-white fir/dwarf Oregongrape
C
     &'CDC432  ','PSME  ',1193.,'DF  ',  93.,   1,   1,   7,
C-----------------------------------------------------------------------
C    6 =  CDC511 = PSME-PIPO                  GBA:       p.
C    Douglas-fir-ponderosa pine
C
     &'CDC511  ','PSME  ', 735.,'DF  ', 101.,   1,   1,   7,
C-----------------------------------------------------------------------
C    7 =  CDC521 = PSME-PIJE                  GBA:       p.
C    Douglas-fir-Jeffrey pine
C
     &'CDC521  ','PSME  ', 595.,'DF  ',  71.,   1,   1,   7,
C-----------------------------------------------------------------------
C    8 =  CDF911 = PSME/DEPAUPERATE           GBA:       p.
C    Douglas-fir/depauperate
C
     &'CDF911  ','PSME  ', 670.,'DF  ',  70.,   1,   1,   7,
C-----------------------------------------------------------------------
C    9 =  CDH111 = PSME-LIDE3/GASH            GBA:       p.
C    Douglas-fir-tanoak/salal
C
     &'CDH111  ','PSME  ', 845.,'DF  ',  86.,   1,   1,   7,
C-----------------------------------------------------------------------
C   10 =  CDH112 = PSME/RHMA                  GBA:       p.
C    Douglas-fir/Pacific rhododendron
C
     &'CDH112  ','PSME  ', 800.,'DF  ',  92.,   1,   1,   7,
C-----------------------------------------------------------------------
C   11 =  CDH121 = PSME-LIDE3-PILA            GBA:       p.
C    Douglas-fir-tanoak-sugar pine
C
     &'CDH121  ','PSME  ', 720.,'DF  ',  97.,   1,   1,   7,
C-----------------------------------------------------------------------
C   12 =  CDH131 = PSME-LIDE3                 GBA:       p.
C    Douglas-fir-tanoak
C
     &'CDH131  ','PSME  ',1098.,'DF  ',  81.,   1,   1,   7,
C-----------------------------------------------------------------------
C   13 =  CDH141 = PSME-LIDE3-QUCH            GBA:       p.
C    Douglas-fir-tanoak-canyon live oak
C
     &'CDH141  ','PSME  ', 780.,'DF  ',  86.,   1,   1,   7,
C-----------------------------------------------------------------------
C   14 =  CDH142 = PSME-LIDE3/RHDI            GBA:       p.
C    Douglas-fir-tanoak/poison oak
C
     &'CDH142  ','PSME  ',1050.,'DF  ',  82.,   1,   1,   7,
C-----------------------------------------------------------------------
C   15 =  CDH511 = PSME-QUSA                  GBA:       p.
C    Douglas-fir-Sadler oak
C
     &'CDH511  ','PSME  ',1087.,'DF  ',  95.,   1,   1,   7/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=16,30) /
C-----------------------------------------------------------------------
C   16 =  CDS111 = PSME/RHDI-BEPI             GBA:       p.
C    Douglas-fir/poison oak-Piper's Oregongrape
C
     &'CDS111  ','PSME  ', 655.,'DF  ',  77.,   1,   1,   7,
C-----------------------------------------------------------------------
C   17 =  CDS112 = PSME/RHDI                  GBA:       p.
C    Douglas-fir/poison oak
C
     &'CDS112  ','PSME  ', 630.,'DF  ',  67.,   1,   1,   7,
C-----------------------------------------------------------------------
C   18 =  CDS511 = PSME/BENE                  GBA:       p.
C    Douglas-fir/dwarf Oregongrape
C
     &'CDS511  ','PSME  ', 635.,'DF  ',  93.,   1,   1,   7,
C-----------------------------------------------------------------------
C   19 =  CDS521 = PSME/BERE                  GBA:       p.
C    Douglas-fir/creeping Oregongrape
C
     &'CDS521  ','PSME  ', 670.,'DF  ',  85.,   1,   1,   7,
C-----------------------------------------------------------------------
C   20 =  CHC111 = TSHE-CHLA                  GBA:       p.
C    Western hemlock-Port-Orford-cedar
C
     &'CHC111  ','PSME  ',1215.,'DF  ', 117.,   1,   1,   7,
C-----------------------------------------------------------------------
C   21 =  CHC412 = TSHE-THPL/HIGH ELEV        GBA:       p.
C    Western hemlock-western redcedar/high elevation
C
     &'CHC412  ','PSME  ', 945.,'DF  ', 108.,   1,   1,   7,
C-----------------------------------------------------------------------
C   22 =  CHC461 = TSHE-THPL                  GBA:       p.
C    Western hemlock-western redcedar
C
     &'CHC461  ','PSME  ',1105.,'DF  ', 146.,   1,   1,   7,
C-----------------------------------------------------------------------
C   23 =  CHC611 = TSHE-ABCO                  GBA:       p.
C    Western hemlock-white fir
C
     &'CHC611  ','PSME  ', 890.,'DF  ', 119.,   1,   1,   7,
C-----------------------------------------------------------------------
C   24 =  CHH111 = TSHE-UMCA                  GBA:       p.
C    Western hemlock-California laurel
C
     &'CHH111  ','PSME  ', 650.,'DF  ', 106.,   1,   1,   7,
C-----------------------------------------------------------------------
C   25 =  CHH511 = TSHE-QUSA                  GBA:       p.
C    Western hemlock-Sadler oak
C
     &'CHH511  ','PSME  ',1152.,'DF  ', 108.,   1,   1,   7,
C-----------------------------------------------------------------------
C   26 =  CHS131 = TSHE/GASH (SWO)            GBA:       p.
C    Western hemlock/salal
C
     &'CHS131  ','PSME  ',1050.,'DF  ',  61.,   1,   1,   7,
C-----------------------------------------------------------------------
C   27 =  CHS331 = TSHE/RHMA (SWO)            GBA:       p.
C    Western hemlock/Pacific rhododendron
C
     &'CHS331  ','PSME  ',1145.,'DF  ', 102.,   1,   1,   7,
C-----------------------------------------------------------------------
C   28 =  CMF211 = TSME/POPU                  GBA:       p.
C    Mountain hemlock/skunkleaf polemonium
C
     &'CMF211  ','ABMAS ', 555.,'SH  ',  74.,   1,   1,   6,
C-----------------------------------------------------------------------
C   29 =  CPC411 = PIPO-PSME                  GBA:       p.
C    Ponderosa pine-Douglas-fir
C
     &'CPC411  ','PSME  ', 720.,'DF  ',  76.,   1,   1,   7,
C-----------------------------------------------------------------------
C   30 =  CPC511 = PIJE-PIMO                  GBA:       p.
C    Jeffrey pine-western white pine
C
     &'CPC511  ','PIJE  ', 420.,'JP  ',  52.,   1,   1,  15/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=31,45) /
C-----------------------------------------------------------------------
C   31 =  CPG141 = PIJE/FEID                  GBA:       p.
C    Jeffrey pine/Idaho fescue
C
     &'CPG141  ','PIJE  ', 200.,'JP  ',  57.,   1,   1,  15,
C-----------------------------------------------------------------------
C   32 =  CPH411 = PIJE-QUVA                  GBA:       p.
C    Jeffrey pine-huckleberry oak
C
     &'CPH411  ','PIJE  ', 470.,'JP  ',  60.,   1,   1,  15,
C-----------------------------------------------------------------------
C   33 =  CPS321 = PIJE/CEPU                  GBA:       p.
C    Jeffrey pine/dwarf ceanothus
C
     &'CPS321  ','PIJE  ', 364.,'JP  ',  58.,   1,   1,  15,
C-----------------------------------------------------------------------
C   34 =  CPS611 = PIJE/GRASS                 GBA:       p.
C    Jeffrey pine/grass
C
     &'CPS611  ','PIJE  ', 340.,'JP  ',  57.,   1,   1,  15,
C-----------------------------------------------------------------------
C   35 =  CQF111 = PIMO/XETE                  GBA:       p.
C    Western white pine/beargrass
C
     &'CQF111  ','ABCO  ', 436.,'WF  ',  33.,   1,   1,   4,
C-----------------------------------------------------------------------
C   36 =  CRF211 = ABMAS/POPU                 GBA:       p.
C    Shasta red fir/skunkleaf polemonium
C
     &'CRF211  ','ABMAS ', 675.,'SH  ',  57.,   1,   1,   6,
C-----------------------------------------------------------------------
C   37 =  CRF311 = ABMAS/SHEEP                GBA:       p.
C    Shasta red fir/sheep    (grazing destroyed understory plants)
C
     &'CRF311  ','ABMAS ', 319.,'SH  ',  50.,   1,   1,   6,
C-----------------------------------------------------------------------
C   38 =  CRH111 = ABMAS-QUSA                 GBA:       p.
C    Shasta red fir-Sadler oak
C
     &'CRH111  ','ABMAS ', 470.,'SH  ',  81.,   1,   1,   6,
C-----------------------------------------------------------------------
C   39 =  CRS211 = ABMAS/SYMO                 GBA:       p.
C    Shasta red fir/creeping snowberry
C
     &'CRS211  ','ABMAS ', 755.,'SH  ',  91.,   1,   1,   6,
C-----------------------------------------------------------------------
C   40 =  CTH111 = CHLA-QUVA                  GBA:       p.
C    Port-Orford-cedar-huckleberry oak
C
     &'CTH111  ','PSME  ',1309.,'DF  ',  87.,   1,   1,   7,
C-----------------------------------------------------------------------
C   41 =  CTH211 = CHLA-ACMA                  GBA:       p.
C    Port-Orford-cedar-bigleaf maple
C
     &'CTH211  ','PSME  ', 760.,'DF  ',  87.,   1,   1,   7,
C-----------------------------------------------------------------------
C   42 =  CTS111 = CHLA/BENE/ACTR             GBA:       p.
C    Port-Orford-cedar/dwarf Oregongrape/vanillaleaf
C
     &'CTS111  ','PSME  ',1348.,'DF  ',  85.,   1,   1,   7,
C-----------------------------------------------------------------------
C   43 =  CTS112 = CHLA/BENE/LIBOL            GBA:       p.
C    Port-Orford-cedar/dwarf Oregongrape/western twinflower
C
     &'CTS112  ','PSME  ', 370.,'DF  ',  92.,   1,   1,   7,
C-----------------------------------------------------------------------
C   44 =  CTS211 = CHLA/GASH                  GBA:       p.
C    Port-Orford-cedar/salal
C
     &'CTS211  ','PSME  ', 990.,'DF  ',  83.,   1,   1,   7,
C-----------------------------------------------------------------------
C   45 =  CTS311 = CHLA/GABU                  GBA:       p.
C    Port-Orford-cedar/box-leaved silktassle
C
     &'CTS311  ','PSME  ', 660.,'DF  ',  87.,   1,   1,   7/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=46,60) /
C-----------------------------------------------------------------------
C   46 =  CWC221 = ABCO-PSME                  GBA:       p.
C    White fir-Douglas-fir
C
     &'CWC221  ','PSME  ', 815.,'DF  ',  92.,   1,   1,   7,
C-----------------------------------------------------------------------
C   47 =  CWC231 = ABCO-PSME/BENE             GBA:       p.
C    White fir-Douglas-fir/dwarf Oregongrape
C
     &'CWC231  ','PSME  ', 785.,'DF  ',  95.,   1,   1,   7,
C-----------------------------------------------------------------------
C   48 =  CWC232 = ABCO-PSME/HODI             GBA:       p.
C    White fir-Douglas-fir/creambush oceanspray
C
     &'CWC232  ','PSME  ', 675.,'DF  ',  89.,   1,   1,   7,
C-----------------------------------------------------------------------
C   49 =  CWC233 = ABCO-PSME/DEPAUPERATE      GBA:       p.
C    White fir-Douglas-fir/depauperate
C
     &'CWC233  ','PSME  ', 988.,'DF  ',  78.,   1,   1,   7,
C-----------------------------------------------------------------------
C   50 =  CWC241 = ABCO-PIPO                  GBA:       p.
C    White fir-ponderosa pine
C
     &'CWC241  ','PSME  ', 930.,'DF  ',  84.,   1,   1,   7,
C-----------------------------------------------------------------------
C   51 =  CWC521 = ABCO-PIBR/VAME             GBA:       p.
C    White fir-Brewer spruce/thin-leaved huckleberry
C
     &'CWC521  ','PSME  ', 899.,'DF  ',  57.,   1,   1,   7,
C-----------------------------------------------------------------------
C   52 =  CWC522 = ABCO-PIBR/GAOV             GBA:       p.
C    White fir-Brewer spruce/slender salal
C
     &'CWC522  ','PSME  ', 874.,'DF  ',  95.,   1,   1,   7,
C-----------------------------------------------------------------------
C   53 =  CWC523 = ABCO-PIBR/CHUM             GBA:       p.
C    White fir-Brewer spruce/western prince's-pine
C
     &'CWC523  ','PSME  ', 335.,'DF  ',  69.,   1,   1,   7,
C-----------------------------------------------------------------------
C   54 =  CWC611 = ABCO-CHLA                  GBA:       p.
C    White fir-Port-Orford-cedar
C
     &'CWC611  ','PSME  ',1399.,'DF  ',  99.,   1,   1,   7,
C-----------------------------------------------------------------------
C   55 =  CWC612 = ABCO-CHLA/DEPAUPERATE      GBA:       p.
C    White fir-Port-Orford-cedar/depauperate
C
     &'CWC612  ','PSME  ',1399.,'DF  ',  99.,   1,   1,   7,
C-----------------------------------------------------------------------
C   56 =  CWC721 = ABCO-ABMAS/RIBES           GBA:       p.
C    White fir-Shasta red fir/currant
C
     &'CWC721  ','ABCO  ', 665.,'WF  ',  77.,   1,   1,   4,
C-----------------------------------------------------------------------
C   57 =  CWC722 = ABCO-ABMAS/ROGY            GBA:       p.
C    White fir-Shasta red fir/baldhip rose
C
     &'CWC722  ','PSME  ',1349.,'DF  ',  89.,   1,   1,   7,
C-----------------------------------------------------------------------
C   58 =  CWC723 = ABCO-ABMAS/SYMO            GBA:       p.
C    White fir-Shasta red fir/creeping snowberry
C
     &'CWC723  ','PSME  ', 945.,'DF  ',  81.,   1,   1,   7,
C-----------------------------------------------------------------------
C   59 =  CWC811 = ABCO-TABR                  GBA:       p.
C    White fir-Pacific yew
C
     &'CWC811  ','PSME  ', 695.,'DF  ',  96.,   1,   1,   7,
C-----------------------------------------------------------------------
C   60 =  CWC911 = ABCO-CHNO                  GBA:       p.
C    White fir-Alaska cedar
C
     &'CWC911  ','ABCO  ',1641.,'WF  ',  65.,   1,   1,   4/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=61,75) /
C-----------------------------------------------------------------------
C   61 =  CWF911 = ABCO/HERB                  GBA:       p.
C    White fir/herb
C
     &'CWF911  ','PSME  ', 670.,'DF  ',  89.,   1,   1,   7,
C-----------------------------------------------------------------------
C   62 =  CWH312 = ABCO-LIDE3                 GBA:       p.
C    White fir-tanoak
C
     &'CWH312  ','PSME  ', 815.,'DF  ',  93.,   1,   1,   7,
C-----------------------------------------------------------------------
C   63 =  CWH413 = ABCO-ACGL                  GBA:       p.
C    White fir-Rocky Mountain maple
C
     &'CWH413  ','PSME  ', 654.,'DF  ', 108.,   1,   1,   7,
C-----------------------------------------------------------------------
C   64 =  CWH511 = ABCO-QUSA/CHUM             GBA:       p.
C    White fir-Sadler oak/western prince's-pine
C
     &'CWH511  ','PSME  ',1337.,'DF  ',  93.,   1,   1,   7,
C-----------------------------------------------------------------------
C   65 =  CWH521 = ABCO-QUSA/BENE-PAMY        GBA:       p.
C    White fir-Sadler oak/dwarf Oregongrape-Oregon boxwood
C
     &'CWH521  ','PSME  ', 470.,'DF  ',  96.,   1,   1,   7,
C-----------------------------------------------------------------------
C   66 =  CWH522 = ABCO-QUSA/BENE             GBA:       p.
C    White fir-Sadler oak/dwarf Oregongrape
C
     &'CWH522  ','PSME  ', 560.,'DF  ', 105.,   1,   1,   7,
C-----------------------------------------------------------------------
C   67 =  CWH531 = ABCO-QUSA-CACH             GBA:       p.
C    White fir-Sadler oak-golden chinquapin
C
     &'CWH531  ','PSME  ', 810.,'DF  ',  94.,   1,   1,   7,
C-----------------------------------------------------------------------
C   68 =  CWS331 = ABCO/SYMO                  GBA:       p.
C    White fir/creeping snowberry
C
     &'CWS331  ','PSME  ', 695.,'DF  ',  92.,   1,   1,   7,
C-----------------------------------------------------------------------
C   69 =  CWS523 = ABCO/BENE                  GBA:       p.
C    White fir/dwarf Oregongrape
C
     &'CWS523  ','PSME  ', 900.,'DF  ', 101.,   1,   1,   7,
C-----------------------------------------------------------------------
C   70 =  HTC111 = LIDE3-SESE2                GBA:       p.
C    Tanoak-coast redwood
C
     &'HTC111  ','PSME  ', 820.,'DF  ', 125.,   1,   1,   7,
C-----------------------------------------------------------------------
C   71 =  HTC211 = LIDE3-TSHE                 GBA:       p.
C    Tanoak-western hemlock
C
     &'HTC211  ','PSME  ', 870.,'DF  ', 103.,   1,   1,   7,
C-----------------------------------------------------------------------
C   72 =  HTC311 = LIDE3-CHLA                 GBA:       p.
C    Tanoak-Port-Orford-cedar
C
     &'HTC311  ','PSME  ', 890.,'DF  ',  98.,   1,   1,   7,
C-----------------------------------------------------------------------
C   73 =  HTC411 = LIDE3-ABCO-ACCI            GBA:       p.
C    Tanoak-white fir-vine maple
C
     &'HTC411  ','PSME  ', 865.,'DF  ',  90.,   1,   1,   7,
C-----------------------------------------------------------------------
C   74 =  HTC412 = LIDE3-ABCO                 GBA:       p.
C    Tanoak-white fir
C
     &'HTC412  ','PSME  ', 970.,'DF  ',  99.,   1,   1,   7,
C-----------------------------------------------------------------------
C   75 =  HTH111 = LIDE3-QUCH                 GBA:       p.
C    Tanoak-canyon live oak
C
     &'HTH111  ','PSME  ', 735.,'DF  ',  96.,   1,   1,   7/
C
      DATA (PA(I),SCIEN(I),SDIMX(I),SPC(I),SITE(I),NUMBR(I),IFLAG(I),
     &     FVSSEQ(I),I=76,90) /
C-----------------------------------------------------------------------
C   76 =  HTH112 = LIDE3-QUCH/BENE            GBA:       p.
C    Tanoak-canyon live oak/dwarf Oregongrape
C
     &'HTH112  ','PSME  ', 650.,'DF  ',  83.,   1,   1,   7,
C-----------------------------------------------------------------------
C   77 =  HTH211 = LIDE3-UMCA                 GBA:       p.
C    Tanoak-California laurel
C
     &'HTH211  ','PSME  ', 810.,'DF  ', 110.,   1,   1,   7,
C-----------------------------------------------------------------------
C   78 =  HTH311 = LIDE3-ACCI                 GBA:       p.
C    Tanoak-vine maple
C
     &'HTH311  ','PSME  ', 595.,'DF  ', 104.,   1,   1,   7,
C-----------------------------------------------------------------------
C   79 =  HTS111 = LIDE3/VAOV2-GASH           GBA:       p.
C    Tanoak/evergreen huckleberry-salal
C
     &'HTS111  ','PSME  ', 910.,'DF  ', 107.,   1,   1,   7,
C-----------------------------------------------------------------------
C   80 =  HTS112 = LIDE3/VAOV2                GBA:       p.
C    Tanoak/evergreen huckleberry
C
     &'HTS112  ','PSME  ', 915.,'DF  ', 116.,   1,   1,   7,
C-----------------------------------------------------------------------
C   81 =  HTS221 = LIDE3/RHMA                 GBA:       p.
C    Tanoak/Pacific rhododendron
C
     &'HTS221  ','PSME  ', 830.,'DF  ', 111.,   1,   1,   7,
C-----------------------------------------------------------------------
C   82 =  HTS222 = LIDE3/RHMA-VAOV2           GBA:       p.
C    Tanoak/Pacific rhododendron-evergreen huckleberry
C
     &'HTS222  ','PSME  ', 815.,'DF  ',  93.,   1,   1,   7,
C-----------------------------------------------------------------------
C   83 =  HTS223 = LIDE3/RHMA-GASH            GBA:       p.
C    Tanoak/Pacific rhododendron-salal
C
     &'HTS223  ','PSME  ', 840.,'DF  ',  68.,   1,   1,   7,
C-----------------------------------------------------------------------
C   84 =  HTS311 = LIDE3/BENE                 GBA:       p.
C    Tanoak/dwarf Oregongrape
C
     &'HTS311  ','PSME  ', 805.,'DF  ',  95.,   1,   1,   7,
C-----------------------------------------------------------------------
C   85 =  HTS312 = LIDE3/BENE-RHDI            GBA:       p.
C    Tanoak/dwarf Oregongrape-poison oak
C
     &'HTS312  ','PSME  ', 785.,'DF  ',  96.,   1,   1,   7,
C-----------------------------------------------------------------------
C   86 =  HTS321 = LIDE3/GASH                 GBA:       p.
C    Tanoak/salal
C
     &'HTS321  ','PSME  ', 970.,'DF  ', 102.,   1,   1,   7,
C-----------------------------------------------------------------------
C   87 =  HTS331 = LIDE3/GASH-RHMA            GBA:       p.
C    Tanoak/salal-Pacific rhododendron
C
     &'HTS331  ','PSME  ', 610.,'DF  ',  90.,   1,   1,   7,
C-----------------------------------------------------------------------
C   88 =  HTS341 = LIDE3/GASH-BENE            GBA:       p.
C    Tanoak/salal-dwarf Oregongrape
C
     &'HTS341  ','PSME  ', 935.,'DF  ', 109.,   1,   1,   7,
C-----------------------------------------------------------------------
C   89 =  HTS411 = LIDE3/RHDI-LOHI            GBA:       p.
C    Tanoak/poison oak-hairy honeysuckle
C
     &'HTS411  ','PSME  ', 730.,'DF  ',  79.,   1,   1,   7,
C-----------------------------------------------------------------------
C   90 =  HTS511 = LIDE3/RHCA                 GBA:       p.
C    Tanoak/California coffeeberry
C
     &'HTS511  ','PSME  ', 450.,'DF  ',  50.,   1,   1,   7/
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
