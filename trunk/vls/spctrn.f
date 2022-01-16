      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C VLS $Id$
C----------
C  CALLED FROM INTREE, WHEN THE INPUT SPECIES CODE IS NOT RECONGIZED.
C  THIS ROUTINE ASSIGNS THE MOST SIMILAR SPECIES SEQUENCE NUMBER TO THE
C  SPECIES NOT RECOGNIZED IN THE VARIANT.  TRANSLATES FVS ALPHA CODES,
C  FIA CODES, AND USDA PLANTS SYMBOLS.  THE ASPT ARRAY CONTAINS THE 
C  SPECIES MAPPING.  ALL SPECIES MAPPING WAS APPROVED BY THE FVS 
C  REGIONAL CONTACTS. TO PROVIDE ADEQUATE CODE DOCUMENTATION,
C  STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
C  THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
C  END OF VALID FORTRAN STATEMENTS.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS
C
      CHARACTER*(*)SPCIN
      INTEGER MAXASPT, ISPC1, I, J, J2, IJSPIN
      PARAMETER (MAXASPT=562)
      CHARACTER*3 SPCOUT
      CHARACTER*8 ASPT(MAXASPT,7)
      CHARACTER VAR*2
C----------
C  DATA STATEMENT
C----------
C
      DATA ((ASPT(I,J),J=1,7),I=1,10) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'FR ','010','ABIES   ','OS ','BF ','BF ','FR ',    !Abies
     & 'BF ','012','ABBA    ','OS ','BF ','BF ','FR ',    !Abies balsamea
     & '   ','016','ABFR    ','OS ','BF ','BF ','FR ',    !Abies fraseri
     & '   ','300','ACACI   ','OH ','OH ','OH ','OH ',    !Acacia
     & '   ','303','ACFA    ','OH ','OH ','OH ','OH ',    !Acacia farnesiana
     & '   ','304','ACGR    ','OH ','OH ','OH ','OH ',    !Acacia greggii
     & 'MP ','310','ACER    ','RM ','SM ','SM ','RM ',    !Acer
     & 'FM ','311','ACBA3   ','SM ','SM ','SM ','FM ',    !Acer barbatum
     & '   ','   ','ACCA5   ','BE ','BE ','BE ','BE ',    !Acer campestre
     & '   ','   ','ACGI    ','RM ','RM ','RM ','RM '/    !Acer ginnala
C
      DATA ((ASPT(I,J),J=1,7),I=11,20) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','322','ACGR3   ','BE ','MM ','BE ','BE ',    !Acer grandidentatum
     & 'CM ','323','ACLE    ','SM ','SM ','SM ','SM ',    !Acer leucoderme
     & 'BE ','313','ACNE2   ','BE ','BE ','BE ','BE ',    !Acer negundo
     & '   ','   ','ACNEN   ','BE ','BE ','BE ','BE ',    !Acer negundo var. negundo
     & '   ','   ','ACNET   ','BE ','BE ','BE ','BE ',    !Acer negundo var. texanum
     & '   ','   ','ACNEV   ','BE ','BE ','BE ','BE ',    !Acer negundo var. violaceum
     & 'BM ','314','ACNI5   ','SM ','BM ','BM ','SM ',    !Acer nigrum
     & '   ','   ','ACPA2   ','RM ','RM ','RM ','RM ',    !Acer palmatum
     & 'ST ','315','ACPE    ','BE ','ST ','ST ','BE ',    !Acer pensylvanicum
     & '   ','320','ACPL    ','SM ','SM ','SM ','SM '/    !Acer platanoides
C
      DATA ((ASPT(I,J),J=1,7),I=21,30) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','ACPS    ','SM ','SM ','SM ','SM ',    !Acer pseudoplatanus
     & 'RM ','316','ACRU    ','RM ','RM ','RM ','RM ',    !Acer rubrum
     & '   ','   ','ACRUD   ','RM ','RM ','RM ','RM ',    !Acer rubrum var. drummondii
     & '   ','   ','ACRUR   ','RM ','RM ','RM ','RM ',    !Acer rubrum var. rubrum
     & '   ','   ','ACRUT   ','RM ','RM ','RM ','RM ',    !Acer rubrum var. trilobum
     & 'SV ','317','ACSA2   ','SV ','SV ','SV ','SV ',    !Acer saccharinum
     & 'SM ','318','ACSA3   ','SM ','SM ','SM ','SM ',    !Acer saccharum
     & '   ','   ','ACSAS   ','SM ','SM ','SM ','SM ',    !Acer saccharum var. saccharum
     & '   ','   ','ACSAS2  ','SM ','SM ','SM ','SM ',    !Acer saccharum var. schneckii
     & 'MM ','319','ACSP2   ','BE ','MM ','BE ','BE '/    !Acer spicatum
C
      DATA ((ASPT(I,J),J=1,7),I=31,40) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'BU ','330','AESCU   ','OB ','OH ','BU ','BU ',    !Aesculus
     & 'YY ','332','AEFL    ','OB ','OH ','YY ','BU ',    !Aesculus flava
     & 'OB ','331','AEGL    ','OB ','OH ','BU ','BU ',    !Aesculus glabra
     & '   ','   ','AEGLG   ','OB ','OH ','BU ','BU ',    !Aesculus glabra
     & '   ','334','AEGLA   ','OB ','OH ','BU ','BU ',    !Aesculus glabra var. arguta
     & '   ','   ','AEHI    ','OB ','OH ','BU ','BU ',    !Aesculus hippocastanum
     & '   ','336','AEPA    ','OH ','OH ','OH ','OH ',    !Aesculus pavia
     & '   ','   ','AEPAP   ','OH ','OH ','OH ','OH ',    !Aesculus pavia var. pavia
     & '   ','337','AESY    ','OH ','OH ','OH ','OH ',    !Aesculus sylvatica
     & '   ','   ','AILAN   ','OH ','OH ','AI ','OH '/    !Ailanthus
C
      DATA ((ASPT(I,J),J=1,7),I=41,50) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'AI ','341','AIAL    ','OH ','OH ','AI ','OH ',    !Ailanthus altissima
     & '   ','345','ALJU    ','OH ','OH ','OH ','OH ',    !Albizia julibrissin
     & '   ','350','ALNUS   ','OH ','OH ','OH ','OH ',    !Alnus
     & '   ','   ','ALFA4   ','OH ','OH ','OH ','OH ',    !Alnus fallacina
     & '   ','355','ALGL2   ','OH ','OH ','OH ','OH ',    !Alnus glutinosa
     & '   ','   ','ALIN2   ','OH ','OH ','OH ','OH ',    !Alnus incana
     & '   ','   ','ALINR   ','OH ','OH ','OH ','OH ',    !Alnus incana ssp. Rugosa
     & '   ','   ','ALSE2   ','OH ','OH ','OH ','OH ',    !Alnus serrulata
     & '   ','   ','ALVI5   ','OH ','OH ','OH ','OH ',    !Alnus viridis
     & '   ','   ','ALVIC   ','OH ','OH ','OH ','OH '/    !Alnus viridis ssp. crispa
C
      DATA ((ASPT(I,J),J=1,7),I=51,60) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'SE ','356','AMELA   ','OH ','OH ','SE ','OH ',    !Amelanchier
     & '   ','357','AMAR3   ','OH ','OH ','SE ','OH ',    !Amelanchier arborea
     & '   ','   ','AMARA4  ','OH ','OH ','SE ','OH ',    !Amelanchier arborea var. arborea
     & '   ','   ','AMARA5  ','OH ','OH ','SE ','OH ',    !Amelanchier arborea var. austromontana
     & '   ','   ','AMBA    ','OH ','OH ','SE ','OH ',    !Amelanchier bartramiana
     & '   ','   ','AMIN2   ','OH ','OH ','SE ','OH ',    !Amelanchier interior
     & '   ','   ','AMLA    ','OH ','OH ','SE ','OH ',    !Amelanchier laevis
     & '   ','358','AMSA    ','OH ','OH ','SE ','OH ',    !Amelanchier sanguinea
     & '   ','   ','ARSP2   ','OH ','OH ','OH ','OH ',    !Aralia spinosa
     & '   ','   ','ASIMI   ','OH ','OH ','OH ','OH '/    !Asimina
C
      DATA ((ASPT(I,J),J=1,7),I=61,70) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','ASOB6   ','OH ','OH ','OH ','OH ',    !Asimina obovata
     & '   ','   ','ASPA18  ','OH ','OH ','OH ','OH ',    !Asimina parviflora
     & '   ','367','ASTR    ','OH ','OH ','OH ','OH ',    !Asimina triloba
     & '   ','986','AVGE    ','OH ','OH ','OH ','OH ',    !Avicennia germinans
     & 'BB ','370','BETUL   ','RB ','PB ','YB ','BB ',    !Betula
     & 'YB ','371','BEAL2   ','RB ','YB ','YB ','BB ',    !Betula alleghaniensis
     & 'SB ','372','BELE    ','RB ','YB ','SB ','SB ',    !Betula lenta
     & 'RB ','373','BENI    ','RB ','PB ','RB ','BB ',    !Betula nigra
     & 'WR ','374','BEOC2   ','RB ','PB ','WR ','BB ',    !Betula occidentalis
     & 'PB ','375','BEPA    ','RB ','PB ','PB ','BB '/    !Betula papyrifera
C
      DATA ((ASPT(I,J),J=1,7),I=71,80) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','BEPAC2  ','RB ','PB ','PB ','BB ',    !Betula papyrifera var. cordifolia
     & '   ','   ','BEPAP   ','RB ','PB ','PB ','BB ',    !Betula papyrifera var. papyrifera
     & '   ','   ','BEPE3   ','RB ','PB ','PB ','BB ',    !Betula pendula
     & 'GB ','379','BEPO    ','RB ','YB ','GB ','BB ',    !Betula populifolia
     & '   ','   ','BEPU5   ','RB ','YB ','GB ','BB ',    !Betula pubescens
     & '   ','   ','BEPUP4  ','RB ','YB ','GB ','BB ',    !Betula pubescens ssp. Pubescens
     & '   ','   ','BEPU4   ','RB ','PB ','YB ','BB ',    !Betula pumila
     & '   ','377','BEUB    ','RB ','PB ','RB ','BB ',    !Betula uber
     & '   ','   ','BRPA4   ','OH ','OH ','OH ','OH ',    !Broussonetia papyrifera
     & '   ','854','BUSI    ','OH ','OH ','OH ','OH '/    !Bursera simaruba
C
      DATA ((ASPT(I,J),J=1,7),I=81,90) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','CARPI   ','AH ','AH ','AH ','AH ',    !Carpinus
     & 'AH ','391','CACA18  ','AH ','AH ','AH ','AH ',    !Carpinus caroliniana
     & '   ','   ','CACAC2  ','AH ','AH ','AH ','AH ',    !Carpinus caroliniana ssp. Caroliniana
     & '   ','   ','CACAV   ','AH ','AH ','AH ','AH ',    !Carpinus caroliniana ssp. Virginiana
     & 'HI ','400','CARYA   ','HI ','BH ','HI ','HI ',    !Carya
     & 'MH ','409','CAAL27  ','MH ','PH ','MH ','HI ',    !Carya alba
     & 'WH ','401','CAAQ2   ','WH ','BH ','HI ','HI ',    !Carya aquatica
     & '   ','   ','CABR16  ','HI ','BH ','HI ','HI ',    !Carya brownii
     & '   ','413','CACA38  ','SH ','SH ','HI ','HI ',    !Carya carolinae-septentrionalis
     & 'BH ','402','CACO15  ','BH ','BH ','HI ','HI '/    !Carya cordiformis
C
      DATA ((ASPT(I,J),J=1,7),I=91,100) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','411','CAFL6   ','BI ','PH ','HI ','HI ',    !Carya floridana
     & 'PH ','403','CAGL8   ','PH ','PH ','PH ','HI ',    !Carya glabra
     & 'PE ','404','CAIL2   ','PE ','BH ','HI ','HI ',    !Carya illinoinensis
     & 'SL ','405','CALA21  ','SL ','SH ','SL ','HI ',    !Carya laciniosa
     & '   ','   ','CALE13  ','HI ','BH ','HI ','HI ',    !Carya lecontei
     & '   ','406','CAMY    ','PE ','BH ','HI ','HI ',    !Carya myristiciformis
     & '   ','412','CAOV3   ','PH ','PH ','HI ','HI ',    !Carya ovalis
     & 'SH ','407','CAOV2   ','SH ','SH ','SH ','HI ',    !Carya ovata
     & '   ','410','CAPA24  ','PH ','PH ','HI ','HI ',    !Carya pallida
     & '   ','   ','CASC16  ','PH ','PH ','HI ','HI '/    !Carya schneckii
C
      DATA ((ASPT(I,J),J=1,7),I=101,110) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'BI ','408','CATE9   ','BI ','PH ','HI ','HI ',    !Carya texana
     & '   ','420','CASTA   ','OH ','AC ','OH ','OH ',    !Castanea
     & 'AC ','421','CADE12  ','OH ','AC ','OH ','OH ',    !Castanea dentata
     & '   ','424','CAMO83  ','OH ','AC ','OH ','OH ',    !Castanea mollissima
     & '   ','422','CAPU9   ','OH ','AC ','OH ','OH ',    !Castanea pumila
     & '   ','423','CAPUO   ','OH ','AC ','OH ','OH ',    !Castanea pumila var. chinquapin
     & '   ','   ','CAPUP3  ','OH ','AC ','OH ','OH ',    !Castanea pumila var. pumila
     & 'CA ','450','CATAL   ','CA ','OH ','OH ','CA ',    !Catalpa
     & '   ','451','CABI8   ','CA ','OH ','OH ','CA ',    !Catalpa bignonioides
     & '   ','   ','CAOV5   ','CA ','OH ','OH ','CA '/    !Catalpa ovata
C
      DATA ((ASPT(I,J),J=1,7),I=111,120) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','452','CASP8   ','CA ','OH ','OH ','CA ',    !Catalpa speciosa
     & '   ','   ','CEDRU   ','JU ','RC ','WC ','JU ',    !Cedrus
     & 'HB ','460','CELTI   ','HK ','HK ','HK ','HB ',    !Celtis
     & 'SG ','461','CELA    ','SG ','HK ','HK ','HB ',    !Celtis laevigata
     & '   ','   ','CELAL   ','SG ','HK ','HK ','HB ',    !Celtis laevigata va. Laevigata
     & '   ','463','CELAR   ','SG ','HK ','HK ','HB ',    !Celtis laevigata var. reticulata
     & '   ','   ','CELAT8  ','SG ','HK ','HK ','HB ',    !Celtis laevigata var. texana
     & 'HK ','462','CEOC    ','HK ','HK ','HK ','HB ',    !Celtis occidentalis
     & '   ','   ','CETE    ','HK ','HK ','HK ','HB ',    !Celtis tenuifolia
     & '   ','   ','CERCI2  ','RD ','OH ','OH ','RD '/    !Cercis
C
      DATA ((ASPT(I,J),J=1,7),I=121,130) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'RD ','471','CECA4   ','RD ','OH ','OH ','RD ',    !Cercis canadensis
     & '   ','   ','CECAC   ','RD ','OH ','OH ','RD ',    !Cercis canadensis var. canadensis
     & '   ','   ','CECAT   ','RD ','OH ','OH ','RD ',    !Cercis canadensis var. texensis
     & '   ','040','CHAMA4  ','OS ','OS ','AW ','JU ',    !Chamaecyparis
     & 'AW ','043','CHTH2   ','OS ','OS ','AW ','JU ',    !Chamaecyparis thyoides
     & '   ','   ','CLADR   ','OH ','OH ','OH ','OH ',    !Cladrastis
     & '   ','481','CLKE    ','OH ','OH ','OH ','OH ',    !Cladrastis kentukea
     & '   ','987','COER2   ','OH ','OH ','OH ','OH ',    !Conocarpus erectus
     & '   ','   ','CORU17  ','OH ','OH ','OH ','OH ',    !Conostegia rufescens
     & '   ','490','CORNU   ','DW ','DW ','DW ','DW '/    !Cornus
C
      DATA ((ASPT(I,J),J=1,7),I=131,140) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','COAL2   ','DW ','DW ','DW ','DW ',    !Cornus alternifolia
     & '   ','   ','CODR    ','DW ','DW ','DW ','DW ',    !Cornus drummondii
     & 'DW ','491','COFL2   ','DW ','DW ','DW ','DW ',    !Cornus florida
     & '   ','   ','COFO    ','DW ','DW ','DW ','DW ',    !Cornus foemina
     & '   ','   ','CORU    ','DW ','DW ','DW ','DW ',    !Cornus rugosa
     & '   ','   ','COSE16  ','DW ','DW ','DW ','DW ',    !Cornus sericea
     & '   ','   ','COSES   ','OH ','OH ','OH ','OH ',    !Cornus sericea ssp. Sericea
     & '   ','   ','COCO10  ','OH ','OH ','OH ','OH ',    !Cotinus coggygria
     & '   ','996','COOB2   ','OH ','OH ','OH ','OH ',    !Cotinus obovatus
     & 'HT ','500','CRATA   ','HT ','HT ','HT ','OH '/    !Crataegus
C
      DATA ((ASPT(I,J),J=1,7),I=141,150) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','CRAM4   ','HT ','HT ','HT ','OH ',    !Crataegus ambitiosa
     & '   ','   ','CRBE2   ','HT ','HT ','HT ','OH ',    !Crataegus berberifolia
     & '   ','   ','CRBR    ','HT ','HT ','HT ','OH ',    !Crataegus brachyacantha
     & '   ','503','CRBR3   ','HT ','HT ','HT ','OH ',    !Crataegus brainerdii
     & '   ','504','CRCA    ','HT ','HT ','HT ','OH ',    !Crataegus calpodendron
     & '   ','505','CRCH    ','HT ','HT ','HT ','OH ',    !Crataegus chrysocarpa
     & '   ','   ','CRCO2   ','HT ','HT ','HT ','OH ',    !Crataegus coccinioides
     & '   ','501','CRCR2   ','HT ','HT ','HT ','OH ',    !Crataegus crus-galli
     & '   ','506','CRDI    ','HT ','HT ','HT ','OH ',    !Crataegus dilatata
     & '   ','   ','CRDI4   ','HT ','HT ','HT ','OH '/    !Crataegus disperma
C
      DATA ((ASPT(I,J),J=1,7),I=151,160) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','CRDI9   ','HT ','HT ','HT ','OH ',    !Crataegus dispessa
     & '   ','   ','CRDI10  ','HT ','HT ','HT ','OH ',    !Crataegus dissona
     & '   ','   ','CRDO3   ','HT ','HT ','HT ','OH ',    !Crataegus dodgei
     & '   ','   ','CRDO2   ','HT ','HT ','HT ','OH ',    !Crataegus douglasii
     & '   ','   ','CREN    ','HT ','HT ','HT ','OH ',    !Crataegus engelmannii
     & '   ','507','CRFL    ','HT ','HT ','HT ','OH ',    !Crataegus flabellata
     & '   ','   ','CRFU2   ','HT ','HT ','HT ','OH ',    !Crataegus fulleriana
     & '   ','   ','CRIM    ','HT ','HT ','HT ','OH ',    !Crataegus immanis
     & '   ','   ','CRIN10  ','HT ','HT ','HT ','OH ',    !Crataegus incaedua
     & '   ','   ','CRIN3   ','HT ','HT ','HT ','OH '/    !Crataegus intricata
C
      DATA ((ASPT(I,J),J=1,7),I=161,170) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','CRIR    ','HT ','HT ','HT ','OH ',    !Crataegus iracunda
     & '   ','   ','CRIR2   ','HT ','HT ','HT ','OH ',    !Crataegus irrasa
     & '   ','   ','CRMA3   ','HT ','HT ','HT ','OH ',    !Crataegus macrosperma
     & '   ','   ','CRMA4   ','HT ','HT ','HT ','OH ',    !Crataegus margarettiae
     & '   ','   ','CRMA5   ','HT ','HT ','HT ','OH ',    !Crataegus marshallii
     & '   ','502','CRMO2   ','HT ','HT ','HT ','OH ',    !Crataegus mollis
     & '   ','508','CRMO3   ','HT ','HT ','HT ','OH ',    !Crataegus monogyna
     & '   ','   ','CRNI    ','HT ','HT ','HT ','OH ',    !Crataegus nitida
     & '   ','   ','CROP    ','HT ','HT ','HT ','OH ',    !Crataegus opaca
     & '   ','   ','CROV2   ','HT ','HT ','HT ','OH '/    !Crataegus ovata
C
      DATA ((ASPT(I,J),J=1,7),I=171,180) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','CRPE13  ','HT ','HT ','HT ','OH ',    !Crataegus pearsonii
     & '   ','509','CRPE    ','HT ','HT ','HT ','OH ',    !Crataegus pedicellata
     & '   ','   ','CRPH    ','HT ','HT ','HT ','OH ',    !Crataegus phaenopyrum
     & '   ','   ','CRPR2   ','HT ','HT ','HT ','OH ',    !Crataegus pruinosa
     & '   ','   ','CRPU    ','HT ','HT ','HT ','OH ',    !Crataegus punctata
     & '   ','   ','CRRE3   ','HT ','HT ','HT ','OH ',    !Crataegus reverchonii
     & '   ','   ','CRSP    ','HT ','HT ','HT ','OH ',    !Crataegus spathulata
     & '   ','   ','CRSU5   ','HT ','HT ','HT ','OH ',    !Crataegus succulenta
     & '   ','   ','CRTR2   ','HT ','HT ','HT ','OH ',    !Crataegus triflora
     & '   ','   ','CRUN    ','HT ','HT ','HT ','OH '/    !Crataegus uniflora
C
      DATA ((ASPT(I,J),J=1,7),I=181,190) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','CRVI2   ','HT ','HT ','HT ','OH ',    !Crataegus viridis
     & '   ','   ','CRWA    ','HT ','HT ','HT ','OH ',    !Crataegus warneri
     & '   ','   ','CULA    ','OS ','OS ','OS ','OS ',    !Cunninghamia lanceolata
     & '   ','050','CUPRE   ','JU ','RC ','WC ','JU ',    !Cupressus
     & '   ','051','CUAR    ','JU ','RC ','WC ','JU ',    !Cupressus arizonica
     & '   ','520','DIOSP   ','PS ','OH ','PS ','PS ',    !Diospyros
     & '   ','522','DITE3   ','PS ','OH ','PS ','PS ',    !Diospyros texana
     & 'PS ','521','DIVI5   ','PS ','OH ','PS ','PS ',    !Diospyros virginiana
     & '   ','   ','DIPA9   ','OH ','OH ','OH ','OH ',    !Dirca palustris
     & '   ','   ','ELAEA   ','OH ','OH ','OH ','OH '/    !Elaeagnus
C
      DATA ((ASPT(I,J),J=1,7),I=191,200) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','997','ELAN    ','OH ','OH ','OH ','OH ',    !Elaeagnus angustifolia
     & '   ','   ','ERJA3   ','OH ','OH ','OH ','OH ',    !Eriobotrya japonica
     & '   ','510','EUCAL   ','OH ','OH ','OH ','OH ',    !Eucalyptus
     & '   ','512','EUCA2   ','OH ','OH ','OH ','OH ',    !Eucalyptus camaldulensis
     & '   ','513','EUGR12  ','OH ','OH ','OH ','OH ',    !Eucalyptus grandis
     & '   ','   ','FAGUS   ','AB ','AB ','AB ','AB ',    !Fagus
     & 'AB ','531','FAGR    ','AB ','AB ','AB ','AB ',    !Fagus grandifolia
     & '   ','   ','FASY    ','AB ','AB ','AB ','AB ',    !Fagus sylvatica
     & 'AS ','540','FRAXI   ','AS ','GA ','AS ','AS ',    !Fraxinus
     & 'WA ','541','FRAM2   ','WA ','WA ','WA ','WA '/    !Fraxinus americana
C
      DATA ((ASPT(I,J),J=1,7),I=201,210) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','548','FRCA3   ','AS ','GA ','AS ','AS ',    !Fraxinus caroliniana
     & 'BA ','543','FRNI    ','BA ','BA ','BA ','BA ',    !Fraxinus nigra
     & 'GA ','544','FRPE    ','GA ','GA ','GA ','GA ',    !Fraxinus pennsylvanica
     & 'PA ','545','FRPR    ','PA ','WA ','PA ','AS ',    !Fraxinus profunda
     & 'UA ','546','FRQU    ','UA ','WA ','AS ','AS ',    !Fraxinus quadrangulata
     & '   ','549','FRTE    ','AS ','GA ','AS ','AS ',    !Fraxinus texensis
     & '   ','547','FRVE2   ','AS ','GA ','AS ','AS ',    !Fraxinus velutina
     & '   ','561','GIBI2   ','OH ','OH ','OH ','OH ',    !Ginkgo biloba
     & '   ','550','GLEDI   ','HL ','BK ','BK ','HL ',    !Gleditsia
     & '   ','551','GLAQ    ','HL ','BK ','BK ','HL '/    !Gleditsia aquatica
C
      DATA ((ASPT(I,J),J=1,7),I=211,220) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','GLTE    ','HL ','BK ','BK ','HL ',    !Gleditsia texana
     & 'HL ','552','GLTR    ','HL ','BK ','BK ','HL ',    !Gleditsia triacanthos
     & 'LB ','555','GOLA    ','OH ','OH ','OH ','LB ',    !Gordonia lasianthus
     & 'KC ','571','GYDI    ','KC ','OH ','OH ','OH ',    !Gymnocladus dioicus
     & 'HA ','580','HALES   ','OH ','OH ','OH ','HA ',    !Halesia
     & '   ','581','HACA3   ','OH ','OH ','OH ','HA ',    !Halesia carolina
     & '   ','582','HADI3   ','OH ','OH ','OH ','HA ',    !Halesia diptera
     & '   ','   ','HATE3   ','OH ','OH ','OH ','HA ',    !Halesia tetraptera
     & '   ','   ','HATET   ','OH ','OH ','OH ','HA ',    !Halesia tetraptera var. tetraptera
     & '   ','   ','ILEX    ','OH ','OH ','HY ','HY '/    !Ilex
C
      DATA ((ASPT(I,J),J=1,7),I=221,230) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','ILAM    ','OH ','OH ','HY ','HY ',    !Ilex ambigua
     & '   ','   ','ILCA    ','OH ','OH ','HY ','HY ',    !Ilex cassine
     & '   ','   ','ILCO2   ','OH ','OH ','HY ','HY ',    !Ilex collina
     & '   ','   ','ILCO    ','OH ','OH ','HY ','HY ',    !Ilex coriacea
     & '   ','   ','ILCR2   ','OH ','OH ','HY ','HY ',    !Ilex crenata
     & '   ','   ','ILDE    ','OH ','OH ','HY ','HY ',    !Ilex decidua
     & '   ','   ','ILGL    ','OH ','OH ','HY ','HY ',    !Ilex glabra
     & '   ','   ','ILLO    ','OH ','OH ','HY ','HY ',    !Ilex longipes
     & '   ','   ','ILMO    ','OH ','OH ','HY ','HY ',    !Ilex montana
     & '   ','   ','ILMU    ','OH ','OH ','HY ','HY '/    !Ilex mucronata
C
      DATA ((ASPT(I,J),J=1,7),I=231,240) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'HY ','591','ILOP    ','OH ','OH ','HY ','HY ',    !Ilex opaca
     & '   ','   ','ILOPO   ','OH ','OH ','HY ','HY ',    !Ilex opaca
     & '   ','600','JUGLA   ','WN ','WN ','WN ','WN ',    !Juglans
     & 'BN ','601','JUCI    ','BN ','BN ','BN ','BN ',    !Juglans cinerea
     & '   ','606','JUMA    ','WN ','WN ','WN ','WN ',    !Juglans major
     & '   ','605','JUMI    ','WN ','WN ','WN ','WN ',    !Juglans microcarpa
     & 'WN ','602','JUNI    ','WN ','WN ','WN ','WN ',    !Juglans nigra
     & 'JU ','057','JUNIP   ','JU ','RC ','JU ','JU ',    !Juniperus
     & '   ','061','JUAS    ','JU ','RC ','JU ','JU ',    !Juniperus ashei
     & '   ','059','JUCO11  ','JU ','RC ','JU ','JU '/    !Juniperus coahuilensis
C
      DATA ((ASPT(I,J),J=1,7),I=241,250) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','JUCO6   ','JU ','RC ','JU ','JU ',    !Juniperus communis
     & '   ','   ','JUCOD   ','JU ','RC ','JU ','JU ',    !Juniperus communis va. Depressa
     & '   ','063','JUDE2   ','JU ','RC ','JU ','JU ',    !Juniperus deppeana
     & '   ','   ','JUMO    ','JU ','RC ','JU ','JU ',    !Juniperus monosperma
     & '   ','058','JUPI    ','JU ','RC ','JU ','JU ',    !Juniperus pinchotii
     & '   ','066','JUSC2   ','JU ','RC ','JU ','JU ',    !Juniperus scopulorum
     & 'RC ','068','JUVI    ','RC ','RC ','RC ','JU ',    !Juniperus virginiana
     & '   ','   ','JUVIV   ','RC ','RC ','RC ','JU ',    !Juniperus virginiana va. Virginiana
     & '   ','067','JUVIS   ','RC ','RC ','RC ','JU ',    !Juniperus virginiana var. silicicola
     & '   ','988','LARA2   ','OH ','OH ','OH ','OH '/    !Laguncularia racemosa
C
      DATA ((ASPT(I,J),J=1,7),I=251,260) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','LAPO    ','OH ','OH ','OH ','OH ',    !Laplacea portoricensis
     & '   ','070','LARIX   ','OS ','TA ','TA ','HM ',    !Larix
     & '   ','   ','LADE2   ','OS ','TA ','TA ','HM ',    !Larix decidua
     & 'TA ','071','LALA    ','OS ','TA ','TA ','HM ',    !Larix laricina
     & 'SU ','611','LIST2   ','SU ','OH ','SU ','SU ',    !Liquidambar styraciflua
     & 'YP ','621','LITU    ','YP ','OH ','YP ','YP ',    !Liriodendron tulipifera
     & 'OO ','641','MAPO    ','OO ','OH ','OO ','OH ',    !Maclura pomifera
     & 'MG ','650','MAGNO   ','CT ','OH ','MG ','MG ',    !Magnolia
     & 'CT ','651','MAAC    ','CT ','OH ','CT ','CT ',    !Magnolia acuminata
     & '   ','   ','MAAS    ','CT ','OH ','MG ','MG '/    !Magnolia ashei
C
      DATA ((ASPT(I,J),J=1,7),I=261,270) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','655','MAFR    ','CT ','OH ','MG ','MG ',    !Magnolia fraseri
     & 'MS ','652','MAGR4   ','CT ','OH ','MG ','MS ',    !Magnolia grandiflora
     & 'ML ','654','MAMA2   ','CT ','OH ','CT ','ML ',    !Magnolia macrophylla
     & '   ','657','MAPY    ','CT ','OH ','MG ','MG ',    !Magnolia pyramidata
     & '   ','   ','MASO9   ','CT ','OH ','MG ','MG ',    !Magnolia soulangiana
     & '   ','   ','MAST6   ','CT ','OH ','MG ','MG ',    !Magnolia stellata
     & '   ','658','MATR    ','CT ','OH ','MG ','MG ',    !Magnolia tripetala
     & 'MV ','653','MAVI2   ','MV ','OH ','MV ','MV ',    !Magnolia virginiana
     & 'AP ','660','MALUS   ','OH ','AP ','AP ','AP ',    !Malus
     & '   ','662','MAAN3   ','OH ','AP ','AP ','AP '/    !Malus angustifolia
C
      DATA ((ASPT(I,J),J=1,7),I=271,280) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','MAANA   ','OH ','AP ','AP ','AP ',    !Malus angustifolia var. angustifolia
     & '   ','   ','MAANP   ','OH ','AP ','AP ','AP ',    !Malus angustifolia var. puberula
     & '   ','   ','MABA    ','OH ','AP ','AP ','AP ',    !Malus baccata
     & '   ','   ','MABAB   ','OH ','AP ','AP ','AP ',    !Malus baccata
     & '   ','664','MAIO    ','OH ','AP ','AP ','AP ',    !Malus ioensis
     & '   ','   ','MAIOI   ','OH ','AP ','AP ','AP ',    !Malus ioensis var. ioensis
     & '   ','   ','MAPR    ','OH ','AP ','AP ','AP ',    !Malus prunifolia
     & '   ','   ','MAPU    ','OH ','AP ','AP ','AP ',    !Malus pumila
     & '   ','   ','MASO3   ','OH ','AP ','AP ','AP ',    !Malus soulardii
     & '   ','   ','MASY2   ','OH ','AP ','AP ','AP '/    !Malus sylvestris
C
      DATA ((ASPT(I,J),J=1,7),I=281,290) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','992','MEQU    ','OH ','OH ','OH ','OH ',    !Melaleuca quinquenervia
     & '   ','993','MEAZ    ','OH ','OH ','OH ','OH ',    !Melia azedarach
     & '   ','   ','MIFO    ','OH ','OH ','OH ','OH ',    !Miconia foveolata
     & '   ','   ','MIPY2   ','OH ','OH ','OH ','OH ',    !Miconia pycnoneura
     & 'MB ','680','MORUS   ','MB ','OH ','OH ','MB ',    !Morus
     & 'WM ','681','MOAL    ','MB ','OH ','OH ','MB ',    !Morus alba
     & '   ','683','MOMI    ','MB ','OH ','OH ','MB ',    !Morus microphylla
     & '   ','684','MONI    ','MB ','OH ','OH ','MB ',    !Morus nigra
     & 'RY ','682','MORU2   ','MB ','OH ','OH ','MB ',    !Morus rubra
     & '   ','   ','MORUR   ','MB ','OH ','OH ','MB '/    !Morus rubra var. rubra
C
      DATA ((ASPT(I,J),J=1,7),I=291,300) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'TL ','690','NYSSA   ','TL ','BG ','BG ','BG ',    !Nyssa
     & 'WT ','691','NYAQ2   ','WT ','BG ','WT ','WT ',    !Nyssa aquatica
     & 'TS ','694','NYBI    ','TS ','BG ','BG ','TS ',    !Nyssa biflora
     & 'OG ','692','NYOG    ','WT ','BG ','WT ','WT ',    !Nyssa ogeche
     & 'BG ','693','NYSY    ','BG ','BG ','BG ','BG ',    !Nyssa sylvatica
     & '   ','   ','NYUR2   ','TL ','BG ','BG ','BG ',    !Nyssa ursina
     & '   ','   ','OSTRY   ','HH ','HH ','HH ','HH ',    !Ostrya
     & 'HH ','701','OSVI    ','HH ','HH ','HH ','HH ',    !Ostrya virginiana
     & '   ','   ','OSVIV   ','HH ','HH ','HH ','HH ',    !Ostrya virginiana var. virginiana
     & 'SD ','711','OXAR    ','SD ','OH ','SD ','SD '/    !Oxydendrum arboreum
C
      DATA ((ASPT(I,J),J=1,7),I=301,310) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'PW ','712','PATO2   ','OH ','OH ','PW ','OH ',    !Paulownia tomentosa
     & '   ','720','PERSE   ','OH ','OH ','OH ','RA ',    !Persea
     & 'RA ','721','PEBO    ','OH ','OH ','OH ','RA ',    !Persea borbonia
     & '   ','   ','PEHU2   ','OH ','OH ','OH ','RA ',    !Persea humilis
     & '   ','   ','PEPA37  ','OH ','OH ','OH ','RA ',    !Persea palustris
     & 'PI ','090','PICEA   ','OS ','WS ','PI ','PI ',    !Picea
     & 'NS ','091','PIAB    ','OS ','NS ','OH ','PI ',    !Picea abies
     & 'WS ','094','PIGL    ','OS ','WS ','WS ','PI ',    !Picea glauca
     & 'BS ','095','PIMA    ','OS ','BS ','BS ','PI ',    !Picea mariana
     & 'RS ','097','PIRU    ','OS ','BS ','RS ','PI '/    !Picea rubens
C
      DATA ((ASPT(I,J),J=1,7),I=311,320) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'OP ','100','PINUS   ','OS ','WP ','OP ','LP ',    !Pinus
     & '   ','   ','PIAR5   ','OS ','OS ','OP ','OS ',    !Pinus arizonica
     & 'JP ','105','PIBA2   ','VP ','JP ','JP ','PU ',    !Pinus banksiana
     & 'PU ','107','PICL    ','VP ','JP ','OP ','PU ',    !Pinus clausa
     & 'SP ','110','PIEC2   ','SP ','RN ','SP ','SP ',    !Pinus echinata
     & 'SA ','111','PIEL    ','LP ','RN ','LP ','SA ',    !Pinus elliottii
     & '   ','144','PIELE2  ','LP ','RN ','OP ','SA ',    !Pinus elliottii var. elliottii
     & 'SR ','115','PIGL2   ','SP ','JP ','SP ','SR ',    !Pinus glabra
     & '   ','136','PINI    ','LP ','RN ','RN ','LP ',    !Pinus nigra
     & 'LL ','121','PIPA2   ','LP ','RN ','LP ','LL '/    !Pinus palustris
C
      DATA ((ASPT(I,J),J=1,7),I=321,330) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'TM ','123','PIPU5   ','VP ','JP ','TM ','TM ',    !Pinus pungens
     & 'RN ','125','PIRE    ','LP ','RN ','RN ','LP ',    !Pinus resinosa
     & 'PP ','126','PIRI    ','VP ','JP ','PP ','PP ',    !Pinus rigida
     & 'PD ','128','PISE    ','LP ','JP ','PD ','PD ',    !Pinus serotina
     & '   ','114','PIST3   ','WP ','WP ','WP ','WP ',    !Pinus strobiformis
     & 'WP ','129','PIST    ','WP ','WP ','WP ','WP ',    !Pinus strobus
     & 'SC ','130','PISY    ','VP ','SC ','SC ','PU ',    !Pinus sylvestris
     & 'LP ','131','PITA    ','LP ','RN ','LP ','LP ',    !Pinus taeda
     & 'VP ','132','PIVI2   ','VP ','JP ','VP ','VP ',    !Pinus virginiana
     & '   ','722','PLAQ    ','OH ','OH ','OH ','OH '/    !Planera aquatica
C
      DATA ((ASPT(I,J),J=1,7),I=331,340) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','729','PLATA   ','SY ','SY ','SY ','SY ',    !Platanus
     & 'SY ','731','PLOC    ','SY ','SY ','SY ','SY ',    !Platanus occidentalis
     & 'CW ','740','POPUL   ','EC ','EC ','EC ','CW ',    !Populus
     & '   ','   ','POAC5   ','EC ','EC ','EC ','CW ',    !Populus acuminata
     & '   ','752','POAL7   ','BP ','BP ','BP ','BT ',    !Populus alba
     & '   ','749','POAN3   ','EC ','EC ','EC ','CW ',    !Populus angustifolia
     & 'BP ','741','POBA2   ','BP ','BP ','BP ','BT ',    !Populus balsamifera
     & '   ','   ','POBAB2  ','BP ','BP ','BP ','BT ',    !Populus balsamifera ssp. Balsamifera
     & '   ','   ','POCA19  ','EC ','EC ','EC ','CW ',    !Populus canadensis
     & '   ','   ','POCA14  ','EC ','EC ','EC ','CW '/    !Populus canescens
C
      DATA ((ASPT(I,J),J=1,7),I=341,350) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'EC ','742','PODE3   ','EC ','EC ','EC ','CW ',    !Populus deltoides
     & '   ','   ','PODED   ','EC ','EC ','EC ','CW ',    !Populus deltoides
     & '   ','745','PODEM   ','EC ','EC ','EC ','CW ',    !Populus deltoides ssp. Monilifera
     & '   ','   ','PODEW   ','EC ','EC ','EC ','CW ',    !Populus deltoides ssp. Wislizeni
     & '   ','748','POFR2   ','EC ','EC ','EC ','CW ',    !Populus fremontii
     & 'BT ','743','POGR4   ','BT ','BT ','BT ','BT ',    !Populus grandidentata
     & 'PY ','744','POHE4   ','EC ','EC ','PY ','CW ',    !Populus heterophylla
     & '   ','753','PONI    ','BP ','BP ','BP ','BT ',    !Populus nigra
     & '   ','   ','POSM2   ','EC ','EC ','EC ','CW ',    !Populus smithii
     & '   ','   ','POTR10  ','QA ','QA ','QA ','BT '/    !Populus tremula
C
      DATA ((ASPT(I,J),J=1,7),I=351,360) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'QA ','746','POTR5   ','QA ','QA ','QA ','BT ',    !Populus tremuloides
     & '   ','755','PROSO   ','OH ','OH ','OH ','OH ',    !Prosopis
     & '   ','756','PRGL2   ','OH ','OH ','OH ','OH ',    !Prosopis glandulosa
     & '   ','   ','PRGLG   ','OH ','OH ','OH ','OH ',    !Prosopis glandulosa var. glandulosa
     & '   ','758','PRPU    ','OH ','OH ','OH ','OH ',    !Prosopis pubescens
     & 'PL ','760','PRUNU   ','OH ','PL ','PL ','AP ',    !Prunus
     & '   ','769','PRAL5   ','OH ','PL ','PL ','AP ',    !Prunus alleghaniensis
     & '   ','   ','PRALD   ','OH ','PL ','PL ','AP ',    !Prunus alleghaniensis
     & '   ','766','PRAM    ','OH ','PL ','PL ','AP ',    !Prunus americana
     & '   ','770','PRAN3   ','OH ','PL ','PL ','AP '/    !Prunus angustifolia
C
      DATA ((ASPT(I,J),J=1,7),I=361,370) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','PRAR3   ','OH ','PL ','PL ','AP ',    !Prunus armeniaca
     & '   ','771','PRAV    ','OH ','CC ','PR ','AP ',    !Prunus avium
     & '   ','   ','PRCA    ','OH ','CC ','PR ','AP ',    !Prunus caroliniana
     & '   ','   ','PRCE2   ','OH ','CC ','PR ','AP ',    !Prunus cerasifera
     & '   ','772','PRCE    ','OH ','CC ','PR ','AP ',    !Prunus cerasus
     & '   ','773','PRDO    ','OH ','PL ','PL ','AP ',    !Prunus domestica
     & '   ','   ','PRDOD   ','OH ','PL ','PL ','AP ',    !Prunus domestica var. domestica
     & '   ','   ','PRHO    ','OH ','PL ','PL ','AP ',    !Prunus hortulana
     & '   ','774','PRMA    ','OH ','PL ','PL ','AP ',    !Prunus mahaleb
     & '   ','   ','PRME    ','OH ','PL ','PL ','AP '/    !Prunus mexicana
C
      DATA ((ASPT(I,J),J=1,7),I=371,380) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','PRMU    ','OH ','PL ','PL ','AP ',    !Prunus munsoniana
     & '   ','   ','PRMY    ','OH ','PL ','PL ','AP ',    !Prunus myrtifolia
     & '   ','765','PRNI    ','OH ','PL ','PL ','AP ',    !Prunus nigra
     & 'PR ','761','PRPE2   ','OH ','PR ','PR ','AP ',    !Prunus pensylvanica
     & '   ','   ','PRPEP   ','OH ','PR ','PR ','AP ',    !Prunus pensylvanica
     & '   ','764','PRPE3   ','OH ','PL ','PL ','AP ',    !Prunus persica
     & 'BC ','762','PRSE2   ','BC ','BC ','BC ','BC ',    !Prunus serotina
     & '   ','   ','PRSEE   ','BC ','BC ','BC ','BC ',    !Prunus serotina
     & '   ','   ','PRSU4   ','OH ','PR ','PR ','AP ',    !Prunus subhirtella
     & '   ','   ','PRTO80  ','OH ','PR ','PR ','AP '/    !Prunus tomentosa
C
      DATA ((ASPT(I,J),J=1,7),I=381,390) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','PRTR3   ','OH ','PL ','PL ','AP ',    !Prunus triloba
     & '   ','   ','PRUM    ','OH ','PL ','PL ','AP ',    !Prunus umbellata
     & 'CC ','763','PRVI    ','OH ','CC ','PR ','AP ',    !Prunus virginiana
     & '   ','   ','PRVID   ','OH ','CC ','PR ','AP ',    !Prunus virginiana var. demissa
     & '   ','   ','PRVIV   ','OH ','CC ','PR ','AP ',    !Prunus virginiana var. virginiana
     & '   ','   ','PYRUS   ','OH ','PL ','PL ','AP ',    !Pyrus
     & '   ','   ','PYCA80  ','OH ','PL ','PL ','AP ',    !Pyrus calleryana
     & '   ','   ','PYCO    ','OH ','PL ','PL ','AP ',    !Pyrus communis
     & '   ','   ','PYPY2   ','OH ','PL ','PL ','AP ',    !Pyrus pyrifolia
     & 'OK ','800','QUERC   ','WO ','WO ','OK ','WO '/    !Quercus
C
      DATA ((ASPT(I,J),J=1,7),I=391,400) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUAC2   ','BJ ','OH ','OK ','BJ ',    !Quercus acerifolia
     & '   ','   ','QUAC80  ','BJ ','OH ','OK ','BJ ',    !Quercus acutissima
     & 'WO ','802','QUAL    ','WO ','WO ','WO ','WO ',    !Quercus alba
     & '   ','803','QUAR    ','BJ ','OH ','OK ','BJ ',    !Quercus arizonica
     & '   ','   ','QUAR2   ','BJ ','OH ','OK ','BJ ',    !Quercus arkansana
     & '   ','   ','QUAS3   ','BJ ','OH ','OK ','BJ ',    !Quercus ashei
     & '   ','   ','QUAU    ','BJ ','OH ','OK ','BJ ',    !Quercus austrina
     & '   ','   ','QUBE    ','BJ ','OH ','OK ','BJ ',    !Quercus beadlei
     & '   ','   ','QUBE3   ','BJ ','OH ','OK ','BJ ',    !Quercus bebbiana
     & 'SW ','804','QUBI    ','SW ','SW ','SW ','WO '/    !Quercus bicolor
C
      DATA ((ASPT(I,J),J=1,7),I=401,410) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUBO2   ','PO ','BR ','PO ','PO ',    !Quercus boyntonii
     & '   ','   ','QUBU2   ','SO ','BO ','SO ','SO ',    !Quercus buckleyi
     & '   ','   ','QUBU    ','PO ','BR ','PO ','PO ',    !Quercus bushii
     & '   ','   ','QUCA    ','SO ','BO ','SO ','SO ',    !Quercus caduca
     & '   ','   ','QUCA2   ','SO ','BO ','SO ','SO ',    !Quercus capesii
     & '   ','   ','QUCH    ','PO ','BR ','PO ','PO ',    !Quercus chapmanii
     & 'SO ','806','QUCO2   ','SO ','BO ','SO ','SO ',    !Quercus coccinea
     & '   ','   ','QUCO5   ','BO ','BO ','BO ','BO ',    !Quercus cocksii
     & '   ','   ','QUCO3   ','BJ ','OH ','OK ','LO ',    !Quercus comptoniae
     & '   ','   ','QUCR    ','BJ ','OH ','OK ','TO '/    !Quercus cravenensis
C
      DATA ((ASPT(I,J),J=1,7),I=411,420) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUDE2   ','WK ','BO ','WK ','WK ',    !Quercus demareei
     & 'NP ','809','QUEL    ','PN ','NP ','PN ','SK ',    !Quercus ellipsoidalis
     & '   ','810','QUEM    ','BJ ','OH ','OK ','BJ ',    !Quercus emoryi
     & 'SK ','812','QUFA    ','SK ','BO ','SK ','SK ',    !Quercus falcata
     & '   ','   ','QUFU    ','BJ ','OH ','OK ','LO ',    !Quercus fusiformis
     & '   ','814','QUGA    ','BJ ','OH ','OK ','BJ ',    !Quercus gambelii
     & '   ','   ','QUGA3   ','SK ','BO ','SK ','SK ',    !Quercus garlandensis
     & '   ','   ','QUGE2   ','BJ ','OH ','OK ','LO ',    !Quercus geminata
     & '   ','   ','QUGR3   ','BJ ','OH ','OK ','BJ ',    !Quercus grisea
     & '   ','   ','QUHE2   ','BJ ','OH ','OK ','LO '/    !Quercus hemisphaerica
C
      DATA ((ASPT(I,J),J=1,7),I=421,430) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUHEH   ','BJ ','OH ','OK ','LO ',    !Quercus hemisphaerica va. Maritima
     & '   ','   ','QUHY    ','BJ ','OH ','OK ','BJ ',    !Quercus hypoleucoides
     & '   ','816','QUIL    ','BJ ','OH ','OK ','BJ ',    !Quercus ilicifolia
     & 'QI ','817','QUIM    ','QI ','BO ','QI ','WK ',    !Quercus imbricaria
     & 'QN ','842','QUIN    ','BJ ','OH ','OK ','TO ',    !Quercus incana
     & '   ','   ','QUJO4   ','SK ','BO ','SK ','SK ',    !Quercus joorii
     & 'TO ','819','QULA2   ','BJ ','OH ','OK ','TO ',    !Quercus laevis
     & 'LK ','820','QULA3   ','WK ','BO ','WK ','LK ',    !Quercus laurifolia
     & 'OV ','822','QULY    ','OV ','BR ','OK ','OV ',    !Quercus lyrata
     & '   ','   ','QUMA    ','PO ','BR ','PO ','PO '/    !Quercus macnabiana
C
      DATA ((ASPT(I,J),J=1,7),I=431,440) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'BR ','823','QUMA2   ','BR ','BR ','BR ','OV ',    !Quercus macrocarpa
     & '   ','   ','QUMAM   ','BR ','BR ','BR ','OV ',    !Quercus macrocarpa var. macrocarpa
     & 'DP ','840','QUMA6   ','PO ','BR ','PO ','TO ',    !Quercus margarettae
     & 'BJ ','824','QUMA3   ','BJ ','OH ','OK ','BJ ',    !Quercus marilandica
     & '   ','   ','QUMAM2  ','BJ ','OH ','OK ','BJ ',    !Quercus marilandica va. Marilandica
     & 'SN ','825','QUMI    ','SN ','WO ','SN ','SN ',    !Quercus michauxii
     & '   ','841','QUMI2   ','BJ ','OH ','OK ','TO ',    !Quercus minima
     & '   ','   ','QUMO    ','BJ ','OH ','OK ','BJ ',    !Quercus mohriana
     & '   ','   ','QUMO3   ','BJ ','OH ','OK ','BJ ',    !Quercus moultonensis
     & 'CK ','826','QUMU    ','CK ','CK ','CK ','CK '/    !Quercus muehlenbergii
C
      DATA ((ASPT(I,J),J=1,7),I=441,450) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUMY    ','BJ ','OH ','OK ','BJ ',    !Quercus myrtifolia
     & 'WK ','827','QUNI    ','WK ','BO ','WK ','WK ',    !Quercus nigra
     & '   ','829','QUOB    ','BJ ','OH ','OK ','BJ ',    !Quercus oblongifolia
     & '   ','844','QUOG    ','BJ ','OH ','OK ','LK ',    !Quercus oglethorpensis
     & 'CB ','813','QUPA5   ','CB ','BO ','CB ','CB ',    !Quercus pagoda
     & 'PN ','830','QUPA2   ','PN ','NP ','PN ','SK ',    !Quercus palustris
     & '   ','   ','QUPA4   ','BJ ','OH ','OK ','BJ ',    !Quercus pauciloba
     & 'WL ','831','QUPH    ','WL ','BO ','WL ','WK ',    !Quercus phellos
     & '   ','845','QUPR    ','BJ ','OH ','OK ','TO ',    !Quercus prinoides
     & 'CO ','832','QUPR2   ','CO ','WO ','CO ','CO '/    !Quercus prinus
C
      DATA ((ASPT(I,J),J=1,7),I=451,460) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUPS    ','BJ ','OH ','OK ','TO ',    !Quercus pseudomargaretta
     & '   ','   ','QUPU80  ','BJ ','OH ','OK ','TO ',    !Quercus pumila
     & '   ','   ','QURO2   ','WO ','WO ','WO ','WO ',    !Quercus robur
     & 'RO ','833','QURU    ','RO ','RO ','RO ','RO ',    !Quercus rubra
     & '   ','   ','QURUR   ','RO ','RO ','RO ','RO ',    !Quercus rubra var. rubra
     & '   ','   ','QURU4   ','BJ ','OH ','OK ','BJ ',    !Quercus rugosa
     & '   ','   ','QUSA    ','WO ','WO ','WO ','WO ',    !Quercus saulii
     & 'QS ','834','QUSH    ','QS ','BO ','SK ','QS ',    !Quercus shumardii
     & '   ','   ','QUSHS   ','QS ','BO ','SK ','QS ',    !Quercus shumardii var. schneckii
     & '   ','   ','QUSHS2  ','QS ','BO ','SK ','QS '/    !Quercus shumardii var.shumardii
C
      DATA ((ASPT(I,J),J=1,7),I=461,470) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'DO ','836','QUSI2   ','DO ','BR ','PO ','PO ',    !Quercus similis
     & '   ','   ','QUSI    ','PO ','BR ','PO ','PO ',    !Quercus sinuata
     & '   ','   ','QUSIB   ','PO ','BR ','PO ','PO ',    !Quercus sinuata var. breviloba
     & '   ','808','QUSIS   ','PO ','BR ','PO ','PO ',    !Quercus sinuata var. sinuata
     & 'PO ','835','QUST    ','PO ','BR ','PO ','PO ',    !Quercus stellata
     & '   ','   ','QUSU5   ','SK ','BO ','SK ','SK ',    !Quercus suber
     & '   ','   ','QUSU6   ','SK ','BO ','SK ','SK ',    !Quercus subfalcata
     & '   ','   ','QUSU2   ','SK ','BO ','SK ','SK ',    !Quercus subintegra
     & '   ','   ','QUTA    ','BJ ','OH ','OK ','BJ ',    !Quercus tardifolia
     & 'NK ','828','QUTE    ','NK ','BO ','OK ','SK '/    !Quercus texana
C
      DATA ((ASPT(I,J),J=1,7),I=471,480) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','QUTO2   ','BJ ','OH ','OK ','BJ ',    !Quercus toumeyi
     & '   ','   ','QUTU2   ','BJ ','OH ','OK ','BJ ',    !Quercus turbinella
     & 'BO ','837','QUVE    ','BO ','BO ','BO ','BO ',    !Quercus velutina
     & 'LO ','838','QUVI    ','BJ ','OH ','OK ','LO ',    !Quercus virginiana
     & '   ','   ','ROBIN   ','BK ','BK ','BK ','BK ',    !Robinia
     & '   ','   ','ROAM2   ','BK ','BK ','BK ','BK ',    !Robinia ambigua
     & '   ','   ','ROHI    ','BK ','BK ','BK ','BK ',    !Robinia hispida
     & '   ','   ','ROHIF8  ','BK ','BK ','BK ','BK ',    !Robinia hispida var. fertilis
     & '   ','   ','ROHIH   ','BK ','BK ','BK ','BK ',    !Robinia hispida var. hispida
     & '   ','   ','RONE    ','BK ','BK ','BK ','BK '/    !Robinia neomexicana
C
      DATA ((ASPT(I,J),J=1,7),I=481,490) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'BK ','901','ROPS    ','BK ','BK ','BK ','BK ',    !Robinia pseudoacacia
     & '   ','   ','ROVI    ','BK ','BK ','BK ','BK ',    !Robinia viscosa
     & '   ','   ','ROVIH2  ','BK ','BK ','BK ','BK ',    !Robinia viscosa var. hartwegi
     & '   ','   ','ROVIV   ','BK ','BK ','BK ','BK ',    !Robinia viscosa var. viscosa
     & 'WI ','920','SALIX   ','WI ','WI ','BL ','WI ',    !Salix
     & '   ','   ','SAAL    ','WI ','WI ','BL ','WI ',    !Salix alaxensis
     & '   ','927','SAAL2   ','WI ','WI ','BL ','WI ',    !Salix alba
     & '   ','921','SAAM2   ','WI ','WI ','BL ','WI ',    !Salix amygdaloides
     & '   ','925','SACA5   ','WI ','WI ','BL ','WI ',    !Salix caroliniana
     & '   ','   ','SACI    ','WI ','WI ','BL ','WI '/    !Salix cinerea
C
      DATA ((ASPT(I,J),J=1,7),I=491,500) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','SAEH    ','WI ','WI ','BL ','WI ',    !Salix ehrhartiana
     & 'DM ','923','SAER    ','WI ','DM ','BL ','WI ',    !Salix eriocephala
     & '   ','   ','SAEX    ','WI ','WI ','BL ','WI ',    !Salix exigua
     & '   ','   ','SAFL    ','WI ','WI ','BL ','WI ',    !Salix floridana
     & '   ','   ','SAFR    ','WI ','WI ','BL ','WI ',    !Salix fragilis
     & '   ','   ','SAGL3   ','WI ','WI ','BL ','WI ',    !Salix glatfelteri
     & '   ','   ','SAGO    ','WI ','WI ','BL ','WI ',    !Salix gooddingii
     & '   ','   ','SALA6   ','WI ','WI ','BL ','WI ',    !Salix lasiolepis
     & '   ','   ','SAMY2   ','WI ','WI ','BL ','WI ',    !Salix myricoides
     & '   ','   ','SAMYA   ','WI ','WI ','BL ','WI '/    !Salix myricoides var. albovestita
C
      DATA ((ASPT(I,J),J=1,7),I=501,510) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','SAMYM   ','WI ','WI ','BL ','WI ',    !Salix myricoides var. myricoides
     & 'BL ','922','SANI    ','BL ','BL ','BL ','WI ',    !Salix nigra
     & '   ','   ','SAPE3   ','WI ','WI ','BL ','WI ',    !Salix pellita
     & '   ','   ','SAPE12  ','WI ','WI ','BL ','WI ',    !Salix pendulina
     & '   ','   ','SAPE4   ','WI ','WI ','BL ','WI ',    !Salix pentandra
     & '   ','   ','SAPE5   ','WI ','WI ','BL ','WI ',    !Salix petiolaris
     & '   ','   ','SAPU2   ','WI ','WI ','BL ','WI ',    !Salix purpurea
     & '   ','926','SAPY    ','WI ','WI ','BL ','WI ',    !Salix pyrifolia
     & '   ','   ','SARU3   ','WI ','WI ','BL ','WI ',    !Salix rubens
     & '   ','929','SASE10  ','WI ','WI ','BL ','WI '/    !Salix sepulcralis
C
      DATA ((ASPT(I,J),J=1,7),I=511,520) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','SASE    ','WI ','WI ','BL ','WI ',    !Salix sericea
     & '   ','   ','SATA    ','WI ','WI ','BL ','WI ',    !Salix taxifolia
     & '   ','   ','SAVI2   ','WI ','WI ','BL ','WI ',    !Salix viminalis
     & '   ','   ','SASAD   ','WI ','WI ','BL ','WI ',    !Sapindus saponaria
     & 'SS ','931','SAAL5   ','SS ','SS ','SS ','SS ',    !Sassafras albidum
     & '   ','934','SORBU   ','OH ','MA ','OH ','OH ',    !Sorbus
     & 'MA ','935','SOAM3   ','OH ','MA ','OH ','OH ',    !Sorbus americana
     & '   ','936','SOAU    ','OH ','MA ','OH ','OH ',    !Sorbus aucuparia
     & '   ','220','TAXOD   ','BY ','OS ','WC ','BY ',    !Taxodium
     & 'PC ','222','TAAS    ','BY ','OS ','WC ','PC '/    !Taxodium ascendens
C
      DATA ((ASPT(I,J),J=1,7),I=521,530) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'BY ','221','TADI2   ','BY ','OS ','WC ','BY ',    !Taxodium distichum
     & '   ','230','TAXUS   ','OS ','OS ','OS ','OS ',    !Taxus
     & '   ','232','TAFL    ','OS ','OS ','OS ','OS ',    !Taxus floridana
     & 'OC ','240','THUJA   ','OS ','WC ','WC ','JU ',    !Thuja
     & 'WC ','241','THOC2   ','OS ','WC ','WC ','JU ',    !Thuja occidentalis
     & 'BD ','950','TILIA   ','BW ','BW ','BW ','BD ',    !Tilia
     & 'BW ','951','TIAM    ','BW ','BW ','BW ','BD ',    !Tilia americana
     & '   ','   ','TIAMA   ','BW ','BW ','BW ','BD ',    !Tilia americana var. americana
     & '   ','953','TIAMC   ','BW ','BW ','BW ','BD ',    !Tilia americana var. caroliniana
     & 'WB ','952','TIAMH   ','BW ','BW ','BW ','BD '/    !Tilia americana var. heterophylla
C
      DATA ((ASPT(I,J),J=1,7),I=531,540) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','TICO2   ','BW ','BW ','BW ','BD ',    !Tilia cordata
     & '   ','   ','TIEU4   ','BW ','BW ','BW ','BD ',    !Tilia europaea
     & '   ','   ','TIHE    ','BW ','BW ','BW ','BD ',    !Tilia heterophylla
     & 'HM ','260','TSUGA   ','OS ','EH ','HM ','HM ',    !Tsuga
     & 'EH ','261','TSCA    ','OS ','EH ','EH ','HM ',    !Tsuga canadensis
     & '   ','262','TSCA2   ','OS ','EH ','HM ','HM ',    !Tsuga caroliniana
     & 'EL ','970','ULMUS   ','EL ','AE ','EL ','EL ',    !Ulmus
     & 'WE ','971','ULAL    ','WE ','AE ','EL ','WE ',    !Ulmus alata
     & 'AE ','972','ULAM    ','AE ','AE ','AE ','AE ',    !Ulmus americana
     & '   ','973','ULCR    ','EL ','RL ','EL ','EL '/    !Ulmus crassifolia
C
      DATA ((ASPT(I,J),J=1,7),I=541,550) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','ULPA    ','EL ','RL ','EL ','EL ',    !Ulmus parvifolia
     & '   ','   ','ULPR    ','EL ','RL ','EL ','EL ',    !Ulmus procera
     & 'SI ','974','ULPU    ','SI ','AE ','EL ','EL ',    !Ulmus pumila
     & 'RL ','975','ULRU    ','RL ','RL ','RL ','RL ',    !Ulmus rubra
     & '   ','976','ULSE    ','EL ','RL ','EL ','EL ',    !Ulmus serotina
     & 'RE ','977','ULTH    ','RE ','RE ','EL ','EL ',    !Ulmus thomasii
     & '   ','299','        ','OS ','OS ','OS ','OS ',    !
     & '   ','998','        ','OH ','OH ','OH ','OH ',    !
     & '   ','999','        ','OH ','OH ','OH ','OT ',    !
     & 'CH ','   ','        ','OH ','OH ','OH ','OH '/    !
C
      DATA ((ASPT(I,J),J=1,7),I=551,560) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & 'HS ','   ','        ','HI ','BH ','HI ','HI ',    !
     & 'NC ','   ','        ','OH ','OH ','OH ','OH ',    !
     & 'OH ','   ','        ','OH ','OH ','OH ','OH ',    !
     & 'OL ','   ','        ','OH ','OH ','OH ','OH ',    !
     & 'OS ','   ','        ','OS ','OS ','OS ','OS ',    !
     & 'OT ','   ','        ','OH ','OH ','OH ','OT ',    !
     & 'UH ','   ','        ','OH ','OH ','OH ','OH ',    !
     & '   ','   ','2TB     ','OH ','OH ','OH ','OH ',    !
     & '   ','   ','2TD     ','OH ','OH ','OH ','OH ',    !
     & '   ','   ','2TE     ','OS ','OS ','OS ','OS '/    !
C
      DATA ((ASPT(I,J),J=1,7),I=561,562) /
C      ALFA   FIA   PLNT       CS    LS    NE    SN       SPECIES
     & '   ','   ','2TN     ','OS ','OS ','OS ','OS ',    !
     & '   ','   ','2TREE   ','OH ','OH ','OH ','OT '/    !
C
C----------
C  INITIALIZATIONS
C----------
      VAR=VARACD
      IJSPIN=3
      ISPC1= 0
      SPCOUT= 'XX '
C----------
C  TRANSLATE INCOMING SPECIES, SPCIN, TO VARIANT SPECIFIC RECOGNIZED
C  SPECIES, SPCOUT.
C----------
      DO 100 I= 1,MAXASPT
      IF (SPCIN .EQ. ASPT(I,1)) THEN
        IJSPIN=1
C
        SELECT CASE (VAR)
C
          CASE('CS')
            SPCOUT= ASPT(I,4)(1:3)
C
          CASE('LS')
            SPCOUT= ASPT(I,5)(1:3)
C
          CASE('NE')
            SPCOUT= ASPT(I,6)(1:3)
C
          CASE('SN')
            SPCOUT= ASPT(I,7)(1:3)
C
        END SELECT 
C
        GO TO 150        
      ELSEIF (SPCIN .EQ. ASPT(I,2)) THEN
        IJSPIN=2
C
        SELECT CASE (VAR)
C
          CASE('CS')
            SPCOUT= ASPT(I,4)(1:3)
C
          CASE('LS')
            SPCOUT= ASPT(I,5)(1:3)
C
          CASE('NE')
            SPCOUT= ASPT(I,6)(1:3)
C
          CASE('SN')
            SPCOUT= ASPT(I,7)(1:3)
C
        END SELECT
C
        GO TO 150   
      ELSEIF (SPCIN .EQ. ASPT(I,3)) THEN
        IJSPIN=3
C
        SELECT CASE (VAR)
C
          CASE('CS')
            SPCOUT= ASPT(I,4)(1:3)
C
          CASE('LS')
            SPCOUT= ASPT(I,5)(1:3)
C
          CASE('NE')
            SPCOUT= ASPT(I,6)(1:3)
C
          CASE('SN')
            SPCOUT= ASPT(I,7)(1:3)
C
        END SELECT
C
        GO TO 150   
      ENDIF      
  100 CONTINUE 
  150 CONTINUE                  
C----------
C  FIND STANDARD SPECIES NUMBER FOR VARIANT TO RETURN TO INTREE.
C  IF SPECIES CODE WAS NOT SET, RETURN NON COMMERCIAL (CS, LS, NE VARIANTS)
C  OR OTHER TREE (SN VARIANT)
C----------
      ISPC1= 0
      IF (SPCOUT.EQ.'XX ') THEN
        IF(VAR.EQ.'CS') ISPC1=85
        IF(VAR.EQ.'LS') ISPC1=49
        IF(VAR.EQ.'NE') ISPC1=98
        IF(VAR.EQ.'SN') ISPC1=90
        GO to 300
      ENDIF
      DO 200 J2= 1,MAXSP
      IF (SPCOUT(1:2).EQ.NSP(J2,1)(1:2)) THEN
        ISPC1= J2
        GO TO 300
      ENDIF
  200 CONTINUE
  300 CONTINUE
      IF(JSPINDEF.LE.0)JSPINDEF=IJSPIN
      JSPIN(ISPC1)=IJSPIN
      RETURN
      END
C