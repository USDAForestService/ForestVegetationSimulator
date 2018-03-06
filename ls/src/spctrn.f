      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C LS $Id$
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
      CHARACTER VVER*7
      CHARACTER*(*)SPCIN
      INTEGER MAXASPT, ISPC1, I, J, J2, IJSPIN
      PARAMETER (MAXASPT=466)
      CHARACTER*3 SPCOUT
      CHARACTER*8 ASPT(MAXASPT,8)
      CHARACTER VAR*2
C----------
C  DATA STATEMENT
C----------
C
      DATA ((ASPT(I,J),J=1,8),I=1,10) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'BF ','012','ABBA    ','OS ','BF ','BF ','EH ','FR ',    !Abies balsamea (L.) Mill.
     & '   ','015','ABCO    ','OS ','BF ','BF ','EH ','FR ',    !Abies concolor (Gord. & Glend.) Lindl. ex Hildebr.
     & '   ','016','ABFR    ','OS ','BF ','BF ','EH ','FR ',    !Abies fraseri (Pursh) Poir.
     & 'FR ','010','ABIES   ','OS ','BF ','BF ','EH ','FR ',    !Abies Mill.
     & '   ','303','ACFA    ','NC ','NC ','NC ','NC ','OH ',    !Acacia farnesiana (L.) Willd.
     & '   ','304','ACGR    ','NC ','NC ','NC ','NC ','OH ',    !Acacia greggii A. Gray
     & '   ','300','ACACI   ','NC ','NC ','NC ','NC ','OH ',    !Acacia Mill.
     & 'FM ','311','ACBA3   ','SM ','SM ','SM ','SM ','FM ',    !Acer barbatum Michx.
     & '   ','321','ACGL    ','BE ','MM ','BE ','BE ','BE ',    !Acer glabrum Torr.
     & '   ','322','ACGR3   ','BE ','MM ','BE ','BE ','BE '/    !Acer grandidentatum Nutt.
C
      DATA ((ASPT(I,J),J=1,8),I=11,20) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'MP ','310','ACER    ','RM ','SM ','SM ','MP ','RM ',    !Acer L.
     & 'CM ','323','ACLE    ','SM ','SM ','SM ','CM ','SM ',    !Acer leucoderme Small
     & 'BE ','313','ACNE2   ','BE ','BE ','BE ','BE ','BE ',    !Acer negundo L.
     & 'BM ','314','ACNI5   ','SM ','BM ','BM ','SM ','SM ',    !Acer nigrum Michx. f.
     & 'ST ','315','ACPE    ','BE ','ST ','ST ','BE ','BE ',    !Acer pensylvanicum L.
     & '   ','320','ACPL    ','SM ','SM ','SM ','SM ','SM ',    !Acer platanoides L.
     & 'RM ','316','ACRU    ','RM ','RM ','RM ','RM ','RM ',    !Acer rubrum L.
     & 'SV ','317','ACSA2   ','SV ','SV ','SV ','SV ','SV ',    !Acer saccharinum L.
     & 'SM ','318','ACSA3   ','SM ','SM ','SM ','SM ','SM ',    !Acer saccharum Marsh.
     & 'MM ','319','ACSP2   ','BE ','MM ','BE ','BE ','BE '/    !Acer spicatum Lam.
C
      DATA ((ASPT(I,J),J=1,8),I=21,30) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','906','ACWR4   ','NC ','NC ','NC ','NC ','OT ',    !Acoelorrhaphe wrightii (Griseb. & H. Wendl.) H. Wendl. ex Becc.
     & 'YY ','332','AEFL    ','OB ','CH ','YY ','OB ','BU ',    !Aesculus flava Aiton
     & 'OB ','331','AEGL    ','OB ','CH ','BU ','OB ','BU ',    !Aesculus glabra Willd.
     & '   ','334','AEGLA   ','NC ','NC ','NC ','NC ','OH ',    !Aesculus glabra Willd. var. arguta (Buckley) B.L. Rob.
     & 'BU ','330','AESCU   ','OB ','CH ','BU ','OB ','BU ',    !Aesculus L.
     & '   ','336','AEPA    ','NC ','NC ','NC ','NC ','OH ',    !Aesculus pavia L.
     & '   ','337','AESY    ','NC ','NC ','NC ','NC ','OH ',    !Aesculus sylvatica Bartram
     & 'AI ','341','AIAL    ','NC ','NC ','AI ','NC ','OH ',    !Ailanthus altissima (Mill.) Swingle
     & '   ','345','ALJU    ','NC ','NC ','NC ','NC ','OH ',    !Albizia julibrissin Durazz.
     & '   ','355','ALGL2   ','NC ','NC ','NC ','NC ','OH '/    !Alnus glutinosa (L.) Gaertn.
C
      DATA ((ASPT(I,J),J=1,8),I=31,40) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','ALIN2   ','NC ','NC ','NC ','NC ','OH ',    !Alnus incana (L.) Moench
     & '   ','   ','ALINR   ','NC ','NC ','NC ','NC ','OH ',    !Alnus incana (L.) Moench ssp. rugosa (Du Roi) R.T. Clausen
     & '   ','350','ALNUS   ','NC ','NC ','NC ','NC ','OH ',    !Alnus Mill.
     & '   ','   ','ALVI5   ','NC ','NC ','NC ','NC ','OH ',    !Alnus viridis (Chaix) DC.
     & '   ','   ','ALVIC   ','NC ','NC ','NC ','NC ','OH ',    !Alnus viridis (Chaix) DC. ssp. crispa (Aiton) Turrill
     & '   ','357','AMAR3   ','NC ','NC ','SE ','NC ','OH ',    !Amelanchier arborea (Michx. f.) Fernald
     & '   ','   ','AMARA4  ','NC ','NC ','SE ','NC ','OH ',    !Amelanchier arborea (Michx. f.) Fernald var. arborea
     & 'SE ','356','AMELA   ','NC ','NC ','SE ','NC ','OH ',    !Amelanchier Medik.
     & '   ','358','AMSA    ','NC ','NC ','SE ','NC ','OH ',    !Amelanchier sanguinea (Pursh) DC.
     & '   ','852','AMEL    ','NC ','NC ','NC ','NC ','OT '/    !Amyris elemifera L.
C
      DATA ((ASPT(I,J),J=1,8),I=41,50) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','853','ANGL4   ','NC ','NC ','NC ','NC ','OT ',    !Annona glabra L.
     & '   ','   ','ARSP2   ','NC ','NC ','NC ','NC ','OH ',    !Aralia spinosa L.
     & '   ','   ','ARXA80  ','NC ','NC ','NC ','NC ','OT ',    !Arbutus xalapensis Kunth
     & '   ','915','ARECA   ','NC ','NC ','NC ','NC ','OT ',    !Areca L.
     & '   ','   ','ASOB6   ','NC ','NC ','NC ','NC ','OH ',    !Asimina obovata (Willd.) Nash
     & '   ','367','ASTR    ','NC ','NC ','NC ','NC ','OH ',    !Asimina triloba (L.) Dunal
     & '   ','986','AVGE    ','NC ','NC ','NC ','NC ','OH ',    !Avicennia germinans (L.) L.
     & 'YB ','371','BEAL2   ','RB ','YB ','YB ','YB ','BB ',    !Betula alleghaniensis Britton
     & 'BB ','370','BETUL   ','RB ','PB ','YB ','BB ','BB ',    !Betula L.
     & 'SB ','372','BELE    ','RB ','YB ','SB ','SB ','SB '/    !Betula lenta L.
C
      DATA ((ASPT(I,J),J=1,8),I=51,60) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'RB ','373','BENI    ','RB ','PB ','RB ','RB ','BB ',    !Betula nigra L.
     & 'WR ','374','BEOC2   ','RB ','PB ','WR ','BB ','BB ',    !Betula occidentalis Hook.
     & 'PB ','375','BEPA    ','RB ','PB ','PB ','BB ','BB ',    !Betula papyrifera Marsh.
     & '   ','   ','BEPAC   ','RB ','PB ','PB ','BB ','BB ',    !Betula papyrifera var. commutata
     & 'GB ','379','BEPO    ','RB ','YB ','GB ','BB ','BB ',    !Betula populifolia Marsh.
     & '   ','   ','BEPU4   ','RB ','PB ','YB ','BB ','BB ',    !Betula pumila L.
     & '   ','377','BEUB    ','RB ','PB ','RB ','BB ','BB ',    !Betula uber (Ashe) Fernald
     & '   ','   ','BRPO3   ','NC ','NC ','NC ','NC ','OH ',    !Brunfelsia portoricensis Krug & Urb.
     & '   ','854','BUSI    ','NC ','NC ','NC ','NC ','OT ',    !Bursera simaruba (L.) Sarg.
     & '   ','   ','CALU12  ','NC ','NC ','NC ','NC ','OH '/    !Calyptranthes luquillensis Alain
C
      DATA ((ASPT(I,J),J=1,8),I=61,70) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','CAWO5   ','NC ','NC ','NC ','NC ','OH ',    !Calyptranthes woodburyi Alain
     & 'AH ','391','CACA18  ','AH ','AH ','AH ','AH ','AH ',    !Carpinus caroliniana Walter
     & 'MH ','409','CAAL27  ','MH ','PH ','MH ','MH ','HI ',    !Carya alba (L.) Nutt.
     & 'WH ','401','CAAQ2   ','WH ','BH ','HI ','WH ','HI ',    !Carya aquatica (Michx. f.) Nutt.
     & '   ','413','CACA38  ','SH ','SH ','HI ','HI ','HI ',    !Carya carolinae-septentrionalis (Ashe) Engl. & Graebn.
     & 'BH ','402','CACO15  ','BH ','BH ','HI ','BH ','HI ',    !Carya cordiformis (Wangenh.) K. Koch
     & '   ','411','CAFL6   ','BI ','PH ','HI ','HI ','HI ',    !Carya floridana Sarg.
     & 'PH ','403','CAGL8   ','PH ','PH ','PH ','PH ','HI ',    !Carya glabra (Mill.) Sweet
     & 'PE ','404','CAIL2   ','PE ','BH ','HI ','PE ','HI ',    !Carya illinoinensis (Wangenh.) K. Koch
     & 'SL ','405','CALA21  ','SL ','SH ','SL ','SL ','HI '/    !Carya laciniosa (Michx. f.) G. Don
C
      DATA ((ASPT(I,J),J=1,8),I=71,80) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','406','CAMY    ','PE ','BH ','HI ','HI ','HI ',    !Carya myristiciformis (Michx. f.) Nutt.
     & 'HI ','400','CARYA   ','HI ','BH ','HI ','HI ','HI ',    !Carya Nutt.
     & '   ','412','CAOV3   ','PH ','PH ','HI ','HI ','HI ',    !Carya ovalis (Wangenh.) Sarg.
     & 'SH ','407','CAOV2   ','SH ','SH ','SH ','SH ','HI ',    !Carya ovata (Mill.) K. Koch
     & '   ','410','CAPA24  ','PH ','PH ','HI ','HI ','HI ',    !Carya pallida (Ashe) Engl. & Graebn.
     & 'HS ','   ','        ','HS ','BH ','HI ','HI ','HI ',    !Carya select
     & 'BI ','408','CATE9   ','BI ','PH ','HI ','BI ','HI ',    !Carya texana Buckley
     & 'AC ','421','CADE12  ','NC ','AC ','NC ','NC ','OH ',    !Castanea dentata (Marsh.) Borkh.
     & '   ','420','CASTA   ','NC ','AC ','NC ','NC ','OH ',    !Castanea Mill.
     & '   ','424','CAMO83  ','NC ','AC ','NC ','NC ','OH '/    !Castanea mollissima Blume
C
      DATA ((ASPT(I,J),J=1,8),I=81,90) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','422','CAPU9   ','NC ','AC ','NC ','NC ','OH ',    !Castanea pumila (L.) Mill.
     & '   ','423','CAPUO   ','NC ','AC ','NC ','NC ','OH ',    !Castanea pumila (L.) Mill. var. ozarkensis (Ashe) Tucker
     & '   ','856','CAGL11  ','NC ','NC ','NC ','NC ','OT ',    !Casuarina glauca Siebold ex Spreng.
     & '   ','857','CALE28  ','NC ','NC ','NC ','NC ','OT ',    !Casuarina lepidophloia F. Muell.
     & '   ','855','CASUA   ','NC ','NC ','NC ','NC ','OT ',    !Casuarina Rumph. ex L.
     & '   ','451','CABI8   ','CA ','CH ','OH ','CA ','CA ',    !Catalpa bignonioides Walter
     & 'CA ','450','CATAL   ','CA ','CH ','OH ','CA ','CA ',    !Catalpa Scop.
     & '   ','452','CASP8   ','CA ','CH ','OH ','CA ','CA ',    !Catalpa speciosa (Warder) Warder ex Engelm.
     & 'HB ','460','CELTI   ','HK ','HK ','HK ','HK ','HB ',    !Celtis L.
     & 'SG ','461','CELA    ','SG ','HK ','HK ','SG ','HB '/    !Celtis laevigata Willd.
C
      DATA ((ASPT(I,J),J=1,8),I=91,100) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','463','CELAR   ','SG ','HK ','HK ','SG ','HB ',    !Celtis laevigata Willd. var. reticulata (Torr.) L.D. Benson
     & 'HK ','462','CEOC    ','HK ','HK ','HK ','HK ','HB ',    !Celtis occidentalis L.
     & '   ','   ','CEPHA   ','NC ','NC ','NC ','NC ','OH ',    !Cephalanthus L.
     & 'RD ','471','CECA4   ','RD ','NC ','NC ','RD ','RD ',    !Cercis canadensis L.
     & '   ','   ','CEMO2   ','NC ','NC ','NC ','NC ','OT ',    !Cercocarpus montanus Raf.
     & '   ','   ','CEMOP   ','NC ','NC ','NC ','NC ','OT ',    !Cercocarpus montanus Raf. var. paucidentatus (S. Watson) F.L. Martin
     & '   ','040','CHAMA4  ','OS ','OS ','AW ','JU ','JU ',    !Chamaecyparis Spach
     & 'AW ','043','CHTH2   ','OS ','OS ','AW ','JU ','JU ',    !Chamaecyparis thyoides (L.) Britton, Sterns & Poggenb.
     & '   ','858','CICA    ','NC ','NC ','NC ','NC ','OT ',    !Cinnamomum camphora (L.) J. Presl
     & '   ','859','CIFR    ','NC ','NC ','NC ','NC ','OT '/    !Citharexylum fruticosum
C
      DATA ((ASPT(I,J),J=1,8),I=101,110) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','860','CITRU2  ','NC ','NC ','NC ','NC ','OT ',    !Citrus L.
     & '   ','481','CLKE    ','OL ','CH ','OH ','NC ','OH ',    !Cladrastis kentukea (Dum. Cours.) Rudd
     & '   ','   ','CLADR   ','OL ','CH ','OH ','NC ','OH ',    !Cladrastis Raf.
     & '   ','863','CODI8   ','NC ','NC ','NC ','NC ','OT ',    !Coccoloba diversifolia Jacq.
     & '   ','907','COAR    ','NC ','NC ','NC ','NC ','OT ',    !Coccothrinax argentata (Jacq.) L.H. Bailey
     & '   ','908','CONU    ','NC ','NC ','NC ','NC ','OT ',    !Cocos nucifera L.
     & '   ','864','COEL2   ','NC ','NC ','NC ','NC ','OT ',    !Colubrina elliptica (Sw.) Briz. & Stern
     & '   ','987','COER2   ','NC ','NC ','NC ','NC ','OH ',    !Conocarpus erectus L.
     & '   ','   ','CORU17  ','NC ','NC ','NC ','NC ','OH ',    !Conostegia rufescens Naud.
     & '   ','865','COSE2   ','NC ','NC ','NC ','NC ','OT '/    !Cordia sebestena L.
C
      DATA ((ASPT(I,J),J=1,8),I=111,120) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','COAL2   ','DW ','DW ','DW ','DW ','DW ',    !Cornus alternifolia L. f.
     & 'DW ','491','COFL2   ','DW ','DW ','DW ','DW ','DW ',    !Cornus florida L.
     & '   ','490','CORNU   ','DW ','DW ','DW ','DW ','DW ',    !Cornus L.
     & '   ','   ','CORU    ','DW ','DW ','DW ','DW ','DW ',    !Cornus rugosa Lam.
     & '   ','   ','COSES   ','NC ','NC ','NC ','NC ','OH ',    !Cornus sericea L.
     & '   ','   ','COSE16  ','DW ','DW ','DW ','DW ','DW ',    !Cornus sericea L. ssp. sericea
     & '   ','   ','COAM3   ','NC ','NC ','NC ','NC ','OT ',    !Corylus americana Walter
     & '   ','   ','COCO6   ','NC ','NC ','NC ','NC ','OT ',    !Corylus cornuta Marsh.
     & '   ','   ','CORYL   ','NC ','NC ','NC ','NC ','OT ',    !Corylus L.
     & '   ','996','COOB2   ','NC ','NC ','NC ','NC ','OH '/    !Cotinus obovatus Raf.
C
      DATA ((ASPT(I,J),J=1,8),I=121,130) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','503','CRBR3   ','HT ','HT ','HT ','HT ','OH ',    !Crataegus brainerdii Sarg.
     & '   ','504','CRCA    ','HT ','HT ','HT ','HT ','OH ',    !Crataegus calpodendron (Ehrh.) Medik.
     & '   ','505','CRCH    ','HT ','HT ','HT ','HT ','OH ',    !Crataegus chrysocarpa Ashe
     & '   ','501','CRCR2   ','HT ','HT ','HT ','HT ','OH ',    !Crataegus crus-galli L.
     & '   ','506','CRDI    ','HT ','HT ','HT ','HT ','OH ',    !Crataegus dilatata Sarg.
     & '   ','507','CRFL    ','HT ','HT ','HT ','HT ','OH ',    !Crataegus flabellata (Bosc ex Spach) K. Koch
     & 'HT ','500','CRATA   ','HT ','HT ','HT ','HT ','OH ',    !Crataegus L.
     & '   ','502','CRMO2   ','HT ','HT ','HT ','HT ','OH ',    !Crataegus mollis Scheele
     & '   ','508','CRMO3   ','HT ','HT ','HT ','HT ','OH ',    !Crataegus monogyna Jacq.
     & '   ','509','CRPE    ','HT ','HT ','HT ','HT ','OH '/    !Crataegus pedicellata Sarg.
C
      DATA ((ASPT(I,J),J=1,8),I=131,140) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','CRWA    ','HT ','HT ','HT ','HT ','OH ',    !Crataegus warneri Sarg.
     & '   ','866','CUAN4   ','NC ','NC ','NC ','NC ','OT ',    !Cupaniopsis anacardioides (A. Rich.) Radlk.
     & '   ','051','CUAR    ','JU ','RC ','OC ','JU ','JU ',    !Cupressus arizonica Greene
     & '   ','220','TAXOD   ','BY ','OS ','OC ','BY ','BY ',    !Cupressus L.
     & '   ','520','DIOSP   ','PS ','CH ','PS ','PS ','PS ',    !Diospyros L.
     & '   ','522','DITE3   ','PS ','CH ','PS ','PS ','PS ',    !Diospyros texana Scheele
     & 'PS ','521','DIVI5   ','PS ','CH ','PS ','PS ','PS ',    !Diospyros virginiana L.
     & '   ','   ','DIPA9   ','NC ','NC ','NC ','NC ','OT ',    !Dirca palustris L.
     & '   ','997','ELAN    ','NC ','NC ','NC ','NC ','OH ',    !Elaeagnus angustifolia L.
     & '   ','512','EUCA2   ','NC ','NC ','OH ','NC ','OH '/    !Eucalyptus camaldulensis Dehnh.
C
      DATA ((ASPT(I,J),J=1,8),I=141,150) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','513','EUGR12  ','NC ','NC ','OH ','NC ','OH ',    !Eucalyptus grandis W. Hill ex Maid.
     & '   ','510','EUCAL   ','NC ','NC ','OH ','NC ','OH ',    !Eucalyptus L'Hér.
     & '   ','514','EURO2   ','NC ','NC ','OH ','NC ','OH ',    !Eucalyptus robusta Sm.
     & '   ','   ','EUEG    ','NC ','NC ','NC ','NC ','OH ',    !Eugenia eggersii Kiaersk.
     & '   ','873','EURH    ','NC ','NC ','NC ','NC ','OT ',    !Eugenia rhombea (Berg) Krug & Urb.
     & '   ','874','EXPA    ','NC ','NC ','NC ','NC ','OT ',    !Exothea paniculata (Juss.) Radlk.
     & 'AB ','531','FAGR    ','AB ','AB ','AB ','AB ','AB ',    !Fagus grandifolia Ehrh.
     & '   ','   ','FAGUS   ','AB ','AB ','AB ','AB ','AB ',    !Fagus L.
     & '   ','876','FIAU    ','NC ','NC ','NC ','NC ','OT ',    !Ficus aurea Nutt.
     & '   ','   ','FICA    ','NC ','NC ','NC ','NC ','OT '/    !Ficus carica L.
C
      DATA ((ASPT(I,J),J=1,8),I=151,160) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','877','FICI    ','NC ','NC ','NC ','NC ','OT ',    !Ficus citrifolia Mill.
     & '   ','   ','FRAL4   ','NC ','NC ','NC ','NC ','OT ',    !Frangula alnus Mill.
     & '   ','   ','FRCA13  ','NC ','NC ','NC ','NC ','OT ',    !Frangula caroliniana (Walter) A. Gray
     & 'WA ','541','FRAM2   ','WA ','WA ','WA ','WA ','WA ',    !Fraxinus americana L.
     & '   ','548','FRCA3   ','AS ','GA ','AS ','AS ','AS ',    !Fraxinus caroliniana Mill.
     & 'AS ','540','FRAXI   ','AS ','GA ','AS ','AS ','AS ',    !Fraxinus L.
     & 'BA ','543','FRNI    ','BA ','BA ','BA ','BA ','BA ',    !Fraxinus nigra Marsh.
     & 'GA ','544','FRPE    ','GA ','GA ','GA ','GA ','GA ',    !Fraxinus pennsylvanica Marsh.
     & 'PA ','545','FRPR    ','PA ','WA ','PA ','PA ','AS ',    !Fraxinus profunda (Bush) Bush
     & 'UA ','546','FRQU    ','UA ','WA ','AS ','UA ','AS '/    !Fraxinus quadrangulata Michx.
C
      DATA ((ASPT(I,J),J=1,8),I=161,170) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','549','FRTE    ','AS ','GA ','AS ','AS ','AS ',    !Fraxinus texensis (A. Gray) Sarg.
     & '   ','547','FRVE2   ','AS ','GA ','AS ','AS ','AS ',    !Fraxinus velutina Torr.
     & '   ','561','GIBI2   ','UH ','CH ','OH ','NC ','OH ',    !Ginkgo biloba L.
     & '   ','551','GLAQ    ','HL ','BK ','BK ','HL ','HL ',    !Gleditsia aquatica Marsh.
     & '   ','550','GLEDI   ','HL ','BK ','BK ','HL ','HL ',    !Gleditsia L.
     & 'HL ','552','GLTR    ','HL ','BK ','BK ','HL ','HL ',    !Gleditsia triacanthos L.
     & 'LB ','555','GOLA    ','NC ','NC ','NC ','LB ','LB ',    !Gordonia lasianthus (L.) Ellis
     & '   ','882','GUDI    ','NC ','NC ','NC ','NC ','OT ',    !Guapira discolor (Spreng.) Little
     & 'KC ','571','GYDI    ','KC ','CH ','OH ','KC ','OH ',    !Gymnocladus dioicus (L.) K. Koch
     & '   ','581','HACA3   ','NC ','NC ','NC ','NC ','HA '/    !Halesia carolina L.
C
      DATA ((ASPT(I,J),J=1,8),I=171,180) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','582','HADI3   ','NC ','NC ','NC ','NC ','HA ',    !Halesia diptera Ellis
     & 'HA ','580','HALES   ','NC ','NC ','NC ','NC ','HA ',    !Halesia Ellis ex L.
     & '   ','583','HAPA2   ','NC ','NC ','NC ','NC ','HA ',    !Halesia parviflora
     & '   ','   ','HAMAM   ','NC ','NC ','NC ','NC ','OT ',    !Hamamelis L.
     & '   ','   ','HAVI4   ','NC ','NC ','NC ','NC ','OT ',    !Hamamelis virginiana L.
     & '   ','883','HIMA2   ','NC ','NC ','NC ','NC ','OT ',    !Hippomane mancinella L.
     & '   ','   ','ILCO2   ','NC ','NC ','HY ','HY ','HY ',    !Ilex collina Alexander
     & '   ','   ','ILLO    ','NC ','NC ','HY ','HY ','HY ',    !Ilex longipes Chapm. ex Trel.
     & '   ','   ','ILMO    ','NC ','NC ','HY ','HY ','HY ',    !Ilex montana Torr. & A. Gray ex A. Gray
     & '   ','   ','NEMU2   ','NC ','NC ','NC ','NC ','OT '/    !Ilex mucronata
C
      DATA ((ASPT(I,J),J=1,8),I=181,190) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'HY ','591','ILOP    ','NC ','NC ','HY ','HY ','HY ',    !Ilex opaca Aiton
     & '   ','   ','ILVE    ','NC ','NC ','NC ','NC ','OT ',    !Ilex verticillata (L.) A. Gray
     & '   ','   ','ILPA    ','NC ','NC ','NC ','NC ','OT ',    !Illicium parviflorum Michx. ex Vent.
     & 'BN ','601','JUCI    ','BN ','BN ','BN ','BN ','BN ',    !Juglans cinerea L.
     & '   ','600','JUGLA   ','WN ','WN ','WN ','WN ','WN ',    !Juglans L.
     & '   ','606','JUMA    ','WN ','WN ','WN ','WN ','WN ',    !Juglans major (Torr.) A. Heller
     & '   ','605','JUMI    ','WN ','WN ','WN ','WN ','WN ',    !Juglans microcarpa Berl.
     & 'WN ','602','JUNI    ','WN ','WN ','WN ','WN ','WN ',    !Juglans nigra L.
     & '   ','061','JUAS    ','JU ','RC ','OC ','JU ','JU ',    !Juniperus ashei J. Buchholz
     & '   ','059','JUCO11  ','JU ','RC ','OC ','JU ','JU '/    !Juniperus coahuilensis (Martiñez) Gaussen ex R.P. Adams
C
      DATA ((ASPT(I,J),J=1,8),I=191,200) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','JUCO6   ','JU ','RC ','OC ','JU ','JU ',    !Juniperus communis L.
     & '   ','   ','JUCOM2  ','JU ','RC ','OC ','JU ','JU ',    !Juniperus communis var. saxatilis
     & '   ','063','JUDE2   ','JU ','RC ','OC ','JU ','JU ',    !Juniperus deppeana Steud.
     & 'JU ','057','JUNIP   ','JU ','RC ','OC ','JU ','JU ',    !Juniperus L.
     & '   ','   ','JUMO    ','JU ','RC ','OC ','JU ','JU ',    !Juniperus monosperma (Engelm.) Sarg.
     & '   ','058','JUPI    ','JU ','RC ','OC ','JU ','JU ',    !Juniperus pinchotii Sudw.
     & '   ','066','JUSC2   ','JU ','RC ','OC ','JU ','JU ',    !Juniperus scopulorum Sarg.
     & 'RC ','068','JUVI    ','RC ','RC ','RC ','RC ','JU ',    !Juniperus virginiana L.
     & '   ','067','JUVIS   ','RC ','RC ','RC ','RC ','JU ',    !Juniperus virginiana L. var. silicicola (Small) J. Silba
     & '   ','   ','KALA    ','NC ','NC ','NC ','NC ','OH '/    !Kalmia latifolia L.
C
      DATA ((ASPT(I,J),J=1,8),I=201,210) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','988','LARA2   ','NC ','NC ','NC ','NC ','OH ',    !Laguncularia racemosa (L.) C.F. Gaertn.
     & '   ','   ','LAPO    ','NC ','NC ','NC ','NC ','OH ',    !Laplacea portoricensis (Krug & Urb.) Dyer
     & 'TA ','071','LALA    ','OS ','TA ','TA ','EH ','HM ',    !Larix laricina (Du Roi) K. Koch
     & '   ','070','LARIX   ','OS ','TA ','TA ','EH ','HM ',    !Larix Mill.
     & '   ','   ','LEFL    ','NC ','NC ','NC ','NC ','OT ',    !Leitneria floridana Chapm.
     & '   ','   ','LIBE3   ','NC ','NC ','NC ','NC ','OT ',    !Lindera benzoin (L.) Blume
     & '   ','   ','LISU8   ','NC ','NC ','NC ','NC ','OT ',    !Lindera subcoriacea B.E. Wofford
     & '   ','   ','LINDE2  ','NC ','NC ','NC ','NC ','OH ',    !Lindera Thunb.
     & 'SU ','611','LIST2   ','SU ','CH ','SU ','SU ','SU ',    !Liquidambar styraciflua L.
     & 'YP ','621','LITU    ','YP ','CH ','YP ','YP ','YP '/    !Liriodendron tulipifera L.
C
      DATA ((ASPT(I,J),J=1,8),I=211,220) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','884','LYLA3   ','NC ','NC ','NC ','NC ','OT ',    !Lysiloma latisiliquum (L.) Benth.
     & 'OO ','641','MAPO    ','OO ','CH ','OO ','OO ','OH ',    !Maclura pomifera (Raf.) C.K. Schneid.
     & 'CT ','651','MAAC    ','CT ','CH ','CT ','CT ','CT ',    !Magnolia acuminata (L.) L.
     & '   ','   ','MAAS    ','CT ','CH ','MG ','MG ','MG ',    !Magnolia ashei Weath.
     & '   ','655','MAFR    ','CT ','CH ','MG ','MG ','MG ',    !Magnolia fraseri Walter
     & 'MS ','652','MAGR4   ','CT ','CH ','MG ','MG ','MS ',    !Magnolia grandiflora L.
     & 'MG ','650','MAGNO   ','CT ','CH ','MG ','MG ','MG ',    !Magnolia L.
     & 'ML ','654','MAMA2   ','CT ','CH ','CT ','CT ','ML ',    !Magnolia macrophylla Michx.
     & '   ','657','MAPY    ','CT ','CH ','MG ','MG ','MG ',    !Magnolia pyramidata Bartram
     & '   ','658','MATR    ','CT ','CH ','MG ','MG ','MG '/    !Magnolia tripetala (L.) L.
C
      DATA ((ASPT(I,J),J=1,8),I=221,230) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'MV ','653','MAVI2   ','MV ','CH ','MV ','MV ','MV ',    !Magnolia virginiana L.
     & '   ','662','MAAN3   ','NC ','AP ','AP ','NC ','AP ',    !Malus angustifolia (Aiton) Michx.
     & '   ','663','MACO5   ','NC ','AP ','AP ','NC ','AP ',    !Malus coronaria (L.) Mill.
     & '   ','664','MAIO    ','NC ','AP ','AP ','NC ','AP ',    !Malus ioensis (Alph. Wood) Britton
     & 'AP ','660','MALUS   ','NC ','AP ','AP ','NC ','AP ',    !Malus Mill.
     & '   ','885','MAIN3   ','NC ','NC ','NC ','NC ','OT ',    !Mangifera indica L.
     & '   ','   ','MASI3   ','NC ','NC ','NC ','NC ','OH ',    !Marlierea sintenisii Kiaersk.
     & '   ','   ','MAEL3   ','NC ','NC ','NC ','NC ','OH ',    !Maytenus elongata (Urb.) Britton
     & '   ','992','MEQU    ','NC ','NC ','NC ','NC ','OH ',    !Melaleuca quinquenervia (Cav.) S.F. Blake
     & '   ','993','MEAZ    ','NC ','NC ','NC ','NC ','OH '/    !Melia azedarach L.
C
      DATA ((ASPT(I,J),J=1,8),I=231,240) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','886','METO3   ','NC ','NC ','NC ','NC ','OT ',    !Metopium toxiferum (L.) Krug & Urb.
     & '   ','   ','MIFO    ','NC ','NC ','NC ','NC ','OH ',    !Miconia foveolata Cogn.
     & '   ','   ','MIPY2   ','NC ','NC ','NC ','NC ','OH ',    !Miconia pycnoneura Urb.
     & '   ','   ','MOHO3   ','NC ','NC ','NC ','NC ','OH ',    !Morella holdridgeana (Lundell) Kartesz
     & 'WM ','681','MOAL    ','MB ','NC ','NC ','WM ','MB ',    !Morus alba L.
     & 'MB ','680','MORUS   ','MB ','NC ','NC ','MB ','MB ',    !Morus L.
     & '   ','683','MOMI    ','MB ','NC ','NC ','MB ','MB ',    !Morus microphylla Buckley
     & '   ','684','MONI    ','MB ','NC ','NC ','MB ','MB ',    !Morus nigra L.
     & 'RY ','682','MORU2   ','MB ','NC ','NC ','RY ','MB ',    !Morus rubra L.
     & 'NC ','   ','        ','NC ','NC ','NC ','NC ','OT '/    !Non-commercial species
C
      DATA ((ASPT(I,J),J=1,8),I=241,250) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'WT ','691','NYAQ2   ','WT ','BG ','WT ','WT ','WT ',    !Nyssa aquatica L.
     & 'TS ','694','NYBI    ','TS ','BG ','BG ','TS ','TS ',    !Nyssa biflora Walter
     & 'TL ','690','NYSSA   ','TL ','BG ','BG ','BG ','BG ',    !Nyssa L.
     & 'OG ','692','NYOG    ','WT ','BG ','WT ','OG ','WT ',    !Nyssa ogeche Bartram ex Marsh.
     & 'BG ','693','NYSY    ','BG ','BG ','BG ','BG ','BG ',    !Nyssa sylvatica Marsh.
     & '   ','   ','NYUR2   ','TL ','BG ','BG ','BG ','BG ',    !Nyssa ursina Small
     & 'HH ','701','OSVI    ','HH ','HH ','HH ','HH ','HH ',    !Ostrya virginiana (Mill.) K. Koch
     & 'OT ','999','2TREE   ','NC ','NC ','NC ','NC ','OT ',    !Other
     & 'CH ','   ','        ','UH ','CH ','OH ','NC ','OH ',    !Other Commercial Hardwoods
     & 'OH ','998','2TB     ','UH ','CH ','OH ','NC ','OH '/    !Other hardwood
C
      DATA ((ASPT(I,J),J=1,8),I=251,260) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','2TD     ','UH ','CH ','OH ','NC ','OH ',    !Other hardwood
     & 'OL ','   ','        ','OL ','CH ','OH ','NC ','OH ',    !Other lowland species
     & '   ','299','2TE     ','OS ','OS ','OS ','JU ','OS ',    !Other softwood
     & 'OS ','298','        ','OS ','OS ','OS ','JU ','OS ',    !Other softwood
     & 'UH ','   ','        ','UH ','CH ','OH ','NC ','OH ',    !Other upland hardwoods
     & 'SD ','711','OXAR    ','SD ','NC ','SD ','SD ','SD ',    !Oxydendrum arboreum (L.) DC.
     & 'PW ','712','PATO2   ','UH ','CH ','PW ','NC ','OH ',    !Paulownia tomentosa (Thunb.) Siebold & Zucc. ex Steud.
     & 'RA ','721','PEBO    ','NC ','NC ','NC ','RA ','RA ',    !Persea borbonia (L.) Spreng.
     & '   ','   ','PEHU2   ','NC ','NC ','NC ','NC ','OH ',    !Persea humilis Nash
     & '   ','720','PERSE   ','NC ','NC ','NC ','RA ','RA '/    !Persea Mill.
C
      DATA ((ASPT(I,J),J=1,8),I=261,270) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'PI ','090','PICEA   ','OS ','WS ','PI ','SR ','PI ',    !Picea A. Dietr.
     & 'NS ','091','PIAB    ','OS ','NS ','NS ','SR ','PI ',    !Picea abies (L.) Karst.
     & 'WS ','094','PIGL    ','OS ','WS ','WS ','SR ','PI ',    !Picea glauca (Moench) Voss
     & 'BS ','095','PIMA    ','OS ','BS ','BS ','SR ','PI ',    !Picea mariana (Mill.) Britton, Sterns & Poggenb.
     & '   ','096','PIPU    ','OS ','BS ','PI ','SR ','PI ',    !Picea pungens Engelm.
     & 'RS ','097','PIRU    ','OS ','BS ','RS ','SR ','PI ',    !Picea rubens Sarg.
     & '   ','   ','PIAR5   ','OS ','OS ','OP ','PZ ','OS ',    !Pinus arizonica Engelm.
     & 'JP ','105','PIBA2   ','VP ','JP ','JP ','VP ','PU ',    !Pinus banksiana Lamb.
     & 'PU ','107','PICL    ','VP ','JP ','OP ','VP ','PU ',    !Pinus clausa (Chapm. ex Engelm.) Vasey ex Sarg.
     & 'SP ','110','PIEC2   ','SP ','RN ','SP ','SP ','SP '/    !Pinus echinata Mill.
C
      DATA ((ASPT(I,J),J=1,8),I=271,280) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','106','PIED    ','VP ','JP ','OP ','VP ','PU ',    !Pinus edulis Engelm.
     & 'SA ','111','PIEL    ','LP ','RN ','LP ','SA ','SA ',    !Pinus elliottii Engelm.
     & '   ','144','PIELE2  ','LP ','RN ','OP ','SA ','SA ',    !Pinus elliottii Engelm. var. elliottii
     & 'SR ','115','PIGL2   ','SP ','JP ','SP ','SR ','SR ',    !Pinus glabra Walter
     & 'OP ','100','PINUS   ','OS ','WP ','OP ','LP ','LP ',    !Pinus L.
     & '   ','136','PINI    ','LP ','RN ','RN ','LP ','LP ',    !Pinus nigra Arnold
     & '   ','270','        ','LP ','RN ','RN ','LP ','LP ',    !Pinus nigra Arnold (old code)
     & 'LL ','121','PIPA2   ','LP ','RN ','LP ','LL ','LL ',    !Pinus palustris Mill.
     & 'PZ ','122','PIPO    ','LP ','RN ','RN ','PZ ','LP ',    !Pinus ponderosa C. Lawson
     & '   ','   ','PIPOS   ','LP ','RN ','RN ','PZ ','LP '/    !Pinus ponderosa C. Lawson var. scopulorum Engelm.
C
      DATA ((ASPT(I,J),J=1,8),I=281,290) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'TM ','123','PIPU5   ','VP ','JP ','TM ','PP ','TM ',    !Pinus pungens Lamb.
     & 'RN ','125','PIRE    ','LP ','RN ','RN ','LP ','LP ',    !Pinus resinosa Aiton
     & 'RP ','   ','        ','LP ','RP ','RN ','LP ','LP ',    !Pinus resinosa Aiton
     & 'PP ','126','PIRI    ','VP ','JP ','PP ','PP ','PP ',    !Pinus rigida Mill.
     & 'PD ','128','PISE    ','LP ','JP ','PD ','PD ','PD ',    !Pinus serotina Michx.
     & '   ','114','PIST3   ','WP ','WP ','WP ','WP ','WP ',    !Pinus strobiformis Engelm.
     & 'WP ','129','PIST    ','WP ','WP ','WP ','WP ','WP ',    !Pinus strobus L.
     & 'SC ','130','PISY    ','VP ','SC ','SC ','VP ','PU ',    !Pinus sylvestris L.
     & 'LP ','131','PITA    ','LP ','RN ','LP ','LP ','LP ',    !Pinus taeda L.
     & 'VP ','132','PIVI2   ','VP ','JP ','VP ','VP ','VP '/    !Pinus virginiana Mill.
C
      DATA ((ASPT(I,J),J=1,8),I=291,300) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','887','PIPI3   ','NC ','NC ','NC ','NC ','OT ',    !Piscidia piscipula (L.) Sarg.
     & '   ','722','PLAQ    ','NC ','NC ','NC ','NC ','OH ',    !Planera aquatica J.F. Gmel.
     & '   ','729','PLATA   ','SY ','SY ','SY ','SY ','SY ',    !Platanus L.
     & 'SY ','731','PLOC    ','SY ','SY ','SY ','SY ','SY ',    !Platanus occidentalis L.
     & '   ','   ','POAC5   ','EC ','EC ','EC ','CW ','CW ',    !Populus ×acuminata Rydb. (pro sp.) [angustifolia × deltoides]
     & '   ','752','POAL7   ','BP ','BP ','BP ','BP ','BT ',    !Populus alba L.
     & '   ','749','POAN3   ','EC ','EC ','EC ','CW ','CW ',    !Populus angustifolia James
     & 'BP ','741','POBA2   ','BP ','BP ','BP ','BP ','BT ',    !Populus balsamifera L.
     & 'EC ','742','PODE3   ','EC ','EC ','EC ','EC ','CW ',    !Populus deltoides Bartram ex Marsh.
     & '   ','745','PODEM   ','EC ','EC ','EC ','CW ','CW '/    !Populus deltoides Bartram ex Marsh. ssp. monilifera (Aiton) Eckenwalder
C
      DATA ((ASPT(I,J),J=1,8),I=301,310) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','PODEW   ','EC ','EC ','EC ','CW ','CW ',    !Populus deltoides Bartram ex Marsh. ssp. wislizeni (S. Watson) Eckenwalder
     & '   ','748','POFR2   ','EC ','EC ','EC ','CW ','CW ',    !Populus fremontii S. Watson
     & 'BT ','743','POGR4   ','BT ','BT ','BT ','BT ','BT ',    !Populus grandidentata Michx.
     & 'PY ','744','POHE4   ','EC ','EC ','PY ','CW ','CW ',    !Populus heterophylla L.
     & 'CW ','740','POPUL   ','EC ','EC ','EC ','CW ','CW ',    !Populus L.
     & '   ','753','PONI    ','BP ','BP ','BP ','BP ','BT ',    !Populus nigra L.
     & 'QA ','746','POTR5   ','QA ','QA ','QA ','QA ','BT ',    !Populus tremuloides Michx.
     & '   ','756','PRGL2   ','NC ','NC ','NC ','NC ','OH ',    !Prosopis glandulosa Torr.
     & '   ','755','PROSO   ','NC ','NC ','NC ','NC ','OH ',    !Prosopis L.
     & '   ','758','PRPU    ','NC ','NC ','NC ','NC ','OH '/    !Prosopis pubescens Benth.
C
      DATA ((ASPT(I,J),J=1,8),I=311,320) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','757','PRVE    ','NC ','NC ','NC ','NC ','OH ',    !Prosopis velutina Woot.
     & '   ','769','PRAL5   ','NC ','PL ','PL ','NC ','AP ',    !Prunus alleghaniensis Porter
     & '   ','766','PRAM    ','NC ','PL ','PL ','NC ','AP ',    !Prunus americana Marsh.
     & '   ','770','PRAN3   ','NC ','PL ','PL ','NC ','AP ',    !Prunus angustifolia Marsh.
     & '   ','771','PRAV    ','NC ','CC ','PR ','NC ','AP ',    !Prunus avium (L.) L.
     & '   ','772','PRCE    ','NC ','CC ','PR ','NC ','AP ',    !Prunus cerasus L.
     & '   ','773','PRDO    ','NC ','PL ','PL ','NC ','AP ',    !Prunus domestica L.
     & 'PL ','760','PRUNU   ','NC ','PL ','PL ','NC ','AP ',    !Prunus L.
     & '   ','774','PRMA    ','NC ','PL ','PL ','NC ','AP ',    !Prunus mahaleb L.
     & '   ','765','PRNI    ','NC ','PL ','PL ','NC ','AP '/    !Prunus nigra Aiton
C
      DATA ((ASPT(I,J),J=1,8),I=321,330) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'PR ','761','PRPE2   ','NC ','PR ','PR ','NC ','AP ',    !Prunus pensylvanica L. f.
     & '   ','764','PRPE3   ','NC ','PL ','PL ','NC ','AP ',    !Prunus persica (L.) Batsch
     & 'BC ','762','PRSE2   ','BC ','BC ','BC ','BC ','BC ',    !Prunus serotina Ehrh.
     & 'CC ','763','PRVI    ','NC ','CC ','PR ','NC ','AP ',    !Prunus virginiana L.
     & '   ','202','PSME    ','OS ','OS ','OS ','SR ','OS ',    !Pseudotsuga menziesii (Mirb.) Franco
     & '   ','   ','PSMEG   ','OS ','OS ','OS ','SR ','OS ',    !Pseudotsuga menziesii (Mirb.) Franco var. glauca (Beissn.) Franco
     & '   ','   ','PSSI2   ','NC ','NC ','NC ','NC ','OH ',    !Psidium sintenisii (Kiaersk.) Alain
     & '   ','   ','QUPA4   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus ×pauciloba Rydb. (pro sp.) [gambelii × turbinella]
     & '   ','   ','QUAC2   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus acerifolia (Palmer) Stoynoff & Hess
     & 'WO ','802','QUAL    ','WO ','WO ','WO ','WO ','WO '/    !Quercus alba L.
C
      DATA ((ASPT(I,J),J=1,8),I=331,340) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','803','QUAR    ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus arizonica Sarg.
     & '   ','   ','QUAR2   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus arkansana Sarg.
     & 'SW ','804','QUBI    ','SW ','SW ','SW ','SW ','WO ',    !Quercus bicolor Willd.
     & '   ','   ','QUBO2   ','PO ','BR ','PO ','PO ','PO ',    !Quercus boyntonii Beadle
     & 'SO ','806','QUCO2   ','SO ','BO ','SO ','SO ','SO ',    !Quercus coccinea Münchh.
     & 'NP ','809','QUEL    ','PN ','NP ','PN ','NP ','SK ',    !Quercus ellipsoidalis E.J. Hill
     & '   ','810','QUEM    ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus emoryi Torr.
     & 'SK ','812','QUFA    ','SK ','BO ','SK ','SK ','SK ',    !Quercus falcata Michx.
     & '   ','814','QUGA    ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus gambelii Nutt.
     & '   ','   ','QUGR3   ','BJ ','NC ','OK ','BJ ','BJ '/    !Quercus grisea Liebm.
C
      DATA ((ASPT(I,J),J=1,8),I=341,350) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','QUHY    ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus hypoleucoides A. Camus
     & '   ','816','QUIL    ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus ilicifolia Wangenh.
     & 'QI ','817','QUIM    ','QI ','BO ','QI ','QI ','WK ',    !Quercus imbricaria Michx.
     & 'QN ','842','QUIN    ','BJ ','NC ','OK ','QN ','TO ',    !Quercus incana Bartram
     & 'OK ','800','QUERC   ','WO ','WO ','OK ','WO ','WO ',    !Quercus L.
     & 'TO ','819','QULA2   ','BJ ','NC ','OK ','TO ','TO ',    !Quercus laevis Walter
     & 'LK ','820','QULA3   ','WK ','BO ','WK ','LK ','LK ',    !Quercus laurifolia Michx.
     & 'OV ','822','QULY    ','OV ','BR ','OK ','OV ','OV ',    !Quercus lyrata Walter
     & 'BR ','823','QUMA2   ','BR ','BR ','BR ','BR ','OV ',    !Quercus macrocarpa Michx.
     & 'DP ','840','QUMA6   ','PO ','BR ','PO ','DP ','TO '/    !Quercus margarettae (Ashe) Small
C
      DATA ((ASPT(I,J),J=1,8),I=351,360) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'BJ ','824','QUMA3   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus marilandica Münchh.
     & 'SN ','825','QUMI    ','SN ','WO ','SN ','SN ','SN ',    !Quercus michauxii Nutt.
     & '   ','841','QUMI2   ','BJ ','NC ','OK ','DP ','TO ',    !Quercus minima (Sarg.) Small
     & 'CK ','826','QUMU    ','CK ','CK ','CK ','CK ','CK ',    !Quercus muehlenbergii Engelm.
     & 'WK ','827','QUNI    ','WK ','BO ','WK ','WK ','WK ',    !Quercus nigra L.
     & 'NK ','828','QUNU    ','NK ','NP ','CB ','NK ','CB ',    !Quercus nuttallii
     & '   ','829','QUOB    ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus oblongifolia Torr.
     & '   ','844','QUOG    ','BJ ','NC ','OK ','LK ','LK ',    !Quercus oglethorpensis Duncan
     & 'CB ','813','QUPA5   ','CB ','BO ','CB ','CB ','CB ',    !Quercus pagoda Raf.
     & 'PN ','830','QUPA2   ','PN ','NP ','PN ','PN ','SK '/    !Quercus palustris Münchh.
C
      DATA ((ASPT(I,J),J=1,8),I=361,370) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'WL ','831','QUPH    ','WL ','BO ','WL ','WL ','WK ',    !Quercus phellos L.
     & '   ','845','QUPR    ','BJ ','NC ','OK ','DP ','TO ',    !Quercus prinoides Willd.
     & 'CO ','832','QUPR2   ','CO ','WO ','CO ','CO ','CO ',    !Quercus prinus L.
     & 'RO ','833','QURU    ','RO ','RO ','RO ','RO ','RO ',    !Quercus rubra L.
     & '   ','   ','QURU4   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus rugosa Née
     & 'QS ','834','QUSH    ','QS ','BO ','SK ','QS ','QS ',    !Quercus shumardii Buckley
     & 'DO ','836','QUSI2   ','DO ','BR ','PO ','DO ','PO ',    !Quercus similis Ashe
     & '   ','808','QUSIS   ','PO ','BR ','PO ','PO ','PO ',    !Quercus sinuata Walter var. sinuata
     & 'PO ','835','QUST    ','PO ','BR ','PO ','PO ','PO ',    !Quercus stellata Wangenh.
     & '   ','   ','QUTE    ','SK ','BO ','OK ','SK ','SK '/    !Quercus texana Buckley
C
      DATA ((ASPT(I,J),J=1,8),I=371,380) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','QUTO2   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus toumeyi Sarg.
     & '   ','   ','QUTU2   ','BJ ','NC ','OK ','BJ ','BJ ',    !Quercus turbinella Greene
     & 'BO ','837','QUVE    ','BO ','BO ','BO ','BO ','BO ',    !Quercus velutina Lam.
     & 'LO ','838','QUVI    ','BJ ','NC ','OK ','LO ','LO ',    !Quercus virginiana Mill.
     & '   ','   ','RAUR    ','NC ','NC ','NC ','NC ','OH ',    !Ravenia urbanii Engl. ex Urb.
     & '   ','   ','RHCA3   ','NC ','NC ','NC ','NC ','OT ',    !Rhamnus cathartica L.
     & '   ','989','RHMA2   ','NC ','NC ','NC ','NC ','OH ',    !Rhizophora mangle L.
     & '   ','   ','RHMA4   ','NC ','NC ','NC ','NC ','OH ',    !Rhododendron maximum L.
     & '   ','   ','RHGL    ','NC ','NC ','NC ','NC ','OH ',    !Rhus glabra L.
     & '   ','   ','RHHI2   ','NC ','NC ','NC ','NC ','OH '/    !Rhus hirta
C
      DATA ((ASPT(I,J),J=1,8),I=381,390) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','ROHIF8  ','BK ','BK ','BK ','BK ','BK ',    !Robinia hispida L. var. fertilis (Ashe) R.T. Clausen
     & '   ','   ','RONE    ','BK ','BK ','BK ','BK ','BK ',    !Robinia neomexicana A. Gray
     & 'BK ','901','ROPS    ','BK ','BK ','BK ','BK ','BK ',    !Robinia pseudoacacia L.
     & '   ','   ','ROVIH2  ','BK ','BK ','BK ','BK ','BK ',    !Robinia viscosa Vent. var. hartwegii (Koehne) Ashe
     & '   ','909','ROYST   ','NC ','NC ','NC ','NC ','OT ',    !Roystonea O.F. Cook
     & '   ','912','SAPA    ','NC ','NC ','NC ','NC ','OT ',    !Sabal palmetto (Walter) Lodd. ex Schult. & Schult. f.
     & '   ','929','SASE10  ','WI ','WI ','BL ','WI ','WI ',    !Salix ×sepulcralis Simonkai [alba × ?pendulina]
     & '   ','927','SAAL2   ','WI ','WI ','BL ','WI ','WI ',    !Salix alba L.
     & '   ','921','SAAM2   ','WI ','WI ','BL ','WI ','WI ',    !Salix amygdaloides Andersson
     & '   ','925','SACA5   ','WI ','WI ','BL ','WI ','WI '/    !Salix caroliniana Michx.
C
      DATA ((ASPT(I,J),J=1,8),I=391,400) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & 'DM ','923','SAERF   ','WI ','DM ','BL ','WI ','WI ',    !Salix eriocephala
     & '   ','   ','SAEX    ','WI ','WI ','BL ','WI ','WI ',    !Salix exigua Nutt.
     & '   ','   ','SAFL    ','WI ','WI ','BL ','WI ','WI ',    !Salix floridana Chapm.
     & '   ','   ','SAGO    ','WI ','WI ','BL ','WI ','WI ',    !Salix gooddingii C.R. Ball
     & 'WI ','920','SALIX   ','WI ','WI ','BL ','WI ','WI ',    !Salix L.
     & '   ','   ','SALA6   ','WI ','WI ','BL ','WI ','WI ',    !Salix lasiolepis Benth.
     & '   ','   ','SALUC   ','WI ','WI ','BL ','WI ','WI ',    !Salix lucida Muhl. ssp. caudata (Nutt.) E. Murray
     & 'BL ','922','SANI    ','BL ','BL ','BL ','BL ','WI ',    !Salix nigra Marsh.
     & '   ','   ','SAPL2   ','WI ','WI ','BL ','WI ','WI ',    !Salix planifolia Pursh
     & '   ','926','SAPY    ','WI ','WI ','BL ','WI ','WI '/    !Salix pyrifolia Andersson
C
      DATA ((ASPT(I,J),J=1,8),I=401,410) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','SASE2   ','WI ','WI ','BL ','WI ','WI ',    !Salix serissima (L.H. Bailey) Fernald
     & '   ','   ','SATA    ','WI ','WI ','BL ','WI ','WI ',    !Salix taxifolia Kunth
     & '   ','   ','SANIC4  ','NC ','NC ','NC ','NC ','OT ',    !Sambucus nigra L. ssp. canadensis (L.) R. Bolli
     & '   ','   ','SARA2   ','NC ','NC ','NC ','NC ','OT ',    !Sambucus racemosa L.
     & '   ','   ','SARAR3  ','NC ','NC ','NC ','NC ','OT ',    !Sambucus racemosa L. var. racemosa
     & '   ','919','SASAD   ','NC ','NC ','NC ','NC ','OT ',    !Sapindus saponaria L. var. drummondii (Hook. & Arn.) L.D. Benson
     & 'SS ','931','SAAL5   ','SS ','SS ','SS ','SS ','SS ',    !Sassafras albidum (Nutt.) Nees
     & '   ','888','SCAC2   ','NC ','NC ','NC ','NC ','OT ',    !Schefflera actinophylla (Endl.) Harms
     & '   ','890','SIFO    ','NC ','NC ','NC ','NC ','OT ',    !Sideroxylon foetidissimum Jacq.
     & '   ','   ','SIDER2  ','NC ','NC ','NC ','NC ','OH '/    !Sideroxylon L.
C
      DATA ((ASPT(I,J),J=1,8),I=411,420) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','381','SILAL3  ','NC ','NC ','NC ','NC ','OH ',    !Sideroxylon lanuginosum Michx. ssp. lanuginosum
     & '   ','891','SISA6   ','NC ','NC ','NC ','NC ','OT ',    !Sideroxylon salicifolium (L.) Lam.
     & '   ','   ','SITE2   ','NC ','NC ','NC ','NC ','OT ',    !Sideroxylon tenax L.
     & '   ','895','SIGL3   ','NC ','NC ','NC ','NC ','OT ',    !Simarouba glauca DC.
     & 'MA ','935','SOAM3   ','UH ','MA ','OH ','NC ','OH ',    !Sorbus americana Marsh.
     & '   ','936','SOAU    ','UH ','MA ','OH ','NC ','OH ',    !Sorbus aucuparia L.
     & '   ','937','SODE3   ','UH ','MA ','OH ','NC ','OH ',    !Sorbus decora (Sarg.) C.K. Schneid.
     & '   ','934','SORBU   ','UH ','MA ','OH ','NC ','OH ',    !Sorbus L.
     & '   ','   ','STTR    ','NC ','NC ','NC ','NC ','OH ',    !Staphylea trifolia L.
     & '   ','940','SWMA2   ','UH ','CH ','OH ','NC ','OH '/    !Swietenia mahagoni (L.) Jacq.
C
      DATA ((ASPT(I,J),J=1,8),I=421,430) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','896','SYCU    ','NC ','NC ','NC ','NC ','OT ',    !Syzygium cumini (L.) Skeels
     & '   ','897','TAIN2   ','NC ','NC ','NC ','NC ','OT ',    !Tamarindus indica L.
     & '   ','991','TAMAR2  ','NC ','NC ','NC ','NC ','OH ',    !Tamarix L.
     & '   ','   ','TARA    ','NC ','NC ','NC ','NC ','OH ',    !Tamarix ramosissima Ledeb.
     & 'PC ','222','TAAS    ','BY ','OS ','OC ','PC ','PC ',    !Taxodium ascendens Brongn.
     & 'BY ','221','TADI2   ','BY ','OS ','OC ','BY ','BY ',    !Taxodium distichum (L.) Rich.
     & '   ','050','CUPRE   ','JU ','RC ','OC ','JU ','JU ',    !Taxodium Rich.
     & '   ','232','TAFL    ','OS ','OS ','OS ','JU ','OS ',    !Taxus floridana Nutt. ex Chapm.
     & '   ','230','TAXUS   ','OS ','OS ','OS ','JU ','OS ',    !Taxus L.
     & '   ','   ','TEHE3   ','NC ','NC ','NC ','NC ','OH '/    !Ternstroemia heptasepala Krug & Urb.
C
      DATA ((ASPT(I,J),J=1,8),I=431,440) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','913','THMO4   ','NC ','NC ','NC ','NC ','OT ',    !Thrinax morrisii H. Wendl.
     & '   ','914','THRA2   ','NC ','NC ','NC ','NC ','OT ',    !Thrinax radiata Lodd. ex Schult. & Schult. f.
     & 'OC ','240','THUJA   ','OS ','WC ','WC ','JU ','JU ',    !Thuja L.
     & 'WC ','241','THOC2   ','OS ','WC ','WC ','JU ','JU ',    !Thuja occidentalis L.
     & 'BW ','951','TIAM    ','BW ','BW ','BW ','BW ','BW ',    !Tilia americana L.
     & '   ','953','TIAMC   ','BW ','BW ','BW ','BW ','BW ',    !Tilia americana L. var. caroliniana (Mill.) Castigl.
     & 'WB ','952','TIAMH   ','BW ','BW ','WB ','BW ','BW ',    !Tilia americana L. var. heterophylla (Vent.) Louden
     & '   ','950','TILIA   ','BW ','BW ','BW ','BW ','BW ',    !Tilia L.
     & '   ','250','TORRE   ','OS ','OS ','OS ','JU ','OS ',    !Torreya Arn.
     & '   ','252','TOTA    ','OS ','OS ','OS ','JU ','OS '/    !Torreya taxifolia Arn.
C
      DATA ((ASPT(I,J),J=1,8),I=441,450) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','994','TRSE6   ','NC ','NC ','NC ','NC ','OH ',    !Triadica sebifera (L.) Small
     & 'EH ','261','TSCA    ','OS ','EH ','EH ','EH ','HM ',    !Tsuga canadensis (L.) Carrière
     & '   ','262','TSCA2   ','OS ','EH ','HM ','EH ','HM ',    !Tsuga caroliniana Engelm.
     & 'HM ','260','TSUGA   ','OS ','EH ','HM ','EH ','HM ',    !Tsuga Carrière
     & 'WE ','971','ULAL    ','WE ','AE ','EL ','WE ','WE ',    !Ulmus alata Michx.
     & 'AE ','972','ULAM    ','AE ','AE ','AE ','AE ','AE ',    !Ulmus americana L.
     & '   ','973','ULCR    ','EL ','RL ','EL ','EL ','EL ',    !Ulmus crassifolia Nutt.
     & 'EL ','970','ULMUS   ','EL ','AE ','EL ','EL ','EL ',    !Ulmus L.
     & 'SI ','974','ULPU    ','SI ','AE ','EL ','SI ','EL ',    !Ulmus pumila L.
     & 'RL ','975','ULRU    ','RL ','RL ','RL ','RL ','RL '/    !Ulmus rubra Muhl.
C
      DATA ((ASPT(I,J),J=1,8),I=451,460) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','976','ULSE    ','EL ','RL ','EL ','EL ','EL ',    !Ulmus serotina Sarg.
     & 'RE ','977','ULTH    ','RE ','RE ','EL ','RE ','EL ',    !Ulmus thomasii Sarg.
     & '   ','   ','URCH2   ','NC ','NC ','NC ','NC ','OH ',    !Urera chlorocarpa Urb.
     & '   ','   ','VAAR    ','NC ','NC ','NC ','NC ','OT ',    !Vaccinium arboreum Marsh.
     & '   ','995','VEFO    ','NC ','NC ','NC ','NC ','OH ',    !Vernicia fordii (Hemsl.) Airy-Shaw
     & '   ','   ','VIDEL   ','NC ','NC ','NC ','NC ','OH ',    !Viburnum dentatum L.
     & '   ','   ','VIBUR   ','NC ','NC ','NC ','NC ','OH ',    !Viburnum L.
     & '   ','   ','VILE    ','NC ','NC ','NC ','NC ','OH ',    !Viburnum lentago L.
     & '   ','   ','VINUC   ','NC ','NC ','NC ','NC ','OH ',    !Viburnum nudum L. var. cassinoides (L.) Torr. & A. Gray
     & '   ','   ','VIOPA2  ','NC ','NC ','NC ','NC ','OH '/    !Viburnum opulus L. var. americanum Aiton
C
      DATA ((ASPT(I,J),J=1,8),I=461,466) /
C      ALFA   FIA   PLNT       CS    LS    NE    SE    SN       SPECIES
     & '   ','   ','VIPR    ','NC ','NC ','NC ','NC ','OH ',    !Viburnum prunifolium L.
     & '   ','   ','VIRU    ','NC ','NC ','NC ','NC ','OT ',    !Viburnum rufidulum Raf.
     & '   ','   ','VIDE    ','NC ','NC ','NC ','NC ','OT ',    !Vicia disperma
     & '   ','   ','XYSC3   ','NC ','NC ','NC ','NC ','OH ',    !Xylosma schwaneckeana (Krug & Urb.) Urb.
     & '   ','   ','ZAAM    ','NC ','NC ','NC ','NC ','OT ',    !Zanthoxylum americanum Mill.
     & '   ','   ','ZANTH   ','NC ','NC ','NC ','NC ','OH '/    !Zanthoxylum L.
C
C----------
C  INITIALIZATIONS
C----------
      CALL VARVER(VVER)
      VAR=VVER(:2)
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
        SELECT CASE (VAR)
          CASE('CS')
            SPCOUT= ASPT(I,4)(1:3)
          CASE('LS')
            SPCOUT= ASPT(I,5)(1:3)
          CASE('NE')
            SPCOUT= ASPT(I,6)(1:3)
          CASE('SE')
            SPCOUT= ASPT(I,7)(1:3)
          CASE('SN')
            SPCOUT= ASPT(I,8)(1:3)
        END SELECT 
        GO TO 150        
      ELSEIF (SPCIN .EQ. ASPT(I,2)) THEN
        IJSPIN=2
        SELECT CASE (VAR)
          CASE('CS')
            SPCOUT= ASPT(I,4)(1:3)
          CASE('LS')
            SPCOUT= ASPT(I,5)(1:3)
          CASE('NE')
            SPCOUT= ASPT(I,6)(1:3)
          CASE('SE')
            SPCOUT= ASPT(I,7)(1:3)
          CASE('SN')
            SPCOUT= ASPT(I,8)(1:3)
        END SELECT
        GO TO 150   
      ELSEIF (SPCIN .EQ. ASPT(I,3)) THEN
        IJSPIN=3
        SELECT CASE (VAR)
          CASE('CS')
            SPCOUT= ASPT(I,4)(1:3)
          CASE('LS')
            SPCOUT= ASPT(I,5)(1:3)
          CASE('NE')
            SPCOUT= ASPT(I,6)(1:3)
          CASE('SE')
            SPCOUT= ASPT(I,7)(1:3)
          CASE('SN')
            SPCOUT= ASPT(I,8)(1:3)
        END SELECT
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
        IF(VAR.EQ.'SE') ISPC1=118
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