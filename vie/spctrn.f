      SUBROUTINE SPCTRN (SPCIN, ISPC1)
      IMPLICIT NONE
C----------
C VIE $Id$
C----------
C  CALLED FROM INTREE, WHEN THE INPUT SPECIES CODE IS NOT RECOGNIZED.
C  THIS ROUTINE ASSIGNS THE MOST SIMILAR SPECIES SEQUENCE NUMBER TO THE
C  SPECIES NOT RECOGNIZED IN THE VARIANT.  TRANSLATES FVS ALPHA CODES,
C  FIA CODES, AND USDA PLANTS SYMBOLS.  THE ASPT ARRAY CONTAINS THE 
C  SPECIES MAPPING.  ALL SPECIES MAPPING WAS APPROVED BY THE FVS 
C  REGIONAL CONTACTS IN APRIL 2007. TO PROVIDE ADEQUATE CODE 
C  DOCUMENTATION, STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
C  THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
C  END OF VALID FORTRAN STATEMENTS.
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'      
C
COMMONS
C----------
      CHARACTER*(*)SPCIN
      INTEGER MAXASPT, ISPC1, I, J, J2, IJSPIN
      PARAMETER (MAXASPT=517)
      CHARACTER*3 SPCOUT
      CHARACTER*8 ASPT(MAXASPT,21)
      CHARACTER VAR*2
C----------
C  DATA STATEMENT
C----------
C
      DATA ((ASPT(I,J),J=1,10),I=1,10) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'OF ','010','ABIES   ','SF ','GF ','WF ','AF ','AF ','GF ','AF ',    !Abies
     &'SH ','021','ABSH    ','OS ','OS ','SH ','OS ','OS ','NF ','OS ',    !Abies ×shastensis
     &'SF ','011','ABAM    ','SF ','OS ','SH ','OS ','OS ','SF ','OS ',    !Abies amabilis
     &'SL ','014','ABBR    ','OS ','OS ','OS ','OS ','OS ','WB ','OS ',    !Abies bracteata
     &'WF ','015','ABCO    ','OS ','GF ','WF ','GF ','WF ','WF ','AF ',    !Abies concolor
     &'   ','   ','ABCOC   ','OS ','GF ','WF ','GF ','WF ','WF ','AF ',    !Abies concolor var. concolor
     &'   ','   ','ABCOL   ','OS ','GF ','WF ','GF ','WF ','WF ','AF ',    !Abies concolor var. lowiana
     &'GF ','017','ABGR    ','OS ','GF ','WF ','GF ','GF ','GF ','AF ',    !Abies grandis
     &'AF ','019','ABLA    ','AF ','AF ','OS ','AF ','AF ','AF ','AF ',    !Abies lasiocarpa
     &'CB ','018','ABLAA   ','OS ','AF ','OS ','AF ','CB ','AF ','OS '/    !Abies lasiocarpa var. arizonica
C
      DATA ((ASPT(I,J),J=11,21),I=1,10) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'GF ','GF ','WF ','SF ','WF ','AF ','AF ','SF ','WF ','GF ','SF ',   !Abies
     &'OS ','OT ','RF ','RF ','SH ','OS ','WF ','RF ','RF ','SH ','RF ',   !Abies ×shastensis
     &'OS ','OT ','RF ','SF ','SF ','OS ','OS ','SF ','SF ','SH ','SF ',   !Abies amabilis
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Abies bracteata
     &'GF ','GF ','WF ','WF ','WF ','AF ','WF ','WF ','WF ','GF ','WF ',   !Abies concolor
     &'GF ','GF ','WF ','WF ','WF ','AF ','WF ','WF ','WF ','GF ','WF ',   !Abies concolor var. concolor
     &'GF ','GF ','WF ','WF ','WF ','AF ','WF ','WF ','WF ','GF ','WF ',   !Abies concolor var. lowiana
     &'GF ','GF ','WF ','GF ','GF ','AF ','OS ','GF ','WF ','GF ','GF ',   !Abies grandis
     &'AF ','AF ','OS ','AF ','AF ','AF ','AF ','AF ','OS ','OS ','AF ',   !Abies lasiocarpa
     &'OS ','AF ','OS ','AF ','OS ','AF ','AF ','AF ','OS ','OS ','AF '/   !Abies lasiocarpa var. arizonica
C
      DATA ((ASPT(I,J),J=1,10),I=11,20) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ABLAL   ','AF ','AF ','OS ','AF ','AF ','AF ','AF ',    !Abies lasiocarpa var. lasiocarpa
     &'RF ','020','ABMA    ','OS ','OS ','RF ','OS ','OS ','NF ','OS ',    !Abies magnifica
     &'NF ','022','ABPR    ','OS ','OS ','RF ','OS ','OS ','NF ','OS ',    !Abies procera
     &'   ','   ','ABMAS   ','OS ','OS ','SH ','OS ','OS ','NF ','OS ',    !Abies x shastensis
     &'AA ','300','ACACI   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia
     &'   ','   ','ACAN    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia angustissima
     &'   ','   ','ACANS   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia angustissima var. shrevei
     &'   ','   ','ACANS2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia angustissima var. suffrutescens
     &'   ','   ','ACANT4  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia angustissima var. texensis
     &'   ','   ','ACBA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Acacia baileyana
C
      DATA ((ASPT(I,J),J=11,21),I=11,20) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'AF ','AF ','OS ','AF ','AF ','AF ','AF ','AF ','OS ','OS ','AF ',   !Abies lasiocarpa var. lasiocarpa
     &'OS ','OT ','RF ','RF ','SH ','OS ','WF ','RF ','RF ','RF ','RF ',   !Abies magnifica
     &'OS ','OT ','RF ','NF ','NF ','OS ','OS ','NF ','RF ','RF ','NF ',   !Abies procera
     &'OS ','OT ','RF ','RF ','SH ','OS ','WF ','RF ','RF ','SH ','RF ',   !Abies x shastensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia angustissima
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia angustissima var. shrevei
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia angustissima var. suffrutescens
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia angustissima var. texensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Acacia baileyana
C
      DATA ((ASPT(I,J),J=1,10),I=21,30) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ACCO2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia constricta
     &'   ','   ','ACCOC   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia constricta var. constricta
     &'   ','   ','ACCOP9  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia constricta var. paucispina
     &'   ','   ','ACCY2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia cyclops
     &'   ','   ','ACDE3   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia dealbata
     &'   ','   ','ACDE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia decurrens
     &'   ','   ','ACEL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia elata
     &'   ','   ','ACFA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia farnesiana
     &'   ','   ','ACGR    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia greggii
     &'   ','   ','ACGRG3  ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Acacia greggii var. greggii
C
      DATA ((ASPT(I,J),J=11,21),I=21,30) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia constricta
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia constricta var. constricta
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia constricta var. paucispina
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia cyclops
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia dealbata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia decurrens
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia elata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia farnesiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia greggii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Acacia greggii var. greggii
C
      DATA ((ASPT(I,J),J=1,10),I=31,40) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ACLO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia longifolia
     &'   ','   ','ACME80  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia mearnsii
     &'   ','   ','ACME    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia melanoxylon
     &'   ','   ','ACMI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia millefolia
     &'   ','   ','ACNE4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia neovernicosa
     &'   ','   ','ACPA8   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia paradoxa
     &'   ','   ','ACPO2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia podalyriifolia
     &'   ','   ','ACPY3   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia pycnantha
     &'   ','   ','ACRE9   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia redolens
     &'   ','   ','ACRE2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Acacia retinodes
C
      DATA ((ASPT(I,J),J=11,21),I=31,40) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia longifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia mearnsii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia melanoxylon
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia millefolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia neovernicosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia paradoxa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia podalyriifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia pycnantha
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia redolens
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Acacia retinodes
C
      DATA ((ASPT(I,J),J=1,10),I=41,50) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ACSA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia saligna
     &'   ','   ','ACVE2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acacia verticillata
     &'OM ','310','ACER    ','OH ','OH ','BM ','OH ','OH ','BM ','OH ',    !Acer
     &'VN ','   ','ACCI    ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer circinatum
     &'MM ','321','ACGL    ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer glabrum
     &'   ','   ','ACGLD3  ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer glabrum var. diffusum
     &'   ','   ','ACGLD4  ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer glabrum var. douglasii
     &'   ','   ','ACGLG2  ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer glabrum var. glabrum
     &'   ','   ','ACGLG   ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer glabrum var. greenei
     &'   ','   ','ACGLN2  ','OH ','OH ','OH ','OH ','OH ','VN ','OH '/    !Acer glabrum var. neomexicanum
C
      DATA ((ASPT(I,J),J=11,21),I=41,50) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia saligna
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acacia verticillata
     &'MM ','OT ','MA ','BM ','BM ','BI ','BI ','BM ','BM ','BM ','BM ',   !Acer
     &'OH ','OT ','OH ','CH ','CH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Acer circinatum
     &'MM ','OT ','OH ','OT ','OH ','MM ','BI ','OT ','OH ','OH ','OT ',   !Acer glabrum
     &'MM ','OT ','OH ','OT ','OH ','MM ','BI ','OT ','OH ','OH ','OT ',   !Acer glabrum var. diffusum
     &'MM ','OT ','OH ','OT ','OH ','MM ','BI ','OT ','OH ','OH ','OT ',   !Acer glabrum var. douglasii
     &'MM ','OT ','OH ','OT ','OH ','MM ','BI ','OT ','OH ','OH ','OT ',   !Acer glabrum var. glabrum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acer glabrum var. greenei
     &'OH ','OT ','OH ','OT ','OH ','OH ','BI ','OT ','OH ','OH ','OT '/   !Acer glabrum var. neomexicanum
C
      DATA ((ASPT(I,J),J=1,10),I=51,60) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ACGLT2  ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer glabrum var. torreyi
     &'BI ','322','ACGR3   ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer grandidentatum
     &'   ','   ','ACGRG   ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer grandidentatum var. grandidentatum
     &'   ','   ','ACGRS   ','OH ','OH ','OH ','OH ','OH ','VN ','OH ',    !Acer grandidentatum var. sinuosum
     &'BM ','312','ACMA3   ','OH ','OH ','BM ','OH ','OH ','BM ','OH ',    !Acer macrophyllum
     &'   ','   ','ACNE2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer negundo
     &'   ','   ','ACNEA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer negundo var. arizonicum
     &'   ','   ','ACNEC2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer negundo var. californicum
     &'   ','   ','ACNEI2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer negundo var. interius
     &'   ','   ','ACNEN   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Acer negundo var. negundo
C
      DATA ((ASPT(I,J),J=11,21),I=51,60) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','BI ','OT ','OH ','OH ','OT ',   !Acer glabrum var. torreyi
     &'MM ','OT ','OH ','OT ','OH ','MM ','BI ','OT ','OH ','OH ','OT ',   !Acer grandidentatum
     &'MM ','OT ','OH ','OT ','OH ','MM ','BI ','OT ','OH ','OH ','OT ',   !Acer grandidentatum var. grandidentatum
     &'OH ','OT ','OH ','OT ','OH ','OH ','BI ','OT ','OH ','OH ','OT ',   !Acer grandidentatum var. sinuosum
     &'OH ','OT ','MA ','BM ','BM ','OH ','OH ','BM ','BM ','BM ','BM ',   !Acer macrophyllum
     &'OH ','OT ','OH ','OT ','OH ','BI ','BE ','OT ','OH ','OH ','OT ',   !Acer negundo
     &'OH ','OT ','OH ','OT ','OH ','BI ','BE ','OT ','OH ','OH ','OT ',   !Acer negundo var. arizonicum
     &'OH ','OT ','OH ','OT ','OH ','BI ','BE ','OT ','OH ','OH ','OT ',   !Acer negundo var. californicum
     &'OH ','OT ','OH ','OT ','OH ','BI ','BE ','OT ','OH ','OH ','OT ',   !Acer negundo var. interius
     &'OH ','OT ','OH ','OT ','OH ','BI ','BE ','OT ','OH ','OH ','OT '/   !Acer negundo var. negundo
C
      DATA ((ASPT(I,J),J=1,10),I=61,70) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ACNET   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer negundo var. texanum
     &'   ','   ','ACPL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer platanoides
     &'   ','   ','ACSA2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Acer saccharinum
     &'BH ','330','AESCU   ','OH ','OH ','BU ','OH ','OH ','OH ','OH ',    !Aesculus
     &'BU ','333','AECA    ','OH ','OH ','BU ','OH ','OH ','OH ','OH ',    !Aesculus californica
     &'OI ','331','AEGL    ','OH ','OH ','BU ','OH ','OH ','OH ','OH ',    !Aesculus glabra
     &'   ','   ','AEHI    ','OH ','OH ','BU ','OH ','OH ','OH ','OH ',    !Aesculus hippocastanum
     &'TH ','341','AIAL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ailanthus altissima
     &'   ','   ','ALJU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Albizia julibrissin
     &'   ','   ','ALLE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Albizia lebbeck
C
      DATA ((ASPT(I,J),J=11,21),I=61,70) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','BI ','BE ','OT ','OH ','OH ','OT ',   !Acer negundo var. texanum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acer platanoides
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Acer saccharinum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','BU ','OT ',   !Aesculus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','BU ','OT ',   !Aesculus californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','BU ','OT ',   !Aesculus glabra
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','BU ','OT ',   !Aesculus hippocastanum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ailanthus altissima
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Albizia julibrissin
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Albizia lebbeck
C
      DATA ((ASPT(I,J),J=1,10),I=71,80) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'AD ','350','ALNUS   ','AD ','OH ','RA ','OH ','OH ','RA ','OH ',    !Alnus
     &'   ','   ','ALCO13  ','OH ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus cordata
     &'EA ','355','ALGL2   ','OH ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus glutinosa
     &'   ','   ','ALIN2   ','AD ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus incana
     &'   ','   ','ALINT   ','AD ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus incana ssp. tenuifolia
     &'   ','   ','ALOB2   ','OH ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus oblongifolia
     &'WA ','352','ALRH2   ','OH ','OH ','RA ','OH ','OH ','RA ','OH ',    !Alnus rhombifolia
     &'RA ','351','ALRU2   ','RA ','OH ','RA ','OH ','OH ','RA ','OH ',    !Alnus rubra
     &'   ','   ','ALVI5   ','AD ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus viridis
     &'   ','   ','ALVIC   ','OH ','OH ','OH ','OH ','OH ','RA ','OH '/    !Alnus viridis ssp. crispa
C
      DATA ((ASPT(I,J),J=11,21),I=71,80) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','RA ','RA ','OH ','OH ','RA ','OH ','RA ','RA ',   !Alnus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus cordata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus glutinosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus incana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus incana ssp. tenuifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus oblongifolia
     &'OH ','OT ','OH ','WA ','WA ','OH ','OH ','WA ','OH ','RA ','WA ',   !Alnus rhombifolia
     &'OH ','OT ','OH ','RA ','RA ','OH ','OH ','RA ','OH ','RA ','RA ',   !Alnus rubra
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus viridis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Alnus viridis ssp. crispa
C
      DATA ((ASPT(I,J),J=1,10),I=81,90) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ALVIF   ','AD ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus viridis ssp. fruticosa
     &'   ','   ','ALVIS   ','AD ','OH ','OH ','OH ','OH ','RA ','OH ',    !Alnus viridis ssp. sinuata
     &'   ','   ','ARALI   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Aralia
     &'   ','   ','AREL8   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Aralia elata
     &'MS ','360','ARBUT   ','OH ','OH ','MA ','OH ','OH ','OH ','OH ',    !Arbutus
     &'AM ','362','ARAR2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Arbutus arizonica
     &'MA ','361','ARME    ','OH ','OH ','MA ','OH ','OH ','OH ','OH ',    !Arbutus menziesii
     &'M  ','   ','        ','OH ','OH ','MA ','OH ','OH ','OH ','OH ',    !Arbutus menziesii (Old Code)
     &'   ','   ','ARXA80  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Arbutus xalapensis
     &'OB ','370','BETUL   ','PB ','OH ','OH ','OH ','OH ','PB ','PB '/    !Betula
C
      DATA ((ASPT(I,J),J=11,21),I=81,90) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus viridis ssp. fruticosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Alnus viridis ssp. sinuata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Aralia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Aralia elata
     &'OH ','OT ','MA ','WA ','OH ','OH ','OH ','WA ','MA ','MA ','WA ',   !Arbutus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Arbutus arizonica
     &'OH ','OT ','MA ','WA ','OH ','OH ','OH ','WA ','MA ','MA ','WA ',   !Arbutus menziesii
     &'OH ','OT ','MA ','WA ','OH ','OH ','OH ','WA ','MA ','MA ','WA ',   !Arbutus menziesii (Old Code)
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Arbutus xalapensis
     &'PB ','OT ','OH ','PB ','OH ','OH ','OH ','PB ','OH ','OH ','PB '/   !Betula
C
      DATA ((ASPT(I,J),J=1,10),I=91,100) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','BEEA    ','PB ','OH ','OH ','OH ','OH ','PB ','PB ',    !Betula ×eastwoodiae
     &'NW ','378','BEUT    ','OH ','OH ','OH ','OH ','OH ','PB ','PB ',    !Betula ×utahensis
     &'   ','   ','BEGL    ','OH ','OH ','OH ','OH ','OH ','PB ','OH ',    !Betula glandulosa
     &'   ','   ','BENA    ','OH ','OH ','OH ','OH ','OH ','PB ','OH ',    !Betula nana
     &'AB ','376','BENE4   ','AB ','OH ','OH ','OH ','OH ','PB ','OH ',    !Betula neoalaskana
     &'WT ','374','BEOC2   ','PB ','OH ','OH ','OH ','OH ','PB ','PB ',    !Betula occidentalis
     &'PB ','375','BEPA    ','PB ','OH ','OH ','OH ','PB ','PB ','PB ',    !Betula papyrifera
     &'   ','   ','BEPAC   ','OH ','OH ','OH ','OH ','PB ','PB ','PB ',    !Betula papyrifera var. commutata
     &'   ','   ','BEPAK   ','AB ','OH ','OH ','OH ','OH ','OH ','OH ',    !Betula papyrifera var. kenaica
     &'   ','   ','BEPAP   ','PB ','OH ','OH ','OH ','PB ','PB ','PB '/    !Betula papyrifera var. papyrifera
C
      DATA ((ASPT(I,J),J=11,21),I=91,100) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'PB ','OT ','OH ','PB ','OH ','OH ','OH ','PB ','OH ','OH ','PB ',   !Betula ×eastwoodiae
     &'PB ','OT ','OH ','PB ','OH ','OH ','OH ','PB ','OH ','OH ','PB ',   !Betula ×utahensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula glandulosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula nana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula neoalaskana
     &'PB ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula occidentalis
     &'PB ','OT ','OH ','PB ','OH ','OH ','OH ','PB ','OH ','OH ','PB ',   !Betula papyrifera
     &'PB ','OT ','OH ','PB ','OH ','OH ','OH ','PB ','OH ','OH ','PB ',   !Betula papyrifera var. commutata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula papyrifera var. kenaica
     &'PB ','OT ','OH ','PB ','OH ','OH ','OH ','PB ','OH ','OH ','PB '/   !Betula papyrifera var. papyrifera
C
      DATA ((ASPT(I,J),J=1,10),I=101,110) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','BEPE3   ','OH ','OH ','OH ','OH ','OH ','OH ','PB ',    !Betula pendula
     &'   ','   ','BEPU4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Betula pumila
     &'   ','   ','BEPUG   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Betula pumila var. glandulifera
     &'   ','   ','BRPO6   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Brachychiton populneum
     &'YC ','042','CANO9   ','YC ','YC ','PC ','OS ','OS ','YC ','OS ',    !Callitropsis nootkatensis
     &'IC ','081','CADE27  ','OS ','OS ','IC ','OS ','OS ','OS ','OS ',    !Calocedrus decurrens
     &'   ','   ','CAGI10  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Carnegiea gigantea
     &'   ','   ','CATAL   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Catalpa
     &'   ','   ','CABI8   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Catalpa bignonioides
     &'   ','   ','CASP8   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Catalpa speciosa
C
      DATA ((ASPT(I,J),J=11,21),I=101,110) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'PB ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula pendula
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula pumila
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Betula pumila var. glandulifera
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Brachychiton populneum
     &'OS ','OT ','DF ','YC ','OS ','OS ','OS ','YC ','OS ','PC ','YC ',   !Callitropsis nootkatensis
     &'OS ','OT ','IC ','IC ','IC ','OS ','OS ','IC ','IC ','IC ','IC ',   !Calocedrus decurrens
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Carnegiea gigantea
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Catalpa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Catalpa bignonioides
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Catalpa speciosa
C
      DATA ((ASPT(I,J),J=1,10),I=111,120) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','CELTI   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis
     &'   ','   ','CEEH    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis ehrenbergiana
     &'   ','   ','CELA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis laevigata
     &'   ','   ','CELAB   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis laevigata var. brevipes
     &'   ','   ','CELAR   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis laevigata var. reticulata
     &'   ','   ','CELAT8  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis laevigata var. texana
     &'   ','   ','CEOC    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Celtis occidentalis
     &'   ','   ','CESI3   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ceratonia siliqua
     &'   ','   ','CERCO   ','OH ','OH ','OH ','MC ','OH ','OH ','OH ',    !Cercocarpus
     &'MB ','478','CEAL8   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Cercocarpus alnifolius
C
      DATA ((ASPT(I,J),J=11,21),I=111,120) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis ehrenbergiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis laevigata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis laevigata var. brevipes
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis laevigata var. reticulata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis laevigata var. texana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Celtis occidentalis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ceratonia siliqua
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT '/   !Cercocarpus alnifolius
C
      DATA ((ASPT(I,J),J=1,10),I=121,130) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','CEIN7   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cercocarpus intricatus
     &'MC ','475','CELE3   ','OH ','OH ','OH ','MC ','OH ','OH ','OH ',    !Cercocarpus ledifolius
     &'   ','   ','CELEI   ','OH ','OH ','OH ','MC ','OH ','OH ','OH ',    !Cercocarpus ledifolius var. intercedens
     &'   ','   ','CELEL   ','OH ','OH ','OH ','MC ','OH ','OH ','OH ',    !Cercocarpus ledifolius var. ledifolius
     &'   ','   ','CEMO2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cercocarpus montanus
     &'BF ','   ','CEMOG   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cercocarpus montanus var. glaber
     &'   ','   ','CEMOM4  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cercocarpus montanus var. montanus
     &'   ','   ','CEMOP   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cercocarpus montanus var. paucidentatus
     &'PC ','041','CHLA    ','OS ','OS ','PC ','OS ','OS ','OS ','OS ',    !Chamaecyparis lawsoniana
     &'   ','   ','CHNO    ','YC ','YC ','PC ','OS ','OS ','YC ','OS '/    !Chamaecyparis nootkatensis
C
      DATA ((ASPT(I,J),J=11,21),I=121,130) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus intricatus
     &'OH ','OT ','OH ','OT ','MC ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus ledifolius
     &'OH ','OT ','OH ','OT ','MC ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus ledifolius var. intercedens
     &'OH ','OT ','OH ','OT ','MC ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus ledifolius var. ledifolius
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus montanus
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus montanus var. glaber
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus montanus var. montanus
     &'OH ','OT ','OH ','OT ','MB ','MC ','MC ','OT ','MC ','OH ','OT ',   !Cercocarpus montanus var. paucidentatus
     &'OS ','OT ','DF ','DF ','OS ','OS ','OS ','DF ','OS ','PC ','DF ',   !Chamaecyparis lawsoniana
     &'OS ','OT ','DF ','YC ','OS ','OS ','OS ','YC ','OS ','PC ','YC '/   !Chamaecyparis nootkatensis
C
      DATA ((ASPT(I,J),J=1,10),I=131,140) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'CS ','040','CHAMA4  ','YC ','YC ','PC ','OS ','OS ','YC ','OS ',    !Chamaecyparis spp.
     &'   ','   ','CHRYS15 ','OH ','OH ','GC ','OH ','OH ','GC ','OH ',    !Chrysolepis
     &'   ','   ','CHCH7   ','OH ','OH ','GC ','OH ','OH ','GC ','OH ',    !Chrysolepis chrysophylla
     &'   ','   ','CACH6   ','OH ','OH ','GC ','OH ','OH ','GC ','OH ',    !Chrysolepis chrysophylla var. chrysophylla
     &'GC ','431','CHCHC4  ','OH ','OH ','GC ','OH ','OH ','GC ','OH ',    !Chrysolepis chrysophylla var. chrysophylla
     &'   ','   ','CHCHM   ','OH ','OH ','GC ','OH ','OH ','GC ','OH ',    !Chrysolepis chrysophylla var. minor
     &'   ','   ','CICA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cinnamomum camphora
     &'   ','   ','COAU12  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Cordyline australis
     &'DS ','490','CORNU   ','OH ','OH ','DG ','OH ','OH ','DG ','OH ',    !Cornus
     &'   ','   ','COGL3   ','OH ','OH ','DG ','OH ','OH ','DG ','OH '/    !Cornus glabrata
C
      DATA ((ASPT(I,J),J=11,21),I=131,140) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','DF ','YC ','OS ','OS ','OS ','YC ','OS ','PC ','YC ',   !Chamaecyparis spp.
     &'OH ','OT ','OH ','GC ','GC ','OH ','OH ','GC ','GC ','GC ','GC ',   !Chrysolepis
     &'OH ','OT ','OH ','GC ','GC ','OH ','OH ','GC ','GC ','GC ','GC ',   !Chrysolepis chrysophylla
     &'OH ','OT ','OH ','GC ','GC ','OH ','OH ','GC ','GC ','GC ','GC ',   !Chrysolepis chrysophylla var. chrysophylla
     &'OH ','OT ','OH ','GC ','GC ','OH ','OH ','GC ','GC ','GC ','GC ',   !Chrysolepis chrysophylla var. chrysophylla
     &'OH ','OT ','OH ','GC ','GC ','OH ','OH ','GC ','GC ','GC ','GC ',   !Chrysolepis chrysophylla var. minor
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Cinnamomum camphora
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Cordyline australis
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG ',   !Cornus
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG '/   !Cornus glabrata
C
      DATA ((ASPT(I,J),J=1,10),I=141,150) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'DG ','492','CONU4   ','OH ','OH ','DG ','OH ','OH ','DG ','OH ',    !Cornus nuttallii
     &'   ','   ','COSE16  ','OH ','OH ','DG ','OH ','OH ','DG ','OH ',    !Cornus sericea
     &'   ','   ','COSEO   ','OH ','OH ','DG ','OH ','OH ','DG ','OH ',    !Cornus sericea ssp. occidentalis
     &'   ','   ','COSES   ','OH ','OH ','DG ','OH ','OH ','DG ','OH ',    !Cornus sericea ssp. sericea
     &'   ','   ','COSE3   ','OH ','OH ','DG ','OH ','OH ','DG ','OH ',    !Cornus sessilis
     &'HZ ','   ','COCO6   ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Corylus cornuta
     &'   ','   ','COCI4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Corymbia citriodora
     &'HT ','500','CRATA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Crataegus
     &'   ','   ','CRDO2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Crataegus douglasii
     &'   ','   ','CRER    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Crataegus erythropoda
C
      DATA ((ASPT(I,J),J=11,21),I=141,150) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG ',   !Cornus nuttallii
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG ',   !Cornus sericea
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG ',   !Cornus sericea ssp. occidentalis
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG ',   !Cornus sericea ssp. sericea
     &'OH ','OT ','OH ','DG ','OH ','OH ','OH ','DG ','DG ','DG ','DG ',   !Cornus sessilis
     &'OH ','OT ','OH ','CH ','OH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Corylus cornuta
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Corymbia citriodora
     &'OH ','OT ','OH ','HT ','OH ','OH ','OH ','HT ','OH ','OH ','HT ',   !Crataegus
     &'OH ','OT ','OH ','HT ','OH ','OH ','OH ','HT ','OH ','OH ','HT ',   !Crataegus douglasii
     &'OH ','OT ','OH ','HT ','OH ','OH ','OH ','HT ','OH ','OH ','HT '/   !Crataegus erythropoda
C
      DATA ((ASPT(I,J),J=1,10),I=151,160) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','CRRI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Crataegus rivularis
     &'   ','   ','CRSA2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Crataegus saligna
     &'   ','   ','CRSU5   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Crataegus succulenta
     &'CU ','050','CUPRE   ','OS ','WJ ','WJ ','OS ','RM ','WJ ','OS ',    !Cupressus
     &'   ','   ','CUAB    ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus abramsiana
     &'AC ','051','CUAR    ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Cupressus arizonica
     &'   ','   ','CUARA   ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Cupressus arizonica ssp. arizonica
     &'   ','   ','CUARN2  ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Cupressus arizonica ssp. nevadensis
     &'   ','   ','CUARS2  ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Cupressus arizonica ssp. stephensonii
     &'CC ','052','CUBA    ','OS ','WJ ','WJ ','OS ','OS ','WJ ','OS '/    !Cupressus bakeri
C
      DATA ((ASPT(I,J),J=11,21),I=151,160) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','HT ','OH ','OH ','OH ','HT ','OH ','OH ','HT ',   !Crataegus rivularis
     &'OH ','OT ','OH ','HT ','OH ','OH ','OH ','HT ','OH ','OH ','HT ',   !Crataegus saligna
     &'OH ','OT ','OH ','HT ','OH ','OH ','OH ','HT ','OH ','OH ','HT ',   !Crataegus succulenta
     &'OS ','OT ','OS ','OT ','WJ ','OS ','WJ ','OT ','WJ ','WJ ','OT ',   !Cupressus
     &'OS ','OT ','OS ','OT ','OS ','OS ','WJ ','OT ','OS ','OS ','OT ',   !Cupressus abramsiana
     &'OS ','OT ','OS ','OT ','OS ','OS ','WJ ','OT ','OS ','OS ','OT ',   !Cupressus arizonica
     &'OS ','OT ','OS ','OT ','OS ','OS ','WJ ','OT ','OS ','OS ','OT ',   !Cupressus arizonica ssp. arizonica
     &'OS ','OT ','OS ','OT ','OS ','OS ','WJ ','OT ','OS ','OS ','OT ',   !Cupressus arizonica ssp. nevadensis
     &'OS ','OT ','OS ','OT ','OS ','OS ','WJ ','OT ','OS ','OS ','OT ',   !Cupressus arizonica ssp. stephensonii
     &'OS ','OT ','OS ','OT ','WJ ','OS ','OS ','OT ','WJ ','WJ ','OT '/   !Cupressus bakeri
C
      DATA ((ASPT(I,J),J=1,10),I=161,170) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'TC ','053','CUFO2   ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus forbesii
     &'   ','   ','CUGO    ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus goveniana
     &'   ','   ','CUGOG   ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus goveniana ssp. goveniana
     &'   ','   ','CUGOP2  ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus goveniana ssp. pygmaea
     &'   ','   ','CUMA    ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus macnabiana
     &'MO ','054','CUMA2   ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Cupressus macrocarpa
     &'   ','   ','CUNO    ','YC ','YC ','PC ','OS ','OS ','YC ','OS ',    !Cupressus nootkatensis
     &'SA ','055','CUSA3   ','OS ','OS ','WJ ','OS ','OS ','OS ','OS ',    !Cupressus sargentii
     &'   ','   ','DIVI5   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Diospyros virginiana
     &'   ','   ','ELAEA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Elaeagnus
C
      DATA ((ASPT(I,J),J=11,21),I=161,170) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Cupressus forbesii
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Cupressus goveniana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Cupressus goveniana ssp. goveniana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Cupressus goveniana ssp. pygmaea
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Cupressus macnabiana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Cupressus macrocarpa
     &'OS ','OT ','DF ','YC ','OS ','OS ','OS ','YC ','OS ','PC ','YC ',   !Cupressus nootkatensis
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','WJ ','WJ ','OT ',   !Cupressus sargentii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Diospyros virginiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Elaeagnus
C
      DATA ((ASPT(I,J),J=1,10),I=171,180) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ELAN    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Elaeagnus angustifolia
     &'   ','   ','ELCO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Elaeagnus commutata
     &'   ','   ','ERJA3   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eriobotrya japonica
     &'   ','   ','EUCAL   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus
     &'   ','   ','EUMO5   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus ×mortoniana
     &'   ','   ','EUCA2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus camaldulensis
     &'   ','   ','EUCL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus cladocalyx
     &'   ','   ','EUGL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus globulus
     &'   ','   ','EUGLG   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus globulus ssp. globulus
     &'   ','   ','EUPO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Eucalyptus polyanthemos
C
      DATA ((ASPT(I,J),J=11,21),I=171,180) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Elaeagnus angustifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Elaeagnus commutata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eriobotrya japonica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus ×mortoniana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus camaldulensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus cladocalyx
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus globulus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus globulus ssp. globulus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Eucalyptus polyanthemos
C
      DATA ((ASPT(I,J),J=1,10),I=181,190) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','EUPU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus pulverulenta
     &'   ','   ','EUSI2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus sideroxylon
     &'   ','   ','EUTE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus tereticornis
     &'   ','   ','EUTO11  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus torquata
     &'   ','   ','EUVI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Eucalyptus viminalis
     &'   ','   ','FAGUS   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Fagus
     &'   ','   ','FAGR    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Fagus grandifolia
     &'   ','   ','FASY    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Fagus sylvatica
     &'   ','   ','FICUS   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ficus
     &'   ','   ','FICA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Ficus carica
C
      DATA ((ASPT(I,J),J=11,21),I=181,190) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus pulverulenta
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus sideroxylon
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus tereticornis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus torquata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Eucalyptus viminalis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fagus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fagus grandifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fagus sylvatica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ficus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Ficus carica
C
      DATA ((ASPT(I,J),J=1,10),I=191,200) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','FIPA2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ficus palmata
     &'   ','   ','FIRU4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ficus rubiginosa
     &'BT ','   ','FRPU7   ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Frangula purshiana
     &'OX ','540','FRAXI   ','OH ','OH ','FL ','OH ','OH ','OH ','GA ',    !Fraxinus
     &'   ','   ','FRAM2   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus americana
     &'   ','   ','FRAN2   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus anomala
     &'   ','   ','FRANA   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus anomala var. anomala
     &'   ','   ','FRANL   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus anomala var. lowellii
     &'   ','   ','FRCU    ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus cuspidata
     &'   ','   ','FRDI2   ','OH ','OH ','OH ','OH ','OH ','OH ','GA '/    !Fraxinus dipetala
C
      DATA ((ASPT(I,J),J=11,21),I=191,200) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ficus palmata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ficus rubiginosa
     &'OH ','OT ','OH ','CH ','OH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Frangula purshiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','FL ','OT ',   !Fraxinus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus americana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus anomala
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus anomala var. anomala
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus anomala var. lowellii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus cuspidata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Fraxinus dipetala
C
      DATA ((ASPT(I,J),J=1,10),I=201,210) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','FRGO    ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus gooddingii
     &'   ','   ','FRGR2   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus greggii
     &'FL ','542','FRLA    ','OH ','OH ','FL ','OH ','OH ','OH ','GA ',    !Fraxinus latifolia
     &'   ','   ','FRPA4   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus papillosa
     &'GA ','544','FRPE    ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus pennsylvanica
     &'   ','   ','FRUH    ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus uhdei
     &'VA ','547','FRVE2   ','OH ','OH ','OH ','OH ','OH ','OH ','GA ',    !Fraxinus velutina
     &'   ','   ','FRCA6   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Fremontodendron californicum
     &'   ','   ','FRME2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Fremontodendron mexicanum
     &'   ','   ','GLTR    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Gleditsia triacanthos
C
      DATA ((ASPT(I,J),J=11,21),I=201,210) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus gooddingii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus greggii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','FL ','OT ',   !Fraxinus latifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus papillosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus pennsylvanica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus uhdei
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fraxinus velutina
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fremontodendron californicum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Fremontodendron mexicanum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Gleditsia triacanthos
C
      DATA ((ASPT(I,J),J=1,10),I=211,220) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','HOPO5   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Hoheria populnea
     &'   ','   ','ILAQ80  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ilex aquifolium
     &'WN ','600','JUGLA   ','OH ','OH ','WN ','OH ','OH ','OH ','OH ',    !Juglans
     &'SB ','604','JUCA    ','OH ','OH ','WN ','OH ','OH ','OH ','OH ',    !Juglans californica
     &'CA ','603','JUHI    ','OH ','OH ','WN ','OH ','OH ','OH ','OH ',    !Juglans hindsii
     &'   ','   ','JUMA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Juglans major
     &'TW ','605','JUMI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Juglans microcarpa
     &'   ','   ','JUNI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Juglans nigra
     &'   ','   ','JURE80  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Juglans regia
     &'JU ','057','JUNIP   ','OS ','WJ ','WJ ','WJ ','RM ','WJ ','RM '/    !Juniperus
C
      DATA ((ASPT(I,J),J=11,21),I=211,220) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Hoheria populnea
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ilex aquifolium
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','WN ','OT ',   !Juglans
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','WN ','OT ',   !Juglans californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','WN ','OT ',   !Juglans hindsii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Juglans major
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Juglans microcarpa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Juglans nigra
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Juglans regia
     &'RM ','OT ','OS ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ','WJ ','WJ '/   !Juniperus
C
      DATA ((ASPT(I,J),J=1,10),I=221,230) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'J  ','   ','        ','OS ','WJ ','WJ ','WJ ','RM ','WJ ','RM ',    !Juniperus (Old Code)
     &'CJ ','062','JUCA7   ','OS ','WJ ','WJ ','WJ ','RM ','WJ ','OS ',    !Juniperus californica
     &'RJ ','059','JUCO11  ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Juniperus coahuilensis
     &'   ','   ','JUCOA2  ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Juniperus coahuilensis var. arizonica
     &'   ','   ','JUCOC2  ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Juniperus coahuilensis var. coahuilensis
     &'   ','   ','JUCO6   ','OS ','WJ ','WJ ','OS ','OS ','WJ ','RM ',    !Juniperus communis
     &'   ','   ','JUCOD   ','OS ','WJ ','WJ ','OS ','OS ','WJ ','RM ',    !Juniperus communis var. depressa
     &'   ','   ','JUCOS2  ','OS ','WJ ','WJ ','OS ','OS ','WJ ','RM ',    !Juniperus communis var. saxatilis
     &'AJ ','063','JUDE2   ','OS ','OS ','OS ','WJ ','AJ ','OS ','OS ',    !Juniperus deppeana
     &'   ','   ','JUER    ','OS ','OS ','OS ','OS ','RM ','OS ','OS '/    !Juniperus erythrocarpa
C
      DATA ((ASPT(I,J),J=11,21),I=221,230) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'RM ','OT ','OS ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ','WJ ','WJ ',   !Juniperus (Old Code)
     &'OS ','OT ','OS ','WJ ','WJ ','OS ','WJ ','WJ ','CJ ','WJ ','WJ ',   !Juniperus californica
     &'OS ','OT ','OS ','WJ ','OS ','OS ','OS ','WJ ','OS ','OS ','WJ ',   !Juniperus coahuilensis
     &'OS ','OT ','OS ','WJ ','OS ','OS ','OS ','WJ ','OS ','OS ','WJ ',   !Juniperus coahuilensis var. arizonica
     &'OS ','OT ','OS ','WJ ','OS ','OS ','OS ','WJ ','OS ','OS ','WJ ',   !Juniperus coahuilensis var. coahuilensis
     &'RM ','OT ','OS ','WJ ','WJ ','OS ','OS ','WJ ','WJ ','WJ ','WJ ',   !Juniperus communis
     &'RM ','OT ','OS ','WJ ','WJ ','OS ','OS ','WJ ','WJ ','WJ ','WJ ',   !Juniperus communis var. depressa
     &'RM ','OT ','OS ','WJ ','WJ ','OS ','OS ','WJ ','WJ ','WJ ','WJ ',   !Juniperus communis var. saxatilis
     &'OS ','OT ','OS ','WJ ','OS ','OS ','WJ ','WJ ','OS ','OS ','WJ ',   !Juniperus deppeana
     &'OS ','OT ','OS ','WJ ','OS ','OS ','OS ','WJ ','OS ','OS ','WJ '/   !Juniperus erythrocarpa
C
      DATA ((ASPT(I,J),J=1,10),I=231,240) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'OJ ','069','JUMO    ','OS ','OS ','OS ','WJ ','OJ ','OS ','OS ',    !Juniperus monosperma
     &'WJ ','064','JUOC    ','OS ','WJ ','WJ ','WJ ','RM ','WJ ','RM ',    !Juniperus occidentalis
     &'   ','   ','JUOCA   ','OS ','WJ ','WJ ','WJ ','RM ','WJ ','RM ',    !Juniperus occidentalis var. australis
     &'   ','   ','JUOCO   ','OS ','WJ ','WJ ','WJ ','RM ','WJ ','RM ',    !Juniperus occidentalis var. occidentalis
     &'UJ ','065','JUOS    ','OS ','WJ ','OS ','WJ ','UJ ','WJ ','RM ',    !Juniperus osteosperma
     &'PJ ','058','JUPI    ','OS ','OS ','OS ','OS ','RM ','OS ','OS ',    !Juniperus pinchotii
     &'RM ','066','JUSC2   ','OS ','WJ ','OS ','WJ ','RM ','WJ ','RM ',    !Juniperus scopulorum
     &'ER ','068','JUVI    ','OS ','OS ','OS ','OS ','ER ','WJ ','OS ',    !Juniperus virginiana
     &'   ','   ','JUVIV   ','OS ','OS ','OS ','OS ','ER ','WJ ','OS ',    !Juniperus virginiana var. virginiana
     &'   ','   ','KOPA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Koelreuteria paniculata
C
      DATA ((ASPT(I,J),J=11,21),I=231,240) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','WJ ','OS ','OS ','WJ ','WJ ','OS ','OS ','WJ ',   !Juniperus monosperma
     &'RM ','OT ','OS ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ','WJ ','WJ ',   !Juniperus occidentalis
     &'RM ','OT ','OS ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ','WJ ','WJ ',   !Juniperus occidentalis var. australis
     &'RM ','OT ','OS ','WJ ','WJ ','RM ','WJ ','WJ ','WJ ','WJ ','WJ ',   !Juniperus occidentalis var. occidentalis
     &'RM ','OT ','OS ','WJ ','WJ ','UJ ','UJ ','WJ ','UJ ','OS ','WJ ',   !Juniperus osteosperma
     &'OS ','OT ','OS ','WJ ','OS ','OS ','OS ','WJ ','OS ','OS ','WJ ',   !Juniperus pinchotii
     &'RM ','OT ','OS ','WJ ','WJ ','RM ','RM ','WJ ','OS ','OS ','WJ ',   !Juniperus scopulorum
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Juniperus virginiana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Juniperus virginiana var. virginiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Koelreuteria paniculata
C
      DATA ((ASPT(I,J),J=1,10),I=241,250) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','LAAN2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Laburnum anagyroides
     &'OL ','070','LARIX   ','TA ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix
     &'TA ','071','LALA    ','TA ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix laricina
     &'LL ','072','LALY    ','OS ','AF ','OS ','WL ','WL ','LL ','LL ',    !Larix lyallii
     &'WL ','073','LAOC    ','OS ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix occidentalis
     &'L  ','   ','        ','OS ','WL ','OS ','WL ','WL ','WL ','WL ',    !Larix occidentalis (Old Code)
     &'   ','   ','LANO80  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Laurus nobilis
     &'   ','   ','LEPTO   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Leptochloa
     &'   ','   ','LEUCA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Leucaena
     &'   ','   ','LEES2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Leucaena esculenta
C
      DATA ((ASPT(I,J),J=11,21),I=241,250) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Laburnum anagyroides
     &'WL ','WL ','OS ','YC ','WL ','OS ','OS ','YC ','OS ','OS ','YC ',   !Larix
     &'WL ','WL ','OS ','YC ','WL ','OS ','OS ','YC ','OS ','OS ','YC ',   !Larix laricina
     &'LL ','WL ','OS ','LL ','OS ','OS ','OS ','LL ','OS ','OS ','LL ',   !Larix lyallii
     &'WL ','WL ','OS ','YC ','WL ','OS ','OS ','YC ','OS ','OS ','YC ',   !Larix occidentalis
     &'WL ','WL ','OS ','YC ','WL ','OS ','OS ','YC ','OS ','OS ','YC ',   !Larix occidentalis (Old Code)
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Laurus nobilis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Leptochloa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Leucaena
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Leucaena esculenta
C
      DATA ((ASPT(I,J),J=1,10),I=251,260) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','LELE10  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Leucaena leucocephala
     &'   ','   ','LIDE    ','OS ','OS ','IC ','OS ','OS ','OS ','OS ',    !Libocedrus decurrens
     &'   ','   ','LIST2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Liquidambar styraciflua
     &'   ','   ','LITHO   ','OH ','OH ','TO ','OH ','OH ','OH ','OH ',    !Lithocarpus
     &'TO ','631','LIDE3   ','OH ','OH ','TO ','OH ','OH ','OH ','OH ',    !Lithocarpus densiflorus
     &'   ','   ','LIDED2  ','OH ','OH ','TO ','OH ','OH ','OH ','OH ',    !Lithocarpus densiflorus var. densiflorus
     &'   ','   ','LIDEE   ','OH ','OH ','TO ','OH ','OH ','OH ','OH ',    !Lithocarpus densiflorus var. echinoides
     &'   ','   ','LIMO4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Lithrea molleoides
     &'   ','   ','LYONO   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Lyonothamnus
     &'   ','   ','LYFL2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Lyonothamnus floribundus
C
      DATA ((ASPT(I,J),J=11,21),I=251,260) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Leucaena leucocephala
     &'OS ','OT ','IC ','IC ','IC ','OS ','OS ','IC ','IC ','IC ','IC ',   !Libocedrus decurrens
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Liquidambar styraciflua
     &'OH ','OT ','TO ','GC ','OH ','OH ','OH ','GC ','TO ','TO ','GC ',   !Lithocarpus
     &'OH ','OT ','TO ','GC ','OH ','OH ','OH ','GC ','TO ','TO ','GC ',   !Lithocarpus densiflorus
     &'OH ','OT ','TO ','GC ','OH ','OH ','OH ','GC ','TO ','TO ','GC ',   !Lithocarpus densiflorus var. densiflorus
     &'OH ','OT ','TO ','GC ','OH ','OH ','OH ','GC ','TO ','TO ','GC ',   !Lithocarpus densiflorus var. echinoides
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Lithrea molleoides
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Lyonothamnus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Lyonothamnus floribundus
C
      DATA ((ASPT(I,J),J=1,10),I=261,270) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','LYFLA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Lyonothamnus floribundus ssp. aspleniifolius
     &'   ','   ','LYFLF   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Lyonothamnus floribundus ssp. floribundus
     &'AL ','660','MALUS   ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Malus
     &'   ','   ','MACO5   ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Malus coronaria
     &'OR ','661','MAFU    ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Malus fusca
     &'   ','   ','MAPU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Malus pumila
     &'MU ','992','MEQU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Melaleuca quinquenervia
     &'   ','   ','MEAZ    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Melia azedarach
     &'WM ','   ','MOCA6   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Morella californica
     &'   ','   ','MORUS   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Morus
C
      DATA ((ASPT(I,J),J=11,21),I=261,270) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Lyonothamnus floribundus ssp. aspleniifolius
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Lyonothamnus floribundus ssp. floribundus
     &'OH ','OT ','OH ','CH ','OH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Malus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Malus coronaria
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','CH ','OH ','OH ','OT ',   !Malus fusca
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Malus pumila
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Melaleuca quinquenervia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Melia azedarach
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Morella californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Morus
C
      DATA ((ASPT(I,J),J=1,10),I=271,280) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','MOAL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Morus alba
     &'   ','   ','MOMI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Morus microphylla
     &'DI ','990','OLTE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Olneya tesota
     &'   ','   ','OSTRY   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ostrya
     &'   ','   ','OSKN    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ostrya knowltonii
     &'   ','   ','OSVI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ostrya virginiana
     &'   ','   ','OSVIV   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ostrya virginiana  var. virginiana
     &'OT ','999','2TREE   ','OH ','OS ','OH ','OS ','OS ','OS ','OS ',    !Other
     &'OC ','   ','        ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Other Conifer
     &'OH ','998','2TD     ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Other Hardwood
C
      DATA ((ASPT(I,J),J=11,21),I=271,280) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Morus alba
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Morus microphylla
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Olneya tesota
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ostrya
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ostrya knowltonii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ostrya virginiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ostrya virginiana  var. virginiana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OH ','OT ',   !Other
     &'OS ','OT ','OS ','DF ','OS ','OS ','OS ','DF ','OS ','OS ','DF ',   !Other Conifer
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Other Hardwood
C
      DATA ((ASPT(I,J),J=1,10),I=281,290) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','290','        ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Other Softwood
     &'OS ','298','2TE     ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Other Softwood
     &'   ','   ','PHOEN2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Phoenix
     &'   ','   ','PHCA13  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Phoenix canariensis
     &'   ','   ','PHDA4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Phoenix dactylifera
     &'SR ','090','PICEA   ','SS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea
     &'BR ','092','PIBR    ','OS ','OS ','BR ','OS ','OS ','ES ','OS ',    !Picea breweriana
     &'ES ','093','PIEN    ','OS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea engelmannii
     &'S  ','   ','        ','OS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea engelmannii (Old Code)
     &'   ','   ','PIENE   ','OS ','ES ','BR ','ES ','ES ','ES ','ES '/    !Picea engelmannii var. engelmannii
C
      DATA ((ASPT(I,J),J=11,21),I=281,290) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Other Softwood
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Other Softwood
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Phoenix
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Phoenix canariensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Phoenix dactylifera
     &'ES ','ES ','OS ','SS ','ES ','ES ','ES ','ES ','OS ','BR ','SS ',   !Picea
     &'OS ','OT ','OS ','OT ','ES ','OS ','OS ','OT ','OS ','BR ','OT ',   !Picea breweriana
     &'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','OS ','BR ','ES ',   !Picea engelmannii
     &'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','OS ','BR ','ES ',   !Picea engelmannii (Old Code)
     &'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','OS ','BR ','ES '/   !Picea engelmannii var. engelmannii
C
      DATA ((ASPT(I,J),J=1,10),I=291,300) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','PIENM2  ','OS ','ES ','BR ','ES ','ES ','ES ','ES ',    !Picea engelmannii var. mexicana
     &'WS ','094','PIGL    ','WS ','OS ','OS ','OS ','WS ','ES ','ES ',    !Picea glauca
     &'LS ','   ','PILU    ','LS ','OS ','OS ','OS ','OS ','ES ','OS ',    !Picea lutzii
     &'BE ','095','PIMA    ','BE ','OS ','OS ','OS ','OS ','ES ','OS ',    !Picea mariana
     &'BS ','096','PIPU    ','OS ','OS ','OS ','OS ','BS ','ES ','ES ',    !Picea pungens
     &'SS ','098','PISI    ','SS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Picea sitchensis
     &'OP ','100','PINUS   ','LP ','PP ','SP ','PP ','PP ','LP ','LP ',    !Pinus
     &'   ','   ','PIAT2   ','OS ','OS ','MP ','OS ','OS ','OS ','OS ',    !Pinus ×attenuradiata
     &'WB ','101','PIAL    ','OS ','WB ','WB ','WB ','WB ','WB ','WB ',    !Pinus albicaulis
     &'BC ','102','PIAR    ','OS ','OS ','OS ','OS ','BC ','WB ','OS '/    !Pinus aristata
C
      DATA ((ASPT(I,J),J=11,21),I=291,300) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'ES ','ES ','OS ','ES ','ES ','ES ','ES ','ES ','OS ','BR ','ES ',   !Picea engelmannii var. mexicana
     &'ES ','ES ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Picea glauca
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Picea lutzii
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Picea mariana
     &'ES ','ES ','OS ','OT ','OS ','BS ','BS ','OT ','OS ','OS ','OT ',   !Picea pungens
     &'OS ','OT ','OS ','SS ','OS ','OS ','OS ','ES ','OS ','OS ','SS ',   !Picea sitchensis
     &'LP ','LP ','SP ','LP ','PP ','LP ','LP ','LP ','PP ','SP ','LP ',   !Pinus
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','MP ','MP ','OT ',   !Pinus ×attenuradiata
     &'WB ','LP ','OS ','WB ','WB ','WB ','WB ','WB ','WB ','WB ','WB ',   !Pinus albicaulis
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT '/   !Pinus aristata
C
      DATA ((ASPT(I,J),J=1,10),I=301,310) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'AR ','135','PIAR5   ','OS ','OS ','OS ','OS ','PP ','OS ','OS ',    !Pinus arizonica
     &'   ','   ','PIARA   ','OS ','OS ','OS ','OS ','PP ','OS ','OS ',    !Pinus arizonica var. arizonica
     &'   ','   ','PIARS2  ','OS ','OS ','OS ','OS ','PP ','OS ','OS ',    !Pinus arizonica var. stormiae
     &'KP ','103','PIAT    ','OS ','LP ','KP ','OS ','OS ','LP ','OS ',    !Pinus attenuata
     &'FP ','104','PIBA    ','OS ','WB ','WB ','OS ','OS ','WB ','OS ',    !Pinus balfouriana
     &'   ','   ','PIBAA   ','OS ','WB ','WB ','OS ','OS ','WB ','OS ',    !Pinus balfouriana ssp. austrina
     &'   ','   ','PIBAB   ','OS ','WB ','WB ','OS ','OS ','WB ','OS ',    !Pinus balfouriana ssp. balfouriana
     &'ME ','140','PICE    ','OS ','OS ','OS ','OS ','PI ','OS ','OS ',    !Pinus cembroides
     &'LP ','108','PICO    ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',    !Pinus contorta
     &'   ','   ','PICOB   ','OS ','OS ','OS ','OS ','OS ','OS ','OS '/    !Pinus contorta var. bolanderi
C
      DATA ((ASPT(I,J),J=11,21),I=301,310) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus arizonica
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus arizonica var. arizonica
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus arizonica var. stormiae
     &'OS ','OT ','OS ','KP ','LP ','OS ','OS ','KP ','GB ','KP ','KP ',   !Pinus attenuata
     &'OS ','OT ','OS ','WB ','WB ','OS ','OS ','WB ','FP ','WB ','WB ',   !Pinus balfouriana
     &'OS ','OT ','OS ','WB ','WB ','OS ','OS ','WB ','FP ','WB ','WB ',   !Pinus balfouriana ssp. austrina
     &'OS ','OT ','OS ','WB ','WB ','OS ','OS ','WB ','FP ','WB ','WB ',   !Pinus balfouriana ssp. balfouriana
     &'OS ','OT ','OS ','OT ','OS ','OS ','PI ','OT ','OS ','OS ','OT ',   !Pinus cembroides
     &'LP ','LP ','PP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',   !Pinus contorta
     &'OS ','OT ','OS ','LP ','OS ','OS ','OS ','OT ','OS ','OS ','LP '/   !Pinus contorta var. bolanderi
C
      DATA ((ASPT(I,J),J=1,10),I=311,320) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','PICOC   ','LP ','OS ','OS ','OS ','OS ','OS ','OS ',    !Pinus contorta var. contorta
     &'   ','   ','PICOL   ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',    !Pinus contorta var. latifolia
     &'   ','   ','PICOM   ','OS ','LP ','LP ','LP ','LP ','LP ','LP ',    !Pinus contorta var. murrayana
     &'CP ','109','PICO3   ','OS ','OS ','CP ','OS ','OS ','OS ','OS ',    !Pinus coulteri
     &'PD ','134','PIDI3   ','OS ','OS ','OS ','OS ','PD ','OS ','OS ',    !Pinus discolor
     &'PI ','106','PIED    ','OS ','OS ','OS ','OS ','PI ','OS ','OS ',    !Pinus edulis
     &'AP ','112','PIEN2   ','OS ','OS ','OS ','OS ','PP ','OS ','OS ',    !Pinus engelmannii
     &'LM ','113','PIFL2   ','OS ','LM ','LM ','LM ','LM ','WB ','LM ',    !Pinus flexilis
     &'   ','   ','PIHA7   ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Pinus halepensis
     &'JP ','116','PIJE    ','OS ','OS ','JP ','OS ','OS ','PP ','OS '/    !Pinus jeffreyi
C
      DATA ((ASPT(I,J),J=11,21),I=311,320) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','LP ','OS ','OS ','OS ','OT ','OS ','OS ','LP ',   !Pinus contorta var. contorta
     &'LP ','LP ','PP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',   !Pinus contorta var. latifolia
     &'LP ','LP ','PP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ','LP ',   !Pinus contorta var. murrayana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','CP ','CP ','OT ',   !Pinus coulteri
     &'OS ','OT ','OS ','OT ','OS ','PM ','PI ','OT ','OS ','OS ','OT ',   !Pinus discolor
     &'PI ','OT ','OS ','OT ','OS ','PM ','PI ','OT ','OS ','OS ','OT ',   !Pinus edulis
     &'OS ','OT ','OS ','OT ','OS ','OS ','PP ','OT ','OS ','OS ','OT ',   !Pinus engelmannii
     &'LM ','OT ','OS ','OT ','WB ','LM ','LM ','OT ','LM ','LM ','OT ',   !Pinus flexilis
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus halepensis
     &'OS ','OT ','PP ','JP ','PP ','OS ','OS ','JP ','JP ','JP ','JP '/   !Pinus jeffreyi
C
      DATA ((ASPT(I,J),J=1,10),I=321,330) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'SP ','117','PILA    ','OS ','OS ','SP ','OS ','OS ','WP ','OS ',    !Pinus lambertiana
     &'   ','   ','PILE    ','OS ','OS ','OS ','OS ','CI ','OS ','OS ',    !Pinus leiophylla
     &'CI ','118','PILEC   ','OS ','OS ','OS ','OS ','CI ','OS ','OS ',    !Pinus leiophylla var. chihuahuana
     &'GB ','142','PILO    ','OS ','OS ','OS ','OS ','BC ','OS ','OS ',    !Pinus longaeva
     &'PM ','133','PIMO    ','OS ','OS ','OS ','OS ','PM ','OS ','OS ',    !Pinus monophylla
     &'   ','   ','PIMOC   ','OS ','OS ','OS ','OS ','PI ','OS ','OS ',    !Pinus monophylla var. californiarum
     &'AZ ','143','PIMOF   ','OS ','OS ','OS ','OS ','AZ ','OS ','OS ',    !Pinus monophylla var. fallax
     &'   ','   ','PIMOM2  ','OS ','OS ','OS ','OS ','PI ','OS ','OS ',    !Pinus monophylla var. monophylla
     &'WP ','119','PIMO3   ','OS ','WP ','WP ','WP ','SW ','WP ','PP ',    !Pinus monticola
     &'BP ','120','PIMU    ','OS ','OS ','OS ','OS ','OS ','OS ','OS '/    !Pinus muricata
C
      DATA ((ASPT(I,J),J=11,21),I=321,330) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','SP ','SP ','SP ','OS ','OS ','SP ','SP ','SP ','SP ',   !Pinus lambertiana
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus leiophylla
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus leiophylla var. chihuahuana
     &'OS ','OT ','OS ','OT ','OS ','OS ','GB ','OT ','GB ','OS ','OT ',   !Pinus longaeva
     &'PI ','OT ','OS ','OT ','OS ','PM ','PM ','OT ','PM ','OS ','OT ',   !Pinus monophylla
     &'OS ','OT ','OS ','OT ','OS ','PM ','PM ','OT ','PM ','OS ','OT ',   !Pinus monophylla var. californiarum
     &'PI ','OT ','OS ','OT ','OS ','PM ','PM ','OT ','PM ','OS ','OT ',   !Pinus monophylla var. fallax
     &'PI ','OT ','OS ','OT ','OS ','PM ','PM ','OT ','PM ','OS ','OT ',   !Pinus monophylla var. monophylla
     &'WP ','WP ','SP ','WP ','WP ','OS ','OS ','WP ','WP ','WP ','WP ',   !Pinus monticola
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT '/   !Pinus muricata
C
      DATA ((ASPT(I,J),J=1,10),I=331,340) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'PN ','136','PINI    ','OS ','OS ','OS ','PP ','PP ','OS ','OS ',    !Pinus nigra
     &'   ','   ','PIPI7   ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Pinus pinea
     &'PP ','122','PIPO    ','OS ','PP ','PP ','PP ','PP ','PP ','PP ',    !Pinus ponderosa
     &'   ','   ','PIPOA2  ','OS ','OS ','OS ','OS ','PP ','OS ','OS ',    !Pinus ponderosa ssp. arizonica
     &'   ','   ','PIPOA   ','OS ','OS ','OS ','OS ','PP ','OS ','OS ',    !Pinus ponderosa var. arizonica
     &'   ','   ','PIPOP   ','OS ','PP ','PP ','PP ','PP ','PP ','PP ',    !Pinus ponderosa var. ponderosa
     &'   ','   ','PIPOS   ','OS ','PP ','PP ','PP ','PP ','PP ','PP ',    !Pinus ponderosa var. scopulorum
     &'FO ','138','PIQU    ','OS ','OS ','OS ','OS ','OS ','OS ','OS ',    !Pinus quadrifolia
     &'MP ','124','PIRA2   ','OS ','OS ','MP ','OS ','OS ','OS ','OS ',    !Pinus radiata
     &'GP ','127','PISA2   ','OS ','OS ','GP ','OS ','OS ','OS ','OS '/    !Pinus sabiniana
C
      DATA ((ASPT(I,J),J=11,21),I=331,340) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','OT ','OS ','OS ','PP ','OT ','OS ','OS ','OT ',   !Pinus nigra
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus pinea
     &'PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ',   !Pinus ponderosa
     &'OS ','OT ','OS ','OT ','OS ','PP ','PP ','OT ','OS ','OS ','OT ',   !Pinus ponderosa ssp. arizonica
     &'OS ','OT ','OS ','OT ','OS ','PP ','PP ','OT ','OS ','OS ','OT ',   !Pinus ponderosa var. arizonica
     &'PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ',   !Pinus ponderosa var. ponderosa
     &'PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ','PP ',   !Pinus ponderosa var. scopulorum
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus quadrifolia
     &'OS ','OT ','PP ','LP ','OS ','OS ','OS ','OT ','MP ','MP ','LP ',   !Pinus radiata
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','GP ','GP ','OT '/   !Pinus sabiniana
C
      DATA ((ASPT(I,J),J=1,10),I=341,350) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'SW ','114','PIST3   ','OS ','OS ','OS ','OS ','SW ','OS ','OS ',    !Pinus strobiformis
     &'ST ','130','PISY    ','OS ','OS ','OS ','OS ','LP ','OS ','OS ',    !Pinus sylvestris
     &'TP ','139','PITO    ','OS ','OS ','MP ','OS ','OS ','OS ','OS ',    !Pinus torreyana
     &'   ','   ','PITOI2  ','OS ','OS ','MP ','OS ','OS ','OS ','OS ',    !Pinus torreyana var. insularis
     &'   ','   ','PITOT2  ','OS ','OS ','MP ','OS ','OS ','OS ','OS ',    !Pinus torreyana var. torreyana
     &'WE ','137','PIWA    ','OS ','OS ','PP ','OS ','OS ','OS ','OS ',    !Pinus washoensis
     &'   ','   ','PISTA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Pistacia
     &'   ','   ','PIAT4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Pistacia atlantica
     &'   ','   ','PICH4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Pistacia chinensis
     &'   ','   ','PLOC    ','OH ','OH ','SY ','OH ','NC ','OH ','OH '/    !Platanus occidentalis
C
      DATA ((ASPT(I,J),J=11,21),I=341,350) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','OS ','OT ',   !Pinus strobiformis
     &'OS ','OT ','OS ','OT ','OS ','LP ','LP ','OT ','OS ','OS ','OT ',   !Pinus sylvestris
     &'OS ','OT ','PP ','OT ','OS ','OS ','OS ','OT ','OS ','MP ','OT ',   !Pinus torreyana
     &'OS ','OT ','PP ','OT ','OS ','OS ','OS ','OT ','OS ','MP ','OT ',   !Pinus torreyana var. insularis
     &'OS ','OT ','PP ','OT ','OS ','OS ','OS ','OT ','OS ','MP ','OT ',   !Pinus torreyana var. torreyana
     &'OS ','OT ','PP ','OT ','OS ','OS ','OS ','OT ','WE ','PP ','OT ',   !Pinus washoensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Pistacia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Pistacia atlantica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Pistacia chinensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','SY ','OT '/   !Platanus occidentalis
C
      DATA ((ASPT(I,J),J=1,10),I=351,360) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'SY ','730','PLRA    ','OH ','OH ','SY ','OH ','NC ','OH ','OH ',    !Platanus racemosa
     &'PS ','729','PLATA   ','OH ','OH ','SY ','OH ','NC ','OH ','OH ',    !Platanus spp.
     &'   ','   ','PLWR2   ','OH ','OH ','SY ','OH ','NC ','OH ','OH ',    !Platanus wrightii
     &'CO ','740','POPUL   ','CW ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus
     &'   ','   ','POAC5   ','OH ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus ×acuminata
     &'   ','   ','POBR7   ','OH ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus ×brayshawii
     &'   ','   ','POCA19  ','OH ','CW ','CW ','CW ','OH ','CW ','OH ',    !Populus ×canadensis
     &'   ','   ','POHI8   ','OH ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus ×hinckleyana
     &'   ','   ','POIN23  ','OH ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus ×inopina
     &'   ','   ','POJA2   ','OH ','CW ','CW ','CW ','OH ','CW ','OH '/    !Populus ×jackii
C
      DATA ((ASPT(I,J),J=11,21),I=351,360) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','SY ','OT ',   !Platanus racemosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','SY ','OT ',   !Platanus spp.
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','SY ','OT ',   !Platanus wrightii
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus ×acuminata
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus ×brayshawii
     &'OH ','OT ','OH ','CW ','CW ','OH ','NC ','CW ','OH ','CW ','CW ',   !Populus ×canadensis
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus ×hinckleyana
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus ×inopina
     &'OH ','OT ','OH ','CW ','CW ','OH ','NC ','CW ','OH ','CW ','CW '/   !Populus ×jackii
C
      DATA ((ASPT(I,J),J=1,10),I=361,370) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','POPA11  ','OH ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus ×parryi
     &'   ','   ','POAL7   ','OH ','CW ','CW ','CW ','OH ','CW ','CW ',    !Populus alba
     &'NC ','749','POAN3   ','OH ','CW ','CW ','CW ','NC ','CW ','NC ',    !Populus angustifolia
     &'BA ','741','POBA2   ','BA ','CW ','CW ','CW ','NC ','CW ','BA ',    !Populus balsamifera
     &'   ','   ','POBAB2  ','BA ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus balsamifera ssp. balsamifera
     &'CW ','747','POBAT   ','CW ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus balsamifera ssp. trichocarpa
     &'   ','   ','PODE3   ','OH ','CW ','CW ','CW ','PW ','CW ','CW ',    !Populus deltoides
     &'PW ','745','PODEM   ','OH ','CW ','CW ','CW ','PW ','CW ','PW ',    !Populus deltoides ssp. monilifera
     &'   ','   ','PODEW   ','OH ','CW ','CW ','CW ','PW ','CW ','OH ',    !Populus deltoides ssp. wislizeni
     &'FC ','748','POFR2   ','OH ','CW ','CW ','CW ','NC ','CW ','OH '/    !Populus fremontii
C
      DATA ((ASPT(I,J),J=11,21),I=361,370) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus ×parryi
     &'CO ','OT ','OH ','CW ','CW ','OH ','NC ','CW ','OH ','CW ','CW ',   !Populus alba
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus angustifolia
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus balsamifera
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus balsamifera ssp. balsamifera
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus balsamifera ssp. trichocarpa
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus deltoides
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus deltoides ssp. monilifera
     &'OH ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus deltoides ssp. wislizeni
     &'OH ','OT ','OH ','CW ','CW ','NC ','FC ','CW ','OH ','CW ','CW '/   !Populus fremontii
C
      DATA ((ASPT(I,J),J=1,10),I=371,380) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','POFRF3  ','OH ','CW ','CW ','CW ','NC ','CW ','OH ',    !Populus fremontii ssp. fremontii
     &'   ','   ','POFRM   ','OH ','CW ','CW ','CW ','NC ','CW ','OH ',    !Populus fremontii ssp. mesetae
     &'   ','   ','PONI    ','OH ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus nigra
     &'AS ','746','POTR5   ','AS ','AS ','AS ','AS ','AS ','AS ','AS ',    !Populus tremuloides
     &'   ','   ','POTR15  ','CW ','CW ','CW ','CW ','NC ','CW ','CW ',    !Populus trichocarpa
     &'MQ ','755','PROSO   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Prosopis
     &'   ','   ','PRGL2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Prosopis glandulosa
     &'   ','   ','PRGLG   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Prosopis glandulosa var. glandulosa
     &'HM ','756','PRGLT   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Prosopis glandulosa var. torreyana
     &'SM ','758','PRPU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Prosopis pubescens
C
      DATA ((ASPT(I,J),J=11,21),I=371,380) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','CW ','CW ','NC ','FC ','CW ','OH ','CW ','CW ',   !Populus fremontii ssp. fremontii
     &'OH ','OT ','OH ','CW ','CW ','NC ','FC ','CW ','OH ','CW ','CW ',   !Populus fremontii ssp. mesetae
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus nigra
     &'AS ','OT ','OH ','AS ','AS ','AS ','AS ','AS ','AS ','AS ','AS ',   !Populus tremuloides
     &'CO ','OT ','OH ','CW ','CW ','NC ','NC ','CW ','OH ','CW ','CW ',   !Populus trichocarpa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Prosopis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Prosopis glandulosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Prosopis glandulosa var. glandulosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Prosopis glandulosa var. torreyana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Prosopis pubescens
C
      DATA ((ASPT(I,J),J=1,10),I=381,390) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'VM ','757','PRVE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Prosopis velutina
     &'PL ','760','PRUNU   ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Prunus
     &'   ','   ','PRAM    ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Prunus americana
     &'CH ','768','PREM    ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Prunus emarginata
     &'   ','   ','PRVID   ','OH ','OH ','OH ','OH ','OH ','PL ','OH ',    !Prunus virginiana var. demissa
     &'OD ','200','PSEUD7  ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga
     &'BD ','201','PSMA    ','OS ','DF ','OS ','OS ','OS ','DF ','OS ',    !Pseudotsuga macrocarpa
     &'DF ','202','PSME    ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga menziesii
     &'   ','   ','PSMEG   ','OS ','DF ','DF ','DF ','DF ','DF ','DF ',    !Pseudotsuga menziesii var. glauca
     &'   ','   ','PSMEM   ','OS ','DF ','DF ','DF ','DF ','DF ','DF '/    !Pseudotsuga menziesii var. menziesii
C
      DATA ((ASPT(I,J),J=11,21),I=381,390) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Prosopis velutina
     &'OH ','OT ','OH ','CH ','CH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Prunus
     &'OH ','OT ','OH ','CH ','CH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Prunus americana
     &'OH ','OT ','OH ','CH ','CH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Prunus emarginata
     &'OH ','OT ','OH ','CH ','CH ','OH ','OH ','CH ','OH ','OH ','CH ',   !Prunus virginiana var. demissa
     &'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',   !Pseudotsuga
     &'OS ','OT ','OS ','DF ','OS ','OS ','OS ','DF ','BD ','OS ','DF ',   !Pseudotsuga macrocarpa
     &'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',   !Pseudotsuga menziesii
     &'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ',   !Pseudotsuga menziesii var. glauca
     &'DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF ','DF '/   !Pseudotsuga menziesii var. menziesii
C
      DATA ((ASPT(I,J),J=1,10),I=391,400) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','PTELE   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ptelea
     &'   ','   ','PTTR    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ptelea trifoliata
     &'   ','   ','PTTRP   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ptelea trifoliata ssp. pallida
     &'   ','   ','PTTRC2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ptelea trifoliata ssp. pallida  var. confinis
     &'   ','   ','PTTRP2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ptelea trifoliata ssp. polyadenia
     &'   ','   ','PYCO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Pyrus communis
     &'OA ','800','QUERC   ','OH ','OH ','LO ','OH ','GO ','WO ','OH ',    !Quercus
     &'OE ','850','        ','OH ','OH ','LO ','OH ','EM ','WO ','OH ',    !Quercus
     &'   ','   ','QUMO2   ','OH ','OH ','OH ','OH ','GO ','OH ','OH ',    !Quercus ×moreha
     &'   ','   ','QUPA4   ','OH ','OH ','LO ','OH ','GO ','WO ','OH '/    !Quercus ×pauciloba
C
      DATA ((ASPT(I,J),J=11,21),I=391,400) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ptelea
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ptelea trifoliata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ptelea trifoliata ssp. pallida
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ptelea trifoliata ssp. pallida  var. confinis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ptelea trifoliata ssp. polyadenia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Pyrus communis
     &'OH ','OT ','BO ','WO ','WO ','OH ','GO ','WO ','BO ','LO ','WO ',   !Quercus
     &'OH ','OT ','BO ','WO ','WO ','OH ','GO ','WO ','BO ','LO ','WO ',   !Quercus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','BO ','OH ','OT ',   !Quercus ×moreha
     &'OH ','OT ','BO ','WO ','WO ','OH ','GO ','WO ','LO ','LO ','WO '/   !Quercus ×pauciloba
C
      DATA ((ASPT(I,J),J=1,10),I=401,410) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'LO ','801','QUAG    ','OH ','OH ','LO ','OH ','OH ','WO ','OH ',    !Quercus agrifolia
     &'AW ','803','QUAR    ','OH ','OH ','OH ','OH ','AW ','OH ','OH ',    !Quercus arizonica
     &'CY ','805','QUCH2   ','OH ','OH ','CY ','OH ','EM ','OH ','OH ',    !Quercus chrysolepis
     &'   ','   ','QUCHC   ','OH ','OH ','CY ','OH ','EM ','OH ','OH ',    !Quercus chrysolepis var. chrysolepis
     &'BL ','807','QUDO    ','OH ','OH ','BL ','OH ','EM ','WO ','OH ',    !Quercus douglasii
     &'   ','   ','QUDU    ','OH ','OH ','IO ','OH ','OH ','OH ','OH ',    !Quercus dumosa
     &'EM ','810','QUEM    ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus emoryi
     &'EO ','811','QUEN    ','OH ','OH ','EO ','OH ','EM ','OH ','OH ',    !Quercus engelmannii
     &'GO ','814','QUGA    ','OH ','OH ','OH ','OH ','GO ','WO ','OH ',    !Quercus gambelii
     &'   ','   ','QUGAG   ','OH ','OH ','OH ','OH ','GO ','WO ','OH '/    !Quercus gambelii var. gambelii
C
      DATA ((ASPT(I,J),J=11,21),I=401,410) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','LO ','LO ','WO ',   !Quercus agrifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus arizonica
     &'OH ','OT ','BO ','WO ','WO ','OH ','GO ','WO ','CY ','CY ','WO ',   !Quercus chrysolepis
     &'OH ','OT ','BO ','WO ','WO ','OH ','GO ','WO ','CY ','CY ','WO ',   !Quercus chrysolepis var. chrysolepis
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','BL ','BL ','WO ',   !Quercus douglasii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','IO ','OT ',   !Quercus dumosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus emoryi
     &'OH ','OT ','BO ','OT ','OH ','OH ','OH ','OT ','BO ','EO ','OT ',   !Quercus engelmannii
     &'OH ','OT ','OH ','OT ','OH ','OH ','GO ','OT ','BO ','OH ','OT ',   !Quercus gambelii
     &'OH ','OT ','OH ','OT ','OH ','OH ','GO ','OT ','BO ','OH ','OT '/   !Quercus gambelii var. gambelii
C
      DATA ((ASPT(I,J),J=1,10),I=411,420) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'WO ','815','QUGA4   ','OH ','OH ','WO ','OH ','OH ','WO ','OH ',    !Quercus garryana
     &'   ','   ','QUGAF   ','OH ','OH ','WO ','OH ','OH ','WO ','OH ',    !Quercus garryana var. fruticosa
     &'   ','   ','QUGAG2  ','OH ','OH ','WO ','OH ','OH ','WO ','OH ',    !Quercus garryana var. garryana
     &'   ','   ','QUGAS   ','OH ','OH ','WO ','OH ','OH ','WO ','OH ',    !Quercus garryana var. semota
     &'GR ','846','QUGR3   ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus grisea
     &'SO ','843','QUHY    ','OH ','OH ','OH ','OH ','SO ','OH ','OH ',    !Quercus hypoleucoides
     &'BO ','818','QUKE    ','OH ','OH ','BO ','OH ','OH ','WO ','OH ',    !Quercus kelloggii
     &'VO ','821','QULO    ','OH ','OH ','VO ','OH ','OH ','WO ','OH ',    !Quercus lobata
     &'BK ','823','QUMA2   ','OH ','OH ','OH ','OH ','BK ','OH ','OH ',    !Quercus macrocarpa
     &'CK ','826','QUMU    ','OH ','OH ','OH ','OH ','BK ','OH ','OH '/    !Quercus muehlenbergii
C
      DATA ((ASPT(I,J),J=11,21),I=411,420) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','BO ','WO ','WO ',   !Quercus garryana
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','BO ','WO ','WO ',   !Quercus garryana var. fruticosa
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','BO ','WO ','WO ',   !Quercus garryana var. garryana
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','BO ','WO ','WO ',   !Quercus garryana var. semota
     &'OH ','OT ','BO ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus grisea
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus hypoleucoides
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','BO ','BO ','WO ',   !Quercus kelloggii
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','VO ','VO ','WO ',   !Quercus lobata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus macrocarpa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Quercus muehlenbergii
C
      DATA ((ASPT(I,J),J=1,10),I=421,430) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'MK ','829','QUOB    ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus oblongifolia
     &'   ','   ','QURU4   ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus rugosa
     &'   ','   ','QUSA2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Quercus sadleriana
     &'   ','   ','QUTO2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Quercus toumeyi
     &'   ','   ','QUTU2   ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus turbinella
     &'   ','   ','QUUN    ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus undulata
     &'   ','   ','QUVA    ','OH ','OH ','OH ','OH ','EM ','OH ','OH ',    !Quercus vacciniifolia
     &'IO ','839','QUWI2   ','OH ','OH ','IO ','OH ','OH ','OH ','OH ',    !Quercus wislizeni
     &'   ','   ','QUWIF   ','OH ','OH ','IO ','OH ','OH ','OH ','OH ',    !Quercus wislizeni var. frutescens
     &'   ','   ','QUWIW   ','OH ','OH ','IO ','OH ','OH ','OH ','OH '/    !Quercus wislizeni var. wislizeni
C
      DATA ((ASPT(I,J),J=11,21),I=421,430) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus oblongifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus rugosa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus sadleriana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Quercus toumeyi
     &'OH ','OT ','OH ','OT ','OH ','OH ','GO ','OT ','OH ','OH ','OT ',   !Quercus turbinella
     &'OH ','OT ','OH ','OT ','OH ','OH ','GO ','OT ','OH ','OH ','OT ',   !Quercus undulata
     &'OH ','OT ','OH ','OT ','OH ','OH ','GO ','OT ','OH ','OH ','OT ',   !Quercus vacciniifolia
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','IO ','IO ','WO ',   !Quercus wislizeni
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','IO ','IO ','WO ',   !Quercus wislizeni var. frutescens
     &'OH ','OT ','BO ','WO ','WO ','OH ','OH ','WO ','IO ','IO ','WO '/   !Quercus wislizeni var. wislizeni
C
      DATA ((ASPT(I,J),J=1,10),I=431,440) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ROBIN   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia
     &'   ','   ','ROHO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia ×holdtii
     &'   ','   ','ROHI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia hispida
     &'   ','   ','ROHIF8  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia hispida var. fertilis
     &'   ','   ','ROHIH   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia hispida var. hispida
     &'NM ','902','RONE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia neomexicana
     &'   ','   ','RONEN   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia neomexicana var. neomexicana
     &'   ','   ','RONER   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia neomexicana var. rusbyi
     &'RP ','901','ROPS    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Robinia pseudoacacia
     &'WI ','920','SALIX   ','WI ','OH ','WI ','OH ','OH ','WI ','OH '/    !Salix
C
      DATA ((ASPT(I,J),J=11,21),I=431,440) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia ×holdtii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia hispida
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia hispida var. fertilis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia hispida var. hispida
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia neomexicana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia neomexicana var. neomexicana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia neomexicana var. rusbyi
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Robinia pseudoacacia
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI '/   !Salix
C
      DATA ((ASPT(I,J),J=1,10),I=441,450) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','SAAL    ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix alaxensis
     &'   ','   ','SAALA   ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix alaxensis var. alaxensis
     &'   ','   ','SAALL   ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix alaxensis var. longistylis
     &'   ','   ','SAAL2   ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix alba
     &'   ','   ','SAAM2   ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix amygdaloides
     &'   ','   ','SABE2   ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix bebbiana
     &'   ','   ','SABO    ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix bonplandiana
     &'   ','   ','SAEX    ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix exigua
     &'   ','   ','SAFR    ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix fragilis
     &'   ','   ','SAGL    ','WI ','OH ','WI ','OH ','OH ','WI ','OH '/    !Salix glauca
C
      DATA ((ASPT(I,J),J=11,21),I=441,450) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix alaxensis
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix alaxensis var. alaxensis
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix alaxensis var. longistylis
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix alba
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix amygdaloides
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix bebbiana
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix bonplandiana
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix exigua
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix fragilis
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI '/   !Salix glauca
C
      DATA ((ASPT(I,J),J=1,10),I=451,460) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','SAGO    ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix gooddingii
     &'   ','   ','SALA5   ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix lasiandra
     &'   ','   ','SALA6   ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix lasiolepis
     &'   ','   ','SALAB   ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix lasiolepis var. bigelovii
     &'   ','   ','SALAL2  ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix lasiolepis var. lasiolepis
     &'   ','   ','SALI    ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix ligulifolia
     &'   ','   ','SALU    ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix lucida
     &'   ','   ','SALUL   ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix lucida ssp. lasiandra
     &'   ','   ','SAMA12  ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix maccalliana
     &'   ','   ','SAMO2   ','OH ','OH ','WI ','OH ','OH ','WI ','OH '/    !Salix monticola
C
      DATA ((ASPT(I,J),J=11,21),I=451,460) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix gooddingii
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix lasiandra
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix lasiolepis
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix lasiolepis var. bigelovii
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix lasiolepis var. lasiolepis
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix ligulifolia
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix lucida
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix lucida ssp. lasiandra
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix maccalliana
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI '/   !Salix monticola
C
      DATA ((ASPT(I,J),J=1,10),I=461,470) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','SAPL2   ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix planifolia
     &'   ','   ','SAPLP4  ','WI ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix planifolia sp. planifolia
     &'SU ','928','SASC    ','SU ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix scouleriana
     &'   ','   ','SATA    ','OH ','OH ','WI ','OH ','OH ','WI ','OH ',    !Salix taxifolia
     &'   ','   ','SAMBU   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Sambucus
     &'   ','   ','SANIC4  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Sambucus nigra ssp. canadensis
     &'   ','   ','SAPIN   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Sapindus
     &'   ','   ','SASA4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Sapindus saponaria
     &'   ','   ','SASAD   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Sapindus saponaria var. drummondii
     &'   ','   ','SCHIN   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Schinus
C
      DATA ((ASPT(I,J),J=11,21),I=461,470) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix planifolia
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix planifolia sp. planifolia
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix scouleriana
     &'OH ','OT ','OH ','WI ','WI ','OH ','OH ','WI ','OH ','WI ','WI ',   !Salix taxifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Sambucus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Sambucus nigra ssp. canadensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Sapindus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Sapindus saponaria
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Sapindus saponaria var. drummondii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Schinus
C
      DATA ((ASPT(I,J),J=1,10),I=471,480) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','SCMO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Schinus molle
     &'   ','   ','SCPO7   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Schinus polygamus
     &'   ','   ','SCTE    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Schinus terebinthifolius
     &'   ','   ','SCTER2  ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Schinus terebinthifolius var. raddianus
     &'RW ','211','SESE3   ','OS ','OS ','GS ','OS ','OS ','OS ','OS ',    !Sequoia sempervirens
     &'GS ','212','SEGI2   ','OS ','OS ','GS ','OS ','OS ','OS ','OS ',    !Sequoiadendron giganteum
     &'   ','   ','SYRIN   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Syringa
     &'   ','   ','SYRE2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Syringa reticulata
     &'   ','   ','SYREA   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Syringa reticulata ssp. amurensis
     &'   ','   ','SYVU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Syringa vulgaris
C
      DATA ((ASPT(I,J),J=11,21),I=471,480) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Schinus molle
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Schinus polygamus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Schinus terebinthifolius
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Schinus terebinthifolius var. raddianus
     &'OS ','OT ','OS ','RW ','OS ','OH ','OS ','RW ','RW ','GS ','RW ',   !Sequoia sempervirens
     &'OS ','OT ','OS ','RW ','OS ','OS ','OS ','RW ','GS ','GS ','RW ',   !Sequoiadendron giganteum
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Syringa
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Syringa reticulata
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Syringa reticulata ssp. amurensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Syringa vulgaris
C
      DATA ((ASPT(I,J),J=1,10),I=481,490) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'SC ','991','TAMAR2  ','OH ','OH ','OH ','OH ','OS ','OH ','OH ',    !Tamarix
     &'   ','   ','TACH2   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Tamarix chinensis
     &'   ','   ','TAPA4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Tamarix parviflora
     &'   ','   ','TARA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Tamarix ramosissima
     &'OY ','230','TAXUS   ','OS ','PY ','PY ','PY ','OS ','PY ','OS ',    !Taxus
     &'   ','   ','TABA80  ','OS ','PY ','PY ','PY ','OS ','PY ','OS ',    !Taxus baccata
     &'PY ','231','TABR2   ','OS ','PY ','PY ','PY ','OS ','PY ','OS ',    !Taxus brevifolia
     &'RC ','242','THPL    ','RC ','GF ','RC ','RC ','RC ','RC ','OS ',    !Thuja plicata
     &'C  ','   ','        ','RC ','GF ','RC ','RC ','RC ','RC ','OS ',    !Thuja plicata (Old Code)
     &'TS ','240','THUJA   ','RC ','GF ','RC ','RC ','RC ','RC ','OS '/    !Thuja spp.
C
      DATA ((ASPT(I,J),J=11,21),I=481,490) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Tamarix
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Tamarix chinensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Tamarix parviflora
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Tamarix ramosissima
     &'PY ','OT ','OS ','PY ','PY ','OS ','OS ','PY ','OS ','PY ','PY ',   !Taxus
     &'PY ','OT ','OS ','PY ','PY ','OS ','OS ','PY ','OS ','PY ','PY ',   !Taxus baccata
     &'PY ','OT ','OS ','PY ','PY ','OS ','OS ','PY ','OS ','PY ','PY ',   !Taxus brevifolia
     &'RC ','RC ','OS ','RC ','RC ','OS ','OS ','RC ','OS ','RC ','RC ',   !Thuja plicata
     &'RC ','RC ','OS ','RC ','RC ','OS ','OS ','RC ','OS ','RC ','RC ',   !Thuja plicata (Old Code)
     &'RC ','RC ','OS ','RC ','RC ','OS ','OS ','RC ','OS ','RC ','RC '/   !Thuja spp.
C
      DATA ((ASPT(I,J),J=1,10),I=491,500) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','TIEU4   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Tilia ×europaea
     &'CN ','251','TOCA    ','OS ','OS ','CN ','OS ','OS ','OS ','OS ',    !Torreya californica
     &'   ','   ','TRSE6   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Triadica sebifera
     &'HS ','260','TSUGA   ','WH ','MH ','WH ','WH ','MH ','WH ','OS ',    !Tsuga
     &'   ','   ','TSJE    ','OS ','MH ','WH ','OS ','OS ','WH ','OS ',    !Tsuga ×jeffreyi
     &'WH ','263','TSHE    ','WH ','GF ','WH ','WH ','OS ','WH ','OS ',    !Tsuga heterophylla
     &'MH ','264','TSME    ','MH ','MH ','MH ','OS ','MH ','MH ','OS ',    !Tsuga mertensiana
     &'   ','   ','ULMUS   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ulmus
     &'   ','   ','ULAM    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ulmus americana
     &'   ','   ','ULGL    ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Ulmus glabra
C
      DATA ((ASPT(I,J),J=11,21),I=491,500) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Tilia ×europaea
     &'OS ','OT ','OS ','OT ','OS ','OS ','OS ','OT ','OS ','CN ','OT ',   !Torreya californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Triadica sebifera
     &'WH ','WH ','DF ','WH ','WH ','OS ','OS ','WH ','MH ','WH ','WH ',   !Tsuga
     &'WH ','WH ','DF ','WH ','WH ','OS ','OS ','WH ','OS ','WH ','WH ',   !Tsuga ×jeffreyi
     &'WH ','WH ','DF ','WH ','WH ','OS ','OS ','WH ','OS ','WH ','WH ',   !Tsuga heterophylla
     &'MH ','WH ','OS ','MH ','MH ','OS ','AF ','MH ','MH ','MH ','MH ',   !Tsuga mertensiana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ulmus
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ulmus americana
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Ulmus glabra
C
      DATA ((ASPT(I,J),J=1,10),I=501,510) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','ULPA    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ulmus parvifolia
     &'   ','   ','ULPR    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ulmus procera
     &'   ','   ','ULPU    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Ulmus pumila
     &'   ','   ','UMBEL   ','OH ','OH ','CL ','OH ','OH ','OH ','OH ',    !Umbellularia
     &'CL ','981','UMCA    ','OH ','OH ','CL ','OH ','OH ','OH ','OH ',    !Umbellularia californica
     &'   ','   ','UMCAC   ','OH ','OH ','CL ','OH ','OH ','OH ','OH ',    !Umbellularia californica var. californica
     &'   ','   ','UMCAF   ','OH ','OH ','CL ','OH ','OH ','OH ','OH ',    !Umbellularia californica var. fresnensis
     &'   ','   ','VAUQU   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Vauquelinia
     &'   ','   ','VACA5   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Vauquelinia californica
     &'   ','   ','VACAP   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Vauquelinia californica  ssp. pauciflora
C
      DATA ((ASPT(I,J),J=11,21),I=501,510) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ulmus parvifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ulmus procera
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Ulmus pumila
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','CL ','CL ','OT ',   !Umbellularia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','CL ','CL ','OT ',   !Umbellularia californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','CL ','CL ','OT ',   !Umbellularia californica var. californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','CL ','CL ','OT ',   !Umbellularia californica var. fresnensis
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Vauquelinia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Vauquelinia californica
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Vauquelinia californica  ssp. pauciflora
C
      DATA ((ASPT(I,J),J=1,10),I=511,517) /
C     ALFA   FIA   PLNT       AK    BM    CA    CI    CR    EC    EM        SPECIES
     &'   ','   ','VEFO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Vernicia fordii
     &'   ','   ','WASHI   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Washingtonia
     &'   ','   ','WAFI    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Washingtonia filifera
     &'   ','   ','WARO    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Washingtonia robusta
     &'   ','   ','YUBR    ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Yucca brevifolia
     &'   ','   ','YUBRB   ','OH ','OH ','OH ','OH ','OH ','OH ','OH ',    !Yucca brevifolia var. brevifolia
     &'   ','   ','YUBRJ   ','OH ','OH ','OH ','OH ','OH ','OH ','OH '/    !Yucca brevifolia var. jaegeriana
C
      DATA ((ASPT(I,J),J=11,21),I=511,517) /
C      IE    KT    NC    PN    SO    TT    UT    WC    WS    OC    OP       SPECIES
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Vernicia fordii
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Washingtonia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Washingtonia filifera
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Washingtonia robusta
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Yucca brevifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT ',   !Yucca brevifolia var. brevifolia
     &'OH ','OT ','OH ','OT ','OH ','OH ','OH ','OT ','OH ','OH ','OT '/   !Yucca brevifolia var. jaegeriana
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
        SELECT CASE (VAR)
          CASE('AK')
            SPCOUT= ASPT(I,4)(1:3)
          CASE('BM')
            SPCOUT= ASPT(I,5)(1:3)
          CASE('CA')
            SPCOUT= ASPT(I,6)(1:3)
          CASE('CI')
            SPCOUT= ASPT(I,7)(1:3)
          CASE('CR')
            SPCOUT= ASPT(I,8)(1:3)
          CASE('EC')
            SPCOUT= ASPT(I,9)(1:3)
          CASE('EM')
            SPCOUT= ASPT(I,10)(1:3)
          CASE('IE')
            SPCOUT= ASPT(I,11)(1:3)
          CASE('KT')
            SPCOUT= ASPT(I,12)(1:3)
          CASE('NC')
            SPCOUT= ASPT(I,13)(1:3)
          CASE('PN')
            SPCOUT= ASPT(I,14)(1:3)
          CASE('SO')
            SPCOUT= ASPT(I,15)(1:3)
          CASE('TT')
            SPCOUT= ASPT(I,16)(1:3)
          CASE('UT')
            SPCOUT= ASPT(I,17)(1:3)
          CASE('WC')
            SPCOUT= ASPT(I,18)(1:3)
          CASE('WS')
            SPCOUT= ASPT(I,19)(1:3)
          CASE('OC')
            SPCOUT= ASPT(I,20)(1:3)
          CASE('OP')
            SPCOUT= ASPT(I,21)(1:3)
        END SELECT 
        GO TO 150        
      ELSEIF (SPCIN .EQ. ASPT(I,2)) THEN
        IJSPIN=2
        SELECT CASE (VAR)
          CASE('AK') 
            SPCOUT= ASPT(I,4)(1:3)
          CASE('BM') 
            SPCOUT= ASPT(I,5)(1:3)
          CASE('CA') 
            SPCOUT= ASPT(I,6)(1:3)
          CASE('CI') 
            SPCOUT= ASPT(I,7)(1:3)
          CASE('CR') 
            SPCOUT= ASPT(I,8)(1:3)
          CASE('EC') 
            SPCOUT= ASPT(I,9)(1:3)
          CASE('EM') 
            SPCOUT= ASPT(I,10)(1:3)
          CASE('IE') 
            SPCOUT= ASPT(I,11)(1:3)
          CASE('KT') 
            SPCOUT= ASPT(I,12)(1:3)
          CASE('NC') 
            SPCOUT= ASPT(I,13)(1:3)
          CASE('PN') 
            SPCOUT= ASPT(I,14)(1:3)
          CASE('SO') 
            SPCOUT= ASPT(I,15)(1:3)
          CASE('TT') 
            SPCOUT= ASPT(I,16)(1:3)
          CASE('UT') 
            SPCOUT= ASPT(I,17)(1:3)
          CASE('WC') 
            SPCOUT= ASPT(I,18)(1:3)
          CASE('WS') 
            SPCOUT= ASPT(I,19)(1:3)
          CASE('OC') 
            SPCOUT= ASPT(I,20)(1:3)
          CASE('OP')
            SPCOUT= ASPT(I,21)(1:3)
        END SELECT
        GO TO 150   
      ELSEIF (SPCIN .EQ. ASPT(I,3)) THEN
        IJSPIN=3
        SELECT CASE (VAR)
          CASE('AK') 
            SPCOUT= ASPT(I,4)(1:3)
          CASE('BM') 
            SPCOUT= ASPT(I,5)(1:3)
          CASE('CA') 
            SPCOUT= ASPT(I,6)(1:3)
          CASE('CI') 
            SPCOUT= ASPT(I,7)(1:3)
          CASE('CR') 
            SPCOUT= ASPT(I,8)(1:3)
          CASE('EC') 
            SPCOUT= ASPT(I,9)(1:3)
          CASE('EM') 
            SPCOUT= ASPT(I,10)(1:3)
          CASE('IE') 
            SPCOUT= ASPT(I,11)(1:3)
          CASE('KT') 
            SPCOUT= ASPT(I,12)(1:3)
          CASE('NC') 
            SPCOUT= ASPT(I,13)(1:3)
          CASE('PN') 
            SPCOUT= ASPT(I,14)(1:3)
          CASE('SO') 
            SPCOUT= ASPT(I,15)(1:3)
          CASE('TT') 
            SPCOUT= ASPT(I,16)(1:3)
          CASE('UT') 
            SPCOUT= ASPT(I,17)(1:3)
          CASE('WC') 
            SPCOUT= ASPT(I,18)(1:3)
          CASE('WS') 
            SPCOUT= ASPT(I,19)(1:3)
          CASE('OC') 
            SPCOUT= ASPT(I,20)(1:3)
          CASE('OP')
            SPCOUT= ASPT(I,21)(1:3)
        END SELECT
        GO TO 150   
      ENDIF      
  100 CONTINUE
  150 CONTINUE  
C----------
C  FIND STANDARD SPECIES NUMBER FOR VARIANT TO RETURN TO INTREE.
C  IF SPECIES CODE WAS NOT SET, RETURN MAXIMUM SPECIES NUMBER.
C----------
      ISPC1= 0
      IF(SPCOUT.EQ.'XX ')THEN
        ISPC1=MAXSP
        GO TO 300
      ENDIF
      DO 200 J2= 1,MAXSP
      IF(SPCOUT(1:2) .EQ. NSP(J2,1)(1:2)) THEN
        ISPC1= J2
        GO TO 300
      ENDIF
  200 CONTINUE
  300 CONTINUE
      IF(JSPINDEF.LE.0)JSPINDEF=IJSPIN
      JSPIN(ISPC1)=IJSPIN
C----------
C  IN THE EVENT THAT EXECUTABLE IS THE OLD 11 SPECIES SORNEC VARIANT
C  SET ISPC1 EQUAL TO MAXSP
C----------
      IF(ISPC1.EQ.0)ISPC1=MAXSP
      RETURN
      END
