      SUBROUTINE FMPHOTOVAL (FotoRef, Fotocode, FotoVal, FotoValS)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     CONTAINS THE SURFACE FUEL VALUES (TONS/ACRE) CORRESPONDING TO
C     TO THE PHOTO (FOTOCODE) FROM THE ASSOCIATED PHOTO SERIES
C     REFERENCE (FOTOREF)
C
C     FOTOREF - THE PHOTO SERIES REFERENCE NUMBER (1 - 32, SEE BELOW)
C     FOTOCODE - THE INTEGER PHOTO CODE (SEE FMPHOTOCODE FOR
C                ASSOCIATED CHARACTER STRING)
C     REFXVLT - ARRAY WITH TOTAL (HARD + SOFT) VALUES FOR REFERENCE X
C     REFXVLH - ARRAY WITH HARD VALUES FOR REFERENCE X
C     REFXVLS - ARRAY WITH SOFT VALUES FOR REFERENCE X
C     FOTOVAL - THE TONS/ACRE (HARD/SOUND) ASSOCIATED WITH THIS PHOTO
C               THE SIZE CATEGORIES MATCH FMPARM.F77
C               A -1 MEANS THAT THIS CATEGORY WAS NOT LISTED IN THE GUIDE
C     FOTOVALS - THE TONS/ACRE (SOFT/ROTTEN) ASSOCIATED WITH THIS PHOTO
C               THE SIZE CATEGORIES MATCH FMPARM.F77 (NO LITTER OR DUFF)
C
C     PHOTO SERIES REFERENCES:
C     NOTE: Some reference numbers (4,10) are not used and one is a replicate
C     of another (14, 15).  This was not changed to maintain consistency with the photo reference numbers used in FSVEG.
C
C     1 – Fischer, W.C. 1981. Photo guide for appraising downed woody fuels in Montana forests: grand fir-larch-Douglas-fir,
C     western hemlock, western redcedar-western hemlock, and western redcedar cover types. Gen. Tech. Rep. INT-96. Ogden, UT:
C     U.S. Department of Agriculture, Forest Service, Intermountain Forest and Range Experiment Station. 53 p.
C
C     2 – Fischer, W.C. 1981. Photo guide for appraising downed woody fuels in Montana forests: interior ponderosa pine,
C     ponderosa pine-larch-Douglas-fir, larch-Douglas-fir, and interior Douglas-fir cover types. Gen. Tech. Rep. INT-97.
C     Ogden, UT: U.S. Department of Agriculture, Forest Service, Intermountain Forest and Range Experiment Station. 133 p.
C
C     Also published by the National Wildfire Coordinating Group as PMS 820 / NFES 2293
C
C     3 – Fischer, W.C. 1981. Photo guide for appraising downed woody fuels in Montana forests: lodgdpole pine and Engelmann
C     spruce-subalpine fir cover types. Gen. Tech. Rep. INT-98. Ogden, UT: U.S. Department of Agriculture, Forest Service,
C     Intermountain Forest and Range Experiment Station. 143 p.
C
C     Also published by the National Wildfire Coordinating Group as PMS 821 / NFES 2294
C
C     4 – not used
C
C     5 – Koski, W.H. and W.C. Fischer. 1979. Photo series for appraising thinning slash in north Idaho: western hemlock,
C     grand fir, and western redcedar timber types. Gen. Tech. Rep. INT-46. Ogden, UT: U.S. Department of Agriculture,
C     Forest Service, Intermountain Forest and Range Experiment Station. 50 p.
C
C     6 – Maxwell, W.G. and F.R. Ward. 1976. Photo series for quantifying forest residues in the ponderosa pine type,
C     ponderosa pine and associated species type, lodgepole pine type.  Gen. Tech. Rep. PNW-52. Portland, OR: U.S.
C     Department of Agriculture, Forest Service, Pacific Northwest Forest and Range Experiment Station. 73 p.
C
C     7 – Blonski, K.S. and J.L. Schramel. 1981. Photo series for quantifying natural forest residues: southern Cascades,
C     northern Sierra Nevada. Gen. Tech. Rep. PSW-56. Berkeley, CA: U.S. Department of Agriculture, Forest Service, Pacific
C     Southwest Forest and Range Experiment Station. 145 p.
C
C     Also published by the National Wildfire Coordinating Group as PMS 818 / NFES 1872
C
C     8 – Maxwell, W.G. and F.R. Ward. 1980. Photo series for quantifying natural forest residues in common vegetation types
C     of the Pacific Northwest. Gen. Tech. Rep. PNW-105. Portland, OR: U.S. Department of Agriculture, Forest Service,
C     Pacific Northwest Forest and Range Experiment Station. 230 p.
C
C     9 – Ottmar, R.D. and C.C. Hardy. 1989. Stereo photo series for quantifying forest residues in coastal Oregon forests:
C     second-growth Douglas-fir-western hemlock type, western hemlock-Stika spruce type, and red alder type. Gen. Tech. Rep.
C     PNW-GTR-231. Portland, OR: U.S. Department of Agriculture, Forest Service, Pacific Northwest Research Station. 67 p.
C
C     10 – not used
C
C     11 – Maxwell, W.G. 1982. Photo series for quantifying forest residues in the black hills, ponderosa pine type, spruce
C     type. A-89-6-82. U.S. Department of Agriculture, Forest Service, Rocky Mountain Region. 80 p.
C
C     12 – 1997?. Photo series for quantifying forest residues in the southwestern region: data compiled from Black Hills
C     Ponderosa Pine and Spruce Type, 1990; GTR-PNW-105, 1980; GTR-PNW-52, 1976; GTR-PSW-56, 1981. Albuquerque, NM:
C     U.S. Department of Agriculture, Forest Service, Southwestern Region. 227 p.
C
C     Also published by the National Wildfire Coordinating Group as PMS 822 / NFES 1395
C
C     13 – Maxwell, W.G. and F.R. Ward. 1976. Photo series for quantifying forest residues in the coastal Douglas-fir-hemlock
C     type, coastal Douglas-fir-hardwood type. Gen. Tech. Rep. PNW-51. Portland, OR: U.S. Department of Agriculture, Forest
C     Service, Pacific Northwest Forest and Range Experiment Station. 73 p.
C
C     Also published by the National Wildfire Coordinating Group as PMS 819 / NFES 1870
C
C     14 – Ottmar, R.D., R.E. Vihnanek, and C.S. Wright. 1998. Stereo photo series for quantifying natural fuels.
C     Volume I: mixed-conifer with mortality, western juniper, sagebrush, and grassland types in the interior Pacific
C     Northwest. PMS 830. Boise, ID: National Wildfire Coordinating Group, National Interagency Fire Center. 73 pp.
C
C     (same as 15)
C
C     15 – Ottmar, R.D., R.E. Vihnanek, and C.S. Wright. 1998. Stereo photo series for quantifying natural fuels.
C     Volume I: mixed-conifer with mortality, western juniper, sagebrush, and grassland types in the interior Pacific
C     Northwest. PMS 830. Boise, ID: National Wildfire Coordinating Group, National Interagency Fire Center. 73 pp.
C
C     16 – Ottmar, R.D. and R.E. Vihnanek. 1998. Stereo photo series for quantifying natural fuels. Volume II: black spruce
C     and white spruce types in Alaska. PMS 831. Boise, ID: National Wildfire Coordinating Group, National Interagency Fire
C     Center. 65 pp.
C
C      and
C
C     Ottmar, R.D. and R.E. Vihnanek. 2002. Stereo photo series for quantifying natural fuels. Volume IIa: hardwoods
C     with spruce in Alaska. PMS 836. Boise, ID: National Wildfire Coordinating Group, National Interagency Fire Center. 41 pp.
C
C     17 – Ottmar, R.D., R.E. Vihnanek, and C.S. Wright. 2000. Stereo photo series for quantifying natural fuels.
C     Volume III: Lodgepole pine, quaking aspen, and gambel oak types in the Rocky Mountains. PMS 832. Boise, ID:
C     National Wildfire Coordinating Group, National Interagency Fire Center. 85 pp.
C
C     18 – Ottmar, R.D. and R.E. Vihnanek. 1999. Stero photo series for quantifying natural fuels.  Volume V: midwest
C     red and white pine, northern tallgrass prairie, and mixed oak types in the Central and Lake States. PMS 834.
C     Boise, ID: National Wildfire Coordinating Group, National Interagency Fire Center.  99 p.
C
C      and
C
C     Ottmar, R.D., R.E. Vihnanek, and C.S. Wright. 2002. Stero photo series for quantifying natural fuels.  Volume Va: jack pine
C     in the Lake States.  PMS 837.  Boise, ID: National Wildfire Coordinating Group, National Interagency Fire Center.  49 p.
C
C     19 – Ottmar, R.D. and R.E. Vihnanek. 2000. Stereo photo series for quantifying natural fuels. Volume VI: longleaf pine,
C     pocosin, and marshgrass types in the Southeast United States. PMS 835. Boise, ID: National Wildfire Coordinating Group,
C     National Interagency Fire Center. 56 p.
C
C      and
C
C     Ottmar, R.D., R.E. Vihnanek, and J.W. Mathey. 2003. Stereo photo series for quantifying natural fuels. Volume VIa:
C     sandhill, sand pine scrub, and hardwoods with white pine types in the Southeast United States. PMS 838. Boise, ID:
C     National Wildfire Coordinating Group, National Interagency Fire Center. 78 p.
C
C     20 – Maxwell, W.G. 1990. Photo series for quantifying forest residues in the black hills, ponderosa pine type,
C     spruce type. A-89-1-90. U.S. Department of Agriculture, Forest Service, Rocky Mountain Region. 80 p.
C
C     21 – Ottmar, R.D., R.E. Vihnanek, and J.C. Regelbrugge. 2000. Stereo photo series for quantifying natural fuels.
C     Volume IV: pinyon-juniper, sagebrush, and chaparral types in the Southwestern United States. PMS 833. Boise, ID:
C     National Wildfire Coordinating Group, National Interagency Fire Center. 97 pp.
C
C     22 – Wright, Clinton S., R.D. Ottmar, R.E. Vihnanek, and D.R. Weise. 2002. Stereo photo series for quantifying natural
C     fuels: grassland, shrubland, woodland, and forest types in Hawaii. Gen. Tech. Rep. PNW-GTR-545. Portland, OR: U.S.
C     Department of Agriculture, Forest Service, Pacific Northwest Research Station. 91 p.
C
C     23 – Ottmar, R.D., C.C. Hardy, and R.E. Vihnanek. 1990. Stereo photo series for quantifying forest residues in the
C     Douglas-fir-hemlock type of the Willamette National Forest. Gen. Tech. Rep. PNW-GTR-258. Portland, OR: U.S. Department
C     of Agriculture, Forest Service, Pacific Northwest Research Station. 63 p.
C
C     24 – Lynch, C.M. and L.J. Horton. 1983. Photo series for quantifying forest residues in loblolly pine, Eastern white
C     pine, pitch pine, Virginia pine. NA-FR-25. Radnor, PA: U.S. Department of Agriculture, Forest Service, Northeastern
C     Area, State and Private Forestry. 69 p.
C
C     25 – Wilcox, F., J. McCarty, and B. Bungard. 1982. Photo series for quantifying forest residues in the northern hardwood
C     type, oak-hickory type. NA-FR-22. Broomall, PA: U.S. Department of Agriculture, Forest Service, Northeastern Area, State
C     and Private Forestry, and Pennsylvania Department of Environmental Resources, Bureau of Forestry. 43 p.
C
C     26 – Scholl, E.R. and T.A. Waldrop. 1999. Photos for estimating fuel loadings before and after prescribed burning in the
C     upper coastal plain of the southeast. Gen. Tech. Rep. SRS-26. Asheville, NC: U.S. Department of Agriculture, Forest
C     Service, Southern Research Station. 25 p.
C
C     27 – Ottmar, R.D., R.E. Vihnanek, C.S. Wright, and D.L. Olsen. 2004. Stero photo series for quantifying natural fuels.
C     Volume VII: Oregon white oak, California deciduous oak, and mixed-conifer with shrub types in the Western United
C     States.  PMS 839.  Boise, ID: National Wildfire Coordinating Group, National Interagency Fire Center.  75 p.
C
C     28 – Maxwell, W.G. and F.R. Ward.  1979. Photo series for quantifying forest residues in the sierra mixed conifer type,
C     sierra true fir type. Gen. Tech. Rep. PNW-95.  Portland, OR: U.S. Department of Agriculture, Forest Service, Pacific
C     Northwest Forest and Range Experiment Station. 79 p.
C
C     29 – Sanders, B.M. and D.H. Van Lear.  1988. Photos for estimating residue loadings before and after burning in Southern
C     Appalachian mixed pine-hardwood clearcuts. Gen. Tech. Rep. SE-49.  Asheville, NC: U.S. Department of Agriculture, Forest
C     Service, Southeastern Forest Experiment Station. 21 p.
C
C     30 – Wade, D.D., J.K. Forbus, and J.M. Saveland.  1993. Photo series for estimating post-hurricane residues and fire
C     behavior in southern pine. Gen. Tech. Rep. SE-82.  Asheville, NC: U.S. Department of Agriculture, Forest Service,
C     Southeastern Forest Experiment Station. 19 p.
C
C     31 – Blank, R.W.  1982. Stereo photos for evaluating jack pine slash fuels.  Gen. Tech. Rep. NC-77.  St. Paul, MN:
C     U.S. Department of Agriculture, Forest Service, North Central Forest Experiment Station. 23 p.
C
C     32 – Popp, J.B. and J.E. Lundquist.  2006. Photos series for quantifying forest residues in managed lands of the
C     Medicine Bow National Forest.  Gen. Tech. Rep. RMRS-GTR-172.  Fort Collins, CO: U.S. Department of Agriculture,
C     Forest Service, Rocky Mountain Research Station. 105 p.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
COMMONS
C----------
      INTEGER FOTOCODE, FOTOREF, I, J
      REAL FOTOVAL(MXFLCL),FOTOVALS(9)
      LOGICAL DEBUG
      REAL REF1VLT(MXFLCL,22), REF1VLH(MXFLCL,22),
     & REF2VLH(MXFLCL,59), REF3VLT(MXFLCL,66), REF3VLH(MXFLCL,66),
     & REF5VLT(MXFLCL,17), REF5VLH(MXFLCL,17), REF6VLT(MXFLCL,27),
     & REF6VLH(MXFLCL,27), REF7VLT(MXFLCL,56), REF7VLH(MXFLCL,56),
     & REF8VLT(MXFLCL,86), REF8VLH(MXFLCL,86), REF9VLT(MXFLCL,26),
     & REF9VLH(MXFLCL,26), REF11VLT(MXFLCL,26), REF11VLH(MXFLCL,26),
     & REF12VLT(MXFLCL,90), REF12VLH(MXFLCL,90), REF13VLH(MXFLCL,42),
     & REF14VLH(MXFLCL,29), REF15VLH(MXFLCL,29), REF16VLH(MXFLCL,41),
     & REF17VLH(MXFLCL,35), REF18VLH(MXFLCL,43), REF19VLH(MXFLCL,34),
     & REF20VLH(MXFLCL,26), REF21VLH(MXFLCL,25), REF22VLH(MXFLCL,36),
     & REF23VLT(MXFLCL,26), REF23VLH(MXFLCL,26), REF24VLH(MXFLCL,27),
     & REF25VLT(MXFLCL,14), REF25VLH(MXFLCL,14), REF26VLH(MXFLCL,16),
     & REF27VLH(MXFLCL,30), REF28VLT(MXFLCL,30), REF28VLH(MXFLCL,30),
     & REF29VLH(MXFLCL,16), REF30VLH(MXFLCL,16), REF31VLH(MXFLCL,10),
     & REF32VLH(MXFLCL,39)

      REAL REF1VLS(9,22), REF2VLS(9,59), REF3VLS(9,66),
     &   REF5VLS(9,17), REF6VLS(9,27), REF7VLS(9,56),
     &   REF8VLS(9,86), REF9VLS(9,26), REF11VLS(9,26),
     &   REF12VLS(9,90), REF13VLS(9,42), REF14VLS(9,29),
     &   REF15VLS(9,29), REF16VLS(9,41), REF17VLS(9,35),
     &   REF18VLS(9,43), REF19VLS(9,34), REF20VLS(9,26),
     &   REF21VLS(9,25), REF22VLS(9,36), REF23VLS(9,26),
     &   REF24VLS(9,27), REF25VLS(9,14), REF26VLS(9,16),
     &   REF27VLS(9,30), REF28VLS(9,30), REF29VLS(9,16),
     &   REF30VLS(9,16), REF31VLS(9,10), REF32VLS(9,39)

      REAL PROPROT1(22), PROPROT3(66), PROPROT5(17), PROPROT6(27)
      REAL PROPROT7(56), PROPROT8(86),  PROPROT9(26), PROPROT11(26)
      REAL PROPROT12(90), PROPROT23(26), PROPROT25(14), PROPROT28(30)

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF1VLT / 0.9, 1.7, 2.1, 3.3, 6.5,  1.8,    0,0,0,-1.0, 37.2,   !16
     &               0.6, 1.3, 3.3, 3.6, 4.8,  6.9,    0,0,0,-1.0, 42.9,   !15
     &               0.4, 1.9, 0.7, 3.6, 9.8,  5.9,    0,0,0,-1.0, 22.8,   !63
     &               0.5, 1.9, 3.6, 7.2, 10.8, 0.0,    0,0,0,-1.0, 48.6,   !65
     &               0.7, 2.9, 2.4, 5.5, 12.8, 2.6,    0,0,0,-1.0, 48.6,   !67
     &               1.1, 2.2, 5.5, 0.6, 9.7,  13.8, 2.6,0,0,-1.0, 74.8,   !25
     &               0.6, 1.6, 1.2, 3.5, 9.7,  21.4,   0,0,0,-1.0, 74.8,   !66
     &               0.4, 1.1, 0.5, 1.8, 4.7,  0.4,    0,0,0,-1.0, 56.0,   !9A
     &               0.6, 1.2, 1.3, 4.0, 3.0,  0.8,    0,0,0,-1.0, 24.0,   !4A
     &               0.2, 1.1, 2.3, 2.1, 3.5,  2.3,    0,0,0,-1.0, 58.0,   !17A
     &               0.4, 1.1, 1.5, 1.8, 5.0,  3.5,    0,0,0,-1.0, 41.5,   !3A
     &               0.4, 2.0, 0.9, 3.3, 5.4,  2.7,    0,0,0,-1.0, 65.5,   !13A
     &               0.4, 1.7, 1.1, 2.2, 3.6,  3.4,  4.6,0,0,-1.0, 44.3,   !6A
     &               0.6, 1.8, 2.1, 5.6, 2.9,  0.4,  4.3,0,0,-1.0, 27.2,   !18A
     &               0.4, 1.5, 1.4, 1.4, 7.7,  3.7,  1.9,0,0,-1.0, 57.2,   !10A
     &               0.4, 1.7, 3.2, 1.7, 8.8,  3.7,    0,0,0,-1.0, 50.0,   !7A
     &               0.3, 1.2, 1.6, 5.3, 10.0, 2.0,    0,0,0,-1.0, 18.0,   !19A
     &               0.4, 1.2, 1.1, 1.1, 8.7,  6.9,  1.4,0,0,-1.0, 28.8,   !11A
     &               0.6, 1.2, 0.8, 4.7, 15.7, 0.7,    0,0,0,-1.0, 41.2,   !5A
     &               0.7, 2.1, 3.1, 5.4, 5.4,  6.3,  4.1,0,0,-1.0, 42.9,   !8A
     &               0.6, 2.2, 2.5, 9.3, 21.7, 2.6,    0,0,0,-1.0, 99.1,   !16A
     &               0.2, 1.1, 1.0, 0.4, 1.9,  2.6, 50.4,0,0,-1.0, 33.6/  !15A
C
      DATA PROPROT1 /  0.38, 0.81, 0.31, 0.27, 0.73,
     &                 0.99, 0.50, 0.56, 0.59, 0.28,
     &                 0.36, 0.67, 0.86, 0.05, 0.46,
     &                 0.45, 0.21, 0.48, 0.35, 0.61,
     &                 0.40, 0.40 /

      DO J = 1, 22
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF1VLH(I,J) = REF1VLT(I,J)
            REF1VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF1VLS(I,J) = REF1VLT(I,J) * PROPROT1(J)
            REF1VLH(I,J) = MAX(REF1VLT(I,J) - REF1VLS(I,J),0.0)
          ELSE
            REF1VLH(I,J) = REF1VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF2VLH / 0.0,0.6,0.4, 0.1, 0.0,  0.0, 0,  0,0,-1.0,  18.0,  ! 24
     &               0.3,0.6,1.1, 0.6, 0.4,  0.0, 0,  0,0,-1.0,  8.4 ,  ! 18
     &               0.2,0.3,0.9, 1.0, 0.4,  0.7, 0,  0,0,-1.0,  7.2 ,  ! 23
     &               0.3,1.0,0.5, 0.1, 0.1,  0.3, 0,  0,0,-1.0,  20.4,  ! 33A
     &               0.2,0.6,1.9, 0.1, 0.5,  0.2, 0,  0,0,-1.0,  13.2,  ! 30A
     &               0.5,1.9,4.5, 0.5, 1.2,  0.0, 0,  0,0,-1.0,  15.7,  ! 32A
     &               0.1,1.1,2.7, 4.0, 2.2,  0.0, 0,  0,0,-1.0,  32.4,  ! 17
     &               0.2,0.4,0.7, 0.2, 0.8,  0.9, 0,  0,0,-1.0,  11.4,  ! 31A
     &               0.2,1.3,3.2, 2.4, 1.0,  1.4, 0,  0,0,-1.0,  5.7 ,  ! 29A
     &               0.1,2.2,4.4, 2.4, 1.2,  0.8, 0,  0,0,-1.0,  30.0,  ! 72
     &               0.1,0.9,0.8, 0.2, 0.0,  0.0, 0,  0,0,-1.0,  14.3,  ! 76
     &               0.3,0.8,0.3, 0.2, 0.2,  0.3, 0,  0,0,-1.0,  18.0,  ! 69
     &               0.3,0.8,0.7, 0.2, 0.3,  0.0, 0,  0,0,-1.0,  13.2,  ! 80
     &               0.4,1.4,1.1, 0.3, 0.1,  0.1, 0,  0,0,-1.0,  14.4,  ! 70
     &               0.2,0.7,0.8, 1.0, 1.3,  0.0, 0,  0,0,-1.0,  19.2,  ! 64
     &               0.2,0.7,0.5, 2.3, 3.9,  0.5, 0,  0,0,-1.0,  25.7,  ! 79
     &               0.5,1.4,4.6, 2.0, 0.4,  0.4, 0,  0,0,-1.0,  14.4,  ! 75
     &               0.5,1.0,1.1, 0.6, 1.9,  0.4, 0,  0,0,-1.0,  14.4,  ! 73
     &               0.1,0.5,0.8, 0.0, 0.7,  1.3, 0,  0,0,-1.0,  20.4,  ! 77
     &               0.7,2.3,2.0, 4.3, 8.3,  1.0, 0,  0,0,-1.0,  11.4,  ! 84
     &               0.9,1.7,3.6, 2.7, 3.3,  1.0, 2.5,0,0,-1.0,  27.2,  ! 74
     &               0.2,1.1,1.7, 1.8, 3.7,  13.4,0,  0,0,-1.0,  27.2,  ! 78
     &               0.5,0.7,0.2, 0.0, 0.0,  0.0, 0,  0,0,-1.0,  21.6,  ! 68
     &               0.5,1.2,1.2, 1.5, 0.3,  0.0, 0,  0,0,-1.0,  19.2,  ! 31
     &               0.5,1.5,4.2, 0.6, 0.5,  0.0, 0,  0,0,-1.0,  33.6,  ! 36A
     &               0.5,1.9,2.1, 0.3, 0.0,  0.0, 0,  0,0,-1.0,  32.9,  ! 71
     &               0.7,1.3,3.1, 1.7, 1.4,  0.0, 0,  0,0,-1.0,  34.3,  ! 14
     &               0.2,1.5,4.0, 3.9, 1.9,  0.0, 0,  0,0,-1.0,  31.5,  ! 88
     &               0.3,1.5,3.5, 6.2, 3.7,  1.4, 0,  0,0,-1.0,  33.7,  ! 13
     &               0.1,1.0,4.0, 4.2, 6.4,  6.5, 0,  0,0,-1.0,  7.1 ,  ! 9
     &               0.6,5.0,2.2, 6.0, 8.2,  0.0, 0,  0,0,-1.0,  22.9,  ! 89
     &               0.5,2.1,4.4, 0.3, 0.6,  2.5, 0,  0,0,-1.0,  45.8,  ! 5
     &               0.3,1.7,1.9, 3.1, 8.0,  0.6, 0,  0,0,-1.0,  24.4,  ! 29
     &               0.3,0.7,0.8, 0.3, 1.5,  2.7, 0.6,0,0,-1.0,  21.5,  ! 30
     &               0.4,2.7,3.5, 5.6, 2.6,  2.8, 0,  0,0,-1.0,  35.5,  ! 56
     &               0.5,2.9,12.9,8.5, 2.1,  2.8, 0,  0,0,-1.0,  35.8,  ! 33
     &               0.7,2.9,6.7, 23.0,4.7,  0.0, 0,  0,0,-1.0,  19.1,  ! 91
     &               0.5,2.1,6.5, 18.2,3.9,  0.0,28.1,0,0,-1.0,  39.3,  ! 7
     &               1.5,4.4,10.3,20.0,26.0, 0.0, 0,  0,0,-1.0,  26.2,  ! 32
     &               1.5,3.1,11.0,31.7,18.8, 0.4, 0,  0,0,-1.0,  65.8,  ! 8
     &               0.3,1.5,4.3, 9.6, 13.7, 1.3,28.9,0,0,-1.0,  38.6,  ! 28
     &               0.4,1.2,0.8, 0.5, 0.0,  0.0, 0,  0,0,-1.0,  16.8,  ! 43
     &               0.9,1.8,1.2, 0.1, 0.1,  0.0, 0,  0,0,-1.0,  24.0,  ! 14A
     &               0.4,0.7,1.3, 0.0, 0.1,  0.0, 0,  0,0,-1.0,  16.8,  ! 39A
     &               0.5,1.5,0.8, 0.5, 1.6,  0.8, 0,  0,0,-1.0,  10.8,  ! 41A
     &               0.4,0.9,1.2, 0.2, 0.5,  0.9, 0,  0,0,-1.0,  12.0,  ! 49
     &               0.4,1.1,3.6, 1.2, 0.5,  0.2, 0,  0,0,-1.0,  28.8,  ! 28A
     &               0.2,0.8,1.2, 0.2, 0.4,  0.1, 0,  0,0,-1.0,  22.8,  ! 27A
     &               0.3,1.1,0.4, 0.2, 1.2,  0.5, 0,  0,0,-1.0,  32.4,  ! 12A
     &               0.3,1.9,2.9, 1.2, 1.7,  1.4, 0,  0,0,-1.0,  15.7,  ! 37A
     &               0.5,1.0,0.8, 0.1, 0.5,  0.2, 0,  0,0,-1.0,  25.2,  ! 42A
     &               0.4,2.4,5.0, 0.9, 0.8,  0.4, 0,  0,0,-1.0,  28.6,  ! 38A
     &               0.8,1.8,3.7, 0.0, 0.1,  0.0, 0,  0,0,-1.0,  34.3,  ! 86
     &               0.5,2.7,8.7, 1.4, 0.7,  0.0, 0,  0,0,-1.0,  50.5,  ! 43A
     &               0.3,2.3,2.5, 6.8, 7.5,  0.0, 0,  0,0,-1.0,  5.6 ,  ! 34
     &               0.4,1.9,6.0, 2.8, 1.5,  2.1, 0,  0,0,-1.0,  56.1,  ! 40A
     &               0.4,1.9,2.5, 1.3, 3.3,  3.6,9.9, 0,0,-1.0,  15.7,  ! 42
     &               0.2,1.1,1.9, 0.3, 1.8,  1.4,0.4, 0,0,-1.0,  17.2,  ! 48
     &               0.5,1.8,5.0, 2.5, 6.6,  1.9, 0,  0,0,-1.0,  41.1/  ! 95

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF2VLS / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,  0,0,  ! 24
     &                0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,  0,0,  ! 18
     &                0.0, 0.0, 0.0, 0.1, 0.1, 0.1, 0,  0,0,  ! 23
     &                0.0, 0.0, 0.0, 0.4, 0.7, 2.2, 0,  0,0,  ! 33A
     &                0.0, 0.0, 0.0, 0.6, 2.0, 0.6, 0,  0,0,  ! 30A
     &                0.0, 0.0, 0.0, 0.5, 1.3, 0.0, 0,  0,0,  ! 32A
     &                0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0,  0,0,  ! 17
     &                0.0, 0.0, 0.0, 0.8, 3.2, 3.5, 0,  0,0,  ! 31A
     &                0.0, 0.0, 0.0, 1.0, 0.4, 0.6, 0,  0,0,  ! 29A
     &                0.0, 0.0, 0.0, 2.9, 1.5, 0.9, 0,  0,0,  ! 72
     &                0.0, 0.0, 0.0, 0.4, 0.1, 0.0, 0,  0,0,  ! 76
     &                0.0, 0.0, 0.0, 0.1, 0.1, 0.2, 0,  0,0,  ! 69
     &                0.0, 0.0, 0.0, 0.6, 0.8, 0.0, 0,  0,0,  ! 80
     &                0.0, 0.0, 0.0, 2.9, 0.9, 1.0, 0,  0,0,  ! 70
     &                0.0, 0.0, 0.0, 2.4, 3.0, 0.0, 0,  0,0,  ! 64
     &                0.0, 0.0, 0.0, 0.8, 1.4, 0.2, 0,  0,0,  ! 79
     &                0.0, 0.0, 0.0, 1.1, 0.2, 0.2, 0,  0,0,  ! 75
     &                0.0, 0.0, 0.0, 1.2, 4.0, 0.8, 0,  0,0,  ! 73
     &                0.0, 0.0, 0.0, 0.1, 3.0, 5.7, 0,  0,0,  ! 77
     &                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,  0,0,  ! 84
     &                0.0, 0.0, 0.0, 1.3, 1.6, 0.5, 1.2,0,0,  ! 74
     &                0.0, 0.0, 0.0, 1.5, 3.2,11.4, 0,  0,0,  ! 78
     &                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,  0,0,  ! 68
     &                0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,  0,0,  ! 31
     &                0.0, 0.0, 0.0, 1.7, 1.4, 0.0, 0,  0,0,  ! 36A
     &                0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 0,  0,0,  ! 71
     &                0.0, 0.0, 0.0, 2.4, 2.0, 0.0, 0,  0,0,  ! 14
     &                0.0, 0.0, 0.0, 1.3, 0.6, 0.0, 0,  0,0,  ! 88
     &                0.0, 0.0, 0.0, 3.1, 1.8, 0.7, 0,  0,0,  ! 13
     &                0.0, 0.0, 0.0, 0.3, 0.5, 0.5, 0,  0,0,  ! 9
     &                0.0, 0.0, 0.0, 0.8, 1.1, 0.0, 0,  0,0,  ! 89
     &                0.0, 0.0, 0.0, 1.2, 2.6,10.5, 0,  0,0,  ! 5
     &                0.0, 0.0, 0.0, 2.8, 7.1, 0.5, 0,  0,0,  ! 29
     &                0.0, 0.0, 0.0, 1.3, 5.9,10.8, 2.5,0,0,  ! 30
     &                0.0, 0.0, 0.0, 6.1, 2.8, 3.1, 0,  0,0,  ! 56
     &                0.0, 0.0, 0.0, 4.6, 1.1, 1.5, 0,  0,0,  ! 33
     &                0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,  0,0,  ! 91
     &                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,  0,0,  ! 7
     &                0.0, 0.0, 0.0, 0.8, 1.1, 0.0, 0,  0,0,  ! 32
     &                0.0, 0.0, 0.0, 3.1, 1.9, 0.0, 0,  0,0,  ! 8
     &                0.0, 0.0, 0.0, 2.6, 3.7, 0.3, 7.7,0,0,  ! 28
     &                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,  0,0,  ! 43
     &                0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,  0,0,  ! 14A
     &                0.0, 0.0, 0.0, 1.4, 2.5, 0.0, 0,  0,0,  ! 39A
     &                0.0, 0.0, 0.0, 0.2, 0.5, 0.3, 0,  0,0,  ! 41A
     &                0.0, 0.0, 0.0, 0.4, 1.2, 2.1, 0,  0,0,  ! 49
     &                0.0, 0.0, 0.0, 0.8, 0.4, 0.1, 0,  0,0,  ! 28A
     &                0.0, 0.0, 0.0, 1.7, 5.1, 1.3, 0,  0,0,  ! 27A
     &                0.0, 0.0, 0.0, 0.9, 5.9, 2.3, 0,  0,0,  ! 12A
     &                0.0, 0.0, 0.0, 1.1, 1.5, 1.2, 0,  0,0,  ! 37A
     &                0.0, 0.0, 0.0, 1.6, 6.3, 2.3, 0,  0,0,  ! 42A
     &                0.0, 0.0, 0.0, 2.5, 2.2, 1.2, 0,  0,0,  ! 38A
     &                0.0, 0.0, 0.0, 0.4, 7.8, 1.6, 0,  0,0,  ! 86
     &                0.0, 0.0, 0.0, 2.2, 1.1, 0.0, 0,  0,0,  ! 43A
     &                0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,  0,0,  ! 34
     &                0.0, 0.0, 0.0, 4.9, 2.5, 3.5, 0,  0,0,  ! 40A
     &                0.0, 0.0, 0.0, 0.3, 0.7, 0.7, 2.0,0,0,  ! 42
     &                0.0, 0.0, 0.0, 3.0,16.3,12.5, 3.2,0,0,  ! 48
     &                0.0, 0.0, 0.0, 7.8,20.9, 5.9, 0,  0,0/  ! 95

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF3VLT /  0.5,0.7, 0.9, 0.8,  0.1,  0.5,    0,0,0,-1., 15.6,  !2
     &                0.2,0.4, 0.7, 1.9,  0.8,  0.0,    0,0,0,-1., 10.0,  !26A
     &                0.6,0.9, 3.8, 1.4,  0.4,  0.0,    0,0,0,-1., 16.8,  !1
     &                0.3,1.2, 5.1, 4.3,  1.1,  0.0,    0,0,0,-1., 34.3,  !35A
     &                0.3,0.3, 1.5, 1.7,  7.6,  1.3,    0,0,0,-1., 21.5,  !82
     &                0.5,1.9, 4.3, 0.2,  2.4,  3.4,    0,0,0,-1., 41.1,  !85
     &                0.2,0.8, 3.7, 6.7,  1.4,  0.0,    0,0,0,-1., 12.0,  !25A
     &                0.4,1.9, 8.5, 2.1,  0.2,  0.0,    0,0,0,-1., 27.2,  !34A
     &                0.2,0.8, 1.7, 1.2,  7.0,  2.3,    0,0,0,-1., 21.5,  !45A
     &                0.1,1.0, 3.3, 3.3,  6.2,  3.2,    0,0,0,-1., 35.8,  !47A
     &                0.4,2.1, 2.6, 4.7,  7.7,  1.0,    0,0,0,-1., 30.0,  !87
     &                0.5,1.7, 1.9, 10.0, 3.4,  1.7,    0,0,0,-1., 20.4,  !83
     &                0.6,1.9, 6.2, 6.6,  3.0,  1.2,    0,0,0,-1., 18.0,  !92
     &                0.3,1.4, 4.5, 8.0,  6.3,  0.0,    0,0,0,-1., 27.2,  !53
     &                0.2,1.1, 2.9, 8.3,  7.7,  1.7,    0,0,0,-1., 20.0,  !41
     &                0.3,1.4, 8.1, 11.8, 1.0,  0.0,    0,0,0,-1., 43.0,  !49A
     &                0.3,1.2, 5.3, 10.8, 6.3,  0.0,    0,0,0,-1., 24.0,  !98
     &                0.5,1.7, 1.3, 5.2,  12.3, 3.3,    0,0,0,-1., 40.0,  !61
     &                0.3,0.8, 0.4, 7.5,  13.6, 5.2,    0,0,0,-1., 30.0,  !60
     &                1.2,3.1, 3.1, 0.3,  9.9,  11.7,   0,0,0,-1., 39.3,  !6
     &                0.3,1.4, 2.1, 10.8, 14.8, 0.8,    0,0,0,-1., 22.4,  !55
     &                0.2,1.0, 1.2, 4.2,  15.9, 9.2,    0,0,0,-1., 41.1,  !48A
     &                0.5,1.5, 5.0, 7.0,  6.9,  11.1,   0,0,0,-1., 22.4,  !11
     &                0.2,0.7, 1.2, 10.7, 17.5, 2.0,    0,0,0,-1., 35.8,  !46A
     &                0.3,1.7, 3.2, 5.0,  16.1, 7.0,    0,0,0,-1., 43.0,  !27
     &                0.3,0.8, 2.4, 2.8,  10.7, 17.7,   0,0,0,-1., 22.9,  !81
     &                0.3,0.6, 0.3, 0.1,  0.0,  0.0,    0,0,0,-1., 15.6,  !45
     &                0.4,1.9, 3.3, 0.9,  0.0,  0.0,    0,0,0,-1., 41.5,  !1A
     &                0.2,1.8, 1.6, 1.4,  1.5,  0.0,    0,0,0,-1., 46.8,  !40
     &                0.1,0.4, 1.4, 3.3,  3.0,  0.0,    0,0,0,-1., 14.4,  !39
     &                0.2,1.0, 3.3, 4.5,  0.0,  0.0,    0,0,0,-1., 10.8,  !35
     &                0.6,1.5, 2.9, 2.3,  2.0,  0.0,    0,0,0,-1., 31.5,  !2A
     &                0.8,2.0, 3.3, 3.0,  1.3,  0.8,    0,0,0,-1., 47.2,  !26
     &                0.3,1.0, 2.5, 7.7,  1.3,  0.0,    0,0,0,-1., 9.6 ,  !24A
     &                0.6,2.6, 7.6, 5.5,  1.1,  0.0,    0,0,0,-1., 29.9,  !44
     &                0.3,1.9, 4.1, 6.4,  4.8,  1.1,    0,0,0,-1., 37.4,  !37
     &                0.7,1.3, 3.1, 4.0,  4.4,  6.7,    0,0,0,-1., 28.0,  !21
     &                0.2,0.9, 1.9, 9.7,  7.5,  0.0,    0,0,0,-1., 30.0,  !21A
     &                0.5,2.2, 4.3, 3.4,  1.3,  3.8,  6.0,0,0,-1., 25.2,  !90
     &                0.7,2.7, 4.1, 6.8,  6.3,  1.2,    0,0,0,-1., 46.8,  !54
     &                0.4,2.0, 2.9, 8.1,  8.9,  0.0,    0,0,0,-1., 48.6,  !36
     &                0.5,1.8, 2.0, 5.9,  9.9,  2.4,    0,0,0,-1., 22.8,  !22A
     &                0.7,2.8, 1.5, 2.0,  11.0, 6.3,    0,0,0,-1., 67.3,  !50
     &                0.3,1.2, 2.1, 2.0,  12.6, 7.2,  1.9,0,0,-1., 22.9,  !58
     &                0.3,0.9, 2.9, 7.7,  14.9, 1.3,    0,0,0,-1., 17.2,  !19
     &                0.2,1.1, 0.6, 1.7,  14.7, 14.2,   0,0,0,-1., 74.8,  !57
     &                0.7,1.6, 1.8, 4.4,  18.8, 6.0,    0,0,0,-1., 40.0,  !96
     &                0.6,1.9, 3.2, 11.7, 16.3, 0.0,    0,0,0,-1., 58.0,  !23A
     &                0.7,2.3, 6.0, 2.6,  11.6, 15.1,   0,0,0,-1., 72.9,  !46
     &                0.9,1.8, 2.8, 8.8,  21.4, 3.8,    0,0,0,-1., 58.0,  !97
     &                0.7,1.6, 3.5, 4.6,  10.7, 18.8,   0,0,0,-1., 63.6,  !20
     &                0.4,1.4, 2.1, 9.4,  25.2, 2.3,    0,0,0,-1., 18.6,  !59
     &                0.4,1.8, 4.1, 3.0,  11.3, 22.9,   0,0,0,-1., 91.6,  !44A
     &                0.7,1.5, 2.3, 3.8,  18.8, 18.2,   0,0,0,-1., 43.0,  !93
     &                0.8,2.6, 6.8, 4.6,  9.8,  22.1,   0,0,0,-1., 48.6,  !47
     &                0.7,2.3, 1.8, 11.2, 26.8, 5.1,    0,0,0,-1., 45.8,  !4
     &                0.6,1.7, 2.4, 4.1,  28.3, 10.9,   0,0,0,-1., 31.8,  !22
     &                0.5,2.7, 4.1, 5.8,  21.5, 11.8, 2.9,0,0,-1., 59.8,  !51
     &                0.7,1.6, 1.3, 1.3,  4.5,  5.4, 35.8,0,0,-1., 76.7,  !62
     &                2.2,4.3, 14.1,16.2, 8.2,  6.0,    0,0,0,-1., 42.9,  !12
     &                0.5,1.4, 3.8, 16.7, 23.3, 5.3,    0,0,0,-1., 63.6,  !20A
     &                0.5,1.6, 2.2, 8.5,  31.8, 6.4,    0,0,0,-1., 38.6,  !94
     &                1.1,3.2, 2.2, 12.5, 30.1, 9.4,    0,0,0,-1., 38.6,  !10
     &                0.4,0.9, 0.5, 2.7,  19.1, 45.7, 3.3,0,0,-1., 84.2,  !3
     &                0.2,1.4, 5.0, 11.8, 49.5, 9.4,    0,0,0,-1., 54.2,  !38
     &                0.5,2.8, 2.6, 4.5,  22.9, 26.2, 3.3,0,0,-1., -1.0/  !52
C
      DATA PROPROT3 /  0.52, 0.23, 0.12, 0.16, 0.89,
     &                 0.98, 0.10, 0.15, 0.99, 0.90,
     &                 0.43, 0.18, 0.42, 0.18, 0.54,
     &                 0.24, 0.23, 0.40, 0.28, 0.99,
     &                 0.13, 0.66, 0.34, 0.77, 0.93,
     &                 0.31, 1.00, 0.79, 0.81, 0.42,
     &                 0.22, 0.57, 0.30, 0.08, 0.30,
     &                 0.33, 0.01, 0.41, 0.01, 0.53,
     &                 0.66, 0.37, 0.41, 0.33, 0.09,
     &                 0.25, 0.54, 0.23, 0.84, 0.72,
     &                 0.06, 0.12, 0.50, 0.42, 0.90,
     &                 0.33, 0.57, 0.33, 0.97, 0.32,
     &                 0.37, 0.55, 0.18, 0.49, 0.17,
     &                 0.17/

      DO J = 1, 66
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF3VLH(I,J) = REF3VLT(I,J)
            REF3VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF3VLS(I,J) = REF3VLT(I,J) * PROPROT3(J)
            REF3VLH(I,J) = MAX(REF3VLT(I,J) - REF3VLS(I,J),0.0)
          ELSE
            REF3VLH(I,J) = REF3VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF5VLT /  0.8,0.9,2.0, 2.5, 3.3,  0.0,    0,0,0,-1.0, -1.0,   !1WH1TH
     &                2.2,4.1,10.1,1.2, 7.2,  22.2,   0,0,0,-1.0, -1.0,   !2WH1TH
     &                2.7,3.5,8.6, 7.9, 13.6, 12.8,13.8,0,0,-1.0, -1.0,   !3WH1TH
     &                4.3,6.6,17.5,0.3, 13.5, 40.4, 4.7,0,0,-1.0, -1.0,   !4WH1TH
     &                5.2,5.1,13.2,4.1, 5.9,  1.8,  5.6,0,0,-1.0, -1.0,   !5WH1TH
     &                4.9,4.9,11.4,7.4, 11.1, 6.2,    0,0,0,-1.0, -1.0,   !6WH1TH
     &                1.6,2.4,3.2, 0.6, 0.8,  0.0,    0,0,0,-1.0, -1.0,   !1GF1TH
     &                2.2,2.6,6.9, 0.7, 0.8,  1.9,    0,0,0,-1.0, -1.0,   !2GF1TH
     &                3.8,2.7,6.0, 2.7, 3.5,  0.0,    0,0,0,-1.0, -1.0,   !3GF1TH
     &                4.3,3.9,8.4, 3.9, 8.2,  11.9,   0,0,0,-1.0, -1.0,   !4GF1TH
     &                0.8,2.2,1.8, 0.8, 2.6,  5.5,    0,0,0,-1.0, -1.0,   !1WC1TH
     &                1.3,2.5,1.5, 0.7, 0.9,  0.0,    0,0,0,-1.0, -1.0,   !2WC1TH
     &                1.5,4.3,5.7, 3.8, 11.6, 26.2,   0,0,0,-1.0, -1.0,   !3WC1TH
     &                1.3,4.6,6.9, 2.6, 3.5,  0.0,    0,0,0,-1.0, -1.0,   !4WC1TH
     &                1.9,4.7,5.4, 3.6, 5.0,  0.8,    0,0,0,-1.0, -1.0,   !5WC1TH
     &                4.7,4.1,6.8, 7.0, 10.6, 5.8,    0,0,0,-1.0, -1.0,   !6WC1TH
     &                6.0,6.4,13.9,10.0,15.3, 9.0,    0,0,0,-1.0, -1.0/   !7WC1TH

C
      DATA PROPROT5 /  1.00, 1.00, 0.97, 0.90, 0.80,
     &                 0.56, 1.00, 1.00, 0.16, 0.74,
     &                 1.00, 0.00, 0.84, 1.00, 0.54,
     &                 0.49, 0.43 /

      DO J = 1, 17
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF5VLH(I,J) = REF5VLT(I,J)
            REF5VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF5VLS(I,J) = REF5VLT(I,J) * PROPROT5(J)
            REF5VLH(I,J) = MAX(REF5VLT(I,J) - REF5VLS(I,J),0.0)
          ELSE
            REF5VLH(I,J) = REF5VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF6VLT / 0.0,3.9,6.7,  2.6,  3.5,  2.3,  3.3,0,0,-1., 15.0 , ! 1PP4CC
     &               0.0,8.1,8.1,  3.2,  7.7,  12.6, 6.6,0,0,-1., 13.1 , ! 2PP4CC
     &               0.0,0.4,1.8,  0.2,  0.3,  0.0,    0,0,0,-1., 13.1 , ! 1PP4PC
     &               0.0,2.2,3.4,  1.4,  1.5,  0.0,    0,0,0,-1., 15.0 , ! 2PP4PC
     &               0.0,2.7,5.0,  2.8,  2.9,  0.0,    0,0,0,-1., 11.2 , ! 3PP4PC
     &               0.0,2.3,4.8,  2.1,  5.7,  4.7,  2.5,0,0,-1., 18.7 , ! 4PP4PC
     &               0.0,3.8,7.4,  3.5,  4.8,  3.4,  6.6,0,0,-1., 22.4 , ! 5PP4PC
     &               0.0,3.4,2.3,  0.9,  1.0,  0.0,    0,0,0,-1., 20.6 , ! 1PP1TH
     &               0.0,2.7,5.5,  1.1,  1.2,  0.0,    0,0,0,-1., 20.6 , ! 2PP1TH
     &               0.0,3.5,3.2,  2.7,  2.7,  0.0,    0,0,0,-1., 24.3 , ! 3PP1TH
     &               0.0,2.1,4.7,  2.7,  2.8,  0.0,    0,0,0,-1., 26.2 , ! 4PP1TH
     &               0.0,5.2,13.0, 1.9,  2.0,  0.0,    0,0,0,-1., 13.0 , ! 5PP1TH
     &               0.0,5.5,6.7,  6.4,  6.4,  0.0,  3.5,0,0,-1., 35.5 , ! 6PP1TH
     &               0.0,1.4,3.7,  1.7,  1.8,  0.0,    0,0,0,-1., 13.0 , ! 1PP&ASSOC4PC
     &               0.0,0.4,2.3,  1.3,  2.5,  3.1,  8.4,0,0,-1., 0.4  , ! 2PP&ASSOC4PC
     &               0.0,3.2,4.8,  6.1,  7.7,  3.9,    0,0,0,-1., 9.4  , ! 3PP&ASSOC4PC
     &               0.0,3.0,5.4,  4.5,  4.6,  0.0, 11.7,0,0,-1., 37.4 , ! 4PP&ASSOC4PC
     &               0.0,2.4,5.1,  5.1,  7.4,  4.3,  3.0,0,0,-1., 13.0 , ! 5PP&ASSOC4PC
     &               0.0,3.9,4.8,  5.0,  6.5,  4.0,  6.9,0,0,-1., 13.0 , ! 6PP&ASSOC4PC
     &               0.0,2.6,5.9,  13.5, 14.2, 1.7,    0,0,0,-1., 31.8 , ! 7PP&ASSOC4PC
     &               0.0,2.1,6.3,  5.1,  7.4,  8.6, 11.6,0,0,-1., 1.9  , ! 8PP&ASSOC4PC
     &               0.0,1.3,5.5,  4.0,  4.4,  0.8,    0,0,0,-1., 7.5  , ! 1LP3CC
     &               0.0,0.8,1.7,  0.5,  0.4,  0.0,    0,0,0,-1., 1.9  , ! 1LP3PC
     &               0.0,1.9,5.1,  1.7,  2.8,  3.0,    0,0,0,-1., 7.5  , ! 2LP3PC
     &               0.0,1.7,4.1,  3.2,  4.7,  3.8,    0,0,0,-1., 9.4  , ! 3LP3PC
     &               0.0,2.5,6.6,  7.9,  8.2,  0.9,    0,0,0,-1., 13.0 , ! 4LP3PC
     &               0.0,2.4,6.9,  10.3, 12.0, 4.2,    0,0,0,-1., 16.8 / ! 5LP3PC
C
      DATA PROPROT6 /  0.34, 0.03, 0.00, 0.00, 0.00,
     &                 0.00, 0.11, 0.00, 0.61, 0.02,
     &                 0.05, 0.02, 0.01, 0.27, 0.01,
     &                 0.03, 0.07, 0.16, 0.02, 0.07,
     &                 0.16, 0.00, 0.00, 0.00, 0.00,
     &                 0.00, 0.14 /

      DO J = 1, 27
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF6VLH(I,J) = REF6VLT(I,J)
            REF6VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF6VLS(I,J) = REF6VLT(I,J) * PROPROT6(J)
            REF6VLH(I,J) = MAX(REF6VLT(I,J) - REF6VLS(I,J),0.0)
          ELSE
            REF6VLH(I,J) = REF6VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF7VLT / 0.1, 1.5, 0.8,  0.7, 0.8, 0.0,    0,0,0,-1., 16.8 , ! 1MP4
     &               0.4, 2.1, 1.1,  1.6, 2.2, 1.4,    0,0,0,-1., 14.3 , ! 2MP4
     &               0.4, 1.6, 1.2,  0.8, 2.4, 4.3,  1.9,0,0,-1., 18.7 , ! 3MP4
     &               0.6, 1.4, 1.2,  0.8, 3.7, 7.6, 12.9,0,0,-1., 15.0 , ! 4MP4
     &               1.0, 2.1, 4.8,  2.8, 8.0, 14.0,13.9,0,0,-1., 44.9 , ! 5MP4
     &               1.0, 2.2, 3.6,  1.2, 1.4, 0.7,    0,0,0,-1., 48.6 , ! 1MF4
     &               1.3, 2.9, 5.6,  1.7, 2.2, 1.2,  1.5,0,0,-1., 74.8 , ! 2MF4
     &               0.8, 1.6, 3.4,  1.1, 3.5, 6.4,  6.1,0,0,-1., 48.6 , ! 3MF4
     &               0.7, 2.1, 3.3,  1.1, 5.1, 10.5,16.4,0,0,-1., 63.6 , ! 4MF4
     &               0.7, 1.5, 2.9,  1.7, 7.9, 17.0,23.4,0,0,-1., 61.7 , ! 5MF4
     &               0.2, 0.5, 1.4,  0.4, 0.4, 0.0,    0,0,0,-1., 25.7 , ! 1PP2
     &               0.1, 0.3, 0.3,  0.2, 1.2, 2.3,    0,0,0,-1., 15.6 , ! 2PP2
     &               0.1, 0.5, 0.7,  0.4, 1.6, 2.9,    0,0,0,-1., 9.0  , ! 3PP2
     &               0.5, 1.1, 2.1,  2.7, 2.9, 0.6,    0,0,0,-1., 42.9 , ! 4PP2
     &               0.1, 0.2, 0.4,  0.5, 0.6, 0.0,    0,0,0,-1., 26.4 , ! 1PP3
     &               0.2, 0.6, 0.8,  0.1, 0.2, 1.1,    0,0,0,-1., 24.0 , ! 2PP3
     &               0.2, 0.5, 1.1,  0.7, 1.3, 1.2,    0,0,0,-1., 29.4 , ! 3PP3
     &               0.1, 0.9, 1.5,  1.2, 1.6, 0.8,  2.3,0,0,-1., 27.2 , ! 4PP3
     &               0.1, 0.7, 0.4,  0.4, 0.9, 1.0,    0,0,0,-1., 12.0 , ! 1PP4
     &               0.0, 1.0, 2.0,  0.9, 1.1, 0.7,    0,0,0,-1., 19.2 , ! 2PP4
     &               0.3, 1.4, 1.8,  0.7, 2.2, 3.6,    0,0,0,-1., 24.0 , ! 3PP4
     &               0.3, 0.6, 0.8,  0.2, 0.4, 0.2,    0,0,0,-1., 9.6  , ! 1LP2
     &               0.1, 0.7, 1.2,  0.2, 0.7, 1.3,    0,0,0,-1., 7.2  , ! 2LP2
     &               0.4, 0.4, 1.0,  2.0, 2.5, 0.9,  5.8,0,0,-1., 28.6 , ! 3LP2
     &               0.2, 2.5, 2.5,  4.1, 5.1, 2.3,    0,0,0,-1., 12.0 , ! 4LP2
     &               0.9, 2.9, 10.5, 7.7, 8.4, 2.0,    0,0,0,-1., 44.9 , ! 5LP2
     &               0.3, 0.4, 0.2,  0.7, 0.8, 0.0,    0,0,0,-1., 4.8  , ! 1LP3
     &               0.1, 0.4, 0.7,  0.8, 2.1, 3.2,    0,0,0,-1., 0.3  , ! 2LP3
     &               0.2, 1.8, 1.6,  3.1, 3.7, 1.4,    0,0,0,-1., 10.0 , ! 3LP3
     &               0.4, 1.0, 3.4,  5.6, 7.6, 5.4,    0,0,0,-1., 15.7 , ! 4LP3
     &               0.1, 0.4, 0.2,  0.8, 0.9, 0.5,  4.7,0,0,-1., 4.3  , ! 1LP4
     &               0.3, 0.7, 2.4,  2.3, 2.4, 0.2,    0,0,0,-1., 56.1 , ! 1WF2
     &               0.7, 2.1, 5.2,  1.1, 1.1, 0.0,    0,0,0,-1., 22.4 , ! 2WF2
     &               0.9, 2.2, 5.1,  3.2, 3.2, 0.0,    0,0,0,-1., 31.8 , ! 3WF2
     &               0.9, 2.5, 3.2,  6.5, 7.0, 1.0,    0,0,0,-1., 41.1 , ! 4WF2
     &               0.6, 3.2, 2.1,  1.1, 1.8, 1.9,    0,0,0,-1., 31.8 , ! 1WF3
     &               1.0, 4.5, 2.6,  3.1, 3.3, 0.3,    0,0,0,-1., 33.7 , ! 2WF3
     &               0.9, 1.8, 2.2,  5.2, 5.4, 0.8,  1.6,0,0,-1., 71.1 , ! 3WF3
     &               0.7, 1.4, 2.2,  3.4, 7.1, 9.8,  5.0,0,0,-1., 50.5 , ! 4WF3
     &               0.8, 3.5, 3.0,  6.5, 9.3, 8.0,  4.6,0,0,-1., 28.1 , ! 5WF3
     &               0.7, 1.6, 1.7,  0.3, 1.2, 2.3,  1.6,0,0,-1., 37.4 , ! 1WF4
     &               1.0, 3.7, 2.4,  3.0, 3.6, 1.2,    0,0,0,-1., 15.0 , ! 2WF4
     &               0.5, 1.2, 4.5,  3.5, 4.9, 3.6,  3.3,0,0,-1., 46.8 , ! 3WF4
     &               0.6, 2.0, 1.8,  0.1, 4.9, 12.6, 5.4,0,0,-1., 190.7, ! 4WF4
     &               1.0, 3.8, 3.6,  2.4, 8.6, 16.4, 8.6,0,0,-1., 37.4 , ! 5WF4
     &               0.4, 1.8, 1.1,  0.3, 0.6, 0.9,    0,0,0,-1., 13.9 , ! 1RF3
     &               1.1, 2.8, 0.8,  1.4, 2.0, 1.3,    0,0,0,-1., 56.1 , ! 2RF3
     &               0.5, 1.8, 2.3,  2.6, 4.7, 5.5,    0,0,0,-1., 37.4 , ! 3RF3
     &               0.9, 4.0, 7.6,  2.3, 4.8, 6.6,    0,0,0,-1., 48.6 , ! 4RF3
     &               0.7, 2.6, 5.4,  7.9, 9.8, 5.1,    0,0,0,-1., 31.8 , ! 5RF3
     &               0.9, 3.1, 3.0,  0.5, 0.7, 0.6,    0,0,0,-1., 24.3 , ! 1RF4
     &               1.2, 3.2, 4.2,  2.5, 3.2, 1.5,    0,0,0,-1., 84.2 , ! 2RF4
     &               0.7, 2.8, 5.6,  2.3, 4.4, 5.4,    0,0,0,-1., 37.4 , ! 3RF4
     &               0.2, 1.5, 3.5,  1.1, 7.5, 17.4,   0,0,0,-1., 11.2 , ! 4RF4
     &               0.3, 2.2, 3.2,  1.6, 7.6, 15.8,10.8,0,0,-1., 26.2 , ! 5RF4
     &               0.3, 0.6, 1.1,  0.5, 2.8, 5.9,  3.0,0,0,-1., 16.8 / ! 1MH4
C
      DATA PROPROT7 /  0.89, 0.60, 0.38, 0.62, 0.93,
     &                 0.85, 0.82, 0.85, 0.92, 0.92,
     &                 0.11, 0.80, 0.48, 0.71, 0.83,
     &                 0.91, 0.89, 0.24, 0.49, 0.08,
     &                 0.51, 0.84, 0.98, 0.69, 0.09,
     &                 0.21, 0.60, 0.16, 0.04, 0.25,
     &                 0.80, 0.61, 0.05, 0.30, 0.19,
     &                 0.09, 0.29, 0.29, 0.69, 0.13,
     &                 0.79, 0.55, 0.51, 0.21, 0.30,
     &                 0.14, 0.59, 0.36, 0.31, 0.28,
     &                 1.00, 0.46, 0.45, 0.24, 0.42,
     &                 0.45 /

      DO J = 1, 56
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF7VLH(I,J) = REF7VLT(I,J)
            REF7VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF7VLS(I,J) = REF7VLT(I,J) * PROPROT7(J)
            REF7VLH(I,J) = MAX(REF7VLT(I,J) - REF7VLS(I,J),0.0)
          ELSE
            REF7VLH(I,J) = REF7VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF8VLT / 0.5,1.1,0.8,  0.4,  0.5, 0.0,     0,0,0,-1., 2.4  , ! 1DFHD3
     &               0.6,3.1,1.8,  2.3,  2.3, 0.0,     0,0,0,-1., 11.2 , ! 2DFHD3
     &               1.2,3.2,1.6,  2.6,  4.0, 3.6,   5.3,0,0,-1., 4.8  , ! 3DFHD3
     &               0.6,1.9,1.9,  0.9,  1.0, 0.0,     0,0,0,-1., 7.5  , ! 1DFHD4
     &               0.8,1.1,1.1,  1.0,  1.4, 1.0,     0,0,0,-1., 3.7  , ! 2DFHD4
     &               0.9,1.8,1.7,  3.1,  3.8, 1.6,     0,0,0,-1., 3.7  , ! 3DFHD4
     &               0.5,1.6,1.7,  2.7,  3.9, 4.5,     0,0,0,-1., 5.6  , ! 4DFHD4
     &               1.0,3.2,1.9,  2.7,  4.5, 5.4,  15.5,0,0,-1., 5.6  , ! 5DFHD4
     &               0.2,1.0,0.6,  0.3,  0.3, 0.0,     0,0,0,-1., 5.6  , ! 1HD2
     &               0.2,0.8,1.3,  2.2,  2.5, 1.0,     0,0,0,-1., 1.9  , ! 2HD2
     &               1.2,2.2,5.5,  5.3,  6.9, 4.1,   4.8,0,0,-1., 5.6  , ! 1DF2
     &               0.7,1.3,4.9,  3.6,  6.5, 7.7,  31.1,0,0,-1., 7.5  , ! 2DF2
     &               1.5,2.6,2.5,  3.3,  6.6, 8.5,  34.1,0,0,-1., 7.5  , ! 1DF3
     &               1.8,8.3,20.4, 6.2,  24.7,50.1,    0,0,0,-1., 52.4 , ! 2DF3
     &               0.4,0.5,0.9,  1.1,  3.9, 7.4,     0,0,0,-1., 43.0 , ! 1DF4
     &               0.2,0.5,0.5,  1.5,  5.0, 9.5,   2.0,0,0,-1., 63.6 , ! 2DF4
     &               0.3,0.9,2.6,  2.5,  7.5, 13.5,    0,0,0,-1., 43.0 , ! 3DF4
     &               0.9,1.3,1.1,  3.6,  13.3,36.1,  9.7,0,0,-1., 56.1 , ! 4DF4
     &               1.2,0.8,1.9,  1.7,  6.4, 13.3, 79.3,0,0,-1., 99.1 , ! 5DF4
     &               0.9,1.9,3.8,  1.3,  21.3,54.2, 21.4,0,0,-1., 115.9, ! 6DF4
     &               1.1,2.1,4.1,  1.6,  29.2,73.6,219.1,0,0,-1., 117.8, ! 7DF4
     &               0.4,0.6,1.1,  6.9,  8.0, 3.1,     0,0,0,-1., 18.7 , ! 1SA1
     &               0.5,1.1,0.8,  1.8,  5.9, 10.8,    0,0,0,-1., 74.8 , ! 2SA1
     &               0.3,3.0,6.0,  7.5,  10.7,8.8,     0,0,0,-1., 11.2 , ! 3SA1
     &               0.2,0.1,0.9,  2.0,  2.0, 0.0,     0,0,0,-1., 0.0  , ! 1SA2
     &               0.2,1.0,3.3,  11.8, 15.3,9.6,     0,0,0,-1., 7.5  , ! 2SA2
     &               0.5,1.1,0.5,  0.3,  0.3, 0.0,     0,0,0,-1., 34.3 , ! 1SA3
     &               0.4,2.7,1.1,  0.6,  2.8, 6.1,     0,0,0,-1., 25.7 , ! 2SA3
     &               0.4,0.8,2.1,  6.8,  9.0, 5.8,     0,0,0,-1., 39.3 , ! 3SA3
     &               0.2,0.2,0.4,  0.3,  6.5, 18.6,  9.0,0,0,-1., 34.3 , ! 1SA4
     &               0.4,1.2,1.5,  2.8,  5.2, 6.5,  18.7,0,0,-1., 52.4 , ! 2SA4
     &               0.6,2.3,1.9,  1.0,  1.0, 0.0,     0,0,0,-1., 28.6 , ! 1MC2
     &               0.5,1.3,3.0,  2.2,  2.7, 1.1,     0,0,0,-1., 28.6 , ! 2MC2
     &               1.3,3.4,4.9,  14.3, 17.0,7.4,   8.0,0,0,-1., 28.1 , ! 3MC2
     &               0.7,1.1,1.5,  1.5,  2.9, 3.4,     0,0,0,-1., 14.3 , ! 1MC3
     &               0.5,1.8,3.5,  6.1,  6.8, 1.7,     0,0,0,-1., 35.5 , ! 2MC3
     &               0.8,2.7,2.6,  5.3,  12.5,19.6,    0,0,0,-1., 20.6 , ! 3MC3
     &               0.5,1.6,3.3,  2.5,  5.3, 7.7,     0,0,0,-1., 20.6 , ! 1MC4
     &               1.2,3.0,4.1,  7.4,  12.8,14.2,    0,0,0,-1., 56.1 , ! 2MC4
     &               0.3,0.7,4.0,  0.4,  0.4, 0.0,     0,0,0,-1., 7.2  , ! 1LP1
     &               0.4,1.2,7.4,  1.0,  1.1, 0.0,     0,0,0,-1., 5.7  , ! 2LP1
     &               0.6,2.1,10.4, 2.3,  2.4, 0.0,     0,0,0,-1., 8.8  , ! 3LP1
     &               0.4,1.1,0.7,  0.4,  0.4, 0.0,     0,0,0,-1., 8.6  , ! 1LP2
     &               0.2,0.3,1.0,  7.0,  8.4, 3.6,     0,0,0,-1., 0.0  , ! 2LP2
     &               0.7,2.3,5.9,  2.5,  7.7, 13.7,    0,0,0,-1., 8.6  , ! 3LP2
     &               0.5,1.9,7.0,  4.8,  9.2, 11.7,    0,0,0,-1., 11.2 , ! 4LP2
     &               0.2,0.9,1.7,  0.6,  0.7, 0.0,     0,0,0,-1., 10.8 , ! 1LP3
     &               0.2,1.0,4.1,  5.3,  5.6, 0.6,     0,0,0,-1., 11.4 , ! 2LP3
     &               0.2,1.1,3.4,  7.4,  8.4, 2.5,     0,0,0,-1., 4.3  , ! 3LP3
     &               0.0,0.3,0.1,  0.5,  0.5, 0.0,     0,0,0,-1., 3.6  , ! 1PP&Assoc3
     &               0.3,0.9,0.2,  0.1,  0.2, 0.0,     0,0,0,-1., 10.8 , ! 2PP&Assoc3
     &               1.2,2.3,1.3,  1.4,  1.6, 0.6,   3.5,0,0,-1., 8.6  , ! 3PP&Assoc3
     &               1.4,2.6,1.8,  3.5,  3.6, 0.0,     0,0,0,-1., 47.2 , ! 4PP&Assoc3
     &               1.2,3.3,4.8,  4.5,  4.9, 0.9,     0,0,0,-1., 58.0 , ! 5PP&Assoc3
     &               0.1,0.7,0.8,  0.8,  1.4, 1.5,     0,0,0,-1., 8.4  , ! 1PP&Assoc4
     &               0.5,1.2,1.2,  1.2,  1.9, 1.6,     0,0,0,-1., 42.9 , ! 2PP&Assoc4
     &               0.7,1.6,1.9,  6.9,  10.7,10.0,    0,0,0,-1., 35.5 , ! 3PP&Assoc4
     &               0.1,1.5,2.2,  0.5,  1.1, 1.3,   3.3,0,0,-1., 12.0 , ! 1PP1
     &               0.1,1.5,4.8,  2.2,  2.9, 1.7,     0,0,0,-1., 15.0 , ! 2PP1
     &               0.1,3.9,4.5,  4.7,  8.0, 8.5,  12.8,0,0,-1., 10.0 , ! 3PP1
     &               0.1,1.5,1.2,  0.4,  0.9, 1.2,     0,0,0,-1., 18.0 , ! 1PP2
     &               0.0,1.5,1.5,  1.7,  2.3, 1.5,     0,0,0,-1., 14.4 , ! 2PP2
     &               0.1,1.0,2.5,  1.1,  2.1, 2.5,   4.5,0,0,-1., 14.4 , ! 3PP2
     &               0.1,2.7,3.7,  4.3,  6.4, 5.8,  16.2,0,0,-1., 12.0 , ! 4PP2
     &               0.1,0.4,0.7,  0.2,  0.2, 0.0,     0,0,0,-1., 21.6 , ! 1PP3
     &               0.0,0.9,2.0,  1.7,  1.8, 0.0,     0,0,0,-1., 38.4 , ! 2PP3
     &               0.1,0.6,1.6,  0.2,  0.2, 0.0,   5.6,0,0,-1., 19.2 , ! 3PP3
     &               0.1,1.6,4.2,  1.0,  1.9, 2.1,   4.7,0,0,-1., 19.2 , ! 4PP3
     &               0.1,1.1,2.5,  5.1,  6.9, 4.3,     0,0,0,-1., 13.2 , ! 5PP3
     &               0.2,2.0,3.6,  1.7,  3.8, 5.6,   3.2,0,0,-1., 21.6 , ! 6PP3
     &               0.1,0.7,0.8,  0.6,  2.4, 4.6,  13.5,0,0,-1., 31.5 , ! 7PP3
     &               0.0,2.5,6.9,  4.0,  6.4, 6.1,   3.6,0,0,-1., 7.2  , ! 8PP3
     &               0.0,0.2,0.5,  0.1,  0.0, 0.0,     0,0,0,-1., 4.8  , ! 1PP4
     &               0.3,0.8,0.8,  0.2,  1.3, 2.6,     0,0,0,-1., 20.0 , ! 2PP4
     &               0.2,1.1,1.0,  0.2,  1.6, 3.5,     0,0,0,-1., 28.8 , ! 3PP4
     &               0.2,1.2,2.3,  1.1,  1.9, 1.7,     0,0,0,-1., 24.0 , ! 4PP4
     &               0.1,0.8,0.8,  0.8,  2.2, 3.8,     0,0,0,-1., 33.6 , ! 5PP4
     &               0.0,1.5,4.9,  7.0,  8.8, 4.5,     0,0,0,-1., 28.8 , ! 6PP4
     &               0.0,0.3,1.5,  3.8,  10.1,16.6,    0,0,0,-1., 21.6 , ! 7PP4
     &               0.1,1.1,3.3,  2.0,  7.6, 15.1, 19.3,0,0,-1., 8.4  , ! 8PP4
     &               0.3,1.0,0.4,  1.8,  1.8, 0.0,     0,0,0,-1., 7.2  , ! 1BR
     &               0.1,0.4,0.3,  5.6,  11.7,16.4,  2.3,0,0,-1., 41.1 , ! 2BR
     &               0.2,0.4,0.3,  0.0,  0.0, 0.0,     0,0,0,-1., 2.4  , ! 1JU2
     &               0.2,0.4,0.8,  0.0,  0.0, 0.0,     0,0,0,-1., 3.6  , ! 2JU2
     &               0.0,0.0,0.0,  0.0,  0.0, 0.0,     0,0,0,-1., 1.2  , ! 1GR
     &               0.0,0.2,0.0,  0.0,  0.0, 0.0,     0,0,0,-1., 1.2  / ! 2GR

      DATA PROPROT8 /  0.43, 0.15, 0.10, 0.00, 0.48,
     &                 0.52, 0.52, 0.81, 0.00, 0.35,
     &                 0.21, 0.59, 0.79, 0.05, 0.74,
     &                 0.62, 0.32, 0.24, 0.23, 0.24,
     &                 0.07, 0.07, 0.14, 0.08, 0.23,
     &                 0.07, 0.00, 0.08, 0.47, 0.74,
     &                 0.10, 0.00, 0.20, 0.07, 0.00,
     &                 0.07, 0.12, 0.15, 0.06, 0.01,
     &                 0.07, 0.02, 0.16, 0.47, 0.02,
     &                 0.15, 0.00, 0.03, 0.05, 0.00,
     &                 0.78, 0.35, 0.02, 0.12, 0.75,
     &                 0.03, 0.16, 0.00, 0.31, 0.68,
     &                 0.16, 0.36, 0.65, 0.08, 0.05,
     &                 0.16, 0.00, 0.51, 0.19, 0.41,
     &                 0.07, 0.06, 0.10, 0.24, 0.00,
     &                 0.45, 0.48, 0.28, 0.28, 0.26,
     &                 1.00, 0.26, 0.00, 0.00, 0.00,
     &                 0.00 /

      DO J = 1, 86
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF8VLH(I,J) = REF8VLT(I,J)
            REF8VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF8VLS(I,J) = REF8VLT(I,J) * PROPROT8(J)
            REF8VLH(I,J) = MAX(REF8VLT(I,J) - REF8VLS(I,J),0.0)
          ELSE
            REF8VLH(I,J) = REF8VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF9VLT / 0.9, 3.5,4.8, 2.5,  3.4,  2.0, 2.7,0,0,-1.0, 18.7 , ! 1DFWHPRE01
     &               1.6, 2.7,3.9, 5.5,  5.5,  0.0, 3.4,0,0,-1.0, 16.8 , ! 1DFWHPRE02
     &               1.2, 2.0,6.2, 4.4,  5.8,  3.7,   0,0,0,-1.0, 9.4  , ! 1DFWHPRE03
     &               2.8, 4.7,10.2,2.9,  2.9,  0.0,   0,0,0,-1.0, 15.0 , ! 1DFWHPRE04
     &               2.5, 3.7,7.1, 4.0,  5.0,  2.6,   0,0,0,-1.0, 5.6  , ! 1DFWHPRE05
     &               0.6, 2.7,5.5, 11.2, 12.8, 4.3,   0,0,0,-1.0, 1.9  , ! 1DFWHPRE06
     &               4.0, 6.7,18.5,4.2,  4.9,  1.9,   0,0,0,-1.0, 1.8  , ! 1DFWHPRE07
     &               3.1, 5.2,10.9,7.7,  19.1,30.7, 5.5,0,0,-1.0, 33.7 , ! 1DFWHPRE08
     &               2.0, 3.4,8.1, 7.4,  11.4,10.8,48.9,0,0,-1.0, 37.4 , ! 1DFWHPRE09
     &               3.8, 6.3,10.8,6.0,  6.8,  1.3,   0,0,0,-1.0, 1.9  , ! 2WHSSPRE01
     &               0.6, 2.5,4.0, 2.9,  4.1,  3.3,   0,0,0,-1.0, 35.5 , ! 3RAPRE01
     &               0.6, 3.7,13.4,8.1,  8.2,  0.3, 6.6,0,0,-1.0, 5.6  , ! 3RAPRE02
     &               1.3, 5.0,15.4,7.5,  8.4,  2.2, 2.8,0,0,-1.0, 16.8 , ! 3RAPRE03
     &               1.1, 3.9,6.5, 6.0,  8.0,  5.3,12.0,0,0,-1.0, 20.6 , ! 3RAPRE04
     &               1.7, 5.1,8.9, 6.4,  9.2,  7.4,15.3,0,0,-1.0, 16.8 , ! 3RAPRE05
     &               1.3, 4.2,12.3,3.3,  4.3,  2.5,27.7,0,0,-1.0, 26.2 , ! 3RAPRE06
     &               0.8, 3.9,7.9, 10.0, 11.6, 3.2,42.6,0,0,-1.0, 11.2 , ! 3RAPRE07
     &               0.5, 0.9,3.3, 3.1,  3.9,  2.8,   0,0,0,-1.0, 20.6 , ! 4DFWHPOST01
     &               0.4, 0.6,2.2, 4.7,  5.2,  1.2, 4.5,0,0,-1.0, 9.4  , ! 4DFWHPOST02
     &               0.4, 0.7,2.5, 3.7,  14.5,29.3,17.3,0,0,-1.0, 15.0 , ! 4DFWHPOST03
     &               0.5, 0.8,4.8, 7.4,  12.6,14.1,65.3,0,0,-1.0, 39.3 , ! 4DFWHPOST04
     &               0.1, 0.6,3.4, 2.1,  4.9,  7.4, 4.4,0,0,-1.0, 22.4 , ! 5RAPOST01
     &               0.2, 1.0,3.7, 3.1,  5.0,  4.8,14.4,0,0,-1.0, 9.4  , ! 5RAPOST02
     &               0.1, 0.6,1.9, 1.7,  5.0,  8.7,32.5,0,0,-1.0, 9.4  , ! 5RAPOST03
     &               0.2, 1.4,4.2, 9.1,  10.4, 3.6,38.3,0,0,-1.0, 3.7  , ! 5RAPOST04
     &               0.2, 1.2,2.8, 3.7,  7.0,  9.0,53.3,0,0,-1.0, 15.0 / ! 5RAPOST05

C
      DATA PROPROT9 /  0.06, 0.13, 0.38, 0.17, 0.19,
     &                 0.33, 0.05, 0.07, 0.20, 0.10,
     &                 0.17, 0.09, 0.04, 0.14, 0.12,
     &                 0.28, 0.07, 0.09, 0.16, 0.18,
     &                 0.14, 0.22, 0.07, 0.19, 0.02,
     &                 0.33 /

      DO J = 1, 26
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF9VLH(I,J) = REF9VLT(I,J)
            REF9VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF9VLS(I,J) = REF9VLT(I,J) * PROPROT9(J)
            REF9VLH(I,J) = MAX(REF9VLT(I,J) - REF9VLS(I,J),0.0)
          ELSE
            REF9VLH(I,J) = REF9VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF11VLT / 0.0, 0.3,  1.5,  2.3, 2.3,  0.0,  0,0,0,-1., 1.9 , !  1PP1TH
     &                0.1, 1.2,  3.3,  3.1, 3.3,  0.4,  0,0,0,-1., 9.6 , !  2PP1TH
     &                0.1, 2.3,  3.2,  5.2, 5.3,  0.0,  0,0,0,-1., 11.2, !  3PP1TH
     &                0.2, 3.1,  3.9,  4.5, 4.6,  0.7,  0,0,0,-1., 30.0, !  4PP1TH
     &                0.3, 2.4,  8.3,  3.6, 3.6,  0.0,  0,0,0,-1., 18.7, !  5PP1TH
     &                0.2, 3.8,  3.9,  5.1, 5.3,  0.4,  0,0,0,-1., 33.7, !  6PP1TH
     &                0.6, 10.2, 21.5, 7.4, 7.4,  0.0,  0,0,0,-1., 43.0, !  7PP1TH
     &                0.1, 1.5,  2.4,  0.1, 0.1,  0.0,  0,0,0,-1., 12.0, !  1PP2PC
     &                0.1, 1.4,  2.8,  1.2, 1.3,  0.0,  0,0,0,-1., 9.6 , !  2PP2PC
     &                0.1, 2.0,  3.2,  1.4, 1.5,  0.0,  0,0,0,-1., 7.2 , !  3PP2PC
     &                0.1, 2.7,  4.3,  1.7, 2.1,  1.0,  0,0,0,-1., 14.4, !  4PP2PC
     &                0.4, 7.6,  4.6,  5.2, 5.8,  1.5,  0,0,0,-1., 24.0, !  5PP2PC
     &                0.1, 0.7,  1.9,  0.8, 1.1,  0.6,  0,0,0,-1., 6.0 , !  1PP3PC
     &                0.3, 3.3,  3.7,  2.8, 3.0,  0.7,  0,0,0,-1., 14.4, !  2PP3PC
     &                0.2, 3.1,  4.0,  3.1, 4.0,  2.1,  0,0,0,-1., 22.9, !  3PP3PC
     &                0.1, 1.2,  3.0,  2.7, 2.9,  0.5,  0,0,0,-1., 9.6 , !  1PP3CC
     &                0.3, 2.5,  6.9,  5.7, 6.3,  1.4,  0,0,0,-1., 16.8, !  2PP3CC
     &                0.6, 3.3,  4.6,  2.6, 3.0,  1.2,  0,0,0,-1., 14.4, !  1PPSP3PC
     &                0.6, 5.5,  8.5,  4.9, 5.2,  0.7,  0,0,0,-1., 25.2, !  2PPSP3PC
     &                0.8, 4.9,  7.5,  7.5, 9.4,  4.0,  0,0,0,-1., 41.5, !  3PPSP3PC
     &                0.6, 3.2,  5.0,  2.3, 3.9,  3.9,  0,0,0,-1., 20.4, !  1SP3PC
     &                0.9, 4.6,  7.9,  9.7, 12.0, 5.7,  0,0,0,-1., 44.3, !  2SP3PC
     &                0.1, 0.5,  0.6,  1.4, 1.4,  0.0,  0,0,0,-1., 19.2, !  1PP1
     &                0.1, 1.0,  1.1,  0.8, 0.8,  0.0,  0,0,0,-1., 14.4, !  1PP2
     &                0.1, 0.3,  0.4,  0.8, 1.4,  1.6,3.6,0,0,-1., 13.2, !  2PP2
     &                0.0, 0.5,  1.2,  1.4, 1.5,  0.0,  0,0,0,-1., 20.4/!  1PP3
C
      DATA PROPROT11 /  0.09, 0.19, 0.02, 0.26, 0.00,
     &                  0.06, 0.05, 0.00, 0.08, 0.00,
     &                  0.29, 0.23, 0.00, 0.09, 0.23,
     &                  0.18, 0.14, 0.34, 0.07, 0.29,
     &                  0.33, 0.27, 0.18, 0.37, 0.01,
     &                  0.24/

      DO J = 1, 26
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF11VLH(I,J) = REF11VLT(I,J)
            REF11VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF11VLS(I,J) = REF11VLT(I,J) * PROPROT11(J)
            REF11VLH(I,J) = MAX(REF11VLT(I,J) - REF11VLS(I,J),0.0)
          ELSE
            REF11VLH(I,J) = REF11VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF12VLT / 0.0,0.3, 1.5, 2.3, 2.3, 0.0,    0,0,0,-1.0, 1.2  , ! 1PP1TH(BH)
     &                0.1,1.2, 3.3, 3.2, 3.3, 0.4,    0,0,0,-1.0, 9.6  , ! 2PP1TH(BH)
     &                0.1,2.3, 3.2, 5.2, 5.1, 0.0,    0,0,0,-1.0, 7.2  , ! 3PP1TH(BH)
     &                0.2,3.1, 3.9, 4.5, 4.6, 0.7,    0,0,0,-1.0, 19.2 , ! 4PP1TH(BH)
     &                0.3,2.4, 8.3, 3.6, 3.6, 0.0,    0,0,0,-1.0, 12.0 , ! 5PP1TH(BH)
     &                0.2,3.8, 3.9, 5.2, 5.3, 0.4,    0,0,0,-1.0, 21.6 , ! 6PP1TH(BH)
     &                0.6,10.2,21.5,7.4, 7.4, 0.0,    0,0,0,-1.0, 27.6 , ! 7PP1TH(BH)
     &                0.1,1.5, 2.4, 0.1, 0.1, 0.0,    0,0,0,-1.0, 12.0 , ! 1PP2PC
     &                0.1,1.4, 2.8, 1.2, 1.3, 0.0,    0,0,0,-1.0, 9.6  , ! 2PP2PC
     &                0.1,2.0, 3.2, 1.4, 1.5, 0.0,    0,0,0,-1.0, 7.2  , ! 3PP2PC
     &                0.1,2.7, 4.3, 1.7, 2.1, 1.0,    0,0,0,-1.0, 14.4 , ! 4PP2PC
     &                0.4,7.6, 4.6, 5.2, 5.8, 1.5,    0,0,0,-1.0, 24.0 , ! 5PP2PC
     &                0.1,0.7, 1.9, 0.8, 1.1, 0.6,    0,0,0,-1.0, 6.0  , ! 1PP3PC
     &                0.3,3.3, 3.7, 2.8, 3.0, 0.7,    0,0,0,-1.0, 14.4 , ! 2PP3PC
     &                0.2,3.1, 4.0, 3.1, 4.0, 2.1,    0,0,0,-1.0, 22.9 , ! 3PP3PC
     &                0.1,1.2, 3.0, 2.7, 2.9, 0.5,    0,0,0,-1.0, 9.6  , ! 1PP3CC
     &                0.3,2.5, 6.9, 5.7, 6.3, 1.4,    0,0,0,-1.0, 16.8 , ! 2PP3CC
     &                0.6,3.3, 4.6, 2.6, 3.0, 1.2,    0,0,0,-1.0, 14.4 , ! 1PPSP3PC
     &                0.6,5.5, 8.5, 4.9, 5.2, 0.7,    0,0,0,-1.0, 25.2 , ! 2PPSP3PC
     &                0.8,4.9, 7.5, 7.5, 9.4, 4.0,    0,0,0,-1.0, 41.5 , ! 3PPSP3PC
     &                0.6,3.2, 5.0, 2.3, 3.9, 3.9,    0,0,0,-1.0, 20.4 , ! 1SP3PC
     &                0.9,4.6, 7.9, 9.7, 12.0,5.7,    0,0,0,-1.0, 44.3 , ! 2SP3PC
     &                0.1,0.5, 0.6, 1.4, 1.4, 0.0,    0,0,0,-1.0, 19.2 , ! 1PP1(BH)
     &                0.1,1.0, 1.1, 0.8, 0.8, 0.0,    0,0,0,-1.0, 14.4 , ! 1PP2(BH)
     &                0.1,0.3, 0.4, 0.8, 1.4, 1.6,  3.6,0,0,-1.0, 13.2 , ! 2PP2(BH)
     &                0.0,0.5, 1.2, 1.4, 1.5, 0.0,    0,0,0,-1.0, 20.4 , ! 1PP3(BH)
     &                1.0,1.6, 1.5, 7.5, 10.5,7.7,    0,0,0, 1.9, 24.3 , ! 1AZPPSPPRE01
     &                1.0,1.6, 2.2, 9.1, 14.8,15.2,   0,0,0, 3.7, 26.2 , ! 1AZPPSPPRE02
     &                1.5,2.5, 2.1, 12.4,17.9,14.4, 5.4,0,0, 5.6, 22.4 , ! 1AZPPSPPRE03
     &                0.6,1.0, 4.0, 8.5, 23.6,40.7,17.4,0,0, 3.7, 28.1 , ! 1AZPPSPPRE04
     &                0.6,2.3, 1.9, 1.0, 1.0, 0.0,    0,0,0,-1.0, 28.6 , ! 1MC2
     &                0.5,1.3, 3.0, 2.2, 2.7, 1.1,    0,0,0,-1.0, 28.6 , ! 2MC2
     &                1.3,3.4, 4.9, 14.3,17.0,7.4,  8.0,0,0,-1.0, 28.1 , ! 3MC2
     &                0.7,1.1, 1.5, 1.5, 2.9, 3.4,    0,0,0,-1.0, 14.3 , ! 1MC3
     &                0.5,1.8, 3.5, 6.1, 6.8, 1.7,    0,0,0,-1.0, 35.5 , ! 2MC3
     &                0.8,2.7, 2.6, 5.3, 12.5,19.6,   0,0,0,-1.0, 20.6 , ! 3MC3
     &                0.0,0.3, 0.1, 0.5, 0.5, 0.0,    0,0,0,-1.0, 3.6  , ! 1PP&Assoc3
     &                0.3,0.9, 0.2, 0.1, 0.2, 0.0,    0,0,0,-1.0, 10.8 , ! 2PP&Assoc3
     &                1.2,2.3, 1.3, 1.4, 1.6, 0.6,  3.5,0,0,-1.0, 8.6  , ! 3PP&Assoc3
     &                1.4,2.6, 1.8, 3.5, 3.6, 0.0,    0,0,0,-1.0, 47.2 , ! 4PP&Assoc3
     &                1.2,3.3, 4.8, 4.5, 4.9, 0.9,    0,0,0,-1.0, 58.0 , ! 5PP&Assoc3
     &                0.1,0.7, 0.8, 0.8, 1.4, 1.5,    0,0,0,-1.0, 8.4  , ! 1PP&Assoc4
     &                0.5,1.2, 1.2, 1.2, 1.9, 1.6,    0,0,0,-1.0, 42.9 , ! 2PP&Assoc4
     &                0.7,1.6, 1.9, 6.9, 10.7,10.0,   0,0,0,-1.0, 35.5 , ! 3PP&Assoc4
     &                0.1,1.5, 2.2, 0.5, 1.1, 1.3,  3.3,0,0,-1.0, 12.0 , ! 1PP1
     &                0.1,1.5, 4.8, 2.2, 2.9, 1.7,    0,0,0,-1.0, 15.0 , ! 2PP1
     &                0.1,3.9, 4.5, 4.7, 8.0, 8.5, 12.8,0,0,-1.0, 10.0 , ! 3PP1
     &                0.1,1.5, 1.2, 0.4, 0.9, 1.2,    0,0,0,-1.0, 18.0 , ! 1PP2(PNW105)
     &                0.0,1.5, 1.5, 1.7, 2.3, 1.5,    0,0,0,-1.0, 14.4 , ! 2PP2(PNW105)
     &                0.1,1.0, 2.5, 1.1, 2.1, 2.5,  4.5,0,0,-1.0, 14.4 , ! 3PP2(PNW105)
     &                0.1,2.7, 3.7, 4.3, 6.4, 5.8, 16.2,0,0,-1.0, 12.0 , ! 4PP2(PNW105)
     &                0.1,0.4, 0.7, 0.2, 0.2, 0.0,    0,0,0,-1.0, 21.6 , ! 1PP3(PNW105)
     &                0.0,0.9, 2.0, 1.7, 1.8, 0.0,    0,0,0,-1.0, 38.4 , ! 2PP3(PNW105)
     &                0.1,0.6, 1.6, 0.2, 0.2, 0.0,  5.6,0,0,-1.0, 19.2 , ! 3PP3(PNW105)
     &                0.1,1.6, 4.2, 1.0, 1.9, 2.1,  4.7,0,0,-1.0, 19.2 , ! 4PP3(PNW105)
     &                0.1,1.1, 2.5, 5.1, 6.9, 4.3,    0,0,0,-1.0, 13.2 , ! 5PP3
     &                0.2,2.0, 3.6, 1.7, 3.8, 5.6,  3.2,0,0,-1.0, 21.6 , ! 6PP3
     &                0.1,0.7, 0.8, 0.6, 2.4, 4.6, 13.5,0,0,-1.0, 31.5 , ! 7PP3
     &                0.0,2.5, 6.9, 4.0, 6.4, 6.1,  3.6,0,0,-1.0, 7.2  , ! 8PP3
     &                0.0,0.2, 0.5, 0.1, 0.0, 0.0,    0,0,0,-1.0, 4.8  , ! 1PP4(PNW105)
     &                0.3,0.8, 0.8, 0.2, 1.3, 2.6,    0,0,0,-1.0, 20.0 , ! 2PP4(PNW105)
     &                0.2,1.1, 1.0, 0.2, 1.6, 3.5,    0,0,0,-1.0, 28.8 , ! 3PP4(PNW105)
     &                0.2,1.2, 2.3, 1.1, 1.9, 1.7,    0,0,0,-1.0, 24.0 , ! 4PP4
     &                0.2,0.4, 0.3, 0.0, 0.0, 0.0,    0,0,0,-1.0, 2.4  , ! 1JU2
     &                0.2,0.4, 0.8, 0.0, 0.0, 0.0,    0,0,0,-1.0, 3.6  , ! 2JU2
     &                0.0,0.4, 1.8, 0.2, 0.3, 0.0,    0,0,0,-1.0, 13.1 , ! 1PP4PC
     &                0.0,2.2, 3.4, 1.4, 1.5, 0.0,    0,0,0,-1.0, 15.0 , ! 2PP4PC
     &                0.0,2.7, 5.0, 2.8, 2.9, 0.0,    0,0,0,-1.0, 11.2 , ! 3PP4PC
     &                0.0,2.3, 4.8, 2.1, 5.7, 4.7,  2.5,0,0,-1.0, 18.7 , ! 4PP4PC
     &                0.0,3.8, 7.4, 3.5, 4.8, 3.4,  6.6,0,0,-1.0, 22.4 , ! 5PP4PC
     &                0.0,3.4, 2.3, 0.9, 1.0, 0.0,    0,0,0,-1.0, 20.6 , ! 1PP1TH
     &                0.0,2.7, 5.5, 1.1, 1.2, 0.0,    0,0,0,-1.0, 20.6 , ! 2PP1TH
     &                0.0,3.5, 3.2, 2.7, 2.7, 0.0,    0,0,0,-1.0, 24.3 , ! 3PP1TH
     &                0.0,2.1, 4.7, 2.7, 2.8, 0.0,    0,0,0,-1.0, 26.2 , ! 4PP1TH
     &                0.0,5.2, 13.0,1.9, 2.0, 0.0,    0,0,0,-1.0, 13.0 , ! 5PP1TH
     &                0.0,5.5, 6.7, 6.4, 6.4, 0.0,  3.5,0,0,-1.0, 35.5 , ! 6PP1TH
     &                0.2,0.5, 1.4, 0.4, 0.4, 0.0,    0,0,0,-1.0, 25.7 , ! 1PP2
     &                0.1,0.3, 0.3, 0.2, 1.2, 2.3,    0,0,0,-1.0, 15.6 , ! 2PP2
     &                0.1,0.5, 0.7, 0.4, 1.6, 2.9,    0,0,0,-1.0, 9.0  , ! 3PP2
     &                0.5,1.1, 2.1, 2.7, 2.9, 0.6,    0,0,0,-1.0, 42.9 , ! 4PP2
     &                0.1,0.2, 0.4, 0.5, 0.6, 0.0,    0,0,0,-1.0, 26.4 , ! 1PP3
     &                0.2,0.6, 0.8, 0.1, 0.2, 1.1,    0,0,0,-1.0, 24.0 , ! 2PP3
     &                0.2,0.5, 1.1, 0.7, 1.3, 1.2,    0,0,0,-1.0, 29.4 , ! 3PP3
     &                0.1,0.9, 1.5, 1.2, 1.6, 0.8,  2.3,0,0,-1.0, 27.2 , ! 4PP3
     &                0.1,0.7, 0.4, 0.4, 0.9, 1.0,    0,0,0,-1.0, 12.0 , ! 1PP4
     &                0.0,1.0, 2.0, 0.9, 1.1, 0.7,    0,0,0,-1.0, 19.2 , ! 2PP4
     &                0.3,1.4, 1.8, 0.7, 2.2, 3.6,    0,0,0,-1.0, 24.0 , ! 3PP4
     &                0.9,2.2, 5.1, 3.2, 3.2, 0.0,    0,0,0,-1.0, 31.8 , ! 3WF2
     &                0.7,1.4, 2.2, 3.4, 7.1, 9.8,  5.0,0,0,-1.0, 41.1 , ! 4WF3
     &                0.9,1.8, 2.2, 5.2, 5.5, 0.8,  1.6,0,0,-1.0, 50.5 / ! 3WF3

      DATA PROPROT12 /  0.09, 0.19, 0.02, 0.26, 0.00,
     &                  0.06, 0.05, 0.00, 0.08, 0.00,
     &                  0.29, 0.23, 0.00, 0.09, 0.23,
     &                  0.18, 0.14, 0.34, 0.07, 0.29,
     &                  0.33, 0.27, 0.18, 0.37, 0.01,
     &                  0.24, 0.43, 0.54, 0.06, 0.38,
     &                  0.00, 0.20, 0.07, 0.00, 0.07,
     &                  0.12, 0.00, 0.78, 0.35, 0.02,
     &                  0.12, 0.75, 0.03, 0.16, 0.00,
     &                  0.31, 0.68, 0.16, 0.36, 0.65,
     &                  0.08, 0.05, 0.16, 0.00, 0.51,
     &                  0.19, 0.41, 0.07, 0.06, 0.10,
     &                  0.24, 0.00, 0.45, 0.00, 0.00,
     &                  0.00, 0.00, 0.00, 0.00, 0.11,
     &                  0.00, 0.61, 0.02, 0.05, 0.02,
     &                  0.01, 0.11, 0.80, 0.48, 0.71,
     &                  0.83, 0.91, 0.89, 0.24, 0.49,
     &                  0.08, 0.51, 0.30, 0.69, 0.29 /

      DO J = 1, 90
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF12VLH(I,J) = REF12VLT(I,J)
            REF12VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF12VLS(I,J) = REF12VLT(I,J) * PROPROT12(J)
            REF12VLH(I,J) = MAX(REF12VLT(I,J) - REF12VLS(I,J),0.0)
          ELSE
            REF12VLH(I,J) = REF12VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF13VLH / 0.0, 2.0, 1.5, 1.8, 1.8, 0.0,   0,0,0,-1.0, 35.5 , ! 1DF4CC
     &                0.0, 2.4, 7.5, 0.7, 0.7, 0.1,   0,0,0,-1.0, 43.1 , ! 2DF4CC
     &                0.0, 0.7, 1.4, 1.2, 2.1, 2.4, 2.1,0,0,-1.0, 76.7 , ! 3DF4CC
     &                0.0, 5.6, 7.6, 0.9, 1.6, 1.9, 1.4,0,0,-1.0, 132.8, ! 4DF4CC
     &                0.0, 2.6, 4.5, 4.8, 7.7, 7.7, 5.6,0,0,-1.0, 61.7 , ! 5DF4CC
     &                0.0, 0.5, 0.4, 2.0, 5.8,10.2,17.4,0,0,-1.0, 39.3 , ! 6DF4CC
     &                0.0, 4.9, 11.3,1.3, 1.8, 1.2, 5.4,0,0,-1.0, 147.7, ! 7DF4CC
     &                0.0, 4.3, 5.9, 3.8, 6.8, 7.9,25.8,0,0,-1.0, 115.9, ! 8DF4CC
     &                0.0, 4.8, 8.4, 5.6,10.3,12.5,15.7,0,0,-1.0, 183.3, ! 9DF4CC
     &                0.0, 3.6, 11.0,1.5, 2.8, 3.5,25.6,0,0,-1.0, 160.8, ! 10DF4CC
     &                0.0, 1.1, 2.3, 0.9, 1.1, 0.6,   0,0,0,-1.0, 13.1 , ! 1DF4PC
     &                0.0, 1.9, 3.6, 3.0, 5.1, 5.7,   0,0,0,-1.0, 26.2 , ! 2DF4PC
     &                0.0, 1.6, 2.7, 2.9, 5.0, 5.7, 3.0,0,0,-1.0, 11.2 , ! 3DF4PC
     &                0.0, 3.5, 5.8, 5.4, 5.8, 1.3,   0,0,0,-1.0, 89.8 , ! 4DF4PC
     &                0.0, 1.4, 3.1, 3.2, 6.7, 9.1,10.0,0,0,-1.0, 20.6 , ! 5DF4PC
     &                0.0, 2.6, 6.2, 5.2,11.3,16.5, 3.5,0,0,-1.0, 61.7 , ! 6DF4PC
     &                0.0, 3.6, 9.5, 6.9, 7.5, 1.7,29.7,0,0,-1.0, 97.2 , ! 7DF4PC
     &                0.0, 3.6, 8.5, 1.6, 3.9, 6.2,12.9,0,0,-1.0, 160.8, ! 8DF4PC
     &                0.0, 2.3, 4.9, 5.4,11.8,16.9,57.9,0,0,-1.0, 61.7 , ! 9DF4PC
     &                0.0, 1.3, 2.8, 2.0, 2.0, 0.0,   0,0,0,-1.0, 24.3 , ! 1DF3PC
     &                0.0, 2.0, 2.1, 5.2, 5.2, 0.0,   0,0,0,-1.0, 15.6 , ! 2DF3PC
     &                0.0, 1.7, 3.6, 3.6, 4.5, 2.4,   0,0,0,-1.0, 24.0 , ! 3DF3PC
     &                0.0, 1.9, 8.0, 4.7, 8.7,10.4, 1.2,0,0,-1.0, 153.3, ! 4DF3PC
     &                0.0, 4.5, 18.6,3.4, 4.6, 3.2,11.2,0,0,-1.0, 82.6 , ! 5DF3PC
     &                0.0, 1.8, 2.6, 5.0, 5.8, 2.3,44.4,0,0,-1.0, 27.5 , ! 6DF3PC
     &                0.0, 0.4, 0.4, 0.0, 0.0, 0.0,   0,0,0,-1.0, 15.6 , ! 1DF1TH
     &                0.0, 0.7, 0.9, 0.0, 0.0, 0.0,   0,0,0,-1.0, 12.0 , ! 2DF1TH
     &                0.0, 2.1, 2.9, 0.6, 0.6, 0.0,   0,0,0,-1.0, 10.8 , ! 3DF1TH
     &                0.0, 4.1, 5.3, 0.8, 0.8, 0.0,   0,0,0,-1.0, 19.8 , ! 4DF1TH
     &                0.0, 1.1, 3.1, 0.9, 1.3, 0.9,   0,0,0,-1.0, 0.0  , ! 1DFHD4CC
     &                0.0, 2.0, 5.1, 4.4, 5.1, 1.9,   0,0,0,-1.0, 0.0  , ! 2DFHD4CC
     &                0.0, 1.3, 4.0, 5.8, 7.6, 4.9, 4.8,0,0,-1.0, 0.0  , ! 3DFHD4CC
     &                0.0, 3.7, 6.3, 7.6,11.0, 9.2, 3.6,0,0,-1.0, 0.0  , ! 4DFHD4CC
     &                0.0, 2.7, 4.8,18.7,25.4,17.9,10.8,0,0,-1.0, 0.0  , ! 5DFHD4CC
     &                0.0, 5.6,10.4,25.3,29.0, 9.8,   0,0,0,-1.0, 0.0  , ! 6DFHD4CC
     &                0.0, 6.5,10.5,38.0,42.3,11.6,   0,0,0,-1.0, 0.0  , ! 7DFHD4CC
     &                0.0, 0.7, 3.4, 0.7, 1.2, 1.1,   0,0,0,-1.0, 1.9  , ! 1DFHD4PC
     &                0.0, 2.0, 8.7, 2.5, 3.2, 2.0,   0,0,0,-1.0, 7.5  , ! 2DFHD4PC
     &                0.0, 1.7, 8.7, 6.3, 8.0, 4.4,   0,0,0,-1.0, 1.9  , ! 3DFHD4PC
     &                0.0, 5.2,10.7, 6.3, 7.8, 4.1, 7.8,0,0,-1.0, 20.6 , ! 4DFHD4PC
     &                0.0, 3.0, 8.8, 9.8,10.9, 2.8,21.9,0,0,-1.0, 22.4 , ! 5DFHD4PC
     &                0.0, 2.5, 8.7, 5.3,11.7,17.1,25.7,0,0,-1.0, 5.6  / ! 6DFHD4PC

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF13VLS / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,    0,0,0, ! 1DF4CC
     &                 0.0, 0.0, 0.0, 2.6, 2.8, 0.4,    0,0,0, ! 2DF4CC
     &                 0.0, 0.0, 0.0, 2.8, 4.9, 5.6,  4.9,0,0, ! 3DF4CC
     &                 0.0, 0.0, 0.0, 2.8, 5.0, 5.9,  4.4,0,0, ! 4DF4CC
     &                 0.0, 0.0, 0.0, 2.4, 3.8, 3.8,  2.7,0,0, ! 5DF4CC
     &                 0.0, 0.0, 0.0, 1.5, 4.6, 8.0, 13.6,0,0, ! 6DF4CC
     &                 0.0, 0.0, 0.0, 9.7,13.0, 8.9, 39.6,0,0, ! 7DF4CC
     &                 0.0, 0.0, 0.0, 4.3, 7.6, 8.9, 29.1,0,0, ! 8DF4CC
     &                 0.0, 0.0, 0.0,11.3,20.8,25.4, 31.8,0,0, ! 9DF4CC
     &                 0.0, 0.0, 0.0, 7.8,14.7,18.5,134.4,0,0, ! 10DF4CC
     &                 0.0, 0.0, 0.0, 0.2, 0.3, 0.2,    0,0,0, ! 1DF4PC
     &                 0.0, 0.0, 0.0, 0.1, 0.2, 0.2,    0,0,0, ! 2DF4PC
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,    0,0,0, ! 3DF4PC
     &                 0.0, 0.0, 0.0, 2.6, 2.9, 0.6,    0,0,0, ! 4DF4PC
     &                 0.0, 0.0, 0.0, 0.2, 0.4, 0.5,  0.5,0,0, ! 5DF4PC
     &                 0.0, 0.0, 0.0, 1.1, 2.5, 3.6,  0.8,0,0, ! 6DF4PC
     &                 0.0, 0.0, 0.0, 0.4, 0.5, 0.1,  1.9,0,0, ! 7DF4PC
     &                 0.0, 0.0, 0.0, 3.4, 8.3,13.1, 27.3,0,0, ! 8DF4PC
     &                 0.0, 0.0, 0.0, 0.5, 1.2, 1.7,  5.7,0,0, ! 9DF4PC
     &                 0.0, 0.0, 0.0, 0.4, 0.4, 0.0,    0,0,0, ! 1DF3PC
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0,    0,0,0, ! 2DF3PC
     &                 0.0, 0.0, 0.0, 3.7, 4.7, 2.5,    0,0,0, ! 3DF3PC
     &                 0.0, 0.0, 0.0, 5.8,10.6,12.8,  1.4,0,0, ! 4DF3PC
     &                 0.0, 0.0, 0.0, 4.7, 6.4, 4.5, 15.4,0,0, ! 5DF3PC
     &                 0.0, 0.0, 0.0, 2.2, 2.6, 1.0, 20.0,0,0, ! 6DF3PC
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,    0,0,0, ! 1DF1TH
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,    0,0,0, ! 2DF1TH
     &                 0.0, 0.0, 0.0, 0.8, 0.8, 0.0,    0,0,0, ! 3DF1TH
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,    0,0,0, ! 4DF1TH
     &                 0.0, 0.0, 0.0, 0.9, 1.3, 0.9,    0,0,0, ! 1DFHD4CC
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,    0,0,0, ! 2DFHD4CC
     &                 0.0, 0.0, 0.0, 2.5, 3.3, 2.1,  2.0,0,0, ! 3DFHD4CC
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.1,    0,0,0, ! 4DFHD4CC
     &                 0.0, 0.0, 0.0, 0.2, 0.3, 0.2,  0.1,0,0, ! 5DFHD4CC
     &                 0.0, 0.0, 0.0, 0.5, 0.6, 0.2,    0,0,0, ! 6DFHD4CC
     &                 0.0, 0.0, 0.0, 0.8, 0.9, 0.2,    0,0,0, ! 7DFHD4CC
     &                 0.0, 0.0, 0.0, 1.3, 2.0, 1.9,    0,0,0, ! 1DFHD4PC
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0,    0,0,0, ! 2DFHD4PC
     &                 0.0, 0.0, 0.0, 2.1, 2.7, 1.5,    0,0,0, ! 3DFHD4PC
     &                 0.0, 0.0, 0.0, 0.1, 0.2, 0.1,  0.2,0,0, ! 4DFHD4PC
     &                 0.0, 0.0, 0.0, 5.1, 5.6, 1.4, 11.3,0,0, ! 5DFHD4PC
     &                 0.0, 0.0, 0.0, 1.8, 3.9, 5.7,  8.6,0,0/ ! 6DFHD4PC

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF14VLH / 0.0,0.2, 0.1, 0.0,  0.0,  0.0,    0,0,0,0.1, -1.0,  !BG01
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.6, -1.0,  !BG02
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,1.8, -1.0,  !BG03
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,1.0, -1.0,  !BG04
     &                0.3,0.6, 1.5, 1.3,  1.8,  1.2,    0,0,0,1.5, 9.7 ,  !MC01
     &                0.4,1.5, 2.7, 2.3,  2.9,  1.6,    0,0,0,1.3, 14.3,  !MC02
     &                0.5,1.7, 2.4, 4.9,  5.0,  0.6,    0,0,0,1.9, 16.3,  !MC03
     &                0.3,1.0, 1.4, 1.7,  3.2,  4.3,    0,0,0,1.4, 15.6,  !MC04
     &                0.3,1.1, 5.4, 11.1, 11.1, 0.0,    0,0,0,2.3, 17.3,  !MC05
     &                0.7,1.6, 3.8, 4.4,  4.8,  1.4,    0,0,0,1.2, 17.2,  !MC06
     &                0.4,3.1, 5.1, 7.1,  7.7,  1.5,  3.0,0,0,2.0, 30.2,  !MC07
     &                0.8,2.5, 4.4, 11.3, 11.7, 1.1,    0,0,0,1.7, 15.7,  !MC08
     &                0.3,0.8, 3.6, 12.3, 12.3, 0.0,    0,0,0,1.2, 14.6,  !MC09
     &                0.5,1.3, 3.5, 6.3,  10.5, 11.3,   0,0,0,1.0, 23.2,  !MC10
     &                0.5,1.1, 2.1, 7.6,  10.7, 8.4, 15.4,0,0,0.9, 6.7 ,  !MC11
     &                0.4,1.3, 1.5, 1.5,  1.5,  0.4,    0,0,0,1.4, 20.4,  !MC12
     &                0.4,1.9, 3.2, 11.5, 17.3, 15.5,   0,0,0,1.3, 21.3,  !MC13
     &                0.7,1.9, 3.0, 16.9, 22.0, 13.9,   0,0,0,2.2, 16.5,  !MC14
     &                0.2,1.3, 4.3, 2.0,  3.1,  2.9, 16.2,0,0,1.2, 13.9,  !MC15
     &                0.6,2.4, 4.1, 14.8, 24.1, 24.9,   0,0,0,1.2, 18.9,  !MC16
     &                0.4,2.0, 2.6, 10.0, 23.4, 36.1, 4.7,0,0,1.1, 14.7,  !MC17
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.0, -1.0,  !SB01
     &                0.1,0.1, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.1, 0.0 ,  !SB02
     &                0.1,0.1, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.2, 0.1 ,  !SB03
     &                0.2,0.3, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.1, -1.0,  !SB04
     &                0.1,0.2, 0.4, 0.2,  0.1,  0.0,    0,0,0,0.1, 0.0 ,  !WJ01
     &                0.2,0.4, 0.2, 0.0,  0.0,  0.0,    0,0,0,0.2, 0.0 ,  !WJ02
     &                0.0,0.1, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.1, 0.0 ,  !WJ03
     &                0.1,0.2, 0.2, 0.0,  0.0,  0.0,    0,0,0,0.1, 0.0 /  !WJ04

C                 <.25  to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF14VLS /  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG01
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG02
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG03
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG04
     &                  0.0, 0.0, 0.0, 0.8, 1.2, 1.1,     0,0,0,  !MC01
     &                  0.0, 0.0, 0.0, 1.7, 2.3, 1.7,     0,0,0,  !MC02
     &                  0.0, 0.0, 0.0, 0.4, 1.1, 2.0,     0,0,0,  !MC03
     &                  0.0, 0.0, 0.0, 1.6, 3.1, 4.0,   5.1,0,0,  !MC04
     &                  0.0, 0.0, 0.0, 0.7, 0.8, 0.4,     0,0,0,  !MC05
     &                  0.0, 0.0, 0.0, 2.0, 4.0, 5.2,   3.1,0,0,  !MC06
     &                  0.0, 0.0, 0.0, 1.9, 3.2, 3.9,     0,0,0,  !MC07
     &                  0.0, 0.0, 0.0, 1.6, 2.5, 2.5,   1.4,0,0,  !MC08
     &                  0.0, 0.0, 0.0, 1.5, 5.0, 9.3,     0,0,0,  !MC09
     &                  0.0, 0.0, 0.0, 3.5, 5.1, 4.7,     0,0,0,  !MC10
     &                  0.0, 0.0, 0.0, 1.1, 2.4, 3.3,     0,0,0,  !MC11
     &                  0.0, 0.0, 0.0, 0.6, 5.1, 12.0, 29.8,0,0,  !MC12
     &                  0.0, 0.0, 0.0, 2.9, 3.7, 2.3,   1.8,0,0,  !MC13
     &                  0.0, 0.0, 0.0, 1.5, 2.1, 1.6,     0,0,0,  !MC14
     &                  0.0, 0.0, 0.0, 0.9, 2.4, 4.1,  35.1,0,0,  !MC15
     &                  0.0, 0.0, 0.0, 1.5, 2.2, 1.7,     0,0,0,  !MC16
     &                  0.0, 0.0, 0.0, 0.9, 2.3, 3.7,     0,0,0,  !MC17
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB01
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB02
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB03
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB04
     &                  0.0, 0.0, 0.0, 0.3, 0.7, 1.0,     0,0,0,  !WJ01
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !WJ02
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !WJ03
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0/  !WJ04

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF15VLH / 0.0,0.2, 0.1, 0.0,  0.0,  0.0,    0,0,0,0.1, -1.0,  !BG01
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.6, -1.0,  !BG02
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,1.8, -1.0,  !BG03
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,1.0, -1.0,  !BG04
     &                0.3,0.6, 1.5, 1.3,  1.8,  1.2,    0,0,0,1.5, 9.7 ,  !MC01
     &                0.4,1.5, 2.7, 2.3,  2.9,  1.6,    0,0,0,1.3, 14.3,  !MC02
     &                0.5,1.7, 2.4, 4.9,  5.0,  0.6,    0,0,0,1.9, 16.3,  !MC03
     &                0.3,1.0, 1.4, 1.7,  3.2,  4.3,    0,0,0,1.4, 15.6,  !MC04
     &                0.3,1.1, 5.4, 11.1, 11.1, 0.0,    0,0,0,2.3, 17.3,  !MC05
     &                0.7,1.6, 3.8, 4.4,  4.8,  1.4,    0,0,0,1.2, 17.2,  !MC06
     &                0.4,3.1, 5.1, 7.1,  7.7,  1.5,  3.0,0,0,2.0, 30.2,  !MC07
     &                0.8,2.5, 4.4, 11.3, 11.7, 1.1,    0,0,0,1.7, 15.7,  !MC08
     &                0.3,0.8, 3.6, 12.3, 12.3, 0.0,    0,0,0,1.2, 14.6,  !MC09
     &                0.5,1.3, 3.5, 6.3,  10.5, 11.3,   0,0,0,1.0, 23.2,  !MC10
     &                0.5,1.1, 2.1, 7.6,  10.7, 8.4, 15.4,0,0,0.9, 6.7 ,  !MC11
     &                0.4,1.3, 1.5, 1.5,  1.5,  0.4,    0,0,0,1.4, 20.4,  !MC12
     &                0.4,1.9, 3.2, 11.5, 17.3, 15.5,   0,0,0,1.3, 21.3,  !MC13
     &                0.7,1.9, 3.0, 16.9, 22.0, 13.9,   0,0,0,2.2, 16.5,  !MC14
     &                0.2,1.3, 4.3, 2.0,  3.1,  2.9, 16.2,0,0,1.2, 13.9,  !MC15
     &                0.6,2.4, 4.1, 14.8, 24.1, 24.9,   0,0,0,1.2, 18.9,  !MC16
     &                0.4,2.0, 2.6, 10.0, 23.4, 36.1, 4.7,0,0,1.1, 14.7,  !MC17
     &                0.0,0.0, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.0, -1.0,  !SB01
     &                0.1,0.1, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.1, 0.0 ,  !SB02
     &                0.1,0.1, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.2, 0.1 ,  !SB03
     &                0.2,0.3, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.1, -1.0,  !SB04
     &                0.1,0.2, 0.4, 0.2,  0.1,  0.0,    0,0,0,0.1, 0.0 ,  !WJ01
     &                0.2,0.4, 0.2, 0.0,  0.0,  0.0,    0,0,0,0.2, 0.0 ,  !WJ02
     &                0.0,0.1, 0.0, 0.0,  0.0,  0.0,    0,0,0,0.1, 0.0 ,  !WJ03
     &                0.1,0.2, 0.2, 0.0,  0.0,  0.0,    0,0,0,0.1, 0.0 /  !WJ04

C                 <.25  to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF15VLS /  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG01
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG02
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG03
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !BG04
     &                  0.0, 0.0, 0.0, 0.8, 1.2, 1.1,     0,0,0,  !MC01
     &                  0.0, 0.0, 0.0, 1.7, 2.3, 1.7,     0,0,0,  !MC02
     &                  0.0, 0.0, 0.0, 0.4, 1.1, 2.0,     0,0,0,  !MC03
     &                  0.0, 0.0, 0.0, 1.6, 3.1, 4.0,   5.1,0,0,  !MC04
     &                  0.0, 0.0, 0.0, 0.7, 0.8, 0.4,     0,0,0,  !MC05
     &                  0.0, 0.0, 0.0, 2.0, 4.0, 5.2,   3.1,0,0,  !MC06
     &                  0.0, 0.0, 0.0, 1.9, 3.2, 3.9,     0,0,0,  !MC07
     &                  0.0, 0.0, 0.0, 1.6, 2.5, 2.5,   1.4,0,0,  !MC08
     &                  0.0, 0.0, 0.0, 1.5, 5.0, 9.3,     0,0,0,  !MC09
     &                  0.0, 0.0, 0.0, 3.5, 5.1, 4.7,     0,0,0,  !MC10
     &                  0.0, 0.0, 0.0, 1.1, 2.4, 3.3,     0,0,0,  !MC11
     &                  0.0, 0.0, 0.0, 0.6, 5.1, 12.0, 29.8,0,0,  !MC12
     &                  0.0, 0.0, 0.0, 2.9, 3.7, 2.3,   1.8,0,0,  !MC13
     &                  0.0, 0.0, 0.0, 1.5, 2.1, 1.6,     0,0,0,  !MC14
     &                  0.0, 0.0, 0.0, 0.9, 2.4, 4.1,  35.1,0,0,  !MC15
     &                  0.0, 0.0, 0.0, 1.5, 2.2, 1.7,     0,0,0,  !MC16
     &                  0.0, 0.0, 0.0, 0.9, 2.3, 3.7,     0,0,0,  !MC17
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB01
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB02
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB03
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !SB04
     &                  0.0, 0.0, 0.0, 0.3, 0.7, 1.0,     0,0,0,  !WJ01
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !WJ02
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0,  !WJ03
     &                  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0,0,0/  !WJ04

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF16VLH / 0.5, 0.6, 6.0, 0.1,  0.1,  0.0, 0,0,0,3.0, 33.9 , ! AH01
     &                0.2, 0.7, 2.6, 0.0,  0.0,  0.0, 0,0,0,2.0, 25.1 , ! AH02
     &                0.5, 0.7, 2.4, 0.0,  0.0,  0.0, 0,0,0,1.7, 40.7 , ! AH03
     &                0.3, 0.5, 0.5, 0.3,  0.2,  0.0, 0,0,0,0.9, 17.3 , ! AH04
     &                0.1, 0.5, 2.3, 0.2,  0.1,  0.0, 0,0,0,0.9, 29.5 , ! AH05
     &                0.3, 0.9, 2.5, 1.2,  1.4,  0.3, 0,0,0,0.8, 20.8 , ! AH06
     &                0.3, 0.6, 1.3, 0.5,  0.4,  0.0, 0,0,0,3.1, 31.2 , ! AH07
     &                0.4, 0.5, 1.5, 0.2,  0.1,  0.0, 0,0,0,3.2, 48.4 , ! AH08
     &                0.2, 0.8, 1.2, 1.6,  1.6,  0.3, 0,0,0,2.7, 30.3 , ! AH09
     &                0.3, 0.4, 0.3, 0.0,  0.0,  0.0, 0,0,0,2.9, 35.3 , ! AH10
     &                0.4, 0.4, 1.3, 5.1,  6.5,  3.6, 0,0,0,2.5, 36.9 , ! AH11
     &                0.4, 0.9, 2.3, 1.0,  1.0,  0.0, 0,0,0,3.4, 41.4 , ! AH12
     &                0.5, 0.4, 0.9, 0.0,  0.0,  0.0, 0,0,0,1.4, 40.4 , ! AH13
     &                0.5, 0.6, 1.6, 0.6,  0.5,  0.0, 0,0,0,2.2, 28.6 , ! AH14
     &                0.4, 0.6, 1.1, 0.1,  0.1,  0.0, 0,0,0,2.1, 28.1 , ! AH15
     &                0.2, 0.3, 1.2, 0.1,  0.0,  0.0, 0,0,0,0.1, 81.6 , ! BS01
     &                0.1, 0.1, 0.6, 0.0,  0.0,  0.0, 0,0,0,0.0, 159.9, ! BS02
     &                0.1, 0.1, 0.2, 0.0,  0.0,  0.0, 0,0,0,0.0, 166.4, ! BS03
     &                0.2, 0.4, 1.4, 0.7,  0.0,  0.0, 0,0,0,0.0, 128.7, ! BS04
     &                0.1, 0.2, 0.8, 0.0,  0.0,  0.0, 0,0,0,0.0, 38.2 , ! BS05
     &                0.1, 0.2, 0.5, 0.1,  0.0,  0.0, 0,0,0,0.0, 44.0 , ! BS06
     &                0.2, 0.4, 0.8, 0.2,  0.0,  0.0, 0,0,0,0.0, 70.5 , ! BS07
     &                0.1, 0.2, 0.5, 1.5,  0.0,  0.0, 0,0,0,0.0, 75.5 , ! BS08
     &                0.2, 0.4, 2.0, 1.4,  0.0,  0.0, 0,0,0,0.4, 49.5 , ! BS09
     &                0.1, 0.2, 0.1, 1.0,  0.0,  0.0, 0,0,0,0.3, 125.8, ! BS10
     &                0.1, 0.2, 0.4, 0.4,  0.0,  0.0, 0,0,0,0.3, 46.1 , ! BS11
     &                0.2, 0.3, 0.1, 0.0,  0.0,  0.0, 0,0,0,0.3, 132.2, ! BS12
     &                0.3, 0.5, 1.5, 7.6,  0.0,  0.0, 0,0,0,0.6, 48.4 , ! BS13
     &                0.4, 0.6, 0.6, 3.9,  0.0,  0.0, 0,0,0,1.2, 97.6 , ! BS14
     &                0.3, 0.4, 1.5, 3.1,  0.0,  0.0, 0,0,0,1.0, 29.4 , ! WS01
     &                0.3, 0.4, 1.5, 0.2,  0.0,  0.0, 0,0,0,0.2, 160.2, ! WS02
     &                0.4, 0.7, 2.0, 0.1,  0.0,  0.0, 0,0,0,0.9, 30.9 , ! WS03
     &                0.3, 0.5, 0.6, 0.0,  0.0,  0.0, 0,0,0,0.2, 30.7 , ! WS04
     &                1.0, 1.6, 3.3, 29.4, 0.0,  0.0, 0,0,0,0.9, 38.6 , ! WS05
     &                1.2, 2.0, 6.4, 21.1, 0.0,  0.0, 0,0,0,1.4, 44.9 , ! WS06
     &                0.5, 0.8, 2.1, 2.8,  0.0,  0.0, 0,0,0,2.3, 20.8 , ! WS07
     &                0.5, 0.9, 1.7, 10.9, 0.0,  0.0, 0,0,0,0.3, 62.2 , ! WS08
     &                0.8, 1.4, 1.5, 35.6, 0.0,  0.0, 0,0,0,1.4, 57.1 , ! WS09
     &                1.0, 1.7, 3.3, 37.5, 0.0,  0.0, 0,0,0,1.9, 77.8 , ! WS10
     &                0.4, 0.7, 1.7,  7.5, 0.0,  0.0, 0,0,0,1.5, 47.5 , ! WS11
     &                0.6, 0.9, 1.2,  8.5, 0.0,  0.0, 0,0,0,1.9, 44.1 / ! WS12


C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF16VLS / 0.0, 0.0, 0.0, 0.4, 0.4, 0.0, 0,0,0, ! AH01
     &                 0.0, 0.0, 0.0, 1.2, 1.2, 0.2, 0,0,0, ! AH02
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,0,0, ! AH03
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,0,0, ! AH04
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! AH05
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! AH06
     &                 0.0, 0.0, 0.0, 0.4, 0.3, 0.0, 0,0,0, ! AH07
     &                 0.0, 0.0, 0.0, 0.1, 0.3, 0.4, 0,0,0, ! AH08
     &                 0.0, 0.0, 0.0, 0.6, 0.6, 0.2, 0,0,0, ! AH09
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! AH10
     &                 0.0, 0.0, 0.0, 3.1, 4.1, 2.8, 0,0,0, ! AH11
     &                 0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0,0,0, ! AH12
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,0,0, ! AH13
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0, ! AH14
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! AH15
     &                 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0,0,0, ! BS01
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! BS02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! BS03
     &                 0.0, 0.0, 0.0, 0.6, 0.0, 0.0, 0,0,0, ! BS04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! BS05
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! BS06
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! BS07
     &                 0.0, 0.0, 0.0, 0.9, 0.0, 0.0, 0,0,0, ! BS08
     &                 0.0, 0.0, 0.0, 4.5, 0.0, 0.0, 0,0,0, ! BS09
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! BS10
     &                 0.0, 0.0, 0.0, 0.3, 0.0, 0.0, 0,0,0, ! BS11
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! BS12
     &                 0.0, 0.0, 0.0, 2.2, 0.0, 0.0, 0,0,0, ! BS13
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! BS14
     &                 0.0, 0.0, 0.0, 3.1, 0.0, 0.0, 0,0,0, ! WS01
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! WS02
     &                 0.0, 0.0, 0.0, 0.3, 0.0, 0.0, 0,0,0, ! WS03
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! WS04
     &                 0.0, 0.0, 0.0, 4.4, 0.0, 0.0, 0,0,0, ! WS05
     &                 0.0, 0.0, 0.0, 1.8, 0.0, 0.0, 0,0,0, ! WS06
     &                 0.0, 0.0, 0.0, 1.3, 0.0, 0.0, 0,0,0, ! WS07
     &                 0.0, 0.0, 0.0, 1.6, 0.0, 0.0, 0,0,0, ! WS08
     &                 0.0, 0.0, 0.0, 4.0, 0.0, 0.0, 0,0,0, ! WS09
     &                 0.0, 0.0, 0.0, 7.0, 0.0, 0.0, 0,0,0, ! WS10
     &                 0.0, 0.0, 0.0,10.6, 0.0, 0.0, 0,0,0, ! WS11
     &                 0.0, 0.0, 0.0, 3.4, 0.0, 0.0, 0,0,0 / ! WS12

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF17VLH / 0.2, 0.6, 1.1, 1.4,  1.9,  0.0,   0,0,0,0.9, -1.0,  !GO01
     &                0.0, 0.1, 0.0, 0.0,  0.0,  0.0,   0,0,0,0.0, -1.0,  !GO02
     &                0.2, 0.2, 0.0, 0.0,  0.0,  0.0,   0,0,0,2.3, -1.0,  !GO03
     &                0.3, 0.2, 0.9, 0.0,  0.0,  0.0,   0,0,0,2.8, -1.0,  !GO04
     &                0.3, 0.7, 0.2, 0.0,  0.0,  0.0,   0,0,0,3.1, -1.0,  !GO05
     &                0.3, 0.4, 0.9, 0.0,  0.0,  0.0,   0,0,0,4.2, -1.0,  !GO06
     &                0.2, 1.2, 3.4, 0.5,  0.5,  0.0,   0,0,0,5.8, -1.0,  !GO07
     &                0.3, 0.4, 1.2, 0.1,  0.1,  0.0,   0,0,0,2.5, -1.0,  !GO08
     &                0.3, 0.9, 2.0, 0.7,  0.7,  0.0,   0,0,0,1.7, -1.0,  !GO09
     &                0.3, 0.1, 0.0, 0.0,  0.0,  0.0,   0,0,0,3.3, 2.0 ,  !LP01
     &                0.3, 0.9, 1.2, 1.1,  1.1,  0.4,   0,0,0,4.6, 17.3,  !LP02
     &                0.3, 0.9, 1.0, 1.0,  0.9,  0.0,   0,0,0,2.4, 10.3,  !LP03
     &                0.2, 0.6, 3.7, 2.0,  2.0,  0.0,   0,0,0,1.8, 12.1,  !LP04
     &                0.0, 0.6, 7.7, 0.7,  0.6,  0.0,   0,0,0,3.5, 8.2 ,  !LP05
     &                0.3, 0.7, 1.9, 3.0,  3.1,  0.6,   0,0,0,1.0, 5.7 ,  !LP06
     &                0.4, 0.8, 1.4, 3.6,  3.8,  0.7,   0,0,0,2.0, 5.8 ,  !LP07
     &                0.4, 2.2, 2.8, 4.2,  4.3,  0.5,   0,0,0,2.5, 6.1 ,  !LP08
     &                0.2, 1.0, 3.1, 6.5,  6.5,  0.3,   0,0,0,0.1, 0.0 ,  !LP09
     &                0.2, 1.1, 1.6, 4.9,  7.2,  6.3,   0,0,0,3.7, 6.1 ,  !LP10
     &                0.0, 0.4, 1.7, 2.2,  5.9,  9.9,   0,0,0,1.5, 14.2,  !LP11
     &                0.4, 0.6, 1.5, 14.8, 24.1, 24.8,  0,0,0,2.7, 12.9,  !LP12
     &                0.3, 0.7, 3.9, 4.7,  10.4, 15.4,4.6,0,0,3.4, 10.8,  !LP13
     &                0.3, 0.4, 0.9, 0.6,  0.5,  0.0,   0,0,0,2.0, 22.3,  !QA01
     &                0.0, 0.2, 1.3, 1.3,  1.2,  0.0,   0,0,0,0.2, 3.3 ,  !QA02
     &                0.2, 0.6, 1.5, 1.3,  1.2,  0.0,   0,0,0,0.9, 0.0 ,  !QA03
     &                0.5, 0.9, 1.5, 0.8,  0.8,  0.3,   0,0,0,1.7, 20.8,  !QA04
     &                0.2, 0.8, 1.9, 1.1,  1.2,  0.3,   0,0,0,1.7, 22.2,  !QA05
     &                0.1, 0.3, 1.5, 3.5,  3.6,  0.2,   0,0,0,1.2, 17.0,  !QA06
     &                0.1, 0.4, 5.0, 2.1,  2.0,  0.0,   0,0,0,0.8, 5.6 ,  !QA07
     &                0.1, 0.5, 2.7, 4.5,  4.7,  0.4,   0,0,0,2.2, 10.9,  !QA08
     &                0.3, 0.8, 3.7, 4.7,  4.8,  0.2,   0,0,0,1.6, 18.9,  !QA09
     &                0.0, 0.3, 3.7, 5.8,  5.7,  0.0,   0,0,0,1.0, 13.7,  !QA10
     &                0.1, 0.4, 3.8, 6.8,  6.7,  0.0,   0,0,0,0.8, 14.7,  !QA11
     &                0.1, 0.5, 3.2, 7.9,  8.3,  1.0,   0,0,0,1.0, 38.7,  !QA12
     &                0.2, 0.6, 3.7, 5.9,  9.7,  10.4,  0,0,0,1.0, 24.3/  !QA13

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF17VLS / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO01
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO03
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0,  0,0,0,  !GO04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO05
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO06
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO07
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !GO08
     &                 0.0, 0.0, 0.0, 0.2, 0.4, 0.0,  0,0,0,  !GO09
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !LP01
     &                 0.0, 0.0, 0.0, 0.4, 0.4, 0.0,  0,0,0,  !LP02
     &                 0.0, 0.0, 0.0, 0.9, 1.0, 0.7,  0,0,0,  !LP03
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0,  0,0,0,  !LP04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !LP05
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0,  0,0,0,  !LP06
     &                 0.0, 0.0, 0.0, 0.2, 0.1, 0.0,  0,0,0,  !LP07
     &                 0.0, 0.0, 0.0, 0.4, 0.5, 0.4,  0,0,0,  !LP08
     &                 0.0, 0.0, 0.0, 0.4, 0.3, 0.0,  0,0,0,  !LP09
     &                 0.0, 0.0, 0.0, 1.7, 2.2, 1.7,  0,0,0,  !LP10
     &                 0.0, 0.0, 0.0, 2.7, 8.0,14.2,  0,0,0,  !LP11
     &                 0.0, 0.0, 0.0, 1.5, 2.1, 2.0,  0,0,0,  !LP12
     &                 0.0, 0.0, 0.0, 4.4, 8.8,11.9,7.5,0,0,  !LP13
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0,  0,0,0,  !QA01
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !QA02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !QA03
     &                 0.0, 0.0, 0.0, 0.1, 0.2, 0.2,  0,0,0,  !QA04
     &                 0.0, 0.0, 0.0, 0.4, 0.4, 0.0,  0,0,0,  !QA05
     &                 0.0, 0.0, 0.0, 0.2, 0.1, 0.0,  0,0,0,  !QA06
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0,  0,0,0,  !QA07
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  0,0,0,  !QA08
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0,  0,0,0,  !QA09
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0,  0,0,0,  !QA10
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0,  0,0,0,  !QA11
     &                 0.0, 0.0, 0.0, 1.1, 1.0, 0.0,  0,0,0,  !QA12
     &                 0.0, 0.0, 0.0, 2.4, 2.6, 0.7,  0,0,0/  !QA13

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF18VLH / 0.0, 0.1, 0.0, 0.0,  0.0,  0.0,  0,0,0,0.1, 1.6 , !JP01
     &                0.1, 0.1, 0.4, 0.3,  0.3,  0.0,  0,0,0,0.1, 1.7 , !JP02
     &                0.1, 0.2, 0.1, 0.2,  0.2,  0.0,  0,0,0,1.0, 10.9, !JP03
     &                0.3, 0.2, 0.1, 0.2,  0.2,  0.0,  0,0,0,1.4, 3.2 , !JP04
     &                1.3, 0.5, 0.5, 1.7,  2.7,  2.8,  0,0,0,1.0, 9.3 , !JP05
     &                1.0, 1.6, 1.9, 5.5,  5.4,  0.0,  0,0,0,1.1, 2.7 , !JP06
     &                0.2, 0.4, 0.9, 4.2,  4.2,  0.0,  0,0,0,2.2, 19.3, !JP07
     &                0.7, 1.2, 1.8, 3.1,  3.1,  0.3,  0,0,0,0.5, 12.0, !JP08
     &                0.4, 0.8, 1.4, 1.9,  2.6,  1.7,  0,0,0,0.8, 10.0, !JP09
     &                0.5, 0.5, 0.5, 2.5,  2.4,  0.0,  0,0,0,1.1, 4.3 , !JP10
     &                0.1, 0.1, 0.1, 0.0,  0.0,  0.0,  0,0,0,2.4, 2.2 , !JP11
     &                0.2, 1.4, 4.3, 15.4, 15.4, 0.0,  0,0,0,2.8, 3.6 , !JP12
     &                0.2, 0.2, 1.3, 0.3,  0.2,  0.0,  0,0,0,4.3, 10.0, !JP13
     &                0.1, 0.1, 0.1, 0.2,  0.1,  0.0,  0,0,0,2.3, 8.5 , !JP14
     &                0.6, 0.6, 0.7, 0.4,  0.4,  0.0,  0,0,0,1.1, 5.4 , !JP15
     &                0.4, 0.7, 0.9, 2.6,  3.0,  1.2,  0,0,0,3.7, 18.3, !JP16
     &                0.3, 0.3, 0.1, 0.2,  0.2,  0.0,  0,0,0,2.8, 15.7, !JP17
     &                0.5, 0.4, 1.1, 1.1,  1.1,  0.0,  0,0,0,1.6, 6.6 , !JP18
     &                0.4, 0.4, 0.4, 0.6,  0.6,  0.0,  0,0,0,2.4, 16.9, !JP19
     &                0.6, 1.0, 1.1, 0.5,  0.4,  0.0,  0,0,0,6.4, 13.4, !MO01
     &                0.6, 0.9, 1.5, 0.2,  0.2,  0.0,  0,0,0,5.0, 15.1, !MO02
     &                0.7, 1.2, 1.1, 0.7,  0.7,  0.0,  0,0,0,4.5, 13.5, !MO03
     &                0.4, 0.7, 1.4, 0.9,  1.4,  1.3,  0,0,0,6.6, 13.6, !MO04
     &                0.5, 0.8, 1.6, 0.7,  1.3,  1.7,  0,0,0,5.0, 8.0 , !MO05
     &                1.4, 2.3, 2.2, 0.6,  0.5,  0.0,  0,0,0,8.3, 0.0 , !MO06
     &                1.0, 1.6, 1.6, 3.3,  3.6,  0.9,  0,0,0,5.4, 23.9, !MO07
     &                0.8, 1.4, 1.5, 3.6,  4.7,  3.2,  0,0,0,5.1, 6.2 , !MO08
     &                0.6, 1.1, 2.4, 4.1,  5.5,  4.1,  0,0,0,5.3, 13.2, !MO09
     &                1.5, 2.5, 3.0, 2.6,  3.9,  6.4,  0,0,0,7.2, 0.0 , !MO10
     &                0.5, 0.9, 2.4, 5.7,  9.0,  9.0,  0,0,0,5.4, 9.5 , !MO11
     &                0.3, 0.6, 0.5, 0.0,  0.0,  0.0,  0,0,0,1.6, 14.3, !MP01
     &                0.2, 0.3, 0.4, 0.2,  0.1,  0.0,  0,0,0,1.7, 11.7, !MP02
     &                0.1, 0.2, 0.1, 0.5,  0.5,  0.0,  0,0,0,2.6, 11.8, !MP03
     &                0.2, 0.4, 0.9, 0.2,  0.1,  0.0,  0,0,0,2.1, 7.8 , !MP04
     &                0.1, 0.2, 0.6, 0.2,  0.5,  1.0,  0,0,0,1.7, 10.4, !MP05
     &                0.5, 0.8, 1.3, 0.1,  0.1,  0.0,  0,0,0,2.1, 10.5, !MP06
     &                0.3, 0.5, 1.8, 1.3,  1.3,  0.3,  0,0,0,1.8, 8.5 , !MP07
     &                0.2, 0.3, 1.7, 1.0,  1.1,  0.3,  0,0,0,1.6, 10.8, !MP08
     &                0.6, 1.0, 1.9, 0.3,  0.2,  0.0,  0,0,0,2.0, 8.2 , !MP09
     &                0.4, 0.7, 1.3, 2.3,  2.5,  0.5,  0,0,0,1.3, 28.3, !MP10
     &                0.5, 0.9, 1.4, 1.2,  2.2,  2.8,  0,0,0,2.1, 16.2, !MP11
     &                0.9, 1.5, 1.9, 2.4,  3.8,  3.7,  0,0,0,2.3, 13.8, !MP12
     &                0.7, 1.1, 1.2, 1.0,  7.6, 17.9,  0,0,0,2.6, 7.2 / !MP13

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF18VLS / 0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0,0,0, !JP01
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, !JP02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, !JP03
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, !JP04
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0, 0,0,0, !JP05
     &                 0.0, 0.0, 0.0, 1.1, 1.0, 0.0, 0,0,0, !JP06
     &                 0.0, 0.0, 0.0, 0.9, 0.8, 0.0, 0,0,0, !JP07
     &                 0.0, 0.0, 0.0, 0.0, 0.1, 0.0, 0,0,0, !JP08
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,0,0, !JP09
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, !JP10
     &                 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0,0,0, !JP11
     &                 0.0, 0.0, 0.0, 0.8, 0.8, 0.0, 0,0,0, !JP12
     &                 0.0, 0.0, 0.0, 1.4, 1.4, 0.0, 0,0,0, !JP13
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0, 0,0,0, !JP14
     &                 0.0, 0.0, 0.0, 0.4, 0.3, 0.0, 0,0,0, !JP15
     &                 0.0, 0.0, 0.0, 2.0, 2.3, 1.2, 0,0,0, !JP16
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, !JP17
     &                 0.0, 0.0, 0.0, 1.1, 1.1, 0.0, 0,0,0, !JP18
     &                 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0,0,0, !JP19
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0, !MO01
     &                 0.0, 0.0, 0.0, 0.4, 0.5, 0.1, 0,0,0, !MO02
     &                 0.0, 0.0, 0.0, 0.8, 0.8, 0.2, 0,0,0, !MO03
     &                 0.0, 0.0, 0.0, 0.1, 0.2, 0.3, 0,0,0, !MO04
     &                 0.0, 0.0, 0.0, 0.6, 0.5, 0.0, 0,0,0, !MO05
     &                 0.0, 0.0, 0.0, 0.1, 0.2, 0.5, 0,0,0, !MO06
     &                 0.0, 0.0, 0.0, 1.0, 0.9, 0.0, 0,0,0, !MO07
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0, 0,0,0, !MO08
     &                 0.0, 0.0, 0.0, 0.6, 0.6, 0.0, 0,0,0, !MO09
     &                 0.0, 0.0, 0.0, 1.2, 1.1, 0.0, 0,0,0, !MO10
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, !MO11
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0, !MP01
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0, 0,0,0, !MP02
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0, 0,0,0, !MP03
     &                 0.0, 0.0, 0.0, 0.3, 0.4, 0.2, 0,0,0, !MP04
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, !MP05
     &                 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0,0,0, !MP06
     &                 0.0, 0.0, 0.0, 0.8, 0.7, 0.0, 0,0,0, !MP07
     &                 0.0, 0.0, 0.0, 0.8, 1.0, 0.8, 0,0,0, !MP08
     &                 0.0, 0.0, 0.0, 1.9, 2.2, 1.1, 0,0,0, !MP09
     &                 0.0, 0.0, 0.0, 1.3, 2.0, 2.0, 0,0,0, !MP10
     &                 0.0, 0.0, 0.0, 1.2, 2.2, 2.5, 0,0,0, !MP11
     &                 0.0, 0.0, 0.0, 0.4, 0.5, 0.3, 0,0,0, !MP12
     &                 0.0, 0.0, 0.0, 1.2, 2.4, 3.1, 0,0,0 / !MP13

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF19VLH / 0.6, 0.9, 2.3, 1.9, 8.0, 0.0, 0,0,0,1.4, 4.9 , ! HP01
     &                0.4, 0.7, 1.0, 1.0, 1.0, 0.0, 0,0,0,2.2, 6.6 , ! HP02
     &                0.5, 0.9, 1.3, 0.8, 0.7, 0.0, 0,0,0,2.2, 3.0 , ! HP03
     &                0.3, 0.5, 1.1, 0.8, 0.7, 0.0, 0,0,0,1.3, 9.8 , ! HP04
     &                0.3, 0.5, 0.9, 0.2, 0.1, 0.0, 0,0,0,1.4, 7.5 , ! HP05
     &                0.3, 0.5, 0.9, 0.4, 0.3, 0.0, 0,0,0,1.0, 8.6 , ! HP06
     &                0.3, 0.6, 1.0, 1.2, 5.2, 0.0, 0,0,0,1.5, 7.7 , ! HP07
     &                0.1, 0.2, 0.4, 0.0, 0.0, 0.0, 0,0,0,0.2, 2.0 , ! LLP01
     &                0.1, 0.4, 0.2, 0.8, 0.0, 0.0, 0,0,0,0.4, 3.3 , ! LLP02
     &                0.1, 0.2, 0.1, 1.0, 0.0, 0.0, 0,0,0,0.9, 0.3 , ! LLP03
     &                0.1, 0.2, 0.2, 0.1, 0.0, 0.0, 0,0,0,1.0, 0.0 , ! LLP04
     &                0.1, 0.4, 0.1, 0.3, 0.0, 0.0, 0,0,0,1.8, 16.9, ! LLP05
     &                0.2, 0.4, 0.5, 0.0, 0.0, 0.0, 0,0,0,0.9, 7.1 , ! LLP06
     &                0.2, 0.4, 0.4, 1.3, 0.0, 0.0, 0,0,0,1.0, 4.1 , ! LLP07
     &                0.0, 0.3, 0.2, 2.3, 0.0, 0.0, 0,0,0,0.7, 3.3 , ! LLP08
     &                0.1, 0.9, 0.2, 0.0, 0.0, 0.0, 0,0,0,6.0, 24.0, ! LLP09
     &                0.0, 0.3, 0.0, 0.0, 0.0, 0.0, 0,0,0,2.6, 24.6, ! LLP10
     &                0.8, 1.4, 1.4, 1.5, 0.0, 0.0, 0,0,0,3.1, -1.0, ! PW01
     &                0.8, 1.4, 1.3, 5.3, 0.0, 0.0, 0,0,0,3.0, -1.0, ! PW02
     &                0.1, 0.1, 0.0, 0.3, 0.3, 0.0, 0,0,0,0.0, 0.0 , ! SH01
     &                0.8, 1.4, 4.8, 0.3, 0.2, 0.0, 0,0,0,2.1, 2.6 , ! SH02
     &                0.2, 0.4, 0.3, 0.1, 0.0, 0.0, 0,0,0,3.4, 1.1 , ! SH03
     &                0.3, 0.6, 0.7, 0.8, 0.7, 0.0, 0,0,0,5.0, 10.1, ! SH04
     &                0.2, 0.7, 1.5, 0.1, 0.0, 0.0, 0,0,0,1.7, 0.0 , ! SH05
     &                0.1, 0.4, 0.2, 0.0, 0.0, 0.0, 0,0,0,1.5, 0.1 , ! SH06
     &                0.2, 0.4, 1.9, 1.5, 3.8, 0.0, 0,0,0,0.9, 0.3 , ! SH07
     &                0.1, 0.2, 0.2, 0.0, 0.0, 0.0, 0,0,0,2.9, 6.1 , ! SH08
     &                0.2, 0.7, 1.0, 0.1, 0.0, 0.0, 0,0,0,3.6, 0.2 , ! SH09
     &                0.0, 0.3, 0.4, 0.0, 0.0, 0.0, 0,0,0,2.0, 0.5 , ! SH10
     &                0.2, 0.5, 0.3, 0.1, 0.0, 0.0, 0,0,0,1.9, 2.2 , ! SH11
     &                0.3, 0.1, 0.1, 0.0, 0.0, 0.0, 0,0,0,3.8, 5.6 , ! SPS01
     &                0.6, 0.7, 0.8, 2.1, 0.0, 0.0, 0,0,0,3.6, 20.7, ! SPS02
     &                0.8, 0.6, 0.4, 0.0, 0.0, 0.0, 0,0,0,5.3, 23.9, ! SPS03
     &                0.8, 1.0, 0.7, 0.3, 0.0, 0.0, 0,0,0,3.3, 19.6 / !SPS04

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF19VLS / 0.0, 0.0, 0.0, 0.6, 0.5, 0.0, 0,0,0, ! HP01
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, ! HP02
     &                 0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0,0,0, ! HP03
     &                 0.0, 0.0, 0.0, 0.7, 0.9, 0.0, 0,0,0, ! HP04
     &                 0.0, 0.0, 0.0, 0.5, 0.4, 0.0, 0,0,0, ! HP05
     &                 0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,0,0, ! HP06
     &                 0.0, 0.0, 0.0, 0.6, 2.0, 0.0, 0,0,0, ! HP07
     &                 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0,0,0, ! LLP01
     &                 0.0, 0.0, 0.1, 0.5, 0.0, 0.0, 0,0,0, ! LLP02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! LLP03
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! LLP04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! LLP05
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! LLP06
     &                 0.0, 0.0, 0.1, 0.3, 0.0, 0.0, 0,0,0, ! LLP07
     &                 0.0, 0.0, 0.1, 0.1, 0.0, 0.0, 0,0,0, ! LLP08
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! LLP09
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! LLP10
     &                 0.0, 0.0, 0.0, 2.7, 0.0, 0.0, 0,0,0, ! PW01
     &                 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0,0,0, ! PW02
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! SH01
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0, ! SH02
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! SH03
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! SH04
     &                 0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0,0,0, ! SH05
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! SH06
     &                 0.0, 0.0, 0.0, 0.8, 0.8, 0.0, 0,0,0, ! SH07
     &                 0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0,0,0, ! SH08
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! SH09
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! SH10
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! SH11
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! SPS01
     &                 0.0, 0.0, 0.0, 0.6, 0.0, 0.0, 0,0,0, ! SPS02
     &                 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0,0,0, ! SPS03
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0 / !SPS04

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF20VLH / 0.0, 0.3, 1.5, 2.1, 2.1, 0.0,  0,0,0,-1.0, 1.4  , ! 1PP1TH
     &                0.1, 1.2, 3.3, 2.6, 2.7, 0.3,  0,0,0,-1.0, 15.0 , ! 2PP1TH
     &                0.1, 2.3, 3.2, 5.0, 5.0, 0.0,  0,0,0,-1.0, 5.1  , ! 3PP1TH
     &                0.2, 3.1, 3.9, 3.3, 3.5, 0.5,  0,0,0,-1.0, 19.2 , ! 4PP1TH
     &                0.3, 2.4, 8.3, 3.6, 3.6, 0.0,  0,0,0,-1.0, 14.3 , ! 5PP1TH
     &                0.2, 3.8, 3.9, 4.8, 5.0, 0.4,  0,0,0,-1.0, 25.7 , ! 6PP1TH
     &                0.6, 10.2,21.5,7.0, 7.0, 0.0,  0,0,0,-1.0, 32.9 , ! 7PP1TH
     &                0.1, 1.5, 2.4, 0.1, 0.1, 0.0,  0,0,0,-1.0, 12.0 , ! 1PP2PC
     &                0.1, 1.4, 2.8, 1.2, 1.2, 0.0,  0,0,0,-1.0, 9.6  , ! 2PP2PC
     &                0.1, 2.0, 3.2, 1.5, 1.5, 0.0,  0,0,0,-1.0, 7.2  , ! 3PP2PC
     &                0.1, 2.7, 4.3, 1.2, 1.5, 0.7,  0,0,0,-1.0, 14.4 , ! 4PP2PC
     &                0.4, 7.6, 4.6, 4.0, 4.4, 1.1,  0,0,0,-1.0, 24.0 , ! 5PP2PC
     &                0.1, 0.7, 1.9, 0.9, 1.1, 0.6,  0,0,0,-1.0, 6.0  , ! 1PP3PC
     &                0.3, 3.3, 3.7, 2.5, 2.8, 0.6,  0,0,0,-1.0, 14.4 , ! 2PP3PC
     &                0.2, 3.1, 4.0, 2.4, 3.0, 1.6,  0,0,0,-1.0, 22.9 , ! 3PP3PC
     &                0.1, 1.2, 3.0, 2.3, 2.4, 0.4,  0,0,0,-1.0, 9.6  , ! 1PP3CC
     &                0.3, 2.5, 6.9, 4.9, 5.4, 1.2,  0,0,0,-1.0, 16.8 , ! 2PP3CC
     &                0.6, 3.3, 4.6, 1.7, 2.0, 0.8,  0,0,0,-1.0, 14.4 , ! 1PPSP3PC
     &                0.6, 5.5, 8.5, 4.6, 4.8, 0.6,  0,0,0,-1.0, 25.2 , ! 2PPSP3PC
     &                0.8, 4.9, 7.5, 5.4, 6.7, 3.5,  0,0,0,-1.0, 41.5 , ! 3PPSP3PC
     &                0.6, 3.2, 5.0, 1.6, 2.6, 2.6,  0,0,0,-1.0, 20.4 , ! 1SP3PC
     &                0.9, 4.6, 7.9, 7.1, 8.7, 4.2,  0,0,0,-1.0, 44.3 , ! 2SP3PC
     &                0.1, 0.5, 0.6, 1.1, 1.1, 0.0,  0,0,0,-1.0, 19.2 , ! 1PP1
     &                0.1, 1.0, 1.1, 0.5, 0.5, 0.0,  0,0,0,-1.0, 14.4 , ! 1PP2
     &                0.1, 0.3, 0.4, 0.8, 1.4, 1.6,3.6,0,0,-1.0, 13.2 , ! 2PP2
     &                0.0, 0.5, 1.2, 1.1, 1.1, 0.0,  0,0,0,-1.0, 20.4 / ! 1PP3

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF20VLS / 0.0, 0.0, 0.0, 0.2, 0.2, 0.0, 0,0,0, ! 1PP1TH
     &                 0.0, 0.0, 0.0, 0.6, 0.6, 0.1, 0,0,0, ! 2PP1TH
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0, ! 3PP1TH
     &                 0.0, 0.0, 0.0, 1.2, 1.2, 0.2, 0,0,0, ! 4PP1TH
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 5PP1TH
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, ! 6PP1TH
     &                 0.0, 0.0, 0.0, 0.4, 0.4, 0.0, 0,0,0, ! 7PP1TH
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 1PP2PC
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0, ! 2PP2PC
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 3PP2PC
     &                 0.0, 0.0, 0.0, 0.5, 0.6, 0.3, 0,0,0, ! 4PP2PC
     &                 0.0, 0.0, 0.0, 1.2, 1.3, 0.3, 0,0,0, ! 5PP2PC
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 1PP3PC
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.1, 0,0,0, ! 2PP3PC
     &                 0.0, 0.0, 0.0, 0.7, 0.9, 0.5, 0,0,0, ! 3PP3PC
     &                 0.0, 0.0, 0.0, 0.5, 0.5, 0.1, 0,0,0, ! 1PP3CC
     &                 0.0, 0.0, 0.0, 0.8, 0.9, 0.2, 0,0,0, ! 2PP3CC
     &                 0.0, 0.0, 0.0, 0.9, 1.0, 0.4, 0,0,0, ! 1PPSP3PC
     &                 0.0, 0.0, 0.0, 0.3, 0.4, 0.0, 0,0,0, ! 2PPSP3PC
     &                 0.0, 0.0, 0.0, 2.2, 2.7, 1.4, 0,0,0, ! 3PPSP3PC
     &                 0.0, 0.0, 0.0, 0.8, 1.3, 1.3, 0,0,0, ! 1SP3PC
     &                 0.0, 0.0, 0.0, 2.6, 3.2, 1.6, 0,0,0, ! 2SP3PC
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, ! 1PP1
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0, ! 1PP2
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 2PP2
     &                 0.0, 0.0, 0.0, 0.3, 0.3, 0.0, 0,0,0 / ! 1PP3

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF21VLH / 0.2, 0.4, 0.3, 0.0, 0.0, 0.0, 0,0,0,1.2, -1.0,  ! PJ01
     &                0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 0,0,0,2.0, -1.0,  ! PJ02
     &                0.2, 0.4, 0.0, 0.2, 0.2, 0.0, 0,0,0,2.5, -1.0,  ! PJ03
     &                0.0, 0.6, 1.4, 0.2, 0.1, 0.0, 0,0,0,3.1, -1.0,  ! PJ04
     &                0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0,0,0,3.4, -1.0,  ! PJ05
     &                0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0,0,0,8.1, -1.0,  ! PJ06
     &                0.2, 0.4, 1.0, 0.1, 0.1, 0.0, 0,0,0,3.1, -1.0,  ! PJ07
     &                0.1, 0.0, 0.3, 0.0, 0.0, 0.0, 0,0,0,9.6, -1.0,  ! PJ08
     &                0.3, 0.5, 2.2, 0.8, 2.6, 0.0, 0,0,0,1.7, -1.0,  ! PJ09
     &                0.1, 0.9, 1.8, 1.9, 2.4, 0.0, 0,0,0,4.0, -1.0,  ! PJ10
     &                0.1, 0.3, 1.9, 2.0, 4.1, 0.0, 0,0,0,4.6, -1.0,  ! PJ11
     &                0.0, 0.3, 1.0, 0.9, 1.8, 0.0, 0,0,0,3.2, -1.0,  ! PJ12
     &                0.1, 0.4, 0.9, 0.6, 0.5, 0.0, 0,0,0,4.5, -1.0,  ! PJ13
     &                0.2, 0.7, 2.0, 1.0, 1.5, 0.0, 0,0,0,9.3, -1.0,  ! PJ14
     &                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.2, -1.0,  ! SWSB01
     &                0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.3, -1.0,  ! SWSB02
     &                0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.3, -1.0,  ! SWSB03
     &                0.1, 0.3, 0.1, 0.0, 0.0, 0.0, 0,0,0,0.5, -1.0,  ! SWSB04
     &                0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.4, -1.0,  ! SWSB05
     &                0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.6, -1.0,  ! SWSB06
     &                0.2, 0.7, 0.9, 0.0, 0.0, 0.0, 0,0,0,0.2, -1.0,  ! SWSB07
     &                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.1, -1.0,  ! SWSB08
     &                0.2, 0.3, 1.2, 0.0, 0.0, 0.0, 0,0,0,0.9, -1.0,  ! SWSB09
     &                0.2, 0.3, 1.5, 0.0, 0.0, 0.0, 0,0,0,4.0, -1.0,  ! SWSB10
     &                0.2, 0.6, 2.4, 0.0, 0.0, 0.0, 0,0,0,4.9, -1.0 / ! SWSB11

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF21VLS / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ01
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ03
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0,  ! PJ04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ05
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ06
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ07
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! PJ08
     &                 0.0, 0.0, 0.0, 0.3, 0.6, 0.0, 0,0,0,  ! PJ09
     &                 0.0, 0.0, 0.0, 0.1, 0.1, 0.0, 0,0,0,  ! PJ10
     &                 0.0, 0.0, 0.0, 0.3, 0.2, 0.0, 0,0,0,  ! PJ11
     &                 0.0, 0.0, 0.0, 0.2, 2.0, 0.0, 0,0,0,  ! PJ12
     &                 0.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0,0,0,  ! PJ13
     &                 0.0, 0.0, 0.0, 0.5, 1.5, 0.0, 0,0,0,  ! PJ14
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB01
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB03
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB05
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB06
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB07
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB08
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB09
     &                 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0,0,0,  ! SWSB10
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0/ ! SWSB11

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF22VLH / 0.0, 0.0, 0.2, 0.4,  0.0, 0.0, 0,0,0,5.4,  0.0  , ! HIF01
     &                0.9, 1.4, 4.5, 0.5,  0.0, 0.0, 0,0,0,5.9,  0.0  , ! HIF02
     &                0.8, 3.3, 4.2, 4.7,  0.0, 0.0, 0,0,0,5.9,  12.9 , ! HIF03
     &                0.6, 1.4, 1.8, 2.6,  0.0, 0.0, 0,0,0,9.2,  4.5  , ! HIF04
     &                0.2, 1.0, 3.1, 39.0, 0.0, 0.0, 0,0,0,4.9,  17.0 , ! HIF05
     &                0.2, 0.9, 0.6, 1.4,  0.0, 0.0, 0,0,0,6.2,  50.8 , ! HIF06
     &                0.0, 0.4, 0.5, 4.5,  0.0, 0.0, 0,0,0,3.5,  16.3 , ! HIF07
     &                0.0, 1.2, 1.9, 6.2,  0.0, 0.0, 0,0,0,10.2, 19.9 , ! HIF08
     &                0.0, 1.4, 1.1, 1.0,  0.0, 0.0, 0,0,0,5.8,  7.7  , ! HIF09
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.0,  0.0  , ! HIG01
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.2,  0.0  , ! HIG02
     &                0.4, 0.3, 0.0, 0.0,  0.0, 0.0, 0,0,0,1.8,  0.0  , ! HIG03
     &                0.0, 0.1, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.9,  0.0  , ! HIG04
     &                0.0, 0.3, 0.3, 0.0,  0.0, 0.0, 0,0,0,0.5,  0.0  , ! HIG05
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.4,  0.0  , ! HIG06
     &                0.0, 0.0, 0.2, 0.0,  0.0, 0.0, 0,0,0,2.2,  0.0  , ! HIG07
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,2.1,  0.0  , ! HIG08
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,3.4,  0.0  , ! HIG09
     &                0.0, 0.2, 0.5, 0.0,  0.0, 0.0, 0,0,0,3.2,  0.0  , ! HIG10
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,2.5,  0.0  , ! HIG11
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.0,  0.0  , ! HIG12
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.0,  0.0  , ! HIG13
     &                0.2, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.2,  0.0  , ! HIS01
     &                0.0, 0.3, 0.0, 0.0,  0.0, 0.0, 0,0,0,0.3,  0.0  , ! HIS02
     &                1.1, 2.5, 0.6, 0.0,  0.0, 0.0, 0,0,0,1.5,  0.0  , ! HIS03
     &                0.2, 0.5, 0.2, 0.0,  0.0, 0.0, 0,0,0,1.4,  0.0  , ! HIS04
     &                0.2, 0.3, 0.1, 0.0,  0.0, 0.0, 0,0,0,1.7,  0.0  , ! HIS05
     &                0.5, 0.4, 0.0, 0.0,  0.0, 0.0, 0,0,0,4.6,  0.8  , ! HIS06
     &                1.0, 1.2, 2.4, 1.2,  0.0, 0.0, 0,0,0,4.0,  0.0  , ! HIS07
     &                2.2, 0.7, 0.2, 3.8,  0.0, 0.0, 0,0,0,1.3,  0.0  , ! HIW01
     &                1.0, 0.6, 1.3, 0.2,  0.0, 0.0, 0,0,0,2.0,  0.0  , ! HIW02
     &                0.3, 0.6, 1.8, 2.3,  0.0, 0.0, 0,0,0,4.4,  0.0  , ! HIW03
     &                0.4, 0.5, 0.8, 0.9,  0.0, 0.0, 0,0,0,2.3,  0.0  , ! HIW04
     &                0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0,0,0,3.3,  0.0  , ! HIW05
     &                0.2, 0.6, 1.0, 2.9,  0.0, 0.0, 0,0,0,5.1,  0.0  , ! HIW06
     &                0.5, 1.7, 0.2, 0.0,  0.0, 0.0, 0,0,0,3.6,  0.0  / ! HIW07

      DATA REF22VLS /  324*0.0 /

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF23VLT / 1.0,1.7,4.7, 3.5, 3.6,  0.0,    0,0,0,-1.0, 22.4 , ! 1DFWHPRE01
     &                1.1,1.9,5.5, 2.4, 3.8,  3.7,  2.3,0,0,-1.0, 3.7  , ! 1DFWHPRE02
     &                2.2,3.6,7.4, 5.3, 5.9,  1.5,  2.0,0,0,-1.0, 39.3 , ! 1DFWHPRE03
     &                1.8,3.0,5.0, 6.1, 9.0,  7.9,    0,0,0,-1.0, 0.7  , ! 1DFWHPRE04
     &                2.0,3.3,6.6, 5.9, 8.8,  7.8,    0,0,0,-1.0, 24.3 , ! 1DFWHPRE05
     &                3.0,5.1,8.1, 9.5, 9.5,  0.0,    0,0,0,-1.0, 29.9 , ! 1DFWHPRE06
     &                2.6,4.3,12.1,6.3, 7.6,  3.2,    0,0,0,-1.0, 31.8 , ! 1DFWHPRE07
     &                2.3,3.8,6.2, 9.8, 11.5, 5.6,    0,0,0,-1.0, 82.3 , ! 1DFWHPRE08
     &                1.3,2.2,6.0, 7.4, 9.4,  5.5,  9.4,0,0,-1.0, 78.5 , ! 1DFWHPRE09
     &                2.5,4.1,10.1,7.8, 11.0, 8.5,    0,0,0,-1.0, 44.9 , ! 1DFWHPRE10
     &                1.3,2.1,4.6, 3.6, 9.5, 15.6,  7.7,0,0,-1.0, 46.8 , ! 1DFWHPRE11
     &                2.4,3.9,11.0,10.9,12.5, 4.2,    0,0,0,-1.0, 54.2 , ! 1DFWHPRE12
     &                1.7,2.8,5.9, 12.6,13.2, 1.5, 10.1,0,0,-1.0, 84.2 , ! 1DFWHPRE13
     &                3.0,5.0,9.0, 12.0,16.7,12.6,  6.7,0,0,-1.0, 72.9 , ! 1DFWHPRE14
     &                2.4,4.0,8.4, 9.2, 15.1,15.7, 30.0,0,0,-1.0, 41.1 , ! 1DFWHPRE15
     &                2.5,4.2,8.7, 4.2, 9.3, 13.6, 72.7,0,0,-1.0, 99.1 , ! 1DFWHPRE16
     &                1.7,2.9,7.4, 9.1, 16.8,22.6, 61.9,0,0,-1.0, 65.4 , ! 1DFWHPRE17
     &                1.1,1.8,4.1, 2.5, 6.9, 10.9,112.3,0,0,-1.0, 91.6 , ! 1DFWHPRE18
     &                1.0,1.6,3.7, 4.6, 10.6,16.9,115.3,0,0,-1.0, 74.8 , ! 1DFWHPRE19
     &                0.1,0.2,0.7, 3.6, 4.8,  2.8,    0,0,0,-1.0, 7.5  , ! 2DFWHPOST01
     &                0.3,0.6,3.0, 6.1, 8.4,  3.2,  3.0,0,0,-1.0, 74.8 , ! 2DFWHPOST02
     &                0.2,0.3,1.5, 5.6, 8.8,  8.3,    0,0,0,-1.0, 22.4 , ! 2DFWHPOST03
     &                0.0,0.1,1.2, 7.9, 11.0, 8.6,    0,0,0,-1.0, 20.6 , ! 2DFWHPOST04
     &                0.2,0.3,3.8, 7.8, 12.4,12.2,  4.7,0,0,-1.0, 56.1 , ! 2DFWHPOST05
     &                0.5,0.9,4.0, 3.4, 10.0,17.7, 64.0,0,0,-1.0, 104.7, ! 2DFWHPOST06
     &                0.2,0.4,2.0, 5.3, 8.5,  8.8, 80.6,0,0,-1.0, 48.6 / ! 2DFWHPOST07

C
      DATA PROPROT23 / 0.17, 0.34, 0.00, 0.20, 0.10,
     &                 0.18, 0.06, 0.08, 0.39, 0.21,
     &                 0.09, 0.17, 0.12, 0.01, 0.14,
     &                 0.27, 0.08, 0.15, 0.08, 0.11,
     &                 0.18, 0.01, 0.06, 0.30, 0.38,
     &                 0.18 /

      DO J = 1, 26
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF23VLH(I,J) = REF23VLT(I,J)
            REF23VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF23VLS(I,J) = REF23VLT(I,J) * PROPROT23(J)
            REF23VLH(I,J) = MAX(REF23VLT(I,J) - REF23VLS(I,J),0.0)
          ELSE
            REF23VLH(I,J) = REF23VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF24VLH / 0.9,  2.7, 1.3,  0.9,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 1LL2N
     &                0.5,  4.3, 2.5,  0.4,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 2LL2H
     &                0.6,  1.7, 2.0,  0.0,  4.2, 0.0,0,0,0,-1.0, -1.0 , ! 3LL3N
     &                0.4,  4.3, 3.7,  2.7,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 4LL2H
     &                0.9,  3.6, 6.2,  10.6, 0.0, 0.0,0,0,0,-1.0, -1.0 , ! 5LL1P
     &                1.2,  5.8, 10.9, 16.1, 0.0, 0.0,0,0,0,-1.0, -1.0 , ! 6LL3H
     &                0.2,  1.1, 5.5,  0.0,  30.3,0.0,0,0,0,-1.0, -1.0 , ! 7LL3H
     &                0.3,  1.1, 1.9,  0.0,  29.4,0.0,0,0,0,-1.0, -1.0 , ! 8LL3N
     &                1.4,  3.2, 6.1,  31.2, 0.0, 0.0,0,0,0,-1.0, -1.0 , ! 9LL3H
     &                0.2,  1.0, 1.3,  0.8,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 1WP3N
     &                0.2,  1.2, 2.2,  2.2,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 2WP2P
     &                0.1,  1.6, 2.3,  3.4,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 3WP3N
     &                0.3,  1.5, 3.9,  3.3,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 4WP3H
     &                0.6,  2.0, 7.9,  6.7,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 5WP3H
     &                0.3,  2.4, 7.6,  9.6,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 6WP2H
     &                0.1,  0.9, 4.4,  0.0,  23.6,0.0,0,0,0,-1.0, -1.0 , ! 7WP3N
     &                3.2,  0.3, 0.1,  0.1,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 1PP1N
     &                2.6,  0.2, 0.6,  0.3,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 2PP2N
     &                3.7,  0.2, 0.6,  0.0,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 3PP1N
     &                3.2,  0.4, 1.3,  1.3,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 4PP1N
     &                3.1,  0.4, 1.4,  2.1,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 5PP2N
     &                4.2,  1.3, 1.8,  0.0,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 6PP2N
     &                10.0, 2.8, 6.3,  2.1,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 7PP3H
     &                3.5,  0.8, 2.3,  5.5,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 1VP2N
     &                4.8,  1.6, 2.8,  2.1,  0.0, 0.0,0,0,0,-1.0, -1.0 , ! 2VP2N
     &                3.8,  1.1, 1.9,  0.0,  5.2, 0.0,0,0,0,-1.0, -1.0 , ! 3VP3N
     &                7.7,  1.9, 5.9,  0.2,  0.0, 0.0,0,0,0,-1.0, -1.0 / ! 4VP2N

C                 <.25  to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF24VLS /  0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! 1LL2N
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 2LL2H
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! 3LL3N
     &                 0.0, 0.0, 0.0, 1.6, 0.0, 0.0, 0,0,0, ! 4LL2H
     &                 0.0, 0.0, 0.0, 5.8, 0.0, 0.0, 0,0,0, ! 5LL1P
     &                 0.0, 0.0, 0.0, 1.4, 0.0, 0.0, 0,0,0, ! 6LL3H
     &                 0.0, 0.0, 0.0, 0.0, 3.6, 0.0, 0,0,0, ! 7LL3H
     &                 0.0, 0.0, 0.0, 0.0,11.0, 0.0, 0,0,0, ! 8LL3N
     &                 0.0, 0.0, 0.0, 0.0, 4.7, 0.0, 0,0,0, ! 9LL3H
     &                 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0,0,0, ! 1WP3N
     &                 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0,0,0, ! 2WP2P
     &                 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0,0,0, ! 3WP3N
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 4WP3H
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! 5WP3H
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! 6WP2H
     &                 0.0, 0.0, 0.0, 1.3, 0.0, 0.0, 0,0,0, ! 7WP3N
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 1PP1N
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 2PP2N
     &                 0.0, 0.0, 0.0, 1.6, 0.0, 0.0, 0,0,0, ! 3PP1N
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 4PP1N
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 5PP2N
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! 6PP2N
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! 7PP3H
     &                 0.0, 0.0, 0.0, 2.6, 0.0, 0.0, 0,0,0, ! 1VP2N
     &                 0.0, 0.0, 0.0, 3.6, 0.0, 0.0, 0,0,0, ! 2VP2N
     &                 0.0, 0.0, 0.0, 6.1, 0.0, 0.0, 0,0,0, ! 3VP3N
     &                 0.0, 0.0, 0.0, 5.2, 0.0, 0.0, 0,0,0 / ! 4VP2N

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF25VLT / 0.0, 1.6, 1.4, 3.7,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 1A21N
     &                0.0, 1.1, 4.3, 1.7,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 2A22N
     &                0.0, 0.4, 0.7, 6.2,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 3B21N
     &                0.0, 1.9, 3.0, 4.5,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 4A22N
     &                0.0, 1.0, 2.5, 5.9,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 5B12N
     &                0.0, 1.7, 4.8, 5.2,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 6A12N
     &                0.0, 0.8, 3.6, 7.9,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 7B22N
     &                0.0, 1.1, 5.1, 9.6,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 8A22N
     &                0.0, 1.7, 2.8, 12.1, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 9A11N
     &                0.0, 2.2, 7.5, 11.7, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 10A22CC
     &                0.0, 4.5, 0.5, 27.0, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 11B22CC
     &                0.0, 1.8, 8.8, 30.1, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 12A22CC
     &                0.0, 2.1, 8.6, 33.4, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 13A22CC
     &                0.0, 7.9, 1.8, 52.6, 0.0, 0.0, 0,0,0,-1.0, -1.0 / ! 14B23CC
C
      DATA PROPROT25 /  0.07, 0.04, 0.25, 0.01, 0.16,
     &                  0.10, 0.14, 0.04, 0.05, 0.09,
     &                  0.07, 0.05, 0.03, 0.01 /

      DO J = 1, 14
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF25VLH(I,J) = REF25VLT(I,J)
            REF25VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF25VLS(I,J) = REF25VLT(I,J) * PROPROT25(J)
            REF25VLH(I,J) = MAX(REF25VLT(I,J) - REF25VLS(I,J),0.0)
          ELSE
            REF25VLH(I,J) = REF25VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF26VLH / 0.2, 0.8, 0.5, 0.0, 0.0, 0.0, 0,0,0,4.8, -1.0 , ! FC1PRE
     &                0.2, 1.1, 0.2, 0.0, 0.0, 0.0, 0,0,0,1.8, -1.0 , ! FC1POST
     &                0.2, 0.7, 0.2, 0.0, 0.0, 0.0, 0,0,0,5.1, -1.0 , ! FC2PRE
     &                0.1, 0.9, 0.1, 0.0, 0.0, 0.0, 0,0,0,1.0, -1.0 , ! FC2POST
     &                0.2, 1.3, 1.8, 1.5, 0.0, 0.0, 0,0,0,5.5, -1.0 , ! FC3PRE
     &                0.2, 2.3, 1.4, 1.5, 0.0, 0.0, 0,0,0,2.9, -1.0 , ! FC3POST
     &                0.1, 1.2, 1.8, 2.1, 0.0, 0.0, 0,0,0,5.9, -1.0 , ! FC4PRE
     &                0.1, 2.0, 2.1, 2.1, 0.0, 0.0, 0,0,0,3.0, -1.0 , ! FC4POST
     &                0.2, 1.2, 1.5, 0.0, 0.0, 0.0, 0,0,0,4.5, -1.0 , ! FC5PRE
     &                0.2, 1.7, 1.9, 0.0, 0.0, 0.0, 0,0,0,1.7, -1.0 , ! FC5POST
     &                0.1, 1.0, 2.9, 1.8, 0.0, 0.0, 0,0,0,2.7, -1.0 , ! FC6PRE
     &                0.1, 1.3, 2.3, 1.8, 0.0, 0.0, 0,0,0,1.0, -1.0 , ! FC6POST
     &                0.8, 2.5, 4.3, 2.7, 0.0, 0.0, 0,0,0,5.0, -1.0 , ! FC7PRE
     &                0.4, 2.7, 2.7, 2.5, 0.0, 0.0, 0,0,0,2.0, -1.0 , ! FC7POST
     &                0.3, 1.1, 1.2, 0.0, 0.0, 0.0, 0,0,0,3.1, -1.0 , ! FC8PRE
     &                0.3, 0.7, 1.4, 0.0, 0.0, 0.0, 0,0,0,1.2, -1.0 / ! FC8POST

      DATA REF26VLS /  144*0.0 /

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF27VLH / 0.1, 0.3, 0.1, 0.0, 0.0, 0.0, 0,0,0,0.7, 0.6 , ! CDO01
     &                0.3, 0.2, 0.1, 0.7, 0.6, 0.0, 0,0,0,1.2, 0.4 , ! CDO02
     &                0.2, 0.2, 0.0, 0.3, 0.2, 0.0, 0,0,0,2.1, 0.8 , ! CDO03
     &                0.3, 0.5, 0.6, 0.4, 0.3, 0.0, 0,0,0,3.5, 1.1 , ! CDO04
     &                0.4, 0.6, 0.6, 0.0, 2.1, 0.0, 0,0,0,3.6, 0.3 , ! CDO05
     &                0.3, 0.4, 0.9, 0.8, 0.7, 0.0, 0,0,0,1.8, 0.3 , ! CDO06
     &                0.2, 0.3, 0.1, 0.0, 0.0, 0.0, 0,0,0,1.4, 0.8 , ! CDO07
     &                0.4, 0.5, 0.0, 0.3, 0.2, 0.0, 0,0,0,2.3, 0.8 , ! CDO08
     &                0.5, 0.8, 1.2, 0.1, 0.1, 0.0, 0,0,0,4.9, 2.7 , ! CDO09
     &                0.3, 0.7, 1.9, 5.0, 0.0, 0.0, 0,0,0,1.4, 4.3 , ! MCS01
     &                0.2, 0.8, 1.5, 0.4, 0.0, 0.0, 0,0,0,3.6, 2.2 , ! MCS02
     &                0.1, 1.0, 0.1, 0.0, 0.0, 0.0, 0,0,0,2.2, 3.1 , ! MCS03
     &                0.1, 0.4, 0.3, 0.0, 0.0, 0.0, 0,0,0,1.0, 0.0 , ! MCS04
     &                0.2, 0.4, 0.7, 1.1, 0.0, 0.0, 0,0,0,1.0, 0.1 , ! MCS05
     &                0.2, 0.2, 0.4, 1.6, 0.0, 0.0, 0,0,0,1.2, 1.7 , ! MCS06
     &                0.2, 0.4, 0.9, 1.4, 0.0, 0.0, 0,0,0,1.2, 3.0 , ! MCS07
     &                0.1, 1.9, 4.8, 0.9, 0.0, 0.0, 0,0,0,5.7, 4.5 , ! MCS08
     &                0.4, 0.4, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.8, 0.2 , ! MCS09
     &                0.3, 0.1, 0.0, 0.0, 0.0, 0.0, 0,0,0,1.7, 0.7 , ! MCS10
     &                0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0,0,0,4.0, 0.8 , ! MCS11
     &                0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.6, 0.0 , ! WO01
     &                0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0,0,0,0.6, 0.0 , ! WO02
     &                0.2, 0.2, 0.1, 0.0, 0.0, 0.0, 0,0,0,1.9, 0.0 , ! WO03
     &                0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,1.6, 0.1 , ! WO04
     &                0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0,2.2, 0.1 , ! WO05
     &                0.1, 0.3, 0.0, 0.1, 0.0, 0.0, 0,0,0,2.2, 3.5 , ! WO06
     &                0.4, 1.0, 1.4, 1.1, 1.1, 0.0, 0,0,0,6.4, 3.4 , ! WO07
     &                0.2, 0.2, 0.1, 0.3, 0.2, 0.0, 0,0,0,4.4, 0.0 , ! WO08
     &                0.2, 0.1, 0.6, 0.1, 0.0, 0.0, 0,0,0,0.8, 3.9 , ! WO09
     &                0.6, 1.4, 1.3, 0.5, 0.4, 0.0, 0,0,0,7.7, 4.7 / ! WO10

C                 <.25  to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF27VLS /  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! CDO01
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! CDO02
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! CDO03
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! CDO04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! CDO05
     &                 0.0, 0.0, 0.0, 0.4, 0.3, 0.0, 0,0,0, ! CDO06
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! CDO07
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! CDO08
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! CDO09
     &                 0.0, 0.0, 0.0, 2.4, 0.0, 0.0, 0,0,0, ! MCS01
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! MCS02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! MCS03
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! MCS04
     &                 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0,0,0, ! MCS05
     &                 0.0, 0.0, 0.0, 0.9, 0.0, 0.0, 0,0,0, ! MCS06
     &                 0.0, 0.0, 0.0, 0.3, 0.0, 0.0, 0,0,0, ! MCS07
     &                 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0,0,0, ! MCS08
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! MCS09
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! MCS10
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! MCS11
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO01
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO02
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO03
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO04
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO05
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO06
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! WO07
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO08
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! WO09
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0 / ! WO10

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF28VLT / 0.2, 2.6, 4.8,  2.2,  2.9, 2.0,    0,0,0,-1.,-1. , ! 1MC4RC
     &                0.4, 5.7, 11.8, 4.5,  5.2, 2.0,  6.9,0,0,-1.,-1. , ! 2MC4RC
     &                0.7, 9.6, 21.6, 6.9,  21.1,38.1,27.9,0,0,-1.,-1. , ! 3MC4RC
     &                0.2, 2.0, 3.8,  0.3,  0.3, 0.0,    0,0,0,-1.,-1. , ! 1MC4PC
     &                1.0, 4.1, 8.0,  4.8,  5.9, 2.8,    0,0,0,-1.,-1. , ! 2MC4PC
     &                0.0, 0.4, 3.1,  4.5,  7.8, 8.9,  4.0,0,0,-1.,-1. , ! 3MC4PC
     &                0.8, 4.0, 6.2,  7.4,  9.8, 6.5,  3.6,0,0,-1.,-1. , ! 4MC4PC
     &                0.5, 5.3, 12.0, 6.5,  9.4, 7.9,    0,0,0,-1.,-1. , ! 5MC4PC
     &                1.6, 8.7, 6.1,  10.3, 12.1,5.0,    0,0,0,-1.,-1. , ! 6MC4PC
     &                0.6, 6.7, 10.8, 1.9,  4.0, 5.8, 21.2,0,0,-1.,-1. , ! 7MC4PC
     &                1.0, 5.9, 7.9,  11.5, 13.4,5.3, 16.5,0,0,-1.,-1. , ! 8MC4PC
     &                0.2, 1.9, 0.8,  0.2,  0.5, 0.8,    0,0,0,-1.,-1. , ! 1MC3PC
     &                0.2, 2.8, 3.1,  2.5,  2.8, 0.9,    0,0,0,-1.,-1. , ! 2MC3PC
     &                0.4, 2.6, 5.1,  3.5,  3.5, 0.0,    0,0,0,-1.,-1. , ! 3MC3PC
     &                0.5, 4.4, 4.0,  3.1,  3.4, 1.0,    0,0,0,-1.,-1. , ! 4MC3PC
     &                0.3, 2.6, 4.4,  3.5,  5.0, 4.1,    0,0,0,-1.,-1. , ! 5MC3PC
     &                0.1, 2.3, 3.0,  5.5,  6.0, 1.5,  2.1,0,0,-1.,-1. , ! 6MC3PC
     &                0.8, 7.2, 7.5,  3.5,  4.3, 2.3,    0,0,0,-1.,-1. , ! 7MC3PC
     &                0.2, 1.8, 3.7,  6.7,  12.3,15.1,13.6,0,0,-1.,-1. , ! 8MC3PC
     &                0.2, 1.1, 2.9,  0.2,  0.2, 0.0,    0,0,0,-1.,-1. , ! 1TF4RC
     &                0.3, 1.4, 3.8,  0.7,  0.7, 0.0,    0,0,0,-1.,-1. , ! 2TF4RC
     &                0.3, 3.6, 12.1, 3.2,  4.3, 3.0,    0,0,0,-1.,-1. , ! 3TF4RC
     &                0.9, 5.3, 4.6,  7.9,  8.5, 1.5,    0,0,0,-1.,-1. , ! 4TF4RC
     &                0.2, 2.0, 4.8,  5.5,  8.3, 7.6,  3.1,0,0,-1.,-1. , ! 5TF4RC
     &                1.6, 6.3, 6.2,  8.1,  15.8,20.4,25.6,0,0,-1.,-1. , ! 6TF4RC
     &                0.5, 2.2, 6.3,  3.7,  4.0, 0.9,    0,0,0,-1.,-1. , ! 1TF4PC
     &                0.3, 2.1, 3.5,  4.5,  5.5, 2.8,  6.0,0,0,-1.,-1. , ! 2TF4PC
     &                1.2, 4.1, 6.6,  5.3,  8.8, 9.2,  3.8,0,0,-1.,-1. , ! 3TF4PC
     &                1.3, 5.8, 7.8,  9.6,  11.9,6.3,  6.4,0,0,-1.,-1. , ! 4TF4PC
     &                0.5, 4.7, 6.2,  10.0, 22.3,32.7,29.9,0,0,-1.,-1. / ! 5TF4PC

C
      DATA PROPROT28 /  0.09, 0.00, 0.00, 0.00, 0.04,
     &                  0.40, 0.08, 0.00, 0.22, 0.00,
     &                  0.27, 0.00, 0.15, 0.00, 0.21,
     &                  0.00, 0.55, 0.47, 0.21, 0.00,
     &                  0.00, 0.16, 0.17, 0.12, 0.48,
     &                  0.07, 0.28, 0.15, 0.02, 0.29/

      DO J = 1, 30
        DO I = 1, MXFLCL
          IF (I .LE. 3) THEN
            REF28VLH(I,J) = REF28VLT(I,J)
            REF28VLS(I,J) = 0.0
          ELSEIF (I .LE. 9) THEN
            REF28VLS(I,J) = REF28VLT(I,J) * PROPROT28(J)
            REF28VLH(I,J) = MAX(REF28VLT(I,J) - REF28VLS(I,J),0.0)
          ELSE
            REF28VLH(I,J) = REF28VLT(I,J)
          ENDIF
        ENDDO
      ENDDO

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF29VLH / 1.1, 2.7, 4.8,  6.7,  0.0, 0.0, 0,0,0,4.9,  11.9 , ! 6A
     &                0.1, 0.7, 2.2,  4.9,  0.0, 0.0, 0,0,0,0.0,  3.2  , ! 6B
     &                0.7, 1.8, 7.9,  6.9,  0.0, 0.0, 0,0,0,7.2,  14.9 , ! 8A
     &                0.0, 0.2, 1.8,  2.6,  0.0, 0.0, 0,0,0,0.0,  11.6 , ! 8B
     &                0.6, 1.9, 5.2,  14.4, 0.0, 0.0, 0,0,0,3.6,  10.8 , ! 10A
     &                0.1, 0.7, 1.2,  6.2,  0.0, 0.0, 0,0,0,0.2,  7.6  , ! 10B
     &                0.7, 2.5, 10.5, 13.6, 0.0, 0.0, 0,0,0,4.8,  13.5 , ! 12A
     &                0.1, 0.8, 2.2,  8.4,  0.0, 0.0, 0,0,0,0.0,  6.3  , ! 12B
     &                1.0, 5.8, 6.8,  17.0, 0.0, 0.0, 0,0,0,10.6, 18.0 , ! 14A
     &                0.0, 0.6, 2.8,  13.4, 0.0, 0.0, 0,0,0,0.0,  6.2  , ! 14B
     &                0.7, 2.1, 6.6,  23.5, 0.0, 0.0, 0,0,0,6.4,  20.7 , ! 16A
     &                0.1, 1.3, 4.0,  13.7, 0.0, 0.0, 0,0,0,0.4,  7.2  , ! 16B
     &                0.8, 3.7, 11.9, 26.0, 0.0, 0.0, 0,0,0,6.7,  9.5  , ! 18A
     &                0.1, 0.8, 4.3,  9.8,  0.0, 0.0, 0,0,0,0.0,  2.7  , ! 18B
     &                1.9, 6.1, 13.5, 28.6, 0.0, 0.0, 0,0,0,8.4,  8.8  , ! 20A
     &                0.0, 0.7, 7.0,  24.9, 0.0, 0.0, 0,0,0,0.0,  4.2  / ! 20B

      DATA REF29VLS /  144*0.0 /

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF30VLH / 0.2, 3.6, 5.5, 45.4, 0.0, 0.0, 0,0,0,19.6, 6.0 , ! 3D
     &                0.1, 1.5, 3.3, 45.8, 0.0, 0.0, 0,0,0,0.0,  2.6 , ! 3Dpost
     &                0.3, 3.6, 6.8, 36.1, 0.0, 0.0, 0,0,0,14.7, 3.1 , ! 2A
     &                0.0, 0.9, 5.5, 30.3, 0.0, 0.0, 0,0,0,0.0,  1.1 , ! 2Apost
     &                0.6, 3.5, 3.4, 31.6, 0.0, 0.0, 0,0,0,16.8, 3.9 , ! 3B
     &                0.1, 0.7, 1.9, 33.0, 0.0, 0.0, 0,0,0,0.0,  1.4 , ! 3Bpost
     &                0.2, 3.5, 4.3, 56.3, 0.0, 0.0, 0,0,0,11.6, 1.8 , ! 2C
     &                0.0, 1.5, 3.0, 48.3, 0.0, 0.0, 0,0,0,0.0,  1.3 , ! 2Cpost
     &                0.1, 1.8, 5.5, 50.7, 0.0, 0.0, 0,0,0,6.0,  4.3 , ! 2D
     &                0.0, 1.9, 3.9, 46.2, 0.0, 0.0, 0,0,0,0.0,  1.5 , ! 2Dpost
     &                0.2, 2.2, 4.8, 14.2, 0.0, 0.0, 0,0,0,8.4,  1.3 , ! 1A
     &                0.1, 2.0, 4.5, 12.0, 0.0, 0.0, 0,0,0,0.0,  1.1 , ! 1Apost
     &                0.1, 1.9, 2.8, 20.6, 0.0, 0.0, 0,0,0,7.1,  2.1 , ! 1C
     &                0.1, 1.6, 1.9, 18.2, 0.0, 0.0, 0,0,0,0.0,  1.2 , ! 1Cpost
     &                0.1, 1.2, 4.6, 7.6,  0.0, 0.0, 0,0,0,2.9,  2.4 , ! 1D
     &                0.1, 1.9, 3.8, 8.2,  0.0, 0.0, 0,0,0,0.0,  2.2 / ! 1Dpost

C                 <.25  to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF30VLS /  0.0, 0.0, 0.0, 0.9, 0.0, 0.0, 0,0,0, ! 3D             
     &                 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0,0,0, ! 3Dpost         
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 2A             
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 2Apost         
     &                 0.0, 0.0, 0.0, 3.6, 0.0, 0.0, 0,0,0, ! 3B             
     &                 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0,0,0, ! 3Bpost         
     &                 0.0, 0.0, 0.0, 1.8, 0.0, 0.0, 0,0,0, ! 2C             
     &                 0.0, 0.0, 0.0, 0.8, 0.0, 0.0, 0,0,0, ! 2Cpost         
     &                 0.0, 0.0, 0.0, 0.4, 0.0, 0.0, 0,0,0, ! 2D             
     &                 0.0, 0.0, 0.0, 0.9, 0.0, 0.0, 0,0,0, ! 2Dpost         
     &                 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0,0,0, ! 1A             
     &                 0.0, 0.0, 0.0, 1.1, 0.0, 0.0, 0,0,0, ! 1Apost         
     &                 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0,0,0, ! 1C             
     &                 0.0, 0.0, 0.0, 0.9, 0.0, 0.0, 0,0,0, ! 1Cpost         
     &                 0.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0,0,0, ! 1D             
     &                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0 /! 1Dpost         

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF31VLH / 0.5, 1.0, 0.2, 0.0, 0.0, 0.0, 0,0,0,3.1, 6.8  , ! 1
     &                1.3, 1.0, 2.1, 2.9, 0.0, 0.0, 0,0,0,3.4, 6.8  , ! 2
     &                1.4, 1.6, 2.5, 4.2, 0.0, 0.0, 0,0,0,7.0, 4.2  , ! 3
     &                2.8, 3.7, 6.3, 0.6, 0.0, 0.0, 0,0,0,8.4, 9.0  , ! 4
     &                2.8, 6.6, 6.0, 5.7, 0.0, 0.0, 0,0,0,5.4, 12.0 , ! 5
     &                4.7, 8.8, 7.0, 5.5, 0.0, 0.0, 0,0,0,5.9, 14.6 , ! 6
     &                3.2, 9.9, 6.2, 9.8, 0.0, 0.0, 0,0,0,4.1, 9.8  , ! 7
     &                0.9, 3.2, 5.5, 2.1, 0.0, 0.0, 0,0,0,4.6, 9.3  , ! 8
     &                1.7, 4.6, 5.8, 4.7, 0.0, 0.0, 0,0,0,7.0, 10.5 , ! 9
     &                0.1, 0.3, 0.5, 1.7, 0.0, 0.0, 0,0,0,0.0, 1.7  / ! 10

      DATA REF31VLS /  90*0.0 /

C                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA REF32VLH / 0.4, 1.8, 4.6,  11.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 1A
     &                0.4, 5.1, 9.1,  15.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 1B
     &                0.2, 1.4, 25.9, 48.6, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 1C
     &                0.2, 1.9, 1.5,  3.5,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 2A
     &                0.2, 1.2, 3.0,  14.1, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 2B
     &                0.2, 2.1, 12.1, 4.4,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 3A
     &                0.1, 1.6, 12.1, 14.3, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 3B
     &                0.2, 0.6, 9.2,  0.9,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 4A
     &                0.2, 1.0, 3.1,  13.2, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 4B
     &                0.0, 0.6, 12.1, 10.0, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 4C
     &                0.2, 1.0, 0.0,  8.0,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 5A
     &                0.2, 1.0, 1.5,  12.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 5B
     &                0.2, 0.2, 10.6, 14.0, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 5C
     &                0.4, 2.3, 9.1,  3.6,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 6A
     &                0.3, 0.6, 7.6,  33.0, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 6B
     &                0.3, 4.1, 7.6,  46.4, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 6C
     &                0.1, 0.8, 4.6,  4.1,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 7A
     &                0.3, 1.4, 4.6,  12.7, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 7B
     &                0.2, 3.6, 0.0,   6.3, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 7C
     &                0.5, 1.4, 3.0,  13.2, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 8A
     &                0.9, 0.6, 4.6,  14.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 9A
     &                0.9, 2.9, 6.1,  75.3, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 9B
     &                0.8, 3.1, 10.6, 15.4, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 10A
     &                0.4, 1.0, 9.3,  59.7, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 10B
     &                0.4, 1.0, 12.1, 62.7, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 10C
     &                0.1, 0.6, 1.5,   9.8, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 11A
     &                0.3, 0.4, 3.1,   6.0, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 12A
     &                0.7, 3.4, 10.8, 20.9, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 12B
     &                0.7, 1.0, 15.2, 33.4, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 12C
     &                0.2, 0.6, 0.0,  0.0,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 13A
     &                0.1, 0.4, 3.1,  4.9,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 13B
     &                0.4, 0.4, 3.0,  16.8, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 13C
     &                0.1, 0.2, 3.0,  23.8, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 13D
     &                0.2, 0.8, 4.5,  22.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 13E
     &                0.5, 0.0, 0.0,  0.6,  0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 14A
     &                0.3, 0.4, 3.1,  18.1, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 14B
     &                0.3, 1.8, 1.5,  20.8, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 14C
     &                0.7, 3.9, 19.7, 28.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 , ! 14D
     &                0.2, 0.6, 7.7,  74.5, 0.0, 0.0, 0,0,0,-1.0, -1.0 / ! 14E

C                 <.25  to1  1-3  3-6 6-12  12-20 20-35 35-50 >50
      DATA REF32VLS /  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0,0,0, ! 1A
     &                 0.0, 0.0, 0.0,  4.8, 0.0, 0.0, 0,0,0, ! 1B
     &                 0.0, 0.0, 0.0,  6.4, 0.0, 0.0, 0,0,0, ! 1C
     &                 0.0, 0.0, 0.0,  1.3, 0.0, 0.0, 0,0,0, ! 2A
     &                 0.0, 0.0, 0.0,  1.0, 0.0, 0.0, 0,0,0, ! 2B
     &                 0.0, 0.0, 0.0,  0.4, 0.0, 0.0, 0,0,0, ! 3A
     &                 0.0, 0.0, 0.0,  0.9, 0.0, 0.0, 0,0,0, ! 3B
     &                 0.0, 0.0, 0.0,  1.7, 0.0, 0.0, 0,0,0, ! 4A
     &                 0.0, 0.0, 0.0, 11.1, 0.0, 0.0, 0,0,0, ! 4B
     &                 0.0, 0.0, 0.0, 11.7, 0.0, 0.0, 0,0,0, ! 4C
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 5A
     &                 0.0, 0.0, 0.0,  2.5, 0.0, 0.0, 0,0,0, ! 5B
     &                 0.0, 0.0, 0.0, 11.3, 0.0, 0.0, 0,0,0, ! 5C
     &                 0.0, 0.0, 0.0,  0.6, 0.0, 0.0, 0,0,0, ! 6A
     &                 0.0, 0.0, 0.0, 14.0, 0.0, 0.0, 0,0,0, ! 6B
     &                 0.0, 0.0, 0.0,  4.5, 0.0, 0.0, 0,0,0, ! 6C
     &                 0.0, 0.0, 0.0,  0.6, 0.0, 0.0, 0,0,0, ! 7A
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 7B
     &                 0.0, 0.0, 0.0, 14.8, 0.0, 0.0, 0,0,0, ! 7C
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 8A
     &                 0.0, 0.0, 0.0,  3.2, 0.0, 0.0, 0,0,0, ! 9A
     &                 0.0, 0.0, 0.0,  2.5, 0.0, 0.0, 0,0,0, ! 9B
     &                 0.0, 0.0, 0.0,  2.9, 0.0, 0.0, 0,0,0, ! 10A
     &                 0.0, 0.0, 0.0,  0.9, 0.0, 0.0, 0,0,0, ! 10B
     &                 0.0, 0.0, 0.0, 21.7, 0.0, 0.0, 0,0,0, ! 10C
     &                 0.0, 0.0, 0.0,  3.9, 0.0, 0.0, 0,0,0, ! 11A
     &                 0.0, 0.0, 0.0,  5.3, 0.0, 0.0, 0,0,0, ! 12A
     &                 0.0, 0.0, 0.0,  4.8, 0.0, 0.0, 0,0,0, ! 12B
     &                 0.0, 0.0, 0.0, 13.8, 0.0, 0.0, 0,0,0, ! 12C
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 13A
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 13B
     &                 0.0, 0.0, 0.0,  1.7, 0.0, 0.0, 0,0,0, ! 13C
     &                 0.0, 0.0, 0.0,  8.3, 0.0, 0.0, 0,0,0, ! 13D
     &                 0.0, 0.0, 0.0, 33.7, 0.0, 0.0, 0,0,0, ! 13E
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 14A
     &                 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0,0,0, ! 14B
     &                 0.0, 0.0, 0.0, 13.8, 0.0, 0.0, 0,0,0, ! 14C
     &                 0.0, 0.0, 0.0,  9.2, 0.0, 0.0, 0,0,0, ! 14D
     &                 0.0, 0.0, 0.0,  0.4, 0.0, 0.0, 0,0,0 / ! 14E

C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMPHOTOVAL',10,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMPHOTOVAL CYCLE = ',I2)

      DO I=1,MXFLCL
        FOTOVAL(I) = -1
        IF (I .LE. 9) FOTOVALS(I) = 0.
      ENDDO

      SELECT CASE (FOTOREF)
      CASE (1)
      IF (FOTOCODE .GT. 22) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF1VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF1VLS(I, FOTOCODE)
      ENDDO

      CASE (2)
      IF (FOTOCODE .GT. 59) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF2VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF2VLS(I, FOTOCODE)
      ENDDO

      CASE (3)
      IF (FOTOCODE .GT. 66) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF3VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF3VLS(I, FOTOCODE)
      ENDDO

      CASE (5)
      IF (FOTOCODE .GT. 17) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF5VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF5VLS(I, FOTOCODE)
      ENDDO

      CASE (6)
      IF (FOTOCODE .GT. 27) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF6VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF6VLS(I, FOTOCODE)
      ENDDO

      CASE (7)
      IF (FOTOCODE .GT. 56) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF7VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF7VLS(I, FOTOCODE)
      ENDDO

      CASE (8)
      IF (FOTOCODE .GT. 86) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF8VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF8VLS(I, FOTOCODE)
      ENDDO

      CASE (9)
      IF (FOTOCODE .GT. 26) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF9VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF9VLS(I, FOTOCODE)
      ENDDO

      CASE (11)
      IF (FOTOCODE .GT. 26) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF11VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF11VLS(I, FOTOCODE)
      ENDDO

      CASE (12)
      IF (FOTOCODE .GT. 90) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF12VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF12VLS(I, FOTOCODE)
      ENDDO

      CASE (13)
      IF (FOTOCODE .GT. 42) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF13VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF13VLS(I, FOTOCODE)
      ENDDO

      CASE (14)
      IF (FOTOCODE .GT. 29) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF14VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF14VLS(I, FOTOCODE)
      ENDDO

      CASE (15)
      IF (FOTOCODE .GT. 29) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF15VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF15VLS(I, FOTOCODE)
      ENDDO

      CASE (16)
      IF (FOTOCODE .GT. 41) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF16VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF16VLS(I, FOTOCODE)
      ENDDO

      CASE (17)
      IF (FOTOCODE .GT. 35) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF17VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF17VLS(I, FOTOCODE)
      ENDDO

      CASE (18)
      IF (FOTOCODE .GT. 43) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF18VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF18VLS(I, FOTOCODE)
      ENDDO

      CASE (19)
      IF (FOTOCODE .GT. 34) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF19VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF19VLS(I, FOTOCODE)
      ENDDO

      CASE (20)
      IF (FOTOCODE .GT. 26) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF20VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF20VLS(I, FOTOCODE)
      ENDDO

      CASE (21)
      IF (FOTOCODE .GT. 25) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF21VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF21VLS(I, FOTOCODE)
      ENDDO

      CASE (22)
      IF (FOTOCODE .GT. 36) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF22VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF22VLS(I, FOTOCODE)
      ENDDO

      CASE (23)
      IF (FOTOCODE .GT. 26) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF23VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF23VLS(I, FOTOCODE)
      ENDDO

      CASE (24)
      IF (FOTOCODE .GT. 27) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF24VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF24VLS(I, FOTOCODE)
      ENDDO

      CASE (25)
      IF (FOTOCODE .GT. 14) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF25VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF25VLS(I, FOTOCODE)
      ENDDO

      CASE (26)
      IF (FOTOCODE .GT. 16) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF26VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF26VLS(I, FOTOCODE)
      ENDDO

      CASE (27)
      IF (FOTOCODE .GT. 30) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF27VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF27VLS(I, FOTOCODE)
      ENDDO

      CASE (28)
      IF (FOTOCODE .GT. 30) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF28VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF28VLS(I, FOTOCODE)
      ENDDO

      CASE (29)
      IF (FOTOCODE .GT. 16) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF29VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF29VLS(I, FOTOCODE)
      ENDDO

      CASE (30)
      IF (FOTOCODE .GT. 16) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF30VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF30VLS(I, FOTOCODE)
      ENDDO

      CASE (31)
      IF (FOTOCODE .GT. 10) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF31VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF31VLS(I, FOTOCODE)
      ENDDO

      CASE (32)
      IF (FOTOCODE .GT. 39) GO TO 50
      DO I = 1, MXFLCL
        FOTOVAL(I) = REF32VLH(I, FOTOCODE)
        IF (I .LE. 9) FOTOVALS(I) = REF32VLS(I, FOTOCODE)
      ENDDO

      END SELECT

   50 CONTINUE

      IF (DEBUG) WRITE(JOSTND,58) FOTOREF, FOTOCODE
   58 FORMAT(' IN FMPHOTOVAL, FOTOREF=',I4,' FOTOCODE=',I4)

      IF (DEBUG) WRITE (JOSTND,60) (FOTOVAL(I),I=1,11)
   60 FORMAT (1X,11F10.4)

      IF (DEBUG) WRITE (JOSTND,62) (FOTOVALS(I),I=1,9)
   62 FORMAT (1X,9F10.4)
C-----------
C IF FOTOVAL(1) IS STILL NEGATIVE THAN AN INVALID CODE WAS ENTERED SINCE THE
C 0-.25" WOOD IS ALWAYS FILLED IN IN THE PHOTO SERIES.
C-----------

      IF (FOTOVAL(1) .LT. 0) THEN
        WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &  'PHOTO REFERENCE OR PHOTO CODE ENTERED.',/1X)")
        CALL RCDSET (2,.TRUE.)
      ENDIF

      RETURN
      END
