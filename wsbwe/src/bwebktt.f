      BLOCK DATA BWEBK
      IMPLICIT NONE
C----------
C  **BWEBK--TT          DATE OF LAST REVISION:  07/14/10
C----------
C
C     BLOCKDATA FOR THE  BUDWORM MODEL.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
C     FORESTRY SCIENCES LAB--MOSCOW, IDAHO--MAY, 1984
C
c   Major revision by K.Sheehan 7/23/96 to strip out
c     BW Pop.Dyn.Model stuff
c   Major revision by K.Sheehan 7/6/96 to add Defoliation Model stuff
c
C     DESCRIPTION :
C
C     MASSIVE INITIALIZATION OF BUDWORM VARIABLES AT LOAD TIME.
C     NON-EXECUTABLE CODE.
C     THE VARIABLES AND ARRAYS INITIALIZED HERE SHOULD BE STATIC AND
C     IF THE VALUES OF ANY OF THESE ARE CHANGED BY KEYWORD OR BY THE
C     MODEL DURING THE SIMULATION, THE INITIALIZATION OF THAT VARIABLE
C     OR ARRAY SHOULD BE MOVED TO SUBROUTINE BWEINT. OTHERWISE, RESIDUAL
C     VALUES OR PARAMETERS WILL BE PASSED TO THE FOLLOWING STAND WHEN
C     PROJECTING MULTIPLE STANDS IN A SINGLE EXECUTION.
C
C  Revision History:
C    01-MAY-00 Lance David (FHTET)
C       Changed keyword BUDLITE to GENDEFOL (General Defoliator).
C    03-MAY-00 Lance David (FHTET)
C       Corrected data statement for TABLE array size.
C       Changed value for MOPT to indicate FVS keyword file.
C    12-MAY-00 Lance David (FHTET)
C       Variable ISIZE changed to TABSZ.
C    31-MAY-00 Lance David (FHTET)
C       Removed data statements that had been commented out during
C       the process of moving initialization of nonstatic variables
C       to the BWEINT routine.
C    04-OCT-00 Lance David (FHTET)
C       Added data statemenst to load weather station names into
C       WSLOOK that had previously been loaded at runtime from 
C       stations.dat file.
C    10-NOV-00 Lance David (FHTET)
C       Put Western Larch back into host species list (IBWSPM). It
C       got left behind somewhere in the model update process.
C       WL is not actually present as a FVS species in the TT variant.
C       Per Kathy Sheehan (10-JAN-01), inadequate data on budworm
C       impacts to Western Larch was available to the modeling team and
C       they chose to leave it as non-host for this reason.
C    09-SEP-06 Lance David (FHTET)
C       Moved non-static variables FOLDVY, FOLWTY and IOUT6A to bweint.f.
C    20-APR-10 Lance David (FMSC)
C       Updated for the Tetons variant 18 species expansion.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'BWESTD.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'
C
COMMONS
C
      INTEGER I

C     ****** IBWSPM IS MODIFIED FOR TT VARIANT
C     DEFOLIATION MODEL SPECIES INDICES:
C       1 = WF - White fir
C       2 = DF - Douglas fir
C       3 = GF - Grand fir
C       4 = AF - Subalpine fir
C       5 = ES - Engelmann spruce
C       6 = WL - Western Larch
C       7 = Not a host
C
C     FVS TT 18 SPECIES VARIANT INDICES:
C                 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18
C                 WB LM DF PM BS AS LP ES AF PP UJ RM BI MM NC MC OS OH
      DATA IBWSPM/7, 7, 2, 3, 7, 7, 7, 5, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7/

      DATA PRCRN3/ 0.05, 0.3, 0.65, 0.15, 0.45, 0.40, 0.15, 0.45, 0.40/
      DATA THEOFL/ 0.70, 0.24, 0.04, 0.02,
     >             0.45, 0.30, 0.20, 0.05,
     >             0.35, 0.30, 0.25, 0.10,
     >             0.50, 0.35, 0.12, 0.03,
     >             0.30, 0.25, 0.20, 0.25,
     >             0.10, 0.20, 0.20, 0.50,
     >             0.50, 0.30, 0.15, 0.05,
     >             0.30, 0.25, 0.20, 0.25,
     >             0.10, 0.20, 0.20, 0.50 /
      DATA RELFX/ 0.0, 0.04, 0.95, 1.0,
     >            0.0, 0.04, 0.95, 1.0,
     >            0.0, 0.04, 0.95, 1.0,
     >            0.0, 0.04, 0.70, 1.0,
     >            0.0, 0.12, 0.80, 1.0,
     >            0.0, 0.15, 0.90, 1.0,
     >            0.0, 0.04, 0.70, 1.0,
     >            0.0, 0.12, 0.80, 1.0,
     >            0.0, 0.15, 0.90, 1.0/
      DATA RELFY/ 0.05, 0.05, 1.0, 1.0,
     >            0.05, 0.05, 1.0, 1.0,
     >            0.05, 0.05, 1.0, 1.0,
     >            0.05, 0.05, 1.0, 1.0,
     >            0.15, 0.15, 1.0, 1.0,
     >            0.20, 0.20, 1.0, 1.0,
     >            0.05, 0.05, 1.0, 1.0,
     >            0.15, 0.15, 1.0, 1.0,
     >            0.20, 0.20, 1.0, 1.0/

      DATA TABLE /
     >    'END','OPEN','CLOSE','MGMTID','RANNSEED','STARTYR',
     >    'COMMENT','DEFOL','SETPRBIO','DAMAGE','NODAMAGE',
     >    'PERDAM','NOPERDAM','GENDEFOL','OUTBRLOC','RECOVERY',
     >    'WEATHER','NEMULT','BWOUTPUT','PARASITE','FQUALDEV',
     >    'FQUALWT','TITLE','BWSPRAY','OBSCHED'/

      DATA TABSZ / 25/

      DATA NHOSTS,NCROWN/6,9/

C  THE FOLLOWING VARIABLES ARE FOR THE DEFOLIATION MODEL
C
      DATA OBTABL/
     >	  32.6,8.1,29.8,43.7,12.6,8.1,11.0,16.5,
     >	  27.3,6.5,21.0,34.0,14.0,1.0,13.0,15.0,
     >	  35.0,0.0, 0.0, 0.0,23.0,8.9, 6.0,33.0/

      DATA EGGDEN/226.0/GPERM2/398.0/

      DATA FWSURV/.29/,
     >   SRATIO/0.5/,PMATED/1.0/,
     >   EATEN/.01066,.256,.0084,.2015,
     >   .00894,.2145,.00927,.2225,.0084,.2015,0.0,0.0/,
     >   WASTED/.1,.5/,WASTO/.6/,ECI/.0583,.0437,.0878,.0659,
     >   .0583,.0437,.0640,.0480,.0878,.0659,0.0,0.0/,
     >   AVEAMT/.3320,.2551,.2760,.2610,.2551,0.0/,
     >   OBPHX/0.0,1.0,6.0,50/

      DATA FOLDVX/0.0,1.0,6.0,50.0/,FOLWTX/0.0,1.0,6.0,50.0/

      DATA STARVX/0.0,0.0,0.1,0.2,0.15,0.8,1.0,1.0/,
     >     STARVY/.85,0.4,0.85,0.3,0.85,0.1,0.0,0.0/,
     >     DISPX/0.0,0.8,0.9,1.0/,DISPY/0.5,0.0,0.0,0.0/,
     >     OLDX/0.0,0.8,0.9,1.0/,OLDY/0.5,0.0,0.0,0.0/,
     >     EWTX/0.0,1.0,1.1,1.2/,EWTY/1.0,0.5,0.5,0.5/

      DATA EGG1/2.144,1.868,3*2.144,0.0/,
     >     EGG2/-0.478,3.921,3*-0.478,0.0/,
     >     FRESHC/4.02,3.54,4.02,3.59,3.84,0.0/

      DATA PHENOL/.38,.31,.31,.42,.34,.24,.38,.31,.31,.38,.31,.31,
     >     .42,.34,.24,3*0.0/,OLDMAX/.30/

      DATA PRATE/3*-0.071,2*-0.039,-0.071,3*-0.039/,
     >     ANT_BD/.40,.60,.80,.10,.30,.60,.10,.10,.30/

C     SET  DEFAULT VALUES (FOLIAGE VARIABLES)
C
      DATA GMAX/79.10,67.94,60.96,79.10,67.94,60.96,79.10,67.94,60.96,
     >     93.77,76.07,58.49,93.77,76.07,58.49,93.77,76.07,58.49,
     >     79.10,67.94,60.96,79.10,67.94,60.96,79.10,67.94,60.96,
     >     79.10,67.94,60.96,79.10,67.94,60.96,79.10,67.94,60.96,
     >     93.77,76.07,58.49,93.77,76.07,58.49,93.77,76.07,58.49,9*.0/
      DATA GMIN/1.66,2.00,1.36,1.66,2.00,1.36,1.66,2.00,1.36,
     >     5.71,5.95,6.11,5.71,5.95,6.11,5.71,5.95,6.11,
     >     1.66,2.00,1.36,1.66,2.00,1.36,1.66,2.00,1.36,
     >     1.66,2.00,1.36,1.66,2.00,1.36,1.66,2.00,1.36,
     >     5.71,5.95,6.11,5.71,5.95,6.11,5.71,5.95,6.11,9*1.0/
      DATA A1/-77.484,-65.953,-59.639,-77.484,-65.953,-59.639,
     >     	  -77.484,-65.953,-59.639,-88.180,-70.158,-52.378,
     >     	  -88.180,-70.158,-52.378,-88.180,-70.158,-52.378,
     >     	  -77.484,-65.953,-59.639,-77.484,-65.953,-59.639,
     >     	  -77.484,-65.953,-59.639,-77.484,-65.953,-59.639,
     >     	  -77.484,-65.953,-59.639,-77.484,-65.953,-59.639,
     >     	  -88.180,-70.158,-52.378,-88.180,-70.158,-52.378,
     >     	  -88.180,-70.158,-52.378,9*1.0/
      DATA A2/
     >     	-7.663,-8.976,-7.345,-7.663,-8.976,-7.345,
     >     	-7.663,-8.976,-7.345,-6.606,-7.549,-11.263,
     >     	-6.606,-7.549,-11.263,-6.606,-7.549,-11.263,
     >     	-7.663,-8.976,-7.345,-7.663,-8.976,-7.345,
     >     	-7.663,-8.976,-7.345,-7.663,-8.976,-7.345,
     >     	-7.663,-8.976,-7.345,-7.663,-8.976,-7.345,
     >     	-6.606,-7.549,-11.263,-6.606,-7.549,-11.263,
     >      -6.606,-7.549,-11.263,9*1.0/
      DATA A3/
     >     .02307,.02733,.02227,.02307,.02733,.02227,
     >     .02307,.02733,.02227,.01879,.02173,.03350,
     >     .01879,.02173,.03350,.01879,.02173,.03350,
     >     .02307,.02733,.02227,.02307,.02733,.02227,
     >     .02307,.02733,.02227,.02307,.02733,.02227,
     >     .02307,.02733,.02227,.02307,.02733,.02227,
     >     .01879,.02173,.03350,.01879,.02173,.03350,
     >     .01879,.02173,.03350,9*1.0/
      DATA A4/
     >     79.105,67.944,60.965,79.105,67.944,60.965,
     >     79.105,67.944,60.965,93.768,76.070,58.492,
     >     93.768,76.070,58.492,93.768,76.070,58.492,
     >     79.105,67.944,60.965,79.105,67.944,60.965,
     >     79.105,67.944,60.965,79.105,67.944,60.965,
     >     79.105,67.944,60.965,79.105,67.944,60.965,
     >     93.768,76.070,58.492,93.768,76.070,58.492,
     >     93.768,76.070,58.492,9*1.0/

      DATA
     >   TEMPS2/'Northern New Mexico','Blue Mountains (PNW)','Montana'/

      DATA SYNCHX/0.0,13.,16.,19.,22.,35./,
     >     SYNCHY/0.3,0.8,1.0,1.0,0.8,0.3/,
     >     RPHEN/0.9,1.0,0.9,0.9,1.0,1.0/

      DATA EVENT/'Outbreak starts','Outbreak ends',
     >  'Small larvae run out of foliage',
     >  'Heavy rain during L2 emergence',
     >  'Heavy rain during L2 to mid-L4',
     >  'Heavy rain during mid-L4 to L6',
     >  'Heavy rain during pupal stage',
     >  'Very warm fall temperatures in previous autumn',
     >  'Great synchrony between trees and budworm',
     >  'Very poor synchrony btwn trees and budworm',
     >  'Insecticide applied',
     >  9*'UNDEFINED'/

      DATA DISPMX/1000.0,400.0,0.0,0.0/,DISPMY/.20,.20,.40,.40/,
     >  ADMORT/0.05/,EPMASS/41.7/,
     >  DISPDR/.20,.15,.10,.08,.06,.04,.01,.01,.01/

      DATA STATES/'Arizona','California','Colorado','Idaho',
     >  'Montana','New Mexico','Oregon','Utah','Washington',
     >  'Wyoming','ARIZONA.DAT','CALIFORN.DAT','COLORADO.DAT',
     >  'IDAHO.DAT','MONTANA.DAT','NEW MEXI.DAT','OREGON.DAT',
     >  'UTAH.DAT','WASHINGT.DAT','WYOMING.DAT'/

C     04-OCT-2000 Lance David
C     The data statements after this point are from the stations.dat
C     file dated 17-NOV-1999 that was originally read at runtime.
C
C     Number of weather stations for each state.
C                 AZ,CA,CO, ID,MO,NM,OR,UT,WA,WY
      DATA NUMSTN/ 2, 2, 2,100,91, 3,51, 3,36, 3/

C     Weather Station names.
C     Arizona
      DATA (WSLOOK(I,1),I=1,100) /
     > "station11       ","station12       ","station13       ",
     > "station14       ","station15       ","station16       ",
     > "station17       ","station18       ","station19       ",
     > "station110      ","station111      ","station112      ",
     > "station113      ","station114      ","station115      ",
     > "station116      ","station117      ","station118      ",
     > "station119      ","station120      ","station121      ",
     > "station122      ","station123      ","station124      ",
     > "station125      ","station126      ","station127      ",
     > "station128      ","station129      ","station130      ",
     > "station131      ","station132      ","station133      ",
     > "station134      ","station135      ","station136      ",
     > "station137      ","station138      ","station139      ",
     > "station140      ","station141      ","station142      ",
     > "station143      ","station144      ","station145      ",
     > "station146      ","station147      ","station148      ",
     > "station149      ","station150      ","station11       ",
     > "station12       ","station13       ","station14       ",
     > "station15       ","station16       ","station17       ",
     > "station18       ","station19       ","station110      ",
     > "station111      ","station112      ","station113      ",
     > "station114      ","station115      ","station116      ",
     > "station117      ","station118      ","station119      ",
     > "station120      ","station121      ","station122      ",
     > "station123      ","station124      ","station125      ",
     > "station126      ","station127      ","station128      ",
     > "station129      ","station130      ","station131      ",
     > "station132      ","station133      ","station134      ",
     > "station135      ","station136      ","station137      ",
     > "station138      ","station139      ","station140      ",
     > "station141      ","station142      ","station143      ",
     > "station144      ","station145      ","station146      ",
     > "station147      ","station148      ","station149      ",
     > "station150      "/

C     California
      DATA (WSLOOK(I,2),I=1,100) /
     > "station21       ","station22       ","station23       ",
     > "station24       ","station25       ","station26       ",
     > "station27       ","station28       ","station29       ",
     > "station210      ","station211      ","station212      ",
     > "station213      ","station214      ","station215      ",
     > "station216      ","station217      ","station218      ",
     > "station219      ","station220      ","station221      ",
     > "station222      ","station223      ","station224      ",
     > "station225      ","station226      ","station227      ",
     > "station228      ","station229      ","station230      ",
     > "station231      ","station232      ","station233      ",
     > "station234      ","station235      ","station236      ",
     > "station237      ","station238      ","station239      ",
     > "station240      ","station241      ","station242      ",
     > "station243      ","station244      ","station245      ",
     > "station246      ","station247      ","station248      ",
     > "station249      ","station250      ","station21       ",
     > "station22       ","station23       ","station24       ",
     > "station25       ","station26       ","station27       ",
     > "station28       ","station29       ","station210      ",
     > "station211      ","station212      ","station213      ",
     > "station214      ","station215      ","station216      ",
     > "station217      ","station218      ","station219      ",
     > "station220      ","station221      ","station222      ",
     > "station223      ","station224      ","station225      ",
     > "station226      ","station227      ","station228      ",
     > "station229      ","station230      ","station231      ",
     > "station232      ","station233      ","station234      ",
     > "station235      ","station236      ","station237      ",
     > "station238      ","station239      ","station240      ",
     > "station241      ","station242      ","station243      ",
     > "station244      ","station245      ","station246      ",
     > "station247      ","station248      ","station249      ",
     > "station250      "/

C     Colorado
      DATA (WSLOOK(I,3),I=1,100) /
     > "station31       ","station32       ","station33       ",
     > "station34       ","station35       ","station36       ",
     > "station37       ","station38       ","station39       ",
     > "station310      ","station311      ","station312      ",
     > "station313      ","station314      ","station315      ",
     > "station316      ","station317      ","station318      ",
     > "station319      ","station320      ","station321      ",
     > "station322      ","station323      ","station324      ",
     > "station325      ","station326      ","station327      ",
     > "station328      ","station329      ","station330      ",
     > "station331      ","station332      ","station333      ",
     > "station334      ","station335      ","station336      ",
     > "station337      ","station338      ","station339      ",
     > "station340      ","station341      ","station342      ",
     > "station343      ","station344      ","station345      ",
     > "station346      ","station347      ","station348      ",
     > "station349      ","station350      ","station31       ",
     > "station32       ","station33       ","station34       ",
     > "station35       ","station36       ","station37       ",
     > "station38       ","station39       ","station310      ",
     > "station311      ","station312      ","station313      ",
     > "station314      ","station315      ","station316      ",
     > "station317      ","station318      ","station319      ",
     > "station320      ","station321      ","station322      ",
     > "station323      ","station324      ","station325      ",
     > "station326      ","station327      ","station328      ",
     > "station329      ","station330      ","station331      ",
     > "station332      ","station333      ","station334      ",
     > "station335      ","station336      ","station337      ",
     > "station338      ","station339      ","station340      ",
     > "station341      ","station342      ","station343      ",
     > "station344      ","station345      ","station346      ",
     > "station347      ","station348      ","station349      ",
     > "station350      "/

C     Idaho
      DATA (WSLOOK(I,4),I=1,100) /
     > "Aberdeen        ","American Falls  ","Anderson Dam    ",
     > "Arbon           ","Arco            ","Arrowrock Dam   ",
     > "Ashton          ","Bayview Model   ","Blackfoot 4NNE  ",
     > "Boise Air Term  ","Bonners Ferry   ","Brownlee Dam    ",
     > "Cabinet Gorge   ","Caldwell        ","Cambridge       ",
     > "Cascade         ","Challis         ","Chilly Barton   ",
     > "Cobalt          ","Coeur dAlene    ","Conda           ",
     > "Council         ","Craters of Moon ","Deadwood        ",
     > "Dixie           ","Driggs          ","Dubois Exp Stn  ",
     > "Dworshak Fish   ","Elk City RS     ","Elk River       ",
     > "Emmett 2E       ","Fairfield RS    ","Fenn RS         ",
     > "Fort Hall       ","Garden Valley   ","Gibbonsville    ",
     > "Grace           ","Grangeville     ","Grouse          ",
     > "Hamer           ","Hazelton        ","Headquarters    ",
     > "Hill City       ","Howe            ","Idaho City      ",
     > "Idaho Falls16SE ","Idaho Falls 46W ","Island Park     ",
     > "Jerome          ","Kellogg         ","Ketchum RS      ",
     > "Kooskia         ","Kuna            ","Lewiston NezP   ",
     > "Lifton Pumping  ","Lowman          ","Mackay RS       ",
     > "Malad           ","Malad City AP   ","May             ",
     > "McCall          ","Minidoka Dam    ","Montpelier RS   ",
     > "Moscow UI       ","New Meadows RS  ","Nez Perce       ",
     > "Ola             ","Orofino         ","Palisade        ",
     > "Parma Exp Stn   ","Paul            ","Payette         ",
     > "Picabo          ","Pierce RS       ","Pocatello       ",
     > "Porthill        ","Potlatch        ","Powell          ",
     > "Priest River ES ","Richfield       ","Riggins         ",
     > "Ruppert         ","Saint Anthony   ","Saint Maries    ",
     > "Salmon          ","Salmon KSRA     ","Sandpoint       ",
     > "Shoshone        ","Strevell        ","Sugar           ",
     > "Swan Valley     ","Tetonia Exp Stn ","Three Creek     ",
     > "Twin Falls 2NNE ","Twin Falls 3SE  ","Wallace         ",
     > "Wallace Woodl.  ","Warren          ","Weiser          ",
     > "Winchester      "/

C     Montana
      DATA (WSLOOK(I,5),I=1,100) /
     > "Alder           ","Augusta         ","Babb            ",
     > "Barber          ","Big Timber      ","Blackleaf       ",
     > "Boulder         ","Bozeman         ","Browning        ",
     > "Butte BM AP     ","Canyon Ferry D  ","Cascade         ",
     > "Cnoteau AP      ","Columbus        ","Creston         ",
     > "Cut Bank AP     ","Deer Lodge      ","Del Bonita      ",
     > "Denton          ","Dillon          ","Divide          ",
     > "Drummond        ","Dunkirk         ","East Anaconda   ",
     > "Elliston        ","Eureka MS       ","Fairfield       ",
     > "Fortine         ","Geraldine       ","Gibson Dam      ",
     > "Glen            ","Great Falls     ","Hamilton        ",
     > "Harlowton       ","Haugen          ","Hebgen Dam      ",
     > "Helena AP       ","Heron           ","Holter Dam      ",
     > "Hungry Horse D  ","Judith Gap      ","Lakeview        ",
     > "Lewiston AP     ","Libby           ","Lima            ",
     > "Lincoln RS      ","Lindberg Lake   ","Livingston      ",
     > "Loma            ","Lone Pine       ","Malta           ",
     > "Martindale      ","Melville        ","Missoula        ",
     > "Mystic          ","Neihart         ","Norris          ",
     > "Ovando          ","Philipsburg     ","Pleasant Valley ",
     > "Plevna          ","Polebridge      ","Polson          ",
     > "Pony            ","Potomac         ","Rapelje         ",
     > "Red Lodge       ","Ryegate         ","Saint Regis     ",
     > "Seeley Lake RS  ","Shonkin         ","Stanford        ",
     > "Stevensville    ","St. Ignatius    ","Sula            ",
     > "Summit          ","Superior        ","Swan Lake       ",
     > "Townsend        ","Trident         ","Trout Creek     ",
     > "Troy            ","Twin Bridges    ","Utica           ",
     > "Valier          ","Virginia City   ","West Glacier    ",
     > "West Yellowst.  ","White Sulphur S ","Wilsall         ",
     > "Wisdom          ","station542      ","station543      ",
     > "station544      ","station545      ","station546      ",
     > "station547      ","station548      ","station549      ",
     > "station550      "/

C     New Mexico
      DATA (WSLOOK(I,6),I=1,100) /
     > "station61       ","station62       ","station63       ",
     > "station64       ","station65       ","station66       ",
     > "station67       ","station68       ","station69       ",
     > "station610      ","station611      ","station612      ",
     > "station613      ","station614      ","station615      ",
     > "station616      ","station617      ","station618      ",
     > "station619      ","station620      ","station621      ",
     > "station622      ","station623      ","station624      ",
     > "station625      ","station626      ","station627      ",
     > "station628      ","station629      ","station630      ",
     > "station631      ","station632      ","station633      ",
     > "station634      ","station635      ","station636      ",
     > "station637      ","station638      ","station639      ",
     > "station640      ","station641      ","station642      ",
     > "station643      ","station644      ","station645      ",
     > "station646      ","station647      ","station648      ",
     > "station649      ","station650      ","station61       ",
     > "station62       ","station63       ","station64       ",
     > "station65       ","station66       ","station67       ",
     > "station68       ","station69       ","station610      ",
     > "station611      ","station612      ","station613      ",
     > "station614      ","station615      ","station616      ",
     > "station617      ","station618      ","station619      ",
     > "station620      ","station621      ","station622      ",
     > "station623      ","station624      ","station625      ",
     > "station626      ","station627      ","station628      ",
     > "station629      ","station630      ","station631      ",
     > "station632      ","station633      ","station634      ",
     > "station635      ","station636      ","station637      ",
     > "station638      ","station639      ","station640      ",
     > "station641      ","station642      ","station643      ",
     > "station644      ","station645      ","station646      ",
     > "station647      ","station648      ","station649      ",
     > "station650      "/

C     Oregon
      DATA (WSLOOK(I,7),I=1,100) /
     > "Adel            ","Alkali Lake     ","Antelope 1 NW   ",
     > "Arlington       ","Austin 3S       ","Baker FCWOS     ",
     > "Barnes Station  ","Belknap Spr. 8N ","Bend            ",
     > "Beulah          ","Brothers        ","Chemult         ",
     > "Condon          ","Cove            ","Crater Lake     ",
     > "Detroit Dam     ","Dufur           ","Echo            ",
     > "Elgin           ","Enterprise RS   ","Fossil          ",
     > "Governmt Camp   ","Grizzly         ","Halfway         ",
     > "Heppner         ","Hermiston       ","Hood River Exp  ",
     > "Huntington      ","Ironside 2W     ","John Day        ",
     > "Klamath Falls   ","LaGrande        ","Lakeview        ",
     > "Long Creek      ","McKenzie Br.    ","Milton Freew.   ",
     > "Mitchell        ","Monument        ","Moro            ",
     > "Ochoco RS       ","Owyhee Dam      ","Pauline         ",
     > "Pendleton AP    ","Pilot Rock      ","Prineville      ",
     > "Seneca          ","Sisters         ","Ukiah           ",
     > "Union           ","Unity           ","Wickiup Dam     ",
     > "station62       ","station63       ","station64       ",
     > "station65       ","station66       ","station67       ",
     > "station68       ","station69       ","station610      ",
     > "station611      ","station612      ","station613      ",
     > "station614      ","station615      ","station616      ",
     > "station617      ","station618      ","station619      ",
     > "station620      ","station621      ","station622      ",
     > "station623      ","station624      ","station625      ",
     > "station626      ","station627      ","station628      ",
     > "station629      ","station630      ","station631      ",
     > "station632      ","station633      ","station634      ",
     > "station635      ","station636      ","station637      ",
     > "station638      ","station639      ","station640      ",
     > "station641      ","station642      ","station643      ",
     > "station644      ","station645      ","station646      ",
     > "station647      ","station648      ","station649      ",
     > "station650      "/

C     Utah
      DATA (WSLOOK(I,8),I=1,100) /
     > "station81       ","station82       ","station83       ",
     > "station84       ","station85       ","station86       ",
     > "station87       ","station88       ","station89       ",
     > "station810      ","station811      ","station812      ",
     > "station813      ","station814      ","station815      ",
     > "station816      ","station817      ","station818      ",
     > "station819      ","station820      ","station821      ",
     > "station822      ","station823      ","station824      ",
     > "station825      ","station826      ","station827      ",
     > "station828      ","station829      ","station830      ",
     > "station831      ","station832      ","station833      ",
     > "station834      ","station835      ","station836      ",
     > "station837      ","station838      ","station839      ",
     > "station840      ","station841      ","station842      ",
     > "station843      ","station844      ","station845      ",
     > "station846      ","station847      ","station848      ",
     > "station849      ","station850      ","station81       ",
     > "station82       ","station83       ","station84       ",
     > "station85       ","station86       ","station87       ",
     > "station88       ","station89       ","station810      ",
     > "station811      ","station812      ","station813      ",
     > "station814      ","station815      ","station816      ",
     > "station817      ","station818      ","station819      ",
     > "station820      ","station821      ","station822      ",
     > "station823      ","station824      ","station825      ",
     > "station826      ","station827      ","station828      ",
     > "station829      ","station830      ","station831      ",
     > "station832      ","station833      ","station834      ",
     > "station835      ","station836      ","station837      ",
     > "station838      ","station839      ","station840      ",
     > "station841      ","station842      ","station843      ",
     > "station844      ","station845      ","station846      ",
     > "station847      ","station848      ","station849      ",
     > "station850      "/

C     Washington
      DATA (WSLOOK(I,9),I=1,100) /
     > "Anatone         ","Appleton        ","Bickleton       ",
     > "Bounding Dam    ","Buckley         ","Bumping Lake    ",
     > "Chelan          ","Chesaw 4NNW     ","Chewelah        ",
     > "Cle Elum        ","Colville        ","Colville AP     ",
     > "Conconully      ","Dayton          ","Ellensburg      ",
     > "Goldendale      ","Lake Cle Elum   ","Lake Kachess    ",
     > "Lake Keechelus  ","Laurier         ","Leavenworth     ",
     > "Methow          ","Mount Adams RS  ","Moxee City      ",
     > "Newhalem        ","Northport       ","Omak 4N         ",
     > "Plain           ","Rimrock Tiet.   ","Stehekin        ",
     > "Sunnyside       ","Tieton Intake   ","Waterville      ",
     > "Wenatchee       ","Wen.Pangborn    ","Yakima AirT.    ",
     > "station937      ","station938      ","station939      ",
     > "station940      ","station941      ","station942      ",
     > "station943      ","station944      ","station945      ",
     > "station946      ","station947      ","station948      ",
     > "station949      ","station950      ","station941      ",
     > "station942      ","station943      ","station944      ",
     > "station945      ","station946      ","station947      ",
     > "station948      ","station949      ","station950      ",
     > "station941      ","station942      ","station943      ",
     > "station944      ","station945      ","station946      ",
     > "station947      ","station948      ","station949      ",
     > "station950      ","station941      ","station942      ",
     > "station943      ","station944      ","station945      ",
     > "station946      ","station947      ","station948      ",
     > "station949      ","station950      ","station941      ",
     > "station942      ","station943      ","station944      ",
     > "station945      ","station946      ","station947      ",
     > "station948      ","station949      ","station950      ",
     > "station941      ","station942      ","station943      ",
     > "station944      ","station945      ","station946      ",
     > "station947      ","station948      ","station949      ",
     > "station950      "/

C     Wyoming
      DATA (WSLOOK(I,10),I=1,100) /
     > "station01       ","station02       ","station03       ",
     > "station04       ","station05       ","station06       ",
     > "station07       ","station08       ","station09       ",
     > "station010      ","station011      ","station012      ",
     > "station013      ","station014      ","station015      ",
     > "station016      ","station017      ","station018      ",
     > "station019      ","station020      ","station021      ",
     > "station022      ","station023      ","station024      ",
     > "station025      ","station026      ","station027      ",
     > "station028      ","station029      ","station030      ",
     > "station031      ","station032      ","station033      ",
     > "station034      ","station035      ","station036      ",
     > "station037      ","station038      ","station039      ",
     > "station040      ","station041      ","station042      ",
     > "station043      ","station044      ","station045      ",
     > "station046      ","station047      ","station048      ",
     > "station049      ","station050      ","station01       ",
     > "station02       ","station03       ","station04       ",
     > "station05       ","station06       ","station07       ",
     > "station08       ","station09       ","station010      ",
     > "station011      ","station012      ","station013      ",
     > "station014      ","station015      ","station016      ",
     > "station017      ","station018      ","station019      ",
     > "station020      ","station021      ","station022      ",
     > "station023      ","station024      ","station025      ",
     > "station026      ","station027      ","station028      ",
     > "station029      ","station030      ","station031      ",
     > "station032      ","station033      ","station034      ",
     > "station035      ","station036      ","station037      ",
     > "station038      ","station039      ","station040      ",
     > "station041      ","station042      ","station043      ",
     > "station044      ","station045      ","station046      ",
     > "station047      ","station048      ","station049      ",
     > "station050      "/

	END
