      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-CS $Id$
C----------
C  COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C  ROUTINE (FMEFF).
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C
      INTEGER ISP
      REAL B1(39),DBH,FMBRKT
      INTEGER EQNUM(MAXSP)
C
      DATA B1/.019, .022, .024, .025, .026, .027, .028, .029, .03,
     & .031, .032, .033, .034, .035, .036, .037, .038, .039, .04, .041,
     & .042, .043, .044, .045, .046, .047, .048, .049, .05, .052, .055,
     & .057, .059, .06, .062, .063, .068, .072, .081/
C
      DATA EQNUM/
     &    17,    !1   eastern redcedar
     &    12,    !2   juniper species
     &    16,    !3   shortleaf pine
     &    12,    !4   virginia pine
     &    30,    !5   loblolly pine
     &    24,    !6   other softwood species
     &    24,    !7   eastern white pine
     &    20,    !8   black walnut
     &    20,    !9   butternut
     &    4 ,    !10  tupelo species
     &    16,    !11  swamp tupelo
     &    9 ,    !12  water tupelo
     &    18,    !13  black tupelo
     &    23,    !14  select hickory
     &    19,    !15  shagbark hickory
     &    22,    !16  shellbark hickory
     &    22,    !17  mockernut hickory
     &    16,    !18  pignut hickory
     &    23,    !19  hickory species
     &    23,    !20  water hickory
     &    16,    !21  bitternut hickory
     &    15,    !22  pecan
     &    19,    !23  black hickory
     &    4 ,    !24  American beech
     &    14,    !25  black ash
     &    16,    !26  pumpkin ash
     &    9 ,    !27  blue ash
     &    19,    !28  eastern cottonwood
     &    7 ,    !29  red maple
     &    13,    !30  boxelder
     &    10,    !31  silver maple
     &    9 ,    !32  black cherry
     &    10,    !33  american elm
     &    15,    !34  sugarberry
     &    15,    !35  hackberry
     &    10,    !36  winged elm
     &    18,    !37  elm species
     &    17,    !38  siberian elm
     &    11,    !39  slippery elm
     &    12,    !40  rock elm
     &    20,    !41  yellow-poplar
     &    17,    !42  american basswood
     &    12,    !43  sugar maple
     &    21,    !44  ash species
     &    21,    !45  white ash
     &    18,    !46  green ash
     &    19,    !47  white oak
     &    21,    !48  northern red oak
     &    23,    !49  southern red oak
     &    24,    !50  black oak
     &    19,    !51  scarlet oak
     &    16,    !52  blackjack oak
     &    21,    !53  chinkapin oak
     &    24,    !54  swamp white oak
     &    21,    !55  bur oak
     &    25,    !56  swamp chestnut oak
     &    23,    !57  post oak
     &    23,    !58  delta post oak
     &    28,    !59  chestnut oak
     &    20,    !60  pin oak
     &    23,    !61  cherrybark oak
     &    20,    !62  shingle oak
     &    18,    !63  overcup oak
     &    15,    !64  water oak
     &    9 ,    !65  nuttall oak
     &    20,    !66  willow oak
     &    16,    !67  shumard oak
     &    17,    !68  other upland hardwoods
     &    14,    !69  sassafras
     &    15,    !70  ohio buckeye
     &    16,    !71  catalpa
     &    20,    !72  common persimmon
     &    17,    !73  honeylocust
     &    19,    !74  balsam poplar
     &    18,    !75  bigtooth aspen
     &    23,    !76  quaking aspen
     &    28,    !77  black locust
     &    12,    !78  other lowland species
     &    12,    !79  sycamore
     &    4 ,    !80  baldcypress
     &    8 ,    !81  river birch
     &    15,    !82  sweet gum
     &    20,    !83  willow species
     &    19,    !84  black willow
     &    20,    !85  non-commercial hardwoods
     &    9 ,    !86  american hornbeam
     &    14,    !87  eastern redbud
     &    20,    !88  flowering dogwood
     &    17,    !89  hawthorn species
     &    10,    !90  kentucky coffeetree  
     &    16,    !91  osage-orange
     &    15,    !92  cucumbertree
     &    19,    !93  sweetbay
     &    12,    !94  mulberry species
     &    16,    !95  eastern hophornbeam
     &    15/    !96  sourwood     
     
C
      FMBRKT = DBH*B1(EQNUM(ISP))
C----------
C  THE BARK THICKNESS FOR SHORTLEAF PINE IS FROM HARMON,
C  SURVIVAL OF TREES AFTER LOW-INTENSITY SURFACE FIRES IN GREAT SMOKY
C  MOUNTAINS NATIONAL PARK, ECOLOGY 65(3), 1984.
C  THIS WAS CHANGED BECAUSE USERS ON THE OZARK NF THOUGHT THE FIRE-
C  RELATED MORTALITY FOR SHORTLEAF WAS TOO HIGH.
C----------
      IF (ISP .EQ. 3) THEN
        FMBRKT = (.07 + .09*DBH*2.54 - .0001*DBH*DBH*2.54*2.54)/2.54
        FMBRKT = MAX(0.,FMBRKT)
      ENDIF
C
      RETURN
      END
