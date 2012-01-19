      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-SN-DATE OF LAST REVISION:  01/19/2011
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
      INTEGER ISP,EQNUM(MAXSP)
      REAL B1(39),DBH,FMBRKT
C
      DATA B1/.019, .022, .024, .025, .026, .027, .028, .029, .03,
     & .031, .032, .033, .034, .035, .036, .037, .038, .039, .04, .041,
     & .042, .043, .044, .045, .046, .047, .048, .049, .05, .052, .055,
     & .057, .059, .06, .062, .063, .068, .072, .081/
C
      DATA EQNUM/
     &     30,    !1   fir sp.
     &     17,    !2   redcedar
     &     13,    !3   spruce sp.
     &     14,    !4   sand pine
     &     16,    !5   shortleaf pine
     &     31,    !6   slash pine
     &     14,    !7   spruce pine
     &     28,    !8   longleaf pine
     &     19,    !9   table mountain pine
     &     24,    !10  pitch pine
     &     35,    !11  pond pine
     &     24,    !12  eastern white pine
     &     30,    !13  loblolly pine
     &     12,    !14  virginia pine
     &      4,    !15  baldcypress
     &     21,    !16  pondcypress
     &     18,    !17  hemlock
     &      8,    !18  Florida maple
     &     13,    !19  boxelder
     &      7,    !20  red maple
     &     10,    !21  silver maple
     &     12,    !22  sugar maple
     &     15,    !23  buckeye/horsechestnut  (used ohio buckeye)
     &     12,    !24  birch sp.
     &      9,    !25  sweet birch
     &      9,    !26  american hornbeam
     &     19,    !27  hickory sp.   (used shagbark hickory)
     &     16,    !28  catalpa
     &     15,    !29  hackberry sp.    (used sugarberry)
     &     14,    !30  eastern redbud
     &     20,    !31  flowering dogwood
     &     20,    !32  common persimmon
     &      4,    !33  american beech
     &     21,    !34  ash sp.
     &     21,    !35  white ash
     &     14,    !36  black ash
     &     18,    !37  green ash
     &     17,    !38  honeylocust
     &     17,    !39  loblolly-bay
     &     17,    !40  silverbell
     &     21,    !41  american holly
     &     20,    !42  butternut
     &     20,    !43  black walnut
     &     15,    !44  sweet gum
     &     20,    !45  yellow-poplar
     &     18,    !46  magnolia sp.
     &     15,    !47  cucumbertree
     &     12,    !48  southern magnolia
     &     19,    !49  sweetbay
     &     12,    !50  bigleaf magnolia
     &     22,    !51  apple sp.
     &     17,    !52  mulberry sp.   (used red mulberry)
     &      9,    !53  water tupelo
     &     18,    !54  black gum
     &     16,    !55  swamp tupelo
     &     16,    !56  e. hophornbeam
     &     15,    !57  sourwood
     &     17,    !58  redbay
     &     12,    !59  sycamore
     &     19,    !60  cottonwood
     &     18,    !61  bigtooth aspen
     &      9,    !62  black cherry
     &     19,    !63  white oak
     &     19,    !64  scarlet oak
     &     23,    !65  southern red oak
     &     23,    !66  cherrybark oak (used southern red oak)
     &     16,    !67  turkey oak
     &     15,    !68  laurel oak
     &     18,    !69  overcup oak
     &     16,    !70  blackjack oak
     &     25,    !71  swamp chestnut oak
     &     21,    !72  chinkapin oak
     &     15,    !73  water oak
     &     28,    !74  chestnut oak
     &     21,    !75  northern red oak
     &     16,    !76  shumard oak
     &     23,    !77  post oak
     &     24,    !78  black oak
     &     22,    !79  live oak
     &     28,    !80  black locust
     &     19,    !81  willow (used black willow)
     &     14,    !82  sassafras
     &     17,    !83  basswood  (used American basswood)
     &     18,    !84  elm sp.
     &     10,    !85  winged elm
     &     10,    !86  american elm
     &     11,    !87  slippery elm
     &     17,    !88  softwoods, misc.  (used redcedar)
     &     24,    !89  hardwoods, misc.   (used black oak)
     &     24/    !90  unknown/not listed   (used black oak)
C
      FMBRKT = DBH*B1(EQNUM(ISP))
C----------
C  THE BARK THICKNESS FOR SHORTLEAF PINE IS FROM HARMON,
C  SURVIVAL OF TREES AFTER LOW-INTENSITY SURFACE FIRES IN GREAT SMOKY
C  MOUNTAINS NATIONAL PARK, ECOLOGY 65(3), 1984.
C  THIS WAS CHANGED BECAUSE USERS ON THE OZARK NF THOUGHT THE FIRE-
C  RELATED MORTALITY FOR SHORTLEAF WAS TOO HIGH.
C----------
      IF (ISP .EQ. 5) THEN
        FMBRKT = (.07 + .09*DBH*2.54 - .0001*DBH*DBH*2.54*2.54)/2.54
        FMBRKT = MAX(0.,FMBRKT)
      ENDIF
C
      RETURN
      END
