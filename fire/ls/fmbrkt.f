      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-LS-DATE OF LAST REVISION:  01/13/12
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
     &     19,    !1   jack pine
     &      9,    !2   scotch pine
     &     22,    !3   red pine natural
     &     22,    !4   red pine plantation
     &     24,    !5   white pine
     &      4,    !6   white spruce
     &      8,    !7   Norway spruce
     &     10,    !8   balsam fir
     &     11,    !9   black spruce
     &     10,    !10  tamarack
     &      4,    !11  n. white cedar
     &     18,    !12  eastern hemlock
     &     17,    !13  other softwoods (use eastern redcedar)
     &     17,    !14  eastern redcedar
     &     14,    !15  black ash
     &     18,    !16  green ash
     &     19,    !17  cottonwood
     &     10,    !18  silver maple
     &      7,    !19  red maple
     &      9,    !20  black cherry
     &     10,    !21  American elm
     &     11,    !22  slippery elm
     &     12,    !23  rock elm
     &     10,    !24  yellow birch
     &     17,    !25  basswood
     &     12,    !26  sugar maple
     &     14,    !27  black maple
     &      4,    !28  American beech
     &     21,    !29  white ash
     &     19,    !30  white oak
     &     24,    !31  swamp white oak
     &     21,    !32  bur oak
     &     21,    !33  chinkapin oak
     &     21,    !34  northern red oak
     &     24,    !35  black oak
     &     17,    !36  northern pin oak
     &     16,    !37  bitternut hickory
     &     16,    !38  pignut hickory
     &     19,    !39  shagbark hickory
     &     18,    !40  bigtooth aspen
     &     23,    !41  quaking aspen
     &     19,    !42  balsam poplar
     &      6,    !43  paper birch
     &     20,    !44  commercial hardwoods (used butternut)
     &     20,    !45  butternut
     &     20,    !46  black walnut
     &     16,    !47  eastern hophornbeam
     &     28,    !48  black locust
     &     13,    !49  non-commercial hardwoods (used boxelder)
     &     13,    !50  boxelder
     &     24,    !51  striped maple
     &     19,    !52  mountain maple
     &      9,    !53  American hornbeam
     &     19,    !54  American chestnut
     &     15,    !55  hackberry (used sugarberry)
     &     20,    !56  flowering dogwood
     &     17,    !57  hawthorn
     &     22,    !58  apple sp.
     &     18,    !59  black gum
     &     12,    !60  sycamore
     &     24,    !61  pin cherry
     &     19,    !62  choke cherry
     &     19,    !63  wild plum
     &     20,    !64  willow
     &     19,    !65  black willow
     &     19,    !66  diamond willow
     &     14,    !67  sassafras               
     &     19/    !68  American mountain ash
C
      FMBRKT = DBH*B1(EQNUM(ISP))
C
      RETURN
      END
