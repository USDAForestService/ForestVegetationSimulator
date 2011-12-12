      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C  **FMBRKT  FIRE-NE-DATE OF LAST REVISION:  05/02/06
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
      REAL B1(39),EQNUM(MAXSP),DBH,FMBRKT
C
      DATA B1/.019, .022, .024, .025, .026, .027, .028, .029, .03,
     & .031, .032, .033, .034, .035, .036, .037, .038, .039, .04, .041,
     & .042, .043, .044, .045, .046, .047, .048, .049, .05, .052, .055,
     & .057, .059, .06, .062, .063, .068, .072, .081/
C
      DATA EQNUM/
     &     10,    !1   balsam fir
     &     10,    !2   tamarack
     &      4,    !3   white spruce
     &     13,    !4   red spruce
     &      8,    !5   Norway spruce
     &     11,    !6   black spruce
     &     13,    !7   other spruce, used picea species
     &     22,    !8   red pine
     &     24,    !9   eastern white pine
     &     30,    !10  loblolly pine                                        
     &     12,    !11  Virginia pine                            
     &      4,    !12  northern white-cedar                                         
     &      4,    !13  Atl. white-cedar                                         
     &     17,    !14  eastern redcedar                         
     &     12,    !15  other cedar , used thuja species                        
     &     18,    !16  eastern hemlock                              
     &     19,    !17  other hemlock, used western / mountain hemlock                          
     &      9,    !18  other pines, used pinus species                            
     &     19,    !19  jack pine                              
     &     16,    !20  shortleaf pine                                
     &     19,    !21  table mtn. pine                           
     &     24,    !22  pitch pine                          
     &     35,    !23  pond pine                               
     &      9,    !24  Scotch pine                                
     &      9,    !25  other softwoods, used pinus species                             
     &      7,    !26  red maple                          
     &     12,    !27  sugar maple                                
     &     14,    !28  black maple                              
     &     10,    !29  silver maple                              
     &     10,    !30  yellow birch                             
     &      9,    !31  sweet birch                             
     &      8,    !32  river birch                              
     &      6,    !33  paper birch                              
     &     12,    !34  gray birch, used betula species                              
     &     23,    !35  hickory, used carya species                              
     &     16,    !36  pignut hickory                                  
     &     22,    !37  shellbark hickory                           
     &     19,    !38  shagbark hickory                        
     &     22,    !39  mockernut hickory                         
     &      4,    !40  American beech                        
     &     21,    !41  ash, used fraxinus species                                       
     &     21,    !42  white ash                                      
     &     14,    !43  black ash                                
     &     18,    !44  green ash                                
     &     16,    !45  pumpkin ash                                
     &     20,    !46  yellow poplar                              
     &     15,    !47  sweetgum                            
     &     15,    !48  cucumbertree                                 
     &     23,    !49  quaking aspen                                         
     &     19,    !50  balsam poplar                            
     &     19,    !51  eastern cottonwood                            
     &     18,    !52  bigtooth aspen                       
     &     29,    !53  swamp cottonwood                           
     &      9,    !54  black cherry                         
     &     19,    !55  white oak                             
     &     21,    !56  bur oak                                
     &     21,    !57  chinkapin oak                                  
     &     23,    !58  post oak                            
     &     24,    !59  other oaks, used quercus species                                
     &     19,    !60  scarlet oak                               
     &     20,    !61  shingle oak                              
     &     15,    !62  water oak                              
     &     20,    !63  pin oak                                
     &     28,    !64  chestnut oak                                  
     &     24,    !65  swamp white oak                                       
     &     25,    !66  swamp chestnut oak                          
     &     21,    !67  northern red oak                       
     &     23 ,   !68  southern red oak
     &     24,    !69  black oak
     &     23,    !70  cherrybark oak
     &     19,    !71  other hardwoods, used middle of this group
     &     15,    !72  buckeye, used ohio buckeye
     &     29,    !73  yellow buckeye
     &     29,    !74  water birch
     &     15,    !75  hackberry, used sugarberry
     &     20,    !76  persimmon
     &     21,    !77  American holly
     &     20,    !78  butternut
     &     20,    !79  black walnut
     &     16,    !80  Osage-orange
     &     18,    !81  magnolia, used magnolia species
     &     19,    !82  sweetbay
     &     22,    !83  apple sp.
     &      9,    !84  water tupelo
     &     18,    !85  blackgum
     &     15,    !86  sourwood
     &     29,    !87  Paulownia
     &     12,    !88  sycamore
     &     20,    !89  willow oak
     &     28,    !90  black locust
     &     19,    !91  black willow
     &     14,    !92  sassafras
     &     19,    !93  American basswood
     &     29,    !94  white basswood
     &     18,    !95  other elm, used ulmus species
     &     10,    !96  American elm
     &     11,    !97  slippery elm
     &     24,    !98  non-commercial hardwoods, in the middle of this group
     &     13,    !99  boxelder
     &     24,    !100 striped maple
     &     29,    !101 ailanthus
     &     29,    !102 serviceberry
     &      9,    !103 American hornbeam
     &     20,    !104 flowering dogwood
     &     17,    !105 hawthorn
     &     16,    !106 eastern hophornbeam
     &     29,    !107 plum, cherry, used prunus species
     &     24/    !108 pine cherry                             
C
      FMBRKT = DBH*B1(EQNUM(ISP))
C
      RETURN
      END
