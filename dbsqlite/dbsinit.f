      SUBROUTINE DBSINIT
      IMPLICIT NONE
C
C DBSQLITE $Id$
C
      INCLUDE 'DBSCOM.F77'
C
C  Table name           Subroutine  Control Var  Organization
C  ==================== =========== ===========  =============================
C  FVS_ATRTList         dbsatrtls   IATRTLIST    case, cycle, tree
C  FVS_Cases            dbscase     CASEID       case 
C  FVS_CalibStats       dbscalib    ICALIB       case, sizeclass, species 
C  FVS_Climate          dbsclsum    ICLIM        case, cycle, species
C  FVS_Compute          dbscmpu     ICOMPUTE     case, cycle 
C  FVS_CutList          dbscuts     ICUTLIST     case, cycle, tree
C  FVS_EconHarvestValue dbsecharv   IDBSECON     case, cycle 
C  FVS_EconSummary      dbsecsum    IDBSECON     case, cycle 
C  FVS_BurnReport       dbsfmburn   IBURN        case, cycle             
C  FVS_CanProfile       dbsfmcanpr  ICANPR       case, cycle, height class
C  FVS_Carbon           dbsfmcrpt   ICMRPT       case, cycle 
C  FVS_SnagDet          dbsfmdsnag  ISDET        case, cycle, species, dbhclass
C  FVS_Down_Wood_Cov    dbsfmdwcov  IDWDCOV      case, cycle 
C  FVS_Down_Wood_Vol    dbsfmdwvol  IDWDVOL      case, cycle 
C  FVS_Consumption      dbsfmfuel   IFUELC       case, cycle 
C  FVS_Hrv_Carbon       dbsfmhrpt   ICHRPT       case, cycle 
C  FVS_Mortality        dbsfmmort   IMORTF       case, cycle 
C  FVS_PotFire_East     dbsfmpf     IPOTFIRE     case, cycle           
C  FVS_PotFire          dbsfmpf     IPOTFIRE     case, cycle 
C  FVS_SnagSum          dbsfmssnag  ISSUM        case, cycle 
C  FVS_Fuels            dbsfuels    IFUELS       case, cycle 
C  FVS_DM_Spp_Sum       dbsmis      IDM1         case, cycle, species
C  FVS_DM_Stnd_Sum      dbsmis      IDM2         case, cycle 
C  FVS_DM_Sz_Sum        dbsmis      IDM3         case, cycle
C  FVS_Regen_Sprouts    dbssprt     IREG1        case, cycle, species 
C  FVS_Regen_SitePrep   dbssiteprep IREG2        case, cycle 
C  FVS_Regen_HabType    dbsplothab  IREG3        case, habitat/forest type
C  FVS_Regen_Tally      dbstally    IREG4        case, cycle, species
C  FVS_Regen_Ingrowth   dbsingrow   IREG5        case, cycle, species
C  FVS_RD_Sum           dbsrd       IRD1         case, cycle 
C  FVS_RD_Det           dbsrd       IRD2         case, cycle 
C  FVS_RD_Beetle        dbsrd       IRD3         case, cycle 
C  FVS_Stats_Species    dbsstats    ISTATS1      case, species
C  FVS_Stats_Stand      dbsstats    ISTATS2      case
C  FVS_StrClass         dbsstrclass ISTRCLAS     case, cycle 
C  FVS_Summary2_East    dbssumry2   ISUMARY      case, cycle, RmvCode
C  FVS_Summary2         dbssumry2   ISUMARY      case, cycle, RmvCode 
C  FVS_Summary_East     dbssumry    ISUMARY      case, cycle 
C  FVS_Summary          dbssumry    ISUMARY      case, cycle 
C  FVS_TreeList         dbstrls     ITREELIST    case, cycle, tree  
C
      CASEID    = ""
      ISUMARY  = 0
      ICOMPUTE = 0
      IATRTLIST= 0
      ITREELIST= 0
      ICUTLIST = 0
      IDM1     = 0
      IDM2     = 0
      IDM3     = 0
      IDM5     = 0
      IDM6     = 0
      IPOTFIRE = 0
      IFUELS   = 0
      ITREEIN  = 0
      IRGIN    = 0
      IFUELC   = 0
      ICMRPT   = 0
      ICHRPT   = 0
      ICLIM    = 0
      IBURN    = 0
      IMORTF   = 0
      ISSUM    = 0
      ISDET    = 0
      IADDCMPU = 0
      I_CMPU   = 0
      ISTRCLAS = 0
      IBMMAIN  = 0
      IBMBKP   = 0
      IBMTREE  = 0
      IBMVOL   = 0
      ICANPR   = 0
      ISPOUT6  = 0
      ISPOUT17 = 0
      ISPOUT21 = 0
      ISPOUT23 = 0
      ISPOUT30 = 0
      ISPOUT31 = 0
      IDWDVOL  = 0
      IDWDCOV  = 0
      IRD1     = 0
      IRD2     = 0
      IRD3     = 0
      ISTATS1  = 0
      ISTATS2  = 0
      IREG1    = 0
      IREG2    = 0
      IREG3    = 0
      IREG4    = 0
      IREG5    = 0
      RETURN
      END

      SUBROUTINE DBSACTV(LACTV)
      LOGICAL LACTV
      LACTV = .TRUE.
      RETURN
      END

