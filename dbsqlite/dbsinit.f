      SUBROUTINE DBSINIT
      IMPLICIT NONE
C
C $Id$
C
      INCLUDE 'DBSCOM.F77'
C
C  Table name           Subroutine  Control Var  Organization
C  ==================== =========== ===========  ============
C  FVS_ATRTList         dbsatrtls   IATRTLIST    cycle, tree
C  FVS_Cases            dbscase     CASEID       cycle 
C  FVS_Climate          dbsclsum    ICLIM        cycle, species
C  FVS_Compute          dbscmpu     ICOMPUTE     cycle 
C  FVS_CutList          dbscuts     ICUTLIST     cycle, tree
C  FVS_EconHarvestValue dbsecharv   IDBSECON     cycle 
C  FVS_EconSummary      dbsecsum    IDBSECON     cycle 
C  FVS_BurnReport       dbsfmburn   IBURN        cycle             
C  FVS_CanProfile       dbsfmcanpr  ICANPR       cycle, height class
C  FVS_Carbon           dbsfmcrpt   ICMRPT       cycle 
C  FVS_SnagDet          dbsfmdsnag  ISDET        cycle, species, dbhclass
C  FVS_Down_Wood_Cov    dbsfmdwcov  IDWDCOV      cycle 
C  FVS_Down_Wood_Vol    dbsfmdwvol  IDWDVOL      cycle 
C  FVS_Consumption      dbsfmfuel   IFUELC       cycle 
C  FVS_Hrv_Carbon       dbsfmhrpt   ICHRPT       cycle 
C  FVS_Mortality        dbsfmmort   IMORTF       cycle 
C  FVS_PotFire_East     dbsfmpf     IPOTFIRE     cycle           
C  FVS_PotFire          dbsfmpf     IPOTFIRE     cycle 
C  FVS_SnagSum          dbsfmssnag  ISSUM        cycle 
C  FVS_Fuels            dbsfuels    IFUELS       cycle 
C  FVS_DM_Spp_Sum       dbsmis      IDM1         cycle, species
C  FVS_DM_Stnd_Sum      dbsmis      IDM2         cycle 
C  FVS_DM_Sz_Sum        dbsmis      IDM3         cycle 
C  FVS_RD_Sum           dbsrd       IRD1         cycle 
C  FVS_RD_Det           dbsrd       IRD2         cycle 
C  FVS_RD_Beetle        dbsrd       IRD3         cycle 
C  FVS_StrClass         dbsstrclass ISTRCLAS     cycle 
C  FVS_Summary_East     dbssumry    ISUMARY      cycle 
C  FVS_Summary          dbssumry    ISUMARY      cycle 
C  FVS_TreeList         dbstrls     ITREELIST    cycle, tree  
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
      
      RETURN
      END

      SUBROUTINE DBSACTV(LACTV)
      LOGICAL LACTV
      LACTV = .TRUE.
      RETURN
      END

