      SUBROUTINE MISINT
***********************************************************************
*  **MISINT--UT  Date of last revision:  07/12/11
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the Utah 24 species version.
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on and off.
*     AFIT:   Array of MISFIT data.
*     ACSP:   Array of CSPARR data.
*     ADGP:   Array of DGPMDR data.
*     APMC:   Array of PMCSP data.
*
*  Common block variables and parameters:
*     CSPARR: From MISCOM; 2-char. representations of all species.
*     DGPDMR: From MISCOM; diameter growth potentials based on species
*                and DMR (0-6).
*     ICYC:   From CONTRL; cycle index number.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MISFIT: From MISCOM; tells which species are affected by DM.
*     PMCSP:  From MISCOM; percent mortality coefficients by species.
*
*  Revision History:
*  19-JUN-00 Lance David (FHTET)
*    Five additional species were added to the FVS Utah variant
*    Two of these are DM hosts BS (index 5, uses PP coefficients
*    and PI (index 11, uses LP coefficients). The other three new
*    codes are JU, OA and OT and are not DM hosts. Species ES has
*    has always been in the UT variant, but NOT a DM host is now
*    a host and uses the DF coefficients. Gary Dixon (FMSC)
*    actually did the work, I just looked it over and added this
*    comment. Thank You, Gary!
*  21-APR-2009 Lance R. David (FMSC)
*    Changed species code JU to WJ (Western Juniper)  
*  01-SEP-2009 Lance R. David (FMSC)
*    Expanded for Utah 24 species variant.  
*  12-JUL-2011 Lance R. David (FMSC)
*    Added arrays for height growth impacts.
*    Impact values must be supplied by MistHMod keyword.
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      REAL AFIT(MAXSP),ADGP(MAXSP,7),AHGP(MAXSP,7),APMC(MAXSP,3)
      CHARACTER*2 ACSP(MAXSP)
      INTEGER I,J

C     SPECIES LIST FOR UTAH VARIANT. ***** 24 species *****
C     
C    SPECIES LIST FOR UT VARIANT.
C    ------FVS UT VARIANT-------  
C     # CD COMMON NAME            SCIENTIFIC NAME
C    -- -- ---------------------  --------------------- 
C     1 WB WHITEBARK PINE         PINUS ALBICAULIS          
C     2 LM LIMBER PINE            PINUS FLEXILIS            
C     3 DF DOUGLAS-FIR            PSEUDOTSUGA MENZIESII     
C     4 WF WHITE FIR              ABIES CONCOLOR            
C     5 BS BLUE SPRUCE            PICEA PUNGENS             
C     6 AS QUAKING ASPEN          POPULUS TREMULOIDES       
C     7 LP LODGEPOLE PINE         PINUS CONTORTA            
C     8 ES ENGLEMANN SPRUCE       PICEA ENGELMANNII         
C     9 AF SUBALPINE FIR          ABIES LASIOCARPA          
C    10 PP PONDEROSA PINE         PINUS PONDEROSA           
C    11 PI COMMON PINYON          PINUS EDULIS              
C    12 WJ WESTERN JUNIPER        JUNIPERUS OCCIDENTALIS    
C    13 GO GAMBEL OAK             QUERCUS GAMBELII            
C    14 PM SINGLELEAF PINYON      PINUS MONOPHYLLA      
C    15 RM ROCKY MOUNTAIN JUNIPER JUNIPERUS SCOPULORUM
C    16 UJ UTAH JUNIPER           JUNIPERUS OSTEOSPERMA
C    17 GB BRISTLECONE PINE       PINUS LONGAEVA
C          (Great Basin)       
C    18 NC NARROWLEAF COTTONWOOD  POPULUS ANGUSTIFOLIA
C    19 FC FREMONT COTTONWOOD     POPULUS FREMONTII
C    20 MC CURLLEAF MOUNTAIN-     CERCOCARPUS LEDIFOLIUS
C          MAHOGANY
C    21 BI BIGTOOTH MAPLE         ACER GRANDIDENTATUM
C    22 BE BOX ELDER              ACER NEGUNDO
C    23 OS OTHER SOFTWOODS        
C    24 OH OTHER HARDWOODS        
C

C.... Species character representations

      DATA (ACSP(I),I=1,MAXSP)/
     & 'WB','LM','DF','WF','BS','AS','LP','ES','AF','PP',
     & 'PI','WJ','GO','PM','RM','UJ','GB','NC','FC','MC',
     & 'BI','BE','OS','OH'/

C.... Species affected by mistletoe

      DATA (AFIT(I),I=1,MAXSP)/
     &   1,   1,   1,   1,   1,   0,   1,   1,   1,   1,
     &   1,   0,   0,   1,   0,   0,   1,   0,   0,   0,
     &   0,   0,   0,   0/

C.... Diameter growth rates

      DATA ((ADGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0,1.0,1.0,1.0,.94,.80,.59,   !  1 WB (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   !  2 LM (    LP  )
     &   1.0,.98,.97,.85,.80,.52,.44,   !  3 DF (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50,   !  4 WF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50,   !  5 BS (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   !  6 AS 
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   !  7 LP (    LP  )
     &   1.0,.98,.97,.85,.80,.52,.44,   !  8 ES (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50,   !  9 AF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50,   ! 10 PP (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   ! 11 PI (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 12 WJ 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 13 GO 
     &   1.0,1.0,1.0,1.0,.94,.80,.59,   ! 14 PM (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 15 RM 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 16 UJ 
     &   1.0,1.0,1.0,.98,.86,.73,.50,   ! 17 GB (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 18 NC 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 19 FC 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 20 MC 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 21 BI 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 22 BE 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,   ! 23 OS 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/   ! 24 OH 

C.... Height growth potential rates
C....
C.... Using Douglas-fir height growth impact values described in:
C....
C.... Marshall, Katy 2007. Permanent plots for measuring spread and
C.... impact of Douglas-fir dwarf mistletoe in the Southern Oregon
C.... Cascades, Pacific Northwest Region: Results of the ten year
C.... remeasurement. USDA Forest Service, Pacific Northwest Region,
C.... Southwest Oregon Forest Insect and Disease Service Center, 
C.... Central Point, Oregon. SWOFIDSC-07-04. 34 pp.
C....
C.... Default values for DF in this table would be:
C.... &   1.0,1.0,1.0,.95,.65,.50,.10,
C.... So that impacts are not unknowingly applied to projections,
C.... the values must be supplied with the MistHMod keyword.
C.... when appropriat default values are developed, they will be
C.... set here.

      DATA ((AHGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/

C.... Mortality coefficients 

      DATA ((APMC(I,J),J=1,3),I=1,MAXSP)
     &  /0.00112, 0.02170,-0.00171,     !  1 WB (    LP  )
     &   0.00112, 0.02170,-0.00171,     !  2 LM (    LP  )
     &   0.01319,-0.01627, 0.00822,     !  3 DF (  DF    )
     &   0.0,     0.00159, 0.00508,     !  4 WF (GF      )
     &   0.00681,-0.00580, 0.00935,     !  5 BS (      PP)
     &   0.0,     0.0,     0.0,         !  6 AS           
     &   0.00112, 0.02170,-0.00171,     !  7 LP (    LP  )
     &   0.01319,-0.01627, 0.00822,     !  8 ES (  DF    )
     &   0.0,     0.00159, 0.00508,     !  9 AF (GF      )
     &   0.00681,-0.00580, 0.00935,     ! 10 PP (      PP)
     &   0.00112, 0.02170,-0.00171,     ! 11 PI (    LP  )
     &   0.0,     0.0,     0.0,         ! 12 WJ           
     &   0.0,     0.0,     0.0,         ! 13 GO           
     &   0.00112, 0.02170,-0.00171,     ! 14 PM (    LP  )
     &   0.0,     0.0,     0.0,         ! 15 RM           
     &   0.0,     0.0,     0.0,         ! 16 UJ           
     &   0.00681,-0.00580, 0.00935,     ! 17 GB (      PP)
     &   0.0,     0.0,     0.0,         ! 18 NC           
     &   0.0,     0.0,     0.0,         ! 19 FC           
     &   0.0,     0.0,     0.0,         ! 20 MC           
     &   0.0,     0.0,     0.0,         ! 21 BI           
     &   0.0,     0.0,     0.0,         ! 22 BE           
     &   0.0,     0.0,     0.0,         ! 23 OS           
     &   0.0,     0.0,     0.0/         ! 24 OH           

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISINT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,10)ICYC
   10 FORMAT(' Begin/end MISINTUT: Cycle = ',I5)

C.... Mistletoe model initializations.

      DO 200 I=1,MAXSP
         MISFIT(I)=AFIT(I)
         CSPARR(I)=ACSP(I)
         DO 100 J=1,7
            DGPDMR(I,J)=ADGP(I,J)
            HGPDMR(I,J)=AHGP(I,J)
  100    CONTINUE
         DO 150 J=1,3
            PMCSP(I,J)=APMC(I,J)
  150    CONTINUE
  200 CONTINUE

      RETURN
      END
