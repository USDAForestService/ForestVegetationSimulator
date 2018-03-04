      SUBROUTINE MISINT
***********************************************************************
C MISTOE $Id: misint.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
*----------------------------------------------------------------------
*  Purpose:
*     Mistletoe parameter initialization routine. This routine is
*  variant dependent and sets the variant dependent variables for other
*  mistletoe routines. This is the Central Rockies version.
*  ********
*  **NOTE** Growth and mortality coefficients are from the Utah variant
*  ********
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
*     IMODTY: From PLOT; input model type code.
*     JOSTND: From CONTRL; logical unit number for stand output.
*     MAXSP:  From PRGPRM; maximum # species.
*     MISFIT: From MISCOM; tells which species are affected by DM.
*     PMCSP:  From MISCOM; percent mortality coefficients by species.
*     USEMRT: From MISCOM; true if using this model's mortality calcs.
*
*
*  24-FEB-98 Update Lance David
*     Changed use of ITYPE (PLOT common variable) to
*     IMODTY (PLOT common variable) which is set by FVS MODTYPE keyword.
*     The removal of embedded mistletoe effects in the FVS Central Rockies
*     variant prompted the following:
*       Activation of mortality (USEMRT=.TRUE.).
*       Activation of diameter growth potential values.
*
*  16-DEC-98 Update Matt Oberle (MJO DEC98)
*     In response to Bulletin #363 (CR species list expanded to include
*       24 species -- realigned the surrogate species assignments for
*       consistency between model types for a given species).
*     Expanded the data statements to include the new, global species 
*       list, changed dimension of ACSP(5,MAXSP) to (MAXSP).
*     Blank data statements for the future Aspen model type (6) were
*       added and commented out.
*     Although parmameters are currently the same for each species
*       across model types, I chose to keep this model type-
*       specific structure for flixibiltiy -- we may find the need for
*       species x model type specific parameters in the future. The
*       only data block that was collapsed to 1 dimension was the 
*       'species character representations'.
*     Blank data statements for the future Aspen model type (6) were
*       added and commented out.    
*
*  15-MAR-00 Update Lance David
*     The species identified as dwarf mistletoe host had not been
*       addressed when the tree species represented by the Central Rockies
*       variant was expanded to 24 and made consistant across all Model
*       types. As a result, only 5 tree species were recognized as host.
*       Parameters assigned for growth and mortality are the same across
*       all model types. Having mistletoe available for the Black Hills
*       model type is not a concern, because the model won't be 
*       automatically activated unless there are mistletoe damage codes
*       in the data set, in which case the data may not be Black Hills
*       data and inclusion of mistletoe may be appropriate. 
*  21-APR-09 Lance R. David (FMSC)
*     Changed species code WP to SW (Southwestern White Pine).
*  17-JUN-09 Gary Dixon (FMSC)
*     Expanded from 24 species to 38 species
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
      INCLUDE 'PLOT.F77'
      INCLUDE 'MISCOM.F77'
C.... Variable declarations.
      LOGICAL DEBUG
      INTEGER I,J,K,LTYPE,AFIT(5,MAXSP)
      REAL ADGP(5,MAXSP,7),AHGP(MAXSP,7),APMC(5,MAXSP,3)
      CHARACTER*2 ACSP(MAXSP)
C.... Data statements.
C.... All the data for the 5 subvariants is stored in these data
C.... statements.  We learn at run-time which subvariant the user
C.... selected, and copy these values to the working arrays.
C....   (1) Southwest Mixed Conifer.
C....   (2) Southwest PP and PJ.
C....   (3) Black Hills.
C....   (4) Spruce-Fir.
C....   (5) Lodgepole Pine.
C....   (6) Aspen (to be completed)
C
C.... Species affected or unaffected by dwarf mistletoe.
      DATA ((AFIT(I,J),J=1,MAXSP),I=1,1)/
     &  1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
     &  1, 1, 1, 1, 1, 0, 1, 1, 0, 0,
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &  0, 0, 1, 1, 1, 1, 0, 0/
      DATA ((AFIT(I,J),J=1,MAXSP),I=2,2)/
     &  1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
     &  1, 1, 1, 1, 1, 0, 1, 1, 0, 0,
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &  0, 0, 1, 1, 1, 1, 0, 0/
      DATA ((AFIT(I,J),J=1,MAXSP),I=3,3)/
     &  1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
     &  1, 1, 1, 1, 1, 0, 1, 1, 0, 0,
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &  0, 0, 1, 1, 1, 1, 0, 0/
      DATA ((AFIT(I,J),J=1,MAXSP),I=4,4)/
     &  1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
     &  1, 1, 1, 1, 1, 0, 1, 1, 0, 0,
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &  0, 0, 1, 1, 1, 1, 0, 0/
      DATA ((AFIT(I,J),J=1,MAXSP),I=5,5)/
     &  1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
     &  1, 1, 1, 1, 1, 0, 1, 1, 0, 0,
     &  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &  0, 0, 1, 1, 1, 1, 0, 0/
C      DATA ((AFIT(I,J),J=1,MAXSP),I=6,6)/
C     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
C     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
C     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
C     & 0, 0, 0, 0, 0, 0, 0, 0/
C
C.... Species character representations.
      DATA (ACSP(J),J=1,MAXSP)/
     &   'AF','CB','DF','GF','WF','MH','RC','WL','BC','LM',
     &   'LP','PI','PP','WB','SW','UJ','BS','ES','WS','AS',
     &   'NC','PW','GO','AW','EM','BK','SO','PB','AJ','RM',
     &   'OJ','ER','PM','PD','AZ','CI','OS','OH'/
C
C.... Diameter growth rates.
C.... Southwest Mixed Conifer.
      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=1,1)/
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 1  AF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 2  CB (GF      )
     &   1.0,.98,.97,.85,.80,.52,.44, ! 3  DF (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 4  GF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 5  WF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 6  MH (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 7  RC not host
     &   1.0,.94,.92,.88,.84,.58,.54, ! 8  WL
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 9  BC (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 10 LM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 11 LP (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 12 PI (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 13 PP (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 14 WB (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 15 SW (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16 UJ not host
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 17 BS (      PP)
     &   1.0,.98,.97,.85,.80,.52,.44, ! 18 ES (  DF    )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19 WS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20 AS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21 NC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22 PW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23 GO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24 AW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25 EM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26 BK not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27 SO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28 PB not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29 AJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30 RM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31 OJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32 ER not host
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 33 PM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 34 PD (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 35 AZ (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 36 CI (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37 OS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

C.... Southwest PP and PJ.
      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=2,2)/
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 1  AF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 2  CB (GF      )
     &   1.0,.98,.97,.85,.80,.52,.44, ! 3  DF (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 4  GF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 5  WF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 6  MH (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 7  RC not host
     &   1.0,.94,.92,.88,.84,.58,.54, ! 8  WL
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 9  BC (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 10 LM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 11 LP (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 12 PI (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 13 PP (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 14 WB (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 15 SW (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16 UJ not host
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 17 BS (      PP)
     &   1.0,.98,.97,.85,.80,.52,.44, ! 18 ES (  DF    )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19 WS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20 AS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21 NC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22 PW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23 GO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24 AW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25 EM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26 BK not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27 SO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28 PB not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29 AJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30 RM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31 OJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32 ER not host
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 33 PM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 34 PD (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 35 AZ (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 36 CI (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37 OS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

C.... Black Hills.
      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=3,3)/
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 1  AF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 2  CB (GF      )
     &   1.0,.98,.97,.85,.80,.52,.44, ! 3  DF (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 4  GF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 5  WF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 6  MH (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 7  RC not host
     &   1.0,.94,.92,.88,.84,.58,.54, ! 8  WL
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 9  BC (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 10 LM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 11 LP (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 12 PI (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 13 PP (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 14 WB (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 15 SW (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16 UJ not host
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 17 BS (      PP)
     &   1.0,.98,.97,.85,.80,.52,.44, ! 18 ES (  DF    )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19 WS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20 AS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21 NC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22 PW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23 GO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24 AW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25 EM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26 BK not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27 SO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28 PB not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29 AJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30 RM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31 OJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32 ER not host
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 33 PM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 34 PD (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 35 AZ (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 36 CI (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37 OS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host

C.... Spruce-Fir.
      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=4,4)/
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 1  AF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 2  CB (GF      )
     &   1.0,.98,.97,.85,.80,.52,.44, ! 3  DF (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 4  GF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 5  WF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 6  MH (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 7  RC not host
     &   1.0,.94,.92,.88,.84,.58,.54, ! 8  WL
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 9  BC (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 10 LM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 11 LP (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 12 PI (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 13 PP (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 14 WB (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 15 SW (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16 UJ not host
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 17 BS (      PP)
     &   1.0,.98,.97,.85,.80,.52,.44, ! 18 ES (  DF    )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19 WS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20 AS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21 NC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22 PW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23 GO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24 AW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25 EM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26 BK not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27 SO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28 PB not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29 AJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30 RM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31 OJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32 ER not host
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 33 PM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 34 PD (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 35 AZ (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 36 CI (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37 OS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host
C.... Lodgepole Pine.
      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=5,5)/
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 1  AF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 2  CB (GF      )
     &   1.0,.98,.97,.85,.80,.52,.44, ! 3  DF (  DF    )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 4  GF (GF      )
     &   1.0,1.0,1.0,.98,.95,.70,.50, ! 5  WF (GF      )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 6  MH (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 7  RC not host
     &   1.0,.94,.92,.88,.84,.58,.54, ! 8  WL
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 9  BC (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 10 LM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 11 LP (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 12 PI (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 13 PP (      PP)
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 14 WB (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 15 SW (    LP  )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16 UJ not host
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 17 BS (      PP)
     &   1.0,.98,.97,.85,.80,.52,.44, ! 18 ES (  DF    )
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19 WS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20 AS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21 NC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22 PW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23 GO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24 AW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25 EM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26 BK not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27 SO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28 PB not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29 AJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30 RM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31 OJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32 ER not host
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 33 PM (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 34 PD (    LP  )
     &   1.0,1.0,1.0,1.0,.94,.80,.59, ! 35 AZ (    LP  )
     &   1.0,1.0,1.0,.98,.86,.73,.50, ! 36 CI (      PP)
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37 OS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host 
C.... Aspen.
C      DATA (((ADGP(I,J,K),K=1,7),J=1,MAXSP),I=6,6)/
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  1
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  2
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  3
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  4
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  5
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  6
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  7
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  8
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, !  9
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 10
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 11
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 12
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 13
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 14
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 15
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 17
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 18
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 33
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 34
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 35
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 36
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37
C     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38

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
C....
C.... Special note: Height growth potential values are not set
C.... based on Model type (subvariant) like diameter growth and 
C.... mortaility.
C....
      DATA ((AHGP(I,J),J=1,7),I=1,MAXSP)
     &  /1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 1  AF 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 2  CB 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 3  DF 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 4  GF 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 5  WF 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 6  MH 
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 7  RC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 8  WL
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 9  BC
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 10 LM
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 11 LP
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 12 PI
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 13 PP
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 14 WB
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 15 SW
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 16 UJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 17 BS
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 18 ES
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 19 WS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 20 AS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 21 NC not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 22 PW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 23 GO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 24 AW not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 25 EM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 26 BK not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 27 SO not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 28 PB not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 29 AJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 30 RM not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 31 OJ not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 32 ER not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 33 PM
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 34 PD
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 35 AZ
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 36 CI
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0, ! 37 OS not host
     &   1.0,1.0,1.0,1.0,1.0,1.0,1.0/ ! 38 OH not host 

C.... Mortality coefficients.
C.... Southwest Mixed Conifer.
      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=1,1)/
     &   0.0,     0.00159, 0.00508, ! 1  AF (GF      )
     &   0.0,     0.00159, 0.00508, ! 2  CB (GF      )
     &   0.01319,-0.01627, 0.00822, ! 3  DF (  DF    )
     &   0.0,     0.00159, 0.00508, ! 4  GF (GF      )
     &   0.0,     0.00159, 0.00508, ! 5  WF (GF      )
     &   0.00681,-0.00580, 0.00935, ! 6  MH (      PP)
     &   0.0,0.0,0.0,               ! 7  RC not host  
     &   0.01319,-0.01627, 0.00822, ! 8  WL (  DF    )
     &   0.00681,-0.00580, 0.00935, ! 9  BC (      PP)
     &   0.00112, 0.02170,-0.00171, ! 10 LM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 11 LP (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 12 PI (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 13 PP (      PP)
     &   0.00112, 0.02170,-0.00171, ! 14 WB (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 15 SW (    LP  )
     &   0.0,0.0,0.0,               ! 16 UJ not host  
     &   0.00681,-0.00580, 0.00935, ! 17 BS (      PP)
     &   0.01319,-0.01627, 0.00822, ! 18 ES (  DF    )
     &   0.0,0.0,0.0,               ! 19 WS not host  
     &   0.0,0.0,0.0,               ! 20 AS not host  
     &   0.0,0.0,0.0,               ! 21 NC not host  
     &   0.0,0.0,0.0,               ! 22 PW not host  
     &   0.0,0.0,0.0,               ! 23 GO not host  
     &   0.0,0.0,0.0,               ! 24 AW not host  
     &   0.0,0.0,0.0,               ! 25 EM not host  
     &   0.0,0.0,0.0,               ! 26 BK not host  
     &   0.0,0.0,0.0,               ! 27 SO not host  
     &   0.0,0.0,0.0,               ! 28 PB not host  
     &   0.0,0.0,0.0,               ! 29 AJ not host  
     &   0.0,0.0,0.0,               ! 30 RM not host  
     &   0.0,0.0,0.0,               ! 31 OJ not host  
     &   0.0,0.0,0.0,               ! 32 ER not host  
     &   0.00112, 0.02170,-0.00171, ! 33 PM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 34 PD (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 35 AZ (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 36 CI (      PP)
     &   0.0,0.0,0.0,               ! 37 OS not host  
     &   0.0,0.0,0.0/               ! 38 OH not host  

C.... Southwest PP and PJ.
      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=2,2)/
     &   0.0,     0.00159, 0.00508, ! 1  AF (GF      )
     &   0.0,     0.00159, 0.00508, ! 2  CB (GF      )
     &   0.01319,-0.01627, 0.00822, ! 3  DF (  DF    )
     &   0.0,     0.00159, 0.00508, ! 4  GF (GF      )
     &   0.0,     0.00159, 0.00508, ! 5  WF (GF      )
     &   0.00681,-0.00580, 0.00935, ! 6  MH (      PP)
     &   0.0,0.0,0.0,               ! 7  RC not host  
     &   0.01319,-0.01627, 0.00822, ! 8  WL (  DF    )
     &   0.00681,-0.00580, 0.00935, ! 9  BC (      PP)
     &   0.00112, 0.02170,-0.00171, ! 10 LM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 11 LP (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 12 PI (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 13 PP (      PP)
     &   0.00112, 0.02170,-0.00171, ! 14 WB (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 15 SW (    LP  )
     &   0.0,0.0,0.0,               ! 16 UJ not host  
     &   0.00681,-0.00580, 0.00935, ! 17 BS (      PP)
     &   0.01319,-0.01627, 0.00822, ! 18 ES (  DF    )
     &   0.0,0.0,0.0,               ! 19 WS not host  
     &   0.0,0.0,0.0,               ! 20 AS not host  
     &   0.0,0.0,0.0,               ! 21 NC not host  
     &   0.0,0.0,0.0,               ! 22 PW not host  
     &   0.0,0.0,0.0,               ! 23 GO not host  
     &   0.0,0.0,0.0,               ! 24 AW not host  
     &   0.0,0.0,0.0,               ! 25 EM not host  
     &   0.0,0.0,0.0,               ! 26 BK not host  
     &   0.0,0.0,0.0,               ! 27 SO not host  
     &   0.0,0.0,0.0,               ! 28 PB not host  
     &   0.0,0.0,0.0,               ! 29 AJ not host  
     &   0.0,0.0,0.0,               ! 30 RM not host  
     &   0.0,0.0,0.0,               ! 31 OJ not host  
     &   0.0,0.0,0.0,               ! 32 ER not host  
     &   0.00112, 0.02170,-0.00171, ! 33 PM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 34 PD (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 35 AZ (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 36 CI (      PP)
     &   0.0,0.0,0.0,               ! 37 OS not host  
     &   0.0,0.0,0.0/               ! 38 OH not host  

C.... Black Hills.
      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=3,3)/
     &   0.0,     0.00159, 0.00508, ! 1  AF (GF      )
     &   0.0,     0.00159, 0.00508, ! 2  CB (GF      )
     &   0.01319,-0.01627, 0.00822, ! 3  DF (  DF    )
     &   0.0,     0.00159, 0.00508, ! 4  GF (GF      )
     &   0.0,     0.00159, 0.00508, ! 5  WF (GF      )
     &   0.00681,-0.00580, 0.00935, ! 6  MH (      PP)
     &   0.0,0.0,0.0,               ! 7  RC not host  
     &   0.01319,-0.01627, 0.00822, ! 8  WL (  DF    )
     &   0.00681,-0.00580, 0.00935, ! 9  BC (      PP)
     &   0.00112, 0.02170,-0.00171, ! 10 LM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 11 LP (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 12 PI (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 13 PP (      PP)
     &   0.00112, 0.02170,-0.00171, ! 14 WB (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 15 SW (    LP  )
     &   0.0,0.0,0.0,               ! 16 UJ not host  
     &   0.00681,-0.00580, 0.00935, ! 17 BS (      PP)
     &   0.01319,-0.01627, 0.00822, ! 18 ES (  DF    )
     &   0.0,0.0,0.0,               ! 19 WS not host  
     &   0.0,0.0,0.0,               ! 20 AS not host  
     &   0.0,0.0,0.0,               ! 21 NC not host  
     &   0.0,0.0,0.0,               ! 22 PW not host  
     &   0.0,0.0,0.0,               ! 23 GO not host  
     &   0.0,0.0,0.0,               ! 24 AW not host  
     &   0.0,0.0,0.0,               ! 25 EM not host  
     &   0.0,0.0,0.0,               ! 26 BK not host  
     &   0.0,0.0,0.0,               ! 27 SO not host  
     &   0.0,0.0,0.0,               ! 28 PB not host  
     &   0.0,0.0,0.0,               ! 29 AJ not host  
     &   0.0,0.0,0.0,               ! 30 RM not host  
     &   0.0,0.0,0.0,               ! 31 OJ not host  
     &   0.0,0.0,0.0,               ! 32 ER not host  
     &   0.00112, 0.02170,-0.00171, ! 33 PM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 34 PD (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 35 AZ (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 36 CI (      PP)
     &   0.0,0.0,0.0,               ! 37 OS not host  
     &   0.0,0.0,0.0/               ! 38 OH not host  

C.... Spruce-Fir.
      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=4,4)/
     &   0.0,     0.00159, 0.00508, ! 1  AF (GF      )
     &   0.0,     0.00159, 0.00508, ! 2  CB (GF      )
     &   0.01319,-0.01627, 0.00822, ! 3  DF (  DF    )
     &   0.0,     0.00159, 0.00508, ! 4  GF (GF      )
     &   0.0,     0.00159, 0.00508, ! 5  WF (GF      )
     &   0.00681,-0.00580, 0.00935, ! 6  MH (      PP)
     &   0.0,0.0,0.0,               ! 7  RC not host  
     &   0.01319,-0.01627, 0.00822, ! 8  WL (  DF    )
     &   0.00681,-0.00580, 0.00935, ! 9  BC (      PP)
     &   0.00112, 0.02170,-0.00171, ! 10 LM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 11 LP (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 12 PI (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 13 PP (      PP)
     &   0.00112, 0.02170,-0.00171, ! 14 WB (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 15 SW (    LP  )
     &   0.0,0.0,0.0,               ! 16 UJ not host  
     &   0.00681,-0.00580, 0.00935, ! 17 BS (      PP)
     &   0.01319,-0.01627, 0.00822, ! 18 ES (  DF    )
     &   0.0,0.0,0.0,               ! 19 WS not host  
     &   0.0,0.0,0.0,               ! 20 AS not host  
     &   0.0,0.0,0.0,               ! 21 NC not host  
     &   0.0,0.0,0.0,               ! 22 PW not host  
     &   0.0,0.0,0.0,               ! 23 GO not host  
     &   0.0,0.0,0.0,               ! 24 AW not host  
     &   0.0,0.0,0.0,               ! 25 EM not host  
     &   0.0,0.0,0.0,               ! 26 BK not host  
     &   0.0,0.0,0.0,               ! 27 SO not host  
     &   0.0,0.0,0.0,               ! 28 PB not host  
     &   0.0,0.0,0.0,               ! 29 AJ not host  
     &   0.0,0.0,0.0,               ! 30 RM not host  
     &   0.0,0.0,0.0,               ! 31 OJ not host  
     &   0.0,0.0,0.0,               ! 32 ER not host  
     &   0.00112, 0.02170,-0.00171, ! 33 PM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 34 PD (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 35 AZ (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 36 CI (      PP)
     &   0.0,0.0,0.0,               ! 37 OS not host  
     &   0.0,0.0,0.0/               ! 38 OH not host  

C.... Lodgepole Pine.
      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=5,5)/
     &   0.0,     0.00159, 0.00508, ! 1  AF (GF      )
     &   0.0,     0.00159, 0.00508, ! 2  CB (GF      )
     &   0.01319,-0.01627, 0.00822, ! 3  DF (  DF    )
     &   0.0,     0.00159, 0.00508, ! 4  GF (GF      )
     &   0.0,     0.00159, 0.00508, ! 5  WF (GF      )
     &   0.00681,-0.00580, 0.00935, ! 6  MH (      PP)
     &   0.0,0.0,0.0,               ! 7  RC not host  
     &   0.01319,-0.01627, 0.00822, ! 8  WL (  DF    )
     &   0.00681,-0.00580, 0.00935, ! 9  BC (      PP)
     &   0.00112, 0.02170,-0.00171, ! 10 LM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 11 LP (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 12 PI (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 13 PP (      PP)
     &   0.00112, 0.02170,-0.00171, ! 14 WB (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 15 SW (    LP  )
     &   0.0,0.0,0.0,               ! 16 UJ not host  
     &   0.00681,-0.00580, 0.00935, ! 17 BS (      PP)
     &   0.01319,-0.01627, 0.00822, ! 18 ES (  DF    )
     &   0.0,0.0,0.0,               ! 19 WS not host  
     &   0.0,0.0,0.0,               ! 20 AS not host  
     &   0.0,0.0,0.0,               ! 21 NC not host  
     &   0.0,0.0,0.0,               ! 22 PW not host  
     &   0.0,0.0,0.0,               ! 23 GO not host  
     &   0.0,0.0,0.0,               ! 24 AW not host  
     &   0.0,0.0,0.0,               ! 25 EM not host  
     &   0.0,0.0,0.0,               ! 26 BK not host  
     &   0.0,0.0,0.0,               ! 27 SO not host  
     &   0.0,0.0,0.0,               ! 28 PB not host  
     &   0.0,0.0,0.0,               ! 29 AJ not host  
     &   0.0,0.0,0.0,               ! 30 RM not host  
     &   0.0,0.0,0.0,               ! 31 OJ not host  
     &   0.0,0.0,0.0,               ! 32 ER not host  
     &   0.00112, 0.02170,-0.00171, ! 33 PM (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 34 PD (    LP  )
     &   0.00112, 0.02170,-0.00171, ! 35 AZ (    LP  )
     &   0.00681,-0.00580, 0.00935, ! 36 CI (      PP)
     &   0.0,0.0,0.0,               ! 37 OS not host  
     &   0.0,0.0,0.0/               ! 38 OH not host  

C.... Aspen.
C      DATA (((APMC(I,J,K),K=1,3),J=1,MAXSP),I=6,6)/
C     &   0.0,0.0,0.0, !  1
C     &   0.0,0.0,0.0, !  2
C     &   0.0,0.0,0.0, !  3
C     &   0.0,0.0,0.0, !  4
C     &   0.0,0.0,0.0, !  5
C     &   0.0,0.0,0.0, !  6
C     &   0.0,0.0,0.0, !  7
C     &   0.0,0.0,0.0, !  8
C     &   0.0,0.0,0.0, !  9
C     &   0.0,0.0,0.0, ! 10
C     &   0.0,0.0,0.0, ! 11
C     &   0.0,0.0,0.0, ! 12
C     &   0.0,0.0,0.0, ! 13
C     &   0.0,0.0,0.0, ! 14
C     &   0.0,0.0,0.0, ! 15
C     &   0.0,0.0,0.0, ! 16
C     &   0.0,0.0,0.0, ! 17
C     &   0.0,0.0,0.0, ! 18
C     &   0.0,0.0,0.0, ! 19
C     &   0.0,0.0,0.0, ! 20
C     &   0.0,0.0,0.0, ! 21
C     &   0.0,0.0,0.0, ! 22
C     &   0.0,0.0,0.0, ! 23
C     &   0.0,0.0,0.0, ! 24
C     &   0.0,0.0,0.0, ! 25
C     &   0.0,0.0,0.0, ! 26
C     &   0.0,0.0,0.0, ! 27
C     &   0.0,0.0,0.0, ! 28
C     &   0.0,0.0,0.0, ! 29
C     &   0.0,0.0,0.0, ! 30
C     &   0.0,0.0,0.0, ! 31
C     &   0.0,0.0,0.0, ! 32
C     &   0.0,0.0,0.0, ! 33
C     &   0.0,0.0,0.0, ! 34
C     &   0.0,0.0,0.0, ! 35
C     &   0.0,0.0,0.0, ! 36
C     &   0.0,0.0,0.0, ! 37
C     &   0.0,0.0,0.0/ ! 38
C.... Check for debug.
      CALL DBCHK(DEBUG,'MISINT',6,ICYC)
 
      IF(DEBUG) WRITE(JOSTND,10)ICYC,IMODTY
   10 FORMAT(' Begin MISINTCR: Cycle = ',I5,' IMODTY = ',I5)
C.... Mistletoe model initializations.
C.... 24-feb-98 Lance David
C.... Commented out so that mortality is used, now that mistletoe effects
C.... have been removed from the base FVS Central Rockies by Gary Dixon.
C.... USEMRT=.FALSE.
C
      IF (IMODTY .LT. 1 .OR. IMODTY .GT. 5) THEN
         LTYPE=1
      ELSE
         LTYPE=IMODTY
      ENDIF
      DO 200 I=1,MAXSP
         MISFIT(I)=AFIT(LTYPE,I)
C
C.... 17-DEC-98 Matt Oberle (MJO DEC98)
C.... Changed dimension of ACSP(LTYPE,I) to ACSP(I)
         CSPARR(I)=ACSP(I)
C
C.... Temporary code to set default growth potentials.
C.... Eventually, we would like correct coefficients in the
C.... data tables so we can use the following statement:
C....
C.... 24-feb-98 Lance David
C.... The values in ADGP array are used now that mistletoe effects
C.... have been removed from the base FVS Central Rockies by Gary Dixon.
C.... Prior to this time, DGPDMR(I,J)=1.0 was used.

         DO 100 J=1,7
            DGPDMR(I,J)=ADGP(LTYPE,I,J)
            HGPDMR(I,J)=AHGP(I,J)
  100    CONTINUE
         DO 150 J=1,3
            PMCSP(I,J)=APMC(LTYPE,I,J)
  150    CONTINUE
  200 CONTINUE

C.... Common return.
 9000 CONTINUE
      IF(DEBUG) WRITE(JOSTND,9010)ICYC,LTYPE
 9010 FORMAT(' End MISINTCR: Cycle = ',I5,' LTYPE = ',I5)
      RETURN
      END
