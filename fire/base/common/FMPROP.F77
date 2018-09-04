C----------
C FIRE-BASE-COMMON $Id$
C----------
C  **FMPROP  FIRE
*-----------------------------------------------------------------------
*  PURPOSE:
*     COMMON BLOCK VARIABLES RELATED TO THE FATE OF C IN HARVEST PRODUCTS
*     INITIALIZED THROUGH DATA STATEMENT IN VARIANT **FMCBLK**
*-----------------------------------------------------------------------
*

C     FAPROP(I,J,K,L):
C       I: 1:2 Region Code (most variants are 1; some have 2,
C              depending on habitat)
C       J: 1:101 - Yr 0-100
C       K: 1:3   - Data; 1=INUSE,2=LANDFILL,3=ENERGY (4=EMITTED defunct)
C       M: 1:2   - 1=PULP,2=SAWLOG
C       N: 1:2   - 1=SOFTWOOD, 2=HARDWOOD
C     BIOGRP(MAXSP): SPECIES GROUPING FOR THE JENKINS EQUATIONS

      REAL     FAPROP(2,101,3,2,2)
      INTEGER  BIOGRP(MAXSP)

      COMMON /FMBLK2/ FAPROP, BIOGRP



