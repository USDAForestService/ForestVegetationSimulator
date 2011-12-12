      BLOCK DATA DBSBLKD
C----------
C  **DBSBLKD--DBS  DATE OF LAST REVISION: 10/31/2011
C----------
C
C     STAND VISUALIZATION GENERATION
C     D.L.GAMMEL -- SEM -- JULY 2002
C
C     INITIALIZE DATABASE VARIABLES
C
COMMONS
C
C
      INCLUDE 'DBSCOM.F77'
C
C
COMMONS
C     
      DATA SUMRYID/-1/
      DATA CMPUID/-1/
      DATA TREEOUTID/-1/
      DATA CUTSID/-1/
      DATA IATRTLID/-1/
      DATA FMPFID/-1/
      DATA FUELID/-1/
      DATA CONID/-1/
      DATA BURNID/-1/
      DATA CMRPTID/-1/
      DATA CHRPTID/-1/
      DATA MORTID/-1/
      DATA SSUMID/-1/
      DATA SDETID/-1/
      DATA CANPRID/-1/
      DATA DWDVID/-1/
      DATA DWDCID/-1/            
      DATA STRCLID/-1/
      DATA BM_MNID/-1/
      DATA BMTREID/-1/
      DATA BMVOLID/-1/
      DATA BMBKPID/-1/
      DATA DSNOUT/'FVSOut.mdb'/
      DATA ConnHndlOut/-1/
      DATA EnvHndlOut/-1/
      DATA DSNIN/'FVSIn.mdb'/
      DATA ConnHndlIn/-1/
      DATA EnvHndlIn/-1/

      END
