C----------
C VOLUME $Id$
C----------
      SUBROUTINE MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,
     >   MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >   PROD)
c     program assigns regional merchandizing rules to be used with
c      profile models
C
C     SETS MERCHANDIZING STANDARDS AND SOME ERROR CHECKNING
C
!== last modified  08-25-2015
!REV  Revised TDH 12/15/10 accidentally had set trim to 0
!     for region 5 for testing and forgot to revert.  fixed.
C     YW 3/25/14 added PROD as input parameter and changed region 3 MINLEN and MINLENT
C                added merch rule for DOD (region 11) using R6 rules
C     YW 04/15/14 Added region 9 Clark merch rule.
C     YW 02/13/15 Changed the merch rule for Region 3 MINLEN and MINLENT to 2'
C     YW 08/25/15 Added merch rule for Region 8 Clark equation
C
      CHARACTER*1 COR 
      CHARACTER*2 FORST, PROD                 
      CHARACTER*3 MDL                 
      character*10 VOLEQ
      INTEGER EVOD,OPT,REGN,spp
      REAL MAXLEN,MINLEN,MERCHL,MTOPP,MTOPS,STUMP,TRIM
      REAL MINLENT,MINBFD,BTR,DBTBH,DBHOB
      CHARACTER*2 CDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      CDANUW(1:2) = FORST(1:2)
C
                  
      IF(BTR.GT.0.0 .AND. DBTBH.LE.0) DBTBH = DBHOB-(DBHOB*BTR/100.0)
      
      MDL = VOLEQ(4:6)
      IF(REGN.EQ.1) THEN
         IF(MDL.EQ.'FW2' .OR. MDL.EQ.'fw2' .OR.
     >      MDL.EQ.'FW3' .OR. MDL.EQ.'fw3')THEN

            COR='Y'
            EVOD = 2
            MAXLEN = 16.0
            MINLEN = 2.0
            MINLENT = 8.0
            OPT = 22
            IF(STUMP.LE.0.0) STUMP = 1.0
            IF(MTOPP .LE. 0.0) MTOPP = 5.6
            IF(MTOPS .LE. 0.0) MTOPS = 4.0
            TRIM = .5
C  MIN SAWTIMBER LENGTH
            MERCHL = 8
c min dbh tree for sawtimber
            MINBFD = 1.0
         ELSE
            COR='Y'
            EVOD = 2
            MAXLEN = 20.0
            MINLEN = 10.0
            MINLENT = 2.0
            OPT = 12
            IF(STUMP.LE.0.0) STUMP = 1.0
            IF(MTOPP .LE. 0.0) MTOPP = 5.6
            IF(MTOPS .LE. 0.0) MTOPS = 4.0
            TRIM = .5
C  MIN SAWTIMBER LENGTH
            MERCHL = 10
c min dbh tree for sawtimber
            MINBFD = 1.0
         ENDIF
      ELSEIF(REGN.EQ.2) THEN
        
        COR='Y'
        EVOD = 2
        MAXLEN = 16.0
        MINLEN = 2.0
        minlent = 2.0
        OPT = 22
        IF(STUMP.LE.0.0) STUMP = 1.0
        IF(MTOPP .LE. 0.0) MTOPP = 6.0
        IF(MTOPS .LE. 0.0) MTOPS = 4.0
        TRIM = 0.5
C  MIN SAWTIMBER LENGTH
        MERCHL = 8
c min dbh tree for sawtimber
c        MINBFD = 7.0
        MINBFD = 1.0

      ELSEIF(REGN.EQ.3) THEN
        
        COR='Y'
        EVOD = 2
        MAXLEN = 16.0
        MINLEN = 2.0
        minlent = 2.0
C Karen requested to change back to 2' for minimum log length (02/13/2015)
c        IF(PROD.EQ.'01')THEN
c          MINLEN = 8.0
c        ELSE
c          MINLEN = 10.0
c        ENDIF
c        minlent = 10.0
        OPT = 22 
        IF(STUMP.LE.0.0) STUMP = 1.0
        IF(MTOPP .LE. 0.0) MTOPP = 6.0
        IF(MTOPS .LE. 0.0) MTOPS = 4.0
        TRIM = 0.5
C  MIN SAWTIMBER LENGTH
        MERCHL = 8
c min dbh tree for sawtimber
c        MINBFD = 7.0
        MINBFD = 1.0

      ELSEIF(REGN.EQ.4) THEN

         COR='Y'
         EVOD = 2
         MAXLEN = 16.0
         MINLEN = 2.0
         minlent = 2.0
         OPT = 22
         IF(STUMP.LE.0.0) STUMP = 1.0
         IF(MTOPP .LE. 0.0) MTOPP = 6.0
         IF(MTOPS .LE. 0.0) MTOPS = 4.0
         TRIM = .5
C  MIN SAWTIMBER LENGTH
         MERCHL = 8
c min dbh tree for sawtimber
c         MINBFD = 7.0
        MINBFD = 1.0

      ELSEIF(REGN.EQ.5) THEN

         COR='Y'
         EVOD = 2
         MAXLEN = 16.0
         MINLEN = 2.0
         minlent = 2.0
         
         OPT = 22
         IF(STUMP.LE.0.0) STUMP = 1.0
         IF(MTOPP .LE. 0.0) MTOPP = 6.0
         IF(MTOPS .LE. 0.0) MTOPS = 4.0
         TRIM = .5
c         TRIM = 0
C  MIN SAWTIMBER LENGTH
         MERCHL = 8
c         MERCHL = 4
c min dbh tree for sawtimber
c         MINBFD = 7.0
         MINBFD = 1.0
         
C Added Region 11 for DOD, using same as R6 (03/26/2014)                     
      ELSEIF(REGN.EQ.6.OR.REGN.EQ.11) THEN
           COR='N'
           EVOD = 2
           MAXLEN = 16.0
           MINLEN = 2.0
           minlent = 2.0
           OPT = 23
           IF(STUMP.LE.0.0) STUMP = 0.0
           IF(MTOPP .LE. 0.0) MTOPP = 2.0
           IF(MTOPS .LE. 0.0) MTOPS = 2.0
           TRIM = .5
C  MIN SAWTIMBER LENGTH
           MERCHL = 8
c min dbh tree for sawtimber
c           MINBFD = 6.0
           MINBFD = 1.0

      ELSEIF(REGN.EQ.7) THEN
         COR='N'
         EVOD = 2
         MAXLEN = 16.0
         MINLEN = 2.0
         minlent = 2.0
         OPT = 23
         IF(STUMP.LE.0.0) STUMP = 1.0
         IF(MTOPP .LE. 0.0) MTOPP = anint((0.184*DBHOB)+2.24)
         IF(MTOPS .LE. 0.0) MTOPS = 2.0
         TRIM = .5
C  MIN SAWTIMBER LENGTH
         MERCHL = 8
c min dbh tree for sawtimber
c         MINBFD = 6.0
         MINBFD = 1.0
      ELSEIF(REGN.EQ.8.AND.MDL.EQ.'CLK') THEN
         COR='Y'
         EVOD = 2
         MAXLEN = 8.0
         MINLEN = 8.0 
c         MINLEN = 4.0
         MERCHL = 8.0
         OPT = 22
         read(volEq(8:10),'(i3)') spp
         IF(spp.LT.300) THEN
           MTOPP = 7.0
         ELSE
           MTOPP = 9.0
         ENDIF
         MTOPS = 4.0
         TRIM = 0.5
         IF(PROD.EQ.'01') THEN
           STUMP = 1.0
         ELSE
           STUMP = 0.5
         ENDIF

      ELSEIF(REGN.EQ.9.AND.MDL.EQ.'CLK') THEN
         COR='Y'
         EVOD = 2
         MAXLEN = 8.0
         MINLEN = 4.0
         MERCHL = 8.0
         OPT = 22
         read(volEq(8:10),'(i3)') spp
         IF(spp.LT.300) THEN
           MTOPP = 7.6
         ELSE
           MTOPP = 9.6
         ENDIF
         MTOPS = 4.0
         TRIM = 0.3
         IF(PROD.EQ.'01') THEN
           STUMP = 1.0
         ELSE
           STUMP = 0.5
         ENDIF
      ELSEIF(REGN.EQ.10) THEN

         COR='Y'
         EVOD = 2
         MAXLEN = 16.0
         MINLEN = 8.0
         MINLENT = 8.0
         OPT = 23
         IF(STUMP.LE.0.0) STUMP = 1.0
         IF(MTOPP .LE. 0.0) MTOPP = 6.0
         IF(MTOPS .LE. 0.0) MTOPS = 4.0
         TRIM = .5
C  MIN SAWTIMBER LENGTH
         MERCHL = 8
c min dbh tree for sawtimber
c         MINBFD = 6.0
        MINBFD = 1.0
      
c default merch rules
      ELSE
         COR='Y'
         EVOD = 2
         MAXLEN = 16.0
         MINLEN = 2.0
         MINLENT = 2.0
         OPT = 22
         IF(STUMP.LE.0.0) STUMP = 1.0
         IF(MTOPP .LE. 0.0) MTOPP = 6.0
         IF(MTOPS .LE. 0.0) MTOPS = 4.0
         TRIM = .5
C  MIN SAWTIMBER LENGTH
         MERCHL = 8
c min dbh tree for sawtimber
         MINBFD = 1.0
      ENDIF
      
      RETURN
      END                      
c--  MERCHANDIZING VARIABLES
C***************************
c--  REGION - INTEGER - Region number used to set Regional Merchandizing Rules
C--  COR - CHARACTER - Flag to indicate Scribner table or Scribner
C--                 factor volumes. "Y" = table volumes, "N" = factor volumes
C--  EVOD - INTEGER - allow even or odd segment lengths
C--         segment options 11-14 allow odd lengths by definition
C--        1 = odd segment lengths allowed
C--        2 = only even segment lengths will be allowed
C--  MAXLEN - REAL - Maximum segment length
C--  MINLEN - REAL - Minimum segment length
C--  MERCHL - REAL - Minimum length of primary product a tree must have
C--                  must have be merchantable
C--  **TOP DIB TO USE**
C--  MTOPP - REAL - BdFt, CuFt and Cord Wood merch top for primary product
C--  MTOPS - REAL - CuFt and Cord Wood merch top for secondary product
C
C--  OPT - INTEGER - Specifies which segmentation option to use for
C--        merchandizing tree bole.  Option codes are as follows:
C--        11 = 16 ft log scale, presented as tree length log. (FSH 2409.11)
C--        12 = 20 ft log scale, presented as tree length log. (FSH 2409.11)
C--        13 = 32 ft log scale, presented as tree length log.
C--        14 = 40 ft log scale, presented as tree length log.
C--        21 = Nominal log length (NLL), if top log is less then half
C--             of the NLL then it is combined with the next lowest log and
C--             this combined piece is then resegmented according to the
C--             entered merchandising parameters giving two approximately
C--             equal log lengths.  If the segment length is greater then
C--             or equal to half the NNL then the segment stands on its' own.
C--        22 = Nominal log length (NLL), top log is combined with the next
C--             lowest log and this combined piece is then resegmented
C--             according to the entered merchandising parameters giving
C--             two approximately equal log lengths.
C--        23 = Nominal log length, top segment stands on its' own.
C--        24 = Nominal log length, if the top segment is less then 1/4 of
C--             NNL then the segment is droped,  it the segment is 1/4 to
C--             3/4 of NNL then the segment length is set to 1/2 of NNL,
C--             if the segment is greater then 3/4 of NNL then the segment
C--             length is set to NNL.
C
C--  STUMP - REAL - height of stump in feet or fractions thereof.
C--  TRIM - REAL - trim length for each segment in feet or fractions thereof.
