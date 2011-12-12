      SUBROUTINE CWHR(ISP, DBH, FVS_HT, FMICR, FMPROB, CWID, FMITRN,
     >           CWXPTS,CCBP,DBHBP,SZDN,CWHR_MOD,CWHR_WT)
      IMPLICIT NONE
C----------
C  **CWHR   FIRE-WS-DATE OF LAST REVISION: 03/14/05
C----------

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

      INCLUDE 'FMFCOM.F77'
      INCLUDE 'CONTRL.F77'
C
C      Public Function CWHRSizeDensity(rst As DAO.Recordset) As String
C'  rst is a DAO recordset, field names must be trees, Dbh, Ht, CrA
C
C      Dim cc As Single, cc1 As Single, cc2 As Single, cc3 As Single,
C          cc4 As Single, cc5 As Single
C      Dim ht3 As Single, ht4 As Single, ht5 As Single
C      Dim tpa3 As Single, tpa4 As Single, tpa5 As Single
C      Dim basal As Single, dbh As Single, bapct As Single, ltrees
C          As Single, sumdbhsq As Single, qmd As Single
C      Dim sng1 As Single
C      Dim ftpa As Field, fdbh As Field, fht As Field, fcra As Field
C      Dim fba As Field
C      Dim size As String, dens As String
C      Dim ccadj As Single, cc3adj As Single
C      Dim bDone As Boolean, QmdPctile As Single, targetbasal
C          As Single, treebasal As Single, sumbasal As Single
C      Dim ccnetavg As Single

      CHARACTER*3 SZDN
      INTEGER     FMITRN, ISP, FMICR, IDUM
      REAL        DBH, FVS_HT, FMPROB, CCBP, DBHBP, CWXPTS, XSUM, CWID

      DIMENSION   ISP(FMITRN), FMICR(FMITRN), DBH(FMITRN), CWID(FMITRN)
      DIMENSION   FVS_HT(FMITRN), FMPROB(FMITRN)
      DIMENSION   CWXPTS(4,2), CCBP(3), DBHBP(4)

      INTEGER     CWHR_MOD(4)
      REAL        CWHR_WT(4), XV(4), YV(4)

      CHARACTER*1 SZ, DN
      INTEGER I,J,K
      INTEGER IXS(MAXTRE)
      REAL CWIDTH, CAREA, XX
      REAL QMDPCTILE, BASAL, QMD, TARGETBASAL, TREEBASAL
      REAL SUMBASAL, SUMDBHSQ, LTREES, SNG1
      REAL CCNETAVG, CCADJ, PCNETAVG
      REAL HT(0:5), CC(0:5), TPA(0:5)
C
C     VARIABLES FOR CALL TO FMDYN
C
      INTEGER   IPTR(4), ITYP(4)
      REAL      EQWT(4)
C
C     THE INTEGER TAGS ASSOCIATED WITH EACH MODEL
C     CLASS. THEY ARE RETURNED WITH THE WEIGHT
C     10=S; 20=P; 30=M; 40=D
C
      DATA IPTR / 10,20,30,40 /
      DATA ITYP /  0, 0, 0, 0 /
C
C     INITIALIZE WEIGHTS OF DYNAMIC MODELS
C
      DO I = 1, 4
        EQWT(I)     = 0.0
        CWHR_MOD(I) = 0
        CWHR_WT(I)  = 0.0
      ENDDO
C
C      With rst
C        Set ftpa = !trees
C        Set fdbh = !dbh
C        Set fht = !ht
C        Set fcra = !cra ! must be crown radius OR AREA??
C        .MoveFirst
C        Do Until .EOF
C          basal = basal + ftpa * fdbh ^ 2 * 0.005454
C          cc = cc + ftpa * fcra
C          trees = trees + ftpa
C          Select Case fdbh
C            Case Is < 1
C              cc1 = cc1 + ftpa * fcra
C            Case Is < 6
C              cc2 = cc2 + ftpa * fcra
C            Case Is < 11
C              cc3 = cc3 + ftpa * fcra
C              ht3 = ht3 + fht * ftpa
C              tpa3 = tpa3 + ftpa
C            Case Is < 24
C              cc4 = cc4 + ftpa * fcra
C              ht4 = ht4 + fht * ftpa
C              tpa4 = tpa4 + ftpa
C            Case Is >= 24
C              cc5 = cc5 + ftpa * fcra
C              ht5 = ht5 + fht * ftpa
C              tpa5 = tpa5 + ftpa
C          End Select
C          MoveNext
C        Loop
C      End With

      XX = 0.0
      DO I = 0,5
        HT(I)  = 0.0
        CC(I)  = 0.0
        TPA(I) = 0.0
      ENDDO
      BASAL = 0.0
      DO I = 1,FMITRN
        IF (FMPROB(I) .GT. 0.0) THEN

          BASAL = BASAL + FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

          CWIDTH=CWID(I)
          CAREA = 3.1415927*CWIDTH*CWIDTH/4.0

          IF     (DBH(I) .LT. DBHBP(1)) THEN
            K = 1
          ELSEIF (DBH(I) .LT. DBHBP(2)) THEN
            K = 2
          ELSEIF (DBH(I) .LT. DBHBP(3)) THEN
            K = 3
          ELSEIF (DBH(I) .LT. DBHBP(4)) THEN
            K = 4
          ELSE
            K = 5
          ENDIF

          HT(K)  = HT(K) + (FVS_HT(I) * FMPROB(I))
          CC(K)  = CC(K) + (CAREA * FMPROB(I))
          TPA(K) = TPA(K) + FMPROB(I)

        ENDIF
      ENDDO

      DO I = 1,5
        HT(0)  = HT(0)  + HT(I)
        CC(0)  = CC(0)  + CC(I)
        TPA(0) = TPA(0) + TPA(I)
      ENDDO
C'  convert crown cover to percent
C
C      cc = cc / 435.6
C      cc1 = cc1 / 435.6
C      cc2 = cc2 / 435.6
C      cc3 = cc3 / 435.6
C      cc4 = cc4 / 435.6
C      cc5 = cc5 / 435.6
C      ccnetavg = pcNetAvg(cc)

      DO I = 0,5
        CC(I) = CC(I)/435.60
      ENDDO
      CCNETAVG = PCNETAVG(CC(0))

C'  divide accumulated ht * tpa by tpa to get avg hts for size 3, 4, and 5
C
C      If tpa3 > 0 Then ht3 = ht3 / tpa3
C      If tpa4 > 0 Then ht4 = ht4 / tpa4
C      If tpa5 > 0 Then ht5 = ht5 / tpa5

      DO I = 3,5
        IF (TPA(I) .GT. 0.0) HT(I) = HT(I) / TPA(I)
      ENDDO

C'  Check for tree dominated habitat
C'  NOTE: pcNetAvg produces a weighted average bewteen uncorrected
C'  percent cover and percent cover assuming random spacing
C
C      ccadj = pcNetAvg(cc)
C      If ccnetavg < 10 Then
C        If trees >= 150 Then
C          CWHRSizeDensity = "1"
C        Else
C          CWHRSizeDensity = "XX"     '  designates as non-forest
C        End If
C        Exit Function
C      End If

      CCADJ = PCNETAVG(CC(0))
      IF (CCNETAVG .LT. 10) THEN
        IF (TPA(0) .GE. 150.) THEN
          SZDN = "1,-"
        ELSE
          SZDN = "X,-"
        ENDIF
        CWHR_MOD(1) = 10
        CWHR_WT(1)  = 1.0
        RETURN
      ENDIF

C      trees = 0      '  reset to use for qmd calculation
C
C'  Check for multi-story
C
C      If ccnetavg >= 60 And ht5 > 0 And ht4 > 0 And ht3 > 0 Then
C        If cc5 >= 20 And cc5 <= 80 Then
C          If cc3 >= 20 And (ht3 / ht5) <= 0.6667 Then
C            CWHRSizeDensity = "6"
C            Exit Function
C          ElseIf cc4 >= 20 And (ht4 / ht5) <= 0.6667 Then
C            CWHRSizeDensity = "6"
C            Exit Function
C          ElseIf cc3 + cc4 >= 30 And (ht3 * tpa3 + ht4 * tpa4)
C                  / (tpa3 + tpa4) / ht5 <= 0.6667 Then
C            CWHRSizeDensity = "6"
C            Exit Function
C          End If
C        End If
C      End If

      IF (CCNETAVG .GE. CCBP(3) .AND.
     >   HT(5) .GT. 0.0 .AND. HT(4) .GT. 0.0 .AND. HT(3) .GT. 0.0) THEN
        IF (CC(5) .GE. 20.0 .AND. CC(5) .LE. 80.0) THEN
          IF (CC(3) .GE. 20.0 .AND. (HT(3)/HT(5)) .LE. (2./3.)) THEN
            SZDN        = "6,-"
            CWHR_MOD(1) = 40
            CWHR_WT(1)  = 1.0
            RETURN
          ELSEIF (CC(4) .GE. 20.0 .AND.
     &        (HT(4)/HT(5)) .LE. (2./3.)) THEN
            SZDN        = "6,-"
            CWHR_MOD(1) = 40
            CWHR_WT(1)  = 1.0
            RETURN
          ELSEIF ( (CC(3) + CC(4)) .GE. 30.0 .AND.
     &        ( ( (HT(3) * TPA(3) + HT(4) * TPA(4)) /
     &        (TPA(3) + TPA(4)) ) / HT(5)) .LE. (2./3.)) THEN
            SZDN        = "6,-"
            CWHR_MOD(1) = 40
            CWHR_WT(1)  = 1.0
            RETURN
          ENDIF
        ENDIF
      ENDIF

C'  Not multi-story
C
C      If ccadj < 25 Then   '  do the sparse stands
C                                 by predominance of pct cover

      IF (CCADJ .LT. CCBP(1)) THEN

C        If cc3 + cc4 + cc5 < cc1 + cc2 Then
C          If cc2 >= cc1 Then
C            CWHRSizeDensity = "2S"
C          Else
C            CWHRSizeDensity = "1S"
C          End If
C        Else
C          If cc3 > cc4 + cc5 Then
C            CWHRSizeDensity = "3S"
C          ElseIf cc5 >= cc4 + cc3 Then
C            CWHRSizeDensity = "3S"
C          Else
C            CWHRSizeDensity = "4S"
C          End If
C        End If
C        Exit Function

        IF ((CC(3) + CC(4) + CC(5)) .LT.
     &      (CC(1) + CC(2))) THEN
          IF (CC(2) .GE. CC(1)) THEN
            SZDN = "2,S"
          ELSE
            SZDN = "1,S"
          ENDIF
        ELSE
          IF (CC(3) .GT. (CC(4) + CC(5))) THEN
            SZDN = "3,S"
          ELSEIF (CC(5) .GE. (CC(4) + CC(3))) THEN
            SZDN = "5,S"
          ELSE
            SZDN = "4,S"
          ENDIF
        ENDIF
        CWHR_MOD(1) = 10
        CWHR_WT(1)  = 1.0
        RETURN

C      Else

      ELSE

C        If cc4 + cc5 < 10 Then  '  use whole stand qmd for small-tree stands
C          QmdPctile = 100
C        End If
C        targetbasal = basal * QmdPctile
C        sumbasal = 0: ltrees = 0
C        bDone = False

        QMDPCTILE = 0.75
        IF (CC(4) + CC(5) .LT. 10.0) THEN
          QMDPCTILE = 1.00
        ENDIF
        TARGETBASAL = BASAL * QMDPCTILE
        SUMBASAL   = 0.0
        LTREES     = 0.0
        SUMDBHSQ   = 0.0
C        bDone = False
C        With rst
C          .Sort = "dbh DESC"
C         .Requery
C         .MoveFirst
C          Do Until bDone
C            treebasal = ftpa * fdbh ^ 2 * 0.005454
C            If sumbasal + treebasal < targetbasal Then
C              sumbasal = sumbasal + treebasal
C              sumdbhsq = sumdbhsq + ftpa * fdbh ^ 2
C              ltrees = ltrees + ftpa
C            Else
C'  how much of this tree record do I need?
C              sng1 = (targetbasal - sumbasal) / treebasal
C              ltrees = ltrees + ftpa * sng1
C              sumdbhsq = sumdbhsq + fdbh ^ 2 * ftpa * sng1
C              bDone = True
C            End If
C            MoveNext
C            If .EOF Then bDone = True
C         Loop
C        End With
C
        IF(FMITRN .GT. 0) CALL RDPSRT(FMITRN,DBH,IXS,.TRUE.)
        DO J = 1,FMITRN
          I = IXS(J)
          TREEBASAL = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
          IF ((SUMBASAL + TREEBASAL) .LT. TARGETBASAL) THEN
            SUMBASAL = SUMBASAL + TREEBASAL
            SUMDBHSQ = SUMDBHSQ + (FMPROB(I) * DBH(I) * DBH(I))
            LTREES   = LTREES + FMPROB(I)
          ELSE
            SNG1 = (TARGETBASAL - SUMBASAL) / TREEBASAL
            LTREES = LTREES + (FMPROB(I) * SNG1)
            SUMDBHSQ = SUMDBHSQ + (DBH(I) * DBH(I) * FMPROB(I) * SNG1)
            GOTO 100
          ENDIF
        ENDDO
  100   CONTINUE


C        If ltrees > 0 Then
C          qmd = Sqr(sumdbhsq / ltrees)
C        Else
C          qmd = 0
C        End If

        QMD = 0.0
        IF (LTREES .GT. 0.0) QMD = SQRT(SUMDBHSQ / LTREES)

C        Select Case qmd
C          Case Is < 1
C            CWHRSizeDensity = "1"
C            Exit Function
C          Case Is < 6
C            size = "2"
C            ccadj = pcNetAvg(cc)
C          Case Is < 11
C            size = "3"
C            cc = cc2 + cc3 + cc4 + cc5
C            ccadj = pcNetAvg(cc)
C'  don't do percent cover correction for medium and large-tree stands
C          Case Is < 24
C            size = "4"
C            cc = cc3 + cc4 + cc5
C            ccadj = cc
C          Case Else
C            size = "5"
C            cc = cc3 + cc4 + cc5
C            ccadj = cc
C        End Select

        IF (QMD .LT. DBHBP(1)) THEN
          SZDN = "1,-"
          CWHR_MOD(1) = 10
          CWHR_WT(1)  = 1.0
          RETURN
        ELSEIF (QMD .LT. DBHBP(2)) THEN
          SZ = "2"
          CCADJ = PCNETAVG(CC(0))
        ELSEIF (QMD .LT. DBHBP(3)) THEN
          SZ = "3"
          CC(0) = CC(2) + CC(3) + CC(4) + CC(5)
          CCADJ = PCNETAVG(CC(0))
        ELSEIF (QMD .LT. DBHBP(4)) THEN
          SZ = "4"
          CC(0) = CC(3) + CC(4) + CC(5)
          CCADJ = CC(0)
        ELSE
          SZ = "5"
          CC(0) = CC(3) + CC(4) + CC(5)
          CCADJ = CC(0)
        ENDIF

C      End If

      ENDIF

C      Select Case ccadj
C        Case Is < 25
C          dens = "S"
C        Case Is < 40
C          dens = "P"
C          If ccnetavg < 25 Then dens = "S"
C        Case Is < 60
C          dens = "M"
C          If ccnetavg < 40 Then dens = "P"
C        Case Else
C          dens = "D"
C          If ccnetavg < 60 Then dens = "M"
C      End Select

      IF (CCADJ .LT. CCBP(1)) THEN
        DN = "S"
      ELSEIF (CCADJ .LT. CCBP(2)) THEN
        DN = "P"
        IF (CCNETAVG .LT. CCBP(1)) DN = "S"
      ELSEIF (CCADJ .LT. CCBP(3)) THEN
        DN = "M"
        IF (CCNETAVG .LT. CCBP(2)) DN = "P"
      ELSE
        DN = "D"
        IF (CCNETAVG .LT. CCBP(3)) DN = "M"
      ENDIF
C
C     CALL FMDYN TO FIND WEIGHTS FOR THE S,M,P,D MODELS
C     FMDYN PLACES RESULTS IN FWT,FMOD (IN **FMFCOM**) AND IN
C     THE RETURNED VARIABLE FMD. THESE VALUES ARE OVERWRITTEN
C     IN A LATER CALL THE FMDYN, SO TEMPORARILY SETTING THEM
C     HERE IS BENIGN
C
      DO I = 1,4
        EQWT(I) = 1.0
      ENDDO
      CALL FMDYN(CCADJ,CCNETAVG,ITYP,CWXPTS,EQWT,IPTR,4,.TRUE.,IDUM)
      DO I = 1,4
        CWHR_MOD(I) = FMOD(I)
        CWHR_WT(I)  = FWT(I)
      ENDDO
C
C     IN SOME CIRCUMSTANCES 3 MODELS MAY BE CANDIDATES. MODEL
C     40 HAS A LOW SLOPE AND IN THE UP/DN/LF/RT SEARCH CAN BE
C     FOUND 'ABOVE' THE POINT BUT OUTSIDE THE DOMAIN. THE
C     THIRD MODEL TO BE REMOVED IS ALWAYS OF TYPE 40.
C     THE REMAINING 2 MODELS ARE PUT IN POSITIONS 1 AND 2
C
      J = 0
      DO I = 1,4
        IF (CWHR_MOD(I) .GT. 0) J = J + 1
      ENDDO

      IF (J .GT. 2) THEN
        DO I = 1,4
          IF (CWHR_MOD(I) .EQ. 40) THEN
            CWHR_MOD(I) = 0
            CWHR_WT(I)  = 0.0
          ENDIF
          XV(I) = REAL(CWHR_MOD(I))
          YV(I) = CWHR_WT(I)
          CWHR_MOD(I) = 0
          CWHR_WT(I)  = 0.0
        ENDDO
        CALL RDPSRT(4,XV,IXS,.TRUE.)
        DO I = 1,2
          J = IXS(I)
          CWHR_MOD(I) = INT(XV(J))
          CWHR_WT(I) = YV(J)
        ENDDO
        XSUM = 0.0
        DO I = 1,2
          XSUM = XSUM + CWHR_WT(I)
        ENDDO
        DO I = 1,2
          CWHR_WT(I) = CWHR_WT(I) / XSUM
        ENDDO
      ENDIF

C      CWHRSizeDensity = size & dens

      SZDN(1:1) = SZ(1:1)
      SZDN(2:2) = ','
      SZDN(3:3) = DN(1:1)
      RETURN

      END

C      End Function

C
C     LOOKS LIKE THIS DOES A CORRECTED % CANOPY COVER
C
      FUNCTION PCNETAVG(grosspercentcover)
C
      real pcRndPack, avg1

      pcRndPack = (1. - 1. / Exp(grosspercentcover / 100.)) * 100.
      avg1 = pcRndPack / 100. * pcRndPack + (100. - pcRndPack) / 100 *
     &        grosspercentcover
C      If (avg1 > 100.) Then avg1 = 100.
      IF(AVG1 .GT. 100.)AVG1=100.
      pcNetAvg = (pcRndPack + avg1) / 2.
      return
      end
