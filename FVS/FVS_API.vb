Option Strict On
Option Explicit On
Imports System.Runtime.InteropServices

Public Class FVS_API

#Region "Declarations"
   Private MeasurementType As String ' "metric" and "imperial"
#End Region

#Region "Constructor"
   Public Sub New()
      MeasurementType = "imperial" ' default
   End Sub
#End Region

#Region "Constants"
   ' taken from open-fvs/trunk/common/METRIC.F77; other conversion factors
   ' are present in that file which are not yet used here
   Protected Const ACRtoHA As Single = 0.4046945
   Protected Const FT2pACRtoM2pHA As Single = 0.2295643
   Protected Const FT3pACRtoM3pHA As Single = 0.0699713
   Protected Const FTtoM As Single = 0.3048
   Protected Const INtoCM As Single = 2.54001
   Protected Const FT3toM3 As Single = 0.028317
   Protected Const LBtoKG As Single = 0.4535924
   ' not yet used
   Protected Const FT2toM2 As Single = 0.0929034
#End Region

#Region "Properties"

   Public Property MeasurementUnits() As String
      Get
         Return MeasurementType
      End Get
      Set(value As String)
         If (value = "metric") Then
            MeasurementType = value
         ElseIf (value = "imperial") Then
            MeasurementType = value
         End If
      End Set
   End Property

#End Region

#Region "Methods"
   Public Sub DimSizes(ByRef nTrees As Integer,
                            ByRef nCycles As Integer,
                            ByRef nPlots As Integer,
                            ByRef MaxTrees As Integer,
                            ByRef MaxSpecies As Integer,
                            ByRef MaxPlots As Integer,
                            ByRef MaxCycles As Integer)
      FVSDIMSIZES(nTrees, nCycles, nPlots, MaxTrees, MaxSpecies, MaxPlots, MaxCycles)
      Return
   End Sub
   Public Sub SVSDimSizes(ByRef nSVSObjs As Integer,
                            ByRef nDeadObjs As Integer,
                            ByRef nCWDobjs As Integer,
                            ByRef MaxSVSObjs As Integer,
                            ByRef MaxDeadObjs As Integer,
                            ByRef MaxCWDObjs As Integer)
      FVSSVSDIMSIZES(nSVSObjs, nDeadObjs, nCWDobjs, MaxSVSObjs, MaxDeadObjs, MaxCWDObjs)
      Return
   End Sub
   Public Sub Summary(ByVal S As Integer(),
                           ByRef iCycle As Integer,
                           ByRef nCycle As Integer,
                           ByRef MaxRow As Integer,
                           ByRef MaxCol As Integer,
                           ByRef RtnCode As Integer)
      FVSSUMMARY(S, iCycle, nCycle, MaxRow, MaxCol, RtnCode)

      ' Out of bounds, return
      If (RtnCode <> 0) Then Return

      ' Return imperial or metric units; based on /open-fvs/metric/base/sumout.f
      ' NOTE THAT the VB indexes are zero-based: S(0) is Year ... S(19)
      ' Division operations mean that the HA unit are "per ha".
      ' Conversion is done AFTER rounding to INT on the DLL-side; expect rounding errors.

      If (Me.MeasurementUnits = "metric") Then
         S(2) = CInt(S(2) / ACRtoHA)
         S(3) = CInt(S(3) * FT3pACRtoM3pHA)
         S(4) = CInt(S(4) * FT3pACRtoM3pHA)
         S(6) = CInt(S(6) / ACRtoHA)
         S(7) = CInt(S(7) * FT3pACRtoM3pHA)
         S(8) = CInt(S(8) * FT3pACRtoM3pHA)
         S(10) = CInt(S(10) * FT2pACRtoM2pHA)
         S(12) = CInt(S(12) * FTtoM)
         S(14) = CInt(S(14) * FT3pACRtoM3pHA)
         S(15) = CInt(S(15) * FT3pACRtoM3pHA)
      End If
      Return
   End Sub
   Public Sub SetCmdLine(ByRef cmdLine As String,
                         ByRef RtnCode As Integer)
      Dim cmdLineLen As Integer = cmdLine.Length
      FVSSETCMDLINE(cmdLine, cmdLineLen, RtnCode)
      Return
   End Sub
   Public Sub RunFVS(ByRef RtnCode As Integer)
      FVS(RtnCode)
      Return
   End Sub
   Public Sub GetRtnCode(ByRef RtnCode As Integer)
      FVSGETRTNCODE(RtnCode)
      Return
   End Sub
   Public Sub CloseFile(ByRef FileName As String)
      Dim FileNameLen As Integer = FileName.Length
      FVSCLOSEFILE(FileName, FileNameLen)
      Return
   End Sub
   Public Sub StandID(ByRef StandID As String,
                      ByRef CNID As String,
                      ByRef mID As String)

      ' maximum length of the 3 labels (26,40,4) is determined on DLL side at compile time
      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#Stand_Identification

      StandID = StrDup(26, " "c)
      CNID = StrDup(40, " "c)
      mID = StrDup(4, " "c)
      Dim StandIDLen = StandID.Length
      Dim CNIDLen As Integer = CNID.Length
      Dim mIDLen As Integer = mID.Length
      FVSSTANDID(StandID, CNID, mID, StandIDLen, CNIDLen, mIDLen)
      StandID = Trim(StandID)
      CNID = Trim(CNID)
      mID = Trim(mID)
      Return
   End Sub
   Public Sub GetKeywordFilename(ByRef Filename As String)

      ' maximum length of the filename is determined on DLL side at compile time
      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#Current_Keyword_File_Name

      Filename = StrDup(256, " "c)
      Dim FilenameLen = Filename.Length
      Dim FilenameLenUsed As Integer
      FVSGETKEYWORDFILENAME(Filename, FilenameLen, FilenameLenUsed)
      Filename = Trim(Filename)
      Return
   End Sub
   Public Sub GetICCode(ByRef ICCode As Integer)
      FVSGETICCODE(ICCode)
      Return
   End Sub
   Public Sub SetRtnCode(ByRef RtnCode As Integer)
      FVSSETRTNCODE(RtnCode)
      Return
   End Sub
   Public Sub GetRestartCode(ByRef RestartCode As Integer)
      FVSGETRESTARTCODE(RestartCode)
      Return
   End Sub
   Public Sub GetStopPointCodes(ByRef StopPointCode As Integer,
                                ByRef StopPointYear As Integer)
      FVSGETSTOPPOINTCODES(StopPointCode, StopPointYear)
      Return
   End Sub
   Public Sub SetStopPointCodes(ByRef StopPointCode As Integer,
                                ByRef StopPointYear As Integer)
      FVSSETSTOPPOINTCODES(StopPointCode, StopPointYear)
      Return
   End Sub
   Public Sub SpeciesCode(ByRef FVSCode As String,
                          ByRef FiaCode As String,
                          ByRef PlantCode As String,
                          ByRef Index As Integer,
                          ByRef RtnCode As Integer)

      ' maximum length of the 3 labels (4,4,6) is determined on DLL side at compile time
      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#Species_Codes

      FVSCode = StrDup(4, " "c)
      FiaCode = StrDup(4, " "c)
      PlantCode = StrDup(6, " "c)
      Dim FVSCodeLen As Integer = FVSCode.Length
      Dim FIACodeLen As Integer = FiaCode.Length
      Dim PlantCodeLen As Integer = PlantCode.Length
      FVSSPECIESCODE(FVSCode, FiaCode, PlantCode, Index, FVSCodeLen, FIACodeLen, PlantCodeLen, RtnCode)
      FVSCode = Trim(FVSCode)
      FiaCode = Trim(FiaCode)
      PlantCode = Trim(PlantCode)
      Return
   End Sub
   Public Sub TreeAttr(ByRef Name As String,
                     ByRef Action As String,
                     ByRef nTrees As Integer,
                     ByVal Attr() As Double,
                     ByRef RtnCode As Integer)

      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#FVS_Tree_Attributes
      ' string matches are case sensitive and exact

      Dim NameLen As Integer = Name.Length
      Dim i As Integer
      Dim x As Double

      Dim StrActionArray() As String = {"get", "set"}

      Dim StrFound As Boolean = False
      Dim StrTarget As String = Action
      For Each Str As String In StrActionArray
         If Str = StrTarget Then
            StrFound = True
            Exit For
         End If
      Next
      If (StrFound = False) Then
         Attr = Nothing
         RtnCode = 1
         Return
      End If

      Dim StrAttrArray() As String = {
         "tpa", "mort", "dbh", "dg", "ht",
         "htg", "crwdth", "cratio", "species", "age",
         "plot", "tcuft", "mcuft", "bdft", "mgmtcd",
         "plotsize", "crownwt0", "crownwt1", "crownwt2", "crownwt3",
         "crownwt4", "crownwt5", "id"}

      ' Division operations mean that the HA unit are "per ha".
      ' NB: 1D = 1.0 double precision
      Dim MetricAttrArray() As Double = {
         1D / ACRtoHA, 1D / ACRtoHA, INtoCM, INtoCM, FTtoM,
         FTtoM, FTtoM, 1D, 1D, 1D,
         1D, FT3toM3, FT3toM3, 1D, 1D,
         ACRtoHA, LBtoKG, LBtoKG, LBtoKG, LBtoKG,
         LBtoKG, LBtoKG, 1D}

      Dim StrIndx As Integer = 0
      StrFound = False
      StrTarget = Name
      For Each Str As String In StrAttrArray
         If Str = StrTarget Then
            StrFound = True
            Exit For
         End If
         StrIndx += 1
      Next
      If (StrFound = False) Then
         Attr = Nothing
         RtnCode = 1
         Return
      End If

      ' To convert input from metric to the imperial required by FVS, multiply
      ' by the inverse (1/X) of the constants if MetricAttrArray().

      If (Action = "set" And Me.MeasurementUnits = "metric") Then
         x = 1D / MetricAttrArray(StrIndx)
         For i = 0 To Attr.Length - 1
            Attr(i) = Attr(i) * x
         Next i
      End If

      FVSTREEATTR(Name, NameLen, Action, nTrees, Attr, RtnCode)

      ' Return imperial or metric units; based on relevant units
      ' NOTE THAT the VB indexes are zero-based: Attr(0) is the first tree in the treelist

      If (Action = "get" And Me.MeasurementUnits = "metric") Then
         x = MetricAttrArray(StrIndx)
         For i = 0 To Attr.Length - 1
            Attr(i) = Attr(i) * x
         Next i
      End If
      Return
   End Sub
   Public Sub FFEAttrs(ByRef Name As String,
                     ByRef Action As String,
                     ByRef nSpecies As Integer,
                     ByVal Attr() As Double,
                     ByRef RtnCode As Integer)

      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#Fire_and_Fuels_Extension_Attributes
      ' string matches are case sensitive and exact

      Dim NameLen As Integer = Name.Length
      Dim i As Integer
      Dim x As Double

      Dim StrActionArray() As String = {"get", "set"}

      Dim StrFound As Boolean = False
      Dim StrTarget As String = Action
      For Each Str As String In StrActionArray
         If Str = StrTarget Then
            StrFound = True
            Exit For
         End If
      Next
      If (StrFound = False) Then
         Attr = Nothing
         RtnCode = 1
         Return
      End If

      Dim StrAttrArray() As String = {
         "fallyrs0", "falllrs1", "fallyrs2", "fallyrs3", "fallyrs4",
         "fallyrs5"}

      ' Division operations mean that the HA unit are "per ha".
      ' NB: 1D = 1.0 double precision
      Dim MetricAttrArray() As Double = {
         1D, 1D, 1D, 1D, 1D,
         1D}

      Dim StrIndx As Integer = 0
      StrFound = False
      StrTarget = Name
      For Each Str As String In StrAttrArray
         If Str = StrTarget Then
            StrFound = True
            Exit For
         End If
         StrIndx += 1
      Next
      If (StrFound = False) Then
         Attr = Nothing
         RtnCode = 1
         Return
      End If

      ' To convert input from metric to the imperial required by FVS, multiply
      ' by the inverse (1/X) of the constants if MetricAttrArray().

      If (Action = "set" And Me.MeasurementUnits = "metric") Then
         x = 1D / MetricAttrArray(StrIndx)
         For i = 0 To Attr.Length - 1
            Attr(i) = Attr(i) * x
         Next i
      End If

      FVSFFEATTRS(Name, NameLen, Action, nSpecies, Attr, RtnCode)

      ' Return imperial or metric units; based on relevant units
      ' NOTE THAT the VB indexes are zero-based: Attr(0) is the first tree in the treelist

      If (Action = "get" And Me.MeasurementUnits = "metric") Then
         x = MetricAttrArray(StrIndx)
         For i = 0 To Attr.Length - 1
            Attr(i) = Attr(i) * x
         Next i
      End If
      Return
   End Sub
   Public Sub AddTrees(ByVal DBH() As Double,
                       ByVal Species() As Double,
                       ByVal Height() As Double,
                       ByVal CrownRatio() As Double,
                       ByVal Plot() As Double,
                       ByVal TPA() As Double,
                       ByRef nTrees As Integer,
                       ByRef RtnCode As Integer)


      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#Add_Trees

      If (DBH.Length <> nTrees Or Species.Length <> nTrees Or
         Height.Length <> nTrees Or CrownRatio.Length <> nTrees Or
         Plot.Length <> nTrees) Then
         RtnCode = 1
         Return
      End If

      ' To convert input from metric to the imperial required by FVS, multiply
      ' by the inverse (1/X) of the constants if MetricAttrArray().

      If (Me.MeasurementUnits = "metric") Then
         For i = 0 To nTrees - 1
            DBH(i) = DBH(i) * 1D / INtoCM
            Height(i) = Height(i) * 1D / FTtoM
            TPA(i) = TPA(i) * ACRtoHA
         Next i
      End If

      FVSADDTREES(DBH, Species, Height, CrownRatio, Plot, TPA, nTrees, RtnCode)

      Return
   End Sub
   Public Sub EvmonAttr(ByRef AttrName As String,
                       ByRef Action As String,
                       ByRef Attr As Double,
                       ByRef RtnCode As Integer)

      ' Public Declare Ansi Sub FVSEVMONATTR Lib "FVS_bcc.dll" (ByVal AttrName As String,
      '                                                    ByRef AttrNameLen As Integer,
      '                                                   ByVal Action As String,
      '                                                  ByRef Attr As Double,
      '                                                 ByRef RtnCode As Integer)
      '   Public Sub TreeAttr(ByRef Name As String,
      '                ByRef Action As String,
      '                ByRef nTrees As Integer,
      '                ByVal Attr() As Double,
      '                ByRef RtnCode As Integer)

      ' see: http://code.google.com/p/open-fvs/wiki/FVS_API#FVS_Event_Monitor_Variables
      ' string matches are case sensitive and exact

      Dim AttrNameLen As Integer = AttrName.Length
      Dim x As Double

      Dim StrActionArray() As String = {"get", "set"}

      Dim StrFound As Boolean = False
      Dim StrTarget As String = Action
      For Each Str As String In StrActionArray
         If Str = StrTarget Then
            StrFound = True
            Exit For
         End If
      Next
      If (StrFound = False) Then
         Attr = Nothing
         RtnCode = 1
         Return
      End If

      ' After- and Before-thin canopy cover %; more to come...

      Dim StrAttrArray() As String = {
         "acancov", "bcancov"}

      ' metric conversions when necessary.
      ' NB: 1D = 1.0 double precision
      Dim MetricAttrArray() As Double = {
         1D, 1D}

      Dim StrIndx As Integer = 0
      StrFound = False
      StrTarget = AttrName
      For Each Str As String In StrAttrArray
         If Str = StrTarget Then
            StrFound = True
            Exit For
         End If
         StrIndx += 1
      Next
      If (StrFound = False) Then
         Attr = Nothing
         RtnCode = 1
         Return
      End If

      ' To convert input from metric to the imperial required by FVS, multiply
      ' by the inverse (1/X) of the constants if MetricAttrArray().

      If (Action = "set" And Me.MeasurementUnits = "metric") Then
         x = 1D / MetricAttrArray(StrIndx)
         Attr = Attr * x
      End If

      FVSEVMONATTR(AttrName, AttrNameLen, Action, Attr, RtnCode)

      ' Return imperial or metric units; based on relevant units
      If (Action = "get" And Me.MeasurementUnits = "metric") Then
         x = MetricAttrArray(StrIndx)
         Attr = Attr * x
      End If
      RtnCode = 999
      Return
   End Sub
#End Region

End Class
