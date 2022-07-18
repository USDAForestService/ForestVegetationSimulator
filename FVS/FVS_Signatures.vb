Option Strict On
Option Explicit On
Imports System.Runtime.InteropServices

Public Module FVS_Signatures
   ' Note that numeric arrays of any type are passed ByVal
   ' Note that strings are passed ByVal with 'Ansi' in the subroutine declaration
   Public Declare Sub FVSDIMSIZES Lib "FVS_bcc.dll" (ByRef nTrees As Integer,
                                                          ByRef nCycles As Integer,
                                                          ByRef nPlots As Integer,
                                                          ByRef MaxTrees As Integer,
                                                          ByRef MaxSpecies As Integer,
                                                          ByRef MaxPlots As Integer,
                                                          ByRef MaxCycles As Integer)

   Public Declare Sub FVSSVSDIMSIZES Lib "FVS_bcc.dll" (ByRef nSVSObjs As Integer,
                                                          ByRef nDeadObjs As Integer,
                                                          ByRef nCWDobjs As Integer,
                                                          ByRef MaxSVSObjs As Integer,
                                                          ByRef MaxDeadObjs As Integer,
                                                          ByRef MaxCWDObjs As Integer)

   Public Declare Sub FVSSUMMARY Lib "FVS_bcc.dll" (ByVal Sumry As Integer(),
                                                         ByRef iCycle As Integer,
                                                         ByRef nCycle As Integer,
                                                         ByRef MaxRow As Integer,
                                                         ByRef MaxCol As Integer,
                                                         ByRef RtnCode As Integer)

   Public Declare Ansi Sub FVSTREEATTR Lib "FVS_bcc.dll" (ByVal Name As String,
                                                          ByRef NameLen As Integer,
                                                          ByVal Action As String,
                                                          ByRef nTrees As Integer,
                                                          ByVal Attr As Double(),
                                                          ByRef RtnCode As Integer)

   Public Declare Ansi Sub FVSFFEATTRS Lib "FVS_bcc.dll" (ByVal Name As String,
                                                          ByRef NameLen As Integer,
                                                          ByVal Action As String,
                                                          ByRef nSpecies As Integer,
                                                          ByVal Attr As Double(),
                                                          ByRef RtnCode As Integer)

   Public Declare Sub FVSADDTREES Lib "FVS_bcc.dll" (ByVal inDBH As Double(),
                                                          ByVal inSpecies As Double(),
                                                          ByVal inHeight As Double(),
                                                          ByVal inCrownRatio As Double(),
                                                          ByVal inPlot As Double(),
                                                          ByVal inTPA As Double(),
                                                          ByRef nTrees As Integer,
                                                          ByRef RtnCode As Integer)

   Public Declare Ansi Sub FVSSETCMDLINE Lib "FVS_bcc.dll" (ByVal cmdLine As String,
                                                         ByRef cmdLineLen As Integer,
                                                         ByRef RtnCode As Integer)

   Public Declare Sub FVS Lib "FVS_bcc.dll" (ByRef RtnCode As Integer)

   Public Declare Ansi Sub FVSCLOSEFILE Lib "FVS_bcc.dll" (ByVal FileName As String,
                                                         ByRef FileNameLen As Integer)

   Public Declare Ansi Sub FVSSTANDID Lib "FVS_bcc.dll" (ByVal StandID As String,
                                                      ByVal CNID As String,
                                                      ByVal mID As String,
                                                      ByRef StandIDLen As Integer,
                                                      ByRef CNIDLen As Integer,
                                                      ByRef mIDLen As Integer)

   Public Declare Ansi Sub FVSSPECIESCODE Lib "FVS_bcc.dll" (ByVal FVSCode As String,
                                                             ByVal FiaCode As String,
                                                             ByVal PlantCode As String,
                                                             ByRef Index As Integer,
                                                             ByRef LenFVSCode As Integer,
                                                             ByRef LenFIACode As Integer,
                                                             ByRef LenPlantCode As Integer,
                                                             ByRef RtnCode As Integer)

   Public Declare Ansi Sub FVSGETKEYWORDFILENAME Lib "FVS_bcc.dll" (ByVal Filename As String,
                                                      ByRef FilenameMaxLen As Integer,
                                                      ByRef FilenameActualLen As Integer)

   Public Declare Sub FVSGETICCODE Lib "FVS_bcc.dll" (ByRef ICCode As Integer)

   Public Declare Sub FVSGETRESTARTCODE Lib "FVS_bcc.dll" (ByRef RestartCode As Integer)

   Public Declare Sub FVSGETRTNCODE Lib "FVS_bcc.dll" (ByRef RtnCode As Integer)

   Public Declare Sub FVSSETRTNCODE Lib "FVS_bcc.dll" (ByRef RtnCode As Integer)

   Public Declare Sub FVSGETSTOPPOINTCODES Lib "FVS_bcc.dll" (ByRef StopPtCode As Integer,
                                                              ByRef StopPtYear As Integer)
   Public Declare Sub FVSSETSTOPPOINTCODES Lib "FVS_bcc.dll" (ByRef StopPtCode As Integer,
                                                              ByRef StopPtYear As Integer)
   Public Declare Ansi Sub FVSEVMONATTR Lib "FVS_bcc.dll" (ByVal AttrName As String,
                                                           ByRef AttrNameLen As Integer,
                                                           ByVal Action As String,
                                                           ByRef Attr As Double,
                                                           ByRef RtnCode As Integer)
End Module
