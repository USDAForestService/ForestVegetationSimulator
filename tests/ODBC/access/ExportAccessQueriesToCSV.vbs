' usage:   cscript.exe ExportAccessQueriesToCSV.vbs [/databaseFile:<source database file path>] [/outputDirectory:<output directory path>] (<SELECT query> <output filename>)*
' example: cscript.exe ExportAccessQueriesToCSV.vbs /outputDirectory:DataDump "SELECT * FROM FVS_Cases" FVS_Cases.csv "SELECT StandID, ROUND(RD, 2) FROM FVS_Compute" FVS_Compute.csv

' arguments:
'   /databaseFile       the path to the Access database file against which queries will be
'                       executed; if this argument is omitted, it defaults to ".\FVS_Data.mdb"
'   /outputDirectory    the path to the directory into which the files containing query results
'                       will be written; if this argument is omitted, it defaults to the script's
'                       working directory
'   (unnamed)           all unnamed arguments are interpreted as either an SQL query string or
'                       the name of a file into which query results will be written; the unnamed
'                       arguments are taken in pairs, where the first member of each pair is the
'                       query and the second is the name of the file into which the results of that
'                       query will be written; the file will be written in the designated output
'                       directory

Const acExportDelim = 2
Const acQuery       = 1


currentDirectory = GetCurrentDirectory()
databasePath     = ToAbsolutePath(GetNamedArgumentOrDefault("databaseFile",    MakePath(currentDirectory, "FVS_Data.mdb")))
outputDirectory  = ToAbsolutePath(GetNamedArgumentOrDefault("outputDirectory", currentDirectory))
CreateDirectoryIfMissing outputDirectory

Set accessApplication = CreateObject("Access.Application")
accessApplication.OpenCurrentDatabase databasePath

For i = 0 To WScript.Arguments.Unnamed.Length - 1 Step 2
    queryText  = WScript.Arguments.Unnamed(i)
    outputFile = MakePath(outputDirectory, WScript.Arguments.Unnamed(i + 1))

    ExportQueryResults accessApplication, queryText, outputFile
Next

accessApplication.Quit
Set accessApplication = Nothing

WScript.Echo "Done."


Function GetNamedArgumentOrDefault(argumentName, defaultValue)
    If WScript.Arguments.Named.Exists(argumentName) Then
        GetNamedArgumentOrDefault = WScript.Arguments.Named(argumentName)
    Else
        GetNamedArgumentOrDefault = defaultValue
    End If
End Function

Function MakePath(directory, file)
    MakePath = directory & "\" & file
End Function

Function GetCurrentDirectory()
    Set shell = CreateObject("WScript.Shell")
    
    GetCurrentDirectory = shell.CurrentDirectory
    Set shell = Nothing
End Function

Function ToAbsolutePath(path)
    Set fso = CreateObject("Scripting.FileSystemObject")
    
    ToAbsolutePath = fso.GetAbsolutePathName(path)
    Set fso = Nothing
End Function

Sub CreateDirectoryIfMissing(path)
    Set fso = CreateObject("Scripting.FileSystemObject")
    
    If Not fso.FolderExists(path) Then
        fso.CreateFolder path
    End If
    Set fso = Nothing
End Sub


Sub ExportQueryResults(accessApplication, queryText, outputFile)
    queryName = CreateTempQuery(accessApplication, queryText)

    On Error Resume Next    ' We want to delete the query object even if the export to CSV fails.
    accessApplication.DoCmd.TransferText acExportDelim, , queryName, outputFile
    
    DeleteQuery accessApplication, queryName
    On Error GoTo 0         ' Return to the default error-handling mode.
End Sub

Function CreateTempQuery(accessApplication, queryText)
    queryName = "ExportAccessQueriesToCSV_TempQuery"
    
    Set query = accessApplication.CurrentDb().CreateQueryDef(queryName, queryText)
    CreateTempQuery = queryName
End Function

Sub DeleteQuery(accessApplication, queryName)
    accessApplication.DoCmd.DeleteObject acQuery, queryName
End Sub
