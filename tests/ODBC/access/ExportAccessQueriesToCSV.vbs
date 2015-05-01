' usage:   cscript.exe ExportAccessQueriesToCSV.vbs [/databaseFile:<source database file path>] [/outputDirectory:<output directory path>] <queries file path>
' example: cscript.exe ExportAccessQueriesToCSV.vbs /databaseFile:FVS_Output.accdb /outputDirectory:DataDump queries.txt

' arguments:
'   /databaseFile       the path to the Access database file against which queries will be
'                       executed; if this argument is omitted, it defaults to ".\FVS_Data.mdb"
'   /outputDirectory    the path to the directory into which the files containing query results
'                       will be written; if this argument is omitted, it defaults to the script's
'                       working directory
'   (unnamed)           a single unnamed argument is expected, which is interpreted as the path to
'                       the "queries" file; the queries file defines the queries that will be used
'                       to query the database; each line in the queries file begins with the name of
'                       the file to which the results of the query will be written, followed by a colon,
'                       followed by the text of the SELECT query itself


Const acExportDelim = 2
Const acQuery       = 1


Set fileSystem = CreateObject("Scripting.FileSystemObject")     ' Referenced in the main script body, as well as in several functions.

currentDirectory = GetCurrentDirectory()
databasePath     = ToAbsolutePath(GetNamedArgumentOrDefault("databaseFile",    MakePath(currentDirectory, "FVS_Data.mdb")))
outputDirectory  = ToAbsolutePath(GetNamedArgumentOrDefault("outputDirectory", currentDirectory))
CreateDirectoryIfMissing outputDirectory

Set accessApplication = CreateObject("Access.Application")
accessApplication.OpenCurrentDatabase databasePath

Set queriesFile = fileSystem.OpenTextFile(WScript.Arguments.Unnamed(0))

Do While Not queriesFile.AtEndOfStream
    line = Trim(queriesFile.ReadLine())
    
    If Len(line) > 0 Then
        fields         = Split(line, ":")
        outputFilename = Trim(fields(0))
        queryText      = Trim(fields(1))
        
        ExportQueryResults accessApplication, queryText, MakePath(outputDirectory, outputFilename)
    End If
Loop
queriesFile.Close
Set queriesFile = Nothing

accessApplication.Quit
Set accessApplication = Nothing
Set fileSystem        = Nothing

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
    ToAbsolutePath = fileSystem.GetAbsolutePathName(path)
End Function

Sub CreateDirectoryIfMissing(path)
    If Not fileSystem.FolderExists(path) Then
        fileSystem.CreateFolder path
    End If
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
