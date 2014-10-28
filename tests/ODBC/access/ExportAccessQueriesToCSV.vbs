' usage:   cscript.exe ExportAccessQueriesToCSV.vbs (<SELECT query> <output filename>)*
' example: cscript.exe ExportAccessQueriesToCSV.vbs "SELECT * FROM FVS_Cases" FVS_Cases.csv "SELECT StandID, ROUND(RD, 2) FROM FVS_Compute" FVS_Compute.csv

' assumption: the Access database file is in the script's working directory, and the name
'             of the file is the same as the value of the databaseFilename constant defined
'             in this script
' output: for each query passed as an argument to the script, a file in the script's working
'         directory that contains the results of the query in comma-separated values format;
'         the arguments to the script are taken in pairs, where the first element of each pair
'         is the query itself and the second element is the name of the file which the results
'         of the query will be written into


Const databaseFilename = "FVS_Data.mdb"

Const acExportDelim = 2
Const acQuery       = 1


currentDirectory = GetCurrentDirectory()

Set accessApplication = CreateObject("Access.Application")
accessApplication.OpenCurrentDatabase MakePath(currentDirectory, databaseFilename)

For i = 0 To WScript.Arguments.Count() - 1 Step 2
    queryText  = WScript.Arguments(i)
    outputFile = MakePath(currentDirectory, WScript.Arguments(i + 1))

    ExportQueryResults accessApplication, queryText, outputFile
Next

accessApplication.Quit
Set accessApplication = Nothing


Function GetCurrentDirectory()
    Set shell = CreateObject("WScript.Shell")
    GetCurrentDirectory = shell.CurrentDirectory
    Set shell = Nothing
End Function

Function MakePath(directory, file)
    MakePath = directory & "\" & file
End Function


Sub ExportQueryResults(accessApplication, queryText, outputFile)
    queryName = CreateTempQuery(accessApplication, queryText)

    On Error Resume Next    ' We want to delete the query object even if the export to CSV fails.
    accessApplication.DoCmd.TransferText acExportDelim, , queryName, outputFile
    
    DeleteQuery accessApplication, queryName
    On Error GoTo 0
End Sub

Function CreateTempQuery(accessApplication, queryText)
    queryName = "ExportAccessQueriesToCSV_TempQuery"
    
    Set query = accessApplication.CurrentDb().CreateQueryDef(queryName, queryText)
    CreateTempQuery = queryName
End Function

Sub DeleteQuery(accessApplication, queryName)
    accessApplication.DoCmd.DeleteObject acQuery, queryName
End Sub
