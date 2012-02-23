' usage:   cscript.exe ExportAccessTablesToCSV.vbs <table name>*
' example: cscript.exe ExportAccessTablesToCSV.vbs FVS_StandInit FVS_PlotInit FVS_TreeInit

' assumption: the Access database file is in the script's working directory, and the name
'              of the file is the same as the value of the databaseFilename constant
' output: for each table T named as an argument to the script, a file in the script's working
'         directory named T.csv, which contains the contents of T in comma-separated values format


Const databaseFilename = "FVS_Data.mdb"

Const acExportDelim = 2


currentDirectory = GetCurrentDirectory()

Set accessApplication = CreateObject("Access.Application")
accessApplication.OpenCurrentDatabase MakePath(currentDirectory, databaseFilename)

For Each tableName In WScript.Arguments
    ExportTableData accessApplication, currentDirectory, tableName
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

Sub ExportTableData(accessApplication, currentDirectory, tableName)
    accessApplication.DoCmd.TransferText acExportDelim, , tableName, MakePath(currentDirectory, tableName & ".csv")
End Sub
