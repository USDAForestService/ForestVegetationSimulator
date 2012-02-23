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
