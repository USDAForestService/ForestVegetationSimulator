' usage:   cscript.exe MakeNewAccessDataBase.vbs

Const databaseFilename = "FVS_Output.accdb"

Const acExportDelim = 2

currentDirectory = GetCurrentDirectory()

Set accessApplication = CreateObject("Access.Application")
accessApplication.NewCurrentDatabase MakePath(currentDirectory, databaseFilename)

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

