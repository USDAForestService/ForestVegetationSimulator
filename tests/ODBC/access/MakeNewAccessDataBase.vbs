' usage:   cscript.exe MakeNewAccessDataBase.vbs [/databaseFile:<Access file name>]

currentDirectory = GetCurrentDirectory()
databaseFilename = GetNamedArgumentOrDefault("databaseFile", "FVS_Output.accdb")

Set accessApplication = CreateObject("Access.Application")
accessApplication.NewCurrentDatabase MakePath(currentDirectory, databaseFilename)

accessApplication.Quit
Set accessApplication = Nothing

WScript.Echo "Done."


Function GetCurrentDirectory()
    Set shell = CreateObject("WScript.Shell")
    
    GetCurrentDirectory = shell.CurrentDirectory
    Set shell = Nothing
End Function

Function MakePath(directory, file)
    MakePath = directory & "\" & file
End Function

Function GetNamedArgumentOrDefault(argumentName, defaultValue)
    If WScript.Arguments.Named.Exists(argumentName) Then
        GetNamedArgumentOrDefault = WScript.Arguments.Named(argumentName)
    Else
        GetNamedArgumentOrDefault = defaultValue
    End If
End Function
