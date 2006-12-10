Option Explicit

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Using the Selection's SaveToFile procedure would
'  be much easier, but I want to show how to
'  use the FileSystemObject object.

Sub Main()
  Dim i, strFileName
  strFileName = InputBox("Where do you want to save these file?", _
    "Save snippet to file", "c:\snip.pas")
  If Len(strFileName) = 0 Then Exit Sub
  
  Dim fso, OutFile
  Set fso = CreateObject("Scripting.FileSystemObject")
  Set OutFile = fso.CreateTextFile(strFileName, True)
  Set fso = Nothing
  
  For i = 0 To Selection.Count - 1
    OutFile.WriteLine Selection.Strings(i)
  Next
  OutFile.Close
  
  Set OutFile = Nothing
End Sub