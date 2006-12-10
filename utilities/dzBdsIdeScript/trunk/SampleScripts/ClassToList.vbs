Option Explicit

' Checks whether a character is visible
Function IsTokenChar(strChar)
  Dim ascPosition
  ascPosition = Asc(strChar)
  If ((ascPosition >= 48) And (ascPosition <= 57)) Or _
     ((ascPosition >= 65) And (ascPosition <= 90)) Or _
     ((ascPosition >= 97) And (ascPosition <= 122)) Then
    IsTokenChar = True
  Else
    IsTokenChar = False
  End If
End Function

Function GetFirstToken(strText)
  Dim i, CharFound, TheChar
  
  CharFound = False
  For i = 1 To Len(strText)
    TheChar = Mid(strText, i, 1)
    If IsTokenChar(TheChar) Then
      If Not CharFound Then
        CharFound = True        
      End If
      GetFirstToken = GetFirstToken & TheChar
    Else
      If CharFound Then Exit Function
    End If
  Next
End Function

Sub Main()
  Const ObjectNameMark = "%OBJECT_NAME%"
  Const FileName = "C:\Program Files\Borland\Delphi5\Bin\Scripts\ClassToList.txt"
  
  Dim strVarName  
  strVarName = GetFirstToken(Selection.Text)
  If Len(strVarName) = 0 Then Exit Sub
  Selection.Clear
  Selection.LoadFromFile FileName
  Selection.Text = Replace(Selection.Text, ObjectNameMark, strVarName)
End Sub