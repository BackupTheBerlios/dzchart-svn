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
  Dim i, strFormName, strFormClass
  strFormName = GetFirstToken(Selection.Text)
  strFormClass = "T" & strFormName
  
  Selection.Clear
  Selection.Add "function Show" & strFormName & ": Boolean;"
  Selection.Add "var"
  Selection.Add "  " & strFormName & ": " & strFormClass & ";"
  Selection.Add "begin"
  Selection.Add "  " & strFormName & " := " & strFormClass & ".Create(nil);"
  Selection.Add "  try"
  Selection.Add "    { TODO: Initialize the form, if required. }"
  Selection.Add "    Result := " & strFormName & ".ShowModal = mrOk;"
  Selection.Add "  finally"
  Selection.Add "    " & strFormName & ".Free;"
  Selection.Add "  end;"  
  Selection.Add "end;"
End Sub