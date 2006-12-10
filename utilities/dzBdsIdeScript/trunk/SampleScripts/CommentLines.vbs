Option Explicit

Sub Main()
  Dim i
  For i = 0 To Selection.Count - 1
    Selection.Strings(i) = "//" & Selection.Strings(i)
  Next
End Sub