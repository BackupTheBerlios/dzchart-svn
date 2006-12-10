Option Explicit

Sub Main()
  Dim i, str
  For i = 0 To Selection.Count - 1
    str = Selection.Strings(i)
    If Left(str, 2) = "//" Then
      Selection.Strings(i) = Right(str, Len(str) - 2)
    End If
  Next
End Sub