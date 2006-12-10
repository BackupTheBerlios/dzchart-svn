Option Explicit

Const ConnectionString = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\MySnips.mdb;"

Function QuotedStr(strText)
  QuotedStr = "'" & Replace(strText, "'", "''") & "'"
End Function

Sub Main()
  Dim SnipName, Snip
  Snip = Selection.Text
  If Len(Trim(Snip)) = 0 Then Exit Sub
  
  SnipName = InputBox("What is the name of the snip?", _
    "Put Snip into DB", Selection.Strings(0))
  If Len(SnipName) = 0 Then Exit Sub
  If Len(SnipName) > 255 Then SnipName = Left(SnipName, 255)
  
  Dim strSQL
  strSQL = "INSERT INTO Snips(SnipName, SnipText) VALUES " & _
    "(" & QuotedStr(SnipName) & ", " & QuotedStr(Snip) & ")"

  Dim cnn
  Set cnn = CreateObject("ADODB.Connection")
  cnn.Open ConnectionString
  cnn.Execute strSQL
  Set cnn = Nothing
End Sub