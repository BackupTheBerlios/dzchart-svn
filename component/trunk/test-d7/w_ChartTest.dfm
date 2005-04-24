object f_ChartTest: Tf_ChartTest
  Left = 536
  Top = 145
  AutoScroll = False
  Caption = 'Chart Test'
  ClientHeight = 572
  ClientWidth = 589
  Color = clBtnFace
  Constraints.MinHeight = 50
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object XYChart: TdzXYChart
    Left = 0
    Top = 0
    Width = 589
    Height = 572
    Cursor = crHandPoint
    OnGetPointHint = XYChartGetPointHint
    Align = alClient
    Grid.GridType = gtBoth
    Grid.Color = clBlack
    Grid.LineStyle = psDot
    ChartTitle.Title = 'hello world'
    ChartTitle.TitleFont.Charset = DEFAULT_CHARSET
    ChartTitle.TitleFont.Color = clRed
    ChartTitle.TitleFont.Height = 15
    ChartTitle.TitleFont.Name = 'arial'
    ChartTitle.TitleFont.Pitch = fpVariable
    ChartTitle.TitleFont.Style = [fsBold]
    BottomAxis.Title = 'Zeit'
    BottomAxis.TitleFont.Charset = DEFAULT_CHARSET
    BottomAxis.TitleFont.Color = clWindowText
    BottomAxis.TitleFont.Height = -13
    BottomAxis.TitleFont.Name = 'Arial'
    BottomAxis.TitleFont.Pitch = fpVariable
    BottomAxis.TitleFont.Style = [fsBold]
    BottomAxis.LabelFont.Charset = DEFAULT_CHARSET
    BottomAxis.LabelFont.Color = clWindowText
    BottomAxis.LabelFont.Height = -11
    BottomAxis.LabelFont.Name = 'Arial'
    BottomAxis.LabelFont.Pitch = fpVariable
    BottomAxis.LabelFont.Style = []
    BottomAxis.LabelOrientation = loHorizontal
    BottomAxis.OnFormatLabel = XYChart1FormatXAxisLabel
    BottomAxis.Scale.Automatic = True
    BottomAxis.Scale.Min = -1.000000000000000000
    BottomAxis.Scale.Max = 1.000000000000000000
    BottomAxis.Scale.Divisions = 10.000000000000000000
    BottomAxis.Scale.ScaleType = stLinear
    LeftAxis.Title = 'Wert'
    LeftAxis.TitleFont.Charset = DEFAULT_CHARSET
    LeftAxis.TitleFont.Color = clWindowText
    LeftAxis.TitleFont.Height = -13
    LeftAxis.TitleFont.Name = 'Arial'
    LeftAxis.TitleFont.Pitch = fpVariable
    LeftAxis.TitleFont.Style = [fsBold]
    LeftAxis.LabelFont.Charset = ANSI_CHARSET
    LeftAxis.LabelFont.Color = clWindowText
    LeftAxis.LabelFont.Height = -11
    LeftAxis.LabelFont.Name = 'Arial'
    LeftAxis.LabelFont.Pitch = fpVariable
    LeftAxis.LabelFont.Style = []
    LeftAxis.LabelOrientation = loHorizontal
    LeftAxis.Scale.Automatic = True
    LeftAxis.Scale.Min = -1.000000000000000000
    LeftAxis.Scale.Max = 15.000000000000000000
    LeftAxis.Scale.Divisions = 10.000000000000000000
    LeftAxis.Scale.ScaleType = stLinear
    Legend.LegendFont.Charset = DEFAULT_CHARSET
    Legend.LegendFont.Color = clWindowText
    Legend.LegendFont.Height = -11
    Legend.LegendFont.Name = 'Arial'
    Legend.LegendFont.Pitch = fpVariable
    Legend.LegendFont.Style = []
    Legend.Position = lpBottom
    Legend.Frame.Color = clRed
    Legend.Frame.Width = 1
    Legend.Frame.Rounded = False
    Legend.BackGround = clWhite
    Legend.Title = 'Legend'
    Legend.TitleFont.Charset = DEFAULT_CHARSET
    Legend.TitleFont.Color = clWindowText
    Legend.TitleFont.Height = -11
    Legend.TitleFont.Name = 'Arial'
    Legend.TitleFont.Pitch = fpVariable
    Legend.TitleFont.Style = [fsBold]
    Legend.Columns = 1
    ChartFrame.Color = clBlack
    ChartFrame.Width = 1
    ChartFrame.Rounded = False
  end
  object Button1: TButton
    Left = 112
    Top = 192
    Width = 75
    Height = 25
    Caption = '&Button'
    TabOrder = 0
    OnClick = Button1Click
  end
  object dzLineStyleCombo1: TdzLineStyleCombo
    Left = 8
    Top = 440
    Width = 145
    Height = 21
    LineStyle = psInsideFrame
    ItemHeight = 13
    TabOrder = 1
    Text = 'InsideFrame'
  end
  object dzPointStyleCombo1: TdzPointStyleCombo
    Left = 8
    Top = 464
    Width = 145
    Height = 21
    PointStyle = psDownArrow
    ItemHeight = 13
    TabOrder = 2
    Text = 'DownArrow'
  end
  object pd_squared2: TdzPolynomialDataseries
    ChartType = ctXY
    MinX = -1.000000000000000000
    MaxX = 1.000000000000000000
    PointCount = 11
    LineColor = clMaroon
    LineStyle = psSolid
    ForceLineColor = False
    PointColor = clRed
    PointStyle = psRectangle
    FillColor = clSilver
    Caption = 'y = x'#178' + x + 10'
    Chart = XYChart
    Polynom.Order = 2
    Polynom.Coefficients = (
      10.000000000000000000
      1.000000000000000000
      1.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000)
    Left = 264
    Top = 312
  end
  object pd_Cubic: TdzPolynomialDataseries
    ChartType = ctXY
    MinX = -1.000000000000000000
    MaxX = 1.000000000000000000
    PointCount = 11
    LineColor = clTeal
    LineStyle = psClear
    ForceLineColor = True
    PointColor = clTeal
    PointStyle = psUpArrow
    FillColor = clTeal
    Caption = 'y = 8x^7 + 7x^6 + 6x^5 + 5x^4 + 4x'#179' + 3x'#178' + 2x + 1'
    Chart = XYChart
    Polynom.Order = 3
    Polynom.Coefficients = (
      0.000000000000000000
      0.000000000000000000
      7.000000000000000000
      8.000000000000000000
      5.000000000000000000
      6.000000000000000000
      7.000000000000000000
      8.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000)
    Left = 120
    Top = 88
  end
  object pd_Squared1: TdzPolynomialDataseries
    ChartType = ctXY
    MinX = -1.000000000000000000
    MaxX = 1.000000000000000000
    PointCount = 11
    LineColor = clGreen
    LineStyle = psSolid
    ForceLineColor = True
    PointColor = clGreen
    PointStyle = psDiagCross
    FillColor = clGreen
    Caption = 'y = x'#178' + x'
    Chart = XYChart
    Polynom.Order = 2
    Polynom.Coefficients = (
      0.000000000000000000
      1.000000000000000000
      1.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000)
    Left = 272
    Top = 256
  end
  object pd_Squared4: TdzPolynomialDataseries
    ChartType = ctXY
    MinX = -1.000000000000000000
    MaxX = 1.000000000000000000
    PointCount = 11
    LineColor = clFuchsia
    LineStyle = psSolid
    ForceLineColor = False
    PointColor = clFuchsia
    PointStyle = psCircle
    FillColor = clFuchsia
    Caption = 'y = 2x'#178' + 1'
    Chart = XYChart
    Polynom.Order = 2
    Polynom.Coefficients = (
      1.000000000000000000
      0.000000000000000000
      2.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000)
    Left = 312
    Top = 360
  end
  object pd_Sqared3: TdzPolynomialDataseries
    ChartType = ctXY
    MinX = -1.000000000000000000
    MaxX = 1.000000000000000000
    PointCount = 11
    LineColor = clOlive
    LineStyle = psSolid
    ForceLineColor = False
    PointColor = clOlive
    PointStyle = psDiamond
    FillColor = clOlive
    Caption = 'y = x'#178
    Chart = XYChart
    Polynom.Order = 2
    Polynom.Coefficients = (
      0.000000000000000000
      0.000000000000000000
      1.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000
      0.000000000000000000)
    Left = 192
    Top = 360
  end
end
