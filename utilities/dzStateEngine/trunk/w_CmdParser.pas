unit w_CmdParser;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls;

type
  Tf_CmdParser = class(TForm)
    l_Commandline: TLabel;
    ed_Commandline: TEdit;
    b_Parse: TButton;
    sg_Result: TStringGrid;
    l_Result: TLabel;
    l_ErrorDisplay: TLabel;
    procedure b_ParseClick(Sender: TObject);
  private
  public
  end;

var
  f_CmdParser: Tf_CmdParser;

implementation

{$R *.dfm}

uses
  StrUtils,
  u_CmdParser,
  u_CmdOptionList;

procedure Tf_CmdParser.b_ParseClick(Sender: TObject);
var
  Parser: TCmdParser;
  i: integer;
  cnt: integer;
begin
  Parser := TCmdParser.Create;
  try
    try
      Parser.Parse(ed_Commandline.Text);
      l_Result.Caption := 'Result';
      l_Result.Font.Color := clWindowText;
      Cnt := Parser.Options.Count;
      if Cnt = 0 then begin
        sg_Result.RowCount := 2;
        sg_Result.Cells[0, 1] := '';
        sg_Result.Cells[1, 1] := '';
      end else
        sg_Result.RowCount := Cnt + 1;
      for I := 0 to Cnt - 1 do begin
        sg_Result.Cells[0, i + 1] := Parser.Options[i].Name;
        sg_Result.Cells[1, i + 1] := '"' + Parser.Options[i].Parameter + '"';
      end;
      sg_Result.Visible := true;
    except
      on e: EParsingErrorAt do begin
        l_Result.Caption := e.Message;
        l_Result.Font.Color := clRed;
        l_ErrorDisplay.Caption := LeftStr(ed_Commandline.Text, e.ErrorPosition - 1) +
          '>' + Copy(ed_Commandline.Text, e.ErrorPosition, 1) + '<' +
          Copy(ed_Commandline.Text, e.ErrorPosition + 1);
        sg_Result.Visible := false;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

end.

