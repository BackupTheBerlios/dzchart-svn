unit u_HtmlTable;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  u_HtmlComponent,
  u_HtmlContainer,
  u_HtmlPrimitive;

type
  THtmlTableCell = class(THtmlPrimitive)
  private
    FColSpan: integer;
    FWidth: string;
    FAlign: string;
  protected
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create;
    property ColSpan: integer read fColSpan write fColSpan;
    property Width: string read FWidth write FWidth;
    property Align: string read FAlign write FAlign;
  end;

  THtmlTableRow = class(THtmlContainer)
  private
    FAlign: string;
  protected
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create;
    function AddCell: THtmlTableCell;
    property Align: string read FAlign write FAlign;
  end;

  THtmlTable = class(THtmlContainer)
  protected
    FBorder: integer;
    FCellpadding: integer;
    FCellspacing: integer;
    FWidth: string;
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create;
    function AddRow: THtmlTableRow;
    property Border: integer read FBorder write FBorder;
    property Cellpadding: integer read FCellpadding write FCellpadding;
    property Cellspacing: integer read FCellspacing write FCellspacing;
    property Width: string read FWidth write FWidth;
  end;

implementation

{ THtmlTableCell }

constructor THtmlTableCell.Create;
begin
  inherited Create('td');
  fColSpan := 1;
  fWidth := '';
end;

procedure THtmlTableCell.GetParams(_Params: TStrings);
begin
  inherited;
  if FColSpan > 1 then begin
    _Params.Add(Format(' colspan="%d"', [FColSpan]));
  end;
  if FWidth <> '' then begin
    _Params.Add(Format(' width="%s"', [FWidth]));
  end;
  if FAlign <> '' then begin
    _Params.Add(Format(' align="%s"', [FAlign]));
  end;
end;

{ THtmlTableRow }

constructor THtmlTableRow.Create;
begin
  inherited Create('tr');
end;

function THtmlTableRow.AddCell: THtmlTableCell;
begin
  Result := THtmlTableCell.Create;
  FItems.Add(Result);
end;

procedure THtmlTableRow.GetParams(_Params: TStrings);
begin
  inherited;
  if FAlign <> '' then
    _Params.Add(Format('align="%s"', [FAlign]));
end;

{ THtmlTable }

constructor THtmlTable.Create;
begin
  inherited Create('table');
  FBorder := -1;
  FCellpadding := -1;
  FCellspacing := -1;
  FWidth := '';
end;

function THtmlTable.AddRow: THtmlTableRow;
begin
  Result := THtmlTableRow.Create;
  FItems.Add(Result);
end;

procedure THtmlTable.GetParams(_Params: TStrings);
begin
  inherited;
  if FBorder <> -1 then
    _Params.Add(Format('border="%d"', [FBorder]));
  if FCellpadding <> -1 then
    _Params.Add(Format('cellpadding="%d"', [FCellpadding]));
  if FCellspacing <> -1 then
    _Params.Add(Format('cellspacing="%d"', [FCellspacing]));
  if FWidth <> '' then
    _Params.Add(Format('width="%s"', [FWidth]));
end;

end.
