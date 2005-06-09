unit u_HtmlCombobox;

interface

uses
  SysUtils,
  Classes,
  u_HtmlComponent,
  u_HtmlFormField;

type
  THtmlCombobox = class(THtmlFormField)
  private
    function StyleStr: string;
    function OnChangeStr: string;
  protected
    FForm: string;
    FOptions: TStringList;
    FStyle: string;
    function GetSorted: boolean;
    procedure SetSorted(const Value: boolean);
    procedure GetParams(_Params: TStrings); override;
    procedure WriteContent; override;
  public
    constructor Create(const _Name: string);
    destructor Destroy; override;
    procedure MakeAutoSubmit(const _Form: string);
    procedure AddOption(const _Value: string; const _Caption: string = ''; _Selected: boolean = false);
    property Style: string read FStyle write FStyle;
    property Sorted: boolean read GetSorted write SetSorted;
  end;

implementation

uses
  StrUtils;

{ THtmlCombobox }

constructor THtmlCombobox.Create(const _Name: string);
begin
  inherited Create('select', _Name);
  FOptions := TStringList.Create;
end;

destructor THtmlCombobox.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure THtmlCombobox.AddOption(const _Value: string; const _Caption: string = ''; _Selected: boolean = false);
var
  Idx: integer;
begin
  if _Caption <> '' then
    Idx := FOptions.Add(Format('%s=%s', [_Value, _Caption]))
  else
    Idx := FOptions.Add(Format('%s=%s', [_Value, _Value]));
  if _Selected then
    FOptions.Objects[Idx] := pointer(1);
end;

procedure THtmlCombobox.MakeAutoSubmit(const _Form: string);
begin
  FForm := _Form;
end;

function THtmlCombobox.StyleStr: string;
begin
  if FStyle <> '' then begin
    Result := Format('style="%s"', [FStyle]);
  end else begin
    Result := '';
  end;
end;

function THtmlCombobox.OnChangeStr: string;
begin
  if FForm <> '' then begin
    Result := Format('onchange="document.%s.submit();"', [fForm]);
  end else begin
    Result := '';
  end;
end;

function THtmlCombobox.GetSorted: boolean;
begin
  Result := FOptions.Sorted;
end;

procedure THtmlCombobox.SetSorted(const Value: boolean);
begin
  FOptions.Sorted := Value;
end;

procedure THtmlCombobox.GetParams(_Params: TStrings);
begin
  inherited;
  _Params.Add(StyleStr);
  _Params.Add(OnChangeStr);
end;

procedure THtmlCombobox.WriteContent;
const
  SELECTED = 'selected="selected"';
var
  i: integer;
  s: string;
begin
  for i := 0 to FOptions.Count - 1 do begin
    s := FOptions.Names[i];
    WriteFmt('<option %s value="%s">%s</option>'#10, [ifthen(Integer(FOptions.Objects[i]) <> 0, SELECTED, ''), s, FOptions.Values[s]]);
  end;
end;

end.


