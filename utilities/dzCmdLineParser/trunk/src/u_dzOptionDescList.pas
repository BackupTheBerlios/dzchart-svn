unit u_dzOptionDescList;

interface

uses
  SysUtils,
  Classes,
  u_dzQuicksort;

type
  EOptionDesc = class(Exception);
  EOptionName = class(EOptionDesc);

type
  TOptionDesc = class
  private
    function CreateDescription(const _OptionName: string): string;
    procedure AssertValidOptionName(_Name: string);
  protected
    FPrimaryName: string;
    FDescription: string;
    FHasValue: boolean;
    FNames: TStringList;
  public
    constructor Create(const _Names: array of string; const _Description: string;
      _HasValue: boolean = false);
    destructor Destroy; override;
    function GetDescription(_Indent: integer): string;
    property HasValue: boolean read FHasValue;
    property PrimaryName: string read FPrimaryName;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TOptionDesc;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  {: List for storing TOptionDesc items sorted by String }
  TOptionDescList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    {: return the key of an item for comparison }
    function KeyOf(const _Item: TOptionDesc): string; override;
    {: compare the keys of two items, must return a value
       < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: string): integer; override;
  end;

implementation

uses
  StrUtils;

resourcestring
  RS_OPTION_NAME_CANNOT_BE_EMPTY = 'Option name cannot be empty.';
  RS_OPTION_NAME_MUST_START_WITH_ALPHANUMERIC = 'Option name must start with an alphanumeric character.';
  RS_OPTION_NAME_CONTAINS_INVALID_CHAR_SD = 'Option name contains invalid character "%s" (%d).';
  RS_VALUE = 'value';

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TOptionDescList.KeyOf(const _Item: TOptionDesc): string;
begin
  Result := _Item.PrimaryName;
end;

function TOptionDescList.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

constructor TOptionDesc.Create(const _Names: array of string;
  const _Description: string; _HasValue: boolean = false);
var
  i: integer;
  s: string;
begin
  inherited Create;
  FDescription := _Description;
  FHasValue := _HasValue;
  Assert(Length(_Names) > 0);
  FPrimaryName := _Names[0];
  AssertValidOptionName(FPrimaryName);
  FNames := TStringList.Create;
  for i := 0 to high(_Names) do begin
    s := _Names[i];
    AssertValidOptionName(s);
    fNames.Add(s);
  end;
end;

destructor TOptionDesc.Destroy;
begin
  FNames.Free;
  inherited;
end;

procedure TOptionDesc.AssertValidOptionName(_Name: string);
var
  i: integer;
begin
  if _Name = '' then
    raise EOptionName.Create(RS_OPTION_NAME_CANNOT_BE_EMPTY);
  { TODO -otwm : Maybe '$', '#' and some other chars should be allowed }
  if not (_Name[1] in ['a'..'z', 'A'..'Z', '0'..'9', '?']) then
    raise EOptionName.Create(RS_OPTION_NAME_MUST_START_WITH_ALPHANUMERIC);
  for i := 2 to Length(_Name) do
    if not (_Name[i] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
      raise EOptionName.CreateFmt(RS_OPTION_NAME_CONTAINS_INVALID_CHAR_SD, [_Name[i], Ord(_Name[i])]);
end;

function TOptionDesc.CreateDescription(const _OptionName: string): string;
begin
  case Length(_OptionName) of
    0: Result := ''; // should never happen
    1: begin
        Result := '-' + _OptionName;
        if FHasValue then
          Result := Result + ' ' + RS_VALUE;
      end;
  else begin
      Result := '--' + _OptionName;
      if FHasValue then
        Result := Result + '=' + RS_VALUE;
    end
  end;
end;

function TOptionDesc.GetDescription(_Indent: integer): string;
var
  i: integer;
  Len: integer;
  s: string;
begin
  Len := 0;
  Result := '';
  for i := 0 to fNames.Count - 1 do begin
    s := CreateDescription(FNames[i]);
    Len := Length(s);
    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + s;
  end;
  Result := Result + StringOfChar(' ', _Indent - Len - 3) + ' : ' + fDescription;
end;

end.

