unit u_HtmlComponent;

interface

uses
  SysUtils,
  Classes;

type
  THtmlComponent = class
  protected
    FTag: string;
    FCssClass: string;
    FId: string;
    FContent: TStringStream;
    procedure Write(const _s: string);
    procedure WriteFmt(const _Fmt: string; const _Params: array of const);
    function CssClassStr: string; virtual;
    function IdStr: string; virtual;
    function StartTag(_ShortForm: boolean): string; virtual;
    function EndTag: string; virtual;
    procedure GetParams(_Params: TStrings); virtual;
    procedure WriteContent; virtual;
  public
    constructor Create(const _Tag: string; const _CssClass: string = '');
    destructor Destroy; override;
    function Html: string;
    property CssClass: string read fCssClass write fCssClass;
    property Id: string read FId write FId;
  end;

implementation

{ THtmlComponent }

constructor THtmlComponent.Create(const _Tag: string; const _CssClass: string = '');
begin
  inherited Create;
  FTag := _Tag;
  FCssClass := _CssClass;
  fContent := TStringStream.Create('');
end;

destructor THtmlComponent.Destroy;
begin
  fContent.Free;
  inherited;
end;

function THtmlComponent.Html: string;
var
  s: string;
  HasContent: boolean;
begin
  fContent.Size := 0;
  WriteContent;
  s := fContent.DataString;
  HasContent := (s <> '');
  Result := StartTag(not HasContent) + s;
  if HasContent then
    Result := Result + EndTag;
end;

function THtmlComponent.CssClassStr: string;
begin
  if FCssClass <> '' then begin
    Result := Format('class="%s"', [fCssClass]);
  end else begin
    Result := '';
  end;
end;

function THtmlComponent.IdStr: string;
begin
  if FId <> '' then begin
    Result := Format('id="%s"', [fId]);
  end else begin
    Result := '';
  end;
end;

procedure THtmlComponent.Write(const _s: string);
begin
  fContent.Write(_s[1], Length(_s));
end;

procedure THtmlComponent.WriteFmt(const _Fmt: string; const _Params: array of const);
begin
  Write(Format(_Fmt, _Params));
end;

procedure THtmlComponent.GetParams(_Params: TStrings);
begin
  _Params.Add(IdStr);
  _Params.Add(CssClassStr);
end;

function THtmlComponent.StartTag(_ShortForm: boolean): string;

  function AddIfNotEmpty(const _Param: string): string;
  begin
    if _Param <> '' then begin
      Result := ' ' + _Param;
    end else begin
      Result := '';
    end;
  end;
  
var
  i: integer;
  Params: TStringList;
begin
  Result := '<'+ FTag;
  Params := TStringList.Create;
  try
    GetParams(Params);
    for i:= 0 to Params.Count - 1 do begin
      Result := Result + AddIfNotEmpty(Params[i]);
    end;
  finally
    Params.Free;
  end;
  if _ShortForm then
    Result := Result + ' /';
  Result := Result + '>';
end;

function THtmlComponent.EndTag: string;
begin
  Result := Format('</%s>', [FTag]);
end;

procedure THtmlComponent.WriteContent;
begin
  // default: no content
end;

end.
