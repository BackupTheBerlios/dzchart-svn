unit MLRAutoStringsUnit;

interface

uses
  Classes, ComObj, ActiveX, MLRStringsLib_TLB, StdVcl;

type
  TMLRAutoStrings = class(TAutoObject, IMLRAutoStrings)
  private
    FModified: Boolean;
    FStrings: TStringList;
  protected
    function Add(const S: WideString): Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_Modified: WordBool; safecall;
    function Get_Strings(Index: Integer): WideString; safecall;
    function Get_Text: WideString; safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    procedure Exchange(Index1, Index2: Integer); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    procedure Set_Strings(Index: Integer; const Value: WideString); safecall;
    procedure Set_Text(const Value: WideString); safecall;
    procedure Set_Modified(Value: WordBool); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

function TMLRAutoStrings.Add(const S: WideString): Integer;
begin
  Result := FStrings.Add(S);
  FModified := True;
end;

function TMLRAutoStrings.Get_Count: Integer;
begin
  Result := FStrings.Count;
end;

function TMLRAutoStrings.Get_Modified: WordBool;
begin
  Result := FModified;
end;

function TMLRAutoStrings.Get_Strings(Index: Integer): WideString;
begin
  Result := FStrings[Index];
end;

function TMLRAutoStrings.Get_Text: WideString;
begin
  Result := FStrings.Text;
end;

procedure TMLRAutoStrings.Clear;
begin
  FStrings.Clear;
  FModified := True;
end;

procedure TMLRAutoStrings.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
  FModified := True;
end;

procedure TMLRAutoStrings.Exchange(Index1, Index2: Integer);
begin
  if Index1 <> Index2 then begin
    FStrings.Exchange(Index1, Index2);
    FModified := True;
  end;
end;

procedure TMLRAutoStrings.LoadFromFile(const FileName: WideString);
begin
  FStrings.LoadFromFile(FileName);
  FModified := True;
end;

procedure TMLRAutoStrings.SaveToFile(const FileName: WideString);
begin
  FStrings.SaveToFile(FileName);
end;

procedure TMLRAutoStrings.Set_Strings(Index: Integer;
  const Value: WideString);
begin
  FStrings[Index] := Value;
  FModified := True;
end;

procedure TMLRAutoStrings.Set_Text(const Value: WideString);
begin
  FStrings.Text := Value;
  FModified := True;
end;

destructor TMLRAutoStrings.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TMLRAutoStrings.Initialize;
begin
  FStrings := TStringList.Create;
  inherited;
end;

procedure TMLRAutoStrings.Set_Modified(Value: WordBool);
begin
  FModified := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TMLRAutoStrings, Class_MLRAutoStrings,
    ciMultiInstance, tmApartment);
end.
