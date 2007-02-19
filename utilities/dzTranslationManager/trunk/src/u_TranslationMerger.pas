{GXFormatter.config=headwork}
unit u_TranslationMerger;

interface

uses
  poparser;

type
  {: Merges several translation sources. It always keeps the first
     translation it finds unless it is fuzzy and a new one isn't.
     Empty MsgIds and MsgStrs are ignored. }
  TTranslationMerger = class
  private
    FTranslations: TPoEntryList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTranslationSource(const _Filename: string);
    function DetachTranslations: TPoEntryList;
  end;

implementation

{ TTranslationMerger }

procedure TTranslationMerger.AddTranslationSource(const _Filename: string);
var
  NewList: TPoEntryList;
  NewEntry: TPoEntry;
  Existing: TPoEntry;
begin
  NewList := TPoEntryList.Create;
  NewList.LoadFromFile(_Filename);
  NewEntry := NewList.FindFirst;
  while NewEntry <> nil do
    begin
      if (NewEntry.MsgId <> '') and (NewEntry.MsgStr <> '') then
        begin
          Existing := FTranslations.Find(NewEntry.MsgId);
          if Existing = nil then
            FTranslations.Add(NewEntry)
          else if Existing.Fuzzy and not NewEntry.Fuzzy then
            begin
              FTranslations.Delete(Existing.MsgId);
              FTranslations.Add(NewEntry);
            end;
        end;
      NewEntry := NewList.FindNext(NewEntry);
    end;
end;

constructor TTranslationMerger.Create;
begin
  inherited Create;
  FTranslations := TPoEntryList.Create;
end;

destructor TTranslationMerger.Destroy;
begin
  FTranslations.Free;
  inherited;
end;

function TTranslationMerger.DetachTranslations: TPoEntryList;
begin
  Result := FTranslations;
  FTranslations := TPoEntryList.Create;
end;

end.

