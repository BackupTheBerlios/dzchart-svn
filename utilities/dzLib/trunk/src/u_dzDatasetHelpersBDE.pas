unit u_dzDatasetHelpersBDE;

interface

uses
  SysUtils,
  db,
  DBTables,
  u_dzTranslator,
  u_dzDatasetHelpers;

type
  TDataSetHelperBDE = class(TDatasetHelper)
  protected
    procedure SetParamByName(const _Param: string; _Value: variant); override;
    function TrySetParamByName(const _Param: string; _Value: variant): Boolean; override;
  public
    constructor Create(_Table: TTable); overload;
  end;

implementation

{ TDataSetHelperBDE }

constructor TDataSetHelperBDE.Create(_Table: TTable);
begin
  inherited Create(_Table, _Table.TableName);
end;

procedure TDataSetHelperBDE.SetParamByName(const _Param: string; _Value: variant);
var
  i: Integer;
  Query: TQuery;
begin
  if FDataset is TQuery then begin
    Query := (FDataset as TQuery);
    for i := 0 to Query.Params.Count - 1 do
      if SameText(Query.Params[i].Name, _Param) then
        Query.Params[i].Value := _Value;
  end else
    raise Exception.CreateFmt(_('SetParamByName is not supported for a %s (only TQuery descendants).'), [FDataset.ClassName]);
end;

function TDataSetHelperBDE.TrySetParamByName(const _Param: string; _Value: variant): Boolean;
var
  BdeParam: TParam;
begin
  if FDataset is TQuery then begin
    BdeParam := (FDataset as TQuery).Params.FindParam(_Param);
    Result := Assigned(BdeParam);
    if Result then
      BdeParam.Value := _Value;
  end else
    raise Exception.CreateFmt(_('TrySetParamByName is not supported for a %s (only TQuery descendants).'), [FDataset.ClassName]);
end;

end.

