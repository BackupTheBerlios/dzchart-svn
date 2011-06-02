unit u_dzDatasetHelpersTdbf;

interface

uses
  u_dzDatasetHelpers,
  dbf;

type
  TDataSetHelperTdbf = class(TDatasetHelper, IDatasetHelper)
  public
    constructor Create(_Table: TDBF); overload;
  end;

implementation

{ TDataSetHelperTdbf }

constructor TDataSetHelperTdbf.Create(_Table: TDBF);
begin
  inherited Create;
  FDataset := _Table;
  FTableName := _Table.TableName;
end;

end.
