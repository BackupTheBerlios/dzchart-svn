{GXFormatter.config=twm}
///<summary> declares the IDatssetHelper unterface and the TDatasetHelper implementation
///          for typesafe access to database fields </summary>
unit u_dzDatasetHelpers;

interface

uses
  SysUtils,
  AdoDb,
  DB;

type
  ///<summary> Interface definition for the Dataset-Helper, the idea is to have simplified
  ///          methods for reading field values, converting them to the appropriate data
  ///          type and generate standardized error messages if something goes wrong
  ///          that contain the table and field name rather than just saying
  ///          "Variant conversion error". </summary>
  IDatasetHelper = interface ['{756CC74A-1623-4FC4-A347-4CA3D90B4D69}']
    ///<summary> return the field value as a string, raise an exception if it cannot be converted </summary>
    function FieldAsString(const _Fieldname: string): string; overload;
    ///<summary> return the field value as a string, return the default if it cannot be converted </summary>
    function FieldAsString(const _Fieldname, _Default: string): string; overload;
    ///<summary> sets the field as a string, if the value is empty set it to NULL </summary>
    procedure SetFieldStringNotEmpty(const _Fieldname: string; const _Value: string);

    ///<summary> return the field value as an integer, raise an exception if it cannot be converted </summary>
    function FieldAsInteger(const _Fieldname: string): integer; overload;
    ///<summary> return the field value as an integer, return the default if it cannot be converted </summary>
    function FieldAsInteger(const _Fieldname: string; _Default: integer): integer; overload;
    ///<summary> return the field value as an integer, raise an exception with the given error message if it cannot be converted </summary>
    function FieldAsInteger(const _Fieldname: string; const _Error: string): integer; overload;

    ///<summary> return the field value as a double, raise an exception if it cannot be converted </summary>
    function FieldAsDouble(const _Fieldname: string): double; overload;
    ///<summary> return the field value as a double, return the default if it cannot be converted </summary>
    function FieldAsDouble(const _Fieldname: string; const _Default: double): double; overload;
    ///<summary> return the field value as a double, raise an exception with the given error message if it cannot be converted </summary>
    function FieldAsDouble(const _Fieldname: string; const _Error: string): double; overload;

    ///<summary> return the field value as a TDateTime, raise an exception if it cannot be converted </summary>
    function FieldAsDate(const _Fieldname: string): TDateTime;

    ///<summary> return the field value as a boolean, raise an exception if it cannot be converted </summary>
    function FieldAsBoolean(const _FieldName: string): boolean; overload;
    ///<summary> return the field value as a boolean, return the default if it cannot be converted </summary>
    function FieldAsBoolean(const _FieldName: string; _Default: boolean): boolean; overload;

    ///<summary> Open the dataset </summary>
    procedure Open;
    ///<summary> Close the dataset </summary>
    procedure Close;

    ///<summary> Move to the next record of the dataset </summary>
    procedure Next;
    ///<summary> True if at the end of the dataset </summary>
    function Eof: boolean;
    ///<summary> True if at the beginning of the dataset </summary>
    function Bof: boolean;

    ///<summary> insert a new record into the dataset </summary>
    procedure Insert;
    ///<summary> put the current record into edit mode </summary>
    procedure Edit;

    ///<summary> post changes to the current record (must call Insert or Edit first </summary>
    procedure Post;
    ///<summary> cancel changes to the current record (must call Insert or Edit first </summary>
    procedure Cancel;

    ///<summary> returns the field value as variant (getter method for FieldValues property) </summary>
    function GetFieldValue(const _FieldName: string): Variant;
    ///<summary> sets the field value as variant (setter method for FieldValues property) </summary>
    procedure SetFieldValue(const _FieldName: string; const _Value: Variant);
    ///<summary> allows access to field values as variants </summary>
    property FieldValues[const _FieldName: string]: Variant read GetFieldValue write SetFieldValue; default;
  end;

type
  ///<summary> implements the IDatasetHelper interface </summary>
  TDatasetHelper = class(TInterfacedObject, IDatasetHelper)
  protected
    FDataset: TDataset;
    FTableName: string;
  public
    ///<summary> creates a TDatasetHelper for accessing a TAdoTable or TAdoQuery </summary>
    constructor Create(_Table: TAdoTable); overload;
    ///<summary> creates a TDatasetHelper for accessing a query
    ///          @param Query is the TAdoQuery to access
    ///          @param Tablename is the table name to use for automatically
    ///                           generated error messages </summary>
    constructor Create(_Query: TAdoQuery; const _Tablename: string); overload;

  public // implementation of IDatasetHelper, see there for a description
    function FieldAsString(const _Fieldname: string): string; overload;
    function FieldAsString(const _Fieldname, _Default: string): string; overload;
    procedure SetFieldStringNotEmpty(const _Fieldname: string; const _Value: string);

    function FieldAsInteger(const _Fieldname: string): integer; overload;
    function FieldAsInteger(const _Fieldname: string; _Default: integer): integer; overload;
    function FieldAsInteger(const _Fieldname: string; const _Error: string): integer; overload;

    function FieldAsDouble(const _Fieldname: string): double; overload;
    function FieldAsDouble(const _Fieldname: string; const _Default: double): double; overload;
    function FieldAsDouble(const _Fieldname: string; const _Error: string): double; overload;

    function FieldAsDate(const _Fieldname: string): TDateTime;

    function FieldAsBoolean(const _FieldName: string): boolean; overload;
    function FieldAsBoolean(const _FieldName: string; _Default: boolean): boolean; overload;

    procedure Open;
    procedure Close;

    procedure Next;
    function Eof: boolean;
    function Bof: boolean;

    procedure Insert;
    procedure Edit;

    procedure Post;
    procedure Cancel;

    function GetFieldValue(const _FieldName: string): Variant;
    procedure SetFieldValue(const _FieldName: string; const _Value: Variant);
    property FieldValues[const _FieldName: string]: Variant read GetFieldValue write SetFieldValue; default;
  end;

implementation

uses
  u_dzVariantUtils,
  u_dzMiscUtils;

{ TDatasetHelper }

constructor TDatasetHelper.Create(_Table: TAdoTable);
begin
  inherited Create;
  FDataset := _Table;
  FTableName := _Table.TableName;
end;

constructor TDatasetHelper.Create(_Query: TAdoQuery; const _Tablename: string);
begin
  inherited Create;
  FDataset := _Query;
  FTableName := _Tablename;
end;

function TDatasetHelper.FieldAsDate(const _Fieldname: string): TDateTime;
begin
  Result := Var2DateTimeEx(FDataset[_Fieldname], FTableName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsDouble(const _Fieldname: string): double;
begin
  Result := Var2DblEx(FDataset[_Fieldname], FTableName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsDouble(const _Fieldname, _Error: string): double;
begin
  Result := Var2DblEx(FDataset[_Fieldname], _Error);
end;

function TDatasetHelper.FieldAsInteger(const _Fieldname: string): integer;
begin
  Result := Var2IntEx(FDataset[_Fieldname], FTableName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsInteger(const _Fieldname, _Error: string): integer;
begin
  Result := Var2IntEx(FDataset[_Fieldname], _Error);
end;

function TDatasetHelper.FieldAsString(const _Fieldname: string): string;
begin
  Result := Var2StrEx(FDataset[_Fieldname], FTableName + '.' + _Fieldname);
end;

function TDatasetHelper.FieldAsBoolean(const _FieldName: string): boolean;
begin
  Result := FieldAsInteger(_FieldName) <> 0;
end;

function TDatasetHelper.FieldAsDouble(const _Fieldname: string; const _Default: double): double;
begin
  Result := Var2Dbl(FDataset[_Fieldname], _Default);
end;

function TDatasetHelper.FieldAsInteger(const _Fieldname: string; _Default: integer): integer;
begin
  Result := Var2Int(FDataset[_Fieldname], _Default);
end;

function TDatasetHelper.FieldAsString(const _Fieldname, _Default: string): string;
begin
  Result := Var2Str(FDataset[_Fieldname], _Default);
end;

function TDatasetHelper.FieldAsBoolean(const _FieldName: string; _Default: boolean): boolean;
begin
  Result := FieldAsInteger(_FieldName, BoolToInt(_Default)) <> 0;
end;

procedure TDatasetHelper.SetFieldStringNotEmpty(const _Fieldname, _Value: string);
begin
  if _Value = '' then
    FDataset.Fields.FieldByName(_Fieldname).Clear
  else
    FDataset[_Fieldname] := _Value;
end;

procedure TDatasetHelper.Close;
begin
  FDataset.Close;
end;

function TDatasetHelper.Eof: boolean;
begin
  Result := FDataset.Eof;
end;

function TDatasetHelper.Bof: boolean;
begin
  Result := FDataset.Bof;
end;

procedure TDatasetHelper.Next;
begin
  FDataset.Next;
end;

procedure TDatasetHelper.Open;
begin
  FDataset.Open;
end;

function TDatasetHelper.GetFieldValue(const _FieldName: string): Variant;
begin
  Result := FDataset[_FieldName];
end;

procedure TDatasetHelper.SetFieldValue(const _FieldName: string; const _Value: Variant);
begin
  FDataset[_FieldName] := _Value;
end;

procedure TDatasetHelper.Cancel;
begin
  FDataset.Cancel;
end;

procedure TDatasetHelper.Edit;
begin
  FDataset.Edit;
end;

procedure TDatasetHelper.Insert;
begin
  FDataset.Insert;
end;

procedure TDatasetHelper.Post;
begin
  FDataset.Post;
end;

end.
