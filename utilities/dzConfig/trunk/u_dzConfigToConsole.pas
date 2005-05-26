{: Implements TConfigToConsole, a simple class implementing the IdzConfigWriter
   interface by dumping everyting to the console using WriteLn. }
unit u_dzConfigToConsole;

interface

uses
  SysUtils,
  Classes,
  u_dzConfigCommon;

type
  {: A simple class implementing the IdzConfigWriter interface by
     dumping everyting to the console using WriteLn. }
  TConfigToConsole = class(TInterfacedObject, IdzConfigWriter)
  protected // IdzConfigWriter interface
    {: Writes a boolean setting to the console }
    procedure StoreSetting(const _Path, _Name: string; const _Value: boolean); overload;
    {: Writes an integer setting to the console }
    procedure StoreSetting(const _Path, _Name: string; const _Value: integer); overload;
    {: Writes a string setting to the console }
    procedure StoreSetting(const _Path, _Name: string; const _Value: string); overload;
  end;

implementation

{ TConfigToConsole }

procedure TConfigToConsole.StoreSetting(const _Path, _Name: string; const _Value: boolean);
begin
  WriteLn(_Path + '\' + _Name, ' = ', _Value);
end;

procedure TConfigToConsole.StoreSetting(const _Path, _Name: string; const _Value: integer);
begin
  WriteLn(_Path + '\' + _Name, ' = ', _Value);
end;

procedure TConfigToConsole.StoreSetting(const _Path, _Name, _Value: string);
begin
  WriteLn(_Path + '\' + _Name, ' = "', _Value, '"');
end;

end.

