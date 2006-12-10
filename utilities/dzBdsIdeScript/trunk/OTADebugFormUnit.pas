unit OTADebugFormUnit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  TOTADebugForm = class(TForm)
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OTADebugForm: TOTADebugForm;

implementation

{$R *.DFM}

procedure TOTADebugForm.FormDestroy(Sender: TObject);
begin
  OTADebugForm := nil;
end;

procedure TOTADebugForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.

