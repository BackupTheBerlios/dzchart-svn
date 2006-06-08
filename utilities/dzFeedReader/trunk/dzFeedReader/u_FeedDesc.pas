unit u_FeedDesc;

interface

uses
  SysUtils;

type
  TFeedDesc = class
  private
    FFeedName: string;
    FFeedUrl: string;
    FFeedKey: string;
    FTop: integer;
    FLeft: integer;
    FWidth: integer;
    FHeight: integer;
  public
    constructor Create;
    procedure Assign(_Src: TFeedDesc);
    function IsFilebased: boolean;
    property FeedName: string read FFeedName write FFeedName;
    property FeedUrl: string read FFeedUrl write FFeedUrl;
    property FeedKey: string read FFeedKey write FFeedKey;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    class function CreateGuidString: string;
  end;

implementation

{ TFeedDesc }

uses
  StrUtils;

procedure TFeedDesc.Assign(_Src: TFeedDesc);
begin
  FFeedKey := _Src.FeedKey;
  FeedName := _Src.FeedName;
  FeedUrl := _Src.FeedUrl;
end;

constructor TFeedDesc.Create;
begin
  inherited Create;
  FeedKey := CreateGuidString;
end;

class function TFeedDesc.CreateGuidString: string;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;

function TFeedDesc.IsFilebased: boolean;
begin
  Result := not StartsText('http', FFeedUrl);
//    frm.Url := s // 'http://rss.slashdot.org/Slashdot/slashdot';
//  else
//    frm.Filename := s; //'d:\slashdot.xml';
end;

end.

