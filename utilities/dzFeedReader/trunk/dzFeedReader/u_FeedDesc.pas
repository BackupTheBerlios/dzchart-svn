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
  public
    constructor Create;
    procedure Assign(_Src: TFeedDesc);
    function IsFilebased: boolean;
    property FeedName: string read FFeedName write FFeedName;
    property FeedUrl: string read FFeedUrl write FFeedUrl;
    property FeedKey: string read FFeedKey write FFeedKey;
    class function CreateGuidString: string;
  end;

implementation

{ TFeedDesc }

uses
  StrUtils;

procedure TFeedDesc.Assign(_Src: TFeedDesc);
begin
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

