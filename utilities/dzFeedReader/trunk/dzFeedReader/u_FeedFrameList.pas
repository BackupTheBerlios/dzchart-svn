unit u_FeedFrameList;

interface

uses
  Classes,
  u_dzQuicksort,
  wf_RssFrame;

{$DEFINE __DZ_SORTED_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer; 
  _ITEM_TYPE_ = Tfr_RssFrame;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedListTemplate.tpl'}

type
  {: Sorted list for storing Tfr_RssFrame items sorted by string }
  TFeedFrameList = class(_DZ_SORTED_LIST_TEMPLATE_)
  protected
    {: return the key of an item for comparison }
    function KeyOf(const _Item: Tfr_RssFrame): string; override;
    {: compare the keys of two items, must return a value
       < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: string): integer; override;
    {: Frees a Tfr_RssFrame }
    procedure FreeItem(_Item: Tfr_RssFrame); override;
  end;

implementation

uses
  SysUtils, u_FeedDesc;

{$INCLUDE 't_dzSortedListTemplate.tpl'}

function TFeedFrameList.KeyOf(const _Item: Tfr_RssFrame): string;
begin
  Result := _Item.Feed.FeedKey;
end;

function TFeedFrameList.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

procedure TFeedFrameList.FreeItem(_Item: Tfr_RssFrame);
begin
  // do not free!
end;

end.
