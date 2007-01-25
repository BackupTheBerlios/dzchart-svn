{
--------------------------------------------------------------------------------
SimpleRSS Version 0.4 (BlueHippo)

http://simplerss.sourceforge.net
Provides a simple methods for accessing, importing, exporting and working with RSS, RDF, Atom & iTunes Feeds

SimpleRSS Originally Created By Robert MacLean
SimpleRSS (C) Copyright 2003-2005 Robert MacLean. All Rights Reserved World Wide

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

This File Originally Created By Robert MacLean <dfantom@gmail.com> 2003
Additional work by:
 - Thomas Zangl <thomas@tzis.net> 2005
--------------------------------------------------------------------------------
}
unit SimpleRSS;

interface
uses
  SysUtils,
  Classes,
  SimpleRSSTypes,
  XMLDoc,
  XMLIntf,
  XMLDOM,
  Variants,
  idHTTP;

type
  TSimpleRSS = class(TComponent)
  private
    FChannel: TRSSChannel;
    FItems: TRSSItems;
    FVersion: string;
    FXMLType: TXMLType;
    FXMLFile: TXMLDocument;
    FIndyHTTP: TIdHTTP;
    FOnParseXML: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnGenerateXML: TNotifyEvent;
    procedure SetChannel(const Value: TRSSChannel);
    procedure SetItems(const Value: TRSSItems);
    procedure SetVersion(const Value: string);
    procedure SetIndyHTTP(const Value: TIdHTTP);
    function GetSimpleRSSVersion: string;
    procedure SetOnCreate(const Value: TNotifyEvent);
    procedure SetOnGenerateXML(const Value: TNotifyEvent);
    procedure SetOnParseXML(const Value: TNotifyEvent);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(Filename: string);
    function SaveToString: string;
    procedure SaveToStream(Stream: TStream);
    function SaveToStrings: TStrings;
    procedure LoadFromHTTP(URL: string);
    procedure LoadFromString(S: string);
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromStrings(Strings: TStrings);
    procedure GenerateXML; overload;
    procedure GenerateXML(FeedType: TXMLType); overload;
    procedure GenerateComponent;
    procedure ClearXML;
  published
    { Published declarations }
    property Channel: TRSSChannel read FChannel write SetChannel;
    property Items: TRSSItems read FItems write SetItems;
    property Version: string read FVersion write SetVersion;
    property XMLType: TXMLType read FXMLType write FXMLType;
    property XMLFile: TXMLDocument read FXMLFile;
    property IndyHTTP: TIdHTTP read FIndyHTTP write SetIndyHTTP;
    property SimpleRSSVersion: string read GetSimpleRSSVersion;
    property OnCreate: TNotifyEvent read FOnCreate write SetOnCreate;
    property OnGenerateXML: TNotifyEvent read FOnGenerateXML write SetOnGenerateXML;
    property OnParseXML: TNotifyEvent read FOnParseXML write SetOnParseXML;
  end;

procedure Register;

implementation

uses
  SimpleRSSConst,
  SimpleParserRDF,
  SimpleParserRSS,
  SimpleParserAtom,
  SimpleParserBase,
  SimpleParseriTunes;

procedure Register;
begin
  RegisterComponents('WebTools', [TSimpleRSS]);
end;
{ TSimpleRSS }

procedure TSimpleRSS.ClearXML;
begin
  FXMLFile.Active := False;
  FXMLFile.Free;
  FXMLFile := TXMLDocument.Create(Self);
//  FXMLFile.DOMVendor := GetDOMVendor('Open XML 3.2');
  FXMLFile.DOMVendor := GetDOMVendor('Open XML');
  FXMLFile.Active := True;
end;

constructor TSimpleRSS.Create(AOwner: TComponent);
begin
  inherited;
  FChannel := TRSSChannel.Create;
  FItems := TRSSItems.Create(Self, TRSSItem);
  FXMLFile := TXMLDocument.Create(Self);
//  FXMLFile.DOMVendor := GetDOMVendor('Open XML 3.2');
  FXMLFile.DOMVendor := GetDOMVendor('Open XML');
  FXMLFile.Active := True;
  FVersion := strRSSVersion;
  FXMLType := xtRSS;
  FXMLFile.Options := [doNodeAutoCreate, doAttrNull, doAutoPrefix, doNodeAutoIndent];
  if Assigned(FOnCreate) then
    FOnCreate(Self);
end;

destructor TSimpleRSS.Destroy;
begin
  if FChannel <> nil then
    FChannel.Free;
  if FItems <> nil then
    FItems.Free;
  if FXMLFile <> nil then
    FXMLFile.Free;
  inherited;
end;

procedure TSimpleRSS.GenerateComponent;
var
  aParser: TSimpleParserBase;
begin
  FChannel.Free;
  FChannel := TRSSChannel.Create;
  FItems.Free;
  FItems := TRSSItems.Create(Self, TRSSItem);

  if (LowerCase(FXMLFile.DocumentElement.NodeName) = reFormatRDF) then begin
    FXMLType := xtRDF
  end // if then
  else begin
    if (LowerCase(FXMLFile.DocumentElement.NodeName) = reFormatRSS) then begin
      if FXMLFile.DocumentElement.Attributes[ituneNS] <> null then
        FXMLType := xtiTunes
      else
        FXMLType := xtRSS
    end // if then
    else begin
      if (LowerCase(FXMLFile.DocumentElement.NodeName) = reFormatAtom) then begin
        FXMLType := xtAtom
      end // if then
      else
        raise ESimpleRSSException.CreateFmt(reFormatUnkown + ': %s', [FXMLFile.DocumentElement.NodeName]);
    end; // if else
  end; // if else

  if (not VarIsNull(FXMLFile.DocumentElement.Attributes[reVersion])) then
    FVersion := FXMLFile.DocumentElement.Attributes[reVersion];

  case FXMLType of
    xtRDF: aParser := TSimpleParserRDF.Create(Self);
    xtRSS: aParser := TSimpleParserRSS.Create(Self);
    xtAtom: aParser := TSimpleParserAtom.Create(Self);
    xtiTunes: aParser := TSimpleParseriTunes.Create(Self);
  else
    aParser := nil;
  end;

  if (aParser <> nil) then begin
    aParser.Parse;

    if Assigned(FOnParseXML) then
      FOnParseXML(Self);
  end else
    raise ESimpleRSSException.CreateFmt(reFormatUnkown + ': %s', [FXMLFile.DocumentElement.NodeName]);
end;

procedure TSimpleRSS.GenerateXML;
begin
  GenerateXML(FXMLType);
end;

procedure TSimpleRSS.GenerateXML(FeedType: TXMLType);
var
  aParser: TSimpleParserBase;
begin
  case FeedType of
    xtRSS: aParser := TSimpleParserRSS.Create(Self);
    xtRDF: aParser := TSimpleParserRDF.Create(Self);
    xtAtom: aParser := TSimpleParserAtom.Create(Self);
    xtiTunes: aParser := TSimpleParseriTunes.Create(Self);
  else
    aParser := nil;
  end; // case of

  if aParser <> nil then begin
    aParser.Generate;
    if Assigned(FOnGenerateXML) then
      FOnGenerateXML(Self);
  end else
    raise ESimpleRSSException.Create(reFormatUnkown);
end;

function TSimpleRSS.GetSimpleRSSVersion: string;
begin
  Result := strSimpleRSSVersion;
end;

procedure TSimpleRSS.LoadFromFile(Filename: string);
begin
  ClearXML;
  FXMLFile.LoadFromFile(Filename);
  GenerateComponent;
end;

procedure TSimpleRSS.LoadFromHTTP(URL: string);
var
  MemoryStream: TMemoryStream;
begin
  if FIndyHTTP = nil then
    raise ESimpleRSSException.Create(emRequireComponentMissing + strIdHTTPComponent)
  else begin
    MemoryStream := TMemoryStream.Create;
    try
      FIndyHTTP.Get(URL, MemoryStream);
      MemoryStream.Position := 0;
      LoadFromStream(MemoryStream);
    finally
      MemoryStream.Free;
    end; // try finally
  end; // if then
end;

procedure TSimpleRSS.LoadFromStream(Stream: TStream);
begin
  ClearXML;
  FXMLFile.LoadFromStream(Stream);
  GenerateComponent;
end;

procedure TSimpleRSS.LoadFromString(S: string);
begin
  FXMLFile.XML.Text := S;
end;

procedure TSimpleRSS.LoadFromStrings(Strings: TStrings);
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  Strings.SaveToStream(MemoryStream);
  FXMLFile.LoadFromStream(MemoryStream);
  MemoryStream.Free;
end;

procedure TSimpleRSS.SaveToFile(Filename: string);
begin
  GenerateXML;
  FXMLFile.SaveToFile(Filename);
end;

procedure TSimpleRSS.SaveToStream(Stream: TStream);
begin
  GenerateXML;
  FXMLFile.SaveToStream(Stream);
end;

function TSimpleRSS.SaveToString: string;
begin
  GenerateXML;
  Result := FXMLFile.XML.Text;
end;

function TSimpleRSS.SaveToStrings: TStrings;
begin
  GenerateXML;
  Result := FXMLFile.XML;
end;

procedure TSimpleRSS.SetChannel(const Value: TRSSChannel);
begin
  FChannel := Value;
end;

procedure TSimpleRSS.SetIndyHTTP(const Value: TIdHTTP);
begin
  FIndyHTTP := Value;
end;

procedure TSimpleRSS.SetItems(const Value: TRSSItems);
begin
  FItems := Value;
end;

procedure TSimpleRSS.SetOnParseXML(const Value: TNotifyEvent);
begin
  FOnParseXML := Value;
end;

procedure TSimpleRSS.SetOnCreate(const Value: TNotifyEvent);
begin
  FOnCreate := Value;
end;

procedure TSimpleRSS.SetOnGenerateXML(const Value: TNotifyEvent);
begin
  FOnGenerateXML := Value;
end;

procedure TSimpleRSS.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

end.

