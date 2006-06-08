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

This File Originally Created By Thomas Zangl <thomas@tzis.net> 2005
--------------------------------------------------------------------------------
}
unit SimpleParserBase;

interface

uses
  Classes,
  SimpleRSS,
  XMLIntf;

type
  TSimpleParserBase = class(TObject)
  private
  protected
    FSimpleRSS: TSimpleRSS;
    function GetNodeValue(_Node: IXmlNode; const _Default: string = ''): string;
    function TryGetChildnodeValue(_ChildNodes: IXMLNodeList; const _NodeName: string): string; overload;
    function TryGetChildnodeValue(_ChildNodes: IXMLNodeList; const _NodeName: string; out _NodeValue: string): boolean; overload;
  public
    procedure Generate; virtual; abstract;
    procedure Parse; virtual; abstract;
    constructor Create(SimpleRSS: TSimpleRSS);
  published
  end; { TSimpleParserBase }

implementation

uses
  Variants;

constructor TSimpleParserBase.Create(SimpleRSS: TSimpleRSS);
begin
  inherited Create;
  FSimpleRSS := SimpleRSS;
end;

function TSimpleParserBase.GetNodeValue(_Node: IXmlNode; const _Default: string = ''): string;
begin
  try
    if Assigned(_Node) then
      Result := VarToStrDef(_Node.NodeValue, _Default)
    else
      Result := _Default;
  except
    Result := 'error parsing NodeValue'
  end;
end;

function TSimpleParserBase.TryGetChildnodeValue(_ChildNodes: IXMLNodeList;
  const _NodeName: string; out _NodeValue: string): boolean;
var
  Node: IXMLNode;
begin
  Node := _ChildNodes.FindNode(_NodeName);
  Result := Assigned(Node);
  if Result then
    _NodeValue := GetNodeValue(Node);
end;

function TSimpleParserBase.TryGetChildnodeValue(_ChildNodes: IXMLNodeList; const _NodeName: string): string;
begin
  TryGetChildnodeValue(_ChildNodes, _NodeName, Result);
end;

end.

