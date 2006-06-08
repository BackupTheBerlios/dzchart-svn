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
Additional work by:
 - Robert MacLean <dfantom@gmail.com> 2005
 - Thomas Mueller <http://www.dummzeuch.de> 2006

Spec
http://www.w3.org/TR/1999/REC-rdf-syntax-19990222/
http://web.resource.org/rss/1.0/spec
--------------------------------------------------------------------------------
}
unit SimpleParserRDF;

interface

uses
  Classes,
  SimpleParserBase,
  XMLIntf,
  Variants;

type
  TSimpleParserRDF = class(TSimpleParserBase)
  private
    procedure ParseItem(_ItemNode: IXMLNode);
    procedure ParseImage(_ImageNode: IXMLNode);
    procedure ParseTextInput(_TextInputNode: IXMLNode);
  protected
  public
    procedure Generate; override;
    procedure Parse; override;
  published
  end; { TSimpleParserRDF }

implementation

uses SimpleRSSTypes,
  SimpleRSSConst,
  SimpleRSSUtils,
  XMLDoc,
  SysUtils,
  SimpleRSS;

procedure TSimpleParserRDF.Generate;
var
  Counter: Integer;
  XNode, Item, Channel, TOC, Image, TextInput: IXMLNode;
begin
  //todo: generate RDF - fix the attack of the xmlns issue
  FSimpleRSS.ClearXML;
  // ROOT
  XNode := FSimpleRSS.XMLFile.CreateNode(strXMLHeader, ntProcessingInstr, strXMLVersion);
  FSimpleRSS.XMLFile.ChildNodes.Add(XNode);
  XNode := FSimpleRSS.XMLFile.CreateElement(rdfeRDF, rdfeRDFNSUrl);
  XNode.DeclareNamespace('', rdfeXMLNSUrl);
  FSimpleRSS.XMLFile.ChildNodes.Add(XNode);
  XNode := FSimpleRSS.XMLFile.CreateNode(Format(strAdvert, [FormatDateTime(strDateFormat, Now)]), ntComment);
  FSimpleRSS.XMLFile.ChildNodes.Nodes[rdfeRDF].ChildNodes.Add(XNode);
  // END ROOT

  // CHANNEL
  Channel := FSimpleRSS.XMLFile.CreateNode(reChannel);
  Channel.Attributes[rdfeAbout] := FSimpleRSS.Channel.AboutURL;
  // REQUIRED
  Channel.ChildNodes.Nodes[reTitle].NodeValue := FSimpleRSS.Channel.Required.Title;
  Channel.ChildNodes.Nodes[reLink].NodeValue := FSimpleRSS.Channel.Required.Link;
  Channel.ChildNodes.Nodes[reDescription].NodeValue := FSimpleRSS.Channel.Required.Desc;
  // END REQUIRED
  // OPTIONAL

  if FSimpleRSS.Channel.Optional.Image.Include then begin
    XNode := FSimpleRSS.XMLFile.CreateNode(reImage);
    XNode.Attributes[rdfeResource] := FSimpleRSS.Channel.Optional.Image.Required.URL;
    Channel.ChildNodes.Add(XNode);

    Image := Channel.AddChild(reImage);
//      Image := FSimpleRSS.XMLFile.CreateNode(reImage);
      // IMAGE REQUIRED
    Image.Attributes[rdfeAbout] := FSimpleRSS.Channel.Optional.Image.Required.URL;
    Image.ChildNodes.Nodes[reURL].NodeValue := FSimpleRSS.Channel.Optional.Image.Required.URL;
    Image.ChildNodes.Nodes[reTitle].NodeValue := FSimpleRSS.Channel.Optional.Image.Required.Title;
    Image.ChildNodes.Nodes[reLink].NodeValue := FSimpleRSS.Channel.Optional.Image.Required.Link;
      // END IMAGE REQUIRED
  end; // if then IMAGE

  // TOC
  TOC := FSimpleRSS.XMLFile.CreateNode(rdfeItems);
  XNode := FSimpleRSS.XMLFile.CreateNode(rdfeSeq);
  for Counter := 0 to FSimpleRSS.Items.Count - 1 do begin
    if FSimpleRSS.Items.Items[Counter].TitleChanged or FSimpleRSS.Items.Items[Counter].DescriptionChanged then begin
      Item := FSimpleRSS.XMLFile.CreateNode(rdfeLi);
      Item.Attributes[rdfeResource] := FSimpleRSS.Items.Items[Counter].Link;
      XNode.ChildNodes.Add(Item);
    end; // if then
  end; // for 2 do
  TOC.ChildNodes.Add(XNode);
  Channel.ChildNodes.Add(TOC);
  // END TOC

  //TEXTINPUT
  if FSimpleRSS.Channel.Optional.TextInput.Include then begin
    XNode := FSimpleRSS.XMLFile.CreateNode(reTextInput);
    XNode.Attributes[rdfeResource] := FSimpleRSS.Channel.Optional.TextInput.Link;
    Channel.ChildNodes.Add(XNode);

    TextInput := FSimpleRSS.XMLFile.CreateNode(reTextInput);
    TextInput.Attributes[rdfeAbout] := FSimpleRSS.Channel.Optional.TextInput.Link;
    TextInput.ChildNodes.Nodes[reTitle].NodeValue := FSimpleRSS.Channel.Optional.TextInput.Title;
    TextInput.ChildNodes.Nodes[reDescription].NodeValue := FSimpleRSS.Channel.Optional.TextInput.Description;
    TextInput.ChildNodes.Nodes[reName].NodeValue := FSimpleRSS.Channel.Optional.TextInput.TextInputName;
    TextInput.ChildNodes.Nodes[reLink].NodeValue := FSimpleRSS.Channel.Optional.TextInput.Link;
  end; // if then
  // END TEXTINPUT
  // END OPTIONAL
  FSimpleRSS.XMLFile.ChildNodes[rdfeRDF].ChildNodes.Add(Channel);

  if Image <> nil then
    FSimpleRSS.XMLFile.ChildNodes[rdfeRDF].ChildNodes.Add(Image);
  // END CHANNEL
  // ITEMS
  for Counter := 0 to FSimpleRSS.Items.Count - 1 do begin
    if FSimpleRSS.Items.Items[Counter].TitleChanged or FSimpleRSS.Items.Items[Counter].DescriptionChanged then begin
      Item := FSimpleRSS.XMLFile.CreateNode(reItem);
      Item.Attributes[rdfeAbout] := FSimpleRSS.Items.Items[Counter].Link;
      Item.ChildNodes.Nodes[reTitle].NodeValue := FSimpleRSS.Items.Items[Counter].Title;
      Item.ChildNodes.Nodes[reLink].NodeValue := FSimpleRSS.Items.Items[Counter].Link;
      Item.ChildNodes.Nodes[reDescription].NodeValue := FSimpleRSS.Items.Items[Counter].Description;
      FSimpleRSS.XMLFile.ChildNodes[rdfeRDF].ChildNodes.Add(Item);
    end; // if then
  end; // for 2 do
  // END ITEMS

  if TextInput <> nil then
    FSimpleRSS.XMLFile.ChildNodes[rdfeRDF].ChildNodes.Add(TextInput);
end;

procedure TSimpleParserRDF.ParseItem(_ItemNode: IXMLNode);
var
  NewItem: TRSSItem;
  ItemChilds: IXMLNodeList;
  s: string;
  Title: string;
  Description: string;
  SecondCounter: Integer;
  NewItemCategory: TRSSItemCategory;
  ChildNode: IXMLNode;
  HasTitle: boolean;
  HasDescription: boolean;
begin
  NewItem := FSimpleRSS.Items.Add;
  ItemChilds := _ItemNode.ChildNodes;
  HasTitle := GetNodeValue(ItemChilds, reTitle, Title);
  HasDescription := GetNodeValue(ItemChilds, reDescription, Description);
  if not HasDescription and not HasTitle then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reItem
      + strArrow + reTitle);
  if HasTitle then
    NewItem.Title := Title;
  if HasDescription then
    NewItem.Description := Description;
  if GetNodeValue(ItemChilds, reLink, s) then
    NewItem.Link := s;
  if GetNodeValue(ItemChilds, reAuthor, s) then
    NewItem.Author.Name := s;
  if GetNodeValue(ItemChilds, reComments, s) then
    NewItem.Comments := s;
  if GetNodeValue(ItemChilds, rdfeDate, s) then
    NewItem.PubDate.LoadDCDateTime(s);
  if ContainsNode(ItemChilds, reEnclosure, ChildNode) then begin
    if not GetNodeValue(ChildNode.AttributeNodes, reURL, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reEnclosure + strArrow + reURL);
    NewItem.Enclosure.URL := s;
    if not GetNodeValue(ChildNode.AttributeNodes, reLength, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reEnclosure + strArrow + reLength);
    NewItem.Enclosure.Length := StrToInt(s);
    if not GetNodeValue(ChildNode.AttributeNodes, reType, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reEnclosure + strArrow + reType);
    NewItem.Enclosure.EnclosureType := s;
    NewItem.Enclosure.Include := True;
  end; // if then
  if ContainsNode(ItemChilds, reGUID, ChildNode) then begin
    NewItem.GUID.Include := True;
    NewItem.GUID.GUID := GetNodeValue(ChildNode);
    if GetNodeValue(ChildNode.AttributeNodes, reIsPermaLink, s) then
      NewItem.GUID.IsPermaLink := (s = strTrue);
  end; // if then
  if GetNodeValue(ItemChilds, rePubDate, s) then
    NewItem.PubDate.LoadDateTime(s);
  if ContainsNode(ItemChilds, reSource, ChildNode) then begin
    if not GetNodeValue(ChildNode.AttributeNodes, reURL, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reSource + strArrow + reURL);
    NewItem.Source.Include := True;
    NewItem.Source.Title := GetNodeValue(ChildNode);
    NewItem.Source.URL := s;
  end; // if then
  for SecondCounter := 0 to ItemChilds.Count - 1 do begin
    ChildNode := ItemChilds.Nodes[SecondCounter];
    if ChildNode.LocalName = reCategory then begin
      NewItemCategory := NewItem.Categories.Add;
      NewItemCategory.Title := GetNodeValue(ChildNode);
      if GetNodeValue(ChildNode.AttributeNodes, reDomain, s) then
        NewItemCategory.Domain := s;
    end; // if then
  end; // for 2 do
end;

procedure TSimpleParserRDF.ParseImage(_ImageNode: IXMLNode);
var
  ImageChilds: IXMLNodeList;
  Url: string;
  Title: string;
  Link: string;
  Image: TRSSImage;
  s: string;
begin
  ImageChilds := _ImageNode.ChildNodes;
  if not GetNodeValue(ImageChilds, reURL, Url) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reImage + strArrow + reURL);
  if not GetNodeValue(ImageChilds, reTitle, Title) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reImage + strArrow + reTitle);
  if not GetNodeValue(ImageChilds, reLink, Link) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reImage +
      strArrow + reLink);
  Image := FSimpleRSS.Channel.Optional.Image;
  Image.Include := true;
  Image.Required.URL := Url;
  Image.Required.Title := Title;
  Image.Required.Link := Link;
  if GetNodeValue(ImageChilds, reDescription, s) then
    Image.Optional.Description := s;
  if GetNodeValue(ImageChilds, reWidth, s) then
    Image.Optional.Width := StrToInt(s);
  if GetNodeValue(ImageChilds, reHeight, s) then
    Image.Optional.Height := StrToInt(s);
end;

procedure TSimpleParserRDF.ParseTextInput(_TextInputNode: IXMLNode);
var
  Children: IXMLNodeList;
  Title: string;
  Description: string;
  Name: string;
  Link: string;
begin
  Children := _TextInputNode.ChildNodes;
  if not GetNodeValue(Children, reTitle, Title) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
      + strArrow + reTitle);
  if not GetNodeValue(Children, reDescription, Description) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
      + strArrow + reDescription);
  if not GetNodeValue(Children, reName, Name) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
      + strArrow + reName);
  if not GetNodeValue(Children, reLink, Link) then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
      + strArrow + reLink);
  FSimpleRSS.Channel.Optional.TextInput.Include := true;
  FSimpleRSS.Channel.Optional.TextInput.Title := Title;
  FSimpleRSS.Channel.Optional.TextInput.Description := Description;
  FSimpleRSS.Channel.Optional.TextInput.Link := Link;
  FSimpleRSS.Channel.Optional.TextInput.TextInputName := Name;
end;

procedure TSimpleParserRDF.Parse;
var
  MainIdx, SecondCounter: Integer;
  MainChild: IXMLNode;
  MainChildName: string;
  MainChildValue: string;
  MainChildChilds: IXMLNodeList;
  Node: IXMLNode;
  NodeName: string;
  Doc: IXMLNode;
  NewChannelCategory: TRSSChannelCategory;
  s: string;
  Cloud: TRSSCloud;
begin
  Doc := FSimpleRSS.XMLFile.DocumentElement;
  for MainIdx := 0 to Doc.ChildNodes.Count - 1 do begin
    MainChild := Doc.ChildNodes.Nodes[MainIdx];
    MainChildChilds := MainChild.ChildNodes;
    MainChildName := MainChild.NodeName;
    MainChildValue := GetNodeValue(MainChild);
    // begin channel
    if MainChildName = reChannel then begin
      for SecondCounter := 0 to MainChildChilds.Count - 1 do begin
        Node := MainChildChilds[SecondCounter];
        NodeName := Node.NodeName;
        if NodeName = reTitle then
          FSimpleRSS.Channel.Required.Title := GetNodeValue(Node, 'no title');
        if NodeName = reLink then
          FSimpleRSS.Channel.Required.Link := GetNodeValue(Node, 'no link');
        if NodeName = reDescription then
          FSimpleRSS.Channel.Required.Desc := GetNodeValue(Node, 'no description');
      end; // for
    end // end channel
    // begin categorie
    else if MainChildName = reCategory then begin
      NewChannelCategory := FSimpleRSS.Channel.Optional.Categories.Add;
      NewChannelCategory.Category := MainChildValue;
      if GetNodeValue(MainChild.AttributeNodes, reDomain, s) then begin
        NewChannelCategory.Domain := s;
      end; // if then
    end // end categorie
    // begin item
    else if MainChildName = reItem then begin
      ParseItem(MainChild);
    end // END ITEM
    else if MainChildName = reTitle then
      FSimpleRSS.Channel.Required.Title := MainChildValue
    else if MainChildName = reLink then
      FSimpleRSS.Channel.Required.Link := MainChildValue
    else if MainChildName = reDescription then
      FSimpleRSS.Channel.Required.Desc := MainChildValue
    else if MainChildName = reLanguage then begin
      FSimpleRSS.Channel.Optional.Language := StringToLanguage(MainChildValue);
      if FSimpleRSS.Channel.Optional.Language = langX then
        FSimpleRSS.Channel.Optional.Xlang := MainChildValue
      else
        FSimpleRSS.Channel.Optional.Xlang := '';
    end // if then
    else if MainChildName = reCopyright then
      FSimpleRSS.Channel.Optional.Copyright := MainChildValue
    else if MainChildName = reManagingEditor then
      FSimpleRSS.Channel.Optional.ManagingEditor := MainChildValue
    else if MainChildName = reWebMaster then
      FSimpleRSS.Channel.Optional.WebMaster := MainChildValue
    else if MainChildName = rePubDate then
      FSimpleRSS.Channel.Optional.PubDate.LoadDateTime(MainChildValue)
    else if MainChildName = reLastBuildDate then
      FSimpleRSS.Channel.Optional.LastBuildDate.LoadDateTime(MainChildValue)
    else if MainChildName = reGenerator then
      FSimpleRSS.Channel.Optional.Generator := MainChildValue
    else if MainChildName = reDocs then
      FSimpleRSS.Channel.Optional.Docs := MainChildValue
    // BEGIN CLOUD
    else if MainChildName = reCloud then begin
      Cloud := FSimpleRSS.Channel.Optional.Cloud;
      Cloud.Include := MainChild.AttributeNodes.Count > 0;
      if Cloud.Include then begin
        Cloud.Domain := GetNodeValue(MainChild.AttributeNodes, reDomain);
        Cloud.Port := StrToInt(GetNodeValue(MainChild.AttributeNodes, rePort));
        Cloud.Path := GetNodeValue(MainChild.AttributeNodes, rePath);
        Cloud.RegisterProcedure := GetNodeValue(MainChild.AttributeNodes, reRegisterProcedure);
        Cloud.Protocol := GetNodeValue(MainChild.AttributeNodes, reProtocol);
      end; // if then
    end //END CLOUD
    else if MainChildName = reTTL then
      FSimpleRSS.Channel.Optional.TTL := StrToInt(MainChildValue)
    else if MainChildName = reImage then
      ParseImage(MainChild)
    else if MainChildName = reRating then
      FSimpleRSS.Channel.Optional.Rating := MainChildValue
    else if MainChildName = reTextInput then
      ParseTextInput(MainChild)
    else if MainChildName = reSkipHours then
      GetSkipHours(Doc.ChildNodes.Nodes[MainIdx], FSimpleRSS)
    else if MainChildName = reSkipDays then
      GetSkipDays(Doc.ChildNodes.Nodes[MainIdx], FSimpleRss);
  end; // for to do
end;

end.

