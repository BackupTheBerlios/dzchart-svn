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
  HasTitle := TryGetChildnodeValue(ItemChilds, reTitle, Title);
  HasDescription := TryGetChildnodeValue(ItemChilds, reDescription, Description);
  if not HasDescription and not HasTitle then
    raise ESimpleRSSException.Create(emRequiredFieldMissing + reItem
      + strArrow + reTitle);
  if HasTitle then
    NewItem.Title := Title;
  if HasDescription then
    NewItem.Description := Description;
  if TryGetChildnodeValue(ItemChilds, reLink, s) then
    NewItem.Link := s;
  if TryGetChildnodeValue(ItemChilds, reAuthor, s) then
    NewItem.Author.Name := s;
  if TryGetChildnodeValue(ItemChilds, reComments, s) then
    NewItem.Comments := s;
  if TryGetChildnodeValue(ItemChilds, rdfeDate, s) then
    NewItem.PubDate.LoadDCDateTime(s);
  if ContainsNode(ItemChilds, reEnclosure, ChildNode) then begin
    if not TryGetAttributeValue(ChildNode.AttributeNodes, reURL, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reEnclosure + strArrow + reURL);
    NewItem.Enclosure.URL := s;
    if not TryGetAttributeValue(ChildNode.AttributeNodes, reLength, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reEnclosure + strArrow + reLength);
    NewItem.Enclosure.Length := StrToInt(s);
    if not TryGetAttributeValue(ChildNode.AttributeNodes, reType, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reEnclosure + strArrow + reType);
    NewItem.Enclosure.EnclosureType := s;
    NewItem.Enclosure.Include := True;
  end; // if then
  if ContainsNode(ItemChilds, reGUID, ChildNode) then begin
    NewItem.GUID.Include := True;
    NewItem.GUID.GUID := GetNodeValue(ChildNode);
    if TryGetAttributeValue(ChildNode.AttributeNodes, reIsPermaLink, s) then
      NewItem.GUID.IsPermaLink := (s = strTrue);
  end; // if then
  if TryGetChildnodeValue(ItemChilds, rePubDate, s) then
    NewItem.PubDate.LoadDateTime(s);
  if ContainsNode(ItemChilds, reSource, ChildNode) then begin
    if not TryGetAttributeValue(ChildNode.AttributeNodes, reURL, s) then
      raise ESimpleRSSException.Create(emRequiredFieldMissing + reSource + strArrow + reURL);
    NewItem.Source.Include := True;
    NewItem.Source.Title := GetNodeValue(ChildNode);
    NewItem.Source.URL := s;
  end; // if then
  for SecondCounter := 0 to ItemChilds.Count - 1 do begin
    if ItemChilds.Nodes[SecondCounter].LocalName = reCategory then begin
      NewItemCategory := NewItem.Categories.Add;
      NewItemCategory.Title := ItemChilds.Nodes[SecondCounter].NodeValue;
      if TryGetAttributeValue(ItemChilds.Nodes[SecondCounter].AttributeNodes, reDomain, s) then
        NewItemCategory.Domain := s;
    end; // if then
  end; // for 2 do
end;

procedure TSimpleParserRDF.Parse;
var
  MainIdx, SecondCounter: Integer;
  MainChild: IXMLNode;
  MainChildName: string;
  MainChildChilds: IXMLNodeList;
  Node: IXMLNode;
  NodeName: string;
  Doc: IXMLNode;
  NewChannelCategory: TRSSChannelCategory;
  s: string;
begin
  Doc := FSimpleRSS.XMLFile.DocumentElement;
  for MainIdx := 0 to Doc.ChildNodes.Count - 1 do begin
    MainChild := Doc.ChildNodes.Nodes[MainIdx];
    MainChildChilds := MainChild.ChildNodes;
    MainChildName := MainChild.NodeName;
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
      NewChannelCategory.Category := MainChild.NodeValue;
      if MainChild.AttributeNodes.FindNode(reDomain) <> nil then begin
        NewChannelCategory.Domain := MainChild.AttributeNodes.Nodes[reDomain].NodeValue;
      end; // if then
    end // end categorie
    // begin item
    else if MainChildName = reItem then begin
      ParseItem(MainChild);
    end // END ITEM
    else if MainChildName = reTitle then
      FSimpleRSS.Channel.Required.Title := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = reLink then
      FSimpleRSS.Channel.Required.Link := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = reDescription then
      FSimpleRSS.Channel.Required.Desc := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = reLanguage then begin
      FSimpleRSS.Channel.Optional.Language := StringToLanguage(Doc.ChildNodes.Nodes[MainIdx].NodeValue);
      if FSimpleRSS.Channel.Optional.Language = langX then
        FSimpleRSS.Channel.Optional.Xlang := Doc.ChildNodes.Nodes[MainIdx].NodeValue
      else
        FSimpleRSS.Channel.Optional.Xlang := '';
    end // if then
    else if MainChildName = reCopyright then
      FSimpleRSS.Channel.Optional.Copyright := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = reManagingEditor then
      FSimpleRSS.Channel.Optional.ManagingEditor := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = reWebMaster then
      FSimpleRSS.Channel.Optional.WebMaster := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = rePubDate then
      FSimpleRSS.Channel.Optional.PubDate.LoadDateTime(Doc.ChildNodes.Nodes[MainIdx].NodeValue)
    else if MainChildName = reLastBuildDate then
      FSimpleRSS.Channel.Optional.LastBuildDate.LoadDateTime(Doc.ChildNodes.Nodes[MainIdx].NodeValue)
    else if MainChildName = reGenerator then
      FSimpleRSS.Channel.Optional.Generator := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    else if MainChildName = reDocs then
      FSimpleRSS.Channel.Optional.Docs := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    // BEGIN CLOUD
    else if MainChildName = reCloud then begin
      FSimpleRSS.Channel.Optional.Cloud.Include := Doc.ChildNodes.Nodes[MainIdx].AttributeNodes.Count > 0;
      if FSimpleRSS.Channel.Optional.Cloud.Include then begin
        FSimpleRSS.Channel.Optional.Cloud.Domain := Doc.ChildNodes.Nodes[MainIdx].AttributeNodes.Nodes[reDomain].NodeValue;
        FSimpleRSS.Channel.Optional.Cloud.Port := Doc.ChildNodes.Nodes[MainIdx].AttributeNodes.Nodes[rePort].NodeValue;
        FSimpleRSS.Channel.Optional.Cloud.Path := Doc.ChildNodes.Nodes[MainIdx].AttributeNodes.Nodes[rePath].NodeValue;
        FSimpleRSS.Channel.Optional.Cloud.RegisterProcedure := Doc.ChildNodes.Nodes[MainIdx].AttributeNodes.Nodes[reRegisterProcedure].NodeValue;
        FSimpleRSS.Channel.Optional.Cloud.Protocol := Doc.ChildNodes.Nodes[MainIdx].AttributeNodes.Nodes[reProtocol].NodeValue;
      end; // if then
    end //END CLOUD
    else if MainChildName = reTTL then
      FSimpleRSS.Channel.Optional.TTL := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    //BEGIN IMAGE
    else if MainChildName = reImage then begin
      if MainChildChilds.FindNode(reURL) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reImage +
          strArrow + reURL);
      if MainChildChilds.FindNode(reTitle) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reImage +
          strArrow + reTitle);
      if MainChildChilds.FindNode(reLink) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reImage +
          strArrow + reLink);
      FSimpleRSS.Channel.Optional.Image.Include :=
        MainChildChilds.Count > 0;
      if FSimpleRSS.Channel.Optional.Image.Include then begin
        FSimpleRSS.Channel.Optional.Image.Required.URL :=
          MainChildChilds.FindNode(reURL).NodeValue;
        FSimpleRSS.Channel.Optional.Image.Required.Title :=
          MainChildChilds.FindNode(reTitle).NodeValue;
        FSimpleRSS.Channel.Optional.Image.Required.Link :=
          MainChildChilds.FindNode(reLink).NodeValue;
        if MainChildChilds.FindNode(reDescription) <>
          nil then
          FSimpleRSS.Channel.Optional.Image.Optional.Description :=
            MainChildChilds.FindNode(reDescription).NodeValue;
        if MainChildChilds.FindNode(reWidth) <> nil then
          FSimpleRSS.Channel.Optional.Image.Optional.Width :=
            MainChildChilds.FindNode(reWidth).NodeValue;
        if MainChildChilds.FindNode(reHeight) <> nil then
          FSimpleRSS.Channel.Optional.Image.Optional.Height :=
            MainChildChilds.FindNode(reHeight).NodeValue;
      end; // if then
    end // END IMAGE
    else if MainChildName = reRating then
      FSimpleRSS.Channel.Optional.Rating := Doc.ChildNodes.Nodes[MainIdx].NodeValue
    //BEGIN TEXTINPUT
    else if MainChildName = reTextInput then begin
      if MainChildChilds.FindNode(reTitle) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
          + strArrow + reTitle);
      if MainChildChilds.FindNode(reDescription) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
          + strArrow + reDescription);
      if MainChildChilds.FindNode(reName) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
          + strArrow + reName);
      if MainChildChilds.FindNode(reLink) = nil then
        raise ESimpleRSSException.Create(emRequiredFieldMissing + reTextInput
          + strArrow + reLink);
      FSimpleRSS.Channel.Optional.TextInput.Include :=
        MainChildChilds.Count > 0;
      if FSimpleRSS.Channel.Optional.TextInput.Include then begin
        FSimpleRSS.Channel.Optional.TextInput.Title :=
          MainChildChilds.FindNode(reTitle).NodeValue;
        FSimpleRSS.Channel.Optional.TextInput.Description :=
          MainChildChilds.FindNode(reDescription).NodeValue;
        FSimpleRSS.Channel.Optional.TextInput.Link :=
          MainChildChilds.FindNode(reLink).NodeValue;
        FSimpleRSS.Channel.Optional.TextInput.TextInputName :=
          MainChildChilds.FindNode(reName).NodeValue;
      end; // if then
    end // END TEXTINPUT
    //BEGIN SKIPHOURS
    else if MainChildName = reSkipHours then begin
      GetSkipHours(Doc.ChildNodes.Nodes[MainIdx], FSimpleRSS);
    end // END SKIPHOURS
    //BEGIN SKIPDAYS
    else if MainChildName = reSkipDays then begin
      GetSkipDays(Doc.ChildNodes.Nodes[MainIdx], FSimpleRss);
    end; //END SKIPDAYS
  end; // for to do
end;

end.

