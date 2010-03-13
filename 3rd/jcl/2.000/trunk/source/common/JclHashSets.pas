{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is HashSet.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date: 2006-07-24 07:34:39 +0200 (lun., 24 juil. 2006) $

unit JclHashSets;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JclBase, JclAbstractContainers, JclContainerIntf, JclHashMaps;

type
  TJclIntfHashSet = class(TJclAbstractContainer, IJclIntfCollection,
    IJclIntfSet, IJclIntfCloneable)
  private
    FMap: IJclIntfIntfMap;
  protected
    { IJclIntfCollection }
    function Add(AInterface: IInterface): Boolean;
    function AddAll(ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(AInterface: IInterface): Boolean;
    function ContainsAll(ACollection: IJclIntfCollection): Boolean;
    function Equals(ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(AInterface: IInterface): Boolean;
    function RemoveAll(ACollection: IJclIntfCollection): Boolean;
    function RetainAll(ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    { IJclIntfSet }
    procedure Intersect(ACollection: IJclIntfCollection);
    procedure Subtract(ACollection: IJclIntfCollection);
    procedure Union(ACollection: IJclIntfCollection);
    { IJclIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
  end;

  //Daniele Teti 02/03/2005
  TJclStrHashSet = class(TJclStrCollection, IJclStrSet, IJclCloneable)
  private
    FMap: IJclStrMap;
  protected
    { IJclStrCollection }
    function Add(const AString: string): Boolean; override;
    function AddAll(ACollection: IJclStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: string): Boolean; override;
    function ContainsAll(ACollection: IJclStrCollection): Boolean; override;
    function Equals(ACollection: IJclStrCollection): Boolean; override;
    function First: IJclStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclStrIterator; override;
    function Remove(const AString: string): Boolean; override;
    function RemoveAll(ACollection: IJclStrCollection): Boolean; override;
    function RetainAll(ACollection: IJclStrCollection): Boolean; override;
    function Size: Integer; override;
    { IJclIntfSet }
    procedure Intersect(ACollection: IJclStrCollection);
    procedure Subtract(ACollection: IJclStrCollection);
    procedure Union(ACollection: IJclStrCollection);
    { IJclIntfCloneable }
    function Clone: TObject;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
  end;

  TJclHashSet = class(TJclAbstractContainer, IJclCollection, IJclSet, IJclCloneable)
  private
    FMap: IJclMap;
  protected
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: IJclCollection): Boolean;
    function Equals(ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(ACollection: IJclCollection): Boolean;
    function RetainAll(ACollection: IJclCollection): Boolean;
    function Size: Integer;
    { IJclSet }
    procedure Intersect(ACollection: IJclCollection);
    procedure Subtract(ACollection: IJclCollection);
    procedure Union(ACollection: IJclCollection);
    { IJclCloneable }
    function Clone: TObject;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity;
      AOwnsObject: Boolean = False);
    destructor Destroy; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net/svnroot/jcl/tags/JCL199-Build2551/jcl/source/common/JclHashSets.pas $';
    Revision: '$Revision: 1694 $';
    Date: '$Date: 2006-07-24 07:34:39 +0200 (lun., 24 juil. 2006) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF CLR}
var
  GlobalRefUnique: TObject = nil;

function RefUnique: TObject;
begin
  // We keep the reference till program end. A unique memory address is not
  // possible under a garbage collector.
  if GlobalRefUnique = nil then
    GlobalRefUnique := TObject.Create;
  Result := GlobalRefUnique;
end;
{$ELSE}
var
  // Here we have TObject reference that points to the data segment. A memory
  // manager cannot return this address.
  RefUnique: TObject {$IFNDEF FPC} = @RefUnique {$ENDIF};
{$ENDIF CLR}

var
  IRefUnique: IInterface = nil;

//=== { TJclIntfHashSet } ====================================================

constructor TJclIntfHashSet.Create(ACapacity: Integer = DefaultContainerCapacity);
begin
  inherited Create;
  FMap := TJclIntfIntfHashMap.Create(ACapacity);
  if IRefUnique = nil then
    IRefUnique := TInterfacedObject.Create;
end;

destructor TJclIntfHashSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfHashSet.Add(AInterface: IInterface): Boolean;
begin
  Result := not FMap.ContainsKey(AInterface);
  if Result then
    FMap.PutValue(AInterface, IRefUnique);
end;

function TJclIntfHashSet.AddAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  Result := ACollection <> nil;
  if Result then
  begin
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  end;
end;

procedure TJclIntfHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclIntfHashSet.Clone: IInterface;
var
  NewSet: TJclIntfHashSet;
begin
  NewSet := TJclIntfHashSet.Create;
  NewSet.FMap := IJclIntfIntfMap(IJclIntfCloneable(FMap).Clone);
  Result := NewSet;
end;

function TJclIntfHashSet.Contains(AInterface: IInterface): Boolean;
begin
  Result := FMap.ContainsKey(AInterface);
end;

function TJclIntfHashSet.ContainsAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclIntfHashSet.Equals(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  ItMap: IJclIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclIntfHashSet.First: IJclIntfIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TJclIntfHashSet.Intersect(ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntfHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclIntfHashSet.Last: IJclIntfIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TJclIntfHashSet.Remove(AInterface: IInterface): Boolean;
begin
  Result := FMap.Remove(AInterface) = IInterface(IRefUnique);
end;

function TJclIntfHashSet.RemoveAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclIntfHashSet.RetainAll(ACollection: IJclIntfCollection): Boolean;
var
  ItMap: IJclIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if not ACollection.Contains(ItMap.Next) then
      ItMap.Remove;
end;

function TJclIntfHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclIntfHashSet.Subtract(ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfHashSet.Union(ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclStrHashSet } =====================================================

constructor TJclStrHashSet.Create(ACapacity: Integer = DefaultContainerCapacity);
begin
  inherited Create;
  FMap := TJclStrHashMap.Create(ACapacity, False);
end;

destructor TJclStrHashSet.Destroy;
begin
  Clear;
  // (rom) no Free of FMap?
  inherited Destroy;
end;

function TJclStrHashSet.Add(const AString: string): Boolean;
begin
  Result := not FMap.ContainsKey(AString);
  if Result then
    FMap.PutValue(AString, RefUnique);
end;

function TJclStrHashSet.AddAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

procedure TJclStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclStrHashSet.Clone: TObject;
var
  NewSet: TJclStrHashSet;
begin
  NewSet := TJclStrHashSet.Create;
  NewSet.FMap := TJclStrHashMap(IJclCloneable(FMap).Clone);
  Result := NewSet;
end;

function TJclStrHashSet.Contains(const AString: string): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclStrHashSet.ContainsAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclStrHashSet.Equals(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  ItMap: IJclStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.KeySet.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclStrHashSet.First: IJclStrIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TJclStrHashSet.Intersect(ACollection: IJclStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclStrHashSet.Last: IJclStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TJclStrHashSet.Remove(const AString: string): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclStrHashSet.RemoveAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclStrHashSet.RetainAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not FMap.ContainsKey(It.Next) then
      FMap.Remove(It.Next);
end;

function TJclStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclStrHashSet.Subtract(ACollection: IJclStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclStrHashSet.Union(ACollection: IJclStrCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclHashSet } ========================================================

constructor TJclHashSet.Create(ACapacity: Integer = DefaultContainerCapacity;
  AOwnsObject: Boolean = False);
begin
  inherited Create;
  FMap := TJclHashMap.Create(ACapacity, AOwnsObject);
end;

destructor TJclHashSet.Destroy;
begin
  Clear;
  // (rom) no Free of FMap?
  inherited Destroy;
end;

function TJclHashSet.Add(AObject: TObject): Boolean;
begin
  Result := not FMap.ContainsKey(AObject);
  if Result then
    FMap.PutValue(AObject, RefUnique);
end;

function TJclHashSet.AddAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

procedure TJclHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclHashSet.Clone: TObject;
var
  NewSet: TJclHashSet;
begin
  NewSet := TJclHashSet.Create;
  NewSet.FMap := TJclHashMap(IJclCloneable(FMap).Clone);
  Result := NewSet;
end;

function TJclHashSet.Contains(AObject: TObject): Boolean;
begin
  Result := FMap.ContainsKey(AObject);
end;

function TJclHashSet.ContainsAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclHashSet.Equals(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  ItMap: IJclIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclHashSet.First: IJclIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TJclHashSet.Intersect(ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

function TJclHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclHashSet.Last: IJclIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TJclHashSet.Remove(AObject: TObject): Boolean;
begin
  Result := FMap.Remove(AObject) = RefUnique;
end;

function TJclHashSet.RemoveAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclHashSet.RetainAll(ACollection: IJclCollection): Boolean;
var
  ItMap: IJclIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if not ACollection.Contains(ItMap.Next) then
      ItMap.Remove;
end;

function TJclHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclHashSet.Subtract(ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet.Union(ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;

{$IFDEF FPC}
initialization
  RefUnique := @RefUnique;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
{$ELSE}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}
{$ENDIF FPC}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}


end.

