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
{ The Original Code is DCL_intf.pas.                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date: 2006-07-24 07:34:39 +0200 (lun., 24 juil. 2006) $

unit JclContainerIntf;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JclBase;

const
  DefaultContainerCapacity = 16;

type
  IJclIntfCloneable = interface
    ['{BCF77740-FB60-4306-9BD1-448AADE5FF4E}']
    function Clone: IInterface;
  end;

  IJclCloneable = interface
    ['{D224AE70-2C93-4998-9479-1D513D75F2B2}']
    function Clone: TObject;
  end;

  IJclIntfIterator = interface
    ['{E121A98A-7C43-4587-806B-9189E8B2F106}']
    procedure Add(AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AInterface: IInterface);
  end;

  IJclStrIterator = interface
    ['{D5D4B681-F902-49C7-B9E1-73007C9D64F0}']
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: string;
    function NextIndex: Integer;
    function Previous: string;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  end;

  IJclIterator = interface
    ['{997DF9B7-9AA2-4239-8B94-14DFFD26D790}']
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  end;

  IJclIntfCollection = interface
    ['{8E178463-4575-487A-B4D5-DC2AED3C7ACA}']
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
  end;

  IJclStrCollection = interface
    ['{3E3CFC19-E8AF-4DD7-91FA-2DF2895FC7B9}']
    function Add(const AString: string): Boolean;
    function AddAll(ACollection: IJclStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(ACollection: IJclStrCollection): Boolean;
    function Equals(ACollection: IJclStrCollection): Boolean;
    function First: IJclStrIterator;
    function IsEmpty: Boolean;
    function Last: IJclStrIterator;
    function Remove(const AString: string): Boolean;
    function RemoveAll(ACollection: IJclStrCollection): Boolean;
    function RetainAll(ACollection: IJclStrCollection): Boolean;
    function Size: Integer;
    //Daniele Teti 27/12/2004
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(const Separator: string = AnsiLineBreak): string;
    procedure AppendDelimited(const AString: string; const Separator: string = AnsiLineBreak);
    procedure LoadDelimited(const AString: string; const Separator: string = AnsiLineBreak);
  end;

  IJclCollection = interface
    ['{58947EF1-CD21-4DD1-AE3D-225C3AAD7EE5}']
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
  end;

  IJclIntfList = interface(IJclIntfCollection)
    ['{E14EDA4B-1DAA-4013-9E6C-CDCB365C7CF9}']
    procedure Insert(Index: Integer; AInterface: IInterface); overload;
    function InsertAll(Index: Integer; ACollection: IJclIntfCollection): Boolean; overload;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(AInterface: IInterface): Integer;
    function LastIndexOf(AInterface: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
  end;

  IJclStrList = interface(IJclStrCollection)
    ['{07DD7644-EAC6-4059-99FC-BEB7FBB73186}']
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; ACollection: IJclStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IJclStrList;
    //Daniele Teti
    property Items[Key: Integer]: string read GetString write SetString; default;
  end;

  IJclList = interface(IJclCollection)
    ['{8ABC70AC-5C06-43EA-AFE0-D066379BCC28}']
    procedure Insert(Index: Integer; AObject: TObject); overload;
    function InsertAll(Index: Integer; ACollection: IJclCollection): Boolean; overload;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    //Daniele Teti
    property Items[Key: Integer]: TObject read GetObject write SetObject; default;
  end;

  IJclIntfArray = interface(IJclIntfList)
    ['{B055B427-7817-43FC-97D4-AD1845643D63}']
    {$IFDEF CLR}
    function GetObject(Index: Integer): IInterface;
    procedure SetObject(Index: Integer; AInterface: IInterface);
    {$ENDIF CLR}
    property Items[Index: Integer]: IInterface read GetObject write SetObject; default;
  end;

  IJclStrArray = interface(IJclStrList)
    ['{B055B427-7817-43FC-97D4-AD1845643D63}']
    {$IFDEF CLR}
    function GetString(Index: Integer): string;
    procedure SetString(Index: Integer; const AString: string);
    {$ENDIF CLR}
    property Items[Index: Integer]: string read GetString write SetString; default;
  end;

  IJclArray = interface(IJclList)
    ['{A69F6D35-54B2-4361-852E-097ED75E648A}']
    {$IFDEF CLR}
    function GetObject(Index: Integer): TObject;
    procedure SetObject(Index: Integer; AObject: TObject);
    {$ENDIF CLR}
    property Items[Index: Integer]: TObject read GetObject write SetObject; default;
  end;

  IJclIntfSet = interface(IJclIntfCollection)
    ['{E2D28852-9774-49B7-A739-5DBA2B705924}']
    procedure Intersect(ACollection: IJclIntfCollection);
    procedure Subtract(ACollection: IJclIntfCollection);
    procedure Union(ACollection: IJclIntfCollection);
  end;

  IJclStrSet = interface(IJclStrCollection)
    ['{72204D85-2B68-4914-B9F2-09E5180C12E9}']
    procedure Intersect(ACollection: IJclStrCollection);
    procedure Subtract(ACollection: IJclStrCollection);
    procedure Union(ACollection: IJclStrCollection);
  end;

  IJclSet = interface(IJclCollection)
    ['{0B7CDB90-8588-4260-A54C-D87101C669EA}']
    procedure Intersect(ACollection: IJclCollection);
    procedure Subtract(ACollection: IJclCollection);
    procedure Union(ACollection: IJclCollection);
  end;

  TJclTraverseOrder = (toPreOrder, toOrder, toPostOrder);

  IJclIntfTree = interface(IJclIntfCollection)
    ['{5A21688F-113D-41B4-A17C-54BDB0BD6559}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclStrTree = interface(IJclStrCollection)
    ['{1E1896C0-0497-47DF-83AF-A9422084636C}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclTree = interface(IJclCollection)
    ['{B0C658CC-FEF5-4178-A4C5-442C0DEDE207}']
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  IJclIntfIntfMap = interface
    ['{01D05399-4A05-4F3E-92F4-0C236BE77019}']
    procedure Clear;
    function ContainsKey(Key: IInterface): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IJclIntfIntfMap): Boolean;
    function GetValue(Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclIntfSet;
    procedure PutAll(AMap: IJclIntfIntfMap);
    procedure PutValue(Key, Value: IInterface);
    function Remove(Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  IJclMultiIntfIntfMap = interface(IJclIntfIntfMap)
    ['{497775A5-D3F1-49FC-A641-15CC9E77F3D0}']
    function GetValues(Key: IInterface): IJclIntfIterator;
    function Count(Key: IInterface): Integer;
  end;

  IJclStrIntfMap = interface
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IJclStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(AMap: IJclStrIntfMap);
    procedure PutValue(const Key: string; Value: IInterface);
    function Remove(const Key: string): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  end;

  IJclStrStrMap = interface
    ['{A4788A96-281A-4924-AA24-03776DDAAD8A}']
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(AMap: IJclStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(AMap: IJclStrStrMap);
    procedure PutValue(const Key, Value: string);
    function Remove(const Key: string): string;
    function Size: Integer;
    function Values: IJclStrCollection;
    //Daniele Teti
    function KeyOfValue(const Value: string): string;
    //Daniele Teti
    property Items[const Key: string]: string read GetValue write PutValue; default;
  end;

  IJclStrMap = interface
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IJclStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(AMap: IJclStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[const Key: string]: TObject read GetValue write PutValue; default;
  end;

  IJclMap = interface
    ['{A7D0A882-6952-496D-A258-23D47DDCCBC4}']
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclSet;
    procedure PutAll(AMap: IJclMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    //Daniele Teti
    property Items[Key: TObject]: TObject read GetValue write PutValue; default;
  end;

  IJclIntfQueue = interface
    ['{B88756FE-5553-4106-957E-3E33120BFA99}']
    function Contains(AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    procedure Enqueue(AInterface: IInterface);
    function Size: Integer;
  end;

  IJclStrQueue = interface
    ['{5BA0ED9A-5AF3-4F79-9D80-34FA7FF15D1F}']
    function Contains(const AString: string): Boolean;
    function Dequeue: string;
    function Empty: Boolean;
    procedure Enqueue(const AString: string);
    function Size: Integer;
  end;

  IJclQueue = interface
    ['{7D0F9DE4-71EA-46EF-B879-88BCFD5D9610}']
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    procedure Enqueue(AObject: TObject);
    function Size: Integer;
  end;

  IJclStrStrSortedMap = interface(IJclStrStrMap)
    ['{4F457799-5D03-413D-A46C-067DC4200CC3}']
    function FirstKey: string;
    function HeadMap(ToKey: string): IJclStrStrSortedMap;
    function LastKey: string;
    function SubMap(FromKey, ToKey: string): IJclStrStrSortedMap;
    function TailMap(FromKey: string): IJclStrStrSortedMap;
  end;

  IJclSortedMap = interface(IJclMap)
    ['{F317A70F-7851-49C2-9DCF-092D8F4D4F98}']
    function FirstKey: TObject;
    function HeadMap(ToKey: TObject): IJclSortedMap;
    function LastKey: TObject;
    function SubMap(FromKey, ToKey: TObject): IJclSortedMap;
    function TailMap(FromKey: TObject): IJclSortedMap;
  end;

  IJclIntfSortedSet = interface(IJclIntfSet)
    ['{159BE5A7-7349-42FF-BE55-9CA1B9DBA991}']
    function HeadSet(AEndObject: IInterface): IJclIntfSortedSet;
    function SubSet(Start, Finish: IInterface): IJclIntfSortedSet;
    function TailSet(AStartObject: IInterface): IJclIntfSortedSet;
  end;

  IJclSortedSet = interface(IJclSet)
    ['{A3D23E76-ADE9-446C-9B97-F49FCE895D9F}']
    function HeadSet(AEndObject: TObject): IJclSortedSet;
    function SubSet(Start, Finish: TObject): IJclSortedSet;
    function TailSet(AStartObject: TObject): IJclSortedSet;
  end;

  IJclIntfStack = interface
    ['{CA1DC7A1-8D8F-4A5D-81D1-0FE32E9A4E84}']
    function Contains(AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Pop: IInterface;
    procedure Push(AInterface: IInterface);
    function Size: Integer;
  end;

  IJclStrStack = interface
    ['{649BB74C-D7BE-40D9-9F4E-32DDC3F13F3B}']
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Pop: string;
    procedure Push(const AString: string);
    function Size: Integer;
  end;

  IJclStack = interface
    ['{E07E0BD8-A831-41B9-B9A0-7199BD4873B9}']
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Pop: TObject;
    procedure Push(AObject: TObject);
    function Size: Integer;
  end;

  // Exceptions
  EJclOutOfBoundsError = class(EJclError);
  EJclNoSuchElementError = class(EJclError);
  EJclIllegalStateError = class(EJclError);
  EJclConcurrentModificationError = class(EJclError);
  EJclIllegalArgumentError = class(EJclError);
  EJclOperationNotSupportedError = class(EJclError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net/svnroot/jcl/tags/JCL199-Build2551/jcl/source/common/JclContainerIntf.pas $';
    Revision: '$Revision: 1694 $';
    Date: '$Date: 2006-07-24 07:34:39 +0200 (lun., 24 juil. 2006) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

