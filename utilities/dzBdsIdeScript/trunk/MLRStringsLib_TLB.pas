unit MLRStringsLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88  $
// File generated on 9/14/2000 4:36:51 PM from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Projs\IDEScript\StrLib\MLRStringsLib.tlb (1)
// IID\LCID: {7DE693CD-DC18-434C-8D46-AE7039F819C9}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  MLRStringsLibMajorVersion = 1;
  MLRStringsLibMinorVersion = 0;

  LIBID_MLRStringsLib: TGUID = '{7DE693CD-DC18-434C-8D46-AE7039F819C9}';

  IID_IMLRAutoStrings: TGUID = '{2BA0C29F-263A-4370-B3FE-2BD01829ECB8}';
  CLASS_MLRAutoStrings: TGUID = '{A3933C7B-C847-4A13-AD54-C3D121E6A319}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IMLRAutoStrings = interface;
  IMLRAutoStringsDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  MLRAutoStrings = IMLRAutoStrings;


// *********************************************************************//
// Interface: IMLRAutoStrings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2BA0C29F-263A-4370-B3FE-2BD01829ECB8}
// *********************************************************************//
  IMLRAutoStrings = interface(IDispatch)
    ['{2BA0C29F-263A-4370-B3FE-2BD01829ECB8}']
    function  Get_Count: Integer; safecall;
    function  Get_Text: WideString; safecall;
    procedure Set_Text(const Value: WideString); safecall;
    function  Add(const S: WideString): Integer; safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    procedure Exchange(Index1: Integer; Index2: Integer); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    function  Get_Modified: WordBool; safecall;
    procedure Set_Modified(Value: WordBool); safecall;
    function  Get_Strings(Index: Integer): WideString; safecall;
    procedure Set_Strings(Index: Integer; const Value: WideString); safecall;
    property Count: Integer read Get_Count;
    property Text: WideString read Get_Text write Set_Text;
    property Modified: WordBool read Get_Modified write Set_Modified;
    property Strings[Index: Integer]: WideString read Get_Strings write Set_Strings;
  end;

// *********************************************************************//
// DispIntf:  IMLRAutoStringsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2BA0C29F-263A-4370-B3FE-2BD01829ECB8}
// *********************************************************************//
  IMLRAutoStringsDisp = dispinterface
    ['{2BA0C29F-263A-4370-B3FE-2BD01829ECB8}']
    property Count: Integer readonly dispid 1;
    property Text: WideString dispid 2;
    function  Add(const S: WideString): Integer; dispid 4;
    procedure Clear; dispid 5;
    procedure Delete(Index: Integer); dispid 6;
    procedure Exchange(Index1: Integer; Index2: Integer); dispid 7;
    procedure LoadFromFile(const FileName: WideString); dispid 8;
    procedure SaveToFile(const FileName: WideString); dispid 9;
    property Modified: WordBool dispid 10;
    property Strings[Index: Integer]: WideString dispid 11;
  end;

// *********************************************************************//
// The Class CoMLRAutoStrings provides a Create and CreateRemote method to          
// create instances of the default interface IMLRAutoStrings exposed by              
// the CoClass MLRAutoStrings. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoMLRAutoStrings = class
    class function Create: IMLRAutoStrings;
    class function CreateRemote(const MachineName: string): IMLRAutoStrings;
  end;

implementation

uses ComObj;

class function CoMLRAutoStrings.Create: IMLRAutoStrings;
begin
  Result := CreateComObject(CLASS_MLRAutoStrings) as IMLRAutoStrings;
end;

class function CoMLRAutoStrings.CreateRemote(const MachineName: string): IMLRAutoStrings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_MLRAutoStrings) as IMLRAutoStrings;
end;

end.
