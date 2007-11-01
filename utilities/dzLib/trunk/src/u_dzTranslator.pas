{GXFormatter.config=twm}
unit u_dzTranslator;

{$I jedi.inc}

{$IFNDEF NO_TRANSLATION}
// for now uses gnugettext
{$DEFINE gnugettext}
{$ELSE}
{$MESSAGE HINT 'translation is turned off, remove NO_TRANSLATION define to turn it on'}
{$ENDIF}

interface

uses
  SysUtils,
{$IFDEF gnugettext}
  // NOTE: If you don't want any translations, define "NO_TRANSLATION" for your project
  gnugettext, // libs\dxgettext
{$ENDIF}
  Classes;

function _(const _s: string): string;
function GetText(const _s: string): string;
procedure TranslateComponent(_Object: TComponent; const _TextDomain: string = '');
procedure AddDomainForResourceString(const _Domain: string);
procedure SelectTextDomain(const _Domain: string);
procedure TP_GlobalIgnoreClass(_IgnClass: TClass);
procedure TP_GlobalIgnoreClassProperty(_IgnClass: TClass; const _PropertyName: string);
procedure UseLanguage(_LanguageCode: string);

type
  {: use this for translation of special strings that might not be in the same language
     as the program (e.g. a German program generating an English report }
  IdzTranslator = interface ['{FD88CFEE-F2D6-45FB-BBD2-D3C6BE066683}']
    function GetText(const _s: string): string;
  end;

function GenerateTranslator(const _LanguageCode: string): IdzTranslator;

implementation

uses
  Controls,
  ActnList,
  Graphics;

function _(const _s: string): string;
begin
{$IFDEF gnugettext}
  Result := gnugettext._(_s);
{$ELSE}
  Result := _s;
{$ENDIF}
end;

function GetText(const _s: string): string;
begin
  Result := u_dzTranslator._(_s);
end;

procedure TranslateComponent(_Object: TComponent; const _TextDomain: string = '');
begin
{$IFDEF gnugettext}
  gnugettext.TranslateComponent(_Object, _TextDomain);
{$ENDIF}
end;

procedure AddDomainForResourceString(const _Domain: string);
begin
{$IFDEF gnugettext}
  gnugettext.AddDomainForResourceString(_Domain);
{$ENDIF}
end;

procedure SelectTextDomain(const _Domain: string);
begin
{$IFDEF gnugettext}
  gnugettext.textdomain(_Domain);
{$ENDIF}
end;

procedure TP_GlobalIgnoreClass(_IgnClass: TClass);
begin
{$IFDEF gnugettext}
  gnugettext.TP_GlobalIgnoreClass(_IgnClass);
{$ENDIF}
end;

procedure TP_GlobalIgnoreClassProperty(_IgnClass: TClass; const _PropertyName: string);
begin
{$IFDEF gnugettext}
  gnugettext.TP_GlobalIgnoreClassProperty(_IgnClass, _PropertyName);
{$ENDIF}
end;

procedure UseLanguage(_LanguageCode: string);
begin
{$IFDEF gnugettext}
  gnugettext.UseLanguage(_LanguageCode);
{$ENDIF}
end;

type
  TdzTranslator = class(TInterfacedObject, IdzTranslator)
  protected
{$IFDEF gnugettext}
    fGetTextInstance: TGnuGettextInstance;
{$ENDIF}
  protected
    function GetText(const _s: string): string;
  public
    constructor Create(const _LanguageCode: string);
  end;

constructor TdzTranslator.Create(const _LanguageCode: string);
begin
  inherited Create;
{$IFDEF gnugettext}
  fGetTextInstance := TGnuGettextInstance.Create;
  fGetTextInstance.UseLanguage(_LanguageCode);
{$ENDIF}
end;

function TdzTranslator.GetText(const _s: string): string;
begin
{$IFDEF gnugettext}
  Result := fGetTextInstance.gettext(_s);
{$ELSE}
  Result := _s;
{$ENDIF}
end;

function GenerateTranslator(const _LanguageCode: string): IdzTranslator;
begin
  Result := TdzTranslator.Create(_LanguageCode);
end;

{$IFDEF gnugettext}
initialization
  // translate runtime library
  AddDomainForResourceString('delphi');

  // ignore these VCL properties / classes
  TP_GlobalIgnoreClassProperty(TControl, 'ImeName');
  TP_GlobalIgnoreClassProperty(TControl, 'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TAction, 'Category');
  TP_GlobalIgnoreClass(TFont);

{$IFDEF TranslateReporting}
  { TODO -otwm -ccheck : This should not always be called because it links in all ReportBuilder units. }
  // Report-Builder Components / Properties to ignore
  TP_GlobalIgnoreClassProperty(TppField, 'FieldAlias');
  TP_GlobalIgnoreClassProperty(TppField, 'FieldName');
  TP_GlobalIgnoreClassProperty(TppField, 'DisplayFormat');
  TP_GlobalIgnoreClassProperty(TppField, 'SortExpression');
  TP_GlobalIgnoreClassProperty(TppField, 'TableAlias');
  TP_GlobalIgnoreClassProperty(TppField, 'TableName');
  TP_GlobalIgnoreClassProperty(TppBackgroundPrintSettings, 'BinName');
  TP_GlobalIgnoreClassProperty(TppBackgroundPrintSettings, 'PrinterName');
  TP_GlobalIgnoreClassProperty(TppBackgroundPrintSettings, 'PaperName');

  TP_GlobalIgnoreClassProperty(TppChildReport, 'DeviceType');
  TP_GlobalIgnoreClassProperty(TppChildReport, 'Version');

  TP_GlobalIgnoreClassProperty(TPsRBExportComponent, 'Version');
{$ENDIF TranslateReporting}

{$IFDEF DXGETTEXTDEBUG}
  gnugettext.DefaultInstance.DebugLogToFile(ExtractFilePath(GetModuleFilename) + 'dxgettext.log');
{$ENDIF DXGETTEXTDEBUG}

{$ENDIF gnugettext}
end.

