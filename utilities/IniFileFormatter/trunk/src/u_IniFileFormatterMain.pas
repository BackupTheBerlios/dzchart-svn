unit u_IniFileFormatterMain;

interface

uses
  SysUtils,
  u_dzDefaultMain;

type
  TIniFileFormatterMain = class(TDefaultMain)
  protected
    procedure InitCmdLineParser; override;
    function doExecute: integer; override;
  end;

implementation

uses
  Forms,
  u_dzTranslator,
  w_IniFileFormatter;

{ TIniFileFormatterMain }

procedure TIniFileFormatterMain.InitCmdLineParser;
begin
  inherited;
  FGetOpt.RegisterOption('SortSections', _(
    'Sort sections. Possible values: no, alpha, template'), true);
  FGetOpt.RegisterOption('SortItems', _(
    'Sort items. Possible values: no, alpha, template'), true);
  FGetOpt.RegisterOption('template', _('Template file for sorting by template')
    , true);
  FGetOpt.RegisterParam('SrcFile', _('Source INI file to read and format'), 0, 1);
  FGetOpt.RegisterParam('DestFile', _('Destination INI file to write, if given, the program exits automatically'), 0, 1);
end;

function TIniFileFormatterMain.doExecute: integer;
var
  SrcFile: string;
  frm: Tf_IniFileFormatter;
  SortSections: string;
  SortItems: string;
  Template: string;
  DestFile: string;
begin
  Application.CreateForm(Tf_IniFileFormatter, frm);

  if FGetOpt.ParamPassed('SrcFile', SrcFile) then
    frm.SetSrcFile(SrcFile);

  if FGetOpt.OptionPassed('template', Template) then begin
    frm.SetTemplate(Template);
  end;

  SortSections := '';
  if FGetOpt.OptionPassed('SortSections', SortSections) then begin
    if SameText(SortSections, 'no') then
      frm.SetSortSections(0)
    else if SameText(SortSections, 'template') then
      frm.SetSortSections(2)
    else if not SameText(SortSections, 'alpha') then
      raise Exception.CreateFmt(_('Invalid value "%s" for option SortSections, must be one of "no", "alpha" or "template"'), [SortSections]);
  end;

  SortItems := '';
  if FGetOpt.OptionPassed('SortItems', SortItems) then begin
    if SameText(SortItems, 'no') then
      frm.SetSortItems(0)
    else if SameText(SortItems, 'template') then
      frm.SetSortItems(2)
    else if not SameText(SortItems, 'alpha') then
      raise Exception.CreateFmt(_('Invalid value "%s" for option SortSections, must be one of "no", "alpha" or "template"'), [SortItems]);
  end;

  if FGetOpt.ParamPassed('DestFile', DestFile) then begin
    frm.Execute(DestFile);
  end else begin
    Application.Run;
  end;
  Result := 0;
end;

end.

