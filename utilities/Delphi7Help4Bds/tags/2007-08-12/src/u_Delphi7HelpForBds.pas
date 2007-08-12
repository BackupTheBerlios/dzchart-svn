unit u_Delphi7HelpForBds;

interface

procedure Register;

implementation

uses
  SysUtils,
  StrUtils,
  Windows,
  Classes,
  Forms,
  ShellApi,
  ToolsAPI,
  Menus,
  Controls,
  Registry,
  w_Delphi7HelpForBds;

type
  TCtrlF1Help = class(TNotifierObject, IOTAKeyboardBinding)
  private
    procedure CallCtrlF1(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure CallShiftF1(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    procedure CallAltF1(const Context: IOTAKeyContext; KeyCode: TShortCut;
      var BindingResult: TKeyBindingResult);
    function GetAction(const _Which: string; out _Url: string): boolean;
  public
    procedure CallHelp(_Which: string; const Context: IOTAKeyContext;
      KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
  protected // IOTAKeyBoardBinding
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;

procedure Register;
var
  KS: IOTAKeyboardServices;
begin
  if Supports(BorlandIDEServices, IOTAKeyBoardServices, KS) then
    KS.AddKeyboardBinding(TCtrlF1Help.Create);
end;

{ TMSDNHelpExpertBinding }

procedure TCtrlF1Help.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  BindingServices.AddKeyBinding([Shortcut(VK_F1, [ssCtrl])], CallCtrlF1, nil);
  BindingServices.AddKeyBinding([Shortcut(VK_F1, [ssShift])], CallShiftF1, nil);
  BindingServices.AddKeyBinding([Shortcut(VK_F1, [ssAlt])], CallAltF1, nil);
end;

function TCtrlF1Help.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TCtrlF1Help.GetDisplayName: string;
begin
  Result := 'dummzeuch.de Delphi 7 help for BDS expert';
end;

function TCtrlF1Help.GetName: string;
begin
  Result := 'dummzeuch.experts.Delphi7HelpForBds';
end;

function TCtrlF1Help.GetAction(const _Which: string; out _Url: string): boolean;
var
  Reg: TRegistry;
begin
  Result := false;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('software\dummzeuch\Delphi7HelpForBds', true) then
      try
        try
          _Url := Reg.ReadString('HelpFile' + _Which);
          if _Url = '-' then begin
            Result := false;
            exit;
          end;
          Result := (_Url <> '');
          if Result then begin
            if LeftStr(_Url, 5) = 'http:' then
              exit;
            if LeftStr(_Url, 8) = 'winhelp:' then
              exit;
            if LeftStr(_Url, 8) = 'chmhelp:' then
              exit;
            _Url := 'winhelp:' + _Url;
            Reg.WriteString('HelpFile' + _Which, _Url);
            exit;
          end;
        except
          // ignore
        end;

        Result := Tf_Delphi7HelpForBds.Execute(_Url);
        if Result then
          Reg.WriteString('HelpFile' + _Which, _Url)
        else
          Result := false;
      finally
        Reg.CloseKey;
      end;
  finally
    Reg.Free;
  end;
end;

procedure TCtrlF1Help.CallAltF1(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
begin
  CallHelp('AltF1', Context, KeyCode, BindingResult);
end;

procedure TCtrlF1Help.CallCtrlF1(
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
begin
  CallHelp('CtrlF1', Context, KeyCode, BindingResult);
end;

procedure TCtrlF1Help.CallHelp(_Which: string;
  const Context: IOTAKeyContext; KeyCode: TShortCut;
  var BindingResult: TKeyBindingResult);
var
  ep: IOTAEditPosition;
  EditView: IOTAEditView;
  CursorPos: TOTAEditPos;
  StartCol, EndCol, WordLen, Element, LineFlag: Integer;
  Keyword: string;
  Url: string;
begin
  if not GetAction(_Which, Url) then begin
    BindingResult := krUnhandled;
    exit;
  end;

  EditView := Context.EditBuffer.TopView;
  ep := EditView.Position;
  CursorPos := EditView.CursorPos;
  EditView.GetAttributeAtPos(CursorPos, True, Element, LineFlag);
  if (Element = atWhiteSpace) or (Element = atSymbol) then begin
    Dec(CursorPos.Col);
    EditView.GetAttributeAtPos(CursorPos, True, Element, LineFlag);
    if Element = atWhiteSpace then begin
      Inc(CursorPos.Col, 2);
      EditView.GetAttributeAtPos(CursorPos, True, Element, LineFlag);
    end;
  end;
  if Element in [atIdentifier, atReservedWord] then begin
    // The cursor can be anywhere on the word, or at the end of a word
    // If cursor is at the middle of a word, then extract the word
    if ep.IsWordCharacter then begin
      ep.Save;
      try
        EndCol := CursorPos.Col;
        while (ep.IsWordCharacter) and (ep.Column > 1) do
          ep.MoveRelative(0, -1);
        if ep.IsWordCharacter then
          StartCol := ep.Column // Start of a line...
        else
          StartCol := ep.Column + 1;
        WordLen := EndCol - StartCol;
        ep.Move(ep.Row, EndCol);
        while ep.IsWordCharacter do begin
          ep.MoveRelative(0, 1);
          Inc(WordLen);
        end;
        ep.MoveRelative(0, -WordLen);
        Keyword := ep.Read(WordLen);
      finally
        ep.Restore;
      end;
    end else begin
      // If cursor is at the end of a word, then extract it
      Keyword := ep.RipText('_', rfBackward or rfIncludeAlphaChars or rfIncludeNumericChars);
    end;
  end else
    Keyword := '';

  if LeftStr(Url, 8) = 'winhelp:' then begin
    Url := Copy(Url, 9);
    if Keyword <> '' then
      WinHelp(0, PChar(Url), HELP_KEY, integer(pchar(Keyword)))
    else
      WinHelp(0, PChar(Url), HELP_FINDER, 0);
    BindingResult := krHandled;
  end else if LeftStr(Url, 5) = 'http:' then begin
    ShellExecute(0, 'open', PChar(Url + Keyword), nil, nil, SW_SHOWNORMAL);
    BindingResult := krHandled;
  end else if LeftStr(Url, 8) = 'chmhelp:' then begin
    Url := Copy(Url, 9);
    UniqueString(Url);
    UniqueString(Keyword);
    HtmlHelp(Application.Handle, PChar(Url), HH_DISPLAY_INDEX, DWord(PChar(Keyword)));
  end;
end;

procedure TCtrlF1Help.CallShiftF1(const Context: IOTAKeyContext;
  KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
begin
  CallHelp('ShiftF1', Context, KeyCode, BindingResult);
end;

end.

