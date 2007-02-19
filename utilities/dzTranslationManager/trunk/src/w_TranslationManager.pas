{GXFormatter.config=headwork}
unit w_TranslationManager;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst;

type
  Tf_TranslationManager = class(TForm)
    ed_Template: TEdit;
    l_Template: TLabel;
    l_OutputFile: TLabel;
    l_TranslationFiles: TLabel;
    clb_TranslationFiles: TCheckListBox;
    ed_OutputFile: TEdit;
    b_SelectTemplate: TButton;
    b_AddTranslationFile: TButton;
    b_OutputFile: TButton;
    b_DeleteTranslationFile: TButton;
    b_Translate: TButton;
    b_Exit: TButton;
    od_OutputFile: TOpenDialog;
    od_TemplateFile: TOpenDialog;
    od_AddTranslation: TOpenDialog;
    b_Up: TButton;
    b_Down: TButton;
    procedure b_ExitClick(Sender: TObject);
    procedure b_SelectTemplateClick(Sender: TObject);
    procedure b_OutputFileClick(Sender: TObject);
    procedure b_AddTranslationFileClick(Sender: TObject);
    procedure b_DeleteTranslationFileClick(Sender: TObject);
    procedure b_UpClick(Sender: TObject);
    procedure b_DownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure b_TranslateClick(Sender: TObject);
  private
  public
  end;

var
  f_TranslationManager: Tf_TranslationManager;

implementation

{$R *.dfm}

uses
  Registry,
  ShlObj,
  ShellApi,
  poparser,
  u_TranslationMerger;

const
  REG_KEY = 'software\dummzeuch.de\TranslationManager';

procedure Tf_TranslationManager.b_AddTranslationFileClick(Sender: TObject);
var
  Idx: integer;
  s: string;
  i: Integer;
begin
  Idx := clb_TranslationFiles.ItemIndex;
  if Idx <> -1 then
    begin
      s := clb_TranslationFiles.Items[Idx];
      od_AddTranslation.FileName := s;
    end;
  if od_AddTranslation.Execute then
    for i := 0 to od_AddTranslation.Files.Count - 1 do
      begin
        Inc(Idx);
        clb_TranslationFiles.Items.Insert(Idx, od_AddTranslation.Files[i]);
        clb_TranslationFiles.Checked[Idx] := true;
      end;
end;

procedure Tf_TranslationManager.b_DeleteTranslationFileClick(Sender: TObject);
var
  Idx: Integer;
  s: string;
begin
  Idx := clb_TranslationFiles.ItemIndex;
  if Idx = -1 then
    exit;
  s := clb_TranslationFiles.Items[Idx];
  if IdYes = MessageDlg('Really remove translation file'#13#10
    + s + #13#10
    + 'from List?', mtConfirmation, [mbYes, mbNo], 0) then
    clb_TranslationFiles.Items.Delete(Idx);
end;

procedure Tf_TranslationManager.b_DownClick(Sender: TObject);
var
  Idx: integer;
  s: string;
begin
  Idx := clb_TranslationFiles.ItemIndex;
  if (Idx = -1) or (Idx >= clb_TranslationFiles.Items.Count - 1) then
    exit;
  s := clb_TranslationFiles.Items[Idx];
  clb_TranslationFiles.Items[Idx] := clb_TranslationFiles.Items[Idx + 1];
  clb_TranslationFiles.Items[Idx + 1] := s;
  clb_TranslationFiles.ItemIndex := Idx + 1;
end;

procedure Tf_TranslationManager.b_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_TranslationManager.b_OutputFileClick(Sender: TObject);
begin
  od_OutputFile.FileName := ed_OutputFile.Text;
  if od_OutputFile.Execute then
    ed_OutputFile.Text := od_OutputFile.FileName;
end;

procedure Tf_TranslationManager.b_SelectTemplateClick(Sender: TObject);
begin
  od_TemplateFile.FileName := ed_Template.Text;
  if od_TemplateFile.Execute then
    ed_Template.Text := od_TemplateFile.FileName;
end;

function ShellExecEx(const _FileName: string; const _Parameters: string): Boolean;
var
  Sei: TShellExecuteInfo;
begin
  FillChar(Sei, SizeOf(Sei), #0);
  Sei.cbSize := SizeOf(Sei);
  Sei.fMask := SEE_MASK_DOENVSUBST or SEE_MASK_FLAG_NO_UI;
  Sei.lpFile := PChar(_FileName);
  Sei.lpParameters := Pointer(_Parameters);
  Sei.lpVerb := nil;
  Sei.nShow := SW_SHOW;
  Result := ShellExecuteEx(@Sei);
end;

function GetTempDir: string;
begin
  SetLength(Result, MAX_PATH);
  GetTempPath(Length(Result), PChar(Result));
  Result := PChar(Result);
end;

function GenerateUniqeTempFile: string;
var
  TempDir: string;
begin
  TempDir := GetTempDir;
  SetLength(Result, MAX_PATH);
  GetTempFileName(PChar(TempDir), 'dz', 0, PChar(Result));
  Result := PChar(Result);
end;

procedure Tf_TranslationManager.b_TranslateClick(Sender: TObject);
var
  Merger: TTranslationMerger;
  Translations: TPoEntryList;
  i: integer;
  s: string;
  Template: TPoEntryList;
  TemplateEntry: TPoEntry;
  Translation: TPoEntry;
begin
  Merger := TTranslationMerger.Create;
  try
    for i := 0 to clb_TranslationFiles.Items.Count - 1 do
      begin
        if not clb_TranslationFiles.Checked[i] then
          continue;
        s := clb_TranslationFiles.Items[i];
        Merger.AddTranslationSource(s);
      end;
    Translations := Merger.DetachTranslations;
  finally
    Merger.Free;
  end;

  Template := TPoEntryList.Create;
  try
    Template.LoadFromFile(ed_Template.Text);
    TemplateEntry := Template.FindFirst;
    while TemplateEntry <> nil do
      begin
        Translation := Translations.Find(TemplateEntry.MsgId);
        if Translation <> nil then
          begin
            TemplateEntry.Fuzzy := Translation.Fuzzy;
            TemplateEntry.MsgStr := Translation.MsgStr;
          end;
        TemplateEntry := Template.FindNext(TemplateEntry);
      end;
    Template.SaveToFile(ed_OutputFile.Text);
  finally
    Template.Free;
  end;
end;

procedure Tf_TranslationManager.b_UpClick(Sender: TObject);
var
  Idx: integer;
  s: string;
begin
  Idx := clb_TranslationFiles.ItemIndex;
  if Idx < 1 then
    exit;
  s := clb_TranslationFiles.Items[Idx];
  clb_TranslationFiles.Items[Idx] := clb_TranslationFiles.Items[Idx - 1];
  clb_TranslationFiles.Items[Idx - 1] := s;
  clb_TranslationFiles.ItemIndex := Idx - 1;
end;

function TryReadInteger(_Reg: TRegistry; const _Name: string; out _Value: integer): boolean;
begin
  Result := _Reg.ValueExists(_Name);
  if Result then
    _Value := _Reg.ReadInteger(_Name);
end;

function GetProgramFilesDir: string;
type
  TSHGetFolderPath = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; Stdcall;

  function LoadSHFolder(var SHGetFolderPath: TSHGetFolderPath): Integer;
  var
    Hdl: Hwnd;
  begin
    Result := 0;
    Hdl := LoadLibrary('SHFOLDER.DLL');
    if Hdl <> 0 then
      begin
        @SHGetFolderPath := GetProcAddress(Hdl, 'SHGetFolderPathA');
        if @SHGetFolderPath <> nil then
          Result := Hdl;
      end;
  end;

const
  CSIDL_PROGRAM_FILES = $26;
  SHGFP_TYPE_CURRENT = 0;
var
  Path: array[0..MAX_PATH] of Char;
  Pidl: PItemIDList;
  Hdl: Hwnd;
  SHGetFolderPath: TSHGetFolderPath;
begin
  ZeroMemory(@Path, SizeOf(Path));
  Hdl := LoadSHFolder(SHGetFolderPath);
  if Hdl <> 0 then
    begin
      if Succeeded(SHGetFolderPath(Application.Handle, CSIDL_PROGRAM_FILES, 0, SHGFP_TYPE_CURRENT, Path)) then
        Result := Path;
      FreeLibrary(Hdl);
    end
  else
    begin
      if Succeeded(SHGetspecialfolderLocation(Application.Handle, CSIDL_PROGRAM_FILES, PIdl)) then
        SHGetPathFromIDList(Pidl, Path);
      Result := Path;
    end;
end;

procedure Tf_TranslationManager.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
  Cnt: Integer;
  i: Integer;
  s: string;
  Entry: string;
  Idx: integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REG_KEY, true);
    try
      ed_Template.Text := Reg.ReadString('Template');
      ed_OutputFile.Text := Reg.ReadString('Output');
      if not TryReadInteger(Reg, 'TranslationCount', Cnt) then
        Cnt := 0;
      Idx := 0;
      for i := 0 to Cnt - 1 do
        begin
          Entry := 'Translation_' + IntToStr(i);
          s := Reg.ReadString(Entry + '_Filename');
          if s <> '' then
            begin
              clb_TranslationFiles.Items.Add(s);
              clb_TranslationFiles.Checked[Idx] := Reg.ReadBool(Entry + '_Checked');
              Inc(Idx);
            end;
        end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tf_TranslationManager.FormDestroy(Sender: TObject);
var
  Reg: TRegistry;
  Cnt: Integer;
  i: Integer;
  Entry: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REG_KEY, true);
    try
      Reg.WriteString('Template', ed_Template.Text);
      Reg.WriteString('Output', ed_OutputFile.Text);
      Cnt := clb_TranslationFiles.Items.Count;
      Reg.WriteInteger('TranslationCount', Cnt);
      for i := 0 to Cnt - 1 do
        begin
          Entry := 'Translation_' + IntToStr(i);
          Reg.WriteString(Entry + '_Filename', clb_TranslationFiles.Items[i]);
          Reg.WriteBool(Entry + '_Checked', clb_TranslationFiles.Checked[i]);
        end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

end.

