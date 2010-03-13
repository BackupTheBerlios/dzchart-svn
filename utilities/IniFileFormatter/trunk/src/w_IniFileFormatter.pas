unit w_IniFileFormatter;

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
  StdCtrls;

type
  Tf_IniFileFormatter = class(TForm)
    l_Filename: TLabel;
    ed_Filename: TEdit;
    od_Filename: TOpenDialog;
    b_Filename: TButton;
    l_Original: TLabel;
    m_Original: TMemo;
    l_Preview: TLabel;
    m_Preview: TMemo;
    gb_SortSections: TGroupBox;
    rb_SectionsUnsorted: TRadioButton;
    rb_SectionsAlpha: TRadioButton;
    rb_SectionsByTemplate: TRadioButton;
    gb_SortItems: TGroupBox;
    rb_ItemsUnsorted: TRadioButton;
    rb_ItemsAlpha: TRadioButton;
    rb_ItemsByTemplate: TRadioButton;
    b_Close: TButton;
    b_SaveAs: TButton;
    sd_Filename: TSaveDialog;
    l_Template: TLabel;
    ed_Template: TEdit;
    b_Template: TButton;

    procedure b_FilenameClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure b_CloseClick(Sender: TObject);
    procedure b_SaveAsClick(Sender: TObject);
    procedure b_TemplateClick(Sender: TObject);
  private
    procedure LoadAndSort(const _Filename: string);
    function FormatIni: boolean;
  public
    procedure SetSrcFile(const _Value: string);
    procedure SetSortSections(_Idx: integer);
    procedure SetSortItems(_Idx: integer);
    procedure SetTemplate(const _Value: string);
    function Execute(const _Value: string): boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzIniFileFormatter,
  u_dzIniSections;

procedure Tf_IniFileFormatter.b_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_IniFileFormatter.b_FilenameClick(Sender: TObject);
begin
  od_Filename.FileName := ed_Filename.Text;
  if od_Filename.Execute then begin
    ed_Filename.Text := od_Filename.FileName;
    LoadAndSort(ed_Filename.Text);
  end;
end;

procedure Tf_IniFileFormatter.b_TemplateClick(Sender: TObject);
begin
  od_Filename.FileName := ed_Template.Text;
  if od_Filename.Execute then begin
    ed_Template.Text := od_Filename.FileName;
  end;
  FormatIni;
end;

procedure Tf_IniFileFormatter.b_SaveAsClick(Sender: TObject);
begin
  sd_Filename.FileName := ed_Filename.Text;
  if sd_Filename.Execute then
    m_Preview.Lines.SaveToFile(sd_Filename.FileName);
end;

function Tf_IniFileFormatter.Execute(const _Value: string): boolean;
begin
  Result := FormatIni;
  if Result then
    m_Preview.Lines.SaveToFile(_Value);
end;

procedure Tf_IniFileFormatter.SetSortItems(_Idx: integer);
begin
  case _Idx of
    0: rb_ItemsUnsorted.Checked := true;
    2: rb_ItemsByTemplate.Checked := true;
  else
    rb_ItemsAlpha.Checked := true;
  end;
end;

procedure Tf_IniFileFormatter.SetSortSections(_Idx: integer);
begin
  case _Idx of
    0: rb_SectionsUnsorted.Checked := true;
    2: rb_SectionsByTemplate.Checked := true;
  else
    rb_SectionsAlpha.Checked := true;
  end;
end;

procedure Tf_IniFileFormatter.SetSrcFile(const _Value: string);
begin
  ed_Filename.Text := _Value;
  m_Original.Lines.LoadFromFile(_Value);
end;

procedure Tf_IniFileFormatter.SetTemplate(const _Value: string);
begin
  ed_Template.Text := _Value;
end;

procedure Tf_IniFileFormatter.SettingsChanged(Sender: TObject);
begin
  FormatIni;
end;

procedure Tf_IniFileFormatter.FormResize(Sender: TObject);
var
  w: Integer;
  l: Integer;
begin
  w := (ClientWidth - m_Original.Left * 3) div 2;
  m_Original.Width := w;
  m_Preview.Width := w;
  l := m_Original.Left * 2 + w;
  m_Preview.Left := l;
  l_Preview.Left := l;
end;

procedure Tf_IniFileFormatter.LoadAndSort(const _Filename: string);
begin
  m_Original.Lines.LoadFromFile(_Filename);
  FormatIni;
end;

function Tf_IniFileFormatter.FormatIni: boolean;
var
  b: boolean;
  Formatter: TIniFileFormatter;
  Template: TIniFileFormatter;
  SortTemplate: TStringList;
  Sections: TStringList;
  s: string;
begin
  Result := false;

  Template := nil;

  b := rb_SectionsByTemplate.Checked or rb_ItemsByTemplate.Checked;
  if b then begin
    if (ed_Template.Text = '') then begin
      m_Preview.Lines.Clear;
      exit;
    end;
    Template := TIniFileFormatter.Create;
    try
      Template.LoadFrom(ed_Template.Text);
    except
      FreeAndNil(Template);
      raise;
    end;
  end;

  Formatter := TIniFileFormatter.Create;
  try
    Formatter.Assign(m_Original.Lines);

    if rb_SectionsAlpha.Checked then begin
      Formatter.SortSections(nil);
    end else if rb_SectionsByTemplate.Checked then begin
      Assert(Assigned(Template));
      SortTemplate := TStringList.Create;
      try
        Template.AssignSectionsTo(SortTemplate);
        Formatter.SortSections(SortTemplate);
      finally
        FreeAndNil(SortTemplate);
      end;
    end;

    Sections := TStringList.Create;
    try
      Formatter.AssignSectionsTo(Sections);
      if rb_ItemsAlpha.Checked then begin
        for s in Sections do begin
          Formatter.SortItems(s, nil);
        end;
      end else if rb_ItemsByTemplate.Checked then begin
        Assert(Assigned(Template));
        for s in Sections do begin
          SortTemplate := TStringList.Create;
          try
            Template.AssignItemsTo(s, SortTemplate);
            Formatter.SortItems(s, SortTemplate);
          finally
            FreeAndNil(SortTemplate);
          end;
        end;
      end;
    finally
      FreeAndNil(Sections);
    end;

    Formatter.AssignTo(m_Preview.Lines);
  finally
    FreeAndNil(Formatter);
    FreeAndNil(Template);
  end;
  Result := true;
end;

end.

