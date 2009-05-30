unit WindowsXPStyleDemo;

// Virtual Treeview sample form demonstrating following features:
//   - Windows XP style treeview.
// Written by Mike Lischke.

interface

{$INCLUDE Compilers.inc}

{$IFDEF COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$ENDIF COMPILER_7_UP}

uses
{$IFDEF VER140}
  Variants,
{$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  VirtualTrees,
  ImgList,
  ComCtrls,
  ToolWin,
  Menus,
  StdCtrls;

type
  TWindowsXPForm = class(TForm)
    XPTree: TVirtualStringTree;
    LargeImages: TImageList;
    SmallImages: TImageList;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    PrintDialog: TPrintDialog;
    procedure XPTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer);
    procedure FormCreate(Sender: TObject);
    procedure XPTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure XPTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure XPTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure XPTreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure XPTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure XPTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
    procedure Label4Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure XPTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
  end;

var
  WindowsXPForm: TWindowsXPForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Main,
  ShellAPI,
  Printers,
  States;

{$R *.dfm}

type
  PEntry = ^TEntry;
  TEntry = record
    Caption: WideString;
    Image: Integer;
    Size: Int64;
  end;

var
  TreeEntries: array[0..17] of TEntry = (
    (Caption: 'My Computer'; Image: 0; Size: 0),
    (Caption: 'Network Places'; Image: 1; Size: 0),
    (Caption: 'Recycle Bin'; Image: 2; Size: 0),
    (Caption: 'My Documents'; Image: 3; Size: 0),
    (Caption: 'My Music'; Image: 4; Size: 0),
    (Caption: 'My Pictures'; Image: 5; Size: 0),
    (Caption: 'Control Panel'; Image: 6; Size: 0),
    (Caption: 'Help'; Image: 7; Size: 0),
    (Caption: 'Help Document'; Image: 8; Size: 0),
    (Caption: 'User Accounts'; Image: 9; Size: 0),
    (Caption: 'Internet'; Image: 10; Size: 0),
    (Caption: 'Network Group'; Image: 11; Size: 0),
    (Caption: 'Folder'; Image: 12; Size: 0),
    (Caption: 'Window'; Image: 13; Size: 0),
    (Caption: 'Warning'; Image: 14; Size: 0),
    (Caption: 'Information'; Image: 15; Size: 0),
    (Caption: 'Critical'; Image: 16; Size: 0),
    (Caption: 'Security'; Image: 17; Size: 0)
    );

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer);

var
  Data: PEntry;

begin
  Data := Sender.GetNodeData(Node);
  case Kind of
    ikNormal, ikSelected:
      if (Column = 0) and (Node.Parent = Sender.RootNode) then
        Index := Data.Image;
    ikState:
      case Column of
        0:
          if Node.Parent <> Sender.RootNode then
            Index := 21;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.FormCreate(Sender: TObject);

begin
  XPTree.NodeDataSize := SizeOf(TEntry);

  ConvertToHighColor(LargeImages);
  ConvertToHighColor(SmallImages);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  Data: PEntry;

begin
  if ParentNode = nil then begin
    Include(InitialStates, ivsHasChildren);
    Data := Sender.GetNodeData(Node);
    Data^ := TreeEntries[Node.Index mod 18];
    Data.Size := Random(100000);
    Node.CheckType := ctCheckBox;
  end else
    Node.CheckType := ctRadioButton;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);

begin
  ChildCount := 5;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);

var
  Data: PEntry;

begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0:
      if Node.Parent = Sender.RootNode then
        CellText := Data.Caption
      else
        Text := 'More entries';
    1:
      if Node.Parent = Sender.RootNode then
        CellText := FloatToStr(Data.Size / 1000) + ' MB';
    2:
      if Node.Parent = Sender.RootNode then
        CellText := 'System Folder';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if Button = mbLeft then begin
    with Sender, Treeview do begin
      if SortColumn > NoColumn then
        Columns[SortColumn].Options := Columns[SortColumn].Options + [coParentColor];

      // Do not sort the last column, it contains nothing to sort.
      if Column = 2 then
        SortColumn := NoColumn
      else begin
        if (SortColumn = NoColumn) or (SortColumn <> Column) then begin
          SortColumn := Column;
          SortDirection := sdAscending;
        end else if SortDirection = sdAscending then
          SortDirection := sdDescending
        else
          SortDirection := sdAscending;

        Columns[SortColumn].Color := $F7F7F7;
        SortTree(SortColumn, SortDirection, False);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  Data1, Data2: PEntry;

begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0:
      Result := CompareText(Data1.Caption, Data2.Caption);
    1:
      Result := Data1.Size - Data2.Size;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);

begin
  // Show only a dummy hint. It is just to demonstrate how to do it.
  HintText := 'Size larger than 536 MB' + #13 +
    'Folders: addins, AppPatch, Config, Connection Wizard, ...' + #13 +
    'Files: 1280.bmp, 1280x1024.bmp, 2001 94 mars.bmp, ac3api.ini, ...';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.Label4Click(Sender: TObject);

begin
  ShellExecute(0, 'open', 'http://groups.yahoo.com/group/VirtualExplorerTree', nil, nil, SW_SHOW);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.ToolButton9Click(Sender: TObject);

begin
  if PrintDialog.Execute then
    XPTree.Print(Printer, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWindowsXPForm.XPTreeStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);

begin
  if not (csDestroying in ComponentState) then
    UpdateStateDisplay(Sender.TreeStates, Enter, Leave);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
