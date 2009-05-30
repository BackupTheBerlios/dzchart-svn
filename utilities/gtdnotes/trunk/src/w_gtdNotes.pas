unit w_gtdNotes;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  VirtualTrees,
  StdCtrls,
  ExtCtrls,
  Contnrs,
  ActnList,
  ComCtrls,
  ToolWin,
  xmldom,
  XMLIntf,
  msxmldom,
  XMLDoc;

type
  TGtdNode = class
  strict private
    FName: string;
  strict protected
    function GetIsNextAction: boolean; virtual;
    procedure SetIsNextAction(const Value: boolean); virtual;
    function GetIsDone: boolean; virtual;
    procedure SetIsDone(const Value: boolean); virtual;
    function GetCount: integer; virtual;
    function GetItems(_Idx: integer): TGtdNode; virtual;
  public
    constructor Create(const _Name: string);
    property Name: string read FName;
    property IsDone: boolean read GetIsDone write SetIsDone;
    property Count: integer read GetCount;
    property IsNextAction: boolean read GetisNextAction write SetisNextAction;
    property Items[_Idx: integer]: TGtdNode read GetItems;
  end;

  TGtdMiddleNode = class(TGtdNode)
  public
    function Add(_Node: TGtdNode): integer; virtual; abstract;
    property Items[_Idx: integer]: TGtdNode read GetItems;
  end;

  TGtdContainer = class(TGtdMiddleNode)
  strict private
    FItems: TObjectList;
  strict protected
    function GetItems(_Idx: integer): TGtdNode; override;
    function GetCount: integer; override;
  public
    constructor Create(const _Name: string);
    destructor Destroy; override;
    function Add(_Node: TGtdNode): integer; override;
  end;

  TGtdFilter = class(TGtdMiddleNode)
  strict private
    FItems: TList;
    FId: string;
  strict protected
    function GetItems(_Idx: integer): TGtdNode; override;
    function GetCount: integer; override;
  public
    constructor Create(const _Name: string; _Id: string); overload;
    constructor Create(_Node: IXMLNode); overload;
    destructor Destroy; override;
    function Add(_Node: TGtdNode): integer; override;
    function Find(_Node: TGtdNode): boolean;
    property Id: string read FId write FId;
  end;

  TGtdAction = class(TGtdNode)
  strict private
    FIsDone: boolean;
    FIsNextAction: boolean;
  strict protected
    procedure SetIsDone(const _Value: boolean); override;
    function GetIsDone: boolean; override;
    function GetIsNextAction: boolean; override;
    procedure SetIsNextAction(const _Value: boolean); override;
  public
    constructor Create(_Node: IXMLNode); overload;
  end;

  TGtdPlace = class(TGtdFilter)
  end;

  TGtdLabel = class(TGtdFilter)
  end;

  TGtdFilterContainer = class(TGtdContainer)
  public
    function FindId(const _Id: string; out _Filter: TGtdFilter): boolean;
  end;

  TGtdPlaces = class(TGtdFilterContainer)
  public
    constructor Create;
  end;

  TGtdLabels = class(TGtdFilterContainer)
  public
    constructor Create;
  end;

  TGtdProject = class(TGtdContainer)
  public
    constructor Create(_Node: IXMLNode; _Places: TGtdPlaces; _Labels: TGtdLabels); overload;
  end;

  TGtdProjects = class(TGtdContainer)
  public
    constructor Create;
    function FindName(const _Name: string; out _Project: TGtdProject): boolean;
  end;

type
  Tf_gtdNotes = class(TForm)
    VST: TVirtualStringTree;
    tb_Main: TToolBar;
    tb_Load: TToolButton;
    tb_Save: TToolButton;
    ToolButton3: TToolButton;
    tb_ShowDone: TToolButton;
    TheActionList: TActionList;
    act_Load: TAction;
    act_Save: TAction;
    act_SaveAs: TAction;
    act_ShowDone: TAction;
    tb_SaveAs: TToolButton;
    tb_SetNextAction: TToolButton;
    act_SetNextAction: TAction;
    ToolButton7: TToolButton;
    tb_AddAction: TToolButton;
    act_AddAction: TAction;
    tb_DeleteAction: TToolButton;
    act_DeleteAction: TAction;
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure act_LoadExecute(Sender: TObject);
    procedure act_SaveExecute(Sender: TObject);
    procedure act_SaveAsExecute(Sender: TObject);
    procedure act_ShowDoneExecute(Sender: TObject);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTKeyPress(Sender: TObject; var Key: Char);
    procedure act_SetNextActionExecute(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FPlaces: TGtdPlaces;
    FProjects: TGtdProjects;
    FLabels: TGtdLabels;
    procedure SetDoneVisible(_Visible: boolean);
    procedure HideNodes(_Sender: TBaseVirtualTree; _Node: PVirtualNode;
      _Data: Pointer; var _Abort: Boolean);
    procedure UnsetNextAction(_Sender: TBaseVirtualTree; _Node: PVirtualNode;
      _Data: Pointer; var _Abort: Boolean);
    procedure MakeFocusedActionNext;
    function GetFocusedNode(out _Node: PVirtualNode;
      out _GtdNode: TGtdNode): boolean;
    function GetFocusedProjectAction(out _Node: PVirtualNode;
      out _GtdNode: TGtdNode): boolean;
    procedure LoadXml(const _Filename: string);
    procedure SaveXml(const _Filename: string);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

var
  f_gtdNotes: Tf_gtdNotes;

implementation

{$R *.DFM}

type
  PGtdLabelRec = ^TGtdLabelRec;
  TGtdLabelRec = record
    GtdNode: TGtdNode;
  end;

{ Tf_gtdNotes }

constructor Tf_gtdNotes.Create(_Owner: TComponent);
begin
  inherited;

  FPlaces := TGtdPlaces.Create;
  FProjects := TGtdProjects.Create;
  FLabels := TGtdLabels.Create;

  LoadXml('data.gtdnote');

  VST.NodeDataSize := SizeOf(TGtdLabelRec);

  // Places / Projects / Labels
  VST.RootNodeCount := 3;
end;

destructor Tf_gtdNotes.Destroy;
begin
  FLabels.Free;
  FPlaces.Free;
  FProjects.Free;
  inherited;
end;

procedure Tf_gtdNotes.LoadXml(const _Filename: string);

  function FindRootNode(_XmlDoc: TXMLDocument; const _Name: string): IXMLNode;
  begin
    Result := _XmlDoc.ChildNodes.First;
    while Assigned(Result) do begin
      if Result.NodeName = 'gtdnotes' then
        exit;
      Result := Result.NextSibling;
    end;
    raise Exception.Create('no "gtdnotes" node found');
  end;

var
  XmlDoc: TXMLDocument;
  LabelNode: IXMLNode;
  RootNode: IXMLNode;
  PlaceNode: IXMLNode;
  ProjectNode: IXMLNode;
  Node: IXMLNode;
  Project: TGtdProject;
begin
  // Owner must NOT be nil, otherwise we get an access violation when accessing
  // the ChildNodes collection.
  XmlDoc := TXMLDocument.Create(self);
  try
    XmlDoc.DOMVendor := GetDOMVendor('MSXML');
    XmlDoc.Options := XmlDoc.Options - [doAttrNull];
    XmlDoc.LoadFromFile(_Filename);
    XmlDoc.Active := true;

    RootNode := FindRootNode(XmlDoc, 'gtdnotes');

    Node := RootNode.ChildNodes.First;
    while Assigned(Node) do begin
      if Node.NodeName = 'places' then begin
        PlaceNode := Node.ChildNodes.First;
        while Assigned(PlaceNode) do begin
          if PlaceNode.NodeName = 'place' then begin
            FPlaces.Add(TGtdPlace.Create(PlaceNode));
          end;
          PlaceNode := PlaceNode.NextSibling;
        end;
      end else if Node.nodeName = 'labels' then begin
        LabelNode := Node.ChildNodes.First;
        while Assigned(LabelNode) do begin
          if LabelNode.NodeName = 'label' then begin
            FLabels.Add(TGtdLabel.Create(LabelNode));
          end;
          LabelNode := LabelNode.NextSibling;
        end;
      end else if Node.nodeName = 'projects' then begin
        ProjectNode := Node.ChildNodes.First;
        while Assigned(ProjectNode) do begin
          if ProjectNode.NodeName = 'project' then begin
            Project := TGtdProject.Create(ProjectNode, FPlaces, FLabels);
            FProjects.Add(Project);
          end;
          ProjectNode := ProjectNode.nextSibling;
        end;
      end;
      Node := Node.nextSibling;
    end;
  finally
    FreeAndNil(XmlDoc);
  end;
end;
//procedure Tf_gtdNotes.LoadXml(const _Filename: string);
//var
//  XmlDoc: TXMLDocument;
//  LabelNode: IDOMNode;
//  RootNode: IDOMNode;
//  PlaceNode: IDOMNode;
//  ProjectNode: IDOMNode;
//  Node: IDOMNode;
//  doc: IDOMDocument;
//  Project: TGtdProject;
//  xmlnode: IXMLNode;
//begin
//  // owner must NOT be nil, otherwise we get an access violation
//  XmlDoc := TXMLDocument.Create(self);
//  try
//    XmlDoc.Active := false;
//    XmlDoc.DOMVendor := GetDOMVendor('MSXML');
//    XmlDoc.LoadFromFile(_Filename);
//    XmlDoc.Active := true;
//
//    if XmlDoc.ChildNodes.Count < 1 then
//      raise Exception.Create('Document must contain at least one child node.');
//    xmlnode := XmlDoc.ChildNodes[0];
//    WriteLn(xmlnode.NodeName);
//
//    doc := XmlDoc.DOMDocument;
//    if doc.childNodes.length <> 1 then
//      raise Exception.Create('Document must contain one child node.');
//    RootNode := doc.firstChild;
//    if RootNode.nodeName <> 'gtdnotes' then
//      raise Exception.Create('no "gtdnotes" node found');
//    Node := RootNode.firstChild;
//    while Assigned(Node) do begin
//      if Node.nodeName = 'places' then begin
//        PlaceNode := Node.firstChild;
//        while Assigned(PlaceNode) do begin
//          if PlaceNode.nodeName = 'place' then begin
//            FPlaces.Add(TGtdPlace.Create(PlaceNode));
//          end;
//          PlaceNode := PlaceNode.nextSibling;
//        end;
//      end else if Node.nodeName = 'labels' then begin
//        LabelNode := Node.firstChild;
//        while Assigned(LabelNode) do begin
//          if LabelNode.nodeName = 'label' then begin
//            FLabels.Add(TGtdLabel.Create(LabelNode));
//          end;
//          LabelNode := LabelNode.nextSibling;
//        end;
//      end else if Node.nodeName = 'projects' then begin
//        ProjectNode := Node.firstChild;
//        while Assigned(ProjectNode) do begin
//          if ProjectNode.nodeName = 'project' then begin
//            Project := TGtdProject.Create(ProjectNode, FPlaces, FLabels);
//            FProjects.Add(Project);
//          end;
//          ProjectNode := ProjectNode.nextSibling;
//        end;
//      end;
//      Node := Node.nextSibling;
//    end;
//  finally
//    FreeAndNil(XmlDoc);
//  end;
//end;

procedure Tf_gtdNotes.SaveXml(const _Filename: string);
var
  XmlDoc: TXMLDocument;
  RootNode: IXMLNode;
  PlacesNode: IXMLNode;
  i: Integer;
  PlaceNode: IXMLNode;
  Place: TGtdPlace;
  LabelsNode: IXMLNode;
  LabelNode: IXMLNode;
  GtdLabel: TGtdLabel;
  ProjectsNode: IXMLNode;
  ProjectNode: IXMLNode;
  Project: TGtdProject;
  ProjIdx: Integer;
  ActIdx: Integer;
  ActionNode: IXMLNode;
  Action: TGtdAction;
begin
  XmlDoc := TXMLDocument.Create(nil);
  try
    XmlDoc.Active := true;
    XmlDoc.Version := '1.0';
    XmlDoc.Encoding := 'UTF-8';
    XmlDoc.StandAlone := 'no';
    XmlDoc.Options := XmlDoc.Options + [doNodeAutoIndent];

    RootNode := XmlDoc.AddChild('gtdnotes');
    PlacesNode := RootNode.AddChild('places');
    for i := 0 to FPlaces.Count - 1 do begin
      PlaceNode := PlacesNode.AddChild('place');
      Place := FPlaces.Items[i] as TGtdPlace;
      PlaceNode.Attributes['name'] := Place.Name;
      PlaceNode.Attributes['id'] := Place.Id;
    end;

    LabelsNode := RootNode.AddChild('labels');
    for i := 0 to FLabels.Count - 1 do begin
      LabelNode := LabelsNode.AddChild('label');
      GtdLabel := FLabels.Items[i] as TGtdLabel;
      LabelNode.Attributes['name'] := GtdLabel.Name;
      LabelNode.Attributes['id'] := GtdLabel.Id;
    end;

    ProjectsNode := RootNode.AddChild('projects');
    for ProjIdx := 0 to FProjects.Count - 1 do begin
      Project := FProjects.Items[ProjIdx] as TGtdProject;
      ProjectNode := ProjectsNode.AddChild('project');
      ProjectNode.Attributes['name'] := Project.Name;
      for ActIdx := 0 to Project.Count - 1 do begin
        Action := Project.Items[ActIdx] as TGtdAction;
        ActionNode := ProjectNode.AddChild('action');
        ActionNode.Attributes['name'] := Action.Name;
        if Action.IsDone then
          ActionNode.Attributes['isdone'] := '1';
        if Action.IsNextAction then
          ActionNode.Attributes['isnext'] := '1';
        for i := 0 to FPlaces.Count - 1 do begin
          Place := FPlaces.Items[i] as TGtdPlace;
          if Place.Find(Action) then begin
            PlaceNode := ActionNode.AddChild('place');
            PlaceNode.Attributes['id'] := Place.Id;
          end;
        end;
        for i := 0 to FLabels.Count - 1 do begin
          GtdLabel := FLabels.Items[i] as TGtdLabel;
          if GtdLabel.Find(Action) then begin
            LabelNode := ActionNode.AddChild('label');
            LabelNode.Attributes['id'] := GtdLabel.Id;
          end;
        end;
      end;
    end;
    XmlDoc.SaveToFile(_Filename);
  finally
    FreeAndNil(XmlDoc);
  end;
end;

function Tf_gtdNotes.GetFocusedProjectAction(out _Node: PVirtualNode; out _GtdNode: TGtdNode): boolean;
var
  Data: PGtdLabelRec;
begin
  Result := GetFocusedNode(_Node, _GtdNode);
  if Result then begin
    if not Assigned(_Node.Parent) then
      Result := false
    else begin
      Data := VST.GetNodeData(_Node.Parent);
      Result := Assigned(Data) and Assigned(Data.GtdNode) and (Data.GtdNode is TGtdProject);
    end;
  end;
end;

function Tf_gtdNotes.GetFocusedNode(out _Node: PVirtualNode; out _GtdNode: TGtdNode): boolean;
var
  Data: PGtdLabelRec;
begin
  Result := false;
  _Node := VST.FocusedNode;
  if not Assigned(_Node) or not Assigned(_Node.Parent) then
    exit;
  Data := VST.GetNodeData(_Node);
  Assert(Assigned(Data.GtdNode));
  _GtdNode := Data.GtdNode;
  Result := true;
end;

procedure Tf_gtdNotes.MakeFocusedActionNext;
var
  Node: PVirtualNode;
  GtdNode: TGtdNode;
begin
  if not GetFocusedProjectAction(Node, GtdNode) then
    exit;
  VST.BeginUpdate;
  try
    GtdNode.IsNextAction := not GtdNode.IsNextAction;
    VST.IterateSubtree(Node.Parent, UnsetNextAction, GtdNode);
  finally
    VST.EndUpdate;
  end;
end;

procedure Tf_gtdNotes.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PGtdLabelRec;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then
    CellText := 'Data not assigned'
  else if not Assigned(Data.GtdNode) then
    CellText := 'Data.GtdNode not assigned'
  else begin
    CellText := Data.GtdNode.Name;
  end;
end;

procedure Tf_gtdNotes.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  GtdNode: TGtdNode;
  TreeNode: PVirtualNode;
  IsProjectAction: boolean;
begin
  if not Assigned(Node) then
    exit;
  IsProjectAction := GetFocusedProjectAction(TreeNode, GtdNode);
  act_SetNextAction.Enabled := IsProjectAction;
end;

procedure Tf_gtdNotes.VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PGtdLabelRec;
  vn: PVirtualNode;
begin
  Data := Sender.GetNodeData(Node);
  Assert(Assigned(Data));
  VST.BeginUpdate;
  try
    Data.GtdNode.IsDone := (Node.CheckState = csCheckedNormal);
    vn := VST.GetFirstChild(nil);
    while assigned(vn) do begin
      Data := Sender.GetNodeData(vn);
      Assert(Assigned(Data));
      if Data.GtdNode.IsDone then
        vn.CheckState := csCheckedNormal
      else
        vn.CheckState := csUncheckedNormal;
      vn := VST.GetNext(vn);
    end;
  finally
    VST.EndUpdate;
  end;
end;

procedure Tf_gtdNotes.UnsetNextAction(_Sender: TBaseVirtualTree; _Node: PVirtualNode; _Data: Pointer; var _Abort: Boolean);
var
  NodeData: PGtdLabelRec;
begin
  NodeData := VST.GetNodeData(_Node);
  Assert(Assigned(NodeData.GtdNode));
  if NodeData.GtdNode <> _Data then
    NodeData.GtdNode.IsNextAction := false;
end;

procedure Tf_gtdNotes.VSTDblClick(Sender: TObject);
begin
  MakeFocusedActionNext;
end;

procedure Tf_gtdNotes.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PGtdLabelRec;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Finalize(Data^);
end;

procedure Tf_gtdNotes.VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Level: integer;
  Data: PGtdLabelRec;
  Index: integer;
  ParentData: PGtdLabelRec;
begin
  Data := Sender.GetNodeData(Node);
  Level := Sender.GetNodeLevel(Node);
  Index := Node.Index;
  if Level = 0 then begin
    case Index of
      0: Data.GtdNode := FPlaces;
      1: Data.GtdNode := FProjects;
      2: Data.GtdNode := FLabels;
    end;
  end else begin
    ParentData := Sender.GetNodeData(ParentNode);
    if Index >= ParentData.GtdNode.Count then
      raise Exception.Create('Programmer Error: Node index must not be greater or equal item count.');
    Data.GtdNode := ParentData.GtdNode.Items[Index] as TGtdNode;
  end;
  VST.ChildCount[Node] := Data.GtdNode.Count;
  VST.Expanded[Node] := (Data.GtdNode.Count > 0);
  if Level = 2 then begin
    VST.CheckType[Node] := ctCheckBox;
    if Data.GtdNode.IsDone then
      VST.CheckState[Node] := csCheckedNormal;
  end;
end;

procedure Tf_gtdNotes.VSTKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    MakeFocusedActionNext;
end;

procedure Tf_gtdNotes.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PGtdLabelRec;
begin
  Data := Sender.GetNodeData(Node);
  if Data.GtdNode.isNextAction then
    TargetCanvas.Font.Style := Canvas.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := Canvas.Font.Style - [fsBold];
end;

procedure Tf_gtdNotes.HideNodes(_Sender: TBaseVirtualTree; _Node: PVirtualNode; _Data: Pointer; var _Abort: Boolean);
var
  NodeData: PGtdLabelRec;
begin
  NodeData := _Sender.GetNodeData(_Node);
  Assert(Assigned(NodeData));
  Assert(Assigned(NodeData.GtdNode));

  if Boolean(Integer(_Data)) then
    _Sender.IsVisible[_Node] := True
  else
    _Sender.IsVisible[_Node] := not NodeData.GtdNode.IsDone;
end;

procedure Tf_gtdNotes.SetDoneVisible(_Visible: boolean);
begin
  act_ShowDone.Checked := _Visible;
  VST.BeginUpdate;
  try
    VST.IterateSubtree(nil, HideNodes, Pointer(Ord(_Visible)), [], True);
  finally
    VST.EndUpdate;
  end;
end;

procedure Tf_gtdNotes.act_LoadExecute(Sender: TObject);
begin
//
end;

procedure Tf_gtdNotes.act_SaveAsExecute(Sender: TObject);
begin
//
end;

procedure Tf_gtdNotes.act_SaveExecute(Sender: TObject);
begin
  SaveXml('test.gtdnote');
end;

procedure Tf_gtdNotes.act_SetNextActionExecute(Sender: TObject);
begin
  MakeFocusedActionNext;
end;

procedure Tf_gtdNotes.act_ShowDoneExecute(Sender: TObject);
begin
  SetDoneVisible(not act_ShowDone.Checked);
end;

{ TGtdNode }

constructor TGtdNode.Create(const _Name: string);
begin
  inherited Create;
  if _Name = '' then
    raise Exception.Create('Name must not be empty.');
  FName := _Name;
end;

function TGtdNode.GetIsDone: boolean;
begin
  Result := false;
end;

function TGtdNode.GetisNextAction: boolean;
begin
  Result := false;
end;

function TGtdNode.GetCount: integer;
begin
  Result := 0;
end;

function TGtdNode.GetItems(_Idx: integer): TGtdNode;
begin
  raise Exception.Create('Programmer error: A leaf node does not have any items.');
end;

procedure TGtdNode.SetIsDone(const Value: boolean);
begin
  // do nothing
end;

procedure TGtdNode.SetisNextAction(const Value: boolean);
begin
  // ignore
end;

{ TGtdFilter }

constructor TGtdFilter.Create(const _Name: string; _Id: string);
begin
  inherited Create(_Name);
  if _Id = '' then
    raise Exception.Create('Id must not be empty.');
  FId := _Id;
  FItems := TList.Create;
end;

constructor TGtdFilter.Create(_Node: IXMLNode);
begin
  Create(_Node.Attributes['name'], _Node.Attributes['id']);
end;

destructor TGtdFilter.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TGtdFilter.Find(_Node: TGtdNode): boolean;
begin
  Result := (FItems.IndexOf(_Node) <> -1);
end;

function TGtdFilter.Add(_Node: TGtdNode): integer;
begin
  Result := FItems.Count;
  FItems.Add(_Node);
end;

function TGtdFilter.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TGtdFilter.GetItems(_Idx: integer): TGtdNode;
begin
  Result := TGtdNode(FItems[_Idx]);
end;

{ TGtdContainer }

constructor TGtdContainer.Create(const _Name: string);
begin
  inherited Create(_Name);
  FItems := TObjectList.Create;
end;

destructor TGtdContainer.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TGtdContainer.Add(_Node: TGtdNode): integer;
begin
  Result := FItems.Count;
  FItems.Add(_Node)
end;

function TGtdContainer.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TGtdContainer.GetItems(_Idx: integer): TGtdNode;
begin
  Result := FItems[_Idx] as TGtdNode;
end;

{ TPlaces }

constructor TGtdPlaces.Create;
begin
  inherited Create('Places');
end;

{ TLabels }

constructor TGtdLabels.Create;
begin
  inherited Create('Labels');
end;

{ TProjects }

constructor TGtdProjects.Create;
begin
  inherited Create('Projects');
end;

{ TGtdAction }

constructor TGtdAction.Create(_Node: IXMLNode);
begin
  Create(_Node.Attributes['name']);
  if _Node.Attributes['isnext'] = '1' then
    IsNextAction := true;
  if _Node.Attributes['isdone'] = '1' then
    IsDone := true;
end;

function TGtdAction.GetIsDone: boolean;
begin
  Result := FIsDone;
end;

function TGtdAction.GetIsNextAction: boolean;
begin
  Result := FIsNextAction;
end;

procedure TGtdAction.SetIsDone(const _Value: boolean);
begin
  FIsDone := _Value;
end;

procedure TGtdAction.SetIsNextAction(const _Value: boolean);
begin
  FIsNextAction := _Value;
end;

function TGtdProjects.FindName(const _Name: string; out _Project: TGtdProject): boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    _Project := Items[i] as TGtdProject;
    Result := (_Project.Name = _Name);
    if Result then
      exit;
  end;
  Result := false;
end;

{ TGtdProject }

constructor TGtdProject.Create(_Node: IXMLNode; _Places: TGtdPlaces; _Labels: TGtdLabels);
var
  ActionNode: IXMLNode;
  Action: TGtdAction;
  FilterNode: IXMLNode;
  Filter: TGtdFilter;
  FilterId: string;
begin
  Create(_Node.Attributes['name']);

  ActionNode := _Node.ChildNodes.First;
  while Assigned(ActionNode) do begin
    if ActionNode.nodeName = 'action' then begin
      Action := TGtdAction.Create(ActionNode);
      Add(Action);
      FilterNode := ActionNode.ChildNodes.First;
      while Assigned(FilterNode) do begin
        FilterId := FilterNode.Attributes['id'];
        if FilterNode.NodeName = 'label' then begin
          if _Labels.FindId(FilterId, Filter) then
            Filter.Add(Action);
        end else if FilterNode.NodeName = 'place' then begin
          if _Places.FindId(FilterId, Filter) then
            Filter.Add(Action);
        end;
        FilterNode := FilterNode.nextSibling;
      end;
    end;
    ActionNode := ActionNode.nextSibling;
  end;
end;

{ TGtdFilterContainer }

function TGtdFilterContainer.FindId(const _Id: string; out _Filter: TGtdFilter): boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do begin
    _Filter := Items[i] as TGtdFilter;
    Result := (_Filter.Id = _Id);
    if Result then
      exit;
  end;
  Result := false;
end;

end.

