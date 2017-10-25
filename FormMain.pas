unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, Dialogs, ComCtrls, ValEdit, ExtCtrls, Menus,
  TreeFilterEdit, View3DTypes, ViewLayout3D, TaskRunner, Classes, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBoxShowFullClassNames: TCheckBox;
    CheckBoxShowViewIDs: TCheckBox;
    DialogOpenFile: TOpenDialog;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemClipToParent: TMenuItem;
    MenuItemToggleView3D: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemNormalSize: TMenuItem;
    MenuItemZoomZOut: TMenuItem;
    MenuItemZoomZIn: TMenuItem;
    MenuItemZoomOut: TMenuItem;
    MenuItemZoomIn: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemOpenFile: TMenuItem;
    MenuItemOpenWindow: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItemHelp: TMenuItem;
    TreeFilterEdit: TTreeFilterEdit;
    PanelTreeView: TPanel;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    TreeView: TTreeView;
    ValueListEditor: TValueListEditor;
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClipToParentClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemToggleView3DClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemOpenFileClick(Sender: TObject);
    procedure MenuItemOpenWindowClick(Sender: TObject);
    procedure MenuItemZoomInClick(Sender: TObject);
    procedure MenuItemZoomOutClick(Sender: TObject);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure UpdateTreeViewLabels(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeFilterEditAfterFilter(Sender: TObject);
    function TreeFilterEditFilterItem(Item: TObject; out Done: boolean): boolean;
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TreeViewSelectionChanged(Sender: TObject);
  private
    FRootView: TView3D;
    FViewLayout3D: TViewLayout3D;
    FScreenCursor: TCursor;
    FLayoutOpenTask: ILayoutOpenTask;
    function GetTreeNodeText(View: TView3D): string;
    procedure SetRootView(AValue: TView3D);
  protected
    procedure ViewLayout3DActiveViewChanged(Sender: TObject);
    procedure ViewLayout3DVisibleBranchChanged(Sender: TObject);
    procedure UpdateTreeView(RootView: TView3D = nil);
    procedure UpdatePropertyInspector(View: TView3D = nil);
    procedure LayoutOpenTaskError(const Task: ITask; Error: Exception);
    procedure LayoutOpenTaskStarted(const Task: ITask);
    procedure LayoutOpenTaskStopped(const Task: ITask);
    procedure LayoutOpenTaskSuccess(const Task: ITask);
    procedure StartOpenLayout(const Task: TLayoutOpenTask);
    procedure CloseLayout;
    procedure CancelOpenLayout;
    property RootView: TView3D read FRootView write SetRootView;
  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLType, FormOpenWindow, LazUTF8, DumpFileLoader, DeviceWindowLoader;

const
  AppName = 'Androli';
  FormFileCaptionFormat = '%s - ' + AppName;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  FViewLayout3D := TViewLayout3D.Create(Self);
  FViewLayout3D.Parent := Self;
  FViewLayout3D.Align := alClient;
  {$IFDEF DEBUG}
  StartOpenLayout(CreateDumpFileOpenTask('dumps/dump3.uix'));
  {$ENDIF}
  FViewLayout3D.OnActiveViewChanged := @ViewLayout3DActiveViewChanged;
  FViewLayout3D.OnVisibleBranchChanged := @ViewLayout3DVisibleBranchChanged;
  MenuItemToggleView3D.Checked := FViewLayout3D.View3DEnabled;
  MenuItemClipToParent.Checked := FViewLayout3D.ClipBounds;
  SetControlIndex(FViewLayout3D, 0);
end;

procedure TMainForm.TreeFilterEditAfterFilter(Sender: TObject);
begin
  FViewLayout3D.Changed;
end;

function TMainForm.TreeFilterEditFilterItem(Item: TObject; out Done: boolean): boolean;
var
  View: TView3D absolute Item;
begin
  // TODO: I don't really understand how Result/Done should be used here.
  // Just know that the code below does exactly what I want.
  // I figured out the values for Result/Done in each code branch by trial-and-error.
  if TreeFilterEdit.Filter = EmptyStr then
  begin
    View.MatchFilter := False;
    Result := True;
  end
  else if Pos(UTF8LowerCase(TreeFilterEdit.Filter),
    UTF8LowerCase(View.TreeNodeText)) > 0 then
  begin
    View.MatchFilter := True;
    Result := True;
  end
  else
  begin
    View.MatchFilter := False;
    Result := False;
    Done := True;
  end;
end;

procedure TMainForm.TreeViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  Node: TTreeNode;
begin
  Node := TreeView.GetNodeAt(X, Y);
  if Assigned(Node) then
    FViewLayout3D.HighlightedView := TView3D(Node.Data);
end;

procedure TMainForm.TreeViewSelectionChanged(Sender: TObject);
var
  SelectedTreeNode: TTreeNode;
  SelectedView: TView3D;
begin
  SelectedTreeNode := TreeView.Selected;
  if Assigned(SelectedTreeNode) then
  begin
    SelectedView := TView3D(SelectedTreeNode.Data);
    FViewLayout3D.ActiveView := SelectedView;
    UpdatePropertyInspector(SelectedView);
  end
  else
    UpdatePropertyInspector;
end;

function TMainForm.GetTreeNodeText(View: TView3D): string;
var
  S: string;
  I: integer;
begin
  if CheckBoxShowFullClassNames.Checked then
    Result := View.QualifiedClassName
  else
    Result := View.SimpleClassName;

  if CheckBoxShowViewIDs.Checked then
  begin
    S := View.GetProp('mID');
    if S <> EmptyStr then
      Result := S + ': ' + Result;
  end;

  if View.QualifiedClassName = 'android.widget.LinearLayout' then
  begin
    I := View.GetIntProp('measurement:mOrientation', -1);
    if I = 0 then
      Result := Result + ': HORIZONTAL'
    else
    if I = 1 then
      Result := Result + ': VERTICAL';
  end
  else
  begin
    S := View.GetProp('text:mText');
    if S <> EmptyStr then
      Result := Result + ': "' + S + '"';
  end;
end;

procedure TMainForm.SetRootView(AValue: TView3D);
var
  OldRootView: TView3D;
begin
  if AValue = FRootView then
    Exit;

  OldRootView := FRootView;
  FRootView := AValue;

  FViewLayout3D.RootView := FRootView;
  UpdateTreeView(FRootView);
  UpdatePropertyInspector;

  if Assigned(OldRootView) then
    OldRootView.Free;
end;

procedure TMainForm.ViewLayout3DActiveViewChanged(Sender: TObject);
var
  View: TView3D;
  TreeNode: TTreeNode;
begin
  View := TViewLayout3D(Sender).ActiveView;
  if Assigned(View) then
  begin
    TreeNode := TreeView.Items.FindNodeWithData(View);
    // Note that changing the selection on TreeView will cause our
    // TreeViewSelectionChanged to be called, which in turn, will set
    // FViewLayout3D.ActiveView to the new selection.
    // But since the new selection is the same as the old one, the assignment
    // won't have any affect on FLayout3DViewer and the notification loop
    // will stop here.
    // Note also that we don't update the property inspector here because
    // TreeViewSelectionChanged will take care of it.
    if Assigned(TreeNode) then
      TreeNode.Selected := True;
  end
  else
    TreeView.ClearSelection(False);
end;

procedure TMainForm.UpdateTreeView(RootView: TView3D);

  procedure AddView(View: TView3D; Parent: TTreeNode = nil);
  var
    I: integer;
    NewNode: TTreeNode;
  begin
    View.TreeNodeText := GetTreeNodeText(View);
    NewNode := TreeView.Items.AddChildObject(Parent, View.TreeNodeText, View);
    for I := 0 to View.ChildrenCount - 1 do
      AddView(View.Children[I], NewNode);
    NewNode.Expanded := View.Expanded;
  end;

begin
  TreeFilterEdit.Clear;

  if Assigned(RootView) then
  begin
    TreeView.BeginUpdate;
    try
      TreeView.Items.Clear;
      AddView(RootView);
    finally
      TreeView.EndUpdate;
    end;

    TreeFilterEdit.Enabled := True;
    CheckBoxShowViewIDs.Enabled := True;
    CheckBoxShowFullClassNames.Enabled := True;
  end
  else
  begin
    TreeView.Items.Clear;
    TreeFilterEdit.Enabled := False;
    CheckBoxShowViewIDs.Enabled := False;
    CheckBoxShowFullClassNames.Enabled := False;
  end;
end;

procedure TMainForm.UpdatePropertyInspector(View: TView3D);
var
  I: integer;
  PName, PValue: string;
begin
  ValueListEditor.BeginUpdate;
  try
    ValueListEditor.Clear;
    if Assigned(View) then
      for I := 0 to View.GetPropCount - 1 do
      begin
        View.GetPropNameValue(I, PName, PValue);
        ValueListEditor.InsertRow(PName, PValue, False);
      end;
  finally
    ValueListEditor.EndUpdate;
  end;
end;

procedure TMainForm.LayoutOpenTaskError(const Task: ITask; Error: Exception);
begin
  //TODO:
end;

procedure TMainForm.LayoutOpenTaskStarted(const Task: ITask);
begin
  FScreenCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

procedure TMainForm.LayoutOpenTaskStopped(const Task: ITask);
begin
  Screen.Cursor := FScreenCursor;
  FLayoutOpenTask := nil;
end;

procedure TMainForm.LayoutOpenTaskSuccess(const Task: ITask);
begin
  RootView := FLayoutOpenTask.GetResult;
  Caption := Format(FormFileCaptionFormat, [FLayoutOpenTask.DisplayName]);
  MenuItemClose.Enabled := True;
  MenuItemZoomIn.Enabled := True;
  MenuItemZoomOut.Enabled := True;
end;

procedure TMainForm.StartOpenLayout(const Task: TLayoutOpenTask);
begin
  if Assigned(FLayoutOpenTask) then
    FLayoutOpenTask.Cancel;

  with Task do
  begin
    OnStarted := @LayoutOpenTaskStarted;
    OnSuccess := @LayoutOpenTaskSuccess;
    OnError := @LayoutOpenTaskError;
    OnStopped := @LayoutOpenTaskStopped;
    FLayoutOpenTask := Start as ILayoutOpenTask;
  end;
end;

procedure TMainForm.CloseLayout;
begin
  RootView := nil;
  Caption := AppName;
  MenuItemClose.Enabled := False;
  MenuItemZoomIn.Enabled := False;
  MenuItemZoomOut.Enabled := False;
end;

procedure TMainForm.CancelOpenLayout;
begin
  if Assigned(FLayoutOpenTask) then
  begin
    FLayoutOpenTask.Cancel;
    FLayoutOpenTask := nil;
  end;
end;

procedure TMainForm.ViewLayout3DVisibleBranchChanged(Sender: TObject);
begin
  UpdateTreeView(TViewLayout3D(Sender).VisibleBranch);
end;

procedure TMainForm.MenuItemOpenFileClick(Sender: TObject);
begin
  if DialogOpenFile.Execute then
    StartOpenLayout(CreateDumpFileOpenTask(DialogOpenFile.FileName));
end;

procedure TMainForm.MenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  //TODO:
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FRootView) then
    FRootView.Free;

  if Assigned(FLayoutOpenTask) then
    FLayoutOpenTask.Cancel;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Ord(Key) = VK_ESCAPE) then
    CancelOpenLayout;
end;

procedure TMainForm.MenuItemClipToParentClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  MenuItem.Checked := not MenuItem.Checked;
  FViewLayout3D.ClipBounds := MenuItem.Checked;
end;

procedure TMainForm.MenuItemCloseClick(Sender: TObject);
begin
  CloseLayout;
end;

procedure TMainForm.MenuItemToggleView3DClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  MenuItem.Checked := not MenuItem.Checked;
  FViewLayout3D.View3DEnabled := MenuItem.Checked;
end;

procedure TMainForm.MenuItemOpenWindowClick(Sender: TObject);
begin
  with TOpenWindowForm.Create(nil) do
    try
      if ShowModal = mrOk then
        StartOpenLayout(CreateDeviceWindowOpenTask(SelectedDeviceSerial,
          SelectedWindowTitle, SelectedWindowHash));
    finally
      Free;
    end;
end;

procedure TMainForm.MenuItemZoomInClick(Sender: TObject);
begin
  FViewLayout3D.Zoom(1);
end;

procedure TMainForm.MenuItemZoomOutClick(Sender: TObject);
begin
  FViewLayout3D.Zoom(-1);
end;

procedure TMainForm.TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
var
  Root: TView3D;
begin
  Root := TView3D(Node.Data);
  // First checking Node.Deleting is important because this event handler
  // may be called when TTreeView is being destroyed.
  if Node.Deleting or not Root.Expanded then
    Exit;

  FViewLayout3D.Collapse(Root);
end;

procedure TMainForm.TreeViewExpanded(Sender: TObject; Node: TTreeNode);

  procedure Visit(Node: TTreeNode);
  var
    View: TView3D;
  begin
    View := TView3D(Node.Data);
    View.Expanded := True;
    Node := Node.GetFirstChild;
    while Assigned(Node) do
    begin
      if Node.Expanded then
        Visit(Node);
      Node := Node.GetNextSibling;
    end;
  end;

var
  Root: TView3D;
begin
  Root := TView3D(Node.Data);
  if Root.Expanded then
    Exit;

  // Note that although Node is expanded this doesn't mean
  // that everything under it is also expanded.
  // So here we traverse the subtree to determine the expanded nodes and
  // flag the corresponding views appropriately.
  // We need this info in the views because expanded views are animated
  // slightly different than collapsed ones.
  Visit(Node);
  FViewLayout3D.Expand(Root);
end;

procedure TMainForm.UpdateTreeViewLabels(Sender: TObject);
var
  Node: TTreeNode;
  View: TView3D;
begin
  TreeView.BeginUpdate;
  try
    for Node in TreeView.Items do
    begin
      View := TView3D(Node.Data);
      View.TreeNodeText := GetTreeNodeText(View);
      Node.Text := View.TreeNodeText;
    end;
  finally
    TreeView.EndUpdate;
  end;
end;

end.
