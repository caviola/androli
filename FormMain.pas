unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, Dialogs, ComCtrls, ValEdit, ExtCtrls, Menus,
  TreeFilterEdit, View3DTypes, ViewLayout3D, TaskRunner, Classes, SysUtils, Bookmarks;

type

  { TBookmark }

  TBookmark = class
    ActiveView: TView3D;
    ActiveBranch: TView3D;
    OriginX: single;
    OriginY: single;
    RotationX: single;
    RotationY: single;
    ScaleZ: single;
    ZoomLevel: single;
    TreeFilter: string;
  end;

  { TMainForm }

  TMainForm = class(TForm, IIndexedBookmarkListener)
    CheckBoxShowFullClassNames: TCheckBox;
    CheckBoxShowViewIDs: TCheckBox;
    DialogOpenFile: TOpenDialog;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItemToggleBookmark4: TMenuItem;
    MenuItemToggleBookmark5: TMenuItem;
    MenuItemToggleBookmark6: TMenuItem;
    MenuItemToggleBookmark7: TMenuItem;
    MenuItemToggleBookmark8: TMenuItem;
    MenuItemToggleBookmark9: TMenuItem;
    MenuItemClearBookmarks: TMenuItem;
    MenuItemSetBookmark: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemToggleBookmark0: TMenuItem;
    MenuItemToggleBookmark1: TMenuItem;
    MenuItemToggleBookmark2: TMenuItem;
    MenuItemToggleBookmark3: TMenuItem;
    MenuItemGotoNextBookmark: TMenuItem;
    MenuItemGotoPreviousBookmark: TMenuItem;
    MenuItemGotoBookmark5: TMenuItem;
    MenuItemGotoBookmark6: TMenuItem;
    MenuItemGotoBookmark7: TMenuItem;
    MenuItemGotoBookmark8: TMenuItem;
    MenuItemGotoBookmark9: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemGotoBookmark0: TMenuItem;
    MenuItemGotoBookmark1: TMenuItem;
    MenuItemGotoBookmark2: TMenuItem;
    MenuItemGotoBookmark3: TMenuItem;
    MenuItemGotoBookmark4: TMenuItem;
    MenuItemGotoBookmark: TMenuItem;
    MenuItemSearch: TMenuItem;
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
    procedure MenuItemClearBookmarksClick(Sender: TObject);
    procedure MenuItemClipToParentClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemGotoNextBookmarkClick(Sender: TObject);
    procedure MenuItemGotoPreviousBookmarkClick(Sender: TObject);
    procedure MenuItemSetBookmarkClick(Sender: TObject);
    procedure MenuItemToggleView3DClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemOpenFileClick(Sender: TObject);
    procedure MenuItemOpenWindowClick(Sender: TObject);
    procedure MenuItemZoomInClick(Sender: TObject);
    procedure MenuItemZoomOutClick(Sender: TObject);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewMouseLeave(Sender: TObject);
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
    FIndexedBookmarkManager: TIndexedBookmarkManager;
    function GetTreeNodeText(View: TView3D): string;
    procedure SetRootView(AValue: TView3D);
  protected
    procedure GotoBookmarkHandler(Sender: TObject);
    procedure ToggleBookmarkHandler(Sender: TObject);
    procedure ViewLayout3DActiveViewChanged(Sender: TObject);
    procedure ViewLayout3DActiveBranchChanged(Sender: TObject);
    procedure UpdateTreeView(ARootView: TView3D = nil);
    procedure UpdatePropertyInspector(View: TView3D = nil);
    procedure LayoutOpenTaskError(const Task: ITask; Error: Exception);
    procedure LayoutOpenTaskStarted(const Task: ITask);
    procedure LayoutOpenTaskStopped(const Task: ITask);
    procedure LayoutOpenTaskSuccess(const Task: ITask);
    procedure StartOpenLayout(const Task: TLayoutOpenTask);
    procedure CloseLayout;
    procedure CancelOpenLayout;
    property RootView: TView3D read FRootView write SetRootView;

    // IIndexedBookmarkListener
    procedure OnBookmarkSet(I: integer);
    procedure OnBookmarkUnset(I: integer);
    function SaveBookmark: TObject;
    procedure RestoreBookmark(Which: TObject);
  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLType, FormOpenWindow, LazUTF8, DumpFileLoader, DeviceWindowLoader, LCLProc, Logging;

const
  AppName = 'Androli';
  FormFileCaptionFormat = '%s - ' + AppName;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FViewLayout3D := TViewLayout3D.Create(Self);
  FViewLayout3D.Parent := Self;
  FViewLayout3D.Align := alClient;
  FViewLayout3D.OnActiveViewChanged := @ViewLayout3DActiveViewChanged;
  FViewLayout3D.OnActiveBranchChanged := @ViewLayout3DActiveBranchChanged;
  {$IFDEF DEBUG}
  StartOpenLayout(CreateDumpFileOpenTask('dumps/dump3.uix'));
  {$ENDIF}
  SetControlIndex(FViewLayout3D, 0);

  MenuItemToggleView3D.Checked := FViewLayout3D.View3DEnabled;
  MenuItemClipToParent.Checked := FViewLayout3D.ClipBounds;

  KeyPreview := True;

  FIndexedBookmarkManager := TIndexedBookmarkManager.Create(10, Self);

  MenuItemToggleBookmark0.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark1.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark2.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark3.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark4.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark5.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark6.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark7.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark8.OnClick := @ToggleBookmarkHandler;
  MenuItemToggleBookmark9.OnClick := @ToggleBookmarkHandler;

  MenuItemGotoBookmark0.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark1.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark2.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark3.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark4.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark5.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark6.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark7.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark8.OnClick := @GotoBookmarkHandler;
  MenuItemGotoBookmark9.OnClick := @GotoBookmarkHandler;
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
  else if Pos(LazUTF8.UTF8LowerCase(TreeFilterEdit.Filter),
    LazUTF8.UTF8LowerCase(View.TreeNodeText)) > 0 then
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
  LogEnterMethod('TMainForm.TreeViewSelectionChanged');

  SelectedTreeNode := TreeView.Selected;
  if Assigned(SelectedTreeNode) then
  begin
    SelectedView := TView3D(SelectedTreeNode.Data);
    FViewLayout3D.ActiveView := SelectedView;
    UpdatePropertyInspector(SelectedView);
  end
  else
    UpdatePropertyInspector;

  LogExitMethod('TMainForm.TreeViewSelectionChanged');
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

  LogEnterMethod('TMainForm.SetRootView');

  OldRootView := FRootView;
  FRootView := AValue;

  FViewLayout3D.RootView := FRootView;
  UpdateTreeView(FRootView);
  UpdatePropertyInspector;

  if Assigned(OldRootView) then
    OldRootView.Free;

  LogExitMethod('TMainForm.SetRootView');
end;

procedure TMainForm.GotoBookmarkHandler(Sender: TObject);
begin
  FIndexedBookmarkManager.Go(TMenuItem(Sender).Tag);
end;

procedure TMainForm.ToggleBookmarkHandler(Sender: TObject);
begin
  FIndexedBookmarkManager.Toggle(TMenuItem(Sender).Tag);
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

procedure TMainForm.UpdateTreeView(ARootView: TView3D);

  procedure AddView(View: TView3D; Parent: TTreeNode = nil);
  var
    ViewChild: TView3D;
    NewNode: TTreeNode;
  begin
    View.TreeNodeText := GetTreeNodeText(View);
    NewNode := TreeView.Items.AddChildObject(Parent, View.TreeNodeText, View);

    ViewChild := View.FirstChild;
    if Assigned(ViewChild) then
      repeat
        AddView(ViewChild, NewNode);
        ViewChild := ViewChild.NextSibbling;
      until ViewChild = View.FirstChild;

    NewNode.Expanded := View.Expanded;
  end;

begin
  LogEnterMethod('TMainForm.UpdateTreeView');

  TreeFilterEdit.Clear;

  if Assigned(RootView) then
  begin
    TreeView.BeginUpdate;
    try
      TreeView.Items.Clear;
      AddView(ARootView);
      TreeFilterEdit.Enabled := True;
      // Filter TreeView immediately.
      TreeFilterEdit.ForceFilter(EmptyStr);
      TreeFilterEdit.Text := EmptyStr;
    finally
      TreeView.EndUpdate;
    end;

    CheckBoxShowViewIDs.Enabled := True;
    CheckBoxShowFullClassNames.Enabled := True;
  end
  else
  begin
    TreeView.BeginUpdate;
    try
      TreeView.Items.Clear;
      // Filter TreeView immediately.
      TreeFilterEdit.ForceFilter(EmptyStr);
      TreeFilterEdit.Text := EmptyStr;
      TreeFilterEdit.Enabled := False;
    finally
      TreeView.EndUpdate;
    end;

    CheckBoxShowViewIDs.Enabled := False;
    CheckBoxShowFullClassNames.Enabled := False;
  end;

  LogExitMethod('TMainForm.UpdateTreeView');
end;

procedure TMainForm.UpdatePropertyInspector(View: TView3D);
var
  I: integer;
  PName, PValue: string;
begin
  Log('TMainForm.UpdatePropertyInspector %s', [DbgS(View)]);

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
  LogException('TMainForm.LayoutOpenTaskError', Error);
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
  LogEnterMethod('TMainForm.LayoutOpenTaskSuccess');

  RootView := FLayoutOpenTask.GetResult;
  Caption := Format(FormFileCaptionFormat, [FLayoutOpenTask.DisplayName]);
  MenuItemClose.Enabled := True;
  MenuItemZoomIn.Enabled := True;
  MenuItemZoomOut.Enabled := True;
  FIndexedBookmarkManager.Clear;

  LogExitMethod('TMainForm.LayoutOpenTaskSuccess');
end;

procedure TMainForm.StartOpenLayout(const Task: TLayoutOpenTask);
begin
  LogEnterMethod('TMainForm.StartOpenLayout');

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

  LogExitMethod('TMainForm.StartOpenLayout');
end;

procedure TMainForm.OnBookmarkSet(I: integer);
begin
  case I of
    0: MenuItemGotoBookmark0.Enabled := True;
    1: MenuItemGotoBookmark1.Enabled := True;
    2: MenuItemGotoBookmark2.Enabled := True;
    3: MenuItemGotoBookmark3.Enabled := True;
    4: MenuItemGotoBookmark4.Enabled := True;
    5: MenuItemGotoBookmark5.Enabled := True;
    6: MenuItemGotoBookmark6.Enabled := True;
    7: MenuItemGotoBookmark7.Enabled := True;
    8: MenuItemGotoBookmark8.Enabled := True;
    9: MenuItemGotoBookmark9.Enabled := True;
  end;
end;

procedure TMainForm.OnBookmarkUnset(I: integer);
begin
  case I of
    0: MenuItemGotoBookmark0.Enabled := False;
    1: MenuItemGotoBookmark1.Enabled := False;
    2: MenuItemGotoBookmark2.Enabled := False;
    3: MenuItemGotoBookmark3.Enabled := False;
    4: MenuItemGotoBookmark4.Enabled := False;
    5: MenuItemGotoBookmark5.Enabled := False;
    6: MenuItemGotoBookmark6.Enabled := False;
    7: MenuItemGotoBookmark7.Enabled := False;
    8: MenuItemGotoBookmark8.Enabled := False;
    9: MenuItemGotoBookmark9.Enabled := False;
  end;
end;

procedure TMainForm.CloseLayout;
begin
  LogEnterMethod('TMainForm.CloseLayout');

  RootView := nil;
  Caption := AppName;
  MenuItemClose.Enabled := False;
  MenuItemZoomIn.Enabled := False;
  MenuItemZoomOut.Enabled := False;
  FIndexedBookmarkManager.Clear;

  LogExitMethod('TMainForm.CloseLayout');
end;

procedure TMainForm.CancelOpenLayout;
begin
  LogEnterMethod('TMainForm.CancelOpenLayout');

  if Assigned(FLayoutOpenTask) then
  begin
    FLayoutOpenTask.Cancel;
    FLayoutOpenTask := nil;
  end;

  LogExitMethod('TMainForm.CancelOpenLayout');
end;

function TMainForm.SaveBookmark: TObject;
begin
  Result := TBookmark.Create;
  with TBookmark(Result) do
  begin
    ActiveView := FViewLayout3D.ActiveView;
    ActiveBranch := FViewLayout3D.ActiveBranch;
    OriginX := FViewLayout3D.OriginX;
    OriginY := FViewLayout3D.OriginY;
    RotationX := FViewLayout3D.RotationX;
    RotationY := FViewLayout3D.RotationY;
    ScaleZ := FViewLayout3D.ScaleZ;
    ZoomLevel := FViewLayout3D.ZoomLevel;
    TreeFilter := TreeFilterEdit.Text;
  end;

  Log('TMainForm.SaveBookmark: Result=%s', [DbgS(Result)]);
end;

procedure TMainForm.RestoreBookmark(Which: TObject);
begin
  LogEnterMethod('TMainForm.RestoreBookmark');

  with TBookmark(Which) do
  begin
    // The following properties are updated in a BeginUpdate/EndUpdate block
    // because they all affect TreeView.
    TreeView.BeginUpdate;
    try
      FViewLayout3D.ActiveBranch := ActiveBranch;
      // We first use ForceFilter so that TreeView is filtered immediately.
      // Otherwise we'll have flickering because filtering by default
      // is done in a TApplication.OnIdle handler.
      TreeFilterEdit.ForceFilter(TreeFilter);
      // This won't have any effect on filtering because Text
      // will be equal to the filter set above.
      TreeFilterEdit.Text := TreeFilter;
    finally
      TreeView.EndUpdate;
    end;

    FViewLayout3D.ActiveView := ActiveView;
    FViewLayout3D.HighlightedView := nil;
    FViewLayout3D.OriginX := OriginX;
    FViewLayout3D.OriginY := OriginY;
    FViewLayout3D.RotationX := RotationX;
    FViewLayout3D.RotationY := RotationY;
    FViewLayout3D.ScaleZ := ScaleZ;
    FViewLayout3D.ZoomLevel := ZoomLevel;
  end;

  LogExitMethod('TMainForm.RestoreBookmark');
end;

procedure TMainForm.ViewLayout3DActiveBranchChanged(Sender: TObject);
begin
  UpdateTreeView(TViewLayout3D(Sender).ActiveBranch);
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

procedure TMainForm.MenuItemClearBookmarksClick(Sender: TObject);
begin
  FIndexedBookmarkManager.Clear;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FIndexedBookmarkManager.Free;

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

procedure TMainForm.MenuItemGotoNextBookmarkClick(Sender: TObject);
begin
  FIndexedBookmarkManager.GoNext;
end;

procedure TMainForm.MenuItemGotoPreviousBookmarkClick(Sender: TObject);
begin
  FIndexedBookmarkManager.GoPrevious;
end;

procedure TMainForm.MenuItemSetBookmarkClick(Sender: TObject);
begin
  FIndexedBookmarkManager.SetFree;
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

procedure TMainForm.TreeViewMouseLeave(Sender: TObject);
begin
  FViewLayout3D.HighlightedView := nil;
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
