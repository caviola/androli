unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, Dialogs, ComCtrls, ValEdit, ExtCtrls, Menus,
  TreeFilterEdit, ViewTypes, LayoutViewer, TaskRunner, Classes, SysUtils, Bookmarks;

type

  { TBookmark }

  TBookmark = class
    ActiveView: TView;
    ActiveBranch: TView;
    OriginX: single;
    OriginY: single;
    RotationX: single;
    RotationY: single;
    ScaleZ: single;
    ZoomLevel: single;
    FilterText: string;
  end;

  { TMainForm }

  TMainForm = class(TForm, IIndexedBookmarkListener)
    CheckBoxShowFullClassNames: TCheckBox;
    CheckBoxShowViewIDs: TCheckBox;
    DialogOpenFile: TOpenDialog;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItemSelectPreviousMatch: TMenuItem;
    MenuItemSelectNextMatch: TMenuItem;
    MenuItemSetActiveBranch: TMenuItem;
    MenuItemToogleTreePanel: TMenuItem;
    MenuItemTogglePropertyInspector: TMenuItem;
    MenuItemResetCamera: TMenuItem;
    MenuItemSelectFirstChild: TMenuItem;
    MenuItemSelectParent: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemSelectNextSibbling: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItemSelectPreviousSibbling: TMenuItem;
    MenuItemFilter: TMenuItem;
    MenuItemShowContent: TMenuItem;
    MenuItemShowWireframes: TMenuItem;
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
    MenuItemClipBounds: TMenuItem;
    MenuItemToggleMode3D: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemDecreaseZ: TMenuItem;
    MenuItemIncreaseZ: TMenuItem;
    MenuItemZoomOut: TMenuItem;
    MenuItemZoomIn: TMenuItem;
    MenuItemLayout: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemOpenFile: TMenuItem;
    MenuItemOpenViewServerWindow: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemHelp: TMenuItem;
    PanelCenter: TPanel;
    PanelPropertyInspector: TPanel;
    TreeFilterEdit: TTreeFilterEdit;
    PanelTreeView: TPanel;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    TreeView: TTreeView;
    ValueListEditor: TValueListEditor;
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure MenuItemResetCameraClick(Sender: TObject);
    procedure MenuItemClearBookmarksClick(Sender: TObject);
    procedure MenuItemClipBoundsClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemFilterClick(Sender: TObject);
    procedure MenuItemSelectNextMatchClick(Sender: TObject);
    procedure MenuItemSelectPreviousMatchClick(Sender: TObject);
    procedure MenuItemSetActiveBranchClick(Sender: TObject);
    procedure MenuItemSelectFirstChildClick(Sender: TObject);
    procedure MenuItemGotoNextBookmarkClick(Sender: TObject);
    procedure MenuItemGotoPreviousBookmarkClick(Sender: TObject);
    procedure MenuItemGotoBookmarkClick(Sender: TObject);
    procedure MenuItemToggleBookmarkClick(Sender: TObject);
    procedure MenuItemSelectNextSibblingClick(Sender: TObject);
    procedure MenuItemOpenViewServerWindowClick(Sender: TObject);
    procedure MenuItemSelectParentClick(Sender: TObject);
    procedure MenuItemSelectPreviousSibblingClick(Sender: TObject);
    procedure MenuItemSetBookmarkClick(Sender: TObject);
    procedure MenuItemShowContentClick(Sender: TObject);
    procedure MenuItemShowWireframesClick(Sender: TObject);
    procedure MenuItemToggleMode3DClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemOpenFileClick(Sender: TObject);
    procedure MenuItemTogglePropertyInspectorClick(Sender: TObject);
    procedure MenuItemToggleTreePanelClick(Sender: TObject);
    procedure MenuItemZoomInClick(Sender: TObject);
    procedure MenuItemZoomOutClick(Sender: TObject);
    procedure TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure TreeViewMouseLeave(Sender: TObject);
    procedure UpdateTreeViewLabels(Sender: TObject);
    procedure UpdateActiveViewMenuItems(ActiveView: TView = nil);
    procedure UpdateFilterMenuItems(const FilterText: string);
    procedure FormCreate(Sender: TObject);
    procedure TreeFilterEditAfterFilter(Sender: TObject);
    function TreeFilterEditFilterItem(Item: TObject; out Done: boolean): boolean;
    procedure TreeViewMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: integer);
    procedure TreeViewSelectionChanged(Sender: TObject);
  private
    FLayout: IViewLayout;
    FLayoutViewer: TLayoutViewer;
    FScreenCursor: TCursor;
    FLayoutLoadTask: ITask;
    FIndexedBookmarkManager: TIndexedBookmarkManager;
    function GetTreeNodeText(View: TView): string;
    procedure SetLayout(AValue: IViewLayout);
  protected
    procedure LayoutChanged;
    procedure LayoutViewerActiveViewChanged(NewView: TView);
    procedure LayoutViewerActiveBranchChanged(NewBranch: TView);
    procedure UpdateTreeView(RootView: TView = nil; SelectedView: TView = nil;
      const FilterText: string = '');
    procedure UpdateTreeViewSelection(AView: TView);
    procedure UpdatePropertyInspector(AView: TView = nil);
    procedure LayoutLoadError(const {%H-}Task: ITask; Error: Exception);
    procedure LayoutLoadStarted(const {%H-}Task: ITask);
    procedure LayoutLoadResult(const {%H-}Task: ITask; TheResult: TViewLayout);
    procedure LayoutLoadStopped(const {%H-}Task: ITask);
    procedure StartLoadLayout(const Task: TLayoutLoadTask);
    procedure CloseLayout;
    procedure CancelLoadLayout;

    // IIndexedBookmarkListener
    function SaveBookmark: TObject;
    procedure RestoreBookmark(Which: TObject);
    procedure OnIndexedBookmarkSet(Index, SetCount, Total: integer);
    procedure OnIndexedBookmarkUnset(Index, SetCount, Total: integer);
  end;

var
  MainForm: TMainForm;

implementation

uses
  LCLType, FormOpenViewServerWindow, LazUTF8, DumpFileLoader,
  ViewServerLoader, LCLProc, Logging;

const
  AppName = 'Androli';
  FormFileCaptionFormat = '%s - ' + AppName;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FLayoutViewer := TLayoutViewer.Create(PanelCenter);
  with FLayoutViewer do
  begin
    Parent := PanelCenter;
    Align := alClient;
    OnActiveViewChanged := @LayoutViewerActiveViewChanged;
    OnActiveBranchChanged := @LayoutViewerActiveBranchChanged;
  end;

  MenuItemToggleMode3D.Checked := FLayoutViewer.Mode3D;
  MenuItemShowWireframes.Checked := FLayoutViewer.ShowWireframes;
  MenuItemShowContent.Checked := FLayoutViewer.ShowContent;

  FIndexedBookmarkManager := TIndexedBookmarkManager.Create(10, Self);

  {$IFDEF DEBUG}
  StartLoadLayout(CreateDumpFileLoadTask('dumps/dump3.uix'));
  {$ELSE}
  SetLayout(nil);
  {$ENDIF}
end;

procedure TMainForm.TreeFilterEditAfterFilter(Sender: TObject);
begin
  // Make sure the active view is one which matches the filter.
  if Assigned(FLayout.ActiveView) then
    if not FLayout.ActiveView.MatchFilter then
      FLayoutViewer.SelectNextMatch;

  UpdateFilterMenuItems(TreeFilterEdit.Filter);
  FLayout.Changed;
end;

function TMainForm.TreeFilterEditFilterItem(Item: TObject; out Done: boolean): boolean;
var
  View: TView absolute Item;
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
    FLayoutViewer.HighlightedView := TView(Node.Data);
end;

procedure TMainForm.TreeViewSelectionChanged(Sender: TObject);
var
  SelectedNode: TTreeNode;
  SelectedView: TView = nil;
begin
  LogEnterMethod('TMainForm.TreeViewSelectionChanged');

  SelectedNode := TreeView.Selected;
  if Assigned(SelectedNode) then
    SelectedView := TView(SelectedNode.Data);

  FLayoutViewer.HighlightedView := nil;
  FLayoutViewer.SetActiveView(SelectedView);
  UpdatePropertyInspector(SelectedView);
  UpdateActiveViewMenuItems(SelectedView);

  LogExitMethod('TMainForm.TreeViewSelectionChanged');
end;

function TMainForm.GetTreeNodeText(View: TView): string;
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

procedure TMainForm.SetLayout(AValue: IViewLayout);
begin
  LogEnterMethod('TMainForm.SetViewLayout');

  FLayout := AValue;
  FLayoutViewer.SetLayout(AValue);

  if Assigned(AValue) then
  begin
    UpdateTreeView(AValue.ActiveBranch);
    Caption := Format(FormFileCaptionFormat, [AValue.Title]);

    MenuItemClose.Enabled := True;
    MenuItemZoomIn.Enabled := True;
    MenuItemZoomOut.Enabled := True;
    MenuItemResetCamera.Enabled := True;
    MenuItemFilter.Enabled := True;

    MenuItemToggleBookmark0.Enabled := True;
    MenuItemToggleBookmark1.Enabled := True;
    MenuItemToggleBookmark2.Enabled := True;
    MenuItemToggleBookmark3.Enabled := True;
    MenuItemToggleBookmark4.Enabled := True;
    MenuItemToggleBookmark5.Enabled := True;
    MenuItemToggleBookmark6.Enabled := True;
    MenuItemToggleBookmark7.Enabled := True;
    MenuItemToggleBookmark8.Enabled := True;
    MenuItemToggleBookmark9.Enabled := True;

    MenuItemSetBookmark.Enabled := True;
  end
  else
  begin
    UpdateTreeView;
    Caption := AppName;

    MenuItemClose.Enabled := False;
    MenuItemZoomIn.Enabled := False;
    MenuItemZoomOut.Enabled := False;
    MenuItemResetCamera.Enabled := False;
    MenuItemFilter.Enabled := False;

    MenuItemToggleBookmark0.Enabled := False;
    MenuItemToggleBookmark1.Enabled := False;
    MenuItemToggleBookmark2.Enabled := False;
    MenuItemToggleBookmark3.Enabled := False;
    MenuItemToggleBookmark4.Enabled := False;
    MenuItemToggleBookmark5.Enabled := False;
    MenuItemToggleBookmark6.Enabled := False;
    MenuItemToggleBookmark7.Enabled := False;
    MenuItemToggleBookmark8.Enabled := False;
    MenuItemToggleBookmark9.Enabled := False;

    MenuItemSetBookmark.Enabled := False;
  end;

  FIndexedBookmarkManager.Clear;
  UpdatePropertyInspector;
  UpdateActiveViewMenuItems;

  LogExitMethod('TMainForm.SetViewLayout');
end;

procedure TMainForm.MenuItemGotoBookmarkClick(Sender: TObject);
begin
  FIndexedBookmarkManager.Go(TMenuItem(Sender).Tag);
end;

procedure TMainForm.LayoutViewerActiveBranchChanged(NewBranch: TView);
begin
  if NewBranch = FLayout.RootView then
    UpdateTreeView(NewBranch) // don't select RootView
  else
    UpdateTreeView(NewBranch, NewBranch);

  UpdateActiveViewMenuItems(FLayout.ActiveView);
end;

procedure TMainForm.LayoutChanged;
begin
  FLayoutViewer.Invalidate;
end;

procedure TMainForm.MenuItemToggleBookmarkClick(Sender: TObject);
begin
  FIndexedBookmarkManager.Toggle(TMenuItem(Sender).Tag);
end;

procedure TMainForm.LayoutViewerActiveViewChanged(NewView: TView);
begin
  UpdateTreeViewSelection(NewView);
end;

procedure TMainForm.UpdateTreeView(RootView: TView; SelectedView: TView;
  const FilterText: string);

  procedure AddView(View: TView; Parent: TTreeNode = nil);
  var
    ViewChild: TView;
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

  TreeView.BeginUpdate;
  try
    TreeView.Items.Clear;

    if Assigned(RootView) then
    begin
      AddView(RootView);
      TreeFilterEdit.Enabled := True;
      CheckBoxShowViewIDs.Enabled := True;
      CheckBoxShowFullClassNames.Enabled := True;
    end
    else
    begin
      TreeFilterEdit.Enabled := False;
      CheckBoxShowViewIDs.Enabled := False;
      CheckBoxShowFullClassNames.Enabled := False;
    end;

    // Filter TreeView immediately instead of waiting for OnIdle
    // to avoid flickering.
    TreeFilterEdit.ForceFilter(FilterText);
    TreeFilterEdit.Text := FilterText;

    UpdateFilterMenuItems(FilterText);
    UpdateTreeViewSelection(SelectedView);
  finally
    TreeView.EndUpdate;
  end;

  LogExitMethod('TMainForm.UpdateTreeView');
end;

procedure TMainForm.UpdatePropertyInspector(AView: TView);
var
  I: integer;
  PName, PValue: string;
begin
  Log('TMainForm.UpdatePropertyInspector %s', [DbgS(AView)]);

  ValueListEditor.BeginUpdate;
  try
    ValueListEditor.Clear;
    if Assigned(AView) then
      for I := 0 to AView.GetPropCount - 1 do
      begin
        AView.GetPropNameValue(I, PName, PValue);
        ValueListEditor.InsertRow(PName, PValue, False);
      end;
  finally
    ValueListEditor.EndUpdate;
  end;
end;

procedure TMainForm.LayoutLoadError(const Task: ITask; Error: Exception);
begin
  LogException('TMainForm.LayoutLoadError', Error);
end;

procedure TMainForm.LayoutLoadStarted(const Task: ITask);
begin
  FScreenCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

procedure TMainForm.LayoutLoadResult(const Task: ITask; TheResult: TViewLayout);
begin
  TheResult.OnChange := @LayoutChanged;
  TheResult.SetClipBounds(MenuItemClipBounds.Checked);
  SetLayout(TheResult);
end;

procedure TMainForm.LayoutLoadStopped(const Task: ITask);
begin
  Screen.Cursor := FScreenCursor;
  FLayoutLoadTask := nil;
end;

procedure TMainForm.StartLoadLayout(const Task: TLayoutLoadTask);
begin
  LogEnterMethod('TMainForm.StartOpenLayout');

  if Assigned(FLayoutLoadTask) then
    FLayoutLoadTask.Cancel;

  with Task do
  begin
    OnStarted := @LayoutLoadStarted;
    OnResult := @LayoutLoadResult;
    OnError := @LayoutLoadError;
    OnStopped := @LayoutLoadStopped;
    FLayoutLoadTask := Start;
  end;

  LogExitMethod('TMainForm.StartOpenLayout');
end;

procedure TMainForm.OnIndexedBookmarkSet(Index, SetCount, Total: integer);
begin
  Log('TMainForm.OnIndexedBookmarkSet: Index=%d, SetCount=%d, Total=%d',
    [Index, SetCount, Total]);

  case Index of
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
    else;
  end;

  MenuItemClearBookmarks.Enabled := True;
  MenuItemGotoNextBookmark.Enabled := True;
  MenuItemGotoPreviousBookmark.Enabled := True;

  MenuItemSetBookmark.Enabled := SetCount <> Total;
end;

procedure TMainForm.OnIndexedBookmarkUnset(Index, SetCount, Total: integer);
begin
  Log('TMainForm.OnIndexedBookmarkUnset: Index=%d, SetCount=%d, Total=%d',
    [Index, SetCount, Total]);

  case Index of
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
    else;
  end;

  if SetCount = 0 then
  begin
    MenuItemClearBookmarks.Enabled := False;
    MenuItemGotoNextBookmark.Enabled := False;
    MenuItemGotoPreviousBookmark.Enabled := False;
  end;

  MenuItemSetBookmark.Enabled := SetCount <> Total;
end;

procedure TMainForm.CloseLayout;
begin
  SetLayout(nil);
end;

procedure TMainForm.CancelLoadLayout;
begin
  LogEnterMethod('TMainForm.CancelLoadLayout');

  if Assigned(FLayoutLoadTask) then
  begin
    FLayoutLoadTask.Cancel;
    FLayoutLoadTask := nil;
  end;

  LogExitMethod('TMainForm.CancelLoadLayout');
end;

procedure TMainForm.UpdateTreeViewSelection(AView: TView);
var
  TreeNode: TTreeNode;
begin
  if Assigned(AView) then
  begin
    TreeNode := TreeView.Items.FindNodeWithData(AView);
    // Note that changing the selection on TreeView will cause our
    // TreeViewSelectionChanged to be called, which in turn, will set
    // FLayoutViewer.ActiveView to the new selection.
    // Note also that we don't update the property inspector here because
    // TreeViewSelectionChanged will take care of it.
    if Assigned(TreeNode) then
      TreeNode.Selected := True;
  end
  else
    TreeView.ClearSelection(False);
end;

function TMainForm.SaveBookmark: TObject;
begin
  Result := TBookmark.Create;
  with TBookmark(Result) do
  begin
    ActiveView := FLayout.ActiveView;
    ActiveBranch := FLayout.ActiveBranch;
    OriginX := FLayoutViewer.OriginX;
    OriginY := FLayoutViewer.OriginY;
    RotationX := FLayoutViewer.RotationX;
    RotationY := FLayoutViewer.RotationY;
    ScaleZ := FLayoutViewer.ScaleZ;
    ZoomLevel := FLayoutViewer.ZoomLevel;
    FilterText := TreeFilterEdit.Text;
  end;

  Log('TMainForm.SaveBookmark: Result=%s', [DbgS(Result)]);
end;

procedure TMainForm.RestoreBookmark(Which: TObject);
begin
  LogEnterMethod('TMainForm.RestoreBookmark');

  with TBookmark(Which) do
  begin
    FLayoutViewer.SetActiveBranch(ActiveBranch);
    FLayoutViewer.HighlightedView := nil;
    FLayoutViewer.OriginX := OriginX;
    FLayoutViewer.OriginY := OriginY;
    FLayoutViewer.RotationX := RotationX;
    FLayoutViewer.RotationY := RotationY;
    FLayoutViewer.ScaleZ := ScaleZ;
    FLayoutViewer.ZoomLevel := ZoomLevel;
    UpdateTreeView(ActiveBranch, ActiveView, FilterText);
  end;

  LogExitMethod('TMainForm.RestoreBookmark');
end;

procedure TMainForm.MenuItemOpenFileClick(Sender: TObject);
begin
  if DialogOpenFile.Execute then
    StartLoadLayout(CreateDumpFileLoadTask(DialogOpenFile.FileName));
end;

procedure TMainForm.MenuItemTogglePropertyInspectorClick(Sender: TObject);
begin
  PanelPropertyInspector.Visible := not PanelPropertyInspector.Visible;
end;

procedure TMainForm.MenuItemToggleTreePanelClick(Sender: TObject);
begin
  PanelTreeView.Visible := not PanelTreeView.Visible;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MenuItemResetCameraClick(Sender: TObject);
begin
  FLayoutViewer.ResetCamera(True);
end;

procedure TMainForm.MenuItemClearBookmarksClick(Sender: TObject);
begin
  FIndexedBookmarkManager.Clear;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FIndexedBookmarkManager.Free;

  FLayout := nil;

  if Assigned(FLayoutLoadTask) then
    FLayoutLoadTask.Cancel;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Ord(Key) = VK_ESCAPE) then
    CancelLoadLayout;
end;

procedure TMainForm.MenuItemClipBoundsClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  MenuItem.Checked := not MenuItem.Checked;
  if Assigned(FLayout) then
    FLayout.ClipBounds := MenuItem.Checked;
end;

procedure TMainForm.MenuItemCloseClick(Sender: TObject);
begin
  CloseLayout;
end;

procedure TMainForm.MenuItemFilterClick(Sender: TObject);
begin
  PanelTreeView.Visible := True;
  if TreeFilterEdit.Enabled then
  begin
    TreeFilterEdit.SetFocus;
    TreeFilterEdit.SelectAll;
  end;
end;

procedure TMainForm.MenuItemSelectNextMatchClick(Sender: TObject);
var
  Match: TView;
begin
  Match := FLayoutViewer.SelectNextMatch;
  if Assigned(Match) then
    UpdateTreeViewSelection(Match);
end;

procedure TMainForm.MenuItemSelectPreviousMatchClick(Sender: TObject);
var
  Match: TView;
begin
  Match := FLayoutViewer.SelectPreviousMatch;
  if Assigned(Match) then
    UpdateTreeViewSelection(Match);
end;

procedure TMainForm.MenuItemSetActiveBranchClick(Sender: TObject);
var
  ActiveView: TView;
begin
  ActiveView := FLayout.ActiveView;
  FLayoutViewer.SetActiveBranch(ActiveView);
  UpdateTreeView(ActiveView, ActiveView);
  UpdateActiveViewMenuItems(ActiveView);
  FLayoutViewer.ResetCamera(False);
end;

procedure TMainForm.MenuItemSelectFirstChildClick(Sender: TObject);
begin
  if FLayoutViewer.SetActiveView(FLayout.ActiveView.FirstChild) then
    UpdateTreeViewSelection(FLayout.ActiveView);
end;

procedure TMainForm.MenuItemGotoNextBookmarkClick(Sender: TObject);
var
  I: integer;
begin
  I := FIndexedBookmarkManager.GoNext;
  Log('TMainForm.MenuItemGotoNextBookmarkClick: GoNext=%d', [I]);
end;

procedure TMainForm.MenuItemGotoPreviousBookmarkClick(Sender: TObject);
var
  I: integer;
begin
  I := FIndexedBookmarkManager.GoPrevious;
  Log('TMainForm.MenuItemGotoPreviousBookmarkClick: GoPrevious=%d', [I]);
end;

procedure TMainForm.MenuItemSelectNextSibblingClick(Sender: TObject);
begin
  if FLayoutViewer.SetActiveView(FLayout.ActiveView.NextSibbling) then
    UpdateTreeViewSelection(FLayout.ActiveView);
end;

procedure TMainForm.MenuItemSetBookmarkClick(Sender: TObject);
var
  I: integer;
begin
  I := FIndexedBookmarkManager.SetFree;
  Log('TMainForm.MenuItemSetBookmarkClick: SetFree=%d', [I]);
end;

procedure TMainForm.MenuItemShowContentClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  MenuItem.Checked := not MenuItem.Checked;
  FLayoutViewer.ShowContent := MenuItem.Checked;
end;

procedure TMainForm.MenuItemShowWireframesClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  MenuItem.Checked := not MenuItem.Checked;
  FLayoutViewer.ShowWireframes := MenuItem.Checked;
end;

procedure TMainForm.MenuItemToggleMode3DClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  MenuItem.Checked := not MenuItem.Checked;
  FLayoutViewer.Mode3D := MenuItem.Checked;
end;

procedure TMainForm.MenuItemOpenViewServerWindowClick(Sender: TObject);
begin
  with TOpenViewServerWindowForm.Create(nil) do
    try
      if ShowModal = mrOk then
        StartLoadLayout(CreateViewServerWindowLoadTask(SelectedDeviceSerial,
          SelectedWindowTitle, SelectedWindowHash));
    finally
      Free;
    end;
end;

procedure TMainForm.MenuItemSelectParentClick(Sender: TObject);
begin
  if FLayoutViewer.SetActiveView(FLayout.ActiveView.Parent) then
    UpdateTreeViewSelection(FLayout.ActiveView);
end;

procedure TMainForm.MenuItemSelectPreviousSibblingClick(Sender: TObject);
begin
  if FLayoutViewer.SetActiveView(FLayout.ActiveView.PrevSibbling) then
    UpdateTreeViewSelection(FLayout.ActiveView);
end;

procedure TMainForm.MenuItemZoomInClick(Sender: TObject);
begin
  FLayoutViewer.Zoom(1);
end;

procedure TMainForm.MenuItemZoomOutClick(Sender: TObject);
begin
  FLayoutViewer.Zoom(-1);
end;

procedure TMainForm.TreeViewCollapsed(Sender: TObject; Node: TTreeNode);
var
  Root: TView;
begin
  Root := TView(Node.Data);
  // First checking Node.Deleting is important because this event handler
  // may be called when TTreeView is being destroyed.
  if Node.Deleting or not Root.Expanded then
    Exit;

  FLayoutViewer.Collapse(Root);
end;

procedure TMainForm.TreeViewExpanded(Sender: TObject; Node: TTreeNode);

  procedure Visit(Node: TTreeNode);
  var
    View: TView;
  begin
    View := TView(Node.Data);
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
  Root: TView;
begin
  Root := TView(Node.Data);
  if Root.Expanded then
    Exit;

  // Note that although Node is expanded this doesn't mean
  // that everything under it is also expanded.
  // So here we traverse the subtree to determine the expanded nodes and
  // flag the corresponding views appropriately.
  // We need this info in the views because expanded views are animated
  // slightly different than collapsed ones.
  Visit(Node);
  FLayoutViewer.Expand(Root);
end;

procedure TMainForm.TreeViewMouseLeave(Sender: TObject);
begin
  FLayoutViewer.HighlightedView := nil;
end;

procedure TMainForm.UpdateTreeViewLabels(Sender: TObject);
var
  Node: TTreeNode;
  View: TView;
begin
  TreeView.BeginUpdate;
  try
    for Node in TreeView.Items do
    begin
      View := TView(Node.Data);
      View.TreeNodeText := GetTreeNodeText(View);
      Node.Text := View.TreeNodeText;
    end;
  finally
    TreeView.EndUpdate;
  end;
end;

procedure TMainForm.UpdateActiveViewMenuItems(ActiveView: TView);
begin
  if Assigned(ActiveView) then
  begin
    MenuItemSelectFirstChild.Enabled := Assigned(ActiveView.FirstChild);
    MenuItemSelectParent.Enabled :=
      Assigned(ActiveView.Parent) and (ActiveView <> FLayout.ActiveBranch);

    MenuItemSetActiveBranch.Enabled := MenuItemSelectParent.Enabled;

    if ActiveView = FLayout.ActiveBranch then
    begin
      // ActiveBranch sibblings are not displayed to the user.
      MenuItemSelectPreviousSibbling.Enabled := False;
      MenuItemSelectNextSibbling.Enabled := False;
    end
    else if ActiveView.NextSibbling <> ActiveView then // has sibblings?
    begin
      MenuItemSelectPreviousSibbling.Enabled := True;
      MenuItemSelectNextSibbling.Enabled := True;
    end;
  end
  else
  begin
    // Disable items as ActiveView=nil.
    MenuItemSelectPreviousSibbling.Enabled := False;
    MenuItemSelectNextSibbling.Enabled := False;
    MenuItemSelectParent.Enabled := False;
    MenuItemSelectFirstChild.Enabled := False;
    MenuItemSetActiveBranch.Enabled := False;
  end;
end;

procedure TMainForm.UpdateFilterMenuItems(const FilterText: string);
begin
  if FilterText = EmptyStr then
  begin
    MenuItemSelectNextMatch.Enabled := False;
    MenuItemSelectPreviousMatch.Enabled := False;
  end
  else
  begin
    MenuItemSelectNextMatch.Enabled := True;
    MenuItemSelectPreviousMatch.Enabled := True;
  end;
end;

end.
