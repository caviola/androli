unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, Dialogs, ComCtrls, ValEdit, ExtCtrls, Menus,
  TreeFilterEdit, View3DTypes, ViewLayout3D, Classes;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBoxShowFullClassNames: TCheckBox;
    CheckBoxShowViewIDs: TCheckBox;
    DialogOpenFile: TOpenDialog;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
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
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemOpenFileClick(Sender: TObject);
    procedure MenuItemOpenWindowClick(Sender: TObject);
    procedure MenuItemZoomInClick(Sender: TObject);
    procedure MenuItemZoomOutClick(Sender: TObject);
    procedure UpdateTreeViewLabels(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeFilterEditAfterFilter(Sender: TObject);
    function TreeFilterEditFilterItem(Item: TObject; out Done: boolean): boolean;
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TreeViewSelectionChanged(Sender: TObject);
  private
    FViewLayout3D: TViewLayout3D;
    function GetTreeNodeText(View: TView3D): string;
  protected
    procedure ViewLayout3DActiveViewChanged(Sender: TObject);
    procedure ViewLayout3DVisibleBranchChanged(Sender: TObject);
    procedure UpdateTreeView(RootView: TView3D = nil);
    procedure UpdatePropertyInspector(View: TView3D = nil);
  end;

var
  MainForm: TMainForm;

implementation

uses
  SysUtils, FormOpenWindow, LazUTF8, DumpFileLoader;

const
  AppName = 'Androli';
  FormFileCaptionFormat = '%s - ' + AppName;
  FormWindowCaptionFormat = '%s:%s - ' + AppName;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  RV: TView3D;
begin
  FViewLayout3D := TViewLayout3D.Create(Self);
  with FViewLayout3D do
  begin
    Parent := Self;
    Align := alClient;
    {$IFDEF DEBUG}
    RV := LoadDeviceMonitorDump('dumps/dump3.uix');
    RootView := RV;
    UpdateTreeView(RV);
    UpdatePropertyInspector;
    RotationY := 30;
    ZoomLevel := 0.4;
    {$ENDIF}
    OnActiveViewChanged := @ViewLayout3DActiveViewChanged;
    OnVisibleBranchChanged := @ViewLayout3DVisibleBranchChanged;
  end;
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
    NewNode.Expanded := True;
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

procedure TMainForm.ViewLayout3DVisibleBranchChanged(Sender: TObject);
begin
  UpdateTreeView(TViewLayout3D(Sender).VisibleBranch);
end;

procedure TMainForm.MenuItemOpenFileClick(Sender: TObject);
var
  RootView: TView3D;
begin
  if DialogOpenFile.Execute then
  begin
    RootView := LoadDeviceMonitorDump(DialogOpenFile.FileName);
    FViewLayout3D.RootView := RootView;
    UpdateTreeView(RootView);
    UpdatePropertyInspector;
    Caption := Format(FormFileCaptionFormat, [DialogOpenFile.FileName]);
    MenuItemClose.Enabled := True;
    MenuItemZoomIn.Enabled := True;
    MenuItemZoomOut.Enabled := True;
  end;
end;

procedure TMainForm.MenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  //TODO:
end;

procedure TMainForm.MenuItemCloseClick(Sender: TObject);
begin
  FViewLayout3D.RootView := nil;
  UpdateTreeView;
  Caption := AppName;
  MenuItemClose.Enabled := False;
  MenuItemZoomIn.Enabled := False;
  MenuItemZoomOut.Enabled := False;
end;

procedure TMainForm.MenuItemOpenWindowClick(Sender: TObject);
begin
  with TOpenWindowForm.Create(nil) do
    try
      if ShowModal = mrOk then
      begin
        //TODO: free RootView when loading a new one.
        FViewLayout3D.RootView := RootView;
        UpdateTreeView(RootView);
        UpdatePropertyInspector;
        Self.Caption := Format(FormWindowCaptionFormat,
          [SelectedDeviceSerial, SelectedWindowTitle]);
        MenuItemClose.Enabled := True;
        MenuItemZoomIn.Enabled := True;
        MenuItemZoomOut.Enabled := True;
      end;
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
