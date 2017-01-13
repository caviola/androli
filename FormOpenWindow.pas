unit FormOpenWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, AndroidDebugBridge, View3DTypes, ViewServerClient;

type

  { TOpenWindowForm }

  TOpenWindowForm = class(TForm)
    ButtonCancel: TButton;
    ButtonOpen: TButton;
    ButtonRefreshWindows: TButton;
    ButtonRefreshDevices: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListBoxDevices: TListBox;
    ListBoxWindows: TListBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonRefreshDevicesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ListBoxDevicesSelectionChange(Sender: TObject; User: boolean);
    procedure ListBoxWindowsDblClick(Sender: TObject);
    procedure ListBoxWindowsSelectionChange(Sender: TObject; User: boolean);
  private
    FRootView: TView3D;
    FAdbInterface: TAdbInterface;
    FSelectedDevice: TDeviceInterface;
    FDumpWindowDevice: TDeviceInterface;
    FWindowHashes: array of string;
    function GetSelectedDeviceSerial: string;
    function GetSelectedWindowHash: string;
    function GetSelectedWindowTitle: string;
    procedure SetSelectedDevice(V: TDeviceInterface);
    procedure SetWindowList(const WindowList: TWindowManagerEntryArray);
  protected
    procedure AdbDeviceListComplete(Sender: TObject;
      const DeviceList: TAdbDeviceEntryArray);
    procedure AdbDeviceListError(Sender: TObject);
    procedure DeviceWindowDumpCancel(Sender: TObject);
    procedure DeviceWindowDumpComplete(Sender: TDeviceInterface; RootView: TView3D);

    procedure DeviceWindowListComplete(Sender: TDeviceInterface;
      const WindowList: TWindowManagerEntryArray);
    procedure DeviceWindowListError(Sender: TObject);
    property SelectedDevice: TDeviceInterface
      read FSelectedDevice write SetSelectedDevice;
    property WindowList: TWindowManagerEntryArray write SetWindowList;
    property SelectedWindowHash: string read GetSelectedWindowHash;
  public
    property RootView: TView3D read FRootView;
    property SelectedWindowTitle: string read GetSelectedWindowTitle;
    property SelectedDeviceSerial: string read GetSelectedDeviceSerial;
  end;

implementation

{$R *.lfm}

const
  sClose = '&Close';
  sCancel = '&Cancel';

procedure FreeListBoxObjects(ListBox: TCustomListBox); inline;
var
  I: integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
    ListBox.Items.Objects[I].Free;
end;

{ TOpenWindowForm }

procedure TOpenWindowForm.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  FAdbInterface := TAdbInterface.Create;
  FAdbInterface.OnDeviceListComplete := @AdbDeviceListComplete;
  FAdbInterface.OnDeviceListError := @AdbDeviceListError;
  FAdbInterface.GetDeviceList;
  ButtonRefreshDevices.Enabled := False;
end;

procedure TOpenWindowForm.ButtonOpenClick(Sender: TObject);
begin
  FDumpWindowDevice := SelectedDevice;
  FDumpWindowDevice.DumpWindow(SelectedWindowHash);
  ButtonOpen.Enabled := False;
  ButtonCancel.Caption := sCancel;
end;

procedure TOpenWindowForm.ButtonRefreshDevicesClick(Sender: TObject);
begin
  ButtonRefreshDevices.Enabled := False;
  FAdbInterface.GetDeviceList;
end;

procedure TOpenWindowForm.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FDumpWindowDevice) then
  begin
    FDumpWindowDevice.CancelDumpWindow;
    ButtonCancel.Enabled := False;
  end
  else
    Close;
end;

procedure TOpenWindowForm.FormDestroy(Sender: TObject);
begin
  FreeListBoxObjects(ListBoxDevices);
  FAdbInterface.Free;
end;

procedure TOpenWindowForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    if Assigned(FDumpWindowDevice) then
      FDumpWindowDevice.CancelDumpWindow
    else
      Close;
  end;
end;

procedure TOpenWindowForm.ListBoxDevicesSelectionChange(Sender: TObject; User: boolean);
var
  I: integer;
begin
  I := ListBoxDevices.ItemIndex;
  if I > -1 then
    SelectedDevice := TDeviceInterface(ListBoxDevices.Items.Objects[I])
  else
    SelectedDevice := nil;
end;

procedure TOpenWindowForm.ListBoxWindowsDblClick(Sender: TObject);
begin
  if (SelectedWindowHash <> EmptyStr) and ButtonOpen.Enabled then
    ButtonOpen.Click;
end;

procedure TOpenWindowForm.ListBoxWindowsSelectionChange(Sender: TObject; User: boolean);
begin
  ButtonOpen.Enabled := ListBoxWindows.ItemIndex > -1;
end;

procedure TOpenWindowForm.SetSelectedDevice(V: TDeviceInterface);
begin
  if FSelectedDevice = V then
    Exit;

  FSelectedDevice := V;

  ListBoxWindows.Clear;
  ButtonRefreshWindows.Enabled := False;

  if Assigned(V) then
    V.GetWindowList;
end;

procedure TOpenWindowForm.SetWindowList(const WindowList: TWindowManagerEntryArray);
var
  I: integer;
begin
  // We need to associate another string (window hash) with each item
  // in ListBoxWindows (window title).
  // But since with TCustomListBox.AddItem we can only associate a TObject,
  // we keep the hashes in a separate string array.
  SetLength(FWindowHashes, Length(WindowList));

  ListBoxWindows.Items.BeginUpdate;
  try
    ListBoxWindows.Clear;
    for I := 0 to Length(WindowList) - 1 do
    begin
      ListBoxWindows.AddItem(WindowList[I].Title, nil);
      FWindowHashes[I] := WindowList[I].HashCode;
    end;
  finally
    ListBoxWindows.Items.EndUpdate;
  end;
end;

procedure TOpenWindowForm.AdbDeviceListComplete(Sender: TObject;
  const DeviceList: TAdbDeviceEntryArray);
var
  E: TAdbDeviceEntry;
  DeviceInterface: TDeviceInterface;
  S: string = '';
  I: integer;
begin
  if Assigned(SelectedDevice) then
    S := SelectedDevice.SerialNumber;

  FreeListBoxObjects(ListBoxDevices);
  ListBoxDevices.Clear;

  for E in DeviceList do
  begin
    DeviceInterface := TDeviceInterface.Create(E.SerialNumber);
    DeviceInterface.OnWindowListComplete := @DeviceWindowListComplete;
    DeviceInterface.OnWindowListError := @DeviceWindowListError;
    DeviceInterface.OnWindowDumpComplete := @DeviceWindowDumpComplete;
    DeviceInterface.OnWindowDumpCancel := @DeviceWindowDumpCancel;
    DeviceInterface.OnWindowDumpError := @DeviceWindowDumpCancel;
    ListBoxDevices.AddItem(E.SerialNumber, DeviceInterface);
  end;

  // Select the previously selected device, the first one or nothing.
  I := ListBoxDevices.Items.IndexOf(S);
  if I > -1 then
    ListBoxDevices.ItemIndex := I
  else
  if ListBoxDevices.Count > 0 then
    ListBoxDevices.ItemIndex := 0
  else
    SelectedDevice := nil;

  ButtonRefreshDevices.Enabled := True;
end;

procedure TOpenWindowForm.AdbDeviceListError(Sender: TObject);
begin
  ButtonRefreshDevices.Enabled := True;
end;

function TOpenWindowForm.GetSelectedDeviceSerial: string;
begin
  if Assigned(FSelectedDevice) then
    Result := FSelectedDevice.SerialNumber
  else
    Result := EmptyStr;
end;

function TOpenWindowForm.GetSelectedWindowHash: string;
var
  I: integer;
begin
  I := ListBoxWindows.ItemIndex;
  if I > -1 then
    Result := FWindowHashes[I]
  else
    Result := EmptyStr;
end;

function TOpenWindowForm.GetSelectedWindowTitle: string;
var
  I: integer;
begin
  I := ListBoxWindows.ItemIndex;
  if I > -1 then
    Result := ListBoxWindows.Items[I]
  else
    Result := EmptyStr;
end;

procedure TOpenWindowForm.DeviceWindowDumpCancel(Sender: TObject);
begin
  FDumpWindowDevice := nil;
  ButtonOpen.Enabled := True;
  ButtonCancel.Enabled := True;
  ButtonCancel.Caption := sClose;
end;

procedure TOpenWindowForm.DeviceWindowDumpComplete(Sender: TDeviceInterface;
  RootView: TView3D);
begin
  FRootView := RootView;
  ModalResult := mrOk;
end;

procedure TOpenWindowForm.DeviceWindowListComplete(Sender: TDeviceInterface;
  const WindowList: TWindowManagerEntryArray);
begin
  if SelectedDevice = Sender then
    Self.WindowList := WindowList;
end;

procedure TOpenWindowForm.DeviceWindowListError(Sender: TObject);
begin
  ButtonRefreshWindows.Enabled := Assigned(SelectedDevice);
end;

end.
