unit FormOpenViewServerWindow;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls,
  StdCtrls, AndroidDebugBridge;

type

  { TOpenViewServerWindowForm }

  TOpenViewServerWindowForm = class(TForm)
    ButtonCancel: TButton;
    ButtonOpen: TButton;
    ButtonRefreshWindows: TButton;
    ButtonRefreshDevices: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListBoxDevices: TListBox;
    ListBoxWindows: TListBox;
    procedure ButtonRefreshDevicesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ListBoxDevicesSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ListBoxWindowsDblClick(Sender: TObject);
    procedure ListBoxWindowsSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    FAdbInterface: TAdbInterface;
    FSelectedDevice: TDeviceInterface;
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

    procedure DeviceWindowListComplete(Sender: TDeviceInterface;
      const WindowList: TWindowManagerEntryArray);
    procedure DeviceWindowListError(Sender: TObject);
    property SelectedDevice: TDeviceInterface
      read FSelectedDevice write SetSelectedDevice;
    property WindowList: TWindowManagerEntryArray write SetWindowList;
  public
    property SelectedWindowTitle: string read GetSelectedWindowTitle;
    property SelectedDeviceSerial: string read GetSelectedDeviceSerial;
    property SelectedWindowHash: string read GetSelectedWindowHash;
  end;


var
  OpenViewServerWindowForm: TOpenViewServerWindowForm;

implementation

const
  WindowNoTitle = '<untitled>';

{$R *.lfm}

procedure FreeListBoxObjects(ListBox: TCustomListBox); inline;
var
  I: integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
    ListBox.Items.Objects[I].Free;
end;

{ TOpenViewServerWindowForm }

procedure TOpenViewServerWindowForm.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  FAdbInterface := TAdbInterface.Create;
  FAdbInterface.OnDeviceListComplete := @AdbDeviceListComplete;
  FAdbInterface.OnDeviceListError := @AdbDeviceListError;
  FAdbInterface.GetDeviceList;
  ButtonRefreshDevices.Enabled := False;
end;

procedure TOpenViewServerWindowForm.ButtonRefreshDevicesClick(Sender: TObject);
begin
  ButtonRefreshDevices.Enabled := False;
  FAdbInterface.GetDeviceList;
end;

procedure TOpenViewServerWindowForm.FormDestroy(Sender: TObject);
begin
  FreeListBoxObjects(ListBoxDevices);
  FAdbInterface.Free;
end;

procedure TOpenViewServerWindowForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

procedure TOpenViewServerWindowForm.ListBoxDevicesSelectionChange(Sender: TObject;
  User: boolean);
var
  I: integer;
begin
  I := ListBoxDevices.ItemIndex;
  if I > -1 then
    SelectedDevice := TDeviceInterface(ListBoxDevices.Items.Objects[I])
  else
    SelectedDevice := nil;
end;

procedure TOpenViewServerWindowForm.ListBoxWindowsDblClick(Sender: TObject);
begin
  if (SelectedWindowHash <> EmptyStr) and ButtonOpen.Enabled then
    ButtonOpen.Click;
end;

procedure TOpenViewServerWindowForm.ListBoxWindowsSelectionChange(Sender: TObject;
  User: boolean);
begin
  ButtonOpen.Enabled := ListBoxWindows.ItemIndex > -1;
end;

procedure TOpenViewServerWindowForm.SetSelectedDevice(V: TDeviceInterface);
begin
  if FSelectedDevice = V then
    Exit;

  FSelectedDevice := V;

  ListBoxWindows.Clear;
  ButtonRefreshWindows.Enabled := False;

  if Assigned(V) then
    V.GetWindowList;
end;

procedure TOpenViewServerWindowForm.SetWindowList(
  const WindowList: TWindowManagerEntryArray);
var
  I: integer;
  WindowTitle: string;
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
      WindowTitle := WindowList[I].Title;
      if WindowTitle = EmptyStr then
        WindowTitle := WindowNoTitle;

      ListBoxWindows.AddItem(WindowTitle, nil);
      FWindowHashes[I] := WindowList[I].HashCode;
    end;
  finally
    ListBoxWindows.Items.EndUpdate;
  end;
end;

procedure TOpenViewServerWindowForm.AdbDeviceListComplete(Sender: TObject;
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

procedure TOpenViewServerWindowForm.AdbDeviceListError(Sender: TObject);
begin
  ButtonRefreshDevices.Enabled := True;
end;

function TOpenViewServerWindowForm.GetSelectedDeviceSerial: string;
begin
  if Assigned(FSelectedDevice) then
    Result := FSelectedDevice.SerialNumber
  else
    Result := EmptyStr;
end;

function TOpenViewServerWindowForm.GetSelectedWindowHash: string;
var
  I: integer;
begin
  I := ListBoxWindows.ItemIndex;
  if I > -1 then
    Result := FWindowHashes[I]
  else
    Result := EmptyStr;
end;

function TOpenViewServerWindowForm.GetSelectedWindowTitle: string;
var
  I: integer;
begin
  I := ListBoxWindows.ItemIndex;
  if I > -1 then
    Result := ListBoxWindows.Items[I]
  else
    Result := EmptyStr;
end;

procedure TOpenViewServerWindowForm.DeviceWindowListComplete(Sender: TDeviceInterface;
  const WindowList: TWindowManagerEntryArray);
begin
  if SelectedDevice = Sender then
    Self.WindowList := WindowList;
end;

procedure TOpenViewServerWindowForm.DeviceWindowListError(Sender: TObject);
begin
  ButtonRefreshWindows.Enabled := Assigned(SelectedDevice);
end;

end.
