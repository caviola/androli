unit AndroidDebugBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TaskRunner, View3DTypes, ViewServerClient,
  blcksock;

const
  AdbServerPort = 5037;
  RequestTimeout = 30000; // 30 seconds
  NoTimeout = -1;

type

  // https://android.googlesource.com/platform/system/core/+/master/adb/OVERVIEW.TXT
  // https://android.googlesource.com/platform/system/core/+/master/adb/SERVICES.TXT

  TAdbDeviceEntry = record
    SerialNumber: string;
    State: string;
  end;

  TAdbDeviceEntryArray = array of TAdbDeviceEntry;

  { TAdbConnection }

  TAdbConnection = class
  private
    FSocket: TTCPBlockSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect; virtual;
    procedure Disconnect;
    procedure SendRequest(const Payload: string);
    function ReadResponse(Timeout: integer = RequestTimeout): string;
    property Socket: TTCPBlockSocket read FSocket;
  end;

  { TAdbHostConnection }

  TAdbHostConnection = class(TAdbConnection)
  protected
    function ParseDeviceEntry(var Line: PChar): TAdbDeviceEntry;
  public
    function GetDevices: TAdbDeviceEntryArray;
    function GetVersion: integer;
  end;

  { TAdbDeviceConnection }

  TAdbDeviceConnection = class(TAdbConnection)
  private
    FDevice: string;
  public
    constructor Create(const ADevice: string);
    procedure Connect; override;
    function ShellExecute(const Command: string): string;
    procedure ConnectTcp(Port: integer);
    function IsViewServerRunning: boolean;
    function StartViewServer: boolean;
    function StopViewServer: boolean;
  end;

  { TDeviceListTask }

  TDeviceListTask = class(TTask)
  private
    FDeviceList: TAdbDeviceEntryArray;
  protected
    procedure Run; override;
  public
    property DeviceList: TAdbDeviceEntryArray read FDeviceList;
  end;

  TDeviceListCompleteEvent = procedure(Sender: TObject;
    const DeviceList: TAdbDeviceEntryArray) of object;

  { TAdbInterface }

  TAdbInterface = class
  private
    FDeviceListTask: TDeviceListTask;
    FOnDeviceListComplete: TDeviceListCompleteEvent;
    FOnDeviceListError: TNotifyEvent;
  protected
    procedure DeviceListTaskComplete(Sender: TObject);
    procedure DeviceListTaskError(Sender: TObject);
  public
    destructor Destroy; override;
    procedure GetDeviceList;
    property OnDeviceListComplete: TDeviceListCompleteEvent
      read FOnDeviceListComplete write FOnDeviceListComplete;
    property OnDeviceListError: TNotifyEvent
      read FOnDeviceListError write FOnDeviceListError;
  end;

  TDeviceInterface = class;

  TWindowListCompleteEvent = procedure(Sender: TDeviceInterface;
    const WindowList: TWindowManagerEntryArray) of object;

  TWindowDumpCompleteEvent = procedure(Sender: TDeviceInterface;
    RootView: TView3D) of object;

  TWindowDumpProgressEvent = procedure(Sender: TDeviceInterface;
    Progress: integer) of object;

  { TDumpWindowTask }

  TDumpWindowTask = class(TTask)
  private
    FRootView: TView3D;
    FWindowHash: string;
    FDeviceSerial: string;
  protected
    procedure Run; override;
  public
    constructor Create(const DeviceSerial, WindowHash: string);
    property RootView: TView3D read FRootView;
  end;

  { TWindowListTask }

  TWindowListTask = class(TTask)
  private
    FDeviceSerial: string;
    FWindowList: TWindowManagerEntryArray;
  protected
    procedure Run; override;
  public
    constructor Create(const DeviceSerial: string);
    property WindowList: TWindowManagerEntryArray read FWindowList;
  end;

  { TDeviceInterface }

  TDeviceInterface = class
  private
    FDumpWindowTask: TDumpWindowTask;
    FWindowListTask: TWindowListTask;
    FOnWindowDumpCancel: TNotifyEvent;
    FOnWindowListComplete: TWindowListCompleteEvent;
    FOnWindowListError: TNotifyEvent;
    FSerialNumber: string;
    FOnWindowDumpComplete: TWindowDumpCompleteEvent;
    FOnWindowDumpError: TNotifyEvent;
    FOnWindowDumpProgress: TWindowDumpProgressEvent;
  protected
    procedure DumpWindowTaskComplete(Sender: TObject);
    procedure DumpWindowTaskError(Sender: TObject);
    procedure WindowListTaskComplete(Sender: TObject);
    procedure WindowListTaskError(Sender: TObject);
  public
    constructor Create(const ADevice: string);
    destructor Destroy; override;
    procedure DumpWindow(const HashCode: string);
    function CancelDumpWindow: boolean;
    procedure GetWindowList;
    property SerialNumber: string read FSerialNumber;
    property OnWindowDumpComplete: TWindowDumpCompleteEvent
      read FOnWindowDumpComplete write FOnWindowDumpComplete;
    property OnWindowDumpProgress: TWindowDumpProgressEvent
      read FOnWindowDumpProgress write FOnWindowDumpProgress;
    property OnWindowDumpError: TNotifyEvent
      read FOnWindowDumpError write FOnWindowDumpError;
    property OnWindowDumpCancel: TNotifyEvent
      read FOnWindowDumpCancel write FOnWindowDumpCancel;
    property OnWindowListComplete: TWindowListCompleteEvent
      read FOnWindowListComplete write FOnWindowListComplete;
    property OnWindowListError: TNotifyEvent
      read FOnWindowListError write FOnWindowListError;
  end;

  EAdbException = class(Exception);

implementation

const
  MsgUnexpectedServerResponse = 'Received unexpected ADB server response.';
  CallWindowSuccessResponse = 'Result: Parcel(00000000 00000001   ''........'')';

{ TDeviceListTask }

procedure TDeviceListTask.Run;
begin
  with TAdbHostConnection.Create do
    try
      FDeviceList := GetDevices;
    finally
      Free;
    end;
end;

{ TWindowListTask }

procedure TWindowListTask.Run;
begin
  with TAdbDeviceConnection.Create(FDeviceSerial) do
    try
      if not IsViewServerRunning and not StartViewServer then
        Exit;

      ConnectTcp(ViewServerPort);
      try
        FWindowList := ViewServerGetWindowList(Socket, 2000);
      finally
        Disconnect;
      end;
    finally
      Free;
    end;
end;

constructor TWindowListTask.Create(const DeviceSerial: string);
begin
  FDeviceSerial := DeviceSerial;
end;

{ TDumpWindowTask }

procedure TDumpWindowTask.Run;
begin
  with TAdbDeviceConnection.Create(FDeviceSerial) do
    try
      ConnectTcp(ViewServerPort);
      try
        FRootView := ViewServerDumpWindow(Socket, FWindowHash, FCanceled);
      finally
        Disconnect;
      end;
    finally
      Free;
    end;
end;

constructor TDumpWindowTask.Create(const DeviceSerial, WindowHash: string);
begin
  FWindowHash := WindowHash;
  FDeviceSerial := DeviceSerial;
end;

{ TAdbHostConnection }

function TAdbHostConnection.GetDevices: TAdbDeviceEntryArray;
var
  Line: PChar;
  N: integer;
  Entry: TAdbDeviceEntry;
begin
  Connect;
  try
    SendRequest('host:devices');
    Line := PChar(ReadResponse(2000));
    // Line is either (1) an empty string meaning there aren't
    // connected devices or (2) a series of newline-terminated strings
    // formatted as:
    //   <serial> "\t" <state> "\N"
    N := 0;
    while Line[0] <> #0 do
    begin
      Entry := ParseDeviceEntry(Line);
      if Entry.State = 'device' then
      begin
        Inc(N);
        SetLength(Result, N);
        Result[N - 1] := Entry;
      end;
    end;
  finally
    Disconnect;
  end;
end;

function TAdbHostConnection.GetVersion: integer;
begin
  Connect;
  try
    SendRequest('host:version');
    Result := StrToInt('$' + ReadResponse);
  finally
    Disconnect;
  end;
end;

function TAdbHostConnection.ParseDeviceEntry(var Line: PChar): TAdbDeviceEntry;
var
  Pos: PChar;
begin
  //TODO: parse error checking
  Pos := StrScan(Line, #9);
  Result.SerialNumber := Copy(Line, 1, Pos - Line);
  Line := Pos + 1;
  repeat
    Inc(Pos);
  until Pos[0] in [#32, #10];
  Result.State := Copy(Line, 1, Pos - Line);
  Line := Pos + 1;
end;

{ TAdbDeviceConnection }

constructor TAdbDeviceConnection.Create(const ADevice: string);
begin
  inherited Create;
  FDevice := ADevice;
end;

procedure TAdbDeviceConnection.Connect;
begin
  inherited Connect;
  try
    SendRequest('host:transport:' + FDevice);
  except
    Disconnect;
    raise;
  end;
end;

function TAdbDeviceConnection.ShellExecute(const Command: string): string;
begin
  Connect;
  try
    SendRequest('shell:' + Command);
    // Read everything until the connection is closed.
    // We assume that the output of any shell command fits into 'string'.
    Result := EmptyStr;
    try
      while True do
        Result := Result + Socket.RecvString(5000);
    except
      // TODO: how to determine the cross-platform error code for connection reset by peer?
      on E: ESynapseError do
        if E.ErrorCode <> 104 then
          raise;
    end;
  finally
    Disconnect;
  end;
end;

procedure TAdbDeviceConnection.ConnectTcp(Port: integer);
begin
  Connect;
  try
    SendRequest('tcp:' + IntToStr(Port));
  except
    Disconnect;
    raise;
  end;
end;

function TAdbDeviceConnection.IsViewServerRunning: boolean;
begin
  Result := ShellExecute('service call window 3 i32 ' + IntToStr(ViewServerPort)) =
    CallWindowSuccessResponse;
end;

function TAdbDeviceConnection.StartViewServer: boolean;
begin
  Result := ShellExecute('service call window 1 i32 ' + IntToStr(ViewServerPort)) =
    CallWindowSuccessResponse;
end;

function TAdbDeviceConnection.StopViewServer: boolean;
begin
  Result := ShellExecute('service call window 2 i32 ' + IntToStr(ViewServerPort)) =
    CallWindowSuccessResponse;
end;

{ TDeviceInterface }

procedure TDeviceInterface.DumpWindowTaskComplete(Sender: TObject);
begin
  if Assigned(OnWindowDumpComplete) then
    OnWindowDumpComplete(Self, TDumpWindowTask(Sender).RootView);

  FDumpWindowTask := nil;
end;

procedure TDeviceInterface.DumpWindowTaskError(Sender: TObject);
begin
  if Assigned(OnWindowDumpError) then
    OnWindowDumpError(Self);

  FDumpWindowTask := nil;
end;

procedure TDeviceInterface.WindowListTaskComplete(Sender: TObject);
begin
  if Assigned(OnWindowListComplete) then
    OnWindowListComplete(Self, TWindowListTask(Sender).WindowList);

  FWindowListTask := nil;
end;

procedure TDeviceInterface.WindowListTaskError(Sender: TObject);
begin
  if Assigned(OnWindowListError) then
    OnWindowListError(Self);

  FWindowListTask := nil;
end;

constructor TDeviceInterface.Create(const ADevice: string);
begin
  inherited Create;
  FSerialNumber := ADevice;
end;

destructor TDeviceInterface.Destroy;
begin
  if Assigned(FWindowListTask) then
  begin
    FWindowListTask.Cancel;
    FWindowListTask := nil;
  end;

  if Assigned(FDumpWindowTask) then
  begin
    FDumpWindowTask.Cancel;
    FDumpWindowTask := nil;
  end;

  inherited;
end;

procedure TDeviceInterface.DumpWindow(const HashCode: string);
begin
  if not Assigned(OnWindowDumpComplete) then
    raise Exception.Create(
      'OnWindowDumpComplete must be set before calling DumpWindow');

  if Assigned(FDumpWindowTask) then
    Exit; // another dump operation in progress

  // IMPORTANT!
  // Note that we create a new task here but never free it ourselves.
  // The task will be freed automatically for us after it completes
  // either successfully, with error or when explicitly canceled.
  // Hence that we should consider any task reference invalid after
  // OnComplete/OnError have returned.

  FDumpWindowTask := TDumpWindowTask.Create(SerialNumber, HashCode);
  FDumpWindowTask.OnComplete := @DumpWindowTaskComplete;
  FDumpWindowTask.OnError := @DumpWindowTaskError;

  StartTask(FDumpWindowTask);
end;

function TDeviceInterface.CancelDumpWindow: boolean;
begin
  if Assigned(FDumpWindowTask) then
  begin
    FDumpWindowTask.Cancel;
    FDumpWindowTask := nil;

    if Assigned(OnWindowDumpCancel) then
      OnWindowDumpCancel(Self);

    Result := True;
  end
  else
    Result := False;
end;

procedure TDeviceInterface.GetWindowList;
begin
  if Assigned(FWindowListTask) then
    Exit; // another operation in progress

  FWindowListTask := TWindowListTask.Create(SerialNumber);
  FWindowListTask.OnComplete := @WindowListTaskComplete;
  FWindowListTask.OnError := @WindowListTaskError;

  StartTask(FWindowListTask);
end;

{ TAdbConnection }

constructor TAdbConnection.Create;
begin
  inherited;
  FSocket := TTCPBlockSocket.Create;
  FSocket.RaiseExcept := True;
end;

destructor TAdbConnection.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

procedure TAdbConnection.Connect;
begin
  FSocket.Connect(cLocalhost, IntToStr(AdbServerPort));
end;

procedure TAdbConnection.Disconnect;
begin
  FSocket.CloseSocket;
end;

procedure TAdbConnection.SendRequest(const Payload: string);
var
  S: string;
begin
  FSocket.SendString(IntToHex(Length(Payload), 4) + Payload);
  S := FSocket.RecvBufferStr(4, RequestTimeout);
  if S = 'OKAY' then
    Exit
  else if S = 'FAIL' then
    raise EAdbException.Create(ReadResponse)
  else
    raise EAdbException.Create(MsgUnexpectedServerResponse);
end;

function TAdbConnection.ReadResponse(Timeout: integer): string;
var
  L: integer;
begin
  L := StrToInt('$' + FSocket.RecvBufferStr(4, Timeout));
  Result := FSocket.RecvBufferStr(L, Timeout);
end;

{ TAdbInterface }

procedure TAdbInterface.DeviceListTaskComplete(Sender: TObject);
begin
  if Assigned(OnDeviceListComplete) then
    OnDeviceListComplete(Self, TDeviceListTask(Sender).DeviceList);

  FDeviceListTask := nil;
end;

procedure TAdbInterface.DeviceListTaskError(Sender: TObject);
begin
  if Assigned(OnDeviceListError) then
    OnDeviceListError(Self);

  FDeviceListTask := nil;
end;

destructor TAdbInterface.Destroy;
begin
  if Assigned(FDeviceListTask) then
  begin
    FDeviceListTask.Cancel;
    FDeviceListTask := nil;
  end;

  inherited;
end;

procedure TAdbInterface.GetDeviceList;
begin
  if Assigned(FDeviceListTask) then
    Exit; // another operation in progress

  FDeviceListTask := TDeviceListTask.Create;
  FDeviceListTask.OnComplete := @DeviceListTaskComplete;
  FDeviceListTask.OnError := @DeviceListTaskError;

  StartTask(FDeviceListTask);
end;

end.
