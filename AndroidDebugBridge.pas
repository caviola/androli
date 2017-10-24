unit AndroidDebugBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TaskRunner, View3DTypes, blcksock, Graphics;

const
  RequestTimeout = 30000; // 30 seconds
  NoTimeout = -1;

type

  // https://android.googlesource.com/platform/system/core/+/master/adb/OVERVIEW.TXT
  // https://android.googlesource.com/platform/system/core/+/master/adb/SERVICES.TXT

  TWindowManagerEntry = record
    HashCode: string;
    Title: string;
  end;

  TWindowManagerEntryArray = array of TWindowManagerEntry;

  { IViewServerClient }

  IViewServerClient = interface
    function IsServerRunning: boolean;
    function StartServer: boolean;
    function StopServer: boolean;
    function GetDeviceSerial: string;
    function GetWindowList(Timeout: integer = -1): TWindowManagerEntryArray;
    function DumpWindow(const WindowHash: string;
      const CheckCanceled: TObjectProcedure): TView3D;
    function CaptureView(const WindowHash, ViewClass, ViewHash: string): TRasterImage;
  end;

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
    property DeviceSerial: string read FDevice;
  end;

  IDeviceListTask = interface(ITask)
    ['{762266BE-3445-4FF0-A87E-43279F0765DE}']
    function GetResult: TAdbDeviceEntryArray;
  end;

  { TDeviceListTask }

  TDeviceListTask = class(TTask, IDeviceListTask)
  private
    FDeviceList: TAdbDeviceEntryArray;
  protected
    procedure Run; override;
    function GetResult: TAdbDeviceEntryArray;
  end;

  TDeviceListCompleteEvent = procedure(Sender: TObject;
    const DeviceList: TAdbDeviceEntryArray) of object;

  { TAdbInterface }

  TAdbInterface = class
  private
    FDeviceListTask: IDeviceListTask;
    FOnDeviceListComplete: TDeviceListCompleteEvent;
    FOnDeviceListError: TNotifyEvent;
  protected
    procedure DeviceListSuccess(const Task: ITask);
    procedure DeviceListError(const Task: ITask; Error: Exception);
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

  IWindowListTask = interface(ITask)
    ['{3079A8C2-5DCF-4371-BCF6-761B5106488B}']
    function GetResult: TWindowManagerEntryArray;
  end;

  { TWindowListTask }

  TWindowListTask = class(TTask, IWindowListTask)
  private
    FDeviceSerial: string;
    FResult: TWindowManagerEntryArray;
  protected
    procedure Run; override;
  public
    constructor Create(const DeviceSerial: string);
    function GetResult: TWindowManagerEntryArray;
  end;

  { TDeviceInterface }

  TDeviceInterface = class
  private
    FWindowListTask: IWindowListTask;
    FOnWindowListComplete: TWindowListCompleteEvent;
    FOnWindowListError: TNotifyEvent;
    FSerialNumber: string;
  protected
    procedure WindowListTaskSuccess(const Task: ITask);
    procedure WindowListTaskError(const Task: ITask; Error: Exception);
    procedure WindowListTaskStopped(const Task: ITask);
  public
    constructor Create(const ADevice: string);
    destructor Destroy; override;
    procedure GetWindowList;
    property SerialNumber: string read FSerialNumber;
    property OnWindowListComplete: TWindowListCompleteEvent
      read FOnWindowListComplete write FOnWindowListComplete;
    property OnWindowListError: TNotifyEvent
      read FOnWindowListError write FOnWindowListError;
  end;

  EAdbException = class(Exception);

function CreateViewServerClient(const DeviceSerial: string): IViewServerClient;

implementation

uses
  Math, LazLogger, synsock;

const
  AdbServerPort = 5037;
  ViewServerPort = 4939;
  MsgUnexpectedServerResponse = 'Received unexpected ADB server response.';
  CallWindowSuccessResponse = 'Result: Parcel(00000000 00000001   ''........'')';
  DONEp = 'DONE.';
  DONE = 'DONE';
  UndefinedMargin = -2147483648;

type

  { TViewServerClient }

  TViewServerClient = class(TInterfacedObject, IViewServerClient)
  private
    FAdbDeviceConnection: TAdbDeviceConnection;
  public
    constructor Create(const DeviceSerial: string);
    destructor Destroy; override;
    function IsServerRunning: boolean;
    function StartServer: boolean;
    function StopServer: boolean;
    function GetDeviceSerial: string;
    function GetWindowList(Timeout: integer = -1): TWindowManagerEntryArray;
    function DumpWindow(const WindowHash: string;
      const CheckCanceled: TObjectProcedure): TView3D;
    function CaptureView(const WindowHash, ViewClass, ViewHash: string): TRasterImage;
  end;

  { TViewServerCaptureViewTask }

  TViewServerCaptureViewTask = class(TCaptureViewTask)
  private
    FDeviceSerial: string;
    FWindowHash: string;
  protected
    procedure Run; override;
  public
    constructor Create(const ADeviceSerial, AWindowHash: string; AView: TView3D);
  end;

  { TViewServerCaptureViewTaskFactory }

  TViewServerCaptureViewTaskFactory = class(TInterfacedObject, ICaptureViewTaskFactory)
  private
    FDeviceSerial: string;
    FWindowHash: string;
  public
    constructor Create(const DeviceSerial, WindowHash: string);
    function CreateTask(View: TView3D): TCaptureViewTask;
  end;

function CreateViewServerClient(const DeviceSerial: string): IViewServerClient;
begin
  Result := TViewServerClient.Create(DeviceSerial);
end;

{ TViewServerCaptureViewTaskFactory }

constructor TViewServerCaptureViewTaskFactory.Create(
  const DeviceSerial, WindowHash: string);
begin
  FDeviceSerial := DeviceSerial;
  FWindowHash := WindowHash;
end;

function TViewServerCaptureViewTaskFactory.CreateTask(View: TView3D): TCaptureViewTask;
begin
  Result := TViewServerCaptureViewTask.Create(FDeviceSerial, FWindowHash, View);
end;

{ TViewServerCaptureViewTask }

procedure TViewServerCaptureViewTask.Run;
var
  View: TView3D;
begin
  View := GetAssociatedView;
  FResult := CreateViewServerClient(FDeviceSerial).CaptureView(FWindowHash,
    View.QualifiedClassName, View.HashCode);
end;

constructor TViewServerCaptureViewTask.Create(const ADeviceSerial, AWindowHash: string;
  AView: TView3D);
begin
  inherited Create(AView);
  FDeviceSerial := ADeviceSerial;
  FWindowHash := AWindowHash;
end;

{ TViewServerConnection }

constructor TViewServerClient.Create(const DeviceSerial: string);
begin
  FAdbDeviceConnection := TAdbDeviceConnection.Create(DeviceSerial);
end;

destructor TViewServerClient.Destroy;
begin
  FAdbDeviceConnection.Free;
  inherited Destroy;
end;

function TViewServerClient.IsServerRunning: boolean;
begin
  Result := FAdbDeviceConnection.ShellExecute('service call window 3 i32 ' +
    IntToStr(ViewServerPort)) = CallWindowSuccessResponse;
end;

function TViewServerClient.StartServer: boolean;
begin
  Result := FAdbDeviceConnection.ShellExecute('service call window 1 i32 ' +
    IntToStr(ViewServerPort)) = CallWindowSuccessResponse;
end;

function TViewServerClient.StopServer: boolean;
begin
  Result := FAdbDeviceConnection.ShellExecute('service call window 2 i32 ' +
    IntToStr(ViewServerPort)) = CallWindowSuccessResponse;
end;

function TViewServerClient.GetDeviceSerial: string;
begin
  Result := FAdbDeviceConnection.DeviceSerial;
end;

function TViewServerClient.GetWindowList(Timeout: integer): TWindowManagerEntryArray;
var
  Line: string;
  Count: integer = 0;
  P: integer;
begin
  FAdbDeviceConnection.ConnectTcp(ViewServerPort);
  try
    FAdbDeviceConnection.Socket.SendString('LIST'#10);
    repeat
      Line := FAdbDeviceConnection.Socket.RecvTerminated(Timeout, #10);
      if (Line = DONEp) or (Line = DONE) then
        Break;

      Inc(Count);
      SetLength(Result, Count);

      P := Pos(#32, Line);
      Result[Count - 1].HashCode := Copy(Line, 1, P - 1);
      Result[Count - 1].Title := Copy(Line, P + 1);
    until False;
  finally
    FAdbDeviceConnection.Disconnect;
  end;
end;

function TViewServerClient.DumpWindow(const WindowHash: string;
  const CheckCanceled: TObjectProcedure): TView3D;

  procedure ParseDumpLine(Line: PChar; View: TView3D);
  var
    Pos: PChar;
    PName, PValue: string;
    PLen: integer;
  begin
    // TODO: error checking
    // Note that we don't do any error checking here.
    // For the time being, we assume all lines are formatted as expected.

    Pos := StrScan(Line, '@');
    View.QualifiedClassName := Copy(Line, 1, Pos - Line);  // substring up to '@'
    Line := Pos + 1; // skip '@'

    Pos := StrScan(Line, #32);
    View.HashCode := Copy(Line, 1, Pos - Line); // substring up to whitespace

    Line := Pos;
    while Line[0] = #32 do
    begin
      Inc(Line); // skip whitespace
      if Line[0] = #0 then
        Break;  // reached end-of-line

      Pos := StrScan(Line, '=');
      PName := Copy(Line, 1, Pos - Line); // substring up to '='
      Line := Pos + 1; // skip '='

      Pos := StrScan(Line, ',');
      PLen := StrToInt(Copy(Line, 1, Pos - Line));  // substring up to ','
      Line := Pos + 1; // skip ','

      PValue := Copy(Line, 1, PLen);  // next PLen characters
      Inc(Line, PLen);

      View.SetProperty(PName, PValue);
    end;
  end;

  function CreateView(AParent: TView3D; Line: PChar; Depth: integer): TView3D;
  var
    OX, OY, ScaleX, ScaleY: single;
  begin
    Result := TView3D.Create;
    try
      ParseDumpLine(Line, Result);

      with Result do
      begin
        // These bounds are relative to parent.
        SetBounds(
          GetIntProp('layout:mLeft'),
          GetIntProp('layout:mTop'),
          GetIntProp('layout:mRight'),
          GetIntProp('layout:mBottom'),
          Depth);

        Translate(
          GetFloatProp('drawing:getTranslationX()'),
          GetFloatProp('drawing:getTranslationY()'),
          0);

        SetPaddings(
          GetIntProp('padding:mPaddingLeft'),
          GetIntProp('padding:mPaddingTop'),
          GetIntProp('padding:mPaddingRight'),
          GetIntProp('padding:mPaddingBottom'));

        // The constant -2147483648 is used by GridLayout and means UNDEFINED.
        // For our rendering purposes we consider that to mean 0.
        // See https://developer.android.com/reference/android/support/v7/widget/GridLayout.html
        MarginLeft := GetIntProp('layout:layout_leftMargin');
        if MarginLeft = UndefinedMargin then
          MarginLeft := 0;

        MarginTop := GetIntProp('layout:layout_topMargin');
        if MarginTop = UndefinedMargin then
          MarginTop := 0;

        MarginRight := GetIntProp('layout:layout_rightMargin');
        if MarginRight = UndefinedMargin then
          MarginRight := 0;

        MarginBottom := GetIntProp('layout:layout_bottomMargin');
        if MarginBottom = UndefinedMargin then
          MarginBottom := 0;

        // This is the scale explicitly defined in this View.
        ScaleX := GetFloatProp('drawing:getScaleX()', 1);
        ScaleY := GetFloatProp('drawing:getScaleY()', 1);
        if Assigned(AParent) then
        begin
          // This is the View's final scale, which also takes into account
          // the scale of its parents.
          TransformScaleX := AParent.TransformScaleX * ScaleX;
          TransformScaleY := AParent.TransformScaleY * ScaleY;
        end
        else
        begin
          TransformScaleX := ScaleX;
          TransformScaleY := ScaleY;
        end;

        if (ScaleX <> 1) or (ScaleY <> 1) then // View defines explicit scale?
        begin
          // Scale around explicit pivot point.
          OX := Left + GetFloatProp('drawing:getPivotX()');
          OY := Top + GetFloatProp('drawing:getPivotY()');
          Left := (Left - OX) * TransformScaleX + OX;
          Top := (Top - OY) * TransformScaleY + OY;
          Right := (Right - OX) * TransformScaleX + OX;
          Bottom := (Bottom - OY) * TransformScaleY + OY;
        end
        else
        begin
          // Scale around origin (0,0).
          Left := Left * TransformScaleX;
          Top := Top * TransformScaleY;
          Right := Right * TransformScaleX;
          Bottom := Bottom * TransformScaleY;
        end;

        PaddingLeft := PaddingLeft * TransformScaleX;
        PaddingTop := PaddingTop * TransformScaleY;
        PaddingRight := PaddingRight * TransformScaleX;
        PaddingBottom := PaddingBottom * TransformScaleY;

        MarginLeft := MarginLeft * TransformScaleX;
        MarginTop := MarginTop * TransformScaleY;
        MarginRight := MarginRight * TransformScaleX;
        MarginBottom := MarginBottom * TransformScaleY;

        if Assigned(AParent) then
        begin
          // Make bounds absolute.
          Translate(AParent.Left, AParent.Top, 0);

          ClippedLeft := EnsureRange(Left, AParent.ClippedLeft, AParent.ClippedRight);
          ClippedTop := EnsureRange(Top, AParent.ClippedTop, AParent.ClippedBottom);
          ClippedRight := EnsureRange(Right, AParent.ClippedLeft, AParent.ClippedRight);
          ClippedBottom := EnsureRange(Bottom, AParent.ClippedTop,
            AParent.ClippedBottom);
        end
        else
        begin
          ClippedLeft := Left;
          ClippedTop := Top;
          ClippedRight := Right;
          ClippedBottom := Bottom;
        end;
      end;

      if Assigned(AParent) then
        AParent.AddChild(Result);
    except
      Result.Free;
      raise;
    end;
  end;

var
  CurrentView: TView3D = nil;
  CurrentDepth: integer = -1;
  Depth: integer;
  Line: PChar;
  TaskFactory: ICaptureViewTaskFactory;
begin
  TaskFactory := TViewServerCaptureViewTaskFactory.Create(
    FAdbDeviceConnection.DeviceSerial, WindowHash);

  FAdbDeviceConnection.ConnectTcp(ViewServerPort);
  try
    FAdbDeviceConnection.Socket.SendString('DUMP ' + WindowHash + #10);
    try
      repeat
        Line := PChar(FAdbDeviceConnection.Socket.RecvTerminated(-1, #10));
        if (Line = DONEp) or (Line = DONE) then
          Break;

        CheckCanceled;

        Depth := 0;
        while Line[0] = #32 do
        begin
          Inc(Line);
          Inc(Depth);
        end;

        while Depth <= CurrentDepth do
        begin
          if Assigned(CurrentView) then
            CurrentView := CurrentView.Parent;
          Dec(CurrentDepth);
        end;

        CurrentView := CreateView(CurrentView, Line, Depth);
        CurrentView.CaptureViewTaskFactory := TaskFactory;
        CurrentDepth := Depth;
      until False;
    except
      // At this point CurrentView may be some branch deep down the tree.
      // So here we rewind to root view to free the whole tree.
      if Assigned(CurrentView) then
      begin
        while Assigned(CurrentView.Parent) do
          CurrentView := CurrentView.Parent;
        CurrentView.Free;
      end;
      raise;
    end;

    // Rewind to root view.
    if Assigned(CurrentView) then
    begin
      while Assigned(CurrentView.Parent) do
        CurrentView := CurrentView.Parent;
      CurrentView := Flatten(CurrentView);
    end;

    Result := CurrentView;
  finally
    FAdbDeviceConnection.Disconnect;
  end;
end;

function TViewServerClient.CaptureView(const WindowHash, ViewClass, ViewHash: string):
TRasterImage;

  function GetImageDataStream: TStream;
  begin
    Result := TMemoryStream.Create;
    try
      FAdbDeviceConnection.Socket.RecvStreamRaw(Result, 5000);
    except
      on E: ESynapseError do
        if E.ErrorCode <> WSAECONNRESET then
          raise;
    end;
    Result.Seek(0, soBeginning);
  end;

var
  Stream: TStream;
begin
  FAdbDeviceConnection.ConnectTcp(ViewServerPort);
  try
    FAdbDeviceConnection.Socket.SendString('CAPTURE ' + WindowHash +
      ' ' + ViewClass + '@' + ViewHash + #10);
    Stream := GetImageDataStream;
    try
      Result := TPortableNetworkGraphic.Create;
      try
        Result.LoadFromStream(Stream);
      except
        Result.Free;
        raise;
      end;
    finally
      Stream.Free;
    end;
  finally
    FAdbDeviceConnection.Disconnect;
  end;
end;

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

function TDeviceListTask.GetResult: TAdbDeviceEntryArray;
begin
  Result := FDeviceList;
end;

{ TWindowListTask }

constructor TWindowListTask.Create(const DeviceSerial: string);
begin
  FDeviceSerial := DeviceSerial;
end;

procedure TWindowListTask.Run;
begin
  with CreateViewServerClient(FDeviceSerial) do
  begin
    if not IsServerRunning and not StartServer then
      Exit;

    FResult := GetWindowList(2000);
  end;
end;

function TWindowListTask.GetResult: TWindowManagerEntryArray;
begin
  Result := FResult;
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
      on E: ESynapseError do
        if E.ErrorCode <> WSAECONNRESET then
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

{ TDeviceInterface }

constructor TDeviceInterface.Create(const ADevice: string);
begin
  inherited Create;
  FSerialNumber := ADevice;
end;

procedure TDeviceInterface.WindowListTaskSuccess(const Task: ITask);
begin
  if Assigned(OnWindowListComplete) then
    OnWindowListComplete(Self, (Task as IWindowListTask).GetResult);
end;

procedure TDeviceInterface.WindowListTaskError(const Task: ITask; Error: Exception);
begin
  if Assigned(OnWindowListError) then
    OnWindowListError(Self);
end;

procedure TDeviceInterface.WindowListTaskStopped(const Task: ITask);
begin
  FWindowListTask := nil;
end;

destructor TDeviceInterface.Destroy;
begin
  if Assigned(FWindowListTask) then
    FWindowListTask.Cancel;

  inherited;
end;

procedure TDeviceInterface.GetWindowList;
begin
  if Assigned(FWindowListTask) then
    FWindowListTask.Cancel;

  with TWindowListTask.Create(SerialNumber) do
  begin
    OnSuccess := @WindowListTaskSuccess;
    OnError := @WindowListTaskError;
    OnStopped := @WindowListTaskStopped;
    FWindowListTask := Start as IWindowListTask;
  end;
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

procedure TAdbInterface.DeviceListSuccess(const Task: ITask);
begin
  if Assigned(OnDeviceListComplete) then
    OnDeviceListComplete(Self, (Task as IDeviceListTask).GetResult);
end;

procedure TAdbInterface.DeviceListError(const Task: ITask; Error: Exception);
begin
  if Assigned(OnDeviceListError) then
    OnDeviceListError(Self);
end;

destructor TAdbInterface.Destroy;
begin
  FDeviceListTask.Cancel;
  FDeviceListTask := nil;
  inherited;
end;

procedure TAdbInterface.GetDeviceList;
begin
  if Assigned(FDeviceListTask) then
    FDeviceListTask.Cancel;

  with TDeviceListTask.Create do
  begin
    OnSuccess := @DeviceListSuccess;
    OnError := @DeviceListError;
    FDeviceListTask := Start as IDeviceListTask;
  end;
end;

end.
