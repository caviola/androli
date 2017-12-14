unit AndroidDebugBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TaskRunner, ViewTypes, blcksock, Graphics;

const
  RequestTimeout = 30000; // 30 seconds
  NoTimeout = -1;
  DumpWindowFileName = 'dumpwindow.txt';

type

  // https://android.googlesource.com/platform/system/core/+/master/adb/OVERVIEW.TXT
  // https://android.googlesource.com/platform/system/core/+/master/adb/SERVICES.TXT

  { TWindowManagerEntry }

  TWindowManagerEntry = record
    HashCode: string;
    Title: string;
  end;

  TWindowManagerEntryArray = array of TWindowManagerEntry;

  { IViewServerClient }

  IViewServerClient = interface
    ['{71B6D23B-C021-4845-BC51-C7303849376D}']
    function IsServerRunning: boolean;
    function StartServer: boolean;
    function StopServer: boolean;
    function GetDeviceSerial: string;
    function GetWindowList(Timeout: integer = -1): TWindowManagerEntryArray;
    function DumpWindow(const WindowHash: string;
      const CheckCanceled: TObjectProcedure): TView;
    function CaptureView(const WindowHash, ViewClass, ViewHash: string): TRasterImage;
  end;

  { TAdbConnection }

  TAdbConnection = class
  private
    FSocket: TTCPBlockSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure SendRequest(const Payload: string);
    function ReadResponse(Timeout: integer = RequestTimeout): string;
    property Socket: TTCPBlockSocket read FSocket;
  end;

  { TAdbDeviceConnection }

  TAdbDeviceConnection = class
  private
    FDevice: string;
    FAdbConnection: TAdbConnection;
    function GetSocket: TTCPBlockSocket; inline;
  public
    constructor Create(const ADevice: string);
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect; inline;
    function ShellExecute(const Command: string): TStream;
    procedure ConnectTcp(Port: integer);
    property DeviceSerial: string read FDevice;
    property Socket: TTCPBlockSocket read GetSocket;
  end;

  { TViewServerClient }

  TViewServerClient = class(TInterfacedObject, IViewServerClient)
  private
    FAdbDeviceConnection: TAdbDeviceConnection;
  protected
    class function ParseDumpLine(AParent: TView; Line: PChar; Depth: integer): TView;
  public
    constructor Create(const DeviceSerial: string);
    destructor Destroy; override;
    function IsServerRunning: boolean;
    function StartServer: boolean;
    function StopServer: boolean;
    function GetDeviceSerial: string;
    function GetWindowList(Timeout: integer = -1): TWindowManagerEntryArray;
    // Used only for debugging purposes.
    class function LoadDumpWindowFile: TView;
    function DumpWindow(const WindowHash: string;
      const CheckCanceled: TObjectProcedure): TView;
    function CaptureView(const WindowHash, ViewClass, ViewHash: string): TRasterImage;
  end;

  { TAdbDeviceEntry }

  TAdbDeviceEntry = record
    SerialNumber: string;
    State: string;
  end;

  TAdbDeviceEntryArray = array of TAdbDeviceEntry;

  { TAdbHostConnection }

  TAdbHostConnection = class(TAdbConnection)
  protected
    function ParseDeviceEntry(var Line: PChar): TAdbDeviceEntry;
  public
    function GetDevices: TAdbDeviceEntryArray;
    function GetVersion: integer;
  end;

  TDeviceListResultEvent = procedure(const Task: ITask;
    const TheResult: TAdbDeviceEntryArray) of object;

  { TDeviceListTask }

  TDeviceListTask = class(TTask)
  private
    FOnResult: TDeviceListResultEvent;
    FResult: TAdbDeviceEntryArray;
  protected
    procedure Run; override;
    procedure DoOnSuccess; override;
  public
    property OnResult: TDeviceListResultEvent read FOnResult write FOnResult;
  end;

  TDeviceListCompleteEvent = procedure(Sender: TObject;
    const DeviceList: TAdbDeviceEntryArray) of object;

  { TAdbInterface }

  TAdbInterface = class
  private
    FDeviceListTask: ITask;
    FOnDeviceListComplete: TDeviceListCompleteEvent;
    FOnDeviceListError: TNotifyEvent;
  protected
    procedure DeviceListResult(const Task: ITask;
      const TheResult: TAdbDeviceEntryArray);
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
    RootView: TView) of object;

  TWindowDumpProgressEvent = procedure(Sender: TDeviceInterface;
    Progress: integer) of object;

  TWindowListResultEvent = procedure(const Task: ITask;
    const TheResult: TWindowManagerEntryArray) of object;

  { TWindowListTask }

  TWindowListTask = class(TTask)
  private
    FDeviceSerial: string;
    FResult: TWindowManagerEntryArray;
    FOnResult: TWindowListResultEvent;
  protected
    procedure Run; override;
    procedure DoOnSuccess; override;
  public
    constructor Create(const DeviceSerial: string);
    property OnResult: TWindowListResultEvent read FOnResult write FOnResult;
  end;

  { TDeviceInterface }

  TDeviceInterface = class
  private
    FWindowListTask: ITask;
    FOnWindowListComplete: TWindowListCompleteEvent;
    FOnWindowListError: TNotifyEvent;
    FSerialNumber: string;
  protected
    procedure WindowListResult(const Task: ITask;
      const TheResult: TWindowManagerEntryArray);
    procedure WindowListError(const Task: ITask; Error: Exception);
    procedure WindowListStopped(const Task: ITask);
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

  { TViewServerCaptureViewTask }

  TViewServerCaptureViewTask = class(TCaptureViewTask)
  private
    FDeviceSerial: string;
    FWindowHash: string;
  protected
    procedure Run; override;
  public
    constructor Create(const ADeviceSerial, AWindowHash: string; AView: TView);
  end;


implementation

uses
  LazLogger, synsock;

const
  AdbServerPort = 5037;
  ViewServerPort = 4939;
  MsgUnexpectedServerResponse = 'Received unexpected ADB server response.';
  CallWindowSuccessResponse = 'Result: Parcel(00000000 00000001   ''........'')';
  DONEp = 'DONE.';
  DONE = 'DONE';
  UndefinedMargin = -2147483648;

type

  { TAdbDeviceConnectionStream }

  TAdbDeviceConnectionStream = class(TStream)
  private
    FConnection: TAdbDeviceConnection;
  public
    constructor Create(AConnection: TAdbDeviceConnection);
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
  end;


{ TAdbDeviceConnectionStream }

constructor TAdbDeviceConnectionStream.Create(AConnection: TAdbDeviceConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

destructor TAdbDeviceConnectionStream.Destroy;
begin
  FConnection.Disconnect;
  inherited Destroy;
end;

function TAdbDeviceConnectionStream.Read(var Buffer; Count: longint): longint;
begin
  Result := FConnection.Socket.RecvBuffer(@Buffer, Count);
end;

{ TViewServerCaptureViewTask }

procedure TViewServerCaptureViewTask.Run;
var
  View: TView;
begin
  View := GetAssociatedView;
  with TViewServerClient.Create(FDeviceSerial) as IViewServerClient do
    SetResult(CaptureView(FWindowHash, View.QualifiedClassName, View.HashCode));
end;

constructor TViewServerCaptureViewTask.Create(const ADeviceSerial, AWindowHash: string;
  AView: TView);
begin
  inherited Create(AView);
  FDeviceSerial := ADeviceSerial;
  FWindowHash := AWindowHash;
end;

{ TViewServerConnection }

class function TViewServerClient.ParseDumpLine(AParent: TView; Line: PChar;
  Depth: integer): TView;

  procedure ParseViewProperties(Line: PChar; View: TView); inline;
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

var
  OX, OY, ScaleX, ScaleY, TX, TY, TransformScaleX, TransformScaleY,
  Left, Top, Right, Bottom, PaddingLeft, PaddingTop, PaddingRight,
  PaddingBottom, MarginLeft, MarginTop, MarginRight, MarginBottom: single;
begin
  Result := TView.Create;
  try
    ParseViewProperties(Line, Result);

    // These bounds are relative to parent view and untransformed.
    // Since the bounds we must set in the view must be final window bounds,
    // we have to apply all transformations first.
    Left := Result.GetIntProp('layout:mLeft');
    Top := Result.GetIntProp('layout:mTop');
    Right := Result.GetIntProp('layout:mRight');
    Bottom := Result.GetIntProp('layout:mBottom');

    // Apply X and Y translation.
    TX := Result.GetFloatProp('drawing:getTranslationX()');
    TY := Result.GetFloatProp('drawing:getTranslationY()');
    Left := Left + TX;
    Top := Top + TY;
    Right := Right + TX;
    Bottom := Bottom + TY;

    // Get the scale explicitly defined in the view.
    ScaleX := Result.GetFloatProp('drawing:getScaleX()', 1);
    ScaleY := Result.GetFloatProp('drawing:getScaleY()', 1);
    // Compute the view's final scale, which takes into account
    // the scale of its parents.
    if Assigned(AParent) then
    begin
      TransformScaleX := AParent.TransformScaleX * ScaleX;
      TransformScaleY := AParent.TransformScaleY * ScaleY;
    end
    else
    begin
      TransformScaleX := ScaleX;
      TransformScaleY := ScaleY;
    end;

    if (ScaleX <> 1) or (ScaleY <> 1) then // view defines explicit scale?
    begin
      // Scale around provided pivot point.
      OX := Left + Result.GetFloatProp('drawing:getPivotX()');
      OY := Top + Result.GetFloatProp('drawing:getPivotY()');
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

    // Make bounds window absolute.
    // These are the final bounds we'll set in the view.
    if Assigned(AParent) then
    begin
      Left := Left + AParent.Left;
      Top := Top + AParent.Top;
      Right := Right + AParent.Left;
      Bottom := Bottom + AParent.Top;
    end;

    PaddingLeft := Result.GetIntProp('padding:mPaddingLeft') * TransformScaleX;
    PaddingTop := Result.GetIntProp('padding:mPaddingTop') * TransformScaleY;
    PaddingRight := Result.GetIntProp('padding:mPaddingRight') * TransformScaleX;
    PaddingBottom := Result.GetIntProp('padding:mPaddingBottom') * TransformScaleY;

    // The constant -2147483648 is used by GridLayout and means UNDEFINED.
    // For our rendering purposes we consider that to mean 0.
    // See https://developer.android.com/reference/android/support/v7/widget/GridLayout.html
    MarginLeft := Result.GetIntProp('layout:layout_leftMargin');
    if MarginLeft = UndefinedMargin then
      MarginLeft := 0;

    MarginTop := Result.GetIntProp('layout:layout_topMargin');
    if MarginTop = UndefinedMargin then
      MarginTop := 0;

    MarginRight := Result.GetIntProp('layout:layout_rightMargin');
    if MarginRight = UndefinedMargin then
      MarginRight := 0;

    MarginBottom := Result.GetIntProp('layout:layout_bottomMargin');
    if MarginBottom = UndefinedMargin then
      MarginBottom := 0;

    MarginLeft := MarginLeft * TransformScaleX;
    MarginTop := MarginTop * TransformScaleY;
    MarginRight := MarginRight * TransformScaleX;
    MarginBottom := MarginBottom * TransformScaleY;

    Result.SetBounds(Left, Top, Right, Bottom, Depth);
    Result.SetPaddings(PaddingLeft, PaddingTop, PaddingRight, PaddingBottom);
    Result.SetMargins(MarginLeft, MarginTop, MarginRight, MarginBottom);
    Result.TransformScaleX := TransformScaleX;
    Result.TransformScaleY := TransformScaleY;

    if Assigned(AParent) then
      AParent.AddChild(Result);
  except
    Result.Free;
    raise;
  end;
end;

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
var
  Response: TStream;
  B: array[0..Length(CallWindowSuccessResponse) - 1] of char;
begin
  Response := FAdbDeviceConnection.ShellExecute('service call window 3 i32 ' +
    IntToStr(ViewServerPort));
  try
    Response.ReadBuffer(B{%H-}, Length(B));
    Result := B = CallWindowSuccessResponse;
  finally
    Response.Free;
  end;
end;

function TViewServerClient.StartServer: boolean;
var
  Response: TStream;
  B: array[0..Length(CallWindowSuccessResponse) - 1] of char;
begin
  Response := FAdbDeviceConnection.ShellExecute('service call window 1 i32 ' +
    IntToStr(ViewServerPort));
  try
    Response.ReadBuffer(B{%H-}, Length(B));
    Result := B = CallWindowSuccessResponse;
  finally
    Response.Free;
  end;
end;

function TViewServerClient.StopServer: boolean;
var
  Response: TStream;
  B: array[0..Length(CallWindowSuccessResponse) - 1] of char;
begin
  Response := FAdbDeviceConnection.ShellExecute('service call window 2 i32 ' +
    IntToStr(ViewServerPort));
  try
    Response.ReadBuffer(B{%H-}, Length(B));
    Result := B = CallWindowSuccessResponse;
  finally
    Response.Free;
  end;
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

class function TViewServerClient.LoadDumpWindowFile: TView;
var
  CurrentView: TView = nil;
  CurrentDepth: integer = -1;
  Depth: integer;
  Line: PChar;
  S: string;
  DumpFile: TextFile;
begin
  AssignFile(DumpFile, DumpWindowFileName);
  Reset(DumpFile);
  try
    try
      repeat
        ReadLn(DumpFile, S);
        Line := PChar(S);
        if (Line = DONEp) or (Line = DONE) then
          Break;

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

        CurrentView := ParseDumpLine(CurrentView, Line, Depth);
        CurrentDepth := Depth;
      until EOF(DumpFile);
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
      while Assigned(CurrentView.Parent) do
        CurrentView := CurrentView.Parent
    else
      raise Exception.CreateFmt('''%s'' has no root view', [DumpWindowFileName]);

    Result := CurrentView;
  finally
    CloseFile(DumpFile);
  end;
end;

function TViewServerClient.DumpWindow(const WindowHash: string;
  const CheckCanceled: TObjectProcedure): TView;
var
  CurrentView: TView = nil;
  CurrentDepth: integer = -1;
  Depth: integer;
  Line: PChar;
  {$IFDEF DEBUG}
  DumpFile: TFileStream;
  {$ENDIF}
begin
  {$IFDEF DEBUG}
  DumpFile := TFileStream.Create(DumpWindowFileName, fmCreate or fmShareDenyWrite);
  {$ENDIF}
  try
    FAdbDeviceConnection.ConnectTcp(ViewServerPort);
    try
      FAdbDeviceConnection.Socket.SendString('DUMP ' + WindowHash + #10);
      try
        repeat
          Line := PChar(FAdbDeviceConnection.Socket.RecvTerminated(-1, #10));
          {$IFDEF DEBUG}
          DumpFile.WriteBuffer(Line[0], strlen(Line));
          DumpFile.WriteBuffer(LineEnding[1], Length(LineEnding));
          {$ENDIF}
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

          CurrentView := ParseDumpLine(CurrentView, Line, Depth);
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
        while Assigned(CurrentView.Parent) do
          CurrentView := CurrentView.Parent
      else
        raise Exception.CreateFmt('Window ''%s'' has no root view', [WindowHash]);

      Result := CurrentView;
    finally
      FAdbDeviceConnection.Disconnect;
    end;
  finally
    {$IFDEF DEBUG}
    DumpFile.Free;
    {$ENDIF}
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
        begin
          Result.Free;
          raise;
        end;
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
      FResult := GetDevices;
    finally
      Free;
    end;
end;

procedure TDeviceListTask.DoOnSuccess;
begin
  inherited;

  if Assigned(FOnResult) then
    FOnResult(Self, FResult);
end;

{ TWindowListTask }

constructor TWindowListTask.Create(const DeviceSerial: string);
begin
  inherited Create;
  FDeviceSerial := DeviceSerial;
end;

procedure TWindowListTask.Run;
begin
  with TViewServerClient.Create(FDeviceSerial) as IViewServerClient do
    if IsServerRunning or StartServer then
      FResult := GetWindowList(10000);
end;

procedure TWindowListTask.DoOnSuccess;
begin
  inherited;

  if Assigned(FOnResult) then
    FOnResult(Self, FResult);
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

function TAdbDeviceConnection.GetSocket: TTCPBlockSocket;
begin
  Result := FAdbConnection.Socket;
end;

constructor TAdbDeviceConnection.Create(const ADevice: string);
begin
  FAdbConnection := TAdbConnection.Create;
  FDevice := ADevice;
end;

destructor TAdbDeviceConnection.Destroy;
begin
  FAdbConnection.Free;
  inherited Destroy;
end;

procedure TAdbDeviceConnection.Connect;
begin
  FAdbConnection.Connect;
  try
    FAdbConnection.SendRequest('host:transport:' + FDevice);
  except
    Disconnect;
    raise;
  end;
end;

procedure TAdbDeviceConnection.Disconnect;
begin
  FAdbConnection.Disconnect;
end;

function TAdbDeviceConnection.ShellExecute(const Command: string): TStream;
begin
  Connect;
  try
    FAdbConnection.SendRequest('shell:' + Command);
    Result := TAdbDeviceConnectionStream.Create(Self);
  except
    Disconnect;
    raise;
  end;
end;

procedure TAdbDeviceConnection.ConnectTcp(Port: integer);
begin
  Connect;
  try
    FAdbConnection.SendRequest('tcp:' + IntToStr(Port));
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

procedure TDeviceInterface.WindowListResult(const Task: ITask;
  const TheResult: TWindowManagerEntryArray);
begin
  if Assigned(OnWindowListComplete) then
    OnWindowListComplete(Self, TheResult);
end;

procedure TDeviceInterface.WindowListError(const Task: ITask; Error: Exception);
begin
  if Assigned(OnWindowListError) then
    OnWindowListError(Self);
end;

procedure TDeviceInterface.WindowListStopped(const Task: ITask);
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
    OnResult := @WindowListResult;
    OnError := @WindowListError;
    OnStopped := @WindowListStopped;
    FWindowListTask := Start;
  end;
end;

{ TAdbConnection }

constructor TAdbConnection.Create;
begin
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

procedure TAdbInterface.DeviceListResult(const Task: ITask;
  const TheResult: TAdbDeviceEntryArray);
begin
  if Assigned(OnDeviceListComplete) then
    OnDeviceListComplete(Self, TheResult);
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
    OnResult := @DeviceListResult;
    OnError := @DeviceListError;
    FDeviceListTask := Start;
  end;
end;

end.
