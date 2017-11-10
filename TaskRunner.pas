unit TaskRunner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type

  TObjectProcedure = procedure of object;

  ITask = interface;

  TTaskNotifyEvent = procedure(const Task: ITask) of object;

  TTaskErrorEvent = procedure(const Task: ITask; Error: Exception) of object;

  { ITask }

  ITask = interface
    ['{196E6D82-4AFA-48B7-8933-A6587702EC00}']
    // Will be executed on UI thread.
    procedure DoOnStarted;
    // Will be executed on UI thread.
    procedure DoOnSuccess;
    // Will be executed on UI thread.
    procedure DoOnError(Error: Exception);
    // Will be executed on UI thread.
    procedure DoOnStopped;
    // Must be called from UI thread.
    procedure Cancel;
    // Will be executed in arbitrary, non-UI thread.
    procedure Run;
    function GetCanceled: boolean;
    property Canceled: boolean read GetCanceled;
  end;

  { ETaskCanceled }

  ETaskCanceled = class(Exception);

  TTaskStatus = (tsReady, tsStarted, tsCanceled, tsStopped);

  { TTask }

  TTask = class(TInterfacedObject, ITask)
  private
    FStatus: TTaskStatus;
    FOnStarted: TTaskNotifyEvent;
    FOnStopped: TTaskNotifyEvent;
    FOnSuccess: TTaskNotifyEvent;
    FOnError: TTaskErrorEvent;
  protected
    procedure CheckCanceled;
    procedure Run; virtual; abstract;
    function GetCanceled: boolean;
    procedure DoOnStarted; virtual;
    procedure DoOnSuccess; virtual;
    procedure DoOnError(Error: Exception); virtual;
    procedure DoOnStopped; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // During this call only OnStopped will be fired,
    // no OnSuccess/OnError afterwards.
    procedure Cancel; virtual;
    // During this call OnStarted will be fired.
    // Must be called from UI thread.
    function Start: ITask;
    property OnStarted: TTaskNotifyEvent write FOnStarted;
    property OnSuccess: TTaskNotifyEvent write FOnSuccess;
    property OnError: TTaskErrorEvent write FOnError;
    property OnStopped: TTaskNotifyEvent write FOnStopped;
  end;

implementation

uses syncobjs, LCLProc, Logging, gdeque;

type

  { ITaskEntry }

  ITaskEntry = interface
    // Will be executed on UI thread.
    procedure DoOnStopped;
    // Will be executed on UI thread.
    procedure DoOnSuccess;
    // Will be executed on UI thread.
    procedure DoOnError;
    // Will be executed on arbitrary, non-UI thread.
    procedure DoRun;
    function GetCanceled: boolean;
    property Canceled: boolean read GetCanceled;
  end;

  { TTaskEntry }

  TTaskEntry = class(TInterfacedObject, ITaskEntry)
  private
    FTask: ITask;
  protected
    procedure DoOnStopped;
    procedure DoOnSuccess;
    procedure DoOnError;
    procedure DoRun;
    function GetCanceled: boolean;
  public
    constructor Create(const ATask: ITask);
    destructor Destroy; override;
  end;

  TTaskEntryDeque = specialize TDeque<ITaskEntry>;

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FDequeLock: TRTLCriticalSection;
    FDeque: TTaskEntryDeque;
    FDequeEvent: TEventObject;
  protected
    procedure Execute; override;
    function PopEntry: ITaskEntry;
    procedure Shutdown;
  public
    constructor Create;
    destructor Destroy; override;
    function PushTask(const ATask: ITask): ITask;
  end;

var
  WorkerThread: TWorkerThread;

{ TTaskQueueEntry }

constructor TTaskEntry.Create(const ATask: ITask);
begin
  FTask := ATask;
end;

destructor TTaskEntry.Destroy;
begin
  FTask := nil;
  inherited;
end;

// DoOnStopped, DoOnSuccess and DoOnError are executed in the UI thread
// with Synchronize (see TWorkerThread.Execute).
// We check for Canceled in case the task was canceled while
// these methods were waiting in the UI thread's message queue.
procedure TTaskEntry.DoOnStopped;
begin
  if not FTask.Canceled then
    FTask.DoOnStopped;
end;

procedure TTaskEntry.DoOnSuccess;
begin
  if not FTask.Canceled then
    FTask.DoOnSuccess;
end;

procedure TTaskEntry.DoOnError;
begin
  if not FTask.Canceled then
    FTask.DoOnError(Exception(ExceptObject));
end;

procedure TTaskEntry.DoRun;
begin
  // Since ITask.Run may be called long after ITask.DoOnStarted was called,
  // here we check for Canceled in case the task was canceled while waiting
  // in the task queue.
  if not FTask.Canceled then
  begin
    Log('TTaskEntry.DoRun: Task=%s', [DbgS(Pointer(FTask))]);
    FTask.Run;
  end;
end;

function TTaskEntry.GetCanceled: boolean;
begin
  Result := FTask.Canceled;
end;

{ TTaskWorkerThread }

constructor TWorkerThread.Create;
begin
  inherited Create(True);
  InitCriticalSection(FDequeLock);
  FDeque := TTaskEntryDeque.Create;
  FDequeEvent := TEventObject.Create(nil, True, False, EmptyStr);
end;

destructor TWorkerThread.Destroy;
begin
  Shutdown;
  WaitFor;
  FDequeEvent.Free;
  FDeque.Free;
  DoneCriticalSection(FDequeLock);
  inherited;
end;

procedure TWorkerThread.Shutdown;
begin
  Log('TWorkerThread.Shutdown');

  // Clear the task queue.
  EnterCriticalSection(FDequeLock);
  try
    while not FDeque.IsEmpty() do
    begin
      FDeque.Items[0] := nil; // release ITaskEntry reference at front
      FDeque.PopFront;
    end;
  finally
    LeaveCriticalSection(FDequeLock);
  end;

  Terminate;
  // Task queue is now empty.
  // Setting the event will wake up PopEntry(), which will return nil.
  FDequeEvent.SetEvent;
end;

function TWorkerThread.PushTask(const ATask: ITask): ITask;
begin
  EnterCriticalSection(FDequeLock);
  try
    Log('TWorkerThread.PushTask %s', [DbgS(Pointer(ATask))]);
    FDeque.PushBack(TTaskEntry.Create(ATask));
  finally
    LeaveCriticalSection(FDequeLock);
  end;

  FDequeEvent.SetEvent;
  Result := ATask;
end;

procedure TWorkerThread.Execute;
var
  Entry: ITaskEntry;
begin
  repeat
    Entry := PopEntry;
    if not Assigned(Entry) then
      Break; // queue is empty, breaking loop to shut down

    try
      Entry.DoRun;
      Synchronize(@Entry.DoOnSuccess);
    except
      LogException('TWorkerThread.Execute', Exception(ExceptObject));

      // Don't report the error if task is canceled.
      // Note that if ETaskCanceled was raised, ITask.Canceled is also true.
      if not Entry.Canceled then
        Synchronize(@Entry.DoOnError);
    end;

    // Don't call DoOnStopped if task was canceled because it was already
    // called inside TTask.Cancel.
    if not Entry.Canceled then
      Synchronize(@Entry.DoOnStopped);

    Entry := nil;
  until Terminated;
end;

function TWorkerThread.PopEntry: ITaskEntry;
begin
  // Wait until a task is queued or the event is set because
  // the worker thread is shutting down.
  FDequeEvent.WaitFor(INFINITE);
  EnterCriticalSection(FDequeLock);
  try
    if not FDeque.IsEmpty then
    begin
      Result := FDeque.Items[0];
      FDeque.Items[0] := nil; // release ITaskEntry reference at front
      FDeque.PopFront;
    end
    else
      Result := nil; // event was set because Shutdown() was called

    if FDeque.IsEmpty then
      FDequeEvent.ResetEvent;
  finally
    LeaveCriticalSection(FDequeLock);
  end;
end;

{ TTask }

constructor TTask.Create;
begin
  FStatus := tsReady;
end;

destructor TTask.Destroy;
begin
  Log('TTask.Destroy %s', [DbgS(Pointer(ITask(Self)))]);
  inherited;
end;

procedure TTask.CheckCanceled;
begin
  if FStatus = tsCanceled then
    raise ETaskCanceled.Create('Task explicitly canceled');
end;

function TTask.GetCanceled: boolean;
begin
  Result := FStatus = tsCanceled;
end;

procedure TTask.DoOnStarted;
begin
  if Assigned(FOnStarted) then
  begin
    Log('TTask.DoOnStarted %s', [DbgS(Pointer(ITask(Self)))]);
    FOnStarted(Self);
  end;
end;

procedure TTask.DoOnStopped;
begin
  if FStatus <> tsCanceled then
    FStatus := tsStopped;

  if Assigned(FOnStopped) then
  begin
    Log('TTask.DoOnStopped %s', [DbgS(Pointer(ITask(Self)))]);
    FOnStopped(Self);
  end;
end;

procedure TTask.DoOnSuccess;
begin
  if Assigned(FOnSuccess) then
  begin
    Log('TTask.DoOnSuccess %s', [DbgS(Pointer(ITask(Self)))]);
    FOnSuccess(Self);
  end;
end;

procedure TTask.DoOnError(Error: Exception);
begin
  if Assigned(FOnError) then
  begin
    Log('TTask.DoOnError %s', [DbgS(Pointer(ITask(Self)))]);
    FOnError(Self, Error);
  end;
end;

procedure TTask.Cancel;
begin
  if FStatus = tsStarted then
  begin
    FStatus := tsCanceled;
    Log('TTask.Cancel %s', [DbgS(Pointer(ITask(Self)))]);
    DoOnStopped;
  end;
end;

function TTask.Start: ITask;
begin
  if FStatus = tsReady then
  begin
    FStatus := tsStarted;
    DoOnStarted;
    Result := WorkerThread.PushTask(Self);
  end;
end;

initialization
  WorkerThread := TWorkerThread.Create;
  WorkerThread.Start;

finalization;
  WorkerThread.Free;

end.
