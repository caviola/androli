unit TaskRunner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type

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
    procedure DoOnStarted;
    procedure DoOnSuccess;
    procedure DoOnError(Error: Exception);
    procedure DoOnStopped;
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

uses syncobjs, LazLogger, Logger, gqueue;

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

  TTaskEntryQueue = specialize TQueue<ITaskEntry>;

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FQueueLock: TRTLCriticalSection;
    FQueue: TTaskEntryQueue;
    FDequeueEvent: TEventObject;
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
    LogDebug('[TTask.Run %s]', [DbgS(FTask)]);
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
  InitCriticalSection(FQueueLock);
  FQueue := TTaskEntryQueue.Create;
  FDequeueEvent := TEventObject.Create(nil, True, False, EmptyStr);
end;

destructor TWorkerThread.Destroy;
begin
  Shutdown;
  WaitFor;
  FDequeueEvent.Free;
  FQueue.Free;
  DoneCriticalSection(FQueueLock);
  inherited;
end;

procedure TWorkerThread.Shutdown;
begin
  // Clear the task queue.
  EnterCriticalSection(FQueueLock);
  try
    while not FQueue.IsEmpty() do
      FQueue.Pop;
  finally
    LeaveCriticalSection(FQueueLock);
  end;

  Terminate;
  // Task queue is now empty.
  // Setting the event will wake up PopEntry(), which will return nil.
  FDequeueEvent.SetEvent;
end;

function TWorkerThread.PushTask(const ATask: ITask): ITask;
begin
  EnterCriticalSection(FQueueLock);
  try
    FQueue.Push(TTaskEntry.Create(ATask));
  finally
    LeaveCriticalSection(FQueueLock);
  end;

  FDequeueEvent.SetEvent;
  Result := ATask;
end;

procedure TWorkerThread.Execute;
var
  Entry: ITaskEntry;
begin
  LogDebug('[TWorkerThread.Execute] begin');

  repeat
    Entry := PopEntry;
    if not Assigned(Entry) then
      Break; // queue is empty, breaking loop to shut down

    try
      Entry.DoRun;
      Synchronize(@Entry.DoOnSuccess);
    except
      LogDebug('[TWorkerThread.Execute] exception: %s',
        [Exception(ExceptObject).Message]);

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

  LogDebug('[TWorkerThread.Execute] end');
end;

function TWorkerThread.PopEntry: ITaskEntry;
begin
  // Wait until a task is queued or the event is set because
  // the worker thread is shutting down.
  FDequeueEvent.WaitFor(INFINITE);
  EnterCriticalSection(FQueueLock);
  try
    if not FQueue.IsEmpty then
    begin
      Result := FQueue.Front;
      FQueue.Pop;
    end
    else
      Result := nil; // event was set because Shutdown() was called

    if FQueue.IsEmpty then
      FDequeueEvent.ResetEvent;
  finally
    LeaveCriticalSection(FQueueLock);
  end;
end;

{ TTask }

constructor TTask.Create;
begin
  FStatus := tsReady;
end;

destructor TTask.Destroy;
begin
  LogDebug('[TTask.Destroy %s]', [DbgS(Self)]);
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
    LogDebug('[TTask.DoOnStarted %s]', [DbgS(Self)]);
    FOnStarted(Self);
  end;
end;

procedure TTask.DoOnStopped;
begin
  if FStatus <> tsCanceled then
    FStatus := tsStopped;

  if Assigned(FOnStopped) then
  begin
    LogDebug('[TTask.DoOnStopped %s]', [DbgS(Self)]);
    FOnStopped(Self);
  end;
end;

procedure TTask.DoOnSuccess;
begin
  if Assigned(FOnSuccess) then
  begin
    LogDebug('[TTask.DoOnSuccess %s]', [DbgS(Self)]);
    FOnSuccess(Self);
  end;
end;

procedure TTask.DoOnError(Error: Exception);
begin
  if Assigned(FOnError) then
  begin
    LogDebug('[TTask.DoOnError %s]', [DbgS(Self)]);
    FOnError(Self, Error);
  end;
end;

procedure TTask.Cancel;
begin
  if FStatus = tsStarted then
  begin
    FStatus := tsCanceled;
    LogDebug('[TTask.Cancel %s]', [DbgS(Self)]);
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
