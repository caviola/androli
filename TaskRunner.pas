unit TaskRunner;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  { TTask }

  TTask = class
  protected
    FCanceled: boolean;
    FOnComplete: TNotifyEvent;
    FOnError: TNotifyEvent;
    procedure Run; virtual; abstract;
    procedure DoComplete;
    procedure DoError;
    procedure DoFree;
  public
    procedure Cancel;
    property Canceled: boolean read FCanceled;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

procedure StartTask(ATask: TTask);

implementation

uses SysUtils, syncobjs, contnrs, LazLogger, Logger;

type

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FQueueLock: TRTLCriticalSection;
    FQueue: TObjectQueue;
    FDequeueEvent: TEventObject;
  protected
    procedure Execute; override;
    function PopTask: TTask;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Shutdown;
    procedure PushTask(ATask: TTask);
  end;

var
  WorkerThread: TWorkerThread;

procedure StartTask(ATask: TTask);
begin
  WorkerThread.PushTask(ATask);
end;

{ TTaskWorkerThread }

constructor TWorkerThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  InitCriticalSection(FQueueLock);
  FQueue := TObjectQueue.Create;
  FDequeueEvent := TEventObject.Create(nil, False, False, EmptyStr);
end;

destructor TWorkerThread.Destroy;
begin
  FDequeueEvent.Free;
  FQueue.Free;
  DoneCriticalSection(FQueueLock);
  inherited Destroy;
end;

procedure TWorkerThread.Shutdown;
var
  T: TObject;
begin
  // Free all tasks left in the queue.
  EnterCriticalSection(FQueueLock);
  try
    repeat
      T := FQueue.Pop;
      if Assigned(T) then
        T.Free;
    until T = nil;
  finally
    LeaveCriticalSection(FQueueLock);
  end;

  Terminate;
  // Task queue is now empty.
  // Setting the event will wake up PopTask(), which will return nil.
  FDequeueEvent.SetEvent;
end;

procedure TWorkerThread.PushTask(ATask: TTask);
begin
  EnterCriticalSection(FQueueLock);
  try
    FQueue.Push(ATask);
  finally
    LeaveCriticalSection(FQueueLock);
  end;

  FDequeueEvent.SetEvent;
end;

procedure TWorkerThread.Execute;
var
  T: TTask;
begin
  repeat
    T := PopTask;
    if not Assigned(T) then
      Break; // queue is empty, breaking loop to shut down

    // Execute task if not canceled while queued.
    if not T.Canceled then
    begin
      try
        LogDebug('[%s.Execute] [%s.Run %s]', [ClassName, T.ClassName, DbgS(T)]);
        T.Run;
        if not T.Canceled then
          Queue(@T.DoComplete);
      except
        LogDebug('[%s.Execute] [%s.Run %s] ERROR: %s',
          [ClassName, T.ClassName, DbgS(T), Exception(ExceptObject).Message]);

        if not T.Canceled then
          Queue(@T.DoError);
      end;
    end;

    // Note that we take complete ownership of the task object and
    // we free it when we're done with it.
    Queue(@T.DoFree);
  until Terminated;

  LogDebug('[%s.Execute] END', [ClassName]);
end;

function TWorkerThread.PopTask: TTask;
begin
  // Wait forever until a task is queued.
  FDequeueEvent.WaitFor(INFINITE);
  EnterCriticalSection(FQueueLock);
  try
    Result := TTask(FQueue.Pop);
  finally
    LeaveCriticalSection(FQueueLock);
  end;
end;

{ TTask }

procedure TTask.DoComplete;
begin
  LogDebug('[%s.DoComplete %s]', [ClassName, DbgS(Self)]);
  if Assigned(OnComplete) then
    OnComplete(Self);
end;

procedure TTask.DoError;
begin
  LogDebug('[%s.DoError %s]', [ClassName, DbgS(Self)]);
  if Assigned(OnError) then
    OnError(Self);
end;

procedure TTask.DoFree;
begin
  LogDebug('[%s.DoFree %s]', [ClassName, DbgS(Self)]);
  Free;
end;

procedure TTask.Cancel;
begin
  LogDebug('[%s.Cancel %s]', [ClassName, DbgS(Self)]);
  FCanceled := True;
  // Dequeue these events in case they were queued but not yet executed.
  TThread.RemoveQueuedEvents(WorkerThread, @DoComplete);
  TThread.RemoveQueuedEvents(WorkerThread, @DoError);
end;

initialization
  WorkerThread := TWorkerThread.Create;
  WorkerThread.Start;

finalization;
  WorkerThread.Shutdown;

end.
