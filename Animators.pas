unit Animators;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TAnimator = class;

  TAnimatorFrameUpdateEvent = procedure(Sender: TAnimator;
    const InterpolatedFraction: single) of object;

  TInterpolatorFunction = function(const Fraction: single): single;

  TAnimatorState = (asNone, asStarted);

  { TAnimator }

  TAnimator = class
  private
    FState: TAnimatorState;
    FStartTick: QWord;
    FDuration: cardinal;
    FInterpolator: TInterpolatorFunction;
    FOnFinish: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnUpdate: TAnimatorFrameUpdateEvent;
  protected
    procedure DoFrameTick;
    procedure DoFrameUpdate(const InterpolatedFraction: single); virtual;
    procedure DoFinish; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Cancel; inline;
    procedure Restart; inline;
    procedure Finish; inline;
    function IsRunning: boolean; inline;
    property Duration: cardinal read FDuration write FDuration;
    property Interpolator: TInterpolatorFunction read FInterpolator write FInterpolator;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnUpdate: TAnimatorFrameUpdateEvent read FOnUpdate write FOnUpdate;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  TIntegerAnimator = class;

  TAnimateIntegerValueEvent = procedure(Sender: TIntegerAnimator;
    Value: integer) of object;

  { TIntegerAnimator }

  TIntegerAnimator = class(TAnimator)
  private
    FStartValue: integer;
    FEndValue: integer;
    FOnAnimateValue: TAnimateIntegerValueEvent;
  protected
    procedure DoFrameUpdate(const InterpolatedFraction: single); override;
  public
    constructor Create(AOnAnimateValue: TAnimateIntegerValueEvent = nil);
    procedure SetValueInterval(StartValue, EndValue: integer);
    property OnAnimateValue: TAnimateIntegerValueEvent
      read FOnAnimateValue write FOnAnimateValue;
  end;

  TFloatAnimator = class;

  TAnimateFloatValueEvent = procedure(Sender: TFloatAnimator;
    Value: single) of object;

  { TFloatAnimator }

  TFloatAnimator = class(TAnimator)
  private
    FStartValue: single;
    FEndValue: single;
    FOnAnimateValue: TAnimateFloatValueEvent;
  protected
    procedure DoFrameUpdate(const InterpolatedFraction: single); override;
  public
    constructor Create(AOnAnimateValue: TAnimateFloatValueEvent = nil);
    procedure SetValueInterval(StartValue, EndValue: single);
    property EndValue: single read FEndValue;
    property OnAnimateValue: TAnimateFloatValueEvent
      read FOnAnimateValue write FOnAnimateValue;
  end;

  TFloatArrayAnimator = class;

  TAnimateFloatArrayEvent = procedure(Sender: TFloatArrayAnimator;
    const Values: array of single) of object;

  { TFloatArrayAnimator }

  TFloatArrayAnimator = class(TAnimator)
  private
    FOnAnimateArray: TAnimateFloatArrayEvent;
    FStartValues: array of single;
    FEndValues: array of single;
    FInterpolatedValues: array of single;
    function GetEndValues(I: integer): single; inline;
  protected
    procedure DoFrameUpdate(const InterpolatedFraction: single); override;
  public
    constructor Create(Count: integer = 1;
      AOnAnimateArray: TAnimateFloatArrayEvent = nil);
    procedure SetElementInterval(Index: integer;
      const StartValue, EndValue: single); inline;
    property EndValues[I: integer]: single read GetEndValues;
    property OnAnimateArray: TAnimateFloatArrayEvent
      read FOnAnimateArray write FOnAnimateArray;
  end;


function LinearInterpolator(const Fraction: single): single; inline;
// Accelerating from zero velocity.
function EaseInQuadInterpolator(const Fraction: single): single; inline;
// Decelerating to zero velocity.
function EaseOutQuadInterpolator(const Fraction: single): single; inline;


function IntegerEvaluator(const Fraction: single;
  const StartValue, EndValue: integer): integer; inline;
function FloatEvaluator(const Fraction: single;
  const StartValue, EndValue: single): single;
  inline;


implementation

uses
  CustomTimer, fgl, LCLIntf;

const
  AnimationTickInterval = 10; // milliseconds
  DefaultAnimationDuration = 200; // milliseconds

function LinearInterpolator(const Fraction: single): single;
begin
  Result := Fraction;
end;

function EaseInQuadInterpolator(const Fraction: single): single;
begin
  Result := Fraction * Fraction;
end;

function EaseOutQuadInterpolator(const Fraction: single): single;
begin
  Result := Fraction * (2 - Fraction);
end;

function IntegerEvaluator(const Fraction: single;
  const StartValue, EndValue: integer): integer;
begin
  Result := Round(StartValue * (1 - Fraction) + EndValue * Fraction);
end;

function FloatEvaluator(const Fraction: single;
  const StartValue, EndValue: single): single;
begin
  Result := StartValue * (1 - Fraction) + EndValue * Fraction;
end;

type
  TAnimatorTickHandler = procedure of object;

  TAnimatorTickHandlerList = specialize TFPGList<TAnimatorTickHandler>;

  { TAnimatorTickTimer }

  TAnimatorTickTimer = class(TCustomTimer)
  private
    FTickHandlers: TAnimatorTickHandlerList;
  protected
    procedure TimerHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTickHandler(AHandler: TAnimatorTickHandler);
    procedure RemoveTickHandler(AHandler: TAnimatorTickHandler);
  end;

var
  AnimationTickTimer: TAnimatorTickTimer;

{ TFloatArrayAnimator }

function TFloatArrayAnimator.GetEndValues(I: integer): single;
begin
  Result := FEndValues[I];
end;

procedure TFloatArrayAnimator.DoFrameUpdate(const InterpolatedFraction: single);
var
  I: integer;
begin
  inherited DoFrameUpdate(InterpolatedFraction);

  if Assigned(FOnAnimateArray) then
  begin
    for I := 0 to Length(FInterpolatedValues) - 1 do
      FInterpolatedValues[I] :=
        FloatEvaluator(InterpolatedFraction, FStartValues[I], FEndValues[I]);
    FOnAnimateArray(Self, FInterpolatedValues);
  end;
end;

constructor TFloatArrayAnimator.Create(Count: integer;
  AOnAnimateArray: TAnimateFloatArrayEvent);
begin
  inherited Create;
  FOnAnimateArray := AOnAnimateArray;
  SetLength(FStartValues, Count);
  SetLength(FEndValues, Count);
  SetLength(FInterpolatedValues, Count);
end;

procedure TFloatArrayAnimator.SetElementInterval(Index: integer;
  const StartValue, EndValue: single);
begin
  FStartValues[Index] := StartValue;
  FEndValues[Index] := EndValue;
end;

{ TFloatAnimator }

procedure TFloatAnimator.DoFrameUpdate(const InterpolatedFraction: single);
begin
  if Assigned(FOnAnimateValue) then
    FOnAnimateValue(Self, FloatEvaluator(InterpolatedFraction,
      FStartValue, FEndValue));
end;

constructor TFloatAnimator.Create(AOnAnimateValue: TAnimateFloatValueEvent);
begin
  inherited Create;
  FOnAnimateValue := AOnAnimateValue;
end;

procedure TFloatAnimator.SetValueInterval(StartValue, EndValue: single);
begin
  FStartValue := StartValue;
  FEndValue := EndValue;
end;

{ TIntegerAnimator }

procedure TIntegerAnimator.DoFrameUpdate(const InterpolatedFraction: single);
begin
  if Assigned(FOnAnimateValue) then
    FOnAnimateValue(Self, IntegerEvaluator(InterpolatedFraction,
      FStartValue, FEndValue));
end;

constructor TIntegerAnimator.Create(AOnAnimateValue: TAnimateIntegerValueEvent);
begin
  inherited Create;
  FOnAnimateValue := AOnAnimateValue;
end;

procedure TIntegerAnimator.SetValueInterval(StartValue, EndValue: integer);
begin
  FStartValue := StartValue;
  FEndValue := EndValue;
end;

{ TAnimatorTickTimer }

procedure TAnimatorTickTimer.TimerHandler(Sender: TObject);
var
  I: integer;
begin
  // Execute all tick callbacks in the list.
  // Note that we traverse the list backwards.
  // This is very important to avoid "index out of bounds" exceptions
  // while traversing the list given that a callback may remove itself
  // from it, eg: when an animation finishes.
  for I := FTickHandlers.Count - 1 downto 0 do
    FTickHandlers.Items[I]();
end;

constructor TAnimatorTickTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTickHandlers := TAnimatorTickHandlerList.Create;
  Interval := AnimationTickInterval;
  OnTimer := @TimerHandler;
end;

destructor TAnimatorTickTimer.Destroy;
begin
  FTickHandlers.Free;
  inherited Destroy;
end;

procedure TAnimatorTickTimer.AddTickHandler(AHandler: TAnimatorTickHandler);
begin
  FTickHandlers.Add(AHandler);
  Enabled := True;
end;

procedure TAnimatorTickTimer.RemoveTickHandler(AHandler: TAnimatorTickHandler);
begin
  FTickHandlers.Remove(AHandler);
  Enabled := FTickHandlers.Count > 0;
end;

{ TAnimator }

constructor TAnimator.Create;
begin
  FState := asNone;
  FDuration := DefaultAnimationDuration;
  FInterpolator := @EaseOutQuadInterpolator;
end;

destructor TAnimator.Destroy;
begin
  Cancel;
  inherited;
end;

procedure TAnimator.DoFrameTick;
var
  Ellapsed: QWord;
begin
  if FStartTick = 0 then  // first tick?
  begin
    FStartTick := GetTickCount64;
    if Assigned(FOnStart) then
      FOnStart(Self);
    DoFrameUpdate(0); // update first frame (fraction=0)
  end
  else
  begin
    Ellapsed := GetTickCount64 - FStartTick;
    if Ellapsed < Duration then
      DoFrameUpdate(Interpolator(Ellapsed / Duration))
    else
    begin
      DoFrameUpdate(1); // update last frame (fraction=1)
      DoFinish;
    end;
  end;
end;

procedure TAnimator.DoFrameUpdate(const InterpolatedFraction: single);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, InterpolatedFraction);
end;

procedure TAnimator.Start;
begin
  if FState = asNone then
  begin
    FState := asStarted;
    AnimationTickTimer.AddTickHandler(@DoFrameTick);
  end;
end;

procedure TAnimator.Cancel;
begin
  if FState = asStarted then
    DoFinish;
end;

procedure TAnimator.Restart;
begin
  Cancel;
  Start;
end;

procedure TAnimator.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
  AnimationTickTimer.RemoveTickHandler(@DoFrameTick);
  FStartTick := 0;
  FState := asNone;
end;

procedure TAnimator.Finish;
begin
  if FState = asStarted then
  begin
    DoFrameUpdate(1); // update last frame (fraction=1)
    DoFinish;
  end;
end;

function TAnimator.IsRunning: boolean;
begin
  Result := FState = asStarted;
end;

initialization
  AnimationTickTimer := TAnimatorTickTimer.Create(nil);

finalization
  AnimationTickTimer.Free;

end.
