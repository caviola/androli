unit LayoutViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, ExtCtrls, ViewTypes, Animators, OpenGLContext,
  LMessages;

type

  TMouseState = (msNone, msDown, msDragging);

  TZOrderAnimator = class;

  TActiveViewChangedEvent = procedure(NewView: TView) of object;
  TActiveBranchChangedEvent = procedure(NewBranch: TView) of object;

  { TLayoutViewer }

  TLayoutViewer = class(TOpenGLControl)
  private
    FMode3D: boolean;
    FLayout: IViewLayout;
    FShowContent: boolean;
    FShowWireframes: boolean;
    FCurrentAnimator: TAnimator;
    FZOrderAnimator: TZOrderAnimator;
    FToggleMode3DAnimator: TFloatArrayAnimator;
    FScaleZAnimator: TFloatAnimator;
    FZoomLevelAnimator: TFloatAnimator;
    FResetCameraAnimator: TFloatArrayAnimator;
    FOriginX: single;
    FOriginY: single;
    FOriginZ: single;
    FHighlightedView: TView;
    FMouseState: TMouseState;
    FLastMouseX: integer;
    FLastMouseY: integer;
    FRotationX: single; // degrees
    FRotationY: single; // degrees
    FZoomLevel: single;
    FScaleZ: single;
    FCameraZ: single;
    FOnActiveViewChanged: TActiveViewChangedEvent;
    FOnActiveBranchChanged: TActiveBranchChangedEvent;
    FActiveViewChangedTimer: TTimer;
    function GetFinalOriginX: single; inline;
    function GetFinalOriginY: single; inline;
    function GetFinalOriginZ: single; inline;
    function GetFinalRotationX: single; inline;
    function GetFinalRotationY: single; inline;
    function GetFinalScaleZ: single; inline;
    function GetFinalZoomLevel: single; inline;
    procedure SetMode3D(AValue: boolean);
    procedure SetHighlightedView(AValue: TView);
    procedure SetOriginZ(AValue: single);
    procedure SetRotationX(Degres: single);
    procedure SetRotationY(Degres: single);
    procedure SetShowContent(AValue: boolean);
    procedure SetShowWireFrames(AValue: boolean);
    procedure SetZoomLevel(AValue: single);
    procedure SetScaleZ(AValue: single);
    procedure SetOriginX(AValue: single);
    procedure SetOriginY(AValue: single);
  protected
    procedure ActiveViewChangedTimerTimer(Sender: TObject);
    procedure LayoutAnimationStart(Sender: TObject);
    procedure ResetCameraAnimate(Sender: TFloatArrayAnimator;
      const Values: array of single);
    procedure MouseLeaveHandler(Sender: TObject);

    procedure ToggleMode3DAnimate(Sender: TFloatArrayAnimator;
      const Values: array of single);

    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseClickHandler(Sender: TObject);
    procedure MouseDblClickHandler(Sender: TObject);
    procedure MouseUpHandler(Sender: TObject; {%H-}Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MouseWheelHandler(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; {%H-}MousePos: TPoint; var Handled: boolean);

    procedure DoActiveViewChanged; inline;
    procedure ZoomLevelAnimate(Sender: TFloatAnimator; Value: single);
    procedure ScaleZAnimate(Sender: TFloatAnimator; Value: single);
    procedure ZOrderAnimatorUpdate(Sender: TAnimator;
      const {%H-}InterpolatedFraction: single);
    procedure GetActiveBranchCenter(
      out CenterX, CenterY, CenterZ, CameraDistance: single);
    procedure WMSize(var {%H-}Message: TLMSize); message LM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoOnPaint; override;
    procedure Zoom(Delta: integer);
    procedure Collapse(Root: TView);
    procedure Expand(Root: TView);
    procedure SetActiveBranch(AValue: TView);
    function SetActiveView(AValue: TView): boolean;
    procedure SetLayout(AValue: IViewLayout);
    procedure ResetCamera(ResetRotation: boolean; Animate: boolean = True);
    function SelectNextMatch: TView;
    function SelectPreviousMatch: TView;
    procedure SetViewSeparation(Delta: integer);
    property RotationX: single read FRotationX write SetRotationX;
    property RotationY: single read FRotationY write SetRotationY;
    property ZoomLevel: single read FZoomLevel write SetZoomLevel;
    property ScaleZ: single read FScaleZ write SetScaleZ;
    property OriginX: single read FOriginX write SetOriginX;
    property OriginY: single read FOriginY write SetOriginY;
    property OriginZ: single read FOriginZ write SetOriginZ;
    property HighlightedView: TView read FHighlightedView write SetHighlightedView;
    // Fired when user changes active view.
    property OnActiveViewChanged: TActiveViewChangedEvent
      read FOnActiveViewChanged write FOnActiveViewChanged;
    // Fires when user changes active branch.
    property OnActiveBranchChanged: TActiveBranchChangedEvent
      write FOnActiveBranchChanged;
    property Mode3D: boolean read FMode3D write SetMode3D;
    property ShowWireframes: boolean read FShowWireframes write SetShowWireframes;
    property ShowContent: boolean read FShowContent write SetShowContent;

    // The following properties return the equivalent property's value
    // (without the "Final" suffix) after any running animation have finished.
    // The idea is to get a property's value, eg. OriginX, as if
    // layout transitions occurred immediately, that is, without animations.
    property FinalRotationX: single read GetFinalRotationX;
    property FinalRotationY: single read GetFinalRotationY;
    property FinalZoomLevel: single read GetFinalZoomLevel;
    property FinalScaleZ: single read GetFinalScaleZ;
    property FinalOriginX: single read GetFinalOriginX;
    property FinalOriginY: single read GetFinalOriginY;
    property FinalOriginZ: single read GetFinalOriginZ;
  end;


  TZOrderAnimatorTargetRec = record
    View: TView;
    StartValue: single;
    EndValue: single;
  end;

  { TZOrderAnimator }

  TZOrderAnimator = class(TAnimator)
  private
    FTargets: array of TZOrderAnimatorTargetRec;
  protected
    procedure DoFrameUpdate(const InterpolatedFraction: single); override;
  public
    procedure ClearTargets;
    procedure AddTarget(View: TView; const StartValue, EndValue: single);
  end;

implementation

uses
  SysUtils, LCLIntf, LCLProc, Math, gl, glu, Logging;

type
  TColorRGBA = record
    case integer of
      0: (ABGR: GLuint);
      1: (
        R: GLubyte;
        G: GLubyte;
        B: GLubyte;
        A: GLubyte;
      );
  end;

const
  rgbaCanvasColor: TColorRGBA = (ABGR: $FF000000);
  rgbaActiveRectColor: TColorRGBA = (ABGR: $FFFF9430);
  rgbaFilterMatchRectColor: TColorRGBA = (ABGR: $C091EEFF);
  rgbaRectColor: TColorRGBA = (ABGR: $C0636363);
  rgbaFillColor: TColorRGBA = (ABGR: $70FF824A);
  rgbaPaddingColor: TColorRGBA = (ABGR: $50C3DEB7);
  rgbaMarginColor: TColorRGBA = (ABGR: $50A0C5E8);

  InitialZoomLevel = 1;
  InitialRotationY = 0;

  Mode3DScaleZ = 20;

  StepZoomLevel = 0.2;
  StepScaleZ = 20;

  MinZoomLevel = 0.2;
  MinScaleZ = 0;

  MaxZoomLevel = 10;
  MaxScaleZ = 200;

  MinRotationX = -90;
  MinRotationY = -90;

  MaxRotationX = 90;
  MaxRotationY = 90;

  MinCameraDistance = 1000;

  // FToggleMode3DAnimator element indexes.
  t3daRotationY = 0;
  t3daScaleZ = 1;

  // FResetCameraAnimator element indexes.
  rcaOriginX = 0;
  rcaOriginY = 1;
  rcaOriginZ = 2;
  rcaRotationY = 3;

{ TZOrderAnimator }

procedure TZOrderAnimator.DoFrameUpdate(const InterpolatedFraction: single);
var
  I: integer;
begin
  for I := 0 to Length(FTargets) - 1 do
    FTargets[I].View.ZOrder :=
      FloatEvaluator(InterpolatedFraction, FTargets[I].StartValue, FTargets[I].EndValue);
  inherited;
end;

procedure TZOrderAnimator.ClearTargets;
begin
  SetLength(FTargets, 0);
end;

procedure TZOrderAnimator.AddTarget(View: TView; const StartValue, EndValue: single);
var
  I: integer;
begin
  // TODO: should I optimize this list or leave it good-enough as it is now?
  I := Length(FTargets);
  SetLength(FTargets, I + 1);
  FTargets[I].View := View;
  FTargets[I].StartValue := StartValue;
  FTargets[I].EndValue := EndValue;
end;

{ TLayoutViewer }

constructor TLayoutViewer.Create(AOwner: TComponent);
begin
  inherited;

  FMouseState := msNone;
  FShowWireFrames := True;
  FShowContent := True;

  FActiveViewChangedTimer := TTimer.Create(Self);
  FActiveViewChangedTimer.Enabled := False;
  FActiveViewChangedTimer.Interval := GetDoubleClickTime;
  FActiveViewChangedTimer.OnTimer := @ActiveViewChangedTimerTimer;

  MultiSampling := 4;
  AutoResizeViewport := True;
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  FMode3D := True;
  FToggleMode3DAnimator := TFloatArrayAnimator.Create(2, @ToggleMode3DAnimate);
  FToggleMode3DAnimator.Duration := 300;
  FToggleMode3DAnimator.OnStart := @LayoutAnimationStart;

  FScaleZAnimator := TFloatAnimator.Create(@ScaleZAnimate);
  FScaleZAnimator.OnStart := @LayoutAnimationStart;

  FZoomLevelAnimator := TFloatAnimator.Create(@ZoomLevelAnimate);
  FZoomLevelAnimator.OnStart := @LayoutAnimationStart;

  FResetCameraAnimator := TFloatArrayAnimator.Create(4, @ResetCameraAnimate);
  FResetCameraAnimator.Duration := 300;
  FResetCameraAnimator.OnStart := @LayoutAnimationStart;

  FZOrderAnimator := TZOrderAnimator.Create;
  FZOrderAnimator.OnUpdate := @ZOrderAnimatorUpdate;
  FZOrderAnimator.OnStart := @LayoutAnimationStart;
end;

destructor TLayoutViewer.Destroy;
begin
  FZOrderAnimator.Free;
  FZoomLevelAnimator.Free;
  FScaleZAnimator.Free;
  FToggleMode3DAnimator.Free;
  FResetCameraAnimator.Free;
  inherited Destroy;
end;

procedure TLayoutViewer.Zoom(Delta: integer);
begin
  FZoomLevelAnimator.SetValueInterval(ZoomLevel,
    EnsureRange(FZoomLevel + Delta * StepZoomLevel, MinZoomLevel, MaxZoomLevel));
  FZoomLevelAnimator.Restart;
end;

procedure TLayoutViewer.Collapse(Root: TView);

  procedure Visit(View: TView);
  var
    ViewChild: TView;
  begin
    ViewChild := View.FirstChild;
    if Assigned(ViewChild) then
    begin
      View.Expanded := False;
      repeat
        FZOrderAnimator.AddTarget(ViewChild, ViewChild.ZOrder, Root.ZOrder);
        Visit(ViewChild);
        ViewChild := ViewChild.NextSibbling;
      until ViewChild = View.FirstChild;
    end
    else
      FZOrderAnimator.AddTarget(View, View.ZOrder, Root.ZOrder);
  end;

begin
  Log('TLayoutViewer.Collapse %s: Root.ZOrder=%f', [DbgS(Root), Root.ZOrder]);

  FZOrderAnimator.Finish;
  FZOrderAnimator.ClearTargets;
  Visit(Root);
  FZOrderAnimator.Start;
end;

procedure TLayoutViewer.Expand(Root: TView);

  procedure Visit(View, LastVisibleParent: TView);
  var
    ViewChild: TView;
  begin
    ViewChild := View.FirstChild;
    if not Assigned(ViewChild) then
      Exit;

    if View.Expanded then
      repeat
        FZOrderAnimator.AddTarget(ViewChild, Root.ZOrder,
          ViewChild.ZOrderOriginal);
        Visit(ViewChild, ViewChild);
        ViewChild := ViewChild.NextSibbling;
      until ViewChild = View.FirstChild
    else
      repeat
        FZOrderAnimator.AddTarget(ViewChild, Root.ZOrder,
          LastVisibleParent.ZOrderOriginal);
        Visit(ViewChild, LastVisibleParent);
        ViewChild := ViewChild.NextSibbling;
      until ViewChild = View.FirstChild;
  end;

begin
  Log('TLayoutViewer.Expand %s', [DbgS(Root)]);

  FZOrderAnimator.Finish;
  FZOrderAnimator.ClearTargets;
  Visit(Root, Root);
  FZOrderAnimator.Start;
end;

procedure TLayoutViewer.SetActiveBranch(AValue: TView);
begin
  if Assigned(FLayout) and FLayout.SetActiveBranch(AValue) then
    Invalidate;
end;

procedure TLayoutViewer.SetLayout(AValue: IViewLayout);
begin
  if Assigned(AValue) then
    Log('TLayoutViewer.SetViewLayout %s: ActiveBranch=%s',
      [DbgS(Pointer(AValue)), DbgS(AValue.ActiveBranch)])
  else
    Log('TLayoutViewer.SetViewLayout nil');

  FLayout := AValue;

  FRotationY := 0;
  FRotationX := 0;
  FZoomLevel := InitialZoomLevel;
  FScaleZ := 0;

  if Assigned(FLayout) then
  begin
    OnMouseDown := @MouseDownHandler;
    OnMouseUp := @MouseUpHandler;
    OnMouseMove := @MouseMoveHandler;
    OnMouseWheel := @MouseWheelHandler;
    OnMouseLeave := @MouseLeaveHandler;
    OnClick := @MouseClickHandler;
    OnDblClick := @MouseDblClickHandler;

    // TODO: resize to fit
    GetActiveBranchCenter(FOriginX, FOriginY, FOriginZ, FCameraZ);

    if Mode3D then
    begin
      FToggleMode3DAnimator.SetElementInterval(t3daRotationY, 0, 0);
      FToggleMode3DAnimator.SetElementInterval(t3daScaleZ, 0, Mode3DScaleZ);
      FToggleMode3DAnimator.Restart;
    end
    else
      Invalidate;
  end
  else
  begin
    OnMouseDown := nil;
    OnMouseUp := nil;
    OnMouseMove := nil;
    OnMouseWheel := nil;
    OnMouseLeave := nil;
    OnClick := nil;
    OnDblClick := nil;
    Invalidate;
  end;
end;

procedure TLayoutViewer.ResetCamera(ResetRotation: boolean; Animate: boolean);
var
  EndOriginX, EndOriginY, EndOriginZ, EndCameraDistance: single;
begin
  if Assigned(FLayout) then
    if Animate then
    begin
      GetActiveBranchCenter(EndOriginX, EndOriginY, EndOriginZ,
        EndCameraDistance);

      FResetCameraAnimator.SetElementInterval(rcaOriginX, OriginX, EndOriginX);
      FResetCameraAnimator.SetElementInterval(rcaOriginY, OriginY, EndOriginY);
      FResetCameraAnimator.SetElementInterval(rcaOriginZ, OriginZ, EndOriginZ);
      if ResetRotation then
        FResetCameraAnimator.SetElementInterval(rcaRotationY, RotationY,
          InitialRotationY)
      else
        FResetCameraAnimator.SetElementInterval(rcaRotationY, RotationY, RotationY);

      FResetCameraAnimator.Restart;
    end
    else
    begin
      GetActiveBranchCenter(FOriginX, FOriginY, FOriginZ, FCameraZ);
      if ResetRotation then
        RotationY := InitialRotationY;
      Invalidate;
    end;
end;

function TLayoutViewer.SelectNextMatch: TView;
var
  FirstView, Current: TView;
begin
  Result := nil;
  if not Assigned(FLayout) then
    Exit;

  if Assigned(FLayout.ActiveView) then
    FirstView := FLayout.ActiveView
  else
    FirstView := FLayout.ActiveBranch;

  Current := FirstView.NextDown;
  while Current <> FirstView do
  begin
    if Current.MatchFilter then
    begin
      if SetActiveView(Current) then
        Result := Current;
      Break;
    end;
    Current := Current.NextDown;
  end;
end;

function TLayoutViewer.SelectPreviousMatch: TView;
var
  FirstView, Current: TView;
begin
  Result := nil;
  if not Assigned(FLayout) then
    Exit;

  if Assigned(FLayout.ActiveView) then
    FirstView := FLayout.ActiveView
  else
    FirstView := FLayout.ActiveBranch;

  Current := FirstView.PreviousUp;
  while Current <> FirstView do
  begin
    if Current.MatchFilter then
    begin
      if SetActiveView(Current) then
        Result := Current;
      Break;
    end;
    Current := Current.PreviousUp;
  end;
end;

procedure TLayoutViewer.SetViewSeparation(Delta: integer);
begin
  if Mode3D then
  begin
    FScaleZAnimator.SetValueInterval(ScaleZ,
      EnsureRange(FScaleZ + Delta * StepScaleZ, MinScaleZ, MaxScaleZ));
    FScaleZAnimator.Restart;
  end;
end;

procedure TLayoutViewer.ActiveViewChangedTimerTimer(Sender: TObject);
begin
  LogEnterMethod('TLayoutViewer.ActiveViewChangedTimerTimer');

  FActiveViewChangedTimer.Enabled := False;
  DoActiveViewChanged;

  LogExitMethod('TLayoutViewer.ActiveViewChangedTimerTimer');
end;

procedure TLayoutViewer.LayoutAnimationStart(Sender: TObject);
begin
  // Keep track of current animation.
  // Whenever a layout animation is started, we "cancel" any active
  // animation so that only one is running at any given time.
  // We do this to prevent animations from interfering with each other.
  if Assigned(FCurrentAnimator) then
  begin
    if FCurrentAnimator <> Sender then
    begin
      FCurrentAnimator.Cancel;
      FCurrentAnimator := TAnimator(Sender);
    end;
  end
  else
    FCurrentAnimator := TAnimator(Sender);
end;

procedure TLayoutViewer.ResetCameraAnimate(Sender: TFloatArrayAnimator;
  const Values: array of single);
begin
  FOriginX := Values[rcaOriginX];
  FOriginY := Values[rcaOriginY];
  FOriginZ := Values[rcaOriginZ];
  FRotationY := Values[rcaRotationY];
  Invalidate;
end;

procedure TLayoutViewer.MouseLeaveHandler(Sender: TObject);
begin
  HighlightedView := nil;
end;

procedure TLayoutViewer.SetRotationX(Degres: single);
begin
  if FRotationX <> Degres then
  begin
    FRotationX := Degres;
    Invalidate;
  end;
end;

function TLayoutViewer.SetActiveView(AValue: TView): boolean;
begin
  if Assigned(FLayout) and FLayout.SetActiveView(AValue) then
  begin
    Invalidate;
    Result := True;
  end
  else
    Result := False;
end;

procedure TLayoutViewer.SetMode3D(AValue: boolean);
begin
  if FMode3D = AValue then
    Exit;

  FMode3D := AValue;
  if FMode3D then
  begin
    // Don't RotateY.
    FToggleMode3DAnimator.SetElementInterval(t3daRotationY, 0, 0);
    FToggleMode3DAnimator.SetElementInterval(t3daScaleZ, 0, Mode3DScaleZ);
  end
  else
  begin
    FToggleMode3DAnimator.SetElementInterval(t3daRotationY, RotationY, 0);
    FToggleMode3DAnimator.SetElementInterval(t3daScaleZ, ScaleZ, 0);
  end;

  FToggleMode3DAnimator.Restart;
end;

function TLayoutViewer.GetFinalOriginX: single;
begin
  if FResetCameraAnimator.IsRunning then
    Result := FResetCameraAnimator.EndValues[rcaOriginX]
  else
    Result := OriginX;
end;

function TLayoutViewer.GetFinalOriginY: single;
begin
  if FResetCameraAnimator.IsRunning then
    Result := FResetCameraAnimator.EndValues[rcaOriginY]
  else
    Result := OriginY;
end;

function TLayoutViewer.GetFinalOriginZ: single;
begin
  if FResetCameraAnimator.IsRunning then
    Result := FResetCameraAnimator.EndValues[rcaOriginZ]
  else
    Result := OriginZ;
end;

function TLayoutViewer.GetFinalRotationX: single;
begin
  Result := RotationX;
end;

function TLayoutViewer.GetFinalRotationY: single;
begin
  if FToggleMode3DAnimator.IsRunning then
    Result := FToggleMode3DAnimator.EndValues[t3daRotationY]
  else if FResetCameraAnimator.IsRunning then
    Result := FResetCameraAnimator.EndValues[rcaRotationY]
  else
    Result := RotationY;
end;

function TLayoutViewer.GetFinalScaleZ: single;
begin
  if FToggleMode3DAnimator.IsRunning then
    Result := FToggleMode3DAnimator.EndValues[t3daScaleZ]
  else if FScaleZAnimator.IsRunning then
    Result := FScaleZAnimator.EndValue
  else
    Result := ScaleZ;
end;

function TLayoutViewer.GetFinalZoomLevel: single;
begin
  if FZoomLevelAnimator.IsRunning then
    Result := FZoomLevelAnimator.EndValue
  else
    Result := ZoomLevel;
end;

procedure TLayoutViewer.SetHighlightedView(AValue: TView);
begin
  if FHighlightedView <> AValue then
  begin
    FHighlightedView := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetOriginZ(AValue: single);
begin
  if FOriginZ <> AValue then
  begin
    FOriginZ := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetRotationY(Degres: single);
begin
  if FRotationY <> Degres then
  begin
    FRotationY := Degres;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetShowContent(AValue: boolean);
begin
  if FShowContent <> AValue then
  begin
    FShowContent := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetShowWireFrames(AValue: boolean);
begin
  if FShowWireframes <> AValue then
  begin
    FShowWireframes := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetZoomLevel(AValue: single);
begin
  if FZoomLevel <> AValue then
  begin
    FZoomLevel := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetScaleZ(AValue: single);
begin
  if FScaleZ <> AValue then
  begin
    FScaleZ := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetOriginX(AValue: single);
begin
  if FOriginX <> AValue then
  begin
    FOriginX := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.SetOriginY(AValue: single);
begin
  if FOriginY <> AValue then
  begin
    FOriginY := AValue;
    Invalidate;
  end;
end;

procedure TLayoutViewer.DoOnPaint;
var
  ModelViewMatrix, ProjectionMatrix: T16dArray;
  ViewportRect: TViewPortArray;

  procedure DrawTexture(Left, Top, Right, Bottom, Z: single;
    TextureName: integer); inline;
  begin
    // Set color to fully opaque white as the texture is GL_COMBINEd with it.
    // by default. The result will be only the texture's color components.
    glColor4f(1, 1, 1, 1);

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, TextureName);

    glBegin(GL_POLYGON);
    glTexCoord2f(0, 0);
    glVertex3f(Left, Top, Z);
    glTexCoord2f(1, 0);
    glVertex3f(Right, Top, Z);
    glTexCoord2f(1, 1);
    glVertex3f(Right, Bottom, Z);
    glTexCoord2f(0, 1);
    glVertex3f(Left, Bottom, Z);
    glEnd;

    glDisable(GL_TEXTURE_2D);
  end;

  procedure PolyFill(Left, Top, Right, Bottom, Z: single; Color: TColorRGBA); inline;
  begin
    glColor4ubv(@Color.ABGR);
    glBegin(GL_POLYGON);
    glVertex3f(Left, Top, Z);
    glVertex3f(Right, Top, Z);
    glVertex3f(Right, Bottom, Z);
    glVertex3f(Left, Bottom, Z);
    glEnd;
  end;

  procedure PolyLine(Left, Top, Right, Bottom, Z: single; Color: TColorRGBA); inline;
  begin
    glColor4ubv(@Color.ABGR);
    glBegin(GL_LINE_LOOP);
    glVertex3f(Left, Top, Z);
    glVertex3f(Right, Top, Z);
    glVertex3f(Right, Bottom, Z);
    glVertex3f(Left, Bottom, Z);
    glEnd;
  end;

  procedure DrawPadding(View: TView);
  begin
    with View do
    begin
      if PaddingLeft <> 0 then
        PolyFill(Left, Top, Left + PaddingLeft, Bottom, ZOrder, rgbaPaddingColor);

      if PaddingRight <> 0 then
        PolyFill(Right - PaddingRight, Top, Right, Bottom, ZOrder, rgbaPaddingColor);

      // Don't draw top paddig over left/right paddings.
      if PaddingTop <> 0 then
        if PaddingLeft <> 0 then
          if PaddingRight <> 0 then
            PolyFill(Left + PaddingLeft, Top, Right - PaddingRight,
              Top + PaddingTop, ZOrder, rgbaPaddingColor)
          else
            PolyFill(Left + PaddingLeft, Top, Right, Top + PaddingTop,
              ZOrder, rgbaPaddingColor)
        else
          PolyFill(Left, Top, Right, Top + PaddingTop, ZOrder, rgbaPaddingColor);

      // Don't draw bottom paddig over left/right paddings.
      if PaddingBottom <> 0 then
        if PaddingLeft <> 0 then
          if PaddingRight <> 0 then
            PolyFill(Left + PaddingLeft, Bottom, Right - PaddingRight,
              Bottom - PaddingBottom, ZOrder, rgbaPaddingColor)
          else
            PolyFill(Left + PaddingLeft, Bottom, Right, Bottom -
              PaddingBottom, ZOrder, rgbaPaddingColor)
        else
          PolyFill(Left, Bottom - PaddingBottom, Right, Bottom,
            ZOrder, rgbaPaddingColor);
    end;
  end;

  procedure DrawMargin(View: TView);
  begin
    with View do
    begin
      if MarginLeft <> 0 then
        PolyFill(Left - MarginLeft, Top, Left, Bottom, ZOrder, rgbaMarginColor);

      if MarginRight <> 0 then
        PolyFill(Right, Top, Right + MarginRight, Bottom, ZOrder, rgbaMarginColor);

      // Don't draw top margin over left/right margins.
      if MarginTop <> 0 then
        if MarginLeft <> 0 then
          if MarginRight <> 0 then
            PolyFill(Left - MarginLeft, Top - MarginTop, Right +
              MarginRight, Top, ZOrder, rgbaMarginColor)
          else
            PolyFill(Left - MarginLeft, Top - MarginTop, Right, Top,
              ZOrder, rgbaMarginColor)
        else
          PolyFill(Left, Top - MarginTop, Right, Top, ZOrder, rgbaMarginColor);

      // Don't draw bottom margin over left/right margins.
      if MarginBottom <> 0 then
        if MarginLeft <> 0 then
          if MarginRight <> 0 then
            PolyFill(Left - MarginLeft, Bottom, Right + MarginRight,
              Bottom + MarginBottom, ZOrder, rgbaMarginColor)
          else
            PolyFill(Left - MarginLeft, Bottom, Right, Bottom +
              MarginBottom, ZOrder, rgbaMarginColor)
        else
          PolyFill(Left, Bottom, Right, Bottom + MarginBottom, ZOrder, rgbaMarginColor);
    end;
  end;

  procedure DrawView(V: TView; Texture: cardinal);

    function GetWindowPoint(const X, Y, Z: single): TPoint; inline;
    var
      WinX, WinY, WinZ: GLdouble;
    begin
      gluProject(X, Y, Z, ModelViewMatrix, ProjectionMatrix,
        ViewportRect, @WinX, @WinY, @WinZ);
      Result.X := Round(WinX);
      Result.Y := Round(ViewportRect[3] - WinY);
    end;

  begin
    if (Texture > 0) and FShowContent then
      DrawTexture(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, Texture);

    if FLayout.ActiveView = V then
    begin
      DrawMargin(V);
      DrawPadding(V);
      PolyFill(V.Left + V.PaddingLeft, V.Top + V.PaddingTop, V.Right -
        V.PaddingRight, V.Bottom - V.PaddingBottom, V.ZOrder, rgbaFillColor);
      PolyLine(V.Left + V.PaddingLeft, V.Top + V.PaddingTop,
        V.Right - V.PaddingRight, V.Bottom - V.PaddingBottom, V.ZOrder,
        rgbaActiveRectColor);
    end
    else if HighlightedView = V then
    begin
      PolyFill(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, rgbaFillColor);
      PolyLine(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, rgbaRectColor);
    end
    else if V.MatchFilter then
      PolyLine(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, rgbaFilterMatchRectColor)
    else if FShowWireframes then
      PolyLine(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, rgbaRectColor);

    with V do
    begin
      // Store transformed points; we use them to perform hit test.
      ViewportRect[0] := {%H-}GetWindowPoint(Left, Top, ZOrder);
      ViewportRect[1] := GetWindowPoint(Right, Top, ZOrder);
      ViewportRect[2] := GetWindowPoint(Right, Bottom, ZOrder);
      ViewportRect[3] := GetWindowPoint(Left, Bottom, ZOrder);
    end;
  end;

var
  View, CurrentRoot: TView;
begin
  with rgbaCanvasColor do
    glClearColor(R / 255, G / 255, B / 255, A / 255);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  try
    if not Assigned(FLayout) then
      Exit;

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;

    // Note that the order in which we specify transformations is crucial.
    // They are applied in reverse, that is, the last "specified"
    // transformation is the first one to be "applied".

    // Move hierarchy far away from camera.
    glTranslatef(0, 0, -FCameraZ);

    if FToggleMode3DAnimator.IsRunning or Mode3D then
    begin
      // Scale Z, rotate Y around origin and invert Y-axis.
      glRotatef(FRotationY, 0, 1, 0);
      glScalef(1, -1, FScaleZ);
    end
    else
      // Drop Z coord (2D mode) and invert Y-axis.
      glScalef(1, -1, 0);

    // Move to origin.
    // Initially our origin is the ActiveBranch's bounding cube center.
    glTranslatef(-FOriginX, -FOriginY, -FOriginZ);


    // Perspective projection.
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glScalef(FZoomLevel, FZoomLevel, 1);
    gluPerspective(45, Width / Height, 0, FCameraZ);

    glGetDoublev(GL_MODELVIEW_MATRIX, ModelViewMatrix);
    glGetDoublev(GL_PROJECTION_MATRIX, ProjectionMatrix);
    glGetIntegerv(GL_VIEWPORT, ViewportRect);

    CurrentRoot := FLayout.ActiveBranch;
    View := CurrentRoot;
    repeat
      // Skip views not visible to the user.
      if View.VisibilityGone or (View.Width = 0) or (View.Height = 0) then
        View := View.Next // continue
      else
      begin
        DrawView(View, View.TextureName);
        View := View.Next; // continue
      end;
    until View = CurrentRoot;
  finally
    SwapBuffers;
  end;
end;

procedure TLayoutViewer.ToggleMode3DAnimate(Sender: TFloatArrayAnimator;
  const Values: array of single);
begin
  FRotationY := Values[t3daRotationY];
  FScaleZ := Values[t3daScaleZ];
  Invalidate;
end;

procedure TLayoutViewer.MouseDownHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FMouseState := msDown;
    FLastMouseX := X;
    FLastMouseY := Y;
  end;
end;

procedure TLayoutViewer.MouseClickHandler(Sender: TObject);
var
  ClickedView: TView;
begin
  LogEnterMethod('TLayoutViewer.MouseClickHandler');

  if FMouseState <> msDragging then
  begin
    // We're here because the user performed a click without dragging.
    // When the user double-clicks we also receive an OnClick
    // followed by OnDblClick.
    // UX-wise, we use click to change the active view and double-click
    // to change both the active branch and active view.
    // Give that at this point we don't know if OnDblClick will ever come, and
    // to avoid flickering, for now update the active view without firing
    // OnActiveViewChanged and wait a little bit to see if OnDblClick is called.
    // OnActiveViewChanged will be fired later, either in ActiveViewChangedTimerTimer
    // or MouseDblClickHandler, whichever comes first.
    ClickedView := FLayout.HitTest(FLastMouseX, FLastMouseY);
    if SetActiveView(ClickedView) then  // active view really changed?
      FActiveViewChangedTimer.Enabled := True
    else
      Log('TLayoutViewer.MouseClickHandler: HitTest(%d,%d)=%s',
        [FLastMouseX, FLastMouseY, DbgS(ClickedView)]);
  end;

  LogExitMethod('TLayoutViewer.MouseClickHandler');
end;

procedure TLayoutViewer.MouseDblClickHandler(Sender: TObject);
begin
  LogEnterMethod('TLayoutViewer.MouseDblClickHandler');

  if FLayout.SetActiveBranch(FLayout.ActiveView) then // active branch changed?
  begin
    FActiveViewChangedTimer.Enabled := False;
    ResetCamera(False);
    if Assigned(FOnActiveBranchChanged) then
      FOnActiveBranchChanged(FLayout.ActiveBranch);
  end
  else if FActiveViewChangedTimer.Enabled then // active view changed?
  begin
    FActiveViewChangedTimer.Enabled := False;
    DoActiveViewChanged;
    Invalidate;
  end;

  LogExitMethod('TLayoutViewer.MouseDblClickHandler');
end;

procedure TLayoutViewer.MouseUpHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseState := msNone;
end;

procedure TLayoutViewer.MouseMoveHandler(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if FMouseState = msDown then
    FMouseState := msDragging;

  if FMouseState = msDragging then
  begin
    if ssShift in Shift then
    begin
      OriginX := OriginX - (X - FLastMouseX);
      OriginY := OriginY - (Y - FLastMouseY);
    end
    else
    if Mode3D then
    begin
      // Constrain X/Y rotation to predefined min/max angles.
      RotationX := EnsureRange(RotationX - Y - FLastMouseY, MinRotationX,
        MaxRotationX);
      RotationY := EnsureRange(RotationY + X - FLastMouseX, MinRotationY,
        MaxRotationY);
    end;

    FLastMouseX := X;
    FLastMouseY := Y;
  end
  else
    HighlightedView := FLayout.HitTest(X, Y);
end;

procedure TLayoutViewer.MouseWheelHandler(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if (FMouseState = msDragging) or FToggleMode3DAnimator.IsRunning then
    Exit;

  WheelDelta := Sign(WheelDelta);
  if ssCtrl in Shift then
  begin
    if Mode3D then
    begin
      // Constrain Z scaling to predefined min/max values.
      FScaleZAnimator.SetValueInterval(ScaleZ,
        EnsureRange(FScaleZ + WheelDelta * StepScaleZ, MinScaleZ, MaxScaleZ));
      FScaleZAnimator.Restart;
      Handled := True;
    end;
  end
  else
  begin
    Zoom(WheelDelta);
    Handled := True;
  end;
end;

procedure TLayoutViewer.DoActiveViewChanged;
begin
  if Assigned(FLayout) and Assigned(FOnActiveViewChanged) then
    FOnActiveViewChanged(FLayout.ActiveView);
end;

procedure TLayoutViewer.ZoomLevelAnimate(Sender: TFloatAnimator; Value: single);
begin
  ZoomLevel := Value;
end;

procedure TLayoutViewer.ScaleZAnimate(Sender: TFloatAnimator; Value: single);

begin
  ScaleZ := Value;
end;

procedure TLayoutViewer.ZOrderAnimatorUpdate(Sender: TAnimator;
  const InterpolatedFraction: single);
begin
  Invalidate;
end;

procedure TLayoutViewer.GetActiveBranchCenter(
  out CenterX, CenterY, CenterZ, CameraDistance: single);
var
  View: TView;
  MinLeft, MinTop, MaxRight, MaxBottom: single;
begin
  MinLeft := NaN;
  MinTop := NaN;
  MaxRight := NaN;
  MaxBottom := NaN;

  View := FLayout.ActiveBranch;

  Log('TLayoutViewer.GetActiveBranchCenter: Left=%f Top=%f Right=%f Bottom=%f',
    [View.Left, View.Top, View.Right, View.Bottom]);

  // Compute ActiveBranch's bounding rectangle.
  repeat
    if View.VisibilityGone or (View.Width = 0) or (View.Height = 0) then
      View := View.Next // continue
    else
    begin
      MinLeft := Min(MinLeft, View.Left);
      MinTop := Min(MinTop, View.Top);
      MaxRight := Max(MaxRight, View.Right);
      MaxBottom := Max(MaxBottom, View.Bottom);
      View := View.Next;
    end;
  until View = FLayout.ActiveBranch;

  Log('TLayoutViewer.GetActiveBranchCenter: MinLeft=%f MinTop=%f MaxRight=%f MaxBottom=%f',
    [MinLeft, MinTop, MaxRight, MaxBottom]);

  CenterX := (MinLeft + MaxRight) / 2;
  CenterY := (MinTop + MaxBottom) / 2;
  CenterZ := (View.ZOrderOriginal + View.Previous.ZOrderOriginal) / 2;

  CameraDistance := View.Previous.ZOrderOriginal - View.ZOrderOriginal;
  CameraDistance := Max(CameraDistance, MaxRight - MinLeft);
  CameraDistance += MinCameraDistance;

  Log('TLayoutViewer.GetActiveBranchCenter: CenterX=%f CenterY=%f CenterZ=%f CameraDistance=%f',
    [CenterX, CenterY, CenterZ, CameraDistance]);
end;

procedure TLayoutViewer.WMSize(var Message: TLMSize);
begin
  Update;
end;

end.
