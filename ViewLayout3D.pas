unit ViewLayout3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, Menus, ExtCtrls, View3DTypes, Animators, OpenGLContext,
  TaskRunner;

type

  TMouseState = (msNone, msDown, msDragging);

  TZOrderAnimator = class;
  TToggleView3DAnimator = class;

  { TViewLayout3D }

  TViewLayout3D = class(TOpenGLControl)
  private
    FView3DEnabled: boolean;
    FRootView: TView3D;
    FClipBounds: boolean;
    FHierarchyWidth: integer;
    FHierarchyHeight: integer;
    FZOrderAnimator: TZOrderAnimator;
    FToggleView3DAnimator: TToggleView3DAnimator;
    FOnActiveBranchChanged: TNotifyEvent;
    FScaleZAnimator: TFloatAnimator;
    FZoomLevelAnimator: TFloatAnimator;
    FOriginX: single;
    FOriginY: single;
    FOriginZ: single;
    FActiveView: TView3D;
    FHighlightedView: TView3D;
    FMouseState: TMouseState;
    FLastMouseX: integer;
    FLastMouseY: integer;
    FRotationX: single; // degrees
    FRotationY: single; // degrees
    FZoomLevel: single;
    FScaleZ: single;
    FCameraZ: single;
    FOnActiveViewChanged: TNotifyEvent;
    FActiveBranch: TView3D;
    FActiveViewChangedTimer: TTimer;
    procedure SetView3DEnabled(AValue: boolean);
    procedure SetActiveBranch(AValue: TView3D);
    procedure SetActiveView(AValue: TView3D);
    procedure SetClipBounds(AValue: boolean);
    procedure SetHighlightedView(AValue: TView3D);
    procedure SetRotationX(Degres: single);
    procedure SetRotationY(Degres: single);
    procedure SetZoomLevel(AValue: single);
    procedure SetScaleZ(AValue: single);
    procedure SetOriginX(AValue: single);
    procedure SetOriginY(AValue: single);
    procedure SetRootView(AValue: TView3D);
    procedure StartCaptureView;
  protected
    procedure ActiveViewChangedTimerTimer(Sender: TObject);
    procedure CaptureViewTaskSuccessHandler(const Task: ITask);
    procedure MouseLeaveHandler(Sender: TObject);

    procedure ToggleView3DAnimatorUpdateHandler(Sender: TAnimator;
      const InterpolatedFraction: single);

    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseClickHandler(Sender: TObject);
    procedure MouseDblClickHandler(Sender: TObject);
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MouseWheelHandler(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);

    procedure DoActiveViewChanged; inline;
    function HitTest(X, Y: integer): TView3D;
    procedure ShowBranch(AView: TView3D);
    procedure ZoomAnimateValueHandler(Sender: TFloatAnimator; Value: single);
    procedure ScaleZAnimateValueHandler(Sender: TFloatAnimator; Value: single);
    procedure ZOrderAnimatorUpdateHandler(Sender: TAnimator;
      const InterpolatedFraction: single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoOnPaint; override;
    procedure Changed;
    procedure Zoom(Delta: integer);
    procedure Collapse(Root: TView3D);
    procedure Expand(Root: TView3D);
    property RootView: TView3D write SetRootView;
    property RotationX: single read FRotationX write SetRotationX;
    property RotationY: single read FRotationY write SetRotationY;
    property ZoomLevel: single read FZoomLevel write SetZoomLevel;
    property ScaleZ: single read FScaleZ write SetScaleZ;
    property OriginX: single read FOriginX write SetOriginX;
    property OriginY: single read FOriginY write SetOriginY;
    property ActiveView: TView3D read FActiveView write SetActiveView;
    property HighlightedView: TView3D read FHighlightedView write SetHighlightedView;
    property OnActiveViewChanged: TNotifyEvent
      read FOnActiveViewChanged write FOnActiveViewChanged;
    property ClipBounds: boolean read FClipBounds write SetClipBounds;
    property ActiveBranch: TView3D read FActiveBranch write SetActiveBranch;
    property OnActiveBranchChanged: TNotifyEvent
      read FOnActiveBranchChanged write FOnActiveBranchChanged;
    property View3DEnabled: boolean read FView3DEnabled write SetView3DEnabled;
  end;


  TZOrderAnimatorTargetRec = record
    View: TView3D;
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
    procedure AddTarget(View: TView3D; const StartValue, EndValue: single);
  end;

  { TToggleView3DAnimator }

  TToggleView3DAnimator = class(TAnimator)
  private
    FRotationYStartValue: single;
    FRotationYEndValue: single;
    FScaleZStartValue: single;
    FScateZEndValue: single;
  public
    procedure SetRotationYInterval(const StartValue, EndValue: single);
    procedure SetScaleZInterval(const StartValue, EndValue: single);
    property RotationYStartValue: single read FRotationYStartValue;
    property RotationYEndValue: single read FRotationYEndValue;
    property ScaleZStartValue: single read FScaleZStartValue;
    property ScateZEndValue: single read FScateZEndValue;
  end;

implementation

uses
  SysUtils, LCLIntf, LCLProc, Math, GLext, gl, glu, Logging;

type
  TColorABGR = cardinal;

const
  clBackgroundCanvas32 = TColorABGR($FF212121);
  clBorderColor32 = TColorABGR($C0636363);
  clActiveBorderColor32 = TColorABGR($FFFF9430);
  clFilteredBorderColor32 = TColorABGR($C091EEFF);
  clFilteredContentColor32 = TColorABGR($2062E5FC);
  clPaddingColor32 = TColorABGR($50C3DEB7);
  clMarginColor32 = TColorABGR($50A0C5E8);
  clContentColor32 = TColorABGR($70FF824A);

  InitialZoomLevel = 1;

  View3DScaleZ = 20;
  View3DRotationX = 0;
  View3DRotationY = 30;

  StepZoomLevel = 0.1;
  StepScaleZ = 20;

  MinZoomLevel = 0.1;
  MinScaleZ = 0;

  MaxZoomLevel = 2;
  MaxScaleZ = 200;

  MinRotationX = -90;
  MinRotationY = -90;

  MaxRotationX = 90;
  MaxRotationY = 90;

  CameraDistance = 1500;

  CanvasPaddingVertical = 50;
  CanvasPaddingHorizontal = 50;

var
  CapturableWidgets: TStringList;

{ TToggleView3DAnimator }

procedure TToggleView3DAnimator.SetRotationYInterval(
  const StartValue, EndValue: single);
begin
  FRotationYStartValue := StartValue;
  FRotationYEndValue := EndValue;
end;

procedure TToggleView3DAnimator.SetScaleZInterval(const StartValue, EndValue: single);
begin
  FScaleZStartValue := StartValue;
  FScateZEndValue := EndValue;
end;

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

procedure TZOrderAnimator.AddTarget(View: TView3D; const StartValue, EndValue: single);
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

{ TViewLayout3D }

constructor TViewLayout3D.Create(AOwner: TComponent);
begin
  inherited;

  FMouseState := msNone;

  FActiveViewChangedTimer := TTimer.Create(Self);
  FActiveViewChangedTimer.Enabled := False;
  FActiveViewChangedTimer.Interval := GetDoubleClickTime;
  FActiveViewChangedTimer.OnTimer := @ActiveViewChangedTimerTimer;

  MultiSampling := 4;
  AutoResizeViewport := True;
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  FView3DEnabled := True;
  FToggleView3DAnimator := TToggleView3DAnimator.Create;
  FToggleView3DAnimator.Duration := 500;
  FToggleView3DAnimator.OnUpdate := @ToggleView3DAnimatorUpdateHandler;

  FScaleZAnimator := TFloatAnimator.Create(@ScaleZAnimateValueHandler);
  FScaleZAnimator.Duration := 200;
  FZoomLevelAnimator := TFloatAnimator.Create(@ZoomAnimateValueHandler);
  FZoomLevelAnimator.Duration := 200;

  FZOrderAnimator := TZOrderAnimator.Create;
  FZOrderAnimator.Duration := 200;
  FZOrderAnimator.OnUpdate := @ZOrderAnimatorUpdateHandler;
end;

destructor TViewLayout3D.Destroy;
begin
  FZOrderAnimator.Free;
  FZoomLevelAnimator.Free;
  FScaleZAnimator.Free;
  FToggleView3DAnimator.Free;
  inherited Destroy;
end;

procedure TViewLayout3D.Changed;
begin
  Log('TViewLayout3D.Changed');
  Invalidate;
end;

procedure TViewLayout3D.Zoom(Delta: integer);
begin
  FZoomLevelAnimator.SetValueInterval(ZoomLevel,
    EnsureRange(FZoomLevel + Delta * StepZoomLevel, MinZoomLevel, MaxZoomLevel));
  FZoomLevelAnimator.Restart;
end;

procedure TViewLayout3D.Collapse(Root: TView3D);

  procedure Visit(View: TView3D);
  var
    ViewChild: TView3D;
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
  Log('TViewLayout3D.Collapse %s', [DbgS(Root)]);

  FZOrderAnimator.Finish;
  FZOrderAnimator.ClearTargets;
  Visit(Root);
  FZOrderAnimator.Start;
end;

procedure TViewLayout3D.Expand(Root: TView3D);

  procedure Visit(View, LastVisibleParent: TView3D);
  var
    ViewChild: TView3D;
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
  Log('TViewLayout3D.Expand %s', [DbgS(Root)]);

  FZOrderAnimator.Finish;
  FZOrderAnimator.ClearTargets;
  Visit(Root, Root);
  FZOrderAnimator.Start;
end;

procedure TViewLayout3D.SetRootView(AValue: TView3D);
begin
  Log('TViewLayout3D.SetRootView %s', [DbgS(AValue)]);

  if FRootView = AValue then
    Exit;
  // TODO: center fit initially

  FRotationY := 0;
  FRotationX := 0;
  FZoomLevel := InitialZoomLevel;
  FScaleZ := 0;

  FRootView := AValue;
  FActiveBranch := AValue;
  FActiveView := nil;

  if Assigned(FRootView) then
  begin
    FCameraZ := CameraDistance + FRootView.Previous.ZOrderOriginal;
    // Initial origin X,Y is the center of root view.
    FOriginX := (FRootView.Right - FRootView.Left) / 2;
    FOriginY := (FRootView.Bottom - FRootView.Top) / 2;
    FOriginZ := FRootView.Previous.ZOrderOriginal / 2;

    OnMouseDown := @MouseDownHandler;
    OnMouseUp := @MouseUpHandler;
    OnMouseMove := @MouseMoveHandler;
    OnMouseWheel := @MouseWheelHandler;
    OnMouseLeave := @MouseLeaveHandler;
    OnClick := @MouseClickHandler;
    OnDblClick := @MouseDblClickHandler;

    if View3DEnabled then
    begin
      // No need to Invalidate here because the animation starts right away
      // and will take care of it.
      FToggleView3DAnimator.SetRotationYInterval(0, 0);
      FToggleView3DAnimator.SetScaleZInterval(0, View3DScaleZ);
      FToggleView3DAnimator.Restart;
    end
    else
      Invalidate;

    StartCaptureView;
  end
  else
  begin
    FCameraZ := 0;
    OnMouseDown := nil;
    OnMouseUp := nil;
    OnMouseMove := nil;
    OnMouseWheel := nil;
    OnMouseLeave := nil;
    OnClick := nil;
    OnDblClick := nil;
    PopupMenu := nil;
    Invalidate;
  end;
end;

procedure TViewLayout3D.StartCaptureView;
var
  View: TView3D;
  TaskFactory: ICaptureViewTaskFactory;
begin
  Log('TViewLayout3D.StartCaptureView');

  View := FRootView;
  repeat
    TaskFactory := View.CaptureViewTaskFactory;
    if (CapturableWidgets.IndexOf(View.QualifiedClassName) <> -1) and
      Assigned(TaskFactory) then
      with TaskFactory.CreateTask(View) do
      begin
        OnSuccess := @CaptureViewTaskSuccessHandler;
        Start;
      end;
    View := View.Next;
  until View = FRootView;
end;

procedure TViewLayout3D.ActiveViewChangedTimerTimer(Sender: TObject);
begin
  LogEnterMethod('TViewLayout3D.ActiveViewChangedTimerTimer');

  FActiveViewChangedTimer.Enabled := False;
  DoActiveViewChanged;

  LogExitMethod('TViewLayout3D.ActiveViewChangedTimerTimer');
end;

procedure TViewLayout3D.MouseLeaveHandler(Sender: TObject);
begin
  HighlightedView := nil;
end;

procedure TViewLayout3D.SetRotationX(Degres: single);
begin
  if FRotationX <> Degres then
  begin
    FRotationX := Degres;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetActiveView(AValue: TView3D);
begin
  if FActiveView = AValue then
    Exit;

  FActiveView := AValue;
  Log('TViewLayout3D.SetActiveView: FActiveView=%s', [DbgS(FActiveView)]);

  DoActiveViewChanged;
  Invalidate;
end;

procedure TViewLayout3D.SetActiveBranch(AValue: TView3D);
begin
  // Activate root view if requested branch is nil.
  if not Assigned(AValue) then
    AValue := FRootView;

  if FActiveBranch = AValue then
    Exit;

  FActiveBranch := AValue;
  Log('TViewLayout3D.SetActiveBranch: FActiveBranch=%s', [DbgS(FActiveBranch)]);

  if Assigned(FActiveBranch) then // something to show?
  begin
    ShowBranch(FActiveBranch);

    if Assigned(FOnActiveBranchChanged) then
    begin
      LogEnterMethod('TViewLayout3D.OnActiveBranchChanged');
      FOnActiveBranchChanged(Self);
      LogExitMethod('TViewLayout3D.OnActiveBranchChanged');
    end;

    Invalidate;
  end;
end;

procedure TViewLayout3D.SetView3DEnabled(AValue: boolean);
begin
  if FView3DEnabled = AValue then
    Exit;

  FView3DEnabled := AValue;
  if FView3DEnabled then
  begin
    // Don't RotateY.
    FToggleView3DAnimator.SetRotationYInterval(0, 0);
    FToggleView3DAnimator.SetScaleZInterval(0, View3DScaleZ);
  end
  else
  begin
    FToggleView3DAnimator.SetRotationYInterval(RotationY, 0);
    FToggleView3DAnimator.SetScaleZInterval(ScaleZ, 0);
  end;

  FToggleView3DAnimator.Restart;
end;

procedure TViewLayout3D.SetClipBounds(AValue: boolean);
begin
  if FClipBounds <> AValue then
  begin
    FClipBounds := AValue;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetHighlightedView(AValue: TView3D);
begin
  if FHighlightedView <> AValue then
  begin
    FHighlightedView := AValue;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetRotationY(Degres: single);
begin
  if FRotationY <> Degres then
  begin
    FRotationY := Degres;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetZoomLevel(AValue: single);
begin
  if FZoomLevel <> AValue then
  begin
    FZoomLevel := AValue;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetScaleZ(AValue: single);
begin
  if FScaleZ <> AValue then
  begin
    FScaleZ := AValue;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetOriginX(AValue: single);
begin
  if FOriginX <> AValue then
  begin
    FOriginX := AValue;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetOriginY(AValue: single);
begin
  if FOriginY <> AValue then
  begin
    FOriginY := AValue;
    Invalidate;
  end;
end;

procedure TViewLayout3D.DoOnPaint;

  procedure DrawTexture(Left, Top, Right, Bottom, Z: single; TextureName: integer);
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

  procedure PolyFill(Left, Top, Right, Bottom, Z: single; Color: TColorABGR);
  begin
    glColor4ubv(@Color);
    glBegin(GL_POLYGON);
    glVertex3f(Left, Top, Z);
    glVertex3f(Right, Top, Z);
    glVertex3f(Right, Bottom, Z);
    glVertex3f(Left, Bottom, Z);
    glEnd;
  end;

  procedure PolyLine(Left, Top, Right, Bottom, Z: single; Color: TColorABGR);
  begin
    glColor4ubv(@Color);
    glBegin(GL_LINE_LOOP);
    glVertex3f(Left, Top, Z);
    glVertex3f(Right, Top, Z);
    glVertex3f(Right, Bottom, Z);
    glVertex3f(Left, Bottom, Z);
    glEnd;
  end;

  procedure DrawPadding(View: TView3D);
  begin
    with View do
    begin
      if PaddingLeft <> 0 then
        PolyFill(Left, Top, Left + PaddingLeft, Bottom, ZOrder, clPaddingColor32);

      if PaddingRight <> 0 then
        PolyFill(Right - PaddingRight, Top, Right, Bottom, ZOrder, clPaddingColor32);

      // Don't draw top paddig over left/right paddings.
      if PaddingTop <> 0 then
        if PaddingLeft <> 0 then
          if PaddingRight <> 0 then
            PolyFill(Left + PaddingLeft, Top, Right - PaddingRight,
              Top + PaddingTop, ZOrder, clPaddingColor32)
          else
            PolyFill(Left + PaddingLeft, Top, Right, Top + PaddingTop,
              ZOrder, clPaddingColor32)
        else
          PolyFill(Left, Top, Right, Top + PaddingTop, ZOrder, clPaddingColor32);

      // Don't draw bottom paddig over left/right paddings.
      if PaddingBottom <> 0 then
        if PaddingLeft <> 0 then
          if PaddingRight <> 0 then
            PolyFill(Left + PaddingLeft, Bottom, Right - PaddingRight,
              Bottom - PaddingBottom, ZOrder, clPaddingColor32)
          else
            PolyFill(Left + PaddingLeft, Bottom, Right, Bottom -
              PaddingBottom, ZOrder, clPaddingColor32)
        else
          PolyFill(Left, Bottom - PaddingBottom, Right, Bottom,
            ZOrder, clPaddingColor32);
    end;
  end;

  procedure DrawMargin(View: TView3D);
  begin
    with View do
    begin
      if MarginLeft <> 0 then
        PolyFill(Left - MarginLeft, Top, Left, Bottom, ZOrder, clMarginColor32);

      if MarginRight <> 0 then
        PolyFill(Right, Top, Right + MarginRight, Bottom, ZOrder, clMarginColor32);

      // Don't draw top margin over left/right margins.
      if MarginTop <> 0 then
        if MarginLeft <> 0 then
          if MarginRight <> 0 then
            PolyFill(Left - MarginLeft, Top - MarginTop, Right +
              MarginRight, Top, ZOrder, clMarginColor32)
          else
            PolyFill(Left - MarginLeft, Top - MarginTop, Right, Top,
              ZOrder, clMarginColor32)
        else
          PolyFill(Left, Top - MarginTop, Right, Top, ZOrder, clMarginColor32);

      // Don't draw bottom margin over left/right margins.
      if MarginBottom <> 0 then
        if MarginLeft <> 0 then
          if MarginRight <> 0 then
            PolyFill(Left - MarginLeft, Bottom, Right + MarginRight,
              Bottom + MarginBottom, ZOrder, clMarginColor32)
          else
            PolyFill(Left - MarginLeft, Bottom, Right, Bottom +
              MarginBottom, ZOrder, clMarginColor32)
        else
          PolyFill(Left, Bottom, Right, Bottom + MarginBottom, ZOrder, clMarginColor32);
    end;
  end;

  procedure DrawView(V: TView3D);
  var
    ModelViewMatrix, ProjectionMatrix: T16dArray;
    ViewportRect: TViewPortArray;

    function GetWindowPoint(const X, Y, Z: single): TPoint; inline;
    var
      WinX, WinY, WinZ: GLdouble;
    begin
      gluProject(X, Y, Z, ModelViewMatrix, ProjectionMatrix,
        ViewportRect, @WinX, @WinY, @WinZ);
      Result.X := Round(WinX);
      Result.Y := Round(ViewportRect[3] - WinY);
    end;

  var
    C: TColorABGR;
  begin
    if V.TextureName <> 0 then
      DrawTexture(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, V.TextureName);

    if FActiveView = V then
    begin
      DrawMargin(V);
      DrawPadding(V);
      PolyFill(V.Left + V.PaddingLeft, V.Top + V.PaddingTop, V.Right -
        V.PaddingRight, V.Bottom - V.PaddingBottom, V.ZOrder, clContentColor32);
      PolyLine(V.Left + V.PaddingLeft, V.Top + V.PaddingTop,
        V.Right - V.PaddingRight, V.Bottom - V.PaddingBottom, V.ZOrder,
        clActiveBorderColor32);
    end
    else
    begin
      if V.MatchFilter then
        C := clFilteredContentColor32
      else
        C := clContentColor32;
      if HighlightedView = V then
        PolyFill(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, C);

      if V.MatchFilter then
        C := clFilteredBorderColor32
      else
        C := clBorderColor32;
      if ClipBounds then
        PolyLine(V.ClippedLeft, V.ClippedTop, V.ClippedRight,
          V.ClippedBottom, V.ZOrder, C)
      else
        PolyLine(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, C);
    end;

    glGetDoublev(GL_MODELVIEW_MATRIX, ModelViewMatrix);
    glGetDoublev(GL_PROJECTION_MATRIX, ProjectionMatrix);
    glGetIntegerv(GL_VIEWPORT, ViewportRect);

    with V do
    begin
      // Store transformed points; we use them to perform hit test.
      if ClipBounds then
      begin
        ViewportRect[0] := GetWindowPoint(ClippedLeft, ClippedTop, ZOrder);
        ViewportRect[1] := GetWindowPoint(ClippedRight, ClippedTop, ZOrder);
        ViewportRect[2] := GetWindowPoint(ClippedRight, ClippedBottom, ZOrder);
        ViewportRect[3] := GetWindowPoint(ClippedLeft, ClippedBottom, ZOrder);
      end
      else
      begin
        ViewportRect[0] := GetWindowPoint(Left, Top, ZOrder);
        ViewportRect[1] := GetWindowPoint(Right, Top, ZOrder);
        ViewportRect[2] := GetWindowPoint(Right, Bottom, ZOrder);
        ViewportRect[3] := GetWindowPoint(Left, Bottom, ZOrder);
      end;

      // Keep track of the hierarchy's width and height.
      // We use this for "center fit" and determining how much can the
      // hierarchy be moved while dragging.
      FHierarchyWidth := Max(FHierarchyWidth, GetViewportWidth +
        CanvasPaddingHorizontal * 2);
      FHierarchyHeight := Max(FHierarchyHeight, GetViewportHeight +
        CanvasPaddingVertical * 2);
    end;
  end;

var
  View: TView3D;
begin
  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  // 3. Move to (-OriginX, OriginY, -FCameraZ).
  glTranslatef(-OriginX, OriginY, -FCameraZ);
  // 2. Rotate and scale Z coords around (OriginX, OriginY, FOriginZ).
  glTranslatef(OriginX, OriginY, FOriginZ);
  glRotatef(RotationY, 0, 1, 0);
  glScalef(1, 1, ScaleZ);
  glTranslatef(-OriginX, -OriginY, -FOriginZ);
  // 1. Invert our Y-axis.
  glScalef(1, -1, 1);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glScalef(ZoomLevel, ZoomLevel, 1);
  gluPerspective(45, Width / Height, 0, FCameraZ);

  if Assigned(FRootView) then
  begin
    FHierarchyWidth := 0;
    FHierarchyHeight := 0;

    View := FRootView;
    repeat
      // Skip views that won't be visible to the user.
      if not View.Visible or View.VisibilityGone or (View.GetWidth = 0) or
        (View.GetHeight = 0) then
        View := View.Next
      else
      if ClipBounds and ((View.GetClippedWidth = 0) or (View.GetClippedHeight = 0)) then
        View := View.Next
      else
      begin
        DrawView(View);
        View := View.Next;
      end;
    until View = FRootView;
  end;

  SwapBuffers;
end;

procedure TViewLayout3D.ToggleView3DAnimatorUpdateHandler(Sender: TAnimator;
  const InterpolatedFraction: single);
var
  A: TToggleView3DAnimator absolute Sender;
begin
  ScaleZ := FloatEvaluator(InterpolatedFraction, A.ScaleZStartValue,
    A.ScateZEndValue);
  RotationY := FloatEvaluator(InterpolatedFraction, A.RotationYStartValue,
    A.RotationYEndValue);
end;

procedure TViewLayout3D.MouseDownHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    FMouseState := msDown;
    FLastMouseX := X;
    FLastMouseY := Y;
  end;
end;

procedure TViewLayout3D.MouseClickHandler(Sender: TObject);
var
  NewActiveView: TView3D;
begin
  if FMouseState <> msDragging then
  begin
    // We're here because the user performed a click without dragging.
    // When the user double-clicks we also receive an OnClick
    // followed by OnDblClick.
    // UX-wise, we use click to change ActiveView and double-click to change
    // both ActiveBranch and ActiveView.
    // Give that at this point we don't know if OnDblClick will ever come, and
    // to avoid flickering, for now update the active view without firing
    // OnActiveViewChanged and wait a little bit to see if OnDblClick is called.
    // OnActiveViewChanged will be fired later, either in our OnTimer or
    // or OnDblClick handler, whichever comes first.
    NewActiveView := HitTest(FLastMouseX, FLastMouseY);

    Log('TViewLayout3D.MouseClickHandler: HitTest(%d,%d)=%s',
      [FLastMouseX, FLastMouseY, DbgS(NewActiveView)]);

    if NewActiveView <> FActiveView then
    begin
      FActiveView := NewActiveView;
      Log('TViewLayout3D.MouseClickHandler: FActiveView=%s', [DbgS(FActiveView)]);
      FActiveViewChangedTimer.Enabled := True;
      Invalidate;
    end;
  end;
end;

procedure TViewLayout3D.MouseDblClickHandler(Sender: TObject);
begin
  LogEnterMethod('TViewLayout3D.MouseDblClickHandler');

  FActiveViewChangedTimer.Enabled := False;
  ActiveBranch := ActiveView;
  DoActiveViewChanged;

  LogExitMethod('TViewLayout3D.MouseDblClickHandler');
end;

procedure TViewLayout3D.MouseUpHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseState := msNone;
end;

procedure TViewLayout3D.MouseMoveHandler(Sender: TObject; Shift: TShiftState;
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
    if View3DEnabled then
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
    HighlightedView := HitTest(X, Y);
end;

procedure TViewLayout3D.MouseWheelHandler(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if (FMouseState = msDragging) or FToggleView3DAnimator.IsRunning then
    Exit;

  WheelDelta := Sign(WheelDelta);
  if ssCtrl in Shift then
  begin
    if View3DEnabled then
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

procedure TViewLayout3D.DoActiveViewChanged;
begin
  if Assigned(FOnActiveViewChanged) then
  begin
    LogEnterMethod('TViewLayout3D.OnActiveViewChanged');
    FOnActiveViewChanged(Self);
    LogExitMethod('TViewLayout3D.OnActiveViewChanged');
  end;
end;

function TViewLayout3D.HitTest(X, Y: integer): TView3D;
var
  View: TView3D;
begin
  Result := nil;
  // Start at last view and traverse the list backwards.
  View := FRootView.Previous;
  repeat
    // Don't take into account views that are not visible to the user.
    if not View.Visible or (View.GetWidth = 0) or (View.GetHeight = 0) then
      View := View.Previous // continue
    else
    if ClipBounds and ((View.GetClippedWidth = 0) or (View.GetClippedHeight = 0)) then
      View := View.Previous // continue
    else
    if View.Contains(X, Y) then
    begin
      Result := View;
      Break;
    end
    else
      View := View.Previous;
  until View = FRootView.Previous;
end;

procedure TViewLayout3D.ShowBranch(AView: TView3D);

  procedure ShowView(V: TView3D);
  var
    ViewChild: TView3D;
  begin
    V.Visible := True;
    ViewChild := V.FirstChild;
    if Assigned(ViewChild) then
      repeat
        ShowView(ViewChild);
        ViewChild := ViewChild.NextSibbling;
      until ViewChild = V.FirstChild;
  end;

var
  View: TView3D;
begin
  // Hide all views.
  View := FRootView;
  repeat
    View.Visible := False;
    View := View.Next;
  until View = FRootView;

  // Shows views in branch.
  ShowView(AView);
end;

procedure TViewLayout3D.ZoomAnimateValueHandler(Sender: TFloatAnimator; Value: single);
begin
  ZoomLevel := Value;
end;

procedure TViewLayout3D.ScaleZAnimateValueHandler(Sender: TFloatAnimator;
  Value: single);
begin
  ScaleZ := Value;
end;

procedure TViewLayout3D.ZOrderAnimatorUpdateHandler(Sender: TAnimator;
  const InterpolatedFraction: single);
begin
  Invalidate;
end;

procedure TViewLayout3D.CaptureViewTaskSuccessHandler(const Task: ITask);
var
  TextureName: GLint = 0;
  View: TView3D;
  Image: TRasterImage;
  CaptureViewTask: ICaptureViewTask;
begin
  CaptureViewTask := Task as ICaptureViewTask;
  View := CaptureViewTask.AssociatedView;
  Assert(Assigned(View), 'CaptureViewTask.AssociatedView is nil');
  Image := CaptureViewTask.GetResult;
  try
    // Abort if image was not fetched or its width/height is 1px.
    if not Assigned(Image) or (Image.Width = 1) or (Image.Height = 1) then
      Exit;

    // Create new OpenGL texture from image data.
    // Note we create the texture for a view only once.
    // If for some reason (eg. out-of-memory) the texture creation fails,
    // the view won't have an associated texture and we won't display its image.
    glGenTextures(1, @TextureName);
    glBindTexture(GL_TEXTURE_2D, TextureName);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(
      GL_TEXTURE_2D,
      0,
      GL_RGBA,
      Image.Width,
      Image.Height,
      0,
      GL_BGRA,
      GL_UNSIGNED_BYTE,
      Image.RawImage.Data);
  finally
    Image.Free;
  end;

  View.TextureName := TextureName;

  // Repaint the whole view layout.
  // This will happen each time a new view image is fetched.
  Invalidate;
end;

initialization
  CapturableWidgets := TStringList.Create;
  with CapturableWidgets do
  begin
    Sorted := True;
    CaseSensitive := True;
    Add('android.inputmethodservice.ExtractEditText');
    Add('android.support.design.widget.FloatingActionButton');
    Add('android.support.design.widget.TextInputEditText');
    Add('android.support.v4.widget.ContentLoadingProgressBar');
    Add('android.support.v7.widget.AppCompatAutoCompleteTextView');
    Add('android.support.v7.widget.AppCompatButton');
    Add('android.support.v7.widget.AppCompatCheckBox');
    Add('android.support.v7.widget.AppCompatCheckedTextView');
    Add('android.support.v7.widget.AppCompatEditText');
    Add('android.support.v7.widget.AppCompatImageButton');
    Add('android.support.v7.widget.AppCompatImageView');
    Add('android.support.v7.widget.AppCompatMultiAutoCompleteTextView');
    Add('android.support.v7.widget.AppCompatRadioButton');
    Add('android.support.v7.widget.AppCompatRatingBar');
    Add('android.support.v7.widget.AppCompatSeekBar');
    Add('android.support.v7.widget.AppCompatSpinner');
    Add('android.support.v7.widget.AppCompatTextView');
    Add('android.support.v7.widget.AppCompatTextView');
    Add('android.support.v7.widget.SwitchCompat');
    Add('android.widget.AutoCompleteTextView');
    Add('android.widget.Button');
    Add('android.widget.Checkbox');
    Add('android.widget.CheckedTextView');
    Add('android.widget.Chronometer');
    Add('android.widget.DigitalClock');
    Add('android.widget.EditText');
    Add('android.widget.ImageButton');
    Add('android.widget.ImageView');
    Add('android.widget.MultiAutoCompleteTextView');
    Add('android.widget.ProgressBar');
    Add('android.widget.QuickContactBadge');
    Add('android.widget.RadioButton');
    Add('android.widget.RatingBar');
    Add('android.widget.SeekBar');
    Add('android.widget.Switch');
    Add('android.widget.TextClock');
    Add('android.widget.TextView');
    Add('android.widget.Toast');
    Add('android.widget.ToggleButton');
    Add('android.widget.ZoomButton');
  end;

finalization
  CapturableWidgets.Free;

end.
