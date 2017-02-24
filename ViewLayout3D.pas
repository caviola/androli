unit ViewLayout3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, Menus, fgl, View3DTypes, Animators,
  OpenGLContext, TaskRunner;

type

  TZOrderAnimator = class;
  TToggleView3DAnimator = class;
  // TODO: use better map class
  TCaptureViewTaskViewMap = specialize TFPGMap<pointer, TView3D>;

  { TViewLayout3D }

  TViewLayout3D = class(TOpenGLControl)
  private
    FView3DEnabled: boolean;
    FCaptureViewTaskViewMap: TCaptureViewTaskViewMap;
    FRootView: TView3D;
    FClipBounds: boolean;
    FHierarchyWidth: integer;
    FHierarchyHeight: integer;
    FZOrderAnimator: TZOrderAnimator;
    FToggleView3DAnimator: TToggleView3DAnimator;
    FOnVisibleBranchChanged: TNotifyEvent;
    FScaleZAnimator: TFloatAnimator;
    FZoomLevelAnimator: TFloatAnimator;
    FOriginX: single;
    FOriginY: single;
    FOriginZ: single;
    FActiveView: TView3D;
    FHighlightedView: TView3D;
    FMouseDown: boolean;
    FDragging: boolean;
    FLastMouseX: integer;
    FLastMouseY: integer;
    FRotationX: single; // degrees
    FRotationY: single; // degrees
    FZoomLevel: single;
    FScaleZ: single;
    FCameraZ: single;
    FOnActiveViewChanged: TNotifyEvent;
    FVisibleBranch: TView3D;
    procedure SetView3DEnabled(V: boolean);
    procedure SetVisibleBranch(V: TView3D);
    procedure SetActiveView(V: TView3D);
    procedure SetClipBounds(V: boolean);
    procedure SetHighlightedView(V: TView3D);
    procedure SetRotationX(Deg: single);
    procedure SetRotationY(Deg: single);
    procedure SetZoomLevel(V: single);
    procedure SetScaleZ(V: single);
    procedure SetOriginX(V: single);
    procedure SetOriginY(V: single);
    procedure SetRootView(V: TView3D);
    procedure StartCaptureView;
    procedure CancelCaptureView;
    function GetCaptureViewTaskView(Task: TCaptureViewTask): TView3D;
  protected
    procedure CaptureViewTaskSuccessHandler(Sender: TObject);
    procedure CaptureViewTaskFreeHandler(Sender: TObject);
    procedure DblClickHandler(Sender: TObject);

    procedure ToggleView3DAnimatorUpdateHandler(Sender: TAnimator;
      const InterpolatedFraction: single);

    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MouseWheelHandler(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    //TODO: this is always available, so make it a toolbar button instead of contextual action

    procedure DoActiveViewChanged;
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
    property VisibleBranch: TView3D read FVisibleBranch write SetVisibleBranch;
    property OnVisibleBranchChanged: TNotifyEvent
      read FOnVisibleBranchChanged write FOnVisibleBranchChanged;
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
  SysUtils, Math, GLext, gl, glu, LazLogger;

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

  FCaptureViewTaskViewMap := TCaptureViewTaskViewMap.Create;
  FCaptureViewTaskViewMap.Sorted := True;
end;

destructor TViewLayout3D.Destroy;
begin
  CancelCaptureView;
  FCaptureViewTaskViewMap.Free;
  FZOrderAnimator.Free;
  FZoomLevelAnimator.Free;
  FScaleZAnimator.Free;
  FToggleView3DAnimator.Free;
  inherited Destroy;
end;

procedure TViewLayout3D.Changed;
begin
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
    I: integer;
    Target: TView3D;
  begin
    if View.ChildrenCount > 0 then
    begin
      View.Expanded := False;
      for I := 0 to View.ChildrenCount - 1 do
      begin
        Target := View.Children[I];
        FZOrderAnimator.AddTarget(Target, Target.ZOrder, Root.ZOrder);
        Visit(Target);
      end;
    end
    else
      FZOrderAnimator.AddTarget(View, View.ZOrder, Root.ZOrder);
  end;

begin
  FZOrderAnimator.Finish;
  FZOrderAnimator.ClearTargets;
  Visit(Root);
  FZOrderAnimator.Start;
end;

procedure TViewLayout3D.Expand(Root: TView3D);

  procedure Visit(View, LastVisibleParent: TView3D);
  var
    I: integer;
    Target: TView3D;
  begin
    if View.Expanded then
      for I := 0 to View.ChildrenCount - 1 do
      begin
        Target := View.Children[I];
        FZOrderAnimator.AddTarget(Target, Root.ZOrder, Target.ZOrderOriginal);
        Visit(Target, Target);
      end
    else
      for I := 0 to View.ChildrenCount - 1 do
      begin
        Target := View.Children[I];
        FZOrderAnimator.AddTarget(Target, Root.ZOrder, LastVisibleParent.ZOrderOriginal);
        Visit(Target, LastVisibleParent);
      end;
  end;

begin
  FZOrderAnimator.Finish;
  FZOrderAnimator.ClearTargets;
  Visit(Root, Root);
  FZOrderAnimator.Start;
end;

procedure TViewLayout3D.SetRootView(V: TView3D);
begin
  // TODO: center fit initially

  CancelCaptureView;

  FRotationY := 0;
  FRotationX := 0;
  FZoomLevel := InitialZoomLevel;
  FScaleZ := 0;

  FRootView := V;
  VisibleBranch := V;

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
    OnDblClick := @DblClickHandler;

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
    OnDblClick := nil;
    PopupMenu := nil;
    Invalidate;
  end;
end;

procedure TViewLayout3D.StartCaptureView;
var
  View: TView3D;
  Task: TTask;
begin
  View := FRootView;
  repeat
    if CapturableWidgets.IndexOf(View.QualifiedClassName) <> -1 then
    begin
      Task := View.CreateCaptureViewTask;
      if Assigned(Task) then
      begin
        Task.OnSuccess := @CaptureViewTaskSuccessHandler;
        Task.OnFree := @CaptureViewTaskFreeHandler;
        StartTask(Task);
        FCaptureViewTaskViewMap.Add(Task, View);
      end;
    end;
    View := View.Next;
  until View = FRootView;
end;

procedure TViewLayout3D.CancelCaptureView;
var
  I: integer;
begin
  // Cancel all pending tasks and remove them from the map.
  // If by any chance a task successfully completes after this call,
  // it will be ignored because there will be no associated view in the map.
  for I := 0 to FCaptureViewTaskViewMap.Count - 1 do
    TCaptureViewTask(FCaptureViewTaskViewMap.Keys[I]).Cancel;
  FCaptureViewTaskViewMap.Clear;
end;

function TViewLayout3D.GetCaptureViewTaskView(Task: TCaptureViewTask): TView3D;
var
  I: integer;
begin
  if FCaptureViewTaskViewMap.Find(Task, I) then
    Result := FCaptureViewTaskViewMap.Data[I]
  else
    Result := nil;
end;

procedure TViewLayout3D.CaptureViewTaskFreeHandler(Sender: TObject);
begin
  // Remove the task from the map as it's about to be freed.
  FCaptureViewTaskViewMap.Remove(Sender);
end;

procedure TViewLayout3D.DblClickHandler(Sender: TObject);
begin
  if Assigned(ActiveView) then
    VisibleBranch := ActiveView
  else
    VisibleBranch := FRootView;
end;

procedure TViewLayout3D.SetRotationX(Deg: single);
begin
  if FRotationX <> Deg then
  begin
    FRotationX := Deg;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetActiveView(V: TView3D);
begin
  if FActiveView <> V then
  begin
    {$IFDEF DEBUG}
    if Assigned(V) then
      DebugLn(
        'TViewLayout3D.SetActiveView: Class=%s, Bounds=[%f, %f, %f, %f], TransformScaleX=%f, TransformScaleY=%f',
        [V.QualifiedClassName, V.Left, V.Top, V.Right, V.Bottom,
        V.TransformScaleX, V.TransformScaleY]);
    {$ENDIF}
    FActiveView := V;
    DoActiveViewChanged;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetVisibleBranch(V: TView3D);
begin
  if FVisibleBranch <> V then
  begin
    FVisibleBranch := V;
    if Assigned(V) then
    begin
      ShowBranch(V);
      Invalidate;
    end;

    if Assigned(FOnVisibleBranchChanged) then
      FOnVisibleBranchChanged(Self);
  end;
end;

procedure TViewLayout3D.SetView3DEnabled(V: boolean);
begin
  if FView3DEnabled = V then
    Exit;

  FView3DEnabled := V;
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

procedure TViewLayout3D.SetClipBounds(V: boolean);
begin
  if FClipBounds <> V then
  begin
    FClipBounds := V;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetHighlightedView(V: TView3D);
begin
  if FHighlightedView <> V then
  begin
    FHighlightedView := V;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetRotationY(Deg: single);
begin
  if FRotationY <> Deg then
  begin
    FRotationY := Deg;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetZoomLevel(V: single);
begin
  if FZoomLevel <> V then
  begin
    FZoomLevel := V;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetScaleZ(V: single);
begin
  FScaleZ := V;
  Invalidate;
end;

procedure TViewLayout3D.SetOriginX(V: single);
begin
  if FOriginX <> V then
  begin
    FOriginX := V;
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetOriginY(V: single);
begin
  if FOriginY <> V then
  begin
    FOriginY := V;
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

    if ActiveView = V then
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
  DebugLn('TViewLayout3D.DoOnPaint');

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
      if not View.Visible or (View.Visibility = vvGone) or (View.GetWidth = 0) or
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
  // Initiate dragging only with left click because right click
  // is used to show context menu.
  if Button = mbLeft then
  begin
    FMouseDown := True;
    FLastMouseX := X;
    FLastMouseY := Y;
  end;
end;

procedure TViewLayout3D.MouseUpHandler(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not FDragging then
    ActiveView := HitTest(X, Y)
  else
    FDragging := False;

  FMouseDown := False;
end;

procedure TViewLayout3D.MouseMoveHandler(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if FMouseDown then
  begin
    FDragging := True;
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
  if FDragging or FToggleView3DAnimator.IsRunning then
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
    FOnActiveViewChanged(Self);
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
    I: integer;
  begin
    V.Visible := True;
    for I := 0 to V.ChildrenCount - 1 do
      ShowView(V.Children[I]);
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

procedure TViewLayout3D.CaptureViewTaskSuccessHandler(Sender: TObject);
var
  TextureName: GLint = 0;
  Task: TCaptureViewTask absolute Sender;
  View: TView3D;
begin
  // Ignore this task if we don't find its associated view.
  // We don't keep the view reference in the task object because the view
  // may be destroyed before the task completes.
  // This can happen, for instance, when the layout is closed or another layout
  // is loaded.
  View := GetCaptureViewTaskView(Task);
  if not Assigned(View) then
    Exit;

  // Abort if image was not fetched or its width/height is 1px.
  if not Assigned(Task.Image) or (Task.Image.Width = 1) or (Task.Image.Height = 1) then
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
    Task.Image.Width,
    Task.Image.Height,
    0,
    GL_BGRA,
    GL_UNSIGNED_BYTE,
    Task.Image.RawImage.Data);

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
