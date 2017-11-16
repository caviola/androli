unit LayoutViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, Menus, ExtCtrls, ViewTypes, Animators, OpenGLContext,
  TaskRunner;

type

  TMouseState = (msNone, msDown, msDragging);

  TZOrderAnimator = class;
  TMode3DToggleAnimator = class;

  TActiveViewChangedEvent = procedure(NewView: TView) of object;
  TActiveBranchChangedEvent = procedure(NewBranch: TView) of object;

  { TLayoutViewer }

  TLayoutViewer = class(TOpenGLControl)
  private
    FMode3D: boolean;
    FLayout: IViewLayout;
    FHierarchyWidth: integer;
    FHierarchyHeight: integer;
    FZOrderAnimator: TZOrderAnimator;
    FMode3DToggleAnimator: TMode3DToggleAnimator;
    FScaleZAnimator: TFloatAnimator;
    FZoomLevelAnimator: TFloatAnimator;
    FOriginX: single;
    FOriginY: single;
    FOriginZ: single;
    FActiveView: TView;
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
    procedure SetMode3D(AValue: boolean);
    procedure SetHighlightedView(AValue: TView);
    procedure SetRotationX(Degres: single);
    procedure SetRotationY(Degres: single);
    procedure SetZoomLevel(AValue: single);
    procedure SetScaleZ(AValue: single);
    procedure SetOriginX(AValue: single);
    procedure SetOriginY(AValue: single);
  protected
    procedure ActiveViewChangedTimerTimer(Sender: TObject);
    procedure MouseLeaveHandler(Sender: TObject);

    procedure Mode3DToggleAnimatorUpdate(Sender: TAnimator;
      const InterpolatedFraction: single);

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
    procedure ZoomAnimateValueHandler(Sender: TFloatAnimator; Value: single);
    procedure ScaleZAnimateValueHandler(Sender: TFloatAnimator; Value: single);
    procedure ZOrderAnimatorUpdateHandler(Sender: TAnimator;
      const {%H-}InterpolatedFraction: single);
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
    property RotationX: single read FRotationX write SetRotationX;
    property RotationY: single read FRotationY write SetRotationY;
    property ZoomLevel: single read FZoomLevel write SetZoomLevel;
    property ScaleZ: single read FScaleZ write SetScaleZ;
    property OriginX: single read FOriginX write SetOriginX;
    property OriginY: single read FOriginY write SetOriginY;
    property ActiveView: TView read FActiveView;
    property HighlightedView: TView read FHighlightedView write SetHighlightedView;
    // Fired when user changes active view.
    property OnActiveViewChanged: TActiveViewChangedEvent
      read FOnActiveViewChanged write FOnActiveViewChanged;
    // Fires when user changes active branch.
    property OnActiveBranchChanged: TActiveBranchChangedEvent
      write FOnActiveBranchChanged;
    property Mode3D: boolean read FMode3D write SetMode3D;
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

  { TMode3DToggleAnimator }

  TMode3DToggleAnimator = class(TAnimator)
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
  SysUtils, LCLIntf, LCLProc, Math, gl, glu, Logging;

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

  Mode3DScaleZ = 20;
  Mode3DRotationX = 0;
  Mode3DRotationY = 30;

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

{ TMode3DToggleAnimator }

procedure TMode3DToggleAnimator.SetRotationYInterval(
  const StartValue, EndValue: single);
begin
  FRotationYStartValue := StartValue;
  FRotationYEndValue := EndValue;
end;

procedure TMode3DToggleAnimator.SetScaleZInterval(const StartValue, EndValue: single);
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
  FMode3DToggleAnimator := TMode3DToggleAnimator.Create;
  FMode3DToggleAnimator.Duration := 500;
  FMode3DToggleAnimator.OnUpdate := @Mode3DToggleAnimatorUpdate;

  FScaleZAnimator := TFloatAnimator.Create(@ScaleZAnimateValueHandler);
  FScaleZAnimator.Duration := 200;
  FZoomLevelAnimator := TFloatAnimator.Create(@ZoomAnimateValueHandler);
  FZoomLevelAnimator.Duration := 200;

  FZOrderAnimator := TZOrderAnimator.Create;
  FZOrderAnimator.Duration := 200;
  FZOrderAnimator.OnUpdate := @ZOrderAnimatorUpdateHandler;
end;

destructor TLayoutViewer.Destroy;
begin
  FZOrderAnimator.Free;
  FZoomLevelAnimator.Free;
  FScaleZAnimator.Free;
  FMode3DToggleAnimator.Free;
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
  if FLayout.SetActiveBranch(AValue) then
    Invalidate;
end;

procedure TLayoutViewer.SetLayout(AValue: IViewLayout);
var
  RootView: TView;
begin
  if Assigned(AValue) then
    Log('TLayoutViewer.SetViewLayout %s: ActiveBranch=%s',
      [DbgS(Pointer(AValue)), DbgS(AValue.ActiveBranch)])
  else
    Log('TLayoutViewer.SetViewLayout nil');

  // TODO: center fit initially

  FRotationY := 0;
  FRotationX := 0;
  FZoomLevel := InitialZoomLevel;
  FScaleZ := 0;

  FLayout := AValue;
  FActiveView := nil;

  if Assigned(FLayout) then
  begin
    RootView := FLayout.ActiveBranch;
    FCameraZ := CameraDistance + RootView.Previous.ZOrderOriginal;
    // Initial origin X,Y is the center of root view.
    FOriginX := (RootView.Right - RootView.Left) / 2;
    FOriginY := (RootView.Bottom - RootView.Top) / 2;
    FOriginZ := RootView.Previous.ZOrderOriginal / 2;

    OnMouseDown := @MouseDownHandler;
    OnMouseUp := @MouseUpHandler;
    OnMouseMove := @MouseMoveHandler;
    OnMouseWheel := @MouseWheelHandler;
    OnMouseLeave := @MouseLeaveHandler;
    OnClick := @MouseClickHandler;
    OnDblClick := @MouseDblClickHandler;

    if Mode3D then
    begin
      // No need to Invalidate here because the animation starts right away
      // and will take care of it.
      FMode3DToggleAnimator.SetRotationYInterval(0, 0);
      FMode3DToggleAnimator.SetScaleZInterval(0, Mode3DScaleZ);
      FMode3DToggleAnimator.Restart;
    end
    else
      Invalidate;
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

procedure TLayoutViewer.ActiveViewChangedTimerTimer(Sender: TObject);
begin
  LogEnterMethod('TLayoutViewer.ActiveViewChangedTimerTimer');

  FActiveViewChangedTimer.Enabled := False;
  DoActiveViewChanged;

  LogExitMethod('TLayoutViewer.ActiveViewChangedTimerTimer');
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
  if FActiveView <> AValue then
  begin
    Log('TLayoutViewer.SetActiveView %s', [DbgS(AValue)]);
    FActiveView := AValue;
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
    FMode3DToggleAnimator.SetRotationYInterval(0, 0);
    FMode3DToggleAnimator.SetScaleZInterval(0, Mode3DScaleZ);
  end
  else
  begin
    FMode3DToggleAnimator.SetRotationYInterval(RotationY, 0);
    FMode3DToggleAnimator.SetScaleZInterval(ScaleZ, 0);
  end;

  FMode3DToggleAnimator.Restart;
end;

procedure TLayoutViewer.SetHighlightedView(AValue: TView);
begin
  if FHighlightedView <> AValue then
  begin
    FHighlightedView := AValue;
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

  procedure PolyFill(Left, Top, Right, Bottom, Z: single; Color: TColorABGR); inline;
  begin
    glColor4ubv(@Color);
    glBegin(GL_POLYGON);
    glVertex3f(Left, Top, Z);
    glVertex3f(Right, Top, Z);
    glVertex3f(Right, Bottom, Z);
    glVertex3f(Left, Bottom, Z);
    glEnd;
  end;

  procedure PolyLine(Left, Top, Right, Bottom, Z: single; Color: TColorABGR); inline;
  begin
    glColor4ubv(@Color);
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

  procedure DrawMargin(View: TView);
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

  procedure DrawView(V: TView; Texture: cardinal);
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
    if Texture > 0 then
      DrawTexture(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, Texture);

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

      PolyLine(V.Left, V.Top, V.Right, V.Bottom, V.ZOrder, C);
    end;

    glGetDoublev(GL_MODELVIEW_MATRIX, ModelViewMatrix);
    glGetDoublev(GL_PROJECTION_MATRIX, ProjectionMatrix);
    glGetIntegerv(GL_VIEWPORT, ViewportRect);

    with V do
    begin
      // Store transformed points; we use them to perform hit test.
      ViewportRect[0] := GetWindowPoint(Left, Top, ZOrder);
      ViewportRect[1] := GetWindowPoint(Right, Top, ZOrder);
      ViewportRect[2] := GetWindowPoint(Right, Bottom, ZOrder);
      ViewportRect[3] := GetWindowPoint(Left, Bottom, ZOrder);

      // Keep track of the hierarchy's width and height.
      // We use this for "center fit" and determining how much can the
      // hierarchy be moved while dragging.
      FHierarchyWidth := Max(FHierarchyWidth, ViewportWidth +
        CanvasPaddingHorizontal * 2);
      FHierarchyHeight := Max(FHierarchyHeight, ViewportHeight +
        CanvasPaddingVertical * 2);
    end;
  end;

var
  View, CurrentRoot: TView;
begin
  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  try
    if not Assigned(FLayout) then
      Exit;

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    // Note that the order in which we apply transformations is important.
    // 1. Move to (-OriginX, OriginY, -FCameraZ).
    glTranslatef(-OriginX, OriginY, -FCameraZ);
    // 2. Invert our Y-axis.
    glScalef(1, -1, 1);

    if FMode3DToggleAnimator.IsRunning then
    begin
      // 3. Rotate and scale Z coords around (OriginX, OriginY, FOriginZ).
      glTranslatef(OriginX, OriginY, FOriginZ);
      glRotatef(RotationY, 0, 1, 0);
      glScalef(1, 1, ScaleZ);
      glTranslatef(-OriginX, -OriginY, -FOriginZ);
    end
    else if Mode3D then
    begin
      // 3. Rotate and scale Z coords around (OriginX, OriginY, FOriginZ).
      glTranslatef(OriginX, OriginY, FOriginZ);
      glRotatef(RotationY, 0, 1, 0);
      glScalef(1, 1, ScaleZ);
      glTranslatef(-OriginX, -OriginY, -FOriginZ);
    end
    else
      glScalef(1, 1, 0); // 2D mode, drop Z coord

    // 4. Perspective projection.
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glScalef(ZoomLevel, ZoomLevel, 1);
    gluPerspective(45, Width / Height, 0, FCameraZ);

    FHierarchyWidth := 0;
    FHierarchyHeight := 0;

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

procedure TLayoutViewer.Mode3DToggleAnimatorUpdate(Sender: TAnimator;
  const InterpolatedFraction: single);
var
  A: TMode3DToggleAnimator absolute Sender;
begin
  ScaleZ := FloatEvaluator(InterpolatedFraction, A.ScaleZStartValue,
    A.ScateZEndValue);
  RotationY := FloatEvaluator(InterpolatedFraction, A.RotationYStartValue,
    A.RotationYEndValue);
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

  if FLayout.SetActiveBranch(ActiveView) then // active branch changed?
  begin
    FActiveViewChangedTimer.Enabled := False;
    if Assigned(FOnActiveBranchChanged) then
      FOnActiveBranchChanged(FLayout.ActiveBranch);
    Invalidate;
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
  if (FMouseState = msDragging) or FMode3DToggleAnimator.IsRunning then
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
  if Assigned(FOnActiveViewChanged) then
    FOnActiveViewChanged(FActiveView);
end;

procedure TLayoutViewer.ZoomAnimateValueHandler(Sender: TFloatAnimator; Value: single);
begin
  ZoomLevel := Value;
end;

procedure TLayoutViewer.ScaleZAnimateValueHandler(Sender: TFloatAnimator;
  Value: single);
begin
  ScaleZ := Value;
end;

procedure TLayoutViewer.ZOrderAnimatorUpdateHandler(Sender: TAnimator;
  const InterpolatedFraction: single);
begin
  Invalidate;
end;

end.
