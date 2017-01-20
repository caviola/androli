unit ViewLayout3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Menus, GR32_Image, View3DTypes, View3DTransformation,
  Animators;

type

  { TViewLayout3D }

  TViewLayout3D = class(TCustomPaintBox32)
  private
    FClipBounds: boolean;
    FHierarchyWidth: integer;
    FHierarchyHeight: integer;
    FAnimationEnabled: boolean;
    FAnimation: TAnimator;
    FOnVisibleBranchChanged: TNotifyEvent;
    FScaleZAnimator: TFloatAnimator;
    FZoomLevelAnimator: TFloatAnimator;
    FOriginX: single;
    FOriginY: single;
    FOriginZ: single;
    FFlatHierarchy: TView3DFlatTree;
    FTransform: TView3DTransformation;
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
    FContextMenu: TPopupMenu;
    FMenuItemShowAll: TMenuItem;
    FMenuItemClipBounds: TMenuItem;
    FOnActiveViewChanged: TNotifyEvent;
    FVisibleBranch: TView3D;
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
  protected
    procedure DblClickHandler(Sender: TObject);
    procedure DoPaintBuffer; override;

    procedure AnimationUpdateHandler(Animator: TAnimator;
      const InterpolatedFraction: single);
    procedure MenuItemClipBoundsClick(Sender: TObject);

    procedure MouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseUpHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MouseWheelHandler(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    //TODO: this is always available, so make it a toolbar button instead of contextual action
    procedure MenuItemShowAllClick(Sender: TObject);
    procedure ContextMenuPopup(Sender: TObject);

    procedure ResizeHandler(Sender: TObject);

    procedure DoActionShowAll;

    procedure DoActiveViewChanged;
    function HitTest(const X, Y: integer): TView3D;
    procedure ZoomAnimateValueHandler(Sender: TFloatAnimator; Value: single);
    procedure ScaleZAnimateValueHandler(Sender: TFloatAnimator; Value: single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Changed;
    procedure Zoom(Delta: integer);
    property RootView: TView3D write SetRootView;
    property RotationX: single read FRotationX write SetRotationX;
    property RotationY: single read FRotationY write SetRotationY;
    property ZoomLevel: single read FZoomLevel write SetZoomLevel;
    property ScaleZ: single read FScaleZ write SetScaleZ;
    property OriginX: single read FOriginX write SetOriginX;
    property OriginY: single read FOriginY write SetOriginY;
    property ActiveView: TView3D read FActiveView write SetActiveView;
    property HighlightedView: TView3D read FHighlightedView write SetHighlightedView;
    property AnimationEnabled: boolean read FAnimationEnabled write FAnimationEnabled;
    property OnActiveViewChanged: TNotifyEvent
      read FOnActiveViewChanged write FOnActiveViewChanged;
    property ClipBounds: boolean read FClipBounds write SetClipBounds;
    property VisibleBranch: TView3D read FVisibleBranch write SetVisibleBranch;
    property OnVisibleBranchChanged: TNotifyEvent
      read FOnVisibleBranchChanged write FOnVisibleBranchChanged;
  end;


implementation

uses
  SysUtils, Math, GR32, GR32_Polygons, LazLogger;

const
  // 32-bit colors in ABGR (don't undetstand why)
  clBackgroundCanvas32 = TColor32($FF212121);
  clBorderColor32 = $C0636363;
  clActiveBorderColor32 = TColor32($FFFF9430);
  clFilteredBorderColor32 = TColor32($C091EEFF);
  clFilteredContentColor32 = TColor32($2062E5FC);
  clPaddingColor32 = TColor32($50C3DEB7);
  clMarginColor32 = TColor32($50A0C5E8);
  clContentColor32 = TColor32($30F9824A);

  InitialZoomLevel = 0.5;
  InitialScaleZ = 20;
  InitialRotationX = 0;
  InitialRotationY = 0;

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

  AnimationDuration = 700;
  AnimationEndScaleZ = 30;
  AnimationEndRotationY = 30;

  CanvasPaddingVertical = 50;
  CanvasPaddingHorizontal = 50;

{ TViewLayout3D }

constructor TViewLayout3D.Create(AOwner: TComponent);

  procedure CreateContextMenu;
  begin
    FContextMenu := TPopupMenu.Create(Self);
    with FContextMenu do
    begin
      Parent := Self;
      OnPopup := @ContextMenuPopup;
      FMenuItemClipBounds :=
        NewItem('Clip Bounds', 0, False, True, @MenuItemClipBoundsClick, 0, '');
      FMenuItemShowAll := NewItem('Show all', 0, False, True,
        @MenuItemShowAllClick, 0, '');

      Items.Add(FMenuItemClipBounds);
      Items.Add(NewLine);
      Items.Add(FMenuItemShowAll);
    end;
  end;

begin
  inherited;

  FTransform := TView3DTransformation.Create;
  FFlatHierarchy := TView3DFlatTree.Create;

  FAnimation := TAnimator.Create;
  FAnimation.Duration := AnimationDuration;
  FAnimation.OnUpdate := @AnimationUpdateHandler;

  FScaleZAnimator := TFloatAnimator.Create(@ScaleZAnimateValueHandler);
  FScaleZAnimator.Duration := 200;
  FZoomLevelAnimator := TFloatAnimator.Create(@ZoomAnimateValueHandler);
  FZoomLevelAnimator.Duration := 200;

  RepaintMode := rmOptimizer;
  BufferOversize := 0;

  CreateContextMenu;
end;

destructor TViewLayout3D.Destroy;
begin
  FZoomLevelAnimator.Free;
  FScaleZAnimator.Free;
  FAnimation.Free;
  FFlatHierarchy.Free;
  FTransform.Free;
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

procedure TViewLayout3D.SetRootView(V: TView3D);
begin
  // TODO: center fit initially
  FRotationY := InitialRotationY;
  FRotationX := InitialRotationX;
  FZoomLevel := InitialZoomLevel;
  FScaleZ := InitialScaleZ;

  FFlatHierarchy.RootView := V;
  VisibleBranch := V;

  if Assigned(V) then
  begin
    // Initial origin X,Y is the center of root view.
    FOriginX := -(FFlatHierarchy.First.Right - FFlatHierarchy.First.Left) / 2;
    FOriginY := -(FFlatHierarchy.First.Bottom - FFlatHierarchy.First.Top) / 2;
    FOriginZ := -FFlatHierarchy.Last.ZOrder / 2;

    with FTransform do
    begin
      SetOrigin(FOriginX, FOriginY, FOriginZ);
      SetScale(FZoomLevel, FZoomLevel, FZoomLevel * FScaleZ);
      SetRotationY(FRotationY);
      SetTranslation(ClientWidth / 2, ClientHeight / 2, 0);
      SetCameraZ(1000);
    end;

    OnMouseDown := @MouseDownHandler;
    OnMouseUp := @MouseUpHandler;
    OnMouseMove := @MouseMoveHandler;
    OnMouseWheel := @MouseWheelHandler;
    OnDblClick := @DblClickHandler;
    PopupMenu := FContextMenu;
    OnResize := @ResizeHandler;

    if AnimationEnabled then
      // No need to Invalidate here because the animation starts right away
      // and will take care of it.
      FAnimation.Restart
    else
      Invalidate;
  end
  else
  begin
    OnMouseDown := nil;
    OnMouseUp := nil;
    OnMouseMove := nil;
    OnMouseWheel := nil;
    OnDblClick := nil;
    PopupMenu := nil;
    OnResize := nil;
    Invalidate;
  end;
end;

procedure TViewLayout3D.DblClickHandler(Sender: TObject);
begin
  if Assigned(ActiveView) then
    VisibleBranch := ActiveView
  else
    VisibleBranch := FFlatHierarchy.RootView;
end;

procedure TViewLayout3D.SetRotationX(Deg: single);
begin
  if FRotationX <> Deg then
  begin
    FRotationX := Deg;
    FTransform.SetRotationX(Deg);
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
        'TViewHierarchyCanvas.SetActiveView: Class=%s, Bounds=[%f, %f, %f, %f], TransformScaleX=%f, TransformScaleY=%f',
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
      FFlatHierarchy.ShowBranch(V);
      Invalidate;
    end;

    if Assigned(FOnVisibleBranchChanged) then
      FOnVisibleBranchChanged(Self);
  end;
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
    FTransform.SetRotationY(Deg);
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetZoomLevel(V: single);
begin
  if FZoomLevel <> V then
  begin
    FZoomLevel := V;
    FTransform.SetScale(FZoomLevel, FZoomLevel, FZoomLevel * FScaleZ);
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetScaleZ(V: single);
begin
  FScaleZ := V;
  FTransform.SetScale(FZoomLevel, FZoomLevel, FZoomLevel * FScaleZ);
  Invalidate;
end;

procedure TViewLayout3D.SetOriginX(V: single);
begin
  if FOriginX <> V then
  begin
    FOriginX := V;
    FTransform.SetOrigin(FOriginX, FOriginY, FOriginZ);
    Invalidate;
  end;
end;

procedure TViewLayout3D.SetOriginY(V: single);
begin
  if FOriginY <> V then
  begin
    FOriginY := V;
    FTransform.SetOrigin(FOriginX, FOriginY, FOriginZ);
    Invalidate;
  end;
end;

procedure TViewLayout3D.DoPaintBuffer;

  procedure PolyFill(Left, Top, Right, Bottom, Z: single; Color: TColor32);
  var
    P: array[0..3] of TFixedPoint;
  begin
    with FTransform do
    begin
      Transform(Left, Top, Z, P[0]);
      Transform(Right, Top, Z, P[1]);
      Transform(Right, Bottom, Z, P[2]);
      Transform(Left, Bottom, Z, P[3]);
    end;
    PolygonXS(Buffer, P, Color);
  end;

  procedure PolyLine(Left, Top, Right, Bottom, Z: single; Color: TColor32);
  var
    P: array[0..3] of TFixedPoint;
  begin
    with FTransform do
    begin
      Transform(Left, Top, Z, P[0]);
      Transform(Right, Top, Z, P[1]);
      Transform(Right, Bottom, Z, P[2]);
      Transform(Left, Bottom, Z, P[3]);
    end;
    PolylineXS(Buffer, P, Color, True);
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
    C: TColor32;
  begin
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

    with FTransform, V do
    begin
      // Store transformed points; we use them to perform hit test.
      if ClipBounds then
      begin
        Transform(ClippedLeft, ClippedTop, ZOrder, ViewportRect[0]);
        Transform(ClippedRight, ClippedTop, ZOrder, ViewportRect[1]);
        Transform(ClippedRight, ClippedBottom, ZOrder, ViewportRect[2]);
        Transform(ClippedLeft, ClippedBottom, ZOrder, ViewportRect[3]);
      end
      else
      begin
        Transform(Left, Top, ZOrder, ViewportRect[0]);
        Transform(Right, Top, ZOrder, ViewportRect[1]);
        Transform(Right, Bottom, ZOrder, ViewportRect[2]);
        Transform(Left, Bottom, ZOrder, ViewportRect[3]);
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
  I: integer;
  View: TView3D;
begin
  inherited;
  DebugLn('TViewHierarchyCanvas.DoPaintBuffer');

  Buffer.BeginUpdate;
  // TODO: only clear the rect that will be affected by all the transformations
  Buffer.Clear(clBackgroundCanvas32);

  if FFlatHierarchy.Count > 0 then
  begin
    FHierarchyWidth := 0;
    FHierarchyHeight := 0;

    for I := 0 to FFlatHierarchy.Count - 1 do
    begin
      View := FFlatHierarchy.Items[I];
      // Skip views that won't be visible to the user.
      if not View.Visible or (View.Visibility = vvGone) or (View.GetWidth = 0) or
        (View.GetHeight = 0) then
        Continue;
      if ClipBounds and ((View.GetClippedWidth = 0) or (View.GetClippedHeight = 0)) then
        Continue;

      DrawView(View);
    end;
  end;

  Buffer.EndUpdate;
end;

procedure TViewLayout3D.AnimationUpdateHandler(Animator: TAnimator;
  const InterpolatedFraction: single);
begin
  ScaleZ := IntegerEvaluator(InterpolatedFraction, InitialScaleZ, AnimationEndScaleZ);
  RotationY := FloatEvaluator(InterpolatedFraction, InitialRotationY,
    AnimationEndRotationY);
end;

procedure TViewLayout3D.MenuItemClipBoundsClick(Sender: TObject);
begin
  FMenuItemClipBounds.Checked := not FMenuItemClipBounds.Checked;
  ClipBounds := FMenuItemClipBounds.Checked;
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
      // Divide the mouse delta by current ZoomLevel level so that it remains
      // unaltered when transformed/scaled (multiplied by ZoomLevel).
      // The visual result is that the layout will always move by delta pixels
      // independent of current ZoomLevel level.
      OriginX := OriginX + (X - FLastMouseX) / ZoomLevel;
      OriginY := OriginY + (Y - FLastMouseY) / ZoomLevel;
    end
    else
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
  if not FDragging then
  begin
    WheelDelta := Sign(WheelDelta);
    // Constrain XY/Z scaling to predefined min/max values.
    if ssCtrl in Shift then
    begin
      FScaleZAnimator.SetValueInterval(ScaleZ, EnsureRange(FScaleZ +
        WheelDelta * StepScaleZ, MinScaleZ, MaxScaleZ));
      FScaleZAnimator.Restart;
    end
    else
      Zoom(WheelDelta);

    Handled := True;
  end
  else
    Handled := False;
end;

procedure TViewLayout3D.MenuItemShowAllClick(Sender: TObject);
begin
  DoActionShowAll;
end;

procedure TViewLayout3D.ContextMenuPopup(Sender: TObject);
begin
  //TODO:
end;

procedure TViewLayout3D.ResizeHandler(Sender: TObject);
begin
  FTransform.SetTranslation(ClientWidth / 2, ClientHeight / 2, 0);
  Invalidate;
end;

procedure TViewLayout3D.DoActionShowAll;
begin
  VisibleBranch := FFlatHierarchy.RootView;
end;

procedure TViewLayout3D.DoActiveViewChanged;
begin
  if Assigned(FOnActiveViewChanged) then
    FOnActiveViewChanged(Self);
end;

function TViewLayout3D.HitTest(const X, Y: integer): TView3D;
var
  I: integer;
  V: TView3D;
begin
  Result := nil;
  for I := FFlatHierarchy.Count - 1 downto 0 do
  begin
    V := FFlatHierarchy.Items[I];

    // Don't take into account views that are not visible to the user.
    if not V.Visible or (V.GetWidth = 0) or (V.GetHeight = 0) then
      Continue;
    if ClipBounds and ((V.GetClippedWidth = 0) or (V.GetClippedHeight = 0)) then
      Continue;

    if V.Contains(X, Y) then
    begin
      Result := V;
      Exit;
    end;
  end;
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

end.
