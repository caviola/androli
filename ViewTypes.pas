unit ViewTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, TaskRunner, Graphics;

const
  MinCaptureViewWidth = 2;
  MinCaptureViewHeight = 2;

type

  TViewFlags = set of (
    vfExpanding,
    vfCollapsing,
    vfExpanded,
    vfVisibilityInvisible,
    vfVisibilityGone,
    vfMatchFilter);

  { TView }

  TView = class
  private
    FFlags: TViewFlags;
    FProperties: TStringList;
    FTextureName: cardinal;
    FInflightCaptureViewTask: ITask;
    FParent: TView;
    FOriginalLeft: single;
    FOriginalTop: single;
    FOriginalRight: single;
    FOriginalBottom: single;
    FOriginalZOrder: single;
    FLeft: single;
    FTop: single;
    FRight: single;
    FBottom: single;
    FZOrder: single;
    FPaddingLeft: single;
    FPaddingTop: single;
    FPaddingRight: single;
    FPaddingBottom: single;
    FMarginLeft: single;
    FMarginTop: single;
    FMarginRight: single;
    FMarginBottom: single;
    FTransformScaleX: single;
    FTransformScaleY: single;
    FNext: TView;
    FPrevious: TView;
    FFirstChild: TView;
    FNextSibbling: TView;
    FPrevSibbling: TView;
    FHashCode: string;
    FQualifiedClassName: string;
    FTreeNodeText: string;
    function GetExpanded: boolean; inline;
    function GetMatchFilter: boolean; inline;
    function GetSimpleClassName: string;
    function GetChildrenCount: integer; inline;
    function GetVisibilityGone: boolean; inline;
    procedure SetExpanded(AValue: boolean); inline;
    procedure SetInflightCaptureViewTask(AValue: ITask);
    procedure SetMatchFilter(AValue: boolean); inline;
    function GetViewportWidth: integer; inline;
    function GetViewportHeight: integer; inline;
    function GetWidth: single; inline;
    function GetHeight: single; inline;
  public
    ViewportRect: array[0..3] of TPoint;
    constructor Create; overload;
    constructor Create(ALeft, ATop, ARight, ABottom, AZOrder: integer); overload;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, ARight, ABottom, AZOrder: single);
    procedure SetPaddings(ALeft, ATop, ARight, ABottom: single);
    procedure SetMargins(ALeft, ATop, ARight, ABottom: single);
    function Contains(const X, Y: integer): boolean;
    procedure SetProperty(const Name, Value: string);
    procedure AddChild(AView: TView);
    function HasProp(const Name: string): boolean; inline;
    function GetProp(const Name: string): string; inline;
    function GetIntProp(const Name: string; DefaultValue: integer = 0): integer; inline;
    function GetFloatProp(const Name: string; DefaultValue: single = 0): single; inline;
    function GetBoolProp(const Name: string;
      DefaultValue: boolean = False): boolean;
    function GetPropCount: integer; inline;
    procedure GetPropNameValue(I: integer; out Name, Value: string); inline;
    property Parent: TView read FParent;
    property Next: TView read FNext;
    property Previous: TView read FPrevious;
    property FirstChild: TView read FFirstChild;
    property NextSibbling: TView read FNextSibbling;
    property PrevSibbling: TView read FPrevSibbling;
    property Left: single read FLeft;
    property Top: single read FTop;
    property Right: single read FRight;
    property Bottom: single read FBottom;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property PaddingLeft: single read FPaddingLeft;
    property PaddingTop: single read FPaddingTop;
    property PaddingRight: single read FPaddingRight;
    property PaddingBottom: single read FPaddingBottom;
    property MarginLeft: single read FMarginLeft;
    property MarginTop: single read FMarginTop;
    property MarginRight: single read FMarginRight;
    property MarginBottom: single read FMarginBottom;
    property TransformScaleX: single read FTransformScaleX write FTransformScaleX;
    property TransformScaleY: single read FTransformScaleX write FTransformScaleY;
    property ViewportWidth: integer read GetViewportWidth;
    property ViewportHeight: integer read GetViewportHeight;
    property HashCode: string read FHashCode write FHashCode;
    property ZOrder: single read FZOrder write FZOrder;
    property ZOrderOriginal: single read FOriginalZOrder;
    property TreeNodeText: string read FTreeNodeText write FTreeNodeText;
    property ChildrenCount: integer read GetChildrenCount;
    property QualifiedClassName: string read FQualifiedClassName
      write FQualifiedClassName;
    property SimpleClassName: string read GetSimpleClassName;
    property Expanded: boolean read GetExpanded write SetExpanded;
    property TextureName: cardinal read FTextureName write FTextureName;
    property InflightCaptureViewTask: ITask
      read FInflightCaptureViewTask write SetInflightCaptureViewTask;
    property VisibilityGone: boolean read GetVisibilityGone;
    property MatchFilter: boolean read GetMatchFilter write SetMatchFilter;
  end;

  { ICaptureViewTask }

  ICaptureViewTask = interface(ITask)
    ['{482F5D70-E9F0-493C-800E-ED6F4A9B40A5}']
    function GetAssociatedView: TView;
    property AssociatedView: TView read GetAssociatedView;
  end;

  TCaptureViewResultEvent = procedure(const Task: ITask; Image: TRasterImage;
    View: TView) of object;

  { TCaptureViewTask }

  TCaptureViewTask = class(TTask, ICaptureViewTask)
  private
    FView: TView;
    FResult: TRasterImage;
    FOnResult: TCaptureViewResultEvent;
  protected
    procedure DoOnSuccess; override;
    procedure SetResult(AValue: TRasterImage);
    function GetAssociatedView: TView; inline;
  public
    constructor Create(AView: TView);
    destructor Destroy; override;
    property OnResult: TCaptureViewResultEvent read FOnResult write FOnResult;
  end;

  { IViewLayout }

  IViewLayout = interface
    ['{16F9F11F-D19C-4830-A8EC-50DD658F4D96}']
    procedure Changed;
    function GetActiveView: TView;
    function HitTest(const X, Y: integer): TView;
    function GetActiveBranch: TView;
    function SetActiveBranch(AValue: TView): boolean;
    function GetRootView: TView;
    function SetActiveView(AValue: TView): boolean;
    procedure SetClipBounds(AValue: boolean);
    property ActiveView: TView read GetActiveView;
    property ActiveBranch: TView read GetActiveBranch;
    property RootView: TView read GetRootView;
    property ClipBounds: boolean write SetClipBounds;
  end;

  { TViewLayout }

  TViewLayout = class(TInterfacedObject, IViewLayout)
  protected
    FRootView: TView;
    FActiveBranch: TView;
    FActiveView: TView;
    FOnChange: TObjectProcedure;
    FClipBounds: boolean;
    procedure Flatten;
    function HitTest(const X, Y: integer): TView;
    function GetActiveBranch: TView; inline;
    function SetActiveBranch(AValue: TView): boolean; virtual;
    function GetRootView: TView; inline;
    function GetActiveView: TView; inline;
    function SetActiveView(AValue: TView): boolean;
    procedure Changed;
    procedure DoOnChange;
  public
    constructor Create(ARootView: TView);
    destructor Destroy; override;
    procedure SetClipBounds(AValue: boolean);
    property OnChange: TObjectProcedure read FOnChange write FOnChange;
  end;

  { ILayoutLoadTask }

  ILayoutLoadTask = interface(ITask)
    ['{F1CC2749-77DC-4AC4-A183-19ECDB4E3924}']
    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;
  end;

  TLayoutLoadResultEvent = procedure(const Task: ITask;
    TheResult: TViewLayout) of object;

  { TLayoutLoadTask }

  TLayoutLoadTask = class(TTask, ILayoutLoadTask)
  private
    FOnResult: TLayoutLoadResultEvent;
    FResult: TViewLayout;
  protected
    procedure SetResult(AValue: TViewLayout);
    procedure DoOnSuccess; override;
  public
    destructor Destroy; override;
    function GetDisplayName: string; virtual; abstract;
    property OnResult: TLayoutLoadResultEvent read FOnResult write FOnResult;
  end;


implementation

uses
  SysUtils, LCLProc, Logging, contnrs, Math;

{ TViewLayout }

procedure TViewLayout.Flatten;
var
  Q: TQueue;
  PreviousView, View, ViewChild: TView;
  ElementsToDepthIncrease: integer = 1;
  NextElementsToDepthIncreate: integer = 0;
begin
  // Link views in ActiveBranch in a circular, double-linked list
  // by their Next/Previous properties in breadth-first order.
  PreviousView := FActiveBranch;
  Q := TQueue.Create;
  try
    Q.Push(FActiveBranch);
    while Q.Count > 0 do
    begin
      View := TView(Q.Pop);
      PreviousView.FNext := View;
      View.FPrevious := PreviousView;
      PreviousView := View;

      Inc(NextElementsToDepthIncreate, View.GetChildrenCount);
      Dec(ElementsToDepthIncrease);
      if ElementsToDepthIncrease = 0 then
      begin
        ElementsToDepthIncrease := NextElementsToDepthIncreate;
        NextElementsToDepthIncreate := 0;
      end;

      ViewChild := View.FirstChild;
      if Assigned(ViewChild) then
        repeat
          Q.Push(ViewChild);
          ViewChild := ViewChild.NextSibbling;
        until ViewChild = View.FirstChild;
    end;
  finally
    Q.Free;
  end;

  // Finish off by making the double-linked list circular.
  PreviousView.FNext := FActiveBranch;
  FActiveBranch.FPrevious := PreviousView;
end;

function TViewLayout.HitTest(const X, Y: integer): TView;
begin
  // Start at last view in ActiveBranch and traverse the list backwards.
  Result := FActiveBranch.Previous;
  repeat
    // Skip views that are not visible to the user.
    if (Result.Width = 0) or (Result.Height = 0) then
      Result := Result.Previous // continue
    else
    if Result.Contains(X, Y) then
      Exit // success
    else
      Result := Result.Previous; // continue
  until Result = FActiveBranch.Previous;
  Result := nil;
end;

function TViewLayout.GetActiveBranch: TView;
begin
  Result := FActiveBranch;
end;

function TViewLayout.SetActiveBranch(AValue: TView): boolean;
begin
  // Activate root view if requested branch is nil.
  if not Assigned(AValue) then
    AValue := FRootView;

  if FActiveBranch <> AValue then
  begin
    Log('TViewLayout.SetActiveBranch %s', [DbgS(AValue)]);
    FActiveView := nil;
    FActiveBranch := AValue;
    Flatten;
    Result := True;
  end
  else
    Result := False;
end;

function TViewLayout.GetRootView: TView;
begin
  Result := FRootView;
end;

function TViewLayout.GetActiveView: TView;
begin
  Result := FActiveView;
end;

function TViewLayout.SetActiveView(AValue: TView): boolean;
begin
  if FActiveView <> AValue then
  begin
    Log('TViewLayout.SetActiveView %s', [DbgS(AValue)]);
    FActiveView := AValue;
    Result := True;
  end
  else
    Result := False;
end;

procedure TViewLayout.SetClipBounds(AValue: boolean);

  procedure Visit(AView: TView);
  var
    Child: TView;
  begin
    with AView do
    begin
      if AValue then // clip bounds?
        if Assigned(Parent) then
        begin
          FLeft := EnsureRange(FLeft, Parent.Left, Parent.Right);
          FTop := EnsureRange(FTop, Parent.Top, Parent.Bottom);
          FRight := EnsureRange(FRight, Parent.Left, Parent.Right);
          FBottom := EnsureRange(FBottom, Parent.Top, Parent.Bottom);
        end
        else
        begin
          FLeft := FOriginalLeft;
          FTop := FOriginalTop;
          FRight := FOriginalRight;
          FBottom := FOriginalBottom;
        end
      else // restore original bounds
      begin
        FLeft := FOriginalLeft;
        FTop := FOriginalTop;
        FRight := FOriginalRight;
        FBottom := FOriginalBottom;
      end;

      Child := FirstChild;
      if Assigned(Child) then
        repeat
          Visit(Child);
          Child := Child.NextSibbling;
        until Child = FirstChild;
    end;
  end;

begin
  if FClipBounds <> AValue then
  begin
    FClipBounds := AValue;
    Visit(FRootView);
    DoOnChange;
  end;
end;

procedure TViewLayout.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange;
end;

constructor TViewLayout.Create(ARootView: TView);
begin
  Assert(Assigned(ARootView), 'ARootView = nil');
  FRootView := ARootView;
  SetActiveBranch(ARootView);
end;

destructor TViewLayout.Destroy;
begin
  FRootView.Free;
  inherited;
end;

procedure TViewLayout.Changed;
begin
  DoOnChange;
end;

{ TLayoutLoadTask }

destructor TLayoutLoadTask.Destroy;
begin
  if Assigned(FResult) then
    FResult.Free;
  inherited;
end;

procedure TLayoutLoadTask.SetResult(AValue: TViewLayout);
begin
  Assert(not Assigned(FResult), 'FResult <> nil');
  FResult := AValue;
end;

procedure TLayoutLoadTask.DoOnSuccess;
begin
  inherited;

  if Assigned(FOnResult) then
  begin
    FOnResult(Self, FResult);
    FResult := nil;
  end;
end;

{ TCaptureViewTask }

procedure TCaptureViewTask.SetResult(AValue: TRasterImage);
begin
  Assert(not Assigned(FResult), 'FResult <> nil');
  FResult := AValue;
end;

function TCaptureViewTask.GetAssociatedView: TView;
begin
  Result := FView;
end;

constructor TCaptureViewTask.Create(AView: TView);
begin
  Assert(Assigned(AView), 'AView = nil');
  FView := AView;
  FView.InflightCaptureViewTask := Self;
end;

destructor TCaptureViewTask.Destroy;
begin
  if Assigned(FResult) then
    FResult.Free;
  inherited;
end;

procedure TCaptureViewTask.DoOnSuccess;
begin
  inherited;

  if Assigned(FOnResult) then
  begin
    Assert(Assigned(FResult), 'FResult = nil');
    FOnResult(Self, FResult, FView);
    FResult := nil;
  end;
end;

{ TView }

function TView.GetSimpleClassName: string;
var
  P: integer;
begin
  P := LastDelimiter('.', QualifiedClassName);
  if P <> 0 then
    Result := Copy(QualifiedClassName, P + 1)
  else
    Result := QualifiedClassName;
end;

function TView.GetExpanded: boolean;
begin
  Result := vfExpanded in FFlags;
end;

function TView.GetMatchFilter: boolean;
begin
  Result := vfMatchFilter in FFlags;
end;

function TView.GetPropCount: integer;
begin
  Result := FProperties.Count;
end;

procedure TView.GetPropNameValue(I: integer; out Name, Value: string);
begin
  FProperties.GetNameValue(I, Name, Value);
end;

function TView.GetChildrenCount: integer;
var
  ViewChild: TView;
begin
  Result := 0;
  ViewChild := FirstChild;
  if Assigned(ViewChild) then
    repeat
      Inc(Result);
      ViewChild := ViewChild.NextSibbling;
    until ViewChild = FirstChild;
end;

function TView.GetVisibilityGone: boolean;
begin
  Result := vfVisibilityGone in FFlags;
end;

procedure TView.SetExpanded(AValue: boolean);
begin
  if AValue then
    Include(FFlags, vfExpanded)
  else
    Exclude(FFlags, vfExpanded);
end;

procedure TView.SetInflightCaptureViewTask(AValue: ITask);
begin
  if Assigned(FInflightCaptureViewTask) then
    FInflightCaptureViewTask.Cancel;
  FInflightCaptureViewTask := AValue;
end;

procedure TView.SetMatchFilter(AValue: boolean);
begin
  if AValue then
    Include(FFlags, vfMatchFilter)
  else
    Exclude(FFlags, vfMatchFilter);
end;

constructor TView.Create;
begin
  FProperties := TStringList.Create;
  FProperties.Duplicates := dupIgnore;
  FProperties.Sorted := True;
  FTransformScaleX := 1;
  FTransformScaleY := 1;
  Expanded := True;
end;

constructor TView.Create(ALeft, ATop, ARight, ABottom, AZOrder: integer);
begin
  Create;
  SetBounds(ALeft, ATop, ARight, ABottom, AZOrder);
end;

destructor TView.Destroy;
var
  CurrentChild, NextChild: TView;
begin
  FProperties.Free;

  // Free all children.
  CurrentChild := FirstChild;
  if Assigned(CurrentChild) then
    repeat
      NextChild := CurrentChild.NextSibbling;
      CurrentChild.Free;
      CurrentChild := NextChild;
    until CurrentChild = FirstChild;

  if Assigned(FInflightCaptureViewTask) then
  begin
    FInflightCaptureViewTask.Cancel;
    FInflightCaptureViewTask := nil;
  end;

  inherited;
end;

procedure TView.SetBounds(ALeft, ATop, ARight, ABottom, AZOrder: single);
begin
  // Keep original bounds separate as we use them when clipping/unclipping.
  // Original bounds must be provided window absolute, that is,
  // relative to root view.
  FOriginalLeft := ALeft;
  FOriginalTop := ATop;
  FOriginalRight := ARight;
  FOriginalBottom := ABottom;
  FOriginalZOrder := AZOrder;

  FLeft := ALeft;
  FTop := ATop;
  FRight := ARight;
  FBottom := ABottom;
  FZOrder := AZOrder;
end;

procedure TView.SetPaddings(ALeft, ATop, ARight, ABottom: single);
begin
  FPaddingLeft := ALeft;
  FPaddingTop := ATop;
  FPaddingRight := ARight;
  FPaddingBottom := ABottom;
end;

procedure TView.SetMargins(ALeft, ATop, ARight, ABottom: single);
begin
  FMarginLeft := ALeft;
  FMarginTop := ATop;
  FMarginRight := ARight;
  FMarginBottom := ABottom;
end;

function TView.Contains(const X, Y: integer): boolean;
var
  I, J: integer;
begin
  Result := False;
  J := 3;
  for I := 0 to 3 do
  begin
    if (((ViewportRect[I].Y <= Y) and (Y < ViewportRect[J].Y)) or
      ((ViewportRect[J].Y <= Y) and (Y < ViewportRect[I].Y))) and
      (X < ((ViewportRect[J].X - ViewportRect[I].X) * (Y - ViewportRect[I].Y) /
      (ViewportRect[J].Y - ViewportRect[I].Y) + ViewportRect[I].X)) then

      Result := not Result;
    J := I;
  end;
end;

procedure TView.SetProperty(const Name, Value: string);
begin
  if Name = 'getVisibility()' then
    if Value = 'INVISIBLE' then
      Include(FFlags, vfVisibilityInvisible)
    else
    if Value = 'GONE' then
      Include(FFlags, vfVisibilityGone);

  if (Name <> 'mID') or (Value <> 'NO_ID') then
    FProperties.Add(Name + '=' + Value);
end;

procedure TView.AddChild(AView: TView);
var
  LastChild: TView;
begin
  Assert(not Assigned(AView.Parent), 'AView.Parent must be nil');
  Assert(not Assigned(AView.NextSibbling), 'AView.NextSibbling must be nil');
  Assert(not Assigned(AView.PrevSibbling), 'AView.PrevSibbling must be nil');

  AView.FParent := Self;

  // Add the new child at the end of a circular, double-linked list.
  if not Assigned(FFirstChild) then
  begin
    AView.FNextSibbling := AView;
    AView.FPrevSibbling := AView;
    FFirstChild := AView;
  end
  else
  begin
    LastChild := FFirstChild.PrevSibbling;
    LastChild.FNextSibbling := AView;
    AView.FPrevSibbling := LastChild;
    AView.FNextSibbling := FirstChild;
    FFirstChild.FPrevSibbling := AView;
  end;
end;

function TView.HasProp(const Name: string): boolean;
begin
  Result := FProperties.IndexOfName(Name) <> -1;
end;

function TView.GetProp(const Name: string): string;
begin
  Result := FProperties.Values[Name];
end;

function TView.GetIntProp(const Name: string; DefaultValue: integer): integer;
begin
  Result := StrToIntDef(FProperties.Values[Name], DefaultValue);
end;

function TView.GetFloatProp(const Name: string; DefaultValue: single): single;
begin
  Result := StrToFloatDef(FProperties.Values[Name], DefaultValue);
end;

function TView.GetBoolProp(const Name: string; DefaultValue: boolean): boolean;
var
  Value: string;
begin
  Value := FProperties.Values[Name];
  if Value = 'true' then
    Result := True
  else if Value = 'false' then
    Result := False
  else
    Result := DefaultValue;
end;

function TView.GetViewportWidth: integer;
begin
  Result := ViewportRect[1].X - ViewportRect[0].X;
end;

function TView.GetViewportHeight: integer;
begin
  Result := ViewportRect[2].Y - ViewportRect[1].Y;
end;

function TView.GetWidth: single;
begin
  Result := FRight - FLeft;
end;

function TView.GetHeight: single;
begin
  Result := FBottom - FTop;
end;

end.
