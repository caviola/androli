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
    function GetClippedWidth: single; inline;
    function GetClippedHeight: single; inline;
  public
    Parent: TView;
    Next: TView;
    Previous: TView;
    FirstChild: TView;
    NextSibbling: TView;
    PrevSibbling: TView;
    HashCode: string;
    QualifiedClassName: string;
    ZOrder: single;
    ZOrderOriginal: single;
    ViewportRect: array[0..3] of TPoint;
    Left: single;
    Top: single;
    Right: single;
    Bottom: single;
    PaddingLeft: single;
    PaddingTop: single;
    PaddingRight: single;
    PaddingBottom: single;
    MarginLeft: single;
    MarginTop: single;
    MarginRight: single;
    MarginBottom: single;
    ClippedLeft: single;
    ClippedTop: single;
    ClippedRight: single;
    ClippedBottom: single;
    TreeNodeText: string;
    TransformScaleX: single;
    TransformScaleY: single;
    constructor Create;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, ARight, ABottom, AZ: integer);
    procedure SetPaddings(ALeft, ATop, ARight, ABottom: integer);
    procedure SetMargins(ALeft, ATop, ARight, ABottom: integer);
    procedure Translate(const DX, DY, DZ: single);
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
    property ViewportWidth: integer read GetViewportWidth;
    property ViewportHeight: integer read GetViewportHeight;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property ClippedWidth: single read GetClippedWidth;
    property ClippedHeight: single read GetClippedHeight;
    property ChildrenCount: integer read GetChildrenCount;
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
    function HitTest(const X, Y: integer; ClipBounds: boolean): TView;
    function GetActiveBranch: TView;
    function SetActiveBranch(AValue: TView): boolean;
    function GetRootView: TView;
    property ActiveBranch: TView read GetActiveBranch;
    property RootView: TView read GetRootView;
  end;

  { TViewLayout }

  TViewLayout = class(TInterfacedObject, IViewLayout)
  protected
    FRootView: TView;
    FActiveBranch: TView;
    FOnChange: TObjectProcedure;
    procedure Flatten;
    function HitTest(const X, Y: integer; ClipBounds: boolean): TView;
    function GetActiveBranch: TView; inline;
    function SetActiveBranch(AValue: TView): boolean; virtual;
    function GetRootView: TView; inline;
    procedure Changed;
    procedure DoOnChange;
  public
    constructor Create(ARootView: TView);
    destructor Destroy; override;
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
  SysUtils, LCLProc, Logging, contnrs;

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
      PreviousView.Next := View;
      View.Previous := PreviousView;
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
  PreviousView.Next := FActiveBranch;
  FActiveBranch.Previous := PreviousView;
end;

function TViewLayout.HitTest(const X, Y: integer; ClipBounds: boolean): TView;
begin
  // Start at last view in ActiveBranch and traverse the list backwards.
  Result := FActiveBranch.Previous;
  repeat
    // Don't take into account views that are not visible to the user.
    if (Result.Width = 0) or (Result.Height = 0) then
      Result := Result.Previous // continue
    else
    if ClipBounds and ((Result.ClippedWidth = 0) or (Result.ClippedHeight = 0)) then
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
  TransformScaleX := 1;
  TransformScaleY := 1;
  Expanded := True;
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

procedure TView.SetBounds(ALeft, ATop, ARight, ABottom, AZ: integer);
begin
  Left := ALeft;
  Top := ATop;
  Bottom := ABottom;
  Right := ARight;
  ZOrder := AZ;
  ZOrderOriginal := AZ;
end;

procedure TView.SetPaddings(ALeft, ATop, ARight, ABottom: integer);
begin
  PaddingLeft := ALeft;
  PaddingTop := ATop;
  PaddingRight := ARight;
  PaddingBottom := ABottom;
end;

procedure TView.SetMargins(ALeft, ATop, ARight, ABottom: integer);
begin
  MarginLeft := ALeft;
  MarginTop := ATop;
  MarginRight := ARight;
  MarginBottom := ABottom;
end;

procedure TView.Translate(const DX, DY, DZ: single);
begin
  Left := Left + DX;
  Top := Top + DY;
  Right := Right + DX;
  Bottom := Bottom + DY;
  ZOrder := ZOrder + DZ;
  ZOrderOriginal := ZOrderOriginal + DZ;
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

  AView.Parent := Self;

  // Add the new child at the end of a circular, double-linked list.
  if not Assigned(FirstChild) then
  begin
    AView.NextSibbling := AView;
    AView.PrevSibbling := AView;
    FirstChild := AView;
  end
  else
  begin
    LastChild := FirstChild.PrevSibbling;
    LastChild.NextSibbling := AView;
    AView.PrevSibbling := LastChild;
    AView.NextSibbling := FirstChild;
    FirstChild.PrevSibbling := AView;
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
  Result := Right - Left;
end;

function TView.GetHeight: single;
begin
  Result := Bottom - Top;
end;

function TView.GetClippedWidth: single;
begin
  Result := ClippedRight - ClippedLeft;
end;

function TView.GetClippedHeight: single;
begin
  Result := ClippedBottom - ClippedTop;
end;

end.
