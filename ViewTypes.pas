unit ViewTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, TaskRunner, Graphics;

type

  TView = class;

  { IViewProviderTask }

  IViewProviderTask = interface(ITask)
    ['{66E57A29-772A-4C95-B7AA-FE9926B6E041}']
    // Can be called only once and caller is responsible for freeing the result.
    function GetResult: TView;
  end;

  { ICaptureViewTask }

  ICaptureViewTask = interface(ITask)
    ['{61FCD982-73FE-4B2D-A4E9-6C380FD1183A}']
    // Can be called only once and caller is responsible for freeing the result.
    function GetResult: TRasterImage;
    function GetAssociatedView: TView;
    property AssociatedView: TView read GetAssociatedView;
  end;

  { TCaptureViewTask }

  TCaptureViewTask = class(TTask, ICaptureViewTask)
  protected
    FAssociatedView: TView;
    FResult: TRasterImage;
  public
    constructor Create(AView: TView);
    destructor Destroy; override;
    function GetResult: TRasterImage;
    function GetAssociatedView: TView;
  end;

  { ICaptureViewTaskFactory }

  ICaptureViewTaskFactory = interface
    function CreateTask(View: TView): TCaptureViewTask;
  end;

  { ILayoutOpenTask }

  ILayoutOpenTask = interface(IViewProviderTask)
    ['{F1CC2749-77DC-4AC4-A183-19ECDB4E3924}']
    function GetDisplayName: string;
    property DisplayName: string read GetDisplayName;
  end;

  { TLayoutOpenTask }

  TLayoutOpenTask = class(TTask, ILayoutOpenTask)
  private
    FResult: TView;
  protected
    procedure SetResult(AValue: TView);
  public
    destructor Destroy; override;
    function GetResult: TView;
    function GetDisplayName: string; virtual; abstract;
  end;

  TViewFlags = set of (
    vfExpanding,
    vfCollapsing,
    vfExpanded,
    vfUserHidden,
    vfVisibilityInvisible,
    vfVisibilityGone,
    vfMatchFilter);

  { TView }

  TView = class
  private
    FFlags: TViewFlags;
    FProperties: TStringList;
    FTextureName: cardinal;
    FInflightCaptureViewTask: ICaptureViewTask;
    function GetExpanded: boolean; inline;
    function GetMatchFilter: boolean; inline;
    function GetSimpleClassName: string;
    function GetChildrenCount: integer; inline;
    function GetVisibilityGone: boolean; inline;
    procedure SetExpanded(AValue: boolean); inline;
    procedure SetInflightCaptureViewTask(AValue: ICaptureViewTask);
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
    Visible: boolean;
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
    CaptureViewTaskFactory: ICaptureViewTaskFactory;
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
    property InflightCaptureViewTask: ICaptureViewTask
      read FInflightCaptureViewTask write SetInflightCaptureViewTask;
    property VisibilityGone: boolean read GetVisibilityGone;
    property MatchFilter: boolean read GetMatchFilter write SetMatchFilter;
  end;


function Flatten(RootView: TView): TView;

implementation

uses
  SysUtils, LazLogger, contnrs;

function Flatten(RootView: TView): TView;
var
  Q: TQueue;
  PreviousView, View, ViewChild: TView;
  ElementsToDepthIncrease: integer = 1;
  NextElementsToDepthIncreate: integer = 0;
begin
  PreviousView := RootView;
  Q := TQueue.Create;
  try
    Q.Push(RootView);
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
  PreviousView.Next := RootView;
  RootView.Previous := PreviousView;

  Result := RootView;
end;

{ TLayoutOpenTask }

destructor TLayoutOpenTask.Destroy;
begin
  if Assigned(FResult) then
    FResult.Free;
  inherited;
end;

function TLayoutOpenTask.GetResult: TView;
begin
  if not Assigned(FResult) then
    raise Exception.CreateFmt(
      '%s.GetResult can be called only once and caller must free the result.',
      [ClassName]);

  Result := FResult;
  FResult := nil;
end;

procedure TLayoutOpenTask.SetResult(AValue: TView);
begin
  FResult := AValue;
end;

{ TCaptureViewTask }

constructor TCaptureViewTask.Create(AView: TView);
begin
  FAssociatedView := AView;
  FAssociatedView.InflightCaptureViewTask := Self;
end;

destructor TCaptureViewTask.Destroy;
begin
  if Assigned(FResult) then
    FResult.Free;
  inherited;
end;

function TCaptureViewTask.GetResult: TRasterImage;
begin
  if not Assigned(FResult) then
    raise Exception.CreateFmt(
      '%s.GetResult can be called only once and caller must free it.', [ClassName]);

  Result := FResult;
  FResult := nil;
end;

function TCaptureViewTask.GetAssociatedView: TView;
begin
  Result := FAssociatedView;
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

procedure TView.SetInflightCaptureViewTask(AValue: ICaptureViewTask);
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
  Visible := True;
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

  CaptureViewTaskFactory := nil;

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
