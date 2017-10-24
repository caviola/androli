unit View3DTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, TaskRunner, Graphics;

type

  TView3D = class;

  { IViewProviderTask }

  IViewProviderTask = interface(ITask)
    ['{66E57A29-772A-4C95-B7AA-FE9926B6E041}']
    // Can be called only once and caller is responsible for freeing the result.
    function GetResult: TView3D;
  end;

  { ICaptureViewTask }

  ICaptureViewTask = interface(ITask)
    ['{61FCD982-73FE-4B2D-A4E9-6C380FD1183A}']
    // Can be called only once and caller is responsible for freeing the result.
    function GetResult: TRasterImage;
    function GetAssociatedView: TView3D;
    property AssociatedView: TView3D read GetAssociatedView;
  end;

  { TCaptureViewTask }

  TCaptureViewTask = class(TTask, ICaptureViewTask)
  protected
    FAssociatedView: TView3D;
    FResult: TRasterImage;
  public
    constructor Create(AView: TView3D);
    destructor Destroy; override;
    function GetResult: TRasterImage;
    function GetAssociatedView: TView3D;
  end;

  { ICaptureViewTaskFactory }

  ICaptureViewTaskFactory = interface
    function CreateTask(View: TView3D): TCaptureViewTask;
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
    FResult: TView3D;
  protected
    procedure SetResult(AValue: TView3D);
  public
    destructor Destroy; override;
    function GetResult: TView3D;
    function GetDisplayName: string; virtual; abstract;
  end;

  TView3DList = specialize TFPGList<TView3D>;

  TViewVisibility = (vvVisible, vvInvisible, vvGone);
  TView3DFlags = set of (vfExpanding, vfCollapsing, vfExpanded);

  { TView3D }

  TView3D = class
  private
    FFlags: TView3DFlags;
    FProperties: TStringList;
    FChildren: TView3DList;
    FTextureName: cardinal;
    FInflightCaptureViewTask: ICaptureViewTask;
    function GetExpanded: boolean;
    function GetSimpleClassName: string;
    function GetChildren(I: integer): TView3D;
    function GetChildrenCount: integer; inline;
    procedure SetExpanded(AValue: boolean);
    procedure SetInflightCaptureViewTask(AValue: ICaptureViewTask);
  public
    Parent: TView3D;
    Next: TView3D;
    Previous: TView3D;
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
    Visibility: TViewVisibility;
    ClippedLeft: single;
    ClippedTop: single;
    ClippedRight: single;
    ClippedBottom: single;
    MatchFilter: boolean;
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
    procedure AddChild(V: TView3D);
    function HasProp(const Name: string): boolean;
    function GetProp(const Name: string): string;
    function GetIntProp(const Name: string; DefaultValue: integer = 0): integer;
    function GetFloatProp(const Name: string; DefaultValue: single = 0): single;
    function GetBoolProp(const Name: string;
      DefaultValue: boolean = False): boolean;
    function GetPropCount: integer;
    procedure GetPropNameValue(I: integer; out Name, Value: string);
    function GetViewportWidth: integer;
    function GetViewportHeight: integer;
    function GetWidth: single; inline;
    function GetHeight: single; inline;
    function GetClippedWidth: single; inline;
    function GetClippedHeight: single; inline;
    property ChildrenCount: integer read GetChildrenCount;
    property Children[I: integer]: TView3D read GetChildren;
    property SimpleClassName: string read GetSimpleClassName;
    property Expanded: boolean read GetExpanded write SetExpanded;
    property TextureName: cardinal read FTextureName write FTextureName;
    property InflightCaptureViewTask: ICaptureViewTask
      read FInflightCaptureViewTask write SetInflightCaptureViewTask;
  end;


function Flatten(RootView: TView3D): TView3D;

implementation

uses
  SysUtils, LazLogger, contnrs;

function Flatten(RootView: TView3D): TView3D;
var
  Q: TQueue;
  PreviousView, View: TView3D;
  ElementsToDepthIncrease: integer = 1;
  NextElementsToDepthIncreate: integer = 0;
  I: integer;
begin
  PreviousView := RootView;
  Q := TQueue.Create;
  try
    Q.Push(RootView);
    while Q.Count > 0 do
    begin
      View := TView3D(Q.Pop);
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

      for I := 0 to View.GetChildrenCount - 1 do
        Q.Push(View.Children[I]);
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

function TLayoutOpenTask.GetResult: TView3D;
begin
  if not Assigned(FResult) then
    raise Exception.CreateFmt(
      '%s.GetResult can be called only once and caller must free the result.',
      [ClassName]);

  Result := FResult;
  FResult := nil;
end;

procedure TLayoutOpenTask.SetResult(AValue: TView3D);
begin
  FResult := AValue;
end;

{ TCaptureViewTask }

constructor TCaptureViewTask.Create(AView: TView3D);
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

function TCaptureViewTask.GetAssociatedView: TView3D;
begin
  Result := FAssociatedView;
end;

{ TView3D }

function TView3D.GetChildren(I: integer): TView3D;
begin
  if Assigned(FChildren) then
    Result := FChildren.Items[I]
  else
    Result := nil;
end;

function TView3D.GetSimpleClassName: string;
var
  P: integer;
begin
  P := LastDelimiter('.', QualifiedClassName);
  if P <> 0 then
    Result := Copy(QualifiedClassName, P + 1)
  else
    Result := QualifiedClassName;
end;

function TView3D.GetExpanded: boolean;
begin
  Result := vfExpanded in FFlags;
end;

function TView3D.GetPropCount: integer; inline;
begin
  Result := FProperties.Count;
end;

procedure TView3D.GetPropNameValue(I: integer; out Name, Value: string);
begin
  FProperties.GetNameValue(I, Name, Value);
end;

function TView3D.GetChildrenCount: integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0;
end;

procedure TView3D.SetExpanded(AValue: boolean);
begin
  if AValue then
    Include(FFlags, vfExpanded)
  else
    Exclude(FFlags, vfExpanded);
end;

procedure TView3D.SetInflightCaptureViewTask(AValue: ICaptureViewTask);
begin
  if Assigned(FInflightCaptureViewTask) then
    FInflightCaptureViewTask.Cancel;
  FInflightCaptureViewTask := AValue;
end;

constructor TView3D.Create;
begin
  Visible := True;
  Visibility := vvVisible;
  FProperties := TStringList.Create;
  FProperties.Duplicates := dupIgnore;
  FProperties.Sorted := True;
  TransformScaleX := 1;
  TransformScaleY := 1;
  Expanded := True;
end;

destructor TView3D.Destroy;
var
  I: integer;
begin
  FProperties.Free;
  for I := 0 to ChildrenCount - 1 do
    Children[I].Free;
  FChildren.Free;
  CaptureViewTaskFactory := nil;

  if Assigned(FInflightCaptureViewTask) then
  begin
    FInflightCaptureViewTask.Cancel;
    FInflightCaptureViewTask := nil;
  end;
  inherited;
end;

procedure TView3D.SetBounds(ALeft, ATop, ARight, ABottom, AZ: integer);
begin
  Left := ALeft;
  Top := ATop;
  Bottom := ABottom;
  Right := ARight;
  ZOrder := AZ;
  ZOrderOriginal := AZ;
end;

procedure TView3D.SetPaddings(ALeft, ATop, ARight, ABottom: integer);
begin
  PaddingLeft := ALeft;
  PaddingTop := ATop;
  PaddingRight := ARight;
  PaddingBottom := ABottom;
end;

procedure TView3D.SetMargins(ALeft, ATop, ARight, ABottom: integer);
begin
  MarginLeft := ALeft;
  MarginTop := ATop;
  MarginRight := ARight;
  MarginBottom := ABottom;
end;

procedure TView3D.Translate(const DX, DY, DZ: single);
begin
  Left := Left + DX;
  Top := Top + DY;
  Right := Right + DX;
  Bottom := Bottom + DY;
  ZOrder := ZOrder + DZ;
  ZOrderOriginal := ZOrderOriginal + DZ;
end;

function TView3D.Contains(const X, Y: integer): boolean;
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

procedure TView3D.SetProperty(const Name, Value: string);
begin
  if Name = 'getVisibility()' then
    if Value = 'INVISIBLE' then
      Visibility := vvInvisible
    else
    if Value = 'GONE' then
      Visibility := vvGone;

  if (Name <> 'mID') or (Value <> 'NO_ID') then
    FProperties.Add(Name + '=' + Value);
end;

procedure TView3D.AddChild(V: TView3D);
begin
  if Assigned(V.Parent) then
    raise EInvalidOperation.CreateFmt('View with HashCode=%s already has a parent.',
      [V.HashCode]);

  if not Assigned(FChildren) then
    FChildren := TView3DList.Create;

  V.Parent := Self;
  FChildren.Add(V);
end;

function TView3D.HasProp(const Name: string): boolean;
begin
  Result := FProperties.IndexOfName(Name) <> -1;
end;

function TView3D.GetProp(const Name: string): string;
begin
  Result := FProperties.Values[Name];
end;

function TView3D.GetIntProp(const Name: string; DefaultValue: integer): integer;
begin
  Result := StrToIntDef(FProperties.Values[Name], DefaultValue);
end;

function TView3D.GetFloatProp(const Name: string; DefaultValue: single): single;
begin
  Result := StrToFloatDef(FProperties.Values[Name], DefaultValue);
end;

function TView3D.GetBoolProp(const Name: string; DefaultValue: boolean): boolean;
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

function TView3D.GetViewportWidth: integer;
begin
  Result := ViewportRect[1].X - ViewportRect[0].X;
end;

function TView3D.GetViewportHeight: integer;
begin
  Result := ViewportRect[2].Y - ViewportRect[1].Y;
end;

function TView3D.GetWidth: single;
begin
  Result := Right - Left;
end;

function TView3D.GetHeight: single;
begin
  Result := Bottom - Top;
end;

function TView3D.GetClippedWidth: single;
begin
  Result := ClippedRight - ClippedLeft;
end;

function TView3D.GetClippedHeight: single;
begin
  Result := ClippedBottom - ClippedTop;
end;

end.
