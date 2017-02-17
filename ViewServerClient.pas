unit ViewServerClient;

{$mode objfpc}{$H+}

interface

uses
  View3DTypes, blcksock;

const
  ViewServerPort = 4939;

type

  TWindowManagerEntry = record
    HashCode: string;
    Title: string;
  end;

  TWindowManagerEntryArray = array of TWindowManagerEntry;

  { TViewServerClient }

  TViewServerClient = class
  protected
    FSocket: TTCPBlockSocket;
    procedure Connect;
    procedure Disconnect;
  public
    constructor Create(Socket: TTCPBlockSocket);
    destructor Destroy; override;
    function GetProtocolVersion: integer;
    function GetServerVersion: integer;
    function GetWindowList: TWindowManagerEntryArray;
    function DumpWindow(const HashCode: string; var Canceled: boolean): TView3D;
    //TODO: function DoCaptureLayers;
  end;


function ViewServerGetWindowList(const Socket: TTCPBlockSocket;
  Timeout: integer = -1): TWindowManagerEntryArray;
function ViewServerDumpWindow(const Socket: TTCPBlockSocket;
  const HashCode: string; var Canceled: boolean): TView3D;

implementation

uses
  SysUtils, Math;

const
  DONEp = 'DONE.';
  DONE = 'DONE';
  UndefinedMargin = -2147483648;

function ViewServerGetWindowList(const Socket: TTCPBlockSocket;
  Timeout: integer): TWindowManagerEntryArray;
var
  Line: string;
  Count: integer = 0;
  P: integer;
begin
  Socket.SendString('LIST'#10);
  repeat
    Line := Socket.RecvTerminated(Timeout, #10);
    if (Line = DONEp) or (Line = DONE) then
      Break;

    Inc(Count);
    SetLength(Result, Count);

    P := Pos(#32, Line);
    Result[Count - 1].HashCode := Copy(Line, 1, P - 1);
    Result[Count - 1].Title := Copy(Line, P + 1);
  until False;
end;

function ViewServerDumpWindow(const Socket: TTCPBlockSocket;
  const HashCode: string; var Canceled: boolean): TView3D;

  procedure ParseDumpLine(Line: PChar; View: TView3D);
  var
    Pos: PChar;
    PName, PValue: string;
    PLen: integer;
  begin
    // TODO: error checking
    // Note that we don't do any error checking here.
    // For the time being, we assume all lines are formatted as expected.

    Pos := StrScan(Line, '@');
    View.QualifiedClassName := Copy(Line, 1, Pos - Line);  // substring up to '@'
    Line := Pos + 1; // skip '@'

    Pos := StrScan(Line, #32);
    View.HashCode := Copy(Line, 1, Pos - Line); // substring up to whitespace

    Line := Pos;
    while Line[0] = #32 do
    begin
      Inc(Line); // skip whitespace
      if Line[0] = #0 then
        Break;  // reached end-of-line

      Pos := StrScan(Line, '=');
      PName := Copy(Line, 1, Pos - Line); // substring up to '='
      Line := Pos + 1; // skip '='

      Pos := StrScan(Line, ',');
      PLen := StrToInt(Copy(Line, 1, Pos - Line));  // substring up to ','
      Line := Pos + 1; // skip ','

      PValue := Copy(Line, 1, PLen);  // next PLen characters
      Inc(Line, PLen);

      View.SetProperty(PName, PValue);
    end;
  end;

  function CreateView(AParent: TView3D; Line: PChar; Depth: integer): TView3D;
  var
    OX, OY, ScaleX, ScaleY: single;
  begin
    Result := TView3D.Create;
    try
      ParseDumpLine(Line, Result);

      with Result do
      begin
        // These bounds are relative to parent.
        SetBounds(
          GetIntProp('layout:mLeft'),
          GetIntProp('layout:mTop'),
          GetIntProp('layout:mRight'),
          GetIntProp('layout:mBottom'),
          Depth);

        Translate(
          GetFloatProp('drawing:getTranslationX()'),
          GetFloatProp('drawing:getTranslationY()'),
          0);

        SetPaddings(
          GetIntProp('padding:mPaddingLeft'),
          GetIntProp('padding:mPaddingTop'),
          GetIntProp('padding:mPaddingRight'),
          GetIntProp('padding:mPaddingBottom'));

        // The constant -2147483648 is used by GridLayout and means UNDEFINED.
        // For our rendering purposes we consider that to mean 0.
        // See https://developer.android.com/reference/android/support/v7/widget/GridLayout.html
        MarginLeft := GetIntProp('layout:layout_leftMargin');
        if MarginLeft = UndefinedMargin then
          MarginLeft := 0;

        MarginTop := GetIntProp('layout:layout_topMargin');
        if MarginTop = UndefinedMargin then
          MarginTop := 0;

        MarginRight := GetIntProp('layout:layout_rightMargin');
        if MarginRight = UndefinedMargin then
          MarginRight := 0;

        MarginBottom := GetIntProp('layout:layout_bottomMargin');
        if MarginBottom = UndefinedMargin then
          MarginBottom := 0;

        // This is the scale explicitly defined in this View.
        ScaleX := GetFloatProp('drawing:getScaleX()', 1);
        ScaleY := GetFloatProp('drawing:getScaleY()', 1);
        if Assigned(AParent) then
        begin
          // This is the View's final scale, which also takes into account
          // the scale of its parents.
          TransformScaleX := AParent.TransformScaleX * ScaleX;
          TransformScaleY := AParent.TransformScaleY * ScaleY;
        end
        else
        begin
          TransformScaleX := ScaleX;
          TransformScaleY := ScaleY;
        end;

        if (ScaleX <> 1) or (ScaleY <> 1) then // View defines explicit scale?
        begin
          // Scale around explicit pivot point.
          OX := Left + GetFloatProp('drawing:getPivotX()');
          OY := Top + GetFloatProp('drawing:getPivotY()');
          Left := (Left - OX) * TransformScaleX + OX;
          Top := (Top - OY) * TransformScaleY + OY;
          Right := (Right - OX) * TransformScaleX + OX;
          Bottom := (Bottom - OY) * TransformScaleY + OY;
        end
        else
        begin
          // Scale around origin (0,0).
          Left := Left * TransformScaleX;
          Top := Top * TransformScaleY;
          Right := Right * TransformScaleX;
          Bottom := Bottom * TransformScaleY;
        end;

        PaddingLeft := PaddingLeft * TransformScaleX;
        PaddingTop := PaddingTop * TransformScaleY;
        PaddingRight := PaddingRight * TransformScaleX;
        PaddingBottom := PaddingBottom * TransformScaleY;

        MarginLeft := MarginLeft * TransformScaleX;
        MarginTop := MarginTop * TransformScaleY;
        MarginRight := MarginRight * TransformScaleX;
        MarginBottom := MarginBottom * TransformScaleY;

        if Assigned(AParent) then
        begin
          // Make bounds absolute.
          Translate(AParent.Left, AParent.Top, 0);

          ClippedLeft := EnsureRange(Left, AParent.ClippedLeft, AParent.ClippedRight);
          ClippedTop := EnsureRange(Top, AParent.ClippedTop, AParent.ClippedBottom);
          ClippedRight := EnsureRange(Right, AParent.ClippedLeft, AParent.ClippedRight);
          ClippedBottom := EnsureRange(Bottom, AParent.ClippedTop,
            AParent.ClippedBottom);
        end
        else
        begin
          ClippedLeft := Left;
          ClippedTop := Top;
          ClippedRight := Right;
          ClippedBottom := Bottom;
        end;
      end;

      if Assigned(AParent) then
        AParent.AddChild(Result);
    except
      Result.Free;
      raise;
    end;
  end;

var
  CurrentView: TView3D = nil;
  CurrentDepth: integer = -1;
  Depth: integer;
  Line: PChar;
begin
  Socket.SendString('DUMP ' + HashCode + #10);
  try
    repeat
      Line := PChar(Socket.RecvTerminated(-1, #10));
      if (Line = DONEp) or (Line = DONE) then
        Break;

      if Canceled then
        raise Exception.CreateFmt('Dump window %s explicitly canceled.', [HashCode]);

      Depth := 0;
      while Line[0] = #32 do
      begin
        Inc(Line);
        Inc(Depth);
      end;

      while Depth <= CurrentDepth do
      begin
        if Assigned(CurrentView) then
          CurrentView := CurrentView.Parent;
        Dec(CurrentDepth);
      end;

      CurrentView := CreateView(CurrentView, Line, Depth);
      CurrentDepth := Depth;
    until False;
  except
    CurrentView.Free;
    raise;
  end;

  // Rewind to root view.
  if Assigned(CurrentView) then
  begin
    while Assigned(CurrentView.Parent) do
      CurrentView := CurrentView.Parent;
    CurrentView := Flatten(CurrentView);
  end;

  Result := CurrentView;
end;

{ TViewServerClient }

procedure TViewServerClient.Connect;
begin
  FSocket.Connect(cLocalhost, IntToStr(ViewServerPort));
end;

procedure TViewServerClient.Disconnect;
begin
  FSocket.CloseSocket;
end;

constructor TViewServerClient.Create(Socket: TTCPBlockSocket);
begin
  FSocket := Socket;
end;

destructor TViewServerClient.Destroy;
begin
  inherited Destroy;
end;

function TViewServerClient.GetProtocolVersion: integer;
begin
  Connect;
  try
    FSocket.SendString('PROTOCOL'#10);
    Result := StrToInt(FSocket.RecvTerminated(-1, #10));
  finally
    Disconnect;
  end;
end;

function TViewServerClient.GetServerVersion: integer;
begin
  Connect;
  try
    FSocket.SendString('SERVER'#10);
    Result := StrToInt(FSocket.RecvTerminated(-1, #10));
  finally
    Disconnect;
  end;
end;

function TViewServerClient.GetWindowList: TWindowManagerEntryArray;
begin
  Connect;
  try
    Result := ViewServerGetWindowList(FSocket);
  finally
    Disconnect;
  end;
end;

function TViewServerClient.DumpWindow(const HashCode: string;
  var Canceled: boolean): TView3D;
begin
  Result := ViewServerDumpWindow(FSocket, HashCode, Canceled);
end;

end.
