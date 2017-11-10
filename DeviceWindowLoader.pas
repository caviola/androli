unit DeviceWindowLoader;

{$mode objfpc}{$H+}

interface

uses
  ViewTypes;

function CreateDeviceWindowLoadTask(const DeviceSerial, WindowTitle,
  WindowHash: string): TLayoutLoadTask;

implementation

uses AndroidDebugBridge, Logging, Classes, Graphics, TaskRunner, GLext, gl, LCLProc;

type

  { TDeviceWindowLayout }

  TDeviceWindowLayout = class(TViewLayout)
  private
    FDeviceSerial: string;
    FWindowHash: string;
  protected
    procedure CaptureViewResult(const Task: ITask; Image: TRasterImage; View: TView);
    procedure CaptureViews(AView: TView);
  public
    constructor Create(const ADeviceSerial, AWindowHash: string; ARootView: TView);
    destructor Destroy; override;
  end;

  { TDeviceWindowLoadTask }

  TDeviceWindowLoadTask = class(TLayoutLoadTask)
  private
    FDeviceSerial: string;
    FWindowTitle: string;
    FWindowHash: string;
  protected
    procedure Run; override;
  public
    constructor Create(const ADeviceSerial, AWindowTitle, AWindowHash: string);
    function GetDisplayName: string; override;
  end;


function CreateDeviceWindowLoadTask(const DeviceSerial, WindowTitle,
  WindowHash: string): TLayoutLoadTask;
begin
  Result := TDeviceWindowLoadTask.Create(DeviceSerial, WindowTitle, WindowHash);
end;

{ TDeviceWindowLayout }

procedure TDeviceWindowLayout.CaptureViewResult(const Task: ITask;
  Image: TRasterImage; View: TView);
var
  TextureName: GLint = 0;
begin
  Log('TDeviceWindowLayout.CaptureViewResult: Task=%s, View=%s@%s, Image=%dx%d',
    [DbgS(Pointer(Task)), View.QualifiedClassName, View.HashCode,
    Image.Width, Image.Height]);

  try
    // Don't bother with images too small.
    if (Image.Width < MinCaptureViewWidth) or (Image.Height < MinCaptureViewHeight) then
      Exit;

    // Delete current view texture, if any.
    glDeleteTextures(1, @View.TextureName);

    // Create new OpenGL texture from image data.
    // Note we create the texture for a view only once.
    // If for some reason (eg. out-of-memory) the texture creation fails,
    // the view won't have an associated texture and we won't display its image.
    TextureName := 0;
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

  Changed;
end;

constructor TDeviceWindowLayout.Create(const ADeviceSerial, AWindowHash: string;
  ARootView: TView);
begin
  inherited Create(ARootView);
  FDeviceSerial := ADeviceSerial;
  FWindowHash := AWindowHash;
  CaptureViews(FRootView);
end;

destructor TDeviceWindowLayout.Destroy;

  procedure DeleteTexture(AView: TView);
  var
    Child: TView;
  begin
    glDeleteTextures(1, @AView.TextureName);
    Child := AView.FirstChild;
    if Assigned(Child) then
      repeat
        DeleteTexture(Child);
        Child := Child.NextSibbling;
      until Child = AView.FirstChild;
  end;

begin
  DeleteTexture(FRootView);
  inherited;
end;

procedure TDeviceWindowLayout.CaptureViews(AView: TView);
var
  Child: TView;
begin
  // Capture leaf views recursively.
  // We don't capture container views because ViewServer doesn't support
  // capturing them without children and for our displaying needs
  // we need each view's content separately.
  Child := AView.FirstChild;
  if Assigned(Child) then
    repeat
      CaptureViews(Child);
      Child := Child.NextSibbling;
    until Child = AView.FirstChild
  else if (AView.Width >= MinCaptureViewWidth) and
    (AView.Height >= MinCaptureViewHeight) then
    with TViewServerCaptureViewTask.Create(FDeviceSerial, FWindowHash, AView) do
    begin
      OnResult := @CaptureViewResult;
      Start;
    end
  else
    Log('TDeviceWindowLayout.CaptureViews: skip view %s@%s too small %dx%d',
      [AView.QualifiedClassName, AView.HashCode, TruncToInt(AView.Width),
      TruncToInt(AView.Height)]);
end;

{ TDeviceWindowOpenTask }

procedure TDeviceWindowLoadTask.Run;
begin
  Log('TDeviceWindowLoadTask.Run: Device=''%s'', WindowTitle=''%s'', WindowHash=''%s''',
    [FDeviceSerial, FWindowTitle, FWindowHash]);

  SetResult(TDeviceWindowLayout.Create(FDeviceSerial, FWindowHash,
    CreateViewServerClient(FDeviceSerial).DumpWindow(FWindowHash, @CheckCanceled)));
end;

constructor TDeviceWindowLoadTask.Create(
  const ADeviceSerial, AWindowTitle, AWindowHash: string);
begin
  FDeviceSerial := ADeviceSerial;
  FWindowTitle := AWindowTitle;
  FWindowHash := AWindowHash;
end;

function TDeviceWindowLoadTask.GetDisplayName: string;
begin
  Result := FDeviceSerial + ':' + FWindowTitle;
end;

end.
