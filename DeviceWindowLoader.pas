unit DeviceWindowLoader;

{$mode objfpc}{$H+}

interface

uses
  View3DTypes;

function CreateDeviceWindowOpenTask(
  const DeviceSerial, WindowTitle, WindowHash: string): TLayoutOpenTask;

implementation

uses AndroidDebugBridge;

type

  { TDeviceWindowOpenTask }

  TDeviceWindowOpenTask = class(TLayoutOpenTask)
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

function CreateDeviceWindowOpenTask(
  const DeviceSerial, WindowTitle, WindowHash: string): TLayoutOpenTask;
begin
  Result := TDeviceWindowOpenTask.Create(DeviceSerial, WindowTitle, WindowHash);
end;

{ TDeviceWindowOpenTask }

procedure TDeviceWindowOpenTask.Run;
begin
  SetResult(CreateViewServerClient(FDeviceSerial).DumpWindow(FWindowHash,
    @CheckCanceled));
end;

constructor TDeviceWindowOpenTask.Create(
  const ADeviceSerial, AWindowTitle, AWindowHash: string);
begin
  FDeviceSerial := ADeviceSerial;
  FWindowTitle := AWindowTitle;
  FWindowHash := AWindowHash;
end;

function TDeviceWindowOpenTask.GetDisplayName: string;
begin
  Result := FDeviceSerial + ':' + FWindowTitle;
end;

end.

