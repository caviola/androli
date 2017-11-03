program Androli;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads,
  cmem, {$ENDIF}
  Interfaces,
  Forms,
  lazcontrols,
  Logging,
  FormMain,
  View3DTypes,
  ViewLayout3D,
  Animators,
  AndroidDebugBridge,
  FormOpenWindow,
  TaskRunner,
  DumpFileLoader,
  lazopenglcontext,
  DeviceWindowLoader,
  Bookmarks;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

