program Androli;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads,
  cmem, {$ENDIF}
  Interfaces,
  Forms,
  lazcontrols,
  FormMain,
  View3DTypes,
  ViewLayout3D,
  Animators,
  AndroidDebugBridge,
  ViewServerClient,
  FormOpenWindow,
  TaskRunner,
  DumpFileLoader,
  Logger,
  lazopenglcontext;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

