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
  ViewTypes,
  LayoutViewer,
  Animators,
  AndroidDebugBridge,
  FormOpenViewServerWindow,
  TaskRunner,
  DumpFileLoader,
  lazopenglcontext,
  ViewServerLoader,
  Bookmarks;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

