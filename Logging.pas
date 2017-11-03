unit Logging;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Log(const Text: string); overload; inline;
procedure Log(const Text: string; const Args: array of const); overload; inline;
procedure LogEnterMethod(const MethodName: string); inline;
procedure LogExitMethod(const MethodName: string); inline;
procedure LogException(const Text: string; Error: Exception); inline;
procedure Log(const AText: string; AValue: pointer); overload; inline;
procedure Log(const AText: string; AValue: TObject); overload; inline;

implementation

{$IFDEF DEBUG}
uses
  MultiLog, FileChannel;

{$ENDIF}

procedure Log(const Text: string);
begin
  {$IFDEF DEBUG}
  MultiLog.Logger.Send(Text);
  {$ENDIF}
end;

procedure Log(const Text: string; const Args: array of const);
begin
  Log(Format(Text, Args));
end;

procedure LogEnterMethod(const MethodName: string);
begin
  {$IFDEF DEBUG}
  MultiLog.Logger.EnterMethod(MethodName);
  {$ENDIF}
end;

procedure LogExitMethod(const MethodName: string);
begin
  {$IFDEF DEBUG}
  MultiLog.Logger.ExitMethod(MethodName);
  {$ENDIF}
end;

procedure LogException(const Text: string; Error: Exception);
begin
  {$IFDEF DEBUG}
  MultiLog.Logger.SendException(Text, Error);
  {$ENDIF}
end;

procedure Log(const AText: string; AValue: pointer);
begin
  Log(AText + '=$' + hexStr(AValue));
end;

procedure Log(const AText: string; AValue: TObject);
begin
  {$IFDEF DEBUG}
  if Assigned(AValue) then
    Log(AText + '=' + AValue.ClassName + '(' + hexStr(AValue) + ')')
  else
    Log(AText + '=nil');
  {$ENDIF}
end;

{$IFDEF DEBUG}
var
  LogChannel: TFileChannel;

initialization
  LogChannel := TFileChannel.Create('androli.log', [fcoShowTime, fcoShowPrefix]);
  MultiLog.Logger.ThreadSafe := True;
  MultiLog.Logger.Channels.Add(LogChannel);
  LogChannel.Clear;

{$ENDIF}

end.
