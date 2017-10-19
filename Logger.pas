unit Logger;

{$mode objfpc}{$H+}

interface

procedure LogDebug(const Fmt: string; const Args: array of const);
procedure LogDebug(const Msg: string);

implementation

{$IFDEF DEBUG}
uses
  SysUtils, LCLProc;

{$ENDIF}

procedure LogDebug(const Fmt: string; const Args: array of const);
begin
  {$IFDEF DEBUG}
  DebugLnThreadLog(Format(Fmt, Args));
  {$ENDIF}
end;

procedure LogDebug(const Msg: string);
begin
  {$IFDEF DEBUG}
  DebugLnThreadLog(Msg);
  {$ENDIF}
end;

end.
