unit Logger;

{$mode objfpc}{$H+}

interface

procedure LogDebug(const Fmt: string; const Args: array of const);

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

end.
