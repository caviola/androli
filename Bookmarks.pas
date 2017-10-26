unit Bookmarks;

{$mode objfpc}{$H+}

interface

type

  { IBookmarkListener }

  IBookmarkListener = interface
    ['{88F1ED7A-BFFA-4FDC-A310-4271B6086C1E}']
    function SaveBookmark: TObject;
    procedure RestoreBookmark(Which: TObject);
  end;

  { IIndexedBookmarkListener }

  IIndexedBookmarkListener = interface(IBookmarkListener)
    ['{B25B4CDB-C46A-4083-8144-6A76AE1BDC0B}']
    procedure OnBookmarkSet(I: integer);
    procedure OnBookmarkUnset(I: integer);
  end;

  { TIndexedBookmarkManager }

  TIndexedBookmarkManager = class
  private
    FLastBookmarkIndex: integer;
    FListener: IIndexedBookmarkListener;
    FBookmarks: array of TObject;
  protected
    procedure CheckBounds(I: integer);
    procedure DoOnBookmarkSet(I: integer);
    procedure DoOnBookmarkUnset(I: integer);
    function DoSaveBookmark: TObject;
    procedure DoRestoreBookmark(Which: TObject);
  public
    constructor Create(Capacity: integer; const AListener: IIndexedBookmarkListener);
    destructor Destroy; override;
    procedure Clear;
    procedure SetFree;
    procedure Toggle(I: integer);
    procedure Go(I: integer);
    procedure GoNext;
    procedure GoPrevious;
  end;


implementation

uses
  SysUtils;


{ TIndexedBookmarkManager }

procedure TIndexedBookmarkManager.CheckBounds(I: integer);
begin
  if (I < 0) or (I > Pred(Length(FBookmarks))) then
    raise Exception.Create('Index out of bounds');
end;

procedure TIndexedBookmarkManager.DoOnBookmarkSet(I: integer);
begin
  FListener.OnBookmarkSet(I);
end;

procedure TIndexedBookmarkManager.DoOnBookmarkUnset(I: integer);
begin
  FListener.OnBookmarkUnset(I);
end;

function TIndexedBookmarkManager.DoSaveBookmark: TObject;
begin
  Result := FListener.SaveBookmark;
end;

procedure TIndexedBookmarkManager.DoRestoreBookmark(Which: TObject);
begin
  FListener.RestoreBookmark(Which);
end;

constructor TIndexedBookmarkManager.Create(Capacity: integer;
  const AListener: IIndexedBookmarkListener);
begin
  FLastBookmarkIndex := -1;
  SetLength(FBookmarks, Capacity);
  FListener := AListener;
end;

destructor TIndexedBookmarkManager.Destroy;
var
  I: integer;
begin
  for I := 0 to Length(FBookmarks) - 1 do
    if Assigned(FBookmarks[I]) then
      FBookmarks[I].Free;

  inherited;
end;

procedure TIndexedBookmarkManager.Clear;
var
  I: integer;
begin
  for I := 0 to Length(FBookmarks) - 1 do
    if Assigned(FBookmarks[I]) then
    begin
      FreeAndNil(FBookmarks[I]);
      DoOnBookmarkUnset(I);
    end;
end;

procedure TIndexedBookmarkManager.SetFree;
var
  I: integer;
begin
  for I := 0 to Length(FBookmarks) - 1 do
    if not Assigned(FBookmarks[I]) then
    begin
      FBookmarks[I] := DoSaveBookmark;
      DoOnBookmarkSet(I);
      Break;
    end;
end;

procedure TIndexedBookmarkManager.Toggle(I: integer);
begin
  CheckBounds(I);
  if Assigned(FBookmarks[I]) then
  begin
    FreeAndNil(FBookmarks[I]);
    DoOnBookmarkUnset(I);
  end
  else
  begin
    FBookmarks[I] := DoSaveBookmark;
    DoOnBookmarkSet(I);
  end;
end;

procedure TIndexedBookmarkManager.Go(I: integer);
begin
  CheckBounds(I);
  if Assigned(FBookmarks[I]) then
    DoRestoreBookmark(FBookmarks[I]);
end;

procedure TIndexedBookmarkManager.GoNext;
begin

end;

procedure TIndexedBookmarkManager.GoPrevious;
begin

end;

end.

