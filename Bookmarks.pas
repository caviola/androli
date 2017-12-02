unit Bookmarks;

{$mode objfpc}{$H+}

interface

uses
  contnrs;

const
  ibmNone = -1; // no active indexed bookmark

type

  TSaveBookmarkEvent = function: TObject of object;
  TRestoreBookmarkEvent = procedure(Which: TObject) of object;

  { TBookmarkManager }

  TBookmarkManager = class
  private
    FOnSaveBookmark: TSaveBookmarkEvent;
    FOnRestoreBookmark: TRestoreBookmarkEvent;
  public
    constructor Create(AOnSaveBookmark: TSaveBookmarkEvent;
      AOnRestoreBookmark: TRestoreBookmarkEvent);
    function DoSaveBookmark: TObject;
    procedure DoRestoreBookmark(Which: TObject);
  end;

  { IBookmarkListener }

  IBookmarkListener = interface
    ['{88F1ED7A-BFFA-4FDC-A310-4271B6086C1E}']
    function SaveBookmark: TObject;
    procedure RestoreBookmark(Which: TObject);
  end;

  { IIndexedBookmarkListener }

  IIndexedBookmarkListener = interface(IBookmarkListener)
    ['{B25B4CDB-C46A-4083-8144-6A76AE1BDC0B}']
    procedure OnIndexedBookmarkSet(Index, SetCount, Total: integer);
    procedure OnIndexedBookmarkUnset(Index, SetCount, Total: integer);
  end;

  { TIndexedBookmarkManager }

  TIndexedBookmarkManager = class
  private
    FSetBookmarkCount: integer;
    FCurrentBookmarkIndex: integer;
    FListener: IIndexedBookmarkListener;
    FBookmarks: array of TObject;
  protected
    procedure CheckBounds(I: integer);
    procedure DoOnBookmarkSet(I: integer);
    procedure DoOnBookmarkUnset(I: integer);
    function DoSaveBookmark: TObject;
    procedure DoRestoreBookmark(I: integer);
  public
    constructor Create(Capacity: integer; const AListener: IIndexedBookmarkListener);
    destructor Destroy; override;
    procedure Clear;
    function SetFree: integer;
    procedure Toggle(I: integer);
    procedure Go(I: integer);
    function GoNext: integer;
    function GoPrevious: integer;
  end;

  { IBookmarkHistoryListener }

  IBookmarkHistoryListener = interface
    ['{65774055-338B-4A5E-A3CB-BFA402F241BB}']
    procedure OnBookmarkHistoryChanged(CurrentIndex, Total: integer);
  end;

  { TBookmarkHistoryManager }

  TBookmarkHistoryManager = class(TBookmarkManager)
  private
    FBookmarks: TFPObjectList;
    FCurrentBookmarkIndex: integer;
    FListener: IBookmarkHistoryListener;
  protected
    procedure DoOnBookmarkHistoryChanged;
  public
    constructor Create(AOnSaveBookmark: TSaveBookmarkEvent;
      AOnRestoreBookmark: TRestoreBookmarkEvent; AListener: IBookmarkHistoryListener);
    destructor Destroy; override;
    procedure Clear;
    procedure Add;
    procedure GoBack;
    procedure GoForward;
  end;


implementation

uses
  SysUtils;

{ TBookmarkManager }

constructor TBookmarkManager.Create(AOnSaveBookmark: TSaveBookmarkEvent;
  AOnRestoreBookmark: TRestoreBookmarkEvent);
begin
  Assert(Assigned(AOnSaveBookmark), 'AOnSaveBookmark must be assigned');
  Assert(Assigned(AOnRestoreBookmark), 'AOnRestoreBookmark must be assigned');
  FOnSaveBookmark := AOnSaveBookmark;
  FOnRestoreBookmark := AOnRestoreBookmark;
end;

function TBookmarkManager.DoSaveBookmark: TObject;
begin
  Result := FOnSaveBookmark();
end;

procedure TBookmarkManager.DoRestoreBookmark(Which: TObject);
begin
  FOnRestoreBookmark(Which);
end;

{ TBookmarkHistoryManager }

procedure TBookmarkHistoryManager.DoOnBookmarkHistoryChanged;
begin
  FListener.OnBookmarkHistoryChanged(FCurrentBookmarkIndex, FBookmarks.Count);
end;

constructor TBookmarkHistoryManager.Create(AOnSaveBookmark: TSaveBookmarkEvent;
  AOnRestoreBookmark: TRestoreBookmarkEvent; AListener: IBookmarkHistoryListener);
begin
  inherited Create(AOnSaveBookmark, AOnRestoreBookmark);
  Assert(Assigned(AListener), 'AListener must be assigned');
  FListener := AListener;
  FBookmarks := TFPObjectList.Create(True);
  FCurrentBookmarkIndex := -1;
end;

destructor TBookmarkHistoryManager.Destroy;
begin
  FBookmarks.Free;
  inherited;
end;

procedure TBookmarkHistoryManager.Clear;
begin
  FBookmarks.Clear;
  FCurrentBookmarkIndex := -1;
  DoOnBookmarkHistoryChanged;
end;

procedure TBookmarkHistoryManager.Add;
var
  B: TObject;
  I: integer;
begin
  B := DoSaveBookmark;
  try
    if FCurrentBookmarkIndex > -1 then
      // Delete all bookmarks above current.
      for I := FBookmarks.Count - 1 downto FCurrentBookmarkIndex + 1 do
        FBookmarks.Delete(I);

    FBookmarks.Add(B);
    Inc(FCurrentBookmarkIndex);
    DoOnBookmarkHistoryChanged;
  except
    B.Free;
    raise;
  end;
end;

procedure TBookmarkHistoryManager.GoBack;
begin
  if FCurrentBookmarkIndex > 0 then
  begin
    Dec(FCurrentBookmarkIndex);
    DoRestoreBookmark(FBookmarks[FCurrentBookmarkIndex]);
    DoOnBookmarkHistoryChanged;
  end;
end;

procedure TBookmarkHistoryManager.GoForward;
begin
  if FCurrentBookmarkIndex < FBookmarks.Count - 1 then
  begin
    Inc(FCurrentBookmarkIndex);
    DoRestoreBookmark(FBookmarks[FCurrentBookmarkIndex]);
    DoOnBookmarkHistoryChanged;
  end;
end;

{ TIndexedBookmarkManager }

procedure TIndexedBookmarkManager.CheckBounds(I: integer);
begin
  if (I < 0) or (I > Pred(Length(FBookmarks))) then
    raise Exception.Create('Index out of bounds');
end;

procedure TIndexedBookmarkManager.DoOnBookmarkSet(I: integer);
begin
  Inc(FSetBookmarkCount);
  FListener.OnIndexedBookmarkSet(I, FSetBookmarkCount, Length(FBookmarks));
end;

procedure TIndexedBookmarkManager.DoOnBookmarkUnset(I: integer);
begin
  Dec(FSetBookmarkCount);
  FListener.OnIndexedBookmarkUnset(I, FSetBookmarkCount, Length(FBookmarks));
end;

function TIndexedBookmarkManager.DoSaveBookmark: TObject;
begin
  Result := FListener.SaveBookmark;
end;

procedure TIndexedBookmarkManager.DoRestoreBookmark(I: integer);
begin
  FListener.RestoreBookmark(FBookmarks[I]);
  FCurrentBookmarkIndex := I;
end;

constructor TIndexedBookmarkManager.Create(Capacity: integer;
  const AListener: IIndexedBookmarkListener);
begin
  Assert(Assigned(AListener), 'AListener must be assigned');
  FListener := AListener;
  FCurrentBookmarkIndex := ibmNone;
  SetLength(FBookmarks, Capacity);
  FSetBookmarkCount := 0;
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

  FCurrentBookmarkIndex := ibmNone;
end;

function TIndexedBookmarkManager.SetFree: integer;
var
  I: integer;
begin
  Result := ibmNone;
  for I := 0 to Length(FBookmarks) - 1 do
    if not Assigned(FBookmarks[I]) then
    begin
      FBookmarks[I] := DoSaveBookmark;
      DoOnBookmarkSet(I);
      Result := I;
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
    DoRestoreBookmark(I);
end;

function TIndexedBookmarkManager.GoNext: integer;
var
  Next, Last: integer;
begin
  Next := FCurrentBookmarkIndex + 1;
  if FCurrentBookmarkIndex = ibmNone then
    Last := 0
  else
    Last := Next;

  repeat
    if Next = Length(FBookmarks) then
      Next := 0;

    if Assigned(FBookmarks[Next]) then
    begin
      DoRestoreBookmark(Next);
      Break;
    end;

    Inc(Next);
  until Next = Last;

  Result := FCurrentBookmarkIndex;
end;

function TIndexedBookmarkManager.GoPrevious: integer;
var
  Previous, Last: integer;
begin
  Previous := FCurrentBookmarkIndex - 1;
  if FCurrentBookmarkIndex = ibmNone then
    Last := Length(FBookmarks) - 1
  else
    Last := Previous;

  repeat
    if Previous < 0 then
      Previous := Length(FBookmarks) - 1;

    if Assigned(FBookmarks[Previous]) then
    begin
      DoRestoreBookmark(Previous);
      Break;
    end;

    Dec(Previous);
  until Previous = Last;

  Result := FCurrentBookmarkIndex;
end;

end.
