unit DumpFileLoader;

{$mode objfpc}{$H+}

interface

uses
  ViewTypes;

function CreateDumpFileLoadTask(const FilePath: string): TLayoutLoadTask;

implementation

uses
  SysUtils, LazUTF8, laz2_XMLRead, laz2_DOM, Logging, Math;

type
  { TDeviceMonitorDumpLoadTask }

  TDeviceMonitorDumpLoadTask = class(TLayoutLoadTask)
  private
    FFilePath: string;
  protected
    procedure Run; override;
  public
    constructor Create(const AFilePath: string);
    function GetDisplayName: string; override;
  end;

{ TDeviceMonitorDumpLoadTask }

constructor TDeviceMonitorDumpLoadTask.Create(const AFilePath: string);
begin
  FFilePath := AFilePath;
end;

procedure TDeviceMonitorDumpLoadTask.Run;

  function GetAttribute(Node: TDOMNode; const Name: string): string;
  begin
    Node := Node.Attributes.GetNamedItem(Name);
    if Assigned(Node) then
      Result := Node.TextContent
    else
      Result := EmptyStr;
  end;

  function CreateView(Parent: TView; Node: TDOMNode; Depth: integer = 0): TView;
  var
    Left, Top, Right, Bottom: integer;
    S: string;
    ChildNode: TDOMNode;
  begin
    CheckCanceled;

    SScanf(GetAttribute(Node, 'bounds'), '[%d,%d][%d,%d]',
      [@Left, @Top, @Right, @Bottom]);

    Result := TView.Create;
    try
      Result.QualifiedClassName := GetAttribute(Node, 'class');

      S := GetAttribute(Node, 'text');
      if S <> EmptyStr then
        Result.SetProperty('text:mText', S);

      S := GetAttribute(Node, 'resource-id');
      if S <> EmptyStr then
        Result.SetProperty('mID', S);

      Result.SetBounds(Left, Top, Right, Bottom, Depth);

      if Assigned(Parent) then
      begin
        Result.ClippedLeft := EnsureRange(Left, Parent.ClippedLeft, Parent.ClippedRight);
        Result.ClippedTop := EnsureRange(Top, Parent.ClippedTop, Parent.ClippedBottom);
        Result.ClippedRight :=
          EnsureRange(Right, Parent.ClippedLeft, Parent.ClippedRight);
        Result.ClippedBottom :=
          EnsureRange(Bottom, Parent.ClippedTop, Parent.ClippedBottom);
      end
      else
      begin
        Result.ClippedLeft := Left;
        Result.ClippedTop := Top;
        Result.ClippedRight := Right;
        Result.ClippedBottom := Bottom;
      end;

      for ChildNode in Node do
        if ChildNode.NodeType <> COMMENT_NODE then
          Result.AddChild(CreateView(Result, ChildNode, Depth + 1));
    except
      Result.Free;
      raise;
    end;
  end;

var
  Document: TXMLDocument;
  Node: TDOMNode;
begin
  Log('TDeviceMonitorDumpLoadTask.Run: FileName=''%s''', [FFilePath]);

  ReadXMLFile(Document, FFilePath);
  try
    Node := Document.FirstChild;
    while Assigned(Node) and (Node.NodeType = COMMENT_NODE) do
      Node := Node.NextSibling;

    if not Assigned(Node) or (Node.CompareName('hierarchy') <> 0) then
      raise Exception.Create('Root element is not "hierarchy"');

    Node := Node.FirstChild;
    while Assigned(Node) and (Node.NodeType = COMMENT_NODE) do
      Node := Node.NextSibling;

    if not Assigned(Node) or (Node.CompareName('node') <> 0) then
      raise Exception.Create('Root element child is not "node"');

    // Since we already checked for the presence of at least the root node,
    // we'll always return at least one view.
    SetResult(TViewLayout.Create(CreateView(nil, Node)));
  finally
    Document.Free;
  end;
end;

function TDeviceMonitorDumpLoadTask.GetDisplayName: string;
begin
  Result := FFilePath;
end;

function CreateDumpFileLoadTask(const FilePath: string): TLayoutLoadTask;
begin
  Result := TDeviceMonitorDumpLoadTask.Create(FilePath);
end;

end.
