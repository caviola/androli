unit DumpFileLoader;

{$mode objfpc}{$H+}

interface

uses
  View3DTypes;

function CreateDumpFileOpenTask(const FilePath: string): TLayoutOpenTask;

implementation

uses
  SysUtils, LazUTF8, laz2_XMLRead, laz2_DOM, Logging;

type
  { TDeviceMonitorDumpOpenTask }

  TDeviceMonitorDumpOpenTask = class(TLayoutOpenTask)
  private
    FFilePath: string;
  protected
    procedure Run; override;
  public
    constructor Create(const AFilePath: string);
    function GetDisplayName: string; override;
  end;

{ TDeviceMonitorDumpLoadTask }

constructor TDeviceMonitorDumpOpenTask.Create(const AFilePath: string);
begin
  FFilePath := AFilePath;
end;

procedure TDeviceMonitorDumpOpenTask.Run;

  function GetAttribute(Node: TDOMNode; const Name: string): string;
  begin
    Node := Node.Attributes.GetNamedItem(Name);
    if Assigned(Node) then
      Result := Node.TextContent
    else
      Result := EmptyStr;
  end;

  function CreateView(Node: TDOMNode; Depth: integer = 0): TView3D;
  var
    Left, Top, Right, Bottom: integer;
    S: string;
    ChildNode: TDOMNode;
  begin
    CheckCanceled;

    SScanf(GetAttribute(Node, 'bounds'), '[%d,%d][%d,%d]',
      [@Left, @Top, @Right, @Bottom]);

    Result := TView3D.Create;
    try
      Result.QualifiedClassName := GetAttribute(Node, 'class');

      S := GetAttribute(Node, 'text');
      if S <> EmptyStr then
        Result.SetProperty('text:mText', S);

      S := GetAttribute(Node, 'resource-id');
      if S <> EmptyStr then
        Result.SetProperty('mID', S);

      Result.SetBounds(Left, Top, Right, Bottom, Depth);
      Result.ClippedLeft := Left;
      Result.ClippedTop := Top;
      Result.ClippedRight := Right;
      Result.ClippedBottom := Bottom;

      for ChildNode in Node do
        if ChildNode.NodeType <> COMMENT_NODE then
          Result.AddChild(CreateView(ChildNode, Depth + 1));
    except
      Result.Free;
      raise;
    end;
  end;

var
  Document: TXMLDocument;
  Node: TDOMNode;
begin
  Log('TDeviceMonitorDumpOpenTask.Run: FileName=''%s''', [FFilePath]);

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
    SetResult(Flatten(CreateView(Node)));
  finally
    Document.Free;
  end;
end;

function TDeviceMonitorDumpOpenTask.GetDisplayName: string;
begin
  Result := FFilePath;
end;

function CreateDumpFileOpenTask(const FilePath: string): TLayoutOpenTask;
begin
  Result := TDeviceMonitorDumpOpenTask.Create(FilePath);
end;

end.
