unit DumpFileLoader;

{$mode objfpc}{$H+}

interface

uses
  ViewTypes;

function CreateDumpFileLoadTask(const FilePath: string): TLayoutLoadTask;

implementation

uses
  SysUtils, laz2_XMLRead, laz2_DOM, Logging;

type
  { TDeviceMonitorDumpLoadTask }

  TDeviceMonitorDumpLoadTask = class(TLayoutLoadTask)
  private
    FFilePath: string;
  protected
    procedure Run; override;
  public
    constructor Create(const AFilePath: string);
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

  function CreateView(Node: TDOMNode; Depth: integer = 0): TView;
  var
    Left, Top, Right, Bottom: integer;
    S: string;
    ChildNode: TDOMNode;
  begin
    CheckCanceled;

    // These bounds are provided right how we need them, that is,
    // window absolute.
    SScanf(GetAttribute(Node, 'bounds'), '[%d,%d][%d,%d]',
      [@Left, @Top, @Right, @Bottom]);

    Result := TView.Create(Left, Top, Right, Bottom, Depth);
    try
      Result.QualifiedClassName := GetAttribute(Node, 'class');

      S := GetAttribute(Node, 'text');
      if S <> EmptyStr then
        Result.SetProperty('text:mText', S);

      S := GetAttribute(Node, 'resource-id');
      if S <> EmptyStr then
        Result.SetProperty('mID', S);

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
    SetResult(TViewLayout.Create(CreateView(Node), FFilePath));
  finally
    Document.Free;
  end;
end;

function CreateDumpFileLoadTask(const FilePath: string): TLayoutLoadTask;
begin
  Result := TDeviceMonitorDumpLoadTask.Create(FilePath);
end;

end.
