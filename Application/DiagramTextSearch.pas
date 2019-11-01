unit DiagramTextSearch;

interface

uses
  DiagramBase;

type
  TDiagramTextSearch = class
    private
      PageIndex, LayerIndex, ObjectIndex : Integer;
      Diagram : TDiagramContainer;
      FFindText : string;
      FCaseSensitive: Boolean;
    public
      property FindText: string read FFindText;
      property CaseSensitive: Boolean read FCaseSensitive;
      constructor Create(Diagram: TDiagramContainer; const FindText: string; CaseSensitive: Boolean);
      function FindNext: Boolean;
    end;

implementation

uses
  Main, ShapeObject, GroupObject, SysUtils, TextObject;

constructor TDiagramTextSearch.Create(Diagram: TDiagramContainer; const FindText: string; CaseSensitive: Boolean);
begin
  inherited Create;
  Self.Diagram:=Diagram;
  FFindText:=FindText;
  FCaseSensitive:=CaseSensitive;
  LayerIndex:=-1;
end;

function TDiagramTextSearch.FindNext: Boolean;

  function CheckObject(Obj: TTextObject): Boolean; overload;
  var
    Text : string;
  begin
    Text:=CleaupText(PString(Obj.Properties[opText])^,True);
    if CaseSensitive then Result:=Pos(FindText,Text)>0
    else Result:=Pos(AnsiUpperCase(FindText),AnsiUpperCase(Text))>0;
  end;

  function CheckObject(Obj: TGroupObject): Boolean; overload;
  var
    I : Integer;
  begin
    Result:=False;
    for I:=0 to Obj.Group.Count-1 do
      if ((Obj.Group.Objects[I] is TTextObject) and CheckObject(TTextObject(Obj.Group.Objects[I]))) and
         ((Obj.Group.Objects[I] is TGroupObject) and CheckObject(TGroupObject(Obj.Group.Objects[I]))) then
      begin
        Result:=True;
        Exit;
      end;
  end;

var
  Obj : TBaseObject;
begin
  Result:=False;

  if LayerIndex=-1 then // Search global stencil
  begin
    while ObjectIndex<Diagram.Stencil.Count do
    begin
      Obj:=Diagram.Stencil.Objects[ObjectIndex];
      if Obj is TTextObject then Result:=CheckObject(TTextObject(Obj))
      else if Obj is TGroupObject then Result:=CheckObject(TGroupObject(Obj));
      Inc(ObjectIndex);
      if Result then
      begin
        MainForm.ActiveLayerIndex:=LayerIndex;
        MainForm.ActiveObject:=Obj;
        Exit;
      end;
    end;
    ObjectIndex:=0;
    Inc(LayerIndex);
  end;

  while PageIndex<Diagram.Count do
  begin
    while LayerIndex<Diagram.Pages[PageIndex].Count do
    begin
      while ObjectIndex<Diagram.Pages[PageIndex].Layers[LayerIndex].Count do
      begin
        Obj:=Diagram.Pages[PageIndex].Layers[LayerIndex].Objects[ObjectIndex];
        if Obj is TTextObject then Result:=CheckObject(TTextObject(Obj))
        else if Obj is TGroupObject then Result:=CheckObject(TGroupObject(Obj));
        Inc(ObjectIndex);
        if Result then
        begin
          MainForm.ActivePageIndex:=PageIndex;
          MainForm.ActiveLayerIndex:=LayerIndex;
          MainForm.ActiveObject:=Obj;
          Exit;
        end;
      end;
      ObjectIndex:=0;
      Inc(LayerIndex);
    end;
    LayerIndex:=0;
    Inc(PageIndex);
  end;
  PageIndex:=0;
end;

end.
