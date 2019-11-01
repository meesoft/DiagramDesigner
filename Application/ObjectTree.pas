unit ObjectTree;

interface

uses Windows, ComCtrls, SysUtils, Classes, DiagramBase, GroupObject;

procedure BuildTree(Tree: TTreeView; Layer: TDiagramLayer);

implementation

procedure BuildTree(Tree: TTreeView; Layer: TDiagramLayer);

  procedure AddObjectList(Parent: TTreeNode; List: TBaseObjectList);
  var
    I : Integer;
    Node : TTreeNode;
  begin
    if Assigned(Parent) and (List.Count>4000) then for I:=0 to List.Count-1 do List.Objects[I].TreeNode:=nil
    else
    for I:=List.Count-1 downto 0 do
    begin
      Node:=Tree.Items.AddChildObject(Parent,List.Objects[I].Name,List.Objects[I]);
      List.Objects[I].TreeNode:=Node;
      if List.Objects[I] is TGroupObject then AddObjectList(Node,TGroupObject(List.Objects[I]).Group);
    end;
  end;

begin
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;
    AddObjectList(nil,Layer);
  finally
    Tree.Items.EndUpdate;
  end;
end;

end.

