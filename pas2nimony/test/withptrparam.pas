program WithPtrParam;
(* The pointer-typed base of the corpus's node layout. In its own
  include the array is reached through a PARAMETER
  or LOCAL of pointer type:

    procedure DeleteItem(n: PNode; x: Integer);
    begin
      with n.Items[x] do begin Node := nil; Key := 0; end;
      ...
      n.Items[x].Value := nil;
      with Path[h] do begin ... Nd.Items[x].Node := n ... end;
    end;

  n: PNode (PNode = ^RNode), RNode.Items: PItems
  (PItems = ^TItemArray), TItemArray = array[..] of RItem.
  Pascal auto-derefs both n and Items; nimony does not index a
  ptr array, so the index site needs n.Items[][x], and the with
  body must qualify its bare fields against the ORIGINAL expression.
  Walk adds the second degree: a pointer-typed FIELD reached through
  a with-scope element (Path[h].Nd) whose own Items needs the deref.

  withalias.pas covers with r[i] (a named array alias) and
  withptr.pas covers with r.Items[i] (a record variable). This
  covers the pointer-typed receiver, which is the container shape. *)

type
  PNode = ^RNode;
  RItem = record
    Key: Integer;
    Value: Pointer;
    Node: PNode;
  end;
  TItemArray = array[0..3] of RItem;
  PItems = ^TItemArray;
  RNode = record
    Items: PItems;
    Left: PNode;
    Count: Integer;
  end;
  RPath = record
    Xi: Integer;
    Nd: PNode;
  end;

var
  arr: TItemArray;
  root: RNode;

procedure ClearOne(n: PNode; x: Integer);
begin
  with n.Items[x] do begin
    Key := 0;
    Value := nil;
    Node := nil;
  end;
end;

procedure Bump(n: PNode; x: Integer);
var
  it: RItem;
begin
  it := n.Items[x];
  it.Key := it.Key + 1;
  n.Items[x] := it;
end;

procedure Walk;
var
  Path: array[0..1] of RPath;
  h, x: Integer;
begin
  h := 0;
  x := 1;
  Path[h].Nd := @root;
  with Path[h] do begin
    Xi := x;
    Nd.Items[x].Key := 7;
  end;
end;

begin
  root.Count := 4;
  root.Items := @arr;
  ClearOne(@root, 1);
  root.Items[1].Key := 5;
  Bump(@root, 1);
  writeln('k=', root.Items[1].Key);
  Walk;
  writeln('n=', root.Items[1].Key);
end.
