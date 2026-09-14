program WithPtr;
(* `with X.Field[i] do` where the field is a POINTER to an array of
  records (`Items: PItems`, `PItems = ^TItemArray`,
  `TItemArray = array[..] of TItem`). This is exactly the node layout
  of pascal4neko's tplbtree.inc, whose `with n.Items[x] do` emitted
  the body's bare field names and failed with "undeclared identifier:
  Node/Value/Key".

  The element spelling of such a field has to follow one pointer level
  and then the array alias. The lowering captures the pointer and the
  index in temps (`var w = r.Items; var i = 1`) and qualifies
  `w[][i].field`: the base is evaluated once and writes go through.
  (nimony rejects `addr` of an explicit-deref operand such as
  `addr(r.Items[][1])`, so the pointer is captured instead.) *)

type
  PRec = ^TRec;
  TItem = record
    Key: Integer;
    Node: PRec;
  end;
  TItemArray = array[0..3] of TItem;
  PItems = ^TItemArray;
  TRec = record
    Items: PItems;
  end;

var
  arr: TItemArray;
  r: TRec;

begin
  r.Items := @arr;
  with r.Items[1] do
  begin
    Key := 5;
    Node := nil;
  end;
  with r.Items[2] do
    Key := 7;
  writeln('k1=', r.Items[1].Key, ' k2=', r.Items[2].Key);
end.
