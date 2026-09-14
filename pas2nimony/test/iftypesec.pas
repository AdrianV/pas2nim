program IfTypeSec;
(* A conditional directive inside a TYPE SECTION may wrap definitions, and
  one group may sit inside another group's arm. pas2nimony forwards each
  group as a Nim `when` hoisted out of the `type` block nimony would
  reject; the arm bodies are type sections in their own right, so the
  hoisting has to RECURSE. A flat pass drops the inner group's
  declarations silently.

  The shape comes from real code (p4nHelper.pas), where an IntPtr type is
  guarded by a "not declared" test around a pointer-size test. The
  definitions before and after the group stay unconditional. *)

type
  TBefore = Integer;
{$IF not defined(IntPtr)}
{$IF sizeof(Pointer) = 8}
  IntPtr = Int64;
{$ELSE}
  IntPtr = LongInt;
{$IFEND}
{$IFEND}
  TAfter = Byte;

var
  a: TBefore;
  p: IntPtr;
  b: TAfter;

begin
  a := 3;
  p := 4;
  b := 5;
  writeln('a=', a, ' p=', p, ' b=', b);
end.
