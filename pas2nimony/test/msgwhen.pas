program MsgWhen;
(* A forwarded conditional may guard nothing but a compiler DIAGNOSTIC: the
  corpus asserts its data model with

    {$if sizeof(ByteAddress) <> sizeof(Pointer)}
      {$Message Error 'wrong size for ByteAddress'}
    {$ifend}

  The {$Message} has no semantics to translate, so the arm body comes out
  empty - and an empty `when` arm is a syntax error for nimony ("nestable
  statement requires indentation"). The arm needs a `discard` in that case,
  here and in the type-section path where the group sits between
  definitions. *)

type
  TBefore = Integer;
{$if sizeof(Pointer) = 4}
  {$Message Error 'this build assumes a 64-bit pointer'}
{$ifend}
  TAfter = Int64;

begin
  writeln('ok');
end.
