program ShortStr;
(* `string[N]` is a fixed-length (short) string - the length prefix is part
  of the Pascal value representation and has no nimony model, so pas2nimony
  keeps the UNBOUNDED `string` and documents the divergence.

  The bound must be dropped completely: the type renderer used to append the
  index to the already-decided `string`, producing a constant declaration
  like `CodeTable: string# unhandled type kind: nkIntLit] = 'ABCDEFGH'`. *)

const
  CodeTable: string[8] = 'ABCDEFGH';

var
  s: string[4];

begin
  s := 'abcd';
  writeln(CodeTable, '/', s);
end.
