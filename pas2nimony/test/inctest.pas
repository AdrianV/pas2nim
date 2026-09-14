program IncTest;
(* An `{$I file}` / `{$INCLUDE file}` directive splices another source file
  in at that point, so the included text may hold type definitions,
  constants or whole routines - and the splice has to land in the
  construct the directive sits in, not in a statement list of its own.
  The included file is lexed by its own lexer, so a token that came from
  it still reports the include's file name and line.

  Both spellings appear below (bare and quoted), the include inside a
  TYPE SECTION carries a forwarded conditional, and one include pulls in
  another. *)

type
{$INCLUDE inctest.inc}
  TAfter = Byte;

{$INCLUDE 'inctestnested.inc'}

(* `{$I+}` / `{$I-}` are the I/O-checking SWITCHES, not file includes:
   they share the letter with `{$I file}` and must not be reported as a
   missing include. *)
{$I+}
{$I-}

var
  a: TFromInclude;
  b: TAfter;
  c: Integer;

begin
  a := 11;
  b := 22;
  c := CFromInclude + 1;
  writeln('a=', a, ' b=', b, ' c=', c, ' n=', NNested);
end.
