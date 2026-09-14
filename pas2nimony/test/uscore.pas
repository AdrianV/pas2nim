program UScore;
(* nimony's lexer refuses an identifier that leads or ends with `_`, and it
  refuses a `__` run anywhere. Delphi code does use those spellings: the
  corpus record `Dynamic` has a field `__d` and a local `str_` appears in
  the RTL shims.

  The escape marks the first underscore of a run (`pas` in front, `_pas`
  behind) and spells every FURTHER one `U`, so `_d` -> `pas_d` and
  `__d` -> `pas_Ud`. Prefixing alone produced `pas__d`, which nifler
  rejected with "invalid token: trailing underscore". *)

type
  TRec = record
    _a: Integer;
    __b: Integer;
    c_: Integer;
  end;

var
  r: TRec;

begin
  r._a := 1;
  r.__b := 2;
  r.c_ := r._a + r.__b;
  writeln('c=', r.c_);
end.
