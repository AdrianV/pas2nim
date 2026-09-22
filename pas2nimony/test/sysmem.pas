program SysMem;
(* The Delphi memory primitives. systempas had no `AllocMem` /
  `FreeMem` / `FreeMemory` / `ReallocMem` / `ReallocMemory` at all, so
  every corpus unit that allocates raw nodes died
  with "undeclared identifier: 'AllocMem'"; `Move` and `FillChar` were
  missing too.

  They map onto nimony's `alloc0`/`realloc`/`dealloc`/`moveMem`;
  `Move`/`FillChar` are untyped in Pascal, so the shim takes the value
  (not an address) with `var` on the destination - a dereferenced
  pointer or an array element binds directly.

  KNOWN GAP, deliberately not exercised here: Pascal assigns `Pointer`
  to a TYPED pointer implicitly (`p: PInts; p := AllocMem(n)`), nimony
  does not ("got: pointer but wanted: ptr array[..]"). That coercion is
  the next blocker for the corpus's allocation sites. *)

uses SysUtils;

type
  TInts = array[0..3] of Integer;

var
  raw: Pointer;
  a, b: TInts;
  i: Integer;

begin
  raw := AllocMem(SizeOf(TInts));
  for i := 0 to 3 do
    a[i] := i + 1;
  Move(a, b, SizeOf(TInts));
  FillChar(b, SizeOf(TInts), 0);
  raw := ReallocMemory(raw, SizeOf(TInts) * 2);
  FreeMem(raw, SizeOf(TInts) * 2);
  FreeMemory(raw);
  writeln('a0=', a[0], ' a3=', a[3], ' b0=', b[0], ' b3=', b[3]);
end.
