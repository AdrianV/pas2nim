program AddrParam;
(* `addr` is an ordinary identifier in Pascal - synsock declares
  `TGetHostByAddr = proc (addr: pointer; len: Integer; ...)` and its
  routines take a parameter of that name. In nimony `addr` is a keyword
  and cannot appear in ANY name position:

    synsock.nim(491, 27) Error: 'addr' is a keyword and cannot be used
    as a parameter name

  A keyword spelling is escaped with the `pas` prefix (`addr` ->
  `pasaddr`), in a routine's parameter list and in a procedural type. *)

type
  TAddrProc = procedure(addr: Pointer; len: Integer);

procedure Show(addr: Pointer; len: Integer);
begin
  writeln('len=', len);
end;

begin
  Show(nil, 3);
end.
