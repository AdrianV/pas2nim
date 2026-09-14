(* included INSIDE a type section: the definitions land in the section
  around the directive, and the forwarded conditional is hoisted between
  them - both by the parser, because the splice is token-level. *)
{$IF sizeof(Pointer) = 8}
  TFromInclude = Int64;
{$ELSE}
  TFromInclude = LongInt;
{$IFEND}
