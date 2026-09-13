program DirLabel;
{ Delphi accepts a symbolic LABEL between a conditional's name and its
  closing brace - ENDIF CLR, ELSE MSWINDOWS, IFEND FPC - and real code
  uses it heavily. The lexer hands the marker over as a token of its
  own, so the label has to be stepped over before the closing brace. }
{$define WANT_FIRST}

{$ifdef WANT_FIRST}
const WHICH = 1;
{$else WANT_SECOND}
const WHICH = 2;
{$endif WANT_FIRST}

{$if sizeof(Pointer) = 8}
const BITS = 64;
{$else}
const BITS = 32;
{$ifend WANT_FIRST}

begin
  writeln('which=', WHICH, ' bits=', BITS);
end.
