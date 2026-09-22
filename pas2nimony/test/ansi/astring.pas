program teststring;

procedure main;
var
    s, s1: string;
begin
    WriteLn('Hallo Welt');
    s:= 'foo bar';
    s1:= s;
    s[1]:= 'g';
    writeln('s: ', s, ' s1: ', s1);
end;

begin
    main;

end.