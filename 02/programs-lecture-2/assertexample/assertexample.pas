program AssertExample;
  { To illustrate the usage of Assert }

var
  r: Real; { input }

begin
  Write ( 'Give me a nonnegative number: ' )
; ReadLn ( r )

; Assert ( 0 <= r, 'This is negative!' )

; WriteLn ( 'Its square root is ', Sqrt(r) : 1 : 2 )
end.
