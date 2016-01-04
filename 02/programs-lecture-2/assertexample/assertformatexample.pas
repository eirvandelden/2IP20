program AssertFormatExample; { with Format }

uses
  SysUtils; { for Format }

var
  r: Real; { input }

begin
  Write ( 'Give me a nonnegative number: ' )
; ReadLn ( r )
; Assert ( 0 <= r, Format ( '%1.2F is negative!', [ r ] ) )
; WriteLn ( 'Its square root is ', Sqrt(r) : 1 : 2 )
end.
