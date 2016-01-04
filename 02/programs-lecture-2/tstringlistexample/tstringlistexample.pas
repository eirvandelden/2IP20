program TStringListExample;
  { illustrates the use of the ADT TStringList }

uses
  Classes;  { has a few ADT definitions }

var
  v : TStringList; { a dynamic list of strings }

begin
  v := TStringList.Create  { create empty list }

; v.LoadFromFile('strings.in')  { read v from text file }

; if v.Count <> 0 then begin  { v is not empty }
    WriteLn(v.Strings[0])  { write first string in v }
  end

; v.Sort  { sort v }

; v.SaveToFile('strings.out')  { write v to text file }

; v.Free  { destroy v and de-allocate memory }
end.

