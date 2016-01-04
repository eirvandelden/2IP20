program queue;
 { Project converted by Frank Staals ( Frank <at> FStaals <dot> Net ) 
          original code by dr.ir. C. Hemerik (Kees) 
          and dr.ir. T. Verhoeff (Tom)
   Date: 12-04-2007 
 }  

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, main;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

