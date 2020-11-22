program ImageTest;

{$mode objfpc}{$H+}

uses
  // for multi threading the cthreads unit must be used on unix systems:
  // for example: Linux, MacOSX, FreeBSD, Solaris
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

