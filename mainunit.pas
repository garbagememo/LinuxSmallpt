{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Demo to show, how to start a thread and how synchronize with the main
    thread.
    Important: The cthread unint must be added to the uses section of the .lpr
               file. See multithreadingexample1.lpr.
}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uVect;

type

  { TMyThread }

  TLineBuffer=array[0..2048] of VecRecord;
  TTileBuffer=array[0..2048] of TLineBuffer;
  (*TColor=r,g,b*)

  TMyThread = class(TThread)
  private
    fStatusText: string;
    DoneCalc:boolean;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    TileBuffer:TTileBuffer;
    LineBuffer:TLineBuffer;
    w,h:integer;
    yRender:integer;
    procedure yCalc(ry:integer);
    constructor Create(CreateSuspended: boolean);
  end;

  { TMainForm }

  TMainForm = class(TForm)
    cmdRender: TButton;
    StrWidth: TEdit;
    StrHeight: TEdit;
    StrSampleCount: TEdit;
    StrThreadCount: TEdit;
    ImgRender: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    
    procedure cmdRenderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public

 end;

    
var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := TRUE;
  TOP:=10;
  Left:=10;
end;

procedure TMainForm.cmdRenderClick(Sender: TObject);
var
  MyThread : TMyThread;
begin

  imgRender.Width := strtoint(strWidth.Text);
  imgRender.Height := strtoint(strHeight.Text);
//add
  imgRender.Picture.Bitmap.Width:=imgRender.Width;
  imgRender.Picture.Bitmap.Height:=imgRender.Height;
//Orginal source
  imgRender.Canvas.Brush.Color := clBlack;
  imgRender.Canvas.FillRect(0,0, imgRender.Width, imgRender.Height);

  cmdRender.Enabled:=FALSE;
  ClientWidth := imgRender.Left + 5 + imgRender.Width;
  ClientHeight := imgRender.Top + 5 + imgRender.Height;

  MyThread := TMyThread.Create(True); // With the True parameter it doesn't start automatically
  if Assigned(MyThread.FatalException) then
    raise MyThread.FatalException;
      
  // Here the code initialises anything required before the threads starts executing

   MyThread.w:=imgRender.Width;
   MyThread.h:=imgRender.Height;
   MyThread.Start;
end;

{ TMyThread }

procedure TMyThread.yCalc(ry:integer);
var
  x:integer;
  cv:real;
BEGIN
  yRender:=ry;
  for x:=0 to w-1 do begin
    cv:=ry/h*256;
    LineBuffer[x].x:=ry/h*256;
    cv:=x;
    cv:=(cv*256)/w;
    LineBuffer[x].y:=x/w*256;
    LineBuffer[x].z:=128;
   END;
END;

procedure TMyThread.ShowStatus;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example MainForm.Caption.
var
   x,y : integer;
BEGIN
  MainForm.Caption := fStatusText;
  IF DoneCalc=FALSE THEN BEGIN
    TileBuffer[yRender]:=LineBuffer;

    FOR x:=0 to W-1 DO BEGIN
      MainForm.ImgRender.Canvas.Pixels[x,yRender]:=
        Trunc(LineBuffer[x].x)+        //red
        Trunc(LineBuffer[x].y)*256+    //green
        Trunc(LineBuffer[x].z)*256*256; //blue
    END;
  END;
  IF DoneCalc THEN BEGIN
     FOR y:=0 to h do begin
      FOR x:=0 to w do begin
	    MainForm.ImgRender.Canvas.Pixels[x,y]:=
	         Trunc(TileBuffer[y,x].x)+        //red
             Trunc(TileBuffer[y,x].y)*256+    //green
             Trunc(TileBuffer[y,x].z)*256*256;//blune
	  END;
    END;
    MainForm.cmdRender.Enabled:=TRUE;
  END;
END;

procedure TMyThread.Execute;
var
  newStatus : string;
   x,y	    : integer;
begin
  fStatusText := 'TMyThread Starting ...';
  Synchronize(@Showstatus);
  fStatusText := 'TMyThread Running ...';
  Synchronize(@ShowStatus);
  for y:=0 to h-1 do begin
    yCalc(y);
    Synchronize(@ShowStatus);
  END;
  newStatus:='TMyThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
  IF newStatus<>fStatusText then fStatusText:=newStatus;
  DoneCalc:=TRUE;
  Synchronize(@ShowStatus);
end;

constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  DoneCalc:=FALSE;
  yRender:=0;
  inherited Create(CreateSuspended);
end;

end.

