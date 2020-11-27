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

}
unit uRenderNE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uVect,uModel;

type

  { TMyThread }

  TLineBuffer=array[0..1980] of VecRecord;
  TTileBuffer=array[0..1980] of TLineBuffer;
  (*TColor=r,g,b*)

  TMyThread = CLASS(TThread)
  private
    fStatusText: string;
    DoneCalc:boolean;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    LineBuffer:TLineBuffer;
    wide,h,samps:integer;
    ThreadNum:integer;
    yOffset:integer;
    sph:TList;
    yRender:integer;
    startTime : Int64;
    function Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
    function Radiance(r:RayRecord):VecRecord;
    constructor Create(CreateSuspended: boolean);
  end;

  { TMainForm }

  TMainForm = class(TForm)
    cmdSave: TButton;
    cmdRender: TButton;
    Label5: TLabel;
    Label6: TLabel;
    SaveDlg: TSaveDialog;
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
    procedure cmdSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveDlgClick(Sender: TObject);
  private
  public
    ThreadNum:integer;
    samps:integer;
    StartTime:Integer;
    ThreadList:TList;
    TileBuffer:TTileBuffer;
    function isAllDone:boolean;
  end;

    
var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


FUNCTION SecToTime(Sec : INTEGER) : STRING;
VAR
  H, M, S : STRING;
  ZH, ZM, ZS : INTEGER;
BEGIN
  ZH := Sec DIV 3600;
  ZM := Sec DIV 60 - ZH * 60;
  ZS := Sec - (ZH * 3600 + ZM * 60);
  H := IntToStr(ZH);
  IF (H = '0') THEN
    H := '00';
  M := IntToStr(ZM);
  IF (M = '0') THEN
    M := '00';
  S := IntToStr(ZS);
  IF (S = '0') THEN
    S := '00';
  Result := H + ':' + M + ':' + S;
END;

function TMainForm.isAllDone:boolean;
var
  i:integer;
begin
  isAllDone:=TRUE;
  for i:=0 to ThreadNum-1 do begin
     IF TMyThread(ThreadList[i]).DoneCalc=FALSE THEN BEGIN
       isAllDone:=FALSE;
       exit;
     end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := TRUE;
  TOP:=10;
  Left:=10;
  InitScene;
end;

procedure TMainForm.SaveDlgClick(Sender: TObject);
begin
  IF (SaveDlg.Execute) THEN
    imgRender.Picture.SaveToFile(SaveDlg.FileName);
end;

procedure TMainForm.cmdRenderClick(Sender: TObject);
var
  MyThread,MyThread2 : TMyThread;
  i:integer;
begin
  Randomize;
  imgRender.Width := strtoint(strWidth.Text);
  imgRender.Height := strtoint(strHeight.Text);
  ThreadNum:=StrToInt(StrThreadCount.Text);
  samps:=StrToInt(StrSampleCount.Text);
//add
  imgRender.Picture.Bitmap.Width:=imgRender.Width;
  imgRender.Picture.Bitmap.Height:=imgRender.Height;
//Orginal source
  imgRender.Canvas.Brush.Color := clBlack;
  imgRender.Canvas.FillRect(0,0, imgRender.Width, imgRender.Height);

  cmdRender.Enabled:=FALSE;
  ClientWidth := imgRender.Left + 5 + imgRender.Width;
  IF (ImgRender.Top+5+ImgRender.Height) >306 THEN
    ClientHeight := imgRender.Top + 5 + imgRender.Height;
  ThreadList:=TList.Create;
  FOR i:=0 TO ThreadNum-1 DO BEGIN
    MyThread := TMyThread.Create(True); // With the True parameter it doesn't start automatically
    if Assigned(MyThread.FatalException) then
      raise MyThread.FatalException;
    MyThread.wide:=imgRender.Width;
    MyThread.h:=imgRender.Height;
    MyThread.samps:=StrToInt(StrSampleCount.text);
    MyThread.ThreadNum:=ThreadNum;
    MyThread.yOffset:=i;
    MyThread.DoneCalc:=FALSE;
    ThreadList.Add(MyThread);
  end;
  StartTime:=GetTickCount64;
 
  FOR i:=0 TO ThreadNum-1 DO BEGIN
    TMyThread(ThreadList[i]).Start;
  end;
end;

procedure TMainForm.cmdSaveClick(Sender: TObject);
begin
  IF SaveDlg.Execute THEN ImgRender.Picture.SaveToFile(SaveDlg.Filename);
end;

{ TMyThread }


procedure TMyThread.ShowStatus;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example MainForm.Caption.
var
   x,y : integer;
   newStatus : string;
BEGIN
  MainForm.Caption := fStatusText;
  IF DoneCalc=FALSE THEN BEGIN
    MainForm.TileBuffer[yRender]:=LineBuffer;
    FOR x:=0 to Wide-1 DO BEGIN
      MainForm.ImgRender.Canvas.Pixels[x,yRender]:=
          ColToByte(LineBuffer[x].x)+        //red
          ColToByte(LineBuffer[x].y)*256+    //green
          ColToByte(LineBuffer[x].z)*256*256; //blue
    END;
    MainForm.Label6.Caption:=SecToTime((GetTickCount64 - MainForm.startTime) DIV 1000);
  END;
  IF MainForm.isAllDone THEN BEGIN
 (*    FOR y:=0 to h do begin
      FOR x:=0 to wide do begin
	    MainForm.ImgRender.Canvas.Pixels[x,y]:=
	       ColToByte(MainForm.TileBuffer[y,x].x)+        //red
             ColToByte(MainForm.TileBuffer[y,x].y)*256+    //green
             ColToByte(MainForm.TileBuffer[y,x].z)*256*256;//blune
	  END;
    END;
 *)   MainForm.cmdRender.Enabled:=TRUE;
    MainForm.Caption:='TMyThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
  END;
END;
procedure TMyThread.Execute;
var
  x,y,sx,sy,s:integer;
  temp,d       : VecRecord;
  r1,r2,dx,dy  : real;
  cam,tempRay  : RayRecord;
  cx,cy: VecRecord;
  tColor,r,camPosition,camDirection : VecRecord;
begin
  fStatusText := 'TMyThread Starting ...';
  Synchronize(@Showstatus);

  camPosition:=CreateVec(50, 52, 295.6);
  camDirection:=CreateVec(0, -0.042612, -1);
  camDirection:=VecNorm( camDirection);
  cam:=CreateRay(camPosition, camDirection);
  cx:=CreateVec(wide * 0.5135 / h, 0, 0);
  cy:= cx/ cam.d;
  cy:=VecNorm(cy);
  cy:= cy* 0.5135;
  y:=yOffset;
  fStatusText := 'TMyThread Running ...';
  Synchronize(@ShowStatus);
  while y<h do begin
    for x:=0 to wide-1 do begin
      for sy:=0 to 1 do begin
        r:=CreateVec(0, 0, 0);
        tColor:=ZeroVec;
        for sx:=0 to 1 do begin
          for s:=0 to samps-1 do begin
            r1 := 2 * random;
            IF (r1 < 1) THEN
              dx := sqrt(r1) - 1
            ELSE
              dx := 1 - sqrt(2 - r1);

            r2 := 2 * random;
            IF (r2 < 1) THEN
              dy := sqrt(r2) - 1
            ELSE
              dy := 1 - sqrt(2 - r2);

            temp:= cx* (((sx + 0.5 + dx) / 2 + x) / wide - 0.5);
            d:= cy* (((sy + 0.5 + dy) / 2 + (h - y - 1)) / h - 0.5);
            d:= d +temp;
            d:= d +cam.d;

            d:=VecNorm(d);
            tempRay.o:= d* 140;
            tempRay.o:= tempRay.o+ cam.o;
            tempRay.d := d;
            temp:=Radiance(tempRay);
            temp:= temp/ samps;
            r:= r+temp;
          end;(*samps*)
          temp:= r* 0.24;
          tColor:=tColor+ temp;
          r:=CreateVec(0, 0, 0);
        end;(*sx*)
      end;(*sy*)
      LineBuffer[x]:=tColor;
     end;(*x*)
     yRender:=y;
     Synchronize(@ShowStatus);
     y:=y+ThreadNum;
  END;(*y*)
  DoneCalc:=TRUE;
  Synchronize(@ShowStatus);
 end;


constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  DoneCalc:=FALSE;
  yRender:=0;
  InitScene;
  sph:=TList.Create;
  sph:=CopyScene(0);
  {
  sph.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  sph.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  sph.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  sph.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170),  ZeroVec,CreateVec(0,0,0),      DIFF) );//Front
  sph.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  sph.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  sph.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999, SPEC) );//Mirror
  sph.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999, REFR) );//Glass
  sph.add( SphereClass.Create(600, CreateVec(50,681.6-0.27,81.6),CreateVec(12,12,12),    ZeroVec,DIFF) );//Ligth
  }
  inherited Create(CreateSuspended);
end;



function TMyThread.Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
var
  n,d:real;
  i:integer;
begin
  t:=INF;
  for i:=0 to sph.count-1 do begin
    d:=SphereClass(sph[i]).intersect(r);
    if d<t THEN BEGIN
      t:=d;
      id:=i;
    END;
  end;
  result:=(t<inf);
END;


function TMyThread.Radiance(r:RayRecord):VecRecord;
var
  id,i,tid:integer;
  obj,s:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf,bcf:VecRecord;
  E,depth:integer;
BEGIN
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      exit;
    END;
    obj:=SphereClass(sph[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    IF n*r.d<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)and(f.x>f.z) THEN
      p:=f.x
    ELSE IF f.y>f.z THEN
      p:=f.y
    ELSE
      p:=f.z;
    tw:=obj.e*E;
    cl:=cl+VecMul(cf,tw);
    IF (depth>5) THEN BEGIN
      IF random<p THEN
        f:=f/p
      ELSE BEGIN
        result:=cl;
        exit;
      END;
    END;
    bcf:=cf;cf:=VecMul(cf,f);
    CASE obj.refl OF
      DIFF:BEGIN
 //       x:=x+nl*eps;(*ad hoc 突き抜け防止*)
        r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
        w:=nl;
        IF abs(w.x)>0.1 THEN
          u:=VecNorm(CreateVec(0,1,0)/w)
        ELSE BEGIN
          u:=VecNorm(CreateVec(1,0,0)/w );
        END;
        v:=w/u;
        tu:=u*(cos(r1)*r2s);tu:=tu+v*(sin(r1)*r2s);tu:=tu+w*sqrt(1-r2);
        d := VecNorm(tu);

      // Loop over any lights
        EL:=ZeroVec;
        tid:=id;
        for i:=0 to sph.count-1 do BEGIN
          s:=SphereClass(sph[i]);
          IF (i=tid) THEN BEGIN
            continue;
          END;
          IF (s.e.x<=0) and  (s.e.y<=0) and (s.e.z<=0)  THEN continue; // skip non-lights
          sw:=s.p-x;
          tr:=sw*sw;  tr:=s.rad2/tr;
          IF abs(sw.x)/sqrt(tr)>1e-8 THEN 
            su:=VecNorm(CreateVec(0,1,0)/sw) 
          ELSE 
            su:=VecNorm(CreateVec(1,0,0)/sw);
          sv:=sw/su;
          IF tr>1 THEN BEGIN
            (*半球の内外=cos_aがマイナスとsin_aが＋、－で場合分け*)
            (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)
            eps1:=2*pi*random;eps2:=random;eps2s:=sqrt(eps2);
            tu:=u*(cos(eps1)*eps2s);tu:=tu+v*(sin(eps1)*eps2s);tu:=tu+w*sqrt(1-eps2);
            l:=VecNorm(tu);
            IF intersect(CreateRay(x,l),t,id) THEN BEGIN
              IF id=i THEN BEGIN
                tr:=l*nl;
                tw:=s.e*tr;
                EL:=EL+VecMul(f,tw);
              END;
            END;
          END
          ELSE BEGIN //半球外部の場合;
            cos_a_max := sqrt(1-tr );
            eps1 := random; eps2:=random;
            cos_a := 1-eps1+eps1*cos_a_max;
            sin_a := sqrt(1-cos_a*cos_a);
            IF (1-2*random)<0 THEN sin_a:=-sin_a; 
            phi := 2*PI*eps2;
            tw:=sw*(cos(phi)*sin_a);tw:=tw+sv*(sin(phi)*sin_a);tw:=tw+sw*cos_a;
            l:=VecNorm(tw);
            IF (intersect(CreateRay(x,l), t, id) ) THEN BEGIN 
              IF id=i THEN BEGIN  // shadow ray
                omega := 2*PI*(1-cos_a_max);
                tr:=l*nl;
                IF tr<0 THEN tr:=0;
                tw:=s.e*tr*omega;tw:=VecMul(f,tw);tw:=tw*M_1_PI;
                EL := EL + tw;  // 1/pi for brdf
              END;
            END;
          END;
        END;(*for*)
        tw:=obj.e*e+EL;
        cl:= cl+VecMul(bcf,tw );
        E:=0;
        r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tw:=obj.e*e;
        cl:=cl+VecMul(bcf,tw);
        E:=1;tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      END;(*SPEC*)
      REFR:BEGIN
        tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        IF cos2t<0 THEN BEGIN   // Total internal reflection
          cl:=cl+VecMul(bcf,obj.e*E);
          E:=1;
          r:=RefRay;
          continue;
        END;
        IF into THEN q:=1 ELSE q:=-1;
        tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
        IF into THEN Q:=-ddn ELSE Q:=tdir*n;
        a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
        Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
        IF random<p THEN BEGIN// 反射
          cf:=cf*RP;
          cl:=cl+VecMul(bcf,obj.e*E);
          E:=1;
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(bcf,obj.e*E);
          E:=1;
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;




end.

