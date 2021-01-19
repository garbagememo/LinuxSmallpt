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
unit MainRender;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType,
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,LMessages,
  SysUtils, uVect,uModel,math;

CONST
  MSG_NEWLINE	     = WM_USER + 0;
  MSG_DecThreadCount = WM_USER+1;
  MSG_DrawNextScene  = WM_USER+2;
  DefaultSaveFile    = 'out.png';
  AutoDir	     = 'auto';
   PathSeparator     = '/';(*IF Windows THEN \*)

type
  { TRenderThread }

  TLineBuffer=array[0..1980] of VecRecord;
  (*TColor=r,g,b*)

  TRenderThread = CLASS(TThread)
  private
    fStatusText: string;
    DoneCalc:boolean;
    procedure InitRend;
    procedure DoRend;
    procedure DoneRend;
  protected
    procedure Execute; override;
  public
    LineBuffer:TLineBuffer;
    wide,h,samps:integer;
    CamR:CameraRecord;
    ModelList:TList;
    yRender:integer;
    AutoFlag:boolean;
    AutoIndex:integer;
    function Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
    function Radiance(r:RayRecord;depth:integer):VecRecord;virtual;
    constructor Create(CreateSuspended: boolean);
  end;
  TNERenderThread = CLASS(TRenderThread)
    function Radiance(r:RayRecord;depth:integer):VecRecord;OverRide;
  END;
  TNRenderThread= CLASS(TRenderThread)
    function Radiance(r:RayRecord;depth:integer):VecRecord;OverRide;
  END;

  { TMainForm }

  TMainForm = class(TForm)
    cmdAuto: TButton;
    cmdSave: TButton;
    cmdRender: TButton;
    AlgolCombo: TComboBox;
    Aloglthm: TLabel;
    Model: TLabel;
    SceneCombo: TComboBox;
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
    Label5: TLabel;
    lblTime: TLabel;

    procedure AlgolComboChange(Sender: TObject);
    procedure cmdAutoClick(Sender: TObject);
    procedure cmdRenderClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure SceneComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveDlgClick(Sender: TObject);
  private
    ModelIndex:Integer;
    AlgolIndex:integer;
    MinimamHeight:integer;
  public
    ThreadNum:integer;
    samps:integer;
    StartTime:Int64;
    ThreadList:TList;
    yAxis:integer;
    AutoFlag:boolean;
    procedure RenderSetup;
    function isAllDone:boolean;
    function GetYAxis:integer;
    PROCEDURE MSGDrawNextScene(VAR Message:TLMessage);MESSAGE MSG_DrawNextScene;
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
     IF TRenderThread(ThreadList[i]).DoneCalc=FALSE THEN BEGIN
        isAllDone:=FALSE;
       exit;
     end;
  end;
end;

function TMainForm.GetYAxis:integer;
begin
   yAxis:=yAxis+1;
   result:=yAxis;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i:integer;
begin
    DoubleBuffered := TRUE;
    TOP:=10;
    Left:=10;
    InitScene;
    for i:=0 to sc.count-1 do
    SceneCombo.Items.Add(ScName[i]);
    SceneCombo.ItemIndex:=1;
    ModelIndex:=1;
    AlgolCombo.Items.Add('Original');
    AlgolCombo.Items.Add('Next Event');
    AlgolCombo.Items.Add('Non Loop');
    AlgolCombo.ItemIndex:=1;
    AlgolIndex:=1;
    MinimamHeight:=Height;
    AutoFlag:=FALSE;
    Randomize;
end;

procedure TMainForm.SaveDlgClick(Sender: TObject);
begin
  IF (SaveDlg.Execute) THEN
    imgRender.Picture.SaveToFile(SaveDlg.FileName);
end;

procedure TMainForm.RenderSetup;
var
  RenderThread: TRenderThread;
  i:integer;
begin
   IF Assigned(ThreadList) THEN BEGIN
      ThreadList.Destroy;
   END;

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
   IF (ImgRender.Top+5+ImgRender.Height) >MinimamHeight THEN
      ClientHeight := imgRender.Top + 5 + imgRender.Height;
   ThreadList:=TList.Create;
   yAxis:=-1;
   FOR i:=0 TO ThreadNum-1 DO BEGIN
      IF AlgolIndex=1 then
	 RenderThread:=TNERenderThread.Create(TRUE);
      IF AlgolIndex=0 THEN
	 RenderThread:=TRenderThread.Create(True);
      IF AlgolIndex=2 THEN
	 RenderThread:=TNRenderThread.Create(True);
      // True parameter it doesnt start automatically
      if Assigned(RenderThread.FatalException) then
	 raise RenderThread.FatalException;
      RenderThread.wide:=imgRender.Width;
      RenderThread.h:=imgRender.Height;
      RenderThread.samps:=StrToInt(StrSampleCount.text);
      RenderThread.yRender:=GetYAxis;
      RenderThread.DoneCalc:=FALSE;
      ThreadList.Add(RenderThread);
   end;
   StartTime:=GetTickCount64; 
end;
procedure TMainForm.cmdRenderClick(Sender: TObject);
var
  RenderThread: TRenderThread;
  i:integer;
begin
   RenderSetup;
   FOR i:=0 TO ThreadList.Count-1 DO BEGIN
     TRenderThread(ThreadList[i]).AutoFlag:=false;
     TRenderThread(ThreadList[i]).Start;
   END;
end;
PROCEDURE TMainForm.MSGDrawNextScene(VAR Message : TLMessage);
var
  SceneRec:SceneRecord;
  RenderThread: TRenderThread;
  i:integer;
BEGIN
   IF SR.GetNextScene(StrToInt(StrWidth.Text),StrToInt(StrHeight.Text) ) THEN BEGIN
      RenderSetup;
      FOR i:=0 TO ThreadList.Count-1 DO BEGIN
        TRenderThread(ThreadList[i]).AutoFlag:=TRUE;
	TRenderThread(ThreadList[i]).Start;
      END;
   END
  ELSE BEGIN
    AutoFlag:=FALSE;
    cmdRender.Enabled:=TRUE;
  END;

END;

procedure TMainForm.AlgolComboChange(Sender: TObject);
begin
  AlgolIndex:=AlgolCombo.ItemIndex;
end;

procedure TMainForm.cmdAutoClick(Sender: TObject);
begin
   AutoFlag:=TRUE;
   SR.MakeSnap;
   IF Not DirectoryExists(AutoDir) then
      IF Not CreateDir (AutoDir) THEN
	 Writeln ('Failed to create directory !')
      else
	 Writeln ('Created "NewDir" directory');
   PostMessage(Handle,MSG_DrawNextScene,0,0);
end;

procedure TMainForm.cmdSaveClick(Sender: TObject);
begin
  IF SaveDlg.Execute THEN ImgRender.Picture.SaveToFile(SaveDlg.Filename);
end;

procedure TMainForm.SceneComboChange(Sender: TObject);
begin
  ModelIndex:=SceneCombo.ItemIndex;
end;


{ TRenderThread }

procedure TRenderThread.InitRend;
begin
   IF AutoFlag = FALSE THEN BEGIN
      ModelList:=CopyScene(MainForm.ModelIndex);
      CamR.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),wide,h,0.5135,140);
   END
   ELSE BEGIN
      CamR:=SR.CurSceneRec.cam;
      ModelList:=SR.CurSceneRec.spl;
      AutoIndex:=SR.SceneIndex;
   END;
end;
procedure TRenderThread.DoRend;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example MainForm.Caption.
var
  x : integer;
BEGIN
   IF AutoFlag THEN
      MainForm.Caption:='AutoIndex='+IntToStr(AutoIndex)+':'+fStatusText
   ELSE
      MainForm.Caption := fStatusText;
  IF DoneCalc=FALSE THEN BEGIN
    FOR x:=0 to Wide-1 DO BEGIN
      MainForm.ImgRender.Canvas.Pixels[x,yRender]:=
 	     ColToByte(LineBuffer[x].x)+         //red
             ColToByte(LineBuffer[x].y)*256+     //green
             ColToByte(LineBuffer[x].z)*256*256; //blune
    END;
    MainForm.LblTime.Caption:=SecToTime((GetTickCount64 - MainForm.startTime) DIV 1000);
    yRender:=MainForm.GetYAxis;
  END;
END;
procedure TRenderThread.DoneRend;
var
   st : string;
BEGIN
   AutoFlag:=MainForm.AutoFlag;
   IF MainForm.isAllDone THEN BEGIN
      MainForm.yAxis:=-1;
      MainForm.cmdRender.Enabled:=TRUE;
      MainForm.Caption:='TRenderThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
      
      IF AutoFlag THEN BEGIN
	 st:=IntToStr(SR.SceneIndex)+'.png';
	 while length(st)<7 do st:='0'+st;
	 MainForm.imgRender.Picture.SaveToFile(AutoDir+PathSeparator+st);
	 PostMessage(MainForm.handle,MSG_DrawNextScene,0,0);
      END
      ELSE BEGIN
	 MainForm.imgRender.Picture.SaveToFile(DefaultSaveFile); 
      END;
  END;
END;

procedure TRenderThread.Execute;
var
  x,y,sx,sy,s:integer;
  temp       : VecRecord;
  tColor,r : VecRecord;
  StatusText1:string;
begin
  y:=yRender;
  Synchronize(@InitRend); 
  fStatusText := 'Render Running ...';
  StatusText1:=fStatusText;
  while y<h do begin
    for x:=0 to wide-1 do begin
     r:=CreateVec(0, 0, 0);
     tColor:=ZeroVec;
     for sy:=0 to 1 do begin
	for sx:=0 to 1 do begin
	  for s:=0 to samps-1 do begin
	    temp:=Radiance(CamR.Ray(x,y,sx,sy),0);
	    temp:= temp/ samps;
	    r:= r+temp;
	  end;(*samps*)
	  temp:= ClampVector(r)* 0.25;
	  tColor:=tColor+ temp;
	  r:=CreateVec(0, 0, 0);
	end;(*sx*)
      end;(*sy*)
      LineBuffer[x]:=tColor;
     end;(*x*)
     fStatusText:=StatusText1+'y='+IntToStr(y);
     Synchronize(@DoRend);
     y:=yRender;
  END;(*y*)
  DoneCalc:=TRUE;
  Synchronize(@DoneRend);
 end;


constructor TRenderThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  DoneCalc:=FALSE;
  yRender:=0;
  inherited Create(CreateSuspended);
end;



function TRenderThread.Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
var
  d:real;
  i:integer;
begin
  t:=INF;
  for i:=0 to ModelList.count-1 do begin
    d:=SphereClass(ModelList[i]).intersect(r);
    if d<t THEN BEGIN
      t:=d;
      id:=i;
    END;
  end;
  result:=(t<inf);
END;

function TRenderThread.Radiance(r:RayRecord;depth:integer):VecRecord;
var
  id:integer;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
begin
  id:=0;depth:=depth+1;
  if intersect(r,t,id)=FALSE then begin
    result:=ZeroVec;exit;
  end;
  obj:=SphereClass(ModelList[id]);
  x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
  IF VecDot(n,r.d)<0 THEN nl:=n else nl:=n*-1;
  IF (f.x>f.y)and(f.x>f.z) THEN
    p:=f.x
  ELSE IF f.y>f.z THEN 
    p:=f.y
  ELSE
    p:=f.z;
  IF (Depth > 5) OR (p = 0) THEN BEGIN
      IF (random < p) THEN BEGIN
        f:= f / p;
        IF (p = 1) AND (f.x = 1) AND (f.y = 1) AND (f.z = 1) THEN BEGIN
          Result := obj.e;
          exit;
        END;
      END
      ELSE BEGIN
        Result := obj.e;
        exit;
      END;
  END;
  CASE obj.refl OF
    DIFF:BEGIN
      r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
      w:=nl;
      IF abs(w.x)>0.1 THEN
        u:=VecNorm(MidOneVec/w)
      ELSE BEGIN
        u:=VecNorm(TopOneVec/w);
      END;
      v:=w/u;
      d := VecNorm(u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2));
      result:=obj.e+VecMul(f,Radiance(CreateRay(x,d),depth) );
    END;(*DIFF*)
    SPEC:BEGIN
      result:=obj.e+VecMul(f,(Radiance(CreateRay(x,r.d-n*2*(n*r.d) ),depth)));
    END;(*SPEC*)
    REFR:BEGIN
      RefRay:=CreateRay(x,r.d-n*2*(n*r.d) );
      into:= (n*nl>0);
      nc:=1;nt:=1.5; if into then nnt:=nc/nt else nnt:=nt/nc; ddn:=r.d*nl; 
      cos2t:=1-nnt*nnt*(1-ddn*ddn);
      if cos2t<0 then begin   // Total internal reflection
        result:=obj.e + VecMul(f,Radiance(RefRay,depth));
        exit;
      end;
      if into then q:=1 else q:=-1;
      tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
      IF into then Q:=-ddn else Q:=tdir*n;
      a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
      Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
      IF depth>2 THEN BEGIN
        IF random<p then // 反射
          result:=obj.e+VecMul(f,Radiance(RefRay,depth)*RP)
        ELSE //屈折
          result:=obj.e+VecMul(f,Radiance(CreateRay(x,tdir),depth)*TP);
      END
      ELSE BEGIN// 屈折と反射の両方を追跡
        result:=obj.e+VecMul(f,Radiance(RefRay,depth)*Re+Radiance(CreateRay(x,tdir),depth)*Tr);
      END;
    END;(*REFR*)
  END;(*CASE*)
end;
FUNCTION Utils_kahanSum3(a, b, c : real) : real;
VAR
  sum,cc,y,t: real;
BEGIN
  sum := a;
  cc  := 0.0;

  y   := b - cc;
  t   := sum + y;
  cc  := (t - sum) - y;
  sum := t;

  y   := c - cc;
  t   := sum + y;
  cc  := (t - sum) - y;
  sum := t;

  Utils_kahanSum3 := sum;
END;

FUNCTION Vector_Add3(a, b, c : VecRecord):VecRecord;
BEGIN
  Result.x := Utils_kahanSum3(a.x, b.x, c.x);
  Result.y := Utils_kahanSum3(a.y, b.y, c.y);
  Result.z := Utils_kahanSum3(a.z, b.z, c.z);
END;


function TNERenderThread.Radiance(r:RayRecord;depth:integer):VecRecord;
var
  id,i,tid:integer;
  obj,s:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf,bcf:VecRecord;
  E:integer;
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
    obj:=SphereClass(ModelList[id]);
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

    IF (Depth > 5) OR (p = 0) THEN
       IF (random < p) THEN BEGIN
         f:= f / p;
         IF (p = 1) AND (f.x = 1) AND (f.y = 1) AND (f.z = 1) THEN BEGIN
           Result := cl;
           exit;
         END;
       END
       ELSE BEGIN
         Result := cl;
         exit;
       END;

    bcf:=cf;cf:=VecMul(cf,f);
    CASE obj.refl OF
      DIFF:BEGIN
        r1  := M_2PI * random;
        r2  := random;
        r2s := sqrt(r2);
        w   := nl;

        IF (abs(w.x) > 0.1) THEN BEGIN
          m1 := 1/sqrt(w.z*w.z+w.x*w.x);
          u := CreateVec(w.z*m1, 0, -w.x*m1);
          v := CreateVec(w.y*u.z, w.z*u.x-w.x*u.z, -w.y*u.x); //4* vs 6*
        END
        ELSE BEGIN
          m1 := 1/sqrt(w.z*w.z+w.y*w.y);
          u := CreateVec(0, -w.z*m1, w.y*m1);
          v := CreateVec(w.y*u.z-w.z*u.y, -w.x*u.z, w.x*u.y); //4* vs 6*
        end;
        sincos(r1,ss,cc);

        u:= u*( cc * r2s); //4* cos
        v:= v*(ss * r2s); //4* sin
        w:= w*( sqrt(1 - r2));  //3* sqrt

	d:=Vector_Add3(u, v, w);d:=VecNorm(d);
       // Loop over any lights
        EL:=ZeroVec;
        tid:=id;
        for i:=0 to ModelList.count-1 do BEGIN
          s:=SphereClass(ModelList[i]);
          IF (i=tid) THEN BEGIN
            continue;
          END;
          IF (s.e.x<=0) and  (s.e.y<=0) and (s.e.z<=0)  THEN continue; // skip non-lights
          sw:=s.p-x;
          tr:=sw*sw;  tr:=s.rad2/tr;
	  IF abs(sw.x)/sqrt(tr)>0.1 THEN 
            su:=VecNorm(CreateVec(0,1,0)/sw) 
          ELSE 
            su:=VecNorm(CreateVec(1,0,0)/sw);
          sv:=sw/su;
          IF tr>1 THEN BEGIN
            (*半球の内外=cos_aがマイナスとsin_aが＋、－で場合分け*)
            (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)
            eps1:=M_2PI*random;eps2:=random;eps2s:=sqrt(eps2);
            sincos(eps,ss,cc);
            tu:=u*(cc*eps2s);tu:=tu+v*(ss*eps2s);tu:=tu+w*sqrt(1-eps2);
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
            phi := M_2PI*eps2;
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


function TNRenderThread.Radiance(r:RayRecord;depth:integer):VecRecord;
var
  id:integer;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  tu,tv:VecRecord;
  cl,cf:VecRecord;
BEGIN
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=OneVec;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      exit;
    END;
    obj:=SphereClass(ModelList[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    nrd:=n*r.d;
    IF nrd<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)and(f.x>f.z) THEN
      p:=f.x
    ELSE IF f.y>f.z THEN
      p:=f.y
    ELSE
      p:=f.z;
    cl:=cl+VecMul(cf,obj.e);
    IF (Depth > 5) OR (p = 0) THEN BEGIN
       //p=0は要するに発光体に撃ちあたる場合＝発光体は色がぜろだから
      IF (random < p) THEN BEGIN
        f:= f / p;
        IF (p = 1) AND (f.x = 1) AND (f.y = 1) AND (f.z = 1) THEN BEGIN
            Result := cl;
          exit;
        END;
      END
      ELSE BEGIN
        Result := cl;
        exit;
      END;
    END;
    cf:=VecMul(cf,f);
    CASE obj.refl OF
      DIFF:BEGIN
        r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
        w:=nl;
        IF abs(w.x)>0.01 THEN
          u:=VecNorm(CreateVec(0,1,0)/w)
        ELSE BEGIN
	   u:=VecNorm(CreateVec(1,0,0)/w);
        END;
        v:=w/u;

       sincos(r1,ss,cc);
       u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
       tu:=(u+v)+w;
       d:=VecNorm(tu);
       r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tv:=n*2*nrd ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      END;(*SPEC*)
      REFR:BEGIN
        tv:=n*2*nrd ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        IF cos2t<0 THEN BEGIN   // Total internal reflection
          cl:=cl+VecMul(cf,obj.e);
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
          cl:=cl+VecMul(cf,obj.e);
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e);
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;

 

BEGIN
END.

