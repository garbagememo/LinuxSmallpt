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
UNIT MainRender;

{$mode objfpc}{$H+}

INTERFACE

USES
  LCLIntf, LCLType,
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,LMessages,
  SysUtils, uVect,uModel,uLightPath,math;

CONST
  MSG_NEWLINE	     = WM_USER + 0;
  MSG_DecThreadCount = WM_USER+1;
  MSG_DrawNextScene  = WM_USER+2;
  DefaultSaveFile    = 'out.png';
  AutoDir	     = 'auto';
   PathSeparator     = '/';(*IF Windows THEN \*)

TYPE
  { TRenderThread }

  TLineBuffer=ARRAY[0..1980] OF VecRecord;
  (*TColor=r,g,b*)

  TRenderThread = CLASS(TThread)
  PRIVATE
    fStatusText: string;
    DoneCalc:BOOLEAN;
    PROCEDURE InitRend;
    PROCEDURE DoRend;
    PROCEDURE DoneRend;
  PROTECTED
    PROCEDURE Execute; override;
  PUBLIC
    LineBuffer:TLineBuffer;
    wide,h,samps:INTEGER;
    MSR:SceneRecord;
    yRender:INTEGER;
    AutoFlag:BOOLEAN;
    AutoIndex:INTEGER;
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;virtual;
    CONSTRUCTOR Create(CreateSuspended: BOOLEAN);
  END;
  TNERenderThread = CLASS(TRenderThread)
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;OverRide;
  END;
  TLoopRenderThread= CLASS(TRenderThread)
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;OverRide;
  END;
  TLightPathRenderThread=CLASS(TRenderThread)
    LPList:LightPathList;
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;OverRide;
  END;


  { TMainForm }

  TMainForm = CLASS(TForm)
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

    PROCEDURE AlgolComboChange(Sender: TObject);
    PROCEDURE cmdAutoClick(Sender: TObject);
    PROCEDURE cmdRenderClick(Sender: TObject);
    PROCEDURE cmdSaveClick(Sender: TObject);
    PROCEDURE SceneComboChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE SaveDlgClick(Sender: TObject);
  PRIVATE
    ModelIndex:INTEGER;
    AlgolIndex:INTEGER;
    MinimamHeight:INTEGER;
  PUBLIC
    ThreadNum:INTEGER;
    samps:INTEGER;
    StartTime:Int64;
    ThreadList:TList;
    yAxis:INTEGER;
    AutoFlag:BOOLEAN;
    PROCEDURE RenderSetup;
    FUNCTION isAllDone:BOOLEAN;
    FUNCTION GetYAxis:INTEGER;
    PROCEDURE MSGDrawNextScene(VAR Message:TLMessage);MESSAGE MSG_DrawNextScene;
  END;

    
VAR
  MainForm: TMainForm;

IMPLEMENTATION

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

FUNCTION TMainForm.isAllDone:BOOLEAN;
VAR
  i:INTEGER;
BEGIN
  isAllDone:=TRUE;
  FOR i:=0 TO ThreadNum-1 DO BEGIN
     IF TRenderThread(ThreadList[i]).DoneCalc=FALSE THEN BEGIN
        isAllDone:=FALSE;
       EXIT;
     END;
  END;
END;

FUNCTION TMainForm.GetYAxis:INTEGER;
BEGIN
   yAxis:=yAxis+1;
   result:=yAxis;
END;

PROCEDURE TMainForm.FormCreate(Sender: TObject);
VAR
  i:INTEGER;
BEGIN
    DoubleBuffered := TRUE;
    TOP:=10;
    Left:=10;
    SLR.InitScene(320,240);
    FOR i:=0 TO SLR.MaxIndex DO
      SceneCombo.Items.Add(SLR.SrA[i].SceneName);
    SceneCombo.ItemIndex:=1;
    ModelIndex:=1;
    AlgolCombo.Items.Add('Original');
    AlgolCombo.Items.Add('Next Event');
    AlgolCombo.Items.Add('Non Loop');
    AlgolCombo.Items.Add('LightPath');
    AlgolCombo.ItemIndex:=1;
    AlgolIndex:=1;
    MinimamHeight:=Height;
    AutoFlag:=FALSE;
    Randomize;
END;

PROCEDURE TMainForm.SaveDlgClick(Sender: TObject);
BEGIN
  IF (SaveDlg.Execute) THEN
    imgRender.Picture.SaveToFile(SaveDlg.FileName);
END;

PROCEDURE TMainForm.RenderSetup;
VAR
  RenderThread: TRenderThread;
  i:INTEGER;
BEGIN
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
      IF AlgolIndex=1 THEN
	 RenderThread:=TNERenderThread.Create(TRUE);
      IF AlgolIndex=0 THEN
	 RenderThread:=TRenderThread.Create(True);
      IF AlgolIndex=2 THEN
	 RenderThread:=TLoopRenderThread.Create(True);
      IF AlgolIndex=3 THEN BEGIN
         RenderThread:=TLightPathRenderThread.Create(True);
      END;
      // True parameter it doesnt start automatically
      IF Assigned(RenderThread.FatalException) THEN
	 raise RenderThread.FatalException;
      RenderThread.wide:=imgRender.Width;
      RenderThread.h:=imgRender.Height;
      RenderThread.samps:=StrToInt(StrSampleCount.text);
      RenderThread.yRender:=GetYAxis;
      RenderThread.DoneCalc:=FALSE;
      ThreadList.Add(RenderThread);
   END;
   StartTime:=GetTickCount64; 
END;
PROCEDURE TMainForm.cmdRenderClick(Sender: TObject);
VAR
  RenderThread: TRenderThread;
  i:INTEGER;
BEGIN
   RenderSetup;
   FOR i:=0 TO ThreadList.Count-1 DO BEGIN
     TRenderThread(ThreadList[i]).AutoFlag:=FALSE;
     TRenderThread(ThreadList[i]).Start;
   END;
END;
PROCEDURE TMainForm.MSGDrawNextScene(VAR Message : TLMessage);
VAR
  SceneRec:SceneRecord;
  RenderThread: TRenderThread;
  i:INTEGER;
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

PROCEDURE TMainForm.AlgolComboChange(Sender: TObject);
BEGIN
  AlgolIndex:=AlgolCombo.ItemIndex;
END;

PROCEDURE TMainForm.cmdAutoClick(Sender: TObject);
BEGIN
   AutoFlag:=TRUE;
   SR.MakeSnap;
   IF NOT DirectoryExists(AutoDir) THEN
      IF NOT CreateDir (AutoDir) THEN
	 WRITELN ('Failed to create directory !')
      ELSE
	 WRITELN ('Created "NewDir" directory');
   PostMessage(Handle,MSG_DrawNextScene,0,0);
END;

PROCEDURE TMainForm.cmdSaveClick(Sender: TObject);
BEGIN
  IF SaveDlg.Execute THEN ImgRender.Picture.SaveToFile(SaveDlg.Filename);
END;

PROCEDURE TMainForm.SceneComboChange(Sender: TObject);
BEGIN
  ModelIndex:=SceneCombo.ItemIndex;
END;


{ TRenderThread }

PROCEDURE TRenderThread.InitRend;
BEGIN
   IF AutoFlag = FALSE THEN BEGIN
      MSR:=SLR.CopyScene(MainForm.ModelIndex,wide,h);
//      sph:=CopyScene(MainForm.ModelIndex);
//      CamR.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),wide,h,0.5135,140);
   END
   ELSE BEGIN
      MSR.Cam:=SR.CurSceneRec.cam;
      MSR.spl:=SR.CurSceneRec.spl;
      AutoIndex:=SR.SceneIndex;
   END;
   IF MainForm.AlgolIndex=3 THEN BEGIN
     TLightPathRenderThread(self).LPList.SetScene(MSR);
   END;
END;
PROCEDURE TRenderThread.DoRend;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example MainForm.Caption.
VAR
  x : INTEGER;
BEGIN
   IF AutoFlag THEN
      MainForm.Caption:='AutoIndex='+IntToStr(AutoIndex)+':'+fStatusText
   ELSE
      MainForm.Caption := fStatusText;
  IF DoneCalc=FALSE THEN BEGIN
    FOR x:=0 TO Wide-1 DO BEGIN
      MainForm.ImgRender.Canvas.Pixels[x,yRender]:=
 	     ColToByte(LineBuffer[x].x)+         //red
             ColToByte(LineBuffer[x].y)*256+     //green
             ColToByte(LineBuffer[x].z)*256*256; //blune
    END;
    MainForm.LblTime.Caption:=SecToTime((GetTickCount64 - MainForm.startTime) DIV 1000);
    yRender:=MainForm.GetYAxis;
  END;
END;
PROCEDURE TRenderThread.DoneRend;
VAR
   st : string;
BEGIN
   AutoFlag:=MainForm.AutoFlag;
   IF MainForm.isAllDone THEN BEGIN
      MainForm.yAxis:=-1;
      MainForm.cmdRender.Enabled:=TRUE;
      MainForm.Caption:='TRenderThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
      
      IF AutoFlag THEN BEGIN
	 st:=IntToStr(SR.SceneIndex)+'.png';
	 WHILE length(st)<7 DO st:='0'+st;
	 MainForm.imgRender.Picture.SaveToFile(AutoDir+PathSeparator+st);
	 PostMessage(MainForm.handle,MSG_DrawNextScene,0,0);
      END
      ELSE BEGIN
	 MainForm.imgRender.Picture.SaveToFile(DefaultSaveFile); 
      END;
  END;
END;

PROCEDURE TRenderThread.Execute;
VAR
  x,y,sx,sy,s:INTEGER;
  temp       : VecRecord;
  tColor,r : VecRecord;
  StatusText1:string;
BEGIN
  y:=yRender;
  Synchronize(@InitRend); 
  fStatusText := 'Render Running ...';
  StatusText1:=fStatusText;
  WHILE y<h DO BEGIN
    FOR x:=0 TO wide-1 DO BEGIN
     r:=CreateVec(0, 0, 0);
     tColor:=ZeroVec;
     FOR sy:=0 TO 1 DO BEGIN
       FOR sx:=0 TO 1 DO BEGIN
         FOR s:=0 TO samps-1 DO BEGIN
           temp:=Radiance(MSR.Cam.Ray(x,y,sx,sy),0);
           temp:= temp/ samps;
            r:= r+temp;
         END;(*samps*)
         temp:= ClampVector(r)* 0.25;
         tColor:=tColor+ temp;
         r:=CreateVec(0, 0, 0);
	   END;(*sx*)
     END;(*sy*)
     LineBuffer[x]:=tColor;
   END;(*x*)
   fStatusText:=StatusText1+'y='+IntToStr(y);
   Synchronize(@DoRend);
   y:=yRender;
  END;(*y*)
  DoneCalc:=TRUE;
  Synchronize(@DoneRend);
 END;


CONSTRUCTOR TRenderThread.Create(CreateSuspended: BOOLEAN);
BEGIN
  FreeOnTerminate := True;
  DoneCalc:=FALSE;
  yRender:=0;
  inherited Create(CreateSuspended);
END;



FUNCTION TRenderThread.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id:INTEGER;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
BEGIN
  id:=0;depth:=depth+1;
  IF MSR.intersect(r,t,id)=FALSE THEN BEGIN
    result:=ZeroVec;EXIT;
  END;
  obj:=SphereClass(MSR.spl[id]);
  x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
  IF VecDot(n,r.d)<0 THEN nl:=n ELSE nl:=n*-1;
  IF (f.x>f.y)AND(f.x>f.z) THEN
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
          EXIT;
        END;
      END
      ELSE BEGIN
        Result := obj.e;
        EXIT;
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
      nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl; 
      cos2t:=1-nnt*nnt*(1-ddn*ddn);
      IF cos2t<0 THEN BEGIN   // Total internal reflection
        result:=obj.e + VecMul(f,Radiance(RefRay,depth));
        EXIT;
      END;
      IF into THEN q:=1 ELSE q:=-1;
      tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
      IF into THEN Q:=-ddn ELSE Q:=tdir*n;
      a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
      Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
      IF depth>2 THEN BEGIN
        IF random<p THEN // 反射
          result:=obj.e+VecMul(f,Radiance(RefRay,depth)*RP)
        ELSE //屈折
          result:=obj.e+VecMul(f,Radiance(CreateRay(x,tdir),depth)*TP);
      END
      ELSE BEGIN// 屈折と反射の両方を追跡
        result:=obj.e+VecMul(f,Radiance(RefRay,depth)*Re+Radiance(CreateRay(x,tdir),depth)*Tr);
      END;
    END;(*REFR*)
  END;(*CASE*)
END;
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


FUNCTION TNERenderThread.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id,i,tid:INTEGER;
  obj,s:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf:VecRecord;
  E:INTEGER;
BEGIN
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF MSR.intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      EXIT;
    END;
    obj:=SphereClass(MSR.spl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    IF n*r.d<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN
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
       END
       ELSE BEGIN
         Result := cl;
         EXIT;
       END;

    cf:=VecMul(cf,f);
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
        END;
        sincos(r1,ss,cc);

        u:= u*( cc * r2s); //4* cos
        v:= v*(ss * r2s); //4* sin
        w:= w*( sqrt(1 - r2));  //3* sqrt

        d:=Vector_Add3(u, v, w);d:=VecNorm(d);
        // Loop over any lights
        EL:=ZeroVec;
        tid:=id;
        FOR i:=0 TO MSR.spl.count-1 DO BEGIN
          s:=SphereClass(MSR.spl[i]);
          IF (i=tid) THEN BEGIN
            continue;
          END;
          IF (s.e.x<=0) AND  (s.e.y<=0) AND (s.e.z<=0)  THEN continue; // skip non-lights
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
            sincos(eps1,ss,cc);
            tu:=u*(cc*eps2s);tu:=tu+v*(ss*eps2s);tu:=tu+w*sqrt(1-eps2);
            l:=VecNorm(tu);
             IF MSR.intersect(CreateRay(x,l),t,id) THEN BEGIN
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
             l:=VecNorm(sw*(cos(phi)*sin_a)+sv*(sin(phi)*sin_a)+sw*cos_a);
            IF (MSR.intersect(CreateRay(x,l), t, id) ) THEN BEGIN 
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
        cl:= cl+VecMul(cf,tw );
        E:=0;
        r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tw:=obj.e*e;
        cl:=cl+VecMul(cf,tw);
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
          cl:=cl+VecMul(cf,obj.e*E);
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
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;


FUNCTION TLoopRenderThread.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id:INTEGER;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd:real;
  into:BOOLEAN;
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
    IF MSR.intersect(r,t,id)=FALSE THEN BEGIN
      result:=cl;
      EXIT;
    END;
    obj:=SphereClass(MSR.spl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    nrd:=n*r.d;
    IF nrd<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN
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
          EXIT;
        END;
      END
      ELSE BEGIN
        Result := cl;
        EXIT;
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

FUNCTION TLightPathRenderThread.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id,i,j,tid:INTEGER;
  obj,s:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf:VecRecord;
  E:INTEGER;
  LPRec:LightPathRecord;
  tVert:VertexRecord;
  FUNCTION GetLightPathEvent:VecRecord;
  VAR
    i,j:INTEGER;
    tRay:RayRecord;
    ts:real;
  BEGIN
    result:=ZeroVec;tid:=id;
    FOR i:=0 TO LPList.LMax DO BEGIN
      LPRec:=LPList.Ary[i];
      FOR j:=0 TO LPRec.LPMax DO BEGIN
        tVert:=LPRec.Ary[j];
        IF tVert.id=tid THEN continue;//光源だったら飛ばすに変えるべき
        s:=SphereClass(MSR.spl[tVert.id]);
        sw:=VecNorm(tVert.p-x);
        tRay.d:=sw;tRay.o:=x;
        IF sw*nl<0 THEN continue;//裏側につきぬけないように
        IF MSR.intersect(tRay,t,id)=FALSE THEN continue;
        IF id<>tVert.id THEN CONTINUE;//影がある？
        tr:=VecSQR(s.p-x);//ここが怖いところ。
        tr:=tVert.rad2/tr;
        ts:=sw*tVert.n;
        IF ts<0 THEN ts:=-ts;//球の表裏で変わるので・・・・
        IF tr>1 THEN BEGIN
          result:=result+VecMul(f,tVert.cf*ts );
        END
        ELSE BEGIN
          cos_a_max := sqrt(1-tr );
          omega := 2*PI*(1-cos_a_max);
          result:=result + VecMul(f,(tVert.cf*ts*omega))*M_1_PI;// 1/pi for brdf
        END;
      END;
    END;
  END;
BEGIN
  LPList.Clear;
  LPList.GetLigthPath;//////LPL
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  WHILE (TRUE) DO BEGIN
    Inc(depth);
    IF MSR.intersect(r,t,id)=FALSE THEN BEGIN
       result:=cl;
       EXIT;
    END;
    obj:=SphereClass(MSR.spl[id]);
    x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
    IF n*r.d<0 THEN nl:=n ELSE nl:=n*-1;
    IF (f.x>f.y)AND(f.x>f.z) THEN p:=f.x ELSE IF f.y>f.z THEN p:=f.y ELSE p:=f.z;
    tw:=obj.e*E;
    cl:=cl+VecMul(cf,tw);

    IF (Depth > 5) OR (p = 0) THEN
       IF (random < p) THEN BEGIN
         f:= f / p;
       END
       ELSE BEGIN
         Result := cl;
         EXIT;
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
        d:=VecNorm((u+v)+w);

//        EL:=GetNextEvent;
        EL:=GetLightPathEvent;
//        EL:=GetFirstLight;
        tw:=obj.e*e+EL;
        cl:= cl+VecMul(cf,tw );
        E:=0;
        r:=CreateRay(x,d)
      END;(*DIFF*)
      SPEC:BEGIN
        tw:=obj.e*e;
        cl:=cl+VecMul(cf,tw);
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
          cl:=cl+VecMul(cf,obj.e*E);
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
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
        END
        ELSE BEGIN//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=CreateRay(x,tdir);
        END
      END;(*REFR*)
    END;(*CASE*)
  END;(*WHILE LOOP *)
END;

BEGIN
END.


