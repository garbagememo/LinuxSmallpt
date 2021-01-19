UNIT uModel;
{$MODE objfpc}
{$INLINE ON}
{$modeswitch advancedrecords}
INTERFACE
uses SysUtils,Classes,uVect;


const
  eps=1e-4;
  INF=1e20;
  M_1_PI=1/pi;
  M_2PI=2*pi;
type
  SphereClass=CLASS
    rad,rad2:real;       //radius, radius^2
    p,e,c:VecRecord;// position. emission,color
    refl:RefType;
    constructor Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
    function intersect(const r:RayRecord):real;
  END;
  CameraRecord=Record
    o,d,cx,cy:VecRecord;
    dist:real;
    w,h:integer;
    ratio:real;
    procedure Setup(o_,d_:VecRecord;w_,h_:integer;ratio_,dist_:real);
    function Ray(x,y,sx,sy:integer):RayRecord;
  END;
  SceneRecord=RECORD
    spl:TList;
    cam:CameraRecord;
  end;

  SnapRecord=RECORD
     SnapList,CamList: TList;
     SceneIndex	     : integer;
     CurSceneRec     : SceneRecord;
     procedure MakeSnap;
     function CopySnap(id:integer):TList;
     function CopyCamera(id,w,h:integer):CameraRecord;
     function GetNextScene(w,h:integer):boolean;
  END;

procedure InitScene;
function CopyScene(id:integer):TList;

var
  sc,ScList:TList;
  ScName:TStringList;
  SR:SnapRecord;

IMPLEMENTATION

procedure SnapRecord.MakeSnap;
var
  i,j,k:integer;
  r,f:real;
  ef:array[0..5] of real;
  bc:array[0..5] of VecRecord;
begin
  SnapList:=TList.Create;
  bc[0]:=CreateVec(1.00,0.50,0.50);
  bc[1]:=CreateVec(0.75,0.50,0.75);
  bc[2]:=CreateVec(0.50,0.50,1.00);
  bc[3]:=CreateVec(0.50,0.75,0.75);
  bc[4]:=CreateVec(0.50,1.00,0.50);
  bc[5]:=CreateVec(0.75,0.75,0.50);
  for i:=0 to 5 do begin
    for k:=0 to 5 do ef[k]:=0;
    ef[i]:=3;
    for j:=0 to 20 do begin
      f:=1-abs(j*0.1-1);
      ScList:=TList.Create;
R:=50;
      ScList.add( SphereClass.Create( R,CreateVec(100,310+R,150), CreateVec(4,4,4),   ZeroVec,  DIFF) );//Ligth
      ScList.add(SphereClass.Create(100000, CreateVec(50, -100000, 0),ZeroVec,CreateVec(0.4,0.6,0.99),DIFF)); // grnd

R:=14;
      ScList.Add(SphereClass.Create(R,CreateVec(    0,R, 40),bc[0]*ef[0]*f,bc[0],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec(-34.6,R, 20),bc[1]*ef[1]*f,bc[1],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec(-34.6,R,-20),bc[2]*ef[2]*f,bc[2],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec(    0,R,-40),bc[3]*ef[3]*f,bc[3],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec( 34.6,R,-20),bc[4]*ef[4]*f,bc[4],DIFF));
      ScList.Add(SphereClass.Create(R,CreateVec( 34.6,R, 20),bc[5]*ef[5]*f,bc[5],DIFF));

      SnapList.Add(ScList);
    end;
  end;
  SceneIndex:=0;
end;
function SnapRecord.GetNextScene(w,h:integer):boolean;
begin
  IF Assigned(SnapList)=FALSE THEN BEGIN
    result:=false;
    exit;
  END;
  IF SnapList.Count<=SceneIndex THEN BEGIN
    result:=false;
     exit; 
  end;
   CurSceneRec.spl:=CopySnap(SceneIndex);
   CurSceneRec.cam:=CopyCamera(SceneIndex,w,h);
   Inc(SceneIndex);
end;

function SnapRecord.CopySnap(id:integer):TList;
BEGIN
  result:=TList(SnapList[id]);
END;
function SnapRecord.CopyCamera(id,w,h:integer):CameraRecord;
var
  tCam:CameraRecord;
begin
  tCam.Setup( CreateVec(0, 300, 0),VecNorm(CreateVec(0,-1,0) ),w,h,0.5135,140);
  result:=tCam;
end;

procedure CameraRecord.Setup(o_,d_:VecRecord;w_,h_:integer;ratio_,dist_:real);
begin
  ratio:=ratio_;dist:=dist_;w:=w_;h:=h_;
  o:=o_;d:=VecNorm(d_);
  cx:=CreateVec(ratio*w_/h_,0,0);
  cy:=VecNorm(cx/d_)*ratio;
end;

function CameraRecord.Ray(x,y,sx,sy:integer):RayRecord;
VAR
  r1,r2,dx,dy:real;
  td:VecRecord;
BEGIN
  r1:=2*random;
  IF r1<1 THEN dx:=sqrt(r1)-1 ELSE dx:=1-sqrt(2-r1);
  r2:=2*random;
  IF (r2 < 1) THEN dy := sqrt(r2)-1 ELSE dy := 1-sqrt(2-r2);
  td:= cy*(((sy + 0.5 + dy)/2 + (h-y-1))/h - 0.5)+cx*(((sx + 0.5 + dx)/2 + x)/w - 0.5)+d;
  td:=VecNorm(td);
  result.o:= td*dist+ o;
  result.d := td;
END;

constructor SphereClass.Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
BEGIN
  rad:=rad_;p:=p_;e:=e_;c:=c_;refl:=refl_;rad2:=rad*rad;
END;
function SphereClass.intersect(const r:RayRecord):real;
var
  op:VecRecord;
  t,b,det:real;
BEGIN
  op:=p-r.o;
  t:=eps;b:=op*r.d;det:=b*b-op*op+rad2;
  IF det<0 THEN
    result:=INF
  ELSE BEGIN
    det:=sqrt(det);
    t:=b-det;
    IF t>eps THEN
      result:=t
    ELSE BEGIN
      t:=b+det;
      IF t>eps THEN
        result:=t
      ELSE
        result:=INF;
    END;
  END;
END;

procedure InitScene;
var
  Cen,C,TC,SCC:VecRecord;
  R,T,D,Z:real;
BEGIN
  sc:=TList.Create;
  ScName:=TStringList.Create;

  //----------debug  sc0-----------
ScName.add('0-Debug Scene');
ScList:=TList.Create;

  ScList.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  ScList.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  ScList.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  ScList.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  ScList.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  ScList.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
  ScList.add( SphereClass.Create(600,CreateVec(50,681.6-0.27,81.6), CreateVec(4,4,4),   ZeroVec,  DIFF) );//Ligth



Sc.Add(ScList);




//----------cornel box sc1-----------
ScName.Add('1-cornel Box');
ScList:=TList.Create;
  ScList.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  ScList.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  ScList.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  ScList.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  ScList.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  ScList.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  ScList.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
  ScList.add( SphereClass.Create( 1.5,CreateVec(50,81.6-16.5,81.6), CreateVec(4,4,4)*100,   ZeroVec,  DIFF) );//Ligth
Sc.Add(ScList);

//-----------sky sc2--------------
ScName.Add('2-Sky');
ScList:=TList.Create;
  Cen:=CreateVec(50,40.8,-860);

  ScList.add(SphereClass.Create(1600, CreateVec(1,0,2)*3000, CreateVec(1,0.9,0.8)*1.2e1*1.56*2,ZeroVec, DIFF)); // sun
  ScList.add(SphereClass.Create(1560, CreateVec(1,0,2)*3500,CreateVec(1,0.5,0.05)*4.8e1*1.56*2, ZeroVec,  DIFF) ); // horizon sun2
  ScList.add(SphereClass.Create(10000,Cen+CreateVec(0,0,-200), CreateVec(0.00063842, 0.02001478, 0.28923243)*6e-2*8, CreateVec(0.7,0.7,1)*0.25,  DIFF)); // sky

  ScList.add(SphereClass.Create(100000, CreateVec(50, -100000, 0),ZeroVec,CreateVec(0.3,0.3,0.3),DIFF)); // grnd
  ScList.add(SphereClass.Create(110000, CreateVec(50, -110048.5, 0),CreateVec(0.9,0.5,0.05)*4,ZeroVec,DIFF));// horizon brightener
  ScList.add(SphereClass.Create(4e4, CreateVec(50, -4e4-30, -3000),ZeroVec,CreateVec(0.2,0.2,0.2),DIFF));// mountains

  ScList.add(SphereClass.Create(26.5,CreateVec(22,26.5,42),ZeroVec,CreateVec(1,1,1)*0.596, SPEC)); // white Mirr
  ScList.add(SphereClass.Create(13,CreateVec(75,13,82),ZeroVec,CreateVec(0.96,0.96,0.96)*0.96, REFR));// Glas
  ScList.add(SphereClass.Create(22,CreateVec(87,22,24),ZeroVec,CreateVec(0.6,0.6,0.6)*0.696, REFR));    // Glas2
Sc.Add(ScList);

//------------nightsky sc3----
ScName.Add('3-nightsky');
ScList:=TList.Create;
  ScList.add(SphereClass.Create(2.5e3,CreateVec(0.82,0.92,0-2)*1e4,    CreateVec(1,1,1)*0.8e2,     ZeroVec, DIFF)); // moon
  ScList.add(SphereClass.Create(2.5e4,CreateVec(50, 0, 0),  CreateVec(0.114, 0.133, 0.212)*1e-2,  CreateVec(0.216,0.384,1)*0.003, DIFF)); // sky
  ScList.add(SphereClass.Create(5e0,  CreateVec(-0.2,0.16,-1)*1e4, CreateVec(1.00, 0.843, 0.698)*1e2,   ZeroVec, DIFF));  // star
  ScList.add(SphereClass.Create(5e0,  CreateVec(0,  0.18,-1)*1e4,  CreateVec(1.00, 0.851, 0.710)*1e2,  ZeroVec, DIFF));  // star
  ScList.add(SphereClass.Create(5e0,  CreateVec(0.3, 0.15,-1)*1e4, CreateVec(0.671, 0.780, 1.00)*1e2,   ZeroVec, DIFF));  // star
  ScList.add(SphereClass.Create(3.5e4,CreateVec(600,-3.5e4+1, 300), ZeroVec,   CreateVec(0.6,0.8,1)*0.01,  REFR));   //pool
  ScList.add(SphereClass.Create(5e4,  CreateVec(-500,-5e4+0, 0),    ZeroVec,   CreateVec(1,1,1)*0.35,  DIFF));    //hill
  ScList.add(SphereClass.Create(16.5, CreateVec(27,0,47),           ZeroVec,   CreateVec(1,1,1)*0.33, DIFF)); //hut
  ScList.add(SphereClass.Create(7,    CreateVec(27+8*sqrt(2),0,47+8*sqrt(2)),ZeroVec,  CreateVec(1,1,1)*0.33,  DIFF)); //door
  ScList.add(SphereClass.Create(500,  CreateVec(-1e3,-300,-3e3), ZeroVec,  CreateVec(1,1,1)*0.351,    DIFF));  //mnt
  ScList.add(SphereClass.Create(830,  CreateVec(0,   -500,-3e3), ZeroVec,  CreateVec(1,1,1)*0.354,    DIFF));  //mnt
  ScList.add(SphereClass.Create(490,  CreateVec(1e3, -300,-3e3), ZeroVec,  CreateVec(1,1,1)*0.352,    DIFF));  //mnt
Sc.Add(ScList);

//-----------island sc4-------
ScName.Add('4-island');
ScList:=TList.Create;
  Cen:=CreateVec(50,-20,-860);

  ScList.add(SphereClass.Create(160, Cen+CreateVec(0, 600, -500),CreateVec(1,1,1)*2e2, ZeroVec,  DIFF)); // sun
  ScList.add(SphereClass.Create(800, Cen+CreateVec(0,-880,-9120),CreateVec(1,1,1)*2e1, ZeroVec,  DIFF)); // horizon
  ScList.add(SphereClass.Create(10000,Cen+CreateVec(0,0,-200), CreateVec(0.0627, 0.188, 0.569)*1e0, CreateVec(1,1,1)*0.4,  DIFF)); // sky
  ScList.add(SphereClass.Create(800, Cen+CreateVec(0,-720,-200),ZeroVec,  CreateVec(0.110, 0.898, 1.00)*0.996,  REFR)); // water
  ScList.add(SphereClass.Create(790, Cen+CreateVec(0,-720,-200),ZeroVec,  CreateVec(0.4,0.3,0.04)*0.6, DIFF)); // earth
  ScList.add(SphereClass.Create(325, Cen+CreateVec(0,-255,-50), ZeroVec,  CreateVec(0.4,0.3,0.04)*0.8, DIFF)); // island
  ScList.add(SphereClass.Create(275, Cen+CreateVec(0,-205,-33), ZeroVec,  CreateVec(0.02,0.3,0.02)*0.75,DIFF)); // grass
Sc.Add(ScList);

//-------------Vista sc5------------
ScName.Add('5-Vista');
ScList:=TList.Create;
  Cen:=CreateVec(50,-20,-860);

  ScList.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,-900),CreateVec(1,0.4,0.1)*5e-1, ZeroVec,  DIFF)); // sun
  ScList.add(SphereClass.Create(1e4,  Cen+ZeroVec, CreateVec(0.631, 0.753, 1.00)*3e-1, CreateVec(1,1,1)*0.5,  DIFF)); // sky

  ScList.add(SphereClass.Create(150,  Cen+CreateVec(-350,0, -100),ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(200,  Cen+CreateVec(-210,0,-100), ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(145,  Cen+CreateVec(-210,85,-100),ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF)); // snow
  ScList.add(SphereClass.Create(150,  Cen+CreateVec(-50,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(150,  Cen+CreateVec(100,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(125,  Cen+CreateVec(250,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  ScList.add(SphereClass.Create(150,  Cen+CreateVec(375,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt

  ScList.add(SphereClass.Create(2500, Cen+CreateVec(0,-2400,-500),ZeroVec,  CreateVec(1,1,1)*0.1,  DIFF)); // mnt base

  ScList.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,200), ZeroVec,  CreateVec(0.2,0.2,1),    REFR)); // water
  ScList.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,1100),ZeroVec,  CreateVec(0,0.3,0),     DIFF)); // grass
  ScList.add(SphereClass.Create(8   , Cen+CreateVec(-75, -5, 850),ZeroVec,  CreateVec(0,0.3,0),     DIFF)); // bush
  ScList.add(SphereClass.Create(30,   Cen+CreateVec(0,   23, 825),ZeroVec,  CreateVec(1,1,1)*0.996, REFR)); // ball

  ScList.add(SphereClass.Create(30,  Cen+CreateVec(200,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(237,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds
  ScList.add(SphereClass.Create(28,  Cen+CreateVec(267,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds

  ScList.add(SphereClass.Create(40,  Cen+CreateVec(150,280,-1000),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(187,280,-1000),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  ScList.add(SphereClass.Create(40,  Cen+CreateVec(600,280,-1100),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(637,280,-1100),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  ScList.add(SphereClass.Create(37,  Cen+CreateVec(-800,280,-1400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF)); // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(0,280,-1600),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));    // clouds
  ScList.add(SphereClass.Create(37,  Cen+CreateVec(537,280,-1800),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
sc.add(ScList);

//----------------Overlap  sc6-----------------
ScName.Add('6-Overlap');
ScList:=TList.Create;

D:=50;
R:=40;
  ScList.add(SphereClass.Create(150, CreateVec(50+75,28,62), CreateVec(1,1,1)*0e-3, CreateVec(1,0.9,0.8)*0.93, REFR));
  ScList.add(SphereClass.Create(28,  CreateVec(50+5,-28,62), CreateVec(1,1,1)*1e1, ZeroVec, DIFF));
  ScList.add(SphereClass.Create(300, CreateVec(50,28,62), CreateVec(1,1,1)*0e-3, CreateVec(1,1,1)*0.93, SPEC));
Sc.add(ScList);

//----------------wada  sc7-------------
ScName.Add('7-wada');
ScList:=TList.Create;

R:=60;
//double R=120;
T:=30*PI/180.;
D:=R/cos(T);
Z:=60;

  ScList.add(SphereClass.Create(1e5, CreateVec(50, 100, 0),      CreateVec(1,1,1)*3e0, ZeroVec, DIFF)); // sky
  ScList.add(SphereClass.Create(1e5, CreateVec(50, -1e5-D-R, 0), ZeroVec,     CreateVec(0.1,0.1,0.1),DIFF));           //grnd

  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec( cos(T),sin(T),0)*D, ZeroVec, CreateVec(1,0.3,0.3)*0.999, SPEC)); //red
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(-cos(T),sin(T),0)*D, ZeroVec, CreateVec(0.3,1,0.3)*0.999, SPEC)); //grn
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,-1,0)*D,         ZeroVec, CreateVec(0.3,0.3,1)*0.999, SPEC)); //blue
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,0,-1)*D,       ZeroVec, CreateVec(0.53,0.53,0.53)*0.999, SPEC)); //back
  ScList.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,0,1)*D,      ZeroVec, CreateVec(1,1,1)*0.999, REFR)); //front
Sc.Add(ScList);

//-----------------wada2 sc8----------
ScName.Add('8-wada2');
ScList:=TList.Create;

R:=120;     // radius
T:=30*PI/180.;
D:=R/cos(T);     //distance
Z:=62;
C:=CreateVec(0.275, 0.612, 0.949);

  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec( cos(T),sin(T),0)*D,    C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //red
  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(-cos(T),sin(T),0)*D,    C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //grn
  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(0,-1,0)*D,              C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //blue
  ScList.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(0,0,-1)*R*2*sqrt(2/3),C*0e-2,CreateVec(1,1,1)*0.996, SPEC)); //back
  ScList.add(SphereClass.Create(2*2*R*2*sqrt(2/3)-R*2*sqrt(2/3)/3, CreateVec(50,28,Z)+CreateVec(0,0,-R*2*sqrt(2/3)/3),   CreateVec(1,1,1)*0,CreateVec(1,1,1)*0.5, SPEC)); //front
sc.Add(ScList);

//---------------forest sc9-----------
ScName.Add('9-forest');
ScList:=TList.Create;

tc:=CreateVec(0.0588, 0.361, 0.0941);
scc:=CreateVec(1,1,1)*0.7;
  ScList.add(SphereClass.Create(1e5, CreateVec(50, 1e5+130, 0),  CreateVec(1,1,1)*1.3,ZeroVec,DIFF)); //lite
  ScList.add(SphereClass.Create(1e2, CreateVec(50, -1e2+2, 47),  ZeroVec,CreateVec(1,1,1)*0.7,DIFF)); //grnd

  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, 300)+CreateVec(-sin(50*PI/180),0,cos(50*PI/180))*1e4, ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr L
  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, 300)+CreateVec(sin(50*PI/180),0,cos(50*PI/180))*1e4,  ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr R
  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, -50)+CreateVec(-sin(30*PI/180),0,-cos(30*PI/180))*1e4,ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr FL
  ScList.add(SphereClass.Create(1e4, CreateVec(50, -30, -50)+CreateVec(sin(30*PI/180),0,-cos(30*PI/180))*1e4, ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr


  ScList.add(SphereClass.Create(4, CreateVec(50,6*0.6,47),   ZeroVec,CreateVec(0.13,0.066,0.033), DIFF));//"tree"
  ScList.add(SphereClass.Create(16,CreateVec(50,6*2+16*0.6,47),   ZeroVec, tc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(11,CreateVec(50,6*2+16*0.6*2+11*0.6,47),   ZeroVec, tc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(7, CreateVec(50,6*2+16*0.6*2+11*0.6*2+7*0.6,47),   ZeroVec, tc,  DIFF));//"tree"

  ScList.add(SphereClass.Create(15.5,CreateVec(50,1.8+6*2+16*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(10.5,CreateVec(50,1.8+6*2+16*0.6*2+11*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
  ScList.add(SphereClass.Create(6.5, CreateVec(50,1.8+6*2+16*0.6*2+11*0.6*2+7*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
sc.add(ScList);

 

END;

function CopyScene(id:integer):TList;
var
  i:integer;
  rc,w:TList;
  s:SphereClass;
begin
  IF (id>sc.count-1) OR (id<0) THEN BEGIN
    result:=NIL;
    EXIT;
  END;
  rc:=TList.Create;
  w:=TList(sc[id]);
  FOR i:=0 TO w.count-1 DO BEGIN
    s:=SphereClass(w[i]);
    rc.add(SphereClass.Create(s.rad,s.p,s.e,s.c,s.refl) );
  END;
  result:=rc;
end;

BEGIN
END.



