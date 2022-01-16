UNIT uLightPath;

{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
INTERFACE
USES SysUtils,Classes,uVect,uModel,uXML,Math;

CONST 
 LightPathMax=5;
TYPE 
  LightPathRecord=RECORD
    LPMax:INTEGER;
    Ary:ARRAY[0..LightPathMax] OF VertexRecord;
    PROCEDURE clear;
    PROCEDURE Add(V_:VertexRecord);
  END;

  LightPathList=RECORD
    LMax:INTEGER;
    ary:ARRAY[0..255] OF LightPathRecord;
    SR: SceneRecord;
    PROCEDURE Clear;
    PROCEDURE Add(LP : LightPathRecord);
    PROCEDURE SetScene(sr_ :SceneRecord );
    PROCEDURE GetLigthPath;
  END;


IMPLEMENTATION

PROCEDURE LightPathRecord.clear;
BEGIN
  LPMax:=-1;
END;

PROCEDURE LightPathRecord.Add(V_:VertexRecord);
BEGIN
  inc(LPMax);
  IF LPMax>4 THEN BEGIN
    WRITELN('Over List!');
    HALT(0);
  END;
  ary[LPMax]:=v_;
END;

PROCEDURE LightPathList.SetScene(sr_:SceneRecord);
BEGIN
  SR:=sr_;
END;

PROCEDURE LightPathList.Clear;
BEGIN
  LMax:=-1;
END;

PROCEDURE LightPathList.Add(LP : LightPathRecord);
BEGIN
  Inc(LMax);
  Ary[LMax]:=LP;
END;

PROCEDURE LightPathList.GetLigthPath;
VAR
  LP:LightPathRecord;
  tVert,cV:VertexRecord;
  i,depth,k:INTEGER;
  r:RayRecord;
  id:INTEGER;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd,OMEGA,cos_a_max:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP,ts:real;
  tDir:VecRecord;
  tu,tv,sw:VecRecord;
  cl,cf:VecRecord;
BEGIN
  FOR i:=0 TO SR.spl.Count-1 DO BEGIN
    IF SphereClass(SR.spl[i]).isLight THEN BEGIN
      tVert:=SR.GenLight(i);
      LP.Clear;
      LP.Add(tVert);
      r.d:=tVert.n;//球であるからこその省略
      r.o:=tVert.p;
      depth:=1;
      REPEAT
        cf:=tVert.cf;
        IF SR.intersect(r,t,id) =FALSE THEN BREAK;
//get radiance
        obj:=SphereClass(SR.spl[id]);
        IF obj.isLight THEN Break;
        x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
        nrd:=n*r.d;
        IF nrd<0 THEN nl:=n ELSE nl:=n*-1;
        cV.p:=x;cV.n:=nl;cV.id:=id;
        IF (f.x>f.y)AND(f.x>f.z) THEN 
          p:=f.x
        ELSE IF f.y>f.z THEN 
          p:=f.y
        ELSE
          p:=f.z;


        cf:=VecMul(cf,f);
        CASE obj.refl OF
          DIFF:BEGIN
            r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
            w:=nl;
            IF abs(w.x)>0.01 THEN u:=VecNorm(CreateVec(0,1,0)/w) ELSE u:=VecNorm(CreateVec(1,0,0)/w);
            v:=w/u;
            sincos(r1,ss,cc);
            u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
            d:=VecNorm( VecAdd3(u,v,w) );
            r:=CreateRay(x,d)
          END;(*DIFF*)
          SPEC:BEGIN
//            tv:=n*2*nrd ;tv:=r.d-tv;
            r:=CreateRay(x,(r.d-(n*2*nrd) ) );
          END;(*SPEC*)
          REFR:BEGIN
//            tv:=n*2*nrd ;tv:=r.d-tv;
            RefRay:=CreateRay(x,(r.d-(n*2*nrd) ) );
            into:= (n*nl>0);
            nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl;
            cos2t:=1-nnt*nnt*(1-ddn*ddn);
            IF cos2t<0 THEN BEGIN   // Total internal reflection
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
              r:=RefRay;
            END
            ELSE BEGIN//屈折
              cf:=cf*TP;
              r:=CreateRay(x,tdir);
            END
          END;(*REFR*)
        END;(*CASE*)

//OMEGA 算出
        IF obj.REFL=DIFF THEN BEGIN
          sw:=r.d;
          tr:=VecSQR(SphereClass(SR.spl[tVert.id]).p-x);
          tr:=SphereClass(SR.spl[tVert.id]).rad2/tr;
          ts:=sw*cV.n;
          IF ts<0 THEN ts:=-ts;
          IF tr>1 THEN BEGIN
              (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)
            OMEGA:=1;
          END
          ELSE BEGIN //半球外部の場合;
            cos_a_max := sqrt(1-tr );
            OMEGA := 2*PI*(1-cos_a_max)/PI;// 1/pi for brdf
            OMEGA:=OMEGA*ts;
          END;
    //OMEGA算出
          cf:=cf*OMEGA;
        END;
        cV.cf:=cf;
        LP.Add( cV);
        tVert:=cV;
        Inc(Depth)
      UNTIL Depth>=LightPathMax;
      Add(LP);
    END;(*is Light*)
  END;(*obj毎*)
END;

BEGIN
END.


