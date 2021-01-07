unit uXML;

{$mode objfpc}{$H+}
Interface

uses
  Classes, SysUtils, DOM, XMLRead,XMLWrite,uVect,uModel;
CONST
  ConfStr='CONF';
  CameraStr='camera';
  OrgStr='org';
  directStr='direct';
  wideStr='wide';
  HeightStr='h';
  ratioStr='ratio';
  distStr='dist';

  SphereStr='Sphere';
  RadiusStr='radius';
  RefTypeStr='ref';
  positionStr='position';
  emittionStr='emittion';
  colorStr='color';


function ReadXMLConf(FN:string):SceneRecord;
procedure WriteXMLScene(ScR :SceneRecord;fn:string);

IMPLEMENTATION
function FtoSF(r:real):UnicodeString;
BEGIN
  result:=FloatToStrF(r,ffgeneral,16,4);
END;

procedure SetVectAttribute(var wNode:TDOMNode;v:VecRecord);
BEGIN
  TDOMElement(wNode).SetAttribute('x',FtoSF(v.x));
  TDOMElement(wNode).SetAttribute('y',FtoSF(v.y));
  TDOMElement(wNode).SetAttribute('z',FtoSF(v.z));
END;
function GetVectAttribute(var wNode:TDOMNode):VecRecord;
var
  DebugSt:Widestring;
BEGIN
  Debugst:=TDOMElement(wNode).GetAttribute('x');
  writeln(' debug st=',Debugst);
  result.x:=StrToFloat(TDOMElement(wNode).GetAttribute('x'));
  result.y:=StrToFloat(TDOMElement(wNode).GetAttribute('y'));
  result.z:=StrToFloat(TDOMElement(wNode).GetAttribute('z'));
END;

procedure CameraToNode(xdoc:TXMLDocument;Cam:CameraRecord;var wNode:TDOMNode);
var
  NodeOrg,NodeDirect:TDOMNode;
begin
  TDOMElement(wNode).SetAttribute(WideStr,IntToStr(cam.w) );
  TDOMElement(wNode).SetAttribute(HeightStr,IntToStr(cam.h)  );
  TDOMElement(wNode).SetAttribute(ratioStr,FtoSF(cam.ratio) );
  TDOMElement(wNode).SetAttribute(distStr,FtoSF(cam.dist) );
//子ノードを作成する
  NodeOrg:= xdoc.CreateElement(OrgStr);                // 子ノードを一つ作成する
  SetVectAttribute(NodeOrg,cam.o);     // 属性を作成する
  wNode.AppendChild(NodeOrg);       // 子ノードをそれぞれの親ノードに挿入する
//子ノードを作成する
  NodeDirect:= xdoc.CreateElement(DirectStr);                // 子ノードを一つ作成する
  SetVectAttribute(NodeDirect,cam.d);     // 属性を作成する
  wNode.AppendChild(NodeDirect);       // 子ノードをそれぞれの親ノードに挿入する

 
end;

procedure SphereToNode(xdoc:TXMLDocument;sp:SphereClass;var wNode:TDOMNode;i:integer);
var
  NodePos,NodeEmit,NodeColor:TDOMnode;
begin
  TDOMElement(wNode).SetAttribute('id', IntToStr(i));       // 親ノードを示す属性を作成する
  TDOMELement(wNode).SetAttribute(RadiusStr,FtoSF(sp.rad) );
  TDOMElement(wNode).SetAttribute(RefTypeStr,RefToStr(sp.refl) );
  //子ノードを作成する
  NodePOS:= xdoc.CreateElement(positionStr);    // 子ノードを一つ作成する
  SetVectAttribute(NodePOS,sp.p);     // 属性を作成する
  wNode.AppendChild(NodePOS);         // 子ノードをそれぞれの親ノードに挿入する

  NodeEmit := xdoc.CreateElement(emittionStr);  // 子ノードを一つ作成する
  SetVectAttribute(NodeEmit,sp.e);
  wNode.AppendChild(NodeEmit);

  NodeColor:=xdoc.CreateElement(ColorStr);
  SetVectAttribute(NodeColor,sp.c);
  wNode.AppendChild(NodeColor);
end;

procedure WriteXMLScene(ScR :SceneRecord;fn:string);
var
  xdoc: TXMLDocument;            // 文書を格納する変数
  RootNode, parentNode: TDOMNode;// ノードを格納する変数
  sp:SphereClass;
  i:integer;
begin
  //文書を作成する
  xdoc := TXMLDocument.create;

  //ルートノードを作成する
  RootNode := xdoc.CreateElement(ConfStr);
  Xdoc.Appendchild(RootNode);               // ルートノードを保存する

  parentNode:=xdoc.CreateElement(CameraStr);
  RootNode.AppendChild(ParentNode);
  CameraToNode(xdoc,ScR.cam,parentNode);

  for i:=0 to ScR.spl.count-1 do begin
 //   RootNode:=xdoc.DocumentElement;
    parentNode:=xdoc.CreateElement(SphereStr);
    RootNode.AppendChild(ParentNode);
    sp:=SphereClass(ScR.spl[i]);
    SphereToNode(xdoc,sp,parentNode,i);
  end;
  writeXMLFile(xDoc,FN);                     // XML に書く
  Xdoc.free;                                 // メモリを解放する
end;


procedure WriteVec(v:VecRecord);
begin
  Writeln('x=',FtoSF(v.x),' y=',FtoSF(v.y),' z=',FtoSF(v.z) );
end;

function ReadXMLConf(FN:string):SceneRecord;
var
   Doc	     : TXMLDocument;
   Child     : TDOMNode;
   wNode     : TDOMNode;
   sp	     : SphereClass;
   p,e,c     : VecRecord;
   r	     : real;
   ref	     : RefType;
   spr	     : TList;
   cam	     : CameraRecord;
   w,h	     : integer;
   ratio,dist: real;
   o,d	     : VecRecord;
begin
   ReadXMLFile(Doc,FN);
   spr:=TList.Create;

   Child:=Doc.FirstChild.FirstChild;
   while Assigned(Child) do begin
      if (Child.NodeName=SphereStr) or (Child.NodeName=CameraStr) THEN BREAK;
      Child:=Child.NextSibling;
   end;

   while Assigned(Child) do begin
      IF Child.NodeName=SphereStr THEN begin
	 r:=StrToFloat(TDOMElement(Child).GetAttribute(RadiusStr));
	 ref:=StrToRef(TDOMElement(Child).GetAttribute(RefTypeStr));
	 wNode:=Child.FirstChild;
	 while Assigned(wNode) do begin
	    if wNode.NodeName=positionStr then begin
	       p:=GetVectAttribute(wNode);
	    end;
	    IF wNode.NodeName=emittionStr THEN BEGIN
	       e:=GetVectAttribute(wNode);
	    END;
	    IF wNode.NodeName=ColorStr THEN BEGIN
	       c:=GetVectAttribute(wNode);
	    END;
	    wNode:=wNode.NextSibling;
	 end;
	 spr.add(SphereClass.Create(r,p,e,c,ref));
      END;(*Sphere*)
      IF Child.NodeName=CameraStr THEN BEGIN
	 dist:=StrToFloat(TDOMElement(Child).GetAttribute(DistStr));
	 ratio:=StrToFloat(TDOMElement(Child).GetAttribute(RatioStr));
	 w:=StrToInt(TDOMElement(Child).GetAttribute(WideStr));
	 h:=StrToInt(TDOMElement(Child).GetAttribute(HeightStr));
	 wNode:=Child.FirstChild;
	 while Assigned(wNode) do begin
	    if wNode.NodeName=OrgStr then begin
	       o:=GetVectAttribute(wNode);
	    end;
	    IF wNode.NodeName=directStr THEN BEGIN
	       d:=GetVectAttribute(wNode);
	    END;
	    wNode:=wNode.NextSibling;
	 END;
	 cam.Setup(o,d,w,h,ratio,dist);
      END;
      Child:=Child.NextSibling;
   end;
   result.spl:=spr;
   result.cam:=cam;
end;

BEGIN
END.


