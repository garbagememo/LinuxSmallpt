UNIT uXML;

{$mode objfpc}{$H+}
INTERFACE

USES
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


FUNCTION ReadXMLConf(FN:string):SceneRecord;
PROCEDURE WriteXMLScene(ScR :SceneRecord;fn:string);

IMPLEMENTATION
FUNCTION FtoSF(r:real):UnicodeString;
BEGIN
  result:=FloatToStrF(r,ffgeneral,16,4);
END;

PROCEDURE SetVectAttribute(VAR wNode:TDOMNode;v:VecRecord);
BEGIN
  TDOMElement(wNode).SetAttribute('x',FtoSF(v.x));
  TDOMElement(wNode).SetAttribute('y',FtoSF(v.y));
  TDOMElement(wNode).SetAttribute('z',FtoSF(v.z));
END;
FUNCTION GetVectAttribute(VAR wNode:TDOMNode):VecRecord;
VAR
  DebugSt:Widestring;
BEGIN
  Debugst:=TDOMElement(wNode).GetAttribute('x');
  WRITELN(' debug st=',Debugst);
  result.x:=StrToFloat(TDOMElement(wNode).GetAttribute('x'));
  result.y:=StrToFloat(TDOMElement(wNode).GetAttribute('y'));
  result.z:=StrToFloat(TDOMElement(wNode).GetAttribute('z'));
END;

PROCEDURE CameraToNode(xdoc:TXMLDocument;Cam:CameraRecord;VAR wNode:TDOMNode);
VAR
  NodeOrg,NodeDirect:TDOMNode;
BEGIN
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

 
END;

PROCEDURE SphereToNode(xdoc:TXMLDocument;sp:SphereClass;VAR wNode:TDOMNode;i:INTEGER);
VAR
  NodePos,NodeEmit,NodeColor:TDOMnode;
BEGIN
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
END;

PROCEDURE WriteXMLScene(ScR :SceneRecord;fn:string);
VAR
  xdoc: TXMLDocument;            // 文書を格納する変数
  RootNode, parentNode: TDOMNode;// ノードを格納する変数
  sp:SphereClass;
  i:INTEGER;
BEGIN
  //文書を作成する
  xdoc := TXMLDocument.create;

  //ルートノードを作成する
  RootNode := xdoc.CreateElement(ConfStr);
  Xdoc.Appendchild(RootNode);               // ルートノードを保存する

  parentNode:=xdoc.CreateElement(CameraStr);
  RootNode.AppendChild(ParentNode);
  CameraToNode(xdoc,ScR.cam,parentNode);

  FOR i:=0 TO ScR.spl.count-1 DO BEGIN
 //   RootNode:=xdoc.DocumentElement;
    parentNode:=xdoc.CreateElement(SphereStr);
    RootNode.AppendChild(ParentNode);
    sp:=SphereClass(ScR.spl[i]);
    SphereToNode(xdoc,sp,parentNode,i);
  END;
  writeXMLFile(xDoc,FN);                     // XML に書く
  Xdoc.free;                                 // メモリを解放する
END;


PROCEDURE WriteVec(v:VecRecord);
BEGIN
  WRITELN('x=',FtoSF(v.x),' y=',FtoSF(v.y),' z=',FtoSF(v.z) );
END;

FUNCTION ReadXMLConf(FN:string):SceneRecord;
VAR
   Doc	     : TXMLDocument;
   Child     : TDOMNode;
   wNode     : TDOMNode;
   sp	     : SphereClass;
   p,e,c     : VecRecord;
   r	     : real;
   ref	     : RefType;
   spr	     : TList;
   cam	     : CameraRecord;
   w,h	     : INTEGER;
   ratio,dist: real;
   o,d	     : VecRecord;
BEGIN
   ReadXMLFile(Doc,FN);
   spr:=TList.Create;

   Child:=Doc.FirstChild.FirstChild;
   WHILE Assigned(Child) DO BEGIN
      IF (Child.NodeName=SphereStr) OR (Child.NodeName=CameraStr) THEN BREAK;
      Child:=Child.NextSibling;
   END;

   WHILE Assigned(Child) DO BEGIN
      IF Child.NodeName=SphereStr THEN BEGIN
	 r:=StrToFloat(TDOMElement(Child).GetAttribute(RadiusStr));
	 ref:=StrToRef(TDOMElement(Child).GetAttribute(RefTypeStr));
	 wNode:=Child.FirstChild;
	 WHILE Assigned(wNode) DO BEGIN
	    IF wNode.NodeName=positionStr THEN BEGIN
	       p:=GetVectAttribute(wNode);
	    END;
	    IF wNode.NodeName=emittionStr THEN BEGIN
	       e:=GetVectAttribute(wNode);
	    END;
	    IF wNode.NodeName=ColorStr THEN BEGIN
	       c:=GetVectAttribute(wNode);
	    END;
	    wNode:=wNode.NextSibling;
	 END;
	 spr.add(SphereClass.Create(r,p,e,c,ref));
      END;(*Sphere*)
      IF Child.NodeName=CameraStr THEN BEGIN
	 dist:=StrToFloat(TDOMElement(Child).GetAttribute(DistStr));
	 ratio:=StrToFloat(TDOMElement(Child).GetAttribute(RatioStr));
	 w:=StrToInt(TDOMElement(Child).GetAttribute(WideStr));
	 h:=StrToInt(TDOMElement(Child).GetAttribute(HeightStr));
	 wNode:=Child.FirstChild;
	 WHILE Assigned(wNode) DO BEGIN
	    IF wNode.NodeName=OrgStr THEN BEGIN
	       o:=GetVectAttribute(wNode);
	    END;
	    IF wNode.NodeName=directStr THEN BEGIN
	       d:=GetVectAttribute(wNode);
	    END;
	    wNode:=wNode.NextSibling;
	 END;
	 cam.Setup(o,d,w,h,ratio,dist);
      END;
      Child:=Child.NextSibling;
   END;
   result.spl:=spr;
   result.cam:=cam;
END;

BEGIN
END.


