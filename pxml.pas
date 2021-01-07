program pXML;
{$mode objfpc}{$H+}
uses SysUtils,Classes,uXML,uModel;

var
   SR  : SnapRecord;
   ScR : SceneRecord;
   spl : TList;
   cam : CameraRecord;
   pSt : string;
BEGIN
   SR.MakeSnap;
   pSt:='xml';
   If Not DirectoryExists(pSt) then
      If Not CreateDir (pSt) Then
	 Writeln ('Failed to create directory !')
      else
	 Writeln ('Created "NewDir" directory');
   Sr.GetNextScene(640,480);
   writeXMLScene(SR.CurSceneRec,pSt+'/'+'teste.xml');
   writeln(' Write XML');
   ScR:=ReadXMLConf(pSt+'/'+'teste.xml');
   writeln(' Read XML');
   writeXMLScene(ScR,'testf.xml'); 
end.
