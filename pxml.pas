PROGRAM pXML;
{$mode objfpc}{$H+}
USES SysUtils,Classes,uXML,uModel;

VAR
   SR  : SnapRecord;
   ScR : SceneRecord;
   spl : TList;
   cam : CameraRecord;
   pSt : string;
BEGIN
   SR.MakeSnap;
   pSt:='xml';
   IF NOT DirectoryExists(pSt) THEN
      IF NOT CreateDir (pSt) THEN
	 WRITELN ('Failed to create directory !')
      ELSE
	 WRITELN ('Created "NewDir" directory');
   Sr.GetNextScene(640,480);
   writeXMLScene(SR.CurSceneRec,pSt+'/'+'teste.xml');
   WRITELN(' Write XML');
   ScR:=ReadXMLConf(pSt+'/'+'teste.xml');
   WRITELN(' Read XML');
   writeXMLScene(ScR,'testf.xml'); 
END.
