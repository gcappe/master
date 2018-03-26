{$ifndef JScript}
unit HTMLUtils;
interface
implementation
{$else}
unit HTMLUtils;
interface
uses Classes, SysUtils, StringUtils, NodeUtils;


procedure setPasteCutAreaSize();
function addHandVBoxStyles():string;
procedure ClearAllScreenObjects;
procedure PrepareHeightWidthHTML(var HW,StrVal,StyleVal:string);
procedure SetHeightWidthHTML(MyNode:TDataNode; ob:TObject; HW,AttrValue:string);
function DeleteScreenObject(MyNode:TDataNode):string;
function CreateWrapperHtml(NewNode,ParentNode:TDataNode;ClassName,ScreenObjectName,ScreenObjectType:string):string;
function CreateWrapperDiv(MyNode,ParentNode:TDataNode;ClassName,ScreenObjectName,ScreenObjectType:string;position:integer ):TObject;
procedure AddObjectToParentObject(ParentNode:TDataNode;ParentId,myId:String;position:integer;HTMLString:string);
function ScreenObjectInnerComponent(SystemNode:TDataNode):TObject;
procedure UnHighlight(ObjID:string; HadBorder:boolean);
procedure Highlight(ObjID:string);
procedure ShowHideSelectedBorder(myNode:TDataNode;showborder:Boolean);
function GetDataNodeFromTreeNode(nodeID:string):TDataNode;

const
  glbBorderWidth:integer = 3;
  glbLabelSpacing:integer = 3;
  glbMarginSpacing:string = '3px';

implementation
uses Popup;

function addHandVBoxStyles():string;
var dummy:integer;
begin
dummy:=0;

asm
   // ----------------------------------------check if the style has already been set
    var x = document.getElementsByTagName("STYLE");
    var StyleIsSet = false;
    if (x.length>0){
      for (var i=0; i<x.length; i++){
        var y= x[i].innerHTML;
        if (y.indexOf(".hbox") !=-1) { StyleIsSet =true}
      }
    }


    if (StyleIsSet == false){
        var HandVBoxStyleString = '<style>'
        +'.hbox { '
            +' margin:0px;'
            +' display: -webkit-flex;'
            +' display: -ms-flexbox;'
            +' display: -flex;'
            +' -webkit-flex-direction: row;'
            +' -ms-flex-direction: row;'
            +' flex-direction: row;'
            +' -webkit-align-content: stretch;'
            +' -ms-flex-line-pack: stretch;'
            +' align-items: stretch;'
            +' }'
          +'.hboxNoStretch { '
              +' margin:0px;'
              +' display: -webkit-flex;'
              +' display: -ms-flexbox;'
              +' display: -flex;'
              +' -webkit-flex-direction: row;'
              +' -ms-flex-direction: row;'
              +' flex-direction: row;'
              +' -webkit-align-content: flex-start;'
              +' -ms-flex-line-pack: start;'
              +' align-items: flex-start;'
              +' }'

          +'.vbox { '
              +' margin:0px;'
              +' display: -webkit-flex;'
              +' display: -ms-flexbox;'
              +' display: flex;'
              +' -webkit-flex-direction: column;'
              +' -ms-flex-direction: column;'
              +' flex-direction: column;'
              +' -webkit-align-content: stretch;'
              +' -ms-flex-line-pack: stretch;'
              +' align-items: stretch;'
              +' }'
            +'.vboxNoStretch { '
               +' margin:0px;'
               +' display: -webkit-flex;'
               +' display: -ms-flexbox;'
               +' display: flex;'
               +' -webkit-flex-direction: column;'
               +' -ms-flex-direction: column;'
               +' flex-direction: column;'
               +' -webkit-align-content: flex-start;'
               +' -ms-flex-line-pack: start;'
               +' align-items: flex-start;'
               +' }'


        +'.AlignmentCentre {display: flex;'
           +'align-items: center;'
           +'justify-content: center;}'
        +'.AlignmentRight {display:flex;'
          +'align-items: flex-e'+'nd;'
          +'justify-content: flex-e'+'nd;}'
        +'.AlignmentLeft {display:flex;'
          +'align-items: flex-start;'
          +'justify-content: flex-start;}'
        +'.AlignmentTop {display:flex;'
          +'align-items: flex-start;'
          +'justify-content: flex-start;}'
        +'.AlignmentBottom {display:flex;'
          +'align-items: flex-e'+'nd;'
          +'justify-content: flex-e'+'nd;}'

        +'  input {'
                +' line-height: 20px;'
             +'}'


           +' </style>';

      //----------------------------- now append the style declarations to the head of the HTML page
      document.head.innerHTML = document.head.innerHTML+HandVBoxStyleString;
     }
     end;

end;

procedure setPasteCutAreaSize();
var localscreentype:boolean;
  SystemRootNameVar:string;
begin
  localscreentype:= PortraitMode;
  SystemRootNameVar:=SystemRootName;

asm
try{
  // add in the paste area in a way that does not upset android / cordova
  var rd= document.getElementById(SystemRootNameVar);

  rd.insertAdjacentHTML('afterend', '<textarea id = "cuttextarea"  spellcheck="false" wrap="false" ></textarea>');

  if (localscreentype==true){setLargeFont();};

  var cutTextarea = document.getElementById("cuttextarea");
  if (cutTextarea==null) {alert('cannot find element cuttextarea');}
  cutTextarea.setAttribute('tabindex', '-1'); // so it can be focused
  cutTextarea.contenteditable=true;


  } catch(err) { alert(err.message+'  in HTMLUtils.setPasteCutAreaSize'); }
end;

end;

function CheckMatch(Instring,teststring:string;startpos:integer):boolean;
var i:integer;
  match:boolean;
  temp1,temp2:string;
begin
   match:=true;
   for i:= 1 to Length(testString) do
   begin
      if (i+startpos-1)<= Length(Instring) then
      begin
        temp1:=Instring[i+startpos-1];
        temp2:=teststring[i] ;
        if temp1<>temp2
        then match:=false;
      end
      else match:=false;
   end;
   result := match;
end;

procedure PrepareHeightWidthHTML(var HW,StrVal,StyleVal:string);
var
  hw1:string;
begin
  // prepare height and width for html
 // ht:=NewNode.getAttribute('myHeight',true).AttribValue;
 // wd:=NewNode.getAttribute('myWidth',true).AttribValue;
  if (FoundString(StrVal,'px')>0) or (FoundString(StrVal,'%')>0) then
    hw1:=StrVal
  else
    hw1:=StrVal+'px';
  StrVal:=hw1;

  if HW='H' then
  begin
    StyleVal:= 'height:'+hw1+';' ;
    if (FoundString(hw1,'px')>0) then
      StyleVal := StyleVal + 'max-height:'+hw1+';';
  end;
  if HW='W' then
  begin
    StyleVal:= 'width:'+hw1+';' ;
    if (FoundString(hw1,'px')>0) then
      StyleVal := StyleVal + 'max-width:'+hw1+';';
  end;
end;
(*
procedure PrepareHeightWidthHTML(NewNode:TDataNode; var Ht,Wd,StyleHt,StyleWd:string);
var
  hh,ww:string;
begin
  // prepare height and width for html
  ht:=NewNode.getAttribute('myHeight',true).AttribValue;
  wd:=NewNode.getAttribute('myWidth',true).AttribValue;
  if Ht<>'' then
  begin
    if (FoundString(ht,'px')>0) or (FoundString(ht,'%')>0) then
      Hh:=Ht
    else
      Hh:=Ht+'px';
    Ht:=Hh;
    StyleHt:= 'height:'+Ht+';' ;
    if (FoundString(ht,'px')>0) then
      StyleHt := StyleHt + 'max-height:'+Ht+';';
  end;
  if Wd<>'' then
  begin
    if (FoundString(wd,'px')>0) or (FoundString(wd,'%')>0) then
      WW:=wd
    else
      WW:=Wd+'px';
    Wd:=WW;
    StyleWd:= 'width:'+Wd+';' ;
    if (FoundString(wd,'px')>0) then
      StyleWd := StyleWd + 'max-width:'+Wd+';';
  end;
end;
*)
procedure SetHeightWidthHTML(MyNode:TDataNode; ob:TObject; HW,AttrValue:string);
var
  hwStr:string;
  StyleHW,StyleHW0:string;
begin
  PrepareHeightWidthHTML(HW,AttrValue,StyleHW0);
  hwStr:=AttrValue;
  StyleHW:=StyleHW0;
  asm
    if (ob!=null) {
      if (HW=='H') {
        ob.style.height=hwStr; }
      else {
        ob.style.width=hwStr;  }
    }
  end;

end;

function DeleteScreenObject(MyNode:TDataNode):string;
var
    ObjName:string;
begin
   ObjName:=MyNode.NodeName;

  asm
    try{
    var ThisObject = document.getElementById(ObjName);
    if (ThisObject!=null) {
       ThisObject.parentNode.removeChild(ThisObject);
      }
    }catch(err) { alert(err.message+' in HTMLUtils.DeleteScreenObject');}
  end;

  NilScreenObject(MyNode);

end;

procedure ClearAllScreenObjects;
var
    SystemRootNameVar:string;
begin
  SystemRootNameVar:=SystemRootName;
  showmessage('ClearAllScreenObjects');
  asm
try{
       var self=document.getElementById(SystemRootNameVar);
       while (self.firstChild)
         { self.removeChild(self.firstChild);}
    }catch(err) { alert(err.message+'  in HTMLUtils.ClearAllScreenObjects');}

end;
//  showmessage('ClearAllScreenObjects done');
end;

procedure AddObjectToParentObject(ParentNode:TDataNode;ParentId,myId:String;position:integer;HTMLString:string);
var
  pos:integer;
  mysib:TDataNode;
begin
     pos:=position;
     if pos > -1 then
     begin
       // NB.  System node for the new element has already been inserted at the relevant position
       if length( ParentNode.ChildNodes)>pos+1 then  // eg. 3 children.  position=1. sib is 2.
       begin
         //ShowMessage('looking for sib at '+ inttostr(position+1));
         mysib:= ParentNode.ChildNodes[pos+1] ;
         //ShowMessage('sib is '+ mysib.NodeName);
         if mysib.ScreenObject=nil then
           pos:=-1;
       end
       else
       begin
         if pos > length( ParentNode.ChildNodes)-1 then
           ShowMessage('cannot insert under '+ParentNode.NodeName+ ' at position '+IntToStr(pos)+'. reverted to end');
         pos:=-1;
       end;
     end;

     asm
       try {
         var myParent=document.getElementById(ParentId);
           // Insert the new container under the given parent, at the correct sibling position
           if (pos==-1)  {
           myParent.insertAdjacentHTML('beforeend', HTMLString);
           }
           else if ( pos==0) {
           myParent.insertAdjacentHTML('afterbegin', HTMLString);
           }
           else {
             var mySibling=document.getElementById(mysib.NodeName);
             if (mySibling!=null) {
               mySibling.insertAdjacentHTML('beforebegin', HTMLString);
             }
             else {
               // insert msg here.... (1)
               var str=sibname;
               alert(str);
               myParent.insertAdjacentHTML('beforeend', HTMLString);
             }
             }
        } catch(err) { alert(err.message+'  in HTMLUtils.AddObjectToParentObject');}
      end;

           (*  (1) *************** this causes compile error : String exceeds end of line  £££££££why????
                str='sibling ' + str + ' not found. inserting at end';
                *)

end;

function ScreenObjectInnerComponent(SystemNode:TDataNode):TObject;
var
  innername:string;
begin
   begin
     innername:=SystemNode.NodeName+'Contents';
     asm
       Result=document.getElementById(innername);
     end;
     if Result=nil then
     asm
       Result=document.getElementById(SystemNode.NodeName);
     end;
     if Result=nil then
       ShowMessage('object '+ SystemNode.NodeName + ' not found in HTMLUtils.ScreenObjectInnerComponent') ;
   end;
end;


function CreateWrapperHtml(NewNode,ParentNode:TDataNode;ClassName,ScreenObjectName,ScreenObjectType:string):string;
var
  Border,BgColor:String;
begin
  Border:=NewNode.GetAttribute('Border',true).AttribValue;
  BgColor:=NewNode.GetAttribute('BgColor',true).AttribValue;

asm
try{

    // note tabindex=0 allows a div to be focused.  Only the focused element will listen to keyboard events.

    var Parent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);

    var ClassString =' class="';
    if (Border=='True') {ClassString = ClassString+' normal-border '+ClassName;}
    else  {ClassString = ClassString+' no-border '+ClassName;}

    // for vbox containers to centre their children we must mark all the children as centred
    if (Parent.classList.contains("vbox")&&(Parent.classList.contains("Centre")))
      {ClassString= ClassString +  ' hCentre '};
    ClassString= ClassString + '" ';

    var ComponentHTML='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var componentClick="'Click'";
    var blankParam="''";

    var WrapperStyle = ' background-color:'+BgColor+'; white-space:nowrap; ';

    var FullHTMLString='<div '+ClassString+' style="'+WrapperStyle+'" tabindex="0" position = "relative" id='+ScreenObjectName+
                ' onclick="event.stopPropagation(); pas.Events.handleEvent('+componentClick+','+NodeIDString+', this.value,'+blankParam+');" '+
                   ' </div> ';

  }catch(err) { alert(err.message+'  in HTMLUtils.CreateWrapperHtml');}

  return FullHTMLString;
end;

end;

function CreateWrapperDiv(MyNode,ParentNode:TDataNode;ClassName,ScreenObjectName,ScreenObjectType:string;position:integer ):TObject;
var
  bdr:string;
  ShowBorder:boolean;
begin
  Bdr:= MyNode.getAttribute('Border',true).AttribValue;
  if Bdr<>'' then
    ShowBorder:=MyStrToBool(Bdr)
  else
    Showborder:=false;

  asm
    try {
       var MyParent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
       var HTMLImplementation = pas.HTMLUtils.CreateWrapperHtml(MyNode,ParentNode,'UI',ScreenObjectName,ScreenObjectType);
       pas.HTMLUtils.AddObjectToParentObject(ParentNode,MyParent.id,ScreenObjectName,position,HTMLImplementation);
       var wrapper=document.getElementById(ScreenObjectName);
       if (wrapper.style.overflow!='scroll')
       {
          wrapper.style.overflow = 'hidden';
       }
       if (ShowBorder==true) {
          wrapper.classList.add("normal-border");
       }

       return wrapper;

   } catch(err) { alert(err.message+'  in HTMLUtils.CreateWrapperDiv');}
  end;

end;

procedure UnHighlight(ObjID:string; HadBorder:boolean);
begin

    asm
      try{
     // alert('unhighlight '+ObjID);
      var ob=document.getElementById(ObjID)
      if (ob!=null) {
        ob.classList.remove("highlight-border");

        if (HadBorder=='True') {
           ob.classList.add("normal-border");
           }
      }
      }catch(err) { alert(ObjID+': '+err.message+'  in HTMLUtils.UnHighlight'); }
    end;

end;

procedure Highlight(ObjID:string);
begin

    asm
    try{
    //alert('Highlight '+ObjID);
    var ob=document.getElementById(ObjID);
    if (ob!=null) {
      ob.classList.remove("normal-border");
      ob.classList.remove("no-border");
      ob.classList.add("highlight-border");
    }
    }catch(err) { alert(err.message+'  in HTMLUtils.Highlight'); }
    end;

end;

procedure ShowHideSelectedBorder(myNode:TDataNode;showborder:Boolean);
var
  HadBorder:Boolean;
begin
  if myNode.GetAttribute('Border',true).AttribValue <> '' then
     HadBorder:=myStrToBool(myNode.GetAttribute('Border',true).AttribValue)
  else
     HadBorder:=false;

  asm
  //alert('set showborder to '+showborder);
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    if (showborder==true) {
       pas.HTMLUtils.Highlight(ob.id);
    }
    else {
       pas.HTMLUtils.UnHighlight(ob.id, HadBorder);
    }      }
  end;

end;

function GetDataNodeFromTreeNode(nodeID:string):TDataNode;
var
  bits:TStringList;
begin
  bits:=stringsplit(nodeID,'Contents');
  result:=FindDataNodeById(SystemNodeTree,bits[0],true);
end;


{$endif}

begin
end.

