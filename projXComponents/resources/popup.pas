unit Popup;
interface

{$ifndef JScript}
uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls, Types,
  WrapperPanel, NodeUtils,StringUtils, LazsUtils,XScrollBox;
{$Else}
uses
  Classes, SysUtils, Types,NodeUtils,StringUtils,HTMLUtils;
{$endif}

procedure ShowPopup(popupID:String; modal:Boolean);

{$ifndef JScript}
var CurrentPopupWindow:TForm;
procedure ClosePopup;
{$else}

type TXPopup = class(TInterfaceObject)
      private
        fIsSelected:Boolean;
        fIsContainer:Boolean;
        function getHeight:string;
        function getWidth:string;
        function getTop:integer;
        function getLeft:integer;
        function getCaption:string;
        procedure SetHeight(AValue:string);
        procedure SetWidth(AValue:string);
        procedure SetTop(AValue:integer);
        procedure SetLeft(AValue:integer);
        procedure SetCaption(AValue:string);
      published
        property IsContainer:Boolean read FIsContainer write FIsContainer;
        property IsSelected:Boolean read fIsSelected write fIsSelected;
        property Height:String read getHeight write SetHeight;
        property Width:String read getWidth write SetWidth;
        property Top:Integer read getTop write SetTop;
        property Left:Integer read getLeft write SetLeft;
        property Caption:String read getCaption write SetCaption;
      end;

var CurrentPopupName:String;
function OpenModal(WindowId:string):string;
function CloseModal(WindowId:string):string;
function addTheModalBackground(ParentName,WindowId,EventType:string):string;
function addaModalContentItem(MyName:string):string;
function InitialisePopupStyles():string;
{$endif}


implementation

{$ifndef JScript}



function FindFormByName(const AName: string): TForm;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    Result := Screen.Forms[i];
    if (Result.Name = AName) then
      Exit;
  end;
  Result := nil;
end;

function CreateForm(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
// for dynamic creation of a popup form at runtime.
var
  TempPopupWindow:TForm;
  NewNode,PanelNode:TDataNode;
  TmpPanel:TXScrollBox;
  dummyEventsList:TStringList;
begin
  dummyEventsList:=TStringList.Create;

  TempPopupWindow :=  TForm.Create(Application);
  TempPopupWindow.name:=ScreenObjectName;

  NewNode:=CreateComponentDataNode(ScreenObjectName,'Popup', dummyEventsList, TempPopupWindow,nil,true);
  AddChildToParentNode(ParentNode,NewNode,position);

  NewNode.MyForm:=TempPopupWindow;

  // default property values
  TempPopupWindow.Height:=200;
  TempPopupWindow.Width:=400;
  TempPopupWindow.Top:= 50;
  TempPopupWindow.Left:= 50;
  TempPopupWindow.Caption:='My Title';

  result:= NewNode;
end;

procedure  ClosePopup;
begin
   if CurrentPopupWindow<>nil then
       CurrentPopupWindow.Close;
   CurrentPopupWindow:=nil;
end;


procedure ShowPopup(popupID:String; modal:Boolean);
var OldParent,tempObject:TComponent;
begin
  tempObject:=Application.FindComponent(popupID);
  if tempObject<>nil then
  begin
    CurrentPopupWindow:=TForm(tempObject);
    ClosePopup;
    CurrentPopupWindow:=TForm(tempObject);
    if modal then
      CurrentPopupWindow.showmodal
    else
      CurrentPopupWindow.Show;
    // clear the global when popup form has closed
    CurrentPopupWindow:=nil;
  end
  else
    ShowMessage('popup '+popupID+' not found');
end;

{$Else}
function TXPopup.GetCaption:string;
begin
  result:=self.GetAttribute('Caption',true).AttribValue;
end;

function TXPopup.GetHeight:string;
begin
  result:=self.GetAttribute('Height',true).AttribValue;
end;

function TXPopup.GetWidth:string;
begin
  result:=self.GetAttribute('Width',true).AttribValue;
end;

function TXPopup.GetTop:integer;
var
  AttrVal:string;
begin
  //showmessage('TXPopup GetTop');
  AttrVal:=self.GetAttribute('Top',true).AttribValue;
  if AttrVal<>'' then
    result:=StrToInt(AttrVal)
  else
    result:=0;
end;

function TXPopup.GetLeft:integer;
var
  AttrVal:string;
begin
  //showmessage('TXPopup GetLeft');
  AttrVal:=self.GetAttribute('Left',true).AttribValue;
  if AttrVal<>'' then
    result:=StrToInt(AttrVal)
  else
    result:=0;
end;

procedure TXPopup.SetCaption(AValue:string);
begin
  //showmessage('TXPopup SetCaption');
  self.SetAttributeValue('Caption',AValue);
  asm
    var ob=document.getElementById(this.NodeName+'Caption');
    if (ob!=null) {
      ob.innerHTML=AValue;
    }
  end;
end;

procedure TXPopup.SetHeight(AValue:string);
begin
  //showmessage('TXPopup SetHeight');
  self.SetAttributeValue('Height',AValue);
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
    }
  end;
end;

procedure TXPopup.SetWidth(AValue:string);
begin
  //showmessage('TXPopup SetWidth');
  self.SetAttributeValue('Width',AValue);
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
    }
  end;
end;

procedure TXPopup.SetTop(AValue:integer);
var
  t:string;
begin
  //showmessage('TXPopup SetTop');
  self.SetAttributeValue('Top',inttostr(AValue));
  t:=inttostr(AValue)+'px';
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.top=t;
    }
  end;
end;

procedure TXPopup.SetLeft(AValue:integer);
    var
      l:string;
begin
  //showmessage('TXPopup SetLeft');
  self.SetAttributeValue('Left',inttostr(AValue));
  l:=inttostr(AValue)+'px';
  asm
    var ob=document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.left=l;
    }
  end;
end;


//
//-------------------------- declaration of the Styles for the Popup -----------------------------
function InitialisePopupStyles():string;
begin

  asm
  try{
    // ----------------------------------------check if the style has already been set
    var x = document.getElementsByTagName("STYLE");
    var StyleIsSet = false;
    if (x.length>0){
      for (var i=0; i<x.length; i++){
        var y= x[i].innerHTML;
        if (y.indexOf("modal-background") !=-1) { StyleIsSet =true}
      }
    }
    if (StyleIsSet == false){
       var ModalBackgroundStyleString = ''
       +'<style>'
        +'/* The Modal (background) */'
        +'.modal-background {'
            +'display: none; /* Hidden by default */'
            +'position: fixed; /* Stay in place */'
            +'z-index: 1; /* Sit on top */'
            +'padding-top: 10px; /* Location of the box */'
            +'left: 0;'
            +'top: 0;'
            +'width: 100%; /* Full width */'
            +'height: 100%; /* Full height */'
            //+'overflow: auto; /* Enable scroll if needed */'
            +'background-color: rgb(0,0,0); /* Fallback color */'
            +'background-color: rgba(0,0,0,0.3); /* Black w/ opacity */'
        +'} '
        +'.modal-content {'
            +'background-color: #FFFFFF;'
            +'position: absolute;'
            +'border: 1px solid #888800;'
        +'}'
        +'</style>';
      //----------------------------- now append the style declarations to the head of the HTML page
      document.head.innerHTML = document.head.innerHTML+ModalBackgroundStyleString;
    }
  }catch(err)  {alert('Error in Popup.InitialisePopupStyles '+ err.message);}
  end;

end;


//===========================================================================================
//-------------------------- declaration of a Modal Window -----------------------------
function addTheModalBackground(ParentName,WindowId,EventType:string):string;
var
  OnClickString:String;
begin
  if WindowId = MainForm.Name then
    EXIT;  //wrapper for main form already exists

  OnClickString:='event.target.style.display = ''none''; event.stopPropagation();';
  OnClickString:='pas.Popup.CloseModal(event.target.id); event.stopPropagation();';
  asm
  try{
  //alert('addTheModalBackground '+WindowId);
    $mod.InitialisePopupStyles()
    var HTMLString = ''
    +'<div id='+WindowId+' class="modal-background" '
    +'onclick="'+OnClickString+'">'
    +'</div>';

    //----- now append the declarations to the Parent -------------------------------------------
    var ParentItem=document.getElementById(ParentName);
    ParentItem.insertAdjacentHTML('beforeend', HTMLString);

    //alert('addTheModalBackground done');
  }catch(err) {alert('Error in Popup.addTheModalBackground '+ err.message);}
  end;

end;

//-------------------------- declaration of the Content Items-----------------------------
function addaModalContentItem(MyName:string):string;
var
  ContentName:String;
begin
  ContentName:=MyName+'Contents';
  asm
  try{
  //alert('addaModalContentItem '+ContentName);
      var HTMLString = ''
      +'  <!-- Modal content -->'
      +'  <div id="'+ContentName+'" class="modal-content" > '
      +'    <div id="'+MyName+'Caption" ></div> '
      +'  </div>';

      var ParentItem = document.getElementById(MyName);
      ParentItem.innerHTML = ParentItem.innerHTML + HTMLString;

      //alert('addaModalContentItem done');
  }catch(err){alert('Error in Popup.addaModalContentItem '+ err.message);}
  end;

end;


function OpenModal(WindowId:string):string;
begin
  if CurrentPopupName<>'' then
    CloseModal(CurrentPopupName);
  asm
  try{
     var modalwindowid= WindowId;
     //alert('open windowid='+WindowId);
      var modal = document.getElementById(modalwindowid);
     // alert('found '+modal);
      modal.style.display = 'block';
  }catch(err){alert('Error in Popup.OpenModal '+ err.message);}
  end;
  CurrentPopupName:=WindowId;
end;

function CloseModal(WindowId:string):string;
var
  UIRootNodeName:String;
begin
  UIRootNodeName:=UIRootNode.Nodename;
  asm
    var modal = document.getElementById(WindowId);
    modal.style.display = "none";
  end;
  CurrentPopupName:='';
end;

procedure ShowPopup(popupID:String; modal:Boolean);
begin
  OpenModal(popupID);
end;

function CreateinterfaceObj(MyForm:TForm;Nodename:String):TObject;
var newobj:TObject;
begin
  //showmessage('createinterfaceobj for popup '+NodeName);
  newObj:=TObject(TXPopup.Create('UI', Nodename, 'Popup', true));
  if MyForm<>nil then
    TInterfaceObject(newObj).myForm:=MyForm
  else
  begin
    // dynamically created popup needs a dummy 'form' object to hold the name
    MyForm:=TForm.Create;
    MyForm.Name:=Nodename;
    TInterfaceObject(newObj).myForm:=MyForm;
  end;
  TInterfaceObject(newObj).myNode:=TDataNode(newObj);

  // eventtypes?
  result:=newObj;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
   ParentName:string;
begin
  //showmessage('createWidget popup '+ScreenObjectName+'.  Parent is '+ParentNode.NodeName);
  //showmessage('createWidget popup.  Node type is '+MyNode.ClassName);

  ParentName:=ParentNode.NodeName;

  asm
  try{
    $mod.addTheModalBackground(ParentName,ScreenObjectName,"");
    $mod.addaModalContentItem(ScreenObjectName);

    }catch(err) { alert(err.message+' in Popup.CreateWidget');}
  end;

  MyNode.ScreenObject:=MyNode;

  if ScreenObjectName=MainForm.Name then
  begin
    TXPopup(myNode).Caption := '';
    TXPopup(myNode).Height := '100%';
    TXPopup(myNode).Width := '100%';
    TXPopup(myNode).Top := 0;
    TXPopup(myNode).Left := 0;
  end
  else
  begin
    TXPopup(myNode).Caption := TXPopup(myNode).Caption;
    TXPopup(myNode).Height := TXPopup(myNode).Height;
    TXPopup(myNode).Width := TXPopup(myNode).Width;
    TXPopup(myNode).Top := TXPopup(myNode).Top;
    TXPopup(myNode).Left := TXPopup(myNode).Left;
    TXPopup(myNode).IsContainer := true;
  end;
  result:=myNode;
  //showmessage('create popup done');
end;
{$endif}

begin
  {$ifndef JScript}
  AddNodeFuncLookup('Popup',@CreateForm);
  {$else}
  AddNodeFuncLookup('Popup',@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.

