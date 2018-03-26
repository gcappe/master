unit XVBox;
{$ifndef JScript}
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls, Types,
  WrapperPanel, NodeUtils,StringUtils, LazsUtils;
{$else}

interface
uses
  Classes, SysUtils, Types,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;
{$endif}

type
  TXVBox = class(TWrapperPanel)         // for Lazarus, descends from TCustomPanel; for JScript, descends from TDataNode
  private
    { Private declarations }
    procedure SetMyEventTypes;

    function GetBorder:Boolean;

    procedure SetBorder(AValue:Boolean);

  protected
    { Protected declarations }
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    {$endif}
  public
    { Public declarations }
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    {$else}
    constructor Create(MyForm:TForm;NodeName:String);
    {$endif}
  published
    { Published declarations }

    // Properties defined for this class...
    property Border: Boolean read GetBorder write SetBorder;
  end;

  {$ifndef JScript}
procedure Register;
{$endif}

implementation

const MyNodeType='TXVBox';

procedure TXVBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('Misc',[TXVBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXVBox, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXVBox, 'LabelText', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXVBox, 'Link', THiddenPropertyEditor);
end;

constructor TXVBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXVBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXVBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  Caption:='';
  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  self.BorderSpacing.Around:=0;

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner, IsDynamic);


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  AlignChildrenVertical:=true;
  Hint:='';
  BgColor:=clWhite;
  MyWidth:='';
  myHeight:='';
  Border:=True;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXVBox;
  NewNode:TDataNode;
begin
  NewWidget:=TXVBox.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

{$else}
constructor TXVBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;

  BgColor:='#FFFFFF';
  MyWidth:='';
  myHeight:='';
  Border:=True;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  ShowBorder:boolean;
  myObj:TXVBox;

begin

asm
try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

    var HTMLString='';
    var CentreString ='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var componentClick="'ComponentClick'";
    var MyObjectName=ScreenObjectName+'Contents';


    // for vbox containers to centre their children we mark the parent for info
    CentreString = 'class="vbox"';
    var blankParam="''";

    HTMLString = '<div  id="'+MyObjectName+'" '+CentreString+
                   //' style="'+Wd1+Ht1+'"'+
                   ' style="height:100%;width:100%;"'+
                   ' onclick="event.stopPropagation();pas.Events.handleEvent('+componentClick+','+NodeIDString+', this.value,'+blankParam+'); " '+
                   '></div>  ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    //MyNode.ScreenObject=wrapper;
}catch(err) { alert(err.message+'  in XVBox.CreateVHBox');}
end;

MyNode.ScreenObject:=MyNode;

TWrapperPanel(myNode).myHeight:=TWrapperPanel(myNode).myHeight;
TWrapperPanel(myNode).myWidth:=TWrapperPanel(myNode).myWidth;
TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;Nodename:String):TObject;
begin
result:=TObject(TXVBox.Create(MyForm,NodeName));
end;

{$endif}
function TXVBox.GetBorder:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('Border',true).AttribValue);
end;


procedure TXVBox.SetBorder(AValue:Boolean);
begin
  myNode.SetAttributeValue('Border',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  if AValue=true then
  begin
    self.BevelInner:=bvLowered;
    self.BevelOuter:=bvRaised;
    self.BevelWidth:=1;
  end
  else
  begin
    self.BevelInner:=bvNone;
    self.BevelOuter:=bvNone;
    self.BorderStyle := bsNone;
    self.BorderWidth:=0;
  end;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    if (AValue==true) {
           ob.classList.add("normal-border");
    }
    else {
           ob.classList.remove("normal-border");
    }  }
  end;
  {$endif}
end;

begin
{$ifndef JScript}
{$I XVBox.lrs}
AddNodeFuncLookup(MyNodeType,@CreateWidget);
{$else}
AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
{$endif}
end.


