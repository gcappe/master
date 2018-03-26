unit XHBox;
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

{$ifndef JScript}
type
  TXHBox = class(TWrapperPanel)       // descends from TCustomPanel>TWrapperPanel
  private
    { Private declarations }
   procedure SetMyEventTypes;

   function GetBorder:Boolean;
    procedure SetBorder(AValue:Boolean);
  protected
    { Protected declarations }
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  public
    { Public declarations }
   constructor Create(TheOwner: TComponent); override;
   constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
  published
    { Published declarations }

    // Properties defined for this class...
    property Border: Boolean read GetBorder write SetBorder;
  end;
  {$else}
  type
    TXHBox = class(TWrapperPanel)     // descends from TObject>TWrapperPanel
    private
      { Private declarations }
     procedure SetMyEventTypes;
      function GetBorder:Boolean;

      procedure SetBorder(AValue:Boolean);
   protected
      { Protected declarations }
    public
      { Public declarations }
     constructor Create(MyForm:TForm;NodeName:String);

    published
      { Published declarations }
      property Border: Boolean read GetBorder write SetBorder;
    end;
  {$endif}

  {$ifndef JScript}
procedure Register;
{$endif}

implementation

const MyNodeType='TXHBox';

procedure TXHBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('Misc',[TXHBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXHBox, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHBox, 'LabelText', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXHBox, 'Link', THiddenPropertyEditor);
end;

constructor TXHBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXHBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXHBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var

  MyAttributes:TNodeAttributesArray;
begin
  Caption:='';
  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  self.BorderSpacing.Around:=0;

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  Hint:='';
  BgColor:=clWhite;

  AlignChildrenVertical:=false;
  MyWidth:='';
  myHeight:='';
  Border:=True;
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXHBox;
  NewNode:TDataNode;
begin
  NewWidget:=TXHBox.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

{$else}
constructor TXHBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;

  AlignChildrenVertical:=false;

  BgColor:='#FFFFFF';
  MyWidth:='';
  myHeight:='';
  Border:=True;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  ShowBorder:boolean;
  Bdr:string;
    //Ht1,Wd1,Ht0,Wd0:String;
begin
  //showmessage('create hbox widget');
  Bdr:= MyNode.getAttribute('Border',true).AttribValue;
  if Bdr<>'' then
    ShowBorder:=MyStrToBool(Bdr)
  else
    Showborder:=false;

  asm
    try{

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var componentClick="'ComponentClick'";
    var MyObjectName=ScreenObjectName+'Contents';
    var blankParam="''";

    HTMLString = '<div  id="'+MyObjectName+'" class="hbox" '+
                       //'style="'+Wd1+Ht1+'"'+
                       'style="height:100%;width:100%"'+
                       ' onclick="event.stopPropagation();pas.Events.handleEvent('+componentClick+','+NodeIDString+', this.value,'+blankParam+'); " '+
                        '></div>  ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

    }catch(err) { alert(err.message+'  in XHBox.CreateHBox');}
  end;

  MyNode.ScreenObject:=MyNode;

  TWrapperPanel(myNode).myHeight:=TWrapperPanel(myNode).myHeight;
  TWrapperPanel(myNode).myWidth:=TWrapperPanel(myNode).myWidth;
  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXHBox.Create(MyForm,NodeName));
end;

{$endif}


function TXHBox.GetBorder:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('Border',true).AttribValue);
end;


procedure TXHBox.SetBorder(AValue:Boolean);
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
    if (AValue==true ) {
       ob.classList.add("normal-border");
    }
    else {
       ob.classList.remove("normal-border");
    } }
  end;
  {$endif}
end;

begin
  {$ifndef JScript}
  {$I XHBox.lrs}
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.


