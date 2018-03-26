unit XButton;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,Propedits, RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;
{$endif}

{$ifndef JScript}

type
  TXButton = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleButtonClick:TEventHandler;

    procedure buttonclick(Sender:TObject);
    procedure SetMyEventTypes;

    function GetmyCaption:string;
    function GetEnabled:Boolean;

    procedure SetmyCaption(AValue:string);
    procedure SetEnabled(AValue:Boolean);
    procedure SetmyWidth(AValue:string); override;


  protected
    { Protected declarations }
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    property ParentColor;

  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;

  published
    { Published declarations }

    // Properties defined for this class...
    property myCaption: String read GetmyCaption write SetmyCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;

    // Events to be handled
    property HandleButtonClick: TEventHandler read FHandleButtonClick write FHandleButtonClick;
  end;


  procedure Register;


{$else}
type
  TXButton = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetmyCaption:string;
    function GetEnabled:Boolean;

    procedure SetmyCaption(AValue:string);
    procedure SetEnabled(AValue:Boolean);


  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property myCaption: String read GetmyCaption write SetmyCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;

  end;
{$endif}

implementation

const MyNodeType='TXButton';

procedure TXButton.SetMyEventTypes;
begin
  MyEventTypes.Add('ButtonClick');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('Misc',[TXButton]);

  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXButton, 'Link', THiddenPropertyEditor);

end;

procedure TXButton.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

    self.BorderSpacing.Around:=glbBorderSpacing;

    myControl:=TButton.Create(self);
    myControl.Parent:=self;

    myControl.SetSubComponent(true);  // Tell the IDE the component is a sub-component
    // Make sure the embedded component can not be selected/deleted within the IDE
    myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

    TButton(myControl).OnClick:=@self.ButtonClick;
    myControl.AutoSize:=true;

    self.SetMyEventTypes;
    self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

    self.ParentColor:=true;
    // Set IsContainer false, to prevent designer dropping new child controls into this one.
    self.IsContainer:=false;



    // cycle through the getters and setters AFTER creating data node, to synchronise
    // node attributes and set defaults for published properties
    IsVisible:=true;
    Hint:='';
    MyWidth:=MyWidth;
    myCaption:='Press Me';
    Enabled:=True;
end;

constructor TXButton.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXButton.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;


procedure TXButton.ButtonClick(Sender: TObject) ;
begin
  CallHandleEvent('ButtonClick',self.myNode.NodeName,self);
end;


procedure TXButton.SetmyWidth(AValue:string);
begin
  myNode.SetAttributeValue('myWidth',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'myWidth','myHeight');
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXButton;
  NewNode:TDataNode;
begin
  NewWidget:=TXButton.Create(ParentNode.MyForm,true);
  NewNode:=NewWidget.myNode;
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

{$else}

constructor TXButton.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  myCaption:='Press Me';
  Enabled:=True;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  xItemText,xEnabled,marginString:string;
begin
//showmessage('Create Button widget');
xItemText:= MyNode.getAttribute('myCaption',true).AttribValue;
xEnabled:= MyNode.getAttribute('Enabled',true).AttribValue;
marginString := 'margin:'+glbMarginSpacing+' '
                         +glbMarginSpacing+' '
                         +glbMarginSpacing+' '
                         +glbMarginSpacing+';';

asm
  try{
  //alert('CreateWidget Button...');
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var componentClick="'ComponentClick'";
    var MyObjectName=ScreenObjectName+'Contents';

    var EnabledString = '';
    if (xEnabled=='False') { EnabledString = ' disabled ';}

    var typestring="'ButtonClick'";
    var Caption="'"+xItemText+"'";
    var blankParam="''";
    HTMLString = '<input type="button" id='+MyObjectName+' style="display: inline-block; '+
                                                                 'width:100%; '+
                                                                  marginString+'" '+
                                                                  '" '+
    'onclick="event.stopPropagation(); pas.Events.handleEvent('+typestring+','+NodeIDString+', '+NodeIDString+','+blankParam+');"'+
                        '  '+EnabledString+' value="'+xItemText+'"> ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    // MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XButton.CreateWidget');}

end;

MyNode.ScreenObject:=MyNode;
TWrapperPanel(myNode).myWidth:=TWrapperPanel(myNode).myWidth;
TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXButton.Create(MyForm,NodeName));
end;

{$endif}



function TXButton.GetmyCaption:string;
begin
  result:=myNode.GetAttribute('myCaption',true).AttribValue;
end;
function TXButton.GetEnabled:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('Enabled',true).AttribValue);
end;

procedure TXButton.SetmyCaption(AValue:string);
begin
  myNode.SetAttributeValue('myCaption',AValue);
  {$ifndef JScript}
  TButton(myControl).Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  {$endif}
end;

procedure TXButton.SetEnabled(AValue:Boolean);
begin
  myNode.SetAttributeValue('Enabled',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TButton(myControl).Enabled:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
    if (AValue==false) {ob.disabled = true}
    else {ob.disabled = false }
    }
  end;
  {$endif}
end;

begin
  {$ifndef JScript}
  {$I XButton.lrs}
  SuppressDesignerProperty(TXButton,'LabelPos');
  SuppressDesignerProperty(TXButton,'LabelText');
  SuppressDesignerProperty(TXButton,'BgColor');
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  SuppressDesignerProperty('TXButton','LabelPos');
  SuppressDesignerProperty('TXButton','LabelText');
  SuppressDesignerProperty('TXButton','BgColor');

  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}

end.


