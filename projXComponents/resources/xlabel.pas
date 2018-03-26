unit XLabel;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface
                           //to do  html....
uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,TypInfo, Propedits,RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}
{$ifndef JScript}

type
  TXLabel = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;

    procedure SetMyEventTypes;

    procedure Labelclick(Sender:TObject);

    function GetLabelCaption:string;

    procedure SetLabelCaption(AValue:string);

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
    property LabelCaption: String read GetLabelCaption write SetLabelCaption;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
  end;


  procedure Register;


{$else}
type
  TXLabel = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetLabelCaption:string;

    procedure SetLabelCaption(AValue:string);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property LabelCaption: String read GetLabelCaption write SetLabelCaption;

  end;
{$endif}


implementation

const MyNodeType='TXLabel';

procedure TXLabel.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure register;
begin
  {$I xlabel_icon.lrs}
  RegisterComponents('Misc',[TXLabel]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXLabel, 'BgColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXLabel, 'myHeight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXLabel, 'myWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXLabel, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXLabel, 'LabelText', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXLabel, 'Link', THiddenPropertyEditor);
end;

constructor TXLabel.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXLabel.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXLabel.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TLabel.Create(self);
  myControl.Parent:=self;

  myControl.AutoSize:=true;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
//  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.LabelClick;

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  Hint:='';
  LabelCaption:='...Label...';


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXLabel;
  NewNode:TDataNode;
begin
  NewWidget:=TXLabel.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXLabel.LabelClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

{$else}

constructor TXLabel.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  LabelCaption:='...Label...';
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  LabelText:string;
  OnClickString:String;
  marginString:string;
begin
  LabelText:= MyNode.getAttribute('LabelCaption',true).AttribValue;
  marginString := 'margin:'+glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+' '
                           +glbMarginSpacing+';';
  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', this.value,'''');" ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';

    var MyObjectName=ScreenObjectName+'Contents';

    var HTMLString = ' <label id='+MyObjectName+' '+
                   OnClickString +
                 ' style="display: inline-block;'+marginString+'"  >'
                 +LabelText+'</label> ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    //MyNode.ScreenObject=wrapper;
    }
    catch(err) { alert(err.message+'  in XLabel.CreateWidget');}

  end;

  MyNode.ScreenObject:=MyNode;

  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXLabel.Create(MyForm,NodeName));
end;


{$endif}

function TXLabel.GetLabelCaption:String;
begin
  result:=MyNode.getAttribute('LabelCaption',true).AttribValue;
end;
procedure TXLabel.SetLabelCaption(AValue:string);
begin
  myNode.SetAttributeValue('LabelCaption',AValue);
  {$ifndef JScript}
  if myControl<>nil then
  begin
     TLabel(myControl).Caption:=AValue;
     //TLabel(myControl).Constraints.MinWidth:= TLabel(myControl).Canvas.TextWidth(AValue);
  end;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
    //alert('set labelCaption to '+AValue);
       ob.innerHTML=AValue;   }
  end;
  {$endif}

end;

begin
{$ifndef JScript}
AddNodeFuncLookup(MyNodeType,@CreateWidget);
{$else}
AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
{$endif}

end.
