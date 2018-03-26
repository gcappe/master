unit XHyperLink;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs,
  ExtCtrls,TypInfo, Propedits,RTTICtrls,LCLIntf,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}
{$ifndef JScript}
type
  TXHyperLink = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;
 //   myLink:TLabel;

    procedure SetMyEventTypes;

    procedure Labelclick(Sender:TObject);

    function GetLabelCaption:string;
    function GetURL:string;

    procedure SetLabelCaption(AValue:string);
    procedure SetURL(AValue:string);

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
    property URL: String read GetURL write SetURL;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
  end;


  procedure Register;


{$else}
type
  TXHyperLink = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetLabelCaption:string;
    function GetURL:string;

    procedure SetLabelCaption(AValue:string);
    procedure SetURL(AValue:string);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property LabelCaption: String read GetLabelCaption write SetLabelCaption;
    property URL: String read GetURL write SetURL;

  end;
{$endif}


implementation
const MyNodeType='TXHyperlink';

procedure TXHyperLink.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xhyperlink_icon.lrs}
  RegisterComponents('Misc',[TXHyperLink]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXHyperLink, 'BgColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'myHeight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'myWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'LabelPos', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXHyperLink, 'LabelText', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXHyperLink, 'Link', THiddenPropertyEditor);
end;

constructor TXHyperLink.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXHyperLink.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXHyperLink.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TLabel.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.OnClick:=@self.LabelClick;
  myControl.font.color := clBlue;
  myControl.font.Underline:=true;

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

//  self.AutoSize:=true;
  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  Hint:='';
  LabelCaption:='BBC News';
  URL:='http://www.bbc.co.uk/news';

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXHyperLink;
  NewNode:TDataNode;
begin
  NewWidget:=TXHyperLink.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;


procedure TXHyperLink.LabelClick(Sender: TObject) ;
var
  LabelItem : TLabel ;
  myURL:string;
begin
  if not (csDesigning in componentState) then
      begin
         LabelItem := TLabel (sender) ;
         myURL:=LabelItem.AccessibleDescription  ;
         OpenURL(myURL);
         CallHandleEvent('Click',LabelItem.AccessibleDescription,Sender);

      end;
end;

{$else}

constructor TXHyperLink.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  LabelCaption:='BBC News';
  URL:='http://www.bbc.co.uk/news';
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  LabelCaption,URL:string;
  OnClickString:String;
begin
  LabelCaption:= MyNode.getAttribute('LabelCaption',true).AttribValue;
  URL:= MyNode.getAttribute('URL',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''','''+URL+''',''''); " ';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';

    var HTMLString='';
    var MyObjectName=ScreenObjectName+'Contents';

    HTMLString = '<a id='+MyObjectName+' href="'+URL+'" target="_blank" '+
                         OnClickString +
                         ' style="display: inline-block;"  >'+LabelCaption+'</a> ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    //MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XHyperLink.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXHyperLink.Create(MyForm,NodeName));
end;


{$endif}

function TXHyperLink.GetLabelCaption:String;
begin
  result:=MyNode.getAttribute('LabelCaption',true).AttribValue;
end;
function TXHyperLink.GetURL:String;
begin
  result:=MyNode.getAttribute('URL',true).AttribValue;
end;

procedure TXHyperLink.SetLabelCaption(AValue:string);
begin
  myNode.SetAttributeValue('LabelCaption',AValue);
  {$ifndef JScript}
  if myControl<>nil then
     TLabel(myControl).Caption:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.innerHTML=AValue;   }
  end;
  {$endif}
end;

  procedure TXHyperLink.SetURL(AValue:string);
  begin
    myNode.SetAttributeValue('URL',AValue);
    {$ifndef JScript}
    if myControl<>nil then
       TLabel(myControl).AccessibleDescription:=AValue;
    {$else}
    asm
      var ob = document.getElementById(this.NodeName+'Contents');
      if (ob!=null) {
         ob.href=AValue;   }
    end;
    {$endif}
  end;

begin
{$ifndef JScript}
AddNodeFuncLookup(MyNodeType,@CreateWidget);
SuppressDesignerProperty(TXHyperLink,'BgColor');
{$else}
AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
SuppressDesignerProperty('TXHyperLink','BgColor');
{$endif}


end.
