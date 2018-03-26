unit XEditBox;
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
  TXEditBox = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;

    procedure SetMyEventTypes;

    procedure EditBoxclick(Sender:TObject);
    procedure EditBoxChange(Sender: TObject);

    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetBoxWidth:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetBoxWidth(AValue:string);

  protected
    { Protected declarations }
    procedure LinkLoadFromProperty(Sender: TObject);  override;
    procedure LinkSaveToProperty(Sender: TObject);  override;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    property ParentColor;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;

  published
    { Published declarations }

    // Properties defined for this class...
    property ItemValue: String read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property BoxWidth: String read GetBoxWidth write SetBoxWidth;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleChange: TEventHandler read FHandleChange write FHandleChange;
  end;


  procedure Register;


{$else}
type
  TXEditBox = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetBoxWidth:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetBoxWidth(AValue:string);

  protected
    { Protected declarations }
    procedure LinkLoadFromProperty(Sender: TObject);  override;
    procedure LinkSaveToProperty(Sender: TObject);  override;
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property ItemValue: String read GetItemValue write SetItemValue;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property BoxWidth: String read GetBoxWidth write SetBoxWidth;

  end;
{$endif}

implementation

const MyNodeType='TXEditBox';

procedure TXEditBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
  {$ifdef JScript}
  MyEventTypes.Add('EditBoxPaste');
  {$endif}
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('Misc',[TXEditBox]);

  // suppress some of the Link properties
  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXEditBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXEditBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXEditBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TEdit.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];


  TEdit(myControl).OnEditingDone:=@self.myeditingDone;

  TEdit(myControl).OnExit:=@self.EditBoxChange;
  TEdit(myControl).OnClick:=@self.EditBoxClick;
  //TEdit(myControl).on

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  AddLabel(myControl);
  Hint:='';
  BoxWidth:='200';
  ItemValue:='';
  ReadOnly:=False;
  LabelText:='Edit Box';
  LabelPos:='Right';

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXEditBox;
  NewNode:TDataNode;
begin
  NewWidget:=TXEditBox.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;


procedure TXEditBox.EditBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXEditBox.EditBoxChange(Sender: TObject) ;
 var
    EditBox: TEdit ;
 begin
    EditBox := TEdit(sender) ;
    self.ItemValue:=EditBox.text;
    CallHandleEvent('Change',EditBox.text,Sender);
 end;


procedure TXEditBox.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (Link.Editor=nil) then exit;
  inherited  LinkLoadFromProperty(Sender);

  self.ItemValue:=Link.GetAsText;

end;

procedure TXEditBox.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  Link.SetAsText(TEdit(myControl).Text);

end;

procedure TXEditBox.SetBoxWidth(AValue:string);
begin
  myNode.SetAttributeValue('BoxWidth',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'BoxWidth','myHeight');
end;


{$else}

constructor TXEditBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  BoxWidth:='200';
  LabelText:='Edit Box';
  LabelPos:='Right';
  ItemValue:='';
  ReadOnly:=False;
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  ItemValue,LabelText,LabelPos:string;
  ReadOnly:Boolean;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', this.value,'''');" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''',''ItemValue'',this.value); pas.Events.handleEvent(''Change'','''+ScreenObjectName+''', this.value, ''ItemValue'');" ';
  OnPasteString:= 'onpaste="pas.Events.handleEvent(''EditBoxPaste'','''+ScreenObjectName+''', this.value,'''');" ';
  //OnPasteString:='';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var componentClick="'Click'";
    var MyObjectName=ScreenObjectName+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly==true) { ReadOnlyString = ' readonly ';}

    var inputtext= ItemValue;
    var Pastetypestring="'EditBoxPaste'";
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    var EBoxString = '<input type="text"  id='+MyObjectName+' ' +
                          OnPasteString +
                          OnClickString +
                          OnChangeString +
                 ' style="display: inline-block; '+
                 '" value="'+inputtext+'"'+ReadOnlyString+'>' ;

    HTMLString = labelstring+EBoxString;

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    //MyNode.ScreenObject=wrapper;

    // fix the height for an edit box to one line-height...
    var ob=document.getElementById(MyObjectName);
    var obStyle = window.getComputedStyle(ob);
    ob.style.maxHeight = obStyle.getPropertyValue('line-height');
    //alert('maxHeight='+ob.style.maxHeight);
  }
  catch(err) { alert(err.message+'  in XEditBox.CreateXEditBox');}

end;

  MyNode.ScreenObject:=MyNode;

  // now that we have a datanode and a widget, cycle attribute settings
  TXEditBox(myNode).BoxWidth:=TXEditBox(myNode).BoxWidth;
  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).LabelPos:=TWrapperPanel(myNode).LabelPos;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXEditBox.Create(MyForm,NodeName));
end;

procedure TXEditBox.LinkLoadFromProperty(Sender: TObject);
begin
  inherited  LinkLoadFromProperty(Sender);
end;

procedure TXEditBox.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  if Link.TIObject=nil then exit;
//  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);

  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
end;


procedure TXEditBox.SetBoxWidth(AValue:string);
begin
  //showmessage('memo width='+AValue);
  myNode.SetAttributeValue('BoxWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;


{$endif}


function TXEditBox.GetBoxWidth:string;
begin
  result:=MyNode.getAttribute('BoxWidth',true).AttribValue;
end;
function TXEditBox.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXEditBox.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXEditBox.SetItemValue(AValue:string);
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  TLabeledEdit(myControl).Text:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  LinkSaveToProperty(self);
  {$endif}
end;

procedure TXEditBox.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TLabeledEdit(myControl).ReadOnly:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.readOnly = AValue  }
  end;
  {$endif}
end;

begin
  {$ifndef JScript}
  {$I XEditBox.lrs}
  SuppressDesignerProperty(TXEditBox,'BgColor');
  SuppressDesignerProperty(TXEditBox,'myHeight');
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  SuppressDesignerProperty('TXEditBox','BgColor');
  SuppressDesignerProperty('TXEditBox','myHeight');

  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.
