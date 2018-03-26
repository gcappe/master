unit XCheckBox;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface
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
  TXCheckBox = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;

    procedure SetMyEventTypes;

    procedure CheckBoxclick(Sender:TObject);
    procedure CheckBoxChange(Sender:TObject);

    function GetChecked:Boolean;
    function GetReadOnly:Boolean;

    procedure SetChecked(AValue:Boolean);
    procedure SetReadOnly(AValue:Boolean);

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
    property Checked: Boolean read GetChecked write SetChecked;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
  end;

procedure Register;

{$else}
type
  TXCheckBox = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetChecked:Boolean;
    function GetReadOnly:Boolean;

    procedure SetChecked(AValue:Boolean);
    procedure SetReadOnly(AValue:Boolean);

  protected
    { Protected declarations }
    procedure LinkLoadFromProperty(Sender: TObject);  override;
    procedure LinkSaveToProperty(Sender: TObject);  override;
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property Checked: Boolean read GetChecked write SetChecked;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;

  end;
{$endif}

implementation

const MyNodeType='TXCheckBox';

procedure TXCheckBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  //MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xcheckbox_icon.lrs}
  RegisterComponents('Misc',[TXCheckBox]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(String), TXCheckBox, 'myHeight', THiddenPropertyEditor);

  // suppress some of the link properties
  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXCheckBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXCheckBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXCheckBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TCheckBox.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  myControl.Constraints.MaxHeight:=20;
  myControl.Constraints.MaxWidth:=20;

  TCheckBox(myControl).OnEditingDone:=@self.myeditingDone;
  myControl.OnClick:=@self.CheckBoxClick;
  TCheckBox(myControl).OnChange:=@self.CheckBoxChange;

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  AddLabel(myControl);
  Hint:='';
  MyWidth:='';
  Checked:=false;
  ReadOnly:=False;

  LabelText:='Check Box';
  LabelPos:='Right';


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXCheckBox;
  NewNode:TDataNode;
begin
  NewWidget:=TXCheckBox.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXCheckBox.CheckBoxChange(Sender: TObject) ;
var
  itemchecked:string;
begin
  itemchecked:=MyBooltostr(TCheckBox(Sender).checked);
 // CallHandleEvent('Change',itemchecked,Sender);

end;

procedure TXCheckBox.CheckBoxClick(Sender: TObject) ;
var
  itemchecked:string;
begin
  if not (csDesigning in componentState) then
  begin
    itemchecked:=MyBooltostr(TCheckBox(Sender).checked);
    self.Checked:=TCheckBox(Sender).checked;
    CallHandleEvent('Click',itemchecked,self);
  end;
end;
procedure TXCheckBox.LinkLoadFromProperty(Sender: TObject);
var
  TxtVal:string;
  NewValue:Boolean;
begin
  if Sender=nil then ;
  if (Link.Editor=nil) then exit;
  inherited  LinkLoadFromProperty(Sender);

  TxtVal:=Link.GetAsText;
  NewValue:= MyStrToBool(Link.GetAsText);
  if NewValue<>TCheckBox(myControl).Checked then
  begin
     TCheckBox(myControl).Checked:=NewValue;
  //   showmessage('checkbox. LinkLoadFromProperty '+TxtVal);
  end;

end;

procedure TXCheckBox.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
 // showmessage('checkbox. LinkSaveToProperty '+myBoolToStr(self.myCheckBox.Checked));
  Link.SetAsText(myBoolToStr(TCheckBox(myControl).Checked));

end;

{$else}

constructor TXCheckBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  MyWidth:='';
  LabelText:='Edit Box';
  LabelPos:='Right';
  Checked:=false;
  ReadOnly:=False;
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  Checked,ReadOnly,LabelText,LabelPos:string;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  Checked:= MyNode.getAttribute('Checked',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;

  OnClickString:='onclick="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''',''Checked'',this.checked.toString());' +
                          'event.stopPropagation(); ' +
                          'pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', this.checked.toString(),'''');"';

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';
    wrapper.style.overflow = 'hidden';
    wrapper.style.display = 'flex';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly=='True') { ReadOnlyString = ' readonly ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var Checkstring = '';
    if (Checked == 'true'){Checkstring = 'checked'};

    var CheckBoxString = '<input  type="checkbox" id='+MyObjectName+ ' '+
                       OnClickString +
                       Checkstring +
                       ' style="display:inline-block;" '+ReadOnlyString+' >' ;

    HTMLString = labelstring+CheckBoxString;

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    //MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');}

end;

  MyNode.ScreenObject:=MyNode;
  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).LabelPos:=TWrapperPanel(myNode).LabelPos;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;
result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXCheckBox.Create(MyForm,NodeName));
end;

procedure TXCheckBox.LinkLoadFromProperty(Sender: TObject);
begin
  inherited  LinkLoadFromProperty(Sender);
end;

procedure TXCheckBox.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  if Link.TIObject=nil then exit;
//  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+myBoolToStr(self.Checked));

  SetBoolProp(Link.TIObject,Link.TIPropertyName,self.Checked);
end;

{$endif}


function TXCheckBox.GetChecked:Boolean;
begin
  //showmessage('getchecked');
  result:=MyStrToBool(MyNode.getAttribute('Checked',true).AttribValue);
end;
function TXCheckBox.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

procedure TXCheckBox.SetChecked(AValue:Boolean);
begin
  myNode.SetAttributeValue('Checked',MyBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TCheckBox(myControl).Checked:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.checked=AValue;  }
  end;
  LinkSaveToProperty(self);
  {$endif}
end;

procedure TXCheckBox.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TCheckBox(myControl).Enabled:=not AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
    if (AValue==true) {ob.disabled = true}
    else {ob.disabled = false }  }
  end;
  {$endif}
end;

begin
  {$ifndef JScript}
  SuppressDesignerProperty(TXCheckBox,'BgColor');
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  SuppressDesignerProperty('TXCheckBox','BgColor');
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.
