unit XRadioBtns;
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

function CreateButtonsList(myNode:TDataNode;OptionList:String):string;
{$endif}

{$ifndef JScript}

type
  TXRadioBtns = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;

    procedure SetMyEventTypes;

    procedure RadioGroupclick(Sender:TObject);
    procedure RadioGroupChange(Sender:TObject);

    function GetReadOnly:Boolean;
    function GetItemValue:String;
    function GetOptionList:String;
    function GetCaption:String;

    procedure SetReadOnly(AValue:Boolean);
    procedure SetItemValue(AValue:String);
    procedure SetOptionList(AValue:String);
    procedure SetCaption(AValue:String);

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
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ItemValue: String read GetItemValue write SetItemValue;
    property Caption: String read GetCaption write SetCaption;
    property OptionList: String read GetOptionList write SetOptionList;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleChange: TEventHandler read FHandleChange write FHandleChange;
  end;

procedure Register;

{$else}
type
  TXRadioBtns = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetReadOnly:Boolean;
    function GetItemValue:String;
    function GetOptionList:String;
    function GetCaption:String;

    procedure SetReadOnly(AValue:Boolean);
    procedure SetItemValue(AValue:String);
    procedure SetOptionList(AValue:String);
    procedure SetCaption(AValue:String);

  protected
    { Protected declarations }
    procedure LinkLoadFromProperty(Sender: TObject);  override;
    procedure LinkSaveToProperty(Sender: TObject);  override;
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ItemValue: String read GetItemValue write SetItemValue;
    property Caption: String read GetCaption write SetCaption;
    property OptionList: String read GetOptionList write SetOptionList;

  end;
{$endif}

implementation

const MyNodeType='TXRadioBtns';

procedure TXRadioBtns.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xradiobtns_icon.lrs}
  RegisterComponents('Misc',[TXRadioBtns]);

   // inherited from TWrapperPanel, not required here
   RegisterPropertyEditor(TypeInfo(String), TXRadioBtns, 'LabelPos', THiddenPropertyEditor);
   RegisterPropertyEditor(TypeInfo(String), TXRadioBtns, 'LabelText', THiddenPropertyEditor);

   // suppress some of the link properties
   RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
   RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
   RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);

end;

constructor TXRadioBtns.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXRadioBtns.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXRadioBtns.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  myControl:=TRadioGroup.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

//  myRadioGroup.OnEditingDone:=@self.myeditingDone;
  myControl.OnClick:=@self.RadioGroupClick;
  TRadioGroup(myControl).OnSelectionChanged:=@self.RadioGroupChange;


  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  Hint:='';
  Caption:='Radio Buttons';
  OptionList:='["Option 1","Option 2","Option 3"]';
  ItemValue:='Option 1';
  ReadOnly:=False;

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXRadioBtns;
  NewNode:TDataNode;
begin
  NewWidget:=TXRadioBtns.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXRadioBtns.RadioGroupChange(Sender: TObject) ;
 var
    RadioButtonGroup : TCustomRadioGroup;
 begin
   if not (csDesigning in componentState)
   and not (csLoading in componentState) then
   begin
    RadioButtonGroup :=  TCustomRadioGroup(sender) ;
    self.ItemValue:=RadioButtonGroup.Items[RadioButtonGroup.ItemIndex];
    self.myeditingDone(self);     //implements a property link
    CallHandleEvent('Change',IntToStr(RadioButtonGroup.itemindex),Sender);
   end;
 end;

procedure TXRadioBtns.RadioGroupClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState)
  and not (csLoading in componentState) then
    CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXRadioBtns.LinkLoadFromProperty(Sender: TObject);
var
  TxtVal:string;
  NewIndex:Integer;
begin
  if Sender=nil then ;
  if (Link.Editor=nil) then exit;
  inherited  LinkLoadFromProperty(Sender);

  TxtVal:=Link.GetAsText;
  NewIndex:=TRadioGroup(myControl).Items.IndexOfName(TxtVal);

  if NewIndex>-1 then
  begin
     TRadioGroup(myControl).ItemIndex:=NewIndex;
     self.ItemValue:=TxtVal;
  end;

end;

procedure TXRadioBtns.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
 // showmessage('radiogroup. LinkSaveToProperty '+myBoolToStr(self.myCheckBox.Checked));
  Link.SetAsText(self.ItemValue);

end;
{$else}

constructor TXRadioBtns.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  Caption:='Radio Buttons';
  OptionList:='["Option 1","Option 2","Option 3"]';
  ItemValue:='Option 1';
  ReadOnly:=False;
end;

function CreateButtonsList(myNode:TDataNode;OptionList:String):string;
var
  OnChangeString, ItemValue,ReadOnly,myName,quot:string;
begin
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  myName:=myNode.NodeName;
  OnChangeString:='onchange="if (this.checked) {pas.NodeUtils.SetInterfaceProperty('''+myName+''',''ItemValue'',this.value);' +
                          'pas.Events.handleEvent(''Change'','''+myName+''',''';
  quot:='''';

  asm
  try{
    var ReadOnlyString = '';
    if (ReadOnly=='True') { ReadOnlyString = ' readonly ';}

    var HTMLString='';
    var optionlistarray=JSON.parse(OptionList);
    for (var i=0; i<optionlistarray.length; i++){
       var currentitemstring = optionlistarray[i];
       var selectedflag ='';
       if (i==ItemValue ){selectedflag = 'checked'}
       HTMLString = HTMLString +'<input type="radio"  '+selectedflag + ReadOnlyString
                               +' id="'+myName+currentitemstring+'" '
                               +' name='+myName+' '
                               + OnChangeString+i+quot+');}" '
                               +' value="'+currentitemstring+'" '
                               +'>'+currentitemstring+'<Br>';
     }
     return HTMLString;
  }
  catch(err) { alert(err.message+'  in XRadioBtns.CreateButtonsList');}
  end;

end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  myCaption,ItemValue,OptionList:string;
  OnClickString:String;
begin
  myCaption:= MyNode.getAttribute('Caption',true).AttribValue;
  OptionList:= MyNode.getAttribute('OptionList',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''','''','''');"';
  asm
    try{
 //   alert('create radiogroup widget');
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    HTMLString = '<fieldset  id='+MyObjectName+' style="display: inline-block;height:100%;width:100%;" '
                 + OnClickString
                 +' >  ';
    var Legend='<legend id='+MyObjectName+'Legend >"'+myCaption+'"</legend>';
    var Buttons=$mod.CreateButtonsList(MyNode,OptionList);
    HTMLString = HTMLString + Legend + Buttons + '</fieldset> ';

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
    //MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XRadioBtns.CreateWidget');}

end;
  MyNode.ScreenObject:=MyNode;

  TWrapperPanel(myNode).myHeight:=TWrapperPanel(myNode).myHeight;
  TWrapperPanel(myNode).myWidth:=TWrapperPanel(myNode).myWidth;
  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;
  TXRadioBtns(myNode).ItemValue:=TXRadioBtns(myNode).ItemValue;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXRadioBtns.Create(MyForm,NodeName));
end;

procedure TXRadioBtns.LinkLoadFromProperty(Sender: TObject);
begin
  inherited  LinkLoadFromProperty(Sender);
end;

procedure TXRadioBtns.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  if Link.TIObject=nil then exit;
//  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);

  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
end;

{$endif}


function TXRadioBtns.GetCaption:String;
begin
  result:=MyNode.getAttribute('Caption',true).AttribValue;
end;
function TXRadioBtns.GetItemValue:String;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXRadioBtns.GetOptionList:String;
begin
  result:=MyNode.getAttribute('OptionList',true).AttribValue;
end;
function TXRadioBtns.GetReadOnly:Boolean;
var
  tmp:string;
begin
  tmp:=MyNode.getAttribute('ReadOnly',true).AttribValue;
  if tmp='' then tmp:='False';
  result:=MyStrToBool(tmp);
end;

procedure TXRadioBtns.SetCaption(AValue:String);
begin
  myNode.SetAttributeValue('Caption',AValue);
  {$ifndef JScript}
  TRadioGroup(myControl).Caption:=AValue;
  {$else}
  asm
  //alert('setcaption');
    var ob = document.getElementById(this.NodeName+'ContentsLegend');
    if (ob!=null) {
       ob.value=AValue  }
  //alert('setcaption done');
  end;
  {$endif}
end;

procedure TXRadioBtns.SetItemValue(AValue:String);
var
  NewIndex:integer;
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  NewIndex:=TRadioGroup(myControl).Items.IndexOf(AValue);
  if NewIndex>-1 then
  begin
     TRadioGroup(myControl).ItemIndex:=NewIndex;
  end;
  {$else}
  asm
  //alert('setitemvalue to '+AValue);
    var ob = document.getElementById(this.NodeName+AValue);
    if (ob!=null) {
       ob.checked=true;  }
      //alert('setitemvalue done');
  end;
  LinkSaveToProperty(self);
  {$endif}
end;

procedure TXRadioBtns.SetOptionList(AValue:String);
var
  NewIndex:integer;
  myName:string;
begin
  myNode.SetAttributeValue('OptionList',AValue);
  {$ifndef JScript}
  TRadioGroup(myControl).items:=ListStringToStringList(AValue);
  {$else}
  myName:=self.NodeName;
  asm
    //alert('setoptionlist. AValue='+AValue);
    var ob = document.getElementById(myName+'Contents');
    if (ob!=null) {
      var Legend='<legend id='+myName+'ContentsLegend >"'+this.Caption+'"</legend>';
      var ItemValue=ob.value;
      var Buttons=$mod.CreateButtonsList(this.myNode,AValue);
      ob.innerHTML=Legend+Buttons;
    }
  end;
  {$endif}
end;

procedure TXRadioBtns.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TRadioGroup(myControl).Enabled:=not AValue;
  {$else}
  asm
  //alert('setreadonly');
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    if (AValue==True) {ob.disabled = true}
    else {ob.disabled = false }  }
     // alert('setreadonly done');
  end;
  {$endif}
end;

begin
  {$ifndef JScript}
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  SuppressDesignerProperty(TXRadioBtns,'LabelPos');
  SuppressDesignerProperty(TXRadioBtns,'LabelText');
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  SuppressDesignerProperty('TXRadioBtns','LabelPos');
  SuppressDesignerProperty('TXRadioBtns','LabelText');
  {$endif}
end.
end.
