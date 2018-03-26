unit XMemo;
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
  TXMemo = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
 //   myMemo:TMemo;

    procedure SetMyEventTypes;

    procedure MemoClick(Sender:TObject);
    procedure MemoChange(Sender: TObject);

    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetMemoWidth:string;
    function GetMemoHeight:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetMemoWidth(AValue:string);
    procedure SetMemoHeight(AValue:string);

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
    property MemoHeight: String read GetMemoHeight write SetMemoHeight;
    property MemoWidth: String read GetMemoWidth write SetMemoWidth;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleChange: TEventHandler read FHandleChange write FHandleChange;
  end;


  procedure Register;


{$else}
type
  TXMemo = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetItemValue:string;
    function GetReadOnly:Boolean;
    function GetMemoWidth:string;
    function GetMemoHeight:string;

    procedure SetItemValue(AValue:string);
    procedure SetReadOnly(AValue:Boolean);
    procedure SetMemoWidth(AValue:string);
    procedure SetMemoHeight(AValue:string);

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
    property MemoHeight: String read GetMemoHeight write SetMemoHeight;
    property MemoWidth: String read GetMemoWidth write SetMemoWidth;

  end;
{$endif}

implementation

const MyNodeType='TXMemo';

procedure TXMemo.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}

procedure Register;
begin
  {$I xmemo_icon.lrs}
  RegisterComponents('Misc',[TXMemo]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXMemo, 'BgColor', THiddenPropertyEditor);

  // suppress some of the Link properties
  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXMemo.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXMemo.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXMemo.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TMemo.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];
  TMemo(myControl).OnEditingDone:=@self.myeditingDone;

  TMemo(myControl).OnExit:=@self.MemoChange;
  myControl.OnClick:=@self.MemoClick;

  TMemo(myControl).WordWrap:=true;
  TMemo(myControl).ScrollBars:=ssVertical;
//  myMemo.Align:=alClient;  // size to containing panel

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

//  self.AutoSize:=false;
  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  AddLabel(myControl);
  Hint:='';
  ItemValue:='....text....';
  ReadOnly:=False;
  MemoHeight:='100';
  MemoWidth:='100';

  LabelText:='Memo Box';
  LabelPos:='Top';


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXMemo;
  NewNode:TDataNode;
begin
  NewWidget:=TXMemo.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXMemo.MemoClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXMemo.MemoChange(Sender: TObject) ;
 var
    Memo: TMemo ;
 begin
    Memo := TMemo(sender) ;
    CallHandleEvent('Change',Memo.text,Sender);
 end;

procedure TXMemo.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (Link.Editor=nil) then exit;
  inherited  LinkLoadFromProperty(Sender);

  self.ItemValue:=Link.GetAsText;

end;

procedure TXMemo.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
  Link.SetAsText(TMemo(myControl).Text);

end;

procedure TXMemo.SetMemoWidth(AValue:string);
begin
  myNode.SetAttributeValue('MemoWidth',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'MemoWidth','MemoHeight');
end;

procedure TXMemo.SetMemoHeight(AValue:string);
begin
  myNode.SetAttributeValue('MemoHeight',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'MemoWidth','MemoHeight');
end;

{$else}

constructor TXMemo.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  LabelText:='Memo Box';
  ItemValue:='...text...';
  ReadOnly:=False;
  MemoHeight:='100';
  MemoWidth:='100';
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  ItemValue,LabelText,LabelPos:string;
  ReadOnly:Boolean;
//  Ht1,Wd1,Ht0,Wd0:String;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  ItemValue:= MyNode.getAttribute('ItemValue',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  LabelPos:= UpperCase(MyNode.getAttribute('LabelPos',true).AttribValue);
  ReadOnly:= StrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', this.value,'''');" ';
  OnChangeString:= 'onchange="pas.NodeUtils.SetInterfaceProperty('''+ScreenObjectName+''',''ItemValue'',this.value); pas.Events.handleEvent(''Change'','''+ScreenObjectName+''', this.value, ''ItemValue'');" ';
  OnPasteString:='';

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
    var Pastetypestring="'MemoPaste'";
    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';


    var MemoString ='<textarea  id='+MyObjectName+' '+
                        OnPasteString +
                        OnClickString +
                        OnChangeString +
                   //     ' style="display: inline-block;padding:1px;height:99%;width:99%;"  cols='+numcols+' rows='+numrows+'>'+
                        ' style="display: inline-block;padding:1px;"  >'+
                       ItemValue+'</textarea> ';

    if (LabelPos=='LEFT') {
     HTMLString = labelstring+MemoString;
    }
    else {
     HTMLString = MemoString+labelstring;
    }

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
   // MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XMemo.CreateXMemo');}

end;

  MyNode.ScreenObject:=MyNode;

  TXMemo(myNode).myHeight:=TXMemo(myNode).myHeight;
  TXMemo(myNode).myWidth:=TXMemo(myNode).myWidth;
  TXMemo(myNode).MemoHeight:=TXMemo(myNode).MemoHeight;
  TXMemo(myNode).MemoWidth:=TXMemo(myNode).MemoWidth;
  TXMemo(myNode).Alignment:=TXMemo(myNode).Alignment;
  TXMemo(myNode).LabelPos:=TXMemo(myNode).LabelPos;
  TXMemo(myNode).LabelText:=TXMemo(myNode).LabelText;
  TXMemo(myNode).ReadOnly:=TXMemo(myNode).ReadOnly;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXMemo.Create(MyForm,NodeName));
end;

procedure TXMemo.LinkLoadFromProperty(Sender: TObject);
begin
  inherited  LinkLoadFromProperty(Sender);
end;

procedure TXMemo.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  if Link.TIObject=nil then exit;
//  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '+self.ItemValue);

  SetStringProp(Link.TIObject,Link.TIPropertyName,self.ItemValue);
end;



procedure TXMemo.SetMemoWidth(AValue:string);
begin
  //showmessage('memo width='+AValue);
  myNode.SetAttributeValue('MemoWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXMemo.SetMemoHeight(AValue:string);
begin
  myNode.SetAttributeValue('MemoHeight',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

{$endif}


function TXMemo.GetItemValue:string;
begin
  result:=MyNode.getAttribute('ItemValue',true).AttribValue;
end;
function TXMemo.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;
function TXMemo.GetMemoHeight:string;
begin
  result:=MyNode.getAttribute('MemoHeight',true).AttribValue;
end;
function TXMemo.GetMemoWidth:string;
begin
  result:=MyNode.getAttribute('MemoWidth',true).AttribValue;
end;

procedure TXMemo.SetItemValue(AValue:string);
begin
  myNode.SetAttributeValue('ItemValue',AValue);
  {$ifndef JScript}
  TMemo(myControl).Text:=AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
       ob.value=AValue;  }
  end;
  LinkSaveToProperty(self);
  {$endif}
end;

procedure TXMemo.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TMemo(myControl).ReadOnly:=AValue;
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
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  SuppressDesignerProperty(TXMemo,'BgColor');
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  SuppressDesignerProperty('TXMemo','BgColor');
  {$endif}
end.
