unit WrapperPanel;
interface
{$ifndef JScript}
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Types, Dialogs,
  ProjectIntf, LazIDEIntf, PropEdits, RTTICtrls, TypInfo, LCLProc,
  StringUtils ,NodeUtils, LazsUtils, Events;
{$else}
uses
  Classes, SysUtils, Types, TypInfo, StringUtils, NodeUtils, HTMLUtils, Events;
{$endif}

{$ifndef JScript}
type TWrapperPanel=class;  //forward

type
  TAlignmentProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    //procedure Edit; override;
  end;
type
  TLabelPosProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    //procedure Edit; override;
  end;
type
  TScrollBarsProperty = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  const DotStr:String='.';


type TWrapperPanel=Class(TCustomPanel)
 private
   FSelectionBorderColor: TColor;
   FIsSelected:Boolean;
   FIsContainer:Boolean;
   FAlignChildrenVertical:Boolean;
   FLink: TXPropertyLink;
   FmyNode:TDataNode;

   function GetName:string;
   function GetIsVisible:Boolean;
   function GetmyWidth:string;
   function GetmyHeight:string;
   function GetHint:string;
   function GetBgColor:TColor;
   function GetLabelText:string;
   function GetLabelPos:String;
   function GetAlignment:String;

   procedure SetIsVisible(AValue:Boolean);
   procedure SetMyName(AValue:string);
   procedure SetIsSelected(AValue: Boolean);
   procedure SetSelectionBorderColor(AValue: TColor);
   procedure SetHint(AValue:string);
   procedure SetBgColor(AValue:TColor);
   procedure SetLabelText(AValue:string);
   procedure SetAlignment(AValue:string);

   procedure SetLink(const AValue: TXPropertyLink);
   function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean; override;

   procedure SetName(const NewName: TComponentName); override;
   procedure SetParent(NewParent: TWinControl);  override;

   procedure WrapperClick(Sender: TObject) ;
 public
    myEventTypes:TStringList;
    myControl:TControl;
    myLbl:TLabel;
    myExtension:TXDesignerExtension;
    constructor Create(TheOwner:TComponent);  override;
    constructor Create(TheOwner:TComponent;IsDynamic:Boolean); virtual;

    destructor Destroy; override;
    procedure AddLabel(TargetControl:TControl);

    procedure SetLabelPos(AValue:String);    virtual;
    procedure SetmyHeight(AValue:string); virtual;
    procedure SetmyWidth(AValue:string); virtual;
    procedure EditingDone; override;
    procedure MyEditingDone(Sender:TObject);

protected
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    procedure Paint;override;
    procedure Loaded; override;
    procedure LabelLeftSettings(TargetControl:TControl);
    procedure LabelRightSettings(TargetControl:TControl);
    procedure LabelTopSettings(TargetControl:TControl);
    procedure LabelBottomSettings(TargetControl:TControl);
    procedure SortOutAlignmentAndLabelPos;

published
  property Align;
  property AutoSize;

  property myNode:TDataNode read FmyNode write FmyNode;

  property IsContainer:Boolean read FIsContainer write FIsContainer;
  property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
  property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
  property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;

  property IsVisible:Boolean read GetIsVisible write SetIsVisible;
  property Hint: String read GetHint write SetHint;
  property Name: String read GetName write SetMyName;
  property myWidth: String read GetmyWidth write SetmyWidth;
  property myHeight: String read GetmyHeight write SetmyHeight;
  property BgColor: TColor read GetBgColor write SetBgColor;
  property LabelText: String read GetLabelText write SetLabelText;
  property LabelPos: String read GetLabelPos write SetLabelPos;
  property Alignment:String read GetAlignment write SetAlignment;

  property Link: TXPropertyLink read FLink write SetLink;
end;


procedure SuppressDesignerProperty(AClass: TClass; pName:String);
procedure SuppressWrapperDesignerProperties;

{$else}


type TWrapperPanel=Class(TInterfaceObject)
 private
   FIsContainer:Boolean;
   FIsSelected:Boolean;
   FAlignChildrenVertical:Boolean;
   function GetName:string;
   function GetIsVisible:Boolean;
   function GetHint:string;
   function GetmyWidth:string;
   function GetmyHeight:string;
   function GetBgColor:string;
   function GetLabelText:string;
   function GetLabelPos:String;
   function GetAlignment:String;
   procedure SetMyName(AValue:string);
   procedure SetIsVisible(AValue:Boolean);
   procedure SetIsSelected(AValue: Boolean);
   procedure SetHint(AValue:string);
   procedure SetLabelText(AValue:string);
   procedure SetLabelPos(AValue:String);
   procedure SetAlignment(AValue:string);
   procedure SortOutAlignmentAndLabelPos;
public
    constructor Create(NodeName:String);
    //procedure ShowHideSelectedBorder(showborder:Boolean);
    procedure SetBgColor(AValue:string); virtual;
    procedure SetmyHeight(AValue:string); virtual;
    procedure SetmyWidth(AValue:string);  virtual;

published
  property IsContainer:Boolean read FIsContainer write FIsContainer;
  property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
  property IsVisible:Boolean read GetIsVisible write SetIsVisible;
  property Hint: String read GetHint write SetHint;
  property Name: String read GetName write SetMyName;
  property myWidth: String read GetmyWidth write SetmyWidth;
  property myHeight: String read GetmyHeight write SetmyHeight;
  property BgColor: String read GetBgColor write SetBgColor;
  property LabelPos: String read GetLabelPos write SetLabelPos;
  property Alignment:String read GetAlignment write SetAlignment;
  property LabelText: String read GetLabelText write SetLabelText;
  property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
end;

procedure SuppressDesignerProperty(Classname:String; pName:String);

{$endif}

type TSuppressedDesignerProperty = record
  ClassName:String;
  PName:String;
  end;

type TSuppressedDesignerProperties = Array of TSuppressedDesignerProperty;
var SuppressedDesignerProperties:TSuppressedDesignerProperties;

function FindSuppressedProperty(Classname,pName:string):integer;

implementation


{$ifndef JScript}
constructor TWrapperPanel.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TWrapperPanel.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TWrapperPanel.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  ControlStyle := ControlStyle - [csSetCaption];
  Caption:='';

  AutoSize:=true;
  ParentColor:=false;

  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  BorderStyle := bsNone;
  BorderWidth:=0;

  IsSelected:=false;
  IsContainer:=true;
  AlignChildrenVertical:=true;

  SelectionBorderColor:=glbSelectionBorderColor;

  MyEventTypes:=TStringList.Create;

  self.OnClick:=@WrapperClick;

  // Lazarus Designer extension...
  if csDesigning in componentState then
   begin
      myExtension:=TXDesignerExtension.Create;
      myExtension.myWrapper:=self;
   end;

  // Property linking...
  FLink:=TXPropertyLink.Create(Self);
  FLink.Filter:=[{tkUnknown,}tkInteger,tkChar,tkEnumeration,
                 tkFloat,{tkSet,tkMethod,}tkSString,tkLString,tkAString,
                 tkWString,tkVariant,{tkArray,tkRecord,tkInterface,}
                 {tkClass,tkObject,}tkWChar,tkBool,tkInt64,
                 tkQWord{,tkDynArray,tkInterfaceRaw}];
  FLink.Options:=[ploAutoSave];
  FLink.OnLoadFromProperty:=@LinkLoadFromProperty;
  FLink.OnSaveToProperty:=@LinkSaveToProperty;
end;

destructor TWrapperPanel.Destroy;
begin
  FreeThenNil(FLink);
  if csDesigning in componentState then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    myExtension.Destroy;
  end;
  inherited Destroy;
end;

procedure TWrapperPanel.Loaded;
var
i:integer;
begin
  inherited Loaded;
  Caption := EmptyStr;

  FLink.LoadFromProperty;

end;

procedure TWrapperPanel.WrapperClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TWrapperPanel.Paint;
var
  PanelArea: TRect;
  R: TRect;
begin
  inherited Paint;
  PaintSelectedRectangle(TCustomControl(self),self.GetClientRect,self.SelectionBorderColor,self.IsSelected);
end;

procedure TWrapperPanel.EditingDone;
begin
  inherited EditingDone;
  FLink.EditingDone;
end;

procedure TWrapperPanel.MyEditingDone(Sender:TObject);
begin
  EditingDone;
end;

procedure TWrapperPanel.SetParent(NewParent: TWinControl);
begin
  inherited;
  ResetAlignment(TWinControl(self));
end;

procedure TWrapperPanel.SetLink(const AValue: TXPropertyLink);
begin
  if FLink=AValue then exit;
  FLink.Assign(AValue);
end;

procedure TWrapperPanel.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if (Link.Editor=nil) then exit;

  //writeln('LinkLoadFromProperty A ',Name,
  //  ' FLink.GetAsText=',FLink.GetAsText,' Text=',Text,
  //  ' PropName=',FLink.TIPropertyName);
  //showmessage('loadfromproperty');

  if MyNode<>nil then
    myNode.SetAttributeValue('Link',LinkToStr(Link));

end;

procedure TWrapperPanel.LinkSaveToProperty(Sender: TObject);
begin
end;

function TLabelPosProperty.GetAttributes: TPropertyAttributes;
begin
  // editor, sorted list
  //Result := [paDialog, paValueList, paSortList];
  Result := [paValueList, paSortList, paPickList];
end;
procedure TLabelPosProperty.GetValues(Proc: TGetStrProc);
begin
  Proc ('Left');
  Proc ('Right');
  Proc ('Top');
  Proc ('Bottom');
end;
function TAlignmentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paPickList];
end;
procedure TAlignmentProperty.GetValues(Proc: TGetStrProc);
begin
  Proc ('Left');
  Proc ('Right');
  Proc ('Centre');
  Proc ('Top');
  Proc ('Bottom');
end;
function TScrollBarsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paPickList];
end;
procedure TScrollBarsProperty.GetValues(Proc: TGetStrProc);
begin
  Proc ('Bottom');
  Proc ('Right');
  Proc ('Both');
end;

procedure SuppressDesignerProperty(AClass: TClass; pName:String);
var
  pInfo:PPropInfo;
  ptInfo:PTypeInfo;
  i:integer;
begin
  pInfo:= FindPropInfo(AClass, pname);
  ptInfo:=pInfo^.PropType;
  RegisterPropertyEditor(ptInfo, AClass, pName, THiddenPropertyEditor);

  // add the property to a suppression list, to be available (eg) for dynamic object inspectors
  if FindSuppressedProperty(AClass.ClassName,pName) < 0 then
  begin
    Setlength(SuppressedDesignerProperties,length(SuppressedDesignerProperties)+1);
    SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].ClassName:=AClass.ClassName;
    SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].pName:=pName;
  end;
end;

procedure SuppressWrapperDesignerProperties;
begin
  // Hide some inherited properties in the Lazarus IDE
  RegisterPropertyEditor(TypeInfo(TAlign), TWrapperPanel, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'Autosize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TWrapperPanel, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TWrapperPanel, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TWrapperPanel, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TWrapperPanel, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TWrapperPanel, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TWrapperPanel, 'Caption', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TWrapperPanel, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TWrapperPanel, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'ParentColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'AlignChildrenVertical', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWrapperPanel, 'IsContainer', THiddenPropertyEditor);

  //special property editors...
  RegisterPropertyEditor (TypeInfo(string), TWrapperPanel, 'Alignment', TAlignmentProperty);
  RegisterPropertyEditor (TypeInfo(string), TWrapperPanel, 'LabelPos', TLabelPosProperty);
end;

{$else}

constructor TWrapperPanel.Create(NodeName:String);
begin

  inherited Create('UI',NodeName,'',false);

  AlignChildrenVertical:=true;

  self.NodeName:=NodeName;
  self.IsContainer:=true;
  self.IsVisible:=true;
  myNode:=TDataNode(self);
  //showmessage('adding '+NodeName+' to SystemNodeTree');
  AddChildToParentNode(SystemNodeTree,myNode,-1);  // to be re-parented later

  Hint:='';
  BgColor:='#FFFFFF';
end;

procedure SuppressDesignerProperty(Classname:String; pName:String);
begin
  // add the property to a suppression list, to be available (eg) for dynamic object inspectors
  if FindSuppressedProperty(ClassName,pName) < 0 then
  begin
    Setlength(SuppressedDesignerProperties,length(SuppressedDesignerProperties)+1);
    SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].ClassName:=ClassName;
    SuppressedDesignerProperties[length(SuppressedDesignerProperties)-1].pName:=pName;
  end;
end;

{$endif}

function FindSuppressedProperty(Classname,pName:string):integer;
var
  i:integer;
begin
  result:=-1;
  i:=0;
  while i < length(SuppressedDesignerProperties) do
  begin
    if (SuppressedDesignerProperties[i].ClassName = ClassName)
    and (SuppressedDesignerProperties[i].PName = pName) then
    begin
      result:=i;
      i:= length(SuppressedDesignerProperties);
    end;
    i:=i+1;
  end;
end;


function TWrapperPanel.GetName:string;
var
  myname:string;
begin
  {$ifndef JScript}
  result:=inherited Name;
  {$else}
  result:=self.NodeName;
  {$endif}
end;

function TWrapperPanel.GetIsVisible:Boolean;
var
  tmp:String;
begin
  if myNode<>nil then
  begin
    tmp:=myNode.GetAttribute('IsVisible',true).AttribValue;
    if tmp='' then tmp:='True';
    result:=myStrToBool(tmp);
  end
  else
    result:=True;
end;
function TWrapperPanel.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TWrapperPanel.GetmyWidth:string;
begin
  result:=myNode.GetAttribute('myWidth',true).AttribValue;
end;
function TWrapperPanel.GetmyHeight:string;
begin
  result:=myNode.GetAttribute('myHeight',true).AttribValue;
end;
function TWrapperPanel.GetLabelPos:string;
begin
  if myNode<>nil then
     result:=myNode.GetAttribute('LabelPos',true).AttribValue
  else
     result:='Top';
end;

{$ifndef JScript}
// Name is the first property loaded from .lfm.
// Hijacking this so that we can reset blank default values for all string properties
// (because there is a problem - string properties are NOT saved to lfm when the value is blank, so
// if the user wants the property to be blank, then we shouldn't set any non-blank defaults when
// re-loading the project from lfm).
procedure TWrapperPanel.SetName(const NewName: TComponentName);
var
  ApplyName:TComponentName;
begin
  ApplyName:=NewName;
  // additional check - component name must be unique in the tree of data nodes for the whole
  // application (ie. not just within a form).  Check here and prefix name if necessary.
  if (csDesigning in componentState) then
    if NodeNameIsUnique(NewName,false) = false then
    begin
      ApplyName:=self.myNode.MyForm.Name + NewName;                  //!!!! when nodes are deleted in IDE, have to destroy the data node and screenobject!!!!
      //showmessage('extending name to '+ApplyName);
    end;

  inherited SetName(ApplyName);

  if (csLoading in componentState) then
  begin
//    showmessage('TWrapperPanel setname. loading.');
    MyWidth:='';
    MyHeight:='';
    Hint:='';
    LabelText:='';
    LabelPos:='';
  end;

end;
{$endif}

procedure TWrapperPanel.SetMyName(AValue:string);
begin

  {$ifndef JScript}
  inherited Name:=AValue;
  {$else}
  self.Name:=AValue;

  asm
     var ob = document.getElementById(this.NodeName);
     inner = pas.HTMLUtils.ScreenObjectInnerComponent(this);
     if (inner.id == this.NodeName+'Contents') {
       inner.id = AValue+'Contents';
       }
        //!!!! issue here with naming of html components / references within event handlers / inner components / etc
     ob.id = AValue;
  end;
  {$endif}

  if myNode<>nil then
     myNode.NodeName:=AValue;
end;

{$ifndef JScript}
procedure TWrapperPanel.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;
{$else}
{$endif}

procedure TWrapperPanel.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      {$ifndef JScript}
      ShowHideSelectedBorder(TCustomControl(self),FIsSelected);
      Repaint;
      {$else}
      ShowHideSelectedBorder(TDataNode(self),FIsSelected);
      {$endif}
    end;
end;


procedure TWrapperPanel.SetmyWidth(AValue:string);
begin
  myNode.SetAttributeValue('myWidth',AValue);
  {$ifndef JScript}
  SetMyHeightWidth(self.myNode,TWinControl(self),'myWidth','myHeight');
  {$else}
  asm
    var ob = document.getElementById(this.NodeName);
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;

  {$endif}
end;

procedure TWrapperPanel.SetmyHeight(AValue:string);
begin
  myNode.SetAttributeValue('myHeight',AValue);
  {$ifndef JScript}
  SetMyHeightWidth(self.myNode,TWinControl(self),'myWidth','myHeight');
  {$else}

  asm
    var ob = document.getElementById(this.NodeName);
    pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;

  {$endif}
end;


procedure TWrapperPanel.SetIsVisible(AValue:Boolean);
begin
  if myNode<>nil then
    myNode.SetAttributeValue('IsVisible',myBoolToStr(AValue));
  {$ifndef JScript}
  self.Visible:=AValue;
  {$else}

  asm
    //alert('set visible '+AValue+' for '+this.NodeName);
    var ob = document.getElementById(this.NodeName);
    if (ob!=null)  {
      if (AValue==true) {
        ob.style.display = 'block';
      }
      else  {
        ob.style.display = 'none';
      }
    }
  end;

  {$endif}
end;


procedure TWrapperPanel.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  {$ifndef JScript}
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
  {$else}

  asm
    var ob = document.getElementById(this.NodeName);
    if (ob!=null)  {
    ob.title=AValue; }
  end;

  {$endif}
end;

function TWrapperPanel.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;

(* childsizing alignment only works on controls with:-
Anchors=[akLeft,akTop]
AnchorSide[akLeft].Control=nil
AnchorSide[akTop].Control=nil
Align=alNone
*)
procedure TWrapperPanel.SetAlignment(AValue:string);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Alignment',AValue);

    {$ifndef JScript}
    if self.Parent<>nil then
    begin
      self.SortOutAlignmentAndLabelPos;
    end;
    {$else}
    //showmessage('setAlignment '+myNode.NodeName+' '+AValue);
    self.SortOutAlignmentAndLabelPos;
    {$endif}
  end;
end;

function TWrapperPanel.GetLabelText:String;
begin
  result:=MyNode.getAttribute('LabelText',true).AttribValue;
end;

procedure TWrapperPanel.SetLabelText(AValue:string);
var tmp:string;
begin
  myNode.SetAttributeValue('LabelText',AValue);
  {$ifndef JScript}
  if self.myLbl<>nil then
     self.myLbl.Caption:=AValue;
  {$else}
  tmp:=myNode.NodeName;

  asm
    var ob = document.getElementById(this.NodeName+'ContentsLbl');
    if (ob!=null) {
       ob.innerHTML=AValue;   }
  end;

  {$endif}
end;

{$ifndef JScript}

procedure TWrapperPanel.LabelLeftSettings(TargetControl:TControl);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akRight, akTop];
    MyLbl.AnchorSideRight.Control := TargetControl;
    MyLbl.AnchorSideRight.Side := asrLeft;
    MyLbl.AnchorVerticalCenterTo(TargetControl);
    MyLbl.Alignment := taRightJustify;
    MyLbl.BorderSpacing.Right:=glbLabelSpacing;
end;
end;
procedure TWrapperPanel.LabelRightSettings(TargetControl:TControl);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akLeft, akTop];
    MyLbl.AnchorSideLeft.Control := TargetControl;
    MyLbl.AnchorSideLeft.Side := asrRight;
    MyLbl.AnchorVerticalCenterTo(TargetControl);
    MyLbl.Alignment := taLeftJustify;
    MyLbl.BorderSpacing.Left:=glbLabelSpacing;
  end;
end;
procedure TWrapperPanel.LabelTopSettings(TargetControl:TControl);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akBottom];
    MyLbl.AnchorSideBottom.Control := TargetControl;
    MyLbl.AnchorSideBottom.Side := asrTop;
    MyLbl.AnchorHorizontalCenterTo(TargetControl);
    MyLbl.Alignment := taLeftJustify;
    MyLbl.BorderSpacing.Bottom:=glbLabelSpacing;
  end;
end;
procedure TWrapperPanel.LabelBottomSettings(TargetControl:TControl);
begin
  if MyLbl<>nil then
  begin

    MyLbl.Anchors := [akTop];
    MyLbl.AnchorSideTop.Control := TargetControl;
    MyLbl.AnchorSideTop.Side := asrBottom;
    MyLbl.AnchorHorizontalCenterTo(TargetControl);
    MyLbl.Alignment := taLeftJustify;
    MyLbl.BorderSpacing.Top:=glbLabelSpacing;
  end;
end;

procedure TWrapperPanel.AddLabel(TargetControl:TControl);
var
  Lbl:TLabel;
begin

  // add a label
   MyLbl := TLabel.Create(self);
   MyLbl.Caption:='.';
   LabelRightSettings(TargetControl);
   MyLbl.parent:=self;
end;

procedure TWrapperPanel.SetLabelPos(AValue:string);
var
  ch:TWinControl;
  i,j:integer;
  Saveccl:TControlChildrenLayout;
begin
  myNode.SetAttributeValue('LabelPos',AValue);
  if (myLbl<>nil) then
  begin
    ch:=nil;
    // find the component to which the label is currently anchored
    for i:=0 to self.ControlCount-1 do
    begin
      for j:=0 to self.Controls[i].AnchoredControlCount-1 do
      begin
        if (self.Controls[i].AnchoredControls[j] = myLbl)
        and (self.Controls[i].AnchoredControls[j]<>myLbl.Owner)
        then
          ch:=TWinControl(self.Controls[i]);
      end;
    end;

    if (ch<>nil)
    and (ch is TWinControl) then
    begin
      self.SortOutAlignmentAndLabelPos;
    end;
  end;
end;

procedure TWrapperPanel.SortOutAlignmentAndLabelPos;
var
    ParentAlignChildrenVertical:Boolean;
    TheControl,TheAnchorTarget:TWinControl;
    MyAlignment,MyLabelPos:String;
begin
  if self.Parent<>nil then
  begin
    MyAlignment:=self.Alignment;
    MyLabelPos:=self.LabelPos;

    ParentAlignChildrenVertical := GetBooleanProperty(self.Parent, 'AlignChildrenVertical');


    if myControl<>nil then
    begin
      TheControl:=TWinControl(myControl);
      TheAnchorTarget:=self;
    end
    else
    begin
      TheControl:=self;
      TheAnchorTarget:=self.Parent;
    end;

    ClearAllAlignment(myLbl,TheControl);

    if (MyLabelPos = 'Right') then
      LabelRightSettings(TheControl)
    else if (MyLabelPos = 'Left') then
      LabelLeftSettings(TheControl)
    else if (MyLabelPos = 'Top') then
      LabelTopSettings(TheControl)
    else if (MyLabelPos = 'Bottom') then
      LabelBottomSettings(TheControl);

    // reset invalid combinations
    if (MyAlignment='Right') or (MyAlignment='Left') then
    begin
      if ParentAlignChildrenVertical=false then   // in a horizontal list
        MyAlignment:='Top';
    end
    else if (MyAlignment='Top') or (MyAlignment='Bottom') then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
        MyAlignment:='Left';
    end;


    if MyAlignment='Right' then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
      begin
        if (MyLabelPos = 'Left') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akRight, akTop];
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;
        end
        else if MyLabelPos = 'Right' then
        begin
          MyLbl.Anchors := [akTop, akRight];
          myLbl.AnchorSideRight.Control := TheAnchorTarget;
          myLbl.AnchorSideRight.Side := asrRight;
          MyLbl.AnchorVerticalCenterTo(TheControl);
          TheControl.Anchors := [akRight, akTop];
          TheControl.anchorsideRight.Control:=MyLbl;
          TheControl.anchorsideRight.Side:=asrLeft;
        end
        else if MyLabelPos = 'Top' then
        begin
          MyLbl.Anchors := [akTop, akRight];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akRight, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;
        end
        else if MyLabelPos = 'Bottom' then
        begin
          TheControl.Anchors := [akRight,akTop];
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;
        end;
      end;
    end
    else if MyAlignment = 'Left' then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
      begin
        if (MyLabelPos = 'Right') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
        end
        else if (MyLabelPos = 'Left') then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideLeft.Control:=MyLbl;
          TheControl.anchorsideLeft.Side:=asrRight;
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideLeft.Control := TheAnchorTarget;
          myLbl.AnchorSideLeft.Side := asrLeft;
          MyLbl.AnchorVerticalCenterTo(TheControl);
        end
        else if (MyLabelPos = 'Top') then
        begin
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
        end
        else if (MyLabelPos = 'Bottom') then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
        end;
      end;
    end
    else if MyAlignment = 'Centre' then
    begin
      TheControl.Anchors := [akLeft, akTop];
      if ParentAlignChildrenVertical=true then
      begin
        if MyLabelPos='Top' then
        begin
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
        end;
         TheControl.AnchorHorizontalCenterTo(TheControl.Parent)
      end
      else
        begin
          if MyLabelPos='Left' then
          begin
            TheControl.Anchors := [akLeft, akTop];
            TheControl.anchorsideLeft.Control:=MyLbl;
            TheControl.anchorsideLeft.Side:=asrRight;
            MyLbl.Anchors := [akTop, akLeft];
            myLbl.AnchorSideLeft.Control := TheAnchorTarget;
            myLbl.AnchorSideLeft.Side := asrLeft;
            MyLbl.AnchorVerticalCenterTo(TheControl);
          end;
         TheControl.AnchorVerticalCenterTo(TheControl.Parent);
        end;
    end
    else if MyAlignment = 'Top' then   // in a horizontal list
    begin
      if ParentAlignChildrenVertical=false then
      begin
        if (MyLabelPos = 'Right') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akLeft,akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
        end
        else if (MyLabelPos = 'Left') then
        begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
          TheControl.anchorsideLeft.Control:=MyLbl;
          TheControl.anchorsideLeft.Side:=asrRight;
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideLeft.Control := TheAnchorTarget;
          myLbl.AnchorSideLeft.Side := asrLeft;
          MyLbl.AnchorVerticalCenterTo(TheControl);

          //TheControl.Anchors := [akTop];
        end
        else if (MyLabelPos = 'Top') then
        begin
          MyLbl.Anchors := [akTop, akLeft];
          myLbl.AnchorSideTop.Control := TheAnchorTarget;
          myLbl.AnchorSideTop.Side := asrTop;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akTop];
          TheControl.anchorsideTop.Control:=MyLbl;
          TheControl.anchorsideTop.Side:=asrBottom;
        end
        else if (MyLabelPos = 'Bottom') then
        begin
          TheControl.Anchors := [akLeft,akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
        end;
      end;
    end
    else if MyAlignment = 'Bottom' then  // in a horizontal list
    begin
      if ParentAlignChildrenVertical=false then
      begin
        if (MyLabelPos = 'Right') or (myLbl=nil) then
        begin
          TheControl.Anchors := [akLeft,akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;
        end
        else if (MyLabelPos = 'Left') then
        begin
          TheControl.Anchors := [akLeft, akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;
          TheControl.anchorsideLeft.Control:=MyLbl;
          TheControl.anchorsideLeft.Side:=asrRight;
          MyLbl.Anchors := [akBottom, akLeft];
          myLbl.AnchorSideLeft.Control := TheAnchorTarget;
          myLbl.AnchorSideLeft.Side := asrLeft;
          MyLbl.AnchorVerticalCenterTo(TheControl);
        end
        else if (MyLabelPos = 'Top') then
        begin
          TheControl.Anchors := [akLeft,akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;
        end
        else if (MyLabelPos = 'Bottom') then
        begin
          MyLbl.Anchors := [akBottom, akLeft];
          myLbl.AnchorSideBottom.Control := TheAnchorTarget;
          myLbl.AnchorSideBottom.Side := asrBottom;
          MyLbl.AnchorHorizontalCenterTo(TheControl);
          TheControl.Anchors := [akLeft, akBottom];
          TheControl.anchorsideBottom.Control:=MyLbl;
          TheControl.anchorsideBottom.Side:=asrTop;
        end;
      end;
    end;

  end;
end;

  {$else}
procedure TWrapperPanel.SortOutAlignmentAndLabelPos;
var
    ParentAlignChildrenVertical:Boolean;
    MyAlignment,MyLabelPos:String;
    ParentNode:TDataNode;
begin
    MyAlignment:=self.Alignment;
    MyLabelPos:=self.LabelPos;
    ParentNode:=FindParentOfNode(SystemNodeTree,self.NodeName);
    ParentAlignChildrenVertical := TWrapperPanel(ParentNode).AlignChildrenVertical;

    //showmessage('SortOutAlignmentAndLabelPos '+self.NodeName+' '+MyAlignment);

    // reset invalid combinations
    if (MyAlignment='Right') or (MyAlignment='Left') then
    begin
      if ParentAlignChildrenVertical=false then   // in a horizontal list
        MyAlignment:='Top';
    end
    else if (MyAlignment='Top') or (MyAlignment='Bottom') then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
        MyAlignment:='Left';
    end;

    asm
    try {
       var ob = document.getElementById(this.NodeName+'Contents');
       var lbl = document.getElementById(this.NodeName+'ContentsLbl');
       var wrapper = document.getElementById(this.NodeName);
       var lp = MyLabelPos;

       if ((ob!=null) && (wrapper!=null)) {
       wrapper.classList.remove('hbox');
       wrapper.classList.remove('hboxNoStretch');
       wrapper.classList.remove('vbox');
       wrapper.classList.remove('vboxNoStretch');
       wrapper.classList.remove('AlignmentCentre');
       wrapper.classList.remove('AlignmentRight');
       wrapper.classList.remove('AlignmentLeft');
       wrapper.classList.remove('AlignmentTop');
       wrapper.classList.remove('AlignmentBottom');

       if (lbl!=null) {
         lbl.style.padding='0px';

         if (lp=='Left') {
           lbl.parentNode.insertBefore(lbl, ob);  //put lbl before ob
           wrapper.classList.add('hboxNoStretch');
           //lbl.style.verticalAlign='left';
           lbl.style.alignSelf='center';
           lbl.style.padding='0px 3px 0px 0px';               // t,r,b,l
         }
         else if (lp=='Right') {
           ob.parentNode.insertBefore(ob, lbl);  //put lbl after ob
           wrapper.classList.add('hboxNoStretch');
           //lbl.style.verticalAlign='right';
           lbl.style.alignSelf='center';
           lbl.style.padding='0px 0px 0px 3px';               // t,r,b,l
         }
         else if (lp=='Top') {
           ob.parentNode.insertBefore(lbl, ob);
           wrapper.classList.add('vboxNoStretch');
           //lbl.style.verticalAlign='top';
           lbl.style.alignSelf='center';
           lbl.style.padding='0px 0px 0px 3px';               // t,r,b,l
         }
         else if (lp=='Bottom') {
           ob.parentNode.insertBefore(ob, lbl);
           wrapper.classList.add('vboxNoStretch');
           //lbl.style.verticalAlign='bottom';
           lbl.style.alignSelf='center';
           lbl.style.padding='3px 0px 0px 0px';               // t,r,b,l
         }
       }


       if (MyAlignment=='Right') {
         if (ParentAlignChildrenVertical) {
         ob.style.float='right';
         wrapper.classList.add('AlignmentRight');
         if (lbl!=null) {
           lbl.style.float='right';
           if ((lp=='Top')||(lp=='Bottom')) {
               lbl.style.alignSelf='flex-e'+'nd';
           }
         }
       }
       }
       else if (MyAlignment=='Left') {
       if (ParentAlignChildrenVertical) {
           ob.style.float='left';
           wrapper.classList.add('AlignmentLeft');
           if (lbl!=null) {
             lbl.style.float='left';
             if ((lp=='Top')||(lp=='Bottom')) {
                 lbl.style.alignSelf='flex-start';
             }
           }
         }
         }
       else if (MyAlignment=='Centre') {
         ob.style.float='left';
          wrapper.classList.add('AlignmentCentre');
          if (lbl!=null) {
             lbl.style.float='left';
          }
       }

       else if (MyAlignment=='Top') {
       if (ParentAlignChildrenVertical==false) {
         ob.style.float='left';
         wrapper.classList.add('AlignmentTop');
         if (lbl!=null) {
           lbl.style.float='left';
           if ((lp=='Left')||(lp=='Right')) {
               lbl.style.alignSelf='flex-start';
             }
         }
        }
        }
       else if (MyAlignment=='Bottom') {
       if (ParentAlignChildrenVertical==false) {
         ob.style.float='left';
         wrapper.classList.add('AlignmentBottom');
         if (lbl!=null) {
          lbl.style.float='left';
          if ((lp=='Left')||(lp=='Right')) {
               lbl.style.alignSelf='flex-e'+'nd';
               }
         }
     }
    }


   }
 } catch(err) { alert(err.message+'  in WrapperPanel.SortOutAlignmentAndLabelPos'); }
end;

end;

procedure TWrapperPanel.SetLabelPos(AValue:string);
var
  lp:string;
begin
  myNode.SetAttributeValue('LabelPos',AValue);
  SortOutAlignmentAndLabelPos;
end;

{$endif}


{$ifndef JScript}
function TWrapperPanel.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TWrapperPanel.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  if ParentColor=false then
    Color:=AValue;
end;
{$else}
function TWrapperPanel.GetBgColor:string;
begin
  result:=myNode.GetAttribute('BgColor',true).AttribValue;
end;
procedure TWrapperPanel.SetBgColor(AValue:string);
begin
  SetAttributeValue('BgColor',AValue,'Color');

  asm
  try {
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    ob.style.backgroundColor = AValue;  }
    } catch(err) { alert(err.message+'  in WrapperPanel.SetBgColor'); }
  end;

end;
{$endif}

{$ifndef JScript}


function TWrapperPanel.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean;
var
  i,j:integer;
  NewList:TFPList;
  tmp:TControl;
begin
  if self.IsContainer then
    AControlList.Sort(@SortAlignList);
  inherited DoAlignChildControls(TheAlign,AControl,AControlList,ARect);
end;


{$endif}
begin
  Setlength(SuppressedDesignerProperties,0);
  {$ifndef JScript}
  // Hide some properties in the Lazarus IDE
  SuppressWrapperDesignerProperties;
  {$endif}

end.

