unit XScrollBox;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface
                           //to do  html....
uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Types,
  TypInfo, Propedits,RTTICtrls,Menus,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}

{$ifndef JScript}

type
  TXScrollBox = class(TScrollBox)
  private
    FSelectionBorderColor: TColor;
    FIsSelected:Boolean;
    FIsContainer:Boolean;
    FAlignChildrenVertical:Boolean;
    fHandleClick:TEventHandler;
    FmyNode:TDataNode;
    myExtension:TXDesignerExtension;

    procedure SetMyEventTypes;

    procedure ScrollBoxclick(Sender:TObject);
    function GetName:string;
    function GetmyWidth:string;
    function GetmyHeight:string;
    function GetHint:string;
    function GetBgColor:TColor;
    function GetScrollType:string;
    function GetAlignment:String;

    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetSelectionBorderColor(AValue: TColor);
    procedure SetmyHeight(AValue:string);
    procedure SetmyWidth(AValue:string);
    procedure SetHint(AValue:string);
    procedure SetBgColor(AValue:TColor);
    procedure SetScrollType(AValue:string);
    procedure SetAlignment(AValue:string);
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(NewParent: TWinControl); override;

    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                      AControlList: TFPList; var ARect: TRect): Boolean; override;
  public
     myEventTypes:TStringList;
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

 protected
   procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
   procedure Paint;override;
   procedure Loaded; override;
 //  procedure SetIsContainer(AValue:Boolean);
   procedure SortOutAlignment;
 published
   property Caption;
   property ClientHeight;
   property ChildSizing;
   property Constraints;
   property ParentColor;

   property myNode:TDataNode read FmyNode write FmyNode;
   property IsContainer:Boolean read FIsContainer write FIsContainer;
   property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;
   property Alignment:String read GetAlignment write SetAlignment;

   property Hint: String read GetHint write SetHint;
   property Name: String read GetName write SetMyName;
   property myWidth: String read GetmyWidth write SetmyWidth;
   property myHeight: String read GetmyHeight write SetmyHeight;
   property BgColor: TColor read GetBgColor write SetBgColor;

   property ScrollType: String read GetScrollType write SetScrollType;

   // Events to be visible in IDE
   property HandleClick: TEventHandler read FHandleClick write FHandleClick;
 end;


  procedure Register;


{$else}
type
  TXScrollBox = class(TWrapperPanel)
  private
    FIsContainer:Boolean;
    FAlignChildrenVertical:Boolean;
    procedure SetMyEventTypes;

    function GetScrollType:string;
    function GetAlignment:String;

    procedure SetScrollType(AValue:string);
    procedure SetAlignment(AValue:string);
  protected
    { Protected declarations }
    procedure SortOutAlignment;
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property Alignment:String read GetAlignment write SetAlignment;
    property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
    property ScrollType: String read GetScrollType write SetScrollType;

  end;
{$endif}

implementation

const MyNodeType='TXScrollBox';

procedure TXScrollBox.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
procedure Register;
begin
  // Lazarus IDE component registration
  RegisterComponents('Misc',[TXScrollBox]);

  //special property editors
  RegisterPropertyEditor (TypeInfo(string), TXScrollBox, 'ScrollType', TScrollBarsProperty);
  RegisterPropertyEditor (TypeInfo(string), TXScrollBox, 'Alignment', TAlignmentProperty);

  // Hide some inherited properties
  RegisterPropertyEditor(TypeInfo(TAlign), TXScrollBox, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchors), TXScrollBox, 'Anchors', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'AutoScroll', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'AutoSize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TXScrollBox, 'BiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentBiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TXScrollBox, 'BorderSpacing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderStyle), TXScrollBox, 'BorderStyle', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXScrollBox, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'DockSite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXScrollBox, 'DragCursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragKind), TXScrollBox, 'DragKind', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragMode), TXScrollBox, 'DragMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'Enabled', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFont), TXScrollBox, 'Font', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentFont', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXScrollBox, 'HorzScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXScrollBox, 'VertScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPopupmenu), TXScrollBox, 'PopupMenu', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TXScrollBox, 'TabOrder', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'TabStop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'Visible', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXScrollBox, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXScrollBox, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TXScrollBox, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TXScrollBox, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXScrollBox, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TXScrollBox, 'Caption', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXScrollBox, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TXScrollBox, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'ParentColor', THiddenPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'AlignChildrenVertical', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXScrollBox, 'IsContainer', THiddenPropertyEditor);

  //.....suppress unwanted designer events.......
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TConstrainedResizeEvent), TXScrollBox, 'OnConstrainedResize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnDblClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXScrollBox, 'OnContextpopup', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockDropEvent), TXScrollBox, 'OnDockDrop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockOverEvent), TXScrollBox, 'OnDockOver', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXScrollBox, 'OnDragDrop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXScrollBox, 'OnDragOver', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXScrollBox, 'OnEndDock', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXScrollBox, 'OnEndDrag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnEnter', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnExit', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGetSiteInfoEvent), TXScrollBox, 'OnGetSiteInfo', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TXScrollBox, 'OnMouseDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnMouseEnter', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnMouseLeave', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXScrollBox, 'OnMouseMove', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseEvent), TXScrollBox, 'OnMouseUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXScrollBox, 'OnMouseWheel', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXScrollBox, 'OnMouseWheelDown', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXScrollBox, 'OnMouseWheelUp', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnPaint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXScrollBox, 'OnResize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDockEvent), TXScrollBox, 'OnStartDock', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXScrollBox, 'OnStartDrag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TUnDockEvent), TXScrollBox, 'OnUnDock', THiddenPropertyEditor);
end;

procedure TXScrollBox.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  ControlStyle := ControlStyle - [csSetCaption];

  AutoSize:=true;
  ParentColor:=false;
  BorderStyle := bsNone;
  BorderWidth:=0;
  Caption:='';
  AutoScroll:=true;


  self.OnClick:=@ScrollBoxClick;

  MyEventTypes:=TStringList.Create;
  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  // Lazarus Designer extension...
  if csDesigning in componentState then
   begin
      myExtension:=TXDesignerExtension.Create;
      myExtension.myWrapper:=self;
   end;

  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  IsSelected:=false;
  IsContainer:=true;
  SelectionBorderColor:=glbSelectionBorderColor;
  AlignChildrenVertical:=true;
  Hint:='';
  MyWidth:='300px';
  MyHeight:='300px';
  ScrollType:='Both';
  BgColor:=clWhite;
end;

constructor TXScrollBox.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXScrollBox.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXScrollBox;
  NewNode:TDataNode;
begin
  NewWidget:=TXScrollBox.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXScrollBox.SetParent(NewParent: TWinControl);
begin
  inherited;
  ResetAlignment(TWinControl(self));
end;

procedure TXScrollBox.ScrollBoxClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

procedure TXScrollBox.Paint;
begin
  inherited Paint;
  PaintSelectedRectangle(TCustomControl(self),self.GetClientRect,self.SelectionBorderColor,self.IsSelected);
end;

procedure TXScrollBox.Loaded;
var
i:integer;
begin
  inherited Loaded;

  // special case (MyRootDiv for template project has align=alClient)
  if align=alClient then
  begin
     myWidth:='';
     myHeight:='';
  end;

end;

procedure TXScrollBox.SortOutAlignment;
var
    ParentAlignChildrenVertical:Boolean;
    TheControl,TheAnchorTarget:TWinControl;
    MyAlignment:String;
begin
  if self.Parent<>nil then
  begin
    MyAlignment:=self.Alignment;

    ParentAlignChildrenVertical := GetBooleanProperty(self.Parent, 'AlignChildrenVertical');


    TheControl:=self;
    TheAnchorTarget:=self.Parent;

    ClearAllAlignment(nil,TheControl);

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
          TheControl.Anchors := [akRight, akTop];
          TheControl.AnchorSideRight.Control := TheAnchorTarget;
          TheControl.AnchorSideRight.Side := asrRight;
      end;
    end
    else if MyAlignment = 'Left' then
    begin
      if ParentAlignChildrenVertical=true then   // in a vertical list
      begin
          TheControl.Anchors := [akLeft, akTop];
          TheControl.AnchorSideLeft.Control := TheAnchorTarget;
          TheControl.AnchorSideLeft.Side := asrLeft;
      end;
    end
    else if MyAlignment = 'Centre' then
    begin
      TheControl.Anchors := [akLeft, akTop];
      if ParentAlignChildrenVertical=true then
      begin
         TheControl.AnchorHorizontalCenterTo(TheControl.Parent)
      end
      else
        begin
         TheControl.AnchorVerticalCenterTo(TheControl.Parent);
        end;
    end
    else if MyAlignment = 'Top' then   // in a horizontal list
    begin
      if ParentAlignChildrenVertical=false then
      begin
          TheControl.Anchors := [akLeft,akTop];
          TheControl.AnchorSideTop.Control := TheAnchorTarget;
          TheControl.AnchorSideTop.Side := asrTop;
      end;
    end
    else if MyAlignment = 'Bottom' then  // in a horizontal list
    begin
      if ParentAlignChildrenVertical=false then
      begin
          TheControl.Anchors := [akLeft,akBottom];
          TheControl.AnchorSideBottom.Control := TheAnchorTarget;
          TheControl.AnchorSideBottom.Side := asrBottom;

      end;
    end;

  end;
end;

{$else}
constructor TXScrollBox.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  AlignChildrenVertical:=true;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  BgColor:='#FFFFFF';
  MyWidth:='300px';
  MyHeight:='300px';
  ScrollType:='Both';
end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  ScrollType:string;
  OnChangeString, OnClickString, OnPasteString:String;
begin
  ScrollType:= uppercase(MyNode.getAttribute('ScrollType',true).AttribValue);

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', this.value, '''');" ';
  //showmessage('scrollbox createwidget');

  asm
    try{

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';

    var HTMLString='';
    var MyObjectName=ScreenObjectName+'Contents';
    var oflow = ''
    if ((ScrollType=='BOTH')||(ScrollType=='RIGHT')) {oflow = 'overflow-y:scroll; '}
    if ((ScrollType=='BOTH')||(ScrollType=='BOTTOM')) {oflow = oflow+'overflow-x:scroll; '}

    HTMLString = '<div id='+MyObjectName+ ' style="'+oflow+' height:100%; width:100%;" ' +
                 OnClickString +
                 '></div> ';


    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }
  catch(err) { alert(err.message+'  in XScrollBox.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  TXScrollBox(myNode).myHeight:=TXScrollBox(myNode).myHeight;
  TXScrollBox(myNode).myWidth:=TXScrollBox(myNode).myWidth;
  TXScrollBox(myNode).Alignment:=TXScrollBox(myNode).Alignment;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  //showmessage('CreateinterfaceObj '+NodeName);
  result:=TObject(TXScrollBox.Create(MyForm,NodeName));
  //showmessage('CreateinterfaceObj '+NodeName+' done');
end;

procedure TXScrollBox.SortOutAlignment;
var
    ParentAlignChildrenVertical:Boolean;
    MyAlignment,MyLabelPos:String;
    ParentNode:TDataNode;
begin
    MyAlignment:=self.Alignment;
    MyLabelPos:=self.LabelPos;
    ParentNode:=FindParentOfNode(SystemNodeTree,self.NodeName);
    ParentAlignChildrenVertical := TWrapperPanel(ParentNode).AlignChildrenVertical;

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
       var wrapper = document.getElementById(this.NodeName);

       if ((ob!=null)  && (wrapper!=null)) {
       wrapper.classList.remove('hboxNoStretch');
       wrapper.classList.remove('vboxNoStretch');
       wrapper.classList.remove('AlignmentCentre');
       wrapper.classList.remove('AlignmentRight');
       wrapper.classList.remove('AlignmentLeft');
       wrapper.classList.remove('AlignmentTop');
       wrapper.classList.remove('AlignmentBottom');

       if (MyAlignment=='Right') {
         if (ParentAlignChildrenVertical) {
         ob.style.float='right';
         wrapper.classList.add('AlignmentRight');
       }
       }
       else if (MyAlignment=='Left') {
       if (ParentAlignChildrenVertical) {
           ob.style.float='left';
           wrapper.classList.add('AlignmentLeft');
         }
         }
       else if (MyAlignment=='Centre') {
         ob.style.float='left';
          wrapper.classList.add('AlignmentCentre');
       }

       else if (MyAlignment=='Top') {
       if (ParentAlignChildrenVertical==false) {
         ob.style.float='left';
         wrapper.classList.add('AlignmentTop');
       }
       }
       else if (MyAlignment=='Bottom') {
       if (ParentAlignChildrenVertical==false) {
         ob.style.float='left';
         wrapper.classList.add('AlignmentBottom');
       }
     }

   }
 } catch(err) { alert(err.message+'  in XScrollBox.SortOutAlignment'); }
end;
end;
{$endif}

function TXScrollBox.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;
procedure TXScrollBox.SetAlignment(AValue:string);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Alignment',AValue);

    {$ifndef JScript}
    if self.Parent<>nil then
    begin
      self.SortOutAlignment;
    end;
    {$else}
    self.SortOutAlignment;
    {$endif}
  end;
end;

{$ifndef JScript}
function TXScrollBox.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXScrollBox.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXScrollBox.GetmyWidth:string;
begin
  result:=myNode.GetAttribute('myWidth',true).AttribValue;
end;
function TXScrollBox.GetmyHeight:string;
begin
  result:=myNode.GetAttribute('myHeight',true).AttribValue;
end;
(*
// At design time, the name property is set AFTER the component has been created, so
// we pick up the new name here.
procedure TXScrollBox.SetName(const NewName: TComponentName);
var
  oldname:string;
begin
  if (csDesigning in componentState)
  and not (csLoading in componentState) then
  begin
    oldname:=inherited name;
    //showmessage('setname. change >'+oldname+'< to >'+NewName+'<');
    if oldname='' then   // Only on newly created object in IDE
    begin
      // write the code needed for event handling after cross-compilation
      //WriteEventHandlerCode(NewName);
    end;
  end;
  inherited SetName(NewName);

end;
*)

// Name is the first property loaded from .lfm.
// Hijacking this so that we can reset blank default values for all string properties
// (because there is a problem - string properties are NOT saved to lfm when the value is blank, so
// if the user wants the property to be blank, then we shouldn't set any non-blank defaults when
// re-loading the project from lfm).
procedure TXScrollBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);

  if (csLoading in componentState) then
  begin
//    showmessage('TWrapperPanel setname. loading.');
    MyWidth:='';
    MyHeight:='';
    Hint:='';
  end;

end;

{$endif}

function TXScrollBox.GetScrollType:string;
begin
  result:=MyNode.getAttribute('ScrollType',true).AttribValue;
end;


{$ifndef JScript}
procedure TXScrollBox.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if myNode<>nil then
     myNode.NodeName:=AValue;
  // also rename any associated event code ???
end;

procedure TXScrollBox.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;

procedure TXScrollBox.SetIsSelected(AValue: Boolean);
begin
  if AValue<>FIsSelected then
    begin
      FIsSelected:=AValue;
      ShowHideSelectedBorder(TCustomControl(self),FIsSelected);
      Repaint;
    end;
end;


procedure TXScrollBox.SetHint(AValue:string);
begin
  myNode.SetAttributeValue('Hint',AValue);
  if AValue<>'' then
    self.ShowHint:=true
  else
    self.ShowHint:=false;
  inherited Hint:=AValue;
end;

procedure TXScrollBox.SetmyWidth(AValue:string);
begin
  myNode.SetAttributeValue('myWidth',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self),'myWidth','myHeight');
end;

procedure TXScrollBox.SetmyHeight(AValue:string);
begin
  myNode.SetAttributeValue('myHeight',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self),'myWidth','myHeight');
end;

{$endif}


procedure TXScrollBox.SetScrollType(AValue:string);
var
  AVal:string;
begin
  myNode.SetAttributeValue('ScrollType',AValue);
  AVal:=uppercase(AValue);
  {$ifndef JScript}
  if (AVal='BOTH')
  or (AVal='RIGHT') then
     self.VertScrollBar.Visible:=true
  else
    self.VertScrollBar.Visible:=false;
  if (AVal='BOTH')
  or (AVal='BOTTOM') then
    self.HorzScrollBar.Visible:=true
  else
    self.HorzScrollBar.Visible:=false;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      ob.style.overlow='none';
      if ((AVal=='BOTH')||(AVal=='RIGHT')) {ob.style.overflowY='scroll';}
      if ((AVal=='BOTH')||(AVal=='BOTTOM')) {ob.style.overflowX='scroll';}
      }
  end;
  {$endif}
end;



{$ifndef JScript}
function TXScrollBox.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TXScrollBox.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;
function TXScrollBox.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean;
var
  i,j:integer;
  NewList:TFPList;
  tmp:TControl;
begin
  AControlList.Sort(@SortAlignList);
  inherited DoAlignChildControls(TheAlign,AControl,AControlList,ARect);
end;

{$endif}

begin
  {$ifndef JScript}
  {$I XScrollBox.lrs}
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.
