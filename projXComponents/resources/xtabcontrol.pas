unit XTabControl;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface
                           //to do  html....
uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, ComCtrls, Graphics, Dialogs, ExtCtrls, Types, ImgList,
  TypInfo, Propedits,RTTICtrls, ComponentEditors, Menus, ObjInspStrConsts,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events, LazLogger;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

function ChangeTabPage(nodeId:string):string;
{$endif}

{$ifndef JScript}
type
 TXTabSheet = class(TTabSheet)
  private
  FIsSelected:Boolean;
  FIsContainer:Boolean;
  FAlignChildrenVertical:Boolean;
  fHandleClick:TEventHandler;
  FSelectionBorderColor: TColor;
  FmyNode:TDataNode;

  procedure SetMyEventTypes;

  procedure TabSheetclick(Sender:TObject);
  //procedure TabSheetChangeBounds(Sender:TObject);

  function GetName:string;
  function GetHint:string;
  function GetBgColor:TColor;
  function GetmyCaption:string;

  procedure SetMyName(AValue:string);
  procedure SetIsSelected(AValue: Boolean);
  procedure SetSelectionBorderColor(AValue: TColor);
  procedure SetHint(AValue:string);
  procedure SetBgColor(AValue:TColor);
  procedure SetmyCaption(AValue:string);

  procedure SetName(const NewName: TComponentName); override;
  function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                    AControlList: TFPList; var ARect: TRect): Boolean; override;
  procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
 // procedure Paint;override;                 // paint override not available
  public
    myEventTypes:TStringList;
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

  published
    property myNode:TDataNode read FmyNode write FmyNode;
    property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
    property IsContainer:Boolean read FIsContainer write FIsContainer;
    property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
    property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;

    property Hint: String read GetHint write SetHint;
    property Name: String read GetName write SetMyName;
    property myCaption: String read GetmyCaption write SetmyCaption;
    property BgColor: TColor read GetBgColor write SetBgColor;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
end;

  TXTabControl = class(TPageControl)
  private
    FSelectionBorderColor: TColor;
    FIsSelected:Boolean;
    FIsContainer:Boolean;
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    FmyNode:TDataNode;
    myExtension:TXDesignerExtension;

    procedure SetMyEventTypes;

    procedure TabControlclick(Sender:TObject);
    procedure TabPageChanged(Sender:TObject);

    function GetName:string;
    function GetmyWidth:string;
    function GetmyHeight:string;
    function GetHint:string;
    function GetBgColor:TColor;
    function GetAlignment:String;

    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetSelectionBorderColor(AValue: TColor);
    procedure SetmyHeight(AValue:string);
    procedure SetmyWidth(AValue:string);
    procedure SetHint(AValue:string);
    procedure SetBgColor(AValue:TColor);
    procedure SetAlignment(AValue:string);

    function AddTabSheet:TXTabSheet;
    function GetActiveTabSheet: TXTabSheet;
    function GetTabSheet(Index: Integer): TXTabSheet;
    procedure SetActiveTabSheet(const AValue: TXTabSheet);
    function GetPageClass: TCustomPageClass;            override;
    procedure SelectNextPage(GoForward: Boolean);
    procedure SelectNextPage(GoForward: Boolean;CheckTabVisible: Boolean);
    function FindNextPage(CurPage: TXTabSheet; GoForward, CheckTabVisible: Boolean): TXTabSheet ;
    procedure SortOutAlignment;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  //  procedure Paint;override;               // paint override not available
  public
     myEventTypes:TStringList;
     constructor Create(TheOwner: TComponent); override;
     constructor Create(TheOwner: TComponent;IsDynamic:Boolean); virtual;

     property Pages[Index: Integer]: TXTabSheet read GetTabSheet;

 published
   property ActivePage: TXTabSheet read GetActiveTabSheet write SetActiveTabSheet;

   property myNode:TDataNode read FmyNode write FmyNode;
   property IsContainer:Boolean read FIsContainer write FIsContainer;
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property SelectionBorderColor: TColor read FSelectionBorderColor write SetSelectionBorderColor default clGreen;

   property Hint: String read GetHint write SetHint;
   property Name: String read GetName write SetMyName;
   property myWidth: String read GetmyWidth write SetmyWidth;
   property myHeight: String read GetmyHeight write SetmyHeight;
   property BgColor: TColor read GetBgColor write SetBgColor;
   property Alignment:String read GetAlignment write SetAlignment;

   // Events to be visible in IDE
   property HandleClick: TEventHandler read FHandleClick write FHandleClick;
   property HandleChange: TEventHandler read FHandleChange write FHandleChange;
 end;


 TXTabControlComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoInsertPage; virtual;
    procedure DoDeletePage; virtual;
    procedure DoMoveActivePageLeft; virtual;
    procedure DoMoveActivePageRight; virtual;
    procedure DoMovePage(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function TabControl: TXTabControl; virtual;
  end;

procedure Register;

{$else}
type
  TXTabControl = class(TWrapperPanel)
    FIsSelected:Boolean;
    function GetName:string;
    function GetHint:string;
    function GetmyWidth:string;
    function GetmyHeight:string;
    function GetBgColor:string;
    function GetAlignment:String;
    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetHint(AValue:string);
    procedure SetAlignment(AValue:string);
    procedure SortOutAlignment;
    procedure SetMyEventTypes;
 public
     constructor Create(MyForm:TForm;NodeName:String);
     procedure SetBgColor(AValue:string); virtual;
     procedure SetmyHeight(AValue:string); virtual;
     procedure SetmyWidth(AValue:string);  virtual;
 published
   property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
   property Hint: String read GetHint write SetHint;
   property Name: String read GetName write SetMyName;
   property myWidth: String read GetmyWidth write SetmyWidth;
   property myHeight: String read GetmyHeight write SetmyHeight;
   property BgColor: String read GetBgColor write SetBgColor;
   property Alignment:String read GetAlignment write SetAlignment;

  end;

type
  TXTabSheet = class(TWrapperPanel)
  private
    FAlignChildrenVertical:Boolean;
    FIsSelected:Boolean;
    function GetName:string;
    function GetHint:string;
    function GetBgColor:string;
    function GetmyCaption:string;
    procedure SetMyName(AValue:string);
    procedure SetIsSelected(AValue: Boolean);
    procedure SetHint(AValue:string);
    procedure SetmyCaption(AValue:string);

    procedure SetMyEventTypes;

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);
    procedure SetBgColor(AValue:string); virtual;

  published
    { Published declarations }
    property AlignChildrenVertical:Boolean read FAlignChildrenVertical write FAlignChildrenVertical;
    property IsSelected: Boolean read FIsSelected write SetIsSelected default false;
    property Hint: String read GetHint write SetHint;
    property Name: String read GetName write SetMyName;
    property BgColor: String read GetBgColor write SetBgColor;
    property myCaption: String read GetmyCaption write SetmyCaption;

  end;

{$endif}

implementation

const MyNodeType='TXTabControl';

procedure TXTabControl.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;
procedure TXTabSheet.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}

procedure Register;
begin
  {$I xtabcontrol_icon.lrs}
  // Lazarus IDE component registration
  RegisterComponents('Misc',[TXTabControl]);
  RegisterNoIcon([TXTabSheet]);

  //special property editors
  //RegisterPropertyEditor (TypeInfo(string), TXScrollBox, 'ScrollType', TScrollBarsProperty);
  RegisterPropertyEditor (TypeInfo(string), TXTabControl, 'Alignment', TAlignmentProperty);

  // Hide some inherited properties
  RegisterPropertyEditor(TypeInfo(TAlign), TXTabControl, 'Align', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TAnchors), TXTabControl, 'Anchors', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'AutoScroll', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'AutoSize', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBiDiMode), TXTabControl, 'BiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentBiDiMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderSpacing), TXTabControl, 'BorderSpacing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlBorderStyle), TXTabControl, 'BorderStyle', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColor), TXTabControl, 'Color', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'DockSite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXTabControl, 'DragCursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragKind), TXTabControl, 'DragKind', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragMode), TXTabControl, 'DragMode', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'Enabled', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFont), TXTabControl, 'Font', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentFont', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXTabControl, 'HorzScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlScrollBar), TXTabControl, 'VertScrollBar', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentShowHint', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPopupmenu), TXTabControl, 'PopupMenu', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TXTabControl, 'TabOrder', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'TabStop', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'Visible', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Height', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Width', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Top', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Left', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCursor), TXTabControl, 'Cursor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TXTabControl, 'Tag', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpContext), TXTabControl, 'HelpContext', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(THelpType), TXTabControl, 'HelpType', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXTabControl, 'HelpKeyword', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TXTabControl, 'Caption', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXTabControl, 'ChildSizing', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TSizeConstraints), TXTabControl, 'Constraints', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ParentColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'HotTrack', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCustomImageList), TXTabControl, 'Images', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'MultiLine', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'MultiSelect', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'OwnerDraw', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'RaggedRight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ScrollOpposite', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabStyle), TXTabControl, 'Style', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(SmallInt), TXTabControl, 'TabHeight', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(longInt), TXTabControl, 'TabIndex', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabPosition), TXTabControl, 'TabPosition', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(SmallInt), TXTabControl, 'TabWidth', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCTabControlOptions), TXTabControl, 'Options', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'ShowTabs', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TXTabControl, 'IsContainer', THiddenPropertyEditor);



//.....TXTabControl events.......
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnChange', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TTabChangingEvent), TXTabControl, 'OnChanging', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnCloseTabClicked', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXTabControl, 'OnContextpopup', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDockDropEvent), TXTabControl, 'OnDockDrop', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDockOverEvent), TXTabControl, 'OnDockOver', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXTabControl, 'OnDragDrop', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXTabControl, 'OnDragOver', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXTabControl, 'OnEndDock', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXTabControl, 'OnEndDrag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnExit', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TGetDockCaptionEvent), TXTabControl, 'OnGetDockCaption', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TGetDockCaptionEvent), TXTabControl, 'OnGetDockCaption', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TTabGetImageEvent), TXTabControl, 'OnGetImageIndex', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TGetSiteInfoEvent), TXTabControl, 'OnGetSiteInfo', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabControl, 'OnMouseDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnMouseEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnMouseLeave', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXTabControl, 'OnMouseMove', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabControl, 'OnMouseUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXTabControl, 'OnMouseWheel', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabControl, 'OnMouseWheelDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabControl, 'OnMouseWheelUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabControl, 'OnResize', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStartDockEvent), TXTabControl, 'OnStartDock', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXTabControl, 'OnStartDrag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TUnDockEvent), TXTabControl, 'OnUnDock', THiddenPropertyEditor);

//.....TXTabSheet properties.......
RegisterPropertyEditor(TypeInfo(TBiDiMode), TXTabSheet, 'BiDiMode', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ParentBiDiMode', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TBorderWidth), TXTabSheet, 'BorderWidth', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TCaption), TXTabSheet, 'Caption', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TControlChildSizing), TXTabSheet, 'ChildSizing', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TCursor), TXTabSheet, 'Cursor', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'Enabled', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TFont), TXTabSheet, 'Font', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Height', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(THelpContext), TXTabSheet, 'HelpContext', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(THelpType), TXTabSheet, 'HelpType', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(String), TXTabSheet, 'HelpKeyword', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TImageIndex), TXTabSheet, 'ImageIndex', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Left', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Longint), TXTabSheet, 'PageIndex', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ParentFont', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ParentShowHint', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'ShowHint', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TPopupmenu), TXTabSheet, 'PopupMenu', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Top', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Tag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'TabVisible', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Integer), TXTabSheet, 'Width', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'AlignChildrenVertical', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(Boolean), TXTabSheet, 'IsContainer', THiddenPropertyEditor);

//.....TXTabSheet events.......
RegisterPropertyEditor(TypeInfo(TContextpopupEvent), TXTabSheet, 'OnContextpopup', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragDropEvent), TXTabSheet, 'OnDragDrop', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TDragOverEvent), TXTabSheet, 'OnDragOver', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TEndDragEvent), TXTabSheet, 'OnEndDrag', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnExit', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnHide', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabSheet, 'OnMouseDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnMouseEnter', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnMouseLeave', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseMoveEvent), TXTabSheet, 'OnMouseMove', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseEvent), TXTabSheet, 'OnMouseUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelEvent), TXTabSheet, 'OnMouseWheel', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabSheet, 'OnMouseWheelDown', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TMouseWheelUpDownEvent), TXTabSheet, 'OnMouseWheelUp', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnResize', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TNotifyEvent), TXTabSheet, 'OnShow', THiddenPropertyEditor);
RegisterPropertyEditor(TypeInfo(TStartDragEvent), TXTabSheet, 'OnStartDrag', THiddenPropertyEditor);

end;

constructor TXTabControl.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXTabControl.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTabControl.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  ControlStyle := ControlStyle - [csSetCaption];

  AutoSize:=false;
  ParentColor:=false;

  BorderStyle := bsNone;
  BorderWidth:=0;

  Caption:='';

  IsSelected:=false;
  IsContainer:=true;
  SelectionBorderColor:=glbSelectionBorderColor;

  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  MyEventTypes:=TStringList.Create;
  self.OnClick:=@self.TabControlClick;
  self.OnChange:=@self.TabPageChanged;
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
  // showmessage('setting default props');
  Hint:='';
  MyWidth:='300px';
  MyHeight:='300px';

end;

constructor TXTabSheet.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXTabSheet.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTabSheet.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin

  ControlStyle := ControlStyle - [csSetCaption];

  AutoSize:=false;
  Align:=alClient;
  ParentColor:=false;
  SelectionBorderColor:=glbSelectionBorderColor;

  BorderStyle := bsNone;
  BorderWidth:=0;

  IsSelected:=false;
  IsContainer:=true;
  AlignChildrenVertical:=true;

  MyEventTypes:=TStringList.Create;
  self.OnClick:=@self.TabSheetClick;
  //self.OnChangeBounds:=@self.TabSheetChangeBounds;

  self.myNode:=TDataNode.Create('UI',self.Name,'TXTabSheet',false);
  self.myNode.ScreenObject:=self;
  if (TheOwner is TXTabControl)
  and (TXTabControl(TheOwner).myNode<>nil) then
     self.myNode.MyForm:=TXTabControl(TheOwner).myNode.MyForm
  else if (TheOwner is TForm)  then          // should never happen (csAcceptsControls in TWinControl(NewParent).ControlStyle)
    self.myNode.MyForm:=TForm(TheOwner);

  self.SetMyEventTypes;
  self.myNode.myEventTypes:=self.myEventTypes;
  SetLength(self.myNode.myEventHandlers,self.myNode.myEventTypes.Count);

  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  Hint:='';
  myCaption:='NewPage';
  debugln('create done');
end;

function CreateTCWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXTabControl;
  NewNode:TDataNode;
begin
  NewWidget:=TXTabControl.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;
function CreateTSWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXTabSheet;
  NewNode:TDataNode;
begin
  NewWidget:=TXTabSheet.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

procedure TXTabControl.SetParent(NewParent: TWinControl);
begin
  inherited;
  ResetAlignment(TWinControl(self));
end;

procedure TXTabControl.TabControlClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;
procedure TXTabSheet.TabSheetClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;
//procedure TXTabSheet.TabSheetChangeBounds(Sender:TObject);
//begin
//  PaintSelectedRectangle(TCustomControl(self),self.GetClientRect,glbSelectionBorderColor,self.IsSelected);
//end;

procedure TXTabControl.TabPageChanged(Sender: TObject) ;
var
   MyTabControl : TPageControl;
   TabID:string;
begin
  if not (csDesigning in componentState) then
  begin
     if NOT (Sender is  TPageControl) then
     begin
       ShowMessage('In TabPageChanged expected TPageControl found '+Sender.ClassName) ;
       exit;
     end;
     MyTabControl:=TPageControl(Sender);
     CallHandleEvent('Change',IntToStr(MyTabControl.TabIndex),Sender);

     // do a Component Click event as well
     TabID:=MyTabControl.ActivePage.Name;
     CallHandleEvent('Click',TabID,Sender);
  end;

end;

function TXTabControl.GetActiveTabSheet: TXTabSheet;
begin
  debugln('XTabControl.GetActiveTabSheet');
  Result:=TXTabSheet(inherited ActivePageComponent);
end;
function TXTabControl.GetTabSheet(Index: Integer): TXTabSheet;
begin
  debugln('XTabControl.GetTabSheet');
  Result:=TXTabSheet(inherited Pages[Index]);
end;
procedure TXTabControl.SetActiveTabSheet(const AValue: TXTabSheet);
begin
  debugln(['XTabControl.SetActiveTabSheet ',DbgSName(Self),' ',DbgSName(AValue)]);
  ActivePageComponent := AValue;
end;
function TXTabControl.GetPageClass: TCustomPageClass;
begin
  debugln('XTabcontrol.GetPageClass');
  Result := TXTabSheet;
end;
function TXTabControl.FindNextPage(CurPage: TXTabSheet; GoForward,
  CheckTabVisible: Boolean): TXTabSheet ;
var
  I, StartIndex: Integer;
begin
  debugln('TXTabControl.FindNextPage');
  Result := nil;
  if PageCount = 0 then
    exit;
  StartIndex := IndexOf(CurPage);
  if StartIndex < 0 then
    if GoForward then
      StartIndex := PageCount - 1
    else
      StartIndex := 0;
  i := StartIndex;
  repeat
    if GoForward then
    begin
      Inc(i);
      if i = PageCount then
        i := 0;
    end else
    begin
      if i = 0 then
        i := PageCount;
      Dec(I);
    end;
    if not CheckTabVisible or Pages[i].TabVisible then
    begin
      Result := Pages[i];
      exit;
    end;
  until i = StartIndex;
end;
procedure TXTabControl.SelectNextPage(GoForward: Boolean);
begin
  debugln('TXTabControl.SelectNextPage1');
  SelectNextPage(GoForward,true);
end;
procedure TXTabControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);
var
  NextPage: TXTabSheet;
begin
   debugln('TXTabControl.SelectNextPage2');
 NextPage:=FindNextPage(ActivePage,GoForward,CheckTabVisible);
  if NextPage<>nil then ActivePage:=NextPage;
end;


{$else}
constructor TXTabControl.Create(MyForm:TForm;NodeName:String);
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
end;
constructor TXTabSheet.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:='TXTabSheet';
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=true;

  BgColor:='#FFFFFF';
end;


function CreateTabControl(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  OnChangeString, OnClickString, OnPasteString, BgColor:String;
begin

  BgColor:=MyNode.GetAttribute('BgColor',true).AttribValue;
  if BgColor='' then BgColor:='#FFFFFF';

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', this.value, '''');" ';
  //showmessage('scrollbox createwidget');

  asm
    try{
    // ----------------------------------------check if the style has already been set
    var x = document.getElementsByTagName("STYLE");
    var StyleIsSet = false;
    if (x.length>0){
      for (var i=0; i<x.length; i++){
        var y= x[i].innerHTML;
        if (y.indexOf("div.TabPage") !=-1) { StyleIsSet =true}
      }
    }

    // ----------------------------------------add style if not already been set
    if (StyleIsSet == false)
    {
      // ----------------------------Define the styling to be used for  "TabPage"
         var Styletext='<style type="text/css">';
         Styletext=Styletext+'div.TabPage { background-color:'+BgColor+'; height:96%; width:100%}';
         Styletext=Styletext+'</style>';

      //----------------------------- now append the style declarations to the head of the HTML page
         document.head.innerHTML = document.head.innerHTML+Styletext;
    }



    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';

    //localcontainer is an inner div.  Its id is  ScreenObjectName+'Contents'
    // It is a child of the outer container div (wrapper)
    //
     var localcontainer = document.createElement("div");
     localcontainer.id = ScreenObjectName+'Contents';
     localcontainer.style.display="inline-block;";
     localcontainer.style.height="100%";
     localcontainer.style.width="100%";
     document.getElementById(ScreenObjectName).appendChild(localcontainer);

  // -----------------------------Define the HTML to be used to create the Tab control
  // NB --- "TabButton" and "TabPage" are the classnames used for styling the tab controls
  // -------"TabButtonDiv" is the classname used for styling the div containing the tab buttons

    var TabButtonsDef = '<div id="'+ScreenObjectName+'ContentsButtons'+'" class="TabButtonDiv"'+
                        '>'+
                        '</div>';

  //------------------------------------ now append the declarations to the Parent
     localcontainer.innerHTML = localcontainer.innerHTML + TabButtonsDef;


    var wrapper=document.getElementById(ScreenObjectName);
    //MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XTabControl.CreateTabControl');}

end;

  MyNode.ScreenObject:=MyNode;

    // now that we have a datanode and a widget, cycle attribute settings
    TXTabControl(myNode).myWidth:=TXTabControl(myNode).myWidth;
    TXTabControl(myNode).myHeight:=TXTabControl(myNode).myHeight;
  TXTabControl(myNode).Alignment:=TXTabControl(myNode).Alignment;
  TXTabControl(myNode).LabelPos:=TXTabControl(myNode).LabelPos;
  TXTabControl(myNode).BgColor:=TXTabControl(myNode).BgColor;
  TXTabControl(myNode).Hint:=TXTabControl(myNode).Hint;

result:=myNode;
end;

function openTab(TabName,TabControlName:string) :string;
begin

asm
try{
    var i;
 //alert('OpenTab  TabControl='+TabControlName+' TabName='+TabName);
    var x = document.getElementsByClassName(TabControlName);
    if (x==null) {alert('cannot find element by class name '+TabControlName);}
    for (i = 0; i < x.length; i++) {
       x[i].style.display = "none";
    }
    //alert('1');
   var y = document.getElementsByClassName(TabControlName+'TabButton');
   if (y==null) {alert('cannot find element by class name '+TabControlName+'TabButton');}
    for (i = 0; i <y.length; i++) {
       y[i].style.background ='#d1d0ce';// dark background when not selected
       y[i].style.border= 'none';
    }
    var selectedTab = document.getElementById(TabName);
    selectedTab.style.display = "block";
    var selectedTab = document.getElementById(TabName+'Contents');
    selectedTab.style.display = "block";

    var selectedTabButton = document.getElementById(TabName+'Button');
    if (selectedTabButton==null) {alert('cannot find element by name '+TabName+'Button');}
    selectedTabButton.style.background = '#f1f0ee'; // Same background color as the tab page when selected

    } catch(err) {alert('Error in XTabControl.OpenTab '+ err.message);}
end;

end;

function ChangeTabPage(nodeId:string):string;
var
  ParentNode:TDataNode;
begin
   // showmessage('changetabpage '+nodeId);
    ParentNode:=FindParentOfNode(SystemNodeTree,NodeId);
   // showmessage('calling openTab('+NodeId+','+ParentNode.NodeName+'Contents)');
    openTab(NodeId,ParentNode.NodeName+'Contents');
end;

function CreateTabSheet(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  ParentName,PageCaption,NodeID:string;
  OnClickString:String;
begin
  ParentName:=MyNode.GetAttribute('ParentName',false).AttribValue+'Contents';
  //PageCaption:=ScreenObjectName;
  PageCaption:=MyNode.GetAttribute('myCaption',false).AttribValue;
  NodeID:=MyNode.NodeName;

  OnClickString:='onclick="event.stopPropagation();pas.XTabControl.ChangeTabPage('''+NodeID+'''); '+
                         'pas.Events.handleEvent(''Change'','''+ScreenObjectName+''','''+ScreenObjectName+''','''');' +
                         'pas.Events.handleEvent(''Click'','''+NodeID+''', '''', ''''); '+
                         '" ';
 //   showmessage('tabsheet createwidget');

  asm
    try{
    //alert('pagecaption='+PageCaption+' parent='+ParentName);

    //var ParentItem = document.getElementById(ParentName);
    var ButtonsDiv = document.getElementById(ParentName+'Buttons');

    var buttonstring ='<button id="'+ScreenObjectName+'Button" class="'+ParentName+'TabButton" ' +
                             OnClickString +
                          '>'+PageCaption+'</button>';
    ButtonsDiv.innerHTML = ButtonsDiv.innerHTML + buttonstring;

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,'TXTabSheet',position);
    wrapper.style.overflow = 'hidden';
    wrapper.style.height = '100%';
    wrapper.style.width = '100%';
    wrapper.className='TabPage  '+ ParentName;

    var TabContentDef ="<div id='" +ScreenObjectName+"Contents'  class='TabPage  "+ ParentName+"' ></div>";
    wrapper.innerHTML = wrapper.innerHTML + TabContentDef;

    var wrapper=document.getElementById(ScreenObjectName);
    //MyNode.ScreenObject=wrapper;
  }
  catch(err) { alert(err.message+'  in XTabControl.CreateTabSheet');}
end;

  MyNode.ScreenObject:=MyNode;
  TXTabSheet(myNode).Hint:=TXTabSheet(myNode).Hint;

result:=myNode;
end;

function CreateTabControlInterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXTabControl.Create(MyForm,NodeName));
end;
function CreateTabPageInterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXTabSheet.Create(MyForm,NodeName));
end;
{$endif}


function TXTabControl.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXTabSheet.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;
function TXTabControl.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXTabSheet.GetHint:string;
begin
  result:=myNode.GetAttribute('Hint',true).AttribValue;
end;
function TXTabControl.GetmyWidth:string;
begin
  result:=myNode.GetAttribute('myWidth',true).AttribValue;
end;
function TXTabControl.GetmyHeight:string;
begin
  result:=myNode.GetAttribute('myHeight',true).AttribValue;
end;
function TXTabControl.GetAlignment:String;
begin
  result:=MyNode.getAttribute('Alignment',true).AttribValue;
end;
function TXTabSheet.GetmyCaption:string;
begin
  result:=myNode.GetAttribute('myCaption',true).AttribValue;
end;
procedure TXTabSheet.SetmyCaption(AValue:string);
begin
  myNode.SetAttributeValue('myCaption',AValue);
  {$ifndef JScript}
  Caption:=AValue;
  {$else}
  asm
  end;
  {$endif}
end;


procedure TXTabControl.SetMyName(AValue:string);
begin
  {$ifndef JScript}
  inherited Name:=AValue;
  {$else}
  self.Name:=AValue;
  asm
     var ob = document.getElementById(this.NodeName);
     ob.id = AValue;
     inner = document.getElementById(this.NodeName+'Contents');
     if (inner != null) {
       inner.id = AValue+'Contents';
       }
  end;
  {$endif}

  if myNode<>nil then
     myNode.NodeName:=AValue;
  // also rename any associated event code ???
end;
procedure TXTabSheet.SetMyName(AValue:string);
begin
  {$ifndef JScript}
  inherited Name:=AValue;
  {$else}
  self.Name:=AValue;
  asm
     var ob = document.getElementById(this.NodeName);
     ob.id = AValue;
     inner = document.getElementById(this.NodeName+'Contents');
     if (inner != null) {
       inner.id = AValue+'Contents';
       }
  end;
  {$endif}

  if myNode<>nil then
     myNode.NodeName:=AValue;
  // what else might need renaming ???
end;

procedure TXTabControl.SetIsSelected(AValue: Boolean);
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
procedure TXTabSheet.SetIsSelected(AValue: Boolean);
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

{$ifndef JScript}
// Name is the first property loaded from .lfm
// Hijacking this so that we can decide which set of default values to apply for other properties
// (because there is a problem produced for string properties which are NOT saved to lfm
// when the value is blank).
procedure TXTabControl.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);

  if (csLoading in componentState) then
  begin
//    showmessage('TXTabControl setname. loading.');
    MyWidth:='';
    MyHeight:='';
    Hint:='';
  end;

end;
procedure TXTabSheet.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);

  if (csLoading in componentState) then
  begin
//    showmessage('TXTabSheet setname. loading.');
    MyCaption:='';
    Hint:='';
  end;

end;
//procedure TXTabControl.Paint;
//begin
//  inherited Paint;
//  PaintSelectedRectangle(TCustomControl(self),self.GetClientRect,self.SelectionBorderColor,self.IsSelected);
//end;
{$endif}

procedure TXTabControl.SetmyWidth(AValue:string);
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

procedure TXTabControl.SetmyHeight(AValue:string);
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

procedure TXTabControl.SetHint(AValue:string);
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
procedure TXTabSheet.SetHint(AValue:string);
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

procedure TXTabControl.SetAlignment(AValue:string);
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

function TXTabSheet.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean;
var
  i,j:integer;
  NewList:TFPList;
  tmp:TControl;
begin
  AControlList.Sort(@SortAlignList);
  inherited DoAlignChildControls(TheAlign,AControl,AControlList,ARect);
end;

procedure TXTabControl.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;
procedure TXTabSheet.SetSelectionBorderColor(AValue: TColor);
// Colour of the 'IsSelected' dotted border
begin
  if AValue<>FSelectionBorderColor then
    begin
      FSelectionBorderColor:=AValue;
      Repaint;
    end;
end;
//procedure TXTabSheet.Paint;
//begin
//  inherited Paint;
//  PaintSelectedRectangle(TCustomControl(self),self.GetClientRect,self.SelectionBorderColor,self.IsSelected);
//end;

function TXTabControl.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
function TXTabSheet.GetBgColor:TColor;
begin
  result:=HexRGBToColor(myNode.GetAttribute('BgColor',true).AttribValue);
end;
procedure TXTabControl.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;
procedure TXTabSheet.SetBgColor(AValue:TColor);
var str:string;
begin
  if myNode<>nil then
  begin
    str:= ColorToHexRGB(AValue);
    myNode.SetAttributeValue('BgColor',str,'Color');
  end;

  Color:=AValue;
end;
{$else}
function TXTabControl.GetBgColor:string;
begin
  result:=myNode.GetAttribute('BgColor',true).AttribValue;
end;
function TXTabSheet.GetBgColor:string;
begin
  result:=myNode.GetAttribute('BgColor',true).AttribValue;
end;
procedure TXTabControl.SetBgColor(AValue:string);
begin
  SetAttributeValue('BgColor',AValue,'Color');

  asm
  try {
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    ob.style.backgroundColor = AValue;  }
    } catch(err) { alert(err.message+'  in XTabControl.SetBgColor'); }
  end;

end;
procedure TXTabSheet.SetBgColor(AValue:string);
begin
  SetAttributeValue('BgColor',AValue,'Color');

  asm
  try {
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    ob.style.backgroundColor = AValue;  }
    } catch(err) { alert(err.message+'  in XTabControl.SetBgColor'); }
  end;

end;
{$endif}


{$ifndef JScript}
procedure TXTabControl.SortOutAlignment;
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


{ TXTabControlComponentEditor }

const
  nbvAddPage       = 0;
  nbvInsertPage    = 1;
  nbvDeletePage    = 2;
  nbvMovePageLeft  = 3;
  nbvMovePageRight = 4;
  nbvShowPage      = 5;

procedure TXTabControlComponentEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem:=TMenuItem(Sender);
  if (AMenuItem=nil) or (not (AMenuItem is TMenuItem)) then exit;
  NewPageIndex:=AMenuItem.MenuIndex;
  if (NewPageIndex<0) or (NewPageIndex>=TabControl.PageCount) then exit;
  TabControl.PageIndex:=NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(TabControl.CustomPage(TabControl.PageIndex));
end;

procedure TXTabControlComponentEditor.AddNewPageToDesigner(Index: integer);
var
  Hook: TPropertyEditorHook;
  NewPage: TCustomPage;
  NewName: string;
  i:integer;
begin
  debugln('AddNewPageToDesigner. index='+inttostr(index));
  Hook:=nil;
  if not GetHook(Hook) then exit;
  NewPage:=TabControl.CustomPage(Index);
  NewName:=GetDesigner.CreateUniqueComponentName(NewPage.ClassName);
  NewPage.Caption:=NewName;
  NewPage.Name:=NewName;
  TabControl.PageIndex:=Index;

  Hook.PersistentAdded(TXTabSheet(NewPage),true);
  Modified;
  debugln('AddNewPageToDesigner done');
end;

function TXTabControl.AddTabSheet: TXTabSheet;
begin
  debugln('addtabsheet');
  Result := TXTabSheet.Create(Self.Owner,self.myNode.IsDynamic);
  Result.PageControl := Self;
  Result.Parent:=Self;
end;

procedure TXTabControlComponentEditor.DoAddPage;
var
  Hook: TPropertyEditorHook;
  newPage:TXTabSheet;
  lPageName:string;
begin
  debugln('doaddpage');
  Hook:=nil;
  if not GetHook(Hook) then exit;

  lPageName := Designer.CreateUniqueComponentName(TXTabSheet.ClassName);
  NewPage := TabControl.AddTabSheet;
  NewPage.Name:=lPageName;
  NewPage.Caption:=lPageName;
  TabControl.PageIndex:=TabControl.PageCount-1;

  Hook.PersistentAdded(NewPage, True);
  Modified;
  if Designer <> nil then Designer.Modified;
  TabControl.Invalidate;


end;

procedure TXTabControlComponentEditor.DoInsertPage;
var
  NewIndex: integer;
  Hook: TPropertyEditorHook;
  newPage:TXTabSheet;
  lPageName:string;
begin
  debugln('doinsertpage');
  Hook:=nil;
  if not GetHook(Hook) then exit;

  NewIndex:=TabControl.PageIndex;
  if NewIndex<0 then NewIndex:=0;

  lPageName := Designer.CreateUniqueComponentName(TXTabSheet.ClassName);
  NewPage := TabControl.AddTabSheet;
  NewPage.Name:=lPageName;
  NewPage.Caption:=lPageName;
  TabControl.PageIndex:=NewIndex;

  Hook.PersistentAdded(NewPage, True);
  Modified;
  if Designer <> nil then Designer.Modified;
  TabControl.Invalidate;

end;

procedure TXTabControlComponentEditor.DoDeletePage;
var
  Hook: TPropertyEditorHook;
  OldIndex: integer;
  PageComponent: TPersistent;
begin
  OldIndex:=TabControl.PageIndex;
  if (OldIndex>=0) and (OldIndex<TabControl.PageCount) then begin
    if not GetHook(Hook) then exit;
    PageComponent := TPersistent(TabControl.Pages[OldIndex]);
    Hook.DeletePersistent(PageComponent);
  end;
end;

procedure TXTabControlComponentEditor.DoMoveActivePageLeft;
var
  Index: integer;
begin
  Index:=TabControl.PageIndex;
  if (Index<0) then exit;
  DoMovePage(Index,Index-1);
end;

procedure TXTabControlComponentEditor.DoMoveActivePageRight;
var
  Index: integer;
begin
  Index:=TabControl.PageIndex;
  if (Index>=0)
  and (Index>=TabControl.PageCount-1) then exit;
  DoMovePage(Index,Index+1);
end;

procedure TXTabControlComponentEditor.DoMovePage(
  CurIndex, NewIndex: Integer);
begin
  TabControl.Pages[CurIndex].PageIndex:=NewIndex;
  //.Pages.Move(CurIndex,NewIndex);
  Modified;
end;

procedure TXTabControlComponentEditor.AddMenuItemsForPages(
  ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
begin
  ParentMenuItem.Enabled:=TabControl.PageCount>0;
  for i:=0 to TabControl.PageCount-1 do begin
    NewMenuItem:=TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name:='ShowPage'+IntToStr(i);
    NewMenuItem.Caption:=TabControl.CustomPage(i).Name+' "'+TabControl.Pages[i].Caption+'"';
    NewMenuItem.OnClick:=@ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TXTabControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    nbvAddPage:       DoAddPage;
    nbvInsertPage:    DoInsertPage;
    nbvDeletePage:    DoDeletePage; // beware: this can free the editor itself
    nbvMovePageLeft:  DoMoveActivePageLeft;
    nbvMovePageRight: DoMoveActivePageRight;
  end;
end;

function TXTabControlComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    nbvAddPage:       Result:=nbcesAddPage;
    nbvInsertPage:    Result:=nbcesInsertPage;
    nbvDeletePage:    Result:=nbcesDeletePage;
    nbvMovePageLeft:  Result:=nbcesMovePageLeft;
    nbvMovePageRight: Result:=nbcesMovePageRight;
    nbvShowPage:      Result:=nbcesShowPage;
  else
    Result:='';
  end;
end;

function TXTabControlComponentEditor.GetVerbCount: Integer;
begin
  Result:=6;
end;

procedure TXTabControlComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    nbvAddPage:       ;
    nbvInsertPage:    AnItem.Enabled:=TabControl.PageIndex>=0;
    nbvDeletePage:    AnItem.Enabled:=TabControl.PageIndex>=0;
    nbvMovePageLeft:  AnItem.Enabled:=TabControl.PageIndex>0;
    nbvMovePageRight: AnItem.Enabled:=TabControl.PageIndex<TabControl.PageCount-1;
    nbvShowPage:      AddMenuItemsForPages(AnItem);
  end;
end;

function TXTabControlComponentEditor.TabControl: TXTabControl;
begin
  Result:=TXTabControl(GetComponent);
end;
{$else}

procedure TXTabControl.SortOutAlignment;
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
 } catch(err) { alert(err.message+'  in XTabControl.SortOutAlignment'); }
end;

end;

{$endif}

begin
  {$ifndef JScript}
  RegisterComponentEditor(TXTabControl, TXTabControlComponentEditor);
  AddNodeFuncLookup('TXTabControl',@CreateTCWidget);
  AddNodeFuncLookup('TXTabSheet',@CreateTSWidget);
  {$else}
  AddNodeFuncLookup('TXTabControl',@CreateTabControlInterfaceObj,@CreateTabControl);
  AddNodeFuncLookup('TXTabSheet',@CreateTabPageInterfaceObj,@CreateTabSheet);
  {$endif}
end.
