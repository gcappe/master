unit Events;
{$ifndef JScript}
interface
uses
  Classes, LCLType,SysUtils, FileUtil, DateTimePicker,  Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Menus,   ColorBox,Clipbrd,
  ComCtrls, Spin,lclintf , EditBtn,
  NodeUtils, StringUtils, LazsUtils;
{$Else}
interface
uses SysUtils,Classes,NodeUtils, StringUtils, HTMLUtils;

function isValidSystemData(SystemDescription:string):boolean;
{$endif}

{$ifndef JScript}
type
  TEventClass = class
    procedure ComponentClick(Sender: TObject) ;
    procedure HandleResizeComponent(Sender:TObject);
  end;

type TLoadTimerTag = class
     public
       systemstring:string;
     end;

procedure CallHandleEvent(EventType,MyValue:string; MyControl:TObject);

var   EventCode : TEventClass;   // contains event handling procedures
{$endif}

procedure handleEvent(MyEventType,nodeID,myValue,PropName:string);     overload;
procedure handleEvent(MyEventType,nodeID,myValue:string);     overload;

var   SuppressEvents:Boolean;

implementation

{$ifndef JScript}
procedure CallHandleEvent(EventType,MyValue:string; MyControl:TObject);
var
  nodeID:string;
  outer:TWinControl;
begin
   if (not SuppressEvents)
   and (MyControl<>nil)
   and (TWinControl(MyControl).Parent<>nil) then
   begin
     // showmessage(EventType);
     outer:= FindOuterParentOf(TWinControl(MyControl));
     if outer<>nil then
     begin
       nodeID:= outer.Name;
       handleEvent(EventType,nodeID,MyValue);
     end;
   end;
end;

procedure TEventClass.ComponentClick(Sender: TObject) ;
begin
    CallHandleEvent('ComponentClick','',Sender);
end;

procedure TEventClass.HandleResizeComponent(Sender:TObject);
var
    myComponent,pr:TWinControl;
begin
   myComponent:= TWinControl(Sender);
   CheckPercentageSizing(myComponent);
end;

//function mygetClipboardData(stringname:string):string;
//begin
// // ShowMessage('Loading new '+ stringname+' data from the clipboard');
//  result :=  Clipboard.AsText;
//end;


{$Else}

implementation

function isValidSystemData(SystemDescription:string):boolean;
var teststring,teststring2:string;
   i:integer;
   MatchFound:boolean;
begin
  MatchFound:=true;
  teststring :='xp= @|';
  teststring2:='<Root|; Class |=R';
  for i :=1 to Length(teststring) do
  begin
     if (SystemDescription[i]<> teststring[i]) and (SystemDescription[i]<> teststring2[i])
     then  MatchFound:=false;
  end;
  result:=MatchFound;
end;

procedure DoSystemLoad(SystemDescription:string);
begin
  loadingSystem:=true;
   if (isValidSystemData(SystemDescription)=true)
   then
   begin
    asm
    $impl.XMLToNodeTree(SystemDescription);
    end;
   end
   else
     ShowMessage('Error.....Put a valid system description string on the clipboard before calling this option ');

     loadingSystem:=false;
end;

{$endif}

{$ifndef JScript}
procedure RunComponentEvent(myName,EventType:string;MyNode:TDataNode;MyValue:string);
var
  m: TMethod;
begin
  m.Code := MyNode.MyForm.MethodAddress(myName+'Handle'+EventType); //find method code
  if m.Code=nil then
  begin
    // the component may have been created dynamically at run-time.
    // in which case look for a registered event.
    m := TMethod(MyNode.FindRegisteredEvent(EventType));
  end;
  if m.Code=nil then
    EXIT                 // no handler has been defined
  else
  begin
    m.Data := pointer(MyNode.MyForm); //store pointer to form object instance
    TEventHandler(m)(myName,myNode,myValue);
  end;
end;
{$else}
procedure RunComponentEvent(myName,EventType:string;MyNode:TDataNode;MyValue:string);
begin

asm
try {
//alert('RunComponentEvent looking for '+myName+'Handle'+EventType);
//alert('RunComponentEvent NodeName='+MyNode.NodeName);

  var fn=null;
  if (MyNode.MyForm!=null) {
    fn = MyNode.MyForm[myName+'Handle'+EventType];
    if (fn!=null) {
      fn = fn.bind(MyNode.MyForm);     // so that the 'this' context will be preserved
    }
  }

  if (fn==null) {
  // the component may have been created dynamically at run-time.
  // in which case look for a registered event.
    fn = MyNode.FindRegisteredEvent(EventType);
  }
  if (fn!=null)  {
     fn(myName,MyNode,MyValue);
     // alert('function done.');
  }
}catch(err) { alert(err.message+'  in NodeUtils.RunComponentEvent '+myName+' '+EventType);}
end;

end;
{$endif}

 function ExecuteEventHandlers(MyEventType,nodeID,myValue:string;myNode:TDataNode): String ;
  var
     i,NumHandlers:integer;
  begin
    //ShowMessage('ExecuteEventHandlers. '+MyEventType+' NodeId='+nodeID+' value='+myValue);
    NumHandlers:= myNode.MyEventTypes.count;
    for i:=0 to NumHandlers - 1 do
    begin
      if  myNode.MyEventTypes[i]=MyEventType then
      begin
        // Execute the registered event handler if it exists
        RunComponentEvent(nodeID,MyEventType,myNode,myValue);
      end;
    end;
end;

procedure  handleEvent(MyEventType,nodeID,myValue,PropName:string);
var CurrentNode :TDataNode;
  DoContinue:Boolean;
  m: TMethod;
begin
  if MyEventType='notnow' then EXIT;


 if StartingUp = false then
 begin
   //ShowMessage('handle event...'+MyEventType+' '+nodeID);

     // Identify the system node.
     // nodeID is the screen object name (nodename in the TDataNode tree)
     //   **** exception - HTML TreeNode events send in the name of the node object
     {$ifndef JScript}
     CurrentNode:=FindDataNodeById(SystemNodeTree,nodeID,false);
     {$else}
     if (MyEventType = 'TreeNodeClick')
     or (MyEventType = 'DragStart')
     or (MyEventType = 'Drop')
     then
     begin
       // try to get the system node name from the tree node name...
       CurrentNode:=GetDataNodeFromTreeNode(nodeID);
     end
     else
       CurrentNode:=FindDataNodeById(SystemNodeTree,nodeID,false);
     {$endif}

     //if CurrentNode=nil then
     //  ShowMessage('handleEvent.'+MyEventType+'   Cannot find node '+nodeID)
    // else
    //   showmessage('node found');

    if (CurrentNode<>nil)
    and (MainForm<>nil) then
    begin
        DoContinue:=true;
        {$ifdef JScript}
        // discover links that the new value needs pushing to
        if (PropName<>'') and (PropName<>'undefined') then
        begin
          //showmessage('propname=>'+PropName+'<');
          PushToLinks(CurrentNode,PropName,myValue, SystemNodeTree);  // propname is the changed property in CurrentNode
        end;
        {$endif}

        // Run the generic event handler that is defined in the project main form by the project designer (if any)
        {$ifndef JScript}
        m.Code := MainForm.MethodAddress('HandleGenericEvent'); //find method code
        if m.Code<>nil then
        begin
          m.Data := pointer(MainForm); //store pointer to object instance
          DoContinue:=TGenericHandler(m)(MyEventType,myValue,CurrentNode);
        end;
        {$else}
        asm
        var fn=null;
        if (pas.NodeUtils.MainForm!=null) {
          fn = pas.NodeUtils.MainForm['HandleGenericEvent'];
          if (fn!=null)  {
             //alert('running generic handler');
             var cn = fn(MyEventType,myValue,CurrentNode);
             DoContinue=cn;
             }
        }
        end;
        {$endif}

        if DoContinue then
        begin
          // run the specific event handler defined in the form for this component and event type (if any)
          //showmessage('calling ExecuteEventHandlers');
          ExecuteEventHandlers(MyEventType,CurrentNode.nodeName,myValue,CurrentNode) ;
        end;

     end;
 end;
end;

procedure  handleEvent(MyEventType,nodeID,myValue:string);
begin
  handleEvent(MyEventType,nodeID,myValue,'');
end;
//======================================================================================================
//============================================  End of Common Events Code ================================
//======================================================================================================
Begin
 handleEvent('notnow','notnow','');   // force pas2JS compiler to keep the procedure
 {$ifndef JScript}
  EventCode := TEventClass.Create;
{$endif}

end.
