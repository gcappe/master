unit JSplitter;

{$mode objfpc}{$H+}
{$modeswitch externalclass}

interface

uses
  Classes, SysUtils, JS, Web, JElement, JPanel;

type
  TJSEvtHandler = function(event: TJSEvent): boolean of object;

  JTouchEventHandlers = class external name 'TouchEventHandlers'
  public
    ontouchstart: TJSEvtHandler;
    ontouchend: TJSEvtHandler;
    ontouchmove: TJSEvtHandler;
    ontouchenter: TJSEvtHandler;
    ontouchleave: TJSEvtHandler;
    ontouchcancel: TJSEvtHandler;
  end;

type
  TWSplitter = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    PanelLeft, PanelRight, ReSizer: TWPanel;
  end;

implementation

{ TWSplitter }

constructor TWSplitter.create(parent: TCustomControl);

  procedure doSplitterOnReadyExecute(sender: TObject);

    function doReSizerOnTouchStart(event : TJSEvent): Boolean;
    begin
      touch2Mouse(event);
    end;

    function doReSizerOnMouseDown(e : TJSMouseEvent): Boolean;
    var
      saveX : NativeInt;
      function doOnMouseMove(e : TJSMouseEvent): Boolean;
      begin
        PanelRight.left := PanelRight.Left - Trunc(saveX - e.clientX);
        saveX := Trunc(e.clientX);
        PanelRight.width := self.Width - PanelRight.Left;
        PanelLeft.SetProperty ('cursor','w-resize');
        PanelRight.SetProperty('cursor','w-resize');
      end;
    Begin
      saveX := Trunc(e.clientX);
      TJSHTMLElement(self.Handle).onmousemove := @doOnMouseMove;
    end;

    function doSplitterOnMouseUp(event : TJSMouseEvent): Boolean;
      function doOnMouseMove(e : TJSMouseEvent): Boolean;
      begin
        //nullify mousemove
      end;
    Begin
      PanelLeft.SetProperty ('cursor','default');
      PanelRight.SetProperty('cursor','default');
      TJSHTMLElement(self.Handle).onmousemove := @doOnMouseMove;
    end;

  Begin
    console.log('OnReadyExecute');

    PanelLeft.SetProperty('height','100%');
    PanelLeft.SetProperty('width','100%');

    PanelRight.SetProperty('height','100%');
    PanelRight.Width := trunc(self.width/2);
    PanelRight.Left  := trunc(self.width/2);

    ReSizer.SetProperty('height','100%');

  //
  // event handling splitter movement
  //
    //mapping touchstart to mousedown, touchend to mouseup and touchmove to mousemove
    //see touch2Mouse in JElement.
    JTouchEventHandlers(ReSizer.Handle).ontouchstart := @doReSizerOnTouchStart;
    JTouchEventHandlers(ReSizer.Handle).ontouchmove  := JTouchEventHandlers(ReSizer.Handle).ontouchstart;
    JTouchEventHandlers(ReSizer.Handle).ontouchend   := JTouchEventHandlers(ReSizer.Handle).ontouchstart;

    TJSHTMLElement(ReSizer.Handle).onmousedown := @doReSizerOnMouseDown;

    TJSHTMLElement(self.Handle).onmouseup := @doSplitterOnMouseUp;

  end;


Begin
  inherited Create('div', parent);

  PanelLeft := TWPanel.Create(self);
  PanelRight := TWPanel.Create(self);

  ReSizer := TWPanel.Create(PanelRight);
  ReSizer.SetProperty('background-color','#ccc');
  ReSizer.SetProperty('cursor','w-resize');
  ReSizer.width := 4;

  //self.Observe;
  self.OnReadyExecute := @doSplitterOnReadyExecute;
end;

end.

