unit jFlipScroll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, types, JElement, JImage;

type
  TWFlipScroll = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure GotoPage(page: integer);
    procedure AutoScroll(destination, duration: integer);
  end;

implementation

{ TWFlipScroll }

constructor TWFlipScroll.Create(parent: TCustomControl);
  function doFlipScrollOnLoad(event: TEventListenerEvent): boolean;
    procedure doContentWindowMessage(evt: TJSObject);
    var
      iframeheight: Integer;
    begin
      iframeheight := StrToInt(StrBefore(TJSHTMLElement(self.Handle).style.getPropertyValue('height'), 'px'));
      AutoScroll(iframeheight * strtoint(String(evt['data'])), 250);
    end;

  begin
    TJSHTMLIFrameElement(self.Handle).contentWindow.addEventListener('message',@doContentWindowMessage);
  end;

begin
  inherited Create('iframe', parent);

  TJSHTMLElement(self.Handle).onload := @doFlipScrollOnLoad;

end;

Procedure TWFlipScroll.AutoScroll(destination, duration: integer);
var
  timer: NativeInt;
  scrollStep: double;

  procedure doContentWindowTimer;
  begin
    If TJSHTMLIFrameElement(self.Handle).contentWindow.scrollY < destination then
      TJSHTMLIFrameElement(self.Handle).contentWindow.scrollBy(0, Trunc(scrollStep)) else
      TJSHTMLIFrameElement(self.Handle).contentWindow.clearInterval(timer);
  end;


begin
  scrollStep := -(TJSHTMLIFrameElement(self.Handle).contentWindow.scrollY - destination) / (duration / 15);
  timer := TJSHTMLIFrameElement(self.Handle).contentWindow.setInterval(@doContentWindowTimer, 15);
end;

Procedure TWFlipScroll.GotoPage(page: integer);
  procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';

begin
  { TODO warleyalex }
///  TJSHTMLIFrameElement(self.Handle).contentWindow.postMessage(inttostr(page),'*');
end;

end.

