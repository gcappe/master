unit jListBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement, JPanel;

type
  TWListBox = class(TCustomControl)
  public
    ItemCount : integer;
    constructor Create(parent: TCustomControl); virtual;
    Procedure Add(item: TCustomControl);
    Procedure Clear;
  end;

implementation

uses
  jGlobals;

{ TWListBox }

constructor TWListBox.Create(parent: TCustomControl);
var
  atop, aheight: Integer;

  function doListBox(event : TJSUIEvent): Boolean;
  var
    i: integer;
    c: TJSHTMLCollection;
  begin
      c := self.Handle.children;
      for i := 0 to c.length -1 do
      begin
        atop    := StrToInt(StrBefore(TJSHTMLElement(c[i]).style.getPropertyValue('top'), 'px'));
        aheight := StrToInt(StrBefore(TJSHTMLElement(c[i]).style.getPropertyValue('height'), 'px'));

        TJSHTMLElement(c[c.length-1]).style.setProperty('display', 'inline-block');          //set last one visible

        if ((atop + aheight) < self.Handle.scrollTop) and       //all previous visible set to none
           (TJSHTMLElement(c[i]).style.getPropertyValue('display') = 'inline-block') then
          TJSHTMLElement(c[i]).style.setProperty('display', 'none');

        if (atop + aheight >= self.Handle.scrollTop) and        //if in viewport make visible
           (atop <= self.Handle.scrollTop + self.height + 2) then
        begin
          TJSHTMLElement(c[i]).style.setProperty('display' ,'inline-block');
        end;

        if (atop > self.Handle.scrollTop + self.height + 2) and   //if past viewport then set invisible
           (TJSHTMLElement(c[i]).style.getPropertyValue('display') = 'inline-block') and
           (i < c.length-1) then
          TJSHTMLElement(c[i]).style.setProperty('display', 'none');
      end;
  end;

begin
  inherited Create('div', parent);

  ItemCount := 0;
  self.Handle.setAttribute('will-change', 'transform');   //'scroll-position'

  TJSHTMLElement(self.Handle).onscroll := @doListBox;

end;

(*procedure TWListBox.Add(item: TCustomControl);
begin
//
  console.log(item.width);
  item.setProperty('width','calc(100% - 40px)');


  //TJSHTMLElement(item.Handle).style.setProperty('width','calc(100%-10px)');
//  console.log( TJSHTMLElement(item.Handle).style.getPropertyValue('width') );

  //item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), item.width, item.height);
  item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), 200, item.height);
  //TJSHTMLElement(item.Handle).style.SetProperty('cursor','pointer');

  //the following construct sets all entries which are not visible (outside the viewport)
  //to display-none, except the last entry which always will have display-inlineblock
  //this will render a correctly dimensioned proportional scroller

  //1-set the last entry to disply-none if it is not visible
  If (item.Top > (self.Height + item.height)) and (self.height > 0) then
    TJSHTMLElement( self.Handle.children[self.Handle.children.length-1]).style.setProperty('display', 'none');

  //2-append the new item
  self.Handle.appendchild(item.Handle);

  //3-always set the last entry to inline-block.
  TJSHTMLElement(self.Handle.children[self.Handle.children.length-1]).style.setProperty('display', 'inline-block');

  Inc(ItemCount);
end;*)

(*
procedure TWListBox.Add(item: TCustomControl);
begin
//
  item.setProperty('width','calc(100% - 4px)');
  item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), item.width, item.height);
  item.SetProperty('cursor','pointer');

  //the following construct sets all entries which are not visible (outside the viewport)
  //to display-none, except the last entry which always will have display-inlineblock
  //this will render a correctly dimensioned proportional scroller

  //1-set the last entry to disply-none if it is not visible
  If (item.Top > (self.Height + item.height)) and (self.height > 0) then
    self.Handle.children[self.Handle.children.length-1].style.display := 'none';

  //2-append the new item
  self.Handle.appendchild(item.Handle);

  //3-always set the last entry to inline-block.
  self.Handle.children[self.Handle.children.length-1].style.display := 'inline-block';

  Inc(ItemCount);
end; *)
procedure TWListBox.Add(item: TCustomControl);
begin
  //Handle.style.setProperty('width','calc(100% - 4px)');
  //item.Handle.style.setProperty('width','calc(100% - 4px)');
  item.setProperty('width','calc(100% - 4px)');
  //item.Handle.style.getPropertyValue('width');
  //item.setProperty('width','calc(100% - 4px)');
  //console.log(item.Width);
  //console.log(item.Handle.style.getPropertyValue('width'));
  item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), item.width, item.height);
  item.SetProperty('cursor','pointer');
//
//  Handle.style.setProperty('width','calc(100% - 4px)');
//  item.Handle.style.setProperty('width','calc(100% - 4)');
//  console.log(item.Handle.style.getPropertyValue('width'));
  //item.setProperty('width','calc(100% - 4px)');
  //TJSHTMLElement(item.Handle).style.setProperty('width','calc(100% - 10px)');
//  item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), item.width, item.height);
//  console.log(item.Width);
  //item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), 100 { StrToInt(Handle.style.getPropertyValue('width')) }, item.height);
//  item.SetProperty('cursor','pointer');

  //the following construct sets all entries which are not visible (outside the viewport)
  //to display-none, except the last entry which always will have display-inlineblock
  //this will render a correctly dimensioned proportional scroller

  //1-set the last entry to disply-none if it is not visible
  If (item.Top > (self.Height + item.height)) and (self.height > 0) then
    TJSHTMLElement(self.Handle.children[self.Handle.children.length-1]).style.setProperty('display', 'none');

//  If (item.Top > self.Height) and (self.height > 0)
//    then item.SetProperty('display','none');

  //2-append the new item
  self.Handle.appendchild(item.Handle);

  //3-always set the last entry to inline-block.
  //self.Handle.children[self.Handle.children.length-1].style.display := 'inline-block';
  TJSHTMLElement(self.Handle.children[self.Handle.children.length-1]).style.setProperty('display', 'inline-block');

  Inc(ItemCount);
end;

procedure TWListBox.Clear;
begin
  While assigned(Handle.firstChild) do
    Handle.removeChild(Handle.firstChild);
  ItemCount := 0;
end;

(*
//old version
procedure TWListBox.Add(item: TCustomControl);
begin
//
  item.setProperty('width','calc(100% - 4px)');
  self.Handle.appendchild(item.Handle);
  item.SetBounds(2, 2 + (ItemCount * item.height) + (ItemCount * 2), item.width, item.height);
  item.SetProperty('cursor','pointer');
  If (item.Top > self.Height) and (self.height > 0)
    then item.SetProperty('display','none');

  Inc(ItemCount);
end;
*)

end.

