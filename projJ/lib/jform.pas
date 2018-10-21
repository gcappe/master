unit jForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWForm = class(TCustomControl)
  public
    procedure InitializeForm; virtual;
    procedure InitializeObject; virtual;
    procedure ReSize; virtual;
    constructor Create(parent: TCustomControl); virtual;
    Caption : String;
  end;

  TFormClass = class of TWForm;

implementation

uses
  JGlobals;

{ TW3Form }

constructor TWForm.Create(parent: TCustomControl);

  procedure doOnResize(sender: TObject);
  begin
    screenwidth := window.innerWidth;
    ReSize;
  end;

begin
  inherited Create('div', parent);
  SetProperty('border','1px double #2196f3');
  Left := 5; Top := 5;
  setProperty('width','calc(100% - 12px)');
  setProperty('height','calc(100% - 12px)');
  setProperty('background-color','white');

  (* This forces the browsers that support it to
     use the GPU rather than CPU for movement *)
  self.setProperty('will-change','transform');
  self.setProperty('-webkit-transform','translateZ(0px)');
  self.setProperty(   '-moz-transform','translateZ(0px)');
  self.setProperty(    '-ms-transform','translateZ(0px)');
  self.setProperty(     '-o-transform','translateZ(0px)');
  self.setProperty(        'transform','translateZ(0px)');

  OnResize := @doOnResize;

end;

Procedure TWForm.InitializeForm;
begin
  //clear form
  self.Clear;
end;

Procedure TWForm.InitializeObject;
begin
//
end;

Procedure TWForm.ReSize;
begin
//
end;

end.

