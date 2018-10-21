unit jProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement, JPanel;

type
  Float = double;

type
  TWProgress = class(TCustomControl)
  private
    procedure SetPerc(aPerc: float);
    function  GetPerc: float;
    Light: TCustomControl;
    timer : NativeInt;
  public
    constructor Create(parent: TCustomControl); virtual;
    property Perc: float read getPerc write setPerc;
    ProgressBar : TCustomControl;
  end;


implementation


{ TWProgress }

constructor TWProgress.Create(parent: TCustomControl);
   procedure doOnTimer;
   begin
     Light.Left := Light.Left + 4;
     If Light.Left + Light.Width > ProgressBar.Width then Light.Left := 0;
     If Light.Left + Light.Width + 2 >= Width then
     begin
       Light.Left := -Light.Width;
       window.clearInterval(timer);
     end;
   end;

begin
  inherited Create('div', parent);

  ProgressBar := TWPanel.Create(self);

  Light := TWPanel.Create(ProgressBar);
  Light.SetProperty('border-radius','2px');
  Light.SetProperty('background-color','white');
  Light.SetProperty('opacity','0.5');

  Light.Left := 0;
  timer := window.setInterval(@doOnTimer, 20);
//
end;

procedure TWProgress.SetPerc(aPerc: Float);
var
  f : float;
begin
  If aPerc > 100 then aPerc := 100;
  If aPerc < 0   then aPerc := 0;

  f := (Width * aPerc)/100;
  ProgressBar.SetProperty('width',FloatToStr(f) + 'px');
  ProgressBar.SetBounds(0,0,ProgressBar.Width,self.height);
  ProgressBar.SetProperty('overflow','hidden');

  Light.Width := ProgressBar.Height * 2;
  Light.Height := ProgressBar.Height - 4;
  Light.Top := 2;
end;

function TWProgress.GetPerc: float;
var
  f : float;
begin
  f := (ProgressBar.width / width) * 100;
  Result := f;
end;

end.

