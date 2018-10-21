unit jStreetView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement, jIFrame;

type
  TWStreetView = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure SetLocation(s1,s2,s3: String);    //(lat/long/api)
    ifr : TWIFrame;
  end;

implementation

{ TWStreetView }

constructor TWStreetView.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);
  ifr := TWIframe.Create(self);
end;

Procedure TWStreetView.SetLocation(s1,s2,s3: String);
var
  s: String;

begin
  s := 'https://www.google.com/maps/embed/v1/streetview?location=' +
                    s1 + ',' + s2 + '&key=' + s3;
//                    'AIzaSyCWiDqHr-ME74FlTd40x2yoLgVA6Qod-Tk';

  ifr.SetAttribute('src',s);
  ifr.SetProperty('width',inttostr(width-60)+'px');
  ifr.SetProperty('height',inttostr(height-60)+'px');
end;

end.

