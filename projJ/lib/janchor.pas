unit JAnchor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement, JImage;

type
  TWAnchor = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    placeholder: TWImage;
  end;

implementation

{ TWAnchor }

constructor TWAnchor.Create(parent: TCustomControl);
begin
  inherited Create('a', parent);
  placeholder := TWImage.Create(self);
end;

end.

