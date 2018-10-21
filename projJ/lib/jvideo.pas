unit jVideo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWVideo = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation

{ TWVideo }

constructor TWVideo.Create(parent: TCustomControl);
begin
  inherited Create('video', parent);
end;

end.

