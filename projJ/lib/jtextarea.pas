unit JTextArea;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWTextArea = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation



{ TWTextArea }

constructor TWTextArea.Create(parent: TCustomControl);
begin
  inherited Create('textarea', parent);
end;


end.
