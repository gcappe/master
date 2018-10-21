unit jApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, JElement, JForm;

type
  TWApplication = class(TCustomControl)
  public
    FormNames: Array of String;
    FormsClasses: Array of TFormClass;       //TFormClass = class of TWForm;
    FormsInstances: Array of TWForm;
    constructor Create(parent: TCustomControl); virtual;
    procedure CreateForm(FormName: String; aClassType: TFormClass);
    procedure GoToForm(FormName: String);
  end;

var
  Application : TWApplication;

implementation

{ TWApplication }

constructor TWApplication.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);

  setProperty('width','100%');
  setProperty('height','100%');
  setProperty('background-color','white');

end;

procedure TWApplication.CreateForm(FormName: String; aClassType: TFormClass);
begin
  TJSArray(FormNames).push(FormName);
  TJSArray(FormsClasses).push(aClassType);
  TJSArray(FormsInstances).push(nil);
end;

(*
,Show:function(Self, FormName$1) {
   var i = 0;
   var $temp1;
   for(i=0,$temp1=Self.FormNames.length;i<$temp1;i++) {
      if (Self.FormsInstances[i]!==null) {
         TCustomControl.SetProperty(Self.FormsInstances[i],"display","none");
      }
      if (Self.FormNames[i]==FormName$1) {
         if (Self.FormsInstances[i]===null) {
            Self.FormsInstances[i]=TWForm.Create$5($NewDyn(Self.FormsClasses[i],""),Self);
            TWForm.ShowForm$(Self.FormsInstances[i]);
         } else {
            TWForm.ClearForm(Self.FormsInstances[i]);
            TWForm.ShowForm$(Self.FormsInstances[i]);
            TCustomControl.SetProperty(Self.FormsInstances[i],"display","inline-block");
         }
      }
   }
}
*)
procedure TWApplication.GoToForm(FormName: String);
var
  i: integer;
begin
//
  For i := 0 to TJSArray(FormNames).length -1 do begin
    If FormsInstances[i] <> nil then
      FormsInstances[i].SetProperty('display','none');
    If FormNames[i] = FormName then begin
      If FormsInstances[i] = nil then       //form has never been displayed yet
        FormsInstances[i] := FormsClasses[i].Create(self) else
        FormsInstances[i].SetProperty('display','inline-block');

      //TW3Form(FormsInstances[i]).InitializeForm;    //ClearForm;
      //console.log(FormsClasses[i]);    //ClearForm;
      { TODO warleyalex }
      (*
      this.Dt$ = function ($) {
  		return $.ClassType.Dt($)
  	},
  	this.YC$ = function ($) {
  		return $.ClassType.YC($)
  	},
      *)
      //*this.FormsClasses[i](this.FormsInstances[i])
      TWForm(FormsInstances[i]).InitializeForm;
      TWForm(FormsInstances[i]).InitializeObject;
        //(FormsInstances[i] as FormsClasses[i]).InitializeForm;    //ClearForm;
        //(FormsInstances[i] as FormsClasses[i]).InitializeObject;  //ShowForm;
    end;
  end;
end;

initialization
Application := TWApplication.Create(nil);

end.

