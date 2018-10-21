program project1;

{$mode objfpc}

uses
  Classes, SysUtils, JS, Web, uMain, jGlobals, jApplication,
  Form1, JSplitter;

(*
uses
  Classes, SysUtils, JS, Web, uMain, jGlobals,
  jForm, jApplication, jImage,jToolBar, jListBox,jProgressBar,JTextArea,
  jVideo, JAnchor, JLoader,JIframe,JStreetView,jGrid,JObjectTable,JStringTable,
  jCanvas,JTreeView,jCheckBox,jSpinner,jFieldSet,jRadioButton,jInput,
  jFlipScroll,jDialog,jWindow,
  jPanel, jButton, JSelect, Form1;


type
  TApp = class
    private
      Fpanel1 : JW3Panel;
      Fbutton1: JW3Button;
    public
      procedure panel1Click(Sender: TObject);
      procedure button1Click(Sender: TObject);
  end;

procedure TApp.panel1Click(Sender: TObject);
begin
  console.log('clicked silver item #' + (Sender as JW3Panel).tag);
end;

procedure TApp.button1Click(Sender:TObject);
begin
  console.log('button1 clicked');
end;


var
  i: integer;
  app: TApp;
  *)
begin
  // Your code here
  (*app:= TApp.Create;
  try
    with app do
    begin
      Fpanel1 := JW3Panel.Create(nil);
      Fpanel1.setProperty('background-color', 'silver');
      Fpanel1.SetBounds(5, 10, 100, 35);
      Fpanel1.tag := IntToStr(i);
      Fpanel1.OnClick := @panel1Click;

      Fbutton1 := JW3Button.Create(nil);
      Fbutton1.SetBounds(5, 50, 100, 50);
      Fbutton1.SetInnerHTML('Button1');
      Fbutton1.OnClick := @button1Click;
    end;
  finally
    app.free;
  end;*)

  //create forms
  Application.CreateForm('Form1',TForm1);
  (*Application.CreateForm('Form2',TForm2);
  Application.CreateForm('Form3',TForm3);
  Application.CreateForm('Form4',TForm4);
  Application.CreateForm('Form5',TForm5);
  Application.CreateForm('Form6',TForm6);
  Application.CreateForm('Form7',TForm7);
  Application.CreateForm('Form8',TForm8);
  Application.CreateForm('Form9',TForm9);
  Application.CreateForm('Form10',TForm10);*)

  //show initial form
  Application.GoToForm('Form1');
end.
