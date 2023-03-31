unit FormsDummy;

// Dummy replacement for the Forms unit.
// Remember to set Forms=FormsDummy; under unit aliases.

interface

type
  TForm = class
            Handle : Integer;
          end;

  TApplication = class
                   MainForm : TForm;
                   Handle : Integer;
                 end;

var
  Application : TApplication = nil;

implementation

end.

