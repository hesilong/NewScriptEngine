unit TestUnit;

interface

type
PStudent = ^TStudent;
TStudent = record
  Name: string;
  Sex: string;
  Next: PStudent;
end;

implementation

end.
