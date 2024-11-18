unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, UTF8Process, Process, FileUtil;

type
  TFormMain = class(TForm)
    ButtonClear: TButton;
    ButtonStart: TButton;
    ButtonAdd: TButton;
    CBQuality: TComboBox;
    CBOverride: TCheckBox;
    LabelQuality: TLabel;
    ListBoxFileNames: TListBox;
    OpenDialogPdf: TOpenDialog;
    ProgressBarProcess: TProgressBar;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    procedure Start(FileName: String; OverWrite: Boolean; Dpi: String);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  ListBoxFileNames.Items.AddStrings(FileNames);
end;

procedure TFormMain.ButtonAddClick(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialogPdf.Execute then
  begin
    for I:=0 to OpenDialogPdf.Files.Count-1 do
			ListBoxFileNames.Items.Add(OpenDialogPdf.Files[I]);
  end;
end;

procedure TFormMain.ButtonClearClick(Sender: TObject);
begin
  ListBoxFileNames.Clear;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
var
  I: Integer;
  Dpi: String;
begin
  Case CBQuality.ItemIndex of
  	0 : Dpi := '600';
    1 : Dpi := '300';
    2 : Dpi := '150';
    3 : Dpi := '100';
  end;
  ProgressBarProcess.Position := 0;
  ProgressBarProcess.Max := ListBoxFileNames.Items.Count;
  For I:=0 to ListBoxFileNames.Items.Count-1 do
  begin
    Start(ListBoxFileNames.Items.Strings[I], CBOverride.Checked, Dpi);
    ProgressBarProcess.Position := ProgressBarProcess.Position + 1;
  end;
  ShowMessage('Success');
  ListBoxFileNames.Clear;
end;

procedure TFormMain.Start(FileName: String; OverWrite: Boolean; Dpi: String);
var
  AProcess: TProcessUTF8;
  NewFileName: String;
begin
  AProcess := TProcessUTF8.Create(nil);
  {$IFDEF WIN32}
    AProcess.Executable:= 'gs32\bin\gswin32c.exe';
  {$ENDIF}
  {$IFDEF WIN64}
    AProcess.Executable:= 'gs64\bin\gswin64c.exe';
  {$ENDIF}
  {$IFDEF LINUX}
    AProcess.Executable:= FindDefaultExecutablePath('gs');
  {$ENDIF}
  NewFileName := StringReplace(FileName, '.pdf', 'New.pdf', [rfReplaceAll, rfIgnoreCase]);
  AProcess.Parameters.Add('-sOutputFile="' + NewFileName + '"');
  AProcess.Parameters.Add('-dNOPAUSE');
  AProcess.Parameters.Add('-dBATCH');
  AProcess.Parameters.Add('-sDEVICE=pdfwrite');
  AProcess.Parameters.Add('-dCompatibilityLevel=1.4');
  AProcess.Parameters.Add('-dPDFSETTINGS=/default');
  AProcess.Parameters.Add('-dEmbedAllFonts=true');
  AProcess.Parameters.Add('-dAutoRotatePages=/PageByPage');
  AProcess.Parameters.Add('-dParseDSCComments=false');
  AProcess.Parameters.Add('-sColorConversionStrategy=RGB');
  AProcess.Parameters.Add('-dProcessColorModel=/DeviceRGB');
  AProcess.Parameters.Add('-dConvertCMYKImagesToRGB=true');
  AProcess.Parameters.Add('-dAutoFilterColorImages=false');
  AProcess.Parameters.Add('-dAutoFilterGrayImages=false');
  AProcess.Parameters.Add('-dEncodeColorImages=true');
  AProcess.Parameters.Add('-dEncodeGrayImages=true');
  AProcess.Parameters.Add('-dColorImageFilter=/DCTEncode');
  AProcess.Parameters.Add('-dGrayImageFilter=/DCTEncode');
  AProcess.Parameters.Add('-dDownsampleColorImages=true');
  AProcess.Parameters.Add('-dColorImageResolution='+Dpi);
  AProcess.Parameters.Add('-dDownsampleGrayImages=true');
  AProcess.Parameters.Add('-dGrayImageResolution='+Dpi);
  AProcess.Parameters.Add('-dEncodeMonoImages=true');
  AProcess.Parameters.Add('-dMonoImageFilter=/CCITTFaxEncode');
  AProcess.Parameters.Add('-c ".setpdfwrite << /ColorImageDict <</QFactor 2.4 /Blend 1 /HSample [2 1 1 2] /VSample [2 1 1 2]>> >> setdistillerparams" ".setpdfwrite << /GrayImageDict <</QFactor 2.4 /Blend 1 /HSample [2 1 1 2] /VSample [2 1 1 2]>> >> setdistillerparams"');
  AProcess.Parameters.Add('-f');
  AProcess.Parameters.Add('"' + FileName + '"');
  AProcess.Options := AProcess.Options + [poWaitOnExit, poNoConsole];
  AProcess.Execute;
  while AProcess.Running = true do
  begin
    Application.ProcessMessages;
  end;
  if OverWrite then
  begin
    DeleteFile(FileName);
    RenameFile(NewFileName, FileName);
  end;
  Application.ProcessMessages;
  FreeAndNil(AProcess);
end;


end.

