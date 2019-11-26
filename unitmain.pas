unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, UTF8Process, Process;

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
    procedure StartWin(FileName: String; OverWrite: Boolean; Dpi: String);
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
    {$IFDEF WINDOWS}
    	StartWin(ListBoxFileNames.Items.Strings[I], CBOverride.Checked, Dpi);
    {$ENDIF}
    {$IFDEF LINUX}

    {$ENDIF}
    ProgressBarProcess.Position := ProgressBarProcess.Position + 1;
  end;
  ShowMessage('Success');
  ListBoxFileNames.Clear;
end;

procedure TFormMain.StartWin(FileName: String; OverWrite: Boolean; Dpi: String);
var
  AProcess: TProcessUTF8;
  NewFileName: String;
begin
  AProcess := TProcessUTF8.Create(nil);
  AProcess.Executable:= 'gs\bin\gswin32c.exe';
  NewFileName := StringReplace(FileName, '.pdf', 'New.pdf', [rfReplaceAll, rfIgnoreCase]);
  AProcess.Executable := AProcess.Executable + ' -sOutputFile="' + NewFileName + '"';
  AProcess.Executable := AProcess.Executable + ' -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/default -dEmbedAllFonts=true -dAutoRotatePages=/PageByPage -dParseDSCComments=false -sColorConversionStrategy=RGB -dProcessColorModel=/DeviceRGB -dConvertCMYKImagesToRGB=true -dAutoFilterColorImages=false -dAutoFilterGrayImages=false -dEncodeColorImages=true -dEncodeGrayImages=true -dColorImageFilter=/DCTEncode -dGrayImageFilter=/DCTEncode -dDownsampleColorImages=true -dColorImageResolution='+Dpi+' -dDownsampleGrayImages=true -dGrayImageResolution='+Dpi+' -dEncodeMonoImages=true -dMonoImageFilter=/CCITTFaxEncode -c ".setpdfwrite << /ColorImageDict <</QFactor 2.4 /Blend 1 /HSample [2 1 1 2] /VSample [2 1 1 2]>> >> setdistillerparams" ".setpdfwrite << /GrayImageDict <</QFactor 2.4 /Blend 1 /HSample [2 1 1 2] /VSample [2 1 1 2]>> >> setdistillerparams"';
  AProcess.Executable := AProcess.Executable + ' -f "' + FileName + '"';
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
  AProcess.Free;
end;

end.

