�
 TFORM1 0�  TPF0TForm1Form1Left� Top� Width�Height"CaptionImage Viewer
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style PixelsPerInch`OnCreate
FormCreate
TextHeight TImageimgLoadedImgLeft� TopWidth� Height� 
OnDragDropimgLoadedImgDragDrop
OnDragOverimgLoadedImgDragOver  TLabelLabel1LeftTop8WidthbHeightCaption&Available image files:FocusControl	lstImages  TLabelLabel2LeftTopWidth8HeightCaption&Image path:FocusControledtPath  TEditedtPathLeftTopWidth� HeightTabOrder OnChangeedtPathChange  TButton
btnGetPathLeft� TopWidthHeightCaption...
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold 
ParentFontTabOrderOnClickbtnGetPathClick  TListBox	lstImagesLeftTopHWidth� Height� 
ItemHeightSorted	TabOrder
OnDblClicklstImagesDblClickOnMouseDownlstImagesMouseDown   