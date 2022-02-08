object KeyForm: TKeyForm
  Left = 818
  Top = 235
  Width = 475
  Height = 379
  Caption = 'Clickety-click'
  Color = 2629664
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    459
    340)
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 16
    Top = 16
    Width = 425
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = 2629664
    Font.Charset = ANSI_CHARSET
    Font.Color = clSilver
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ItemHeight = 30
    ParentFont = False
    TabOrder = 0
    OnClick = ListBox1Click
  end
end
