object fmMain: TfmMain
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Dynamic Class Registration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblInitScript: TLabel
    Left = 360
    Top = 40
    Width = 84
    Height = 13
    Caption = 'Initialization Script'
  end
  object mDescription: TMemo
    Left = 0
    Top = 0
    Width = 345
    Height = 281
    TabStop = False
    Lines.Strings = (
      'This demo shows how you can use an "initialization script" to '
      'dynamically register classes for your other scripts to utilize.'
      ''
      'The "InitializeUnit" contains the functions needed for dynamic '
      'registration and the DynamicSymbolUnit will have all the dynamic'
      'classes registered there. '
      ''
      'This is ideal when base classes like TComponent are already '
      'defined and a class can be returned through calls like '
      '"FindComponent()". Dynamic classes cannot be created and '
      'have them create Delphi analog objects. They can only give '
      'you access to published properties of basic types. For things '
      'like the VCL this is more than sufficient to handle unique '
      'situations that you may not have considered at the time an '
      'application was created and deployed.'
      ''
      'The true power of this is most evident when coupled with '
      'CodeInsight features so you can "see" what you have available'
      'when you go to use it.')
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnRunInitScript: TButton
    Left = 360
    Top = 8
    Width = 169
    Height = 25
    Caption = 'Run Initialization Script'
    TabOrder = 1
    OnClick = btnRunInitScriptClick
  end
  object mInitScript: TMemo
    Left = 360
    Top = 56
    Width = 321
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      
        '// First is class to register, second is ancestor to inherit fro' +
        'm'
      'RegisterClassWithUnit('#39'TButton'#39', '#39'TComponent'#39');'
      'RegisterClassWithUnit('#39'TCheckBox'#39', '#39'TComponent'#39');'
      'RegisterClassWithUnit('#39'TLabel'#39', '#39'TComponent'#39');'
      'RegisterClassWithUnit('#39'TEdit'#39', '#39'TComponent'#39');'
      'RegisterClassWithUnit('#39'TTrackBar'#39', '#39'TComponent'#39');')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object grpTestingArea: TGroupBox
    Left = 0
    Top = 288
    Width = 345
    Height = 161
    Caption = 'Testing Area (grpTestingArea)'
    TabOrder = 6
    object Label1: TLabel
      Left = 16
      Top = 28
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object Edit1: TEdit
      Left = 80
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Button2: TButton
      Left = 176
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 1
    end
    object Panel1: TPanel
      Left = 8
      Top = 96
      Width = 169
      Height = 49
      Caption = 'Panel1'
      TabOrder = 2
    end
    object TrackBar1: TTrackBar
      Left = 208
      Top = 96
      Width = 110
      Height = 45
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 3
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 56
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 4
    end
  end
  object btnRunScript: TButton
    Left = 360
    Top = 176
    Width = 169
    Height = 25
    Caption = 'Run regular usage script'
    TabOrder = 4
    OnClick = btnRunScriptClick
  end
  object mRegularScript: TMemo
    Left = 360
    Top = 208
    Width = 321
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '// Form is accessible as a TComponent through "HostForm"'
      ''
      'var label: TLabel;'
      'label := HostForm.FindComponent('#39'Label1'#39');'
      'label.Caption := '#39'Edit1 ====>'#39';'
      ''
      'var edit: TEdit;'
      'edit := HostForm.FindComponent('#39'Edit1'#39');'
      'edit.Text := '#39'My new text'#39';'
      ''
      'var box: TCheckBox;'
      'box := HostForm.FindComponent('#39'CheckBox1'#39');'
      'box.Checked := not box.Checked;'
      ''
      'var track: TTrackBar;'
      'track := HostForm.FindComponent('#39'TrackBar1'#39');'
      'track.Position := track.Position + 1;')
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object btnRegisterInCode: TButton
    Left = 536
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Register TPanel in Code'
    TabOrder = 2
    OnClick = btnRegisterInCodeClick
  end
  object ScriptInit: TDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 32
    Top = 160
  end
  object InitializeUnit: Tdws2Unit
    Script = ScriptInit
    Arrays = <>
    Classes = <>
    Constants = <>
    Enumerations = <>
    Forwards = <>
    Functions = <
      item
        Name = 'RegisterClassWithUnit'
        Parameters = <
          item
            Name = 'AClassName'
            DataType = 'String'
            IsVarParam = True
            IsWritable = False
          end
          item
            Name = 'AAncestorType'
            DataType = 'String'
            IsVarParam = True
            IsWritable = False
          end>
        OnEval = InitializeUnitFunctionsRegisterClassWithUnitEval
      end>
    Instances = <>
    Records = <>
    Synonyms = <>
    UnitName = 'InitializeUnit'
    Variables = <>
    Left = 136
    Top = 160
  end
  object ScriptForUse: TDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 32
    Top = 216
  end
  object DynamicSymbolUnit: Tdws2Unit
    Script = ScriptForUse
    Arrays = <>
    Classes = <>
    Constants = <>
    Dependencies.Strings = (
      'BasicClasses')
    Enumerations = <>
    Forwards = <>
    Functions = <>
    Instances = <>
    Records = <>
    Synonyms = <>
    UnitName = 'DynamicUnit'
    Variables = <>
    Left = 136
    Top = 216
  end
  object BasicClassesUnit: Tdws2Unit
    Script = ScriptForUse
    Arrays = <>
    Classes = <
      item
        Name = 'TPersistent'
        Ancestor = 'TObject'
        Constructors = <>
        Fields = <>
        Methods = <
          item
            Name = 'Assign'
            Parameters = <
              item
                Name = 'Source'
                DataType = 'TPersistent'
                IsWritable = False
              end>
            OnEval = BasicClassesUnitTPersistentMethodsAssignEval
            Attributes = [maVirtual]
            Kind = mkProcedure
          end
          item
            Name = 'GetNamePath'
            Parameters = <>
            ResultType = 'String'
            OnEval = BasicClassesUnitTPersistentMethodsGetNamePathEval
            Attributes = [maVirtual]
            Kind = mkFunction
          end>
        Properties = <>
      end
      item
        Name = 'TComponent'
        Ancestor = 'TPersistent'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'AOwner'
                DataType = 'TComponent'
                IsWritable = False
              end>
            Attributes = [maVirtual]
            OnAssignExternalObject = BasicClassesUnitTComponentConstructorsCreateAssignExternalObject
          end>
        Fields = <>
        Methods = <
          item
            Name = 'SetTag'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = BasicClassesUnitTComponentMethodsSetTagEval
            Kind = mkProcedure
          end
          item
            Name = 'GetTag'
            Parameters = <>
            ResultType = 'Integer'
            OnEval = BasicClassesUnitTComponentMethodsGetTagEval
            Kind = mkFunction
          end
          item
            Name = 'SetName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = BasicClassesUnitTComponentMethodsSetNameEval
            Kind = mkProcedure
          end
          item
            Name = 'GetName'
            Parameters = <>
            ResultType = 'String'
            OnEval = BasicClassesUnitTComponentMethodsGetNameEval
            Kind = mkFunction
          end
          item
            Name = 'GetOwner'
            Parameters = <>
            ResultType = 'TComponent'
            OnEval = BasicClassesUnitTComponentMethodsGetOwnerEval
            Kind = mkFunction
          end
          item
            Name = 'SetComponentIndex'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = BasicClassesUnitTComponentMethodsSetComponentIndexEval
            Kind = mkProcedure
          end
          item
            Name = 'GetComponentIndex'
            Parameters = <>
            ResultType = 'Integer'
            OnEval = BasicClassesUnitTComponentMethodsGetComponentIndexEval
            Kind = mkFunction
          end
          item
            Name = 'GetComponentCount'
            Parameters = <>
            ResultType = 'Integer'
            OnEval = BasicClassesUnitTComponentMethodsGetComponentCountEval
            Kind = mkFunction
          end
          item
            Name = 'GetComponent'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'TComponent'
            OnEval = BasicClassesUnitTComponentMethodsGetComponentEval
            Kind = mkFunction
          end
          item
            Name = 'FindComponent'
            Parameters = <
              item
                Name = 'AName'
                DataType = 'String'
                IsVarParam = True
                IsWritable = False
              end>
            ResultType = 'TComponent'
            OnEval = BasicClassesUnitTComponentMethodsFindComponentEval
            Kind = mkFunction
          end
          item
            Name = 'FreeNotification'
            Parameters = <
              item
                Name = 'AComponent'
                DataType = 'TComponent'
                IsWritable = False
              end>
            OnEval = BasicClassesUnitTComponentMethodsFreeNotificationEval
            Kind = mkProcedure
          end
          item
            Name = 'RemoveFreeNotification'
            Parameters = <
              item
                Name = 'AComponent'
                DataType = 'TComponent'
                IsWritable = False
              end>
            OnEval = BasicClassesUnitTComponentMethodsRemoveFreeNotificationEval
            Kind = mkProcedure
          end
          item
            Name = 'GetParentComponent'
            Parameters = <>
            ResultType = 'TComponent'
            OnEval = BasicClassesUnitTComponentMethodsGetParentComponentEval
            Attributes = [maVirtual]
            Kind = mkFunction
          end
          item
            Name = 'GetNamePath'
            Parameters = <>
            ResultType = 'String'
            OnEval = BasicClassesUnitTComponentMethodsGetNamePathEval
            Attributes = [maOverride]
            Kind = mkFunction
          end
          item
            Name = 'HasParent'
            Parameters = <>
            ResultType = 'Boolean'
            OnEval = BasicClassesUnitTComponentMethodsHasParentEval
            Attributes = [maVirtual]
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'Components'
            DataType = 'TComponent'
            ReadAccess = 'GetComponent'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            IsDefault = False
          end
          item
            Name = 'ComponentCount'
            DataType = 'Integer'
            ReadAccess = 'GetComponentCount'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'ComponentIndex'
            DataType = 'Integer'
            ReadAccess = 'GetComponentIndex'
            WriteAccess = 'SetComponentIndex'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Owner'
            DataType = 'TComponent'
            ReadAccess = 'GetOwner'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Name'
            DataType = 'String'
            ReadAccess = 'GetName'
            WriteAccess = 'SetName'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Tag'
            DataType = 'Integer'
            ReadAccess = 'GetTag'
            WriteAccess = 'SetTag'
            Parameters = <>
            IsDefault = False
          end>
      end>
    Constants = <>
    Enumerations = <>
    Forwards = <
      item
        Name = 'TPersistent'
      end
      item
        Name = 'TComponent'
      end>
    Functions = <>
    Instances = <
      item
        Name = 'HostForm'
        DataType = 'TComponent'
        OnInstantiate = BasicClassesUnitInstancesHostFormInstantiate
      end>
    Records = <>
    Synonyms = <>
    UnitName = 'BasicClasses'
    Variables = <>
    Left = 80
    Top = 248
  end
end
