//---------------------------------------------------------------------------

#include <ctype.h>
#include <vcl.h>
#pragma hdrstop

#include "w_PhoneSpell.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
Tf_PhoneSpell *f_PhoneSpell;
//---------------------------------------------------------------------------
__fastcall Tf_PhoneSpell::Tf_PhoneSpell(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::FormCreate(TObject *Sender)
{
  FDictionaries = new TStringList;
  FDeutschlandDict = new TStringList;
  AddDictionary("Deutschland", FDeutschlandDict);
  FOesterreichDict = new TStringList;
  AddDictionary("Österreich", FOesterreichDict);
  FSchweizDict = new TStringList;
  AddDictionary("Schweiz", FSchweizDict);
  FNatoDict = new TStringList;
  AddDictionary("NATO", FNatoDict);
  FJointForcesDict = new TStringList;
  AddDictionary("Joint Army/Navy", FJointForcesDict);

  FCurrentDict = FDeutschlandDict;
  cmb_Which->ItemIndex = 0;

  InitDeutschlandDict();
  InitOesterreichDict();
  InitSchweizDict();
  InitNatoDict();
  InitJointForcesDict();
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::InitDeutschlandDict()
{
  TStringList *d = FDeutschlandDict;
  d->Values["a"] = "Anton";
  d->Values["ä"] = "Ärger";
  d->Values["b"] = "Berta";
  d->Values["c"] = "Cäsar";
  d->Values["d"] = "Dora";
  d->Values["e"] = "Emil";
  d->Values["f"] = "Friedrich";
  d->Values["g"] = "Gustav";
  d->Values["h"] = "Heinrich";
  d->Values["i"] = "Ida";
  d->Values["j"] = "Julius";
  d->Values["k"] = "Kaufmann";
  d->Values["l"] = "Ludwig";
  d->Values["m"] = "Martha";
  d->Values["n"] = "Nordpol";
  d->Values["o"] = "Otto";
  d->Values["ö"] = "Ökonom";
  d->Values["p"] = "Paula";
  d->Values["q"] = "Quelle";
  d->Values["r"] = "Richard";
  d->Values["s"] = "Samuel";
  d->Values["t"] = "Theodor";
  d->Values["u"] = "Ulrich";
  d->Values["ü"] = "Übermut";
  d->Values["v"] = "Viktor";
  d->Values["w"] = "Wilhelm";
  d->Values["x"] = "Xanthippe";
  d->Values["y"] = "Ypsilon";
  d->Values["z"] = "Zacharias";
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::InitOesterreichDict()
{
  TStringList *d = FOesterreichDict;
  d->Values["a"] = "Anton";
  d->Values["ä"] = "Ärger";
  d->Values["b"] = "Berta";
  d->Values["c"] = "Csar";
  d->Values["d"] = "Dora";
  d->Values["e"] = "Emil";
  d->Values["f"] = "Friedrich";
  d->Values["g"] = "Gustav";
  d->Values["h"] = "Heinrich";
  d->Values["i"] = "Ida";
  d->Values["j"] = "Julius";
  d->Values["k"] = "Konrad";
  d->Values["l"] = "Ludwig";
  d->Values["m"] = "Martha";
  d->Values["n"] = "Nordpol";
  d->Values["o"] = "Otto";
  d->Values["ö"] = "Österreich";
  d->Values["p"] = "Paula";
  d->Values["q"] = "Quelle";
  d->Values["r"] = "Richard";
  d->Values["s"] = "Siegfried";
  d->Values["t"] = "Theodor";
  d->Values["u"] = "Ulrich";
  d->Values["ü"] = "Übel";
  d->Values["v"] = "Viktor";
  d->Values["w"] = "Wilhelm";
  d->Values["x"] = "Xaver";
  d->Values["y"] = "Ypsilon";
  d->Values["z"] = "Zürich";
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::InitSchweizDict()
{
  TStringList *d = FSchweizDict;
  d->Values["a"] = "Anna";
  d->Values["ä"] = "Äsch";
  d->Values["b"] = "Berta";
  d->Values["c"] = "Csar";
  d->Values["d"] = "Daniel";
  d->Values["e"] = "Emil";
  d->Values["f"] = "Friedrich";
  d->Values["g"] = "Gustav";
  d->Values["h"] = "Heinrich";
  d->Values["i"] = "Ida";
  d->Values["j"] = "Jakob";
  d->Values["k"] = "Kaiser";
  d->Values["l"] = "Leopold";
  d->Values["m"] = "Marie";
  d->Values["n"] = "Niklaus";
  d->Values["o"] = "Otto";
  d->Values["ö"] = "Örlikon";
  d->Values["p"] = "Peter";
  d->Values["q"] = "Quasi";
  d->Values["r"] = "Rosa";
  d->Values["s"] = "Sophie";
  d->Values["t"] = "Theodor";
  d->Values["u"] = "Ulrich";
  d->Values["ü"] = "Übermut";
  d->Values["v"] = "Viktor";
  d->Values["w"] = "Wilhelm";
  d->Values["x"] = "Xaver";
  d->Values["y"] = "Yverdon";
  d->Values["z"] = "Zürich";
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::InitNatoDict()
{
  TStringList *d = FNatoDict;
  d->Values["a"] = "Alpha";
  d->Values["b"] = "Bravo";
  d->Values["c"] = "Charlie";
  d->Values["d"] = "Delta";
  d->Values["e"] = "Echo";
  d->Values["f"] = "Foxtrott";
  d->Values["g"] = "Golf";
  d->Values["h"] = "Hotel";
  d->Values["i"] = "India";
  d->Values["j"] = "Juliet";
  d->Values["k"] = "Kilo";
  d->Values["l"] = "Lima";
  d->Values["m"] = "Mike";
  d->Values["n"] = "November";
  d->Values["o"] = "Oscar";
  d->Values["p"] = "Papa";
  d->Values["q"] = "Quebec";
  d->Values["r"] = "Romeo";
  d->Values["s"] = "Sierra";
  d->Values["t"] = "Tango";
  d->Values["u"] = "Uniform";
  d->Values["v"] = "Victor";
  d->Values["w"] = "Whisky";
  d->Values["x"] = "X-Ray";
  d->Values["y"] = "Yankee";
  d->Values["z"] = "Zulu";
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::InitJointForcesDict()
{
  TStringList *d = FJointForcesDict;
  d->Values["a"] = "Able";
  d->Values["b"] = "Baker";
  d->Values["c"] = "Charlie";
  d->Values["d"] = "Dog";
  d->Values["e"] = "Easy";
  d->Values["f"] = "Fox";
  d->Values["g"] = "George";
  d->Values["h"] = "How";
  d->Values["i"] = "Item";
  d->Values["j"] = "Jig";
  d->Values["k"] = "King";
  d->Values["l"] = "Love";
  d->Values["m"] = "Mike";
  d->Values["n"] = "Nan";
  d->Values["o"] = "Oboe";
  d->Values["p"] = "Peter";
  d->Values["q"] = "Queen";
  d->Values["r"] = "Roger";
  d->Values["s"] = "Sugar";
  d->Values["t"] = "Tare";
  d->Values["u"] = "Uncle";
  d->Values["v"] = "Victor";
  d->Values["w"] = "William";
  d->Values["x"] = "X-Ray";
  d->Values["y"] = "Yoke";
  d->Values["z"] = "Zebra";
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::AddDictionary(const AnsiString _Name, TStringList *_Dictionary)
{
  cmb_Which->Items->Add(_Name);
  FDictionaries->AddObject(_Name, _Dictionary);
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::FormDestroy(TObject *Sender)
{
	delete FDictionaries;

	delete FDeutschlandDict;
	delete FSchweizDict;
	delete FOesterreichDict;
	delete FNatoDict;
	delete FJointForcesDict;
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::cmb_WhichChange(TObject *Sender)
{
	AnsiString Language;
	int idx;

	idx = cmb_Which->ItemIndex;
	if (-1 == idx) {
		return;
	}
	Language = cmb_Which->Items->Strings[idx];

	if (!(FDictionaries->Find(Language, idx))) {
		exit;
	}
	FCurrentDict = (TStringList*) FDictionaries->Objects[idx];
	UpdateSpelling(ed_What->Text);
}
//---------------------------------------------------------------------------
void __fastcall Tf_PhoneSpell::UpdateSpelling(AnsiString _s)
{
	int i;
	char c;
	TStringList *d;
	AnsiString s;

	d = FCurrentDict;
	lb_Result->Items->Clear();

	for (i = 1; i <= _s.Length(); i++) {
		c = _s[i];
		c = tolower(c);
		s = d->Values[c];
		if ((!s.IsEmpty())) {
			lb_Result->Items->Add(s);
		} else {
			lb_Result->Items->Add("<unknown>");
		}
	}
}
void __fastcall Tf_PhoneSpell::ed_WhatChange(TObject *Sender)
{
	UpdateSpelling(ed_What->Text);
}
//---------------------------------------------------------------------------

