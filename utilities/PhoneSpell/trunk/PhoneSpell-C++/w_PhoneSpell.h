//---------------------------------------------------------------------------

#ifndef w_PhoneSpellH
#define w_PhoneSpellH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class Tf_PhoneSpell : public TForm
{
__published:	// IDE-managed Components
	TLabel *l_What;
	TEdit *ed_What;
	TLabel *l_Which;
	TComboBox *cmb_Which;
	TListBox *lb_Result;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall cmb_WhichChange(TObject *Sender);
	void __fastcall ed_WhatChange(TObject *Sender);
private:	// User declarations
	TStringList *FDeutschlandDict;
	TStringList *FSchweizDict;
	TStringList *FOesterreichDict;
	TStringList *FNatoDict;
	TStringList *FJointForcesDict;

	TStringList *FDictionaries;
	TStringList *FCurrentDict;
	void __fastcall AddDictionary(const AnsiString _Name, TStringList *_Dictionary);
	void __fastcall InitDeutschlandDict();
	void __fastcall InitOesterreichDict();
	void __fastcall InitSchweizDict();
	void __fastcall InitNatoDict();
	void __fastcall InitJointForcesDict();
	void __fastcall UpdateSpelling(AnsiString _s);
public:
	__fastcall Tf_PhoneSpell(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE Tf_PhoneSpell *f_PhoneSpell;
//---------------------------------------------------------------------------
#endif
