using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.Collections.Specialized;

namespace PhoneSpell
{
	/// <summary>
	/// Summary description for WinForm.
	/// </summary>
	public class TPhoneSpell : System.Windows.Forms.Form
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.TextBox ed_What;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.ComboBox cmb_Which;
		private System.Windows.Forms.ListBox lb_Result;

		public TPhoneSpell()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
			FDictionaries = new Hashtable();

			FDeutschlandDict = new StringDictionary();
			AddDictionary("Deutschland", FDeutschlandDict);
			FOesterreichDict = new StringDictionary();
			AddDictionary("Österreich", FOesterreichDict);
			FSchweizDict = new StringDictionary();
			AddDictionary("Schweiz", FSchweizDict);
			FNatoDict = new StringDictionary();
			AddDictionary("NATO", FNatoDict);
			FJointForcesDict = new StringDictionary();
			AddDictionary("Joint Army/Navy", FJointForcesDict);

			FCurrentDict = FDeutschlandDict;
			cmb_Which.SelectedIndex = 0;

			InitDeutschlandDict();
			InitOesterreichDict();
			InitSchweizDict();
			InitNatoDict();
			InitJointForcesDict();
		}

		private StringDictionary FDeutschlandDict;
		private StringDictionary FSchweizDict;
		private StringDictionary FOesterreichDict;
		private StringDictionary FNatoDict;
		private StringDictionary FJointForcesDict;

		private Hashtable FDictionaries;
		private StringDictionary FCurrentDict;

		private void AddDictionary(string _Name, StringDictionary _Dictionary)
		{
			cmb_Which.Items.Add(_Name);
			FDictionaries.Add(_Name, _Dictionary);
		}
		private void InitDeutschlandDict()
		{
			StringDictionary d = FDeutschlandDict;
			d.Add("a", "Anton");
			d.Add("ä", "Ärger");
			d.Add("b", "Berta");
			d.Add("c", "Cäsar");
			d.Add("d", "Dora");
			d.Add("e", "Emil");
			d.Add("f", "Friedrich");
			d.Add("g", "Gustav");
			d.Add("h", "Heinrich");
			d.Add("i", "Ida");
			d.Add("j", "Julius");
			d.Add("k", "Kaufmann");
			d.Add("l", "Ludwig");
			d.Add("m", "Martha");
			d.Add("n", "Nordpol");
			d.Add("o", "Otto");
			d.Add("ö", "Ökonom");
			d.Add("p", "Paula");
			d.Add("q", "Quelle");
			d.Add("r", "Richard");
			d.Add("s", "Samuel");
			d.Add("t", "Theodor");
			d.Add("u", "Ulrich");
			d.Add("ü", "Übermut");
			d.Add("v", "Viktor");
			d.Add("w", "Wilhelm");
			d.Add("x", "Xanthippe");
			d.Add("y", "Ypsilon");
			d.Add("z", "Zacharias");
		}
		private void InitOesterreichDict()
		{
			StringDictionary d = FOesterreichDict;
			d.Add("a", "Anton");
			d.Add("ä", "Ärger");
			d.Add("b", "Berta");
			d.Add("c", "Csar");
			d.Add("d", "Dora");
			d.Add("e", "Emil");
			d.Add("f", "Friedrich");
			d.Add("g", "Gustav");
			d.Add("h", "Heinrich");
			d.Add("i", "Ida");
			d.Add("j", "Julius");
			d.Add("k", "Konrad");
			d.Add("l", "Ludwig");
			d.Add("m", "Martha");
			d.Add("n", "Nordpol");
			d.Add("o", "Otto");
			d.Add("ö", "Österreich");
			d.Add("p", "Paula");
			d.Add("q", "Quelle");
			d.Add("r", "Richard");
			d.Add("s", "Siegfried");
			d.Add("t", "Theodor");
			d.Add("u", "Ulrich");
			d.Add("ü", "Übel");
			d.Add("v", "Viktor");
			d.Add("w", "Wilhelm");
			d.Add("x", "Xaver");
			d.Add("y", "Ypsilon");
			d.Add("z", "Zürich");
		}
		private void InitSchweizDict()
		{
			StringDictionary d = FSchweizDict;
			d.Add("a", "Anna");
			d.Add("ä", "Äsch");
			d.Add("b", "Berta");
			d.Add("c", "Csar");
			d.Add("d", "Daniel");
			d.Add("e", "Emil");
			d.Add("f", "Friedrich");
			d.Add("g", "Gustav");
			d.Add("h", "Heinrich");
			d.Add("i", "Ida");
			d.Add("j", "Jakob");
			d.Add("k", "Kaiser");
			d.Add("l", "Leopold");
			d.Add("m", "Marie");
			d.Add("n", "Niklaus");
			d.Add("o", "Otto");
			d.Add("ö", "Örlikon");
			d.Add("p", "Peter");
			d.Add("q", "Quasi");
			d.Add("r", "Rosa");
			d.Add("s", "Sophie");
			d.Add("t", "Theodor");
			d.Add("u", "Ulrich");
			d.Add("ü", "Übermut");
			d.Add("v", "Viktor");
			d.Add("w", "Wilhelm");
			d.Add("x", "Xaver");
			d.Add("y", "Yverdon");
			d.Add("z", "Zürich");
		}
		private void InitNatoDict()
		{
			StringDictionary d = FNatoDict;
			d.Add("a", "Alpha");
			d.Add("b", "Bravo");
			d.Add("c", "Charlie");
			d.Add("d", "Delta");
			d.Add("e", "Echo");
			d.Add("f", "Foxtrott");
			d.Add("g", "Golf");
			d.Add("h", "Hotel");
			d.Add("i", "India");
			d.Add("j", "Juliet");
			d.Add("k", "Kilo");
			d.Add("l", "Lima");
			d.Add("m", "Mike");
			d.Add("n", "November");
			d.Add("o", "Oscar");
			d.Add("p", "Papa");
			d.Add("q", "Quebec");
			d.Add("r", "Romeo");
			d.Add("s", "Sierra");
			d.Add("t", "Tango");
			d.Add("u", "Uniform");
			d.Add("v", "Victor");
			d.Add("w", "Whisky");
			d.Add("x", "X-Ray");
			d.Add("y", "Yankee");
			d.Add("z", "Zulu");
		}
		private void InitJointForcesDict()
		{
			StringDictionary d = FJointForcesDict;
			d.Add("a", "Able");
			d.Add("b", "Baker");
			d.Add("c", "Charlie");
			d.Add("d", "Dog");
			d.Add("e", "Easy");
			d.Add("f", "Fox");
			d.Add("g", "George");
			d.Add("h", "How");
			d.Add("i", "Item");
			d.Add("j", "Jig");
			d.Add("k", "King");
			d.Add("l", "Love");
			d.Add("m", "Mike");
			d.Add("n", "Nan");
			d.Add("o", "Oboe");
			d.Add("p", "Peter");
			d.Add("q", "Queen");
			d.Add("r", "Roger");
			d.Add("s", "Sugar");
			d.Add("t", "Tare");
			d.Add("u", "Uncle");
			d.Add("v", "Victor");
			d.Add("w", "William");
			d.Add("x", "X-Ray");
			d.Add("y", "Yoke");
			d.Add("z", "Zebra");
		}
		private void UpdateSpelling(string _s)
		{
			int i;
			char c;
			StringDictionary d;

			d = FCurrentDict;
			lb_Result.Items.Clear();

			for (i = 0; i < _s.Length; i++) {
				c = _s[i];
				c = System.Char.ToLower(c);
				if (d.ContainsKey(c.ToString())) {
				  lb_Result.Items.Add(d[c.ToString()]);
				} else {
				  lb_Result.Items.Add("<unknown>");
				}
			}
		}
		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				if (components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.label1 = new System.Windows.Forms.Label();
			this.ed_What = new System.Windows.Forms.TextBox();
			this.label2 = new System.Windows.Forms.Label();
			this.cmb_Which = new System.Windows.Forms.ComboBox();
			this.lb_Result = new System.Windows.Forms.ListBox();
			this.SuspendLayout();
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(8, 8);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(140, 16);
			this.label1.TabIndex = 0;
			this.label1.Text = "What do you want to spell?";
			// 
			// ed_What
			// 
			this.ed_What.Location = new System.Drawing.Point(8, 24);
			this.ed_What.Name = "ed_What";
			this.ed_What.Size = new System.Drawing.Size(184, 20);
			this.ed_What.TabIndex = 1;
			this.ed_What.Text = "type it here ...";
			this.ed_What.TextChanged += new System.EventHandler(this.textBox1_TextChanged);
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(8, 56);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(168, 16);
			this.label2.TabIndex = 2;
			this.label2.Text = "Which phonetic alphabet to use?";
			// 
			// cmb_Which
			// 
			this.cmb_Which.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmb_Which.Location = new System.Drawing.Point(8, 72);
			this.cmb_Which.Name = "cmb_Which";
			this.cmb_Which.Size = new System.Drawing.Size(184, 21);
			this.cmb_Which.TabIndex = 3;
			this.cmb_Which.SelectionChangeCommitted += new System.EventHandler(this.cmb_Which_SelectionChangeCommitted);
			// 
			// lb_Result
			// 
			this.lb_Result.Location = new System.Drawing.Point(8, 104);
			this.lb_Result.Name = "lb_Result";
			this.lb_Result.Size = new System.Drawing.Size(184, 225);
			this.lb_Result.TabIndex = 4;
			// 
			// TPhoneSpell
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(200, 336);
			this.Controls.Add(this.lb_Result);
			this.Controls.Add(this.cmb_Which);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.ed_What);
			this.Controls.Add(this.label1);
			this.Name = "TPhoneSpell";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "PhoneSpell";
			this.ResumeLayout(false);
		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main()
		{
			Application.Run(new TPhoneSpell());
		}

		private void textBox1_TextChanged(object sender, System.EventArgs e)
		{
			UpdateSpelling(ed_What.Text);
		}
		
		private void cmb_Which_SelectionChangeCommitted(object sender, System.EventArgs e)
		{
			string s;
			object Item;

			Item = cmb_Which.SelectedItem;
			if (Item == null) {
				return;
			}
			s = Item.ToString();
			if (!FDictionaries.ContainsKey(s)) {
				return;
			}
			FCurrentDict = FDictionaries[s] as StringDictionary;
			UpdateSpelling(ed_What.Text);
		}
	}
}
