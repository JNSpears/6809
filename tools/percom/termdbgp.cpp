//
//
//	term.cpp
//
//	(C) R.P.Bellis 1994
//

#include <cstdlib>
#include <cassert>
#include "termdbgp.h"
#include "Z5023.h"

#include <iostream>
//#include <fstream>
#include <iomanip>

using namespace std;


extern shared_ptr<Z5023> lfd400ctrl;

//------------------------------------------------------------------------
// Machine dependent Terminal implementations
//------------------------------------------------------------------------

//------------------------------------------------------------------------

TerminalDbgPercom::TerminalDbgPercom(mc6809dbg& _sys)
: TerminalDbg(_sys)
{
}

TerminalDbgPercom::~TerminalDbgPercom()
{
}

//------------------------------------------------------------------------
// Machine independent part, handles ssh-like tilde-escapes
//------------------------------------------------------------------------

void TerminalDbgPercom::tilde_escape_help_other()
{
	fprintf(stderr, " ~M <n> <file> - mount drive.\r\n");
	fprintf(stderr, " ~U <n> - unmount drive.\r\n");
	TerminalDbg::tilde_escape_help_other();
}

void TerminalDbgPercom::tilde_escape_do_other(char ch)
{
	int drive;
	string filename;

	switch (ch) {
	case 'm':
	case 'M':
		set_debug();
		cerr << "\r\nMount <d> <filename>\r" << endl;
		cin >> drive >> filename;
		set_exec();
		cerr << "\r\nMount dr:" << drive << " filename:" << filename  << "\r" << endl;
		lfd400ctrl->Mount(drive, filename.c_str());
		lfd400ctrl->Info();
		break;
	case 'u':
	case 'U':
		set_debug();
		cerr << "\r\nUnMount <d>\r" << endl;
		cin >> drive;
		set_exec();
		cerr << "\r\nUnMount dr:" << drive << "\r" << endl;
		lfd400ctrl->UnMount(drive);
		lfd400ctrl->Info();
		break;
	default:
		TerminalDbg::tilde_escape_do_other(ch);
		break;
	}
}

