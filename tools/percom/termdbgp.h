//
//
//	term.h
//
//	(C) R.P.Bellis 1994
//

#pragma once


#include "termdbg.h"
#include "mc6809dbg.h"

class TerminalDbgPercom : public TerminalDbg {

protected:
	virtual void		tilde_escape_help_other();
	virtual void 		tilde_escape_do_other(char ch);

// Public constructor and destructor
public:
						 TerminalDbgPercom(mc6809dbg& _sys);
	virtual				~TerminalDbgPercom();
};
