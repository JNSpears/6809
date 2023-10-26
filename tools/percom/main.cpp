//
//	main.cpp
//

#include <cstdlib>
#include <cstdio>
#include <csignal>
#include <unistd.h>

#ifdef USIMDBG
#include "mc6809dbg.h"
#include "termdbg.h"
#else
#include "mc6809.h"
#include "term.h"
#endif
#include "mc6850.h"
#include "memory.h"

#include "Z5023.h"
#include "parser.hpp"

int main(int argc, char *argv[])
{
	if (argc < 4) {
		fprintf(stderr, "usage: usim <hexfile1> <hexfile2> <hexfile3> (%d)\n", argc);
		return EXIT_FAILURE;
	}

	(void)signal(SIGINT, SIG_IGN);

	//
	/// Percom SBC/9
	//

	// Scratch Ram.
	const Word PERCOM_SBC9_RAM_BASE  = 0xF000;
	const Word PERCOM_SBC9_RAM_SIZE  = 0x0400;

	// Monitor
	const Word PERCOM_SBC9_ROM1_BASE = 0xFC00;
	const Word PERCOM_SBC9_ROM1_SIZE = 0x0400;

	// Monitor extension.
	const Word PERCOM_SBC9_ROM2_BASE = 0xF800;
	const Word PERCOM_SBC9_ROM2_SIZE = 0x0400;

#ifdef USIMDBG
	mc6809dbg cpu;
	TerminalDbg term(cpu);
#else 
	mc6809 cpu;
	Terminal term;
#endif

	// Monitor extension.
	const Word PERCOM_SBC9_ACIA_BASE = 0xF7FE;

	auto ram = std::make_shared<RAM>(PERCOM_SBC9_RAM_SIZE);
	auto rom1 = std::make_shared<ROM>(PERCOM_SBC9_ROM1_SIZE);
	auto rom2 = std::make_shared<ROM>(PERCOM_SBC9_ROM2_SIZE);
	auto acia = std::make_shared<mc6850>(term);

	cpu.attach(ram, PERCOM_SBC9_RAM_BASE, ~(PERCOM_SBC9_RAM_SIZE - 1));
	cpu.attach(rom1, PERCOM_SBC9_ROM1_BASE, ~(PERCOM_SBC9_ROM1_SIZE - 1));
	cpu.attach(rom2, PERCOM_SBC9_ROM2_BASE, ~(PERCOM_SBC9_ROM2_SIZE - 1));
	cpu.attach(acia, PERCOM_SBC9_ACIA_BASE, 0xfffe);

	cpu.FIRQ.bind([&]() {
		return acia->IRQ;
	});

	//
	/// Corsham 64k Memory card (configured as 48k)
	//

	// System Ram.
	const Word CORSHAM_64K_RAM1_BASE  = 0;
	const Word CORSHAM_64K_RAM1_SIZE  = 32*1024;
	const Word CORSHAM_64K_RAM2_BASE  = 0x8000;
	const Word CORSHAM_64K_RAM2_SIZE  = 16*1024;

	auto main_ram1 = std::make_shared<RAM>(CORSHAM_64K_RAM1_SIZE);
	auto main_ram2 = std::make_shared<RAM>(CORSHAM_64K_RAM2_SIZE);

	cpu.attach(main_ram1, CORSHAM_64K_RAM1_BASE, (Word)~(CORSHAM_64K_RAM1_SIZE - 1));
	cpu.attach(main_ram2, CORSHAM_64K_RAM2_BASE, (Word)~(CORSHAM_64K_RAM2_SIZE - 1));

	//
	/// Percom LFD-400
	//

	const Word PERCOM_LDF400_BASE  = 0xc000;
	const Word PERCOM_LDF400_ROM_SIZE  = 1024;
	const Word PERCOM_LDF400_ROM1_BASE  = PERCOM_LDF400_BASE;
	const Word PERCOM_LDF400_ROM2_BASE  = PERCOM_LDF400_ROM1_BASE + PERCOM_LDF400_ROM_SIZE;
	const Word PERCOM_LDF400_ROM3_BASE  = PERCOM_LDF400_ROM2_BASE + PERCOM_LDF400_ROM_SIZE;
	const Word PERCOM_LDF400_CONTROLLER_BASE  = PERCOM_LDF400_ROM3_BASE + PERCOM_LDF400_ROM_SIZE;

	auto lfd400rom1 = std::make_shared<ROM>(PERCOM_LDF400_ROM_SIZE);
	auto lfd400rom2 = std::make_shared<ROM>(PERCOM_LDF400_ROM_SIZE);
	auto lfd400rom3 = std::make_shared<ROM>(PERCOM_LDF400_ROM_SIZE);
	auto lfd400ctrl = std::make_shared<Z5023>();

	cpu.attach(lfd400rom1, PERCOM_LDF400_ROM1_BASE, ~(PERCOM_LDF400_ROM_SIZE - 1));
	cpu.attach(lfd400rom2, PERCOM_LDF400_ROM2_BASE, ~(PERCOM_LDF400_ROM_SIZE - 1));
	cpu.attach(lfd400rom3, PERCOM_LDF400_ROM3_BASE, ~(PERCOM_LDF400_ROM_SIZE - 1));
	cpu.attach(lfd400ctrl, PERCOM_LDF400_CONTROLLER_BASE, ~(8 - 1));

 	/* Loop counter */
	int i = 0;

	// ** switches **
	int verbose = 0;
	char *dsk1 = NULL, *dsk2 = NULL, *dsk3 = NULL, *dsk4 = NULL;
	char **new_argv = NULL;

    /*
     * Read through command-line arguments for options.
     */
    for (i = 1; i < argc; i++)
    {
        // fprintf(stderr, "argv[%u] = %s\n", i, argv[i]);
        if (argv[i][0] == '-')
        {
			 if (argv[i][1] == 'v')
             {
                 verbose++;
             }
             else if (argv[i][1] == '1')
             {
                 dsk1 = argv[++i];
             }
             else if (argv[i][1] == '2')
             {
                 dsk2 = argv[++i];
             }
             else if (argv[i][1] == '3')
             {
                 dsk3 = argv[++i];
             }
             else if (argv[i][1] == '4')
             {
                 dsk4 = argv[++i];
             }
             else if (argv[i][1] == '-')
             {
             		// int new_argc = argc - i;
             		new_argv = &argv[i+1];
             		break;
             }
             else
             {
                 fprintf(stderr, "Invalid option.");
                 return 2;
             }
        }
    }

	fprintf(stderr, "\r\n");

	// Load ROM images.
	fprintf(stderr, "loading file 1,: %s \r\n", new_argv[0]);
	rom1->load_intelhex(new_argv[0], PERCOM_SBC9_ROM1_BASE);
	fprintf(stderr, "loading file 2,: %s \r\n", new_argv[1]);
	rom2->load_intelhex(new_argv[1], PERCOM_SBC9_ROM2_BASE);
	fprintf(stderr, "loading file 3,: %s \r\n", new_argv[2]);
	lfd400rom1->load_intelhex(new_argv[2], PERCOM_LDF400_ROM1_BASE);

	lfd400ctrl->debug = verbose;
	lfd400ctrl->mountedDisks[0].debug = lfd400ctrl->debug;
	lfd400ctrl->mountedDisks[1].debug = lfd400ctrl->debug;
	lfd400ctrl->mountedDisks[2].debug = lfd400ctrl->debug;
	lfd400ctrl->mountedDisks[3].debug = lfd400ctrl->debug;

	lfd400ctrl->mountedDisks[0].Mount(dsk1);
	fprintf(stderr, "loading DskFile 1,: %s \r\n", dsk1);
	lfd400ctrl->mountedDisks[1].Mount(dsk2);
	fprintf(stderr, "loading DskFile 2,: %s \r\n", dsk2);
	lfd400ctrl->mountedDisks[2].Mount(dsk3);
	fprintf(stderr, "loading DskFile 3,: %s \r\n", dsk3);
	lfd400ctrl->mountedDisks[3].Mount(dsk4);
	fprintf(stderr, "loading DskFile 4,: %s \r\n", dsk3);

	fprintf(stderr, "Done loading files\r\n");

	cpu.reset();
#ifdef USIMDBG
	cpu.debug(term);
#else 
	cpu.run();
#endif

	return EXIT_SUCCESS;
}
