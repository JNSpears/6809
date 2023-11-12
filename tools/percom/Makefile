USIM_PATH	= ../usim/
USIMDBG_PATH	= ../usimdbg/

CPPFLAGS = -O3  -I ../usim -I ../usimdbg --std=c++17
  
SRCS		= main.cpp Z5023.cpp
OBJS		= $(SRCS:.cpp=.o)

SRCSd		= main.cpp Z5023.cpp termdbgp.cpp
OBJSd		= $(SRCSd:.cpp=.o)

BIN			= percom

all: $(BIN) $(BIN)d

$(BIN):	$(OBJS) $(LIBS)
	$(CXX) $(CCFLAGS) $(LDFLAGS) $(OBJS) $(USIM_PATH)term.o -L$(USIM_PATH) -lusim -o $(@)

$(BIN)d: maind.o Z5023.o termdbgp.o $(LIBS) # debugger
	$(CXX) $(CCFLAGS) $(LDFLAGS) maind.o Z5023.o termdbgp.o $(USIM_PATH)term.o -lreadline -L$(USIM_PATH) -L$(USIMDBG_PATH) -lusim -lusimdbg -o $(@)


main.o: main.cpp

maind.o: main.cpp
	$(CXX) $(CPPFLAGS) -DUSIMDBG  $(CCFLAGS) -c $< -o $(@)

.SUFFIXES:	.cpp

.cpp.o:
	$(CXX) $(CPPFLAGS) $(CCFLAGS) -c $<

# kill:
# 	kill `ps -C percom -o pid | tail -1`

# killd:
# 	kill `ps -C percomd -o pid | tail -1`

.PHONY: clean
clean:
	$(RM) -f main.o $(OBJS)

depend:
	makedepend $(SRCS)

# DO NOT DELETE THIS LINE -- make depend depends on it.

main.o: /usr/include/unistd.h /usr/include/features.h
main.o: /usr/include/stdc-predef.h Z5023.h /usr/include/memory.h
main.o: /usr/include/string.h /usr/include/strings.h
Z5023.o: Z5023.h
