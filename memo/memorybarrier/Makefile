
all:
	g++ -std=c++11 -o mbar -O2 -g main.cpp Buffer.cpp -lboost_system -lboost_thread  -DASM_MBAR
	g++ -std=c++11 -o atom -O2 -g main.cpp Buffer.cpp -lboost_system -lboost_thread

obj:
	g++ -std=c++11 -O2 -o mb.o -c Buffer.cpp -DASM_MBAR
	g++ -std=c++11 -O2 -o at.o -c Buffer.cpp
	objdump -d mb.o > mb.txt
	objdump -d at.o > at.txt


check:
	spin dekker.pml

clean:
	rm -rf a.out
