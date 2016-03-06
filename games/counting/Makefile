rebuild:
	mkdir -p build
	hastec -O2 -Wall -fno-warn-unused-do-bind Main.hs -o ./build/Main.js
	pandoc -s memo/memo.md > memo.html

clean:
	rm -rf *.jsmod *.o *.hi

-.PHONY: rebuild clean
