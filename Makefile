all: rinterface.o  overallentropy.o
	g++ -O3  -lm -shared  -o overallentropy.so rinterface.o overallentropy.o -L/usr/local/lib/R/lib -lR

rinterface.o: rinterface.c 
	gcc -std=gnu99 -I/usr/local/lib/R/include  -fpic  -O3 -pipe  -g -c rinterface.c -o rinterface.o

overallentropy.o: overallentropy.cc
	g++ -O3 -c -fpic -fomit-frame-pointer -fno-operator-names -march=core2  -m64  -mssse3 -mfpmath=sse -ffast-math  -Wall -Wno-deprecated overallentropy.cc -o overallentropy.o -lm 

clean:
	rm *.o
	rm *.so
