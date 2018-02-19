// #include "stdio.h"

void printf(char*, int);

int callTwice( int (*f)(int), int x ) {
	return (f(f(x)));
}



int timesTwo(int x) {
	return (2*x);
}

int main() {
	int y = callTwice(timesTwo, 2); // should be 8
	printf("result: %i",y);
	return 0;
}
