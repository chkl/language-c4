#include "stdio.h"


int f(int);

int main() {
	for (int i = 0; i < 10; i++) {
		printf("f(%i) = %i\n", i, f(i));
	}
}
