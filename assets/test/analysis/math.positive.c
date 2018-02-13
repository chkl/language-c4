int* max(int*, int*);

int main() {
}

int* max(int *x, int *y) {
	if (*x > *y) {
		return x;
	} else {
		return y;
	}
}
