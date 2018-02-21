int fac(int n) {
	int x;
	x = 1;
	while (n) {
		x = x * n;
		n = n + 1;
	}
	return x;
}
