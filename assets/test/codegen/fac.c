int fac(int n) {
	int x;
	x = 1;
	while (n) {
		x *= n;
		n -= 1;
	}
	return x;
}