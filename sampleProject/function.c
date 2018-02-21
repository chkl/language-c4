
int g(int* p) {
	return *p;
}

int f(int x) {
	int* p = &x; // &x;
	return g(p);

}
