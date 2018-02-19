int f() {
	goto blah;
	1 + 1;

	while (1) {
		blah: 2 + 2;
	}
}
int main() {
}
