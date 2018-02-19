int f(int x) {
	foo: x+3; 
}

int g(int y) {
	foo: y+3;
}

int main() {
	f(3) + g(5);
}
