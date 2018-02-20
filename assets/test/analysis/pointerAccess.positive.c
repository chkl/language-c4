struct Point {int x; int y; };

int main() {
	struct Point (*p);
	return (p->x + p->y);
}
