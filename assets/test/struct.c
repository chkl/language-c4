struct S {
  int a;
  int b;
  int c;
  struct S *d;
} GS;

int f(struct S *AS) {
  struct S LS;
  return LS.d->c;
}
int main() { }
