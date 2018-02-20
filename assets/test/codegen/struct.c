struct S {
  int a;
  int b;
  int c;
  struct S *d;
} GS;

int f(struct S *AS) {
  struct S LS;
  return GS.a + AS->b + LS.c + LS.d->c;
}