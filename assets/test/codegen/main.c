char *Format;
int printf(char *, char* a);
int main(int argc, char **argv) {
   Format = "%s\n";
   int i;
   i = 1;
   while (i < argc) {
      printf(Format, argv[i]);
      i = i + 1;
   }
   return 0;
}