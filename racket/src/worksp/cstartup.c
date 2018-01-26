#include <stdio.h>

/* 65535 characters should be enough for any string --- or so says
   MSVC. Convert "startup.inc" to a character array. */

int main(int argc, char **argv) {
  FILE *in, *out;
  int c, col = 0;

  in = fopen(argv[1], "r");
  out = fopen(argv[2], "w");

  fprintf(out, "{\nSHARED_OK static unsigned char expr[] = {\n");
  
  while (fgetc(in) != '(') { }

  while (1) {
    while (1) {
      c = fgetc(in);
      if (c == '"')
        break;

      if (c == EOF) {
        fprintf(out, "\n 0 };\n");
        fprintf(out, "EVAL_ONE_STR((char *)expr);\n }\n");
        return 0;
      }
    }

    while (1) {
      c = fgetc(in);
      if (c == '"')
        break;
      if (c == '\\')
        c = fgetc(in);
      fprintf(out, "%d,", c);
      col++;
      if (col == 20) {
        fprintf(out, "\n");
        col = 0;
      }
    }
  }

  return 0;
}
