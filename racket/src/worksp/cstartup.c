#include <stdio.h>

/* 65535 characters should be enough for any string --- or so says
   MSVC. Convert "startup.inc" to a character array. */

int main(int argc, char **argv) {
  FILE *in, *out;
  int c, col = 0;

  in = fopen(argv[1], "r");
  out = fopen(argv[2], "w");

  fprintf(out, "#define EVAL_STARTUP EVAL_ONE_STR((char *)startup_source)\n");
  fprintf(out, "static unsigned char startup_source[] = {\n");
  
  while (1) {
    while (1) {
      c = fgetc(in);
      if (c == '"')
        break;

      if (c == EOF) {
        fprintf(out, "\n 0 };\n");
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
