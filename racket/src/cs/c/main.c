#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "boot.h"

#define MZ_CHEZ_SCHEME
#ifndef INITIAL_BIN_TYPE
# define INITIAL_BIN_TYPE "zi"
#endif
#ifndef RACKET_IS_GUI
# define RACKET_IS_GUI 0
#endif

#include "../../start/config.inc"

char *boot_file_data = "BooT FilE OffsetS:xxxxyyyyyzzzz";
static int boot_file_offset = 18;

#ifdef OS_X
# include <mach-o/dyld.h>
char *get_self_path()
{
  char buf[1024], *s;
  uint32_t size = sizeof(buf);
  int r;
  
  r = _NSGetExecutablePath(buf, &size);
  if (!r)
    return strdup(buf);
  else {
    s = malloc(size);
    r = _NSGetExecutablePath(s, &size);
    if (!r)
      return s;
    fprintf(stderr, "failed to get self\n");
    exit(1);
  }
}
#endif

#ifndef do_pre_filter_cmdline_arguments
# define do_pre_filter_cmdline_arguments(argc, argv) /* empty */
#endif

int main(int argc, char **argv)
{
  char *self, *prog = argv[0], *sprog = NULL;
  int pos1, pos2, pos3;
  long segment_offset;

  do_pre_filter_cmdline_arguments(&argc, &argv);

  argc--;
  argv++;

  extract_built_in_arguments(&prog, &sprog, &argc, &argv);
  segment_offset = get_segment_offset();

  self = get_self_path();

  memcpy(&pos1, boot_file_data + boot_file_offset, sizeof(pos1));
  memcpy(&pos2, boot_file_data + boot_file_offset + 4, sizeof(pos2));
  memcpy(&pos3, boot_file_data + boot_file_offset + 8, sizeof(pos2));

  racket_boot(argc, argv, self, segment_offset,
              extract_coldir(), extract_configdir(),
              pos1, pos2, pos3,
              RACKET_IS_GUI);
  
  return 0;
}
