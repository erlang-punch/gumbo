#include <stdio.h>
#include "minunit.h"
#include "gumbo.h"

int
main() {
  printf("start test...\n");

  _gumbo_parse("", "");
  
  printf("end test.\n");
}
