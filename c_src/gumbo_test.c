#include "minunit.h"
#include "gumbo_erlang.h"
#include <string.h>

/*
 * Check if an html document is valid.
 */
MU_TEST(test_valid) {
  char *valid = "<!doctype html><html></html>";
  mu_check(0 == gumbo_html_validation(valid, strlen(valid)));

  char *invalid = "<html></html>";
  mu_check(1 == gumbo_html_validation(invalid, strlen(invalid)));
}

MU_TEST_SUITE(test_suite) {
  MU_RUN_TEST(test_valid);
}

int
main(int argc, char *argv[]) {
  MU_RUN_SUITE(test_suite);
  MU_REPORT();
  return MU_EXIT_CODE;
}
