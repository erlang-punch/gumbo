#include "minunit.h"
#include "gumbo.h"

/*
 * Check if an html document is valid.
 */
MU_TEST(test_valid) {
  mu_check(0 == _valid("<!doctype html><html></html>"));
  mu_check(1 == _valid("<html></html>"));
}

/*
 *
 */

MU_TEST_SUITE(test_suite) {
  MU_RUN_TEST(test_valid);
}

int
main(int argc, char *argv[]) {
  MU_RUN_SUITE(test_suite);
  MU_REPORT();
  return MU_EXIT_CODE;
}
