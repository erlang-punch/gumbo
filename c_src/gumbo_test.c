#include "minunit.h"
#include "gumbo_erlang.h"
#include <string.h>

const char *valid = "<!doctype html><html></html>";
const char *invalid = "<html></html>";

/**
 * Check if an html document is valid.
 */
MU_TEST(test_valid) {
  mu_check(0 == gumbo_html_validation(valid, strlen(valid)));
  mu_check(1 == gumbo_html_validation(invalid, strlen(invalid)));
}

/**
 *
 */
MU_TEST(test_serializer) {
  // Some simple test
  // {gumbo, Content, Errors}
  // {gumbo, {<<"html">>, [], []}, []}
  // valid_serialized = {{<<"html">>, [], []}, []}
  // mu_check(
}

/**
 *
 */
MU_TEST_SUITE(test_suite) {
  MU_RUN_TEST(test_valid);
}

/**
 *
 */
int
main(int argc, char *argv[]) {
  MU_RUN_SUITE(test_suite);

  // TODO: remove
  _gumbo_parse("", "");
  
  MU_REPORT();
  return MU_EXIT_CODE;
}
