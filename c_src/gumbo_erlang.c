/* 
 * Gumbo Erlang Library
 *
 * This library has been created to support HTML validation and
 * parsing using an external interface to Erlang. In this case, Gumbo
 * project is used to do the job
 * 
 * Usage and Exported Functions
 * 
 * gumbo:valid_html/1
 * gumbo:valid
 *
 * 
 * {<<"div">>, [{<<"class">>, <<"main">>}, ...], [{div, [], ...}]}
 * 
 * see https://matze.github.io/clib-doc/gumbo-parser/index.html#parser_8c_1acc6867266c808111d0c38b790f5a01fa
 *     https://github.com/google/gumbo-parser/blob/aa91b27b02c0c80c482e24348a457ed7c3c088e0/tests/parser.cc
 *     https://github.com/google/gumbo-parser/blob/aa91b27b02c0c80c482e24348a457ed7c3c088e0/tests/test_utils.cc
 */
#include <gumbo.h>
#include <ei.h>
#include "gumbo_erlang.h"

// to remove
#include <assert.h>
#include <unistd.h>
#include <stdlib.h>

// gumbo:valid_html().
// gumbo:to_term().
// int valid_html(const char *)
//   -spec valid_html(bitstring()) -> boolean()
// int html_to_term(const char *, char *result, size_t len)
//  {ok, term()}
//  {error, reason()}
// 

/*
 *
 */
int
gumbo_html_validation(const char *input, size_t input_length) {
  // GumboOutput *gumbo_output = gumbo_parse(input);
  GumboOptions options = kGumboDefaultOptions;
  GumboOutput *gumbo_output = gumbo_parse_with_options(&options, input, input_length);
  int ret = gumbo_output->errors.length;
  gumbo_destroy_output(&kGumboDefaultOptions, gumbo_output);
  return ret;
}

GumboNode *
_gumbo_get_child(GumboNode *parent, int index) {
  if (parent->type == GUMBO_NODE_DOCUMENT)
    return (parent->v.document.children.data[index]);
  else
    return (parent->v.element.children.data[index]);
}

int
_gumbo_get_child_count(GumboNode *node) {
  if (node->type == GUMBO_NODE_DOCUMENT)
    return node->v.document.children.length;
  else
    return node->v.element.children.length;
}

void
_print_count(GumboNode *node, const char *name) {
  printf("element (%s) count: %d\n", name, _gumbo_get_child_count(node));
}

void
_gumbo_parse(const char *_input, const char *_output) {
  const char *input = "<><!doctype html><html><body>this is my content</body></html>";
  GumboOutput *gumbo_output = gumbo_parse(input);

  if (gumbo_output->errors.length>0) {
    int len = gumbo_output->errors.length;
    printf("error: %d, %d\n", len, gumbo_output->errors.capacity);
  }
  
  /* A root element, {root, [{html, []}]}
   * 
   */
  GumboNode *root = gumbo_output->document;
  _print_count(root, "root");

  /* An html element, {html, [{body, []}]}}
   *
   */
  GumboNode *html = _gumbo_get_child(root, 0);
  _print_count(html, "html");

  /* A body element {body, [{text, <<"this is my content">>}]}
   *
   */
  GumboNode *body = _gumbo_get_child(html, 1);
  _print_count(body, "body");

  GumboNode *text = _gumbo_get_child(body, 0);  
  
  printf("%d\n", _gumbo_get_child_count(body));
  printf("%d\n", text->type);
  printf("%s\n", text->v.text.text);

  free(gumbo_output);
}
