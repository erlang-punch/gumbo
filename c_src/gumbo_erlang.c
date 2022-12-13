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

/** 
 * gumbo_html_validation function is the simpliest function present
 * in this library. It takes a raw input as buffer, parse it
 * with gumbo_parse_with_options and returns the error code as
 * integer.
 *
 * @param input the string to parse
 * @param size_t the size of the string
 */
int
gumbo_html_validation(const char *input, size_t input_length) {
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
  const char *input = "<!doctype html><html><body>this is my content</body></html>";  
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
  _gumbo_node_type(root);

  /* An html element, {html, [{body, []}]}}
   *
   */
  GumboNode *html = _gumbo_get_child(root, 0);
  _gumbo_node_type(html);
  _print_count(html, "html");

  /* A body element {body, [{text, <<"this is my content">>}]}
   *
   */
  GumboNode *body = _gumbo_get_child(html, 1);
  _gumbo_node_type(body);
  _print_count(body, "body");

  GumboNode *text = _gumbo_get_child(body, 0);  
  
  printf("%d\n", _gumbo_get_child_count(body));
  printf("%d\n", text->type);
  printf("%s\n", text->v.text.text);

  gumbo_destroy_output(&kGumboDefaultOptions, gumbo_output);
}

void
_gumbo_parser2(){
  const char *input = "<><!doctype html><html><body>this is my content</body></html>";
  GumboOutput *gumbo_output = gumbo_parse(input);

  if (gumbo_output->errors.length>0) {
    int len = gumbo_output->errors.length;
    printf("error: %d, %d\n", len, gumbo_output->errors.capacity);
  }

  GumboNode *p = NULL;
  GumboNode *root = gumbo_output->document;
}

void
_gumbo_node_type(GumboNode *node) {
  switch(node->type) {
  case GUMBO_NODE_DOCUMENT: printf("GUMBO_NODE_DOCUMENT\n"); break;
  case GUMBO_NODE_ELEMENT: printf("GUMBO_NODE_ELEMENT\n"); break;
  case GUMBO_NODE_TEXT: printf("GUMBO_NODE_TEXT\n"); break;
  case GUMBO_NODE_CDATA: printf("GUMBO_NODE_CDATA\n"); break;
  case GUMBO_NODE_COMMENT: printf("GUMBO_NODE_COMMENT\n"); break;
  case GUMBO_NODE_WHITESPACE: printf("GUMBO_NODE_WHITESPACE\n"); break;
  case GUMBO_NODE_TEMPLATE: printf("GUMBO_NODE_TEMPLATE\n"); break;
  }
}

void
_document_has_doctype(GumboDocument *document) {
  printf("%d\n", document->has_doctype);
}

void
_document_name(GumboDocument *document) {
  printf("%s\n", document->name);
}

void
_document_public_identifier(GumboDocument *document) {
  printf("%s\n", document->public_identifier);
}

void
_document_system_identifier(GumboDocument *document) {
  printf("%s\n", document->system_identifier);
}

void
_vector_length(GumboVector *vector) {
  printf("%d\n", vector->length);
}

void
_vector_get_data(GumboVector *vector, unsigned int index) {
}

