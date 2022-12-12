/*
 *
 */
int gumbo_html_validation(const char *, size_t);
void _gumbo_parse(const char *, const char *);

/**
 * {
 *   {GUMBO_NODE_DOCUMENT,  "gumbo_node_document"},
 *   {GUMBO_NODE_ELEMENT, "gumbo_node_element"},
 *   {GUMBO_NODE_TEXT, "gumbo_node_text"},
 *   {GUMBO_NODE_CDATA, "gumbo_node_cdata"},
 *   {GUMBO_NODE_COMMENT, "gumbo_node_comment"},
 *   {GUMBO_NODE_WHITESPACE, "gumbo_node_whitespace"},
 *   {GUMBO_NODE_TEMPLATE, "gumbo_node_template"}
 * }
 *
 */
