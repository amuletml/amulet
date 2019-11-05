Amulet Compiler Documentation
=============================

Currently this directory has all the text files that get embedded for
error message explanations.

Style conventions for error explanations:

* Format error explanations as Markdown:

  * Always use numbered lists
  * Always use indented code blocks, indenting by 8 spaces if they're
    meant to be in a list element
  * Align list elements vertically after the counter
  * Start any headers inside the explanation at `####`

* Hard wrap lines at 72 characters.

* Don't use \` in code examples (etc) because it will be suffixed with
  `{.amulet}` for the error index; If you really need monospace,
  non-highlighted text, suffix it with a ZWS
