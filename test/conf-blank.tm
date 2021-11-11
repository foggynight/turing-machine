;; The blank character may be redefined to allow the default blank character to
;; be used as a read/write symbol, but it must be a non-whitespace character.

CONF:BEGIN
  SYNTAX:BLANK #
CONF:END

0 # -> R # S
0 _ -> A _ S
