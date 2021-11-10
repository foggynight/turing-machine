;; This will get past the parser, but redefining the number of tapes used within
;; the program will cause no rule found errors.

CONF:BEGIN
  TAPE:COUNT 2
CONF:END

0 _ _ -> 1 _ _ S S

CONF:BEGIN
  TAPE:COUNT 1
CONF:END

1 _ -> A _ S
