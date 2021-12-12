;; Example of checking for a tape count error in the first rule.

CONF:BEGIN
  TAPE:COUNT 2
CONF:END

0 _ -> 1 _ S
1 _ _ -> A _ _ S S
