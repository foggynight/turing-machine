;; Testing displaying the configuration tree using unicode characters.

CONF:BEGIN
  MODE:DETERMINISTIC FALSE
  TREE:UNICODE TRUE
CONF:END

0 * -> 1 * S
0 * -> 2 * S

1 * -> A * S
1 * -> R * S

2 * -> A * S
2 * -> R * S
