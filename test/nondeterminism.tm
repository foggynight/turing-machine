CONF:BEGIN
  MODE:DETERMINISTIC FALSE
CONF:END

0 * -> 1 * S
0 * -> 2 * S

1 * -> A * S
1 * -> R * S

2 * -> A * S
2 * -> R * S
2 * -> 3 * S

3 * -> A * S
3 * -> R * S
