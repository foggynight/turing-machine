CONF:BEGIN
  MODE:DETERMINISTIC FALSE
CONF:END

0 * -> 1 * S
0 * -> 4 * S

1 * -> 2 * S
1 * -> 3 * S

2 * -> A * S
2 * -> R * S

3 * -> A * S
3 * -> R * S

4 * -> 5 * S
4 * -> 6 * S

5 * -> A * S
5 * -> R * S

6 * -> A * S
6 * -> R * S
