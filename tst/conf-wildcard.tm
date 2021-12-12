CONF:BEGIN
  SYNTAX:WILDCARD .
CONF:END

0 _ -> 1 1 R
1 _ -> 2 2 R
2 _ -> 3 3 S

3 _ -> 4 _ R
3 . -> 3 . L

4 _ -> A _ S
4 . -> 4 * R
