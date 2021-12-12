CONF:BEGIN
  SYNTAX:LEFT <
  SYNTAX:RIGHT >
  SYNTAX:STAY -
CONF:END

0 _ -> 1 2 <
1 _ -> 2 1 >
2 2 -> 3 2 >
3 _ -> 4 _ -
4 _ -> A 3 -
