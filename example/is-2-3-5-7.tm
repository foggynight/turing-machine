;; Non-deterministically determine if a binary number written in big-endian is
;; one of the first four prime numbers: 2, 3, 5, and 7.

CONF:BEGIN
  MODE:DETERMINISTIC FALSE
CONF:END

0 * -> 2_0 * S
0 * -> 3_0 * S
0 * -> 5_0 * S
0 * -> 7_0 * S

2_0 1 -> 2_1 1 R
2_0 * -> R * S
2_1 0 -> 2_2 0 R
2_1 * -> R * S
2_2 _ -> ret _ L
2_2 * -> R * S

3_0 1 -> 3_1 1 R
3_0 * -> R * S
3_1 1 -> 3_2 1 R
3_1 * -> R * S
3_2 _ -> ret _ L
3_2 * -> R * S

5_0 1 -> 5_1 1 R
5_0 * -> R * S
5_1 0 -> 5_2 0 R
5_1 * -> R * S
5_2 1 -> 5_3 1 R
5_2 * -> R * S
5_3 _ -> ret _ L
5_3 * -> R * S

7_0 1 -> 7_1 1 R
7_0 * -> R * S
7_1 1 -> 7_2 1 R
7_1 * -> R * S
7_2 1 -> 7_3 1 R
7_2 * -> R * S
7_3 _ -> ret _ L
7_3 * -> R * S

ret _ -> A _ R
ret 0 -> ret 0 L
ret 1 -> ret 1 L
