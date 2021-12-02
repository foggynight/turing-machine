;; Testing the output of blanks. When given an empty input this program should
;; output the tape: 0_1, where _ is the blank character.
0 * -> 1 0 R
1 * -> 2 _ R
2 * -> 3 1 R
3 * -> A _ R
