;; Decrement a binary number written in big-endian.

; 0: Check if the tape is empty.
0 _ -> E _ S
0 * -> 1 * S

; 1: Move the head to the last non-blank cell of the tape.
1 _ -> 2 _ L
1 * -> 1 * R

; 2: Subtract one and then subtract carries if necessary.
2 _ -> 3 _ S
2 0 -> 2 1 L
2 1 -> 3 0 S

; 3/4: Clear all leading zeroes.
3 _ -> 4 _ R
3 * -> 3 * L
4 _ -> 5 _ L
4 0 -> 4 _ R
4 1 -> A 1 S

; 5: If tape is now empty, add a zero.
5 _ -> A 0 S
5 * -> A * S
