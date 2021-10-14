;; Increment a binary number written in big-endian.

; 0: Check if the tape is empty.
0 _ -> E _ S
0 * -> 1 * S

; 1: Move the head to the last non-blank cell of the tape.
1 _ -> 2 _ L
1 * -> 1 * R

; 2: Add one and then add carries if necessary.
2 _ -> A 1 S
2 0 -> A 1 S
2 1 -> 2 0 L
