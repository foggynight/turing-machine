;; Increment a binary number written in big-endian.

; 0: Check if the tape is empty.
0 0 -> 1 0 S
0 1 -> 1 1 S
0 _ -> E _ S

; 1: Move the head to the last non-blank cell of the tape.
1 0 -> 1 0 R
1 1 -> 1 1 R
1 _ -> 2 _ L

; 2: Add one and then add carries if necessary.
2 0 -> A 1 S
2 1 -> 2 0 L
2 _ -> A 1 S
