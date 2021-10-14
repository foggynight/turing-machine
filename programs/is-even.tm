;; Determine if a binary number written in big-endian is even.

; 0: Move the head to the last non-blank cell of the tape.
0 _ -> 1 _ L
0 * -> 0 * R

; 1: Check if the cell under the head is 0 or 1.
1 _ -> E _ S
1 0 -> A 0 S
1 1 -> R 1 S
