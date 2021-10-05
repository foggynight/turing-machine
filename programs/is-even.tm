;; Determine if a binary number written in big-endian is even.

; 0: Move the head to the last non-blank cell of the tape.
0 0 -> 0 0 R
0 1 -> 0 1 R
0 _ -> 1 _ L

; 1: Check if the cell under the head is 0 or 1.
1 0 -> A 0 S
1 1 -> R 1 S
1 _ -> E _ S
