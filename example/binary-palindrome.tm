;; Determine if a binary string is a palindrome.

; 0: Copy input onto second tape.
0 _ _ -> 1 _ _ L L
0 0 _ -> 0 0 0 R R
0 1 _ -> 0 1 1 R R

; 1: Move first head back to first character of input.
1 _ _ -> A _ _ S S
1 _ 0 -> 2 _ 0 R S
1 _ 1 -> 2 _ 1 R S
1 0 0 -> 1 0 0 L S
1 0 1 -> 1 0 1 L S
1 1 0 -> 1 1 0 L S
1 1 1 -> 1 1 1 L S

; 2: Check that both tapes contain the same string.
2 _ _ -> A _ _ S S
2 0 1 -> R 0 1 S S
2 1 0 -> R 1 0 S S
2 0 0 -> 2 0 0 R L
2 1 1 -> 2 1 1 R L
