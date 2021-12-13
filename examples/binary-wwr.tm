;; Decide if a binary string is of the form ww^r, that is, can the string be
;; split into two strings such that the second is the reverse of the first.

CONF:BEGIN
  STATE:INITIAL empty
CONF:END

; empty: Check if the input is the empty string.
empty _ _ -> A _ _ S S
empty * _ -> even * _ S S

; even/odd: Check if the input is an even length string.
even _ _ -> left _ _ L S
even * _ -> odd * _ R S
odd _ _ -> R _ _ S S
odd * _ -> even * _ R S

; left: Move first head back to first character of input.
left _ _ -> copy _ _ R S
left _ * -> compare _ * R S
left * * -> left * * L S

; copy: Copy input onto second tape.
copy _ _ -> left _ _ L L
copy 0 _ -> copy 0 0 R R
copy 1 _ -> copy 1 1 R R

; compare: Check if the second tape contains the reverse of the first tape.
compare _ _ -> A _ _ S S
compare 1 0 -> R 1 0 S S
compare 0 1 -> R 0 1 S S
compare * * -> compare * * R L
