;; Add two binary numbers written in big-endian using two tapes. Input is to be
;; of the form: NUMBER+NUMBER. The result is written on the first tape.

CONF:BEGIN
  STATE:INITIAL move
CONF:END

; move: Move the first number onto the second tape and erase the + symbol.
move + _ -> last _ _ R S
move 0 _ -> move _ 0 R R
move 1 _ -> move _ 1 R R

; last: Move the heads to the last non-blank symbol of each tape.
last _ _ -> add _ _ L L
last * _ -> last * _ R S

; add/carry: Add the two numbers, storing the result on the first tape.
add _ _ -> A _ _ R R
add * _ -> add * _ L S
add _ 0 -> add 0 _ L L
add _ 1 -> add 1 _ L L
add 0 0 -> add 0 _ L L
add 1 0 -> add 1 _ L L
add 0 1 -> add 1 _ L L
add 1 1 -> carry 0 _ L L
carry _ _ -> add 1 _ L L
carry 0 _ -> add 1 _ L L
carry _ 0 -> add 1 _ L L
carry 1 _ -> carry 0 _ L L
carry _ 1 -> carry 0 _ L L
carry 0 0 -> add 1 _ L L
carry 1 0 -> carry 0 _ L L
carry 0 1 -> carry 0 _ L L
carry 1 1 -> carry 1 _ L L
