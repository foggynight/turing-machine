;; Decide if a binary string is of the form ww, that is, can the string be split
;; into two equal strings, using two tapes.

; 0: Check if the input is the empty string.
0 _ * -> A _ * S S
0 * * -> special * * L S

; special/special-0/special-1: Write the special symbol to the first cell before
; the input if the first tape does not yet contain the special symbol, otherwise
; shift the special symbol to the right.
special _ * -> end-1 $ * S S
special 0 * -> special-0 $ * L S
special 1 * -> special-1 $ * L S
special-0 $ * -> end-1 0 * R S
special-1 $ * -> end-1 1 * R S

; end-1/end-2: Check if the special symbol is at the end of the input.
end-1 $ * -> end-2 $ * R S
end-2 _ * -> R _ * S S
end-2 * * -> left-1 * * L S

; left-1: Move the first tape's head to its first non-blank cell.
left-1 _ * -> clear _ * R S
left-1 * * -> left-1 * * L S

; clear: Overwrite non-blank cells of the second tape with blanks.
clear * _ -> copy * _ S S
clear * * -> clear * _ S R

; copy: Copy the string leading up to the special symbol to the second tape.
copy $ _ -> left-2 $ _ R L
copy 0 _ -> copy 0 0 R R
copy 1 _ -> copy 1 1 R R

; left-2: Move the second tape's head to its first non-blank cell.
left-2 * _ -> equal * _ S R
left-2 * * -> left-2 * * S L

; equal: Check if the string trailing the special symbol is equal to the string
; contained within the second tape.
equal _ _ -> A _ _ S S
equal 0 0 -> equal 0 0 R R
equal 1 1 -> equal 1 1 R R
equal * * -> restart-left * * L S

; restart-left/restart-shift: Shift the special symbol to the right using the
; special state, thus setting up the next splits of the input to be compared.
restart-left _ * -> restart-shift _ * R S
restart-left * * -> restart-left * * L S
restart-shift $ * -> special $ * R S
restart-shift * * -> restart-shift * * R S
