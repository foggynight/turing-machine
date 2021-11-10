;; For the sake of clarity or to check for a tape count error in the first rule,
;; TAPE:COUNT may be used to explicitly state the number of tapes used.

CONF:BEGIN
  TAPE:COUNT 2
CONF:END

0 _ _ -> A _ _ S S
