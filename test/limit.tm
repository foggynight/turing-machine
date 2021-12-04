;; Testing the step and time limits. Give this program an empty input to test
;; the step limit, or a long input to test the time limit.

CONF:BEGIN
  LIMIT:STEP 10
  LIMIT:TIME 10
CONF:END

0 * -> 1 * R
1 * -> 0 * L
