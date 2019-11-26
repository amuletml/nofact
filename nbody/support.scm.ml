@cg (import (chicken flonum))

external val sqrt : float -> float =
  "fpsqrt"

@cg (flonum-print-precision 9)
external val print_float : float -> unit = "print"
