@cg (import (chicken flonum))

external val sqrt : float -> float =
  "fpsqrt"
external val putc : string -> unit = "display"
