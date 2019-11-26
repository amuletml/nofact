open import "amulet/base.ml"
open import {
  lua = "./support.lua.ml",
  scheme = "./support.scm.ml"
}

type complex 'a = Complex of { real : 'a, imaginary : 'a }

let (Complex x) !+ (Complex y) =
  Complex {
    real = x.real +. y.real,
    imaginary = x.imaginary +. y.imaginary 
  }

let (Complex { real = x, imaginary = y }) !* (Complex { real = x', imaginary = y' }) =
  Complex {
    real = x' *. x' -. y *. y',
    imaginary = x *. y' +. y *. x'
  }

let magnitude (Complex { real, imaginary }) =
  sqrt (real *. real +. imaginary *. imaginary)

external val print : 'a -> unit = "print"

let mandelbrot a =
  let rec loop iter acc =
    if iter < 50 then
      loop (iter + 1) ((acc !* acc) !+ a)
    else
      acc
  loop 0 (Complex { real = 0.0, imaginary = 0.0 })

let negate x = 0.0 -. x

let () =
  let rec inner y x =
    if x <=. 0.5 then
      let ours = mandelbrot (Complex { real = x, imaginary = y })
      if magnitude ours <. 2.0 then
        putc "*"
      else
        putc " "
      inner y (x +. 0.0315)
    else
      ()
  let rec outer y =
    if y >=. negate 1.0 then
      inner y (negate 2.0)
      putc "\n"
      outer (y -. 0.05)
    else
      ()
  outer 1.0
