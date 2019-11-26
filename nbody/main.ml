open import "prelude.ml"
module List = import "amulet/list.ml"
open import {
  lua = "./support.lua.ml",
  scheme = "./support.scm.ml"
}

let x |> f = f x

let pi = 3.141592653589793
let solar_mass = 4.0 *. pi *. pi
let days_per_year = 365.24

type planet = Planet of {
  x : ref float,  y : ref float,  z : ref float,
  vx : ref float, vy : ref float, vz : ref float,
  mass : float
}

let rec pairwise f = function
| [] -> ()
| Cons (x, xs) ->
  let rec go = function
  | [] -> ()
  | Cons (y, ys) -> f x y; go ys
  go xs
  pairwise f xs

let rec times f n =
  if n <= 0 then () else
    f ()
    times f (n - 1)

let advance bodies dt =
  bodies |> pairwise (fun (Planet b) (Planet b') ->
    let dx = !b.x -. !b'.x and dy = !b.y -. !b'.y and dz = !b.z -. !b'.z
    let dist2 = dx *. dx +. dy *. dy +. dz *. dz
    let mag = dt /. (dist2 *. sqrt dist2)

    b.vx := !b.vx -. dx *. b'.mass *. mag
    b.vy := !b.vy -. dy *. b'.mass *. mag
    b.vz := !b.vz -. dz *. b'.mass *. mag

    b'.vx := !b'.vx +. dx *. b.mass *. mag
    b'.vy := !b'.vy +. dy *. b.mass *. mag
    b'.vz := !b'.vz +. dz *. b.mass *. mag)

  bodies |> List.iter (fun (Planet b) ->
    b.x := !b.x +. dt *. !b.vx
    b.y := !b.y +. dt *. !b.vy
    b.z := !b.z +. dt *. !b.vz)

let energy (bodies : list planet) : float =
  let initial = foldl (fun x (Planet b) ->
    x +. 0.5 *. b.mass *. (!b.vx *. !b.vx +. !b.vy *. !b.vy +. !b.vz *. !b.vz)) 0.0 bodies
  let e = ref initial
  bodies |> pairwise (fun (Planet b) (Planet b') ->
    let dx = !b.x -. !b'.x  and dy = !b.y -. !b'.y  and dz = !b.z -. !b'.z
    let distance = sqrt (dx *. dx +. dy *. dy +. dz *. dz)
    e := !e -. (b.mass *. b'.mass) /. distance); (* TODO: Why is this required? *)

  !e

let get_bodies () =
  let jupiter = Planet {
      x = ref (4.84143144246472090e+00),
      y = ref (0.0 -. 1.16032004402742839e+00),
      z = ref (0.0 -. 1.03622044471123109e-01),
      vx = ref (1.66007664274403694e-03 *. days_per_year),
      vy = ref (7.69901118419740425e-03 *. days_per_year),
      vz = ref (0.0 -. 6.90460016972063023e-05 *. days_per_year),
      mass = 9.54791938424326609e-04 *. solar_mass }

  let saturn = Planet {
      x = ref (8.34336671824457987e+00),
      y = ref (4.12479856412430479e+00),
      z = ref (0.0 -. 4.03523417114321381e-01),
      vx = ref (0.0 -. 2.76742510726862411e-03 *. days_per_year),
      vy = ref (4.99852801234917238e-03 *. days_per_year),
      vz = ref (2.30417297573763929e-05 *. days_per_year),
      mass = 2.85885980666130812e-04 *. solar_mass }

  let uranus = Planet {
      x = ref (1.28943695621391310e+01),
      y = ref (0.0 -. 1.51111514016986312e+01),
      z = ref (0.0 -. 2.23307578892655734e-01),
      vx = ref (2.96460137564761618e-03 *. days_per_year),
      vy = ref (2.37847173959480950e-03 *. days_per_year),
      vz = ref (0.0 -. 2.96589568540237556e-05 *. days_per_year),
      mass = 4.36624404335156298e-05 *. solar_mass }

  let neptune = Planet {
      x = ref (1.53796971148509165e+01),
      y = ref (0.0 -. 2.59193146099879641e+01),
      z = ref (1.79258772950371181e-01),
      vx = ref (2.68067772490389322e-03 *. days_per_year),
      vy = ref (1.62824170038242295e-03 *. days_per_year),
      vz = ref (0.0 -. 9.51592254519715870e-05 *. days_per_year),
      mass = 5.15138902046611451e-05 *. solar_mass }

  let sun = { x = ref 0.0, y = ref 0.0, z = ref 0.0, vx = ref 0.0, vy = ref 0.0, vz = ref 0.0, mass = solar_mass }
  let bodies = [ jupiter, saturn, uranus, neptune ]

  bodies |> List.iter (fun (Planet b) ->
    sun.vx := !sun.vx +. 0.0 -. (!b.vx *. b.mass) /. solar_mass
    sun.vy := !sun.vy +. 0.0 -. (!b.vy *. b.mass) /. solar_mass
    sun.vz := !sun.vz +. 0.0 -. (!b.vz *. b.mass) /. solar_mass)

  Cons (Planet sun, bodies)

let () =
  let bodies = get_bodies ()
  print_float (energy bodies)
  times (fun () -> advance bodies 0.01) 500000
  print_float (energy bodies)

(* Expected outputs:

1000:
-0.169075164
-0.169087605 *)
