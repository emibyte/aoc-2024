open Core
open Stdio

module Coordinate = struct
  module T = struct
    type t = int * int

    let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let ( + ) (a1, a2) (b1, b2) = a1 + b1, a2 + b2
  end

  include T
  include Comparable.Make (T)
end

module CoordinateSet = Set.Make (Coordinate)

let grid =
  In_channel.read_lines "inputs/day6.txt" |> List.to_array |> Array.map ~f:String.to_array
;;

let get_grid_point row col =
  if row >= Array.length grid || col >= Array.length grid.(0) || row < 0 || col < 0
  then None
  else Some grid.(row).(col)
;;

type direction =
  | Up
  | Right
  | Down
  | Left

let get_direction = function
  | '^' -> Up
  | '>' -> Right
  | 'v' -> Down
  | '<' -> Left
  | _ -> failwith "non valid guard"
;;

type guard =
  { position : Coordinate.t
  ; direction : direction
  ; traversed : CoordinateSet.t
  }

type cell =
  | Guard of guard
  | Empty
  | Obstacle
  | Outside

let turn_at_obstacle guard =
  match guard.direction with
  | Up -> { guard with direction = Right }
  | Right -> { guard with direction = Down }
  | Down -> { guard with direction = Left }
  | Left -> { guard with direction = Up }
;;

let get_step_destination guard = function
  | Up -> Coordinate.( + ) guard.position (-1, 0)
  | Right -> Coordinate.( + ) guard.position (0, 1)
  | Down -> Coordinate.( + ) guard.position (1, 0)
  | Left -> Coordinate.( + ) guard.position (0, -1)
;;

let walk_in_dir guard = function
  (* TODO: not sure if guard.position is already the updated one for the set update (i think not) *)
  | Up ->
    { guard with
      position = get_step_destination guard Up
    ; traversed = Set.add guard.traversed (get_step_destination guard Up)
    }
  | Right ->
    { guard with
      position = get_step_destination guard Right
    ; traversed = Set.add guard.traversed (get_step_destination guard Right)
    }
  | Down ->
    { guard with
      position = get_step_destination guard Down
    ; traversed = Set.add guard.traversed (get_step_destination guard Down)
    }
  | Left ->
    { guard with
      position = get_step_destination guard Left
    ; traversed = Set.add guard.traversed (get_step_destination guard Left)
    }
;;

let get_cell_type (row, col) =
  match get_grid_point row col with
  | Some c ->
    (match c with
     | '#' -> Obstacle
     | '^' | '>' | 'v' | '<' ->
       Guard
         { position = row, col
         ; direction = get_direction c
         ; traversed = Set.add CoordinateSet.empty (row, col)
         }
     | _ -> Empty)
  | None -> Outside
;;

let take_a_step guard =
  let step_row, step_col = get_step_destination guard guard.direction in
  match get_cell_type (step_row, step_col) with
  | Empty -> walk_in_dir guard guard.direction
  | Guard _ -> walk_in_dir guard guard.direction
  | Obstacle -> turn_at_obstacle guard
  | Outside -> guard
;;

let rec walk_maze guard =
  let guard_after_step = take_a_step guard in
  if
    Poly.( = ) guard_after_step.direction guard.direction
    && Poly.( = ) guard_after_step.position guard.position
  then guard
  else walk_maze guard_after_step
;;

let init_guard grid =
  let is_guard = function
    | '^' | 'v' | '>' | '<' -> true
    | _ -> false
  in
  let row, col_and_dir_opt =
    Array.mapi grid ~f:(fun _ row -> Array.findi row ~f:(fun _ el -> is_guard el))
    |> Array.findi ~f:(fun _ x -> Option.is_some x)
    |> Option.value_exn
  in
  let row, (col, dir) = row, Option.value_exn col_and_dir_opt in
  let position, dir = (row, col), dir in
  { position = row, col
  ; direction = get_direction dir
  ; traversed = Set.add CoordinateSet.empty position
  }
;;

let part_1 =
  let final = init_guard grid |> walk_maze in
  Set.length final.traversed
;;

let () = part_1 |> Int.to_string |> print_endline
