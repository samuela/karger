let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

type vertex = string
type edge = vertex * vertex
module VertexSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = vertex
  end)
type graph = { vertices: VertexSet.t; edges: edge list }

let edge_equal (a, b) (x, y) = (a = x && b = y) || (a = y && b = x)

let contract_random_edge graph =
  let ix = Random.int (List.length graph.edges) in
  let edge_to_remove = List.nth graph.edges ix in
  let a, b = edge_to_remove in
  let new_vertex = Printf.sprintf "%s/%s" a b in
  let vertices' =
    graph.vertices
    |> VertexSet.remove a
    |> VertexSet.remove b
    |> VertexSet.add new_vertex
  in
  let edges' =
    graph.edges
    (* Remove all edges between `a` and `b`. *)
    |> List.filter (fun edge -> not (edge_equal edge edge_to_remove))
    (* Map all vertex names to `new_vertex`. `rev_map` is more efficient than
      `map`. *)
    |> List.rev_map (fun (x, y) ->
      let x' = if x = a || x = b then new_vertex else x in
      let y' = if y = a || y = b then new_vertex else y in
      x', y')
  in
  { vertices = vertices'; edges = edges' }

let rec karger graph =
  if VertexSet.cardinal graph.vertices > 2 && graph.edges <> [] then
    karger (contract_random_edge graph)
  else
    List.length graph.edges

let rec run_karger best_cut edges =
  let cut = karger edges in
  let best_cut' =
    match best_cut with
    | None -> cut
    | Some c -> min c cut
  in
  if best_cut <> (Some best_cut') then
    (* `print_endline` flushes stdout where `Printf.printf` does not. *)
    print_endline (Printf.sprintf "Best cut so far: %d" best_cut');
  run_karger (Some best_cut') edges

let () =
  let filename =  Sys.argv.(1) in
  let contents = load_file filename in
  let edges =
    String.split_on_char '\n' contents
    |> List.filter (fun line -> String.length line > 0)
    |> List.rev_map (fun line ->
      let verts = String.split_on_char ' ' line in
      (List.nth verts 0, List.nth verts 1))
  in
  let vertices =
    List.fold_left
      (fun vs (a, b) -> vs |> VertexSet.add a |> VertexSet.add b)
      VertexSet.empty
      edges
  in
  print_endline "starting...";
  run_karger None { vertices; edges }
