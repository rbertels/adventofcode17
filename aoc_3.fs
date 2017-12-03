type SpiralState =
  { Coordinates : int * int
    Direction : int * int 
    Distance : int 
    DistanceIndex : int
    DistanceIteration : int }

let startState =
  { Coordinates = (0, 0)
    Direction = (1, 0) 
    Distance = 1 
    DistanceIndex = 0
    DistanceIteration = 0 }

let inline addCoordinates t1 t2 = (fst t1 + fst t2, snd t1 + snd t2)

let updateState spiralState =
  let coords = addCoordinates spiralState.Coordinates spiralState.Direction
  let index = spiralState.DistanceIndex + 1
  if index = spiralState.Distance then
    let direction =
      match spiralState.Direction with
      | (1, 0) -> (0, -1)
      | (0, -1) -> (-1, 0)
      | (-1, 0) -> (0, 1)
      | (0, 1) -> (1, 0)
      | _ -> failwith "something went wrong"
    
    if spiralState.DistanceIteration = 1 then
      { spiralState with
          Coordinates = coords
          DistanceIndex = 0
          Direction = direction
          DistanceIteration = 0
          Distance = spiralState.Distance + 1 }
    else
      { spiralState with
          Coordinates = coords
          DistanceIndex = 0
          Direction = direction
          DistanceIteration = 1 }
  else 
    { spiralState with 
        Coordinates = coords
        DistanceIndex = index }

let findSteps index =
  [1..index-1] 
  |> List.scan (fun state _ -> updateState state) startState
  |> List.map (fun  state -> state.Coordinates)
  |> List.last
  |> function (x, y) -> abs x + abs y


let result = findSteps 277678


