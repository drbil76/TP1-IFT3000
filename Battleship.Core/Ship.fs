namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}
    
    //RECUÉRER LA TAILLE DU NAVIRE EN FONCTION DU NOM
    let sizeOf name =
        match name with 
        | Spy |PatrolBoat -> 2
        | Destroyer | Submarine -> 3
        | Cruiser -> 4
        | AircraftCarrier -> 5

  // Crée un bateau avec ses coordonnées calculées
    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let len = sizeOf name
        let indexCenter =
            match len with
            | 2 -> 1
            | 3 -> 1
            | 4 -> 2
            | 5 -> 2
            | _ -> len / 2

        let getOffset facing offset =
            match facing with
            | North -> (-offset, 0)
            | South -> (offset, 0)
            | East -> (0, offset)
            | West -> (0, -offset)

        let applyOffset (x, y) (dx, dy) = (x + dx, y + dy)

        let rec generateCoords i acc =
            if i = len then List.rev acc
            else
                let offset = i - indexCenter
                let dx, dy = getOffset facing offset
                let coord = applyOffset center (dx, dy)
                generateCoords (i + 1) (coord :: acc)

        let coords = generateCoords 0 []
        { Coords = coords; Center = center; Facing = facing; Name = name }

    // Retourne les cases adjacentes d'une coordonnée
    let adjacent (x, y) : Coord list =
        [ (x+1, y); (x, y+1); (x+1, y+1);
          (x-1, y); (x, y-1); (x-1, y-1);
          (x+1, y-1); (x-1, y+1) ]

    // Vérifie si un élément est dans une liste (version récursive de List.contains)
    let rec mem x l =
        match l with
        | [] -> false
        | y::ys -> x = y || mem x ys

    // Supprime les doublons d'une liste
    let rec removeDup l acc =
        match l with
        | [] -> acc
        | x::xs -> if mem x acc then removeDup xs acc else removeDup xs (x :: acc)

    // Retourne les coordonnées autour d’un bateau (le périmètre)
    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        let rec buildAdj coords acc =
            match coords with
            | [] -> acc
            | c::cs ->
                let adjs = adjacent c
                let valid =
                    let rec filter l acc2 =
                        match l with
                        | [] -> acc2
                        | (x, y)::rest ->
                            if x >= 0 && y >= 0 && x < fst dims && y < snd dims && not (mem (x, y) ship.Coords)
                            then filter rest ((x, y)::acc2)
                            else filter rest acc2
                    in filter adjs acc
                buildAdj cs (valid @ acc)

        buildAdj ship.Coords [] |> removeDup [] |> List.rev
