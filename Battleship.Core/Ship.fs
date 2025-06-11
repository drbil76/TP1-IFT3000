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

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        []
