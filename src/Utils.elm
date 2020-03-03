module Utils exposing (allPass, is, propEq, propSatisfies)


allPass : List (a -> Bool) -> a -> Bool
allPass list a =
    List.all ((|>) a) list


is =
    (==)


propEq : (obj -> prop) -> prop -> obj -> Bool
propEq func prop obj =
    func obj == prop


propSatisfies : (obj -> prop) -> (prop -> Bool) -> obj -> Bool
propSatisfies propFunc pred obj =
    pred (propFunc obj)
