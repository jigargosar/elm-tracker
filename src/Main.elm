module Main exposing (main)

import Bomb exposing (Bomb)
import BombId exposing (BombId)
import Box exposing (Box)
import List.Extra
import Location as Loc exposing (Location)
import Maybe.Extra
import Playground exposing (..)
import Random exposing (Generator, Seed)
import Random.Extra
import String exposing (fromInt)



-- Config


isDebug =
    True


buttonWidth =
    100


buttonHeight =
    50


firstUpgradeCost =
    75


secondUpgradeCost =
    150


bombTowerReloadDelay =
    50


bombTowerRange =
    150


bombTowerRange2 =
    200


bombSpeed =
    3


bombAOE =
    40


towerReloadDelay =
    40


arrowTowerRange =
    150


arrowTowerRange2 =
    200


bulletSpeed =
    10


monsterSpeed =
    1


monsterHealth =
    5


allTowersViewWidth =
    30



-- Tower
-- TODO: Rename to ArrowTower?


type TowerId
    = TowerId Int


towerIdGenerator : Generator TowerId
towerIdGenerator =
    mapRandomIdInt TowerId


type TowerType
    = ArrowTower
    | BombTower


type alias Tower =
    { -- META
      id : TowerId
    , delay : Number -- RELOAD TIME
    , location : Location
    , viewWidth : Number
    , towerType : TowerType

    -- STATE
    , upgrade : UpgradeState
    , elapsed : Number -- RELOAD PROGRESS
    }


isPowerUpgraded : Tower -> Bool
isPowerUpgraded tower =
    isUpgradeApplied PowerUpgrade (upgradeOfTower tower)


rangeOfTower : Tower -> Number
rangeOfTower tower =
    case ( tower.towerType, isUpgradeApplied RangeUpgrade tower.upgrade ) of
        ( ArrowTower, False ) ->
            arrowTowerRange

        ( ArrowTower, True ) ->
            arrowTowerRange2

        ( BombTower, False ) ->
            bombTowerRange

        ( BombTower, True ) ->
            bombTowerRange2


arrowTowerGenerator : Location -> Generator Tower
arrowTowerGenerator location =
    towerIdGenerator
        |> Random.map
            (\tid ->
                { id = tid
                , delay = towerReloadDelay
                , towerType = ArrowTower
                , location = location
                , viewWidth = allTowersViewWidth
                , upgrade = NoUpgrades
                , elapsed = 0
                }
            )


bombTowerGenerator : Location -> Generator Tower
bombTowerGenerator location =
    towerIdGenerator
        |> Random.map
            (\tid ->
                { id = tid
                , delay = bombTowerReloadDelay
                , towerType = BombTower
                , location = location
                , viewWidth = allTowersViewWidth
                , upgrade = NoUpgrades
                , elapsed = 0
                }
            )


upgradeTower : UpgradeType -> Tower -> Maybe ( Number, Tower )
upgradeTower upgradeType tower =
    applyUpgrade upgradeType (upgradeOfTower tower)
        |> Maybe.map (Tuple.mapSecond (\upgrade -> { tower | upgrade = upgrade }))


idOfTower : Tower -> TowerId
idOfTower =
    .id


locationOfTower : Tower -> Location
locationOfTower =
    .location


upgradeOfTower : Tower -> UpgradeState
upgradeOfTower =
    .upgrade


isLocationInRangeOfTower : Location -> Tower -> Bool
isLocationInRangeOfTower location tower =
    Loc.distanceFromTo location tower.location <= rangeOfTower tower


isLocationOnTowerView : Location -> Tower -> Bool
isLocationOnTowerView location tower =
    Loc.isLocationInSquareAt tower.location tower.viewWidth location



-- BUTTON


type UpgradeType
    = RangeUpgrade
    | PowerUpgrade


type UpgradeState
    = NoUpgrades
    | OneUpgraded UpgradeType
    | BothUpgraded


type alias UpgradeButton =
    { box : Box
    , state : UpgradeButtonState
    , upgradeType : UpgradeType
    }


type UpgradeButtonState
    = Active
    | Disabled Number
    | Enabled Number


upgradeCost : UpgradeState -> UpgradeType -> Maybe Number
upgradeCost upgradeState upgradeType =
    case upgradeState of
        NoUpgrades ->
            Just firstUpgradeCost

        OneUpgraded appliedUT ->
            if upgradeType == appliedUT then
                Nothing

            else
                Just secondUpgradeCost

        BothUpgraded ->
            Nothing


initUpgradeButton : Box -> UpgradeState -> UpgradeType -> Number -> UpgradeButton
initUpgradeButton box upgradeState upgradeType gold =
    { box = box
    , upgradeType = upgradeType
    , state =
        case upgradeCost upgradeState upgradeType of
            Nothing ->
                Active

            Just cost ->
                if cost <= gold then
                    Enabled cost

                else
                    Disabled cost
    }


viewUpgradeButton : UpgradeButton -> Shape
viewUpgradeButton btn =
    [ Box.shape
        (case btn.state of
            Active ->
                darkOrange

            Disabled _ ->
                darkGray

            Enabled _ ->
                lightOrange
        )
        btn.box
    , (case btn.state of
        Active ->
            words white "upgraded"

        Disabled number ->
            words black (fromInt (round number))

        Enabled number ->
            words black (fromInt (round number))
      )
        |> moveDown 10
    , words
        (case btn.state of
            Active ->
                white

            Disabled _ ->
                black

            Enabled _ ->
                black
        )
        (case btn.upgradeType of
            PowerUpgrade ->
                "POWER"

            RangeUpgrade ->
                "RANGE"
        )
        |> moveUp 10
    ]
        |> group
        |> Box.moveShape btn.box


initTowerUpgradeButtons : Location -> UpgradeState -> Number -> List UpgradeButton
initTowerUpgradeButtons location upgradeState gold =
    let
        upgradesList =
            [ RangeUpgrade, PowerUpgrade ]

        boxList =
            List.repeat (List.length upgradesList)
                (Box.initAt location buttonWidth buttonHeight)
                |> Box.horizontalLayout 50
                |> List.map (Box.shiftY 50)

        initBtnHelp box upgradeType =
            initUpgradeButton box upgradeState upgradeType gold
    in
    List.map2 initBtnHelp boxList upgradesList


isUpgradeApplied : UpgradeType -> UpgradeState -> Bool
isUpgradeApplied upgradeType upgradeState =
    case upgradeState of
        NoUpgrades ->
            False

        OneUpgraded appliedUpgradeType ->
            upgradeType == appliedUpgradeType

        BothUpgraded ->
            True



-- Bullet
-- TODO: Should we rename it to Arrow?


type alias Bullet =
    { --CONFIG
      id : BulletId
    , monsterId : MonsterId
    , target : Location
    , speed : Number

    -- STATE
    , location : Location
    }


type alias BulletInit =
    { monsterId : MonsterId
    , start : Location
    , target : Location
    }


initBullet : Int -> BulletInit -> Bullet
initBullet idx { monsterId, target, start } =
    { id = BulletId idx
    , monsterId = monsterId
    , target = target
    , location = start
    , speed = bulletSpeed
    }


idOfBullet : Bullet -> BulletId
idOfBullet bullet =
    bullet.id


type BulletId
    = BulletId Int



-- TRAVEL PATH


type Path
    = Path Number Location (List Location)


initPath : Location -> List Location -> Path
initPath start rest =
    let
        pathLen =
            List.foldl (\to ( accDistance, from ) -> ( Loc.distanceFromTo from to + accDistance, to ))
                ( 0, start )
                rest
                |> Tuple.first
    in
    Path pathLen start rest


pathToLocations : Path -> List Location
pathToLocations (Path _ s r) =
    s :: r


lengthOfPath : Path -> Number
lengthOfPath (Path l _ _) =
    l


startOfPath : Path -> Location
startOfPath (Path _ s _) =
    s


restOfPath : Path -> List Location
restOfPath (Path _ _ rest) =
    rest



-- PATH BUILDER


type alias PathBuilder =
    { start : Location
    , offset : Number
    , current : Location
    , restReverse : List Location
    }


initPathBuilder : Number -> Location -> PathBuilder
initPathBuilder offset start =
    { start = start
    , offset = offset
    , current = start
    , restReverse = []
    }


goDown : PathBuilder -> PathBuilder
goDown p =
    { p | current = Loc.shiftY -p.offset p.current }


goUp : PathBuilder -> PathBuilder
goUp p =
    { p | current = Loc.shiftY p.offset p.current }


goRight : PathBuilder -> PathBuilder
goRight p =
    { p | current = Loc.shiftX p.offset p.current }


addWayPoint : PathBuilder -> PathBuilder
addWayPoint p =
    { p | restReverse = p.current :: p.restReverse }


buildPath : PathBuilder -> Path
buildPath p =
    initPath p.start (List.reverse p.restReverse)



-- TRAVEL PATH PROGRESS


type PathProgress
    = PathProgress
        { path : Path
        , speed : Number
        , location : Location
        , wayPoints : List Location
        }


initPathProgress : Path -> Number -> PathProgress
initPathProgress path speed =
    PathProgress
        { path = path
        , speed = speed
        , location = startOfPath path
        , wayPoints = restOfPath path
        }


locationOfPathProgress : PathProgress -> Location
locationOfPathProgress (PathProgress { location }) =
    location


stepPathProgress : PathProgress -> Maybe PathProgress
stepPathProgress (PathProgress p) =
    case p.wayPoints of
        [] ->
            Nothing

        wp :: rest ->
            case Loc.stepLocationTowards wp p.speed p.location of
                Nothing ->
                    Just (PathProgress { p | location = wp, wayPoints = rest })

                Just newLocation ->
                    Just (PathProgress { p | location = newLocation })


distanceToPathEnd : PathProgress -> Number
distanceToPathEnd (PathProgress p) =
    initPath p.location p.wayPoints
        |> lengthOfPath



-- Monster


type alias Monster =
    { -- CONFIG
      id : MonsterId
    , maxHealth : Number
    , speed : Number
    , dyingTicks : Number

    -- STATE
    , state : MonsterState
    }


type MonsterState
    = AliveAndKicking { health : Number, travel : PathProgress }
    | JustDied { travel : PathProgress, overKill : Number }
    | DyingAnimation { travel : PathProgress, remainingTicks : Number, overKill : Number }
    | ReachedHouse { health : Number }
    | ReadyForRemoval


initMonster : Int -> Path -> Monster
initMonster idx path =
    let
        maxHealth =
            monsterHealth

        speed =
            monsterSpeed
    in
    { id = MonsterId idx
    , maxHealth = maxHealth
    , speed = speed
    , dyingTicks = 120
    , state =
        AliveAndKicking
            { health = maxHealth
            , travel = initPathProgress path speed
            }
    }


decrementMonsterHealth : Monster -> Monster
decrementMonsterHealth =
    decrementMonsterHealthBy 1


decrementMonsterHealthBy : Number -> Monster -> Monster
decrementMonsterHealthBy damage monster =
    let
        func state =
            case state of
                AliveAndKicking { health, travel } ->
                    let
                        newHealth =
                            health - damage
                    in
                    (if newHealth <= 0 then
                        JustDied
                            { travel = travel
                            , overKill = abs newHealth
                            }

                     else
                        AliveAndKicking { health = newHealth, travel = travel }
                    )
                        |> Just

                JustDied r ->
                    JustDied { r | overKill = r.overKill + 1 }
                        |> Just

                DyingAnimation r ->
                    DyingAnimation { r | overKill = r.overKill + 1 }
                        |> Just

                ReachedHouse _ ->
                    Nothing

                ReadyForRemoval ->
                    Nothing
    in
    case func monster.state of
        Nothing ->
            monster

        Just state ->
            { monster | state = state }


type alias AAKMonster =
    { id : MonsterId
    , location : Location
    , remainingDistance : Number
    }


aakMonsterState : Monster -> Maybe AAKMonster
aakMonsterState monster =
    case monster.state of
        AliveAndKicking { travel } ->
            Just (AAKMonster monster.id (locationOfPathProgress travel) (distanceToPathEnd travel))

        JustDied _ ->
            Nothing

        DyingAnimation _ ->
            Nothing

        ReachedHouse _ ->
            Nothing

        ReadyForRemoval ->
            Nothing


idOfMonster : Monster -> MonsterId
idOfMonster =
    .id


type MonsterId
    = MonsterId Int



-- LAIR


type alias Lair =
    { seed : Seed
    , delay : Number
    , elapsed : Number
    }


lairGenerator : Generator Lair
lairGenerator =
    Random.independentSeed
        |> Random.map
            (\seed ->
                { seed = seed
                , delay = 1
                , elapsed = 0
                }
            )



-- HOUSE


type alias House =
    { maxHealth : Number
    , health : Number
    }


initHouse : House
initHouse =
    let
        maxHealth =
            10
    in
    { maxHealth = maxHealth
    , health = maxHealth
    }


healthOfHouse : House -> Number
healthOfHouse =
    .health


decrementHouseHealth : House -> House
decrementHouseHealth house =
    { house | health = max 0 (house.health - 1) }



-- WORLD


type alias World =
    { lair : Lair
    , path : Path
    , towers : List Tower
    , bullets : List Bullet
    , bombs : List Bomb
    , monsters : List Monster
    , house : House
    , gold : Number
    , selectedTowerId : Maybe TowerId
    , nextIdx : Int
    , seed : Seed
    }


hasHouseBurnedDown : World -> Bool
hasHouseBurnedDown world =
    healthOfHouse world.house == 0



-- GAME


type Game
    = Running World
    | GameOver World


applyNTimes : Int -> (c -> c) -> c -> c
applyNTimes n func val =
    List.foldl (always func) val (List.range 0 n)


init : Game
init =
    let
        path : Path
        path =
            initPathBuilder 50 (Loc.at -250 0)
                |> applyNTimes 2 goRight
                |> addWayPoint
                |> applyNTimes 3 goDown
                |> addWayPoint
                |> applyNTimes 3 goRight
                |> addWayPoint
                |> applyNTimes 3 goUp
                |> addWayPoint
                |> applyNTimes 2 goRight
                |> addWayPoint
                |> buildPath

        ( ig, worldSeed ) =
            Random.step initialGen (Random.initialSeed 0)
    in
    Running
        { lair = ig.lair
        , path = path
        , towers = ig.towers
        , selectedTowerId = ig.towers |> List.head |> Maybe.map idOfTower
        , bullets = []
        , bombs = []
        , monsters = []
        , house = initHouse
        , gold = 200
        , nextIdx = 0
        , seed = worldSeed
        }


type alias InitialWorldData =
    { lair : Lair
    , towers : List Tower
    }


initialGen : Generator InitialWorldData
initialGen =
    let
        initialTowersGenerator : Generator (List Tower)
        initialTowersGenerator =
            [ arrowTowerGenerator (Loc.at -150 -100)
            , arrowTowerGenerator (Loc.at 150 100)
            , bombTowerGenerator (Loc.at 0 0)
            , bombTowerGenerator (Loc.at 150 -100)
            ]
                |> Random.Extra.combine
    in
    Random.map2 InitialWorldData
        lairGenerator
        initialTowersGenerator



-- UPDATE


type Event
    = NoEvent
    | SpawnMonster
    | SpawnArrow BulletInit
    | BulletHitMonster MonsterId
    | MonsterDied
    | RemoveBullet BulletId
    | RemoveMonster MonsterId
    | MonsterReachedHouse
    | BombExploded { at : Location, aoe : Number, damage : Number }
    | RemoveBomb BombId
    | SpawnBomb { from : Location, to : Location }


update : Computer -> Game -> Game
update computer game =
    case game of
        Running world ->
            let
                newWorld =
                    updateWorld computer world
            in
            if hasHouseBurnedDown newWorld then
                GameOver newWorld

            else
                Running newWorld

        GameOver world ->
            GameOver world



-- UPDATE WORLD


updateWorld : Computer -> World -> World
updateWorld computer =
    stepWorldLair
        >> stepWorldHouse
        >> stepWorldBullets
        >> stepWorldMonsters
        >> stepWorldBombs
        >> stepWorldTowers computer
        >> stepWordClick computer


stepWordClick : Computer -> World -> World
stepWordClick computer world =
    let
        { mouse } =
            computer
    in
    if mouse.click then
        case computeClickEventAt (Loc.ofMouse mouse) world of
            Just e ->
                handleWorldClickEvent e world

            Nothing ->
                clearTowerSelection world

    else
        world


handleWorldClickEvent : ClickEvent -> World -> World
handleWorldClickEvent e world =
    case e of
        TowerClicked t ->
            selectTower t world

        SelectedTowerClicked _ ->
            clearTowerSelection world

        UpgradeBtnClicked tower upgradeButton ->
            case upgradeTower upgradeButton.upgradeType tower of
                Just ( cost, upgradedTower ) ->
                    let
                        gold =
                            world.gold - cost
                    in
                    if gold >= 0 then
                        world
                            |> setTower upgradedTower
                            |> setGold gold

                    else
                        world

                Nothing ->
                    world


applyUpgrade : UpgradeType -> UpgradeState -> Maybe ( Number, UpgradeState )
applyUpgrade upgradeType upgradeState =
    case upgradeState of
        NoUpgrades ->
            Just ( firstUpgradeCost, OneUpgraded upgradeType )

        OneUpgraded appliedUT ->
            if upgradeType == appliedUT then
                Nothing

            else
                Just ( secondUpgradeCost, BothUpgraded )

        BothUpgraded ->
            Nothing


findSelectedTower : World -> Maybe Tower
findSelectedTower world =
    world.selectedTowerId
        |> Maybe.andThen (\tid -> List.Extra.find (idOfTower >> is tid) world.towers)


computeClickEventAt : Location -> World -> Maybe ClickEvent
computeClickEventAt location world =
    let
        lazy1 _ =
            findSelectedTower world
                |> Maybe.andThen
                    (\t ->
                        if isLocationOnTowerView location t then
                            Just (SelectedTowerClicked t)

                        else
                            initTowerUpgradeButtons (locationOfTower t) (upgradeOfTower t) world.gold
                                |> List.Extra.find (.box >> Box.contains location)
                                |> Maybe.map (UpgradeBtnClicked t)
                    )

        lazy2 _ =
            List.Extra.find (isLocationOnTowerView location) world.towers
                |> Maybe.map TowerClicked
    in
    Maybe.Extra.orListLazy [ lazy1, lazy2 ]


type ClickEvent
    = SelectedTowerClicked Tower
    | UpgradeBtnClicked Tower UpgradeButton
    | TowerClicked Tower


stepWorldLair : World -> World
stepWorldLair world =
    stepLair world.lair
        |> (\( lair, events ) -> handleEvents events { world | lair = lair })


stepWorldHouse : World -> World
stepWorldHouse world =
    stepHouse world.house
        |> (\( house, events ) -> handleEvents events { world | house = house })


stepWorldBullets : World -> World
stepWorldBullets =
    let
        func bullet world =
            stepBullet bullet
                |> (\( newBullet, events ) -> handleEvents events (setBullet newBullet world))
    in
    \world -> List.foldl func world world.bullets


setBullet : Bullet -> World -> World
setBullet bullet world =
    { world | bullets = List.Extra.setIf (idOfBullet >> is (idOfBullet bullet)) bullet world.bullets }


setTower : Tower -> World -> World
setTower tower world =
    { world | towers = List.Extra.setIf (idOfTower >> is (idOfTower tower)) tower world.towers }


setSelectedTowerId : Maybe TowerId -> World -> World
setSelectedTowerId maybeTowerId world =
    { world | selectedTowerId = maybeTowerId }


selectTowerId : TowerId -> World -> World
selectTowerId towerId =
    setSelectedTowerId (Just towerId)


selectTower : Tower -> World -> World
selectTower tower =
    selectTowerId (idOfTower tower)


clearTowerSelection : World -> World
clearTowerSelection =
    setSelectedTowerId Nothing


setGold : Number -> World -> World
setGold gold world =
    { world | gold = gold }


stepWorldTowers : Computer -> World -> World
stepWorldTowers _ world =
    let
        akaMonstersSortedByRemainingDistance =
            world.monsters
                |> List.filterMap aakMonsterState
                |> List.sortBy .remainingDistance

        stepTowerHelp =
            stepTower akaMonstersSortedByRemainingDistance

        ( selfUpdatedTowers, towerEventGroups ) =
            List.map stepTowerHelp world.towers
                |> List.unzip
    in
    { world | towers = selfUpdatedTowers }
        |> handleEvents (List.concat towerEventGroups)


stepWorldBombs : World -> World
stepWorldBombs =
    let
        func bomb world =
            Bomb.stepBomb
                { remove = RemoveBomb
                , exploded = BombExploded
                }
                bomb
                |> (\( newBomb, events ) -> handleEvents events (setBomb newBomb world))
    in
    \world -> List.foldl func world world.bombs


setBomb : Bomb -> World -> World
setBomb bomb world =
    { world | bombs = List.Extra.setIf (Bomb.id >> is (Bomb.id bomb)) bomb world.bombs }


stepWorldMonsters : World -> World
stepWorldMonsters =
    let
        func monster world =
            stepMonster monster
                |> (\( newMonster, events ) -> handleEvents events (setMonster newMonster world))
    in
    \world -> List.foldl func world world.monsters


setMonster : Monster -> World -> World
setMonster monster world =
    { world | monsters = List.Extra.setIf (idOfMonster >> is (idOfMonster monster)) monster world.monsters }


handleEvents : List Event -> World -> World
handleEvents events acc =
    List.foldl handleEvent acc events


handleEvent : Event -> World -> World
handleEvent event world =
    case event of
        NoEvent ->
            world

        BulletHitMonster monsterId ->
            { world
                | monsters =
                    List.Extra.updateIf (idOfMonster >> is monsterId) decrementMonsterHealth world.monsters
            }

        RemoveBullet bulletId ->
            { world | bullets = List.filter (idOfBullet >> isNot bulletId) world.bullets }

        RemoveMonster monsterId ->
            { world | monsters = List.filter (idOfMonster >> isNot monsterId) world.monsters }

        MonsterReachedHouse ->
            { world | house = decrementHouseHealth world.house }

        SpawnMonster ->
            { world
                | monsters = initMonster world.nextIdx world.path :: world.monsters
                , nextIdx = world.nextIdx + 1
            }

        SpawnArrow bulletInit ->
            { world
                | bullets = initBullet world.nextIdx bulletInit :: world.bullets
                , nextIdx = world.nextIdx + 1
            }

        BombExploded { at, aoe, damage } ->
            let
                isLocationInAOE =
                    Loc.isLocationInRangeOf at aoe

                monsterIdsInBombAOE =
                    world.monsters
                        |> List.filterMap aakMonsterState
                        |> List.filter (.location >> isLocationInAOE)
                        |> List.map .id

                isMonsterInBombAOE monster =
                    List.member (idOfMonster monster) monsterIdsInBombAOE
            in
            { world
                | monsters =
                    List.Extra.updateIf isMonsterInBombAOE (decrementMonsterHealthBy damage) world.monsters
            }

        RemoveBomb bombId ->
            { world | bombs = List.filter (Bomb.id >> isNot bombId) world.bombs }

        SpawnBomb { from, to } ->
            stepWorldSeed
                (Bomb.generator
                    { location = from
                    , target = to
                    , aoe = bombAOE
                    , speed = bombSpeed
                    }
                )
                world
                |> uncurry insertNewBomb

        MonsterDied ->
            { world | gold = world.gold + 1 }


insertNewBomb : Bomb -> World -> World
insertNewBomb bomb world =
    { world | bombs = bomb :: world.bombs }


stepWorldSeed : Generator a -> World -> ( a, World )
stepWorldSeed func world =
    Random.step func world.seed
        |> Tuple.mapSecond (\seed -> { world | seed = seed })



-- WORLD ENTITY STEP FUNCTIONS


stepTower : List AAKMonster -> Tower -> ( Tower, List Event )
stepTower aakMonsters tower =
    if tower.elapsed >= tower.delay then
        let
            inRangeHelp aak =
                isLocationInRangeOfTower aak.location tower
        in
        case
            ( List.filter inRangeHelp aakMonsters, tower.towerType )
        of
            ( [], _ ) ->
                ( tower, [] )

            ( aak :: _, BombTower ) ->
                ( { tower | elapsed = 0 }
                , [ SpawnBomb
                        { from = tower.location
                        , to = aak.location
                        }
                  ]
                )

            ( aakList, ArrowTower ) ->
                let
                    spawnArrowHelp aak =
                        SpawnArrow
                            { monsterId = aak.id
                            , start = tower.location
                            , target = aak.location
                            }
                in
                ( { tower | elapsed = 0 }
                , List.map spawnArrowHelp
                    (aakList
                        |> (if isPowerUpgraded tower then
                                List.take 2

                            else
                                List.take 1
                           )
                    )
                )

    else
        ( { tower | elapsed = tower.elapsed + 1 }, [] )


stepLair : Lair -> ( Lair, List Event )
stepLair lair =
    if lair.elapsed >= lair.delay then
        let
            true =
                10

            false =
                100 - true

            randomBool =
                Random.weighted ( false, False ) [ ( true, True ) ]

            ( bool, seed ) =
                Random.step randomBool lair.seed
        in
        ( { lair | elapsed = 0, seed = seed }
        , if bool then
            [ SpawnMonster ]

          else
            []
        )

    else
        ( { lair | elapsed = lair.elapsed + 1 }, [] )


stepHouse : House -> ( House, List Event )
stepHouse house =
    ( house, [] )


stepMonster : Monster -> ( Monster, List Event )
stepMonster monster =
    let
        func state =
            case state of
                AliveAndKicking { health, travel } ->
                    case stepPathProgress travel of
                        Just nt ->
                            ( AliveAndKicking { health = health, travel = nt }, [] )

                        Nothing ->
                            ( ReachedHouse { health = health }, [ MonsterReachedHouse ] )

                JustDied r ->
                    ( DyingAnimation
                        { travel = r.travel
                        , remainingTicks = monster.dyingTicks
                        , overKill = r.overKill
                        }
                    , [ MonsterDied ]
                    )

                DyingAnimation { travel, remainingTicks, overKill } ->
                    if remainingTicks <= 0 then
                        ( ReadyForRemoval, [ RemoveMonster monster.id ] )

                    else
                        ( DyingAnimation
                            { travel = travel
                            , remainingTicks = max 0 (remainingTicks - 1)
                            , overKill = overKill
                            }
                        , []
                        )

                ReachedHouse _ ->
                    ( ReadyForRemoval, [ RemoveMonster monster.id ] )

                ReadyForRemoval ->
                    ( ReadyForRemoval, [] )
    in
    func monster.state
        |> Tuple.mapFirst (\state -> { monster | state = state })


stepBullet : Bullet -> ( Bullet, List Event )
stepBullet bullet =
    case Loc.stepLocationTowards bullet.target bullet.speed bullet.location of
        Nothing ->
            ( bullet, [ RemoveBullet bullet.id, BulletHitMonster bullet.monsterId ] )

        Just newLocation ->
            ( { bullet | location = newLocation }, [] )



-- View


view : Computer -> Game -> List Shape
view computer game =
    case game of
        Running world ->
            [ words blue "Game Running"
                |> scale 3
                |> moveY computer.screen.top
                |> moveDown 50
            , viewWorldStats computer world |> moveDown 50
            , viewWorld computer world
            ]

        GameOver world ->
            [ [ viewWorldStats computer world |> moveDown 50
              , viewWorld computer world
              ]
                |> group
                |> fade 0.5
            , words red "GAME OVER" |> scale 3
            ]


viewWorldStats : Computer -> World -> Shape
viewWorldStats computer world =
    [ [ words black ("House Health: " ++ fromInt (round (healthOfHouse world.house)))
            |> scale 2
      , words black ("Monster Count: " ++ fromInt (List.length world.monsters))
            |> scale 2
      , words black ("Tower Count: " ++ fromInt (List.length world.towers))
            |> scale 2
      , words black ("Bullet Count: " ++ fromInt (List.length world.bullets))
            |> scale 2
      ]
        |> List.indexedMap (toFloat >> (\idx -> moveDown (idx * 50)))
        |> group
        |> moveY computer.screen.top
        |> moveDown 50
    ]
        |> group


viewWorld : Computer -> World -> Shape
viewWorld computer world =
    let
        { screen } =
            computer

        viewTowerHelp tower =
            viewTower (world.selectedTowerId == Just (idOfTower tower)) tower
    in
    [ List.map viewTowerHelp world.towers |> group
    , viewPath world.path
    , List.map viewMonster world.monsters |> group
    , List.map Bomb.viewBomb world.bombs |> group
    , List.map viewBullet world.bullets |> group
    , case List.Extra.find (idOfTower >> Just >> is world.selectedTowerId) world.towers of
        Just t ->
            initTowerUpgradeButtons (locationOfTower t) (upgradeOfTower t) world.gold
                |> List.map viewUpgradeButton
                |> group

        Nothing ->
            noShape
    , [ words black ("Gold: " ++ (round world.gold |> fromInt))
      ]
        |> List.indexedMap (\i -> moveDown (toFloat i * 20))
        |> group
        |> scale 1.5
        |> moveY screen.top
        |> moveX screen.left
        |> moveRight 100
        |> moveDown 50
    ]
        |> group


viewPath : Path -> Shape
viewPath path =
    let
        ep location =
            circle lightCharcoal 8
                |> Loc.moveShape location
    in
    pathToLocations path
        |> List.map ep
        |> group


viewMonster : Monster -> Shape
viewMonster monster =
    let
        monsterShape =
            --[ circle red 20 |> fade 0.9 ] |> group
            [ rectangle purple 20 30
            , circle charcoal 10 |> moveUp 22.5
            , -- legs
              rectangle charcoal 7.5 20 |> moveLeft 5 |> moveDown 25
            , rectangle charcoal 7.5 20 |> moveRight 5 |> moveDown 25
            , rectangle purple 22.5 15 |> moveDown 10
            ]
                |> group

        healthBarShape pct =
            let
                width =
                    40

                healthWidth =
                    width * pct

                barHeight =
                    10
            in
            [ rectangle red width barHeight
            , rectangle green healthWidth barHeight
                |> moveLeft ((width - healthWidth) / 2)
            ]
                |> group

        viewHealthBar health =
            healthBarShape (health / monster.maxHealth)

        placeShape : PathProgress -> Shape -> Shape
        placeShape travel =
            scale 0.7 >> Loc.moveShape (locationOfPathProgress travel)
    in
    case monster.state of
        AliveAndKicking { travel, health } ->
            [ monsterShape
            , viewHealthBar health |> moveUp 40
            , debugShape <|
                \_ -> words white (fromInt (round health))
            ]
                |> group
                |> placeShape travel

        JustDied r ->
            [ monsterShape
            , debugShape <|
                \_ -> words white (fromInt (round r.overKill))
            ]
                |> group
                |> placeShape r.travel

        DyingAnimation { travel, remainingTicks, overKill } ->
            let
                remainingProgress =
                    remainingTicks / monster.dyingTicks
            in
            [ monsterShape |> fade (remainingProgress / 2)
            , debugShape <|
                \_ -> words white (fromInt (round overKill))
            ]
                |> group
                |> placeShape travel

        ReachedHouse _ ->
            group []

        ReadyForRemoval ->
            group []


viewTower : Bool -> Tower -> Shape
viewTower isSelected tower =
    let
        ( lightC, darkC ) =
            case tower.towerType of
                ArrowTower ->
                    ( lightBlue, blue )

                BombTower ->
                    ( lightBrown, brown )
    in
    [ [ circle lightC (rangeOfTower tower) |> fade 0.3
      ]
        |> group
        |> fade
            (if isSelected then
                1

             else
                0
            )
    , square darkC tower.viewWidth
    ]
        |> group
        |> Loc.moveShape tower.location


viewBullet : Bullet -> Shape
viewBullet bullet =
    circle blue 5
        |> Loc.moveShape bullet.location


noShape : Shape
noShape =
    group []


debugShape : (() -> Shape) -> Shape
debugShape func =
    if isDebug then
        func ()

    else
        noShape



-- EXTRA


is =
    (==)


isNot =
    (/=)


mul =
    (*)


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry func ( a, b ) =
    func a b


mapRandomIdInt : (Int -> b) -> Generator b
mapRandomIdInt func =
    Random.int 0 Random.maxInt
        |> Random.map func



-- MAIN


main =
    game view update init
