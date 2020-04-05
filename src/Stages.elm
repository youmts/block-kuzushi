module Stages exposing (makeStage, Vector, Block)

type alias Vector = 
  { x : Float
  , y : Float
  }

type alias Block = 
  {
    position : Vector
  , width : Float
  , height : Float
  , color : String
  }

makeStage : Int -> Float -> Float -> List Block
makeStage stageNumber width height =
  let
    toBlocks = \a -> a|> List.indexedMap (makeRow width height) |> List.concat
  in
  case stageNumber of
     0 -> stage0 |> toBlocks
     1 -> stage1 |> toBlocks
     2 -> stage2 |> toBlocks
     3 -> stage3 |> toBlocks
     _ -> []

makeRow : Float -> Float -> Int -> List Int -> List Block
makeRow width height yIndex rowList =
  rowList
    |> List.indexedMap (makeBlock width height yIndex)
    |> List.filterMap (\x -> x)

makeBlock : Float -> Float -> Int -> Int -> Int -> Maybe Block
makeBlock width height yIndex xIndex blockNumber =
  case blockNumber of
    0 -> Nothing
    number -> 
      Just
      ( Block
        (Vector (toFloat xIndex * width) (toFloat yIndex * height))
        width
        height
        (toColor number)
      )

toColor : Int -> String
toColor blockNumber =
  case blockNumber of
    1 -> "white"
    2 -> "red"
    3 -> "green"
    4 -> "blue"
    5 -> "yellow"
    6 -> "cyan"
    7 -> "pink"
    8 -> "purple"
    9 -> "orange"
    _ -> "gray"

stage0 : List (List Int)
stage0 =
  [ [0,0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0,0]
  , [0,1,1,1,1,1,1,1,1,0]
  , [0,1,1,1,1,1,1,1,1,0]
  , [0,1,1,1,1,1,1,1,1,0]
  , [0,1,1,1,1,1,1,1,1,0]
  , [0,1,1,1,1,1,1,1,1,0]
  , [0,1,1,1,1,1,1,1,1,0]
  ]
stage1 : List (List Int)
stage1 =
  [ [0,0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0,0]
  , [0,1,1,0,0,0,0,0,0,0]
  , [0,2,2,2,0,0,0,0,0,0]
  , [0,3,3,3,3,0,0,0,0,0]
  , [0,4,4,4,4,4,0,0,0,0]
  , [0,5,5,5,5,5,5,0,0,0]
  , [0,6,6,6,6,6,6,6,0,0]
  , [0,7,7,7,7,7,7,7,7,0]
  ]

stage2 : List (List Int)
stage2 =
  [ [4,4,4,4,4,4,4,4,4,4]
  , [0,0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0,0]
  , [0,2,2,2,2,2,2,2,2,0]
  , [0,2,2,2,2,2,2,2,2,0]
  , [0,3,3,3,3,3,3,3,3,0]
  , [0,3,3,3,3,3,3,3,3,0]
  , [0,7,7,7,7,7,7,7,7,0]
  , [0,7,7,7,7,7,7,7,7,0]
  ]

stage3 : List (List Int)
stage3 =
  [ [0,0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0,0]
  , [0,3,3,3,3,3,3,3,3,0]
  , [0,7,7,7,7,7,7,7,7,0]
  , [0,4,4,4,4,4,4,4,4,0]
  , [0,8,8,8,8,8,8,8,8,0]
  , [0,2,2,2,2,2,2,2,2,0]
  , [0,9,9,9,9,9,9,9,9,0]
  ]