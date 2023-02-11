let ItemCount = { count : Natural, item : Text }

let mkIc = \(i : Text) -> \(c : Natural) -> { item = i, count = c }

let Account =
      { chessPieceEqualizer : Natural
      , dailyRoll : Natural
      , etherealShine : Natural
      , gambitShop : List ItemCount
      , inventory : List ItemCount
      , loafConverter : Natural
      , moakBooster : Natural
      , prestigeLevel : Natural
      , recipeRefinement : Bool
      }

let testAccount
    : Account
    = { loafConverter = 8
      , dailyRoll = 318
      , recipeRefinement = False
      , moakBooster = 1
      , chessPieceEqualizer = 1
      , etherealShine = 1
      , inventory = [ mkIc "shadow_gem_gold" 20 ]
      , prestigeLevel = 2
      , gambitShop =
        [ mkIc "flatbread" 2
        , mkIc "stuffed_flatbread" 2
        , mkIc "sandwich" 2
        , mkIc "french_bread" 2
        ]
      }

in  { ItemCount = ItemCount, Account = Account, mkIc = mkIc }
