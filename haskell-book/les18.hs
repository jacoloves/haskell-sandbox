boxMap :: (a -> b) -> Box a -> Box b
boxMap func (Box val) = Box (func val)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple v1 v2 v3) = Triple (func v1) (func v2) (func v3)

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where
    countOrgan = (\organ -> (length . filter (== organ)) values)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)