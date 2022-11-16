import System.IO  

type Film = Int
type Actor = Int
type Act = (Actor, Actor, Film)

type Database = [Act]


acted_together :: Database -> Actor -> Actor -> Bool
acted_together db a1 a2 = or [(x == a1 && y == a2) || (x == a2 && y == a1) | (x,y,z) <- db]

actors_with_bacon_number :: Database -> Int -> [Actor]
actors_with_bacon_number db n = undefined

