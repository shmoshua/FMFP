
type Graph = ([Node], [Edge])
type Node = Int
type Edge = (Int, Int)

singletonGraph :: Graph
singletonGraph = ([1],[])

exists :: Graph -> Node -> Bool
exists (v,e) u = u `elem` v

addNode :: Graph -> Node -> Graph
addNode (v,e) u = if exists (v,e) u then (v,e) else (u:v, e)
-- addNode g@(v,e) u = if exists g u then g else (u:v, e)

addEdge :: Graph -> Edge -> Graph
addEdge g (u,w) = (v,(u,w):e)
    where (v,e) = addNode (addNode g u) w

returnN :: Graph -> Int
returnN (u,v) = length u

returnM :: Graph -> Int
returnM (u,v) = length v

completeGraph :: Int -> Graph
completeGraph n = ([1..n],[(x,y)|x <- [1..(n-1)],y <- [(x+1)..n]])