module Graph where

type V = Int
data Graph = Graph [(V, [V])]

g1 = Graph [(1,[2,3]),
            (2, [1,4]),
            (3, [1,4]),
            (4, [2,3,5]),
            (5, [4])]

vert :: Graph -> [V]
vert (Graph l) = map fst l

edges :: Graph -> [(V,V)]
edges (Graph l) = takeRep $ 
  
neighbors :: V -> Graph -> [V]
neighbors v (Graph l) = head [snd e | e <- l, fst e==v] 

dfs :: V -> Graph -> [V]
dfs v g = walkthrough [v] [] where
  walkthrough [] vis = vis
  walkthrough (v:vs) vis | elem v vis = walkthrough vs vis
                         | otherwise = walkthrough (neighbors v g ++ vs) (vis ++[v])

bfs :: V -> Graph -> [V]
bfs v g = walkthrough [v] [] where
  walkthrough [] vis = vis
  walkthrough (v:vs) vis | elem v vis = walkthrough vs vis
                         | otherwise = walkthrough (vs ++ neighbors v g) (vis ++[v])

isConnected :: Graph -> Bool
isConnected g@(Graph l) = (length $ vert g) == (length $ dfs(fst $ head l) g)
