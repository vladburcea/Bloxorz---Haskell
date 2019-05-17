{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState

import Data.Maybe
import qualified Data.List as L

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Nil |  Node {
    resulted :: Maybe a,
    nodeState :: s,
    parent :: Node s a,
    depth :: Int,
    children :: [Node s a]} deriving Show

{-
    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

makeListOfChildren :: [Node s a] -> Node s a -> [Node s a]
makeListOfChildren list el = [el] ++ list

generateKids :: (ProblemState s a) => [(a, s)] -> Node s a -> [Node s a]
generateKids [] _ = []
generateKids succs parent_node = makeListOfChildren (generateKids (tail succs) parent_node) current_node
    where current_node = Node { depth = (depth parent_node) + 1,
                        nodeState = snd $ head succs,
                        resulted = Just (fst $ head succs),
                        parent = parent_node,
                        children = generateKids (successors (snd (head succs))) current_node }

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace st = root_node
    where root_node = Node {resulted = Nothing, parent = Nil, depth = 0, nodeState = st, children = generateKids (successors st) root_node}

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace = undefined


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

limitedDFS :: (ProblemState s a, Ord s) =>
              [Node s a] -> S.Set s -> Node s a -> Int -> [Node s a]
limitedDFS _ _ Nil _ = []
limitedDFS nodes vis (Node _ _ p d succNodes) maxDepth
  | d == 0 && null notVis        = nodes
  | d == maxDepth || null notVis = limitedDFS nodes vis p maxDepth
  | otherwise                        = limitedDFS newNodes newVis next maxDepth
    where
      newNodes = nodes ++ [next]
      newVis   = S.insert (nodeState next) vis
      notVis   = filter (\n -> not $ S.member (nodeState n) vis) succNodes
      next     = head notVis

limitedDfs :: (ProblemState s a, Ord s) => Node s a -> Int -> [Node s a]
limitedDfs root maxDepth = root : (limitedDFS [] vis root maxDepth)
  where
    vis = S.insert rootState S.empty
    rootState = nodeState root

    -- resulted :: Maybe a,
    -- nodeState :: s,
    -- parent :: Node s a,
    -- depth :: Int,
    -- children :: [Node s a]} deriving Show

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}  
checkIfIsGoal :: (ProblemState s a, Ord s, Eq s)
    => Node s a -> Int -> [Node s a] -> Int -> (Node s a, Int)
checkIfIsGoal node count [] max_depth = checkIfIsGoal node count (limitedDfs node (max_depth + 1)) (max_depth + 1)
checkIfIsGoal node count node_list max_depth = if (isGoal (nodeState (head node_list)) == True)
                                            then (head node_list, count)
                                            else checkIfIsGoal node (count + 1) (tail node_list) max_depth

-- -- iterativeDeepening :: (ProblemState s a, Ord s)
--     => Node s a         -- Nodul stării inițiale
--     -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
-- iterativeDeepening node = checkIfIsGoal node 0 (limitedDfs node 0) 0
iterativeDeepening :: (ProblemState s a, Ord s) => Node s a -> (Node s a, Int)
iterativeDeepening root = (goalNode, numNodes + nodesToGoal)
  where
    allDFSs     = map (limitedDfs root) [0..] 
    nodes       = head $ filter (any (isGoal . nodeState)) allDFSs
    nodesToGoal = length $ takeWhile (not . isGoal . nodeState) nodes
    noGoalLists = takeWhile (not . any (isGoal . nodeState)) allDFSs
    numNodes    = length $ concat noGoalLists
    goalNode    = fromJust (L.find (isGoal . nodeState) nodes)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath (Node _ _ Nil _ _ ) = []
extractPath node = (extractPath (parent node)) ++ [(fromJust $ resulted node, nodeState node)]

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve stt _ = extractPath $ fst (iterativeDeepening (createStateSpace stt))

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))