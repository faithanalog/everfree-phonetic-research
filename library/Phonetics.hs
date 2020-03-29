module Phonetics (main) where

import Text.EditDistance
import Data.List
import Data.Maybe
import Data.Clustering.Hierarchical
import Control.Monad

main :: IO ()
main = do
  input <- getContents


  let phoneMap = makePhoneMap input
      (expand, condense) = phoneCondenser (map fst phoneMap)
      condensedMap = map condense phoneMap
      gram = dendrogram CompleteLinkage condensedMap distance
      (Branch topDistance _ _ ) = gram
      groups = map (map expand . elements) (cutAt gram 1)

  forM_ (zip [1..] groups) $ \(n, cluster) -> do
    putStrLn ("== Cluster " ++ show n ++ " ==")
    let allWords = nub (map snd cluster)
    forM_ allWords putStrLn
    putStrLn ""
    putStrLn ""


type PhonePair = ([String], String)
type CondensedPair = (String, String)
type PhoneMap = [PhonePair]
type Condenser = PhonePair -> CondensedPair
type Expander = CondensedPair -> PhonePair

phoneCondenser :: [[String]] -> (Expander, Condenser)
phoneCondenser phones =
  let uniquePhones = nub (concat phones)
      chars = ['0' ..]
      condenserMap = zip uniquePhones chars
      expanderMap = zip chars uniquePhones
      condensePhone phone = fromJust (lookup phone condenserMap)
      expandPhone phone = fromJust (lookup phone expanderMap)
      condense (phones, word) = (map condensePhone phones, word)
      expand (phones, word) = (map expandPhone phones, word)
  in (expand, condense)

phoneMapLine :: String -> PhonePair
phoneMapLine str =
  let (word:phones) = words str
   in (phones, takeWhile (/= '(') word)

makePhoneMap :: String -> PhoneMap
makePhoneMap string = map phoneMapLine (lines string)

myCosts ::  EditCosts
myCosts = EditCosts (ConstantCost 5) (ConstantCost 5) (ConstantCost 1) (ConstantCost 10)

distance :: CondensedPair -> CondensedPair -> Double
distance (phone1, _) (phone2, _) = fromIntegral (levenshteinDistance myCosts phone1 phone2)
