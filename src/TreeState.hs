{-# LANGUAGE TupleSections #-}

module TreeState where

import Control.Lens
import Control.Monad.State
import Criterion.Main
import Data.Tree

tag :: Tree a -> Tree (a, Int)
tag tree = evalState (tagStep tree) 0
  where
    tagStep :: Tree a -> State Int (Tree (a, Int))
    tagStep (Node a subtrees) = do
      counter <- get
      put (counter + 1)
      subtrees' <- mapM tagStep subtrees
      return $ Node (a, counter) subtrees'

tag' :: Tree a -> Tree (a, Int)
tag' tree = evalState (tagStep tree) 0
  where
    tagStep :: Tree a -> State Int (Tree (a, Int))
    tagStep = mapTreeM step
      where
        step :: a -> State Int (a, Int)
        step a = do
          counter <- postIncrement
          return (a, counter)

    mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
    mapTreeM action (Node a subtrees) = do
     a' <- action a
     subtrees' <- mapM (mapTreeM action) subtrees
     return $ Node a' subtrees'

postIncrement :: Enum s => State s s
postIncrement = do
  result <- get
  modify succ
  return result

tag'' :: Traversable t => t a -> t (a, Int)
tag'' t = evalState (traverse step t) 0
  where
    step a = do
      tag <- postIncrement
      return (a, tag)

lensTag :: Traversable t => t a -> t (a, Int)
lensTag t = evalState (traverse step t) 0
  where
    step a = (a,) <$> (id <<+= 1)

lensTag' :: Tree a -> Tree (a, Int)
lensTag' = unsafePartsOf each %~ flip zip [0..]

lensTag'' :: Tree a -> Tree (Int, a)
lensTag'' = unsafePartsOf each %~ zip [0..]

tree_ :: Tree Char
tree_ = Node 'd'
  [ Node 'b'
    [ Node 'a' []
    , Node 'c' []
    ]
  , Node 'f'
    [ Node 'e' []
    , Node 'g' []
    ]
  ]

allSame :: Bool
allSame = all . (==) . head <*> tail $ map ($ tree_) funcs
  where
    funcs =
      [ tag
      , tag
      , tag''
      , lensTag
      , lensTag'
      ]

main :: IO ()
main = defaultMain
  [ bgroup "taggers"
    [ bench "tag"      $ nf tag      tree_
    , bench "tag'"     $ nf tag'     tree_
    , bench "tag''"    $ nf tag''    tree_
    , bench "lensTag"  $ nf lensTag  tree_
    , bench "lensTag'" $ nf lensTag' tree_
    ]
  ]
