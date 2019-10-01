module Main where

import Prelude

import Control.Apply (lift2)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Int (odd, toNumber)
import Data.List.Lazy (List, fromFoldable, (..), length)
import Data.Map as M
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomBool)
import Effect.Ref as R
import Effect.Timer as T
import Graphics.Canvas as C
import Partial.Unsafe (unsafePartial)
import Web.HTML (window)

type CellState = { alive :: Boolean }
type Cell = Tuple Int Int

type Step = M.Map Cell CellState

type Grid = { width :: Int , height :: Int }

draw :: C.Context2D -> Number -> (Tuple Cell CellState) -> Effect Unit
draw ctx boxSize (Tuple (Tuple w h) {alive}) = do
  let getSized = (*) boxSize <<< toNumber
  let sizedX = getSized w
  let sizedY = getSized h
  let color = if alive then "#1f1f21" else "#d1eefc"
  C.beginPath ctx
  C.rect ctx { height: boxSize 
  , width: boxSize
  , x: sizedX
  , y: sizedY }
  C.setFillStyle ctx color
  C.fill ctx

firstStep :: Grid -> Effect (List (Tuple Cell CellState))
firstStep {width, height} = let 
  grid = lift2 Tuple (1..width) (1..height) 
  addState = 
    \t@(Tuple row col) -> 
      (\b -> Tuple t {alive: odd col && b}) <$> randomBool
  in traverse (addState) grid

nextStep :: Grid -> Step -> List (Tuple Cell CellState)
nextStep {width, height} all = do
  let isAlive = _.alive <<< unsafePartial fromJust <<< flip M.lookup all
      getExistsNeighboringCell (Tuple row col) = do
        r <- (row - 1) .. (row + 1)
        c <- (col - 1) .. (col + 1)
        let t = Tuple r c
        guard $ M.member t all
        pure $ t
  key <- fromFoldable <<< keys $ all
  guard $ isAlive key
  t <- getExistsNeighboringCell key
  let nAliveC = length $ do
        nT <- getExistsNeighboringCell t
        guard $ t /= nT && isAlive nT
        pure$ nT
      isCurrentAlive = isAlive t
      alive = case isCurrentAlive of
        true -> nAliveC `Array.elem` [2,3]
        false -> nAliveC == 3
  pure $ Tuple t {alive}

main :: Effect Unit
main = void $ unsafePartial do
  let grid = {width: 30, height: 30}
  cells <- firstStep grid
  Just canvas <- C.getCanvasElementById "canvas"
  ctx <- C.getContext2D canvas
  boxSize <- flip (/) 30.0 <$> C.getCanvasHeight canvas
  C.setLineWidth ctx 2.0
  C.setStrokeStyle ctx "black"
  traverse_ (draw ctx boxSize) cells
  w <- window
  mC <- R.new (cells)
  -- let fn step = void $ (
  --     do 
  --       let result = nextStep grid $ Map.fromFoldable step
  --       traverse_ (draw ctx boxSize) result
  --       -- requestAnimationFrame (fn result) w
  -- )
  -- requestAnimationFrame (fn cells) w
  void $ T.setInterval 16 do
    log "here"
    v <- R.read mC
    let result = nextStep grid $ M.fromFoldable v
    traverse_ (draw ctx boxSize) result
    void $ R.modify (\step -> result) mC
