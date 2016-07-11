{-
 The colored ball example, example 1.1 from

 http://people.csail.mit.edu/milch/papers/blog-chapter.pdf

``An urn contains an unknown number of balls--say, a number chosen from
a Poisson or a uniform distributions. Balls are equally likely to be blue or
green. We draw some balls from the urn, observing the color of each
and replacing it. We cannot tell two identically colored balls apart;
furthermore, observed colors are wrong with probability 0.2.  How many
balls are in the urn? Was the same ball drawn twice?''

-}

module ColoredBalls where

import Syntax
import Util
import qualified Data.IntMap as M
import Data.IntMap ((!))
import Control.Applicative

import qualified Debug.Trace as TR


-- ---------------  The colored-ball model

data Color = Blue | Green deriving (Eq, Show)

opposite_color :: Color -> Color
opposite_color Blue  = Green
opposite_color Green = Blue

-- Distribution for the observing of a ball, with the failure rate 20%
observed_color color =
  categorical [(color,0.8),(opposite_color color,0.2)]

maxBalls :: Int
maxBalls = 8

-- Create the prior for the max amount of balls that may be in the urn
balls_prior n = do
  balls <- sequence . replicate n $ dist uniformly (pure [Blue,Green])
  return $ M.fromList $ zip [1..] balls

{-
Problem with the following model: nballs and b below are strongly correlated.
Hence if nballs is drawn as 5 and b is drawn as, say, 3, all the
proposals to resample nbalss as 1 or 2 will be rejected.

-- The model takes an array of observations as an argument
cballs_model :: [Color] -> Model Int
cballs_model obs = do
  balls  <- balls_prior maxBalls
  nballs <- dist uniformly (pure [1..maxBalls])
  -- The same ball can be drawn twice as we return the drawn balls
  -- into the urn. A ball keeps its true color, no matter how many
  -- times it is drawn.
  let observe_ball color = do
        b <- dist uniformly ((\n -> [1..n]) <$> nballs)
        -- It is more efficient to create if statements, rather than
        -- create an SExp(IntMap Color).
        let observe i =
              let true_color = balls ! i in
              if_ ((== i) <$> b) $
                dist (color `condition` categorical)
                     ((\bc -> [(bc,0.8),(opposite_color bc,0.2)]) <$>
                      true_color)
        foldr observe (return (pure color)) [1..maxBalls]
  mapM_ observe_ball obs
  return nballs
-}

-- Submodel: Given balls color and the observations, perform conditioning
balls_observe :: M.IntMap (SExp Color) -> [Color] -> Int -> Model ()
balls_observe balls obs nballs = mapM_ observe_ball obs >> return (pure ())
 where
  -- The same ball can be drawn twice as we return the drawn balls
  -- into the urn. A ball keeps its true color, no matter how many
  -- times it is drawn.
   observe_ball color = do
        b <- dist uniformly (pure [1..nballs])
        -- It is more efficient to create if statements, rather than
        -- create an SExp(IntMap Color).
        let observe i =
              let true_color = balls ! i in
              if_ ((== i) <$> b) $
                dist (color `condition` observed_color) true_color
        foldr observe (return (pure color)) [1..maxBalls]

-- The model takes a list of observations as an argument
cballs_model :: [Color] -> Model Int
cballs_model obs = do
  balls  <- balls_prior maxBalls
  nballs <- dist uniformly (pure [1..maxBalls])
  let obs_number i =
        if_ ((== i) <$> nballs) $ balls_observe balls obs i
  foldr obs_number (return (pure())) [1..maxBalls]
  return nballs

cbr = hist $ mcmC 10000 (cballs_model (replicate 10 Blue))

-- fromList [(1,4265),(2,2203),(3,1244),(4,805),(5,486),(6,291),(7,565),(8,141)]
-- Good agreement

{-
   Sec 1.6.3, Fig 1.7.
   Ten balls were drawn, and all appeared blue

let t1 = sample_importance (random_selector 17) 10000 (model_nballs 
		 [|Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;|]);;

let
    [(0.031942788306915014, V 8); (0.0422088248548995876, V 7);
     (0.0481489259210673634, V 6); (0.0611501366775565255, V 5);
     (0.0843776503618006435, V 4); (0.123159075103266638, V 3);
     (0.203898291338526577, V 2); (0.405114307435967658, V 1)] =
 snd (Inference.normalize t1);;

(* Do a more serious run, accumulating the statistics *)
let dist_add l1 l2 =
  List.map2 (fun (p1,v1) (p2,v2) ->
    assert (v1 = v2); (p1 +. p2, v1)) l1 l2;;

let average_it ntrials nsamples model =
  let rec loop acc = function 0 -> acc
    | n -> loop (dist_add acc (sample_importance 
				 (random_selector (17+n)) nsamples model))
	        (pred n)
  in snd (Inference.normalize 
	    (loop (sample_importance (random_selector (17+ntrials)) 
		     nsamples model) 
	       (pred ntrials)))
;;

(* It took about a minute *)

(*
let t2 = average_it 5 5000 (model_nballs 
		 [|Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;|]);;

let 
  [(0.0318386565368576138, V 8); (0.0384083436381660176, V 7);
   (0.0445344709122082669, V 6); (0.055953476873295431, V 5);
   (0.0777907061915841, V 4); (0.123751456856916756, V 3);
   (0.211354336837841883, V 2); (0.416368552153129956, V 1)]
 = t2;;
*)

(* The results seem to be in an excellent agreement with Fig 1.7a;
   It seems, 5,000 samples are sufficient
*)

(* We now try the second query: Was the same ball drawn twice? *)

let model_duplicate obs () =
  let nballs = nballs_prior () in
  (* The same ball can be drawn twice as we return the drawn balls
     into the urn. A ball keeps its true color, no matter how many
     times it is drawn.
   *)
  let ball_color = memo (fun b -> uniformly [|Blue; Green|]) in
  (* perform observations *)
  let drawn = Array.fold_left (fun acc obs_color -> 
    let b = uniform_range 1 nballs in
    if observed_color (ball_color b) = obs_color 
    then PMap.insert_with (+) b 1 acc else fail ()) 
    PMap.empty obs
  in PMap.fold (fun x y -> min x y) drawn (1 + (Array.length obs))
;;


let td = sample_importance (random_selector 17) 20000 (model_duplicate
		 [|Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;Blue;|]);;

let   [(0.00673060540800008485, V 10); (0.000852212937600005, V 5);
       (0.00140560366560001852, V 4); (0.00132793777440001672, V 3);
       (0.00193966835520000447, V 2); (0.00410314812000006201, V 1)] = td;;

let [(0.411426914210096362, V 10); (0.0520938783233282424, V 5);
     (0.0859214206871847103, V 4); (0.0811738777814881435, V 3);
     (0.118567605378018456, V 2); (0.250816303619884162, V 1)] =
  snd (Inference.normalize td);;

(* There is 25% chance some ball was drawn only once *)


-}
