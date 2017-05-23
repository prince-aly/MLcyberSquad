module ANN.Simple
    ( NeuralNetwork ()
    , makeNetwork
    , newRandomNetwork
    , evaluateNetwork
    , sigmoid
    , relu
    ) where

--------------------------------------------------------------------------------

import System.Random (newStdGen, randomRs, split)
import Data.List (mapAccumL)

--------------------------------------------------------------------------------
-- DATATYPE

-- | an inductive neural network representation where a row of neurons is paired
-- with the axons coming in to the neurons which then are evaluated and passed
-- as inputs to the next row, finally ending with the output row. When
-- evaluating, one can think of the input row being the list passed to the
-- evaluation function. The creation does not check the validity of the network
-- and as such if improperly constructed, it will have some nodes and axons that
-- are not used.
-- The length of the neurons and axons outer lists should be the same.
data NeuralNetwork
    = NeuralRow [Neuron] [[Axon]] NeuralNetwork -- ^ An intermediate row of neurons with the axons that lead into it, followed by the next row of the neural network
    | NeuralOutputRow [Neuron] [[Axon]] -- ^ The root case for the row of neurons. Note that it has no next row

-- | A wrapper for a transformation function
type Neuron = Double -> Double

-- | A wrapper for a weight value
type Axon = Double

-- GENERATION

-- | makes a neural network by filling in the neural rows in the order specified by the matrices given
makeNetwork :: [[Neuron]] -> [[[Axon]]] -> NeuralNetwork
makeNetwork [n] [a] = NeuralOutputRow n a
makeNetwork (n:ns) (a:as) = NeuralRow n a (makeNetwork ns as)

-- | generates a network filled with random values where the list supplied specifies
-- the number of neurons in each row for as many rows as that list is long
newRandomNetwork :: [Int] -> IO NeuralNetwork
newRandomNetwork neurons = do
    gen <- newStdGen
    let ns = [replicate n sigmoid | n <- neurons]
    let as = [snd $ mapAccumL (\g b -> (snd (split g), randomRs (-1, 1) g)) gen (replicate n [0]) | n <- neurons] -- build a list of axon weights so that it matches the neurons --NOTE: currently built as an infinite list, which with lazy evaluation is fine
    return $ makeNetwork ns as

--------------------------------------------------------------------------------
-- EVALUATION 

-- | evaluates a neuron by multiplying its inputs by their weights and applying
-- the neuron's transformation function to their sum
evaluateNeuron :: [Double] -> [Double] -> Neuron -> Double
evaluateNeuron inputs weights f = f $ sum (zipWith (*) inputs weights)

-- | evaluates each neuron in a row and returns their values as output
evaluateNeuralRow :: [Double] -> [Neuron] -> [[Axon]] -> [Double]
evaluateNeuralRow inputs neurons axons = zipWith (\n as -> evaluateNeuron inputs as n) neurons axons

-- | evaluates a network by recursively evaluating the row and passing its result
-- to the next row and returning the output row's results
evaluateNetwork :: [Double] -> NeuralNetwork -> [Double]
evaluateNetwork inputs (NeuralOutputRow neurons axons) = evaluateNeuralRow inputs neurons axons
evaluateNetwork inputs (NeuralRow neurons axons nextRow) = evaluateNetwork (evaluateNeuralRow inputs neurons axons) nextRow

--------------------------------------------------------------------------------
-- NEURON EVALUATION FUNCTIONS

-- | the sigmoid squashing function, where all numbers are reduced to the range (-1, 1).
-- In this implementation it's tanh.
sigmoid :: Double -> Double
sigmoid x = tanh x

-- | rectified linear units, where all numbers which are negative are zeroed
relu :: Double -> Double
relu x = if x < 0 then 0 else x
