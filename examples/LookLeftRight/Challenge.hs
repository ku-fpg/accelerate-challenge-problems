{-# LANGUAGE BangPatterns, RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeOperators#-}

import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate as A

{-
        This works fine in ghci, once we start using the CUDA backend, compiling with the -threaded flag is required
-}

accmat = use $ fromList (Z :. 5 :. 5) [1..25] :: Acc(A.Array DIM2 Double)
accvec = use $ fromList(Z :. 5) [1..25] :: Acc(A.Array DIM1 Double)

-- This function was lifted from Trevor McDonell's slides @ http://www.cse.unsw.edu.au/~tmcdonell/presentations/2013-lambdajam-workshop.pdf
takeRow	::	Exp	Int	->	Acc	(A.Array	DIM2	((Int,Int), Double))	->	Acc	(A.Vector	((Int,Int), Double))
takeRow	n	mat	=
		let	Z	:.	_	:.	cols	=	unlift	(shape	mat)	::	Z:.	Exp	Int	:.	Exp	Int
		in	backpermute	(index1	cols)
						(\ix	->	index2	n	(unindex1	ix))
						mat                      


genIndices :: Acc(A.Array DIM2 Double) -> Acc(A.Array DIM2 (Int, Int))
genIndices amat = generate (index2 rows cols) (\ix -> let (Z:.y:.x) = unlift ix :: Z :. Exp Int :. Exp Int in lift(y,x)) :: Acc(A.Array DIM2 (Int,Int))
        where Z :. rows :. cols = unlift (shape amat) :: Z :. Exp Int :. Exp Int                        

tan_h :: Exp Double -> Exp Double
tan_h x = (1 - exp(-2*x)) / (1 + exp(-2*x))   
 
atan_h :: Exp Double -> Exp Double  
atan_h x = (0.5) * (log(x + 1) - log(1 - x))


{-
        This function does not actually apply tanh and atanh, but they would be easy to add once we get it working.
        I used tuple selectors instead of pattern matching because there are weird lifting/unlifting issues with pattern matching that I'm still learning.     
        
-}                            
ne_attempt ::  Acc	(A.Array  DIM2	((Int,Int), Double)) -> Acc	(A.Array  DIM2	((Int,Int), Double))   
ne_attempt amat = generate (index2 rows cols) (\ix -> let (Z:.y:.x) = unlift ix :: Z :. Exp Int :. Exp Int in  (lift ((y,x), (neighbor_product amat y))) :: Exp((Int,Int), Double))
        where Z :. rows :. cols = unlift (shape amat) :: Z :. Exp Int :. Exp Int
              neighbor_product :: Acc(A.Array DIM2  ((Int,Int), Double)) -> Exp Int -> Exp Double
              neighbor_product mat arg = the (A.fold1 (*) $ A.snd $ (lift (A.unzip $ A.filter (\tup -> ((A.snd . A.fst) tup) /=* arg)
                            (takeRow arg amat)) :: Acc(A.Array DIM1 (Int, Int), A.Array DIM1 Double)))
                            
main = print $ ne_attempt $ A.zip (genIndices accmat) accmat                             