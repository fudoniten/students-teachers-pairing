module Main where

import Domain
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad (forM)
import GHC.IO.Unsafe (unsafePerformIO)
import Debug.Trace (trace, traceShow)


-- Given a list of students, generate a pairing of the form (Tutor, Student)

-- Note: Our definition of 'Most Optimal':

-- We strive to make sure that all teachers have approximately the same load.
-- So, when there is the possibility that a Student can be paired to multiple tutors,
-- prioritize the teacher who is the least busy right now, i.e: with the least amount of students.
-- We do that because we want to shoot for equal workload and work/life balance for our personel, leading to a better handling of the clients and happier employees.

-- We use a greedy approach to solving this problem, and thus we end up with an approximate optimization. If correctness is absolutely necessary, we would make use of 'Backtracking', which would take more time, as it would explore all possibilities.

-- ToDO's: (these are things to be done in the future)
-- * Remove duplicates from the list
-- * Remove entries where there's no tutor available to handle a subject
-- * Fix the issue with prioritization not working perfectly

generatePairings :: [Student] -> [(Tutor, Student, Subject)]
generatePairings students =
  let result = concat . (flip map) students $ \student ->
        let (xs, _) = runState (generatePairingsInternal student) (TutorLoad M.empty)
        in xs
  in [(tut, stu, subj) | (Just tut, stu, subj) <- result]


generatePairingsInternal :: Student -> State TutorLoad [(Maybe Tutor, Student, Subject)]
generatePairingsInternal student = do
  tutorLoad <- get
  let subjects = getStudentPreferences student
  forM subjects $ \subject -> do
    let tutors = getTutorsForSubject subject
    -- let (mTutor, _) = runState (getAppropriateTutor tutors student subject) tutorLoad
    mTutor <- getAppropriateTutor tutors student subject
    return (mTutor, student, subject)
   where
      str :: Tutor -> Load -> String
      str tut load = "Tutor: " ++ (show tut) ++ ", " ++ "Load: " ++ (show load)
      debug_ :: TutorLoad -> IO ()
      debug_ tutorLoad  = do
         mapM_ (\(tut, load) -> putStrLn (str tut load)) $ M.toList (getMap tutorLoad)

type Load = Int
data TutorLoad = TutorLoad { getMap :: M.Map Tutor Load }

-- For debugging purposes
instance Show TutorLoad where
  show tl = concat $ map (\(tut, load) -> (str tut load)) $ M.toList (getMap tl)
            where
              str tutor load = "Tutor: " ++ (show tutor) ++ ", " ++ "Load: " ++ (show load) 
getAppropriateTutor :: [Tutor] -> Student -> Subject -> State TutorLoad (Maybe Tutor)        
getAppropriateTutor [] _ _ = return Nothing
getAppropriateTutor tutors _ subject = do
  tutorWithMinimumLoad <- getTutorWithMinimumLoad tutors
  return $ Just tutorWithMinimumLoad

  where
    getTutorWithMinimumLoad :: [Tutor] -> State TutorLoad Tutor
    getTutorWithMinimumLoad tutors = do
      tutorLoadM <- get
      let tutorLoad = getMap tutorLoadM
      return $ foldl (\t1 t2 -> getMin t1 (M.lookup t1 tutorLoad) t2 (M.lookup t2 tutorLoad)) (tutors !! 0) tutors
        

    getMin :: Tutor -> Maybe Load -> Tutor -> Maybe Load -> Tutor
    getMin t1 t1load t2 t2load = case (t1load, t2load) of
      (Nothing, Nothing) -> t1
      (Just _, Nothing) -> t2
      (Nothing, Just _) -> t1
      (Just t1cost, Just t2cost) -> if (t1cost > t2cost) then t2 else t1

displayPairings :: [(Tutor, Student, Subject)] -> IO ()
displayPairings pairings = do
  forM_ pairings $ \(tutor, student, subject) ->
    let str = "( " ++ (show tutor) ++ ", " ++ (show student) ++ " )"
    in putStrLn str

main :: IO ()
main = do
  putStrLn "Here are the pairings: "
  let pairings = generatePairings getStudents
  displayPairings pairings
