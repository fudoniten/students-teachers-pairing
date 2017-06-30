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
  tutorLoad <- get
  tutorWithMinimumLoad <- getTutorWithMinimumLoad tutors
  return $ Just tutorWithMinimumLoad

  where
    getTutorWithMinimumLoad :: [Tutor] -> State TutorLoad Tutor
    getTutorWithMinimumLoad tutors = do
      tutorLoad <- get
      return $ foldl (\t1 t2 ->
               let (tut, _) = runState (getMin t1 t2) tutorLoad
               in tut) (tutors !! 0) tutors

    getMin :: Tutor -> Tutor -> State TutorLoad Tutor
    getMin t1 t2 = do
      load <- get
      let map = getMap load
      let mt1Load = M.lookup t1 map
      let mt2Load = M.lookup t2 map
      comp (t1, mt1Load) (t2, mt2Load)

    comp :: (Tutor, Maybe Load) -> (Tutor, Maybe Load) -> State TutorLoad Tutor
    comp (t1, Nothing) (t2, Nothing) = do
      load <- get
      let map = getMap load
      let map' = M.insert t1 1 map -- This one is being used for the first time, mark it as so
      let map'' = M.insert t2 0 map'
      return t1
    comp (t1, Nothing) (t2, Just t2Load) = do
      load <- get
      let map = getMap load
      let map' = M.insert t1 1 map
      return t1
    comp (t1, Just t1Load) (t2, Nothing) = do
      load <- get
      let map = getMap load
      let map' = M.insert t2 1 map
      return t2
    comp (t1, Just t1Load) (t2, Just t2Load) = do
      load <- get
      let map = getMap load
      case t1Load < t2Load of
        True -> do
          let map' = M.insert t1 (t1Load + 1) map
          return t1
        False -> do
          let map' = M.insert t2 (t2Load + 1) map
          return t2


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
