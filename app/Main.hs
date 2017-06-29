module Main where

import Domain
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad (forM)

-- Given a list of students, generate a pairing of the form (Tutor, Student)
-- When there is the possibility that a Student can be paired to multiple tutors,
generatePairings :: [Student] -> [(Tutor, Student, Subject)]
generatePairings students =
  let result = concat . (flip map) students $ \student ->
        let (xs, _) = runState (generatePairingsInternal student) (TutorLoad M.empty)
        in xs
  in [(tut, stu, subj) | (Just tut, stu, subj) <- result]

  -- ToDO: Clarify comment
  -- Find the 'most optimal tutor' for each subject the student is interested in
  -- We'll strive to make sure that tutors have approximately the same load
  --(flip map) (getStudentPreferences student) $ \subject ->
  --  let tutors = getTutorsForSubject subject
  --      (mTutor, _) = runState (getAppropriateTutor tutors student subject) tutorLoad
  --  in (mTutor, student, subject)


  --(flip map) (getStudentPreferences student) $ \subject ->
  --  let tutors = getTutorsForSubject subject
  --      (tut, _) = runState (getAppropriateTutor tutors student subject)
  --  in case tut of Just t -> Just (t, student, subject)
  --                 Nothing -> Nothing


 -- ToDO: Remove duplicate pairings
 -- ToDO: Handle case when no teacher can handle a subject desired/preferred by the student

generatePairingsInternal :: Student -> State TutorLoad [(Maybe Tutor, Student, Subject)]
generatePairingsInternal student = do
  state <- get
  let subjects = getStudentPreferences student
  forM subjects $ \subject -> do
    let tutors = getTutorsForSubject subject
    mTutor <- getAppropriateTutor tutors student subject
    --
    liftIO debug_
    --
    return (mTutor, student, subject)
   where
      str :: Tutor -> Load -> String
      str tut load = "Tutor: " + (show tut) ++ ", " ++ "Load: " ++ (show load)

      debug_ :: IO ()
      debug_  = do
         mapM_ (\(tut, load) -> putStrLn (str tut load)) $ M.toList (getMap state)

type Load = Int
data TutorLoad = TutorLoad { getMap :: M.Map Tutor Load }

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
  putStrLn "Display "
