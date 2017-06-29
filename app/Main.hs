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
    return (mTutor, student, subject)

-- ToDO: Change this
type Load = Int
data TutorLoad = TutorLoad { getMap ::  M.Map Tutor Load }

getAppropriateTutor :: [Tutor] -> Student -> Subject -> State TutorLoad (Maybe Tutor)
getAppropriateTutor [] _ _ = return Nothing
getAppropriateTutor tutors _ subject = do
  tutorLoad <- get
  let tutorWithMinimumLoad = getTutorWithMinimumLoad
  let newMap = M.adjust (\load -> load + 1) tutorWithMinimumLoad (getMap tutorLoad)
  put (TutorLoad newMap)
  return $ Just tutorWithMinimumLoad

  where
    getTutorWithMinimumLoad = foldl min (tutors !! 0) tutors

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
