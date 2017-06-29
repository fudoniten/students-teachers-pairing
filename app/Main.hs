module Main where

import Domain
import Data.Map (Map)

generatePairings :: [(Tutor, Student)] -- State Map [(Tutor, Student)]
generatePairings = (flip map) getStudents $ \student ->
  

  -- ToDO: Clarify comment
  -- Find the 'most optimal tutor' for each subject the student is interested in
  -- We'll strive to make sure that tutors have approximately the same load
  let res = (flip map) (getStudentPreferences student) $ \subject ->
        let tutors = getTutorsForSubject subject
            appropriateTutor =  getAppropriateTutor tutors student subject
        in (appropriateTutor, subject)
  in catMaybes res


 -- ToDO: Remove duplicate pairings
 -- ToDO: Handle case when no teacher can handle a subject desired/preferred by the student


-- ToDO: Change this
getAppropriateTutor :: [Tutor] -> Student -> Subject -> Maybe Tutor
getAppropriateTutor [] _ _ = None
getAppropriateTutor tutors _ subject = tutors !! 0

displayPairings :: [(Tutor, Student)] -> IO()
displayPairings @pairings = do
  forM pairings $ \ p ->
    putStrLn "(" ++ fst p ++ ")"

main :: IO ()
main = do
  let pairings = runState generatePairings $ Map.fromList()
  displayPairings pairings
