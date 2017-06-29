module Domain (
  Student(..),
  Tutor(..),
  Subject(..),
  getStudents,
  getTutorsForSubject,
  getStudentPreferences
) where

import qualified Data.Map as M


-- Student related logic
type StudentId = Int
type StudentName  = String
data Student = Student StudentId StudentName

instance Show Student where
  show (Student _ name) = name

getStudents :: [Student]
getStudents = map (\s -> Student (fst s) (snd s)) students_raw

getStudentPreferences :: Student -> [Subject]
getStudentPreferences (Student studentId _) =
  [subject | Just subject <- getSubjects (lookup studentId student_to_subjects_raw)]
  where
    getSubjects :: Maybe [SubjectId] -> [Maybe Subject]
    getSubjects msubjectId = case msubjectId of
                               Just ids -> map (\id ->
                                                  case lookup id subjects_raw of
                                                    Just subjectName -> Just (Subject id subjectName)
                                                    Nothing -> Nothing
                                               ) ids
                               Nothing -> []



-- Tutor related logic
type TutorId = Int
type TutorName = String
data Tutor = Tutor TutorId TutorName

instance Ord Tutor where
  compare (Tutor id1 _) (Tutor id2 _) = id1 `compare` id2

instance Eq Tutor where
 (Tutor id1 _) == (Tutor id2 _) = id1 == id2

instance Show Tutor where
  show (Tutor _ name) = name


getTutorsForSubject :: Subject -> [Tutor]
getTutorsForSubject (Subject subjectId _) =
  [tutor | Just tutor <- getTutors (lookup subjectId subject_to_tutors_raw)]
  where
    getTutors :: Maybe [TutorId] -> [Maybe Tutor]
    getTutors mtutorId = case mtutorId of
                           Just ids -> map (\id ->
                                              case lookup id tutors_raw of
                                                Just tutorName -> Just (Tutor id tutorName)
                                                Nothing -> Nothing
                                           ) ids
                           Nothing -> []

-- Subject related logic
type SubjectId = Int
type SubjectName = String
data Subject = Subject SubjectId SubjectName deriving Show

-- Raw Student Data
students_raw = [(1, "Annie"), (2, "Oskar"), (3, "Olle"), (4, "Ingrid"),
            (5, "Yuchen"), (6, "Arjun"), (7, "Esmeralda")]


-- Raw Subject Data
subjects_raw = [(1, "ACT Reading"), (2, "ACT Writing"), (3, "ACT Essay"),
            (4, "ACT Science"), (5, "ACT Math"), (6, "US History"),
            (7, "AP Chemistry")]

-- Raw 'Student to Subject relationship' Data
student_to_subjects_raw :: [(StudentId, [SubjectId])]
student_to_subjects_raw = [(1, [5]), -- Annie's preferences include 'ACT Maths'
                       (2, [7]),
                       (3, [1, 2, 3, 4, 5]),
                       (4, [6, 7]),
                       (5, [5]),
                       (6, [1, 2, 3]),
                       (7, [7])
                      ]

-- Encoding of the Tutor Eligibilities
-- Raw 'Subject To Tutor relationship' Data
subject_to_tutors_raw :: [(SubjectId, [TutorId])]
subject_to_tutors_raw = [(1, [1]),
                     (2, [1, 3]),
                     (3, [1, 3]),
                     (4, [2, 5]),
                     (5, [6]),
                     (6, [4]),
                     (7, [2])
                      ]


-- Raw Tutors Data
tutors_raw = [(1, "Joakim"),
          (2, "Bob"),
          (3, "Frieda"),
          (4, "Kseniya"),
          (5, "Maggie"),
          (6, "Jamal")
         ]

