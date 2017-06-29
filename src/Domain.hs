module Domain (
  Student(..),
  Tutor(..),
  getStudents,
  getTutors,
  getStudentPreferences
) where

import Data.Map (Map)


-- Student related logic
type StudentId = Int
type StudentName  = String
data Student = Student StudentId StudentName deriving Show

getStudents :: [Student]
getStudents = map (\s -> Student (fst s) (snd s)) students_raw

getStudentPreferences :: Student -> [Subject]
getStudentPreferences student = [Subject 1 "S"]


-- Tutor related logic
type TutorId = Int
type TutorName = String
data Tutor = Tutor TutorId TutorName deriving Show

getTutors :: [Tutor]
getTutors = []


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

