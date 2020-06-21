module School (School, add, empty, grade, sorted) where

import Data.List (sort)

type Grade = Int

type Student = String

data GradeStudents = GradeStudents Grade [Student]

data School
  = School [GradeStudents]

removeGrade :: Grade -> School -> School
removeGrade gradeNum (School gs) = School $ filter (\(GradeStudents g _) -> gradeNum /= g) gs

add :: Int -> String -> School -> School
add gradeNum student school =
  case removeGrade gradeNum school of
    (School gs) -> School $ withNewStudent : gs
  where
    withNewStudent = GradeStudents gradeNum ((grade gradeNum school) ++ [student])

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum (School grades) =
  case (filter (\(GradeStudents g _) -> g == gradeNum) grades) of
    [] -> []
    (GradeStudents _ students : _) -> students

sorted :: School -> [(Int, [String])]
sorted (School gs) =
  sort $
    fmap
      (\(GradeStudents g s) -> (g, sort s))
      gs
