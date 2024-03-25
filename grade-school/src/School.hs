module School (School, add, empty, grade, sorted) where
import Data.List (sortBy, groupBy)

data Student = Student Int String

type School = [ Student ]

getGrade :: Student -> Int
getGrade (Student i _) = i

getName :: Student -> String
getName (Student _ name) = name

add :: Int -> String -> School -> School
add gradeNum student school = Student gradeNum student : school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade gradeNum school = [ getName s | s  <- school, (getGrade s)  == gradeNum ]

compareByGrade :: Student -> Student -> Ordering
compareByGrade (Student a _) (Student b _) = compare a b

compareByName :: Student -> Student -> Ordering
compareByName (Student _ a) (Student _ b) = compare a b

getGradeAndNames :: [Student] -> (Int, [String])
getGradeAndNames s = (g, [ getName n | n <- s ]) where g = getGrade.head $ s 

isEq :: Ordering -> Bool
isEq EQ = True
isEq _  = False

sorted :: School -> [(Int, [String])]
sorted school = getGradeAndNames <$> ( (sortBy compareByName) <$> groupedByGrade )
  where
    sortedByGrade  = sortBy compareByGrade school
    groupedByGrade = groupBy (isEq.compareByGrade) sortedByGrade

