import Control.Applicative
import Data.Traversable

data File a = File a

instance Functor File
instance Applicative File
instance Monad File


file :: a -> File a
file = File


file1 = file "X"
file2 = file "Y"

list_txt = file [file1, file2]

result1 = tar [file1, file2]

result2 = tar =<< list_txt


tar :: [File a] -> File [a]
tar = sequenceA
