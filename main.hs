import Prelude hiding (null, head, length, tail)
-- import System.IO
import MinHeap

sortStrings :: [String] -> [String]
sortStrings = toList . fromList

main = interact (unlines . sortStrings . lines)
