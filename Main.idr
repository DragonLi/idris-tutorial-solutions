module Main

import btree

main : IO ()
main = do let t = btree.fromList [1,2,3,4,5,6]
          print (btree.toList' t)
