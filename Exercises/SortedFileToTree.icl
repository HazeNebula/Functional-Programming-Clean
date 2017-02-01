implementation module SortedFileToTree

import StdEnv
import StdDebug
import SimpleFileIO

Start                       :: *World -> (BTree String,*World)
Start world                 = readSortedFile path world

path :== "EnglishWords.txt"

::  BTree a                 = Leaf
                            | Node (BTree a) a (BTree a)

readSortedFile              :: String *env -> (BTree String,*env) | FileSystem env
readSortedFile _ env = trace_n "readSortedFile not yet implemented" (Leaf,env)

writeSortedFile :: String (BTree String) *env -> *env | FileSystem env
writeSortedFile _ _ env = trace_n "writeSortedFile not yet implemented" env

