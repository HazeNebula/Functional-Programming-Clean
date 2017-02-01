module TestSimpleFileIO

import SimpleFileIO
import StdEnv
import StdDebug

file n :== "TestSimpleFileIO" +++ toString n +++ ".icl"

Start :: *World -> (String,*World)

/* Test readFile / writeFile: */
Start world
	= case readFile (file "") world of
		(Just contents,world) = case writeFile (file 1) contents world of
			(True,world) = ("File '" +++ file 1 +++ "' written.\n",    world)
			(_,   world) = ("File '" +++ file 1 +++ "' not written.\n",world)
		(Nothing, world) = ("Couldn't read '" +++ file "" +++ "'.\n",  world)

/* Test readLines / writeLines: */
Start world
	= case readLines (file 1) world of
		(Just regels,world) = case writeLines (file 2) (reverse regels) world of
			(True, world) = ("File '" +++ file 2 +++ "' written.\n",     world)
			(False,world) = ("File '" +++ file 2 +++ "' not written.\n", world)
		(Nothing,  world) = ("Couldn't read '" +++ file 1 +++ "'.\n",    world)

/* Test mapFile: */
Start world
	= case mapFile (file 2) (file 3) (map toUpper) world of
		(False,world) = ("Couldn't read '" +++ file 2 +++ "' or write '" +++ file 3 +++ "'.\n",world)
		(_,    world) = ("File '" +++ file 3 +++ "' written.\n",world)
