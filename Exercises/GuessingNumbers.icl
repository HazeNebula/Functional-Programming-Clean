module GuessingNumbers

/*	Dynamics only work with a 32-bit version of the Clean compiler.
	(*) Use Environment 'Experimental'
	(*) In Project:Project Options: check flag 'Enable dynamics'
*/
import StdEnv
import StdDebug
import StdDynamic, StdDynamicFileIO		// necessary for dynamics and dynamics in files
import StdFileSelect					// open platform file selector dialogue

number_ok :== 5

Start						:: *World -> *World
Start world
= case selectInputFile world of
	(Just fname, world)
		# (ok,dyn,world)	= readDynamic fname world
		# (io,    world)	= stdio world
		| not ok			= snd (fclose (io <<< "Could not open file '" <<< fname <<< "' with readDynamic.\n") world)
		# (path,name,ext)	= strip fname
		# io				= io <<< "Guess the number sequence in '" <<< name <<< "'\n"
		# io				= io <<< "<ENTER>: displays the next number\nnumber+<ENTER> is the number that you guess\ntext+<ENTER> terminates the program\n"
		# io				= inspect name (fromDynamic [] dyn) io
		= snd (fclose io world)
	(nothing, world)		= world

/** inspect fname sequence io:
	allows the user to guess the secret sequence of numbers in @sequence, that were stored in file @fname.
	All user-I/O is done via the console @io.
*/
inspect						:: String [Int] *File -> *File
inspect _ _ io = trace_n "inspect not yet implemented" io

fromDynamic					:: t Dynamic -> t | TC t
fromDynamic v (x :: t^)		= x
fromDynamic v _				= v

/** strip file_path:
	returns (path,name,extension) that are extracted from the @file_path.
*/
strip						:: String -> (String,String,String)
strip path_file_ext			= (path,name,ext)
where
	cs						= fromString path_file_ext
	ext						= toString ['.' : reverse (takeWhile ((<>) '.') (reverse cs))]
	name					= toString (reverse (drop (size ext) (takeWhile ((<>) '\\') (reverse cs))))
	path					= path_file_ext%(0, size path_file_ext-size name-size ext-1)

/** is_a_number str:
	returns (True,n) if @str represents the number @n, and returns (False,0) otherwise.
*/
is_a_number					:: String -> (Bool,Int)
is_a_number str				= case fromString str of
								[c : cs] = ((isDigit c || c == '-') && all isDigit cs,toInt str)
								empty    = (False,0)
