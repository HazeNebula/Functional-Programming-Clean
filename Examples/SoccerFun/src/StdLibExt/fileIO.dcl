definition module fileIO

/**	Collection of functions that extend functionality of StdFile.
*/

import StdBitmap

/** getImage path env
		expects a bitmap file at path. If this is not the case, the function
		aborts. Otherwise, it returns the bitmap.
*/
getImage :: !String !*env -> (!Bitmap,!*env) | FileSystem env

/** writeFile appendData path content env
		writes content to a currently closed file, located at path, and closes it again.
		It appends content to current content in case of appendData, and replaces content otherwise.
		The function aborts in case of incorrect path and failing to close the file.
*/
writeFile :: !Bool !String !String !*env -> *env | FileSystem env

/** readFile path env
		reads the current content of the file located at path as a text file and closes it.
		The function yields Nothing in case of incorrect path and failing to close the file
		and (Just content) otherwise.
*/
readFile :: !String !*env -> (!Maybe String,*env) | FileSystem env
