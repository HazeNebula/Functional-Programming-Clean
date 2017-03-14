implementation module fileIO

import StdEnvExt
import StdBitmap

getImage				:: !String !*env -> (!Bitmap,!*env) | FileSystem env
getImage path env
	= case openBitmap path env of
		(Nothing,_)		= abort ("getImage: unable to get Image " <+++ path <+++".\n")
		(Just bm,env)	= (bm,env)

writeFile				:: !Bool !String !String !*env -> *env | FileSystem env
writeFile append path text env
# (open,outputFile,env)	= fopen path (if append FAppendText FWriteText) env
| not open				= abort ("writeFile: could not open " <+++ path <+++ ".\n")
# (close, env)			= fclose (outputFile <<< text) env
| not close				= abort ("writeFile: could not close " <+++ path <+++".\n")
| otherwise				= env

readFile				:: !String !*env -> (!Maybe String,*env) | FileSystem env
readFile path env
# (open,inputFile,env)	= fopen path FReadText env
| not open				= (Nothing,snd (fclose inputFile env))
# (ok,inputFile)		= fseek inputFile 0 FSeekEnd
| not ok				= (Nothing,snd (fclose inputFile env))
# (pos,inputFile)		= fposition inputFile
# (ok,inputFile)		= fseek inputFile 0 FSeekSet
| not ok				= (Nothing,snd (fclose inputFile env))
# (str,inputFile)		= freads inputFile pos
# (ok,env)				= fclose inputFile env
| not ok				= (Nothing, env)
| otherwise				= (Just str,env)
