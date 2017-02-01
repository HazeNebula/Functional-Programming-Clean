implementation module SimpleFileIO

import StdEnv
import StdDebug
import StdMaybe

readFile				:: String *env -> (Maybe String,*env) | FileSystem env
readFile _ env = trace_n " not yet implemented" (Nothing,env)

writeFile				:: String String *env -> (Bool,*env) | FileSystem env
writeFile _ _ env = trace_n "writeFile not yet implemented" (False,env)

readLines				:: String *env -> (Maybe [String],*env) | FileSystem env
readLines _ env = trace_n "readLines not yet implemented" (Nothing,env)

writeLines				:: String [String] *env -> (Bool,*env) | FileSystem env
writeLines _ _ env = trace_n "writeLines not yet implemented" (False,env)

mapFile					:: String String (a -> b) *env -> (Bool,*env) | FileSystem env & fromString a & toString b
mapFile _ _ _ env = trace_n "mapFile not yet implemented" (False,env)

