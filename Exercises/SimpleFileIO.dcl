definition module SimpleFileIO

import StdFile, StdOverloaded, StdMaybe

// 1.
readFile	:: String                 *env -> (Maybe String,  *env) | FileSystem env
writeFile	:: String String          *env -> (Bool,          *env) | FileSystem env

// 2.
readLines	:: String                 *env -> (Maybe [String],*env) | FileSystem env
writeLines	:: String [String]        *env -> (Bool,          *env) | FileSystem env

// 3.
mapFile		:: String String (a -> b) *env -> (Bool,          *env) | FileSystem env & fromString a & toString b
