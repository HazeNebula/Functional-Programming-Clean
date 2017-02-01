module FQL

import StdEnv
import StdDebug
import StdT

::  Song = { group   :: String         // Name of the group
           , album   :: String         // Name of the album
           , year    :: Int            // Release date
           , track   :: Int            // Track nr. (1..)
           , title   :: String         // Title of the song
           , tags    :: [String]       // Descriptive tags of the album / song
           , time    :: T              // Playing time of the song
           , country :: [String]       // Country of origin of the group
           }

Start world
# (ok,dbs,world)        = fopen "FQL-songs.dbs" FReadText world
| not ok                = abort "Couldn't open 'FQL-songs.dbs'."
# (inhoud,dbs)          = filelines dbs
# (ok,world)            = fclose dbs world
| not ok                = abort "Couldn't close 'FQL-songs.dbs' after reading."
# nummersDB             = [ { group   = group
                            , album   = cd
                            , year    = toInt year
                            , track   = toInt track
                            , title   = title
                            , tags    = sort (symbol_separated_list ',' tags)
                            , time    = fromString length
                            , country = sort (symbol_separated_list ',' countries)
                            }
                         \\ [_,group,cd,year,track,title,tags,length,countries]
                            <- collect (nr_of_fields+1)   // collect all elements of an entry
                               (drop (nr_of_fields+1)     // delete the headers
                               (map initString inhoud))   // remove the \n
                          ]
= (all_groups nummersDB,world)
where
	nr_of_fields = 8

all_groups              :: [Song] -> [String]
all_groups _ = trace_n "all_groups not yet implemented" []

all_periods             :: [Song] -> [String]
all_periods _ = trace_n "all_periods not yet implemented" []

all_albums_of           :: String [Song] -> [(Int,String)]
all_albums_of _ _ = trace_n "all_albums_of not yet implemented" []

all_tracks              :: String String [Song] -> [(Int,String,T)]
all_tracks _ _ _ = trace_n "all_tracks not yet implemented" []

time_albums             :: [Song] -> [(T,String,String)]
time_albums _ = trace_n "time_albums not yet implemented" []

total_time              :: [Song] -> T
total_time _ = trace_n "total_time not yet implemented" zero

dutch_metal             :: [Song] -> [String]
dutch_metal _ = trace_n "dutch_metal not yet implemented" []

/* The functions below are needed in the Start rule to read the file 'FQL-songs.dbs'.
 * You do not need to understand how they work.
 */

//  filelines reads all lines in a File
filelines               :: !*File -> (![String],!*File)
filelines file
# (end,file)            = fend file
| end                   = ([],file)
# (line,file)           = freadline file
# (lines,file)          = filelines file
= ([line:lines],file)

//  initString removes whitespace at the end of a String
initString              :: (String -> String)
initString				= toString o reverse o (dropWhile isSpace) o reverse o fromString

/*  collect n [x_1, ..., x_n, x_{n+1}, ... x_{2n}, ..., x_{mn+1} ... x_{mn+k}]
        = [[x_1, ..., x_n], [x_{n+1}, ... x_{2n}], ..., [x_{(m-1)n+1} ... x_{mn}]]
    where:
        n > 0 /\ m >= 0 /\ k <= n
*/
collect                 :: !Int ![x] -> [[x]]
collect n list
| length groupN < n     = []
| otherwise             = [groupN:collect n list`]
where
    (groupN,list`)      = splitAt n list

symbol_separated_list   :: !Char !String -> [String]
symbol_separated_list c str
                        = filter (\str -> str <> "" && str <> (toString c)) [toString cs \\ cs <- group` ((==) c) (fromString str)]
where
//	eliminates unnecessary dependency on other assignments, so define as local function
	group`				:: (a -> Bool) [a] -> [[a]]
	group` p []			= []
	group` p xs			= [yes,no:group` p more]
	where
		(yes,no_more)	= span p xs
		(no,more)		= span (not o p) no_more
