//Coded by Niels van Nistelrooij (s4713648) and Jasper van den Bogart (s4781686)

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
= (time_albums nummersDB,world)
where
	nr_of_fields = 8

all_groups              :: [Song] -> [String]
all_groups songs	= noDuplicates (sort [x.group \\ x <- songs])

noDuplicates									:: [a] -> [a] | Eq a
noDuplicates [x:[]]	= [x]
noDuplicates [x:xs]	
| x == (hd xs)		= noDuplicates xs
| otherwise			= [x] ++ noDuplicates xs

all_periods             :: [Song] -> [String]
all_periods songs													= all_periods` (noDuplicates (sort [x.year \\ x <- songs])) 1
	where
		all_periods`	:: [Int] Int -> [String]
		all_periods` periods int
		| length periods == 1						= [toString (hd periods)]
		| int == (length periods)					= [toString (hd periods) +++ "-" +++ toString (periods !! (length periods - 1))]
		| ((hd periods) + int) == (periods !! int)	= all_periods` periods (int + 1)
		| int == 1									= [toString (hd periods)] ++ all_periods` (periods % (1, length periods - 1)) 1
		| otherwise									= [toString (hd periods) +++ "-" +++ toString (periods !! (int - 1))] 
													++ all_periods` (periods % (int, length periods - 1)) 1
		
all_albums_of           :: String [Song] -> [(Int,String)]
all_albums_of group songs		= noDuplicates (sort [(x.year, x.album) \\ x <- songs | x.group == group])

all_tracks              :: String String [Song] -> [(Int,String,T)]
all_tracks album group songs	= sort  [(x.track, x.title, x.time) \\ x <- songs | x.album == album && x.group == group]

time_albums             :: [Song] -> [(T,String,String)]
time_albums songs				=  noDuplicates (sort [(albumPlayTime songs x.group x.album, x.group, x.album) \\ x <- songs])
	where
		albumPlayTime	:: [Song] String String -> T
		albumPlayTime songs group album	= sum [x.time \\ x <- songs | x.group == group && x.album == album]
// shortest:	In abstenia (bonus CD) by Porcupine tree - 16:12
// longest:  	De Standaards van Spits by De Standaards van Spits - 309:48

total_time              :: [Song] -> T
total_time songs	= sum [x.time \\ x <- songs]
// Total playing time is:	27976:5

dutch_metal             :: [Song] -> [String]
dutch_metal songs	= noDuplicates [x.group \\ x <- songs | isMember "metal" x.tags && isMember "Netherlands" x.country]




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
