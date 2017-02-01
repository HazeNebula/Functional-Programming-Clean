implementation module ThisOldMan

/*	Create an application with 'Project:Project Options...' 'Console:Basic Values Only'
*/
import StdEnv
import StdDebug

numList = [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten" ]
locationList = [ "on my thumb", "on my shoe", "on my knee", "on my door", "on my hive", "on my sticks", "up in heaven", "on my gate", "on my spine", "once again" ]

Start				:: String
Start				= this_old_man

this_old_man		:: String
this_old_man 		= this_old_man` 0

this_old_man` n
| n < 0				= abort "n cannot be smaller than 0."
| n <= 9			= "This old man, he played " +++
					  numList !! ( n ) +++
					  ",\nHe played knick-knack " +++
					  locationList !! ( n ) +++
					  ";\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\n" +++
					  this_old_man` ( n + 1 )
| n >= 10			= ""