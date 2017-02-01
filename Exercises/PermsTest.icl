module PermsTest

/*	Test module Perms
	For working with Gast:
		(*) Use Environment 'Gast'
		(*) Set Project Options to 'Basic Values Only'
*/

import StdEnv
import gast
import Perms

Start							= testn 1000
									(\xs ->    all_lengths_are_n        xs
									        /\ all_elements_are_present xs
									        /\ gives_fac_n_results      xs

									        /\ no_duplicate_lists       xs
									        /\ True
									)

all_lengths_are_n				:: [Int] -> Property
all_lengths_are_n xs			= let n = length xs
								   in name ("all lengths are " +++ toString n)
								           (all (\p -> length p == n) (perms xs))

all_elements_are_present		:: [Int] -> Property
all_elements_are_present xs		= let ps = perms xs
								   in name "all_elements_are_present"
								           (and [isMember x p \\ x <- xs, p <- ps])

no_duplicate_lists				:: [Int] -> Property
no_duplicate_lists xs			= let ps = perms (removeDup xs)
								   in name "no_duplicate_lists"
								           (length (removeDup ps) == length ps)

