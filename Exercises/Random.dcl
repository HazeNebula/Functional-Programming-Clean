definition module Random

//*****************************************************************************
// General utility for random number generation.
//
// This module was originally written for Clean 1.3.2 using Clean Standard
// Object I/O library 1.2, and rewritten to remove the Object I/O dependency
// for Clean 2.4 on Linux.
//
//*****************************************************************************

:: RandomSeed

nullRandomSeed   :: RandomSeed
// nullRandomSeed generates a useless RandomSeed (random nullRandomSeed = (0,nullRandomSeed)).

getNewRandomSeed :: !*World -> (!RandomSeed, !*World)
// GetNewRandomSeed generates a useful RandomSeed, using the current time.

random           :: !RandomSeed -> .(!Int, !RandomSeed)
// Given a RandomSeed, Random generates a random number and a new RandomSeed.
