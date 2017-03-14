system module Team_Harmless

import Team

:: NumPlayers :== Int
:: Difficulty = Auto | Level Int

// import this module and add the team 'Harmless (n, level)' in Game\Team.icl
MostlyHarmless :: (!NumPlayers,!Difficulty) Home FootballField -> Team

Harmless :== MostlyHarmless (11,Auto)
