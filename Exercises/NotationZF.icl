module NotationZF

import StdEnv
import StdDebug

g1 as bs	= [(a,b) \\ a <- as, b <- bs]

g2 as bs = [(a,b) \\ a<-as & b<-bs]

g3 as bs	= [(a,b) \\ a <- as, b <- bs | a <> b]

g4 as bs = [(a, b) \\ a<-as, b<-bs | a==b]

g5 xss = [x \\ xs <- xss, x <- xs]

g6 a xs = [i \\ i <- [0..] & x <- xs | a == x]

Start = ""
