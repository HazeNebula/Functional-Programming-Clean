module FamilyTreePrint

import StdEnv
import FamilyTree
import GenTreePrint
import TextCompose
import StringUtil

Start = toString Baggins_of_Hobbiton

/** source family tree Bilbo Baggins: 
    http://www.frodoforever.com/tree.php 
    (date december 10 2011)
*/
Baggins_of_Hobbiton
	= Node (Couple (m "Balbo Baggins") (f "Berylla Boffin"))
	       [Node (Couple (m "Mungo Baggins") (f "Laura Grubb"))
	             [Node (Couple (m "Bungo Baggins") (f "Belladonna Took"))
	                   [Leaf (Single (m "Bilbo Baggins"))
	                   ]
	             ,Node (Couple (f "Belba Baggins") (m "Rudigar Bolger"))
	                   []
	             ,Node (Couple (m "Longo Baggins") (f "Camellia Sackville"))
	                   [Node (Couple (m "Otho Sackville-Baggins") (f "Lobelia Bracegirdle"))
	                         [Leaf (Single (m "Lotho"))
	                         ]
	                   ]
	             ,Node (Couple (f "Linda Baggins") (m "Bodo Proudfoot"))
	                   [Leaf (Single (m "Odo Proudfoot"))
	                   ]
	             ,Node (Couple (m "Bingo Baggins") (f "Chica Chubb"))
	                   [Node (Couple (m "Falco Chubb-Baggins") unknown)
	                         [Node (Couple (f "Poppy Chubb-Baggins") (m "Filibert Bolger"))
	                               []
	                         ]
	                   ]
	             ]
	       ,Node (Couple (f "Pansy Baggins") (m "Fastolph Bolger"))
	             []
	       ,Node (Couple (m "Ponto Baggins") (f "Mimosa Bunce"))
	             [Node (Couple (f "Rosa Baggins") (m "Hildigrim Took"))
	                   []
	             ,Node (Couple (m "Polo Baggins") unknown)
	                   [Node (Couple (m "Posco Baggins") (f "Gilly Brownlock"))
	                         [Node (Couple (m "Ponto Baggins") unknown)
	                               [Leaf (Single (f "Angelica Baggins"))]
	                         ,Leaf (Single (m "Porto Baggins"))
	                         ,Node (Couple (f "Peony Baggins") (m "Milo Burrows"))
	                               [Leaf (Single (m "Mosco Baggins"))
	                               ,Leaf (Single (m "Moro Baggins"))
	                               ,Leaf (Single (f "Myrtle Baggins"))
	                               ,Leaf (Single (m "Minto Baggins"))
	                               ]
	                         ]
	                   ,Node (Couple (f "Prisca Baggins") (m "Wilibald Bolger"))
	                         []
	                   ]
	             ]
	       ,Node (Couple (m "Largo Baggins") (f "Tanta Hornblower"))
	             [Node (Couple (m "Fosco Baggins") (f "Ruby Bolger"))
	                   [Leaf (Single (f "Dora Baggins"))
	                   ,Node (Couple (m "Drogo Baggins") (f "Primula Brandybuck"))
	                         [Leaf (Single (m "Frodo Baggins"))
	                         ]
	                   ,Node (Couple (m "Dudo Baggins") unknown)
	                         [Node (Couple (f "Daisy Baggins") (m "Griffo Boffin"))
	                               []
	                         ]
	                   ]
	             ]
	       ,Node (Couple (f "Lily Baggins") (m "Togo Goodbody"))
	             []
	       ]
where
	unknown	= m "?"

instance zero (a,b,c) | zero a & zero b & zero c where zero = (zero,zero,zero)

p (d,m,y) gender name = Person (DoB y m d) gender name
m name = p zero Male   name
f name = p zero Female name

instance toString Couple where
	toString (Couple p1 p2)
		= toString (vert CenterH [fitText p1, fitText "x", fitText p2])
instance toString Single where
	toString (Single p)
		= toString p
instance toString Person where
	toString (Person dOb g name)
		= name <+ " (" <+ g <+ ")"
instance toString Gender where
	toString Male	= "Male"
	toString Female	= "Female"
instance toString DateOfBirth where
	toString (DoB y m d)	= d +> "-" +> m +> ("-" <+ y)
