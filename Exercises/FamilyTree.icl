implementation module FamilyTree

import StdEnv
import StdDebug
import GenTree

::  FamilyTree					:== GenTree Couple Single
::  Couple						  = Couple Person Person
::  Single						  = Single Person
::  Person						  = Person DateOfBirth Gender String
::  Gender						  = Male | Female
::  DateOfBirth					  = DoB Year Month Day
::  Year						:== Int
::  Month						:== Int
::  Day							:== Int

okFamilyTree					:: FamilyTree -> Bool
okFamilyTree _ = trace_n "okFamilyTree not yet implemented" False

rootAncestor					:: FamilyTree -> Person
rootAncestor _ = trace_n "rootAncestor not yet implemented" (Person (DoB 0 0 0) Male "Bilbo Baggins")

inFamilyTree					:: Person FamilyTree -> Bool
inFamilyTree _ _ = trace_n "inFamilyTree not yet implemented" False

marry							:: Person Person FamilyTree -> FamilyTree
marry _ _ _ = trace_n "marry not yet implemented" (Leaf (Single (Person (DoB 0 0 0) Male "Bilbo Baggins")))

addChild						:: Person Couple FamilyTree -> FamilyTree
addChild _ _ tree = trace_n "addChild not yet implemented" tree

children						:: Person FamilyTree -> [Person]
children _ _ = trace_n "children not yet implemented" []

offspring						:: Person FamilyTree -> [Person]
offspring _ _ = trace_n "offspring not yet implemented" []

