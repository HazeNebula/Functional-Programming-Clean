definition module StdNum

import StdClass
// import StdQ only if you have implemented it
//import StdQ

::  Num

instance ==     Num
instance <      Num

instance +      Num
instance -      Num
instance zero   Num

instance *      Num
instance /      Num
instance one    Num

instance abs    Num
instance sign   Num
instance ~      Num

instance toInt  Num
instance toReal Num
// implement this instance only if you have implemented StdQ:
//instance toQ    Num

class    fromNum a :: !Num -> a
instance fromNum Int
instance fromNum Real
// implement this instance only if you have implemented StdQ:
//instance fromNum Q

class    toNum   a :: !a -> Num
instance toNum   Int
instance toNum   Real
// implement this instance only if you have implemented StdQ:
//instance toNum   Q

instance toString Num
