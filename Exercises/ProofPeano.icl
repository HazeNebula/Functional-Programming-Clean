Given:

	::	Nat	= Zero | Suc Nat
	
	(##) :: Nat -> Int
	(##) Zero		= 0				(1)
	(##) (Suc n)	= 1 + ##n		(2)

1. Also given:

	add				:: Nat Nat -> Nat
	add Zero    n	= n				(3)
	add (Suc m) n	= Suc (add m n)	(4)
	
Prove the following proposition for all Nat m and n:

	## (add m n) = ##m + ##n

2. Be also given:

	mul				:: Nat Nat -> Nat
	mul m Zero		= Zero				(5)
	mul m (Suc n)	= add (mul m n) m	(6)

Prove the following proposition for all Nat m and n:

	## (mul m n) = ##m * ##n

