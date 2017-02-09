implementation module VectorOverloading

import StdEnv
import StdDebug

:: Vector2 a = {x0 :: a, x1 :: a}

instance ==   (Vector2 a) | == a   where == vector0 vector1	= vector0.x0 == vector1.x0 && vector0.x1 == vector1.x1

instance zero (Vector2 a) | zero a where zero	= {x0 = zero, x1 = zero}

instance one  (Vector2 a) | one a  where one 	= {x0 = one, x1 = one}

instance ~    (Vector2 a) | ~ a    where ~ vector0	= {x0 = ~vector0.x0, x1 = ~vector0.x1}

instance +    (Vector2 a) | + a    where + vector0 vector1	= {x0 = (vector0.x0 + vector1.x0), x1 = (vector0.x1 + vector1.x1)}

instance -    (Vector2 a) | - a    where - vector0 vector1	= {x0 = (vector0.x0 - vector1.x0), x1 = (vector0.x1 - vector1.x1)}

instance *    (Vector2 a) | * a    where * vector0 vector1	= {x0 = (vector0.x0 * vector1.x0), x1 = (vector0.x1 * vector1.x1)}

instance /    (Vector2 a) | / a    where / vector0 vector1	= {x0 = (vector0.x0 / vector1.x0), x1 = (vector0.x1 / vector1.x1)}

Start	= {x0 = 5, x1 = 6} + one

