implementation module VectorOverloading

import StdEnv
import StdDebug

:: Vector2 a = {x0 :: a, x1 :: a}

instance ==   (Vector2 a) | == a   where == _ _ = abort "instance == (Vector2 a) not yet implemented"

instance zero (Vector2 a) | zero a where zero = abort "instance zero (Vector2 a) not yet implemented"

instance one  (Vector2 a) | one a  where one = abort "instance one (Vector2 a) not yet implemented"

instance ~    (Vector2 a) | ~ a    where ~ _ = abort "instance ~ (Vector2 a) not yet implemented" 

instance +    (Vector2 a) | + a    where + _ _ = abort "instance + (Vector2 a) not yet implemented"

instance -    (Vector2 a) | - a    where - _ _ = abort "instance - (Vector2 a) not yet implemented"

instance *    (Vector2 a) | * a    where * _ _ = abort "instance * (Vector2 a) not yet implemented"

instance /    (Vector2 a) | / a    where / _ _ = abort "instance / (Vector2 a) not yet implemented"

