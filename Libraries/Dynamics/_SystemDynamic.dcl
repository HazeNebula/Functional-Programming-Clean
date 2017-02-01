definition module _SystemDynamic

from StdOverloaded import class toString, class ==
from StdCleanTypes import :: CTTypeDef

// dynamics consist of a value with a type code
:: DynamicTemp	= E.a:
	{	_value	:: a
	,	_type	:: TypeCode
	}

// representation of type codes
:: TypeCode
	=	TypeScheme !Int TypeCode		// Int = # variables
	|	TypeVar !Int					// Int = variable number
	|	TypeCons !TypeCodeConstructor
	|	TypeApp !TypeCode !TypeCode
	|	TypeUnique !TypeCode
	|	_TypeFixedVar !Int				// used internally between compiler and
										// unification algorithm
	|	_TypeEmpty						// used internally during unification

instance toString TypeCode
instance == TypeCode

// representation of type constructor codes
:: TypeCodeConstructor
instance toString TypeCodeConstructor
instance == TypeCodeConstructor
typeCodeOfDynamic :: !Dynamic -> TypeCode

// type code constructors for predefined types
TypeCodeConstructorInt :: TypeCodeConstructor
TypeCodeConstructorChar :: TypeCodeConstructor
TypeCodeConstructorReal :: TypeCodeConstructor
TypeCodeConstructorBool :: TypeCodeConstructor
TypeCodeConstructorDynamic :: TypeCodeConstructor
TypeCodeConstructorFile :: TypeCodeConstructor
TypeCodeConstructorWorld :: TypeCodeConstructor
TypeCodeConstructor_Arrow :: TypeCodeConstructor
TypeCodeConstructor_List :: TypeCodeConstructor
TypeCodeConstructor_StrictList :: TypeCodeConstructor
TypeCodeConstructor_UnboxedList :: TypeCodeConstructor
TypeCodeConstructor_TailStrictList :: TypeCodeConstructor
TypeCodeConstructor_StrictTailStrictList :: TypeCodeConstructor
TypeCodeConstructor_UnboxedTailStrictList :: TypeCodeConstructor
TypeCodeConstructor_Tuple :: !Int -> TypeCodeConstructor	// arity: 2 .. 32
TypeCodeConstructor_LazyArray :: TypeCodeConstructor
TypeCodeConstructor_StrictArray :: TypeCodeConstructor
TypeCodeConstructor_UnboxedArray :: TypeCodeConstructor

// unification functions

:: _UnificationEnvironment

_initial_unification_environment :: !Int !Int -> *_UnificationEnvironment
_bind_global_type_pattern_var :: !TypeCode !TypeCode !*_UnificationEnvironment -> *_UnificationEnvironment
// first type arg is the actual type, second is the type pattern
_unify :: !_UnificationEnvironment !TypeCode !TypeCode -> (!Bool, _UnificationEnvironment)
_normalise :: !_UnificationEnvironment !TypeCode -> TypeCode

typeCodeConstructorIsPredefined :: !TypeCodeConstructor -> Bool

_to_TypeCodeConstructor :: !(Bool -> CTTypeDef) -> TypeCodeConstructor

typeDefOfTypeConstructor :: TypeCodeConstructor -> CTTypeDef
