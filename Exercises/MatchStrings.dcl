definition module MatchStrings

// head s gives the first character of s and fails when s is the empty string.
head			:: String        -> Char

// tail s gives the same string but without the first character. It fails when
// s is the empty string
tail			:: String        -> String

// is_equal s1 s2 gives True if and only if s1 and s2 are fully identical.
is_equal		:: String String -> Bool

// is_substring s1 s2 gives True if and only if s1 is a literal substring of
// s2. In other words: there are t1 and t2 such that s1 == t1 ++ s2 ++ t2 where
// t1 and t2 can be empty.
is_substring	:: String String -> Bool

// is_sub s1 s2 gives True if and only if the characters of s1 appear in the
// same order in s2. Parts of s2 may be skipped.
is_sub	:: String String -> Bool

// is_match p s gives True if and only if pattern p can be applied fully on s.
// p may contain special wildcards: '.' matches one character in s and '*'
// matches zero or more characters.
is_match		:: String String -> Bool
