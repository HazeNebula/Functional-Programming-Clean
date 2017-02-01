definition module TextCompose

import StdEnv

/** This module implements a number of operations to compose blocks of text.
*/
::	Text
::	AlignH		=	LeftH | CenterH | RightH	// align left, center, right  horizontally
::	AlignV		=	TopV  | CenterV | BottomV	// align top,  center, bottom vertically
::	Width		:== NrOfChars
::	Height		:== NrOfLines
::	NrOfChars	:== Int							// 0 <= nr of chars
::	NrOfLines	:== Int							// 0 <= nr of lines

/** zero
	returns the empty text: horz align [zero] = zero and vert align [zero] = zero.
*/
instance zero Text

/** toText (ah,reqw) (av,reqh) a
	transforms the String representation of a into a text block that is at least reqw 
	characters wide and consists of atleast reqh lines.
	The String representation of a determines whether more width or height is required.
	The text is aligned horizontally according to ah, and vertically according to av.
*/
toText					:: !(!AlignH,!Width) !(!AlignV,!Height) !a -> Text | toString a

/** fitText a
	transforms the String representation of a with: toText (LeftH,0) (TopV,0) a.
*/
fitText					:: !a -> Text | toString a

/** toString text
	adds a newline character to each line in text, and concatenates them.
*/
instance toString Text

/** sizeOf x
	determines the number of characters text block x is wide and the number of lines 
	it consists of.
*/
class sizeOf a			:: !a -> (!Width,!Height)
instance sizeOf String
instance sizeOf Text

/** horz av texts
	puts all text blocks within texts next to each other, and aligning them according to av.
	It is assumed that all lines within a text block have the same width.
*/
horz					:: !AlignV ![Text] -> Text

/** vert ah texts
	puts all text blocks within text below each other, and aligning them according to ah.
*/
vert					:: !AlignH ![Text] -> Text

/** repeath n c
	creates a single line of text consisting of n occurrences of c.
	It is illegal to pass '\n' as argument c.
*/
repeath					:: !Int !Char -> Text

/** repeatv n c
	creates a single column of text consisting of n occurrences of c.
	It is illegal to pass '\n' as argument c.
*/
repeatv					:: !Int !Char -> Text

/** frame t
	creates a new text that has a frame around t.
*/
frame					:: !Text -> Text
