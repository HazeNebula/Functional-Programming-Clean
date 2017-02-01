implementation module TextCompose

import StdEnv
import StdDebug
import StringUtil

/** This module implements a number of operations to compose blocks of text.
*/
::	Text				:== [String]
::	AlignH				=	LeftH | CenterH | RightH	// align left, center, right  horizontally
::	AlignV				=	TopV  | CenterV | BottomV	// align top,  center, bottom vertically
::	Width				:== NrOfChars
::	Height				:== NrOfLines
::	NrOfChars			:== Int							// 0 <= nr of chars
::	NrOfLines			:== Int							// 0 <= nr of lines

toText					:: !(!AlignH,!Width) !(!AlignV,!Height) !a -> Text | toString a
toText _ _ _ = trace_n "toText not yet implemented" []

fitText					:: !a -> Text | toString a
fitText _ = trace_n "fitText not yet implemented" []

instance toString Text where toString _ = trace_n "instance toString Text not yet implemented" ""

class sizeOf a			:: !a -> (!Width,!Height)
instance sizeOf String where sizeOf _ = trace_n "instance sizeOf String not yet implemented" (zero,zero)

instance sizeOf Text where sizeOf _ = trace_n "instance sizeOf Text not yet implemented" (zero,zero)

instance zero Text where zero = trace_n "instance zero Text not yet implemented" []

horz					:: !AlignV ![Text] -> Text
horz _ _ = trace_n "horz not yet implemented" []

vert					:: !AlignH ![Text] -> Text
vert _ _ = trace_n "vert not yet implemented" []

frame					:: !Text -> Text
frame _ = trace_n "frame not yet implemented" []

repeath					:: !Int !Char -> Text
repeath _ _ = trace_n "repeath not yet implemented" []

repeatv					:: !Int !Char -> Text
repeatv _ _ = trace_n "repeatv not yet implemented" []

