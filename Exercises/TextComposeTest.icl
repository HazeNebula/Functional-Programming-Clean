module TextComposeTest

/*	Test module TextCompose
	For working with Gast:
		(*) use Environment 'Gast'
		(*) set Project Options to 'Basic Values Only'.
*/

import gast
import TextCompose

derive genShow AlignH, AlignV
derive ggen AlignH, AlignV
derive bimap [], AlignH, AlignV

Start							= testnm 1000 9
									(\t ah av -> 
									       sizeOf_works                t
										/\ identity               (hdd t)
										/\ align_check           av ah
										/\ frame_dimensions       (hdd t)
										/\ vert_preserves_width     ah t
										/\ vert_sums_height         ah t
										/\ horz_sums_width       av    t
										/\ horz_preserves_height av    t
										/\ horzvert_check        av ah
										/\ True
									)
hdd :: [String] -> String
hdd [] = ""
hdd s = hd s

cut :: Char a -> String | toString a
cut ch x = toString (dropln (reverse (dropln (reverse s))))
where s = [ c \\ c <-: toString x ]
      dropln = dropWhile ((==) ch)

// A text block of height 0 and width 1 is not representable unless you are doing weird things.
sizeOf_works :: [String] -> Property
sizeOf_works strs
  = name "sizeOf correct" 
    (xh == n && yh == 1 && xv == if (n==0) 0 1 && yv == n)
		where (xh,yh) = sizeOf (repeath n '.')
			  (xv,yv) = sizeOf (repeatv n '.')
			  n = length strs+1

identity :: String -> Property
identity str
  = name "fitText->toString == id"
    (cut '\n' (fitText str`) == str)
	where str` = toString [ c \\ c<-: str | c <> '\n' ]

align_check :: AlignV AlignH -> Property
align_check av ah
  = name "align_check"
    (
		cut ' ' (foldl (+++) "" (map toString str)) == "." && str!!ypos av!!xpos ah == '.'
	)
	where str = mklines [ c \\ c<-: cut '\n' (toString (toText (ah,3) (av,3) '.')) ]
	      ypos TopV = 0
	      ypos CenterV = 1
	      ypos BottomV = 2
	      xpos LeftH = 0
	      xpos CenterH = 1
	      xpos RightH = 2

horzvert_check :: AlignV AlignH -> Property
horzvert_check av ah
  = name "horzvert_check"
    (
		str hor!!0!!xpos ah == '.' && str ver!!ypos av!!0 == '.'
    )
	where str what = mklines [ c \\ c<-: cut '\n' (toString what) ]
	      ypos TopV = 0
	      ypos CenterV = 1
	      ypos BottomV = 2
	      xpos LeftH = 0
	      xpos CenterH = 1
	      xpos RightH = 2
		  hor = vert ah [fitText ".", repeath 3 '+']
		  ver = horz av [fitText ".", repeatv 3 '*']

frame_dimensions :: String -> Property
frame_dimensions strs
  = name "frame dimensions"
    (fx == x+2 && fy==y+2)
        where (fx,fy) = sizeOf (frame texts)
		      (x,y)   = sizeOf texts
		      texts   = fitText strs

horz_preserves_height :: AlignV [String] -> Property
horz_preserves_height a strs
  = name "horz_preserves_height"
    (
		snd (sizeOf (horz a texts)) == maxList [ 0 : [ h \\ (b,h) <- map sizeOf texts ] ]
	)
	    where texts = map fitText strs

vert_preserves_width :: AlignH [String] -> Property
vert_preserves_width a strs
  = name "vert_preserves_width"
	(
		fst (sizeOf (vert a texts)) == maxList [ 0 : [ b \\ (b,h) <- map sizeOf texts ] ]
	)
	    where texts = map fitText strs

horz_sums_width :: AlignV [String] -> Property
horz_sums_width a strs
  = name "horz_sums_width"
	(
		fst (sizeOf (horz a texts)) == sum [ 0 : [ b \\ (b,h) <- map sizeOf texts ] ]
	)
	    where texts = map fitText strs

vert_sums_height :: AlignH [String] -> Property
vert_sums_height a strs
  = name "vert_sums_height"
	(
		snd (sizeOf (vert a texts)) == sum [ 0 : [ h \\ (b,h) <- map sizeOf texts ] ]
	)
	    where texts = map fitText strs
