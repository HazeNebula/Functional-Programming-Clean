implementation module FrequencylistGUI

import StdEnv
import StdDebug
import StdIO

showFrequencylist :: [(a,Int)] *World -> *World | toString a
showFrequencylist freqs world
	= startIO SDI Void initGUI [ProcessClose closeProcess] world
where
	nrFreqs			= length freqs
	domain			= map fst freqs
	range			= map snd freqs
	(r_min,r_max)	= (minList range, maxList range)
	r_range			= if (r_min <> r_max) (toReal (r_max - r_min + 1)) 1.0

	initGUI			= snd o (openWindow Void wdef)

	wdef			= Window "Frequencylist" NilLS
						[ WindowClose (noLS closeProcess)
						, WindowLook  False look
						]
	look _ {newFrame} picture
	# (fm,picture)	= getPenFontMetrics picture
	# picture		= unfill newFrame picture
	# picture		= setPenColour Red picture
	# picture		= drawAt {x=hMargin,y=h-vMargin} {zero & vx=w} picture
	# picture		= drawAt {x=hMargin,y=h-vMargin} {zero & vy=0-h} picture
	# picture		= setPenColour Black picture
	# picture		= seq [bar (fontLineHeight fm,fm.fDescent) af i \\ af <- freqs & i <- [0..]] picture
	= picture
	where
		frameSize	= rectangleSize newFrame
		(w,h)		= (frameSize.w,frameSize.h)
		wbar		= (w-hMargin) / nrFreqs
		height f	= toInt (toReal (h-vMargin) * toReal (f-r_min+1) / r_range)

		bar (fh,fd) (a,f) i picture
		# picture		= fill { corner1={x=hMargin+ i   *wbar+1,y=h-vMargin}
						       , corner2={x=hMargin+(i+1)*wbar-1,y=h-vMargin-height f}
						       } picture
		# (aw,picture)	= getPenFontStringWidth (toString a) picture
		# (fw,picture)	= getPenFontStringWidth (toString f) picture
		# picture		= undrawAt {x=hMargin + i*wbar + (wbar-aw)/2, y=h-vMargin-fd}          (toString a) picture
		# picture		= undrawAt {x=hMargin + i*wbar + (wbar-fw)/2, y=h-vMargin-height f+fh} (toString f) picture
		= picture

hMargin				= 10
vMargin				= 10

showFrequencylist2 :: [(a,Int)] *World -> *World | toString a
showFrequencylist2 freqs world
	= startIO SDI Void initGUI [ProcessClose closeProcess] world
where
	nrFreqs			= length freqs
	domain			= map fst freqs
	range			= map snd freqs
	(r_min,r_max)	= (minList range, maxList range)
	r_range			= if (r_min <> r_max) (toReal (r_max - r_min + 1)) 1.0

	initGUI			= snd o (openWindow Void wdef)

	wdef			= Window "Frequencylist" NilLS
						[ WindowClose      (noLS closeProcess)
						, WindowLook       False look
						, WindowVScroll    (stdScrollFunction Vertical 50)
						, WindowViewDomain {zero & corner2={x=2^16,y=nrFreqs*20}}
						]
	look _ {newFrame} picture
	# (fm,picture)	= getPenFontMetrics picture
	# picture		= unfill newFrame picture
	# picture		= setPenColour Red picture
	# picture		= drawAt {x=hMargin,y=vMargin} {zero & vx=w} picture
	# picture		= drawAt {x=hMargin,y=vMargin} {zero & vy=nrFreqs*fontLineHeight fm} picture
	# picture		= setPenColour Black picture
	# picture		= seq [bar (fontLineHeight fm,fm.fDescent) af i \\ af <- freqs & i <- [0..]] picture
	= picture
	where
		frameSize	= rectangleSize newFrame
		(w,h)		= (frameSize.w,frameSize.h)
		wbar		= (w-hMargin) / nrFreqs
		height f	= toInt (toReal (w-vMargin) * toReal (f-r_min+1) / r_range)

		bar (fh,fd) (a,f) i picture
		# picture		= fill { corner1={y=vMargin+ i   *fh,x=hMargin+1}
						       , corner2={y=vMargin+(i+1)*fh,x=hMargin+height f}
						       } picture
		# (fw,picture)	= getPenFontStringWidth (toString f) picture
		# picture		= undrawAt {y=vMargin+(i+1)*fh-fd,x=hMargin + 2}             (toString a) picture
		# picture		= undrawAt {y=vMargin+(i+1)*fh-fd,x=hMargin + height f - fw} (toString f) picture
		= picture
