/********************************************************************************************
	Clean OS Windows library module version 1.2.1.
	This module is part of the Clean Object I/O library, version 1.2.1, 
	for the Windows platform.
********************************************************************************************/

/********************************************************************************************
	About this module:
	Routines related to window/dialog handling. 
********************************************************************************************/
#define WINVER 0x0500
#define _WIN32_WINNT 0x0500

#include "cCrossCallWindows_121.h"
#include "cCCallWindows_121.h"
#include "cCCallSystem_121.h"
#include "cCrossCallCursor_121.h"
#include "cCrossCall_121.h"
#include "cAcceleratorTable_121.h"
#include "cCrossCallxDI_121.h"

/*	Global data:
*/
static PAINTSTRUCT gPaintStruct;
#ifdef _WIN64
static LONG_PTR stdEditCallback  = 0;		/* The standard internal Windows callback routine of edit controls. */
static LONG_PTR stdPopUpCallback = 0;		/* The standard internal Windows callback routine of pop up controls. */
#else
static LONG stdEditCallback      = 0;			/* The standard internal Windows callback routine of edit controls. */
static LONG stdPopUpCallback     = 0;			/* The standard internal Windows callback routine of pop up controls. */
#endif

/*	Registered Windows class names:
*/
static char CustomControlClassName[]   = "__CleanCustomControl";	/* Class for CustomControls */
static char CompoundControlClassName[] = "__CleanCompoundControl";	/* Class for CompoundControls */


/*	Local window data structures:
*/
struct LocalWindowData
{
	int  lwd_cursorcode;		/* The cursor shape of the window */
	BOOL lwd_usersizemoving;	/* The user is sizing/moving the window */
};

typedef struct LocalWindowData *LocalWindowData;

static LocalWindowData AllocateLocalWindowData (void)
{
	LocalWindowData lwd_wdata;

	lwd_wdata = (LocalWindowData) rmalloc (sizeof (struct LocalWindowData));
	lwd_wdata->lwd_cursorcode     = 0;
	lwd_wdata->lwd_usersizemoving = (BOOL)FALSE;

	return (lwd_wdata);
}

/*	DestroyLocalWindowData (wdata) frees the memory used by the wdata. 
*/
static void DestroyLocalWindowData (LocalWindowData wdata)
{
	rfree (wdata);
}

/* PA: The following two procedures do not seem to be used anymore.
//	IsSDIDocumentWindow (hwnd)
//	returns TRUE if the class name of hwnd is SDIWindowClassName.
//
BOOL IsSDIDocumentWindow (HWND hwnd)
{
	char *classname;
	int  classnamelength;
	BOOL isSDI;
	
	classnamelength = lstrlen (SDIWindowClassName) + 1;
	classname       = rmalloc (classnamelength);
	GetClassName (hwnd, classname, classnamelength);

	isSDI = nstrequal (classnamelength, classname, SDIWindowClassName);

	rfree (classname);

	return isSDI;
}

//	IsMDIDocumentWindow (hwnd)
//	returns TRUE if the class name of hwnd is MDIWindowClassName.
//
BOOL IsMDIDocumentWindow (HWND hwnd)
{
	char *classname;
	int  classnamelength;
	BOOL isMDI;
	
	classnamelength = lstrlen (MDIWindowClassName) + 1;
	classname       = rmalloc (classnamelength);
	GetClassName (hwnd, classname, classnamelength);

	isMDI = nstrequal (classnamelength, classname, MDIWindowClassName);

	rfree (classname);

	return isMDI;
}
*/


/*	Find the first non CompoundControl parent window of the argument
	hwnd. This procedure assumes that hwnd is the handle of a control. 
*/
static HWND GetControlParent (HWND hwndControl)
{
	HWND parent;
	char *parentclassname;
	int  classnamelength;

	parent = GetParent (hwndControl);
	classnamelength = lstrlen (CompoundControlClassName) + 1;
	parentclassname = rmalloc (classnamelength);
	GetClassName (parent, parentclassname, classnamelength);

	while (nstrequal (classnamelength-1, parentclassname, CompoundControlClassName))
	{
		parent = GetParent (parent);
		GetClassName (parent,parentclassname,classnamelength);
	}
	rfree (parentclassname);
	return parent;
}

/*	Return the hwnd of the parent window only if this is
	a Dialog (actually: is neither a SDIWindow or MDIWindow). If the parent is not
	a Dialog then NULL is returned. 
	This procedure assumes that hwnd is the handle of a control.
*/
static HWND GetControlParentDialog (HWND hwndControl)
{
	HWND parent;
	char *parentclassname;
	int  classnamelength;

	parent = GetControlParent (hwndControl);

	classnamelength = lstrlen (SDIWindowClassName) + 1;
	parentclassname = rmalloc (classnamelength);
	GetClassName (parent, parentclassname, classnamelength);

	if (nstrequal (classnamelength, parentclassname, SDIWindowClassName))
	{
		parent = NULL;
	}
	else if (nstrequal (classnamelength, parentclassname, MDIWindowClassName))
	{
		parent = NULL;
	}
	rfree (parentclassname);

	return parent;
}


static BOOL CALLBACK SetControlFontProc (HWND hchild,		/* handle to child window */
										 LPARAM lParam		/* application-defined value */
										)
{
	HFONT hfont;

	hfont = (HFONT) lParam;

	if (hfont)
		SendMessage (hchild, WM_SETFONT, (WPARAM) hfont, MAKELPARAM (TRUE, 0));

	return TRUE;
}


/*	WindowProcedures of the registered Clean windows/dialogs/controls.
*/

/*********************************************************************************************
	The callback routine for a modal/modeless dialog box.
*********************************************************************************************/
static BOOL CALLBACK DialogProcedure (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	printMessage ("Dialog procedure", hwnd, message, wParam, lParam);

	switch (message)
	{
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmCLOSE, hwnd);
				return TRUE;
			}
			break;
		case WM_SETCURSOR:
			{
				if (!GlobalCursorSet ())
				{
					return FALSE;
				}
				else
				{
					SetCursorFromCode (GetGlobalCursorCode ());
#ifdef _WIN64
					SetWindowLongPtr (hwnd, DWLP_MSGRESULT, (LONG_PTR)TRUE);
#else
					SetWindowLong (hwnd, DWL_MSGRESULT, TRUE);
#endif
					return TRUE;
				}
			} break;
		/*	WM_ENTERIDLE message is used to let Clean evaluate the initialisation action
			of a modal dialog by sending the CcWmIDLEDIALOG message.
		*/
		case WM_ENTERIDLE:
			{
				HWND hwndModalDialog;
				
				hwndModalDialog = (HWND)lParam;

				if (wParam == MSGF_DIALOGBOX && hwndModalDialog != ghwndLastModalDialog)
				{
					SendMessage1ToClean (CcWmIDLEDIALOG,(size_t)hwndModalDialog);
					ghwndLastModalDialog = hwndModalDialog;
				}
				else
				{
					SendMessage0ToClean (CcWmIDLETIMER);
				}
				return FALSE;
			} break;
		case WM_TIMER:
			{
				SendMessage2ToClean (CcWmTIMER, wParam, GetMessageTime ());
				return FALSE;
			} break;
		/* WM_ACTIVATE:
			*	in case of activate and previously a window was active, 
				send a deactivate message to Clean for the window.
			*	in case of deactivate and new active is a window,
				send an activate message to Clean for the window.
		*/
		case WM_ACTIVATE:
			{
				switch (LOWORD (wParam))
				{
					case WA_ACTIVE:
					case WA_CLICKACTIVE:
						{
							if (gActiveDialog == NULL && ghTopDocWindow != NULL)
							{
								/*	Currently a window is active. 
									Notify Clean of this by sending a deactivate event for this window.
								*/
								SendMessage1ToClean (CcWmDEACTIVATE, ghTopDocWindow);
							}
							SendMessage1ToClean (CcWmACTIVATE, hwnd);	/* Now tell Clean that the dialog is active. */
							gActiveDialog = hwnd;
						}
						break;
					case WA_INACTIVE:
						{
							HWND hwndNewActive = (HWND)lParam;			/* The window handle to be activated. */

							SendMessage1ToClean (CcWmDEACTIVATE, hwnd);
							gActiveDialog = NULL;

							if (hwndNewActive != NULL && hwndNewActive == ghTopDocWindow)
							{
								SendMessage1ToClean (CcWmACTIVATE, hwndNewActive);
							}
							else if (hwndNewActive == NULL && ghTopDocWindow != NULL)
							{
								SendMessage1ToClean (CcWmACTIVATE, ghTopDocWindow);
							}
						}
						break;
				}
				return FALSE;
			}
			break;
		case WM_COMMAND:
			{
				switch (LOWORD (wParam))	// First check if OK or CANCEL button has been pressed
				{
					case IDOK:
						{
							SendMessage2ToClean (CcWmSPECIALBUTTON, hwnd, ISOKBUTTON);
						}
						return TRUE;
					case IDCANCEL:
						{
							SendMessage2ToClean (CcWmSPECIALBUTTON, hwnd, ISCANCELBUTTON);
						}
						return TRUE;
				}
				switch (HIWORD (wParam))
				{
					case BN_CLICKED:
						{
							if (lParam != 0)
							{
								/* Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hwnd, lParam, GetModifiers (), LOWORD (wParam));
							}
							else if (LOWORD (wParam) == 2)
							{
								SendMessage1ToClean (CcWmCLOSE, hwnd);
							}
							return TRUE;
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lParam, CB_GETCURSEL, 0, 0);
							return FALSE;
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
							return FALSE;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lParam;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, hwnd, combo, newsel);
							}
							return 1;
						}
						break;
				}
				return FALSE;
			} break;
		case WM_INITDIALOG:
			{
				int x, y, w, h;
				HWND defctrl;

				SendMessage1ToClean (CcWmINITDIALOG, hwnd);

				x = gCci.p1;
				y = gCci.p2;
				w = gCci.p3;
				h = gCci.p4;
				defctrl = (HWND) gCci.p5;

				w += 2 * GetSystemMetrics (SM_CXFIXEDFRAME);
				h += 2 * GetSystemMetrics (SM_CXFIXEDFRAME) + GetSystemMetrics (SM_CYCAPTION);

				if (x == -1 && y == -1)
				{
					x = (GetSystemMetrics (SM_CXSCREEN) - w) / 2;
					y = (GetSystemMetrics (SM_CYSCREEN) - h) / 2;
				}

				MoveWindow (hwnd, x, y, w, h, FALSE);

				EnumChildWindows (hwnd, SetControlFontProc, (LPARAM) gDlogFont);

				if (defctrl != NULL)
				{
					SetFocus (defctrl);
					return FALSE;
				}
				else
				{
					return TRUE;		/* allow windows to set focus;	*/
				}
			}
			break;
		case WM_SETFONT:
			{
				HFONT hfont;

				hfont = (HFONT) wParam;

				gDlogFont = hfont;
				return FALSE;
			}
			break;
		case WM_HSCROLL:
		case WM_VSCROLL:
			{
				int nPos,nScrollCode;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					hwndScrollBar = (HWND) lParam;
					SendMessage5ToClean (CcWmSCROLLBARACTION, hwnd, hwndScrollBar, SB_CTL, nScrollCode, nPos);
				}
				return TRUE;
			}
			break;
		case WM_MOUSEWHEEL:		/* New: react on mouse wheel events. */
			{
				int direction;
				int zDelta = (int)HIWORD(wParam);
				if ((int)wParam > 0)
					direction = 0;			// Wheel moved away from the user.
				else
					direction = 1;			// Wheel moved towards the user. 
				SendMessage3ToClean (CcWmMOUSEWHEEL, hwnd, hwnd, direction);
				return FALSE;
			}
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lParam;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context         */
										lpdis->rcItem.left + 2,		/* ref point x            */
										lpdis->rcItem.top + 1,		/* ref point y            */
										ETO_CLIPPED | ETO_OPAQUE,	/* options                */
										&lpdis->rcItem,				/* clipping rect          */
										text,						/* text to draw           */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array       */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);

							return TRUE;
						} break;
					case ODT_BUTTON:
						{
							SendMessage3ToClean (CcWmDRAWCONTROL, hwnd, lpdis->hwndItem, lpdis->hDC);
								 
							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);
								
							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return TRUE;
						} break;
				}
				return FALSE;
			}
			break;
		/*	WM_SETFOCUS alternative circumvents application-crash when dialog is closed
			from the inside (by an element button or close box).
		*/
		case WM_SETFOCUS:
			{
				return TRUE;
			}
			break;
		default:
			return FALSE;
			break;
	}
	ErrorExit ("Fatal error: case leak in DialogProcedure (%d).",message);
}	/* DialogProcedure */


/*********************************************************************************************
	The callback routine for a custom control.
*********************************************************************************************/
static LRESULT CALLBACK CustomControlProcedure (HWND hwnd, UINT uMess, WPARAM wParam, LPARAM lParam)
{
	printMessage ("CustomControlProcedure", hwnd, uMess, wParam, lParam);
	switch (uMess)
	{
		case WM_PAINT:
			{
				HWND parent;
				HDC hdc;
				PAINTSTRUCT ps;

				parent = GetControlParent (hwnd);

				hdc = BeginPaint (hwnd, &ps);
				SendMessage3ToClean (CcWmDRAWCONTROL, parent, hwnd, hdc);
				EndPaint (hwnd, &ps);

				return 0;
			} break;
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				return 0;
			} break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				else
				{
					SendMouseStillUpToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				return 0;
			} break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return 0;
			} break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			} break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseUpToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				return 0;
			} break;
		case WM_TIMER:
			{
				if ((int) wParam == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				else
				{
					rprintf ("Custom control did not expect normal WM_TIMER message with id = %d.\n", wParam);
				}
				return 0;
			} break;
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				HWND hwndParent;
				int c = CheckVirtualKeyCode ((int) wParam);;

				if (!c || (uMess==WM_SYSKEYDOWN && c>=VK_F1 && c<=VK_F12))
				{
					return DefWindowProc (hwnd, uMess, wParam, lParam);
				}
				hwndParent = GetControlParent (hwnd);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
//		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent = GetControlParent (hwnd);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute). 
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return 0;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean (because of
					ControlActivate attribute).
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return 0;
			}
			break;
		case WM_GETDLGCODE:		/*	Inform dialog procedure to pass all keyboard input to the control. */
			return (DLGC_WANTCHARS | DLGC_WANTARROWS);
			break;
		default:
			return DefWindowProc (hwnd, uMess, wParam, lParam);
			break;
	}
	ErrorExit ("Fatal error: case leak in CustomControlProcedure (%d).",uMess);
}	/* CustomControlProcedure */


/*********************************************************************************************
	The callback routine for a compound control.
*********************************************************************************************/
static LRESULT CALLBACK CompoundControlProcedure (HWND hwnd, UINT uMess, WPARAM wParam, LPARAM lParam)
{
	printMessage ("CompoundControlProcedure", hwnd, uMess, wParam, lParam);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wParam))
				{
					case BN_CLICKED:
						{
							if (lParam != 0)
							{
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, GetControlParent (hwnd), lParam, GetModifiers (), LOWORD (wParam));
							}
							return 0;
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lParam, CB_GETCURSEL, 0, 0);
							return 0;
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
							return 0;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lParam;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, GetControlParent (hwnd), combo, newsel);
								return 1;
							}
						}
						break;
				}
				return 0;
			} break;
		case WM_PAINT:
			{
				HWND parentwindow;
				HDC hdc;
				PAINTSTRUCT ps;

				if (GetUpdateRect(hwnd,NULL,FALSE))	// determine if there is really an update area. 
				{
					parentwindow = GetControlParent (hwnd);
					hdc = BeginPaint (hwnd, &ps);
					SendMessage3ToClean (CcWmDRAWCONTROL, parentwindow, hwnd, hdc);
					EndPaint (hwnd, &ps);
				}
				
				return 0;
			} break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND parentwindow, hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					parentwindow  = GetControlParent (hwnd);
					hwndScrollBar = (HWND) lParam;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lParam==0 in case of Compound scrollbars. */
						hwndScrollBar = hwnd;		/* pass the compound control handle to Clean. */
						UpdateWindow (hwnd);		/* but first ensure that compound control is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lParam!==0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, parentwindow, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
				return 0;
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND parentwindow, hwndScrollBar;

				nScrollCode = LOWORD (wParam);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wParam);
					parentwindow  = GetControlParent (hwnd);
					hwndScrollBar = (HWND) lParam;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lParam==0 in case of Compound scrollbars. */
						hwndScrollBar = hwnd;		/* pass the compound control handle to Clean. */
						UpdateWindow (hwnd);		/* but first ensure that compound control is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lParam!==0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, parentwindow, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
				return 0;
			}
			break;
		case WM_MOUSEWHEEL:		/* New: react on mouse wheel events. */
			{
				HWND parentWindow = GetControlParent (hwnd);
				int direction;
				int zDelta = (int)HIWORD(wParam);
				if ((int)wParam > 0)
					direction = 0;			// Wheel moved away from the user.
				else
					direction = 1;			// Wheel moved towards the user. 
				SendMessage3ToClean (CcWmMOUSEWHEEL, parentWindow, hwnd, direction);
				return FALSE;
			}
			break;
		/*	The following cases concerning mouse events 
				(WM_LBUTTONDOWN upto WM_TIMER) have been copied from CustomControlProcedure. 
		*/
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				return 0;
			} break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				else
				{
					SendMouseStillUpToClean (GetControlParent (hwnd), hwnd, SIGNEDLOWORD (lParam), SIGNEDHIWORD (lParam));
				}
				return 0;
			} break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return 0;
			} break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			} break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseUpToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				return 0;
			} break;
		case WM_TIMER:
			{
				if ((int) wParam == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hwnd, &p);
					SendMouseStillDownToClean (GetControlParent (hwnd), hwnd, p.x, p.y);
				}
				else
				{
					rprintf ("Compound control did not expect normal WM_TIMER message with id = %d.\n", wParam);
				}
				return 0;
			} break;
		/*	The following cases concerning key events and focus events 
				(WM_SYSKEYDOWN upto WM_GETDLGCODE) have been copied from CustomControlProcedure.
		*/
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				HWND hwndParent;
				int c = CheckVirtualKeyCode ((int) wParam);

				if (!c || (uMess==WM_SYSKEYDOWN && c>=VK_F1 && c<=VK_F12))
				{
					return DefWindowProc (hwnd, uMess, wParam, lParam);
				}
				hwndParent = GetControlParent (hwnd);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
//		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent = GetControlParent (hwnd);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return DefWindowProc (hwnd, uMess, wParam, lParam);
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute).
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return 0;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return 0;
			}
			break;
		/*	The WM_CLOSE event is generated when a user presses escape inside an EditControl that exists
			within the CompoundControl which exists within a Dialog.
		*/
		case WM_CLOSE:
			{
				SendMessage1ToClean (CcWmCLOSE, GetControlParent (hwnd));
				return 0;
			}
			break;
		case WM_GETDLGCODE:		/*	Inform dialog procedure to pass all keyboard input to the control. */
			return (DLGC_WANTCHARS | DLGC_WANTARROWS);
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lParam;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context         */
										lpdis->rcItem.left + 2,		/* ref point x            */
										lpdis->rcItem.top + 1,		/* ref point y            */
										ETO_CLIPPED | ETO_OPAQUE,	/* options                */
										&lpdis->rcItem,				/* clipping rect          */
										text,						/* text to draw           */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array       */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return 0;
						} break;
					case ODT_BUTTON:
						{
							HWND parentwindow;
							parentwindow  = GetControlParent (hwnd);

							SendMessage3ToClean (CcWmDRAWCONTROL, parentwindow, lpdis->hwndItem, lpdis->hDC);
							 
							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);
							
							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return 0;
						} break;
				}
				return 0;
			}
			break;
		default:
			return DefWindowProc (hwnd, uMess, wParam, lParam);
			break;
	}
	ErrorExit ("Fatal error: case leak in CompoundControlProcedure (%d).",uMess);
}	/* CompoundControlProcedure */


/*********************************************************************************************
	There are two event handlers for subclassing edit controls:
	*	EditControlProcedure:
		This routine should be used for key sensitive edit controls. 
		It processes tab advance if in a dialog, activate/deactivate, keyboard input.
	*	SimpleEditControlProcedure:
		This routine should be used for non key sensitive edit controls.
		It processes tab advance if in a dialog, activate/deactivate.
*********************************************************************************************/
/*********************************************************************************************
	EditControlProcedure. 
	This routine catches all keyboard input and handles it as a standard windows edit control 
	would, but in addition it also sends each keyboard input to Clean. The only exception are 
	tab key presses. For this keyboard input EditControlProcedure first checks if it is inside 
	a Dialog. If so, then the keyfocus is advanced to the next control; otherwise the tab key 
	is also sent to Clean.
	SetFocus/KillFocus events are passed to Clean as Activate and Deactivate events. 
	All other events are handled as a standard windows edit control. 
*********************************************************************************************/
static LRESULT CALLBACK EditControlProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	LRESULT stdresult;

	printMessage ("Clean Edit Control", hwnd, uMess, wParam, lParam);

	/* First preprocess all messages as if you are a standard Windows edit control. */
	stdresult = CallWindowProc ((WNDPROC) stdEditCallback, hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
//		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				/* Check if the keyboard input is a tab key. If we're in a Dialog then move focus to next control. */
				if (c==WinTabKey)
				{
					hwndParent = GetControlParentDialog (hwnd);
					if (hwndParent!=NULL)	/* The EditControl is in a Dialog */
					{
						int modifiers;
						BOOL bPrevious;
						HWND hwndNextCtrl;

						modifiers = GetModifiers ();
						bPrevious = (BOOL) modifiers & SHIFTBIT;
						hwndNextCtrl = GetNextDlgTabItem (hwndParent,hwnd,bPrevious);

						if (hwndNextCtrl != hwnd)		/* You're not the only focusable item. */
						{
							SetFocus (hwndNextCtrl);	/* Advance key focus to next control. */
						}
						return stdresult;
					}
				}
				/* Check if the keyboard input is a escape key. If we're in a Dialog then further processing should halt,
				   because the message has been passed along the parent hierarchy to cause a WM_CLOSE event. 
				*/
				if (c==WinEscapeKey)
				{
					hwndParent = GetControlParentDialog (hwnd);
					if (hwndParent!=NULL)	/* The EditControl is in a Dialog */
					{
						return stdresult;
					}
				}

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return stdresult;
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndParent = GetControlParent (hwnd);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
//		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndParent;
				
				/* First check if tab/escape key should be suppressed inside Dialog. */
				if (((int)wParam == WinTabKey || (int)wParam == WinEscapeKey) && GetControlParentDialog (hwnd) != NULL)
				{
					return stdresult;
				}

				hwndParent = GetControlParent (hwnd);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwnd, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwnd, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwnd, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
//		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				/* First check if tab/escape key should be suppressed inside Dialog. */
				if (((int)wParam == WinTabKey || (int)wParam == WinEscapeKey) && GetControlParentDialog (hwnd) != NULL)
				{
					return stdresult;
				}

				if (gInKey)
					SendKeyUpToClean (GetControlParent (hwnd), hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return stdresult;
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute). 
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return stdresult;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return stdresult;
			}
			break;
	}
	return stdresult;
}	/* EditControlProcedure */


/*********************************************************************************************
	SimpleEditControlProcedure. 
	This routine catches all keyboard input and handles it as a standard windows edit control would. 
	The only exception are tab key presses. For this keyboard input SimpleEditControlProcedure first 
	checks if it is inside a Dialog. If so, then the keyfocus is advanced to the next
	control.
	SetFocus/KillFocus events are passed to Clean as Activate and Deactivate events. 
	All other events are handled as a standard windows edit control. 
*********************************************************************************************/
static LRESULT CALLBACK SimpleEditControlProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	LRESULT stdresult;

	printMessage ("Clean Simple Edit Control", hwnd, uMess, wParam, lParam);

	/* First preprocess all messages as if you are a standard Windows edit control. */
	stdresult = CallWindowProc ((WNDPROC) stdEditCallback, hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
//		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;

				c = CheckVirtualKeyCode ((int) wParam);

				/* Check if the keyboard input is a tab key. If we're in a Dialog then move focus to next control. */
				if (c==WinTabKey)
				{
					hwndParent = GetControlParentDialog (hwnd);
					if (hwndParent!=NULL)	/* The EditControl is in a Dialog */
					{
						int modifiers;
						BOOL bPrevious;
						HWND hwndNextCtrl;

						modifiers = GetModifiers ();
						bPrevious = (BOOL) modifiers & SHIFTBIT;
						hwndNextCtrl = GetNextDlgTabItem (hwndParent,hwnd,bPrevious);

						if (hwndNextCtrl != hwnd)		/* You're not the only focusable item. */
						{
							SetFocus (hwndNextCtrl);	/* Advance key focus to next control. */
						}
					}
				}
				return stdresult;
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndParent = GetControlParent (hwnd);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwnd, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to
					Clean (because of the ControlDeactivate attribute). 
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwnd);
				return stdresult;
			}
			break;
		case WM_SETFOCUS:
			{
				/*	WM_SETFOCUS sends the CcWmSETFOCUS message to Clean because
					of the ControlActivate attribute.
				*/
				SendMessage2ToClean (CcWmSETFOCUS, GetControlParent (hwnd), hwnd);
				return stdresult;
			}
			break;
	}
	return stdresult;
}	/* SimpleEditControlProcedure */


/*********************************************************************************************
	Event handler for subclassing editable pop up controls. 
	This routine catches all keyboard input and handles it as a standard windows pop up control 
	would, but in addition it also sends each keyboard input to Clean.
	All other events are handled as a standard windows pop up control. 
*********************************************************************************************/
static LRESULT CALLBACK PopUpControlProcedure (HWND hwnd,UINT uMess,WPARAM wParam,LPARAM lParam)
{
	LRESULT stdresult;

	printMessage ("Clean PopUp Control", hwnd, uMess, wParam, lParam);

	/* First preprocess all messages as if you are a standard Windows edit control of a combobox. */
	stdresult = CallWindowProc ((WNDPROC) stdPopUpCallback, hwnd, uMess, wParam, lParam);

	switch (uMess)
	{
//		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = 0;
				HWND hwndParent;
				HWND hwndCombo;

				c = CheckVirtualKeyCode ((int) wParam);

				if (!c)
				/* Ignore non-virtual keys, because they arrive as WM_SYSCHAR and WM_CHAR. */
				{
					return stdresult;
				}
				/* Handle virtual keys analogously to keys received as WM_SYSCHAR and WM_CHAR. */
				hwndCombo  = GetParent (hwnd);
				hwndParent = GetControlParent (hwndCombo);
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hwndParent, hwndCombo, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
//		case WM_SYSCHAR:
		case WM_CHAR:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);

				if (gInKey)
				{
					if (gCurChar == (int) wParam)
						SendKeyStillDownToClean (hwndParent, hwndCombo, gCurChar);
					else
					{
						SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
						gCurChar = wParam;
						SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					}
				}
				else
				{
					gCurChar = wParam;
					SendKeyDownToClean (hwndParent, hwndCombo, gCurChar);
					gInKey = TRUE;
				}
				return stdresult;
			}
			break;
//		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return stdresult;
			}
			break;
		case WM_KILLFOCUS:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);
				if (gInKey)
					SendKeyUpToClean (hwndParent, hwndCombo, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				/*	WM_KILLFOCUS now also sends the CcWmKILLFOCUS message to 
					Clean (because of the ControlDeactivate attribute).
				*/
				SendMessage2ToClean (CcWmKILLFOCUS, hwndParent, hwndCombo);
				return stdresult;
			}
			break;
		/*	Send CcWmGETFOCUS message to Clean because of the ControlActivate attribute. 
		*/
		case WM_SETFOCUS:
			{
				HWND hwndCombo  = GetParent (hwnd);
				HWND hwndParent = GetControlParent (hwndCombo);
				SendMessage2ToClean (CcWmSETFOCUS, hwndParent, hwndCombo);
				return stdresult;
			}
			break;
	}

	return stdresult;
}	/* PopUpControlProcedure */


/*********************************************************************************************
	Callback routine for SDI client windows.
	The callback routine handles all events for the client window of a SDI window.
	The accelerator table is now managed by its parent SDI frame window callback routine.
*********************************************************************************************/
static LRESULT CALLBACK SDIWindowProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	printMessage ("Clean SDI Window", hWin, uMess, wPara, lPara);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wPara))
				{
					case 0:		/*	0: message originates from a menu or equals BN_CLICKED */
						{
							if (lPara != 0)		/*	It was BN_CLICKED. */
							{
								/*	Send also modifiers to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hWin, lPara, GetModifiers (), LOWORD (wPara));
							}
							else				/*	It was from a menu. */
							{
								SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
							}
						}
						break;
					case 1:		/*	1: message originates from an accelerator */
						{
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lPara, CB_GETCURSEL, 0, 0);
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lPara;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, hWin, combo, newsel);
								return 1;
							}
						}
						break;
				}
			} break;
		case WM_PAINT:
			{
				RECT updaterect;
				HDC  hdc;
				PAINTSTRUCT ps;
				
				if (GetUpdateRect (hWin, &updaterect, FALSE))
				{
					hdc = BeginPaint (hWin, &ps);
					if (updaterect.left != updaterect.right && updaterect.top != updaterect.bottom)						
						SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,updaterect.top,updaterect.right,updaterect.bottom,hdc);
					EndPaint (hWin, &ps);
				}
				else
				{	
					GetClientRect (hWin, &updaterect);
					SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,
									 updaterect.top,
									 updaterect.right,
									 updaterect.bottom,wPara);
				}
				return 0;		//PAPAPA
			}
			break;
		case WM_SETCURSOR:
			{
				int cursorcode;
				LocalWindowData wdata;

				if (!GlobalCursorSet ())
				{
					if ((HWND) wPara != hWin || LOWORD (lPara) != HTCLIENT)
					{
						return DefWindowProc (hWin, uMess, wPara, lPara);
					}
#ifdef _WIN64
					wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
					wdata = (LocalWindowData) GetWindowLong (hWin,0);
#endif
					cursorcode = wdata->lwd_cursorcode;
				}
				else
				{
					cursorcode = GetGlobalCursorCode ();
				}
				SetCursorFromCode (cursorcode);
			}
			break;
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = CheckVirtualKeyCode ((int) wPara);

				if (!c || (uMess == WM_SYSKEYDOWN && c >= VK_F1 && c <= VK_F12))
				{
					return DefMDIChildProc (hWin,uMess,wPara,lPara);
				}
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hWin, hWin, gCurChar);
					else
					{
						SendKeyUpToClean (hWin, hWin, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hWin, hWin, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hWin, hWin, gCurChar);
					gInKey = TRUE;
				}
				break;
			}
//		case WM_SYSCHAR:
		case WM_CHAR:
			{
				if (gInKey)
				{
					if (gCurChar == (int) wPara)
						SendKeyStillDownToClean (hWin, hWin, gCurChar);
					else
					{
						SendKeyUpToClean (hWin, hWin, gCurChar);
						gCurChar = wPara;
						SendKeyDownToClean (hWin, hWin, gCurChar);
					}
				}
				else
				{
					gCurChar = wPara;
					SendKeyDownToClean (hWin, hWin, gCurChar);
					gInKey = TRUE;
				}
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return (DefWindowProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_KILLFOCUS:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
			}
			break;
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
			}
			break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
				else
				{
					SendMouseStillUpToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
			}
			break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
			}
			break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefWindowProc (hWin, uMess, wPara, lPara);
			}
			break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseUpToClean (hWin, hWin, p.x, p.y);
				}
			}
			break;
		case WM_TIMER:
			{
				if ((int) wPara == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseStillDownToClean (hWin, hWin, p.x, p.y);
				}
				else
				{
					rprintf ("Clean Window did not expect normal WM_TIMER message with id = %d.\n", wPara);
				}
			}
			break;
		/*	The WM_CREATE message should cause the SDI client window to notify Clean that its controls
			can be built. The accelerator table is maintained by the SDI frame window.
			In addition, the LocalWindowData is created and stored in the local memory of the SDI window.
		*/
		case WM_CREATE:
			{
				LocalWindowData wdata;

				wdata = AllocateLocalWindowData ();			// create the LocalWindowData struct
#ifdef _WIN64
				SetWindowLongPtr (hWin, 0, (LONG_PTR) wdata);//	and store it in the local memory of the window
#else
				SetWindowLong (hWin, 0, (long) wdata);		//	and store it in the local memory of the window
#endif

				SendMessage1ToClean (CcWmCREATE, hWin);

				/*	After creation of the window controls, their HFONT should be set to 8pt "MS Sans Serif" */
				EnumChildWindows (hWin, SetControlFontProc, (LPARAM) gControlFont);
			}
			break;
		/*	The WM_DESTROY message should free the local SDI window memory.
		*/
		case WM_DESTROY:
			{
				LocalWindowData wdata;

#ifdef _WIN64
				wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);//	get the local SDI window data
#else
				wdata = (LocalWindowData) GetWindowLong (hWin,0);	//	get the local SDI window data
#endif
				DestroyLocalWindowData (wdata);						//	and destroy it.

				return 0;
			}
			break;
		/*	The cases WM_ENTERSIZEMOVE and WM_EXITSIZEMOVE flag the lwd_usersizemove field
			of the LocalWindowData of the window. This is used to determine whether the window
			should be redrawn in case of resizing. 
		*/
		case WM_ENTERSIZEMOVE:
			{
				LocalWindowData wdata;
#ifdef _WIN64
				wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
				wdata = (LocalWindowData) GetWindowLong (hWin,0);
#endif
				wdata->lwd_usersizemoving = (BOOL)TRUE;
#ifdef _WIN64
				SetWindowLongPtr (hWin, 0, (LONG_PTR)wdata);
#else
				SetWindowLong (hWin, 0, (long)wdata);
#endif
			}
			break;
		case WM_EXITSIZEMOVE:
			{
				LocalWindowData wdata;

#ifdef _WIN64
				wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
				wdata = (LocalWindowData) GetWindowLong (hWin,0);
#endif
				wdata->lwd_usersizemoving = (BOOL)FALSE;
#ifdef _WIN64
				SetWindowLongPtr (hWin, 0, (LONG_PTR)wdata);
#else
				SetWindowLong (hWin, 0, (long)wdata);
#endif
			}
			break;
		/*	The WM_SIZE message informs Clean about the new size.
		*/
		case WM_SIZE:
			{
				if (wPara != SIZE_MAXHIDE && wPara != SIZE_MAXSHOW)
				{
					int width,height;
					LocalWindowData wdata;

					width  = LOWORD (lPara);		// Width  of window excluding vertical scrollbar 
					height = HIWORD (lPara);		// Height of window excluding horizontal scrollbar 
#ifdef _WIN64
					wdata  = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
					wdata  = (LocalWindowData) GetWindowLong (hWin,0);
#endif
					UpdateWindow (hWin);			// But first update the window
					SendMessage4ToClean (CcWmSIZE, hWin, width, height, (int)wdata->lwd_usersizemoving);
				}
			}
			break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the compound control handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the window handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_MOUSEWHEEL:		/* New: react on mouse wheel events. */
			{
				int direction;
				int zDelta = (int)HIWORD(wPara);
				if ((int)wPara > 0)
					direction = 0;			// Wheel moved away from the user.
				else
					direction = 1;			// Wheel moved towards the user. 
				SendMessage3ToClean (CcWmMOUSEWHEEL, hWin, hWin, direction);
				return FALSE;
			}
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lPara;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context */
										lpdis->rcItem.left + 2,		/* ref point x */
										lpdis->rcItem.top + 1,		/* ref point y */
										ETO_CLIPPED | ETO_OPAQUE,	/* options */
										&lpdis->rcItem,				/* clipping rect */
										text,						/* text to draw */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);

						} break;
					case ODT_BUTTON:
						{
							SendMessage3ToClean (CcWmDRAWCONTROL, hWin, lpdis->hwndItem, lpdis->hDC);
							 
							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);
							
							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return TRUE;
						} break;
				}
			} break;
	}
	return DefWindowProc (hWin,uMess,wPara,lPara);
}	/* SDIWindowProcedure */


/*********************************************************************************************
	Callback routine for MDI document window procedure. 
	Copied almost straight from SDIWindowProcedure. Differences are:
	-	WM_COMMAND can not originate from menu selections (handled by MDIFrameProcedure);
	-	DefWindowProc must be DefMDIChildProc;
	-	Instead of WM_ACTIVATE, a MDI document window receives WM_MDIACTIVATE messages. However, 
		these do not work well when dialogs are involved. Instead WM_NCACTIVATE messages are 
		checked.
*********************************************************************************************/
static LRESULT CALLBACK MDIWindowProcedure (HWND hWin,UINT uMess,WPARAM wPara,LPARAM lPara)
{
	printMessage ("Clean MDI Doc Window",hWin,uMess,wPara,lPara);
	switch (uMess)
	{
		case WM_COMMAND:
			{
				switch (HIWORD (wPara))
				{
					case BN_CLICKED:
						{
							if (lPara != 0)
							{
								/*	Send modifiers also to Clean */
								SendMessage4ToClean (CcWmBUTTONCLICKED, hWin, lPara, GetModifiers (), LOWORD (wPara));
							}
						}
						break;
					case 1:		/*	1: message originates from an accelerator */
						{
							SendMessage2ToClean (CcWmCOMMAND, LOWORD (wPara), GetModifiers ());
						}
						break;
					case CBN_SETFOCUS:
						{
							gComboSelection = SendMessage ((HWND) lPara, CB_GETCURSEL, 0, 0);
						}
						break;
					case CBN_KILLFOCUS:
						{
							gComboSelection = -1;
						}
						break;
					case CBN_SELENDOK:
						{
							char text[256];
							int newsel;
							HWND combo;

							combo = (HWND) lPara;
							newsel = SendMessage (combo, CB_GETCURSEL, 0, 0);
							SendMessage (combo, CB_GETLBTEXT, newsel, (LPARAM) text);
							if (!SendMessage (combo, CB_GETITEMDATA, newsel, 0))
							{
								SendMessage (combo, CB_SETCURSEL, gComboSelection, (LPARAM) text);
								MessageBeep (0xFFFFFFFF);
								return 0;
							}
							else
							{
								gComboSelection = newsel;
								if (newsel!=CB_ERR)
									SendMessage3ToClean (CcWmCOMBOSELECT, hWin, combo, newsel);
								return 1;
							}
						}
						break;
				}
			} break;
		case WM_PAINT:
			{
				RECT updaterect;
				HDC  hdc;
				PAINTSTRUCT ps;
				
				if (GetUpdateRect (hWin, &updaterect, FALSE))
				{
					hdc = BeginPaint (hWin, &ps);
					if (updaterect.left != updaterect.right && updaterect.top != updaterect.bottom)
						SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,updaterect.top,updaterect.right,updaterect.bottom,hdc);
					EndPaint (hWin, &ps);
				}
				else
				{	
					GetClientRect (hWin, &updaterect);
					SendMessage6ToClean (CcWmPAINT, hWin, updaterect.left,updaterect.top,updaterect.right,updaterect.bottom,wPara);
				}
				return 0;
			}
		case WM_CLOSE:		/*	The window is requested to be closed. */
			{
				SendMessage1ToClean (CcWmCLOSE, hWin);
				return 0;
			}
		case WM_DESTROY:	/*	The window is in the act of being closed. */
			{
				LocalWindowData wdata;

#ifdef _WIN64
				wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);// Get the local MDI window data
#else
				wdata = (LocalWindowData) GetWindowLong (hWin,0);	// Get the local MDI window data
#endif
				DestroyLocalWindowData (wdata);						//	and destroy it.
				
				ghTopDocWindow=NULL;
				return 0;
			}
		case WM_SETCURSOR:
			{
				int cursorcode;
				LocalWindowData wdata;

				if (!GlobalCursorSet ())
				{
					if ((HWND) wPara != hWin || LOWORD (lPara) != HTCLIENT)
					{
						return DefMDIChildProc (hWin, uMess, wPara, lPara);
					}
#ifdef _WIN64
					wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
					wdata = (LocalWindowData) GetWindowLong (hWin,0);
#endif
					cursorcode = wdata->lwd_cursorcode;
				}
				else
				{
					cursorcode = GetGlobalCursorCode ();
				}
				SetCursorFromCode (cursorcode);
			}
			break;
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			{
				int c = CheckVirtualKeyCode ((int) wPara);

				if (!c || (uMess == WM_SYSKEYDOWN && c >= VK_F1 && c <= VK_F12))
				{
					return DefMDIChildProc (hWin,uMess,wPara,lPara);
				}
				if (gInKey)
				{
					if (gCurChar == c)
						SendKeyStillDownToClean (hWin, hWin, gCurChar);
					else
					{
						SendKeyUpToClean (hWin, hWin, gCurChar);
						gCurChar = c;
						SendKeyDownToClean (hWin, hWin, gCurChar);
					}
				}
				else
				{
					gCurChar = c;
					SendKeyDownToClean (hWin, hWin, gCurChar);
					gInKey = TRUE;
				}
				break;
			}
//		case WM_SYSCHAR:
		case WM_CHAR:
			{
				if (gInKey)
				{
					if (gCurChar == (int) wPara)
						SendKeyStillDownToClean (hWin, hWin, gCurChar);
					else
					{
						SendKeyUpToClean (hWin, hWin, gCurChar);
						gCurChar = wPara;
						SendKeyDownToClean (hWin, hWin, gCurChar);
					}
				}
				else
				{
					gCurChar = wPara;
					SendKeyDownToClean (hWin, hWin, gCurChar);
					gInKey = TRUE;
				}
				return 0;
			}
			break;
		case WM_SYSKEYUP:
		case WM_KEYUP:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
				return (DefMDIChildProc (hWin, uMess, wPara, lPara));
			}
			break;
		case WM_KILLFOCUS:
			{
				if (gInKey)
					SendKeyUpToClean (hWin, hWin, gCurChar);
				gInKey = FALSE;
				gCurChar = 0;
			}
			break;
		case WM_LBUTTONDOWN:
			{
				SendMouseDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				return 0;
			}
			break;
		case WM_MOUSEMOVE:
			{
				if (gInMouseDown)
				{
					SendMouseStillDownToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
				else
				{
					SendMouseStillUpToClean (hWin, hWin, SIGNEDLOWORD (lPara), SIGNEDHIWORD (lPara));
				}
				return 0;
			}
			break;
		case WM_LBUTTONUP:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
			}
			break;
		case WM_CANCELMODE:
			{
				if (gInMouseDown)
				{
					ReleaseCapture ();	/* rely on WM_CAPTURECHANGED to send the mouseUp event */
				}
				return DefMDIChildProc (hWin, uMess, wPara, lPara);
			}
			break;
		case WM_CAPTURECHANGED:
			{
				if (gInMouseDown)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseUpToClean (hWin, hWin, p.x, p.y);
				}
			}
			break;
		case WM_TIMER:
			{
				if ((int) wPara == -1)
				{
					POINT p;
					GetCursorPos (&p);
					ScreenToClient (hWin, &p);
					SendMouseStillDownToClean (hWin, hWin, p.x, p.y);
				}
				else
				{
					rprintf ("Clean Window did not expect normal WM_TIMER message with id = %d.\n", wPara);
				}
			}
			break;
		/*	Instead of using WM_MDIACTIVATE to know the activation state of a MDI document window, 
			we use WM_NCACTIVATE. This is because WM_MDIACTIVATE events are  not generated when dialogs
			are involved. The WM_NCACTIVATE is more robust because it is used by the system to change
			the active state of the window title bar. Incidentally to many WM_NCACTIVATE message can be
			generated. This has to be dealt with on the Clean side.
		*/
		case WM_NCACTIVATE:
			{
				BOOL fActive = (BOOL)wPara;

				if (fActive)
				{
					SendMessage1ToClean (CcWmACTIVATE, hWin);
					ghTopDocWindow       = hWin;
					ghActiveClientWindow = GetParent (hWin);
					ghActiveFrameWindow  = GetParent (ghActiveClientWindow);
					UpdateWindow (hWin);		// enforce update at Clean side (necessary for setActiveWindow)
				}
				else
				{
					SendMessage1ToClean (CcWmDEACTIVATE, hWin);
					ghTopDocWindow       = NULL;
					ghActiveFrameWindow  = NULL;
					ghActiveClientWindow = NULL;
				}
				return DefMDIChildProc (hWin, uMess, wPara, lPara);
			}
			break;
		/*	The WM_CREATE message notifies Clean that its controls can be built. 
			In addition, the LocalWindowData is created and stored in the local memory of the MDI window.
		*/
		case WM_CREATE:
			{
				LocalWindowData wdata;

				wdata = AllocateLocalWindowData ();			// create the LocalWindowData struct
#ifdef _WIN64
				SetWindowLongPtr (hWin, 0, (LONG_PTR)wdata);//	and store it in the local memory of the window
#else
				SetWindowLong (hWin, 0, (long) wdata);		//	and store it in the local memory of the window
#endif
				SendMessage1ToClean (CcWmCREATE, hWin);

				/*	After creation of the window controls, their HFONT should be set to 8pt "MS Sans Serif" */
				EnumChildWindows (hWin, SetControlFontProc, (LPARAM) gControlFont);
			}
			break;
		/*	The cases WM_ENTERSIZEMOVE and WM_EXITSIZEMOVE flag the lwd_usersizemove field
			of the LocalWindowData of the window. This is used to determine whether the window
			should be redrawn in case of resizing. 
		*/
		case WM_ENTERSIZEMOVE:
			{
				LocalWindowData wdata;
#ifdef _WIN64
				wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
				wdata = (LocalWindowData) GetWindowLong (hWin,0);
#endif
				wdata->lwd_usersizemoving = (BOOL)TRUE;
#ifdef _WIN64
				SetWindowLongPtr (hWin, 0, (LONG_PTR)wdata);
#else
				SetWindowLong (hWin, 0, (long)wdata);
#endif
			}
			break;
		case WM_EXITSIZEMOVE:
			{
				LocalWindowData wdata;
#ifdef _WIN64
				wdata = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
				wdata = (LocalWindowData) GetWindowLong (hWin,0);
#endif
				wdata->lwd_usersizemoving = (BOOL)FALSE;
#ifdef _WIN64
				SetWindowLongPtr (hWin, 0, (LONG_PTR)wdata);
#else
				SetWindowLong (hWin, 0, (long)wdata);
#endif
			}
			break;
		case WM_SIZE:
			{
				HWND hwndToolbar;
				int width,height;
				LocalWindowData wdata;

#ifdef _WIN64
				hwndToolbar = (HWND) GetGWLP_USERDATA (hWin);
#else
				hwndToolbar = (HWND) GetGWL_USERDATA (hWin);
#endif
				// First resize the toolbar if present
				if (hwndToolbar!=NULL)
					SendMessage (hwndToolbar, TB_AUTOSIZE, (WPARAM)0, (LPARAM)0);

				if (wPara != SIZE_MAXHIDE && wPara != SIZE_MAXSHOW)
				{
					width  = LOWORD (lPara);		// Width  of window excluding vertical scrollbar
					height = HIWORD (lPara);		// Height of window excluding horizontal scrollbar
#ifdef _WIN64
					wdata  = (LocalWindowData) GetWindowLongPtr (hWin,0);
#else
					wdata  = (LocalWindowData) GetWindowLong (hWin,0);
#endif
					UpdateWindow (hWin);			// But first update the window
					SendMessage4ToClean (CcWmSIZE, hWin, width, height, (int)wdata->lwd_usersizemoving);
				}
			}
			break;
		case WM_HSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_HORZ;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the window handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_VSCROLL:
			{
				int nPos,nScrollCode,controlkind;
				HWND hwndScrollBar;

				nScrollCode = LOWORD (wPara);

				if (nScrollCode != SB_ENDSCROLL)	/* Do not send the SB_ENDSCROLL to Clean. */
				{
					nPos = (short int) HIWORD (wPara);
					hwndScrollBar = (HWND) lPara;

					if (hwndScrollBar==0)
					{
						controlkind = SB_VERT;		/* lPara==0 in case of Window scrollbars. */
						hwndScrollBar = hWin;		/* pass the window handle to Clean. */
						UpdateWindow (hWin);		/* but first ensure that window is updated. */
					}
					else
					{
						controlkind = SB_CTL;		/* lPara!=0 in case of SliderControls. */
					}
					SendMessage5ToClean (CcWmSCROLLBARACTION, hWin, hwndScrollBar, controlkind, nScrollCode, nPos);
				}
			}
			break;
		case WM_MOUSEWHEEL:		/* New: react on mouse wheel events. */
			{	
				int direction;
				int zDelta = (int)HIWORD(wPara);
				if ((int)wPara > 0)
					direction = 0;			// Wheel moved away from the user.
				else
					direction = 1;			// Wheel moved towards the user. 
				SendMessage3ToClean (CcWmMOUSEWHEEL, hWin, hWin, direction);
				return FALSE;
			}
			break;
		case WM_DRAWITEM:
			{
				LPDRAWITEMSTRUCT lpdis;
				lpdis = (LPDRAWITEMSTRUCT) lPara;

				switch (lpdis->CtlType)
				{
					case ODT_COMBOBOX:
						{
							char text[256];
							COLORREF forecolor, bkcolor;
							SendMessage (lpdis->hwndItem, CB_GETLBTEXT, lpdis->itemID, (LPARAM) text);
							if (lpdis->itemState & ODS_DISABLED)
							{
								forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_3DFACE));
							}
							else if (lpdis->itemState & ODS_SELECTED)
							{
								if (lpdis->itemData)
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHTTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_HIGHLIGHT));
								}
								else
								{
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
									bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
								}
							}
							else
							{
								if (lpdis->itemData)
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_WINDOWTEXT));
								else
									forecolor = SetTextColor (lpdis->hDC, GetSysColor (COLOR_GRAYTEXT));
								bkcolor = SetBkColor (lpdis->hDC, GetSysColor (COLOR_WINDOW));
							}

							ExtTextOut (lpdis->hDC,					/* device context */
										lpdis->rcItem.left + 2,		/* ref point x */
										lpdis->rcItem.top + 1,		/* ref point y */
										ETO_CLIPPED | ETO_OPAQUE,	/* options */
										&lpdis->rcItem,				/* clipping rect */
										text,						/* text to draw */
										lstrlen (text),				/* length of text to draw */
										NULL						/* no kerning array */
								);

							SetTextColor (lpdis->hDC, forecolor);
							SetBkColor (lpdis->hDC, bkcolor);

							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);

						} break;
					case ODT_BUTTON:
						{
							SendMessage3ToClean (CcWmDRAWCONTROL, hWin, lpdis->hwndItem, lpdis->hDC);
							 
							if (lpdis->itemState & ODS_SELECTED)
								InvertRect (lpdis->hDC, &lpdis->rcItem);
							
							if (lpdis->itemState & ODS_FOCUS)
								DrawFocusRect (lpdis->hDC, &lpdis->rcItem);
							return TRUE;
						} break;
				}
			} break;
	}
	return DefMDIChildProc (hWin,uMess,wPara,lPara);
}	/* MDIWindowProcedure */


/*********************************************************************************************
	Cross call procedure implementations.
	Eval<nr> corresponds with a CrossCallEntry generated by NewCrossCallEntry (nr,Eval<nr>).
*********************************************************************************************/
void EvalCcRqBEGINPAINT (CrossCallInfo *pcci)	/* hwnd; HDC result. */
{
	HDC hdc;
	hdc = BeginPaint ((HWND) pcci->p1, &gPaintStruct);
	MakeReturn1Cci (pcci, (size_t) hdc);
}

void EvalCcRqENDPAINT (CrossCallInfo *pcci)		/* hwnd; no result.  */
{
	EndPaint ((HWND) pcci->p1, &gPaintStruct);
	MakeReturn0Cci (pcci);
}

void EvalCcRqFAKEPAINT (CrossCallInfo *pcci)		/* hwnd; no result. */
{
	HWND hwnd = (HWND) pcci->p1;

	BeginPaint (hwnd, &gPaintStruct);
	EndPaint (hwnd,&gPaintStruct);
	InvalidateRect (hwnd, NULL, FALSE);

	MakeReturn0Cci (pcci);
}

void EvalCcRqDESTROYMODALDIALOG (CrossCallInfo *pcci) /* hwnd; no result. */
{
	EndDialog ((HWND)pcci->p1,0);
	ghwndLastModalDialog = NULL;	/*	Create only one zero timer for modal dialog stack. */
	MakeReturn0Cci (pcci);
}

void EvalCcRqDESTROYMDIDOCWINDOW (CrossCallInfo *pcci)	/* hwndFrame, hwndClient, wPtr; no result. */
{
	HWND hwndFrame, hwndClient, hwndDocWindow, newActiveDocWindow;

	hwndFrame     = (HWND) pcci->p1;
	hwndClient    = (HWND) pcci->p2;
	hwndDocWindow = (HWND) pcci->p3;

	// First destroy the given MDI document window. 
	SendMessage (hwndClient, WM_MDIDESTROY, (WPARAM) hwndDocWindow, 0);

	// Obtain the new active MDI document window.
	newActiveDocWindow = (HWND) SendMessage (hwndClient, WM_MDIGETACTIVE, (WPARAM) 0, (LPARAM) NULL);
	if (newActiveDocWindow==NULL)			// If all document windows have been closed
	{
		ghActiveFrameWindow  = hwndFrame;	// then keep track of the MDI frame window
		ghActiveClientWindow = hwndClient;	// and client window
	}

	MakeReturn0Cci (pcci);
}

/*	Create a SDI document window. */
void EvalCcRqCREATESDIDOCWINDOW (CrossCallInfo *pcci)	/* textptr, frameptr, packed pos, w,h, flags; client ptr result. */
{
	HWND    hwndFrame, hwndClient, hwndToolbar;
	POINT   clientDims, frameDims, winpos;
	LPCTSTR pwintitle;
	DWORD   styleFlags;
	RECT    tbRect;
	int     tbHeight;

	pwintitle    = (LPCTSTR) pcci->p1;
	hwndFrame    = (HWND) pcci->p2;
	winpos.x     = pcci->p3>>16;
	winpos.y     = (SHORT)pcci->p3;
	clientDims.x = pcci->p4;
	clientDims.y = pcci->p5;
	styleFlags   = (DWORD) pcci->p6;

#ifdef _WIN64
	hwndToolbar  = (HWND)GetGWLP_USERDATA (hwndFrame);
#else
	hwndToolbar  = (HWND)GetGWL_USERDATA (hwndFrame);
#endif
	if (hwndToolbar == NULL || !GetWindowRect (hwndToolbar,&tbRect))
	{
		tbHeight = 0;
	}
	else
	{
		tbHeight = tbRect.bottom - tbRect.top;
	}

	frameDims.x = clientDims.x;
	frameDims.y = clientDims.y + tbHeight;
	W95AdjustCleanSDIWindowDimensions (styleFlags, &frameDims);

	/* Adjust the pos and size of the frame window. */
	SetWindowPos (hwndFrame,NULL,winpos.x,winpos.y,frameDims.x,frameDims.y,SWP_NOZORDER);
	UpdateWindow (hwndFrame);
	SetWindowText (hwndFrame,pwintitle);

	/* The client style flags are WS_CHILD and styleFlags. */
	styleFlags |= WS_CHILD;

	/* Create the new client window of the frame window. */
	hwndClient = CreateWindow  (SDIWindowClassName,			/* Class name						*/
								pwintitle,					/* title							*/
								styleFlags,					/* SDI client style flags			*/
								0, tbHeight,				/* x, y								*/
								clientDims.x,clientDims.y,	/* width, height					*/
								hwndFrame,					/* Parent window					*/
								(HMENU) NULL,				/* no menu handle					*/
								(HANDLE) ghInst,			/* Instance that owns the window	*/
								0);
	SetWindowPos (hwndClient,HWND_TOP,0,0,0,0,SWP_NOMOVE | SWP_NOSIZE);
	ShowWindow (hwndClient,SW_SHOWNORMAL);
	UpdateWindow (hwndClient);
	UpdateWindowScrollbars (hwndClient);

	MakeReturn1Cci (pcci, (size_t) hwndClient);
}

/*	Create MDI child window. */
void EvalCcRqCREATEMDIDOCWINDOW (CrossCallInfo *pcci)		/* textptr, clientPtr, behindPtr, packed pos, packed size, flags; HWND result. */
{
	HWND    whandle,hwndClient,hwndBehind;
	POINT   dims, winpos;
	LPCTSTR pwintitle;
	DWORD   styleFlags, exStyleFlags;
	MDICREATESTRUCT mdicreate;		// The structure sent to the client window

	pwintitle    = (LPCTSTR) pcci->p1;
	hwndClient   = (HWND) pcci->p2;
	hwndBehind   = (HWND) pcci->p3;
	winpos.x     = pcci->p4>>16;
	winpos.y     = (SHORT)pcci->p4;
	dims.x       = pcci->p5>>16;
	dims.y       = (SHORT)pcci->p5;
	styleFlags   = (DWORD) pcci->p6;
	exStyleFlags = (DWORD) 0;

	W95AdjustCleanMDIWindowDimensions (styleFlags, &dims);

	if (styleFlags&WS_THICKFRAME != 0)				/* If resizable*/
	{
		exStyleFlags |= WS_EX_CLIENTEDGE;			/* then provide a 3D look to the window. */
	}

	/* fill the MDICREATESTRUCT record */
	mdicreate.szClass = MDIWindowClassName;
	mdicreate.szTitle = pwintitle;
	mdicreate.hOwner  = (HANDLE) ghInst;
	mdicreate.x       = winpos.x;
	mdicreate.y       = winpos.y;
	mdicreate.cx      = dims.x;
	mdicreate.cy      = dims.y;
	mdicreate.style   = styleFlags;
	mdicreate.lParam  = 0;

	/* create the window */
	whandle = (HWND) SendMessage (hwndClient,WM_MDICREATE,0,(LPARAM)(LPMDICREATESTRUCT) &mdicreate);

	/* take care of window stacking */
	if (hwndBehind!=0)
	{
		SetWindowPos (whandle, hwndBehind, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
	}

	MakeReturn1Cci (pcci, (size_t) whandle);
}

void EvalCcRqSETWINDOWTITLE (CrossCallInfo *pcci)		/* hwnd, textptr		no result. */
{
	SetWindowText ((HWND) pcci->p1, (LPCTSTR) pcci->p2);
	MakeReturn0Cci (pcci);
}

void EvalCcRqGETWINDOWTEXT (CrossCallInfo *pcci) /* hwnd;  textptr result. */
{
	char *textptr;
	HWND hwnd;
	int length;

	hwnd = (HWND) pcci->p1;
	length = GetWindowTextLength (hwnd);

	textptr = (char *) rmalloc (length + 1);
	GetWindowText (hwnd, textptr, length + 1);
	textptr[length] = 0;

	MakeReturn1Cci (pcci, (size_t) textptr);
}

/*	Update rect part of a window. */
void EvalCcRqUPDATEWINDOWRECT (CrossCallInfo *pcci) /* hwnd, left,top,right,bottom; no result. */
{
	RECT rect;
	HWND hwnd;

	hwnd       = (HWND) pcci->p1;
	rect.left  = pcci->p2;
	rect.top   = pcci->p3;
	rect.right = pcci->p4;
	rect.bottom= pcci->p5;

	InvalidateRect (hwnd,&rect,FALSE);
	UpdateWindow (hwnd);
	RedrawWindow (hwnd,&rect,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);

	MakeReturn0Cci (pcci);
}

/*	Set the ClientRect. */
void EvalCcRqSETCLIENTSIZE (CrossCallInfo *pcci) /* hwnd, width, height; no result. */
{
	HWND hwnd;
	int w,h,curw,curh,clientw,clienth;
	UINT flags;
	RECT clientRect,windowRect;

	hwnd  = (HWND) pcci->p1;
	w     = pcci->p2;
	h     = pcci->p3;
	flags = SWP_NOMOVE			/* retain position */
		  | SWP_NOZORDER;		/* retain Z order */

	GetClientRect (hwnd, &clientRect);
	GetWindowRect (hwnd, &windowRect);
	clientw = clientRect.right - clientRect.left;
	clienth = clientRect.bottom- clientRect.top;
	curw    = windowRect.right - windowRect.left;
	curh    = windowRect.bottom- windowRect.top;

	SetWindowPos (hwnd, HWND_TOP, 0,0, curw+w-clientw,curh+h-clienth, flags);
	MakeReturn0Cci (pcci);
}

/*	(En/Dis)able windows/dialogues. */
void EvalCcRqSETSELECTWINDOW (CrossCallInfo *pcci)	/* hwnd, hasHScroll, hasVScroll, toAble, modalContext; no result. */
{
	HWND window;
	BOOL hasHScroll, hasVScroll, toAble, modalContext;

	window       = (HWND) pcci->p1;
	hasHScroll   = (BOOL) pcci->p2;
	hasVScroll   = (BOOL) pcci->p3;
	toAble       = (BOOL) pcci->p4;
	modalContext = (BOOL) pcci->p5;

	if (modalContext)					/*	if not a modal context, then do not disable window	*/
		EnableWindow (window,toAble);	/*  because it can't be moved, or closed. */
	if (hasHScroll)
		EnableScrollBar (window,SB_HORZ,toAble ? ESB_ENABLE_BOTH : ESB_DISABLE_BOTH);
	if (hasVScroll)
		EnableScrollBar (window,SB_VERT,toAble ? ESB_ENABLE_BOTH : ESB_DISABLE_BOTH);

	MakeReturn0Cci (pcci);
}

/*	Set the position of windows/controls. */
void EvalCcRqSETWINDOWPOS (CrossCallInfo *pcci)	/* hwnd, x,y, update, include scrollbars ; no result. */
{
	HWND hwnd;
	int x,y;
	BOOL update,inclScrollbars;
	UINT flags;

	hwnd           = (HWND)pcci->p1;
	x              = pcci->p2;
	y              = pcci->p3;
	update         = pcci->p4;
	inclScrollbars = pcci->p5;
	flags          = SWP_NOSIZE			/* retain size */
				   | SWP_NOZORDER;		/* retain Z order */

	SetWindowPos (hwnd, HWND_TOP, x,y, 0,0, flags);
	if (IsWindowVisible (hwnd) && update!=0)
	{	/* only if window is visible and update is requested, proceed to enforce update. */
		if (inclScrollbars)
			UpdateWindowScrollbars (hwnd);
		else
		{
			InvalidateRect (hwnd,NULL,TRUE);
			UpdateWindow (hwnd);
			RedrawWindow (hwnd,NULL,NULL,RDW_FRAME | RDW_VALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN);
		}
	}

	MakeReturn0Cci (pcci);
}

/*	Get the size of the bounding rectangle of windows/controls. */
void EvalCcRqGETWINDOWSIZE (CrossCallInfo *pcci) /* hwnd; width,height result. */
{
	RECT rect;

	GetWindowRect ((HWND) pcci->p1, &rect);

	MakeReturn2Cci (pcci, rect.right-rect.left, rect.bottom-rect.top);
}

/*	Set the size of windows/controls. */
void EvalCcRqSETWINDOWSIZE (CrossCallInfo *pcci) /* hwnd, w,h, update; no result. */
{
	HWND hwnd;
	int w,h;
	BOOL update;
	UINT flags;

	hwnd   = (HWND)pcci->p1;
	w      = pcci->p2;
	h      = pcci->p3;
	update = pcci->p4;
	flags  = SWP_NOMOVE			/* retain position */
		   | SWP_NOZORDER;		/* retain Z order  */
	/* flags do not contain SWP_NOREDRAW because otherwise not all windows get updated properly. */

	SetWindowPos (hwnd, HWND_TOP, 0,0, w,h, flags);
	if (update!=0)				/* still, updates are not sufficient using SetWindowPos only. */
		UpdateWindowScrollbars (hwnd);

	MakeReturn0Cci (pcci);
}

/*	Activate control. */
void EvalCcRqACTIVATECONTROL (CrossCallInfo *pcci)	/* controlPtr; no result. */
{
	SetFocus ((HWND) pcci->p1);

	MakeReturn0Cci (pcci);
}

/*	Activate window. */
void EvalCcRqACTIVATEWINDOW (CrossCallInfo *pcci)	/* isMDI, clientPtr, thisWindow; no result. */
{
	BOOL isMDI;
	HWND clientPtr, thisWindow;

	isMDI       = (BOOL) pcci->p1;
	clientPtr   = (HWND) pcci->p2;
	thisWindow  = (HWND) pcci->p3;

	if (isMDI)
	{
		SendMessage (clientPtr,WM_MDIACTIVATE,(WPARAM)thisWindow,(LPARAM)0);
	}
	else
	{
		SetActiveWindow (thisWindow);
	}
	MakeReturn0Cci (pcci);
}

void EvalCcRqCHANGEWINDOWCURSOR (CrossCallInfo *pcci)	/* hwnd, cursor code; no result. It is assumed that the hwnd argument	*/
														/* corresponds to either a SDI/MDI window (and not frame).				*/
{
	HWND hwnd;
	POINT p;
	int cursorcode;
	LocalWindowData wdata;

	hwnd = (HWND) pcci->p1;
	cursorcode = pcci->p2;

	GetCursorPos (&p);

	if (hwnd == WindowFromPoint (p) &&
		SendMessage (hwnd, WM_NCHITTEST, 0, MAKELPARAM (p.x, p.y)) == HTCLIENT)
	{
		SetCursorFromCode (cursorcode);
	}

#ifdef _WIN64
	wdata = (LocalWindowData) GetWindowLongPtr (hwnd,0);
#else
	wdata = (LocalWindowData) GetWindowLong (hwnd,0);
#endif
	wdata->lwd_cursorcode = cursorcode;
#ifdef _WIN64
	SetWindowLongPtr (hwnd, 0, (LONG_PTR)wdata);
#else
	SetWindowLong (hwnd, 0, (long)wdata);
#endif
	MakeReturn0Cci (pcci);
}

void EvalCcRqOBSCURECURSOR (CrossCallInfo *pcci) /* no params; no result. */
{
	SetCursor (GetHiddenCursor ());
	MakeReturn0Cci (pcci);
}

/*	Set range of scrollbars. */
void EvalCcRqSETSCROLLRANGE (CrossCallInfo *pcci)	/* hwnd, iBar, min, max, redraw, no result */
{
	HWND hwnd;
	int min, max, iBar;
	BOOL redraw;

	hwnd = (HWND) pcci->p1;
	iBar = pcci->p2;
	min  = pcci->p3;
	max  = pcci->p4;
	redraw = (BOOL) pcci->p5;

	SetScrollRange (hwnd, iBar, min, max, redraw);

	MakeReturn0Cci (pcci);
}

/*	Set pos of scrollbars. */
void EvalCcRqSETSCROLLPOS (CrossCallInfo *pcci)	/* hwnd, iBar, thumb, maxx, maxy, extent, no result */
{
	HWND hwnd;
	int thumb, iBar, maxx, maxy, extent;
	SCROLLINFO sif;
	RECT scrollBarRect;
	BOOL noredraw;

	hwnd   = (HWND) pcci->p1;
	iBar   = pcci->p2;
	thumb  = pcci->p3;
	maxx   = pcci->p4;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
	maxy   = pcci->p5;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
	extent = pcci->p6;		// extent is the width (height) of the vertical (horizontal) scrollbar
	
	sif.cbSize = sizeof (SCROLLINFO);
	sif.fMask  = SIF_POS;
	sif.nPos   = thumb;

	SetScrollInfo (hwnd, iBar, &sif, TRUE);	// Setting TRUE here to force redraw is not sufficient.
	noredraw = maxx==0 && maxy==0 && extent==0;
	if (!noredraw)	// if the scrollbar does not need to be updated then all values must be zero
	{
		if (iBar==SB_HORZ)
		{
			scrollBarRect.left   = 0;
			scrollBarRect.top    = maxy-extent;
			scrollBarRect.right  = maxx;
			scrollBarRect.bottom = maxy;
		}
		else
		{
			scrollBarRect.left   = maxx-extent;
			scrollBarRect.top    = 0;
			scrollBarRect.right  = maxx;
			scrollBarRect.bottom = maxy;
		}
		InvalidateRect (hwnd, &scrollBarRect, FALSE);
	//	UpdateWindow (hwnd);
	// PA: use RedrawWindow to immediately redraw the indicated scrollbar instead of update mechanism.
		if (!RedrawWindow (hwnd,&scrollBarRect,NULL,RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN))
			rMessageBox (NULL,MB_APPLMODAL,"CcRqSETSCROLLPOS","RedrawWindow failed");
		ValidateRect (hwnd, &scrollBarRect);
	}

	MakeReturn0Cci (pcci);
}

/*	Set thumb size of scrollbars. */
void EvalCcRqSETSCROLLSIZE (CrossCallInfo *pcci)	/* hwnd, iBar, size, maxx, maxy, extent, no result */
{
	HWND hwnd;
	int size, iBar, maxx, maxy, extent;
	SCROLLINFO sif;
	RECT scrollBarRect;
	BOOL noredraw;

	hwnd   = (HWND) pcci->p1;
	iBar   = pcci->p2;
	size   = pcci->p3;
	maxx   = pcci->p4;		// maxx   is the right-most  x coordinate of the enclosing rectangle of the scrollbar
	maxy   = pcci->p5;		// maxy   is the bottom-most y coordinate of the enclosing rectangle of the scrollbar
	extent = pcci->p6;		// extent is the width (height) of the vertical (horizontal) scrollbar
	
	sif.cbSize = sizeof (SCROLLINFO);
	sif.fMask = SIF_PAGE;
	sif.nPage = size;

	SetScrollInfo (hwnd, iBar, &sif, TRUE);	// Setting TRUE here to force redraw is not sufficient.
	noredraw = maxx==0 && maxy==0 && extent==0;
	if (!noredraw)	// if the scrollbar does not need to be updated then all values must be zero
	{
		if (iBar==SB_HORZ)
		{
			scrollBarRect.left   = 0;
			scrollBarRect.top    = maxy-extent;
			scrollBarRect.right  = maxx;
			scrollBarRect.bottom = maxy;
		}
		else
		{
			scrollBarRect.left   = maxx-extent;
			scrollBarRect.top    = 0;
			scrollBarRect.right  = maxx;
			scrollBarRect.bottom = maxy;
		}
		InvalidateRect (hwnd, &scrollBarRect, FALSE);
	//	UpdateWindow (hwnd);
	// PA: use RedrawWindow to immediately redraw the indicated scrollbar instead of update mechanism.
		if (!RedrawWindow (hwnd,&scrollBarRect,NULL,RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN))
			rMessageBox (NULL,MB_APPLMODAL,"CcRqSETSCROLLSIZE","RedrawWindow failed");
		ValidateRect (hwnd, &scrollBarRect);
	}

	MakeReturn0Cci (pcci);
}

/*	Set selection of edit controls. */
void EvalCcRqSETEDITSELECTION (CrossCallInfo *pcci)	/* hwnd, first, last, no result. */
{
	HWND hwnd;
	int first,last;

	hwnd  = (HWND) pcci->p1;
	first = pcci->p2;
	last  = pcci->p3;

	SendMessage (hwnd, EM_SETSEL, (WPARAM) first, (LPARAM) last);		/* Set the selection of the edit control. */
	SendMessage (hwnd, EM_SCROLLCARET, 0,0);		/* Let the caret be displayed - (w/l)Param MUST be 0. */

	MakeReturn0Cci (pcci);
}

/*	EvalCcRqCREATEDIALOG is now restricted to modeless dialogues only. */
void EvalCcRqCREATEDIALOG (CrossCallInfo *pcci)	// textptr,parentptr,behindPtr; HWND result. 
{
	WORD *p, *pdlgtemplate;
	int nchar;
	DWORD lStyle;
	HWND hwnd,hwndParent,hwndBehind;

	hwndParent = (HWND) pcci->p2;	// The owner window    
	hwndBehind = (HWND) pcci->p3;	// The stacking window 

	// allocate some memory to play with  
	pdlgtemplate = p = (PWORD) rmalloc (1000);

	// start to fill in the dlgtemplate information. Addressing by WORDs 
	lStyle = WS_CAPTION | DS_SETFONT | DS_MODALFRAME | WS_SYSMENU | WS_OVERLAPPED;

	*p++ = LOWORD (lStyle);
	*p++ = HIWORD (lStyle);
	*p++ = 0;		// LOWORD (lExtendedStyle) 
	*p++ = 0;		// HIWORD (lExtendedStyle) 
	*p++ = 0;		// NumberOfItems 
	*p++ = 0;		// x  (dummy value) 
	*p++ = 0;		// y  (dummy value) 
	*p++ = 1000;	// cx (dummy value) 
	*p++ = 1000;	// cy (dummy value) 
	*p++ = 0;		// Menu 
	*p++ = 0;		// Class 

	// copy the title of the dialog 
	nchar = nCopyAnsiToWideChar (p, (char *) pcci->p1);
	p += nchar;
	// Font information because of DS_SETFONT 
	*p++ = 8;		// point size 
	nchar = nCopyAnsiToWideChar (p, "MS Sans Serif");		// Face name 
	p += nchar;
	// make sure the first item starts on a DWORD boundary 

	hwnd = CreateDialogIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwndParent, (DLGPROC) DialogProcedure, (LPARAM) 0);
	LocalFree (LocalHandle (pdlgtemplate));

	if (hwndBehind!=NULL)									// In case the dialog should not be opened top-most: 
	{
		UINT uflags = SWP_NOMOVE + SWP_NOSIZE;				// then restack the dialog. 
		SetWindowPos (hwnd,hwndBehind, 0,0,0,0, uflags);
	}
	ShowWindow (hwnd, SW_SHOWNORMAL);						// Only now the dialog should be made visible. 
	UpdateWindow (hwnd);

	MakeReturn1Cci (pcci, (size_t) hwnd);
}

//	Create modal dialogues.
void EvalCcRqCREATEMODALDIALOG (CrossCallInfo *pcci)	/* textptr,parentptr; error code result. */
{
	WORD *p, *pdlgtemplate;
	int nchar, errorcode;
	DWORD lStyle;
	HWND hwndParent;
	UINT zeroTimer;

	hwndParent = (HWND) pcci->p2;	/* The owner window    */
	if (hwndParent == 0)
		hwndParent = ghMainWindow;	/* If no parent is passed (NDI) then ghMainWindow should be the parent. */

	/* allocate some memory to play with  */
	pdlgtemplate = p = (PWORD) rmalloc (1000);

	/* start to fill in the dlgtemplate information. Addressing by WORDs */
//	lStyle = WS_CAPTION | DS_SETFONT | DS_MODALFRAME | WS_SYSMENU | DS_SYSMODAL | WS_VISIBLE;	PA: DS_SYSMODAL has become obsolete
	lStyle = WS_CAPTION | DS_SETFONT | DS_MODALFRAME | WS_SYSMENU | WS_VISIBLE;

	*p++ = LOWORD (lStyle);
	*p++ = HIWORD (lStyle);
	*p++ = 0;		/* LOWORD (lExtendedStyle) */
	*p++ = 0;		/* HIWORD (lExtendedStyle) */
	*p++ = 0;		/* NumberOfItems */
	*p++ = 0;		/* x  (dummy value) */
	*p++ = 0;		/* y  (dummy value) */
	*p++ = 1000;	/* cx (dummy value) */
	*p++ = 1000;	/* cy (dummy value) */
	*p++ = 0;		/* Menu */
	*p++ = 0;		/* Class */

	/* copy the title of the dialog */
	nchar = nCopyAnsiToWideChar (p, (char *) pcci->p1);
	p += nchar;
	/* Font information because of DS_SETFONT */
	*p++ = 8;		/* point size */
	nchar = nCopyAnsiToWideChar (p, "MS Sans Serif");		/* Face name */
	p += nchar;
	/* make sure the first item starts on a DWORD boundary */

	/*	Before the modal dialog enters its event loop we create a timer to generate timer events. 
		This is done only once per modal dialog stack.
	*/
	if (ghwndLastModalDialog == NULL)	// The first modal dialog. 
	{
		zeroTimer = SetTimer (NULL, (UINT) 0, (UINT) 0, NULL);
		
		if (zeroTimer==0)				// Creation of zero timer failed. 
		{
			LocalFree (LocalHandle (pdlgtemplate));
			errorcode = 1;				// Report error to Clean side
		}
		else
		{
			DialogBoxIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwndParent, (DLGPROC) DialogProcedure, (LPARAM) 0);
			LocalFree (LocalHandle (pdlgtemplate));
			KillTimer (NULL, (UINT) zeroTimer);
			errorcode = 0;				// Report no error to Clean side
		}
	}
	else	// a zero timer has already been created. 
	{
		DialogBoxIndirectParam (ghInst, (LPDLGTEMPLATE) pdlgtemplate, hwndParent, (DLGPROC) DialogProcedure, (LPARAM) 0);
		LocalFree (LocalHandle (pdlgtemplate));
		errorcode = 0;					// Report no error to Clean side
	}

	MakeReturn1Cci (pcci,errorcode);
}

/*	Create compound controls (window in window) */
void EvalCcRqCREATECOMPOUND (CrossCallInfo *pcci)	/* hwnd, packed pos,w,h, scrollbars, transparent; HWND result. */
{
	HWND parentwindow, compoundhandle;
	int left,top, width,height;
	int compoundstyle;
	BOOL transparent;
	DWORD compoundExStyle;

	parentwindow  = (HWND) pcci->p1;
	left          = pcci->p2>>16;
	top           = (SHORT)pcci->p2;
	width         = pcci->p3;
	height        = pcci->p4;
	compoundstyle = pcci->p5;
	transparent   = (BOOL) pcci->p6;

	compoundExStyle = WS_EX_CONTROLPARENT;
	if (transparent)
		 compoundExStyle |= WS_EX_TRANSPARENT;

	compoundstyle |= WS_CHILD;// | WS_CLIPSIBLINGS;

	/* create the compound window */
	compoundhandle
		= CreateWindowEx (compoundExStyle,				/* Extended style				 */
						  CompoundControlClassName,		/* Class name					 */
						  "",							/* Window title 				 */
						  compoundstyle,				/* style flags					 */
						  left, top,					/* x, y 						 */
						  width, height,		 		/* width, height				 */
						  parentwindow,					/* Parent window				 */
						  NULL,							/* menu handle					 */
						  (HANDLE) ghInst,				/* Instance that owns the window */
						  0);
	SendMessage  (compoundhandle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (compoundhandle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) compoundhandle);
}

/*	Create scrollbars. */
void EvalCcRqCREATESCROLLBAR (CrossCallInfo *pcci)	/* hwnd, x,y,w,h bool; HWND result. */
{
	HWND scroll;
	int style;
	int x, y, w, h;
	HWND parent;
	BOOL ishorizontal;

	parent = (HWND) pcci->p1;
	x = pcci->p2;
	y = pcci->p3;
	w = pcci->p4;
	h = pcci->p5;
	ishorizontal = pcci->p6;

	style	= WS_CHILD
			| WS_GROUP
			| WS_CLIPSIBLINGS;

	if (ishorizontal)
		style |= SBS_HORZ;
	else
		style |= SBS_VERT;

	scroll = CreateWindow (	"scrollbar",
							"",
							style,
							x, y, w, h,
							parent,
							0,
							ghInst,
							0);
	SendMessage  (scroll, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (scroll, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) scroll);
}

void EvalCcRqCREATEBUTTON (CrossCallInfo *pcci)	/* hwnd, x,y,w,h, kind;  HWND result. */
{
	HWND but, parent;
	int style;
	size_t id;
	int x, y, w, h, kind;

	parent	= (HWND) pcci->p1;
	x		= pcci->p2;
	y		= pcci->p3;
	w		= pcci->p4;
	h		= pcci->p5;
	kind	= pcci->p6;
	style	= WS_CHILD
			| WS_GROUP
			| WS_TABSTOP
			| WS_CLIPSIBLINGS;

	if (kind==ISOKBUTTON)
	{
		style |= BS_DEFPUSHBUTTON;
		id = IDOK;
	}
	else
	{
		style |= BS_PUSHBUTTON;

		if (kind==ISCANCELBUTTON)
		{
			id = IDCANCEL;
		}
		else
		{
			id = 0;
		}
	}

	but = CreateWindow ("button",
						"",
						style,
						x, y, w, h,
						parent,
						(HMENU) id,
						ghInst,
						0);
	SendMessage  (but, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (but, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) but);
}

void EvalCcRqCREATEICONBUT (CrossCallInfo *pcci)	/* hwnd, x,y,w,h,kind; HWND result. */
{
	HWND but, parent;
	int style, x, y, w, h, kind;
	size_t id;

	parent	= (HWND) pcci->p1;
	x		= pcci->p2;
	y		= pcci->p3;
	w		= pcci->p4;
	h		= pcci->p5;
	kind    = pcci->p6;

	style	= WS_CHILD
			| WS_GROUP
			| WS_TABSTOP
			| BS_OWNERDRAW
			| WS_CLIPSIBLINGS;
	
	switch (kind)
	{
		case ISOKBUTTON:
			id = IDOK;
			break;
		case ISCANCELBUTTON:
			id = IDCANCEL;
			break;
		default:
			id = 0;
	}

	but = CreateWindow ("button",
						"", 
						style,
						x, y, w, h,
						parent, 
						(HMENU) id, 
						ghInst, 
						0);
	SendMessage  (but, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (but, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) but);
}

void EvalCcRqCREATECUSTOM (CrossCallInfo *pcci)	/* hwnd, x,y,w,h; HWND result. */
{
	HWND ctrl, parent;
	int style, x, y, w, h;

	parent	= (HWND) pcci->p1;
	x		= pcci->p2;
	y		= pcci->p3;
	w		= pcci->p4;
	h		= pcci->p5;
	style	= WS_CHILD
			| WS_GROUP
			| WS_TABSTOP
			| WS_CLIPSIBLINGS;		// Necessary to enforce proper control stack

	ctrl = CreateWindow (CustomControlClassName,
						 "",
						 style,
						 x, y, w, h,
						 parent,
						 (HMENU) 0,
						 ghInst,
						 0);
	SendMessage  (ctrl, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (ctrl, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) ctrl);
}

void EvalCcRqCREATESTATICTXT (CrossCallInfo *pcci)		/* hwnd, x,y,w,h; HWND result. */
{
	HWND handle;
	int x, y, w, h, style;
	HWND parent;

	parent = (HWND) pcci->p1;
	x = pcci->p2;
	y = pcci->p3;
	w = pcci->p4;
	h = pcci->p5;

	style	= WS_CHILD
			| WS_GROUP
			| SS_LEFTNOWORDWRAP	// PA: SS_LEFT replaced by SS_LEFTNOWORDWRAP because text wrapping is not intended for text controls
			| WS_CLIPSIBLINGS;

	handle = CreateWindow (	"static",
							"",
							style,
							x, y, w, h,
							parent,
							0,
							ghInst,
							0);
	SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) handle);
}

void EvalCcRqCREATEEDITTXT (CrossCallInfo *pcci) /* hwnd, x,y,w,h, flags; HWND result. */
{
	HWND handle;
	int x, y, w, h, style, flags;
	HWND parent;
	BOOL isml,isKeySensitive;

	parent			= (HWND) pcci->p1;
	x				= pcci->p2;
	y				= pcci->p3;
	w				= pcci->p4;
	h				= pcci->p5;
	flags			= pcci->p6;
	isml			= flags & EDITISMULTILINE;
	isKeySensitive	= flags & EDITISKEYSENSITIVE;

	style			= WS_CHILD
					| WS_GROUP
					| WS_TABSTOP
					| ES_LEFT
					| WS_CLIPSIBLINGS;

	if (isml)
		style |= ES_AUTOVSCROLL | ES_MULTILINE | ES_WANTRETURN;
	else
		style |= ES_AUTOVSCROLL | ES_AUTOHSCROLL | ES_MULTILINE;

	handle = CreateWindowEx (WS_EX_CLIENTEDGE,
							 "edit",
							 "",
							 style,
							 x, y, w, h,
							 parent,
							 0,
							 ghInst,
							 0);
	SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack
	
	/*	Store the standard Windows callback routine adress in stdEditCallback
		and subclass the edit control with EditControlProcedure if isKeySensitive and
		SimpleEditControlProcedure otherwise.
	*/
#ifdef _WIN64
	if (isKeySensitive)
		stdEditCallback = SetWindowLongPtr (handle, GWLP_WNDPROC, (LONG_PTR) EditControlProcedure);
	else
		stdEditCallback = SetWindowLongPtr (handle, GWLP_WNDPROC, (LONG_PTR) SimpleEditControlProcedure);
#else
	if (isKeySensitive)
		stdEditCallback = SetWindowLong (handle, GWL_WNDPROC, (LONG) EditControlProcedure);
	else
		stdEditCallback = SetWindowLong (handle, GWL_WNDPROC, (LONG) SimpleEditControlProcedure);
#endif

	MakeReturn1Cci (pcci, (size_t) handle);
}

void EvalCcRqCREATERADIOBUT (CrossCallInfo *pcci)		/* hwnd, x,y,w,h, isfirst;	HWND result. */
{
	HWND handle;
	int x, y, w, h, style;
	HWND parent;
	BOOL first;

	parent = (HWND) pcci->p1;
	x = pcci->p2;
	y = pcci->p3;
	w = pcci->p4;
	h = pcci->p5;
	first = pcci->p6;

	style	= WS_CHILD			
			| BS_AUTORADIOBUTTON	
			| WS_CLIPSIBLINGS;
	if (first)
		style |= WS_GROUP | WS_TABSTOP;

	handle = CreateWindow (	"button",
							"", 
							style,
							x, y, w, h,
							parent,
							0,
							ghInst,
							0);
	SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) handle);
}

void EvalCcRqCREATECHECKBOX (CrossCallInfo *pcci)		/* hwnd, x,y,w,h, isfirst; HWND result. */
{
	HWND handle;
	int x, y, w, h, style;
	HWND parent;
	BOOL first;

	parent = (HWND) pcci->p1;
	x = pcci->p2;
	y = pcci->p3;
	w = pcci->p4;
	h = pcci->p5;
	first = pcci->p6;

	style	= WS_CHILD
			| BS_AUTOCHECKBOX
			| WS_CLIPSIBLINGS;
	if (first)
		style |= WS_GROUP | WS_TABSTOP;

	handle = CreateWindow (	"button",
							"",
							style,
							x, y, w, h,
							parent,
							0,
							ghInst,
							0);
	SendMessage  (handle, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (handle, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	MakeReturn1Cci (pcci, (size_t) handle);
}

void EvalCcRqSETITEMCHECK (CrossCallInfo *pcci)	/* hwnd, bool; no result. */
{
	HWND hwnd;
	BOOL on;
	int check;

	hwnd = (HWND) pcci->p1;
	on = pcci->p2;

	if (on)
		check = 1;
	else
		check = 0;

	SendMessage (hwnd, BM_SETCHECK, (WPARAM) check, 0);

	MakeReturn0Cci (pcci);
}

void EvalCcRqENABLECONTROL (CrossCallInfo *pcci) /* hwnd, bool; no result. */
{
	HWND window;
	BOOL wasUnable, newSelect;

	window    = (HWND) pcci->p1;
	newSelect = (BOOL) pcci->p2;
	
	wasUnable = EnableWindow (window, newSelect);
	if (wasUnable != newSelect)
		InvalidateRect (window, NULL, FALSE);

	MakeReturn0Cci (pcci);
}

void EvalCcRqSHOWCONTROL (CrossCallInfo *pcci)	// hwnd, bool; no result. 
{
	int nCmdShow;

	if (pcci->p2) 
		nCmdShow = SW_SHOWNA;
	else
		nCmdShow = SW_HIDE;

	ShowWindow ((HWND) pcci->p1, nCmdShow);
	MakeReturn0Cci (pcci);
}

/* Hide/show windows. */
void EvalCcRqSHOWWINDOW (CrossCallInfo *pcci)	/* hwnd, show, activate; no result. */
{
	int nCmdShow;
	BOOL show, activate;
	HWND wPtr;

	wPtr     = (HWND) pcci->p1;
	show     = (BOOL) pcci->p2;
	activate = (BOOL) pcci->p3;

	if (!show)
		nCmdShow = SW_HIDE;
	else if (activate)
		nCmdShow = SW_SHOW;
	else 
		nCmdShow = SW_SHOWNOACTIVATE;

	ShowWindow (wPtr, nCmdShow);
	MakeReturn0Cci (pcci);
}

void EvalCcRqCREATEPOPUP (CrossCallInfo *pcci)	/* hwnd, x,y,w,h,isEditable;  HWND hwndPopUp,hwndEdit (if isEditable). */
{
	HWND parent, hwndPopUp, hwndEdit;
	int x, y, w, h;
	int style;
	BOOL isEditable;

	parent     = (HWND) pcci->p1;
	x          = pcci->p2;
	y          = pcci->p3;
	w          = pcci->p4;
	h          = pcci->p5;
	isEditable = (BOOL) pcci->p6;

	style	= WS_CHILD
			| WS_GROUP
			| WS_TABSTOP
			| WS_VSCROLL
			| CBS_AUTOHSCROLL
		//	| CBS_OWNERDRAWFIXED	// PA: removed, otherwise garbage strings appear for empty popups.
			| CBS_HASSTRINGS
			| WS_CLIPSIBLINGS;
	if (isEditable)
		style |= CBS_DROPDOWN;
	else
		style |= CBS_DROPDOWNLIST;

	hwndPopUp = CreateWindowEx (0,
							 "COMBOBOX",
							 "",
							 style,
							 x, y, w, h,
							 parent,
							 (HMENU) 0,
							 ghInst,
							 0);
	SendMessage  (hwndPopUp, CB_SETEXTENDEDUI, (WPARAM) TRUE, 0);
	SendMessage  (hwndPopUp, WM_SETFONT, (WPARAM)gControlFont, MAKELPARAM (TRUE,0));
	SetWindowPos (hwndPopUp, HWND_BOTTOM, 0,0,0,0, SWP_NOMOVE+SWP_NOSIZE);	// This should implement control stack

	/*	Store the standard Windows callback routine adress in stdPopUpCallback
		and subclass the combobox's edit control with PopUpControlProcedure.
	*/
	if (isEditable)
	{
		hwndEdit = GetWindow (hwndPopUp,GW_CHILD);
#ifdef _WIN64
		stdPopUpCallback = SetWindowLongPtr (hwndEdit, GWLP_WNDPROC, (LONG_PTR) PopUpControlProcedure);
#else
		stdPopUpCallback = SetWindowLong (hwndEdit, GWL_WNDPROC, (LONG) PopUpControlProcedure);
#endif
	}
	else
	{
		hwndEdit = 0;
	}
	MakeReturn2Cci (pcci, (size_t) hwndPopUp, (size_t) hwndEdit);
}

void EvalCcRqADDTOPOPUP (CrossCallInfo *pcci)	/* hwnd, textptr, enabled, selected, index; Pos result. */
{
	int  index, pos;
	HWND hpopup;
	char *text;
	BOOL enabled, selected;

	hpopup   = (HWND) pcci->p1;
	text     = (char *) pcci->p2;
	enabled  = (BOOL) pcci->p3;
	selected = (BOOL) pcci->p4;
	index    = (int)  pcci->p5;

	pos = SendMessage (hpopup, CB_ADDSTRING, 0, (LPARAM) text);
	//pos = SendMessage (hpopup, CB_INSERTSTRING, (WPARAM) index, (LPARAM) text);

	SendMessage (hpopup, CB_SETITEMDATA, (WPARAM) pos, (LPARAM) enabled);

	if (selected)
		SendMessage (hpopup, CB_SETCURSEL, (WPARAM) pos, 0);

	MakeReturn1Cci (pcci, pos);
}

void EvalCcRqSELECTPOPUPITEM (CrossCallInfo *pcci)		/* hwnd, pos; no result */
{
	HWND hpopup;
	int pos;

	hpopup = (HWND) pcci->p1;
	pos = pcci->p2;

	SendMessage (hpopup, CB_SETCURSEL, (WPARAM) pos, 0);

	MakeReturn0Cci (pcci);
}

void EvalCcRqENABLEPOPUPITEM (CrossCallInfo *pcci)		/* hwnd, pos, enabled; no result */
{
	HWND hpopup;
	int pos;
	LPARAM enabled;

	hpopup = (HWND) pcci->p1;
	pos = pcci->p2;
	enabled = (LPARAM) pcci->p3;

	SendMessage (hpopup, CB_SETITEMDATA, (WPARAM) pos, enabled);
	if (SendMessage (hpopup, CB_GETCURSEL, 0, 0) == pos)
	{
		InvalidateRect (hpopup, NULL, TRUE);
	}

	MakeReturn0Cci (pcci);
}

void EvalCcRqRESTACKWINDOW (CrossCallInfo *pcci)		/* thewindow,behind; no result. */
{
	HWND thePtr, behindPtr;
	UINT uflags = SWP_NOMOVE + SWP_NOSIZE;	/*	Do not change current size or location */

	thePtr    = (HWND) pcci->p1;
	behindPtr = (HWND) pcci->p2;

	SetWindowPos (thePtr, behindPtr, 0, 0, 0, 0, uflags);

	MakeReturn0Cci (pcci);
}

/*	Add controls to tooltip area. */
void EvalCcRqADDCONTROLTIP (CrossCallInfo *pcci) /* parentPtr, controlPtr, textPtr; no result. */
{
	HWND hwndParent,hwndControl;	/* The handle to the window and control. */
	TOOLINFO ti;					/* The tool information that is sent to the tooltip control. */
	char *text;

	hwndParent  = (HWND) pcci->p1;
	hwndControl = (HWND) pcci->p2;
	text        = (char *)pcci->p3;
	
	/* Fill the tooltip info with the appropriate information. */
	ti.cbSize     = sizeof(TOOLINFO);
	ti.uFlags     = TTF_IDISHWND | TTF_SUBCLASS;
	ti.hwnd       = hwndParent;
#ifdef _WIN64
	ti.uId        = (UINT_PTR) hwndControl;
#else
	ti.uId        = (UINT) hwndControl;
#endif
	ti.rect.left  = 0;
	ti.rect.top   = 0;
	ti.rect.right = 0;
	ti.rect.bottom= 0;
	ti.hinst      = ghInst;
	ti.lpszText   = (LPSTR) text;

	SendMessage (ghwndTT, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO)&ti);
	MakeReturn0Cci (pcci);
}

/*	Remove controls from tooltip area. */
void EvalCcRqDELCONTROLTIP (CrossCallInfo *pcci) /* parentPtr, controlPtr; no result. */
{
	HWND hwndParent,hwndControl;		/* The handle to the window and control. */
	TOOLINFO ti;						/* The tool information that is sent to the tooltip control. */
	
	hwndParent  = (HWND) pcci->p1;
	hwndControl = (HWND) pcci->p2;

	/* Fill the tooltip info with the appropriate information. */
	ti.cbSize     = sizeof(TOOLINFO);
	ti.uFlags     = TTF_IDISHWND;
	ti.hwnd       = hwndParent;
#ifdef _WIN64
	ti.uId        = (UINT_PTR) hwndControl;
#else
	ti.uId        = (UINT) hwndControl;
#endif

	SendMessage (ghwndTT, TTM_DELTOOL, 0, (LPARAM) (LPTOOLINFO)&ti);
	MakeReturn0Cci (pcci);
}


/*	Initialisation:
*/
void InitialiseCrossCallWindows (void)
{
	WNDCLASSEX wclass;
	
	/* register custom control class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = CS_NOCLOSE | CS_PARENTDC;
	wclass.lpfnWndProc   = (WNDPROC) CustomControlProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = NULL;
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) (COLOR_BTNFACE + 1);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = CustomControlClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	/* register clean compound control class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = CS_PARENTDC;
	wclass.lpfnWndProc   = (WNDPROC) CompoundControlProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = 0;
	wclass.hInstance     = ghInst;
	wclass.hIcon         = NULL;
	wclass.hCursor       = LoadCursor (ghInst, IDC_ARROW);
	wclass.hbrBackground = (HBRUSH) (COLOR_BTNFACE + 1);//(NULL_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = CompoundControlClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	/* register clean SDI window class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) SDIWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for LocalWindowData
	wclass.hInstance     = ghInst;
	wclass.hIcon         = NULL;
	wclass.hCursor       = NULL;								// Must be NULL, otherwise SetCursor won't have effect.
	wclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = SDIWindowClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	/* register clean MDI window class */
	wclass.cbSize        = sizeof (WNDCLASSEX);
	wclass.style         = 0;
	wclass.lpfnWndProc   = (WNDPROC) MDIWindowProcedure;
	wclass.cbClsExtra    = 0;
	wclass.cbWndExtra    = sizeof (HANDLE);						// Allocate local memory for LocalWindowData
	wclass.hInstance     = ghInst;
	wclass.hIcon         = LoadIcon (ghInst, IDI_APPLICATION);
	wclass.hCursor       = NULL;								// Must be NULL, otherwise SetCursor won't have effect.
	wclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
	wclass.lpszMenuName  = NULL;
	wclass.lpszClassName = MDIWindowClassName;
	wclass.hIconSm       = NULL;
	RegisterClassEx (&wclass);

	/* initialise the common control library. */
	InitCommonControls ();
}


/*	Install the cross call procedures in the gCrossCallProcedureTable of cCrossCall_121.
*/
int InstallCrossCallWindows (int ios)
{
	CrossCallProcedureTable newTable;

	InitialiseCrossCallWindows ();

	newTable = EmptyCrossCallProcedureTable ();
	AddCrossCallEntry (newTable, CcRqBEGINPAINT,             EvalCcRqBEGINPAINT);
	AddCrossCallEntry (newTable, CcRqENDPAINT,               EvalCcRqENDPAINT);
	AddCrossCallEntry (newTable, CcRqFAKEPAINT,              EvalCcRqFAKEPAINT);
	AddCrossCallEntry (newTable, CcRqDESTROYMODALDIALOG,     EvalCcRqDESTROYMODALDIALOG);
	AddCrossCallEntry (newTable, CcRqDESTROYMDIDOCWINDOW,    EvalCcRqDESTROYMDIDOCWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATESDIDOCWINDOW,     EvalCcRqCREATESDIDOCWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATEMDIDOCWINDOW,     EvalCcRqCREATEMDIDOCWINDOW);
	AddCrossCallEntry (newTable, CcRqSETWINDOWTITLE,         EvalCcRqSETWINDOWTITLE);
	AddCrossCallEntry (newTable, CcRqGETWINDOWTEXT,          EvalCcRqGETWINDOWTEXT);
	AddCrossCallEntry (newTable, CcRqUPDATEWINDOWRECT,       EvalCcRqUPDATEWINDOWRECT);
	AddCrossCallEntry (newTable, CcRqSETCLIENTSIZE,          EvalCcRqSETCLIENTSIZE);
	AddCrossCallEntry (newTable, CcRqSETSELECTWINDOW,        EvalCcRqSETSELECTWINDOW);
	AddCrossCallEntry (newTable, CcRqSETWINDOWPOS,           EvalCcRqSETWINDOWPOS);
	AddCrossCallEntry (newTable, CcRqGETWINDOWSIZE,          EvalCcRqGETWINDOWSIZE);
	AddCrossCallEntry (newTable, CcRqSETWINDOWSIZE,          EvalCcRqSETWINDOWSIZE);
	AddCrossCallEntry (newTable, CcRqACTIVATECONTROL,        EvalCcRqACTIVATECONTROL);
	AddCrossCallEntry (newTable, CcRqACTIVATEWINDOW,         EvalCcRqACTIVATEWINDOW);
	AddCrossCallEntry (newTable, CcRqCHANGEWINDOWCURSOR,     EvalCcRqCHANGEWINDOWCURSOR);
	AddCrossCallEntry (newTable, CcRqOBSCURECURSOR,          EvalCcRqOBSCURECURSOR);
	AddCrossCallEntry (newTable, CcRqSETSCROLLRANGE,         EvalCcRqSETSCROLLRANGE);
	AddCrossCallEntry (newTable, CcRqSETSCROLLPOS,           EvalCcRqSETSCROLLPOS);
	AddCrossCallEntry (newTable, CcRqSETSCROLLSIZE,          EvalCcRqSETSCROLLSIZE);
	AddCrossCallEntry (newTable, CcRqSETEDITSELECTION,       EvalCcRqSETEDITSELECTION);
	AddCrossCallEntry (newTable, CcRqCREATEDIALOG,           EvalCcRqCREATEDIALOG);
	AddCrossCallEntry (newTable, CcRqCREATEMODALDIALOG,      EvalCcRqCREATEMODALDIALOG);
	AddCrossCallEntry (newTable, CcRqCREATECOMPOUND,         EvalCcRqCREATECOMPOUND);
	AddCrossCallEntry (newTable, CcRqCREATESCROLLBAR,        EvalCcRqCREATESCROLLBAR);
	AddCrossCallEntry (newTable, CcRqCREATEBUTTON,           EvalCcRqCREATEBUTTON);
	AddCrossCallEntry (newTable, CcRqCREATEICONBUT,          EvalCcRqCREATEICONBUT);
	AddCrossCallEntry (newTable, CcRqCREATECUSTOM,           EvalCcRqCREATECUSTOM);
	AddCrossCallEntry (newTable, CcRqCREATESTATICTXT,        EvalCcRqCREATESTATICTXT);
	AddCrossCallEntry (newTable, CcRqCREATEEDITTXT,          EvalCcRqCREATEEDITTXT);
	AddCrossCallEntry (newTable, CcRqCREATERADIOBUT,         EvalCcRqCREATERADIOBUT);
	AddCrossCallEntry (newTable, CcRqCREATECHECKBOX,         EvalCcRqCREATECHECKBOX);
	AddCrossCallEntry (newTable, CcRqSETITEMCHECK,           EvalCcRqSETITEMCHECK);
	AddCrossCallEntry (newTable, CcRqENABLECONTROL,          EvalCcRqENABLECONTROL);
	AddCrossCallEntry (newTable, CcRqSHOWCONTROL,            EvalCcRqSHOWCONTROL);
	AddCrossCallEntry (newTable, CcRqSHOWWINDOW,             EvalCcRqSHOWWINDOW);
	AddCrossCallEntry (newTable, CcRqCREATEPOPUP,            EvalCcRqCREATEPOPUP);
	AddCrossCallEntry (newTable, CcRqADDTOPOPUP,             EvalCcRqADDTOPOPUP);
	AddCrossCallEntry (newTable, CcRqSELECTPOPUPITEM,        EvalCcRqSELECTPOPUPITEM);
	AddCrossCallEntry (newTable, CcRqENABLEPOPUPITEM,        EvalCcRqENABLEPOPUPITEM);
	AddCrossCallEntry (newTable, CcRqRESTACKWINDOW,          EvalCcRqRESTACKWINDOW);
	AddCrossCallEntry (newTable, CcRqADDCONTROLTIP,          EvalCcRqADDCONTROLTIP);
	AddCrossCallEntry (newTable, CcRqDELCONTROLTIP,          EvalCcRqDELCONTROLTIP);
	AddCrossCallEntries (gCrossCallProcedureTable, newTable);

	return ios;
}