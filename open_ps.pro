;+
; NAME:
;	open_ps
;
; PURPOSE:
;	opens a postcript-file and directs the graphic-output to this
;	file.
;	Stores the postscript file in the current directory if no
;       full path is provided.
;
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	open_ps,name,/color,/landscape,pssize=pssize,aspect=aspect,/eps,$
;	   lolepos=lolepos,fullpage=fullpage,tt=tt,tt_type=tt_type,tt_size=tt_size
; EXAMPLE:
;	open_ps,'test.eps',/eps
; INPUTS:
;	filename:the filename of the post script file
;		if no name is specified, 'idl.ps' is used.
;		if filename starts with ~ or /, it is interpreted as
;		an absolute filepath.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;	/color    : (Keyword) if set, a color postscriptfile is generated.
;	/landscape: (Keyword) if set, the display is plotted in landscape
;	aspect    : the aspect ration of the ps-output, default is 0.7,
;		    i.e. pssize=[15.,15.*aspect]=[15.,15.*0.7]
;	pssize    : fltarr(2) the size in cm of the ps-output, but rather
;		    use aspect than defining the x-size by yourself.
;		    (-> xsize=pssize(0), ysize=pssize(1)
;	lolepos   : fltarr(2) the position of the lower left corner
;		    (-> xoffset=lolepos(0), yoffset=lolepos(1))
;	/eps      : if set, encapsulated postscript rather than full-page
;		    postcript output is generated.
;	/fullpage : (Keyword) if set, a fullpage is created
;	/inches   : if set, all measures are in inches instead of cm
;       /tt       : if set, true type characters are used
;       tt_type   : True type character type (default is 'Times')
;       tt_size   : True type character size (default is 10)
;
; OUTPUTS:
;	postscript-file
; COMMON BLOCKS:
;	see common.pro;
;	postscript
; SIDE EFFECTS:
;	changes directory and changes back, old directory in old_dir
; RESTRICTIONS:
;	none
; PROCEDURE:
;
; MODIFICATION HISTORY:
; (c) by David N. Bresch, 21.12.92
;	revision	950915
;	Dominik Brunner 970823 fullpage, landscape und eps-Optionen verbessert
;       Dominik Brunner 140213 added true type character support
;-

PRO open_ps,filename,color=color,pssize=pssize,aspect=aspect,eps=eps,$
            lolepos=lolepos,fullpage=fullpage,landscape=landscape,inches=inches,$
            tt_type=tt_type,tt_size=tt_size

def_ps_dir=''

IF keyword_set(landscape) THEN xdefault=25. ELSE xdefault=16.

if n_elements(aspect) eq 0 then aspect=0.7
if n_elements(pssize) eq 0 then pssize=[xdefault,xdefault*aspect]

; position of lower left corner for postscript files (no effect on eps)
IF (n_elements(lolepos) NE 2) AND (NOT keyword_set(landscape)) THEN $
	lolepos=[1.,1.]

; full (portrait) page
if KEYWORD_SET(fullpage) then begin
   ;A4 fullpage
   lolepos=[1.,1.]
   pssize=[19.,28.]
endif

IF N_ELEMENTS(filename) EQ 0 THEN begin
   filename='idl.ps'
   print,'% ps file:',def_ps_dir+filename
endif

;test, if filename is an absolute path
if ((strmid(filename,0,1) eq '/') or (strmid(filename,0,1) eq '~')) $
   then begin
   filename=expand_path(filename)
endif else begin
   filename=def_ps_dir+filename
endelse

; get current color table
tvlct,r,g,b,/get

plotcol=!p.color
bgcol=!p.background

set_plot,'PS'
!p.font = 1

; check if color index 0 is white. In this case suppress default IDL
; behaviour of exchanging the values of !p.color and !p.background
IF (r[0] EQ 255B) AND (g[0] EQ 255B) AND (b[0] EQ 255B) THEN BEGIN
   !p.color=plotcol
   !p.background=bgcol
ENDIF

IF keyword_set(inches) THEN device,/inches ELSE device,inches=0

if keyword_set(eps) then begin
   device,/encapsul,filename=filename
endif else begin
   device,encapsul=0,filename=filename
endelse

IF KEYWORD_SET(color) THEN device,/color, bits=8

if KEYWORD_SET(landscape) then device,/landscape ELSE device,landscape=0

IF NOT keyword_set(landscape) THEN device,xsize=pssize(0),ysize=pssize(1) $
ELSE device,xsize=pssize(0),ysize=pssize(1)

IF(n_elements(lolepos) NE 0) AND (NOT keyword_set(eps)) THEN $
   device,xoffset=lolepos(0),yoffset=lolepos(1)

IF keyword_set(tt_type) THEN BEGIN
   IF n_elements(tt_type) EQ 0 THEN tt_type = 'Times'
   IF n_elements(tt_size) EQ 0 THEN tt_size = 10
   device,/tt_font,set_font=tt_type,font_size=tt_size
ENDIF
device_language_level=2

END ;of open_ps
