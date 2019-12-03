;+
; NAME:
;	dtg2gvtime
;
; PURPOSE:
;	Converts a date/time string 'YYYYMMDDhhmmss' or a
;       date/time bytarr defined in the same way into a
;	GredVis time (days since 1 Jan 0001, 00:00 UTC)
;	Important: The year must be given as a 4 digits number
;	(2 digits not allowed)
;   
; CATEGORY:
;
; CALLING SEQUENCE:
;       gvtime=dtg2gvtime(dtg)
;
; EXAMPLE:
;       dtg='19990101'
;       gvtime=dtg2gvtime(dtg)
;
; INPUTS:
;	dtg:	STRING OR strarr(n) OR BYTARR(m) OR BYTARR(m,n):
;               The n date/time strings in the format 'YYYYMMDDhhmmss'
;		hh, mm or ss are assumed to be 0 if not provided
;		MM and DD are set to 1 if not provided
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;	
; MODIFICATION HISTORY:
;
; Dominik Brunner, 13 Jan 2000
;-
FUNCTION dtg2gvtime,dtg

gvtime=0D	; 1. Jan 0001, 00:00 UTC as default
n=n_elements(dtg)
IF n EQ 0 THEN return,gvtime

hdtg=strcompress(dtg,/rem) ; necessary if dtg is a byte array

basetime=julday(1,1,0001,0,0,0)
gvtime=make_array(n)+gvtime

FOR i=0L,n-1L DO BEGIN
    IF STRLEN(hdtg[i]) GT 3 THEN YYYY=FIX(STRMID(hdtg[i],0,4)) ELSE YYYY=0001
    IF STRLEN(hdtg[i]) GT 5 THEN MM=FIX(STRMID(hdtg[i],4,2)) ELSE MM=1
    IF STRLEN(hdtg[i]) GT 7 THEN DD=FIX(STRMID(hdtg[i],6,2)) ELSE DD=1
    IF STRLEN(hdtg[i]) GT 9 THEN hh=FIX(STRMID(hdtg[i],8,2)) ELSE hh=0
    IF STRLEN(hdtg[i]) GT 11 THEN minu=FIX(STRMID(hdtg[i],10,2)) ELSE minu=0
    IF STRLEN(hdtg[i]) GT 13 THEN ss=FIX(STRMID(hdtg[i],12,2)) ELSE ss=0
    gvtime[i]=julday(MM,DD,YYYY,hh,minu,ss)-basetime
ENDFOR

IF n EQ 1 THEN return,gvtime[0] ELSE return,gvtime

END
