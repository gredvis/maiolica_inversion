;+
; NAME:
;	gvtime2dtg
;
; PURPOSE:
;	Converts a GredVis date/time into a date/time string 'YYYYMMDDhhmm'
;
; CATEGORY:
;
; CALLING SEQUENCE:
;       dtg=gvtime2dtg(gvtime)
;
; EXAMPLE:
;       gvtime=1D	; 2 Jan 0001, 00:00h
;       dtg=gvtime2dtg(gvtime)
;
; INPUTS:
;	gvtime:	(dblarr(n)) The GredVis time(s) in terms of
;			number of days since 1 Jan 0001, 00:00
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;       /sec:           set this keyword to include seconds in answer
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
FUNCTION gvtime2dtg,gvtime,sec=sec

   dtg='000101010000'
   n=n_elements(gvtime)
   IF n EQ 0 THEN return,dtg

   dtg=StrArr(n)
   basetime=julday(1,1,0001,0,0,0)
   FOR i=0L,n-1L DO BEGIN
      caldat,double(gvtime[i]+basetime),m,d,y,h,minu,s
      IF NOT keyword_set(sec) THEN minu=minu+(s GT 30)
      h=h+minu/60
      d=d+h/24
      minu=minu MOD 60
      h=h MOD 24
      IF y LT 10 THEN yy='000'+STRCOMPRESS(y,/rem) ELSE $
      IF y LT 100 THEN yy='00'+STRCOMPRESS(y,/rem) ELSE $
      IF y LT 1000 THEN yy='0'+STRCOMPRESS(y,/rem) ELSE $
      yy=STRING(y,format='(i4)')
      IF m LT 10 THEN mm='0'+STRCOMPRESS(m,/rem) ELSE mm=STRCOMPRESS(m,/rem)
      IF d LT 10 THEN dd='0'+STRCOMPRESS(d,/rem) ELSE dd=STRCOMPRESS(d,/rem)
      IF h LT 10 THEN hh='0'+STRCOMPRESS(h,/rem) ELSE hh=STRCOMPRESS(h,/rem)
      IF minu LT 10 THEN minu='0'+STRCOMPRESS(minu,/rem) $
      ELSE minu=STRCOMPRESS(minu,/rem)
      IF keyword_set(sec) THEN BEGIN
         IF s LT 10 THEN ss='0'+STRING(s,format='(i1)') ELSE ss=STRING(s,format='(i2)')
         dtg[i]=yy+mm+dd+hh+minu+ss
      ENDIF ELSE dtg[i]=yy+mm+dd+hh+minu
   ENDFOR
   IF n_elements(dtg) EQ 1 THEN return,dtg[0] ELSE return,dtg

END
