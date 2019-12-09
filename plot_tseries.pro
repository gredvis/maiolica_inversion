;+
; NAME:
;
;  plot_tseries
;
; PURPOSE:
;
;  Create a time series plot with reasonable date and time labels that automatically
;  adjust to the time span
;
; CATEGORY:
;
;   plotting, time series
;
; CALLING SEQUENCE:
;
;   plot_tseries,x,y,over=over,shades=shades,xshade=xshade,_extra=e,xrange=xrange,nsum=nsum,$
;                    noxticklab=noxticklab
;
; INPUTS:
;
;   x         : FltArr(n), the n discrete time values in Gredvis date format
;   y         : FltArr(n) the dependent values
;
; KEYWORD PARAMETERS:
;
;   /over     : set this keyword to overlay over existing plot
;  _extra     : additional keywords accepted by IDL plot routine
;   xrange    : date range
;   nsum      : set to a value larger than 1 to average over nsum points for smoothing
;   /noxticklab : suppress plotting of x-axis title and labels
;
; OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  none
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;  none
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 10 Nov 2012: first implementation
;   DB, 16 Jun 2016: added keyword /noxticklab to suppress plotting of x-axis labels
;
;-
PRO plot_tseries,x,y,over=over,shades=shades,xshade=xshade,_extra=e,xrange=xrange,xstyle=xstyle,$
                 nsum=nsum,noxticklab=noxticklab

  ; check for input
  IF n_elements(x) LE 1 OR n_elements(x) NE n_elements(y) THEN RETURN

  IF n_elements(xrange) EQ 2 THEN BEGIN
     xmin=xrange[0] & xmax=xrange[1]
  ENDIF ELSE BEGIN
     xmin=min(x,max=xmax)
     xrange=[xmin,xmax]
  ENDELSE
  isseconds = 0B & isminutes = 0B & ishours = 0B
  isdays = 0B & ismonths = 0B & isyears = 0B

  ;; check if time span is best measured in years, months, days, or hours
  CASE 1 OF
     (xmax-xmin) LE 0.003: isseconds = 1B
     (xmax-xmin) GT 0.003 AND (xmax-xmin) LE 0.08: isminutes = 1B
     (xmax-xmin) GT 0.08 AND (xmax-xmin) LE 1: ishours = 1B
     (xmax-xmin) GT 1 AND (xmax-xmin) LE 30: isdays = 1B
     (xmax-xmin) GT 30 AND (xmax-xmin) LE 366: ismonths = 1B
     ELSE: isyears = 1B
  ENDCASE

  tbase=julday(1,1,1,0,0,0)
  xrng = xrange + tbase

  ;;***********************************
  ;; create date/time labels
  ;;***********************************
  IF isseconds THEN BEGIN
     xtitle='Minute/seconds'
     result=label_date(date_format='%I:%S')
     ;; number of seconds
     nsec = ceil((xmax-floor(xmin))*86400D) - floor((xmin-floor(xmin)) * 86400D)
     CASE 1 OF
        nsec LE 10: BEGIN
           sstep=2
           xminor=4
        END
        nsec GT 10 AND nsec LE 30: BEGIN
           sstep=5
           xminor=5
        END
        ELSE: BEGIN
           sstep=20
           xminor=4
        END
     ENDCASE
     xfmin=floor(xmin)+floor((xmin-floor(xmin))*86400D)/(86400D)
     xticklab=gvtime2dtg(xfmin)
     FOR k=sstep,nsec,sstep DO xticklab=[xticklab,gvtime2dtg(xfmin+k/86400D)]
  ENDIF

  IF isminutes THEN BEGIN
     xtitle='Time (UTC)'
     result=label_date(date_format='%H:%I')
     ;; number of minutes
     nminu = ceil((xmax-floor(xmin)) * 1440D) - floor((xmin-floor(xmin)) * 1440D)
     CASE 1 OF
        nminu LE 10: BEGIN
           mstep=1
           xminor=6
        END
        nminu GT 10 AND nminu LE 30: BEGIN
           mstep=5
           xminor=5
        END
        ELSE: BEGIN
           mstep=10
           xminor=5
        END
     ENDCASE
     xfmin=floor(xmin*1440D)/(1440D)
     xticklab=gvtime2dtg(xfmin)
     FOR k=mstep,nminu,mstep DO xticklab=[xticklab,gvtime2dtg(xfmin+k/1440D)]
  ENDIF

  IF ishours THEN BEGIN
     xtitle='Time (UTC)'
     result=label_date(date_format='%H:00')
     nhour = ceil(xmax * 24D) - floor(xmin * 24D)
     CASE 1 OF
        nhour LE 5: BEGIN
           hstep=1
           xminor=6
        END
        nhour GT 5 AND nhour LE 12: BEGIN
           hstep=3
           xminor=6
        END
        ELSE: BEGIN
           hstep=3
           xminor=6
        END
     ENDCASE
     xfmin=floor(xmin*24D)/24D
     xticklab=gvtime2dtg(xfmin)
     FOR k=hstep,nhour,hstep DO xticklab=[xticklab,gvtime2dtg(xfmin+k/24D)]
  ENDIF

  IF isdays THEN BEGIN
     xtitle='Day/Month (UTC)'
     result=label_date(date_format='%D/%M')
     nday = ceil(xmax) - floor(xmin)
     CASE 1 OF
        nday LE 10: BEGIN
           dstep=1
           xminor=8
        END
        nday GT 10 AND nday LE 20: BEGIN
           dstep=2
           xminor=8
        END
        ELSE: BEGIN
           dstep=5
           xminor=5
        END
     ENDCASE
     xfmin=floor(xmin)
     xticklab=gvtime2dtg(xfmin)
     FOR k=dstep,nday,dstep DO xticklab=[xticklab,gvtime2dtg(xfmin+k)]
  ENDIF

  IF ismonths THEN BEGIN
     xtitle='Day/Month (UTC)'
     result=label_date(date_format='%D/%M')
     nday = ceil(xmax) - floor(xmin)
     CASE 1 OF
        nday LE 60: BEGIN
           dstep=10
           xminor=10
        END
        nday GT 60 AND nday LE 120: BEGIN
           dstep=20
           xminor=5
        END
        ELSE: BEGIN
           dstep=30
           xminor=5
        END
     ENDCASE
     xfmin=floor(xmin)
     xticklab=gvtime2dtg(xfmin)
     FOR k=dstep,nday,dstep DO xticklab=[xticklab,gvtime2dtg(xfmin+k)]
  ENDIF

  IF isyears THEN BEGIN
     xtitle='Month/Year'
     result=label_date(date_format='%M/%Y')
     yrmax = STRMID(gvtime2dtg(xmax),0,4) & yrmin = STRMID(gvtime2dtg(xmin),0,4)
     nyr = long(yrmax) - long(yrmin) + 1
     CASE 1 OF
        nyr LE 3: BEGIN
           mstep=3
           xminor=3
        END
        nyr GT 3 AND nyr LE 10: BEGIN
           mstep=12
           xminor=4
        END
        ELSE: BEGIN
           mstep=12*5
           xminor=5
        END
     ENDCASE
     dtgmin=STRMID(gvtime2dtg(xmin),0,6)+'01'
     mmmin=long(STRMID(dtgmin,4,2))
     dtgmax=STRMID(gvtime2dtg(xmax+15D),0,6)+'01'
     mmmax=long(STRMID(dtgmax,4,2))
     ;; number of months to cover
     nmonth = mmmax + (nyr-2)*12 + (12-mmmin)
     xfmin=dtg2gvtime(dtgmin)
     xticklab=STRMID(gvtime2dtg(xfmin),0,8)
     FOR k=mmmin+mstep,mmmin+nmonth,mstep DO BEGIN
        yract = STRING(yrmin + k/12,format='(i4)')
        mmact = k MOD 12
        IF mmact LT 10 THEN mmact = '0'+STRING(mmact,format='(i1)') ELSE $
           mmact = STRING(mmact,format='(i2)')
        xticklab=[xticklab,yract+mmact+'01']
     ENDFOR
  ENDIF
  xtickv=tbase+dtg2gvtime(xticklab)
  

  IF keyword_set(noxticklab) THEN BEGIN
     xtitle = ''
     xtickname = replicate(' ',n_elements(xticklab)+1)
  ENDIF ELSE BEGIN
     xtickformat = 'label_date'
  ENDELSE

  ;print,xticklab
  IF n_elements(xstyle) EQ 0 THEN xstyle = 1
  IF NOT keyword_set(over) THEN BEGIN
     IF keyword_set(shades) THEN BEGIN
        ;; plot areas between tickmarks or between distances xshade in different grey shades
        ;; assuming grey is color index 99
        plot,x+tbase,y,xstyle=xstyle,xtickformat=xtickformat,xtitle=xtitle,xminor=xminor,nsum=nsum,$
             xtickv=xtickv,xticks=n_elements(xtickv)-1,_extra=e,/nodata,ystyle=4,xrange=xrng,$
             xtickname=xtickname
        IF n_elements(xshade) EQ 0 THEN BEGIN
           FOR i=1,n_elements(xtickv)-1,2 DO BEGIN
              polyfill,[xtickv[i-1],xtickv[i],xtickv[i],xtickv[i-1]],$
                       [!y.crange[0],!y.crange[0],!y.crange[1],!y.crange[1]],color=99B,$
                       noclip=0
           ENDFOR
        ENDIF ELSE BEGIN
           nshade = ceil((!x.crange[1] - !x.crange[0])/xshade)
           xshadeticks = !x.crange[0] + indgen(nshade+1)*xshade
           FOR i=1,nshade-1,2 DO BEGIN
              polyfill,[xshadeticks[i],xshadeticks[i+1],xshadeticks[i+1],xshadeticks[i]],$
                       [!y.crange[0],!y.crange[0],!y.crange[1],!y.crange[1]],color=99B,$
                       noclip=0
           ENDFOR
        ENDELSE
        plot,x+tbase,y,xstyle=xstyle,xtickformat=xtickformat,xtitle=xtitle,xminor=xminor,nsum=nsum,$
             xtickv=xtickv,xticks=n_elements(xtickv)-1,_extra=e,/noerase,xrange=xrng,xtickname=xtickname
     ENDIF ELSE BEGIN
        plot,x+tbase,y,xstyle=xstyle,xtickformat=xtickformat,xtitle=xtitle,xminor=xminor,nsum=nsum,$
             xtickv=xtickv,xticks=n_elements(xtickv)-1,_extra=e,xrange=xrng,xtickname=xtickname
     ENDELSE
  ENDIF ELSE BEGIN
     oplot,x+tbase,y,nsum=nsum,_extra=e
  ENDELSE
END
