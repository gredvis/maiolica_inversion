;+
; NAME:
;
;   plot_inv_timeseries
;
; PURPOSE:
;
;   Plot modelled estimates of observational values
;   using zma = H*xa or zmp = H*xp
;   
;   zma = apriori model estimate of observational values
;   zmp = aposteriori model estimate of observational values
;   H   = sensitivity matrix
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  plot_inv_timeseries,sim,syyyy=syyyy,eyyyy=eyyyy,prelim=prelim,prior=prior,$
;                            post=post,bg=bg,eps=eps,dump=dump,map=map
;
; INPUTS:
;
;       sim              : the simulation structure, see inv_configurations.pro
;
; KEYWORD PARAMETERS:
;
;       syyyy (string)   : first year to plot
;       eyyyy (string)   : last year to plot
;       /prelim          : set this keyword to plot results of preliminary inversion
;       /prior           : set this keyword to plot only obs and prior
;       /post            : set this keyword to plot only obs and posterior
;                          If neither /prior or /post is set, both are plotted
;       /bg              : set this keyword to plot backgrounds (oldest age class)
;       /eps             : set this keyword to write to eps file instead of screen
;       /dump            : set this keyword to write out station data (obs, apri, apost)
;                          to files (one file per station)
;       /map             : set this keyword to create geographical maps of the mean biases
;                          and correlations of posterior values in 5-year intervals at all sites
;
; OUTPUTS:
;
; COMMON BLOCKS:
;        none
;
; SIDE EFFECTS:
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   xyouts_fiveyr_detrended_r2_bias
;     Calculate and display 5-yearly rsquare-values for detrended data and biases at 
;     a given station
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 18 January 2012
; 
; DB 24 November 2017, major rewrite
;-

;***************************************************************************************
;              xyouts_fiveyr_detrended_r2_bias
;***************************************************************************************
PRO xyouts_fiveyr_detrended_r2_bias,gvt,obs,model,r2=r2,bias=bias,syyyy=syyyy,eyyyy=eyyyy,$
                                    color=color,ypos=ypos,chs=chs

  yyyy=STRMID(gvtime2dtg(gvt),0,4)

  ;; detrended time series
  smobs = smooth(obs,5*12+1,/nan,/edge_truncate)
  smmod = smooth(model,5*12+1,/nan,/edge_truncate)
  nperiod = 4
  r2 = FltArr(nperiod)+!values.f_nan
  bias = FltArr(nperiod)+!values.f_nan
  syyyy = ['1990','1995','2000','2005']
  eyyyy = ['1994','1999','2004','2009']
  basetime = julday(1,1,1,0,0,0)

  FOR i=0,n_elements(syyyy)-1 DO BEGIN
     ind = WHERE(yyyy GE syyyy[i] AND yyyy LE eyyyy[i] AND FINITE(obs),cnt)
     IF cnt GE 30 THEN BEGIN    ; at least half of data points need to be present
        obsdev = obs[ind]-smobs[ind]
        moddev = model[ind]-smmod[ind]
        r2[i] = (correlate(obsdev,moddev))^2
        bias[i] = mean(model[ind]-obs[ind])
        yrange = !y.crange
        xp = basetime + (dtg2gvtime(syyyy[i]+'0101')+dtg2gvtime(eyyyy[i]+'0101'))/2.
        yp = yrange[0]+ypos*(yrange[1]-yrange[0])
        XYOUTS,xp+190D,yp,'R!U2!N='+STRING(r2[i],format='(f5.2)')+$
               ', b='+STRING(bias[i],format='(f5.1)')+' ppb',color=color,alignment=0.5,charsize=chs
     ENDIF
  ENDFOR

END

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO plot_inv_timeseries,sim,syyyy=syyyy,eyyyy=eyyyy,prelim=prelim,prior=prior,$
                            post=post,bg=bg,eps=eps,dump=dump,map=map

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  load_ctb,'ive.ctb'

  IF NOT keyword_set(prior) AND NOT keyword_set(post) THEN pripost=1 ELSE pripost=0

  IF keyword_set(eps) THEN BEGIN
     psdir = sim.invdir + 'FIGURES/TSERIES/'
     suffix = '.eps'
     IF keyword_set(prior) THEN suffix = '_prior.eps'
     IF keyword_set(post) THEN suffix = '_posterior.eps'
     IF keyword_set(pripost) THEN suffix = '_prior_posterior.eps'
     xthick = 2 & ythick = 2
     linethick = 2
     symsize=0.7
     chs = 1.2
  ENDIF ELSE BEGIN
     window,xsize=950,ysize=950
     xthick = 2 & ythick = 2
     linethick=1
     symsize=1.0
     chs = 1.0
  ENDELSE

  IF n_elements(syyyy) EQ 0 THEN syyyy = '1990' ; STRMID(sim.syyyymm,0,4)
  IF n_elements(eyyyy) EQ 0 THEN eyyyy = STRMID(sim.eyyyymm,0,4)

  
  ;; Read in CH4 observations and model prior and posterior fields
  nst      = n_elements(sim.stats)
  nyears   = long(fix(eyyyy)-fix(syyyy)+1)
  
  read_inv_station_output_netcdf,sim,prelim=prelim,yyyymm=yyyymm,ndata=ndata,$
                                 ch4obs=ch4obs,ch4apri=ch4apri,ch4post=ch4apost,$
                                 catapri=ch4apricat,catpost=ch4apostcat,$
                                 cats=cats,stats=stats,ok=ok

  gvt = dtg2gvtime(yyyymm)

  ;; calculate a mean seasonal cycle from all years with full 12 month coverage
  seasobs=FltArr(12,nst)
  seasapri=FltArr(12,nst)
  seaspost=FltArr(12,nst)
  
  nyavg = IntArr(nst)
  FOR i=0,nst-1 DO BEGIN
     FOR y = 0,nyears-1 DO BEGIN
        yyyy = STRING(fix(syyyy)+y,format='(i4)')
        index = WHERE(STRMID(yyyymm,0,4) EQ yyyy AND finite(ch4obs[*,i]),cnt)
        IF cnt EQ 12 THEN BEGIN
           seasobs[*,i]=seasobs[*,i]+ch4obs[index,i]
           seasapri[*,i]=seasapri[*,i]+ch4apri[index,i]
           IF keyword_set(post) OR keyword_set(pripost) THEN $
              seaspost[*,i]=seaspost[*,i]+ch4apost[index,i]
           nyavg[i]=nyavg[i]+1
        ENDIF
     ENDFOR
  ENDFOR
  FOR i=0,nst-1 DO BEGIN
     seasobs[*,i]=seasobs[*,i]/nyavg[i]
     seasapri[*,i]=seasapri[*,i]/nyavg[i]
     seaspost[*,i]=seaspost[*,i]/nyavg[i]
  ENDFOR

  ;; write out station data to file
  IF keyword_set(dump) THEN BEGIN
     FOR i=0,nst-1 DO BEGIN
        filename = sim.outdir + 'station_'+sim.stats[i]+'_'+sim.sn+'_'+sim.name+'_'+$
                   sim.qunc+'_tseries.txt'
        openw,lun,filename,/get_lun
        printf,lun,'yyyymm  CH4obs(ppb) CH4apri(ppb) CH4apost(ppb)'
        format = '(a6,3(f13.2))'
        FOR k=0,n_elements(yyyymm)-1 DO BEGIN
           IF finite(ch4obs[k,i]) THEN BEGIN
              printf,lun,yyyymm[k],ch4obs[k,i],ch4apri[k,i],$
                     ch4apost[k,i],format=format
           ENDIF ELSE BEGIN
              printf,lun,yyyymm[k],-9999.99,-9999.99,$
                     -9999.99,format=format
           ENDELSE
        ENDFOR
        free_lun,lun
     ENDFOR
  ENDIF

  ;; sort stations by increasing latitude
  lats = FltArr(nst)
  names = StrArr(nst)
  lons = StrArr(nst)
  FOR i=0,nst-1 DO BEGIN
     statinfo = station_info(sim.stats[i])
     lats[i]=statinfo.lat
  ENDFOR
  isort = sort(lats)
  stats = sim.stats[isort]
  lats = lats[isort]
  ch4obs   = ch4obs[*,isort]
  ch4apri  = ch4apri[*,isort]
  ch4apost = ch4apost[*,isort]
  ch4apostcat = ch4apostcat[*,*,isort]
  IF keyword_set(bg) THEN BEGIN
     ch4apribg = ch4apribg[*,isort]
     ch4apostbg = ch4apostbg[*,isort]
  ENDIF
  seasobs=seasobs[*,isort]
  seasapri=seasapri[*,isort]
  seaspost=seaspost[*,isort]

  ;; define number of figures per page and their sizes
  nfig = 6
  x1s = 0.06 & x1e = 0.69       ; start and end positions of time series panel
  x2s = 0.76 & x2e = 0.90       ; start and end positions of mean seasonal cycle
  x3s = 0.91                    ; start positions of main contribution label
  figh = 1./nfig                ; height of each figure panel
  yh = 0.76 * figh
  ys = 0.15 * figh               ; relative to figure panel bottom
  
  red = 24B
  blu = 11B

  ytitle = 'CH!D4!N (ppb)'
  months = indgen(12)+1 & xmrange=[1,12]

  ;; create arrays to store 5-yearly station biases and r2 values
  ;; for a posteriori simulation
  nperiod = 4
  r2_5yr = FltArr(nperiod,nst)
  bias_5yr = FltArr(nperiod,nst)

  cats = emiss_categories(labels=labels)

  IF keyword_set(prelim) THEN pstr = '_prelim' ELSE pstr = ''
  ;; loop over stations and plot
  FOR i=0,nst-1 DO BEGIN
     statinfo = station_info(stats[i])
     ip = (i MOD nfig)+1        ; figure number on page
     IF (i/nfig) LT 5 THEN ylab = 0.8 ELSE ylab = 0.28
     yoffs = 1.-ip*figh
     yrange = statinfo.yrange
     pos1 = [x1s,yoffs+ys,x1e,yoffs+ys+yh]
     pos2 = [x2s,yoffs+ys,x2e,yoffs+ys+yh]

     noerase = ip GT 1
     IF keyword_set(eps) AND noerase EQ 0 THEN BEGIN
        figname = 'tseries_fig'+STRING(floor(i/nfig),format='(i2.2)')+'_'+sim.sn+'_'+$
                  sim.name+'_'+string(syyyy,format='(i4)')+'_'+STRING(eyyyy,format='(i4)')+$
                  '_'+sim.qunc+pstr+suffix
        filename = psdir+figname
        open_ps,filename,/eps,/color,pssize=[26,24],tt_type='Helvetica'
     ENDIF

     ;; plot time series of monthly mean values
     plot_tseries,gvt,ch4obs[*,i],position=pos1,yrange=yrange,charsize=chs,noerase=noerase,$
                  /shades,xshade=365.25D,ytitle=ytitle,xtitle='',xthick=xthick,ythick=ythick,$
                  thick=linethick
     plot_tseries,gvt,ch4obs[*,i],psym=plotsymbol(1),/over,symsize=symsize
     IF keyword_set(prior) OR keyword_set(pripost) THEN BEGIN
        IF keyword_set(bg) THEN plot_tseries,gvt,ch4apribg[*,i],/over,color=red,thick=linethick
        plot_tseries,gvt,ch4apri[*,i],/over,color=red,thick=linethick
        plot_tseries,gvt,ch4apri[*,i],psym=plotsymbol(2),/over,color=red,symsize=symsize
        xyouts_fiveyr_detrended_r2_bias,gvt,ch4obs[*,i],ch4apri[*,i],r2=r2,bias=bias,$
                                        color=red,ypos=ylab-0.16*(ylab LT 0.5)+0.06,chs=chs
     ENDIF
     IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
        IF keyword_set(bg) THEN plot_tseries,gvt,ch4apostbg[*,i],/over,color=blu,thick=linethick
        plot_tseries,gvt,ch4apost[*,i],/over,color=blu,thick=linethick
        plot_tseries,gvt,ch4apost[*,i],psym=plotsymbol(3),/over,color=blu,symsize=symsize
        xyouts_fiveyr_detrended_r2_bias,gvt,ch4obs[*,i],ch4apost[*,i],r2=r2,bias=bias,syyyy=s5yr,eyyyy=e5yr,$
                                        color=blu,ypos=ylab-0.16*(ylab LT 0.5)-0.06,chs=chs
        r2_5yr[*,i]=r2
        bias_5yr[*,i]=bias
     ENDIF

     ;; plot mean seasonal cycle
     plot,months,seasobs[*,i],position=pos2,ytitle=ytitle,charsize=chs,/noerase,$
          thick=linethick,xrange=xmrange,/xst,yrange=yrange
     oplot,months,seasobs[*,i],psym=plotsymbol(1),symsize=symsize
     IF keyword_set(prior) OR keyword_set(pripost) THEN BEGIN
        oplot,months,seasapri[*,i],color=red,thick=linethick
        oplot,months,seasapri[*,i],psym=plotsymbol(2),color=red,symsize=symsize
        r = correlate(seasapri[*,i],seasobs[*,i])
        bias = mean(seasapri[*,i]-seasobs[*,i])
        XYOUTS,x2s+0.03,yoffs+ylab*figh,'R!U2!N='+string(r^2,format='(f5.2)'),$
               charsize=chs,/normal,alignment=0.5,color=red
        XYOUTS,x2s+0.1,yoffs+ylab*figh,'b='+string(bias,format='(f5.1)'),$
               charsize=chs,/normal,alignment=0.5,color=red
     ENDIF
     IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
        oplot,months,seaspost[*,i],color=blu,thick=linethick
        oplot,months,seaspost[*,i],psym=plotsymbol(3),color=blu,symsize=symsize
        ;; mean bias and correlation
        r = correlate(seaspost[*,i],seasobs[*,i])
        bias = mean(seaspost[*,i]-seasobs[*,i])
        XYOUTS,x2s+0.03,yoffs+(ylab-0.1)*figh,'R!U2!N='+string(r^2,format='(f5.2)'),$
               charsize=chs,/normal,alignment=0.5,color=blu
        XYOUTS,x2s+0.1,yoffs+(ylab-0.1)*figh,'b='+string(bias,format='(f5.1)'),$
               charsize=chs,/normal,alignment=0.5,color=blu
     ENDIF
     
     XYOUTS,0.4,yoffs+0.93*figh,statinfo.name+' ('+statinfo.id+'), lon = '+$
            string(statinfo.lon,format='(f7.2)')+', lat = '+string(statinfo.lat,format='(f6.2)')+$
            ', alt = '+string(statinfo.alt,format='(f6.1)')+' m',$
            charsize=chs,/normal,alignment=0.5

     ;; calculate and plot 4 most important emission categories
     mcat = mean(ch4apostcat[*,*,i],dimension=1,/nan)
     isort = sort(mcat)
     top6 = isort[sim.ntrace-6:sim.ntrace-1]
     XYOUTS,x3s,yoffs+0.8*figh,'Top 6:',alignment=0,/normal,charsize=chs
     FOR l=0,5 DO XYOUTS,x3s,yoffs+(0.7-l*0.1)*figh,cats[top6[5-l]],alignment=0,/normal,charsize=chs

     IF ip EQ nfig OR i EQ (nst-1) THEN BEGIN
        IF keyword_set(eps) THEN close_ps ELSE stop
     ENDIF
  ENDFOR


  ;; create 5-yearly maps of station biases and correlations
  IF keyword_set(map) THEN BEGIN

     FOR i=0,nperiod-1 DO BEGIN
        sstr = s5yr[i] & estr = STRING(fix(s5yr[i])+5,format='(i4)')
        
        ;; first bias plot
        load_ctb,'stephan_cold_warm20.ctb'
        biasrange = [-20.,20.]
        colrange = [1B,25B]
        scale = (colrange[1]-colrange[0])/(biasrange[1]-biasrange[0])
        IF keyword_set(eps) THEN BEGIN
           symsize=1.5
           figname = 'map_bias_'+sstr+'-'+estr+'_'+sim.sn+'_'+sim.name+'_'+sim.qunc+pstr+suffix
           filename = sim.invdir + 'FIGURES/MAPS/'+figname
           open_ps,filename,/eps,/color,pssize=[18,12],tt_type='Helvetica'
        ENDIF ELSE BEGIN
           window,xsize=900,ysize=600
           symsize=2.0
        ENDELSE
        
        map_set,londel=30,latdel=15,/robinson,position=[0.02,0.02,0.98,0.92],$
                title = 'Mean bias for period '+sstr + ' - ' + estr,charsize=1.5
        map_continents
        FOR k=0,nst-1 DO BEGIN
           statinfo = station_info(stats[k])
           color=((colrange[0]+(bias_5yr[i,k]-biasrange[0])*scale)>colrange[0])<colrange[1]
           plots,statinfo.lon,statinfo.lat,psym=plotsymbol(1),symsize=symsize,color=color
           IF abs(bias_5yr[i,k]) GT 20 THEN  plots,statinfo.lon,statinfo.lat,psym=plotsymbol(1),symsize=0.3*symsize
           XYOUTS,statinfo.lon+3,statinfo.lat-4,statinfo.ID,charsize=chs,alignment=0
        ENDFOR
        levs = biasrange[0]+indgen(25)/scale
        cols = colrange[0]+indgen(24)
        colorbar,levs,cols,/col,/right,charsize=1.1,lowleft=[0.05,0.04],xsize=0.02,ysize=0.4,$
                 format='(f5.1)',loffset=[0,0.007]

        IF keyword_set(eps) THEN close_ps

        ;; second correlation plot
        load_ctb,'bgyr18.ctb'
        r2range = [0.,1.]
        colrange = [1B,18B]
        scale = (colrange[1]-colrange[0])/(r2range[1]-r2range[0])
        IF keyword_set(eps) THEN BEGIN
           symsize=1.5
           figname = 'map_r2_'+sstr+'-'+estr+'_'+sim.sn+'_'+sim.name+'_'+sim.qunc+pstr+suffix
           filename = sim.invdir + 'FIGURES/MAPS/'+figname
           open_ps,filename,/eps,/color,pssize=[18,12],tt_type='Helvetica'
        ENDIF ELSE BEGIN
           window,xsize=900,ysize=600
           symsize=2.0
        ENDELSE
        
        map_set,londel=30,latdel=15,/robinson,position=[0.02,0.02,0.98,0.92],$
                title = 'R!U2!N for period '+sstr + ' - ' + estr,charsize=1.5
        map_continents
        FOR k=0,nst-1 DO BEGIN
           statinfo = station_info(stats[k])
           color=((colrange[0]+(r2_5yr[i,k]-r2range[0])*scale)>colrange[0])<colrange[1]
           plots,statinfo.lon,statinfo.lat,psym=plotsymbol(1),symsize=symsize,color=color
           XYOUTS,statinfo.lon+3,statinfo.lat-4,statinfo.ID,charsize=chs,alignment=0
        ENDFOR

        levs = r2range[0]+indgen(18)/scale
        cols = colrange[0]+indgen(17)
        colorbar,levs,cols,/col,/right,charsize=1.1,lowleft=[0.05,0.04],xsize=0.02,ysize=0.4,$
                 format='(f5.2)',loffset=[0,0.007]

        IF keyword_set(eps) THEN close_ps
     ENDFOR
  ENDIF
END
