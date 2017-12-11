;+
; NAME:
;
;   plot_inv_timeseries_brd
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
;  plot_inv_timeseries_brd,sim,syyyy=syyyy,eyyyy=eyyyy,prelim=prelim,prior=prior,$
;                            post=post,pripost=pripost,bg=bg,eps=eps,dump=dump     
; INPUTS:
;
;       sim              : the simulation structure, see inv_configurations.pro
;
; KEYWORD PARAMETERS:
;
;       syyyy (string)   : first year to plot
;       eyyyy (string)   : last year to plot
;       /prelim          : set this keyword to plot results of preliminary inversion
;       /prior           : set this keyword to plot obs and prior
;       /post            : set this keyword to plot obs and posterior
;       /pripost         : set this keyword to plot all
;       /bg              : set this keyword to plot backgrounds (oldest age class)
;       /eps             : set this keyword to write to eps file instead of screen
;       /dump            : set this keyword to write out station data (obs, apri, apost)
;                          to files (one file per station)
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
;   plot_inv_modelestimates
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 18 January 2012
; 
; DB 24 November 2017, major rewrite
;-

  ;; Last settings by Florian
  ;;
  ;; sim = {name:'final_sim01',$
  ;;        obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
  ;;        modeldir:'/nas/arf/output/',$
  ;;        outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
  ;;        hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
  ;;        syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,$
  ;;           0.27,0.27,0.27,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,$
  ;;           0.55,0.55,0.55,0.55,0.55,0.55,0.55,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
  ;;           0.5,0.4,0.4,0.4,0.4],ntrace:48,nage:5} ;keeppos1 change fcorr all


  ;; ;; stats includes data from 34 continuous stations, until 'cgo'
  ;; stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'mhd',   'ngl',$
  ;;                'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
  ;;                'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
  ;;                'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
  ;;                'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
  ;;                'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
  ;;                'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
  ;;                'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey',   'asc',   'cfa',$
  ;;                'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
  ;;                'cya',   'syo',   'hba']
                 

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO plot_inv_timeseries_brd,sim,syyyy=syyyy,eyyyy=eyyyy,prelim=prelim,prior=prior,$
                            post=post,pripost=pripost,bg=bg,eps=eps,dump=dump

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  load_ctb,'ive.ctb'
  sn   = STRCOMPRESS(string(fix(n_elements(sim.stats))),/REM)+'stats'
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)

  IF keyword_set(eps) THEN BEGIN
     psdir = sim.basedir + 'FIGURES/TSERIES/'
     suffix = '.eps'
     IF keyword_set(prior) THEN suffix = '_prior.eps'
     IF keyword_set(post) THEN suffix = '_posterior.eps'
     IF keyword_set(pripost) THEN suffix = '_prior_posterior.eps'
     xthick = 2 & ythick = 2
     linethick = 2
     symsize=0.7
     chs = 1.2
  ENDIF ELSE BEGIN
     window,xsize=900,ysize=950
     xthick = 2 & ythick = 2
     linethick=1
     symsize=1.0
     chs = 1.0
  ENDELSE

  IF n_elements(syyyy) EQ 0 THEN syyyy = '1990' ; STRMID(sim.syyyymm,0,4)
  IF n_elements(eyyyy) EQ 0 THEN eyyyy = STRMID(sim.eyyyymm,0,4)

  basedir  = sim.obsdir
  modeldir = sim.modeldir+sim.name+'/'
  
  ;; Create data arrays
  nst      = n_elements(sim.stats)
  nmonths  = 12
  mon      = STRING(indgen(12)+1,format='(i2.2)')
  nyears   = long(fix(eyyyy)-fix(syyyy)+1)
  ch4obs   = Fltarr(nyears*nmonths,nst)+!values.f_nan
  ch4apri  = Fltarr(nyears*nmonths,nst)+!values.f_nan
  ch4apost = Fltarr(nyears*nmonths,nst)+!values.f_nan
  IF keyword_set(bg) THEN BEGIN
     ch4apribg  = Fltarr(nyears*nmonths,nst)+!values.f_nan
     ch4apostbg = Fltarr(nyears*nmonths,nst)+!values.f_nan
  ENDIF     
  ndata    = IntArr(nyears*nmonths,nst)
  yyyymm   = StrArr(nyears*nmonths)
 
  ;; loop over the years and months and get observation and model data
  cnt = 0L
  FOR y = 0,nyears-1 DO BEGIN
     yyyy = STRING(fix(syyyy)+y,format='(i4)')
     FOR m = 0,nmonths-1 DO BEGIN
        ind = y*nmonths+m
        mm = mon[m]
        yyyymm[ind]=yyyy+mm
        read_processed_obs_data_month,sim,yyyymm[ind],ch4obs=ch4tmp
        read_processed_model_data_month,sim,yyyymm[ind],ch4recs=ch4mod
        IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
           inv_calculate_posterior_tseries,sim,yyyymm[ind],ch4obs=ch4tmp,ch4apri=ch4mod,$
                                           sa=sa,sp=sp,fcorr=fcorr,ch4post=ch4post,prelim=prelim
        ENDIF

        IF keyword_set(bg) THEN BEGIN
           ;; replace CH4 values by total of oldest age class
           iold = sim.nage-1
           ch4modbg = total(ch4mod.ch4trace[iold*sim.ntrace:sim.nage*sim.ntrace-1],1)
           ch4postbg = total(ch4post.ch4trace[iold*sim.ntrace:sim.nage*sim.ntrace-1],1)
        ENDIF

        ;; loop over the stations and assign values
        FOR i=0,nst-1 DO BEGIN
           index = WHERE(ch4tmp.name EQ sim.stats[i],cnt)
           IF cnt GT 1 THEN BEGIN
              ch4obs[ind,i]=mean(ch4tmp[index].ch4,/nan)
              ch4apri[ind,i]=mean(ch4mod[index].ch4,/nan)
              IF keyword_set(bg) THEN ch4apribg[ind,i]=mean(ch4modbg[index],/nan)
              IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
                 ch4apost[ind,i]=mean(ch4post[index].ch4,/nan)
                 IF keyword_set(bg) THEN ch4apostbg[ind,i]=mean(ch4postbg[index],/nan)
              ENDIF
           ENDIF ELSE IF cnt EQ 1 THEN BEGIN
              ch4obs[ind,i]=ch4tmp[index].ch4
              ch4apri[ind,i]=ch4mod[index].ch4
              IF keyword_set(bg) THEN ch4apribg[ind,i]=ch4modbg[index]
              IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
                 ch4apost[ind,i]=ch4post[index].ch4
                 IF keyword_set(bg) THEN ch4apostbg[ind,i]=ch4postbg[index]
              ENDIF
           ENDIF
           ndata[ind,i]=cnt
        ENDFOR
     ENDFOR
  ENDFOR

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
        filename = sim.outdir + 'station_'+sim.stats[i]+'_'+sn+'_'+sim.name+'_'+$
                   qunc+'_tseries.txt'
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
  IF keyword_set(bg) THEN BEGIN
     ch4apribg = ch4apribg[*,isort]
     ch4apostbg = ch4apostbg[*,isort]
  ENDIF
  seasobs=seasobs[*,isort]
  seasapri=seasapri[*,isort]
  seaspost=seaspost[*,isort]

  ;; define number of figures per page and their sizes
  nfig = 6
  x1s = 0.08 & x1e = 0.77       ; start and end positions of time series panel
  x2s = 0.83 & x2e = 0.98       ; start and end positions of mean seasonal cycle
  figh = 1./nfig                ; height of each figure panel
  yh = 0.76 * figh
  ys = 0.15 * figh               ; relative to figure panel bottom
  
  red = 24B
  blu = 11B

  ytitle = 'CH!D4!N (ppb)'
  months = indgen(12)+1 & xmrange=[1,12]

  ;; loop over stations and plot
  FOR i=0,nst-1 DO BEGIN
     statinfo = station_info(stats[i])
     ip = (i MOD nfig)+1        ; figure number on page
     yoffs = 1.-ip*figh
     yrange = statinfo.yrange
     pos1 = [x1s,yoffs+ys,x1e,yoffs+ys+yh]
     pos2 = [x2s,yoffs+ys,x2e,yoffs+ys+yh]

     noerase = ip GT 1
     IF keyword_set(eps) AND noerase EQ 0 THEN BEGIN
        figname = 'tseries_fig'+STRING(floor(i/nfig),format='(i2.2)')+'_'+sn+'_'+$
                  sim.name+'_'+string(syyyy,format='(i4)')+'_'+STRING(eyyyy,format='(i4)')+$
                  '_'+qunc+suffix
        filename = psdir+figname
        open_ps,filename,/eps,/color,pssize=[24,24],tt_type='Helvetica'
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
     ENDIF
     IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
        IF keyword_set(bg) THEN plot_tseries,gvt,ch4apostbg[*,i],/over,color=blu,thick=linethick
        plot_tseries,gvt,ch4apost[*,i],/over,color=blu,thick=linethick
        plot_tseries,gvt,ch4apost[*,i],psym=plotsymbol(3),/over,color=blu,symsize=symsize
     ENDIF

     ;; plot mean seasonal cycle
     plot,months,seasobs[*,i],position=pos2,ytitle=ytitle,charsize=chs,/noerase,$
          thick=linethick,xrange=xmrange,/xst,yrange=yrange
     oplot,months,seasobs[*,i],psym=plotsymbol(1),symsize=symsize
     IF keyword_set(prior) OR keyword_set(pripost) THEN BEGIN
        oplot,months,seasapri[*,i],color=red,thick=linethick
        oplot,months,seasapri[*,i],psym=plotsymbol(2),color=red,symsize=symsize
     ENDIF
     IF keyword_set(post) OR keyword_set(pripost) THEN BEGIN
        oplot,months,seaspost[*,i],color=blu,thick=linethick
        oplot,months,seaspost[*,i],psym=plotsymbol(3),color=blu,symsize=symsize
     ENDIF
     
     XYOUTS,0.4,yoffs+0.93*figh,statinfo.name+' ('+statinfo.id+'), lon = '+$
            string(statinfo.lon,format='(f7.2)')+', lat = '+string(statinfo.lat,format='(f6.2)')+$
            ', alt = '+string(statinfo.alt,format='(f6.1)')+' m',$
            charsize=chs,/normal,alignment=0.5

     IF ip EQ nfig OR i EQ (nst-1) THEN BEGIN
        IF keyword_set(eps) THEN close_ps ELSE stop ;wait,2
     ENDIF
  ENDFOR

END
