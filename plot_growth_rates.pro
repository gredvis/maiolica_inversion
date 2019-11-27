;+
; NAME:
;
;   plot_growth_rates
;
; PURPOSE:
;
;   Calculate global,tropical, or hemispheric growth rates
;   using the weekly observational and model data for individual categories
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;   plot_growth_rates,sim,lat=lat
;
; INPUTS:
;
;   sim: structure containing directory and parameter information for simulation
;
; KEYWORD PARAMETERS:
;
; /eps: set keyword to generate postscript files in directory
;       sim.basedir + 'FIGURES/GROWTHRATES/'
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
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;   Dominik Brunner (DB), Empa
;   DB, 17 Dec 2017: first implementation, modified version of Tina Schnadt's
;                    plot_station_methane_categories_MAIOLICA_apriori
;   DB, 06 Jan 2018: added keyword /prelim to allow plotting results of preliminary inversion
;-

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO plot_growth_rates,sim,prelim=prelim,sort=sort,eps=eps

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  
  IF keyword_set(sort) THEN BEGIN
     load_ctb,'/home/brd134/IDL/arfeuille_final/maiolica_emiss_categories.ctb'
  ENDIF ELSE BEGIN
     load_ctb,'ive.ctb'
  ENDELSE

  IF keyword_set(eps) THEN BEGIN
     psdir = sim.basedir + 'FIGURES/GROWTHRATES/'
     suffix = '.eps'
     xthick = 2 & ythick = 2
     linethick = 2
     symsize=0.7
     chs = 1.2
  ENDIF ELSE BEGIN
     window,xsize=900,ysize=800
     xthick = 2 & ythick = 2
     linethick=2
     symsize=1.0
     chs = 1.0
  ENDELSE

  IF n_elements(syyyy) EQ 0 THEN syyyy = STRMID(sim.syyyymm,0,4)
  IF n_elements(eyyyy) EQ 0 THEN eyyyy = STRMID(sim.eyyyymm,0,4)
  
  ;; read observations and prior and posterior model output at all stations
  read_inv_station_output_netcdf,sim,prelim=prelim,yyyymm=yyyymm,ndata=ndata,$
                                 ch4obs=ch4obs,ch4apri=ch4apri,ch4post=ch4apost,$
                                 catapri=ch4apri_cat,catpost=ch4apost_cat,$
                                 cats=cats,stats=stats,ok=ok

  IF NOT ok THEN RETURN

  ;; Create data arrays
  nst      = n_elements(sim.stats)
  nmonths  = 12
  mon      = STRING(indgen(12)+1,format='(i2.2)')
  nyears   = long(fix(eyyyy)-fix(syyyy)+1)
  ntot = n_elements(yyyymm)

  gvt = dtg2gvtime(yyyymm)

  ;;----------------------------------------------------------------------------------
  ;; loop over latitude bands, calculate 12-month running mean time series, and plot
  ;;----------------------------------------------------------------------------------
  latc = FltArr(nst)
  FOR i=0,nst-1 DO BEGIN
     statinfo = station_info(sim.stats[i])
     latc[i]=statinfo.lat
  ENDFOR

  ;; stations within given latitude band +/-15 deg
  ; divide into latitude bands
  ind_NH   = WHERE(latc gt 30.,cnh)
  ind_trop = WHERE(latc le 30. and latc ge -30.,ctrop)
  ind_SH   = WHERE(latc lt -30.,csh)
  ind_glob = WHERE(latc ge -90. and latc le 90.,cglob)
  FOR lat = 0,3 DO BEGIN

     CASE lat OF 
        0: BEGIN
           ind = ind_NH & region = 'NH'
        END
        1: BEGIN
           ind = ind_trop & region = 'TROPICS'
        END
        2: BEGIN
           ind = ind_SH & region = 'SH'
        END
        3: BEGIN
           ind = ind_glob & region = 'GLOBAL'
        END
     ENDCASE

     mch4obs=mean(ch4obs[*,ind],dimension=2,/nan)
     mch4apri=mean(ch4apri[*,ind],dimension=2,/nan)
     mch4apost=mean(ch4apost[*,ind],dimension=2,/nan)
     mch4apri_cat = Fltarr(ntot,sim.ntrace)+!values.f_nan
     mch4apost_cat = Fltarr(ntot,sim.ntrace)+!values.f_nan
     FOR k=0,sim.ntrace-1 DO BEGIN
        mch4apri_cat[*,k]=mean(ch4apri_cat[*,k,ind],dimension=3,/nan)
        mch4apost_cat[*,k]=mean(ch4apost_cat[*,k,ind],dimension=3,/nan)
     ENDFOR
     
     ;; 12-month running means
     sch4obs   = smooth(mch4obs,12,/EDGE_TRUNCATE) ; running mean of CH4 curve
     sch4apri  = smooth(mch4apri,12,/EDGE_TRUNCATE)
     sch4apost = smooth(mch4apost,12,/EDGE_TRUNCATE)
     sch4apri_cat = Fltarr(ntot,sim.ntrace)+!values.f_nan
     sch4apost_cat = Fltarr(ntot,sim.ntrace)+!values.f_nan
     FOR k=0,sim.ntrace-1 DO BEGIN
        sch4apri_cat[*,k]=smooth(mch4apri_cat[*,k],12,/EDGE_TRUNCATE)
        sch4apost_cat[*,k]=smooth(mch4apost_cat[*,k],12,/EDGE_TRUNCATE)
     ENDFOR
     
     ;; annual growth rates
     dch4obs   = FltArr(ntot)
     dch4apri  = FltArr(ntot)
     dch4apost  = FltArr(ntot)
     dch4apri_cat = Fltarr(ntot,sim.ntrace)
     dch4apost_cat = Fltarr(ntot,sim.ntrace)
     FOR i=6,ntot-7 DO BEGIN
        dch4obs[i]   = sch4obs[i+6]-sch4obs[i-6]
        dch4apri[i]  = sch4apri[i+6]-sch4apri[i-6]
        dch4apost[i] = sch4apost[i+6]-sch4apost[i-6]
        FOR k=0,sim.ntrace-1 DO BEGIN
           dch4apri_cat[i,k] = sch4apri_cat[i+6,k]-sch4apri_cat[i-6,k]
           dch4apost_cat[i,k]= sch4apost_cat[i+6,k]-sch4apost_cat[i-6,k]
        ENDFOR
     ENDFOR
     
     
     ;;----------------------------------------------------------------------
     ;;  Plot figures for a priori and a posteriori growth rates
     ;;----------------------------------------------------------------------
     
     ;; define number of figures per page and their sizes
     nfig = 3
     x1s = 0.08 & x1e = 0.95    ; start and end position on x-axis
     figh = 1./nfig             ; height of each figure panel
     yh = 0.85 * figh
     ys = 0.1 * figh            ; relative to figure panel bottom
     
     red = 24B
     blu = 11B
     
     ytitle = 'CH!D4!N growth rate (ppb/yr)'
     
     tind = WHERE(yyyymm GE 199007 AND yyyymm LE 201206)
     
     ;; emission categories and colors
     IF keyword_set(sort) THEN BEGIN
        sort_emiss_categories,ocats=cat,oind=oind,ocol=col
        grey=99B 
     ENDIF ELSE BEGIN
        cat = emiss_categories()
        col = emiss_colors()
        oind = indgen(n_elements(cat))
        grey = cgcolor("LIGHTGRAY")
     ENDELSE

     xthick = 2 & ythick = 2 & linethick=2
     IF keyword_set(eps) THEN BEGIN
        figname = 'ch4_growth_rate_'+sim.sn+'_'+sim.name+'_'+string(syyyy,format='(i4)')+'_'+$
                  STRING(eyyyy,format='(i4)')+'_'+sim.qunc+'_'+region+suffix
        filename = psdir+figname
        open_ps,filename,/eps,/color,pssize=[24,20],tt_type='Helvetica'
        linethick=4
     ENDIF
     
     ;; first a priori panel
     ip = 1
     yoffs = 1.-ip*figh
     yrange = [-25,25]
     pos1 = [x1s,yoffs+ys,x1e,yoffs+ys+yh]
     xrange = dtg2gvtime(['19900101','20130101'])
     
     ;; plot time series of monthly growth rates
     plot_tseries,gvt[tind],dch4obs[tind],position=pos1,yrange=yrange,charsize=chs,noerase=noerase,$
                  /shades,xshade=365.25D,ytitle=ytitle,xtitle='',xthick=xthick,ythick=ythick,$
                  thick=linethick,/nodata,grey=grey,xstyle=5,ystyle=5,xrange=xrange
     
     ;; loop over source categories and plot accumulated contributions
     posacc = FltArr(ntot)
     negacc = FltArr(ntot)
     lastposacc = posacc & lastnegacc = negacc
     
     basetime = julday(1,1,1,0,0,0)
     time = gvt + basetime
     
     FOR k=0,sim.ntrace-2 DO BEGIN
        pind  = WHERE(dch4apri_cat[*,oind[k]] GT 0.,cpos)
        IF cpos GT 0 THEN posacc[pind] = posacc[pind] + dch4apri_cat[pind,oind[k]]
        polyfill,[time[tind],reverse(time[tind])],[lastposacc[tind],reverse(posacc[tind])],color=col[k]
        lastposacc = posacc
        nind  = WHERE(dch4apri_cat[*,oind[k]] LT 0.,cneg)
        IF cneg GT 0 THEN negacc[nind] = negacc[nind] + dch4apri_cat[nind,oind[k]]
        polyfill,[time[tind],reverse(time[tind])],[lastnegacc[tind],reverse(negacc[tind])],color=col[k]
        lastnegacc = negacc
     ENDFOR
     
     plot_tseries,gvt[tind],dch4obs[tind],position=pos1,yrange=yrange,charsize=chs,/noerase,/ystyle,/xstyle,$
                  ytitle=ytitle,xtitle='',xthick=xthick,ythick=ythick,thick=linethick,xrange=xrange
     
     plot_tseries,gvt[tind],dch4apri[tind],/over,thick=linethick,color=cgcolor("RED")
     
     
     ;; second a posteriori panel
     ip = 2
     yoffs = 1.-ip*figh
     pos1 = [x1s,yoffs+ys,x1e,yoffs+ys+yh]
     
     ;; plot time series of monthly growth rates
     plot_tseries,gvt[tind],dch4obs[tind],position=pos1,yrange=yrange,charsize=chs,/noerase,$
                  /shades,xshade=365.25D,ytitle=ytitle,xtitle='',xthick=xthick,ythick=ythick,$
                  thick=linethick,/nodata,grey=grey,xstyle=5,ystyle=5,xrange=xrange
     
     ;; loop over source categories and plot accumulated contributions
     posacc = FltArr(ntot)
     negacc = FltArr(ntot)
     lastposacc = posacc & lastnegacc = negacc
     
     basetime = julday(1,1,1,0,0,0)
     time = gvt + basetime
     
     FOR k=0,sim.ntrace-2 DO BEGIN
        pind  = WHERE(dch4apost_cat[*,oind[k]] GT 0.,cpos)
        IF cpos GT 0 THEN posacc[pind] = posacc[pind] + dch4apost_cat[pind,oind[k]]
        polyfill,[time[tind],reverse(time[tind])],[lastposacc[tind],reverse(posacc[tind])],color=col[k]
        lastposacc = posacc
        nind  = WHERE(dch4apost_cat[*,oind[k]] LT 0.,cneg)
        IF cneg GT 0 THEN negacc[nind] = negacc[nind] + dch4apost_cat[nind,oind[k]]
        polyfill,[time[tind],reverse(time[tind])],[lastnegacc[tind],reverse(negacc[tind])],color=col[k]
        lastnegacc = negacc
     ENDFOR
     
     plot_tseries,gvt[tind],dch4obs[tind],position=pos1,yrange=yrange,charsize=chs,/noerase,/ystyle,/xstyle,$
                  ytitle=ytitle,xtitle='',xthick=xthick,ythick=ythick,thick=linethick,xrange=xrange
     
     plot_tseries,gvt[tind],dch4apost[tind],/over,thick=linethick,color=cgcolor("RED")
     
     plot_category_legend,position=[0,0.1,0.3,0.9],sort=sort
     
     IF keyword_set(eps) THEN close_ps

  ENDFOR ;loop over the latitudes

END
