;+
; NAME:
;  plot_fcorr
; 
; PURPOSE:
;  Plot evolution of scaling factors for the oldest age class of all emission
;  categories, which is an indication of the long-term deviations of posterior
;  from prior emissions.
;
;  Set keyword /prelim to plot results of first preliminary inversion
;  Set keyword /eps to create a postscript plot in sim.invdir + 'FIGURES/FCORR/'
;-
PRO plot_fcorr,sim,prelim=prelim,yrange=yrange,eps=eps

  load_ctb,'bgyr18.ctb'
  result = read_inv_results_netcdf(sim,prelim=prelim,/emisonly)
  IF size(result,/type) EQ 2 THEN RETURN
  fcorr = result.scalef

  cats = emiss_categories()
  cols = emiss_colors()

  yyyymm =  get_yyyymm(sim)
  gvt = dtg2gvtime(yyyymm+'01')

  ytitle = 'Scaling factors (-)'
  pos = [0.1,0.36,0.95,0.95]
  IF n_elements(yrange) EQ 0 THEN yrange=[0,3]
  xrange = dtg2gvtime(['19900101','20130101'])

  IF keyword_set(eps) THEN BEGIN
     psdir = sim.invdir + 'FIGURES/FCORR/'
     figname = 'fcorr_'+sim.sn+'_'+sim.name+'_'+sim.syyyymm+'_'+sim.eyyyymm+'_'+sim.qunc+'.eps'
     filename = psdir+figname
     open_ps,filename,/eps,/color,pssize=[20,20],tt_type='Helvetica'
     linethick=4
  ENDIF ELSE linethick=2

  chs = 1.5
  basetime = julday(1,1,1,0,0,0)
  FOR i=0,sim.ntrace-2 DO BEGIN
     IF i EQ 0 THEN BEGIN
        plot_tseries,gvt,fcorr[*,i],position=pos,yrange=yrange,charsize=chs,/shades,xshade=365.25D,$
                     ytitle=ytitle,xtitle='',xthick=xthick,ythick=ythick,thick=linethick,$
                     xrange=xrange,/nodata,grey=25
     ENDIF
     plot_tseries,gvt,fcorr[*,i],color=cols[i],thick=linethick,/over
     mdiff = max(abs(fcorr[*,i]-1.0))
     IF mdiff GT 0.2 THEN BEGIN
        index = WHERE(abs(fcorr[*,i]-1.0) EQ max(abs(fcorr[*,i]-1.0)))
        IF fcorr[index,i] LT 1 THEN offs = -0.05 ELSE offs = 0.05
        XYOUTS,gvt[index]+basetime-10,fcorr[index,i]+offs,cats[i],alignment=0.5,charsize=chs
     ENDIF
  ENDFOR

  plot_category_legend,position=[0,0.1,0.3,0.9]

  IF keyword_set(eps) THEN close_ps
END
