;+
; NAME:
;
;   plot_chisquare_statistics
;
; PURPOSE:
;
;   Create a plot of the chisquare statistics and print mean zlen (should be around 1)
;   and total log-likelihood (should be as large as possible)
;
; CATEGORY:
;
;   MAIOLICA-2 Kalman smoother
;
; CALLING SEQUENCE:
;
;  plot_chisquare_statistics,sim,s2=s2,rapriori=rapriori,eps=eps
;
; INPUTS:
;
;  sim   (STRUCTURE) : the simulation information structure, see inv_configurations
;
; KEYWORD PARAMETERS:
;
;  s2    (STRUCTURE) : an optional second simulation info structure for comparision
;  /rapriori         : set this keyword to plot statistics for first inversion based
;                      on model-data mismatches calculated fror a priori simulation
;  /eps              : set this keyword to create postscript files in 
;                      sim.invdir + 'FIGURES/CHISTATS/'
;
; OUTPUTS:
;
;  none
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
;   sim = inv_configurations(run='28.4',sconfig='flask_DLR2',ok=ok)
;   plot_chisquare_statistics,sim=sim
;
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 27 Dec 2017: first implementation
;   DB, 06 Jan 2018: adjusted to new netcdf output
;-
PRO plot_chisquare_statistics,sim,s2=s2,prelim=prelim,eps=eps

  IF n_elements(sim) EQ 0 THEN RETURN
  
  psdir = sim.invdir + 'FIGURES/CHISTAT/'

  xrange = dtg2gvtime(['19900101','20130101'])

  result = read_inv_results_netcdf(sim,prelim=prelim,/statonly)
  IF size(result,/type) EQ 2 THEN RETURN
  zlen = result.zlen
  dllh = result.dllh
  nobse = result.nobs
  yyyymm = result.yyyymm
  gvt = dtg2gvtime(yyyymm+'01')

  IF keyword_set(eps) THEN BEGIN
     figname = 'chisquare_'+sim.sn+'_'+sim.name+'_'+string(sim.syyyymm,format='(i6)')+'_'+$
               STRING(sim.eyyyymm,format='(i6)')+'_'+sim.qunc+'.eps'
     filename = psdir+figname
     open_ps,filename,/eps,/color,pssize=[16,12],tt_type='Helvetica'
     linethick=4
     chs = 1.5
     greek = '!9'
     position=[0.1,0.12,0.9,0.95]
  ENDIF ELSE BEGIN
     linethick = 2
     chs = 1.2
     greek = '!7'
     position=[0.09,0.1,0.91,0.95]
  ENDELSE

  ytitle = greek + 'c!3!Di!U2!N/N!Di'
  plot_tseries,gvt,zlen,ytitle=ytitle,charsize=chs,/nodata,position=position,$
               yrange=[0,3],ystyle=9,xrange=xrange
  plot_tseries,gvt,zlen,thick=linethick,/over

  red = CGCOLOR('RED')

  IF n_elements(s2) NE 0 AND NOT keyword_set(eps) THEN BEGIN

     result = read_inv_results_netcdf(sim2,prelim=prelim,/statonly)
     IF size(result,/type) EQ 2 THEN RETURN
     zlen2 = result.zlen
     dllh2 = result.dllh
     nobse2 = result.nobs
     yyyymm = result.yyyymm

     plot_tseries,gvt,zlen2,color=red,/over

  ENDIF

  IF n_elements(s2) EQ 0 OR keyword_set(eps) THEN BEGIN
     axis,/yaxis,/save,yrange=[0,350],ytitle='Observations N!Di',charsize=chs,color=red
     plot_tseries,gvt,nobse,thick=linethick,/over,color=red
  ENDIF ELSE BEGIN
     axis,/yaxis,/save,yrange=[0,300],ytitle='Observations N!Di',charsize=chs
     plot_tseries,gvt,nobse,thick=linethick,/over,linestyle=2
  ENDELSE
     
  IF n_elements(s2) NE 0 AND NOT keyword_set(eps) THEN BEGIN
     plot_tseries,gvt,nobse2,thick=linethick,/over,color=red,linestyle=2
  ENDIF

  print,'Statistics for '+sim.sn+' '+sim.qunc
  print,'Mean zlen = ',mean(zlen)
  print,'Total dllh = ',total(dllh)
  XYOUTS,0.2,0.87,'Mean '+ytitle + '!N = ' + STRING(mean(zlen),format='(f6.2)'),/normal,charsize=chs
  XYOUTS,0.2,0.82,'Log-likelihood = ' + STRING(total(dllh),format='(i9)'),/normal,charsize=chs

  IF n_elements(s2) NE 0 AND NOT keyword_set(eps) THEN BEGIN
     print,''
     print,'Statistics for '+s2.sn+' '+s2.qunc
     print,'Mean zlen2 = ',mean(zlen2)
     print,'Total dllh2 = ',total(dllh2)
     XYOUTS,0.2,0.74,'Mean '+ytitle + '!N s2 = ' + STRING(mean(zlen2),format='(f6.2)'),/normal,charsize=chs
     XYOUTS,0.2,0.69,'Log-likelihood s2 = ' + STRING(total(dllh2),format='(i9)'),/normal,charsize=chs
  ENDIF

  IF keyword_set(eps) THEN close_ps

END
