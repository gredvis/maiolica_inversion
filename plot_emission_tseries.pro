;+
; NAME:
;
;   plot_emission_tseries
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
;   plot_emission_tseries,sim,prelim=prelim,eps=eps
;
; INPUTS:
;
;   sim: structure containing directory and parameter information for simulation
;
; KEYWORD PARAMETERS:
;
; /eps: set keyword to generate postscript files in directory
;       sim.basedir + 'FIGURES/EMISSIONS/'
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
;   DB, 06 Jan 2018: first implementation
;-

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO plot_emission_tseries,sim,prelim=prelim,eps=eps

  result = read_inv_results_netcdf(sim,prelim=prelim)
  ;; sort_emiss_categories,ocats=ocats,oind=oind,ocol=ocol
  
  load_ctb,'/home/brd134/IDL/arfeuille_final/maiolica_emiss_categories.ctb'
     
  cats = emiss_categories()
  maincats = emiss_categories(/main)
  nmain = n_elements(maincats)
  nt = n_elements(result.yyyymm)

  escale = 365.25/1e9

  grey = 99B
  xrange = dtg2gvtime(['19900101','20130101'])
  xchs = 1.5
  ytitle = 'Emissions (Tg/yr)'
  linethick=2
  red = 29B
  orange = 35B
  lorange = 37B
  yellow = 39B

  pos1 = [0.1,0.1,0.7,0.92]
  pos2 = [0.75,0.1,0.95,0.92]
  chs = 1.5

  ;; first loop over main categories and plot
  FOR i=0,nmain-5 DO BEGIN

     index = WHERE(strpos(cats,maincats[i]) NE -1,complement=complement)
     
     eapri = total(result.emis_apri[*,index],2)*escale
     eapost = total(result.emis_apost[*,index],2)*escale
     eapost1 = total(result.emis_apost1[*,index],2)*escale
     eapost2 = total(result.emis_apost2[*,index],2)*escale
     eapost2[nt-2:nt-1]=!values.f_nan
     eapost3 = total(result.emis_apost3[*,index],2)*escale
     eapost2[nt-3:nt-1]=!values.f_nan

     ;; smooth versions
     width=25
     smeapri = smooth(eapri,width,/edge_truncate)
     smeapost = smooth(eapost,width,/edge_truncate)
     smeapost1 = smooth(eapost1,width,/edge_truncate)
     smeapost2 = smooth(eapost2,width,/edge_truncate)
     smeapost3 = smooth(eapost3,width,/edge_truncate)

     ;; compute uncertainties
     uapri = FltArr(nt)
     uapost = FltArr(nt)
     uapost1 = FltArr(nt)
     uapost2 = FltArr(nt)
     uapost3 = FltArr(nt)
     
     FOR t=0,nt-1 DO BEGIN
        emis = reform(result.emis_apri[t,*])*escale
        emis[complement]=0.
        uapri[t] = total(emis*sim.scaleq)

        emis = reform(result.emis_apost[t,*])*escale
        emis[complement]=0.
        uapost[t] = sqrt(transpose(emis)#reform(result.cov_apost[t,*,*])#emis)

        emis = reform(result.emis_apost1[t,*])*escale
        emis[complement]=0.
        uapost1[t] = sqrt(transpose(emis)#reform(result.cov_apost1[t,*,*])#emis)
       
        emis = reform(result.emis_apost2[t,*])*escale
        emis[complement]=0.
        uapost2[t] =  sqrt(transpose(emis)#reform(result.cov_apost2[t,*,*])#emis)
        
        emis = reform(result.emis_apost3[t,*])*escale
        emis[complement]=0.
        uapost3[t] =  sqrt(transpose(emis)#reform(result.cov_apost3[t,*,*])#emis)
     ENDFOR

     ymin = min([eapri,eapost,eapost1,eapost2,eapost3],max=ymax)
     yrange = [ymin,ymax]
     yrange = [0,ymax]
     gvt = dtg2gvtime(result.yyyymm+'01')
     plot_tseries,gvt,eapri,xrange=xrange,yrange=yrange,ytitle=ytitle,grey=grey,/shade,$
                  xshade=365.25D,charsize=chs,/nodata,position=pos1
     plot_tseries,gvt,eapri,thick=linethick,/over

     plot_tseries,gvt,eapost,color=red,thick=linethick,/over
     plot_tseries,gvt[0:nt-1],eapost1[0:nt-1],color=yellow,/over
     plot_tseries,gvt[0:nt-2],eapost2[0:nt-2],color=lorange,/over
     plot_tseries,gvt[0:nt-3],eapost3[0:nt-3],color=orange,/over
    
     plot_tseries,gvt[0:nt-7],smeapri[0:nt-7],thick=2*linethick,/over
     plot_tseries,gvt[0:nt-7],smeapost[0:nt-7],thick=2*linethick,/over,color=red
    

     ;; mean seasonal cycle of emissions
     nmonth = 12
     months = STRING(indgen(nmonth)+1,format='(i2.2)')
     seasapri=FltArr(nmonth)
     seasapost=FltArr(nmonth)
     seasapost1=FltArr(nmonth)
     seasapost2=FltArr(nmonth)
     seasapost3=FltArr(nmonth)
 
     FOR t=0,nmonth-1 DO BEGIN
        ind = WHERE(STRMID(result.yyyymm,4,2) EQ months[t])
        seasapri[t] = mean(eapri[ind])
        seasapost[t] = mean(eapost[ind])
        seasapost1[t] = mean(eapost1[ind])
        ind2 = WHERE(STRMID(result.yyyymm,4,2) EQ months[t] AND STRMID(result.yyyymm,0,4) LT '2012')
        seasapost2[t] = mean(eapost2[ind2])
        ind3 = WHERE(STRMID(result.yyyymm,4,2) EQ months[t] AND STRMID(result.yyyymm,0,4) LT '2011')
        seasapost3[t] = mean(eapost3[ind])
     ENDFOR
     
     ;; plot mean seasonal cycle
     plot,fix(months),seasapri,position=pos2,ytitle='',charsize=chs,/noerase,$
          thick=linethick,xrange=[1,12],/xst,yrange=yrange,/nodata,$
          xtitle='Month'
     oplot,fix(months),seasapri,thick=linethick
     oplot,fix(months),seasapost,thick=linethick,color=red
     oplot,fix(months),seasapost1,color=yellow
     oplot,fix(months),seasapost2,color=lorange
     oplot,fix(months),seasapost3,color=orange


     XYOUTS,0.4,0.94,maincats[i],charsize=1.5*chs,/normal,alignment=0.5


     ;; plot uncertainties
     plot_tseries,gvt,uapri,xrange=xrange,ytitle=ytitle,grey=grey,/shade,$
                  xshade=365.25D,charsize=chs,/nodata,position=pos1
     plot_tseries,gvt,uapri,thick=linethick,/over

     plot_tseries,gvt,uapost,color=red,thick=linethick,/over
     plot_tseries,gvt,uapost1,color=yellow,thick=linethick,/over
     plot_tseries,gvt,uapost2,color=lorange,thick=linethick,/over
     plot_tseries,gvt,uapost3,color=orange,thick=linethick,/over

  ENDFOR
  
  ;; i=20
  ;; udiag = FltArr(nt) & udiag1 = udiag & udiag2 = udiag & udiag3 = udiag
  ;; FOR k=0,nt-1 DO BEGIN
  ;;    udiag[k]=sqrt(result.cov_apost[k,i,i])
  ;;    udiag1[k]=sqrt(result.cov_apost1[k,i,i])
  ;;    udiag2[k]=sqrt(result.cov_apost2[k,i,i])
  ;;    udiag3[k]=sqrt(result.cov_apost3[k,i,i])
  ;; ENDFOR

END
