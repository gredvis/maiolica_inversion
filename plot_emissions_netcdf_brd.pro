;+
; NAME:
;
;   plot_inv_emissions_anomalies_paper
;
; PURPOSE:
;
;   Plot aposteriori emissions anomalies over time
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  plot_inv_emissions,sim=sim
;                            
; INPUTS:
;
;       ya      (integer): start year of inversions
;       ye      (integer): end year of inversion
;       sa        (float): apriori emissions
;       sp        (float): aposteriori emissions
;       stations (string): stations to be evaluated
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
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
;-
PRO read_montzka_series,montzka=montzka

  floatmonth = [0.041667,0.125,0.208333,0.291667,0.375,0.458333,0.541667,0.625,0.708333,0.791667,0.875,0.958333]
  mon        = ['01','02','03','04','05','06','07','08','09','10','11','12']
  line     = ''
  addfile  = '/home/spc134/DATA/OH_anomalies_montzkaetal_2011.txt'
  nadd     = FILE_LINES(addfile)
  ndat     = nadd-6
  yyyymm   = StrArr(ndat)
  rmontzka = FltArr(ndat)
  openr,lun,addfile,/get_lun
  FOR i=0,5 DO readf,lun,line ; read header lines
  FOR i=0,ndat-1 DO BEGIN
    readf,lun,line
    result      = STRSPLIT(line,/EXTRACT)
    yyyy        = STRMID(result[0],0,4)
    h           = float(STRMID(result[0],4,7))
    ind         = WHERE(h eq floatmonth,c)
    IF c eq 1L THEN mm = mon[ind]
    yyyymm[i]   = yyyy+mm
    rmontzka[i] = result[1]*100.         
  ENDFOR
  free_lun,lun
  ; only consider time series from year 1994 onward
  ind = WHERE(fix(STRMID(yyyymm,0,4)) ge 1994,c)
  montzka    = FltArr(c)
  montzka[*] = rmontzka[ind]   
END
;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO make_emissions_netcdf,sim,rel=rel,stat_selection=stat_selection

  print, 'plot_inv_emissions_anomalies'

  print, 'Analyse sn ', sim.sn, ' stations'
   
  ;; read ENSO anomalies
  ;;  ensofile = '/home/spc134/DATA/monthly_enso_index_mei_2001-2008.txt'
  ensofile = '/home/arf/pers/IDL/urmel/INVERSION/PAPER/monthly_enso_index_mei_1990-2012.txt' 
  ;;http://www.esrl.noaa.gov/psd/data/correlation/mei.data
  ;;ensofile = '/home/spc134/DATA/southernoscillation_index.txt'
  ;;ensofile = '/home/spc134/DATA/globalsstenso_index.txt'
  openr,lun,ensofile,/get_lun
  nyr     = 2012-1990+1
  nenso   = nyr*12
  line    = ''
  enso    = FltArr(nenso)
  a       = 0 & e = 0
  FOR in=0,nyr-1 DO BEGIN
     readf,lun,line
     e += 12
     result = STRSPLIT(line,/EXTRACT)
     enso[a:e-1] = result[1:12]
     a += 12
  ENDFOR
  free_lun,lun
  
  ;;**********************************
  ;; GENERAL PARAMETERS AND VARIABLES
  ;;**********************************
  ntrace   = 48
  nyears   = fix(strmid(sim.eyyyymm,0,4))-fix(strmid(sim.syyyymm,0,4))+1
  nmonths  = 12
  syyyy = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n = eyyyy*12L+emm-(syyyy*12L+smm)+1
  
  ;; conversion factor from kg/d to Tg/day
  fact = 1.e-9
  
  syra  = sim.syyyymm
  syre  = sim.eyyyymm
  
  dir      = '/home/arf/pers/IDL/urmel/INVERSION/FINAL/'
  ;;dir      = '/home/arf/pers/IDL/urmel/INVERSION/'
  file     = dir+'inv_output_weekly_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'_nov12.txt'  
  IF keyword_set(special) THEN $
     file     = dir+'inv_output_weekly_special_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'_nov12.txt'  
  IF keyword_set(special) THEN $ 
     ctrlfile = dir+'inv_output_weekly_special_'+sim.sn+'_'+sctrl+'_'+syra+'-'+syre+'_'+sim.qunc+'_nov12.txt' $
  ELSE $
     ctrlfile = dir+'inv_output_weekly_'+sim.sn+'_'+sctrl+'_'+syra+'-'+syre+'_'+sim.qunc+'_nov12.txt'  
  
  IF keyword_set(flask) THEN $
     flaskfile = dir+'inv_output_weekly_flask_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'_nov12.txt'
  
  
;ctrlfile =dir+'inv_output_weekly_'+sim.sn+'_'+sctrl+'_'+syra+'-'+syre+'_opt35.0000_nov12.txt' 
;stop
  
                                ;nall = 1
  nall = 2 
  IF keyword_set(flask) THEN nall = 3
  print, 'nall = ', nall
  
  fcorr    = FltArr(n,nall)
  fcorr    = FltArr(n,ntrace,nall) ;new 
  sa       = DblArr(n,ntrace,nall)
  sp       = DblArr(n,ntrace,nall)    
  FOR i=0,nall-1 DO BEGIN
     
     IF i eq 0 THEN file = file
     IF i eq 1 THEN file = ctrlfile
     IF i eq 2 THEN file = flaskfile 
     
     help = FltArr(n) & sahelp = DblArr(n,ntrace) & sphelp = DblArr(n,ntrace)   
     openr,lun,flaskfile,/get_lun
     
                                ; fcorr[*,i] = help[*]
     FOR j=0,ntrace-1 DO BEGIN
        readf,lun,help
        fcorr[*,j,i] = help[*,*] ;new 
     ENDFOR
     readf,lun,sahelp
     sa[*,*,i] = sahelp[*,*]
     readf,lun,sphelp
     sp[*,*,i] = sphelp[*,*]
     free_lun,lun
  ENDFOR
  
;stop
  
  
  m        = n-11               ; only plot from 2001 on   
  convfact = 1.e-9
  null     = FltArr(m)
  null[*]  = 0.       
  
  
  spserie = FltArr(m,ntrace,nall) & spall = FltArr(m,nall) & seas_ALL  = FltArr(m,nall)
                                ; anomalies
  spanom     = spserie   &  seas = spserie
  spallanom  = seas_ALL
  diffserie  = DblArr(m,ntrace)
  diffall    = DblArr(m)           
  emissavg   = FltArr(ntrace,nall)  
  emisstot   = FltArr(nall)
  FOR j=0,nall-1 DO BEGIN
     FOR i=0,m-1 DO BEGIN
        FOR it=0,ntrace-1 DO spserie[i,it,j]   = sp[i+11,it,j]*convfact
        spall[i,j]   = total(spserie[i,*,j])
     ENDFOR
     FOR it=0,ntrace-1 DO emissavg[it,j] = mean(spserie[*,it,j],/nan) 
     emisstot[j]                          = mean(spall[*,j],/nan)
  ENDFOR
  
  print, '----------------'
  print, 'Emission numbers: '
  print
  FOR j=0,nall-1 DO FOR it=0,ntrace-1 DO print, j, ' ', it, ' ', emissavg[it,j]*365.
  print
  FOR j=0,nall-1 DO print, emisstot[j]*365.
;  print
  
;stop
  
  int = 6
  FOR it=0,ntrace-1 DO diffserie[*,it] = smooth(spserie[*,it,1]-spserie[*,it,0],int,/EDGE_TRUNCATE)
  diffall[*] = smooth(spall[*,1]-spall[*,0],int,/EDGE_TRUNCATE)
  
                                ; Spezialfall Tropical BB
  
; indbb = WHERE((spserie[*,3,1] gt 0.1) and (spserie[*,3,0] gt 0.1),cbb,complement=nochange)
;  IF cbb gt 0L THEN BEGIN
;    diffserie[indbb,3]    = spserie[indbb,3,1]-spserie[indbb,3,0]
;    diffserie[nochange,3] = 0.
;    diffserie[*,3] = smooth(diffserie[*,3],int,/EDGE_TRUNCATE)
;  ENDIF
;removed FLO
    
  ; Montzka timeseries only until the end of 2007!

  ; emissions based on Montzka time series

  fact = 365.

  ; calculate annual trends of the anomalies of all categories
  myears = nyears-1
  days     = [31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.]
  daysleap = [31.,29.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.]
  alldays  =  [days,days,days,daysleap,days,days,days,daysleap]
  nyears   = 2012-1990+1 
  manom    = FltArr(nyears)
  time     = 0.+findgen(nyears)
  vartime  = variance(time)  

  ;; NETCDF FILE
  spp = FltArr(287,48) 
  ;spp = FltArr(59,48) ; if to 1993     
  
  FOR k=0,ntrace-1 DO BEGIN
;spp[*,k]=sphelp[*,k]*1e-9*365*fcorr[*,0,0]
     
     spp[*,k]=sphelp[*,k]*1e-9*365 ;new REMOVED multipl by
;spp[*,k]=sphelp[*,k]*1e-9*365*fcorr[*,k,0] ;new REMOVED multipl by
;fcorr (sp already correc?)
     
     
  ENDFOR
;stop
  
  saa=sahelp*1e-9*365
  
  filename_ncdf='emission_prior_posts_26_1000_62stats.nc'
  ncid4=NCDF_CREATE(filename_ncdf,/CLOBBER)
  tracer = 1.+findgen(49-1)
  ntracer=N_ELEMENTS(tracer)
  time = 1.+findgen(288-1)      ;198902-201212
  ntime=N_ELEMENTS(time)
  
  tracerdimid=NCDF_DIMDEF(ncid4,'tracer',ntracer)
  timedimid=NCDF_DIMDEF(ncid4,'time',ntime)
  
  tracerid=NCDF_VARDEF(ncid4, 'tracer', [tracerdimid], /DOUBLE)
  NCDF_ATTPUT, ncid4, tracerid, 'long_name', 'tracers'
  NCDF_ATTPUT, ncid4, tracerid, 'units', 'tracers'
  
  timeid=NCDF_VARDEF(ncid4, 'time', [timedimid], /DOUBLE)
  NCDF_ATTPUT, ncid4, timeid, 'long_name', 'tim'
  NCDF_ATTPUT, ncid4, timeid, 'units', 'months since 1989-2-1-0:0:0'
  
  apostcatid=NCDF_VARDEF(ncid4, 'aposteriori_cat', [timedimid, tracerdimid], /DOUBLE)
  NCDF_ATTPUT, ncid4, apostcatid, 'long_name', 'post cat ch4'
  NCDF_ATTPUT, ncid4, apostcatid, 'units', 'Tg/yr'
  
  apriorcatid=NCDF_VARDEF(ncid4, 'apriori_cat', [timedimid, tracerdimid], /DOUBLE)
  NCDF_ATTPUT, ncid4, apriorcatid, 'long_name', 'prior cat ch4'
  NCDF_ATTPUT, ncid4, apriorcatid, 'units', 'Tg/yr'
  
  fcorrid=NCDF_VARDEF(ncid4, 'fcorr', [timedimid, tracerdimid], /DOUBLE)
  NCDF_ATTPUT, ncid4, fcorrid, 'long_name', 'factors correction'
  NCDF_ATTPUT, ncid4, fcorrid, 'units', 'fac'
  
  NCDF_CONTROL, ncid4, /ENDEF    
  
  NCDF_VARPUT, ncid4, tracerid, tracer     
  NCDF_VARPUT, ncid4, timeid, time    
  NCDF_VARPUT, ncid4, apostcatid, spp        ; emissions
  NCDF_VARPUT, ncid4, apriorcatid, saa       ; emissions
  NCDF_VARPUT, ncid4, fcorrid, fcorr[*,*,0]  ; emissions
  NCDF_CLOSE, ncid4
  
; END NETCDF FILE
      
  ;;*******************
  ;;* PLOT TIME SERIES
  ;;*******************
  syra = strmid(sim.syyyymm,0,4)
  syre = strmid(sim.eyyyymm,0,4)
  
  plotdir  = '/home/arf/pers/IDL/EPS/URMEL/INVERSION/'
  
  IF keyword_set(rel) THEN np = 'rel' ELSE np = 'abs'
  
  plotfile = plotdir+'ts_inv_emissions_anomalies_weekly_'+np+'_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'.eps'
  IF keyword_set(nobg) THEN $
     plotfile = plotdir+'ts_inv_emissions_anomalies_weekly_nobg_'+np+'_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'.eps'
  IF keyword_set(special) THEN $
     plotfile = plotdir+'ts_inv_emissions_anomalies_weekly_special_'+np+'_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'.eps'
  
  IF keyword_set(flask) and keyword_set(sensctrl) THEN $
     plotfile = plotdir+'ts_inv_emissions_anomalies_weekly_sensctrlflask_'+np+'_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'.eps' 
  
  IF keyword_set(sensctrl) and NOT keyword_set(flask) THEN $
     plotfile = plotdir+'ts_inv_emissions_anomalies_weekly_sensctrl_'+np+'_'+sim.sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'.eps'   
  
  load_ctb,'/home/spc134/IDL/GEOP/diff3.ctb'
  open_ps,plotfile,pssize=[24,20],/eps,/color
  
  !P.BACKGROUND=0
  !P.COLOR=255
  !P.FONT=1
  
  IF keyword_set(rel) THEN ytitle = '%' ELSE ytitle = 'Tg/yr'
  col    = [3,5,7,132,19,23,9,11,14,15,207]
  time   = 6.+findgen(m-6)
  null   = FltArr(m)
  null[*] = 0.
  
  title  = ''
  xtickv = 0+12*indgen(nyears-1)
  
  cat    = ['Agriculture','Fuel production','Waste and combustion','BB Tropics','BB Extra','Rice agriculture',$
            'Inundated wetlands','Wet mineral soils','Peatlands North America',$
            'Peatlands Eurasia','Other natural','All']
  
  col    = [3,5,7,19,27,23,10,11,14,15,207]            
  
  FOR k=0,ntrace DO BEGIN
     
     ytitle=''
     IF k le ntrace-1 THEN BEGIN
        maxi = max(spanom[6:m-7,k])
        mini = min(spanom[6:m-7,k])
     ENDIF ELSE BEGIN
        maxi = max(spallanom[6:m-7])
        mini = min(spallanom[6:m-7])
     ENDELSE
     yrange = [mini,maxi]
     
     
     CASE k OF
        0: position = [0.07,0.78,0.33,0.98]
        1: position = [0.38,0.78,0.65,0.98]
        2: position = [0.70,0.78,0.97,0.98]
        3: position = [0.07,0.53,0.33,0.73]
        4: position = [0.38,0.53,0.65,0.73]
        5: position = [0.70,0.53,0.97,0.73]
        6: position = [0.07,0.28,0.33,0.48]
        7: position = [0.38,0.28,0.65,0.48]
        8: position = [0.70,0.28,0.97,0.48]
        9: position = [0.07,0.05,0.33,0.23]
        10: position = [0.38,0.05,0.65,0.23]
        11: position = [0.70,0.05,0.97,0.23]
     ENDCASE    
     
     IF keyword_set(rel) THEN BEGIN
        yrange = [-70.,120.]
        IF k le  2 THEN yrange = [-15.,15.]
        IF k eq 8  THEN yrange = [-50.,50.]
        IF k eq 9  THEN yrange = [-50.,50.]
        IF k eq 3  THEN yrange = [-70.,120.]
        IF k eq 11 THEN yrange = [-15.,20.]
     ENDIF ELSE BEGIN
        yrange = [-50.,50.]
        IF k le 2  THEN  yrange = [-15.,15.]
        IF k eq 3  THEN  yrange = [-50.,60.]
        IF k eq 4  THEN  yrange = [-15.,15.]
        IF k eq 5  THEN  yrange = [-50.,60.]
        IF k eq 6  THEN  yrange = [-50.,60.]
        IF k eq 7  THEN  yrange = [-50.,60.]      
        IF k eq 8  THEN  yrange = [-15.,15.]
        IF k eq 9  THEN  yrange = [-15.,15.]
        IF k eq 10 THEN  yrange = [-15.,15.]
        IF k eq 11 THEN  yrange = [-50.,60.] 
     ENDELSE
     
     xtickname = StrArr(nyears-1)
     FOR ij=0,nyears-2 DO xtickname[ij] = STRCOMPRESS(string(syyyy+ij+1),/REM)
     xticks = n_elements(xtickname)-1
     
                                ;10!E-2!N     
     IF k gt 0 THEN title = ''
     IF keyword_set(rel) THEN BEGIN
        IF k eq 0 THEN ytitle='%'
        IF k eq 3 THEN ytitle='%'
        IF k eq 6 THEN ytitle='%'
        IF k eq 9 THEN ytitle='%'    
     ENDIF ELSE BEGIN
        IF k eq 0 THEN ytitle='Tg/yr'
        IF k eq 3 THEN ytitle='Tg/yr'
        IF k eq 6 THEN ytitle='Tg/yr'
        IF k eq 9 THEN ytitle='Tg/yr'
     ENDELSE
     plot,time,spallanom[6:m-7,0],/xstyle,ytitle=ytitle,yrange=yrange,xrange=xrange,/ystyle,$
          charsize=1.1,xtickv=xtickv,xticks=xticks,xtickname=xtickname,$
          charthick=1.,position=position,/nodata,/noerase,title=title,noclip=0
     
     plots,0.,0.
     plots,time[m-7],0.,linestyle=1.,color=255,/CONTINUE
     
     IF k le ntrace-1 THEN BEGIN     
        
                                ; shading ENSO
        indp = WHERE(enso gt 0.,cep)
        ensop = FltArr(m)
        ensop[*] = 0.
        ensop[indp] = enso[indp]
        indm = WHERE(enso lt 0.,cem)
        ensom  = FltArr(m)
        ensom[*]    = 0.
        ensom[indm] = enso[indm]       
                                ; shading ENSO
        offwl     = 0
        offbb     = 0      
        IF k eq 3 THEN BEGIN
           IF k eq 3 THEN off = offbb 
           IF k eq 6 THEN off = offwl
           mfuenf = FltArr(m)
           IF keyword_set(rel) THEN mfuenf[*] = -40. ELSE mfuenf[*] = -35.  
           IF keyword_set(rel) THEN f         =   10. ELSE f         = 7.   
           FOR i=0,m-13 DO BEGIN
              linre = [time[i],time[i+1],time[i+1],time[i],time[i]]
              obun  = [ensop[i+6+off]*f+mfuenf[i+6+off],ensop[i+7+off]*f+mfuenf[i+7+off],mfuenf[i+7],mfuenf[i+6],$
                       ensop[i+6+off]*f+mfuenf[i+6+off]]
              polyfill,linre,obun,color=26,noclip=0
              
              linre = [time[i],time[i+1],time[i+1],time[i],time[i]]
              obun  = [ensom[i+6+off]*f+mfuenf[i+6+off],ensom[i+7+off]*f+mfuenf[i+7+off],mfuenf[i+7],mfuenf[i+6],$
                       ensom[i+6+off]*f+mfuenf[i+6+off]]
              polyfill,linre,obun,color=20,noclip=0          
           ENDFOR
           IF KEYWORD_SET(rel) THEN yenso = -60. ELSE yenso = -45. 
           xyouts,6.,yenso,'ENSO index',charsize=1.,charthick=1.,color=255
           
           oplot,time,mfuenf[6:m-7],color=255,linestyle=2   
           oplot,time,enso[(6+off):(m-7+off)]*f+mfuenf[(6+off):(m-7+off)],color=255,thick=2,noclip=0
        ENDIF
        
                                ; now shading
        FOR i=0,m-13 DO BEGIN
           linre = [time[i],time[i+1],time[i+1],time[i],time[i]]
           obun  = [spanom[i+6,k,0],spanom[i+7,k,0],spanom[i+7,k,1],spanom[i+6,k,1],spanom[i+6,k,0]]
           polyfill,linre,obun,color=11,noclip=0,orientation=45,spacing=0.05,/LINE_FILL        
        ENDFOR
        
        oplot,time,spanom[6:m-7,k,0],color=13,thick=4,noclip=0
        oplot,time,spanom[6:m-7,k,1],color=13,thick=2,noclip=0
        IF keyword_set(flask) THEN oplot,time,spanom[6:m-7,k,2],color=5,thick=2,noclip=0
        
        IF k eq 3 THEN oplot,time[0:m-13],spanom_mBB[6:nmontz-1],color=27,thick=2,noclip=0
        IF k eq 7 THEN oplot,time[0:m-13],spanom_mWML[6:nmontz-1],color=27,thick=2,noclip=0
        
        
     ENDIF ELSE BEGIN
        
                                ; now shading
        FOR i=0,m-13 DO BEGIN
           linre = [time[i],time[i+1],time[i+1],time[i],time[i]]
           obun  = [spallanom[i+6,0],spallanom[i+7,0],spallanom[i+7,1],spallanom[i+6,1],spallanom[i+6,0]]
           polyfill,linre,obun,color=11,noclip=0,spacing=0.05,orientation=45,/LINE_FILL     
        ENDFOR    
        
        oplot,time,spallanom[6:m-7,0],color=13,thick=5,linestyle=0,noclip=0
        oplot,time,spallanom[6:m-7,1],color=13,thick=2,linestyle=0,noclip=0
        IF keyword_set(flask) THEN oplot,time,spallanom[6:m-7,2],color=5,thick=2,linestyle=0,noclip=0               
        IF k eq ntrace THEN oplot,time[0:m-13],spanom_mALL[6:nmontz-1],color=27,thick=2,noclip=0            
     ENDELSE
     
     xyouts,position[0]+0.01,position[3]-0.02,cat[k],charsize=1.3,charthick=1.,color=255,/normal
     
  ENDFOR
  
  close_ps
  !p.font=-1        

END
