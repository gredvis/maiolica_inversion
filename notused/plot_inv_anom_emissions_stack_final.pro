;+
; NAME:
;
;   plot_inv_anom_emissions_stack
;
; PURPOSE:
;
;   Plots anomalies of aposteriori emissions as function of category
;
; CATEGORY:
;
;   trajectory, atmospheric transport, dispersion modelling, inverse modelling
;
; CALLING SEQUENCE:
;
;  plot_inv_anom_emissions_stack.pro
;
; INPUTS:
;
;       filename: the name of the binary output file
;
; OPTIONAL INPUTS:
;
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
;   plot_inv_anom_emissions_stack,sim=sim,weekly=weekly,keeppos=keeppos
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; Tina on 06/10/2011
;-

PRO plot_inv_anom_emissions_stack_final,sim=sim

  weekly = 1

  print, 'plot_inv_emissions_anomalies'
  IF keyword_set(weekly) THEN print, 'Keyword_set weekly'

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'URMEL_SENSC_II',$
          obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
          modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
            outdir:'/home/spc134/IDL/urmel/INVERSION/',$
             hdir: '/nas/spc134/URMEL/INVERSION/SENSITIVITY/',$
          syyyymm:'200002',eyyyymm:'200812',scaleq:[0.25,0.25,0.25,0.75,0.75,0.5,0.3,0.6,0.5,0.9,0.4],$
          ntrace:11,nage:4}


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1


  ENDIF

  ;**********************************
  ; GENERAL PARAMETERS AND VARIABLES
  ;**********************************
  ntrace   = 48
  nyears   = fix(strmid(sim.eyyyymm,0,4))-fix(strmid(sim.syyyymm,0,4))+1
  nmonths  = 12
  syyyy = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n = eyyyy*12L+emm-(syyyy*12L+smm)+1
  
  ; conversion factor from kg/d to Tg/day
  fact = 1.e-9

  syra  = sim.syyyymm
  syre  = sim.eyyyymm

  testfile = '/home/arf/pers/IDL/urmel/INVERSION/FINAL/inv_output_weekly_flask_69stats_'+sim.name+'_'+syra+'-'+syre+'_'+sim.qunc+'_nov12.txt'

  fcorr    = FltArr(n,ntrace)
  sa       = DblArr(n,ntrace)
  sp       = DblArr(n,ntrace)
  qp       = DblArr(n,ntrace)
  uncert   = DblArr(n)

  openr,lun,testfile,/get_lun
  readf,lun,fcorr
  readf,lun,sa
  readf,lun,sp
  readf,lun,qp
  readf,lun,uncert
  free_lun,lun

  m = n-12 ; time from 2002 on

  spserie  = FltArr(m,ntrace)
  FOR i=0,m-1 DO FOR it=0,ntrace-1 DO spserie[i,it] = sp[i+12,it]
  ; anomalies
  spanom = spserie & spdes  = spanom & seas   = spserie
  FOR it=0,ntrace-1 DO BEGIN
    spdes[*,it]  = smooth(spserie[*,it],12,/EDGE_TRUNCATE)
    seas[it]     = mean(spdes[*,it],/nan)
    spanom[*,it]  = (spdes[*,it]-seas[it])*fact
  ENDFOR 
    
  ; accumulate category anomalies divided up for pos. and neg. anomalies
  posaccum = FltArr(ntrace,m)
  negaccum = FltArr(ntrace,m)
  pindex   = IntArr(ntrace,m)
  nindex   = IntArr(ntrace,m)
  npos     = IntArr(m)
  nneg     = IntArr(m)
  posaccum[*,*] = 0.
  negaccum[*,*] = 0.
  kpos = 0
  kneg = 0
  j = 0        
  FOR i=0,m-1 DO BEGIN 
     pind  = WHERE(spanom[i,*] gt 0.,cpos)
     nind  = WHERE(spanom[i,*] lt 0.,cneg)
    IF cpos gt 0L THEN BEGIN
      phelp    = FltArr(cpos)
      phelp[0] = spanom[i,pind[0]]
      FOR k=1,cpos-1 DO phelp[k] = phelp[k-1]+spanom[i,pind[k]] 
      posaccum[0:cpos-1,i] = phelp[*]
      pindex[0:cpos-1,i]   = pind[*]
      npos[i]              = cpos
    ENDIF
    IF cneg gt 0L THEN BEGIN
      nhelp = FltArr(cneg)
      nhelp[0] = spanom[i,nind[0]]
      FOR k=1,cneg-1 DO nhelp[k] = nhelp[k-1]+spanom[i,nind[k]]      
      negaccum[0:cneg-1,i] = nhelp[*]
      nindex[0:cneg-1,i]   = nind[*]
      nneg[i]              = cneg
    ENDIF
  ENDFOR    
  
  ; ************************************
  ; * PLOT TIME SERIES
  ; ************************************
  position = [0.15,0.35,0.95,0.9]
  syra = '2001'
  syra = '1990'
  syre = strmid(sim.eyyyymm,0,4)  

  plotdir = '/home/arf/pers/IDL/EPS/URMEL/INVERSION/'
  plotfile = plotdir+'tseries_inv_catsanom_final_sim01_aposteriori_'+syra+'-'+syre+'_'+sim.qunc+'.eps'
  load_ctb,'/home/arf/pers/IDL/GEOP/diff3.ctb'
  open_ps,plotfile,pssize=[20,10],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !p.font=1

  time   = 1.+findgen(m-1)

  xtickname = StrArr(nyears-1)
  FOR ij=0,nyears-2 DO xtickname[ij] = STRCOMPRESS(string(syyyy+ij+1),/REM)
  xticks = n_elements(xtickname)-1
  xtickv = 1+12*indgen(nyears-1)
 
  print, xtickv
 
  ytitle = 'Tg/day'
  yrange = [-0.24,0.24]
  xrange = [0.,m]

  col    = emiss_colors()

  plot,time,spanom[*,0],/xst,ytitle=ytitle,yrange=yrange,xrange=xrange,/yst,$
       charsize=1.4,xtickv=xtickv,xticks=xticks,xtickname=xtickname,charthick=1.,$
       position=position,/nodata,noerase=noerase,title=title,noclip=0

  ; positive anomalies first
  lowbound = FltArr(2)
  upbound  = FltArr(2)
  FOR it=0,m-2 DO BEGIN
    IF npos[it] gt 0 THEN BEGIN
      ; positive anomalies
      FOR i=0,npos[it]-1 DO BEGIN
        IF i eq 0 THEN BEGIN
          lowbound[*] = 0. 
        ENDIF ELSE BEGIN
          lowbound[0] = posaccum[i-1,it]
          lowbound[1] = posaccum[i-1,it]
        ENDELSE
        linre = [time[it]-0.5,time[it]+0.5,time[it]+0.5,time[it]-0.5,time[it]-0.5]
        obun  = [lowbound[0],lowbound[1],posaccum[i,it],posaccum[i,it],lowbound[0]]
        polyfill,linre,obun,color=col[pindex[i,it]],/FILL
      ENDFOR
    ENDIF
  ENDFOR
  
  FOR it=0,m-2 DO BEGIN
    IF nneg[it] gt 0 THEN BEGIN
      FOR i=0,nneg[it]-1 DO BEGIN
        IF i eq 0 THEN BEGIN
          upbound[*] = 0. 
        ENDIF ELSE BEGIN
          upbound[0] = negaccum[i-1,it]
          upbound[1] = negaccum[i-1,it]
        ENDELSE
        linre = [time[it]-0.5,time[it]+0.5,time[it]+0.5,time[it]-0.5,time[it]-0.5]
        obun  = [upbound[0],upbound[1],negaccum[i,it],negaccum[i,it],upbound[0]]
        polyfill,linre,obun,color=col[nindex[i,it]],/FILL
      ENDFOR
    ENDIF
  ENDFOR

  plot,time,spanom[*,0],/xstyle,ytitle=ytitle,yrange=yrange,xrange=xrange,/yst,$
       charsize=1.4,xtickv=xtickv,xticks=xticks,xtickname=xtickname,charthick=1.,$
       position=position,/nodata,/noerase,title=title,noclip=0


  ; * ADD LEGEND
  ; **********************
  c = emiss_categories()

  dy = 0.006
  ; anthropogenic emissions
  FOR i=0,2 DO BEGIN
    top    = 0.22-0.04*i
    bottom = 0.19-0.04*i
    left   = 0.15
    right  = 0.17
    linre  = [left,right,right,left,left]
    obun   = [bottom,bottom,top,top,bottom]
    polyfill,linre,obun,color=col[i],/FILL,/normal

    xyouts,right+0.01,bottom+dy,c[i],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR
  ; biomass burning emissions
  FOR i=3,4 DO BEGIN
    top    = 0.19-0.04*i
    bottom = 0.16-0.04*i
    left   = 0.15
    right  = 0.17
    linre  = [left,right,right,left,left]
    obun   = [bottom,bottom,top,top,bottom]
    polyfill,linre,obun,color=col[i],/FILL,/normal

    xyouts,right+0.01,bottom+dy,c[i],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR
  ; wetlands
  FOR i=0,4 DO BEGIN
    top    = 0.22-0.04*i
    bottom = 0.19-0.04*i
    left   = 0.4
    right  = 0.42
    linre  = [left,right,right,left,left]
    obun   = [bottom,bottom,top,top,bottom]
    polyfill,linre,obun,color=col[i+5],/FILL,/normal

    xyouts,right+0.01,bottom+dy,c[i+5],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR
  ; other natural
  top    = 0.22
  bottom = 0.19
  left   = 0.68
  right  = 0.7
  linre  = [left,right,right,left,left]
  obun   = [bottom,bottom,top,top,bottom]
  polyfill,linre,obun,color=col[10],/FILL,/normal
  xyouts,right+0.01,bottom+dy,c[10],charsize=1.1,charthick=1.2,color=255,/normal

  plot,time,spanom[*,0],/xstyle,ytitle=ytitle,yrange=yrange,xrange=xrange,/yst,$
       charsize=1.4,xtickv=xtickv,xticks=xticks,xtickname=xtickname,charthick=1.,$
       position=position,/nodata,/noerase,title=title,noclip=0

  close_ps
  !p.font=-1


END

