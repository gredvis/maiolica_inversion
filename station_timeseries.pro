;+
; NAME:
;
;   plot_inv_aptimeseries
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
;  plot_inv_aptimeseries
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

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO station_timeseries,sim=sim,stn=stn,post=post,stat_selection=stat_selection

  ;; initializations
  weekly  = 1
  brw     = 0
  special = 0
  ;;  nyr  = 8
  nyr  = 23                     ;flo

  IF n_elements(stn) EQ 0 THEN BEGIN
     message,'parameter stn missing',/continue
     RETURN
  ENDIF
  
  IF n_elements(stat_selection) EQ 0 THEN stat_selection = 'DLR62'
  IF n_elements(sim_name) EQ 0 THEN sim_name = 'final_sim01_u23.7'
 

  ;; get simulation setup and list of stations to plot
  sim = simulation_setup(sim_name)
;  stats = station_lists(stat_selection)
  stats = station_lists(stat_selection,sdetails=sdetails)
  ;; get information for selected station
;  statrec = station_info(stn)

;  station = statrec.name
  ind_stat=WHERE(stats eq stn) 
  station=sdetails[ind_stat].longname
  latitude=sdetails[ind_stat].lat
  longitude=sdetails[ind_stat].lon

  ;; list of dates of the year
  dat = ['0104','0112','0120','0128','0204','0211','0218','0226','0304','0312','0320','0328','0404','0412','0420','0427',$
         '0504','0512','0520','0528','0604','0612','0620','0627','0704','0712','0720','0728','0804','0812','0820','0828',$
         '0904','0912','0920','0927','1004','1012','1020','1028','1104','1112','1120','1127','1204','1212','1220','1228']

  ndat = n_elements(dat)
  ntog = nyr*ndat
  
  sdatall = StrArr(nyr*ndat)
  datall  = FltArr(nyr*ndat)
  i = 0
  FOR ij=0,nyr-1 DO BEGIN
     ;;  syear = STRCOMPRESS(string(2001+ij),/REM)
     syear = STRCOMPRESS(string(1990+ij),/REM)
     FOR j=0,ndat-1 DO BEGIN
        sdatall[i] = syear+dat[j]+'0000'
        i += 1
     ENDFOR
  ENDFOR


  i = 0

  datall = dtg2hiptime(sdatall)
  ;*************
  ; DIRECTORIES
  ;*************
  obsdir   = sim.obsdir
  sensdir  = sim.hdir
  modeldir = sim.modeldir+sim.name+'/'

  ;**********************************
  ; GENERAL PARAMETERS AND VARIABLES
  ;**********************************
  mdays    = [31,28,31,30,31,30,31,31,30,31,30,31]
  nmonths  = 12
  nst      = n_elements(stats)
;  nyears   = fix(STRMID(sim.eyyyymm,0,4))-2001+1
    nyears   = fix(STRMID(sim.eyyyymm,0,4))-1990+1 ;flo
;  syyyy    = 2001 & smm = 1
  syyyy    = 1990 & smm = 1 ;flo
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1
;  m        = eyyyy*12L+emm-(2000*12L+2)+1  
  m        = eyyyy*12L+emm-(1989*12L+2)+1  ;flo
  sn       = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'     
  IF keyword_set(special) THEN sn='88stats' 
 

  ;*************** 
  ; evaluate file
  ;***************  
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  testfile = sim.outdir+'inv_output_weekly_flask_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'    
  IF keyword_set(nobg) THEN $  
  testfile = sim.outdir+'inv_output_weekly_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'
  IF keyword_set(special) THEN $  
  testfile = sim.outdir+'inv_output_weekly_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'

  fhelp    = DblArr(m,sim.ntrace)
  sahelp   = DblArr(m,sim.ntrace)
  sphelp   = DblArr(m,sim.ntrace)

  fcorr    = DblArr(n,sim.ntrace)
  sa       = DblArr(n,sim.ntrace)
  sp       = DblArr(n,sim.ntrace)

  openr,lun,testfile,/get_lun
  readf,lun,fhelp
  readf,lun,sahelp
  readf,lun,sphelp
  free_lun,lun
  

;stop

  ; only evaluate from 2002 on. 2001 is spin up year of inversion
  fcorr[*,*] = fhelp[11:m-1,*]
  FOR it=0,sim.ntrace-1 DO BEGIN
    sa[*,it] = sahelp[11:m-1,it]
    sp[*,it] = sphelp[11:m-1,it]
 ENDFOR

;  nall = 2*366+6*365   
  nall = 2*366+8*365  
  nall = 2*366+20*365  
  zstat_coll   = DblArr(nall)+!VALUES.D_NAN ; observational data at selected stations
  IF keyword_set(brw) THEN  zstat_coll_brw = DblArr(nall)+!VALUES.D_NAN ; observational data at selected stations
  zmastat_coll = DblArr(nall)+!VALUES.D_NAN ; modelled apriori data at selected stations
  zmpstat_coll = DblArr(nall)+!VALUES.D_NAN ; modelled aposteriori data at selected station
  dtgstat_coll = FltArr(nall)+!VALUES.F_NAN
  dtgstat_coll_brw = FltArr(nall)+!VALUES.F_NAN  

  am    = 0 & ambrw = 0
  em    = 0 & embrw = 0
  icorr = 0
  FOR ij=0,nyears-1 DO BEGIN

    syear = STRCOMPRESS(string(syyyy+ij),/REM)
    ;IF syyyy+ij eq syyyy THEN ima = 1 ELSE ima = 0 

    FOR im=0,nmonths-1 DO BEGIN
       
      IF im le 8 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
      yyyymm = syear+mm
      
flask=1

      read_data_single_final,sim=sim,yyyymm=yyyymm,ch4obs=ch4obs,dtgobs=dtgobs,$
                       lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,weekly=weekly,stats=stats,flask=flask,$
                       nobg=nobg,special=special                      

      ;************************************************************************
      ; In February 2006, model data are only available from 3 February on, 
      ; because of the change from lower vertical to higher vertical resolution 
      ; ECMWF data. Hence, restrict observational data to the same period.
      ;************************************************************************
      IF yyyymm eq '200602' THEN BEGIN
        startdate = dtg2gvtime('200602030000')
        ind       = WHERE(dtg2gvtime(dtgobs) ge startdate,c)
        IF c gt 0L THEN BEGIN
          zj        = FltArr(c)
          namec     = StrArr(c)
          dtgc      = StrArr(c)
          statsc    = StrArr(c)
          zj[*]     = double(ch4obs[ind])      
          namec[*]  = nameobs[ind]
          dtgc[*]   = dtgobs[ind]
          nc        = c
        ENDIF
      ENDIF ELSE BEGIN
         zj       = DblArr(nobs)
         zj       = double(ch4obs[0:nobs-1])
         dtgc     = dtgobs
         namec    = nameobs
         nc       = nobs
      ENDELSE

      IF keyword_set(brw) THEN BEGIN
        brw = 1
        special=0
        read_data_single,sim=sim,yyyymm=yyyymm,ch4obs=ch4obs,dtgobs=dtgobs,lonobs=lonobs,latobs=latobs,$
        nameobs=nameobs,nobs=nobs,weekly=weekly,stats=stats,flask=flask,brw=brw,nobg=nobg,special=special                      
       ; special = 1 ;flo
       ;************************************************************************
      ; In February 2006, model data are only available from 3 February on, 
      ; because of the change from lower vertical to higher vertical resolution 
      ; ECMWF data. Hence, restrict observational data to the same period.
      ;************************************************************************
      IF yyyymm eq '200602' THEN BEGIN
        startdate = dtg2gvtime('200602030000')
        ind       = WHERE(dtg2gvtime(dtgobs) ge startdate,c)
        IF c gt 0L THEN BEGIN
          zjbrw      = FltArr(c)
          dtgbrw     = StrArr(c)
          namebrw    = StrArr(c)
          dtgbrw[*]  = dtgobs[ind]
          zjbrw[*]   = double(ch4obs[ind])
          namebrw[*] = nameobs[ind]      
          nbrw      = c
        ENDIF
      ENDIF ELSE BEGIN
         zjbrw      = DblArr(nobs)
         dtgbrw     = StrArr(nobs)
         namebrw    = StrArr(nobs)
         dtgbrw[*]  = dtgobs[0:nobs-1] 
         namebrw[*] = nameobs[0:nobs-1]
         zjbrw      = double(ch4obs[0:nobs-1])
         nbrw       = nobs
      ENDELSE
      
      ENDIF
            
      ;****************************************************************
      ; read sensitivity matrix of year and month. Unit: ppb/(kg/month)
      ;****************************************************************
      IF yyyymm eq sim.syyyymm THEN BEGIN
        H = DblArr(sim.ntrace,nc)
      ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
        H = DblArr(sim.ntrace*2,nc)
      ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
        H = DblArr(sim.ntrace*3,nc)
     ENDIF ELSE BEGIN
        H = DblArr(sim.ntrace*4,nc)
      ENDELSE
      IF keyword_set(weekly) THEN BEGIN
       sensfile = sim.hdir+'inv_sensitivity_weekly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'      
       IF keyword_set(nobg) THEN $
       sensfile = sim.hdir+'inv_sensitivity_weekly_nobg_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'       
       IF keyword_set(special) THEN $
       sensfile = sim.hdir+'inv_sensitivity_weekly_special_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'       
      ENDIF      
      nch = 0 & nospt = 0 & dummy = 0
      openr,lun,sensfile,/get_lun  
      readf,lun,dummy
      readf,lun,nch
      readf,lun,H
      free_lun,lun
             
      IF nch ne nc THEN stop
            
      IF yyyymm eq sim.syyyymm THEN BEGIN
        xp = DblArr(sim.ntrace) & xa = DblArr(sim.ntrace)
        xp[*]  = sp[icorr,*]  
        xa[*]  = sa[icorr,*]                       
      ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
        xp = DblArr(sim.ntrace*2) & xa = DblArr(sim.ntrace*2)
        xp[0:sim.ntrace-1]             = sp[icorr,*]
        xp[sim.ntrace:2*sim.ntrace-1]  = sp[icorr-1,*]
        xa[0:sim.ntrace-1]             = sa[icorr,*]
        xa[sim.ntrace:2*sim.ntrace-1]  = sa[icorr-1,*]                
      ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
        xp = DblArr(sim.ntrace*3) & xa = DblArr(sim.ntrace*3)
        xp[0:sim.ntrace-1]             = sp[icorr,*]
        xp[sim.ntrace:2*sim.ntrace-1]  = sp[icorr-1,*]
        xp[2*sim.ntrace:3*sim.ntrace-1]  = sp[icorr-2,*]   
        xa[0:sim.ntrace-1]             = sa[icorr,*]
        xa[sim.ntrace:2*sim.ntrace-1]  = sa[icorr-1,*] 
        xa[2*sim.ntrace:3*sim.ntrace-1]  = sa[icorr-2,*]    
     ENDIF ELSE BEGIN
        xp = DblArr(sim.ntrace*4) & xa = DblArr(sim.ntrace*4)
        xp[0:sim.ntrace-1]               = sp[icorr,*]
        xp[sim.ntrace:2*sim.ntrace-1]    = sp[icorr-1,*]
        xp[2*sim.ntrace:3*sim.ntrace-1]  = sp[icorr-2,*]   
        xp[3*sim.ntrace:4*sim.ntrace-1]  = sp[icorr-3,*]   
        xa[0:sim.ntrace-1]               = sa[icorr,*]
        xa[sim.ntrace:2*sim.ntrace-1]    = sa[icorr-1,*]
        xa[2*sim.ntrace:3*sim.ntrace-1]  = sa[icorr-2,*]   
        xa[3*sim.ntrace:4*sim.ntrace-1]  = sa[icorr-3,*]   
      ENDELSE

      ; read weekly model data in correct temporal order
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'       
      IF keyword_set(nobg) THEN $
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_nobg_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'      
      IF keyword_set(special) THEN $
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_special_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'      
      mch4 = DblArr(sim.ntrace*sim.nage,nc)
      help = DblArr(sim.ntrace*sim.nage)
      mall = DblArr(nc)    
      line    = ''
      openr,lun,modfile,/get_lun
      FOR i=0,nc-1 DO BEGIN
        readf,lun,line
        result  = STRSPLIT(line,/EXTRACT)
        mall[i] = double(result[4])
        readf,lun,help
        mch4[*,i] = help[*]
      ENDFOR
      free_lun,lun

      BGa = DblArr(nc)
      BGp = DblArr(nc)
      BGa_temp = DblArr(nc,sim.ntrace)
      BGp_temp = DblArr(nc,sim.ntrace)
      zmp = DblArr(nc)
      zma = DblArr(nc)
      z   = DblArr(nc)
      IF keyword_set(brw) THEN zbrw = DblArr(nbrw)
;      FOR i=0,nc-1 DO BGp[i] = fcorr[icorr]*total(mch4[(sim.nage-1)*sim.ntrace:sim.nage*sim.ntrace-1,i])
;      FOR i=0,nc-1 DO BGa[i] = total(mch4[(sim.nage-1)*sim.ntrace:sim.nage*sim.ntrace-1,i])



  FOR it=0,sim.ntrace-1 DO BEGIN
 FOR i=0,nc-1 DO BGp_temp[i,it] = fcorr[icorr,it]*(mch4[it+(sim.nage-1)*sim.ntrace,i])   
FOR i=0,nc-1 DO BGa_temp[i,it] = (mch4[it+(sim.nage-1)*sim.ntrace,i])                                 
ENDFOR

FOR k=0,nc-1 DO BGp[k] = total(BGp_temp[k,*]) 
FOR k=0,nc-1 DO BGa[k] = total(BGa_temp[k,*]) 


      zmp = reform(H ## xp) + BGp
      zma = mall
      z   = zj

;stop

      IF keyword_set(brw) THEN zbrw = zjbrw
      
      ind = WHERE(namec eq stn,ck)
      IF ck gt 0L THEN BEGIN
        dtgstat    = FltArr(ck)  ; dates of observational data at stn      
        ch4o       = DblArr(ck)  ; observational data at stats[i]
        ch4ma      = DblArr(ck)  ; apriori values at stats[i]
        ch4mp      = DblArr(ck)  ; aposteriori values at stats[i]              
        dtgstat[*] = dtg2hiptime(dtgc[ind])        
        ch4o[*]    = z[ind]
        ch4ma[*]   = zma[ind]
        ch4mp[*]   = zmp[ind]
        em         += ck
        zstat_coll[am:em-1]   = ch4o[*]
        zmpstat_coll[am:em-1] = ch4mp[*]
        zmastat_coll[am:em-1] = ch4ma[*]
        dtgstat_coll[am:em-1] = dtgstat[*]
        am         += ck
      ENDIF
      IF keyword_set(brw) THEN BEGIN
        ind = WHERE(namebrw eq stn,cbrw)
        IF cbrw gt 0L THEN BEGIN
          ch4o2      = DblArr(cbrw)  ; observational data at stats[i]
          ch4o2[*]      = zbrw[ind]
          embrw        += cbrw
          dtgstat_coll_brw[ambrw:embrw-1] = dtg2hiptime(dtgbrw[ind])
          zstat_coll_brw[ambrw:embrw-1]   = ch4o2[*]
          ambrw        += cbrw
        ENDIF 
      ENDIF     
      icorr += 1
              
    ENDFOR ; end loop over months
  ENDFOR   ; end loop over years   

  zstat_plot   = DblArr(ntog) & zmastat_plot = DblArr(ntog) & zmpstat_plot = DblArr(ntog)
  zstat_plot[*] = !VALUES.D_NAN & zmastat_plot[*] = !VALUES.D_NAN & zmpstat_plot[*] = !VALUES.D_NAN
  IF keyword_set(brw) THEN zstat_plot_brw = DblArr(ntog)+!VALUES.D_NAN
  FOR i=0,em-1 DO BEGIN
    indt         = WHERE(dtgstat_coll[i] eq datall,ct)
    IF ct gt 0L THEN BEGIN
      zstat_plot[indt[0]]   = zstat_coll[i]
      zmastat_plot[indt[0]] = zmastat_coll[i]
      zmpstat_plot[indt[0]] = zmpstat_coll[i]
    ENDIF
  ENDFOR
  IF keyword_set(brw) THEN BEGIN
    FOR i=0,embrw-1 DO BEGIN
      indt         = WHERE(dtgstat_coll_brw[i] eq datall,ct)
      IF ct gt 0L THEN zstat_plot_brw[indt[0]]   = zstat_coll_brw[i]
    ENDFOR  
  ENDIF
;    stop   
  ;*******************
  ;* PLOT TIME SERIES
  ;*******************
  sname = stn+'_'
;  syra = '2001'
  syra = '1990' ;flo
  syre = strmid(sim.eyyyymm,0,4)  

  plotdir = '/home/arf/pers/IDL/EPS/URMEL/INVERSION/'
  IF KEYWORD_SET(post) THEN BEGIN
    IF keyword_set(brw) THEN BEGIN
      plotfile = plotdir+'ts_inv_final_sim01_aposteriori_plusnobg_'+sname+syra+'-'+syre+'_'+qunc+'.eps' 
    ENDIF ELSE BEGIN
      plotfile = plotdir+'ts_inv_final_sim01_aposteriori_'+sname+syra+'-'+syre+'_'+qunc+'.eps'    
      IF keyword_set(nobg) THEN $
      plotfile = plotdir+'ts_inv_final_sim01_aposteriori_nobg_'+sname+syra+'-'+syre+'_'+qunc+'.eps'      
      IF keyword_set(special) THEN $
      plotfile = plotdir+'ts_inv_final_sim01_aposteriori_special_'+sname+syra+'-'+syre+'_'+qunc+'.eps'
    ENDELSE 
  ENDIF ELSE BEGIN
    plotfile = plotdir+'ts_inv_final_sim01_apriori_'+sname+syra+'-'+syre+'_'+qunc+'.eps'  
    IF keyword_set(nobg) THEN $
    plotfile = plotdir+'ts_inv_final_sim01_apriori_nobg_'+sname+syra+'-'+syre+'_'+qunc+'.eps'   
    IF keyword_set(special) THEN $
    plotfile = plotdir+'ts_inv_final_sim01_apriori_special_'+sname+syra+'-'+syre+'_'+qunc+'.eps'       
   ENDELSE
  load_ctb,'/home/arf/pers/IDL/GEOP/diff3.ctb'
  open_ps,plotfile,pssize=[24,20],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !P.FONT=1

  ytitle = 'CH!I4!N (ppbv)'
  col    = [3,5,7,132,19,20,9,11,14,15,207]
  ;time   = 0.+findgen(nall)
 
  xrange = [datall[0],datall[ntog-1]]
  
  setv = FltArr(nyears)
  setv[0] = 0
  FOR ij=0,nyears-1 DO BEGIN
    ind = WHERE(STRMID(hiptime2dtg(datall),0,4) eq STRCOMPRESS(string(syyyy+ij),/REM),ctickv)
    print, ij, ' ', ind[0], datall[ind[0]]
    IF ctickv gt 0L THEN setv[ij] = datall[ind[0]]
  ENDFOR

  position = [0.15,0.3,0.95,0.8]

  ind1 = WHERE(finite(zmastat_plot[0:ntog-1]) eq 1,c1)
  ind2 = WHERE(finite(zmpstat_plot[0:ntog-1]) eq 1,c2)
  ind3 = WHERE(finite(zstat_plot[0:ntog-1]) eq 1,c3)

  xmaxi1 = max(zmastat_plot[ind1])
  xmaxi2 = max(zmpstat_plot[ind2])
  xmaxi3 = max(zstat_plot[ind3])
  xmaxi = xmaxi1
  IF xmaxi2 gt xmaxi1 THEN xmaxi = xmaxi2
  IF xmaxi3 gt xmaxi  THEN xmaxi = xmaxi3
  
  xmini1 = min(zmastat_plot[ind1])
  xmini2 = min(zmpstat_plot[ind2])
  xmini3 = min(zstat_plot[ind3])
  xmini = xmini1
  IF xmini2 lt xmini1 THEN xmini = xmini2
  IF xmini3 lt xmini  THEN xmini = xmini3  
  
;  yrange = statrec.yrange
  IF sdetails[ind_stat].lat lt 30 THEN yrange=[1600.,1850.] ELSE  yrange = [1750.,2000.]


;  xtickname = StrArr(nyears)
;  FOR ij=0,nyears-1 DO xtickname[ij] = STRCOMPRESS(string(syyyy+ij),/REM)
;  xticks = nyears-1
;  plot,datall[0:ntog-1],zstat_plot[0:ntog-1],/xstyle,ytitle=ytitle,yrange=yrange,xrange=xrange,/yst,$
;       charsize=1.0,xtickv=setv,xticks=xticks,ytickname=ytickname,xtickname=xtickname,$
;       charthick=1.,position=position,/nodata,/noerase,title=title,noclip=0

  xtickname = StrArr(nyears)
  FOR ij=0,nyears-1,2 DO xtickname[ij] = STRCOMPRESS(string(syyyy+ij),/REM)
  xtickname=['1990','1992','1994','1998','2000','2002','2004','2006','2008','2010','2012']
  xticks = nyears/2-1
  plot,datall[0:ntog-1],zstat_plot[0:ntog-1],/xstyle,ytitle=ytitle,yrange=yrange,xrange=xrange,/yst,$
       charsize=1.8,xtickv=setv[1:21:2],xticks=xticks,ytickname=ytickname,xtickname=xtickname,$
       charthick=1.,position=position,/nodata,/noerase,title=title,noclip=0



;stop

  IF keyword_set(brw) THEN BEGIN
    oplot,datall,zstat_plot_brw[0:ntog-1],color=19,thick=1,noclip=0  
    plotsym,0,0.6,color=19,/FILL   
    oplot,datall,zstat_plot_brw[0:ntog-1],color=19,psym=8
  ENDIF

  oplot,datall[0:ntog-1],zstat_plot[0:ntog-1],color=255,thick=3,linestyle=0
  plotsym,0,0.6,color=255,/FILL   
  oplot,datall,zstat_plot[0:ntog-1],color=255,psym=8
      
oplot,datall,zmastat_plot[0:ntog-1],color=6,thick=3,noclip=0
plotsym,0,0.6,color=6,/FILL 
  oplot,datall,zmastat_plot[0:ntog-1],color=6,psym=8
  
  IF keyword_set(brw) THEN BEGIN
    statistik = DblArr(2,embrw)
    FOR i=0,embrw-1 DO BEGIN
      ind = WHERE(dtgstat_coll_brw[i] eq dtgstat_coll,cind)
      IF cind gt 0L THEN BEGIN
        statistik[0,i] = zmastat_coll[ind]
        statistik[1,i] = zmpstat_coll[ind]
      ENDIF
    ENDFOR
  ENDIF
  
  xyouts,0.17,0.76,station,charthick=1.4,charsize=2.,color=255,/normal
  
  IF keyword_set(post) THEN BEGIN
oplot,datall,zmpstat_plot[0:ntog-1],color=13,thick=3,noclip=0
plotsym,0,0.6,color=13,/FILL 
  oplot,datall,zmpstat_plot[0:ntog-1],color=13,psym=8
ENDIF

  
  ; print RMS error for apriori and aposteriori data
  pRMS = FltArr(2)
  print, 'RMS error apriori aposteriori'
  ind1 = WHERE(finite(zmastat_coll[0:em-1]-zstat_coll[0:em-1]) eq 1,c1)
  ind2 = WHERE(finite(zmpstat_coll[0:em-1]-zstat_coll[0:em-1]) eq 1,c2)
;stop

  prms[0] = sqrt(total( ( (zmastat_coll[ind1]-mean(zmastat_coll[ind1])) - (zstat_coll[ind1]-mean(zstat_coll[ind1])))^2)/float(c1))
  prms[1] = sqrt(total( ( (zmpstat_coll[ind2]-mean(zmpstat_coll[ind2])) - (zstat_coll[ind2]-mean(zstat_coll[ind2])))^2)/float(c2))
  print, stn, '  ', prms[0], prms[1]
  print
  print,'Correlation apriori aposteriori'
  corr = FltArr(2)
  corr[0] = correlate(zmastat_coll[ind1],zstat_coll[ind1])
  corr[1] = correlate(zmpstat_coll[ind2],zstat_coll[ind2])
  print, stn, '  ', corr[0], corr[1]
  IF keyword_set(brw) THEN print, correlate(statistik[0,*],zstat_coll_brw), ' ', correlate(statistik[1,*],zstat_coll_brw)  
  print
  print, 'Model bias'
  bias = FltArr(2)
  bias = FltArr(6)
  bias[0] = mean(zmastat_coll[ind1])-mean(zstat_coll[ind1])
  bias[1] = mean(zmpstat_coll[ind2])-mean(zstat_coll[ind2])
  
bias[2] = mean(zmastat_coll[ind1[50]:ind1[c1/3]])-mean(zstat_coll[ind1[50]:ind1[c1/3]])
  bias[3] = mean(zmpstat_coll[ind2[50]:ind2[c2/3]])-mean(zstat_coll[ind2[50]:ind2[c2/3]])

bias[4] = mean(zmastat_coll[ind1[c1/3]:ind1[2*c1/3-1]])-mean(zstat_coll[ind1[c1/3]:ind1[2*c1/3-1]])
  bias[5] = mean(zmpstat_coll[ind2[c2/3]:ind2[2*c2/3-1]])-mean(zstat_coll[ind2[c2/3]:ind2[2*c2/3-1]])

  print, stn, '  ', bias[0], bias[1]
  print, stn, '  ', bias[2], bias[3]
  print, stn, '  ', bias[4], bias[5]

;  bias[0] = bias[0]/mean(zstat_coll[ind1])*100.
;  bias[1] = bias[1]/mean(zstat_coll[ind2])*100.
;  print, stn, '  ', bias[0], bias[1]


statfile=sim.outdir+'station_statistics_'+sname+syra+'-'+syre+'_'+qunc+'.txt'
openw,lun,statfile,/get_lun
 printf,lun,'RMS error apriori aposteriori'
printf,lun,stn, '  ', prms[0], prms[1]
printf,lun,'Correlation apriori aposteriori'
printf,lun,stn, '  ', corr[0], corr[1]
printf,lun, 'Model bias 1990-2012'
printf,lun, stn, '  ', bias[0], bias[1]
printf,lun, 'Model bias 1990-2001'
printf,lun, stn, '  ', bias[2], bias[3]
printf,lun, 'Model bias 2001-2012 '
printf,lun, stn, '  ', bias[4], bias[5]
free_lun,lun    

  
  ; **********************
  ; * ADD LEGEND
  ; **********************
  IF NOT keyword_set(brw) THEN BEGIN
    c      = ['Observational data','A priori','A posteriori']
    col    = [255,6,13]    
    dx     = [0.16,0.33,0.40]    
  ENDIF ELSE BEGIN
    c = ['Observational data','Observational data filtered', 'A priori','A posteriori']
    col    = [19,255,6,13]
    dx     = [0.16,0.33,0.56,0.63]
  ENDELSE  
  IF NOT keyword_set(post) THEN cn = 2 ELSE cn = n_elements(c)

  FOR i=0,cn-1 DO xyouts,dx[i],position[1]+0.01,c[i],charsize=1.4,charthick=1.2,color=col[i],/normal

  close_ps
  !p.font=-1

END
