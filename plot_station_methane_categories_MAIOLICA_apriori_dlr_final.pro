;+
; NAME:
;
;   compute_growth_rates_model_categories
;
; PURPOSE:
;
;   Calculate global,tropical, or hemispheric growth rates
;   for the period 2001-2008 using the weekly observational
;   AND model data. Look at contributions from individual categories.
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  compute_growth_rates_categories,sim=sim,lat=lat
;
; INPUTS:
;  sim: structure containing directory and parameter information for simulation
;  lat: choose latitude band to investigate
;
; KEYWORD PARAMETERS:
; /dump: write out time series for later plotting only
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
; CSP 08 May 2012
;-

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO plot_station_methane_categories_MAIOLICA_apriori,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,legend=legend,$
                                                abc=abc,nobg=nobg

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'final_sim01',$
          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
          modeldir:'/nas/arf/output/',$
            outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
             hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
            syyyymm:'198901',eyyyymm:'199212',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30],$
         ntrace:48,nage:5}

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198901',eyyyymm:'199512',scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.85,0.85,0.50,0.50,0.85,0.50,0.85,0.85,0.50,0.85,0.85,0.55,0.50,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.85,0.85,0.85,0.85],$
         ntrace:48,nage:5} ;1995


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'199912',scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.85,0.85,0.50,0.50,0.85,0.50,0.85,0.85,0.50,0.85,0.85,0.55,0.50,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.85,0.85,0.85,0.85],$
         ntrace:48,nage:5}

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200112',scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.1,0.1,0.1,0.1],$
         ntrace:48,nage:5} 


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200612',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45],$
         ntrace:48,nage:5} ;1999

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200612',scaleq:[0.950000 ,    0.950000 ,    0.650000,     0.650000 ,    0.950000   ,  0.950000, 0.450000  ,   0.950000 ,    0.950000   ,  0.950000   ,  0.950000  ,   0.950000, 0.950000   ,  0.950000 ,    0.95000, 0.950000   ,   0.00000   ,  0.950000 , 0.950000  ,    0.00000  ,   0.950000   ,   0.00000  ,   0.950000  ,    0.00000 , 0.950000   ,  0.950000  ,   0.950000   ,  0.950000  ,   0.950000  ,   0.950000  , 0.950000 ,    0.950000   ,  0.700000 ,    0.950000 ,    0.950000  ,   0.950000 , 0.850000  ,   0.950000  ,   0.950000  ,   0.800000  ,   0.950000   ,  0.100000 ,  1.66000  ,    1.66000  ,    1.45000  ,    1.45000  ,    1.45000  ,    1.45000],$ ; MAX LLH
ntrace:48,nage:5} ;1999


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200912',scaleq:[0.950000 ,    0.950000 ,    0.650000,     0.650000 ,    0.950000   ,  0.950000, 0.450000  ,   0.950000 ,    0.950000   ,  0.950000   ,  0.950000  ,   0.950000, 0.950000   ,  0.950000 ,    0.95000, 0.950000   ,   0.00000   ,  0.950000 , 0.950000  ,    0.00000  ,   0.950000   ,   0.00000  ,   0.950000  ,    0.00000 , 0.950000   ,  0.950000  ,   0.950000   ,  0.950000  ,   0.950000  ,   0.950000  , 0.950000 ,    0.950000   ,  0.700000 ,    0.950000 ,    0.950000  ,   0.950000 , 0.850000  ,   0.950000  ,   0.950000  ,   0.800000  ,   0.950000   ,  0.100000 ,  1.66000  ,    1.66000  ,    1.45000  ,    1.45000  ,    1.45000  ,    1.45000],$
       ntrace:48,nage:5} ;NEW TEST


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45],$
         ntrace:48,nage:5} ;REF with uncert derived from spc

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198901',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  23.7000 DLR



;sim = {name:'final_sim01',$
;         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;         modeldir:'/nas/arf/output/',$
;         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
;         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
;         syyyymm:'198902',eyyyymm:'200512',scaleq:[0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90],$
;         ntrace:48,nage:5} ;NEW TEST

;sim = {name:'final_sim01',$
;         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;         modeldir:'/nas/arf/output/',$
;         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
;         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
;         syyyymm:'198902',eyyyymm:'200612',scaleq:[1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90,1.90],$
;         ntrace:48,nage:5} ;NEW TEST

  ENDIF
                 
  IF n_elements(stats) EQ 0 THEN BEGIN
;    stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'mhd',   'ngl',$
;                   'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
;                   'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
;                   'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
;                   'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
;                   'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
 ;                  'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
  ;                 'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey',   'asc',   'cfa',$
   ;                'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
    ;               'cya',   'syo',   'hba']

;stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
;                   'zep',   'sum',$
;                   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
;                   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
;                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
;                   'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
;                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
;                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']


 stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'mhd',   'ngl',$
                   'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                   'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                   'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                   'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                   'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                   'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                   'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey',   'asc',   'cfa',$
                   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                   'cya',   'syo',   'hba']

;DLR wo smo,ice,sis,amt
    stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                   'hpb',   'hun',   'puy',   'lef',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                   'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']

flask=1                   
;;;;TEST FLO;;;;

; flask measurements only
;    stats  =   [   'cba',   'shm',   'nwr',   'bmw',   'bme',   'key', 'kum', 'gmi',   'chr', 'asc', 'syo']
 

  ENDIF                     
  sn = STRCOMPRESS(string(n_elements(stats)),/REM)+'stats'   

  ; calculate number of months to simulate
  syyyy = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n = eyyyy*12L+emm-(syyyy*12L+smm);+1 dlr
  ;nyears = eyyyy-2001+1
  nyears = eyyyy-1990+1;flo

  print, 'Run compute_growth_rates for period ', syyyy, ' to ', eyyyy

  m = n-11

  ntrace = sim.ntrace  

  ch4mean     = DblArr(m) ; observations
  mch4mean    = DblArr(m) ; apriori model
  sch4mean    = DblArr(m) ; apriori model with constant OH
  mch4post    = DblArr(m) ; CH4 series according to aposteriori emissions estimates
  mch4cat     = DblArr(m,ntrace) ; CH4 aposteriori series individual cats 
  ch4_catap   = DblArr(240,ntrace*sim.nage) ; CH4 aposteriori series individual cats 
ch4_catap   = DblArr(276,ntrace*sim.nage) ; until 2012

  ch4smooth   = DblArr(m) ; 12-month running mean of monthly CH4 obs
  mch4smooth  = DblArr(m) ; 12-month running mean of monthly CH4 apriori model
  sch4smooth  = DblArr(m) ; 12-month running mean of monthly CH4 apriori model constant OH
  pmch4smooth = DblArr(m) ; smoothed CH4 series according to aposteriori emissions estimates
  cmch4smooth = DblArr(m,ntrace) ; smoothed CH4 series categories aposteriori 
  
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  syra  = sim.syyyymm
  syre  = sim.eyyyymm
  IF keyword_set(flask) THEN BEGIN
    IF keyword_set(nobg) THEN $
    testfile = sim.outdir+'inv_output_weekly_flask_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt' $        
    ELSE testfile = sim.outdir+'inv_output_weekly_flask_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'    
  ENDIF ELSE BEGIN
    IF keyword_set(nobg) THEN $
         testfile = sim.outdir+'inv_output_weekly_nobg_'+sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+qunc+'_nov12.txt' $    
    ELSE testfile = sim.outdir+'inv_output_weekly_'+sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+qunc+'_nov12.txt'
  ENDELSE
  
  fcorr    = DblArr(n)
  sa       = DblArr(n,sim.ntrace)
  sp       = DblArr(n,sim.ntrace)
  
  ccorr    = DblArr(m)
  ca       = DblArr(m,sim.ntrace)
  cp       = DblArr(m,sim.ntrace)

  openr,lun,testfile,/get_lun
  readf,lun,fcorr
  readf,lun,sa
  readf,lun,sp
  free_lun,lun

  FOR i=0,m-1 DO BEGIN
    ccorr[i] = fcorr[i+11]
    ca[i,*]  = sa[i+11,*]
    cp[i,*]  = sp[i+11,*]
  ENDFOR
  
  ;*****************************************************************
  ; MAIN LOOP: read and process observational data of year and month
  ;*****************************************************************
  ;cyyyymm = '200101'  
  cyyyymm = '199001'   ;flo
  icorr   = 0
  FOR i=0,m-1 DO BEGIN

;  FOR i=0,m-30 DO BEGIN ;TEST
    ; read observational data
    read_data_single,sim=sim,yyyymm=cyyyymm,ch4obs=ch4obs,dtgobs=dtgobs,$
                     lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,/weekly,stats=stats,flask=flask,nobg=nobg
                     
    ; prepare reading weekly model data
    IF cyyyymm eq '200602' THEN BEGIN
      startdate = dtg2gvtime('200602030000')
      ind       = WHERE(dtg2gvtime(dtgobs) ge startdate,c)
      IF c gt 0L THEN BEGIN
        z        = DblArr(c)
        namec    = StrArr(c)
        dtgc     = StrArr(c)
        latc     = FltArr(c)
        z[*]     = double(ch4obs[ind])      
        dtgc[*]  = dtgobs[ind]
        latc[*]  = latobs[ind]
        nc       = c
     ENDIF
    ENDIF ELSE BEGIN
      z     = ch4obs
      dtgc  = dtgobs
      latc  = latobs
      nc    = nobs
    ENDELSE

    ;****************************************************************
    ; read sensitivity matrix of year and month. Unit: ppb/(kg/month)
    ;****************************************************************
    IF cyyyymm eq sim.syyyymm THEN BEGIN
      H = DblArr(sim.ntrace,nc)
    ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
      H = DblArr(sim.ntrace*2,nc)
    ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
      H = DblArr(sim.ntrace*3,nc)
   ENDIF ELSE BEGIN
      H = DblArr(sim.ntrace*4,nc)
    ENDELSE    
    IF keyword_set(flask) THEN $
    sensfile = sim.hdir+'inv_sensitivity_weekly_flask_DLR_'+sn+'_'+sim.name+'_'+cyyyymm+'_nov12.txt' $
    ELSE sensfile = sim.hdir+'inv_sensitivity_weekly_'+sn+'_'+sim.name+'_'+cyyyymm+'_nov12.txt'
    nc = 0 & nospt = 0 & dummy = 0
    openr,lun,sensfile,/get_lun  
    readf,lun,dummy
    readf,lun,nc
    readf,lun,H
    free_lun,lun
        
    namec = StrArr(nc)
    namec[*] = nameobs[0:nc-1]
            
    IF cyyyymm eq sim.syyyymm THEN BEGIN
      xp = DblArr(sim.ntrace)
      xp[*]  = cp[icorr,*]                
    ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
      xp = DblArr(sim.ntrace*2)
      xp[0:sim.ntrace-1]             = cp[icorr,*]
      xp[sim.ntrace:2*sim.ntrace-1]  = cp[icorr-1,*]    
 ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
      xp = DblArr(sim.ntrace*3)
      xp[0:sim.ntrace-1]               = cp[icorr,*]
      xp[sim.ntrace:2*sim.ntrace-1]    = cp[icorr-1,*]
      xp[2*sim.ntrace:3*sim.ntrace-1]  = cp[icorr-2,*]        
    ENDIF ELSE BEGIN
      xp = DblArr(sim.ntrace*4)
      xp[0:sim.ntrace-1]               = cp[icorr,*]
      xp[sim.ntrace:2*sim.ntrace-1]    = cp[icorr-1,*]
      xp[2*sim.ntrace:3*sim.ntrace-1]  = cp[icorr-2,*]        
      xp[3*sim.ntrace:4*sim.ntrace-1]  = cp[icorr-3,*]        
    ENDELSE

    ; read model data
    IF keyword_set(flask) THEN BEGIN      
      IF keyword_set(nobg) THEN $      
           modfile = '/nas/arf/INVERSION/m_allweekly_flask_nobg_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat' $             
      ELSE modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_DLR'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'       
    ENDIF ELSE BEGIN
      IF keyword_set(nobg) THEN $
           modfile = '/nas/arf/INVERSION/m_allweekly_nobg_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat' $      
      ELSE modfile = '/nas/arf/INVERSION/m_allweekly_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'
    ENDELSE
    mch4 = DblArr(sim.ntrace*sim.nage,nc)
    help = DblArr(sim.ntrace*sim.nage)
    mall = DblArr(nc)    
    line    = ''
    openr,lun,modfile,/get_lun
    FOR k=0,nc-1 DO BEGIN
      readf,lun,line
      result  = STRSPLIT(line,/EXTRACT)
      mall[k] = double(result[4])
      readf,lun,help
      mch4[*,k] = help[*]
    ENDFOR
    free_lun,lun
    
    BG = DblArr(nc)
    FOR k=0,nc-1 DO BG[k] = ccorr[icorr]*total(mch4[(sim.nage-1)*sim.ntrace:sim.nage*sim.ntrace-1,k])
    
    ; methane values acccording to aposteriori estimates of methane emissions 
    zmp = H ## xp + BG

    ; now read weekly aposteriori model estimates for individual categories
    IF keyword_set(flask) THEN BEGIN
      IF keyword_set(nobg) THEN $
           catfile = '/nas/arf/INVERSION/m_weekly_apost_categories_flask_nobg_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat' $      
      ELSE catfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_flask_DLR_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'
    ENDIF ELSE BEGIN
      IF keyword_set(nobg) THEN $
           catfile = '/nas/arf/INVERSION/m_weekly_apost_categories_nobg_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat' $      
      ELSE catfile = '/nas/arf/INVERSION/m_weekly_apost_categories_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'
    ENDELSE
    ch4cat  = FltArr(ntrace,nc)
    openr,lun2,catfile,/get_lun
    FOR k=0,nc-1 DO BEGIN
     ; readf,lun2,line ;flo
      linefin  = FltArr(49)
readf,lun2,line
linefin[0:11]=STRSPLIT(line,/EXTRACT)
      readf,lun2,line
linefin[12:22]=STRSPLIT(line,/EXTRACT)
      readf,lun2,line
linefin[23:33]=STRSPLIT(line,/EXTRACT)
      readf,lun2,line
linefin[34:44]=STRSPLIT(line,/EXTRACT)
      readf,lun2,line
linefin[45:48]=STRSPLIT(line,/EXTRACT)
;      result      = STRSPLIT(line,/EXTRACT)
;      result      = STRSPLIT(linefin,/EXTRACT)
result=linefin

      FOR j=0,ntrace-1 DO ch4cat[j,k] = float(result[1+j])  
    ENDFOR
    free_lun,lun2   
        
    ;******************************************************************    
    ; divide into latitude bands
    ind_NH   = WHERE(latc gt 30.,cnh)
    ind_trop = WHERE(latc le 30. and latc ge -30.,ctrop)
    ind_SH   = WHERE(latc lt -30.,csh)
    ind_glob = WHERE(latc ge -90. and latc le 90.,cglob)
;    ind_glob = WHERE(namec eq 'jfj',cglob) ;TEST specific station
;    ind_NH   = WHERE(namec eq 'jfj',cnh)
;    ind_trop = WHERE(namec eq 'jfj',ctrop)
;    ind_SH   = WHERE(namec eq 'jfj',csh)
  
    
    CASE lat OF 
      0: ind = ind_NH
      1: ind = ind_trop
      2: ind = ind_SH
      3: ind = ind_glob
    ENDCASE
    IF lat le 2 THEN BEGIN
      ch4mean[i]  = mean(z[ind],/NAN)
      mch4mean[i] = mean(mall[ind],/NAN)
      mch4post[i] = mean(zmp[ind],/NAN)
;      ch4mean[i]  = mean(z[3],/NAN);TEST
;      mch4mean[i] = mean(mall[3],/NAN);
;      mch4post[i] = mean(zmp[3],/NAN);
      FOR j=0,ntrace-1 DO mch4cat[i,j] = mean(ch4cat[j,ind],/NAN)
      FOR j=0,sim.nage*ntrace-1 DO ch4_catap[i,j] = mean(mch4[j,ind],/NAN) ;prior flo
    ENDIF ELSE BEGIN
      ch4mean[i]  = (mean(z[ind_NH],/NAN)+mean(z[ind_trop],/NAN)+mean(z[ind_SH],/NAN))/3. 
      mch4mean[i] = (mean(mall[ind_NH],/NAN)+mean(mall[ind_trop],/NAN)+mean(mall[ind_SH],/NAN))/3.
      mch4post[i] = (mean(zmp[ind_NH],/NAN)+mean(zmp[ind_trop],/NAN)+mean(zmp[ind_SH],/NAN))/3.
      FOR j=0,ntrace-1 DO mch4cat[i,j] = (mean(ch4cat[j,ind_NH],/NAN)+$
                                          mean(ch4cat[j,ind_trop],/NAN)+$
                                          mean(ch4cat[j,ind_SH],/NAN))/3.
      FOR j=0,sim.nage*ntrace-1 DO ch4_catap[i,j] = (mean(mch4[j,ind_NH],/NAN)+$
                                          mean(mch4[j,ind_trop],/NAN)+$
                                          mean(mch4[j,ind_SH],/NAN))/3.
    ENDELSE

    ; get next month by adding 40 days and rounding off to full months
    cyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)

    icorr += 1

 ENDFOR

;;;; TAKE APRIORI CAT NOT APOST ;;;;
;ch4_catap= FltArr(sim.nage*ntrace,nc)
;FOR j=0,sim.nage*ntrace-1 DO ch4_catap[i,j] = mean(mch4[j,ind],/NAN)

;mch4cat --> ch4_catap

mch4catap=mch4cat*0
FOR j=0,47 DO mch4catap[*,0+j]=(ch4_catap[*,0+j]+ch4_catap[*,48+j]+ch4_catap[*,96+j]+ch4_catap[*,144+j]+ch4_catap[*,192+j])


mch4cat=mch4catap
;stop



;plot,z,yrange=[1600,1900] 
;oplot,mall,col=4
;oplot,zmp,col=10 
;stop 
    
  ch4diff       = DblArr(m)
  mch4diff      = DblArr(m)
  sch4diff      = DblArr(m)
  pmch4diff     = DblArr(m)
  cmch4diff     = DblArr(m,ntrace)
  
  diffsmooth    = DblArr(m) ; obs

  mdiffsmooth   = DblArr(m) ; apriori model
  sdiffsmooth   = DblArr(m) ; apriori model constant OH
  pmdiffsmooth  = DblArr(m) ; aposteriori model
  cmdiffsmooth  = DblArr(m,ntrace) ; categories aposteriori model
  
  ch4diff[*]     = !VALUES.D_NAN & mch4diff[*]       = !VALUES.D_NAN
  diffsmooth[*]  = !VALUES.D_NAN & mdiffsmooth[*]    = !VALUES.D_NAN
  pmch4diff[*]   = !VALUES.D_NAN & pmdiffsmooth[*]   = !VALUES.D_NAN
  cmch4diff[*,*] = !VALUES.D_NAN & cmdiffsmooth[*,*] = !VALUES.D_NAN
  sch4diff[*]    = !VALUES.D_NAN & sdiffsmooth[*]    = !VALUES.D_NAN  

  ; annual running mean of latitudinal mean CH4, obs and different model series
  ; for period 2002 to 2008
  ch4smooth[*]   = smooth(ch4mean,1,/EDGE_TRUNCATE)  ; running mean of CH4 curve
  mch4smooth[*]  = smooth(mch4mean,1,/EDGE_TRUNCATE)
  pmch4smooth[*] = smooth(mch4post,1,/EDGE_TRUNCATE)
  FOR j=0,ntrace-1 DO cmch4smooth[*,j] = smooth(mch4cat[*,j],1,/EDGE_TRUNCATE)
  
;  ch4smooth[*]   = smooth(ch4mean,3,/EDGE_TRUNCATE)  ; running mean of CH4 curve
;  mch4smooth[*]  = smooth(mch4mean,3,/EDGE_TRUNCATE)
;  IF keyword_set(sensoh) THEN sch4smooth[*]  = smooth(sch4mean,3,/EDGE_TRUNCATE) 
;  pmch4smooth[*] = smooth(mch4post,3,/EDGE_TRUNCATE)
;  FOR j=0,mtrace-1 DO cmch4smooth[*,j] = smooth(mch4cat[*,j],3,/EDGE_TRUNCATE)
  
  
  FOR i=6,m-7 DO BEGIN 
    ch4diff[i]   = ch4smooth[i+6]-ch4smooth[i-6]
    mch4diff[i]  = mch4smooth[i+6]-mch4smooth[i-6]
    pmch4diff[i] = pmch4smooth[i+6]-pmch4smooth[i-6]
    FOR j=0,ntrace-1 DO cmch4diff[i,j] = cmch4smooth[i+6,j]-cmch4smooth[i-6,j]
  ENDFOR 
  
  diffsmooth[6:m-7]   = smooth(ch4diff[6:m-7],12,/EDGE_TRUNCATE)
  mdiffsmooth[6:m-7]  = smooth(mch4diff[6:m-7],12,/EDGE_TRUNCATE)
  pmdiffsmooth[6:m-7] = smooth(pmch4diff[6:m-7],12,/EDGE_TRUNCATE)
  FOR j=0,ntrace-1 DO cmdiffsmooth[6:m-7,j] = smooth(cmch4diff[6:m-7,j],12,/EDGE_TRUNCATE)
  
;flo
; cmdiffsmooth[where(finite(cmdiffsmooth) eq 0)] = 0 ;flo
;totcm=DblArr(37) 
; FOR cc=0,36 DO totcm[cc]=total(cmdiffsmooth[cc,*])

; mch4cat[where(finite(mch4cat) eq 0)] = 0 ;flo
;totmch4=DblArr(37) 
; FOR cc=0,36 DO totmch4[cc]=total(mch4cat[cc,*])
;flo
;totmch42=DblArr(37)
;FOR cc=0,36 DO totmch42[cc]=total(posaccum[cc,*]+negaccum[cc,*])

  ; accumulate category growth rates divided up for pos. and neg. anomalies
  posaccum = FltArr(m,ntrace)
  negaccum = FltArr(m,ntrace)
  post_tot = FltArr(m)
  pindex   = IntArr(m,ntrace)
  nindex   = IntArr(m,ntrace)
  npos     = IntArr(m)
  nneg     = IntArr(m)
  posaccum[*,*] = 0.
  negaccum[*,*] = 0.
  kpos = 0
  kneg = 0
  j = 0        
  FOR i=6,m-7 DO BEGIN 
     pind  = WHERE(cmdiffsmooth[i,*] gt 0.,cpos)
     nind  = WHERE(cmdiffsmooth[i,*] lt 0.,cneg)
    IF cpos gt 0L THEN BEGIN
      phelp    = FltArr(cpos)
      phelp[0] = cmdiffsmooth[i,pind[0]]
      FOR k=1,cpos-1 DO phelp[k] = phelp[k-1]+cmdiffsmooth[i,pind[k]] 
      posaccum[i,0:cpos-1] = phelp[*]
      pindex[i,0:cpos-1]   = pind[*]
      npos[i]              = cpos
    ENDIF
    IF cneg gt 0L THEN BEGIN
      nhelp = FltArr(cneg)
      nhelp[0] = cmdiffsmooth[i,nind[0]]
      FOR k=1,cneg-1 DO nhelp[k] = nhelp[k-1]+cmdiffsmooth[i,nind[k]]      
      negaccum[i,0:cneg-1] = nhelp[*]
      nindex[i,0:cneg-1]   = nind[*]
      nneg[i]              = cneg
    ENDIF
 ENDFOR

  FOR i=6,m-7 DO BEGIN
; FOR ii=0,npos[i]-1 DO BEGIN
post_tot[i]=total(posaccum[i,*])+total(negaccum[i,*])
ENDFOR
;ENDFOR


;flo : group into larger categories
posaccum_new = FltArr(m,ntrace)
  negaccum_new = FltArr(m,ntrace)
posaccum_new[*,*] = 0.
  negaccum_new[*,*] = 0.
FOR i=6,m-7 DO BEGIN
posaccum_new[i,0]=total(posaccum[i,0:10])
negaccum_new[i,0]=total(negaccum[i,0:10])
posaccum_new[i,1]=total(posaccum[i,11:23])
negaccum_new[i,1]=total(negaccum[i,11:23])
posaccum_new[i,2]=total(posaccum[i,24:30])
negaccum_new[i,2]=total(negaccum[i,24:30])
posaccum_new[i,3]=total(posaccum[i,31:43])
negaccum_new[i,3]=total(negaccum[i,31:43])
posaccum_new[i,4]=total(posaccum[i,44:47])
negaccum_new[i,4]=total(negaccum[i,44:47])
ENDFOR

;;;;NEW smoothing ;;;;

;FOR i=0,43 DO BEGIN
;posaccum_new[6:m-7,i]=smooth(posaccum[6:m-7,i],12)
;negaccum_new[6:m-7,i]=smooth(negaccum[6:m-7,i],12)

;ENDFOR

;stop

;filename_ncdf='inversion_maiolica_mai11_1995.nc'
filename_ncdf='growth_rates_dlr.nc'


; Create nc-file:
ncid4=NCDF_CREATE(filename_ncdf,/CLOBBER)

time = 1.+findgen(241-1)
ntime=N_ELEMENTS(time)

tracer = 1.+findgen(49-1)
ntracer=N_ELEMENTS(tracer)

;diffsmooth
;mdiffsmooth
;pmdiffsmooth
;negaccum
;posaccum

timedimid=NCDF_DIMDEF(ncid4,'time',ntime)
tracerdimid=NCDF_DIMDEF(ncid4,'tracer',ntracer)


timeid=NCDF_VARDEF(ncid4, 'time', [timedimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, timeid, 'long_name', 'tim'
    NCDF_ATTPUT, ncid4, timeid, 'units', 'months since 1989-12-1-0:0:0'

tracerid=NCDF_VARDEF(ncid4, 'tracer', [tracerdimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, tracerid, 'long_name', 'tracers'
    NCDF_ATTPUT, ncid4, tracerid, 'units', 'tracers'

    apriorid=NCDF_VARDEF(ncid4, 'apriori', [timedimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, apriorid, 'long_name', 'priori ch4'
    NCDF_ATTPUT, ncid4, apriorid, 'units', 'ppm'

    apostid=NCDF_VARDEF(ncid4, 'aposteriori', [timedimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, apostid, 'long_name', 'posterior'
    NCDF_ATTPUT, ncid4, apostid, 'units', 'ppm'

    obsid=NCDF_VARDEF(ncid4, 'obs', [timedimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, apostid, 'long_name', 'observ'
    NCDF_ATTPUT, ncid4, apostid, 'units', 'ppm'

    positid=NCDF_VARDEF(ncid4, 'posit', [timedimid,tracerdimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, positid, 'long_name', 'pos cat ch4'
    NCDF_ATTPUT, ncid4,positid, 'units', 'ppm'

    negitid=NCDF_VARDEF(ncid4, 'negit', [timedimid,tracerdimid], /DOUBLE)
    NCDF_ATTPUT, ncid4, negitid, 'long_name', 'neg cat ch4'
    NCDF_ATTPUT, ncid4, negitid, 'units', 'ppm'


    NCDF_CONTROL, ncid4, /ENDEF    

   NCDF_VARPUT, ncid4, tracerid, tracer          ; Latitudes
    NCDF_VARPUT, ncid4, timeid, time      ; Latitudes
    NCDF_VARPUT, ncid4, obsid,diffsmooth
    NCDF_VARPUT, ncid4, apostid,pmdiffsmooth
    NCDF_VARPUT, ncid4, apriorid,mdiffsmooth
    NCDF_VARPUT, ncid4,  negitid,negaccum
    NCDF_VARPUT, ncid4,  positid,posaccum

NCDF_CLOSE, ncid4


  ;****************************************
  ;; plot results
  ;****************************************
  load_ctb,'/home/arf/pers/IDL/COLORTABLES/diff3.ctb'
 
  CASE lat OF 
    0: np = 'NHextratropics'
    1: np = 'tropics'
    2: np = 'SHextratropics'
    3: np = 'global'
  ENDCASE
 
  IF keyword_set(flask) THEN BEGIN
    IF keyword_set(nobg) THEN $    
         file='/home/arf/pers/IDL/EPS/URMEL/INVERSION/ch4_growthrates_flask_nobg_'+sn+'_'+sim.name+'_'+np+'_inversion_cat_'+qunc+'.eps' $    
    ELSE file='/home/arf/pers/IDL/EPS/URMEL/INVERSION/ch4_growthrates_flask_DLR_'+sn+'_'+sim.name+'_'+np+'_inversion_cat_'+qunc+'.eps'  
  ENDIF ELSE BEGIN
    IF keyword_set(nobg) THEN $
         file='/home/arf/pers/IDL/EPS/URMEL/INVERSION/ch4_growthrates_nobg_'+sn+'_'+sim.name+'_'+np+'_inversion_cat_'+qunc+'.eps' $    
    ELSE file='/home/arf/pers/IDL/EPS/URMEL/INVERSION/ch4_growthrates_'+sn+'_'+sim.name+'_'+np+'_inversion_cat_'+qunc+'_apriori_sey.eps'
  ENDELSE
  open_ps,file,pssize=[20,14],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !p.font=1

  position = [0.15,0.35,0.95,0.9]

  xtickname = StrArr(nyears)
  FOR ij=0,nyears-1 DO xtickname[ij] = STRCOMPRESS(string(syyyy+ij+1),/REM)
  xticks = n_elements(xtickname)-1  
  xtickv = 0+12*indgen(nyears)
  x      = 0+indgen(m)
  time   = x

;  yrangea = [-10.,11.]
  yrangea = [-50.,110.]
;  IF lat eq 0 THEN yrangea = [-20.,25.]
;  IF lat eq 3 THEN yrangea = [-10.,15.]
;  IF lat eq 1 THEN yrangea = [-15.,16.]
 IF lat eq 0 THEN yrangea = [-15.,50.]
  IF lat eq 3 THEN yrangea = [-15.,50.]
  IF lat eq 3 THEN yrangea = [-20.,30.] ;flo
  IF lat eq 1 THEN yrangea = [-15.,50.]
  xrange  = [0,m-1]
  ytitlea = 'd(CH!I4!N)/dt (ppbv yr!E-1!N)'
  xminor  = 0

  null = FltArr(n)
  null[*] = 0.

  col    = [4,5,6,132,19,20,9,11,14,15,207]
  col    = [4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19,4,5,6,132,19]

col    = [11,11,11,11,11,11,11,11,11,11,11,254,254,254,254,254,254,254,254,254,254,254,254,254,21,21,21,21,21,21,21,76,76,76,76,76,76,76,76,76,76,76,76,76,120,120,120,120]

col    = [cgcolor("OLIVE"),cgcolor("DARKGREEN"),cgcolor("PALEGREEN"),cgcolor("AQUAMARINE"),cgcolor("GREENYELLOW"),cgcolor("TEAL"),cgcolor("LIGHTSEAGREEN"),cgcolor("GREEN"),cgcolor("KHAKI"),cgcolor("SPRINGGREEN"),cgcolor("LIMEGREEN"),cgcolor("RED"),cgcolor("ORANGERED"),cgcolor("CRIMSON"),cgcolor("FIREBRICK"),cgcolor("SALMON"),cgcolor("DARKRED"),cgcolor("TOMATO"),cgcolor("PINK"),cgcolor("ROSE"),cgcolor("VIOLETRED"),cgcolor("MAGENTA"),cgcolor("SIENNA"),cgcolor("ORANGE"),cgcolor("BEIGE"),cgcolor("SEASHELL"),cgcolor("LIGHTYELLOW"),cgcolor("PAPAYA"),cgcolor("WHEAT"),cgcolor("BURLYWOOD"),cgcolor("LIGHTGRAY"),cgcolor("BLUE"),cgcolor("ROYALBLUE"),cgcolor("NAVY"),cgcolor("STEELBLUE"),cgcolor("CADETBLUE"),cgcolor("CORNFLOWERBLUE"),cgcolor("SKYBLUE"),cgcolor("DARKSLATEBLUE"),cgcolor("PURPLE"),cgcolor("POWDERBLUE"),cgcolor("DODGERBLUE"),cgcolor("YGB3"),cgcolor("TURQUOISE"),cgcolor("BLACK"),cgcolor("YELLOW"),cgcolor("GOLD"),cgcolor("GOLDENROD")]

; catname = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA','ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
;          'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS','BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
;'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_EU','WETL_MID','WETL_NAbor','WETL_NAFR','WETL_RUS','WETL_SAtemp','WETL_SAFR','WETL_CHIN','WETL_IND',$
;'WETL_NAtemp','WETL_SAtrop','WETL_SE_ASIA','WILD_anim','TERMITES','OCEAN','Volc','Anthropogenic','Biomass Burning','Rice','Wetland']



;col    = [11,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254]


 ; col    = [4,5,6,132,19]


  noerase = 1
  plot,x,diffsmooth,/xst,ytitle=ytitlea,yrange=yrangea,xrange=xrange,xminor=xminor,/yst,$
       ycharsize=1.6,xcharsize=0.8,xtickv=xtickv,xticks=xticks,xtickname=xtickname,charthick=0.1,$
       position=position,/nodata,noerase=noerase,noclip=0

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
          lowbound[0] = posaccum[it,i-1]
          lowbound[1] = posaccum[it,i-1]
         
        ENDELSE
        linre = [time[it]-0.5,time[it]+0.5,time[it]+0.5,time[it]-0.5,time[it]-0.5]
       
        obun  = [lowbound[0],lowbound[1],posaccum[it,i],posaccum[it,i],lowbound[0]]
       ; polyfill,linre,obun,color=col[pindex[it,i]],/FILL
      ; polyfill,linre,obun,color=col[pindex[it,i]],/FILL
       cgColorfill,linre,obun,color=col[pindex[it,i]],/FILL
      ENDFOR
    ENDIF
  ENDFOR
  
  FOR it=0,m-2 DO BEGIN
    IF nneg[it] gt 0 THEN BEGIN
      FOR i=0,nneg[it]-1 DO BEGIN
        IF i eq 0 THEN BEGIN
          upbound[*] = 0. 
        ENDIF ELSE BEGIN
          upbound[0] = negaccum[it,i-1]
          upbound[1] = negaccum[it,i-1]
        ENDELSE
        linre = [time[it]-0.5,time[it]+0.5,time[it]+0.5,time[it]-0.5,time[it]-0.5]
        obun  = [upbound[0],upbound[1],negaccum[it,i],negaccum[it,i],upbound[0]]
        ;polyfill,linre,obun,color=col[nindex[it,i]],/FILL
        ;polyfill,linre,obun,color=col[nindex[it,i]],/FILL
        cgColorFill,linre,obun,color=col[nindex[it,i]],/FILL

      ENDFOR
    ENDIF
  ENDFOR

;  oplot,x,diffsmooth,thick=6,linestyle=0,color=255 ;flo removed
;  oplot,x,mdiffsmooth,thick=5,linestyle=1,color=255 ;flo removed
  ;IF keyword_set(sensoh) THEN oplot,x,sch4diff,thick=5,linestyle=2,color=255
;  oplot,x,pmdiffsmooth,thick=5,linestyle=3,color=255 ;flo removed
 ;oplot,x,post_tot,thick=5,linestyle=3,color=255 ;flo
  oplot,x,null,thick=2,linestyle=2,color=255
  
  cdat  = 'WDCGG data'
  cmod1 = 'Model apriori'
  cmod2 = 'Model aposteriori'  
  dx  = 2.8
  dy  = 5.
  IF lat eq 0 THEN dy  = 15.
  IF lat eq 1 THEN dy  = 10.

;; COMMENTED FLO
;  plots,5.,-3.5-dy
;  plots,8.,-3.5-dy,linestyle=0,color=255,thick=4,/CONTINUE
;  xyouts,9.,-4.-dy,cdat,charsize=1.8,color=255
   
;  plots,5.+9.*dx,-3.5-dy
;  plots,8.+9.*dx,-3.5-dy,linestyle=1,color=255,thick=4,/CONTINUE
;  xyouts,9.+9.*dx,-4.-dy,cmod1,charsize=1.8,color=255
   
;  plots,5.+18.*dx,-3.5-dy
;  plots,8.+18.*dx,-3.5-dy,linestyle=2,color=255,thick=4,/CONTINUE
;  xyouts,9.+18.*dx,-4.-dy,cmod2,charsize=1.8,color=255   

  IF lat eq 0 THEN dabc = 5.
  IF lat eq 1 THEN dabc = 3.
  IF lat eq 2 THEN dabc = 2.
  IF lat eq 3 THEN dabc = 3.
  
  IF keyword_set(abc) THEN xyouts,5.,yrangea[1]-dabc,abc,charsize=1.8,color=255

  IF keyword_set(legend) THEN BEGIN
  ; * ADD LEGEND
  ; **********************
;  c = ['Agriculture','Fuel production','Waste and combustion','BBTropics','BB Extratropics',$
;       'Rice agriculture','Inundated wetlands','Mineral soils','Peatlands North America',$
;       'Peatlands Eurasia', 'Other natural']

;c = ['Anthropogenic','Biomass burning','Rice','Wetlands','Other']
;col    = [11,254,21,76,120]

c = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA','ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
          'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS','BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_EU','WETL_MID','WETL_NAbor','WETL_NAFR','WETL_RUS','WETL_SAtemp','WETL_SAFR','WETL_CHIN','WETL_IND',$
'WETL_NAtemp','WETL_SAtrop','WETL_SE_ASIA','WILD_anim','TERMITES','OCEAN','Volc']

col    = [cgcolor("OLIVE"),cgcolor("DARKGREEN"),cgcolor("PALEGREEN"),cgcolor("AQUAMARINE"),cgcolor("GREENYELLOW"),cgcolor("TEAL"),cgcolor("LIGHTSEAGREEN"),cgcolor("GREEN"),cgcolor("KHAKI"),cgcolor("SPRINGGREEN"),cgcolor("LIMEGREEN"),cgcolor("RED"),cgcolor("ORANGERED"),cgcolor("CRIMSON"),cgcolor("FIREBRICK"),cgcolor("SALMON"),cgcolor("DARKRED"),cgcolor("TOMATO"),cgcolor("PINK"),cgcolor("ROSE"),cgcolor("VIOLETRED"),cgcolor("MAGENTA"),cgcolor("SIENNA"),cgcolor("ORANGE"),cgcolor("BEIGE"),cgcolor("SEASHELL"),cgcolor("LIGHTYELLOW"),cgcolor("PAPAYA"),cgcolor("WHEAT"),cgcolor("BURLYWOOD"),cgcolor("LIGHTGRAY"),cgcolor("BLUE"),cgcolor("ROYALBLUE"),cgcolor("NAVY"),cgcolor("STEELBLUE"),cgcolor("CADETBLUE"),cgcolor("CORNFLOWERBLUE"),cgcolor("SKYBLUE"),cgcolor("DARKSLATEBLUE"),cgcolor("PURPLE"),cgcolor("POWDERBLUE"),cgcolor("DODGERBLUE"),cgcolor("YGB3"),cgcolor("TURQUOISE"),cgcolor("BLACK"),cgcolor("YELLOW"),cgcolor("GOLD"),cgcolor("GOLDENROD")]


  dy = 0.006
  ; anthropogenic emissions
  FOR i=0,4 DO BEGIN
    top    = 0.22-0.04*i
    bottom = 0.19-0.04*i
    left   = 0.15
    right  = 0.17
    linre  = [left,right,right,left,left]
    obun   = [bottom,bottom,top,top,bottom]
    polyfill,linre,obun,color=col[i+30],/FILL,/normal

    xyouts,right+0.01,bottom+dy,c[i+30],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR

  FOR i=0,4 DO BEGIN
    top    = 0.22-0.04*i
    bottom = 0.19-0.04*i
    left   = 0.4
    right  = 0.42
    linre  = [left,right,right,left,left]
    obun   = [bottom,bottom,top,top,bottom]
    polyfill,linre,obun,color=col[i+35],/FILL,/normal

    xyouts,right+0.01,bottom+dy,c[i+35],charsize=1.1,charthick=1.2,color=255,/normal
  ENDFOR
 ;  other natural
  FOR i=0,2 DO BEGIN
    top    = 0.22-0.04*i
    bottom = 0.19-0.04*i
    left   = 0.68
    right  = 0.7
    linre  = [left,right,right,left,left]
    obun   = [bottom,bottom,top,top,bottom]
    polyfill,linre,obun,color=col[i+45],/FILL,/normal
    xyouts,right+0.01,bottom+dy,c[i+45],charsize=1.1,charthick=1.2,color=255,/normal
 ENDFOR

;FOR i=0,4 DO BEGIN
;    top    = 0.22-0.04*i
;    bottom = 0.19-0.04*i
;    left   = 0.95
;    right  = 0.97
;    linre  = [left,right,right,left,left]
;    obun   = [bottom,bottom,top,top,bottom]
;    polyfill,linre,obun,color=col[i+15],/FILL,/normal
;    xyouts,right+0.01,bottom+dy,c[i+15],charsize=1.1,charthick=1.2,color=255,/normal
;  ENDFOR
  
  ENDIF

  plot,x,ch4diff,/xstyle,ytitle=ytitle,yrange=yrangea,xrange=xrange,/yst,$
       charsize=1.6,xtickv=xtickv,xticks=xticks,xtickname=xtickname,charthick=1.,$
       position=position,/nodata,/noerase,title=title,noclip=0
  
  close_ps
  !p.font=-1
 
 stop
END
