;+
; NAME:
;
;   plot_inv_methane_seasonalcycle
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
PRO plot_inv_methane_seasonalcycle_final,sim=sim,lat=lat,stats=stats

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'URMEL_SENSC_II',$
          obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
          modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
            outdir:'/home/spc134/IDL/urmel/INVERSION/',$
             hdir: '/nas/spc134/URMEL/INVERSION/SENSITIVITIES/',$
          syyyymm:'200002',eyyyymm:'200812',scaleq:[0.1,0.1,0.1,0.65,0.85,0.3,0.35,0.45,0.55,0.95,0.85],$
          ntrace:11,nage:4}

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200912',scaleq:[0.950000 ,    0.950000 ,    0.650000,     0.650000 ,    0.950000   ,  0.950000, 0.450000  ,   0.950000 ,    0.950000   ,  0.950000   ,  0.950000  ,   0.950000, 0.950000   ,  0.950000 ,    0.95000, 0.950000   ,   0.00000   ,  0.950000 , 0.950000  ,    0.00000  ,   0.950000   ,   0.00000  ,   0.950000  ,    0.00000 , 0.950000   ,  0.950000  ,   0.950000   ,  0.950000  ,   0.950000  ,   0.950000  , 0.950000 ,    0.950000   ,  0.700000 ,    0.950000 ,    0.950000  ,   0.950000 , 0.850000  ,   0.950000  ,   0.950000  ,   0.800000  ,   0.950000   ,  0.100000 ,  1.66000  ,    1.66000  ,    1.45000  ,    1.45000  ,    1.45000  ,    1.45000],$ ; MAX LLH
ntrace:48,nage:5} 

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
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1


  ENDIF
                 
  IF n_elements(stats) EQ 0 THEN BEGIN
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

 stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                   'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']

  ENDIF  

flask=1
                   
  sn = STRCOMPRESS(string(n_elements(stats)),/REM)+'stats'   

  ; calculate number of months to simulate
  syyyy = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n = eyyyy*12L+emm-(syyyy*12L+smm)+1
  nyears = eyyyy-2001+1
  nyears = eyyyy-1990+1

  print, 'Run compute_growth_rates for period ', syyyy, ' to ', eyyyy

  m = n-11
  ntrace = sim.ntrace  

  ch4mean     = DblArr(m) ; observations
  mch4mean    = DblArr(m) ; apriori model
  sch4mean    = DblArr(m) ; apriori model with constant OH
  mch4post    = DblArr(m) ; CH4 series according to aposteriori emissions estimates
  mch4cata    = DblArr(m,ntrace) ; CH4 apriori series individual cats
  mch4catp    = DblArr(m,ntrace) ; CH4 aposteriori series individual cats 

  x           = 1.+findgen(m)
  seasa       = FltArr(m,ntrace)
  seasp       = FltArr(m,ntrace)
  seasalla    = FltArr(m)
  seasallp    = FltArr(m)
  
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  syra  = sim.syyyymm
  syre  = sim.eyyyymm
  IF keyword_set(flask) THEN $
  testfile = sim.outdir+'inv_output_weekly_flask_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt' $
  ELSE testfile = sim.outdir+'inv_output_weekly_'+sn+'_'+sim.name+'_'+syra+'-'+syre+'_'+qunc+'_nov12.txt'
  
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
  cyyyymm = '199001' 
  icorr   = 0
  FOR i=0,m-1 DO BEGIN
  
    ; read observational data
    read_data_single_final,sim=sim,yyyymm=cyyyymm,ch4obs=ch4obs,dtgobs=dtgobs,$
                     lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,/weekly,stats=stats,flask=flask
                     
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
    sensfile = sim.hdir+'inv_sensitivity_weekly_flask_'+sn+'_'+sim.name+'_'+cyyyymm+'_nov12.txt' $
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
        xp = DblArr(sim.ntrace) & xa = DblArr(sim.ntrace)
        xp[*]  = sp[icorr,*]  
        xa[*]  = sa[icorr,*]                       
      ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
        xp = DblArr(sim.ntrace*2) & xa = DblArr(sim.ntrace*2)
        xp[0:sim.ntrace-1]             = sp[icorr,*]
        xp[sim.ntrace:2*sim.ntrace-1]  = sp[icorr-1,*]
        xa[0:sim.ntrace-1]             = sa[icorr,*]
        xa[sim.ntrace:2*sim.ntrace-1]  = sa[icorr-1,*]                
      ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
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



    ; read model data
    IF keyword_set(flask) THEN $
    modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat' $
    ELSE modfile = '/nas/arf/INVERSION/m_allweekly_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'
    mch4  = DblArr(sim.ntrace*sim.nage,nc)
    mch4a = DblArr(ntrace,nc) 
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
;      FOR it=0,ntrace-1 DO mch4a[it,k] = mch4[0+it,k]+mch4[11+it,k]+mch4[22+it,k]+mch4[33+it,k]
;   ENDFOR
    FOR it=0,ntrace-1 DO mch4a[it,k] = mch4[0+it,k]+mch4[48+it,k]+mch4[96+it,k]+mch4[144+it,k]+mch4[192+it,k]
    ENDFOR

    free_lun,lun
        
    BG = DblArr(nc)
    FOR k=0,nc-1 DO BG[k] = ccorr[icorr]*total(mch4[(sim.nage-1)*sim.ntrace:sim.nage*sim.ntrace-1,k])
    
    ; methane values acccording to aposteriori estimates of methane emissions 
    zmp = H ## xp + BG

    ; now read weekly aposteriori model estimates for individual categories
    IF keyword_set(flask) THEN $
    catfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_flask_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat' $
    ELSE catfile = '/nas/arf/INVERSION/m_weekly_apost_categories_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'
    ch4cat  = FltArr(ntrace,nc)
    openr,lun2,catfile,/get_lun
    FOR k=0,nc-1 DO BEGIN
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
    ;  result      = STRSPLIT(line,/EXTRACT)
      FOR j=0,ntrace-1 DO ch4cat[j,k] = float(result[1+j])  
    ENDFOR
    free_lun,lun2   
        
    ;******************************************************************    
    ; divide into latitude bands
    ind_NH   = WHERE(latc gt 30.,cnh)
    ind_trop = WHERE(latc le 30. and latc ge -30.,ctrop)
    ind_SH   = WHERE(latc lt -30.,csh)
    ind_glob = WHERE(latc ge -90. and latc le 90.,cglob)
    
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
      FOR j=0,ntrace-1 DO mch4cata[i,j] = mean(mch4a[j,ind],/NAN)
      FOR j=0,ntrace-1 DO mch4catp[i,j] = mean(ch4cat[j,ind],/NAN)
    ENDIF ELSE BEGIN
      ch4mean[i]  = (mean(z[ind_NH],/NAN)+mean(z[ind_trop],/NAN)+mean(z[ind_SH],/NAN))/3. 
      mch4mean[i] = (mean(mall[ind_NH],/NAN)+mean(mall[ind_trop],/NAN)+mean(mall[ind_SH],/NAN))/3.
      mch4post[i] = (mean(zmp[ind_NH],/NAN)+mean(zmp[ind_trop],/NAN)+mean(zmp[ind_SH],/NAN))/3.
      FOR j=0,ntrace-1 DO mch4catp[i,j] = (mean(ch4cat[j,ind_NH],/NAN)+$
                                            mean(ch4cat[j,ind_trop],/NAN)+$
                                            mean(ch4cat[j,ind_SH],/NAN))/3.
      FOR j=0,ntrace-1 DO mch4cata[i,j] = (mean(mch4a[j,ind_NH],/NAN)+$
                                            mean(mch4a[j,ind_trop],/NAN)+$
                                            mean(mch4a[j,ind_SH],/NAN))/3.                                         
    ENDELSE
  
    ; get next month by adding 40 days and rounding off to full months
    cyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)

    icorr += 1

  ENDFOR
    
  FOR j=0,11 DO BEGIN
    ind=WHERE( ((x) MOD 12) EQ j)
    FOR it=0,ntrace-1 DO BEGIN
      seasa[ind,it] = mean(mch4cata[ind,it],/nan)
      seasp[ind,it] = mean(mch4catp[ind,it],/nan)
    ENDFOR
    seasalla[ind] = mean(mch4mean[ind],/nan)
    seasallp[ind] = mean(mch4post[ind],/nan)
  ENDFOR  


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
 
  file='/home/arf/pers/IDL/EPS/URMEL/INVERSION/ch4_seasonalcycle_cats_'+sn+'_'+sim.name+'_'+np+'_'+qunc+'.eps'
  load_ctb,'/home/spc134/IDL/GEOP/diff3.ctb'
  open_ps,file,pssize=[24,20],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !P.FONT=1

  col    = [5,132,19,20,9,11,14,15,207] 
  col    = [cgcolor("OLIVE"),cgcolor("DARKGREEN"),cgcolor("PALEGREEN"),cgcolor("AQUAMARINE"),cgcolor("GREENYELLOW"),cgcolor("TEAL"),cgcolor("LIGHTSEAGREEN"),cgcolor("GREEN"),cgcolor("KHAKI"),cgcolor("SPRINGGREEN"),cgcolor("LIMEGREEN"),cgcolor("RED"),cgcolor("ORANGERED"),cgcolor("CRIMSON"),cgcolor("FIREBRICK"),cgcolor("SALMON"),cgcolor("DARKRED"),cgcolor("TOMATO"),cgcolor("PINK"),cgcolor("ROSE"),cgcolor("VIOLETRED"),cgcolor("MAGENTA"),cgcolor("SIENNA"),cgcolor("ORANGE"),cgcolor("BEIGE"),cgcolor("SEASHELL"),cgcolor("LIGHTYELLOW"),cgcolor("PAPAYA"),cgcolor("WHEAT"),cgcolor("BURLYWOOD"),cgcolor("LIGHTGRAY"),cgcolor("BLUE"),cgcolor("ROYALBLUE"),cgcolor("NAVY"),cgcolor("STEELBLUE"),cgcolor("CADETBLUE"),cgcolor("CORNFLOWERBLUE"),cgcolor("SKYBLUE"),cgcolor("DARKSLATEBLUE"),cgcolor("PURPLE"),cgcolor("POWDERBLUE"),cgcolor("DODGERBLUE"),cgcolor("YGB3"),cgcolor("TURQUOISE"),cgcolor("BLACK"),cgcolor("YELLOW"),cgcolor("GOLD"),cgcolor("GOLDENROD")]
  
  time   = 0.+findgen(12)
  title  = ''

  cat    = ['Agriculture','Fuel production','Waste and combustion','BB Tropics','BB Extratropics','Rice agriculture','Inundated wetlands','Mineral soils','Peatlands North America',$
            'Peatlands Eurasia','Other natural','All']         


 cat  = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA','ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
          'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS','BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_EU','WETL_MID','WETL_NAbor','WETL_NAFR','WETL_RUS','WETL_SAtemp','WETL_SAFR','WETL_CHIN','WETL_IND',$
'WETL_NAtemp','WETL_SAtrop','WETL_SE_ASIA','WILD_anim','TERMITES','OCEAN','Volc']

  nall   = ntrace+1
  nall   = 12 ;flo

  FOR k=0,nall-1 DO BEGIN
  
    ytitle = ''
    
    IF k le ntrace-1 THEN BEGIN
      ind1 = WHERE(seasa[*,k] eq max(seasa[*,k]),c1)   
      ind2 = WHERE(seasp[*,k] eq max(seasp[*,k]),c2)     
      IF seasa[ind1[0],k] gt seasp[ind2[0],k] THEN maxi = seasa[ind1[0],k] $
      ELSE maxi = seasp[ind2[0],k]
      
      ind1 = WHERE(seasa[*,k] eq min(seasa[*,k]),c1)   
      ind2 = WHERE(seasp[*,k] eq min(seasp[*,k]),c2)     
      IF seasa[ind1[0],k] lt seasp[ind2[0],k] THEN mini = seasa[ind1[0],k] $
      ELSE mini = seasp[ind2[0],k]      
      
      yrange = [mini-mini/4.,maxi+maxi/4.]
    ENDIF ELSE BEGIN
      ind1 = WHERE(seasalla[*] eq max(seasalla[*]),c1)   
      ind2 = WHERE(seasallp[*] eq max(seasallp[*]),c2)     
      IF seasalla[ind1[0]] gt seasallp[ind2[0]] THEN maxi = seasalla[ind1[0]] $
      ELSE maxi = seasallp[ind2[0]]
      
      ind1 = WHERE(seasalla[*] eq min(seasalla[*]),c1)   
      ind2 = WHERE(seasallp[*] eq min(seasallp[*]),c2)     
      IF seasalla[ind1[0]] lt seasallp[ind2[0]] THEN mini = seasalla[ind1[0]] $
      ELSE mini = seasallp[ind2[0]]
      
      yrange = [mini-mini/20.,maxi+maxi/20.]
    ENDELSE
    
    IF k le 1 THEN yrange = [350.,450.]
    IF k eq 2 THEN yrange = [240.,300.]

    CASE k OF
      0: position = [0.06,0.78,0.33,0.98]
      1: position = [0.38,0.78,0.65,0.98]
      2: position = [0.70,0.78,0.97,0.98]
      3: position = [0.06,0.53,0.33,0.73]
      4: position = [0.38,0.53,0.65,0.73]
      5: position = [0.70,0.53,0.97,0.73]
      6: position = [0.06,0.28,0.33,0.48]
      7: position = [0.38,0.28,0.65,0.48]
      8: position = [0.70,0.28,0.97,0.48]
      9: position = [0.06,0.05,0.33,0.23]
     10: position = [0.38,0.05,0.65,0.23]
     11: position = [0.70,0.05,0.97,0.23]
    ENDCASE

  ;  IF k ge 0 THEN  position = [0.06,0.78,0.33,0.98]


    xtickname = ['J','F','M','A','J','J','A','S','O','N','D']
    xticks = n_elements(xtickname)-1

    IF k gt 0 THEN title = ''
    IF k eq 0 THEN ytitle='Tg/yr'
    IF k eq 3 THEN ytitle='Tg/yr'
    IF k eq 6 THEN ytitle='Tg/yr'
    IF k eq 9 THEN ytitle='Tg/yr'
    IF keyword_set(rel) THEN BEGIN
      IF k gt 0 THEN title = ''
      IF k eq 0 THEN ytitle='%'
      IF k eq 3 THEN ytitle='%'
      IF k eq 6 THEN ytitle='%'
      IF k eq 9 THEN ytitle='%'    
    ENDIF

    plot,time,seasallp[0:11],/xstyle,ytitle=ytitle,yrange=yrange,xrange=xrange,ystyle=1,$
         charsize=1.1,xtickv=xtickv,xticks=xticks,ytickname=ytickname,xtickname=xtickname,$
         charthick=1.,position=position,/nodata,/noerase,title=title,noclip=0

    IF k le 10 THEN oplot,time,seasa[0:11,k],color=5,thick=3,noclip=0
    IF k le 10 THEN oplot,time,seasp[0:11,k],color=13,thick=3,noclip=0   
    
    IF k eq 11 THEN oplot,time,seasalla[0:11],color=5,thick=3,noclip=0
    IF k eq 11 THEN oplot,time,seasallp[0:11],color=13,thick=3,noclip=0

    xyouts,position[0]+0.01,position[3]-0.02,cat[k],charsize=1.3,charthick=1.,color=255,/normal
    
    ; add info on amplitude of apriori and aposteriori seasonal cycle
    IF k eq 0 or k eq 2 THEN BEGIN
      dseasa = STRCOMPRESS(string(max(seasa[0:11,k])-min(seasa[0:11,k]),format='(f5.2)'),/REM)+' amplitude seas cycle'
      dseasp = STRCOMPRESS(string(max(seasp[0:11,k])-min(seasp[0:11,k]),format='(f5.2)'),/REM)+' amplitude seas cycle'
      xyouts,position[0]+0.01,position[3]-0.05,dseasa,charsize=1.3,charthick=1.,color=5,/normal
      xyouts,position[0]+0.01,position[3]-0.07,dseasp,charsize=1.3,charthick=1.,color=13,/normal        
    ENDIF
    IF k eq 1 THEN BEGIN
      dseasa = STRCOMPRESS(string(max(seasa[0:11,k])-min(seasa[0:11,k]),format='(f5.2)'),/REM)+' amplitude seas cycle'
      dseasp = STRCOMPRESS(string(max(seasp[0:11,k])-min(seasp[0:11,k]),format='(f5.2)'),/REM)+' amplitude seas cycle'
      xyouts,position[0]+0.01,position[3]-0.12,dseasa,charsize=1.3,charthick=1.,color=5,/normal
      xyouts,position[0]+0.01,position[3]-0.14,dseasp,charsize=1.3,charthick=1.,color=13,/normal        
    ENDIF    
    

  ENDFOR

  ; **********************
  ; * ADD LEGEND
  ; **********************
  c      = ['A posteriori','A priori']
  col    = [13,5]
  cn     = n_elements(c) 
  bottom = 0.06
  dy     = 0.03
  FOR i=0,cn-1 DO xyouts,0.71,bottom+i*dy,c[i],charsize=1.7,charthick=1.2,color=col[i],/normal    

  close_ps
  !p.font=-1
  
          
 
END
