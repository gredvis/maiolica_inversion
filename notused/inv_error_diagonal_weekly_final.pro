;+
; NAME:
;
;   inv_error_diagnoal_weekly
;
; PURPOSE:
;
;   Read weekly observational data (flask and continuous) and
;   weekly mean model data for the number of years of inversion.
;   Calculate monthly diagonal error covariance matrix, sum of measurement and
;   transport errors.
;
;   sigma =
;      sigma(meas)               ( = sqrt(inst precision^2 )
;    + sigma(transport error)    ( = sqrt(mean of the 3-day SDs) )
;    + sigma(sampling frequency) ( = sqrt(var(CH4mod)/nmod) )
;    + sigma(intercalibration)   ( = 0, because all data have been scaled to NOAA04 scale)
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;  inv_error_covariance
;
; INPUTS:
;
;       from: start year of inversion
;       to: end year of inversion
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;       daily or weekly station data in predefined order
;             
; COMMON BLOCKS:
;
;        none
;
; SIDE EFFECTS:
;
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   inv_error_covariance
;
;     Read all available data from continuous/flask stations and model
;     data per month
;     Output daily or weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 22 September 2011
;-
;******************************************************
;* SUBROUTINES
;******************************************************
PRO read_data,sim=sim,dir=dir,syear,stats=stats,ch4obs=ch4stat,dtgobs=dtgstat,lonobs=lonstat,$
              latobs=latstat,nameobs=namestat,numobs=numstat,type=typestat,nobs=nobs,flask=flask,nobg=nobg,special=special

  nmonths = 12
  nst     = n_elements(stats)
  sn      = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'        

  e = 0 
  a0 = ''
  a1 = '' 
  r0 = 0.
  
  dtg  = StrArr(10000)+''
  ch4  = DblArr(10000)+!VALUES.D_NAN
  name = StrArr(10000)+''
  lat  = FltArr(10000)+!VALUES.F_NAN
  lon  = FltArr(10000)+!VALUES.F_NAN
  num  = IntArr(10000)+0
  type = StrArr(10000)+''

  dtgstat  = StrArr(10000,nst)+''
  ch4stat  = DblArr(10000,nst)+!VALUES.D_NAN
  namestat = StrArr(10000,nst)+''
  latstat  = FltArr(10000,nst)+!VALUES.F_NAN
  lonstat  = FltArr(10000,nst)+!VALUES.F_NAN
  numstat  = IntArr(10000,nst)+0
  typestat = StrArr(10000,nst)+''
  nobs     = IntArr(nst)

  FOR im=0,nmonths-1 DO BEGIN
    ;*******************************
    ;* read data
    ;*******************************     
    IF im lt 9 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
    IF keyword_set(flask) THEN BEGIN
      infile   = dir+'z_allweekly_flask_'+sn+'_'+syear+mm+'.dat'     
      IF keyword_set(nobg) THEN infile   = dir+'z_allweekly_flask_nobg_'+sn+'_'+syear+mm+'.dat'      
    ENDIF ELSE BEGIN
      infile   = dir+'z_allweekly_'+sn+'_'+syear+mm+'.dat'    
      IF keyword_set(nobg)    THEN infile   = dir+'z_allweekly_nobg_'+sn+'_'+syear+mm+'.dat'
      IF keyword_set(special) THEN infile   = dir+'z_allweekly_special_'+sn+'_'+syear+mm+'.dat'          
    ENDELSE
    nlines   = FILE_LINES(infile)

    openr,lun,infile,/get_lun
    FOR i=0,nlines-1 DO BEGIN
      readf,lun,a0
      result = STRSPLIT(a0,/EXTRACT)
      dtg[e]  = result[0]
      name[e] = result[1]
      lat[e]  = float(result[2])
      lon[e]  = float(result[3])
      ch4[e]  = float(result[4])
      num[e]  = fix(result[5])
      type[e] = result[6]
      e       += 1
    ENDFOR
    free_lun,lun

  ENDFOR ; end loop over months

  FOR i=0,nst-1 DO BEGIN
    ind = WHERE(stats[i] eq name,cst)
    IF cst gt 0L THEN BEGIN
    
      y     = float(ch4[ind])
      miin  = mean(y)
      ober  = miin + 3.*stddev(y)
      indo  = WHERE(y gt ober,c,complement=include)
      IF c gt 0L THEN BEGIN
        nobs[i] = cst-c
        dtgstat[0:cst-c-1,i]  = dtg[ind[include]]
        latstat[0:cst-c-1,i]  = lat[ind[include]]
        lonstat[0:cst-c-1,i]  = lon[ind[include]]
        ch4stat[0:cst-c-1,i]  = y[include]
        numstat[0:cst-c-1,i]  = num[ind[include]]
        typestat[0:cst-c-1,i] = type[ind[include]]       
      ENDIF ELSE BEGIN   
        dtgstat[0L:cst-1L,i]  = dtg[ind]
        namestat[0L:cst-1L,i] = name[ind]
        latstat[0L:cst-1L,i]  = lat[ind]
        lonstat[0L:cst-1L,i]  = lon[ind]
        ch4stat[0L:cst-1L,i]  = ch4[ind]
        numstat[0L:cst-1L,i]  = num[ind]
        typestat[0L:cst-1L,i] = type[ind]   
        nobs[i]               = cst
      ENDELSE
      
    ENDIF
           
  ENDFOR

END ; of routine read_data per year

;**************************************
;* SUBROUTINE OF read_model_data
;* to read in FLEXPART receptor output
;**************************************
PRO read_receptor_output,sim=sim,yyyy=yyyy,stats=stats,ntime=ntime,time=timecollect,mch4=mch4collect

  ppbfact = 1.e9
  nmonths = 12
  nst     = n_elements(stats)
sn       = STRCOMPRESS(string(fix(n_elements(stats))),/REM) ;flo
  ;******************************************
  ;* determine optimum model output level for 
  ;* comparison with observations
  ;******************************************

;  filein  = '/nas/spc134/URMEL/INVERSION/choice_modellevel_station-2001to2008.dat'

;filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station-1990to1992.dat' ;FLO

;filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station.dat' ;FLO
;filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station.dat' ;FLO
filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station_'+sn+'.dat' ; FLO


  nlines  = FILE_LINES(filein)
  station = STrArr(nlines)
  mlev    = IntArr(nlines)
  line    = ''
  openr,lun1,filein,/get_lun
  FOR i=0,nlines-1 DO BEGIN
    readf,lun1,line
    result = STRSPLIT(line,/EXTRACT)
    station[i] = result[0]
    mlev[i]    = fix(result[1])
  ENDFOR
  free_lun,lun1
  
  mlevel = 0
  index  = WHERE(stats eq station,cstat)
  IF cstat eq 0 THEN stop
  mlevel = mlev[index]
  
  mch4collect = DblArr(366,nst)
  timecollect = StrArr(366)
  
  tcoll = IntArr(nst)+0
  t     = 0
  FOR im=0,nmonths-1 DO BEGIN
  
    IF im le 8 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
    yyyymm = yyyy+mm
    read_receptors_maiolica_final,sim=sim,yyyymm=yyyymm,info=info,data=data
    ntime  = info[0].time  ; total number of receptor output points in yyyymm
    time   = StrArr(ntime) ; corresponding times in dtg time format
    ; problem with a few days missing in Feb 2006 due to ECMWF model change
    IF yyyymm eq '200602' THEN BEGIN
      FOR it=0,6 DO time[it] = yyyymm+'0'+STRCOMPRESS(string(it+3),/REM)+'0000'
      FOR it=7,ntime-1 DO time[it]=yyyymm+STRCOMPRESS(string(it+3),/REM)+'0000' 
    ENDIF ELSE BEGIN
      FOR it=0,8 DO time[it] = yyyymm+'0'+STRCOMPRESS(string(it+1),/REM)+'0000'
      FOR it=9,ntime-1 DO time[it]=yyyymm+STRCOMPRESS(string(it+1),/REM)+'0000'
    ENDELSE
    FOR it=0,ntime-1 DO timecollect[t+it] = time[it]
    t += ntime
                          
    recname = strlowcase(STRMID(info.rcptname,0,3))
  
    FOR i=0,nst-1 DO BEGIN
      ind        = WHERE(stats[i] eq recname,cout)            ; this can be more data than desired because
                                                              ; of potential several output levels
      IF cout gt 0L THEN BEGIN
        indstat  = WHERE(stats[i] eq station,cstat)
        mlevel = mlev[indstat]    
        cout1      = n_elements(data.pptv[0,ind[mlevel]])     ; cout1 : consider data at one output level only 
        FOR it=0,cout1-1 DO mch4collect[it+tcoll[i],i] = $
        total(data[it].pptv[1:sim.nage*sim.ntrace,ind[mlevel]])/data[it].pptv[0,ind[mlevel]]
        ntime      = cout1
        tcoll[i]  += cout1
      ENDIF ELSE BEGIN
        IF stats[i] eq 'bgu' THEN BEGIN
          ssearch = 'beg'
          slon    = 3.23
          slat    = 41.97
        ENDIF
        IF stats[i] eq 'lpo' THEN BEGIN
          ssearch = 'ile'
          slon    = -3.58
          slat    = 48.8
        ENDIF

        ind1 = WHERE(ssearch eq recname,cout2) ; the station is found in the observations
        IF cout2 ge 1L THEN BEGIN
          indlon = WHERE(float(string(info.xrcpt,format='(f7.2)')) eq slon,clon)
          indlat = WHERE(float(string(info.yrcpt,format='(f7.2)')) eq slat,clat)
          IF clon gt 0L and clat gt 0L THEN BEGIN
            intersect = setintersection(indlon,indlat) ; intersect of modelled lats and lons
            mname     = info[intersect].rcptname       ; search model station name
            clatlon   = n_elements(data.pptv[0,intersect])     ; cout1 : consider data at one output level only
            FOR it=0,clatlon-1 DO $
            mch4collect[tcoll[i]+it,i] = total(data[it].pptv[1:sim.nage*sim.ntrace,intersect])/data[it].pptv[0,intersect]
            tcoll[i] += clatlon
          ENDIF ELSE BEGIN
            print, 'Station ', stats, ' not in model output list! => STOP '
            stop
          ENDELSE
        ENDIF       ; endif station needs to be searched by lat/lon identifyers
      ENDELSE       ; endif station is found in recname or by lat/lon identifyers
    
    ENDFOR          ; end loop over stations stats
  ENDFOR            ; end loop over months in year syear
  
  mch4collect = mch4collect*1.e9
    
END

;* READ FLEXPART DATA
PRO read_model_data,sim=sim,dir=dir,syear,stats=stats,odtg=odtg,type=type,mch4=ch4stat,nmod=numstat

  nst     = n_elements(stats)
  nmonths = 12    
  ncats   = 11
  nlev    = 17
  ppbfact = 1.e9
  days    = ['31','28','31','30','31','30','31','31','30','31','30','31']
  IF syear eq '2004' or syear eq '2008' THEN days[1] = '29'
  
  IF keyword_set(oldest) THEN BEGIN
    ntot = sim.ntrace                ; only one age class
    modch4    = DblArr(10000,nst)+!values.d_nan
  ENDIF ELSE BEGIN
    ntot = sim.ntrace * sim.nage     ; all nage age classes
    modch4    = DblArr(10000,nst)+!values.d_nan
  ENDELSE  
  moddtg      = StrArr(10000,nst) + ''

  ; call subroutine to read model data of current year
  yyyy = syear        
  read_receptor_output,sim=sim,yyyy=yyyy,stats=stats,ntime=ntime,time=time,mch4=mch4       
                  
  dtgstat = StrArr(366,nst)
  ch4stat = DblArr(366,nst)         
  numstat = IntArr(nst)         
                                    
  FOR i=0,nst-1 DO BEGIN
    indstat = WHERE(odtg[*,i] ne '',cstat)
    IF cstat gt 0L THEN BEGIN
      odtgstat = StrArr(cstat) & typestat = StrArr(cstat) & mch4stat = DblArr(cstat)
      odtgstat[*]  = odtg[indstat,i]
      typestat[*]  = type[indstat,i]
      numstat[i]   = cstat
                                
      FOR k=0,cstat-1 DO BEGIN                ; loop over all dates of station stats 
        indz  = WHERE(dtg2hiptime(odtgstat[k]) eq dtg2hiptime(time),cz)  ; where to place model data in month
        
        ;****************************************************************************
        IF cz eq 1L THEN BEGIN   ; station identified          
          IF typestat[k] eq 'ev' THEN BEGIN ; deal with event data and average model data weekly around event
            array = DblArr(7)
            CASE dtg2hiptime(odtgstat[k]) OF
            dtg2hiptime(yyyy+'01010000'): BEGIN              
                                          array[0:3] = mch4[indz,i]
                                          array[4:6] = mch4[indz+1:indz+3,i]
                                          END
            dtg2hiptime(yyyy+'01020000'): BEGIN
                                          array[0:1] = mch4[indz,i]                                              
                                          array[2:6] = mch4[indz-1:indz+3,i]
                                          END
            dtg2hiptime(yyyy+'01030000'): BEGIN
                                          array[0]   = mch4[indz]                                              
                                          array[1:6] = mch4[indz-2:indz+3,i] 
                                          END                                                            
            dtg2hiptime(yyyy+'12290000'): BEGIN
                                          array[0:5] = mch4[indz-3:indz+2,i]                                              
                                          array[6]   = mch4[indz,i]
                                          END 
            dtg2hiptime(yyyy+'12300000'): BEGIN
                                          array[0:4] = mch4[indz-3:indz+1,i]                                              
                                          array[5:6] = mch4[indz,i]
                                          END 
            dtg2hiptime(yyyy+'12310000'): BEGIN
                                          array[0:3] = mch4[indz-3:indz,i]                                              
                                          array[4:6] = mch4[indz,i]
                                          END                                              
                                          ELSE: array[*] = mch4[indz-3:indz+3,i]             
            ENDCASE              
            mch4stat[k] = mean(array,/NAN)
          ENDIF ELSE IF typestat[k] eq 'cn' THEN BEGIN
              
            dat = fix(STRMID(odtgstat[k],6,2))
            mon = fix(STRMID(odtgstat[k],4,2))
            mm  = STRMID(odtgstat[k],4,2)        
            CASE fix(days[mon-1]) OF
              31: BEGIN
                  wmin = ['01','09','17','25'] & wmax = ['08','16','24','31'] & dates = ['04','12','20','28']
                  END
              30: BEGIN
                  wmin = ['01','09','17','25'] & wmax = ['08','16','24','30'] & dates = ['04','12','20','27']
                  END
              28: BEGIN
                  wmin = ['01','08','15','23'] & wmax = ['07','14','22','28'] & dates = ['04','11','18','26']
                  END
              29: BEGIN
                  wmin = ['01','08','15','23'] & wmax = ['07','14','22','29'] & dates = ['04','11','18','26']
                  END
                  ELSE: stop
            ENDCASE        
            FOR j=0,3 DO BEGIN
              inds = WHERE(dat ge fix(wmin[j]) and dat le fix(wmax[j]),c)
             ; IF c eq 1L THEN BEGIN ; commented because 199308 issue FLO
                datmwa = yyyy+mm+wmin[j]+'0000' ; beginning date week j in month
                datmwe = yyyy+mm+wmax[j]+'0000' ; end date of week j in month
             ; ENDIF
              ind = WHERE(dtg2hiptime(time) ge dtg2hiptime(datmwa) and dtg2hiptime(time) le dtg2hiptime(datmwe),cweek)
              IF cweek gt 0L THEN mch4stat[k] = mean(mch4[ind,i],/NAN) ELSE mch4stat[k] = !VALUES.F_NAN
            ENDFOR
          ENDIF  ; endif ev or cn data                                                          
        ENDIF    ; endif cz eq 1                                 
      ENDFOR     ; end loop over all dates of station stats[i]
      ch4stat[0:cstat-1,i] = mch4stat[*]
    ENDIF        ; endif station data available in year         
  ENDFOR         ; end loop over stations 

END
;******************************************************

;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_error_diagonal_weekly_final,sim=sim,stats=stats,flask=flask,ufact=ufact,nobg=nobg,special=special

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'final_sim01',$
          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
          modeldir:'/nas/arf/output/',$
            outdir:'/home/arf/pers/IDL/urmel/INVERSION/SENSITIVITIES/',$
          syyyymm:'198901',eyyyymm:'199212',ntrace:48,nage:5}
  ENDIF

  sn         = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'    
  
  from = fix(STRMID(sim.syyyymm,0,4))
  to   = fix(STRMID(sim.eyyyymm,0,4))

  basedir  = sim.obsdir
  modeldir = sim.modeldir+sim.name+'/'
                                    
  ; number of stations
  nst      = n_elements(stats)
  nmonths  = 12
  mon      = ['01','02','03','04','05','06','07','08','09','10','11','12']
  nyears   = long(to-from+1)
  nall     = nyears*nmonths*31L
  syear    = STRCOMPRESS(string(from+indgen(nyears)),/REM)
  ndata    = IntArr(nmonths,nyears)

  sigma_meas  = FltArr(nst)+!values.f_nan
  sigma_trans = FltArr(nst,nyears)+!values.f_nan
  sigma_freq  = FltArr(nst,nyears)+!values.f_nan
  sigma_stan  = FltArr(nst,nyears)+!values.f_nan  
  sigma_all   = FltArr(nst,nyears)+!values.f_nan
  sigma_mean  = FltArr(nst)+!VALUES.f_nan
  sigmasquare = FltArr(nst)+!VALUES.f_nan  
  bias        = FltArr(nst,nyears)+!values.f_nan
  bias_mean   = FltArr(nst)+!VALUES.F_NAN
  mismatch    = FltArr(nst,nyears)+!values.f_nan
  
  ;***************************************
  ;* MAIN LOOP: YEARS
  ;***************************************
  FOR ij=0,nyears-1 DO BEGIN

    print, 'process year ', syear[ij]
    
    ;********************************
    ;* 1. read in observational data 
    ;********************************   
    read_data,sim=sim,dir=basedir,syear[ij],stats=stats,ch4obs=ch4obs,dtgobs=dtgobs,lonobs=lonobs,latobs=latobs,$
              nameobs=nameobs,numobs=numobs,type=type,nobs=nobs,flask=flask,nobg=nobg,special=special

    ;********************************
    ;* read in model data from stats
    ;********************************
    read_model_data,sim=sim,dir=modeldir,syear[ij],stats=stats,odtg=dtgobs,type=type,$
                    mch4=mch4,nmod=nmod
                                                                                                      
    FOR i=0,nst-1 DO BEGIN
        
      IF nobs[i] gt 0L THEN BEGIN
      
        model = DblArr(nmod[i]) & obs = DblArr(nobs[i])
        model[*] = mch4[0:nmod[i]-1,i]
        obs[*]   = ch4obs[0:nobs[i]-1,i]      
      
        diff    = FltArr(nmod[i])
        diff = model-obs
        IF nobs[i] ne nmod[i] THEN stop
        bias[i,ij] = median(diff)
        ind        = WHERE(finite(model) eq 1,cf)
        IF cf gt 0L THEN modcorr = model[ind]-bias[i,ij]
        mismatch[i,ij] = 1./(float(cf)-1.)*total((obs[ind]-modcorr)^2)

        ;************************************************************************
        ;* 2. sampling frequency error: use station data for hourly measurements,
        ;*    and model data for event data
        ;************************************************************************
        sigma_all[i,ij] = sqrt(mismatch[i,ij])*ufact[i]
      ENDIF
    ENDFOR ; end loop over years
    
 ;   stop ;flo

    FOR i=0,nst-1 DO sigma_mean[i]  = mean(sigma_all[i,*],/nan)
    FOR i=0,nst-1 DO sigmasquare[i] = mean(sigma_all[i,*]^2,/nan)
    FOR i=0,nst-1 DO bias_mean[i]  = mean(bias[i,*],/NAN)
    
  ENDFOR   ; end loop over stations

  FOR i=0,nst-1 DO print, stats[i],' ',sigma_mean[i]
  print
  print, 'Mean error covariance over all stations (preliminary): ', mean(sigmasquare) 
  
  IF keyword_set(flask) THEN BEGIN
    filemean = '/nas/arf/INVERSION/FINAL/inv_errorcovariance_stations_flask_'+sim.name+'_'+sn+'_'+'mean.dat'  
    IF keyword_set(nobg) THEN $    
    filemean = '/nas/arf/INVERSION/FINAL/inv_errorcovariance_stations_flask_nobg_'+sim.name+'_'+sn+'_'+'mean.dat'
  ENDIF ELSE BEGIN
    filemean = '/nas/arf/INVERSION/FINAL/inv_errorcovariance_stations_'+sim.name+'_'+sn+'_'+'mean.dat'  
    IF keyword_set(nobg) THEN $
    filemean = '/nas/arf/INVERSION/FINAL/inv_errorcovariance_stations_nobg_'+sim.name+'_'+sn+'_'+'mean.dat'
    IF keyword_set(special) THEN $
    filemean = '/nas/arf/INVERSION/FINAL/inv_errorcovariance_stations_special_'+sim.name+'_'+sn+'_'+'mean.dat'    
  ENDELSE
  openw,lun,filemean,/get_lun
  FOR i=0,nst-1 DO printf,lun,stats[i],sigma_mean[i],bias_mean[i],format='(a3,1x,f9.3,1x,f9.3)'
  free_lun,lun

  ;************************************************************
  ;* write out data for every month in the order that the 
  ;* data came in
  ;************************************************************
  FOR ij=0,nyears-1 DO BEGIN
    FOR im=0,nmonths-1 DO BEGIN
    
      IF im le 8 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
      yyyymm = syear[ij]+mm

      weekly = 1
    
      read_data_single_final,sim=sim,yyyymm=yyyymm,ch4obs=ch4,dtgobs=dtgobs,$
                       lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,weekly=weekly,$
                       stats=stats,flask=flask,nobg=nobg,special=special           
     
       IF syear[ij] eq '2006' and im eq 1 THEN BEGIN
         ind = WHERE(dtg2hiptime(dtgobs[0:nobs-1]) ge dtg2hiptime('200602030000'),c)
         dtgc     = StrArr(c)
         namec    = StrArr(c)
         dtgc[*]  = dtgobs[ind]
         namec[*] = nameobs[ind]
         nc       = c
       ENDIF ELSE BEGIN
         namec    = StrArr(nobs)
         namec[*] = nameobs[0:nobs-1]
         nc       = nobs
       ENDELSE

       IF keyword_set(flask) THEN BEGIN
         IF keyword_set(nobg) THEN $
         monfile = '/nas/arf/INVERSION/FINAL/'+$
         'inv_errorcovariance_stations_wm_mismatchonly_flask_nobg_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat' $               
         ELSE $
         monfile = '/nas/arf/INVERSION/FINAL/'+$
         'inv_errorcovariance_stations_wm_mismatchonly_flask_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'       
       ENDIF ELSE BEGIN
         monfile = '/nas/arf/INVERSION/FINAL/'+$
                   'inv_errorcovariance_stations_wm_mismatchonly_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'       
         IF keyword_set(nobg) THEN $
         monfile = '/nas/arf/INVERSION/FINAL/'+$
                 'inv_errorcovariance_stations_wm_mismatchonly_nobg_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'
         IF keyword_set(special) THEN $
         monfile = '/nas/arf/INVERSION/FINAL/'+$
                 'inv_errorcovariance_stations_wm_mismatchonly_special_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'                                   
       ENDELSE
       openw,lun,monfile,/get_lun
       printf,lun,'STN    SIG_ALL'
       FOR i=0,nc-1 DO BEGIN
         ind = WHERE(namec[i] eq stats,c1)
         IF c1 eq 1 THEN printf,lun,namec[i],sigma_all[ind,ij],format='(a3,1x,f9.3)' ELSE stop

       ENDFOR
       free_lun,lun
    ENDFOR
  ENDFOR

END

