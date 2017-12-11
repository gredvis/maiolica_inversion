;+
; NAME:
;
;   inv_error_diagonal_weekly_aposteriori_brd
;
; PURPOSE:
;
;   Read weekly observational data (flask and continuous) and weekly mean a posteriori 
;   model data for the number of years of inversion. Calculate monthly diagonal error 
;   covariance matrix, sum of measurement and transport errors.
;
;
; CATEGORY:
;
;   inverse modelling MAIOLICA2 run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;   inv_error_diagonal_weekly_aposteriori_brd,sim
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
;       weekly station data in predefined order
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
;
;     Read all available data from continuous/flask stations and model
;     data per month. Output  weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 22 September 2011
; DB, 11 Feb 2017: revised version of Florian's 
;                  inv_error_diagonal_weekly_aposteriori_maiolica2
;-
;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_error_diagonal_weekly_aposteriori_brd,sim

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  sn      = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'
  
  from = 1990 ;fix(STRMID(sim.syyyymm,0,4))
  to   = fix(STRMID(sim.eyyyymm,0,4))

  basedir  = sim.obsdir
  modeldir = sim.modeldir+sim.name+'/'
                                                     
  ; number of stations
  nst      = n_elements(stats)
  nmonths  = 12
  mon      = STRING(indgen(12)+1,format='(i2.2)')
  nyears   = long(to-from+1)
  nall     = nyears*nmonths*31L
  syear    = STRCOMPRESS(string(from+indgen(nyears)),/REM)
  ndata    = IntArr(nmonths,nyears)
 
  syyyy    = 1990 & smm = 1
  ;syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1 
  m        = eyyyy*12L+emm-(1989*12L+2)+1    

  sigma        = FltArr(nst,nyears)+!values.f_nan
  sigma_mean   = FltArr(nst)+!VALUES.f_nan
  sigmasquare  = FltArr(nst)+!VALUES.f_nan  
  bias         = FltArr(nst,nyears)+!values.f_nan
  mismatch     = FltArr(nst,nyears)+!values.f_nan

  zstat_coll   = DblArr(52,nst)+!VALUES.D_NAN ; observational data at selected stations
  zmpstat_coll = DblArr(52,nst)+!VALUES.D_NAN ; modelled aposteriori data at selected stations

  ;;*******************************************************************
  ;; read in output from first preliminary inversion simulation
  ;; including the correction factors and the prior and posterior
  ;; emissions. Together with the a priori simulated values at the
  ;; stations, this can be used to calculate the posterior simulated
  ;; station concentrations 
  ;;*******************************************************************
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  IF keyword_set(sim.flask) THEN BEGIN
     testfile = sim.outdir+'inv_output_weekly_flask_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                sim.eyyyymm+'_'+qunc+'.txt'
  ENDIF ELSE BEGIN
     testfile = sim.outdir+'inv_output_weekly_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                sim.eyyyymm+'_'+qunc+'.txt'  
     IF keyword_set(sim.special) THEN BEGIN
        testfile = sim.outdir+'inv_output_weekly_prelim_special_'+sn+'_'+sim.name+'_'+$
                   sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'.txt'
        IF keyword_set(sim.startcf) THEN $
           testfile = sim.outdir+'inv_output_weekly_prelim_special_startcf'+sstart+'_'+sn+'_'+$
                      sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'.txt'      
     ENDIF               
  ENDELSE  
  
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

  ;; only evaluate from January 1990 onward. 1989 is spin up year of inversion.
  FOR it=0,sim.ntrace-1 DO BEGIN
     fcorr[*,it] = fhelp[11:m-1,it]
     sa[*,it] = sahelp[11:m-1,it]
     sp[*,it] = sphelp[11:m-1,it]
  ENDFOR  

  ;***************************************
  ;* MAIN LOOP: YEARS
  ;***************************************

  FOR ij=0,nyears-1 DO BEGIN
    
     icorr = 0 & em = IntArr(nst) & am = IntArr(nst)

     FOR im=0,nmonths-1 DO BEGIN
        
        yyyymm = syear[ij]+mon[im]

        print, 'process ', yyyymm
        
        ;;********************************
        ;;* 1. read in observational data 
        ;;********************************
        read_processed_obs_data_month,sim,syear[ij],ch4obs=ch4obs

        Z = ch4obs.ch4
    
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
          zj[*]     = double(Z[ind])      
          statsc[*] = nameobs[ind]
          dtgc[*]   = dtgobs[ind]
          nc        = c
        ENDIF
      ENDIF ELSE BEGIN
        dtgc     = StrArr(nobs)
        namec    = StrArr(nobs)
        zj       = Z
        dtgc[*]  = dtgobs[0:nobs-1]
        namec[*] = nameobs[0:nobs-1]
        nc       = nobs
      ENDELSE

      ;****************************************************************
      ; read sensitivity matrix of year and month. Unit: ppb/(kg/month)
      ;****************************************************************            
      IF keyword_set(sim.flask) THEN BEGIN
         sensfile = sim.hdir+'inv_sensitivity_weekly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.txt'
      ENDIF ELSE BEGIN
         sensfile = sim.hdir+'inv_sensitivity_weekly_'+sn+'_'+sim.name+'_'+yyyymm+'.txt'      
         IF keyword_set(sim.nobg)    THEN sensfile = sim.hdir+'inv_sensitivity_weekly_nobg_'+$
            sn+'_'+sim.name+'_'+yyyymm+'.txt'
         IF keyword_set(sim.special) THEN sensfile = sim.hdir+'inv_sensitivity_weekly_special_'+$
            sn+'_'+sim.name+'_'+yyyymm+'.txt'     
      ENDELSE      
      nch = 0 & nospt = 0 & dummy = 0
      openr,lun,sensfile,/get_lun  
      readf,lun,dummy
      readf,lun,nch
      sensin = DblArr(4*sim.ntrace,nch)
      readf,lun,sensin
      free_lun,lun

      xp = DblArr(sim.ntrace*4) 
      H = sensin
      xp[0:sim.ntrace-1]               = sp[icorr,*]
      xp[sim.ntrace:2*sim.ntrace-1]    = sp[icorr-1,*]
      xp[2*sim.ntrace:3*sim.ntrace-1]  = sp[icorr-2,*]  
      xp[3*sim.ntrace:4*sim.ntrace-1]  = sp[icorr-3,*] 
      
      ;; read weekly model data in correct temporal order
      IF keyword_set(sim.flask) THEN BEGIN
         modfile = '/nas/arf/INVERSION/MAIOLICA2/m_allweekly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.dat' 
      ENDIF ELSE BEGIN
         modfile = '/nas/arf/INVERSION/MAIOLICA2/m_allweekly_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'      
         IF keyword_set(sim.special) THEN $
            modfile = '/nas/arf/INVERSION/MAIOLICA2/m_allweekly_special_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'                       
      ENDELSE          
            
      mch4    = DblArr(sim.ntrace*sim.nage,nc)
      mall    = DblArr(nc)          
      help = DblArr(sim.ntrace*sim.nage)      
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
      
      BG  = DblArr(nc,sim.ntrace) & zmp = DblArr(nc)
      FOR it=0,sim.ntrace-1 DO BEGIN
         FOR i=0,nc-1 DO BG[i,it] = fcorr[icorr,it]*mch4[it+(sim.nage-1)*sim.ntrace,i]
      ENDFOR


      BG2=DblArr(nc)
      FOR i=0,nc-1 DO BG2[i] = total(BG[i,*])  
      zmp = reform(H ## xp + BG2)
      
      zma = mall
    
      FOR i=0,nst-1 DO BEGIN
         ind = WHERE(stats[i] eq namec,ck)
         IF ck gt 0L THEN BEGIN
            em[i] += ck
            zstat_coll[am[i]:em[i]-1,i]   = zj[ind]
            zmpstat_coll[am[i]:em[i]-1,i] = zmp[ind]
            am[i] += ck
         ENDIF
      ENDFOR                    ; end loop over stations
      icorr += 1
      
      ;; get next month by adding 40 days and rounding off to full months
      yyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')+40),0,6)
        
   ENDFOR                       ; end loop over months
                            
     FOR i=0,nst-1 DO BEGIN
        
        IF em[i] gt 0L THEN BEGIN
           
           model = DblArr(em[i]) & obs = DblArr(em[i])
           model[*] = zmpstat_coll[0:em[i]-1,i]
           obs[*]   = zstat_coll[0:em[i]-1,i]
           
           diff = FltArr(em[i])
           diff = model-obs
           bias[i,ij] = median(diff)
           ind        = WHERE(finite(model) eq 1,cf)
           IF cf gt 0L THEN modcorr = model[ind]-bias[i,ij]
           mismatch[i,ij] = 1./(float(cf)-1.)*total((obs[ind]-modcorr)^2)
           sigma[i,ij] = sqrt(mismatch[i,ij])        
        ENDIF
     ENDFOR                     ; end loop over stations
     
  ENDFOR                        ; end loop over years    
  
  FOR i=0,nst-1 DO sigma_mean[i]  = mean(sigma[i,*],/nan)
  FOR i=0,nst-1 DO sigmasquare[i] = mean(sigma[i,*]^2,/nan)            
  FOR i=0,nst-1 DO print, stats[i],' ',sigma_mean[i]  
  print
  
  print, 'Mean error covariance over all stations (final): ', mean(sigmasquare)   
  IF keyword_set(flask) THEN BEGIN
     filemean = '/nas/arf/INVERSION/MAIOLICA2/inv_errorcovariance_stations_allweekly_flask_'+sn+'_'+'mean.dat' 
  ENDIF ELSE BEGIN
     filemean = '/nas/arf/INVERSION/MAIOLICA2/inv_errorcovariance_stations_allweekly_'+sn+'_'+'mean.dat'  
     IF keyword_set(special) THEN $
        filemean = '/nas/arf/INVERSION/MAIOLICA2/inv_errorcovariance_stations_allweekly_special_'+sn+'_'+'mean.dat'         
  ENDELSE
  openw,lun,filemean,/get_lun
  FOR i=0,nst-1 DO printf,lun,stats[i],sigma_mean[i],format='(a3,1x,f9.3)'
  free_lun,lun
  
  ;;************************************************************
  ;;* write out data for every month in the order that the 
  ;;* data came in
  ;;************************************************************
  
  yyyymm = '198901' 
  dirout = '/nas/arf/INVERSION/MAIOLICA2/'
  FOR ij=0,nyears DO BEGIN
     IF ij eq 0 THEN jahr = 0 ELSE jahr = ij-1
     FOR im=0,nmonths-1 DO BEGIN
        weekly = 1
        read_data_single_maiolica2,sim=sim,yyyymm=yyyymm,ch4obs=ch4obs,dtgobs=dtgobs,$
                                   lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,weekly=weekly,$
                                   stats=stats,flask=flask,nobg=nobg,special=special
        
        yyyy = STRMID(yyyymm,0,4)             
        IF yyyy eq '2006' and im eq 1 THEN BEGIN
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
           monfile = dirout+'inv_errorcovariance_wm_mismatchonly_aposteriori_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'        
        ENDIF ELSE BEGIN
           monfile = dirout+'inv_errorcovariance_wm_mismatchonly_aposteriori_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'       
           IF keyword_set(special) THEN $
              monfile = dirout+'inv_errorcovariance_wm_mismatchonly_aposteriori_special_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'                           
        ENDELSE
        openw,lun,monfile,/get_lun
        printf,lun,'STN    SIG_ALL'
        FOR i=0,nc-1 DO BEGIN
           ind = WHERE(namec[i] eq stats,c1)
           IF c1 eq 1 THEN $
              printf,lun,namec[i],sigma[ind,jahr],format='(a3,1x,f9.3)' ELSE stop
        ENDFOR
        free_lun,lun
        
        ;; get next month by adding 40 days and rounding off to full months
        yyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')+40),0,6)    
        
     ENDFOR
     
  ENDFOR

END

