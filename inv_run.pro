;+
; NAME:
;
;   inv_run
;
; PURPOSE:
;
;   Program that carries out the time stepping of the inversion 
;   carried out by inv_kalman_step.pro
;
;   An inversion is started in February of the start year of the
;   inversion. This is because the inversion needs information
;   on the global model bias in the month previous to the first
;   inversion month and to guarantee best usage of all available
;   model data and to account for some spinup time of the inversion.
;   
; CATEGORY:
;
;   Inverse modelling MAIOLICA2 run.
;
; CALLING SEQUENCE:
;
;  inv_run,sim,Hdump=Hdump,prelim=prelim,result=result
;
;
; INPUTS:
;  sim   (structure) : the model simulation information structure
;
;
; KEYWORD PARAMETERS:
;
;  /hdump            : set this keyword to write out receptor point
;                      values (obs, prior, posterior, and background)
;  /prelim           : set this keyword for first preliminary inversion in which 
;                      observation uncertainty is estimated from differences
;                      between obs and a priori simulation. If not set, the 
;                      observation uncertainty is estimated from differences
;                      between obs and a posteriori of prelim inversion
;
; OUTPUTS:
;
;  result            : structure with all inversion outputs, i.e.
;                      monthly state vectors (emission), their error
;                      covariance matrices, oldest age class correction factors
;                      and chi square and log-likelihood statistics
;                      {emis_apri:FltArr(nmonth,ntrace),emis_apost:FltArr(nmonth,ntrace),$
;                       emis_apost1:FltArr(nmonth,ntrace),emis_apost2:FltArr(nmonth,ntrace),$
;                       emis_apost3:FltArr(nmonth,ntrace),$
;                       cov_apost:FltArr(nmonth,ntrace,ntrace),$
;                       cov_apost1:FltArr(nmonth,ntrace,ntrace),$
;                       cov_apost2:FltArr(nmonth,ntrace,ntrace),$
;                       cov_apost3:FltArr(nmonth,ntrace,ntrace),$
;                       scalef:FltArr(nmonth,ntrace),$
;                       zlen:FltArr(nmonth),dllh:FltArr(nmonth),nobs:FltArr(nmonth)}  
;
; COMMON BLOCKS:
;        none
;
; PROCEDURE:
;  NAME:
;      dump_inv_results_netcdf,sim,result,prelim=prelim
;  PURPOSE:
;      Write out content of the result output structure to a netcdf file including
;      some meta data on the simulation
;  KEYWORD PARAMETERS:
;      /prelim : set this keyword if output is from first preliminary simulation
;    
; SIDE EFFECTS:
;        none
;
; RESTRICTIONS:
;        none
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
; CSP 29 September 2011
; BRD 10 March 2012, 2 indices for year and month replaced by one index,
;                    and variable sim changed to a structure containing all
;                    details about the inversion
; FA 2015
; BRD 19 Nov 2017, call adjusted/simplified for new structure of sim
; BRD 05 Jan 2018, output to ASCII replaced by output to netcdf
;-
PRO dump_inv_results_netcdf,sim,result,prelim=prelim

  ;; metadata for global file attributes
  IF NOT keyword_set(sim.dlr) THEN BEGIN
     model = 'FLEXPART-CTM' 
     institute = 'EMPA'
     modeller = 'Stephan Henne'
  ENDIF ELSE BEGIN
     model = 'EMAC'
     institute = 'DLR'
     modeller = 'Franziska Frank'
  ENDELSE

  metadata = $
     create_struct( "description" , "Kalman smoother inversion of global CH4 emissions", $
                    "inversion_name" , sim.name, $
                    "station_config", sim.sconfig,$
                    "num_stats",sim.sn,$
                    "tot_apri_uncert",sim.qunc,$
                    "SYYYYMM",sim.syyyymm,$
                    "EYYYYMM",sim.eyyyymm,$
                    "ntrace",sim.ntrace,$
                    "nage",sim.nage,$
                    "keeppos",sim.keeppos,$
                    "flask",sim.flask,$
                    "filter",sim.filter,$
                    "model", model, $
                    "is_dlr", sim.dlr,$
                    "dlr_scale",sim.dlrscale,$
                    "modeller", modeller, $
                    "institute", institute, $
                    "references"  , " ", $
                    "creator"     , "Dominik Brunner", $
                    "email"       , "dominik.brunner@empa.ch", $
                    "affiliation" , "Empa Duebendorf, Switzerland", $
                    "version"     , '1.0', $
                    "date"        ,  systime(/UTC), $
                    "study"       , "MAIOLICA-2" )


  ;; total number of data
  ntime = n_elements(result.zlen)
  ntrace = sim.ntrace*sim.nage

  ;;**************************************************************************
  ;; write to netcdf file
  ;;**************************************************************************
  sstr =sim_filename_str(sim,prelim=prelim)
  ncfile = sim.outdir + 'inv_result_'+sstr+sim.syyyymm+'-'+sim.eyyyymm+'.nc'
  
  IF file_test(ncfile) THEN BEGIN
     print,'replacing file ',ncfile
     file_delete, ncfile,/quiet
  ENDIF ELSE print,'writing file ',ncfile

  ncid = NCDF_CREATE(ncfile,/CLOBBER,/NETCDF4_FORMAT)
  NCDF_CONTROL, ncid, /FILL

  ;;******************************************
  ;; global attributes
  ;;******************************************
  metatags = tag_names(metadata)
  FOR i = 0, n_elements(metatags)-1 DO BEGIN
     NCDF_ATTPUT, ncid, metatags[i], metadata.(i), /GLOBAL
  ENDFOR

  ;;******************************************
  ;; create dimensions
  ;;******************************************

  tid = NCDF_DIMDEF(ncid,'time',/UNLIMITED)
  tracid  = NCDF_DIMDEF(ncid,'tracer', sim.ntrace)
  len2id = NCDF_DIMDEF(ncid,'len2', 2)
  len4id = NCDF_DIMDEF(ncid,'len4', 4)
  len13id = NCDF_DIMDEF(ncid,'len13', 13)
  
  ;;******************************************
  ;; create variables
  ;;******************************************
  yearid = NCDF_VARDEF(ncid,"year", [len4id,tid],/CHAR)
  monthid = NCDF_VARDEF(ncid,"month", [len2id,tid],/CHAR)
  catid  = NCDF_VARDEF(ncid,'source_cat', [len13id,tracid], /CHAR)

  units = 'kg/day'
  missing_value = !values.f_nan

  eapriid = NCDF_VARDEF(ncid,'emis_apri',[tid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, eapriid, 'long_name', 'apriori emissions'
  NCDF_ATTPUT, ncid, eapriid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, eapriid, 'units',units

  eapostid = NCDF_VARDEF(ncid,'emis_apost',[tid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, eapostid, 'long_name', 'final aposteriori emissions'
  NCDF_ATTPUT, ncid, eapostid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, eapostid, 'units',units

  eapost1id = NCDF_VARDEF(ncid,'emis_apost1',[tid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, eapost1id, 'long_name', 'first aposteriori emissions'
  NCDF_ATTPUT, ncid, eapost1id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, eapost1id, 'units',units

  eapost2id = NCDF_VARDEF(ncid,'emis_apost2',[tid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, eapost2id, 'long_name', 'second aposteriori emissions'
  NCDF_ATTPUT, ncid, eapost2id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, eapost2id, 'units',units

  eapost3id = NCDF_VARDEF(ncid,'emis_apost3',[tid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, eapost3id, 'long_name', 'third aposteriori emissions'
  NCDF_ATTPUT, ncid, eapost3id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, eapost3id, 'units',units

  ecovid = NCDF_VARDEF(ncid,'emis_cov',[tid,tracid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, ecovid, 'long_name', 'final aposteriori emission_error covariances'
  NCDF_ATTPUT, ncid, ecovid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, ecovid, 'units','1'

  ecov1id = NCDF_VARDEF(ncid,'emis_cov1',[tid,tracid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, ecov1id, 'long_name', 'first aposteriori emission error covariances'
  NCDF_ATTPUT, ncid, ecov1id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, ecov1id, 'units','1'

  ecov2id = NCDF_VARDEF(ncid,'emis_cov2',[tid,tracid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, ecov2id, 'long_name', 'second aposteriori emission error covariances'
  NCDF_ATTPUT, ncid, ecov2id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, ecov2id, 'units','1'

  ecov3id = NCDF_VARDEF(ncid,'emis_cov3',[tid,tracid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, ecov3id, 'long_name', 'third aposteriori emission error covariances'
  NCDF_ATTPUT, ncid, ecov3id, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, ecov3id, 'units','1'

  scaleid = NCDF_VARDEF(ncid,'scalef',[tid,tracid],/FLOAT)
  NCDF_ATTPUT, ncid, scaleid, 'long_name', 'scaling factors for oldest age class'
  NCDF_ATTPUT, ncid, scaleid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, scaleid, 'units','1'

  zlenid = NCDF_VARDEF(ncid,'zlen',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, zlenid, 'long_name', 'normalized chi square statistics'
  NCDF_ATTPUT, ncid, zlenid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, zlenid, 'units','1'

  dllhid = NCDF_VARDEF(ncid,'dllh',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, dllhid, 'long_name', 'log-likelihood'
  NCDF_ATTPUT, ncid, dllhid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, dllhid, 'units','1'

  nobsid = NCDF_VARDEF(ncid,'nobs',[tid],/FLOAT)
  NCDF_ATTPUT, ncid, nobsid, 'long_name', 'number of assimilated observations'
  NCDF_ATTPUT, ncid, nobsid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, nobsid, 'units','1'

  NCDF_CONTROL, ncid, /ENDEF
  
  ;;******************************************
  ;; fill with data
  ;;******************************************
  yyyy = STRMID(result.yyyymm,0,4) & mm = STRMID(result.yyyymm,4,2)
  NCDF_VARPUT, ncid, yearid, yyyy
  NCDF_VARPUT, ncid, monthid, mm
  cats = emiss_categories()
  NCDF_VARPUT, ncid, catid, STRING(cats,format='(a13)')

  NCDF_VARPUT, ncid, eapriid, result.emis_apri
  NCDF_VARPUT, ncid, eapostid, result.emis_apost
  NCDF_VARPUT, ncid, eapost1id, result.emis_apost1
  NCDF_VARPUT, ncid, eapost2id, result.emis_apost2
  NCDF_VARPUT, ncid, eapost3id, result.emis_apost3

  NCDF_VARPUT, ncid, ecovid, result.cov_apost
  NCDF_VARPUT, ncid, ecov1id, result.cov_apost1
  NCDF_VARPUT, ncid, ecov2id, result.cov_apost2
  NCDF_VARPUT, ncid, ecov3id, result.cov_apost3

  NCDF_VARPUT, ncid, scaleid, result.scalef
  NCDF_VARPUT, ncid, zlenid, result.zlen
  NCDF_VARPUT, ncid, dllhid, result.dllh
  NCDF_VARPUT, ncid, nobsid, result.nobs

  NCDF_CLOSE, ncid

END


;*************************** main **********************************

PRO inv_run,sim,Hdump=Hdump,prelim=prelim,result=result

  sstart = STRCOMPRESS(string(sim.startcf),/REM)
  print, 'start scaling factors = ', sstart

  nopt = sim.nage-1             ; number of times an emission is estimated,
                                ; one less than nage beccause oldest age is background 
  
  ;; calculate number of months to simulate
  syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1
  ;yyyysave = IntArr(n)
  nyears   = eyyyy-syyyy+1
  year     = syyyy+indgen(nyears)

  ;; create arrays
  fprev     = sim.startcf
  fsave     = DblArr(n,sim.ntrace)
  Qpsave    = DblArr(n,sim.ntrace,sim.ntrace) ; final a posteriori uncertainties
  Qp1       = DblArr(n,sim.ntrace,sim.ntrace) ; 1st a posteriori uncertainties
  Qp2       = DblArr(n,sim.ntrace,sim.ntrace) ; 2nd a posteriori uncertainties    
  Qp3       = DblArr(n,sim.ntrace,sim.ntrace) ; 3rd a posteriori uncertainties    
  Spsave    = DblArr(n,sim.ntrace)            ; final a posteriori emissions
  SP1       = DblArr(n,sim.ntrace)            ; 1st a posteriori estimate
  SP2       = DblArr(n,sim.ntrace)            ; 2nd a posteriori estimate
  SP3       = DblArr(n,sim.ntrace)            ; 3rd a posteriori estimate
  Sasave    = DblArr(n,sim.ntrace)            ; a priori emissions
  yyyymm    = StrArr(n)                       ; month and year
  serzlen   = DblArr(n)                       ; time series chi-square statistics
  sernobse  = DblArr(n)                       ; time series nobs
  serdllh   = DblArr(n)                       ; time series log likelihood statistics
  em3_apost = DblArr(sim.ntrace)              ; posterior emissions month i-3

  irun     = 0

  print, 'run inversion with uncertainty parameters for all tracers: '
  cats = emiss_categories(labels=labels)
  print,string(labels,format='(a4)')+': '+string(sim.scaleq,format='(f6.1)')

  ;; loop over all months of the inversion period
  cyyyymm = sim.syyyymm
  FOR i=0,n-1 DO BEGIN

     ;cyyyy       = fix(STRMID(cyyyymm,0,4)) & cmm = fix(STRMID(cyyyymm,4,2))
     ;yyyysave[i] = cyyyy
     yyyymm[i] = cyyyymm
     
     print, 'Analysing year ', STRMID(cyyyymm,0,4), ', month ', STRMID(cyyyymm,4,2)
     
     ;; save final a posteriori emissions for month i-3
     IF i GE nopt THEN BEGIN
        IF keyword_set(sim.keeppos) THEN em3_apost[*] = exp(Spsave[i-nopt,*]) ELSE $
           em3_apost[*] = Spsave[i-nopt,*]
        ind = WHERE(finite(em3_apost) eq 0,c)
        IF c gt 0L THEN stop
     ENDIF

     ;;***************************************************
     ;; call inv_kalman_step for current year and month
     ;;***************************************************  
     inv_kalman_step,sim,cyyyymm,fprev=fprev,fcorr=fcorr,$
                     em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
                     Spout=Spout,Qpout=Qpout,Saout=Saout,$
                     zlen=zlen,dllh=dllh,prelim=prelim,nobse=nobse

     serzlen[i] = zlen
     serdllh[i] = dllh
     sernobse[i] = nobse

     ;; store current state and error covariance  
     fsave[i,*] = fcorr

     IF i GE (nopt-1) THEN BEGIN
        ;; start in month nopt to take final estimate that has been calculated nopt times
        ;; and is now representative of month i-(nopt-1)
        QP1[i-(nopt-4),*,*]    = Qpout[(nopt-4)*sim.ntrace:(nopt-3)*sim.ntrace-1,$
                                       (nopt-4)*sim.ntrace:(nopt-3)*sim.ntrace-1]
        QP2[i-(nopt-3),*,*]    = Qpout[(nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1,$
                                       (nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1]
        QP3[i-(nopt-2),*,*]    = Qpout[(nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1,$
                                       (nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1]
        Qpsave[i-(nopt-1),*,*] = Qpout[(nopt-1)*sim.ntrace:nopt*sim.ntrace-1,$
                                       (nopt-1)*sim.ntrace:nopt*sim.ntrace-1]
        SP1[i-(nopt-4),*]      = Spout[(nopt-4)*sim.ntrace:(nopt-3)*sim.ntrace-1]
        SP2[i-(nopt-3),*]      = Spout[(nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1]         
        SP3[i-(nopt-2),*]      = Spout[(nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1]         
        Spsave[i-(nopt-1),*]   = Spout[(nopt-1)*sim.ntrace:nopt*sim.ntrace-1]
     ENDIF 

     ;; Estimates for the last few months are only available from 
     ;; less than nopt optimizations
     IF i EQ n-1 THEN BEGIN
        Spsave[i-2,*]   = Spout[(nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1]
        Qpsave[i-2,*,*] = Qpout[(nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1,$
                                (nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1]
        Spsave[i-1,*]   = Spout[(nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1]
        Qpsave[i-1,*,*] = Qpout[(nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1,$
                                (nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1]
        Spsave[i,*]     = Spout[(nopt-4)*sim.ntrace:(nopt-3)*sim.ntrace-1]
        Qpsave[i,*,*]   = Qpout[(nopt-4)*sim.ntrace:(nopt-3)*sim.ntrace-1,$
                                (nopt-4)*sim.ntrace:(nopt-3)*sim.ntrace-1]
     ENDIF
     
     Sasave[i,*] = saout[0:sim.ntrace-1]
     
     ;; get next month by adding 40 days and rounding off to full months
     cyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)
     
     ;; variables as input for next inversion step
     fprev = fcorr
     Spin  = Spout
     Qpin  = Qpout

  ENDFOR
  
  IF keyword_set(sim.keeppos) THEN BEGIN
     result = {yyyymm:yyyymm,emis_apri:Sasave,emis_apost:exp(Spsave),$
               emis_apost1:exp(SP1),emis_apost2:exp(SP2),emis_apost3:exp(SP3),$
               cov_apost:Qpsave,cov_apost1:Qp1,cov_apost2:QP2,cov_apost3:QP3,$
               scalef:fsave,zlen:serzlen,dllh:serdllh,nobs:sernobse}   
  ENDIF ELSE BEGIN
     result = {yyyymm:yyyymm,emis_apri:Sasave,emis_apost:Spsave,$
               emis_apost1:SP1,emis_apost2:SP2,emis_apost3:SP3,$
               cov_apost:Qpsave,cov_apost1:Qp1,cov_apost2:QP2,cov_apost3:QP3,$
               scalef:fsave,zlen:serzlen,dllh:serdllh,nobs:sernobse}   
  ENDELSE 

  ;; write output to netcdf file
  dump_inv_results_netcdf,sim,result,prelim=prelim
  
  print, 'End of program: inversion completed.'

END
