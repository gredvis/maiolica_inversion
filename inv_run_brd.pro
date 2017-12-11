;+
; NAME:
;
;   inv_run_brd
;
; PURPOSE:
;
;   Program that carries out the time stepping of the inversion 
;   carried out by inv_determine_aposteriori.pro
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
;  inv_run_brd,sim,Hdump=Hdump,serdllh=serdllh,sumdllh=sumdllh,$
;            serzlen=serzlen,sumzlen=sumzlen,rapriori=rapriori,$
;            sernobse=sernobse
;
;
; INPUTS:
;     sim      (structure) : the model simulation information structure
;
;
; KEYWORD PARAMETERS:
;
;       /hdump             : set this keyword to write out receptor point
;                            values (obs, prior, posterior, and background)
;       /rapriori          : set this keyword for first preliminary inversion 
;                            in which observation uncertainty is estimated from
;                            differences between obs and a priori simulation
;
; OUTPUTS:
;
;       fsave (float)      : background correction factor to be used
;                            as fprev in next simulation month
;       Sp (FltArr, ntrace): aposteriori monthly emissions estimates for the month of 
;                            simulation
;       serdllh (Fltarr)   : time series log-likelihood
;       sumdllh            : accumulated log-likelihood
;       serzlen (Fltarr)   : time series chi-square statistics
;       sumzlen            : accumulated chi-square statistics
;       sernobse           : time series of number of observations per month
;             
; COMMON BLOCKS:
;        none
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
;-
PRO inv_run_brd,sim,Hdump=Hdump,serdllh=serdllh,sumdllh=sumdllh,$
                serzlen=serzlen,sumzlen=sumzlen,rapriori=rapriori,$
                sernobse=sernobse,dlr=dlr

  sn = STRCOMPRESS(string(fix(n_elements(sim.stats))),/REM)+'stats'
  sstart = STRCOMPRESS(string(sim.startcf),/REM)
  print, 'start scaling factors = ', sstart

  nopt = sim.nage-1             ; number of times an emission is estimated,
                                ; one less than nage beccause oldest age is background 
  
  ;; calculate number of months to simulate
  syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1
  yyyysave = IntArr(n)
  nyears   = eyyyy-syyyy+1
  year     = syyyy+indgen(nyears)

  ;; create arrays
  fprev     = 0.
  fsave     = DblArr(n,sim.ntrace)
  Qpsave    = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties
  Qp1       = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties
  Qp2       = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties    
  Qp3       = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties    
  Spsave    = DblArr(n,sim.ntrace)            ; a posteriori emissions
  SP1       = DblArr(n,sim.ntrace)            ; 1st a posteriori estimate
  SP2       = DblArr(n,sim.ntrace)            ; 2nd a posteriori estimate
  SP3       = DblArr(n,sim.ntrace)            ; 3rd a posteriori estimate
  Sasave    = DblArr(n,sim.ntrace)            ; a priori emissions
  yyyymm    = StrArr(n)                       ; month and year
  serzlen   = DblArr(n)                       ; time series chi-square statistics
  sernobse  = DblArr(n)                       ; time series nobs
  serdllh   = DblArr(n)                       ; time series log likelihood statistics
  sumzlen   = 0.D
  sumdllh   = 0.D
  em3_apost = DblArr(sim.ntrace)              ; posterior emissions month i-3

  irun     = 0

  print, 'run inversion with uncertainty parameters for all tracers: '
  print, sim.scaleq

  ;; loop over all months of the inversion period
  cyyyymm = sim.syyyymm
  FOR i=0,n-1 DO BEGIN

     cyyyy       = fix(STRMID(cyyyymm,0,4)) & cmm = fix(STRMID(cyyyymm,4,2))
     yyyysave[i] = cyyyy
     
     print, 'Analysing year ', cyyyy, ', month ', cmm
     
     ;; save final a posteriori emissions for month i-3
     IF i GE nopt THEN BEGIN
       IF keyword_set(sim.keeppos) THEN em3_apost[*] = exp(Spsave[i-nopt,*]) ELSE em3_apost[*] = Spsave[i-nopt,*]
       ind = WHERE(finite(em3_apost) eq 0,c)
       IF c gt 0L THEN stop
     ENDIF

     ;;***************************************************
     ;; call inv_determine_aposteriori for year and month
     ;;***************************************************  
     inv_determine_aposteriori_brd,sim,cyyyymm,fprev=fprev,fcorr=fcorr,$
                                   em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
                                   Spout=Spout,Qpout=Qpout,Saout=Saout,Hdump=Hdump,$
                                   zlen=zlen,dllh=dllh,$
                                   rapriori=rapriori,nobse=nobse,dlr=dlr

     serzlen[i] = zlen
     serdllh[i] = dllh
     sernobse[i] = nobse
     IF finite(zlen) eq 1 THEN sumzlen += zlen
     IF finite(dllh) eq 1 THEN sumdllh += dllh

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
  
  ;; calculate uncertainties of posterior estimates as sqrt of diagonal elements
  covar    = DblArr(n,sim.ntrace,sim.ntrace)
  mcovar   = DblArr(sim.ntrace,sim.ntrace)
  mcovaryr = DblArr(nyears,sim.ntrace,sim.ntrace)
  qpdiag = DblArr(n,sim.ntrace) ; diagonal elements
  q1diag = DblArr(n,sim.ntrace)
  q2diag = DblArr(n,sim.ntrace)
  q3diag = DblArr(n,sim.ntrace)
  uncert = DblArr(n)
  IF keyword_set(sim.keeppos) THEN BEGIN
     FOR i=0,n-1 DO BEGIN
        qpdiag[i,*]  = sqrt(diag_matrix(reform(qpsave[i,*,*])))
        FOR j=0,sim.ntrace-1 DO FOR k=0,sim.ntrace-1 DO covar[i,j,k] = $
           qpsave[i,j,k]*exp(Spsave[i,j])*exp(Spsave[i,k])/$
           ( sqrt(qpsave[i,j,j]*exp(Spsave[i,j])^2)*sqrt(qpsave[i,k,k]*exp(Spsave[i,k])^2) )  
        q1diag[i,*] = sqrt(diag_matrix(reform(Qp1[i,*,*])))
        q2diag[i,*] = sqrt(diag_matrix(reform(Qp2[i,*,*])))
        q3diag[i,*] = sqrt(diag_matrix(reform(Qp3[i,*,*])))
     ENDFOR
     FOR j=0,sim.ntrace-1 DO BEGIN
        FOR k=0,sim.ntrace-1 DO BEGIN
           mcovar[j,k] = mean(covar[10:n-1,j,k]) ; mean error covariance 1990-2012
           FOR ij=0,nyears-1 DO BEGIN
              ind = WHERE(year[ij] eq yyyysave,cn)
              IF cn gt 0L THEN mcovaryr[ij,j,k] = mean(covar[ind,j,k])
           ENDFOR
        ENDFOR
     ENDFOR
  ENDIF ELSE BEGIN
     FOR i=0,n-1 DO BEGIN
        qpdiag[i,*] = sqrt(diag_matrix(reform(qpsave[i,*,*])))
        q1diag[i,*] = sqrt(diag_matrix(reform(qp1[i,*,*])))
        q2diag[i,*] = sqrt(diag_matrix(reform(qp2[i,*,*])))
        q3diag[i,*] = sqrt(diag_matrix(reform(qp3[i,*,*])))      
     ENDFOR
  ENDELSE       
  
  ;; absolute total uncertainty
  FOR i=0,n-1 DO BEGIN
     tsp       = DblArr(sim.ntrace)
     tsp[*]    = exp(Spsave[i,*])
     tQ        = DblArr(sim.ntrace,sim.ntrace)
     tQ[*,*]   = reform(qpsave[i,*,*]) 
     step      = transpose(tsp) # tQ # tsp
     uncert[i] = sqrt(step)/1.e9
  ENDFOR

  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  
  IF keyword_set(rapriori) THEN BEGIN
     testfile = sim.outdir+'inv_output_weekly_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'.txt'          
     IF keyword_set(sim.flask) THEN $
        testfile = sim.outdir+'inv_output_weekly_flask_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'.txt' 
     IF keyword_set(sim.special) THEN BEGIN
        testfile = sim.outdir+'inv_output_weekly_prelim_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'.txt'
     ENDIF             
  ENDIF ELSE BEGIN
     IF keyword_set(sim.flask) THEN BEGIN
        testfile = sim.outdir+'inv_output_weekly_flask_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'.txt' 
     ENDIF ELSE BEGIN
        testfile = sim.outdir+'inv_output_weekly_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'.txt'        
        IF keyword_set(sim.special) THEN BEGIN          
           testfile = sim.outdir+'inv_output_weekly_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                      sim.eyyyymm+'_'+qunc+'.txt'
        ENDIF        
     ENDELSE
  ENDELSE
  
  openw,lun,testfile,/get_lun
  printf,lun,fsave
  IF keyword_set(sim.keeppos) THEN BEGIN
     printf,lun,Sasave
     printf,lun,exp(Spsave)
     printf,lun,exp(SP1)
     printf,lun,exp(SP2)
     printf,lun,exp(SP3) 
     printf,lun,Qpdiag
     printf,lun,Q1diag
     printf,lun,Q2diag
     printf,lun,Q3diag 
     printf,lun,uncert    
  ENDIF ELSE BEGIN
     printf,lun,Sasave
     printf,lun,Spsave
     printf,lun,SP1
     printf,lun,SP2   
     printf,lun,SP3   
     printf,lun,Qpsave
     printf,lun,Qp1
     printf,lun,Qp2
     printf,lun,Qp3 
     printf,lun,uncert    
  ENDELSE 
  free_lun,lun
  
  matrixfile = sim.outdir+'inv_matrix_weekly_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'.txt'
  IF keyword_set(sim.special) THEN $
     matrixfile = sim.outdir+'inv_matrix_weekly_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'.txt'  
  openw,lun,matrixfile,/get_lun
  printf,lun,Qpsave
  free_lun,lun
  
  IF NOT keyword_set(rapriori) THEN BEGIN
     IF keyword_set(sim.keeppos) THEN BEGIN
        IF keyword_set(sim.flask) THEN BEGIN
           covfile = sim.outdir+'inv_covar_flask_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                     sim.eyyyymm+'_'+qunc+'.txt'      
           IF keyword_set(sim.nobg) THEN $
              covfile = sim.outdir+'inv_covar_flask_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                        sim.eyyyymm+'_'+qunc+'.txt'
           IF keyword_set(sim.special) THEN $
              covfile = sim.outdir+'inv_covar_flask_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                        sim.eyyyymm+'_'+qunc+'.txt'                  
        ENDIF ELSE BEGIN
           covfile = sim.outdir+'inv_covar_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                     sim.eyyyymm+'_'+qunc+'.txt'      
           IF keyword_set(sim.nobg) THEN $
              covfile = sim.outdir+'inv_covar_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                        sim.eyyyymm+'_'+qunc+'.txt'
           IF keyword_set(sim.special) THEN $
              covfile = sim.outdir+'inv_covar_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                        sim.eyyyymm+'_'+qunc+'.txt'
        ENDELSE
        openw,lun,covfile,/get_lun
        printf,lun,string(mcovar,format='(f6.3)')
        printf,lun,' '
        FOR ij=0,nyears-1 DO BEGIN
           printf,lun,year[ij]
           help = DblArr(sim.ntrace,sim.ntrace)
           help[*,*] = mcovaryr[ij,*,*]
           printf,lun,string(help,format='(f6.3)')
        ENDFOR    
        free_lun,lun              
     ENDIF
  ENDIF
  
  IF keyword_set(rapriori) THEN xifile=sim.outdir+'xi_statstics_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                                       sim.eyyyymm+'_'+qunc+'_apriori.txt'
  
  IF NOT keyword_set(rapriori) THEN xifile=sim.outdir+'xi_statstics_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                                           sim.eyyyymm+'_'+qunc+'_.txt'
  
  openw,lun,xifile,/get_lun
  printf,lun,serzlen
  printf,lun,sernobse
  free_lun,lun    
  
  print, 'End of program: inversion completed.'

END
