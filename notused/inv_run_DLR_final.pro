;+
; NAME:
;
;   inv_run
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
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  inv_run,sim=sim,scalef_Q=scalef_Q
;
; INPUTS:
;
;       sim      :  structure with all information about the simulation
;                           {name:'',$     ; simulation name, default is 'URMEL_CTRL_II'
;                            obsdir:'',$   ; observation data directory, default is
;                                          ; '/nas/spc134/URMEL/INVERSION/OBSINPUT/'
;                            modeldir:'',$ ; model output directory, default is
;                                          ; '/nas/spc134/URMEL/FLEXPART80CTP/output/'
;                            outdir:'',$   ; inversion result output directory
;                            syyyymm:'',$  ; year and month of start of inversion
;                            eyyyymm:'',$  ; year and month of end of inversion
;                            scalef_Q:1,$  ; uncertainty class of a
;                            priori emissions
;                            ntrace:11,$   ; number of tracers/CH4 sources
;                            nage:4}       ; number of age classes
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;       /dump              : set this keyword to write out receptor point
;                            values (obs, prior, posterior, and background)
;
; OUTPUTS:
;
;       fsave (float)      : background correction factor to be used
;                            as fprev in next simulation month
;       Sp (FltArr, ntrace): aposteriori monthly emissions estimates for the month of simulation
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
;   syyyymm = '200102'
;   eyyyymm = '200201'
;   sim = {name:'URMEL_CTRL_II',$
;          obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
;          modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
;          outdir:'/home/brd134/projects/URMEL/',$
;          syyyymm:'200102',eyyyymm:'200201',scalef_Q:1,ntrace:11,nage:4}
;   inv_run,sim=sim
;
; MODIFICATION HISTORY:
; CSP 29 September 2011
; BRD 10 March 2012, 2 indices for year and month replaced by one index,
;                    and variable sim changed to a structure containing all
;                    details about the inversion
;-
PRO inv_run_DLR_final,sim=sim,Hdump=Hdump,weekly=weekly,keeppos=keeppos,$
            serdllh=serdllh,sumdllh=sumdllh,serzlen=serzlen,sumzlen=sumzlen,flask=flask,rapriori=rapriori,$
            stats=stats,startcf=startcf,nobg=nobg,special=special,nobse=nobse

;  weekly  = 1 ;flon_new_nov

  sn      = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'
  IF keyword_set(startcf) THEN sstart = STRCOMPRESS(string(startcf),/REM)
  print, 'sstart = ', sstart 
  
  IF n_elements(sim) EQ 0 THEN BEGIN
;     sim = {name:'final_sim01',$
;          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;          modeldir:'/nas/arf/output/',$
;            outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
;             hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
;          syyyymm:'199002',eyyyymm:'199004',scaleq:[0.10,0.10,0.10,0.65,0.85,0.3,0.35,0.45,0.55,0.95,0.85],$
;          ntrace:11,nage:4}

sim = {name:'final_sim01',$
          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
          modeldir:'/nas/arf/output/',$
            outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
             hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
          syyyymm:'199002',eyyyymm:'199112',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30],$
          ntrace:48,nage:5}

  ENDIF
    
  nopt = sim.nage-1 ; number of times an emission is estimated,
                    ; one less than nage beccause oldest age is background 
  
  ; calculate number of months to simulate
  syyyy    = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n        = eyyyy*12L+emm-(syyyy*12L+smm)+1
  yyyysave = IntArr(n)
  nyears   = eyyyy-syyyy+1
  year     = syyyy+indgen(nyears)

  fprev     = 0.
  fsave     = DblArr(n,sim.ntrace)
  Qpsave    = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties
  Qp1       = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties
  Qp2       = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties    
  Qp3       = DblArr(n,sim.ntrace,sim.ntrace) ; a posteriori uncertainties    
  Spsave    = DblArr(n,sim.ntrace)            ; a posteriori emissions
  SP1       = DblArr(n,sim.ntrace)            ; 1st a posteriori estimate
  SP2       = DblArr(n,sim.ntrace)            ; 2nd a posteriori estimate
  SP3       = DblArr(n,sim.ntrace)            ; 3nd a posteriori estimate
  Sasave    = DblArr(n,sim.ntrace)            ; a priori emissions
  yyyymm    = StrArr(n)                       ; month and year
  serzlen   = DblArr(n)                       ; time series chi-square statistics
sernobse   = DblArr(n)                       ; time series nobs
  serdllh   = DblArr(n)                       ; time series log likelihood statistics
  sumzlen   = 0.D
  sumdllh   = 0.D
  em3_apost = DblArr(sim.ntrace)              ; posterior emissions month i-3

  irun     = 0

  print, 'run inversion with uncertainty parameters: '
  print, sim.scaleq
  
  ;; loop over all months of the inversion period
  cyyyymm = sim.syyyymm
  FOR i=0,n-1 DO BEGIN

     cyyyy       = fix(STRMID(cyyyymm,0,4)) & cmm = fix(STRMID(cyyyymm,4,2))
     yyyysave[i] = cyyyy 
     
     print, 'Analysing year ', cyyyy, ', month ', cmm
     
     IF i GE nopt THEN BEGIN
       IF keyword_set(keeppos) THEN em3_apost[*] = exp(Spsave[i-nopt,*]) ELSE em3_apost[*] = Spsave[i-nopt,*]
       ind = WHERE(finite(em3_apost) eq 0,c)
       IF c gt 0L THEN stop
     ENDIF

;stop
;IF Long(cyyyymm) eq (Long(sim.syyyymm)+4L)  THEN stop
 

    ;;***************************************************
     ;; call inv_determine_aposteriori for year and month
     ;;***************************************************  
     inv_determine_aposteriori_DLR_final,sim=sim,yyyymm=cyyyymm,fprev=fprev,fcorr=fcorr,$
                               em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
                               Spout=Spout,Qpout=Qpout,Saout=Saout,Hdump=Hdump,$
                               weekly=weekly,keeppos=keeppos,zlen=zlen,dllh=dllh,$
                               flask=flask,rapriori=rapriori,stats=stats,startcf=startcf,nobg=nobg,special=special,nobse=nobse
                                           
     serzlen[i] = zlen
     serdllh[i] = dllh
     sernobse[i] = nobse
     IF finite(zlen) eq 1 THEN sumzlen += zlen
     IF finite(dllh) eq 1 THEN sumdllh += dllh



;stop

     ; store current state and error covariance  
   FOR it=0,sim.ntrace-1 DO BEGIN
     fsave[i,it] = fcorr[it]
  ENDFOR
     IF i GE (nopt-1) THEN BEGIN
;stop
        ; start in month nopt to take final estimate that has been calculated nopt times
        ; and is now representative of month i-(nopt-1)
         Qpsave[i-(nopt-1),*,*] = Qpout[(nopt-1)*sim.ntrace:nopt*sim.ntrace-1,$
                                        (nopt-1)*sim.ntrace:nopt*sim.ntrace-1]
         ;Qp1[i,*,*]             = Qpout[0:sim.ntrace-1,0:sim.ntrace-1]
         FOR j=0,sim.ntrace-1 DO BEGIN
           Qp1[i,0:sim.ntrace-1,j] = Qpout[0:sim.ntrace-1,j]
           Qp2[i-(nopt-3),0:sim.ntrace-1,j] = Qpout[sim.ntrace:sim.ntrace*2-1,sim.ntrace+j]
           Qp3[i-(nopt-2),0:sim.ntrace-1,j] = Qpout[2*sim.ntrace:sim.ntrace*3-1,2*sim.ntrace+j]
         ENDFOR         
         Spsave[i-(nopt-1),*]   = Spout[(nopt-1)*sim.ntrace:nopt*sim.ntrace-1]
         SP1[i,*]               = Spout[0:sim.ntrace-1]
         SP2[i-(nopt-3),*]      = Spout[(nopt-3)*sim.ntrace:(nopt-2)*sim.ntrace-1]         
         SP3[i-(nopt-2),*]      = Spout[(nopt-2)*sim.ntrace:(nopt-1)*sim.ntrace-1]         
     ENDIF 

     ; estimates for the last few month are only available from less than nopt
     ; optimizations
     IF i EQ (n-1) THEN BEGIN
       Qpsave[i-1,*,*] = Qpout[sim.ntrace:2*sim.ntrace-1,sim.ntrace:2*sim.ntrace-1]
       Spsave[i-1,*]   = Spout[sim.ntrace:2*sim.ntrace-1]
       SP2[i,*]        = Spsave[i-2,*]
       Qp2[i,*,*]      = Qpsave[i-2,*,*]
       SP3[i,*]        = Spsave[i-1,*] ;flo NEW
       Qp3[i,*,*]      = Qpsave[i-1,*,*] ;flo NEW
       Qpsave[i,*,*]   = Qpout[0:sim.ntrace-1,0:sim.ntrace-1]
       Spsave[i,*]     = Spout[0:sim.ntrace-1]
     ENDIF

     Sasave[i,*] = saout[0:sim.ntrace-1]

     ;; get next month by adding 40 days and rounding off to full months
     cyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)

     ; variables as input for next inversion step
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
  IF keyword_set(keeppos) THEN BEGIN
    FOR i=0,n-1 DO BEGIN
      qpdiag[i,*]  = sqrt(diag_matrix(reform(qpsave[i,*,*])))
      FOR j=0,sim.ntrace-1 DO FOR k=0,sim.ntrace-1 DO covar[i,j,k] = $
      qpsave[i,j,k]*exp(Spsave[i,j])*exp(Spsave[i,k])/( sqrt(qpsave[i,j,j]*exp(Spsave[i,j])^2)*sqrt(qpsave[i,k,k]*exp(Spsave[i,k])^2) )  
      q1diag[i,*] = sqrt(diag_matrix(reform(Qp1[i,*,*])))
      q2diag[i,*] = sqrt(diag_matrix(reform(Qp2[i,*,*])))
      q3diag[i,*] = sqrt(diag_matrix(reform(Qp3[i,*,*])))
   ENDFOR
;stop
    FOR j=0,sim.ntrace-1 DO BEGIN
      FOR k=0,sim.ntrace-1 DO BEGIN
        mcovar[j,k] = mean(covar[12:n-1,j,k])  ; mean error covariance
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
  
  ; absolute total uncertainty
  FOR i=0,n-1 DO BEGIN
    tsp       = DblArr(sim.ntrace)
    tsp[*]    = exp(Spsave[i,*])
    tQ        = DblArr(sim.ntrace,sim.ntrace)
    tQ[*,*]   = reform(qpsave[i,*,*]) 
    step      = transpose(tsp) # tQ # tsp
    uncert[i] = sqrt(step)/1.e9
  ENDFOR

;stop

  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)

  IF keyword_set(weekly) THEN BEGIN
    IF keyword_set(rapriori) THEN BEGIN
      testfile = sim.outdir+'inv_output_weekly_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'          
      IF keyword_set(flask) THEN $
      testfile = sim.outdir+'inv_output_weekly_flask_DLR_prelim_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt' 
      IF keyword_set(special) THEN BEGIN
        testfile = sim.outdir+'inv_output_weekly_prelim_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'_nov12.txt'
        IF keyword_set(startcf) THEN $
        testfile = sim.outdir+'inv_output_weekly_prelim_special_startcf'+sstart+'_'+sn+'_'+sim.name+'_'+$
                   sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'
      ENDIF             
    ENDIF ELSE BEGIN
      IF keyword_set(flask) THEN BEGIN
        testfile = sim.outdir+'inv_output_weekly_flask_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'_nov12.txt' 
      ENDIF ELSE BEGIN
        testfile = sim.outdir+'inv_output_weekly_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                   sim.eyyyymm+'_'+qunc+'_nov12.txt'        
        IF keyword_set(special) THEN BEGIN          
          testfile = sim.outdir+'inv_output_weekly_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                     sim.eyyyymm+'_'+qunc+'_nov12.txt'
          IF keyword_set(startcf) THEN $
          testfile = sim.outdir+'inv_output_weekly_special_startcf'+sstart+'_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                     sim.eyyyymm+'_'+qunc+'_nov12.txt'  
        ENDIF        
                                 
      ENDELSE    
    ENDELSE 
  ENDIF 
            

;stop 
;stop

;print,((exp(Spsave))*365*1e-9)/((Sasave)*365*1e-9)


 
  openw,lun,testfile,/get_lun
  printf,lun,fsave
  IF keyword_set(keeppos) THEN BEGIN
    printf,lun,Sasave
    printf,lun,exp(Spsave)
    printf,lun,exp(SP1)
    printf,lun,exp(SP2)
    printf,lun,exp(SP3) ;flo
    printf,lun,Qpdiag
    printf,lun,Q1diag
    printf,lun,Q2diag
    printf,lun,Q3diag ;flo
    printf,lun,uncert    
  ENDIF ELSE BEGIN
    printf,lun,Sasave
    printf,lun,Spsave
    printf,lun,SP1
    printf,lun,SP2   
    printf,lun,SP3 ;flo   
    printf,lun,Qpsave
    printf,lun,Qp1
    printf,lun,Qp2
    printf,lun,Qp3 ;flo
  ENDELSE 
  free_lun,lun

  matrixfile = sim.outdir+'inv_matrix_weekly_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'
  IF keyword_set(special) THEN $
  matrixfile = sim.outdir+'inv_matrix_weekly_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'  
  openw,lun,matrixfile,/get_lun
  printf,lun,Qpsave
  free_lun,lun

  IF NOT keyword_set(rapriori) THEN BEGIN
    IF keyword_set(keeppos) THEN BEGIN
      IF keyword_set(flask) THEN BEGIN
        covfile = sim.outdir+'inv_covar_flask_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_nov12.txt'      
        IF keyword_set(nobg) THEN $
        covfile = sim.outdir+'inv_covar_flask_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_nov12.txt'
        IF keyword_set(special) THEN $
        covfile = sim.outdir+'inv_covar_flask_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_nov12.txt'                  
      ENDIF ELSE BEGIN
        covfile = sim.outdir+'inv_covar_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_nov12.txt'      
        IF keyword_set(nobg) THEN $
        covfile = sim.outdir+'inv_covar_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_nov12.txt'
        IF keyword_set(special) THEN $
        covfile = sim.outdir+'inv_covar_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_nov12.txt'
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

IF keyword_set(rapriori) THEN xifile=sim.outdir+'xi_statstics_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_dec15_apriori.txt'

IF NOT keyword_set(rapriori) THEN xifile=sim.outdir+'xi_statstics_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                  sim.eyyyymm+'_'+qunc+'_dec15.txt'

openw,lun,xifile,/get_lun
 printf,lun,serzlen
printf,lun,sernobse
free_lun,lun    

  print, 'End of program: inversion completed.'
;stop
END
