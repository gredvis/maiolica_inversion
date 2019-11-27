;+
; NAME:
;
;   inv_kalman_step
;
; PURPOSE:
;
;   Perform a Kalman smoother step for the current month yyyymm to 
;   calculate the aposteriori emissions and their uncertainties.
;
;   Sp = aposteriori emissions calculated for year and month
;   Sa = apriori emissions from read_emissions.pro
;   Qa = apriori covariance matrix of emissions uncertainties
;   Qp = aposteriori covariance matrix of emissions uncertainties
;   HT = transpose of the sensitivity matrix
;   R  = observation error covariance matrix
;   Z  = observational data as read from read_processed_obs_data_month.pro
;   BG = calculated background concentration on the basis of
;        inv_background_yearmonth.pro, through which the background
;        correction factor fcorr is calculated
;
;   Sp = Sa+Qa*HT*(R+H*Qa*HT)^(-1)*(Z-H*Sa-BG)
;
;   Calculation of aposteriori emissions uncertainty covariance matrix
;   inbracket = HT*R^-1*H + Qa^-1
;   Qp = inbracket^-1
;   
; CATEGORY:
;
;   Inverse modelling MAIOLICA2, Kalman smoother
;
; CALLING SEQUENCE:
;
;  inv_kalman_step,sim,yyyymm,fprev=fprev,fcorr=fcorr,$
;                   em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
;                   Spout=Spout,Qpout=Qpout,Saout=Saout,$
;                   zlen=zlen,dllh=dllh,prelim=prelim,nobse=nobse
;
; INPUTS:
;
;       sim       (structure)          : model simulation information structure
;                                        (see inv_run.pro for details)
;       yyyymm    (string)             : year and month of current inversion step
;       fprev     (fltarr)             : background correction factor(s) calculated for previous
;                                        month, input for current step
;       em3_apost (float)              : aposteriori emissions from inversion of month i-3
;       Spin      FltArr(ntrace)       : emissions state vector from previous inversion step
;       Qpin      FltArr(ntrace,ntrace): the corresponding error covariance
;       Saout     FltArr(ntrace)       : a priori emissions of current and previous months
;                                        Parameter is both input and output
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;       fcorr (float or fltarr,ntrace) : background correction factor(s) to be used
;                                        as fprev in next simulation month
;       Spout (FltArr,ntrace)          : a posteriori monthly emissions estimates
;                                        for the month of simulation
;       Qpout (FltArr,ntrace x ntrace) : a posteriori estimate of
;                                        emissions uncertainty
;       Saout (FltArr,ntrace)          : a priori emissions of current and previous months
;                                        Parameter is both input and output
;       zlen                           : Xhisquare statistic, already scaled by number of
;                                        observations, should therefore be close to 1
;       dllh                           : log-likelihood
;       nobse                          : number of observations in this month
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
; CSP 04 Jan 2012
; BRD 20 Nov 2017: simplified and adjusted to new structure of sim
; BRD 05 Jan 2018: adjusted to new netcdf input and ouptut files and renamed
;                  from inv_determine_aposteriori to inv_kalman_step
;-

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO inv_kalman_step,sim,yyyymm,fprev=fprev,fcorr=fcorr,$
                    em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
                    Spout=Spout,Qpout=Qpout,Saout=Saout,$
                    zlen=zlen,dllh=dllh,prelim=prelim,nobse=nobse
  
  minalogemiss = -20D           ; minimum log number when an emission is zero
                                ; this corresponds to 7.5 x 1e-16 Tg/yr, which is
                                ; a very small number
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'Error: structure sim missing in call to inv_kalman_step'
     RETURN
  ENDIF
  
  print, 'Run inv_kalman_step for ', sim.name, ' ', yyyymm

  ;****************************************************
  ; read observation and model data of year and month
  ;****************************************************
  read_obsmod_netcdf_month,sim,yyyymm,ch4obs=ch4obs,ch4mod=ch4mod

  ;************************************************************************
  ; In February 2006, model data are only available from 3 February onwards, 
  ; because of the change from lower vertical to higher vertical resolution 
  ; ECMWF data. Hence, restrict data to the same period.
  ;************************************************************************
  IF yyyymm eq '200602' THEN BEGIN
     startdate = dtg2gvtime('200602030000')
     ind       = WHERE(dtg2gvtime(ch4obs.dtg) ge startdate,c)
     IF c EQ 0 THEN stop
     ch4obs = ch4obs[ind]
     ch4mod = ch4mod[ind]
  ENDIF
  nc = n_elements(ch4obs)

  bias = total(ch4obs.ch4)/total(ch4mod.ch4)

  ;;*********************************************
  ;; read apriori emissions of current year/month
  ;;*********************************************
  ;; units: kg/day
  read_emissions,sim,yyyymm,m3=0,sa=sa
  
  month1 = yyyymm eq sim.syyyymm
  month2 = yyyymm eq STRMID(gvtime2dtg(dtg2gvtime(sim.syyyymm)+40D),0,6)
  month3 = yyyymm eq STRMID(gvtime2dtg(dtg2gvtime(sim.syyyymm)+70D),0,6)

  IF month1 THEN BEGIN
     ;; first month of assimilation
     IF keyword_set(sim.keeppos) THEN BEGIN
        SAP   = alog(double(sa))
        SAP[where(SAP eq '-Infinity')] = minalogemiss 
        saout = double(sa)     
     ENDIF ELSE BEGIN
        SAP   = double(sa)
        saout = SAP
     ENDELSE  
  ENDIF ELSE IF month2 THEN BEGIN
     ;; 2nd month of assimilation
     SAP = DblArr(sim.ntrace*2)
     IF keyword_set(sim.keeppos) THEN BEGIN 
        SAP[0:sim.ntrace-1] = alog(double(sa)) 
        SAP[where(SAP eq '-Infinity')] = minalogemiss 
     ENDIF ELSE SAP[0:sim.ntrace-1] = double(sa) 
     SAP[sim.ntrace:sim.ntrace*2-1] = Spin[0:sim.ntrace-1]
     SAP[where(SAP eq 0.00000)] = minalogemiss
     saold = saout
     saout = DblArr(sim.ntrace*2)
     saout[0:sim.ntrace-1] = double(sa)
     saout[sim.ntrace:sim.ntrace*2-1] = saold
  ENDIF ELSE IF month3 THEN BEGIN
     ;; 3rd month of assimilation
     SAP = DblArr(sim.ntrace*3)
     IF keyword_set(sim.keeppos) THEN BEGIN
        SAP[0:sim.ntrace-1] = alog(double(sa)) 
        SAP[where(SAP eq '-Infinity')] = minalogemiss 
     ENDIF ELSE SAP[0:sim.ntrace-1] = double(sa)
     SAP[sim.ntrace:sim.ntrace*2-1]   = Spin[0:sim.ntrace-1]
     SAP[sim.ntrace*2:sim.ntrace*3-1] = Spin[sim.ntrace:sim.ntrace*2-1]
     SAP[where(SAP eq 0.00000)] = minalogemiss
     saold = saout
     saout = DblArr(sim.ntrace*3)
     saout[0:sim.ntrace-1] = double(sa)
     saout[sim.ntrace:sim.ntrace*2-1] = saold[0:sim.ntrace-1]
     saout[sim.ntrace*2:sim.ntrace*3-1] = saold[sim.ntrace:sim.ntrace*2-1]
  ENDIF ELSE BEGIN
     ;; fourth month or later
     SAP = DblArr(sim.ntrace*4)
     IF keyword_set(sim.keeppos) THEN BEGIN 
        SAP[0:sim.ntrace-1] = alog(double(sa)) 
        SAP[where(SAP eq '-Infinity')] = minalogemiss 
     ENDIF ELSE SAP[0:sim.ntrace-1] = double(sa)
     SAP[sim.ntrace:sim.ntrace*2-1]   = Spin[0:sim.ntrace-1]
     SAP[sim.ntrace*2:sim.ntrace*3-1] = Spin[sim.ntrace:sim.ntrace*2-1]
     SAP[sim.ntrace*3:sim.ntrace*4-1] = Spin[sim.ntrace*2:sim.ntrace*3-1]
     SAP[where(SAP eq 0.00000)] = minalogemiss 
     saold = saout
     saout = DblArr(sim.ntrace*4)
     saout[0:sim.ntrace-1] = double(sa)
     saout[sim.ntrace:sim.ntrace*2-1] = saold[0:sim.ntrace-1]
     saout[sim.ntrace*2:sim.ntrace*3-1] = saold[sim.ntrace:sim.ntrace*2-1]
     saout[sim.ntrace*3:sim.ntrace*4-1] = saold[sim.ntrace*2:sim.ntrace*3-1]
  ENDELSE

  ;;**********************************************************
  ;; calculate sensitivities as input for observation operator
  ;;**********************************************************
  nopt = 4                      ; default
  IF month1 THEN nopt = 1
  IF month2 THEN nopt = 2
  IF month3 THEN nopt = 3

  sensin = DblArr(sim.ntrace,nc,nopt)
  FOR i=0,nc-1 DO BEGIN
     FOR j=0,nopt-1 DO BEGIN
        offs=j*sim.ntrace
        sensin[*,i,j]=(ch4mod[i].ch4trace[offs:offs+sim.ntrace-1])/saout[offs:offs+sim.ntrace-1] 
        sensin[where(finite(sensin) eq 0)] = 0D
     ENDFOR
  ENDFOR

  IF month1 THEN BEGIN
     H = sensin
  ENDIF ELSE IF month2 THEN BEGIN
     H = DblArr(sim.ntrace*2,nc)
     H[0:sim.ntrace-1,*]              = sensin[*,*,0]
     H[sim.ntrace:sim.ntrace*2-1,*]   = sensin[*,*,1]
  ENDIF ELSE IF month3 THEN BEGIN
     H = DblArr(sim.ntrace*3,nc)
     H[0:sim.ntrace-1,*]              = sensin[*,*,0]
     H[sim.ntrace:sim.ntrace*2-1,*]   = sensin[*,*,1]
     H[sim.ntrace*2:sim.ntrace*3-1,*] = sensin[*,*,2]
  ENDIF ELSE BEGIN
     H = DblArr(sim.ntrace*4,nc)
     H[0:sim.ntrace-1,*]              = sensin[*,*,0]
     H[sim.ntrace:sim.ntrace*2-1,*]   = sensin[*,*,1]
     H[sim.ntrace*2:sim.ntrace*3-1,*] = sensin[*,*,2]
     H[sim.ntrace*3:sim.ntrace*4-1,*] = sensin[*,*,3]
  ENDELSE
  

  ;;*********************************************************
  ;; read error covariance matrix (vector) of year and month 
  ;; (only diagonal elements). Unit: ppb^2
  ;;*********************************************************
  ;read_errcov_month,sim,yyyymm,errcov=errcov,stats=stats,prelim=prelim
  errcov=read_model_data_mismatch_netcdf(sim,prelim=prelim,yyyymm=yyyymm,$
                                         stats=ch4obs.name)
  R = Diag_Matrix(errcov)
  
  ;;*********************************************************************
  ;; calculate apriori emissions covariance matrix of year and month (vector)
  ;; Unit: (kg/s)^2
  ;;*********************************************************************
  IF month1 THEN BEGIN
     Qa = DblArr(sim.ntrace,sim.ntrace)
     IF keyword_set(sim.keeppos) THEN $
        Qa = Diag_Matrix(sim.scaleq^2) ELSE $
           Qa = Diag_Matrix( (sim.scaleq*Sa)^2 ) ; transform to diag. matrix
  ENDIF ELSE IF month2 THEN BEGIN
     Qa = DblArr(2*sim.ntrace,2*sim.ntrace)
     IF keyword_set(sim.keeppos) THEN $
        FOR i=0,sim.ntrace-1 DO Qa[i,i] = sim.scaleq[i]^2 ELSE $
           FOR i=0,sim.ntrace-1 DO Qa[i,i] = (sim.scaleq[i]*Sa[i])^2
     FOR j=0,sim.ntrace-1 DO BEGIN
        Qa[sim.ntrace:2*sim.ntrace-1,sim.ntrace+j] = Qpin[0:sim.ntrace-1,j]
     ENDFOR
  ENDIF ELSE IF month3 THEN BEGIN
     Qa = DblArr(3*sim.ntrace,3*sim.ntrace)
     IF keyword_set(sim.keeppos) THEN $
        FOR i=0,sim.ntrace-1 DO Qa[i,i] = sim.scaleq[i]^2 ELSE $
           FOR i=0,sim.ntrace-1 DO Qa[i,i] = (sim.scaleq[i]*Sa[i])^2
     FOR j=0,sim.ntrace-1 DO BEGIN
        Qa[sim.ntrace:2*sim.ntrace-1,sim.ntrace+j]     = Qpin[0:sim.ntrace-1,j]
        Qa[2*sim.ntrace:3*sim.ntrace-1,sim.ntrace*2+j] = Qpin[sim.ntrace:sim.ntrace*2-1,sim.ntrace+j]
     ENDFOR
  ENDIF ELSE BEGIN
     Qa = DblArr(4*sim.ntrace,4*sim.ntrace)
     IF keyword_set(sim.keeppos) THEN $
        FOR i=0,sim.ntrace-1 DO Qa[i,i] = sim.scaleq[i]^2 ELSE $
           FOR i=0,sim.ntrace-1 DO Qa[i,i] = (sim.scaleq[i]*Sa[i])^2
     FOR j=0,sim.ntrace-1 DO BEGIN
        Qa[sim.ntrace:2*sim.ntrace-1,sim.ntrace+j]     = Qpin[0:sim.ntrace-1,j]
        Qa[2*sim.ntrace:3*sim.ntrace-1,sim.ntrace*2+j] = Qpin[sim.ntrace:sim.ntrace*2-1,sim.ntrace+j]
        Qa[3*sim.ntrace:4*sim.ntrace-1,sim.ntrace*3+j] = Qpin[sim.ntrace*2:sim.ntrace*3-1,sim.ntrace*2+j]      
     ENDFOR
  ENDELSE

  ind    = WHERE(DIAG_MATRIX(Qa) lt 0.,c)
  IF c gt 0L THEN print, c, ' diag. elements of Qa lt 0.!'
  
  ;;-------------------------- END READ INGREDIENTS FOR CALCULATION ---------

  ;;------------------------ START COMPUTATION OF APOSTERIORI EMISSIONS -----

  ;;********************************
  ;; calculate background factors f
  ;;********************************

  BG = DblArr(nc,sim.ntrace)
  inv_background,sim,yyyymm,fprev=fprev,em3_apost=em3_apost,fcorr=fcorr  
  
  FOR it=0,sim.ntrace-1 DO BEGIN
     FOR i=0,nc-1 DO BG[i,it] = fcorr[it]*(ch4mod[i].ch4trace[(sim.nage-1)*sim.ntrace+it])       
  ENDFOR
  
  ;; subtract simulated background from the observations before
  ;; calculating residual model - obs
  zmod2 = ch4obs.ch4 - total(BG,2)
  IF keyword_set(sim.keeppos) THEN BEGIN 
     zmod= zmod2 - H ## exp(SAP)
  ENDIF ELSE BEGIN  
     zmod= zmod2 - H ## SAP
  ENDELSE

  s    = size(H)
  ncol = s[1]                   ; number of columns of H
  IF keyword_set(sim.keeppos) THEN FOR j=0,ncol-1 DO H[j,*] = H[j,*]*exp(SAP[j])
  
  ;;********************************
  ;; perform Kalman filter step
  ;;********************************

  TM     = Qa ## TRANSPOSE(H)
  M      = H ## TM + R
  Minv   = invert(M)
  KALMAN = TM ## Minv
  
  v    = zmod
  dllh = kf_max_likeli_final(M,Minv,v,zlen=zlen,nobse=nobse)

  ;; Kalamn update only after first few months have been filled with data
  IF month1 EQ 0 AND month2 EQ 0 AND month3 EQ 0 THEN BEGIN
     Spout = SAP + KALMAN ## zmod
     Qpout = Qa -KALMAN ## H ## Qa
  ENDIF ELSE BEGIN
     Spout = SAP
     Qpout = Qa
  ENDELSE

  FOR it=0,sim.ntrace-1 DO IF finite(Spout[it]) eq 0 THEN stop 
  
END
