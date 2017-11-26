;+
; NAME:
;
;   inv_determine_aposteriori
;
; PURPOSE:
;
;   Calculate aposteriori emissions and their uncertainties for a
;   specific month and year.
;
;   Sp = aposteriori emissions calculated for year and month
;   Sa = apriori emissions from program read_emissions.pro
;   Qa = apriori covariance matrix of emissions uncertainties
;   Qp = aposteriori covariance matrix of emissions uncertainties
;   HT = transpose of the sensitivity matrix
;   R  = error covariance matrix
;   Z  = observational data as read from read_data_single.pro
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
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  inv_determine_aposteriori,sim=sim,yyyymm=yyyymm,fprev=fprev,fcorr=fcorr,$
;                            em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
;                            Spout=Spout,Qpout=Qpout,/dump
;
; INPUTS:
;
;       sim       (structure)          :  model simulation information structure
;                                         (see inv_run.pro for details)
;       yyyymm    (string)             :  year and month of current inversion step
;       fprev     (float)              :  background correction factor calculated for previous
;                                         month, input for current step
;       em3_apost (float)              :  aposteriori emissions from inversion of month i-3 
;       Spin      FltArr(ntrace)       :  emissions state vector from previous inversion step
;       Qpin      FltArr(ntrace,ntrace):  the corresponding error covariance
;
;
; KEYWORD PARAMETERS:
;
;       /dump                         : set this keyword to write out receptor point
;                                       values (obs, prior, posterior, background)
;
; OUTPUTS:
;
;       fcorr (float)                 : background correction factor to be used
;                                       as fprev in next simulation month
;       Spout (FltArr,ntrace)         : a posteriori monthly emissions estimates
;                                       for the month of simulation
;       Qpout (FltArr,ntrace x ntrace): a posteriori estimate of
;                                       emissions uncertainty
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
; CSP 04 January 2012
;-

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO inv_determine_aposteriori_final,sim=sim,yyyymm=yyyymm,fprev=fprev,fcorr=fcorr,$
                              em3_apost=em3_apost,Spin=Spin,Qpin=Qpin,$
                              Spout=Spout,Qpout=Qpout,Saout=Saout,Hdump=Hdump,$
                              weekly=weekly,keeppos=keeppos,zlen=zlen,dllh=dllh,$
                              flask=flask,rapriori=rapriori,stats=stats,startcf=startcf,nobg=nobg,special=special,nobse=nobse
                                                     
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'Error: structure sim missing in call to inv_determine_aposteriori'
     RETURN
  ENDIF

  sn = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'     

  IF keyword_set(startcf) THEN sstart = STRCOMPRESS(string(startcf),/REM)

  print, 'Run inv_determine_aposteriori for ', sim.name, ' ', yyyymm

  ;*******************************************
  ; read observational data of year and month
  ;*******************************************
  read_data_single_final,sim=sim,yyyymm=yyyymm,ch4obs=ch4obs,dtgobs=dtgobs,$
                   lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,weekly=weekly,$
                   stats=stats,flask=flask,nobg=nobg,special=special
                                      
  Z = ch4obs

  ;*****************************************************************
  ;* read in 4th age class model data from stats for year and month
  ;*****************************************************************
  stations = nameobs
  dtg      = dtgobs

  ;************************************************************************
  ; In February 2006, model data are only available from 3 February on, 
  ; because of the change from lower vertical to higher vertical resolution 
  ; ECMWF data. Hence, restrict observational data to the same period.
  ;************************************************************************
  IF yyyymm eq '200602' THEN BEGIN
    startdate = dtg2gvtime('200602030000')
    ind       = WHERE(dtg2gvtime(dtg) ge startdate,c)
    IF c gt 0L THEN BEGIN
      zj        = FltArr(c)
      namec     = StrArr(c)
      dtgc      = StrArr(c)
      statsc    = StrArr(c)
      zj[*]     = double(Z[ind])      
      statsc[*] = stations[ind]
      dtgc[*]   = dtg[ind]
      nc        = c
    ENDIF
  ENDIF ELSE BEGIN
    zj       = Z
    dtgc     = dtg
    namec    = stations
    statsc   = stations
    nc       = nobs
  ENDELSE

  IF keyword_set(weekly) THEN BEGIN
    IF keyword_set(flask) THEN BEGIN
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'    
    ENDIF ELSE BEGIN
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'    
      IF keyword_set(special) THEN $
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_special_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'           
    ENDELSE
    
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
  ENDIF ELSE BEGIN
    read_model_data_month_final,sim=sim,yyyymm=yyyymm,stats=statsc,dtg=dtgc,mch4=mch4,mall=mall

    ; convert to ppb
    mch4=mch4*1d9 & mall=mall*1d9                         
  ENDELSE                          
  
  bias = total(ch4obs[0:nc-1])/total(mall)
;print,ch4obs[0:nc-1]/mall
;stop
  ;*********************************************
  ; read apriori emissions of current year/month
  ;*********************************************
  ; units: kg/month
  m3 = 0
  read_emissions_final,yyyymm=yyyymm,m3=m3,sa=sa

;sa[where(sa eq 'Infinity')] = 0 ;flo
;sa[where(sa eq '-Infinity')] = 0 ;flo
;sa[where(finite(sa) eq 0)] = 0 ;flo

  IF yyyymm eq sim.syyyymm THEN BEGIN
     IF keyword_set(keeppos) THEN BEGIN
       SAP   = alog(double(sa))
       SAP[where(SAP eq '-Infinity')] = -20.00000 ;flo to give > 0 in exp(SAP)
       SAP[sim.ntrace-1]=alog(double(sa[sim.ntrace-1]))
       saout = double(sa)     
     ENDIF ELSE BEGIN
       SAP   = double(sa)
       saout = SAP
     ENDELSE  
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
    SAP = DblArr(sim.ntrace*2)
    IF keyword_set(keeppos) THEN BEGIN 
       SAP[0:sim.ntrace-1] = alog(double(sa)) 
       SAP[where(SAP eq '-Infinity')] = -20.00000 ;flo to give > 0 in exp(SAP)
       SAP[sim.ntrace-1]=alog(double(sa[sim.ntrace-1]))
    ENDIF ELSE SAP[0:sim.ntrace-1] = double(sa) 
    SAP[sim.ntrace:sim.ntrace*2-1] = Spin[0:sim.ntrace-1]
    SAP[where(SAP eq 0.00000)] = -20.00000 ;flo to give > 0 in exp(SAP)
    saold = saout
    saout = DblArr(sim.ntrace*2)
    saout[0:sim.ntrace-1] = double(sa)
    saout[sim.ntrace:sim.ntrace*2-1] = saold
 ENDIF ELSE  IF Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
    SAP = DblArr(sim.ntrace*3)
    IF keyword_set(keeppos) THEN BEGIN
       SAP[0:sim.ntrace-1] = alog(double(sa)) 
       SAP[where(SAP eq '-Infinity')] = -20.00000 ;flo to give > 0 in exp(SAP)
       SAP[sim.ntrace-1]=alog(double(sa[sim.ntrace-1]))
ENDIF ELSE SAP[0:sim.ntrace-1] = double(sa)
    SAP[sim.ntrace:sim.ntrace*2-1]   = Spin[0:sim.ntrace-1]
    SAP[sim.ntrace*2:sim.ntrace*3-1] = Spin[sim.ntrace:sim.ntrace*2-1]
    SAP[where(SAP eq 0.00000)] = -20.00000 ;flo to give > 0 in exp(SAP)
    saold = saout
    saout = DblArr(sim.ntrace*3)
    saout[0:sim.ntrace-1] = double(sa)
    saout[sim.ntrace:sim.ntrace*2-1] = saold[0:sim.ntrace-1]
    saout[sim.ntrace*2:sim.ntrace*3-1] = saold[sim.ntrace:sim.ntrace*2-1]
ENDIF ELSE BEGIN
    SAP = DblArr(sim.ntrace*4)
    IF keyword_set(keeppos) THEN BEGIN 
       SAP[0:sim.ntrace-1] = alog(double(sa)) 
       SAP[where(SAP eq '-Infinity')] = -20.00000 ;flo to give > 0 in exp(SAP)
       SAP[sim.ntrace-1]=alog(double(sa[sim.ntrace-1]))
    ENDIF ELSE SAP[0:sim.ntrace-1] = double(sa)
    SAP[sim.ntrace:sim.ntrace*2-1]   = Spin[0:sim.ntrace-1]
    SAP[sim.ntrace*2:sim.ntrace*3-1] = Spin[sim.ntrace:sim.ntrace*2-1]
    SAP[sim.ntrace*3:sim.ntrace*4-1] = Spin[sim.ntrace*2:sim.ntrace*3-1]
    SAP[where(SAP eq 0.00000)] = -20.00000 ;flo to give > 0 in exp(SAP)
    saold = saout
    saout = DblArr(sim.ntrace*4)
    saout[0:sim.ntrace-1] = double(sa)
    saout[sim.ntrace:sim.ntrace*2-1] = saold[0:sim.ntrace-1]
    saout[sim.ntrace*2:sim.ntrace*3-1] = saold[sim.ntrace:sim.ntrace*2-1]
    saout[sim.ntrace*3:sim.ntrace*4-1] = saold[sim.ntrace*2:sim.ntrace*3-1]

 ENDELSE

;  stop
  ;**********************************************************
  ; calculate sensitivities as input for observation operator
  ;**********************************************************
  IF yyyymm eq sim.syyyymm THEN BEGIN
     nopt = 1
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
     nopt = 2
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN 
     nopt = 3
  ENDIF ELSE nopt = 4
sensin = DblArr(sim.ntrace,nc,nopt)
  FOR i=0,nc-1 DO BEGIN
     FOR j=0,nopt-1 DO BEGIN
        offs=j*sim.ntrace
        sensin[*,i,j]=(mch4[offs:offs+sim.ntrace-1,i])/saout[offs:offs+sim.ntrace-1] 

sensin[where(finite(sensin) eq 0)] = 0.00000 ;flo
     ENDFOR
  ENDFOR

;SAP[where(SAP eq '-Infinity')] = -20.00000 ;flo to give > 0 in exp(SAP)
;SAP[where(SAP eq '-Infinity')] = -999.00000 ;flo to give > 0 in exp(SAP)
;stop
;sensin[where(sensin eq 'Infinity')] = 0.00000 ;flo
;SAP[where(SAP eq '-Infinity')] = -20.00000 ;flo to give > 0 in exp(SAP)
;saout[where(saout eq '-Infinity')] = 0.00000 ;flo

;sensin[where(finite(sensin) eq 0)] = 0.00000 ;flo
;sensin[where(finite(sensin) eq 0)] = 0.00000 ;flo
;TEST 22/04/2015

;sensin[13,*,*]=0
;sensin[15,*,*]=0
;sensin[16,*,*]=0
;sensin[19,*,*]=0
;sensin[39,*,*]=0

;SAP[13]=0
;SAP[15]=0
;SAP[16]=0
;SAP[19]=0
;SAP[39]=0
;stop
;TEST

;stop

  IF yyyymm eq sim.syyyymm THEN BEGIN
    H = DblArr(sim.ntrace,nc)
    H[*,*] = sensin[*,*,0]
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
    H = DblArr(sim.ntrace*2,nc)
    H[0:sim.ntrace-1,*]              = sensin[*,*,0]
    H[sim.ntrace:sim.ntrace*2-1,*]   = sensin[*,*,1]
  ENDIF ELSE  IF Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
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
  
  IF keyword_set(Hdump) THEN BEGIN
    IF keyword_set(weekly) THEN BEGIN
      IF keyword_set(flask) THEN BEGIN
        outfile = sim.hdir+'inv_sensitivity_weekly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'        
      ENDIF ELSE BEGIN
        outfile = sim.hdir+'inv_sensitivity_weekly_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'      
        IF keyword_set(special) THEN $
        outfile = sim.hdir+'inv_sensitivity_weekly_special_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'         
      ENDELSE
    ENDIF
    openw,lun,outfile,/get_lun  
    printf,lun,sim.ntrace
    printf,lun,nc
    printf,lun,H
    free_lun,lun
    outfile2 = sim.hdir+'inv_names_weekly_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'
    openw,lun,outfile2,/get_lun
    FOR i=0,nc-1 DO printf,lun,namec[i]
    free_lun,lun
    outfile3 = sim.hdir+'inv_dates_weekly_'+sn+'_'+sim.name+'_'+yyyymm+'_nov12.txt'
    openw,lun,outfile3,/get_lun
    FOR i=0,nc-1 DO printf,lun,dtgc[i]
    free_lun,lun
  ENDIF

  ;*********************************************************
  ; read error covariance matrix (vector)
  ; of year and month (only diagonal elements)
  ; Unit: ppb^2
  ;*********************************************************
  direrror = '/nas/arf/INVERSION/FINAL/'
  IF keyword_set(weekly) THEN BEGIN
    IF keyword_set(rapriori) THEN BEGIN
      IF keyword_set(flask) THEN BEGIN
        IF keyword_set(nobg) THEN $
        errorfile = direrror+'inv_errorcovariance_stations_wm_mismatchonly_flask_nobg_'+sn+'_'+sim.name+'_'+yyyymm+'.dat' $        
        ELSE errorfile = direrror+'inv_errorcovariance_stations_wm_mismatchonly_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'
      ENDIF ELSE BEGIN
        errorfile = direrror+'inv_errorcovariance_stations_wm_mismatchonly_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'      
        IF keyword_set(special) THEN $
        errorfile = direrror+'inv_errorcovariance_stations_wm_mismatchonly_special_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'        
      ENDELSE
    ENDIF ELSE BEGIN    
      IF keyword_set(flask) THEN BEGIN
        errorfile = direrror+'inv_errorcovariance_wm_mismatchonly_aposteriori_flask_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'         
      ENDIF ELSE BEGIN
        errorfile = direrror+'inv_errorcovariance_wm_mismatchonly_aposteriori_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'      
        IF keyword_set(special) THEN $
        errorfile = direrror+'inv_errorcovariance_wm_mismatchonly_aposteriori_special_'+sn+'_'+sim.name+'_'+yyyymm+'.dat'
      ENDELSE
    ENDELSE    
  ENDIF
  header    = '' & dummy = header
  n         = FILE_LINES(errorfile)-1 ; number of data lines by subtracting the header line
  stationen = StrArr(n)
  Rin       = DblArr(n)
  rfact     = 1d
  ;rfact     = 1.d-9
  openr,lun,errorfile,/get_lun
  readf,lun,header
  FOR i=0,n-1 DO BEGIN
    readf,lun,dummy
    result       = STRSPLIT(dummy,/EXTR)
    stationen[i] = result[0]
    Rin[i]   = (result[1]*rfact)^2
  ENDFOR
  free_lun,lun
  R = Diag_Matrix(Rin)
 
;stop

  ;*********************************************************************
  ; calculate apriori emissions covariance matrix of year and month (vector)
  ; Unit: (kg/s)^2
  ;*********************************************************************
  IF yyyymm eq sim.syyyymm THEN BEGIN
    Qa = DblArr(sim.ntrace,sim.ntrace)
    IF keyword_set(keeppos) THEN $
    ;Qa = Diag_Matrix(alog(1.+sim.scaleq)^2) ELSE $   ; transform to diag. matrix
    Qa = Diag_Matrix(sim.scaleq^2) ELSE $
    Qa = Diag_Matrix( (sim.scaleq*Sa)^2 ) ; transform to diag. matrix
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
    Qa      = DblArr(2*sim.ntrace,2*sim.ntrace)
    IF keyword_set(keeppos) THEN $
    ;FOR i=0,sim.ntrace-1 DO Qa[i,i] = alog(1.+sim.scaleq[i])^2 ELSE $
    FOR i=0,sim.ntrace-1 DO Qa[i,i] = sim.scaleq[i]^2 ELSE $
    FOR i=0,sim.ntrace-1 DO Qa[i,i] = (sim.scaleq[i]*Sa[i])^2
    FOR j=0,sim.ntrace-1 DO BEGIN
      Qa[sim.ntrace:2*sim.ntrace-1,sim.ntrace+j] = Qpin[0:sim.ntrace-1,j]
   ENDFOR
 ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
    Qa = DblArr(3*sim.ntrace,3*sim.ntrace)
    IF keyword_set(keeppos) THEN $
    ;FOR i=0,sim.ntrace-1 DO Qa[i,i] = alog(1.+sim.scaleq[i])^2 ELSE $
    FOR i=0,sim.ntrace-1 DO Qa[i,i] = sim.scaleq[i]^2 ELSE $
    FOR i=0,sim.ntrace-1 DO Qa[i,i] = (sim.scaleq[i]*Sa[i])^2
    FOR j=0,sim.ntrace-1 DO BEGIN
      Qa[sim.ntrace:2*sim.ntrace-1,sim.ntrace+j]     = Qpin[0:sim.ntrace-1,j]
      Qa[2*sim.ntrace:3*sim.ntrace-1,sim.ntrace*2+j] = Qpin[sim.ntrace:sim.ntrace*2-1,sim.ntrace+j]
    ENDFOR
 ENDIF ELSE BEGIN
    Qa = DblArr(4*sim.ntrace,4*sim.ntrace)
    IF keyword_set(keeppos) THEN $
    ;FOR i=0,sim.ntrace-1 DO Qa[i,i] = alog(1.+sim.scaleq[i])^2 ELSE $
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
  ;IF c gt 0L THEN stop ;flo 20oct
  ;stop
  ;-------------------------- END READ INGREDIENTS FOR CALCULATION ---------
  ;------------------------ START COMPUTATION OF APOSTERIORI EMISSIONS -----

  ;********************************
  ; calculate background factors f
  ;********************************

  BG = DblArr(nc,sim.ntrace)
  inv_background_final,sim=sim,yyyymm=yyyymm,fprev=fprev,$
                                  em3_apost=em3_apost,fcorr=fcorr,startcf=startcf  
 
;stop

  FOR it=0,sim.ntrace-1 DO BEGIN
 FOR i=0,nc-1 DO BG[i,it] = fcorr[it]*(mch4[it+(sim.nage-1)*sim.ntrace,i])                                     
ENDFOR

                             
     
;   IF keyword_set(keeppos) THEN zmod = zj - BG - H ## exp(SAP) ELSE zmod = zj - BG - H ## SAP
zmod2=DblArr(nc)
   IF keyword_set(keeppos) THEN BEGIN 
FOR i=0,nc-1 DO zmod2[i] = zj[i] - total(BG[i,*]) 
zmod= zmod2 - H ## exp(SAP) 
ENDIF ELSE BEGIN  
FOR i=0,nc-1 DO zmod2[i] = zj[i] - total(BG[i,*])  
zmod= zmod2 - H ## SAP 
;zmod = zj - BG - H ## SAP 

ENDELSE


;stop

  s    = size(H)
  ncol = s[1]    ; number of columns of H
  IF keyword_set(keeppos) THEN FOR j=0,ncol-1 DO H[j,*] = H[j,*]*exp(SAP[j])
  
  TM     = Qa ## TRANSPOSE(H)
  M      = H ## TM + R
  Minv   = invert(M)
  KALMAN = TM ## Minv

  v    = zmod
  dllh = kf_max_likeli_final(M,Minv,v,zlen=zlen,nobse=nobse)
  
  IF yyyymm EQ sim.syyyymm THEN BEGIN
     Spout = DblArr(sim.ntrace)
     Qp    = DblArr(sim.ntrace,sim.ntrace)
  ENDIF ELSE IF Long(yyyymm) EQ (Long(sim.syyyymm)+1L) THEN BEGIN
     Spout = DblArr(2*sim.ntrace)
     Qp    = DblArr(2*sim.ntrace,2*sim.ntrace)
  ENDIF ELSE IF Long(yyyymm) EQ (Long(sim.syyyymm)+2L) THEN  BEGIN
     Spout = DblArr(3*sim.ntrace)
     Qp    = DblArr(3*sim.ntrace,3*sim.ntrace)
  ENDIF ELSE BEGIN
     Spout = DblArr(4*sim.ntrace)
     Qp    = DblArr(4*sim.ntrace,4*sim.ntrace)
  ENDELSE

 IF keyword_set(keeppos) THEN BEGIN
    SAP[where(SAP eq -20.00000)] = 0.00000 ;FLO NEW 08/05
 ENDIF

  Spout = SAP + KALMAN ## zmod
  Qp    = Qa -KALMAN ## H ## Qa

;stop
;IF keyword_set(keeppos) THEN Spout[where(finite(Spout) eq 0)]=0.00000
;;flo 24 nov  

;stop 

aa=n_elements(Spout)-1
Spout_temp=Spout
Spout[where(finite(Spout) eq 0)]=0.00000 ;flo 24 nov  
Spout[aa]=Spout_temp[aa]



;;; TEST COV FLO;;;
 Qpp    = Qp*0


IF yyyymm EQ sim.syyyymm THEN BEGIN
;Qpp[INDGEN(sim.ntrace) * sim.ntrace+1] = 1

indi1=INDGEN(sim.ntrace) * sim.ntrace+INDGEN(sim.ntrace) * 1
FOR i=0,sim.ntrace-1 DO qpp[indi1[i]]=1
ENDIF ELSE IF Long(yyyymm) EQ (Long(sim.syyyymm)+1L) THEN BEGIN
;Qpp[INDGEN(2*sim.ntrace) * 2*sim.ntrace+1] = 1
indi2=INDGEN(2*sim.ntrace) * 2*sim.ntrace+INDGEN(2*sim.ntrace) * 1
FOR i=0,2*sim.ntrace-1 DO qpp[indi2[i]]=1 
ENDIF ELSE IF Long(yyyymm) EQ (Long(sim.syyyymm)+2L) THEN  BEGIN
;Qpp[INDGEN(3*sim.ntrace) * 3*sim.ntrace+1] = 1
indi3=INDGEN(3*sim.ntrace) * 3*sim.ntrace+INDGEN(3*sim.ntrace) * 1
FOR i=0,3*sim.ntrace-1 DO qpp[indi3[i]]=1
ENDIF ELSE BEGIN
;Qpp[INDGEN(4*sim.ntrace) * 4*sim.ntrace+1] = 1
;stop
indi4=INDGEN(4*sim.ntrace) * 4*sim.ntrace+INDGEN(4*sim.ntrace) * 1
FOR i=0,4*sim.ntrace-1 DO qpp[indi4[i]]=1

ENDELSE

;stop
 
;Qp=Qp*Qpp ;comment to keep non diagonal values in Qp

;;;;;

;IF Long(yyyymm) EQ (Long(sim.syyyymm)+4L) THEN stop


  Qpout = Qp
;  stop
  ind1 = WHERE(finite(Spout) eq 0,c1) 
  IF c1 gt 0L THEN stop ; TEST commented this line to see

  FOR it=0,sim.ntrace-1 DO IF finite(Spout[it]) eq 0 THEN stop ;IDEM

  ;IF yyyymm eq '200003' THEN stop
;IF yyyymm EQ 199001 THEN stop

;IF Long(yyyymm) EQ (Long(sim.syyyymm)+3L) THEN stop

;stop

END
