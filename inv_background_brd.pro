;+
; NAME:
;
;   inv_background_brd
;
; PURPOSE:
;
;   Read totals file to compute the background correction factor f
;   to estimate true background concentration. 
;   Needs to be called at the beginning of the actual inversion month.
;
;   Cbg(i) =  total(C4(i,k)*f(i,k))
;   with it         = daily values in month i
;        C4(i,k)    = daily values of CH4 concentration of k-th category
;                     and 4th age class at time it in month i
;        f(i,k)     = monthly correction factor
;
;   f(i,k) = (M41(i-1,k)*f(i-1,k)+M31(i-1,k)*fs(i-3,k))/M40(i,k)
;   with i          = month index
;        k          = category index
;        M41(i-1,k) = CH4 mass of 4th age class at the end of month i-1
;        M31(i-1,k) = CH4 mass of 3rd age class at the end of month i-1
;        M40(i,k)   = CH4 mass of 4th age class at the beginning of month i        
;        fs(i-3,k)  = sp(i-3,k)/sa(i-3,k), sp = aposteriori emissions,
;                     sa = apriori emissions of k-th category  in month i-3,
;                     sp determined from inversion
;
;   For the first three months, when fs is not determined yet, fs is
;   set to 1. For the first month, when f(i-1,k) cannot be computed iteratively,
;   f(i-1,k) is assumed to be represented by the global model-obs bias
;   bias(i-1) = CH4mod(i-1)/CH4obs(i-1)
;
; CATEGORY:
;
;   Inverse modelling MAIOLICA2 run. Subroutine inversion.
;
; CALLING SEQUENCE:
;
;  inv_background_brd,sim,yyyymm,fprev=fprev,em3_apost=em3_apost,fcorr=fcorr
;
; INPUTS:
;       sim       = the simulation structure (see inv_configuration for details)
;       yyyymm    = current year and month of inversion
;       fprev     = fltarr, correction factors of previous month
;       em3_apost = aposteriori emissions from inversion of month i-3
;                   to calculate correction factor of month i-3 sp(i-3)/sa(i-3)
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;       fcorr     = fltarr, monthly background correction factors to be used in inversion
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
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 24 November 2011
; BRD 23 November 2017, small adjustments to new structure of sim. This simplifies
;                       the call since structure sim already contains most information.
;-
PRO inv_background_brd,sim,yyyymm,fprev=fprev,em3_apost=em3_apost,fcorr=fcorr

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameters sim missing in call'
     RETURN
  ENDIF
  IF n_elements(yyyymm) EQ 0 THEN BEGIN
     print,'parameters yyyymm missing in call'
     RETURN
  ENDIF

  ncats   = 7                   ; 7 different variables to plot 
                                ;(mass,em_up,em_field,em_res,OH_loss,OH_gain,chem_loss)
  nspec   = sim.nage * sim.ntrace + 1

  fcorr   = FltArr(sim.ntrace)

  ;********************************************************************************
  ; READ DATA FROM CURRENT MONTH
  ; mass4a = mass of 4th age class on first day
  ;********************************************************************************
  mass4a = DblArr(sim.ntrace)

  read_totals_for_inv_brd,sim,yyyymm,data=data
  s    = size(data.budget) 
  nlen = s[3]
  
  FOR it=0,sim.ntrace-1 DO mass4a[it] = data.budget[4*sim.ntrace+5+it,0,0]

  ;********************************************************************************
  ; READ DATA FROM PRECEDING MONTH
  ; mass4e = mass of 4th age class on last day of preceding month
  ; mass3e = mass of 3rd age class on last day of preceding month
  ;********************************************************************************
  mass4e = DblArr(sim.ntrace)
  mass3e = DblArr(sim.ntrace)
  ;; previous month
  pyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')-10),0,6)

  read_totals_for_inv_brd,sim,pyyyymm,data=data
  s    = size(data.budget) 
  nlen = s[3]
  ;data.budget[1,2,3] 1: tracer, 2: ncats categories, need mass => 0 ,3: time
  FOR it=0,sim.ntrace-1 DO mass4e[it] = data.budget[4*sim.ntrace+5+it,0,nlen-1]
  FOR it=0,sim.ntrace-1 DO mass3e[it] = data.budget[3*sim.ntrace+4+it,0,nlen-1]
  
  ;********************************************************************************
  ; READ APRIORI EMISSIONS FROM MONTH i-3
  ;********************************************************************************  
  ; set keyword m3: get apriori emissions from three months prior the actual month
  read_emissions_brd,sim,yyyymm,/m3,sa=sa

  ;********************************************************************************
  ; CALCULATE BACKGROUND CORRECTION FACTOR
  ;********************************************************************************   
  ;f(i,k) = (M41(i-1,k)*f(i-1,k)+M31(i-1,k)*fs(i-3,k))/M40(i,k)
  month1 = yyyymm eq sim.syyyymm
  month2 = yyyymm eq STRMID(gvtime2dtg(dtg2gvtime(sim.syyyymm)+40D),0,6)
  month3 = yyyymm eq STRMID(gvtime2dtg(dtg2gvtime(sim.syyyymm)+70D),0,6)
  month4 = yyyymm eq STRMID(gvtime2dtg(dtg2gvtime(sim.syyyymm)+100D),0,6)

  IF month1 THEN BEGIN
     FOR it=0,sim.ntrace-1 DO fcorr[it] = sim.startcf[it]
  ENDIF ELSE IF month2 OR month3 OR month4 THEN BEGIN
     fcorr = (mass4e*fprev+mass3e)/mass4a
;     FOR it=0,sim.ntrace-1 DO $
;        fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it])/mass4a[it]
;  ENDIF ELSE IF month4 THEN BEGIN
;     fcorr = (mass4e*fprev+mass3e)/mass4a]
;     FOR it=0,sim.ntrace-1 DO $
;        fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it])/mass4a[it]
  ENDIF ELSE BEGIN
     FOR it=0,sim.ntrace-1 DO BEGIN
        IF sa[it] GT 0. THEN BEGIN
           fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it]*(em3_apost[it]/sa[it]))/mass4a[it]
        ENDIF ELSE BEGIN
           fcorr[it] = (mass4e[it]*fprev[it])/mass4a[it]
        ENDELSE
     ENDFOR
  ENDELSE

  ;; aa=n_elements(fcorr)-1
  ;; fcorr_temp=fcorr
  ;; fcorr[where(finite(fcorr) eq 0)] = 1.00000 
  ;; fcorr[aa]=fcorr_temp[aa]
  
  print,'current correction factors = '
  print,fcorr

END
