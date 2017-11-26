;+
; NAME:
;
;   inv_background_yearmonth
;
; PURPOSE:
;
;   Read totals file to compute the background correction factor f
;   to estimate true background concentration. Needs to be called
;   first hand in the beginning of the actual inversion month.
;
;   Cbg(it) =  total(C4(it,k)*f(i,k))
;   with it         = daily values in month i
;        C4(it,k)   = daily values of CH4 concentration of k-th category
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
;   Inverse modelling URMEL CTRL run. Subroutine inversion.
;
; CALLING SEQUENCE:
;
;  inv_background_yearmonth,sim=sim,yyyymm=yyyymm,syyyymm=syyyymm,$
;                           fprev=fprev,em3_apost=em3_apost,fcorr=fcorr
;
; INPUTS:
;       sim       = string, define simulation
;       syyyymm   = start year and month of inversion
;       yyyymm    = current year and month of inversion
;       fprev     = float, correction factor of previous month
;       em3_apost = aposteriori emissions from inversion of month i-3
;                   to calculate correction factor of month i-3 sp(i-3)/sa(i-3)
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;       fcorr     = float, monthly background correction factor to be used in inversion
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
;-
PRO inv_background_yearmonth_48,sim=sim,yyyymm=yyyymm,fprev=fprev,$
                                      em3_apost=em3_apost,fcorr=fcorr,startcf=startcf

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'URMEL_SENSC_II',$
          obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
          modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
            outdir:'/home/spc134/IDL/urmel/INVERSION/',$
             hdir: '/nas/spc134/URMEL/INVERSION/SENSITIVITIES/',$
          syyyymm:'200002',eyyyymm:'200812',scaleq:[0.10,0.10,0.10,0.50,0.50,0.3,0.35,0.45,0.55,0.95,0.85],$
          ntrace:11,nage:4}
  ENDIF

  ncats   = 7  ; 7 different variables to plot 
;                (mass,em_up,em_field,em_res,OH_loss,OH_gain,chem_loss)
  nspec   = sim.nage * sim.ntrace + 1

  fcorr   = FltArr(sim.ntrace)

  ;********************************************************************************
  ; READ DATA FROM CURRENT MONTH
  ; mass4a = mass of 4th age class on first day
  ;********************************************************************************
  mass4a = DblArr(sim.ntrace)

 ; read_totals_URMEL,sim=sim,yyyymm=yyyymm,data=data
  read_totals_maiolicaii_final_for_inv,sim=sim,yyyymm=yyyymm,data=data
  s    = size(data.budget) 
  nlen = s[3]
  ;data.budget[1,2,3] 1: tracer, 2: ncats categories, need mass => 0 ,3: time
  ;FOR it=0,sim.ntrace-1 DO mass4a[it] = data.budget[3*sim.ntrace+1+it,0,0]
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

  ;read_totals_URMEL,sim=sim,yyyymm=pyyyymm,data=data
  read_totals_maiolicaii_final_for_inv,sim=sim,yyyymm=pyyyymm,data=data
  s    = size(data.budget) 
  nlen = s[3]
  ;data.budget[1,2,3] 1: tracer, 2: ncats categories, need mass => 0 ,3: time
  FOR it=0,sim.ntrace-1 DO mass4e[it] = data.budget[4*sim.ntrace+5+it,0,nlen-1]
  FOR it=0,sim.ntrace-1 DO mass3e[it] = data.budget[3*sim.ntrace+4+it,0,nlen-1]
  
  ;********************************************************************************
  ; READ APRIORI EMISSIONS FROM MONTH i-3
  ;********************************************************************************  
  ; set keyword m3: get apriori emissions from three months prior the actual month
  read_emissions,yyyymm=yyyymm,/m3,sa=sa

  ;********************************************************************************
  ; CALCULATE BACKGROUND CORRECTION FACTOR
  ;********************************************************************************   
  ;f(i,k) = (M41(i-1,k)*f(i-1,k)+M31(i-1,k)*fs(i-3,k))/M40(i,k)

  ; spinup background correction factors
  ; for the first three months of the inversion
  ;startcorr = [1.025,1.025,1.025,1.10,1.10,1.10,1.025,1.10,1.025,1.025,1.025]
;  startcorr = [1.014,0.99,1.01,1.43,1.26,1.28,1.07,1.30,1.06,0.68,0.54]
  ;corrstart = [1.019,0.99,1.00,1.18,1.15,1.13,1.15,1.11,1.03,0.68,0.90]
  IF yyyymm EQ sim.syyyymm THEN BEGIN
     FOR it=0,sim.ntrace-1 DO fcorr[it] = startcf[it]
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) or $
     Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN     
     FOR it=0,sim.ntrace-1 DO $
     ;   fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it])/mass4a[it]
     ;FOR it=0,sim.ntrace-1 DO $
        fcorr[it] = startcf[it]     
  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+3L)  $
      THEN BEGIN     
     FOR it=0,sim.ntrace-1 DO $
     ;   fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it])/mass4a[it]
     ;FOR it=0,sim.ntrace-1 DO $
        fcorr[it] = startcf[it]     
  ENDIF ELSE BEGIN


     FOR it=0,sim.ntrace-1 DO BEGIN $
        fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it]*(em3_apost[it]/sa[it]))/mass4a[it]  ; normal version


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;fcorr[HS[it]]=(total(mass4e[HS[it]]))*total(fprev[HS[it]])+total(mass3e[HS[it]])*total(em3_apost[HS[it]])/total(sa[HS[it]])/total(mass4a[HS[it]])


;fcorr[it] = (mass4e[it]*fprev[it]+mass3e[it]*em3_apost[it]/sa[it])/mass4a[it]
;fcorr = (mass4e*fprev+mass3e*total(em3_apost)/total(sa))/mass4a

;stop
     ENDFOR
;stop
 ;    print, fcorr
  ;   stop
ENDELSE





;IF yyyymm NE sim.syyyymm THEN BEGIN
;  FOR it=0,sim.ntrace-1 DO BEGIN
;       IF fcorr[it] gt fprev[it]+fprev[it]/5. THEN fcorr[it] = fprev[it]
;       IF fcorr[it] lt fprev[it]-fprev[it]/5. THEN fcorr[it] = fprev[it] 
;    IF fcorr[it] gt fprev[it]+fprev[it]/10. THEN fcorr[it] = fprev[it]
;       IF fcorr[it] lt fprev[it]-fprev[it]/10. THEN fcorr[it] = fprev[it]    
;    ENDFOR ;;; FLO: put back this constraint ? 
;ENDIF


fcorr[where(finite(fcorr) eq 0)] = 1.00000 ;flo

;fcorr = startcf ;flo test with constant 1 fcorr
;stop

;print,data.budget[4*sim.ntrace+5:4*sim.ntrace+5+sim.ntrace-1,0,0]


  print,fcorr 
;IF Long(yyyymm) eq (Long(sim.syyyymm)+12L)  THEN stop

END
