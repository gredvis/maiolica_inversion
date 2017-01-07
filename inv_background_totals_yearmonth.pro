;+
; NAME:
;
;   inv_background_totals_yearmonth
;
; PURPOSE:
;
;   Read totals file to compute the background correction factor f
;   to estimate true background concentration. Needs to be called
;   first hand in the beginning of the actual inversion month.
;
;   Cbg(it) =  C4(it)*f
;   with it         = daily values in month i
;        C4(it)     = daily values of CH4 concentration of
;                     4th age class at time it in month i
;        f          = monthly correction factor
;
;   f = (M41(i-1)*f(i-1)+total(M31(i-1,k)*fs(i-3,k)) )/M40(i)
;   with i          = month index
;        M41(i-1)   = CH4 mass of 4th age class at the end of month i-1
;        M31(i-1,k) = CH4 mass of 3rd age class at the end of month
;                     i-1 and k-th category
;        M40(i)     = CH4 mass of 4th age class at the beginning of month i        
;        fs(i-3,k)  = sp(i-3,k)/sa(i-3,k), sp = aposteriori emissions,
;                     sa = apriori emissions of k-th category  in month i-3,
;                     sp determined from inversion
;
;   For the first three months, when fs is not determined yet, fs is
;   set to 1. For the first month, when f(i-1) cannot be computed iteratively,
;   f(i-1) is assumed to be represented by the global model-obs bias
;   bias(i-1) = CH4mod(i-1)/CH4obs(i-1)
;
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run. Subroutine inversion.
;
; CALLING SEQUENCE:
;
;  inv_background_totals_yearmonth,sim=sim,yyyymm=yyyymm,fprev=fprev,$
;                                  em3_apost=em3_apost,fcorr=fcorr
;
; INPUTS:
;       sim      =  structure with all information about the simulation
;                           {name:'',$     ; simulation name, default is 'URMEL_CTRL_II'
;                            obsdir:'',$   ; observation data directory, default is
;                                          ; '/nas/spc134/URMEL/INVERSION/OBSINPUT/'
;                            modeldir:'',$ ; model output directory, default is
;                                          ; '/nas/spc134/URMEL/FLEXPART80CTP/output/'
;                            syyyymm:'',$  ; year and month of start of inversion
;                            eyyyymm:'',$  ; year and month of end of inversion
;                            scalef_Q:1,$  ; uncertainty class of a priori emissions
;                            ntrace:11,$   ; number of tracers/CH4 sources
;                            nage:4}       ; number of age classes
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
PRO inv_background_totals_yearmonth,sim=sim,yyyymm=yyyymm,fprev=fprev,$
                                         em3_apost=em3_apost,fcorr=fcorr,startcf=startcf

  ncats   = 7  ; 7 different variables to plot 
               ; (mass,em_up,em_field,em_res,OH_loss,OH_gain,chem_loss)
  nspec   = sim.nage * sim.ntrace + 1

  ;********************************************************************************
  ; READ DATA FROM ACTUAL MONTH
  ; mass4a = mass of 4th age class on first day
  ;********************************************************************************
  mass4a = 0.1D  ; initialize mass of 4th age class at beginning of actual month

  read_totals_maiolicaii_final_for_inv,sim=sim,yyyymm=yyyymm,data=data
  s    = size(data.budget) 
  nlen = s[3]
  ;data.budget[1,2,3] 1: tracer, 2: ncats categories, need mass => 0 ,3: time
;  mass4a = total(data.budget[3*sim.ntrace+1:4*sim.ntrace,0,0])
  mass4a = total(data.budget[4*sim.ntrace+5:5*sim.ntrace+4,0,0]) ;flo
  ;********************************************************************************
  ; READ DATA FROM PRECEDING MONTH
  ; mass4e = mass of 4th age class on last day of preceding month
  ; mass3e = mass of 3rd age class on last day of preceding month
  ;********************************************************************************
  ;; previous month
  pyyyymm = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')-10),0,6)

  mass4e = 0L
  mass3e = 0L

  read_totals_maiolicaii_final_for_inv,sim=sim,yyyymm=pyyyymm,data=data
  s    = size(data.budget) 
  nlen = s[3]
  ;data.budget[1,2,3] 1: tracer, 2: ncats categories, need mass => 0 ,3: time
;  mass4e = total(data.budget[3*sim.ntrace+1:4*sim.ntrace,0,nlen-1])

; mass4e = total(data.budget[4*sim.ntrace+1:5*sim.ntrace,0,nlen-1]) ;flo
mass4e = total(data.budget[4*sim.ntrace+5:5*sim.ntrace+4,0,nlen-1]) ;flo2 due to MCF

  ;FOR it=0,ntrace-1 DO mass3e[it] = data.budget[23+it,0,nlen-1]
;  mass3e = total(data.budget[2*sim.ntrace+1:3*sim.ntrace,0,nlen-1])
;mass3e = total(data.budget[3*sim.ntrace+1:4*sim.ntrace,0,nlen-1]);flo
mass3e = total(data.budget[3*sim.ntrace+4:4*sim.ntrace+3,0,nlen-1]);flo2

  ;********************************************************************************
  ; READ APRIORI EMISSIONS FROM MONTH i-3
  ;********************************************************************************  
  ; set keyword m3: get apriori emissions from three months prior the actual month
  read_emissions_final,yyyymm=yyyymm,/m3,sa=sa

  ;********************************************************************************
  ; CALCULATE BACKGROUND CORRECTION FACTOR
  ;********************************************************************************   
  ;f(i) = (M41(i-1)*f(i-1)+total(M31(i-1,k)*fs(i-3,k)) )/M40(i)

  ; spinup background correction factors
  ; for the first three months of the inversion
  IF yyyymm EQ sim.syyyymm THEN BEGIN
   ;  fcorr = 1.025
     IF keyword_set(startcf) THEN fcorr = startcf

 ;  ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) or $
 ;    Long(yyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN

ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+1L) or $
     Long(yyyymm) eq (Long(sim.syyyymm)+2L)  THEN BEGIN

     ;;fcorr = (mass4e*fprev+total(mass3e[*]))/mass4a
     fcorr = (mass4e*fprev+mass3e)/mass4a
     ;print, yyyymm, ' ', mass4e*fprev/mass4a, ' ', mass3e/mass4a

ENDIF ELSE IF Long(yyyymm) eq (Long(sim.syyyymm)+3L) THEN BEGIN ;NEW

    
     fcorr = (mass4e*fprev+mass3e)/mass4a  ;NEW
   

  ENDIF ELSE BEGIN
     ;;fcorr = (mass4e*fprev+total(mass3e[*]*em3_apost[*]/sa[*]))/mass4a
     fcorr = (mass4e*fprev+mass3e*total(em3_apost)/total(sa))/mass4a
     ;print, yyyymm, ' ', mass4e*fprev/mass4a, ' ', total(em3_apost)/total(sa), ' ', mass3e/mass4a, ' ',$
     ;       total(em3_apost)/total(sa)*mass3e/mass4a     
     
     IF finite(fcorr) eq 0 THEN stop
     print, yyyymm, ' ', fcorr
     
  ENDELSE
stop
END
