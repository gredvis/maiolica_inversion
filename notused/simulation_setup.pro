;+
; NAME:
;
;   simulation_setup
;
; PURPOSE:
;
;  Create a structure with information about a given simulation.
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;   sim = simulation_setup(sim_name)
;                            
; INPUTS:
;
;   sim_name (string): The name of a simulation. Options are
;        'final_sim01_u23.7'
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; COMMON BLOCKS:
;        none
;
; SIDE EFFECTS:
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   plot_inv_modelestimates
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   brd 6 Jun 2016
;-

FUNCTION simulation_setup,sim_name

  IF n_elements(sim_name) EQ 0 THEN BEGIN
     message,'parameter sim_name missing',/continue
     RETURN,-1
  ENDIF
  
  CASE sim_name OF
     'URMEL_SENSC_II': $
        sim = {name:'URMEL_SENSC_II',$
               obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
               modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
               outdir:'/home/spc134/IDL/urmel/INVERSION/',$
               hdir: '/nas/spc134/URMEL/INVERSION/SENSITIVITIES/',$
               syyyymm:'200002',eyyyymm:'200812',scaleq:[0.10,0.10,0.10,0.5,0.85,0.3,0.35,0.45,0.55,0.95,0.85],$
               ntrace:11,nage:4}
     'final_sim01_01': $
        sim = {name:'final_sim01',$
               obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
               modeldir:'/nas/arf/output/',$
               outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
               hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
               syyyymm:'198902',eyyyymm:'199912',$
               scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.85,0.85,0.50,0.50,0.85,0.50,0.85,$
                       0.85,0.50,0.85,0.85,0.55,0.50,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.35,0.35,0.35,0.35,0.35,$
                       0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.85,0.85,0.85,0.85],$
               ntrace:48,nage:5}
     'final_sim01_02': $   ; 1989 to 2001
        sim = {name:'final_sim01',$
               obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
               modeldir:'/nas/arf/output/',$
               outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
               hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
               syyyymm:'198902',eyyyymm:'200112',$
               scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.50,0.50,0.50,0.50,$
                       0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.30,0.30,0.30,0.30,0.30,0.30,$
                       0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.1,$
                       0.1,0.1,0.1],$
               ntrace:48,nage:5} ;1999
     'final_sim01_03' : $
        sim = {name:'final_sim01',$ ; 1989 to 2004
               obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
               modeldir:'/nas/arf/output/',$
               outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
               hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
               syyyymm:'198902',eyyyymm:'200412',$
               scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,$
                       1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,$
                       0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,$
                       1.45,1.45,1.45],$
               ntrace:48,nage:5} ;1999
     'final_sim01_04' : $
        sim = {name:'final_sim01',$
               obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
               modeldir:'/nas/arf/output/',$
               outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
               hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
               syyyymm:'198902',eyyyymm:'200912',$
               scaleq:[0.950000 ,    0.950000 ,    0.650000,     0.650000 ,    0.950000   ,  $
                       0.950000, 0.450000  ,   0.950000 ,    0.950000   ,  0.950000   ,  $
                       0.950000  ,   0.950000, 0.950000   ,  0.950000 ,    0.95000, 0.950000   ,  $
                       0.00000   ,  0.950000 , 0.950000  ,    0.00000  ,   0.950000   ,   0.00000  ,   $
                       0.950000  ,    0.00000 , 0.950000   ,  0.950000  ,   0.950000   ,  0.950000  ,  $
                       0.950000  ,   0.950000  , 0.950000 ,    0.950000   ,  0.700000 ,    0.950000 ,   $
                       0.950000  ,   0.950000 , 0.850000  ,   0.950000  ,   0.950000  ,   0.800000  ,  $
                       0.950000   ,  0.100000 ,  1.66000  ,    1.66000  ,    1.45000  ,    1.45000  , $
                       1.45000  ,    1.45000],$ ; MAX LLH
               ntrace:48,nage:5} ;1999

;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
;;          syyyymm:'198902',eyyyymm:'201012',scaleq:[0.950000 ,    0.950000 ,    0.650000,     0.650000 ,    0.950000   ,  0.950000, 0.450000  ,   0.950000 ,    0.950000   ,  0.950000   ,  0.950000  ,   0.950000, 0.950000   ,  0.950000 ,    0.95000, 0.950000   ,   0.00000   ,  0.950000 , 0.950000  ,    0.00000  ,   0.950000   ,   0.00000  ,   0.950000  ,    0.00000 , 0.950000   ,  0.950000  ,   0.950000   ,  0.950000  ,   0.950000  ,   0.950000  , 0.950000 ,    0.950000   ,  0.700000 ,    0.950000 ,    0.950000  ,   0.950000 , 0.850000  ,   0.950000  ,   0.950000  ,   0.800000  ,   0.10000   ,  0.950000 ,  1.66000  ,    1.66000  ,    1.45000  ,    1.45000  ,    1.45000  ,    1.45000],$
;;        ntrace:48,nage:5} ;NEW TEST

;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45],$
;;          ntrace:48,nage:5} ;REF with uncert derived from spc

;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45],$
;;          ntrace:48,nage:5} 

;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.4,0.4,0.4,0.4],$
;;          ntrace:48,nage:5} ;keeppos1 change fcorr all


;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
;;          ntrace:48,nage:5} ;keeppos1 change fcorr all, flask only

;; flask=1


;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
;;          ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1


;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20],$
;;          ntrace:48,nage:5} ;keeppos1 change fcorr all flask NOT initialised to 1

;; sim = {name:'final_sim01',$
;;          obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;;          modeldir:'/nas/arf/output/',$
;;          outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;;          hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;;          syyyymm:'198902',eyyyymm:'201212',scaleq:[0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.30,0.30,0.30,0.30],$
;;          ntrace:48,nage:5} ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times


;sim = {name:'final_sim01',$
;         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;         modeldir:'/nas/arf/output/',$
;         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
;         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
;         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30],$
;         ntrace:48,nage:5} ;REF with uncert derived from spc
     'final_sim01_u23.7': $
        sim = {name:'final_sim01',$ ;keeppos0 change fcorr all flask not initialised to 1 and 
                                ; new inv_run sp23 apriori uncert from
                                ; litterature, taking into account diff regions and also
                                ; fact that we optimize each month 4 times  23.7000
               obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
               modeldir:'/nas/arf/output/',$
               outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
               hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
               syyyymm:'198902',eyyyymm:'201212',$
               scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,$
                       0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,$
                       0.8,0.8,0.8,0.8,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,$
                       0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
               ntrace:48,nage:5}
     ELSE: BEGIN
        message,'sim_name '+sim_name+' not known'
        RETURN,-1
     END
  ENDCASE
  
  RETURN,sim

END

