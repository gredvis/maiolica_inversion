;+
; NAME:
;
;   run_inversion_final
;
; PURPOSE:
;
;   Runs all necessary inversion programs to carry out an inversion and plot the results
;   using a predefined set of observations and parameters.
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  run_inversion
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
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
;        none
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; FA 25 November 2015
;-

PRO run_inversion_final_DLR

  ;***********************************************
  ; define inversion parameters
  ;***********************************************
       sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'199112',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45],$
         ntrace:48,nage:5} 

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.0,1.0,1.0,1.0],$
         ntrace:48,nage:5} 

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.8,1.8,1.8,1.8],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,0.9,0.9,0.9,0.9],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all , antarctica scaled

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'199312',scaleq:[1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.27,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.65,1.55,1.55,1.55,1.55,1.55,1.55,1.55,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,0.9,0.9,0.9,0.9],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all , tropics scaled

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'199312',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'199312',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.20,0.20,0.20,0.20],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.97,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,1.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask NOT initialised to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.05,0.20,0.20,0.20,0.20,0.20,0.20,0.20],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask NOT initialised to 1

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.70,0.70,0.70,0.70],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.10,0.40,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.20,0.20,0.20,0.20],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.12,0.12,0.12,0.12,0.12,0.12,0.12,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.30,0.30,0.30,0.30],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.10,1.2,1.2,1.2,1.2,1.2,1.2,1.2,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.50,2.50,2.50,2.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert large


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.40,0.40,0.40,0.40],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  32.8000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  26.1000



sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  23.7000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  22.4000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'200901',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  22.4000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198901',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  23.7000 DLR

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'200001',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  NEW DLR

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  23.7000 DLR

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  22.4000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  26.1000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  32.8000

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  NEW DLR 16.4000

;sim = {name:'final_sim01',$
;         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
;         modeldir:'/nas/arf/output/',$
;         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
;         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
;         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.80,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
;         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  23.7000 DLR
;sim.scaleq=sim.scaleq/2

  ; only flask measurements
  flask   = 1 ;1 
  
  ; w/o BKT station
  bkt     = 0
  
  ; non-background conditions only where available
  statfilt = ['brw']
  special  = 0 ;1

  ; global fcorr
  fcorr_glob   = 0 ;1 
  
  
  IF NOT keyword_set(flask) THEN BEGIN
    ; all data included
    ; stats includes data from 34 continuous stations, until 'cgo'
    stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'mhd',   'ngl',$
                   'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                   'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                   'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                   'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                   'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                   'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                   'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey' , 'asc',   'cfa',$
                   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                   'cya',   'syo',   'hba']
                   
    enh     = 1.               
    ufact   = [       1.,      1.,     enh,      1.,     enh,     enh,     enh,      1.,      1.,$
                     enh,      1.,     enh,      1.,     enh,     enh,     enh,      1.,     enh,      1.,$
                      1.,     enh,     enh,      1.,     enh,      1.,      1.,     enh,      1.,      1.,$
                      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,     enh,$
                      1.,     enh,     enh,      1.,      1.,      1.,     enh,     enh,      1.,     enh,$
                     enh,      1.,      1.,      enh,      1.,     enh,      1.,     enh,     enh,$
                      1.,      1.,      1.,      1.,     enh,      1.,      1.,      1.,      1.,     enh,$
                      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                      1.,      1.,      1.]      


nstats = n_elements(stats)
    ufact  = FltArr(nstats)
    ufact[*] = 1.


 ;   ufact[81:90] =100.0   ; TEST less weigth antarctica

    ; TEST less weigth tropics
 ;   ufact[26] =100.0 ;mlo
;ufact[65] =100.0   ;key
;  ufact[70] =100.0   ;gmi
; ufact[72] =100.0   ;chr
;ufact[75] =100.0 ; sey
;ufact[76] =100.0 ;asc 

     ; TEST less weigth except high lat
;ufact[*] =100.0 ;mlo
;ufact[0]=1.0
;ufact[1]=1.0
;ufact[37]=1.0   
;ufact[39]=1.0   ;shm
;ufact[81:90] =1.0                      
    
    IF keyword_set(special) THEN BEGIN
      ; all data included (88 stations)
      ; stats includes data from 34 continuous stations, until 'cgo'
      stats  =   [   'alt',   'brw',   'cdl',   'zgt',   'etl',   'mhd',   'ngl',$
                     'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                     'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                     'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                     'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                     'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                     'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                     'kum',   'cri',   'gmi',   'abp',   'chr',   'mkn',   'sey',   'asc',   'cfa',$
                     'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                     'cya',   'syo',   'hba']
                   
    enh     = 1.               
    ufact   = [       1.,      1.,      1.,     enh,     enh,      1.,      1.,$
                     enh,      1.,     enh,      1.,     enh,     enh,     enh,      1.,     enh,      1.,$
                      1.,     enh,     enh,      1.,     enh,      1.,      1.,     enh,      1.,      1.,$
                      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,     enh,$
                      1.,     enh,     enh,      1.,      1.,      1.,     enh,     enh,      1.,     enh,$
                     enh,      1.,      1.,      enh,      1.,     enh,      1.,     enh,     enh,$
                      1.,      1.,      1.,      1.,     enh,      1.,      1.,      1.,      1.,     enh,$
                      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                      1.,      1.,      1.]                                            
    ENDIF
                             
  ENDIF ELSE BEGIN
    ; flask measurements only
;    stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
;                   'zep',   'sum',$
;                   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
;                   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
;                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
;                   'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
;                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
;                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']

;DLR wo smo,ice,sis,amt (65 stations)
    stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                   'hpb',   'hun',   'puy',   'lef',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                   'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']

    nstats = n_elements(stats)
    ufact  = FltArr(nstats)
    
ufact[*] = 1.

;DLR wo smo,ice,sis,amt and LPO,BSC,WKT removed (62 stations)
    stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',   'cba',   'bal',   'shm',   'oxk', 'esp',$
                   'hpb',   'hun',   'puy',   'lef',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                     'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']


;;;;TEST FLO;;;;

; flask measurements only
 ;   stats  =   [   'cba',   'shm',   'nwr',   'azr', 'key',   'gmi',   'chr', 'brw', 'mlo','sey','maa', 'asc']
 ;   nstats = n_elements(stats)
 ;   ufact  = FltArr(nstats)
 ;   ufact[*] = 1.
;ufact[10] = 100.
;ufact[11] = 100.


                      
  ENDELSE   
                                  
  sn         = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'                 
  print, 'Run inversion with ', sn, ' stations'
 
;IF keyword_set(fcorr_glob) THEN  startcf = 1.0325924 ELSE startcf = [1.00000  ,    1.00000 ,     1.00000   ,   1.00000  ,    1.04629 ,     1.04930   ,   1.00000  ,    1.00000   ,   1.00000,   1.00000 ,     1.17080   ,   1.00000  ,    1.02708  ,    1.00000  ,    1.05412   ,   1.00132  ,    1.04544   ,   1.21240,  1.06390 ,     1.00000    ,  1.24261   ,  0.955105  ,    1.11636    ,  1.00000  ,    1.09041  ,    1.00000    ,  1.00000, 1.03611  ,   0.939701,      1.09191  ,    1.00000  ,   0.909144   ,   1.00000   ,   1.00000 ,     1.39767   ,   1.05893, 1.03421  ,    1.00000   ,  0.907644   ,   1.09670  ,    1.00000  ,    1.00000  ,    1.00000    ,  1.00000    ,  1.08723, 1.00000   ,   1.00000     , 1.00000]

IF keyword_set(fcorr_glob) THEN  startcf = 1.0325924 ELSE startcf = [1.00000  ,    1.00000 ,     1.00000   ,   1.00000  ,    1.04629 ,     1.04930   ,   1.00000  ,    1.00000   ,   1.00000,   1.00000 ,     1.17080   ,   1.00000  ,    1.02708  ,    1.00000  ,    1.05412   ,   1.00132  ,    1.04544   ,   1.21240,  1.06390 ,     1.00000    ,  1.24261   ,  0.955105  ,    1.11636    ,  1.00000  ,    1.09041  ,    1.00000    ,  1.00000, 1.03611  ,   0.939701,      1.09191  ,    1.00000  ,   0.909144   ,   1.00000   ,   1.00000 ,     1.39767   ,   1.05893, 1.03421  ,    1.00000   ,  0.907644   ,   1.09670  ,    1.00000  ,    1.00000  ,    1.00000    ,  1.00000    ,  1.08723, 1.00000   ,   1.00000     , 1.00000]

IF keyword_set(fcorr_glob) THEN  startcf = 1.0325924 ELSE BEGIN 
startcf = FltArr(48)
startcf[*] = 1/0.98871541
startcf[*] =1.0275

startcf[*] = 1.0000

;startcf = [0.990934  ,   0.998725   ,  0.966918  ,   0.970774 ,
;0.969145 ,    0.988575 ,    0.967936 ,    0.989237 ,    0.999264 ,
;1.08774, 0.980201   ,   1.02635 ,    0.990535 ,     1.00026,
;0.995482 ,     1.00000 ,     1.00258   ,  0.988409  ,   0.964153  ,
;0.999978,1.26844  ,    1.12428  ,   0.915826 ,    0.972333  ,
;0.997255  ,   0.954269 ,    0.970943  ,   0.999860  ,   0.954971  ,
;0.996703,   0.987350 ,    0.964997  ,   0.951639  ,   0.989671  ,
;0.958195 ,    0.997164   ,  0.905059  ,   0.898224,     0.961744  ,
;1.03772 , 2.27135 ,    0.882971  ,   0.910372  ,   0.969549    ,
;1.00155  ,   0.983641 ,     1.10581  ,   0.885359]



ENDELSE


;IF keyword_set(fcorr_glob) THEN  startcf = 1.0325924 ELSE startcf =
;[0.750000  ,    1.80000 ,     1.00000   ,   1.00000  ,    1.0000 ,
;1.00000   ,   1.00000  ,    1.00000   ,   1.00000,   1.80000 ,
;0.75000   ,   1.80000  ,    1.0000  ,    1.00000  ,    0.75000   ,
;1.00000  ,    1.0000   ,   1.0000,  0.75000 ,     1.00000    ,
;1.8000   ,  1.8000  ,    0.75000    ,  1.80000  ,    0.75000  ,
;0.75000    ,  1.00000, 1.0000  ,   0.75000,      1.0000  ,    0.75000
;,   1.80000   ,   1.00000   ,   1.00000 ,     0.75000   ,   1.0000,
;1.0000  ,    1.00000   ,  0.75000   ,   1.0000  ,    1.80000  ,
;0.75000  ,    0.75000    ,  1.80000    ,  1.0000, 1.00000   ,
;1.00000     , 1.00000]


;end 1992
;startcf = [0.990934  ,   0.998725   ,  0.966918  ,   0.970774 ,    0.969145 ,    0.988575 ,    0.967936 ,    0.989237 ,    0.999264 ,     1.08774, 0.980201   ,   1.02635 ,    0.990535 ,     1.00026,  0.995482 ,     1.00000 ,     1.00258   ,  0.988409  ,   0.964153  ,   0.999978,1.26844  ,    1.12428  ,   0.915826 ,    0.972333  ,   0.997255  ,   0.954269 ,    0.970943  ,   0.999860  ,   0.954971  ,   0.996703,   0.987350 ,    0.964997  ,   0.951639  ,   0.989671  ,   0.958195 ,    0.997164   ,  0.905059  ,   0.898224,     0.961744  ,    1.03772 , 2.27135 ,    0.882971  ,   0.910372  ,   0.969549    ,  1.00155  ,   0.983641 ,     1.10581  ,   0.885359]


startcf = [0.990934  ,   0.998725   ,  0.966918  ,   0.970774 ,    0.969145 ,    0.988575 ,    0.967936 ,    0.989237 ,    0.999264 ,     1.08774, 0.980201   ,   1.02635 ,    0.990535 ,     1.00026,  0.995482 ,     1.00000 ,     1.00258   ,  0.988409  ,   0.964153  ,   0.999978,1.26844  ,    1.12428  ,   0.915826 ,    0.972333  ,   0.997255  ,   0.954269 ,    0.970943  ,   0.999860  ,   0.954971  ,   0.996703,   0.987350 ,    0.964997  ,   0.951639  ,   0.989671  ,   0.958195 ,    0.997164   ,  0.905059  ,   0.898224,     0.961744  ,    1.03772 , 2.27135 ,    0.882971  ,   0.910372  ,   0.969549    ,  1.00155  ,   0.983641 ,     1.10581  ,   0.885359]


startcf[*] = 1.0000

  ; use weekly means of observational and model data
  weekly  = 1
  
  ; take the logarithm of the emissions
  keeppos = 0 ;1
   
  ; for Barrow: use non-background data only
  brw     = 0
    
  ; activate steps
  step1 = 0   ; step1: compute weekly mean observational data
  step2 = 0   ; step2: compute weekly mean model data 
  step3 = 0   ; step3: compute model-data mismatch first time
  step4 = 0   ; step4: run preliminary inversion to compute aposteriori model-data mismatch
  step5 = 0   ; step5: compute model-data mismatch second time using aposteriori model data
  step6 = 0   ; step6: run final inversion
  step7 = 1   ; step7: run inv_emissions_ratio to determine model estimate separated into ategories
  step8 = 0   ; step8: run plot programs      
  step9 = 0   ; step9: run plot programs 2     
  
 
jahr    = [1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012] 
;jahr    = [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012] 
;jahr    = [1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012] 
;jahr    = [1989,1990,1991]
;jahr    = [2007,2008,2009,2010,2011,2012]

  nyears  = n_elements(jahr)  
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
    
  ;************************************************************************
  ; start program chain:
  ;************************************************************************
  ; 1. compute weekly mean observational data and put out into monthly files
  IF keyword_set(step1) THEN BEGIN
    IF keyword_set(flask) THEN FOR ij=0,nyears-1 DO inv_obsvector_mon_weekly_final,year=jahr[ij],stats=stats,/flask,brw=brw,nobg=nobg $
    ELSE FOR ij=0,nyears-1 DO inv_obsvector_mon_weekly_final,year=jahr[ij],stats=stats,brw=brw,nob=nobg,special=special,statfilt=statfilt,flask=flask
  ENDIF ELSE BEGIN
    print, 'Skipped step1: computing weekly mean observational data'
  ENDELSE  
    
  ; 2. compute weekly mean model data and put out into monthly files
  IF keyword_set(step2) THEN BEGIN
    read_weekly_model_data_all_DLR_final,sim=sim,stats=stats,flask=flask,nobg=nobg,special=special    
  ENDIF ELSE BEGIN
    print, 'Skipped step2: computing weekly mean model data'  
  ENDELSE
  
  IF keyword_set(step3) THEN BEGIN
    print, '3. Run inv_error_diagonal_weekly'
    ; 3. compute observational errors
    inv_error_diagonal_weekly_dlr_final,sim=sim,stats=stats,flask=flask,ufact=ufact,nobg=nobg,special=special
  ENDIF ELSE BEGIN
    print, 'Skipped step3: computing model-data mismatch first time'    
  ENDELSE
  
  ; 4. run preliminary inversion that yields first apriori model estimates with which,
  ;    in the following, improved observational errors can be calculated
  IF keyword_set(step4) THEN BEGIN
    print, '4. Run preliminary inversion'
    hdump    = 1
    rapriori = 1
    inv_run_DLR_final,sim=sim,hdump=hdump,weekly=weekly,keeppos=keeppos,serdllh=serdllh,sumdllh=sumdllh,$
            serzlen=serzlen,sumzlen=sumzlen,flask=flask,rapriori=rapriori,stats=stats,startcf=startcf,nobg=nobg,$
            special=special,nobse=nobse
  ENDIF ELSE BEGIN
    print, 'Skipped step4: running preliminary inversion to compute aposteriori model-data mismatch'      
  ENDELSE
 
  ; 5. compute new observational errors that go into the final inversion
  IF keyword_set(step5) THEN BEGIN
    print, '5. Compute error covariance values again for final inversion'
    inv_error_diagonal_weekly_aposteriori_DLR_final,sim=sim,stats=stats,flask=flask,ufact=ufact,nobg=nobg,special=special,$
                                          startcf=startcf
  ENDIF ELSE BEGIN
    print, 'Skipped step5: computing error covariance values again for final inversion'
  ENDELSE  
  
  ; 6. run inversion second time to obtain final estimates
  IF keyword_set(step6) THEN BEGIN
    print, '6. Run final inversion'
    hdump     = 1
    rapriori  = 0 ;1
    inv_run_DLR_final,sim=sim,hdump=hdump,weekly=weekly,keeppos=keeppos,serdllh=serdllh,sumdllh=sumdllh,$
            serzlen=serzlen,sumzlen=sumzlen,flask=flask,rapriori=rapriori,stats=stats,startcf=startcf,nobg=nobg,$
            special=special,nobse=nobse
  ENDIF ELSE BEGIN
    print, 'Skipped step6: running final inversion'        
  ENDELSE        
  
  ; 7. compute model estimates from final inversion separated in categories
  IF keyword_set(step7) THEN BEGIN
    print, '7. compute a posteriori model estimates divided into categories'
    inv_emissions_ratio_DLR_final,sim=sim,stats=stats,flask=flask,nobg=nobg,special=special
  ENDIF ELSE BEGIN
    print, 'Skipped step7: running inv_emissions_ratio to determine model estimate separated into categories'        
  ENDELSE
  
  print, 'End of process chain'
  
  ;************************ END PROCESSING *************************************************
  
  IF keyword_set(step8) THEN BEGIN
  
  print, 'Plotting now ...'
  
  ; 8. plot anomalies of emissions
;  flask = 1
  rel   = 0
  plot_inv_emissions_anomalies_paper_maiolica_final,sim=sim,stats=stats,flask=flask,rel=rel,nobg=nobg,special=special

 ENDIF
 
   ; 8. plot anomalies of emissions
;  rel   = 1
;  plot_inv_emissions_anomalies_paper,sim=sim,stats=stats,flask=flask,rel=rel,nobg=nobg,special=special
;  flask = 0
  
  ;unsich = 0
  ;plot_inv_testaposteriori_annmean,sim=sim,unsich=unsich,stats=stats  
  
  ;unsich = 1
  ;plot_inv_testaposteriori_annmean,sim=sim,unsich=unsich,stats=stats    
 IF keyword_set(step9) THEN BEGIN
;    9. growth rates  
  plot_station_methane_categories_MAIOLICA,lat=3
  ; 9. plot emissions errors
  ;plot_inv_q,sim=sim,stats=stats
  
  ; 10. plot growth rates
  ; 10a: NH
;  lat = 0
  ;compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg
  
  ; 10b: tropics
;  lat = 1
  ;compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg  
  
  ; 10c: SH
;  lat = 2
  ;compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg  
  
  ; 10d: global    
;  lat = 3
  ;compute_growth_rates_model_categories,sim=sim,lat=lat,dump=dump,stats=stats,flask=flask,nobg=nobg
  
  ENDIF
      
END
