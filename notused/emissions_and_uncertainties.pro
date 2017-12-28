;+
; NAME:
;
;   plot_inv_testaposteriori
;
; PURPOSE:
;
;   Plot first, second and third aposteriori estimates and apriori
;   for chosen category and year
;   ;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  plot_inv_testaposteriori,sim=sim,year=year,cat=cat
;                            
; INPUTS:
;       sim    (structure): simulation information
;       year      (string): year to plot
;       cat      (integer): category to plot
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
;   plot_inv_testaposteriori
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 03 July 2012
;-
;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO emissions_and_uncertainties,sim=sim,year=year,cat=cat

  weekly = 1

  IF n_elements(sim) EQ 0 THEN BEGIN
;     sim = {name:'URMEL_CTRL_II',$
;          obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
;          modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
;            outdir:'/home/spc134/IDL/urmel/INVERSION/',$
;             hdir: '/nas/spc134/URMEL/INVERSION/SENSITIVITIES/',$
;          syyyymm:'200002',eyyyymm:'200812',scaleq:[0.25,0.25,0.25,0.75,0.75,0.5,0.3,0.6,0.5,0.9,0.4],$
;          ntrace:11,nage:4}

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'199912',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30],$
         ntrace:48,nage:5}

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'199912',scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.85,0.85,0.50,0.50,0.85,0.50,0.85,0.85,0.50,0.85,0.85,0.55,0.50,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.85,0.85,0.85,0.85],$
         ntrace:48,nage:5}

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200112',scaleq:[0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.1,0.1,0.1,0.1],$
         ntrace:48,nage:5} 


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200612',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,1.65,1.65,1.05,1.65,1.65,1.05,1.05,1.65,1.05,1.65,1.65,1.65,1.65,0.55,0.55,0.55,0.55,0.55,0.55,0.55,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.66,1.45,1.45,1.45,1.45],$
         ntrace:48,nage:5} ;1999

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'198902',eyyyymm:'200612',scaleq:[0.950000 ,    0.950000 ,    0.650000,     0.650000 ,    0.950000   ,  0.950000, 0.450000  ,   0.950000 ,    0.950000   ,  0.950000   ,  0.950000  ,   0.950000, 0.950000   ,  0.950000 ,    0.95000, 0.950000   ,   0.00000   ,  0.950000 , 0.950000  ,    0.00000  ,   0.950000   ,   0.00000  ,   0.950000  ,    0.00000 , 0.950000   ,  0.950000  ,   0.950000   ,  0.950000  ,   0.950000  ,   0.950000  , 0.950000 ,    0.950000   ,  0.700000 ,    0.950000 ,    0.950000  ,   0.950000 , 0.850000  ,   0.950000  ,   0.950000  ,   0.800000  ,   0.950000   ,  0.100000 ,  1.66000  ,    1.66000  ,    1.45000  ,    1.45000  ,    1.45000  ,    1.45000],$ ; MAX LLH
ntrace:48,nage:5} ;1999

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
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised not to 1


sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'198902',eyyyymm:'201212',scaleq:[0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.27,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.70,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50],$
         ntrace:48,nage:5} ;keeppos1 change fcorr all flask initialised  to 1 and new inv_run sp23

  ENDIF

  ;*************
  ; DIRECTORIES
  ;*************
  obsdir   = sim.obsdir
  sensdir  = sim.hdir
  modeldir = sim.modeldir+sim.name+'/'

  ;**********************************
  ; GENERAL PARAMETERS AND VARIABLES
  ;**********************************
  mdays    = [31,28,31,30,31,30,31,31,30,31,30,31]
  nmonths  = 12
;  syyyy    = 2001 & smm = 1
  syyyy    = 1990 & smm = 1
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  m        = (eyyyy-syyyy+1)*12+11
  ;m        = eyyyy*12L+emm-(2001*12L+2)+1
  n        = (eyyyy-syyyy+1)*12


  ;*************** 
  ; evaluate file
  ;***************  
  testfile = sim.outdir+'inv_output_weekly'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+sim.qunc+'.txt'
  testfile = sim.outdir+'inv_output_weekly_flask_69stats_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+sim.qunc+'_nov12.txt' ;flo
;testfile = sim.outdir+'inv_output_weekly_91stats_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+sim.qunc+'_nov12.txt' ;flo
  
fhelp    = DblArr(m,sim.ntrace)
  sahelp   = DblArr(m,sim.ntrace)
  sphelp   = DblArr(m,sim.ntrace)
  sp1help  = DblArr(m,sim.ntrace)
  sp2help  = DblArr(m,sim.ntrace)
  sp3help  = DblArr(m,sim.ntrace)
  qphelp   = DblArr(m,sim.ntrace)
  qp1help  = DblArr(m,sim.ntrace)  
  qp2help  = DblArr(m,sim.ntrace)
  qp3help  = DblArr(m,sim.ntrace)

  fcorr    = DblArr(n,sim.ntrace)
  sa       = DblArr(n,sim.ntrace)
  qa       = DblArr(n,sim.ntrace)
  sp       = DblArr(n,sim.ntrace)
  sp1      = DblArr(n,sim.ntrace)
  sp2      = DblArr(n,sim.ntrace)
  sp3      = DblArr(n,sim.ntrace)
  qp       = DblArr(n,sim.ntrace)
  qp1      = DblArr(n,sim.ntrace)
  qp2      = DblArr(n,sim.ntrace)
  qp3      = DblArr(n,sim.ntrace)

  openr,lun,testfile,/get_lun
  readf,lun,fhelp
  readf,lun,sahelp
  readf,lun,sphelp
  readf,lun,sp1help
  readf,lun,sp2help   
  readf,lun,sp3help   
  readf,lun,qphelp
  readf,lun,qp1help
  readf,lun,qp2help 
  readf,lun,qp3help 
;stop
  free_lun,lun
  
  ; only evaluate from 2002 on. 2001 is spin up year of inversion
  fcorr[*,*] = fhelp[11:m-1,*]
  
  FOR it=0,sim.ntrace-1 DO BEGIN
    sa[*,it]  = sahelp[11:m-1,it]
    sp[*,it]  = sphelp[11:m-1,it]
    sp1[*,it] = sp1help[11:m-1,it]
    sp2[*,it] = sp2help[11:m-1,it]
    sp3[*,it] = sp3help[11:m-1,it]
    qa[*,it]  = sim.scaleq[it]
    qp[*,it]  = qphelp[11:m-1,it]
    qp1[*,it] = qp1help[11:m-1,it]
    qp2[*,it] = qp2help[11:m-1,it]   
    qp3[*,it] = qp3help[11:m-1,it]   

      

  ENDFOR
 ;stop
  
  ; select data for one year and category for plotting
  nser       = 5
  emissplot  = DblArr(12,nser)
  uncertplot = DblArr(12,nser)
  sdate      = (fix(year)-1991)*12
 ; IF cat eq 0 OR cat eq 1 OR cat eq 2 THEN BEGIN
     IF cat eq 48 THEN BEGIN
    emissplot[*,0] = sa[sdate:(sdate+11),0]+sa[sdate:(sdate+11),1]+sa[sdate:(sdate+11),2]+sa[sdate:(sdate+11),3]+sa[sdate:(sdate+11),4]+sa[sdate:(sdate+11),5]+sa[sdate:(sdate+11),6]+sa[sdate:(sdate+11),7]+sa[sdate:(sdate+11),8]+sa[sdate:(sdate+11),9]+sa[sdate:(sdate+11),10]
    emissplot[*,1] = sp1[sdate:(sdate+11),0]+sp1[sdate:(sdate+11),1]+sp1[sdate:(sdate+11),2]+sp1[sdate:(sdate+11),3]+sp1[sdate:(sdate+11),4]+sp1[sdate:(sdate+11),5]+sp1[sdate:(sdate+11),6]+sp1[sdate:(sdate+11),7]+sp1[sdate:(sdate+11),8]+sp1[sdate:(sdate+11),9]+sp1[sdate:(sdate+11),10]
    emissplot[*,2] = sp2[sdate:(sdate+11),0]+sp2[sdate:(sdate+11),1]+sp2[sdate:(sdate+11),2]+sp2[sdate:(sdate+11),3]+sp2[sdate:(sdate+11),4]+sp2[sdate:(sdate+11),5]+sp2[sdate:(sdate+11),6]+sp2[sdate:(sdate+11),7]+sp2[sdate:(sdate+11),8]+sp2[sdate:(sdate+11),9]+sp2[sdate:(sdate+11),10]
    emissplot[*,3] = sp3[sdate:(sdate+11),0]+sp3[sdate:(sdate+11),1]+sp3[sdate:(sdate+11),2]+sp3[sdate:(sdate+11),3]+sp3[sdate:(sdate+11),4]+sp3[sdate:(sdate+11),5]+sp3[sdate:(sdate+11),6]+sp3[sdate:(sdate+11),7]+sp3[sdate:(sdate+11),8]+sp3[sdate:(sdate+11),9]+sp3[sdate:(sdate+11),10]
    emissplot[*,4] = sp[sdate:(sdate+11),0]+sp[sdate:(sdate+11),1]+sp[sdate:(sdate+11),2]+sp[sdate:(sdate+11),3]+sp[sdate:(sdate+11),4]+sp[sdate:(sdate+11),5]+sp[sdate:(sdate+11),6]+sp[sdate:(sdate+11),7]+sp[sdate:(sdate+11),8]+sp[sdate:(sdate+11),9]+sp[sdate:(sdate+11),10]

 
    uncertplot[*,0] = sqrt(qa[sdate:(sdate+11),0]^2+qa[sdate:(sdate+11),1]^2+qa[sdate:(sdate+11),2]^2+qa[sdate:(sdate+11),3]^2+qa[sdate:(sdate+11),4]^2+qa[sdate:(sdate+11),5]^2+qa[sdate:(sdate+11),6]^2+qa[sdate:(sdate+11),7]^2+qa[sdate:(sdate+11),8]^2+qa[sdate:(sdate+11),9]^2+qa[sdate:(sdate+11),10]^2)*100.
    uncertplot[*,1] = sqrt(qp1[sdate:(sdate+11),0]^2+qp1[sdate:(sdate+11),1]^2+qp1[sdate:(sdate+11),2]^2+qp1[sdate:(sdate+11),3]^2+qp1[sdate:(sdate+11),4]^2+qp1[sdate:(sdate+11),5]^2+qp1[sdate:(sdate+11),6]^2+qp1[sdate:(sdate+11),7]^2+qp1[sdate:(sdate+11),8]^2+qp1[sdate:(sdate+11),9]^2+qp1[sdate:(sdate+11),10]^2)*100.
    uncertplot[*,2] = sqrt(qp2[sdate:(sdate+11),0]^2+qp2[sdate:(sdate+11),1]^2+qp2[sdate:(sdate+11),2]^2+qp2[sdate:(sdate+11),3]^2+qp2[sdate:(sdate+11),4]^2+qp2[sdate:(sdate+11),5]^2+qp2[sdate:(sdate+11),6]^2+qp2[sdate:(sdate+11),7]^2+qp2[sdate:(sdate+11),8]^2+qp2[sdate:(sdate+11),9]^2+qp2[sdate:(sdate+11),10]^2)*100.
    uncertplot[*,3] = sqrt(qp3[sdate:(sdate+11),0]^2+qp3[sdate:(sdate+11),1]^2+qp3[sdate:(sdate+11),2]^2+qp3[sdate:(sdate+11),3]^2+qp3[sdate:(sdate+11),4]^2+qp3[sdate:(sdate+11),5]^2+qp3[sdate:(sdate+11),6]^2+qp3[sdate:(sdate+11),7]^2+qp3[sdate:(sdate+11),8]^2+qp3[sdate:(sdate+11),9]^2+qp3[sdate:(sdate+11),10]^2)*100.
    uncertplot[*,4] = sqrt(qp[sdate:(sdate+11),0]^2+qp[sdate:(sdate+11),1]^2+qp[sdate:(sdate+11),2]^2+qp[sdate:(sdate+11),3]^2+qp[sdate:(sdate+11),4]^2+qp[sdate:(sdate+11),5]^2+qp[sdate:(sdate+11),6]^2+qp[sdate:(sdate+11),7]^2+qp[sdate:(sdate+11),8]^2+qp[sdate:(sdate+11),9]^2+qp[sdate:(sdate+11),10]^2)*100.
  ENDIF 

IF cat eq 49 THEN BEGIN
    emissplot[*,0] = sa[sdate:(sdate+11),11]+sa[sdate:(sdate+11),12]+sa[sdate:(sdate+11),13]+sa[sdate:(sdate+11),14]+sa[sdate:(sdate+11),15]+sa[sdate:(sdate+11),16]+sa[sdate:(sdate+11),17]+sa[sdate:(sdate+11),18]+sa[sdate:(sdate+11),19]+sa[sdate:(sdate+11),20]+sa[sdate:(sdate+11),21]+sa[sdate:(sdate+11),22]+sa[sdate:(sdate+11),23]
    emissplot[*,1] = sp1[sdate:(sdate+11),11]+sp1[sdate:(sdate+11),12]+sp1[sdate:(sdate+11),13]+sp1[sdate:(sdate+11),14]+sp1[sdate:(sdate+11),15]+sp1[sdate:(sdate+11),16]+sp1[sdate:(sdate+11),17]+sp1[sdate:(sdate+11),18]+sp1[sdate:(sdate+11),19]+sp1[sdate:(sdate+11),20]+sp1[sdate:(sdate+11),21]+sp1[sdate:(sdate+11),22]+sp1[sdate:(sdate+11),23]
    emissplot[*,2] = sp2[sdate:(sdate+11),11]+sp2[sdate:(sdate+11),12]+sp2[sdate:(sdate+11),13]+sp2[sdate:(sdate+11),14]+sp2[sdate:(sdate+11),15]+sp2[sdate:(sdate+11),16]+sp2[sdate:(sdate+11),17]+sp2[sdate:(sdate+11),18]+sp2[sdate:(sdate+11),19]+sp2[sdate:(sdate+11),20]+sp2[sdate:(sdate+11),21]+sp2[sdate:(sdate+11),22]+sp2[sdate:(sdate+11),23]
    emissplot[*,3] = sp3[sdate:(sdate+11),11]+sp3[sdate:(sdate+11),12]+sp3[sdate:(sdate+11),13]+sp3[sdate:(sdate+11),14]+sp3[sdate:(sdate+11),15]+sp3[sdate:(sdate+11),16]+sp3[sdate:(sdate+11),17]+sp3[sdate:(sdate+11),18]+sp3[sdate:(sdate+11),19]+sp3[sdate:(sdate+11),20]+sp3[sdate:(sdate+11),21]+sp3[sdate:(sdate+11),22]+sp3[sdate:(sdate+11),23]
    emissplot[*,4] = sp[sdate:(sdate+11),11]+sp[sdate:(sdate+11),12]+sp[sdate:(sdate+11),13]+sp[sdate:(sdate+11),14]+sp[sdate:(sdate+11),15]+sp[sdate:(sdate+11),16]+sp[sdate:(sdate+11),17]+sp[sdate:(sdate+11),18]+sp[sdate:(sdate+11),19]+sp[sdate:(sdate+11),20]+sp[sdate:(sdate+11),21]+sp[sdate:(sdate+11),22]+sp[sdate:(sdate+11),23]

    uncertplot[*,0] = sqrt(qa[sdate:(sdate+11),11]^2+qa[sdate:(sdate+11),12]^2+qa[sdate:(sdate+11),13]^2+qa[sdate:(sdate+11),14]^2+qa[sdate:(sdate+11),15]^2+qa[sdate:(sdate+11),16]^2+qa[sdate:(sdate+11),17]^2+qa[sdate:(sdate+11),18]^2+qa[sdate:(sdate+11),19]^2+qa[sdate:(sdate+11),20]^2+qa[sdate:(sdate+11),21]^2+qa[sdate:(sdate+11),22]^2+qa[sdate:(sdate+11),23]^2)*100.

uncertplot[*,1] = sqrt(qp1[sdate:(sdate+11),11]^2+qp1[sdate:(sdate+11),12]^2+qp1[sdate:(sdate+11),13]^2+qp1[sdate:(sdate+11),14]^2+qp1[sdate:(sdate+11),15]^2+qp1[sdate:(sdate+11),16]^2+qp1[sdate:(sdate+11),17]^2+qp1[sdate:(sdate+11),18]^2+qp1[sdate:(sdate+11),19]^2+qp1[sdate:(sdate+11),20]^2+qp1[sdate:(sdate+11),21]^2+qp1[sdate:(sdate+11),22]^2+qp1[sdate:(sdate+11),23]^2)*100.

uncertplot[*,2] = sqrt(qp2[sdate:(sdate+11),11]^2+qp2[sdate:(sdate+11),12]^2+qp2[sdate:(sdate+11),13]^2+qp2[sdate:(sdate+11),14]^2+qp2[sdate:(sdate+11),15]^2+qp2[sdate:(sdate+11),16]^2+qp2[sdate:(sdate+11),17]^2+qp2[sdate:(sdate+11),18]^2+qp2[sdate:(sdate+11),19]^2+qp2[sdate:(sdate+11),20]^2+qp2[sdate:(sdate+11),21]^2+qp2[sdate:(sdate+11),22]^2+qp2[sdate:(sdate+11),23]^2)*100.

uncertplot[*,3] = sqrt(qp3[sdate:(sdate+11),11]^2+qp3[sdate:(sdate+11),12]^2+qp3[sdate:(sdate+11),13]^2+qp3[sdate:(sdate+11),14]^2+qp3[sdate:(sdate+11),15]^2+qp3[sdate:(sdate+11),16]^2+qp3[sdate:(sdate+11),17]^2+qp3[sdate:(sdate+11),18]^2+qp3[sdate:(sdate+11),19]^2+qp3[sdate:(sdate+11),20]^2+qp3[sdate:(sdate+11),21]^2+qp3[sdate:(sdate+11),22]^2+qp3[sdate:(sdate+11),23]^2)*100.

uncertplot[*,4] = sqrt(qp[sdate:(sdate+11),11]^2+qp[sdate:(sdate+11),12]^2+qp[sdate:(sdate+11),13]^2+qp[sdate:(sdate+11),14]^2+qp[sdate:(sdate+11),15]^2+qp[sdate:(sdate+11),16]^2+qp[sdate:(sdate+11),17]^2+qp[sdate:(sdate+11),18]^2+qp[sdate:(sdate+11),19]^2+qp[sdate:(sdate+11),20]^2+qp[sdate:(sdate+11),21]^2+qp[sdate:(sdate+11),22]^2+qp[sdate:(sdate+11),23]^2)*100.



ENDIF


IF cat eq 50 THEN BEGIN
    emissplot[*,0] = sa[sdate:(sdate+11),24]+sa[sdate:(sdate+11),25]+sa[sdate:(sdate+11),26]+sa[sdate:(sdate+11),27]+sa[sdate:(sdate+11),28]+sa[sdate:(sdate+11),29]+sa[sdate:(sdate+11),30]
    emissplot[*,1] = sp1[sdate:(sdate+11),24]+sp1[sdate:(sdate+11),25]+sp1[sdate:(sdate+11),26]+sp1[sdate:(sdate+11),27]+sp1[sdate:(sdate+11),28]+sp1[sdate:(sdate+11),29]+sp1[sdate:(sdate+11),30]
    emissplot[*,2] = sp2[sdate:(sdate+11),24]+sp2[sdate:(sdate+11),25]+sp2[sdate:(sdate+11),26]+sp2[sdate:(sdate+11),27]+sp2[sdate:(sdate+11),28]+sp2[sdate:(sdate+11),29]+sp2[sdate:(sdate+11),30]
    emissplot[*,3] = sp3[sdate:(sdate+11),24]+sp3[sdate:(sdate+11),25]+sp3[sdate:(sdate+11),26]+sp3[sdate:(sdate+11),27]+sp3[sdate:(sdate+11),28]+sp3[sdate:(sdate+11),29]+sp3[sdate:(sdate+11),30]
    emissplot[*,4] = sp[sdate:(sdate+11),24]+sp[sdate:(sdate+11),25]+sp[sdate:(sdate+11),26]+sp[sdate:(sdate+11),27]+sp[sdate:(sdate+11),28]+sp[sdate:(sdate+11),29]+sp[sdate:(sdate+11),30]

    uncertplot[*,0] = sqrt(qa[sdate:(sdate+11),11]^2+qa[sdate:(sdate+11),12]^2+qa[sdate:(sdate+11),13]^2+qa[sdate:(sdate+11),14]^2+qa[sdate:(sdate+11),15]^2+qa[sdate:(sdate+11),16]^2+qa[sdate:(sdate+11),17]^2+qa[sdate:(sdate+11),18]^2+qa[sdate:(sdate+11),19]^2+qa[sdate:(sdate+11),20]^2+qa[sdate:(sdate+11),21]^2+qa[sdate:(sdate+11),22]^2+qa[sdate:(sdate+11),23]^2)*100.

uncertplot[*,1] = sqrt(qp1[sdate:(sdate+11),24]^2+qp1[sdate:(sdate+11),25]^2+qp1[sdate:(sdate+11),26]^2+qp1[sdate:(sdate+11),27]^2+qp1[sdate:(sdate+11),28]^2+qp1[sdate:(sdate+11),29]^2+qp1[sdate:(sdate+11),30]^2)*100.

uncertplot[*,2] = sqrt(qp2[sdate:(sdate+11),24]^2+qp2[sdate:(sdate+11),25]^2+qp2[sdate:(sdate+11),26]^2+qp2[sdate:(sdate+11),27]^2+qp2[sdate:(sdate+11),28]^2+qp2[sdate:(sdate+11),29]^2+qp2[sdate:(sdate+11),30]^2)*100.

uncertplot[*,3] = sqrt(qp3[sdate:(sdate+11),24]^2+qp3[sdate:(sdate+11),25]^2+qp3[sdate:(sdate+11),26]^2+qp3[sdate:(sdate+11),27]^2+qp3[sdate:(sdate+11),28]^2+qp3[sdate:(sdate+11),29]^2+qp3[sdate:(sdate+11),30]^2)*100.

uncertplot[*,4] = sqrt(qp[sdate:(sdate+11),24]^2+qp[sdate:(sdate+11),25]^2+qp[sdate:(sdate+11),26]^2+qp[sdate:(sdate+11),27]^2+qp[sdate:(sdate+11),28]^2+qp[sdate:(sdate+11),29]^2+qp[sdate:(sdate+11),30]^2)*100.



ENDIF

IF cat eq 51 THEN BEGIN
    emissplot[*,0] = sa[sdate:(sdate+11),31]+sa[sdate:(sdate+11),32]+sa[sdate:(sdate+11),33]+sa[sdate:(sdate+11),34]+sa[sdate:(sdate+11),35]+sa[sdate:(sdate+11),36]+sa[sdate:(sdate+11),37]+sa[sdate:(sdate+11),38]+sa[sdate:(sdate+11),39]+sa[sdate:(sdate+11),40]+sa[sdate:(sdate+11),41]+sa[sdate:(sdate+11),42]+sa[sdate:(sdate+11),43]
    emissplot[*,1] = sp1[sdate:(sdate+11),31]+sp1[sdate:(sdate+11),32]+sp1[sdate:(sdate+11),33]+sp1[sdate:(sdate+11),34]+sp1[sdate:(sdate+11),35]+sp1[sdate:(sdate+11),36]+sp1[sdate:(sdate+11),37]+sp1[sdate:(sdate+11),38]+sp1[sdate:(sdate+11),39]+sp1[sdate:(sdate+11),40]+sp1[sdate:(sdate+11),41]+sp1[sdate:(sdate+11),42]+sp1[sdate:(sdate+11),43]
    emissplot[*,2] = sp2[sdate:(sdate+11),31]+sp2[sdate:(sdate+11),32]+sp2[sdate:(sdate+11),33]+sp2[sdate:(sdate+11),34]+sp2[sdate:(sdate+11),35]+sp2[sdate:(sdate+11),36]+sp2[sdate:(sdate+11),37]+sp2[sdate:(sdate+11),38]+sp2[sdate:(sdate+11),39]+sp2[sdate:(sdate+11),40]+sp2[sdate:(sdate+11),41]+sp2[sdate:(sdate+11),42]+sp2[sdate:(sdate+11),43]
    emissplot[*,3] = sp3[sdate:(sdate+11),31]+sp3[sdate:(sdate+11),32]+sp3[sdate:(sdate+11),33]+sp3[sdate:(sdate+11),34]+sp3[sdate:(sdate+11),35]+sp3[sdate:(sdate+11),36]+sp3[sdate:(sdate+11),37]+sp3[sdate:(sdate+11),38]+sp3[sdate:(sdate+11),39]+sp3[sdate:(sdate+11),40]+sp3[sdate:(sdate+11),41]+sp3[sdate:(sdate+11),42]+sp3[sdate:(sdate+11),43]
    emissplot[*,4] = sp[sdate:(sdate+11),31]+sp[sdate:(sdate+11),32]+sp[sdate:(sdate+11),33]+sp[sdate:(sdate+11),34]+sp[sdate:(sdate+11),35]+sp[sdate:(sdate+11),36]+sp[sdate:(sdate+11),37]+sp[sdate:(sdate+11),38]+sp[sdate:(sdate+11),39]+sp[sdate:(sdate+11),40]+sp[sdate:(sdate+11),41]+sp[sdate:(sdate+11),42]+sp[sdate:(sdate+11),43]

    uncertplot[*,0] = sqrt(qa[sdate:(sdate+11),31]^2+qa[sdate:(sdate+11),32]^2+qa[sdate:(sdate+11),33]^2+qa[sdate:(sdate+11),34]^2+qa[sdate:(sdate+11),35]^2+qa[sdate:(sdate+11),36]^2+qa[sdate:(sdate+11),37]^2+qa[sdate:(sdate+11),38]^2+qa[sdate:(sdate+11),39]^2+qa[sdate:(sdate+11),40]^2+qa[sdate:(sdate+11),41]^2+qa[sdate:(sdate+11),42]^2+qa[sdate:(sdate+11),43]^2)*100.

uncertplot[*,1] = sqrt(qp1[sdate:(sdate+11),31]^2+qp1[sdate:(sdate+11),32]^2+qp1[sdate:(sdate+11),33]^2+qp1[sdate:(sdate+11),34]^2+qp1[sdate:(sdate+11),35]^2+qp1[sdate:(sdate+11),36]^2+qp1[sdate:(sdate+11),37]^2+qp1[sdate:(sdate+11),38]^2+qp1[sdate:(sdate+11),39]^2+qp1[sdate:(sdate+11),40]^2+qp1[sdate:(sdate+11),41]^2+qp1[sdate:(sdate+11),42]^2+qp1[sdate:(sdate+11),43]^2)*100.

uncertplot[*,2] = sqrt(qp2[sdate:(sdate+11),31]^2+qp2[sdate:(sdate+11),32]^2+qp2[sdate:(sdate+11),33]^2+qp2[sdate:(sdate+11),34]^2+qp2[sdate:(sdate+11),35]^2+qp2[sdate:(sdate+11),36]^2+qp2[sdate:(sdate+11),37]^2+qp2[sdate:(sdate+11),38]^2+qp2[sdate:(sdate+11),39]^2+qp2[sdate:(sdate+11),40]^2+qp2[sdate:(sdate+11),41]^2+qp2[sdate:(sdate+11),42]^2+qp2[sdate:(sdate+11),43]^2)*100.

uncertplot[*,3] = sqrt(qp3[sdate:(sdate+11),31]^2+qp3[sdate:(sdate+11),32]^2+qp3[sdate:(sdate+11),33]^2+qp3[sdate:(sdate+11),34]^2+qp3[sdate:(sdate+11),35]^2+qp3[sdate:(sdate+11),36]^2+qp3[sdate:(sdate+11),37]^2+qp3[sdate:(sdate+11),38]^2+qp3[sdate:(sdate+11),39]^2+qp3[sdate:(sdate+11),40]^2+qp3[sdate:(sdate+11),41]^2+qp3[sdate:(sdate+11),42]^2+qp3[sdate:(sdate+11),43]^2)*100.

uncertplot[*,4] = sqrt(qp[sdate:(sdate+11),31]^2+qp[sdate:(sdate+11),32]^2+qp[sdate:(sdate+11),33]^2+qp[sdate:(sdate+11),34]^2+qp[sdate:(sdate+11),35]^2+qp[sdate:(sdate+11),36]^2+qp[sdate:(sdate+11),37]^2+qp[sdate:(sdate+11),38]^2+qp[sdate:(sdate+11),39]^2+qp[sdate:(sdate+11),40]^2+qp[sdate:(sdate+11),41]^2+qp[sdate:(sdate+11),42]^2+qp[sdate:(sdate+11),43]^2)*100.



ENDIF 

IF cat lt 48 THEN BEGIN
  
    emissplot[*,0] = sa[sdate:(sdate+11),cat]
    emissplot[*,1] = sp1[sdate:(sdate+11),cat]
    emissplot[*,2] = sp2[sdate:(sdate+11),cat]
    emissplot[*,3] = sp3[sdate:(sdate+11),cat]
    emissplot[*,4] = sp[sdate:(sdate+11),cat]
    
    uncertplot[*,0] = qa[sdate:(sdate+11),cat]*100.
    uncertplot[*,1] = qp1[sdate:(sdate+11),cat]*100.
    uncertplot[*,2] = qp2[sdate:(sdate+11),cat]*100.
    uncertplot[*,3] = qp3[sdate:(sdate+11),cat]*100.
    uncertplot[*,4] = qp[sdate:(sdate+11),cat]*100.
  ENDIF      
 
  emissplot=emissplot*365./1.e9  
  
  ;*******************
  ;* PLOT TIME SERIES
  ;*******************
  plotdir = '/home/arf/pers/IDL/EPS/URMEL/INVERSION/'
  name = ['ANTH','ANTH','ANTH','BBTROP','BBEXTRA','RICE','WETLINUND','WETSOILS','PEATNA','PEATEUR',$
          'OTHERNAT']
;  name = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA','ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
;          'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS','BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
;'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_EU','WETL_MID','WETL_NAbor','WETL_NAFR','WETL_RUS','WETL_SAtemp','WETL_SAFR','WETL_CHIN','WETL_IND',$
;'WETL_NAtemp','WETL_SAtrop','WETL_SE_ASIA','wild_anim','termites','ocean','volcanic','ANTH','BB','RICE','WETL']

 name = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA','ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
          'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS','BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_CHIN','WETL_EU','WETL_IND','WETL_MID','WETL_NAbor','WETL_NAtemp','WETL_NAFR','WETL_RUS','WETL_SAtemp',$
'WETL_SAtrop','WETL_SE_asia','WETL_SAfr','WILD_anim','TERMITES','OCEAN','Volc','Anthropogenic','Biomass Burning','Rice','Wetland']




  plotfile = plotdir+'ts_inv_'+sim.name+'_'+name[cat]+'_testaposteriori_'+year+'_'+sim.qunc+'.eps'
  load_ctb,'/home/arf/pers/IDL/GEOP/diff3.ctb'
  open_ps,plotfile,pssize=[20,15],/eps,/color

  !P.BACKGROUND=0
  !P.COLOR=255
  !P.FONT=1

  ytitle1 = 'Emissions estimate (Tg/yr)'
  ytitle2 = 'Relative uncertainty (%)'
;  col    = [3,5,7,132,19,20,9,11,14,15,207]
  col    = [3,3,5,7,132,19,20,9,11,14,15,207,3,5,7,132,19,20,9,11,14,15,207,3,5,7,132,19,20,9,11,14,15,207,3,5,7,132,19,20,9,11,14,15,207,3,5,7,132]
  ;colsel = [5,11,12,13] 
  colsel = [5,11,12,13,14] 
  ;thicki = [4,3,3,4]
  thicki = [4,3,3,3,4]
  time   = 0.+findgen(12)
  
  xtickname = StrArr(12)
  xtickname = ['J','F','M','A','M','J','J','A','S','O','N','D']
  
;  catname = ['Anthropogenic','Anthropogenic','Anthropogenic','BB Tropics','BB Extratropics',$
;             'Rice agriculture','Inundated wetlands','Wet Min Soils','Peatlands NA','Peatlands Eurasia',$
;             'Other Natural']

  catname = ['ANTH_AFR','ANTH_AUS','ANTH_CHIN','ANTH_EU','ANTH_IND','ANTH_MIDEAST','ANTH_NA','ANTH_OCE','ANTH_RUS','ANTH_SA','ANTH_SE_ASIA','BB_AUS','BB_CHIN',$
          'BB_EU','BB_IND','BB_MID','BB_NAbor','BB_NAtemp','BB_NAFR','BB_RUS','BB_SAtemp','BB_SAtrop','BB_SE_ASIA','BB_SAFR','RICE_AFR','RICE_ASIA_AUS',$
'RICE_CHIN','RICE_EU','RICE_IND','RICE_NA','RICE_SA','WETL_AUS','WETL_CHIN','WETL_EU','WETL_IND','WETL_MID','WETL_NAbor','WETL_NAtemp','WETL_NAFR','WETL_RUS','WETL_SAtemp',$
'WETL_SAtrop','WETL_SE_asia','WETL_SAfr','WILD_anim','TERMITES','OCEAN','Volc','Anthropogenic','Biomass Burning','Rice','Wetland']



  position1 = [0.1,0.55,0.95,0.95]
  position2 = [0.1,0.07,0.95,0.47]

  position1 = [0.1,0.55,0.95,0.95,0.95]
  position2 = [0.1,0.07,0.95,0.47,0.65]

  xmaxi1 = max(emissplot)
  xmini1 = min(emissplot)  
  yrange1 = [xmini1,xmaxi1]
  
  xmaxi2 = max(uncertplot)
  xmini2 = min(uncertplot)
  yrange2 = [xmini2,xmaxi2+xmaxi2/20.]

  plot,time,emissplot[*,0],/xstyle,ytitle=ytitle1,yrange=yrange1,xrange=xrange,/yst,$
       charsize=1.4,xtickv=time,xticks=11,ytickname=ytickname,xtickname=xtickname,$
       charthick=1.,position=position1,/nodata,/noerase,title=title,noclip=0
       
  xyouts,0.3,xmaxi1-(xmaxi1-xmini1)/10.,catname[cat]+', '+year,charthick=1.4,charsize=1.5,color=255

  FOR i=0,nser-1 DO oplot,time,emissplot[*,i],color=colsel[i],thick=thicki[i],linestyle=0,noclip=0
  
  plot,time,uncertplot[*,0],/xstyle,ytitle=ytitle2,yrange=yrange2,xrange=xrange,/yst,$
       charsize=1.4,xtickv=time,xticks=11,ytickname=ytickname,xtickname=xtickname,$
       charthick=1.,position=position2,/nodata,/noerase,title=title,noclip=0  

  FOR i=0,nser-1 DO oplot,time,uncertplot[*,i],color=colsel[i],thick=thicki[i],linestyle=0,noclip=0

  ca  = 'apriori'
  cp1 = 'aposteriori 1'
  cp2 = 'aposteriori 2'
  cp3 = 'aposteriori 3'
  cp4 = 'aposteriori 4'
  c = [ca,cp1,cp2,cp3,cp4]

  FOR i=0,nser-1 DO BEGIN
    plots,8.,xmaxi2-(xmaxi2-xmini2)/100.*20.-i*(xmaxi2-xmini2)/10.
    plots,8.7,xmaxi2-(xmaxi2-xmini2)/100*20.-i*(xmaxi2-xmini2)/10.,color=colsel[i],thick=thicki[i],linestyle=0,/CONTINUE
    xyouts,8.8,xmaxi2-(xmaxi2-xmini2)/100*22.-i*(xmaxi2-xmini2)/10.,c[i],charsize=1.4,charthick=1.4,color=255 
  ENDFOR

  close_ps
  !p.font=-1

END
