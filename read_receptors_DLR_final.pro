;+
; NAME:
;
;   READ_receptors_maiolica
;
; PURPOSE:
;
;   Read FLEXPART receptor output
;
;   Attention!!
;   Receptor output for month yyyymm is located in a directory
;   which has the time stamp of the following month.
; 
; CATEGORY:
;
;   trajectory, atmospheric transport, dispersion modelling
;
; CALLING SEQUENCE:
;
;   read_receptors_urmel,sim=sim,yyyymm=yyyymm,info=info,data=data
;
; INPUTS:
;
;       sim      :  structure with all information about the simulation
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
;       yyyymm   : (STRING) year and month for which to get receptor output
;           
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;    info: structure array of length nrecpt (number of receptor points) of type
;           {rcptname:'',xrcpt:0.,yrcpt:0.,zrcpt:0.,hxmax:0.,hymax:0.,hzmax:0.,time:0}
;           containing name and coordinates of receptor point and number of time points
;    data: structure array of length ntime (number of time points) of type
;           {pptv:DblArr(nspec,nrcpt),std:DblArr(nspec,nrcpt),avgnum:DblArr(nrcpt),$
;           kernweights:DblArr(nrcpt)}
;           containing the actual concentrations for the nspec different species
;           at the nrcpt different receptor points.
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
;
; EXAMPLE:
;
;  sim.modeldir = '/nas/spc134/URMEL/FLEXPART80CTP/output/URMEL_CTRL_II/'
;  yyyymm = '200401' ; get receptor point values for Jan 2004
;  read_receptors_urmel,sim=sim,yyyymm=yyyymm,info=info,data=data
;
; MODIFICATION HISTORY:
;
;-

;--------------------------------------------------------------------

PRO read_receptors_DLR_final,sim=sim,yyyymm=yyyymm,info=info,data=data

;  IF n_elements(sim) EQ 0 THEN BEGIN
;     sim = {name:'final_sim01',$
;          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;          modeldir:'/nas/arf/output/',$
;            outdir:'/nas/arf/INVERSION/SENSITIVITIES/',$
;          syyyymm:'199002',eyyyymm:'199201',scalef_Q:1,$
;          ntrace:48,nage:5}
;  ENDIF

IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'final_sim01',$
          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
          modeldir:'/nas/arf/output/',$
            outdir:'/nas/arf/INVERSION/SENSITIVITIES/',$
          syyyymm:'199002',eyyyymm:'199004',scalef_Q:1,$
          ntrace:48,nage:5}
  ENDIF ;TEST


;;;; MAIOLICA RECEPTOR READ ;;;

;; Attention: receptor output for month i is in directory for month i+1
;yyyymmp1 = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')+40),0,6)
;  direc = sim.modeldir+sim.name+'/'+yyyymmp1+'01/'
;yyyymmp1 = STRMID(gvtime2dtg(dtg2gvtime(yyyymm)+40),0,6)
; DLR month i in directory for month i 
yyyymmp1 = STRMID(gvtime2dtg(dtg2gvtime(yyyymm)),0,6)
;stop

;  direc = sim.modeldir+sim.name+'/'+yyyymmp1+'01/'
;  direc = sim.modeldir+'DLR/stations_atheight/scout_'+station

;Number of receptors 
;file2 ='/nas/arf/output/final_sim01/'+yyyymmp1+'01/receptor_pptv.nc'
file2 ='/nas/arf/output/final_sim01/19890101/receptor_pptv.nc'
 ncid2 = ncdf_open(file2)

  dimyid=ncdf_dimid(ncid2,'rec')
  ncdf_diminq,ncid2,dimyid,name,nrcpt
  dimtid=ncdf_dimid(ncid2,'pnt')
  ncdf_diminq,ncid2,dimtid,name,npnt
  dimtid=ncdf_dimid(ncid2,'age')
  ncdf_diminq,ncid2,dimtid,name,nage
 ; dimtid=ncdf_dimid(ncid,'receptorname')
 ; ncdf_diminq,ncid,dimtid,name,nreceptorname
 ;dimtid=ncdf_dimid(ncid,'CH4_A8_1')

  ncdf_varget,ncid2,'rec',rec
  ncdf_varget,ncid2,'pnt',pnt
  ncdf_varget,ncid2,'age',age
  ncdf_varget,ncid2,'receptorname',receptorname
  ncdf_varget,ncid2,'lon',lon
  ncdf_varget,ncid2,'lat',lat
  ncdf_varget,ncid2,'lev',lev
  ncdf_varget,ncid2,'hx',hx
ncdf_varget,ncid2,'hy',hy
ncdf_varget,ncid2,'hz',hz
ncdf_varget,ncid2,'Air_tracer',Air_tracer
ncdf_varget,ncid2,'nn_Air_tracer',nn_Air_tracer
ncdf_varget,ncid2,'xk_Air_tracer',xk_Air_tracer
ncdf_varget,ncid2,'sd_Air_tracer',sd_Air_tracer

;ncdf_varget,ncid2,'time',time ;ESSAI PUT 21/05/2016

ncdf_close,ncid2

;nrcpt2=347 ;69 used
nrcpt2=nrcpt
;FOR i=0,nrcpt2-1 DO BEGIN 
direc3='/nas/arf/INVERSION/DLR/empa/scout_ALE/'
file3 = direc3+'empa___________'+yyyymmp1+'_scout_ALE_atstatheight.nc'
IF yyyymmp1 ge 200800 then file3 = direc3+'empa14_________'+yyyymmp1+'_scout_ALE_atstatheight.nc'
ncid3 = ncdf_open(file3)
ncdf_varget,ncid3,'time',time ;;;ESSAI commented 21/05/2016
time_temp=(time-time[0]+1)*86400 ;essai2 put 21/05/2016 
time=time_temp;essai2 put 21/05/2016 
ntime=n_elements(time)
ncdf_close,ncid3
CH4_A1_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A1_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A1_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A1_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A1_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A2_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A2_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A2_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A2_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A2_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A3_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A3_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A3_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A3_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A3_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A4_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A4_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A4_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A4_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A4_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A5_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A5_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A5_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A5_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A5_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A6_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A6_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A6_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A6_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A6_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A7_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A7_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A7_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A7_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A7_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A8_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A8_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A8_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A8_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A8_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A9_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A9_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A9_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A9_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A9_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A10_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A10_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A10_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A10_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A10_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A11_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A11_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A11_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A11_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_A11_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_B1_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B1_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B1_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B1_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B1_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B2_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B2_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B2_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B2_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B2_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B3_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B3_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B3_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B3_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B3_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B4_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B4_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B4_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B4_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B4_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B5_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B5_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B5_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B5_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B5_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B6_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B6_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B6_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B6_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B6_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B7_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B7_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B7_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B7_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B7_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B8_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B8_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B8_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B8_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B8_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B9_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B9_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B9_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B9_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B9_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B10_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B10_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B10_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B10_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B10_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B11_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B11_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B11_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B11_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B11_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B12_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B12_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B12_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B12_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B12_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B13_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B13_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B13_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B13_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_B13_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_W1_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W1_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W1_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W1_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W1_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W2_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W2_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W2_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W2_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W2_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W3_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W3_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W3_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W3_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W3_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W4_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W4_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W4_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W4_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W4_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W5_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W5_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W5_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W5_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W5_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W6_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W6_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W6_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W6_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W6_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W7_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W7_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W7_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W7_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W7_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W8_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W8_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W8_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W8_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W8_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W9_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W9_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W9_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W9_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W9_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W10_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W10_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W10_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W10_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W10_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W11_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W11_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W11_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W11_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W11_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W12_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W12_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W12_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W12_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W12_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W13_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W13_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W13_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W13_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_W13_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_R1_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R1_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R1_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R1_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R1_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R2_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R2_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R2_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R2_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R2_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R3_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R3_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R3_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R3_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R3_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R4_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R4_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R4_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R4_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R4_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R5_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R5_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R5_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R5_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R5_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R6_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R6_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R6_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R6_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R6_5_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R7_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R7_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R7_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R7_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_R7_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_WAN_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_WAN_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_WAN_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_WAN_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_WAN_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_OCE_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_OCE_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_OCE_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_OCE_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_OCE_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_TER_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_TER_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_TER_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_TER_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_TER_5_mat=DblArr(nrcpt2,1,1,ntime)

CH4_VOL_1_mat=DblArr(nrcpt2,1,1,ntime)
CH4_VOL_2_mat=DblArr(nrcpt2,1,1,ntime)
CH4_VOL_3_mat=DblArr(nrcpt2,1,1,ntime)
CH4_VOL_4_mat=DblArr(nrcpt2,1,1,ntime)
CH4_VOL_5_mat=DblArr(nrcpt2,1,1,ntime)

;FOR i=0,2 DO BEGIN ;test
FOR i=0,nrcpt2-1 DO BEGIN ;test

;IF i eq 80 THEN station='ALT' ELSE IF i eq 83 THEN station='BRW' ELSE IF i eq 61 THEN station='MHD' ELSE IF i eq 84 THEN station='MLO' ELSE IF i eq 275 THEN station='RPB' ELSE IF i eq 98  THEN station='SMO' ELSE IF i eq 11  THEN station='THD' ELSE IF i eq 281  THEN station='WSA' ELSE IF i eq 45  THEN station='CGO' ELSE IF i eq 35  THEN station='IZO' ELSE IF i eq 51  THEN station='ZEP' ELSE IF i eq 306  THEN station='SUM' ELSE IF i eq 317  THEN station='TER' ELSE IF i eq 33  THEN station='PAL' ELSE IF i eq 305  THEN station='ICE' ELSE IF i eq 190  THEN station='SIS' ELSE IF i eq 127  THEN station='CBA' ELSE IF i eq 106  THEN station='BAL' ELSE IF i eq 292  THEN station='SHM' ELSE IF i eq 242  THEN station='OXK' ELSE IF i eq 137  THEN station='ESP' ELSE IF i eq 69  THEN station='HPB' ELSE IF i eq 155  THEN station='HUN' ELSE IF i eq 271  THEN station='PUY' ELSE IF i eq 249  THEN station='LEF' ELSE IF i eq 102 THEN station='AMT' ELSE IF i eq 128 THEN station='BSC' ELSE IF i eq 284 THEN station='KZD' ELSE IF i eq 323 THEN station='UUM' ELSE IF i eq 253 THEN station='PDM' ELSE IF i eq 236 THEN station='NWR' ELSE IF i eq 324 THEN station='UTA' ELSE IF i eq 290 THEN station='AZR' ELSE IF i eq 270 THEN station='PTA' ELSE IF i eq 302 THEN station='SGP' ELSE IF i eq 315 THEN station='TAP' ELSE IF i eq 46 THEN station='WLG' ELSE IF i eq 186 THEN station='LMP' ELSE IF i eq 322 THEN station='BMW' ELSE IF i eq 304 THEN station='BME' ELSE IF i eq 222 THEN station='WKT' ELSE IF i eq 288 THEN station='WIS' ELSE IF i eq 162 THEN station='KEY' ELSE IF i eq 53 THEN station='ASK' ELSE IF i eq 193 THEN station='LLN' ELSE IF i eq 120 THEN station='KUM' ELSE IF i eq 123 THEN station='CRI' ELSE IF i eq 150 THEN station='GMI' ELSE IF i eq 50 THEN station='ABP' ELSE IF i eq 126 THEN station='CHR' ELSE IF i eq 70 THEN station='MKN' ELSE IF i eq 202 THEN station='SEY' ELSE IF i eq 105 THEN station='ASC' ELSE IF i eq 119 THEN station='CFA' ELSE IF i eq 145 THEN station='NMB' ELSE IF i eq 134 THEN station='EIC' ELSE IF i eq 78 THEN station='AMS' ELSE IF i eq 203 THEN station='MAA' ELSE IF i eq 103 THEN station='ARH' ELSE IF i eq 107 THEN station='BHD' ELSE IF i eq 129 THEN station='CRZ' ELSE IF i eq 201 THEN station='MQA' ELSE IF i eq 319 THEN station='TDF' ELSE IF i eq 248 THEN station='PSA' ELSE IF i eq 124 THEN station='CYA' ELSE IF i eq 307 THEN station='SYO' ELSE IF i eq 151 THEN station='HBA' ELSE station='ALE'


;IF i eq 80 THEN station='ALE' ELSE IF i eq 83 THEN station='BRW' ELSE IF i eq 61 THEN station='MAC' ELSE IF i eq 84 THEN station='MAU' ELSE IF i eq 275 THEN station='RAG'  ELSE IF i eq 11  THEN station='TRI' ELSE IF i eq 281  THEN station='SAB' ELSE IF i eq 45  THEN station='CPG' ELSE IF i eq 35  THEN station='IZA' ELSE IF i eq 51  THEN station='ZEP' ELSE IF i eq 306  THEN station='SUM' ELSE IF i eq 317  THEN station='TER' ELSE IF i eq 33  THEN station='PSA'  ELSE  IF i eq 127  THEN station='COL' ELSE IF i eq 106  THEN station='BAL' ELSE IF i eq 292  THEN station='SHE' ELSE IF i eq 242  THEN station='OCH' ELSE IF i eq 137  THEN station='EST' ELSE IF i eq 69  THEN station='HOH' ELSE IF i eq 155  THEN station='HEG' ELSE IF i eq 271  THEN station='PUY' ELSE IF i eq 249  THEN station='PAR' ELSE IF i eq 128 THEN station='BLA' ELSE IF i eq 284 THEN station='SAR' ELSE IF i eq 323 THEN station='ULA' ELSE IF i eq 253 THEN station='PIC' ELSE IF i eq 332 THEN station='BEG' ELSE IF i eq 236 THEN station='NIW' ELSE IF i eq 324 THEN station='WEN' ELSE IF i eq 290 THEN station='TCI' ELSE IF i eq 270 THEN station='POI' ELSE IF i eq 302 THEN station='SGP' ELSE IF i eq 315 THEN station='TAE' ELSE IF i eq 46 THEN station='MTW' ELSE IF i eq 186 THEN station='LAM' ELSE IF i eq 322 THEN station='TUD' ELSE IF i eq 304 THEN station='STD' ELSE IF i eq 222 THEN station='MOO' ELSE IF i eq 288 THEN station='SED' ELSE IF i eq 162 THEN station='KEY' ELSE IF i eq 53 THEN station='ASS' ELSE IF i eq 193 THEN station='LUL' ELSE IF i eq 120 THEN station='CPK' ELSE IF i eq 123 THEN station='CPR' ELSE IF i eq 150 THEN station='GUA' ELSE IF i eq 50 THEN station='ARE' ELSE IF i eq 126 THEN station='CHR' ELSE IF i eq 70 THEN station='MTK' ELSE IF i eq 202 THEN station='MAH' ELSE IF i eq 105 THEN station='ASC' ELSE IF i eq 119 THEN station='CPF' ELSE IF i eq 145 THEN station='GLO' ELSE IF i eq 134 THEN station='EAS' ELSE IF i eq 78 THEN station='AMS' ELSE IF i eq 203 THEN station='MAW' ELSE IF i eq 103 THEN station='ARR' ELSE IF i eq 107 THEN station='BAR' ELSE IF i eq 129 THEN station='CRO' ELSE IF i eq 201 THEN station='MAC' ELSE IF i eq 319 THEN station='TIE' ELSE IF i eq 248 THEN station='PST' ELSE IF i eq 124 THEN station='CAS' ELSE IF i eq 307 THEN station='SYO' ELSE IF i eq 151 THEN station='HAL' ELSE IF i eq 338 THEN station='ILE' ELSE station='MAW'


;station=StrArr(65)
;station[i]=['ALE','BRW','MAC','MAU','RAG','TRI','SAB','CPG','IZA','ZEP','SUM','TER','PSA','COL','BAL','SHE','OCH','EST','HOH','HEG','PUY','PAR','BLA','SAR','ULA','PIC','BEG','NIW','WEN','TCI','POI','SGP','TAE','MTW','LAM','TUD','STD','MOO','SED','KEY','ASS','LUL','CPK','CPR','GUa','ARE','CHR','MTK','MAH','ASC','CPF','GLO','EAS','AMS','MAW','ARR','BAR','CRO','MAC','TIE','PST','CAS','SYO','HAL','ILE']

station=StrArr(347)
station[0:346]='ALE'

station[80]='ALE'
station[83]='BRW'
station[61]='MAC'
station[84]='MAU'
station[275]='RAG'
station[11]='TRI'
station[281]='SAB'
station[45]='CPG'
station[35]='IZA'
station[51]='ZEP'
station[306]='SUM'
station[317]='TER'
station[33]='PSA'
station[127]='COL'
station[106]='BAL'
station[292]='SHE'
station[242]='OCH'
station[137]='EST'
station[69]='HOH'
station[155]='HEG'
station[271]='PUY'
station[249]='PAR'
station[128]='BLA'
station[284]='SAR'
station[323]='ULA'
station[253]='PIC'
station[332]='BEG'
station[236]='NIW'
station[324]='WEN'
station[290]='TCI'
station[270]='POI'
station[302]='SGP'
station[315]='TAE'
station[46]='MTW'
station[186]='LAM'
station[322]='TUD'
station[304]='STD'
station[222]='MOO'
station[288]='SED'
station[162]='KEY'
station[53]='ASS'
station[193]='LUL'
station[120]='CPK'
station[123]='CPR'
station[150]='GUA'
station[50]='ARE'
station[126]='CHR'
station[70]='MTK'
station[202]='MAH'
station[105]='ASC'
station[119]='CPF'
station[145]='GLO'
station[134]='EAS'
station[78]='AMS'
station[203]='MAW'
station[103]='ARR'
station[107]='BAR'
station[129]='CRO'
station[201]='MQI'
station[319]='TIE'
station[248]='PST'
station[124]='CAS'
station[307]='SYO'
station[151]='HAL'
station[338]='ILE'








;station[i]=[ALE,BRW,MAC,MAU,RAG,TRI,SAB,CPG,IZA,ZEP,SUM,TER,PSA,COL,BAL,SHE,OCH,EST,HOH,HEG,PUY,PAR,BLA,SAR,ULA,PIC,BEG,NIW,WEN,TCI,POI,SGP,TAE,'MTW','LAM','TUD','STD','MOO','SED','KEY','ASS','LUL','CPK','CPR','GUa','ARE','CHR','MTK','MAH','ASC','CPF','GLO','EAS','AMS','MAW','ARR','BAR','CRO','MAC','TIE','PST','CAS','SYO','HAL','ILE']

;IF i eq ?    THEN station='LPO '
;IF i eq ? THEN station='BGU'

;direc='/nas/arf/INVERSION/DLR/empa/scout_'+station+'/'
direc='/nas/arf/INVERSION/DLR/empa/scout_'+station[i]+'/'

print, direc

;file ='/nas/arf/output/final_sim01/19900101/receptor_pptv.nc'
;  file = direc+'receptor_pptv.nc'

;file = direc+'empa___________'+yyyymmp1+'_scout_'+station+'_atstatheight.nc'
file = direc+'empa___________'+yyyymmp1+'_scout_'+station[i]+'_atstatheight.nc'
IF yyyymmp1 ge 200800 then file = direc+'empa14_________'+yyyymmp1+'_scout_'+station[i]+'_atstatheight.nc'
;file2 ='/nas/arf/output/final_sim01/'+yyyymmp1+'01/receptor_pptv.nc' ;722
;ENDFOR ; TEST 



;Number of receptors  
;nrcpt=347

;Number CH4 species (48*5) + air_tracer
nspec=241
;file = direc+'header'
;read_header_URMEL,filename=fileheader,header=header
;  nspec  = header.nspec
;nspec=11 ;TEST

;  mv   = -9.96921e36

;gsw_socol=fltarr(nspsw,nlat,nz,nmonth*nyear)
  ; openfield
  ncid = ncdf_open(file)

;  ncid2 = ncdf_open(file2);722

;  dimxid=ncdf_dimid(ncid,'time');essai commented 21/05/2016
;  ncdf_diminq,ncid,dimxid,name,ntime ;essai commented 21/05/2016

;  dimyid=ncdf_dimid(ncid2,'rec');722
;  ncdf_diminq,ncid2,dimyid,name,nrcpt;722
;  dimtid=ncdf_dimid(ncid2,'pnt');722
;  ncdf_diminq,ncid2,dimtid,name,npnt;722
;  dimtid=ncdf_dimid(ncid2,'age');722
;  ncdf_diminq,ncid2,dimtid,name,nage;722
 ; dimtid=ncdf_dimid(ncid,'receptorname')
 ; ncdf_diminq,ncid,dimtid,name,nreceptorname
 ;dimtid=ncdf_dimid(ncid,'CH4_A8_1')



;  ncdf_varget,ncid,'time',time ;essai commented 21/05/2016
;time_temp=(time-time[0]+1)*86400 ;essai2 put 21/05/2016 
;time=time_temp;essai2 put 21/05/2016 

;stop
;  ncdf_varget,ncid2,'rec',rec;722
;  ncdf_varget,ncid2,'pnt',pnt;722
;  ncdf_varget,ncid2,'age',age;722
;  ncdf_varget,ncid2,'receptorname',receptorname;722
;  ncdf_varget,ncid2,'lon',lon;722
;  ncdf_varget,ncid2,'lat',lat;722
;  ncdf_varget,ncid2,'lev',lev;722
;  ncdf_varget,ncid2,'hx',hx;722
;ncdf_varget,ncid2,'hy',hy;722
;ncdf_varget,ncid2,'hz',hz;722
;ncdf_varget,ncid2,'Air_tracer',Air_trace;722r
;ncdf_varget,ncid2,'nn_Air_tracer',nn_Air_tracer;722
;ncdf_varget,ncid2,'xk_Air_tracer',xk_Air_tracer;722
;ncdf_varget,ncid2,'sd_Air_tracer',sd_Air_tracer;722


ncdf_varget,ncid,'tracer_gp_CH4_fx_e01_a01_ave',CH4_A1_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e02_a01_ave',CH4_A2_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e03_a01_ave',CH4_A3_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e04_a01_ave',CH4_A4_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e05_a01_ave',CH4_A5_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e06_a01_ave',CH4_A6_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e07_a01_ave',CH4_A7_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e08_a01_ave',CH4_A8_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e09_a01_ave',CH4_A9_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e10_a01_ave',CH4_A10_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e11_a01_ave',CH4_A11_1

ncdf_varget,ncid,'tracer_gp_CH4_fx_e01_a02_ave',CH4_A1_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e02_a02_ave',CH4_A2_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e03_a02_ave',CH4_A3_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e04_a02_ave',CH4_A4_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e05_a02_ave',CH4_A5_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e06_a02_ave',CH4_A6_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e07_a02_ave',CH4_A7_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e08_a02_ave',CH4_A8_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e09_a02_ave',CH4_A9_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e10_a02_ave',CH4_A10_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e11_a02_ave',CH4_A11_2

ncdf_varget,ncid,'tracer_gp_CH4_fx_e01_a03_ave',CH4_A1_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e02_a03_ave',CH4_A2_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e03_a03_ave',CH4_A3_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e04_a03_ave',CH4_A4_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e05_a03_ave',CH4_A5_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e06_a03_ave',CH4_A6_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e07_a03_ave',CH4_A7_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e08_a03_ave',CH4_A8_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e09_a03_ave',CH4_A9_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e10_a03_ave',CH4_A10_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e11_a03_ave',CH4_A11_3

ncdf_varget,ncid,'tracer_gp_CH4_fx_e01_a04_ave',CH4_A1_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e02_a04_ave',CH4_A2_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e03_a04_ave',CH4_A3_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e04_a04_ave',CH4_A4_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e05_a04_ave',CH4_A5_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e06_a04_ave',CH4_A6_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e07_a04_ave',CH4_A7_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e08_a04_ave',CH4_A8_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e09_a04_ave',CH4_A9_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e10_a04_ave',CH4_A10_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e11_a04_ave',CH4_A11_4

ncdf_varget,ncid,'tracer_gp_CH4_fx_e01_a05_ave',CH4_A1_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e02_a05_ave',CH4_A2_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e03_a05_ave',CH4_A3_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e04_a05_ave',CH4_A4_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e05_a05_ave',CH4_A5_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e06_a05_ave',CH4_A6_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e07_a05_ave',CH4_A7_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e08_a05_ave',CH4_A8_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e09_a05_ave',CH4_A9_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e10_a05_ave',CH4_A10_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e11_a05_ave',CH4_A11_5


;;;;;;;;;;;;;;;B;;;;;;;;;;;;;;;;;;;;;


ncdf_varget,ncid,'tracer_gp_CH4_fx_e12_a01_ave',CH4_B1_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e13_a01_ave',CH4_B2_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e14_a01_ave',CH4_B3_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e15_a01_ave',CH4_B4_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e19_a01_ave',CH4_B5_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e16_a01_ave',CH4_B6_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e18_a01_ave',CH4_B7_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e17_a01_ave',CH4_B8_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e20_a01_ave',CH4_B9_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e22_a01_ave',CH4_B10_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e23_a01_ave',CH4_B11_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e24_a01_ave',CH4_B12_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e21_a01_ave',CH4_B13_1

ncdf_varget,ncid,'tracer_gp_CH4_fx_e12_a02_ave',CH4_B1_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e13_a02_ave',CH4_B2_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e14_a02_ave',CH4_B3_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e15_a02_ave',CH4_B4_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e19_a02_ave',CH4_B5_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e16_a02_ave',CH4_B6_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e18_a02_ave',CH4_B7_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e17_a02_ave',CH4_B8_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e20_a02_ave',CH4_B9_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e22_a02_ave',CH4_B10_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e23_a02_ave',CH4_B11_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e24_a02_ave',CH4_B12_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e21_a02_ave',CH4_B13_2

ncdf_varget,ncid,'tracer_gp_CH4_fx_e12_a03_ave',CH4_B1_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e13_a03_ave',CH4_B2_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e14_a03_ave',CH4_B3_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e15_a03_ave',CH4_B4_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e19_a03_ave',CH4_B5_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e16_a03_ave',CH4_B6_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e18_a03_ave',CH4_B7_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e17_a03_ave',CH4_B8_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e20_a03_ave',CH4_B9_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e22_a03_ave',CH4_B10_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e23_a03_ave',CH4_B11_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e24_a03_ave',CH4_B12_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e21_a03_ave',CH4_B13_3

ncdf_varget,ncid,'tracer_gp_CH4_fx_e12_a04_ave',CH4_B1_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e13_a04_ave',CH4_B2_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e14_a04_ave',CH4_B3_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e15_a04_ave',CH4_B4_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e19_a04_ave',CH4_B5_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e16_a04_ave',CH4_B6_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e18_a04_ave',CH4_B7_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e17_a04_ave',CH4_B8_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e20_a04_ave',CH4_B9_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e22_a04_ave',CH4_B10_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e23_a04_ave',CH4_B11_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e24_a04_ave',CH4_B12_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e21_a04_ave',CH4_B13_4

ncdf_varget,ncid,'tracer_gp_CH4_fx_e12_a05_ave',CH4_B1_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e13_a05_ave',CH4_B2_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e14_a05_ave',CH4_B3_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e15_a05_ave',CH4_B4_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e19_a05_ave',CH4_B5_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e16_a05_ave',CH4_B6_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e18_a05_ave',CH4_B7_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e17_a05_ave',CH4_B8_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e20_a05_ave',CH4_B9_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e22_a05_ave',CH4_B10_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e23_a05_ave',CH4_B11_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e24_a05_ave',CH4_B12_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e21_a05_ave',CH4_B13_5


;;;;;;;;;;;;;;;R;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'tracer_gp_CH4_fx_e26_a01_ave',CH4_R1_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e27_a01_ave',CH4_R2_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e28_a01_ave',CH4_R3_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e29_a01_ave',CH4_R4_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e30_a01_ave',CH4_R5_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e31_a01_ave',CH4_R6_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e32_a01_ave',CH4_R7_1

ncdf_varget,ncid,'tracer_gp_CH4_fx_e26_a02_ave',CH4_R1_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e27_a02_ave',CH4_R2_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e28_a02_ave',CH4_R3_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e29_a02_ave',CH4_R4_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e30_a02_ave',CH4_R5_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e31_a02_ave',CH4_R6_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e32_a02_ave',CH4_R7_2

ncdf_varget,ncid,'tracer_gp_CH4_fx_e26_a03_ave',CH4_R1_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e27_a03_ave',CH4_R2_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e28_a03_ave',CH4_R3_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e29_a03_ave',CH4_R4_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e30_a03_ave',CH4_R5_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e31_a03_ave',CH4_R6_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e32_a03_ave',CH4_R7_3

ncdf_varget,ncid,'tracer_gp_CH4_fx_e26_a04_ave',CH4_R1_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e27_a04_ave',CH4_R2_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e28_a04_ave',CH4_R3_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e29_a04_ave',CH4_R4_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e30_a04_ave',CH4_R5_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e31_a04_ave',CH4_R6_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e32_a04_ave',CH4_R7_4

ncdf_varget,ncid,'tracer_gp_CH4_fx_e26_a05_ave',CH4_R1_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e27_a05_ave',CH4_R2_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e28_a05_ave',CH4_R3_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e29_a05_ave',CH4_R4_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e30_a05_ave',CH4_R5_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e31_a05_ave',CH4_R6_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e32_a05_ave',CH4_R7_5

;;;;;;;;;;;;;;;W;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'tracer_gp_CH4_fx_e35_a01_ave',CH4_W1_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e36_a01_ave',CH4_W2_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e37_a01_ave',CH4_W3_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e38_a01_ave',CH4_W4_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e39_a01_ave',CH4_W5_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e40_a01_ave',CH4_W6_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e42_a01_ave',CH4_W7_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e41_a01_ave',CH4_W8_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e43_a01_ave',CH4_W9_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e45_a01_ave',CH4_W10_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e46_a01_ave',CH4_W11_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e47_a01_ave',CH4_W12_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e44_a01_ave',CH4_W13_1

ncdf_varget,ncid,'tracer_gp_CH4_fx_e35_a02_ave',CH4_W1_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e36_a02_ave',CH4_W2_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e37_a02_ave',CH4_W3_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e38_a02_ave',CH4_W4_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e39_a02_ave',CH4_W5_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e40_a02_ave',CH4_W6_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e42_a02_ave',CH4_W7_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e41_a02_ave',CH4_W8_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e43_a02_ave',CH4_W9_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e45_a02_ave',CH4_W10_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e46_a02_ave',CH4_W11_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e47_a02_ave',CH4_W12_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e44_a02_ave',CH4_W13_2

ncdf_varget,ncid,'tracer_gp_CH4_fx_e35_a03_ave',CH4_W1_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e36_a03_ave',CH4_W2_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e37_a03_ave',CH4_W3_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e38_a03_ave',CH4_W4_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e39_a03_ave',CH4_W5_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e40_a03_ave',CH4_W6_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e42_a03_ave',CH4_W7_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e41_a03_ave',CH4_W8_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e43_a03_ave',CH4_W9_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e45_a03_ave',CH4_W10_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e46_a03_ave',CH4_W11_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e47_a03_ave',CH4_W12_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e44_a03_ave',CH4_W13_3

ncdf_varget,ncid,'tracer_gp_CH4_fx_e35_a04_ave',CH4_W1_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e36_a04_ave',CH4_W2_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e37_a04_ave',CH4_W3_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e38_a04_ave',CH4_W4_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e39_a04_ave',CH4_W5_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e40_a04_ave',CH4_W6_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e42_a04_ave',CH4_W7_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e41_a04_ave',CH4_W8_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e43_a04_ave',CH4_W9_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e45_a04_ave',CH4_W10_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e46_a04_ave',CH4_W11_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e47_a04_ave',CH4_W12_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e44_a04_ave',CH4_W13_4

ncdf_varget,ncid,'tracer_gp_CH4_fx_e35_a05_ave',CH4_W1_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e36_a05_ave',CH4_W2_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e37_a05_ave',CH4_W3_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e38_a05_ave',CH4_W4_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e39_a05_ave',CH4_W5_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e40_a05_ave',CH4_W6_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e42_a05_ave',CH4_W7_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e41_a05_ave',CH4_W8_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e43_a05_ave',CH4_W9_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e45_a05_ave',CH4_W10_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e46_a05_ave',CH4_W11_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e47_a05_ave',CH4_W12_5
ncdf_varget,ncid,'tracer_gp_CH4_fx_e44_a05_ave',CH4_W13_5

;;;;;;;;;;;;;;OTHERS;;;;;;;;

ncdf_varget,ncid,'tracer_gp_CH4_fx_e48_a01_ave',CH4_WAN_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e48_a02_ave',CH4_WAN_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e48_a03_ave',CH4_WAN_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e48_a04_ave',CH4_WAN_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e48_a05_ave',CH4_WAN_5

ncdf_varget,ncid,'tracer_gp_CH4_fx_e33_a01_ave',CH4_TER_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e33_a02_ave',CH4_TER_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e33_a03_ave',CH4_TER_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e33_a04_ave',CH4_TER_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e33_a05_ave',CH4_TER_5

ncdf_varget,ncid,'tracer_gp_CH4_fx_e25_a01_ave',CH4_OCE_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e25_a02_ave',CH4_OCE_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e25_a03_ave',CH4_OCE_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e25_a04_ave',CH4_OCE_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e25_a05_ave',CH4_OCE_5

ncdf_varget,ncid,'tracer_gp_CH4_fx_e34_a01_ave',CH4_VOL_1
ncdf_varget,ncid,'tracer_gp_CH4_fx_e34_a02_ave',CH4_VOL_2
ncdf_varget,ncid,'tracer_gp_CH4_fx_e34_a03_ave',CH4_VOL_3
ncdf_varget,ncid,'tracer_gp_CH4_fx_e34_a04_ave',CH4_VOL_4
ncdf_varget,ncid,'tracer_gp_CH4_fx_e34_a05_ave',CH4_VOL_5

ncdf_close,ncid

;;;TEST just DLR concentr otherwise flexpart (sd)

;;:::::::::::::::::::::::::;SD;;;:::::::::::::::::::::::::::;;

;ncdf_varget,ncid2,'sd_Air_tracer',sd_Air_tracer

;ncdf_varget,ncid2,'sd_CH4_A1_1',sd_CH4_A1_1
;ncdf_varget,ncid2,'sd_CH4_A2_1',sd_CH4_A2_1
;ncdf_varget,ncid2,'sd_CH4_A3_1',sd_CH4_A3_1
;ncdf_varget,ncid2,'sd_CH4_A4_1',sd_CH4_A4_1
;ncdf_varget,ncid2,'sd_CH4_A5_1',sd_CH4_A5_1
;ncdf_varget,ncid2,'sd_CH4_A6_1',sd_CH4_A6_1
;ncdf_varget,ncid2,'sd_CH4_A7_1',sd_CH4_A7_1
;ncdf_varget,ncid2,'sd_CH4_A8_1',sd_CH4_A8_1
;ncdf_varget,ncid2,'sd_CH4_A9_1',sd_CH4_A9_1
;ncdf_varget,ncid2,'sd_CH4_A10_1',sd_CH4_A10_1
;ncdf_varget,ncid2,'sd_CH4_A11_1',sd_CH4_A11_1

;ncdf_varget,ncid2,'sd_CH4_A1_2',sd_CH4_A1_2
;ncdf_varget,ncid2,'sd_CH4_A2_2',sd_CH4_A2_2
;ncdf_varget,ncid2,'sd_CH4_A3_2',sd_CH4_A3_2
;ncdf_varget,ncid2,'sd_CH4_A4_2',sd_CH4_A4_2
;ncdf_varget,ncid2,'sd_CH4_A5_2',sd_CH4_A5_2
;ncdf_varget,ncid2,'sd_CH4_A6_2',sd_CH4_A6_2
;ncdf_varget,ncid2,'sd_CH4_A7_2',sd_CH4_A7_2
;ncdf_varget,ncid2,'sd_CH4_A8_2',sd_CH4_A8_2
;ncdf_varget,ncid2,'sd_CH4_A9_2',sd_CH4_A9_2
;ncdf_varget,ncid2,'sd_CH4_A10_2',sd_CH4_A10_2
;ncdf_varget,ncid2,'sd_CH4_A11_2',sd_CH4_A11_2

;ncdf_varget,ncid2,'sd_CH4_A1_3',sd_CH4_A1_3
;ncdf_varget,ncid2,'sd_CH4_A2_3',sd_CH4_A2_3
;ncdf_varget,ncid2,'sd_CH4_A3_3',sd_CH4_A3_3
;ncdf_varget,ncid2,'sd_CH4_A4_3',sd_CH4_A4_3
;ncdf_varget,ncid2,'sd_CH4_A5_3',sd_CH4_A5_3
;ncdf_varget,ncid2,'sd_CH4_A6_3',sd_CH4_A6_3
;ncdf_varget,ncid2,'sd_CH4_A7_3',sd_CH4_A7_3
;ncdf_varget,ncid2,'sd_CH4_A8_3',sd_CH4_A8_3
;ncdf_varget,ncid2,'sd_CH4_A9_3',sd_CH4_A9_3
;ncdf_varget,ncid2,'sd_CH4_A10_3',sd_CH4_A10_3
;ncdf_varget,ncid2,'sd_CH4_A11_3',sd_CH4_A11_3

;ncdf_varget,ncid2,'sd_CH4_A1_4',sd_CH4_A1_4
;ncdf_varget,ncid2,'sd_CH4_A2_4',sd_CH4_A2_4
;ncdf_varget,ncid2,'sd_CH4_A3_4',sd_CH4_A3_4
;ncdf_varget,ncid2,'sd_CH4_A4_4',sd_CH4_A4_4
;ncdf_varget,ncid2,'sd_CH4_A5_4',sd_CH4_A5_4
;ncdf_varget,ncid2,'sd_CH4_A6_4',sd_CH4_A6_4
;ncdf_varget,ncid2,'sd_CH4_A7_4',sd_CH4_A7_4
;ncdf_varget,ncid2,'sd_CH4_A8_4',sd_CH4_A8_4
;ncdf_varget,ncid2,'sd_CH4_A9_4',sd_CH4_A9_4
;ncdf_varget,ncid2,'sd_CH4_A10_4',sd_CH4_A10_4
;ncdf_varget,ncid2,'sd_CH4_A11_4',sd_CH4_A11_4

;ncdf_varget,ncid2,'sd_CH4_A1_5',sd_CH4_A1_5
;ncdf_varget,ncid2,'sd_CH4_A2_5',sd_CH4_A2_5
;ncdf_varget,ncid2,'sd_CH4_A3_5',sd_CH4_A3_5
;ncdf_varget,ncid2,'sd_CH4_A4_5',sd_CH4_A4_5
;ncdf_varget,ncid2,'sd_CH4_A5_5',sd_CH4_A5_5
;ncdf_varget,ncid2,'sd_CH4_A6_5',sd_CH4_A6_5
;ncdf_varget,ncid2,'sd_CH4_A7_5',sd_CH4_A7_5
;ncdf_varget,ncid2,'sd_CH4_A8_5',sd_CH4_A8_5
;ncdf_varget,ncid2,'sd_CH4_A9_5',sd_CH4_A9_5
;ncdf_varget,ncid2,'sd_CH4_A10_5',sd_CH4_A10_5
;ncdf_varget,ncid2,'sd_CH4_A11_5',sd_CH4_A11_5


;;;;;;;;;;;;;;;B;;;;;;;;;;;;;;;;;;;;;

;ncdf_varget,ncid2,'sd_CH4_B1_1',sd_CH4_B1_1
;ncdf_varget,ncid2,'sd_CH4_B2_1',sd_CH4_B2_1
;ncdf_varget,ncid2,'sd_CH4_B3_1',sd_CH4_B3_1
;ncdf_varget,ncid2,'sd_CH4_B4_1',sd_CH4_B4_1
;ncdf_varget,ncid2,'sd_CH4_B5_1',sd_CH4_B5_1
;ncdf_varget,ncid2,'sd_CH4_B6_1',sd_CH4_B6_1
;ncdf_varget,ncid2,'sd_CH4_B7_1',sd_CH4_B7_1
;ncdf_varget,ncid2,'sd_CH4_B8_1',sd_CH4_B8_1
;ncdf_varget,ncid2,'sd_CH4_B9_1',sd_CH4_B9_1
;ncdf_varget,ncid2,'sd_CH4_B10_1',sd_CH4_B10_1
;ncdf_varget,ncid2,'sd_CH4_B11_1',sd_CH4_B11_1
;ncdf_varget,ncid2,'sd_CH4_B12_1',sd_CH4_B12_1
;ncdf_varget,ncid2,'sd_CH4_B13_1',sd_CH4_B13_1

;ncdf_varget,ncid2,'sd_CH4_B1_2',sd_CH4_B1_2
;ncdf_varget,ncid2,'sd_CH4_B2_2',sd_CH4_B2_2
;ncdf_varget,ncid2,'sd_CH4_B3_2',sd_CH4_B3_2
;ncdf_varget,ncid2,'sd_CH4_B4_2',sd_CH4_B4_2
;ncdf_varget,ncid2,'sd_CH4_B5_2',sd_CH4_B5_2
;ncdf_varget,ncid2,'sd_CH4_B6_2',sd_CH4_B6_2
;ncdf_varget,ncid2,'sd_CH4_B7_2',sd_CH4_B7_2
;ncdf_varget,ncid2,'sd_CH4_B8_2',sd_CH4_B8_2
;ncdf_varget,ncid2,'sd_CH4_B9_2',sd_CH4_B9_2
;ncdf_varget,ncid2,'sd_CH4_B10_2',sd_CH4_B10_2
;ncdf_varget,ncid2,'sd_CH4_B11_2',sd_CH4_B11_2
;ncdf_varget,ncid2,'sd_CH4_B12_2',sd_CH4_B12_2
;ncdf_varget,ncid2,'sd_CH4_B13_2',sd_CH4_B13_2

;ncdf_varget,ncid2,'sd_CH4_B1_3',sd_CH4_B1_3
;ncdf_varget,ncid2,'sd_CH4_B2_3',sd_CH4_B2_3
;ncdf_varget,ncid2,'sd_CH4_B3_3',sd_CH4_B3_3
;ncdf_varget,ncid2,'sd_CH4_B4_3',sd_CH4_B4_3
;ncdf_varget,ncid2,'sd_CH4_B5_3',sd_CH4_B5_3
;ncdf_varget,ncid2,'sd_CH4_B6_3',sd_CH4_B6_3
;ncdf_varget,ncid2,'sd_CH4_B7_3',sd_CH4_B7_3
;ncdf_varget,ncid2,'sd_CH4_B8_3',sd_CH4_B8_3
;ncdf_varget,ncid2,'sd_CH4_B9_3',sd_CH4_B9_3
;ncdf_varget,ncid2,'sd_CH4_B10_3',sd_CH4_B10_3
;ncdf_varget,ncid2,'sd_CH4_B11_3',sd_CH4_B11_3
;ncdf_varget,ncid2,'sd_CH4_B12_3',sd_CH4_B12_3
;ncdf_varget,ncid2,'sd_CH4_B13_3',sd_CH4_B13_3

;ncdf_varget,ncid2,'sd_CH4_B1_4',sd_CH4_B1_4
;ncdf_varget,ncid2,'sd_CH4_B2_4',sd_CH4_B2_4
;ncdf_varget,ncid2,'sd_CH4_B3_4',sd_CH4_B3_4
;ncdf_varget,ncid2,'sd_CH4_B4_4',sd_CH4_B4_4
;ncdf_varget,ncid2,'sd_CH4_B5_4',sd_CH4_B5_4
;ncdf_varget,ncid2,'sd_CH4_B6_4',sd_CH4_B6_4
;ncdf_varget,ncid2,'sd_CH4_B7_4',sd_CH4_B7_4
;ncdf_varget,ncid2,'sd_CH4_B8_4',sd_CH4_B8_4
;ncdf_varget,ncid2,'sd_CH4_B9_4',sd_CH4_B9_4
;ncdf_varget,ncid2,'sd_CH4_B10_4',sd_CH4_B10_4
;ncdf_varget,ncid2,'sd_CH4_B11_4',sd_CH4_B11_4
;ncdf_varget,ncid2,'sd_CH4_B12_4',sd_CH4_B12_4
;ncdf_varget,ncid2,'sd_CH4_B13_4',sd_CH4_B13_4

;ncdf_varget,ncid2,'sd_CH4_B1_5',sd_CH4_B1_5
;ncdf_varget,ncid2,'sd_CH4_B2_5',sd_CH4_B2_5
;ncdf_varget,ncid2,'sd_CH4_B3_5',sd_CH4_B3_5
;ncdf_varget,ncid2,'sd_CH4_B4_5',sd_CH4_B4_5
;ncdf_varget,ncid2,'sd_CH4_B5_5',sd_CH4_B5_5
;ncdf_varget,ncid2,'sd_CH4_B6_5',sd_CH4_B6_5
;ncdf_varget,ncid2,'sd_CH4_B7_5',sd_CH4_B7_5
;ncdf_varget,ncid2,'sd_CH4_B8_5',sd_CH4_B8_5
;ncdf_varget,ncid2,'sd_CH4_B9_5',sd_CH4_B9_5
;ncdf_varget,ncid2,'sd_CH4_B10_5',sd_CH4_B10_5
;ncdf_varget,ncid2,'sd_CH4_B11_5',sd_CH4_B11_5
;ncdf_varget,ncid2,'sd_CH4_B12_5',sd_CH4_B12_5
;ncdf_varget,ncid2,'sd_CH4_B13_5',sd_CH4_B13_5

;;;;;;;;;;;;;;;R;;;;;;;;;;;;;;;;;;;;;

;ncdf_varget,ncid2,'sd_CH4_R1_1',sd_CH4_R1_1
;ncdf_varget,ncid2,'sd_CH4_R2_1',sd_CH4_R2_1
;ncdf_varget,ncid2,'sd_CH4_R3_1',sd_CH4_R3_1
;ncdf_varget,ncid2,'sd_CH4_R4_1',sd_CH4_R4_1
;ncdf_varget,ncid2,'sd_CH4_R5_1',sd_CH4_R5_1
;ncdf_varget,ncid2,'sd_CH4_R6_1',sd_CH4_R6_1
;ncdf_varget,ncid2,'sd_CH4_R7_1',sd_CH4_R7_1


;ncdf_varget,ncid2,'sd_CH4_R1_2',sd_CH4_R1_2
;ncdf_varget,ncid2,'sd_CH4_R2_2',sd_CH4_R2_2
;ncdf_varget,ncid2,'sd_CH4_R3_2',sd_CH4_R3_2
;ncdf_varget,ncid2,'sd_CH4_R4_2',sd_CH4_R4_2
;ncdf_varget,ncid2,'sd_CH4_R5_2',sd_CH4_R5_2
;ncdf_varget,ncid2,'sd_CH4_R6_2',sd_CH4_R6_2
;ncdf_varget,ncid2,'sd_CH4_R7_2',sd_CH4_R7_2


;ncdf_varget,ncid2,'sd_CH4_R1_3',sd_CH4_R1_3
;ncdf_varget,ncid2,'sd_CH4_R2_3',sd_CH4_R2_3
;ncdf_varget,ncid2,'sd_CH4_R3_3',sd_CH4_R3_3
;ncdf_varget,ncid2,'sd_CH4_R4_3',sd_CH4_R4_3
;ncdf_varget,ncid2,'sd_CH4_R5_3',sd_CH4_R5_3
;ncdf_varget,ncid2,'sd_CH4_R6_3',sd_CH4_R6_3
;ncdf_varget,ncid2,'sd_CH4_R7_3',sd_CH4_R7_3

;ncdf_varget,ncid2,'sd_CH4_R1_4',sd_CH4_R1_4
;ncdf_varget,ncid2,'sd_CH4_R2_4',sd_CH4_R2_4
;ncdf_varget,ncid2,'sd_CH4_R3_4',sd_CH4_R3_4
;ncdf_varget,ncid2,'sd_CH4_R4_4',sd_CH4_R4_4
;ncdf_varget,ncid2,'sd_CH4_R5_4',sd_CH4_R5_4
;ncdf_varget,ncid2,'sd_CH4_R6_4',sd_CH4_R6_4
;ncdf_varget,ncid2,'sd_CH4_R7_4',sd_CH4_R7_4


;ncdf_varget,ncid2,'sd_CH4_R1_5',sd_CH4_R1_5
;ncdf_varget,ncid2,'sd_CH4_R2_5',sd_CH4_R2_5
;ncdf_varget,ncid2,'sd_CH4_R3_5',sd_CH4_R3_5
;ncdf_varget,ncid2,'sd_CH4_R4_5',sd_CH4_R4_5
;ncdf_varget,ncid2,'sd_CH4_R5_5',sd_CH4_R5_5
;ncdf_varget,ncid2,'sd_CH4_R6_5',sd_CH4_R6_5
;ncdf_varget,ncid2,'sd_CH4_R7_5',sd_CH4_R7_5


;;;;;;;;;;;;;;;W;;;;;;;;;;;;;;;;;;;;;

;ncdf_varget,ncid2,'sd_CH4_W1_1',sd_CH4_W1_1
;ncdf_varget,ncid2,'sd_CH4_W2_1',sd_CH4_W2_1
;ncdf_varget,ncid2,'sd_CH4_W3_1',sd_CH4_W3_1
;ncdf_varget,ncid2,'sd_CH4_W4_1',sd_CH4_W4_1
;ncdf_varget,ncid2,'sd_CH4_W5_1',sd_CH4_W5_1
;ncdf_varget,ncid2,'sd_CH4_W6_1',sd_CH4_W6_1
;ncdf_varget,ncid2,'sd_CH4_W7_1',sd_CH4_W7_1
;ncdf_varget,ncid2,'sd_CH4_W8_1',sd_CH4_W8_1
;ncdf_varget,ncid2,'sd_CH4_W9_1',sd_CH4_W9_1
;ncdf_varget,ncid2,'sd_CH4_W10_1',sd_CH4_W10_1
;ncdf_varget,ncid2,'sd_CH4_W11_1',sd_CH4_W11_1
;ncdf_varget,ncid2,'sd_CH4_W12_1',sd_CH4_W12_1
;ncdf_varget,ncid2,'sd_CH4_W13_1',sd_CH4_W13_1

;ncdf_varget,ncid2,'sd_CH4_W1_2',sd_CH4_W1_2
;ncdf_varget,ncid2,'sd_CH4_W2_2',sd_CH4_W2_2
;ncdf_varget,ncid2,'sd_CH4_W3_2',sd_CH4_W3_2
;ncdf_varget,ncid2,'sd_CH4_W4_2',sd_CH4_W4_2
;ncdf_varget,ncid2,'sd_CH4_W5_2',sd_CH4_W5_2
;ncdf_varget,ncid2,'sd_CH4_W6_2',sd_CH4_W6_2
;ncdf_varget,ncid2,'sd_CH4_W7_2',sd_CH4_W7_2
;ncdf_varget,ncid2,'sd_CH4_W8_2',sd_CH4_W8_2
;ncdf_varget,ncid2,'sd_CH4_W9_2',sd_CH4_W9_2
;ncdf_varget,ncid2,'sd_CH4_W10_2',sd_CH4_W10_2
;ncdf_varget,ncid2,'sd_CH4_W11_2',sd_CH4_W11_2
;ncdf_varget,ncid2,'sd_CH4_W12_2',sd_CH4_W12_2
;ncdf_varget,ncid2,'sd_CH4_W13_2',sd_CH4_W13_2

;ncdf_varget,ncid2,'sd_CH4_W1_3',sd_CH4_W1_3
;ncdf_varget,ncid2,'sd_CH4_W2_3',sd_CH4_W2_3
;ncdf_varget,ncid2,'sd_CH4_W3_3',sd_CH4_W3_3
;ncdf_varget,ncid2,'sd_CH4_W4_3',sd_CH4_W4_3
;ncdf_varget,ncid2,'sd_CH4_W5_3',sd_CH4_W5_3
;ncdf_varget,ncid2,'sd_CH4_W6_3',sd_CH4_W6_3
;ncdf_varget,ncid2,'sd_CH4_W7_3',sd_CH4_W7_3
;ncdf_varget,ncid2,'sd_CH4_W8_3',sd_CH4_W8_3
;ncdf_varget,ncid2,'sd_CH4_W9_3',sd_CH4_W9_3
;ncdf_varget,ncid2,'sd_CH4_W10_3',sd_CH4_W10_3
;ncdf_varget,ncid2,'sd_CH4_W11_3',sd_CH4_W11_3
;ncdf_varget,ncid2,'sd_CH4_W12_3',sd_CH4_W12_3
;ncdf_varget,ncid2,'sd_CH4_W13_3',sd_CH4_W13_3

;ncdf_varget,ncid2,'sd_CH4_W1_4',sd_CH4_W1_4
;ncdf_varget,ncid2,'sd_CH4_W2_4',sd_CH4_W2_4
;ncdf_varget,ncid2,'sd_CH4_W3_4',sd_CH4_W3_4
;ncdf_varget,ncid2,'sd_CH4_W4_4',sd_CH4_W4_4
;ncdf_varget,ncid2,'sd_CH4_W5_4',sd_CH4_W5_4
;ncdf_varget,ncid2,'sd_CH4_W6_4',sd_CH4_W6_4
;ncdf_varget,ncid2,'sd_CH4_W7_4',sd_CH4_W7_4
;ncdf_varget,ncid2,'sd_CH4_W8_4',sd_CH4_W8_4
;ncdf_varget,ncid2,'sd_CH4_W9_4',sd_CH4_W9_4
;ncdf_varget,ncid2,'sd_CH4_W10_4',sd_CH4_W10_4
;ncdf_varget,ncid2,'sd_CH4_W11_4',sd_CH4_W11_4
;ncdf_varget,ncid2,'sd_CH4_W12_4',sd_CH4_W12_4
;ncdf_varget,ncid2,'sd_CH4_W13_4',sd_CH4_W13_4

;ncdf_varget,ncid2,'sd_CH4_W1_5',sd_CH4_W1_5
;ncdf_varget,ncid2,'sd_CH4_W2_5',sd_CH4_W2_5
;ncdf_varget,ncid2,'sd_CH4_W3_5',sd_CH4_W3_5
;ncdf_varget,ncid2,'sd_CH4_W4_5',sd_CH4_W4_5
;ncdf_varget,ncid2,'sd_CH4_W5_5',sd_CH4_W5_5
;ncdf_varget,ncid2,'sd_CH4_W6_5',sd_CH4_W6_5
;ncdf_varget,ncid2,'sd_CH4_W7_5',sd_CH4_W7_5
;ncdf_varget,ncid2,'sd_CH4_W8_5',sd_CH4_W8_5
;ncdf_varget,ncid2,'sd_CH4_W9_5',sd_CH4_W9_5
;ncdf_varget,ncid2,'sd_CH4_W10_5',sd_CH4_W10_5
;ncdf_varget,ncid2,'sd_CH4_W11_5',sd_CH4_W11_5
;ncdf_varget,ncid2,'sd_CH4_W12_5',sd_CH4_W12_5
;ncdf_varget,ncid2,'sd_CH4_W13_5',sd_CH4_W13_5

;;;;;;;;;;;;;;OTHERS;;;;;;;;

;ncdf_varget,ncid2,'sd_CH4_WAN_1',sd_CH4_WAN_1
;ncdf_varget,ncid2,'sd_CH4_WAN_2',sd_CH4_WAN_2
;ncdf_varget,ncid2,'sd_CH4_WAN_3',sd_CH4_WAN_3
;ncdf_varget,ncid2,'sd_CH4_WAN_4',sd_CH4_WAN_4
;ncdf_varget,ncid2,'sd_CH4_WAN_5',sd_CH4_WAN_5

;ncdf_varget,ncid2,'sd_CH4_TER_1',sd_CH4_TER_1
;ncdf_varget,ncid2,'sd_CH4_TER_2',sd_CH4_TER_2
;ncdf_varget,ncid2,'sd_CH4_TER_3',sd_CH4_TER_3
;ncdf_varget,ncid2,'sd_CH4_TER_4',sd_CH4_TER_4
;ncdf_varget,ncid2,'sd_CH4_TER_5',sd_CH4_TER_5

;ncdf_varget,ncid2,'sd_CH4_OCE_1',sd_CH4_OCE_1
;ncdf_varget,ncid2,'sd_CH4_OCE_2',sd_CH4_OCE_2
;ncdf_varget,ncid2,'sd_CH4_OCE_3',sd_CH4_OCE_3
;ncdf_varget,ncid2,'sd_CH4_OCE_4',sd_CH4_OCE_4
;ncdf_varget,ncid2,'sd_CH4_OCE_5',sd_CH4_OCE_5

;ncdf_varget,ncid2,'sd_CH4_VOL_1',sd_CH4_VOL_1
;ncdf_varget,ncid2,'sd_CH4_VOL_2',sd_CH4_VOL_2
;ncdf_varget,ncid2,'sd_CH4_VOL_3',sd_CH4_VOL_3
;ncdf_varget,ncid2,'sd_CH4_VOL_4',sd_CH4_VOL_4
;ncdf_varget,ncid2,'sd_CH4_VOL_5',sd_CH4_VOL_5



;;;; NN AND XK;;;;;;;;;;;;;;

;ncdf_varget,ncid2,'nn_Air_tracer',nn_Air_tracer
;ncdf_varget,ncid2,'xk_Air_tracer',xk_Air_tracer



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FOR MCF:
;ncdf_varget,ncid,'MCF_1           Tr',MCF_1   
;ncdf_varget,ncid,'MCF_2           Tr',MCF_1          

;print,CH4_A8_1[*,0,0,1]

;print,string(receptorname)    

;tracers=fltarr(nspec,nrcpt)


;    info: structure array of length nrecpt (number of receptor points) of type
;           {rcptname:'',xrcpt:0.,yrcpt:0.,zrcpt:0.,hxmax:0.,hymax:0.,hzmax:0.,time:0}
;           containing name and coordinates of receptor point and number of time points
;    data: structure array of length ntime (number of time points) of type
;           {pptv:DblArr(nspec,nrcpt),std:DblArr(nspec,nrcpt),avgnum:DblArr(nrcpt),$
;           kernweights:DblArr(nrcpt)}
;           containing the actual concentrations for the nspec different species
;           at the nrcpt different receptor points.

rec      = {rcptname:'',xrcpt:0.,yrcpt:0.,zrcpt:0.,hxmax:0.,hymax:0.,hzmax:0.,time:0}
  info     = replicate(rec,nrcpt)

rec  = {pptv:DblArr(nspec,nrcpt),std:DblArr(nspec,nrcpt),avgnum:DblArr(nrcpt),$
          kernweights:DblArr(nrcpt)}
  data = replicate(rec,ntime)


CH4_A1_1_mat[i,0,0,*]=CH4_A1_1
CH4_A1_2_mat[i,0,0,*]=CH4_A1_2
CH4_A1_3_mat[i,0,0,*]=CH4_A1_3
CH4_A1_4_mat[i,0,0,*]=CH4_A1_4
CH4_A1_5_mat[i,0,0,*]=CH4_A1_5
CH4_A2_1_mat[i,0,0,*]=CH4_A2_1
CH4_A2_2_mat[i,0,0,*]=CH4_A2_2
CH4_A2_3_mat[i,0,0,*]=CH4_A2_3
CH4_A2_4_mat[i,0,0,*]=CH4_A2_4
CH4_A2_5_mat[i,0,0,*]=CH4_A2_5
CH4_A3_1_mat[i,0,0,*]=CH4_A3_1
CH4_A3_2_mat[i,0,0,*]=CH4_A3_2
CH4_A3_3_mat[i,0,0,*]=CH4_A3_3
CH4_A3_4_mat[i,0,0,*]=CH4_A3_4
CH4_A3_5_mat[i,0,0,*]=CH4_A3_5
CH4_A4_1_mat[i,0,0,*]=CH4_A4_1
CH4_A4_2_mat[i,0,0,*]=CH4_A4_2
CH4_A4_3_mat[i,0,0,*]=CH4_A4_3
CH4_A4_4_mat[i,0,0,*]=CH4_A4_4
CH4_A4_5_mat[i,0,0,*]=CH4_A4_5
CH4_A5_1_mat[i,0,0,*]=CH4_A5_1
CH4_A5_2_mat[i,0,0,*]=CH4_A5_2
CH4_A5_3_mat[i,0,0,*]=CH4_A5_3
CH4_A5_4_mat[i,0,0,*]=CH4_A5_4
CH4_A5_5_mat[i,0,0,*]=CH4_A5_5
CH4_A6_1_mat[i,0,0,*]=CH4_A6_1
CH4_A6_2_mat[i,0,0,*]=CH4_A6_2
CH4_A6_3_mat[i,0,0,*]=CH4_A6_3
CH4_A6_4_mat[i,0,0,*]=CH4_A6_4
CH4_A6_5_mat[i,0,0,*]=CH4_A6_5
CH4_A7_1_mat[i,0,0,*]=CH4_A7_1
CH4_A7_2_mat[i,0,0,*]=CH4_A7_2
CH4_A7_3_mat[i,0,0,*]=CH4_A7_3
CH4_A7_4_mat[i,0,0,*]=CH4_A7_4
CH4_A7_5_mat[i,0,0,*]=CH4_A7_5
CH4_A8_1_mat[i,0,0,*]=CH4_A8_1
CH4_A8_2_mat[i,0,0,*]=CH4_A8_2
CH4_A8_3_mat[i,0,0,*]=CH4_A8_3
CH4_A8_4_mat[i,0,0,*]=CH4_A8_4
CH4_A8_5_mat[i,0,0,*]=CH4_A8_5
CH4_A9_1_mat[i,0,0,*]=CH4_A9_1
CH4_A9_2_mat[i,0,0,*]=CH4_A9_2
CH4_A9_3_mat[i,0,0,*]=CH4_A9_3
CH4_A9_4_mat[i,0,0,*]=CH4_A9_4
CH4_A9_5_mat[i,0,0,*]=CH4_A9_5
CH4_A10_1_mat[i,0,0,*]=CH4_A10_1
CH4_A10_2_mat[i,0,0,*]=CH4_A10_2
CH4_A10_3_mat[i,0,0,*]=CH4_A10_3
CH4_A10_4_mat[i,0,0,*]=CH4_A10_4
CH4_A10_5_mat[i,0,0,*]=CH4_A10_5
CH4_A11_1_mat[i,0,0,*]=CH4_A11_1
CH4_A11_2_mat[i,0,0,*]=CH4_A11_2
CH4_A11_3_mat[i,0,0,*]=CH4_A11_3
CH4_A11_4_mat[i,0,0,*]=CH4_A11_4
CH4_A11_5_mat[i,0,0,*]=CH4_A11_5

;;;;
CH4_B1_1_mat[i,0,0,*]=CH4_B1_1
CH4_B1_2_mat[i,0,0,*]=CH4_B1_2
CH4_B1_3_mat[i,0,0,*]=CH4_B1_3
CH4_B1_4_mat[i,0,0,*]=CH4_B1_4
CH4_B1_5_mat[i,0,0,*]=CH4_B1_5
CH4_B2_1_mat[i,0,0,*]=CH4_B2_1
CH4_B2_2_mat[i,0,0,*]=CH4_B2_2
CH4_B2_3_mat[i,0,0,*]=CH4_B2_3
CH4_B2_4_mat[i,0,0,*]=CH4_B2_4
CH4_B2_5_mat[i,0,0,*]=CH4_B2_5
CH4_B3_1_mat[i,0,0,*]=CH4_B3_1
CH4_B3_2_mat[i,0,0,*]=CH4_B3_2
CH4_B3_3_mat[i,0,0,*]=CH4_B3_3
CH4_B3_4_mat[i,0,0,*]=CH4_B3_4
CH4_B3_5_mat[i,0,0,*]=CH4_B3_5
CH4_B4_1_mat[i,0,0,*]=CH4_B4_1
CH4_B4_2_mat[i,0,0,*]=CH4_B4_2
CH4_B4_3_mat[i,0,0,*]=CH4_B4_3
CH4_B4_4_mat[i,0,0,*]=CH4_B4_4
CH4_B4_5_mat[i,0,0,*]=CH4_B4_5
CH4_B5_1_mat[i,0,0,*]=CH4_B5_1
CH4_B5_2_mat[i,0,0,*]=CH4_B5_2
CH4_B5_3_mat[i,0,0,*]=CH4_B5_3
CH4_B5_4_mat[i,0,0,*]=CH4_B5_4
CH4_B5_5_mat[i,0,0,*]=CH4_B5_5
CH4_B6_1_mat[i,0,0,*]=CH4_B6_1
CH4_B6_2_mat[i,0,0,*]=CH4_B6_2
CH4_B6_3_mat[i,0,0,*]=CH4_B6_3
CH4_B6_4_mat[i,0,0,*]=CH4_B6_4
CH4_B6_5_mat[i,0,0,*]=CH4_B6_5
CH4_B7_1_mat[i,0,0,*]=CH4_B7_1
CH4_B7_2_mat[i,0,0,*]=CH4_B7_2
CH4_B7_3_mat[i,0,0,*]=CH4_B7_3
CH4_B7_4_mat[i,0,0,*]=CH4_B7_4
CH4_B7_5_mat[i,0,0,*]=CH4_B7_5
CH4_B8_1_mat[i,0,0,*]=CH4_B8_1
CH4_B8_2_mat[i,0,0,*]=CH4_B8_2
CH4_B8_3_mat[i,0,0,*]=CH4_B8_3
CH4_B8_4_mat[i,0,0,*]=CH4_B8_4
CH4_B8_5_mat[i,0,0,*]=CH4_B8_5
CH4_B9_1_mat[i,0,0,*]=CH4_B9_1
CH4_B9_2_mat[i,0,0,*]=CH4_B9_2
CH4_B9_3_mat[i,0,0,*]=CH4_B9_3
CH4_B9_4_mat[i,0,0,*]=CH4_B9_4
CH4_B9_5_mat[i,0,0,*]=CH4_B9_5
CH4_B10_1_mat[i,0,0,*]=CH4_B10_1
CH4_B10_2_mat[i,0,0,*]=CH4_B10_2
CH4_B10_3_mat[i,0,0,*]=CH4_B10_3
CH4_B10_4_mat[i,0,0,*]=CH4_B10_4
CH4_B10_5_mat[i,0,0,*]=CH4_B10_5
CH4_B11_1_mat[i,0,0,*]=CH4_B11_1
CH4_B11_2_mat[i,0,0,*]=CH4_B11_2
CH4_B11_3_mat[i,0,0,*]=CH4_B11_3
CH4_B11_4_mat[i,0,0,*]=CH4_B11_4
CH4_B11_5_mat[i,0,0,*]=CH4_B11_5
CH4_B12_1_mat[i,0,0,*]=CH4_B12_1
CH4_B12_2_mat[i,0,0,*]=CH4_B12_2
CH4_B12_3_mat[i,0,0,*]=CH4_B12_3
CH4_B12_4_mat[i,0,0,*]=CH4_B12_4
CH4_B12_5_mat[i,0,0,*]=CH4_B12_5
CH4_B13_1_mat[i,0,0,*]=CH4_B13_1
CH4_B13_2_mat[i,0,0,*]=CH4_B13_2
CH4_B13_3_mat[i,0,0,*]=CH4_B13_3
CH4_B13_4_mat[i,0,0,*]=CH4_B13_4
CH4_B13_5_mat[i,0,0,*]=CH4_B13_5

;;;;
CH4_W1_1_mat[i,0,0,*]=CH4_W1_1
CH4_W1_2_mat[i,0,0,*]=CH4_W1_2
CH4_W1_3_mat[i,0,0,*]=CH4_W1_3
CH4_W1_4_mat[i,0,0,*]=CH4_W1_4
CH4_W1_5_mat[i,0,0,*]=CH4_W1_5
CH4_W2_1_mat[i,0,0,*]=CH4_W2_1
CH4_W2_2_mat[i,0,0,*]=CH4_W2_2
CH4_W2_3_mat[i,0,0,*]=CH4_W2_3
CH4_W2_4_mat[i,0,0,*]=CH4_W2_4
CH4_W2_5_mat[i,0,0,*]=CH4_W2_5
CH4_W3_1_mat[i,0,0,*]=CH4_W3_1
CH4_W3_2_mat[i,0,0,*]=CH4_W3_2
CH4_W3_3_mat[i,0,0,*]=CH4_W3_3
CH4_W3_4_mat[i,0,0,*]=CH4_W3_4
CH4_W3_5_mat[i,0,0,*]=CH4_W3_5
CH4_W4_1_mat[i,0,0,*]=CH4_W4_1
CH4_W4_2_mat[i,0,0,*]=CH4_W4_2
CH4_W4_3_mat[i,0,0,*]=CH4_W4_3
CH4_W4_4_mat[i,0,0,*]=CH4_W4_4
CH4_W4_5_mat[i,0,0,*]=CH4_W4_5
CH4_W5_1_mat[i,0,0,*]=CH4_W5_1
CH4_W5_2_mat[i,0,0,*]=CH4_W5_2
CH4_W5_3_mat[i,0,0,*]=CH4_W5_3
CH4_W5_4_mat[i,0,0,*]=CH4_W5_4
CH4_W5_5_mat[i,0,0,*]=CH4_W5_5
CH4_W6_1_mat[i,0,0,*]=CH4_W6_1
CH4_W6_2_mat[i,0,0,*]=CH4_W6_2
CH4_W6_3_mat[i,0,0,*]=CH4_W6_3
CH4_W6_4_mat[i,0,0,*]=CH4_W6_4
CH4_W6_5_mat[i,0,0,*]=CH4_A6_5
CH4_W7_1_mat[i,0,0,*]=CH4_W7_1
CH4_W7_2_mat[i,0,0,*]=CH4_W7_2
CH4_W7_3_mat[i,0,0,*]=CH4_W7_3
CH4_W7_4_mat[i,0,0,*]=CH4_W7_4
CH4_W7_5_mat[i,0,0,*]=CH4_W7_5
CH4_W8_1_mat[i,0,0,*]=CH4_W8_1
CH4_W8_2_mat[i,0,0,*]=CH4_W8_2
CH4_W8_3_mat[i,0,0,*]=CH4_W8_3
CH4_W8_4_mat[i,0,0,*]=CH4_W8_4
CH4_W8_5_mat[i,0,0,*]=CH4_W8_5
CH4_W9_1_mat[i,0,0,*]=CH4_W9_1
CH4_W9_2_mat[i,0,0,*]=CH4_W9_2
CH4_W9_3_mat[i,0,0,*]=CH4_W9_3
CH4_W9_4_mat[i,0,0,*]=CH4_W9_4
CH4_W9_5_mat[i,0,0,*]=CH4_W9_5
CH4_W10_1_mat[i,0,0,*]=CH4_W10_1
CH4_W10_2_mat[i,0,0,*]=CH4_W10_2
CH4_W10_3_mat[i,0,0,*]=CH4_W10_3
CH4_W10_4_mat[i,0,0,*]=CH4_W10_4
CH4_W10_5_mat[i,0,0,*]=CH4_W10_5
CH4_W11_1_mat[i,0,0,*]=CH4_W11_1
CH4_W11_2_mat[i,0,0,*]=CH4_W11_2
CH4_W11_3_mat[i,0,0,*]=CH4_W11_3
CH4_W11_4_mat[i,0,0,*]=CH4_W11_4
CH4_W11_5_mat[i,0,0,*]=CH4_W11_5
CH4_W12_1_mat[i,0,0,*]=CH4_W12_1
CH4_W12_2_mat[i,0,0,*]=CH4_W12_2
CH4_W12_3_mat[i,0,0,*]=CH4_W12_3
CH4_W12_4_mat[i,0,0,*]=CH4_W12_4
CH4_W12_5_mat[i,0,0,*]=CH4_W12_5
CH4_W13_1_mat[i,0,0,*]=CH4_W13_1
CH4_W13_2_mat[i,0,0,*]=CH4_W13_2
CH4_W13_3_mat[i,0,0,*]=CH4_W13_3
CH4_W13_4_mat[i,0,0,*]=CH4_W13_4
CH4_W13_5_mat[i,0,0,*]=CH4_W13_5

;;;;
CH4_R1_1_mat[i,0,0,*]=CH4_R1_1
CH4_R1_2_mat[i,0,0,*]=CH4_R1_2
CH4_R1_3_mat[i,0,0,*]=CH4_R1_3
CH4_R1_4_mat[i,0,0,*]=CH4_R1_4
CH4_R1_5_mat[i,0,0,*]=CH4_R1_5
CH4_R2_1_mat[i,0,0,*]=CH4_R2_1
CH4_R2_2_mat[i,0,0,*]=CH4_R2_2
CH4_R2_3_mat[i,0,0,*]=CH4_R2_3
CH4_R2_4_mat[i,0,0,*]=CH4_R2_4
CH4_R2_5_mat[i,0,0,*]=CH4_R2_5
CH4_R3_1_mat[i,0,0,*]=CH4_R3_1
CH4_R3_2_mat[i,0,0,*]=CH4_R3_2
CH4_R3_3_mat[i,0,0,*]=CH4_R3_3
CH4_R3_4_mat[i,0,0,*]=CH4_R3_4
CH4_R3_5_mat[i,0,0,*]=CH4_R3_5
CH4_R4_1_mat[i,0,0,*]=CH4_R4_1
CH4_R4_2_mat[i,0,0,*]=CH4_R4_2
CH4_R4_3_mat[i,0,0,*]=CH4_R4_3
CH4_R4_4_mat[i,0,0,*]=CH4_R4_4
CH4_R4_5_mat[i,0,0,*]=CH4_R4_5
CH4_R5_1_mat[i,0,0,*]=CH4_R5_1
CH4_R5_2_mat[i,0,0,*]=CH4_R5_2
CH4_R5_3_mat[i,0,0,*]=CH4_R5_3
CH4_R5_4_mat[i,0,0,*]=CH4_R5_4
CH4_R5_5_mat[i,0,0,*]=CH4_R5_5
CH4_R6_1_mat[i,0,0,*]=CH4_R6_1
CH4_R6_2_mat[i,0,0,*]=CH4_R6_2
CH4_R6_3_mat[i,0,0,*]=CH4_R6_3
CH4_R6_4_mat[i,0,0,*]=CH4_R6_4
CH4_R6_5_mat[i,0,0,*]=CH4_A6_5
CH4_R7_1_mat[i,0,0,*]=CH4_R7_1
CH4_R7_2_mat[i,0,0,*]=CH4_R7_2
CH4_R7_3_mat[i,0,0,*]=CH4_R7_3
CH4_R7_4_mat[i,0,0,*]=CH4_R7_4
CH4_R7_5_mat[i,0,0,*]=CH4_R7_5

CH4_WAN_1_mat[i,0,0,*]=CH4_WAN_1
CH4_WAN_2_mat[i,0,0,*]=CH4_WAN_2
CH4_WAN_3_mat[i,0,0,*]=CH4_WAN_3
CH4_WAN_4_mat[i,0,0,*]=CH4_WAN_4
CH4_WAN_5_mat[i,0,0,*]=CH4_WAN_5

CH4_OCE_1_mat[i,0,0,*]=CH4_OCE_1
CH4_OCE_2_mat[i,0,0,*]=CH4_OCE_2
CH4_OCE_3_mat[i,0,0,*]=CH4_OCE_3
CH4_OCE_4_mat[i,0,0,*]=CH4_OCE_4
CH4_OCE_5_mat[i,0,0,*]=CH4_OCE_5

CH4_TER_1_mat[i,0,0,*]=CH4_TER_1
CH4_TER_2_mat[i,0,0,*]=CH4_TER_2
CH4_TER_3_mat[i,0,0,*]=CH4_TER_3
CH4_TER_4_mat[i,0,0,*]=CH4_TER_4
CH4_TER_5_mat[i,0,0,*]=CH4_TER_5

CH4_VOL_1_mat[i,0,0,*]=CH4_VOL_1
CH4_VOL_2_mat[i,0,0,*]=CH4_VOL_2
CH4_VOL_3_mat[i,0,0,*]=CH4_VOL_3
CH4_VOL_4_mat[i,0,0,*]=CH4_VOL_4
CH4_VOL_5_mat[i,0,0,*]=CH4_VOL_5


;ncdf_close,ncid
;
ENDFOR ;test
;stop


FOR it=0,ntime-1 DO BEGIN
;    FOR i=0,nspec-1 DO BEGIN

;data[it].pptv[0,*]=Air_tracer[*,0,0,it]
data[it].pptv[0,*]=0

;;;AGECLASSE 1;;; 

;CH4_A1_1_mat=DblArr(ncrpt,1,1,ntime)
;FOR i=0,nrcpt-1 DO BEGIN
;CH4_A1_1_mat[i,1,1,it]=CH4_A1_1


data[it].pptv[1,*]=CH4_A1_1_mat[*,0,0,it]
data[it].pptv[2,*]=CH4_A2_1_mat[*,0,0,it]
data[it].pptv[3,*]=CH4_A3_1_mat[*,0,0,it]
data[it].pptv[4,*]=CH4_A4_1_mat[*,0,0,it]
data[it].pptv[5,*]=CH4_A5_1_mat[*,0,0,it]
data[it].pptv[6,*]=CH4_A6_1_mat[*,0,0,it]
data[it].pptv[7,*]=CH4_A7_1_mat[*,0,0,it]
data[it].pptv[8,*]=CH4_A8_1_mat[*,0,0,it]
data[it].pptv[9,*]=CH4_A9_1_mat[*,0,0,it]
data[it].pptv[10,*]=CH4_A10_1_mat[*,0,0,it]
data[it].pptv[11,*]=CH4_A11_1_mat[*,0,0,it]

data[it].pptv[12,*]=CH4_B1_1_mat[*,0,0,it]
data[it].pptv[13,*]=CH4_B2_1_mat[*,0,0,it]
data[it].pptv[14,*]=CH4_B3_1_mat[*,0,0,it]
data[it].pptv[15,*]=CH4_B4_1_mat[*,0,0,it]
data[it].pptv[16,*]=CH4_B5_1_mat[*,0,0,it]
data[it].pptv[17,*]=CH4_B6_1_mat[*,0,0,it]
data[it].pptv[18,*]=CH4_B7_1_mat[*,0,0,it]
data[it].pptv[19,*]=CH4_B8_1_mat[*,0,0,it]
data[it].pptv[20,*]=CH4_B9_1_mat[*,0,0,it]
data[it].pptv[21,*]=CH4_B10_1_mat[*,0,0,it]
data[it].pptv[22,*]=CH4_B11_1_mat[*,0,0,it]
data[it].pptv[23,*]=CH4_B12_1_mat[*,0,0,it]
data[it].pptv[24,*]=CH4_B13_1_mat[*,0,0,it]

data[it].pptv[25,*]=CH4_R1_1_mat[*,0,0,it]
data[it].pptv[26,*]=CH4_R2_1_mat[*,0,0,it]
data[it].pptv[27,*]=CH4_R3_1_mat[*,0,0,it]
data[it].pptv[28,*]=CH4_R4_1_mat[*,0,0,it]
data[it].pptv[29,*]=CH4_R5_1_mat[*,0,0,it]
data[it].pptv[30,*]=CH4_R6_1_mat[*,0,0,it]
data[it].pptv[31,*]=CH4_R7_1_mat[*,0,0,it]

data[it].pptv[32,*]=CH4_W1_1_mat[*,0,0,it]
data[it].pptv[33,*]=CH4_W2_1_mat[*,0,0,it]
data[it].pptv[34,*]=CH4_W3_1_mat[*,0,0,it]
data[it].pptv[35,*]=CH4_W4_1_mat[*,0,0,it]
data[it].pptv[36,*]=CH4_W5_1_mat[*,0,0,it]
data[it].pptv[37,*]=CH4_W6_1_mat[*,0,0,it]
data[it].pptv[38,*]=CH4_W7_1_mat[*,0,0,it]
data[it].pptv[39,*]=CH4_W8_1_mat[*,0,0,it]
data[it].pptv[40,*]=CH4_W9_1_mat[*,0,0,it]
data[it].pptv[41,*]=CH4_W10_1_mat[*,0,0,it]
data[it].pptv[42,*]=CH4_W11_1_mat[*,0,0,it]
data[it].pptv[43,*]=CH4_W12_1_mat[*,0,0,it]
data[it].pptv[44,*]=CH4_W13_1_mat[*,0,0,it]

data[it].pptv[45,*]=CH4_WAN_1_mat[*,0,0,it]
data[it].pptv[46,*]=CH4_TER_1_mat[*,0,0,it]
data[it].pptv[47,*]=CH4_OCE_1_mat[*,0,0,it]
data[it].pptv[48,*]=CH4_VOL_1_mat[*,0,0,it]

;;;AGECLASSE 2;;; 

data[it].pptv[49,*]=CH4_A1_2_mat[*,0,0,it]
data[it].pptv[50,*]=CH4_A2_2_mat[*,0,0,it]
data[it].pptv[51,*]=CH4_A3_2_mat[*,0,0,it]
data[it].pptv[52,*]=CH4_A4_2_mat[*,0,0,it]
data[it].pptv[53,*]=CH4_A5_2_mat[*,0,0,it]
data[it].pptv[54,*]=CH4_A6_2_mat[*,0,0,it]
data[it].pptv[55,*]=CH4_A7_2_mat[*,0,0,it]
data[it].pptv[56,*]=CH4_A8_2_mat[*,0,0,it]
data[it].pptv[57,*]=CH4_A9_2_mat[*,0,0,it]
data[it].pptv[58,*]=CH4_A10_2_mat[*,0,0,it]
data[it].pptv[59,*]=CH4_A11_2_mat[*,0,0,it]

data[it].pptv[60,*]=CH4_B1_2_mat[*,0,0,it] ; no B-W replace in DLR
data[it].pptv[61,*]=CH4_B2_2_mat[*,0,0,it]
data[it].pptv[62,*]=CH4_B3_2_mat[*,0,0,it]
data[it].pptv[63,*]=CH4_B4_2_mat[*,0,0,it]
data[it].pptv[64,*]=CH4_B5_2_mat[*,0,0,it]
data[it].pptv[65,*]=CH4_B6_2_mat[*,0,0,it]
data[it].pptv[66,*]=CH4_B7_2_mat[*,0,0,it]
data[it].pptv[67,*]=CH4_B8_2_mat[*,0,0,it]
data[it].pptv[68,*]=CH4_B9_2_mat[*,0,0,it]
data[it].pptv[69,*]=CH4_B10_2_mat[*,0,0,it]
data[it].pptv[70,*]=CH4_B11_2_mat[*,0,0,it]
data[it].pptv[71,*]=CH4_B12_2_mat[*,0,0,it]
data[it].pptv[72,*]=CH4_B13_2_mat[*,0,0,it] ;flo

data[it].pptv[73,*]=CH4_R1_2_mat[*,0,0,it]
data[it].pptv[74,*]=CH4_R2_2_mat[*,0,0,it]
data[it].pptv[75,*]=CH4_R3_2_mat[*,0,0,it]
data[it].pptv[76,*]=CH4_R4_2_mat[*,0,0,it]
data[it].pptv[77,*]=CH4_R5_2_mat[*,0,0,it]
data[it].pptv[78,*]=CH4_R6_2_mat[*,0,0,it]
data[it].pptv[79,*]=CH4_R7_2_mat[*,0,0,it]

data[it].pptv[80,*]=CH4_W1_2_mat[*,0,0,it] ;flo
data[it].pptv[81,*]=CH4_W2_2_mat[*,0,0,it]
data[it].pptv[82,*]=CH4_W3_2_mat[*,0,0,it]
data[it].pptv[83,*]=CH4_W4_2_mat[*,0,0,it]
data[it].pptv[84,*]=CH4_W5_2_mat[*,0,0,it]
data[it].pptv[85,*]=CH4_W6_2_mat[*,0,0,it]
data[it].pptv[86,*]=CH4_W7_2_mat[*,0,0,it]
data[it].pptv[87,*]=CH4_W8_2_mat[*,0,0,it]
data[it].pptv[88,*]=CH4_W9_2_mat[*,0,0,it]
data[it].pptv[89,*]=CH4_W10_2_mat[*,0,0,it]
data[it].pptv[90,*]=CH4_W11_2_mat[*,0,0,it]
data[it].pptv[91,*]=CH4_W12_2_mat[*,0,0,it]
data[it].pptv[92,*]=CH4_W13_2_mat[*,0,0,it] ;flo

data[it].pptv[93,*]=CH4_WAN_2_mat[*,0,0,it]
data[it].pptv[94,*]=CH4_TER_2_mat[*,0,0,it]
data[it].pptv[95,*]=CH4_OCE_2_mat[*,0,0,it]
data[it].pptv[96,*]=CH4_VOL_2_mat[*,0,0,it]


;;;AGECLASSE 3;;; 

data[it].pptv[97,*]=CH4_A1_3_mat[*,0,0,it]
data[it].pptv[98,*]=CH4_A2_3_mat[*,0,0,it]
data[it].pptv[99,*]=CH4_A3_3_mat[*,0,0,it]
data[it].pptv[100,*]=CH4_A4_3_mat[*,0,0,it]
data[it].pptv[101,*]=CH4_A5_3_mat[*,0,0,it]
data[it].pptv[102,*]=CH4_A6_3_mat[*,0,0,it]
data[it].pptv[103,*]=CH4_A7_3_mat[*,0,0,it]
data[it].pptv[104,*]=CH4_A8_3_mat[*,0,0,it]
data[it].pptv[105,*]=CH4_A9_3_mat[*,0,0,it]
data[it].pptv[106,*]=CH4_A10_3_mat[*,0,0,it]
data[it].pptv[107,*]=CH4_A11_3_mat[*,0,0,it]

data[it].pptv[108,*]=CH4_B1_3_mat[*,0,0,it]
data[it].pptv[109,*]=CH4_B2_3_mat[*,0,0,it]
data[it].pptv[110,*]=CH4_B3_3_mat[*,0,0,it]
data[it].pptv[111,*]=CH4_B4_3_mat[*,0,0,it]
data[it].pptv[112,*]=CH4_B5_3_mat[*,0,0,it]
data[it].pptv[113,*]=CH4_B6_3_mat[*,0,0,it]
data[it].pptv[114,*]=CH4_B7_3_mat[*,0,0,it]
data[it].pptv[115,*]=CH4_B8_3_mat[*,0,0,it]
data[it].pptv[116,*]=CH4_B9_3_mat[*,0,0,it]
data[it].pptv[117,*]=CH4_B10_3_mat[*,0,0,it]
data[it].pptv[118,*]=CH4_B11_3_mat[*,0,0,it]
data[it].pptv[119,*]=CH4_B12_3_mat[*,0,0,it]
data[it].pptv[120,*]=CH4_B13_3_mat[*,0,0,it]

data[it].pptv[121,*]=CH4_R1_3_mat[*,0,0,it]
data[it].pptv[122,*]=CH4_R2_3_mat[*,0,0,it]
data[it].pptv[123,*]=CH4_R3_3_mat[*,0,0,it]
data[it].pptv[124,*]=CH4_R4_3_mat[*,0,0,it]
data[it].pptv[125,*]=CH4_R5_3_mat[*,0,0,it]
data[it].pptv[126,*]=CH4_R6_3_mat[*,0,0,it]
data[it].pptv[127,*]=CH4_R7_3_mat[*,0,0,it]

data[it].pptv[128,*]=CH4_W1_3_mat[*,0,0,it]
data[it].pptv[129,*]=CH4_W2_3_mat[*,0,0,it]
data[it].pptv[130,*]=CH4_W3_3_mat[*,0,0,it]
data[it].pptv[131,*]=CH4_W4_3_mat[*,0,0,it]
data[it].pptv[132,*]=CH4_W5_3_mat[*,0,0,it]
data[it].pptv[133,*]=CH4_W6_3_mat[*,0,0,it]
data[it].pptv[134,*]=CH4_W7_3_mat[*,0,0,it]
data[it].pptv[135,*]=CH4_W8_3_mat[*,0,0,it]
data[it].pptv[136,*]=CH4_W9_3_mat[*,0,0,it]
data[it].pptv[137,*]=CH4_W10_3_mat[*,0,0,it]
data[it].pptv[138,*]=CH4_W11_3_mat[*,0,0,it]
data[it].pptv[139,*]=CH4_W12_3_mat[*,0,0,it]
data[it].pptv[140,*]=CH4_W13_3_mat[*,0,0,it]

data[it].pptv[141,*]=CH4_WAN_3_mat[*,0,0,it]
data[it].pptv[142,*]=CH4_TER_3_mat[*,0,0,it]
data[it].pptv[143,*]=CH4_OCE_3_mat[*,0,0,it]
data[it].pptv[144,*]=CH4_VOL_3_mat[*,0,0,it]

;;;AGECLASSE 4;;; 

data[it].pptv[145,*]=CH4_A1_4_mat[*,0,0,it]
data[it].pptv[146,*]=CH4_A2_4_mat[*,0,0,it]
data[it].pptv[147,*]=CH4_A3_4_mat[*,0,0,it]
data[it].pptv[148,*]=CH4_A4_4_mat[*,0,0,it]
data[it].pptv[149,*]=CH4_A5_4_mat[*,0,0,it]
data[it].pptv[150,*]=CH4_A6_4_mat[*,0,0,it]
data[it].pptv[151,*]=CH4_A7_4_mat[*,0,0,it]
data[it].pptv[152,*]=CH4_A8_4_mat[*,0,0,it]
data[it].pptv[153,*]=CH4_A9_4_mat[*,0,0,it]
data[it].pptv[154,*]=CH4_A10_4_mat[*,0,0,it]
data[it].pptv[155,*]=CH4_A11_4_mat[*,0,0,it]

data[it].pptv[156,*]=CH4_B1_4_mat[*,0,0,it]
data[it].pptv[157,*]=CH4_B2_4_mat[*,0,0,it]
data[it].pptv[158,*]=CH4_B3_4_mat[*,0,0,it]
data[it].pptv[159,*]=CH4_B4_4_mat[*,0,0,it]
data[it].pptv[160,*]=CH4_B5_4_mat[*,0,0,it]
data[it].pptv[161,*]=CH4_B6_4_mat[*,0,0,it]
data[it].pptv[162,*]=CH4_B7_4_mat[*,0,0,it]
data[it].pptv[163,*]=CH4_B8_4_mat[*,0,0,it]
data[it].pptv[164,*]=CH4_B9_4_mat[*,0,0,it]
data[it].pptv[165,*]=CH4_B10_4_mat[*,0,0,it]
data[it].pptv[166,*]=CH4_B11_4_mat[*,0,0,it]
data[it].pptv[167,*]=CH4_B12_4_mat[*,0,0,it]
data[it].pptv[168,*]=CH4_B13_4_mat[*,0,0,it]

data[it].pptv[169,*]=CH4_R1_4_mat[*,0,0,it]
data[it].pptv[170,*]=CH4_R2_4_mat[*,0,0,it]
data[it].pptv[171,*]=CH4_R3_4_mat[*,0,0,it]
data[it].pptv[172,*]=CH4_R4_4_mat[*,0,0,it]
data[it].pptv[173,*]=CH4_R5_4_mat[*,0,0,it]
data[it].pptv[174,*]=CH4_R6_4_mat[*,0,0,it]
data[it].pptv[175,*]=CH4_R7_4_mat[*,0,0,it]

data[it].pptv[176,*]=CH4_W1_4_mat[*,0,0,it]
data[it].pptv[177,*]=CH4_W2_4_mat[*,0,0,it]
data[it].pptv[178,*]=CH4_W3_4_mat[*,0,0,it]
data[it].pptv[179,*]=CH4_W4_4_mat[*,0,0,it]
data[it].pptv[180,*]=CH4_W5_4_mat[*,0,0,it]
data[it].pptv[181,*]=CH4_W6_4_mat[*,0,0,it]
data[it].pptv[182,*]=CH4_W7_4_mat[*,0,0,it]
data[it].pptv[183,*]=CH4_W8_4_mat[*,0,0,it]
data[it].pptv[184,*]=CH4_W9_4_mat[*,0,0,it]
data[it].pptv[185,*]=CH4_W10_4_mat[*,0,0,it]
data[it].pptv[186,*]=CH4_W11_4_mat[*,0,0,it]
data[it].pptv[187,*]=CH4_W12_4_mat[*,0,0,it]
data[it].pptv[188,*]=CH4_W13_4_mat[*,0,0,it]

data[it].pptv[189,*]=CH4_WAN_4_mat[*,0,0,it]
data[it].pptv[190,*]=CH4_TER_4_mat[*,0,0,it]
data[it].pptv[191,*]=CH4_OCE_4_mat[*,0,0,it]
data[it].pptv[192,*]=CH4_VOL_4_mat[*,0,0,it]

;;;AGECLASSE 5;;; 

data[it].pptv[193,*]=CH4_A1_5_mat[*,0,0,it]
data[it].pptv[194,*]=CH4_A2_5_mat[*,0,0,it]
data[it].pptv[195,*]=CH4_A3_5_mat[*,0,0,it]
data[it].pptv[196,*]=CH4_A4_5_mat[*,0,0,it]
data[it].pptv[197,*]=CH4_A5_5_mat[*,0,0,it]
data[it].pptv[198,*]=CH4_A6_5_mat[*,0,0,it]
data[it].pptv[199,*]=CH4_A7_5_mat[*,0,0,it]
data[it].pptv[200,*]=CH4_A8_5_mat[*,0,0,it]
data[it].pptv[201,*]=CH4_A9_5_mat[*,0,0,it]
data[it].pptv[202,*]=CH4_A10_5_mat[*,0,0,it]
data[it].pptv[203,*]=CH4_A11_5_mat[*,0,0,it]

data[it].pptv[204,*]=CH4_B1_5_mat[*,0,0,it]
data[it].pptv[205,*]=CH4_B2_5_mat[*,0,0,it]
data[it].pptv[206,*]=CH4_B3_5_mat[*,0,0,it]
data[it].pptv[207,*]=CH4_B4_5_mat[*,0,0,it]
data[it].pptv[208,*]=CH4_B5_5_mat[*,0,0,it]
data[it].pptv[209,*]=CH4_B6_5_mat[*,0,0,it]
data[it].pptv[210,*]=CH4_B7_5_mat[*,0,0,it]
data[it].pptv[211,*]=CH4_B8_5_mat[*,0,0,it]
data[it].pptv[212,*]=CH4_B9_5_mat[*,0,0,it]
data[it].pptv[213,*]=CH4_B10_5_mat[*,0,0,it]
data[it].pptv[214,*]=CH4_B11_5_mat[*,0,0,it]
data[it].pptv[215,*]=CH4_B12_5_mat[*,0,0,it]
data[it].pptv[216,*]=CH4_B13_5_mat[*,0,0,it]

data[it].pptv[217,*]=CH4_R1_5_mat[*,0,0,it]
data[it].pptv[218,*]=CH4_R2_5_mat[*,0,0,it]
data[it].pptv[219,*]=CH4_R3_5_mat[*,0,0,it]
data[it].pptv[220,*]=CH4_R4_5_mat[*,0,0,it]
data[it].pptv[221,*]=CH4_R5_5_mat[*,0,0,it]
data[it].pptv[222,*]=CH4_R6_5_mat[*,0,0,it]
data[it].pptv[223,*]=CH4_R7_5_mat[*,0,0,it]

data[it].pptv[224,*]=CH4_W1_5_mat[*,0,0,it]
data[it].pptv[225,*]=CH4_W2_5_mat[*,0,0,it]
data[it].pptv[226,*]=CH4_W3_5_mat[*,0,0,it]
data[it].pptv[227,*]=CH4_W4_5_mat[*,0,0,it]
data[it].pptv[228,*]=CH4_W5_5_mat[*,0,0,it]
data[it].pptv[229,*]=CH4_W6_5_mat[*,0,0,it]
data[it].pptv[230,*]=CH4_W7_5_mat[*,0,0,it]
data[it].pptv[231,*]=CH4_W8_5_mat[*,0,0,it]
data[it].pptv[232,*]=CH4_W9_5_mat[*,0,0,it]
data[it].pptv[233,*]=CH4_W10_5_mat[*,0,0,it]
data[it].pptv[234,*]=CH4_W11_5_mat[*,0,0,it]
data[it].pptv[235,*]=CH4_W12_5_mat[*,0,0,it]
data[it].pptv[236,*]=CH4_W13_5_mat[*,0,0,it]

data[it].pptv[237,*]=CH4_WAN_5_mat[*,0,0,it]
data[it].pptv[238,*]=CH4_TER_5_mat[*,0,0,it]
data[it].pptv[239,*]=CH4_OCE_5_mat[*,0,0,it]
data[it].pptv[240,*]=CH4_VOL_5_mat[*,0,0,it]


;stop

;;;;;;;;;;;;;;;;;;SD;;;;;;;;;;;;;;;;;;


data[it].std[0,*]=0;sd_Air_tracer[*,0,0,it]/16

;;;AGECLASSE 1;;; 

data[it].std[1,*]=0;sd_CH4_A1_1[*,0,0,it]/16
data[it].std[2,*]=0;sd_CH4_A2_1[*,0,0,it]/16
data[it].std[3,*]=0;sd_CH4_A3_1[*,0,0,it]/16
data[it].std[4,*]=0;sd_CH4_A4_1[*,0,0,it]/16
data[it].std[5,*]=0;sd_CH4_A5_1[*,0,0,it]/16
data[it].std[6,*]=0;sd_CH4_A6_1[*,0,0,it]/16
data[it].std[7,*]=0;sd_CH4_A7_1[*,0,0,it]/16
data[it].std[8,*]=0;sd_CH4_A8_1[*,0,0,it]/16
data[it].std[9,*]=0;sd_CH4_A9_1[*,0,0,it]/16
data[it].std[10,*]=0;sd_CH4_A10_1[*,0,0,it]/16
data[it].std[11,*]=0;sd_CH4_A11_1[*,0,0,it]/16

data[it].std[12,*]=0;sd_CH4_B1_1[*,0,0,it]/16
data[it].std[13,*]=0;sd_CH4_B2_1[*,0,0,it]/16
data[it].std[14,*]=0;sd_CH4_B3_1[*,0,0,it]/16
data[it].std[15,*]=0;sd_CH4_B4_1[*,0,0,it]/16
data[it].std[16,*]=0;sd_CH4_B5_1[*,0,0,it]/16
data[it].std[17,*]=0;sd_CH4_B6_1[*,0,0,it]/16
data[it].std[18,*]=0;sd_CH4_B7_1[*,0,0,it]/16
data[it].std[19,*]=0;sd_CH4_B8_1[*,0,0,it]/16
data[it].std[20,*]=0;sd_CH4_B9_1[*,0,0,it]/16
data[it].std[21,*]=0;sd_CH4_B10_1[*,0,0,it]/16
data[it].std[22,*]=0;sd_CH4_B11_1[*,0,0,it]/16
data[it].std[23,*]=0;sd_CH4_B12_1[*,0,0,it]/16
data[it].std[24,*]=0;sd_CH4_B13_1[*,0,0,it]/16

data[it].std[25,*]=0;sd_CH4_R1_1[*,0,0,it]/16
data[it].std[26,*]=0;sd_CH4_R2_1[*,0,0,it]/16
data[it].std[27,*]=0;sd_CH4_R3_1[*,0,0,it]/16
data[it].std[28,*]=0;sd_CH4_R4_1[*,0,0,it]/16
data[it].std[29,*]=0;sd_CH4_R5_1[*,0,0,it]/16
data[it].std[30,*]=0;sd_CH4_R6_1[*,0,0,it]/16
data[it].std[31,*]=0;sd_CH4_R7_1[*,0,0,it]/16

data[it].std[32,*]=0;sd_CH4_W1_1[*,0,0,it]/16
data[it].std[33,*]=0;sd_CH4_W2_1[*,0,0,it]/16
data[it].std[34,*]=0;sd_CH4_W3_1[*,0,0,it]/16
data[it].std[35,*]=0;sd_CH4_W4_1[*,0,0,it]/16
data[it].std[36,*]=0;sd_CH4_W5_1[*,0,0,it]/16
data[it].std[37,*]=0;sd_CH4_W6_1[*,0,0,it]/16
data[it].std[38,*]=0;sd_CH4_W7_1[*,0,0,it]/16
data[it].std[39,*]=0;sd_CH4_W8_1[*,0,0,it]/16
data[it].std[40,*]=0;sd_CH4_W9_1[*,0,0,it]/16
data[it].std[41,*]=0;sd_CH4_W10_1[*,0,0,it]/16
data[it].std[42,*]=0;sd_CH4_W11_1[*,0,0,it]/16
data[it].std[43,*]=0;sd_CH4_W12_1[*,0,0,it]/16
data[it].std[44,*]=0;sd_CH4_W13_1[*,0,0,it]/16

data[it].std[45,*]=0;sd_CH4_WAN_1[*,0,0,it]/16
data[it].std[46,*]=0;sd_CH4_TER_1[*,0,0,it]/16
data[it].std[47,*]=0;sd_CH4_OCE_1[*,0,0,it]/16
data[it].std[48,*]=0;sd_CH4_VOL_1[*,0,0,it]/16

;;;AGECLASSE 2;;; 

data[it].std[49,*]=0;sd_CH4_A1_2[*,0,0,it]/16
data[it].std[50,*]=0;sd_CH4_A2_2[*,0,0,it]/16
data[it].std[51,*]=0;sd_CH4_A3_2[*,0,0,it]/16
data[it].std[52,*]=0;sd_CH4_A4_2[*,0,0,it]/16
data[it].std[53,*]=0;sd_CH4_A5_2[*,0,0,it]/16
data[it].std[54,*]=0;sd_CH4_A6_2[*,0,0,it]/16
data[it].std[55,*]=0;sd_CH4_A7_2[*,0,0,it]/16
data[it].std[56,*]=0;sd_CH4_A8_2[*,0,0,it]/16
data[it].std[57,*]=0;sd_CH4_A9_2[*,0,0,it]/16
data[it].std[58,*]=0;sd_CH4_A10_2[*,0,0,it]/16
data[it].std[59,*]=0;sd_CH4_A11_2[*,0,0,it]/16

data[it].std[60,*]=0;sd_CH4_W1_2[*,0,0,it]/16
data[it].std[61,*]=0;sd_CH4_W2_2[*,0,0,it]/16
data[it].std[62,*]=0;sd_CH4_W3_2[*,0,0,it]/16
data[it].std[63,*]=0;sd_CH4_W4_2[*,0,0,it]/16
data[it].std[64,*]=0;sd_CH4_W5_2[*,0,0,it]/16
data[it].std[65,*]=0;sd_CH4_W6_2[*,0,0,it]/16
data[it].std[66,*]=0;sd_CH4_W7_2[*,0,0,it]/16
data[it].std[67,*]=0;sd_CH4_W8_2[*,0,0,it]/16
data[it].std[68,*]=0;sd_CH4_W9_2[*,0,0,it]/16
data[it].std[69,*]=0;sd_CH4_W10_2[*,0,0,it]/16
data[it].std[70,*]=0;sd_CH4_W11_2[*,0,0,it]/16
data[it].std[71,*]=0;sd_CH4_W12_2[*,0,0,it]/16
data[it].std[72,*]=0;sd_CH4_W13_2[*,0,0,it]/16

data[it].std[73,*]=0;sd_CH4_R1_2[*,0,0,it]/16
data[it].std[74,*]=0;sd_CH4_R2_2[*,0,0,it]/16
data[it].std[75,*]=0;sd_CH4_R3_2[*,0,0,it]/16
data[it].std[76,*]=0;sd_CH4_R4_2[*,0,0,it]/16
data[it].std[77,*]=0;sd_CH4_R5_2[*,0,0,it]/16
data[it].std[78,*]=0;sd_CH4_R6_2[*,0,0,it]/16
data[it].std[79,*]=0;sd_CH4_R7_2[*,0,0,it]/16

data[it].std[80,*]=0;sd_CH4_B1_2[*,0,0,it]/16
data[it].std[81,*]=0;sd_CH4_B2_2[*,0,0,it]/16
data[it].std[82,*]=0;sd_CH4_B3_2[*,0,0,it]/16
data[it].std[83,*]=0;sd_CH4_B4_2[*,0,0,it]/16
data[it].std[84,*]=0;sd_CH4_B5_2[*,0,0,it]/16
data[it].std[85,*]=0;sd_CH4_B6_2[*,0,0,it]/16
data[it].std[86,*]=0;sd_CH4_B7_2[*,0,0,it]/16
data[it].std[87,*]=0;sd_CH4_B8_2[*,0,0,it]/16
data[it].std[88,*]=0;sd_CH4_B9_2[*,0,0,it]/16
data[it].std[89,*]=0;sd_CH4_B10_2[*,0,0,it]/16
data[it].std[90,*]=0;sd_CH4_B11_2[*,0,0,it]/16
data[it].std[91,*]=0;sd_CH4_B12_2[*,0,0,it]/16
data[it].std[92,*]=0;sd_CH4_B13_2[*,0,0,it]/16

data[it].std[93,*]=0;sd_CH4_WAN_2[*,0,0,it]/16
data[it].std[94,*]=0;sd_CH4_TER_2[*,0,0,it]/16
data[it].std[95,*]=0;sd_CH4_OCE_2[*,0,0,it]/16
data[it].std[96,*]=0;sd_CH4_VOL_2[*,0,0,it]/16


;;;AGECLASSE 3;;; 

data[it].std[97,*]=0;sd_CH4_A1_3[*,0,0,it]/16
data[it].std[98,*]=0;sd_CH4_A2_3[*,0,0,it]/16
data[it].std[99,*]=0;sd_CH4_A3_3[*,0,0,it]/16
data[it].std[100,*]=0;sd_CH4_A4_3[*,0,0,it]/16
data[it].std[101,*]=0;sd_CH4_A5_3[*,0,0,it]/16
data[it].std[102,*]=0;sd_CH4_A6_3[*,0,0,it]/16
data[it].std[103,*]=0;sd_CH4_A7_3[*,0,0,it]/16
data[it].std[104,*]=0;sd_CH4_A8_3[*,0,0,it]/16
data[it].std[105,*]=0;sd_CH4_A9_3[*,0,0,it]/16
data[it].std[106,*]=0;sd_CH4_A10_3[*,0,0,it]/16
data[it].std[107,*]=0;sd_CH4_A11_3[*,0,0,it]/16

data[it].std[108,*]=0;sd_CH4_B1_3[*,0,0,it]/16
data[it].std[109,*]=0;sd_CH4_B2_3[*,0,0,it]/16
data[it].std[110,*]=0;sd_CH4_B3_3[*,0,0,it]/16
data[it].std[111,*]=0;sd_CH4_B4_3[*,0,0,it]/16
data[it].std[112,*]=0;sd_CH4_B5_3[*,0,0,it]/16
data[it].std[113,*]=0;sd_CH4_B6_3[*,0,0,it]/16
data[it].std[114,*]=0;sd_CH4_B7_3[*,0,0,it]/16
data[it].std[115,*]=0;sd_CH4_B8_3[*,0,0,it]/16
data[it].std[116,*]=0;sd_CH4_B9_3[*,0,0,it]/16
data[it].std[117,*]=0;sd_CH4_B10_3[*,0,0,it]/16
data[it].std[118,*]=0;sd_CH4_B11_3[*,0,0,it]/16
data[it].std[119,*]=0;sd_CH4_B12_3[*,0,0,it]/16
data[it].std[120,*]=0;sd_CH4_B13_3[*,0,0,it]/16

data[it].std[121,*]=0;sd_CH4_R1_3[*,0,0,it]/16
data[it].std[122,*]=0;sd_CH4_R2_3[*,0,0,it]/16
data[it].std[123,*]=0;sd_CH4_R3_3[*,0,0,it]/16
data[it].std[124,*]=0;sd_CH4_R4_3[*,0,0,it]/16
data[it].std[125,*]=0;sd_CH4_R5_3[*,0,0,it]/16
data[it].std[126,*]=0;sd_CH4_R6_3[*,0,0,it]/16
data[it].std[127,*]=0;sd_CH4_R7_3[*,0,0,it]/16

data[it].std[128,*]=0;sd_CH4_W1_3[*,0,0,it]/16
data[it].std[129,*]=0;sd_CH4_W2_3[*,0,0,it]/16
data[it].std[130,*]=0;sd_CH4_W3_3[*,0,0,it]/16
data[it].std[131,*]=0;sd_CH4_W4_3[*,0,0,it]/16
data[it].std[132,*]=0;sd_CH4_W5_3[*,0,0,it]/16
data[it].std[133,*]=0;sd_CH4_W6_3[*,0,0,it]/16
data[it].std[134,*]=0;sd_CH4_W7_3[*,0,0,it]/16
data[it].std[135,*]=0;sd_CH4_W8_3[*,0,0,it]/16
data[it].std[136,*]=0;sd_CH4_W9_3[*,0,0,it]/16
data[it].std[137,*]=0;sd_CH4_W10_3[*,0,0,it]/16
data[it].std[138,*]=0;sd_CH4_W11_3[*,0,0,it]/16
data[it].std[139,*]=0;sd_CH4_W12_3[*,0,0,it]/16
data[it].std[140,*]=0;sd_CH4_W13_3[*,0,0,it]/16

data[it].std[141,*]=0;sd_CH4_WAN_3[*,0,0,it]/16
data[it].std[142,*]=0;sd_CH4_TER_3[*,0,0,it]/16
data[it].std[143,*]=0;sd_CH4_OCE_3[*,0,0,it]/16
data[it].std[144,*]=0;sd_CH4_VOL_3[*,0,0,it]/16

;;;AGECLASSE 4;;; 

data[it].std[145,*]=0;sd_CH4_A1_4[*,0,0,it]/16
data[it].std[146,*]=0;sd_CH4_A2_4[*,0,0,it]/16
data[it].std[147,*]=0;sd_CH4_A3_4[*,0,0,it]/16
data[it].std[148,*]=0;sd_CH4_A4_4[*,0,0,it]/16
data[it].std[149,*]=0;sd_CH4_A5_4[*,0,0,it]/16
data[it].std[150,*]=0;sd_CH4_A6_4[*,0,0,it]/16
data[it].std[151,*]=0;sd_CH4_A7_4[*,0,0,it]/16
data[it].std[152,*]=0;sd_CH4_A8_4[*,0,0,it]/16
data[it].std[153,*]=0;sd_CH4_A9_4[*,0,0,it]/16
data[it].std[154,*]=0;sd_CH4_A10_4[*,0,0,it]/16
data[it].std[155,*]=0;sd_CH4_A11_4[*,0,0,it]/16

data[it].std[156,*]=0;sd_CH4_B1_4[*,0,0,it]/16
data[it].std[157,*]=0;sd_CH4_B2_4[*,0,0,it]/16
data[it].std[158,*]=0;sd_CH4_B3_4[*,0,0,it]/16
data[it].std[159,*]=0;sd_CH4_B4_4[*,0,0,it]/16
data[it].std[160,*]=0;sd_CH4_B5_4[*,0,0,it]/16
data[it].std[161,*]=0;sd_CH4_B6_4[*,0,0,it]/16
data[it].std[162,*]=0;sd_CH4_B7_4[*,0,0,it]/16
data[it].std[163,*]=0;sd_CH4_B8_4[*,0,0,it]/16
data[it].std[164,*]=0;sd_CH4_B9_4[*,0,0,it]/16
data[it].std[165,*]=0;sd_CH4_B10_4[*,0,0,it]/16
data[it].std[166,*]=0;sd_CH4_B11_4[*,0,0,it]/16
data[it].std[167,*]=0;sd_CH4_B12_4[*,0,0,it]/16
data[it].std[168,*]=0;sd_CH4_B13_4[*,0,0,it]/16

data[it].std[169,*]=0;sd_CH4_R1_4[*,0,0,it]/16
data[it].std[170,*]=0;sd_CH4_R2_4[*,0,0,it]/16
data[it].std[171,*]=0;sd_CH4_R3_4[*,0,0,it]/16
data[it].std[172,*]=0;sd_CH4_R4_4[*,0,0,it]/16
data[it].std[173,*]=0;sd_CH4_R5_4[*,0,0,it]/16
data[it].std[174,*]=0;sd_CH4_R6_4[*,0,0,it]/16
data[it].std[175,*]=0;sd_CH4_R7_4[*,0,0,it]/16

data[it].std[176,*]=0;sd_CH4_W1_4[*,0,0,it]/16
data[it].std[177,*]=0;sd_CH4_W2_4[*,0,0,it]/16
data[it].std[178,*]=0;sd_CH4_W3_4[*,0,0,it]/16
data[it].std[179,*]=0;sd_CH4_W4_4[*,0,0,it]/16
data[it].std[180,*]=0;sd_CH4_W5_4[*,0,0,it]/16
data[it].std[181,*]=0;sd_CH4_W6_4[*,0,0,it]/16
data[it].std[182,*]=0;sd_CH4_W7_4[*,0,0,it]/16
data[it].std[183,*]=0;sd_CH4_W8_4[*,0,0,it]/16
data[it].std[184,*]=0;sd_CH4_W9_4[*,0,0,it]/16
data[it].std[185,*]=0;sd_CH4_W10_4[*,0,0,it]/16
data[it].std[186,*]=0;sd_CH4_W11_4[*,0,0,it]/16
data[it].std[187,*]=0;sd_CH4_W12_4[*,0,0,it]/16
data[it].std[188,*]=0;sd_CH4_W13_4[*,0,0,it]/16

data[it].std[189,*]=0;sd_CH4_WAN_4[*,0,0,it]/16
data[it].std[190,*]=0;sd_CH4_TER_4[*,0,0,it]/16
data[it].std[191,*]=0;sd_CH4_OCE_4[*,0,0,it]/16
data[it].std[192,*]=0;sd_CH4_VOL_4[*,0,0,it]/16

;;;AGECLASSE 5;;; 

data[it].std[193,*]=0;sd_CH4_A1_5[*,0,0,it]/16
data[it].std[194,*]=0;sd_CH4_A2_5[*,0,0,it]/16
data[it].std[195,*]=0;sd_CH4_A3_5[*,0,0,it]/16
data[it].std[196,*]=0;sd_CH4_A4_5[*,0,0,it]/16
data[it].std[197,*]=0;sd_CH4_A5_5[*,0,0,it]/16
data[it].std[198,*]=0;sd_CH4_A6_5[*,0,0,it]/16
data[it].std[199,*]=0;sd_CH4_A7_5[*,0,0,it]/16
data[it].std[200,*]=0;sd_CH4_A8_5[*,0,0,it]/16
data[it].std[201,*]=0;sd_CH4_A9_5[*,0,0,it]/16
data[it].std[202,*]=0;sd_CH4_A10_5[*,0,0,it]/16
data[it].std[203,*]=0;sd_CH4_A11_5[*,0,0,it]/16

data[it].std[204,*]=0;sd_CH4_B1_5[*,0,0,it]/16
data[it].std[205,*]=0;sd_CH4_B2_5[*,0,0,it]/16
data[it].std[206,*]=0;sd_CH4_B3_5[*,0,0,it]/16
data[it].std[207,*]=0;sd_CH4_B4_5[*,0,0,it]/16
data[it].std[208,*]=0;sd_CH4_B5_5[*,0,0,it]/16
data[it].std[209,*]=0;sd_CH4_B6_5[*,0,0,it]/16
data[it].std[210,*]=0;sd_CH4_B7_5[*,0,0,it]/16
data[it].std[211,*]=0;sd_CH4_B8_5[*,0,0,it]/16
data[it].std[212,*]=0;sd_CH4_B9_5[*,0,0,it]/16
data[it].std[213,*]=0;sd_CH4_B10_5[*,0,0,it]/16
data[it].std[214,*]=0;sd_CH4_B11_5[*,0,0,it]/16
data[it].std[215,*]=0;sd_CH4_B12_5[*,0,0,it]/16
data[it].std[216,*]=0;sd_CH4_B13_5[*,0,0,it]/16

data[it].std[217,*]=0;sd_CH4_R1_5[*,0,0,it]/16
data[it].std[218,*]=0;sd_CH4_R2_5[*,0,0,it]/16
data[it].std[219,*]=0;sd_CH4_R3_5[*,0,0,it]/16
data[it].std[220,*]=0;sd_CH4_R4_5[*,0,0,it]/16
data[it].std[221,*]=0;sd_CH4_R5_5[*,0,0,it]/16
data[it].std[222,*]=0;sd_CH4_R6_5[*,0,0,it]/16
data[it].std[223,*]=0;sd_CH4_R7_5[*,0,0,it]/16

data[it].std[224,*]=0;sd_CH4_W1_5[*,0,0,it]/16
data[it].std[225,*]=0;sd_CH4_W2_5[*,0,0,it]/16
data[it].std[226,*]=0;sd_CH4_W3_5[*,0,0,it]/16
data[it].std[227,*]=0;sd_CH4_W4_5[*,0,0,it]/16
data[it].std[228,*]=0;sd_CH4_W5_5[*,0,0,it]/16
data[it].std[229,*]=0;sd_CH4_W6_5[*,0,0,it]/16
data[it].std[230,*]=0;sd_CH4_W7_5[*,0,0,it]/16
data[it].std[231,*]=0;sd_CH4_W8_5[*,0,0,it]/16
data[it].std[232,*]=0;sd_CH4_W9_5[*,0,0,it]/16
data[it].std[233,*]=0;sd_CH4_W10_5[*,0,0,it]/16
data[it].std[234,*]=0;sd_CH4_W11_5[*,0,0,it]/16
data[it].std[235,*]=0;sd_CH4_W12_5[*,0,0,it]/16
data[it].std[236,*]=0;sd_CH4_W13_5[*,0,0,it]/16

data[it].std[237,*]=0;sd_CH4_WAN_5[*,0,0,it]/16
data[it].std[238,*]=0;sd_CH4_TER_5[*,0,0,it]/16
data[it].std[239,*]=0;sd_CH4_OCE_5[*,0,0,it]/16
data[it].std[240,*]=0;sd_CH4_VOL_5[*,0,0,it]/16



;;;;;;;;;;;NN AND XK;;;;;;;;;;;;;


;data[it].avgnum[*] = nn_Air_tracer[*,0,0,it] ;TEST for now
;data[it].avgnum[1,*]=nn_CH4_A1_5[*,0,0,it]
data[it].avgnum[*] =0; nn_Air_tracer[*,0,0,it] ;TEST for now


data[it].kernweights[*] =0; xk_Air_tracer[*,0,0,it] ; TEST for now
;data[it].kernweights[1,*]=xk_CH4_A1_5[*,0,0,it]


ENDFOR




FOR i=0,nrcpt-1 DO info[i].rcptname = STRMID(string(receptorname[*,i]),0,3)+$
                                        STRCOMPRESS(STRMID(string(receptorname[*,i]),4,4),/REM)
  ind = WHERE(info.rcptname eq 'Matrova',c)
  IF c eq 1L THEN info[ind].rcptname = 'Matorova0'

info.xrcpt=lon
info.yrcpt=lat
info.zrcpt=lev
info.hxmax=hx
info.hymax=hy
info.hzmax=hz
info.time=ntime

;stop

;stop

;data.pptv
;data.std
;data.avgnum
;data.kernweights


;FOR it=0,ntime-1 DO BEGIN
;    FOR i=0,nspec-1 DO BEGIN
;data[it].pptv[i,*] = CH4_A5_1
;data[it].std[i,*]  = sd_CH4_A1_1
;ENDFOR
;data[it].avgnum[*] = nn_CH4_W12_4
;    
;    data[it].kernweights[*] = xk_CH4_W12_4
; ENDFOR
;stop
;IF yyyymm eq 199002 then stop
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
