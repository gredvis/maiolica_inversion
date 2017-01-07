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

PRO read_receptors_maiolica_final,sim=sim,yyyymm=yyyymm,info=info,data=data

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
yyyymmp1 = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')+40),0,6)
  direc = sim.modeldir+sim.name+'/'+yyyymmp1+'01/'

print, direc

;file ='/nas/arf/output/final_sim01/19900101/receptor_pptv.nc'
  file = direc+'receptor_pptv.nc'

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

  dimxid=ncdf_dimid(ncid,'time')
  ncdf_diminq,ncid,dimxid,name,ntime
  dimyid=ncdf_dimid(ncid,'rec')
  ncdf_diminq,ncid,dimyid,name,nrcpt
  dimtid=ncdf_dimid(ncid,'pnt')
  ncdf_diminq,ncid,dimtid,name,npnt
  dimtid=ncdf_dimid(ncid,'age')
  ncdf_diminq,ncid,dimtid,name,nage
 ; dimtid=ncdf_dimid(ncid,'receptorname')
 ; ncdf_diminq,ncid,dimtid,name,nreceptorname
 ;dimtid=ncdf_dimid(ncid,'CH4_A8_1')



  ncdf_varget,ncid,'time',time
  ncdf_varget,ncid,'rec',rec
  ncdf_varget,ncid,'pnt',pnt
  ncdf_varget,ncid,'age',age
  ncdf_varget,ncid,'receptorname',receptorname
  ncdf_varget,ncid,'lon',lon
  ncdf_varget,ncid,'lat',lat
  ncdf_varget,ncid,'lev',lev
  ncdf_varget,ncid,'hx',hx
ncdf_varget,ncid,'hy',hy
ncdf_varget,ncid,'hz',hz
ncdf_varget,ncid,'Air_tracer',Air_tracer
ncdf_varget,ncid,'nn_Air_tracer',nn_Air_tracer
ncdf_varget,ncid,'xk_Air_tracer',xk_Air_tracer
ncdf_varget,ncid,'sd_Air_tracer',sd_Air_tracer

ncdf_varget,ncid,'CH4_A1_1',CH4_A1_1
ncdf_varget,ncid,'CH4_A2_1',CH4_A2_1
ncdf_varget,ncid,'CH4_A3_1',CH4_A3_1
ncdf_varget,ncid,'CH4_A4_1',CH4_A4_1
ncdf_varget,ncid,'CH4_A5_1',CH4_A5_1
ncdf_varget,ncid,'CH4_A6_1',CH4_A6_1
ncdf_varget,ncid,'CH4_A7_1',CH4_A7_1
ncdf_varget,ncid,'CH4_A8_1',CH4_A8_1
ncdf_varget,ncid,'CH4_A9_1',CH4_A9_1
ncdf_varget,ncid,'CH4_A10_1',CH4_A10_1
ncdf_varget,ncid,'CH4_A11_1',CH4_A11_1

ncdf_varget,ncid,'CH4_A1_2',CH4_A1_2
ncdf_varget,ncid,'CH4_A2_2',CH4_A2_2
ncdf_varget,ncid,'CH4_A3_2',CH4_A3_2
ncdf_varget,ncid,'CH4_A4_2',CH4_A4_2
ncdf_varget,ncid,'CH4_A5_2',CH4_A5_2
ncdf_varget,ncid,'CH4_A6_2',CH4_A6_2
ncdf_varget,ncid,'CH4_A7_2',CH4_A7_2
ncdf_varget,ncid,'CH4_A8_2',CH4_A8_2
ncdf_varget,ncid,'CH4_A9_2',CH4_A9_2
ncdf_varget,ncid,'CH4_A10_2',CH4_A10_2
ncdf_varget,ncid,'CH4_A11_2',CH4_A11_2

ncdf_varget,ncid,'CH4_A1_3',CH4_A1_3
ncdf_varget,ncid,'CH4_A2_3',CH4_A2_3
ncdf_varget,ncid,'CH4_A3_3',CH4_A3_3
ncdf_varget,ncid,'CH4_A4_3',CH4_A4_3
ncdf_varget,ncid,'CH4_A5_3',CH4_A5_3
ncdf_varget,ncid,'CH4_A6_3',CH4_A6_3
ncdf_varget,ncid,'CH4_A7_3',CH4_A7_3
ncdf_varget,ncid,'CH4_A8_3',CH4_A8_3
ncdf_varget,ncid,'CH4_A9_3',CH4_A9_3
ncdf_varget,ncid,'CH4_A10_3',CH4_A10_3
ncdf_varget,ncid,'CH4_A11_3',CH4_A11_3

ncdf_varget,ncid,'CH4_A1_4',CH4_A1_4
ncdf_varget,ncid,'CH4_A2_4',CH4_A2_4
ncdf_varget,ncid,'CH4_A3_4',CH4_A3_4
ncdf_varget,ncid,'CH4_A4_4',CH4_A4_4
ncdf_varget,ncid,'CH4_A5_4',CH4_A5_4
ncdf_varget,ncid,'CH4_A6_4',CH4_A6_4
ncdf_varget,ncid,'CH4_A7_4',CH4_A7_4
ncdf_varget,ncid,'CH4_A8_4',CH4_A8_4
ncdf_varget,ncid,'CH4_A9_4',CH4_A9_4
ncdf_varget,ncid,'CH4_A10_4',CH4_A10_4
ncdf_varget,ncid,'CH4_A11_4',CH4_A11_4

ncdf_varget,ncid,'CH4_A1_5',CH4_A1_5
ncdf_varget,ncid,'CH4_A2_5',CH4_A2_5
ncdf_varget,ncid,'CH4_A3_5',CH4_A3_5
ncdf_varget,ncid,'CH4_A4_5',CH4_A4_5
ncdf_varget,ncid,'CH4_A5_5',CH4_A5_5
ncdf_varget,ncid,'CH4_A6_5',CH4_A6_5
ncdf_varget,ncid,'CH4_A7_5',CH4_A7_5
ncdf_varget,ncid,'CH4_A8_5',CH4_A8_5
ncdf_varget,ncid,'CH4_A9_5',CH4_A9_5
ncdf_varget,ncid,'CH4_A10_5',CH4_A10_5
ncdf_varget,ncid,'CH4_A11_5',CH4_A11_5

;;;;;;;;;;;;;;;B;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'CH4_B1_1',CH4_B1_1
ncdf_varget,ncid,'CH4_B2_1',CH4_B2_1
ncdf_varget,ncid,'CH4_B3_1',CH4_B3_1
ncdf_varget,ncid,'CH4_B4_1',CH4_B4_1
ncdf_varget,ncid,'CH4_B5_1',CH4_B5_1
ncdf_varget,ncid,'CH4_B6_1',CH4_B6_1
ncdf_varget,ncid,'CH4_B7_1',CH4_B7_1
ncdf_varget,ncid,'CH4_B8_1',CH4_B8_1
ncdf_varget,ncid,'CH4_B9_1',CH4_B9_1
ncdf_varget,ncid,'CH4_B10_1',CH4_B10_1
ncdf_varget,ncid,'CH4_B11_1',CH4_B11_1
ncdf_varget,ncid,'CH4_B12_1',CH4_B12_1
ncdf_varget,ncid,'CH4_B13_1',CH4_B13_1

ncdf_varget,ncid,'CH4_B1_2',CH4_B1_2
ncdf_varget,ncid,'CH4_B2_2',CH4_B2_2
ncdf_varget,ncid,'CH4_B3_2',CH4_B3_2
ncdf_varget,ncid,'CH4_B4_2',CH4_B4_2
ncdf_varget,ncid,'CH4_B5_2',CH4_B5_2
ncdf_varget,ncid,'CH4_B6_2',CH4_B6_2
ncdf_varget,ncid,'CH4_B7_2',CH4_B7_2
ncdf_varget,ncid,'CH4_B8_2',CH4_B8_2
ncdf_varget,ncid,'CH4_B9_2',CH4_B9_2
ncdf_varget,ncid,'CH4_B10_2',CH4_B10_2
ncdf_varget,ncid,'CH4_B11_2',CH4_B11_2
ncdf_varget,ncid,'CH4_B12_2',CH4_B12_2
ncdf_varget,ncid,'CH4_B13_2',CH4_B13_2

ncdf_varget,ncid,'CH4_B1_3',CH4_B1_3
ncdf_varget,ncid,'CH4_B2_3',CH4_B2_3
ncdf_varget,ncid,'CH4_B3_3',CH4_B3_3
ncdf_varget,ncid,'CH4_B4_3',CH4_B4_3
ncdf_varget,ncid,'CH4_B5_3',CH4_B5_3
ncdf_varget,ncid,'CH4_B6_3',CH4_B6_3
ncdf_varget,ncid,'CH4_B7_3',CH4_B7_3
ncdf_varget,ncid,'CH4_B8_3',CH4_B8_3
ncdf_varget,ncid,'CH4_B9_3',CH4_B9_3
ncdf_varget,ncid,'CH4_B10_3',CH4_B10_3
ncdf_varget,ncid,'CH4_B11_3',CH4_B11_3
ncdf_varget,ncid,'CH4_B12_3',CH4_B12_3
ncdf_varget,ncid,'CH4_B13_3',CH4_B13_3

ncdf_varget,ncid,'CH4_B1_4',CH4_B1_4
ncdf_varget,ncid,'CH4_B2_4',CH4_B2_4
ncdf_varget,ncid,'CH4_B3_4',CH4_B3_4
ncdf_varget,ncid,'CH4_B4_4',CH4_B4_4
ncdf_varget,ncid,'CH4_B5_4',CH4_B5_4
ncdf_varget,ncid,'CH4_B6_4',CH4_B6_4
ncdf_varget,ncid,'CH4_B7_4',CH4_B7_4
ncdf_varget,ncid,'CH4_B8_4',CH4_B8_4
ncdf_varget,ncid,'CH4_B9_4',CH4_B9_4
ncdf_varget,ncid,'CH4_B10_4',CH4_B10_4
ncdf_varget,ncid,'CH4_B11_4',CH4_B11_4
ncdf_varget,ncid,'CH4_B12_4',CH4_B12_4
ncdf_varget,ncid,'CH4_B13_4',CH4_B13_4

ncdf_varget,ncid,'CH4_B1_5',CH4_B1_5
ncdf_varget,ncid,'CH4_B2_5',CH4_B2_5
ncdf_varget,ncid,'CH4_B3_5',CH4_B3_5
ncdf_varget,ncid,'CH4_B4_5',CH4_B4_5
ncdf_varget,ncid,'CH4_B5_5',CH4_B5_5
ncdf_varget,ncid,'CH4_B6_5',CH4_B6_5
ncdf_varget,ncid,'CH4_B7_5',CH4_B7_5
ncdf_varget,ncid,'CH4_B8_5',CH4_B8_5
ncdf_varget,ncid,'CH4_B9_5',CH4_B9_5
ncdf_varget,ncid,'CH4_B10_5',CH4_B10_5
ncdf_varget,ncid,'CH4_B11_5',CH4_B11_5
ncdf_varget,ncid,'CH4_B12_5',CH4_B12_5
ncdf_varget,ncid,'CH4_B13_5',CH4_B13_5

;;;;;;;;;;;;;;;R;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'CH4_R1_1',CH4_R1_1
ncdf_varget,ncid,'CH4_R2_1',CH4_R2_1
ncdf_varget,ncid,'CH4_R3_1',CH4_R3_1
ncdf_varget,ncid,'CH4_R4_1',CH4_R4_1
ncdf_varget,ncid,'CH4_R5_1',CH4_R5_1
ncdf_varget,ncid,'CH4_R6_1',CH4_R6_1
ncdf_varget,ncid,'CH4_R7_1',CH4_R7_1


ncdf_varget,ncid,'CH4_R1_2',CH4_R1_2
ncdf_varget,ncid,'CH4_R2_2',CH4_R2_2
ncdf_varget,ncid,'CH4_R3_2',CH4_R3_2
ncdf_varget,ncid,'CH4_R4_2',CH4_R4_2
ncdf_varget,ncid,'CH4_R5_2',CH4_R5_2
ncdf_varget,ncid,'CH4_R6_2',CH4_R6_2
ncdf_varget,ncid,'CH4_R7_2',CH4_R7_2


ncdf_varget,ncid,'CH4_R1_3',CH4_R1_3
ncdf_varget,ncid,'CH4_R2_3',CH4_R2_3
ncdf_varget,ncid,'CH4_R3_3',CH4_R3_3
ncdf_varget,ncid,'CH4_R4_3',CH4_R4_3
ncdf_varget,ncid,'CH4_R5_3',CH4_R5_3
ncdf_varget,ncid,'CH4_R6_3',CH4_R6_3
ncdf_varget,ncid,'CH4_R7_3',CH4_R7_3

ncdf_varget,ncid,'CH4_R1_4',CH4_R1_4
ncdf_varget,ncid,'CH4_R2_4',CH4_R2_4
ncdf_varget,ncid,'CH4_R3_4',CH4_R3_4
ncdf_varget,ncid,'CH4_R4_4',CH4_R4_4
ncdf_varget,ncid,'CH4_R5_4',CH4_R5_4
ncdf_varget,ncid,'CH4_R6_4',CH4_R6_4
ncdf_varget,ncid,'CH4_R7_4',CH4_R7_4


ncdf_varget,ncid,'CH4_R1_5',CH4_R1_5
ncdf_varget,ncid,'CH4_R2_5',CH4_R2_5
ncdf_varget,ncid,'CH4_R3_5',CH4_R3_5
ncdf_varget,ncid,'CH4_R4_5',CH4_R4_5
ncdf_varget,ncid,'CH4_R5_5',CH4_R5_5
ncdf_varget,ncid,'CH4_R6_5',CH4_R6_5
ncdf_varget,ncid,'CH4_R7_5',CH4_R7_5


;;;;;;;;;;;;;;;W;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'CH4_W1_1',CH4_W1_1
ncdf_varget,ncid,'CH4_W2_1',CH4_W2_1
ncdf_varget,ncid,'CH4_W3_1',CH4_W3_1
ncdf_varget,ncid,'CH4_W4_1',CH4_W4_1
ncdf_varget,ncid,'CH4_W5_1',CH4_W5_1
ncdf_varget,ncid,'CH4_W6_1',CH4_W6_1
ncdf_varget,ncid,'CH4_W7_1',CH4_W7_1
ncdf_varget,ncid,'CH4_W8_1',CH4_W8_1
ncdf_varget,ncid,'CH4_W9_1',CH4_W9_1
ncdf_varget,ncid,'CH4_W10_1',CH4_W10_1
ncdf_varget,ncid,'CH4_W11_1',CH4_W11_1
ncdf_varget,ncid,'CH4_W12_1',CH4_W12_1
ncdf_varget,ncid,'CH4_W13_1',CH4_W13_1

ncdf_varget,ncid,'CH4_W1_2',CH4_W1_2
ncdf_varget,ncid,'CH4_W2_2',CH4_W2_2
ncdf_varget,ncid,'CH4_W3_2',CH4_W3_2
ncdf_varget,ncid,'CH4_W4_2',CH4_W4_2
ncdf_varget,ncid,'CH4_W5_2',CH4_W5_2
ncdf_varget,ncid,'CH4_W6_2',CH4_W6_2
ncdf_varget,ncid,'CH4_W7_2',CH4_W7_2
ncdf_varget,ncid,'CH4_W8_2',CH4_W8_2
ncdf_varget,ncid,'CH4_W9_2',CH4_W9_2
ncdf_varget,ncid,'CH4_W10_2',CH4_W10_2
ncdf_varget,ncid,'CH4_W11_2',CH4_W11_2
ncdf_varget,ncid,'CH4_W12_2',CH4_W12_2
ncdf_varget,ncid,'CH4_W13_2',CH4_W13_2

ncdf_varget,ncid,'CH4_W1_3',CH4_W1_3
ncdf_varget,ncid,'CH4_W2_3',CH4_W2_3
ncdf_varget,ncid,'CH4_W3_3',CH4_W3_3
ncdf_varget,ncid,'CH4_W4_3',CH4_W4_3
ncdf_varget,ncid,'CH4_W5_3',CH4_W5_3
ncdf_varget,ncid,'CH4_W6_3',CH4_W6_3
ncdf_varget,ncid,'CH4_W7_3',CH4_W7_3
ncdf_varget,ncid,'CH4_W8_3',CH4_W8_3
ncdf_varget,ncid,'CH4_W9_3',CH4_W9_3
ncdf_varget,ncid,'CH4_W10_3',CH4_W10_3
ncdf_varget,ncid,'CH4_W11_3',CH4_W11_3
ncdf_varget,ncid,'CH4_W12_3',CH4_W12_3
ncdf_varget,ncid,'CH4_W13_3',CH4_W13_3

ncdf_varget,ncid,'CH4_W1_4',CH4_W1_4
ncdf_varget,ncid,'CH4_W2_4',CH4_W2_4
ncdf_varget,ncid,'CH4_W3_4',CH4_W3_4
ncdf_varget,ncid,'CH4_W4_4',CH4_W4_4
ncdf_varget,ncid,'CH4_W5_4',CH4_W5_4
ncdf_varget,ncid,'CH4_W6_4',CH4_W6_4
ncdf_varget,ncid,'CH4_W7_4',CH4_W7_4
ncdf_varget,ncid,'CH4_W8_4',CH4_W8_4
ncdf_varget,ncid,'CH4_W9_4',CH4_W9_4
ncdf_varget,ncid,'CH4_W10_4',CH4_W10_4
ncdf_varget,ncid,'CH4_W11_4',CH4_W11_4
ncdf_varget,ncid,'CH4_W12_4',CH4_W12_4
ncdf_varget,ncid,'CH4_W13_4',CH4_W13_4

ncdf_varget,ncid,'CH4_W1_5',CH4_W1_5
ncdf_varget,ncid,'CH4_W2_5',CH4_W2_5
ncdf_varget,ncid,'CH4_W3_5',CH4_W3_5
ncdf_varget,ncid,'CH4_W4_5',CH4_W4_5
ncdf_varget,ncid,'CH4_W5_5',CH4_W5_5
ncdf_varget,ncid,'CH4_W6_5',CH4_W6_5
ncdf_varget,ncid,'CH4_W7_5',CH4_W7_5
ncdf_varget,ncid,'CH4_W8_5',CH4_W8_5
ncdf_varget,ncid,'CH4_W9_5',CH4_W9_5
ncdf_varget,ncid,'CH4_W10_5',CH4_W10_5
ncdf_varget,ncid,'CH4_W11_5',CH4_W11_5
ncdf_varget,ncid,'CH4_W12_5',CH4_W12_5
ncdf_varget,ncid,'CH4_W13_5',CH4_W13_5

;;;;;;;;;;;;;;OTHERS;;;;;;;;

ncdf_varget,ncid,'CH4_WAN_1',CH4_WAN_1
ncdf_varget,ncid,'CH4_WAN_2',CH4_WAN_2
ncdf_varget,ncid,'CH4_WAN_3',CH4_WAN_3
ncdf_varget,ncid,'CH4_WAN_4',CH4_WAN_4
ncdf_varget,ncid,'CH4_WAN_5',CH4_WAN_5

ncdf_varget,ncid,'CH4_TER_1',CH4_TER_1
ncdf_varget,ncid,'CH4_TER_2',CH4_TER_2
ncdf_varget,ncid,'CH4_TER_3',CH4_TER_3
ncdf_varget,ncid,'CH4_TER_4',CH4_TER_4
ncdf_varget,ncid,'CH4_TER_5',CH4_TER_5

ncdf_varget,ncid,'CH4_OCE_1',CH4_OCE_1
ncdf_varget,ncid,'CH4_OCE_2',CH4_OCE_2
ncdf_varget,ncid,'CH4_OCE_3',CH4_OCE_3
ncdf_varget,ncid,'CH4_OCE_4',CH4_OCE_4
ncdf_varget,ncid,'CH4_OCE_5',CH4_OCE_5

ncdf_varget,ncid,'CH4_VOL_1',CH4_VOL_1
ncdf_varget,ncid,'CH4_VOL_2',CH4_VOL_2
ncdf_varget,ncid,'CH4_VOL_3',CH4_VOL_3
ncdf_varget,ncid,'CH4_VOL_4',CH4_VOL_4
ncdf_varget,ncid,'CH4_VOL_5',CH4_VOL_5


;;:::::::::::::::::::::::::;SD;;;:::::::::::::::::::::::::::;;

ncdf_varget,ncid,'sd_Air_tracer',sd_Air_tracer

ncdf_varget,ncid,'sd_CH4_A1_1',sd_CH4_A1_1
ncdf_varget,ncid,'sd_CH4_A2_1',sd_CH4_A2_1
ncdf_varget,ncid,'sd_CH4_A3_1',sd_CH4_A3_1
ncdf_varget,ncid,'sd_CH4_A4_1',sd_CH4_A4_1
ncdf_varget,ncid,'sd_CH4_A5_1',sd_CH4_A5_1
ncdf_varget,ncid,'sd_CH4_A6_1',sd_CH4_A6_1
ncdf_varget,ncid,'sd_CH4_A7_1',sd_CH4_A7_1
ncdf_varget,ncid,'sd_CH4_A8_1',sd_CH4_A8_1
ncdf_varget,ncid,'sd_CH4_A9_1',sd_CH4_A9_1
ncdf_varget,ncid,'sd_CH4_A10_1',sd_CH4_A10_1
ncdf_varget,ncid,'sd_CH4_A11_1',sd_CH4_A11_1

ncdf_varget,ncid,'sd_CH4_A1_2',sd_CH4_A1_2
ncdf_varget,ncid,'sd_CH4_A2_2',sd_CH4_A2_2
ncdf_varget,ncid,'sd_CH4_A3_2',sd_CH4_A3_2
ncdf_varget,ncid,'sd_CH4_A4_2',sd_CH4_A4_2
ncdf_varget,ncid,'sd_CH4_A5_2',sd_CH4_A5_2
ncdf_varget,ncid,'sd_CH4_A6_2',sd_CH4_A6_2
ncdf_varget,ncid,'sd_CH4_A7_2',sd_CH4_A7_2
ncdf_varget,ncid,'sd_CH4_A8_2',sd_CH4_A8_2
ncdf_varget,ncid,'sd_CH4_A9_2',sd_CH4_A9_2
ncdf_varget,ncid,'sd_CH4_A10_2',sd_CH4_A10_2
ncdf_varget,ncid,'sd_CH4_A11_2',sd_CH4_A11_2

ncdf_varget,ncid,'sd_CH4_A1_3',sd_CH4_A1_3
ncdf_varget,ncid,'sd_CH4_A2_3',sd_CH4_A2_3
ncdf_varget,ncid,'sd_CH4_A3_3',sd_CH4_A3_3
ncdf_varget,ncid,'sd_CH4_A4_3',sd_CH4_A4_3
ncdf_varget,ncid,'sd_CH4_A5_3',sd_CH4_A5_3
ncdf_varget,ncid,'sd_CH4_A6_3',sd_CH4_A6_3
ncdf_varget,ncid,'sd_CH4_A7_3',sd_CH4_A7_3
ncdf_varget,ncid,'sd_CH4_A8_3',sd_CH4_A8_3
ncdf_varget,ncid,'sd_CH4_A9_3',sd_CH4_A9_3
ncdf_varget,ncid,'sd_CH4_A10_3',sd_CH4_A10_3
ncdf_varget,ncid,'sd_CH4_A11_3',sd_CH4_A11_3

ncdf_varget,ncid,'sd_CH4_A1_4',sd_CH4_A1_4
ncdf_varget,ncid,'sd_CH4_A2_4',sd_CH4_A2_4
ncdf_varget,ncid,'sd_CH4_A3_4',sd_CH4_A3_4
ncdf_varget,ncid,'sd_CH4_A4_4',sd_CH4_A4_4
ncdf_varget,ncid,'sd_CH4_A5_4',sd_CH4_A5_4
ncdf_varget,ncid,'sd_CH4_A6_4',sd_CH4_A6_4
ncdf_varget,ncid,'sd_CH4_A7_4',sd_CH4_A7_4
ncdf_varget,ncid,'sd_CH4_A8_4',sd_CH4_A8_4
ncdf_varget,ncid,'sd_CH4_A9_4',sd_CH4_A9_4
ncdf_varget,ncid,'sd_CH4_A10_4',sd_CH4_A10_4
ncdf_varget,ncid,'sd_CH4_A11_4',sd_CH4_A11_4

ncdf_varget,ncid,'sd_CH4_A1_5',sd_CH4_A1_5
ncdf_varget,ncid,'sd_CH4_A2_5',sd_CH4_A2_5
ncdf_varget,ncid,'sd_CH4_A3_5',sd_CH4_A3_5
ncdf_varget,ncid,'sd_CH4_A4_5',sd_CH4_A4_5
ncdf_varget,ncid,'sd_CH4_A5_5',sd_CH4_A5_5
ncdf_varget,ncid,'sd_CH4_A6_5',sd_CH4_A6_5
ncdf_varget,ncid,'sd_CH4_A7_5',sd_CH4_A7_5
ncdf_varget,ncid,'sd_CH4_A8_5',sd_CH4_A8_5
ncdf_varget,ncid,'sd_CH4_A9_5',sd_CH4_A9_5
ncdf_varget,ncid,'sd_CH4_A10_5',sd_CH4_A10_5
ncdf_varget,ncid,'sd_CH4_A11_5',sd_CH4_A11_5


;;;;;;;;;;;;;;;B;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'sd_CH4_B1_1',sd_CH4_B1_1
ncdf_varget,ncid,'sd_CH4_B2_1',sd_CH4_B2_1
ncdf_varget,ncid,'sd_CH4_B3_1',sd_CH4_B3_1
ncdf_varget,ncid,'sd_CH4_B4_1',sd_CH4_B4_1
ncdf_varget,ncid,'sd_CH4_B5_1',sd_CH4_B5_1
ncdf_varget,ncid,'sd_CH4_B6_1',sd_CH4_B6_1
ncdf_varget,ncid,'sd_CH4_B7_1',sd_CH4_B7_1
ncdf_varget,ncid,'sd_CH4_B8_1',sd_CH4_B8_1
ncdf_varget,ncid,'sd_CH4_B9_1',sd_CH4_B9_1
ncdf_varget,ncid,'sd_CH4_B10_1',sd_CH4_B10_1
ncdf_varget,ncid,'sd_CH4_B11_1',sd_CH4_B11_1
ncdf_varget,ncid,'sd_CH4_B12_1',sd_CH4_B12_1
ncdf_varget,ncid,'sd_CH4_B13_1',sd_CH4_B13_1

ncdf_varget,ncid,'sd_CH4_B1_2',sd_CH4_B1_2
ncdf_varget,ncid,'sd_CH4_B2_2',sd_CH4_B2_2
ncdf_varget,ncid,'sd_CH4_B3_2',sd_CH4_B3_2
ncdf_varget,ncid,'sd_CH4_B4_2',sd_CH4_B4_2
ncdf_varget,ncid,'sd_CH4_B5_2',sd_CH4_B5_2
ncdf_varget,ncid,'sd_CH4_B6_2',sd_CH4_B6_2
ncdf_varget,ncid,'sd_CH4_B7_2',sd_CH4_B7_2
ncdf_varget,ncid,'sd_CH4_B8_2',sd_CH4_B8_2
ncdf_varget,ncid,'sd_CH4_B9_2',sd_CH4_B9_2
ncdf_varget,ncid,'sd_CH4_B10_2',sd_CH4_B10_2
ncdf_varget,ncid,'sd_CH4_B11_2',sd_CH4_B11_2
ncdf_varget,ncid,'sd_CH4_B12_2',sd_CH4_B12_2
ncdf_varget,ncid,'sd_CH4_B13_2',sd_CH4_B13_2

ncdf_varget,ncid,'sd_CH4_B1_3',sd_CH4_B1_3
ncdf_varget,ncid,'sd_CH4_B2_3',sd_CH4_B2_3
ncdf_varget,ncid,'sd_CH4_B3_3',sd_CH4_B3_3
ncdf_varget,ncid,'sd_CH4_B4_3',sd_CH4_B4_3
ncdf_varget,ncid,'sd_CH4_B5_3',sd_CH4_B5_3
ncdf_varget,ncid,'sd_CH4_B6_3',sd_CH4_B6_3
ncdf_varget,ncid,'sd_CH4_B7_3',sd_CH4_B7_3
ncdf_varget,ncid,'sd_CH4_B8_3',sd_CH4_B8_3
ncdf_varget,ncid,'sd_CH4_B9_3',sd_CH4_B9_3
ncdf_varget,ncid,'sd_CH4_B10_3',sd_CH4_B10_3
ncdf_varget,ncid,'sd_CH4_B11_3',sd_CH4_B11_3
ncdf_varget,ncid,'sd_CH4_B12_3',sd_CH4_B12_3
ncdf_varget,ncid,'sd_CH4_B13_3',sd_CH4_B13_3

ncdf_varget,ncid,'sd_CH4_B1_4',sd_CH4_B1_4
ncdf_varget,ncid,'sd_CH4_B2_4',sd_CH4_B2_4
ncdf_varget,ncid,'sd_CH4_B3_4',sd_CH4_B3_4
ncdf_varget,ncid,'sd_CH4_B4_4',sd_CH4_B4_4
ncdf_varget,ncid,'sd_CH4_B5_4',sd_CH4_B5_4
ncdf_varget,ncid,'sd_CH4_B6_4',sd_CH4_B6_4
ncdf_varget,ncid,'sd_CH4_B7_4',sd_CH4_B7_4
ncdf_varget,ncid,'sd_CH4_B8_4',sd_CH4_B8_4
ncdf_varget,ncid,'sd_CH4_B9_4',sd_CH4_B9_4
ncdf_varget,ncid,'sd_CH4_B10_4',sd_CH4_B10_4
ncdf_varget,ncid,'sd_CH4_B11_4',sd_CH4_B11_4
ncdf_varget,ncid,'sd_CH4_B12_4',sd_CH4_B12_4
ncdf_varget,ncid,'sd_CH4_B13_4',sd_CH4_B13_4

ncdf_varget,ncid,'sd_CH4_B1_5',sd_CH4_B1_5
ncdf_varget,ncid,'sd_CH4_B2_5',sd_CH4_B2_5
ncdf_varget,ncid,'sd_CH4_B3_5',sd_CH4_B3_5
ncdf_varget,ncid,'sd_CH4_B4_5',sd_CH4_B4_5
ncdf_varget,ncid,'sd_CH4_B5_5',sd_CH4_B5_5
ncdf_varget,ncid,'sd_CH4_B6_5',sd_CH4_B6_5
ncdf_varget,ncid,'sd_CH4_B7_5',sd_CH4_B7_5
ncdf_varget,ncid,'sd_CH4_B8_5',sd_CH4_B8_5
ncdf_varget,ncid,'sd_CH4_B9_5',sd_CH4_B9_5
ncdf_varget,ncid,'sd_CH4_B10_5',sd_CH4_B10_5
ncdf_varget,ncid,'sd_CH4_B11_5',sd_CH4_B11_5
ncdf_varget,ncid,'sd_CH4_B12_5',sd_CH4_B12_5
ncdf_varget,ncid,'sd_CH4_B13_5',sd_CH4_B13_5

;;;;;;;;;;;;;;;R;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'sd_CH4_R1_1',sd_CH4_R1_1
ncdf_varget,ncid,'sd_CH4_R2_1',sd_CH4_R2_1
ncdf_varget,ncid,'sd_CH4_R3_1',sd_CH4_R3_1
ncdf_varget,ncid,'sd_CH4_R4_1',sd_CH4_R4_1
ncdf_varget,ncid,'sd_CH4_R5_1',sd_CH4_R5_1
ncdf_varget,ncid,'sd_CH4_R6_1',sd_CH4_R6_1
ncdf_varget,ncid,'sd_CH4_R7_1',sd_CH4_R7_1


ncdf_varget,ncid,'sd_CH4_R1_2',sd_CH4_R1_2
ncdf_varget,ncid,'sd_CH4_R2_2',sd_CH4_R2_2
ncdf_varget,ncid,'sd_CH4_R3_2',sd_CH4_R3_2
ncdf_varget,ncid,'sd_CH4_R4_2',sd_CH4_R4_2
ncdf_varget,ncid,'sd_CH4_R5_2',sd_CH4_R5_2
ncdf_varget,ncid,'sd_CH4_R6_2',sd_CH4_R6_2
ncdf_varget,ncid,'sd_CH4_R7_2',sd_CH4_R7_2


ncdf_varget,ncid,'sd_CH4_R1_3',sd_CH4_R1_3
ncdf_varget,ncid,'sd_CH4_R2_3',sd_CH4_R2_3
ncdf_varget,ncid,'sd_CH4_R3_3',sd_CH4_R3_3
ncdf_varget,ncid,'sd_CH4_R4_3',sd_CH4_R4_3
ncdf_varget,ncid,'sd_CH4_R5_3',sd_CH4_R5_3
ncdf_varget,ncid,'sd_CH4_R6_3',sd_CH4_R6_3
ncdf_varget,ncid,'sd_CH4_R7_3',sd_CH4_R7_3

ncdf_varget,ncid,'sd_CH4_R1_4',sd_CH4_R1_4
ncdf_varget,ncid,'sd_CH4_R2_4',sd_CH4_R2_4
ncdf_varget,ncid,'sd_CH4_R3_4',sd_CH4_R3_4
ncdf_varget,ncid,'sd_CH4_R4_4',sd_CH4_R4_4
ncdf_varget,ncid,'sd_CH4_R5_4',sd_CH4_R5_4
ncdf_varget,ncid,'sd_CH4_R6_4',sd_CH4_R6_4
ncdf_varget,ncid,'sd_CH4_R7_4',sd_CH4_R7_4


ncdf_varget,ncid,'sd_CH4_R1_5',sd_CH4_R1_5
ncdf_varget,ncid,'sd_CH4_R2_5',sd_CH4_R2_5
ncdf_varget,ncid,'sd_CH4_R3_5',sd_CH4_R3_5
ncdf_varget,ncid,'sd_CH4_R4_5',sd_CH4_R4_5
ncdf_varget,ncid,'sd_CH4_R5_5',sd_CH4_R5_5
ncdf_varget,ncid,'sd_CH4_R6_5',sd_CH4_R6_5
ncdf_varget,ncid,'sd_CH4_R7_5',sd_CH4_R7_5


;;;;;;;;;;;;;;;W;;;;;;;;;;;;;;;;;;;;;

ncdf_varget,ncid,'sd_CH4_W1_1',sd_CH4_W1_1
ncdf_varget,ncid,'sd_CH4_W2_1',sd_CH4_W2_1
ncdf_varget,ncid,'sd_CH4_W3_1',sd_CH4_W3_1
ncdf_varget,ncid,'sd_CH4_W4_1',sd_CH4_W4_1
ncdf_varget,ncid,'sd_CH4_W5_1',sd_CH4_W5_1
ncdf_varget,ncid,'sd_CH4_W6_1',sd_CH4_W6_1
ncdf_varget,ncid,'sd_CH4_W7_1',sd_CH4_W7_1
ncdf_varget,ncid,'sd_CH4_W8_1',sd_CH4_W8_1
ncdf_varget,ncid,'sd_CH4_W9_1',sd_CH4_W9_1
ncdf_varget,ncid,'sd_CH4_W10_1',sd_CH4_W10_1
ncdf_varget,ncid,'sd_CH4_W11_1',sd_CH4_W11_1
ncdf_varget,ncid,'sd_CH4_W12_1',sd_CH4_W12_1
ncdf_varget,ncid,'sd_CH4_W13_1',sd_CH4_W13_1

ncdf_varget,ncid,'sd_CH4_W1_2',sd_CH4_W1_2
ncdf_varget,ncid,'sd_CH4_W2_2',sd_CH4_W2_2
ncdf_varget,ncid,'sd_CH4_W3_2',sd_CH4_W3_2
ncdf_varget,ncid,'sd_CH4_W4_2',sd_CH4_W4_2
ncdf_varget,ncid,'sd_CH4_W5_2',sd_CH4_W5_2
ncdf_varget,ncid,'sd_CH4_W6_2',sd_CH4_W6_2
ncdf_varget,ncid,'sd_CH4_W7_2',sd_CH4_W7_2
ncdf_varget,ncid,'sd_CH4_W8_2',sd_CH4_W8_2
ncdf_varget,ncid,'sd_CH4_W9_2',sd_CH4_W9_2
ncdf_varget,ncid,'sd_CH4_W10_2',sd_CH4_W10_2
ncdf_varget,ncid,'sd_CH4_W11_2',sd_CH4_W11_2
ncdf_varget,ncid,'sd_CH4_W12_2',sd_CH4_W12_2
ncdf_varget,ncid,'sd_CH4_W13_2',sd_CH4_W13_2

ncdf_varget,ncid,'sd_CH4_W1_3',sd_CH4_W1_3
ncdf_varget,ncid,'sd_CH4_W2_3',sd_CH4_W2_3
ncdf_varget,ncid,'sd_CH4_W3_3',sd_CH4_W3_3
ncdf_varget,ncid,'sd_CH4_W4_3',sd_CH4_W4_3
ncdf_varget,ncid,'sd_CH4_W5_3',sd_CH4_W5_3
ncdf_varget,ncid,'sd_CH4_W6_3',sd_CH4_W6_3
ncdf_varget,ncid,'sd_CH4_W7_3',sd_CH4_W7_3
ncdf_varget,ncid,'sd_CH4_W8_3',sd_CH4_W8_3
ncdf_varget,ncid,'sd_CH4_W9_3',sd_CH4_W9_3
ncdf_varget,ncid,'sd_CH4_W10_3',sd_CH4_W10_3
ncdf_varget,ncid,'sd_CH4_W11_3',sd_CH4_W11_3
ncdf_varget,ncid,'sd_CH4_W12_3',sd_CH4_W12_3
ncdf_varget,ncid,'sd_CH4_W13_3',sd_CH4_W13_3

ncdf_varget,ncid,'sd_CH4_W1_4',sd_CH4_W1_4
ncdf_varget,ncid,'sd_CH4_W2_4',sd_CH4_W2_4
ncdf_varget,ncid,'sd_CH4_W3_4',sd_CH4_W3_4
ncdf_varget,ncid,'sd_CH4_W4_4',sd_CH4_W4_4
ncdf_varget,ncid,'sd_CH4_W5_4',sd_CH4_W5_4
ncdf_varget,ncid,'sd_CH4_W6_4',sd_CH4_W6_4
ncdf_varget,ncid,'sd_CH4_W7_4',sd_CH4_W7_4
ncdf_varget,ncid,'sd_CH4_W8_4',sd_CH4_W8_4
ncdf_varget,ncid,'sd_CH4_W9_4',sd_CH4_W9_4
ncdf_varget,ncid,'sd_CH4_W10_4',sd_CH4_W10_4
ncdf_varget,ncid,'sd_CH4_W11_4',sd_CH4_W11_4
ncdf_varget,ncid,'sd_CH4_W12_4',sd_CH4_W12_4
ncdf_varget,ncid,'sd_CH4_W13_4',sd_CH4_W13_4

ncdf_varget,ncid,'sd_CH4_W1_5',sd_CH4_W1_5
ncdf_varget,ncid,'sd_CH4_W2_5',sd_CH4_W2_5
ncdf_varget,ncid,'sd_CH4_W3_5',sd_CH4_W3_5
ncdf_varget,ncid,'sd_CH4_W4_5',sd_CH4_W4_5
ncdf_varget,ncid,'sd_CH4_W5_5',sd_CH4_W5_5
ncdf_varget,ncid,'sd_CH4_W6_5',sd_CH4_W6_5
ncdf_varget,ncid,'sd_CH4_W7_5',sd_CH4_W7_5
ncdf_varget,ncid,'sd_CH4_W8_5',sd_CH4_W8_5
ncdf_varget,ncid,'sd_CH4_W9_5',sd_CH4_W9_5
ncdf_varget,ncid,'sd_CH4_W10_5',sd_CH4_W10_5
ncdf_varget,ncid,'sd_CH4_W11_5',sd_CH4_W11_5
ncdf_varget,ncid,'sd_CH4_W12_5',sd_CH4_W12_5
ncdf_varget,ncid,'sd_CH4_W13_5',sd_CH4_W13_5

;;;;;;;;;;;;;;OTHERS;;;;;;;;

ncdf_varget,ncid,'sd_CH4_WAN_1',sd_CH4_WAN_1
ncdf_varget,ncid,'sd_CH4_WAN_2',sd_CH4_WAN_2
ncdf_varget,ncid,'sd_CH4_WAN_3',sd_CH4_WAN_3
ncdf_varget,ncid,'sd_CH4_WAN_4',sd_CH4_WAN_4
ncdf_varget,ncid,'sd_CH4_WAN_5',sd_CH4_WAN_5

ncdf_varget,ncid,'sd_CH4_TER_1',sd_CH4_TER_1
ncdf_varget,ncid,'sd_CH4_TER_2',sd_CH4_TER_2
ncdf_varget,ncid,'sd_CH4_TER_3',sd_CH4_TER_3
ncdf_varget,ncid,'sd_CH4_TER_4',sd_CH4_TER_4
ncdf_varget,ncid,'sd_CH4_TER_5',sd_CH4_TER_5

ncdf_varget,ncid,'sd_CH4_OCE_1',sd_CH4_OCE_1
ncdf_varget,ncid,'sd_CH4_OCE_2',sd_CH4_OCE_2
ncdf_varget,ncid,'sd_CH4_OCE_3',sd_CH4_OCE_3
ncdf_varget,ncid,'sd_CH4_OCE_4',sd_CH4_OCE_4
ncdf_varget,ncid,'sd_CH4_OCE_5',sd_CH4_OCE_5

ncdf_varget,ncid,'sd_CH4_VOL_1',sd_CH4_VOL_1
ncdf_varget,ncid,'sd_CH4_VOL_2',sd_CH4_VOL_2
ncdf_varget,ncid,'sd_CH4_VOL_3',sd_CH4_VOL_3
ncdf_varget,ncid,'sd_CH4_VOL_4',sd_CH4_VOL_4
ncdf_varget,ncid,'sd_CH4_VOL_5',sd_CH4_VOL_5



;;;; NN AND XK;;;;;;;;;;;;;;

ncdf_varget,ncid,'nn_Air_tracer',nn_Air_tracer
ncdf_varget,ncid,'xk_Air_tracer',xk_Air_tracer


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


FOR it=0,ntime-1 DO BEGIN
;    FOR i=0,nspec-1 DO BEGIN
data[it].pptv[0,*]=Air_tracer[*,0,0,it]

;;;AGECLASSE 1;;; 

data[it].pptv[1,*]=CH4_A1_1[*,0,0,it]
data[it].pptv[2,*]=CH4_A2_1[*,0,0,it]
data[it].pptv[3,*]=CH4_A3_1[*,0,0,it]
data[it].pptv[4,*]=CH4_A4_1[*,0,0,it]
data[it].pptv[5,*]=CH4_A5_1[*,0,0,it]
data[it].pptv[6,*]=CH4_A6_1[*,0,0,it]
data[it].pptv[7,*]=CH4_A7_1[*,0,0,it]
data[it].pptv[8,*]=CH4_A8_1[*,0,0,it]
data[it].pptv[9,*]=CH4_A9_1[*,0,0,it]
data[it].pptv[10,*]=CH4_A10_1[*,0,0,it]
data[it].pptv[11,*]=CH4_A11_1[*,0,0,it]

data[it].pptv[12,*]=CH4_B1_1[*,0,0,it]
data[it].pptv[13,*]=CH4_B2_1[*,0,0,it]
data[it].pptv[14,*]=CH4_B3_1[*,0,0,it]
data[it].pptv[15,*]=CH4_B4_1[*,0,0,it]
data[it].pptv[16,*]=CH4_B5_1[*,0,0,it]
data[it].pptv[17,*]=CH4_B6_1[*,0,0,it]
data[it].pptv[18,*]=CH4_B7_1[*,0,0,it]
data[it].pptv[19,*]=CH4_B8_1[*,0,0,it]
data[it].pptv[20,*]=CH4_B9_1[*,0,0,it]
data[it].pptv[21,*]=CH4_B10_1[*,0,0,it]
data[it].pptv[22,*]=CH4_B11_1[*,0,0,it]
data[it].pptv[23,*]=CH4_B12_1[*,0,0,it]
data[it].pptv[24,*]=CH4_B13_1[*,0,0,it]

data[it].pptv[25,*]=CH4_R1_1[*,0,0,it]
data[it].pptv[26,*]=CH4_R2_1[*,0,0,it]
data[it].pptv[27,*]=CH4_R3_1[*,0,0,it]
data[it].pptv[28,*]=CH4_R4_1[*,0,0,it]
data[it].pptv[29,*]=CH4_R5_1[*,0,0,it]
data[it].pptv[30,*]=CH4_R6_1[*,0,0,it]
data[it].pptv[31,*]=CH4_R7_1[*,0,0,it]

data[it].pptv[32,*]=CH4_W1_1[*,0,0,it]
data[it].pptv[33,*]=CH4_W2_1[*,0,0,it]
data[it].pptv[34,*]=CH4_W3_1[*,0,0,it]
data[it].pptv[35,*]=CH4_W4_1[*,0,0,it]
data[it].pptv[36,*]=CH4_W5_1[*,0,0,it]
data[it].pptv[37,*]=CH4_W6_1[*,0,0,it]
data[it].pptv[38,*]=CH4_W7_1[*,0,0,it]
data[it].pptv[39,*]=CH4_W8_1[*,0,0,it]
data[it].pptv[40,*]=CH4_W9_1[*,0,0,it]
data[it].pptv[41,*]=CH4_W10_1[*,0,0,it]
data[it].pptv[42,*]=CH4_W11_1[*,0,0,it]
data[it].pptv[43,*]=CH4_W12_1[*,0,0,it]
data[it].pptv[44,*]=CH4_W13_1[*,0,0,it]

data[it].pptv[45,*]=CH4_WAN_1[*,0,0,it]
data[it].pptv[46,*]=CH4_TER_1[*,0,0,it]
data[it].pptv[47,*]=CH4_OCE_1[*,0,0,it]
data[it].pptv[48,*]=CH4_VOL_1[*,0,0,it]

;;;AGECLASSE 2;;; 

data[it].pptv[49,*]=CH4_A1_2[*,0,0,it]
data[it].pptv[50,*]=CH4_A2_2[*,0,0,it]
data[it].pptv[51,*]=CH4_A3_2[*,0,0,it]
data[it].pptv[52,*]=CH4_A4_2[*,0,0,it]
data[it].pptv[53,*]=CH4_A5_2[*,0,0,it]
data[it].pptv[54,*]=CH4_A6_2[*,0,0,it]
data[it].pptv[55,*]=CH4_A7_2[*,0,0,it]
data[it].pptv[56,*]=CH4_A8_2[*,0,0,it]
data[it].pptv[57,*]=CH4_A9_2[*,0,0,it]
data[it].pptv[58,*]=CH4_A10_2[*,0,0,it]
data[it].pptv[59,*]=CH4_A11_2[*,0,0,it]

data[it].pptv[60,*]=CH4_W1_2[*,0,0,it] ;flo replaced B by W due to output error
data[it].pptv[61,*]=CH4_W2_2[*,0,0,it]
data[it].pptv[62,*]=CH4_W3_2[*,0,0,it]
data[it].pptv[63,*]=CH4_W4_2[*,0,0,it]
data[it].pptv[64,*]=CH4_W5_2[*,0,0,it]
data[it].pptv[65,*]=CH4_W6_2[*,0,0,it]
data[it].pptv[66,*]=CH4_W7_2[*,0,0,it]
data[it].pptv[67,*]=CH4_W8_2[*,0,0,it]
data[it].pptv[68,*]=CH4_W9_2[*,0,0,it]
data[it].pptv[69,*]=CH4_W10_2[*,0,0,it]
data[it].pptv[70,*]=CH4_W11_2[*,0,0,it]
data[it].pptv[71,*]=CH4_W12_2[*,0,0,it]
data[it].pptv[72,*]=CH4_W13_2[*,0,0,it] ;flo

data[it].pptv[73,*]=CH4_R1_2[*,0,0,it]
data[it].pptv[74,*]=CH4_R2_2[*,0,0,it]
data[it].pptv[75,*]=CH4_R3_2[*,0,0,it]
data[it].pptv[76,*]=CH4_R4_2[*,0,0,it]
data[it].pptv[77,*]=CH4_R5_2[*,0,0,it]
data[it].pptv[78,*]=CH4_R6_2[*,0,0,it]
data[it].pptv[79,*]=CH4_R7_2[*,0,0,it]

data[it].pptv[80,*]=CH4_B1_2[*,0,0,it] ;flo
data[it].pptv[81,*]=CH4_B2_2[*,0,0,it]
data[it].pptv[82,*]=CH4_B3_2[*,0,0,it]
data[it].pptv[83,*]=CH4_B4_2[*,0,0,it]
data[it].pptv[84,*]=CH4_B5_2[*,0,0,it]
data[it].pptv[85,*]=CH4_B6_2[*,0,0,it]
data[it].pptv[86,*]=CH4_B7_2[*,0,0,it]
data[it].pptv[87,*]=CH4_B8_2[*,0,0,it]
data[it].pptv[88,*]=CH4_B9_2[*,0,0,it]
data[it].pptv[89,*]=CH4_B10_2[*,0,0,it]
data[it].pptv[90,*]=CH4_B11_2[*,0,0,it]
data[it].pptv[91,*]=CH4_B12_2[*,0,0,it]
data[it].pptv[92,*]=CH4_B13_2[*,0,0,it] ;flo

data[it].pptv[93,*]=CH4_WAN_2[*,0,0,it]
data[it].pptv[94,*]=CH4_TER_2[*,0,0,it]
data[it].pptv[95,*]=CH4_OCE_2[*,0,0,it]
data[it].pptv[96,*]=CH4_VOL_2[*,0,0,it]


;;;AGECLASSE 3;;; 

data[it].pptv[97,*]=CH4_A1_3[*,0,0,it]
data[it].pptv[98,*]=CH4_A2_3[*,0,0,it]
data[it].pptv[99,*]=CH4_A3_3[*,0,0,it]
data[it].pptv[100,*]=CH4_A4_3[*,0,0,it]
data[it].pptv[101,*]=CH4_A5_3[*,0,0,it]
data[it].pptv[102,*]=CH4_A6_3[*,0,0,it]
data[it].pptv[103,*]=CH4_A7_3[*,0,0,it]
data[it].pptv[104,*]=CH4_A8_3[*,0,0,it]
data[it].pptv[105,*]=CH4_A9_3[*,0,0,it]
data[it].pptv[106,*]=CH4_A10_3[*,0,0,it]
data[it].pptv[107,*]=CH4_A11_3[*,0,0,it]

data[it].pptv[108,*]=CH4_B1_3[*,0,0,it]
data[it].pptv[109,*]=CH4_B2_3[*,0,0,it]
data[it].pptv[110,*]=CH4_B3_3[*,0,0,it]
data[it].pptv[111,*]=CH4_B4_3[*,0,0,it]
data[it].pptv[112,*]=CH4_B5_3[*,0,0,it]
data[it].pptv[113,*]=CH4_B6_3[*,0,0,it]
data[it].pptv[114,*]=CH4_B7_3[*,0,0,it]
data[it].pptv[115,*]=CH4_B8_3[*,0,0,it]
data[it].pptv[116,*]=CH4_B9_3[*,0,0,it]
data[it].pptv[117,*]=CH4_B10_3[*,0,0,it]
data[it].pptv[118,*]=CH4_B11_3[*,0,0,it]
data[it].pptv[119,*]=CH4_B12_3[*,0,0,it]
data[it].pptv[120,*]=CH4_B13_3[*,0,0,it]

data[it].pptv[121,*]=CH4_R1_3[*,0,0,it]
data[it].pptv[122,*]=CH4_R2_3[*,0,0,it]
data[it].pptv[123,*]=CH4_R3_3[*,0,0,it]
data[it].pptv[124,*]=CH4_R4_3[*,0,0,it]
data[it].pptv[125,*]=CH4_R5_3[*,0,0,it]
data[it].pptv[126,*]=CH4_R6_3[*,0,0,it]
data[it].pptv[127,*]=CH4_R7_3[*,0,0,it]

data[it].pptv[128,*]=CH4_W1_3[*,0,0,it]
data[it].pptv[129,*]=CH4_W2_3[*,0,0,it]
data[it].pptv[130,*]=CH4_W3_3[*,0,0,it]
data[it].pptv[131,*]=CH4_W4_3[*,0,0,it]
data[it].pptv[132,*]=CH4_W5_3[*,0,0,it]
data[it].pptv[133,*]=CH4_W6_3[*,0,0,it]
data[it].pptv[134,*]=CH4_W7_3[*,0,0,it]
data[it].pptv[135,*]=CH4_W8_3[*,0,0,it]
data[it].pptv[136,*]=CH4_W9_3[*,0,0,it]
data[it].pptv[137,*]=CH4_W10_3[*,0,0,it]
data[it].pptv[138,*]=CH4_W11_3[*,0,0,it]
data[it].pptv[139,*]=CH4_W12_3[*,0,0,it]
data[it].pptv[140,*]=CH4_W13_3[*,0,0,it]

data[it].pptv[141,*]=CH4_WAN_3[*,0,0,it]
data[it].pptv[142,*]=CH4_TER_3[*,0,0,it]
data[it].pptv[143,*]=CH4_OCE_3[*,0,0,it]
data[it].pptv[144,*]=CH4_VOL_3[*,0,0,it]

;;;AGECLASSE 4;;; 

data[it].pptv[145,*]=CH4_A1_4[*,0,0,it]
data[it].pptv[146,*]=CH4_A2_4[*,0,0,it]
data[it].pptv[147,*]=CH4_A3_4[*,0,0,it]
data[it].pptv[148,*]=CH4_A4_4[*,0,0,it]
data[it].pptv[149,*]=CH4_A5_4[*,0,0,it]
data[it].pptv[150,*]=CH4_A6_4[*,0,0,it]
data[it].pptv[151,*]=CH4_A7_4[*,0,0,it]
data[it].pptv[152,*]=CH4_A8_4[*,0,0,it]
data[it].pptv[153,*]=CH4_A9_4[*,0,0,it]
data[it].pptv[154,*]=CH4_A10_4[*,0,0,it]
data[it].pptv[155,*]=CH4_A11_4[*,0,0,it]

data[it].pptv[156,*]=CH4_B1_4[*,0,0,it]
data[it].pptv[157,*]=CH4_B2_4[*,0,0,it]
data[it].pptv[158,*]=CH4_B3_4[*,0,0,it]
data[it].pptv[159,*]=CH4_B4_4[*,0,0,it]
data[it].pptv[160,*]=CH4_B5_4[*,0,0,it]
data[it].pptv[161,*]=CH4_B6_4[*,0,0,it]
data[it].pptv[162,*]=CH4_B7_4[*,0,0,it]
data[it].pptv[163,*]=CH4_B8_4[*,0,0,it]
data[it].pptv[164,*]=CH4_B9_4[*,0,0,it]
data[it].pptv[165,*]=CH4_B10_4[*,0,0,it]
data[it].pptv[166,*]=CH4_B11_4[*,0,0,it]
data[it].pptv[167,*]=CH4_B12_4[*,0,0,it]
data[it].pptv[168,*]=CH4_B13_4[*,0,0,it]

data[it].pptv[169,*]=CH4_R1_4[*,0,0,it]
data[it].pptv[170,*]=CH4_R2_4[*,0,0,it]
data[it].pptv[171,*]=CH4_R3_4[*,0,0,it]
data[it].pptv[172,*]=CH4_R4_4[*,0,0,it]
data[it].pptv[173,*]=CH4_R5_4[*,0,0,it]
data[it].pptv[174,*]=CH4_R6_4[*,0,0,it]
data[it].pptv[175,*]=CH4_R7_4[*,0,0,it]

data[it].pptv[176,*]=CH4_W1_4[*,0,0,it]
data[it].pptv[177,*]=CH4_W2_4[*,0,0,it]
data[it].pptv[178,*]=CH4_W3_4[*,0,0,it]
data[it].pptv[179,*]=CH4_W4_4[*,0,0,it]
data[it].pptv[180,*]=CH4_W5_4[*,0,0,it]
data[it].pptv[181,*]=CH4_W6_4[*,0,0,it]
data[it].pptv[182,*]=CH4_W7_4[*,0,0,it]
data[it].pptv[183,*]=CH4_W8_4[*,0,0,it]
data[it].pptv[184,*]=CH4_W9_4[*,0,0,it]
data[it].pptv[185,*]=CH4_W10_4[*,0,0,it]
data[it].pptv[186,*]=CH4_W11_4[*,0,0,it]
data[it].pptv[187,*]=CH4_W12_4[*,0,0,it]
data[it].pptv[188,*]=CH4_W13_4[*,0,0,it]

data[it].pptv[189,*]=CH4_WAN_4[*,0,0,it]
data[it].pptv[190,*]=CH4_TER_4[*,0,0,it]
data[it].pptv[191,*]=CH4_OCE_4[*,0,0,it]
data[it].pptv[192,*]=CH4_VOL_4[*,0,0,it]

;;;AGECLASSE 5;;; 

data[it].pptv[193,*]=CH4_A1_5[*,0,0,it]
data[it].pptv[194,*]=CH4_A2_5[*,0,0,it]
data[it].pptv[195,*]=CH4_A3_5[*,0,0,it]
data[it].pptv[196,*]=CH4_A4_5[*,0,0,it]
data[it].pptv[197,*]=CH4_A5_5[*,0,0,it]
data[it].pptv[198,*]=CH4_A6_5[*,0,0,it]
data[it].pptv[199,*]=CH4_A7_5[*,0,0,it]
data[it].pptv[200,*]=CH4_A8_5[*,0,0,it]
data[it].pptv[201,*]=CH4_A9_5[*,0,0,it]
data[it].pptv[202,*]=CH4_A10_5[*,0,0,it]
data[it].pptv[203,*]=CH4_A11_5[*,0,0,it]

data[it].pptv[204,*]=CH4_B1_5[*,0,0,it]
data[it].pptv[205,*]=CH4_B2_5[*,0,0,it]
data[it].pptv[206,*]=CH4_B3_5[*,0,0,it]
data[it].pptv[207,*]=CH4_B4_5[*,0,0,it]
data[it].pptv[208,*]=CH4_B5_5[*,0,0,it]
data[it].pptv[209,*]=CH4_B6_5[*,0,0,it]
data[it].pptv[210,*]=CH4_B7_5[*,0,0,it]
data[it].pptv[211,*]=CH4_B8_5[*,0,0,it]
data[it].pptv[212,*]=CH4_B9_5[*,0,0,it]
data[it].pptv[213,*]=CH4_B10_5[*,0,0,it]
data[it].pptv[214,*]=CH4_B11_5[*,0,0,it]
data[it].pptv[215,*]=CH4_B12_5[*,0,0,it]
data[it].pptv[216,*]=CH4_B13_5[*,0,0,it]

data[it].pptv[217,*]=CH4_R1_5[*,0,0,it]
data[it].pptv[218,*]=CH4_R2_5[*,0,0,it]
data[it].pptv[219,*]=CH4_R3_5[*,0,0,it]
data[it].pptv[220,*]=CH4_R4_5[*,0,0,it]
data[it].pptv[221,*]=CH4_R5_5[*,0,0,it]
data[it].pptv[222,*]=CH4_R6_5[*,0,0,it]
data[it].pptv[223,*]=CH4_R7_5[*,0,0,it]

data[it].pptv[224,*]=CH4_W1_5[*,0,0,it]
data[it].pptv[225,*]=CH4_W2_5[*,0,0,it]
data[it].pptv[226,*]=CH4_W3_5[*,0,0,it]
data[it].pptv[227,*]=CH4_W4_5[*,0,0,it]
data[it].pptv[228,*]=CH4_W5_5[*,0,0,it]
data[it].pptv[229,*]=CH4_W6_5[*,0,0,it]
data[it].pptv[230,*]=CH4_W7_5[*,0,0,it]
data[it].pptv[231,*]=CH4_W8_5[*,0,0,it]
data[it].pptv[232,*]=CH4_W9_5[*,0,0,it]
data[it].pptv[233,*]=CH4_W10_5[*,0,0,it]
data[it].pptv[234,*]=CH4_W11_5[*,0,0,it]
data[it].pptv[235,*]=CH4_W12_5[*,0,0,it]
data[it].pptv[236,*]=CH4_W13_5[*,0,0,it]

data[it].pptv[237,*]=CH4_WAN_5[*,0,0,it]
data[it].pptv[238,*]=CH4_TER_5[*,0,0,it]
data[it].pptv[239,*]=CH4_OCE_5[*,0,0,it]
data[it].pptv[240,*]=CH4_VOL_5[*,0,0,it]



;;;;;;;;;;;;;;;;;;SD;;;;;;;;;;;;;;;;;;

data[it].std[0,*]=sd_Air_tracer[*,0,0,it]/16

;;;AGECLASSE 1;;; 

data[it].std[1,*]=sd_CH4_A1_1[*,0,0,it]/16
data[it].std[2,*]=sd_CH4_A2_1[*,0,0,it]/16
data[it].std[3,*]=sd_CH4_A3_1[*,0,0,it]/16
data[it].std[4,*]=sd_CH4_A4_1[*,0,0,it]/16
data[it].std[5,*]=sd_CH4_A5_1[*,0,0,it]/16
data[it].std[6,*]=sd_CH4_A6_1[*,0,0,it]/16
data[it].std[7,*]=sd_CH4_A7_1[*,0,0,it]/16
data[it].std[8,*]=sd_CH4_A8_1[*,0,0,it]/16
data[it].std[9,*]=sd_CH4_A9_1[*,0,0,it]/16
data[it].std[10,*]=sd_CH4_A10_1[*,0,0,it]/16
data[it].std[11,*]=sd_CH4_A11_1[*,0,0,it]/16

data[it].std[12,*]=sd_CH4_B1_1[*,0,0,it]/16
data[it].std[13,*]=sd_CH4_B2_1[*,0,0,it]/16
data[it].std[14,*]=sd_CH4_B3_1[*,0,0,it]/16
data[it].std[15,*]=sd_CH4_B4_1[*,0,0,it]/16
data[it].std[16,*]=sd_CH4_B5_1[*,0,0,it]/16
data[it].std[17,*]=sd_CH4_B6_1[*,0,0,it]/16
data[it].std[18,*]=sd_CH4_B7_1[*,0,0,it]/16
data[it].std[19,*]=sd_CH4_B8_1[*,0,0,it]/16
data[it].std[20,*]=sd_CH4_B9_1[*,0,0,it]/16
data[it].std[21,*]=sd_CH4_B10_1[*,0,0,it]/16
data[it].std[22,*]=sd_CH4_B11_1[*,0,0,it]/16
data[it].std[23,*]=sd_CH4_B12_1[*,0,0,it]/16
data[it].std[24,*]=sd_CH4_B13_1[*,0,0,it]/16

data[it].std[25,*]=sd_CH4_R1_1[*,0,0,it]/16
data[it].std[26,*]=sd_CH4_R2_1[*,0,0,it]/16
data[it].std[27,*]=sd_CH4_R3_1[*,0,0,it]/16
data[it].std[28,*]=sd_CH4_R4_1[*,0,0,it]/16
data[it].std[29,*]=sd_CH4_R5_1[*,0,0,it]/16
data[it].std[30,*]=sd_CH4_R6_1[*,0,0,it]/16
data[it].std[31,*]=sd_CH4_R7_1[*,0,0,it]/16

data[it].std[32,*]=sd_CH4_W1_1[*,0,0,it]/16
data[it].std[33,*]=sd_CH4_W2_1[*,0,0,it]/16
data[it].std[34,*]=sd_CH4_W3_1[*,0,0,it]/16
data[it].std[35,*]=sd_CH4_W4_1[*,0,0,it]/16
data[it].std[36,*]=sd_CH4_W5_1[*,0,0,it]/16
data[it].std[37,*]=sd_CH4_W6_1[*,0,0,it]/16
data[it].std[38,*]=sd_CH4_W7_1[*,0,0,it]/16
data[it].std[39,*]=sd_CH4_W8_1[*,0,0,it]/16
data[it].std[40,*]=sd_CH4_W9_1[*,0,0,it]/16
data[it].std[41,*]=sd_CH4_W10_1[*,0,0,it]/16
data[it].std[42,*]=sd_CH4_W11_1[*,0,0,it]/16
data[it].std[43,*]=sd_CH4_W12_1[*,0,0,it]/16
data[it].std[44,*]=sd_CH4_W13_1[*,0,0,it]/16

data[it].std[45,*]=sd_CH4_WAN_1[*,0,0,it]/16
data[it].std[46,*]=sd_CH4_TER_1[*,0,0,it]/16
data[it].std[47,*]=sd_CH4_OCE_1[*,0,0,it]/16
data[it].std[48,*]=sd_CH4_VOL_1[*,0,0,it]/16

;;;AGECLASSE 2;;; 

data[it].std[49,*]=sd_CH4_A1_2[*,0,0,it]/16
data[it].std[50,*]=sd_CH4_A2_2[*,0,0,it]/16
data[it].std[51,*]=sd_CH4_A3_2[*,0,0,it]/16
data[it].std[52,*]=sd_CH4_A4_2[*,0,0,it]/16
data[it].std[53,*]=sd_CH4_A5_2[*,0,0,it]/16
data[it].std[54,*]=sd_CH4_A6_2[*,0,0,it]/16
data[it].std[55,*]=sd_CH4_A7_2[*,0,0,it]/16
data[it].std[56,*]=sd_CH4_A8_2[*,0,0,it]/16
data[it].std[57,*]=sd_CH4_A9_2[*,0,0,it]/16
data[it].std[58,*]=sd_CH4_A10_2[*,0,0,it]/16
data[it].std[59,*]=sd_CH4_A11_2[*,0,0,it]/16

data[it].std[60,*]=sd_CH4_W1_2[*,0,0,it]/16
data[it].std[61,*]=sd_CH4_W2_2[*,0,0,it]/16
data[it].std[62,*]=sd_CH4_W3_2[*,0,0,it]/16
data[it].std[63,*]=sd_CH4_W4_2[*,0,0,it]/16
data[it].std[64,*]=sd_CH4_W5_2[*,0,0,it]/16
data[it].std[65,*]=sd_CH4_W6_2[*,0,0,it]/16
data[it].std[66,*]=sd_CH4_W7_2[*,0,0,it]/16
data[it].std[67,*]=sd_CH4_W8_2[*,0,0,it]/16
data[it].std[68,*]=sd_CH4_W9_2[*,0,0,it]/16
data[it].std[69,*]=sd_CH4_W10_2[*,0,0,it]/16
data[it].std[70,*]=sd_CH4_W11_2[*,0,0,it]/16
data[it].std[71,*]=sd_CH4_W12_2[*,0,0,it]/16
data[it].std[72,*]=sd_CH4_W13_2[*,0,0,it]/16

data[it].std[73,*]=sd_CH4_R1_2[*,0,0,it]/16
data[it].std[74,*]=sd_CH4_R2_2[*,0,0,it]/16
data[it].std[75,*]=sd_CH4_R3_2[*,0,0,it]/16
data[it].std[76,*]=sd_CH4_R4_2[*,0,0,it]/16
data[it].std[77,*]=sd_CH4_R5_2[*,0,0,it]/16
data[it].std[78,*]=sd_CH4_R6_2[*,0,0,it]/16
data[it].std[79,*]=sd_CH4_R7_2[*,0,0,it]/16

data[it].std[80,*]=sd_CH4_B1_2[*,0,0,it]/16
data[it].std[81,*]=sd_CH4_B2_2[*,0,0,it]/16
data[it].std[82,*]=sd_CH4_B3_2[*,0,0,it]/16
data[it].std[83,*]=sd_CH4_B4_2[*,0,0,it]/16
data[it].std[84,*]=sd_CH4_B5_2[*,0,0,it]/16
data[it].std[85,*]=sd_CH4_B6_2[*,0,0,it]/16
data[it].std[86,*]=sd_CH4_B7_2[*,0,0,it]/16
data[it].std[87,*]=sd_CH4_B8_2[*,0,0,it]/16
data[it].std[88,*]=sd_CH4_B9_2[*,0,0,it]/16
data[it].std[89,*]=sd_CH4_B10_2[*,0,0,it]/16
data[it].std[90,*]=sd_CH4_B11_2[*,0,0,it]/16
data[it].std[91,*]=sd_CH4_B12_2[*,0,0,it]/16
data[it].std[92,*]=sd_CH4_B13_2[*,0,0,it]/16

data[it].std[93,*]=sd_CH4_WAN_2[*,0,0,it]/16
data[it].std[94,*]=sd_CH4_TER_2[*,0,0,it]/16
data[it].std[95,*]=sd_CH4_OCE_2[*,0,0,it]/16
data[it].std[96,*]=sd_CH4_VOL_2[*,0,0,it]/16


;;;AGECLASSE 3;;; 

data[it].std[97,*]=sd_CH4_A1_3[*,0,0,it]/16
data[it].std[98,*]=sd_CH4_A2_3[*,0,0,it]/16
data[it].std[99,*]=sd_CH4_A3_3[*,0,0,it]/16
data[it].std[100,*]=sd_CH4_A4_3[*,0,0,it]/16
data[it].std[101,*]=sd_CH4_A5_3[*,0,0,it]/16
data[it].std[102,*]=sd_CH4_A6_3[*,0,0,it]/16
data[it].std[103,*]=sd_CH4_A7_3[*,0,0,it]/16
data[it].std[104,*]=sd_CH4_A8_3[*,0,0,it]/16
data[it].std[105,*]=sd_CH4_A9_3[*,0,0,it]/16
data[it].std[106,*]=sd_CH4_A10_3[*,0,0,it]/16
data[it].std[107,*]=sd_CH4_A11_3[*,0,0,it]/16

data[it].std[108,*]=sd_CH4_B1_3[*,0,0,it]/16
data[it].std[109,*]=sd_CH4_B2_3[*,0,0,it]/16
data[it].std[110,*]=sd_CH4_B3_3[*,0,0,it]/16
data[it].std[111,*]=sd_CH4_B4_3[*,0,0,it]/16
data[it].std[112,*]=sd_CH4_B5_3[*,0,0,it]/16
data[it].std[113,*]=sd_CH4_B6_3[*,0,0,it]/16
data[it].std[114,*]=sd_CH4_B7_3[*,0,0,it]/16
data[it].std[115,*]=sd_CH4_B8_3[*,0,0,it]/16
data[it].std[116,*]=sd_CH4_B9_3[*,0,0,it]/16
data[it].std[117,*]=sd_CH4_B10_3[*,0,0,it]/16
data[it].std[118,*]=sd_CH4_B11_3[*,0,0,it]/16
data[it].std[119,*]=sd_CH4_B12_3[*,0,0,it]/16
data[it].std[120,*]=sd_CH4_B13_3[*,0,0,it]/16

data[it].std[121,*]=sd_CH4_R1_3[*,0,0,it]/16
data[it].std[122,*]=sd_CH4_R2_3[*,0,0,it]/16
data[it].std[123,*]=sd_CH4_R3_3[*,0,0,it]/16
data[it].std[124,*]=sd_CH4_R4_3[*,0,0,it]/16
data[it].std[125,*]=sd_CH4_R5_3[*,0,0,it]/16
data[it].std[126,*]=sd_CH4_R6_3[*,0,0,it]/16
data[it].std[127,*]=sd_CH4_R7_3[*,0,0,it]/16

data[it].std[128,*]=sd_CH4_W1_3[*,0,0,it]/16
data[it].std[129,*]=sd_CH4_W2_3[*,0,0,it]/16
data[it].std[130,*]=sd_CH4_W3_3[*,0,0,it]/16
data[it].std[131,*]=sd_CH4_W4_3[*,0,0,it]/16
data[it].std[132,*]=sd_CH4_W5_3[*,0,0,it]/16
data[it].std[133,*]=sd_CH4_W6_3[*,0,0,it]/16
data[it].std[134,*]=sd_CH4_W7_3[*,0,0,it]/16
data[it].std[135,*]=sd_CH4_W8_3[*,0,0,it]/16
data[it].std[136,*]=sd_CH4_W9_3[*,0,0,it]/16
data[it].std[137,*]=sd_CH4_W10_3[*,0,0,it]/16
data[it].std[138,*]=sd_CH4_W11_3[*,0,0,it]/16
data[it].std[139,*]=sd_CH4_W12_3[*,0,0,it]/16
data[it].std[140,*]=sd_CH4_W13_3[*,0,0,it]/16

data[it].std[141,*]=sd_CH4_WAN_3[*,0,0,it]/16
data[it].std[142,*]=sd_CH4_TER_3[*,0,0,it]/16
data[it].std[143,*]=sd_CH4_OCE_3[*,0,0,it]/16
data[it].std[144,*]=sd_CH4_VOL_3[*,0,0,it]/16

;;;AGECLASSE 4;;; 

data[it].std[145,*]=sd_CH4_A1_4[*,0,0,it]/16
data[it].std[146,*]=sd_CH4_A2_4[*,0,0,it]/16
data[it].std[147,*]=sd_CH4_A3_4[*,0,0,it]/16
data[it].std[148,*]=sd_CH4_A4_4[*,0,0,it]/16
data[it].std[149,*]=sd_CH4_A5_4[*,0,0,it]/16
data[it].std[150,*]=sd_CH4_A6_4[*,0,0,it]/16
data[it].std[151,*]=sd_CH4_A7_4[*,0,0,it]/16
data[it].std[152,*]=sd_CH4_A8_4[*,0,0,it]/16
data[it].std[153,*]=sd_CH4_A9_4[*,0,0,it]/16
data[it].std[154,*]=sd_CH4_A10_4[*,0,0,it]/16
data[it].std[155,*]=sd_CH4_A11_4[*,0,0,it]/16

data[it].std[156,*]=sd_CH4_B1_4[*,0,0,it]/16
data[it].std[157,*]=sd_CH4_B2_4[*,0,0,it]/16
data[it].std[158,*]=sd_CH4_B3_4[*,0,0,it]/16
data[it].std[159,*]=sd_CH4_B4_4[*,0,0,it]/16
data[it].std[160,*]=sd_CH4_B5_4[*,0,0,it]/16
data[it].std[161,*]=sd_CH4_B6_4[*,0,0,it]/16
data[it].std[162,*]=sd_CH4_B7_4[*,0,0,it]/16
data[it].std[163,*]=sd_CH4_B8_4[*,0,0,it]/16
data[it].std[164,*]=sd_CH4_B9_4[*,0,0,it]/16
data[it].std[165,*]=sd_CH4_B10_4[*,0,0,it]/16
data[it].std[166,*]=sd_CH4_B11_4[*,0,0,it]/16
data[it].std[167,*]=sd_CH4_B12_4[*,0,0,it]/16
data[it].std[168,*]=sd_CH4_B13_4[*,0,0,it]/16

data[it].std[169,*]=sd_CH4_R1_4[*,0,0,it]/16
data[it].std[170,*]=sd_CH4_R2_4[*,0,0,it]/16
data[it].std[171,*]=sd_CH4_R3_4[*,0,0,it]/16
data[it].std[172,*]=sd_CH4_R4_4[*,0,0,it]/16
data[it].std[173,*]=sd_CH4_R5_4[*,0,0,it]/16
data[it].std[174,*]=sd_CH4_R6_4[*,0,0,it]/16
data[it].std[175,*]=sd_CH4_R7_4[*,0,0,it]/16

data[it].std[176,*]=sd_CH4_W1_4[*,0,0,it]/16
data[it].std[177,*]=sd_CH4_W2_4[*,0,0,it]/16
data[it].std[178,*]=sd_CH4_W3_4[*,0,0,it]/16
data[it].std[179,*]=sd_CH4_W4_4[*,0,0,it]/16
data[it].std[180,*]=sd_CH4_W5_4[*,0,0,it]/16
data[it].std[181,*]=sd_CH4_W6_4[*,0,0,it]/16
data[it].std[182,*]=sd_CH4_W7_4[*,0,0,it]/16
data[it].std[183,*]=sd_CH4_W8_4[*,0,0,it]/16
data[it].std[184,*]=sd_CH4_W9_4[*,0,0,it]/16
data[it].std[185,*]=sd_CH4_W10_4[*,0,0,it]/16
data[it].std[186,*]=sd_CH4_W11_4[*,0,0,it]/16
data[it].std[187,*]=sd_CH4_W12_4[*,0,0,it]/16
data[it].std[188,*]=sd_CH4_W13_4[*,0,0,it]/16

data[it].std[189,*]=sd_CH4_WAN_4[*,0,0,it]/16
data[it].std[190,*]=sd_CH4_TER_4[*,0,0,it]/16
data[it].std[191,*]=sd_CH4_OCE_4[*,0,0,it]/16
data[it].std[192,*]=sd_CH4_VOL_4[*,0,0,it]/16

;;;AGECLASSE 5;;; 

data[it].std[193,*]=sd_CH4_A1_5[*,0,0,it]/16
data[it].std[194,*]=sd_CH4_A2_5[*,0,0,it]/16
data[it].std[195,*]=sd_CH4_A3_5[*,0,0,it]/16
data[it].std[196,*]=sd_CH4_A4_5[*,0,0,it]/16
data[it].std[197,*]=sd_CH4_A5_5[*,0,0,it]/16
data[it].std[198,*]=sd_CH4_A6_5[*,0,0,it]/16
data[it].std[199,*]=sd_CH4_A7_5[*,0,0,it]/16
data[it].std[200,*]=sd_CH4_A8_5[*,0,0,it]/16
data[it].std[201,*]=sd_CH4_A9_5[*,0,0,it]/16
data[it].std[202,*]=sd_CH4_A10_5[*,0,0,it]/16
data[it].std[203,*]=sd_CH4_A11_5[*,0,0,it]/16

data[it].std[204,*]=sd_CH4_B1_5[*,0,0,it]/16
data[it].std[205,*]=sd_CH4_B2_5[*,0,0,it]/16
data[it].std[206,*]=sd_CH4_B3_5[*,0,0,it]/16
data[it].std[207,*]=sd_CH4_B4_5[*,0,0,it]/16
data[it].std[208,*]=sd_CH4_B5_5[*,0,0,it]/16
data[it].std[209,*]=sd_CH4_B6_5[*,0,0,it]/16
data[it].std[210,*]=sd_CH4_B7_5[*,0,0,it]/16
data[it].std[211,*]=sd_CH4_B8_5[*,0,0,it]/16
data[it].std[212,*]=sd_CH4_B9_5[*,0,0,it]/16
data[it].std[213,*]=sd_CH4_B10_5[*,0,0,it]/16
data[it].std[214,*]=sd_CH4_B11_5[*,0,0,it]/16
data[it].std[215,*]=sd_CH4_B12_5[*,0,0,it]/16
data[it].std[216,*]=sd_CH4_B13_5[*,0,0,it]/16

data[it].std[217,*]=sd_CH4_R1_5[*,0,0,it]/16
data[it].std[218,*]=sd_CH4_R2_5[*,0,0,it]/16
data[it].std[219,*]=sd_CH4_R3_5[*,0,0,it]/16
data[it].std[220,*]=sd_CH4_R4_5[*,0,0,it]/16
data[it].std[221,*]=sd_CH4_R5_5[*,0,0,it]/16
data[it].std[222,*]=sd_CH4_R6_5[*,0,0,it]/16
data[it].std[223,*]=sd_CH4_R7_5[*,0,0,it]/16

data[it].std[224,*]=sd_CH4_W1_5[*,0,0,it]/16
data[it].std[225,*]=sd_CH4_W2_5[*,0,0,it]/16
data[it].std[226,*]=sd_CH4_W3_5[*,0,0,it]/16
data[it].std[227,*]=sd_CH4_W4_5[*,0,0,it]/16
data[it].std[228,*]=sd_CH4_W5_5[*,0,0,it]/16
data[it].std[229,*]=sd_CH4_W6_5[*,0,0,it]/16
data[it].std[230,*]=sd_CH4_W7_5[*,0,0,it]/16
data[it].std[231,*]=sd_CH4_W8_5[*,0,0,it]/16
data[it].std[232,*]=sd_CH4_W9_5[*,0,0,it]/16
data[it].std[233,*]=sd_CH4_W10_5[*,0,0,it]/16
data[it].std[234,*]=sd_CH4_W11_5[*,0,0,it]/16
data[it].std[235,*]=sd_CH4_W12_5[*,0,0,it]/16
data[it].std[236,*]=sd_CH4_W13_5[*,0,0,it]/16

data[it].std[237,*]=sd_CH4_WAN_5[*,0,0,it]/16
data[it].std[238,*]=sd_CH4_TER_5[*,0,0,it]/16
data[it].std[239,*]=sd_CH4_OCE_5[*,0,0,it]/16
data[it].std[240,*]=sd_CH4_VOL_5[*,0,0,it]/16



;;;;;;;;;;;NN AND XK;;;;;;;;;;;;;


data[it].avgnum[*] = nn_Air_tracer[*,0,0,it] ;TEST for now
;data[it].avgnum[1,*]=nn_CH4_A1_5[*,0,0,it]



data[it].kernweights[*] = xk_Air_tracer[*,0,0,it] ; TEST for now
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


END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
