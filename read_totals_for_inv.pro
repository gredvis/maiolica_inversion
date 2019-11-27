;+
; NAME:
;
;   read_totals_for_inv
;
; PURPOSE:
;
;   Read totals binary files of FLEXPART
;
;   Attention!!
;   Totals for month i are stored in directory for month i+1
; 
; CATEGORY:
;
;   trajectory, atmospheric transport, dispersion modelling
;
; CALLING SEQUENCE:
;
;   read_totals_for_inv,sim,yyyymm,data=data
;
; INPUTS:
;
;       sim:    (structure) the simulation information structure
;       yyyymm: (STRING) year and month for which to get totals
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;       data:    xx
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
;
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;-

;--------------------------------------------------------------------

PRO read_totals_for_inv,sim,yyyymm,data=data

  ;; Attention: totals output for month i is in directory for month i+1
  yyyymmp1 = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')+40),0,6)
  dirt = sim.modeldir+sim.name+'/'+yyyymmp1+'01/'

  fileheader = dirt+'header'
  filename   = dirt+'totals'

  ; call header reading routine
  read_flexpart_header,filename=fileheader,header=header

  ; define variables for reading
  itime  = 0L
  nspec  = header.nspec
  ncats  = 7L            ; mass, emissions uptake, emissions field, ...
  kg2tg  = 1.e-9         ; convert kg to Tg

  ; open the file, determine record number
  openr,lun,filename,/get_lun,/f77_unformatted
  nrec = 0 & savetime = 0L
  structin  = {itime:0L,budget:DblArr(ncats*nspec)}
  structout = {itime:0L,budget:DblArr(nspec,ncats)}
  WHILE NOT eof(lun) DO BEGIN
    readu,lun,structin
    nrec += 1
  ENDWHILE
  tmp = replicate(structout,nrec)
  point_lun,lun,0
  ihelp     = 0L
  tmpbudget = DblArr(ncats*nspec)
  FOR i=0,nrec-1 DO BEGIN
    readu,lun,ihelp,tmpbudget
    structout.budget = reform(tmpbudget,nspec,ncats)  
    tmp[i].budget    = structout.budget*kg2tg
    tmp[i].itime     = ihelp
  ENDFOR
  free_lun,lun
  
  catnames = ['mass','em_up','em_field','em_res','OH_loss','OH_gain','chem_loss']
  specnames = header.species

  data = {catnames:catnames,specnames:specnames,itime:tmp.itime,budget:tmp.budget}
 
END

