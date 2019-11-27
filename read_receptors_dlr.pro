;+
; NAME:
;
;   read_receptors_dlr
;
; PURPOSE:
;
;   Read receptor output of DLR EMAC simulations for month yyyymm
;
; 
; CATEGORY:
;
;   atmospheric transport, dispersion modelling,
;
; CALLING SEQUENCE:
;
;   read_receptors_dlr,sim,yyyymm,info=info,data=data
;
; INPUTS:
;
;       sim      :  structure with all information about the simulation
;                   see inv_configurations.pro for details
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
;           {ppb:DblArr(nspec,nrcpt),std:DblArr(nspec,nrcpt),avgnum:DblArr(nrcpt),$
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
;  Dominik Brunner (DB), Empa
;  DB, 2 Dec 2017: replaces Florian Arfeuille's read_receptors_DLR_final.pro 
;-
PRO read_receptors_dlr,sim,yyyymm,info=info,data=data,dtg=dtg

  ok=0
  IF n_elements(sim) EQ 0 THEN BEGIN
     message,'parameter sim missing in call',/continue
     RETURN
  ENDIF
  IF n_elements(yyyymm) EQ 0 THEN BEGIN
     message,'parameter yyyymm missing in call',/continue
     RETURN
  ENDIF

  ;; directory of DLR model output
  dlrdir = sim.modeldir+'DLR/empa/'
  
  ;; problem with netcdf library made it necessary to copy files to /bigdat
  ;; see https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/5dbp8dV1q3U
  dlrdir = '/bigdat/brd134/tmp/empa/'

  ;; names of variables used in DLR output
  cat = emiss_categories(/dlr,labels=labels)

  ;; loop over the stations and get tracer data
  nst = n_elements(sim.stats)
  rec = {rcptname:'',name:'',xrcpt:0.,yrcpt:0.,zrcpt:0.,dlrname:'',dlrheight:0.}
  info = replicate(rec,nst)

  FOR k=0,nst-1 DO BEGIN

     sinfo=station_info(sim.stats[k])
     dlrid = station_mapping_empa2dlr(sim.stats[k])
     IF dlrid EQ '?missing?' THEN stop
     info[k].rcptname=sim.stats[k]
     info[k].name=sinfo.name
     info[k].dlrname=dlrid
     info[k].zrcpt=sinfo.alt
     info[k].xrcpt=sinfo.lon
     info[k].yrcpt=sinfo.lat

     direc = dlrdir+'scout_'+dlrid+'/'
     IF file_test(direc) EQ 0 THEN GOTO,nextstat

     IF yyyymm ge 200800 then BEGIN
        filename = direc+'empa14_________'+yyyymm+'_scout_'+dlrid+'_atstatheight.nc'
     ENDIF ELSE BEGIN
        filename = direc+'empa___________'+yyyymm+'_scout_'+dlrid+'_atstatheight.nc'
     ENDELSE

     ;print,'reading file ',filename
     IF file_test(filename) EQ 0 THEN stop
     ncid = ncdf_open(filename)
     ncdf_varget,ncid,'time',time
     ntime=n_elements(time)

     ;; read global attribute station height
     ncdf_attget,ncid,'height',height,/global
     info[k].dlrheight=height

     IF k EQ 0 THEN BEGIN
        ntime0 = ntime
        varid=ncdf_varid(ncid,'time')
        ncdf_attget,ncid,varid,'units',units
        units=string(units)
        pos=strpos(units,'since')+6
        ryyyy = STRMID(units,pos,4) & rmm = STRMID(units,pos+5,2)
        rdd = STRMID(units,pos+8,2) & rhh = STRMID(units,pos+11,2)
        dtg = gvtime2dtg(dtg2gvtime(ryyyy+rmm+rdd+rhh)+time)
        time=(time-time[0]+1)*86400
        ;; Initialize data structure array
        nspec=241 ; number CH4 species (48*5) + air_tracer
        rec  = {ppb:DblArr(nspec,n_elements(sim.stats))+!values.d_nan}
        data = replicate(rec,ntime)
        data.ppb[0,*]=1D       ; air tracer set to 1. for DLR output
     ENDIF
     IF ntime NE ntime0 THEN stop

     ;;****************************************************************
     ;; read all tracers. tracer 0 is air tracer, which does not exist
     ;; in DLR output but is kept here for compatibility
     ;;****************************************************************
     FOR i=0,sim.nage-1 DO BEGIN
        FOR j=0,sim.ntrace-1 DO BEGIN
           varname = labels[j]+'_a'+STRING(i+1,format='(i2.2)')+'_ave'
           ncdf_varget,ncid,varname,var
           data.ppb[i*sim.ntrace+j+1L,k]=var*1e9 ; convert to ppb
        ENDFOR
     ENDFOR
     ncdf_close,ncid
     
     nextstat:

  ENDFOR ; end loop over station

END
