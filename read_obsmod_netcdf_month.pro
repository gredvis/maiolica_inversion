;+
; NAME:
;
;  read_obsmod_netcdf_month
;
; PURPOSE:
;
;  Read CH4 observation and model data for a given month yyyymm which have been
;  pre-processed with inv_create_monthly_obs_mod_data.pro.
;  The data roughly represent weekly means. The model data includes the output
;  of all CH4 tracers and age classes.
;
; CATEGORY:
;
;  MAIOLICA-2, CH4 inversion
;
; CALLING SEQUENCE:
;
;  read_obsmod_netcdf_month,sim,yyyymm,ch4obs=ch4obs,ch4mod=ch4mod
;
; INPUTS:
;
;  sim                (structure) : the model simulation information structure
;                                  (see inv_configurations.pro for details)
;  yyyymm             (string)    : the year and month for which to get the data
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;  ch4obs    : The observation data. Vector of structures of the form
;              {dtg:'',name:'',lon:0.,lat:0.,ch4:0.,stdch4:0.,numobs:0,type:''}
;               where name is the station name, type the station type
;               and numobs the number of individual data points contributing
;               to the average
;  ch4mod    : The corresponding model data. Vector of structures of the form
;               {dtg:'',name:'',dtg:'',ch4:0.,stdch4:0,ch4trace:FltArr(ntrace*nage),$
;                stdch4trace:FltArr(ntrace*nage),nummod:0}
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  none
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;  none
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   02 Jan 2018: first implementation. Replaces previous routines
;   read_processed_obs_data_month and read_processed_mod_data_month.
;-
PRO read_obsmod_netcdf_month,sim,yyyymm,ch4obs=ch4obs,ch4mod=ch4mod

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF
  
  ;*******************************
  ;* read data
  ;******************************* 
  ncfile = sim.obsmoddir + 'maiolica2_obs_mod_'+yyyymm+'.nc'

  obsrec = {dtg:'',name:'',lon:0.,lat:0.,ch4:0.,stdch4:0.,numobs:0,type:''}
 
  modrec = {dtg:'',name:'',ch4:0.,stdch4:0,ch4trace:FltArr(sim.ntrace*sim.nage),$
            stdch4trace:FltArr(sim.ntrace*sim.nage),nummod:0}

  IF NOT file_test(ncfile) THEN BEGIN
     print,'could not find file ',ncfile
     RETURN
  ENDIF

  ncid = ncdf_open(ncfile)

  varid = ncdf_varid(ncid,'statid')
  ncdf_varget,ncid,varid,statid
  statid = string(statid)
  ndat = n_elements(statid)

  varid = ncdf_varid(ncid,'obs_type')
  ncdf_varget,ncid,varid,obstype
  obstype = string(obstype)

  ;; create index of data points corresponding to current simulation configuration
  iok = BytArr(ndat)+1B
  FOR i=0,ndat-1 DO BEGIN
     ;IF sim.flask THEN iok[i] = obstype[i] EQ 'ev' ELSE iok[i] = 1B
     iok[i] =  obstype[i] EQ 'ev' OR NOT sim.flask
     index = WHERE(statid[i] EQ sim.stats,cnt)
     iok[i] = iok[i] AND (cnt GT 0)
  ENDFOR

  index = WHERE(iok,ndat)

  ch4obs = replicate(obsrec,ndat)
  ch4mod = replicate(modrec,ndat)

  ;; read in remaining fields and assign
  varid = ncdf_varid(ncid,'time')
  ncdf_varget,ncid,varid,time
  ncdf_attget,ncid,varid,'units',tunits

  yyyy='' & mm='' & dd = '' & hh = '' & minu = ''
  reads,tunits,yyyy,mm,dd,hh,minu,format='(11x,a4,1x,a2,1x,a2,1x,a2,1x,a2)'
  reftime = dtg2gvtime(yyyy+mm+dd+hh+minu)
  dtg = gvtime2dtg(time+reftime)

  varid = ncdf_varid(ncid,'lon')
  ncdf_varget,ncid,varid,lon
  varid = ncdf_varid(ncid,'lat')
  ncdf_varget,ncid,varid,lat
  varid = ncdf_varid(ncid,'obs_CH4')
  ncdf_varget,ncid,varid,obsch4
  varid = ncdf_varid(ncid,'obs_stdev_CH4')
  ncdf_varget,ncid,varid,obsstdCH4
  varid = ncdf_varid(ncid,'obs_num')
  ncdf_varget,ncid,varid,obsnum
  varid = ncdf_varid(ncid,'mod_CH4')
  ncdf_varget,ncid,varid,modch4
  varid = ncdf_varid(ncid,'mod_stdev_CH4')
  ncdf_varget,ncid,varid,modstdCH4
  varid = ncdf_varid(ncid,'mod_CH4tracer')
  ncdf_varget,ncid,varid,modch4trace
  varid = ncdf_varid(ncid,'mod_stdev_CH4tracer')
  ncdf_varget,ncid,varid,modstdCH4trace
  varid = ncdf_varid(ncid,'mod_num')
  ncdf_varget,ncid,varid,modnum

  ch4obs.lon = lon[index]
  ch4obs.lat = lat[index]
  ch4obs.name = statid[index]
  ch4obs.type = obstype[index]
  ch4obs.dtg = dtg[index]
  ch4obs.ch4 = obsch4[index]
  ch4obs.stdch4 = obsstdch4[index]
  ch4obs.numobs = obsnum[index]

  ch4mod.name = statid[index]
  ch4mod.dtg = dtg[index]
  ch4mod.ch4 = modch4[index]
  ch4mod.stdch4 = modstdch4[index]
  ch4mod.ch4trace = modch4trace[*,index]
  ch4mod.stdch4trace = modstdch4trace[*,index]
  ch4mod.nummod = modnum[index]

  IF sim.dlr THEN BEGIN
     ch4mod.ch4=ch4mod.ch4*sim.dlrscale
     ch4mod.ch4trace=ch4mod.ch4trace*sim.dlrscale
     ch4mod.stdch4 = ch4mod.stdch4*sim.dlrscale
     ch4mod.stdch4trace = ch4mod.stdch4trace*sim.dlrscale
  ENDIF

  ncdf_close,ncid

END
