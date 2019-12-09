;+
; NAME:
;
;  read_receptors_maiolica
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
;   read_receptors_maiolica,sim,yyyymm,info=info,data=data,dtg=dtg
;
; INPUTS:
;
;       sim      :  structure with all information about the simulation, see
;                   inv_configurations.pro for details
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
;    dtg:   the output date/times (YYYYMMDDhhmm), centered on midday (UTC)
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
;  sim = inv_configurations(run='NEW_DLR',sconfig='flask_DLR2',ok=ok)
;
;  yyyymm = '200401' ; get receptor point values for Jan 2004
;  read_receptors_maiolica_final,sim,yyyymm,info=info,data=data
;
; MODIFICATION HISTORY:
;  Tina Schnadt, Empa: 2012 first implementation
;  Dominik Brunner, Empa, Feb 2017: modified to use new simulation structure sim
;-

;--------------------------------------------------------------------

PRO read_receptors_maiolica,sim,yyyymm,info=info,data=data,dtg=dtg

  IF n_elements(sim) EQ 0 THEN BEGIN
     message,'parameter sim missing in call',/continue
     RETURN
  ENDIF
  
  ;; Attention: receptor output for month i is in directory for month i+1
  yyyymmp1 = STRMID(gvtime2dtg(dtg2gvtime(yyyymm+'010000')+40),0,6)
  direc = sim.modeldir+yyyymmp1+'01/'

  file = direc+'receptor_pptv.nc'
  nspec = sim.ntrace*sim.nage+1 ; including air_tracer
  nstat = n_elements(sim.stats)
  
  ;; get emission categories and corresponding labels used in receptor files
  cat = emiss_categories(labels=labels)
  
;file = direc+'header'
;read_header_URMEL,filename=fileheader,header=header
;  nspec  = header.nspec
;nspec=11 ;TEST

;  mv   = -9.96921e36

;gsw_socol=fltarr(nspsw,nlat,nz,nmonth*nyear)
  ; openfield
  ncid = ncdf_open(file)

  ;; get dimensions
  dimxid=ncdf_dimid(ncid,'time')
  ncdf_diminq,ncid,dimxid,name,ntime
  dimyid=ncdf_dimid(ncid,'rec')
  ncdf_diminq,ncid,dimyid,name,nrcpt
  dimtid=ncdf_dimid(ncid,'pnt')
  ncdf_diminq,ncid,dimtid,name,npnt
  dimtid=ncdf_dimid(ncid,'age')
  ncdf_diminq,ncid,dimtid,name,nage

  ;; get receptor date/times
  varid=ncdf_varid(ncid,'time')
  ncdf_varget,ncid,varid,time
  ncdf_attget,ncid,varid,'units',tunits
  ;; convert times into dates
  tunits=string(tunits)
  pos = strpos(tunits,'since')
  refdate = STRMID(tunits,pos+6,STRLEN(tunits)-pos-6)
  refdtg = STRMID(refdate,0,4)+STRMID(refdate,5,2)+STRMID(refdate,8,2)+$
           STRMID(refdate,11,2)+STRMID(refdate,14,2)
  gvtime = dtg2gvtime(refdtg)+time/86400D - 1D ; shift by 1 day because date label is at end of day
  dtg = gvtime2dtg(gvtime)

  ;; get receptor details (name, position)
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

  ;;    info: structure array of length nstat (number of stations) containing name 
  ;;          and coordinates of receptor point and number of time points
  ;;    data: structure array of length ntime (number of time points) containing the 
  ;;          actual concentrations for the nspec different species
  ;;          at the nstat different receptor points.
  rec = {rcptname:'',xrcpt:0.,yrcpt:0.,zrcpt:0.,hxmax:0.,hymax:0.,hzmax:0.,time:0}
  info  = replicate(rec,nstat)

  rec  = {ppb:DblArr(nspec,nstat),std:DblArr(nspec,nstat),avgnum:DblArr(nstat),$
          kernweights:DblArr(nstat)}
  data = replicate(rec,ntime)

  ;; get indices of receptors corresponding to list of stations sim.stats
  ;; and optimal receptor levels
  rname=strlowcase(receptorname)
  index = WHERE(STRMID(rname,0,3) EQ 'beg',cnt)
  IF cnt NE 0 THEN rname[index] = 'bgu  0'
  index = WHERE(STRMID(rname,0,3) EQ 'ile',cnt)
  IF cnt NE 0 THEN rname[index] = 'lpo  0'

  statind = IntArr(nstat)-1
  rlist = strsplit(rname,' ',/extract)
  rlist = rlist.toarray()
  rlist = strcompress(rlist,/rem)
  FOR i=0,nstat-1 DO BEGIN
     index = WHERE(rlist[*,0] EQ sim.stats[i] AND rlist[*,1] EQ sim.stat_levs[i],cnt)
     IF cnt EQ 0 AND sim.stat_levs[i] GT 0. THEN BEGIN
        ;; try 250 m lower level
        index = WHERE(rlist[*,0] EQ sim.stats[i] AND rlist[*,1] EQ $
                      strcompress(fix(sim.stat_levs[i])-250,/rem),cnt)
        IF cnt GT 0 THEN print,'level for ',sim.stats[i],' shifted by -250 m'
     ENDIF
     IF cnt EQ 0 THEN BEGIN
        print,'station lev ',sim.stats[i]+' '+sim.stat_levs[i],' not found in receptor output'
        stop
     ENDIF ELSE statind[i] = index[0]
  ENDFOR
  statind = statind[WHERE(statind NE -1)]

  ;;*************************************************
  ;; assign air tracer
  ;;*************************************************
  ncdf_varget,ncid,'Air_tracer',Air_tracer
  ncdf_varget,ncid,'nn_Air_tracer',nn_Air_tracer
  ncdf_varget,ncid,'xk_Air_tracer',xk_Air_tracer
  ncdf_varget,ncid,'sd_Air_tracer',sd_Air_tracer

  data.ppb[0,*]=reform(Air_tracer[statind,0,0,*],1,nstat,ntime)
  data.std[0,*]=reform(sd_Air_tracer[statind,0,0,*]/16.,1,nstat,ntime)
  data.avgnum=reform(nn_Air_tracer[statind,0,0,*],nstat,ntime)
  data.kernweights=reform(xk_Air_tracer[statind,0,0,*],nstat,ntime)
  
  ;;*************************************************
  ;; assign all other tracers, and convert to ppb
  ;;*************************************************

  FOR i=0,sim.nage-1 DO BEGIN
     FOR j=0,sim.ntrace-1 DO BEGIN
        varname = 'CH4_'+labels[j]+'_'+STRING(i+1,format='(i1)')
        ncdf_varget,ncid,varname,var
        varname = 'sd_'+varname
        ncdf_varget,ncid,varname,sdvar
        data.ppb[i*sim.ntrace+j+1L,*]=reform(var[statind,0,0,*],1,nstat,ntime)/data.ppb[0,*]*1e9
        data.std[i*sim.ntrace+j+1L,*]=reform(sdvar[statind,0,0,*]/16.,1,nstat,ntime)/data.ppb[0,*]*1e9
     ENDFOR
  ENDFOR

  ;;FOR MCF:
  ;;ncdf_varget,ncid,'MCF_1           Tr',MCF_1   
  ;;ncdf_varget,ncid,'MCF_2           Tr',MCF_1          

  ncdf_close,ncid

  info.rcptname=rname[statind]
  ind = WHERE(info.rcptname eq 'Matrova',c)
  IF c eq 1L THEN info[ind].rcptname = 'Matorova 0'

  info.xrcpt=lon[statind]
  info.yrcpt=lat[statind]
  info.zrcpt=lev[statind]
  info.hxmax=hx[statind]
  info.hymax=hy[statind]
  info.hzmax=hz[statind]
  info.time=ntime

END
