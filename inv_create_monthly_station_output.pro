;+
; NAME:
;
;   inv_create_monthly_station_output
;
; PURPOSE:
;
;   For a given inversion simulation, create a netcdf file with monthly
;   mean observations and a priori and a posteriori simulated values.
;   This output can be used to 
;   a) calculate monthly mean model-observation mismatches
;   b) plot station time series of observed and modelled CH4 concentrations
;      using plot_inv_timeseries.pro
;   c) calculate and plot concentration growth rates using plot_growthrates.pro
;
; CATEGORY:
;
;   MAIOLICA-II model output
;
; CALLING SEQUENCE:
;
;    inv_create_monthly_station_output,sim,prelim=prelim,prior=prior,append=append
;
; INPUTS:
;
;    sim (STRUCTURE)  : the simulation information structure
;                       see inv_configurations.pro
;
; KEYWORD PARAMETERS:
;
;    /prelim          : set this keyword to write output for first preliminary inversion
;    /prior           : set this keyword to write only a priori fields (can be used
;                       already before an inversion is run)
;    /append          : append posteriori fields from final or preliminary inversion
;                       to existing file previously generated e.g. with /prior keyword
;
; OUTPUTS:
;
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  writes out monthly netcdf files to directory sim.outdir
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
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
;   DB, 06 Jan 2018:  first implementation
;
;-

PRO inv_create_monthly_station_output,sim,prelim=prelim,prior=prior,append=append

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  yyyymm = get_yyyymm(sim)

  ;; Create data arrays
  nst      = n_elements(sim.stats)
  ntot     = n_elements(yyyymm)
  ch4obs   = Fltarr(ntot,nst)+!values.f_nan
  ch4apri  = Fltarr(ntot,nst)+!values.f_nan
  ch4apost = Fltarr(ntot,nst)+!values.f_nan
  ch4apri_cat = Fltarr(ntot,sim.ntrace,nst)+!values.f_nan
  ch4apost_cat = Fltarr(ntot,sim.ntrace,nst)+!values.f_nan
  ndata    = IntArr(ntot,nst)
 
  ;; loop over the years and months and get observation and model data
  FOR m = 0,ntot-1 DO BEGIN
     read_obsmod_netcdf_month,sim,yyyymm[m],ch4obs=ch4tmp,ch4mod=ch4mod
     IF NOT keyword_set(prior) THEN $
        inv_calculate_posterior_tseries,sim,yyyymm[m],ch4obs=ch4tmp,ch4apri=ch4mod,$
                                        sa=sa,sp=sp,fcorr=fcorr,ch4post=ch4post,prelim=prelim

     ;; loop over the stations and assign values
     FOR i=0,nst-1 DO BEGIN
        index = WHERE(ch4tmp.name EQ sim.stats[i],cnt)
        IF cnt GT 1 THEN BEGIN
           ch4obs[m,i]=mean(ch4tmp[index].ch4,/nan)
           ch4apri[m,i]=mean(ch4mod[index].ch4,/nan)
           IF NOT keyword_set(prior) THEN $
              ch4apost[m,i]=mean(ch4post[index].ch4,/nan)
           ;; total per category
           FOR k=0,sim.ntrace-1 DO BEGIN
              catind = indgen(sim.nage)*sim.ntrace+k
              ch4apri_cat[m,k,i]=mean(total(ch4mod[index].ch4trace[catind],1),/nan)
              IF NOT keyword_set(prior) THEN $
                 ch4apost_cat[m,k,i]=mean(total(ch4post[index].ch4trace[catind],1),/nan)
           ENDFOR
        ENDIF ELSE IF cnt EQ 1 THEN BEGIN
           ch4obs[m,i]=ch4tmp[index].ch4
           ch4apri[m,i]=ch4mod[index].ch4
           IF NOT keyword_set(prior) THEN $
              ch4apost[m,i]=ch4post[index].ch4
           FOR k=0,sim.ntrace-1 DO BEGIN
              catind = indgen(sim.nage)*sim.ntrace+k
              ch4apri_cat[m,k,i]=total(ch4mod[index].ch4trace[catind])
              IF NOT keyword_set(prior) THEN $
                 ch4apost_cat[m,k,i]=total(ch4post[index].ch4trace[catind])
           ENDFOR
        ENDIF
        ndata[m,i]=cnt
     ENDFOR
  ENDFOR
 
  ;;*************************************************************************************
  ;; write fields to netcdf file
  ;;*************************************************************************************

  ;; metadata for global file attributes
  IF NOT keyword_set(sim.dlr) THEN BEGIN
     model = 'FLEXPART-CTM' 
     institute = 'EMPA'
     modeller = 'Stephan Henne, Florian Arfeuille, Dominik Brunner'
  ENDIF ELSE BEGIN
     model = 'EMAC'
     institute = 'DLR'
     modeller = 'Franziska Frank, Patrick Joeckel'
  ENDELSE
  metadata = create_struct($
             "description" , "Monthly mean CH4 observations and model prior and posterior concentrations", $
             "model", model, $
             "institute", institute, $
             "modeller", modeller, $
             "references"  , " ", $
             "creator"     , "Dominik Brunner", $
             "email"       , "dominik.brunner@empa.ch", $
             "affiliation" , "Empa Duebendorf, Switzerland", $
             "version"     , '1.0', $
             "date"        ,  systime(/UTC), $
             "study"       , "MAIOLICA-2" )

  sstr =sim_filename_str(sim,prelim=prelim)

  ncfile = sim.outdir + 'inv_station_output_'+sstr+sim.syyyymm+'-'+sim.eyyyymm+'.nc'

  IF file_test(ncfile) THEN BEGIN
     IF NOT keyword_set(append) THEN BEGIN
        print,'replacing file ',ncfile
        file_delete, ncfile,/quiet
     ENDIF ELSE BEGIN
        print,'adding posterior fields to file ',ncfile
        ncid = NCDF_OPEN(ncfile,/write)
     ENDELSE
  ENDIF ELSE print,'writing file ',ncfile


  IF NOT keyword_set(append) THEN BEGIN
     ncid = NCDF_CREATE(ncfile,/CLOBBER,/NETCDF4_FORMAT)
     NCDF_CONTROL, ncid, /FILL

     ;;******************************************
     ;; global attributes
     ;;******************************************
     metatags = tag_names(metadata)
     FOR i = 0, n_elements(metatags)-1 DO BEGIN
        NCDF_ATTPUT, ncid, metatags[i], metadata.(i), /GLOBAL
     ENDFOR
     
     ;;******************************************
     ;; create dimensions
     ;;******************************************
     
     tid = NCDF_DIMDEF(ncid,'time',/UNLIMITED)
     tracid  = NCDF_DIMDEF(ncid,'tracer', sim.ntrace)
     statid = NCDF_DIMDEF(ncid,'stations', nst)
     len2id = NCDF_DIMDEF(ncid,'len2', 2)
     len3id = NCDF_DIMDEF(ncid,'len3', 3)
     len4id = NCDF_DIMDEF(ncid,'len4', 4)
     len13id = NCDF_DIMDEF(ncid,'len13', 13)
     
     ;;******************************************
     ;; create variables
     ;;******************************************
     yearid = NCDF_VARDEF(ncid,"year", [len4id,tid],/CHAR)
     monthid = NCDF_VARDEF(ncid,"month", [len2id,tid],/CHAR)
     catid  = NCDF_VARDEF(ncid,'source_cat', [len13id,tracid], /CHAR)
     sid  = NCDF_VARDEF(ncid,'station', [len3id,statid], /CHAR)
     
     units = 'ppb'
     missing_value = !values.f_nan
     
     ch4obsid = NCDF_VARDEF(ncid,'CH4_obs',[tid,statid],/FLOAT)
     NCDF_ATTPUT, ncid, ch4obsid,'long_name', 'monthly mean observed CH4'
     NCDF_ATTPUT, ncid, ch4obsid, 'missing_value', missing_value
     NCDF_ATTPUT, ncid, ch4obsid, 'units',units
     
     ndatid = NCDF_VARDEF(ncid,'num_data',[tid,statid],/SHORT)
     NCDF_ATTPUT, ncid, ndatid,'long_name', 'number of data points per month'
     NCDF_ATTPUT, ncid, ndatid, 'units','-'
     
     ch4apriid = NCDF_VARDEF(ncid,'CH4_apri',[tid,statid],/FLOAT)
     NCDF_ATTPUT, ncid, ch4apriid, 'long_name', 'monthly mean a priori CH4'
     NCDF_ATTPUT, ncid, ch4apriid, 'missing_value', missing_value
     NCDF_ATTPUT, ncid, ch4apriid, 'units',units
     
     ch4apostid = NCDF_VARDEF(ncid,'CH4_apost',[tid,statid],/FLOAT)
     NCDF_ATTPUT, ncid, ch4apostid, 'long_name', 'monthly mean a posteriori CH4'
     NCDF_ATTPUT, ncid, ch4apostid, 'missing_value', missing_value
     NCDF_ATTPUT, ncid, ch4apostid, '_FillValue', missing_value
     NCDF_ATTPUT, ncid, ch4apostid, 'units',units
     
     ch4apricatid = NCDF_VARDEF(ncid,'CH4_apri_tracer',[tid,tracid,statid],/FLOAT)
     NCDF_ATTPUT, ncid, ch4apricatid, 'long_name', 'monthly mean a priori CH4 per tracer'
     NCDF_ATTPUT, ncid, ch4apricatid, 'missing_value', missing_value
     NCDF_ATTPUT, ncid, ch4apriid, 'units',units
     
     ch4apostcatid = NCDF_VARDEF(ncid,'CH4_apost_tracer',[tid,tracid,statid],/FLOAT)
     NCDF_ATTPUT, ncid, ch4apostcatid, 'long_name', 'monthly mean a posteriori CH4 per tracer'
     NCDF_ATTPUT, ncid, ch4apostcatid, 'missing_value', missing_value
     NCDF_ATTPUT, ncid, ch4apostcatid, '_FillValue', missing_value
     NCDF_ATTPUT, ncid, ch4apostcatid, 'units',units
     
     NCDF_CONTROL, ncid, /ENDEF
     
     ;;******************************************
     ;; fill with data
     ;;******************************************
     yyyy = STRMID(yyyymm,0,4) & mm = STRMID(yyyymm,4,2)
     NCDF_VARPUT, ncid, yearid, yyyy
     NCDF_VARPUT, ncid, monthid, mm
     cats = emiss_categories()
     NCDF_VARPUT, ncid, catid, STRING(cats,format='(a13)')
     NCDF_VARPUT, ncid, sid, STRING(sim.stats,format='(a3)')
 
     NCDF_VARPUT, ncid, ch4obsid, ch4obs
     NCDF_VARPUT, ncid, ndatid, ndata
     NCDF_VARPUT, ncid, ch4apriid, ch4apri
     NCDF_VARPUT, ncid, ch4apricatid, ch4apri_cat
     
  ENDIF

  IF NOT keyword_set(prior) THEN BEGIN
     IF keyword_set(append) THEN BEGIN
        ch4apostid = NCDF_VARID(ncid,'CH4_apost')
        ch4apostcatid = NCDF_VARID(ncid,'CH4_apost_tracer')
     ENDIF
     NCDF_VARPUT, ncid, ch4apostid, ch4apost
     NCDF_VARPUT, ncid, ch4apostcatid, ch4apost_cat
  ENDIF

  NCDF_CLOSE,ncid

END
