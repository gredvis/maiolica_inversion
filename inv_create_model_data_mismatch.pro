;+
; NAME:
;
;   inv_create_model_data_mismatch
;
; PURPOSE:
;
;   Calculate monthly diagonal error covariance matrix representing the sum of measurement 
;   and transport errors and write out to a netcdf file.
;
;   One way of estimating the uncertainty would be:
;    sigma =
;      sigma(meas)               ( = sqrt(inst precision^2 )
;    + sigma(transport error)    ( = sqrt(mean of the 3-day SDs) )
;    + sigma(sampling frequency) ( = sqrt(var(CH4mod)/nmod) )
;    + sigma(intercalibration)   ( = 0, because all data have been scaled to NOAA04 scale)
;
;   However, the method chosen here estimates the uncertainty from the differences
;   between observations and model simulations per year, with the mean annual bias
;   subtracted from the simulations. The routine, therefore, reads the weekly 
;   observational data (flask and continuous) and weekly mean a priori model data for 
;   the years of the inversion and computes the uncertainty statistics.
;
; CATEGORY:
;
;   inverse modelling, MAIOLICA-2. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;  inv_create_model_data_mismatch,sim,prelim=prelim
;
; INPUTS:
;
;       sim :  the simulation structure
;
; KEYWORD PARAMETERS:
;
;       /prelim : if set, the model-data mismatches for the first preliminary inversion
;                 are calculated from the prior simulation. If not set, the
;                 model-data mismatches are calculated from the posterior output of
;                 a previous preliminary simulation.
;
; OUTPUTS:
;
;       wri
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
;     Read all available data from continuous/flask stations and model
;     data per month
;     Output weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 22 September 2011
; 
; Dominik Brunner, 7 Feb 2017: deleted all subroutines, because they
;       are idential to other sn
; DB, 11 Feb 2017: all subroutines deleted and routine restructured
;                  and simplified in a major way
;-

;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO inv_create_model_data_mismatch,sim,prelim=prelim,plot=plot

  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  ;; create data arrays
  yyyymm = get_yyyymm(sim)
  nm = n_elements(yyyymm)
  ntot = nm*8                   ; assuming that there are max 10 obs points per month
  nst      = n_elements(sim.stats)

  ;; arrays to hold all observations and model posterior fields (or prior if keyword /prelim is set)
  ch4obs    = FltArr(ntot,nst)+!values.f_nan
  smobs    = FltArr(ntot,nst)+!values.f_nan
  ch4devobs = FltArr(ntot,nst)+!values.f_nan
  ch4gvt    = DblArr(ntot,nst)
  ch4mod    = FltArr(ntot,nst)+!values.f_nan
  smmod    = FltArr(ntot,nst)+!values.f_nan
  ch4devmod = FltArr(ntot,nst)+!values.f_nan
  count     = LonArr(nst)

  ;; fill in arrays
  FOR i = 0,nm-1 DO BEGIN
     read_obsmod_netcdf_month,sim,yyyymm[i],ch4obs=ch4otmp,ch4mod=ch4mtmp
     IF NOT keyword_set(prelim) THEN BEGIN
        inv_calculate_posterior_tseries,sim,yyyymm[i],ch4obs=ch4otmp,$
                                        ch4apri=ch4mtmp,/prelim,$
                                        sa=sa,sp=sp,fcorr=fcorr,ch4post=ch4tmp
        ;; replace prior CH4 by posterior CH4 values
        ch4mtmp.ch4 = ch4tmp.ch4
     ENDIF
     FOR k=0,nst-1 DO BEGIN
        index = WHERE(ch4otmp.name EQ sim.stats[k] AND FINITE(ch4otmp.ch4),cnt)
        IF cnt GT 0 THEN BEGIN
           ch4obs[count[k]:count[k]+cnt-1L,k]=ch4otmp[index].ch4
           ch4gvt[count[k]:count[k]+cnt-1L,k]=dtg2gvtime(ch4otmp[index].dtg)
           ch4mod[count[k]:count[k]+cnt-1L,k]=ch4mtmp[index].ch4
           count[k]=count[k]+cnt
        ENDIF

     ENDFOR
  ENDFOR

  ;; read in monthly mean fields and compute a smoothed field
  read_inv_station_output_netcdf,sim,/prelim,yyyymm=oyyyymm,prior=keyword_set(prelim),$
                                 ok=ok,ch4obs=ch4mmobs,ch4apri=ch4mmapri,ch4post=ch4mmpost,$
                                 stats=stats
 
  IF NOT ok THEN return

  IF total(stats EQ sim.stats) NE nst THEN BEGIN
     print,'incompatible station list'
     RETURN
  ENDIF
  IF total(oyyyymm EQ yyyymm) NE nm THEN BEGIN
     print,'incompatible list of months'
     RETURN
  ENDIF

  width = 7                     ; smoothing width of 6 months
  tind = findgen(nm)

  FOR k=0,nst-1 DO BEGIN
     smoothobs = smooth(ch4mmobs[*,k],width,/nan,/edge_truncate)
     IF keyword_set(prelim) THEN BEGIN
        smoothmod = smooth(ch4mmapri[*,k],width,/nan,/edge_truncate)
     ENDIF ELSE BEGIN
        smoothmod = smooth(ch4mmpost[*,k],width,/nan,/edge_truncate)
     ENDELSE
     smoothgvt = dtg2gvtime(oyyyymm+'15')
     
     ;; compute deviations of observation data from smooth fields
     xint = (interpol(tind,smoothgvt,ch4gvt[0:count[k]-1,k])>0)<(nm-1)
     smobs[0:count[k]-1,k]=interpolate(smoothobs,xint)
     smmod[0:count[k]-1,k]=interpolate(smoothmod,xint)
     ch4devobs[0:count[k]-1,k]=ch4obs[0:count[k]-1,k]-smobs[0:count[k]-1,k]
     ch4devmod[0:count[k]-1,k]=ch4mod[0:count[k]-1,k]-smmod[0:count[k]-1,k]

     IF keyword_set(plot) THEN BEGIN
        position=[0.1,0.6,0.95,0.95]
        plot_tseries,ch4gvt[0:count[k]-1,k],ch4obs[0:count[k]-1,k],title=stats[k],/yst,position=position
        plot_tseries,ch4gvt[0:count[k]-1,k],smobs[0:count[k]-1,k],color=24,/over

        position=[0.1,0.1,0.95,0.45]
        plot_tseries,ch4gvt[0:count[k]-1,k],ch4mod[0:count[k]-1,k],title=stats[k],/yst,position=position,/noerase
        plot_tseries,ch4gvt[0:count[k]-1,k],smmod[0:count[k]-1,k],color=24,/over
        wait,0.2
     ENDIF
  ENDFOR

  ;; compute monthly standard deviations of differences between deviations from smooth
  ;; curves of observations and model values
  mismatch = FltArr(nm,nst)+!values.f_nan
  sm_mismatch = FltArr(nm,nst)+!values.f_nan

  FOR i=0,nm-1 DO BEGIN
     nyyyymm=gvtime2dtg(dtg2gvtime(yyyymm[i]+'01')+40D)
     sgvt = dtg2gvtime(yyyymm[i]+'01')
     egvt = dtg2gvtime(nyyyymm+'01')
     FOR k=0,nst-1 DO BEGIN
        index = WHERE(ch4gvt[*,k] GE sgvt AND ch4gvt[*,k] LE egvt,cnt)
        IF cnt GT 1 THEN BEGIN
           mismatch[i,k]=stddev(ch4devobs[index,k]-ch4devmod[index,k])
        ENDIF ELSE IF cnt EQ 1 THEN BEGIN
           mismatch[i,k]=sqrt((ch4devobs[index[0],k]-ch4devmod[index[0],k])^2)
        ENDIF
     ENDFOR
  ENDFOR

  ;; finally compute smoothed standard deviations, scaled by uncertainty amplification
  ;; factor
  width = 7
  FOR k=0,nst-1 DO BEGIN
     sm_mismatch[*,k] = smooth(mismatch[*,k],width,/nan,/edge_truncate)*sim.ufact[k]
     ;; lower limit uncertainty is 0.5*mean, upper limit is 2*mean
     smmean = mean(sm_mismatch[*,k],/nan)
     index = WHERE(sm_mismatch[*,k] LT 0.5*smmean,cnt)
     IF cnt GT 0 THEN sm_mismatch[index,k]=0.5*smmean
     index = WHERE(sm_mismatch[*,k] GT 2*smmean,cnt)
     IF cnt GT 0 THEN sm_mismatch[index,k]=2*smmean
     index = WHERE(finite(sm_mismatch[*,k]) EQ 0,cnt)
     ;; set to mean where values are not defined (and not observations are available)
     IF cnt GT 0 THEN sm_mismatch[index,k]=smmean
     IF keyword_set(plot) THEN BEGIN
        plot_tseries,dtg2gvtime(yyyymm+'01'),sm_mismatch[*,k],title=stats[k],yrange=[0,50]
        wait,0.2
     ENDIF
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
             "description" , "Monthly model-data mismatch uncertainties (1 sigma)", $
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

  ncfile = sim.outdir + 'inv_model_data_mismatch_'+sstr+sim.syyyymm+'-'+sim.eyyyymm+'.nc'

  IF file_test(ncfile) THEN BEGIN
     print,'replacing file ',ncfile
     file_delete, ncfile,/quiet
  ENDIF ELSE print,'writing file ',ncfile
  
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
  statid = NCDF_DIMDEF(ncid,'stations', nst)
  len2id = NCDF_DIMDEF(ncid,'len2', 2)
  len3id = NCDF_DIMDEF(ncid,'len3', 3)
  len4id = NCDF_DIMDEF(ncid,'len4', 4)
     
  ;;******************************************
  ;; create variables
  ;;******************************************
  yearid = NCDF_VARDEF(ncid,"year", [len4id,tid],/CHAR)
  monthid = NCDF_VARDEF(ncid,"month", [len2id,tid],/CHAR)
  sid  = NCDF_VARDEF(ncid,'station', [len3id,statid], /CHAR)
  
  units = 'ppb'
  missing_value = !values.f_nan
  
  varid = NCDF_VARDEF(ncid,'sigma_CH4',[tid,statid],/FLOAT)
  NCDF_ATTPUT, ncid, varid,'long_name', 'Monthly model-data mismatch smoothed with 6-month smoothing window'
  NCDF_ATTPUT, ncid, varid, 'missing_value', missing_value
  NCDF_ATTPUT, ncid, varid, 'units',units
  
  NCDF_CONTROL, ncid, /ENDEF
     
  ;;******************************************
  ;; fill with data
  ;;******************************************
  yyyy = STRMID(yyyymm,0,4) & mm = STRMID(yyyymm,4,2)
  NCDF_VARPUT, ncid, yearid, yyyy
  NCDF_VARPUT, ncid, monthid, mm
  NCDF_VARPUT, ncid, sid, STRING(sim.stats,format='(a3)')
  
  NCDF_VARPUT, ncid, varid, sm_mismatch
 
  NCDF_CLOSE,ncid
  
END
