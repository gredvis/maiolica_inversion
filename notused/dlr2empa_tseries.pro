;+
; NAME:
;
;   DLR2EMPA_TSERIES
;
; PURPOSE:
;
;   Convert station output generated by DLR with the EMAC model for the MAIOLICA project 
;   into the "Empa format", i.e. one ascii file per station with all tracers in separate columns
;
; CATEGORY:
;
;   MAIOLICA-2, FLEXPART, inverse modelling
;
; CALLING SEQUENCE:
;
;   DLR2EMPA_TSERIES,statid,dlrdir=dlrdir,empadir=empadir
;
; INPUTS:
;
;   statid : The station ID
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
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
;   DB, 20 May 2016:  first implementation
;
;-

;----------------- subroutine for reading in a DLR receptor output file ----------------
PRO read_dlr_ncdf,file,yyyymm,data=data,totch4=totch4,gvt=gvt

  IF file_test(file) EQ 0 THEN BEGIN
     message,'file '+file+' does not exist',/continue
     RETURN
  ENDIF

  ntracer = 48
  nageclass = 5

  tracers = STRING(indgen(ntracer)+1,format='(i2.2)')
  ageclasses =  STRING(indgen(nageclass)+1,format='(i2.2)')
  prefix = 'tracer_gp_CH4_fx_e'
  
  ;; open netcdf file and read in receptor output
  ncid = ncdf_open(file)

  ;; read total CH4 tracer
  varid = ncdf_varid(ncid,'tracer_gp_CH4_12C_ave')
  ncdf_varget,ncid,varid,tmp
  varid = ncdf_varid(ncid,'tracer_gp_CH4_13C_ave')
  ncdf_varget,ncid,varid,totch4
  totch4=(totch4+tmp)*1e9

  nday = n_elements(totch4)
  
  ;; create tracer arrays
  data = FltArr(ntracer,nageclass,nday)
  
  FOR i=0,ntracer-1 DO BEGIN
     FOR j=0,nageclass-1 DO BEGIN
        varid = ncdf_varid(ncid,prefix+tracers[i]+'_a'+ageclasses[j]+'_ave')
        ncdf_varget,ncid,varid,tracer
        data[i,j,*]=tracer*1e9
     ENDFOR
  ENDFOR

  ncdf_close,ncid

  ;; create date/time values
  dd = STRING(indgen(n_elements(totch4)),format='(i2.2)')
  dtg = yyyymm+dd
  gvt = dtg2gvtime(dtg)
          
END

;;----------------------------------------- main ---------------------------------------

PRO dlr2empa_tseries,statid,dlrdir=dlrdir,empadir=empadir,allgvt=allgvt,allch4=allch4

  ;; check for inputs
  IF n_elements(statid) EQ 0 THEN BEGIN
     message,'parameter statid is missing',/continue
     RETURN
  ENDIF


  IF n_elements(dlrdir) EQ 0 THEN BEGIN
     dlrdir = '/nas/arf/INVERSION/DLR/empa/'

  ENDIF

  IF n_elements(empadir) EQ 0 THEN BEGIN
     empadir = '/remote7/dominik/MAIOLICA/DLR/'
  ENDIF


  ;; check if station data exist
  statdirs = file_search(dlrdir+'scout_*',/test_directory)
  stats = STRMID(statdirs,STRLEN(statdirs[0])-3,3)
  
  index = WHERE(statid EQ stats,cnt)
  IF cnt EQ 0 THEN BEGIN
     message,'station '+statid+' not found',/continue
     RETURN
  ENDIF

  ;; loop over all files between 199001 and 201212 and read and write out to ASCII
  years = STRING(1990+indgen(23),format='(i4)')
  months = STRING(indgen(12)+1,format='(i2.2)')

 ; outfile = empadir+statid+'_dlr.dat'
 ; openw,lun,outfile,/get_lun
  

  first = 1
  
  FOR y = 0,n_elements(years)-1 DO BEGIN
     print,'processing year ',years[y]
     FOR m = 0,n_elements(months)-1 DO BEGIN
        yyyymm = years[y]+months[m]
        file = file_search(dlrdir+'scout_'+STRUPCASE(statid)+'/*'+yyyymm+'*')
        IF file EQ '' THEN BEGIN
           print,'skipping file ',file,' because not found'
           GOTO,nextdate
        ENDIF

        ;; read DLR netcdf file
        read_dlr_ncdf,file,yyyymm,data=data,totch4=totch4,gvt=gvt

        IF first EQ 1 THEN BEGIN
           allgvt = gvt
           allch4 = totch4
           first = 0
        ENDIF ELSE BEGIN
           allgvt = [allgvt,gvt]
           allch4 = [allch4,totch4]
        ENDELSE

        ;; write out to ASCII file
        nextdate:
     ENDFOR
  ENDFOR

  ;; close the output file
  ;;free_lun,lun

END