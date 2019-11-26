PRO compare_old_new_errcov,sim,prelim=prelim

  yyyymm = get_yyyymm(sim)

  nm = n_elements(yyyymm)
  nst = n_elements(sim.stats)
  old = FltArr(nm,nst)+!values.f_nan
  new = FltArr(nm,nst)+!values.f_nan


  FOR i=0,nm-1 DO BEGIN
     read_errcov_month,sim,yyyymm[i],errcov=errcov,stats=stats,prelim=prelim
     errcovnew=read_model_data_mismatch_netcdf(sim,prelim=prelim,yyyymm=yyyymm[i],$
                                               stats=stats)
     FOR k=0,nst-1 DO BEGIN
        index = WHERE(sim.stats[k] EQ stats,cnt)
        IF cnt GT 0 THEN BEGIN
           old[i,k]=mean(sqrt(errcov[index]),/nan)
           new[i,k]=mean(sqrt(errcovnew[index]),/nan)
        ENDIF
     ENDFOR
  ENDFOR

  gvt = dtg2gvtime(yyyymm+'01')
  FOR k=0,nst-1 DO BEGIN
     plot_tseries,gvt,old[*,k],yrange=[0,100],title=sim.stats[k],charsize=1.5
     plot_tseries,gvt,new[*,k],/over,color=24
     wait,0.2
  ENDFOR

END
