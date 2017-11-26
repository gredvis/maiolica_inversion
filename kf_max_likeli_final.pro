FUNCTION kf_max_likeli_final,M,Minv,v,zlen=zlen,nobse=nobse
   ; calculate new term for maximum likelihood function
   ; = -0.5 * (ln(det M) + transpose(v) ## Minv ## v)
;stop
   IF n_elements(M) EQ 1 THEN logdetM=alog(M) $
   ELSE BEGIN
      ;; first we calculate Choleski factorisation of matrix M
      la_choldc,M,status=status ; M will be replaced by its lower triangular
   
      n=n_elements(v)
      logdetM=0D
      FOR i=0,n-1 DO logdetM=logdetM+alog(M[i,i])
      logdetM = 2*logdetM
   ENDELSE

   zlen2 = transpose(v) # Minv # v
   zlen = zlen2/n
   nobse=n
 ;  zi=logdetM^(-0.5) # v
 ;  qi=zi # transpose(zi)
 ;  print,qi/n
;   print,zlen

;stop

   RETURN, -0.5 * ( logdetM + zlen2 )



END
