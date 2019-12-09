FUNCTION PlotSymbol,number,thick=thick
;+
; This method calculates a user-defined symbol for the PSYM keyword.
;       number - The plot symbol value. By default, 18. Possible values are:
;         0 - Dot
;         1 - Filled Circle
;         2 - Filled Upward Triangle
;         3 - Filled Downward Triangle
;         4 - Filled Diamond
;         5 - Filled Square
;         6 - Open Circle
;         7 - Open Upward Triangle
;         8 - Open Downward Triangle
;         9 - Open Diamond
;        10 - Open Square
;        11 - Plus Sign
;        12 - X
;        13 - Star
;        14 - Filed Rightfacing Triangle
;        15 - Filled Leftfacing Triangle
;        16 - Open Rightfacing Triangle
;        17 - Open Leftfacing Triangle
;        18 - No Symbol (the default).
;-

IF N_Elements(number) EQ 0 THEN RETURN, 18
IF (number LT 0) OR (number GT 18) THEN BEGIN
    Message, /NoName, 'Symbol number out of defined range. Returning NO SYMBOL.'
    RETURN,18
ENDIF

   ; Use user defined symbol by default.

result = 8

CASE number OF
   0  : result = 3              ; Dot
   1  : BEGIN
      phi = Findgen(32) * (!PI * 2 / 32.)
      phi = [ phi, phi(0) ]
      UserSym, Cos(phi), Sin(phi), /Fill ; Filled circle.
   END
   2  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ], /Fill        ; Filled upward triangle.
   3  : UserSym, [ -1, 0, 1, -1 ], [  1, -1, 1, 1 ], /Fill         ; Filled downward triangle.
   4  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ], /Fill     ; Filled diamond.
   5  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ], /Fill  ; Filled square.
   6  : BEGIN
      phi = Findgen(32) * (!PI * 2 / 32.)
      phi = [ phi, phi(0) ]
      UserSym, cos(phi), sin(phi),thick=thick ; Open circle.
   END
   7  : UserSym, [ -1, 0, 1, -1 ], [ -1, 1, -1, -1 ],tnick=thick         ; Open upward triangle.
   8  : UserSym, [ -1, 0, 1, -1 ], [  1, -1, 1, 1 ],tnick=thick          ; Open downward triangle.
   9  : UserSym, [ 0, 1, 0, -1, 0 ], [ 1, 0, -1, 0, 1 ],tnick=thick      ; Open diamond.
   10  : UserSym, [ -1, 1, 1, -1, -1 ], [ 1, 1, -1, -1, 1 ],thick=thick  ; Open square.
   11  : result = 1                                                      ; Plus.
   12  : result = 7                                                      ; X.
   13  : result = 2                                                      ; Star.
   14  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ], /Fill               ; Filled rightfacing triangle.
   15  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ], /Fill                 ; Filled leftfacing triangle.
   16  : UserSym, [ -1, 1, -1, -1 ], [1, 0, -1, 1 ],tnick=thick          ; Open rightfacing triangle.
   17  : UserSym, [ 1, -1, 1, 1 ], [1, 0, -1, 1 ]                        ; Open leftfacing triangle.
   18  : result  = 0                                                     ; No symbol at all.
   
ENDCASE

RETURN, result
END ;-----------------------------------------------------------------------------------------------------------------------------
