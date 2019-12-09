;+
; NAME:
;	close_ps
;
; PURPOSE:
;	closes a postcript-file opened e.g. by open_ps
;	Sets the graphic output back to the screen
; CATEGORY:
;	Input/Output
;
; CALLING SEQUENCE:
;	close_ps
; EXAMPLE:
;
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;	none
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;	none
; PROCEDURE:
;
; MODIFICATION HISTORY:
; (c) by David N. Bresch, 21.12.92
;	revision	950914
;-

PRO close_ps

device,/close
IF !version.os_family EQ 'Windows' THEN BEGIN
   set_plot,'Win'
ENDIF ELSE BEGIN
   set_plot,'X'
ENDELSE
!p.font = -1

END ;of close_ps
