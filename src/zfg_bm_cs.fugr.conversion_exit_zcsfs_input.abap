FUNCTION CONVERSION_EXIT_ZCSFS_INPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ANY
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ANY
*"----------------------------------------------------------------------

  CALL FUNCTION 'CONVERSION_EXIT_ZZALL_INPUT'
    EXPORTING
      INPUT     = INPUT
      I_DOMNAME = 'ZDO_BM_CS_CSFSTS'
    IMPORTING
      OUTPUT    = OUTPUT.

ENDFUNCTION.
