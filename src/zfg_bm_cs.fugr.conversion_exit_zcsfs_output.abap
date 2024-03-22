FUNCTION CONVERSION_EXIT_ZCSFS_OUTPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ANY
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ANY
*"----------------------------------------------------------------------

  CALL FUNCTION 'CONVERSION_EXIT_ZZALL_OUTPUT'
    EXPORTING
      INPUT     = INPUT
      I_DOMNAME = 'ZDO_BM_CS_CSFSTS'
    IMPORTING
      OUTPUT    = OUTPUT.

ENDFUNCTION.
