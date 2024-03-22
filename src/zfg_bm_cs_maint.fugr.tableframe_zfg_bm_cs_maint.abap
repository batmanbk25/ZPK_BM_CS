*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFG_BM_CS_MAINT
*   generation date: 21.10.2022 at 18:00:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFG_BM_CS_MAINT    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
