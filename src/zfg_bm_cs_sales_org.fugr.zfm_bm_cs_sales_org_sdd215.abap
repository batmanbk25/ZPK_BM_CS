FUNCTION ZFM_BM_CS_SALES_ORG_SDD215.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD215
*"----------------------------------------------------------------------
  DATA:
           LSS_TVAP TYPE TVAP.
  GS_DATASD215 = I_DATA.
  SELECT SINGLE *
    FROM TVAP
    INTO CORRESPONDING FIELDS OF GT_SD215
    WHERE PSTYV = GS_DATASD215-PSTYV.
  IF  SY-SUBRC IS INITIAL.
    GS_SD215-PSTYV = GS_DATASD215-PSTYV.
    GS_SD215-STGAP = GS_DATASD215-STGAP.
    MOVE-CORRESPONDING GS_SD215 TO LSS_TVAP.
    UPDATE TVAP FROM LSS_TVAP.
  ELSE.
    GS_SD215-PSTYV = GS_DATASD215-PSTYV.
    GS_SD215-STGAP = GS_DATASD215-STGAP.
    MOVE-CORRESPONDING GS_SD215 TO LSS_TVAP.
    INSERT TVAP FROM LSS_TVAP.
  ENDIF.
ENDFUNCTION.
