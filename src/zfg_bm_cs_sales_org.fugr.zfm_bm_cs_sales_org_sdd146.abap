FUNCTION ZFM_BM_CS_SALES_ORG_SDD146.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD146
*"----------------------------------------------------------------------
  DATA:
         LSS_TVAP TYPE TVAP.
  GS_DATASD146 = I_DATA.
  SELECT SINGLE *
    FROM TVAP
    INTO CORRESPONDING FIELDS OF GT_SD146
    WHERE PSTYV = GS_DATASD146-PSTYV.
  IF  SY-SUBRC IS INITIAL.
    GS_SD146-PSTYV = GS_DATASD146-PSTYV.
    GS_SD146-CHAUT = GS_DATASD146-CHAUT.
    MOVE-CORRESPONDING GS_SD146 TO LSS_TVAP.
    UPDATE TVAP FROM LSS_TVAP.
  ELSE.
    GS_SD146-PSTYV = GS_DATASD146-PSTYV.
    GS_SD146-CHAUT = GS_DATASD146-CHAUT.
    MOVE-CORRESPONDING GS_SD146 TO LSS_TVAP.
    INSERT TVAP FROM LSS_TVAP.

  ENDIF.
ENDFUNCTION.
