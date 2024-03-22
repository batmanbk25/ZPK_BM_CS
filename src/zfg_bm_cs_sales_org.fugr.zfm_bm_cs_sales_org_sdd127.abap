FUNCTION ZFM_BM_CS_SALES_ORG_SDD127.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD127
*"----------------------------------------------------------------------
DATA:
        LSS_T6B1  TYPE T6B1.
  GS_DATASD127 = I_DATA.
  SELECT SINGLE *
    FROM T6B1
    INTO CORRESPONDING FIELDS OF GT_SD127
    WHERE BOART = GS_DATASD127-BOART.
  IF  SY-SUBRC IS INITIAL.
    GS_SD127-BOART = GS_DATASD127-BOART.
    GS_SD127-KOBOG = GS_DATASD127-KOBOG.
    GS_SD127-KAPPL = 'V'.
    MOVE-CORRESPONDING GS_SD127 TO LSS_T6B1.
    UPDATE T6B1 FROM LSS_T6B1.
  ELSE.
    GS_SD127-BOART = GS_DATASD127-BOART.
    GS_SD127-KOBOG = GS_DATASD127-KOBOG.
    GS_SD127-KAPPL = 'V'.
    MOVE-CORRESPONDING GS_SD127 TO LSS_T6B1.
    INSERT T6B1 FROM LSS_T6B1.
  ENDIF.
ENDFUNCTION.