FUNCTION ZFM_BM_CS_SALES_ORG_SDD197.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD197
*"----------------------------------------------------------------------
DATA:
         LSS_TVFT TYPE TVFT.
  GS_DATASD197 = I_DATA.
  SELECT SINGLE *
    FROM TVFT
    INTO CORRESPONDING FIELDS OF GT_SD197
    WHERE FKPTY = GS_DATASD197-FKPTY.
  IF  SY-SUBRC IS INITIAL.
    GS_SD197-FKPTY = GS_DATASD197-FKPTY.
    GS_SD197-FCGRP = GS_DATASD197-FCGRP.
    MOVE-CORRESPONDING GS_SD197 TO LSS_TVFT.
    UPDATE TVFT FROM LSS_TVFT.
  ELSE.
    GS_SD197-FKPTY = GS_DATASD197-FKPTY.
    GS_SD197-FCGRP = GS_DATASD197-FCGRP.
    MOVE-CORRESPONDING GS_SD197 TO LSS_TVFT.
    INSERT TVFT FROM LSS_TVFT.
  ENDIF.
ENDFUNCTION.
