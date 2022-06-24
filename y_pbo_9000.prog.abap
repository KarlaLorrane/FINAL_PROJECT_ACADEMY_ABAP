*----------------------------------------------------------------------*
***INCLUDE Y_PBO_9000.
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  DATA: lv_str TYPE string.

  PERFORM f_build_screen_9000.

  CASE 'X'.
    WHEN p_create. lv_str = 'Create'.
    WHEN p_modify. lv_str = 'Modify'.
    WHEN p_delete. lv_str = 'Delete'.
    WHEN p_del_it. lv_str = 'Delete Item'.
    WHEN p_view.   lv_str = 'View'.
  ENDCASE.

  SET PF-STATUS 'S9000'.
  SET TITLEBAR  'T9000' WITH lv_str.

ENDMODULE.
