*&---------------------------------------------------------------------*
*& Report  zcoy_dyna_function
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zcoy_dyna_function LINE-SIZE 130.

TYPES:BEGIN OF ts_func_param,
        value TYPE string,
        name  TYPE abap_parmname,
      END OF ts_func_param,
      tt_func_param TYPE STANDARD TABLE OF ts_func_param.

CLASS lcx_dyna_func DEFINITION INHERITING FROM cx_static_check CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !text     TYPE string OPTIONAL
        !previous LIKE previous OPTIONAL .
    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA text TYPE string.
ENDCLASS.

CLASS lcx_dyna_func IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->text = text.
  ENDMETHOD.

  METHOD get_text.
    result = me->text.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  TRY.
      DATA lt_param TYPE abap_func_parmbind_tab.
      DATA lt_output TYPE tt_func_param.
      DATA ls_param TYPE abap_func_parmbind.
      DATA lt_excep TYPE abap_func_excpbind_tab.
      DATA lt_data TYPE tt_func_param.
      DATA lv_funcname TYPE rs38l_fnam .
      DATA lv_request TYPE string.

*      lv_funcname = 'TR_SYS_PARAMS'.
*      lv_request = `[{"name":"SYSTEMTYPE","value":null},{"name":"SYSTEMNAME","value":null},{"name":"SYSTEMEDIT","value":null}]`.
*      lv_funcname = 'FUNCTION_EXISTS'.
*      lv_request = `[{"name":"FUNCNAME","value":"\"TR_SYS_PARAMS\""},{"name":"NAMESPACE","value":null},{"name":"GROUP","value":null}]`.
*      lv_request = `[{"name":"DUMMY","value":""},{"name":"FUNCNAME","value":"\"TR_SYS_PARAMS\""},{"name":"NAMESPACE","value":null},{"name":"GROUP","value":null}]`.
*      lv_funcname = 'OIL_MONTH_GET_FIRST_LAST'.
*      lv_request = `[{"NAME":"I_MONTH","VALUE":"11"},{"NAME":"I_YEAR","VALUE":"2018"},{"NAME":"E_LAST_DAY","VALUE":null}]`.
*      lv_funcname = 'GET_CPU_ALL'.
*      lv_request = `[{"NAME":"LOCAL_REMOTE","VALUE":"\"LOCAL\""},{"NAME":"F_CPU_ALL_READ","VALUE":null},{"NAME":"TF_CPU_ALL","VALUE":null}]`.
      lv_funcname = 'MD_STOCK_REQUIREMENTS_LIST_API'.
      lv_request = `[{"NAME":"MATNR","VALUE":"\"000000000002000132\""},{"NAME":"WERKS","VALUE":"\"0781\""},{"NAME":"E_MT61D","VALUE":null},{"NAME":"MDPSX","VALUE":null}]`.

      /ui2/cl_json=>deserialize(
        EXPORTING
        json = lv_request
        CHANGING
        data = lt_data
      ).

      SELECT funcname, parameter, paramtype, structure FROM fupararef INTO TABLE @DATA(lt_fupararef) WHERE funcname = @lv_funcname.
      IF ( sy-subrc <> 0 ).
        RAISE EXCEPTION TYPE lcx_dyna_func
          EXPORTING
            text = |Function { lv_funcname } not found|.
      ENDIF.

      LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
        CLEAR ls_param.

        IF NOT line_exists( lt_fupararef[ parameter = <fs_data>-name ] ).
          RAISE EXCEPTION TYPE lcx_dyna_func
            EXPORTING
              text = |Parameter { <fs_data>-name } not existed|.
        ENDIF.

        DATA(lv_type) = lt_fupararef[ parameter = <fs_data>-name ]-structure.
        CREATE DATA ls_param-value TYPE (lv_type).

        DATA lv_kind TYPE i.
        CASE lt_fupararef[ parameter = <fs_data>-name ]-paramtype.
          WHEN 'I'.
            lv_kind = abap_func_exporting.
          WHEN 'E'.
            lv_kind = abap_func_importing.
          WHEN 'C'.
            lv_kind = abap_func_changing.
          WHEN 'T'.
            lv_kind = abap_func_tables.
            CREATE DATA ls_param-value TYPE STANDARD TABLE OF (lv_type).
        ENDCASE.

        ls_param-name = <fs_data>-name.
        ls_param-kind = lv_kind.

        FIELD-SYMBOLS <fs_value>.
        ASSIGN ls_param-value->* TO <fs_value>.
        /ui2/cl_json=>deserialize(
          EXPORTING
          json = <fs_data>-value
          CHANGING
          data = <fs_value>
        ).

        INSERT ls_param INTO TABLE lt_param.
      ENDLOOP.

      LOOP AT lt_fupararef ASSIGNING FIELD-SYMBOL(<fs_fupararef>)
        WHERE paramtype = 'X'.
        INSERT VALUE #( name = <fs_fupararef>-parameter value = sy-tabix ) INTO TABLE lt_excep.
      ENDLOOP.
      INSERT VALUE #( name = 'OTHERS' value = sy-tabix + 1 ) INTO TABLE lt_excep.

      DATA lv_data TYPE REF TO data.
      FIELD-SYMBOLS <fv_data>.

      CALL FUNCTION lv_funcname
        PARAMETER-TABLE
        lt_param
        EXCEPTION-TABLE
        lt_excep.
      DATA(lv_subrc) = sy-subrc.
      IF ( lv_subrc <> 0 ).
        RAISE EXCEPTION TYPE lcx_dyna_func
          EXPORTING
            text = |Exception '{ lt_excep[ value = lv_subrc ]-name }' occured|.
      ENDIF.

      LOOP AT lt_param ASSIGNING FIELD-SYMBOL(<fs_param>).
        INSERT VALUE #( name = <fs_param>-name value = /ui2/cl_json=>serialize( data = <fs_param>-value numc_as_string = abap_true ) ) INTO TABLE lt_output.
      ENDLOOP.

      DATA(lv_json) = /ui2/cl_json=>serialize( data = lt_output numc_as_string = abap_true ).
*  DATA(lv_excep) = /ui2/cl_json=>serialize( data = lt_excep numc_as_string = abap_true ).

    CATCH cx_root INTO DATA(lx).
  ENDTRY.

END-OF-SELECTION.
  WRITE: / 'FUNCTION:', lv_funcname COLOR 2 INTENSIFIED OFF.
  WRITE: / 'REQUEST:', lv_request COLOR 2 INTENSIFIED OFF.
  WRITE: / 'RESPONSE:', lv_json COLOR 2 INTENSIFIED OFF.
  IF ( lx IS BOUND ).
    WRITE: / 'EXCEPTION:' COLOR 3, lx->get_text( ) COLOR 3.
  ENDIF.
