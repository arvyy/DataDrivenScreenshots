#include "./raylib/raylib.h"
#include <libguile.h>

SCM
get_mouse_pos()
{
    return scm_cons(scm_from_double(GetMouseX()), scm_from_double(GetMouseY()));
}

SCM
is_mouse(SCM button, SCM pred)
{
    int btn = -1;
    if (scm_is_true(scm_equal_p(button, scm_from_utf8_symbol("left")))) {
        btn = MOUSE_LEFT_BUTTON;
    } else if (scm_is_true(scm_equal_p(button, scm_from_utf8_symbol("middle")))) {
        btn = MOUSE_MIDDLE_BUTTON;
    } else if (scm_is_true(scm_equal_p(button, scm_from_utf8_symbol("left")))) {
        btn = MOUSE_RIGHT_BUTTON;
    }
    if ( btn == -1) return SCM_BOOL_F;
    int rez = 0;
    if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("pressed")))) {
        rez = IsMouseButtonPressed(btn);
    } else if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("down")))) {
        rez = IsMouseButtonDown(btn);
    } else if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("released")))) {
        rez = IsMouseButtonReleased(btn);
    } else if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("up")))) {
        rez = IsMouseButtonUp(btn);
    }
    return scm_from_bool(rez);
}

SCM
is_key(SCM key_scm, SCM pred)
{
    int key = scm_to_int(key_scm);
    int rez = 0;
    if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("pressed")))) {
        rez = IsKeyPressed(key);
    } else if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("down")))) {
        rez = IsKeyDown(key);
    } else if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("released")))) {
        rez = IsKeyReleased(key);
    } else if (scm_is_true(scm_equal_p(pred, scm_from_utf8_symbol("up")))) {
        rez = IsKeyUp(key);
    }
    return scm_from_bool(rez);
}

SCM
get_key_pressed()
{
    return scm_from_int(GetKeyPressed());
}

void input_init(void* data) {
    scm_c_define_gsubr("get-mouse-pos", 0, 0, 0, get_mouse_pos);
    scm_c_define_gsubr("is-mouse?", 2, 0, 0, is_mouse);
    scm_c_define_gsubr("is-key?", 2, 0, 0, is_key);
    scm_c_define_gsubr("get-key-pressed", 0, 0, 0, get_key_pressed);
}
