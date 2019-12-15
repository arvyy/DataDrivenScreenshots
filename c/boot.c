#include "./raylib/raylib.h"
#include "./raylib/rlgl.h"
#include "base.h"
#include <libguile.h>

SCM
get_frame_time()
{
    return scm_from_double(GetFrameTime());
}

SCM
init_window(SCM width, SCM height, SCM fps_scm, SCM title)
{
    int w = scm_to_int(width);
    int h = scm_to_int(height);
    int fps = scm_to_int(fps_scm);
    setWidth(w);
    setHeight(h);
    char* t = scm_to_utf8_stringn(title, NULL);
    SetTargetFPS(fps);
    SetConfigFlags(FLAG_MSAA_4X_HINT);
    InitWindow(w, h, t);
    initAfterRL();
    free(t);
    return SCM_BOOL_T;
}

SCM
window_should_close() 
{
    return scm_from_bool(WindowShouldClose());
}

SCM
rlgl_draw()
{
    rlglDraw();
    return SCM_BOOL_T;
}

SCM
begin_draw()
{
    BeginDrawing();
    return SCM_BOOL_T;
}

SCM
end_draw()
{
    EndDrawing();
    return SCM_BOOL_T;
}

SCM
clear_bg(SCM bgColor_scm)
{
    Color bgColor = (Color) { 
        scm_to_int(SCM_SIMPLE_VECTOR_REF(bgColor_scm, 0)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(bgColor_scm, 1)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(bgColor_scm, 2)),
        0
    };
    ClearBackground(bgColor);
    return SCM_BOOL_T;
}

void boot_init(void* data)
{
    scm_c_define_gsubr("init-window", 4, 0, 0, init_window);
    scm_c_define_gsubr("window-close?", 0, 0, 0, window_should_close);
    scm_c_define_gsubr("rlgl-draw", 0, 0, 0, rlgl_draw);
    scm_c_define_gsubr("begin-draw", 0, 0, 0, begin_draw);
    scm_c_define_gsubr("end-draw", 0, 0, 0, end_draw);
    scm_c_define_gsubr("clear-bg", 1, 0, 0, clear_bg);
    scm_c_define_gsubr("get-frame-time", 0, 0, 0, get_frame_time);
}
