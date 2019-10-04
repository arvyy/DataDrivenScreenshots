#include <raylib.h>
#include "raymath.h"
#include "rlgl.h"
#include <libguile.h>
#include <GL/gl.h>

char* script;
int width;
int height;

#define MAX_CNT_STACK_SIZE 50
typedef struct CntContext {
    RenderTexture2D renderTexture;
    int srcBlendFactor;
    int destBlendFactor;
    int blendEnabled;
} CntContext;
CntContext cnt_context_stack[MAX_CNT_STACK_SIZE];
int cnt_context_stack_length = 0;
int cnt_context_stack_texture_init_count = 0;

int lastPPImgIndex = 0;
RenderTexture2D ppTextures[2];

RenderTexture2D *draw_to_texture = NULL;

Model planeModel;


void init_plane_model()
{
    Mesh planeMesh = GenMeshPlane(1, 1, 1, 1);
    planeModel = LoadModelFromMesh(planeMesh);
}

void draw_plane(float x, float y, float width, float height, Color color, Texture2D texture)
{
    planeModel.materials[0].maps[MAP_DIFFUSE].texture = texture;
    planeModel.transform = MatrixMultiply(MatrixRotate((Vector3){1, 0, 0}, -0.5*PI), MatrixScale(width, height, 1));
    DrawModel(planeModel, (Vector3){ x + width/2, y + height / 2, -0.1 }, 1, color);
}

int blend_factor_from_scm(SCM blend_fac)
{
#define cmp_sym_with_enum(SYMBOL) if (scm_is_eq(blend_fac, scm_from_utf8_symbol(#SYMBOL))) {\
    return SYMBOL;\
}
    cmp_sym_with_enum(GL_ZERO);
    cmp_sym_with_enum(GL_ONE);
    cmp_sym_with_enum(GL_SRC_COLOR);
    cmp_sym_with_enum(GL_ONE_MINUS_SRC_COLOR);
    cmp_sym_with_enum(GL_DST_COLOR);
    cmp_sym_with_enum(GL_ONE_MINUS_DST_COLOR);
    cmp_sym_with_enum(GL_SRC_ALPHA);
    cmp_sym_with_enum(GL_ONE_MINUS_SRC_ALPHA);
    cmp_sym_with_enum(GL_DST_ALPHA);
    cmp_sym_with_enum(GL_ONE_MINUS_DST_ALPHA);
    cmp_sym_with_enum(GL_CONSTANT_COLOR);
    cmp_sym_with_enum(GL_ONE_MINUS_CONSTANT_COLOR);
    cmp_sym_with_enum(GL_CONSTANT_ALPHA);
    cmp_sym_with_enum(GL_ONE_MINUS_CONSTANT_ALPHA);
    cmp_sym_with_enum(GL_SRC_ALPHA_SATURATE);
    cmp_sym_with_enum(GL_SRC1_COLOR);
    cmp_sym_with_enum(GL_ONE_MINUS_SRC1_COLOR);
    cmp_sym_with_enum(GL_SRC1_ALPHA);
    cmp_sym_with_enum(GL_ONE_MINUS_SRC1_ALPHA);
#undef cmp_sym_with_enum
    return -1;
}

void enable_blend_after_first_draw() 
{
    if (cnt_context_stack_length < 1) return;
    CntContext *c = &cnt_context_stack[cnt_context_stack_length - 1];
    if (!c->blendEnabled) {
        c->blendEnabled = 1;
        rlglDraw();
        glEnable(GL_BLEND);
    }
}

SCM
push_pp_texture(SCM blend)
{
    if (cnt_context_stack_length > 0 || draw_to_texture != NULL) {
        EndTextureMode();
    }
    RenderTexture2D target;
    if (cnt_context_stack_texture_init_count > cnt_context_stack_length) {
        target = cnt_context_stack[cnt_context_stack_length].renderTexture;
    } else {
        target = LoadRenderTexture(width, height);
        cnt_context_stack_texture_init_count++;
    }
    CntContext *c = &cnt_context_stack[cnt_context_stack_length];
    cnt_context_stack_length++;
    c->renderTexture = target;
    c->blendEnabled = 0;
    BeginTextureMode(target);
    ClearBackground(BLANK);
    if (scm_is_true(blend)) {
        c->srcBlendFactor = blend_factor_from_scm(scm_car(blend));
        c->destBlendFactor = blend_factor_from_scm(scm_cdr(blend));
    } else {
        c->srcBlendFactor = GL_SRC_ALPHA;
        c->destBlendFactor = GL_ONE_MINUS_SRC_ALPHA;
    }
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    return SCM_BOOL_T;
}

SCM
begin_pp_chain() 
{
    rlglDraw();
    EndTextureMode();
    rlLoadIdentity();
    glBlendFunc(GL_ONE, GL_ZERO);
    RenderTexture2D src = cnt_context_stack[cnt_context_stack_length - 1].renderTexture;
    lastPPImgIndex = 0;
    BeginTextureMode(ppTextures[0]);
    ClearBackground(BLANK);
    DrawTextureRec(
            src.texture, 
            (Rectangle){ 0, 0, src.texture.width, -src.texture.height }, 
            (Vector2) { 0, 0 }, 
            WHITE);
    EndTextureMode();
    return SCM_BOOL_T;
}

SCM
pp_chain_next(SCM shader_id)
{
    RenderTexture2D src = ppTextures[lastPPImgIndex];
    int ppImgIndex = (lastPPImgIndex + 1) % 2;
    BeginTextureMode(ppTextures[ppImgIndex]);
    Shader* shader_ptr = NULL;
    if (scm_is_true(shader_id)){
        shader_ptr = scm_to_pointer(shader_id);
    } 
    Shader shader = shader_ptr != NULL? *shader_ptr : GetShaderDefault();
    ClearBackground(BLANK);
    planeModel.materials[0].shader = shader;
    draw_plane(0, 0, src.texture.width, src.texture.height, WHITE, src.texture);
    planeModel.materials[0].shader = GetShaderDefault();
    EndTextureMode();
    lastPPImgIndex = ppImgIndex;
    return SCM_BOOL_T;
}

SCM
pop_pp_texture()
{
    if (cnt_context_stack_length <= 0)
        return SCM_BOOL_T;
    if (lastPPImgIndex == 1) {
        pp_chain_next(SCM_BOOL_F);
        return pop_pp_texture();
    }
    RenderTexture2D target = ppTextures[lastPPImgIndex];
    cnt_context_stack_length--;
    if (cnt_context_stack_length > 0) {
        CntContext *prev = &cnt_context_stack[cnt_context_stack_length - 1];
        if (!prev->blendEnabled) {
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        } else {
            glBlendFunc(prev->srcBlendFactor, prev->destBlendFactor);
        }
        BeginTextureMode(prev->renderTexture);
    } else {
        if (draw_to_texture != NULL) {
            BeginTextureMode(*draw_to_texture);
        }
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }
    DrawTextureRec(
            target.texture, 
            (Rectangle){ 0, 0, target.texture.width, -target.texture.height }, 
            (Vector2) { 0, 0 }, 
            WHITE);
    enable_blend_after_first_draw();
    return SCM_BOOL_T;
}

Matrix scm_to_matrix(SCM transform)
{
    Matrix m = MatrixIdentity();
    m.m0 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 0));
    m.m4 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 1));
    m.m12 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 2));
    m.m1 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 3));
    m.m5 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 4));
    m.m13 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 5));
    return m;
}

SCM
apply_transform(SCM transform)
{
    rlglDraw();
    rlLoadIdentity();
    Matrix m = scm_to_matrix(transform);
    rlMultMatrixf(MatrixToFloat(m));
    return SCM_BOOL_T;
}

SCM
load_font(SCM fontFile)
{
    char* file = scm_to_utf8_stringn(fontFile, NULL);
    Font *f;
    f = malloc(sizeof(Font));
    *f = LoadFont(file);
    free(file);
    return scm_from_pointer(f, NULL);
}

SCM
load_texture(SCM textureFile)
{
    char* file = scm_to_utf8_stringn(textureFile, NULL);
    Texture *t;
    t = malloc(sizeof(Texture));
    *t = LoadTexture(file);
    free(file);
    return scm_from_pointer(t, NULL);
}

SCM
create_render_texture(SCM width, SCM height)
{
    RenderTexture2D *t = malloc(sizeof(RenderTexture2D));
    *t = LoadRenderTexture(scm_to_int(width), scm_to_int(height));
    return scm_from_pointer(t, NULL);
}

SCM
render_texture_to_texture(SCM texture)
{
    RenderTexture2D *t  = scm_to_pointer(texture);
    return scm_from_pointer(&(t->texture), NULL);
}

SCM
set_draw_target(SCM texture)
{
    if (draw_to_texture != NULL)
        EndTextureMode();
    if (scm_is_true(texture)) {
        draw_to_texture = scm_to_pointer(texture);
        BeginTextureMode(*draw_to_texture);
    } else {
        draw_to_texture = NULL;
    }
    return SCM_BOOL_T;
}

SCM
load_shader(SCM vertexShaderFile, SCM fragmentShaderFile)
{
    char* vsFile = NULL;
    if (scm_is_true(vertexShaderFile)) {
        vsFile = scm_to_utf8_stringn(vertexShaderFile, NULL);
    }
    char* fsFile = NULL;
    if (scm_is_true(fragmentShaderFile)) {
        fsFile = scm_to_utf8_stringn(fragmentShaderFile, NULL);
    }
    Shader* shader_ptr = malloc(sizeof (Shader));
    *shader_ptr = LoadShader(vsFile, fsFile);
    if (vsFile != NULL) free(vsFile);
    if (fsFile != NULL) free(fsFile);
    SCM new_shader_id = scm_from_pointer(shader_ptr, NULL);
    return new_shader_id;
}

SCM
get_shader_loc(SCM shader_id, SCM uniformName)
{
    Shader shader = *((Shader*)scm_to_pointer(shader_id));
    char* name = scm_to_utf8_stringn(uniformName, NULL);
    int loc = GetShaderLocation(shader, name);
    free(name);
    return scm_from_int(loc);
}

SCM
set_shader_value_matrix(SCM shader_id, SCM loc, SCM matrix)
{
    Shader shader = *((Shader*)scm_to_pointer(shader_id));
    SetShaderValueMatrix(shader, scm_to_int(loc), scm_to_matrix(matrix));
    return SCM_BOOL_T;
}

SCM
set_shader_value_matrix_invert(SCM shader_id, SCM loc, SCM matrix)
{
    Shader shader = *((Shader*)scm_to_pointer(shader_id));
    SetShaderValueMatrix(shader, scm_to_int(loc), MatrixInvert(scm_to_matrix(matrix)));
    return SCM_BOOL_T;
}

SCM
set_shader_value_texture(SCM shader_id, SCM loc, SCM texture_id)
{
    Shader *shader = (Shader*)scm_to_pointer(shader_id);
    planeModel.materials[0].maps[MAP_EMISSION].texture = *((Texture*)scm_to_pointer(texture_id));
    shader->locs[LOC_MAP_EMISSION] = scm_to_int(loc);
    return SCM_BOOL_T;
}

SCM
set_shader_value_vec_float(SCM shader_id, SCM loc, SCM vec)
{
    Shader shader = *((Shader*)scm_to_pointer(shader_id));
    float value[4];
    size_t l = SCM_SIMPLE_VECTOR_LENGTH(vec);
    for (size_t i = 0; i < l; i++) {
        SCM f =SCM_SIMPLE_VECTOR_REF(vec, i);
        value[i] = scm_to_double(f);
    }
    int type;
    switch (l) {
        case 2:
            type = UNIFORM_VEC2; break;
        case 3 :
            type = UNIFORM_VEC3; break;
        case 4 :
            type = UNIFORM_VEC4; break;
        case 1: 
        default:
            type = UNIFORM_FLOAT; break;
    }
    SetShaderValue(shader, scm_to_int(loc), value, type);
    return SCM_BOOL_T;
}

Color
scm_to_color(SCM color_vec)
{
    Color c;
    c.r = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 0) );
    c.g = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 1) );
    c.b = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 2) );
    c.a = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 3) );
    return c;
}

SCM
draw_rect_scm(SCM x, SCM y, SCM width, SCM height, SCM roundness, SCM fill, SCM stroke, SCM stroke_width, SCM segments)
{
    Rectangle r = {
        scm_to_double(x),
        scm_to_double(y),
        scm_to_double(width),
        scm_to_double(height)
    };
    float round = scm_to_double(roundness);
    int segm = scm_to_int(segments);
    if (scm_is_true(fill)) {
        DrawRectangleRounded(r, round, segm, scm_to_color(fill));
    }
    if (scm_is_true(stroke)) {
        DrawRectangleRoundedLines(r, round, segm, scm_to_int(stroke_width), scm_to_color(stroke));
    }
    return SCM_BOOL_T;
}

SCM
draw_circle_scm(SCM x, SCM y, SCM r, SCM inner_r, SCM angl, SCM angl2, SCM fill, SCM stroke, SCM stroke_width, SCM segments_scm)
{
    Vector2 center = { scm_to_double(x), scm_to_double(y) };
    float radius = scm_to_double(r);
    float inner_radius = scm_to_double(inner_r);
    int startAngle = scm_to_int(angl);
    int endAngle = scm_to_int(angl2);
    int segments = scm_to_int(segments_scm);
    float width = scm_to_double(stroke_width);
    if (scm_is_true(fill)) {
        DrawRing(center, inner_radius, radius, startAngle, endAngle, segments, scm_to_color(fill)); // Draw ring
    }
    if (scm_is_true(stroke)) {
        Color c = scm_to_color(stroke);
        if (inner_radius > 0) {
            DrawRing(center, inner_radius - 0.5*width, inner_radius + 0.5*width, startAngle, endAngle, segments, c);
        }
        DrawRing(center, radius - 0.5*width, radius + 0.5*width, startAngle, endAngle, segments, c);
    }
    return SCM_BOOL_T;
}

SCM
draw_triangle_scm(SCM a, SCM b, SCM c, SCM fill, SCM stroke, SCM stroke_width)
{
    Vector2 v1 = { scm_to_double(scm_car(a)), scm_to_double(scm_cdr(a)) };
    Vector2 v2 = { scm_to_double(scm_car(b)), scm_to_double(scm_cdr(b)) };
    Vector2 v3 = { scm_to_double(scm_car(c)), scm_to_double(scm_cdr(c)) };
    float thick = scm_to_double(stroke_width);
    if (scm_is_true(fill)) {
        DrawTriangle(v1, v2, v3, scm_to_color(fill));
    }
    if (scm_is_true(stroke)) {
        Color c = scm_to_color(stroke);
        DrawLineEx(v1, v2, thick, c);
        DrawLineEx(v2, v3, thick, c);
        DrawLineEx(v3, v1, thick, c);
    }
    return SCM_BOOL_T;
}

SCM
draw_line_scm(SCM points, SCM stroke, SCM stroke_width)
{
    if (!scm_is_true(stroke)) return SCM_BOOL_T;
    float thick = scm_to_double(stroke_width);
    Color c = scm_to_color(stroke);
    for (int i = 1; i < SCM_SIMPLE_VECTOR_LENGTH(points); i++) {
        SCM p1_s = SCM_SIMPLE_VECTOR_REF(points, i - 1);
        SCM p2_s = SCM_SIMPLE_VECTOR_REF(points, i);
        Vector2 p1 = { scm_to_double(scm_car(p1_s)), scm_to_double(scm_cdr(p1_s))};
        Vector2 p2 = { scm_to_double(scm_car(p2_s)), scm_to_double(scm_cdr(p2_s))};
        DrawLineEx(p1, p2, thick, c);
    }
    return SCM_BOOL_T;
}

SCM
draw_t_rect_scm(SCM x, SCM y, SCM width, SCM height, SCM color_vec, SCM texture_id)
{
    Color c = scm_to_color(color_vec);
    Vector2 size = { 
        scm_to_double(width), 
        scm_to_double(height)
    };
    Vector2 pos = { 
        scm_to_double(x),
        scm_to_double(y)
    };
    Texture* t_id;
    if (scm_is_true(texture_id)){
        t_id = scm_to_pointer(texture_id);
    } else {
        t_id = NULL;
    }
    Texture2D texture = t_id == NULL? GetTextureDefault() : *t_id;
    Rectangle srcRec = (Rectangle){ 0, 0, texture.width, texture.height };
    Rectangle destRec = (Rectangle){ pos.x, pos.y, size.x, size.y };
    DrawTexturePro(texture, srcRec, destRec, (Vector2) { 0, 0 }, 0, c);
    enable_blend_after_first_draw();
    return SCM_BOOL_T;
}

SCM
draw_text_scm(SCM text, SCM x, SCM y, SCM font_scm, SCM font_size, SCM spacing, SCM color_vec)
{
    Color c = scm_to_color(color_vec);
    char* txt = scm_to_utf8_stringn(text, NULL);
    Vector2 pos = {scm_to_int(x),scm_to_int(y)};
    Font font;
    if (scm_is_true(font_scm)) {
        font = *((Font*) scm_to_pointer(font_scm));
    } else {
        font = GetFontDefault();
    }
    DrawTextEx(font, txt, pos, scm_to_double(font_size), scm_to_double(spacing), c);
    enable_blend_after_first_draw();
    free(txt);
    return SCM_BOOL_T;
}

SCM
frame_time()
{
    return scm_from_double(GetFrameTime());
}

SCM
total_time()
{
    return scm_from_double(GetTime());
}

void init_chart_base_native_module(void* data)
{
    scm_c_define_gsubr("frame-time", 0, 0, 0, frame_time);
    scm_c_define_gsubr("total-time", 0, 0, 0, total_time);
    scm_c_define_gsubr("set-draw-target", 1, 0, 0, set_draw_target);
    scm_c_define_gsubr("draw-rect*", 9, 0, 0, draw_rect_scm);
    scm_c_define_gsubr("draw-triangle*", 6, 0, 0, draw_triangle_scm);
    scm_c_define_gsubr("draw-circle*", 10, 0, 0, draw_circle_scm);
    scm_c_define_gsubr("draw-line*", 3, 0, 0, draw_line_scm);
    scm_c_define_gsubr("draw-t-rect*", 6, 0, 0, draw_t_rect_scm);
    scm_c_define_gsubr("draw-text*", 7, 0, 0, draw_text_scm);
    scm_c_define_gsubr("apply-transform", 1, 0, 0, apply_transform);
    scm_c_define_gsubr("load-shader", 2, 0, 0, load_shader);
    scm_c_define_gsubr("load-font", 1, 0, 0, load_font);
    scm_c_define_gsubr("load-texture", 1, 0, 0, load_texture);
    scm_c_define_gsubr("create-render-texture", 2, 0, 0, create_render_texture);
    scm_c_define_gsubr("render-texture->texture", 1, 0, 0, render_texture_to_texture);
    scm_c_define_gsubr("get-shader-loc", 2, 0, 0, get_shader_loc);
    scm_c_define_gsubr("set-shader-value", 3, 0, 0, set_shader_value_vec_float);
    scm_c_define_gsubr("set-shader-value-texture", 3, 0, 0, set_shader_value_texture);
    scm_c_define_gsubr("set-shader-value-matrix", 3, 0, 0, set_shader_value_matrix);
    scm_c_define_gsubr("set-shader-value-matrix/invert", 3, 0, 0, set_shader_value_matrix_invert);
    scm_c_define_gsubr("push-pp-texture", 1, 0, 0, push_pp_texture);
    scm_c_define_gsubr("pop-pp-texture", 0, 0, 0, pop_pp_texture);
    scm_c_define_gsubr("begin-pp-chain", 0, 0, 0, begin_pp_chain);
    scm_c_define_gsubr("pp-chain-next", 1, 0, 0, pp_chain_next);



    scm_c_export(
            "frame-time", "total-time", 
            "set-draw-target", 
            "draw-rect*", "draw-text*", "draw-t-rect*", "draw-circle*", "draw-line*", "draw-triangle*",
            "apply-transform",
            "load-shader", "get-shader-loc", 
            "load-font",
            "set-shader-value", "set-shader-value-texture", "set-shader-value-matrix", "set-shader-value-matrix/invert",
            "create-render-texture", "render-texture->texture", "load-texture",
            "push-pp-texture", "pop-pp-texture", "begin-pp-chain", "pp-chain-next",
            NULL);
}

void init_guile_fn() {
    scm_c_define_module ("chart base native", init_chart_base_native_module, NULL);  
}

void loadBootstrap() {
    scm_c_primitive_load("guile/preamble.scm");
}

void loadScript() {
    scm_c_primitive_load(script);
}

void updateProtected(SCM* data, SCM value) {
    scm_gc_protect_object(value);
    scm_gc_unprotect_object(*data);
    *data = value;
}

void* main2(void* args) {
    int fps;
    Color bgColor;
    int maxFrames;
    int currentRecFrame;
    int loop;
    char* outputFolder = NULL;
    float elapsedTime = 0;
    int recording = 0;
    int paused = 0;
    int showValue = 1;
    float speed = 1;
    init_guile_fn();
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT | FLAG_WINDOW_HIDDEN);
    InitWindow(100, 100, "D3");
    loadBootstrap();
    loadScript();
    width = scm_to_int(scm_variable_ref(scm_c_lookup("width")));
    height = scm_to_int(scm_variable_ref(scm_c_lookup("height")));
    fps = scm_to_int(scm_variable_ref(scm_c_lookup("fps")));
    maxFrames = scm_to_int(scm_variable_ref(scm_c_lookup("max-frames")));
    loop = scm_is_true(scm_variable_ref(scm_c_lookup("loop?")));
    outputFolder = scm_to_utf8_stringn(scm_variable_ref(scm_c_lookup("output-folder")), NULL);
    SetWindowSize(width, height);
    SetWindowPosition(0, 0);
    UnhideWindow();
    SCM bgColor_scm = scm_variable_ref(scm_c_lookup("bg-color"));
    bgColor = (Color) { 
        scm_to_int(SCM_SIMPLE_VECTOR_REF(bgColor_scm, 0)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(bgColor_scm, 1)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(bgColor_scm, 2)),
        0
    };
    ppTextures[0] = LoadRenderTexture(width, height);
    ppTextures[1] = LoadRenderTexture(width, height);
    init_plane_model();

    SetTargetFPS(fps);
    SCM update, render, init, data, new_data, key_press, format, debug_info, stop, output_frame, init_render;
    update = scm_variable_ref(scm_c_lookup("update"));
    init = scm_variable_ref(scm_c_lookup("init-data"));
    init_render = scm_variable_ref(scm_c_lookup("init-render"));
    key_press = scm_variable_ref(scm_c_lookup("key-press"));
    render = scm_variable_ref(scm_c_lookup("render"));
    format = scm_variable_ref(scm_c_lookup("format"));
    debug_info = scm_variable_ref(scm_c_lookup("debug-info"));
    stop = scm_variable_ref(scm_c_lookup("stop?"));
    output_frame = scm_variable_ref(scm_c_lookup("output-frame"));
    data = scm_gc_protect_object(scm_call_0(init));
    BeginDrawing();
    scm_call_0(init_render);
    EndDrawing();
    while (!WindowShouldClose()) {
        int key = GetKeyPressed();
        if (key == KEY_R || key == KEY_R + 32) {
            loadScript();
        } else if (key == KEY_SPACE) {
            paused = !paused;
        } else if (key == KEY_ZERO) {
            updateProtected(&data, scm_call_0(init));
            elapsedTime = 0;
        } else if (key == KEY_W || key == KEY_W + 32) {
            elapsedTime += 1;
            updateProtected(&data, scm_call_0(init));
            updateProtected(&data, scm_call_2(update, scm_from_double(elapsedTime), data));
        } else if (key == KEY_S || key == KEY_S + 32) {
            elapsedTime -= 1;
            if (elapsedTime < 0)
                elapsedTime = 0;
            updateProtected(&data, scm_call_0(init));
            updateProtected(&data, scm_call_2(update, scm_from_double(elapsedTime), data));
        } else if (key == KEY_Q || key == KEY_Q + 32) {
            speed = speed < 8? speed * 2 : 8;
        } else if (key == KEY_A || key == KEY_A + 32) {
            speed = speed > 0.125? speed / 2 : 0.125;
        } else if (key == KEY_D || key == KEY_D + 32) {
            showValue = !showValue;
        } else if (key == KEY_G || key == KEY_G + 32){
            recording = !recording;
            if (recording) {
                paused = 0;
                currentRecFrame = 0;
            }
        } else {
            updateProtected(&data, scm_call_2(key_press, scm_from_int(key), data));
        }
        BeginDrawing();
        ClearBackground(bgColor);
        scm_call_1(render, data);
        rlglDraw();
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        if (recording) {
            EndDrawing();
            char* outputFrame = scm_to_utf8_stringn(scm_call_2(output_frame, scm_from_locale_string(script), scm_from_int(currentRecFrame)), NULL);
            currentRecFrame++;
            const char* ssName = TextFormat("%s/%s", outputFolder, outputFrame);
            TakeScreenshot(ssName);
            free(outputFrame);
            BeginDrawing();
        }
        if (showValue && !recording) {
            rlglDraw();
            rlLoadIdentity();
            SCM debug_data = scm_call_1(debug_info, data);
            SCM scm_text = scm_call_3(format, SCM_BOOL_F, scm_from_locale_string("~a"), debug_data);
            char* text = scm_to_utf8_stringn(scm_text, NULL);
            DrawText(text, 20, 20, 10, BLACK);
            DrawText(TextFormat("Speed: %fx", speed), 20, 30, 10, BLACK);
            if (paused) {
                DrawText("Paused", 20, 40, 10, BLACK);
            }
            free(text);
        }
        EndDrawing();
        if (!paused) {
            float frameTime = !recording? (GetFrameTime() * speed) : (1.0 / fps);
            elapsedTime += frameTime;
            updateProtected(&data, scm_call_2(update, scm_from_double(frameTime), data));
        }
        int should_stop = scm_is_true(scm_call_1(stop, data)); 
        if (recording) {
            if (should_stop || currentRecFrame > maxFrames) {
                recording = 0;
            }
        } else if (should_stop && loop) {
            updateProtected(&data, scm_call_0(init));
            elapsedTime = 0;
        }
    }
    return 0;
}

int main(int argc, char* args[]) {
    if (argc < 2) {
        printf("Expected first parameter to be scheme script file name. Exiting\n");
        return 1;
    }
    script = args[1];
    scm_with_guile(main2, NULL);
    return 0;
}
