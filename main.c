#include <raylib.h>
#include "raymath.h"
#include "rlgl.h"
#include <libguile.h>
#include <GL/gl.h>

#define GUILE_AUTO_COMPILE 0

#define MAX_SHADER 20
#define MAX_RENDER_TEXTURE 20
#define MAX_TEXTURES 100

Shader shaders[MAX_SHADER];
int shaderLength = 0;

RenderTexture2D renderTextures[MAX_RENDER_TEXTURE];
int renderTexturesLength = 0;
int renderTexturesInitCount = 0;

Texture textures[MAX_TEXTURES];
int texturesLength = 0;

int lastPPImgIndex = 0;
RenderTexture2D ppTextures[2];

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

SCM
push_pp_texture()
{
    if (renderTexturesLength > 0) {
        EndTextureMode();
    }
    RenderTexture2D target;
    if (renderTexturesInitCount > renderTexturesLength) {
        target = renderTextures[renderTexturesLength];
    } else {
        target = LoadRenderTexture(800, 450);
        renderTexturesInitCount++;
    }
    BeginTextureMode(target);
    ClearBackground(BLANK);
    renderTextures[renderTexturesLength] = target;
    renderTexturesLength++;
    return SCM_BOOL_T;
}

SCM
begin_pp_chain() 
{
    rlglDraw();
    EndTextureMode();
    rlLoadIdentity();
    BeginBlendMode(BLEND_ADDITIVE);
    glBlendFunc(GL_ONE, GL_ZERO);
    RenderTexture2D src = renderTextures[renderTexturesLength - 1];
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
    int shader_index = scm_to_int(shader_id);
    Shader shader = shader_index != -1? shaders[shader_index] : GetShaderDefault() ;
    //BeginShaderMode(shader);
    ClearBackground(BLANK);
    /*
    DrawTextureRec(
            src.texture, 
            (Rectangle){ 0, 0, src.texture.width, -src.texture.height }, 
            (Vector2) { 0, 0 }, 
            WHITE);
            */
    planeModel.materials[0].shader = shader;
    draw_plane(0, 0, src.texture.width, src.texture.height, WHITE, src.texture);
    planeModel.materials[0].shader = GetShaderDefault();
    //EndShaderMode();
    EndTextureMode();
    lastPPImgIndex = ppImgIndex;
    return SCM_BOOL_T;
}

SCM
pop_pp_texture()
{
    if (renderTexturesLength <= 0)
        return SCM_BOOL_T;
    if (lastPPImgIndex == 1) {
        pp_chain_next(scm_from_int(-1));
        return pop_pp_texture();
    }
    RenderTexture2D target = ppTextures[lastPPImgIndex];
    renderTexturesLength--;
    if (renderTexturesLength > 0) {
        BeginTextureMode(renderTextures[renderTexturesLength - 1]);
    }
    EndBlendMode();
    DrawTextureRec(
            target.texture, 
            (Rectangle){ 0, 0, target.texture.width, -target.texture.height }, 
            (Vector2) { 0, 0 }, 
            WHITE);
    return SCM_BOOL_T;
}

SCM
apply_transform(SCM transform)
{
    rlglDraw();
    Matrix m = MatrixIdentity();
    m.m0 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 0));
    m.m4 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 1));
    m.m8 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 2));
    m.m1 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 3));
    m.m5 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 4));
    m.m9 = scm_to_double(SCM_SIMPLE_VECTOR_REF(transform, 5));
    rlLoadIdentity();
    rlMultMatrixf(MatrixToFloat(MatrixInvert(m)));
    //rlMultMatrixf(MatrixToFloat(m));
    return SCM_BOOL_T;
}

SCM
load_texture(SCM textureFile)
{
    char* file = scm_to_utf8_stringn(textureFile, NULL);
    Texture t = LoadTexture(file);
    free(file);
    textures[texturesLength] = t;
    return scm_from_int(texturesLength++);
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
    shaders[shaderLength] = LoadShader(vsFile, fsFile);
    if (vsFile != NULL) free(vsFile);
    if (fsFile != NULL) free(fsFile);
    SCM new_shader_id = scm_from_int(shaderLength++);
    return new_shader_id;
}

SCM
get_shader_loc(SCM shader_id, SCM uniformName)
{
    Shader shader = shaders[scm_to_int(shader_id)];
    char* name = scm_to_utf8_stringn(uniformName, NULL);
    int loc = GetShaderLocation(shader, name);
    free(name);
    return scm_from_int(loc);
}

SCM
set_shader_value_texture(SCM shader_id, SCM loc, SCM texture_id)
{
    Shader *shader = &shaders[scm_to_int(shader_id)];
    planeModel.materials[0].maps[MAP_EMISSION].texture = textures[scm_to_int(texture_id)];
    shader->locs[LOC_MAP_EMISSION] = scm_to_int(loc);
    return SCM_BOOL_T;
}

SCM
set_shader_value_vec_float(SCM shader_id, SCM loc, SCM vec)
{
    Shader shader = shaders[scm_to_int(shader_id)];
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

SCM
draw_rect_scm(SCM x, SCM y, SCM width, SCM height, SCM color_vec, SCM texture_id)
{
    Color c;
    c.r = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 0) );
    c.g = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 1) );
    c.b = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 2) );
    c.a = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 3) );
    Vector2 size = { 
        scm_to_double(width), 
        scm_to_double(height)
    };
    Vector2 pos = { 
        scm_to_double(x),
        scm_to_double(y)
    };
    int t_id;
    if (scm_is_true(texture_id)){
        t_id = scm_to_int(texture_id);
    } else {
        t_id = -1;
    }
    Texture2D texture = t_id == -1? GetTextureDefault() : textures[t_id];
    Rectangle srcRec = (Rectangle){ 0, 0, texture.width, texture.height };
    Rectangle destRec = (Rectangle){ pos.x, pos.y, size.x, size.y };
    DrawTexturePro(texture, srcRec, destRec, (Vector2) { 0, 0 }, 0, c);
    /*
    //rlPushMatrix();
    //rlTranslatef(center.x, center.y, 0);
    //rlScalef(size.x, size.y, 1);
    //rlRotatef(-90, 1, 0, 0);
    //DrawPlane((Vector3) {0, 0, 0}, (Vector2) {1, 1}, RED);
    //rlPopMatrix();
    rlPushMatrix();
    Mesh planeMesh = GenMeshPlane(1, 1, 1, 1);
    Model planeModel = LoadModelFromMesh(planeMesh);
    Image checked = GenImageChecked(2, 2, 1, 1, RED, GREEN);
    Texture2D texture = LoadTextureFromImage(checked);
    UnloadImage(checked);
    planeModel.materials[0].maps[MAP_DIFFUSE].texture = texture;
    planeModel.transform = MatrixMultiply(MatrixRotate((Vector3){1, 0, 0}, -0.5*PI), MatrixScale(size.x, size.y, 1));
    DrawModel(planeModel, (Vector3){ center.x, center.y, -0.1 }, 1, c);
    UnloadTexture(texture);
    UnloadModel(planeModel);
    rlPopMatrix();
    int t_id;
    if (scm_is_true(texture_id)){
        t_id = scm_to_int(texture_id);
    } else {
        t_id = -1;
    }
    Texture2D texture = t_id == -1? GetTextureDefault() : textures[t_id];
    draw_plane(
        scm_to_double(x),
        scm_to_double(y),
        scm_to_double(width),
        scm_to_double(height),
        c,
        texture
    );
    */
    return SCM_BOOL_T;
}

SCM
draw_text_scm(SCM text, SCM x, SCM y, SCM font_size, SCM color_vec)
{
    Color c;
    c.r = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 0) );
    c.g = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 1) );
    c.b = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 2) );
    c.a = scm_to_int( SCM_SIMPLE_VECTOR_REF(color_vec, 3) );
    char* txt = scm_to_utf8_stringn(text, NULL);
    DrawText(txt, scm_to_int(x),scm_to_int(y), scm_to_int(font_size), c);
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
    scm_c_define_gsubr("draw-rect*", 6, 0, 0, draw_rect_scm);
    scm_c_define_gsubr("draw-text*", 5, 0, 0, draw_text_scm);
    /*
    scm_c_define_gsubr("push-transform", 0, 0, 0, push_transform);
    scm_c_define_gsubr("pop-transform", 0, 0, 0, pop_transform);
    scm_c_define_gsubr("transform-identity", 0, 0, 0, transform_identity);
    scm_c_define_gsubr("transform-translate", 2, 0, 0, transform_translate);
    scm_c_define_gsubr("transform-scale", 1, 0, 0, transform_scale);
    scm_c_define_gsubr("transform-rotate", 1, 0, 0, transform_rotate);
    */
    scm_c_define_gsubr("apply-transform", 1, 0, 0, apply_transform);


    scm_c_define_gsubr("load-shader", 2, 0, 0, load_shader);
    scm_c_define_gsubr("load-texture", 1, 0, 0, load_texture);
    scm_c_define_gsubr("get-shader-loc", 2, 0, 0, get_shader_loc);
    scm_c_define_gsubr("set-shader-value", 3, 0, 0, set_shader_value_vec_float);
    scm_c_define_gsubr("set-shader-value-texture", 3, 0, 0, set_shader_value_texture);
    scm_c_define_gsubr("push-pp-texture", 0, 0, 0, push_pp_texture);
    scm_c_define_gsubr("pop-pp-texture", 0, 0, 0, pop_pp_texture);
    scm_c_define_gsubr("begin-pp-chain", 0, 0, 0, begin_pp_chain);
    scm_c_define_gsubr("pp-chain-next", 1, 0, 0, pp_chain_next);



    scm_c_export(
            "frame-time", "total-time", 
            "draw-rect*", "draw-text*", 
            //"push-transform", "pop-transform", "transform-identity", "transform-translate", "transform-scale", "transform-rotate",
            "apply-transform",
            "load-shader", "get-shader-loc", "set-shader-value", "set-shader-value-texture",
            "load-texture",
            "push-pp-texture", "pop-pp-texture", "begin-pp-chain", "pp-chain-next",
            NULL);
}

void init_guile_fn() {
    scm_c_define_module ("chart base native", init_chart_base_native_module, NULL);  
}

void* main2(void* args) {
    char* schemeFileName = (char*) args;
    init_guile_fn();
    SetConfigFlags(FLAG_MSAA_4X_HINT);
    InitWindow(800, 450, "Test");
    ppTextures[0] = LoadRenderTexture(800, 450);
    ppTextures[1] = LoadRenderTexture(800, 450);
    init_plane_model();

    SetTargetFPS(60);
    scm_c_primitive_load(schemeFileName);
    SCM update, render, init, data, new_data, key_press;
    update = scm_variable_ref(scm_c_lookup("update"));
    init = scm_variable_ref(scm_c_lookup("init-data"));
    key_press = scm_variable_ref(scm_c_lookup("key-press"));
    render = scm_variable_ref(scm_c_lookup("render"));
    data = scm_gc_protect_object(scm_call_0(init));
    while (!WindowShouldClose()) {
        int key = GetKeyPressed();
        if (key != 0 && key != -1) {
            new_data = scm_call_2(key_press, scm_from_int(key), data);
            scm_gc_protect_object(new_data);
            scm_gc_unprotect_object(data);
            data = new_data;
        }
        BeginDrawing();
        ClearBackground(RAYWHITE);
        scm_call_1(render, data);
        EndDrawing();
        new_data = scm_call_2(update, scm_from_double(GetFrameTime()), data);
        scm_gc_protect_object(new_data);
        scm_gc_unprotect_object(data);
        data = new_data;
    }
    return 0;
}

int main(int argc, char* args[]) {
    if (argc < 2) {
        printf("Expected first parameter to be scheme script file name. Exiting\n");
        return 1;
    }
    scm_with_guile(main2, args[1]);
    return 0;
}
