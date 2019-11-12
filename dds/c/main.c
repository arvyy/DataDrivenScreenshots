/*
#include "base.h"
#include "./raylib/raylib.h"
#include "./raylib/raymath.h"
#include "./raylib/rlgl.h"
#include <libguile.h>
#include <GL/gl.h>
#include <sys/inotify.h>
#include <poll.h>
#include <fcntl.h>

//loaded script name
char* script;
//load script's dir name
char* scriptDir;

//inotify file descriptor and watch descriptor.
int notify_fd;
int notify_wd;

int fps;
int paused;
int currentRecFrame;
int recording;

char* version = "0.1";

void loadBootstrap() {
    scm_c_primitive_load("dds/guile/preamble.scm");
}

void loadScript() {
    scm_c_primitive_load(script);
}

int scriptChanged() {
    struct pollfd pfd = { notify_fd, POLLIN, 0 };
    int ret = poll(&pfd, 1, 0);
    if (ret < 0) {
        perror("poll");
    } else if (ret == 0) {
    } else {
        for (;;) {
            struct inotify_event *event;
            char buffer[4000];
            int bytesRead = read(notify_fd, buffer, sizeof(buffer));
            if (bytesRead < 0) {
                perror("read");
            }
            if (bytesRead <= 0) break;
            int returnValue = 0;
            for (int i = 0; i < bytesRead; i += sizeof(struct inotify_event) + event->len) {
                event = (struct inotify_event*) &buffer[i];
                if ((event->mask & (IN_MODIFY | IN_MOVE | IN_CREATE | IN_CLOSE_WRITE))) {
                    int script_fd = open(script, O_RDONLY);
                    pfd.fd = script_fd;
                    ret = poll(&pfd, 1, 200);
                    if (ret < 0) {
                        perror("poll");
                    }
                    close(script_fd);
                    return ret || returnValue;
                }
                if (event->mask & IN_IGNORED) {
                    close(notify_fd);
                    notify_fd = inotify_init();
                    if (notify_fd < 0) {
                        perror("inotify_init");
                    }
                    notify_wd = inotify_add_watch(notify_fd, scriptDir, (IN_MOVE | IN_MODIFY | IN_CREATE | IN_CLOSE_WRITE));
                }
            }
        };
    }
    return 0;
}

void updateProtected(SCM* data, SCM value) {
    scm_gc_protect_object(value);
    scm_gc_unprotect_object(*data);
    *data = value;
}

void stopRecording() {
    recording = 0;
    SetTargetFPS(fps);
}

void startRecording() {
    recording = 1;
    paused = 0;
    currentRecFrame = 0;
    SetTargetFPS(0);
}

SCM
call_update(SCM update_fn, float delta, SCM data)
{
    return scm_call_2(update_fn, scm_from_double(delta), data);
}

void* main2(void* args) {
    Color bgColor;
    int maxFrames;
    int loop;
    char* outputFolder = NULL;
    float elapsedTime = 0;
    int showDebug = 1;
    float speed = 1;
    recording = 0;
    paused = 0;
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_VSYNC_HINT | FLAG_WINDOW_HIDDEN);
    InitWindow(100, 100, "DDS");
    loadBootstrap();
    loadScript();
    int width = scm_to_int(scm_variable_ref(scm_c_lookup("width")));
    int height = scm_to_int(scm_variable_ref(scm_c_lookup("height")));
    setWidth(width);
    setHeight(height);
    initAfterRL();
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

    SetTargetFPS(fps);
    SCM update, render, init, data, new_data, format, stop, output_frame, init_render;
    update = scm_variable_ref(scm_c_lookup("update"));
    init = scm_variable_ref(scm_c_lookup("init-data"));
    init_render = scm_variable_ref(scm_c_lookup("init-render"));
    render = scm_variable_ref(scm_c_lookup("render"));
    format = scm_variable_ref(scm_c_lookup("format"));
    stop = scm_variable_ref(scm_c_lookup("stop?"));
    output_frame = scm_variable_ref(scm_c_lookup("output-frame"));
    data = scm_gc_protect_object(scm_call_0(init));
    BeginDrawing();
    scm_call_0(init_render);
    EndDrawing();
    while (!WindowShouldClose()) {
        int key = GetKeyPressed();
        if (key == KEY_R || key == KEY_R + 32 || scriptChanged()) {
            loadScript();
        } else if (key == KEY_SPACE) {
            paused = !paused;
        } else if (key == KEY_ZERO) {
            updateProtected(&data, scm_call_0(init));
            BeginDrawing();
            scm_call_0(init_render);
            EndDrawing();
            elapsedTime = 0;
        } else if (key == KEY_W || key == KEY_W + 32) {
            elapsedTime += 1;
            updateProtected(&data, call_update(update, 1, data));
        } else if (key == KEY_S || key == KEY_S + 32) {
            elapsedTime -= 1;
            if (elapsedTime < 0)
                elapsedTime = 0;
            updateProtected(&data, scm_call_0(init));
            updateProtected(&data, call_update(update, elapsedTime, data));
        } else if (key == KEY_Q || key == KEY_Q + 32) {
            speed = speed < 8? speed * 2 : 8;
        } else if (key == KEY_A || key == KEY_A + 32) {
            speed = speed > 0.125? speed / 2 : 0.125;
        } else if (key == KEY_D || key == KEY_D + 32) {
            showDebug = !showDebug;
        } else if (key == KEY_G || key == KEY_G + 32){
            if (!recording) {
                startRecording();
            } else {
                stopRecording();
            }
        }
        BeginDrawing();
        ClearBackground(bgColor);
        scm_call_1(render, data);
        rlglDraw();
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        if (showDebug && !recording) {
            rlglDraw();
            rlLoadIdentity();
            DrawText(TextFormat("Time: %f", elapsedTime), 20, 20, 10, BLACK);
            DrawText(TextFormat("Speed: %fx", speed), 20, 30, 10, BLACK);
            if (paused) {
                DrawText("Paused", 20, 40, 10, BLACK);
            }
        }
        EndDrawing();
        if (recording) {
            char* outputFrame = scm_to_utf8_stringn(scm_call_2(output_frame, scm_from_locale_string(script), scm_from_int(currentRecFrame)), NULL);
            rlglDraw();
            currentRecFrame++;
            const char* ssName = TextFormat("%s/%s", outputFolder, outputFrame);
            TakeScreenshot(ssName);
            free(outputFrame);
        }
        if (!paused) {
            float frameTime = !recording? (GetFrameTime() * speed) : (1.0 / fps);
            elapsedTime += frameTime;
            updateProtected(&data, call_update(update, frameTime, data));
        }
        int should_stop = scm_is_true(scm_call_1(stop, data)); 
        if (recording) {
            if (should_stop || currentRecFrame > maxFrames) {
                stopRecording();
            }
        } else if (should_stop && loop) {
            updateProtected(&data, scm_call_0(init));
            BeginDrawing();
            scm_call_0(init_render);
            EndDrawing();
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
    int scriptArgsCount = argc - 2;
    char** scriptArgs = &args[2];
    setScriptArgs(scriptArgs, scriptArgsCount);
    char* lastSlash = strrchr(script, '/');
    if (lastSlash == NULL) {
        scriptDir = "./";
    } else {
        int len = (lastSlash - script) / sizeof (char);
        scriptDir = calloc(len + 1, sizeof(char));
        strncpy(scriptDir, script, len);
    }
    notify_fd = inotify_init();
    if (notify_fd < 0) {
        perror("inotify_init");
    }
    notify_wd = inotify_add_watch(notify_fd, scriptDir, (IN_MOVE | IN_MODIFY | IN_CREATE | IN_CLOSE_WRITE));
    scm_with_guile(main2, NULL);
    return 0;
}*/
