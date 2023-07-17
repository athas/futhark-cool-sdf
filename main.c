#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <pthread.h>

#define PROGHEADER "lys.h"
#include "lib/github.com/diku-dk/lys/liblys.h"

#include "expr.h"

#include <unistd.h>
#include <getopt.h>

#define MAX_FPS 60

struct internal {
  bool show_text;
  uint64_t* program;
  int program_len;
  pthread_mutex_t mutex;
};

void load_program(struct lys_context *ctx, struct internal *internal) {
  struct futhark_u64_1d *program_arr = futhark_new_u64_1d(ctx->fut, internal->program, internal->program_len);
  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_set_program(ctx->fut, &new_state, program_arr, ctx->state));
  FUT_CHECK(ctx->fut, futhark_context_sync(ctx->fut));
  FUT_CHECK(ctx->fut, futhark_free_opaque_state(ctx->fut, ctx->state));
  ctx->state = new_state;
  FUT_CHECK(ctx->fut, futhark_free_u64_1d(ctx->fut, program_arr));
  free(internal->program);
  internal->program = NULL;
}

void loop_iteration(struct lys_context *ctx, struct internal *internal) {
  pthread_mutex_lock(&internal->mutex);
  if (internal->program != NULL) {
    load_program(ctx, internal);
  }
  pthread_mutex_unlock(&internal->mutex);
}

void handle_event(struct lys_context *ctx, enum lys_event event) {
  struct internal *internal = (struct internal *) ctx->event_handler_data;
  switch (event) {
  case LYS_LOOP_ITERATION:
    loop_iteration(ctx, internal);
    break;
  default:
    return;
  }
}

const char* default_expr = "(1+sin(u*20*3+t)*sin(t))/2 + (1+cos(v*20*3+t)*sin(t))/2";

void* ui_thread_fn(void* arg) {
  struct internal* internal = arg;

  char* line = NULL;
  size_t bufsize;
  ssize_t linelen;

  printf("> ");
  fflush(stdout);
  while ((linelen = getline(&line, &bufsize, stdin)) != -1) {
    line[linelen-1] = 0;
    struct expr *e = parse_expr(line);

    if (e != NULL) {
      pthread_mutex_lock(&internal->mutex);
      free(internal->program);
      internal->program = encode_expr(e, &internal->program_len);
      pthread_mutex_unlock(&internal->mutex);
      free_expr(e);
    }
    printf("> ");
    fflush(stdout);
  }
  printf("\n");
  exit(0);
}


static void run_interactive(struct futhark_context *futctx,
                            int width, int height, int seed,
                            bool show_text_initial) {
  struct lys_context ctx;
  lys_setup(&ctx, width, height, MAX_FPS, 0);
  ctx.fut = futctx;

  ctx.event_handler_data = NULL;
  ctx.event_handler = handle_event;

  struct internal internal;
  internal.show_text = show_text_initial;
  pthread_mutex_init(&internal.mutex, NULL);

  struct expr *e = parse_expr(default_expr);
  assert(e != NULL);
  internal.program = encode_expr(e, &internal.program_len);
  free_expr(e);

  int err = 0;

  err |= futhark_entry_init(ctx.fut, &ctx.state, seed, height, width);

  load_program(&ctx, &internal);

  err |= futhark_context_sync(ctx.fut);

  if (err != 0) {
    char* errmsg = futhark_context_get_error(ctx.fut);
    fprintf(stderr, "Error during initialisation:\n%s", errmsg);
    free(errmsg);
    return;
  }

  ctx.event_handler_data = (void*) &internal;

  pthread_t ui_thread;
  pthread_create(&ui_thread, NULL, ui_thread_fn, &internal);

  lys_run_sdl(&ctx);
}

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  struct futhark_context_config *futcfg;
  struct futhark_context *futctx;
  futcfg = futhark_context_config_new();
  #ifdef FUTHARK_BACKEND_opencl
  futhark_context_config_select_device_interactively(futcfg);
  #endif
  assert(futcfg != NULL);
  futhark_context_config_set_cache_file(futcfg, "main.cache");
  futctx = futhark_context_new(futcfg);

  int width = 400, height = 400;
  int seed = 1337;
  bool show_text = 0;
  run_interactive(futctx, width, height, seed, show_text);

  futhark_context_free(futctx);
  futhark_context_config_free(futcfg);

  return EXIT_SUCCESS;
}
