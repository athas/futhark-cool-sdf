#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#define PROGHEADER "lys.h"
#include "lib/github.com/diku-dk/lys/liblys.h"

#include "expr.h"

#include <unistd.h>
#include <getopt.h>

#define MAX_FPS 60

struct internal {
  bool show_text;
};

void loop_iteration(struct lys_context *ctx, struct internal *internal) {
  (void)ctx;
  (void)internal;
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

static void run_interactive(struct futhark_context *futctx,
                            int width, int height, int seed,
                            bool show_text_initial) {
  struct lys_context ctx;
  lys_setup(&ctx, width, height, MAX_FPS, 0);
  ctx.fut = futctx;

  ctx.event_handler_data = NULL;
  ctx.event_handler = handle_event;

  struct expr *e = parse_expr("(1+sin(u*20*3+t)*sin(t))/2 + (1+cos(v*20*3+t)*sin(t))/2");
  assert(e != NULL);

  int len;
  int err;
  uint32_t* program = encode_expr(e, &len);

  struct futhark_u32_1d *program_arr = futhark_new_u32_1d(ctx.fut, program, len);
  err = futhark_entry_init(ctx.fut, &ctx.state, program_arr, seed, height, width);
  err |= futhark_context_sync(ctx.fut);

  if (err != 0) {
    char* errmsg = futhark_context_get_error(ctx.fut);
    fprintf(stderr, "Error during initialisation:\n%s", errmsg);
    free(errmsg);
    return;
  }

  struct internal internal;
  ctx.event_handler_data = (void*) &internal;
  internal.show_text = show_text_initial;

  lys_run_sdl(&ctx);
}

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  char *deviceopt = NULL;
  bool device_interactive = false;
  struct futhark_context_config *futcfg;
  struct futhark_context *futctx;
  char* opencl_device_name;
  lys_setup_futhark_context(deviceopt, device_interactive,
                            &futcfg, &futctx, &opencl_device_name);

  int width = 400, height = 400;
  int seed = 1337;
  bool show_text = 0;
  run_interactive(futctx, width, height, seed, show_text);

  futhark_context_free(futctx);
  futhark_context_config_free(futcfg);

  return EXIT_SUCCESS;
}
