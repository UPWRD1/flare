#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// ---------------------------------------------------------------------------
// Handle table — maps i64 handle -> void*
// ---------------------------------------------------------------------------

#define MAX_HANDLES 4096
#define INVALID_HANDLE 0

static void *handle_table[MAX_HANDLES];
static int64_t next_handle = 1; // 0 is reserved as INVALID_HANDLE

static int64_t handle_alloc(void *ptr) {
    if (!ptr || next_handle >= MAX_HANDLES) return INVALID_HANDLE;
    int64_t h = next_handle++;
    handle_table[h] = ptr;
    return h;
}

static void *handle_get(int64_t h) {
    if (h <= 0 || h >= MAX_HANDLES) return NULL;
    return handle_table[h];
}

static void handle_free(int64_t h) {
    if (h > 0 && h < MAX_HANDLES)
        handle_table[h] = NULL;
}

// ---------------------------------------------------------------------------
// printf / fprintf
// ---------------------------------------------------------------------------

// shim_printf(fmt: i8*) -> i64
// Wraps printf. Format string comes straight from your @.str global.
// Returns number of characters written (or -1 on error).
int64_t shim_printf(const char *fmt) {
    return (int64_t)printf("%s", fmt);
}

// shim_printf_i64(fmt: i8*, val: i64) -> i64
// For printing a single integer. Extend similarly for other types.
int64_t shim_printf_i32(const char *fmt, int32_t val) {
    return (int64_t)printf("%d", val);
}

// shim_printf_f64(fmt: i8*, val: double) -> i64
int64_t shim_printf_f32(float val) {
    return (int64_t)printf("%f", val);
}

// shim_printf_str(fmt: i8*, s: i8*) -> i64
int64_t shim_printf_str(const char *fmt, const char *s) {
    return (int64_t)printf(fmt, s);
}

// shim_fprintf(file_handle: i64, fmt: i8*) -> i64
int64_t shim_fprintf(int64_t fh, const char *fmt) {
    FILE *f = (FILE *)handle_get(fh);
    if (!f) return -1;
    return (int64_t)fprintf(f, "%s", fmt);
}

// shim_fprintf_i64(file_handle: i64, fmt: i8*, val: i64) -> i64
int64_t shim_fprintf_i64(int64_t fh, const char *fmt, int64_t val) {
    FILE *f = (FILE *)handle_get(fh);
    if (!f) return -1;
    return (int64_t)fprintf(f, fmt, val);
}

// ---------------------------------------------------------------------------
// File open / close / read / write
// ---------------------------------------------------------------------------

// shim_fopen(path: i8*, mode: i8*) -> i64  (handle, or 0 on failure)
int64_t shim_fopen(const char *path, const char *mode) {
    FILE *f = fopen(path, mode);
    return handle_alloc(f); // returns 0 (INVALID_HANDLE) if fopen failed
}

// shim_fclose(handle: i64) -> i64  (0 = success, -1 = error)
int64_t shim_fclose(int64_t fh) {
    FILE *f = (FILE *)handle_get(fh);
    if (!f) return -1;
    int r = fclose(f);
    handle_free(fh);
    return (int64_t)r;
}

// shim_fwrite(buf_handle: i64, n: i64, file_handle: i64) -> i64
// Writes n bytes from a buffer handle to a file handle.
int64_t shim_fwrite(int64_t buf_h, int64_t n, int64_t fh) {
    void *buf = handle_get(buf_h);
    FILE *f   = (FILE *)handle_get(fh);
    if (!buf || !f) return -1;
    return (int64_t)fwrite(buf, 1, (size_t)n, f);
}

// shim_fread(buf_handle: i64, n: i64, file_handle: i64) -> i64
// Reads up to n bytes into a buffer handle. Returns bytes read.
int64_t shim_fread(int64_t buf_h, int64_t n, int64_t fh) {
    void *buf = handle_get(buf_h);
    FILE *f   = (FILE *)handle_get(fh);
    if (!buf || !f) return -1;
    return (int64_t)fread(buf, 1, (size_t)n, f);
}

// Convenience: get handles for stdin/stdout/stderr
int64_t shim_stdin()  { return handle_alloc(stdin);  }
int64_t shim_stdout() { return handle_alloc(stdout); }
int64_t shim_stderr() { return handle_alloc(stderr); }

// ---------------------------------------------------------------------------
// getline / fgets
// ---------------------------------------------------------------------------

// shim_getline(file_handle: i64) -> i64  (handle to a malloc'd buffer, or 0)
// Caller must shim_buf_free() the returned handle when done.
int64_t shim_getline(int64_t fh) {
    FILE *f = (FILE *)handle_get(fh);
    if (!f) return INVALID_HANDLE;
    char *line = NULL;
    size_t cap = 0;
    ssize_t n = getline(&line, &cap, f);
    if (n < 0) { free(line); return INVALID_HANDLE; }
    return handle_alloc(line);
}

// shim_fgets(buf_handle: i64, n: i64, file_handle: i64) -> i64
// Fills an existing buffer handle; returns the same handle, or 0 on EOF/error.
int64_t shim_fgets(int64_t buf_h, int64_t n, int64_t fh) {
    char *buf = (char *)handle_get(buf_h);
    FILE *f   = (FILE *)handle_get(fh);
    if (!buf || !f) return INVALID_HANDLE;
    return fgets(buf, (int)n, f) ? buf_h : INVALID_HANDLE;
}

// ---------------------------------------------------------------------------
// malloc / free wrappers
// ---------------------------------------------------------------------------

// shim_malloc(n: i64) -> i64  (handle, or 0 on failure)
int64_t shim_malloc(int64_t n) {
    void *p = malloc((size_t)n);
    return handle_alloc(p);
}

// shim_free(handle: i64) -> i64  (always 0)
int64_t shim_free(int64_t h) {
    void *p = handle_get(h);
    free(p);
    handle_free(h);
    return 0;
}

// shim_buf_to_str(buf_handle: i64) -> i8*
// Lets you pass a buffer back to a shim function expecting i8*.
// Use with care — the pointer is only valid while the handle is live.
const char *shim_buf_to_str(int64_t h) {
    return (const char *)handle_get(h);
}
