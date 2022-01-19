/* -*- Mode:C; coding:iso-8859-1; indent-tabs-mode:nil; tab-width:8; -*- */
/* Copyright (C) 2020, RISE Research Institutes of Sweden AB. */

/*
Usage: ./jsonrpc_client [sicstus_path [ jsonrpc_server.pl_path ] ]

The program starts the sicstus sub-process and sends some requests to
Prolog. Finally it tells the sicstus sub-process to exit, and quits.

*/

/*-
  Example transcripts

  On macOS/Linux, assuming sicstus is on PATH

     $ SRC_DIR="$( sicstus --goal "absolute_file_name(library('jsonrpc/clients'),D), write(D), nl, halt." 2>/dev/null )"
     $ mkdir json_test
     $ cd json_test
     $ cc "${SRC_DIR}/jsonrpc_client.c" -o jsonrpc_client_c
     $ ls
     jsonrpc_client_c
     $ ./jsonrpc_client_c 
     state ==> State is null
     state:=4 ==> State was null
     state ==> State is 4

     once(Result is StateIn+1, StateOut=Result). ==> Result=5
     once(Result is StateIn+1, StateOut=Result). ==> Result=6

     Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> Result=11

     Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
     retry ==> (next) Result=211
     cut ==> Result=null

     Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
     retry ==> (next) Result=211
     retry ==> (next) Result=311
     retry ==> Prolog failed (this is expected)

     once(foo is bar). ==> Prolog threw an exception (this is expected)

     quit ==> Result="Bye"
     $ 

  Windows transcript:

  FIXME: Windows Instructions.
   . ./env.sh && cl /MD /Zi /RTCs /RTCc /RTCu /GS /MD /WX /W3  /Gr -D_UNICODE -DLOGGING=1 library/jsonrpc/jsonrpc_client.c
*/
#if (defined(_WIN32) && !defined(__CYGWIN__))
/* Windows API */
#define USE_WINDOWS_API 1

/* If either _UNICODE or UNICODE are defined, then both should be
   defined. This must be done before including any headers. */
#if defined(_UNICODE)
#undef UNICODE
#define UNICODE 1
#endif  /* defined(_UNICODE) */
#if defined(UNICODE)
#undef _UNICODE
#define _UNICODE 1
#endif  /* defined(UNICODE) */

#if defined(UNICODE)
#define USE_WINDOWS_UNICODE 1
#endif  /* defined(UNICODE) */

#else /* !(defined(_WIN32) && !defined(__CYGWIN__)) */
#define USE_POSIX_API 1
#endif /* !(defined(_WIN32) && !defined(__CYGWIN__)) */

#if !defined (LOGGING)
/* The default logging level (1 (0) means on (off)). */
#define LOGGING 0
#endif  /* !defined (LOGGING) */

/* Ensure LOGGING is always defined to 0 if undefined */
#if !LOGGING
#undef LOGGING
#define LOGGING 0
#endif  /* !LOGGING */

#if USE_POSIX_API
#include <unistd.h>
#include <fcntl.h>
/* Dummy version of the Windows Unicode functionality */
#define TEXT(X) X
#define TCHAR char
#endif  /* USE_POSIX_API */

#if USE_WINDOWS_API
#include <windows.h>            /* Defines TEXT and TCHAR, but not _TEXT/_TCHAR, if UNICODE is defined */
#include <io.h>                 /* _open_osfhandle() */
#include <tchar.h>              /* _tmain */
#endif  /* USE_WINDOWS_API */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* The code used by jsonrpc_server.pl to indicate failure. The code is
   passed, in an "error object", as a number. The constant is the
   (unique) JSON representation av that code. */
#define FAILURE_CODE_STRING "-4711"
/* The code used by jsonrpc_server.pl to indicate a Prolog exception. */
#define EXCEPTION_CODE_STRING "-4712"


#if !defined(DEFINE_JSON_PARSER)
#define DEFINE_JSON_PARSER 1
#endif  /* !defined(DEFINE_JSON_PARSER) */

#if DEFINE_JSON_PARSER
/* Simplistic JSON tokenizer/parser.

   Does not do any validation, i.e. it assumes that incoming JSON is well formed.

   This is just for demonstrational purposes. There are many capable
   (and free) libraries that parse/tokenize JSON properly (see,
   e.g. the list at json.org).
*/

enum json_kind {
  /* JSON_KIND_UNKNOWN is used for indicating syntax error, and also
     other failures (e.g. no-such-member in an object).  When
     JSON_KIND_UNKNOWN is returned, the start and end pointer have
     unspecified values. */
  JSON_KIND_UNKNOWN,

  JSON_KIND_LEFT_SQUARE_BRACKET,
#define JSON_KIND_ARRAY JSON_KIND_LEFT_SQUARE_BRACKET
  JSON_KIND_RIGHT_SQUARE_BRACKET,
  JSON_KIND_LEFT_CURLY_BRACKET,
#define JSON_KIND_OBJECT JSON_KIND_LEFT_CURLY_BRACKET
  JSON_KIND_RIGHT_CURLY_BRACKET,
  JSON_KIND_COLON,
  JSON_KIND_COMMA,

  JSON_KIND_TRUE,
  JSON_KIND_FALSE,
  JSON_KIND_NULL,

  JSON_KIND_STRING,
  JSON_KIND_NUMBER
};

static
char const *json_skip_whitespace(char const *buf)
{
  char const *p;

  if (buf == NULL) {
    return buf;
  }

  for (p=buf;;p++) {
    switch (*p) {
    case 0x09:          /* tabulation */
    case 0x0A:          /* line feed */
    case 0x0D:          /* carriage return */
    case 0x20:          /* space */
      continue;
    default:
      return p;
    }
  }
}

/* Returns JSON_KIND_UNKNOWN, to indicate that something went wrong. */
static
enum json_kind at_unknown(char const *p, char const **pstart, char const **pend)
{
  *pstart = p;
  *pend = p;

  return JSON_KIND_UNKNOWN;
}

/* Return kind and extent of the specified literal. */
static
enum json_kind at_literal(char const *buf, char const *literal, enum json_kind kind, char const **pstart, char const **pend)
{
  char const *p;
  char const *q;

  for (p = buf, q = literal; ; p++,q++) {
    char cp = *p;
    char cq = *q;

    if (cq == '\0') {
      *pstart = buf;
      *pend = p;
      return kind;
    }

    if (cp != cq) {
      return at_unknown(buf, pstart, pend);
    }
  }
}

/* Return kind and extent of the string that starts at buf
   (i.e. buf[0] must be the opening double-quote). */
static
enum json_kind at_string(char const *buf, char const **pstart, char const **pend)
{
  char const *p = buf;

  if (*p != '"') {
    return at_unknown(buf, pstart, pend);
  }

  p++;

  for (;;) {
    char c = *p;

    switch (c) {
    case '"':
      p++;

      *pstart = buf;
      *pend = p;
      return JSON_KIND_STRING;

    case '\\':
      p++;
      switch (*p) {
      case 'u':
        /* Expect "\uXXXX" but we do not validate and just skip the "\u". */
        /* FALLTHROUGH */
      case '"':
      case '\\':
      case '/':
      case 'b':
      case 'f':
      case 'n':
      case 'r':
      case 't':
        p++;
        break;

      case '\0':
      default:
        return at_unknown(buf, pstart, pend);
      }
      break;

    case '\0':
    case 0x09:          /* tabulation */
    case 0x0A:          /* line feed */
    case 0x0D:          /* carriage return */
      return at_unknown(buf, pstart, pend);

    default:
      /* Control characters are not allowed in strings. */
      if (c <= 0x1F) {
        return at_unknown(buf, pstart, pend);
      }

      p++;
    }
  }
}

static
enum json_kind at_exponent(char const *buf, char const **pstart, char const **pend)
{
  char const *p = buf;
  char const *q;
  if (*p != 'e' && *p != 'E') {
    return at_unknown(buf, pstart, pend);
  }
  p++;
  if (*p == '-' || *p == '+') {
    p++;
  }
  q = p;
  for (;;) {
    switch (*p) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      p++;
      break;
    default:
      break;
    }
  }
  if (p == q) {
    /* no digits after "E" */
    return at_unknown(buf, pstart, pend);
  }

  *pstart = buf;
  *pend = p;
  return JSON_KIND_NUMBER;
}

static
enum json_kind at_fraction(char const *buf, char const **pstart, char const **pend)
{
  char const *p = buf;
  char const *q;
  if (*p != '.') {
    return at_unknown(buf, pstart, pend);
  }

  p++;
  q = p;
  for (;;) {
    switch (*p) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      p++;
      break;
    default:
      break;
    }
  }
  if (p == q) {
    /* no digits after "." */
    return at_unknown(buf, pstart, pend);
  }
  if (*p == 'e' || *p == 'E') {
    return at_exponent(p, pstart, pend);
  }

  *pstart = buf;
  *pend = p;
  return JSON_KIND_NUMBER;
}

/* Return kind and extent of the number that starts at buf
   (i.e. buf[0] must be a digit, or a minus sign). */
static
enum json_kind at_number(char const *buf, char const **pstart, char const **pend)
{
  char const *p = buf;
  char const *q;

  if (*p == '-') {
    p++;
  }

  q = p;
  for (;;) {
    char c = *p;

    switch (c) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      p++;
      break;
    case '.':
      at_fraction(p, pstart, pend);
    case 'e':
    case 'E':
      return at_exponent(p, pstart, pend);

    case '\0':
    default:
      if (p == q) {
        /* No digits */
        return at_unknown(buf, pstart, pend);
      }
      *pstart = buf;
      *pend = p;

      return JSON_KIND_NUMBER;
    }
  }
}

/* Return kind and extent of the token that starts at buf (i.e. buf[0]
   must be the start of the token. */
static
enum json_kind json_at_token(char const *buf, char const **pstart, char const **pend)
{
  enum json_kind kind;
  char const *p = buf;
  switch (*p) {
  case '[':
    kind = JSON_KIND_LEFT_SQUARE_BRACKET;
    break;
  case ']':
    kind = JSON_KIND_RIGHT_SQUARE_BRACKET;
    break;
  case '{':
    kind = JSON_KIND_LEFT_CURLY_BRACKET;
    break;
  case '}':
    kind = JSON_KIND_RIGHT_CURLY_BRACKET;
    break;
  case ':':
    kind = JSON_KIND_COLON;
    break;
  case ',':
    kind = JSON_KIND_COMMA;
    break;

  case 't':
    {
      return at_literal(p, "true", JSON_KIND_TRUE, pstart, pend);
    }
  case 'f':
    {
      return at_literal(p, "false", JSON_KIND_FALSE, pstart, pend);
    }
  case 'n':
    {
      return at_literal(p, "null", JSON_KIND_NULL, pstart, pend);
    }

  case '"':
    {
      return at_string(p, pstart, pend);
    }

  case '-':
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    {
      return at_number(p, pstart, pend);
    }

  default:
    return at_unknown(p, pstart, pend);
  }

  /* Gets here for single char tokens. */
  *pstart = p;
  *pend = p+1;
  return kind;
}

/* Skip whitespace and return kind and extent of next token. */
static
enum json_kind json_next_token(char const *buf, char const **pstart, char const **pend)
{
  return json_at_token(json_skip_whitespace(buf), pstart, pend);
}

/* Return kind and extent of the value that starts at buf (i.e. buf[0]
   must be the start of the first token of the value. */
static
enum json_kind json_at_value(char const *buf, char const **pstart, char const **pend)
{
  char const *p = buf;
  char const *start;
  char const *end;
  enum json_kind kind;
  switch ((kind = json_at_token(p, &start, &end))) {
  case JSON_KIND_LEFT_SQUARE_BRACKET:
    {
      size_t nesting = 1;

      do {
        p = end;
        kind = json_at_token(p, &start, &end);
        switch (kind) {
        case JSON_KIND_LEFT_SQUARE_BRACKET:
          nesting ++;
          break;
        case JSON_KIND_RIGHT_SQUARE_BRACKET:
          nesting--;
          break;
        case JSON_KIND_UNKNOWN:
          return at_unknown(buf, pstart, pend);
        default:
          break;
        }
      } while (nesting != 0);
      *pstart = buf;
      *pend = end;
      return JSON_KIND_ARRAY;
    }
    break;
  case JSON_KIND_LEFT_CURLY_BRACKET:
    {
      size_t nesting = 1;

      do {
        p = end;
        kind = json_at_token(p, &start, &end);
        switch (kind) {
        case JSON_KIND_LEFT_CURLY_BRACKET:
          nesting ++;
          break;
        case JSON_KIND_RIGHT_CURLY_BRACKET:
          nesting--;
          break;
        case JSON_KIND_UNKNOWN:
          return at_unknown(buf, pstart, pend);
        default:
          break;
        }
      } while (nesting != 0);
      *pstart = buf;
      *pend = end;
      return JSON_KIND_OBJECT;
    }
    break;

  case JSON_KIND_TRUE:
  case JSON_KIND_FALSE:
  case JSON_KIND_NULL:
  case JSON_KIND_NUMBER:
  case JSON_KIND_STRING:
    *pstart = start;
    *pend = end;
    return kind;

  default:
    return at_unknown(p, pstart, pend);
  }


  return JSON_KIND_UNKNOWN;
}

/* Skip whitespace and return kind and extent of next value. */
static
enum json_kind json_next_value(char const *buf, char const **pstart, char const **pend)
{
  return json_at_value(json_skip_whitespace(buf), pstart, pend);
}

/* Skip whitespace and return the kind and extent of the value of the next member with the given name, if any.
   buf should point inside an object (after "{"; before a NAME:VALUE pair) */
static
enum json_kind json_lookup_next_value(char const *buf, char const *name, char const **pstart, char const **pend)
{
  size_t name_length = strlen(name);
  enum json_kind kind;
  char const *start;
  char const *end;
  char const *p = buf;

  for (;;) {
    /* Expect "  NAME:VALUE ..." or "}" */
    kind = json_next_token(p, &start, &end);
    if (kind == JSON_KIND_STRING) {
      char const *key_start = start+1; /* +1 skip initial double quote. */
      size_t key_length = (end-start)-2; /* -2 for surrounding double quotes. */

      kind = json_next_token(end, &start, &end);
      if (kind == JSON_KIND_COLON) {
        kind = json_next_value(end, &start, &end);
        if (name_length == key_length
            && strncmp(name, key_start, key_length) == 0) {
          /* Match */
          *pstart = start;
          *pend = end;
          return kind;
        } else {
          /* Wrong key, continue searching */
          kind = json_next_token(end, &start, &end);
          if (kind == JSON_KIND_COMMA) {
            p = end;
            continue;
          }
        }
      }
    }
    /* Not found */
    break;
  }
  /* Most likely at JSON_KIND_RIGHT_CURLY_BRACKET. */
  return at_unknown(buf, pstart, pend);
}

/* Skip whitespace and return the kind and extent of the value of the
   object next member with the given name, if any.

   buf should point at (whitespace before) "{". Return
   JSON_KIND_UNKNOWN if name is not found, or buf is not pointing at
   an object. */
static
enum json_kind json_lookup_member(char const *buf, char const *name, char const **pstart, char const **pend)
{
  char const *p = json_skip_whitespace(buf);
  char const *start;
  char const *end;

  if (json_next_token(p, &start, &end) == JSON_KIND_OBJECT) {
    return json_lookup_next_value(end, name, pstart, pend);
  }
  return at_unknown(p, pstart, pend);
}

#endif /* DEFINE_JSON_PARSER */


#if LOGGING
#define LOG_DO(X) do{X;}while(0)
#else  /* !LOGGING */
#define LOG_DO(X) do{;}while(0)
#endif  /* !LOGGING */


#if USE_WINDOWS_API
// Returns 0 on success, -1 on error.
static int win_launch(TCHAR *szCmd, TCHAR *szCmdline,
                      HANDLE *phChildStd_OUT_Rd,
                      // HANDLE *phChildStd_ERR_Rd,
                      HANDLE  *phChildStd_IN_Wr)
{
  int result;
  DWORD info = 0;
  SECURITY_ATTRIBUTES saAttr;
  HANDLE hChildStd_IN_Rd = INVALID_HANDLE_VALUE;
  HANDLE hChildStd_IN_Wr = INVALID_HANDLE_VALUE;
  HANDLE hChildStd_OUT_Rd = INVALID_HANDLE_VALUE;
  HANDLE hChildStd_OUT_Wr = INVALID_HANDLE_VALUE;
  // It would be easy to extend this code to pipe the standard error stream (or connect it to the NULL device).
  // HANDLE hChildStd_ERR_Rd = INVALID_HANDLE_VALUE;
  HANDLE hChildStd_ERR_Wr = INVALID_HANDLE_VALUE;
  HANDLE hTmp = INVALID_HANDLE_VALUE;


  // Set the bInheritHandle flag so pipe handles are inherited.
  saAttr.nLength = sizeof saAttr;
  saAttr.bInheritHandle = TRUE;
  saAttr.lpSecurityDescriptor = NULL;

  // Create a pipe for the child process's STDOUT.
  if (!CreatePipe(&hChildStd_OUT_Rd, &hChildStd_OUT_Wr, &saAttr, 0)) {
    hChildStd_OUT_Rd = INVALID_HANDLE_VALUE;
    hChildStd_OUT_Wr = INVALID_HANDLE_VALUE;
    fprintf(stderr, "%d: Failed CreatePipe, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    goto barf;
  }

  // Ensure the read handle to the pipe for STDOUT is not inherited.
  if (!SetHandleInformation(hChildStd_OUT_Rd, HANDLE_FLAG_INHERIT, 0)) {
    fprintf(stderr, "%d: Failed SetHandleInformation, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    goto barf;
  }

#if 0
  // Create a pipe for the child process's STDERR.
  if (!CreatePipe(&hChildStd_ERR_Rd, &hChildStd_ERR_Wr, &saAttr, 0)) {
    hChildStd_ERR_Rd = INVALID_HANDLE_VALUE;
    hChildStd_ERR_Wr = INVALID_HANDLE_VALUE;
    fprintf(stderr, "%d: Failed CreatePipe, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    goto barf;
  }

  // Ensure the read handle to the pipe for STDERR is not inherited.
  if (!SetHandleInformation(hChildStd_ERR_Rd, HANDLE_FLAG_INHERIT, 0)) {
    fprintf(stderr, "%d: Failed SetHandleInformation, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    goto barf;
  }

#else  /*  */
  // Make the child use our standard error stream.
  {
    HANDLE hParentStdErr = GetStdHandle(STD_ERROR_HANDLE);
    if (hParentStdErr == INVALID_HANDLE_VALUE) {
      fprintf(stderr, "%d: Failed GetStdHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
      goto barf;
    }
    // NULL is special.
    if (hParentStdErr == NULL) {
      fprintf(stderr, "%d: Failed GetStdHandle() (NULL)\n", (int)__LINE__);
      goto barf;
    }
    if (!GetHandleInformation(hParentStdErr, &info)) {
      fprintf(stderr, "%d: Failed GetHandleInformation, gle=%lx\n", (int)__LINE__, (long)GetLastError());
      goto barf;
    }
    if (info & HANDLE_FLAG_INHERIT) {
      // Already inheritable. Must not DuplicateHandle() it.
      hChildStd_ERR_Wr = hParentStdErr;
    } else {
      if (!DuplicateHandle(GetCurrentProcess(), hParentStdErr,
                           GetCurrentProcess(), &hTmp,
                           0,
                           // bInheritHandle
                           TRUE,
                           DUPLICATE_SAME_ACCESS))
        {
          fprintf(stderr, "%d: Failed DuplicateHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
          hTmp = INVALID_HANDLE_VALUE;
          goto barf;
        }
      hChildStd_ERR_Wr = hTmp;
    }
  }
#endif

  // Create a pipe for the child process's STDIN.
  if (!CreatePipe(&hChildStd_IN_Rd, &hChildStd_IN_Wr, &saAttr, 0)) {
    hChildStd_IN_Rd = INVALID_HANDLE_VALUE;
    hChildStd_IN_Wr = INVALID_HANDLE_VALUE;
    fprintf(stderr, "%d: Failed CreatePipe, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    goto barf;
  }
  // Ensure the write handle to the pipe for STDIN is not inherited.
  if (!SetHandleInformation(hChildStd_IN_Wr, HANDLE_FLAG_INHERIT, 0)) {
    fprintf(stderr, "%d: Failed SetHandleInformation, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    goto barf;
  }


#if USE_WINDOWS_UNICODE
  LOG_DO(fprintf(stderr, "CreateProcess(\"%S\", \"%S\")\n", szCmd, szCmdline));
#else  /* !USE_WINDOWS_UNICODE */
  LOG_DO(fprintf(stderr, "CreateProcess(\"%s\", \"%s\")\n", szCmd, szCmdline));
#endif  /* !USE_WINDOWS_UNICODE */

  // Create the child process.
  {
    PROCESS_INFORMATION piProcInfo;
    STARTUPINFO siStartInfo;
    BOOL bSuccess = FALSE;

    // Set up members of the PROCESS_INFORMATION structure.
    ZeroMemory(&piProcInfo, sizeof piProcInfo);

    // Set up members of the STARTUPINFO structure.
    // This structure specifies the STDIN and STDOUT handles for redirection.
    ZeroMemory(&siStartInfo, sizeof siStartInfo);
    siStartInfo.cb = sizeof siStartInfo;
    siStartInfo.hStdError = hChildStd_ERR_Wr;
    siStartInfo.hStdOutput = hChildStd_OUT_Wr;
    siStartInfo.hStdInput = hChildStd_IN_Rd;
    siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

    // Create the child process.
    // NOTE: Modifies szCmdline (!)
    bSuccess = CreateProcess(szCmd,         // Executable
                             szCmdline,     // command line
                             NULL,          // process security attributes
                             NULL,          // primary thread security attributes
                             TRUE,          // handles are inherited
                             0,             // creation flags
                             NULL,          // use parent's environment
                             NULL,          // use parent's current directory
                             &siStartInfo,  // STARTUPINFO pointer
                             &piProcInfo);  // receives PROCESS_INFORMATION

    if (!bSuccess ) {
      fprintf(stderr, "%d: Failed CreateProcess, gle=%lx\n", (int)__LINE__, (long)GetLastError());
      goto barf;
    }

    // Close handles to the child process and its primary thread.
    // We do not need these.
    if (!CloseHandle(piProcInfo.hProcess)) {
      fprintf(stderr, "%d: Failed CloseHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
      goto barf;
    }
    if (!CloseHandle(piProcInfo.hThread)) {
      fprintf(stderr, "%d: Failed CloseHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
      goto barf;
    }
  }
  *phChildStd_OUT_Rd = hChildStd_OUT_Rd;
  hChildStd_OUT_Rd = INVALID_HANDLE_VALUE; // Now owned by caller
#if 0
  *phChildStd_ERR_Rd = hChildStd_ERR_Rd;
  hChildStd_ERR_Rd = INVALID_HANDLE_VALUE; // Now owned by caller
#endif
  *phChildStd_IN_Wr = hChildStd_IN_Wr;
  hChildStd_IN_Wr = INVALID_HANDLE_VALUE; // Now owned by caller

  result = 0;                   // Success
 cleanup:
  if (hChildStd_OUT_Rd != INVALID_HANDLE_VALUE) {
    if (!CloseHandle(hChildStd_OUT_Rd)) {
      fprintf(stderr, "%d: Failed CloseHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    }
    hChildStd_OUT_Rd = INVALID_HANDLE_VALUE;
  }
#if 0
  if (hChildStd_ERR_Rd != INVALID_HANDLE_VALUE) {
    if (!CloseHandle(hChildStd_ERR_Rd)) {
      fprintf(stderr, "%d: Failed CloseHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    }
    hChildStd_ERR_Rd = INVALID_HANDLE_VALUE;
  }
#else
  if (hTmp != INVALID_HANDLE_VALUE) {
    if (!CloseHandle(hTmp)) {
      fprintf(stderr, "%d: Failed CloseHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    }
    hTmp = INVALID_HANDLE_VALUE;
  }
#endif
  if (hChildStd_IN_Wr != INVALID_HANDLE_VALUE) {
    if (!CloseHandle(hChildStd_IN_Wr)) {
      fprintf(stderr, "%d: Failed CloseHandle, gle=%lx\n", (int)__LINE__, (long)GetLastError());
    }
    hChildStd_IN_Wr = INVALID_HANDLE_VALUE;
  }
  return result;
 barf:
  result = -1;
  goto cleanup;
}

// The number of elements until the terminating NULL element.
static
int argvCount(TCHAR *argv[])
{
  int i;
  for (i = 0; argv[i] != NULL; i++) {
    // empty
  }
  return i;
}

static
size_t maxCommandLineCharCount(int argc, TCHAR const *argv[])
{
  size_t maxNeeded = 0;
  int i;

  for (i = 0; ; i++) {
    TCHAR const *arg = argv[i];
    size_t j;
    TCHAR c;
    if (arg == NULL) {
      break;
    }
    for (j = 0; (c = arg[j]) != '\0'; j++) {
      if (c == '\\') {
        // Some backslashes need doubling
        maxNeeded += 2;
      } else if (c == '"') {
        // All embedded double quotes need a backslash
        maxNeeded += 2;
      } else {
        // Most chars are stored as-is.
        maxNeeded += 1;
      }
    }
    // Each arg may need surrounding double quotes.
    maxNeeded += 2;
    // The rightmost surrounding double quote, if any, may need an extra backslash.
    maxNeeded += 1;
    // Each argument may need a space to separate it from the next argument.
    maxNeeded += 1;
  }
  // +1 Always make room for terminating NUL.
  return maxNeeded +1;
}

#define PUSH(C,P,LIMIT) do{ if ((P) < (LIMIT)) { *((P)++) = (C); } else { exit(EXIT_FAILURE); } }while(0)
/* The inverse of CommandLineToArgvW().
 */
static
TCHAR *myArgvToCommandLine(int argc, TCHAR *argv[])
{
  size_t maxNeeded = maxCommandLineCharCount(argc, argv);
  TCHAR *buf = LocalAlloc(LMEM_FIXED, maxNeeded * sizeof buf[0]);
  TCHAR *bufEnd;
  TCHAR *p;
  int i;

  if (buf == NULL) {
    return NULL;
  }
  bufEnd = buf+maxNeeded;
  p = buf;

  for (i = 0; i < argc; i++) {
    TCHAR const *arg = argv[i];
    size_t j;
    TCHAR c;
    if (!(p < bufEnd)) {
      exit(EXIT_FAILURE);
    }
    if (i > 0) {
      // Separate this argument from the one before it
      PUSH(' ', p, bufEnd);
    }

    PUSH('"', p, bufEnd);

    for (j = 0; (c = arg[j]) != '\0'; j++) {
      if (c == '\\') {
        int followedByDoubleQuote = 0;
        size_t backslashCount;
        size_t const k = j;

        // exits with arg[j] the last backslash of the adjacent backslashes
        for (; ; j++) {
          TCHAR next = arg[j+1];
          if (next == '\0') {
            // Backslashes all the way to the end of the argument, where we will add a "quotation mark".
            // So, these backslashes are "followed by a quotation mark"
            followedByDoubleQuote = 1;
            break;
          } else if (next == '"') {
            // These backslashes are followed by quotation mark, so need to be doubled
            followedByDoubleQuote = 1;
            break;
          } else if (next == '\\') {
            continue;
          } else {
            // Something else, we're done
            break;
          }
        }
        backslashCount = (j-k) + 1;

        if (followedByDoubleQuote) {
          // These need to be doubled
          backslashCount *= 2;
        }

          while (backslashCount > 0) {
            backslashCount--;
            PUSH('\\', p, bufEnd);
          }
      } else if (c == '"') {
        // All embedded double quotes need a backslash
        // Any preceding backslashes have already been doubled.
        PUSH('\\', p, bufEnd);
        PUSH('"', p, bufEnd);
      } else {
        // Most chars are stored as-is.
        PUSH(c, p, bufEnd);
      }
    }

    PUSH('"', p, bufEnd);
  }
  // Finally NUL-terminate the string
  PUSH('\0', p, bufEnd);
  return buf;
}
#undef PUSH


/* Returns 0 on success. */
static int my_launch(TCHAR *path, TCHAR *argv[], FILE **child_stdin, FILE **child_stdout)
{
  int result;
  TCHAR *szCmd = path;
  TCHAR *szCmdline = myArgvToCommandLine(argvCount(argv), argv);

  HANDLE hChildStd_OUT_Rd = INVALID_HANDLE_VALUE;
  // HANDLE hChildStd_ERR_Rd = INVALID_HANDLE_VALUE;
  HANDLE hChildStd_IN_Wr = INVALID_HANDLE_VALUE;

  int hChildStd_OUT_Rd_fd = -1;
  // int hChildStd_ERR_Rd_fd = -1;
  int hChildStd_IN_Wr_fd = -1;

  FILE *hChildStd_OUT_Rd_file = NULL;
  // FILE *hChildStd_ERR_Rd_file = NULL;
  FILE *hChildStd_IN_Wr_file = NULL;

  if (szCmdline == NULL) {
    fprintf(stderr, "%d: Out of memory in myArgvToCommandLine()\n", (int)__LINE__);
    goto barf;
  }

  if (win_launch(szCmd, szCmdline,
                 &hChildStd_OUT_Rd,
                 // &hChildStd_ERR_Rd,
                 &hChildStd_IN_Wr) != 0) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }

  hChildStd_OUT_Rd_fd = _open_osfhandle((intptr_t)hChildStd_OUT_Rd, 0);
  if (hChildStd_OUT_Rd_fd == -1) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }

#if 0
  hChildStd_ERR_Rd_fd = _open_osfhandle((intptr_t)hChildStd_ERR_Rd, 0);
  if (hChildStd_ERR_Rd_fd == -1) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }
#endif

  hChildStd_IN_Wr_fd = _open_osfhandle((intptr_t)hChildStd_IN_Wr, 0);
  if (hChildStd_IN_Wr_fd == -1) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }

  hChildStd_OUT_Rd_file = _fdopen(hChildStd_OUT_Rd_fd, "rb");
  if (hChildStd_OUT_Rd_file == NULL) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }

#if 0
  hChildStd_ERR_Rd_file = _fdopen(hChildStd_ERR_Rd_fd, "rb");
  if (hChildStd_ERR_Rd_file == NULL) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }
#endif

  hChildStd_IN_Wr_file = _fdopen(hChildStd_IN_Wr_fd, "wb");
  if (hChildStd_IN_Wr_file == NULL) {
    fprintf(stderr, "%d: Failed win_launch()\n", (int)__LINE__);
    goto barf;
  }

  *child_stdin  = hChildStd_IN_Wr_file;
  hChildStd_IN_Wr_file = NULL;
  *child_stdout = hChildStd_OUT_Rd_file;
  hChildStd_OUT_Rd_file = NULL;

  result = 0;
 cleanup:
  return result;
 barf:
  result = -1;
  goto cleanup;
}

#endif  /* USE_WINDOWS_API */

#if USE_POSIX_API

/* We need one pipe (each pipe having two end-points) for each standard stream.
   We currently only connect standard in and standard out, so we need two pipes.

   It would be easy to extend this code to pipe the standard error
   stream (or connect it to the NULL device).
*/
#define NUM_PIPES          2    /* sub-process stdin/CHILD_STDIN_PIPE, sub-process stdout/CHILD_STDOUT_PIPE */

#define CHILD_STDIN_PIPE  0
#define CHILD_STDOUT_PIPE   1

/* pipe(fildes) fills in an fildes[2] where the first (fildes[READ_FD]) is the
   read-end, the the second (fildes[WRITE_FD]) i the write-end. */
int pipes[NUM_PIPES][2];

#define READ_FD  0
#define WRITE_FD 1

#define PARENT_READ_FD  ( pipes[CHILD_STDOUT_PIPE][READ_FD]   )
#define PARENT_WRITE_FD ( pipes[CHILD_STDIN_PIPE][WRITE_FD] )

#define CHILD_STDIN_FD   ( pipes[CHILD_STDIN_PIPE][READ_FD]  )
#define CHILD_STDOUT_FD  ( pipes[CHILD_STDOUT_PIPE][WRITE_FD]  )

static pid_t launch(char const *path, char *argv[], int*child_stdin_fd, int *child_stdout_fd)
{
  pid_t pid;

  /* pipes for parent to write and read */
  if (pipe(pipes[CHILD_STDOUT_PIPE]) != 0) {
    fprintf(stderr, "Failed to pipe() errno=%d\n", errno);
    return -1;
  }

  if (pipe(pipes[CHILD_STDIN_PIPE])) {
    fprintf(stderr, "Failed to pipe() errno=%d\n", errno);
    return -1;
  }
  /* We do not want these FDs to be seen by the child after exec, so
     set their FD_CLOEXEC flag. */
  {
    int res;

    if ((res=fcntl(CHILD_STDIN_FD, F_GETFD)) == -1
        || fcntl(CHILD_STDIN_FD, F_SETFD, (res | FD_CLOEXEC)) == -1) {
      fprintf(stderr, "Failed to fctl() errno=%d\n", errno);
      return -1;
    }

    if ((res=fcntl(CHILD_STDOUT_FD, F_GETFD)) == -1
        || fcntl(CHILD_STDOUT_FD, F_SETFD, (res | FD_CLOEXEC)) == -1) {
      fprintf(stderr, "Failed to fctl() errno=%d\n", errno);
      return -1;
    }

    if ((res=fcntl(PARENT_READ_FD, F_GETFD)) == -1
        || fcntl(PARENT_READ_FD, F_SETFD, (res | FD_CLOEXEC)) == -1) {
      fprintf(stderr, "Failed to fctl() errno=%d\n", errno);
      return -1;
    }

    if ((res=fcntl(PARENT_WRITE_FD, F_GETFD)) == -1
        || fcntl(PARENT_WRITE_FD, F_SETFD, (res | FD_CLOEXEC)) == -1) {
      fprintf(stderr, "Failed to fctl() errno=%d\n", errno);
      return -1;
    }

  }

  pid = fork();
  if (pid > 0)
    {
      /* In parent process, return pid of child. */

      /* close fds not required by parent */
      if (close(CHILD_STDIN_FD) == -1) {
        fprintf(stderr, "Failed to close() errno=%d\n", errno);
        return -1;
      }
      if (close(CHILD_STDOUT_FD) == -1) {
        fprintf(stderr, "Failed to close() errno=%d\n", errno);
        return -1;
      }

      *child_stdin_fd = PARENT_WRITE_FD;
      *child_stdout_fd = PARENT_READ_FD;
      return pid;
    }

  if (pid == -1)
    {
      int err = errno;

      fprintf(stderr, "Failed to fork %s, errno=%d\n", path, err);
      return -1;
    }

  /* In child process, exec the program. */

  /* Make child stdin read from (the read-end of) the pipe the the
     parent will write to. */
  if (dup2(CHILD_STDIN_FD, STDIN_FILENO) == -1) {
    /* It is not safe to report the error with fprintf() here (before exec()).  */
    _exit(EXIT_FAILURE);
  }
  /* Make child stdout write to (the write-end of) of the pipe the the
     parent will read from. */
  if (dup2(CHILD_STDOUT_FD, STDOUT_FILENO) == -1) {
    /* It is not safe to report the error with fprintf() here (before exec()).  */
    _exit(EXIT_FAILURE);
  }

  /* The exec will close all our pipe FDs in the child, because of the
     FD_CLOEXEC set above. (The pipe ends the child needs have already
     been copied to the STDIN_FILENO and STDOUT_FILENO, which are not
     closed by the exec). */
  execvp(path, argv);
  {
    int err = errno;
    fprintf(stderr, "Failed to exec %s, errno=%d\n", path, err);
    fflush(stderr);
    /* FALLTHROUGH */
  }
  /* If we get here, exec failed, we are in a sub-process, and there
     is nothing we can do except exit the sub-process with and
     error. */
  _exit(EXIT_FAILURE);
  return -1;             /* Not reached */
}

/* Returns 0 on success. */
static int my_launch(char const *path, char *argv[], FILE **pchild_stdin, FILE **pchild_stdout)
{
  pid_t pid;
  int child_stdin_fd, child_stdout_fd;
  FILE *child_stdin, *child_stdout;

  pid = launch(path, argv, &child_stdin_fd, &child_stdout_fd);
  if (pid <= 0) {
    exit(EXIT_FAILURE);
  }
  child_stdin = fdopen(child_stdin_fd, "wb");
  if (child_stdin == NULL) {
    int err = errno;
    fprintf(stderr, "Failed to fdopen(child_stdin), errno=%d\n", err);
    fflush(stderr);
    return -1;
  }
  child_stdout = fdopen(child_stdout_fd, "rb");
  if (child_stdout == NULL) {
    int err = errno;
    fprintf(stderr, "Failed to fdopen(child_stdout), errno=%d\n", err);
    fflush(stderr);
    return -1;
  }

  *pchild_stdin = child_stdin;
  *pchild_stdout = child_stdout;
  return 0;
}

#endif  /* USE_POSIX_API */

#if USE_WINDOWS_API
static char *my_get_line(FILE *s)
{
  size_t i;
  size_t buf_len = 512;
  char *buf = malloc(buf_len * sizeof buf[0]);
  int const delim = 10; // ASCII LF (either last if Posix line ending, or after CR if Windows line ending
  int c;

  i = 0;
  for (;;) {
    // Ensure room for next char and NUL-termination.
    while (buf_len < i+2) {
      size_t old_size = buf_len*sizeof buf[0];
      size_t new_len = buf_len*2;
      size_t new_size = new_len*sizeof buf[0];
      void *tmp = realloc(buf, new_size);
      // DBG
      // fprintf(stderr, "%d: realloc buffer |%lu| ==> |%lu|\n", (int)__LINE__, (unsigned long)old_size, (unsigned long)new_size);

      if (tmp == NULL) {
        free(buf);
        fprintf(stderr, "%d: Failed realloc()\n", (int)__LINE__);
        return NULL;
      }
      buf = tmp;
      buf_len = new_len;
    }
    // Here: i+1 < buf_len
    c = fgetc(s);
    if (c == -1) {
      buf[i++] = '\0';
      break;
    } else {
      buf[i++] = (char)c;
      if (c == delim) {
        buf[i++] = '\0';
        break;
      }
    }
  }
  // DBG
  // fprintf(stderr, "%d: my_get_line()=\"%s\"\n", (int)__LINE__, (char *) buf);

  return buf;
}

#endif  /* USE_WINDOWS_API */


static char *read_json(FILE *in)
{
  char *line = NULL;
#if USE_POSIX_API
  {
    size_t linecap = 0;
    ssize_t linelen = getline(&line, &linecap, in);

    if (linelen < 0) {
      int err = errno;
      fprintf(stderr, "Failed to getline(), errno=%d\n", err);
      fflush(stderr);
      exit(EXIT_FAILURE);
      return NULL;
    }
  }
#endif  /* USE_POSIX_API */
#if USE_WINDOWS_API
  line = my_get_line(in);
  if (line == NULL) {
    fprintf(stderr, "Failed to getline()\n");
    fflush(stderr);
    exit(EXIT_FAILURE);
    return NULL;
  }
#endif  /* USE_WINDOWS_API */

  LOG_DO(fprintf(stderr, "Got: \"%s\"\n", line));

  return line;
}

static int send_json(FILE *out, char const *json)
{
  LOG_DO(fprintf(stderr, "Send: \"%s\"\n", json));

  fprintf(out, "%s\n", json);
  fflush(out);
  return 0;
}

/* Returns kind and extent of the "result" member, if present in the reply. Otherwise returns JSON_KIND_UNKNOWN.

Also see is_failure() and is_exception().
*/
static
enum json_kind parse_reply(char const *buf, char const **pstart, char const **pend)
{
  enum json_kind kind;
  char const *start;
  char const *end;

  if ((kind = json_lookup_member(buf, "result", &start, &end)) != JSON_KIND_UNKNOWN) {
    *pstart = start;
    *pend = end;
    return kind;
  }

  if ((kind = json_lookup_member(buf, "error", &start, &end)) != JSON_KIND_UNKNOWN) {
    char const * const error_start = start;
    char const *code_start;
    char const *code_end;
    size_t code_length;
    char const *message_start;
    char const *message_end;
    char const *data_start;
    char const *data_end;

    if ((kind = json_lookup_member(error_start, "code", &code_start, &code_end)) == JSON_KIND_UNKNOWN) {
      code_start="No code";
      code_end = code_start + strlen(code_start);
    }
    if ((kind = json_lookup_member(error_start, "message", &message_start, &message_end)) == JSON_KIND_UNKNOWN) {
      message_start = "No Message";
      message_end = message_start + strlen(message_start);
    }
    if ((kind = json_lookup_member(error_start, "data", &data_start, &data_end)) == JSON_KIND_UNKNOWN) {
      data_start = "No data";
      data_end = data_start + strlen(data_start);
    }

    code_length = code_end-code_start;
    if (code_length == strlen(FAILURE_CODE_STRING)
        &&
        strncmp(FAILURE_CODE_STRING, code_start, code_length) == 0) {
      LOG_DO(fprintf(stderr, "Prolog failed (this is expected)\n"));
    }
    else if (code_length == strlen(EXCEPTION_CODE_STRING)
             &&
             strncmp(EXCEPTION_CODE_STRING, code_start, code_length) == 0) {
      LOG_DO(fprintf(stderr, "Prolog threw an exception: message=%.*s, data=%.*s\n",
                     (int)(message_end-message_start), message_start,
                     (int)(data_end-data_start), data_start));
    } else {
      LOG_DO(fprintf(stderr, "Some other error: code=%.*s, message=%.*s, data=%.*s\n",
                     (int)(code_end-code_start), code_start,
                     (int)(message_end-message_start), message_start,
                     (int)(data_end-data_start), data_start));

    }
  } else {
    LOG_DO(fprintf(stderr, "Malformed reply \"%s\"\n", (buf == NULL ? "NULL" : buf)));
  }

  return JSON_KIND_UNKNOWN;
}

static
int is_error(char const *buf, char const *code_string)
{
  enum json_kind kind;
  char const *error_start;
  char const *error_end;

  if ((kind = json_lookup_member(buf, "error", &error_start, &error_end)) != JSON_KIND_UNKNOWN) {
    char const *code_start;
    char const *code_end;
    size_t code_length;

    /* The code must exist, and must be a number */
    if (json_lookup_member(error_start, "code", &code_start, &code_end) == JSON_KIND_NUMBER) {
      code_length = code_end-code_start;
      if (code_length == strlen(code_string)
          &&
          strncmp(code_string, code_start, code_length) == 0) {
        return 1;
      }
    }
  }
  return 0;
}

/* Returns non-zero if the reply is a failure (error-) reply. */
static
int is_failure(char const *buf)
{
  return is_error(buf, FAILURE_CODE_STRING);
}

/* Returns non-zero if the reply is a exception (error-) reply. */
static
int is_exception(char const *buf)
{
  return is_error(buf, EXCEPTION_CODE_STRING);
}

static void interact(FILE *child_stdin, FILE *child_stdout)
{
  char *json_buf = NULL;
  enum json_kind kind;
  char const *start;
  char const *end;

  /* Read the initial state (it is null) */
  fprintf(stdout, "state ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"state\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "State is %.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Set the state (to 4) and return its previous value. */
  fprintf(stdout, "state:=4 ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"state\", \"params\":[4]}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "State was %.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Read current state. */
  fprintf(stdout, "state ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"state\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "State is %.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
  fprintf(stdout, "\n"); fflush(stdout);

  /* Increment current state by 1. */
  fprintf(stdout, "once(Result is StateIn+1, StateOut=Result). ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"once\", \"params\":{\"goal\":\"Result is StateIn+1, StateOut=Result.\"}}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Increment current state by 1 (again). */
  fprintf(stdout, "once(Result is StateIn+1, StateOut=Result). ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"once\", \"params\":{\"goal\":\"Result is StateIn+1, StateOut=Result.\"}}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
  fprintf(stdout, "\n"); fflush(stdout);

  /* Increment the state with an increment specified as a VariableName:Value pair */
  fprintf(stdout, "Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"once\", \"params\":{\"goal\":\"Result is StateIn+Increment,StateOut=Result.\", \"bindings\":{\"Increment\":5}}}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
  fprintf(stdout, "\n"); fflush(stdout);

  /* Call member(...), backtracking over solutions. */
  fprintf(stdout, "Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> "); fflush(stdout);
  send_json(child_stdin,
            "{"
            "\"jsonrpc\":\"2.0\",\"id\":1,"
            "\"method\":\"call\","
            " \"params\":"
            "{"
            "\"goal\":"
            "\"member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.\","
            "\"bindings\":"
            "{\"Multiplier\":10}"
            "}"
            "}"
            "\n"
            );
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "First Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Ask for the next solution */
  fprintf(stdout, "retry ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"retry\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "(next) Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Commits to the most recent solution of the most recent 'call' */
  fprintf(stdout, "cut ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"cut\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
  fprintf(stdout, "\n"); fflush(stdout);


  /* Backtrack until failure. */
  fprintf(stdout, "Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> "); fflush(stdout);
  send_json(child_stdin,
            "{"
            "\"jsonrpc\":\"2.0\",\"id\":1,"
            "\"method\":\"call\","
            " \"params\":"
            "{"
            "\"goal\":"
            "\"member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.\","
            "\"bindings\":"
            "{\"Multiplier\":10}"
            "}"
            "}"
            "\n"
            );
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "First Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Ask for the next solution */
  fprintf(stdout, "retry ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"retry\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "(next) Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Ask for the next solution */
  fprintf(stdout, "retry ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"retry\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "(next) Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;

  /* Ask for the next solution (this will fail, since there are only 3 elements in the list). */
  fprintf(stdout, "retry ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"retry\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "(next) Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  } else if (is_failure(json_buf)) {
    fprintf(stdout, "Prolog failed (this is expected)\n"); fflush(stdout);
  } else if (is_exception(json_buf)) {
    fprintf(stdout, "Prolog threw an exception\n"); fflush(stdout);
  } else {
    fprintf(stdout, "Error, unexpected reply %s\n", json_buf); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
  fprintf(stdout, "\n"); fflush(stdout);

  /* once(foo is bar). This will throw an exception in Prolog, which translates into an error reply. */
  fprintf(stdout, "once(foo is bar). ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"once\", \"params\":{\"goal\":\"foo is bar.\"}}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "(next) Result=%.*s\n", (int)(end-start), start); fflush(stdout);
  } else if (is_failure(json_buf)) {
    fprintf(stdout, "Prolog failed\n"); fflush(stdout);
  } else if (is_exception(json_buf)) {
    fprintf(stdout, "Prolog threw an exception (this is expected)\n"); fflush(stdout);
  } else {
    fprintf(stdout, "Error, unexpected reply %s\n", json_buf); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
  fprintf(stdout, "\n"); fflush(stdout);

  /* Tell server, i.e. sub-process, to quit. */
  fprintf(stdout, "quit ==> "); fflush(stdout);
  send_json(child_stdin, "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"quit\"}\n");
  json_buf = read_json(child_stdout);
  if ((kind = parse_reply(json_buf, &start, &end)) != JSON_KIND_UNKNOWN) {
    fprintf(stdout, "Result=%.*s\n", (int)(end-start), start);
  } else if (is_failure(json_buf)) {
    fprintf(stdout, "Prolog failed\n"); fflush(stdout);
  } else if (is_exception(json_buf)) {
    fprintf(stdout, "Prolog threw an exception\n"); fflush(stdout);
  } else {
    fprintf(stdout, "Error, unexpected reply %s\n", json_buf); fflush(stdout);
  }
  free(json_buf); json_buf = NULL;
}

#if USE_WINDOWS_API
// int __cdecl _tmain(int argc, TCHAR **argv)
int __cdecl _tmain(int argc, TCHAR **argv)
#else  /* USE_POSIX_API */
int main(int argc, char **argv)
#endif  /* USE_POSIX_API */
{
  FILE *child_stdin;
  FILE *child_stdout;
  TCHAR *exe_path = (argc > 1 ? argv[1] : TEXT("sicstus"));
  TCHAR *pl_path = (argc > 2 ? argv[2] : TEXT("$SP_LIBRARY_DIR/jsonrpc/jsonrpc_server"));

  int largv_skip = (LOGGING ? 2 : 0);
  TCHAR *largv[] = {
    NULL,
    /* largv_skip+1 should skip the above element when LOGGING */
    TEXT("--nologo"),TEXT("--noinfo"),     /* Only when !LOGGING */
    /* largv_skip+1 should skip the above elements when !LOGGING. */

    TEXT("-l"), pl_path,
    TEXT("--goal"),
#if LOGGING
    /* The call hook could do input validation, or handle "messages" instead of raw goals. */
    TEXT("assert((my_call(X) :- format(user_error, '~NIn Server, ~q~n', [call(X)]),flush_output(user_error), call(X))),")
    TEXT("jsonrpc_server_main([call_hook(my_call)]),halt."),
#else  /* !LOGGING */
    TEXT("jsonrpc_server_main([call_hook(call)]),halt."),
#endif  /* !LOGGING */

    NULL
  };

  largv[largv_skip] = exe_path;

  if (my_launch(exe_path, largv+largv_skip, &child_stdin, &child_stdout) != 0) {
    exit(EXIT_FAILURE);
  }

  interact(child_stdin, child_stdout);
  return 0;
}
