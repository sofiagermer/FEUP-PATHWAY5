/* Copyright(C) 1994-95, Swedish Institute of Computer Science */
#define SICSTUS_HIDDEN_API 1    /* [PM] 4.0 Needed for SP_reset_term_refs */
/*
[PM] 3.10.2 See http://www.tcl.tk/cgi-bin/tct/tip/66.html for some details on embedding Tcl/Tk. (There may be things there that we should do but don't).
 */

/*
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tcl.c -c -Fox86-win32-nt-4/tcltk/tcl_d.obj
tcl.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tk.c -c -Fox86-win32-nt-4/tcltk/tk_d.obj
tk.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tkappini.c -c -Fox86-win32-nt-4/tcltk/tkappini_d.obj
tkappini.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tkterm.c -c -Fox86-win32-nt-4/tcltk/tkterm_d.obj
tkterm.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/util.c -c -Fox86-win32-nt-4/tcltk/util_d.obj
util.c
*/


#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <sicstus/sicstus.h>

#if USE_TCL_STUBS
#if !SP_WIN32			/* (POSIX) */
#include <dlfcn.h>
#endif	/* !SP_WIN32 */
#endif	/* USE_TCL_STUBS */

#include "tcl.h"

#if USE_TCL_STUBS
#if SP_WIN32
#undef None
#undef HANDLE
#undef ControlMask
#include <Windows.h>
#endif	/* SP_WIN32 */
#endif	/* USE_TCL_STUBS */

#define TCL_NUMVERSION (TCL_MAJOR_VERSION*10+TCL_MINOR_VERSION)

/*   --------------------------------------------------------------  */

/* Local and non local variable definitions */

/* [PM] 3.9b5 explicit init to force link-time conflict if multiply defines.

   Could have used local = {0}; but that will give a GCC warning about
   to few initializers which will break together with -Werror.

 */

struct local_state local = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,
                            0,0,0,0,0,0,0,0,0,
#if SICSTUS_DBG
                            0,  /* old_err */
#endif /* SICSTUS_DBG */
                            0,0,0,0,
                            {0},  /* err_msg[] */
                            0,   /* err_argno */
                            0    /* err_culprit */
};

/*   --------------------------------------------------------------  */
/* Local function prototypes */

static int SPCDECL prolog_call _ANSI_ARGS_((ClientData clientData,
				  Tcl_Interp *interp, int argc, char **argv));

static int SPCDECL prolog_event _ANSI_ARGS_((ClientData clientData,
				  Tcl_Interp *interp, int argc, char **argv));

static int initialize_tcl(void);

/*   --------------------------------------------------------------  */

/* 
   Ensure Tcl/Tk is initialized. Must be called before any call to Tcl_...().

   Return zero on error, non-zero on success.
*/
static int ensure_tcl_initialized(void) {
  if (!local.tcl_inited) {
    return initialize_tcl();
  }
  return 1;
}


/* Create and init a new interpreter */
/* Define some SICStus specific commands */

void SPCDECL tcl_new(SP_term_ref tInterp)
{
  char const * error_message;
  struct interp_data *interp_data;
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */

  if (!ensure_tcl_initialized()) {
    error_message = "Couldn't initialized Tcl";
    goto barf_;
  }

  interp_data = (struct interp_data *)SP_malloc(sizeof(struct interp_data));

  if (interp_data == NULL
      || !(interp_data->interp = interp = Tcl_CreateInterp()))
    {
      if (interp_data != NULL)
        {
          SP_free(interp_data);
        }
      error_message = "Couldn't create Tcl interpreter";
      goto barf_;
    }
  interp_data->self = interp_data; /* Enables (unsafe) check */
  interp_data->event_list = NULL;
  interp_data->stream_buf = NULL;
  ptr_to_wrapper(local.atm_interp, interp_data, tInterp);

  Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);

  if (Tcl_Init(interp) == TCL_ERROR) {
    error_message = Tcl_GetStringResult(interp);
    goto barf_;
  }

  Tcl_CreateCommand(interp, "prolog", (Tcl_CmdProc *)prolog_call,
		    (ClientData)interp_data, (Tcl_CmdDeleteProc *)NULL);
  Tcl_CreateCommand(interp, "prolog_event", (Tcl_CmdProc *)prolog_event,
		    (ClientData)interp_data, (Tcl_CmdDeleteProc *)NULL);
  
  return;
 barf_:
  SAVE_AND_RAISE_ERROR(SPTCL_ERROR, error_message, tInterp, 0, "tcl_new", 1);
}

/*   --------------------------------------------------------------  */

/* Delete an interpreter */

void SPCDECL tcl_delete_interp(SP_term_ref tInterp)
{
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */

  CHECK_INTERP(interp_data, interp, tInterp, 1);

#if 0                           /* [PM] 4.0 no stream caching yet */
  if (interp_data->stream)
    {
      (void)SP_fclose(interp_data->stream, SP_FCLOSE_OPTION_FORCE);
      interp_data->stream = NULL;
    }
#endif  /* 0 */

  {
    struct event_q *p=interp_data->event_list, *q;

    for (; p; p=q)
      {
	q = p->next;
	SP_free(p);
      }
  }

  Tcl_DeleteInterp(interp);  

  if (interp_data->stream_buf != NULL)
    {
      SP_free(interp_data->stream_buf);
      interp_data->stream_buf = NULL; /* DBG, not needed */
    }
  SP_free(interp_data);
  return;

  RAISE_ERROR("tcl_delete", 1);
}

/*   --------------------------------------------------------------  */

/*
 *  
 *  PROLOG CALL:  tcl_eval()
 *  
 *  This routine should now be reentrant, i.e. you can call
 *  Tcl that call Prolog that call Tcl....
 *  
 *  '*current_event' is a pointer to the head of a chain of
 *  events. A new event has the initial value of [],
 *  i.e. nil. If Prolog is called recursively 'current_event'
 *  has to be stored. After the call it should be restored.
 *  
 */

void SPCDECL tcl_eval(SP_term_ref tInterp, SP_term_ref tScript, SP_term_ref tTerm)
{
  char const *error_message;

  int code;
  size_t len;
  char const *script;
  char *script_copy;
  char buf[BUFLEN];
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */  

  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if ((script = translate_command(tScript, interp_data)) == NULL)
    {
      goto error;
    }
 
  len = strlen(script)+1;
  BUF_ALLOC(len, BUFLEN, buf, {goto memerr;}, script_copy);
  memcpy(script_copy, script, len);

  code = Tcl_Eval(interp, script_copy);

  BUF_FREE(buf, script_copy);

  switch (code)
    {
    case TCL_OK:
    case TCL_RETURN:
    case TCL_BREAK:
    case TCL_CONTINUE:
      {
	SP_term_ref tail = SP_new_term_ref();

	SP_put_atom(tail, local.atm_nil); /* End of list */
	SP_put_list_codes(tTerm, tail, Tcl_GetStringResult(interp));
	break;
      }
    case TCL_ERROR:
    default:			/* Could there be others? */
      error_message = Tcl_GetStringResult(interp);
      goto barf_;
    }

  return;

memerr:
  error_message = "Couldn't allocate memory";
  goto barf_;

 barf_:
  SAVE_AND_RAISE_ERROR(SPTCL_ERROR, error_message, tInterp, 0, "tcl_eval", 3);
}

/*   --------------------------------------------------------------  */

/*
 *  
 *  PROLOG CALL:  tcl_event()
 *  
 *  Similar to tcl_eval() above
 *  
 */

void SPCDECL tcl_event(SP_term_ref tInterp, SP_term_ref tScript, SP_term_ref tEvent)
{
  int code;
  size_t len;
  char const *script;
  char *script_copy;
  char buf[BUFLEN];
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp,(tInterp));
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */

  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if ((script = translate_command(tScript, interp_data)) == NULL)
    {
      goto error;
    }

  len = strlen(script)+1;
  BUF_ALLOC(len, BUFLEN, buf, {goto memerr;}, script_copy);
  memcpy(script_copy, script, len);

  code = Tcl_Eval(interp, script_copy);
  BUF_FREE(buf, script_copy);

  switch (code)
    {
    case TCL_OK:
      if (put_event_queue(interp_data, TRUE, tEvent) >= 0)
	return;
      goto error;
    case TCL_ERROR:
    default:
      SAVE_ERROR(SPTCL_ERROR, Tcl_GetStringResult(interp), tInterp, 0);
    }
 memerr:
  SAVE_ERROR(SPTCL_ERROR, "Couldn't allocate memory", tInterp, 0);
  RAISE_ERROR("tcl_event", 3);
}

/*   --------------------------------------------------------------  */
/*   --------------------------------------------------------------  */
/*   --------------------------------------------------------------  */

/*
 *
 *  TCL COMMAND:  prolog_call "goal"
 *  
 */

static int SPCDECL prolog_call(ClientData clientData, Tcl_Interp *interp, int argc, char **argv)
{
  struct interp_data *interp_data = (struct interp_data *)clientData;
  SP_stream *stream;
  SP_term_ref tStream;
  SP_term_ref tInterp;
  int tcl_res = TCL_OK;
  int res;

  SP_ASSERT(local.tcl_inited);
  
  if (argc != 2)
    {
      Tcl_AppendResult(interp, "Wrong number of arguments: should be \"",
		       argv[0], " term\"", (char *)NULL);
      return TCL_ERROR;
    }
#if !FORCE_BUILD && SICSTUS_VERSION > 40700 /* [PM] 4.3.1 postpone */
#error "Should re-use the functionality of the SP_read_from_string stream (which then needs a local copy of the string, xref codesio too)"
#endif  /* SICSTUS_TODO */

  if ((stream = get_tcl_stream(interp_data, argv[1])) == NULL)
    {
      Tcl_AppendResult(interp, "Couldn't allocate memory", (char *)NULL);
      return TCL_ERROR;
    }
  tStream = SP_new_term_ref();
  /* All returns below this point should pass through cleanup: */

  if (!SP_put_address(tStream, (void *)stream)) goto unexpected_error;
  tInterp = SP_new_term_ref();
  ptr_to_wrapper(local.atm_interp, interp_data, tInterp);


  /* Because we copy the result to Tcl strings we can reclaim */
  /* all heap storage that we have used */
  res = SP_query_cut_fail(local.call_pred, tStream, tInterp);

  switch (res)
    {
    case SP_SUCCESS:
      /* [PM] May 2000, was interp->result = "1"; */
      Tcl_SetResult(interp, "1", TCL_STATIC);
      goto cleanup;
    default:
      SP_ASSERT(0);
      /* FALLTHROUGH */
    case SP_FAILURE:
      /* [PM] May 2000, was interp->result = "0"; */
      Tcl_SetResult(interp, "0", TCL_STATIC);
      goto cleanup;
    case SP_ERROR:
      {
        char const *excpstr;
        SP_term_ref tExcp;
        
        tExcp = SP_new_term_ref();

        if(SP_exception_term(tExcp)
           &&
           (excpstr = trans_term(tExcp, interp_data)) != NULL)
          {
            ; /* empty, excpstr already set-up */
          }
        else                    /* some failure */
          {
          unexpected_error:
            excpstr = "unexpected error in " __FILE__;
          }

        Tcl_AppendResult(interp,"Exception during Prolog execution: ",
                         argv[1], "  ", excpstr, (char *)NULL);

        tcl_res = TCL_ERROR;
        goto cleanup;
      }
    }
 cleanup:
  SP_reset_term_refs(tStream);
  return tcl_res;
}

/*   --------------------------------------------------------------  */

/*
 *
 *  TCL COMMAND:  prolog_event "term"
 *  
 */

static int SPCDECL prolog_event(ClientData clientData, Tcl_Interp *interp, int argc, char **argv)
{
  struct interp_data *interp_data = (struct interp_data *)clientData;
  struct event_q *p;
  int i;

  (void)interp;

  SP_ASSERT(local.tcl_inited);

  for (i = 1; i < argc; i++)
    {
      size_t len = strlen(argv[i]);
      LAZY_NULL_CHECK(p = (struct event_q *)SP_malloc(len + sizeof(struct event_q))); /* event_q size includes space for NUL */
      memcpy(p->event_string, argv[i], len +1);
      p->next = interp_data->event_list;
      interp_data->event_list = p;
    }
  return TCL_OK;
}


/*   --------------------------------------------------------------  */

void SPCDECL tcl_add_result(SP_term_ref tInterp, SP_term_ref tVarName, SP_term_ref tResult)
{
  char const *varName;
  struct interp_data *interp_data;
  Tcl_Interp *interp;
  char const *varval = NULL;

  CLEAR_ERROR(); /* [PM] 4.3 */

  if (!SP_get_string(tVarName, &varName))
    {
      goto unexpected_error;
    }
  /* [PM] Make it possible to ignore results not in the "special command format" */
  if (( varName[0] == '_'        /* 3.8.5 used to be too restrictive (strcmp(varName, "_") == 0) */
        || SP_is_variable(tResult) )) /* 3.8.5 as per GvN feedback */
    {
      return;
    }

  interp_data = (struct interp_data *)wrapper_to_ptr(local.atm_interp,tInterp);
  SP_ASSERT(interp_data != NULL);
  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if ((varval = trans_command(tResult, interp_data)) == NULL)
    {
      SAVE_ERROR(SPTCL_ERROR, "Incorrect command format", tResult, 3);
    }

#if TCL_MAJOR_VERSION > 8 || ( TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION >= 4) /* >= 8.4 */
  /* [PM] 4.0 const should be OK in 8.4 */
  Tcl_SetVar2(interp, "prolog_variables", varName, varval, 0);
#else  /* < 8.4 */
  /* [PM] 4.0 Need to cast away const-ness since Tcl_SetVar has incorrect (const-less) prototype before 8.4 */
  Tcl_SetVar2(interp, "prolog_variables", (char *)varName, (char *)varval, 0);
#endif  /* < 8.4 */

  return;

 unexpected_error:
  SAVE_ERROR(SPTCL_ERROR, "Unexpected error", tResult, 0);

  RAISE_ERROR("$tcl_add_result", 3);
}

/*   --------------------------------------------------------------  */

/* Initialize Tcl/Tk, possibly first loading its DSO if using stubs.

   Return zero on error, non-zero on success.
*/
static int initialize_tcl(void)
{
  int rc;
  Tcl_Interp *interp = NULL;	   /* neeeds cleanup */
  char *allocated_dso_file = NULL; /* needs cleanup */
  char *allocated_app_path = SP_getenv("SP_APP_PATH"); /* needs cleanup */
  char *app_path = (allocated_app_path != NULL ? allocated_app_path : "");

#if USE_TCL_STUBS
  /*
     See "Dynamically Loading Tcl" in
     <https://wiki.tcl-lang.org/page/How+to+embed+Tcl+in+C+applications>
     for hints.
  */
  {
#if __linux__
    /* On e.g. Ubuntu, libtcl.so is installed by tcl-dev, whereas
       libtcl8.6.so is installed by libtcl8.6. */
    char const *default_dso_files[] = {"libtcl.so", "libtcl8.6.so", "libtcl8.5.so", NULL};
#elif __APPLE__
    /* This will make dlopen() look in the "usual [framework-]
       places", including /Library/Frameworks/ and
       /System/Library/Frameworks/ */
    char const *default_dso_files[] = {"Tcl.framework/Tcl", NULL};
#elif SP_WIN32

    char const *default_dso_files[] ={
      /* The name tcl-lang.org tcl8.6.11 uses (and ActiveState 8.6.6). */
      "tcl86t.dll",
      /* The name ActiveState 8.6.1 uses. */
      "tcl86.dll",
      NULL};
#endif
    char const *dso_file;
    size_t i;

    allocated_dso_file = SP_getenv("SP_TCL_DSO");
    if (allocated_dso_file != NULL) {
      SP_ASSERT(((sizeof default_dso_files)/(sizeof default_dso_files[0])) >= 2); /* Room for one entry + NULL. */
      /* If allocated_dso_file is set, then _only_ try that. */
      default_dso_files[0] = allocated_dso_file;
      default_dso_files[1] = NULL;
    }

    for (i = 0; (dso_file = default_dso_files[i]) != NULL; i++) {
      /* "The first two function calls when initializing an embedded Tcl
	 interpreter should be Tcl_FindExecutable and
	 Tcl_CreateInterp. These two calls can be done in either order,
	 but both should be done before calling Tcl_Init."
	 https://www.sciencedirect.com/topics/computer-science/tcl-interpreter
      */
#if SP_WIN32
      {
	HANDLE handle = NULL;

	handle = LoadLibraryA(dso_file);
	if (handle == NULL) {
	  continue;
	}
	/* Once we found a dso_file, all other errors should goto barf. */

	{
	  void* findExecutable;
	  findExecutable = (void *)GetProcAddress(handle, "Tcl_FindExecutable");
	  if (findExecutable == NULL) {
	    goto barf;
	  }
	  ((void  (SPCDECL *)(char const *))findExecutable)(app_path);
	}

	{
	  void *createInterp = (void *)GetProcAddress(handle, "Tcl_CreateInterp");
	  if (createInterp == NULL) {
	    goto barf;
	  }
	  interp = ((Tcl_Interp* (SPCDECL *)(void))createInterp)();
	}
      }
#else  /* !SP_WIN32 (POSIX, Linux/macOS) */
      {
	void* handle;

	/* We could probably use RTLD_LAZY */
	handle = dlopen(dso_file, RTLD_NOW | RTLD_LOCAL);
	if (handle == NULL ) {
	  continue;
	}
	/* Once we found a dso_file, all other errors should goto barf. */

	{
	  void* findExecutable = dlsym(handle, "Tcl_FindExecutable");
	  if (findExecutable == NULL) {
	    goto barf;
	  }
	  ((void (*)(char const *))findExecutable)(app_path);
	}

	{
	  void *createInterp = dlsym(handle, "Tcl_CreateInterp");
	  if (createInterp == NULL) {
	    goto barf;
	  }
	  interp = ((Tcl_Interp* (*)(void))createInterp)();
	}
      }
#endif	/* !SP_WIN32 (POSIX) */

	/* Successfully opened and used a dso_file, we're done. */
      break;
    }
    if (dso_file == NULL) {
      /* Failed to open any of the dso files. */
      goto barf;
    }
    /*
      "NULL means use the latest version available." is undocumented
      in Man pages for Tcl_PkgRequireEx(), so do not use that. Instead
      use explicit lowest known good version.

      The version returned will be "" in 8.6.9 (fixed in 8.6.10),
      <https://core.tcl-lang.org/tcl/tktview/39fed4dae51e4fd7f1abf6baf3ba094a072b8beb>
    */
    if (Tcl_InitStubs(interp, "8.5", 0) == NULL) {
      goto barf;
    }

    if (Tcl_Init(interp) == TCL_ERROR) {
      goto barf;
    }
  }
#endif	/* USE_TCL_STUBS */

#if USE_TCL_STUBS
  /* Tcl_FindExecutable() is called above */
#else			  /* !USE_TCL_STUBS */
  Tcl_FindExecutable(app_path);
#endif			  /* !USE_TCL_STUBS */


#if TCL_NUMVERSION >= 75 && !DO_NOT_CREATE_DUMMY_INTERPRETER
  /* Tcl7.5 (and up?) has the bad habit of closing stdin etc when the
     last interpreter is deleted. To avoid this we make an interpreter
     that is never deleted. Preserve as an extra safety measure.
  */
  if (interp == NULL) {
    interp = Tcl_CreateInterp();
  }

  Tcl_Preserve((ClientData)interp);
  interp = NULL;		/* protect from cleanup */
#endif	/* DO_NOT_CREATE_DUMMY_INTERPRETER */
  local.tcl_inited = 1;
  rc = 1;
 cleanup:
  if (interp != NULL) {
    Tcl_DeleteInterp(interp);
    interp = NULL;
  }
  if (allocated_dso_file != NULL) {
    SP_free(allocated_dso_file);
    allocated_dso_file = NULL;
  }
  if (allocated_app_path != NULL) {
    SP_free(allocated_app_path);
    allocated_app_path = NULL;
  }
  return rc;
 barf:
  rc = 0;
  goto cleanup;
}

void SPCDECL tcl_initialize(int when)
{
  local.tcl_inited = 0;
  local.tk_inited = 0;
  
  CLEAR_ERROR(); /* [PM] 4.3 */

  (void)when;

  (void)SP_register_atom(local.atm_nil = SP_atom_from_string("[]"));
  (void)SP_register_atom(local.atm_period = SP_atom_from_string("."));
  (void)SP_register_atom(local.atm_true = SP_atom_from_string("true"));
  (void)SP_register_atom(local.atm_false = SP_atom_from_string("false"));
  (void)SP_register_atom(local.atm_interp = SP_atom_from_string("$TclInterp"));
  (void)SP_register_atom(local.atm_write = SP_atom_from_string("write"));
  (void)SP_register_atom(local.atm_writeq = SP_atom_from_string("writeq"));
  (void)SP_register_atom(local.atm_write_canonical = SP_atom_from_string("write_canonical"));
  (void)SP_register_atom(local.atm_format = SP_atom_from_string("format"));
  (void)SP_register_atom(local.atm_codes = SP_atom_from_string("codes"));
  (void)SP_register_atom(local.atm_chars = SP_atom_from_string("chars"));
  (void)SP_register_atom(local.atm_br = SP_atom_from_string("br"));
  (void)SP_register_atom(local.atm_dq = SP_atom_from_string("dq"));
  (void)SP_register_atom(local.atm_sqb = SP_atom_from_string("sqb"));
  (void)SP_register_atom(local.atm_min = SP_atom_from_string("min"));
  (void)SP_register_atom(local.atm_dot = SP_atom_from_string("dot"));
  (void)SP_register_atom(local.atm_list = SP_atom_from_string("list"));
  (void)SP_register_atom(local.atm_term = SP_atom_from_string("term"));

  local.call_pred = SP_predicate("call_from_tcl", 2, "tcltk");
  if (local.call_pred == NULL)
    goto existence_error;

  local.read_pred = SP_predicate("read_sc", 2, "tcltk");
  if (local.read_pred == NULL)
    goto existence_error;

  local.write_pred = SP_predicate("write_sc", 2, "tcltk");
  if (local.write_pred == NULL)
    goto existence_error;

  local.writeq_pred = SP_predicate("writeq_sc", 2, "tcltk");
  if (local.writeq_pred == NULL)
    goto existence_error;

  local.write_canonical_pred = SP_predicate("write_canonical_sc", 2, "tcltk");
  if (local.write_canonical_pred == NULL)
    goto existence_error;

  local.format_pred = SP_predicate("format_sc", 3, "tcltk");
  if (local.format_pred == NULL)
    goto existence_error;

  return;
 existence_error:
  /* [PM] 4.0 FIXME: EXISTENCE_ERROR|EXIST_PREDICATE is not handled correctly */
  SAVE_ERROR(EXISTENCE_ERROR|EXIST_PREDICATE, "", 0, 0);
  RAISE_ERROR("tcl_initialize", 0);
}

void SPCDECL tcl_deinitialize(int when)
{
  (void)when;

  if (local.tcl_inited) {
    Tcl_Finalize();               /* [PM] 3.11.2 SPRM 7990 ("halt/0 on prolog could not kill the sicstus process.") */
  }

  tcl_stream_deinit();

  (void)SP_unregister_atom(local.atm_nil);
  (void)SP_unregister_atom(local.atm_period);
  (void)SP_unregister_atom(local.atm_true);
  (void)SP_unregister_atom(local.atm_false);
  (void)SP_unregister_atom(local.atm_interp);
  (void)SP_unregister_atom(local.atm_write);
  (void)SP_unregister_atom(local.atm_writeq);
  (void)SP_unregister_atom(local.atm_write_canonical);
  (void)SP_unregister_atom(local.atm_format);
  (void)SP_unregister_atom(local.atm_codes);
  (void)SP_unregister_atom(local.atm_chars);
  (void)SP_unregister_atom(local.atm_br);
  (void)SP_unregister_atom(local.atm_dq);
  (void)SP_unregister_atom(local.atm_sqb);
  (void)SP_unregister_atom(local.atm_min);
  (void)SP_unregister_atom(local.atm_dot);
  (void)SP_unregister_atom(local.atm_list);
  (void)SP_unregister_atom(local.atm_term);
}
