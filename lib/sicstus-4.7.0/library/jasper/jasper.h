/*
 * Copyright (c) 1999, SICS
 */


#ifndef INCLUDED_JASPER_H_
#define INCLUDED_JASPER_H_

#include <jni.h>
#include <stdlib.h>
#include <sicstus/sicstus.h>
#include <sicstus/config.h>     /* ([PM] 4.2.1 this also defines SICSTUS_DBG) */



#if !defined(JASPER_DBG)
#if SICSTUS_DBG
#define JASPER_DBG SICSTUS_DBG
#endif  /* SICSTUS_DBG */
#endif  /* !defined(JASPER_DBG) */

#if JASPER_DBG
#define JASPER_CHECK_JNI 1
#endif

#if !defined(MULTI_SP)
#define MULTI_SP 1
#endif /* !defined(MULTI_SP) */


#if (JASPER_DBG+0) > 0
#define JASPER_STRINGISIZE1(X) #X
#define JASPER_STRINGISIZE(X) JASPER_STRINGISIZE1(X)
#define JASPER_DBGLINE(PROC) do {fprintf(stderr, "%s:%d %s\n", __FILE__, (int)__LINE__, JASPER_STRINGISIZE(PROC)); fflush(stderr);} while(0)
#else
#define JASPER_DBGLINE(_)
#endif

/* Helpers used by both jasper.c and spnative.c */

#if JASPER_DBG

/* Does (*jnienv)->ExceptionDescribe(jnienv) but does *not* clear the pending exception */
static void jasper_DescribeException(JNIEnv *jnienv)
{
  jthrowable pending = NULL;

  pending = (*jnienv)->ExceptionOccurred(jnienv);
  if (pending)
    {
      fprintf(stderr, "%s:%d jasper_DescribeException: ", __FILE__, (int)__LINE__);
      (*jnienv)->ExceptionDescribe(jnienv); /* clears it  */
      (*jnienv)->Throw(jnienv, pending); /* restores it */
      fflush(stderr);
    }

  if (pending)
    {
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }
}
#endif /* JASPER_DBG */

#if JASPER_DBG
#define ASSERT_NO_EXCP(JNIENV, STRING) \
   do{ \
      if ( (( *(JNIENV) )->ExceptionCheck((JNIENV))) ) { \
         fprintf(stderr, "ERROR: Pending Java exception (%s)\n", (STRING)); \
         jasper_DescribeException((JNIENV));\
         fflush(stderr); \
      } \
   } while (0)
#else /* no JASPER_DBG */
#define ASSERT_NO_EXCP(JNIENV, STRING)
#endif

static jvalue SPCDECL
CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj,
                 const char *name,
                 const char *descriptor,
                 ...)
{
  va_list args;
  jclass clazz;
  jmethodID mid;
  jvalue result;

  result.l = NULL; /* prevent compiler warning (or __MSVC_RUNTIME_CHECKS break) */

  ASSERT_NO_EXCP(env, "CallMethodByName");
  if ((*env)->EnsureLocalCapacity(env, 2) == JNI_OK)
    {
      clazz = (*env)->GetObjectClass(env, obj);
      mid = (*env)->GetMethodID(env, clazz, name, descriptor);
      if (mid) {
        const char *p = descriptor;
        /* skip over argument types to find out the return type */
        while (*p != ')') p++;
        /* skip ')' */
        p++;
        va_start(args, descriptor);
        switch (*p) {
        case 'V':
          (*env)->CallVoidMethodV(env, obj, mid, args);
          break;
        case '[':
        case 'L':
          result.l = (*env)->CallObjectMethodV(
                                               env, obj, mid, args);
          break;
        case 'Z':
          result.z = (*env)->CallBooleanMethodV(
                                                env, obj, mid, args);
          break;
        case 'B':
          result.b = (*env)->CallByteMethodV(
                                             env, obj, mid, args);
          break;
        case 'C':
          result.c = (*env)->CallCharMethodV(
                                             env, obj, mid, args);
          break;
        case 'S':
          result.s = (*env)->CallShortMethodV(
                                              env, obj, mid, args);
          break;
        case 'I':
          result.i = (*env)->CallIntMethodV(
                                            env, obj, mid, args);
          break;
        case 'J':
          result.j = (*env)->CallLongMethodV(
                                             env, obj, mid, args);
          break;
        case 'F':
          result.f = (*env)->CallFloatMethodV(
                                              env, obj, mid, args);
          break;
        case 'D':
          result.d = (*env)->CallDoubleMethodV(
                                               env, obj, mid, args);
          break;
        default:
          (*env)->FatalError(env, "illegal descriptor");
        }
        va_end(args);
      }
      (*env)->DeleteLocalRef(env, clazz);

      {
        jboolean hasExceptionTmp = (*env)->ExceptionCheck(env);

        if (hasException)
          {
            *hasException = hasExceptionTmp;
          }
        else
          {
#if JASPER_DBG && 0
            if (hasExceptionTmp) {
              fprintf(stderr, "WARNING: Got exception from %s(%s) but caller ignores exceptions\n", name, descriptor); fflush(stderr);
            }
#endif  /* JASPER_DBG */
          }
      }
    }
  else
    {
#if JASPER_DBG
      {
        fprintf(stderr, "WARNING: EnsureLocalCapacity() != JNI_OK (%s:%d)\n", __FILE__, (int)__LINE__);fflush(stderr);
        if (!(*env)->ExceptionCheck(env))
          {
            fprintf(stderr, "ERROR: EnsureLocalCapacity()!=JNI_OK && ! ExceptionCheck() (%s:%d)\n", __FILE__, (int)__LINE__);fflush(stderr);
            exit(1);
          }
      }
#endif  /* JASPER_DBG */

      if (hasException)
        {
          *hasException = JNI_TRUE;
        }
    }

  return result;
}

static jvalue
CallStaticMethodByName(JNIEnv *env,
                       jboolean *hasException,
                       const char *clazzName,
                       const char *name,
                       const char *descriptor,
                       ...)
{
  va_list args;
  jclass clazz = NULL;          /* NULL for cleanup */
  jmethodID mid;
  jvalue result;
#if JASPER_DBG
  int expect_exception = 0;
#endif

  result.l = NULL; /* prevent compiler warning (or __MSVC_RUNTIME_CHECKS break) */

  if ((*env)->EnsureLocalCapacity(env, 2) == JNI_OK)
    {
      if (((clazz = (*env)->FindClass(env, clazzName)) != NULL)
          &&
          ((mid = (*env)->GetStaticMethodID(env, clazz, name, descriptor)) != NULL))
        {
          const char *p = descriptor;
          /* skip over argument types to find out the return type */
          while (*p != ')') p++;
          /* skip ')' */
          p++;
          va_start(args, descriptor);
          switch (*p) {
          case 'V':
            (*env)->CallStaticVoidMethodV(env, clazz, mid, args);
            break;
          case '[':
          case 'L':
            result.l = (*env)->CallStaticObjectMethodV(
                                                       env, clazz, mid, args);
            break;
          case 'Z':
            result.z = (*env)->CallStaticBooleanMethodV(
                                                        env, clazz, mid, args);
            break;
          case 'B':
            result.b = (*env)->CallStaticByteMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'C':
            result.c = (*env)->CallStaticCharMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'S':
            result.s = (*env)->CallStaticShortMethodV(
                                                      env, clazz, mid, args);
            break;
          case 'I':
            result.i = (*env)->CallStaticIntMethodV(
                                                    env, clazz, mid, args);
            break;
          case 'J':
            result.j = (*env)->CallStaticLongMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'F':
            result.f = (*env)->CallStaticFloatMethodV(
                                                      env, clazz, mid, args);
            break;
          case 'D':
            result.d = (*env)->CallStaticDoubleMethodV(
                                                       env, clazz, mid, args);
            break;
          default:
            /* We should not get here unless GetMethodID goofed up */
            (*env)->FatalError(env, "illegal descriptor");
          }
          va_end(args);
        }
      else
        {
#if JASPER_DBG
          expect_exception = 1;
#endif/* JASPER_DBG */
        }
      if (clazz) (*env)->DeleteLocalRef(env, clazz);
    }
  else
    {
#if JASPER_DBG
      expect_exception = 1;
#endif/* JASPER_DBG */
    }

  if (hasException)
    {
      *hasException = (*env)->ExceptionCheck(env);
    }
#if JASPER_DBG
  if (expect_exception && !(*env)->ExceptionCheck(env))
    {
      fprintf(stderr, "%s:%d ERROR: expected exception but ExceptionCheck()\n", __FILE__, (int)__LINE__);fflush(stderr);
      exit(2);
    }
#endif/* JASPER_DBG */
  return result;
}

#endif /* INCLUDED_JASPER_H_ */

